#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define RIOT_FUZZ_AFL_FORKSRV_FD_READ 198
#define RIOT_FUZZ_AFL_FORKSRV_FD_WRITE 199
#define RIOT_FUZZ_AFL_MAP_SIZE (1 << 16)
#define RIOT_FUZZ_AFL_STARTUP_TIMEOUT_MS 5000

#define RIOT_FUZZ_STATUS_EXITED 0
#define RIOT_FUZZ_STATUS_SIGNALED 1
#define RIOT_FUZZ_STATUS_STOPPED 2
#define RIOT_FUZZ_STATUS_TIMED_OUT 3

typedef struct {
  int shmid;
  unsigned char *area;
  size_t size;
  int closed;
} riot_fuzz_afl_map;

typedef struct {
  pid_t pid;
  int ctl_fd;
  int status_fd;
  pid_t current_child;
  int has_child;
  int closed;
} riot_fuzz_afl_forkserver;

static value riot_fuzz_result_ok(value payload) {
  CAMLparam1(payload);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, payload);
  CAMLreturn(result);
}

static value riot_fuzz_result_error(void) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc(1, 1);
  Store_field(result, 0, Val_int(errno));
  CAMLreturn(result);
}

static int riot_fuzz_write_u32(int fd, uint32_t value) {
  unsigned char *cursor = (unsigned char *)&value;
  size_t remaining = sizeof(value);
  while (remaining > 0) {
    ssize_t written = write(fd, cursor, remaining);
    if (written == -1) {
      if (errno == EINTR) continue;
      return -1;
    }
    if (written == 0) {
      errno = EPIPE;
      return -1;
    }
    cursor += written;
    remaining -= (size_t)written;
  }
  return 0;
}

static int riot_fuzz_read_u32(int fd, uint32_t *out) {
  unsigned char *cursor = (unsigned char *)out;
  size_t remaining = sizeof(*out);
  while (remaining > 0) {
    ssize_t bytes_read = read(fd, cursor, remaining);
    if (bytes_read == -1) {
      if (errno == EINTR) continue;
      return -1;
    }
    if (bytes_read == 0) {
      errno = EPIPE;
      return -1;
    }
    cursor += bytes_read;
    remaining -= (size_t)bytes_read;
  }
  return 0;
}

static int riot_fuzz_wait_readable(int fd, int timeout_ms) {
  struct pollfd pfd;
  pfd.fd = fd;
  pfd.events = POLLIN;
  pfd.revents = 0;

  int result;
  do {
    result = poll(&pfd, 1, timeout_ms);
  } while (result == -1 && errno == EINTR);

  if (result == 0) {
    errno = ETIMEDOUT;
    return 0;
  }
  if (result == -1) return -1;
  if ((pfd.revents & POLLIN) != 0) return 1;
  if ((pfd.revents & (POLLERR | POLLHUP | POLLNVAL)) != 0) {
    errno = EPIPE;
    return -1;
  }
  return 1;
}

static void riot_fuzz_close_if_open(int fd) {
  if (fd >= 0) {
    close(fd);
  }
}

static int riot_fuzz_dup_to(int source_fd, int target_fd) {
  if (source_fd == target_fd) return 0;
  return dup2(source_fd, target_fd);
}

static char **riot_fuzz_make_argv(value program_val, value args_val) {
  int arg_count = Wosize_val(args_val);
  char **argv = calloc((size_t)arg_count + 2, sizeof(char *));
  if (argv == NULL) return NULL;
  argv[0] = strdup(String_val(program_val));
  if (argv[0] == NULL) {
    free(argv);
    return NULL;
  }
  for (int i = 0; i < arg_count; i++) {
    argv[i + 1] = strdup(String_val(Field(args_val, i)));
    if (argv[i + 1] == NULL) {
      for (int j = 0; j <= i; j++) free(argv[j]);
      free(argv);
      return NULL;
    }
  }
  argv[arg_count + 1] = NULL;
  return argv;
}

static void riot_fuzz_free_argv(char **argv) {
  if (argv == NULL) return;
  for (int i = 0; argv[i] != NULL; i++) free(argv[i]);
  free(argv);
}

static int riot_fuzz_apply_env(value env_val, int shmid) {
  char shm_id[32];
  snprintf(shm_id, sizeof(shm_id), "%d", shmid);
  if (setenv("__AFL_SHM_ID", shm_id, 1) == -1) return -1;

  int count = Wosize_val(env_val);
  for (int index = 0; index < count; index++) {
    value entry = Field(env_val, index);
    if (setenv(String_val(Field(entry, 0)), String_val(Field(entry, 1)), 1) == -1) {
      return -1;
    }
  }
  return 0;
}

static value riot_fuzz_status_value(int status) {
  CAMLparam0();
  CAMLlocal1(tuple);
  tuple = caml_alloc_tuple(2);
  if (WIFEXITED(status)) {
    Store_field(tuple, 0, Val_int(RIOT_FUZZ_STATUS_EXITED));
    Store_field(tuple, 1, Val_int(WEXITSTATUS(status)));
  } else if (WIFSIGNALED(status)) {
    Store_field(tuple, 0, Val_int(RIOT_FUZZ_STATUS_SIGNALED));
    Store_field(tuple, 1, Val_int(WTERMSIG(status)));
  } else if (WIFSTOPPED(status)) {
    Store_field(tuple, 0, Val_int(RIOT_FUZZ_STATUS_STOPPED));
    Store_field(tuple, 1, Val_int(WSTOPSIG(status)));
  } else {
    Store_field(tuple, 0, Val_int(RIOT_FUZZ_STATUS_SIGNALED));
    Store_field(tuple, 1, Val_int(0));
  }
  CAMLreturn(tuple);
}

static value riot_fuzz_timed_out_status_value(void) {
  CAMLparam0();
  CAMLlocal1(tuple);
  tuple = caml_alloc_tuple(2);
  Store_field(tuple, 0, Val_int(RIOT_FUZZ_STATUS_TIMED_OUT));
  Store_field(tuple, 1, Val_int(SIGKILL));
  CAMLreturn(tuple);
}

static riot_fuzz_afl_map *riot_fuzz_map_val(value block) {
  return *(riot_fuzz_afl_map **)Data_custom_val(block);
}

static riot_fuzz_afl_forkserver *riot_fuzz_forkserver_val(value block) {
  return *(riot_fuzz_afl_forkserver **)Data_custom_val(block);
}

static void riot_fuzz_afl_map_finalize(value value) {
  riot_fuzz_afl_map *map = riot_fuzz_map_val(value);
  if (map == NULL) return;
  if (!map->closed) {
    if (map->area != NULL) shmdt(map->area);
    if (map->shmid >= 0) shmctl(map->shmid, IPC_RMID, NULL);
    map->closed = 1;
  }
  free(map);
}

static struct custom_operations riot_fuzz_afl_map_ops = {
  "riot_fuzz.afl_map",
  riot_fuzz_afl_map_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static void riot_fuzz_forkserver_close(riot_fuzz_afl_forkserver *forkserver) {
  if (forkserver == NULL || forkserver->closed) return;
  riot_fuzz_close_if_open(forkserver->ctl_fd);
  riot_fuzz_close_if_open(forkserver->status_fd);
  if (forkserver->pid > 0) {
    kill(forkserver->pid, SIGKILL);
    int status;
    while (waitpid(forkserver->pid, &status, 0) == -1 && errno == EINTR) {
    }
  }
  forkserver->closed = 1;
}

static void riot_fuzz_afl_forkserver_finalize(value value) {
  riot_fuzz_afl_forkserver *forkserver = riot_fuzz_forkserver_val(value);
  riot_fuzz_forkserver_close(forkserver);
  free(forkserver);
}

static struct custom_operations riot_fuzz_afl_forkserver_ops = {
  "riot_fuzz.afl_forkserver",
  riot_fuzz_afl_forkserver_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value riot_fuzz_afl_map_size(value unit) {
  (void)unit;
  return Val_int(RIOT_FUZZ_AFL_MAP_SIZE);
}

CAMLprim value riot_fuzz_afl_supported(value unit) {
  (void)unit;
  return Val_bool(1);
}

CAMLprim value riot_fuzz_afl_create_map(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(block, result);
  (void)unit;

  riot_fuzz_afl_map *map = calloc(1, sizeof(riot_fuzz_afl_map));
  if (map == NULL) {
    errno = ENOMEM;
    CAMLreturn(riot_fuzz_result_error());
  }
  map->size = RIOT_FUZZ_AFL_MAP_SIZE;
  map->shmid = shmget(IPC_PRIVATE, map->size, IPC_CREAT | IPC_EXCL | 0600);
  if (map->shmid == -1) {
    free(map);
    CAMLreturn(riot_fuzz_result_error());
  }
  map->area = shmat(map->shmid, NULL, 0);
  if (map->area == (void *)-1) {
    int saved_errno = errno;
    shmctl(map->shmid, IPC_RMID, NULL);
    free(map);
    errno = saved_errno;
    CAMLreturn(riot_fuzz_result_error());
  }
  memset(map->area, 0, map->size);

  block = caml_alloc_custom(&riot_fuzz_afl_map_ops, sizeof(riot_fuzz_afl_map *), 0, 1);
  *((riot_fuzz_afl_map **)Data_custom_val(block)) = map;
  result = riot_fuzz_result_ok(block);
  CAMLreturn(result);
}

CAMLprim value riot_fuzz_afl_map_id(value map_val) {
  CAMLparam1(map_val);
  riot_fuzz_afl_map *map = riot_fuzz_map_val(map_val);
  CAMLreturn(Val_int(map->shmid));
}

CAMLprim value riot_fuzz_afl_reset_map(value map_val) {
  CAMLparam1(map_val);
  riot_fuzz_afl_map *map = riot_fuzz_map_val(map_val);
  if (map == NULL || map->closed || map->area == NULL) {
    errno = EINVAL;
    CAMLreturn(riot_fuzz_result_error());
  }
  memset(map->area, 0, map->size);
  CAMLreturn(riot_fuzz_result_ok(Val_unit));
}

CAMLprim value riot_fuzz_afl_snapshot_map(value map_val) {
  CAMLparam1(map_val);
  CAMLlocal1(bytes);
  riot_fuzz_afl_map *map = riot_fuzz_map_val(map_val);
  bytes = caml_alloc_string(map->size);
  memcpy(Bytes_val(bytes), map->area, map->size);
  CAMLreturn(bytes);
}

CAMLprim value riot_fuzz_afl_close_map(value map_val) {
  CAMLparam1(map_val);
  riot_fuzz_afl_map *map = riot_fuzz_map_val(map_val);
  if (map == NULL || map->closed) {
    CAMLreturn(riot_fuzz_result_ok(Val_unit));
  }
  if (map->area != NULL && shmdt(map->area) == -1) {
    CAMLreturn(riot_fuzz_result_error());
  }
  map->area = NULL;
  if (map->shmid >= 0 && shmctl(map->shmid, IPC_RMID, NULL) == -1) {
    CAMLreturn(riot_fuzz_result_error());
  }
  map->shmid = -1;
  map->closed = 1;
  CAMLreturn(riot_fuzz_result_ok(Val_unit));
}

static int riot_fuzz_start_next_child(riot_fuzz_afl_forkserver *forkserver) {
  uint32_t child_pid;
  int ready = riot_fuzz_wait_readable(
    forkserver->status_fd,
    RIOT_FUZZ_AFL_STARTUP_TIMEOUT_MS
  );
  if (ready != 1) {
    return -1;
  }
  if (riot_fuzz_read_u32(forkserver->status_fd, &child_pid) == -1) {
    return -1;
  }
  forkserver->current_child = (pid_t)child_pid;
  forkserver->has_child = 1;
  return 0;
}

CAMLprim value riot_fuzz_afl_start_forkserver(
  value program_val,
  value args_val,
  value env_val,
  value current_dir_val,
  value map_val)
{
  CAMLparam5(program_val, args_val, env_val, current_dir_val, map_val);
  CAMLlocal2(block, result);

  riot_fuzz_afl_map *map = riot_fuzz_map_val(map_val);
  int ctl_pipe[2] = {-1, -1};
  int status_pipe[2] = {-1, -1};
  int error_pipe[2] = {-1, -1};
  char **argv = NULL;

  if (pipe(ctl_pipe) == -1 || pipe(status_pipe) == -1 || pipe(error_pipe) == -1) {
    goto system_error;
  }
  if (fcntl(error_pipe[0], F_SETFD, FD_CLOEXEC) == -1 ||
      fcntl(error_pipe[1], F_SETFD, FD_CLOEXEC) == -1) {
    goto system_error;
  }

  argv = riot_fuzz_make_argv(program_val, args_val);
  if (argv == NULL) {
    errno = ENOMEM;
    goto system_error;
  }

  pid_t child_pid = fork();
  if (child_pid == -1) {
    goto system_error;
  }

  if (child_pid == 0) {
    int child_errno = 0;
    riot_fuzz_close_if_open(ctl_pipe[1]);
    riot_fuzz_close_if_open(status_pipe[0]);
    riot_fuzz_close_if_open(error_pipe[0]);

    if (riot_fuzz_dup_to(ctl_pipe[0], RIOT_FUZZ_AFL_FORKSRV_FD_READ) == -1 ||
        riot_fuzz_dup_to(status_pipe[1], RIOT_FUZZ_AFL_FORKSRV_FD_WRITE) == -1) {
      child_errno = errno;
      write(error_pipe[1], &child_errno, sizeof(child_errno));
      _exit(127);
    }

    riot_fuzz_close_if_open(ctl_pipe[0]);
    riot_fuzz_close_if_open(status_pipe[1]);

    int dev_null = open("/dev/null", O_RDWR);
    if (dev_null >= 0) {
      dup2(dev_null, STDIN_FILENO);
      dup2(dev_null, STDOUT_FILENO);
      dup2(dev_null, STDERR_FILENO);
      close(dev_null);
    }

    if (Is_block(current_dir_val)) {
      if (chdir(String_val(Field(current_dir_val, 0))) == -1) {
        child_errno = errno;
        write(error_pipe[1], &child_errno, sizeof(child_errno));
        _exit(127);
      }
    }

    if (riot_fuzz_apply_env(env_val, map->shmid) == -1) {
      child_errno = errno;
      write(error_pipe[1], &child_errno, sizeof(child_errno));
      _exit(127);
    }

    execvp(String_val(program_val), argv);
    child_errno = errno;
    write(error_pipe[1], &child_errno, sizeof(child_errno));
    _exit(127);
  }

  riot_fuzz_close_if_open(ctl_pipe[0]);
  riot_fuzz_close_if_open(status_pipe[1]);
  riot_fuzz_close_if_open(error_pipe[1]);

  int child_errno = 0;
  ssize_t error_bytes;
  caml_enter_blocking_section();
  error_bytes = read(error_pipe[0], &child_errno, sizeof(child_errno));
  caml_leave_blocking_section();
  riot_fuzz_close_if_open(error_pipe[0]);
  riot_fuzz_free_argv(argv);
  argv = NULL;

  if (error_bytes > 0) {
    int wait_status = 0;
    waitpid(child_pid, &wait_status, 0);
    riot_fuzz_close_if_open(ctl_pipe[1]);
    riot_fuzz_close_if_open(status_pipe[0]);
    errno = child_errno;
    CAMLreturn(riot_fuzz_result_error());
  }

  uint32_t startup_msg = 0;
  int startup_ready = riot_fuzz_wait_readable(
    status_pipe[0],
    RIOT_FUZZ_AFL_STARTUP_TIMEOUT_MS
  );
  if (startup_ready != 1) {
    int saved_errno = errno;
    kill(child_pid, SIGKILL);
    waitpid(child_pid, NULL, 0);
    riot_fuzz_close_if_open(ctl_pipe[1]);
    riot_fuzz_close_if_open(status_pipe[0]);
    errno = saved_errno;
    CAMLreturn(riot_fuzz_result_error());
  }
  if (riot_fuzz_read_u32(status_pipe[0], &startup_msg) == -1) {
    int saved_errno = errno;
    kill(child_pid, SIGKILL);
    waitpid(child_pid, NULL, 0);
    riot_fuzz_close_if_open(ctl_pipe[1]);
    riot_fuzz_close_if_open(status_pipe[0]);
    errno = saved_errno;
    CAMLreturn(riot_fuzz_result_error());
  }
  if (riot_fuzz_write_u32(ctl_pipe[1], 0) == -1) {
    int saved_errno = errno;
    kill(child_pid, SIGKILL);
    waitpid(child_pid, NULL, 0);
    riot_fuzz_close_if_open(ctl_pipe[1]);
    riot_fuzz_close_if_open(status_pipe[0]);
    errno = saved_errno;
    CAMLreturn(riot_fuzz_result_error());
  }

  riot_fuzz_afl_forkserver *forkserver = calloc(1, sizeof(riot_fuzz_afl_forkserver));
  if (forkserver == NULL) {
    kill(child_pid, SIGKILL);
    waitpid(child_pid, NULL, 0);
    riot_fuzz_close_if_open(ctl_pipe[1]);
    riot_fuzz_close_if_open(status_pipe[0]);
    errno = ENOMEM;
    CAMLreturn(riot_fuzz_result_error());
  }
  forkserver->pid = child_pid;
  forkserver->ctl_fd = ctl_pipe[1];
  forkserver->status_fd = status_pipe[0];
  forkserver->current_child = 0;
  forkserver->has_child = 0;
  forkserver->closed = 0;

  if (riot_fuzz_start_next_child(forkserver) == -1) {
    int saved_errno = errno;
    riot_fuzz_forkserver_close(forkserver);
    free(forkserver);
    errno = saved_errno;
    CAMLreturn(riot_fuzz_result_error());
  }

  block = caml_alloc_custom(&riot_fuzz_afl_forkserver_ops, sizeof(riot_fuzz_afl_forkserver *), 0, 1);
  *((riot_fuzz_afl_forkserver **)Data_custom_val(block)) = forkserver;
  result = riot_fuzz_result_ok(block);
  CAMLreturn(result);

system_error:
  {
    int saved_errno = errno;
    riot_fuzz_close_if_open(ctl_pipe[0]);
    riot_fuzz_close_if_open(ctl_pipe[1]);
    riot_fuzz_close_if_open(status_pipe[0]);
    riot_fuzz_close_if_open(status_pipe[1]);
    riot_fuzz_close_if_open(error_pipe[0]);
    riot_fuzz_close_if_open(error_pipe[1]);
    riot_fuzz_free_argv(argv);
    errno = saved_errno;
    CAMLreturn(riot_fuzz_result_error());
  }
}

CAMLprim value riot_fuzz_afl_finish_run(value forkserver_val, value timeout_ms_val) {
  CAMLparam2(forkserver_val, timeout_ms_val);
  CAMLlocal2(status_value, result);
  riot_fuzz_afl_forkserver *forkserver = riot_fuzz_forkserver_val(forkserver_val);
  if (forkserver == NULL || forkserver->closed || !forkserver->has_child) {
    errno = EINVAL;
    CAMLreturn(riot_fuzz_result_error());
  }

  uint32_t raw_status = 0;
  int timed_out = 0;
  int wait_result = 0;
  int saved_errno = 0;
  int timeout_ms = Int_val(timeout_ms_val);

  caml_enter_blocking_section();
  wait_result = riot_fuzz_wait_readable(forkserver->status_fd, timeout_ms);
  if (wait_result == 0) {
    timed_out = 1;
    if (forkserver->current_child > 0) {
      kill(forkserver->current_child, SIGKILL);
    }
    wait_result = riot_fuzz_wait_readable(forkserver->status_fd, -1);
  }
  if (wait_result == 1 && riot_fuzz_read_u32(forkserver->status_fd, &raw_status) == -1) {
    wait_result = -1;
  }
  saved_errno = errno;
  caml_leave_blocking_section();
  errno = saved_errno;

  if (wait_result == -1) {
    CAMLreturn(riot_fuzz_result_error());
  }
  forkserver->has_child = 0;
  if (timed_out) {
    status_value = riot_fuzz_timed_out_status_value();
  } else {
    status_value = riot_fuzz_status_value((int)raw_status);
  }
  result = riot_fuzz_result_ok(status_value);
  CAMLreturn(result);
}

CAMLprim value riot_fuzz_afl_start_next_run(value forkserver_val) {
  CAMLparam1(forkserver_val);
  riot_fuzz_afl_forkserver *forkserver = riot_fuzz_forkserver_val(forkserver_val);
  if (forkserver == NULL || forkserver->closed || forkserver->has_child) {
    errno = EINVAL;
    CAMLreturn(riot_fuzz_result_error());
  }
  if (riot_fuzz_write_u32(forkserver->ctl_fd, 0) == -1) {
    CAMLreturn(riot_fuzz_result_error());
  }
  if (riot_fuzz_start_next_child(forkserver) == -1) {
    CAMLreturn(riot_fuzz_result_error());
  }
  CAMLreturn(riot_fuzz_result_ok(Val_unit));
}

CAMLprim value riot_fuzz_afl_stop_forkserver(value forkserver_val) {
  CAMLparam1(forkserver_val);
  riot_fuzz_afl_forkserver *forkserver = riot_fuzz_forkserver_val(forkserver_val);
  riot_fuzz_forkserver_close(forkserver);
  CAMLreturn(riot_fuzz_result_ok(Val_unit));
}
