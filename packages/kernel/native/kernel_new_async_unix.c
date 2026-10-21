#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include "kernel_new_errors.h"

static intnat kernel_new_async_next_token_id = 0;

#if defined(__APPLE__) || defined(__MACH__)

#include <sys/event.h>

typedef struct token_binding {
  int selector_fd;
  int target_fd;
  int filter;
  value *token_root;
  struct token_binding *next;
} token_binding;

static token_binding *kernel_new_async_bindings = NULL;

static void kernel_new_async_remove_selector_bindings(int selector_fd) {
  token_binding **cursor = &kernel_new_async_bindings;
  while (*cursor != NULL) {
    token_binding *binding = *cursor;
    if (binding->selector_fd == selector_fd) {
      caml_remove_generational_global_root(binding->token_root);
      free(binding->token_root);
      *cursor = binding->next;
      free(binding);
    } else {
      cursor = &binding->next;
    }
  }
}

static token_binding *kernel_new_async_find_binding(int selector_fd, int target_fd, int filter) {
  token_binding *binding = kernel_new_async_bindings;
  while (binding != NULL) {
    if (binding->selector_fd == selector_fd && binding->target_fd == target_fd && binding->filter == filter) {
      return binding;
    }
    binding = binding->next;
  }
  return NULL;
}

static void kernel_new_async_remove_binding(int selector_fd, int target_fd, int filter) {
  token_binding **cursor = &kernel_new_async_bindings;
  while (*cursor != NULL) {
    token_binding *binding = *cursor;
    if (binding->selector_fd == selector_fd && binding->target_fd == target_fd && binding->filter == filter) {
      caml_remove_generational_global_root(binding->token_root);
      free(binding->token_root);
      *cursor = binding->next;
      free(binding);
      return;
    }
    cursor = &binding->next;
  }
}

static token_binding *kernel_new_async_store_binding(int selector_fd, int target_fd, int filter, value token) {
  token_binding *binding = kernel_new_async_find_binding(selector_fd, target_fd, filter);
  if (binding == NULL) {
    binding = malloc(sizeof(token_binding));
    if (binding == NULL) {
      caml_raise_out_of_memory();
    }
    binding->token_root = malloc(sizeof(value));
    if (binding->token_root == NULL) {
      free(binding);
      caml_raise_out_of_memory();
    }
    binding->selector_fd = selector_fd;
    binding->target_fd = target_fd;
    binding->filter = filter;
    binding->next = kernel_new_async_bindings;
    kernel_new_async_bindings = binding;
    *(binding->token_root) = token;
    caml_register_generational_global_root(binding->token_root);
  } else {
    *(binding->token_root) = token;
    caml_modify_generational_global_root(binding->token_root, token);
  }
  return binding;
}

static value kernel_new_async_event_to_ocaml(const struct kevent *event) {
  CAMLparam0();
  CAMLlocal1(out);

  out = caml_alloc_tuple(4);
  Store_field(out, 0, Val_int((int)event->ident));
  Store_field(out, 1, Val_int((int)event->filter));
  Store_field(out, 2, Val_int((int)event->flags));

  if (event->udata == NULL) {
    Store_field(out, 3, Val_int(0));
  } else {
    value *token_root = (value *)event->udata;
    Store_field(out, 3, *token_root);
  }

  CAMLreturn(out);
}

static int kernel_new_async_ignore_error(value ignored_errors_val, int code) {
  int ignored_count = Wosize_val(ignored_errors_val);
  for (int index = 0; index < ignored_count; index++) {
    if (Int_val(Field(ignored_errors_val, index)) == code) {
      return 1;
    }
  }
  return 0;
}

#endif

CAMLprim value kernel_new_async_token_make(value payload_val) {
  CAMLparam1(payload_val);
  CAMLlocal1(token_val);

  kernel_new_async_next_token_id += 1;
  token_val = caml_alloc_tuple(2);
  Store_field(token_val, 0, Val_long(kernel_new_async_next_token_id));
  Store_field(token_val, 1, payload_val);

  CAMLreturn(token_val);
}

CAMLprim value kernel_new_async_token_id(value token_val) {
  CAMLparam1(token_val);
  CAMLreturn(Field(token_val, 0));
}

CAMLprim value kernel_new_async_token_value(value token_val) {
  CAMLparam1(token_val);
  CAMLreturn(Field(token_val, 1));
}

#if defined(__APPLE__) || defined(__MACH__)

CAMLprim value kernel_new_async_unix_selector_create(value unit_val) {
  CAMLparam1(unit_val);

  int selector_fd = kqueue();
  if (selector_fd == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  if (fcntl(selector_fd, F_SETFD, FD_CLOEXEC) == -1) {
    int saved_errno = errno;
    close(selector_fd);
    errno = saved_errno;
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(selector_fd)));
}

CAMLprim value kernel_new_async_unix_selector_close(value selector_val) {
  CAMLparam1(selector_val);

  int selector_fd = Int_val(selector_val);
  if (close(selector_fd) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  kernel_new_async_remove_selector_bindings(selector_fd);
  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_async_unix_selector_wait(value max_events_val, value timeout_ns_val, value selector_val) {
  CAMLparam3(max_events_val, timeout_ns_val, selector_val);
  CAMLlocal3(out, result, event_val);

  int selector_fd = Int_val(selector_val);
  int max_events = Int_val(max_events_val);
  int64_t timeout_ns = Int64_val(timeout_ns_val);
  struct kevent *events = malloc(sizeof(struct kevent) * max_events);
  int ready_count = 0;

  if (events == NULL) {
    caml_raise_out_of_memory();
  }

  if (timeout_ns < 0) {
    caml_enter_blocking_section();
    ready_count = kevent(selector_fd, NULL, 0, events, max_events, NULL);
    caml_leave_blocking_section();
  } else {
    struct timespec timeout;
    timeout.tv_sec = (time_t)(timeout_ns / 1000000000LL);
    timeout.tv_nsec = (long)(timeout_ns % 1000000000LL);
    caml_enter_blocking_section();
    ready_count = kevent(selector_fd, NULL, 0, events, max_events, &timeout);
    caml_leave_blocking_section();
  }

  if (ready_count == -1) {
    int code = kernel_new_error_of_errno(errno);
    free(events);
    if (code == KERNEL_NEW_ERR_INTERRUPTED) {
      out = Atom(0);
      result = kernel_new_result_ok(out);
      CAMLreturn(result);
    }
    CAMLreturn(kernel_new_result_error(code));
  }

  if (ready_count == 0) {
    out = Atom(0);
  } else {
    out = caml_alloc(ready_count, 0);
    for (int index = 0; index < ready_count; index++) {
      event_val = kernel_new_async_event_to_ocaml(&events[index]);
      Store_field(out, index, event_val);
    }
  }

  free(events);
  result = kernel_new_result_ok(out);
  CAMLreturn(result);
}

CAMLprim value kernel_new_async_unix_selector_apply(value selector_val, value changes_val, value ignored_errors_val) {
  CAMLparam3(selector_val, changes_val, ignored_errors_val);
  CAMLlocal1(result);

  int selector_fd = Int_val(selector_val);
  int change_count = Wosize_val(changes_val);
  struct kevent *changes = malloc(sizeof(struct kevent) * change_count);
  int applied_change_count = 0;

  if (fcntl(selector_fd, F_GETFD) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  if (changes == NULL) {
    caml_raise_out_of_memory();
  }

  for (int index = 0; index < change_count; index++) {
    value event_val = Field(changes_val, index);
    int target_fd = Int_val(Field(event_val, 0));
    int filter = Int_val(Field(event_val, 1));
    int flags = Int_val(Field(event_val, 2));
    value token = Field(event_val, 3);

    if ((flags & EV_DELETE) != 0) {
      token_binding *binding = kernel_new_async_find_binding(selector_fd, target_fd, filter);
      if (binding != NULL) {
        EV_SET(
          &changes[applied_change_count],
          target_fd,
          filter,
          flags,
          0,
          0,
          binding->token_root);
        applied_change_count += 1;
      }
    } else {
      token_binding *binding =
        kernel_new_async_store_binding(selector_fd, target_fd, filter, token);
      EV_SET(&changes[applied_change_count], target_fd, filter, flags, 0, 0, binding->token_root);
      applied_change_count += 1;
    }
  }

  int syscall_result = 0;
  if (applied_change_count != 0) {
    caml_enter_blocking_section();
    syscall_result = kevent(selector_fd, changes, applied_change_count, NULL, 0, NULL);
    caml_leave_blocking_section();
  }

  free(changes);

  if (syscall_result == -1) {
    int code = kernel_new_error_of_errno(errno);
    if (code == KERNEL_NEW_ERR_INTERRUPTED || kernel_new_async_ignore_error(ignored_errors_val, code)) {
      result = kernel_new_result_ok(Val_unit);
      CAMLreturn(result);
    }
    CAMLreturn(kernel_new_result_error(code));
  }

  for (int index = 0; index < change_count; index++) {
    value event_val = Field(changes_val, index);
    int target_fd = Int_val(Field(event_val, 0));
    int filter = Int_val(Field(event_val, 1));
    int flags = Int_val(Field(event_val, 2));
    if ((flags & EV_DELETE) != 0) {
      kernel_new_async_remove_binding(selector_fd, target_fd, filter);
    }
  }

  result = kernel_new_result_ok(Val_unit);
  CAMLreturn(result);
}

static value kernel_new_async_process_change(
  value selector_val,
  value pid_val,
  value token_val,
  int flags
) {
  CAMLparam3(selector_val, pid_val, token_val);

  int selector_fd = Int_val(selector_val);
  int pid = Int_val(pid_val);
  struct kevent change;
  token_binding *binding = NULL;

  if ((flags & EV_DELETE) != 0) {
    binding = kernel_new_async_find_binding(selector_fd, pid, EVFILT_PROC);
    if (binding == NULL) {
      CAMLreturn(kernel_new_result_ok(Val_unit));
    }
  } else {
    binding = kernel_new_async_store_binding(selector_fd, pid, EVFILT_PROC, token_val);
  }

  EV_SET(
    &change,
    (uintptr_t)pid,
    EVFILT_PROC,
    flags,
    NOTE_EXIT,
    0,
    binding->token_root);

  int syscall_result;
  int saved_errno;
  caml_enter_blocking_section();
  do {
    syscall_result = kevent(selector_fd, &change, 1, NULL, 0, NULL);
    saved_errno = errno;
  } while (syscall_result == -1 && saved_errno == EINTR);
  caml_leave_blocking_section();

  if (syscall_result == -1) {
    int code = kernel_new_error_of_errno(saved_errno);
    if ((flags & EV_DELETE) == 0) {
      kernel_new_async_remove_binding(selector_fd, pid, EVFILT_PROC);
    }
    if ((flags & EV_DELETE) != 0 && code == KERNEL_NEW_ERR_NO_SUCH_PROCESS) {
      kernel_new_async_remove_binding(selector_fd, pid, EVFILT_PROC);
      CAMLreturn(kernel_new_result_ok(Val_unit));
    }
    CAMLreturn(kernel_new_result_error(code));
  }

  if ((flags & EV_DELETE) != 0) {
    kernel_new_async_remove_binding(selector_fd, pid, EVFILT_PROC);
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

static value kernel_new_async_timer_change(
  value selector_val,
  value timer_id_val,
  value timeout_parts_val,
  value repeat_val,
  value token_val,
  int flags
) {
  CAMLparam5(selector_val, timer_id_val, timeout_parts_val, repeat_val, token_val);

  int selector_fd = Int_val(selector_val);
  int timer_id = Int_val(timer_id_val);
  int filter = EVFILT_TIMER;
  struct kevent change;
  token_binding *binding = NULL;

  if ((flags & EV_DELETE) != 0) {
    binding = kernel_new_async_find_binding(selector_fd, timer_id, filter);
    if (binding == NULL) {
      CAMLreturn(kernel_new_result_ok(Val_unit));
    }
  } else {
    binding = kernel_new_async_store_binding(selector_fd, timer_id, filter, token_val);
  }

  int kevent_flags = flags | EV_ENABLE;
  if (Bool_val(repeat_val) == 0) {
    kevent_flags |= EV_ONESHOT;
  }

  int64_t timeout_ns = 0;
  if ((flags & EV_DELETE) == 0) {
    int timeout_secs = Int_val(Field(timeout_parts_val, 0));
    int timeout_nanos = Int_val(Field(timeout_parts_val, 1));
    timeout_ns =
      ((int64_t)timeout_secs * 1000000000LL) + (int64_t)timeout_nanos;
  }

  EV_SET(
    &change,
    (uintptr_t)timer_id,
    filter,
    kevent_flags,
    NOTE_NSECONDS,
    timeout_ns,
    binding->token_root);

  int syscall_result;
  caml_enter_blocking_section();
  syscall_result = kevent(selector_fd, &change, 1, NULL, 0, NULL);
  caml_leave_blocking_section();

  if (syscall_result == -1) {
    int code = kernel_new_error_of_errno(errno);
    if ((flags & EV_DELETE) == 0) {
      kernel_new_async_remove_binding(selector_fd, timer_id, filter);
    }
    if (
      code == KERNEL_NEW_ERR_INTERRUPTED
      || code == KERNEL_NEW_ERR_NO_SUCH_FILE_OR_DIRECTORY
    ) {
      if ((flags & EV_DELETE) != 0) {
        kernel_new_async_remove_binding(selector_fd, timer_id, filter);
      }
      CAMLreturn(kernel_new_result_ok(Val_unit));
    }
    CAMLreturn(kernel_new_result_error(code));
  }

  if ((flags & EV_DELETE) != 0) {
    kernel_new_async_remove_binding(selector_fd, timer_id, filter);
  }

  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_async_unix_selector_register_process(
  value selector_val,
  value pid_val,
  value token_val
) {
  return kernel_new_async_process_change(
    selector_val,
    pid_val,
    token_val,
    EV_ADD | EV_RECEIPT | EV_CLEAR
  );
}

CAMLprim value kernel_new_async_unix_selector_reregister_process(
  value selector_val,
  value pid_val,
  value token_val
) {
  return kernel_new_async_process_change(
    selector_val,
    pid_val,
    token_val,
    EV_ADD | EV_RECEIPT | EV_CLEAR
  );
}

CAMLprim value kernel_new_async_unix_selector_deregister_process(
  value selector_val,
  value pid_val
) {
  return kernel_new_async_process_change(
    selector_val,
    pid_val,
    Val_int(0),
    EV_DELETE | EV_RECEIPT
  );
}

CAMLprim value kernel_new_async_unix_selector_register_timer(
  value selector_val,
  value timer_id_val,
  value timeout_parts_val,
  value repeat_val,
  value token_val
) {
  return kernel_new_async_timer_change(
    selector_val,
    timer_id_val,
    timeout_parts_val,
    repeat_val,
    token_val,
    EV_ADD | EV_RECEIPT | EV_CLEAR
  );
}

CAMLprim value kernel_new_async_unix_selector_reregister_timer(
  value selector_val,
  value timer_id_val,
  value timeout_parts_val,
  value repeat_val,
  value token_val
) {
  return kernel_new_async_timer_change(
    selector_val,
    timer_id_val,
    timeout_parts_val,
    repeat_val,
    token_val,
    EV_ADD | EV_RECEIPT | EV_CLEAR
  );
}

CAMLprim value kernel_new_async_unix_selector_deregister_timer(
  value selector_val,
  value timer_id_val
) {
  return kernel_new_async_timer_change(
    selector_val,
    timer_id_val,
    Val_int(0),
    Val_false,
    Val_int(0),
    EV_DELETE | EV_RECEIPT
  );
}

#elif defined(__linux__)

#include <signal.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/syscall.h>
#include <sys/timerfd.h>
#include <time.h>

#ifndef EPOLLRDHUP
#define EPOLLRDHUP 0
#endif

#ifndef SYS_pidfd_open
#ifdef __NR_pidfd_open
#define SYS_pidfd_open __NR_pidfd_open
#endif
#endif

#define KERNEL_NEW_LINUX_EV_ADD 0x1
#define KERNEL_NEW_LINUX_EV_DELETE 0x2
#define KERNEL_NEW_LINUX_EV_EOF 0x8000
#define KERNEL_NEW_LINUX_EV_ERROR 0x4000
#define KERNEL_NEW_LINUX_FILTER_READ -1
#define KERNEL_NEW_LINUX_FILTER_WRITE -2
#define KERNEL_NEW_LINUX_FILTER_PROC -5
#define KERNEL_NEW_LINUX_FILTER_TIMER -7
#define KERNEL_NEW_LINUX_INTEREST_READ 0x1
#define KERNEL_NEW_LINUX_INTEREST_WRITE 0x2

typedef enum {
  KERNEL_NEW_LINUX_BINDING_FD,
  KERNEL_NEW_LINUX_BINDING_PROCESS,
  KERNEL_NEW_LINUX_BINDING_TIMER
} kernel_new_linux_binding_kind;

typedef struct kernel_new_linux_binding {
  int selector_fd;
  int target_id;
  int epoll_fd;
  int filter_mask;
  int filter;
  kernel_new_linux_binding_kind kind;
  value *token_root;
  struct kernel_new_linux_binding *next;
} kernel_new_linux_binding;

static kernel_new_linux_binding *kernel_new_linux_bindings = NULL;

static int kernel_new_async_ignore_error(value ignored_errors_val, int code) {
  int ignored_count = Wosize_val(ignored_errors_val);
  for (int index = 0; index < ignored_count; index++) {
    if (Int_val(Field(ignored_errors_val, index)) == code) {
      return 1;
    }
  }
  return 0;
}

static void kernel_new_linux_free_binding(kernel_new_linux_binding *binding) {
  caml_remove_generational_global_root(binding->token_root);
  free(binding->token_root);
  if (
    binding->kind != KERNEL_NEW_LINUX_BINDING_FD
    && binding->epoll_fd >= 0
  ) {
    close(binding->epoll_fd);
  }
  free(binding);
}

static void kernel_new_linux_remove_binding(kernel_new_linux_binding *target) {
  kernel_new_linux_binding **cursor = &kernel_new_linux_bindings;
  while (*cursor != NULL) {
    kernel_new_linux_binding *binding = *cursor;
    if (binding == target) {
      *cursor = binding->next;
      kernel_new_linux_free_binding(binding);
      return;
    }
    cursor = &binding->next;
  }
}

static void kernel_new_linux_remove_selector_bindings(int selector_fd) {
  kernel_new_linux_binding **cursor = &kernel_new_linux_bindings;
  while (*cursor != NULL) {
    kernel_new_linux_binding *binding = *cursor;
    if (binding->selector_fd == selector_fd) {
      *cursor = binding->next;
      kernel_new_linux_free_binding(binding);
    } else {
      cursor = &binding->next;
    }
  }
}

static kernel_new_linux_binding *kernel_new_linux_find_binding(
  int selector_fd,
  kernel_new_linux_binding_kind kind,
  int target_id
) {
  kernel_new_linux_binding *binding = kernel_new_linux_bindings;
  while (binding != NULL) {
    if (
      binding->selector_fd == selector_fd
      && binding->kind == kind
      && binding->target_id == target_id
    ) {
      return binding;
    }
    binding = binding->next;
  }
  return NULL;
}

static void kernel_new_linux_update_token(
  kernel_new_linux_binding *binding,
  value token
) {
  *(binding->token_root) = token;
  caml_modify_generational_global_root(binding->token_root, token);
}

static kernel_new_linux_binding *kernel_new_linux_create_binding(
  int selector_fd,
  kernel_new_linux_binding_kind kind,
  int target_id,
  int epoll_fd,
  int filter_mask,
  int filter,
  value token
) {
  kernel_new_linux_binding *binding = malloc(sizeof(kernel_new_linux_binding));
  if (binding == NULL) {
    caml_raise_out_of_memory();
  }

  binding->token_root = malloc(sizeof(value));
  if (binding->token_root == NULL) {
    free(binding);
    caml_raise_out_of_memory();
  }

  binding->selector_fd = selector_fd;
  binding->kind = kind;
  binding->target_id = target_id;
  binding->epoll_fd = epoll_fd;
  binding->filter_mask = filter_mask;
  binding->filter = filter;
  binding->next = kernel_new_linux_bindings;
  kernel_new_linux_bindings = binding;

  *(binding->token_root) = token;
  caml_register_generational_global_root(binding->token_root);
  return binding;
}

static uint32_t kernel_new_linux_fd_events(int filter_mask) {
  uint32_t events = EPOLLERR | EPOLLHUP;
  if ((filter_mask & KERNEL_NEW_LINUX_INTEREST_READ) != 0) {
    events |= EPOLLIN | EPOLLPRI | EPOLLRDHUP;
  }
  if ((filter_mask & KERNEL_NEW_LINUX_INTEREST_WRITE) != 0) {
    events |= EPOLLOUT;
  }
  return events;
}

static int kernel_new_linux_event_flags(uint32_t events) {
  int flags = 0;
  if ((events & EPOLLERR) != 0) {
    flags |= KERNEL_NEW_LINUX_EV_ERROR;
  }
  if ((events & (EPOLLHUP | EPOLLRDHUP | EPOLLERR)) != 0) {
    flags |= KERNEL_NEW_LINUX_EV_EOF;
  }
  return flags;
}

static int kernel_new_linux_epoll_ctl_code(
  int selector_fd,
  int operation,
  int fd,
  struct epoll_event *event
) {
  if (epoll_ctl(selector_fd, operation, fd, event) == -1) {
    return kernel_new_error_of_errno(errno);
  }
  return 0;
}

static int kernel_new_linux_ready_output_count(
  kernel_new_linux_binding *binding,
  uint32_t events
) {
  if (binding->kind != KERNEL_NEW_LINUX_BINDING_FD) {
    return 1;
  }

  int count = 0;
  if (
    (binding->filter_mask & KERNEL_NEW_LINUX_INTEREST_READ) != 0
    && (events & (EPOLLIN | EPOLLPRI | EPOLLERR | EPOLLHUP | EPOLLRDHUP)) != 0
  ) {
    count += 1;
  }
  if (
    (binding->filter_mask & KERNEL_NEW_LINUX_INTEREST_WRITE) != 0
    && (events & (EPOLLOUT | EPOLLERR | EPOLLHUP)) != 0
  ) {
    count += 1;
  }
  return count;
}

static value kernel_new_linux_event_to_ocaml(
  kernel_new_linux_binding *binding,
  int filter,
  int flags
) {
  CAMLparam0();
  CAMLlocal1(out);

  out = caml_alloc_tuple(4);
  Store_field(out, 0, Val_int(binding->target_id));
  Store_field(out, 1, Val_int(filter));
  Store_field(out, 2, Val_int(flags));
  Store_field(out, 3, *(binding->token_root));

  CAMLreturn(out);
}

static int kernel_new_linux_add_or_modify_fd(
  int selector_fd,
  kernel_new_linux_binding *binding,
  int operation
) {
  struct epoll_event event;
  memset(&event, 0, sizeof(event));
  event.events = kernel_new_linux_fd_events(binding->filter_mask);
  event.data.ptr = binding;

  int code = kernel_new_linux_epoll_ctl_code(selector_fd, operation, binding->epoll_fd, &event);
  if (operation == EPOLL_CTL_ADD && code == KERNEL_NEW_ERR_ALREADY_EXISTS) {
    code = kernel_new_linux_epoll_ctl_code(selector_fd, EPOLL_CTL_MOD, binding->epoll_fd, &event);
  }
  return code;
}

static int kernel_new_linux_apply_fd_change(
  int selector_fd,
  int target_fd,
  int filter,
  int flags,
  value token
) {
  int interest_bit = 0;
  if (filter == KERNEL_NEW_LINUX_FILTER_READ) {
    interest_bit = KERNEL_NEW_LINUX_INTEREST_READ;
  } else if (filter == KERNEL_NEW_LINUX_FILTER_WRITE) {
    interest_bit = KERNEL_NEW_LINUX_INTEREST_WRITE;
  } else {
    return KERNEL_NEW_ERR_INVALID_ARGUMENT;
  }

  kernel_new_linux_binding *binding = kernel_new_linux_find_binding(
    selector_fd,
    KERNEL_NEW_LINUX_BINDING_FD,
    target_fd
  );

  if ((flags & KERNEL_NEW_LINUX_EV_DELETE) != 0) {
    if (binding == NULL || (binding->filter_mask & interest_bit) == 0) {
      return 0;
    }

    int old_mask = binding->filter_mask;
    int new_mask = old_mask & ~interest_bit;
    if (new_mask == 0) {
      int code = kernel_new_linux_epoll_ctl_code(selector_fd, EPOLL_CTL_DEL, target_fd, NULL);
      if (
        code == KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR
        || code == KERNEL_NEW_ERR_NO_SUCH_FILE_OR_DIRECTORY
      ) {
        kernel_new_linux_remove_binding(binding);
        return 0;
      }
      if (code != 0) {
        return code;
      }
      kernel_new_linux_remove_binding(binding);
      return 0;
    }

    binding->filter_mask = new_mask;
    int code = kernel_new_linux_add_or_modify_fd(selector_fd, binding, EPOLL_CTL_MOD);
    if (code != 0) {
      binding->filter_mask = old_mask;
      return code;
    }
    return 0;
  }

  if (binding == NULL) {
    binding = kernel_new_linux_create_binding(
      selector_fd,
      KERNEL_NEW_LINUX_BINDING_FD,
      target_fd,
      target_fd,
      interest_bit,
      0,
      token
    );
    int code = kernel_new_linux_add_or_modify_fd(selector_fd, binding, EPOLL_CTL_ADD);
    if (code != 0) {
      kernel_new_linux_remove_binding(binding);
      return code;
    }
    return 0;
  }

  int old_mask = binding->filter_mask;
  binding->filter_mask = old_mask | interest_bit;
  kernel_new_linux_update_token(binding, token);
  int code = kernel_new_linux_add_or_modify_fd(selector_fd, binding, EPOLL_CTL_MOD);
  if (code != 0) {
    binding->filter_mask = old_mask;
    return code;
  }
  return 0;
}

static int kernel_new_linux_timeout_ms(int64_t timeout_ns) {
  if (timeout_ns < 0) {
    return -1;
  }
  int64_t timeout_ms = (timeout_ns + 999999LL) / 1000000LL;
  if (timeout_ms > INT_MAX) {
    return INT_MAX;
  }
  return (int)timeout_ms;
}

static int kernel_new_linux_drain_timer(kernel_new_linux_binding *binding) {
  uint64_t expirations = 0;
  ssize_t read_result;

  do {
    read_result = read(binding->epoll_fd, &expirations, sizeof(expirations));
  } while (read_result == -1 && errno == EINTR);

  if (read_result == -1) {
    if (errno == EAGAIN
#ifdef EWOULDBLOCK
        || errno == EWOULDBLOCK
#endif
    ) {
      return 0;
    }
    return kernel_new_error_of_errno(errno);
  }

  if (read_result != (ssize_t)sizeof(expirations)) {
    return KERNEL_NEW_ERR_INPUT_OUTPUT;
  }

  return 0;
}

static int kernel_new_linux_pidfd_open(int pid) {
#ifdef SYS_pidfd_open
  return (int)syscall(SYS_pidfd_open, pid, 0);
#else
  errno = ENOSYS;
  return -1;
#endif
}

CAMLprim value kernel_new_async_unix_selector_create(value unit_val) {
  CAMLparam1(unit_val);

  int selector_fd = epoll_create1(EPOLL_CLOEXEC);
  if (selector_fd == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  CAMLreturn(kernel_new_result_ok(Val_int(selector_fd)));
}

CAMLprim value kernel_new_async_unix_selector_close(value selector_val) {
  CAMLparam1(selector_val);

  int selector_fd = Int_val(selector_val);
  if (close(selector_fd) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  kernel_new_linux_remove_selector_bindings(selector_fd);
  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_async_unix_selector_wait(value max_events_val, value timeout_ns_val, value selector_val) {
  CAMLparam3(max_events_val, timeout_ns_val, selector_val);
  CAMLlocal3(out, result, event_val);

  int selector_fd = Int_val(selector_val);
  int max_events = Int_val(max_events_val);
  int64_t timeout_ns = Int64_val(timeout_ns_val);
  int timeout_ms = kernel_new_linux_timeout_ms(timeout_ns);
  struct epoll_event *events = malloc(sizeof(struct epoll_event) * max_events);
  int ready_count = 0;

  if (events == NULL) {
    caml_raise_out_of_memory();
  }

  caml_enter_blocking_section();
  ready_count = epoll_wait(selector_fd, events, max_events, timeout_ms);
  caml_leave_blocking_section();

  if (ready_count == -1) {
    int code = kernel_new_error_of_errno(errno);
    free(events);
    if (code == KERNEL_NEW_ERR_INTERRUPTED) {
      out = Atom(0);
      result = kernel_new_result_ok(out);
      CAMLreturn(result);
    }
    CAMLreturn(kernel_new_result_error(code));
  }

  int output_count = 0;
  for (int index = 0; index < ready_count; index++) {
    kernel_new_linux_binding *binding = (kernel_new_linux_binding *)events[index].data.ptr;
    if (binding == NULL) {
      continue;
    }
    if (binding->kind == KERNEL_NEW_LINUX_BINDING_TIMER) {
      int drain_code = kernel_new_linux_drain_timer(binding);
      if (drain_code != 0) {
        free(events);
        CAMLreturn(kernel_new_result_error(drain_code));
      }
    }
    output_count += kernel_new_linux_ready_output_count(binding, events[index].events);
  }

  if (output_count == 0) {
    out = Atom(0);
  } else {
    out = caml_alloc(output_count, 0);
    int output_index = 0;
    for (int index = 0; index < ready_count; index++) {
      kernel_new_linux_binding *binding = (kernel_new_linux_binding *)events[index].data.ptr;
      if (binding == NULL) {
        continue;
      }

      int flags = kernel_new_linux_event_flags(events[index].events);
      if (binding->kind == KERNEL_NEW_LINUX_BINDING_FD) {
        if (
          (binding->filter_mask & KERNEL_NEW_LINUX_INTEREST_READ) != 0
          && (events[index].events & (EPOLLIN | EPOLLPRI | EPOLLERR | EPOLLHUP | EPOLLRDHUP)) != 0
        ) {
          event_val = kernel_new_linux_event_to_ocaml(
            binding,
            KERNEL_NEW_LINUX_FILTER_READ,
            flags
          );
          Store_field(out, output_index, event_val);
          output_index += 1;
        }
        if (
          (binding->filter_mask & KERNEL_NEW_LINUX_INTEREST_WRITE) != 0
          && (events[index].events & (EPOLLOUT | EPOLLERR | EPOLLHUP)) != 0
        ) {
          event_val = kernel_new_linux_event_to_ocaml(
            binding,
            KERNEL_NEW_LINUX_FILTER_WRITE,
            flags
          );
          Store_field(out, output_index, event_val);
          output_index += 1;
        }
      } else {
        event_val = kernel_new_linux_event_to_ocaml(binding, binding->filter, flags);
        Store_field(out, output_index, event_val);
        output_index += 1;
      }
    }
  }

  free(events);
  result = kernel_new_result_ok(out);
  CAMLreturn(result);
}

CAMLprim value kernel_new_async_unix_selector_apply(value selector_val, value changes_val, value ignored_errors_val) {
  CAMLparam3(selector_val, changes_val, ignored_errors_val);
  CAMLlocal1(result);

  int selector_fd = Int_val(selector_val);
  int change_count = Wosize_val(changes_val);

  if (fcntl(selector_fd, F_GETFD) == -1) {
    CAMLreturn(kernel_new_result_errno());
  }

  for (int index = 0; index < change_count; index++) {
    value event_val = Field(changes_val, index);
    int target_fd = Int_val(Field(event_val, 0));
    int filter = Int_val(Field(event_val, 1));
    int flags = Int_val(Field(event_val, 2));
    value token = Field(event_val, 3);

    int code = kernel_new_linux_apply_fd_change(selector_fd, target_fd, filter, flags, token);
    if (
      code != 0
      && code != KERNEL_NEW_ERR_INTERRUPTED
      && !kernel_new_async_ignore_error(ignored_errors_val, code)
    ) {
      CAMLreturn(kernel_new_result_error(code));
    }
  }

  result = kernel_new_result_ok(Val_unit);
  CAMLreturn(result);
}

static int kernel_new_linux_process_change(
  int selector_fd,
  int pid,
  value token,
  int delete
) {
  kernel_new_linux_binding *binding = kernel_new_linux_find_binding(
    selector_fd,
    KERNEL_NEW_LINUX_BINDING_PROCESS,
    pid
  );

  if (delete) {
    if (binding == NULL) {
      return 0;
    }
    int code = kernel_new_linux_epoll_ctl_code(selector_fd, EPOLL_CTL_DEL, binding->epoll_fd, NULL);
    if (
      code != 0
      && code != KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR
      && code != KERNEL_NEW_ERR_NO_SUCH_FILE_OR_DIRECTORY
    ) {
      return code;
    }
    kernel_new_linux_remove_binding(binding);
    return 0;
  }

  if (binding != NULL) {
    kernel_new_linux_update_token(binding, token);
    return 0;
  }

  int pidfd = kernel_new_linux_pidfd_open(pid);
  if (pidfd == -1) {
    int code = kernel_new_error_of_errno(errno);
    if (code == KERNEL_NEW_ERR_NO_SUCH_PROCESS) {
      return 0;
    }
    return code;
  }

  binding = kernel_new_linux_create_binding(
    selector_fd,
    KERNEL_NEW_LINUX_BINDING_PROCESS,
    pid,
    pidfd,
    0,
    KERNEL_NEW_LINUX_FILTER_PROC,
    token
  );

  struct epoll_event event;
  memset(&event, 0, sizeof(event));
  event.events = EPOLLIN | EPOLLERR | EPOLLHUP;
  event.data.ptr = binding;

  int code = kernel_new_linux_epoll_ctl_code(selector_fd, EPOLL_CTL_ADD, pidfd, &event);
  if (code != 0) {
    kernel_new_linux_remove_binding(binding);
    return code;
  }
  return 0;
}

static int kernel_new_linux_timer_change(
  int selector_fd,
  int timer_id,
  value timeout_parts_val,
  value repeat_val,
  value token,
  int delete
) {
  kernel_new_linux_binding *binding = kernel_new_linux_find_binding(
    selector_fd,
    KERNEL_NEW_LINUX_BINDING_TIMER,
    timer_id
  );

  if (delete) {
    if (binding == NULL) {
      return 0;
    }
    int code = kernel_new_linux_epoll_ctl_code(selector_fd, EPOLL_CTL_DEL, binding->epoll_fd, NULL);
    if (
      code != 0
      && code != KERNEL_NEW_ERR_BAD_FILE_DESCRIPTOR
      && code != KERNEL_NEW_ERR_NO_SUCH_FILE_OR_DIRECTORY
    ) {
      return code;
    }
    kernel_new_linux_remove_binding(binding);
    return 0;
  }

  int timeout_secs = Int_val(Field(timeout_parts_val, 0));
  int timeout_nanos = Int_val(Field(timeout_parts_val, 1));
  struct itimerspec spec;
  memset(&spec, 0, sizeof(spec));
  spec.it_value.tv_sec = (time_t)timeout_secs;
  spec.it_value.tv_nsec = (long)timeout_nanos;
  if (Bool_val(repeat_val) != 0) {
    spec.it_interval.tv_sec = (time_t)timeout_secs;
    spec.it_interval.tv_nsec = (long)timeout_nanos;
  }

  if (binding == NULL) {
    int timer_fd = timerfd_create(CLOCK_MONOTONIC, TFD_NONBLOCK | TFD_CLOEXEC);
    if (timer_fd == -1) {
      return kernel_new_error_of_errno(errno);
    }

    binding = kernel_new_linux_create_binding(
      selector_fd,
      KERNEL_NEW_LINUX_BINDING_TIMER,
      timer_id,
      timer_fd,
      0,
      KERNEL_NEW_LINUX_FILTER_TIMER,
      token
    );

    struct epoll_event event;
    memset(&event, 0, sizeof(event));
    event.events = EPOLLIN | EPOLLERR | EPOLLHUP;
    event.data.ptr = binding;

    int code = kernel_new_linux_epoll_ctl_code(selector_fd, EPOLL_CTL_ADD, timer_fd, &event);
    if (code != 0) {
      kernel_new_linux_remove_binding(binding);
      return code;
    }
  } else {
    kernel_new_linux_update_token(binding, token);
  }

  if (timerfd_settime(binding->epoll_fd, 0, &spec, NULL) == -1) {
    return kernel_new_error_of_errno(errno);
  }
  return 0;
}

CAMLprim value kernel_new_async_unix_selector_register_process(
  value selector_val,
  value pid_val,
  value token_val
) {
  CAMLparam3(selector_val, pid_val, token_val);
  int code = kernel_new_linux_process_change(Int_val(selector_val), Int_val(pid_val), token_val, 0);
  if (code != 0) {
    CAMLreturn(kernel_new_result_error(code));
  }
  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_async_unix_selector_reregister_process(
  value selector_val,
  value pid_val,
  value token_val
) {
  CAMLparam3(selector_val, pid_val, token_val);
  int code = kernel_new_linux_process_change(Int_val(selector_val), Int_val(pid_val), token_val, 0);
  if (code != 0) {
    CAMLreturn(kernel_new_result_error(code));
  }
  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_async_unix_selector_deregister_process(
  value selector_val,
  value pid_val
) {
  CAMLparam2(selector_val, pid_val);
  int code = kernel_new_linux_process_change(Int_val(selector_val), Int_val(pid_val), Val_int(0), 1);
  if (code != 0) {
    CAMLreturn(kernel_new_result_error(code));
  }
  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_async_unix_selector_register_timer(
  value selector_val,
  value timer_id_val,
  value timeout_parts_val,
  value repeat_val,
  value token_val
) {
  CAMLparam5(selector_val, timer_id_val, timeout_parts_val, repeat_val, token_val);
  int code = kernel_new_linux_timer_change(
    Int_val(selector_val),
    Int_val(timer_id_val),
    timeout_parts_val,
    repeat_val,
    token_val,
    0
  );
  if (code != 0) {
    CAMLreturn(kernel_new_result_error(code));
  }
  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_async_unix_selector_reregister_timer(
  value selector_val,
  value timer_id_val,
  value timeout_parts_val,
  value repeat_val,
  value token_val
) {
  CAMLparam5(selector_val, timer_id_val, timeout_parts_val, repeat_val, token_val);
  int code = kernel_new_linux_timer_change(
    Int_val(selector_val),
    Int_val(timer_id_val),
    timeout_parts_val,
    repeat_val,
    token_val,
    0
  );
  if (code != 0) {
    CAMLreturn(kernel_new_result_error(code));
  }
  CAMLreturn(kernel_new_result_ok(Val_unit));
}

CAMLprim value kernel_new_async_unix_selector_deregister_timer(
  value selector_val,
  value timer_id_val
) {
  CAMLparam2(selector_val, timer_id_val);
  int code = kernel_new_linux_timer_change(
    Int_val(selector_val),
    Int_val(timer_id_val),
    Val_int(0),
    Val_false,
    Val_int(0),
    1
  );
  if (code != 0) {
    CAMLreturn(kernel_new_result_error(code));
  }
  CAMLreturn(kernel_new_result_ok(Val_unit));
}

#else

CAMLprim value kernel_new_async_unix_selector_create(value unit_val) {
  CAMLparam1(unit_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_async_unix_selector_close(value selector_val) {
  CAMLparam1(selector_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_async_unix_selector_wait(value max_events_val, value timeout_ns_val, value selector_val) {
  CAMLparam3(max_events_val, timeout_ns_val, selector_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_async_unix_selector_apply(value selector_val, value changes_val, value ignored_errors_val) {
  CAMLparam3(selector_val, changes_val, ignored_errors_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_async_unix_selector_register_process(
  value selector_val,
  value pid_val,
  value token_val
) {
  CAMLparam3(selector_val, pid_val, token_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_async_unix_selector_reregister_process(
  value selector_val,
  value pid_val,
  value token_val
) {
  CAMLparam3(selector_val, pid_val, token_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_async_unix_selector_deregister_process(
  value selector_val,
  value pid_val
) {
  CAMLparam2(selector_val, pid_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_async_unix_selector_register_timer(
  value selector_val,
  value timer_id_val,
  value timeout_parts_val,
  value repeat_val,
  value token_val
) {
  CAMLparam5(selector_val, timer_id_val, timeout_parts_val, repeat_val, token_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_async_unix_selector_reregister_timer(
  value selector_val,
  value timer_id_val,
  value timeout_parts_val,
  value repeat_val,
  value token_val
) {
  CAMLparam5(selector_val, timer_id_val, timeout_parts_val, repeat_val, token_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

CAMLprim value kernel_new_async_unix_selector_deregister_timer(
  value selector_val,
  value timer_id_val
) {
  CAMLparam2(selector_val, timer_id_val);
  CAMLreturn(kernel_new_result_error(KERNEL_NEW_ERR_NOT_SUPPORTED));
}

#endif
