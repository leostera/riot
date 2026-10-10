#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <sys/event.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include "kernel_new_errors.h"

typedef struct token_binding {
  int selector_fd;
  int target_fd;
  int filter;
  value *token_root;
  struct token_binding *next;
} token_binding;

static token_binding *kernel_new_async_bindings = NULL;
static intnat kernel_new_async_next_token_id = 0;

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
