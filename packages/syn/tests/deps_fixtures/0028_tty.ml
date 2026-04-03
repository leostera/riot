open Std
open Std.IO

(** Re-export types from Terminal module *)
type size = Terminal.size = {
  rows: int;
  cols: int;
}

type error = Terminal.error =
  | NoTtyConnected
  | SystemError of IO.error

type mode = Terminal.mode =
  | LineBuffered
  | Immediate

type input_buffer = Terminal.input_buffer = {
  data: bytes;  (* 4KB buffer *)
  mutable pos: int;  (* Current read position *)
  mutable len: int;  (* Valid data length *)
}

type input_mode = Terminal.input_mode =
  | SingleFd of Kernel.Fd.t
  (* Traditional single FD mode *)
  | DualFd of {
      (* Dual FD mode for piped input + TTY control *)
      data_fd: Kernel.Fd.t;  (* stdin for data *)
      control_fd: Kernel.Fd.t;  (* /dev/tty for control *)
      mutable active: 
        [
          `Data
          | `Control
        ];  (* Which FD to read from *)
    }

type t = Terminal.t = {
  fd: Kernel.Fd.t;  (* Primary TTY fd - used for termios operations *)
  input: input_mode;  (* Input configuration *)
  stdout: Kernel.Fd.t;  (* Output file descriptor *)
  stderr: Kernel.Fd.t;  (* Error output file descriptor *)
  original_attrs: Kernel.Terminal.termios;
  mutable size: size;
  mutable mode: mode;
  mutable input_buffer: input_buffer option;  (* Buffered input *)
  mutable data_buffer: input_buffer option;  (* Separate buffer for data FD in dual mode *)
}

(* Helper to open /dev/tty *)

let open_tty = fun () ->
  try
    match Fs.File.open_read_write (Path.v "/dev/tty") with
    | Ok file ->
        let fd = Fs.File.into_fd file in
        if Kernel.Terminal.is_tty fd then
          Ok fd
        else (
          Kernel.Fd.close fd;
          Error NoTtyConnected
        )
    | Error _ ->
        (* Fallback to stdin *)
        if Kernel.Terminal.is_tty IO.stdin then
          Ok IO.stdin
        else
          Error NoTtyConnected
  with
  | Failure msg -> Error (SystemError (IO.Unknown_error msg))
  | e -> Error (SystemError (IO.Unknown_error (Exception.to_string e)))

let make = fun ?fd ?stdin ?stdout ?stderr ?size ?(mode = LineBuffered) () ->
  (* If fd is provided, use it; otherwise try to open TTY if we're not in fake mode *)
  let is_fake_tty = stdin != None || stdout != None || stderr != None in
  let fd_result =
    match fd, is_fake_tty with
    | Some f, _ -> Ok f
    | None, true -> Ok IO.stdin
    | None, false -> open_tty ()
  in
  match fd_result with
  | Error e -> Error e
  | Ok tty_fd ->
      try
        let is_real_tty = Kernel.Terminal.is_tty tty_fd in
        (* Get termios only if this is a real TTY *)
        let original_attrs =
          if is_real_tty then
            Kernel.Terminal.get_attributes tty_fd
          else
            Kernel.Terminal.default_termios ()
        in
        (* Get size: use provided, detect if real TTY, or default *)
        let detected_size =
          match size with
          | Some s ->
              s
          | None when is_real_tty -> (
              match Kernel.Terminal.get_size tty_fd with
              | Ok (cols, rows) -> { rows; cols }
              | Error _ -> { rows = 24; cols = 80 }
            )
          | None ->
              { rows = 24; cols = 80 }
        in
        (* Determine input mode: dual FD if stdin is piped and we have a TTY *)
        let stdin_is_tty = Kernel.Terminal.is_tty IO.stdin in
        let input_mode =
          match stdin, stdin_is_tty, is_real_tty with
          | Some fd, _, _ ->
              (* Explicit stdin provided *)
              Terminal.SingleFd fd
          | None, false, true ->
              (* stdin is piped but we have a TTY - use dual FD mode *)
              let data_fd = IO.stdin in
              Terminal.DualFd { data_fd; control_fd = tty_fd; active = `Control }
          | None, _, _ ->
              (* Normal mode - stdin is a TTY or no TTY available *)
              let fd =
                if stdin_is_tty then
                  IO.stdin
                else
                  tty_fd
              in
              Terminal.SingleFd fd
        in
        let t =
          Terminal.{
            fd = tty_fd;
            input = input_mode;
            stdout = Option.unwrap_or ~default:tty_fd stdout;
            stderr = Option.unwrap_or ~default:IO.stderr stderr;
            original_attrs;
            size = detected_size;
            mode = LineBuffered;
            input_buffer = None;
            data_buffer = None;
          }
        in
        (* Apply mode if Immediate requested and this is a real TTY *)
        (
          match mode, is_real_tty with
          | Immediate, true ->
              let new_attrs = Kernel.Terminal.make_raw_mode original_attrs in
              Kernel.Terminal.set_attributes tty_fd Kernel.Terminal.Now new_attrs;
              t.mode <- Immediate
          | Immediate, false ->
              (* Fake TTY - just set the mode without termios *)
              t.mode <- Immediate
          | LineBuffered, _ ->
              ()
        );
        Ok t
      with
      | Failure msg ->
          (
            match fd with
            | None -> Kernel.Fd.close tty_fd
            | Some _ -> ()
          );
          Error (SystemError (IO.Unknown_error msg))
      | e ->
          (
            match fd with
            | None -> Kernel.Fd.close tty_fd
            | Some _ -> ()
          );
          Error (SystemError (IO.Unknown_error (Exception.to_string e)))

(* Convenience function for creating immediate mode TTY *)

let make_raw = fun () -> make ~mode:Immediate ()

let set_raw = fun t ->
  match t.mode with
  | Immediate -> ()
  | LineBuffered ->
      if Kernel.Terminal.is_tty t.fd then
        (
          let new_attrs = Kernel.Terminal.make_raw_mode t.original_attrs in
          Kernel.Terminal.set_attributes t.fd Kernel.Terminal.Now new_attrs
        );
      t.mode <- Immediate

let set_normal = fun t ->
  match t.mode with
  | LineBuffered -> ()
  | Immediate ->
      if Kernel.Terminal.is_tty t.fd then
        Kernel.Terminal.set_attributes t.fd Kernel.Terminal.Now t.original_attrs;
      t.mode <- LineBuffered

let restore = fun t ->
  set_normal t;
  Kernel.Fd.close t.fd

let size = fun t -> t.size

let refresh_size = fun t ->
  match Kernel.Terminal.get_size t.fd with
  | Ok (cols, rows) -> t.size <- { rows; cols }
  | Error _ -> ()

(* Keep cached size on error *)

let mode = fun t -> t.mode

let is_tty = Kernel.Terminal.is_tty

let set_line_buffered = fun t ->
  match t.mode with
  | LineBuffered -> ()
  | Immediate ->
      (* Switch to line-buffered mode *)
      Kernel.Terminal.set_attributes t.fd Kernel.Terminal.Flush t.original_attrs;
      t.mode <- LineBuffered

let resume = fun t ->
  (* Re-apply the mode that was active before suspension *)
  match t.mode with
  | Immediate -> set_raw t
  | LineBuffered -> set_line_buffered t

let width = fun t -> t.size.cols

let height = fun t -> t.size.rows

let fd = fun t -> t.fd

(** Read result type *)
type read =
  | Read of string
  | End
  | Malformed of string
  | Retry

(* UTF-8 input reading *)

let utf8_char_length = fun first_byte ->
  if first_byte land 0x80 = 0 then
    1
  else if first_byte land 0xe0 = 0xc0 then
    2
  else if first_byte land 0xf0 = 0xe0 then
    3
  else if first_byte land 0xf8 = 0xf0 then
    4
  else
    0

(* Get the active input FD based on input mode *)

let get_input_fd = fun t ->
  match t.input with
  | Terminal.SingleFd fd -> fd
  | Terminal.DualFd { control_fd; data_fd; active } ->
      match active with
      | `Control -> control_fd
      | `Data -> data_fd

(* Get or create the appropriate buffer for the current input mode *)

let get_buffer_for_mode = fun t ->
  match t.input with
  | Terminal.SingleFd _ -> t.input_buffer
  | Terminal.DualFd { active; _ } ->
      match active with
      | `Control -> t.input_buffer
      | `Data -> t.data_buffer

let set_buffer_for_mode = fun t buf ->
  match t.input with
  | Terminal.SingleFd _ -> t.input_buffer <- Some buf
  | Terminal.DualFd { active; _ } ->
      match active with
      | `Control -> t.input_buffer <- Some buf
      | `Data -> t.data_buffer <- Some buf

(* Ensure buffer exists and fill it if empty *)

let ensure_buffer = fun t ->
  let existing_buffer = get_buffer_for_mode t in
  match existing_buffer with
  | Some buf when buf.pos < buf.len -> buf
  | _ ->
      (* Need to create or refill buffer *)
      let buf =
        match existing_buffer with
        | Some b ->
            b.pos <- 0;
            b.len <- 0;
            b
        | None ->
            (* Create new 256-byte buffer for responsive input *)
            Terminal.{ data = Bytes.create 256; pos = 0; len = 0 }
      in
      (* Try to fill buffer from active FD *)
      let fd = get_input_fd t in
      let file = Fs.File.from_fd fd in
      (
        match Fs.File.read file buf.Terminal.data ~offset:0 ~len:(Bytes.length buf.Terminal.data) with
        | Ok n -> buf.Terminal.len <- n
        | Error _ -> buf.Terminal.len <- 0
      );
      set_buffer_for_mode t buf;
      buf

let read_utf8_buffered = fun t ->
  let buf = ensure_buffer t in
  let open Terminal in
    if buf.pos >= buf.len then
      End
      (* No more data *)
    else
      let first_byte = Char.code (Bytes.get buf.data buf.pos) in
      let char_len = utf8_char_length first_byte in
      if char_len = 0 then
        (
          buf.pos <- buf.pos + 1;
          (* Skip invalid byte *)
          Malformed "Invalid UTF-8 start byte"
        )
      else if buf.pos + char_len > buf.len then
        Retry
      else
        (
          let str = Bytes.sub_string buf.data buf.pos char_len in
          buf.pos <- buf.pos + char_len;
          Read str
        )

(* Switch to reading data in dual FD mode *)

let switch_to_data = fun t ->
  match t.input with
  | Terminal.DualFd dual -> dual.active <- `Data
  | _ -> ()

(* Switch to reading control in dual FD mode *)

let switch_to_control = fun t ->
  match t.input with
  | Terminal.DualFd dual -> dual.active <- `Control
  | _ -> ()

(* Check if we're in dual FD mode *)

let is_dual_fd = fun t ->
  match t.input with
  | Terminal.DualFd _ -> true
  | _ -> false

(* Check if input is available with timeout (milliseconds) *)

let input_available = fun t ~timeout_ms ->
  let fd = get_input_fd t in
  let file = Fs.File.from_fd fd in
  (* Check if data is available using Fs.File operations *)
  (* For now, we'll use a simple approach - try a non-blocking read *)
  match Fs.File.read file (Bytes.create 0) ~offset:0 ~len:0 with
  | Ok _ -> true
  | Error IO.Resource_unavailable_try_again -> false
  | Error _ -> false

let rec read_utf8 = fun t ->
  (* Use buffered reading if in immediate mode, otherwise fall back to old implementation *)
  if t.mode = Immediate then
    read_utf8_buffered t
  else
    (* Original byte-by-byte implementation for LineBuffered mode *)
    let fd = get_input_fd t in
    let file = Fs.File.from_fd fd in
    let bytes = Bytes.create 4 in
    match Fs.File.read file bytes ~offset:0 ~len:1 with
    | Ok 0 ->
        End
    | Ok 1 ->
        let first_byte = Char.code (Bytes.get bytes 0) in
        let len = utf8_char_length first_byte in
        if len = 0 then
          Malformed "Invalid UTF-8 start byte"
        else if len = 1 then
          Read (Bytes.sub_string bytes 0 1)
        else
          (
            match Fs.File.read file bytes ~offset:1 ~len:((len - 1)) with
            | Ok n when n = len - 1 -> Read (Bytes.sub_string bytes 0 len)
            | Ok _ -> Malformed "Incomplete UTF-8 sequence"
            | Error _ -> Malformed "Read error"
          )
    | Ok _ ->
        Malformed "Unexpected read length"
    | Error _ ->
        End

let read_utf8_with_timeout = fun t ~timeout_ms ->
  if not (input_available t ~timeout_ms) then
    Retry
  else
    read_utf8 t

let read = fun t ->
  match read_utf8 t with
  | Read s -> Ok s
  | End -> Error IO.End_of_file
  | Malformed s -> Error (IO.Unknown_error s)
  | Retry -> Error IO.Resource_unavailable_try_again

let read_line = fun t ->
  let buf = Buffer.create 256 in
  let rec loop () =
    match read t with
    | Ok s ->
        Buffer.add_string buf s;
        if String.contains s "\n" || String.contains s "\r" then
          Ok (Buffer.contents buf)
        else
          loop ()
    | Error e -> Error e
  in
  loop ()

(* Note: All output operations have been removed from TTY module.
   Use Escape_seq module to get escape sequences and write them to stdout.
   The TTY module now only manages terminal state and reads input. *)

(* Mouse support - removed, use Escape_seq module for mouse control sequences *)

(* Kitty keyboard and sync - removed, use Escape_seq module *)

(* Utility functions *)

let to_string = fun t ->
  "TTY { size=" ^ Int.to_string t.size.cols ^ "x" ^ Int.to_string t.size.rows ^ "; mode=" ^ (
    match t.mode with
    | LineBuffered -> "line-buffered"
    | Immediate -> "immediate"
  ) ^ "; fd=" ^ Int.to_string (Kernel.Fd.to_int t.fd) ^ " }"

let equal = fun t1 t2 ->
  Kernel.Fd.equal t1.fd t2.fd

let stdin_fd = fun () -> IO.stdin

let stdout_fd = fun () -> IO.stdout

let stderr_fd = fun () -> IO.stderr

(* Signal handling *)

let suspend = fun t ->
  match t.mode with
  | LineBuffered -> ()
  | Immediate -> set_normal t

(* TODO: Send SIGSTOP signal - Unix.kill not available *)

(* When SIGCONT arrives, we'll resume in normal mode *)

(* User can call set_raw again if needed *)

(* Re-export other modules *)

module Color = Color
module Escape_seq = Escape_seq
module Profile = Profile
module Style = Style
module Size = Size
module Input = Input
module Terminal_control = Terminal_control
