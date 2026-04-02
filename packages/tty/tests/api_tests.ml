open Std
module Test = Std.Test

(** Tests for TTY API - focusing on state management and input, not output *)
let test_make_tty = fun _ctx ->
  (* Test that we can create a TTY (may fail in non-interactive mode) *)
  match Tty.make () with
  | Ok _tty -> Ok ()
  | Error Tty.NoTtyConnected -> Ok ()
  | Error (Tty.SystemError _err) -> Ok ()

(* Also acceptable *)

let test_make_raw = fun _ctx ->
  (* Test creating a raw mode TTY *)
  match Tty.make_raw () with
  | Ok tty ->
      let mode = Tty.mode tty in
      if mode = Tty.Immediate then
        Ok ()
      else
        Error "Expected Immediate mode for make_raw"
  | Error Tty.NoTtyConnected ->
      Ok ()
  | Error (Tty.SystemError _err) ->
      Ok ()

let test_size_accessors = fun _ctx ->
  (* Test size accessor when TTY is available *)
  match Tty.make () with
  | Error _ -> Ok ()
  | Ok tty ->
      let size = Tty.size tty in
      if size.cols > 0 && size.rows > 0 then
        Ok ()
      else
        Error ("Invalid size: " ^ Int.to_string size.cols ^ "x" ^ Int.to_string size.rows)

let test_refresh_size = fun _ctx ->
  (* Test that refresh_size doesn't crash *)
  match Tty.make () with
  | Error _ -> Ok ()
  | Ok tty ->
      Tty.refresh_size tty;
      let size = Tty.size tty in
      if size.cols > 0 && size.rows > 0 then
        Ok ()
      else
        Error "Size invalid after refresh"

let test_mode_switching = fun _ctx ->
  (* Test switching between raw and line-buffered modes *)
  match Tty.make () with
  | Error _ -> Ok ()
  | Ok tty ->
      (* Start in line-buffered mode *)
      if Tty.mode tty != Tty.LineBuffered then
        Error "Expected LineBuffered mode initially"
      else (
        (* Switch to raw *)
        Tty.set_raw tty;
        if Tty.mode tty != Tty.Immediate then
          Error "Expected Immediate mode after set_raw"
        else (
          (* Switch back to line-buffered *)
          Tty.set_line_buffered tty;
          if Tty.mode tty != Tty.LineBuffered then
            Error "Expected LineBuffered mode after set_line_buffered"
          else
            Ok ()
        )
      )

let test_escape_sequences_are_strings = fun _ctx ->
  (* Test that escape sequences are pure strings, not functions *)
  try
    let sequences = [
      Tty.Escape_seq.show_cursor_seq;
      Tty.Escape_seq.hide_cursor_seq;
      Tty.Escape_seq.alt_screen_seq;
      Tty.Escape_seq.exit_alt_screen_seq;
      Tty.Escape_seq.enable_bracketed_paste_seq;
      Tty.Escape_seq.disable_bracketed_paste_seq;
      Tty.Escape_seq.enable_focus_events_seq;
      Tty.Escape_seq.disable_focus_events_seq;
      Tty.Escape_seq.enable_kitty_keyboard_seq;
      Tty.Escape_seq.disable_kitty_keyboard_seq;
    ]
    in
    (* Verify they're all non-empty strings starting with ESC *)
    if List.for_all (fun s -> String.length s > 0 && String.get s 0 = '\x1b') sequences then
      Ok ()
    else
      Error "Some escape sequences are invalid"
  with
  | e -> Error ("Escape sequence error: " ^ (Exception.to_string e))

let test_csi_constant = fun _ctx ->
  (* Test the CSI constant *)
  if Tty.Escape_seq.csi = "\x1b[" then
    Ok ()
  else
    Error ("Invalid CSI: " ^ Tty.Escape_seq.csi)

let test_strip_ansi = fun _ctx ->
  (* Test stripping ANSI codes *)
  let text_with_ansi = "\x1b[31mRed Text\x1b[0m" in
  let stripped = Tty.Escape_seq.strip text_with_ansi in
  if stripped = "Red Text" then
    Ok ()
  else
    Error ("Strip failed: got '" ^ stripped)

let test_width_calculation = fun _ctx ->
  (* Test width calculation ignoring ANSI codes *)
  let text_with_ansi = "\x1b[1;32mBold Green\x1b[0m" in
  let width = Tty.Escape_seq.width text_with_ansi in
  if width = 10 then
    Ok ()
  else
    Error ("Width calculation failed: got " ^ Int.to_string width ^ ", expected 10")

let test_is_tty = fun _ctx ->
  (* Test is_tty function *)
  let stdin_is_tty = Tty.is_tty (Tty.stdin_fd ()) in
  (* We don't know if stdin is a TTY or not, just test it doesn't crash *)
  let _ = stdin_is_tty in
  Ok ()

let test_stdin_stdout_stderr_fds = fun _ctx ->
  (* Test that we can get standard file descriptors *)
  let _stdin = Tty.stdin_fd () in
  let _stdout = Tty.stdout_fd () in
  let _stderr = Tty.stderr_fd () in
  Ok ()

let tests =
  Test.[
    case "make_tty" test_make_tty;
    case "make_raw" test_make_raw;
    case "size_accessors" test_size_accessors;
    case "refresh_size" test_refresh_size;
    case "mode_switching" test_mode_switching;
    case "escape_sequences_are_strings" test_escape_sequences_are_strings;
    case "csi_constant" test_csi_constant;
    case "strip_ansi" test_strip_ansi;
    case "width_calculation" test_width_calculation;
    case "is_tty" test_is_tty;
    case "stdin_stdout_stderr_fds" test_stdin_stdout_stderr_fds;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"tty_api" ~tests ~args) ~args:Env.args ()
