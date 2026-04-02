open Std
module Test = Std.Test

(** Comprehensive tests for TTY module - tests both TTY state management and Escape_seq *)
(** ## Test Categories
    
    This test suite covers:
    1. Cursor visibility (show/hide)
    2. Cursor movement (up/down/forward/back/position)
    3. Screen clearing (screen/line/partial)
    4. Alternate screen buffer
    5. Mouse support
    6. Bracketed paste mode
    7. Focus tracking
    8. Kitty keyboard protocol
    9. Synchronized output
    10. Color sequences
    11. Text attributes
    12. ANSI stripping and width calculation
*)

(** ## 1. Cursor Visibility Tests *)

let test_show_cursor = fun _ctx ->
  let output = Tty.Escape_seq.show_cursor_seq in
  if output = "\x1b[?25h" then
    Ok ()
  else
    Error ("show_cursor: expected '\\x1b[?25h', got '" ^ output ^ "'")

let test_hide_cursor = fun _ctx ->
  let output = Tty.Escape_seq.hide_cursor_seq in
  if output = "\x1b[?25l" then
    Ok ()
  else
    Error ("hide_cursor: expected '\\x1b[?25l', got '" ^ output ^ "'")

let test_show_hide_sequence = fun _ctx ->
  let hide = Tty.Escape_seq.hide_cursor_seq in
  let show = Tty.Escape_seq.show_cursor_seq in
  let output = hide ^ show in
  if output = "\x1b[?25l\x1b[?25h" then
    Ok ()
  else
    Error ("show/hide sequence failed, got '" ^ output ^ "'")

(** ## 2. Cursor Movement Tests *)

let test_cursor_position_home = fun _ctx ->
  let output = Tty.Escape_seq.cursor_position_seq 1 1 in
  if output = "\x1b[1;1H" then
    Ok ()
  else
    Error ("move to home: expected '\\x1b[1;1H', got '" ^ output ^ "'")

let test_cursor_position_arbitrary = fun _ctx ->
  let output = Tty.Escape_seq.cursor_position_seq 15 42 in
  if output = "\x1b[15;42H" then
    Ok ()
  else
    Error ("move arbitrary: expected '\\x1b[15;42H', got '" ^ output ^ "'")

let test_cursor_up = fun _ctx ->
  let output = Tty.Escape_seq.cursor_up_seq 5 in
  if output = "\x1b[5A" then
    Ok ()
  else
    Error ("cursor up: expected '\\x1b[5A', got '" ^ output ^ "'")

let test_cursor_down = fun _ctx ->
  let output = Tty.Escape_seq.cursor_down_seq 3 in
  if output = "\x1b[3B" then
    Ok ()
  else
    Error ("cursor down: expected '\\x1b[3B', got '" ^ output ^ "'")

let test_cursor_forward = fun _ctx ->
  let output = Tty.Escape_seq.cursor_forward_seq 10 in
  if output = "\x1b[10C" then
    Ok ()
  else
    Error ("cursor forward: expected '\\x1b[10C', got '" ^ output ^ "'")

let test_cursor_back = fun _ctx ->
  let output = Tty.Escape_seq.cursor_back_seq 2 in
  if output = "\x1b[2D" then
    Ok ()
  else
    Error ("cursor back: expected '\\x1b[2D', got '" ^ output ^ "'")

let test_cursor_next_line = fun _ctx ->
  let output = Tty.Escape_seq.cursor_next_line_seq 3 in
  if output = "\x1b[3E" then
    Ok ()
  else
    Error ("cursor next line: expected '\\x1b[3E', got '" ^ output ^ "'")

let test_cursor_previous_line = fun _ctx ->
  let output = Tty.Escape_seq.cursor_previous_line_seq 2 in
  if output = "\x1b[2F" then
    Ok ()
  else
    Error ("cursor prev line: expected '\\x1b[2F', got '" ^ output ^ "'")

let test_cursor_horizontal = fun _ctx ->
  let output = Tty.Escape_seq.cursor_horizontal_seq 25 in
  if output = "\x1b[25G" then
    Ok ()
  else
    Error ("cursor horizontal: expected '\\x1b[25G', got '" ^ output ^ "'")

let test_save_restore_cursor = fun _ctx ->
  let save = Tty.Escape_seq.save_cursor_position_seq in
  let restore = Tty.Escape_seq.restore_cursor_position_seq in
  if save = "\x1b[s" && restore = "\x1b[u" then
    Ok ()
  else
    Error ("save/restore cursor failed: '" ^ save ^ "' '" ^ restore ^ "'")

(** ## 3. Screen Clearing Tests *)

let test_erase_display_below = fun _ctx ->
  let output = Tty.Escape_seq.erase_display_seq 0 in
  if output = "\x1b[0J" then
    Ok ()
  else
    Error ("erase below: expected '\\x1b[0J', got '" ^ output ^ "'")

let test_erase_display_above = fun _ctx ->
  let output = Tty.Escape_seq.erase_display_seq 1 in
  if output = "\x1b[1J" then
    Ok ()
  else
    Error ("erase above: expected '\\x1b[1J', got '" ^ output ^ "'")

let test_erase_display_all = fun _ctx ->
  let output = Tty.Escape_seq.erase_display_seq 2 in
  if output = "\x1b[2J" then
    Ok ()
  else
    Error ("erase all: expected '\\x1b[2J', got '" ^ output ^ "'")

let test_erase_entire_line = fun _ctx ->
  let output = Tty.Escape_seq.erase_entire_line_seq in
  if output = "\x1b[2K" then
    Ok ()
  else
    Error ("erase line: expected '\\x1b[2K', got '" ^ output ^ "'")

let test_erase_line_right = fun _ctx ->
  let output = Tty.Escape_seq.erase_line_right_seq in
  if output = "\x1b[0K" then
    Ok ()
  else
    Error ("erase line right: expected '\\x1b[0K', got '" ^ output ^ "'")

let test_erase_line_left = fun _ctx ->
  let output = Tty.Escape_seq.erase_line_left_seq in
  if output = "\x1b[1K" then
    Ok ()
  else
    Error ("erase line left: expected '\\x1b[1K', got '" ^ output ^ "'")

(** ## 4. Alternate Screen Tests *)

let test_alt_screen = fun _ctx ->
  let enter = Tty.Escape_seq.alt_screen_seq in
  let exit = Tty.Escape_seq.exit_alt_screen_seq in
  if enter = "\x1b[?1049h" && exit = "\x1b[?1049l" then
    Ok ()
  else
    Error ("alt screen failed: '" ^ enter ^ "' '" ^ exit ^ "'")

let test_save_restore_screen = fun _ctx ->
  let save = Tty.Escape_seq.save_screen_seq in
  let restore = Tty.Escape_seq.restore_screen_seq in
  if save = "\x1b[?47h" && restore = "\x1b[?47l" then
    Ok ()
  else
    Error ("save/restore screen failed: '" ^ save ^ "' '" ^ restore ^ "'")

let test_reset_scroll_region = fun _ctx ->
  let reset = Tty.Escape_seq.reset_scroll_region_seq in
  if reset = "\x1b[r" then
    Ok ()
  else
    Error ("reset scroll region: expected '\\x1b[r', got '" ^ reset ^ "'")

let test_change_scrolling_region = fun _ctx ->
  let output = Tty.Escape_seq.change_scrolling_region_seq 5 20 in
  if output = "\x1b[5;20r" then
    Ok ()
  else
    Error ("scrolling region: expected '\\x1b[5;20r', got '" ^ output ^ "'")

(** ## 5. Mouse Support Tests *)

let test_mouse_sequences = fun _ctx ->
  let tests = [
    ("enable_press", Tty.Escape_seq.enable_mouse_press_seq, "\x1b[?9h");
    ("disable_press", Tty.Escape_seq.disable_mouse_press_seq, "\x1b[?9l");
    ("enable_basic", Tty.Escape_seq.enable_mouse_seq, "\x1b[?1000h");
    ("disable_basic", Tty.Escape_seq.disable_mouse_seq, "\x1b[?1000l");
    ("enable_hilite", Tty.Escape_seq.enable_mouse_hilite_seq, "\x1b[?1001h");
    ("disable_hilite", Tty.Escape_seq.disable_mouse_hilite_seq, "\x1b[?1001l");
    ("enable_cell", Tty.Escape_seq.enable_mouse_cell_motion_seq, "\x1b[?1002h");
    ("disable_cell", Tty.Escape_seq.disable_mouse_cell_motion_seq, "\x1b[?1002l");
    ("enable_all", Tty.Escape_seq.enable_mouse_all_motion_seq, "\x1b[?1003h");
    ("disable_all", Tty.Escape_seq.disable_mouse_all_motion_seq, "\x1b[?1003l");
    ("enable_sgr", Tty.Escape_seq.enable_mouse_extended_mode_seq, "\x1b[?1006h");
    ("disable_sgr", Tty.Escape_seq.disable_mouse_extended_mode_seq, "\x1b[?1006l");
    ("enable_pixels", Tty.Escape_seq.enable_mouse_pixels_mode_seq, "\x1b[?1016h");
    ("disable_pixels", Tty.Escape_seq.disable_mouse_pixels_mode_seq, "\x1b[?1016l");
  ]
  in
  let errors =
    List.filter_map
      (fun ((name, actual, expected)) ->
        if actual = expected then
          None
        else
          Some (name ^ ": expected '" ^ expected ^ "', got '" ^ actual ^ "'"))
      tests
  in
  match errors with
  | [] -> Ok ()
  | errs -> Error (String.concat "; " errs)

(** ## 6. Bracketed Paste Tests *)

let test_bracketed_paste = fun _ctx ->
  let enable = Tty.Escape_seq.enable_bracketed_paste_seq in
  let disable = Tty.Escape_seq.disable_bracketed_paste_seq in
  let start = Tty.Escape_seq.start_bracketed_paste_seq in
  let end_paste = Tty.Escape_seq.end_bracketed_paste_seq in
  if
    enable = "\x1b[?2004h" && disable = "\x1b[?2004l" && start = "\x1b[200~" && end_paste = "\x1b[201~"
  then
    Ok ()
  else
    Error "Bracketed paste sequences incorrect"

(** ## 7. Focus Tracking Tests *)

let test_focus_tracking = fun _ctx ->
  let enable = Tty.Escape_seq.enable_focus_events_seq in
  let disable = Tty.Escape_seq.disable_focus_events_seq in
  if enable = "\x1b[?1004h" && disable = "\x1b[?1004l" then
    Ok ()
  else
    Error "Focus tracking sequences incorrect"

(** ## 8. Kitty Keyboard Tests *)

let test_kitty_keyboard = fun _ctx ->
  let enable = Tty.Escape_seq.enable_kitty_keyboard_seq in
  let disable = Tty.Escape_seq.disable_kitty_keyboard_seq in
  if enable = "\x1b[>1u" && disable = "\x1b[<u" then
    Ok ()
  else
    Error "Kitty keyboard sequences incorrect"

(** ## 9. Synchronized Output Tests *)

let test_sync_output = fun _ctx ->
  let begin_sync = Tty.Escape_seq.begin_sync_seq in
  let end_sync = Tty.Escape_seq.end_sync_seq in
  if begin_sync = "\x1b[?2026h" && end_sync = "\x1b[?2026l" then
    Ok ()
  else
    Error "Sync output sequences incorrect"

(** ## 10. Color and Title Tests *)

let test_color_sequences = fun _ctx ->
  let fg = Tty.Escape_seq.set_foreground_color_seq "255;0;0" in
  let bg = Tty.Escape_seq.set_background_color_seq "0;255;0" in
  let cursor = Tty.Escape_seq.set_cursor_color_seq "0;0;255" in
  let title = Tty.Escape_seq.set_window_title_seq "Test Title" in
  if
    fg = "\x1b[10;255;0;0" && bg = "\x1b[11;0;255;0" && cursor = "\x1b[12;0;0;255" && title = "\x1b[2;Test Title"
  then
    Ok ()
  else
    Error "Color/title sequences incorrect"

(** ## 11. Text Attribute Tests *)

let test_text_attributes = fun _ctx ->
  let tests = [
    ("reset", Tty.Escape_seq.reset_seq, "0");
    ("bold", Tty.Escape_seq.bold_seq, "1");
    ("faint", Tty.Escape_seq.faint_seq, "2");
    ("italic", Tty.Escape_seq.italics_seq, "3");
    ("underline", Tty.Escape_seq.underline_seq, "4");
    ("blink", Tty.Escape_seq.blink_seq, "5");
    ("reverse", Tty.Escape_seq.reverse_seq, "7");
    ("cross_out", Tty.Escape_seq.cross_out_seq, "9");
    ("overline", Tty.Escape_seq.overline_seq, "53");
    ("foreground", Tty.Escape_seq.foreground_seq, "38");
    ("background", Tty.Escape_seq.background_seq, "48");
  ]
  in
  let errors =
    List.filter_map
      (fun ((name, actual, expected)) ->
        if actual = expected then
          None
        else
          Some (name ^ ": expected '" ^ expected ^ "', got '" ^ actual ^ "'"))
      tests
  in
  match errors with
  | [] -> Ok ()
  | errs -> Error (String.concat "; " errs)

(** ## 12. ANSI Stripping Tests *)

let test_strip_simple = fun _ctx ->
  let input = "\x1b[31mRed\x1b[0m" in
  let output = Tty.Escape_seq.strip input in
  if output = "Red" then
    Ok ()
  else
    Error ("strip simple: expected 'Red', got '" ^ output ^ "'")

let test_strip_multiple = fun _ctx ->
  let input = "\x1b[1m\x1b[32mBold Green\x1b[0m Normal \x1b[4mUnderline\x1b[0m" in
  let output = Tty.Escape_seq.strip input in
  if output = "Bold Green Normal Underline" then
    Ok ()
  else
    Error ("strip multiple: expected 'Bold Green Normal Underline', got '" ^ output ^ "'")

let test_strip_complex = fun _ctx ->
  let input = "\x1b[38;2;255;0;0mRGB Red\x1b[0m" in
  let output = Tty.Escape_seq.strip input in
  if output = "RGB Red" then
    Ok ()
  else
    Error ("strip complex: expected 'RGB Red', got '" ^ output ^ "'")

let test_width_simple = fun _ctx ->
  let input = "Hello" in
  let width = Tty.Escape_seq.width input in
  if width = 5 then
    Ok ()
  else
    Error ("width simple: expected 5, got " ^ Int.to_string width)

let test_width_with_ansi = fun _ctx ->
  let input = "\x1b[31mHello\x1b[0m" in
  let width = Tty.Escape_seq.width input in
  if width = 5 then
    Ok ()
  else
    Error ("width with ANSI: expected 5, got " ^ Int.to_string width)

(** ## TTY State Management Tests *)

let test_tty_creation = fun _ctx ->
  match Tty.make () with
  | Ok tty ->
      let size = Tty.size tty in
      if size.cols > 0 && size.rows > 0 then
        Ok ()
      else
        Error "Invalid TTY size"
  | Error Tty.NoTtyConnected ->
      Ok ()
  | Error (Tty.SystemError _) ->
      Ok ()

let test_raw_mode = fun _ctx ->
  match Tty.make_raw () with
  | Ok tty ->
      if Tty.mode tty = Tty.Immediate then
        Ok ()
      else
        Error "Expected Immediate mode"
  | Error _ -> Ok ()

(* Skip if no TTY *)

let test_mode_switching = fun _ctx ->
  match Tty.make () with
  | Ok tty ->
      Tty.set_raw tty;
      let is_raw = Tty.mode tty = Tty.Immediate in
      Tty.set_line_buffered tty;
      let is_line = Tty.mode tty = Tty.LineBuffered in
      if is_raw && is_line then
        Ok ()
      else
        Error "Mode switching failed"
  | Error _ -> Ok ()

let tests =
  Test.[
    case "show_cursor" test_show_cursor;
    case "hide_cursor" test_hide_cursor;
    case "show_hide_sequence" test_show_hide_sequence;
    case "cursor_position_home" test_cursor_position_home;
    case "cursor_position_arbitrary" test_cursor_position_arbitrary;
    case "cursor_up" test_cursor_up;
    case "cursor_down" test_cursor_down;
    case "cursor_forward" test_cursor_forward;
    case "cursor_back" test_cursor_back;
    case "cursor_next_line" test_cursor_next_line;
    case "cursor_previous_line" test_cursor_previous_line;
    case "cursor_horizontal" test_cursor_horizontal;
    case "save_restore_cursor" test_save_restore_cursor;
    case "erase_display_below" test_erase_display_below;
    case "erase_display_above" test_erase_display_above;
    case "erase_display_all" test_erase_display_all;
    case "erase_entire_line" test_erase_entire_line;
    case "erase_line_right" test_erase_line_right;
    case "erase_line_left" test_erase_line_left;
    case "alt_screen" test_alt_screen;
    case "save_restore_screen" test_save_restore_screen;
    case "reset_scroll_region" test_reset_scroll_region;
    case "change_scrolling_region" test_change_scrolling_region;
    case "mouse_sequences" test_mouse_sequences;
    case "bracketed_paste" test_bracketed_paste;
    case "focus_tracking" test_focus_tracking;
    case "kitty_keyboard" test_kitty_keyboard;
    case "sync_output" test_sync_output;
    case "color_sequences" test_color_sequences;
    case "text_attributes" test_text_attributes;
    case "strip_simple" test_strip_simple;
    case "strip_multiple" test_strip_multiple;
    case "strip_complex" test_strip_complex;
    case "width_simple" test_width_simple;
    case "width_with_ansi" test_width_with_ansi;
    case "tty_creation" test_tty_creation;
    case "raw_mode" test_raw_mode;
    case "mode_switching" test_mode_switching;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"tty_comprehensive" ~tests ~args)
    ~args:Env.args
    ()
