open Std
module DateTime = Datetime

let test_parse_utc_basic = fun _ctx ->
  match DateTime.parse "2025-08-27T14:07:31Z" with
  | Ok dt ->
      if
        dt.year = 2_025
        && dt.month = 8
        && dt.day = 27
        && dt.hour = 14
        && dt.minute = 7
        && dt.second = 31
        && dt.time_zone = DateTime.Tz.Etc_UTC
        && dt.utc_offset = 0
      then
        Ok ()
      else
        Error "Parsed datetime doesn't match expected values"
  | Error _ -> Error "Failed to parse valid UTC datetime"

let test_parse_with_microseconds = fun _ctx ->
  match DateTime.parse "2025-08-27T14:07:31.426822Z" with
  | Ok dt ->
      if
        dt.year = 2_025
        && dt.month = 8
        && dt.day = 27
        && dt.hour = 14
        && dt.minute = 7
        && dt.second = 31
        && fst dt.microseconds = 426_822
        && snd dt.microseconds = 6
        && dt.time_zone = DateTime.Tz.Etc_UTC
      then
        Ok ()
      else
        Error "Parsed datetime with microseconds doesn't match expected values"
  | Error _ -> Error "Failed to parse valid UTC datetime with microseconds"

let test_parse_with_timezone_offset = fun _ctx ->
  match DateTime.parse "2025-08-27T14:07:31+05:30" with
  | Ok dt ->
      if
        dt.year = 2_025
        && dt.month = 8
        && dt.day = 27
        && dt.hour = 14
        && dt.minute = 7
        && dt.second = 31
        && dt.time_zone = DateTime.Tz.Local
        && dt.utc_offset = 19_800
      then
        Ok ()
      else
        Error "Parsed datetime with timezone offset doesn't match expected values"
  | Error _ -> Error "Failed to parse valid datetime with timezone offset"

let test_parse_negative_timezone_offset = fun _ctx ->
  match DateTime.parse "2025-08-27T14:07:31-07:00" with
  | Ok dt ->
      if
        dt.year = 2_025
        && dt.month = 8
        && dt.day = 27
        && dt.hour = 14
        && dt.minute = 7
        && dt.second = 31
        && dt.time_zone = DateTime.Tz.Local
        && dt.utc_offset = (-25_200)
      then
        Ok ()
      else
        Error "Parsed datetime with negative timezone offset doesn't match expected values"
  | Error _ -> Error "Failed to parse valid datetime with negative timezone offset"

let test_parse_invalid_format = fun _ctx ->
  match DateTime.parse "invalid" with
  | Ok _ -> Error "Should have failed to parse invalid format"
  | Error (DateTime.Invalid_format _) -> Ok ()
  | Error _ -> Error "Wrong error type for invalid format"

let test_parse_invalid_date = fun _ctx ->
  match DateTime.parse "2025-13-27T14:07:31Z" with
  | Ok _ -> Error "Should have failed to parse invalid month"
  | Error (DateTime.Invalid_date _) -> Ok ()
  | Error _ -> Error "Wrong error type for invalid date"

let test_parse_invalid_time = fun _ctx ->
  match DateTime.parse "2025-08-27T25:07:31Z" with
  | Ok _ -> Error "Should have failed to parse invalid hour"
  | Error (DateTime.Invalid_time _) -> Ok ()
  | Error _ -> Error "Wrong error type for invalid time"

let test_parse_invalid_timezone = fun _ctx ->
  match DateTime.parse "2025-08-27T14:07:31+25:00" with
  | Ok _ -> Error "Should have failed to parse invalid timezone hour"
  | Error (DateTime.Invalid_timezone _) -> Ok ()
  | Error _ -> Error "Wrong error type for invalid timezone"

let test_parse_leap_year = fun _ctx ->
  match DateTime.parse "2024-02-29T12:00:00Z" with
  | Ok dt ->
      if dt.year = 2_024 && dt.month = 2 && dt.day = 29 then
        Ok ()
      else
        Error "Failed to parse valid leap year date"
  | Error _ -> Error "Should have parsed valid leap year date"

let test_parse_non_leap_year_feb_29 = fun _ctx ->
  match DateTime.parse "2025-02-29T12:00:00Z" with
  | Ok _ -> Error "Should have failed to parse Feb 29 in non-leap year"
  | Error (DateTime.Invalid_date _) -> Ok ()
  | Error _ -> Error "Wrong error type for Feb 29 in non-leap year"

let test_roundtrip = fun _ctx ->
  let original = DateTime.now_utc () in
  let iso_string = DateTime.to_iso8601 original in
  match DateTime.parse iso_string with
  | Ok parsed ->
      if
        parsed.year = original.year
        && parsed.month = original.month
        && parsed.day = original.day
        && parsed.hour = original.hour
        && parsed.minute = original.minute
        && parsed.second = original.second
      then
        Ok ()
      else
        Error "Roundtrip parsing failed"
  | Error _ -> Error "Failed to parse ISO 8601 string from to_iso8601"

(* New tests for Elixir compatibility *)

let test_parse_with_space_separator = fun _ctx ->
  match DateTime.parse "2025-08-27 14:07:31Z" with
  | Ok dt ->
      if
        dt.year = 2_025
        && dt.month = 8
        && dt.day = 27
        && dt.hour = 14
        && dt.minute = 7
        && dt.second = 31
        && dt.time_zone = DateTime.Tz.Etc_UTC
      then
        Ok ()
      else
        Error "Parsed datetime with space separator doesn't match expected values"
  | Error _ -> Error "Failed to parse datetime with space separator"

let test_parse_basic_format = fun _ctx ->
  match DateTime.parse "20250827T140731Z" with
  | Ok dt ->
      if
        dt.year = 2_025
        && dt.month = 8
        && dt.day = 27
        && dt.hour = 14
        && dt.minute = 7
        && dt.second = 31
        && dt.time_zone = DateTime.Tz.Etc_UTC
      then
        Ok ()
      else
        Error "Parsed basic format datetime doesn't match expected values"
  | Error _ -> Error "Failed to parse basic format datetime"

let test_parse_basic_format_with_offset = fun _ctx ->
  match DateTime.parse "20250827T140731+0530" with
  | Ok dt ->
      if
        dt.year = 2_025
        && dt.month = 8
        && dt.day = 27
        && dt.hour = 14
        && dt.minute = 7
        && dt.second = 31
        && dt.time_zone = DateTime.Tz.Local
        && dt.utc_offset = 19_800
      then
        Ok ()
      else
        Error "Parsed basic format with offset doesn't match expected values"
  | Error _ -> Error "Failed to parse basic format with offset"

let test_parse_with_comma_decimal = fun _ctx ->
  match DateTime.parse "2025-08-27T14:07:31,426822Z" with
  | Ok dt ->
      if
        dt.year = 2_025
        && dt.month = 8
        && dt.day = 27
        && dt.hour = 14
        && dt.minute = 7
        && dt.second = 31
        && fst dt.microseconds = 426_822
        && snd dt.microseconds = 6
        && dt.time_zone = DateTime.Tz.Etc_UTC
      then
        Ok ()
      else
        Error "Parsed datetime with comma decimal doesn't match expected values"
  | Error _ -> Error "Failed to parse datetime with comma decimal separator"

let test_parse_negative_year = fun _ctx ->
  match DateTime.parse "-2015-08-27T14:07:31Z" with
  | Ok dt ->
      if
        dt.year = (-2_015)
        && dt.month = 8
        && dt.day = 27
        && dt.hour = 14
        && dt.minute = 7
        && dt.second = 31
        && dt.time_zone = DateTime.Tz.Etc_UTC
      then
        Ok ()
      else
        Error "Parsed negative year datetime doesn't match expected values"
  | Error _ -> Error "Failed to parse negative year datetime"

let test_parse_positive_year_sign = fun _ctx ->
  match DateTime.parse "+2025-08-27T14:07:31Z" with
  | Ok dt ->
      if
        dt.year = 2_025
        && dt.month = 8
        && dt.day = 27
        && dt.hour = 14
        && dt.minute = 7
        && dt.second = 31
        && dt.time_zone = DateTime.Tz.Etc_UTC
      then
        Ok ()
      else
        Error "Parsed positive year datetime doesn't match expected values"
  | Error _ -> Error "Failed to parse positive year datetime"

let test_parse_basic_with_microseconds = fun _ctx ->
  match DateTime.parse "20250827T140731.123Z" with
  | Ok dt ->
      if
        dt.year = 2_025
        && dt.month = 8
        && dt.day = 27
        && dt.hour = 14
        && dt.minute = 7
        && dt.second = 31
        && fst dt.microseconds = 123_000
        && snd dt.microseconds = 6
      then
        Ok ()
      else
        Error "Parsed basic format with microseconds doesn't match expected values"
  | Error _ -> Error "Failed to parse basic format with microseconds"

let test_parse_space_and_comma = fun _ctx ->
  match DateTime.parse "2025-08-27 14:07:31,123+02:30" with
  | Ok dt ->
      if
        dt.year = 2_025
        && dt.month = 8
        && dt.day = 27
        && dt.hour = 14
        && dt.minute = 7
        && dt.second = 31
        && fst dt.microseconds = 123_000
        && dt.utc_offset = 9_000
      then
        Ok ()
      else
        Error "Parsed datetime with space and comma doesn't match expected values"
  | Error _ -> Error "Failed to parse datetime with space separator and comma decimal"

let test_parse_negative_year_leap = fun _ctx ->
  match DateTime.parse "-2020-02-29T12:00:00Z" with
  | Ok dt ->
      if dt.year = (-2_020) && dt.month = 2 && dt.day = 29 then
        Ok ()
      else
        Error "Failed to parse negative leap year date"
  | Error _ -> Error "Should have parsed negative leap year date"

let tests =
  Test.[
    case "parse UTC basic" test_parse_utc_basic;
    case "parse with microseconds" test_parse_with_microseconds;
    case "parse with timezone offset" test_parse_with_timezone_offset;
    case "parse negative timezone offset" test_parse_negative_timezone_offset;
    case "parse invalid format" test_parse_invalid_format;
    case "parse invalid date" test_parse_invalid_date;
    case "parse invalid time" test_parse_invalid_time;
    case "parse invalid timezone" test_parse_invalid_timezone;
    case "parse leap year" test_parse_leap_year;
    case "parse non-leap year Feb 29" test_parse_non_leap_year_feb_29;
    case "roundtrip parse" test_roundtrip;
    case "parse with space separator" test_parse_with_space_separator;
    case "parse basic format" test_parse_basic_format;
    case "parse basic format with offset" test_parse_basic_format_with_offset;
    case "parse with comma decimal" test_parse_with_comma_decimal;
    case "parse negative year" test_parse_negative_year;
    case "parse positive year sign" test_parse_positive_year_sign;
    case "parse basic with microseconds" test_parse_basic_with_microseconds;
    case "parse space and comma" test_parse_space_and_comma;
    case "parse negative year leap" test_parse_negative_year_leap;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"datetime" ~tests ~args) ~args:Env.args ()
