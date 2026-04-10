open Std

let () =
  Runtime.run
    ~main:(fun ~args:_ ->
      (* Test Greek letter *)
      let alpha =
        match Unicode.Utf8.decode_rune "α" 0 with
        | Some (r, _) -> r
        | None -> panic "Failed to decode"
      in
      (* Test Cyrillic letter *)
      let cyrillic_a =
        match Unicode.Utf8.decode_rune "А" 0 with
        | Some (r, _) -> r
        | None -> panic "Failed to decode"
      in
      (* Test CJK ideograph *)
      let zhong =
        match Unicode.Utf8.decode_rune "中" 0 with
        | Some (r, _) -> r
        | None -> panic "Failed to decode"
      in
      (* Test Arabic digit *)
      let arabic_5 =
        match Unicode.Utf8.decode_rune "٥" 0 with
        | Some (r, _) -> r
        | None -> panic "Failed to decode"
      in
      println "Testing Unicode character classification:";
      println "";
      let alpha_int = Unicode.Rune.to_int alpha in
      let alpha_hex =
        let hex_chars = "0123456789ABCDEF" in
        let rec to_hex n acc =
          if n = 0 then
            if acc = "" then
              "0000"
            else
              String.make (4 - String.length acc) '0' ^ acc
          else
            to_hex (n / 16) (String.make 1 (String.get hex_chars (n mod 16)) ^ acc)
        in
        to_hex alpha_int ""
      in
      println ("Greek α (U+" ^ alpha_hex ^ "):");
      println ("  is_letter: " ^ Bool.to_string (Unicode.Rune.is_letter alpha) ^ " (should be true)");
      println ("  is_lower: " ^ Bool.to_string (Unicode.Rune.is_lower alpha) ^ " (should be true)");
      println "";
      let cyrillic_int = Unicode.Rune.to_int cyrillic_a in
      let cyrillic_hex =
        let hex_chars = "0123456789ABCDEF" in
        let rec to_hex n acc =
          if n = 0 then
            if acc = "" then
              "0000"
            else
              String.make (4 - String.length acc) '0' ^ acc
          else
            to_hex (n / 16) (String.make 1 (String.get hex_chars (n mod 16)) ^ acc)
        in
        to_hex cyrillic_int ""
      in
      println ("Cyrillic А (U+" ^ cyrillic_hex ^ "):");
      println
        ("  is_letter: " ^ Bool.to_string (Unicode.Rune.is_letter cyrillic_a) ^ " (should be true)");
      println
        ("  is_upper: " ^ Bool.to_string (Unicode.Rune.is_upper cyrillic_a) ^ " (should be true)");
      println "";
      let zhong_int = Unicode.Rune.to_int zhong in
      let zhong_hex =
        let hex_chars = "0123456789ABCDEF" in
        let rec to_hex n acc =
          if n = 0 then
            if acc = "" then
              "0000"
            else
              String.make (4 - String.length acc) '0' ^ acc
          else
            to_hex (n / 16) (String.make 1 (String.get hex_chars (n mod 16)) ^ acc)
        in
        to_hex zhong_int ""
      in
      println ("CJK 中 (U+" ^ zhong_hex ^ "):");
      println ("  is_letter: " ^ Bool.to_string (Unicode.Rune.is_letter zhong) ^ " (should be true)");
      println "";
      let arabic_int = Unicode.Rune.to_int arabic_5 in
      let arabic_hex =
        let hex_chars = "0123456789ABCDEF" in
        let rec to_hex n acc =
          if n = 0 then
            if acc = "" then
              "0000"
            else
              String.make (4 - String.length acc) '0' ^ acc
          else
            to_hex (n / 16) (String.make 1 (String.get hex_chars (n mod 16)) ^ acc)
        in
        to_hex arabic_int ""
      in
      println ("Arabic digit ٥ (U+" ^ arabic_hex ^ "):");
      println
        ("  is_digit: " ^ Bool.to_string (Unicode.Rune.is_digit arabic_5) ^ " (should be true)");
      println "";
      (* Test ASCII for regression *)
      let a = Unicode.Rune.of_char 'A' in
      println "ASCII 'A':";
      println ("  is_letter: " ^ Bool.to_string (Unicode.Rune.is_letter a) ^ " (should be true)");
      println ("  is_upper: " ^ Bool.to_string (Unicode.Rune.is_upper a) ^ " (should be true)");
      println "";
      println "✅ All tests completed!";
      Ok ())
    ~args:Env.args
    ()
