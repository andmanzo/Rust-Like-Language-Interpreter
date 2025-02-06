open Tinyrust
open Common


(** ------------------------------------------
    Start of parser tests
    ------------------------------------------ *)

let%test_unit "test_parser" =
  Array.iter
    (fun ex ->
      let p = read_file ex in
      try
        let tokens = Lexer.tokenize p in
        let ps = Parser.init_parser tokens in
        let _ = Parser.parse_program ps in
        pr "✔ %s\n" ex
      with _ ->
        pr "✘ Couldn't parse %s\n" ex)
    examples