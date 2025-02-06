(* main.ml - Entry point per TinyRust in OCaml *)

open Ast
open Lexer
open Parser
open Interpreter

let () =
  (* Esempio di codice TinyRust *)
  let source_code = {|
    fn presta(y: &String) {
      println!("{y}");
    }
    fn bar(x: i32) {
      let mut y = 4;
      y = y*x;
      println!("{y}");
    }
    fn main () {
      let x = 3;
      bar(x);
      let mut z = String::from("Hello");
      z.push_str(", world");
      {
        let z = String::from("Ciao mondo!");
        println!("{z}");
      }
      println!("{z}");
      println!("{z}");
      
      let mut g = String::from("Ciao");
      let f = &mut g;
      g.push_str(", mondo");
      println!("{f}");
      let q = String::from("Prova");
      presta(&q);
      println!("{q}");

      let b = true;
      println!("{b}");

      let c = 4 == 3;
      println!("{c}");

      let q = 3;

      if q % 2 == 0 {
        println!("pari");
      } else {
        println!("dispari");
      }

      let mut i = 0;
      loop {
        let mut j = 0;
        loop {
          if j==2 { break; }
          else { println!("{i}"); println!("{j}"); j = j + 1; }
        }
        if i==2 { break; }
        else { i = i + 1; }
      }
    }
    |} in

  (* 1) LEXER: trasforma il sorgente in token *)
  let tokens = tokenize source_code in
  print_endline "--- TOKENS ---";
  Printf.printf "Lunghezza tokens: %d\n" (List.length tokens);
  List.iter (fun tk ->
    match tk with
    | FN -> print_endline "FN"
    | MAIN -> print_endline "MAIN"
    | LET -> print_endline "LET"
    | MUT -> print_endline "MUT"
    | PRINTLN -> print_endline "PRINTLN"
    | LParen -> print_endline "LParen"
    | RParen -> print_endline "RParen"
    | LBrace -> print_endline "LBrace"
    | RBrace -> print_endline "RBrace"
    | STRINGFROM -> print_endline "STRINGFROM"
    | DOUBLECOLON -> print_endline "DOUBLECOLON"
    | PUSHSTR -> print_endline "PUSHSTR"
    | DOT -> print_endline "DOT"
    | StringTok s -> Printf.printf "StringTok(%s)\n" s
    | Plus -> print_endline "Plus"
    | Minus -> print_endline "Minus"
    | Asterisk -> print_endline "Asterisk"
    | Slash -> print_endline "Slash"
    | Percent -> print_endline "Percent"
    | Eq -> print_endline "Eq"
    | DoubleEq -> print_endline "DoubleEq"
    | Semicolon -> print_endline "Semicolon"
    | Exclamation -> print_endline "Exclamation"
    | NumberTok n -> Printf.printf "NumberTok(%d)\n" n
    | Ident s -> Printf.printf "Ident(%s)\n" s
    | Arrow -> print_endline "Arrow"
    | Colon -> print_endline "Colon"
    | Comma -> print_endline "Comma"
    | Ampersand -> print_endline "Ampersand"
    | TRUE -> print_endline "TRUE"
    | FALSE -> print_endline "FALSE"
    | EOF -> print_endline "EOF"
    | IF -> print_endline "IF"
    | ELSE -> print_endline "ELSE"
    | LOOP -> print_endline "LOOP"
    | BREAK -> print_endline "BREAK"
    | NONE -> print_endline "NONE"
  ) tokens;

  (* 2) PARSER: costruisce l'AST *)
  print_endline "Lexing concluso. Inizio il parsing...";
  let ps = init_parser tokens in
  let prog = parse_program ps in
  print_ast prog; 
  print_endline "--- PARSING COMPLETED ---";

  (* 3) INTERPRETE: esegue il programma (run_program) *)
  print_endline "--- OUTPUT ---";
  let gas = 200 in
  run_program prog gas;
