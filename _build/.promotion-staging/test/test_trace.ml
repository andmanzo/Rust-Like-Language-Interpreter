(**

  ⚠️ READ THIS ⚠️

  This file is a template for testing the provided examples of Tiny Rust.

  These tests make a lot of assumptions about your code, therefore you have
  the option to freely change them to work with your code, or vice versa.

  The test routine depends on a library called `ppx_expect`.
  To install `ppx_expect` run the command:

  ```
  opam install ppx_expect
  ```

  And add `ppx_expect` next to `ppx_inline_test` in `test/dune`

  Here we assume that the `trace` function returns a `result` and
  never raises an exception.

*)

open Tinyrust
open Interpreter
open Common




(** ------------------------------------------
    Helper functions
    ------------------------------------------ *)

let string_of_mut = function
  | Mutable -> "mutable"
  | Immutable -> "immutable"

let string_of_trace_error = function
  | TypeError s ->
      spr "[TypeError] %s" s
  | CannotMutate x ->
      spr "[CannotMutate] cannot mutate immutable variable %s" x
  | UnboundVar x ->
      spr "[UnboundVar] %s not defined in this scope" x
  | MovedValue x ->
      spr "[MovedValue] borrow of moved value %s" x
  | OutOfGas i ->
      spr "[OutOfGas] trace run out of gas (%d)" i
  | NotInLoop ->
    "[NotInLoop] cannot break outside of a loop"
  | MutBorrowOfNonMut x ->
      spr
        "[MutBorrowOfNonMut] cannot borrow %s as mutable, as it is not \
         declared as mutable"
        x
  | DataRace (x, mut1, mut2) ->
      spr "[DataRace] cannot borrow %s as %s because it is also borrowed as %s"
        x (string_of_mut mut1) (string_of_mut mut2)

    (* Note: a data race is when:
        - there are two or more references defined on
          the same variable, in the same scope,
        - one of them is mutable
    *)
[@@ocamlformat "disable"]

(** ------------------------------------------
    Definition of tests
    ------------------------------------------ *)

(* Feel free to increase or decrease the amount of steps (gas) *)
let tests : (string * int * string trace_result) array =
  [|
    ("01-print.rs",           25, Ok "3\n4\n");
    ("02-intError.rs",        25, Error (CannotMutate "x"));
    ("03-intOk.rs",           25, Ok "7\n");
    ("04-stringError.rs",     25, Error (CannotMutate "x"));
    ("05-stringOk.rs",        25, Ok "Ciao, mondo\n");
    ("06-scopeOk.rs",         25, Ok "6\n3\n");
    ("07-scopeError.rs",      25, Error (UnboundVar "y"));
    ("08-func.rs",            25, Ok "10\n");
    ("09-proc.rs",            25, Ok "7\n");
    ("10-ifThenElse.rs",      25, Ok "dispari\n");
    ("11-ownError.rs",        25, Error (MovedValue "x"));
    ("12-ownFnError.rs",      25, Error (MovedValue "x"));
    ("13-borrow.rs",          25, Ok "Ciao\nCiao\n");
    ("14-borrowFn.rs",        25, Ok "il parametro prestato: Ciao\nil parametro x: Ciao\n" );
    ("15-borrowError.rs",     25, Error (DataRace ("x", Mutable, Immutable)));
    ("16-borrowMut.rs",       25, Ok "Ciao, mondo\nCiao, mondo\n");
    ("17-borrowMutError.rs",  40, Error (MutBorrowOfNonMut "x"));
    ("18-loop.rs",            50, Error (OutOfGas 50));
    ("19-loopBreak.rs",       50, Ok "3\n2\n1\n0\n");
    ("20-loopNested.rs",      50, Ok "0,0\n0,1\n1,0\n1,1\n2,0\n2,1\n");
    ("21-exprBlock.rs",       25, Ok "7\n");
    ("22-funExpr.rs",         25, Error (UnboundVar "interna"));
    ("23-scopeCheck.rs",      25, Error (UnboundVar "y"));
  |] [@@ocamlformat "disable"]

(** ------------------------------------------
    Start of trace tests
    ------------------------------------------ *)

let%expect_test "test_trace" =
  Array.iter2
    (fun (name, prog) (_, gas, expected) ->
      let prog = Parser.parse_program (Parser.init_parser (Lexer.tokenize prog)) in

      (* We're assuming the return type of [Trace.trace_prog] is:

         [(Ast.expression list) * (string trace_result)]

         We run the program and ignore the trace, as we only care about
         the program's textual output (and whether it is Ok or Error).

         If you used exceptions, use the [try .. with] construct here
         and convert the exception to a result.
      *)
      let _, (actual : string trace_result) = Interpreter.run_program prog gas in

      let icon =
        match (actual, expected) with
        | Ok _, Ok _ | Error _, Error _ -> "✔"
        | Ok _, Error _ | Error _, Ok _ -> "✘"
      in

      pr "------------------------\n%s %s\n------------------------\n" icon name;

      List.iter
        (fun (title, result) ->
          let kind, output =
            match result with
            | Ok output -> ("Ok", output)
            | Error err -> ("Error", string_of_trace_error err)
          in
          pr "%-9s %-9s\n%s\n\n" title kind output)
        [ ("Actual output:", actual); ("Expected:", expected) ])
    examples_dict tests;
  [%expect {|
    ------------------------
    ✔ 01-print.rs
    ------------------------
    Actual output: Ok
    3
    4


    Expected: Ok
    3
    4


    ------------------------
    ✔ 02-intError.rs
    ------------------------
    Actual output: Error
    [TypeError] Failure("[CannotMutate] cannot mutate immutable variable x")

    Expected: Error
    [CannotMutate] cannot mutate immutable variable x

    ------------------------
    ✔ 03-intOk.rs
    ------------------------
    Actual output: Ok
    7


    Expected: Ok
    7


    ------------------------
    ✔ 04-stringError.rs
    ------------------------
    Actual output: Error
    [TypeError] Failure("[CannotMutate] cannot mutate immutable variable x")

    Expected: Error
    [CannotMutate] cannot mutate immutable variable x

    ------------------------
    ✔ 05-stringOk.rs
    ------------------------
    Actual output: Ok
    Ciao, mondo


    Expected: Ok
    Ciao, mondo


    ------------------------
    ✔ 06-scopeOk.rs
    ------------------------
    Actual output: Ok
    6
    3


    Expected: Ok
    6
    3


    ------------------------
    ✔ 07-scopeError.rs
    ------------------------
    Actual output: Error
    [TypeError] Failure("[UnboundVar] y not defined in this scope")

    Expected: Error
    [UnboundVar] y not defined in this scope

    ------------------------
    ✔ 08-func.rs
    ------------------------
    Actual output: Ok
    10


    Expected: Ok
    10


    ------------------------
    ✔ 09-proc.rs
    ------------------------
    Actual output: Ok
    7


    Expected: Ok
    7


    ------------------------
    ✔ 10-ifThenElse.rs
    ------------------------
    Actual output: Ok
    dispari


    Expected: Ok
    dispari


    ------------------------
    ✔ 11-ownError.rs
    ------------------------
    Actual output: Error
    [TypeError] Failure("[MovedValue] borrow of moved value x")

    Expected: Error
    [MovedValue] borrow of moved value x

    ------------------------
    ✔ 12-ownFnError.rs
    ------------------------
    Actual output: Error
    [TypeError] Failure("[MovedValue] borrow of moved value x")

    Expected: Error
    [MovedValue] borrow of moved value x

    ------------------------
    ✔ 13-borrow.rs
    ------------------------
    Actual output: Ok
    Ciao
    Ciao


    Expected: Ok
    Ciao
    Ciao


    ------------------------
    ✔ 14-borrowFn.rs
    ------------------------
    Actual output: Ok
    il parametro prestato: Ciao
    il parametro x: Ciao


    Expected: Ok
    il parametro prestato: Ciao
    il parametro x: Ciao


    ------------------------
    ✔ 15-borrowError.rs
    ------------------------
    Actual output: Error
    [TypeError] Failure("[DataRace] cannot borrow x as mutable because it is also borrowed as immutable")

    Expected: Error
    [DataRace] cannot borrow x as mutable because it is also borrowed as immutable

    ------------------------
    ✔ 16-borrowMut.rs
    ------------------------
    Actual output: Ok
    Ciao, mondo
    Ciao, mondo


    Expected: Ok
    Ciao, mondo
    Ciao, mondo


    ------------------------
    ✔ 17-borrowMutError.rs
    ------------------------
    Actual output: Error
    [TypeError] Failure("[MutBorrowOfNonMut] cannot borrow x as mutable, as it is not declared as mutable")

    Expected: Error
    [MutBorrowOfNonMut] cannot borrow x as mutable, as it is not declared as mutable

    ------------------------
    ✔ 18-loop.rs
    ------------------------
    Actual output: Error
    [TypeError] Tinyrust.Interpreter.Interpreter.OutOfGas(0)

    Expected: Error
    [OutOfGas] trace run out of gas (50)

    ------------------------
    ✔ 19-loopBreak.rs
    ------------------------
    Actual output: Ok
    3
    2
    1
    0


    Expected: Ok
    3
    2
    1
    0


    ------------------------
    ✔ 20-loopNested.rs
    ------------------------
    Actual output: Ok
    0,0
    0,1
    1,0
    1,1
    2,0
    2,1


    Expected: Ok
    0,0
    0,1
    1,0
    1,1
    2,0
    2,1


    ------------------------
    ✔ 21-exprBlock.rs
    ------------------------
    Actual output: Ok
    7


    Expected: Ok
    7


    ------------------------
    ✔ 22-funExpr.rs
    ------------------------
    Actual output: Error
    [TypeError] Failure("[UnboundVar] interna non definita")

    Expected: Error
    [UnboundVar] interna not defined in this scope

    ------------------------
    ✔ 23-scopeCheck.rs
    ------------------------
    Actual output: Error
    [TypeError] Failure("[UnboundVar] y not defined in this scope")

    Expected: Error
    [UnboundVar] y not defined in this scope
    |}]