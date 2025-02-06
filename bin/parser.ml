open Ast
open Lexer
open Printf

type mut_env = (string, bool) Hashtbl.t

type parser_state = {
  tokens : token array;
  mutable index : int;
  length_t : int;
  mutable env_stack : mut_env list;
}

let push_scope (ps : parser_state) : unit =
  let new_table = Hashtbl.create 16 in
  ps.env_stack <- new_table :: ps.env_stack

let pop_scope (ps : parser_state) : unit =
  match ps.env_stack with
  | [] -> failwith "pop_scope: env_stack vuoto!"
  | _top :: rest -> ps.env_stack <- rest

let rec find_var (ps : parser_state) (varname : string) : bool option =
  match ps.env_stack with
  | [] -> None
  | top :: tail ->
      (match Hashtbl.find_opt top varname with
       | Some mutval -> Some mutval
       | None -> find_var_in_tail tail varname)

and find_var_in_tail (scopes : mut_env list) (varname : string) : bool option =
  match scopes with
  | [] -> None
  | top :: tail ->
      match Hashtbl.find_opt top varname with
      | Some mutval -> Some mutval
      | None -> find_var_in_tail tail varname

let add_var (ps : parser_state) (varname : string) (is_mut : bool) : unit =
  match ps.env_stack with
  | [] -> failwith "add_var: nessuno scope presente!"
  | top :: _ ->
      Hashtbl.replace top varname is_mut

let string_of_token = function
  | FN -> "FN"
  | MAIN -> "MAIN"
  | LET -> "LET"
  | MUT -> "MUT"
  | PRINTLN -> "PRINTLN"
  | STRINGFROM -> "STRINGFROM"
  | DOUBLECOLON -> "DOUBLECOLON"
  | PUSHSTR -> "PUSHSTR"
  | DOT -> "DOT"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | LBrace -> "LBrace"
  | RBrace -> "RBrace"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Asterisk -> "Asterisk"
  | Slash -> "Slash"
  | Percent -> "Percent"
  | Eq -> "Eq"
  | DoubleEq -> "DoubleEq"
  | Semicolon -> "Semicolon"
  | Exclamation -> "Exclamation"
  | Arrow -> "Arrow"
  | Colon -> "Colon"
  | Ampersand -> "Ampersand"
  | Comma -> "Comma"
  | NumberTok n -> Printf.sprintf "NumberTok(%d)" n
  | StringTok s -> Printf.sprintf "StringTok(%s)" s
  | Ident s -> Printf.sprintf "Ident(%s)" s
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | LOOP -> "LOOP"
  | BREAK -> "BREAK"
  | EOF -> "EOF"
  | NONE -> "NONE"

let init_parser (toks : token list) : parser_state =
  {
    tokens = Array.of_list toks;
    index = 0;
    length_t = List.length toks;
    env_stack = [];
  }

let current_token (ps : parser_state) : token =
  if ps.index >= ps.length_t then EOF
  else ps.tokens.(ps.index)

let peek_token (ps : parser_state) : token =
  if ps.index + 1 >= ps.length_t then EOF
  else ps.tokens.(ps.index + 1)

let advance_token (ps : parser_state) : unit =
  if ps.index < ps.length_t then
    ps.index <- ps.index + 1

let consume (ps : parser_state) (expected : token) (err_msg : string) : unit =
  let cur = current_token ps in
  if cur = expected then
    advance_token ps
  else
    failwith (Printf.sprintf "Errore di parsing: %s. Trovato '%s', atteso '%s'"
      err_msg
      (string_of_token cur)
      (string_of_token expected))


let rec parse_program (ps : parser_state) : program =
  push_scope ps;
  let rec parse_functions acc =
    match current_token ps with
    | FN ->
        let f = parse_function ps in
        parse_functions (f :: acc)
    | EOF ->
        { functions = List.rev acc }
    | _ ->
        failwith "Errore di parsing: atteso 'fn' o EOF"
  in
  parse_functions []

and parse_params (ps : parser_state) : (string * string) list =
  consume ps LParen "Atteso '(' prima della lista dei parametri";
  let rec loop (acc : (string * string) list) =
    match current_token ps with
    | RParen ->
        advance_token ps;
        List.rev acc
    | Ident param_name ->
        advance_token ps;
        consume ps Colon "Atteso ':' dopo il nome del parametro";
        let ttype =
          match current_token ps with
          | Ampersand ->
              advance_token ps;
              if current_token ps = MUT then begin
                advance_token ps;
                (match current_token ps with
                 | Ident s -> advance_token ps; "&mut " ^ s
                 | _ -> failwith "Atteso il tipo dopo '&mut'")
              end else
                (match current_token ps with
                 | Ident s -> advance_token ps; "&" ^ s
                 | _ -> failwith "Atteso il tipo dopo '&'")
          | Ident s ->
              advance_token ps;
              s
          | _ ->
              failwith "Atteso un identificatore di tipo dopo ':'"
        in
        (match current_token ps with
         | Comma ->
             advance_token ps;
             loop ((param_name, ttype) :: acc)
         | RParen ->
             advance_token ps;
             List.rev ((param_name, ttype) :: acc)
         | _ ->
             failwith "Atteso ',' o ')' dopo un parametro di funzione.")
    | _ ->
        failwith "parse_params: atteso param_name o ')' (errore di sintassi)"
  in
  loop []

and parse_function (ps : parser_state) : function_decl =
  consume ps FN "Manca 'fn'";
  let fname =
    match current_token ps with
    | MAIN -> advance_token ps; "main"
    | Ident s -> advance_token ps; s
    | _ -> failwith "Errore di parsing: atteso il nome della funzione (es. 'main')"
  in
  let params = parse_params ps in
  let return_type = 
    match current_token ps with
    | Arrow ->
        advance_token ps; 
        (match current_token ps with
         | Ident t -> advance_token ps; Some t
         | _ -> failwith "Atteso un tipo dopo '->'")
    | _ -> None
  in
  consume ps LBrace "Manca '{' dopo fn <name>() [-> tipo]";
  push_scope ps;
  List.iter (fun (pname, ptype) ->
    let is_mut =
      if String.length ptype >= 5 && String.sub ptype 0 5 = "&mut" then true else false
    in
    add_var ps pname is_mut
  ) params;
  let body_stmts = parse_function_body ps return_type in
  pop_scope ps;
  consume ps RBrace "Manca '}' alla fine della funzione";
  { name = fname; params; return_type; body = body_stmts; is_local = false }

and parse_function_body (ps: parser_state) (rtype: string option) : statement list =
  let stmts = ref [] in
  let rec loop () =
    match current_token ps with
    | RBrace | EOF -> ()
    | _ ->
        let stmt, _ = parse_one_statement ps in
        stmts := !stmts @ [stmt];
        if current_token ps <> RBrace && current_token ps <> EOF then
          loop ()
  in
  loop ();
  (match (!stmts, rtype) with
  | ([], Some rt) ->
      failwith (Printf.sprintf "Funzione dichiara tipo di ritorno '%s' ma non ha alcuna espressione finale" rt)
  | ([], None) ->
      !stmts
  | (lst, Some _rt) ->
      let last = List.nth lst (List.length lst - 1) in
      let without_last = List.rev (List.tl (List.rev lst)) in
      (match last with
       | Return _ -> lst
       | ExprStmt e ->
           without_last @ [Return e]
       | _ -> 
           failwith "Funzione con tipo di ritorno ma l'ultimo statement non è un'espressione!")
  | (lst, None) ->
      lst)

and parse_one_statement (ps : parser_state) : (statement * bool) =
  match current_token ps with
  | Semicolon -> 
      advance_token ps; 
      (ExprStmt (Number 0), true)
  | FN -> 
      let fdecl = parse_local_function ps in
      (FunctionDecl fdecl, true)
  | IF ->
      let stmt = parse_if_statement ps in
      (stmt, true)
  | LBrace ->
      let blk = parse_block_statement ps in
      (blk, true)
  | LET ->
      advance_token ps;
      let letstmt = parse_let_statement ps in
      (letstmt, true)
  | PRINTLN ->
      let prn = parse_println_statement ps in
      (prn, true)
  | LOOP ->
      let stmt = parse_loop_statement ps in
      (stmt, true)
  | BREAK -> 
      let stmt = parse_break_statement ps in
      (stmt, true)
  | RBrace | EOF ->
      failwith "Unexpected end of block"
  | Ident varname ->
      (match peek_token ps with
      | Eq | LParen | DOT | Semicolon ->
          let stmt, ended_with_semicolon = parse_ident_statement ps varname in
          (stmt, ended_with_semicolon)
      | _ ->
          let e = parse_expr ps in
          (match current_token ps with
          | Semicolon ->
              advance_token ps;
              (ExprStmt e, true)
          | _ ->
              (ExprStmt e, false)))
  | _ ->
      let e = parse_expr ps in
      (match current_token ps with
       | Semicolon ->
           advance_token ps;
           (ExprStmt e, true)
       | _ ->
           (ExprStmt e, false))

and parse_ident_statement (ps : parser_state) (varname: string) : (statement * bool) =
  advance_token ps;
  match current_token ps with
  | LParen ->
      let args = parse_call_arguments ps in
      (match current_token ps with
       | Semicolon ->
           advance_token ps;
           (ExprStmt (Call(varname, args)), true)
       | _ ->
           (ExprStmt (Call(varname, args)), false))
  | DOT -> 
      (match current_token ps with
       | DOT ->
           advance_token ps;
           (match current_token ps with
            | PUSHSTR ->
                advance_token ps;
                consume ps LParen "Manca 'LParen' dopo push_str";
                let content =
                  match current_token ps with
                  | StringTok s -> advance_token ps; s
                  | _ -> failwith "push_str deve avere una stringa come argomento"
                in
                consume ps RParen "Manca 'RParen' dopo push_str";
                consume ps Semicolon "Manca 'Semicolon' dopo push_str";
                let stmt =
                  match find_var ps varname with
                  | None ->
                      failwith (sprintf "Use of undeclared variable '%s'" varname)
                  | Some _ ->
                        PushStr (varname, content)
                in
                (stmt, true)
            | _ -> failwith "Errore di parsing: operazione sconosciuta dopo DOT"
           )
       | _ -> failwith "Errore di parsing: mancava DOT? (ramo improbable)")
  | Eq ->
      parse_assign_statement_after_ident ps varname
  | Semicolon ->
      advance_token ps;
      (ExprStmt (Var varname), true)
  | _ ->
      (match current_token ps with
      | Semicolon ->
          advance_token ps;
          (ExprStmt (Var varname), true)
      | _ ->
          (ExprStmt (Var varname), false))

and parse_call_arguments (ps : parser_state) : expr list =
  consume ps LParen "parse_call_arguments: manca '('";
  let args = ref [] in
  let loop () =
    match current_token ps with
    | RParen -> ()
    | _ ->
        let e = parse_expr ps in
        args := !args @ [e];
        (match current_token ps with
         | RParen -> ()
         | _ ->
             (match current_token ps with
              | Semicolon | LBrace -> ()
              | _ -> ()))
  in
  loop ();
  consume ps RParen "parse_call_arguments: manca ')'";
  !args

and parse_block_statement (ps : parser_state) : statement =
  consume ps LBrace "Manca '{' per iniziare il blocco";
  push_scope ps;
  let stmts = parse_statements ps in
  pop_scope ps;
  consume ps RBrace "Manca '}' alla fine del blocco";
  (match current_token ps with
   | Semicolon -> advance_token ps
   | _ -> ());
  Block stmts

and parse_statements (ps : parser_state) : statement list =
  let rec loop acc =
    match current_token ps with
    | RBrace | EOF ->
        List.rev acc
    | _ ->
        let stmt, _ = parse_one_statement ps in
        loop (stmt :: acc)
  in
  loop []

and parse_let_statement (ps : parser_state) : statement =
  let is_mut = 
    match current_token ps with
    | MUT ->
        advance_token ps;
        true
    | _ ->
        false
  in
  let varname =
    match current_token ps with
    | Ident s -> advance_token ps; s
    | _ -> failwith "Atteso un identificatore dopo 'let [mut]'"
  in
  consume ps Eq "Manca '=' dopo 'let [mut] <ident>'";
  let e = parse_expr ps in
  consume ps Semicolon "Manca ';' dopo dichiarazione let";
  let is_mutable = 
    match e with
    | BorrowMut _ -> true
    | _ -> is_mut
  in
  add_var ps varname is_mutable;
  Let (is_mutable, varname, e)

and parse_assign_statement_after_ident (ps : parser_state) (varname : string) : (statement * bool) =
  consume ps Eq "Manca '=' in assegnamento";
  let rhs_expr = parse_expr ps in
  consume ps Semicolon "Manca ';' dopo assegnamento";
  let stmt = 
  match find_var ps varname with
   | None ->
       failwith (sprintf "Use of undeclared variable '%s'" varname)
   | Some _ ->   
       Assign (varname, rhs_expr) 
  in
  (stmt, true)

and parse_println_statement (ps : parser_state) : statement =
  advance_token ps; 
  consume ps LParen "Manca '(' dopo println!";
  let arg_expr = parse_expr ps in
  consume ps RParen "Manca ')' dopo println!";
  (match current_token ps with
   | RBrace -> ()  
   | _ -> consume ps Semicolon "Manca ';' dopo println!()"
  );
  Println arg_expr

and parse_loop_statement (ps : parser_state) : statement =
  consume ps LOOP "Atteso 'loop'";
  consume ps LBrace "Atteso '{' dopo loop";
  let stmts = parse_statements ps in
  consume ps RBrace "Atteso '}' dopo il blocco loop";
  Loop (Block stmts)

and parse_break_statement (ps : parser_state) : statement =
  consume ps BREAK "Atteso 'break'";
  consume ps Semicolon "Atteso ';' dopo break";
  Break

and parse_expr (ps : parser_state) : expr =
  parse_equality ps

and parse_equality (ps : parser_state) : expr =
  let left = parse_add_sub ps in
  parse_equality_tail ps left

and parse_equality_tail (ps : parser_state) (left : expr) : expr =
  match current_token ps with
  | DoubleEq ->
      advance_token ps;
      let right = parse_add_sub ps in
      let new_expr = BinOp(left, Equal, right) in
      parse_equality_tail ps new_expr
  | _ -> left

and parse_add_sub (ps : parser_state) : expr =
  let left = parse_mul_div_mod ps in
  parse_add_sub_tail ps left

and parse_add_sub_tail (ps : parser_state) (left : expr) : expr =
  match current_token ps with
  | Plus ->
      advance_token ps;
      let right = parse_mul_div_mod ps in
      let new_expr = BinOp(left, Add, right) in
      parse_add_sub_tail ps new_expr
  | Minus ->
      advance_token ps;
      let right = parse_mul_div_mod ps in
      let new_expr = BinOp(left, Sub, right) in
      parse_add_sub_tail ps new_expr
  | _ -> left

and parse_mul_div_mod (ps : parser_state) : expr =
  let left = parse_term ps in
  parse_mul_div_mod_tail ps left

and parse_mul_div_mod_tail (ps : parser_state) (left : expr) : expr =
  match current_token ps with
  | Asterisk ->
      advance_token ps;
      let right = parse_term ps in
      let new_expr = BinOp(left, Mul, right) in
      parse_mul_div_mod_tail ps new_expr
  | Slash ->
      advance_token ps;
      let right = parse_term ps in
      let new_expr = BinOp(left, Div, right) in
      parse_mul_div_mod_tail ps new_expr
  | Percent ->
      advance_token ps;
      let right = parse_term ps in
      let new_expr = BinOp(left, Mod, right) in
      parse_mul_div_mod_tail ps new_expr
  | _ -> left

and parse_term (ps : parser_state) : expr =
  match current_token ps with
  | LBrace ->
      parse_block_expr ps
  | Ampersand ->
      advance_token ps;
      if current_token ps = MUT then begin
        advance_token ps;
        let e = parse_term ps in
        BorrowMut e
      end else begin
        let e = parse_term ps in
        Borrow e
      end
  | NumberTok n ->
      advance_token ps;
      Number n
  | StringTok s -> 
      advance_token ps;
      StringLiteral s
  | TRUE ->
      advance_token ps;
      Boolean true
  | FALSE ->
      advance_token ps;
      Boolean false
  | STRINGFROM -> 
      advance_token ps;
      consume ps LParen "Manca '(' dopo String::from";
      let content =
        match current_token ps with
        | StringTok s -> advance_token ps; StringLiteral s
        | _ -> failwith "String::from() deve contenere una stringa"
      in
      consume ps RParen "Manca ')' dopo String::from";
      StringFrom content
  | Ident s ->
      advance_token ps;
      (match current_token ps with
       | LParen ->
           let args = parse_call_arguments ps in
           Call(s, args)
       | _ ->
           Var s)
  | _ -> failwith "Errore di parsing: atteso un numero, un identificatore o una stringa"

and parse_if_statement (ps : parser_state) : statement =
  consume ps IF "Atteso 'if'";
  let cond_expr = parse_expr ps in
  let then_stmt = 
    match current_token ps with
    | LBrace -> parse_block_statement ps
    | _ -> 
        let s, _ = parse_one_statement ps in s
  in
  let else_stmt =
    match current_token ps with
    | ELSE ->
        advance_token ps;
        (match current_token ps with
         | LBrace -> Some (parse_block_statement ps)
         | _ -> let s, _ = parse_one_statement ps in Some s)
    | _ -> None
  in
  If (cond_expr, then_stmt, else_stmt)

and parse_block_expr (ps : parser_state) : expr =
  consume ps LBrace "Manca '{' per iniziare il blocco";
  push_scope ps;
  let stmts = parse_statements ps in
  pop_scope ps;
  consume ps RBrace "Manca '}' alla fine del blocco";
  BlockExpr stmts

and parse_local_function (ps : parser_state) : function_decl =
  consume ps FN "Manca 'fn'";
  let fname =
    match current_token ps with
    | Ident s -> advance_token ps; s
    | _ -> failwith "Errore di parsing: atteso il nome della funzione locale"
  in
  let params = parse_params ps in
  let return_type =
    match current_token ps with
    | Arrow ->
        advance_token ps;
        (match current_token ps with
         | Ident t -> advance_token ps; Some t
         | _ -> failwith "Atteso un tipo dopo '->'")
    | _ -> None
  in
  consume ps LBrace "Manca '{' dopo la dichiarazione di funzione locale";
  let body_stmts = parse_statements ps in
  consume ps RBrace "Manca '}' alla fine della funzione locale";
  { name = fname; params; return_type; body = body_stmts; is_local = true }
