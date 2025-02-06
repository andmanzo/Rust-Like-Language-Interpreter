
open Ast
open Common

module Interpreter = struct

  let output_buffer = Buffer.create 1024

  type value =
    | IntVal of int
    | StringVal of string
    | BoolVal of bool
    | VoidVal   
    | MovedVal
    | RefVal of { ref_name : string; mutable_ref : bool}
    | FunctionVal of function_decl

    type env_entry = {
      mutable is_mutable: bool;
      mutable value: value;
    }

  type env_stack = (string, env_entry) Hashtbl.t list

  type func_map = (string, function_decl) Hashtbl.t

  type borrow_info = {
    mutable immut_count : int;
    mutable has_mut : bool;
  }

  let borrow_table : (string, borrow_info) Hashtbl.t = Hashtbl.create 16

  let register_borrow var_name is_mut =
    match Hashtbl.find_opt borrow_table var_name with
    | None ->
        if is_mut then
          Hashtbl.add borrow_table var_name { immut_count = 0; has_mut = true }
        else
          Hashtbl.add borrow_table var_name { immut_count = 1; has_mut = false }
    | Some info ->
        if is_mut then begin
          info.has_mut <- true;
          ()
        end else begin
          if info.has_mut then
            failwith ("cannot borrow '" ^ var_name ^ "' as immutable because it is already borrowed as mutable")
          else
            info.immut_count <- info.immut_count + 1
        end

  let unregister_borrow var_name is_mut =
    match Hashtbl.find_opt borrow_table var_name with
    | None -> ()
    | Some info ->
        if is_mut then
          info.has_mut <- false
        else
          info.immut_count <- info.immut_count - 1;
        if info.immut_count <= 0 && not info.has_mut then
          Hashtbl.remove borrow_table var_name

  let build_function_map (prog: program) : func_map =
    let fm = Hashtbl.create 16 in
    List.iter (fun f ->
      Hashtbl.add fm f.name f
    ) prog.functions;
    fm


  let push_scope (stack : env_stack) : env_stack =
    let new_table = Hashtbl.create 16 in
    new_table :: stack

  let pop_scope (stack : env_stack) : env_stack =
    match stack with
    | [] -> failwith "pop_scope: env_stack vuoto!"
    | top :: rest -> 
        Hashtbl.iter (fun _ entry ->
          match entry.value with
          | RefVal { ref_name; mutable_ref } ->
              unregister_borrow ref_name mutable_ref
          | _ -> ()
        ) top;
        rest

  let rec find_var (stack : env_stack) (varname : string) : env_entry option =
    match stack with
    | [] -> None
    | top :: tail ->
        (match Hashtbl.find_opt top varname with
         | Some entry -> Some entry
         | None -> find_var tail varname)

  let add_var (stack : env_stack) (varname : string) (is_mut : bool) (v: value) : unit =
    match stack with
    | [] -> failwith "add_var: env_stack vuoto!"
    | top :: _ ->
        Hashtbl.replace top varname { is_mutable = is_mut; value = v }

  exception ReturnException of value

  exception BreakException

  exception OutOfGas of int

  let var_to_expr (var_name : string) : expr =
    Var var_name

  let check_gas (gas: int ref) =
    if !gas <= 0 then raise (OutOfGas !gas)
    else gas := !gas - 1

let rec deref_value (stack : env_stack) (v : value) : value =
  match v with
  | RefVal { ref_name; _ } ->
      (match find_var stack ref_name with
       | Some underlying -> deref_value stack underlying.value
       | None -> failwith ("Errore di dereferenziazione: variabile '" ^ ref_name ^ "' non trovata"))
  | _ -> v

  let interpolate (stack : env_stack) (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec aux i =
    if i >= len then ()
    else if s.[i] = '{' then
      let j = ref (i + 1) in
      while !j < len && s.[!j] <> '}' do
        incr j
      done;
      if !j >= len then
        failwith "String interpolation error: manca '}'"
      else
        let var_name = String.sub s (i + 1) (!j - i - 1) |> String.trim in
        let value =
          match find_var stack var_name with
          | None -> failwith ("[UnboundVar] " ^ var_name ^ " not defined in this scope")
          | Some v -> v.value
        in
        let str_val =
          match value with
          | IntVal n -> string_of_int n
          | StringVal s -> s
          | BoolVal b -> string_of_bool b
          | RefVal _ ->
              let deref = deref_value stack value in
              (match deref with
              | IntVal n -> string_of_int n
              | StringVal s -> s
              | BoolVal b -> string_of_bool b
              | MovedVal -> failwith ("[MovedValue] borrow of moved value " ^ var_name)
              | _ -> failwith ("String interpolation error: tipo non supportato per la variabile '" ^ var_name ^ "'"))
          | MovedVal -> failwith ("[MovedValue] borrow of moved value " ^ var_name)
          | _ -> failwith ("String interpolation error: tipo non supportato per la variabile '" ^ var_name ^ "'")
        in
        Buffer.add_string buf str_val;
        aux (!j + 1)
    else (
      Buffer.add_char buf s.[i];
      aux (i + 1)
    )
  in
  aux 0;
  Buffer.contents buf

  let rec check_local_function (f: function_decl) : unit =
  let local_env = Hashtbl.create 16 in
  List.iter (fun (pname, _) ->
    Hashtbl.add local_env pname true  (* Il valore 'true' è un placeholder *)
  ) f.params;
  List.iter (fun stmt -> check_statement local_env stmt) f.body

  and check_expr (env: (string, bool) Hashtbl.t) (e: expr) : unit =
  match e with
  | Number _ | StringLiteral _ | Boolean _ -> ()
  | Var v ->
      if not (Hashtbl.mem env v) then
        failwith (Printf.sprintf "[UnboundVar] %s not defined in this scope" v)
  | BinOp (e1, _, e2) ->
      check_expr env e1; check_expr env e2
  | Call (_, args) ->
      List.iter (check_expr env) args
  | Borrow inner | BorrowMut inner ->
      check_expr env inner
  | StringFrom inner ->
      check_expr env inner
  | BlockExpr stmts ->
      check_statements (clone_env env) stmts  (* clone_env crea una copia locale dell'ambiente *)
      
  and check_statement (env: (string, bool) Hashtbl.t) (s: statement) : unit =
    match s with
    | Let (_, var, e) ->
        check_expr env e;
        Hashtbl.add env var true
    | Assign (var, e) ->
        if not (Hashtbl.mem env var) then
          failwith (Printf.sprintf "[UnboundVar] %s not defined in this scope" var);
        check_expr env e
    | ExprStmt e ->
        check_expr env e
    | Return e ->
        check_expr env e
    | Println e ->
        check_expr env e
    | PushStr (_, _) ->
        () 
    | If (cond, s_then, s_else_opt) ->
        check_expr env cond;
        check_statement env s_then;
        (match s_else_opt with
        | Some s_else -> check_statement env s_else
        | None -> ())
    | Loop s ->
        check_statement env s
    | Break -> ()
    | Block stmts ->
        let new_env = clone_env env in
        check_statements new_env stmts
    | FunctionDecl f ->
        let new_env = Hashtbl.create 16 in
        List.iter (fun (p, _) -> Hashtbl.add new_env p true) f.params;
        check_statements new_env f.body

  and check_statements env stmts =
    List.iter (check_statement env) stmts

  and clone_env (env: (string, bool) Hashtbl.t) : (string, bool) Hashtbl.t =
    let new_env = Hashtbl.create (Hashtbl.length env) in
    Hashtbl.iter (fun key value -> Hashtbl.add new_env key value) env;
    new_env


  let rec run_program (prog: program) (gas: int) : (unit * string trace_result) =
    Buffer.clear output_buffer;
    try
    let fm = build_function_map prog in
    match Hashtbl.find_opt fm "main" with
    | None ->
        ((), Error (TypeError "No 'main' function found!"))
    | Some main_fun ->
        let stack : env_stack = [] in
        let stack = push_scope stack in
        let gas_ref = ref gas in
        ignore (run_function fm stack main_fun [] gas_ref);  
        ((), Ok (Buffer.contents output_buffer))
    with e ->
      ((), Error (TypeError (Printexc.to_string e)))

  and run_function (fm: func_map) (stack: env_stack) (f: function_decl) (args: value list) (gas : int ref) : value =
    let new_stack = 
      if f.is_local then
        push_scope []
      else
        push_scope stack
    in
    let param_count = List.length f.params in
    let args_count = List.length args in
    if param_count <> args_count then
      failwith (Printf.sprintf "Numero di argomenti errato per la funzione '%s': attesi %d, trovati %d"
                  f.name param_count args_count);
    List.iter2 (fun (pname, ptype) arg_val ->
      let is_mut =
        if String.length ptype >= 5 && String.sub ptype 0 5 = "&mut" then true
        else false
      in
      Hashtbl.replace (List.hd new_stack) pname { is_mutable = is_mut; value = arg_val}
    ) f.params args;
    let ret_value = 
      try
        let _final_stack = run_statements fm new_stack f.body gas in
        (match f.return_type with
         | Some rt -> failwith (Printf.sprintf "La funzione '%s' con return type '%s' non ha eseguito un return finale." f.name rt)
         | None -> VoidVal)
      with
      | ReturnException v -> v
    in
    ignore (pop_scope new_stack);
    ret_value

  and run_statements_no_scope (fm: func_map) (stack: env_stack) (stmts: statement list) (gas : int ref) : unit =
    List.iter (fun s -> ignore (run_statement fm stack s gas); ) stmts

  and run_statements (fm: func_map) (stack: env_stack) (stmts: statement list) (gas : int ref) : env_stack =
    let rec loop stck = function
      | [] -> stck
      | s :: rest ->
          let stck' = run_statement fm stck s gas in
          loop stck' rest
    in
    loop stack stmts

  and run_statement (fm: func_map) (stack: env_stack) (st: statement) (gas : int ref) : env_stack =
    check_gas gas;
    match st with
    | FunctionDecl f ->
        check_local_function f;
        add_var stack f.name false (FunctionVal f);
        stack
    | Let (is_mut, varname, e) ->
        let v = eval_expr fm stack e gas in
        add_var stack varname is_mut v;
        stack
    | Assign (varname, e) ->
        let value = eval_expr fm stack e gas in
        (match find_var stack varname with
         | Some entry ->
              if not entry.is_mutable then
                failwith ("[CannotMutate] cannot mutate immutable variable " ^ varname)
              else begin
                (match Hashtbl.find_opt borrow_table varname with
                  | Some _ -> failwith ("cannot assign to variable '" ^ varname ^ "' because it is borrowed")
                  | None -> entry.value <- value)
              end;
              stack
         | None ->
             failwith ("Variable '" ^ varname ^ "' not found (runtime)"))
    | Println e ->
        let v = eval_expr_no_move fm stack e gas in
        let output_str =
        match v with
        | IntVal i -> string_of_int i ^ "\n"
        | StringVal s ->
          if String.contains s '{' then
            (interpolate stack s) ^ "\n"
          else
            s ^ "\n"
        | BoolVal b -> string_of_bool b ^ "\n"
        | VoidVal -> "void\n"
        | MovedVal -> failwith "Cannot print a moved value"
        | RefVal { ref_name; mutable_ref = _ } -> "Reference to " ^ ref_name ^ "\n"
        | FunctionVal f -> "function " ^ f.name ^ "\n"
        in
        Buffer.add_string output_buffer output_str;
        stack
    | PushStr (varname, s) ->
        begin
          match find_var stack varname with
          | Some entry ->
            if not entry.is_mutable then
              failwith ("[CannotMutate] cannot mutate immutable variable " ^ varname)
            else
            (match entry.value with
            | RefVal { ref_name; mutable_ref = true } ->
              (match eval_expr_no_move fm stack (var_to_expr ref_name) gas with
              | StringVal existing ->
                  let entry = 
                      match find_var stack ref_name with
                      | Some e -> e
                      | None -> failwith ("Variabile '" ^ ref_name ^ "' non trovata (runtime)")
                    in
                  add_var stack ref_name entry.is_mutable (StringVal (existing ^ s))
              | _ ->
                  failwith "La variabile referenziata non contiene una stringa")
            | RefVal { ref_name; mutable_ref = false } ->
              ignore(ref_name);
              failwith ("Non è possibile applicare push_str su una reference non mutabile")
            | StringVal existing -> 
              (match Hashtbl.find_opt borrow_table varname with
              | Some info ->
                  if info.has_mut then (
                    let entry = 
                      match find_var stack varname with
                      | Some e -> e
                      | None -> failwith ("Variabile '" ^ varname ^ "' non trovata (runtime)")
                    in
                    add_var stack varname entry.is_mutable (StringVal (existing ^ s));
                  ) else
                  failwith ("[DataRace] cannot borrow " ^ varname ^ " as mutable because it is also borrowed as immutable")
              | None ->
                  let entry = 
                      match find_var stack varname with
                      | Some e -> e
                      | None -> failwith ("Variabile '" ^ varname ^ "' non trovata (runtime)")
                    in
                  add_var stack varname entry.is_mutable (StringVal (existing ^ s)))
            | _ -> failwith ("push_str applicato a una variabile che non è StringVal"))
          | None -> failwith ("Variabile '" ^ varname ^ "' non trovata (runtime)")
        end;
        stack
    | ExprStmt e ->
        let _ = eval_expr_no_move fm stack e gas in
        stack
    | Return e ->
        let v = eval_expr_no_move fm stack e gas in
        raise (ReturnException v)
    | If (cond, then_stmt, else_stmt) ->
        let cond_val = eval_expr_no_move fm stack cond gas in
        (match cond_val with
        | BoolVal true -> 
            (match then_stmt with
            | Block stmts -> run_statements_no_scope fm stack stmts gas; stack
            | _ -> run_statement fm stack then_stmt gas)
        | BoolVal false ->
            (match else_stmt with
              | Some (Block stmts) -> run_statements_no_scope fm stack stmts gas; stack
              | Some s -> run_statement fm stack s gas
              | None -> stack)
        | _ ->
            failwith "La condizione dell'if non è un valore booleano")
    | Break -> raise BreakException
    | Loop (Block stmts) ->
        let rec loop_loop () =
          try
            run_statements_no_scope fm stack stmts gas;
            loop_loop ()
          with BreakException -> stack
        in
        loop_loop ()
    | Loop stmt ->
        let rec loop_loop () =
          try
            let _ = run_statement fm stack stmt gas in
            loop_loop ()
          with BreakException -> stack
        in
        loop_loop ()
    | Block stmts ->
        let new_stack = push_scope stack in
        let _ = run_statements fm new_stack stmts gas in
        ignore (pop_scope new_stack);
        stack

  and eval_expr (fm: func_map) (stack: env_stack) (e: expr) (gas : int ref): value =
    match e with
    | Number n -> IntVal n
    | StringLiteral s -> StringVal s
    | Boolean b -> BoolVal b
    | StringFrom e ->
        let v = eval_expr fm stack e gas in
        (match v with
         | StringVal s -> StringVal s
         | _ -> failwith "String::from() deve contenere una stringa")
    | Borrow inner_expr ->
        (match inner_expr with
         | Var v ->
             (match find_var stack v with
              | None -> failwith ("Variabile '" ^ v ^ "' non dichiarata")
              | Some _ ->
                  register_borrow v false;  
                  RefVal { ref_name = v; mutable_ref = false })
         | _ -> failwith "Il borrowing è permesso solo su variabili")
    | BorrowMut inner_expr ->
        (match inner_expr with
         | Var v ->
             (match find_var stack v with
              | None -> failwith ("Variabile '" ^ v ^ "' non dichiarata")
              | Some entry ->
                if not entry.is_mutable then 
                  failwith ("[MutBorrowOfNonMut] cannot borrow " ^ v ^ " as mutable, as it is not declared as mutable")
                else
                  (match entry.value with
                    | RefVal _ -> entry.value
                    | _ -> RefVal { ref_name = v; mutable_ref = true}))
         | _ -> failwith "Il borrowing mutabile è permesso solo su variabili")
    | Var v ->
        (match find_var stack v with
         | None -> failwith ("Variabile '" ^ v ^ "' non dichiarata")
         | Some entry -> 
            match entry.value with
            | MovedVal -> failwith ("[MovedValue] borrow of moved value " ^ v)
            | RefVal _ -> deref_value stack entry.value
            | StringVal s ->
             entry.value <- MovedVal;
             StringVal s
            | _ -> entry.value)
    | BinOp (e1, op, e2) ->
        let v1 = eval_expr_no_move fm stack e1 gas in
        let v2 = eval_expr_no_move fm stack e2 gas in
        (match op with
         | Add | Sub | Mul | Div | Mod ->
             (match (v1, v2) with
              | (IntVal l, IntVal r) ->
                  (match op with
                  | Add -> IntVal (l + r)
                  | Sub -> IntVal (l - r)
                  | Mul -> IntVal (l * r)
                  | Div -> if r = 0 then failwith "Division by zero" else IntVal (l / r)
                  | Mod -> if r = 0 then failwith "Modulo by zero" else IntVal (l mod r)
                  | _ -> failwith "Operazione aritmetica non valida")
              | _ -> failwith "Operazione non valida su non interi")
        | Equal ->
            let result =
              match (v1, v2) with
              | (IntVal l, IntVal r) -> l = r
              | (StringVal s1, StringVal s2) -> s1 = s2
              | (BoolVal b1, BoolVal b2) -> b1 = b2
              | _ -> false
            in
            BoolVal result
        )
    | Call (fname, args) ->
        let argvals = List.map (fun arg -> eval_expr fm stack arg gas) args in
        (match find_var stack fname with
         | Some { value = FunctionVal f; _} ->
            run_function fm stack f argvals gas
         | _ ->
           match Hashtbl.find_opt fm fname with
           | None -> failwith ("[UnboundVar] " ^ fname ^ " non definita")
           | Some fdecl ->
               run_function fm stack fdecl argvals gas)
    | BlockExpr stmts -> run_block_expr fm stack stmts gas
      
  and eval_expr_no_move (fm: func_map) (stack: env_stack) (e: expr) (gas : int ref) : value =
  match e with
  | Number n -> IntVal n
  | StringLiteral s -> StringVal s
  | Boolean b -> BoolVal b
  | StringFrom e ->
      let v = eval_expr_no_move fm stack e gas in
      (match v with
       | StringVal s -> StringVal s
       | _ -> failwith "String::from() deve contenere una stringa")
  | Borrow inner_expr ->
      (match inner_expr with
       | Var v ->
           (match find_var stack v with
            | None -> failwith ("Variabile '" ^ v ^ "' non dichiarata")
            | Some entry ->
                (match entry.value with
                 | RefVal _ -> entry.value
                 | _ -> RefVal { ref_name = v; mutable_ref = false }))
       | _ -> failwith "Il borrowing è permesso solo su variabili")
  | BorrowMut inner_expr ->
      (match inner_expr with
       | Var v ->
           (match find_var stack v with
            | None -> failwith ("Variabile '" ^ v ^ "' non dichiarata")
            | Some entry ->
                (match entry.value with
                 | RefVal _ -> entry.value
                 | _ -> RefVal { ref_name = v; mutable_ref = true }))
       | _ -> failwith "Il borrowing mutabile è permesso solo su variabili")
  | Var v ->
      (match find_var stack v with
       | None -> failwith ("Variabile '" ^ v ^ "' non dichiarata")
       | Some entry ->
           (match entry.value with
            | MovedVal ->
                failwith ("[MovedValue] borrow of moved value " ^ v)
            | RefVal { ref_name; mutable_ref = _ } ->
                deref_value stack (RefVal { ref_name; mutable_ref = false })
            | other_val ->
                other_val))
  | BinOp (e1, op, e2) ->
      let v1 = eval_expr_no_move fm stack e1 gas in
      let v2 = eval_expr_no_move fm stack e2 gas in
      (match op with
       | Add | Sub | Mul | Div | Mod ->
           (match (v1, v2) with
            | (IntVal l, IntVal r) ->
                (match op with
                 | Add -> IntVal (l + r)
                 | Sub -> IntVal (l - r)
                 | Mul -> IntVal (l * r)
                 | Div -> if r = 0 then failwith "Division by zero" else IntVal (l / r)
                 | Mod -> if r = 0 then failwith "Modulo by zero" else IntVal (l mod r)
                 | _ -> failwith "Operazione aritmetica non valida")
            | _ -> failwith "Operazione non valida su non interi")
       | Equal ->
           let result =
             match (v1, v2) with
             | (IntVal l, IntVal r) -> l = r
             | (StringVal s1, StringVal s2) -> s1 = s2
             | (BoolVal b1, BoolVal b2) -> b1 = b2
             | _ -> false
           in
           BoolVal result)
  | Call (fname, args) ->
      let argvals = List.map (fun arg -> eval_expr fm stack arg gas) args in
      (match Hashtbl.find_opt fm fname with
       | None -> failwith ("[UnboundVar] " ^ fname ^ " non definita")
       | Some fdecl ->
           run_function fm stack fdecl argvals gas)
  | BlockExpr stmts -> run_block_expr fm stack stmts gas

and run_block_expr (fm: func_map) (stack: env_stack) (stmts: statement list) (gas : int ref) : value =
  let new_stack = push_scope stack in
  let result =
    try
      let _ = run_statements fm new_stack stmts gas in
      match List.rev stmts with
      | Return e :: _ -> eval_expr fm new_stack e gas
      | ExprStmt e :: _ -> eval_expr fm new_stack e gas
      | _ -> VoidVal
    with ReturnException v ->
      v
  in
  ignore (pop_scope new_stack);
  result


end