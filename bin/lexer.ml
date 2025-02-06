type token =
  | FN
  | MAIN
  | LET
  | MUT
  | PRINTLN
  | STRINGFROM  
  | DOUBLECOLON  
  | PUSHSTR     
  | DOT         
  | LParen      
  | RParen       
  | LBrace    
  | RBrace      
  | Plus         
  | Minus        
  | Asterisk    
  | Slash      
  | Percent      
  | Eq           
  | DoubleEq     
  | Semicolon   
  | Exclamation 
  | Arrow       
  | Colon        
  | Ampersand    
  | Comma
  | NumberTok of int
  | StringTok of string 
  | Ident of string
  | TRUE
  | FALSE
  | IF
  | ELSE
  | LOOP
  | BREAK
  | EOF
  | NONE

type lexer_state = {
  text : string;
  mutable pos : int;
  length : int;
  mutable stringmode : bool;
  mutable readingident : bool;
}

let init_lexer (s : string) : lexer_state =
  { text = s; pos = 0; length = String.length s; stringmode = false; readingident = false }

let current_char (st : lexer_state) : char =
  if st.pos >= st.length then '\000'
  else st.text.[st.pos]

let advance (st : lexer_state) : unit =
  if st.pos < st.length then
    st.pos <- st.pos + 1

let rec skip_whitespace (st : lexer_state) : unit =
  match current_char st with
  | ' ' | '\t' | '\n' | '\r'  ->
      advance st;
      skip_whitespace st
  | _ -> ()

let read_number (st : lexer_state) : int =
  let start_pos = st.pos in
  let rec loop () =
    match current_char st with
    | '0'..'9' -> advance st; loop ()
    | _ -> ()
  in
  loop ();
  let num_str = String.sub st.text start_pos (st.pos - start_pos) in
  int_of_string num_str

let read_ident (st : lexer_state) : string =
  let start_pos = st.pos in
  let rec loop () =
    match current_char st with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '!' ->
        advance st;
        loop ()
    | _ -> ()
  in
  loop ();
  st.readingident <- false;
  String.sub st.text start_pos (st.pos - start_pos)

let next_token (st : lexer_state) : token =
  skip_whitespace st;
  let c = current_char st in
  if c = '\000' then
    EOF
  else if st.readingident then
    match c with
    | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' | '!' | ':' ->
        let id = read_ident st in
        (match id with
         | "fn" -> FN
         | "main" -> MAIN
         | "let" -> LET
         | "mut" -> MUT
         | "if" -> IF
         | "else" -> ELSE
         | "String::from" -> STRINGFROM
         | "push_str" -> PUSHSTR
         | "println!" -> PRINTLN
         | "true" -> TRUE
         | "false" -> FALSE
         | "loop" -> LOOP
         | "break" -> BREAK
         | _ -> Ident id)
    | _ ->
        advance st;
        EOF
  else if st.stringmode then
    match c with
    | '"' ->  
        st.stringmode <- false; 
        advance st;
        NONE  
    | _ ->  
        let start_pos = st.pos in
        let rec loop () =
          match current_char st with
          | '"' | '\000' -> ()
          | _ -> advance st; loop ()
        in
        loop ();
        StringTok (String.sub st.text start_pos (st.pos - start_pos))
  else
    match c with
    | '&' -> advance st; Ampersand
    | '(' -> advance st; LParen
    | ')' -> advance st; RParen
    | '{' -> advance st; LBrace
    | '}' -> advance st; RBrace 
    | '+' -> advance st; Plus
    | '-' ->
        let lookahead_pos = st.pos + 1 in
        if lookahead_pos < st.length && st.text.[lookahead_pos] = '>' then
        begin
          st.pos <- st.pos + 2;
          Arrow
        end else
        begin
          advance st;
          Minus
        end
    | '*' -> advance st; Asterisk
    | '/' ->
      let lookahead_pos = st.pos + 1 in
      if lookahead_pos < st.length && st.text.[lookahead_pos] = '/' then begin
        while current_char st <> '\n' && current_char st <> '\000' do
          advance st
        done;
        skip_whitespace st;
        NONE
      end else
        (advance st; Slash)
    | '%' -> advance st; Percent
    | '=' ->
        let lookahead_pos = st.pos + 1 in
        if lookahead_pos < st.length && st.text.[lookahead_pos] = '=' then
        begin
          st.pos <- st.pos + 2; 
          DoubleEq
        end else
          (advance st; Eq)
    | ';' -> advance st; Semicolon
    | '.' -> advance st; DOT
    | '!' -> advance st; Exclamation
    | ':' -> advance st; Colon
    | ',' -> advance st; Comma
    | '"' -> st.stringmode <- true; advance st; NONE
    | '0'..'9' ->
        let n = read_number st in
        NumberTok n
    | 'a'..'z' | 'A'..'Z' | '_' ->
        let id = read_ident st in
        (match id with
         | "fn" -> FN
         | "main" -> MAIN
         | "let" -> LET
         | "mut" -> MUT
         | "true" -> TRUE
         | "false" -> FALSE
         | "if" -> IF
         | "else" -> ELSE
         | "loop" -> LOOP
         | "break" -> BREAK
         | "String" -> 
            let c1 = current_char st in
            if c1 = ':' then begin
              advance st;
              let c2 = current_char st in
              if c2 = ':' then begin
                advance st;
                let id = read_ident st in
                if id = "from" then
                  STRINGFROM
                else
                  NONE
              end else
                NONE
            end else
              Ident id
         | "push_str" -> PUSHSTR
         | "println!" -> PRINTLN
         | _ -> Ident id)
    | _ ->
        advance st;
        EOF

let tokenize (source: string) : token list =
  let st = init_lexer source in
  let rec loop acc =
    let tk = next_token st in
    match tk with
    | EOF -> List.rev (EOF :: acc)
    | NONE -> loop acc
    | _ -> loop (tk :: acc)
  in
  loop []
