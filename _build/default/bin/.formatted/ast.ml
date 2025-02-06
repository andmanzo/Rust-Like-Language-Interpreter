type bin_op = 
  | Add  
  | Sub  
  | Mul  
  | Div  
  | Mod 
  | Equal 

type param = (string * string)

type expr = 
  | Number of int
  | StringLiteral of string
  | Boolean of bool
  | Var of string
  | BinOp of expr * bin_op * expr
  | StringFrom of expr
  | Call of string * expr list
  | Borrow of expr
  | BorrowMut of expr
  | BlockExpr of statement list

and statement =
  | Let of bool * string * expr  
  | Assign of string * expr      
  | Println of expr                  
  | PushStr of string * string       
  | ExprStmt of expr                
  | Return of expr                   
  | Block of statement list
  | If of expr * statement * statement option
  | Loop of statement
  | Break
  | FunctionDecl of function_decl

and function_decl = {
  name: string;
  params: param list;
  return_type: string option;  
  body: statement list;
  is_local: bool;
}

type program = {
  functions: function_decl list;
}

