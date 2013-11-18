type op = Add | Mult | Conn | Paral | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type expr =
    V_def of string * string
  | Bar_def of expr list
  | Tuple of expr * expr
  | Bar_val_1 of expr list
  | Bar_val_2 of expr * expr list
  | Rhy_val of expr list
  | Track_def of expr list
  | Track_val of expr list
  | Literal of int
  | Note_value of string
  | Str of string
  | Bool of string
  | Null of string
  | Id of string
  | M_at of string * int
  | M_updn of string * string * int
  | M_len of string
  | Binop of expr * op * expr
  | Not of expr
  | Assign of expr * expr
  | Concat of expr * expr
  | Call of expr * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | Break
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    rtype: string;
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }

type program = string list * func_decl list

let rec string_of_expr = function
     V_def(s1,s2) -> s1 ^ " " ^ s2
  | Bar_def(el) ->
       "bar(" ^ String.concat "&" (List.map string_of_expr el) ^ ")"
  | Tuple(e1,e2) -> "(" ^ string_of_expr e1 ^ ";" ^ string_of_expr e2 ^ ")"
  | Bar_val_1(el) -> "[" ^ String.concat ", " List.map string_of_expr el ^ "]"
  | Bar_val_2(e,el) ->
       "[" ^ string_of_expr e ^ ",(" ^ String.concat ", " (List.map string_of_expr el) ^ ")]"
  | Rhy_val(el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | Track_def(el) -> "track(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Track_val(el) -> "{" ^ String.concat ", " (List.map string_of_expr el) ^ "}"
  | Literal(l) -> string_of_int l
  | Note_value(s) -> s
  | Str(s) -> s
  | Bool(s) -> s
  | Null(s) -> s
  | Id(s) -> s
  | M_at(s,l) -> s ^ ".at(" ^ string_of_int l ^ ");\n"
  | M_updn(s1,s2,l) -> s1 ^ s2 ^ "(" ^ string_of_int l ^ ");\n"
  | M_len(s) -> s ^ ".length();\n"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Mult -> "*" | Conn -> "^" | Paral -> "&"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
      | And -> "&&" | Or -> "||") ^ " " ^
      string_of_expr e2
  | Not(e) -> "!" ^ string_of_expr e
  | Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Concat(e1, e2) -> string_of_expr e1 ^ " <- " ^ string_of_expr e2
  | Call(e, el) ->
      string_of_expr e ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Break -> "break;\n"
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl id = "Type " ^ id ^ ";\n"
let string_of_formal id = "Type " ^ id

let string_of_fdecl fdecl =
  "function " ^ fdecl.rtype ^ " " ^ fdecl.fname ^ "(" ^
  String.concat ", " (List.map string_of_formal fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
