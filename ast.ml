type op = Add | Mult | Conn | Paral | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type expr =
     Note_value of expr * int
  | Track_or_Bar_or_Rhy_val of expr list
  | Bar_val of expr * expr list
  (*| Rhy_val of int list
  | Track_val of expr list*)
  | Literal of int
  | Pitch_value of string
  | Str of string
  | Bool of string
  | Null of string
  | Id of string
  (*| M_at of string * int
  | M_updn of string * string * int
  | M_len of string*)
  | Binop of expr * op * expr
  | Not of expr
  | Assign of expr * expr
  | Concat of expr * expr
  (*| Call of expr * expr list*)
  | Call of string * expr list
  | Method of string * expr list
 (* | Length of expr *)
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | Break
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type var_decl = {
    v_type: string;
    v_init: expr;
    v_attr: expr list;
  }

type par_decl = {
    p_type: string;
    p_name: string;
  }

type func_decl = {
    rtype: string;
    fname : string;
    formals : par_decl list;
    locals : var_decl list;
    body : stmt list;
  }

type program = var_decl list * func_decl list


let rec string_of_expr = function
     Note_value(e,l) -> "(" ^ string_of_expr e ^ "; " ^ string_of_int l ^ ")"
  | Track_or_Bar_or_Rhy_val(el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | Bar_val(e, el) ->
      "[" ^ string_of_expr e ^ ";" ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")]"
  | Literal(l) -> string_of_int l
  | Pitch_value(s) -> s
  | Str(s) -> s
  | Bool(s) -> s
  | Null(s) -> s
  | Id(s) -> s
  (*| M_at(s,l) -> s ^ ".at(" ^ string_of_int l ^ ");\n"
  | M_updn(s1,s2,l) -> s1 ^ s2 ^ "(" ^ string_of_int l ^ ");\n"
  | M_len(s) -> s ^ ".length();\n"*)
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
  | Call(s, el) -> s ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Method(s,el) ->
        s ^ " (" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
(*  | Length(e) -> "length (" ^ string_of_expr e ^ ")" *)
  | Noexpr -> ":)"

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

let string_of_var_decl var_decl = match var_decl.v_type with
	"bar" -> var_decl.v_type ^ "<<" ^ String.concat ", " (List.map string_of_expr var_decl.v_attr) ^ ">>" ^ string_of_expr var_decl.v_init  ^ ";\n"
	|"track"-> var_decl.v_type ^ "<<" ^ String.concat ", " (List.map string_of_expr var_decl.v_attr) ^ ">>" ^ string_of_expr var_decl.v_init  ^ ";\n"
	|_ -> var_decl.v_type ^ " " ^ string_of_expr var_decl.v_init  ^ ";\n"


let string_of_par_decl par_decl =
  par_decl.p_type ^ " " ^ par_decl.p_name

let string_of_func_decl func_decl =
  "function " ^ func_decl.rtype ^ " " ^ func_decl.fname ^ "(" ^
  String.concat ", " (List.map string_of_par_decl func_decl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_var_decl func_decl.locals) ^
  String.concat "" (List.map string_of_stmt func_decl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_var_decl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_func_decl funcs)
