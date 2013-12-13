type op = Add | Mult | Paral | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type expr =
     Note_value of expr * int
  | Track_value of expr list
(*  | Melody_value of expr list *)
  | Bar_value1 of expr list
  | Rhythm_value of expr list
  | Bar_value2 of expr * expr list
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
  | Assign of string * expr
  | Concat of expr * expr
  (*| Call of expr * expr list*)
  | Call of string * expr list
(*  | Method of string * expr list*)
 (* | Length of expr *)
  | Noexpr
(*
  | Nte of int * int
  | Bar of (int * int) list
  | Tra of (int * int) list list
  | Mel of (int * int) list list list
  | Rhy of int list
  | Pit of int(*pay attention to this!!!!*)
  | Lit of int
  | Stg of string
  | Bol of int(*we will transfer such type into *)
*)


type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
(*  | Break *)
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type var_decl = {
    v_type: string;
    v_name: string;
    v_attr: expr list;
  }

(*
type par_decl = {
    p_type: string;
    p_name: string;
  }
*)

type func_decl = {
    rtype: string;
    fname : string;
    formals : string list;
    locals : var_decl list;
    body : stmt list;
  }

type program = var_decl list * func_decl list


let rec string_of_expr = function
     Note_value(e,l) -> "(" ^ string_of_expr e ^ "; " ^ string_of_int l ^ ")"
(*  | Melody_value(el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"  *)
  | Track_value(el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | Bar_value1(el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | Rhythm_value(el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | Bar_value2(e, el) ->
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
  Add -> "+" | Mult -> "*" | Paral -> "&"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
      | And -> "&&" | Or -> "||") ^ " " ^
      string_of_expr e2
  | Assign(s, e2) -> s ^ " = " ^ string_of_expr e2
  | Concat(e1, e2) -> string_of_expr e1 ^ " <- " ^ string_of_expr e2
  | Call(s, el) -> s ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
(*  | Method(s,el) ->
        s ^ " (" ^ String.concat ", " (List.map string_of_expr el) ^ ")"*)
(*  | Length(e) -> "length (" ^ string_of_expr e ^ ")" *)
  | Noexpr -> ":)"
(*
  | Nte(i1,i2) -> ""
  | Bar(el) -> ""
  | Tra(l) -> ""
  | Mel(l) -> ""
  | Rhy(l) -> ""
  | Pit(i) -> ""
  | Lit(i) -> ""
  | Stg(s) -> ""
  | Bol(i) -> ""
*)

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
(*  | Break -> "break;\n"  *)
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_var_decl var_decl = match var_decl.v_type with
	"bar" -> var_decl.v_type ^ "<<" ^ String.concat ", " (List.map string_of_expr var_decl.v_attr) ^ ">>" ^ var_decl.v_name  ^ ";\n"
	|"track"-> var_decl.v_type ^ "<<" ^ String.concat ", " (List.map string_of_expr var_decl.v_attr) ^ ">>" ^ var_decl.v_name  ^ ";\n"
	|_ -> var_decl.v_type ^ " " ^ var_decl.v_name  ^ ";\n"

(*
let string_of_par_decl par_decl =
  par_decl.p_type ^ " " ^ par_decl.p_name
*)

let string_of_func_decl func_decl =
  "function " ^ func_decl.rtype ^ " " ^ func_decl.fname ^ "(" ^
  String.concat "," func_decl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_var_decl func_decl.locals) ^
  String.concat "" (List.map string_of_stmt func_decl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_var_decl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_func_decl funcs)
