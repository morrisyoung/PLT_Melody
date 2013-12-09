open Ast

module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)


type element = (*here we temporarily don't consider...*)
   Not of int * int
  |Bar of (int * int) list
  |Tra of (int * int) list list
  |Rhy of int list
  |Pit of string

module StringMap = Map.Make(struct
  type t = string
  let compare x y=Pervasives.compare x y
end)

module IntMap = Map.Make(struct
  type t = int
  let compare x y = Pervasives.compare x y
end)

let maxStringInt = 95;;
let minStringInt = 0;;

let str2int = StringMap.empty in
let str2int = StringMap.add "~C" 0 str2int in
let str2int = StringMap.add "~C#" 1 str2int in
let str2int = StringMap.add "~Db" 1 str2int in
let str2int = StringMap.add "~D" 2 str2int in
let str2int = StringMap.add "~D#" 3 str2int in
let str2int = StringMap.add "~Eb" 3 str2int in
let str2int = StringMap.add "~E" 4 str2int in
let str2int = StringMap.add "~F" 5 str2int in
let str2int = StringMap.add "~F#" 6 str2int in
let str2int = StringMap.add "~Gb" 6 str2int in
let str2int = StringMap.add "~G" 7 str2int in
let str2int = StringMap.add "~G#" 8 str2int in
let str2int = StringMap.add "~Ab" 8 str2int in
let str2int = StringMap.add "~A" 9 str2int in
let str2int = StringMap.add "~A#" 10 str2int in
let str2int = StringMap.add "~Bb" 10 str2int in
let str2int = StringMap.add "~B" 11 str2int in
let int2str = IntMap.empty in
let int2str = IntMap.add 0 "~C" int2str in
let int2str = IntMap.add 1 "~C#" int2str in
let int2str = IntMap.add 2 "~D" int2str in
let int2str = IntMap.add 3 "~D#" int2str in
let int2str = IntMap.add 4 "~E" int2str in
let int2str = IntMap.add 5 "~F" int2str in
let int2str = IntMap.add 6 "~F#" int2str in
let int2str = IntMap.add 7 "~G" int2str in
let int2str = IntMap.add 8 "~G#" int2str in
let int2str = IntMap.add 9 "~A" int2str in
let int2str = IntMap.add 10 "~A#" int2str in
let int2str = IntMap.add 11 "~B" int2str in



let mapstr2int = fun x ->
let octave = String.get x ((String.length x)-1) in
if (octave == '1')||(octave == '2')||(octave == '3')||(octave == '4')||(octave == '5')||(octave == '6')||(octave == '7') then
let basicString = String.sub x 0 ((String.length x) - 2) in
((StringMap.find basicString str2int) + ((int_of_char octave) - 48) * 12)
else (StringMap.find x str2int)
in

let mapint2str = fun x ->
if x > maxStringInt then raise (Failure ("String higher than allowable reference threshold"))
else if x < minStringInt then raise (Failure ("String lower than allowable reference threshold"))
else if x > 11 then (IntMap.find (x - 12*(x/12)) int2str) ^ (string_of_int (x/12))
else (IntMap.find x int2str)
in




(*
variables initialization:
pitch:int;
note:(int,int)                        (initialize as (0,0)                                     )
bar:[(int,int);(int,int);...]         (initialize as [(0,0)]                                   )
track:[bar,....]                      (initialize as [[(0,0)]       ]                          )
pitch: string                         (initialize as "~"                                       )

*)


exception ReturnException of int * int NameMap.t

(* Main entry point: run a program *)

let run (vars, funcs) =
  (* Put function declarations in a symbol table *)
  let func_decls = List.fold_left
      (fun funcs fdecl -> NameMap.add fdecl.fname fdecl funcs)
      NameMap.empty funcs
  in

  (* Invoke a function and return an updated global symbol table *)
  let rec call fdecl actuals globals =

    (* Evaluate an expression and return (value, updated environment) *)
    let rec eval env = function
	Note_value(e,i) -> let p,env = (eval env e) in
			(p,i),env
      | Track_value(el) -> (List.fold_left eval env el)[(,);(,);(,)],env(*not done*)
      | Bar_value1(el) -> (List.map (fun e -> let note_value,env = (eval env) e in note_value) el), env
      | Rhythm_value(el) -> (List.map (fun e -> let rhy_value,env = (eval env) e in rhy_value) el), env
      | Bar_value2(e,el) -> let l1,env = (eval env e) in
			let l2 = (List.map (fun e -> let pitch_value,env = (eval env) e in pitch_value) el) in
			let (l,_) = List.fold_left(fun (l,n) e -> ((e,(List.nth l1 n))::l,n+1) ([],0) l2)
			in List.rev l
      | Literal(i) -> i, env
      | Pitch_value(s) -> (mapstr2int s), env
      | Str(s) -> s,env
      | Bool(s) -> if s == "true" then (1,env) 
		else if s == "false" then (0,env)
		else raise (Failure ("Not a Bool type"))
      | Null(s) -> 0,env
      | Id(s) ->
	  let locals, globals = env in
	  if NameMap.mem s locals then (*locals and globles are all strings and their values? Can be any type value?*)
	    (NameMap.find s locals), env
	  else if NameMap.mem var globals then
	    (NameMap.find var globals), env
	  else raise (Failure ("undeclared identifier " ^ var))
      | Id(var) ->
	  let locals, globals = env in
	  if NameMap.mem var locals then
	    (NameMap.find var locals), env
	  else if NameMap.mem var globals then
	    (NameMap.find var globals), env
	  else raise (Failure ("undeclared identifier " ^ var))


      | Binop(e1, op, e2) ->
	  let v1, env = eval env e1 in
          let v2, env = eval env e2 in
	  let boolean i = if i then 1 else 0 in
	  (match op with
	    Add -> v1 + v2
	  | Sub -> v1 - v2
	  | Mult -> v1 * v2
	  | Div -> v1 / v2
	  | Equal -> boolean (v1 = v2)
	  | Neq -> boolean (v1 != v2)
	  | Less -> boolean (v1 < v2)
	  | Leq -> boolean (v1 <= v2)
	  | Greater -> boolean (v1 > v2)
	  | Geq -> boolean (v1 >= v2)), env


      | Assign(var, e) ->
	  let v, (locals, globals) = eval env e in
	  if NameMap.mem var locals then
	    v, (NameMap.add var v locals, globals)
	  else if NameMap.mem var globals then
	    v, (locals, NameMap.add var v globals)
	  else raise (Failure ("undeclared identifier " ^ var))
      | Call("print", [e]) ->
	  let v, env = eval env e in
	  print_endline (string_of_int v);
	  0, env
      | Call(f, actuals) ->
	  let fdecl =
	    try NameMap.find f func_decls
	    with Not_found -> raise (Failure ("undefined function " ^ f))
	  in
	  let actuals, env = List.fold_left
	      (fun (actuals, env) actual ->
		let v, env = eval env actual in v :: actuals, env)
   	      ([], env) (List.rev actuals)
	  in
	  let (locals, globals) = env in
	  try
	    let globals = call fdecl actuals globals
	    in 0, (locals, globals)
	  with ReturnException(v, globals) -> v, (locals, globals)
      | Noexpr -> 1, env (* must be non-zero for the for loop predicate *)

    in

    (* Execute a statement and return an updated environment *)
    let rec exec env = function
	Block(stmts) -> List.fold_left exec env stmts
      | Expr(e) -> let _, env = eval env e in env
      | If(e, s1, s2) ->
	  let v, env = eval env e in
	  exec env (if v != 0 then s1 else s2)
      | While(e, s) ->
	  let rec loop env =
	    let v, env = eval env e in
	    if v != 0 then loop (exec env s) else env
	  in loop env
      | For(e1, e2, e3, s) ->
	  let _, env = eval env e1 in
	  let rec loop env =
	    let v, env = eval env e2 in
	    if v != 0 then
	      let _, env = eval (exec env s) e3 in
	      loop env
	    else
	      env
	  in loop env
      | Return(e) ->
	  let v, (locals, globals) = eval env e in
	  raise (ReturnException(v, globals))
    in

    (* Enter the function: bind actual values to formal arguments *)
    let locals =
      try List.fold_left2
	  (fun locals formal actual -> NameMap.add formal actual locals)
	  NameMap.empty fdecl.formals actuals
      with Invalid_argument(_) ->
	raise (Failure ("wrong number of arguments passed to " ^ fdecl.fname))
    in

    (* Initialize local variables to 0 *)
    let locals_note = List.fold_left (fun locals_note var_decl -> match var_decl.v_type with
	"note" -> NameMap.add var_decl.v_name (0,0) locals_note) NameMap.empty func_decl.locals
    in
    let locals_track = List.fold_left (fun locals_track var_decl -> match var_decl.v_type with
	"track" -> NameMap.add var_decl.v_name (0,0) locals_track) NameMap.empty func_decl.locals
    in
    let locals_bar = List.fold_left (fun locals_bar var_decl -> match var_decl.v_type with
	"bar" -> NameMap.add var_decl.v_name (0,0) locals_bar) NameMap.empty func_decl.locals
    in
    let locals_rhy = List.fold_left (fun locals_rhy var_decl -> match var_decl.v_type with
	"rhythm" -> NameMap.add var_decl.v_name (0,0) locals_rhythm) NameMap.empty func_decl.locals
    in
    let locals_note = List.fold_left (fun locals_note var_decl -> match var_decl.v_type with
	"" -> NameMap.add var_decl.v_name (0,0) locals_note) NameMap.empty func_decl.locals
    in
    (* Execute each statement in sequence, return updated global symbol table *)
    snd (List.fold_left exec (locals, globals) fdecl.body)


(*we use n different maps to store the globals and the locals*)
    in let globals_note = List.fold_left (fun globals_note var_decl -> match var_decl.v_type with
	"note" -> NameMap.add var_decl.v_name (0,0) globals_note) NameMap.empty vars

    in let globals_track = List.fold_left (fun globals_track var_decl -> match var_decl.v_type with
	"track" -> NameMap.add var_decl.v_name [[(0,0)]] globals_track) NameMap.empty vars

    in let globals_bar = List.fold_left (fun globals_bar var_decl -> match var_decl.v_type with
    "bar" -> NameMap.add var_decl.v_name [(0,0)]  globals_bar) NameMap.empty vars

    in let globals_rhy = List.fold_left (fun globals_rhy var_decl -> match var_decl.v_type with
	"rhythm" -> NameMap.add var_decl.v_name (0,0) globals_rhy) NameMap.empty vars

    in let globals_lit = List.fold_left (fun globals_lit var_decl -> match var_decl.v_type with
	"int" -> NameMap.add var_decl.v_name (0,0) globals_lit) NameMap.empty vars

    in let globals_pitch = List.fold_left (fun globals_pitch var_decl -> match var_decl.v_type with
	"pitch" -> NameMap.add var_decl.v_name (0,0) globals_pitch) NameMap.empty vars

    in let globals_str = List.fold_left (fun globals_str var_decl -> match var_decl.v_type with
	"string" -> NameMap.add var_decl.v_name (0,0) globals_str) NameMap.empty vars

    in let globals_bool = List.fold_left (fun globals_bool var_decl -> match var_decl.v_type with
	"bool" -> NameMap.add var_decl.v_name (0,0) globals_bool) NameMap.empty vars 

  (* Run a program: initialize global variables to 0, find and run "main" *)
  in let globals = List.fold_left
      (fun globals match var_decl.v_type with
	Note_value -> NameMap.add vdecl 0 globals NameMap.empty vars
      | Track_value ->
      | Bar_value1 ->
      | Rhythm_value ->
      | Literal -> 
      | Pitch_value ->
      | Str ->
      | Bool ->

-> NameMap.add vdecl 0 globals) NameMap.empty vars
  in try
    call (NameMap.find "main" func_decls) [] globals
  with Not_found -> raise (Failure ("did not find the main() function"))
