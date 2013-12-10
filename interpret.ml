open Ast

module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end);;


exception ReturnException of int * int NameMap.t

type element = (*here we temporarily don't consider...*)
   Not of int * int
  |Bar of (int * int) list
  |Tra of (int * int) list list
  |Mel of (int * int) list list list
  |Rhy of int list
  |Pit of int(*pay attention to this!!!!*)
  |Lit of int
  |Str of string
  |Bol of int(*we will transfer such type into *)

module StringMap = Map.Make(struct
  type t = string
  let compare x y=Pervasives.compare x y
end);;

module IntMap = Map.Make(struct
  type t = int
  let compare x y = Pervasives.compare x y
end);;

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
else (StringMap.find x str2int) in

let mapint2str = fun x ->
if x > maxStringInt then raise (Failure ("String higher than allowable reference threshold"))
else if x < minStringInt then raise (Failure ("String lower than allowable reference threshold"))
else if x > 11 then (IntMap.find (x - 12*(x/12)) int2str) ^ (string_of_int (x/12))
else (IntMap.find x int2str) in




(*
variables initialization:
Pit:int;                              (    as 0)
Not:(int,int)                        (initialize as (0,0)                                     )
Bar:[(int,int);(int,int);...]         (initialize as [(0,0)]                                   )
Tra:[bar;....]                      (initialize as [[(0,0)]       ]                          )
Mel:[Tra;...]                       (initialize as [[[(0,0)]       ]            ]              )
Rhy:[int;int;...]                       ([0])
Str:string                            ("")
Bol:int                                (0)
*)



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
       Pitch_value(s) -> Pit(mapstr2int s), env
	  |Note_value(e,i) -> let Pit(p),env = (eval env e) in
			(Not(p,i)),env
     (* | Track_value(el) -> (List.fold_left eval env el)[(,);(,);(,)],env *)
      | Bar_value1(el) -> Bar(List.map (fun e -> let Not(p,d),env = (eval env) e in (p,d)) el), env
      | Rhythm_value(el) -> Rhy(List.map (fun e -> let Lit(rhy_value),env = (eval env) e in rhy_value) el), env
      | Bar_value2(e,el) -> let Rhy(l1),env = (eval env e) in
			let l2 = (List.map (fun p -> let Pit(pitch_value),env = (eval env) p in pitch_value) el) in
			let l = (List.fold_left2 (fun l p d -> ((p,d)::l)) [] l2 l1)
			in Bar(List.rev l), env
      | Literal(i) -> Lit(i), env
      | Str(s) -> Str(s),env (*两个str重名了！*)
      | Bool(s) -> if s == "true" then (Bol(1),env) 
		else if s == "false" then (Bol(0),env)
		else raise (Failure ("Not a Bool type"))
      (*| Null(s) -> 0,env*)(*null 怎么还带值啊！*)
      | Id(s) ->
	  let locals, globals = env in
	  if NameMap.mem s locals then (*locals and globles are all strings and their values? Can be any type value?*)
	    (NameMap.find s locals), env
	  else if NameMap.mem s globals then
	    (NameMap.find s globals), env
	  else raise (Failure ("undeclared identifier " ^ s))
  





      | Binop(e1,op,e2)-> (let op1,env = (eval env e1) in let op2,env = (eval env e2) in
       let boolean i = if i then 1 else 0 in
       match op with
	Add     -> (match (op1,op2) with
              (Lit(op1), Lit(op2)) -> Lit(op1+op2),env
              | (Str(op1), Str(op2)) -> Str(op1^op2),env
              | (Tra(op1),Tra(op2)) -> Tra(op1@op2),env
              | _ -> raise (Failure ("unexpected type for +")))
        | Mult    -> (match (op1,op2) with
                    (Lit(l1), Lit(l2)) -> Lit(l1*l2),env
                    | (Not(p,l1), Lit(l2)) -> Not(p,l1/l2),env
                    | _ -> raise (Failure ("unexpected type for *")))
        | Paral   -> (match (op1,op2) with
(*                    (Pitch_value(t1), Pitch_value(t2))   -> (*chord部分 不会写 考虑删掉*)*)
                     (Tra(t1), Tra(t2)) -> Mel([t2;t1]),env
                    | (Mel(m), Tra(t)) -> Mel(t::m),env
                    | _ -> raise (Failure ("unexpected type for &")))
        | Equal   -> (match (op1,op2) with
                    (Lit(l1),Lit(l2)) -> Bol(boolean (l1=l2)),env
                    | (Str(s1),Str(s2))   -> Bol(boolean (s1=s2)),env
                    | (Pit(p1),Pit(p2))  -> Bol(boolean (p1=p2)),env
                    | (Not(p1,d1), Not(p2,d2)) -> Bol(boolean ((p1=p2)&&(d1=d2))),env
                    | _ -> raise (Failure ("unexpected type for ==")))
        | Neq     -> (match (op1,op2) with
                    (Lit(l1),Lit(l2)) -> Bol(boolean (l1!=l2)),env
                    | (Str(s1),Str(s2))   -> Bol(boolean (s1!=s2)),env
                    | (Pit(p1),Pit(p2))  -> Bol(boolean (p1!=p2)),env
                    | (Not(p1,l1), Not(p2,l2)) -> Bol(boolean ((p1!=p2)||(l1!=l2))),env
                    | _ -> raise (Failure ("unexpected type for !=")))
        | Less    -> (match (op1,op2) with
                    (Lit(l1),Lit(l2)) -> Bol(boolean (l1 < l2)),env
                    | _ -> raise (Failure ("unexpected type for <")))
        | Leq     -> (match (op1,op2) with
                    (Lit(l1),Lit(l2)) -> Bol(boolean (l1 <= l2)),env
                    | _ -> raise (Failure ("unexpected type for <=")))
        | Greater -> (match (op1,op2) with
                    (Lit(l1),Lit(l2)) -> Bol(boolean (l1 > l2)),env
                    | _ -> raise (Failure ("unexpected type for >")))
        | Geq     -> (match (op1,op2) with
                    (Lit(l1),Lit(l2)) -> Bol(boolean (l1 >= l2)),env
                    | _ -> raise (Failure ("unexpected type for >=")))
        | And     -> (match (op1,op2) with
                    (Bol(b1),Bol(b2)) -> Bol(boolean (b1==1 && b2==1)),env
                    | _ -> raise (Failure ("unexpected type for &&")))
        | Or      -> (match (op1,op2) with
                    (Bol(b1),Bol(b2)) -> Bol(boolean (b1==1 || b2==1)),env
                    | _ -> raise (Failure ("unexpected type for ||"))))

      | Assign(var,e) -> let Id(varid) = var in 
	  let v, (locals, globals) = eval env e in
	  if NameMap.mem varid locals then
	    v, (NameMap.add varid v locals, globals)
	  else if NameMap.mem varid globals then
	    v, (locals, NameMap.add varid v globals)
	  else raise (Failure ("undeclared identifier " ^ varid))
      (*| Call("print", [e]) ->
	  let v, env = eval env e in 
	  print_endline (string_of_int v);
	  0, env*)
      | Call(f, el) -> match f with
            "at" -> (let v,env = eval env (List.nth el 0) in
			match v	with
		  (Bar(l)) -> let Lit(i),env=(eval env (List.nth el 1)) in let (p,d)=(List.nth l i) in Not(p,d),env
		| (Tra(ll)) -> let Lit(i),env=(eval env (List.nth el 1)) in let l=(List.nth ll i) in Bar(l),env
		|_->raise(Failure("obj at type failed")))
		   | "toneUp" -> (let v,env = eval env (List.nth el 0) in
		   	match v with
		 (Pit(p)) -> let Lit(i),env = (eval env (List.nth el 1)) in (Pit(p+i)),env
		|(Not(p,d)) -> let Lit(i),env = (eval env (List.nth el 1)) in (Not(p+i,d)),env
		|(Bar(l)) -> let Lit(i),env = (eval env (List.nth el 1)) in (Bar(List.map (fun (p,d) -> (p+i,d)) l)),env
		|(Tra(ll)) -> let Lit(i),env = (eval env (List.nth el 1)) in (Tra(List.map (List.map (fun (p,d) -> (p+i,d))) ll)),env
		|_->raise(Failure("toneUp type failed")))
       | "toneDown" -> (let v,env = eval env (List.nth el 0) in
        match v with
     (Pit(p)) -> let Lit(i),env = (eval env (List.nth el 1)) in (Pit(p-i)),env
    |(Not(p,d)) -> let Lit(i),env = (eval env (List.nth el 1)) in (Not(p-i,d)),env
    |(Bar(l)) -> let Lit(i),env = (eval env (List.nth el 1)) in (Bar(List.map (fun (p,d) -> (p-i,d)) l)),env
    |(Tra(ll)) -> let Lit(i),env = (eval env (List.nth el 1)) in (Tra(List.map (List.map (fun (p,d) -> (p-i,d))) ll)),env
    |_->raise(Failure("toneDown type failed")))
            | "length" -> (let e,env = (eval env (List.nth el 0)) in
            match e with
         (Bar(l)) -> Lit(List.length l),env
        |(Tra(ll)) -> Lit(List.length ll),env
        |_->raise(Failure("Check length type failed")))

  (*----------------------------------------syntax error了！*)
    |_ ->
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
	  NameMap.empty func_decl.formals actuals
      with Invalid_argument(_) ->
	raise (Failure ("wrong number of arguments passed to " ^ fdecl.fname))
    in
    (* Initialize local variables to 0 *)
    let locals = List.fold_left (fun locals var_decl -> match var_decl.v_type with
	 "note" -> NameMap.add var_decl.v_name (Not(0,0)) locals
	| "track" -> NameMap.add var_decl.v_name (Tra([[(0,0)]])) locals
	| "bar" -> NameMap.add var_decl.v_name (Bar([(0,0)])) locals
	| "rhythm" -> NameMap.add var_decl.v_name (Rhy([0])) locals
	| "melody" -> NameMap.add var_decl.v_name (Mel([[[(0,0)]]])) locals
	| "int" -> NameMap.add var_decl.v_name (Lit(0)) locals
	| "pitch" -> NameMap.add var_decl.v_name (Pit(0)) locals
	| "string" -> NameMap.add var_decl.v_name (Str("")) locals
	| "bool" -> NameMap.add var_decl.v_name (Bol(0)) locals) locals func_decl.locals
    in
    (* Execute each statement in sequence, return updated global symbol table *)
    snd (List.fold_left exec (locals, globals) fdecl.body)

    (* Run a program: initialize global variables to "0", find and run "main" *)
    in let globals = List.fold_left
		(fun globals var_decl -> match var_decl.v_type with
	"note" -> NameMap.add var_decl.v_name (Not(0,0)) globals
	|"track" -> NameMap.add var_decl.v_name (Tra([[(0,0)]])) globals
	|"bar" -> NameMap.add var_decl.v_name (Bar([(0,0)])) globals
	|"rhythm" -> NameMap.add var_decl.v_name (Rhy([0])) globals
	|"melody" -> NameMap.add var_decl.v_name (Mel([[[(0,0)]]])) globals
	|"int" -> NameMap.add var_decl.v_name (Lit(0)) globals
	|"pitch" -> NameMap.add var_decl.v_name (Pit(0)) globals
	|"string" -> NameMap.add var_decl.v_name (Str("")) globals
	|"bool" -> NameMap.add var_decl.v_name (Bol(0)) globals) NameMap.empty vars
  in try
    call (NameMap.find "main" func_decls) [] globals
  with Not_found -> raise (Failure ("did not find the main() function"))
