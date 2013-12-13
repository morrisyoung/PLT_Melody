open Ast

module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end);;


module StringMap = Map.Make(struct
  type t = string
  let compare x y=Pervasives.compare x y
end);;

module IntMap = Map.Make(struct
  type t = int
  let compare x y = Pervasives.compare x y
end);;

(*
type element = (*here we temporarily don't consider...*)
   Nte of int * int
  |Bar of (int * int) list
  |Tra of (int * int) list list
  |Mel of (int * int) list list list
  |Rhy of int list
  |Pit of int(*pay attention to this!!!!*)
  |Lit of int
  |Stg of string
  |Bol of int(*we will transfer such type into *)
*)

type element = (*here we temporarily don't consider...*)
   Nte of int * int
  |Bar of (int * int) list
  |Tra of (int * int) list list
  |Mel of (int * int) list list list
  |Rhy of int list
  |Pit of int(*pay attention to this!!!!*)
  |Lit of int
  |Stg of string
  |Bol of int(*we will transfer such type into *)

exception ReturnException of element * element NameMap.t

let maxStringInt = 95;;
let minStringInt = 0;;

let str2int = StringMap.empty;;
let str2int = StringMap.add "~C" 0 str2int;;
let str2int = StringMap.add "~C#" 1 str2int;;
let str2int = StringMap.add "~Db" 1 str2int;;
let str2int = StringMap.add "~D" 2 str2int;;
let str2int = StringMap.add "~D#" 3 str2int;;
let str2int = StringMap.add "~Eb" 3 str2int;;
let str2int = StringMap.add "~E" 4 str2int;;
let str2int = StringMap.add "~F" 5 str2int;;
let str2int = StringMap.add "~F#" 6 str2int;;
let str2int = StringMap.add "~Gb" 6 str2int;;
let str2int = StringMap.add "~G" 7 str2int;;
let str2int = StringMap.add "~G#" 8 str2int;;
let str2int = StringMap.add "~Ab" 8 str2int;;
let str2int = StringMap.add "~A" 9 str2int;;
let str2int = StringMap.add "~A#" 10 str2int;;
let str2int = StringMap.add "~Bb" 10 str2int;;
let str2int = StringMap.add "~B" 11 str2int;;
let int2str = IntMap.empty;;
let int2str = IntMap.add 0 "~C" int2str;;
let int2str = IntMap.add 1 "~C#" int2str;;
let int2str = IntMap.add 2 "~D" int2str;;
let int2str = IntMap.add 3 "~D#" int2str;;
let int2str = IntMap.add 4 "~E" int2str;;
let int2str = IntMap.add 5 "~F" int2str;;
let int2str = IntMap.add 6 "~F#" int2str;;
let int2str = IntMap.add 7 "~G" int2str;;
let int2str = IntMap.add 8 "~G#" int2str;;
let int2str = IntMap.add 9 "~A" int2str;;
let int2str = IntMap.add 10 "~A#" int2str;;
let int2str = IntMap.add 11 "~B" int2str;;


let mapstr2int = fun x ->
let octave = String.get x ((String.length x)-1) in
if (octave == '1')||(octave == '2')||(octave == '3')||(octave == '4')||(octave == '5')||(octave == '6')||(octave == '7') then
let basicString = String.sub x 0 ((String.length x) - 2) in
((StringMap.find basicString str2int) + ((int_of_char octave) - 48) * 12)
else (StringMap.find x str2int);;

let mapint2str = fun x ->
if x > maxStringInt then raise (Failure ("String higher than allowable reference threshold"))
else if x < minStringInt then raise (Failure ("String lower than allowable reference threshold"))
else if x > 11 then (IntMap.find (x - 12*(x/12)) int2str) ^ (string_of_int (x/12))
else (IntMap.find x int2str);;


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


(*
let rec string_of_element = function
   Nte(p,d) -> " Nte(" ^ (string_of_int p) ^ "," ^ (string_of_int d)^")"
  |Bar(l) ->  let readNote (p,d) = " (" ^ (string_of_int p) ^ "," ^ (string_of_int d)^")" in
				"Bar(" ^ (String.concat " "(List.map (fun (p,d) -> readNote (p,d)) l)) ^")"
  |Tra(ll) ->  let readNote p d = (" (" ^ (string_of_int p) ^ "," ^ (string_of_int d)^")" )in
				let readBar l = ("(" ^ (String.concat " "(List.map (fun (p,d) -> readNote p d) l)) ^")")
				in "Tra(" ^ (String.concat " " (List.map readBar ll)) ^")"
  |Mel(lll) ->let readNote p d = (" (" ^ (string_of_int p) ^ "," ^ (string_of_int d)^")" )in
				let readBar l = ("(" ^ (String.concat " "(List.map (fun (p,d) -> readNote p d) l)) ^")")
				   in let readTra l = (String.concat " " (List.map readBar l))
					in "Mel(" ^ (String.concat " " (List.map readTra lll)) ^")"
  |Rhy(il) ->  "Rhy(" ^ (String.concat " "(List.map string_of_int il)) ^")"
  |Pit(i) -> "Pit(" ^ string_of_int(i) ^ ")" 
  |Lit(i) -> "Lit(" ^ string_of_int(i) ^ ")" 
  |Stg(s) -> "Stg(" ^ s ^ ")" 
  |Bol (b) ->"Bol(" ^ string_of_int(b) ^ ")";;
*)

let rec string_of_element = function
   Nte(p,d) -> " Nte(" ^ (string_of_int p) ^ "," ^ (string_of_int d)^")"
  |Bar(l) ->  let readNote (p,d) = " Nte(" ^ (string_of_int p) ^ "," ^ (string_of_int d)^")" in
				"Bar(" ^ (String.concat " "(List.map (fun (p,d) -> readNote (p,d)) l)) ^")"
  |Tra(ll) ->  let readNote p d = (" Nte(" ^ (string_of_int p) ^ "," ^ (string_of_int d)^")" )in
				let readBar l = ("Bar(" ^ (String.concat " "(List.map (fun (p,d) -> readNote p d) l)) ^")")
				in "Tra(" ^ (String.concat " " (List.map readBar ll)) ^")"
  |Mel(lll) ->let readNote p d = (" Nte(" ^ (string_of_int p) ^ "," ^ (string_of_int d)^")" )in
				let readBar l = ("Bar(" ^ (String.concat " "(List.map (fun (p,d) -> readNote p d) l)) ^")")
				   in let readTra l = "Tra(" ^(String.concat " " (List.map readBar l))^")"
					in "Mel(" ^ (String.concat " " (List.map readTra lll)) ^")"
  |Rhy(il) ->  "Rhy(" ^ (String.concat " "(List.map string_of_int il)) ^")"
  |Pit(i) -> "Pit(" ^ string_of_int(i) ^ ")" 
  |Lit(i) -> "Lit(" ^ string_of_int(i) ^ ")" 
  |Stg(s) -> "Stg(" ^ s ^ ")" 
  |Bol (b) ->"Bol(" ^ string_of_int(b) ^ ")";;



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
(*       Pitch_value(s) -> Pit(mapstr2int s), env  *)
(*
       Nte(i1,i2) -> (Nte(i1,i2)),env
      | Bar(l) ->  (Bar(l)),env
      | Tra(l) -> (Tra(l)),env
      | Mel(l) -> (Mel(l)),env
      | Rhy(l) -> (Rhy(l)),env
      | Pit(i) -> (Pit(i)),env
      | Lit(i) -> (Lit(i)),env
      | Stg(s) -> (Stg(s)),env
      | Bol(i) -> (Bol(i)),env
*)
      Pitch_value(s) -> Pit(mapstr2int s), env
      | Note_value(e,i) ->(match (eval env e) with
			Pit(p),env -> ((Nte(p,i)),env)
			|_-> raise (Failure ("wrong type in Note_value!")) )(*env right*)
     (* | Track_value(el) -> (List.fold_left eval env el)[(,);(,);(,)],env *)
      | Bar_value1(el) ->(*Bar(List.map (fun e -> match eval env e with
			Nte(p,d),env-> (p,d)
			Nte(p,d),env->Bar((p,d)::l),env   *)
		(*	Nte(p,d),env-> let ignore(env) in (p,d)   *)
			let actuals, env = List.fold_left (fun (actuals, env) actual ->
						let v, env = (match eval env actual with
							Nte(p,d),env->(p,d),env
							|_->raise (Failure ("wrong type in Bar_value!")) )
						in v :: actuals, env)
					([], env) (List.rev el)
			in Bar(actuals),env(*env right*)
		(*	|_-> raise (Failure ("wrong type in Bar_value!")) ) el),env   *)
      | Rhythm_value(el) ->(* Rhy(List.map (fun e -> let Lit(rhy_value),env = (eval env) e in rhy_value) el), env *)
		(*	Rhy(List.map (fun e -> match eval env e with
			Lit(i),env-> i
			|_-> raise (Failure ("wrong type in Rhythm_value!")) ) el),env
		*)
			let actuals, env = List.fold_left (fun (actuals, env) actual ->
						let v, env = (match eval env actual with
							Lit(i),env->i,env
							|_->raise (Failure ("wrong type in Bar_value!")) )
						in v :: actuals, env)
					([], env) (List.rev el)
			in Rhy(actuals),env(*env right*)
      | Bar_value2(e,el) -> let l1,env = (match (eval env e) with
			Rhy(l1),env -> l1,env
			|_-> raise (Failure ("wrong type in Rhythm_value!")) )
			in
		(*	let l2 = (List.map (fun p -> match (eval env p) with
			Pit(pitch_value),env ->(pitch_value)
			|_-> raise (Failure ("wrong type in Rhythm_value!")) ) el)   *)
			let l2, env = List.fold_left (fun (actuals, env) actual ->
						let v, env = (match eval env actual with
							Pit(i),env->i,env
							|_->raise (Failure ("wrong type in Bar_value!")) )
						in v :: actuals, env)
					([], env) (List.rev el)
		(*	in l2,env(*env right*)   *)
			in
			let l = (List.fold_left2 (fun l p d -> ((p,d)::l)) [] l2 l1)
			in Bar(List.rev l), env(*env right*)
(*actually the three 4 types have something wrong with the env!!*)
    (*  | Track_value(el) -> Tra(List.map (fun e -> let Bar(l),env = (eval env) e in l) el), env  *)
(*      | Track_value(el) -> Tra(List.map (fun e -> match eval env e with
			Bar(l),env -> l
			|_-> raise (Failure ("wrong type in Track_value!")) ) el),env
*)

(*      | Melody_value(el) -> Mel(List.map (fun e -> match eval env e with
			Tra(l),env -> l
			|_->raise (Failure ("wrong type in Melody_value!")) ) el),env    *)
      | Track_value(el) -> let actuals, env =List.fold_left (fun (actuals, env) actual ->
  					let v, env = (match eval env actual with
  						Bar(l),env -> l,env
  						|_ -> raise (Failure ("wrong type in Track_value!")))
  					in v :: actuals, env)
      					([], env) (List.rev el)
      					in Tra(actuals),env(*env right*)
      | Literal(i) -> Lit(i), env
      | Str(s) -> let s1=String.sub s 1 ((String.length s)-2) in Stg(s1),env
      | Bool(s) -> if s = "true" then (Bol(1),env) 
		else if s = "false" then (Bol(0),env)
		else raise (Failure ("Not a Bool type"))
      | Null(s) -> Stg(s),env
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
              | (Stg(op1), Stg(op2)) -> Stg(op1^op2),env
              | (Tra(op1),Tra(op2)) -> Tra(op1@op2),env
              | _ -> raise (Failure ("unexpected type for +")))
        | Mult    -> (match (op1,op2) with
                    (Lit(l1), Lit(l2)) -> Lit(l1*l2),env
                    | (Nte(p,l1), Lit(l2)) -> Nte(p,l1/l2),env
                    | _ -> raise (Failure ("unexpected type for *")))
        | Paral   -> (match (op1,op2) with
(*                    (Pitch_value(t1), Pitch_value(t2))   -> (*chord部分 不会写 考虑删掉*)*)
                     (Tra(t1), Tra(t2)) -> Mel([t2;t1]),env
                    | (Mel(m), Tra(t)) -> Mel(t::m),env
                    | _ -> raise (Failure ("unexpected type for &")))
        | Equal   -> (match (op1,op2) with
                    (Lit(l1),Lit(l2)) -> Bol(boolean (l1=l2)),env
                    | (Stg(s1),Stg(s2))   -> Bol(boolean (s1=s2)),env
                    | (Pit(p1),Pit(p2))  -> Bol(boolean (p1=p2)),env
                    | (Nte(p1,d1), Nte(p2,d2)) -> Bol(boolean ((p1=p2)&&(d1=d2))),env
                    | _ -> raise (Failure ("unexpected type for ==")))
        | Neq     -> (match (op1,op2) with
                    (Lit(l1),Lit(l2)) -> Bol(boolean (l1!=l2)),env
                    | (Stg(s1),Stg(s2))   -> Bol(boolean (s1!=s2)),env
                    | (Pit(p1),Pit(p2))  -> Bol(boolean (p1!=p2)),env
                    | (Nte(p1,l1), Nte(p2,l2)) -> Bol(boolean ((p1!=p2)||(l1!=l2))),env
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

      | Assign(var,e) ->
	  let v, (locals, globals) = eval env e in
	  if NameMap.mem var locals then
	    v, (NameMap.add var v locals, globals)
	  else if NameMap.mem var globals then
	    v, (locals, NameMap.add var v globals)
	  else raise (Failure ("undeclared identifier " ^ var))
      (*| Call("print", [e]) ->
	  let v, env = eval env e in 
	  print_endline (string_of_int v);
	  0, env*)
      | Concat(e1,e2) -> (let op1,env = (eval env e1) in let op2,env = (eval env e2) in 
        match (op1,op2) with
        (Tra(t), Bar(b)) -> Tra(List.rev (b::(List.rev t))), env
        |(Bar(b),Nte(p,d)) -> Bar(List.rev ((p,d)::(List.rev b))), env
        |_ -> raise (Failure ("unexpected type for Concat")))


(*      | Call("print", [e]) ->
	  let v, env = eval env e in
	  print_endline (string_of_element v);
	  Lit(0), env
*)
      | Call(f, el) -> (match f with
	"print" ->let actuals, env = (List.fold_left (fun (actuals, env) actual ->
					let v, env = eval env actual
					in v :: actuals, env)
					([], env) (List.rev el) ) in
			print_endline (String.concat "\n" (List.map string_of_element actuals));
			Lit(0),env

(*
	 "print" ->let e = List.nth el 0 in
			let v, env = eval env e in
			print_endline string_of_element v;
			Lit(0),env
*)
	|"at" -> (let v,env = eval env (List.nth el 0) in
		match v	with
		(Bar(l)) -> (match eval env (List.nth el 1) with
				Lit(i),env ->  let (p,d)=(List.nth l i) in Nte(p,d),env
				|_-> raise (Failure ("wrong type in Rhythm_value!"))   )
	(*	| (Tra(ll)) -> let Lit(i),env=(eval env (List.nth el 1)) in let l=(List.nth ll i) in Bar(l),env  *)
		| (Tra(ll)) -> (match eval env (List.nth el 1) with 
				Lit(i), env-> let l=(List.nth ll i) in Bar(l),env
				|_->raise (Failure("unexpected type for at()"))    )
		|_->raise(Failure("obj at type failed")))
	| "toneUp" -> (let v,env = eval env (List.nth el 0) in
		match v with
		(Pit(p)) -> (match eval env (List.nth el 1) with 
			Lit(i),env -> (Pit(p+i)),env
			|_ -> raise(Failure"toneUp type failed"))
        	|(Nte(p,d)) -> (match eval env (List.nth el 1) with 
			Lit(i),env -> (Nte(p+i,d)),env
			|_ -> raise (Failure("toneUp type failed")))
        	|(Bar(l)) -> (match (eval env (List.nth el 1)) with
			Lit(i),env -> Bar(List.map (fun (p,d) -> (p+i,d)) l),env
			|_ -> raise (Failure("toneUp type failed")))
        	|(Tra(ll)) -> (match (eval env (List.nth el 1)) with
			Lit(i),env -> Tra(List.map (List.map (fun (p,d) -> (p+i,d))) ll),env
			|_ -> raise (Failure("toneUp type failed")))
        	|_->raise(Failure("toneUp type failed")))       
	| "toneDown" -> (let v,env = eval env (List.nth el 0) in
        	match v with
		(Pit(p)) -> (match eval env (List.nth el 1) with 
			Lit(i),env -> (Pit(p-i)),env
			|_ -> raise(Failure"toneDown type failed"))
		|(Nte(p,d)) -> (match eval env (List.nth el 1) with 
			Lit(i),env -> (Nte(p-i,d)),env
			|_ -> raise (Failure("toneDown type failed")))
		|(Bar(l)) -> (match (eval env (List.nth el 1)) with
                      Lit(i),env -> Bar(List.map (fun (p,d) -> (p-i,d)) l),env
                      |_ -> raise (Failure("toneDown type failed")))
		|(Tra(ll)) -> (match (eval env (List.nth el 1)) with
                      Lit(i),env -> Tra(List.map (List.map (fun (p,d) -> (p-i,d))) ll),env
                      |_ -> raise (Failure("toneDown type failed")))
    		|_->raise(Failure("toneDown type failed")))
	| "length" -> (let e,env = (eval env (List.nth el 0)) in
		match e with
		(Bar(l)) -> Lit(List.length l),env
		|(Tra(ll)) -> Lit(List.length ll),env
		|_->raise(Failure("Check length type failed")))
	|_ ->
	  let fdecl =
	    try NameMap.find f func_decls
	    with Not_found -> raise (Failure ("undefined function " ^ f))
	  in
	  let actuals, env = List.fold_left
	      (fun (actuals, env) actual ->
		let v, env = eval env actual in v :: actuals, env)
   	      ([], env) (List.rev el)
	  in
	  let (locals, globals) = env in
	  try
	    let globals = call fdecl actuals globals
	    in Lit(0), (locals, globals)(*try a different type*)
	  with ReturnException(v, globals) -> v, (locals, globals)  )
      | Noexpr -> Lit(1), env (* must be non-zero for the for loop predicate *)
    in

    (* Execute a statement and return an updated environment *)
    let rec exec env = function
	Block(stmts) -> List.fold_left exec env stmts
      | Expr(e) -> let _, env = eval env e in env
      | If(e, s1, s2) ->
	  let v, env = eval env e in
	  exec env (if v = Bol(1) then s1 else s2)
      | While(e, s) ->
	  let rec loop env =
	    let v, env = eval env e in
	    if v = Bol(1) then loop (exec env s) else env
	  in loop env
      | For(e1, e2, e3, s) ->
	  let _, env = eval env e1 in
	  let rec loop env =
	    let v, env = eval env e2 in
	    if v = Bol(1) then
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
    let locals = List.fold_left (fun locals var_decl -> match var_decl.v_type with
	 "note" -> NameMap.add var_decl.v_name (Nte(0,0)) locals
	| "track" -> NameMap.add var_decl.v_name (Tra([[(0,0)]])) locals
	| "bar" -> NameMap.add var_decl.v_name (Bar([(0,0)])) locals
	| "rhythm" -> NameMap.add var_decl.v_name (Rhy([0])) locals
	| "melody" -> NameMap.add var_decl.v_name (Mel([[[(0,0)]]])) locals
	| "int" -> NameMap.add var_decl.v_name (Lit(0)) locals
	| "pitch" -> NameMap.add var_decl.v_name (Pit(0)) locals
	| "string" -> NameMap.add var_decl.v_name (Stg("")) locals
	| "bool" -> NameMap.add var_decl.v_name (Bol(0)) locals
	|_ -> raise (Failure ("undefined type!"))  ) locals fdecl.locals

    in
    (* Execute each statement in sequence, return updated global symbol table *)
    snd (List.fold_left exec (locals, globals) fdecl.body)

    (* Run a program: initialize global variables to "0", find and run "main" *)
  in let globals = List.fold_left
		(fun globals var_decl -> match var_decl.v_type with
	"note" -> NameMap.add var_decl.v_name (Nte(0,0)) globals
	|"track" -> NameMap.add var_decl.v_name (Tra([[(0,0)]])) globals
	|"bar" -> NameMap.add var_decl.v_name (Bar([(0,0)])) globals
	|"rhythm" -> NameMap.add var_decl.v_name (Rhy([0])) globals
	|"melody" -> NameMap.add var_decl.v_name (Mel([[[(0,0)]]])) globals
	|"int" -> NameMap.add var_decl.v_name (Lit(0)) globals
	|"pitch" -> NameMap.add var_decl.v_name (Pit(0)) globals
	|"string" -> NameMap.add var_decl.v_name (Stg("")) globals
	|"bool" -> NameMap.add var_decl.v_name (Bol(0)) globals
	|_ -> raise (Failure ("undefined type!"))   ) NameMap.empty vars

  in try
     call (NameMap.find "main" func_decls) [] globals
  with Not_found -> raise (Failure ("did not find the main() function"))
(*
  in
let global = call (NameMap.find "main" func_decls) [] globals in
let s = (string_of_element (NameMap.find "a" global)) in
print_endline s;;
*)
