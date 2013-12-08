open Ast
open Bytecode

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


(* Symbol table: Information about all the names in scope *)
type env = {
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : int StringMap.t; (* "Address" for global variables *)
    local_index    : int StringMap.t; (* FP offset for args, locals *)
  }



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



(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs



(** Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)
let translate (globals, functions) =

  (* Allocate "addresses" for each global variable *)
  let global_indexes = string_map_pairs StringMap.empty (enum 1 0 globals) in

  (* Assign indexes to function names; built-in "print" is special *)
  let built_in_functions = StringMap.add "print" (-1) StringMap.empty in
(*change the following to built-in function*)
  let built_in_functions = StringMap.add "at" (-2) built_in_functions in
  let built_in_functions = StringMap.add "toneUp" (-3) built_in_functions in
  let built_in_functions = StringMap.add "toneDown" (-4) built_in_functions in
  let built_in_functions = StringMap.add "length" (-5) built_in_functions in
  let function_indexes = string_map_pairs built_in_functions
      (enum 1 1 (List.map (fun f -> f.fname) functions)) in

  (* Translate a function in AST form into a list of bytecode statements *)
  let translate env func_decl =
    (* Bookkeeping: FP offsets for locals and arguments *)
(*!!!locals and formals here are not only a name, but a type list!!! Here should do a type check!!!*)
    let num_formals = List.length func_decl.formals
    and num_locals = List.length func_decl.locals
    and local_offsets = enum 1 1 func_decl.locals
    and formal_offsets = enum (-1) (-2) func_decl.formals in
    let env = { env with local_index = string_map_pairs
		  StringMap.empty (local_offsets @ formal_offsets) } in
(*
    let get_the_pitch =
	Id s-> (try [Lfp (StringMap.find s env.local_index)]
          with Not_found -> try [Lod (StringMap.find s env.global_index)]
          with Not_found -> raise (Failure ("undeclared pitch variable " ^ s)))
	| Pitch_value s-> StringToInt s
    in
*)

    let rec expr = function
	Note_value(e,i) -> match e with
		Id s->
		| Pitch_value s-> [Not(StringToInt s,i)](*should we do the type check for a pitch?*)
(*      | Track_value(el) -> [Tra [[],[],[],[]]](*we should normalize the track now!!!*)  *)


(*
      | Bar_value1(el) -> [Bar [[],[],[],[]]]



      | Pitch_value(s) -> (NoteMap.find s StringToIntMap)


      | Literal i -> [Lit i]
      | Id s ->
	  (try [Lfp (StringMap.find s env.local_index)]
          with Not_found -> try [Lod (StringMap.find s env.global_index)]
          with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Binop (e1, op, e2) -> expr e1 @ expr e2 @ [Bin op]
      | Assign (s, e) -> expr e @
	  (try [Sfp (StringMap.find s env.local_index)]
  	  with Not_found -> try [Str (StringMap.find s env.global_index)]
	  with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Call (fname, actuals) -> (try
	  (List.concat (List.map expr (List.rev actuals))) @
	  [Jsr (StringMap.find fname env.function_index) ]   
        with Not_found -> raise (Failure ("undefined function " ^ fname)))
      | Noexpr -> []


*)



    in let rec stmt = function
	Block sl     ->  List.concat (List.map stmt sl)
      | Expr e       -> expr e @ [Drp]
      | Return e     -> expr e @ [Rts num_formals]
      | If (p, t, f) -> let t' = stmt t and f' = stmt f in
	expr p @ [Beq(2 + List.length t')] @
	t' @ [Bra(1 + List.length f')] @ f'
      | For (e1, e2, e3, b) ->
	  stmt (Block([Expr(e1); While(e2, Block([b; Expr(e3)]))]))
      | While (e, b) ->
	  let b' = stmt b and e' = expr e in
	  [Bra (1+ List.length b')] @ b' @ e' @
	  [Bne (-(List.length b' + List.length e'))]

    in [Ent num_locals] @      (* Entry: allocate space for locals *)
    stmt (Block func_decl.body) @  (* Body *)
    [Lit 0; Rts num_formals]   (* Default = return 0 *)

  in let env = { function_index = function_indexes;
		 global_index = global_indexes;
		 local_index = StringMap.empty } in

  (* Code executed to start the program: Jsr main; halt *)
  let entry_function = try
    [Jsr (StringMap.find "main" function_indexes); Hlt]
  with Not_found -> raise (Failure ("no \"main\" function"))
  in

  (* Compile the functions *)
  let func_bodies = entry_function :: List.map (translate env) functions in

  (* Calculate function entry points by adding their lengths *)
  let (fun_offset_list, _) = List.fold_left
      (fun (l,i) f -> (i :: l, (i + List.length f))) ([],0) func_bodies in
  let func_offset = Array.of_list (List.rev fun_offset_list) in

  { num_globals = List.length globals;
    (* Concatenate the compiled functions and replace the function
       indexes in Jsr statements with PC values *)
    text = Array.of_list (List.map (function
	Jsr i when i > 0 -> Jsr func_offset.(i)
      | _ as s -> s) (List.concat func_bodies))
  }
