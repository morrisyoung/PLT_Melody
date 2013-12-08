open Ast
open Bytecode

(* Stack layout just after "Ent":

              <-- SP
   Local n
   ...
   Local 0
   Saved FP   <-- FP
   Saved PC
   Arg 0
   ...
   Arg n *)

let execute_prog prog =
  let stack = Array.make 1024 0
  and globals = Array.make prog.num_globals 0 in

  let rec exec fp sp pc = match prog.text.(pc) with
    Lit i  -> stack.(sp) <- i ; exec fp (sp+1) (pc+1)
  | Drp    -> exec fp (sp-1) (pc+1)
  | Bin op -> let op1 = stack.(sp-2) and op2 = stack.(sp-1) in     
      stack.(sp-2) <- (let boolean i = if i then 1 else 0 in
      match op with
	Add     -> (match (op1,op2) with
              (Literal(l1), Literal(l2)) -> Literal(l1+l2)
              | (Str(s1), Str(s2)) -> Str(op1^op2)
              | (Track_value(tl1),Track_value(tl2)) -> Track_value(tl1@tl2)
              | _ -> raise (Failure ("unexpected type for +")))
      | Mult    -> (match (op1,op2) with
                    (Literal(l1), Literal(l2)) -> Literal(op1*op2)
                    | (Note_value(p,l1), Literal(l2)) -> Note_value(p,l1/l2) 
                    | _ -> raise (Failure ("unexpected type for *")))
      | Paral   -> (match (op1,op2) with
                    (Pitch_value(t1), Pitch_value(t2))   -> (*chord部分 不会写 考虑删掉*)
                    | (Track_value(t1), Track_value(t2)) -> Melody_value(t2@t1)
                    | (Melody_value(m), Track_value(t)) -> Melody_value(t::m)
                    | _ -> raise (Failure ("unexpected type for &")))
      | Equal   -> (match (op1,op2) with
                    (Literal(l1),Literal(l2)) -> boolean (l1=l2)
                    | (Str(s1),Str(s2))   -> boolean (s1=s2)
                    | (Pitch_value(p1),Pitch_value(p2))  -> boolean (p1=p2)
                    | (Note_value(p1,l1), Note_value(p2,l2)) -> boolean ((p1=p2)&&(l1=l2))
                    | _ -> raise (Failure ("unexpected type for ==")))
      | Neq     -> (match (op1,op2) with
                    (Literal(l1),Literal(l2)) -> boolean (l1!=l2)
                    | (Str(s1),Str(s2))   -> boolean (s1!=s2)
                    | (Pitch_value(p1),Pitch_value(p2))  -> boolean (p1!=p2)
                    | (Note_value(p1,l1), Note_value(p2,l2)) -> boolean ((p1!=p2)||(l1!=l2))
                    | _ -> raise (Failure ("unexpected type for !=")))
      | Less    -> (match (op1,op2) with
                    (Literal(l1),Literal(l2)) -> boolean (l1 <  l2)
                    | _ -> raise (Failure ("unexpected type for <")))
      | Leq     -> (match (op1,op2) with
                    (Literal(l1),Literal(l2)) -> boolean (l1 <=  l2)
                    | _ -> raise (Failure ("unexpected type for <=")))
      | Greater -> (match (op1,op2) with
                    (Literal(l1),Literal(l2)) -> boolean (l1 >  l2)
                    | _ -> raise (Failure ("unexpected type for >")))
      | Geq     -> (match (op1,op2) with
                    (Literal(l1),Literal(l2)) -> boolean (l1 >=  l2)
                    | _ -> raise (Failure ("unexpected type for >=")))
      | And     -> (match (op1,op2) with
                    (Bool(b1),Bool(b2)) -> boolean (b1&&b2)
                    | _ -> raise (Failure ("unexpected type for &&")))
      | Or      -> (match (op1,op2) with
                    (Bool(b1),Bool(b2)) -> boolean (b1||b2)
                    | _ -> raise (Failure ("unexpected type for ||"))));
      exec fp (sp-1) (pc+1)
  | Concat  -> (match (op1,op2) with
                    (Track_value(t),Bar_value1(b)) -> Track_value(List.rev (b::(List.rev t)))
                    | (Track_value(t), Bar_value2(b)) -> Track_value(List.rev (b::(List.rev t)))
                    | (Bar_value1(b), Note_value(p2,l2)) -> Bar_value1(List.rev ((p2,l2)::(List.rev b)))
                    | _ -> raise (Failure ("unexpected type for <-"))
  | Lod i   -> stack.(sp)   <- globals.(i)  ; exec fp (sp+1) (pc+1)
  | Str i   -> globals.(i)  <- stack.(sp-1) ; exec fp sp     (pc+1)
  | Lfp i   -> stack.(sp)   <- stack.(fp+i) ; exec fp (sp+1) (pc+1)
  | Sfp i   -> stack.(fp+i) <- stack.(sp-1) ; exec fp sp     (pc+1)
  | Jsr(-1) -> print_endline (string_of_int stack.(sp-1)) ; exec fp sp (pc+1)
  | Jsr i   -> stack.(sp)   <- pc + 1       ; exec fp (sp+1) i
  | Ent i   -> stack.(sp)   <- fp           ; exec sp (sp+i+1) (pc+1)
  | Rts i   -> let new_fp = stack.(fp) and new_pc = stack.(fp-1) in
               stack.(fp-i-1) <- stack.(sp-1) ; exec new_fp (fp-i) new_pc
  | Beq i   -> exec fp (sp-1) (pc + if stack.(sp-1) =  0 then i else 1)
  | Bne i   -> exec fp (sp-1) (pc + if stack.(sp-1) != 0 then i else 1)
  | Bra i   -> exec fp sp (pc+i)
  
    (*By Jingsi*)
  | Nva (p, d) -> stack.(sp) <- (Nva(p, d)) ; exec fp (sp+1) (pc+1)
  | Tva (ll) -> stack.(sp) <- (Tva (ll)) ; exec fp (sp+1) (pc+1)
  | Str(s1) -> stack.(sp) <- (Str(s1)) ; exec fp (sp+1) (pc+1)
  | Boo(b)-> stack.(sp) <- (Boo(b)) ; exec fp (sp+1) (pc+1)
  | Pva(p)-> stack.(sp) <- (Pva(p)) ; exec fp (sp+1) (pc+1)
  | Rva(r)-> stack.(sp) <- (Rva(r)) ; exec fp (sp+1) (pc+1)
  | Mva(m)-> stack.(sp) <- (Mva(m)) ; exec fp (sp+1) (pc+1)
  | Bv1(Note_list)-> stack.(sp) <- (Bva(Note_list)) ; exec fp (sp+1) (pc+1)
  | Bv2(Rhythm,Pitch_list)->  stack.(sp) <- let (l,_) 
											= List.fold_left(fun (l,n) e -> ((e,(List.nth Rhythem n))::l,n+1) ([],0) Pitch_list)
											in List.rev l in Bva(l);
							 exec fp (sp+1) (pc+1)
  | Oat(obj,i) -> stack.(sp-1) <- match(obj,stack.(sp-1), i) with
		 ("bar",(Bv1(Note_list)),_) -> Bv1((List.nth Note_list i))
		|("bar",(Bv2(Note_list)),_) -> Bv2((List.nth Note_list i))
		|("track",(Tva (ll))) -> Tva((List.nth ll i))
		|_->raise(Failure("obj at type failed"))
  | Tup(obj,i) -> stach.(sp-1) <- match(obj,stack.(sp-1), i) with
		 ("pitch",Pva(p),_) -> Pva(p+i)
		|("note",Nva(p,d),_)-> Nva(p+i,d)
		|("bar",Bva(Note_list),_)-> List.map (fun (p,d)->p+i) Note_list
		|("track",Tva(ll),_)->List.map (fun (p,d)->p+i) ll
		|_->raise(Failure("toneUp type failed"))
  | Tdn(obj,i) -> stach.(sp-1) <- match(obj,stack.(sp-1), i) with
		 ("pitch",Pva(p),_) -> Pva(p-i)
		|("note",Nva(p,d),_)-> Nva(p-i,d)
		|("bar",Bva(Note_list),_)-> List.map (fun (p,d)->p-i) Note_list
		|("track",Tva(ll),_)->List.map (fun (p,d)->p-i) ll  
		|_->raise(Failure("toneDown type failed"))
  | Len(obj) -> stach.(sp-1)  <- match(obj,stack.(sp-1)) with
		|("bar",Bva(Note_list)) ->List.length Note_list
		|("track",Tva(ll))->List.length ll
		|_->raise(Failure("Check length type failed"))
  (*END JINGSI*)
  
  | Hlt     -> ()

  in exec 0 0 0
