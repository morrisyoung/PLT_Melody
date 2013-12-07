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
  | Hlt     -> ()

  in exec 0 0 0
