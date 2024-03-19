
open Ast.IR
open Mips


module Env = Map.Make(String)

type cinfo = 
{ code: Mips.instr list
; env: Mips.loc Env.t
; fpo: int
; counter: int
; return: string 
}

let next_label =
  let counter = ref 0 in
  fun () ->
    let current = !counter in
    counter := current + 1;
    Lbl ("label_" ^ string_of_int current)


let get_inline_func f =
  match f with
  | "_add" -> [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Add (V0, T0, T1) ]
  
  | "_sub" -> [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Sub (V0, T0, T1) ]
  
  | "_mul" -> [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Mul (V0, T0, T1) ]
  
  | "_div" -> [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Div (V0, T0, T1) ]
  
  | "_puti" -> [ Lw (A0, Mem (SP, 0))
              ; Li (V0, Syscall.print_int)
              ; Syscall ]
  
  | "_geti" -> [ Li (V0, Syscall.read_int)
              ; Syscall ]
  
  | "_puts" -> [ Lw (A0, Mem (SP, 0))
              ; Li (V0, Syscall.print_str)
              ; Syscall ]
  
  | "_and"  -> [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; And (T2, T0, T1) ]
  
  | "_or"   -> [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Or (T2, T0, T1) ]
  
  | "_inf"  -> [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Slt (V0, T0, T1) ]
  
  | "_sup"  -> [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Slt (V0, T1, T0) ]

  | _       -> failwith ("Unknown function: " ^ f)




let compile_value v =
  match v with
  | Int n  -> [ Li (V0, n) ]
  | Bool b -> [ Li (V0, if b then 1 else 0) ]
    

let rec compile_expr e info =
  match e with
  | Int n  -> [ Li (V0, n) ]
  | Bool b -> [ Li (V0, if b then 1 else 0) ]
  | String s -> [ La (V0, next_label()); Str (s, next_label()) ]
  | Call (f, args) ->
    let ca = List.rev_map (fun a ->
                 compile_expr a info
                 @ [ Addi (SP, SP, -4)
                   ; Sw (V0, Mem (SP, 0)) ])
               args in
    List.flatten ca
    (*@ get_inline_func f*)
    @ [ Addi (SP, SP, 4 * (List.length args)) ]





let rec compile_instr i info =
  match i with
  | DeclarationVariable v ->
      { info with
        env = Env.add v (Mem (FP, -info.fpo)) info.env
      ; fpo = info.fpo + 4 }

  | Return e ->
      { info with
        code = info.code
              @ compile_expr e info
              @ [ B info.return ] }
  (*| Expr e ->
      { info with
        code = info.code
              @ compile_expr e info } *)
  | Assign (lv, e) ->
    { info with
      code = info.code
            @ compile_expr e info.env

            @ [ Sw (V0, Env.find lv info.env) ]

            }
            (*@ (match lv with
                | LVar v -> [ Sw (V0, Env.find v info.env) ]
                | LAddr a ->
                  [ Addi (SP, SP, -4)
                  ; Sw (V0, Mem (SP, 0)) ]
                  @ compile_expr a info
                  @ [ Lw (T0, Mem (SP, 0))
                    ; Addi (SP, SP, 4)
                    ; Sw (T0, Mem (V0, 0)) ]) }*)
              
   (*| BranchementCondSi (c, t, e) ->
      let uniq = next_label() in
      let ct = compile_block t { info with code = []
                                          ; counter = info.counter + 1
                                          ; return = "else" ^ uniq } in
      let ce = compile_block e { info with code = []
                                          ; counter = ct.counter + 1
                                          ; return = "finif" ^ uniq } in
      { info with
        code = info.code
              @ compile_expr c info
              @ [ Beqz (V0, "else" ^ uniq) ]
              @ ct.code
              @ [ B ("finif" ^ uniq)
                ; Label ("else_if" ^ uniq) ]
              @ ce.code
              @ [ Label ("finif" ^ uniq) ]
      ; counter = ce.counter }
*)


      

      and compile_block b info =
        match b with
        | [] -> info
        | i :: r ->
            compile_block r (compile_instr i info)




let compile_def (Func (name, args, b)) info =
  let cb = compile_block b
              { info with
                code = []
              ; env = List.fold_left
                        (fun e (i, a) -> Env.add a (Mem (FP, 4 * i)) e)
                        Env.empty (List.mapi (fun i a -> i + 1, a) args)
              ; fpo = 8
              ; counter = info.counter + 1
              ; return = "ret" ^ (string_of_int info.counter) }
  in
  (*info.data @ cb.data*)
  cb.counter, []
  @ [ Label name
    ; Addi (SP, SP, -cb.fpo)
    ; Sw (RA, Mem (SP, cb.fpo - 4))
    ; Sw (FP, Mem (SP, cb.fpo - 8))
    ; Addi (FP, SP, cb.fpo - 4) ]
  @ cb.code
  @ [ Label cb.return
    ; Addi (SP, SP, cb.fpo)
    ; Lw (RA, Mem (FP, 0))
    ; Lw (FP, Mem (FP, -4))
    ; Jr (RA) ]


    (*
               
let rec compile_prog p counter =
  match p with
  | [] -> []
  | d :: r ->
      let new_counter, cd = compile_def d counter in
      cd @ (compile_prog r new_counter).code

  *)

 
 (*  
let compile ir =
  { text = Baselib.builtins @ compile_expr ir
  ; data = [] }
*)



(*
let compile ir =
  let info =
    compile_block ir { code = Baselib.builtins; env = Env.empty; fpo = 0 }
  in
  { text = Move (FP, SP) :: Addi (FP, SP, info.fpo) :: info.code; data = [] }

*)



  let compile ir =
    let info =
      compile_block ir { code = Baselib.builtins; env = Env.empty; fpo = 0; counter = 0; return = "end_func" }
    in
    { text = Move (FP, SP) :: Addi (FP, SP, info.fpo) :: info.code; data = [] }
  

