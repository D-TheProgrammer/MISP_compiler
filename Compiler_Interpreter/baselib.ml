open Ast.Syntax
open Mips

module Env = Map.Make(String)

(* Définition des types de fonctions *)
let _types_ =
  Env.of_seq
    (List.to_seq
       [ "_add", Func_t (Int_t, [Int_t; Int_t])
       ; "_sub", Func_t (Int_t, [Int_t; Int_t])
       ; "_mul", Func_t (Int_t, [Int_t; Int_t])
       ; "_div", Func_t (Int_t, [Int_t; Int_t])
       ; "_puti", Func_t (Void_t, [Int_t])
       ; "_geti", Func_t (Int_t, [])
       ; "_puts", Func_t (Void_t, [String_t])
       ;"_or", Func_t (Bool_t, [ Bool_t; Bool_t ])
       ;"_and", Func_t (Bool_t, [ Bool_t; Bool_t ])
       ;"_sup", Func_t (Bool_t, [ Int_t; Int_t ])
       ;"_inf", Func_t (Bool_t, [ Int_t; Int_t ])

    ])


(* Définition des différentes opérations *)

(*
let _add =
  [ Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Add (V0, T0, T1) ]

let _sub =
  [ Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Sub (V0, T0, T1) ]

let _mul =
  [ Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Mul (V0, T0, T1) ]

let _div =
  [ Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Div (V0, T0, T1) ]

let puti =
  [ Lw (A0, Mem (SP, 0))
  ; Li (V0, Syscall.print_int)
  ; Syscall ]

let geti =
  [ Li (V0, Syscall.read_int)
  ; Syscall ]

let puts =
  [ Lw (A0, Mem (SP, 0))
  ; Li (V0, Syscall.print_str)
  ; Syscall ]


let _and =
  [ Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; And (T2, T0, T1) ]

let _or = 
  [ Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Or (T2, T0, T1) ]


let inf= 
  [Lw (T0, Mem (SP, 0))
  ;Lw (T1, Mem (SP, 4))
  ;Slt (V0, T0, T1);]

let sup=
  [Lw (T0, Mem (SP, 0))
  ;Lw (T1, Mem (SP, 4))
  ;Slt (V0, T1, T0);]


(* On associe les noms à leurs fonctions *)
let builtins =
  [ "%add", _add
  ; "%sub", _sub
  ; "%mul", _mul
  ; "%div", _div
  ; "puti", puti
  ; "geti", geti
  ; "puts", puts
  ; "and", _and
  ; "or", _or
  ; "inf", inf 
  ; "sup",sup
  ]

  *)

 
  (*
  let builtins =
    [ "_add", [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Add (V0, T0, T1) ]
  
    ; "_sub", [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Sub (V0, T0, T1) ]
  
    ; "_mul", [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Mul (V0, T0, T1) ]
  
    ; "_div", [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Div (V0, T0, T1) ]
  
    ; "_puti", [ Lw (A0, Mem (SP, 0))
              ; Li (V0, Syscall.print_int)
              ; Syscall ]
  
    ; "_geti", [ Li (V0, Syscall.read_int)
              ; Syscall ]
  
    ; "_puts", [ Lw (A0, Mem (SP, 0))
              ; Li (V0, Syscall.print_str)
              ; Syscall ]
  
    ; "_and",  [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; And (T2, T0, T1) ]
  
    ; "_or",   [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Or (T2, T0, T1) ]
  
    ; "_inf",  [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Slt (V0, T0, T1) ]
  
    ; "_sup",  [ Lw (T0, Mem (SP, 0))
              ; Lw (T1, Mem (SP, 4))
              ; Slt (V0, T1, T0) ]
    ]


*)



let builtins =
  [
    Label "_add";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Add (V0, T0, T1);
    Jr RA;
    Label "_sub";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Sub (V0, T0, T1);
    Jr RA;
    Label "_mul";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Mul (V0, T0, T1);
    Jr RA;
    Label "_div";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Div (V0, T0, T1);
    Jr RA;

    Label "_and";
    And (V0, T0, T1);
    Jr RA;

    Label "_or";
    Or (V0, T0, T1);
    Jr RA;

    Label "_inf";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Slt (V0, T0, T1);
    Jr RA;
    Label "_sup";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Slt (V0, T1, T0);
    Jr RA;
  ]