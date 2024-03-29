type reg =
  | Zero
  | SP
  | RA
  | FP
  | V0
  | A0
  | A1
  | T0
  | T1
  | T2

type label = string

type loc =
  | Lbl of label
  | Mem of reg * int
  | Str of string 

type instr =
  | Label of label
  | Li    of reg * int
  | La    of reg * loc
  | Sw    of reg * loc
  | Lw    of reg * loc
  | Sb    of reg * loc
  | Lb    of reg * loc
  | Move  of reg * reg
  | Addi  of reg * reg * int
  | Add   of reg * reg * reg
  | Sub   of reg * reg * reg
  | Mul   of reg * reg * reg
  | Div   of reg * reg * reg
  | Syscall
  | Or of reg * reg * reg
  | And of reg * reg * reg
  | Xori of reg * reg * int
  | Beq of reg * reg * label
  | Sge of reg * reg * reg
  | Sne of reg * reg
  | Slt of reg * reg * reg
  | B     of label
  | Beqz  of reg * label
  | Jal   of label
  | Jr    of reg
  | Str of string * loc
  | StringLiteral of string * loc

type directive =
  | Asciiz of string

type decl = label * directive

type asm = { text: instr list ; data: decl list }

let ps = Printf.sprintf (* alias raccourci *)

module Syscall = struct
  let print_int = 1
  let print_str = 4
  let read_int  = 5
  let read_str  = 8
  let sbrk      = 9
end

let fmt_reg = function
  | Zero -> "$zero"
  | SP   -> "$sp"
  | FP   -> "$fp"
  | RA   -> "$ra"
  | V0   -> "$v0"
  | A0   -> "$a0"
  | A1   -> "$a1"
  | T0   -> "$t0"
  | T1   -> "$t1"
  | T2   -> "$t2"

let fmt_loc = function
  | Lbl (l)    -> l
  | Mem (r, o) -> ps "%d(%s)" o (fmt_reg r)
  | Str (s)      -> ps "%s" s


let fmt_instr = function
  | Label (l)        -> ps "%s:" l
  | Li (r, i)        -> ps "  li %s, %d" (fmt_reg r) i
  | La (r, a)        -> ps "  la %s, %s" (fmt_reg r) (fmt_loc a)
  | Sw (r, a)        -> ps "  sw %s, %s" (fmt_reg r) (fmt_loc a)
  | Lw (r, a)        -> ps "  lw %s, %s" (fmt_reg r) (fmt_loc a)
  | Sb (r, a)        -> ps "  sb %s, %s" (fmt_reg r) (fmt_loc a)
  | Lb (r, a)        -> ps "  lb %s, %s" (fmt_reg r) (fmt_loc a)
  | Move (rd, rs)    -> ps "  move %s, %s" (fmt_reg rd) (fmt_reg rs)
  | Addi (rd, rs, i) -> ps "  addi %s, %s, %d" (fmt_reg rd) (fmt_reg rs) i
  | Add (rd, rs, rt) -> ps "  add %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sub (rd, rs, rt) -> ps "  sub %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Mul (rd, rs, rt) -> ps "  mul %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Div (rd, rs, rt) -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Syscall          -> ps "  syscall"
  | Or (rd, rs, rt)   -> ps "  or %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | And (rd, rs, rt)  -> ps "  and %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Xori (rd, rs, i)-> ps "  xori %s, %s, %d" (fmt_reg rd) (fmt_reg rs) i
  | B (l)            -> ps "  b %s" l
  | Beq (rd, rs, l)   -> ps "  beq %s, %s, %s" (fmt_reg rd) (fmt_reg rs) l
  | Sge (rd, rs, rt)   -> ps "  sge %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rs) 
  | Sne (rd, rs)      -> ps "  sne %s, %s" (fmt_reg rd) (fmt_reg rs)
  | Slt (rd, rs, rt)   -> ps "  slt %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rs) 
  | Beqz (r, l)      -> ps "  beqz %s, %s" (fmt_reg r) l
  | Jal (l)          -> ps "  jal %s" l
  | Jr (r)           -> ps "  jr %s" (fmt_reg r)


let fmt_dir = function
  | Asciiz (s) -> ps ".asciiz \"%s\"" s

let emit oc asm =
  Printf.fprintf oc ".text\n.globl main\nmain:\n" ;
  List.iter (fun i -> Printf.fprintf oc "%s\n" (fmt_instr i)) asm.text ;
  Printf.fprintf oc "  move $a0, $v0\n  li $v0, 1\n  syscall\n  jr $ra\n" ;
  Printf.fprintf oc "\n.data\n" ;
  List.iter (fun (l, d) -> Printf.fprintf oc "%s: %s\n" l (fmt_dir d)) asm.data
