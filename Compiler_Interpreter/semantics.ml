open Ast
open Ast.IR

exception Error of string * Lexing.position

let rec analyze_expr env expr =
  match expr with
  | Syntax.Int n -> Int n.value
  | Syntax.Bool b -> Bool b.value
  | Syntax.Var v -> Var v.name
  | Syntax.Call c ->
      let args = List.map (analyze_expr env) c.args in
      Call (c.func, args)

let rec analyze_instr instr env =
  match instr with
  | Syntax.DeclarationVariable dv -> (DeclarationVariable dv.nom_var, env)
  | Syntax.Assign a ->
      let ae = analyze_expr env a.expr in
      (Assign (a.var, ae), env)
  | Syntax.Return r ->
      let re = analyze_expr env r.expr in
      (Return re, env)


and analyze_block block env =
  match block with
  | i :: b ->
      let ai, new_env = analyze_instr i env in
      ai :: analyze_block b new_env
  | [] -> []

let analyze parsed = analyze_block parsed Baselib._types_
