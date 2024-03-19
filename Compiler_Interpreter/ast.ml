
module Syntax = struct
  type type_n = 
    | Int of int 
    | Void
    | Bool of bool
    | String of string
    
  type type_t =
  | Int_t
  | Void_t
  | Bool_t
  | String_t
  | Func_t of type_t * type_t list

  let rec string_of_type_t t =
    match t with
    | Int_t  -> "int"   
    | Void_t -> "void"
    | String_t -> "string"
    | Func_t (r, a) ->
        (if (List.length a) > 1 then "(" else "")
        ^ (String.concat ", " (List.map string_of_type_t a))
        ^ (if (List.length a) > 1 then ")" else "")
        ^ " -> " ^ (string_of_type_t r)

  type ident = string
  type expr =
    | Int  of { value: int
              ; pos: Lexing.position }
    | Bool  of { value: bool
              ; pos: Lexing.position }
    | String of { value: string
                ; pos: Lexing.position }
    | Var  of { name: ident
              ; pos: Lexing.position }
    | Call of { func: ident
              ; args: expr list
              ; pos: Lexing.position }
  type instr =
    | Assign of { var: string
                ; expr: expr
                ; pos: Lexing.position }
    | Return of { expr: expr
                ; pos: Lexing.position } 
    
    | DeclarationVariable of { nom_var: ident; 
                                pos: Lexing.position }


    | Boucle of { parametre: expr;  
                  corps_condition: block; 
                  pos: Lexing.position }



    | BranchementCondSi of { parametre: expr;  
                              else_if: block;   
                              else_ : block; 
                              pos: Lexing.position; }
  
            
 
  and block = instr list
end
      

module IR = struct
  type ident = string
  type expr =
    | Int  of int
    | String of string
    | Bool of bool
    | Var  of ident
    | Call of ident * expr list
  type instr =
    | Assign of string * expr
    | Return of expr
    | DeclarationVariable of ident 

    
    | Boucle of expr * block 
    
    
    | BranchementCondSi of expr * block * block 
    and block = instr list

  type def =
    | Func of string * string list * block
  
  
(*
  let string_of_ir ast =
    let rec fmt_e = function
      | Int n       -> "Int " ^ (string_of_int n)
      | Var v       -> "Var \"" ^ v ^ "\""
      | Call (f, a) -> "Call (\"" ^ f ^ "\", [ "
                       ^ (String.concat " ; " (List.map fmt_e a))
                       ^ " ])"
    and fmt_i = function
      | Assign (v, e) -> "Assign (\"" ^ v ^ "\", " ^ (fmt_e e) ^ ")"
      | Return e      -> "Return (" ^ (fmt_e e) ^ ")"
    and fmt_b b = "[ " ^ (String.concat "\n; " (List.map fmt_i b)) ^ " ]"
    in fmt_b ast
    *)
end

