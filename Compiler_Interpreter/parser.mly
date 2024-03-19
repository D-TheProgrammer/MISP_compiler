%{
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Ltrue
%token <bool> Lfalse
%token <string> Lident



%token Ldef
%token Lcomma
%token <string> Lstring

%token Ladd Lsub Lmul Ldiv
%token Lreturn Leq Lsc Lend

%token Land Lor

%token Lif Lelse  Lelseif Lwhile Ldo

%token Linf Lsup Lvar


%start prog

%type <Ast.Syntax.block> prog

%%





block_i:
  | instr = instr { [instr] }
  | instr = instr; block = block_i { instr :: block }
;


prog:
	| i = instr ; Lsc ; b = prog { i @ b }
	| i = instr ; Lsc ; Lend { i }
;


param_list:
  | Lvar id = Lident { [id] }
  | Lvar id = Lident Lcomma rest = param_list { id :: rest }
  | id = Lident { [id] }
  | id = Lident Lcomma rest = param_list { id :: rest }

instr:
  
  | Lvar; id = Lident; Leq; e = expr
  {
    [ DeclarationVariable { nom_var = id ; pos = $startpos(id)}
      ; Assign { var = id ; expr = e ; pos = $startpos($3) }
    ]
  }

  | Lvar; id = Lident 
  {
   [ DeclarationVariable { nom_var = id ; pos = $startpos(id)}]
  }
  
  | id = Lident; Leq; e = expr
  {
	[ Assign { var = id
     		 ; expr = e 
    		 ; pos = $startpos($2) 
    		 }
    ]
  }
  | Lreturn; e = expr { [ Return { expr = e; pos = $startpos($1) } ] }





expr:
  | n = Lint    { Int    { value = n ; pos = $startpos(n) } }


  | s = Lstring { String { value = s ; pos = $startpos(s) } }
  | b = Ltrue   { Bool   { value = b ; pos = $startpos(b) } }


  | v = Lident  { Var    { name = v  ; pos = $startpos(v) } }
  | b = Lfalse  { Bool   { value = b ; pos = $startpos(b) } }


  | expression1 = expr; 
    Lmul; 
    expression2 = expr{ 
    Call { func = "_mul"
        ; args = [expression1 ; expression2 ]
        ; pos = $startpos($2) }
  }
  | expression1 = expr; 
    Ladd; 
    expression2 = expr{
    Call { func = "_add"
        ; args = [expression1 ; expression2 ]
        ; pos = $startpos($2) }
  }

  | expression1 = expr; 
    Lsub; 
    expression2 = expr{
    Call { func = "_sub"
        ; args = [expression1 ; expression2 ]
        ; pos = $startpos($2) }
  }

  | expression1 = expr; 
    Ldiv; 
    expression2 = expr{
    Call { func = "_div"
        ; args = [expression1 ; expression2 ]
        ; pos = $startpos($2) }
  }


  | expression1 = expr; 
    Land; 
    expression2 = expr{
    Call { func = "_and"
        ; args = [expression1 ; expression2 ]
        ; pos = $startpos($2) }
  }

  | expression1 = expr; 
    Lor; 
    expression2 = expr{
    Call { func = "_or"
        ; args = [expression1 ; expression2 ]
        ; pos = $startpos($2) }
  }


  | expression1 = expr; 
    Linf; 
    expression2 = expr{
    Call { func = "_inf"
        ; args = [expression1 ; expression2 ]
        ; pos = $startpos($2) }
  }


  | expression1 = expr; 
    Lsup; 
    expression2 = expr{
    Call { func = "_sup"
        ; args = [expression1 ; expression2 ]
        ; pos = $startpos($2) }
  }





;
