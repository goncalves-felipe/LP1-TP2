%%

%name PlcParser

%pos int

%term VAR
    | FUN
    | REC
    | IF
    | THEN
    | ELSE
    | NOT
    | MATCH
    | WITH
    | ISE
    | AND
    | HD
    | TL
    | PLUS
    | MINUS
    | TIMES
    | DIV
    | EQUAL
    | DIFF
    | LT
    | LTE
    | SEMICOL
    | DCOL
    | RSBR
    | LSBR
    | RPAR
    | LPAR
    | RBR
    | LBR
    | AFARROW
    | ANONF
    | END
    | COMMA
    | PIPE
    | TRUE
    | FALSE
    | UNDERSCORE
    | MARROW
    | NIL
    | INT
    | BOOL
    | PRINT
    | COLON
    | NAME of string
    | CINT of int
    | EOF

%nonterm Prog of expr
  | Decl of expr
  | AppExpr of expr
  | AtomExpr of expr
  | Params of (plcType * string) list
  | Const of expr
  | Expr of expr
  | Args of (plcType * string) list
  | Comps of expr list
  | MatchExpr of (expr option * expr) list
  | TypedVar of plcType * string
  | Type of plcType
  | Types of plcType list
  | AtomType of plcType
  | CondExpr of expr option

%right SEMICOL MARROW
%nonassoc IF
%left ELSE
%left AND
%left EQUAL DIFF
%left LT LTE
%right DCOL
%left PLUS MINUS
%left TIMES DIV
%nonassoc NOT HD TL ISE PRINT
%left LSBR

%eop EOF

%noshift EOF

%start Prog

%%

Prog: Expr (Expr) 
    | Decl (Decl)

Decl: VAR NAME EQUAL Expr SEMICOL Prog (Let(NAME, Expr, Prog))
    | FUN NAME Args EQUAL Expr SEMICOL Prog (Let(NAME, makeAnon(Args, Expr), Prog))
    | FUN REC NAME Args COLON Type EQUAL Expr SEMICOL Prog (makeFun(NAME, Args, Type, Expr, Prog))

Expr: AtomExpr(AtomExpr)
    | AppExpr(AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | NOT Expr (Prim1("!", Expr))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | HD Expr (Prim1("hd", Expr))
    | TL Expr (Prim1("tl", Expr))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | PRINT Expr (Prim1("print", Expr))
    | ISE Expr (Prim1("ise", Expr))
    | Expr TIMES Expr (Prim2("*", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | MINUS Expr (Prim1("-", Expr))
    | Expr DIFF Expr (Prim2("!=", Expr1, Expr2))
    | Expr EQUAL Expr (Prim2("=", Expr1, Expr2))
    | Expr LTE Expr (Prim2("<=", Expr1, Expr2))
    | Expr DCOL Expr (Prim2("::", Expr1, Expr2))
    | Expr LT Expr (Prim2("<", Expr1, Expr2))
    | Expr SEMICOL Expr (Prim2(";", Expr1, Expr2))
    | Expr LSBR CINT RSBR (Item(CINT, Expr))

AtomExpr: Const (Const)
    | NAME (Var(NAME))
    | LBR Prog RBR (Prog)
    | LPAR Expr RPAR (Expr)
    | LPAR Comps RPAR (List(Comps))
    | ANONF Args AFARROW Expr END (makeAnon(Args, Expr))

AppExpr: AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
    | AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const: CINT (ConI(CINT))
    | TRUE (ConB true)
    | FALSE (ConB false)
    | LPAR RPAR (List [])
    | LPAR Type LSBR RSBR RPAR (ESeq(Type))

Comps: Expr COMMA Expr (Expr1::Expr2::[])
    | Expr COMMA Comps (Expr::Comps)

MatchExpr: END ([])
    | PIPE CondExpr MARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr: UNDERSCORE (NONE)
    | Expr (SOME Expr)

Args: LPAR RPAR ([])
    | LPAR Params RPAR (Params)
    
Params: TypedVar (TypedVar::[])
    | TypedVar COMMA Params (TypedVar::Params)

TypedVar: Type NAME ((Type, NAME))

Type: AtomType (AtomType)
    | LSBR Type RSBR (SeqT(Type))
    | Type MARROW Type (FunT (Type1, Type2))
    | LPAR Types RPAR (ListT(Types))

AtomType: NIL (ListT [])
    | INT (IntT)
    | BOOL (BoolT)
    | LPAR Type RPAR (Type)

Types: Type COMMA Type (Type1::Type2::[])
    | Type COMMA Types (Type::Types)
