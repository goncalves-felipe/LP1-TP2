signature PlcParser_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val CINT: (int) *  'a * 'a -> (svalue,'a) token
val NAME: (string) *  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val BOOL:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val NIL:  'a * 'a -> (svalue,'a) token
val MARROW:  'a * 'a -> (svalue,'a) token
val UNDERSCORE:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val PIPE:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val ANONF:  'a * 'a -> (svalue,'a) token
val AFARROW:  'a * 'a -> (svalue,'a) token
val LBR:  'a * 'a -> (svalue,'a) token
val RBR:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val LSBR:  'a * 'a -> (svalue,'a) token
val RSBR:  'a * 'a -> (svalue,'a) token
val DCOL:  'a * 'a -> (svalue,'a) token
val SEMICOL:  'a * 'a -> (svalue,'a) token
val LTE:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val DIFF:  'a * 'a -> (svalue,'a) token
val EQUAL:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val TL:  'a * 'a -> (svalue,'a) token
val HD:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val ISE:  'a * 'a -> (svalue,'a) token
val WITH:  'a * 'a -> (svalue,'a) token
val MATCH:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val REC:  'a * 'a -> (svalue,'a) token
val FUN:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
end
signature PlcParser_LRVALS=
sig
structure Tokens : PlcParser_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
