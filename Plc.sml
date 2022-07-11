(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun run exp =
    let
      val expType = teval exp []
      val expResult = eval exp []
    in
      val2string(expResult) ^ " : " ^ type2string(expType)

      handle SymbolNotFound => "Símbolo indefinido."
      | EmptySeq => "A sequência de entrada não contém nenhum elemento."
      | UnknownType => "Nenhuma das específicas se encaixa."
      | NotEqTypes => "Os tipos usados numa comparação são diferentes."
      | WrongRetType => "O tipo de retorno da função não condiz com o corpo da mesma."
      | DiffBrTypes => "Os tipos da expressões dos possíveis caminhos de um If divergem."
      | IfCondNotBool => "A condição do if não é booleana."
      | NoMatchResults => "Não há resultados para a expressão match."
      | MatchResTypeDiff => "O tipo de algum dos casos em match difere dos demais."
      | MatchCondTypesDiff => "O tipo das opções de match difere do tipo da expressão passada para Match."
      | CallTypeMisM => "Você está passando pra uma chamada de função um tipo diferente do qual ela suporta."
      | NotFunc => "Você está tentando chamar algo que não é uma função."
      | ListOutOfRange => "Tentativa de acessar um elemento fora dos limites da lista."
      | OpNonList => "Tentativa de acessar um elemento em uma expressão que não é uma lista."
      | HDEmptySeq => "Tentativa de acessar a cabeca de uma sequencia vazia."
      | TLEmptySeq => "Tentativa de acessar a cauda de uma sequencia vazia."
      | ValueNotFoundInMatch => "Valor nao encontrado em nenhuma expressao match."
      | NotAFunc => "Você está tentando chamar algo que não é uma função."
      | Impossible => "Operacao impossivel de ser realizada."
      | _ => "Um erro inesperado aconteceu."
    end