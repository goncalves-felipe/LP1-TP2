(* Infrastructure to run the Plc Front-End *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

use "testParserCases.sml";

(* Try to add a systematic way of using the test cases in
   testParserCases to stress test your parser *)

fun runTests ([], brokenCase, brokenCases, i) = if size brokenCases = 0 then "Success" else "Errors: " ^ brokenCases
    | runTests ((s, e)::t, brokenCase, brokenCases, i) = 
         runTests(t, s, if (fromString(s) = e) then brokenCases else brokenCases ^ Int.toString(i) ^ " ", i + 1);

runTests(cases,"", "", 0);
