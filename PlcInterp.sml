(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun getItemVAt ([], index) = raise Impossible
  | getItemVAt ((head::_), 1) = head
  | getItemVAt ((head::[]), index) = raise Impossible
  | getItemVAt ((head::tail), index) = getItemVAt (tail, index - 1)

fun eval (ConI value) _ = IntV value
  | eval (ConB value) _ = BoolV value
  | eval (ESeq _) _ = SeqV []
  | eval (Var varName) env = lookup env varName
  | eval (Let (var, expression1, expression2)) env =
    let
      val scope = (var, eval expression1 env) :: env
    in
      eval expression2 scope
    end
  | eval (Letrec (funName, argumentType, argument, funType, expression1, expression2)) env =
    let
      val scope = (funName, Clos(funName, argument, expression1, env)) :: env
    in
      eval expression2 scope
    end
  | eval (Prim1 (operator, expression)) env =
    let
      val value = eval expression env
    in
      case value of
        IntV intValue => 
          let in
            case operator of
              "-" => IntV (intValue * ~1)
            | "print" => 
              let 
                val _ = print(Int.toString(intValue) ^ "\n")
              in
                ListV []
              end
            | _ => raise Impossible
          end
      | BoolV boolValue =>
        let in
          case operator of
            "!" => BoolV (not boolValue)
          | "print" => 
            let 
              val _ = print(Bool.toString(boolValue) ^ "\n")
            in
              ListV []
            end
          | _ => raise Impossible
        end
      | SeqV sequence =>
        let in
          case operator of
            "hd" => let in hd sequence handle Empty => raise HDEmptySeq end
          | "tl" => let in SeqV (tl sequence) handle Empty => raise TLEmptySeq end
          | "ise" => BoolV (length sequence = 0)
          | "print" => 
            let 
              val _ = print(list2string(val2string, sequence) ^ "\n")
            in
              ListV []
            end
          | _ => raise Impossible
        end
      | ListV list =>
        let in
          case operator of
            "print" => 
              let 
                val _ = print(list2string(val2string, list) ^ "\n")
              in
                ListV []
              end
          | _ => raise Impossible
        end
      | _ => raise Impossible
    end
  | eval (Prim2 (operator, expression1, expression2)) env =
    if operator = ";" then
      let
        val _ = eval expression1 env
      in
        eval expression2 env
      end
    else
      let
        val firstValue = eval expression1 env
        val secondValue = eval expression2 env
      in
        case (firstValue, secondValue) of
          (IntV intValue1, IntV intValue2) => 
            let in
              case operator of
                "-" => IntV (intValue1 - intValue2)
              | "+" => IntV (intValue1 + intValue2)
              | "/" => IntV (intValue1 div intValue2)
              | "*" => IntV (intValue1 * intValue2)
              | "<" => BoolV (intValue1 < intValue2)
              | "<=" => BoolV (intValue1 <= intValue2)
              | "!=" => BoolV (intValue1 <> intValue2)
              | "=" => BoolV (intValue1 = intValue2)
              | _ => raise Impossible
            end
        | (BoolV boolValue1, BoolV boolValue2) => 
          let in
            case operator of
              "&&" => BoolV (boolValue1 andalso boolValue2)
            | "=" => BoolV (boolValue1 = boolValue2)
            | "!=" => BoolV (boolValue1 <> boolValue2)
            | _ => raise Impossible
          end
        | (IntV intValue, SeqV sequence) => 
          let in
            case operator of
              "::" => SeqV (IntV intValue :: sequence)
            | _ => raise Impossible
          end
        | (BoolV boolValue1, SeqV sequence) => 
          let in
            case operator of
              "::" => SeqV (BoolV boolValue1 :: sequence)
            | _ => raise Impossible
          end
        | (ListV list, SeqV sequence) => 
          let in
            case operator of
              "::" => SeqV (ListV list :: sequence)
            | _ => raise Impossible
          end
        | _ => raise Impossible
      end
  | eval (If (expression1, expression2, expression3)) env = 
    let in
      case eval expression1 env of 
        BoolV true => eval expression2 env
      | BoolV false => eval expression3 env
      | _ => raise Impossible
    end
  | eval (Match (expression1, matchList)) env = 
    let 
      val evalMatchVar = eval expression1 env 
      fun match (matchVar, head::[]) env =
          let in
            case head of
              (SOME expression2, expression3) => if eval expression2 env = matchVar then expression3 else raise ValueNotFoundInMatch
            | (NONE, expression3) => expression3
          end
        | match (matchVar, head::tail) env =  let in
            case head of
              (SOME expression2, expression3) => if eval expression2 env = matchVar then expression3 else match (matchVar, tail) env
            | (NONE, expression3) => raise Impossible
          end
        | match (matchVar, _ ) env = raise Impossible
    in
      eval (match(evalMatchVar, matchList) env) env
    end
  | eval (Call (expression1, expression2)) env = 
    let
      fun buildArguments (List (head::[])) = [eval head env]
        | buildArguments (List (head::tail)) = [eval head env] @ buildArguments (List tail)
        | buildArguments (expression) = [eval expression env]
      val scope = [("$list", ListV (buildArguments expression2))] @ env
      val function = eval expression1 env
    in
      case function of
        Clos(name, var, expression, closureScope) =>
          let
            val expression2Value = eval expression2 scope
            val scope = (var, expression2Value)::(name, function)::closureScope
          in
            eval expression scope
          end
      | _ => raise NotAFunc
    end
  | eval (List []) env = ListV []
  | eval (List list) env = 
    let
      fun evalList (head::[]) = eval head env :: []
        | evalList (head::tail) = eval head env :: evalList tail
        | evalList _ = raise Impossible;
    in
      ListV (evalList list)
    end
  | eval (Item (index, expression)) env =
    let
      val value = eval expression env
    in
      case value of
        ListV list => getItemVAt (list, index)
      | SeqV sequence => getItemVAt (sequence, index)
      | _ => raise Impossible
    end
  | eval (Anon (funType, argument, expression)) env = Clos ("", argument, expression, env)