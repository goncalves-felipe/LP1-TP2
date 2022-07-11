(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun getItemTAt ([], index) = raise ListOutOfRange
  | getItemTAt ((head::_), 1) = head
  | getItemTAt ((head::[]), index) = raise ListOutOfRange
  | getItemTAt ((head::tail), index) = getItemTAt (tail, index - 1)

fun teval (ConI _) _ = IntT
  | teval (ConB _) _ = BoolT
  | teval (ESeq (SeqT seqType)) _ = SeqT seqType
  | teval (ESeq _) _ = raise EmptySeq
  | teval (Var varId) env = lookup env varId
  | teval (Let(var, expression1, expression2)) env =
    let
      val expression1Type = teval expression1 env
      val newScope = (var, expression1Type) :: env
    in
      teval expression2 newScope
    end
  | teval (Letrec(funName, argumentType, argument, funType, expression1, expression2)) env =
    let
      val recursionEnv = (funName, FunT (argumentType, funType))
      val argumentEnv = (argument, argumentType)
      val expression1Type = teval expression1 (recursionEnv :: argumentEnv :: env)
      val expression2Type = teval expression2 (recursionEnv :: env)
    in
      if expression1Type = funType then expression2Type else raise WrongRetType
    end
  | teval (Prim1(operator, expression)) env =
    let
      val expressionType = teval expression env
    in
      case operator of
        "!" => if expressionType = BoolT then BoolT else raise UnknownType
      | "-" => if expressionType = IntT then IntT else raise UnknownType
      | "hd" => let in
          case expressionType of
            SeqT sequence => sequence
          | _ => raise UnknownType
        end
      | "tl" => let in
          case expressionType of
            SeqT sequence => SeqT sequence
          | _ => raise UnknownType
        end
      | "ise" => let in
          case expressionType of
            SeqT sequence => BoolT
          | _ => raise UnknownType
        end
      | "print" => ListT []
      | _ => raise UnknownType
    end
  | teval (Prim2(oper, expression1, expression2)) env =
    let
      val expression1Type = teval expression1 env
      val expression2Type = teval expression2 env
    in
      case oper of
        "&&" => if expression1Type = BoolT andalso expression2Type = BoolT then BoolT else raise UnknownType
      | "::" => let in
          case (expression1Type, expression2Type) of
            (IntT, ListT []) => SeqT IntT
          | (IntT, SeqT sequence2) => if sequence2 = IntT then SeqT sequence2 else raise NotEqTypes
          | (BoolT, ListT []) => SeqT BoolT
          | (BoolT, SeqT sequence2) => if sequence2 = BoolT then SeqT sequence2 else raise NotEqTypes
          | (ListT list, ListT []) => SeqT (ListT list)
          | (ListT list, SeqT sequence2) => if sequence2 = ListT list then SeqT sequence2 else raise NotEqTypes
          | _ => raise UnknownType
        end
      | "-" => if expression1Type = IntT andalso expression2Type = IntT then IntT else raise UnknownType
      | "+" => if expression1Type = IntT andalso expression2Type = IntT then IntT else raise UnknownType
      | "/" => if expression1Type = IntT andalso expression2Type = IntT then IntT else raise UnknownType
      | "*" => if expression1Type = IntT andalso expression2Type = IntT then IntT else raise UnknownType
      | "<" => if expression1Type = IntT andalso expression2Type = IntT then BoolT else raise UnknownType
      | "<=" => if expression1Type = IntT andalso expression2Type = IntT then BoolT else raise UnknownType
      | "!=" => if expression1Type = expression2Type andalso (expression1Type = IntT orelse expression1Type = BoolT) then BoolT else raise NotEqTypes
      | "=" => if expression1Type = expression2Type andalso (expression1Type = IntT orelse expression1Type = BoolT) then BoolT else raise NotEqTypes
      | ";" => expression2Type
      | _ => raise UnknownType
    end
  | teval (If(ifCondExp, thenExp, elseExp)) env =
    let
      val ifCondType = teval ifCondExp env
      val thenType = teval thenExp env
      val elseType = teval elseExp env
    in
      case ifCondType of
        BoolT => if thenType = elseType then thenType else raise DiffBrTypes
      | _ => raise IfCondNotBool
    end
  | teval (Match(expression1, matchList)) env = let in case matchList of
        [] => raise NoMatchResults
      | _ => let
          val initialCond = teval expression1 env
          val firstRes = (#2 (hd matchList))
          val firstResType = teval firstRes env
          fun match (Match(expression1, matchList)) env =
              let in
                case matchList of
                  head::[] => let in
                      case head of
                        (SOME expression2, expression3) => 
                          if (teval expression3 env) = firstResType then
                            if initialCond = (teval expression2 env) then 
                              teval expression3 env 
                            else raise MatchCondTypesDiff
                          else raise MatchResTypeDiff
                      | (NONE, expression3) => if (teval expression3 env) = firstResType then firstResType else raise MatchResTypeDiff
                    end
                | head::tail => let in
                    case head of
                      (SOME expression2, expression3) => 
                        if (teval expression3 env) = firstResType then
                          if initialCond = (teval expression2 env) then
                            match (Match(expression1, tail)) env 
                          else raise MatchCondTypesDiff
                        else raise MatchResTypeDiff
                    | _ => raise UnknownType
                  end
                | _ => raise NoMatchResults
              end
            | match _ _ = raise UnknownType
        in
          match (Match(expression1, matchList)) env
        end
    end
  | teval (Call(expression2, expression1)) env =
    let
      val expression1Type = teval expression1 env
      val expression2Type = teval expression2 env
    in
      case expression2Type of
        FunT (argumentType, resultType) => 
          if expression1Type = argumentType then resultType else raise CallTypeMisM
      | _ => raise NotFunc
    end
  | teval (List list) env =
    let
      fun evalList (head::[]) = (teval head env)::[]
        | evalList (head::tail) = (teval head env)::evalList tail
        | evalList _ = []
    in
      ListT (evalList(list))
    end
  | teval (Item (index, expression)) env =
    let
      val expressionType = teval expression env
    in
      case expressionType of
        ListT list => getItemTAt(list, index)
      | _ => raise OpNonList
    end
  | teval (Anon(funType, argument, expression)) env = 
    let
      val nEnv = (argument, funType) :: env
      val expressionType = teval expression nEnv
    in
      FunT (funType, expressionType)
    end