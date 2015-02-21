import jdk.nashorn.internal.runtime.regexp.joni.exception.SyntaxException

/** file Interpreter.scala **/

abstract class Tuple {
  def getJamVal: JamVal
  def getAST: AST
}

class ValueTuple(jamVal: JamVal, ast: AST) extends Tuple{
  override def getJamVal = jamVal
  override def getAST = ast
}

class NameTuple(jamVal: => JamVal, ast: => AST) extends Tuple{
  override def getJamVal = jamVal
  override def getAST = ast
}

class NeedTuple(helper: (AST, Map[Symbol, NeedTuple]) => JamVal, notUntil: (AST, Map[Symbol, NeedTuple]) => AST, env: Map[Symbol, NeedTuple], rawVar: AST) extends Tuple{
  lazy val jamVal = helper(rawVar, env)
  lazy val lazyAst = notUntil(rawVar, env)
  override def getJamVal = jamVal
  override def getAST = lazyAst
}

class EvalException(msg: String) extends RuntimeException(msg)
class Interpreter(reader: java.io.Reader) {
  def this(fileName: String) = this(new java.io.FileReader(fileName))

  val ast: AST = new Parser(reader).parse()

  def callByValue: JamVal = callGeneral[ValueTuple](
    (e, defs, helper, untilNotVariable) => {
      var newMap = e
      defs.foreach(d => {
        var pair = (d.lhs.sym, new ValueTuple(helper(d.rhs, newMap), untilNotVariable(d.rhs, newMap)))
        newMap += pair
      })
      //defs.map(d => (d.lhs.sym, new ValueTuple(helper(d.rhs, newMap), untilNotVariable(d.rhs, newMap)))).foreach(pair => newMap += pair)
      newMap
    },
    (en, e, vars, args, helper, untilNotVariable) => {
      var newMap = en
      var newMap2 = e
      if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
      vars.zip(args).foreach(p => {
        var pair = (p._1.sym, new ValueTuple(helper(p._2, newMap2), untilNotVariable(p._2, newMap2)))
        newMap += pair
        newMap2 += pair
      })
      //vars.zip(args).map(pair => (pair._1.sym, new ValueTuple(helper(pair._2, e), untilNotVariable(pair._2, e)))).foreach(pair => newMap += (pair))
      newMap
    }
  )

  def callByName: JamVal = callGeneral[NameTuple](
    (e, defs, helper, untilNotVariable) => {
      var newMap = e
      defs.foreach(d => {
        var pair = (d.lhs.sym, new NameTuple(helper(d.rhs, newMap), untilNotVariable(d.rhs, newMap)))
        newMap += pair
      })
      newMap
    },
    (en, e, vars, args, helper, untilNotVariable) => {
      var newMap = en
      var newMap2 = e
      if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
      vars.zip(args).foreach(p => {
        var pair = (p._1.sym, new NameTuple(helper(p._2, newMap2), untilNotVariable(p._2, newMap2)))
        newMap += pair
        newMap2 += pair
      })
      //vars.zip(args).map(pair => (pair._1.sym, new NameTuple(helper(pair._2, e), untilNotVariable(pair._2, e)))).foreach(pair => newMap += (pair))
      newMap
    }
  )

  def callByNeed: JamVal = callGeneral[NeedTuple](
    (e, defs, helper, untilNotVariable) => {
      var newMap = e
      defs.foreach(d => {
        var pair = (d.lhs.sym, new NeedTuple(helper, untilNotVariable, newMap, d.rhs))
        newMap += pair
      })
      newMap
    },
    (en, e, vars, args, helper, untilNotVariable) => {
      var newMap = en
      var newMap2 = e
      if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
      vars.zip(args).foreach(p => {
        var pair = (p._1.sym, new NeedTuple(helper, untilNotVariable, newMap2, p._2))
        newMap += pair
        newMap2 += pair
      })
      //vars.zip(args).map(pair => (pair._1.sym, new NeedTuple(helper, untilNotVariable, e, pair._2))).foreach(pair => newMap += (pair))
      newMap
    }
  )

  private def callGeneral[Tp <: Tuple](
                                f1: (Map[Symbol, Tp], Array[Def], (AST, Map[Symbol, Tp]) => JamVal, (AST, Map[Symbol, Tp]) => AST) => Map[Symbol, Tp],
                                f2: (Map[Symbol, Tp], Map[Symbol, Tp], Array[Variable], Array[AST], (AST, Map[Symbol, Tp]) => JamVal, (AST, Map[Symbol, Tp]) => AST) => Map[Symbol, Tp]
                                ): JamVal = {
    def binIntOp(e: Map[Symbol, Tp], arg1: AST, arg2: AST, op: (Int, Int) => Int, exceptionContent: String) = (helper(arg1, e), helper(arg2, e)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => IntConstant(op(value1, value2))
      case _ => throw new EvalException(exceptionContent)
    }
    def binIntCmpOp(e: Map[Symbol, Tp], arg1: AST, arg2: AST, op: (Int, Int) => Boolean, exceptionContent: String) = (helper(arg1, e), helper(arg2, e)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => op(value1, value2) match {
        case true => True
        case false => False
      }
      case _ => throw new EvalException(exceptionContent)
    }
    def UnIntOp(e: Map[Symbol, Tp], arg: AST, op: Int => Int, exceptionContent: String) = helper(arg, e) match {
      case (IntConstant(value: Int)) => IntConstant(op(value))
      case _ => throw new EvalException(exceptionContent)
    }
    def untilNotVariable(input: AST, e: Map[Symbol, Tp]): AST = {
      input match {
        case vv: Variable => untilNotVariable(e(vv.sym).getAST, e)
        case _ => input
      }
    }

    def helper(ast: AST, e: Map[Symbol, Tp]): JamVal = ast match {
      case BinOpApp(binOpPlus: BinOp, arg1: AST, arg2: AST) => binOpPlus match {
        case BinOpPlus => binIntOp(e, arg1, arg2, _ + _, "BinOpPlus not with int")
        case BinOpMinus => binIntOp(e, arg1, arg2, _ - _, "BinOpMinus not with int")
        case OpTimes => binIntOp(e, arg1, arg2, _ * _, "OpTimes not with int")
        case OpDivide => binIntOp(e, arg1, arg2, _ / _, "OpDivide not with int")
        case OpEquals => (helper(arg1, e), helper(arg2, e)) match {
          case (int1: IntConstant, int2: IntConstant) => int1.value == int2.value match {
            case true => True
            case false => False
          }
          case (obj1, obj2) => obj1 == obj2 match {
            case true => True
            case false => False
          }
        }
        case OpNotEquals => (helper(arg1, e), helper(arg2, e)) match {
          case (int1: IntConstant, int2: IntConstant) => int1.value != int2.value match {
            case true => True
            case false => False
          }
          case (obj1, obj2) => obj1 != obj2 match {
            case true => True
            case false => False
          }
        }
        case OpLessThan => binIntCmpOp(e, arg1, arg2, _ < _, "BinOpLessthan not with int")
        case OpGreaterThan => binIntCmpOp(e, arg1, arg2, _ > _, "BinOpGreaterthan not with int")
        case OpLessThanEquals => binIntCmpOp(e, arg1, arg2, _ <= _, "BinOpLessthanEq not with int")
        case OpGreaterThanEquals => binIntCmpOp(e, arg1, arg2, _ >= _, "BinOpGreaterthanEq not with int")
        case OpAnd => helper(arg1, e) match {
          case False => False
          case True => helper(arg2, e)
          case _ => throw new EvalException("Never should be here!")
        }
        case OpOr => helper(arg1, e) match {
          case True => True
          case False => helper(arg2, e)
          case _ => throw new EvalException("Never should be here!")
        }
      }
      // The followings are below Term
      case If(test: AST, conseq: AST, alt: AST) => helper(test, e) match {
        case True => helper(conseq, e)
        case False => helper(alt, e)
        case _ => throw new EvalException("Never should be here!")
      }
      case Let(defs: Array[Def], body: AST) => {
        var repeatedMap = defs.map(d => d.lhs).groupBy(l => l).map(t => (t._1, t._2.length)).filter(pair => pair._2 > 1)
        if (repeatedMap.size > 0) throw new SyntaxException(repeatedMap.keys + " are repeatedly defined variables")
        else helper(body, f1(e, defs, helper, untilNotVariable))
      }

      // Constant
      case EmptyConstant => EmptyConstant
      case b: BoolConstant => b
      case i: IntConstant => i

      case Variable(sym: Symbol) => if (e.contains(sym)) e(sym).getJamVal else throw new SyntaxException(sym + " is a free variable!")


      case UnOpApp(rator: UnOp, arg: AST) => rator match {
        case UnOpPlus =>  UnIntOp(e, arg, + _, "unary plus without int")
        case UnOpMinus => UnIntOp(e, arg, - _, "unary minus without int")
        case OpTilde => helper(arg, e) match {
          case True => False
          case False => True
          case _ => throw new EvalException("Never should be here!")
        }
      }
      case maplit: MapLiteral => {
        var repeatedMap = maplit.vars.groupBy(l => l).map(t => (t._1, t._2.length)).filter(pair => pair._2 > 1)
        if (repeatedMap.size > 0) throw new SyntaxException(repeatedMap.keys + " are repeatedly defined variables")
        else JamClosure[Tp](maplit, e)
      }


      case App(rator: AST, args: Array[AST]) => helper(rator, e) match {
        // PrimFun
        case FunctionPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument")
          args(0) match {
            case _: JamFun => True
            case _ => False
          }

        case NumberPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for NumberPPrim")
          args(0) match {
            case IntConstant(_) => True
            case _ => False
          }

        case ListPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for ListPPrim")
          args(0) match {
            case EmptyConstant => True
            case App(ConsPrim, _) => True
            case _ => False
          }

        case ConsPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for ConsPPrim")
          args(0) match {
            case App(ConsPrim, _) => True
            case _ => False
          }

        case EmptyPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for EmptyPPrim")
          args(0) match {
            case EmptyConstant => True
            case _ => False
          }

        case ArityPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          args(0) match {
            case MapLiteral(vars, _) => IntConstant(vars.length)
            case ConsPrim => IntConstant(2)
            case _: PrimFun => IntConstant(1)
            case _ => throw new EvalException("arg0 is not a function")
          }

        case ConsPrim =>
          if (args.length != 2) throw new EvalException("Should have two arguments")
          new JamListNE(helper(args(0), e), helper(args(1), e).asInstanceOf[JamList])

        case FirstPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          args(0) match {
            case va: Variable => e(va.sym).getJamVal match {
              case jl: JamListNE => jl.first
              case _ => throw new EvalException("Calling FirstPrim on a non-list variable")
            }
            case App(ConsPrim, l) => helper(l(0), e)
            case _ => throw new EvalException("arg0 is not a jam list, it is a " + args(0).getClass)
          }

        case RestPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          args(0) match {
            case va: Variable => e(va.sym).getJamVal match {
              case jl: JamListNE => jl.rest
              case _ => throw new EvalException("Calling RestPrim on a non-list variable")
            }
            case App(ConsPrim, l) => helper(l(1), e)
            case _ => throw new EvalException("arg0 is not a jam list, it is a " + args(0).getClass)
          }

        case JamClosure(MapLiteral(vars, body), en: Map[Symbol, Tp]) => {
          // Bind
          if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")

          var repeatedMap = vars.groupBy(l => l).map(p => (p._1, p._2.length)).filter(p => p._2 > 1)
          if(repeatedMap.size > 1) throw new SyntaxException(repeatedMap + " are repeatedly defined variables.")
          else helper(body, f2(en, e, vars, args, helper, untilNotVariable))
        }
        case _=> throw new EvalException("Did not match. Got a class: " + rator.getClass)
      }
      case pf: PrimFun => pf
    }
    helper(ast, Map())
  }
}