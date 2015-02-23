/** file Interpreter.scala **/

abstract class Tuple {
  def getJamVal: JamVal
  def getAST: AST
}

class ValueTuple(jamVal: JamVal, ast: AST) extends Tuple{
  override def getJamVal: JamVal = jamVal
  override def getAST: AST = ast
}

class NameTuple(helper: (AST, Map[Symbol, NameTuple]) => JamVal, notUntil: (AST, Map[Symbol, NameTuple]) => AST, env: Map[Symbol, NameTuple], rawVar: AST) extends Tuple{
  override def getJamVal: JamVal = helper(rawVar, env)
  override def getAST: AST = notUntil(rawVar, env)
}

class NeedTuple(helper: (AST, Map[Symbol, NeedTuple]) => JamVal, notUntil: (AST, Map[Symbol, NeedTuple]) => AST, env: Map[Symbol, NeedTuple], rawVar: AST) extends Tuple{
  lazy val jamVal = helper(rawVar, env)
  lazy val lazyAst = notUntil(rawVar, env)
  override def getJamVal: JamVal = jamVal
  override def getAST: AST = lazyAst
}

class EvalException(msg: String) extends RuntimeException(msg)
class SyntaxException(msg: String) extends RuntimeException(msg)

class Interpreter(reader: java.io.Reader) {
  def this(fileName: String) = this(new java.io.FileReader(fileName))

  val ast: AST = new Parser(reader).parse()

  def makeConsValue[Tp](first: JamVal, helper: (AST, Map[Symbol, Tp]) => JamVal, arg1: AST, e: Map[Symbol, Tp]): JamVal =
  helper(arg1, e) match {
    case EmptyConstant => new JamListNEValue(first, EmptyConstant)
    case j: JamListNEValue => new JamListNEValue(first, j)
    case _ => throw new EvalException("The rest of cons is not of a valid type")
  }

  def makeConsName[Tp](first: JamVal, helper: (AST, Map[Symbol, Tp]) => JamVal, arg1: AST, e: Map[Symbol, Tp]): JamVal =
    new JamListNEName(first, helper, arg1, e)

  def makeConsNeed[Tp](first: JamVal, helper: (AST, Map[Symbol, Tp]) => JamVal, arg1: AST, e: Map[Symbol, Tp]): JamVal =
    new JamListNENeed(first, helper, arg1, e)

  def valueValue: JamVal = callGeneral[ValueTuple, JamListNEValue](
    (e, defs, helper, untilNotVariable) => {
      var newMap = e
      defs.foreach(d => {
        var pair = (d.lhs.sym, new ValueTuple(helper(d.rhs, newMap), untilNotVariable(d.rhs, newMap)))
        newMap += pair
      })
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
      newMap
    },
    makeConsValue[ValueTuple]
  )

  def nameValue: JamVal = callGeneral[NameTuple, JamListNEValue](
    (e, defs, helper, untilNotVariable) => {
      var newMap = e
      defs.foreach(d => {
        var pair = (d.lhs.sym, new NameTuple(helper, untilNotVariable, newMap, d.rhs))
        newMap += pair
      })
      newMap
    },
    (en, e, vars, args, helper, untilNotVariable) => {
      var newMap = en
      var newMap2 = e
      if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
      vars.zip(args).foreach(p => {
        var pair = (p._1.sym, new NameTuple(helper, untilNotVariable, newMap2, p._2))
        newMap += pair
        newMap2 += pair
      })
      //vars.zip(args).map(pair => (pair._1.sym, new NeedTuple(helper, untilNotVariable, e, pair._2))).foreach(pair => newMap += (pair))
      newMap
    },
    makeConsValue[NameTuple]
  )

  def needValue: JamVal = callGeneral[NeedTuple, JamListNEValue](
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
    },
    makeConsValue[NeedTuple]
  )

  def valueName: JamVal = callGeneral[ValueTuple, JamListNEName[ValueTuple]](
    (e, defs, helper, untilNotVariable) => {
      var newMap = e
      defs.foreach(d => {
        var pair = (d.lhs.sym, new ValueTuple(helper(d.rhs, newMap), untilNotVariable(d.rhs, newMap)))
        newMap += pair
      })
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
      newMap
    },
    makeConsName[ValueTuple]
  )

  def nameName: JamVal = callGeneral[NameTuple, JamListNEName[NameTuple]](
    (e, defs, helper, untilNotVariable) => {
      var newMap = e
      defs.foreach(d => {
        var pair = (d.lhs.sym, new NameTuple(helper, untilNotVariable, newMap, d.rhs))
        newMap += pair
      })
      newMap
    },
    (en, e, vars, args, helper, untilNotVariable) => {
      var newMap = en
      var newMap2 = e
      if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
      vars.zip(args).foreach(p => {
        var pair = (p._1.sym, new NameTuple(helper, untilNotVariable, newMap2, p._2))
        newMap += pair
        newMap2 += pair
      })
      newMap
    },
    makeConsName[NameTuple]
  )

  def needName: JamVal = callGeneral[NeedTuple, JamListNEName[NeedTuple]](
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
      newMap
    },
    makeConsName[NeedTuple]
  )

  def valueNeed: JamVal = callGeneral[ValueTuple, JamListNENeed[ValueTuple]](
    (e, defs, helper, untilNotVariable) => {
      var newMap = e
      defs.foreach(d => {
        var pair = (d.lhs.sym, new ValueTuple(helper(d.rhs, newMap), untilNotVariable(d.rhs, newMap)))
        newMap += pair
      })
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
      newMap
    },
    makeConsNeed[ValueTuple]
  )

  def nameNeed: JamVal = callGeneral[NameTuple, JamListNENeed[NameTuple]](
    (e, defs, helper, untilNotVariable) => {
      var newMap = e
      defs.foreach(d => {
        var pair = (d.lhs.sym, new NameTuple(helper, untilNotVariable, newMap, d.rhs))
        newMap += pair
      })
      newMap
    },
    (en, e, vars, args, helper, untilNotVariable) => {
      var newMap = en
      var newMap2 = e
      if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
      vars.zip(args).foreach(p => {
        var pair = (p._1.sym, new NameTuple(helper, untilNotVariable, newMap2, p._2))
        newMap += pair
        newMap2 += pair
      })
      newMap
    },
    makeConsNeed[NameTuple]
  )

  def needNeed: JamVal = callGeneral[NeedTuple, JamListNENeed[NeedTuple]](
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
      newMap
    },
    makeConsNeed[NeedTuple]
  )

  private def callGeneral[Tp <: Tuple, Cons <: JamList](
                                        letBinding: (Map[Symbol, Tp], Array[Def], (AST, Map[Symbol, Tp]) => JamVal, (AST, Map[Symbol, Tp]) => AST) => Map[Symbol, Tp],
                                        appMapBinding: (Map[Symbol, Tp], Map[Symbol, Tp], Array[Variable], Array[AST], (AST, Map[Symbol, Tp]) => JamVal, (AST, Map[Symbol, Tp]) => AST) => Map[Symbol, Tp],
                                        makeCons: (JamVal, (AST, Map[Symbol, Tp]) => JamVal, AST, Map[Symbol, Tp]) => JamVal
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
          case True => helper(arg2, e) match {
            case bool: BoolConstant => bool
            case _ => throw new EvalException("And operation arg2 is not boolean")
          }
          case _ => throw new EvalException("And operation arg1 is not boolean")
        }
        case OpOr => helper(arg1, e) match {
          case True => True
          case False => helper(arg2, e) match {
            case bool: BoolConstant => bool
            case _ => throw new EvalException("Or operation arg2 is not boolean")
          }
          case _ => throw new EvalException("Or operation arg1 is not boolean")
        }
      }
      // The followings are below Term
      case If(test: AST, conseq: AST, alt: AST) => helper(test, e) match {
        case True => helper(conseq, e)
        case False => helper(alt, e)
        case _ => throw new EvalException("If condition is not boolean")
      }
      case Let(defs: Array[Def], body: AST) =>
        val repeatedMap = defs.map(d => d.lhs).groupBy(l => l).map(t => (t._1, t._2.length)).filter(pair => pair._2 > 1)
        if (repeatedMap.size > 0) throw new SyntaxException(repeatedMap.keys + " are repeatedly defined variables")
        else helper(body, letBinding(e, defs, helper, untilNotVariable))

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
          case _ => throw new EvalException("Tilde without Boolean")
        }
      }
      case maplit: MapLiteral =>
        val repeatedMap = maplit.vars.groupBy(l => l).map(t => (t._1, t._2.length)).filter(pair => pair._2 > 1)
        if (repeatedMap.size > 0) throw new SyntaxException(repeatedMap.keys + " are repeatedly defined variables")
        else JamClosure[Tp](maplit, e)

      case App(rator: AST, args: Array[AST]) => helper(rator, e) match {
        // PrimFun
        case FunctionPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument")
          helper(args(0), e) match {
            case _: JamFun => True
            case _ => False
          }

        case NumberPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for NumberPPrim")
          helper(args(0), e) match {
            case IntConstant(_) => True
            case _ => False
          }

        case ListPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for ListPPrim")
          helper(args(0), e) match {
            case _ : JamList => True
            case _ => False
          }

        case ConsPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for ConsPPrim")
          helper(args(0), e) match {
            case _ : JamList => True
            case _ => False
          }

        case EmptyPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for EmptyPPrim")
          helper(args(0), e) match {
            case EmptyConstant => True
            case _ => False
          }

        case ArityPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          helper(args(0), e) match {
            case JamClosure(body: MapLiteral, env: Map[Symbol, Tp]) => IntConstant(body.vars.length)
            case ConsPrim => IntConstant(2)
            case _: PrimFun => IntConstant(1)
            case _ => throw new EvalException("arg0 is not a function")
          }

        case ConsPrim =>
          if (args.length != 2) throw new EvalException("Should have two arguments")
          makeCons(helper(args(0), e), helper, args(1), e)

        case FirstPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          helper(args(0),e) match {
            case EmptyConstant => throw new EvalException("arg0 is empty, it is a " + args(0).getClass)
            case c: Cons => c.getFirst
            case _ => throw new EvalException("arg0 is not a jam list, it is a " + args(0).getClass)
          }

        case RestPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          helper(args(0),e) match {
            case EmptyConstant => throw new EvalException("arg0 is empty, it is a " + args(0).getClass)
            case c: Cons => c.getRest
            case _ => throw new EvalException("arg0 is not a jam list, it is a " + args(0).getClass)
          }

        case JamClosure(MapLiteral(vars, body), en: Map[Symbol, Tp]) =>
          // Bind
          if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
          val repeatedMap = vars.groupBy(l => l).map(p => (p._1, p._2.length)).filter(p => p._2 > 1)
          if(repeatedMap.size > 1) throw new SyntaxException(repeatedMap + " are repeatedly defined variables.")
          else helper(body, appMapBinding(en, e, vars, args, helper, untilNotVariable))

        case _=> throw new EvalException("Did not match. Got a class: " + rator.getClass)
      }
      case pf: PrimFun => pf
    }
    helper(ast, Map())
  }
}