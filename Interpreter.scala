/** file Interpreter.scala **/
class Interpreter(reader: java.io.Reader) {
  def this(fileName: String) = this(new java.io.FileReader(fileName))

  val ast: AST = new Parser(reader).parse()

  private def bindRecLetName(e: Env[NameTuple], defs: Array[Def], evaluate: (AST, Env[NameTuple]) => JamVal, untilNotVariable: (AST, Env[NameTuple]) => AST) = {
      var newMap = e
      var proxyList = List[Env[NameTuple]]()
      defs.foreach(d => {
        val thisProxy = new ProxyEnv[NameTuple]
        newMap += (d.lhs.sym, new NameTuple(evaluate, untilNotVariable, thisProxy, d.rhs))
        proxyList = thisProxy :: proxyList
      })
      // Set new map to the proxy
      proxyList.foreach(proxy => proxy.setMap(newMap.getMap))
      newMap
  }
  private def bindRecLetNeed(e: Env[NeedTuple], defs: Array[Def], evaluate: (AST, Env[NeedTuple]) => JamVal, untilNotVariable: (AST, Env[NeedTuple]) => AST) = {
    var newMap = e
    var proxyList = List[Env[NeedTuple]]()
    defs.foreach(d => {
      val thisProxy = new ProxyEnv[NeedTuple]
      newMap += (d.lhs.sym, new NeedTuple(evaluate, untilNotVariable, thisProxy, d.rhs))
      proxyList = thisProxy :: proxyList
    })
    // Set new map to the proxy
    proxyList.foreach(proxy => proxy.setMap(newMap.getMap))
    newMap
  }

  private def bindAppMapValue(en: Env[ValueTuple], e: Env[ValueTuple], vars: Array[Variable], args: Array[AST], evaluate: (AST, Env[ValueTuple]) => JamVal, untilNotVariable: (AST, Env[ValueTuple]) => AST) = {
      var newMap = en
      var newMap2 = e
      if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
      vars.zip(args).foreach(p => {
        var pair = (p._1.sym, new ValueTuple(evaluate(p._2, newMap2), untilNotVariable(p._2, newMap2)))
        newMap += pair
        newMap2 += pair
      })
      newMap
  }
  private def bindAppMapName(en: Env[NameTuple], e: Env[NameTuple], vars: Array[Variable], args: Array[AST], evaluate: (AST, Env[NameTuple]) => JamVal, untilNotVariable: (AST, Env[NameTuple]) => AST) = {
      var newMap = en
      var newMap2 = e
      if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
      vars.zip(args).foreach(p => {
        var pair = (p._1.sym, new NameTuple(evaluate, untilNotVariable, newMap2, p._2))
        newMap += pair
        newMap2 += pair
      })
      newMap
  }
  private def bindAppMapNeed(en: Env[NeedTuple], e: Env[NeedTuple], vars: Array[Variable], args: Array[AST], evaluate: (AST, Env[NeedTuple]) => JamVal, untilNotVariable: (AST, Env[NeedTuple]) => AST) = {
    var newMap = en
    var newMap2 = e
    if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
    vars.zip(args).foreach(p => {
      var pair = (p._1.sym, new NeedTuple(evaluate, untilNotVariable, newMap2, p._2))
      newMap += pair
      newMap2 += pair
    })
    newMap
  }

  private def makeConsValue[Tp](first: JamVal, evaluate: (AST, Env[Tp]) => JamVal, arg1: AST, e: Env[Tp]): JamVal =
  evaluate(arg1, e) match {
    case EmptyConstant => new JamListNEValue(first, EmptyConstant)
    case j: JamListNEValue => new JamListNEValue(first, j)
    case _ => throw new EvalException("The rest of cons is not of a valid type")
  }
  private def makeConsName[Tp](first: JamVal, evaluate: (AST, Env[Tp]) => JamVal, arg1: AST, e: Env[Tp]): JamVal = new JamListNEName(first, evaluate, arg1, e)
  private def makeConsNeed[Tp](first: JamVal, evaluate: (AST, Env[Tp]) => JamVal, arg1: AST, e: Env[Tp]): JamVal = new JamListNENeed(first, evaluate, arg1, e)

  def valueValue: JamVal = callGeneral[ValueTuple, JamListNEValue](
    (e, defs, evaluate, untilNotVariable) => {
      var newMap = e
      defs.foreach(d => {
        var pair = (d.lhs.sym, new ValueTuple(evaluate(d.rhs, newMap), untilNotVariable(d.rhs, newMap)))
        newMap += pair
      })
      newMap
    },
    bindAppMapValue,
    makeConsValue[ValueTuple]
  )
  def nameValue: JamVal = callGeneral[NameTuple, JamListNEValue](
    bindRecLetName,
    bindAppMapName,
    makeConsValue[NameTuple]
  )
  def needValue: JamVal = callGeneral[NeedTuple, JamListNEValue](
    bindRecLetNeed,
    bindAppMapNeed,
    makeConsValue[NeedTuple]
  )

  def valueName: JamVal = callGeneral[ValueTuple, JamListNEName[ValueTuple]](
    (e, defs, evaluate, untilNotVariable) => {
      var newMap = e
      defs.foreach(d => {
        var pair = (d.lhs.sym, new ValueTuple(evaluate(d.rhs, newMap), untilNotVariable(d.rhs, newMap)))
        newMap += pair
      })
      newMap
    },
    bindAppMapValue,
    makeConsName[ValueTuple]
  )
  def nameName: JamVal = callGeneral[NameTuple, JamListNEName[NameTuple]](
    bindRecLetName,
    bindAppMapName,
    makeConsName[NameTuple]
  )
  def needName: JamVal = callGeneral[NeedTuple, JamListNEName[NeedTuple]](
    bindRecLetNeed,
    bindAppMapNeed,
    makeConsName[NeedTuple]
  )

  def valueNeed: JamVal = callGeneral[ValueTuple, JamListNENeed[ValueTuple]](
    (e, defs, evaluate, untilNotVariable) => {
      var newMap = e
      defs.foreach(d => {
        var pair = (d.lhs.sym, new ValueTuple(evaluate(d.rhs, newMap), untilNotVariable(d.rhs, newMap)))
        newMap += pair
      })
      newMap
    },
    bindAppMapValue,
    makeConsNeed[ValueTuple]
  )
  def nameNeed: JamVal = callGeneral[NameTuple, JamListNENeed[NameTuple]](
    bindRecLetName,
    bindAppMapName,
    makeConsNeed[NameTuple]
  )
  def needNeed: JamVal = callGeneral[NeedTuple, JamListNENeed[NeedTuple]](
    bindRecLetNeed,
    bindAppMapNeed,
    makeConsNeed[NeedTuple]
  )

  private def callGeneral[Tp <: Tuple, Cons <: JamList](
                                        letBinding: (Env[Tp], Array[Def], (AST, Env[Tp]) => JamVal, (AST, Env[Tp]) => AST) => Env[Tp],
                                        appMapBinding: (Env[Tp], Env[Tp], Array[Variable], Array[AST], (AST, Env[Tp]) => JamVal, (AST, Env[Tp]) => AST) => Env[Tp],
                                        makeCons: (JamVal, (AST, Env[Tp]) => JamVal, AST, Env[Tp]) => JamVal
                                        ): JamVal = {
    def binIntOp(e: Env[Tp], arg1: AST, arg2: AST, op: (Int, Int) => Int, exceptionContent: String) = (evaluate(arg1, e), evaluate(arg2, e)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => IntConstant(op(value1, value2))
      case _ => throw new EvalException(exceptionContent)
    }
    def binIntCmpOp(e: Env[Tp], arg1: AST, arg2: AST, op: (Int, Int) => Boolean, exceptionContent: String) = (evaluate(arg1, e), evaluate(arg2, e)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => op(value1, value2) match {
        case true => True
        case false => False
      }
      case _ => throw new EvalException(exceptionContent)
    }
    def UnIntOp(e: Env[Tp], arg: AST, op: Int => Int, exceptionContent: String) = evaluate(arg, e) match {
      case (IntConstant(value: Int)) => IntConstant(op(value))
      case _ => throw new EvalException(exceptionContent)
    }
    def untilNotVariable(input: AST, e: Env[Tp]): AST = {
      input match {
        case vv: Variable => untilNotVariable(e.get(vv.sym).getAST, e)
        case _ => input
      }
    }

    def evaluate(ast: AST, e: Env[Tp]): JamVal = ast match {
      case BinOpApp(binOpPlus: BinOp, arg1: AST, arg2: AST) => binOpPlus match {
        case BinOpPlus => binIntOp(e, arg1, arg2, _ + _, "BinOpPlus not with int")
        case BinOpMinus => binIntOp(e, arg1, arg2, _ - _, "BinOpMinus not with int")
        case OpTimes => binIntOp(e, arg1, arg2, _ * _, "OpTimes not with int")
        case OpDivide => binIntOp(e, arg1, arg2, _ / _, "OpDivide not with int")
        case OpEquals => (evaluate(arg1, e), evaluate(arg2, e)) match {
          case (int1: IntConstant, int2: IntConstant) => int1.value == int2.value match {
            case true => True
            case false => False
          }
          case (obj1, obj2) => obj1 == obj2 match {
            case true => True
            case false => False
          }
        }
        case OpNotEquals => (evaluate(arg1, e), evaluate(arg2, e)) match {
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
        case OpAnd => evaluate(arg1, e) match {
          case False => False
          case True => evaluate(arg2, e) match {
            case bool: BoolConstant => bool
            case _ => throw new EvalException("And operation arg2 is not boolean")
          }
          case _ => throw new EvalException("And operation arg1 is not boolean")
        }
        case OpOr => evaluate(arg1, e) match {
          case True => True
          case False => evaluate(arg2, e) match {
            case bool: BoolConstant => bool
            case _ => throw new EvalException("Or operation arg2 is not boolean")
          }
          case _ => throw new EvalException("Or operation arg1 is not boolean")
        }
      }
      // The followings are below Term
      case If(test: AST, conseq: AST, alt: AST) => evaluate(test, e) match {
        case True => evaluate(conseq, e)
        case False => evaluate(alt, e)
        case _ => throw new EvalException("If condition is not boolean")
      }
      case Let(defs: Array[Def], body: AST) =>
        val repeatedMap = defs.map(d => d.lhs).groupBy(l => l).map(t => (t._1, t._2.length)).filter(pair => pair._2 > 1)
        if (repeatedMap.size > 0) throw new SyntaxException(repeatedMap.keys + " are repeatedly defined variables")
        else evaluate(body, letBinding(e, defs, evaluate, untilNotVariable))

      // Constant
      case EmptyConstant => EmptyConstant
      case b: BoolConstant => b
      case i: IntConstant => i

      case Variable(sym: Symbol) => if (e.contains(sym)) e.get(sym).getJamVal else throw new SyntaxException(sym + " is a free variable!")

      case UnOpApp(rator: UnOp, arg: AST) => rator match {
        case UnOpPlus =>  UnIntOp(e, arg, + _, "unary plus without int")
        case UnOpMinus => UnIntOp(e, arg, - _, "unary minus without int")
        case OpTilde => evaluate(arg, e) match {
          case True => False
          case False => True
          case _ => throw new EvalException("Tilde without Boolean")
        }
      }
      case maplit: MapLiteral =>
        val repeatedMap = maplit.vars.groupBy(l => l).map(t => (t._1, t._2.length)).filter(pair => pair._2 > 1)
        if (repeatedMap.size > 0) throw new SyntaxException(repeatedMap.keys + " are repeatedly defined variables")
        else JamClosure[Tp](maplit, e)

      case App(rator: AST, args: Array[AST]) => evaluate(rator, e) match {
        // PrimFun
        case FunctionPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument")
          evaluate(args(0), e) match {
            case _: JamFun => True
            case _ => False
          }

        case NumberPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for NumberPPrim")
          evaluate(args(0), e) match {
            case IntConstant(_) => True
            case _ => False
          }

        case ListPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for ListPPrim")
          evaluate(args(0), e) match {
            case _ : JamList => True
            case _ => False
          }

        case ConsPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for ConsPPrim")
          evaluate(args(0), e) match {
            case _ : JamList => True
            case _ => False
          }

        case EmptyPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for EmptyPPrim")
          evaluate(args(0), e) match {
            case EmptyConstant => True
            case _ => False
          }

        case ArityPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          evaluate(args(0), e) match {
            case JamClosure(body: MapLiteral, env: Env[Tp]) => IntConstant(body.vars.length)
            case ConsPrim => IntConstant(2)
            case _: PrimFun => IntConstant(1)
            case _ => throw new EvalException("arg0 is not a function")
          }

        case ConsPrim =>
          if (args.length != 2) throw new EvalException("Should have two arguments")
          makeCons(evaluate(args(0), e), evaluate, args(1), e)

        case FirstPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          evaluate(args(0),e) match {
            case EmptyConstant => throw new EvalException("arg0 is empty, it is a " + args(0).getClass)
            case c: Cons => c.getFirst
            case _ => throw new EvalException("arg0 is not a jam list, it is a " + args(0).getClass)
          }

        case RestPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          evaluate(args(0),e) match {
            case EmptyConstant => throw new EvalException("arg0 is empty, it is a " + args(0).getClass)
            case c: Cons => c.getRest
            case _ => throw new EvalException("arg0 is not a jam list, it is a " + args(0).getClass)
          }

        case JamClosure(MapLiteral(vars, body), en: Env[Tp]) =>
          // Bind
          if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
          val repeatedMap = vars.groupBy(l => l).map(p => (p._1, p._2.length)).filter(p => p._2 > 1)
          if(repeatedMap.size > 1) throw new SyntaxException(repeatedMap + " are repeatedly defined variables.")
          else evaluate(body, appMapBinding(en, e, vars, args, evaluate, untilNotVariable))

        case _=> throw new EvalException("Did not match. Got a class: " + rator.getClass)
      }
      case pf: PrimFun => pf
    }
    evaluate(ast, new ConcreteEnv[Tp](Map[Symbol, Tp]()))
  }
}