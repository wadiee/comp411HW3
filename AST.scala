import java.io._

import scala.collection.mutable.{Map => MutableMap}

/** Jam general AST type */
trait AST

/** Visitor trait for general AST type */
trait ASTVisitor[T] {
  def forBoolConstant(b: BoolConstant): T
  def forIntConstant(i: IntConstant): T
  def forEmptyConstant: T
  def forVariable(v: Variable): T
  def forPrimFun(f: PrimFun): T
  def forUnOpApp(u: UnOpApp): T
  def forBinOpApp(b: BinOpApp): T
  def forApp(a: App): T
  def forMapLiteral(m: MapLiteral): T
  def forIf(i: If): T
  def forLet(l: Let): T
}

/** Jam bound value type; may be a suspension */
//sealed trait BoundVal {
//  def force: JamVal
//}
  
  
/** Jam value type */
//sealed trait JamVal extends BoundVal {
sealed trait JamVal {
  def force:JamVal = this
}

//case class JamList(list: List[JamVal]) extends JamVal {
//case class JamList(Jam) extends JamVal {
//  def first = list.head
//  def rest = JamList(list.tail)
//  override def toString = list.toString
//}
trait JamList extends JamVal{
  def getFirst: JamVal
  def getRest: JamList
}

case class JamListNEValue(first: JamVal, rest: JamList) extends JamList {
  override def getFirst = first
  override def getRest = rest
  private def jamListToList(jamList: JamList): List[JamVal] = jamList match {
    case EmptyConstant => Nil
    case JamListNEValue(first, rest) => first :: jamListToList(rest)
    case _ => throw new EvalException("Name: the type Jamlist not a JamlistNEValue")
  }
  var listme = jamListToList(this)

  override def toString = jamListToList(this).mkString("(", " ", ")")
}

case class JamListNEName[Tp](first: JamVal, helper: (AST, Map[Symbol, Tp]) => JamVal, arg1: AST, e: Map[Symbol, Tp]) extends JamList {
  override def getFirst = first
  override def getRest = helper(arg1, e) match {
    case EmptyConstant => EmptyConstant
    case j: JamListNEName[Tp] => j
    case _ => throw new EvalException("Name: the type of rest is not a jamlist")
  }
  private def jamListToList(jamList: JamList): List[JamVal] = jamList match {
    case EmptyConstant => Nil
    case JamListNEName(firstt, _, arg11, env: Map[Symbol, Tp]) => helper(arg11, env)  match {
      case EmptyConstant => firstt :: Nil
      case j: JamListNEName[Tp] => firstt :: jamListToList(j)
      case _ => throw new EvalException("Name: the type of rest is not a jamlist")
    }
  }
  override def toString = jamListToList(this).mkString("(", " ", ")")
}

case class JamListNENeed[Tp](first: JamVal, helper: (AST, Map[Symbol, Tp]) => JamVal, arg1: AST, e: Map[Symbol, Tp]) extends JamList {
  lazy val restlazy : JamList = helper(arg1, e) match{
    case EmptyConstant => EmptyConstant
    case j: JamListNENeed[Tp] => j
    case _ => throw new EvalException("Need: the type of rest is not a jamlist")
  }
  override def getFirst = first
  override def getRest = restlazy

  private def jamListToList(jamList: JamList): List[JamVal] = jamList match {
    case EmptyConstant => Nil
    case JamListNENeed(firstt, _, arg11, env: Map[Symbol, Tp]) => helper(arg11, env) match {
      case EmptyConstant => firstt :: Nil
      case j: JamListNEName[Tp] => firstt :: jamListToList(j)
      case _ => throw new EvalException("Need: the type of rest is not a jamlist")
    }
  }
  override def toString = jamListToList(this).mkString("(", " ", ")")
}

/** Jam term AST type */
sealed trait Term extends AST {}

/** Jam constant token type; some constant tokens are JamVals but some are not */
sealed trait Constant extends Term with Token

/** a visitor object for Jam values */
trait JamValVisitor[T] {
  def forIntConstant(ic: IntConstant): T
  def forBoolConstant(bc: BoolConstant): T
  def forJamList(jl: JamListNEValue): T
  def forJamFun(jf: JamFun): T
  // def forJamVoid(jv: JamVoid): T  // Supports the addition of recursive let to Jam
}

/** JamVal and Token Data Definitions */

/** Jam token type */
trait Token {}

/** Jam Boolean constant class.  Note: Constant is a subtype of Token. */
sealed abstract class BoolConstant(value: Boolean) extends Constant with JamVal {
  def not = if (value) False else True
  override def toString = value toString
}

case object True extends BoolConstant(true)
case object False extends BoolConstant(false)

object BoolConstant {
  def toBoolConstant(b: Boolean) = if (b) True else False
}

/** Jam integer constant class */
case class IntConstant(value: Int) extends Constant with JamVal {
  def neg = new IntConstant( - value)
  override def toString = value toString
}

/** Other JamVal classes */

/** a Jam function (closure or primitive function) */
sealed trait JamFun extends JamVal

/** The visitor interface for the JamFun type */
//trait JamFunVisitor[T] {
//  def forJamClosure(c: JamClosure): T
//  def forPrimFun(pf: PrimFun): T
//}

/** An environment trait required to define JamClosure below. V is the type of bound values. */
trait Env {
  /** returns the value bound to s in this */
  def get(s: Symbol): ValueTuple
  /** returns a new environment containing the bindings in this augmented by (s -> v) */
  def add(p: Pair[Symbol,ValueTuple]): Env
}
/** A Jam closure */
case class JamClosure[Tup](body: MapLiteral, env: Map[Symbol, Tup]) extends JamFun {
  /**
   * Use default implementations (inherited from java.lang.Object) for
   * equals and hashCode. Closures should not use the default structural
   * equality generated by the Scala compiler for case classes.
   */
  override def equals(other: Any) = super.equals(other)
  override def hashCode = super.hashCode
}

case class NameJamClosure(body: MapLiteral, env: Map[Symbol, NameTuple]) extends JamFun {
  /**
   * Use default implementations (inherited from java.lang.Object) for
   * equals and hashCode. Closures should not use the default structural
   * equality generated by the Scala compiler for case classes.
   */
  override def equals(other: Any) = super.equals(other)
  override def hashCode = super.hashCode
}

case class NeedJamClosure(body: MapLiteral, env: Map[Symbol, NeedTuple]) extends JamFun {
  /**
   * Use default implementations (inherited from java.lang.Object) for
   * equals and hashCode. Closures should not use the default structural
   * equality generated by the Scala compiler for case classes.
   */
  override def equals(other: Any) = super.equals(other)
  override def hashCode = super.hashCode
}

/** A Jam Primitive Function; should be an abstract case class but Scala forbids this*/
abstract class PrimFun(name: Symbol) extends JamFun with Token with Term

///** A dummy Jam value used to implement recursive let */
// case object JamVoid extends JamVal {
//  override def accept[T](jvv: JamValVisitor[T]): T =  jvv.forJamVoid(this)
//}

/** A visitor for PrimFun classes.  Note: the visitor methods do NOT take "this" as an argument. */
trait PrimFunVisitor[T] {
  def forFunctionPPrim: T
  def forNumberPPrim: T
  def forListPPrim: T
  def forConsPPrim: T
  def forEmptyPPrim: T
  def forArityPrim: T
  def forConsPrim: T
  def forFirstPrim: T
  def forRestPrim: T
}

case object FunctionPPrim extends PrimFun(Symbol("function?")) {
  override def toString = "function?"
}

case object NumberPPrim extends PrimFun(Symbol("number?")) {
  override def toString = "number?"
}
case object ListPPrim extends PrimFun(Symbol("list?")) {
  override def toString = "list?"
}
case object ConsPPrim extends PrimFun(Symbol("cons?")) {
  override def toString = "cons?"
}
case object EmptyPPrim extends PrimFun(Symbol("null?")) {
  override def toString = "null?"
}
//case object RefPPrim extends PrimFun(Symbol("ref?")) {
//  override def accept[T](pfv: PrimFunVisitor[T]) = pfv.forRefPPrim
//  override def toString = "ref?" 
//}
case object ArityPrim extends PrimFun(Symbol("arity")) {
  override def toString = "arity"
}
case object ConsPrim extends PrimFun(Symbol("cons")) {
  override def toString = "cons"
}
case object FirstPrim extends PrimFun(Symbol("first")) {
  override def toString = "first"
}
case object RestPrim extends PrimFun(Symbol("rest")) { 
  override def toString = "rest"
}

/** The basic Jam binding framework. */

/** A class representing an unevaluated expresssion (together with the corresponding evaluator). In your
  * interpreter code file you will need to extend this class by concrete suspension classes for call-by-name
  * and call-by-need bindings in the environment.  Note that an environment is simply an immutable 
  * Map[Symbol, BoundVal] or some equivalent data structure.  For call-by-value, every BoundVal is a JamVal.  For call-by-name and call-by-need
  * BoundVal also includes suspensions.*/
//abstract class AbstractSuspension(exp: AST) extends BoundVal
  

/* Token classes that are NOT JamVals */

/** The token representing Jam empty (list).  NOTE: not a JamVal. */
case object EmptyConstant extends Constant with JamList {
  override def getFirst = throw new UnsupportedOperationException("Empty Constant has no get first method")
  override def getRest = throw new UnsupportedOperationException("Empty Constant has no get rest method")
  override def toString = "null"
}

/** Jam variable Token and AST class */
case class Variable(sym: Symbol) extends Term with Token {
  override def toString = sym.name
}

/** Jam operator Token class */
case class OpToken(symbol: Symbol, isUnOp: Boolean, isBinOp: Boolean,  unOp: UnOp, binOp: BinOp) extends Token {
  /* Both UnOp & BinOp constructor */
  def this(s: Symbol, unOp: UnOp, binOp: BinOp) = this(s, true, true, unOp, binOp)  
  /* UnOp only constructor */
  def this(s: Symbol, unOp: UnOp) = this(s, true, false, unOp, null);
  /* BinOp only constructor */
  def this(s: Symbol, binOp: BinOp) = this(s, false, true, null, binOp);
  override def toString = symbol.toString
}

case class KeyWord(name: Symbol) extends Token {
  override def toString = name.toString
}

sealed abstract class Delimiter(s: Symbol) extends Token {
  override def toString = s.toString
}

case object LeftParen extends Delimiter(Symbol("(")) 
case object RightParen extends Delimiter(Symbol(")")) 
case object LeftBrack extends Delimiter(Symbol("["))
case object RightBrack extends Delimiter(Symbol("]"))
case object LeftBrace extends Delimiter(Symbol("{"))
case object RightBrace extends Delimiter(Symbol("}"))
case object Comma extends Delimiter(Symbol(","))
case object SemiColon extends Delimiter(Symbol(";"))

// AST class definitions

/** Jam unary operator application class */
case class UnOpApp(rator: UnOp, arg: AST) extends Term {
  override def toString = rator + " " + arg
}

/** Jam binary operator application class */
case class BinOpApp(rator: BinOp, arg1: AST, arg2: AST) extends AST {
  override def toString =  "(" + arg1 + " " + rator + " " + arg2 + ")"
}

/** Jam fun (closure) class */
case class MapLiteral(vars: Array[Variable], body: AST) extends AST {
  override def toString = "map " + vars.mkString(",") + (if (vars.isEmpty) "" else " ") + "to " + body
}  

/** Jam function (PrimFun or MapLiteral) application class */
case class App(rator: AST, args: Array[AST]) extends Term {
  override def toString =
    rator match {
    case _: Variable | _: PrimFun => 
      rator + "(" + args.mkString(", ") + ")"
    case _ =>
      "(" +  rator + ")(" + args.mkString(", ") + ")" 
  }
}  

/** Jam if expression class */
case class If(test: AST, conseq: AST, alt: AST) extends AST {
  override def toString = "if " + test + " then " + conseq + " else " + alt
}  

/** Jam let expression class */
case class Let(defs: Array[Def], body: AST) extends AST {
  override def toString =  "let " + defs.mkString(" ") + " in " + body
}  

/** Jam definition class */
case class Def(lhs: Variable, rhs: AST) { 
  override def toString = lhs + " := " + rhs + ";"
}

/* UnOp definitions */

abstract class UnOp(s: Symbol) {
  override def toString = s.name
}

trait UnOpVisitor[T] {
  def forUnOpPlus: T
  def forUnOpMinus: T
  def forOpTilde: T
  // def forOpBang(op: OpBang)  // Supports ref cell extension to Jam
  // def forOpRef(opL OpRef)    // Supports ref cell extension to Jam
}

case object UnOpPlus extends UnOp('+)

case object UnOpMinus extends UnOp('-)

case object OpTilde extends UnOp('~)

/* The following commented out objects are used to support the addition of ref cells to Jam */
//object OpBang extends UnOp("!") {
//  override def accept[T](v: UnOpVisitor[T]) =  v.forOpBang(this)
//}
//
//object OpRef extends UnOp("ref") {
//  override def accept[T](v: UnOpVisitor[T]) =  v.forOpRef(this)
//}

/* BinOp definitions */
/** Should be a case class but forbidden by misguided Scala rules */
abstract class BinOp(s: Symbol) {
  override def toString = s.name
}

trait BinOpVisitor[T] {
  def forBinOpPlus: T
  def forBinOpMinus: T
  def forOpTimes: T
  def forOpDivide: T
  def forOpEquals: T
  def forOpNotEquals: T
  def forOpLessThan: T
  def forOpGreaterThan: T
  def forOpLessThanEquals: T
  def forOpGreaterThanEquals: T
  def forOpAnd: T
  def forOpOr: T
  // def forOpGets(op: OpGets)  // Supports the ref cell extension to Jam
}

case object BinOpPlus extends BinOp(Symbol("+"))

case object BinOpMinus extends BinOp(Symbol("-"))

case object OpTimes extends BinOp('*)

case object OpDivide extends BinOp('/)

case object OpEquals extends BinOp('=)

case object OpNotEquals extends BinOp('!=)

case object OpLessThan extends BinOp('<)

case object OpGreaterThan extends BinOp('>)

case object OpLessThanEquals extends BinOp('<=)

case object OpGreaterThanEquals extends BinOp('>=)

case object OpAnd extends BinOp('&)

case object OpOr extends BinOp('|)

/* Supports the ref cell extension to Jam
case object OpGets extends BinOp("<-") {
  override def accept[T](v: BinOpVisitor[T]) =  v.forOpGets 
}
*/

/** Parsing error class */
class ParseException(s: String) extends RuntimeException(s)

/** Jam lexer class.              
  *  Given a Lexer object, the next token in that input stream being
  *  processed by the Lexer is returned by static method readToken(); it
  *  throws a ParseException (a form of RuntimeException) if it
  *  encounters a syntax error.  Calling readToken() advances the cursor
  *  in the input stream to the next token.
  *  The method peek() in the Lexer class has the same behavior as
  *  readToken() except for the fact that it does not advance the cursor.
  */
class Lexer(rdr: Reader) extends StreamTokenizer(new BufferedReader(rdr)) {
  
  // short names for StreamTokenizer codes
  import java.io.StreamTokenizer.{TT_EOF => EOF, TT_EOL => EOL, TT_NUMBER => NUMBER, TT_WORD => WORD}
  
  // These "static" fields should be moved to a companion object
  val PLUS = new OpToken(Symbol("+"), UnOpPlus, BinOpPlus) 
  val MINUS = new OpToken(Symbol("-"), UnOpMinus, BinOpMinus)
  val TIMES =  new OpToken(Symbol("*"), OpTimes)
  val DIVIDE = new OpToken(Symbol("/"), OpDivide)
  val EQUALS = new OpToken(Symbol("="), OpEquals)
  val NOT_EQUALS = new OpToken(Symbol("!="), OpNotEquals)
  val LESS_THAN = new OpToken(Symbol("<"), OpLessThan)
  val GREATER_THAN = new OpToken(Symbol(">"), OpGreaterThan)
  val LESS_THAN_EQUALS = new OpToken(Symbol("<="), OpLessThanEquals)
  val GREATER_THAN_EQUALS = new OpToken(Symbol(">="), OpGreaterThanEquals)
  val AND = new OpToken(Symbol("&"), OpAnd)
  val OR = new OpToken(Symbol("|"), OpOr)
  
  val NOT = new OpToken(Symbol("~"), OpTilde)
  /* Used to support reference cells. */
//  val BANG = new OpToken(Symbol("!"), OpBang)
//  val GETS = new OpToken(Symbol("<-"), OpGets)
//  val REF  = new OpToken(Symbol("ref"), OpRef)
  
  /* Keywords */
  
  val IF     = KeyWord(Symbol("if"))
  val THEN   = KeyWord(Symbol("then"))
  val ELSE   = KeyWord(Symbol("else"))
  val LET    = KeyWord(Symbol("let"))
//  val KeyWord LETREC = KeyWord(Symbol("letrec"))   // Used to support letrec extension
  val IN     = KeyWord(Symbol("in"))
  val MAP    = KeyWord(Symbol("map"))
  val TO     = KeyWord(Symbol("to"))
  val BIND   = KeyWord(Symbol(":="))
  
  // wordtable for classifying words (identifiers/operators) in token stream
  val wordTable: MutableMap[Symbol,Token] = MutableMap()
  
  // Lexer peek cannot be implemented using StreamTokenizer pushBack 
  // because some Tokens are composed of two StreamTokenizer tokens
  
  var buffer: Token = null  // holds token for peek() operation; may be null
  
  /* constructors */
  
  /** Constructs a Lexer for the contents of the specified file */
  def this(fileName: String) { this(new FileReader(fileName)) }
  
  /** Constructs a Lexer for in */
  def this() { this(new InputStreamReader(System.in)) }
  
  /* Initializes lexer tables and the StreamTokenizer that the lexer extends */
  def initLexer() {
    
    // configure StreamTokenizer portion of this
    resetSyntax()
    parseNumbers()
    ordinaryChar('-')
    slashSlashComments(true)
    wordChars('0','9')
    wordChars('a','z')
    wordChars('A','Z')
    wordChars('_','_')
    wordChars('?','?')
    whitespaceChars(0,' ') 
    
    // `+' `-' `*' `/' `~' `=' `<' `>' `&' `|' `:' `;' `,' '!'
    // `(' `)' `[' `]' are ordinary characters (self-delimiting)
    
    initWordTable()
    buffer = null  // buffer initially empty
  }
  
  /** Reads tokens until next end-of-line */
  def flush() {
    eolIsSignificant(true)
    while (nextToken() != EOL)  // eat tokens until EOL
      eolIsSignificant(false)
  }
  
  /** Returns the next token in the input stream without consuming it */
  def peek() = {
    if (buffer eq null) buffer = readToken()
    buffer
  }
  
  /** Reads the next token as defined by StreamTokenizer in the input stream  (consuming it). */
  private def getToken(): Int =
    // synonymous with nextToken() except for throwing an unchecked 
    // ParseException instead of a checked IOException
    try {
      val tokenType = nextToken()
      tokenType
    } catch {
      case e:IOException =>
        throw new ParseException("IOException " + e + "thrown by nextToken()")
    } 
  
  
  /** Reads the next Token in the input stream (consuming it) and returns the corresponding Token object */
  def readToken(): Token = {
    
    /* Use getToken() to read next token and construct a Token object representing that token.  Token representations 
     * for all Token classes except IntConstant are unique; a HashMap is used to avoid duplication.   Hence, eq can 
     * safely be used to compare all Tokens except IntConstants for equality */
    if (buffer != null) {
      val token = buffer
      buffer = null  // clear buffer
      token
    }
    else {
      val tokenType = getToken()
      tokenType match {
        case NUMBER =>
          val intValue = nval.asInstanceOf[Int]  // nval: Double is inherited from StreamTokenizer
          if (nval == intValue.asInstanceOf[Double]) IntConstant(intValue)
          else throw new ParseException("The number " + nval + " is not a 32 bit integer")
        case WORD =>
          val optToken = wordTable.get(Symbol(sval))
          optToken match {
            case None =>
              // must be new variable name
              val newVar = Variable(Symbol(sval))
              wordTable += (Symbol(sval) -> newVar)
              newVar
            case Some(token) => token
          }
        case EOF =>  null
        case '(' =>  LeftParen
        case ')' =>  RightParen
        case '[' =>  LeftBrack
        case ']' =>  RightBrack
        // case '{' =>  LeftBrace
        // case '}' =>  RightBrace
        case ',' =>  Comma
        case ';' =>  SemiColon
        
        case '+' =>  PLUS
        case '-' =>  MINUS  
        case '*' =>  TIMES 
        case '/' =>  DIVIDE
        case '~' =>  NOT
        case '=' =>  EQUALS
        case '<' =>  
          val nextTokenType = getToken()
          if (nextTokenType == '=') LESS_THAN_EQUALS 
          // else if (tokenType eq '-') GETS  
          else {
            pushBack()
            LESS_THAN
          }
        case '>' =>  
          val nextTokenType = getToken()
          if (nextTokenType == '=') GREATER_THAN_EQUALS
          else {
            pushBack()
            GREATER_THAN
          }
        case '!' =>  
          val nextTokenType = getToken()
          if (nextTokenType == '=') NOT_EQUALS 
          else throw new ParseException("!" + (nextTokenType.asInstanceOf[Char]) + " is not a legal token")
//        else { // this alternate else clause will be used in later assignments
//         pushBack()
//         BANG
//        }
        
        case '&' =>  AND  
        case '|' =>  OR 
        case ':' =>  {
          val nextTokenType = getToken()
          if (nextTokenType ==  '=') BIND
          else {
            pushBack()
            throw new ParseException("`:' is not a legalken")
          }
        }
        case _ =>  /* default */
          throw new  ParseException("`" + (tokenType.asInstanceOf[Char]) + "' is not a legal token")
      }
    }
  }
  
  /** Initializes the table of Strings used to recognize Tokens */
  def initWordTable() {
    
    // constants
    // <empty>  ::= empty
    // <bool>  ::= true | false
    
    wordTable.put(Symbol("null"), EmptyConstant)
    wordTable.put(Symbol("true"),  True)
    wordTable.put(Symbol("false"), False)
    
    // install opeators that are words
//    wordTable.put("ref", new OpToken("ref",true,false)
    
    // keywords: if then else let in map to := 
    wordTable.put(Symbol("if"),   IF)
    wordTable.put(Symbol("then"), THEN)
    wordTable.put(Symbol("else"), ELSE)
    wordTable.put(Symbol("let"),  LET)
    wordTable.put(Symbol("in"),   IN)
    wordTable.put(Symbol("map"),  MAP)
    wordTable.put(Symbol("to"),   TO)
    wordTable.put(Symbol(":="),   BIND)
    
    // Install primitive functions
    // <prim>  ::= number? | function? | list? | empty? 
    //           | cons? | cons | first | rest | arity
    
    wordTable.put(Symbol("number?"),   NumberPPrim)
    wordTable.put(Symbol("function?"), FunctionPPrim)
//    wordTable.put(Symbol("ref?"),    RefPPrim)
    wordTable.put(Symbol("list?"),     ListPPrim)
    wordTable.put(Symbol("null?"),    EmptyPPrim)
    wordTable.put(Symbol("cons?"),     ConsPPrim)
    wordTable.put(Symbol("arity"),     ArityPrim)
    wordTable.put(Symbol("cons"),      ConsPrim)
    wordTable.put(Symbol("first"),     FirstPrim)
    wordTable.put(Symbol("rest"),      RestPrim)
  }
  
  /* Body of Lexer class; intializes lexer state */
  initLexer()
}
