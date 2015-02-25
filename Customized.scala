/**
 * Created by ziliangzhu on 2/23/15.
 */
class EvalException(msg: String) extends RuntimeException(msg)
class SyntaxException(msg: String) extends RuntimeException(msg)

/**
 * Created by ziliangzhu on 2/23/15.
 */
trait Env[Tp] {
  def setMap(m: Map[Symbol, Tp]): Unit
  def get(s: Symbol): Tp
  def + (pair: (Symbol, Tp)): Env[Tp]
  def contains(s: Symbol): Boolean
  def getMap: Map[Symbol, Tp]
}

class ConcreteEnv[Tp](val map: Map[Symbol, Tp]) extends Env[Tp] {
  def setMap(m: Map[Symbol, Tp]): Unit = throw new UnsupportedOperationException("ConcreteEnv.setMap")
  def get(s: Symbol): Tp = map(s)
  def + (pair: (Symbol, Tp)): Env[Tp] = new ConcreteEnv[Tp](map + pair)
  def contains(s: Symbol): Boolean = map.contains(s)
  def getMap: Map[Symbol, Tp] = map
}

class ProxyEnv[Tp] extends Env[Tp] {
  var map: Option[Map[Symbol, Tp]] = None
  def setMap(m: Map[Symbol, Tp]) = {map = Some(m)}
  def get(s: Symbol): Tp = map match {
    case Some(ma) => ma(s)
    case None => throw new UnsupportedOperationException("ProxyEnv.get called before map is set")
  }
  def + (pair: (Symbol, Tp)): Env[Tp] = map match {
    case Some(ma) => new ConcreteEnv[Tp](ma + pair)
    case None => throw new UnsupportedOperationException("ProxyEnv.+ called before map is set")
  }
  def contains(s: Symbol): Boolean = map match {
    case Some(ma) => ma.contains(s)
    case None => throw new UnsupportedOperationException("ProxyEnv.contains called before map is set")
  }
  def getMap: Map[Symbol, Tp] = map match {
    case Some(ma) => ma
    case None => throw new UnsupportedOperationException("ProxyEnv.getMap called before map is set")
  }
}

/**
 * Created by ziliangzhu on 2/23/15.
 */
trait Tuple {
  def getJamVal: JamVal
  def getAST: AST
}

class ValueTuple(evaluate: (AST, Env[ValueTuple]) => JamVal, notUntil: (AST, Env[ValueTuple]) => AST, env: Env[ValueTuple], rawVar: AST) extends Tuple{
  val jamVal = evaluate(rawVar, env)
  val ast = notUntil(rawVar, env)
  val x = 1
  override def getJamVal: JamVal = jamVal
  override def getAST: AST = ast
}

class NameTuple(evaluate: (AST, Env[NameTuple]) => JamVal, notUntil: (AST, Env[NameTuple]) => AST, env: Env[NameTuple], rawVar: AST) extends Tuple{
  override def getJamVal: JamVal = evaluate(rawVar, env)
  override def getAST: AST = notUntil(rawVar, env)
}

class NeedTuple(evaluate: (AST, Env[NeedTuple]) => JamVal, notUntil: (AST, Env[NeedTuple]) => AST, env: Env[NeedTuple], rawVar: AST) extends Tuple{
  lazy val jamVal = evaluate(rawVar, env)
  lazy val lazyAst = notUntil(rawVar, env)
  override def getJamVal: JamVal = jamVal
  override def getAST: AST = lazyAst
}