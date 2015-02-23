/**
 * Created by ziliangzhu on 2/23/15.
 */
trait Env[Tp] {
  def setMap(m: Map[Symbol, Tp])
  def get(s: Symbol)
}

class ConcreteEnv[Tp](val map: Map[Symbol, Tp]) extends Env[Tp] {
  def setMap(m: Map[Symbol, Tp]): Unit = throw new UnsupportedOperationException("ConcreteEnv.setMap")
  def get(s: Symbol): Tp = map(s)
}

class ProxyEnv[Tp] extends Env[Tp] {
  var map: Option[Map[Symbol, Tp]] = None
  def setMap(m: Map[Symbol, Tp]) = {map = Some(m)}
  def get(s: Symbol) = map match {
    case Some(ma) => ma(s)
    case None => throw new UnsupportedOperationException("ProxyEnv.get called before map is set")
  }
}