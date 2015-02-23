/**
 * Created by ziliangzhu on 2/23/15.
 */
abstract class Tuple {
  def getJamVal: JamVal
  def getAST: AST
}

class ValueTuple(jamVal: JamVal, ast: AST) extends Tuple{
  override def getJamVal: JamVal = jamVal
  override def getAST: AST = ast
}

class NameTuple(evaluate: (AST, Map[Symbol, NameTuple]) => JamVal, notUntil: (AST, Map[Symbol, NameTuple]) => AST, env: Map[Symbol, NameTuple], rawVar: AST) extends Tuple{
  override def getJamVal: JamVal = evaluate(rawVar, env)
  override def getAST: AST = notUntil(rawVar, env)
}

class NeedTuple(evaluate: (AST, Map[Symbol, NeedTuple]) => JamVal, notUntil: (AST, Map[Symbol, NeedTuple]) => AST, env: Map[Symbol, NeedTuple], rawVar: AST) extends Tuple{
  lazy val jamVal = evaluate(rawVar, env)
  lazy val lazyAst = notUntil(rawVar, env)
  override def getJamVal: JamVal = jamVal
  override def getAST: AST = lazyAst
}