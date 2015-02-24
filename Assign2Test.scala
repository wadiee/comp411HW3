import java.io.StringReader
import junit.framework.TestCase
import junit.framework.Assert._

/**
 * Created by Wade on 2/6/2015.
 */
class Assign2Test extends TestCase{

  def testConsFalse() = {
    assert(new Interpreter(new StringReader("cons(1,cons(2,null))=cons(1,cons(3,null))")).valueValue.toString.equals("false"), "Should not equal")
  }

  def testBinAdd() = {
    assert(new Interpreter(new StringReader("1+2")).valueValue.toString.equals("3"), "1+2 should equal to 3")
  }

  def testBinCmp() = {
    assert(new Interpreter(new StringReader("5 = 5")).valueValue.toString.equals("true"), "Should equal")
  }

  def testBinCmpFalse() = {
    assert(new Interpreter(new StringReader("5 = 7")).valueValue.toString.equals("false"), "Should not equal")
  }

  def testConsTrue() = {
    assert(new Interpreter(new StringReader("cons(1,cons(2,null))=cons(1,cons(2,null))")).valueValue.toString.equals("true"), "cons should equal cons")
  }

  def testBinConsFalse() = {
    assert(new Interpreter(new StringReader("cons(1,cons(2,null))=cons(1,null)")).valueValue.toString.equals("false"))
  }

  def testBinPrimFunTrue() = {
    assert(new Interpreter(new StringReader("cons? = cons?")).valueValue.toString.equals("true"))
  }

  def testBinPrimFunFalse() = {
    assert(new Interpreter(new StringReader("cons? = null?")).valueValue.toString.equals("false"))
  }

  def testBinPrimMapFalse() = {
    assert(new Interpreter(new StringReader("cons? = (map x to x)")).valueValue.toString.equals("false"))
  }

  def testBinObjFalse() = {
    assert(new Interpreter(new StringReader("(map x to x) = (map x to x)")).valueValue.toString.equals("false"))
  }

  def testValLet() = {
    assert(new Interpreter(new StringReader("let m:=(map x to x); in m = m")).valueValue.toString.equals("true"))
  }

  def testValLet1() ={
    assert(new Interpreter(new StringReader("let m:= 5; in m+1")).valueValue.toString.equals("6"))
  }

  def testCons() ={
    assert(new Interpreter(new StringReader("cons(1, cons(2, cons(3, null)))")).valueValue.toString.equals("(1 2 3)"))
  }

  def testmid1 = {
    assert(new Interpreter(new StringReader("let x:=2; in let y:=-x; in (map t to t * y)(100)")).valueValue.toString.equals("-200"))
  }

  def testhard1() = {
    assert(new Interpreter(new StringReader("let Y    := map f to\n      let g := map x to f(map z to (x(x))(z));\n    in g(g);\n    FACT := map f to\n    map n to if n = 0 then 1 else n * f(n - 1);\n    in (Y(FACT))(3)")).valueValue.toString.equals("6"))
  }

  def testhard2() = {
    assert(new Interpreter(new StringReader("let Y    := map f to \n              let g := map x to f(map z1,z2 to (x(x))(z1,z2));\n\t    in g(g);\n    APPEND := map ap to \n\t        map x,y to \n                  if x = null then y else cons(first(x), ap(rest(x), y));\n    l      := cons(1,cons(2,cons(3,null)));\t\nin (Y(APPEND))(l,l)")).valueValue.toString.equals("(1 2 3 1 2 3)"))
  }

  def testMap() = {
    assert(new Interpreter(new StringReader("(map x, y to x + y)(3, 2)")).valueValue.toString.equals("5"))
  }

//==============================Call by name====================================
  def testName() = {
    assert(new Interpreter(new StringReader("let m:=(map x to x); in m = m")).nameValue.toString.equals("false"))
  }

  def testhard3() = {
    assert(new Interpreter(new StringReader("let Y    := map f to\n      let g := map x to f(map z to (x(x))(z));\n    in g(g);\n    FACT := map f to\n    map n to if n = 0 then 1 else n * f(n - 1);\n    in (Y(FACT))(3)")).nameValue.toString.equals("6"))
  }

  def testhard4() = {
    assert(new Interpreter(new StringReader("let Y    := map f to \n              let g := map x to f(map z1,z2 to (x(x))(z1,z2));\n\t    in g(g);\n    APPEND := map ap to \n\t        map x,y to \n                  if x = null then y else cons(first(x), ap(rest(x), y));\n    l      := cons(1,cons(2,cons(3,null)));\t\nin (Y(APPEND))(l,l)")).nameValue.toString.equals("(1 2 3 1 2 3)"))
  }
//============================Call by need
  def testNeed() = {
    assert(new Interpreter(new StringReader("let m:=(map x to x); in m = m")).needValue.toString.equals("true"))
  }

  def testhard5() = {
    assert(new Interpreter(new StringReader("let Y    := map f to\n      let g := map x to f(map z to (x(x))(z));\n    in g(g);\n    FACT := map f to\n    map n to if n = 0 then 1 else n * f(n - 1);\n    in (Y(FACT))(3)")).needValue.toString.equals("6"))
  }

  def testhard6() = {
    assert(new Interpreter(new StringReader("let Y    := map f to \n              let g := map x to f(map z1,z2 to (x(x))(z1,z2));\n\t    in g(g);\n    APPEND := map ap to \n\t        map x,y to \n                  if x = null then y else cons(first(x), ap(rest(x), y));\n    l      := cons(1,cons(2,cons(3,null)));\t\nin (Y(APPEND))(l,l)")).needValue.toString.equals("(1 2 3 1 2 3)"))
  }
  // Test All
  def valueValueCheck(answer: String, program: String) = {
    val interp = new Interpreter(new StringReader(program))
    val ge = interp.valueValue.toString
    assertEquals(answer, ge)
  }

  def nameValueCheck(answer: String, program: String) = {
    val interp = new Interpreter(new StringReader(program))
    val ge = interp.nameValue.toString
    assertEquals(answer, ge)
  }

  def needValueCheck(answer: String, program: String) = {
    val interp = new Interpreter(new StringReader(program))
    val ge = interp.needValue.toString
    assertEquals(answer, ge)
  }

  def valueNameCheck(answer: String, program: String) = {
    val interp = new Interpreter(new StringReader(program))
    val ge = interp.valueName.toString
    assertEquals(answer, ge)
  }

  def nameNameCheck(answer: String, program: String) = {
    val interp = new Interpreter(new StringReader(program))
    val ge = interp.nameName.toString
    assertEquals(answer, ge)
  }

  def needNameCheck(answer: String, program: String) = {
    val interp = new Interpreter(new StringReader(program))
    val ge = interp.needName.toString
    assertEquals(answer, ge)
  }

  def valueNeedCheck(answer: String, program: String) = {
    val interp = new Interpreter(new StringReader(program))
    val ge = interp.valueNeed.toString
    assertEquals(answer, ge)
  }

  def nameNeedCheck(answer: String, program: String) = {
    val interp = new Interpreter(new StringReader(program))
    val ge = interp.nameNeed.toString
    assertEquals(answer, ge)
  }

  def needNeedCheck(answer: String, program: String) = {
    val interp = new Interpreter(new StringReader(program))
    val ge = interp.needNeed.toString
    assertEquals(answer, ge)
  }



  def allValueCheck(answer: String, program: String) {
    valueValueCheck(answer, program)
    nameValueCheck(answer, program)
    needValueCheck(answer, program)
  }

  def allNameCheck(answer: String, program: String) {
    valueNameCheck(answer, program)
    nameNameCheck(answer, program)
    needNameCheck(answer, program)
  }

  def allNeedCheck(answer: String, program: String) {
    valueNameCheck(answer, program)
    nameNameCheck(answer, program)
    needNameCheck(answer, program)
  }

  def allCheck(answer: String, program: String){
    allValueCheck(answer, program)
    allNameCheck(answer, program)
    allNeedCheck(answer, program)
  }

  def testNumberP() {
    try {
      val output = "number?"
      val input = "number?"
      allValueCheck(output, input );

    } catch{
      case _: Throwable => fail("numberP threw ");
    }
  } //end of func

  def testMathOP() {
    try {
      val output = "18"
      val input = "2 * 3 + 12"
      allValueCheck(output, input );

    } catch{
      case _: Throwable => fail("MathOP threw ");
    }
  } //end of func

  def testParseException() {
    try {
      val output = "haha"
      val input = " 1 +"
      allValueCheck(output, input)
      fail("parseException did not throw ParseException exception");
    }catch{
      case _: Throwable =>
    }
  } //end of func

  def testEvalException() {
    try {
      val output = "mojo"
      val input = "1 + number?"
      allValueCheck(output, input)
      fail("parseException did not throw ParseException exception");
    }catch{
      case _: Throwable =>
    }
  } //end of func

  // Test primitive functions
  def testOutputCons() {
    try {
      val output = "(1 2)"
      val input = "cons(1, cons(2, null))"
      nameValueCheck(output, input )

    } catch{
      case e: Throwable => {
        e.printStackTrace()
        fail("cons threw")
      }
    }
  } //end of func

  def testOutputEmpty() {
    try {
      val output = "true"
      val input = "null?(null)"

      allValueCheck(output, input );
    } catch{
      case _: Throwable => fail("null? threw ");
    }
  } //end of func

  def testOutputConsP() {
    try {
      val output = "true"
      val input = "cons?(cons(1, null))"

      allValueCheck(output, input );
    } catch{
      case _: Throwable => fail("cons? threw ");
    }
  } //end of func

  def testOutputNumber() {
    try {
      val output = "true"
      val input = "number?(1)"

      allValueCheck(output, input );
    } catch{
      case _: Throwable => fail("number? threw ");
    }
  } //end of func

  def testOutputNumberLet() {
    try {
      val output = "true"
      val input = "let x := 1; \n in number?(x)"

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail(e.getMessage);
    }
  } //end of func

  def testOutputConPPrimLet() {
    try {
      val output = "true"
      val input = "let x := cons(1, null); \n in cons?(x)"

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail(e.getMessage);
    }
  } //end of func

  def testOutputFunction() {
    try {
      val output = "true"
      val input = "function?(number?)"

      allValueCheck(output, input );
    } catch{
      case _: Throwable => fail("function? threw ");
    }
  } //end of func

  def testOutputFunction1() {
    try {
      val output = "true"
      val input = "function?(map x to x)"

      allValueCheck(output, input );
    } catch{
      case _: Throwable => fail("function? threw ");
    }
  } //end of func

  def testOutputArity() {
    try {
      val output = "3"
      val input = "arity(map x, y, z to x)"

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("arity threw " + e.printStackTrace());
    }
  } //end of func

  def testOutputListNull() {
    try {
      val output = "true"
      val input = "list?(null)"

      allValueCheck(output, input );
    } catch{
      case _: Throwable => fail("list? null threw ");
    }
  } //end of func

  def testOutputList() {
    try {
      val output = "true"
      val input = "list?(cons(1, cons(2, null)))"

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("list? threw " + e.printStackTrace());
    }
  } //end of func

  def testOutputFirst() {
    try {
      val output = "1"
      val input = "first(cons(1, cons(2, null)))"

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("first threw " + e.printStackTrace());
    }
  } //end of func

  def testOutputRest() {
    try {
      val output = "(2)"
      val input = "rest(cons(1, cons(2, null)))"

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("rest threw " + e.printStackTrace());
    }
  } //end of func

  // should fail cases

  def testFailCons() {
    try {
      val output = "false"
      val input = "cons?(map x to 1)"

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("failcons threw " + e.printStackTrace());
    }
  } //end of func

  def testFailNull() {
    try {
      val output = "false"
      val input = "null?(3)"

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("failnull threw " + e.printStackTrace());
    }
  } //end of func

  def testFailNumber() {
    try {
      val output = "false"
      val input = "number?(null)"

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("failnumber threw " + e.printStackTrace());
    }
  } //end of func

  def testFailFunction() {
    try {
      val output = "false"
      val input = "function?(null)"

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("failfunction threw " + e.printStackTrace());
    }
  } //end of func

  def testFaillist() {
    try {
      val output = "false"
      val input = "list?(map x to 1)"

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("faillist threw " + e.printStackTrace());
    }
  } //end of func



  // Test Free variable, should get different result for value, name, need calls.

  def testFreeVarValue() {
    try {
      val output = "5"
      val input = "let x := f; \n in 5"

      try{
        valueValueCheck(output, input)
        fail("Did not throw free variable")
      } catch {
        case e: Throwable =>
      }
    } catch{
      case e: Throwable => fail("free variable value call by value");
    }
  } //end of func

  def testFreeVarName() {
    try {
      val output = "5"
      val input = "let x := f; \n in 5"

      nameValueCheck(output, input)
    } catch{
      case e: Throwable => fail("free variable throw for call by name");
    }
  } //end of func

  def testFreeVarNeed() {
    try {
      val output = "5"
      val input = "let x := f; \n in 5"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("free variable throw for call by need");
    }
  } //end of func

  def testUnary() {
    try {
      val output = "5"
      val input = "+5"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("unary throw");
    }
  } //end of func

  def testUnary2() {
    try {
      val output = "-5"
      val input = "-5"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("unary throw");
    }
  } //end of func

  def testUnary3() {
    try {
      val output = "false"
      val input = "~true"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("unary throw");
    }
  } //end of func

  def testUnary4() {
    try {
      val output = "true"
      val input = "~false"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("unary throw");
    }
  } //end of func

  def testBinary1() {
    try {
      val output = "true"
      val input = "5 > 3"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary2() {
    try {
      val output = "false"
      val input = "5 < 3"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary3() {
    try {
      val output = "false"
      val input = "5 <= 3"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary4() {
    try {
      val output = "true"
      val input = "2 <= 3"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary5() {
    try {
      val output = "true"
      val input = "2 != 3"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary6() {
    try {
      val output = "false"
      val input = "3 != 3"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary7() {
    try {
      val output = "5"
      val input = "2 + 3"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary8() {
    try {
      val output = "-1"
      val input = "2 - 3"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary9() {
    try {
      val output = "4"
      val input = "8 / 2"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary10() {
    try {
      val output = "6"
      val input = "2 * 3"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary11() {
    try {
      val output = "true"
      val input = "3 >= 3"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary12() {
    try {
      val output = "false"
      val input = "2 >= 3"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary13() {
    try {
      val output = "true"
      val input = "true & true"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary14() {
    try {
      val output = "false"
      val input = "false & true"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary15() {
    try {
      val output = "true"
      val input = "false | true"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary16() {
    try {
      val output = "true"
      val input = "true | false"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary17() {
    try {
      val output = "false"
      val input = "true & false"

      needValueCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func


// Added testcases for HW 3

  def testPiazza1() {
    try {
      val output = "6"
      val input = "let a:=b; b:=3; in (a+b)"
      allValueCheck(output, input)
      fail("Did not throw for free variable");
    } catch{
      case e: SyntaxException =>  print(e.getMessage + "\n")
    }
  }

  def testBinopFV(){
    try {
      val output = "haha"
      val input = "x + 5"
      allValueCheck(output, input)
      fail("Did not throw for free variable");
    }catch{
      case e: SyntaxException => print(e.getMessage + "\n")
    }
  }

  def testml1() {
    try {
      val output = "JamClosure(map x,y to (x + y),Map())"
      val input = "(map x, y to x + y)"
      allValueCheck(output, input)

    } catch{
      case e: Throwable => fail(e.getMessage);
    }
  } //end of func

  def testml2() {
    try {
      val output = "JamClosure(map x,x to (x + x),Map())"
      val input = "(map x, x to x + x)"
      allValueCheck(output, input)
      fail("Did not throw for repeated variables");
    } catch{
      case e: SyntaxException => print(e.getMessage + "\n")
    }
  } //end of func

  def testlet1() {
    try {
      val output = "2"
      val input = "let x:=1; y := x +1; in y"
      allValueCheck(output, input)

    } catch{
      case e: Throwable => fail("throw" + e.printStackTrace());
    }
  } //end of func

//  def testlet2() {
//    try {
//      val output = "2"
//      val input = "let y := x +1; x:=1; in y"
//      nameCheck(output, input)
//      needCheck(output, input)
//
//    } catch{
//      case e: Throwable => fail("throw" + e.printStackTrace());
//    }
//  } //end of func

  def testmapApp() {
    try {
      val output = "3"
      val input = "(map x, y to x + y)(1,2)"
      allValueCheck(output, input)
    } catch{
      case e: Throwable => fail("throw" + e.printStackTrace());
    }
  } //end of func

  def testmapApp2() {
    try {
      val output = ""
      val input = "(map x, x to x + x)(1,3)"
      allValueCheck(output, input)
      fail("Did not throw for repeated variables");
    } catch{
      case e: SyntaxException => print(e.getMessage + "\n")
    }
  } //end of func

  def testmapApp3() {
    try {
      val output = "3"
      val input = "(map x, y to x + y)(1,x+1)"
      allValueCheck(output, input)
    } catch{
      case e: Throwable => fail("throw" + e.printStackTrace());
    }
  } //end of func

  def testAnd(){
    try {
      val output = "true"
      val input = "(3 = 3) & (4 = 4)"
      allValueCheck(output, input)
    } catch{
      case e: Throwable => fail("throw" + e.printStackTrace());
    }
  }

  def testAnd2() {
    try {
      val output = ""
      val input = "(3 = 3) & 4"
      allValueCheck(output, input)
      fail("Did not throw for binary And without boolean");
    } catch{
      case e: EvalException => print(e.getMessage + "\n")
    }
  } //end of func

  def testOr(){
    try {
      val output = "true"
      val input = "(3 = 3) | x"
      allValueCheck(output, input)
    } catch{
      case e: Throwable => fail("throw" + e.printStackTrace());
    }
  }

  def testOr2() {
    try {
      val output = ""
      val input = "(3 = 2) | 4"
      allValueCheck(output, input)
      fail("Did not throw for binary Or without boolean");
    } catch{
      case e: EvalException => print(e.getMessage + "\n")
    }
  } //end of func

//  def testReclet() {
//    try {
//      val output = "true"
//      val input = "let is_even := map x to if x = 0 then true else is_odd(x-1);\n     is_odd := map x to if x = 0 then false else is_even(x-1);\nin is_even(42)"
//      allCheck(output, input)
//    } catch {
//      case e: Throwable => fail("throw" + e.printStackTrace());
//    }
//  }


  def testValueCons() {
    try {
      val output = "1"
      val input = "first(cons(1, x))"
      allValueCheck(output, input)
      fail("not throw cons with free var")
    } catch{
      case e: SyntaxException => print(e.getMessage + "\n")
    }
  } //end of func

  def testValueCons2() {
  try {
    val output = "1"
    val input = "first(cons(1, cons(2, x)))"
    allValueCheck(output, input)
    fail("not throw cons with free var")
  } catch{
    case e: Throwable => print(e.getMessage + "\n")
  }
} //end of func

  def testValueCons3() {
    try {
      val output = "1"
      val input = "rest(cons(1, cons(2, x)))"
      allValueCheck(output, input)
      fail("not throw cons with free var")
    } catch{
      case e: Throwable => print(e.getMessage + "\n")
    }
  } //end of func

  def testValueCons4() {
  try {
    val output = "1"
    val input = "rest(cons(1, x))"
    allValueCheck(output, input)
    fail("not throw cons with free var")
  } catch{
    case e: Throwable => print(e.getMessage + "\n")
  }
} //end of func

  def testNameCons() {
    try {
      val output = "1"
      val input = "first(cons(1, x))"
      allNameCheck(output, input)
    } catch{
      case e: Throwable => fail(e.getMessage)
    }
  } //end of func

  def testNameCons2() {
    try {
      val output = "1"
      val input = "rest(cons(1, x))"
      allNameCheck(output, input)
      fail("not throw cons with free var")
    } catch{
      case e: Throwable => print(e.getMessage + "\n")
    }
  } //end of func

  def testNeedCons() {
    try {
      val output = "1"
      val input = "first(cons(1, x))"
      allNeedCheck(output, input)
    } catch{
      case e: Throwable => fail(e.getMessage)
    }
  } //end of func

  def testNeedCons2() {
    try {
      val output = "1"
      val input = "rest(cons(1, x))"
      allNeedCheck(output, input)
      fail("not throw cons with free var")
    } catch{
      case e: Throwable => print(e.getMessage + "\n")
    }
  } //end of func

  def testCons2() {
    try {
      val output = "2"
      val input = "first(rest(cons(1, cons(2, x))))"
      allNeedCheck(output, input)
    } catch{
      case e: Throwable => fail(e.getMessage)
    }
  } //end of func

  def testCons3() {
    try {
      val output = "2"
      val input = "first(rest(cons(1, cons(2, x))))"
      allNameCheck(output, input)
    } catch{
      case e: Throwable => fail(e.getMessage)
    }
  } //end of func

  def testCons4() {
    try {
      val output = "2"
      val input = "first(rest(cons(1, cons(2, x))))"
      allValueCheck(output, input)
      fail("not throw cons with free var")
    } catch{
      case e: Throwable => print(e.getMessage + "\n")
    }
  } //end of func

  def testRecursive(): Unit = {
    try {
      val output = "true"
      val input = "let is_even := map x to if x = 0 then true else is_odd(x-1);\n     is_odd := map x to if x = 0 then false else is_even(x-1);\nin is_even(42)"
      nameValueCheck(output, input)
      nameNameCheck(output, input)
      nameNeedCheck(output, input)
      needValueCheck(output, input)
      needNameCheck(output, input)
      needNeedCheck(output, input)
    } catch {
      case e: Throwable => fail(e.getMessage)
    }
  }
}