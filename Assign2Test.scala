import java.io.StringReader
import jdk.nashorn.internal.runtime.regexp.joni.exception.SyntaxException
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
  def valueCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    var ge = interp.valueValue.toString()
    assertEquals(answer, ge)
  }

  def nameCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    var ge = interp.nameValue.toString()
    assertEquals(answer, ge)
  }

  def needCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    var ge = interp.needValue.toString()
    assertEquals(answer, ge)
  }

  def valueNameCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    var ge = interp.valueName.toString()
    assertEquals(answer, ge)
  }

  def nameNameCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    var ge = interp.nameName.toString()
    assertEquals(answer, ge)
  }

  def needNameCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    var ge = interp.needName.toString()
    assertEquals(answer, ge)
  }

  def valueNeedCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    var ge = interp.valueNeed.toString()
    assertEquals(answer, ge)
  }

  def nameNeedCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    var ge = interp.nameNeed.toString()
    assertEquals(answer, ge)
  }

  def needNeedCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    var ge = interp.needNeed.toString()
    assertEquals(answer, ge)
  }
  
  

  def allValueCheck(answer: String, program: String) {
    valueCheck(answer, program);
    nameCheck(answer, program);
    needCheck(answer, program);
  }

  def allNameCheck(answer: String, program: String) {
    valueNameCheck(answer, program);
    nameNameCheck(answer, program);
    needNameCheck(answer, program);
  }

  def allNeedCheck(answer: String, program: String) {
    valueNameCheck(answer, program);
    nameNameCheck(answer, program);
    needNameCheck(answer, program);
  }

  def allCheck(answer: String, program: String){
    allValueCheck(answer, program)
    allNameCheck(answer, program)
    allNeedCheck(answer, program)
  }

  def testNumberP() {
    try {
      var output = "number?";
      var input = "number?";
      allValueCheck(output, input );

    } catch{
      case _: Throwable => fail("numberP threw ");
    }
  } //end of func

  def testMathOP() {
    try {
      var output = "18";
      var input = "2 * 3 + 12";
      allValueCheck(output, input );

    } catch{
      case _: Throwable => fail("MathOP threw ");
    }
  } //end of func

  def testParseException() {
    try {
      var output = "haha";
      var input = " 1 +"
      allValueCheck(output, input);
      fail("parseException did not throw ParseException exception");
    }catch{
      case _: Throwable =>
    }
  } //end of func

  def testEvalException() {
    try {
      var output = "mojo";
      var input = "1 + number?"
      allValueCheck(output, input);
      fail("parseException did not throw ParseException exception");
    }catch{
      case _: Throwable =>
    }
  } //end of func

  // Test primitive functions
  def testOutputCons() {
    try {
      var output = "(1 2)";
      var input = "cons(1, cons(2, null))";
      nameCheck(output, input );

    } catch{
      case e: Throwable => {
        e.printStackTrace()
        fail("cons threw")
      }
    }
  } //end of func

  def testOutputEmpty() {
    try {
      var output = "true";
      var input = "null?(null)";

      allValueCheck(output, input );
    } catch{
      case _: Throwable => fail("null? threw ");
    }
  } //end of func

  def testOutputConsP() {
    try {
      var output = "true";
      var input = "cons?(cons(1, null))";

      allValueCheck(output, input );
    } catch{
      case _: Throwable => fail("cons? threw ");
    }
  } //end of func

  def testOutputNumber() {
    try {
      var output = "true";
      var input = "number?(1)";

      allValueCheck(output, input );
    } catch{
      case _: Throwable => fail("number? threw ");
    }
  } //end of func

  def testOutputNumberLet() {
    try {
      var output = "true";
      var input = "let x := 1; \n in number?(x)";

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail(e.getMessage);
    }
  } //end of func

  def testOutputConPPrimLet() {
    try {
      var output = "true";
      var input = "let x := cons(1, null); \n in cons?(x)";

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail(e.getMessage);
    }
  } //end of func

  def testOutputFunction() {
    try {
      var output = "true";
      var input = "function?(number?)";

      allValueCheck(output, input );
    } catch{
      case _: Throwable => fail("function? threw ");
    }
  } //end of func

  def testOutputFunction1() {
    try {
      var output = "true";
      var input = "function?(map x to x)";

      allValueCheck(output, input );
    } catch{
      case _: Throwable => fail("function? threw ");
    }
  } //end of func

  def testOutputArity() {
    try {
      var output = "3";
      var input = "arity(map x, y, z to x)";

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("arity threw " + e.printStackTrace());
    }
  } //end of func

  def testOutputListNull() {
    try {
      var output = "true";
      var input = "list?(null)";

      allValueCheck(output, input );
    } catch{
      case _: Throwable => fail("list? null threw ");
    }
  } //end of func

  def testOutputList() {
    try {
      var output = "true";
      var input = "list?(cons(1, cons(2, null)))";

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("list? threw " + e.printStackTrace());
    }
  } //end of func

  def testOutputFirst() {
    try {
      var output = "1";
      var input = "first(cons(1, cons(2, null)))";

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("first threw " + e.printStackTrace());
    }
  } //end of func

  def testOutputRest() {
    try {
      var output = "(2)";
      var input = "rest(cons(1, cons(2, null)))";

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("rest threw " + e.printStackTrace());
    }
  } //end of func

  // should fail cases

  def testFailCons() {
    try {
      var output = "false";
      var input = "cons?(map x to 1)";

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("failcons threw " + e.printStackTrace());
    }
  } //end of func

  def testFailNull() {
    try {
      var output = "false";
      var input = "null?(3)";

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("failnull threw " + e.printStackTrace());
    }
  } //end of func

  def testFailNumber() {
    try {
      var output = "false";
      var input = "number?(null)";

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("failnumber threw " + e.printStackTrace());
    }
  } //end of func

  def testFailFunction() {
    try {
      var output = "false";
      var input = "function?(null)";

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("failfunction threw " + e.printStackTrace());
    }
  } //end of func

  def testFaillist() {
    try {
      var output = "false";
      var input = "list?(map x to 1)";

      allValueCheck(output, input );
    } catch{
      case e: Throwable => fail("faillist threw " + e.printStackTrace());
    }
  } //end of func



  // Test Free variable, should get different result for value, name, need calls.

  def testFreeVarValue() {
    try {
      var output = "5";
      var input = "let x := f; \n in 5";

      try{
        valueCheck(output, input)
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
      var output = "5";
      var input = "let x := f; \n in 5";

      nameCheck(output, input)
    } catch{
      case e: Throwable => fail("free variable throw for call by name");
    }
  } //end of func

  def testFreeVarNeed() {
    try {
      var output = "5";
      var input = "let x := f; \n in 5";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("free variable throw for call by need");
    }
  } //end of func

  def testUnary() {
    try {
      var output = "5";
      var input = "+5";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("unary throw");
    }
  } //end of func

  def testUnary2() {
    try {
      var output = "-5";
      var input = "-5";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("unary throw");
    }
  } //end of func

  def testUnary3() {
    try {
      var output = "false";
      var input = "~true";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("unary throw");
    }
  } //end of func

  def testUnary4() {
    try {
      var output = "true";
      var input = "~false";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("unary throw");
    }
  } //end of func

  def testBinary1() {
    try {
      var output = "true";
      var input = "5 > 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary2() {
    try {
      var output = "false";
      var input = "5 < 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary3() {
    try {
      var output = "false";
      var input = "5 <= 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary4() {
    try {
      var output = "true";
      var input = "2 <= 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary5() {
    try {
      var output = "true";
      var input = "2 != 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary6() {
    try {
      var output = "false";
      var input = "3 != 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary7() {
    try {
      var output = "5";
      var input = "2 + 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary8() {
    try {
      var output = "-1";
      var input = "2 - 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary9() {
    try {
      var output = "4";
      var input = "8 / 2";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary10() {
    try {
      var output = "6";
      var input = "2 * 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary11() {
    try {
      var output = "true";
      var input = "3 >= 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary12() {
    try {
      var output = "false";
      var input = "2 >= 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary13() {
    try {
      var output = "true";
      var input = "true & true";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary14() {
    try {
      var output = "false";
      var input = "false & true";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary15() {
    try {
      var output = "true";
      var input = "false | true";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary16() {
    try {
      var output = "true";
      var input = "true | false";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary17() {
    try {
      var output = "false";
      var input = "true & false";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func


// Added testcases for HW 3

  def testPiazza1() {
    try {
      var output = "6";
      var input = "let a:=b; b:=3; in (a+b)";
      allValueCheck(output, input)
      fail("Did not throw for free variable");
    } catch{
      case e: SyntaxException =>  print(e.getMessage + "\n")
    }
  }

  def testBinopFV(){
    try {
      var output = "haha";
      var input = "x + 5"
      allValueCheck(output, input);
      fail("Did not throw for free variable");
    }catch{
      case e: SyntaxException => print(e.getMessage + "\n")
    }
  }

  def testml1() {
    try {
      var output = "JamClosure(map x,y to (x + y),Map())";
      var input = "(map x, y to x + y)";
      allValueCheck(output, input)

    } catch{
      case e: Throwable => fail("throw" + e.printStackTrace());
    }
  } //end of func

  def testml2() {
    try {
      var output = "JamClosure(map x,x to (x + x),Map())";
      var input = "(map x, x to x + x)";
      allValueCheck(output, input)
      fail("Did not throw for repeated variables");
    } catch{
      case e: SyntaxException => print(e.getMessage + "\n")
    }
  } //end of func

  def testlet1() {
    try {
      var output = "2";
      var input = "let x:=1; y := x +1; in y";
      allValueCheck(output, input)

    } catch{
      case e: Throwable => fail("throw" + e.printStackTrace());
    }
  } //end of func

//  def testlet2() {
//    try {
//      var output = "2";
//      var input = "let y := x +1; x:=1; in y";
//      nameCheck(output, input)
//      needCheck(output, input)
//
//    } catch{
//      case e: Throwable => fail("throw" + e.printStackTrace());
//    }
//  } //end of func

  def testmapApp() {
    try {
      var output = "3";
      var input = "(map x, y to x + y)(1,2)";
      allValueCheck(output, input)
    } catch{
      case e: Throwable => fail("throw" + e.printStackTrace());
    }
  } //end of func

  def testmapApp2() {
    try {
      var output = "";
      var input = "(map x, x to x + x)(1,3)";
      allValueCheck(output, input)
      fail("Did not throw for repeated variables");
    } catch{
      case e: SyntaxException => print(e.getMessage + "\n")
    }
  } //end of func

  def testmapApp3() {
    try {
      var output = "3";
      var input = "(map x, y to x + y)(1,x+1)";
      allValueCheck(output, input)
    } catch{
      case e: Throwable => fail("throw" + e.printStackTrace());
    }
  } //end of func

  def testAnd(){
    try {
      var output = "true";
      var input = "(3 = 3) & (4 = 4)";
      allValueCheck(output, input)
    } catch{
      case e: Throwable => fail("throw" + e.printStackTrace());
    }
  }

  def testAnd2() {
    try {
      var output = "";
      var input = "(3 = 3) & 4";
      allValueCheck(output, input)
      fail("Did not throw for binary And without boolean");
    } catch{
      case e: EvalException => print(e.getMessage + "\n")
    }
  } //end of func

  def testOr(){
    try {
      var output = "true";
      var input = "(3 = 3) | x";
      allValueCheck(output, input)
    } catch{
      case e: Throwable => fail("throw" + e.printStackTrace());
    }
  }

  def testOr2() {
    try {
      var output = "";
      var input = "(3 = 2) | 4";
      allValueCheck(output, input)
      fail("Did not throw for binary Or without boolean");
    } catch{
      case e: EvalException => print(e.getMessage + "\n")
    }
  } //end of func

//  def testReclet() {
//    try {
//      var output = "true";
//      var input = "let is_even := map x to if x = 0 then true else is_odd(x-1);\n     is_odd := map x to if x = 0 then false else is_even(x-1);\nin is_even(42)";
//      allCheck(output, input)
//    } catch {
//      case e: Throwable => fail("throw" + e.printStackTrace());
//    }
//  }


  def testValueCons() {
    try {
      var output = "1";
      var input = "first(cons(1, x))";
      allValueCheck(output, input)
      fail("not throw cons with free var")
    } catch{
      case e: SyntaxException => print(e.getMessage + "\n")
    }
  } //end of func

  def testValueCons2() {
  try {
    var output = "1";
    var input = "first(cons(1, cons(2, x)))";
    allValueCheck(output, input)
    fail("not throw cons with free var")
  } catch{
    case e: Throwable => print(e.getMessage + "\n")
  }
} //end of func

  def testValueCons3() {
    try {
      var output = "1";
      var input = "rest(cons(1, cons(2, x)))";
      allValueCheck(output, input)
      fail("not throw cons with free var")
    } catch{
      case e: Throwable => print(e.getMessage + "\n")
    }
  } //end of func

  def testValueCons4() {
  try {
    var output = "1";
    var input = "rest(cons(1, x))";
    allValueCheck(output, input)
    fail("not throw cons with free var")
  } catch{
    case e: Throwable => print(e.getMessage + "\n")
  }
} //end of func

  def testNameCons() {
    try {
      var output = "1";
      var input = "first(cons(1, x))";
      allNameCheck(output, input)
    } catch{
      case e: Throwable => fail(e.getMessage)
    }
  } //end of func

  def testNameCons2() {
    try {
      var output = "1";
      var input = "rest(cons(1, x))";
      allNameCheck(output, input)
      fail("not throw cons with free var")
    } catch{
      case e: Throwable => print(e.getMessage + "\n")
    }
  } //end of func

  def testNeedCons() {
    try {
      var output = "1";
      var input = "first(cons(1, x))";
      allNeedCheck(output, input)
    } catch{
      case e: Throwable => fail(e.getMessage)
    }
  } //end of func

  def testNeedCons2() {
    try {
      var output = "1";
      var input = "rest(cons(1, x))";
      allNeedCheck(output, input)
      fail("not throw cons with free var")
    } catch{
      case e: Throwable => print(e.getMessage + "\n")
    }
  } //end of func

  def testCons2() {
    try {
      var output = "2";
      var input = "first(rest(cons(1, cons(2, x))))";
      allNeedCheck(output, input)
    } catch{
      case e: Throwable => fail(e.getMessage)
    }
  } //end of func

  def testCons3() {
    try {
      var output = "2";
      var input = "first(rest(cons(1, cons(2, x))))";
      allNameCheck(output, input)
    } catch{
      case e: Throwable => fail(e.getMessage)
    }
  } //end of func

  def testCons4() {
    try {
      var output = "2";
      var input = "first(rest(cons(1, cons(2, x))))";
      allValueCheck(output, input)
      fail("not throw cons with free var")
    } catch{
      case e: Throwable => print(e.getMessage + "\n")
    }
  } //end of func
}