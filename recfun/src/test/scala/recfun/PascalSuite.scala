package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }
  
  test("pascal: col=0,row=0") {
	  assert(pascal(0,0) === 1)
  }
  
  test("pascal: col > row throws IllegalArgumentException") {
    intercept[IllegalArgumentException] {
      pascal(3,2);
    }
  }
  	
  test("pascal: row == col") {
    assert(pascal(1,1) === 1)
    assert(pascal(2,2) === 1)
    assert(pascal(3,3) === 1)
    assert(pascal(4,4) === 1)
  }
  
  test("pascal: col=2,row=5") {
    assert(pascal(2,5) === 10)
  }
  
}