package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersect contains only common elements") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(s2, s3)
      val u3 = union(s1, s3)
      assert(contains(intersect(u1, u2), 2), "Intersect 1")
      assert(contains(intersect(u2, u3), 3), "Intersect 2")
      assert(!contains(intersect(u1, u3), 2), "Intersect 3")
    }
  }
  
  test("diff contains elements in first set but not in second set") {
    new TestSets {
      val u1 = union(s1, s2)
      val u2 = union(s2, s3)
      val u3 = union(s1, s3)
      val d1 = diff(u1, u2)
      val d2 = diff(u2, u3)
      assert(contains(d1, 1), "Diff 1")
      assert(!contains(d1, 2), "Diff 2")
      assert(!contains(d1, 3), "Diff 3")
      assert(!contains(d2, 1), "Diff 4")
      assert(contains(d2, 2), "Diff 5")
      assert(!contains(d2, 3), "Diff 5")
      assert(!contains(d2, 10), "NotInOriginalSets")
    }
  }
  
  test("filter only contains elements indicated by predicate") {
    new TestSets {
      val s = union(s1, union(s2, union(s3, union(s4, union(s5, s6)))))
      def odd(x: Int): Boolean = x%2 == 1
      def even(x: Int): Boolean = x%2 == 0
      def multipleOf3(x: Int): Boolean = x%3 == 0
      assert(contains(filter(s, odd), 1), "Odd 1") 
      assert(!contains(filter(s, odd), 4), "Odd 2") 
      assert(contains(filter(s, even), 2), "Even 1") 
      assert(!contains(filter(s, even), 5), "Even 2") 
      assert(!contains(filter(s, multipleOf3), 2), "MultipleOf3 1") 
      assert(contains(filter(s, multipleOf3), 6), "MultipleOf3 2") 
      assert(!contains(filter(s, odd), 14), "NotInOriginalSets") 
    }
  }

  test("forall checks predicate for all elements of set") {
    new TestSets {
      val odds = union(s1, union(s3, s5))
      val evens = union(s2, union(s4, s6))
      val multiplesOf3 = union(s3, s6)
      def odd(x: Int): Boolean = x%2 == 1
      def even(x: Int): Boolean = x%2 == 0
      def multipleOf3(x: Int): Boolean = x%3 == 0
      assert(forall(odds, odd), "Odds")
      assert(!forall(odds, even), "Not odds 1");
      assert(!forall(odds, multiplesOf3), "Not odds 2");
      assert(forall(evens, even), "Evens")
      assert(!forall(evens, odd), "Not evens")
      assert(forall(multiplesOf3, multipleOf3), "Multiples of 3")
      assert(!forall(multiplesOf3, odds), "Not multiples of 3")
    }
  }

  test("exists checks predicate for at least one") {
    new TestSets {
      val odds = union(s1, union(s3, s5))
      val evens = union(s2, union(s4, s6))
      val s = union(s1, union(s2, union(s3, union(s4, union(s5, s6)))))
      def odd(x: Int): Boolean = x%2 == 1
      def even(x: Int): Boolean = x%2 == 0
      def multipleOfFive(x: Int): Boolean = x%5 == 0
      assert(exists(odds, odd), "Odd")
      assert(!exists(odds, even), "No odd");
      assert(exists(evens, even), "Even")
      assert(!exists(evens, odd), "No even")
      assert(exists(s, multipleOfFive), "Multiples of 5")
      assert(!exists(evens, multipleOfFive), "No multiples of 5")
    }
  }

  test("map to transform a set") {
    new TestSets {
      val s = union(s1, union(s2, union(s3, union(s4, union(s5, s6)))))
      def double(x: Int): Int = 2*x
      def negativeEvens(x: Int): Int = if (x%2 == 0) -x else x 
      def tripleOdds(x: Int): Int = if (x%2 == 1) 3*x else x
      assert(contains(map(s, double), 12), "Map 1")
      assert(!contains(map(s, double), 5), "Map 2")
      assert(!contains(map(s, double), 45), "Map 3")
      assert(!contains(map(s, negativeEvens), -3), "Map 4")
      assert(!contains(map(s, negativeEvens), -32), "Map 5")
      assert(contains(map(s, negativeEvens), -6), "Map 6")
      assert(contains(map(s, tripleOdds), 3), "Map 4")
      assert(!contains(map(s, tripleOdds), 5), "Map 5")
      assert(!contains(map(s, tripleOdds), 12), "Map 6")
    }
  }


}
