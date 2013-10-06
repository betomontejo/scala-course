package funsets

object Worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import FunSets._
  
	val s1 = singletonSet(1)                  //> s1  : Int => Boolean = <function1>
	val s2 = singletonSet(2)                  //> s2  : Int => Boolean = <function1>
	val s3 = singletonSet(3)                  //> s3  : Int => Boolean = <function1>
	val s4 = singletonSet(4)                  //> s4  : Int => Boolean = <function1>
	val s5 = singletonSet(5)                  //> s5  : Int => Boolean = <function1>
	val s6 = singletonSet(6)                  //> s6  : Int => Boolean = <function1>
  
  val u1 = union(s1,union(s2, s3))                //> u1  : Int => Boolean = <function1>
  val u2 = union(s2, union(s4, s5))               //> u2  : Int => Boolean = <function1>
  val u3 = union(s1, union(s2, union(s3, union(s4, union(s5, s6)))))
                                                  //> u3  : Int => Boolean = <function1>
  
  FunSets.toString(u3)                            //> res0: String = {1,2,3,4,5,6}
  FunSets.toString(union(u1, u2))                 //> res1: String = {1,2,3,4,5}
  FunSets.toString(intersect(u1, u2))             //> res2: String = {2}
  FunSets.toString(diff(u3, u1))                  //> res3: String = {4,5,6}
  FunSets.toString(filter(u3, (x) => x%2 == 0))   //> res4: String = {2,4,6}
  
  FunSets.toString(map(u3, (x) => x*x))           //> res5: String = {1,4,9,16,25,36}
  
}