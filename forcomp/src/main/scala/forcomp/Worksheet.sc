package forcomp

import Anagrams._

object Worksheet {

  val w1 = "abcdEaBc"                             //> w1  : String = abcdEaBc
  
  val occurs = wordOccurrences("tea")             //> occurs  : forcomp.Anagrams.Occurrences = List((a,1), (e,1), (t,1))

  val lst1 = List( ('a',1), ('a',2) )             //> lst1  : List[(Char, Int)] = List((a,1), (a,2))
  val lst2 = List( ('b',1), ('b',2) )             //> lst2  : List[(Char, Int)] = List((b,1), (b,2))
  val lst3 = List( ('c',1), ('c',2) )             //> lst3  : List[(Char, Int)] = List((c,1), (c,2))
  val lst = List(lst1, lst2, lst3)                //> lst  : List[List[(Char, Int)]] = List(List((a,1), (a,2)), List((b,1), (b,2))
                                                  //| , List((c,1), (c,2)))
  
  val comb1 = for (b <- lst2 ; c <- lst3) yield List(b,c)
                                                  //> comb1  : List[List[(Char, Int)]] = List(List((b,1), (c,1)), List((b,1), (c,2
                                                  //| )), List((b,2), (c,1)), List((b,2), (c,2)))
  val comb2 = for (a <- lst1 ; cb <- comb1) yield  a :: cb
                                                  //> comb2  : List[List[(Char, Int)]] = List(List((a,1), (b,1), (c,1)), List((a,1
                                                  //| ), (b,1), (c,2)), List((a,1), (b,2), (c,1)), List((a,1), (b,2), (c,2)), List
                                                  //| ((a,2), (b,1), (c,1)), List((a,2), (b,1), (c,2)), List((a,2), (b,2), (c,1)),
                                                  //|  List((a,2), (b,2), (c,2)))

  def expand(pair: (Char, Int)): List[(Char, Int)] = (for ( i <- 0 to pair._2) yield (pair._1,i)) toList
                                                  //> expand: (pair: (Char, Int))List[(Char, Int)]
                                                  
  expand(('a',2))                                 //> res0: List[(Char, Int)] = List((a,0), (a,1), (a,2))

 def combin1(ls1: List[(Char, Int)], ls2: List[(Char, Int)]) : List[List[(Char, Int)]] = for (a <- ls1 ; b <- ls2) yield List(a,b)
                                                  //> combin1: (ls1: List[(Char, Int)], ls2: List[(Char, Int)])List[List[(Char, In
                                                  //| t)]]
 combin1(lst2, lst3)                              //> res1: List[List[(Char, Int)]] = List(List((b,1), (c,1)), List((b,1), (c,2)),
                                                  //|  List((b,2), (c,1)), List((b,2), (c,2)))
 def combin2(ls1: List[(Char, Int)], ls2: List[List[(Char, Int)]]) : List[List[(Char, Int)]] = for (a <- ls1 ; b <- ls2) yield  a :: b
                                                  //> combin2: (ls1: List[(Char, Int)], ls2: List[List[(Char, Int)]])List[List[(Ch
                                                  //| ar, Int)]]
 combin2(lst1, combin1(lst2,lst3))                //> res2: List[List[(Char, Int)]] = List(List((a,1), (b,1), (c,1)), List((a,1), 
                                                  //| (b,1), (c,2)), List((a,1), (b,2), (c,1)), List((a,1), (b,2), (c,2)), List((a
                                                  //| ,2), (b,1), (c,1)), List((a,2), (b,1), (c,2)), List((a,2), (b,2), (c,1)), Li
                                                  //| st((a,2), (b,2), (c,2)))
                                                  
def combinAll(occur: List[(Char, Int)], listOccur: List[List[(Char, Int)]]) : List[List[(Char, Int)]] = listOccur match {
  case ls :: Nil => for (a <- occur ; b <- ls) yield List(a,b)
  case ls :: lr => for (a <- occur ; b <- combinAll(ls, lr)) yield  a :: b
}                                                 //> combinAll: (occur: List[(Char, Int)], listOccur: List[List[(Char, Int)]])Li
                                                  //| st[List[(Char, Int)]]

def explode(l: List[(Char, Int)]) : List[List[(Char, Int)]] =
 ( for (lst <- l) yield expand(lst) ) toList      //> explode: (l: List[(Char, Int)])List[List[(Char, Int)]]

explode(occurs)                                   //> res3: List[List[(Char, Int)]] = List(List((a,0), (a,1)), List((e,0), (e,1))
                                                  //| , List((t,0), (t,1)))
                         


 def combine(l: List[List[(Char, Int)]]) : List[List[(Char, Int)]] = l match {
    case Nil => List(List())
    case la :: Nil => List() :: (for (lst <- la) yield List(lst) )
    case la :: ls => combinAll(la, ls)
 }                                                //> combine: (l: List[List[(Char, Int)]])List[List[(Char, Int)]]


  val totalComb = combine(explode(occurs))        //> totalComb  : List[List[(Char, Int)]] = List(List((a,0), (e,0), (t,0)), List
                                                  //| ((a,0), (e,0), (t,1)), List((a,0), (e,1), (t,0)), List((a,0), (e,1), (t,1))
                                                  //| , List((a,1), (e,0), (t,0)), List((a,1), (e,0), (t,1)), List((a,1), (e,1), 
                                                  //| (t,0)), List((a,1), (e,1), (t,1)))

  totalComb map (_.filter( x => x._2 > 0))        //> res4: List[List[(Char, Int)]] = List(List(), List((t,1)), List((e,1)), List
                                                  //| ((e,1), (t,1)), List((a,1)), List((a,1), (t,1)), List((a,1), (e,1)), List((
                                                  //| a,1), (e,1), (t,1)))

  val m1 = w1 map (_.toLower -> 1)                //> m1  : scala.collection.immutable.IndexedSeq[(Char, Int)] = Vector((a,1), (b
                                                  //| ,1), (c,1), (d,1), (e,1), (a,1), (b,1), (c,1))

  val m2 = w1 groupBy ( ch => ch.toLower )        //> m2  : scala.collection.immutable.Map[Char,String] = Map(e -> E, a -> aa, b 
                                                  //| -> bB, c -> cc, d -> d)
 
  val l1 = m2 toList                              //> l1  : List[(Char, String)] = List((e,E), (a,aa), (b,bB), (c,cc), (d,d))
  
  var l2 = for ( (x,y) <- l1 ) yield ( x,y.length )
                                                  //> l2  : List[(Char, Int)] = List((e,1), (a,2), (b,2), (c,2), (d,1))
  
  l2 sortBy( x => x._1 )                          //> res5: List[(Char, Int)] = List((a,2), (b,2), (c,2), (d,1), (e,1))
 
  (for ( (x,y) <- (w1 groupBy (ch => ch.toLower) toList) ) yield (x, y.length)) sortBy(x => x._1)
                                                  //> res6: List[(Char, Int)] = List((a,2), (b,2), (c,2), (d,1), (e,1))
 
  wordOccurrences(w1)                             //> res7: forcomp.Anagrams.Occurrences = List((a,2), (b,2), (c,2), (d,1), (e,1)
                                                  //| )

  val lw1 = List("this","is","a","word")          //> lw1  : List[String] = List(this, is, a, word)
  
  val lw2 = List()                                //> lw2  : List[Nothing] = List()
  
  lw1 flatten                                     //> res8: List[Char] = List(t, h, i, s, i, s, a, w, o, r, d)
  
  lw1 reduceLeft(_ + _)                           //> res9: String = thisisaword
  
  //def concat(x: List[String]): String = x foldLeft("")( (a,b) => a.concat(b) )
  
  lw1 mkString                                    //> res10: String = thisisaword
  
  lw2 mkString("")                                //> res11: String = ""
  
  Anagrams.sentenceOccurrences(lw1)               //> res12: forcomp.Anagrams.Occurrences = List((a,1), (d,1), (h,1), (i,2), (o,1
                                                  //| ), (r,1), (s,2), (t,1), (w,1))
 
  val words = List("tea", "eat", "ate")           //> words  : List[String] = List(tea, eat, ate)
  
  words map ( x =>  wordOccurrences(x))           //> res13: List[forcomp.Anagrams.Occurrences] = List(List((a,1), (e,1), (t,1)),
                                                  //|  List((a,1), (e,1), (t,1)), List((a,1), (e,1), (t,1)))

  words groupBy( w => wordOccurrences(w))         //> res14: scala.collection.immutable.Map[forcomp.Anagrams.Occurrences,List[Str
                                                  //| ing]] = Map(List((a,1), (e,1), (t,1)) -> List(tea, eat, ate))

  val ls1 = List(1,2,3)                           //> ls1  : List[Int] = List(1, 2, 3)
  val ls2 = List(4,5,6)                           //> ls2  : List[Int] = List(4, 5, 6)
  for ( i <- ls1 ; j <- ls2) yield (i,j)          //> res15: List[(Int, Int)] = List((1,4), (1,5), (1,6), (2,4), (2,5), (2,6), (3
                                                  //| ,4), (3,5), (3,6))
  val sentence = wordOccurrences("sentence")      //> sentence  : forcomp.Anagrams.Occurrences = List((c,1), (e,3), (n,2), (s,1),
                                                  //|  (t,1))
  val set = wordOccurrences("set")                //> set  : forcomp.Anagrams.Occurrences = List((e,1), (s,1), (t,1))
  
  val setMap = set.toMap.withDefaultValue(0)      //> setMap  : scala.collection.immutable.Map[Char,Int] = Map(e -> 1, s -> 1, t 
                                                  //| -> 1)
  
  
  (for(elem <- sentence ) yield (elem._1, elem._2 -(setMap(elem._1)) )).filter( x => x._2 > 0)
                                                  //> res16: List[(Char, Int)] = List((c,1), (e,2), (n,2))
  val combsOfTea = combinations(occurs)           //> combsOfTea  : List[forcomp.Anagrams.Occurrences] = List(List(), List((t,1))
                                                  //| , List((e,1)), List((e,1), (t,1)), List((a,1)), List((a,1), (t,1)), List((a
                                                  //| ,1), (e,1)), List((a,1), (e,1), (t,1)))
                                                  
  dictionaryByOccurrences(combsOfTea(7))          //> res17: List[forcomp.Anagrams.Word] = List(ate, eat, tea)
}