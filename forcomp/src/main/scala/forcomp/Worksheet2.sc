package forcomp

import Anagrams._

object Worksheet2 {
  val combsOfTea = combinations(wordOccurrences("tea"))
                                                  //> combsOfTea  : List[forcomp.Anagrams.Occurrences] = List(List(), List((t,1)),
                                                  //|  List((e,1)), List((e,1), (t,1)), List((a,1)), List((a,1), (t,1)), List((a,1
                                                  //| ), (e,1)), List((a,1), (e,1), (t,1)))
                                                  
  dictionaryByOccurrences(combsOfTea(7))          //> res0: List[forcomp.Anagrams.Word] = List(ate, eat, tea)
  
  def test(occurs: Occurrences) : List[Word] = occurs match {
      case Nil => List()
      case _ => {
	      val combs = combinations(occurs)
	      for (word <- dictionary; if combs.contains(wordOccurrences(word))) yield word
	    }
  }                                               //> test: (occurs: forcomp.Anagrams.Occurrences)List[forcomp.Anagrams.Word]
  
  test(wordOccurrences("tea"))                    //> res1: List[forcomp.Anagrams.Word] = List(at, ate, eat, et, tea)
  
  
  def test2(occurs: Occurrences) : List[Word] = occurs match {
     case Nil => Nil
     case _ => {
      val combs = combinations(occurs)
      dictionary.foldLeft(List[Word]())((a,w) => {
         val oc = wordOccurrences(w)
         if ( combs.contains(oc) ) (a:+w):::test2(subtract(occurs,oc))
         else a
      })
     }
     
  }                                               //> test2: (occurs: forcomp.Anagrams.Occurrences)List[forcomp.Anagrams.Word]
  
  test2(wordOccurrences("tea"))                   //> res2: List[forcomp.Anagrams.Word] = List(at, ate, eat, et, tea)
  
  def dicFold(l: List[Word]) = dictionary.foldLeft(List[Word]())( (a,w) => a:+w )
                                                  //> dicFold: (l: List[forcomp.Anagrams.Word])List[forcomp.Anagrams.Word]
  
}