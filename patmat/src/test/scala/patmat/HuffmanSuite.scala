package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }
  
  trait TestListOfChars {
    val l1 = string2Chars("hello, world")
    val l2 = string2Chars("lorem ipsum dolor sit amet")
  }
  
  trait TestHuffmanTree {
    val t1 = Fork(Leaf('x',3), Fork(Leaf('e',2),Leaf('t',2),List('e', 't'),4), List('x','e','t'), 7 )
    val lt1 = List(t1)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t1) === List('a','b'))
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  
  test("times of list of chars") {
    new TestListOfChars {
      assert(times(l1) === List(('h',1), ('e',1), ('l',3), ('o',2), (',',1), (' ',1), ('w',1), ('r',1), ('d',1)))
      assert(times(l2) === List(('l', 2), ('o', 3), ('r', 2), ('e', 2), ('m', 3), (' ', 4), ('i',2), ('p',1), ('s',2), ('u',1), ('d',1), ('t',2), ('a',1)))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  
  test("singleton") {
    assert(singleton(List(new Leaf('a',1))))
    assert(!singleton(List()))
    assert(!singleton(List(new Leaf('a',1), new Leaf('b',2))))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  test ("combine a list of less than two") {
    val leaflist1 = List(Leaf('a',1))
    assert(combine(leaflist1) === leaflist1)
    val leaflist2 = List()
    assert(combine(leaflist2) === leaflist2)
  }
  test("combine and test order") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 2), Leaf('x', 3))
    assert(combine(leaflist) === List(Leaf('x',3), Fork(Leaf('e',2),Leaf('t',2),List('e', 't'),4)))
  }
  
  test("until huffman tree") {
    new TestHuffmanTree {
	    val leaflist = List(Leaf('e', 2), Leaf('t', 2), Leaf('x', 3))
	    assert (until(singleton, combine)(leaflist) === lt1)
    }
  }
  
  test("create codetree") {
    new TestHuffmanTree {
	    val s = "xetxtex"
	    assert(createCodeTree(string2Chars(s)) === t1)
    }
  }
  
  test("decode test") {
    assert(decodedSecret === "huffmanestcool".toList)
  }
  
  test ("encode test") {
    assert(encode(frenchCode)("huffmanestcool".toList) === secret)
  }
  

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("quickencode") {
    assert(quickEncode(frenchCode)("huffmanestcool".toList) === secret)
  }
}
