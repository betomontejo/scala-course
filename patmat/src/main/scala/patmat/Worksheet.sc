package patmat

import patmat.Huffman._

object Worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  var decoded = decode(frenchCode,secret)         //> decoded  : List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
  
  secret                                          //> res0: List[patmat.Huffman.Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0,
                                                  //|  1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 
                                                  //| 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)
                                                  
  var decoded2 = decode(frenchCode, List(0,0,0,0))//> decoded2  : List[Char] = List(s)
  
  var encoded = encode(frenchCode)("huffmanestcool".toList)
                                                  //> encoded  : List[patmat.Huffman.Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 
                                                  //| 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1
                                                  //| , 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0,
                                                  //|  1)

                                            
  var encoded2 = encode(frenchCode)("sdx".toList) //> encoded2  : List[patmat.Huffman.Bit] = List(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1,
                                                  //|  0, 0, 0)
                                                  
  var h = encode(frenchCode)(List('h'))           //> h  : List[patmat.Huffman.Bit] = List(0, 0, 1, 1, 1, 0, 1)
  var i = encode(frenchCode)(List('i'))           //> i  : List[patmat.Huffman.Bit] = List(1, 1, 1, 0)
  var e = encode(frenchCode)(List('e'))           //> e  : List[patmat.Huffman.Bit] = List(1, 1, 0)
  var f = encode(frenchCode)(List('f'))           //> f  : List[patmat.Huffman.Bit] = List(0, 0, 1, 1, 0, 1)
  var a = encode(frenchCode)(List('a'))           //> a  : List[patmat.Huffman.Bit] = List(1, 1, 1, 1)
  var l = encode(frenchCode)(List('l'))           //> l  : List[patmat.Huffman.Bit] = List(0, 1, 0, 1)
  var s = encode(frenchCode)(List('s'))           //> s  : List[patmat.Huffman.Bit] = List(0, 0, 0)
  var t = encode(frenchCode)(List('t'))           //> t  : List[patmat.Huffman.Bit] = List(1, 0, 1, 1)
  var d = encode(frenchCode)(List('d'))           //> d  : List[patmat.Huffman.Bit] = List(0, 0, 1, 0)
  var r = encode(frenchCode)(List('r'))           //> r  : List[patmat.Huffman.Bit] = List(1, 0, 0, 0)
  var x = encode(frenchCode)(List('x'))           //> x  : List[patmat.Huffman.Bit] = List(0, 0, 1, 1, 0, 0, 0)
  
  var codeTable1 = convert(frenchCode)            //> codeTable1  : patmat.Huffman.CodeTable = List((s,List(0, 0, 0)), (d,List(0, 
                                                  //| 0, 1, 0)), (x,List(0, 0, 1, 1, 0, 0, 0)), (j,List(0, 0, 1, 1, 0, 0, 1)), (f,
                                                  //| List(0, 0, 1, 1, 0, 1)), (z,List(0, 0, 1, 1, 1, 0, 0, 0, 0)), (k,List(0, 0, 
                                                  //| 1, 1, 1, 0, 0, 0, 1, 0)), (w,List(0, 0, 1, 1, 1, 0, 0, 0, 1, 1)), (y,List(0,
                                                  //|  0, 1, 1, 1, 0, 0, 1)), (h,List(0, 0, 1, 1, 1, 0, 1)), (q,List(0, 0, 1, 1, 1
                                                  //| , 1)), (o,List(0, 1, 0, 0)), (l,List(0, 1, 0, 1)), (m,List(0, 1, 1, 0, 0)), 
                                                  //| (p,List(0, 1, 1, 0, 1)), (u,List(0, 1, 1, 1)), (r,List(1, 0, 0, 0)), (c,List
                                                  //| (1, 0, 0, 1, 0)), (v,List(1, 0, 0, 1, 1, 0)), (g,List(1, 0, 0, 1, 1, 1, 0)),
                                                  //|  (b,List(1, 0, 0, 1, 1, 1, 1)), (n,List(1, 0, 1, 0)), (t,List(1, 0, 1, 1)), 
                                                  //| (e,List(1, 1, 0)), (i,List(1, 1, 1, 0)), (a,List(1, 1, 1, 1)))
                                                  
 var c1 = codeBits(codeTable1)('a')               //> c1  : List[patmat.Huffman.Bit] = List(1, 1, 1, 1)
 var c2 = codeBits(codeTable1)('d')               //> c2  : List[patmat.Huffman.Bit] = List(0, 0, 1, 0)
 var c3 = codeBits(codeTable1)('s')               //> c3  : List[patmat.Huffman.Bit] = List(0, 0, 0)
                                                    
}