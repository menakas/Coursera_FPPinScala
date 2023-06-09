package patmat

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a','b','d'))
  }
  
  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(List('a', 'b', 'a'))") {
    assertEquals(times(List('a', 'b', 'a')), List(('a', 2),('b',1)))
    assertEquals(times(List('a', 'b', 'a', 'a', 'c', 'b')), List(('a', 3),('b',2),('c',1)))
  }
  
  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list (15pts)") {
    val ll = List(Leaf('a',1))
    assert(singleton(ll))

    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))

    //println(createCodeTree(string2Chars("London")))
    assertEquals(createCodeTree(string2Chars("London")),
      Fork(Leaf('n',2),Fork(Fork(Leaf('L',1),Leaf('d',1),List('L', 'd'),2),Leaf('o',2),List('L', 'd', 'o'),4),List('n', 'L', 'd', 'o'),6))
      //Fork(Fork(Fork(Leaf('L',1),Leaf('d',1),List('L', 'd'),2),Leaf('o',2),List('L', 'd', 'o'),4),Leaf('n',2),List('L', 'd', 'o', 'n'),6))

      //println(decode(createCodeTree(string2Chars("London")),List(0,0,0,0,1,1)))
  }

  
  test("decode and encode a very short text should be identity (10pts)") {
    println(decodedSecret)
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }

   test("quickEncode huffmanestcool") {
    new TestTrees:
      assertEquals(quickEncode(frenchCode)("huffmanestcool".toList), List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1))
  }

  
  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
