import common._

object Workbook {

  import patmat.Huffman._

  val s = "This is a sample text for us to use during testing"

  val alphabet:List[Char]  = string2Chars("abcdefghijklmnopqrstuvwxyz")
  val singleOccurenceTree: CodeTree = createCodeTree(alphabet)

  val ts_string:List[Char]  = string2Chars("thequickbrownfoxjumpedoverthelazydog")

  val encoder:(List[Char] => List[Bit]) =  encode(singleOccurenceTree)

  val decoder:(List[Bit] => List[Char]) =  
  { (bits:List[Bit]) => decode(singleOccurenceTree,bits) }
  

  def testString(str:String) = {
    val ts_bits :List[Bit]  = encoder(str.toList)
    val back :List[Char] = decoder(ts_bits)
    println("ts_bits: " + ts_bits)
    println("ts:\n"+str+"\n"+ back.mkString)
    assert(str == back.mkString)
  }

  def main() = {

    println("Running sample test")

    println("times:"  + times(alphabet))
    println("leaves:" + makeOrderedLeafList(times(alphabet)))

    println("tree:"   + singleOccurenceTree)
    println("tree:"   + toTreeString(singleOccurenceTree))

    testString("foobar")
    testString("barfoo")
    testString("lajldfjaljfajfjafj")

    
    println("Finished sample test")
  }

}

//Workbook.main()
