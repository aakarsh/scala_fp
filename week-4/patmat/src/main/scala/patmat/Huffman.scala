package patmat

import common._

/**
 * Assignment 4: Huffman coding
 */
/**
 * Historic Background:
 *
 * Source: [https://en.wikipedia.org/wiki/Huffman_coding]
 *
 * In 1951, David A. Huffman and his MIT information theory classmates
 * were given the choice of a term paper or a final exam. The
 * professor, Robert M. Fano, assigned a term paper on the problem of
 * finding the most efficient binary code. Huffman, unable to prove
 * any codes were the most efficient, was about to give up and start
 * studying for the final when he hit upon the idea of using a
 * frequency-sorted binary tree and quickly proved this method the
 * most efficient.[3]
 *
 * In doing so, Huffman outdid Fano, who had worked with information
 * theory inventor Claude Shannon to develop a similar code. Building
 * the tree from the bottom up guaranteed optimality, unlike top-down
 * Shannon-Fano coding.
 *
 * Terminology :
 *
 * Q: Should Huffman coding be called Prefix-Free codes instead ?
 *
 * Huffman coding uses a specific method for choosing the
 * representation for each symbol, resulting in a prefix code
 * (sometimes called "prefix-free codes", that is, the bit string
 * representing some particular symbol is never a prefix of the bit
 * string representing any other symbol). Huffman coding is such a
 * widespread method for creating prefix codes that the term "Huffman
 * code" is widely used as a synonym for "prefix code" even when such a
 * code is not produced by Huffman's algorithm.
 *
 * Informal Description :
 *
 * Given : A set of symbols and their weights (usually proportional to
 * probabilities).
 *
 * Find :
 *
 * A prefix-free binary code (a set of codewords) with minimum
 * expected codeword length (equivalently, a tree with minimum
 * weighted path length from the root).
 *
 * Formal Description :
 *
 * Given :
 *
 * 1. Alphabet A = { a_1, a_2, a_3, ... , a_n }. A set of symbol alphabet
 * of size n
 * 2. Set W = { w_1, w_2, w_3,  ... w_n } a set of weights where w_i is weight(a_i).
 * Such that the weights are proportional to the frequency of occurances.
 *
 * Output:
 *
 * 1. Code C(A,W) = ( c_1, c_2, c_3, ..., c_n ) which is a
 * tuple of code words where c_i is a code word for a_i.
 *
 * Goal:
 *
 * Let L(C) = \sum{i=0..n} (w_i * length(c_i)) be the weighted path length of code C.
 * L(C) \leq L(T) for any arbitrary code T.
 *
 *
 * [ http://compression.ru/download/articles/huff/huffman_1952_minimum-redundancy-codes.pdf ]
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the
   * alphabet that the tree can encode.  The weight of a `Leaf` is the
   * frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a
   * set containing all the characters present in the leaves below
   * it. The weight of a `Fork` node is the sum of the weights of
   * these leaves.
   */
  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree,
                  chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics
  def onLeaves[T](tree:CodeTree) (unit: Leaf => T) (combine:(T,T) => T ): T = tree match {
    case Leaf(ch:Char,weight:Int) => unit(Leaf(ch,weight))
    case Fork(left:CodeTree, right:CodeTree,_,_) => {
      def onTree(tree:CodeTree) =  onLeaves(tree)(unit)(combine)
      combine(onTree(left),onTree(right))  // combine results of left and right tree
    }
  }

  /**
   * List of leaf weights.
   */
  def weight(tree: CodeTree): Int =
    onLeaves(tree){ case Leaf(_,weight:Int) => weight }{ _ + _ }

  /**
   * Returns list of all leaf characters in this Code Tree
   */
  def chars(tree: CodeTree): List[Char] =
    onLeaves(tree) { case Leaf(ch:Char,_) => List[Char](ch) } { _ ::: _ }


  def hasChar(ch:Char, tree:CodeTree) = tree match {
    case Leaf(leaf_ch,weight) => leaf_ch == ch
    case Fork(_,_,char_list: List[Char], _) => char_list.contains(ch)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  def toTreeString(root: CodeTree ) : String = {
    def toTreeString_r(tree:CodeTree,depth:Int):String = {
      (" "*depth) + 
      (tree match {
        case Leaf(ch:Char,w:Int) =>  "-"+"("+ch+","+w+")"
        case Fork(left:CodeTree,right: CodeTree,ch:List[Char],w:Int ) =>
          "[" +ch.mkString(",")+":"+w+ "]\n"+toTreeString_r(left,depth+1)+"\n"+toTreeString_r(right,depth+1)
      })
    }
    "\n"+toTreeString_r(root,0)
  }

  // Part 2: Generating Huffman trees
  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   *
   * This function computes for each unique character in the list
   * `chars` the number of times it occurs. For example, the
   * invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is
   * not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each
   * pair consists of a character and an integer. Pairs can be
   * constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the
   * accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern-matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   *
   */
  def times(chars: List[Char]): List[(Char, Int)] =
    chars.groupBy(identity).map({case (k,v) => (k,v.size)}).toList.sorted

  /**
   * Returns a list of `Leaf` nodes for a given frequency table
   *`freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =  {

    def makeLeafList(res: List[Leaf],freqs: List[(Char,Int)]) : List[Leaf] = {
      freqs match {
        case Nil => res
        case (ch:Char,freq:Int)::(xs:List[(Char,Int)]) => makeLeafList(Leaf(ch,freq)::res,xs)
      }
    }

    def freqOrdering(a:(Char,Int),b:(Char,Int)):Boolean = a._2 < b._2

    makeLeafList(List[Leaf](), freqs.sortWith(freqOrdering)).reverse
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {

    def reinsert(node:CodeTree, trees:List[CodeTree]) : List[CodeTree] = {
      trees match {
        case Nil => List[CodeTree](node)
        case tree::Nil =>
          if(weight(tree) > weight(node)) 
            node::tree::Nil // why is order reversed again?
          else 
            tree::node::Nil
        case head::rest =>
          if (weight(head) > weight(node)) node::head::rest
          else head::reinsert(node,rest)
      }
    }

    trees match {
      case Nil => Nil
      case tree::Nil => List[CodeTree](tree)
      case left::right::(xs:List[CodeTree]) => {
        reinsert(makeCodeTree(left,right),xs)
      }
    }

  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(stop_transform:  List[CodeTree] => Boolean,
            transform: List[CodeTree] => List[CodeTree]) (trees: List[CodeTree]): List[CodeTree] = {
    if(stop_transform(trees)) trees
    else until(stop_transform,transform)(transform(trees))
  }

  /**
   * This function creates a code tree which is optimal to encode the
   * text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function
   * extracts the character frequencies from that text and creates a
   * code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    val leafList = makeOrderedLeafList(times(chars))
    until(singleton,combine)(leafList) match {
      case tree::Nil => tree
      case _ => Leaf('.',0) // error state?
    }
  }

  // Part 3: Decoding
  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree
   * `tree` and returns the resulting list of characters.  Decoding
   * also starts at the root of the tree. Given a sequence of bits to
   * decode, we successively read the bits, and for each 0, we choose
   * the left branch, and for each 1 we choose the right branch. When
   * we reach a leaf, we decode the corresponding character and then
   * start again at the root.
   */
  def decode(root: CodeTree,bits: List[Bit]): List[Char] = {

    def decode_r(res: List[Char], tree: CodeTree,
                 bits: List[Bit]) : List[Char] =
          tree match {
            case Leaf(ch:Char, _) if bits.isEmpty => ch :: res // Append current leaf
            case Fork(_,_,_,_)    if bits.isEmpty => throw new Error("decode.unexpected.end:"+res)

            case Leaf(ch:Char,_) => decode_r(ch::res,root,bits)
            case Fork(left:CodeTree, _,_,_) if bits.head == 0 => decode_r(res,left,bits.tail)
            case Fork(_,right:CodeTree,_,_) if bits.head == 1 => decode_r(res,right,bits.tail)
          }

    val e = List[Char]()
    decode_r(e,root, bits).reverse
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree =
    Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode,secret)

  // Part-4a : Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   *
   * For a given Huffman tree, one can obtain the encoded
   * representation of a character by traversing from the root of the
   * tree
   *
   */
  def encode(root: CodeTree) (text: List[Char]) : List[Bit] = text.flatMap(encode_char(root))

  def encode_char(root:CodeTree)(ch:Char): List[Bit] = {

    def encode_char_r(tree:CodeTree): List[Bit] =
      tree match {
        case Leaf(_,_) => List[Bit]() // if three is a leaf
        case Fork(left:CodeTree,_,_,_)
          if hasChar(ch,left)  =>  0 :: encode_char_r(left)
        case Fork(_,right:CodeTree,_,_)
          if hasChar(ch,right) =>  1 :: encode_char_r(right)
        case _ => throw new Error("char not found"+ch)
      }

    // Apparently dropping reverse has no effect
    encode_char_r(root)
  }

  // Part 4b: Encoding using code table
  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] =
    table.filter( _ match { case (ch:Char, repr:List[Bit]) => ch == char }) match {
      case Nil =>  List[Bit]()
      case (ch:Char,repr:List[Bit])::other => repr
    }

  /**
   * Given a code tree, create a code table which contains, for every
   * character in the code tree, the sequence of bits representing
   * that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code
   * tree `tree` is itself a valid code tree that can be represented
   * as a code table. Using the code tables of the sub-trees, think of
   * how to build the code table for the entire tree.
   */
  def convert(root: CodeTree): CodeTable = {

    def convert_r(res: CodeTable, tree: CodeTree): CodeTable =
      tree match { // TODO : Did I change the semantics here by adding reverse?
        case Leaf(char : Char, _ ) =>
          (char, encode_char(root)(char))::res
        case Fork(left: CodeTree, right: CodeTree, _ , _ ) =>
          convert_r ( convert_r (res ,left) ::: res, right)
      }

    val e = List[(Char,List[Bit])]()
    convert_r(e, root)
  }

  /**
   * This function takes two code tables and merges them into
   * one. Depending on how you use it in the `convert` method above,
   * this merge method might also do some transformations on the two
   * parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = ???

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree
   * to a code table and then uses it to perform the actual encoding.
   *
   */
  def quickEncode(tree: CodeTree)(text: List[Char]) : List[Bit] = {
    val codeTable:CodeTable = convert(tree)
    text.flatMap(codeBits(codeTable))
  }

}
