import org.scalatest.FunSuite

class Ciphers extends FunSuite {

  test("Caesar cipher: Letter encode/decode.") {

    val Message = "A"
    val CodedMessage = CaesarCipher.Encode(Message)

    assert(CodedMessage == "D")

    val DecodedMessage = CaesarCipher.Decode(CodedMessage)

    assert(DecodedMessage == Message)

  }

  test("Caesar cipher: Simple phrase encode/decode.") {

    val Message = "Hello, World!"
    val CodedMessage = CaesarCipher.Encode(Message)

    assert(CodedMessage == "Khoor, Zruog!")

    val DecodedMessage = CaesarCipher.Decode(CodedMessage)

    assert(DecodedMessage == Message)

  }

  test("Rotation cipher: Two letter encode/decode without shift.") {

    val Message = "AB"
    val CodedMessage = RotationCipher.Encode(Message, 0)

    assert(CodedMessage == "AC")

    val DecodedMessage = RotationCipher.Decode(CodedMessage, 0)

    assert(DecodedMessage == Message)

  }

  test("Rotation cipher: Simple phrase encode/decode without shift.") {

    val Message = "Hello, World!"
    val CodedMessage = RotationCipher.Encode(Message, 0)

    assert(CodedMessage == "Hfnos, Buytm!")

    val DecodedMessage = RotationCipher.Decode(CodedMessage, 0)

    assert(DecodedMessage == Message)

  }

  test("Rotation cipher: Two letter encode/decode with shift.") {

    val Message = "AB"
    val CodedMessage = RotationCipher.Encode(Message, 3)

    assert(CodedMessage == "DF")

    val DecodedMessage = RotationCipher.Decode(CodedMessage, 3)

    assert(DecodedMessage == Message)

  }

  test("Rotation cipher: Simple phrase encode/decode with shift.") {

    val Message = "Hello, World!"
    val CodedMessage = RotationCipher.Encode(Message, 3)

    assert(CodedMessage == "Kiqrv, Exbwp!")

    val DecodedMessage = RotationCipher.Decode(CodedMessage, 3)

    assert(DecodedMessage == Message)

  }

  test("Enigma cipher: Two letter encode/decode without shift.") {

    val Message = "AB"
    val CodedMessage = EnigmaCipher.Encode(Message, 0, 0)

    assert(CodedMessage == "NR")

    val DecodedMessage = EnigmaCipher.Decode(CodedMessage, 0, 0)

    assert(DecodedMessage == Message)

  }

  test("Enigma cipher: Two letter encode/decode with shift in inner ring.") {

    val Message = "AB"
    val CodedMessage = EnigmaCipher.Encode(Message, 5, 0)

    assert(CodedMessage == "RV")

    val DecodedMessage = EnigmaCipher.Decode(CodedMessage, 5, 0)

    assert(DecodedMessage == Message)

  }

  test("Enigma cipher: Two letter encode/decode with shifts in inner ring and middle ring.") {

    val Message = "AB"
    val CodedMessage = EnigmaCipher.Encode(Message, 3, 2)

    assert(CodedMessage == "OS")

    val DecodedMessage = EnigmaCipher.Decode(CodedMessage, 3, 2)

    assert(DecodedMessage == Message)

  }

  test("Enigma cipher: Simple word encode/decode without shift.") {

    val Message = "Hello!"
    val CodedMessage = EnigmaCipher.Encode(Message, 0, 0)

    assert(CodedMessage == "Dfmch!")

    val DecodedMessage = EnigmaCipher.Decode(CodedMessage, 0, 0)

    assert(DecodedMessage == Message)

  }

  test("Enigma cipher: Simple word encode/decode with shift in inner ring.") {

    val Message = "Hello!"
    val CodedMessage = EnigmaCipher.Encode(Message, 2, 0)

    assert(CodedMessage == "Kmtjo!")

    val DecodedMessage = EnigmaCipher.Decode(CodedMessage, 2, 0)

    assert(DecodedMessage == Message)

  }

  test("Enigma cipher: Simple word encode/decode with shifts in inner ring and middle ring.") {

    val Message = "Hello!"
    val CodedMessage = EnigmaCipher.Encode(Message, 2, 2)

    assert(CodedMessage == "Oqxns!")

    val DecodedMessage = EnigmaCipher.Decode(CodedMessage, 2, 2)

    assert(DecodedMessage == Message)

  }

  test("Enigma cipher: Big phrase encode/decode without shift.") {

    val Message = "Hello, World! What a beaultiful morning this is!"
    val CodedMessage = EnigmaCipher.Encode(Message, 0, 0)
    val DecodedMessage = EnigmaCipher.Decode(CodedMessage, 0, 0)

    assert(DecodedMessage == Message)

  }

  test("Enigma cipher: Big phrase encode/decode with shift in inner ring.") {

    val Message = "Hello, World! What a beaultiful morning this is!"
    val CodedMessage = EnigmaCipher.Encode(Message, 7, 0)
    val DecodedMessage = EnigmaCipher.Decode(CodedMessage, 7, 0)

    assert(DecodedMessage == Message)

  }

  test("Enigma cipher: Big phrase encode/decode with shifts in inner ring and middle ring.") {

    val Message = "Hello, World! What a beaultiful morning this is!"
    val CodedMessage = EnigmaCipher.Encode(Message, 15, 7)
    val DecodedMessage = EnigmaCipher.Decode(CodedMessage, 15, 7)

    assert(DecodedMessage == Message)

  }

}

class BinaryResearchTrees extends FunSuite {

  test("Integer Tree: Node creation.") {

    val Tree01 = new NormalNode[Integer](1)

    assert(Tree01.Value() == 1)
    assert(Tree01.Right() == EmptyNode())
    assert(Tree01.Left() == EmptyNode())

  }

  test("Integer Tree: Inserting a node in a empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    assert(Tree01.Head() == EmptyNode())

    Tree01.Insert(1)

    Tree01.Head() match {

      case e: EmptyNode => fail()

      case n: NormalNode[Integer] =>

        assert(n.Value() == 1)
        assert(n.Right() == EmptyNode())
        assert(n.Left() == EmptyNode())

    }

  }

  test("Integer Tree: Inserting a node in a non-empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    assert(Tree01.Head() == EmptyNode())

    Tree01.Insert(5)
    Tree01.Insert(3)

    Tree01.Head() match {

      case e: EmptyNode => fail()

      case n: NormalNode[Integer] =>

        assert(n.Value() == 5)
        assert(n.Right() == EmptyNode())
        assert(n.Left() != EmptyNode())

        n.Left() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 3)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

    }

    Tree01.Insert(7)

    Tree01.Head() match {

      case e: EmptyNode => fail()

      case n: NormalNode[Integer] =>

        assert(n.Value() == 5)
        assert(n.Right() != EmptyNode())
        assert(n.Left() != EmptyNode())

        n.Left() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 3)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

        n.Right() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 7)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

    }

  }

  test("Integer Tree: Attempting to insert a node that is already in tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    assert(Tree01.Head() == EmptyNode())

    Tree01.Insert(5)

    try {
      Tree01.Insert(5)
    }

    catch {

      case i: IllegalArgumentException => succeed

      case _: Throwable => fail()

    }

  }

  test("Integer Tree: Inserting multiple nodes in a non-empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    val list: List[Integer] = List(3, 7)

    assert(Tree01.Head() == EmptyNode())

    Tree01.Insert(5)

    Tree01.Insert(list)

    Tree01.Head() match {

      case e: EmptyNode => fail()

      case n: NormalNode[Integer] =>

        assert(n.Value() == 5)
        assert(n.Right() != EmptyNode())
        assert(n.Left() != EmptyNode())

        n.Left() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 3)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

        n.Right() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 7)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

    }

  }

  test("Integer Tree: Destroy a tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    assert(Tree01.Head() == EmptyNode())

    Tree01.Insert(1)

    Tree01.Head() match {

      case e: EmptyNode => fail()

      case n: NormalNode[Integer] =>

        assert(n.Value() == 1)
        assert(n.Right() == EmptyNode())
        assert(n.Left() == EmptyNode())

    }

    Tree01.Delete()

    assert(Tree01.Head() == EmptyNode())

  }

  test("Integer Tree: Reading a empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    val List = Tree01.Read()

    assert(List.isEmpty)

  }

  test("Integer Tree: Reading a non-empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(5)
    Tree01.Insert(3)
    Tree01.Insert(2)
    Tree01.Insert(10)
    Tree01.Insert(7)

    val List: List[Integer] = Tree01.Read()

    assert(List.nonEmpty)
    assert(List.contains(5))
    assert(List.contains(3))
    assert(List.contains(2))
    assert(List.contains(10))
    assert(List.contains(7))
    assert(List.size == 5)

  }

  test("Integer Tree: Creating a non-empty tree from a list.") {

    val list: List[Integer] = List(5, 3, 7)

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Create(list)

    Tree01.Head() match {

      case e: EmptyNode => fail()

      case n: NormalNode[Integer] =>

        assert(n.Value() == 5)
        assert(n.Right() != EmptyNode())
        assert(n.Left() != EmptyNode())

        n.Left() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 3)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

        n.Right() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 7)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

    }

  }

  test("Integer Tree: Creating a empty tree from a list.") {

    val list: List[Integer] = List()

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Create(list)

    assert(Tree01.Head() == EmptyNode())

  }

  test("Integer Tree: Overriding a tree from a list.") {

    val list: List[Integer] = List()

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(5)
    Tree01.Insert(3)
    Tree01.Insert(2)
    Tree01.Insert(10)
    Tree01.Insert(7)

    Tree01.Create(list)

    assert(Tree01.Head() == EmptyNode())

  }

  test("Integer Tree: Finding a element in a empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    try {
      Tree01.Find(7)
    }

    catch {

      case r: RuntimeException => succeed

      case _: Throwable => fail()

    }


  }

  test("Integer Tree: Finding a element in a non-empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(5)
    Tree01.Insert(3)
    Tree01.Insert(7)

    try {
      Tree01.Find(3)
    }

    catch {

      case r: RuntimeException => fail()
      case i: IllegalArgumentException => fail()
      case _: Throwable =>

    }

    val Searched = Tree01.Find(3)

    assert(Searched == 3)

  }

  test("Integer Tree: Trying to find a non-existing element in a non-empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(5)
    Tree01.Insert(3)
    Tree01.Insert(7)

    try {
      Tree01.Find(4)
    }

    catch {

      case i: IllegalArgumentException => succeed
      case _: Throwable => fail()

    }

  }

  test("Integer Tree: Height of a empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    assert(Tree01.Height() == 0)

  }

  test("Integer Tree: Height of a tree with a single element.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(5)

    assert(Tree01.Height() == 1)

  }

  test("Integer Tree: Height of a non-empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(5)
    Tree01.Insert(3)
    Tree01.Insert(7)
    Tree01.Insert(6)
    Tree01.Insert(8)
    Tree01.Insert(9)
    Tree01.Insert(1)
    Tree01.Insert(2)

    assert(Tree01.Height() == 4)

  }

  test("Integer Tree: Balancing a empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Balance()

    assert(Tree01.Head() == EmptyNode())

  }

  test("Integer Tree: Balancing a single node tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(5)

    Tree01.Balance()

    Tree01.Head() match {

      case e: EmptyNode => fail()

      case n: NormalNode[Integer] =>

        assert(n.Value() == 5)
        assert(n.Right() == EmptyNode())
        assert(n.Left() == EmptyNode())

    }

  }

  test("Integer Tree: Balancing a non-empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(3)
    Tree01.Insert(5)
    Tree01.Insert(7)

    Tree01.Balance()

    Tree01.Head() match {

      case e: EmptyNode => fail()

      case n: NormalNode[Integer] =>

        assert(n.Value() == 5)
        assert(n.Right() != EmptyNode())
        assert(n.Left() != EmptyNode())

        n.Left() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 3)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

        n.Right() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 7)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

    }

  }

  test("Integer Tree: Balancing a non-empty tree with a even number of nodes.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(3)
    Tree01.Insert(5)
    Tree01.Insert(7)
    Tree01.Insert(4)

    Tree01.Balance()

    Tree01.Head() match {

      case e: EmptyNode => fail()

      case n: NormalNode[Integer] =>

        assert(n.Value() == 4)
        assert(n.Right() != EmptyNode())
        assert(n.Left() != EmptyNode())

        n.Left() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 3)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

        n.Right() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 5)
            assert(n.Right() != EmptyNode())
            assert(n.Left() == EmptyNode())

            n.Right() match {

              case e: EmptyNode => fail()

              case n: NormalNode[Integer] =>

                assert(n.Value() == 7)
                assert(n.Right() == EmptyNode())
                assert(n.Left() == EmptyNode())

            }

        }

    }

  }

  test("Integer tree: Filtering a non-empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(5)
    Tree01.Insert(3)
    Tree01.Insert(7)
    Tree01.Insert(4)
    Tree01.Insert(2)
    Tree01.Insert(1)

    Tree01.Filter(x => x > 4)

    Tree01.Head() match {

      case e: EmptyNode => fail()

      case n: NormalNode[Integer] =>

        assert(n.Value() == 5)
        assert(n.Right() != EmptyNode())
        assert(n.Left() == EmptyNode())

        n.Right() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 7)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

    }

  }

  test("Integer tree: Status of a empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    assert(Tree01.IsEmpty())

  }

  test("Integer tree: Status of a non-empty tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(5)

    assert(!Tree01.IsEmpty())

  }

  test("Integer tree: Seeing if a element is present in a tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(5)

    assert(Tree01.Contains(5))
    assert(!Tree01.Contains(3))

  }

  test("Integer tree: Mapping a change to all the tree.") {

    val Tree01 = new BinarySearchTree[Integer]

    Tree01.Insert(5)
    Tree01.Insert(7)
    Tree01.Insert(9)

    Tree01.Map(x => x-3)

    Tree01.Balance()

    Tree01.Head() match {

      case e: EmptyNode => fail()

      case n: NormalNode[Integer] =>

        assert(n.Value() == 4)
        assert(n.Right() != EmptyNode())
        assert(n.Left() != EmptyNode())

        n.Left() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 2)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

        n.Right() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Integer] =>

            assert(n.Value() == 6)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

    }

  }

  test("Student tree: General testing") {

    class Student(private val id: Integer) extends Comparable[Student] {

      def compareTo(other: Student): Int = {

        if (this.id > other.id) 1

        else if (this.id < other.id) -1

        else 0

      }

      def ID(): Integer = this.id

    }

    val Tree01 = new BinarySearchTree[Student]

    val Student01 = new Student(160034887)
    val Student02 = new Student(160048990)
    val Student03 = new Student(140031226)

    Tree01.Insert(Student01)
    Tree01.Insert(Student03)
    Tree01.Insert(Student02)

    Tree01.Head() match {

      case e: EmptyNode => fail()

      case n: NormalNode[Student] =>

        assert(n.Value().ID() == 160034887)
        assert(n.Right() != EmptyNode())
        assert(n.Left() != EmptyNode())

        n.Left() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Student] =>

            assert(n.Value().ID() == 140031226)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

        n.Right() match {

          case e: EmptyNode => fail()

          case n: NormalNode[Student] =>

            assert(n.Value().ID() == 160048990)
            assert(n.Right() == EmptyNode())
            assert(n.Left() == EmptyNode())

        }

    }

  }

}
