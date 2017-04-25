import scala.collection.mutable.ListBuffer

sealed abstract class BinarySearchTreeNode[+T]{

  val empty: Boolean

}

case class EmptyNode() extends BinarySearchTreeNode[Nothing] {

  val empty = true

}

class NormalNode[T <: Comparable[T]]  (private var value: T) extends BinarySearchTreeNode[T] {

  val empty = false
  private var right: BinarySearchTreeNode[T] = EmptyNode()
  private var left: BinarySearchTreeNode[T] = EmptyNode()

  def Value (): T = value
  def Right (): BinarySearchTreeNode[T] = right
  def Left (): BinarySearchTreeNode[T] = left

  def Find (SearchedValue: T): T = {

    val comparison: Int = SearchedValue.compareTo(this.value)

    if (comparison < 0) {

      this.left match {

        case e: EmptyNode => throw new IllegalArgumentException("Element is not present in tree.\n")
        case n: NormalNode[T] => n.Find(SearchedValue)

      }

    }

    else if (comparison > 0) {

      this.right match {

        case e: EmptyNode => throw new IllegalArgumentException("Element is not present in tree.\n")
        case n: NormalNode[T] => n.Find(SearchedValue)

      }

    }

    else {
      this.Value()
    }

  }

  def Height(): Integer = {

    var LeftHeight: Integer = 0
    var RightHeight: Integer = 0

    this.left match {

      case e: EmptyNode => LeftHeight = 0
      case n: NormalNode[T] => LeftHeight = n.Height()

    }

    this.right match {

      case e: EmptyNode => RightHeight = 0
      case n: NormalNode[T] => RightHeight = n.Height()

    }

    math.max(1 + LeftHeight, 1 + RightHeight)

  }

  def Insert (NewValue: T): Unit = {

    val comparison: Int = NewValue.compareTo(this.value)

    if (comparison < 0) {

      this.left match {

        case e: EmptyNode => this.left = new NormalNode(NewValue)
        case n: NormalNode[T] => n.Insert(NewValue)

      }

    }

    else if (comparison > 0) {

      this.right match {

        case e: EmptyNode => this.right = new NormalNode(NewValue)
        case n: NormalNode[T] => n.Insert(NewValue)

      }

    }

    else {
      throw new IllegalArgumentException("Value is already present in tree.\n")
    }

  }

  def Read(list: ListBuffer[T]): Unit = {

    this.left match {

      case n: NormalNode[T] => n.Read(list)
      case _ =>

    }

    list += this.Value()

    this.right match {

      case n: NormalNode[T] => n.Read(list)
      case _ =>

    }

  }

}
