import scala.collection.mutable.ListBuffer

class BinarySearchTree[T <: Comparable[T]] {

  private var head:BinarySearchTreeNode[T] = EmptyNode()

  def Head(): BinarySearchTreeNode[T] = this.head

  def Balance(): Unit = {

    val list = this.Read()

    if (list.nonEmpty) {

      this.Delete()
      this.BalanceInsert(list, 0, list.size - 1)

    }

  }

  private def BalanceInsert(list: List[T], start: Integer, end: Integer): Unit = {

    if (start <= end) {

      val middle: Integer = math.floor((start + end)/2.0).toInt

      this.Insert(list(middle))
      this.BalanceInsert(list, start, middle - 1)
      this.BalanceInsert(list, middle + 1, end)

    }

  }

  def Contains(value: T): Boolean = {

    val list = this.Read()

    list.contains(value)

  }

  def Create(list: List[T]) : Unit = {

    this.head match {

      case e: EmptyNode => list.foreach(c => this.Insert(c))

      case _ =>

        this.Delete()
        list.foreach(c => this.Insert(c))

    }

  }

  def Delete(): Unit = {

    this.head = EmptyNode()

  }

  def Filter(p:(T) => Boolean): Unit = {

    val list = this.Read()

    val newList = list.filter(p)

    this.Create(newList)

  }

  def Find(SearchedValue: T): T = {

    this.head match {

      case e: EmptyNode => throw new RuntimeException("Tree is empty.\n")
      case n: NormalNode[T] => n.Find(SearchedValue)

    }

  }

  def Height(): Integer = {

    this.head match {

      case e: EmptyNode => 0
      case n: NormalNode[T] => n.Height()

    }

  }

  def Insert(NewValue: T): Unit = {

    this.head match {

      case e: EmptyNode => this.head = new NormalNode(NewValue)

      case n: NormalNode[T] => n.Insert(NewValue)

    }

  }

  def Insert(list: List[T]): Unit = {

    list.foreach(c => this.Insert(c))

  }

  def IsEmpty(): Boolean = {

    this.head match {

      case e: EmptyNode => true

      case n: NormalNode[T] => false

    }

  }

  def Map(p:(T) => T): Unit = {

    val list = this.Read()

    val newList = list.map(p)

    this.Create(newList)

  }

  def Read(): List[T] = {

    val elements:ListBuffer[T] = ListBuffer()

    this.head match {

      case e: EmptyNode =>  elements.toList

      case n: NormalNode[T] =>

        n.Read(elements)
        elements.toList

    }

  }

}
