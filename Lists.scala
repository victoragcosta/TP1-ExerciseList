abstract class ListCell[T] {
  var value : T
  var index : Int
  var nextElem : ListCell[T]
  def isEmpty: Boolean
}

class EmptyCell[T] extends ListCell[T]{
  var value : T = _
  var index : Int = _
  var nextElem : ListCell[T] = _
  def isEmpty = true
}

class ValueCell[T](v: T, idx: Int) extends ListCell[T]{
  var value : T = v
  var index : Int = idx
  var nextElem: ListCell[T] = new EmptyCell[T]
  def isEmpty = false
}

class GenericList[T] {
  private var head: ListCell[T] = new EmptyCell[T]
  private var last: ListCell[T] = head

  def printList(list: GenericList[T]): Unit = {
    var cur_cell: ListCell[T] = list.head
    while (!cur_cell.isEmpty) {
      print("Cell: "+cur_cell.index+"\tValue: "+cur_cell.value+"\n")
      cur_cell = cur_cell.nextElem
    }
  }

  def size(list: GenericList[T]): Int = {
    list.last.index
  }

  def getIdx(list: GenericList[T], value: T): Int = {
    var cur_cell : ListCell[T] = list.head
    if(cur_cell.isEmpty){
      -1
    }
    else{
      while (cur_cell.value != value && !cur_cell.isEmpty){
        cur_cell = cur_cell.nextElem
      }
      if(cur_cell.value == value){
        cur_cell.index
      }
      else{
        -1
      }
    }
  }

  def getValue(list: GenericList[T], idx :Int) : T = {
    var cur_cell : ListCell[T] = list.head
    if(!cur_cell.isEmpty){
      while (cur_cell.index != idx && !cur_cell.isEmpty){
        cur_cell = cur_cell.nextElem
      }
      cur_cell.value
    }
    cur_cell.value
  }

  def add(list: GenericList[T], value: T): Unit = {
    if (list.head.isEmpty) {
      list.head = new ValueCell[T](value, 1)
      list.last = list.head
    }
    else {
      list.last.nextElem = new ValueCell[T](value, list.last.index + 1)
      list.last = list.last.nextElem
    }
  }

  def addIndex(list: GenericList[T], value: T, idx: Int): Unit = {
    var currentCell : ListCell[T] = list.head
    var auxCell : ListCell[T] = list.head
    var currentIdx : Int = 0
    if(currentCell.isEmpty && idx == 1) {
      list.head = new ValueCell[T](value, idx)
      list.last = list.head
    }
    else if(idx == list.last.index+1){
      list.last.nextElem = new ValueCell[T](value, idx)
      list.last = list.last.nextElem
    }
    else if(idx == list.last.index){
      list.last.nextElem = new ValueCell[T](list.last.value, list.last.index+1)
      list.last.index = idx
      list.last.value = value
      list.last = list.last.nextElem
    }
    else if (idx > 0 && idx < list.last.index){
      while(currentIdx!=idx){
        currentIdx+=1
        auxCell = currentCell
        currentCell = currentCell.nextElem
      }
      auxCell.nextElem = new ValueCell[T](auxCell.value, auxCell.index+1)
      auxCell.value = value
      auxCell.nextElem.nextElem = currentCell
      while(!currentCell.isEmpty){
        currentCell.index +=1
        currentCell = currentCell.nextElem
      }
    }
  }

  def remove(list : GenericList[T]) : Unit = {
    var currentCell : ListCell[T] = list.head
    var auxCell : ListCell[T] = list.head.nextElem
    if(auxCell.isEmpty){
      deleteList(list)
    }
    else if(!currentCell.isEmpty){
      while(auxCell!=list.last){
        auxCell = auxCell.nextElem
        currentCell = currentCell.nextElem
      }
      list.last = currentCell
      currentCell.nextElem = new EmptyCell[T]
    }
  }

  def removeIndex(list : GenericList[T], idx : Int): Unit ={
    var currentCell : ListCell[T] = list.head
    var auxCell : ListCell[T] = list.head
    if(idx == list.last.index && !currentCell.isEmpty){
      list.remove(list)
    }
    else if(idx == currentCell.index && !currentCell.isEmpty){
      list.head = currentCell.nextElem
      currentCell.nextElem = null
      currentCell = list.head
      while(!currentCell.isEmpty){
        currentCell.index -=1
        currentCell = currentCell.nextElem
      }
    }
    else if(idx > list.head.index && idx < list.last.index){
      while(currentCell.index!=idx){
        auxCell = currentCell
        currentCell = currentCell.nextElem
      }
      auxCell.nextElem = currentCell.nextElem
      auxCell = currentCell.nextElem
      currentCell.nextElem = null
      while(!auxCell.isEmpty){
        auxCell.index -=1
        auxCell = auxCell.nextElem
      }
    }
  }

  def removeValue(list : GenericList[T], value : T): Unit ={
    val idx : Int = getIdx(list, value)
    removeIndex(list, idx)
  }

  def deleteList(list : GenericList[T]): Unit = {
    list.head = new EmptyCell[T]
    list.last = list.head
  }

  def concatenateLists(list1 : GenericList[T] , list2 : GenericList[T]): Unit ={
    var currentCell : ListCell[T] = list2.head
    var i : Int = list1.size(list1)
    list1.last.nextElem = list2.head
    list1.last = list2.last
    deleteList(list2)
    while(!currentCell.isEmpty){
      i+=1
      currentCell.index = i
      currentCell = currentCell.nextElem
    }
  }

  def invertList(list : GenericList[T]) : Unit = {
    var currentCell : ListCell[T] = list.head
    var nextCell : ListCell[T] = list.head.nextElem
    var auxCell : ListCell[T] =list.last
    var i : Int = 1
    if(!currentCell.isEmpty){
      list.last = currentCell
      list.head = auxCell
      auxCell = nextCell.nextElem
      while(!nextCell.isEmpty){
        nextCell.nextElem = currentCell
        currentCell = nextCell
        nextCell = auxCell
        auxCell = auxCell.nextElem
      }
      list.last.nextElem = new EmptyCell[T]
      currentCell = list.head
      while(!currentCell.isEmpty){
        currentCell.index = i
        currentCell = currentCell.nextElem
        i+=1
      }
    }
  }
}

class GenericStack[T]{
  private var base : ListCell[T] = new EmptyCell[T]
  private var top : ListCell[T] = base

  def printStack (stack : GenericStack[T]) : Unit = {
    var currentCell : ListCell[T] = stack.base
    while (!currentCell.isEmpty){
      print("Cell: "+currentCell.index+"\tValue: "+currentCell.value+"\n")
      currentCell = currentCell.nextElem
    }
  }

  def deleteStack(stack : GenericStack[T]): Unit ={
    stack.base = new EmptyCell[T]
    stack.top = base
  }

  def top(stack : GenericStack[T]): T ={
    var currentCell : ListCell[T] = stack.top
    if(!stack.top.isEmpty){
      stack.top.value
    }
    currentCell.value
  }

  def push(stack: GenericStack[T], value : T): Unit ={
    if (stack.top.isEmpty) {
      stack.base = new ValueCell[T](value, 1)
      stack.top = stack.base
    }
    else {
      stack.top.nextElem = new ValueCell[T](value, stack.top.index+1)
      stack.top = stack.top.nextElem
    }
  }

  def pop(stack : GenericStack[T]) : Unit = {
    var currentCell : ListCell[T] = stack.base
    var auxCell : ListCell[T] = stack.base.nextElem
    if (auxCell.isEmpty){
      deleteStack(stack)
    }
    else if(!currentCell.isEmpty){
      while(auxCell!=stack.top){
        auxCell = auxCell.nextElem
        currentCell = currentCell.nextElem
      }
      stack.top = currentCell
      currentCell.nextElem = new EmptyCell[T]
    }
  }

  def size(stack: GenericStack[T]) : Int = {
    stack.top.index
  }
}

class GenericQueue[T]{
  private var first : ListCell[T] = new EmptyCell[T]
  private var last: ListCell[T] = first

  def printQueue(queue: GenericQueue[T]): Unit ={
    var currentCell : ListCell[T] = queue.first
    while(!currentCell.isEmpty){
      print("Cell: "+currentCell.index+"\tValue: "+currentCell.value+"\n")
      currentCell = currentCell.nextElem
    }
  }

  def deleteQueue (queue: GenericQueue[T]): Unit ={
    queue.first = new EmptyCell[T]
    queue.last = first
  }

  def size(queue : GenericQueue[T]): Int = {
    queue.last.index
  }

  def queue(queue: GenericQueue[T], value : T) : Unit = {
    if (queue.last.isEmpty) {
      queue.first = new ValueCell[T](value, 1)
      queue.last = queue.first
    }
    else {
      queue.last.nextElem = new ValueCell[T](value, queue.last.index+1)
      queue.last = queue.last.nextElem
    }
  }

  def dequeue(queue: GenericQueue[T]): Unit ={
    var currentCell : ListCell[T] = queue.first
    val idx : Int = 1

    if(currentCell.nextElem.isEmpty){
      deleteQueue(queue)
    }
    else if(idx == currentCell.index && !currentCell.isEmpty){
      queue.first = currentCell.nextElem
      currentCell.nextElem = null
      currentCell = queue.first
      while(!currentCell.isEmpty){
        currentCell.index -=1
        currentCell = currentCell.nextElem
      }
    }
  }

  def peek(queue: GenericQueue[T]) : T = {
    var currentCell : ListCell[T] = queue.first
    if(!currentCell.isEmpty) {
      queue.first.value
    }
    currentCell.value
  }

}