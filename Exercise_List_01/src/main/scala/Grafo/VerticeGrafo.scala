package Grafo

class VerticeGrafo[T](
                       _id: Int,
                       _dado: T = _,
                       _proxVertice: VerticeGrafo[T] = _,
                       _antVertice: VerticeGrafo[T] = _,
                       _primeiroArco: ArcoGrafo[T] = _
                     ) {
  var dado: T = _dado
  var proxVertice: VerticeGrafo[T] = _proxVertice
  var antVertice: VerticeGrafo[T] = _antVertice
  var primeiroArco: ArcoGrafo[T] = _primeiroArco

  def id: Int = _id

  def addArco(vertice: VerticeGrafo[T], dado: T): Unit = {
    val arco = new ArcoGrafo[T](vertice, dado)
    arco.proxArco = primeiroArco
    arco.proxArco.antArco = arco
    primeiroArco = arco
  }

  def getArco(end: VerticeGrafo[T]): Option[ArcoGrafo[T]] ={
    var iterador: ArcoGrafo[T] = primeiroArco
    while(iterador != null && iterador.vertice != end)
      iterador = iterador.proxArco
    if(iterador == null)
      return None
    Some(iterador)
  }
}
