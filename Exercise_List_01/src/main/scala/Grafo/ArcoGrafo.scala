package Grafo

class ArcoGrafo[T](
                    _vertice: VerticeGrafo[T],
                    _dado: T = _,
                    _proxArco: ArcoGrafo[T] = _,
                    _antArco: ArcoGrafo[T] = _
                  ) {
  var dado: T = _dado
  var proxArco: ArcoGrafo[T] = _proxArco
  var antArco: ArcoGrafo[T] = _antArco

  def vertice = _vertice
}
