package Grafo

class Grafo[T] {
  var primeiroVertice: VerticeGrafo[T] = _

  /* Funções de Vértice */

  /**
    * Checa se há vértices no Grafo.
    * @return Um Boolean em que true significa vazio.
    */
  def vazio: Boolean = this.primeiroVertice == null

  /**
    * Retorna o Vértice com o id passado.
    * @param id Id do vértice desejado.
    * @return Objeto do vértice.
    */
  private def getVertice(id: Int): Option[VerticeGrafo[T]] = {
    var iterator: VerticeGrafo[T] = primeiroVertice
    while((iterator != null)&&(iterator.id != id))
      iterator = iterator.proxVertice
    if(iterator == null)
      return None
    Some(iterator)
  }

  /**
    * Retorna o dado do vértice do id passado.
    * @param id Id do vértice da qual se quer os dados.
    * @return Dados requisitados em forma de Option.
    */
  def getVerticeData(id: Int): Option[T] = {
    val optVertice = getVertice(id)
    if(optVertice.isDefined){
      val vertice = optVertice.orNull
      return Some(vertice.dado)
    }
    None
  }

  /**
    * Edita o dado do vértice do id passado.
    * @param id Id do vértice da qual se quer editar os dados.
    * @return Antigos dados em forma de Option.
    */
  def setVerticeData(id: Int, dado: T): Option[T] = {
    var old: Option[T] = None
    val optVertice = getVertice(id)
    if(optVertice.isDefined){
      val vertice = optVertice.orNull
      old = Some(vertice.dado)
      vertice.dado = dado
    }
    old
  }

  /**
    * Adiciona um vértice na estrutura do Grafo.
    * @param id id a atribuir ao Vértice
    * @param dado Objeto a qual foi parametrizado seu grafo que representa seu dado.
    */
  def addVertice(id: Int, dado: T): Unit = {
    if(getVertice(id).isEmpty){
      val novoVertice = new VerticeGrafo[T](id, dado, this.primeiroVertice)
      primeiroVertice = novoVertice
      novoVertice.proxVertice.antVertice = primeiroVertice
    }
  }

  /**
    * Deleta o Vértice com o id fornecido do grafo.
    * @param id Id do vértice.
    * @return O dado dentro do Vértice.
    */
  def delVertice(id: Int): Option[T] = {
    val optDeletar: Option[VerticeGrafo[T]] = getVertice(id)
    if(optDeletar.isDefined){
      val deletar: VerticeGrafo[T] = optDeletar.orNull
      deletar.antVertice.proxVertice = deletar.proxVertice
      deletar.proxVertice.antVertice = deletar.antVertice
      val dado = deletar.dado
      //deletar.finalize()
      return Some(dado)
    }
    None
  }

  /* Funções de Arcos */

  /**
    * Adiciona um arco no grafo saindo de start e indo para end.
    * @param start Vértice de onde sai o arco.
    * @param end Vértice para onde o arco aponta.
    * @param dado Dado inserido no arco novo.
    */
  def addArco(start: Int, end: Int, dado: T): Unit = {
    if(getArco(start, end).isEmpty){
      val optVerticeInicio = getVertice(start)
      val optVerticeFim = getVertice(end)
      if(optVerticeInicio.isDefined && optVerticeFim.isDefined){
        val verticeInicio = optVerticeInicio.orNull
        val verticeFim = optVerticeFim.orNull
        verticeInicio.addArco(verticeFim, dado)
      }
    }
  }

  /**
    * Consegue a referência do arco que sai do Vértice de índice start e aponta para o índice end.
    * @param start De onde sai o arco.
    * @param end Para onde aponta o arco.
    * @return Option contendo ou não o arco procurado. Se for None, não existe o arco.
    */
  private def getArco(start: Int, end: Int): Option[ArcoGrafo[T]] = {
    val optVerticeInicio = getVertice(start)
    val optVerticeFim = getVertice(end)
    if(optVerticeInicio.isDefined && optVerticeFim.isDefined){
      val verticeInicio = optVerticeInicio.orNull
      val verticeFim = optVerticeFim.orNull
      val arco = verticeInicio.getArco(verticeFim)
      return arco
    }
    None
  }

  /**
    * Retorna a referência do dado contido no arco que sai de start e aponta para end.
    * @param start Id do vértice contendo o arco.
    * @param end Id do vértice para o qual o arco aponta.
    * @return Option contendo ou não T. None significa a não existência do arco.
    */
  def getArcoData(start: Int, end: Int): Option[T] = {
    val optArco = getArco(start, end)
    if(optArco.isDefined){
      val arco = optArco.orNull
      return Some(arco.dado)
    }
    None
  }

  /**
    * Muda o dado dentro do arco de inicio start e aponta para end com o dado passado.
    * @param start Id do vértice contendo o arco.
    * @param end Id do vértice para onde aponta o arco.
    * @param dado Dado do tipo do grafo para tomar o lugar.
    * @return Option com o dado antigo do tipo do grafo. None significa a falta do arco ou o dado null.
    */
  def setArcoData(start: Int, end: Int, dado: T): Option[T] = {
    val optArco = getArco(start, end)
    if(optArco.isDefined){
      val arco = optArco.orNull
      val old = arco.dado
      arco.dado = dado
      return Some(old)
    }
    None
  }

  /**
    * Remove o arco do vértice start que aponta para end.
    * @param start Id do vértice contendo o arco.
    * @param end Id do vértice para qual o arco aponta.
    * @return Option contendo ou não o dado antigo do arco.
    */
  def delArco(start: Int, end: Int): Option[T] = {
    val optArco = getArco(start, end)
    if(optArco.isDefined){
      val arco = optArco.orNull
      val dado = arco.dado
      arco.antArco = arco.proxArco
      arco.proxArco = arco.antArco
      return Some(dado)
    }
    None
  }
}
