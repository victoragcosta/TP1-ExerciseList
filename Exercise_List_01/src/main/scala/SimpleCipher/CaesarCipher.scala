package SimpleCipher

class CaesarCipher(protected var chave : Int = 3) {
  
  private val indiceInicial = 'a'.toInt
  private val indiceFinal = 'z'.toInt
  
  /* tamanho do alfabeto: quantidade de letras minusculas */ 
  private val alfabeto = 1 + indiceFinal - indiceInicial
  
  /* Codifica um caracter usando o algoritmos CaesarCipher
   *  @param c caracter a ser codificado
   *  @return caracter cifrado com o  algoritmo
   */
  protected def codificar(c : Char) : Char = {
    //require(c >= 'a' && c <= 'z')
    var retorno : Char = 'a'
    if(c >= 'a' && c <= 'z'){
      retorno = (indiceInicial + ((c.toInt + this.chave - indiceInicial) % alfabeto)).toChar
    } else {
      retorno = c
    }
    return retorno
  }
  
  /* Decodifica um caracter usando o algoritmo CaesarCipher
   * @param c caracter a ser decifrado
   * @return caracter decifrado usando o algoritmo
   */
  protected def decodificar(c : Char) : Char = {
    //require(c >= 'a' && c <= 'z')
    var retorno : Char = 'a'
    if(c >= 'a' && c <= 'z'){
      retorno = (indiceFinal - ((indiceFinal - (c.toInt - this.chave)) % alfabeto)).toChar
    } else {
      retorno = c
    }
    return retorno
  }
  
  /** Codifica uma string de acordo com o algoritmo CaesarCipher
   *  @param texto : texto a ser cifrado
   *  @return texto codificado de acordo com o algoritmo
   */
  def codificaString(texto : String) : String = texto.map(c => codificar(c))
  
  def decodificarString(texto : String) : String = texto.map(c => decodificar(c))
 
}