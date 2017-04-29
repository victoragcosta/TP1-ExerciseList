package SimpleCipher

class RotationCipher(protected val c: Int) extends CaesarCipher(c) {

  /* Modifica o Caesar Cipher para mudar a chave toda vez que codifica
  *  @param c : caracter a ser codificado
  *  @return caracter codificado
  * */
  protected override def codificar(c : Char): Char = {
    val retorno = super.codificar(c)
    this.chave += 1
    return retorno
  }

  /* Modifica o Caesar Cipher para mudar a chave toda vez que decodifica
  * @param c : caracter a ser decodificado
  * @return caracter decodificado
  * */
  protected override def decodificar(c: Char): Char = {
    val retorno = super.decodificar(c)
    this.chave += 1
    return retorno
  }

  /** Codifica uma string usando Rotation Cipher
    * @param texto : string a ser codificada
    * @return string codificada
    * */
  override def codificaString(texto : String): String = {
    val retorno = texto.map(c => codificar(c))
    this.chave = this.c
    return retorno
  }

  /** Descodifica uma string usando Rotation Cipher
    * @param texto : string a ser descodificada
    * @return string descodificada
    * */
  override def decodificarString(texto: String): String = {
    val retorno = texto.map(c => decodificar(c))
    this.chave = this.c
    return retorno
  }
}
