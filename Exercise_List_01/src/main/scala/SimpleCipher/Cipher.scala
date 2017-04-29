package SimpleCipher

import scala.io.StdIn

object Cipher {
  def main(args: Array[String]): Unit = {
    val codificador: CaesarCipher = new RotationCipher(3)

    var op: Int = -1
    while(op != 0){
      println("Escolha:")
      println("1) codificar;")
      println("2) decodificar;")
      println("0) sair.")
      op = StdIn.readInt()

      if(op == 0){
        println("Tchaaaaaau!")
      } else if(op == 1){
        print("String para codificar: ")
        val textao: String = StdIn.readLine()
        val codificado: String = codificador.codificaString(textao)
        println("Codificado: " + codificado)
      } else if(op == 2){
        print("String para decodificar: ")
        val textao: String = StdIn.readLine()
        val decodificado: String = codificador.decodificarString(textao)
        println("Decodificado: " + decodificado)
      } else {
        println("Comando inválido, tente novamente!")
      }
    }

  }
}
