package Enigma

import scala.io.StdIn

object EnigmaEnigmatico {
  def main(args: Array[String]): Unit = {
    val anelInterno: String = "#GNUAHOVBIPWCJQXDKRYELSZFMT"
    val anelMediano: String = "#EJOTYCHMRWAFKPUZDINSXBGLQV"
    val anelExterno: String = "#BDFHJLNPRTVXZACEGIKMOQSUWY"
    val enigmaSimples: EnigmaSimples = new EnigmaSimples(anelInterno, anelMediano, anelExterno)

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
        val codificado: String = enigmaSimples.codificaString(textao)
        println("Codificado: " + codificado)
      } else if(op == 2){
        print("String para decodificar: ")
        val textao: String = StdIn.readLine()
        val decodificado: String = enigmaSimples.decodificaString(textao)
        println("Decodificado: " + decodificado)
      } else {
        println("Comando inválido, tente novamente!")
      }
    }
  }
}
