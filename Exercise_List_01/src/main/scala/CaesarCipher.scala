

object CaesarCipher extends ShiftLetters {

  def Encode(OldString: String): String = OldString.map(c => ShiftRight(c, 3))

  def Decode(OldString: String): String = OldString.map(c => ShiftLeft(c, 3))

}
