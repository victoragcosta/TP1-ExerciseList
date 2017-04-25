

object EnigmaCipher extends ShiftLetters{

  val UpperOuterRing: List[Char] = List(' ', 'B', 'D', 'F', 'H', 'J', 'L', 'N', 'P',
    'R', 'T', 'V', 'X', 'Z', 'A', 'C', 'E', 'G', 'I', 'K', 'M', 'O', 'Q', 'S', 'U', 'W', 'Y')

  val UpperMiddleRing: List[Char] = List(' ', 'E', 'J', 'O', 'T', 'Y', 'C', 'H', 'M',
    'R', 'W', 'A', 'F', 'K', 'P', 'U', 'Z', 'D', 'I', 'N', 'S', 'X', 'B', 'G', 'L', 'Q', 'V')

  val UpperInnerRing: List[Char] = List(' ', 'G', 'N', 'U', 'A', 'H', 'O', 'V', 'B',
    'I', 'P', 'W', 'C', 'J', 'Q', 'X', 'D', 'K', 'R', 'Y', 'E', 'L', 'S', 'Z', 'F', 'M', 'T')

  val LowerOuterRing: List[Char] = UpperOuterRing.map(c => c.toLower)

  val LowerMiddleRing: List[Char] = UpperMiddleRing.map(c => c.toLower)

  val LowerInnerRing: List[Char] = UpperInnerRing.map(c => c.toLower)

  def EncodeChar(c: Char, InnerShift: Int, MiddleShift: Int): Char = {

    val letter =

      if (LowerInnerRing.contains(c)) {
        LowerOuterRing(SimplifyShift(LowerMiddleRing.indexOf(LowerOuterRing(SimplifyShift(LowerInnerRing.indexOf(c) + InnerShift))) + MiddleShift))
      }

      else if (UpperInnerRing.contains(c)) {
        UpperOuterRing(SimplifyShift(UpperMiddleRing.indexOf(UpperOuterRing(SimplifyShift(UpperInnerRing.indexOf(c) + InnerShift))) + MiddleShift))
      }

      else {
        c
      }

    letter

  }

  def DecodeChar(c: Char, InnerShift: Int, MiddleShift: Int): Char = {

    val letter =

      if (LowerInnerRing.contains(c)) {
        LowerInnerRing(SimplifyShift(LowerOuterRing.indexOf(LowerMiddleRing(SimplifyShift(LowerOuterRing.indexOf(c) - MiddleShift))) - InnerShift))
      }

      else if (UpperInnerRing.contains(c)) {
        UpperInnerRing(SimplifyShift(UpperOuterRing.indexOf(UpperMiddleRing(SimplifyShift(UpperOuterRing.indexOf(c) - MiddleShift))) - InnerShift))
      }

      else {
        c
      }

    letter

  }

  def Encode(OldString: String, InicialInnerShift: Int, InicialMiddleShift: Int): String = {

    var NewString = ""

    var InnerShift = InicialInnerShift

    var MiddleShift = InicialMiddleShift

    for (c <- OldString) {

      NewString += EnigmaCipher.EncodeChar(c, InnerShift, MiddleShift)

      if (LowerCase.contains(c) || UpperCase.contains(c) || c == ' ')
        InnerShift += 1

      if (InnerShift == 27) {
        InnerShift = 0
        MiddleShift += 1
      }

      if (MiddleShift == 27)
        MiddleShift = 0

    }

    NewString

  }

  def Decode(OldString: String, InicialInnerShift: Int, InicialMiddleShift: Int): String = {

    var NewString = ""

    var InnerShift = InicialInnerShift

    var MiddleShift = InicialMiddleShift

    for (c <- OldString) {

      NewString += EnigmaCipher.DecodeChar(c, InnerShift, MiddleShift)

      if (LowerCase.contains(c) || UpperCase.contains(c) || c == ' ')
        InnerShift += 1

      if (InnerShift == 27) {
        InnerShift = 0
        MiddleShift += 1
      }

      if (MiddleShift == 27)
        MiddleShift = 0

    }

    NewString

  }

  def SimplifyShift (Shift: Int): Int = {

    var Auxiliary = Shift

    while (Auxiliary < 0)
      Auxiliary += 27

    Auxiliary = Auxiliary % 27

    Auxiliary

  }

}
