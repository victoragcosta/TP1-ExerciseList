

object RotationCipher extends ShiftLetters {

  def Encode (OldString: String, InicialShift: Int): String = {

    var NewString = ""
    var SimplifiedInicialShift = SimplifyShift(InicialShift)
    var Counter = 0

    for (c <- OldString) {

      NewString += ShiftRight(c, SimplifiedInicialShift + Counter)

      if (LowerCase.contains(c) || UpperCase.contains(c))
        Counter += 1

    }

    NewString

  }

  def Decode (OldString: String, InicialShift: Int): String = {

    var NewString = ""
    var SimplifiedInicialShift = SimplifyShift(InicialShift)
    var Counter = 0

    for (c <- OldString) {

      NewString += ShiftLeft(c, SimplifiedInicialShift + Counter)

      if (LowerCase.contains(c) || UpperCase.contains(c))
        Counter += 1

    }

    NewString

  }

  def SimplifyShift (Shift: Int): Int = {

    var Auxiliary = Shift

    while (Auxiliary < 0)
      Auxiliary += 26

    Auxiliary = Auxiliary % 26

    Auxiliary

  }

}