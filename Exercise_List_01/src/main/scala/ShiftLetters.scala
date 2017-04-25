

trait ShiftLetters {

  val UpperCase: List[Char] = List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
    'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')

  val LowerCase: List[Char] = UpperCase.map(c => c.toLower)

  def ShiftRight(c: Char, n: Int): Char = {

    val letter =

      if (LowerCase.contains(c)) {
        LowerCase((n + LowerCase.indexOf(c)) % 26)
      }

      else if (UpperCase.contains(c)) {
        UpperCase((n + UpperCase.indexOf(c)) % 26)
      }

      else {
        c
      }

    return letter

  }

  def ShiftLeft(c: Char, n: Int): Char = { ShiftRight(c, 26 - (n % 26)) }

}
