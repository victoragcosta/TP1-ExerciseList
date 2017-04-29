package Enigma

class EnigmaSimples(private val interno: String, private val meio: String, private val externo: String) {
  var internoAtual: String = interno
  var meioAtual: String = meio
  var externoAtual: String = externo

  private def resetPos(): Unit = {internoAtual = interno; meioAtual = meio; externoAtual = externo}

  private def rodarHorario(anel: String): String = anel.charAt(anel.length-1) + anel.substring(0,anel.length-1)
  protected def rodarHorarioInterno(): Unit = {internoAtual = rodarHorario(internoAtual)}
  protected def rodarHorarioMeio(): Unit = {meioAtual = rodarHorario(meioAtual)}

//  private def rodarAntiHorario(anel: String): String = anel.substring(1,anel.length) + anel.charAt(0)
//  protected def rodarAntiHorarioInterno(): Unit = {internoAtual = rodarAntiHorario(internoAtual)}
//  protected def rodarAntiHorarioMeio(): Unit = {meioAtual = rodarAntiHorario(meioAtual)}

  protected def codifica(c: Char): Char = {
    if((c.toLower >= 'a' && c.toLower <= 'z') || c == ' '){
      var procurado: Char = '#'
      if(c != ' ') procurado = c.toUpper
      val achado: Char = externoAtual.charAt(meioAtual.indexOf(externoAtual.charAt(internoAtual.indexOf(procurado))))
      var retorno: Char = ' '
      if(achado != '#') retorno = achado
      return retorno
    } else {
      return c
    }
  }
  protected def decodifica(c: Char): Char = {
    if((c.toLower >= 'a' && c.toLower <= 'z') || c == ' ') {
      var procurado: Char = '#'
      if (c != ' ') procurado = c.toUpper
      val achado: Char = internoAtual.charAt(externoAtual.indexOf(meioAtual.charAt(externoAtual.indexOf(procurado))))
      var retorno: Char = ' '
      if (achado != '#') retorno = achado
      return retorno
    } else {
      return c
    }
  }

  protected def codificaChar(c: Char): Char = {
    var retorno = codifica(c)
    if(c >= 'a' && c <= 'z'){
      retorno = retorno.toLower
    } else if(c >= 'A' && c <= 'Z'){
      retorno = retorno.toUpper
    }
    this.rodarHorarioInterno()
    if(meioAtual == meio)
      this.rodarHorarioMeio()
    return retorno
  }

  protected def decodificaChar(c: Char): Char = {
    var retorno = decodifica(c)
    if(c >= 'a' && c <= 'z'){
      retorno = retorno.toLower
    } else if(c >= 'A' && c <= 'Z'){
      retorno = retorno.toUpper
    }
    this.rodarHorarioInterno()
    if(meioAtual == meio)
      this.rodarHorarioMeio()
    return retorno
  }

  def codificaString(texto: String): String = {internoAtual = interno; meioAtual = meio; texto.map(c => codificaChar(c))}
  def decodificaString(texto: String): String = {internoAtual = interno; meioAtual = meio; texto.map(c => decodificaChar(c))}

}
