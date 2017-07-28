import io.Source

object WLP4Gen {
  var terminalSet:Set[String] = Set("BOF", "BECOMES", "COMMA", "ELSE", "EOF", "EQ", "GE", "GT", "ID", "IF", "INT", "LBRACE", "LE", "LPAREN", "LT", "MINUS", "NE", "NUM", "PCT", "PLUS", "PRINTLN", "RBRACE", "RETURN", "RPAREN", "SEMI",
    "SLASH", "STAR", "WAIN", "WHILE", "AMP", "LBRACK", "RBRACK", "NEW", "DELETE", "NULL")

  var symbolTable: Map[String, (String, String)] = Map()
  var stackLocation:Int = 0
  var labelCount = 0


  case class Node(value: String, childList: List[Node], syntax:String, prule:String) {
    var children:List[Node] = childList
    var synt = syntax
    var nodeType = value
    var rule = prule

  }

  def buildParseTree(in: Iterator[String]): Node = {
    var prule = in.next()
    var curInput = prule.split(" ")
    var newchildList = List()
    if(terminalSet.contains(curInput.head)) {
      var terminalNode = Node(curInput.head, newchildList, curInput.tail.head, prule)
      return terminalNode
    }
    else {
      var nonterminalNode = Node(curInput.head, newchildList, "", prule)
      for (x <- (1 to curInput.tail.length)) {
        nonterminalNode.children = nonterminalNode.children :+ buildParseTree(in)
      }
      return nonterminalNode
    }
  }

  def traverseMultipleDeclarations(root:Node): Unit = {
    if (root.children.isEmpty) {
      return
    }
    else if (root.rule == "dcl type ID") {
      var mapID = root.children.tail.head.synt
      var maptype = ""
      root.children.head.children.foreach(x => {
        maptype = maptype + x.synt
      })
      if (symbolTable.contains(mapID)) {
        System.err.println("ERROR: already exists in symbolTable")
        return
      }
      else {
        symbolTable = symbolTable + (mapID -> (maptype, stackLocation.toString))
        if(stackLocation == -4) {
          stackLocation -=8
        }
        else {
          stackLocation -= 4
        }
      }
    }
    else {
      for (x <- (0 to root.children.length - 1)) {
        traverseMultipleDeclarations(root.children(x))
      }
    }
  }

  def traverseUndeclared(root:Node): Unit = {
    if(root.children.isEmpty) {
      return
    }
    else if (root.rule == "factor ID" || root.rule == "statement ID BECOMES expr SEMI") {
      for (x <- root.children) {
        if (x.nodeType == "ID" && !symbolTable.contains(x.synt)){
          System.err.println("ERROR: use before declaration")
          return
        }
      }
    }
    else {
      for (x <- (0 to root.children.length - 1)) {
        traverseUndeclared(root.children(x))
      }
    }
  }

  def typeChecking(root:Node):Unit = {
    if(root.children.isEmpty) {
      return
    }
    else {
      for (x <- root.children) {
        if (x.nodeType == "expr" || x.nodeType == "lvalue" || x.nodeType == "statements" || x.nodeType == "test"
          || x.nodeType == "dcls" || x.nodeType == "dcl" || x.nodeType == "main") {
          if (getType(x) == "") {
            return
          }
        }
        typeChecking(x)
      }
    }
  }

  def codeGeneration(root:Node): Unit = {
    if(root.children.isEmpty) {
      return
    }
    else {
      for (x <- root.children) {
        if(!terminalSet.contains(x.nodeType)) {
          proceduresCode(x)
        }
      }
    }
  }

  def getType(root:Node):String = {
    root.prule match {
      case "factor NUM" => return "int"
      case "factor NULL" => return "int*"
      case "factor ID" | "lvalue ID" => {
        var mapID = root.children.head.synt
        return symbolTable(mapID)._1.toString
      }
      case "factor LPAREN expr RPAREN" | "lvalue LPAREN lvalue RPAREN" => {
        return getType(root.children.tail.head)
      }
      case "factor AMP lvalue" => {
        var typeString = getType(root.children.last)
        if(typeString == "int") {
          return "int*"
        }
        else {
          System.err.println("ERROR:address of non int")
          return ""
        }
      }
      case "factor STAR factor" | "lvalue STAR factor" => {
        var typeString = getType(root.children.last)
        if (typeString == "int*") {
          return "int"
        }
        else {
          System.err.println("ERROR:dereferencing of non int*")
          return ""
        }
      }
      case "factor NEW INT LBRACK expr RBRACK" => {
        var typeString = getType(root.children(3))
        if(typeString == "int") {
          return "int*"
        }
        else {
          System.err.println("ERROR:allocation is incorrect")
          return ""
        }
      }
      case "term factor" | "expr term" => {
        return getType(root.children.head)
      }
      case "term term STAR factor" | "term term SLASH factor" | "term term PCT factor" => {
        var leftType = getType(root.children.head)
        var rtype = getType(root.children.last)
        if (leftType == "int" && rtype == "int") {
          return "int"
        }
        else {
          System.err.println("ERROR:multiplication/division/modulo of non ints")
          return ""
        }
      }
      case "expr expr PLUS term" => {
        var leftType = getType(root.children.head)
        var rtype = getType(root.children.last)
        leftType match {
          case "int" => {
            rtype match {
              case "int" => return "int"
              case "int*" => return "int*"
            }
          }
          case "int*" => {
            rtype match {
              case "int" => return "int*"
              case _ => {
                System.err.println("ERROR:addition of two pointers")
                return ""
              }
            }
          }
        }
      }
      case "expr expr MINUS term" => {
        var leftType = getType(root.children.head)
        var rtype = getType(root.children.last)
        leftType match {
          case "int" => {
            rtype match {
              case "int" => return "int"
              case "int*" => {
                System.err.println("ERROR:addition of two pointers")
                return ""
              }
            }
          }
          case "int*" => {
            rtype match {
              case "int" => return "int*"
              case "int*" => return "int"
            }
          }
        }
      }
      case "statements" => {
        return "correct"
      }
      case "statements statements statement" => {
        var typeCheck1 = getType(root.children.head)
        var typeCheck2 = getType(root.children.tail.head)
        if (typeCheck1 == "" || typeCheck2 == "") {
          System.err.println("ERROR:statement")
          return ""
        }
        else {
          return "correct"
        }
      }
      case "statement lvalue BECOMES expr SEMI" => {
        var leftType = getType(root.children.head)
        var rtype = getType(root.children(2))
        if (leftType == rtype) {
          return leftType
        }
        else {
          System.err.println("ERROR:= wrong")
          return ""
        }

      }
      case "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE" => {
        var check1 = getType(root.children(2))
        var check2 = getType((root.children(5)))
        var check3 = getType(root.children(9))
        if(check1 == "" || check2 == "" || check3 == "") {
          System.err.println("ERROR:if wrong")
          return ""
        }
        else {
          return "correct"
        }
      }
      case "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE" => {
        var check1 = getType(root.children(2))
        var check2 = getType((root.children(5)))
        if(check1 == "" || check2 == "") {
          System.err.println("ERROR:while wrong")
          return ""
        }
        else {
          return "correct"
        }
      }
      case "statement PRINTLN LPAREN expr RPAREN SEMI" => {
        var check1 = getType(root.children(2))
        if(check1 != "int") {
          System.err.println("ERROR:println wrong")
          return ""
        }
        else {
          return "correct"
        }
      }
      case "statement DELETE LBRACK RBRACK expr SEMI" => {
        var check1 = getType(root.children(3))
        if(check1 != "int*") {
          System.err.println("ERROR:dealloc wrong")
          return ""
        }
        else {
          return "correct"
        }
      }
      case "test expr EQ expr" | "test expr NE expr" | "test expr GE expr" |
           "test expr LE expr" | "test expr LT expr" | "test expr GT expr"
      => {
        var leftType = getType(root.children.head)
        var rtype = getType(root.children.last)
        if (leftType == rtype) {
          return leftType
        }
        else {
          System.err.println("ERROR:inequality wrong")
          return ""
        }
      }
      case "dcls" => {
        return "correct"
      }
      case "dcls dcls dcl BECOMES NUM SEMI" => {
        var check1 = getType(root.children.head)
        var check2 = getType(root.children(1))

        if (check1 == "" || check2 != "int") {
          System.err.println("ERROR:dcls error")
          return ""
        }
        else{
          return "correct"
        }
      }
      case "dcls dcls dcl BECOMES NULL SEMI" => {
        var check1 = getType(root.children.head)
        var check2 = getType(root.children(1))

        if (check1 == "" || check2 != "int*") {
          System.err.println("ERROR:dcls error")
          return ""
        }
        else{
          return "correct"
        }
      }
      case "dcl type ID" => {
        var mapID = root.children.tail.head.synt
        return symbolTable(mapID)._1.toString
      }
      case "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE" =>{
        var dcl2 = getType(root.children(5))
        var dcls = getType(root.children(8))
        var statements = getType(root.children(9))
        var expr = getType(root.children(11))
        if(dcl2 != "int" | dcls == "" | statements == "" | expr != "int") {
          System.err.println("ERROR:main wrong")
          return ""
        }
        else {
          return "correct"
        }
    }
      case _ => {
        return ""
      }
    }
  }

  def pop(register:String): Unit = {
    println("add $30, $30, $4")
    println("lw " + register + ", -4($30)")

  }

  def push(register:String): Unit = {
    println("sw " + register + ", -4($30)")
    println("sub $30, $30, $4")
  }

  //frame pointer setup
  def framePointerCode(): Unit = {
    println(".import print")
    println(".import init")
    println(".import new")
    println(".import delete")
    println("lis $4")
    println(".word 4")
    println("sub $29, $30, $4")
    println("lis $11")
    println(".word 1")
  }

  def prolog(): Unit = {
    push("$1")
    push("$2")
    push("$31")
  }

  def proceduresCode(root:Node): Unit = {
    root.rule match {
      case "procedures main" => {
        framePointerCode()
        prolog()
        mainCode(root.children.head)
      }
    }
    println("add $30, $29, $4")
    println("lw $31, -12($30)")
    println("jr $31")
  }

  def mainCode(root:Node): Unit = {
    root.rule match {
      case "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE" => {
        if(getType(root.children(3)) == "int") {
          println("add $2, $0, $0")
        }
        println("lis $10")
        println(".word init")
        println("jalr $10")
        dclCode(root.children(3), false)
        dclCode(root.children(5), false)
        dclsCode(root.children(8))
        statementsCode(root.children(9))
        exprCode(root.children(11))
      }
    }
  }

  def exprCode(root:Node): Unit = {
    root.rule match {
      case "expr term" => {
        termCode(root.children.head)
      }
      case "expr expr PLUS term" => {
        var type1 = getType(root.children.head)
        var type2 = getType(root.children.last)
        if(type1 == "int" && type2 == "int") {
          exprCode(root.children.head)
          push("$3")
          termCode(root.children.last)
          pop("$5")
          println("add $3, $5, $3")
        }
        else if(type1 == "int*" && type2 == "int") {
          exprCode(root.children.head)
          push("$3")
          termCode(root.children.last)
          println("mult $3, $4")
          println("mflo $3")
          pop("$5")
          println("add $3, $5, $3")
        }
        else if(type2 == "int*" && type1 == "int") {
          exprCode(root.children.head)
          push("$3")
          termCode(root.children.last)
          pop("$5")
          println("mult $5, $4")
          println("mflo $5")
          println("add $3, $5, $3")
        }
      }
      case "expr expr MINUS term" => {
        var type3 = getType(root.children.head)
        var type4 = getType(root.children.last)
        if(type3 == "int" && type4 == "int") {
          exprCode(root.children.head)
          push("$3")
          termCode(root.children.last)
          pop("$5")
          println("sub $3, $5, $3")
        }
        else if(type3 == "int*" && type4 == "int") {
          exprCode(root.children.head)
          push("$3")
          termCode(root.children.last)
          println("mult $3, $4")
          println("mflo $3")
          pop("$5")
          println("sub $3, $5, $3")
        }
        else if(type3 == "int*" && type4 == "int*") {
          exprCode(root.children.head)
          push("$3")
          termCode(root.children.last)
          pop("$5")
          println("sub $3, $5, $3")
          println("div $3, $4")
          println("mflo $3")
        }
      }
    }
  }

  def termCode(root:Node): Unit = {
    root.rule match {
      case "term factor" => {
        factorCode(root.children.head)
      }
      case "term term STAR factor" => {
        termCode(root.children.head)
        push("$3")
        factorCode(root.children.last)
        pop("$5")
        println("mult $5, $3")
        println("mflo $3")
      }
      case "term term SLASH factor" => {
        termCode(root.children.head)
        push("$3")
        factorCode(root.children.last)
        pop("$5")
        println("div $5, $3")
        println("mflo $3")
      }
      case "term term PCT factor" => {
        termCode(root.children.head)
        push("$3")
        factorCode(root.children.last)
        pop("$5")
        println("div $5, $3")
        println("mfhi $3")
      }
    }
  }

  def factorCode(root:Node): Unit = {
    root.rule match {
      case "factor ID" => {
        println("lw $3, " + symbolTable(root.children.head.synt)._2.toString + "($29)")
      }
      case "factor LPAREN expr RPAREN" => {
        exprCode(root.children(1))
      }
      case "factor NUM" => {
        println("lis $3")
        println(".word " + root.children.head.synt)
      }
      case "factor STAR factor" => {
        factorCode(root.children.last)
        println("lw $3, 0($3)")
      }
      case "factor NULL" => {
        println("add $3, $0, $11")
      }
      case "factor AMP lvalue" => {
        lvalueCode(root.children.last, true)
      }
      case "factor NEW INT LBRACK expr RBRACK" => {
        var genLabel = label(false)
        exprCode(root.children(3))
        println("add $1, $3, $0")
        println("lis $10")
        println(".word new")
        println("jalr $10")
        println("bne $3, $0, " + genLabel)
        println("add $3, $0, $11")
        println(genLabel + ":")
      }
    }
  }

  def testCode(root:Node): Unit = {
    var pointerOrInt = getType(root.children.head)
    var unsigned = ""
    if(pointerOrInt == "int*") {
      unsigned = "sltu"
    }
    else {
      unsigned = "slt"
    }
    root.rule match {
      case "test expr LT expr" => {
        exprCode(root.children.head)
        push("$3")
        exprCode(root.children.last)
        pop("$5")
        println(unsigned + " $3, $5, $3")
      }
      case "test expr GT expr" => {
        exprCode(root.children.head)
        push("$3")
        exprCode(root.children.last)
        pop("$5")
        println(unsigned + " $3, $3, $5")
      }
      case "test expr GE expr" => {
        exprCode(root.children.head)
        push("$3")
        exprCode(root.children.last)
        pop("$5")
        println(unsigned + " $3, $5, $3")
        println("sub $3, $11, $3")
      }
      case "test expr LE expr" => {
        exprCode(root.children.head)
        push("$3")
        exprCode(root.children.last)
        pop("$5")
        println(unsigned + " $3, $3, $5")
        println("sub $3, $11, $3")
      }
      case "test expr NE expr" => {
        exprCode(root.children.head)
        push("$3")
        exprCode(root.children.last)
        pop("$5")
        println(unsigned + " $6, $3, $5")
        println(unsigned + " $7, $5, $3")
        println("add $3, $6, $7")
      }
      case "test expr EQ expr" => {
        exprCode(root.children.head)
        push("$3")
        exprCode(root.children.last)
        pop("$5")
        println(unsigned + " $6, $3, $5")
        println(unsigned + " $7, $5, $3")
        println("add $3, $6, $7")
        println("sub $3, $11, $3")
      }
    }
  }

  def label(start:Boolean): String = {
    var future = ""
    if(start) {
      future = "S" + labelCount
    }else {
      future = "E" + labelCount
    }
    labelCount += 1
    return future
  }

  def statementCode(root:Node): Unit = {
    root.rule match {
      case "statement PRINTLN LPAREN expr RPAREN SEMI" => {
        exprCode(root.children(2))
        println("add $1, $3, $0")
        println("lis $10")
        println(".word print")
        println("jalr $10")
      }
      case "statement lvalue BECOMES expr SEMI" => {
        if(root.children.head.children.head.nodeType == "STAR") {
          exprCode(root.children(2))
          push("$3")
          lvalueCode(root.children.head, false)
          pop("$5")
          println("sw $5, 0($3)")
        }
        else {
          exprCode(root.children(2))
          lvalueCode(root.children.head, false)
        }
      }
      case "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE" => {
        var startLabel = label(true)
        var endLabel = label(false)
        println(startLabel + ":")
        testCode(root.children(2))
        println("beq $3, $0, " + endLabel)
        statementsCode(root.children(5))
        println("beq $0, $0, " + startLabel)
        println(endLabel + ":")
      }
      case "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE" => {
        var sLabel = label(true)
        var eLabel = label(false)
        testCode(root.children(2))
        println("beq $3, $0, " + sLabel)
        statementsCode(root.children(5))
        println("beq $0, $0, " + eLabel)
        println(sLabel +":")
        statementsCode(root.children(9))
        println(eLabel + ":")
      }
      case "statement DELETE LBRACK RBRACK expr SEMI" => {
        exprCode(root.children(3))
        var lookup = root.children(3).children.head.children.head.children.head.synt
        var offset = symbolTable(lookup)._2
        var genLabel = label(false)
        println("beq $11, $3, " + genLabel)
        println("lw $1, " + offset + "($29)")
        println("lis $10")
        println(".word delete")
        println("jalr $10")
        println(genLabel + ":")
      }
    }
  }

  def lvalueCode(root:Node, pointer:Boolean): Unit = {
    root.rule match {
      case "lvalue ID" => {
        if(pointer) {
          println("lis $3")
          println(".word " + symbolTable(root.children.last.synt)._2.toString)
          println("add $3, $3, $29")
        }
        else {
          println("sw $3, " + symbolTable(root.children.last.synt)._2.toString + "($29)")
          println("sub $30, $30, $4")
        }
      }
      case "lvalue LPAREN lvalue RPAREN"=> {
        lvalueCode(root.children(1), false)
      }
      case "lvalue STAR factor" => {
        factorCode(root.children.last)
      }
    }
  }

  def statementsCode(root:Node): Unit = {
    root.rule match {
      case "statements" => {}
      case "statements statements statement" => {
        statementsCode(root.children.head)
        statementCode(root.children.last)
      }
    }
  }

  def dclsCode(root:Node): Unit = {
    root.rule match {
      case "dcls" => {}
      case "dcls dcls dcl BECOMES NUM SEMI" => {
        dclsCode(root.children.head)
        println("lis $3")
        println(".word " + root.children(3).synt)
        dclCode(root.children(1), true)
      }
      case "dcls dcls dcl BECOMES NULL SEMI" => {
        dclsCode(root.children.head)
        dclCode(root.children(1), true)
      }
    }
  }

  def dclCode(root:Node, declaration:Boolean): Unit = {
    root.rule match {
      case "dcl type ID" => {
        typeCode(root.children.head)
        if(declaration) {
          println("sw $3, " + symbolTable(root.children.last.synt)._2.toString + "($29)")
          println("sub $30, $30, $4")
        }
      }
    }
  }

  def typeCode(root:Node): Unit = {
    root.rule match {
      case "type INT" => {}
      case "type INT STAR" => {}
    }
  }


  def main(args: Array[String]) {
    val in = Source.fromInputStream(System.in).getLines
    var newTree = buildParseTree(in)
    traverseMultipleDeclarations(newTree)
    traverseUndeclared(newTree)
    typeChecking(newTree)
    codeGeneration(newTree)
  }
}