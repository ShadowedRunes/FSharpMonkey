module FSharpMonkeyTests

open System.Text.Json
open FSharpMonkey
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestNextToken () =
    let input = "=+(){},;"
    let expectedToken = [TokenType.Assign; TokenType.Plus; TokenType.LParen
                         TokenType.RParen; TokenType.LBrace; TokenType.RBrace;
                         TokenType.Comma; TokenType.SemiColon]
    let mutable outputList = []
    let lexer = Lexer.newLexer input
    let mutable aChar = Lexer.nextToken lexer
    while aChar <> TokenType.EOF do
        let temp = Lexer.peek lexer
        outputList <- aChar :: outputList
        aChar <- Lexer.nextToken lexer
        
    Assert.True((expectedToken = (List.rev outputList)))
    
[<Test>]
let TestNextTokenAdvanced() =
    let input = "let five = 5; \
        let ten = 10; \
        let add = fn(x, y) { \
        x + y; \
        }; \
        == !=
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
          return true;
        } else {
          return false;
        }
        "
        
    let expectedTokens = [TokenType.Let; TokenType.Ident("five"); TokenType.Assign;TokenType.Int(5); TokenType.SemiColon;
                          TokenType.Let; TokenType.Ident("ten"); TokenType.Assign;TokenType.Int(10); TokenType.SemiColon
                          TokenType.Let; TokenType.Ident("add"); TokenType.Assign;TokenType.Function;TokenType.LParen;
                          TokenType.Ident("x"); TokenType.Comma; TokenType.Ident("y"); TokenType.RParen; TokenType.LBrace;
                          TokenType.Ident("x"); TokenType.Plus; TokenType.Ident("y"); TokenType.SemiColon; TokenType.RBrace; TokenType.SemiColon
                          TokenType.EQ; TokenType.NOT_EQ;
                          TokenType.Let; TokenType.Ident("result");TokenType.Assign;TokenType.Ident("add");TokenType.LParen
                          TokenType.Ident("five");TokenType.Comma;TokenType.Ident("ten");TokenType.RParen;TokenType.SemiColon;
                          TokenType.Bang; TokenType.Minus; TokenType.Slash; TokenType.Asterisk; TokenType.Int(5);TokenType.SemiColon
                          TokenType.Int(5); TokenType.LessThan; TokenType.Int(10); TokenType.GreaterThan; TokenType.Int(5); TokenType.SemiColon
                          TokenType.If; TokenType.LParen; TokenType.Int(5); TokenType.LessThan; TokenType.Int(10); TokenType.RParen; TokenType.LBrace
                          TokenType.Return; TokenType.True; TokenType.SemiColon
                          TokenType.RBrace; TokenType.Else; TokenType.LBrace
                          TokenType.Return; TokenType.False; TokenType.SemiColon; TokenType.RBrace;TokenType.EOF]
    let mutable outputList = []
    let lexer = Lexer.newLexer input
    let mutable aToken = Lexer.nextToken lexer
    while aToken <> TokenType.EOF do
        outputList <- aToken :: outputList
        aToken <-Lexer.nextToken lexer
    outputList <- TokenType.EOF :: outputList
    let revList = List.rev outputList
    (*let mutable counter = 0
    while counter < List.length revList do
        printfn "%A" counter
        Assert.True (expectedTokens[counter] = revList[counter])
        counter <- counter + 1*)
    
    Assert.True((expectedTokens = revList))
    
[<Test>]
let simpleIdentifierTest() =
    let input = "abc"
    let expectedToken = TokenType.Ident("abc")
    let lexer = Lexer.newLexer input
    let output = Lexer.nextToken lexer
    Assert.True((output = expectedToken))