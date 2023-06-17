module FSharpMonkey.repl
let prompt = ">>"
let start () =
    printf $"%s{prompt} "
    let mutable input = System.Console.ReadLine()
    while input <> "quit" do
        let lexer = Lexer.newLexer input
        let mutable token = Lexer.nextToken lexer
        while token <> TokenType.EOF do 
            printf "%A " token
            token <- Lexer.nextToken lexer
        printfn ""
        printf $"%s{prompt} "
        input <- System.Console.ReadLine()
        