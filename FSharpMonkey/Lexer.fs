module FSharpMonkey.Lexer

type Lexer = {mutable input: string; mutable position: int
              mutable readPosition: int; mutable ch: char option}

let readChar (l: Lexer) =
    if l.readPosition >= l.input.Length then
        l.ch <- None
    else
        l.ch <- Some l.input[l.readPosition] 
    l.position <-l.readPosition
    l.readPosition <- l.readPosition + 1
    l // returning the lexer so I can chain operations
    
let newLexer (input: string) =
    let lexer = {input=input; position = 0;readPosition = 0; ch = None }
    readChar lexer |> ignore
    lexer


let inline isLetter (input: char option) =
    match input with
    | Some(ch) -> 'a'<= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch = '_'
    | None -> false
   
let peek (lexer: Lexer): char =
    if lexer.readPosition >= lexer.input.Length then
        (0 |> char)
    else
        lexer.input[lexer.readPosition]
    
let readIdentifier (lexer: Lexer) =
    let rec readIdentifier' (lexer: Lexer) (position: int) =
        if isLetter (Some (peek lexer)) then
            readChar lexer |> ignore
            readIdentifier' lexer position
        else 
            lexer.input[position..lexer.position]
    readIdentifier' lexer lexer.position
    
let rec skipWhitespace (lexer: Lexer) =
    match lexer.ch with
    | (Some ' ') | (Some '\t') | (Some '\n') | (Some '\r') ->
        readChar lexer |> ignore
        skipWhitespace lexer
    | _ -> ()
let isDigit input =
    (Some '0') <= input && input <= (Some '9')
    
let rec readNumber (lexer: Lexer) =
    let rec readNumberInternal (lexer: Lexer) (position: int) =
        if isDigit (Some (peek lexer)) then
            readChar lexer |> ignore
            readNumberInternal lexer position
        else
            lexer.input[position..lexer.position]
    readNumberInternal lexer lexer.position
    
        
let nextToken(lexer: Lexer) =
    skipWhitespace lexer
    let output =
        match lexer.ch with
        | Some x ->
                match x with
                | '=' ->
                    if (peek lexer) = '=' then
                        readChar lexer |> ignore
                        TokenType.EQ
                    else
                        TokenType.Assign
                | ';' -> TokenType.SemiColon
                | '(' -> TokenType.LParen
                | ')' -> TokenType.RParen
                | '}' -> TokenType.RBrace
                | '{' -> TokenType.LBrace
                | ',' -> TokenType.Comma
                | '+' -> TokenType.Plus
                | '-' -> TokenType.Minus
                | '/' -> TokenType.Slash
                | '<' -> TokenType.LessThan
                | '>' -> TokenType.GreaterThan
                | '*' -> TokenType.Asterisk
                | '!' ->
                    if (peek lexer) = '=' then
                        readChar lexer |> ignore
                        TokenType.NOT_EQ
                    else
                        TokenType.Bang
                | ch ->
                    if isLetter (Some ch) then
                        let possibleIdent = readIdentifier lexer
                        match possibleIdent with
                        | "let" -> TokenType.Let
                        | "fn" -> TokenType.Function
                        | "else" -> TokenType.Else
                        | "if" -> TokenType.If
                        | "return" -> TokenType.Return
                        | "true" -> TokenType.True
                        | "false" -> TokenType.False
                        | identifier -> TokenType.Ident(identifier)
                    elif isDigit (Some ch) then
                        let possibleNumber = readNumber lexer
                        TokenType.Int(possibleNumber |> int)
                    else
                        TokenType.Illegal
        | None -> TokenType.EOF
    
    readChar lexer |> ignore
    output
