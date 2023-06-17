module FSharpMonkey.TokenType

type Tokens =
    | Assign
    | Plus
    | LParen
    | RParen
    | LBrace
    | RBrace
    | Comma
    | SemiColon
    | EOF
    | Let
    | Ident of string
    | Int of int
    | Function
    | Illegal
    | Minus
    | Slash
    | Asterisk
    | Bang
    | LessThan
    | GreaterThan
    | Else
    | Return
    | If
    | True
    | False
    | EQ
    | NOT_EQ
    