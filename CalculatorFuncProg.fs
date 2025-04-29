open System
open System.Collections.Generic

/// Описывает возможные выражения
type Expr =
    | Var of string           // Переменная
    | Const of float          // Константа
    | Add of Expr * Expr      // Сложение
    | Sub of Expr * Expr      // Вычитание
    | Mul of Expr * Expr      // Умножение
    | Div of Expr * Expr      // Деление
    | Sin of Expr             // sin(...)
    | Cos of Expr             // cos(...)
    | Pow of Expr * Expr      // Возведение в степень
    | Exp of Expr             // Экспонента
    | Ln of Expr              // Натуральный логарифм

/// Функция для вычисления значения выражения
let rec evaluate (expr: Expr) (vars: IDictionary<string, float>) : float =
    match expr with
    | Var x -> vars.[x]
    | Const c -> c
    | Add (a, b) -> evaluate a vars + evaluate b vars
    | Sub (a, b) -> evaluate a vars - evaluate b vars
    | Mul (a, b) -> evaluate a vars * evaluate b vars
    | Div (a, b) -> evaluate a vars / evaluate b vars
    | Sin e -> Math.Sin(evaluate e vars)
    | Cos e -> Math.Cos(evaluate e vars)
    | Pow (a, b) -> Math.Pow(evaluate a vars, evaluate b vars)
    | Exp e -> Math.Exp(evaluate e vars)
    | Ln e -> Math.Log(evaluate e vars)

/// Символьное дифференцирование
let rec differentiate (expr: Expr) (varName: string) : Expr =
    match expr with
    | Const _ -> Const 0.0
    | Var x -> if x = varName then Const 1.0 else Const 0.0
    | Add (a, b) -> Add(differentiate a varName, differentiate b varName)
    | Sub (a, b) -> Sub(differentiate a varName, differentiate b varName)
    | Mul (a, b) -> Add(Mul(differentiate a varName, b), Mul(a, differentiate b varName))
    | Div (a, b) -> Div(Sub(Mul(differentiate a varName, b), Mul(a, differentiate b varName)), Mul(b, b))
    | Sin e -> Mul(Cos e, differentiate e varName)
    | Cos e -> Mul(Const -1.0, Mul(Sin e, differentiate e varName))
    | Pow (u, v) ->
        Mul(
            Pow(u, v),
            Add(
                Mul(differentiate v varName, Ln u),
                Div(Mul(v, differentiate u varName), u)
            )
        )
    | Exp u -> Mul(Exp u, differentiate u varName)
    | Ln u -> Div(differentiate u varName, u)

/// Преобразование выражения в строку
let rec exprToString (expr: Expr) : string =
    match expr with
    | Var x -> x
    | Const c -> if float (int c) = c then string (int c) else string c
    | Add (a, b) -> sprintf "(%s + %s)" (exprToString a) (exprToString b)
    | Sub (a, b) -> sprintf "(%s - %s)" (exprToString a) (exprToString b)
    | Mul (a, b) -> sprintf "(%s * %s)" (exprToString a) (exprToString b)
    | Div (a, b) -> sprintf "(%s / %s)" (exprToString a) (exprToString b)
    | Sin e -> sprintf "sin(%s)" (exprToString e)
    | Cos e -> sprintf "cos(%s)" (exprToString e)
    | Pow (a, b) -> sprintf "(%s ^ %s)" (exprToString a) (exprToString b)
    | Exp e -> sprintf "exp(%s)" (exprToString e)
    | Ln e -> sprintf "ln(%s)" (exprToString e)

/// Лексер
let consumeTokens (chars: string list) : string list =
    let rec loop (acc: string list) (items: string list) =
        match items with
        | [] -> List.rev acc
        | "(" :: rest -> loop ("(" :: acc) rest
        | ")" :: rest -> loop (")" :: acc) rest
        | "+" :: rest -> loop ("+" :: acc) rest
        | "-" :: rest -> loop ("-" :: acc) rest
        | "*" :: rest -> loop ("*" :: acc) rest
        | "/" :: rest -> loop ("/" :: acc) rest
        | "^" :: rest -> loop ("^" :: acc) rest
        | "s" :: "i" :: "n" :: "(" :: rest -> loop ("sin(" :: acc) rest
        | "c" :: "o" :: "s" :: "(" :: rest -> loop ("cos(" :: acc) rest
        | "e" :: "x" :: "p" :: "(" :: rest -> loop ("exp(" :: acc) rest
        | "l" :: "n" :: "(" :: rest -> loop ("ln(" :: acc) rest
        | x :: rest ->
            // Исправление: явная проверка типа и преобразование
            if x.Length > 0 && (Char.IsDigit(x.[0]) || x = ".") then
                let rec gatherNumber num (lst: string list) =
                    match lst with
                    | d :: tail when (d.Length > 0 && (Char.IsDigit(d.[0]) || d = ".")) ->
                        gatherNumber (num + d) tail
                    | _ -> num, lst
                let number, remain = gatherNumber x rest
                loop (number :: acc) remain
            else
                loop (x :: acc) rest
    loop [] chars

/// Парсер
let parseExpr (input: string) =
    let chars = 
        input 
        |> Seq.filter (fun c -> not (Char.IsWhiteSpace c)) 
        |> Seq.map string 
        |> Seq.toList

    let tokens = consumeTokens chars

    let rec parse ts =
        let rec parseFactor rest =
            match rest with
            | [] -> failwith "Неожиданный конец выражения"
            | "(" :: tail ->
                let expr, tailAfter = parse tail
                match tailAfter with
                | ")" :: t -> expr, t
                | _ -> failwith "Не закрыта скобка"
            | tok :: tail when tok.StartsWith("sin(") ->
                let inside = tok.Substring(4)
                let expr, t = parse (("(" + inside) :: tail)
                Sin expr, t
            | tok :: tail when tok.StartsWith("cos(") ->
                let inside = tok.Substring(4)
                let expr, t = parse (("(" + inside) :: tail)
                Cos expr, t
            | tok :: tail when tok.StartsWith("exp(") ->
                let inside = tok.Substring(4)
                let expr, t = parse (("(" + inside) :: tail)
                Exp expr, t
            | tok :: tail when tok.StartsWith("ln(") ->
                let inside = tok.Substring(3)
                let expr, t = parse (("(" + inside) :: tail)
                Ln expr, t
            | tok :: tail ->
                match Double.TryParse(tok) with
                | true, num -> Const num, tail
                | _ -> Var tok, tail

        let rec parseTerm left rest =
            match rest with
            | "*" :: tail ->
                let right, t = parseFactor tail
                parseTerm (Mul(left, right)) t
            | "/" :: tail ->
                let right, t = parseFactor tail
                parseTerm (Div(left, right)) t
            | "^" :: tail ->
                let right, t = parseFactor tail
                parseTerm (Pow(left, right)) t
            | _ -> left, rest

        let rec parseExprInner left rest =
            match rest with
            | "+" :: tail ->
                let right, t = parseFactor tail
                parseExprInner (Add(left, right)) t
            | "-" :: tail ->
                let right, t = parseFactor tail
                parseExprInner (Sub(left, right)) t
            | _ -> left, rest

        let factor, r = parseFactor ts
        let term, r2 = parseTerm factor r
        parseExprInner term r2

    let expr, leftover = parse tokens
    if not (List.isEmpty leftover) then
        failwithf "Остались неразобранные токены: %A" leftover
    expr

[<EntryPoint>]
let main _ =
    let vars = Dictionary<string, float>()
    // Новое подробное описание возможностей
    printfn "
╔══════════════════════════════════════════════════╗
║          Функциональный калькулятор 2.0          ║
╠══════════════════════════════════════════════════╣
║ Поддерживаемые операции:                         ║
║  • Арифметика: +, -, *, /, ^ (степень)           ║
║  • Функции: sin(), cos(), exp(), ln()            ║
║  • Переменные: x = 5, y = 2^3 + sin(1)           ║
║  • Дифференцирование: diff <выражение> d <var> ║
╠══════════════════════════════════════════════════╣
║ Примеры команд:                                  ║
║  > 2 + 3 * 5                                     ║
║  > x = 3.14                                      ║
║  > diff x^2 + sin(x) d x                       ║
║  > exp(2) + ln(e^3)                              ║
║  > exit                                          ║
╚══════════════════════════════════════════════════╝"

    let rec loop() =
        Console.Write("> ")
        match Console.ReadLine() with
        | null | "exit" -> 0
        | input ->
            try
                if input.Contains("=") then
                    let parts = input.Split('=')
                    let varName = parts.[0].Trim()
                    let expr = parseExpr (parts.[1].Trim())
                    vars.[varName] <- evaluate expr vars
                    printfn "Успешно: %s = %f" varName vars.[varName]
                elif input.StartsWith("diff") then
                    let parts = input.Split([|"d"|], StringSplitOptions.RemoveEmptyEntries)
                    let expr = parseExpr (parts.[0].Substring(4).Trim())
                    let var = parts.[1].Trim()
                    let derivative = differentiate expr var
                    printfn "Производная: %s" (exprToString derivative)
                else
                    let expr = parseExpr input
                    printfn "Результат: %f" (evaluate expr vars)
            with ex -> printfn "Ошибка: %s" ex.Message
            loop()
    loop()
