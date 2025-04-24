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

/// Функция для вычисления значения выражения, используя значения переменных из словаря vars
let rec evaluate (expr: Expr) (vars: IDictionary<string, float>) : float =
    match expr with
    | Var x ->
        if vars.ContainsKey(x) then vars.[x]
        else 
            failwithf "Переменная '%s' не определена" x
    | Const c -> c
    | Add (a, b) -> (evaluate a vars) + (evaluate b vars)
    | Sub (a, b) -> (evaluate a vars) - (evaluate b vars)
    | Mul (a, b) -> (evaluate a vars) * (evaluate b vars)
    | Div (a, b) -> (evaluate a vars) / (evaluate b vars)
    | Sin e      -> Math.Sin(evaluate e vars)
    | Cos e      -> Math.Cos(evaluate e vars)

/// Символьное дифференцирование выражения expr по переменной varName
let rec differentiate (expr: Expr) (varName: string) : Expr =
    match expr with
    | Const _ -> Const 0.0
    | Var x ->
        if x = varName then Const 1.0
        else Const 0.0
    | Add (a, b) ->
        Add(differentiate a varName, differentiate b varName)
    | Sub (a, b) ->
        Sub(differentiate a varName, differentiate b varName)
    | Mul (a, b) ->
        // (f*g)' = f'*g + f*g'
        Add(Mul(differentiate a varName, b), Mul(a, differentiate b varName))
    | Div (a, b) ->
        // (f/g)' = (f'*g - f*g') / g^2
        Div( Sub(Mul(differentiate a varName, b),
                 Mul(a, differentiate b varName)),
             Mul(b, b))
    | Sin e ->
        // (sin e)' = cos e * e'
        Mul(Cos e, differentiate e varName)
    | Cos e ->
        // (cos e)' = -sin e * e'
        Mul(Const -1.0, Mul(Sin e, differentiate e varName))

/// Функция для "печати" символьного выражения в строку
let rec exprToString (expr: Expr) : string =
    match expr with
    | Var x       -> x
    | Const c     -> 
        // Для красоты отсекаем .0, если число целое
        let intPart = int c
        if float intPart = c then string intPart
        else string c
    | Add (a, b)  -> sprintf "(%s + %s)" (exprToString a) (exprToString b)
    | Sub (a, b)  -> sprintf "(%s - %s)" (exprToString a) (exprToString b)
    | Mul (a, b)  -> sprintf "(%s * %s)" (exprToString a) (exprToString b)
    | Div (a, b)  -> sprintf "(%s / %s)" (exprToString a) (exprToString b)
    | Sin e       -> sprintf "sin(%s)" (exprToString e)
    | Cos e       -> sprintf "cos(%s)" (exprToString e)


/// Примитивная лексер-функция, разбирающая строку на токены
/// (числа, скобки, операторы, "sin(", "cos(" и т.д.)
let consumeTokens (chars: string list) : string list =
    let rec loop (acc: string list) (items: string list) =
        match items with
        | [] -> 
            // Когда входных символов больше нет, 
            // переворачиваем собранные токены 
            List.rev acc

        // Скобки
        | "(" :: rest -> loop ("(" :: acc) rest
        | ")" :: rest -> loop (")" :: acc) rest

        // Арифметические операторы
        | "+" :: rest -> loop ("+" :: acc) rest
        | "-" :: rest -> loop ("-" :: acc) rest
        | "*" :: rest -> loop ("*" :: acc) rest
        | "/" :: rest -> loop ("/" :: acc) rest

        // sin( и cos(
        | "s" :: "i" :: "n" :: "(" :: rest -> loop ("sin(" :: acc) rest
        | "c" :: "o" :: "s" :: "(" :: rest -> loop ("cos(" :: acc) rest

        // Любой другой символ (цифра/буква/точка)
        | (x: string) :: rest ->
            // Если x — потенциально часть числа (цифра или точка)
            if x.Length > 0 && (Char.IsDigit(x.[0]) || x = ".") then
                // Собираем целое/вещественное число (например "3.14")
                let rec gatherNumber soFar lst =
                    match lst with
                    | (d: string) :: tail when d.Length > 0 && (Char.IsDigit(d.[0]) || d = ".") ->
                        gatherNumber (soFar + d) tail
                    | other -> soFar, other

                let number, remain = gatherNumber x rest
                loop (number :: acc) remain
            else
                // Иначе считаем x самостоятельным токеном (например, имя переменной 'x')
                loop (x :: acc) rest

    loop [] chars

/// Главная функция парсинга: принимает строку, разбивает на токены и строит Expr
let parseExpr (input: string) : Expr =

    // 1) разбиваем на список символов (убирая пробелы)
    let chars =
        input
        |> Seq.filter (fun c -> not (Char.IsWhiteSpace c))
        |> Seq.map string
        |> Seq.toList

    // 2) превращаем список символов в список токенов
    let tokens = consumeTokens chars

    // 3) Разбираем токены в дерево Expr
    let rec parse (ts: string list) : (Expr * string list) =

        let rec parseFactor (rest: string list) : (Expr * string list) =
            match rest with
            | [] -> failwith "Неожиданный конец выражения в parseFactor"

            // Скобки ( ... )
            | "(" :: tail ->
                let exprInside, tailAfter = parse tail
                match tailAfter with
                | ")" :: tailAfterParen -> exprInside, tailAfterParen
                | _ -> failwith "Отсутствует закрывающая скобка"

            // sin(...) / cos(...)
            | tok :: tail when tok.StartsWith("sin(") ->
                // Пример токена: "sin("
                // После него мы ждём выражение, заканчивающееся на ")"
                let inside = tok.Substring(4, tok.Length - 4) // убираем "sin("
                // Чтобы не усложнять парсер, мы притворимся, что дальше стоит "(" + inside,
                // и потом дойдём до закрывающей скобки.
                let exprInside, tailAfter = parse (("("+inside) :: tail)
                Sin exprInside, tailAfter

            | tok :: tail when tok.StartsWith("cos(") ->
                let inside = tok.Substring(4, tok.Length - 4) // убираем "cos("
                let exprInside, tailAfter = parse (("("+inside) :: tail)
                Cos exprInside, tailAfter

            // Числа / переменные
            | tok :: tail ->
                match System.Double.TryParse(tok) with
                | (true, num) -> Const num, tail
                | _ -> Var tok, tail

        // parseTerm: обрабатывает операции * и /
        let rec parseTerm (leftExpr: Expr) (rest: string list) : (Expr * string list) =
            match rest with
            | "*" :: tail ->
                let rightExpr, tail2 = parseFactor tail
                parseTerm (Mul(leftExpr, rightExpr)) tail2
            | "/" :: tail ->
                let rightExpr, tail2 = parseFactor tail
                parseTerm (Div(leftExpr, rightExpr)) tail2
            | _ -> leftExpr, rest

        // parseExprInner: обрабатывает операции + и -
        let rec parseExprInner (leftExpr: Expr) (rest: string list) : (Expr * string list) =
            match rest with
            | "+" :: tail ->
                let rightExpr, tail2 = parseFactor tail
                parseExprInner (Add(leftExpr, rightExpr)) tail2
            | "-" :: tail ->
                let rightExpr, tail2 = parseFactor tail
                parseExprInner (Sub(leftExpr, rightExpr)) tail2
            | _ -> leftExpr, rest

        // Сначала получаем factor, затем обрабатываем * /, затем + -
        let f, r = parseFactor ts
        let t, r2 = parseTerm f r
        parseExprInner t r2

    let parsedExpr, leftover = parse tokens
    if leftover.Length > 0 then
        failwithf "Не удалось разобрать выражение до конца. Остались лишние токены: %A" leftover

    parsedExpr


[<EntryPoint>]
let main _ =
    // Словарь для хранения значений переменных
    let vars = Dictionary<string, float>()

    let rec loop () =
        Console.Write("> ")
        let input = Console.ReadLine()
        match input with
        | null -> () // EOF (Ctrl+D/Ctrl+Z) — выходим
        | command -> 
            if command.StartsWith("exit") then
                ()
            elif command.Contains("=") then
                // Присвоение переменной: x = 3.14
                let parts = command.Split('=')
                if parts.Length = 2 then
                    let varName = parts.[0].Trim()
                    let exprStr = parts.[1].Trim()
                    try
                        let exprParsed = parseExpr exprStr
                        let value = evaluate exprParsed vars
                        vars.[varName] <- value
                        printfn "Переменной '%s' присвоено значение %f" varName value
                    with ex ->
                        printfn "Ошибка: %s" ex.Message
                else
                    printfn "Неверный формат присваивания. Используйте: x = выражение"
                loop ()
            elif command.StartsWith("diff") then
                // Символьная производная: diff <expr> d <var>
                let pattern = "diff "
                let d = " d "
                let idx = command.IndexOf(d, pattern.Length)
                if idx > 0 then
                    let exprStr = command.Substring(pattern.Length, idx - pattern.Length)
                    let varName = command.Substring(idx + d.Length).Trim()
                    try
                        let exprParsed = parseExpr exprStr
                        let diffExpr = differentiate exprParsed varName
                        printfn "d/d%s %s = %s" varName (exprToString exprParsed) (exprToString diffExpr)
                    with ex ->
                        printfn "Ошибка при дифференцировании: %s" ex.Message
                else
                    printfn "Неверный формат команды diff. Используйте: diff <expr> d <var>"
                loop ()
            else
                // Просто вычислить выражение (без присвоения)
                try
                    let exprParsed = parseExpr command
                    let value = evaluate exprParsed vars
                    printfn "%s = %f" (exprToString exprParsed) value
                with ex ->
                    printfn "Ошибка: %s" ex.Message
                loop ()

    printfn "Функциональный калькулятор на F#.\nКоманды:\n 1) +  -  *  /       (Сложение, Вычитание, Умножение, Деление)\n 2) x = 3.14        (присвоить переменной x)\n 3) diff x^2 d x  (символьная производная)\n 4) sin(x), cos(x)  (триг.функции)\n 5) exit            (выход)\n"
    loop ()
    0
