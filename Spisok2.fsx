open System
open System.Globalization

// Функция генерации случайного списка дробных чисел
let generateRandomList length =
    let rand = Random()
    List.init length (fun _ -> rand.NextDouble() * 1000.0) // Генерация чисел от 0.0 до 1000.0

// Функция проверки, начинается ли число с заданной цифры
let startsWithDigit digit (num: float) =
    let strNum = num.ToString("G17", CultureInfo.InvariantCulture) // Используем G17 для точного представления без лишних нулей
    strNum.StartsWith(digit.ToString())

// Функция суммирования элементов, начинающихся с указанной цифры
let sumNumbersStartingWith digit numbers =
    numbers
    |> List.filter (startsWithDigit digit)
    |> List.fold (+) 0.0

// Функция проверки и парсинга `float`
let tryParseFloat (str: string) =
    match Double.TryParse(str, NumberStyles.Float, CultureInfo.InvariantCulture) with
    | (true, num) -> Some num
    | _ -> None

// Функция для запроса у пользователя корректного `float`
let rec getValidFloatInput prompt validate =
    printf "%s" prompt
    match Console.ReadLine() with
    | null | "" -> 
        printfn "Ошибка ввода! Попробуйте ещё раз."
        getValidFloatInput prompt validate
    | input ->
        match tryParseFloat input with
        | Some num when validate num -> num
        | _ ->
            printfn "Ошибка ввода! Попробуйте ещё раз."
            getValidFloatInput prompt validate

// Функция получения списка дробных чисел от пользователя
let rec getValidFloatList prompt =
    printf "%s" prompt
    let input = Console.ReadLine()
    match input with
    | null | "" ->
        printfn "Ошибка ввода! Введите хотя бы одно число."
        getValidFloatList prompt
    | _ ->
        let numbers = input.Split(' ') |> Array.toList |> List.choose tryParseFloat
        if numbers.IsEmpty then
            printfn "Ошибка ввода! Введите хотя бы одно число."
            getValidFloatList prompt
        else numbers

// Функция проверки, хочет ли пользователь продолжить
let rec askToContinue () =
    printf "Хотите продолжить? (да/нет): "
    match Console.ReadLine() with
    | null | "" ->
        printfn "Неверный ввод! Введите 'да' или 'нет'."
        askToContinue ()
    | input ->
        match input.Trim().ToLower() with
        | "да" -> true
        | "нет" -> false
        | _ ->
            printfn "Неверный ввод! Введите 'да' или 'нет'."
            askToContinue ()

// Главная программа с циклом
let rec main () =
    let choice = getValidFloatInput "Выберите режим (1 - ввод списка, 2 - случайная генерация): " (fun x -> x = 1.0 || x = 2.0)

    let numbers =
        match int choice with
        | 1 -> getValidFloatList "Введите числа через пробел (используйте точку для дробных): "
        | 2 -> 
            let count = getValidFloatInput "Введите количество случайных чисел (1-100): " (fun x -> x > 0.0 && x <= 100.0)
            let generatedNumbers = generateRandomList (int count)
            printfn "Случайный список: %A" generatedNumbers
            generatedNumbers

    let digit = getValidFloatInput "Введите цифру (0-9), с которой должны начинаться числа: " (fun x -> x >= 0.0 && x <= 9.0)

    let result = sumNumbersStartingWith (int digit) numbers

    printfn "Сумма чисел, начинающихся на %d: %s" (int digit) (result.ToString("G17", CultureInfo.InvariantCulture))

    if askToContinue () then 
        main ()
    else 
        printfn "Программа завершила работу."

// Запуск программы
main ()
