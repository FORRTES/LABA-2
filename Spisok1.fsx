open System

/// Функция для получения суммы цифр числа
let sumOfDigits (n: int) =
    n.ToString()
    |> Seq.map (fun c -> int c - int '0')
    |> Seq.sum

/// Функция для обработки списка и получения нового списка с суммами цифр
let processList lst =
    List.map sumOfDigits lst

/// Функция для получения списка чисел от пользователя
let rec inputList count =
    if count <= 0 then []
    else
        printf "Введите натуральное число: "
        match Int32.TryParse(Console.ReadLine()) with
        | true, n when n > 0 -> n :: inputList (count - 1)
        | _ -> 
            printfn "Ошибка ввода! Попробуйте снова."
            inputList count

/// Функция для генерации случайного списка
let generateRandomList count maxVal =
    let rnd = Random()
    List.init count (fun _ -> rnd.Next(1, maxVal + 1))

/// Функция выбора метода получения списка
let rec getMethodChoice () =
    printfn "Выберите способ ввода данных:"
    printfn "1 - Ввести вручную"
    printfn "2 - Сгенерировать случайные числа"
    printf "Ваш выбор: "
    match Console.ReadLine() with
    | "1" | "2" as choice -> choice
    | _ -> 
        printfn "Ошибка: некорректный выбор метода! Попробуйте снова."
        getMethodChoice ()

/// Функция для ввода количества чисел
let rec getCount () =
    printf "Сколько чисел? "
    match Int32.TryParse(Console.ReadLine()) with
    | true, count when count > 0 -> count
    | _ -> 
        printfn "Ошибка ввода! Попробуйте снова."
        getCount ()

/// Функция для запроса продолжения работы
let rec askContinue () =
    printfn "Хотите продолжить? (да/нет)"
    match Console.ReadLine().ToLower() with
    | "да" | "д" -> true
    | "нет" | "н" -> false
    | _ -> 
        printfn "Ошибка: введите 'да' или 'нет'."
        askContinue ()

/// Основная функция работы с пользователем
let rec main () =
    let methodChoice = getMethodChoice()
    let count = getCount()
    
    let inputList = 
        match methodChoice with
        | "1" -> inputList count
        | "2" -> generateRandomList count 1000
        | _ -> []  // этот случай никогда не произойдет, но нужен для соответствия типам

    printfn "Исходный список: %A" inputList
    let resultList = processList inputList
    printfn "Результат: %A" resultList

    if askContinue () then main ()
    else printfn "Программа завершена."

main ()
