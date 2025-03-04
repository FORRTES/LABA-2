open System

/// Функция для получения суммы цифр числа (Function to get the sum of digits of a number)
let sumOfDigits (n: int) =
    n.ToString()
    |> Seq.map (fun c -> int c - int '0')
    |> Seq.sum

/// Функция для обработки списка и получения нового списка с суммами цифр (Function to process the list and get a new list with digit sums)
let processList lst =
    List.map sumOfDigits lst

/// Функция для получения списка чисел от пользователя (Function to get a list of numbers from the user)
let rec inputList count =
    if count <= 0 then []
    else
        printf "Enter a natural number: "
        match Int32.TryParse(Console.ReadLine()) with
        | true, n when n > 0 -> n :: inputList (count - 1)
        | _ -> 
            printfn "Input error! Please try again."
            inputList count

/// Функция для генерации случайного списка (Function to generate a random list)
let generateRandomList count maxVal =
    let rnd = Random()
    List.init count (fun _ -> rnd.Next(1, maxVal + 1))

/// Функция выбора метода получения списка (Function to select the input method)
let rec getMethodChoice () =
    printfn "Select input method:"
    printfn "1 - Enter manually"
    printfn "2 - Generate random numbers"
    printf "Your choice: "
    match Console.ReadLine() with
    | "1" | "2" as choice -> choice
    | _ -> 
        printfn "Error: incorrect method choice! Please try again."
        getMethodChoice ()

/// Функция для ввода количества чисел (Function to input the number of numbers)
let rec getCount () =
    printf "How many numbers? "
    match Int32.TryParse(Console.ReadLine()) with
    | true, count when count > 0 -> count
    | _ -> 
        printfn "Input error! Please try again."
        getCount ()

/// Функция для запроса продолжения работы (Function to ask whether to continue)
let rec askContinue () =
    printfn "Do you want to continue? (yes/no)"
    match Console.ReadLine().ToLower() with
    | "yes" | "y" -> true
    | "no" | "n" -> false
    | _ -> 
        printfn "Error: please enter 'yes' or 'no'."
        askContinue ()

/// Основная функция работы с пользователем (Main function to interact with the user)
let rec main () =
    let methodChoice = getMethodChoice()
    let count = getCount()
    
    let inputList = 
        match methodChoice with
        | "1" -> inputList count
        | "2" -> generateRandomList count 1000

    printfn "Original list: %A" inputList
    let resultList = processList inputList
    printfn "Result: %A" resultList

    if askContinue () then main ()
    else printfn "Program terminated."

main ()
