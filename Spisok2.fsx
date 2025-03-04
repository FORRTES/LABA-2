open System
open System.Globalization

// ������� ��������� ���������� ������ ������� �����
let generateRandomList length =
    let rand = Random()
    List.init length (fun _ -> rand.NextDouble() * 1000.0) // ��������� ����� �� 0.0 �� 1000.0

// ������� ��������, ���������� �� ����� � �������� �����
let startsWithDigit digit (num: float) =
    let strNum = num.ToString("G17", CultureInfo.InvariantCulture) // ���������� G17 ��� ������� ������������� ��� ������ �����
    strNum.StartsWith(digit.ToString())

// ������� ������������ ���������, ������������ � ��������� �����
let sumNumbersStartingWith digit numbers =
    numbers
    |> List.filter (startsWithDigit digit)
    |> List.fold (+) 0.0

// ������� �������� � �������� `float`
let tryParseFloat (str: string) =
    match Double.TryParse(str, NumberStyles.Float, CultureInfo.InvariantCulture) with
    | (true, num) -> Some num
    | _ -> None

// ������� ��� ������� � ������������ ����������� `float`
let rec getValidFloatInput prompt validate =
    printf "%s" prompt
    match Console.ReadLine() with
    | null | "" -> 
        printfn "������ �����! ���������� ��� ���."
        getValidFloatInput prompt validate
    | input ->
        match tryParseFloat input with
        | Some num when validate num -> num
        | _ ->
            printfn "������ �����! ���������� ��� ���."
            getValidFloatInput prompt validate

// ������� ��������� ������ ������� ����� �� ������������
let rec getValidFloatList prompt =
    printf "%s" prompt
    let input = Console.ReadLine()
    match input with
    | null | "" ->
        printfn "������ �����! ������� ���� �� ���� �����."
        getValidFloatList prompt
    | _ ->
        let numbers = input.Split(' ') |> Array.toList |> List.choose tryParseFloat
        if numbers.IsEmpty then
            printfn "������ �����! ������� ���� �� ���� �����."
            getValidFloatList prompt
        else numbers

// ������� ��������, ����� �� ������������ ����������
let rec askToContinue () =
    printf "������ ����������? (��/���): "
    match Console.ReadLine() with
    | null | "" ->
        printfn "�������� ����! ������� '��' ��� '���'."
        askToContinue ()
    | input ->
        match input.Trim().ToLower() with
        | "��" -> true
        | "���" -> false
        | _ ->
            printfn "�������� ����! ������� '��' ��� '���'."
            askToContinue ()

// ������� ��������� � ������
let rec main () =
    let choice = getValidFloatInput "�������� ����� (1 - ���� ������, 2 - ��������� ���������): " (fun x -> x = 1.0 || x = 2.0)

    let numbers =
        match int choice with
        | 1 -> getValidFloatList "������� ����� ����� ������ (����������� ����� ��� �������): "
        | 2 -> 
            let count = getValidFloatInput "������� ���������� ��������� ����� (1-100): " (fun x -> x > 0.0 && x <= 100.0)
            let generatedNumbers = generateRandomList (int count)
            printfn "��������� ������: %A" generatedNumbers
            generatedNumbers
        | _ -> [] // ������� �� ���������, ��� ��� `getValidFloatInput` ����������� ���������� ����

    let digit = getValidFloatInput "������� ����� (0-9), � ������� ������ ���������� �����: " (fun x -> x >= 0.0 && x <= 9.0)

    let result = sumNumbersStartingWith (int digit) numbers

    printfn "����� �����, ������������ �� %d: %s" (int digit) (result.ToString("G17", CultureInfo.InvariantCulture))

    if askToContinue () then 
        main ()
    else 
        printfn "��������� ��������� ������."

// ������ ���������
main ()
