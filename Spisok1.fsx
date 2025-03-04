open System

/// ������� ��� ��������� ����� ���� �����
let sumOfDigits (n: int) =
    n.ToString()
    |> Seq.map (fun c -> int c - int '0')
    |> Seq.sum

/// ������� ��� ��������� ������ � ��������� ������ ������ � ������� ����
let processList lst =
    List.map sumOfDigits lst

/// ������� ��� ��������� ������ ����� �� ������������
let rec inputList count =
    if count <= 0 then []
    else
        printf "������� ����������� �����: "
        match Int32.TryParse(Console.ReadLine()) with
        | true, n when n > 0 -> n :: inputList (count - 1)
        | _ -> 
            printfn "������ �����! ���������� �����."
            inputList count

/// ������� ��� ��������� ���������� ������
let generateRandomList count maxVal =
    let rnd = Random()
    List.init count (fun _ -> rnd.Next(1, maxVal + 1))

/// ������� ������ ������ ��������� ������
let rec getMethodChoice () =
    printfn "�������� ������ ����� ������:"
    printfn "1 - ������ �������"
    printfn "2 - ������������� ��������� �����"
    printf "��� �����: "
    match Console.ReadLine() with
    | "1" | "2" as choice -> choice
    | _ -> 
        printfn "������: ������������ ����� ������! ���������� �����."
        getMethodChoice ()

/// ������� ��� ����� ���������� �����
let rec getCount () =
    printf "������� �����? "
    match Int32.TryParse(Console.ReadLine()) with
    | true, count when count > 0 -> count
    | _ -> 
        printfn "������ �����! ���������� �����."
        getCount ()

/// ������� ��� ������� ����������� ������
let rec askContinue () =
    printfn "������ ����������? (��/���)"
    match Console.ReadLine().ToLower() with
    | "��" | "�" -> true
    | "���" | "�" -> false
    | _ -> 
        printfn "������: ������� '��' ��� '���'."
        askContinue ()

/// �������� ������� ������ � �������������
let rec main () =
    let methodChoice = getMethodChoice()
    let count = getCount()
    
    let inputList = 
        match methodChoice with
        | "1" -> inputList count
        | "2" -> generateRandomList count 1000
        | _ -> []  // ���� ������ ������� �� ����������, �� ����� ��� ������������ �����

    printfn "�������� ������: %A" inputList
    let resultList = processList inputList
    printfn "���������: %A" resultList

    if askContinue () then main ()
    else printfn "��������� ���������."

main ()
