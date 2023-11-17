Creation of list and finding salaries and highIncomeSalaries
let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

let highIncomeSalaries = List.filter (fun y -> y > 100000) salaries

printfn "Original Salaries: %A" salaries
printfn "High-Income Salaries: %A" highIncomeSalaries

Calculating tax with original and highIncomeSalaries
let calculateTax income =
    if income <= 49020 then float income * 0.15
    elif income <= 98040 then float income * 0.205
    elif income <= 151978 then float income * 0.26
    elif income <= 216511 then float income * 0.29
    else float income * 0.33

let taxedIncomes = List.map calculateTax [75000; 48000; 120000; 190000; 300113; 92000; 36000]

printfn "Original Salaries: %A" [75000; 48000; 120000; 190000; 300113; 92000; 36000]
printfn "Taxed Incomes: %A" taxedIncomes

Filtering of salaries and highIncomeSalaries
let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

let adjustedSalaries =
    salaries
    |> List.map (fun y -> if y < 49020 then y + 20000 else y)

printfn "Original Salaries: %A" salaries
printfn "Adjusted Salaries: %A" adjustedSalaries

Filtering of sum of salaries and highIncomeSalaries
let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

let sumOfMidSalaries =
    salaries
    |> List.filter (fun y -> y >= 50000 && y <= 100000)
    |> List.fold (+) 0

printfn "Original Salaries: %A" salaries
printfn "Sum of Mid Salaries: %d" sumOfMidSalaries

Tail recursion
let sumOfMultiplesOf3 y =
    let rec sumHelper acc current =
        if current > y then acc
        else sumHelper (acc + current) (current + 3)
    sumHelper 0 3

let result = sumOfMultiplesOf3 27
printfn "Result: %d" result



