namespace PrimitiveObsession.Original

type ProfitCalculatorOriginal(localCurrency: string) =
  let rates =
    [ ("GBP", 1.0)
      ("USD", 1.6)
      ("EUR", 1.2) ]
    |> Map.ofSeq

  let mutable localAmount = 0
  let mutable foreignAmount = 0

  do
    try
      rates.[localCurrency] |> ignore
    with
    | _ -> invalidArg (nameof localCurrency) "Was not a valid currency"

  member _.add amount currency incoming =
    let mutable realAmount: int = amount

    let exchangeRate =
      rates.TryFind currency
      |> Option.map (fun incomingRate -> incomingRate / rates.[localCurrency])

    realAmount <-
      exchangeRate
      |> Option.map (fun rate -> ((realAmount |> float) / rate) |> int)
      |> Option.defaultValue realAmount

    if not incoming then
      do realAmount <- -realAmount

    if localCurrency = currency then
      do localAmount <- localAmount + realAmount
    else
      do foreignAmount <- foreignAmount + realAmount

  member _.calculateTax =
    match localAmount with
    | amount when amount < 0 -> 0
    | amount -> ((amount |> float) * 0.2) |> int

  member this.calculateProfit =
    localAmount - this.calculateTax + foreignAmount
