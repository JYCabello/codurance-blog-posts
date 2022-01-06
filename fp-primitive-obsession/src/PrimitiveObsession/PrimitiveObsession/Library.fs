namespace PrimitiveObsession.Excercise

type Currency = GBP | USD | EUR

type ProfitCalculator(localCurrency: Currency) =
  let getRate =
    function
    | GBP -> 1.0
    | USD -> 1.6
    | EUR -> 1.2

  let mutable localAmount = 0
  let mutable foreignAmount = 0

  member _.add amount currency incoming =
    let mutable realAmount: int = amount

    let exchangeRate = (getRate currency) / (getRate localCurrency)

    realAmount <- ((realAmount |> float) / exchangeRate) |> int

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
