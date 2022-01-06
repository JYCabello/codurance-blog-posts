namespace PrimitiveObsession.Excercise

type Currency =
  | GBP
  | USD
  | EUR

type ExchangeRate =
  | Rate of float
  static member (/)(Rate a, Rate b) = (a / b) |> Rate

module ProfitCalculator =
  let applyRate amount rate =
    match rate with
    | Rate r -> ((amount |> float) / r) |> int

  let getRate =
    function
    | GBP -> Rate 1.0
    | USD -> Rate 1.6
    | EUR -> Rate 1.2

type ProfitCalculator(localCurrency: Currency) =

  let mutable localAmount = 0
  let mutable foreignAmount = 0

  member _.add amount currency incoming =
    let mutable realAmount: int = amount

    let exchangeRate =
      (ProfitCalculator.getRate currency)
      / (ProfitCalculator.getRate localCurrency)

    realAmount <- ProfitCalculator.applyRate realAmount exchangeRate

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
