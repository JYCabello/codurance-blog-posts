namespace PrimitiveObsession.Excercise

module Finance =
  type Currency =
    | GBP
    | USD
    | EUR

  type ExchangeRate =
    | Rate of float
    static member (/)(Rate a, Rate b) = (a / b) |> Rate

    static member get =
      function
      | GBP -> Rate 1.0
      | USD -> Rate 1.6
      | EUR -> Rate 1.2

  let (>>=>) (a: Currency) (b: Currency) = ExchangeRate.get a / ExchangeRate.get b

  let applyRate amount rate =
    match rate with
    | Rate r -> ((amount |> float) / r) |> int

  type Money = { Amount: int; Currency: Currency }

  let add local other =
    other.Currency >>=> local.Currency
    |> applyRate other.Amount
    |> fun amount -> { Amount = local.Amount + amount; Currency = local.Currency }

  type Transaction =
  | Incoming of Money
  | Outgoing of Money


open Finance


type ProfitCalculator(localCurrency: Currency) =

  let mutable localAmount = { Amount = 0; Currency = localCurrency }
  let mutable foreignAmount = { Amount = 0; Currency = localCurrency }

  member _.add money incoming =
    let money = if incoming then money else { money with Amount = -money.Amount }

    if money.Currency = localAmount.Currency then
      do localAmount <- add localAmount money
    else
      do foreignAmount <- add foreignAmount money

  member _.calculateTax =
    match localAmount.Amount with
    | amount when amount < 0 -> { Amount = 0; Currency = localAmount.Currency }
    | amount -> { Amount = ((amount |> float) * 0.2) |> int; Currency = localAmount.Currency }

  member this.calculateProfit =
    foreignAmount
    |> add localAmount
    |> add { this.calculateTax with Amount = -this.calculateTax.Amount }
