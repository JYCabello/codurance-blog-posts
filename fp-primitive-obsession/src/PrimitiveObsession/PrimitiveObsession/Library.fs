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

  type Money =
    { Amount: int
      Currency: Currency }
    static member (+)(local: Money, other: Money) =
      other.Currency >>=> local.Currency
      |> applyRate other.Amount
      |> fun amount ->
           { Amount = local.Amount + amount
             Currency = local.Currency }


  type Transaction =
    | Incoming of Money
    | Outgoing of Money

  let isIn currency =
    function
    | Incoming i -> i.Currency = currency
    | Outgoing o -> o.Currency = currency

  let amountIn currency transactions =
    ({ Amount = 0; Currency = currency }, transactions |> List.filter (isIn currency))
    ||> List.fold
          (fun acc trx ->
            let money =
              match trx with
              | Incoming i -> i
              | Outgoing o -> o

            acc + money)

open Finance


type ProfitCalculator(localCurrency: Currency) =

  let mutable localAmount = { Amount = 0; Currency = localCurrency }
  let mutable foreignAmount = { Amount = 0; Currency = localCurrency }
  let mutable transactions: Transaction list = []

  member _.add transaction =
    transactions <- transaction :: transactions

    let money =
      match transaction with
      | Incoming i -> i
      | Outgoing o -> o

    if money.Currency = localAmount.Currency then
      do localAmount <- localAmount + money
    else
      do foreignAmount <- foreignAmount + money

  member _.calculateTax =
    match amountIn localAmount.Currency transactions with
    | money when money.Amount < 0 -> { money with Amount = 0 }
    | money ->
      { money with
          Amount = ((money.Amount |> float) * 0.2) |> int }

  member this.calculateProfit =
    let tax = this.calculateTax

    localAmount
    + foreignAmount
    + { tax with
          Amount = -this.calculateTax.Amount }
