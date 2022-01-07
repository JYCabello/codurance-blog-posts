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

  let private applyRate amount rate =
    match rate with
    | Rate r -> ((amount |> float) / r) |> int

  type Money =
    { Amount: int
      Currency: Currency }
    static member (+)(local: Money, other: Money) =
      let applyRate amount rate =
        match rate with
        | Rate r -> ((amount |> float) / r) |> int

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

  let isNotIn currency = isIn currency >> not

  let private amount currency transactions =
    ({ Amount = 0; Currency = currency }, transactions)
    ||> List.fold
          (fun acc trx ->
            let money =
              match trx with
              | Incoming i -> i
              | Outgoing o -> o

            acc + money)

  let amountIn currency transactions =
    amount currency (transactions |> List.filter (isIn currency))

  let amountNotIn currency transactions =
    amount currency (transactions |> List.filter (isNotIn currency))

  type Balance =
    { Transactions: Transaction list
      LocalCurrency: Currency }

open Finance

module ProfitCalculator =
  let add transaction balance =
    { balance with
        Transactions = transaction :: balance.Transactions }

  let calculateTax balance =
    match amountIn balance.LocalCurrency balance.Transactions with
    | money when money.Amount < 0 -> { money with Amount = 0 }
    | money ->
      { money with
          Amount = ((money.Amount |> float) * 0.2) |> int }

  let calculateProfit balance =
    let tax = calculateTax balance

    amountIn balance.LocalCurrency balance.Transactions
    + amountNotIn balance.LocalCurrency balance.Transactions
    + { tax with Amount = -tax.Amount }
