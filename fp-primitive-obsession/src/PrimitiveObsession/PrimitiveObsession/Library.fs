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

  type Money =
    { Amount: int
      Currency: Currency }
    static member (+)(local: Money, other: Money) =
      let applyRate amount =
        function
        | Rate rate -> ((amount |> float) / rate) |> int

      other.Currency >>=> local.Currency
      |> applyRate other.Amount
      |> fun amount ->
           { Amount = local.Amount + amount
             Currency = local.Currency }

  type Transaction =
    | Incoming of Money
    | Outgoing of Money

  let money =
    function
    | Incoming incoming -> incoming
    | Outgoing outgoing -> outgoing

  type Balance =
    { Transactions: Transaction list
      LocalCurrency: Currency }

  let isIn currency =
    function
    | Incoming i -> i.Currency = currency
    | Outgoing o -> o.Currency = currency

  let isNotIn currency = isIn currency >> not

  let private amount currency transactions =
    let aggregate acc trx = acc + (trx |> money)

    ({ Amount = 0; Currency = currency }, transactions)
    ||> List.fold aggregate

  let amountIn balance =
    amount
      balance.LocalCurrency
      (balance.Transactions
       |> List.filter (isIn balance.LocalCurrency))

  let amountNotIn balance =
    amount
      balance.LocalCurrency
      (balance.Transactions
       |> List.filter (isNotIn balance.LocalCurrency))

open Finance

module ProfitCalculator =
  let add transaction balance =
    { balance with
        Transactions = transaction :: balance.Transactions }

  let calculateTax balance =
    match amountIn balance with
    | money when money.Amount < 0 -> { money with Amount = 0 }
    | money ->
      { money with
          Amount = ((money.Amount |> float) * 0.2) |> int }

  let calculateProfit balance =
    let tax = calculateTax balance

    amountIn balance
    + amountNotIn balance
    + { tax with Amount = -tax.Amount }
