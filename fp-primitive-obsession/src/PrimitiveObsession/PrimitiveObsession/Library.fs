namespace PrimitiveObsession.Excercise


module Finance =
  module Currencies =
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

  module Trading =

    open Currencies
    type Transaction =
      | Incoming of Money
      | Outgoing of Money

    // The "money" function I skipped before, I found a use for it now.
    let money =
      function
      | Incoming incoming -> incoming
      | Outgoing outgoing -> outgoing

    // Ended up understanding that my "I see where I'm going" was wrong,
    // introduced a first class collection for transactions.
    type Transactions = Transactions of Transaction list

    let (-->>) transaction =
      function
      | Transactions transactions -> Transactions <| transaction :: transactions

    let transactionList =
      function
      | Transactions transactions -> transactions

  module Accounting =

    open Currencies
    open Trading
    // The aforementioned entity, which will replace the Items collection suggested in the excercise.
    type Balance =
      { Transactions: Transactions
        LocalCurrency: Currency }

    let isIn currency transaction =
      transaction
      |> money
      |> fun money -> money.Currency = currency

    let isNotIn currency = isIn currency >> not

    let amount currency transactions =
      let aggregate acc trx = acc + (trx |> money)

      ({ Amount = 0; Currency = currency }, transactions)
      ||> List.fold aggregate

  module Taxes =

    open Accounting
    open Trading
    // It became obvious that the point of these functions was to get amounts to
    // be taxed or not.
    let taxableAmount balance =
      amount
        balance.LocalCurrency
        (balance.Transactions
         |> transactionList
         |> List.filter (isIn balance.LocalCurrency))

    let taxFreeAmount balance =
      amount
        balance.LocalCurrency
        (balance.Transactions
         |> transactionList
         |> List.filter (isNotIn balance.LocalCurrency))

open Finance.Trading
open Finance.Accounting
open Finance.Taxes

module ProfitCalculator =
  let add transaction balance =
    { balance with
        Transactions = transaction -->> balance.Transactions }

  let calculateTax balance =
    match taxableAmount balance with
    | money when money.Amount < 0 -> { money with Amount = 0 }
    | money ->
      { money with
          Amount = ((money.Amount |> float) * 0.2) |> int }

  let calculateProfit balance =
    let tax = calculateTax balance

    taxableAmount balance
    + taxFreeAmount balance
    + { tax with Amount = -tax.Amount }
