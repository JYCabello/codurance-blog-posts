namespace PrimitiveObsession.Finance

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


module Profits =
  open Accounting
  open Trading
  open Taxes

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
