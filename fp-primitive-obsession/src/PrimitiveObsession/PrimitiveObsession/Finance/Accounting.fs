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
    let aggregate moneySoFar transaction = moneySoFar + (transaction |> money)

    ({ Amount = 0; Currency = currency }, transactions)
    ||> List.fold aggregate

  let add transaction balance =
    balance.Transactions
    |> transactionList
    |> fun transactions ->
         { balance with
             Transactions = Transactions <| transaction :: transactions }
