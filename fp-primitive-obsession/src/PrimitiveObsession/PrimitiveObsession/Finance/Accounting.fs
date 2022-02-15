namespace PrimitiveObsession.Finance

module Accounting =
  open Currencies
  open Trading

  // Ended up understanding that my "I see where I'm going" was wrong,
  // introduced a first class collection for transactions.
  type Transactions = Transactions of Transaction list

  let private transactionList =
    function
    | Transactions transactions -> transactions

  // The aforementioned entity, which will replace the Items collection suggested in the excercise.
  type Balance =
    { Transactions: Transactions
      LocalCurrency: Currency }

  let private isIn currency transaction =
    transaction
    |> money
    |> fun money -> money.Currency = currency

  let private isNotIn currency = isIn currency >> not

  let inCurrency currency transactions =
    transactions
    |> transactionList
    |> List.filter (isIn currency)
    |> Transactions

  let notInCurrency currency transactions =
    transactions
    |> transactionList
    |> List.filter (isNotIn currency)
    |> Transactions

  let amount currency transactions =
    let aggregate moneySoFar transaction = moneySoFar + (transaction |> money)

    ({ Amount = 0; Currency = currency }, transactions |> transactionList)
    ||> List.fold aggregate

  let add transaction balance =
    balance.Transactions
    |> transactionList
    |> fun transactions ->
         { balance with
             Transactions = Transactions <| transaction :: transactions }
