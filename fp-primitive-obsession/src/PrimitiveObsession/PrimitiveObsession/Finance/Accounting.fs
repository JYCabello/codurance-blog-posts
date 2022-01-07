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
