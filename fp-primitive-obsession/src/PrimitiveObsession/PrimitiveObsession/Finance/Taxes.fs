namespace PrimitiveObsession.Finance

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
