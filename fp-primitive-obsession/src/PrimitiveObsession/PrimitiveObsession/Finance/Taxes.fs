namespace PrimitiveObsession.Finance

module Taxes =
  open Accounting

  // It became obvious that the point of these functions was to get amounts to
  // be taxed or not.
  let taxableAmount balance =
    amount
      balance.LocalCurrency
      (balance.Transactions
       |> inCurrency balance.LocalCurrency)

  let taxFreeAmount balance =
    amount
      balance.LocalCurrency
      (balance.Transactions
       |> notInCurrency balance.LocalCurrency)
