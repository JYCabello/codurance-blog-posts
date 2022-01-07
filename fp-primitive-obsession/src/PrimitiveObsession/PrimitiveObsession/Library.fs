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
