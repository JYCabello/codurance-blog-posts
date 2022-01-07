namespace PrimitiveObsession.Finance

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
