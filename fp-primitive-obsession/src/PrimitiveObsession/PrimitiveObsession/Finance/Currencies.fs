namespace PrimitiveObsession.Finance

module Currencies =
  type Currency =
    | GBP
    | USD
    | EUR

  type ExchangeRate =
    | Rate of float

    static member (/)(Rate a, Rate b) = (a / b) |> Rate

  module ExchangeRate =
    let get =
      function
      | GBP -> Rate 1.0
      | USD -> Rate 1.6
      | EUR -> Rate 1.2

  let private (>>=>) source destination =
    ExchangeRate.get source
    / ExchangeRate.get destination

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
