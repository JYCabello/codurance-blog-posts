module PrimitiveObsession.Test.Excercise

open PrimitiveObsession
open PrimitiveObsession.Finance
open Accounting
open Currencies
open Trading
open Xunit

let gbpBalance =
  { Transactions = [] |> Transactions
    LocalCurrency = GBP }

let eurBalance =
  { Transactions = [] |> Transactions
    LocalCurrency = EUR }

[<Fact>]
let ``Calculates the tax at 20 percent`` () =
  let balance =
    gbpBalance
    |> Profits.add (Incoming { Amount = 500; Currency = GBP })

  Assert.Equal({ Amount = 400; Currency = GBP }, Profits.calculateProfit balance)
  Assert.Equal({ Amount = 100; Currency = GBP }, Profits.calculateTax balance)

[<Fact>]
let ``Calculates the tax of multiple amounts`` () =
  let balance =
    gbpBalance
    |> Profits.add (Incoming { Amount = 120; Currency = GBP })
    |> Profits.add (Incoming { Amount = 200; Currency = GBP })

  Assert.Equal({ Amount = 256; Currency = GBP }, Profits.calculateProfit balance)
  Assert.Equal({ Amount = 64; Currency = GBP }, Profits.calculateTax balance)

[<Fact>]
let ``Different currencies are not taxed`` () =
  let balance =
    gbpBalance
    |> Profits.add (Incoming { Amount = 120; Currency = GBP })
    |> Profits.add (Incoming { Amount = 200; Currency = USD })

  Assert.Equal({ Amount = 221; Currency = GBP }, Profits.calculateProfit balance)
  Assert.Equal({ Amount = 24; Currency = GBP }, Profits.calculateTax balance)

[<Fact>]
let ``Handles outgoins`` () =
  let balance =
    gbpBalance
    |> Profits.add (Incoming { Amount = 500; Currency = GBP })
    |> Profits.add (Incoming { Amount = 80; Currency = USD })
    |> Profits.add (Outgoing { Amount = -360; Currency = EUR })

  Assert.Equal({ Amount = 150; Currency = GBP }, Profits.calculateProfit balance)
  Assert.Equal({ Amount = 100; Currency = GBP }, Profits.calculateTax balance)

[<Fact>]
let ``A negative balance results in no tax`` () =
  let balance =
    gbpBalance
    |> Profits.add (Incoming { Amount = 500; Currency = GBP })
    |> Profits.add (Outgoing { Amount = -200; Currency = GBP })
    |> Profits.add (Outgoing { Amount = -400; Currency = GBP })
    |> Profits.add (Outgoing { Amount = -20; Currency = GBP })

  Assert.Equal({ Amount = -120; Currency = GBP }, Profits.calculateProfit balance)
  Assert.Equal({ Amount = 0; Currency = GBP }, Profits.calculateTax balance)

[<Fact>]
let ``Everything is reported in the local currency`` () =
  let balance =
    eurBalance
    |> Profits.add (Incoming { Amount = 400; Currency = GBP })
    |> Profits.add (Outgoing { Amount = -200; Currency = USD })
    |> Profits.add (Incoming { Amount = 200; Currency = EUR })

  Assert.Equal({ Amount = 491; Currency = EUR }, Profits.calculateProfit balance)
  Assert.Equal({ Amount = 40; Currency = EUR }, Profits.calculateTax balance)
