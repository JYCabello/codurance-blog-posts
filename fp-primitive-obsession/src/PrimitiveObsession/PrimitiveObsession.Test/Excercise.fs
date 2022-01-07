module PrimitiveObsession.Test.Excercise

open PrimitiveObsession.Excercise
open PrimitiveObsession.Excercise.Finance
open Xunit

let gbpBalance =
  { Transactions = []
    LocalCurrency = GBP }

let eurBalance =
  { Transactions = []
    LocalCurrency = EUR }

[<Fact>]
let ``Calculates the tax at 20 percent`` () =
  let balance =
    gbpBalance
    |> ProfitCalculator.add (Incoming { Amount = 500; Currency = GBP })

  Assert.Equal({ Amount = 400; Currency = GBP }, ProfitCalculator.calculateProfit balance)
  Assert.Equal({ Amount = 100; Currency = GBP }, ProfitCalculator.calculateTax balance)

[<Fact>]
let ``Calculates the tax of multiple amounts`` () =
  let balance =
    gbpBalance
    |> ProfitCalculator.add (Incoming { Amount = 120; Currency = GBP })
    |> ProfitCalculator.add (Incoming { Amount = 200; Currency = GBP })

  Assert.Equal({ Amount = 256; Currency = GBP }, ProfitCalculator.calculateProfit balance)
  Assert.Equal({ Amount = 64; Currency = GBP }, ProfitCalculator.calculateTax balance)

[<Fact>]
let ``Different currencies are not taxed`` () =
  let balance =
    gbpBalance
    |> ProfitCalculator.add (Incoming { Amount = 120; Currency = GBP })
    |> ProfitCalculator.add (Incoming { Amount = 200; Currency = USD })

  Assert.Equal({ Amount = 221; Currency = GBP }, ProfitCalculator.calculateProfit balance)
  Assert.Equal({ Amount = 24; Currency = GBP }, ProfitCalculator.calculateTax balance)

[<Fact>]
let ``Handles outgoins`` () =
  let balance =
    gbpBalance
    |> ProfitCalculator.add (Incoming { Amount = 500; Currency = GBP })
    |> ProfitCalculator.add (Incoming { Amount = 80; Currency = USD })
    |> ProfitCalculator.add (Outgoing { Amount = -360; Currency = EUR })

  Assert.Equal({ Amount = 150; Currency = GBP }, ProfitCalculator.calculateProfit balance)
  Assert.Equal({ Amount = 100; Currency = GBP }, ProfitCalculator.calculateTax balance)

[<Fact>]
let ``A negative balance results in no tax`` () =
  let balance =
    gbpBalance
    |> ProfitCalculator.add (Incoming { Amount = 500; Currency = GBP })
    |> ProfitCalculator.add (Outgoing { Amount = -200; Currency = GBP })
    |> ProfitCalculator.add (Outgoing { Amount = -400; Currency = GBP })
    |> ProfitCalculator.add (Outgoing { Amount = -20; Currency = GBP })

  Assert.Equal({ Amount = -120; Currency = GBP }, ProfitCalculator.calculateProfit balance)
  Assert.Equal({ Amount = 0; Currency = GBP }, ProfitCalculator.calculateTax balance)

[<Fact>]
let ``Everything is reported in the local currency`` () =
  let balance =
    eurBalance
    |> ProfitCalculator.add (Incoming { Amount = 400; Currency = GBP })
    |> ProfitCalculator.add (Outgoing { Amount = -200; Currency = USD })
    |> ProfitCalculator.add (Incoming { Amount = 200; Currency = EUR })

  Assert.Equal({ Amount = 491; Currency = EUR }, ProfitCalculator.calculateProfit balance)
  Assert.Equal({ Amount = 40; Currency = EUR }, ProfitCalculator.calculateTax balance)
