module PrimitiveObsession.Test.Excercise

open PrimitiveObsession.Excercise
open PrimitiveObsession.Excercise.Finance
open Xunit


[<Fact>]
let ``Calculates the tax at 20 percent`` () =
  let calculator = ProfitCalculator(GBP)
  calculator.add { Amount = 500; Currency = GBP } true
  Assert.Equal({ Amount = 400; Currency = GBP }, calculator.calculateProfit)
  Assert.Equal({ Amount = 100; Currency = GBP }, calculator.calculateTax)

[<Fact>]
let ``Calculates the tax of multiple amounts`` () =
  let calculator = ProfitCalculator(GBP)
  calculator.add { Amount = 120; Currency = GBP } true
  calculator.add { Amount = 200; Currency = GBP } true
  Assert.Equal({ Amount = 256; Currency = GBP }, calculator.calculateProfit)
  Assert.Equal({ Amount = 64; Currency = GBP }, calculator.calculateTax)

[<Fact>]
let ``Different currencies are not taxed`` () =
  let calculator = ProfitCalculator(GBP)
  calculator.add { Amount = 120; Currency = GBP } true
  calculator.add { Amount = 200; Currency = USD } true
  Assert.Equal({ Amount = 221; Currency = GBP }, calculator.calculateProfit)
  Assert.Equal({ Amount = 24; Currency = GBP }, calculator.calculateTax)

[<Fact>]
let ``Handles outgoins`` () =
  let calculator = ProfitCalculator(GBP)
  calculator.add { Amount = 500; Currency = GBP } true
  calculator.add { Amount = 80; Currency = USD } true
  calculator.add { Amount = 360; Currency = EUR } false
  Assert.Equal({ Amount = 150; Currency = GBP }, calculator.calculateProfit)
  Assert.Equal({ Amount = 100; Currency = GBP }, calculator.calculateTax)

[<Fact>]
let ``A negative balance results in no tax`` () =
  let calculator = ProfitCalculator(GBP)
  calculator.add { Amount = 500; Currency = GBP } true
  calculator.add { Amount = 200; Currency = GBP } false
  calculator.add { Amount = 400; Currency = GBP } false
  calculator.add { Amount = 20; Currency = GBP } false
  Assert.Equal({ Amount = -120; Currency = GBP }, calculator.calculateProfit)
  Assert.Equal({ Amount = 0; Currency = GBP }, calculator.calculateTax)

[<Fact>]
let ``Everything is reported in the local currency`` () =
  let calculator = ProfitCalculator(EUR)
  calculator.add { Amount = 400; Currency = GBP } true
  calculator.add { Amount = 200; Currency = USD } false
  calculator.add { Amount = 200; Currency = EUR } true
  Assert.Equal({ Amount = 491; Currency = EUR }, calculator.calculateProfit)
  Assert.Equal({ Amount = 40; Currency = EUR }, calculator.calculateTax)
