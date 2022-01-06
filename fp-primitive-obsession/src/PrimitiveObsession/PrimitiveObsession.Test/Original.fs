module ProfitCalculatorTests

open System
open PrimitiveObsession
open Xunit

let eurCalculator = ProfitCalculatorOriginal("EUR")

[<Fact>]
let ``Calculates the tax at 20 percent`` () =
  let calculator = ProfitCalculatorOriginal("GBP")
  calculator.add 500 "GBP" true
  Assert.Equal(400, calculator.calculateProfit)
  Assert.Equal(100, calculator.calculateTax)

[<Fact>]
let ``Calculates the tax of multiple amounts`` () =
  let calculator = ProfitCalculatorOriginal("GBP")
  calculator.add 120 "GBP" true
  calculator.add 200 "GBP" true
  Assert.Equal(256, calculator.calculateProfit)
  Assert.Equal(64, calculator.calculateTax)

[<Fact>]
let ``Different currencies are not taxed`` () =
  let calculator = ProfitCalculatorOriginal("GBP")
  calculator.add 120 "GBP" true
  calculator.add 200 "USD" true
  Assert.Equal(221, calculator.calculateProfit)
  Assert.Equal(24, calculator.calculateTax)

[<Fact>]
let ``Handles outgoins`` () =
  let calculator = ProfitCalculatorOriginal("GBP")
  calculator.add 500 "GBP" true
  calculator.add 80 "USD" true
  calculator.add 360 "EUR" false
  Assert.Equal(150, calculator.calculateProfit)
  Assert.Equal(100, calculator.calculateTax)

[<Fact>]
let ``A negative balance results in no tax`` () =
  let calculator = ProfitCalculatorOriginal("GBP")
  calculator.add 500 "GBP" true
  calculator.add 200 "GBP" false
  calculator.add 400 "GBP" false
  calculator.add 20 "GBP" false
  Assert.Equal(-120, calculator.calculateProfit)
  Assert.Equal(0, calculator.calculateTax)

[<Fact>]
let ``Everything is reported in the local currency`` () =
  let calculator = ProfitCalculatorOriginal("EUR")
  calculator.add 400 "GBP" true
  calculator.add 200 "USD" false
  calculator.add 200 "EUR" true
  Assert.Equal(491, calculator.calculateProfit)
  Assert.Equal(40, calculator.calculateTax)
