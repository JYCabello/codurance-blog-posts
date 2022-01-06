module Tests

open System
open PrimitiveObsession
open Xunit

let eurCalculator = ProfitCalculator("EUR")

[<Fact>]
let ``Calculates the tax at 20 percent`` () =
    let calculator = ProfitCalculator("GBP")
    calculator.add 500 "GBP" true
    Assert.Equal(400, calculator.calculateProfit)
    Assert.Equal(100, calculator.calculateTax)
