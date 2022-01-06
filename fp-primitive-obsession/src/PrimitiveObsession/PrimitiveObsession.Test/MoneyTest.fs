module PrimitiveObsession.Test.MoneyTest

open PrimitiveObsession.Excercise.Finance
open Xunit


[<Fact>]
let ``Does not convert for same currency`` () =
  let first = { Amount = 10; Currency = GBP }
  let second = { Amount = 15; Currency = GBP }
  let result = add first second
  Assert.Equal({ Amount = 25; Currency = GBP }, result)

[<Fact>]
let ``Converts between currencies`` () =
  let first = { Amount = 100; Currency = GBP }
  let second = { Amount = 100; Currency = EUR }
  let result = add first second
  Assert.Equal({ Amount = 183; Currency = GBP }, result)
  match result.Currency with
  | GBP -> ()
  | _ -> failwith "Expected GBP"

[<Fact>]
let ``Result has first currency`` () =
  let first = { Amount = 100; Currency = USD }
  let second = { Amount = 100; Currency = EUR }
  let result = add first second
  Assert.Equal({ Amount = 233; Currency = USD }, result)
