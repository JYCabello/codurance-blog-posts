# Calisthenics? Where we're going we don't need calisthenics

I try to be consistent, and sometimes I make wild assesments that I love seeing getting challenged. Well, I asserted pretty much the title of the article, it does have a context and there was a challenge (thank you, Christopher Eyre).

I was looking for a Kata to do as my onboarding in Codurance, since the whole katas and TDD it's been a core part of most of my career (I did got hooked on the Craftmanship concept many years ago, thanks to Sandro's book) and Chris suggested doing [primitive obsession](https://williamdurand.fr/2013/06/03/object-calisthenics/) with object calisthenics.

Now, I can, sometimes (more than some) be what in Spain is called a "bocachancla" which would translate as a "big mouth" and I went on to state that I do FP and calisthenics is something that just happens automagically when you are doing so and he suggested to do so and write an article explaining why is that so. To make things more interesting, I would take a functional first language, F# is my weapon of choice.

I did the exercise, here is the article with a free spoiler: No, I did not end with a "calisthenistic" piece of code.

I would not be a true "bocachancla" if I didn't double down though, so I'm going to guide you through the process I followed, we'll take a look at the resulting code and try and explain the points that are not met. I will skip the changes I did in the tests as they are pretty much the same as what you would do in object oriented programming, with the notable exception of the last step.

## The excercise

> Disclaimer: All indentation follow Fantomas' standard code convention, I try to avoid indenting myself, I let the machine do it.

> Also: Everything is statically typed in F#, types don't need to be anotated if the inference can figure what you are using.

First, I replicated the excercise in F#, being a language without the possibility of early returns, some things look different, but behave the same way:

```fs
type ProfitCalculatorOriginal(localCurrency: string) =
  let rates =
    [ ("GBP", 1.0)
      ("USD", 1.6)
      ("EUR", 1.2) ]
    |> Map.ofSeq

  let mutable localAmount = 0
  let mutable foreignAmount = 0

  do
    try
      rates.[localCurrency] |> ignore
    with
    | _ -> invalidArg (nameof localCurrency) "Was not a valid currency"

  member _.add amount currency incoming =
    let mutable realAmount: int = amount

    let exchangeRate =
      rates.TryFind currency
      |> Option.map (fun incomingRate -> incomingRate / rates.[localCurrency])

    realAmount <-
      exchangeRate
      |> Option.map (fun rate -> ((realAmount |> float) / rate) |> int)
      |> Option.defaultValue realAmount

    if not incoming then
      do realAmount <- -realAmount

    if localCurrency = currency then
      do localAmount <- localAmount + realAmount
    else
      do foreignAmount <- foreignAmount + realAmount

  member _.calculateTax =
    match localAmount with
    | amount when amount < 0 -> 0
    | amount -> ((amount |> float) * 0.2) |> int

  member this.calculateProfit =
    localAmount - this.calculateTax + foreignAmount
```
### 1. Introduce a Currency class or enum; Use it on ProfitCalculator
For these kind of situations, we use discriminated unions in F#:
```fs
type Currency =
  | GBP
  | USD
  | EUR

type ProfitCalculator(localCurrency: Currency) =
  // As now we are using a discriminated union
  // we match the currency instead of using an index
  let getRate =
    function
    | GBP -> 1.0
    | USD -> 1.6
    | EUR -> 1.2

  let mutable localAmount = 0
  let mutable foreignAmount = 0

  member _.add amount currency incoming =
    let mutable realAmount: int = amount

    let exchangeRate = (getRate currency) / (getRate localCurrency)

    realAmount <- ((realAmount |> float) / exchangeRate) |> int

    if not incoming then
      do realAmount <- -realAmount

    if localCurrency = currency then
      do localAmount <- localAmount + realAmount
    else
      do foreignAmount <- foreignAmount + realAmount
//...
```
### 2. Create an ExchageRates first class collection; Use it on ProfitCalculator
This really does not apply to discriminated unions, as we no longer need a collection for it, the pattern matching itself acts as a map for the values with exhaustivenes enforced by the compiler. So I went ahead and introduce a type for the rates.
```fs
// One of the brilliant things of F# is how easy it is to wrap primitives.
type ExchangeRate =
  | Rate of float

  // This is an overload of the division operator with pattern matching
  // (Rate a, Rate b) will match when both elements are of subtype `Rate`
  // which will always happen, because we only have one subtype.
  // The beauty of this is that this operator will not compile if we add a different
  // subtype and forget to add operators to match other cases.
  static member (/)(Rate a, Rate b) = (a / b) |> Rate

  static member get =
    function
    | GBP -> Rate 1.0
    | USD -> Rate 1.6
    | EUR -> Rate 1.2

//...

type ProfitCalculator(localCurrency: Currency) =

  let mutable localAmount = 0
  let mutable foreignAmount = 0

  let applyRate amount =
    function
    | Rate rate -> ((amount |> float) / rate) |> int

  member _.add amount currency incoming =
    let mutable realAmount: int = amount

    let exchangeRate =
      (ExchangeRate.get currency)
      / (ExchangeRate.get localCurrency)

    realAmount <- applyRate realAmount exchangeRate

//...
```
### 3. Create a Money class. Identify all amount operations used by ProfitCalculator and add them to it

```fs
// In order to do this bit, I needed an operation to get the conversion rate between currencies
// I figured that this did not belogn into a profit calculator, so I created the "Finance" module
// and started moving logic and data in there.
module Finance =
  type Currency =
    | GBP
    | USD
    | EUR

  type ExchangeRate =
    | Rate of float

    static member (/)(Rate a, Rate b) = (a / b) |> Rate

    static member get =
      function
      | GBP -> Rate 1.0
      | USD -> Rate 1.6
      | EUR -> Rate 1.2

  // The aforementioned operation to get the rate:
  let (>>=>) (a: Currency) (b: Currency) = ExchangeRate.get a / ExchangeRate.get b

  type Money = { Amount: int; Currency: Currency }

  let add local other =
    other.Currency >>=> local.Currency
    |> applyRate other.Amount
    |> fun amount -> { Amount = local.Amount + amount; Currency = local.Currency }
```
### 4. Change ProfitCalculator and its tests to use the Money class
Straightforward, led to a much smaller class.
```fs
open Finance


type ProfitCalculator(localCurrency: Currency) =

  let mutable localAmount = { Amount = 0; Currency = localCurrency }
  let mutable foreignAmount = { Amount = 0; Currency = localCurrency }

  member _.add money incoming =
    let money = if incoming then money else { money with Amount = -money.Amount }

    if money.Currency = localAmount.Currency then
      do localAmount <- add localAmount money
    else
      do foreignAmount <- add foreignAmount money

  member _.calculateTax =
    match localAmount.Amount with
    | amount when amount < 0 -> { Amount = 0; Currency = localCurrency }
    | amount -> { Amount = ((amount |> float) * 0.2) |> int; Currency = localCurrency }

  member this.calculateProfit =
    foreignAmount
    // This is the "add" operation from the finance module in the previous step.
    |> add localAmount
    |> add { this.calculateTax with Amount = -this.calculateTax.Amount }
```
### 5. Create an abstract Item class with the method Money amount();
### 6. Create classes Outgoing and Incoming implementing Item. Outgoing has negative amount
Inheritance is disencouraged in FP, however discriminated unions, as you have seen...
```fs
type Transaction =
| Incoming of Money
| Outgoing of Money
```
This is really doing points 5 and 6.
