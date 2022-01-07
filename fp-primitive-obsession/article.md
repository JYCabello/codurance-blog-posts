# Calisthenics? Where we're going we don't need calisthenics

I try to be consistent, and sometimes I make wild assesments that I love seeing getting challenged. Well, I asserted pretty much the title of the article, it does have a context and there was a challenge (thank you, Christopher Eyre).

I was looking for a Kata to do as my onboarding in Codurance, since the whole katas and TDD it's been a core part of most of my career (I did got hooked on the Craftmanship concept many years ago, thanks to Sandro's book) and Chris suggested doing [primitive obsession](https://williamdurand.fr/2013/06/03/object-calisthenics/) with object calisthenics.

Now, I can, sometimes (more than some) be what in Spain is called a "bocachancla" which would translate as a "big mouth" and I went on to state that I do functional programing and calisthenics is something that just happens automagically when you do functional programming then he suggested to do so and write an article explaining why is that the case. To make things more interesting, I would take a functional first language, F# is my weapon of choice.

I did the exercise, in case you are one of those that likes to peek at the end of the book: No, I did not end with a "calisthenistic" piece of code.

If you don't like to peek at the end of the book: Books are long, and the amount of books you can read in your life is limited, start peeking, it will save you a lot of time.

I would not be a true "bocachancla" if I didn't double down though, so I'm going to guide you through the process I followed, we'll take a look at the resulting code and try and explain the points that are not met. I will skip the changes I did in the tests as they are pretty much the same as what you would do in object oriented programming, with the notable exception of the last step.

## The excercise

> Disclaimer: All indentation follow Fantomas' standard code convention, I try to avoid indenting myself, I let the machine do it.

> Also: Everything is statically typed in F#, types don't need to be anotated if the inference can figure what you are using, there are camps advocating to annotate all types at war with camps that go for full inference. The claim for annotation about readability is weak, but it feels like it's strong when you come from fully annotated languages. There's a compile time improvement when annotating also, but also considered neglectible. Strongest points for and against are pretty much: "Type inference can, in very strange ocasions, produce unexpected effects and they are hard to track"; "With such a clean syntax, adding annotations dillutes the focus on the logic itself"; "Annotating makes refactoring harder, as often you produce elevated types out of a base one which has similar behaviour and changes are minimal". Since refactoring tooling for F# is not its strongest feature, the last bit has put me on camp do-not-annotate. I honestly think it's a matter of taste and the only objective thing I can say is: Whatever you choose, do it consistently across the codebase.

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
// I decided to call it transaction
type Transaction =
| Incoming of Money
| Outgoing of Money
```
This is really doing points 5 and 6.
### 7. Change ProfitCalculator and its tests to use Item
```fs
type ProfitCalculator(localCurrency: Currency) =

//...

  member _.add transaction =
    let money =
      match transaction with
      | Incoming i -> i
      | Outgoing o -> { o with Amount = -o.Amount }

    if money.Currency = localAmount.Currency then
      do localAmount <- add localAmount money
    else
      do foreignAmount <- add foreignAmount money
//...
```
Showing one of the tests for this for the sake of those unfamiliar with discriminated unions
```fs
[<Fact>]
let ``Handles outgoins`` () =
  let calculator = ProfitCalculator(GBP)

  calculator.add
  <| Incoming { Amount = 500; Currency = GBP }

  calculator.add
  <| Incoming { Amount = 80; Currency = USD }

  calculator.add
  <| Outgoing { Amount = 360; Currency = EUR }

  Assert.Equal({ Amount = 150; Currency = GBP }, calculator.calculateProfit)
  Assert.Equal({ Amount = 100; Currency = GBP }, calculator.calculateTax)
```
### 8. Create an Items first class collection and store each Item added to ProfitCalculator
I cheated here, already saw that I was heading towards and assumed the advantages of having an items first class type would come automatically later on.
### 9. Create boolean isIn(Currency) method in Item
The tendency in functional programing is to separate data from logic, so implemented a function in the finance module that takes a currency and a transaction.
```fs
module Finance =
//...
  let isIn currency =
    function
    | Incoming i -> i.Currency = currency
    | Outgoing o -> o.Currency = currency

  // This is shorthand for:
  let isIn currency transaction =
    match transaction with
    | Incoming i -> i.Currency = currency
    | Outgoing o -> o.Currency = currency
//...
```
For those coming from a more OOP background, I'll explain this a bit.
The type of this function is:
```fs
Currency -> Transaction -> bool
// This parameter order looks hard to read from a class method perspective,
// because a method would look like:
let inEuros = transaction.isIn(EUR)
// And a regular tupled function would look like:
let inEuros = isIn EUR transaction
// F# allow us to pipe parameters:
let inEuros =
  transaction
  |> isIn EUR
// And to partially apply functions
let isInEur = isIn EUR
let inEuros =
  transaction
  |> isInEur
```
### 10. Create Money amountIn(Currency) in Items
```fs
module Finance =
//...
  let amountIn currency transactions =
    ({ Amount = 0; Currency = currency }, transactions |> List.filter (isIn currency))
    ||> List.fold
          (fun acc trx ->
            let money =
              match trx with
              | Incoming i -> i
              | Outgoing o -> o

            add acc money)
//...
```
### 11. Change ProfitCalculator.calculateTax() to use methods created in steps 9 and 10
```fs
type ProfitCalculator(localCurrency: Currency) =
//...
  member _.calculateTax =
    match amountIn localAmount.Currency transactions with
    | money when money.Amount < 0 -> { money with Amount = 0 }
    | money ->
      { money with
          Amount = ((money.Amount |> float) * 0.2) |> int }
//...
```
### 12. Remove localAmount field from ProfitCalculator, making necessary changes
```fs
module Finance =
//...
  type Money =
    { Amount: int
      Currency: Currency }

    // Converted the "add" function to a "+" operator.
    static member (+)(local: Money, other: Money) =
      other.Currency >>=> local.Currency
      |> applyRate other.Amount
      |> fun amount ->
           { Amount = local.Amount + amount
             Currency = local.Currency }
//...
open Finance

type ProfitCalculator(localCurrency: Currency) =

  let mutable foreignAmount = { Amount = 0; Currency = localCurrency }
  let mutable transactions: Transaction list = []

  member _.add transaction =
    transactions <- transaction :: transactions

    let money =
      match transaction with
      | Incoming i -> i
      | Outgoing o -> o

    if money.Currency = localCurrency |> not then
      do foreignAmount <- foreignAmount + money

  member _.calculateTax =
    match amountIn localCurrency transactions with
    | money when money.Amount < 0 -> { money with Amount = 0 }
    | money ->
      { money with
          Amount = ((money.Amount |> float) * 0.2) |> int }

  member this.calculateProfit =
    let tax = this.calculateTax
    amountIn localCurrency transactions
    + foreignAmount
    + { tax with
          Amount = -tax.Amount }
```
Saving the `Transaction list` bit, this is getting already pretty into the calisthenics domain.
### 13. Create Items notIn(currency) and Money amountIn(Currency, ExchangeRates) in Items
```fs
module Finance =
//...
  let isIn currency =
    function
    | Incoming i -> i.Currency = currency
    | Outgoing o -> o.Currency = currency

  // Composing (>>) the function "isIn currency" with the function "not"
  // gives us "isNotIn currency", talk about readability.
  let isNotIn currency = isIn currency >> not

  let private amount currency transactions =
    ({ Amount = 0; Currency = currency }, transactions)
    ||> List.fold
          (fun acc trx ->
            let money =
              match trx with
              | Incoming i -> i
              | Outgoing o -> o

            acc + money)

  let amountIn currency transactions =
    amount currency (transactions |> List.filter (isIn currency))

  let amountNotIn currency transactions =
    amount currency (transactions |> List.filter (isNotIn currency))
//...
```
### 14. Simplify ProfitCalculator, removing all the logic from add(Item). calculateProfit() must be simple
```fs
type ProfitCalculator(localCurrency: Currency) =
  let mutable transactions: Transaction list = []

  member _.add transaction =
    transactions <- transaction :: transactions

  member _.calculateTax =
    match amountIn localCurrency transactions with
    | money when money.Amount < 0 -> { money with Amount = 0 }
    | money ->
      { money with
          Amount = ((money.Amount |> float) * 0.2) |> int }

  member this.calculateProfit =
    let tax = this.calculateTax

    amountIn localCurrency transactions
    + amountNotIn localCurrency transactions
    + { tax with
          Amount = -this.calculateTax.Amount }
```
Simple enough, but we are not done, because this is not functional, calling `add` several times will give us different resuls, which leads us tho the final step:
### 15. Make it functional
Functional programing is about referential transparency, which is the capacity of replacing any function with certain parameters with its output. We cannot do that with an object that holds state and changes every time you perform a certain action, so we will have a new type, called `Balance` and we will have our `ProfitCalculator` become a set of operations over this data in order to calculate profit, returing a [new Balance](https://www.newbalance.com/) every time we add a transaction.

This made quite a few smells rather obvious.

Finally, made some sub-modules. Turned `Finance` into a namespace and split the modules in files, but only in the [repository](https://github.com/JYCabello/codurance-blog-posts/tree/main/fp-primitive-obsession).
```fs
module Finance =
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


  module Trading =
    open Currencies

    type Transaction =
      | Incoming of Money
      | Outgoing of Money

    // The "money" function I skipped before, I found a use for it now.
    let money =
      function
      | Incoming incoming -> incoming
      | Outgoing outgoing -> outgoing

    // Ended up understanding that my "I see where I'm going" was wrong,
    // introduced a first class collection for transactions.
    type Transactions = Transactions of Transaction list

    let transactionList =
      function
      | Transactions transactions -> transactions


  module Accounting =
    open Currencies
    open Trading

    // The aforementioned entity, which will replace the Items collection suggested in the excercise.
    type Balance =
      { Transactions: Transactions
        LocalCurrency: Currency }

    let isIn currency transaction =
      transaction
      |> money
      |> fun money -> money.Currency = currency

    let isNotIn currency = isIn currency >> not

    let amount currency transactions =
      let aggregate moneySoFar transaction = moneySoFar + (transaction |> money)

      ({ Amount = 0; Currency = currency }, transactions)
      ||> List.fold aggregate

    let add transaction balance =
      balance.Transactions
      |> transactionList
      |> fun transactions ->
          { balance with
              Transactions = Transactions <| transaction :: transactions }


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
    open Taxes

    let add transaction balance = balance |> add transaction

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

// Then, the longest test to illustrate a point I'll make later:
// In functional programming, we are in control of the flow.
[<Fact>]
let ``Everything is reported in the local currency`` () =
  // Every piping takes the result of the previous addition and
  // passes it to the next call. We own the data flow, we do every
  // operation with the result of the previous one.
  let balance =
    eurBalance
    |> Profits.add (Incoming { Amount = 400; Currency = GBP })
    |> Profits.add (Outgoing { Amount = -200; Currency = USD })
    |> Profits.add (Incoming { Amount = 200; Currency = EUR })

  Assert.Equal({ Amount = 491; Currency = EUR }, Profits.calculateProfit balance)
  Assert.Equal({ Amount = 40; Currency = EUR }, Profits.calculateTax balance)
```
## So, what about calisthenics?
1. Only One Level Of Indentation Per Method ✅

    When it comes to the problem this point tries to solve: cognitive load, it complies. Every line is a statement that collapses into a single type of value and every branch can be read independently of the rest of the function. In fact, everything that is indented is technically a single line, it's just a code convention indentation.

    I did consider "cheating" and making them one liners, but it makes the code less readable, defeating the goal of reducing cognitive load.
1. Don’t Use The ELSE Keyword ❔

    "Technically" not met, because there's quite a bit of pattern matching around and that's equivalent to switches with a simple return for every case. Again this rule is about reducing cognitive load and pattern matching does exactly that: Give you a single function to read per case, reducing the amount of code you have to load in your brain to understand what's going on.
1. Wrap All Primitives And Strings ✅

    Some cases are not even wrapping, but statically typed discriminated unions.
1. First Class Collections ✅
1. One Dot Per Line ✅
1. Don’t Abbreviate ✅

    I didn't abbreviate, but this is, and I'm not trying to start a holy war here, possibly a pointless exercise. In an [impressively thorough study](http://www2.unibas.it/gscanniello/Giuseppe_Scanniello%40unibas/Home_files/TOSEM.pdf) from 2017, the team could find no difference on time taken or quality when it came to debugging. Mind you, the study was only about bug fixing, but it's the only empirical thing I've found about it.

    It would seem like it's a matter of taste. I'm not advocating to abreviate, mind you, if there's no difference and we are already used to not doing, it's wasteful to change, as Miyamoto Musashi put it in "The Book Of Five Rings": Do nothing which is of no use.
1. Keep All Entities Small ✅
1. No Classes With More Than Two Instance Variables ✅

    No mutation allowed, hence no instance variables whatsoever.
1. No Getters/Setters/Properties ❌

    No such tihng as setters, as records are immutable, but since logic and data live separatedly, records forcibly make their data readable.

    Again, the purpose of the item is not violated. Encapsulation is meant to protect from unwanted access to data, but with immutability there's no real "access", but the creation of copies. Consumers of logic are in absolute control and functions are black boxes that take data and return data.

Seven out of nine points are thoroughly and easily met, another one is virtually met and the last one is unnatainable at the core of the paradigm, but the goal chased is achieved in other way.

Functional programming is a paradigm that comes at a cost. Functional code is usually slower, the initial learning curve is quite steep and requires a complete rewiring of some hard-earned muscle memory, but the value we get out of it on a regular basis is the same we get of some extreme design exercise.

I can be a "bocachancla" but I might get away with this one.
