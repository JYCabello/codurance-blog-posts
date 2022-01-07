# Calisthenics? Where we're going we don't need calisthenics

I try to be consistent, and sometimes I make wild assesments that I love seeing getting challenged. Well, I asserted pretty much the title of the article, it does have a context and there was a challenge (thank you, Christopher Eyre).

I was looking for a Kata to do as my onboarding in Codurance, since the whole katas and TDD it's been a core part of most of my career (I did got hooked on the Craftmanship concept many years ago, thanks to Sandro's book) and Chris suggested doing [primitive obsession](https://williamdurand.fr/2013/06/03/object-calisthenics/) with object calisthenics.

Now, I can, sometimes (more than some) be what in Spain is called a "bocachancla" which would translate as a "big mouth" and I went on to state that I do FP and calisthenics is something that just happens automagically when you are doing so and he suggested to do so and write an article explaining why is that so. To make things more interesting, I would take a functional first language, F# is my weapon of choice.

I did the exercise, here's the article, and here's a spoiler: No, I did not end with a "calisthenistic" piece of code.

I would not be a true "bocachancla" if I didn't double down though, so I'm going to guide you through the process I followed, we'll take a look at the resulting code and try and explain the points that are not met. I will skip the changes I did in the tests as they are pretty much the same as what you would do in object oriented programming, with the notable exception of the last step.

## The excercise

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
###