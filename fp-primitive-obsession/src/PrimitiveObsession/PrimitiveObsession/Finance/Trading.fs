namespace PrimitiveObsession.Finance

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
