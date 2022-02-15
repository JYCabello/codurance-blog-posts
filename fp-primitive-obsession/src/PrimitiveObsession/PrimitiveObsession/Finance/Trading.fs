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
