type _ witness_delta =
  | WInt_delta : int witness_delta
  | WBool_delta : bool witness_delta

let _ : bool witness_delta = WInt_delta
