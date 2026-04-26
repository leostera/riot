type _ witness_alpha =
  | WInt_alpha : int witness_alpha
  | WBool_alpha : bool witness_alpha

let _ : bool witness_alpha = WInt_alpha
