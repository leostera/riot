type _ witness_gamma =
  | WInt_gamma : int witness_gamma
  | WBool_gamma : bool witness_gamma

let _ : bool witness_gamma = WInt_gamma
