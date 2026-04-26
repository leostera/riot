type _ witness_eta =
  | WInt_eta : int witness_eta
  | WBool_eta : bool witness_eta

let _ : bool witness_eta = WInt_eta
