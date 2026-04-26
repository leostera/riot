type _ witness_beta =
  | WInt_beta : int witness_beta
  | WBool_beta : bool witness_beta

let _ : bool witness_beta = WInt_beta
