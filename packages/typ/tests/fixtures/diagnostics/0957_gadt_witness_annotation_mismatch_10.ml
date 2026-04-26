type _ witness_kappa =
  | WInt_kappa : int witness_kappa
  | WBool_kappa : bool witness_kappa

let _ : bool witness_kappa = WInt_kappa
