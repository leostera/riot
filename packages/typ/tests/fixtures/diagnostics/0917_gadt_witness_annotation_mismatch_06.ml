type _ witness_zeta =
  | WInt_zeta : int witness_zeta
  | WBool_zeta : bool witness_zeta

let _ : bool witness_zeta = WInt_zeta
