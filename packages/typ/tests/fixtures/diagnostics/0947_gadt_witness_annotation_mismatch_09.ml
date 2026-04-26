type _ witness_iota =
  | WInt_iota : int witness_iota
  | WBool_iota : bool witness_iota

let _ : bool witness_iota = WInt_iota
