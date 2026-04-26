type _ witness_epsilon =
  | WInt_epsilon : int witness_epsilon
  | WBool_epsilon : bool witness_epsilon

let _ : bool witness_epsilon = WInt_epsilon
