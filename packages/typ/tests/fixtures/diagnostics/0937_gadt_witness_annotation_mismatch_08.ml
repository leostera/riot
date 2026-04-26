type _ witness_theta =
  | WInt_theta : int witness_theta
  | WBool_theta : bool witness_theta

let _ : bool witness_theta = WInt_theta
