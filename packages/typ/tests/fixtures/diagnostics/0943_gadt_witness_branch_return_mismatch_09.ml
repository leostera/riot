type _ witness_iota =
  | WInt_iota : int witness_iota
  | WBool_iota : bool witness_iota

let bad_iota : type a. a witness_iota -> a = function
  | WInt_iota -> true
  | WBool_iota -> false
