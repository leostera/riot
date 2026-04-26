type 'a mylist_iota = Nil_iota | Cons_iota of 'a * 'a mylist_iota
let _ : int mylist_iota = Cons_iota (true, Nil_iota)
