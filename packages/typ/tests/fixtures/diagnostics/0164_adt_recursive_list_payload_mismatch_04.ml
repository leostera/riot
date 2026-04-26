type 'a mylist_delta = Nil_delta | Cons_delta of 'a * 'a mylist_delta
let _ : int mylist_delta = Cons_delta (true, Nil_delta)
