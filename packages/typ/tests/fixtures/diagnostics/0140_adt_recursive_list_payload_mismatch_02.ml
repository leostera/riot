type 'a mylist_beta = Nil_beta | Cons_beta of 'a * 'a mylist_beta
let _ : int mylist_beta = Cons_beta (true, Nil_beta)
