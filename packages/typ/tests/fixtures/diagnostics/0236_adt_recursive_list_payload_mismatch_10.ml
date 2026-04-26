type 'a mylist_kappa = Nil_kappa | Cons_kappa of 'a * 'a mylist_kappa
let _ : int mylist_kappa = Cons_kappa (true, Nil_kappa)
