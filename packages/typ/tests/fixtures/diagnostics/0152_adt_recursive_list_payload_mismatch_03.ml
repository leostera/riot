type 'a mylist_gamma = Nil_gamma | Cons_gamma of 'a * 'a mylist_gamma
let _ : int mylist_gamma = Cons_gamma (true, Nil_gamma)
