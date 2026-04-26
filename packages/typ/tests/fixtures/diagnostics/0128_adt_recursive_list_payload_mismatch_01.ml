type 'a mylist_alpha = Nil_alpha | Cons_alpha of 'a * 'a mylist_alpha
let _ : int mylist_alpha = Cons_alpha (true, Nil_alpha)
