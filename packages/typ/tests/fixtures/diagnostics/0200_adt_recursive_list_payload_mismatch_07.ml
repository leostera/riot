type 'a mylist_eta = Nil_eta | Cons_eta of 'a * 'a mylist_eta
let _ : int mylist_eta = Cons_eta (true, Nil_eta)
