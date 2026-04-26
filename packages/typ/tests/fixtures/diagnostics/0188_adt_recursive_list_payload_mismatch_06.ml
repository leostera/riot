type 'a mylist_zeta = Nil_zeta | Cons_zeta of 'a * 'a mylist_zeta
let _ : int mylist_zeta = Cons_zeta (true, Nil_zeta)
