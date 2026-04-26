type 'a mylist_theta = Nil_theta | Cons_theta of 'a * 'a mylist_theta
let _ : int mylist_theta = Cons_theta (true, Nil_theta)
