type 'a mylist_epsilon = Nil_epsilon | Cons_epsilon of 'a * 'a mylist_epsilon
let _ : int mylist_epsilon = Cons_epsilon (true, Nil_epsilon)
