open Std

let main ~args:_ =
  println "Hello, world!";
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
