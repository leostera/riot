open Global

(**
   XML construction and rendering.

   A small XML tree builder and serializer for generating XML documents.
   Supports elements, escaped text nodes, and CDATA sections.

   ## Example

   ```ocaml
   let doc =
     Xml.element "greeting" ~attrs:[ ("lang", "en") ] [ Xml.text "hello <world>" ]
   in
   let xml = Xml.declaration ^ "\n" ^ Xml.to_string doc
   ```
*)

(** XML node representation. *)
type t =
  | Element of {
      name: string;
      attrs: (string * string) list;
      children: t list;
    }
  | Text of string
  | CData of string

type error =
  | Parse_error of {
      message: string;
      offset: int;
    }

(**
   Creates an XML element with optional attributes and child nodes.

   ## Example

   ```ocaml
   let node =
     Xml.element "user"
       ~attrs:[ ("id", "42") ]
       [ Xml.element "name" [ Xml.text "Alice" ] ]
   ```
*)
val element: string -> ?attrs:(string * string) list -> t list -> t

(**
   Creates an escaped text node. Special XML characters are escaped
   automatically.

   ## Example

   ```ocaml
   let node = Xml.text "5 < 10 & 10 > 5"
   ```
*)
val text: string -> t

(**
   Creates a CDATA node without escaping its contents.

   ## Example

   ```ocaml
   let node = Xml.cdata {|if (a < b) return "ok";|}
   ```
*)
val cdata: string -> t

(** Returns the first attribute with [name] on an element. *)
val attr: string -> t -> string option

(** Returns an element's children, or [[]] for text and CDATA nodes. *)
val children: t -> t list

(** Returns all direct element children, optionally narrowed by element name. *)
val child_elements: ?name:string -> t -> t list

(** Concatenates text and CDATA descendants into one decoded string. *)
val text_content: t -> string

(**
   Serializes an XML node using two-space indentation.

   ## Example

   ```ocaml
   let xml =
     Xml.element "root" [ Xml.element "child" [ Xml.text "value" ] ]
     |> Xml.to_string
   ```
*)
val to_string: ?indent:int -> t -> string

(**
   Standard XML declaration: [<?xml version="1.0" encoding="UTF-8"?>].

   ## Example

   ```ocaml
   let xml = Xml.declaration ^ "\n" ^ Xml.to_string (Xml.element "root" [])
   ```
*)
val declaration: string

(** Parses one XML document from a string. *)
val from_string: string -> (t, error) result

val error_message: error -> string
