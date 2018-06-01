open Core_kernel

type t =
  [ `As3
  | `C
  | `Cfml
  | `Cocoa
  | `Csharp
  | `Go
  | `Java
  | `Javascript
  | `Node
  | `Objc
  | `Other
  | `Perl
  | `Php
  | `Python
  | `Ruby ]
[@@deriving sexp_of]

let unwrap = function
  | `As3 -> "as3"
  | `C -> "c"
  | `Cfml -> "cfml"
  | `Cocoa -> "cocoa"
  | `Csharp -> "csharp"
  | `Go -> "go"
  | `Java -> "java"
  | `Javascript -> "javascript"
  | `Node -> "node"
  | `Objc -> "objc"
  | `Other -> "other"
  | `Perl -> "perl"
  | `Php -> "php"
  | `Python -> "python"
  | `Ruby -> "ruby"

let wrap = function
  | "as3" -> `As3
  | "c" -> `C
  | "cfml" -> `Cfml
  | "cocoa" -> `Cocoa
  | "csharp" -> `Csharp
  | "go" -> `Go
  | "java" -> `Java
  | "javascript" -> `Javascript
  | "node" -> `Node
  | "objc" -> `Objc
  | "other" -> `Other
  | "perl" -> `Perl
  | "php" -> `Php
  | "python" -> `Python
  | "ruby" -> `Ruby
  | s -> failwithf "Unknown platform %s" s ()
