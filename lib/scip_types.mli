(** scip.proto Types *)



(** {2 Types} *)

type protocol_version =
  | Unspecified_protocol_version 

type tool_info = {
  name : string;
  version : string;
  arguments : string list;
}

type text_encoding =
  | Unspecified_text_encoding 
  | Utf8 
  | Utf16 

type metadata = {
  version : protocol_version;
  tool_info : tool_info option;
  project_root : string;
  text_document_encoding : text_encoding;
}

type syntax_kind =
  | Unspecified_syntax_kind 
  | Comment 
  | Punctuation_delimiter 
  | Punctuation_bracket 
  | Keyword 
  | Identifier_keyword 
  | Identifier_operator 
  | Identifier 
  | Identifier_builtin 
  | Identifier_null 
  | Identifier_constant 
  | Identifier_mutable_global 
  | Identifier_parameter 
  | Identifier_local 
  | Identifier_shadowed 
  | Identifier_namespace 
  | Identifier_module 
  | Identifier_function 
  | Identifier_function_definition 
  | Identifier_macro 
  | Identifier_macro_definition 
  | Identifier_type 
  | Identifier_builtin_type 
  | Identifier_attribute 
  | Regex_escape 
  | Regex_repeated 
  | Regex_wildcard 
  | Regex_delimiter 
  | Regex_join 
  | String_literal 
  | String_literal_escape 
  | String_literal_special 
  | String_literal_key 
  | Character_literal 
  | Numeric_literal 
  | Boolean_literal 
  | Tag 
  | Tag_attribute 
  | Tag_delimiter 

type severity =
  | Unspecified_severity 
  | Error 
  | Warning 
  | Information 
  | Hint 

type diagnostic_tag =
  | Unspecified_diagnostic_tag 
  | Unnecessary 
  | Deprecated 

type diagnostic = {
  severity : severity;
  code : string;
  message : string;
  source : string;
  tags : diagnostic_tag list;
}

type occurrence = {
  range : int32 list;
  symbol : string;
  symbol_roles : int32;
  override_documentation : string list;
  syntax_kind : syntax_kind;
  diagnostics : diagnostic list;
}

type relationship = {
  symbol : string;
  is_reference : bool;
  is_implementation : bool;
  is_type_definition : bool;
  is_definition : bool;
}

type symbol_information = {
  symbol : string;
  documentation : string list;
  relationships : relationship list;
}

type document = {
  language : string;
  relative_path : string;
  occurrences : occurrence list;
  symbols : symbol_information list;
}

type index = {
  metadata : metadata option;
  documents : document list;
  external_symbols : symbol_information list;
}

type package = {
  manager : string;
  name : string;
  version : string;
}

type descriptor_suffix =
  | Unspecified_suffix 
  | Namespace 
  | Package 
  | Type 
  | Term 
  | Method 
  | Type_parameter 
  | Parameter 
  | Macro 
  | Meta 
  | Local 

type descriptor = {
  name : string;
  disambiguator : string;
  suffix : descriptor_suffix;
}

type symbol = {
  scheme : string;
  package : package option;
  descriptors : descriptor list;
}

type symbol_role =
  | Unspecified_symbol_role 
  | Definition 
  | Import 
  | Write_access 
  | Read_access 
  | Generated 
  | Test 

type language =
  | Unspecified_language 
  | Abap 
  | Apl 
  | Ada 
  | Agda 
  | Ascii_doc 
  | Assembly 
  | Awk 
  | Bat 
  | Bib_te_x 
  | C 
  | Cobol 
  | Cpp 
  | Css 
  | Csharp 
  | Clojure 
  | Coffeescript 
  | Common_lisp 
  | Coq 
  | Dart 
  | Delphi 
  | Diff 
  | Dockerfile 
  | Dyalog 
  | Elixir 
  | Erlang 
  | Fsharp 
  | Fish 
  | Flow 
  | Fortran 
  | Git_commit 
  | Git_config 
  | Git_rebase 
  | Go 
  | Groovy 
  | Html 
  | Hack 
  | Handlebars 
  | Haskell 
  | Idris 
  | Ini 
  | J 
  | Json 
  | Java 
  | Java_script 
  | Java_script_react 
  | Jsonnet 
  | Julia 
  | Kotlin 
  | La_te_x 
  | Lean 
  | Less 
  | Lua 
  | Makefile 
  | Markdown 
  | Matlab 
  | Nix 
  | Ocaml 
  | Objective_c 
  | Objective_cpp 
  | Php 
  | Plsql 
  | Perl 
  | Power_shell 
  | Prolog 
  | Python 
  | R 
  | Racket 
  | Raku 
  | Razor 
  | Re_st 
  | Ruby 
  | Rust 
  | Sas 
  | Scss 
  | Sml 
  | Sql 
  | Sass 
  | Scala 
  | Scheme 
  | Shell_script 
  | Skylark 
  | Swift 
  | Toml 
  | Te_x 
  | Type_script 
  | Type_script_react 
  | Visual_basic 
  | Vue 
  | Wolfram 
  | Xml 
  | Xsl 
  | Yaml 
  | Zig 


(** {2 Default values} *)

val default_protocol_version : unit -> protocol_version
(** [default_protocol_version ()] is the default value for type [protocol_version] *)

val default_tool_info : 
  ?name:string ->
  ?version:string ->
  ?arguments:string list ->
  unit ->
  tool_info
(** [default_tool_info ()] is the default value for type [tool_info] *)

val default_text_encoding : unit -> text_encoding
(** [default_text_encoding ()] is the default value for type [text_encoding] *)

val default_metadata : 
  ?version:protocol_version ->
  ?tool_info:tool_info option ->
  ?project_root:string ->
  ?text_document_encoding:text_encoding ->
  unit ->
  metadata
(** [default_metadata ()] is the default value for type [metadata] *)

val default_syntax_kind : unit -> syntax_kind
(** [default_syntax_kind ()] is the default value for type [syntax_kind] *)

val default_severity : unit -> severity
(** [default_severity ()] is the default value for type [severity] *)

val default_diagnostic_tag : unit -> diagnostic_tag
(** [default_diagnostic_tag ()] is the default value for type [diagnostic_tag] *)

val default_diagnostic : 
  ?severity:severity ->
  ?code:string ->
  ?message:string ->
  ?source:string ->
  ?tags:diagnostic_tag list ->
  unit ->
  diagnostic
(** [default_diagnostic ()] is the default value for type [diagnostic] *)

val default_occurrence : 
  ?range:int32 list ->
  ?symbol:string ->
  ?symbol_roles:int32 ->
  ?override_documentation:string list ->
  ?syntax_kind:syntax_kind ->
  ?diagnostics:diagnostic list ->
  unit ->
  occurrence
(** [default_occurrence ()] is the default value for type [occurrence] *)

val default_relationship : 
  ?symbol:string ->
  ?is_reference:bool ->
  ?is_implementation:bool ->
  ?is_type_definition:bool ->
  ?is_definition:bool ->
  unit ->
  relationship
(** [default_relationship ()] is the default value for type [relationship] *)

val default_symbol_information : 
  ?symbol:string ->
  ?documentation:string list ->
  ?relationships:relationship list ->
  unit ->
  symbol_information
(** [default_symbol_information ()] is the default value for type [symbol_information] *)

val default_document : 
  ?language:string ->
  ?relative_path:string ->
  ?occurrences:occurrence list ->
  ?symbols:symbol_information list ->
  unit ->
  document
(** [default_document ()] is the default value for type [document] *)

val default_index : 
  ?metadata:metadata option ->
  ?documents:document list ->
  ?external_symbols:symbol_information list ->
  unit ->
  index
(** [default_index ()] is the default value for type [index] *)

val default_package : 
  ?manager:string ->
  ?name:string ->
  ?version:string ->
  unit ->
  package
(** [default_package ()] is the default value for type [package] *)

val default_descriptor_suffix : unit -> descriptor_suffix
(** [default_descriptor_suffix ()] is the default value for type [descriptor_suffix] *)

val default_descriptor : 
  ?name:string ->
  ?disambiguator:string ->
  ?suffix:descriptor_suffix ->
  unit ->
  descriptor
(** [default_descriptor ()] is the default value for type [descriptor] *)

val default_symbol : 
  ?scheme:string ->
  ?package:package option ->
  ?descriptors:descriptor list ->
  unit ->
  symbol
(** [default_symbol ()] is the default value for type [symbol] *)

val default_symbol_role : unit -> symbol_role
(** [default_symbol_role ()] is the default value for type [symbol_role] *)

val default_language : unit -> language
(** [default_language ()] is the default value for type [language] *)
