[@@@ocaml.warning "-27-30-39"]


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

let rec default_protocol_version () = (Unspecified_protocol_version:protocol_version)

let rec default_tool_info 
  ?name:((name:string) = "")
  ?version:((version:string) = "")
  ?arguments:((arguments:string list) = [])
  () : tool_info  = {
  name;
  version;
  arguments;
}

let rec default_text_encoding () = (Unspecified_text_encoding:text_encoding)

let rec default_metadata 
  ?version:((version:protocol_version) = default_protocol_version ())
  ?tool_info:((tool_info:tool_info option) = None)
  ?project_root:((project_root:string) = "")
  ?text_document_encoding:((text_document_encoding:text_encoding) = default_text_encoding ())
  () : metadata  = {
  version;
  tool_info;
  project_root;
  text_document_encoding;
}

let rec default_syntax_kind () = (Unspecified_syntax_kind:syntax_kind)

let rec default_severity () = (Unspecified_severity:severity)

let rec default_diagnostic_tag () = (Unspecified_diagnostic_tag:diagnostic_tag)

let rec default_diagnostic 
  ?severity:((severity:severity) = default_severity ())
  ?code:((code:string) = "")
  ?message:((message:string) = "")
  ?source:((source:string) = "")
  ?tags:((tags:diagnostic_tag list) = [])
  () : diagnostic  = {
  severity;
  code;
  message;
  source;
  tags;
}

let rec default_occurrence 
  ?range:((range:int32 list) = [])
  ?symbol:((symbol:string) = "")
  ?symbol_roles:((symbol_roles:int32) = 0l)
  ?override_documentation:((override_documentation:string list) = [])
  ?syntax_kind:((syntax_kind:syntax_kind) = default_syntax_kind ())
  ?diagnostics:((diagnostics:diagnostic list) = [])
  () : occurrence  = {
  range;
  symbol;
  symbol_roles;
  override_documentation;
  syntax_kind;
  diagnostics;
}

let rec default_relationship 
  ?symbol:((symbol:string) = "")
  ?is_reference:((is_reference:bool) = false)
  ?is_implementation:((is_implementation:bool) = false)
  ?is_type_definition:((is_type_definition:bool) = false)
  ?is_definition:((is_definition:bool) = false)
  () : relationship  = {
  symbol;
  is_reference;
  is_implementation;
  is_type_definition;
  is_definition;
}

let rec default_symbol_information 
  ?symbol:((symbol:string) = "")
  ?documentation:((documentation:string list) = [])
  ?relationships:((relationships:relationship list) = [])
  () : symbol_information  = {
  symbol;
  documentation;
  relationships;
}

let rec default_document 
  ?language:((language:string) = "")
  ?relative_path:((relative_path:string) = "")
  ?occurrences:((occurrences:occurrence list) = [])
  ?symbols:((symbols:symbol_information list) = [])
  () : document  = {
  language;
  relative_path;
  occurrences;
  symbols;
}

let rec default_index 
  ?metadata:((metadata:metadata option) = None)
  ?documents:((documents:document list) = [])
  ?external_symbols:((external_symbols:symbol_information list) = [])
  () : index  = {
  metadata;
  documents;
  external_symbols;
}

let rec default_package 
  ?manager:((manager:string) = "")
  ?name:((name:string) = "")
  ?version:((version:string) = "")
  () : package  = {
  manager;
  name;
  version;
}

let rec default_descriptor_suffix () = (Unspecified_suffix:descriptor_suffix)

let rec default_descriptor 
  ?name:((name:string) = "")
  ?disambiguator:((disambiguator:string) = "")
  ?suffix:((suffix:descriptor_suffix) = default_descriptor_suffix ())
  () : descriptor  = {
  name;
  disambiguator;
  suffix;
}

let rec default_symbol 
  ?scheme:((scheme:string) = "")
  ?package:((package:package option) = None)
  ?descriptors:((descriptors:descriptor list) = [])
  () : symbol  = {
  scheme;
  package;
  descriptors;
}

let rec default_symbol_role () = (Unspecified_symbol_role:symbol_role)

let rec default_language () = (Unspecified_language:language)
