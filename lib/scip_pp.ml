[@@@ocaml.warning "-27-30-39"]

let rec pp_protocol_version fmt (v:Scip_types.protocol_version) =
  match v with
  | Scip_types.Unspecified_protocol_version -> Format.fprintf fmt "Unspecified_protocol_version"

let rec pp_tool_info fmt (v:Scip_types.tool_info) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "name" Pbrt.Pp.pp_string fmt v.Scip_types.name;
    Pbrt.Pp.pp_record_field ~first:false "version" Pbrt.Pp.pp_string fmt v.Scip_types.version;
    Pbrt.Pp.pp_record_field ~first:false "arguments" (Pbrt.Pp.pp_list Pbrt.Pp.pp_string) fmt v.Scip_types.arguments;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_text_encoding fmt (v:Scip_types.text_encoding) =
  match v with
  | Scip_types.Unspecified_text_encoding -> Format.fprintf fmt "Unspecified_text_encoding"
  | Scip_types.Utf8 -> Format.fprintf fmt "Utf8"
  | Scip_types.Utf16 -> Format.fprintf fmt "Utf16"

let rec pp_metadata fmt (v:Scip_types.metadata) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "version" pp_protocol_version fmt v.Scip_types.version;
    Pbrt.Pp.pp_record_field ~first:false "tool_info" (Pbrt.Pp.pp_option pp_tool_info) fmt v.Scip_types.tool_info;
    Pbrt.Pp.pp_record_field ~first:false "project_root" Pbrt.Pp.pp_string fmt v.Scip_types.project_root;
    Pbrt.Pp.pp_record_field ~first:false "text_document_encoding" pp_text_encoding fmt v.Scip_types.text_document_encoding;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_syntax_kind fmt (v:Scip_types.syntax_kind) =
  match v with
  | Scip_types.Unspecified_syntax_kind -> Format.fprintf fmt "Unspecified_syntax_kind"
  | Scip_types.Comment -> Format.fprintf fmt "Comment"
  | Scip_types.Punctuation_delimiter -> Format.fprintf fmt "Punctuation_delimiter"
  | Scip_types.Punctuation_bracket -> Format.fprintf fmt "Punctuation_bracket"
  | Scip_types.Keyword -> Format.fprintf fmt "Keyword"
  | Scip_types.Identifier_keyword -> Format.fprintf fmt "Identifier_keyword"
  | Scip_types.Identifier_operator -> Format.fprintf fmt "Identifier_operator"
  | Scip_types.Identifier -> Format.fprintf fmt "Identifier"
  | Scip_types.Identifier_builtin -> Format.fprintf fmt "Identifier_builtin"
  | Scip_types.Identifier_null -> Format.fprintf fmt "Identifier_null"
  | Scip_types.Identifier_constant -> Format.fprintf fmt "Identifier_constant"
  | Scip_types.Identifier_mutable_global -> Format.fprintf fmt "Identifier_mutable_global"
  | Scip_types.Identifier_parameter -> Format.fprintf fmt "Identifier_parameter"
  | Scip_types.Identifier_local -> Format.fprintf fmt "Identifier_local"
  | Scip_types.Identifier_shadowed -> Format.fprintf fmt "Identifier_shadowed"
  | Scip_types.Identifier_namespace -> Format.fprintf fmt "Identifier_namespace"
  | Scip_types.Identifier_module -> Format.fprintf fmt "Identifier_module"
  | Scip_types.Identifier_function -> Format.fprintf fmt "Identifier_function"
  | Scip_types.Identifier_function_definition -> Format.fprintf fmt "Identifier_function_definition"
  | Scip_types.Identifier_macro -> Format.fprintf fmt "Identifier_macro"
  | Scip_types.Identifier_macro_definition -> Format.fprintf fmt "Identifier_macro_definition"
  | Scip_types.Identifier_type -> Format.fprintf fmt "Identifier_type"
  | Scip_types.Identifier_builtin_type -> Format.fprintf fmt "Identifier_builtin_type"
  | Scip_types.Identifier_attribute -> Format.fprintf fmt "Identifier_attribute"
  | Scip_types.Regex_escape -> Format.fprintf fmt "Regex_escape"
  | Scip_types.Regex_repeated -> Format.fprintf fmt "Regex_repeated"
  | Scip_types.Regex_wildcard -> Format.fprintf fmt "Regex_wildcard"
  | Scip_types.Regex_delimiter -> Format.fprintf fmt "Regex_delimiter"
  | Scip_types.Regex_join -> Format.fprintf fmt "Regex_join"
  | Scip_types.String_literal -> Format.fprintf fmt "String_literal"
  | Scip_types.String_literal_escape -> Format.fprintf fmt "String_literal_escape"
  | Scip_types.String_literal_special -> Format.fprintf fmt "String_literal_special"
  | Scip_types.String_literal_key -> Format.fprintf fmt "String_literal_key"
  | Scip_types.Character_literal -> Format.fprintf fmt "Character_literal"
  | Scip_types.Numeric_literal -> Format.fprintf fmt "Numeric_literal"
  | Scip_types.Boolean_literal -> Format.fprintf fmt "Boolean_literal"
  | Scip_types.Tag -> Format.fprintf fmt "Tag"
  | Scip_types.Tag_attribute -> Format.fprintf fmt "Tag_attribute"
  | Scip_types.Tag_delimiter -> Format.fprintf fmt "Tag_delimiter"

let rec pp_severity fmt (v:Scip_types.severity) =
  match v with
  | Scip_types.Unspecified_severity -> Format.fprintf fmt "Unspecified_severity"
  | Scip_types.Error -> Format.fprintf fmt "Error"
  | Scip_types.Warning -> Format.fprintf fmt "Warning"
  | Scip_types.Information -> Format.fprintf fmt "Information"
  | Scip_types.Hint -> Format.fprintf fmt "Hint"

let rec pp_diagnostic_tag fmt (v:Scip_types.diagnostic_tag) =
  match v with
  | Scip_types.Unspecified_diagnostic_tag -> Format.fprintf fmt "Unspecified_diagnostic_tag"
  | Scip_types.Unnecessary -> Format.fprintf fmt "Unnecessary"
  | Scip_types.Deprecated -> Format.fprintf fmt "Deprecated"

let rec pp_diagnostic fmt (v:Scip_types.diagnostic) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "severity" pp_severity fmt v.Scip_types.severity;
    Pbrt.Pp.pp_record_field ~first:false "code" Pbrt.Pp.pp_string fmt v.Scip_types.code;
    Pbrt.Pp.pp_record_field ~first:false "message" Pbrt.Pp.pp_string fmt v.Scip_types.message;
    Pbrt.Pp.pp_record_field ~first:false "source" Pbrt.Pp.pp_string fmt v.Scip_types.source;
    Pbrt.Pp.pp_record_field ~first:false "tags" (Pbrt.Pp.pp_list pp_diagnostic_tag) fmt v.Scip_types.tags;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_occurrence fmt (v:Scip_types.occurrence) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "range" (Pbrt.Pp.pp_list Pbrt.Pp.pp_int32) fmt v.Scip_types.range;
    Pbrt.Pp.pp_record_field ~first:false "symbol" Pbrt.Pp.pp_string fmt v.Scip_types.symbol;
    Pbrt.Pp.pp_record_field ~first:false "symbol_roles" Pbrt.Pp.pp_int32 fmt v.Scip_types.symbol_roles;
    Pbrt.Pp.pp_record_field ~first:false "override_documentation" (Pbrt.Pp.pp_list Pbrt.Pp.pp_string) fmt v.Scip_types.override_documentation;
    Pbrt.Pp.pp_record_field ~first:false "syntax_kind" pp_syntax_kind fmt v.Scip_types.syntax_kind;
    Pbrt.Pp.pp_record_field ~first:false "diagnostics" (Pbrt.Pp.pp_list pp_diagnostic) fmt v.Scip_types.diagnostics;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_relationship fmt (v:Scip_types.relationship) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "symbol" Pbrt.Pp.pp_string fmt v.Scip_types.symbol;
    Pbrt.Pp.pp_record_field ~first:false "is_reference" Pbrt.Pp.pp_bool fmt v.Scip_types.is_reference;
    Pbrt.Pp.pp_record_field ~first:false "is_implementation" Pbrt.Pp.pp_bool fmt v.Scip_types.is_implementation;
    Pbrt.Pp.pp_record_field ~first:false "is_type_definition" Pbrt.Pp.pp_bool fmt v.Scip_types.is_type_definition;
    Pbrt.Pp.pp_record_field ~first:false "is_definition" Pbrt.Pp.pp_bool fmt v.Scip_types.is_definition;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_symbol_information fmt (v:Scip_types.symbol_information) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "symbol" Pbrt.Pp.pp_string fmt v.Scip_types.symbol;
    Pbrt.Pp.pp_record_field ~first:false "documentation" (Pbrt.Pp.pp_list Pbrt.Pp.pp_string) fmt v.Scip_types.documentation;
    Pbrt.Pp.pp_record_field ~first:false "relationships" (Pbrt.Pp.pp_list pp_relationship) fmt v.Scip_types.relationships;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_document fmt (v:Scip_types.document) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "language" Pbrt.Pp.pp_string fmt v.Scip_types.language;
    Pbrt.Pp.pp_record_field ~first:false "relative_path" Pbrt.Pp.pp_string fmt v.Scip_types.relative_path;
    Pbrt.Pp.pp_record_field ~first:false "occurrences" (Pbrt.Pp.pp_list pp_occurrence) fmt v.Scip_types.occurrences;
    Pbrt.Pp.pp_record_field ~first:false "symbols" (Pbrt.Pp.pp_list pp_symbol_information) fmt v.Scip_types.symbols;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_index fmt (v:Scip_types.index) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "metadata" (Pbrt.Pp.pp_option pp_metadata) fmt v.Scip_types.metadata;
    Pbrt.Pp.pp_record_field ~first:false "documents" (Pbrt.Pp.pp_list pp_document) fmt v.Scip_types.documents;
    Pbrt.Pp.pp_record_field ~first:false "external_symbols" (Pbrt.Pp.pp_list pp_symbol_information) fmt v.Scip_types.external_symbols;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_package fmt (v:Scip_types.package) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "manager" Pbrt.Pp.pp_string fmt v.Scip_types.manager;
    Pbrt.Pp.pp_record_field ~first:false "name" Pbrt.Pp.pp_string fmt v.Scip_types.name;
    Pbrt.Pp.pp_record_field ~first:false "version" Pbrt.Pp.pp_string fmt v.Scip_types.version;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_descriptor_suffix fmt (v:Scip_types.descriptor_suffix) =
  match v with
  | Scip_types.Unspecified_suffix -> Format.fprintf fmt "Unspecified_suffix"
  | Scip_types.Namespace -> Format.fprintf fmt "Namespace"
  | Scip_types.Package -> Format.fprintf fmt "Package"
  | Scip_types.Type -> Format.fprintf fmt "Type"
  | Scip_types.Term -> Format.fprintf fmt "Term"
  | Scip_types.Method -> Format.fprintf fmt "Method"
  | Scip_types.Type_parameter -> Format.fprintf fmt "Type_parameter"
  | Scip_types.Parameter -> Format.fprintf fmt "Parameter"
  | Scip_types.Macro -> Format.fprintf fmt "Macro"
  | Scip_types.Meta -> Format.fprintf fmt "Meta"
  | Scip_types.Local -> Format.fprintf fmt "Local"

let rec pp_descriptor fmt (v:Scip_types.descriptor) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "name" Pbrt.Pp.pp_string fmt v.Scip_types.name;
    Pbrt.Pp.pp_record_field ~first:false "disambiguator" Pbrt.Pp.pp_string fmt v.Scip_types.disambiguator;
    Pbrt.Pp.pp_record_field ~first:false "suffix" pp_descriptor_suffix fmt v.Scip_types.suffix;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_symbol fmt (v:Scip_types.symbol) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "scheme" Pbrt.Pp.pp_string fmt v.Scip_types.scheme;
    Pbrt.Pp.pp_record_field ~first:false "package" (Pbrt.Pp.pp_option pp_package) fmt v.Scip_types.package;
    Pbrt.Pp.pp_record_field ~first:false "descriptors" (Pbrt.Pp.pp_list pp_descriptor) fmt v.Scip_types.descriptors;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_symbol_role fmt (v:Scip_types.symbol_role) =
  match v with
  | Scip_types.Unspecified_symbol_role -> Format.fprintf fmt "Unspecified_symbol_role"
  | Scip_types.Definition -> Format.fprintf fmt "Definition"
  | Scip_types.Import -> Format.fprintf fmt "Import"
  | Scip_types.Write_access -> Format.fprintf fmt "Write_access"
  | Scip_types.Read_access -> Format.fprintf fmt "Read_access"
  | Scip_types.Generated -> Format.fprintf fmt "Generated"
  | Scip_types.Test -> Format.fprintf fmt "Test"

let rec pp_language fmt (v:Scip_types.language) =
  match v with
  | Scip_types.Unspecified_language -> Format.fprintf fmt "Unspecified_language"
  | Scip_types.Abap -> Format.fprintf fmt "Abap"
  | Scip_types.Apl -> Format.fprintf fmt "Apl"
  | Scip_types.Ada -> Format.fprintf fmt "Ada"
  | Scip_types.Agda -> Format.fprintf fmt "Agda"
  | Scip_types.Ascii_doc -> Format.fprintf fmt "Ascii_doc"
  | Scip_types.Assembly -> Format.fprintf fmt "Assembly"
  | Scip_types.Awk -> Format.fprintf fmt "Awk"
  | Scip_types.Bat -> Format.fprintf fmt "Bat"
  | Scip_types.Bib_te_x -> Format.fprintf fmt "Bib_te_x"
  | Scip_types.C -> Format.fprintf fmt "C"
  | Scip_types.Cobol -> Format.fprintf fmt "Cobol"
  | Scip_types.Cpp -> Format.fprintf fmt "Cpp"
  | Scip_types.Css -> Format.fprintf fmt "Css"
  | Scip_types.Csharp -> Format.fprintf fmt "Csharp"
  | Scip_types.Clojure -> Format.fprintf fmt "Clojure"
  | Scip_types.Coffeescript -> Format.fprintf fmt "Coffeescript"
  | Scip_types.Common_lisp -> Format.fprintf fmt "Common_lisp"
  | Scip_types.Coq -> Format.fprintf fmt "Coq"
  | Scip_types.Dart -> Format.fprintf fmt "Dart"
  | Scip_types.Delphi -> Format.fprintf fmt "Delphi"
  | Scip_types.Diff -> Format.fprintf fmt "Diff"
  | Scip_types.Dockerfile -> Format.fprintf fmt "Dockerfile"
  | Scip_types.Dyalog -> Format.fprintf fmt "Dyalog"
  | Scip_types.Elixir -> Format.fprintf fmt "Elixir"
  | Scip_types.Erlang -> Format.fprintf fmt "Erlang"
  | Scip_types.Fsharp -> Format.fprintf fmt "Fsharp"
  | Scip_types.Fish -> Format.fprintf fmt "Fish"
  | Scip_types.Flow -> Format.fprintf fmt "Flow"
  | Scip_types.Fortran -> Format.fprintf fmt "Fortran"
  | Scip_types.Git_commit -> Format.fprintf fmt "Git_commit"
  | Scip_types.Git_config -> Format.fprintf fmt "Git_config"
  | Scip_types.Git_rebase -> Format.fprintf fmt "Git_rebase"
  | Scip_types.Go -> Format.fprintf fmt "Go"
  | Scip_types.Groovy -> Format.fprintf fmt "Groovy"
  | Scip_types.Html -> Format.fprintf fmt "Html"
  | Scip_types.Hack -> Format.fprintf fmt "Hack"
  | Scip_types.Handlebars -> Format.fprintf fmt "Handlebars"
  | Scip_types.Haskell -> Format.fprintf fmt "Haskell"
  | Scip_types.Idris -> Format.fprintf fmt "Idris"
  | Scip_types.Ini -> Format.fprintf fmt "Ini"
  | Scip_types.J -> Format.fprintf fmt "J"
  | Scip_types.Json -> Format.fprintf fmt "Json"
  | Scip_types.Java -> Format.fprintf fmt "Java"
  | Scip_types.Java_script -> Format.fprintf fmt "Java_script"
  | Scip_types.Java_script_react -> Format.fprintf fmt "Java_script_react"
  | Scip_types.Jsonnet -> Format.fprintf fmt "Jsonnet"
  | Scip_types.Julia -> Format.fprintf fmt "Julia"
  | Scip_types.Kotlin -> Format.fprintf fmt "Kotlin"
  | Scip_types.La_te_x -> Format.fprintf fmt "La_te_x"
  | Scip_types.Lean -> Format.fprintf fmt "Lean"
  | Scip_types.Less -> Format.fprintf fmt "Less"
  | Scip_types.Lua -> Format.fprintf fmt "Lua"
  | Scip_types.Makefile -> Format.fprintf fmt "Makefile"
  | Scip_types.Markdown -> Format.fprintf fmt "Markdown"
  | Scip_types.Matlab -> Format.fprintf fmt "Matlab"
  | Scip_types.Nix -> Format.fprintf fmt "Nix"
  | Scip_types.Ocaml -> Format.fprintf fmt "Ocaml"
  | Scip_types.Objective_c -> Format.fprintf fmt "Objective_c"
  | Scip_types.Objective_cpp -> Format.fprintf fmt "Objective_cpp"
  | Scip_types.Php -> Format.fprintf fmt "Php"
  | Scip_types.Plsql -> Format.fprintf fmt "Plsql"
  | Scip_types.Perl -> Format.fprintf fmt "Perl"
  | Scip_types.Power_shell -> Format.fprintf fmt "Power_shell"
  | Scip_types.Prolog -> Format.fprintf fmt "Prolog"
  | Scip_types.Python -> Format.fprintf fmt "Python"
  | Scip_types.R -> Format.fprintf fmt "R"
  | Scip_types.Racket -> Format.fprintf fmt "Racket"
  | Scip_types.Raku -> Format.fprintf fmt "Raku"
  | Scip_types.Razor -> Format.fprintf fmt "Razor"
  | Scip_types.Re_st -> Format.fprintf fmt "Re_st"
  | Scip_types.Ruby -> Format.fprintf fmt "Ruby"
  | Scip_types.Rust -> Format.fprintf fmt "Rust"
  | Scip_types.Sas -> Format.fprintf fmt "Sas"
  | Scip_types.Scss -> Format.fprintf fmt "Scss"
  | Scip_types.Sml -> Format.fprintf fmt "Sml"
  | Scip_types.Sql -> Format.fprintf fmt "Sql"
  | Scip_types.Sass -> Format.fprintf fmt "Sass"
  | Scip_types.Scala -> Format.fprintf fmt "Scala"
  | Scip_types.Scheme -> Format.fprintf fmt "Scheme"
  | Scip_types.Shell_script -> Format.fprintf fmt "Shell_script"
  | Scip_types.Skylark -> Format.fprintf fmt "Skylark"
  | Scip_types.Swift -> Format.fprintf fmt "Swift"
  | Scip_types.Toml -> Format.fprintf fmt "Toml"
  | Scip_types.Te_x -> Format.fprintf fmt "Te_x"
  | Scip_types.Type_script -> Format.fprintf fmt "Type_script"
  | Scip_types.Type_script_react -> Format.fprintf fmt "Type_script_react"
  | Scip_types.Visual_basic -> Format.fprintf fmt "Visual_basic"
  | Scip_types.Vue -> Format.fprintf fmt "Vue"
  | Scip_types.Wolfram -> Format.fprintf fmt "Wolfram"
  | Scip_types.Xml -> Format.fprintf fmt "Xml"
  | Scip_types.Xsl -> Format.fprintf fmt "Xsl"
  | Scip_types.Yaml -> Format.fprintf fmt "Yaml"
  | Scip_types.Zig -> Format.fprintf fmt "Zig"
