[@@@ocaml.warning "-11-27-30-39"]

type tool_info_mutable = {
  mutable name : string;
  mutable version : string;
  mutable arguments : string list;
}

let default_tool_info_mutable () : tool_info_mutable =
  { name = ""; version = ""; arguments = [] }

type metadata_mutable = {
  mutable version : Scip_types.protocol_version;
  mutable tool_info : Scip_types.tool_info option;
  mutable project_root : string;
  mutable text_document_encoding : Scip_types.text_encoding;
}

let default_metadata_mutable () : metadata_mutable =
  {
    version = Scip_types.default_protocol_version ();
    tool_info = None;
    project_root = "";
    text_document_encoding = Scip_types.default_text_encoding ();
  }

type diagnostic_mutable = {
  mutable severity : Scip_types.severity;
  mutable code : string;
  mutable message : string;
  mutable source : string;
  mutable tags : Scip_types.diagnostic_tag list;
}

let default_diagnostic_mutable () : diagnostic_mutable =
  {
    severity = Scip_types.default_severity ();
    code = "";
    message = "";
    source = "";
    tags = [];
  }

type occurrence_mutable = {
  mutable range : int32 list;
  mutable symbol : string;
  mutable symbol_roles : int32;
  mutable override_documentation : string list;
  mutable syntax_kind : Scip_types.syntax_kind;
  mutable diagnostics : Scip_types.diagnostic list;
}

let default_occurrence_mutable () : occurrence_mutable =
  {
    range = [];
    symbol = "";
    symbol_roles = 0l;
    override_documentation = [];
    syntax_kind = Scip_types.default_syntax_kind ();
    diagnostics = [];
  }

type relationship_mutable = {
  mutable symbol : string;
  mutable is_reference : bool;
  mutable is_implementation : bool;
  mutable is_type_definition : bool;
  mutable is_definition : bool;
}

let default_relationship_mutable () : relationship_mutable =
  {
    symbol = "";
    is_reference = false;
    is_implementation = false;
    is_type_definition = false;
    is_definition = false;
  }

type symbol_information_mutable = {
  mutable symbol : string;
  mutable documentation : string list;
  mutable relationships : Scip_types.relationship list;
}

let default_symbol_information_mutable () : symbol_information_mutable =
  { symbol = ""; documentation = []; relationships = [] }

type document_mutable = {
  mutable language : string;
  mutable relative_path : string;
  mutable occurrences : Scip_types.occurrence list;
  mutable symbols : Scip_types.symbol_information list;
}

let default_document_mutable () : document_mutable =
  { language = ""; relative_path = ""; occurrences = []; symbols = [] }

type index_mutable = {
  mutable metadata : Scip_types.metadata option;
  mutable documents : Scip_types.document list;
  mutable external_symbols : Scip_types.symbol_information list;
}

let default_index_mutable () : index_mutable =
  { metadata = None; documents = []; external_symbols = [] }

type package_mutable = {
  mutable manager : string;
  mutable name : string;
  mutable version : string;
}

let default_package_mutable () : package_mutable =
  { manager = ""; name = ""; version = "" }

type descriptor_mutable = {
  mutable name : string;
  mutable disambiguator : string;
  mutable suffix : Scip_types.descriptor_suffix;
}

let default_descriptor_mutable () : descriptor_mutable =
  {
    name = "";
    disambiguator = "";
    suffix = Scip_types.default_descriptor_suffix ();
  }

type symbol_mutable = {
  mutable scheme : string;
  mutable package : Scip_types.package option;
  mutable descriptors : Scip_types.descriptor list;
}

let default_symbol_mutable () : symbol_mutable =
  { scheme = ""; package = None; descriptors = [] }

let rec decode_protocol_version d =
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Scip_types.Unspecified_protocol_version : Scip_types.protocol_version)
  | _ -> Pbrt.Decoder.malformed_variant "protocol_version"

let rec decode_tool_info d =
  let v = default_tool_info_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.arguments <- List.rev v.arguments;
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.name <- Pbrt.Decoder.string d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(tool_info), field(1)" pk
    | Some (2, Pbrt.Bytes) -> v.version <- Pbrt.Decoder.string d
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(tool_info), field(2)" pk
    | Some (3, Pbrt.Bytes) ->
        v.arguments <- Pbrt.Decoder.string d :: v.arguments
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(tool_info), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Scip_types.name = v.name;
     Scip_types.version = v.version;
     Scip_types.arguments = v.arguments;
   }
    : Scip_types.tool_info)

let rec decode_text_encoding d =
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Scip_types.Unspecified_text_encoding : Scip_types.text_encoding)
  | 1 -> (Scip_types.Utf8 : Scip_types.text_encoding)
  | 2 -> (Scip_types.Utf16 : Scip_types.text_encoding)
  | _ -> Pbrt.Decoder.malformed_variant "text_encoding"

let rec decode_metadata d =
  let v = default_metadata_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Varint) -> v.version <- decode_protocol_version d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(metadata), field(1)" pk
    | Some (2, Pbrt.Bytes) ->
        v.tool_info <- Some (decode_tool_info (Pbrt.Decoder.nested d))
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(metadata), field(2)" pk
    | Some (3, Pbrt.Bytes) -> v.project_root <- Pbrt.Decoder.string d
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(metadata), field(3)" pk
    | Some (4, Pbrt.Varint) ->
        v.text_document_encoding <- decode_text_encoding d
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(metadata), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Scip_types.version = v.version;
     Scip_types.tool_info = v.tool_info;
     Scip_types.project_root = v.project_root;
     Scip_types.text_document_encoding = v.text_document_encoding;
   }
    : Scip_types.metadata)

let rec decode_syntax_kind d =
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Scip_types.Unspecified_syntax_kind : Scip_types.syntax_kind)
  | 1 -> (Scip_types.Comment : Scip_types.syntax_kind)
  | 2 -> (Scip_types.Punctuation_delimiter : Scip_types.syntax_kind)
  | 3 -> (Scip_types.Punctuation_bracket : Scip_types.syntax_kind)
  | 4 -> (Scip_types.Keyword : Scip_types.syntax_kind)
  | 4 -> (Scip_types.Identifier_keyword : Scip_types.syntax_kind)
  | 5 -> (Scip_types.Identifier_operator : Scip_types.syntax_kind)
  | 6 -> (Scip_types.Identifier : Scip_types.syntax_kind)
  | 7 -> (Scip_types.Identifier_builtin : Scip_types.syntax_kind)
  | 8 -> (Scip_types.Identifier_null : Scip_types.syntax_kind)
  | 9 -> (Scip_types.Identifier_constant : Scip_types.syntax_kind)
  | 10 -> (Scip_types.Identifier_mutable_global : Scip_types.syntax_kind)
  | 11 -> (Scip_types.Identifier_parameter : Scip_types.syntax_kind)
  | 12 -> (Scip_types.Identifier_local : Scip_types.syntax_kind)
  | 13 -> (Scip_types.Identifier_shadowed : Scip_types.syntax_kind)
  | 14 -> (Scip_types.Identifier_namespace : Scip_types.syntax_kind)
  | 14 -> (Scip_types.Identifier_module : Scip_types.syntax_kind)
  | 15 -> (Scip_types.Identifier_function : Scip_types.syntax_kind)
  | 16 -> (Scip_types.Identifier_function_definition : Scip_types.syntax_kind)
  | 17 -> (Scip_types.Identifier_macro : Scip_types.syntax_kind)
  | 18 -> (Scip_types.Identifier_macro_definition : Scip_types.syntax_kind)
  | 19 -> (Scip_types.Identifier_type : Scip_types.syntax_kind)
  | 20 -> (Scip_types.Identifier_builtin_type : Scip_types.syntax_kind)
  | 21 -> (Scip_types.Identifier_attribute : Scip_types.syntax_kind)
  | 22 -> (Scip_types.Regex_escape : Scip_types.syntax_kind)
  | 23 -> (Scip_types.Regex_repeated : Scip_types.syntax_kind)
  | 24 -> (Scip_types.Regex_wildcard : Scip_types.syntax_kind)
  | 25 -> (Scip_types.Regex_delimiter : Scip_types.syntax_kind)
  | 26 -> (Scip_types.Regex_join : Scip_types.syntax_kind)
  | 27 -> (Scip_types.String_literal : Scip_types.syntax_kind)
  | 28 -> (Scip_types.String_literal_escape : Scip_types.syntax_kind)
  | 29 -> (Scip_types.String_literal_special : Scip_types.syntax_kind)
  | 30 -> (Scip_types.String_literal_key : Scip_types.syntax_kind)
  | 31 -> (Scip_types.Character_literal : Scip_types.syntax_kind)
  | 32 -> (Scip_types.Numeric_literal : Scip_types.syntax_kind)
  | 33 -> (Scip_types.Boolean_literal : Scip_types.syntax_kind)
  | 34 -> (Scip_types.Tag : Scip_types.syntax_kind)
  | 35 -> (Scip_types.Tag_attribute : Scip_types.syntax_kind)
  | 36 -> (Scip_types.Tag_delimiter : Scip_types.syntax_kind)
  | _ -> Pbrt.Decoder.malformed_variant "syntax_kind"

let rec decode_severity d =
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Scip_types.Unspecified_severity : Scip_types.severity)
  | 1 -> (Scip_types.Error : Scip_types.severity)
  | 2 -> (Scip_types.Warning : Scip_types.severity)
  | 3 -> (Scip_types.Information : Scip_types.severity)
  | 4 -> (Scip_types.Hint : Scip_types.severity)
  | _ -> Pbrt.Decoder.malformed_variant "severity"

let rec decode_diagnostic_tag d =
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Scip_types.Unspecified_diagnostic_tag : Scip_types.diagnostic_tag)
  | 1 -> (Scip_types.Unnecessary : Scip_types.diagnostic_tag)
  | 2 -> (Scip_types.Deprecated : Scip_types.diagnostic_tag)
  | _ -> Pbrt.Decoder.malformed_variant "diagnostic_tag"

let rec decode_diagnostic d =
  let v = default_diagnostic_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.tags <- List.rev v.tags;
        continue__ := false
    | Some (1, Pbrt.Varint) -> v.severity <- decode_severity d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(diagnostic), field(1)" pk
    | Some (2, Pbrt.Bytes) -> v.code <- Pbrt.Decoder.string d
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(diagnostic), field(2)" pk
    | Some (3, Pbrt.Bytes) -> v.message <- Pbrt.Decoder.string d
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(diagnostic), field(3)" pk
    | Some (4, Pbrt.Bytes) -> v.source <- Pbrt.Decoder.string d
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(diagnostic), field(4)" pk
    | Some (5, Pbrt.Varint) -> v.tags <- decode_diagnostic_tag d :: v.tags
    | Some (5, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(diagnostic), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Scip_types.severity = v.severity;
     Scip_types.code = v.code;
     Scip_types.message = v.message;
     Scip_types.source = v.source;
     Scip_types.tags = v.tags;
   }
    : Scip_types.diagnostic)

let rec decode_occurrence d =
  let v = default_occurrence_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.diagnostics <- List.rev v.diagnostics;
        v.override_documentation <- List.rev v.override_documentation;
        v.range <- List.rev v.range;
        continue__ := false
    | Some (1, Pbrt.Bytes) ->
        v.range <-
          Pbrt.Decoder.packed_fold
            (fun l d -> Pbrt.Decoder.int32_as_varint d :: l)
            [] d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(occurrence), field(1)" pk
    | Some (2, Pbrt.Bytes) -> v.symbol <- Pbrt.Decoder.string d
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(occurrence), field(2)" pk
    | Some (3, Pbrt.Varint) -> v.symbol_roles <- Pbrt.Decoder.int32_as_varint d
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(occurrence), field(3)" pk
    | Some (4, Pbrt.Bytes) ->
        v.override_documentation <-
          Pbrt.Decoder.string d :: v.override_documentation
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(occurrence), field(4)" pk
    | Some (5, Pbrt.Varint) -> v.syntax_kind <- decode_syntax_kind d
    | Some (5, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(occurrence), field(5)" pk
    | Some (6, Pbrt.Bytes) ->
        v.diagnostics <-
          decode_diagnostic (Pbrt.Decoder.nested d) :: v.diagnostics
    | Some (6, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(occurrence), field(6)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Scip_types.range = v.range;
     Scip_types.symbol = v.symbol;
     Scip_types.symbol_roles = v.symbol_roles;
     Scip_types.override_documentation = v.override_documentation;
     Scip_types.syntax_kind = v.syntax_kind;
     Scip_types.diagnostics = v.diagnostics;
   }
    : Scip_types.occurrence)

let rec decode_relationship d =
  let v = default_relationship_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.symbol <- Pbrt.Decoder.string d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(relationship), field(1)" pk
    | Some (2, Pbrt.Varint) -> v.is_reference <- Pbrt.Decoder.bool d
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(relationship), field(2)" pk
    | Some (3, Pbrt.Varint) -> v.is_implementation <- Pbrt.Decoder.bool d
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(relationship), field(3)" pk
    | Some (4, Pbrt.Varint) -> v.is_type_definition <- Pbrt.Decoder.bool d
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(relationship), field(4)" pk
    | Some (5, Pbrt.Varint) -> v.is_definition <- Pbrt.Decoder.bool d
    | Some (5, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(relationship), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Scip_types.symbol = v.symbol;
     Scip_types.is_reference = v.is_reference;
     Scip_types.is_implementation = v.is_implementation;
     Scip_types.is_type_definition = v.is_type_definition;
     Scip_types.is_definition = v.is_definition;
   }
    : Scip_types.relationship)

let rec decode_symbol_information d =
  let v = default_symbol_information_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.relationships <- List.rev v.relationships;
        v.documentation <- List.rev v.documentation;
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.symbol <- Pbrt.Decoder.string d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(symbol_information), field(1)"
          pk
    | Some (3, Pbrt.Bytes) ->
        v.documentation <- Pbrt.Decoder.string d :: v.documentation
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(symbol_information), field(3)"
          pk
    | Some (4, Pbrt.Bytes) ->
        v.relationships <-
          decode_relationship (Pbrt.Decoder.nested d) :: v.relationships
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(symbol_information), field(4)"
          pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Scip_types.symbol = v.symbol;
     Scip_types.documentation = v.documentation;
     Scip_types.relationships = v.relationships;
   }
    : Scip_types.symbol_information)

let rec decode_document d =
  let v = default_document_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.symbols <- List.rev v.symbols;
        v.occurrences <- List.rev v.occurrences;
        continue__ := false
    | Some (4, Pbrt.Bytes) -> v.language <- Pbrt.Decoder.string d
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(document), field(4)" pk
    | Some (1, Pbrt.Bytes) -> v.relative_path <- Pbrt.Decoder.string d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(document), field(1)" pk
    | Some (2, Pbrt.Bytes) ->
        v.occurrences <-
          decode_occurrence (Pbrt.Decoder.nested d) :: v.occurrences
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(document), field(2)" pk
    | Some (3, Pbrt.Bytes) ->
        v.symbols <-
          decode_symbol_information (Pbrt.Decoder.nested d) :: v.symbols
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(document), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Scip_types.language = v.language;
     Scip_types.relative_path = v.relative_path;
     Scip_types.occurrences = v.occurrences;
     Scip_types.symbols = v.symbols;
   }
    : Scip_types.document)

let rec decode_index d =
  let v = default_index_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.external_symbols <- List.rev v.external_symbols;
        v.documents <- List.rev v.documents;
        continue__ := false
    | Some (1, Pbrt.Bytes) ->
        v.metadata <- Some (decode_metadata (Pbrt.Decoder.nested d))
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(index), field(1)" pk
    | Some (2, Pbrt.Bytes) ->
        v.documents <- decode_document (Pbrt.Decoder.nested d) :: v.documents
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(index), field(2)" pk
    | Some (3, Pbrt.Bytes) ->
        v.external_symbols <-
          decode_symbol_information (Pbrt.Decoder.nested d)
          :: v.external_symbols
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(index), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Scip_types.metadata = v.metadata;
     Scip_types.documents = v.documents;
     Scip_types.external_symbols = v.external_symbols;
   }
    : Scip_types.index)

let rec decode_package d =
  let v = default_package_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.manager <- Pbrt.Decoder.string d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(package), field(1)" pk
    | Some (2, Pbrt.Bytes) -> v.name <- Pbrt.Decoder.string d
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(package), field(2)" pk
    | Some (3, Pbrt.Bytes) -> v.version <- Pbrt.Decoder.string d
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(package), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Scip_types.manager = v.manager;
     Scip_types.name = v.name;
     Scip_types.version = v.version;
   }
    : Scip_types.package)

let rec decode_descriptor_suffix d =
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Scip_types.Unspecified_suffix : Scip_types.descriptor_suffix)
  | 1 -> (Scip_types.Namespace : Scip_types.descriptor_suffix)
  | 1 -> (Scip_types.Package : Scip_types.descriptor_suffix)
  | 2 -> (Scip_types.Type : Scip_types.descriptor_suffix)
  | 3 -> (Scip_types.Term : Scip_types.descriptor_suffix)
  | 4 -> (Scip_types.Method : Scip_types.descriptor_suffix)
  | 5 -> (Scip_types.Type_parameter : Scip_types.descriptor_suffix)
  | 6 -> (Scip_types.Parameter : Scip_types.descriptor_suffix)
  | 9 -> (Scip_types.Macro : Scip_types.descriptor_suffix)
  | 7 -> (Scip_types.Meta : Scip_types.descriptor_suffix)
  | 8 -> (Scip_types.Local : Scip_types.descriptor_suffix)
  | _ -> Pbrt.Decoder.malformed_variant "descriptor_suffix"

let rec decode_descriptor d =
  let v = default_descriptor_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.name <- Pbrt.Decoder.string d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor), field(1)" pk
    | Some (2, Pbrt.Bytes) -> v.disambiguator <- Pbrt.Decoder.string d
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor), field(2)" pk
    | Some (3, Pbrt.Varint) -> v.suffix <- decode_descriptor_suffix d
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Scip_types.name = v.name;
     Scip_types.disambiguator = v.disambiguator;
     Scip_types.suffix = v.suffix;
   }
    : Scip_types.descriptor)

let rec decode_symbol d =
  let v = default_symbol_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.descriptors <- List.rev v.descriptors;
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.scheme <- Pbrt.Decoder.string d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(symbol), field(1)" pk
    | Some (2, Pbrt.Bytes) ->
        v.package <- Some (decode_package (Pbrt.Decoder.nested d))
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(symbol), field(2)" pk
    | Some (3, Pbrt.Bytes) ->
        v.descriptors <-
          decode_descriptor (Pbrt.Decoder.nested d) :: v.descriptors
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(symbol), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Scip_types.scheme = v.scheme;
     Scip_types.package = v.package;
     Scip_types.descriptors = v.descriptors;
   }
    : Scip_types.symbol)

let rec decode_symbol_role d =
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Scip_types.Unspecified_symbol_role : Scip_types.symbol_role)
  | 1 -> (Scip_types.Definition : Scip_types.symbol_role)
  | 2 -> (Scip_types.Import : Scip_types.symbol_role)
  | 4 -> (Scip_types.Write_access : Scip_types.symbol_role)
  | 8 -> (Scip_types.Read_access : Scip_types.symbol_role)
  | 16 -> (Scip_types.Generated : Scip_types.symbol_role)
  | 32 -> (Scip_types.Test : Scip_types.symbol_role)
  | _ -> Pbrt.Decoder.malformed_variant "symbol_role"

let rec decode_language d =
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Scip_types.Unspecified_language : Scip_types.language)
  | 60 -> (Scip_types.Abap : Scip_types.language)
  | 49 -> (Scip_types.Apl : Scip_types.language)
  | 39 -> (Scip_types.Ada : Scip_types.language)
  | 45 -> (Scip_types.Agda : Scip_types.language)
  | 86 -> (Scip_types.Ascii_doc : Scip_types.language)
  | 58 -> (Scip_types.Assembly : Scip_types.language)
  | 66 -> (Scip_types.Awk : Scip_types.language)
  | 68 -> (Scip_types.Bat : Scip_types.language)
  | 81 -> (Scip_types.Bib_te_x : Scip_types.language)
  | 34 -> (Scip_types.C : Scip_types.language)
  | 59 -> (Scip_types.Cobol : Scip_types.language)
  | 35 -> (Scip_types.Cpp : Scip_types.language)
  | 26 -> (Scip_types.Css : Scip_types.language)
  | 1 -> (Scip_types.Csharp : Scip_types.language)
  | 8 -> (Scip_types.Clojure : Scip_types.language)
  | 21 -> (Scip_types.Coffeescript : Scip_types.language)
  | 9 -> (Scip_types.Common_lisp : Scip_types.language)
  | 47 -> (Scip_types.Coq : Scip_types.language)
  | 3 -> (Scip_types.Dart : Scip_types.language)
  | 57 -> (Scip_types.Delphi : Scip_types.language)
  | 88 -> (Scip_types.Diff : Scip_types.language)
  | 80 -> (Scip_types.Dockerfile : Scip_types.language)
  | 50 -> (Scip_types.Dyalog : Scip_types.language)
  | 17 -> (Scip_types.Elixir : Scip_types.language)
  | 18 -> (Scip_types.Erlang : Scip_types.language)
  | 42 -> (Scip_types.Fsharp : Scip_types.language)
  | 65 -> (Scip_types.Fish : Scip_types.language)
  | 24 -> (Scip_types.Flow : Scip_types.language)
  | 56 -> (Scip_types.Fortran : Scip_types.language)
  | 91 -> (Scip_types.Git_commit : Scip_types.language)
  | 89 -> (Scip_types.Git_config : Scip_types.language)
  | 92 -> (Scip_types.Git_rebase : Scip_types.language)
  | 33 -> (Scip_types.Go : Scip_types.language)
  | 7 -> (Scip_types.Groovy : Scip_types.language)
  | 30 -> (Scip_types.Html : Scip_types.language)
  | 20 -> (Scip_types.Hack : Scip_types.language)
  | 90 -> (Scip_types.Handlebars : Scip_types.language)
  | 44 -> (Scip_types.Haskell : Scip_types.language)
  | 46 -> (Scip_types.Idris : Scip_types.language)
  | 72 -> (Scip_types.Ini : Scip_types.language)
  | 51 -> (Scip_types.J : Scip_types.language)
  | 75 -> (Scip_types.Json : Scip_types.language)
  | 6 -> (Scip_types.Java : Scip_types.language)
  | 22 -> (Scip_types.Java_script : Scip_types.language)
  | 93 -> (Scip_types.Java_script_react : Scip_types.language)
  | 76 -> (Scip_types.Jsonnet : Scip_types.language)
  | 55 -> (Scip_types.Julia : Scip_types.language)
  | 4 -> (Scip_types.Kotlin : Scip_types.language)
  | 83 -> (Scip_types.La_te_x : Scip_types.language)
  | 48 -> (Scip_types.Lean : Scip_types.language)
  | 27 -> (Scip_types.Less : Scip_types.language)
  | 12 -> (Scip_types.Lua : Scip_types.language)
  | 79 -> (Scip_types.Makefile : Scip_types.language)
  | 84 -> (Scip_types.Markdown : Scip_types.language)
  | 52 -> (Scip_types.Matlab : Scip_types.language)
  | 77 -> (Scip_types.Nix : Scip_types.language)
  | 41 -> (Scip_types.Ocaml : Scip_types.language)
  | 36 -> (Scip_types.Objective_c : Scip_types.language)
  | 37 -> (Scip_types.Objective_cpp : Scip_types.language)
  | 19 -> (Scip_types.Php : Scip_types.language)
  | 70 -> (Scip_types.Plsql : Scip_types.language)
  | 13 -> (Scip_types.Perl : Scip_types.language)
  | 67 -> (Scip_types.Power_shell : Scip_types.language)
  | 71 -> (Scip_types.Prolog : Scip_types.language)
  | 15 -> (Scip_types.Python : Scip_types.language)
  | 54 -> (Scip_types.R : Scip_types.language)
  | 11 -> (Scip_types.Racket : Scip_types.language)
  | 14 -> (Scip_types.Raku : Scip_types.language)
  | 62 -> (Scip_types.Razor : Scip_types.language)
  | 85 -> (Scip_types.Re_st : Scip_types.language)
  | 16 -> (Scip_types.Ruby : Scip_types.language)
  | 40 -> (Scip_types.Rust : Scip_types.language)
  | 61 -> (Scip_types.Sas : Scip_types.language)
  | 29 -> (Scip_types.Scss : Scip_types.language)
  | 43 -> (Scip_types.Sml : Scip_types.language)
  | 69 -> (Scip_types.Sql : Scip_types.language)
  | 28 -> (Scip_types.Sass : Scip_types.language)
  | 5 -> (Scip_types.Scala : Scip_types.language)
  | 10 -> (Scip_types.Scheme : Scip_types.language)
  | 64 -> (Scip_types.Shell_script : Scip_types.language)
  | 78 -> (Scip_types.Skylark : Scip_types.language)
  | 2 -> (Scip_types.Swift : Scip_types.language)
  | 73 -> (Scip_types.Toml : Scip_types.language)
  | 82 -> (Scip_types.Te_x : Scip_types.language)
  | 23 -> (Scip_types.Type_script : Scip_types.language)
  | 94 -> (Scip_types.Type_script_react : Scip_types.language)
  | 63 -> (Scip_types.Visual_basic : Scip_types.language)
  | 25 -> (Scip_types.Vue : Scip_types.language)
  | 53 -> (Scip_types.Wolfram : Scip_types.language)
  | 31 -> (Scip_types.Xml : Scip_types.language)
  | 32 -> (Scip_types.Xsl : Scip_types.language)
  | 74 -> (Scip_types.Yaml : Scip_types.language)
  | 38 -> (Scip_types.Zig : Scip_types.language)
  | _ -> Pbrt.Decoder.malformed_variant "language"

let rec encode_protocol_version (v : Scip_types.protocol_version) encoder =
  match v with
  | Scip_types.Unspecified_protocol_version ->
      Pbrt.Encoder.int_as_varint 0 encoder

let rec encode_tool_info (v : Scip_types.tool_info) encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.name encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.version encoder;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder)
    v.Scip_types.arguments;
  ()

let rec encode_text_encoding (v : Scip_types.text_encoding) encoder =
  match v with
  | Scip_types.Unspecified_text_encoding -> Pbrt.Encoder.int_as_varint 0 encoder
  | Scip_types.Utf8 -> Pbrt.Encoder.int_as_varint 1 encoder
  | Scip_types.Utf16 -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_metadata (v : Scip_types.metadata) encoder =
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder;
  encode_protocol_version v.Scip_types.version encoder;
  (match v.Scip_types.tool_info with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_tool_info x) encoder
  | None -> ());
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.project_root encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder;
  encode_text_encoding v.Scip_types.text_document_encoding encoder;
  ()

let rec encode_syntax_kind (v : Scip_types.syntax_kind) encoder =
  match v with
  | Scip_types.Unspecified_syntax_kind -> Pbrt.Encoder.int_as_varint 0 encoder
  | Scip_types.Comment -> Pbrt.Encoder.int_as_varint 1 encoder
  | Scip_types.Punctuation_delimiter -> Pbrt.Encoder.int_as_varint 2 encoder
  | Scip_types.Punctuation_bracket -> Pbrt.Encoder.int_as_varint 3 encoder
  | Scip_types.Keyword -> Pbrt.Encoder.int_as_varint 4 encoder
  | Scip_types.Identifier_keyword -> Pbrt.Encoder.int_as_varint 4 encoder
  | Scip_types.Identifier_operator -> Pbrt.Encoder.int_as_varint 5 encoder
  | Scip_types.Identifier -> Pbrt.Encoder.int_as_varint 6 encoder
  | Scip_types.Identifier_builtin -> Pbrt.Encoder.int_as_varint 7 encoder
  | Scip_types.Identifier_null -> Pbrt.Encoder.int_as_varint 8 encoder
  | Scip_types.Identifier_constant -> Pbrt.Encoder.int_as_varint 9 encoder
  | Scip_types.Identifier_mutable_global ->
      Pbrt.Encoder.int_as_varint 10 encoder
  | Scip_types.Identifier_parameter -> Pbrt.Encoder.int_as_varint 11 encoder
  | Scip_types.Identifier_local -> Pbrt.Encoder.int_as_varint 12 encoder
  | Scip_types.Identifier_shadowed -> Pbrt.Encoder.int_as_varint 13 encoder
  | Scip_types.Identifier_namespace -> Pbrt.Encoder.int_as_varint 14 encoder
  | Scip_types.Identifier_module -> Pbrt.Encoder.int_as_varint 14 encoder
  | Scip_types.Identifier_function -> Pbrt.Encoder.int_as_varint 15 encoder
  | Scip_types.Identifier_function_definition ->
      Pbrt.Encoder.int_as_varint 16 encoder
  | Scip_types.Identifier_macro -> Pbrt.Encoder.int_as_varint 17 encoder
  | Scip_types.Identifier_macro_definition ->
      Pbrt.Encoder.int_as_varint 18 encoder
  | Scip_types.Identifier_type -> Pbrt.Encoder.int_as_varint 19 encoder
  | Scip_types.Identifier_builtin_type -> Pbrt.Encoder.int_as_varint 20 encoder
  | Scip_types.Identifier_attribute -> Pbrt.Encoder.int_as_varint 21 encoder
  | Scip_types.Regex_escape -> Pbrt.Encoder.int_as_varint 22 encoder
  | Scip_types.Regex_repeated -> Pbrt.Encoder.int_as_varint 23 encoder
  | Scip_types.Regex_wildcard -> Pbrt.Encoder.int_as_varint 24 encoder
  | Scip_types.Regex_delimiter -> Pbrt.Encoder.int_as_varint 25 encoder
  | Scip_types.Regex_join -> Pbrt.Encoder.int_as_varint 26 encoder
  | Scip_types.String_literal -> Pbrt.Encoder.int_as_varint 27 encoder
  | Scip_types.String_literal_escape -> Pbrt.Encoder.int_as_varint 28 encoder
  | Scip_types.String_literal_special -> Pbrt.Encoder.int_as_varint 29 encoder
  | Scip_types.String_literal_key -> Pbrt.Encoder.int_as_varint 30 encoder
  | Scip_types.Character_literal -> Pbrt.Encoder.int_as_varint 31 encoder
  | Scip_types.Numeric_literal -> Pbrt.Encoder.int_as_varint 32 encoder
  | Scip_types.Boolean_literal -> Pbrt.Encoder.int_as_varint 33 encoder
  | Scip_types.Tag -> Pbrt.Encoder.int_as_varint 34 encoder
  | Scip_types.Tag_attribute -> Pbrt.Encoder.int_as_varint 35 encoder
  | Scip_types.Tag_delimiter -> Pbrt.Encoder.int_as_varint 36 encoder

let rec encode_severity (v : Scip_types.severity) encoder =
  match v with
  | Scip_types.Unspecified_severity -> Pbrt.Encoder.int_as_varint 0 encoder
  | Scip_types.Error -> Pbrt.Encoder.int_as_varint 1 encoder
  | Scip_types.Warning -> Pbrt.Encoder.int_as_varint 2 encoder
  | Scip_types.Information -> Pbrt.Encoder.int_as_varint 3 encoder
  | Scip_types.Hint -> Pbrt.Encoder.int_as_varint 4 encoder

let rec encode_diagnostic_tag (v : Scip_types.diagnostic_tag) encoder =
  match v with
  | Scip_types.Unspecified_diagnostic_tag ->
      Pbrt.Encoder.int_as_varint 0 encoder
  | Scip_types.Unnecessary -> Pbrt.Encoder.int_as_varint 1 encoder
  | Scip_types.Deprecated -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_diagnostic (v : Scip_types.diagnostic) encoder =
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder;
  encode_severity v.Scip_types.severity encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.code encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.message encoder;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.source encoder;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (5, Pbrt.Varint) encoder;
      encode_diagnostic_tag x encoder)
    v.Scip_types.tags;
  ()

let rec encode_occurrence (v : Scip_types.occurrence) encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.nested
    (fun encoder ->
      List.iter
        (fun x -> Pbrt.Encoder.int32_as_varint x encoder)
        v.Scip_types.range)
    encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.symbol encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder;
  Pbrt.Encoder.int32_as_varint v.Scip_types.symbol_roles encoder;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder)
    v.Scip_types.override_documentation;
  Pbrt.Encoder.key (5, Pbrt.Varint) encoder;
  encode_syntax_kind v.Scip_types.syntax_kind encoder;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (6, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_diagnostic x) encoder)
    v.Scip_types.diagnostics;
  ()

let rec encode_relationship (v : Scip_types.relationship) encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.symbol encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder;
  Pbrt.Encoder.bool v.Scip_types.is_reference encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder;
  Pbrt.Encoder.bool v.Scip_types.is_implementation encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder;
  Pbrt.Encoder.bool v.Scip_types.is_type_definition encoder;
  Pbrt.Encoder.key (5, Pbrt.Varint) encoder;
  Pbrt.Encoder.bool v.Scip_types.is_definition encoder;
  ()

let rec encode_symbol_information (v : Scip_types.symbol_information) encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.symbol encoder;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder)
    v.Scip_types.documentation;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_relationship x) encoder)
    v.Scip_types.relationships;
  ()

let rec encode_document (v : Scip_types.document) encoder =
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.language encoder;
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.relative_path encoder;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_occurrence x) encoder)
    v.Scip_types.occurrences;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_symbol_information x) encoder)
    v.Scip_types.symbols;
  ()

let rec encode_index (v : Scip_types.index) encoder =
  (match v.Scip_types.metadata with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_metadata x) encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_document x) encoder)
    v.Scip_types.documents;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_symbol_information x) encoder)
    v.Scip_types.external_symbols;
  ()

let rec encode_package (v : Scip_types.package) encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.manager encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.name encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.version encoder;
  ()

let rec encode_descriptor_suffix (v : Scip_types.descriptor_suffix) encoder =
  match v with
  | Scip_types.Unspecified_suffix -> Pbrt.Encoder.int_as_varint 0 encoder
  | Scip_types.Namespace -> Pbrt.Encoder.int_as_varint 1 encoder
  | Scip_types.Package -> Pbrt.Encoder.int_as_varint 1 encoder
  | Scip_types.Type -> Pbrt.Encoder.int_as_varint 2 encoder
  | Scip_types.Term -> Pbrt.Encoder.int_as_varint 3 encoder
  | Scip_types.Method -> Pbrt.Encoder.int_as_varint 4 encoder
  | Scip_types.Type_parameter -> Pbrt.Encoder.int_as_varint 5 encoder
  | Scip_types.Parameter -> Pbrt.Encoder.int_as_varint 6 encoder
  | Scip_types.Macro -> Pbrt.Encoder.int_as_varint 9 encoder
  | Scip_types.Meta -> Pbrt.Encoder.int_as_varint 7 encoder
  | Scip_types.Local -> Pbrt.Encoder.int_as_varint 8 encoder

let rec encode_descriptor (v : Scip_types.descriptor) encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.name encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.disambiguator encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder;
  encode_descriptor_suffix v.Scip_types.suffix encoder;
  ()

let rec encode_symbol (v : Scip_types.symbol) encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Scip_types.scheme encoder;
  (match v.Scip_types.package with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_package x) encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_descriptor x) encoder)
    v.Scip_types.descriptors;
  ()

let rec encode_symbol_role (v : Scip_types.symbol_role) encoder =
  match v with
  | Scip_types.Unspecified_symbol_role -> Pbrt.Encoder.int_as_varint 0 encoder
  | Scip_types.Definition -> Pbrt.Encoder.int_as_varint 1 encoder
  | Scip_types.Import -> Pbrt.Encoder.int_as_varint 2 encoder
  | Scip_types.Write_access -> Pbrt.Encoder.int_as_varint 4 encoder
  | Scip_types.Read_access -> Pbrt.Encoder.int_as_varint 8 encoder
  | Scip_types.Generated -> Pbrt.Encoder.int_as_varint 16 encoder
  | Scip_types.Test -> Pbrt.Encoder.int_as_varint 32 encoder

let rec encode_language (v : Scip_types.language) encoder =
  match v with
  | Scip_types.Unspecified_language -> Pbrt.Encoder.int_as_varint 0 encoder
  | Scip_types.Abap -> Pbrt.Encoder.int_as_varint 60 encoder
  | Scip_types.Apl -> Pbrt.Encoder.int_as_varint 49 encoder
  | Scip_types.Ada -> Pbrt.Encoder.int_as_varint 39 encoder
  | Scip_types.Agda -> Pbrt.Encoder.int_as_varint 45 encoder
  | Scip_types.Ascii_doc -> Pbrt.Encoder.int_as_varint 86 encoder
  | Scip_types.Assembly -> Pbrt.Encoder.int_as_varint 58 encoder
  | Scip_types.Awk -> Pbrt.Encoder.int_as_varint 66 encoder
  | Scip_types.Bat -> Pbrt.Encoder.int_as_varint 68 encoder
  | Scip_types.Bib_te_x -> Pbrt.Encoder.int_as_varint 81 encoder
  | Scip_types.C -> Pbrt.Encoder.int_as_varint 34 encoder
  | Scip_types.Cobol -> Pbrt.Encoder.int_as_varint 59 encoder
  | Scip_types.Cpp -> Pbrt.Encoder.int_as_varint 35 encoder
  | Scip_types.Css -> Pbrt.Encoder.int_as_varint 26 encoder
  | Scip_types.Csharp -> Pbrt.Encoder.int_as_varint 1 encoder
  | Scip_types.Clojure -> Pbrt.Encoder.int_as_varint 8 encoder
  | Scip_types.Coffeescript -> Pbrt.Encoder.int_as_varint 21 encoder
  | Scip_types.Common_lisp -> Pbrt.Encoder.int_as_varint 9 encoder
  | Scip_types.Coq -> Pbrt.Encoder.int_as_varint 47 encoder
  | Scip_types.Dart -> Pbrt.Encoder.int_as_varint 3 encoder
  | Scip_types.Delphi -> Pbrt.Encoder.int_as_varint 57 encoder
  | Scip_types.Diff -> Pbrt.Encoder.int_as_varint 88 encoder
  | Scip_types.Dockerfile -> Pbrt.Encoder.int_as_varint 80 encoder
  | Scip_types.Dyalog -> Pbrt.Encoder.int_as_varint 50 encoder
  | Scip_types.Elixir -> Pbrt.Encoder.int_as_varint 17 encoder
  | Scip_types.Erlang -> Pbrt.Encoder.int_as_varint 18 encoder
  | Scip_types.Fsharp -> Pbrt.Encoder.int_as_varint 42 encoder
  | Scip_types.Fish -> Pbrt.Encoder.int_as_varint 65 encoder
  | Scip_types.Flow -> Pbrt.Encoder.int_as_varint 24 encoder
  | Scip_types.Fortran -> Pbrt.Encoder.int_as_varint 56 encoder
  | Scip_types.Git_commit -> Pbrt.Encoder.int_as_varint 91 encoder
  | Scip_types.Git_config -> Pbrt.Encoder.int_as_varint 89 encoder
  | Scip_types.Git_rebase -> Pbrt.Encoder.int_as_varint 92 encoder
  | Scip_types.Go -> Pbrt.Encoder.int_as_varint 33 encoder
  | Scip_types.Groovy -> Pbrt.Encoder.int_as_varint 7 encoder
  | Scip_types.Html -> Pbrt.Encoder.int_as_varint 30 encoder
  | Scip_types.Hack -> Pbrt.Encoder.int_as_varint 20 encoder
  | Scip_types.Handlebars -> Pbrt.Encoder.int_as_varint 90 encoder
  | Scip_types.Haskell -> Pbrt.Encoder.int_as_varint 44 encoder
  | Scip_types.Idris -> Pbrt.Encoder.int_as_varint 46 encoder
  | Scip_types.Ini -> Pbrt.Encoder.int_as_varint 72 encoder
  | Scip_types.J -> Pbrt.Encoder.int_as_varint 51 encoder
  | Scip_types.Json -> Pbrt.Encoder.int_as_varint 75 encoder
  | Scip_types.Java -> Pbrt.Encoder.int_as_varint 6 encoder
  | Scip_types.Java_script -> Pbrt.Encoder.int_as_varint 22 encoder
  | Scip_types.Java_script_react -> Pbrt.Encoder.int_as_varint 93 encoder
  | Scip_types.Jsonnet -> Pbrt.Encoder.int_as_varint 76 encoder
  | Scip_types.Julia -> Pbrt.Encoder.int_as_varint 55 encoder
  | Scip_types.Kotlin -> Pbrt.Encoder.int_as_varint 4 encoder
  | Scip_types.La_te_x -> Pbrt.Encoder.int_as_varint 83 encoder
  | Scip_types.Lean -> Pbrt.Encoder.int_as_varint 48 encoder
  | Scip_types.Less -> Pbrt.Encoder.int_as_varint 27 encoder
  | Scip_types.Lua -> Pbrt.Encoder.int_as_varint 12 encoder
  | Scip_types.Makefile -> Pbrt.Encoder.int_as_varint 79 encoder
  | Scip_types.Markdown -> Pbrt.Encoder.int_as_varint 84 encoder
  | Scip_types.Matlab -> Pbrt.Encoder.int_as_varint 52 encoder
  | Scip_types.Nix -> Pbrt.Encoder.int_as_varint 77 encoder
  | Scip_types.Ocaml -> Pbrt.Encoder.int_as_varint 41 encoder
  | Scip_types.Objective_c -> Pbrt.Encoder.int_as_varint 36 encoder
  | Scip_types.Objective_cpp -> Pbrt.Encoder.int_as_varint 37 encoder
  | Scip_types.Php -> Pbrt.Encoder.int_as_varint 19 encoder
  | Scip_types.Plsql -> Pbrt.Encoder.int_as_varint 70 encoder
  | Scip_types.Perl -> Pbrt.Encoder.int_as_varint 13 encoder
  | Scip_types.Power_shell -> Pbrt.Encoder.int_as_varint 67 encoder
  | Scip_types.Prolog -> Pbrt.Encoder.int_as_varint 71 encoder
  | Scip_types.Python -> Pbrt.Encoder.int_as_varint 15 encoder
  | Scip_types.R -> Pbrt.Encoder.int_as_varint 54 encoder
  | Scip_types.Racket -> Pbrt.Encoder.int_as_varint 11 encoder
  | Scip_types.Raku -> Pbrt.Encoder.int_as_varint 14 encoder
  | Scip_types.Razor -> Pbrt.Encoder.int_as_varint 62 encoder
  | Scip_types.Re_st -> Pbrt.Encoder.int_as_varint 85 encoder
  | Scip_types.Ruby -> Pbrt.Encoder.int_as_varint 16 encoder
  | Scip_types.Rust -> Pbrt.Encoder.int_as_varint 40 encoder
  | Scip_types.Sas -> Pbrt.Encoder.int_as_varint 61 encoder
  | Scip_types.Scss -> Pbrt.Encoder.int_as_varint 29 encoder
  | Scip_types.Sml -> Pbrt.Encoder.int_as_varint 43 encoder
  | Scip_types.Sql -> Pbrt.Encoder.int_as_varint 69 encoder
  | Scip_types.Sass -> Pbrt.Encoder.int_as_varint 28 encoder
  | Scip_types.Scala -> Pbrt.Encoder.int_as_varint 5 encoder
  | Scip_types.Scheme -> Pbrt.Encoder.int_as_varint 10 encoder
  | Scip_types.Shell_script -> Pbrt.Encoder.int_as_varint 64 encoder
  | Scip_types.Skylark -> Pbrt.Encoder.int_as_varint 78 encoder
  | Scip_types.Swift -> Pbrt.Encoder.int_as_varint 2 encoder
  | Scip_types.Toml -> Pbrt.Encoder.int_as_varint 73 encoder
  | Scip_types.Te_x -> Pbrt.Encoder.int_as_varint 82 encoder
  | Scip_types.Type_script -> Pbrt.Encoder.int_as_varint 23 encoder
  | Scip_types.Type_script_react -> Pbrt.Encoder.int_as_varint 94 encoder
  | Scip_types.Visual_basic -> Pbrt.Encoder.int_as_varint 63 encoder
  | Scip_types.Vue -> Pbrt.Encoder.int_as_varint 25 encoder
  | Scip_types.Wolfram -> Pbrt.Encoder.int_as_varint 53 encoder
  | Scip_types.Xml -> Pbrt.Encoder.int_as_varint 31 encoder
  | Scip_types.Xsl -> Pbrt.Encoder.int_as_varint 32 encoder
  | Scip_types.Yaml -> Pbrt.Encoder.int_as_varint 74 encoder
  | Scip_types.Zig -> Pbrt.Encoder.int_as_varint 38 encoder
