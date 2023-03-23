(** scip.proto Binary Encoding *)

(** {2 Protobuf Encoding} *)

(** [encode_protocol_version v encoder] encodes [v] with the given [encoder] *)
val encode_protocol_version : Scip_types.protocol_version -> Pbrt.Encoder.t -> unit

(** [encode_tool_info v encoder] encodes [v] with the given [encoder] *)
val encode_tool_info : Scip_types.tool_info -> Pbrt.Encoder.t -> unit

(** [encode_text_encoding v encoder] encodes [v] with the given [encoder] *)
val encode_text_encoding : Scip_types.text_encoding -> Pbrt.Encoder.t -> unit

(** [encode_metadata v encoder] encodes [v] with the given [encoder] *)
val encode_metadata : Scip_types.metadata -> Pbrt.Encoder.t -> unit

(** [encode_syntax_kind v encoder] encodes [v] with the given [encoder] *)
val encode_syntax_kind : Scip_types.syntax_kind -> Pbrt.Encoder.t -> unit

(** [encode_severity v encoder] encodes [v] with the given [encoder] *)
val encode_severity : Scip_types.severity -> Pbrt.Encoder.t -> unit

(** [encode_diagnostic_tag v encoder] encodes [v] with the given [encoder] *)
val encode_diagnostic_tag : Scip_types.diagnostic_tag -> Pbrt.Encoder.t -> unit

(** [encode_diagnostic v encoder] encodes [v] with the given [encoder] *)
val encode_diagnostic : Scip_types.diagnostic -> Pbrt.Encoder.t -> unit

(** [encode_occurrence v encoder] encodes [v] with the given [encoder] *)
val encode_occurrence : Scip_types.occurrence -> Pbrt.Encoder.t -> unit

(** [encode_relationship v encoder] encodes [v] with the given [encoder] *)
val encode_relationship : Scip_types.relationship -> Pbrt.Encoder.t -> unit

(** [encode_symbol_information v encoder] encodes [v] with the given [encoder] *)
val encode_symbol_information : Scip_types.symbol_information -> Pbrt.Encoder.t -> unit

(** [encode_document v encoder] encodes [v] with the given [encoder] *)
val encode_document : Scip_types.document -> Pbrt.Encoder.t -> unit

(** [encode_index v encoder] encodes [v] with the given [encoder] *)
val encode_index : Scip_types.index -> Pbrt.Encoder.t -> unit

(** [encode_package v encoder] encodes [v] with the given [encoder] *)
val encode_package : Scip_types.package -> Pbrt.Encoder.t -> unit

(** [encode_descriptor_suffix v encoder] encodes [v] with the given [encoder] *)
val encode_descriptor_suffix : Scip_types.descriptor_suffix -> Pbrt.Encoder.t -> unit

(** [encode_descriptor v encoder] encodes [v] with the given [encoder] *)
val encode_descriptor : Scip_types.descriptor -> Pbrt.Encoder.t -> unit

(** [encode_symbol v encoder] encodes [v] with the given [encoder] *)
val encode_symbol : Scip_types.symbol -> Pbrt.Encoder.t -> unit

(** [encode_symbol_role v encoder] encodes [v] with the given [encoder] *)
val encode_symbol_role : Scip_types.symbol_role -> Pbrt.Encoder.t -> unit

(** [encode_language v encoder] encodes [v] with the given [encoder] *)
val encode_language : Scip_types.language -> Pbrt.Encoder.t -> unit

(** {2 Protobuf Decoding} *)

(** [decode_protocol_version decoder] decodes a [protocol_version] value from [decoder] *)
val decode_protocol_version : Pbrt.Decoder.t -> Scip_types.protocol_version

(** [decode_tool_info decoder] decodes a [tool_info] value from [decoder] *)
val decode_tool_info : Pbrt.Decoder.t -> Scip_types.tool_info

(** [decode_text_encoding decoder] decodes a [text_encoding] value from [decoder] *)
val decode_text_encoding : Pbrt.Decoder.t -> Scip_types.text_encoding

(** [decode_metadata decoder] decodes a [metadata] value from [decoder] *)
val decode_metadata : Pbrt.Decoder.t -> Scip_types.metadata

(** [decode_syntax_kind decoder] decodes a [syntax_kind] value from [decoder] *)
val decode_syntax_kind : Pbrt.Decoder.t -> Scip_types.syntax_kind

(** [decode_severity decoder] decodes a [severity] value from [decoder] *)
val decode_severity : Pbrt.Decoder.t -> Scip_types.severity

(** [decode_diagnostic_tag decoder] decodes a [diagnostic_tag] value from [decoder] *)
val decode_diagnostic_tag : Pbrt.Decoder.t -> Scip_types.diagnostic_tag

(** [decode_diagnostic decoder] decodes a [diagnostic] value from [decoder] *)
val decode_diagnostic : Pbrt.Decoder.t -> Scip_types.diagnostic

(** [decode_occurrence decoder] decodes a [occurrence] value from [decoder] *)
val decode_occurrence : Pbrt.Decoder.t -> Scip_types.occurrence

(** [decode_relationship decoder] decodes a [relationship] value from [decoder] *)
val decode_relationship : Pbrt.Decoder.t -> Scip_types.relationship

(** [decode_symbol_information decoder] decodes a [symbol_information] value from [decoder] *)
val decode_symbol_information : Pbrt.Decoder.t -> Scip_types.symbol_information

(** [decode_document decoder] decodes a [document] value from [decoder] *)
val decode_document : Pbrt.Decoder.t -> Scip_types.document

(** [decode_index decoder] decodes a [index] value from [decoder] *)
val decode_index : Pbrt.Decoder.t -> Scip_types.index

(** [decode_package decoder] decodes a [package] value from [decoder] *)
val decode_package : Pbrt.Decoder.t -> Scip_types.package

(** [decode_descriptor_suffix decoder] decodes a [descriptor_suffix] value from [decoder] *)
val decode_descriptor_suffix : Pbrt.Decoder.t -> Scip_types.descriptor_suffix

(** [decode_descriptor decoder] decodes a [descriptor] value from [decoder] *)
val decode_descriptor : Pbrt.Decoder.t -> Scip_types.descriptor

(** [decode_symbol decoder] decodes a [symbol] value from [decoder] *)
val decode_symbol : Pbrt.Decoder.t -> Scip_types.symbol

(** [decode_symbol_role decoder] decodes a [symbol_role] value from [decoder] *)
val decode_symbol_role : Pbrt.Decoder.t -> Scip_types.symbol_role

(** [decode_language decoder] decodes a [language] value from [decoder] *)
val decode_language : Pbrt.Decoder.t -> Scip_types.language
