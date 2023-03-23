(** scip.proto Pretty Printing *)

(** {2 Formatters} *)

(** [pp_protocol_version v] formats v *)
val pp_protocol_version : Format.formatter -> Scip_types.protocol_version -> unit

(** [pp_tool_info v] formats v *)
val pp_tool_info : Format.formatter -> Scip_types.tool_info -> unit

(** [pp_text_encoding v] formats v *)
val pp_text_encoding : Format.formatter -> Scip_types.text_encoding -> unit

(** [pp_metadata v] formats v *)
val pp_metadata : Format.formatter -> Scip_types.metadata -> unit

(** [pp_syntax_kind v] formats v *)
val pp_syntax_kind : Format.formatter -> Scip_types.syntax_kind -> unit

(** [pp_severity v] formats v *)
val pp_severity : Format.formatter -> Scip_types.severity -> unit

(** [pp_diagnostic_tag v] formats v *)
val pp_diagnostic_tag : Format.formatter -> Scip_types.diagnostic_tag -> unit

(** [pp_diagnostic v] formats v *)
val pp_diagnostic : Format.formatter -> Scip_types.diagnostic -> unit

(** [pp_occurrence v] formats v *)
val pp_occurrence : Format.formatter -> Scip_types.occurrence -> unit

(** [pp_relationship v] formats v *)
val pp_relationship : Format.formatter -> Scip_types.relationship -> unit

(** [pp_symbol_information v] formats v *)
val pp_symbol_information : Format.formatter -> Scip_types.symbol_information -> unit

(** [pp_document v] formats v *)
val pp_document : Format.formatter -> Scip_types.document -> unit

(** [pp_index v] formats v *)
val pp_index : Format.formatter -> Scip_types.index -> unit

(** [pp_package v] formats v *)
val pp_package : Format.formatter -> Scip_types.package -> unit

(** [pp_descriptor_suffix v] formats v *)
val pp_descriptor_suffix : Format.formatter -> Scip_types.descriptor_suffix -> unit

(** [pp_descriptor v] formats v *)
val pp_descriptor : Format.formatter -> Scip_types.descriptor -> unit

(** [pp_symbol v] formats v *)
val pp_symbol : Format.formatter -> Scip_types.symbol -> unit

(** [pp_symbol_role v] formats v *)
val pp_symbol_role : Format.formatter -> Scip_types.symbol_role -> unit

(** [pp_language v] formats v *)
val pp_language : Format.formatter -> Scip_types.language -> unit
