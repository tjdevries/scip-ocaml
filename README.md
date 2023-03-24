# scip-ocaml


## Regenerate `scip` bindings

This should happen automatically with dune now :) Just make sure you have the deps installed.

You can update `lib/scip.proto` to generate new bindings

## TODO:

first pass:
- visit the tree (find all the definitions)
- save symbols for those definitions in a map (position : Location.t -> symbol : string)

second pass:
- visit the tree
- lookup locations to see if we have a symbol
- if it's a def, emit def. if it's a ref, emit ref.
