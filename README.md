# scip-ocaml


## Regenerate `scip` bindings

TODO: Should probably nest it one deeper
TODO: You have to add some ignore warnings, but you can probably do that automatically in dune

```
❯ ocaml-protoc -binary -pp -ml_out ./lib/ ~/sourcegraph/scip/scip.proto
```
