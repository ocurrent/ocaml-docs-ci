## Docs2web


Docs2web is a temporary and experimental website to display the odoc-generated documentation of all packages.
It relies on https://github.com/ocurrent/ocaml-docs-ci to build the documentation, and acts as a mere proxy 
for the generated files.

```
dune exec -- docs2web --api=<DOCS_CI_ENDPOINT> --port=8082
```
