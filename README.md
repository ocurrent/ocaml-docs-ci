# Docs CI

Building docs using odoc.

## Running

```
dune exec -- ocaml-docs-ci \
    --ocluster-submission cap/XXX.cap \
    --ssh-host ci.mirage.io \
    --ssh-user docs \
    --ssh-privkey cap/id_rsa \
    --ssh-pubkey cap/id_rsa.pub \
    --ssh-folder /data/ocaml-docs-ci \
    --ssh-endpoint https://ci.mirage.io/staging \
    --jobs 6 \
    --filter mirage \
    --limit 6
```

## Documentation

To understand better how it works under the hood, you can check the [pipeline](doc/pipeline-diagram.md) documentation.
