# Docs CI

Building docs using odoc.

## Running

```
dune exec -- docs-ci \
    --ocluster-submission cap/XXX.cap \
    --ssh-host ci.mirage.io \
    --ssh-user docs \
    --ssh-privkey cap/id_rsa \
    --ssh-pubkey cap/id_rsa.pub \
    --ssh-folder /data/docs-ci \
    --ssh-endpoint https://ci.mirage.io/staging \
    --jobs 6 \
    --filter mirage \
    --limit 6
```
