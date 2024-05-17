This is a nixified [Bonsai RPC Chat Example](https://github.com/janestreet/bonsai/tree/v0.16/examples/open_source/rpc_chat).

It also has gzip compression support and integrates Tailwind.

## How to run

```bash
$ nix-shell
$ dune exec -- server/bin/main
```

Then navigate to http://localhost:8080/. Open two tabs and try sending messages
in the same chat room.
