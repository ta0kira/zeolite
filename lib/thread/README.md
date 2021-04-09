# [Zeolite Thread Library](https://github.com/ta0kira/zeolite/tree/master/lib/thread)

## Library Usage

- See filenames ending in `.0rp` for documentation.

- Include `"lib/thread"` in `public_deps` or `private_deps` in your
  `.zeolite-module`.

## Configuring and Building

This library is automatically built by `zeolite-setup`.

If you run into linker errors when building binaries or running tests, you might
need to update `link_flags` in `lib/thread/.zeolite-module` to either change or
remove dependence on `-lpthread`, then manually build the module again.

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p "$ZEOLITE_PATH" -r lib/thread
```
