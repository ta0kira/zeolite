# [Zeolite Thread Library](https://github.com/ta0kira/zeolite/tree/master/lib/thread)

## Library Usage

- See filenames ending in `.0rp` for documentation.

- Include `"lib/thread"` in `public_deps` or `private_deps` in your
  `.zeolite-module`.

## Configuring and Building

This library *is not* automatically built by `zeolite-setup`; you need to build
it manually. This is because there might be issues with linker dependencies
between different systems.

1. If you are running Linux, the config should work as-is. If you are running
   FreeBSD, you might need to remove or change `"-lpthread"` at the bottom of
   `.zeolite-module`.

   To build the library:

   ```shell
   ZEOLITE_PATH=$(zeolite --get-path)
   zeolite -p "$ZEOLITE_PATH" -r lib/thread
   ```

2. Even if the library builds without errors, you should still run the tests to
   ensure that the linker flags are correct.

   ```shell
   zeolite -p "$ZEOLITE_PATH" -t lib/thread
   ```

   If you run into linker errors, you might need to go to step 1 and update the
   `link_flags` in `.zeolite-module`.
