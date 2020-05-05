# Template Generation Test

This module tests `--templates` mode for generating `.cpp` templates.

To compile and execute:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
rm -f $ZEOLITE_PATH/tests/templates/Category_Templated.cpp  # Remove the old template.
zeolite -p $ZEOLITE_PATH --templates tests/templates        # Create a new template.
zeolite -p $ZEOLITE_PATH -r tests/templates                 # Recompile.
zeolite -p $ZEOLITE_PATH -t tests/templates                 # Run the test.
```

The most important step above is to delete the *old* template first.
