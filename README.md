# dbg_value_statement
Debugger Script to generate Value-Statements for arbitrary Structures/Tables.
The scripts needs to be loaded in the Abap-Debugger via Script-Tab.
The name of the script is
```
ZDBG_DISPL_VALUE_STMT
```
Make sure that the script is loaded from database and is executed directly.
You only need to enter the variable name which is then transformed into value-statement.

The function-module 
```
Z_DISPLAY_VALUE_STMT
```
is not limited to be used only while debugging. It can be executed in Reports as well. See report  
```
ZTEST_RTTI
```
for examples.

This tool comes with a tree-explorer to visualize the corresponding object (structure/table).

The Script was developed and test on SAP_BASIS 752.
