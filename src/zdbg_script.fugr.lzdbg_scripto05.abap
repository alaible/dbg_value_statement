*----------------------------------------------------------------------*
***INCLUDE LZDBG_SCRIPTO05.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module SETUP_8000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE setup_8000 OUTPUT.
  SET PF-STATUS space.
*  SET PF-STATUS '8000_STD'.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  LOOP AT go_root_ent->get_content( ) ASSIGNING FIELD-SYMBOL(<cont>).
    WRITE:/ <cont>.
  ENDLOOP.
*  SUPPRESS DIALOG.
*  LEAVE SCREEN.
ENDMODULE.
