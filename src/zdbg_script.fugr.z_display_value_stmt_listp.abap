FUNCTION z_display_value_stmt_listp.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(I_ENT) TYPE REF TO  ZCL_COMPL_ENTITY
*"----------------------------------------------------------------------

  go_root_ent = i_ent.

  CALL SCREEN 8000 STARTING AT 5 5 ENDING AT 150 30.

ENDFUNCTION.
