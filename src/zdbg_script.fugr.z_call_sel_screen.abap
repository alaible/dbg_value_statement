FUNCTION z_call_sel_screen.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  EXPORTING
*"     REFERENCE(EV_VARNAME) TYPE  STRING
*"     REFERENCE(EV_DBG) TYPE  ABAP_BOOL
*"  EXCEPTIONS
*"      CANCEL
*"----------------------------------------------------------------------
  CALL SELECTION-SCREEN 1010 STARTING AT 10 10.
  ev_varname = p_var.
  ev_dbg = p_dbg.

  IF sy-subrc <> 0.
    RAISE cancel.
  ENDIF.




ENDFUNCTION.
