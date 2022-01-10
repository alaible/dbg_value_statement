*----------------------------------------------------------------------*
***INCLUDE LZDBG_SCRIPTO03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module SETUP_9300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE setup_9300 OUTPUT.
  TRY.
      IF go_screen_state->mv_grid_inital_rendered EQ abap_false.
        go_screen_objects_col->build_alv( ).
        go_screen_objects_col->show_text( ).
        go_screen_state->set_grid_rendered( abap_true ).
*        go_screen_objects_col->reset_text_edit( ).
      ENDIF.
    CATCH lcx_screen_error INTO go_err.
      MESSAGE go_err->get_text( ) TYPE 'E' DISPLAY LIKE 'I'.
    CATCH zcx_screen_err INTO go_err.
      MESSAGE go_err->get_text( ) TYPE 'E' DISPLAY LIKE 'I'.
  ENDTRY.
  IF lcl_screen_log=>get_instance( )->mv_show_log EQ abap_false.
    LOOP AT SCREEN INTO DATA(wa_screen).
      IF wa_screen-group1 EQ 'LOG'.
        wa_screen-invisible = 1.
        wa_screen-active = 0.
        MODIFY SCREEN FROM wa_screen.
      ENDIF.
    ENDLOOP.
  ELSE.
    lcl_screen_log=>get_instance( )->render_logs( ).
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9300 OUTPUT.
 SET PF-STATUS '9300_STD'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
