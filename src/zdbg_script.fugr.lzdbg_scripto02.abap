*----------------------------------------------------------------------*
***INCLUDE LZDBG_SCRIPTO02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*MODULE status_9200 OUTPUT.
*  SET PF-STATUS '9200_STD'.
** SET TITLEBAR 'xxx'.
*ENDMODULE.
*
**&---------------------------------------------------------------------*
**& Module SETUP_9200 OUTPUT
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
*MODULE setup_9200 OUTPUT.
*  TRY.
*      IF NOT lcl_screen_state=>get_instance( )->is_alv_constructed( ).
*        lcl_alv_tree_objects=>build_alv( ).
*        lcl_alv_tree_objects=>show_text( ).
*        lcl_screen_state=>get_instance( )->set_alv_constructed( abap_true ).
*      ENDIF.
*    CATCH lcx_screen_error INTO DATA(lo_err).
*      MESSAGE lo_err->get_text( ) TYPE 'E'.
*  ENDTRY.
*  lcl_screen_log=>get_instance( )->render_logs( ).
*ENDMODULE.
