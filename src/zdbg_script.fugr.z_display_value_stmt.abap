FUNCTION z_display_value_stmt.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(IR_ENTITY) TYPE REF TO  ZCL_COMPL_ENTITY
*"     REFERENCE(I_MAX_LINE_LENGTH) TYPE  INT2 DEFAULT 256
*"  EXCEPTIONS
*"      SCREEN_ERROR
*"----------------------------------------------------------------------
  DATA: lt_c_tab TYPE lcl_screen_objects_col=>t_c_text.
**********************************************************************
  CLEAR: ok_code, ok_9400, flg_search_value, cursor_info, gv_searchfield, max_linesize.
**********************************************************************
*** Default Values
  txt_info_1 = 'value-stmnt-generator v.0.9'.
  txt_info_2 = |{ sy-datum DATE = USER } -- { sy-uzeit TIME = USER } -- { sy-title }|.
  flag_filter = abap_true.
  flg_search_value = abap_true.
  max_linesize = i_max_line_length.
**********************************************************************
  log_config_dd_handler = NEW #( ).
  log_config_dd_handler->set_dd_loglevl( VALUE #( name = 'LOG_LEVEL' ref_dd_f = REF #( log_level ) ) ).
  TRY.
      log_config_dd_handler->set_values( ).
    CATCH lcx_vrm_value.
  ENDTRY.
**********************************************************************
  lcl_screen_log=>get_instance( )->set_screen_var_base_name( |txt_log_| ).
  lcl_screen_log=>get_instance( )->set_screen_lin_nr_base( |log_lnr_| ).
  lcl_screen_log=>get_instance( )->set_visible_rows( 4 ).
  lcl_screen_log=>get_instance( )->set_log_level( lcl_screen_log=>lc_log_info ).
  lcl_screen_log=>get_instance( )->set_max_log_entries( 999 ).
  lcl_screen_log=>get_instance( )->set_max_msg_length( 59 ).
*** Info if Refrence is contained in Entity
*  lcl_screen_log=>get_instance( )->info( 'Test' ).
  lcl_screen_log=>get_instance( )->info( 'Starting Value-Stmnt-Generator v.0.9' ).
  lcl_screen_log=>get_instance( )->info( 'Node-Key Search-Pattern: nkey=[[:digit:]]' ).
  IF ir_entity->contains_reference( ) EQ abap_true.
    lcl_screen_log=>get_instance( )->info( 'Entity contains references!' ).
  ENDIF.
**********************************************************************
*  lcl_screen_state=>force_init( ).
  go_screen_state = NEW #( ).

*** TODO: Zeilenlänge beachten!
  LOOP AT ir_entity->get_content( i_max_line_length = CONV #( i_max_line_length ) ) ASSIGNING FIELD-SYMBOL(<line>).
    APPEND <line> TO lt_c_tab.
  ENDLOOP.

*** Referenz für Basis-Pfad übergeben
  IF go_screen_objects_col IS NOT BOUND. go_screen_objects_col = NEW #( ). ENDIF.
  go_screen_objects_col->set_root_ref( ir_entity ).
  go_screen_objects_col->set_ref_str_tab( REF #( lt_c_tab ) ).
  go_screen_objects_col->set_max_line_len( REF #( max_linesize ) ).
  go_screen_objects_col->set_screen_state( go_screen_state ).
  go_screen_objects_col->set_screen_log( lcl_screen_log=>get_instance( ) ).

  CALL SCREEN 9300 STARTING AT 5 5.

*** Cleanup!
*** -> Delete all nodes of cl_column_tree_model
  TRY.
      go_screen_objects_col->delete_all_nodes( ).
      lcl_screen_log=>get_instance( )->clear_log_table( ).
      lcl_screen_log=>get_instance( )->clear_screen_fields( ).
    CATCH lcx_screen_error INTO DATA(lo_err).
      RAISE screen_error.
  ENDTRY.
ENDFUNCTION.
