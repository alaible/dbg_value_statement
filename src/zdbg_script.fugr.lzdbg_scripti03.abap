*----------------------------------------------------------------------*
***INCLUDE LZDBG_SCRIPTI03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9300 INPUT.
*  BREAK-POINT.
  CASE ok_code.
*    WHEN 'TOGGLE_EDIT'.
*      lcl_alv_tree_objects=>toggle_edit( ).
    WHEN space.
      CASE abap_true.
        WHEN flag_filter.
          TRY.
              IF gv_searchfield IS INITIAL.
                go_screen_objects_col->reset_tree( CHANGING c_curs_info = cursor_info ).
              ELSE.
                go_screen_objects_col->handle_item_search_fil(
                    EXPORTING iv_search_term = CONV #( to_lower( gv_searchfield ) ) iv_search_val = flg_search_value
                    CHANGING c_curs_info = cursor_info ).
              ENDIF.
            CATCH lcx_tree_error zcx_tree_err INTO go_err.
              MESSAGE |Fehler: { go_err->get_text( ) }| TYPE 'I'.
          ENDTRY.
        WHEN OTHERS.
          TRY.
              IF gv_searchfield IS INITIAL.
                go_screen_objects_col->reset_tree( CHANGING c_curs_info = cursor_info ).
              ELSE.
                go_screen_objects_col->handle_item_search(
                      EXPORTING iv_search_term = CONV #( to_lower( gv_searchfield ) ) iv_search_val = flg_search_value
                      CHANGING c_curs_info = cursor_info ).
              ENDIF.
            CATCH lcx_tree_error zcx_tree_err INTO go_err.
              MESSAGE |Fehler: { go_err->get_text( ) }| TYPE 'I'.
          ENDTRY.
      ENDCASE.
    WHEN 'LOG_UP'.
      lcl_screen_log=>get_instance( )->move_up( ).
    WHEN 'LOG_DOWN'.
      lcl_screen_log=>get_instance( )->move_down( ).
    WHEN 'LOG_FIRST'.
      lcl_screen_log=>get_instance( )->move_to_top( ).
    WHEN 'LOG_LAST'.
      lcl_screen_log=>get_instance( )->move_to_bottom( ).
    WHEN 'CRS_DOWN'.
      TRY.
          go_screen_objects_col->move_cursor_down( CHANGING c_curs_info = cursor_info ).
        CATCH lcx_tree_error.
      ENDTRY.
    WHEN 'CRS_UP'.
      TRY.
          go_screen_objects_col->move_cursor_up( CHANGING c_curs_info = cursor_info ).
        CATCH lcx_tree_error.
      ENDTRY.
    WHEN 'LOG_CNF'.
      CLEAR: ok_9400.
      log_config_dd_handler->set_loglevel( lcl_screen_log=>get_instance( )->mv_root_log_level-bez ).
      max_lines_log = lcl_screen_log=>get_instance( )->mv_max_logentries.
      CALL SCREEN 9400 STARTING AT 70 5.
      IF ok_9400 = 'OK'.
        TRY.
            lcl_screen_log=>get_instance( )->set_max_log_entries( max_lines_log ).
            lcl_screen_log=>get_instance( )->set_log_level_from_name( log_config_dd_handler->get_loglevel( ) ).
            lcl_screen_log=>get_instance( )->info( |New Config: Level { lcl_screen_log=>get_instance( )->mv_root_log_level-bez }| &&
                                                        | -- Max Log entries: { lcl_screen_log=>get_instance( )->mv_max_logentries }| ).
          CATCH lcx_no_value_set.
        ENDTRY.
      ENDIF.
    WHEN 'TGL_LOG'.
      lcl_screen_log=>get_instance( )->toggle_log( ).
    WHEN 'REFR_LS'.
      TRY.
          go_screen_objects_col->reload_line_size( i_line_size = max_linesize ).
        CATCH lcx_screen_error.
      ENDTRY.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CLEAR_OK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear_ok INPUT.
  CLEAR ok_code.
ENDMODULE.
