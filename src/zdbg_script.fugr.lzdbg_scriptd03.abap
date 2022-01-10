*----------------------------------------------------------------------*
***INCLUDE LZDBG_SCRIPTD03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_screen_log
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_screen_log DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_log_level,
             bez     TYPE c LENGTH 10,
             int_val TYPE i,
           END OF ty_log_level.
    TYPES: BEGIN OF ty_log_msg,
             level TYPE ty_log_level,
             msg   TYPE string,
           END OF ty_log_msg.
    TYPES: tt_msg TYPE TABLE OF string WITH EMPTY KEY.
**********************************************************************
    CLASS-DATA:
      lc_log_error TYPE ty_log_level READ-ONLY,
      lc_log_info  TYPE ty_log_level READ-ONLY,
      lc_log_debug TYPE ty_log_level READ-ONLY.
**********************************************************************
    METHODS:
      set_screen_var_base_name IMPORTING iv_base_name TYPE string,
      set_screen_lin_nr_base IMPORTING iv_base_name TYPE string,
      set_log_level IMPORTING iv_log_level TYPE ty_log_level,
      set_log_level_from_key IMPORTING iv_key TYPE i,
      set_log_level_from_name IMPORTING iv_name TYPE string,
      set_max_msg_length IMPORTING i_max_len TYPE i,
      render_logs,
      clear_log_table,
      clear_screen_fields,
      move_to_bottom,
      move_to_top,
      move_up,
      move_down,
      set_visible_rows IMPORTING iv_vis_row TYPE i,
      set_max_log_entries IMPORTING iv_max_entries TYPE i.
    METHODS:
      toggle_log,
      info_t IMPORTING info_tab TYPE tt_msg,
      debug IMPORTING iv_log_msg TYPE string,
      info IMPORTING iv_log_msg TYPE string,
      error IMPORTING iv_log_msg TYPE string,
      add_log IMPORTING iv_log_level TYPE ty_log_level iv_log_msg TYPE string.
    DATA: mv_root_log_level TYPE ty_log_level READ-ONLY,
          mv_max_logentries TYPE i READ-ONLY.
    DATA: mv_show_log TYPE abap_bool VALUE abap_true READ-ONLY.
    CLASS-METHODS:
      class_constructor,
      get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_screen_log.
  PRIVATE SECTION.
    DATA: mv_vis_rows        TYPE i,
          mv_screen_var_base TYPE string,
          mv_line_nr_base    TYPE string.
    DATA: mv_max_msg_len TYPE i.

*** Default to 1
    DATA: mv_current_idx TYPE i VALUE 1.
    DATA: mt_log_table TYPE TABLE OF ty_log_msg.
    CLASS-DATA: lo_instance TYPE REF TO lcl_screen_log.
ENDCLASS.

CLASS lcl_screen_log IMPLEMENTATION.
  METHOD get_instance.
    ro_instance = lo_instance.
  ENDMETHOD.
  METHOD toggle_log.
    mv_show_log = COND #( WHEN mv_show_log EQ abap_true THEN abap_false ELSE abap_true ).
  ENDMETHOD.
  METHOD class_constructor.
    lo_instance = NEW #( ).
*** init log-levels
    lc_log_error = VALUE #( bez = 'ERROR' int_val = 1 ).
    lc_log_info  = VALUE #( bez = 'INFO' int_val = 2 ).
    lc_log_debug = VALUE #( bez = 'DEBUG' int_val = 3 ).
  ENDMETHOD.
  METHOD set_log_level_from_key.
    CASE iv_key.
      WHEN 1.
        mv_root_log_level = lc_log_error.
      WHEN 2.
        mv_root_log_level = lc_log_info.
      WHEN 3.
        mv_root_log_level = lc_log_debug.
    ENDCASE.
  ENDMETHOD.
  METHOD set_log_level_from_name.
    CASE to_upper( iv_name ).
      WHEN 'ERROR'.
        mv_root_log_level = lc_log_error.
      WHEN 'INFO'.
        mv_root_log_level = lc_log_info.
      WHEN 'DEBUG'.
        mv_root_log_level = lc_log_debug.
    ENDCASE.
  ENDMETHOD.
  METHOD set_max_msg_length.
    mv_max_msg_len = i_max_len.
  ENDMETHOD.
  METHOD set_screen_var_base_name.
    me->mv_screen_var_base = iv_base_name.
  ENDMETHOD.
  METHOD set_screen_lin_nr_base.
    me->mv_line_nr_base = iv_base_name.
  ENDMETHOD.
  METHOD set_visible_rows.
    me->mv_vis_rows = iv_vis_row.
  ENDMETHOD.
  METHOD set_max_log_entries.
    me->mv_max_logentries = iv_max_entries.
  ENDMETHOD.
  METHOD set_log_level.
    me->mv_root_log_level = iv_log_level.
  ENDMETHOD.
  METHOD add_log.
    DATA: ls_log LIKE LINE OF mt_log_table.
    IF me->mv_root_log_level-int_val >= iv_log_level-int_val.
      IF mv_max_msg_len IS NOT INITIAL
        AND strlen( iv_log_msg ) > mv_max_msg_len.
        DATA(splitted) = zcl_dbg_utils=>split_string(
                           i_string  = iv_log_msg
                           i_max_len = mv_max_msg_len
                         ).
        DO lines( splitted ) TIMES.
          CASE sy-index.
            WHEN lines( splitted ).
              ls_log = VALUE #( level = iv_log_level msg = |{ sy-uzeit TIME = USER }: { iv_log_level-bez WIDTH = 5 } -- { splitted[ lines( splitted ) - sy-index + 1 ] }| ).
            WHEN OTHERS.
              "({ lines( splitted ) - sy-index + 1 ALIGN = RIGHT WIDTH = 2 pad = space })
              ls_log = VALUE #( level = iv_log_level msg = |{ space WIDTH = 18 PAD = space } { splitted[ lines( splitted ) - sy-index + 1 ] }| ).
          ENDCASE.
        INSERT ls_log INTO mt_log_table INDEX 1.
        ENDDO.
      ELSE.
        ls_log = VALUE #( level = iv_log_level msg = |{ sy-uzeit TIME = USER }: { iv_log_level-bez WIDTH = 5 } -- { iv_log_msg }| ).
        INSERT ls_log INTO mt_log_table INDEX 1.
      ENDIF.
    ENDIF.
**********************************************************************
*** handle max log entries
    IF lines( mt_log_table ) > mv_max_logentries.
      DO lines( mt_log_table ) - mv_max_logentries TIMES.
        DELETE mt_log_table INDEX mv_max_logentries + sy-index.
      ENDDO.
    ENDIF.
  ENDMETHOD.
  METHOD move_down.
    mv_current_idx = COND #( WHEN mv_current_idx = lines( mt_log_table ) THEN mv_current_idx ELSE mv_current_idx + 1 ).
  ENDMETHOD.
  METHOD move_up.
    mv_current_idx = COND #( WHEN mv_current_idx = 1 THEN 1 ELSE mv_current_idx - 1 ).
  ENDMETHOD.
  METHOD move_to_bottom.
    mv_current_idx = lines( mt_log_table ).
  ENDMETHOD.
  METHOD move_to_top.
    mv_current_idx = 1.
  ENDMETHOD.
  METHOD render_logs.
    FIELD-SYMBOLS: <log_field> TYPE clike,
                   <log>       LIKE LINE OF mt_log_table.
    DATA: lv_fieldname TYPE string.
    DATA: lv_cntr TYPE i VALUE 0.
**********************************************************************
    me->clear_screen_fields( ).
    DO mv_vis_rows TIMES.
      IF lv_cntr > me->mv_vis_rows. EXIT. ENDIF.
      READ TABLE mt_log_table INDEX mv_current_idx + lv_cntr ASSIGNING <log>.
      CHECK sy-subrc = 0.
      lv_fieldname = to_upper( |{ me->mv_screen_var_base }{ lv_cntr + 1 }| ).
      ASSIGN (lv_fieldname) TO <log_field>.
      CHECK sy-subrc = 0.
      <log_field> = <log>-msg.
      lv_fieldname = to_upper( |{ me->mv_line_nr_base }{ lv_cntr + 1 }| ).
      ASSIGN (lv_fieldname) TO <log_field>.
      CHECK sy-subrc = 0.
      <log_field> = |{ lines( me->mt_log_table ) - mv_current_idx - lv_cntr + 1 }/{ lines( me->mt_log_table ) }|.
      lv_cntr = lv_cntr + 1.
    ENDDO.
  ENDMETHOD.
  METHOD clear_screen_fields.
    DATA: lv_fieldname TYPE string.
    FIELD-SYMBOLS: <log_field> TYPE any.
    DO me->mv_vis_rows TIMES.
      lv_fieldname = to_upper( |{ me->mv_screen_var_base }{ sy-index }| ).
      ASSIGN (lv_fieldname) TO <log_field>.
      CHECK sy-subrc = 0.
      <log_field> = ''.
    ENDDO.
    DO me->mv_vis_rows TIMES.
      lv_fieldname = to_upper( |{ me->mv_line_nr_base }{ sy-index }| ).
      ASSIGN (lv_fieldname) TO <log_field>.
      CHECK sy-subrc = 0.
      <log_field> = ''.
    ENDDO.
  ENDMETHOD.
  METHOD clear_log_table.
    CLEAR me->mt_log_table.
    mv_current_idx = 1.
  ENDMETHOD.
  METHOD debug.
    me->add_log( iv_log_level = lc_log_debug iv_log_msg = iv_log_msg ).
  ENDMETHOD.
  METHOD info_t.
    LOOP AT info_tab ASSIGNING FIELD-SYMBOL(<info>).
      me->info( <info> ).
    ENDLOOP.
  ENDMETHOD.
  METHOD info.
    me->add_log( iv_log_level = lc_log_info iv_log_msg = iv_log_msg ).
  ENDMETHOD.
  METHOD error.
    me->add_log( iv_log_level = lc_log_error iv_log_msg = iv_log_msg ).
  ENDMETHOD.
ENDCLASS.
