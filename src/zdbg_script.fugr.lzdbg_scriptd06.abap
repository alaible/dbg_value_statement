*&---------------------------------------------------------------------*
*& Include          LZDBG_SCRIPTD06
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_match_cursor
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_match_cursor DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_match_table,
             node_key        TYPE tm_nodekey,
             node_key_as_int TYPE i,
           END OF t_match_table.
    TYPES: t_cursor_info TYPE c LENGTH 30.
**********************************************************************
    METHODS:
      init_from_ref IMPORTING ir_ref TYPE REF TO zcl_value_entity=>tt_node_search CHANGING c_cursor_info TYPE t_cursor_info,
      set_empty_result CHANGING c_cursor_info TYPE t_cursor_info,
      reset_cursor_info CHANGING c_cursor_info TYPE t_cursor_info,
      go_down CHANGING c_cursor_info TYPE t_cursor_info,
      go_up CHANGING c_cursor_info TYPE t_cursor_info.
**********************************************************************
    DATA: mv_current_node TYPE tm_nodekey READ-ONLY.
  PROTECTED SECTION.
    METHODS:
      update_cursor_info CHANGING c_cursor_info TYPE t_cursor_info,
      cursor_is_valid RETURNING VALUE(rv_valid) TYPE abap_bool.
    DATA: mt_matches   TYPE SORTED TABLE OF t_match_table WITH UNIQUE KEY node_key_as_int,
          mv_tab_index TYPE i.
ENDCLASS.

CLASS lcl_match_cursor IMPLEMENTATION.
  METHOD init_from_ref.
    CLEAR mt_matches.
    IF lines( ir_ref->* ) > 0.
      LOOP AT ir_ref->* ASSIGNING FIELD-SYMBOL(<match>) WHERE highl EQ abap_true OR value EQ abap_true OR nkey EQ abap_true.
        mt_matches = VALUE #( BASE mt_matches ( node_key = <match>-node_key node_key_as_int = CONV #( <match>-node_key ) ) ).
      ENDLOOP.
    ELSE.
      CLEAR mt_matches.
      me->update_cursor_info(
        CHANGING
          c_cursor_info = c_cursor_info
      ).
      RETURN.
    ENDIF.
*    SORT mt_matches BY table_line ASCENDING.
    mv_tab_index = 1.
    mv_current_node = mt_matches[ mv_tab_index ]-node_key.
    me->update_cursor_info(
      CHANGING
        c_cursor_info = c_cursor_info
    ).
  ENDMETHOD.
  METHOD set_empty_result.
    CLEAR: mt_matches, mv_current_node, mv_tab_index.
    c_cursor_info = |no_matches|.
  ENDMETHOD.

  METHOD reset_cursor_info.
    CLEAR: mt_matches, mv_current_node, mv_tab_index, c_cursor_info.
  ENDMETHOD.

  METHOD go_down.
    CHECK me->cursor_is_valid( ) EQ abap_true.
    mv_tab_index = COND #( WHEN mv_tab_index = lines( mt_matches ) THEN mv_tab_index ELSE mv_tab_index + 1 ).
    mv_current_node = mt_matches[ mv_tab_index ]-node_key.
    me->update_cursor_info(
      CHANGING
        c_cursor_info = c_cursor_info
    ).
  ENDMETHOD.
  METHOD go_up.
    CHECK me->cursor_is_valid( ) EQ abap_true.
    mv_tab_index = COND #( WHEN mv_tab_index = 1 THEN mv_tab_index ELSE mv_tab_index - 1 ).
    mv_current_node = mt_matches[ mv_tab_index ]-node_key.
    me->update_cursor_info(
      CHANGING
        c_cursor_info = c_cursor_info
    ).
  ENDMETHOD.
  METHOD cursor_is_valid.
    rv_valid = boolc( mv_tab_index NE 0 AND NOT mv_tab_index GT lines( mt_matches ) ).
  ENDMETHOD.
  METHOD update_cursor_info.
    c_cursor_info = CONV #( |{ mv_tab_index }/{ lines( mt_matches ) }| ).
  ENDMETHOD.
ENDCLASS.
