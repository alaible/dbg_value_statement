*&---------------------------------------------------------------------*
*& Include          LZDBG_SCRIPTD05
*&---------------------------------------------------------------------*
CLASS ltc_column_tree_test DEFINITION DEFERRED.
*&---------------------------------------------------------------------*
*& Class lcl_screen_objects_col
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_screen_objects_col DEFINITION INHERITING FROM lcl_screen_base FRIENDS ltc_column_tree_test.
  PUBLIC SECTION.
**********************************************************************
*** Hilfstypen
    TYPES: ref_char_tab TYPE REF TO t_c_text.
**********************************************************************
    METHODS:
      constructor,
      set_root_ref IMPORTING io_root TYPE REF TO zcl_compl_entity,
      set_ref_str_tab IMPORTING ir_ref TYPE ref_char_tab,
      set_screen_state IMPORTING io_screen_state TYPE REF TO lcl_screen_state,
      set_screen_log IMPORTING io_screen_log TYPE REF TO lcl_screen_log,
      set_max_line_len IMPORTING i_max_line_len TYPE REF TO i,
      delete_all_nodes,
      reset_tree CHANGING c_curs_info TYPE lcl_match_cursor=>t_cursor_info RAISING zcx_tree_err,
      toggle_edit RAISING lcx_screen_error,
      show_text RAISING lcx_screen_error,
      reload_line_size IMPORTING i_line_size TYPE i RAISING lcx_screen_error,
      build_alv RAISING lcx_screen_error zcx_screen_err zcx_tree_err.
**********************************************************************
    METHODS:
      handle_item_search IMPORTING iv_search_term TYPE string
                                   iv_search_val  TYPE abap_bool
                         CHANGING  c_curs_info    TYPE lcl_match_cursor=>t_cursor_info
                         RAISING   zcx_tree_err,
      handle_item_search_fil IMPORTING iv_search_term TYPE string
                                       iv_search_val  TYPE abap_bool
                             CHANGING  c_curs_info    TYPE lcl_match_cursor=>t_cursor_info
                             RAISING   zcx_tree_err,
      move_cursor_down CHANGING c_curs_info TYPE lcl_match_cursor=>t_cursor_info RAISING zcx_tree_err,
      move_cursor_up CHANGING c_curs_info TYPE lcl_match_cursor=>t_cursor_info RAISING zcx_tree_err.
  PROTECTED SECTION.
*    DATA: mo_alv_tree         TYPE REF TO cl_column_tree_model.
    DATA: mr_node_table_tmc    TYPE REF TO zcl_value_entity=>tt_node_table_tmc,
          mr_search_index      TYPE REF TO zcl_value_entity=>tt_search_index,
          mr_search_index_cont TYPE REF TO zcl_value_entity=>tt_search_index_cont.
**********************************************************************
    DATA: mr_matches      TYPE REF TO zcl_value_entity=>tt_node_search,
          mr_curr_nodekey TYPE REF TO tm_nodekey.
    DATA: mo_match_cursor TYPE REF TO lcl_match_cursor,
          mo_screen_state TYPE REF TO lcl_screen_state,
          mo_screen_log   TYPE REF TO lcl_screen_log.
**********************************************************************
*** Drag and Drop-Objekte
    DATA: mv_handle_tree TYPE i,
          mv_handle_file TYPE i.
    DATA: mo_root_ent TYPE REF TO zcl_compl_entity.
*** Flag für initialisierung
    DATA: mv_initialized TYPE abap_bool.
    DATA: mr_ref_str_tab     TYPE ref_char_tab,
          mr_max_line_length TYPE REF TO i,
          mv_root_node_key   TYPE string,
          mv_filtered        TYPE abap_bool.
    DATA: mv_selected_node_key TYPE tm_nodekey,
          mr_ref_node_table    TYPE REF TO zcl_value_entity=>tty_node_table.
**********************************************************************
    METHODS:
      register_events REDEFINITION,
      register_events_toolb REDEFINITION,
      get_hierarchy_hdr REDEFINITION,
      tree_add_colums REDEFINITION,
      add_toolb_buttons REDEFINITION.
    METHODS:
      get_container_name REDEFINITION,
      get_left_splter_width REDEFINITION.
**********************************************************************
    METHODS:
      add_root_node_to_tree IMPORTING iv_set_selected_nkey TYPE abap_bool RAISING zcx_tree_err,
      init_tree_tables.
    METHODS:
      save_text_content RAISING lcx_screen_error zcx_tree_err,
      perform_search IMPORTING iv_search_term TYPE string iv_search_value TYPE abap_bool,
      perform_search_nk IMPORTING iv_search_term TYPE string,
      get_num_matches RETURNING VALUE(rv_num) TYPE i,
      build_filtered_tree RAISING zcx_tree_err,
      find_all_parent_nodes
        IMPORTING
          iv_node_key TYPE zcl_value_entity=>t_node_table_tmc-node_key
          ir_matches  TYPE zcl_value_entity=>tr_node_search,
      expand_matches RAISING zcx_tree_err,
      set_styles_from_matches RAISING zcx_tree_err,
      highlight_ref_elements RAISING zcx_tree_err,
      disable_child_elemnts IMPORTING iv_parent_node_key TYPE tm_nodekey RAISING zcx_tree_err,
      reset_visible_entries,
      mark_disabled_nodes RAISING zcx_tree_err,
      mark_modified_nodes RAISING zcx_tree_err.
**********************************************************************
    METHODS:
      find_all_child_elements IMPORTING iv_nodekey TYPE tm_nodekey ir_child_nodes TYPE REF TO tt_nodekey,
      find_all_parent_elements IMPORTING iv_nodekey TYPE tm_nodekey ir_parent_nodes TYPE REF TO tt_nodekey,
      find_subtree IMPORTING ir_child_nodes TYPE REF TO tt_nodekey RAISING zcx_tree_err,
      reset_styles RAISING zcx_tree_err,
      expand_sub_tree RAISING zcx_tree_err,
      collapse_sub_tree RAISING zcx_tree_err.
**********************************************************************
*** Event-Handler
    METHODS:
      handle_node_double_click FOR EVENT node_double_click OF cl_column_tree_model IMPORTING node_key,
      handle_item_double_click FOR EVENT item_double_click OF cl_column_tree_model IMPORTING node_key.
*** Toolbar!
    METHODS:
      on_function_selected FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode.
**********************************************************************
*** konstante für custom-container
    CLASS-DATA: lc_control_name TYPE string VALUE 'CUST_CONTROL'.
    CLASS-METHODS:
      str_tab_as_flat_str IMPORTING it_str_tab TYPE zcl_value_entity=>tty_string RETURNING VALUE(rv_flatstring) TYPE string.
ENDCLASS.


CLASS lcl_screen_objects_col IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_match_cursor = NEW #( ).
  ENDMETHOD.
**********************************************************************
  METHOD get_hierarchy_hdr.
    rs_hdr = VALUE #( hierarchy_column_name = 'NODE_NAME'
                      hierarchy_header      = VALUE #( t_image = icon_folder
                                                       heading = 'Element-Name'
                                                       tooltip = 'Element-Name'
                                                       width   = 30 )
                     ).      " Name der Spalte im Hierarchie-Bereich

  ENDMETHOD.
  METHOD tree_add_colums.
    mo_alv_tree->add_column( name = 'NODE_TYPE' width = 15 header_text = 'NODE_TYPE' ).
    mo_alv_tree->add_column( name = 'VALUE' width = 15 header_text = 'Value' ).
    mo_alv_tree->add_column( name = 'CONTAINS_REF' width = 10 header_text = 'Contains Ref' ).
    mo_alv_tree->add_column( name = 'NODE_KEY' width = 10 header_text = 'NODE_KEY' ).
  ENDMETHOD.
  METHOD get_container_name.
    rv_name = lc_control_name.
  ENDMETHOD.
  METHOD get_left_splter_width.
    rv_left_width = 35.
  ENDMETHOD.
  METHOD add_toolb_buttons.
    mo_toolbar->add_button( fcode     = 'EXPAND_N'
                            butn_type = cntb_btype_button
                            icon      = icon_expand_all ).
    mo_toolbar->add_button( fcode     = 'COLLPASE_N'
                            butn_type = cntb_btype_button
                            icon      = icon_collapse_all ).
    mo_toolbar->add_button( fcode     = ''
                            butn_type = cntb_btype_sep
                            icon      = '' ).
    mo_toolbar->add_button( fcode     = 'PRINT'
                            butn_type = cntb_btype_button
                            icon      = icon_print ).
    mo_toolbar->add_button( fcode     = ''
                            butn_type = cntb_btype_sep
                            icon      = '' ).
    mo_toolbar->add_button( fcode     = 'TOGGLE'
                            butn_type = cntb_btype_button
                            icon      = icon_toggle_display_change ).
    mo_toolbar->add_button( fcode     = 'SAVE'
                            butn_type = cntb_btype_button
                            icon      = icon_system_save ).
  ENDMETHOD.
  METHOD register_events.
    DATA: lt_events TYPE cntl_simple_events.
**********************************************************************
    mo_alv_tree->get_registered_events( IMPORTING events = lt_events ).
*** Tabelle Erweitern
    APPEND VALUE #( eventid = cl_column_tree_model=>eventid_item_double_click appl_event = abap_true ) TO lt_events.
    APPEND VALUE #( eventid = cl_column_tree_model=>eventid_node_double_click appl_event = abap_true ) TO lt_events.
    TRY.
        me->set_registered_evts( lt_events ).
      CATCH lcx_ctrl_error INTO DATA(lo_err).
        MESSAGE lo_err->get_text( ) TYPE 'E'.
    ENDTRY.
*    CATCH lcx_tree_error.
*** Handler Registrieren
    SET HANDLER me->handle_node_double_click FOR mo_alv_tree.
    SET HANDLER me->handle_item_double_click FOR mo_alv_tree.
  ENDMETHOD.
  METHOD register_events_toolb.
    TYPES: t_events TYPE STANDARD TABLE OF cntl_simple_event WITH DEFAULT KEY.
    DATA: lt_events TYPE t_events.
**********************************************************************
    lt_events = VALUE #( ( eventid = cl_gui_toolbar=>m_id_function_selected
                           appl_event = abap_true ) ).
    mo_toolbar->set_registered_events( lt_events ).
    SET HANDLER me->on_function_selected FOR mo_toolbar.
  ENDMETHOD.
**********************************************************************
  METHOD build_alv.
*** Falls der FB im gleichen internen Modus mehrfach aufgerufen wird
*** -> Initialisierung der Container/Screen-Objete darf zu einmal passieren!
    IF mv_initialized EQ abap_false.
      me->init_container( ).
      me->init_splitter( ).
      me->init_splitter_left( ).
      me->init_toolbar( ).
      me->init_alv_tree_col( ).
    ENDIF.
*** In der Method add_root_node_to_tree wird die Interne Tabelle für den alv_tree aufgebaut
    add_root_node_to_tree( abap_true ).
  ENDMETHOD.
  METHOD set_root_ref.
    mo_root_ent = io_root.
  ENDMETHOD.
  METHOD set_screen_state.
    mo_screen_state = io_screen_state.
  ENDMETHOD.
  METHOD set_screen_log.
    mo_screen_log = io_screen_log.
  ENDMETHOD.
  METHOD set_max_line_len.
    mr_max_line_length = i_max_line_len.
  ENDMETHOD.
  METHOD init_tree_tables.
    IF mr_node_table_tmc IS BOUND. FREE mr_node_table_tmc. ENDIF.
**********************************************************************
    mr_node_table_tmc = NEW #( ).
    mr_search_index = NEW #( ).
    mr_search_index_cont = NEW #( ).
    mr_nodetab_int = NEW #( ).
    mr_itemtab_int = NEW #( ).
    mr_curr_nodekey = NEW #( ).
*********************************************************************
    mo_root_ent->build_node_table(
      EXPORTING
        ir_node_tab    = mr_nodetab_int
        ir_item_tab    = mr_itemtab_int
        iv_parent_key  = space
        ir_current_key = mr_curr_nodekey
        iv_dd_id       = CONV #( mv_handle_tree )
        ir_node_table  = mr_node_table_tmc
        ir_search_index = mr_search_index
        ir_search_index_cont = mr_search_index_cont
    ).
*    BREAK-POINT.
**********************************************************************
  ENDMETHOD.
  METHOD add_root_node_to_tree.
    DATA: mr_message TYPE REF TO zcl_value_entity=>tty_string.
    me->init_tree_tables( ).
**********************************************************************
    me->add_nodes( ).
    me->add_items( ).
*** check out of bounds
    CHECK lines( mr_nodetab_int->* ) GT 0.
    mv_root_node_key = mo_root_ent->mv_alv_tree_node_tmc.
    IF iv_set_selected_nkey EQ abap_true.
      mv_selected_node_key = mv_root_node_key.
    ENDIF.
    me->highlight_ref_elements( ).
*** Expand Root-Node
    me->expand_node( mr_nodetab_int->*[ 1 ]-node_key ).
*** Collect Info Messages
    mr_message = NEW #( ).
    mo_root_ent->collect_messages( mr_message ).
    mo_screen_log->info_t( mr_message->* ).
  ENDMETHOD.
  METHOD delete_all_nodes.
    mo_alv_tree->delete_all_nodes( ).
  ENDMETHOD.
  METHOD reset_tree.
    mv_filtered = abap_false.
    me->delete_all_nodes( ).
    me->add_root_node_to_tree( abap_true ).
    me->mark_disabled_nodes( ).
    me->mark_modified_nodes( ).
    mo_match_cursor->reset_cursor_info(
      CHANGING
        c_cursor_info = c_curs_info
    ).
    mo_screen_log->info( |Resetting tree finished| ).
  ENDMETHOD.
  METHOD on_function_selected.
    DATA: lo_err TYPE REF TO cx_static_check.
    CASE fcode.
      WHEN 'EXPAND_N'.
        TRY.
            me->expand_sub_tree( ).
          CATCH zcx_tree_err INTO lo_err.
            MESSAGE lo_err->get_text( ) TYPE 'I'.
        ENDTRY.
      WHEN 'COLLPASE_N'.
        TRY.
            me->collapse_sub_tree( ).
          CATCH zcx_tree_err INTO lo_err.
            MESSAGE lo_err->get_text( ) TYPE 'I'.
        ENDTRY.
      WHEN 'PRINT'.
      WHEN 'TOGGLE'.
        TRY.
            me->toggle_edit( ).
          CATCH lcx_tree_error lcx_screen_error INTO lo_err.
            MESSAGE lo_err->get_text( ) TYPE 'I'.
        ENDTRY.
      WHEN 'SAVE'.
        TRY.
            me->save_text_content( ).
          CATCH lcx_screen_error zcx_tree_err INTO lo_err.
            MESSAGE lo_err->get_text( ) TYPE 'I'.
        ENDTRY.
    ENDCASE.
  ENDMETHOD.
  METHOD reset_styles.
    LOOP AT mr_node_table_tmc->* ASSIGNING FIELD-SYMBOL(<node_entry>) WHERE highlighted EQ abap_true.
      me->item_set_style(
        EXPORTING
          iv_node_key  = <node_entry>-node_key   " Schlüssel des Knotens
          iv_item_name = 'NODE_NAME'     " Name des Items
          iv_style     = cl_column_tree_model=>style_default " siehe Methodendokumentation
      ).
      IF <node_entry>-is_complex EQ abap_true. CONTINUE. ENDIF.
      me->item_set_style(
        EXPORTING
          iv_node_key  = <node_entry>-node_key   " Schlüssel des Knotens
          iv_item_name = 'VALUE'     " Name des Items
          iv_style     = cl_column_tree_model=>style_default " siehe Methodendokumentation
      ).
    ENDLOOP.
  ENDMETHOD.
  METHOD find_all_parent_nodes.
    DATA: lv_parent_node_key TYPE zcl_value_entity=>t_node_table_tmc-node_key.
    FIELD-SYMBOLS: <node_table> TYPE zcl_value_entity=>tt_node_table_tmc,
                   <node_entry> TYPE zcl_value_entity=>t_node_table_tmc.
    FIELD-SYMBOLS: <matches> TYPE zcl_value_entity=>tt_node_search,
                   <match>   TYPE zcl_value_entity=>t_node_search.
**********************************************************************
    ASSIGN me->mr_node_table_tmc->* TO <node_table>.
    ASSIGN ir_matches->* TO <matches>.
**********************************************************************
    READ TABLE <node_table> WITH KEY node_key = iv_node_key ASSIGNING <node_entry>.
*** Loop until root node is reached (parent_node_key = space)
    WHILE <node_entry>-parent_node_key NE space.
      lv_parent_node_key = <node_entry>-parent_node_key.
*** Achtung: <matches> ist hashed table
      IF line_exists( <matches>[ node_key = lv_parent_node_key ] ).
        READ TABLE <matches> WITH KEY node_key = lv_parent_node_key ASSIGNING <match>.
        <match>-parent = abap_true.
      ELSE.
        INSERT VALUE #( node_key = lv_parent_node_key parent = abap_true ) INTO TABLE <matches>.
      ENDIF.
      READ TABLE <node_table> WITH KEY node_key = lv_parent_node_key ASSIGNING <node_entry>.
    ENDWHILE.
  ENDMETHOD.
  METHOD handle_item_search.
**********************************************************************
    DATA: lt_item_layout TYPE lvc_t_laci,
          lt_check_nodes TYPE TABLE OF tm_nodekey.
    FIELD-SYMBOLS: <search_index>      TYPE zcl_value_entity=>tt_search_index,
                   <search_index_cont> TYPE zcl_value_entity=>tt_search_index_cont,
                   <node_tab_line>     TYPE zcl_value_entity=>t_node_table_tmc.
    FIELD-SYMBOLS: <search_matches> TYPE zcl_value_entity=>tt_node_search,
                   <match>          TYPE zcl_value_entity=>t_node_search.
**********************************************************************
*** if tree was previously filtered via handle_file_search_fil,
*** the tree is rebuilt so that all elements are added and therefore
*** are available for highlighting
    IF mv_filtered = abap_true.
      me->delete_all_nodes( ).
      me->add_root_node_to_tree( abap_true ).
      mv_filtered = abap_false.
    ENDIF.
*** Initial Styles zurücksetzen!
    me->reset_styles( ).
*    CATCH lcx_screen_error.
    mo_alv_tree->collapse_all_nodes( ).
    me->expand_node( mr_nodetab_int->*[ 1 ]-node_key ).
**********************************************************************
    IF mr_matches IS NOT BOUND. mr_matches = NEW #( ). ENDIF.
    ASSIGN mr_matches->* TO <search_matches>.
    CLEAR <search_matches>.
**********************************************************************
    me->perform_search(
      EXPORTING
        iv_search_term  = iv_search_term
        iv_search_value = iv_search_val
    ).
**********************************************************************
    lt_check_nodes = VALUE #( FOR match IN mr_matches->* ( match-node_key ) ).
    LOOP AT lt_check_nodes ASSIGNING FIELD-SYMBOL(<_node>).
      me->find_all_parent_nodes( iv_node_key = <_node> ir_matches = mr_matches ).
*      me->find_all_child_nodes( iv_node_key = <_node> ir_matches = mr_matches ).
    ENDLOOP.
    me->set_styles_from_matches( ).
    me->expand_matches( ).
    me->mark_disabled_nodes( ).
    me->mark_modified_nodes( ).
**********************************************************************
    IF lines( mr_matches->* ) GE 1.
      mo_match_cursor->init_from_ref( EXPORTING ir_ref = mr_matches CHANGING c_cursor_info = c_curs_info ).
      mo_alv_tree->set_selected_node( mo_match_cursor->mv_current_node ).
      mo_screen_log->info( |Found { me->get_num_matches( ) } entries for searchterm: { iv_search_term }| ).
    ELSE.
      mo_match_cursor->set_empty_result( CHANGING c_cursor_info = c_curs_info ).
      mo_screen_log->info( |Found no entries for searchterm: { iv_search_term }| ).
    ENDIF.
**********************************************************************
    cl_gui_cfw=>flush( ).
  ENDMETHOD.
  METHOD handle_item_search_fil.
    FIELD-SYMBOLS: <search_index>      TYPE zcl_value_entity=>tt_search_index,
                   <search_index_cont> TYPE zcl_value_entity=>tt_search_index_cont,
                   <node_tab_line>     TYPE zcl_value_entity=>t_node_table_tmc.
    FIELD-SYMBOLS: <search_matches> TYPE zcl_value_entity=>tt_node_search,
                   <match>          TYPE zcl_value_entity=>t_node_search.
    DATA: lt_item_layout TYPE lvc_t_laci.
**********************************************************************
    DATA: lt_check_nodes TYPE TABLE OF tm_nodekey.
**** Initial Styles zurücksetzen!
*    me->reset_styles( ).
**    CATCH lcx_screen_error.
*    mo_alv_tree->collapse_all_nodes( ).
*    mo_alv_tree->expand_node( node_key = mr_nodetab_int->*[ 1 ]-node_key ).
**********************************************************************
    IF mr_matches IS NOT BOUND. mr_matches = NEW #( ). ENDIF.
    ASSIGN mr_matches->* TO <search_matches>.
    CLEAR <search_matches>.
**********************************************************************
    me->perform_search(
      EXPORTING
        iv_search_term  = iv_search_term
        iv_search_value = iv_search_val
    ).
**********************************************************************
    lt_check_nodes = VALUE #( FOR match IN mr_matches->* ( match-node_key ) ).
    LOOP AT lt_check_nodes ASSIGNING FIELD-SYMBOL(<_node>).
      me->find_all_parent_nodes( iv_node_key = <_node> ir_matches = mr_matches ).
*      me->find_all_child_nodes( iv_node_key = <_node> ir_matches = mr_matches ).
    ENDLOOP.
    me->delete_all_nodes( ).
    me->reset_visible_entries( ).
    me->build_filtered_tree( ).
*    CATCH lcx_tree_error.
**********************************************************************
    IF lines( mr_matches->* ) GE 1.
      mo_match_cursor->init_from_ref( EXPORTING ir_ref = mr_matches CHANGING c_cursor_info = c_curs_info ).
      mo_alv_tree->set_selected_node( mo_match_cursor->mv_current_node ).
      mo_screen_log->info( |Found { me->get_num_matches( ) } entries for searchterm: { iv_search_term }| ).
    ELSE.
      mo_screen_log->info( |Found no entries for searchterm: { iv_search_term }| ).
      mo_match_cursor->set_empty_result( CHANGING c_cursor_info = c_curs_info ).
    ENDIF.
**********************************************************************
*** Filter-Flag auf abap_true
    mv_filtered = abap_true.
**********************************************************************
    me->mark_disabled_nodes( ).
    me->mark_modified_nodes( ).
**********************************************************************
    cl_gui_cfw=>flush( ).
  ENDMETHOD.
  METHOD build_filtered_tree.
    FREE: mr_nodetab_int, mr_itemtab_int.
    mr_nodetab_int = NEW #( ).
    mr_itemtab_int = NEW #( ).
    mo_root_ent->build_node_table_fil(
      EXPORTING
        ir_node_tab     = mr_nodetab_int
        ir_item_tab     = mr_itemtab_int
        ir_matches      = mr_matches
        ir_node_table   = mr_node_table_tmc
    ).
    me->add_nodes( ).
    me->add_items( ).
    me->expand_matches( ).
  ENDMETHOD.
  METHOD perform_search.
    FIELD-SYMBOLS: <search_index>      TYPE zcl_value_entity=>tt_search_index,
                   <search_index_cont> TYPE zcl_value_entity=>tt_search_index_cont,
                   <node_tab_line>     TYPE zcl_value_entity=>t_node_table_tmc.
    FIELD-SYMBOLS: <search_matches> TYPE zcl_value_entity=>tt_node_search.
**********************************************************************
*** Check for nkey search
    IF matches( val = iv_search_term regex = 'nkey=([[:digit:]]+)*' case = space ).
      me->perform_search_nk( iv_search_term ).
      RETURN.
    ENDIF.
**********************************************************************
    ASSIGN me->mr_matches->* TO <search_matches>.
    ASSIGN me->mr_search_index->* TO <search_index>.
    ASSIGN me->mr_search_index_cont->* TO <search_index_cont>.
**********************************************************************
*** is the right index!
    CASE abap_true.
      WHEN iv_search_value.
        LOOP AT <search_index_cont> ASSIGNING FIELD-SYMBOL(<search_item_cont>).
          IF iv_search_term IS NOT INITIAL AND matches( val = <search_item_cont>-search_term_w_cont regex = |.*{ iv_search_term }.*| ).
            INSERT VALUE #( node_key = <search_item_cont>-node_key value = abap_true ) INTO TABLE <search_matches>.
*        APPEND <search_item>-node_key_tm TO <nodes_to_high>.
            READ TABLE mr_node_table_tmc->* WITH KEY node_key = <search_item_cont>-node_key ASSIGNING <node_tab_line>.
            <node_tab_line>-highlighted = abap_true.
            UNASSIGN <node_tab_line>.
          ENDIF.
        ENDLOOP.
      WHEN OTHERS.
        LOOP AT <search_index> ASSIGNING FIELD-SYMBOL(<search_item>).
          IF iv_search_term IS NOT INITIAL AND matches( val = <search_item>-search_term regex = |.*{ iv_search_term }.*| ).
            INSERT VALUE #( node_key = <search_item>-node_key highl = abap_true ) INTO TABLE <search_matches>.
*        APPEND <search_item>-node_key_tm TO <nodes_to_high>.
            READ TABLE mr_node_table_tmc->* WITH KEY node_key = <search_item>-node_key ASSIGNING <node_tab_line>.
            <node_tab_line>-highlighted = abap_true.
            UNASSIGN <node_tab_line>.
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.
  METHOD perform_search_nk.
    FIELD-SYMBOLS: <search_matches> TYPE zcl_value_entity=>tt_node_search.
**********************************************************************
    DATA(nkey) = replace( val = iv_search_term regex = 'nkey=' with = space case = space ).
    IF nkey IS NOT INITIAL
      AND line_exists( mr_node_table_tmc->*[ node_key = CONV #( nkey ) ] ).
      ASSIGN me->mr_matches->* TO <search_matches>.
      INSERT VALUE #( node_key = CONV #( nkey ) nkey = abap_true ) INTO TABLE <search_matches>.
      READ TABLE mr_node_table_tmc->* WITH KEY node_key = CONV #( nkey ) ASSIGNING FIELD-SYMBOL(<node_tab_line>).
      <node_tab_line>-highlighted = abap_true.
*      BREAK-POINT.
    ENDIF.
  ENDMETHOD.
  METHOD get_num_matches.
    LOOP AT mr_matches->* ASSIGNING FIELD-SYMBOL(<match>) WHERE value = abap_true OR highl = abap_true OR nkey EQ abap_true.
      rv_num = rv_num + 1.
    ENDLOOP.
  ENDMETHOD.
  METHOD show_text.
    FIELD-SYMBOLS: <str_tab> TYPE t_c_text.
**********************************************************************
    IF me->mv_initialized EQ abap_false.
      me->init_text_edit( ).
      me->mv_initialized = abap_true.
    ENDIF.
*** SET read_only as default
    me->set_read_only_mode( abap_true ).
    ASSIGN me->mr_ref_str_tab->* TO <str_tab>.
    me->set_text_as_table( <str_tab> ).
*    CATCH lcx_screen_error.
  ENDMETHOD.
  METHOD reload_line_size.
    DATA: str_tab TYPE t_c_text,
          info_t  TYPE zcl_value_entity=>tty_string.
**********************************************************************
    "clear before reload text!
    mo_root_ent->clear_messages( ).
    mo_screen_log->info( |Starting reload of value statement| ).
    LOOP AT mo_root_ent->get_content( i_line_size ) ASSIGNING FIELD-SYMBOL(<text>).
      APPEND <text> TO str_tab.
    ENDLOOP.
    me->set_text_as_table( str_tab ).
*    CATCH lcx_screen_error.
    mo_root_ent->collect_messages( i_messages = REF #( info_t ) ).
    mo_screen_log->info_t( info_t ).
    mo_screen_log->info( |Reload of value statement with max_line_size = { i_line_size NUMBER = USER } finished!| ).
  ENDMETHOD.
  METHOD set_ref_str_tab.
    me->mr_ref_str_tab = ir_ref.
  ENDMETHOD.
  METHOD expand_matches.
    DATA: lt_nodes TYPE tt_nodekey.
**********************************************************************
    LOOP AT mr_matches->* ASSIGNING FIELD-SYMBOL(<match>) WHERE parent = abap_true.
      APPEND <match>-node_key TO lt_nodes.
    ENDLOOP.
    me->expand_nodes( lt_nodes ).
*    CATCH lcx_tree_error.
  ENDMETHOD.
  METHOD set_styles_from_matches.
*** Node-Style setzen (NODE_NAME oder VALUE)
    LOOP AT mr_matches->* ASSIGNING FIELD-SYMBOL(<match>).
      CASE abap_true.
        WHEN <match>-highl.
          me->item_set_style(
            EXPORTING
              iv_node_key  = <match>-node_key  " Schlüssel des Knotens
              iv_item_name = 'NODE_NAME'     " Name des Items
              iv_style     = cl_column_tree_model=>style_emphasized_positive " siehe Methodendokumentation
          ).
*          CATCH lcx_tree_error.
        WHEN <match>-value.
          me->item_set_style(
           EXPORTING
             iv_node_key  = <match>-node_key  " Schlüssel des Knotens
             iv_item_name = 'VALUE'     " Name des Items
             iv_style     = cl_column_tree_model=>style_emphasized_positive " siehe Methodendokumentation
         ).
*          CATCH lcx_tree_error.
        WHEN <match>-nkey.
          me->item_set_style(
           EXPORTING
             iv_node_key  = <match>-node_key  " Schlüssel des Knotens
             iv_item_name = 'NODE_KEY'     " Name des Items
             iv_style     = cl_column_tree_model=>style_emphasized_positive " siehe Methodendokumentation
         ).
*          CATCH lcx_tree_error.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  METHOD highlight_ref_elements.
    FIELD-SYMBOLS: <node_tab_tmc> TYPE zcl_value_entity=>tt_node_table_tmc.
    ASSIGN mr_node_table_tmc->* TO <node_tab_tmc>.
**********************************************************************
    LOOP AT <node_tab_tmc> ASSIGNING FIELD-SYMBOL(<entry>) WHERE contains_ref EQ abap_true.
      IF <entry>-node_key EQ mv_root_node_key. CONTINUE. ENDIF.
      me->node_set_style(
        EXPORTING
          iv_node_key = <entry>-node_key " Schlüssel des Knotens
          iv_style    = cl_column_tree_model=>style_emphasized_a " siehe Methodendokumentation
      ).
    ENDLOOP.
  ENDMETHOD.
  METHOD handle_item_double_click.
    DATA: lo_compl_ent TYPE REF TO zcl_compl_entity,
          lt_char_tab  TYPE t_c_text.
    data: lt_message type zcl_value_entity=>tty_string.
    FIELD-SYMBOLS: <node_el>    TYPE zcl_value_entity=>t_node_table_tmc.
**********************************************************************
    READ TABLE me->mr_node_table_tmc->* WITH KEY node_key = node_key ASSIGNING <node_el>.
**********************************************************************
    CHECK sy-subrc = 0.
    CHECK <node_el>-is_complex EQ abap_true.
    CHECK <node_el>-disabled EQ abap_false.
    mo_root_ent->clear_messages( ).
    lcl_screen_log=>get_instance( )->debug( iv_log_msg = |Double Clicked on Node { node_key }| ).
    mv_selected_node_key = node_key.
*** Downcast auf ZCL_COMPL_ENTITY
    lo_compl_ent ?= <node_el>-entity.
    LOOP AT lo_compl_ent->get_content( i_max_line_length = mr_max_line_length->* ) ASSIGNING FIELD-SYMBOL(<cont>).
      APPEND <cont> TO lt_char_tab.
    ENDLOOP.
*    mo_root_ent->collect_messages( ref #( lt_message ) ).
*    mo_screen_log->info_t( lt_message ).
    TRY.
        me->set_text_as_table( lt_char_tab ).
      CATCH lcx_screen_error.
    ENDTRY.
*    CATCH lcx_screen_error.
**********************************************************************
  ENDMETHOD.
  METHOD handle_node_double_click.
    me->handle_item_double_click( node_key ).
  ENDMETHOD.
  METHOD find_all_child_elements.
    FIELD-SYMBOLS: <children> TYPE tt_nodekey,
                   <node_tab> TYPE zcl_value_entity=>tt_node_table_tmc.
    DATA: mv_node_key TYPE tm_nodekey.
    ASSIGN ir_child_nodes->* TO <children>.
    ASSIGN mr_node_table_tmc->* TO <node_tab>.
**********************************************************************
    mv_node_key = iv_nodekey.
    LOOP AT <node_tab> ASSIGNING FIELD-SYMBOL(<node_entry>) WHERE parent_node_key = mv_node_key.
      IF <node_entry>-is_complex EQ abap_true.
        IF mv_filtered EQ abap_true AND <node_entry>-vis_filtered EQ abap_false. RETURN. ENDIF.
        APPEND <node_entry>-node_key TO <children>.
        me->find_all_child_elements(
          EXPORTING
            iv_nodekey     = <node_entry>-node_key
            ir_child_nodes = ir_child_nodes
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD find_all_parent_elements.
    DATA: lv_parent_node_key TYPE zcl_value_entity=>t_node_table_tmc-node_key.
    FIELD-SYMBOLS: <node_table> TYPE zcl_value_entity=>tt_node_table_tmc,
                   <node_entry> TYPE zcl_value_entity=>t_node_table_tmc.
    FIELD-SYMBOLS: <parent_nodes> TYPE tt_nodekey.
**********************************************************************
    ASSIGN me->mr_node_table_tmc->* TO <node_table>.
    ASSIGN ir_parent_nodes->* TO <parent_nodes>.
**********************************************************************
    READ TABLE <node_table> WITH KEY node_key = iv_nodekey ASSIGNING <node_entry>.
*** Loop until root node is reached (parent_node_key = space)
    WHILE <node_entry>-parent_node_key NE space.
      lv_parent_node_key = <node_entry>-parent_node_key.
      IF NOT line_exists( <parent_nodes>[ table_line = lv_parent_node_key ] ).
        APPEND lv_parent_node_key TO <parent_nodes>.
      ENDIF.
      READ TABLE <node_table> WITH KEY node_key = lv_parent_node_key ASSIGNING <node_entry>.
    ENDWHILE.
  ENDMETHOD.
  METHOD find_subtree.
    DATA: lv_selected_nkey TYPE tm_nodekey.
**********************************************************************
    lv_selected_nkey = me->get_selected_nkey( ).
    CHECK lv_selected_nkey IS NOT INITIAL.
    CHECK mr_node_table_tmc->*[ node_key = lv_selected_nkey ]-is_complex EQ abap_true.
    APPEND lv_selected_nkey TO ir_child_nodes->*.
    me->find_all_child_elements(
      EXPORTING
        iv_nodekey     = lv_selected_nkey
        ir_child_nodes = ir_child_nodes
    ).
  ENDMETHOD.
  METHOD collapse_sub_tree.
    DATA: lr_children TYPE REF TO tt_nodekey.
**********************************************************************
    lr_children = NEW #( ).
    me->find_subtree( lr_children ).
    LOOP AT lr_children->* ASSIGNING FIELD-SYMBOL(<child>).
      me->collapse_node( <child> ).
    ENDLOOP.
  ENDMETHOD.
  METHOD expand_sub_tree.
    DATA: lr_children TYPE REF TO tt_nodekey.
    lr_children = NEW #( ).
    me->find_subtree( lr_children ).
    me->expand_nodes( CONV #( lr_children->* ) ).
  ENDMETHOD.
  METHOD save_text_content.
    DATA: lo_compl_ent    TYPE REF TO zcl_compl_entity,
          lt_parent_nodes TYPE tt_nodekey.
    DATA: lt_content  TYPE t_c_text,
          lt_cont_ext TYPE TABLE OF string.
**********************************************************************
    READ TABLE me->mr_node_table_tmc->* WITH KEY node_key = mv_selected_node_key ASSIGNING FIELD-SYMBOL(<node_entry>).
    CHECK sy-subrc = 0.
*** Downcast auf compl_ent -> set external content
    lo_compl_ent ?= <node_entry>-entity.
    lt_content = me->get_text_as_stream( ).
*** Save external content to instance
    LOOP AT lt_content ASSIGNING FIELD-SYMBOL(<cont>).
      APPEND <cont> TO lt_cont_ext.
    ENDLOOP.
    lo_compl_ent->set_external_content( CONV #( lt_cont_ext ) ).
    lo_compl_ent->set_modified( abap_true ).
**********************************************************************
    lcl_screen_log=>get_instance( )->info( |Updated Content for node: { lo_compl_ent->get_key( ) }| ).
*** reinit tree, but do not set root node as selected key
    me->delete_all_nodes( ).
    me->add_root_node_to_tree( abap_false ).
*    if mv_filtered eq abap_true.
*      me->build_filtered_tree( ).
*    endif.
    TRY.
        me->find_all_parent_elements(
          EXPORTING
            iv_nodekey      = mv_selected_node_key
            ir_parent_nodes = REF #( lt_parent_nodes )
        ).
        APPEND mv_selected_node_key TO lt_parent_nodes.
        me->expand_nodes( lt_parent_nodes ).
        me->highlight_ref_elements( ).
*        mo_alv_tree2->get_nodes( )->get_node( mv_selected_node_key )->set_row_style( if_salv_c_tree_style=>emphasized_positive ).
        me->node_set_style(
          EXPORTING
            iv_node_key       = mv_selected_node_key                " Schlüssel des Knotens
            iv_style          = cl_column_tree_model=>style_emphasized " siehe Methodendokumentation
        ).
        me->disable_child_elemnts( mv_selected_node_key ).
        lcl_screen_log=>get_instance( )->info( |Disabling child-elements of node: { lo_compl_ent->get_key( ) }| ).
      CATCH lcx_screen_error INTO DATA(lo_err).
        RAISE EXCEPTION lo_err.
    ENDTRY.
  ENDMETHOD.
  METHOD reset_visible_entries.
*** Reset Flag vis filtered
    LOOP AT me->mr_node_table_tmc->* ASSIGNING FIELD-SYMBOL(<node_entry>) WHERE vis_filtered EQ abap_true.
      <node_entry>-vis_filtered = abap_false.
    ENDLOOP.
  ENDMETHOD.
  METHOD mark_disabled_nodes.
*** Prevent tree-error: if filtered, only visible items are updated
*** vis_filtered is updated when filtered search is performed
    LOOP AT me->mr_node_table_tmc->* ASSIGNING FIELD-SYMBOL(<node_entry>) WHERE disabled EQ abap_true.
      IF mv_filtered EQ abap_true AND <node_entry>-vis_filtered EQ abap_false.
        CONTINUE.
      ENDIF.
      me->node_set_style(
        EXPORTING
          iv_node_key = <node_entry>-node_key " Schlüssel des Knotens
          iv_style    = cl_column_tree_model=>style_inactive " siehe Methodendokumentation
      ).
    ENDLOOP.
*      CATCH lcx_tree_error.
  ENDMETHOD.
  METHOD mark_modified_nodes.
*** Prevent tree-error: if filtered, only visible items are updated
*** vis_filtered is updated when filtered search is performed
    LOOP AT me->mr_node_table_tmc->* ASSIGNING FIELD-SYMBOL(<node_entry>) WHERE modified EQ abap_true.
      IF mv_filtered EQ abap_true AND <node_entry>-vis_filtered EQ abap_false.
        CONTINUE.
      ENDIF.
      me->node_set_style(
        EXPORTING
          iv_node_key = <node_entry>-node_key " Schlüssel des Knotens
          iv_style    = cl_column_tree_model=>style_emphasized " siehe Methodendokumentation
      ).
*      CATCH lcx_tree_error.
    ENDLOOP.
  ENDMETHOD.
  METHOD disable_child_elemnts.
    FIELD-SYMBOLS: <node_table> TYPE zcl_value_entity=>tt_node_table_tmc,
                   <node_entry> TYPE zcl_value_entity=>t_node_table_tmc.
    DATA: lo_node TYPE REF TO cl_salv_node.
**********************************************************************
*** Alle direkten Kind-Elemente (Zuordnung über Tabelle) inaktiv setzen
*** Falls komplexe Kind-Elemente: rekursiver Aufruf
    LOOP AT me->mr_node_table_tmc->* ASSIGNING <node_entry> WHERE parent_node_key = iv_parent_node_key.
      <node_entry>-disabled = abap_true.
      <node_entry>-entity->set_disabled( abap_true ).
      me->node_set_style(
        EXPORTING
          iv_node_key = <node_entry>-node_key " Schlüssel des Knotens
          iv_style    = cl_column_tree_model=>style_inactive " siehe Methodendokumentation
      ).
*** Falls komplex, auch Kind-Elemente disablen (-> Funktioner rekursiv aufrufen)
      IF <node_entry>-is_complex EQ abap_true.
        me->disable_child_elemnts( <node_entry>-node_key ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD move_cursor_down.
    mo_match_cursor->go_down( CHANGING c_cursor_info = c_curs_info ).
    me->set_selected_nkey( mo_match_cursor->mv_current_node ).
*    CATCH lcx_tree_error.
  ENDMETHOD.
  METHOD move_cursor_up.
    mo_match_cursor->go_up( CHANGING c_cursor_info = c_curs_info ).
    me->set_selected_nkey( mo_match_cursor->mv_current_node ).
*    CATCH lcx_tree_error.
  ENDMETHOD.
  METHOD toggle_edit.
    mo_screen_state->toggle_edit( ).
    me->set_read_only_mode( mo_screen_state->is_edit( ) ).
*    CATCH lcx_screen_error.
  ENDMETHOD.
  METHOD str_tab_as_flat_str.
    LOOP AT it_str_tab ASSIGNING FIELD-SYMBOL(<str>).
      rv_flatstring = |{ rv_flatstring } { <str> }|.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
