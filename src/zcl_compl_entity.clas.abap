CLASS zcl_compl_entity DEFINITION PUBLIC INHERITING FROM zcl_entity ABSTRACT.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_mock_data,
             comp_fullname TYPE string,
           END OF t_mock_data.
    TYPES: tt_mock_data TYPE TABLE OF t_mock_data WITH EMPTY KEY.
    TYPES: BEGIN OF ty_node_cont.
             INCLUDE TYPE zvalue_alv_tree.
*    TYPES: ref_str_tab TYPE ztest_entity=>ty_ref_strtab,
             TYPES:   val_cont TYPE REF TO zcl_value_content,
           END OF ty_node_cont.
    METHODS:
      constructor IMPORTING iv_is_root  TYPE abap_bool
                            iv_curr_ind TYPE i,
      get_node_type REDEFINITION,
      contains_reference REDEFINITION,
      build_node_table REDEFINITION,
      build_node_table_fil REDEFINITION,
      collect_messages REDEFINITION,
      clear_messages REDEFINITION,
      set_external_content IMPORTING it_cont TYPE zcl_entity=>tty_string,
      get_alv_tree_key RETURNING VALUE(rv_key) TYPE string,
      get_content_line_size IMPORTING i_line_size TYPE i DEFAULT 128 RETURNING VALUE(rt_content) TYPE zcl_entity=>tty_string.
  PROTECTED SECTION.
    DATA: mt_components TYPE TABLE OF REF TO zcl_entity,
          mv_is_root    TYPE abap_bool.
    DATA: mt_external_content TYPE zcl_entity=>tty_string.

    CLASS-METHODS: str_tab_as_flat_str IMPORTING it_str_tab TYPE zcl_entity=>tty_string RETURNING VALUE(rv_flatstring) TYPE string.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_COMPL_ENTITY IMPLEMENTATION.


  METHOD build_node_table.
    FIELD-SYMBOLS: <nodetab_int>     TYPE treemcnota,
                   <itemtab_int>     TYPE treemcitac,
                   <current_nodekey> TYPE tm_nodekey.
    FIELD-SYMBOLS: <nodetab_tmc>       TYPE tt_node_table_tmc,
                   <search_index>      TYPE zcl_entity=>tt_search_index,
                   <search_index_cont> TYPE zcl_entity=>tt_search_index_cont.
    DATA: lv_nkey_asint TYPE i.
**********************************************************************
    ASSIGN ir_current_key->* TO <current_nodekey>.
    lv_nkey_asint = CONV #( <current_nodekey> ).
    mv_alv_tree_node_tmc = |{ lv_nkey_asint + 1 }|.
    mv_alv_tree_node_tmc_par = iv_parent_key.
    <current_nodekey> = mv_alv_tree_node_tmc.
    ASSIGN ir_node_tab->* TO <nodetab_int>.
    <nodetab_int> = VALUE #( BASE <nodetab_int>
                              (
                                node_key = mv_alv_tree_node_tmc
                                relatkey = iv_parent_key
                                isfolder = abap_true
                                relatship = cl_tree_model=>relat_last_child
                              )
                           ).
    ASSIGN ir_item_tab->* TO <itemtab_int>.
    <itemtab_int> = VALUE #( BASE <itemtab_int>
                              (
                                item_name = 'NODE_NAME'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                text = me->get_key( )
                              )
                              (
                                item_name = 'NODE_TYPE'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                text = me->get_node_type( )
                              )
                              (
                                item_name = 'NODE_KEY'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                text = |{ me->mv_alv_tree_node_tmc ALIGN = right PAD = space WIDTH = 10 }|
                              )
                              (
                                item_name = 'CONTAINS_REF'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                text = me->contains_reference( )
                              )
                          ).

**********************************************************************
*** Node-Table
    ASSIGN ir_node_table->* TO <nodetab_tmc>.
    <nodetab_tmc> = VALUE #( BASE <nodetab_tmc>
                              (
                                node_key = mv_alv_tree_node_tmc
                                parent_node_key = iv_parent_key
                                entity = me
                                disabled = mv_disabled
                                modified = mv_modified
                                is_complex = abap_true
                                contains_ref = me->contains_reference( )
*                               highlighted     TYPE abap_bool,
                              )
                           ).
**********************************************************************
*** Search Index
    ASSIGN ir_search_index->* TO <search_index>.
    ASSIGN ir_search_index_cont->* TO <search_index_cont>.
    <search_index> = VALUE #( BASE <search_index> ( node_key = mv_alv_tree_node_tmc search_term = mv_key ) ).
*    <search_index_cont> = value #( BASE <search_index_cont> ( node_key = mv_alv_tree_node_tmc search_term = mv_key ) ).
*** Alle Kind-Elemente rekursiv hinzufügen
    LOOP AT me->mt_components ASSIGNING FIELD-SYMBOL(<comp>).
      <comp>->build_node_table(
        EXPORTING
          ir_node_tab     = ir_node_tab
          ir_item_tab     = ir_item_tab
          iv_parent_key   = mv_alv_tree_node_tmc
          ir_current_key  = ir_current_key
          iv_dd_id        = iv_dd_id
          ir_node_table   = ir_node_table
          ir_search_index = ir_search_index
          ir_search_index_cont = ir_search_index_cont
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD build_node_table_fil.
    FIELD-SYMBOLS: <nodetab_int> TYPE treemcnota,
                   <itemtab_int> TYPE treemcitac.
    FIELD-SYMBOLS: <match>      TYPE zcl_entity=>t_node_search,
                   <node_table> TYPE zcl_entity=>tt_node_table_tmc.
**********************************************************************
*** Check if Node is relevant after filtering!
    READ TABLE ir_matches->* WITH KEY node_key = mv_alv_tree_node_tmc ASSIGNING <match>.
    CHECK sy-subrc = 0.
**********************************************************************
    ASSIGN ir_node_tab->* TO <nodetab_int>.
    <nodetab_int> = VALUE #( BASE <nodetab_int>
                              (
                                node_key = mv_alv_tree_node_tmc
                                relatkey = mv_alv_tree_node_tmc_par
                                isfolder = abap_true
                                relatship = cl_tree_model=>relat_last_child
                                style = cl_column_tree_model=>style_default
                              )
                           ).
    ASSIGN ir_item_tab->* TO <itemtab_int>.
    <itemtab_int> = VALUE #( BASE <itemtab_int>
                              (
                                item_name = 'NODE_NAME'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                style = COND #(
                                          WHEN <match>-highl EQ abap_true "line_exists( <highl_nodes>[ table_line = mv_alv_tree_node_tmc ] )
                                          THEN cl_column_tree_model=>style_emphasized_positive
                                          ELSE cl_column_tree_model=>style_default )
                                text = me->get_key( )
                              )
                              (
                                item_name = 'NODE_TYPE'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                text = me->get_node_type( )
                              )
                              (
                                item_name = 'NODE_KEY'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                style = COND #(
                                          WHEN <match>-nkey EQ abap_true "line_exists( <highl_nodes>[ table_line = mv_alv_tree_node_tmc ] )
                                          THEN cl_column_tree_model=>style_emphasized_positive
                                          ELSE cl_column_tree_model=>style_default )
                                text = |{ me->mv_alv_tree_node_tmc ALIGN = right PAD = space WIDTH = 10 }|
                              )
                              (
                                item_name = 'CONTAINS_REF'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                text = me->contains_reference( )
                              )
                            ).
**********************************************************************
    ASSIGN ir_node_table->* TO <node_table>.
    READ TABLE <node_table> WITH KEY node_key = mv_alv_tree_node_tmc ASSIGNING FIELD-SYMBOL(<node_entry>).
    <node_entry>-vis_filtered = abap_true.
*** Alle Kind-Elemente rekursiv hinzufügen
    LOOP AT me->mt_components ASSIGNING FIELD-SYMBOL(<comp>).
      <comp>->build_node_table_fil(
        EXPORTING
          ir_node_tab = ir_node_tab
          ir_item_tab = ir_item_tab
          ir_node_table = ir_node_table
          ir_matches  = ir_matches
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD clear_messages.
    super->clear_messages( ).
    LOOP AT mt_components ASSIGNING FIELD-SYMBOL(<comp>).
      <comp>->clear_messages( ).
    ENDLOOP.
  ENDMETHOD.


  METHOD collect_messages.
    FIELD-SYMBOLS: <messages> TYPE zcl_entity=>tty_string.
    ASSIGN i_messages->* TO <messages>.
    <messages> = VALUE #( BASE <messages> FOR message IN mt_info_messages ( |Nkey: { mv_alv_tree_node_tmc } -- { message }| ) ).
    LOOP AT me->mt_components ASSIGNING FIELD-SYMBOL(<comp>).
      <comp>->collect_messages( i_messages ).
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    me->mv_is_root = iv_is_root.
    me->mv_indent_level = iv_curr_ind + 1.
*    IF me->mv_is_root EQ abap_true.
*      me->init_node_table( ).
*    ENDIF.
  ENDMETHOD.


  METHOD contains_reference.
    LOOP AT mt_components ASSIGNING FIELD-SYMBOL(<comp>).
      IF <comp>->contains_reference( ) EQ abap_true.
        rv_contains_ref = abap_true.
        RETURN.
      ENDIF.
      rv_contains_ref = abap_false.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_alv_tree_key.
    rv_key = me->mv_alv_tree_key.
  ENDMETHOD.


  METHOD get_content_line_size.
    DATA: cut TYPE string.
    LOOP AT me->get_content( ) ASSIGNING FIELD-SYMBOL(<cont>).
      IF strlen( <cont> ) > i_line_size.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_node_type.
    rv_node_type = 'complex'.
  ENDMETHOD.


  METHOD set_external_content.
    me->mt_external_content = it_cont.
  ENDMETHOD.


  METHOD str_tab_as_flat_str.
    LOOP AT it_str_tab ASSIGNING FIELD-SYMBOL(<str>).
      rv_flatstring = |{ rv_flatstring } { <str> }|.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
