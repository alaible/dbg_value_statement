CLASS zcl_simple_entity DEFINITION PUBLIC INHERITING FROM zcl_value_entity ABSTRACT CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_typekind TYPE c iv_indent TYPE i,
      get_node_type REDEFINITION,
*      add_to_node REDEFINITION,
      build_node_table REDEFINITION,
      build_node_table_fil REDEFINITION,
      contains_reference REDEFINITION,
      collect_messages REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      get_payload_f_typekind
        IMPORTING i_left TYPE string DEFAULT space
                  i_plus_left_offset type i DEFAULT 0
                  i_max_line_size TYPE i
        RETURNING VALUE(r_payload) TYPE zcl_value_entity=>tty_string.
    DATA: mv_simple_value TYPE string,
          mv_type_kind    TYPE c.
private section.
ENDCLASS.



CLASS ZCL_SIMPLE_ENTITY IMPLEMENTATION.


  METHOD build_node_table.
    FIELD-SYMBOLS: <nodetab_int>     TYPE treemcnota,
                   <itemtab_int>     TYPE treemcitac,
                   <current_nodekey> TYPE tm_nodekey.
    FIELD-SYMBOLS: <nodetab_tmc>       TYPE tt_node_table_tmc,
                   <search_index>      TYPE zcl_value_entity=>tt_search_index,
                   <search_index_cont> TYPE zcl_value_entity=>tt_search_index_cont.
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
                                isfolder = abap_false
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
                                item_name = 'VALUE'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                text = me->mv_simple_value
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
                                is_complex = abap_false
                                contains_ref = me->contains_reference( )
*                               highlighted     TYPE abap_bool,
                              )
                           ).
**********************************************************************
*** Search Index
    ASSIGN ir_search_index->* TO <search_index>.
    ASSIGN ir_search_index_cont->* TO <search_index_cont>.
    <search_index> = VALUE #( BASE <search_index> ( node_key = mv_alv_tree_node_tmc search_term = mv_key ) ).
    <search_index_cont> = VALUE #( BASE <search_index_cont> ( node_key = mv_alv_tree_node_tmc search_term_w_cont = |{ to_lower( mv_simple_value ) }| ) ).
*    LOOP AT me->get_content( ) ASSIGNING FIELD-SYMBOL(<cont>).
*      <search_index_cont> = VALUE #( BASE <search_index_cont> ( node_key = mv_alv_tree_node_tmc search_term = |{ mv_key } { <cont> }| ) ).
*    ENDLOOP.
  ENDMETHOD.


  METHOD build_node_table_fil.
    FIELD-SYMBOLS: <nodetab_int> TYPE treemcnota,
                   <itemtab_int> TYPE treemcitac.
    FIELD-SYMBOLS: <match>      TYPE zcl_value_entity=>t_node_search,
                   <node_table> TYPE zcl_value_entity=>tt_node_table_tmc.
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
                                isfolder = abap_false
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
                                item_name = 'VALUE'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                style = COND #(
                                          WHEN <match>-value EQ abap_true "line_exists( <highl_nodes>[ table_line = mv_alv_tree_node_tmc ] )
                                          THEN cl_column_tree_model=>style_emphasized_positive
                                          ELSE cl_column_tree_model=>style_default )
                                text = me->mv_simple_value
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
  ENDMETHOD.


  METHOD collect_messages.
    FIELD-SYMBOLS: <messages> TYPE zcl_value_entity=>tty_string.
    ASSIGN i_messages->* TO <messages>.
    <messages> = VALUE #( BASE <messages> FOR message IN mt_info_messages ( |Nkey: { mv_alv_tree_node_tmc } -- { message }| ) ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    mv_type_kind = iv_typekind.
    mv_indent_level = iv_indent.
  ENDMETHOD.


  METHOD contains_reference.
    rv_contains_ref = abap_false.
  ENDMETHOD.


  METHOD get_node_type.
    rv_node_type = 'simple'.
  ENDMETHOD.


  METHOD get_payload_f_typekind.
    DATA: single_line TYPE string,
          split_size  TYPE i.
    CASE mv_type_kind.
      WHEN cl_abap_typedescr=>typekind_int.
        single_line = |{ i_left }{ condense( me->mv_simple_value ) }|.
        r_payload = VALUE #( ( single_line ) ).
      WHEN cl_abap_typedescr=>typekind_packed.
        single_line = |{ i_left }'{ condense( me->mv_simple_value ) }'|.
        r_payload = VALUE #( ( single_line ) ).
      WHEN cl_abap_typedescr=>typekind_decfloat16.
        single_line = |{ i_left }'{ condense( me->mv_simple_value ) }'|.
        r_payload = VALUE #( ( single_line ) ).
      WHEN cl_abap_typedescr=>typekind_decfloat34.
        single_line = |{ i_left }'{ condense( me->mv_simple_value ) }'|.
        r_payload = VALUE #( ( single_line ) ).
      WHEN cl_abap_typedescr=>typekind_int8.
        single_line = |{ i_left }conv int8( '{ me->mv_simple_value }' )|.
        r_payload = VALUE #( ( single_line ) ).
      WHEN cl_abap_typedescr=>typekind_xstring.
        single_line = |{ i_left }conv xstring( '{ me->mv_simple_value }' )|.
        r_payload = VALUE #( ( single_line ) ).
      WHEN cl_abap_typedescr=>typekind_hex.
        single_line = |{ i_left }'{ me->mv_simple_value }'|.
**********************************************************************
*** String with splitting and escaping
      WHEN cl_abap_typedescr=>typekind_string.
*        APPEND |{ get_indention( ) }INDENT_LEVEL = { mv_indent_level }| TO r_payload.
        DATA left_spaces TYPE string.
        DO strlen( i_left ) TIMES.
          left_spaces = |{ left_spaces } |.
        ENDDO.
        DATA(escaped) = |{ replace( val = me->mv_simple_value regex = |`| with = |``| occ = 0 ) }|.
        single_line = |{ i_left }`{ escaped }`|.
        split_size = i_max_line_size - 6 - i_plus_left_offset - ( mv_indent_level * ( mv_indent_level + 1 ) ) - strlen( |{ i_left }| ). "<content>` && = 50 - 46
        IF split_size <= 0.
          me->add_info_message( i_message = |Splitsize negative!| ).
          r_payload = VALUE #( ( single_line ) ).
        ELSEIF ( mv_indent_level * ( mv_indent_level + 1 ) ) + 1 * strlen( |{ i_left }`{ escaped }`| ) > i_max_line_size.
          DATA(splitted) = zcl_dbg_utils=>split_string(
                             i_string  = escaped
                             i_max_len = split_size
                           ).
          me->add_info_message( i_message = |lines after split: { lines( splitted ) }| ).
          LOOP AT splitted ASSIGNING FIELD-SYMBOL(<splitted>).
            CASE sy-tabix.
              WHEN 1.
                APPEND |{ i_left }`{ <splitted> }` &&| TO r_payload.
              WHEN lines( splitted ).
                APPEND |\t`{ <splitted> }`| TO r_payload.
              WHEN OTHERS.
                APPEND |\t`{ <splitted> }` &&| TO r_payload.
            ENDCASE.
          ENDLOOP.
        ELSE.
          r_payload = VALUE #( ( single_line ) ).
        ENDIF.
      WHEN OTHERS.
*        APPEND |{ get_indention( ) }INDENT_LEVEL = { mv_indent_level }| TO r_payload.
        escaped = |{ replace( val = me->mv_simple_value regex = |'| with = |''| occ = 0 ) }|.
        single_line = |{ i_left }'{ escaped }'|.
        split_size = i_max_line_size - 6 - i_plus_left_offset - ( mv_indent_level * ( mv_indent_level + 1 ) ) - strlen( |{ i_left }| ). "<content>` && = 50 - 46
        DATA(strlen) = ( mv_indent_level * ( mv_indent_level + 1 ) ) + 1 + strlen( |{ i_left }'{ escaped }'| ).
        IF split_size <= 0.
          me->add_info_message( i_message = |Splitsize negative!| ).
          r_payload = VALUE #( ( single_line ) ).
        ELSEIF ( mv_indent_level * ( mv_indent_level + 1 ) ) + 1 + strlen( |{ i_left }'{ escaped }'| ) > i_max_line_size.
          splitted = zcl_dbg_utils=>split_string(
                             i_string  = escaped
                             i_max_len = split_size
                           ).
          me->add_info_message( i_message = |lines after split: { lines( splitted ) }| ).
          LOOP AT splitted ASSIGNING <splitted>.
            CASE sy-tabix.
              WHEN 1.
                APPEND |{ i_left }'{ <splitted> }' &&| TO r_payload.
              WHEN lines( splitted ).
                APPEND |\t'{ <splitted> }'| TO r_payload.
              WHEN OTHERS.
                APPEND |\t'{ <splitted> }' &&| TO r_payload.
            ENDCASE.
          ENDLOOP.
        ELSE.
          r_payload = VALUE #( ( single_line ) ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
