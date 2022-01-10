"! Abstract Base-Class for all Entity-Types
CLASS zcl_entity DEFINITION PUBLIC ABSTRACT.
  PUBLIC SECTION.
    TYPES: tty_string TYPE TABLE OF string WITH EMPTY KEY.
    TYPES: ty_ref_strtab TYPE REF TO tty_string.
    TYPES: BEGIN OF ty_node_table,
             node_key        TYPE salv_de_node_key,
             node_key_old    TYPE salv_de_node_key,
             entity          TYPE REF TO zcl_entity,
             parent_node_key TYPE salv_de_node_key,
             contains_ref    TYPE abap_bool,
             disabled        TYPE abap_bool,
             is_complex      TYPE abap_bool,
           END OF ty_node_table.
    TYPES: tty_node_table TYPE HASHED TABLE OF ty_node_table WITH UNIQUE KEY node_key.
**********************************************************************
*** Für Tree-Model
    TYPES: BEGIN OF t_node_table_tmc,
             node_key        TYPE tm_nodekey,
             node_key_old    TYPE tm_nodekey,
             entity          TYPE REF TO zcl_entity,
             parent_node_key TYPE tm_nodekey,
             contains_ref    TYPE abap_bool,
             disabled        TYPE abap_bool,
             modified        TYPE abap_bool,
             highlighted     TYPE abap_bool,
             is_complex      TYPE abap_bool,
             vis_filtered    TYPE abap_bool,
           END OF t_node_table_tmc.
    TYPES: tt_node_table_tmc TYPE HASHED TABLE OF t_node_table_tmc WITH UNIQUE KEY node_key.
*** Search-Index
    TYPES: BEGIN OF t_search_index,
             search_term        TYPE string,
             search_term_w_cont TYPE string,
             node_key           TYPE tm_nodekey,
           END OF t_search_index.
    TYPES: tt_search_index      TYPE SORTED TABLE OF t_search_index WITH NON-UNIQUE KEY search_term,
           tt_search_index_cont TYPE SORTED TABLE OF t_search_index WITH NON-UNIQUE KEY search_term_w_cont.
**********************************************************************
*** Typ für Knotensuche
    TYPES: BEGIN OF t_node_search,
             node_key TYPE tm_nodekey,
             highl    TYPE abap_bool,
             value    TYPE abap_bool,
             parent   TYPE abap_bool,
             child    TYPE abap_bool,
             nkey     TYPE abap_bool,
           END OF t_node_search.
    TYPES: tt_node_search TYPE HASHED TABLE OF t_node_search WITH UNIQUE KEY node_key.
    TYPES: tr_node_search TYPE REF TO tt_node_search.
*** abstract methods
    METHODS:
      "! Returning the Content (Including subnodes, if existent)
      "! @parameter rt_content | String-Table with Node-Content
      get_content ABSTRACT IMPORTING i_max_line_length TYPE i DEFAULT 256 RETURNING VALUE(rt_content) TYPE zcl_entity=>tty_string,
      "! Returning a flag if reference Objects are contained (Including subnodes)
      "! @parameter rv_contains_ref | Flag, if reference is contained
      contains_reference ABSTRACT RETURNING VALUE(rv_contains_ref) TYPE abap_bool,
      "! Returning a Description of the Node-Type
      "! @parameter rv_node_type | Description of the Node-Type
      get_node_type ABSTRACT RETURNING VALUE(rv_node_type) TYPE string,
      "! Abstract Method for building node-table (neede for cl_column_tree_model)
      "! @parameter ir_node_tab | Reference to node-table
      "! @parameter ir_item_tab | Reference to item-table
      "! @parameter iv_parent_key | NodeKey of Parent-Node
      "! @parameter ir_current_key | Current(=Max) NodeKey (for incrementation)
      "! @parameter iv_dd_id | Drag-Drop-id (currently not used)
      "! @parameter ir_node_table | Reference to NodeTable (Internal Type)
      "! @parameter ir_search_index | Reference to Search-Index (Node-Names)
      "! @parameter ir_search_index_cont | Reference to Search-Index (Node-Content)
      build_node_table ABSTRACT IMPORTING ir_node_tab          TYPE REF TO treemcnota
                                          ir_item_tab          TYPE REF TO treemcitac
                                          iv_parent_key        TYPE tm_nodekey
                                          ir_current_key       TYPE REF TO tm_nodekey
                                          iv_dd_id             TYPE treemcnodt-dragdropid
                                          ir_node_table        TYPE REF TO tt_node_table_tmc
                                          ir_search_index      TYPE REF TO tt_search_index
                                          ir_search_index_cont TYPE REF TO tt_search_index_cont,
      "! Abstract Method for building the filtered NodeTable (After Searching)
      "! @parameter ir_node_tab |
      "! @parameter ir_item_tab |
      "! @parameter ir_node_table |
      "! @parameter ir_matches |
      build_node_table_fil ABSTRACT IMPORTING ir_node_tab   TYPE REF TO treemcnota
                                              ir_item_tab   TYPE REF TO treemcitac
                                              ir_node_table TYPE REF TO tt_node_table_tmc
                                              ir_matches    TYPE REF TO zcl_entity=>tt_node_search,
      collect_messages ABSTRACT IMPORTING i_messages TYPE REF TO zcl_entity=>tty_string.
*** instance methods:
    METHODS:
      clear_messages,
      get_key RETURNING VALUE(rv_key) TYPE string,
      set_disabled IMPORTING iv_disabled TYPE abap_bool,
      set_modified IMPORTING iv_modified TYPE abap_bool.
    DATA:
      mv_alv_tree_node_tmc     TYPE tm_nodekey READ-ONLY,
      mv_alv_tree_node_tmc_par TYPE tm_nodekey READ-ONLY.
  PROTECTED SECTION.
*** Key To Display in ALV-Tree
    DATA: mv_key          TYPE string,
          mv_contains_ref TYPE abap_bool,
          mv_disabled     TYPE abap_bool,
          mv_modified     TYPE abap_bool,
          mv_indent_level TYPE i.
    DATA: mv_alv_tree_key     TYPE string,
          mv_alv_tree_key_old TYPE string.
    DATA: mt_info_messages TYPE TABLE OF string WITH EMPTY KEY.
    METHODS:
      get_indention RETURNING VALUE(rv_ind) TYPE string,
      add_info_message IMPORTING i_message TYPE string.
ENDCLASS.



CLASS ZCL_ENTITY IMPLEMENTATION.


  METHOD add_info_message.
    APPEND i_message TO mt_info_messages.
  ENDMETHOD.


  METHOD clear_messages.
    CLEAR mt_info_messages.
  ENDMETHOD.


  METHOD get_indention.
    DO mv_indent_level TIMES.
*      rv_ind = |{ rv_ind }{ cl_abap_char_utilities=>horizontal_tab }|.
      rv_ind = |{ rv_ind }  |.
    ENDDO.
  ENDMETHOD.


  METHOD get_key.
    rv_key = me->mv_key.
  ENDMETHOD.


  METHOD set_disabled.
    mv_disabled = iv_disabled.
  ENDMETHOD.


  METHOD set_modified.
    mv_modified = iv_modified.
  ENDMETHOD.
ENDCLASS.
