CLASS zcl_table_entity DEFINITION
  PUBLIC
  INHERITING FROM zcl_compl_entity
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_compname  TYPE string
                            io_data_ref  TYPE REF TO cl_tpda_script_data_descr
                            iv_is_root   TYPE abap_bool
*                            ir_mock_data TYPE REF TO zcl_compl_entity=>tt_mock_data
                            iv_curr_ind  TYPE i
                  RAISING   cx_sy_move_cast_error cx_tpda_varname cx_tpda_data_descr_invalidated,
      get_node_type REDEFINITION,
      get_content REDEFINITION.
ENDCLASS.



CLASS ZCL_TABLE_ENTITY IMPLEMENTATION.


  METHOD constructor.
* Target-References for typecasts
    DATA: lr_simple  TYPE REF TO cl_tpda_script_elemdescr,
          lr_struct  TYPE REF TO cl_tpda_script_structdescr,
          lr_string  TYPE REF TO cl_tpda_script_stringdescr,
          lr_tab     TYPE REF TO cl_tpda_script_tabledescr,
          lr_dataref TYPE REF TO cl_tpda_script_datrefdescr,
          lr_objref  TYPE REF TO cl_tpda_script_orefdescr.
* Target-References for Importing-Reference and get_line_handle( )
    DATA: lo_table_descr TYPE REF TO cl_tpda_script_tabledescr,
          lo_line_desc   TYPE REF TO cl_tpda_script_data_descr.
    DATA: lt_components TYPE tpda_scr_table_comp_it.
    DATA: lv_line_cnt TYPE i VALUE 1.
    FIELD-SYMBOLS: <mock_data> TYPE zcl_compl_entity=>tt_mock_data.
**********************************************************************
* iv_is_root is passed through
    super->constructor( iv_is_root = iv_is_root iv_curr_ind = iv_curr_ind ).
**********************************************************************
    me->mv_key = iv_compname.
*    BREAK-POINT.
*    ASSIGN ir_mock_data->* TO <mock_data>.
**********************************************************************
*** cast to table_descr
    lo_table_descr ?= io_data_ref.
    lt_components = lo_table_descr->components( ).
**********************************************************************
    DO lo_table_descr->linecnt( ) TIMES.
      TRY.
          lo_line_desc = lo_table_descr->get_line_handle( p_line = lv_line_cnt ).
          lr_struct ?= lo_line_desc.
          mt_components = VALUE #( BASE mt_components
            ( NEW zcl_table_line_ent(
                io_data_descr = lo_line_desc
                iv_table_name = iv_compname
                iv_tab_index = sy-index
                iv_is_root = abap_false
*                ir_mock_data = ir_mock_data
                iv_curr_ind = mv_indent_level
              )
            )
          ).
        CATCH cx_sy_move_cast_error.
          TRY.
              lr_simple ?= lo_line_desc.
              DATA(l_comp) = lt_components[ compname = 'TABLE_LINE' ].
*              DATA(lv_tech_info) = lr_simple->get_technical_info( ).
              DATA(lv_simple) =  lr_simple->value( ) .
              mt_components = VALUE #( BASE mt_components
                ( NEW zcl_simple_table(
                    iv_value = lv_simple
                    iv_comp_name = to_lower( iv_compname )
                    iv_tab_idx = sy-index
                    iv_typekind = l_comp-typid
                    iv_indent = mv_indent_level
                  )
                )
              ).
            CATCH cx_sy_move_cast_error.
              TRY.
                  lr_string ?= lo_line_desc.
                  l_comp = lt_components[ compname = 'TABLE_LINE' ].
                  DATA(lv_string_val) = lr_string->value( ).
                  mt_components = VALUE #( BASE mt_components
                    ( NEW zcl_simple_table(
                        iv_value = lv_string_val
                        iv_comp_name = to_lower( iv_compname )
                        iv_tab_idx = sy-index
                        iv_typekind = l_comp-typid
                        iv_indent = mv_indent_level
                      )
                    )
                  ).
                CATCH cx_sy_move_cast_error.
                  TRY.
                      lr_tab ?= lo_line_desc.
                      mt_components = VALUE #( BASE mt_components
                        ( NEW zcl_table_entity(
                            iv_compname = space
                            io_data_ref = lo_line_desc
                            iv_is_root = abap_false
*                            ir_mock_data = ir_mock_data
                            iv_curr_ind = mv_indent_level
                          )
                        )
                      ).
                    CATCH cx_sy_move_cast_error.
                  ENDTRY.
                  TRY.
                      lr_objref ?= lo_line_desc.
                      l_comp = lt_components[ compname = 'TABLE_LINE' ].
                      DATA(class_name) = lr_objref->get_object_handle( )->classname( ).
                      mt_components = VALUE #( BASE mt_components
                        ( NEW zcl_simple_ref_table(
                            iv_comp_name = to_lower( iv_compname )
                            iv_tab_idx = sy-index
                            iv_type = |OBJ::{ class_name }|
                            iv_typekind = l_comp-typid
                            iv_indent = mv_indent_level
                          )
                        )
                      ).
                    CATCH cx_sy_move_cast_error.
                      TRY.
                          lr_dataref ?= lo_line_desc.
                          DATA(dref_handle) = lr_dataref->get_ref_to_handle( ).
                          DATA(instance_name) = lr_dataref->instancename( ).
                          l_comp = lt_components[ compname = 'TABLE_LINE' ].
                          mt_components = VALUE #( BASE mt_components
                            ( NEW zcl_simple_ref_table(
                                iv_comp_name = iv_compname
                                iv_tab_idx = sy-index
                                iv_type = |DATA::{ instance_name }|
                                iv_typekind = l_comp-typid
                                iv_indent = mv_indent_level
                              )
                            )
                          ).
                        CATCH cx_sy_move_cast_error.
                      ENDTRY.
                  ENDTRY.
              ENDTRY.
          ENDTRY.
      ENDTRY.
*** Komponenten-Index erhöhen
      lv_line_cnt = lv_line_cnt + 1.
    ENDDO.
  ENDMETHOD.


  METHOD get_content.
    IF mt_external_content IS NOT INITIAL.
      rt_content = me->mt_external_content.
      RETURN.
    ENDIF.
*** leere Tabellen gesondert ausgeben, da sonst eine leeres Element fehlt
    IF lines( mt_components ) EQ 0.
      APPEND |{ COND string( WHEN mv_is_root EQ abap_true THEN space ELSE me->get_key( ) ) } = value #( )| TO rt_content.
      RETURN.
    ENDIF.
    APPEND |{ COND string( WHEN mv_is_root EQ abap_true THEN space ELSE me->get_key( ) ) } = value #(| TO rt_content.
    LOOP AT mt_components ASSIGNING FIELD-SYMBOL(<comp>).
*      APPEND LINES OF <fs_line_cont>->get_content( ) TO rt_content.
      LOOP AT <comp>->get_content( i_max_line_length = i_max_line_length ) ASSIGNING FIELD-SYMBOL(<cont>).
        APPEND |{ get_indention( ) }{ <cont> }| TO rt_content.
      ENDLOOP.
    ENDLOOP.
    APPEND |)| TO rt_content.
  ENDMETHOD.


  METHOD get_node_type.
    rv_node_type = 'table'.
  ENDMETHOD.
ENDCLASS.
