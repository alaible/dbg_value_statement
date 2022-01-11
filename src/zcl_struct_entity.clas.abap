CLASS zcl_struct_entity DEFINITION PUBLIC INHERITING FROM zcl_compl_entity CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      get_content REDEFINITION,
      get_node_type REDEFINITION,
      constructor
        IMPORTING iv_compname   TYPE string
                  io_data_descr TYPE REF TO cl_tpda_script_data_descr
                  iv_is_root    TYPE abap_bool
*                  ir_mock_data  TYPE REF TO zcl_compl_entity=>tt_mock_data
                  iv_curr_ind   TYPE i
        RAISING   cx_sy_move_cast_error cx_tpda_varname cx_tpda_data_descr_invalidated.
*  PROTECTED SECTION.
*    DATA: mt_components TYPE ztest_entity=>ty_struc_content.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_STRUCT_ENTITY IMPLEMENTATION.


  METHOD constructor.
*** Payload (Quickdata) (Used for Downcasts)
    DATA: lr_symbsimple TYPE REF TO tpda_sys_symbsimple,
          lr_symbstruct TYPE REF TO tpda_sys_symbstruct,
          lr_symbstring TYPE REF TO tpda_sys_symbstring,
          lr_symbref    TYPE REF TO tpda_sys_symbdatref,
          lr_symbobjref TYPE REF TO tpda_sys_symbobjref,
          lr_symbtab    TYPE REF TO tpda_sys_symbtab.
*** Components of Structure
    DATA: lt_components_full_it TYPE tpda_scr_struct_comp_it,
          lt_components_it      TYPE tpda_script_struc_componentsit.
*** Target Refernces for Importing Reference and Component Reference
    DATA: lo_struct_descr TYPE REF TO cl_tpda_script_structdescr,
          lo_comp_descr   TYPE REF TO cl_tpda_script_data_descr.
*    FIELD-SYMBOLS: <mock_data> TYPE zcl_compl_entity=>tt_mock_data.
**********************************************************************

*** iv_is_root is handed through
    super->constructor( iv_is_root = iv_is_root iv_curr_ind = iv_curr_ind ).
* -> The Outer sy_move_cast_error is not caught here
*    TRY.
    lo_struct_descr ?= io_data_descr.
    lo_struct_descr->components(
      IMPORTING
        p_components_full_it = lt_components_full_it                " TPDA: Retrieval-Tabelle für get_Symb_Struct_1stLevel
        p_components_it = lt_components_it
    ).
*** -> Do this in super->constructor( )
    me->mv_key = iv_compname.
**********************************************************************
*    ASSIGN ir_mock_data->* TO <mock_data>.

* Loop through all components of the structure and
    LOOP AT lt_components_full_it ASSIGNING FIELD-SYMBOL(<comp>).
*      <mock_data> = VALUE #( BASE <mock_data> ( comp_fullname = lt_components_it[ compname = <comp>-compname ]-longname ) ).
      ASSIGN <comp>-symbquick-quickdata TO FIELD-SYMBOL(<quick_data>).
      TRY.
          lr_symbsimple ?= <quick_data>.
          mt_components = VALUE #( BASE mt_components
            ( NEW zcl_simple_struct(
                iv_key = to_lower( <comp>-compname )
                iv_value = lr_symbsimple->valstring
                iv_typekind = <comp>-symbquick-typid
                iv_indent = mv_indent_level
              )
            )
          ).
        CATCH cx_sy_move_cast_error.
          TRY.
              lr_symbstring ?= <quick_data>.
              mt_components = VALUE #( BASE mt_components
                ( NEW zcl_simple_struct(
                    iv_key = to_lower( <comp>-compname )
                    iv_value = lr_symbstring->valstring
                    iv_typekind = <comp>-symbquick-typid
                    iv_indent = mv_indent_level
                  )
                )
              ).
            CATCH cx_sy_move_cast_error.
              TRY.
                  lr_symbstruct ?= <quick_data>.
                  lo_comp_descr = cl_tpda_script_data_descr=>factory( p_var_name = lt_components_it[ compname = <comp>-compname ]-longname ).
                  mt_components = VALUE #( BASE mt_components
                    ( NEW zcl_struct_entity(
                        iv_compname = to_lower( <comp>-compname )
                        io_data_descr = lo_comp_descr
                        iv_is_root = abap_false
*                        ir_mock_data = ir_mock_data
                        iv_curr_ind = mv_indent_level
                      )
                    )
                  ).
                CATCH cx_sy_move_cast_error.
                  TRY.
                      lr_symbtab ?= <quick_data>.
                      lo_comp_descr = cl_tpda_script_data_descr=>factory( p_var_name = lt_components_it[ compname = <comp>-compname ]-longname ).
                      mt_components = VALUE #( BASE mt_components
                        ( NEW zcl_table_entity(
                            iv_compname = to_lower( <comp>-compname )
                            io_data_ref = lo_comp_descr
                            iv_is_root = abap_false
*                            ir_mock_data = new #( )
                            iv_curr_ind = mv_indent_level
                          )
                        )
                      ).
                    CATCH cx_sy_move_cast_error.
                      TRY.
                          lr_symbref ?= <quick_data>.
                          DATA(d_ref) = lr_symbref->datref.
                          mt_components = VALUE #( BASE mt_components
                            ( NEW zcl_ref_entity(
                                iv_key = to_lower( <comp>-compname )
                                iv_type = |DATA::{ lr_symbref->instancename }|
                                iv_typekind = <comp>-symbquick-typid
                                iv_indent = mv_indent_level
                              )
                            )
                          ).
                        CATCH cx_sy_move_cast_error.
                      ENDTRY.
                      TRY.
                          lr_symbobjref ?= <quick_data>.
                          DATA(o_ref) = lr_symbobjref->objref.
                          mt_components = VALUE #( BASE mt_components
                            ( NEW zcl_ref_entity(
                                iv_key = to_lower( <comp>-compname )
                                iv_type = |OBJ::{ lr_symbobjref->instancename }|
                                iv_typekind = <comp>-symbquick-typid
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
    ENDLOOP.
*      CATCH cx_sy_move_cast_error.
*    ENDTRY.
  ENDMETHOD.


  METHOD get_content.
    IF mt_external_content IS NOT INITIAL.
      rt_content = me->mt_external_content.
      RETURN.
    ENDIF.
    APPEND |{ COND string( WHEN mv_is_root EQ abap_true THEN space ELSE me->get_key( ) ) } = value #(| TO rt_content.
    LOOP AT me->mt_components ASSIGNING FIELD-SYMBOL(<comp>).
*      rt_content = value #( BASE rt_content ( <comp>->get_content( ) ) ).
*      APPEND LINES OF <comp>->get_content( ) TO rt_content.
      LOOP AT <comp>->get_content( i_max_line_length = i_max_line_length ) ASSIGNING FIELD-SYMBOL(<cont>).
        APPEND |{ get_indention( ) }{ <cont> }| TO rt_content.
      ENDLOOP.
    ENDLOOP.
    APPEND |)| TO rt_content.
  ENDMETHOD.


  METHOD get_node_type.
    rv_node_type = 'structured'.
  ENDMETHOD.
ENDCLASS.
