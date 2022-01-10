CLASS zcl_struct_rtti DEFINITION
  PUBLIC
  INHERITING FROM zcl_compl_entity
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_is_root  TYPE abap_bool
        io_data_ref TYPE REF TO cl_abap_typedescr
        ir_struc    TYPE REF TO data
        iv_key      TYPE string
        iv_curr_ind TYPE i
      RAISING
        cx_sy_move_cast_error .

    METHODS: get_content REDEFINITION .
ENDCLASS.



CLASS ZCL_STRUCT_RTTI IMPLEMENTATION.


  METHOD constructor.
    DATA: lo_struct TYPE REF TO cl_abap_structdescr.
    DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lo_table_descr  TYPE REF TO cl_abap_tabledescr.
    DATA: lo_el_descr  TYPE REF TO cl_abap_elemdescr,
          lo_ref_descr TYPE REF TO cl_abap_refdescr,
          lo_obj_descr TYPE REF TO cl_abap_objectdescr.
    DATA: lo_inner_struct TYPE REF TO cl_abap_structdescr,
          lo_inner_table  TYPE REF TO cl_abap_tabledescr.
    FIELD-SYMBOLS: <comp_>  TYPE any,
                   <struc_> TYPE any.
**********************************************************************
    super->constructor( iv_is_root = iv_is_root iv_curr_ind = iv_curr_ind ).
**********************************************************************
    lo_struct ?= io_data_ref.
    ASSIGN ir_struc->* TO <struc_>.
*    me->mv_key = lo_struct->get_relative_name( ).
    me->mv_key = iv_key.
    LOOP AT lo_struct->components ASSIGNING FIELD-SYMBOL(<field>).
      ASSIGN COMPONENT <field>-name OF STRUCTURE <struc_> TO <comp_>.
      DATA(lo_inner_type) = cl_abap_typedescr=>describe_by_data( <comp_> ).
      TRY.
          lo_el_descr ?= lo_inner_type.
          mt_components = VALUE #( BASE mt_components
            ( NEW zcl_simple_struct(
                iv_key = CONV #( to_lower( <field>-name ) )
                iv_value = <comp_>
                iv_typekind = lo_inner_type->type_kind
                iv_indent = mv_indent_level
              )
            )
          ).
        CATCH cx_sy_move_cast_error.
          TRY.
              lo_ref_descr ?= lo_inner_type.
              mt_components = VALUE #( BASE mt_components
                                ( NEW zcl_ref_entity(
                                    iv_key = CONV #( to_lower( <field>-name ) )
                                    iv_type = |{ to_lower( lo_ref_descr->get_referenced_type( )->get_relative_name( ) ) }|
                                    iv_typekind = lo_inner_type->type_kind
                                    iv_indent = mv_indent_level
                                  )
                                )
                              ).
            CATCH cx_sy_move_cast_error.
              TRY.
                  lo_inner_struct ?= lo_inner_type.
                  mt_components = VALUE #( BASE mt_components
                                    ( NEW zcl_struct_rtti(
                                        iv_is_root  = abap_false
                                        io_data_ref = lo_inner_type
                                        ir_struc    = REF #( <comp_> )
                                        iv_key      = CONV #( to_lower( <field>-name ) )
                                        iv_curr_ind = mv_indent_level
                                      )
                                    )
                                  ).
                CATCH cx_sy_move_cast_error.
                  TRY.
                      lo_inner_table ?= lo_inner_type.
                      mt_components = VALUE #( BASE mt_components
                                        ( NEW zcl_table_rtti(
                                            iv_is_root  = abap_false
                                            io_data_ref = lo_inner_type
                                            ir_tab      = REF #( <comp_> )
                                            iv_key      = CONV #( to_lower( <field>-name ) )
                                            iv_curr_ind = mv_indent_level
                                          )
                                         )
                                        ).
                    CATCH cx_sy_move_cast_error.
                  ENDTRY.
              ENDTRY.
          ENDTRY.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_content.
    IF mt_external_content IS NOT INITIAL.
      rt_content = me->mt_external_content.
      RETURN.
    ENDIF.
    APPEND |{ COND string( WHEN mv_is_root EQ abap_true THEN space ELSE me->get_key( ) ) } = value #(| TO rt_content.
*    APPEND |{ get_indention( ) }INDENT_LEVEL = { mv_indent_level }| TO rt_content.
    LOOP AT me->mt_components ASSIGNING FIELD-SYMBOL(<comp>).
*      rt_content = value #( BASE rt_content ( <comp>->get_content( ) ) ).
*      APPEND LINES OF <comp>->get_content( ) TO rt_content.
      LOOP AT <comp>->get_content( i_max_line_length = i_max_line_length ) ASSIGNING FIELD-SYMBOL(<cont>).
        APPEND |{ get_indention( ) }{ <cont> }| TO rt_content.
*        APPEND |{ <cont> }| TO rt_content.
      ENDLOOP.
    ENDLOOP.
    APPEND |)| TO rt_content.
  ENDMETHOD.
ENDCLASS.
