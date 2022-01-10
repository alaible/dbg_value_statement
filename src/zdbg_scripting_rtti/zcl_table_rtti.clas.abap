CLASS zcl_table_rtti DEFINITION PUBLIC CREATE PUBLIC INHERITING FROM zcl_compl_entity.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_is_root  TYPE abap_bool
                            io_data_ref TYPE REF TO cl_abap_typedescr
                            ir_tab      TYPE REF TO data
                            iv_key      TYPE string
                            iv_curr_ind TYPE i
                  RAISING   cx_sy_move_cast_error,
      get_content REDEFINITION,
      get_node_type REDEFINITION.
ENDCLASS.



CLASS ZCL_TABLE_RTTI IMPLEMENTATION.


  METHOD constructor.
    DATA: lo_table_descr TYPE REF TO cl_abap_tabledescr,
          lo_line_struct TYPE REF TO cl_abap_structdescr,
          lo_line_elem   TYPE REF TO cl_abap_elemdescr,
          lo_ref_elem    TYPE REF TO cl_abap_refdescr.
    DATA: l_type TYPE string.
    FIELD-SYMBOLS: <table> TYPE ANY TABLE,
                   <line>  TYPE any.
**********************************************************************
    super->constructor( iv_is_root = iv_is_root iv_curr_ind = iv_curr_ind ).
**********************************************************************
    lo_table_descr ?= io_data_ref.
    ASSIGN ir_tab->* TO <table>.
    me->mv_key = iv_key.
    TRY.
        lo_line_struct ?= lo_table_descr->get_table_line_type( ).
        LOOP AT <table> ASSIGNING <line>.
          mt_components = VALUE #( BASE mt_components
                                    ( NEW zcl_tableline_rtti(
                                        iv_is_root  = abap_false
                                        io_data_ref = lo_line_struct
                                        ir_struc    = REF #( <line> )
                                        iv_key = |{ mv_key }[{ sy-tabix }]|
                                        iv_curr_ind = mv_indent_level
                                      )
                                    )
                                 ).
        ENDLOOP.
      CATCH cx_sy_move_cast_error.
        TRY.
            lo_line_elem ?= lo_table_descr->get_table_line_type( ).
            LOOP AT <table> ASSIGNING <line>.
              mt_components = VALUE #( BASE mt_components
                                        ( NEW zcl_simple_table(
                                             iv_value     = <line>
                                             iv_comp_name = mv_key
                                             iv_tab_idx   = sy-tabix
                                             iv_typekind  = lo_line_elem->type_kind
                                             iv_indent = mv_indent_level
                                          )
                                        )
                                     ).
            ENDLOOP.
          CATCH cx_sy_move_cast_error.
            lo_ref_elem ?= lo_table_descr->get_table_line_type( ).
            LOOP AT <table> ASSIGNING <line>.
              TRY.
                  DATA(class_n) = cl_abap_classdescr=>get_class_name( <line> ).
                  l_type = class_n.
                CATCH cx_root.
                  l_type = lo_ref_elem->get_referenced_type( )->get_relative_name( ).
              ENDTRY.
              mt_components = VALUE #( BASE mt_components
                                        ( NEW zcl_simple_ref_table(
                                             iv_comp_name = mv_key
                                             iv_tab_idx   = sy-tabix
                                             iv_type      = l_type
                                             iv_typekind  = lo_ref_elem->type_kind
                                             iv_indent    = mv_indent_level
                                          )
                                        )
                                     ).
            ENDLOOP.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.


  METHOD get_content.
    IF mt_external_content IS NOT INITIAL.
      rt_content = me->mt_external_content.
      RETURN.
    ENDIF.
*** leere Tabellen gesondert ausgeben, da sonst eine leeres Element fehlt
    IF lines( mt_components ) EQ 0.
*      APPEND |{ get_indention( ) }INDENT_LEVEL = { mv_indent_level }| TO rt_content.
      APPEND |{ COND string( WHEN mv_is_root EQ abap_true THEN space ELSE me->get_key( ) ) } = value #( )| TO rt_content.
      RETURN.
    ENDIF.
    APPEND |{ COND string( WHEN mv_is_root EQ abap_true THEN space ELSE me->get_key( ) ) } = value #(| TO rt_content.
*    APPEND |{ get_indention( ) }INDENT_LEVEL = { mv_indent_level }| TO rt_content.
    LOOP AT mt_components ASSIGNING FIELD-SYMBOL(<comp>).
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
