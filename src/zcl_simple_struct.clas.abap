CLASS zcl_simple_struct DEFINITION PUBLIC INHERITING FROM zcl_simple_entity CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_key      TYPE string
                  iv_value    TYPE any
                  iv_typekind TYPE c
                  iv_indent   TYPE i,
      get_content REDEFINITION.

ENDCLASS.



CLASS ZCL_SIMPLE_STRUCT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_typekind = iv_typekind iv_indent = iv_indent ).
    DATA(lo_elelemnt_descr) = cl_abap_typedescr=>describe_by_data( iv_value ).
    mv_key = iv_key.
    mv_simple_value = iv_value.
  ENDMETHOD.


  METHOD get_content.
*    rt_content = VALUE #( ( |{ me->mv_key } = '{ me->mv_simple_value }'| ) ).
*    DATA(left_offset) = strlen( |{ me->get_indention( ) }{ me->mv_key } = | ).
*    DATA(full_len) = |{ me->mv_key } = { me->get_payload_f_typekind( ) }|.
*    IF full_len > i_max_line_length.
*    ELSE.
*      rt_content = VALUE #( ( full_len ) ).
*    ENDIF.
*    rt_content = VALUE #( ( |{ me->get_payload_f_typekind( i_left = |{ me->mv_key } = | ) }| ) ).
    IF to_lower( mv_key ) EQ 'mandt'.
      APPEND |{ me->mv_key } = sy-mandt| TO rt_content.
    ELSE.
      APPEND LINES OF me->get_payload_f_typekind( i_left = |{ me->mv_key } = | i_max_line_size = i_max_line_length ) TO rt_content.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
