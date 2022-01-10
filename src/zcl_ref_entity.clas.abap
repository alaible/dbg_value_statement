CLASS zcl_ref_entity DEFINITION PUBLIC INHERITING FROM zcl_simple_entity CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_key TYPE string
                  iv_type TYPE string
                  iv_typekind type c
                  iv_indent type i,
      get_node_type REDEFINITION,
      contains_reference REDEFINITION,
      get_content REDEFINITION.
  PROTECTED SECTION.
    DATA: mv_type TYPE string.
ENDCLASS.



CLASS ZCL_REF_ENTITY IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_typekind = iv_typekind iv_indent = iv_indent ).
    me->mv_key = iv_key.
    me->mv_type = iv_type.
  ENDMETHOD.


  METHOD contains_reference.
    rv_contains_ref = abap_true.
  ENDMETHOD.


  METHOD get_content.
    rt_content = VALUE #( ( |{ me->mv_key } = REF_FOR::{ me->mv_type }| ) ).
  ENDMETHOD.


  METHOD get_node_type.
    rv_node_type = 'reference'.
  ENDMETHOD.
ENDCLASS.
