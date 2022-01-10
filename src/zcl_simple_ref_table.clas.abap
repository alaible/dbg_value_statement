CLASS zcl_simple_ref_table DEFINITION
  PUBLIC
  INHERITING FROM zcl_simple_table
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_comp_name TYPE string
                  iv_tab_idx   TYPE i
                  iv_type      TYPE string
                  iv_typekind  TYPE c
                  iv_indent    TYPE i.
    METHODS: contains_reference REDEFINITION,
      get_content REDEFINITION,
      get_node_type REDEFINITION.
  PROTECTED SECTION.
    DATA: mv_type TYPE string.

ENDCLASS.



CLASS ZCL_SIMPLE_REF_TABLE IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
      EXPORTING
        iv_value     = space
        iv_comp_name = iv_comp_name
        iv_tab_idx   = iv_tab_idx
        iv_typekind  = iv_typekind
        iv_indent    = iv_indent
    ).
    me->mv_type = iv_type.
  ENDMETHOD.


  METHOD contains_reference.
    rv_contains_ref = abap_true.
  ENDMETHOD.


  METHOD get_content.
    rt_content = VALUE #( ( |( REF_FOR::{ me->mv_type } )| ) ).
  ENDMETHOD.


  METHOD get_node_type.
    rv_node_type = 'reference'.
  ENDMETHOD.
ENDCLASS.
