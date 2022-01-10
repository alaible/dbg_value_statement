CLASS zcl_simple_table DEFINITION PUBLIC INHERITING FROM zcl_simple_entity CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_value     TYPE any
                  iv_comp_name TYPE string
                  iv_tab_idx   TYPE i
                  iv_typekind  TYPE c
                  iv_indent    type i,
      get_content REDEFINITION.
  PROTECTED SECTION.
    DATA: mv_comp_name TYPE string,
          mv_index     TYPE i.
ENDCLASS.



CLASS ZCL_SIMPLE_TABLE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_typekind = iv_typekind iv_indent = iv_indent ).
*    DATA(lo_elelemnt_descr) = cl_abap_typedescr=>describe_by_data( iv_value ).
*    mv_key = iv_key.
    mv_simple_value = iv_value.
    mv_comp_name = iv_comp_name.
    mv_index = iv_tab_idx.
*    mv_indent_level = iv_indent.
    me->mv_key = |{ mv_comp_name }[{ mv_index }]|.
  ENDMETHOD.


  METHOD get_content.
*    rt_content = VALUE #( ( |( '{ me->mv_simple_value }' )| ) ).
*    rt_content = VALUE #( ( |( { me->get_payload_f_typekind( ) } )| ) ).
    DATA(as_table) = me->get_payload_f_typekind( i_max_line_size = i_max_line_length i_plus_left_offset = 2 ).
    CASE lines( as_table ).
      WHEN 1.
        rt_content = VALUE #( ( |( { as_table[ 1 ] } )| ) ).
      WHEN 2.
        rt_content = VALUE #( ( |( { as_table[ 1 ] }| ) ).
        rt_content = VALUE #( BASE rt_content ( |{ as_table[ 2 ] } )| ) ).
      WHEN OTHERS.
        LOOP AT as_table ASSIGNING FIELD-SYMBOL(<line>).
          CASE sy-tabix.
            WHEN 1.
              rt_content = VALUE #( ( |( { as_table[ sy-tabix ] }| ) ).
            WHEN lines( as_table ).
              rt_content = VALUE #( BASE rt_content ( |{ as_table[ sy-tabix ] } )| ) ).
            WHEN OTHERS.
              rt_content = VALUE #( BASE rt_content ( as_table[ sy-tabix ] ) ).
          ENDCASE.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
