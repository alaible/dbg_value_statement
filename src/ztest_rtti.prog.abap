*&---------------------------------------------------------------------*
*& Report ZTEST_RTTI
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_rtti.
CLASS lcl_test_class DEFINITION.
ENDCLASS.

TYPES: BEGIN OF t_simple_struct,
         comp1    TYPE string,
         comp2    TYPE string,
         some_int TYPE i,
         obj      TYPE REF TO lcl_test_class,
         obj2     TYPE REF TO zcl_test_class,
         int_ref  TYPE REF TO i,
       END OF t_simple_struct.

TYPES: BEGIN OF t_nested,
         comp1  TYPE string,
         nested TYPE t_simple_struct,
       END OF t_nested.

TYPES: BEGIN OF t_scarr_splfi,
         carr  TYPE scarr,
         spfli TYPE TABLE OF spfli WITH EMPTY KEY,
       END OF t_scarr_splfi.

START-OF-SELECTION.
  DATA: ls_simple TYPE t_simple_struct,
        lt_simple TYPE TABLE OF t_simple_struct,
        ls_nested TYPE t_nested.
  DATA: lo_rtti_struct TYPE REF TO zcl_struct_rtti,
        lo_rtti_table  TYPE REF TO zcl_table_rtti.
  DATA: lt_tab     TYPE TABLE OF string,
        lt_ref_tab TYPE TABLE OF REF TO string,
        lt_but000  TYPE TABLE OF but000,
        lt_scarr   TYPE TABLE OF t_scarr_splfi.
**********************************************************************
  ls_simple = VALUE #( comp1 = 'eins' comp2 = 'zwei' obj = NEW #( ) some_int = 5 int_ref = REF #( 5 ) obj2 = NEW #( ) ).
  ls_nested = VALUE #( comp1 = 'outer_nested' nested = ls_simple ).
  lt_tab = VALUE #( ( |eins| ) ( |zwei| ) ).
  lt_simple = VALUE #( ( ls_simple ) ( ls_simple ) ).
  lt_ref_tab = VALUE #( ( REF #( `eins` ) ) ( REF #( `zwei` ) ) ).
  SELECT FROM but000 FIELDS * INTO TABLE @lt_but000.
  DATA(lo_type) = cl_abap_typedescr=>describe_by_data( p_data = ls_nested ).
  lo_rtti_struct = NEW #( io_data_ref = lo_type iv_is_root = abap_true ir_struc = REF #( ls_nested ) iv_key = 'ls_nested' iv_curr_ind = 0 ).
  lo_type = cl_abap_typedescr=>describe_by_data( lt_but000 ).
  lo_rtti_table = NEW #( io_data_ref = lo_type iv_is_root = abap_true ir_tab = REF #( lt_but000 ) iv_key = 'lt_but000' iv_curr_ind = 0 ).
*  lo_rtti_table = NEW #( io_data_ref = lo_type iv_is_root = abap_true ir_tab = REF #( lt_ref_tab ) iv_key = 'lt_ref_tab' ).
  CALL FUNCTION 'Z_DISPLAY_VALUE_STMT'
    EXPORTING
      ir_entity = lo_rtti_struct. " zcl_compl_entity
**********************************************************************
  SELECT FROM scarr
       FIELDS *
   INTO TABLE @DATA(lt_scarr_).
  LOOP AT lt_scarr_ ASSIGNING FIELD-SYMBOL(<scarr>).
    SELECT FROM spfli
         FIELDS spfli~*,
                'some_string' as inline
          WHERE carrid = @<scarr>-carrid
     INTO TABLE @DATA(lt_spfli).
    INSERT VALUE t_scarr_splfi( carr = <scarr> spfli = lt_spfli ) INTO TABLE lt_scarr.
  ENDLOOP.
  lo_type = cl_abap_typedescr=>describe_by_data( lt_scarr ).
  lo_rtti_table = NEW #( io_data_ref = lo_type iv_is_root = abap_true ir_tab = REF #( lt_scarr ) iv_key = 'lt_scarr' iv_curr_ind = 0 ).
  CALL FUNCTION 'Z_DISPLAY_VALUE_STMT'
    EXPORTING
      ir_entity = lo_rtti_table
      i_max_line_length = 100. " zcl_compl_entity

*  BREAK-POINT.
