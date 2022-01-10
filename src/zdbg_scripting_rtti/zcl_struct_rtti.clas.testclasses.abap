
CLASS ltc_struct_rtti DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Struct_Rtti
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_STRUCT_RTTI
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PUBLIC SECTION.
**********************************************************************
    TYPES: BEGIN OF t_numeric,
             int     TYPE i,
             int8    TYPE int8,
             packed  TYPE p LENGTH 3 DECIMALS 1,
             decf_16 TYPE decfloat16,
             decf_34 TYPE decfloat34,
           END OF t_numeric.
  PRIVATE SECTION.
**********************************************************************
    TYPES: BEGIN OF t_simple,
             comp1 TYPE string,
             comp2 TYPE i,
           END OF t_simple.
**********************************************************************
    TYPES: BEGIN OF t_simple_nested,
             nested TYPE t_simple,
             comp1  TYPE string,
           END OF t_simple_nested.
*********************************************************************
    TYPES: BEGIN OF t_st_with_ref,
             comp1    TYPE string,
             comp2    TYPE string,
             some_int TYPE i,
             obj      TYPE REF TO lcl_test_class,
*             obj2     TYPE REF TO zcl_test_class,
             int_ref  TYPE REF TO i,
           END OF t_st_with_ref.
**********************************************************************
    TYPES: BEGIN OF t_nested,
             outer  TYPE string,
             nested TYPE t_st_with_ref,
           END OF t_nested.
    DATA:
      f_cut TYPE REF TO zcl_struct_rtti.  "class under test
    METHODS:
      test_subr_pool FOR TESTING RAISING cx_static_check,
      test_subr_pool_nested FOR TESTING RAISING cx_static_check,
      test_subr_pool_numeric FOR TESTING RAISING cx_static_check,
      test_simple_struct FOR TESTING RAISING cx_static_check,
      test_nested FOR TESTING RAISING cx_static_check,
      test_with_ref FOR TESTING RAISING cx_static_check.
    METHODS:
      init_from_object IMPORTING i_obj TYPE any RAISING cx_sy_move_cast_error,
      init_struct_simple RAISING cx_sy_move_cast_error,
      init_struct_with_ref RAISING cx_sy_move_cast_error,
      init_struct_nested RAISING cx_sy_move_cast_error.
ENDCLASS.       "ltc_Struct_Rtti


CLASS ltc_struct_rtti IMPLEMENTATION.

  METHOD test_simple_struct.
    DATA: compare TYPE TABLE OF string.
**********************************************************************
    compare = VALUE #( ( | = value #(| )
                       ( |  comp1 = `one`| )
                       ( |  comp2 = 1| )
                       ( |)| ) ).
**********************************************************************
    TRY.
        me->init_struct_simple( ).
        LOOP AT f_cut->get_content( ) ASSIGNING FIELD-SYMBOL(<cont>).
          cl_abap_unit_assert=>assert_equals(
            EXPORTING
              act                  = <cont>
              exp                  = compare[ sy-tabix ]
          ).
        ENDLOOP.
      CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Error creating cut (test_simple_struct)|
        ).
    ENDTRY.
  ENDMETHOD.
  METHOD test_subr_pool.
    DATA: subr_pool  TYPE TABLE OF string.
    DATA: simple_st TYPE t_simple,
          l_compare TYPE t_simple.
    DATA: class TYPE string,
          type_descr type ref to cl_abap_structdescr.
**********************************************************************
    simple_st = VALUE #( comp1 = `one` comp2 = 1 ).
**********************************************************************
    TRY.
        me->init_from_object( simple_st ).
        type_descr ?= cl_abap_typedescr=>describe_by_data( simple_st ).
        subr_pool = VALUE #(
           ( |program.|                     )
           ( |class main definition.| )
           ( |  public section.|            )
           ( |    TYPES: BEGIN OF t_simple,| )
           ( |            comp1 TYPE string,| )
           ( |            comp2 TYPE i,| )
           ( |           END OF t_simple.| )
           ( |    class-methods: gen_object returning value(object) type t_simple.| )
*           ( |                   gen_object_ref importing ir_ref type ref to data changing c_data type any.| )
           ( |endclass.|                    )
           ( |class main implementation.|   )
           ( |  method gen_object.|               ) ).
**********************************************************************
        DATA(cont) = f_cut->get_content( ).
        LOOP AT cont  ASSIGNING FIELD-SYMBOL(<cont>).
          CASE sy-tabix.
            WHEN 1.
              APPEND |object { <cont> }| TO subr_pool.
            WHEN lines( cont ).
              APPEND |{ <cont> }.| TO subr_pool.
            WHEN OTHERS.
              APPEND <cont> TO subr_pool.
          ENDCASE.
        ENDLOOP.
**********************************************************************
        subr_pool = VALUE #( BASE subr_pool
          ( |  endmethod.|                 ) ).
*          ( |  method gen_object_ref.| )
*          ( |  data: l_data type ref to data.| )
*          ( |  field-symbols <data> type any.| )
*          ( |  assign ir_ref->* to <data>.| )
*          ( |  data: l_type type string.| )
*          ( |  l_type = '{ condense( type_descr->absolute_name ) }'. | )
*          ( |  create data l_data type (l_type). | )
*          ( |  l_data->* = value #( ). | ) ).
*        LOOP AT cont  ASSIGNING <cont>.
*          CASE sy-tabix.
*            WHEN 1.
*              APPEND |l_data { <cont> }| TO subr_pool.
*            WHEN lines( cont ).
*              APPEND |{ <cont> }.| TO subr_pool.
*            WHEN OTHERS.
*              APPEND <cont> TO subr_pool.
*          ENDCASE.
*        ENDLOOP.
        subr_pool = VALUE #( BASE subr_pool
*          ( |  endmethod.| )
          ( |endclass.| ) ).

        GENERATE SUBROUTINE POOL subr_pool NAME DATA(prog).
        IF sy-subrc <> 0.
          cl_abap_unit_assert=>fail(
            EXPORTING
              msg    = |Could not generate subroutine pool|
          ).
        ENDIF.
        class = `\PROGRAM=` && prog && `\CLASS=MAIN`.
        CALL METHOD (class)=>gen_object
          RECEIVING
            object = l_compare.
**********************************************************************
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = l_compare
            exp                  = simple_st ).
        DATA: l_compare_ TYPE t_simple.
*        CALL METHOD (class)=>gen_object_ref
*          EXPORTING
*            ir_ref = REF t_simple( l_compare_ )
*          CHANGING
*            c_data = l_compare_.
      CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Error creating cut (test_simple_struct)|
        ).
    ENDTRY.
  ENDMETHOD.
  METHOD test_subr_pool_nested.
  ENDMETHOD.
  METHOD test_subr_pool_numeric.
    DATA: l_numeric TYPE t_numeric.
    DATA: subr_pool  TYPE TABLE OF string.
    DATA: l_compare TYPE t_numeric.
    DATA: class TYPE string.
**********************************************************************
    l_numeric = VALUE #( int = 2147483647 int8 = 2147483648 packed = '1234.123' decf_16 = '123456789.1234567891234567' decf_34 = '123456789.1234567891234567' ).
    TRY.
        me->init_from_object( l_numeric ).
        subr_pool = VALUE #(
           ( |program.|                     )
           ( |class main2 definition.| )
           ( |  public section.|            )
           ( |   TYPES: BEGIN OF t_numeric,| )
           ( |              int     TYPE i,| )
           ( |              int8    TYPE int8,| )
           ( |              packed  TYPE p LENGTH 3 DECIMALS 1,| )
           ( |              decf_16 TYPE decfloat16,| )
           ( |              decf_34 TYPE decfloat34,| )
           ( |          END OF t_numeric.| )
           ( |    class-methods gen_object returning value(object) type t_numeric.| )
           ( |endclass.|                    )
           ( |class main2 implementation.|   )
           ( |  method gen_object.|               ) ).
**********************************************************************
        DATA(cont) = f_cut->get_content( ).
        LOOP AT cont  ASSIGNING FIELD-SYMBOL(<cont>).
          CASE sy-tabix.
            WHEN 1.
              APPEND |object { <cont> }| TO subr_pool.
            WHEN lines( cont ).
              APPEND |{ <cont> }.| TO subr_pool.
            WHEN OTHERS.
              APPEND <cont> TO subr_pool.
          ENDCASE.
        ENDLOOP.
**********************************************************************
        subr_pool = VALUE #( BASE subr_pool
          ( |  endmethod.|                 )
          ( |endclass.|                    ) ).

        GENERATE SUBROUTINE POOL subr_pool NAME DATA(prog).
        IF sy-subrc <> 0.
          cl_abap_unit_assert=>fail(
            EXPORTING
              msg    = |Could not generate subroutine pool|
          ).
        ENDIF.
        class = `\PROGRAM=` && prog && `\CLASS=MAIN2`.
        CALL METHOD (class)=>gen_object
          RECEIVING
            object = l_compare.
**********************************************************************
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = l_compare
            exp                  = l_numeric ).
      CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Error creating cut (test_simple_struct)|
        ).
    ENDTRY.
  ENDMETHOD.
  METHOD test_nested.
    DATA: compare TYPE TABLE OF string.
**********************************************************************
    compare = VALUE #( ( | = value #(| )
                       ( |  outer = `outer`| )
                       ( |  nested = value #(| )
                       ( |      comp1 = `one`| )
                       ( |      comp2 = `two`| )
                       ( |      some_int = 5| )
                       ( |      obj = REF_FOR::lcl_test_class| )
                       ( |      int_ref = REF_FOR::i| )
                       ( |  )| )
                       ( |)| ) ).
**********************************************************************
    TRY.
        me->init_struct_nested( ).
        LOOP AT f_cut->get_content( ) ASSIGNING FIELD-SYMBOL(<cont>).
          cl_abap_unit_assert=>assert_equals(
            EXPORTING
              act                  = <cont>
              exp                  = compare[ sy-tabix ]
          ).
        ENDLOOP.
      CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Error creating cut (test_nested_struct)|
        ).
    ENDTRY.
  ENDMETHOD.
  METHOD test_with_ref.
    DATA: compare TYPE TABLE OF string.
**********************************************************************
    compare = VALUE #( ( | = value #(| )
                       ( |  comp1 = `one`| )
                       ( |  comp2 = `two`| )
                       ( |  some_int = 5| )
                       ( |  obj = REF_FOR::lcl_test_class| )
                       ( |  int_ref = REF_FOR::i| )
                       ( |)| ) ).
**********************************************************************
    TRY.
        me->init_struct_with_ref( ).
        LOOP AT f_cut->get_content( ) ASSIGNING FIELD-SYMBOL(<cont>).
          cl_abap_unit_assert=>assert_equals(
            EXPORTING
              act                  = <cont>
              exp                  = compare[ sy-tabix ]
          ).
        ENDLOOP.
      CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Error creating cut (test_ref_struct)|
        ).
    ENDTRY.
  ENDMETHOD.
  METHOD init_from_object.
    IF f_cut IS BOUND. CLEAR f_cut. ENDIF.
    CREATE OBJECT f_cut
      EXPORTING
        iv_is_root  = abap_true
        io_data_ref = cl_abap_typedescr=>describe_by_data( i_obj )
        ir_struc    = REF #( i_obj )
        iv_key      = 'object'
        iv_curr_ind = 0.
  ENDMETHOD.
  METHOD init_struct_simple.
    DATA: simple_st TYPE t_simple.
**********************************************************************
    simple_st = VALUE #( comp1 = `one` comp2 = 1 ).
    me->init_from_object( simple_st ).
*    CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
  ENDMETHOD.

  METHOD init_struct_with_ref.
    DATA: st_with_ref TYPE t_st_with_ref.
**********************************************************************
    st_with_ref = VALUE #( comp1 = `one` comp2 = `two` obj = NEW #( ) some_int = 5 int_ref = REF #( 5 ) ).
    me->init_from_object( st_with_ref ).
*    CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
  ENDMETHOD.
  METHOD init_struct_nested.
    DATA: st_nested TYPE t_nested.
**********************************************************************
    st_nested = VALUE #( outer = `outer`
                         nested = VALUE #( comp1 = `one` comp2 = `two` obj = NEW #( ) some_int = 5 int_ref = REF #( 5 ) ) ).
    me->init_from_object( st_nested ).
*    CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
  ENDMETHOD.

ENDCLASS.
