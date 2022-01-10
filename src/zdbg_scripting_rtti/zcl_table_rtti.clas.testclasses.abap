
CLASS ltc_table_rtti DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Table_Rtti
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_TABLE_RTTI
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
**********************************************************************
    TYPES: BEGIN OF t_simple,
             comp1 TYPE string,
             comp2 TYPE i,
           END OF t_simple.
*** Nested Table
    TYPES: BEGIN OF t_nested,
             nested_tab TYPE TABLE OF t_simple WITH EMPTY KEY,
           END OF t_nested.
*** Table of refrenes
    TYPES: BEGIN OF t_ref,
             ref TYPE REF TO string,
           END OF t_ref.
    TYPES: BEGIN OF t_scarr_spfli,
             carr  TYPE scarr,
             spfli TYPE TABLE OF spfli WITH EMPTY KEY,
           END OF t_scarr_spfli.

    DATA:
      f_cut TYPE REF TO zcl_table_rtti.  "class under test
    METHODS:
      setup,
      teardown.
    METHODS:
      test_simple_table FOR TESTING RAISING cx_static_check,
      test_simple_node_table FOR TESTING RAISING cx_static_check,
      test_nested_table FOR TESTING RAISING cx_static_check,
      test_ref_table FOR TESTING RAISING cx_static_check,
      test_cast_error FOR TESTING RAISING cx_static_check,
      test_subr_pool_nested FOR TESTING RAISING cx_static_check,
      get_node_type FOR TESTING RAISING cx_static_check.
**********************************************************************
*** helper methods:
    METHODS:
      init_from_object IMPORTING i_obj TYPE any i_key TYPE string DEFAULT 'object' RAISING cx_sy_move_cast_error,
      init_table_simple RAISING cx_sy_move_cast_error,
      init_table_ref RAISING cx_sy_move_cast_error,
      init_wrong_type RAISING cx_sy_move_cast_error,
      init_table_nested RAISING cx_sy_move_cast_error,
      init_from_select RAISING cx_sy_move_cast_error.
ENDCLASS.       "ltc_Table_Rtti


CLASS ltc_table_rtti IMPLEMENTATION.
  METHOD setup.
  ENDMETHOD.
  METHOD teardown.
  ENDMETHOD.
**********************************************************************
*** Methods for testing
  METHOD get_node_type.
    me->init_table_simple( ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->get_node_type( )
        exp                  = 'table'
    ).
  ENDMETHOD.
  METHOD test_simple_table.
    DATA: lt_compare TYPE TABLE OF string.
    TRY.
**********************************************************************
*** Init Instance -> Check key and Node-Type
        me->init_table_simple( ).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = f_cut->get_key( )
            exp                  = 'lt_simple'
        ).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = f_cut->get_node_type( )
            exp                  = 'table'
        ).
**********************************************************************
        lt_compare = VALUE #( ( | = value #(| )
                              ( |  (| )
                              ( |      comp1 = `one`| )
                              ( |      comp2 = 1| )
                              ( |  )| )
                              ( |  (| )
                              ( |      comp1 = `two`| )
                              ( |      comp2 = 2| )
                              ( |  )| )
                              ( |)| ) ).
        LOOP AT f_cut->get_content( ) ASSIGNING FIELD-SYMBOL(<cont>).
          cl_abap_unit_assert=>assert_equals(
            EXPORTING
              act                  = <cont>
              exp                  = lt_compare[ sy-tabix ]
          ).
        ENDLOOP.
      CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Error creating cut (test_simple_table)|
        ).
    ENDTRY.
  ENDMETHOD.
  METHOD test_nested_table.
    DATA: lt_compare TYPE TABLE OF string.
**********************************************************************
    TRY.
        me->init_table_nested( ).
*        CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
        lt_compare = VALUE #( ( | = value #(| )
                              ( |  (| )
                              ( |      nested_tab = value #(| )
                              ( |            (| )
                              ( |                    comp1 = `one`| )
                              ( |                    comp2 = 1| )
                              ( |            )| )
                              ( |            (| )
                              ( |                    comp1 = `two`| )
                              ( |                    comp2 = 2| )
                              ( |            )| )
                              ( |      )| )
                              ( |  )| )
                              ( |  (| )
                              ( |      nested_tab = value #(| )
                              ( |            (| )
                              ( |                    comp1 = `three`| )
                              ( |                    comp2 = 3| )
                              ( |            )| )
                              ( |            (| )
                              ( |                    comp1 = `four`| )
                              ( |                    comp2 = 4| )
                              ( |            )| )
                              ( |      )| )
                              ( |  )| )
                              ( |)| ) ).
**********************************************************************
        LOOP AT f_cut->get_content( ) ASSIGNING FIELD-SYMBOL(<cont>).
          cl_abap_unit_assert=>assert_equals(
            EXPORTING
              act                  = <cont>
              exp                  = lt_compare[ sy-tabix ]
          ).
        ENDLOOP.
      CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Error creating cut (test_simple_table)|
        ).
    ENDTRY.
  ENDMETHOD.
  METHOD test_ref_table.
    DATA: lt_compare TYPE TABLE OF string.
**********************************************************************
    TRY.
        me->init_table_ref( ).
*        CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
        lt_compare = VALUE #( ( | = value #(| )
                              ( |  (| )
                              ( |      ref = REF_FOR::string| )
                              ( |  )| )
                              ( |  (| )
                              ( |      ref = REF_FOR::string| )
                              ( |  )| )
                              ( |)| ) ).
**********************************************************************
        LOOP AT f_cut->get_content( ) ASSIGNING FIELD-SYMBOL(<cont>).
          cl_abap_unit_assert=>assert_equals(
            EXPORTING
              act                  = <cont>
              exp                  = lt_compare[ sy-tabix ]
          ).
        ENDLOOP.
      CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Error creating cut (test_simple_table)|
        ).
    ENDTRY.
  ENDMETHOD.
  METHOD test_cast_error.
    TRY.
        me->init_wrong_type( ).
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Wrong Test Type failed. No Exception was thrown|
        ).
      CATCH cx_sy_move_cast_error INTO DATA(err). " Oberklasse aller System-Exceptions
        cl_abap_unit_assert=>assert_bound(
          EXPORTING
            act              = err
        ).
    ENDTRY.
  ENDMETHOD.
  METHOD test_subr_pool_nested.
    DATA: subr_pool  TYPE TABLE OF string.
    DATA: compare TYPE TABLE OF t_scarr_spfli WITH EMPTY KEY.
    DATA: lt_scarr   TYPE TABLE OF t_scarr_spfli WITH EMPTY KEY.
    DATA: class TYPE string.
    TRY.
*        me->init_from_select( ).
        SELECT FROM scarr
             FIELDS *
         INTO TABLE @DATA(lt_scarr_).
        LOOP AT lt_scarr_ ASSIGNING FIELD-SYMBOL(<scarr>).
          SELECT FROM spfli
               FIELDS *
                WHERE carrid = @<scarr>-carrid
           INTO TABLE @DATA(lt_spfli).
          INSERT VALUE t_scarr_spfli( carr = <scarr> spfli = lt_spfli ) INTO TABLE lt_scarr.
        ENDLOOP.
        me->init_from_object( i_obj = lt_scarr ).
        subr_pool = VALUE #(
           ( |program.|                     )
           ( |class main definition.| )
           ( |  public section.|            )
           ( |   TYPES: BEGIN OF t_scarr_spfli,| )
           ( |            carr  TYPE scarr,| )
           ( |            spfli TYPE TABLE OF spfli WITH EMPTY KEY,| )
           ( |   END OF t_scarr_spfli.| )
           ( |   types: tt_scarr_spfli type table OF t_scarr_spfli with empty key.| )
           ( |    class-methods gen_object returning value(object) type tt_scarr_spfli.| )
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
          ( |  endmethod.|                 )
          ( |endclass.|                    ) ).

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
            object = compare.
**********************************************************************
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = compare
            exp                  = lt_scarr ).
      CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Error creating cut (test_simple_struct)|
        ).
    ENDTRY.
  ENDMETHOD.
  METHOD test_simple_node_table.
    DATA: lr_node_tab          TYPE REF TO zcl_entity=>tt_node_table_tmc,
          lr_search_index      TYPE REF TO zcl_entity=>tt_search_index,
          lr_search_index_cont TYPE REF TO zcl_entity=>tt_search_index_cont,
          lr_node_key          TYPE REF TO tm_nodekey.
**********************************************************************
    DATA: lr_nodetab_int TYPE REF TO treemcnota,
          lr_itemtab_int TYPE REF TO treemcitac,
          lv_dd_id       TYPE i.
**********************************************************************
    me->init_table_simple( ).
**********************************************************************
    lr_node_tab = NEW #( ).
    lr_search_index = NEW #( ).
    lr_search_index_cont = NEW #( ).
    lr_nodetab_int = NEW #( ).
    lr_itemtab_int = NEW #( ).
    lr_node_key = NEW #( ).
**********************************************************************
    f_cut->build_node_table(
      EXPORTING
        ir_node_tab          = lr_nodetab_int
        ir_item_tab          = lr_itemtab_int
        iv_parent_key        = space
        ir_current_key       = lr_node_key
        iv_dd_id             = CONV #( lv_dd_id )
        ir_node_table        = lr_node_tab
        ir_search_index      = lr_search_index
        ir_search_index_cont = lr_search_index_cont
    ).
**********************************************************************
*** Check-Node-Table Components
    LOOP AT ltc_test_values=>gt_node_tab_compare ASSIGNING FIELD-SYMBOL(<ntab_compare>).
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act                  = lr_node_tab->*[ node_key = <ntab_compare>-node_key ]-node_key
          exp                  = <ntab_compare>-node_key
      ).
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act                  = lr_node_tab->*[ node_key = <ntab_compare>-node_key ]-node_key_old
          exp                  = <ntab_compare>-node_key_old
      ).
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act                  = lr_node_tab->*[ node_key = <ntab_compare>-node_key ]-parent_node_key
          exp                  = <ntab_compare>-parent_node_key
      ).
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act                  = lr_node_tab->*[ node_key = <ntab_compare>-node_key ]-contains_ref
          exp                  = <ntab_compare>-contains_ref
      ).
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act                  = lr_node_tab->*[ node_key = <ntab_compare>-node_key ]-disabled
          exp                  = <ntab_compare>-disabled
      ).
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act                  = lr_node_tab->*[ node_key = <ntab_compare>-node_key ]-modified
          exp                  = <ntab_compare>-modified
      ).
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act                  = lr_node_tab->*[ node_key = <ntab_compare>-node_key ]-highlighted
          exp                  = <ntab_compare>-highlighted
      ).
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act                  = lr_node_tab->*[ node_key = <ntab_compare>-node_key ]-is_complex
          exp                  = <ntab_compare>-is_complex
      ).
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act                  = lr_node_tab->*[ node_key = <ntab_compare>-node_key ]-is_complex
          exp                  = <ntab_compare>-is_complex
      ).
    ENDLOOP.
**********************************************************************
*** Check Instances of Node-Table
    LOOP AT ltc_test_values=>gt_class_name_cast ASSIGNING FIELD-SYMBOL(<class_name>).
      DATA(l_class_name) = cl_abap_classdescr=>get_class_name( lr_node_tab->*[ node_key = <class_name>-nkey ]-entity ).
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act                  = l_class_name
          exp                  = <class_name>-class_name
      ).
    ENDLOOP.
  ENDMETHOD.
**********************************************************************
  METHOD init_from_object.
    IF f_cut IS BOUND. CLEAR f_cut. ENDIF.
    CREATE OBJECT f_cut
      EXPORTING
        iv_is_root  = abap_true
        io_data_ref = cl_abap_typedescr=>describe_by_data( i_obj )
        ir_tab      = REF #( i_obj )
        iv_key      = i_key
        iv_curr_ind = 0.
  ENDMETHOD.
  METHOD init_table_simple.
    DATA: lt_simple TYPE TABLE OF t_simple.
    lt_simple = VALUE #(
                  ( comp1 = `one` comp2 = 1 )
                  ( comp1 = `two` comp2 = 2 ) ).
    me->init_from_object( i_obj = lt_simple i_key = 'lt_simple' ).
*    CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
  ENDMETHOD.
  METHOD init_table_nested.
    DATA: nested_tab TYPE TABLE OF t_nested.
    nested_tab = VALUE #(
                    ( nested_tab = VALUE #(
                                  ( comp1 = `one` comp2 = 1 )
                                  ( comp1 = `two` comp2 = 2 ) ) )
                    ( nested_tab = VALUE #(
                                  ( comp1 = `three` comp2 = 3 )
                                  ( comp1 = `four` comp2 = 4 ) ) ) ).
    me->init_from_object( i_obj = nested_tab ).
*    CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
  ENDMETHOD.
  METHOD init_table_ref.
    DATA: ref_tab TYPE TABLE OF t_ref.
    ref_tab = VALUE #( ( ref = REF #( `one` ) ) ( ref = REF #( `two` ) ) ).
    me->init_from_object( ref_tab ).
*    CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
  ENDMETHOD.
  METHOD init_wrong_type.
    DATA: struc TYPE t_simple.
    struc = VALUE #( comp1 = `one` comp2 = 1 ).
**********************************************************************
    me->init_from_object( struc ).
*    CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
  ENDMETHOD.
  METHOD init_from_select.
    DATA: lt_scarr   TYPE TABLE OF t_scarr_spfli.
    SELECT FROM scarr
         FIELDS *
     INTO TABLE @DATA(lt_scarr_).
    LOOP AT lt_scarr_ ASSIGNING FIELD-SYMBOL(<scarr>).
      SELECT FROM spfli
           FIELDS *
            WHERE carrid = @<scarr>-carrid
       INTO TABLE @DATA(lt_spfli).
      INSERT VALUE t_scarr_spfli( carr = <scarr> spfli = lt_spfli ) INTO TABLE lt_scarr.
    ENDLOOP.
    me->init_from_object( i_obj = lt_scarr ).
*    CATCH cx_sy_move_cast_error. " Oberklasse aller System-Exceptions
  ENDMETHOD.
ENDCLASS.
