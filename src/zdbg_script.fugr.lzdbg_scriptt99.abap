*&---------------------------------------------------------------------*
*& Include          LZDBG_SCRIPTT99
*&---------------------------------------------------------------------*
CLASS lcl_test_class DEFINITION.
ENDCLASS.
CLASS ltc_column_tree_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
**********************************************************************
*** Hilfs-typen
    TYPES: BEGIN OF t_st_with_ref,
             comp1    TYPE string,
             comp2    TYPE string,
             some_int TYPE i,
             obj      TYPE REF TO lcl_test_class,
             int_ref  TYPE REF TO i,
           END OF t_st_with_ref.
    TYPES: BEGIN OF t_nested_,
             outer  TYPE string,
             nested TYPE t_st_with_ref,
           END OF t_nested_.
**********************************************************************
    DATA: f_cut TYPE REF TO lcl_screen_objects_col.
    METHODS:
      init_with_root_ref RAISING cx_sy_move_cast_error,
      test_add_root_node FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_column_tree_test IMPLEMENTATION.

  METHOD test_add_root_node.
    TRY.
        me->init_with_root_ref( ).
**********************************************************************
        f_cut->init_tree_tables( ).
        LOOP AT f_cut->mr_node_table_tmc->* ASSIGNING FIELD-SYMBOL(<node_tab_entry>).
          CLEAR <node_tab_entry>-entity.
          cl_abap_unit_assert=>assert_equals(
            EXPORTING
              act                  = <node_tab_entry>
              exp                  = ltc_test_data=>node_tab_tmc[ node_key = <node_tab_entry>-node_key ]
          ).
        ENDLOOP.
        LOOP AT f_cut->mr_search_index->* ASSIGNING FIELD-SYMBOL(<search_index>).
          cl_abap_unit_assert=>assert_equals(
            EXPORTING
              act                  = <search_index>
              exp                  = ltc_test_data=>search_index_comp[ node_key = <search_index>-node_key ]
          ).
        ENDLOOP.
        LOOP AT f_cut->mr_search_index_cont->* ASSIGNING FIELD-SYMBOL(<search_index_c>).
          cl_abap_unit_assert=>assert_equals(
            EXPORTING
              act                  = condense( <search_index_c>-search_term_w_cont )
*              act                  = <search_index_c>-search_term_w_cont
              exp                  = ltc_test_data=>search_index_cont_comp[ node_key = <search_index_c>-node_key ]-search_term_w_cont
          ).
        ENDLOOP.
        LOOP AT f_cut->mr_nodetab_int->* ASSIGNING FIELD-SYMBOL(<nodetab>).
          cl_abap_unit_assert=>assert_equals(
            EXPORTING
              act                  = <nodetab>
              exp                  = ltc_test_data=>node_tab_comp[ sy-tabix ]
          ).
        ENDLOOP.
        LOOP AT f_cut->mr_itemtab_int->* ASSIGNING FIELD-SYMBOL(<itemtab>).
          cl_abap_unit_assert=>assert_equals(
            EXPORTING
              act                  = <itemtab>
              exp                  = ltc_test_data=>item_tab_comp[ sy-tabix ]
          ).
        ENDLOOP.
      CATCH cx_sy_move_cast_error. " Superclass for All System Exceptions .
        cl_abap_unit_assert=>fail( 'Error initializing root entity' ).
    ENDTRY.
  ENDMETHOD.

  METHOD init_with_root_ref.
    DATA: rtti_struct TYPE REF TO zcl_struct_rtti.
    DATA: st_nested TYPE t_nested_.
**********************************************************************
    st_nested = VALUE #(
                  outer = `outer`
                  nested = VALUE #(
                              comp1 = `one`
                              comp2 = `two`
                              obj = NEW #( )
                              some_int = 5
                              int_ref = REF #( 5 )
                  )
                ).

    CREATE OBJECT rtti_struct
      EXPORTING
        iv_is_root  = abap_true
        io_data_ref = cl_abap_typedescr=>describe_by_data( st_nested )
        ir_struc    = REF #( st_nested )
        iv_key      = 'st_with_ref'
        iv_curr_ind = 0.
    CREATE OBJECT f_cut.
    f_cut->set_root_ref( rtti_struct ).
  ENDMETHOD.

ENDCLASS.
