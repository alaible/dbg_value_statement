*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS ltc_test_values DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_nkey_classname,
             nkey       TYPE tm_nodekey,
             class_name TYPE string,
           END OF t_nkey_classname.
    CLASS-METHODS: class_constructor.
    CLASS-DATA: gt_node_tab_compare TYPE zcl_value_entity=>tt_node_table_tmc,
                gt_class_name_cast  TYPE SORTED TABLE OF t_nkey_classname WITH UNIQUE KEY nkey.
ENDCLASS.

CLASS ltc_test_values IMPLEMENTATION.
  METHOD class_constructor.
**********************************************************************
*** gt_node_tab_compare
    gt_node_tab_compare = VALUE zcl_value_entity=>tt_node_table_tmc(  (
                                        node_key = '1'
                                        node_key_old = ''
*                                        entity = REF_FOR::OBJ::{O:20*\CLASS=ZCL_TABLE_RTTI}
                                        parent_node_key = ''
                                        contains_ref = ''
                                        disabled = ''
                                        modified = ''
                                        highlighted = ''
                                        is_complex = 'X'
                                        vis_filtered = ''
                                    )
                                    (
                                        node_key = '2'
                                        node_key_old = ''
*                                        entity = REF_FOR::OBJ::{O:9*\CLASS=ZCL_TABLELINE_RTTI}
                                        parent_node_key = '1'
                                        contains_ref = ''
                                        disabled = ''
                                        modified = ''
                                        highlighted = ''
                                        is_complex = 'X'
                                        vis_filtered = ''
                                    )
                                    (
                                        node_key = '3'
                                        node_key_old = ''
*                                        entity = REF_FOR::OBJ::{O:6*\CLASS=ZCL_SIMPLE_STRUCT}
                                        parent_node_key = '2'
                                        contains_ref = ''
                                        disabled = ''
                                        modified = ''
                                        highlighted = ''
                                        is_complex = ''
                                        vis_filtered = ''
                                    )
                                    (
                                        node_key = '4'
                                        node_key_old = ''
*                                        entity = REF_FOR::OBJ::{O:25*\CLASS=ZCL_SIMPLE_STRUCT}
                                        parent_node_key = '2'
                                        contains_ref = ''
                                        disabled = ''
                                        modified = ''
                                        highlighted = ''
                                        is_complex = ''
                                        vis_filtered = ''
                                    )
                                    (
                                        node_key = '5'
                                        node_key_old = ''
*                                        entity = REF_FOR::OBJ::{O:26*\CLASS=ZCL_TABLELINE_RTTI}
                                        parent_node_key = '1'
                                        contains_ref = ''
                                        disabled = ''
                                        modified = ''
                                        highlighted = ''
                                        is_complex = 'X'
                                        vis_filtered = ''
                                    )
                                    (
                                        node_key = '6'
                                        node_key_old = ''
*                                        entity = REF_FOR::OBJ::{O:27*\CLASS=ZCL_SIMPLE_STRUCT}
                                        parent_node_key = '5'
                                        contains_ref = ''
                                        disabled = ''
                                        modified = ''
                                        highlighted = ''
                                        is_complex = ''
                                        vis_filtered = ''
                                    )
                                    (
                                        node_key = '7'
                                        node_key_old = ''
*                                        entity = REF_FOR::OBJ::{O:28*\CLASS=ZCL_SIMPLE_STRUCT}
                                        parent_node_key = '5'
                                        contains_ref = ''
                                        disabled = ''
                                        modified = ''
                                        highlighted = ''
                                        is_complex = ''
                                        vis_filtered = ''
                                    )
                                  ).

    gt_class_name_cast = VALUE #(
                            ( nkey = '1' class_name = |\\CLASS=ZCL_TABLE_RTTI| )
                            ( nkey = '2' class_name = |\\CLASS=ZCL_TABLELINE_RTTI| )
                            ( nkey = '3' class_name = |\\CLASS=ZCL_SIMPLE_STRUCT| )
                            ( nkey = '4' class_name = |\\CLASS=ZCL_SIMPLE_STRUCT| )
                            ( nkey = '5' class_name = |\\CLASS=ZCL_TABLELINE_RTTI| )
                            ( nkey = '6' class_name = |\\CLASS=ZCL_SIMPLE_STRUCT| )
                            ( nkey = '7' class_name = |\\CLASS=ZCL_SIMPLE_STRUCT| ) ).
  ENDMETHOD.
ENDCLASS.
