*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_test_class DEFINITION.
ENDCLASS.

CLASS ltc_test_values DEFINITION.
  PUBLIC SECTION.
      TYPES: BEGIN OF t_simple_,
             comp1 TYPE string,
             comp2 TYPE i,
           END OF t_simple_.
**********************************************************************
    CLASS-METHODS:
      class_constructor.
    CLASS-DATA: node_tab_tmc TYPE zcl_value_entity=>tt_node_table_tmc.

ENDCLASS.

CLASS ltc_test_values IMPLEMENTATION.
  METHOD class_constructor.
    node_tab_tmc = VALUE #(
                    (
                        node_key = `1`
                        node_key_old = ``
*                        entity = REF_FOR::OBJ::{O:184*\CLASS=ZCL_STRUCT_RTTI}
                        parent_node_key = ``
                        contains_ref = 'X'
                        disabled = ''
                        modified = ''
                        highlighted = ''
                        is_complex = 'X'
                        vis_filtered = ''
                    )
                    (
                        node_key = `2`
                        node_key_old = ``
*                        entity = REF_FOR::OBJ::{O:183*\CLASS=ZCL_SIMPLE_STRUCT}
                        parent_node_key = `1`
                        contains_ref = ''
                        disabled = ''
                        modified = ''
                        highlighted = ''
                        is_complex = ''
                        vis_filtered = ''
                    )
                    (
                        node_key = `3`
                        node_key_old = ``
*                        entity = REF_FOR::OBJ::{O:180*\CLASS=ZCL_STRUCT_RTTI}
                        parent_node_key = `1`
                        contains_ref = 'X'
                        disabled = ''
                        modified = ''
                        highlighted = ''
                        is_complex = 'X'
                        vis_filtered = ''
                    )
                    (
                        node_key = `4`
                        node_key_old = ``
*                        entity = REF_FOR::OBJ::{O:179*\CLASS=ZCL_SIMPLE_STRUCT}
                        parent_node_key = `3`
                        contains_ref = ''
                        disabled = ''
                        modified = ''
                        highlighted = ''
                        is_complex = ''
                        vis_filtered = ''
                    )
                    (
                        node_key = `5`
                        node_key_old = ``
*                        entity = REF_FOR::OBJ::{O:178*\CLASS=ZCL_SIMPLE_STRUCT}
                        parent_node_key = `3`
                        contains_ref = ''
                        disabled = ''
                        modified = ''
                        highlighted = ''
                        is_complex = ''
                        vis_filtered = ''
                    )
                    (
                        node_key = `6`
                        node_key_old = ``
*                        entity = REF_FOR::OBJ::{O:177*\CLASS=ZCL_SIMPLE_STRUCT}
                        parent_node_key = `3`
                        contains_ref = ''
                        disabled = ''
                        modified = ''
                        highlighted = ''
                        is_complex = ''
                        vis_filtered = ''
                    )
                    (
                        node_key = `7`
                        node_key_old = ``
*                        entity = REF_FOR::OBJ::{O:176*\CLASS=ZCL_REF_ENTITY}
                        parent_node_key = `3`
                        contains_ref = 'X'
                        disabled = ''
                        modified = ''
                        highlighted = ''
                        is_complex = ''
                        vis_filtered = ''
                    )
                    (
                        node_key = `8`
                        node_key_old = ``
*                        entity = REF_FOR::OBJ::{O:175*\CLASS=ZCL_REF_ENTITY}
                        parent_node_key = `3`
                        contains_ref = 'X'
                        disabled = ''
                        modified = ''
                        highlighted = ''
                        is_complex = ''
                        vis_filtered = ''
                    ) ).

  ENDMETHOD.
ENDCLASS.
