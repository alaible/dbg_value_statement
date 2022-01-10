*&---------------------------------------------------------------------*
*& Include          LZDBG_SCRIPTD98
*&---------------------------------------------------------------------*
CLASS ltc_test_data DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.
    CLASS-DATA:
      node_tab_tmc           TYPE zcl_entity=>tt_node_table_tmc,
      search_index_comp      TYPE zcl_entity=>tt_search_index,
      search_index_cont_comp TYPE zcl_entity=>tt_search_index_cont,
      node_tab_comp type treemcnota,
      item_tab_comp type treemcitac.
ENDCLASS.

CLASS ltc_test_data IMPLEMENTATION.
  METHOD class_constructor.
**********************************************************************
    search_index_comp = VALUE #(
                          (
                              search_term = `comp1`
                              search_term_w_cont = ``
                              node_key = `4`
                          )
                          (
                              search_term = `comp2`
                              search_term_w_cont = ``
                              node_key = `5`
                          )
                          (
                              search_term = `int_ref`
                              search_term_w_cont = ``
                              node_key = `8`
                          )
                          (
                              search_term = `nested`
                              search_term_w_cont = ``
                              node_key = `3`
                          )
                          (
                              search_term = `obj`
                              search_term_w_cont = ``
                              node_key = `7`
                          )
                          (
                              search_term = `outer`
                              search_term_w_cont = ``
                              node_key = `2`
                          )
                          (
                              search_term = `some_int`
                              search_term_w_cont = ``
                              node_key = `6`
                          )
                          (
                              search_term = `st_with_ref`
                              search_term_w_cont = ``
                              node_key = `1`
                          )
                        ).
**********************************************************************
    search_index_cont_comp  = VALUE #(
                                (
                                    search_term = ``
                                    search_term_w_cont = ``
                                    node_key = `8`
                                )
                                (
                                    search_term = ``
                                    search_term_w_cont = ``
                                    node_key = `7`
                                )
                                (
                                    search_term = ``
                                    search_term_w_cont = `5`
                                    node_key = `6`
                                )
                                (
                                    search_term = ``
                                    search_term_w_cont = `one`
                                    node_key = `4`
                                )
                                (
                                    search_term = ``
                                    search_term_w_cont = `outer`
                                    node_key = `2`
                                )
                                (
                                    search_term = ``
                                    search_term_w_cont = `two`
                                    node_key = `5`
                                )
                              ).
**********************************************************************
    node_tab_tmc = VALUE #(
                      (
                          node_key = `1`
                          node_key_old = ``
*                      entity = REF_FOR::OBJ::{O:38*\CLASS=ZCL_STRUCT_RTTI}
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
*                      entity = REF_FOR::OBJ::{O:41*\CLASS=ZCL_SIMPLE_STRUCT}
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
*                      entity = REF_FOR::OBJ::{O:44*\CLASS=ZCL_STRUCT_RTTI}
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
*                      entity = REF_FOR::OBJ::{O:45*\CLASS=ZCL_SIMPLE_STRUCT}
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
*                      entity = REF_FOR::OBJ::{O:46*\CLASS=ZCL_SIMPLE_STRUCT}
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
*                      entity = REF_FOR::OBJ::{O:49*\CLASS=ZCL_SIMPLE_STRUCT}
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
*                      entity = REF_FOR::OBJ::{O:54*\CLASS=ZCL_REF_ENTITY}
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
*                      entity = REF_FOR::OBJ::{O:57*\CLASS=ZCL_REF_ENTITY}
                          parent_node_key = `3`
                          contains_ref = 'X'
                          disabled = ''
                          modified = ''
                          highlighted = ''
                          is_complex = ''
                          vis_filtered = ''
                      )
                    ).
**********************************************************************
    node_tab_comp  = value #(
                       (
                           node_key = `1`
                           relatkey = ``
                           relatship = 1
                           hidden = ''
                           disabled = ''
                           isfolder = 'X'
                           n_image = ''
                           exp_image = ''
                           style = 0
                           no_branch = ''
                           expander = ''
                           dragdropid = '0 '
*                           userobject = REF_FOR::OBJ::{O:initial}
                           itemsincom = ''
                       )
                       (
                           node_key = `2`
                           relatkey = `1`
                           relatship = 1
                           hidden = ''
                           disabled = ''
                           isfolder = ''
                           n_image = ''
                           exp_image = ''
                           style = 0
                           no_branch = ''
                           expander = ''
                           dragdropid = '0 '
*                           userobject = REF_FOR::OBJ::{O:initial}
                           itemsincom = ''
                       )
                       (
                           node_key = `3`
                           relatkey = `1`
                           relatship = 1
                           hidden = ''
                           disabled = ''
                           isfolder = 'X'
                           n_image = ''
                           exp_image = ''
                           style = 0
                           no_branch = ''
                           expander = ''
                           dragdropid = '0 '
*                           userobject = REF_FOR::OBJ::{O:initial}
                           itemsincom = ''
                       )
                       (
                           node_key = `4`
                           relatkey = `3`
                           relatship = 1
                           hidden = ''
                           disabled = ''
                           isfolder = ''
                           n_image = ''
                           exp_image = ''
                           style = 0
                           no_branch = ''
                           expander = ''
                           dragdropid = '0 '
*                           userobject = REF_FOR::OBJ::{O:initial}
                           itemsincom = ''
                       )
                       (
                           node_key = `5`
                           relatkey = `3`
                           relatship = 1
                           hidden = ''
                           disabled = ''
                           isfolder = ''
                           n_image = ''
                           exp_image = ''
                           style = 0
                           no_branch = ''
                           expander = ''
                           dragdropid = '0 '
*                           userobject = REF_FOR::OBJ::{O:initial}
                           itemsincom = ''
                       )
                       (
                           node_key = `6`
                           relatkey = `3`
                           relatship = 1
                           hidden = ''
                           disabled = ''
                           isfolder = ''
                           n_image = ''
                           exp_image = ''
                           style = 0
                           no_branch = ''
                           expander = ''
                           dragdropid = '0 '
*                           userobject = REF_FOR::OBJ::{O:initial}
                           itemsincom = ''
                       )
                       (
                           node_key = `7`
                           relatkey = `3`
                           relatship = 1
                           hidden = ''
                           disabled = ''
                           isfolder = ''
                           n_image = ''
                           exp_image = ''
                           style = 0
                           no_branch = ''
                           expander = ''
                           dragdropid = '0 '
*                           userobject = REF_FOR::OBJ::{O:initial}
                           itemsincom = ''
                       )
                       (
                           node_key = `8`
                           relatkey = `3`
                           relatship = 1
                           hidden = ''
                           disabled = ''
                           isfolder = ''
                           n_image = ''
                           exp_image = ''
                           style = 0
                           no_branch = ''
                           expander = ''
                           dragdropid = '0 '
*                           userobject = REF_FOR::OBJ::{O:initial}
                           itemsincom = ''
                       )
                     ).
    item_tab_comp  = value #(
                      (
                          node_key = `1`
                          item_name = 'NODE_NAME'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `st_with_ref`
                      )
                      (
                          node_key = `1`
                          item_name = 'NODE_TYPE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `complex`
                      )
                      (
                          node_key = `1`
                          item_name = 'CONTAINS_REF'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `X`
                      )
                      (
                          node_key = `2`
                          item_name = 'NODE_NAME'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `outer`
                      )
                      (
                          node_key = `2`
                          item_name = 'NODE_TYPE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `simple`
                      )
                      (
                          node_key = `2`
                          item_name = 'VALUE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `outer`
                      )
                      (
                          node_key = `2`
                          item_name = 'CONTAINS_REF'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = ``
                      )
                      (
                          node_key = `3`
                          item_name = 'NODE_NAME'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `nested`
                      )
                      (
                          node_key = `3`
                          item_name = 'NODE_TYPE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `complex`
                      )
                      (
                          node_key = `3`
                          item_name = 'CONTAINS_REF'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `X`
                      )
                      (
                          node_key = `4`
                          item_name = 'NODE_NAME'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `comp1`
                      )
                      (
                          node_key = `4`
                          item_name = 'NODE_TYPE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `simple`
                      )
                      (
                          node_key = `4`
                          item_name = 'VALUE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `one`
                      )
                      (
                          node_key = `4`
                          item_name = 'CONTAINS_REF'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = ``
                      )
                      (
                          node_key = `5`
                          item_name = 'NODE_NAME'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `comp2`
                      )
                      (
                          node_key = `5`
                          item_name = 'NODE_TYPE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `simple`
                      )
                      (
                          node_key = `5`
                          item_name = 'VALUE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `two`
                      )
                      (
                          node_key = `5`
                          item_name = 'CONTAINS_REF'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = ``
                      )
                      (
                          node_key = `6`
                          item_name = 'NODE_NAME'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `some_int`
                      )
                      (
                          node_key = `6`
                          item_name = 'NODE_TYPE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `simple`
                      )
                      (
                          node_key = `6`
                          item_name = 'VALUE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `5 `
                      )
                      (
                          node_key = `6`
                          item_name = 'CONTAINS_REF'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = ``
                      )
                      (
                          node_key = `7`
                          item_name = 'NODE_NAME'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `obj`
                      )
                      (
                          node_key = `7`
                          item_name = 'NODE_TYPE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `reference`
                      )
                      (
                          node_key = `7`
                          item_name = 'VALUE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = ``
                      )
                      (
                          node_key = `7`
                          item_name = 'CONTAINS_REF'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `X`
                      )
                      (
                          node_key = `8`
                          item_name = 'NODE_NAME'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `int_ref`
                      )
                      (
                          node_key = `8`
                          item_name = 'NODE_TYPE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `reference`
                      )
                      (
                          node_key = `8`
                          item_name = 'VALUE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = ``
                      )
                      (
                          node_key = `8`
                          item_name = 'CONTAINS_REF'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `X`
                      )
                    ).
  ENDMETHOD.
ENDCLASS.
