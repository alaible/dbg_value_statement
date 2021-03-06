*&---------------------------------------------------------------------*
*& Include          LZDBG_SCRIPTCX1
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
CLASS lcx_screen_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    INTERFACES:
      if_t100_message,
      if_t100_dyn_msg.
ENDCLASS.
*----------------------------------------------------------------------*
CLASS lcx_tree_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    INTERFACES:
      if_t100_message,
      if_t100_dyn_msg.
ENDCLASS.
*----------------------------------------------------------------------*
CLASS lcx_ctrl_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    INTERFACES:
      if_t100_message,
      if_t100_dyn_msg.
ENDCLASS.
*----------------------------------------------------------------------*
CLASS lcx_vrm_value DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    INTERFACES:
      if_t100_message,
      if_t100_dyn_msg.
ENDCLASS.
*----------------------------------------------------------------------*
CLASS lcx_no_value_set DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    INTERFACES:
      if_t100_message,
      if_t100_dyn_msg.
ENDCLASS.
