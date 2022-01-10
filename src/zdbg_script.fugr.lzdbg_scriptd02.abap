*----------------------------------------------------------------------*
***INCLUDE LZDBG_SCRIPTD02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_screen_state
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class lcl_screen_state
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_screen_state DEFINITION.
  PUBLIC SECTION.
    METHODS:
      set_alv_constructed IMPORTING iv_alv_constr TYPE abap_bool,
      is_alv_constructed RETURNING VALUE(rv_is_constr) TYPE abap_bool,
      toggle_edit,
      is_edit RETURNING VALUE(rv_is_edit) TYPE abap_bool.
    METHODS:
      set_grid_rendered IMPORTING iv_grid_rendered TYPE abap_bool,
      set_file_selection_allowed IMPORTING iv_filsel_allowed TYPE abap_bool.
    DATA: mv_grid_inital_rendered   TYPE abap_bool READ-ONLY,
          mv_file_selection_allowed TYPE abap_bool READ-ONLY.

    CLASS-METHODS:
      get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_screen_state,
      force_init,
      class_constructor.
  PRIVATE SECTION.
    DATA: mv_alv_constructed TYPE abap_bool,
          mv_is_edit         TYPE abap_bool VALUE abap_true.
    CLASS-DATA: lo_instance TYPE REF TO lcl_screen_state.
ENDCLASS.

CLASS lcl_screen_state IMPLEMENTATION.
  METHOD force_init.
    IF lo_instance IS BOUND. FREE lo_instance. ENDIF.
    lo_instance = NEW #( ).
  ENDMETHOD.
  METHOD get_instance.
    ro_instance = lo_instance.
  ENDMETHOD.
  METHOD class_constructor.
    lo_instance = NEW #( ).
  ENDMETHOD.
  METHOD is_alv_constructed.
    rv_is_constr = mv_alv_constructed.
  ENDMETHOD.
  METHOD set_alv_constructed.
    mv_alv_constructed = iv_alv_constr.
  ENDMETHOD.
  METHOD toggle_edit.
    mv_is_edit = COND #( WHEN mv_is_edit EQ abap_true THEN abap_false ELSE abap_true ).
  ENDMETHOD.
  METHOD is_edit.
    rv_is_edit = mv_is_edit.
  ENDMETHOD.
  METHOD set_file_selection_allowed.
    mv_file_selection_allowed = iv_filsel_allowed.
  ENDMETHOD.
  METHOD set_grid_rendered.
    mv_grid_inital_rendered = iv_grid_rendered.
  ENDMETHOD.
ENDCLASS.
