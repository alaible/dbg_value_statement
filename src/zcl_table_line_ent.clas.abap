CLASS zcl_table_line_ent DEFINITION
  PUBLIC
  INHERITING FROM zcl_struct_entity
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_data_descr TYPE REF TO cl_tpda_script_data_descr
                            iv_table_name TYPE string iv_tab_index TYPE i
                            iv_is_root TYPE abap_bool
*                            ir_mock_data TYPE REF TO zcl_compl_entity=>tt_mock_data
                            iv_curr_ind TYPE i
                  RAISING   cx_sy_move_cast_error cx_tpda_varname cx_tpda_data_descr_invalidated,
      get_key REDEFINITION,
      get_content REDEFINITION.
  PROTECTED SECTION.
    DATA: mv_line_index TYPE i.
ENDCLASS.



CLASS ZCL_TABLE_LINE_ENT IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
      EXPORTING
        iv_compname   = iv_table_name
        io_data_descr = io_data_descr
        iv_is_root = iv_is_root
*        ir_mock_data = ir_mock_data
        iv_curr_ind = iv_curr_ind
    ).
    me->mv_line_index = iv_tab_index.
  ENDMETHOD.


  METHOD get_content.
*** check if external content is set
    IF mt_external_content IS NOT INITIAL.
      rt_content = me->mt_external_content.
      RETURN.
    ENDIF.
*** Table line is surounded by ( )...
    APPEND |(| TO rt_content.
    LOOP AT me->mt_components ASSIGNING FIELD-SYMBOL(<comp>).
      LOOP AT <comp>->get_content( i_max_line_length = i_max_line_length ) ASSIGNING FIELD-SYMBOL(<cont>).
        APPEND |{ get_indention( ) }{ <cont> }| TO rt_content.
      ENDLOOP.
    ENDLOOP.
    APPEND |)| TO rt_content.
  ENDMETHOD.


  METHOD get_key.
    rv_key = CONV #( |{ me->mv_key }[{ me->mv_line_index }]| ).
  ENDMETHOD.
ENDCLASS.
