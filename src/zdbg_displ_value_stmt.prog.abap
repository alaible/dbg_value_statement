*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>ZTEST_VALUE_STMT</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Value Statement Generator</SCRIPT_COMMENT>
*<SINGLE_STEP>X</SINGLE_STEP>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super  .

  PUBLIC SECTION.
    METHODS: prologue  REDEFINITION,
      init    REDEFINITION,
      script  REDEFINITION,
      end     REDEFINITION.

ENDCLASS.                    "lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script IMPLEMENTATION.
  METHOD prologue.
*** generate abap_source (source handler for ABAP)
    super->prologue( ).
  ENDMETHOD.                    "prolog

  METHOD init.
*** insert your initialization code here
  ENDMETHOD.                    "init
  METHOD script.
    DATA: lo_type_desc TYPE REF TO cl_tpda_script_data_descr,
          lv_var_name  TYPE string,
          lv_dbg       TYPE abap_bool.
* Target-Refernces for Downcast of Root-Entity
    DATA: lo_struct_descr TYPE REF TO cl_tpda_script_structdescr,
          lo_table_descr  TYPE REF TO cl_tpda_script_tabledescr.
* Target-References for Root-Entity
    DATA: lo_struct TYPE REF TO zcl_struct_entity,
          lo_table  TYPE REF TO zcl_table_entity.
    DATA: lr_mock_data TYPE REF TO zcl_compl_entity=>tt_mock_data.


*** insert your script code here
    me->break( ).

* Read VAriable Name via selection-screen
    CALL FUNCTION 'Z_CALL_SEL_SCREEN'
      IMPORTING
        ev_varname = lv_var_name
        ev_dbg     = lv_dbg
      EXCEPTIONS
        cancel     = 1                " Abbruch d. Benutzer
        OTHERS     = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Cancelled by User...' TYPE 'I'.
      RETURN.
    ENDIF.

*** Break if Requested
    IF lv_dbg EQ abap_true.
      BREAK-POINT.
    ENDIF.

* The Debugger-Script just creates the root entities
* -> The tree-like structure is mainly build in the corresponding constructors of
* the classes ztest_table_entity and ztest_struct_entity (which take as input the lo_type_descr)
    TRY.
*        DATA(lt_globals) = cl_tpda_script_data_descr=>globals( ).
*        lr_mock_data = NEW #( ).
        lo_type_desc = cl_tpda_script_data_descr=>factory( p_var_name = lv_var_name ).
        TRY.
* Cast to Structure-Type First
            lo_struct_descr ?= lo_type_desc.
            lo_struct = NEW #(
                          iv_compname = to_lower( lv_var_name )
                          io_data_descr = lo_type_desc
                          iv_is_root = abap_true
*                          ir_mock_data = lr_mock_data
                          iv_curr_ind = 0
                        ).

            CALL FUNCTION 'Z_DISPLAY_VALUE_STMT'
              EXPORTING
                ir_entity = lo_struct.                 " ztest_compl_entity

          CATCH cx_sy_move_cast_error.
            TRY.
* Cast to Table-Type
                lo_table_descr ?= lo_type_desc.
                lo_table = NEW #(
                            iv_compname = to_lower( lv_var_name )
                            io_data_ref = lo_type_desc
                            iv_is_root = abap_true
*                            ir_mock_data = NEW #( )
                            iv_curr_ind = 0
                           ).

                CALL FUNCTION 'Z_DISPLAY_VALUE_STMT'
                  EXPORTING
                    ir_entity = lo_table.                 " ztest_compl_entity

              CATCH cx_sy_move_cast_error.
            ENDTRY.
        ENDTRY.
      CATCH cx_tpda_varname cx_tpda_data_descr_invalidated INTO DATA(lo_err).                " TPDA: Variable existiert nicht
        MESSAGE lo_err->get_text( ) TYPE 'I'.
    ENDTRY.
*** insert your script code here
    me->break( ).

  ENDMETHOD.                    "script
  METHOD end.
*** insert your code which shall be executed at the end of the scripting (before trace is saved)
*** here

  ENDMETHOD.                    "end
ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
