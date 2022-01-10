*&---------------------------------------------------------------------*
*& Report ZTEST_VALUE_DBTAB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_value_dbtab.

PARAMETERS: p_tab   TYPE dd02l-tabname LOWER CASE OBLIGATORY DEFAULT 'scarr',
            p_flds  TYPE string DEFAULT '*' LOWER CASE,
            p_where TYPE string LOWER CASE,
            p_order TYPE string DEFAULT 'primary key' LOWER CASE,
            p_max   TYPE i DEFAULT 500000.

SELECTION-SCREEN ULINE.
PARAMETERS: p_wrt TYPE xfeld DEFAULT abap_true.

START-OF-SELECTION.
  DATA: lr_data TYPE REF TO data.
  DATA: lo_rtti_table TYPE REF TO zcl_table_rtti.
  FIELD-SYMBOLS: <res_tab> TYPE ANY TABLE.
**********************************************************************
  CREATE DATA lr_data TYPE TABLE OF (p_tab).
  ASSIGN lr_data->* TO <res_tab>.
  DATA(lv_where) = COND #( WHEN p_where IS INITIAL THEN '1 = 1' ELSE p_where ).
  TRY.
      SELECT FROM (p_tab)
           FIELDS *
            WHERE (lv_where)
         ORDER BY (p_order)
       INTO TABLE @<res_tab>
            UP TO @p_max ROWS.
    CATCH cx_sy_open_sql_error INTO DATA(err).
      MESSAGE err->get_text( ) TYPE 'E'.
  ENDTRY.
*  BREAK-POINT.
**********************************************************************
  CREATE OBJECT lo_rtti_table
    EXPORTING
      iv_is_root  = abap_true
      io_data_ref = cl_abap_typedescr=>describe_by_data( p_data = <res_tab> )
      ir_tab      = REF #( <res_tab> )
      iv_key      = 'res_tab'
      iv_curr_ind = 0.
**********************************************************************
  CASE abap_true.
    WHEN p_wrt.
      LOOP AT lo_rtti_table->get_content( i_max_line_length = 256 ) ASSIGNING FIELD-SYMBOL(<content>).
        WRITE:/ |{ <content> }|.
      ENDLOOP.
    WHEN OTHERS.
**********************************************************************
      CALL FUNCTION 'Z_DISPLAY_VALUE_STMT'
        EXPORTING
          ir_entity    = lo_rtti_table    " zcl_compl_entity
        EXCEPTIONS
          screen_error = 1                " Control-Fehler
          OTHERS       = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
  ENDCASE.
