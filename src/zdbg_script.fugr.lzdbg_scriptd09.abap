*&---------------------------------------------------------------------*
*& Include          LZDBG_SCRIPTD09
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_dd_handler
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_dd_handler DEFINITION.
  PUBLIC SECTION.
**********************************************************************
*** Public Types
    TYPES: t_char_255 TYPE c LENGTH 255.
    TYPES: BEGIN OF t_init,
             ref_dd_f TYPE REF TO t_char_255,
             name     TYPE vrm_id,
           END OF t_init.
*** benötigt für vrm_setvalues
    TYPES: BEGIN OF t_keypair,
             key  TYPE string,
             text TYPE string,
           END OF t_keypair.
**********************************************************************
    CLASS-METHODS:
      class_constructor.
    CLASS-DATA:
      gt_loglevel   TYPE vrm_values.
    CONSTANTS: BEGIN OF c_loglevel,
                 error TYPE t_char_255 VALUE 'ERROR',
                 info  TYPE t_char_255 VALUE 'INFO',
                 debug TYPE t_char_255 VALUE 'DEBUG',
               END OF c_loglevel.
    METHODS:
      set_dd_loglevl IMPORTING is_init TYPE t_init,
      set_values RAISING lcx_vrm_value,
      get_loglevel RETURNING VALUE(rv_ll) TYPE t_char_255 RAISING lcx_no_value_set,
      set_loglevel IMPORTING iv_loglevel TYPE t_char_255,
      set_initialized IMPORTING iv_init TYPE abap_bool.
    DATA: mv_initialized TYPE abap_bool READ-ONLY.
  PROTECTED SECTION.
    CLASS-METHODS: vrm_setvalues IMPORTING iv_id TYPE vrm_id it_values TYPE vrm_values RAISING lcx_vrm_value.
    DATA: ms_loglevel TYPE t_init.
ENDCLASS.

CLASS lcl_dd_handler IMPLEMENTATION.
  METHOD class_constructor.
    gt_loglevel = VALUE #(
                    ( key = '1' text = c_loglevel-error )
                    ( key = '2' text = c_loglevel-info )
                    ( key = '3' text = c_loglevel-debug ) ).
  ENDMETHOD.
  METHOD set_dd_loglevl.
    ms_loglevel = is_init.
  ENDMETHOD.
  METHOD set_initialized.
    mv_initialized = iv_init.
  ENDMETHOD.
  METHOD set_loglevel.
    FIELD-SYMBOLS: <log_level_f> TYPE t_char_255.
    ASSIGN ms_loglevel-ref_dd_f->* TO <log_level_f>.
    READ TABLE gt_loglevel WITH KEY text = to_upper( iv_loglevel ) ASSIGNING FIELD-SYMBOL(<log_level>).
    CHECK sy-subrc = 0.
    <log_level_f> = <log_level>-key.
  ENDMETHOD.
  METHOD get_loglevel.
    FIELD-SYMBOLS: <dd_loglev> TYPE t_char_255.
    ASSIGN ms_loglevel-ref_dd_f->* TO <dd_loglev>.
    IF line_exists( gt_loglevel[ key = <dd_loglev> ] ).
      rv_ll = gt_loglevel[ key = <dd_loglev> ]-text.
    ELSE.
      RAISE EXCEPTION TYPE lcx_no_value_set.
    ENDIF.
  ENDMETHOD.
  METHOD set_values.
    FIELD-SYMBOLS: <dd_loglevel> TYPE t_char_255.
    ASSIGN ms_loglevel-ref_dd_f->* TO <dd_loglevel>.
    CHECK sy-subrc = 0.
    me->vrm_setvalues(
      EXPORTING
        iv_id     = ms_loglevel-name
        it_values = gt_loglevel
    ).
  ENDMETHOD.
  METHOD vrm_setvalues.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = iv_id " Name der Wertemenge
        values          = it_values    " Wertetabelle für ID
      EXCEPTIONS
        id_illegal_name = 1                " Der Name (ID) enthält ungültige Zeichen
        OTHERS          = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_vrm_value
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
