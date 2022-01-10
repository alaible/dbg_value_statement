*----------------------------------------------------------------------*
***INCLUDE LZDBG_SCRIPTD04.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_screen_objects
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
***INCLUDE ZTEST_SIMPLE_TRANSFORMATIONC01.
*&---------------------------------------------------------------------*
*&       Class lcl_screen_objects
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_screen_objects DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_c30 TYPE c LENGTH 30.
    TYPES: string_tab TYPE TABLE OF string WITH EMPTY KEY.
    TYPES: t_c_text TYPE TABLE OF char90 WITH DEFAULT KEY.
    TYPES: ref_char_tab TYPE REF TO t_c_text.
    CLASS-METHODS:
      class_constructor,
      set_ref_str_tab IMPORTING ir_ref TYPE ref_char_tab,
      show_text RAISING lcx_screen_error,
      call_screen_9000.
  PRIVATE SECTION.
*** GUI-Objekte
    DATA: mo_cust_control TYPE REF TO cl_gui_custom_container.
    DATA: mo_text_edit TYPE REF TO cl_gui_textedit.
*** Flag für initialisierung
    DATA: mv_initialized TYPE abap_bool.
    DATA: mr_ref_str_tab TYPE ref_char_tab.

    METHODS:
      init_container RAISING lcx_screen_error,
      init_text_edit RAISING lcx_screen_error,
      set_read_only_mode RAISING lcx_screen_error.
*** Konstante für Custom-Container
    CLASS-DATA: lc_control_name TYPE string VALUE 'CUST_CONTROL'.
    CLASS-DATA: lo_instance TYPE REF TO lcl_screen_objects.

ENDCLASS.

CLASS lcl_screen_objects IMPLEMENTATION.
  METHOD class_constructor.
    lo_instance = NEW #( ).
  ENDMETHOD.
  METHOD set_ref_str_tab.
    lo_instance->mr_ref_str_tab = ir_ref.
  ENDMETHOD.
  METHOD call_screen_9000.
    CALL SCREEN 9000 STARTING AT 5 5.
  ENDMETHOD.
  METHOD show_text.
    FIELD-SYMBOLS: <fs_str_tab> TYPE t_c_text.
    IF lo_instance->mv_initialized EQ abap_false.
      TRY.
          lo_instance->init_container( ).
          lo_instance->init_text_edit( ).
*** set read_only
          lo_instance->set_read_only_mode( ).
          lo_instance->mv_initialized = abap_true.
        CATCH lcx_screen_error INTO DATA(lo_err).
          MESSAGE |{ lo_err->get_text( ) }| TYPE 'E'.
      ENDTRY.
    ENDIF.
    ASSIGN lo_instance->mr_ref_str_tab->* TO <fs_str_tab>.
    lo_instance->mo_text_edit->set_text_as_r3table(
      EXPORTING
        table           = <fs_str_tab>     " table with text
      EXCEPTIONS
        error_dp        = 1                " Error while sending R/3 table to TextEdit control!
        error_dp_create = 2                " ERROR_DP_CREATE
        OTHERS          = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_screen_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD init_container.
    CREATE OBJECT mo_cust_control
      EXPORTING
        container_name              = CONV ty_c30( lc_control_name ) " Name of the dynpro CustCtrl name to link this container to
      EXCEPTIONS
        cntl_error                  = 1                " CNTL_ERROR
        cntl_system_error           = 2                " CNTL_SYSTEM_ERROR
        create_error                = 3                " CREATE_ERROR
        lifetime_error              = 4                " LIFETIME_ERROR
        lifetime_dynpro_dynpro_link = 5                " LIFETIME_DYNPRO_DYNPRO_LINK
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_screen_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD init_text_edit.
    CREATE OBJECT mo_text_edit
      EXPORTING
        parent                 = mo_cust_control          " Parent-Container
      EXCEPTIONS
        error_cntl_create      = 1                        " Error while performing creation of TextEdit control!
        error_cntl_init        = 2                        " Error while initializing TextEdit control!
        error_cntl_link        = 3                        " Error while linking TextEdit control!
        error_dp_create        = 4                        " Error while creating DataProvider control!
        gui_type_not_supported = 5                        " This type of GUI is not supported!
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_screen_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD set_read_only_mode.
    lo_instance->mo_text_edit->set_readonly_mode(
*      EXPORTING
*        readonly_mode          = true             " readonly mode; eq 0: OFF ; ne 0: ON
      EXCEPTIONS
        error_cntl_call_method = 1                " Error while setting readonly mode!
        invalid_parameter      = 2                " INVALID_PARAMETER
        OTHERS                 = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_screen_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
