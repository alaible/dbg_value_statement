**----------------------------------------------------------------------*
****INCLUDE LZDBG_SCRIPTD01.
**----------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**& Class lcl_event_handler
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
*CLASS lcl_event_handler DEFINITION.
*  PUBLIC SECTION.
*    CLASS-METHODS: handle_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key.
*ENDCLASS.
**&---------------------------------------------------------------------*
**&       Class lcl_alv_tree_objects
**&---------------------------------------------------------------------*
**        Text
**----------------------------------------------------------------------*
*CLASS lcl_alv_tree_objects DEFINITION.
*  PUBLIC SECTION.
*    TYPES: ty_c30 TYPE c LENGTH 30.
*    TYPES: t_c_text TYPE TABLE OF char90 WITH DEFAULT KEY.
*    TYPES: ref_char_tab TYPE REF TO t_c_text.
**    TYPES: BEGIN OF ty_node_cont.
**        INCLUDE TYPE zcl_value_alv_tree.
**    TYPES: ref_str_tab TYPE zcl_entity=>ty_ref_strtab,
**           END OF ty_node_cont.
*    CLASS-METHODS:
*      set_root_ref IMPORTING io_root TYPE REF TO zcl_compl_entity,
*      set_ref_str_tab IMPORTING ir_ref TYPE ref_char_tab,
*      free_container RAISING lcx_screen_error,
*      delete_all_nodes RAISING lcx_screen_error,
*      toggle_edit,
*      show_text RAISING lcx_screen_error,
*      build_alv RAISING lcx_screen_error,
*      class_constructor.
*  PRIVATE SECTION.
**** GUI-Objekte
*    DATA: mo_cust_control     TYPE REF TO cl_gui_custom_container,
*          mo_splitter_control TYPE REF TO cl_gui_splitter_container,
*          mo_alv_tree         TYPE REF TO cl_salv_tree,
*          mo_text_edit        TYPE REF TO cl_gui_textedit.
*    DATA: mt_data_table TYPE TABLE OF zcl_compl_entity=>ty_node_cont.
*    DATA: mo_root_ent TYPE REF TO zcl_compl_entity.
**** flag für initialisierung
*    DATA: mv_initialized TYPE abap_bool.
*    DATA: mr_ref_str_tab   TYPE ref_char_tab,
*          mv_root_node_key TYPE string.
*
*    DATA: mv_selected_node_key TYPE salv_de_node_key.
*
*    DATA: mr_ref_node_table TYPE REF TO zcl_entity=>tty_node_table.
*
*    METHODS:
*      handle_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key,
*      on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function,
**      handle_toolbar FOR EVENT toolbar OF cl_gui_textedit IMPORTING e_object e_interactive,
*      init_container RAISING lcx_screen_error,
*      init_alv_tree RAISING cx_salv_error,
*      init_splitter RAISING lcx_screen_error,
*      init_alv RAISING lcx_screen_error,
*      init_text_edit RAISING lcx_screen_error,
*      save_text_content,
*      highlight_ref_elements,
*      disable_child_elemnts IMPORTING iv_parent_node_key TYPE salv_de_node_key,
*      set_read_only_mode IMPORTING iv_read_only TYPE abap_bool RAISING lcx_screen_error,
*      expand_all_parent_node RAISING cx_salv_msg,
*      set_word_rap RAISING lcx_screen_error,
*      add_root_node IMPORTING iv_set_root_as_selected TYPE abap_bool RAISING lcx_screen_error.
**** konstante für custom-container
*    CLASS-DATA: lc_control_name TYPE string VALUE 'CUST_CONTROL'.
*    CLASS-DATA: lo_instance TYPE REF TO lcl_alv_tree_objects.
*    CLASS-METHODS: str_tab_as_flat_str IMPORTING it_str_tab TYPE zcl_entity=>tty_string RETURNING VALUE(rv_flatstring) TYPE string.
*ENDCLASS.
*
*
*CLASS lcl_alv_tree_objects IMPLEMENTATION.
*  METHOD build_alv.
*    IF lo_instance->mv_initialized EQ abap_false.
*      lo_instance->init_alv( ).
*    ENDIF.
*    lo_instance->add_root_node( abap_true ).
*
**** Ersten Knoten Expandieren
*    TRY.
*        lo_instance->mo_alv_tree->get_nodes( )->get_node( node_key = CONV #( lo_instance->mo_root_ent->get_alv_tree_key( ) ) )->expand( ).
*      CATCH cx_salv_msg. " ALV: Allg. Fehlerklasse  mit Meldung.
*    ENDTRY.
*    lo_instance->mo_alv_tree->display( ).
**    CATCH lcx_screen_error.
*  ENDMETHOD.
*  METHOD class_constructor.
*    lo_instance = NEW #( ).
*  ENDMETHOD.
*  METHOD set_root_ref.
*    lo_instance->mo_root_ent = io_root.
*  ENDMETHOD.
*  METHOD init_alv.
*    TRY.
*        me->init_container( ).
*        me->init_splitter( ).
*        me->init_alv_tree( ).
*
*        DATA(lt_columns) = me->lo_instance->mo_alv_tree->get_columns( ).
*
*        DATA(ls_col) = lt_columns->get_column( columnname = 'VALUE_STMT' ).
*        IF ls_col IS BOUND. ls_col->set_visible( value = if_salv_c_bool_sap=>false ). ENDIF.
*        ls_col = lt_columns->get_column( columnname = 'NODE_NAME' ).
*        ls_col->set_medium_text( value = CONV #( |Component Name| ) ).
*        ls_col->set_visible( if_salv_c_bool_sap=>false ).
*
*        ls_col = lt_columns->get_column( columnname = 'CONTAINS_REF_EL' ).
*        ls_col->set_medium_text( CONV #( |Contains Ref| ) ).
*        ls_col->set_output_length( 5 ).
*
*        ls_col = lt_columns->get_column( columnname = 'NODE_TYPE' ).
*        ls_col->set_medium_text( CONV #( |Node-Type| ) ).
*        ls_col->set_output_length( 10 ).
*
*
*        DATA(lo_events) = me->lo_instance->mo_alv_tree->get_event( ).
**       CATCH cx_salv_not_found. " ALV: Allg. Fehlerklasse (wird bei Syntaxprüfung geprüft)
**       CATCH cx_salv_not_found. " ALV: Allg. Fehlerklasse (wird bei Syntaxprüfung geprüft)
*      CATCH cx_salv_error INTO DATA(lo_err). " ALV: Allg. Fehlerklasse (wird bei Syntaxprüfung geprüft)
*        RAISE EXCEPTION TYPE lcx_screen_error
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDTRY.
*  ENDMETHOD.
*  METHOD add_root_node.
*    TRY.
*        mr_ref_node_table = NEW #( ).
**        mo_root_ent->add_to_node(
**          EXPORTING
**            io_salv_tree  = mo_alv_tree
**            iv_parent_key = space
**            io_ref_node_table = mr_ref_node_table
**        ).
*        me->highlight_ref_elements( ).
*        IF iv_set_root_as_selected EQ abap_true.
*          mv_selected_node_key = mo_root_ent->get_alv_tree_key( ).
*        ENDIF.
*        IF mo_root_ent->contains_reference( ) EQ abap_true.
*          lcl_screen_log=>get_instance( )->info( |Entity contains reference Elements!!!| ).
*        ENDIF.
*      CATCH cx_salv_msg INTO DATA(lo_err). " ALV: Allg. Fehlerklasse  mit Meldung
*        RAISE EXCEPTION TYPE lcx_screen_error
*          EXPORTING
*            previous = lo_err.
*    ENDTRY.
*  ENDMETHOD.
*  METHOD free_container.
*    lo_instance->mo_splitter_control->get_container( row = 1  column = 1 )->free(
*      EXCEPTIONS
*        cntl_error        = 1                " CNTL_ERROR
*        cntl_system_error = 2                " CNTL_SYSTEM_ERROR
*        OTHERS            = 3
*    ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF..
*  ENDMETHOD.
*  METHOD delete_all_nodes.
*    TRY.
*        DATA(lo_nodes) = lo_instance->mo_alv_tree->get_nodes( ).
*        lo_nodes->delete_all( ).
*
*      CATCH cx_salv_error INTO DATA(lo_err). " ALV: Allg. Fehlerklasse (wird bei Syntaxprüfung geprüft).
*        RAISE EXCEPTION TYPE lcx_screen_error
*          EXPORTING
*            previous = lo_err.
*    ENDTRY.
*  ENDMETHOD.
*  METHOD init_alv_tree.
*    CLEAR mt_data_table.
*    IF me->mv_initialized EQ abap_false.
*      cl_salv_tree=>factory(
*        EXPORTING
*        r_container = mo_splitter_control->get_container( row = 1 column = 1 )
*        IMPORTING
*          r_salv_tree = mo_alv_tree                 " ALV: Tree model
*        CHANGING
*          t_table     = mt_data_table
*      ).
**** Handler registrieren
*      SET HANDLER me->handle_double_click FOR mo_alv_tree->get_event( ).
*      SET HANDLER me->on_user_command FOR mo_alv_tree->get_event( ).
*      mo_alv_tree->get_tree_settings( )->set_hierarchy_header( 'NODE_NAME' ).
*      mo_alv_tree->get_tree_settings( )->set_hierarchy_tooltip( 'NODE_NAME' ).
*      mo_alv_tree->get_tree_settings( )->set_hierarchy_size( 40 ).
*      mo_alv_tree->get_tree_settings( )->set_hierarchy_icon( CONV #( icon_tree ) ).
*
*      mo_alv_tree->get_columns( )->set_optimize( abap_true ).
*
*      DATA(lt_functions) = mo_alv_tree->get_functions( ).
*
*      lt_functions->set_all( ).
*
*      lt_functions->add_function(
*        EXPORTING
*          name     =  'TOGGLE_EDIT'                " ALV Funktion
*          icon     = '@3I@'
*          tooltip  = 'toggle'
*          position =   if_salv_c_function_position=>right_of_salv_functions               " Funktion Positionierung
*      ).
*      lt_functions->add_function(
*        EXPORTING
*          name     =  'SAVE_CHANGES'                " ALV Funktion
*          icon     = '@2L@'
*          tooltip  = 'Sichern'
*          position =   if_salv_c_function_position=>right_of_salv_functions               " Funktion Positionierung
*      ).
**      CATCH cx_salv_existing.   " ALV: Allg. Fehlerklasse (wird bei Syntaxprüfung geprüft)
**      CATCH cx_salv_wrong_call. " ALV: Allg. Fehlerklasse (wird bei Syntaxprüfung geprüft)
*    ENDIF.
**    CATCH cx_salv_error. " ALV: Allg. Fehlerklasse (wird bei Syntaxprüfung geprüft)
*  ENDMETHOD.
*  METHOD init_container.
**    IF mo_cust_control IS BOUND.
**      mo_cust_control->free( ).
**      FREE mo_cust_control.
**    ENDIF.
*    CREATE OBJECT mo_cust_control
*      EXPORTING
*        container_name              = CONV ty_c30( lc_control_name ) " Name of the dynpro CustCtrl name to link this container to
*      EXCEPTIONS
*        cntl_error                  = 1                " CNTL_ERROR
*        cntl_system_error           = 2                " CNTL_SYSTEM_ERROR
*        create_error                = 3                " CREATE_ERROR
*        lifetime_error              = 4                " LIFETIME_ERROR
*        lifetime_dynpro_dynpro_link = 5                " LIFETIME_DYNPRO_DYNPRO_LINK
*        OTHERS                      = 6.
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD show_text.
*    FIELD-SYMBOLS: <fs_str_tab> TYPE t_c_text.
*    IF lo_instance->mv_initialized EQ abap_false.
*      TRY.
*          lo_instance->init_text_edit( ).
*          lo_instance->set_word_rap( ).
*          lo_instance->mv_initialized = abap_true.
*        CATCH lcx_screen_error INTO DATA(lo_err).
*          MESSAGE |{ lo_err->get_text( ) }| TYPE 'E'.
*      ENDTRY.
*    ENDIF.
**** SET read_only as default
*    lo_instance->set_read_only_mode( abap_true ).
*    ASSIGN lo_instance->mr_ref_str_tab->* TO <fs_str_tab>.
*    lo_instance->mo_text_edit->set_text_as_r3table(
*      EXPORTING
*        table           = <fs_str_tab>     " table with text
*      EXCEPTIONS
*        error_dp        = 1                " Error while sending R/3 table to TextEdit control!
*        error_dp_create = 2                " ERROR_DP_CREATE
*        OTHERS          = 3
*    ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD set_ref_str_tab.
*    lo_instance->mr_ref_str_tab = ir_ref.
*  ENDMETHOD.
*  METHOD init_splitter.
*    IF mo_splitter_control IS BOUND.
*      IF mo_splitter_control->is_alive( ).
*        FREE mo_splitter_control.
*      ENDIF.
*    ENDIF.
*    CREATE OBJECT mo_splitter_control
*      EXPORTING
**       link_dynnr        =                    " Dynpro-Nummer
**       link_repid        =                    " Reportname
**       shellstyle        =                    " Fenster-Style
**       left              =                    " Links
**       top               =                    " Oben
**       width             =                    " Breite
**       height            =                    " Hoehe
**       metric            = cntl_metric_dynpro " Metrik
**       align             = 15                 " Alignment
*        parent            = mo_cust_control                   " Parent Container
*        rows              = 1                   " Anzahl zu zeigender Zeilen
*        columns           = 2                   " Anzahl zu zeigender Spalten
**       no_autodef_progid_dynnr =                    " dont autodefine progid and dynnr?
**       name              =                    " Name
*      EXCEPTIONS
*        cntl_error        = 1                  " siehe Oberklasse
*        cntl_system_error = 2                  " siehe Oberklasse
*        OTHERS            = 3.
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    mo_splitter_control->set_row_mode(
*      EXPORTING
*        mode              = cl_gui_splitter_container=>mode_relative " Zeilenmodus
**      IMPORTING
**        result            =                  " Ergebniscode
**      EXCEPTIONS
**        cntl_error        = 1                " siehe CL_GUI_CONTROL
**        cntl_system_error = 2                " siehe CL_GUI_CONTROL
**        others            = 3
*    ).
*    IF sy-subrc <> 0.
**     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*    mo_splitter_control->set_row_sash(
*          EXPORTING
*          id                = 1
*          type              = cl_gui_splitter_container=>type_sashvisible
*          value             = cl_gui_splitter_container=>false ).
*
*    mo_splitter_control->set_column_width(
*      EXPORTING
*        id                = 1                 " Id der Spalte
*        width             = 35                 " Breite
**      IMPORTING
**        result            =                  " Ergebniscode
**      EXCEPTIONS
**        cntl_error        = 1                " siehe CL_GUI_CONTROL
**        cntl_system_error = 2                " siehe CL_GUI_CONTROL
**        others            = 3
*    ).
*    IF sy-subrc <> 0.
**     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*
*  ENDMETHOD.
*
*  METHOD init_text_edit.
*    CREATE OBJECT mo_text_edit
*      EXPORTING
*        parent                 = mo_splitter_control->get_container( row = 1 column = 2 )         " Parent-Container
*      EXCEPTIONS
*        error_cntl_create      = 1                        " Error while performing creation of TextEdit control!
*        error_cntl_init        = 2                        " Error while initializing TextEdit control!
*        error_cntl_link        = 3                        " Error while linking TextEdit control!
*        error_dp_create        = 4                        " Error while creating DataProvider control!
*        gui_type_not_supported = 5                        " This type of GUI is not supported!
*        OTHERS                 = 6.
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD highlight_ref_elements.
*    FIELD-SYMBOLS: <fs_node_table> TYPE zcl_compl_entity=>tty_node_table.
*
*    ASSIGN me->mr_ref_node_table->* TO <fs_node_table>.
*
*    LOOP AT <fs_node_table> ASSIGNING FIELD-SYMBOL(<fs_node_tab_entry>).
*      IF <fs_node_tab_entry>-contains_ref EQ abap_true.
*        mo_alv_tree->get_nodes( )->get_node( <fs_node_tab_entry>-node_key )->set_row_style( if_salv_c_tree_style=>emphasized_a ).
*      ENDIF.
*    ENDLOOP.
*  ENDMETHOD.
*  METHOD save_text_content.
*    DATA: lo_compl_ent TYPE REF TO zcl_compl_entity.
*    DATA: lt_content  TYPE t_c_text,
*          lt_cont_ext TYPE TABLE OF string.
*
*    READ TABLE me->mr_ref_node_table->* WITH KEY node_key = mv_selected_node_key ASSIGNING FIELD-SYMBOL(<fs_node_entry>).
*
**** Downcast auf compl_ent -> set external content
*    lo_compl_ent ?= <fs_node_entry>-entity.
*
*    TRY.
*        mo_text_edit->get_text_as_r3table(
**          EXPORTING
**            only_when_modified     = false            " get text only when modified
*          IMPORTING
*            table                  = lt_content                 " text as R/3 table
**            is_modified            =                  " modify status of text
*          EXCEPTIONS
*            error_dp               = 1                " Error while retrieving text table via DataProvider control!
*            error_cntl_call_method = 2                " Error while retrieving a property from TextEdit control
*            error_dp_create        = 3                " Error while creating DataProvider Control
*            potential_data_loss    = 4                " Potential data loss: use get_text_as_stream instead
*            OTHERS                 = 5
*        ).
*        IF sy-subrc <> 0.
*          RAISE EXCEPTION TYPE lcx_screen_error
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        ENDIF.
*      CATCH lcx_screen_error INTO DATA(lo_err).
*        DATA(lv_text) = lo_err->get_longtext( ).
*    ENDTRY.
*
**    BREAK-POINT.
*
**** Save external content to instance
*    LOOP AT lt_content ASSIGNING FIELD-SYMBOL(<fs_cont>).
*      APPEND <fs_cont> TO lt_cont_ext.
*    ENDLOOP.
*    lo_compl_ent->set_external_content( CONV #( lt_cont_ext ) ).
*
*    lcl_screen_log=>get_instance( )->info( |Updated Content for node: { lo_compl_ent->get_key( ) }| ).
*
**** reinit tree, but do not set root node as selected key
*    me->delete_all_nodes( ).
*
*    me->add_root_node( abap_false ).
*
**** Ersten Knoten Expandieren
*    TRY.
**        me->mo_alv_tree->get_nodes( )->get_node( node_key = CONV #( lo_instance->mo_root_ent->get_alv_tree_key( ) ) )->expand( ).
*        me->expand_all_parent_node( ).
*
**** Selected Key is set as the new alv_tree_key of the changed node
*        mv_selected_node_key = lo_compl_ent->get_alv_tree_key( ).
**** highligth elements that contain references
*        me->highlight_ref_elements( ).
*
*        mo_alv_tree->get_nodes( )->get_node( mv_selected_node_key )->set_row_style( if_salv_c_tree_style=>emphasized_positive ).
**    CATCH cx_salv_msg. " ALV: Allg. Fehlerklasse  mit Meldung
*
*        me->disable_child_elemnts( iv_parent_node_key = mv_selected_node_key ).
*        lcl_screen_log=>get_instance( )->info( |Disabling child-elements of node: { lo_compl_ent->get_key( ) }| ).
*      CATCH cx_salv_msg. " ALV: Allg. Fehlerklasse  mit Meldung.
*    ENDTRY.
*  ENDMETHOD.
*  METHOD expand_all_parent_node.
*    DATA: lv_parent_node_key TYPE zcl_entity=>ty_node_table-parent_node_key.
*    FIELD-SYMBOLS: <fs_node_table> TYPE zcl_entity=>tty_node_table,
*                   <fs_node_entry> TYPE zcl_entity=>ty_node_table.
*
*    ASSIGN me->mr_ref_node_table->* TO <fs_node_table>.
*
*    READ TABLE <fs_node_table> WITH KEY node_key_old = mv_selected_node_key ASSIGNING <fs_node_entry>.
**** TODO: Expand selected  node
*    me->mo_alv_tree->get_nodes( )->get_node( node_key = <fs_node_entry>-node_key )->expand( ).
*
*    WHILE <fs_node_entry>-parent_node_key NE space.
*      lv_parent_node_key = <fs_node_entry>-parent_node_key.
*      me->mo_alv_tree->get_nodes( )->get_node( node_key = lv_parent_node_key )->expand( ).
**      CATCH cx_salv_msg. " ALV: Allg. Fehlerklasse  mit Meldung.
*      READ TABLE <fs_node_table> WITH KEY node_key = lv_parent_node_key ASSIGNING <fs_node_entry>.
*    ENDWHILE.
*
*  ENDMETHOD.
*  METHOD disable_child_elemnts.
*    FIELD-SYMBOLS: <fs_node_table> TYPE zcl_entity=>tty_node_table,
*                   <fs_node_entry> TYPE zcl_entity=>ty_node_table.
*    FIELD-SYMBOLS: <fs_data>       TYPE any,
*                   <fs_is_complex> TYPE abap_bool.
*    DATA: lo_node TYPE REF TO cl_salv_node.
*
*    ASSIGN me->mr_ref_node_table->* TO <fs_node_table>.
*
*    LOOP AT <fs_node_table> ASSIGNING <fs_node_entry> WHERE parent_node_key = iv_parent_node_key.
*
*      <fs_node_entry>-disabled = abap_true.
*      lo_node = mo_alv_tree->get_nodes( )->get_node( node_key = <fs_node_entry>-node_key ).
*      lo_node->set_row_style( value = if_salv_c_tree_style=>inactive ).
*
**** Falls komplex auch Kind-Elemente disablen
*      IF <fs_node_entry>-is_complex EQ abap_true.
*        me->disable_child_elemnts( iv_parent_node_key = <fs_node_entry>-node_key ).
*      ENDIF.
*    ENDLOOP.
*
*  ENDMETHOD.
*  METHOD toggle_edit.
*    lcl_screen_state=>get_instance( )->toggle_edit( ).
*    lo_instance->set_read_only_mode( lcl_screen_state=>get_instance( )->is_edit( ) ).
**    CATCH lcx_screen_error.
*  ENDMETHOD.
*  METHOD set_read_only_mode.
*    lo_instance->mo_text_edit->set_readonly_mode(
*      EXPORTING
*        readonly_mode          = COND #( WHEN iv_read_only EQ abap_true THEN 1 ELSE 0 )             " readonly mode; eq 0: OFF ; ne 0: ON
*      EXCEPTIONS
*        error_cntl_call_method = 1                " Error while setting readonly mode!
*        invalid_parameter      = 2                " INVALID_PARAMETER
*        OTHERS                 = 3
*    ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD set_word_rap.
*    me->mo_text_edit->set_wordwrap_behavior(
*      EXPORTING
*        wordwrap_mode              = 2               " 0: OFF; 1: wrap a window border; 2: wrap at fixed position
*        wordwrap_position          = 80               " position of wordwrap, only makes sense with wordwrap_mode=2
**        wordwrap_to_linebreak_mode = bool_initial     " eq 1: change wordwrap to linebreak; 0: preserve wordwraps
*      EXCEPTIONS
*        error_cntl_call_method     = 1                " Error while setting word wrap properties of control!
*        OTHERS                     = 2
*    ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD str_tab_as_flat_str.
*    LOOP AT it_str_tab ASSIGNING FIELD-SYMBOL(<fs_str>).
*      rv_flatstring = |{ rv_flatstring } { <fs_str> }|.
*    ENDLOOP.
*  ENDMETHOD.
*  METHOD handle_double_click.
*    TRY.
*        DATA(lo_node) = me->mo_alv_tree->get_nodes( )->get_node( node_key  ).
*        DATA(lo_data) = lo_node->get_data_row( ).
*        FIELD-SYMBOLS: <fs_string>    TYPE string,
*                       <fs_type>      TYPE string,
*                       <fs_disabled>  TYPE abap_bool,
*                       <fs_val_cont>  TYPE REF TO zcl_value_content,
*                       <fs_data_line> TYPE any.
*        DATA: lt_char_tab TYPE t_c_text.
**        BREAK-POINT.
*        ASSIGN lo_data->* TO <fs_data_line>.
*
*        READ TABLE me->mr_ref_node_table->* WITH KEY node_key = node_key ASSIGNING FIELD-SYMBOL(<fs_node_entry>).
*
*        IF <fs_node_entry>-disabled = abap_true. RETURN. ENDIF.
*
*        mv_selected_node_key = node_key.
*
*        lcl_screen_log=>get_instance( )->debug(
*              iv_log_msg = |Double clicked on node { node_key }| ).
*
*        ASSIGN COMPONENT 'value_stmt' OF STRUCTURE <fs_data_line> TO <fs_string>.
*        ASSIGN COMPONENT 'val_cont' OF STRUCTURE <fs_data_line> TO <fs_val_cont>.
*        IF <fs_val_cont> IS ASSIGNED.
*          LOOP AT <fs_val_cont>->get_content( ) ASSIGNING FIELD-SYMBOL(<fs_line>).
*            APPEND <fs_line> TO lt_char_tab.
*          ENDLOOP.
*          lo_instance->mo_text_edit->set_text_as_r3table(
*                                        EXPORTING
*                                         table           = lt_char_tab     " table with text
*                                        EXCEPTIONS
*                                         error_dp        = 1                " Error while sending R/3 table to TextEdit control!
*                                         error_dp_create = 2                " ERROR_DP_CREATE
*                                         OTHERS          = 3
*                                         ).
*          IF sy-subrc <> 0.
*            RAISE EXCEPTION TYPE lcx_screen_error
*              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*          ENDIF.
*        ENDIF.
*      CATCH cx_salv_msg. " ALV: Allg. Fehlerklasse  mit Meldung
*    ENDTRY.
*  ENDMETHOD.
*  METHOD on_user_command.
*    CASE e_salv_function.
*      WHEN 'TOGGLE_EDIT'.
*        me->toggle_edit( ).
*      WHEN 'SAVE_CHANGES'.
*        me->save_text_content( ).
*    ENDCASE.
*  ENDMETHOD.
*ENDCLASS.
*
*CLASS lcl_event_handler IMPLEMENTATION.
*  METHOD handle_double_click.
*  ENDMETHOD.
*ENDCLASS.
