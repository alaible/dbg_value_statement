*&---------------------------------------------------------------------*
*& Report ZTEST_VALUE_FROM_SQL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_value_from_sql.

CLASS lcx_sql_err DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcl_sql_executor DEFINITION.
ENDCLASS.

CLASS lcl_sql_executor IMPLEMENTATION.
ENDCLASS.

DATA: o_dock     TYPE REF TO cl_gui_docking_container,
      o_splitter TYPE REF TO cl_gui_splitter_container.
DATA: o_salv_ida TYPE REF TO if_salv_gui_table_ida.
DATA: o_text_edit     TYPE REF TO cl_gui_textedit,
      o_text_edit_val TYPE REF TO cl_gui_textedit.
DATA: l_cntr TYPE i VALUE 0.

PARAMETERS: p_name TYPE string. " Dummy-Parameter zum erzwingen des Selektionsbildes

AT SELECTION-SCREEN OUTPUT.
  IF NOT o_dock IS BOUND.
* maximierten Dockingcontainer auf dem Selektionsbild erzeugen
    o_dock = NEW #( repid     = sy-repid
                    dynnr     = sy-dynnr
                    side      = cl_gui_docking_container=>dock_at_bottom
*                    extension = cl_gui_docking_container=>ws_maximizebox
                    ratio     = 70
                  ).
    CREATE OBJECT o_splitter
      EXPORTING
        parent  = o_dock              " Parent Container
        rows    = 1                   " Anzahl zu zeigender Zeilen
        columns = 2.                   " Anzahl zu zeigender Spalten
    CREATE OBJECT o_text_edit
      EXPORTING
        parent = o_splitter->get_container(
                   row    = 1
                   column = 1
                 ).                    " Parent-Container
* feste Zeichenbreite
    o_text_edit->set_font_fixed( mode = cl_gui_textedit=>true ).
    o_text_edit->set_wordwrap_behavior(
      EXPORTING
        wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position = 1024 ).
    CREATE OBJECT o_text_edit_val
      EXPORTING
        parent = o_splitter->get_container(
                   row    = 1
                   column = 2
                 ).                         " Parent-Container
    o_text_edit_val->set_font_fixed( mode = cl_gui_textedit=>true ).
    o_text_edit_val->set_wordwrap_behavior(
      EXPORTING
        wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position = 1024 ).
    o_text_edit_val->set_readonly_mode( 1 ).
  ENDIF.

AT SELECTION-SCREEN.
  DATA: text TYPE TABLE OF char1024.
  o_text_edit->get_text_as_r3table(
    IMPORTING
      table                   = text  " text as stream with carrige retruns and linefeeds
  ).
  DATA subr_pool TYPE TABLE OF string.
  l_cntr = l_cntr + 1.
  subr_pool = VALUE #(
   ( |program.|                     )
   ( |class { condense( 'main' && l_cntr ) } definition.| )
   ( |  public section.|            )
   ( |    class-methods: gen_object returning value(r_object) type ref to zcl_table_rtti.| )
*           ( |                   gen_object_ref importing ir_ref type ref to data changing c_data type any.| )
   ( |endclass.|                    )
   ( |class { condense( 'main' && l_cntr ) } implementation.|   )
   ( |  method gen_object.|               ) ).
  LOOP AT text ASSIGNING FIELD-SYMBOL(<text_>).
    IF matches( val = to_lower( <text_> ) regex = '.*(@data\(([[:alnum:]|_]*)\)).*' ).
*     data(result) = find( val = <text_> regex = '.*(@data\(([[:alnum:]])*\)).*'
*      BREAK-POINT.
      DATA(matcher) = cl_abap_matcher=>create( pattern     = '.*(@data\(([[:alnum:]|_]*)\)).*'
                                               text        = <text_>
                                               ignore_case = abap_true ).

* Alle Suchergebnisse ausgeben
      WHILE abap_true = matcher->find_next( ).
*        DATA(result) = matcher->get_submatch( 1 ).
        DATA(result_) = matcher->get_submatch( 2 ).
      ENDWHILE.
    ENDIF.
    APPEND <text_> TO subr_pool.
  ENDLOOP.
  subr_pool = VALUE #( BASE subr_pool
    ( |   CREATE OBJECT r_object| )
    ( |     EXPORTING| )
    ( |       iv_is_root  = abap_true| )
    ( |       io_data_ref = cl_abap_typedescr=>describe_by_data( p_data = { result_ } )| )
    ( |       ir_tab      = REF #( { result_ } )| )
    ( |       iv_key      = '{ result_ }'| )
    ( |       iv_curr_ind = 0.| ) ).
  subr_pool = VALUE #( BASE subr_pool
    ( |  endmethod.|                 )
    ( |endclass.| ) ).

  GENERATE SUBROUTINE POOL subr_pool NAME DATA(prog).
  IF sy-subrc <> 0.
  ELSE.
    DATA: class TYPE string.
    DATA: table_rtti TYPE REF TO zcl_table_rtti.
    class = `\PROGRAM=` && prog && |\\CLASS={ to_upper( condense( 'main' && l_cntr ) ) }|.
    CALL METHOD (class)=>gen_object
      RECEIVING
        r_object = table_rtti.
  ENDIF.
  DATA value_stmt TYPE TABLE OF char1024.
  CHECK table_rtti IS NOT INITIAL.
  value_stmt = VALUE #( FOR cont IN table_rtti->get_content( ) ( CONV #( cont ) ) ).
  o_text_edit_val->set_text_as_r3table(
    EXPORTING
      table           = value_stmt                 " table with text
  ).
*  CALL FUNCTION 'Z_DISPLAY_VALUE_STMT_LISTP'
*    EXPORTING
*      i_ent = table_rtti.                  " zcl_compl_entity
*START-OF-SELECTION.
