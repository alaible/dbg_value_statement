CLASS zcl_tableline_rtti DEFINITION PUBLIC CREATE PUBLIC INHERITING FROM zcl_struct_rtti.
  PUBLIC SECTION.
    METHODS:
      get_content REDEFINITION.
ENDCLASS.



CLASS ZCL_TABLELINE_RTTI IMPLEMENTATION.


  METHOD get_content.
    IF mt_external_content IS NOT INITIAL.
      rt_content = me->mt_external_content.
      RETURN.
    ENDIF.
    APPEND |(| TO rt_content.
*    APPEND |{ get_indention( ) }INDENT_LEVEL = { mv_indent_level }| TO rt_content.
    LOOP AT me->mt_components ASSIGNING FIELD-SYMBOL(<comp>).
*      rt_content = value #( BASE rt_content ( <fs_comp>->get_content( ) ) ).
*      APPEND LINES OF <comp>->get_content( ) TO rt_content.
      LOOP AT <comp>->get_content( i_max_line_length = i_max_line_length ) ASSIGNING FIELD-SYMBOL(<cont>).
        APPEND |{ get_indention( ) }{ <cont> }| TO rt_content.
      ENDLOOP.
    ENDLOOP.
    APPEND |)| TO rt_content.
  ENDMETHOD.
ENDCLASS.
