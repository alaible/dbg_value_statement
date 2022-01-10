CLASS zcl_value_content DEFINITION PUBLIC CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: constructor IMPORTING it_content TYPE zcl_entity=>tty_string,
      get_content RETURNING VALUE(rt_cont) TYPE zcl_entity=>tty_string.

  PRIVATE SECTION.
    DATA: mt_content TYPE zcl_entity=>tty_string.
ENDCLASS.



CLASS ZCL_VALUE_CONTENT IMPLEMENTATION.


  METHOD constructor.
    me->mt_content = it_content.
  ENDMETHOD.


  METHOD get_content.
    rt_cont = me->mt_content.
  ENDMETHOD.
ENDCLASS.
