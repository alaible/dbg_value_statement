CLASS zcx_tree_err DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS:
      BEGIN OF zcx_tree_err,
        msgid TYPE symsgid VALUE 'ZDBG',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_tree_err .
    CONSTANTS:
      BEGIN OF tree_error,
        msgid TYPE symsgid VALUE 'ZDBG',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'MV_CLASS',
        attr2 TYPE scx_attrname VALUE 'MV_METHOD',
        attr3 TYPE scx_attrname VALUE 'MV_EXCEPTION',
        attr4 TYPE scx_attrname VALUE '',
      END OF tree_error .
    DATA mv_class TYPE symsgv .
    DATA mv_method TYPE symsgv .
    DATA mv_exception TYPE symsgv .

    METHODS constructor
      IMPORTING
        !textid       LIKE if_t100_message=>t100key OPTIONAL
        !previous     LIKE previous OPTIONAL
        !mv_class     TYPE symsgv OPTIONAL
        !mv_method    TYPE symsgv OPTIONAL
        !mv_exception TYPE symsgv OPTIONAL .
    CLASS-METHODS: raise_from_sysubrc IMPORTING iv_classname TYPE string DEFAULT 'cl_column_tree_model' iv_method TYPE string iv_sysubrc LIKE sy-subrc RAISING zcx_tree_err.
  PRIVATE SECTION.
    CLASS-METHODS: raise_from_sysubrc_int IMPORTING iv_classname TYPE string iv_method TYPE string iv_sysubrc LIKE sy-subrc RAISING zcx_tree_err.
ENDCLASS.



CLASS ZCX_TREE_ERR IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->mv_class = mv_class .
    me->mv_method = mv_method .
    me->mv_exception = mv_exception .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_tree_err .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD raise_from_sysubrc.
    zcx_tree_err=>raise_from_sysubrc_int(
      EXPORTING
        iv_classname = to_upper( iv_classname )
        iv_method    = to_upper( iv_method )
        iv_sysubrc   = iv_sysubrc
    ).
*    CATCH zcx_tree_err.
  ENDMETHOD.
  METHOD RAISE_FROM_SYSUBRC_INT.
    DATA: lv_exception TYPE string.
**********************************************************************
    SELECT SINGLE FROM seosubco AS exc
            INNER JOIN seosubcodf AS df
                    ON exc~clsname = df~clsname
                   AND exc~cmpname = df~cmpname
                   AND exc~sconame = df~sconame
                FIELDS exc~sconame AS exception
                 WHERE exc~scotype = '1'
                   AND exc~clsname = @iv_classname
                   AND exc~cmpname = @iv_method
                   AND df~editorder = @iv_sysubrc
                  INTO @lv_exception.
    IF sy-subrc <> 0.
*** In case there is noch result -> return exception with default text
      RAISE EXCEPTION TYPE zcx_tree_err.
    ELSE.
      RAISE EXCEPTION TYPE zcx_tree_err
        EXPORTING
          textid       = zcx_tree_err=>tree_error
          mv_class     = CONV #( iv_classname )
          mv_method    = CONV #( iv_method )
          mv_exception = CONV #( lv_exception ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
