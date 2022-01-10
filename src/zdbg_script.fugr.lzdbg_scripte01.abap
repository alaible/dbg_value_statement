*&---------------------------------------------------------------------*
*& Include          LZDBG_SCRIPTE01
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 1010 TITLE TEXT-001 AS WINDOW.
PARAMETERS: p_var TYPE c LENGTH 50.
SELECTION-SCREEN ULINE.
PARAMETERS: p_dbg TYPE c AS CHECKBOX DEFAULT abap_false.
SELECTION-SCREEN END OF SCREEN 1010.
