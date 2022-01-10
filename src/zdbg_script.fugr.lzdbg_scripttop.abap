FUNCTION-POOL zdbg_script.                  "MESSAGE-ID ..

* INCLUDE LZDBG_SCRIPTD...                   " Local class definition

INCLUDE lzdbg_scriptcx1. "error-classes

INCLUDE lzdbg_scriptd07. "lcl_splitter_base

INCLUDE lzdbg_scriptd08. "lcl_tree_base

INCLUDE lzdbg_scriptd06. "lcl_match_cursor

INCLUDE lzdbg_scriptd04. "lcl_screen_objects/lcx_screen_error

INCLUDE lzdbg_scriptd09. "lcl_dd_handler

INCLUDE lzdbg_scriptd03. "lcl_screen_log

INCLUDE lzdbg_scriptd02. "lcl_screen_state

INCLUDE lzdbg_scriptd05. "lcl_screen_objects_col

INCLUDE lzdbg_scriptd01. "lcl_event_handler/lcl_alv_tree_objects

INCLUDE lzdbg_scripte01. "Included Selscreen

INCLUDE lzdbg_scriptd98. "Unit Test Data
INCLUDE lzdbg_scriptt99. "ABAP Unit tests

DATA: ok_code LIKE sy-ucomm.
DATA: ok_9400 LIKE sy-ucomm.

DATA: gv_searchfield   TYPE c LENGTH 150,
      max_linesize     TYPE i,
      flg_search_value TYPE xfeld,
      flag_filter      TYPE xfeld.

DATA: go_screen_state       TYPE REF TO lcl_screen_state,
      go_screen_objects_col TYPE REF TO lcl_screen_objects_col,
      log_config_dd_handler TYPE REF TO lcl_dd_handler.

DATA: log_level     TYPE lcl_dd_handler=>t_char_255,
      max_lines_log TYPE i.

DATA: cursor_info TYPE lcl_match_cursor=>t_cursor_info.

DATA: go_err TYPE REF TO cx_static_check.


DATA: txt_info_1 TYPE c LENGTH 100,
      txt_info_2 TYPE c LENGTH 100.

DATA: txt_log_1 TYPE c LENGTH 200,
      txt_log_2 TYPE c LENGTH 200,
      txt_log_3 TYPE c LENGTH 200,
      txt_log_4 TYPE c LENGTH 200.

DATA: log_lnr_1 TYPE c LENGTH 10,
      log_lnr_2 TYPE c LENGTH 10,
      log_lnr_3 TYPE c LENGTH 10,
      log_lnr_4 TYPE c LENGTH 10.

**********************************************************************
*** FB (list-processing)
DATA: go_root_ent TYPE REF TO zcl_compl_entity.
