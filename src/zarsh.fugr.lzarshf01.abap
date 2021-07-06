*----------------------------------------------------------------------*
***INCLUDE LZARSHF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form message
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM message  USING    iv_error_text TYPE clike.
  CHECK: iv_error_text IS NOT INITIAL.

  IF wdr_task=>application IS NOT INITIAL.
    " WD or FPM
    wdr_task=>application->component->if_wd_controller~get_message_manager( )->report_error_message(
      EXPORTING
        message_text = iv_error_text
    ).
  ELSE.
    " GUI
    MESSAGE iv_error_text TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
