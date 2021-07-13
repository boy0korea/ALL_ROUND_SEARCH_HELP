FUNCTION zarsh_export_callfield.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(CONTEXT_ELEMENT) TYPE REF TO  IF_WD_CONTEXT_ELEMENT
*"       OPTIONAL
*"     REFERENCE(CONTEXT_ATTRIBUTE) TYPE  STRING OPTIONAL
*"  CHANGING
*"     REFERENCE(HELP_INFO) TYPE  HELP_INFO OPTIONAL
*"----------------------------------------------------------------------
  DATA: lv_string  TYPE string,
        lr_data    TYPE REF TO data,
        lv_help_id TYPE string.
  FIELD-SYMBOLS: <lv_data> TYPE data.


  IF 1 EQ 2.
** to return cursor position after closing the Performance Assistant
*  export CALLED_FOR_FIELD = help_infos-fieldname
*       CALLED_FOR_tab = help_infos-tabname to MEMORY ID 'CALLFIELD'.
    CALL FUNCTION 'HELP_START'.
  ENDIF.



  IF help_info-dynpprog IS NOT INITIAL AND help_info-dynprofld IS NOT INITIAL.
    lv_string = |({ help_info-dynpprog }){ help_info-dynprofld }|.
    ASSIGN (lv_string) TO <lv_data>.
  ENDIF.

  IF context_element IS NOT INITIAL AND context_attribute IS NOT INITIAL.
    lr_data = context_element->get_static_attributes_ref( ).
    ASSIGN lr_data->(context_attribute) TO <lv_data>.
  ENDIF.


  IF <lv_data> IS ASSIGNED.
    DESCRIBE FIELD <lv_data> HELP-ID lv_help_id.
    IF lv_help_id CS '-'.
      SPLIT lv_help_id AT '-' INTO help_info-tabname help_info-fieldname.
    ELSE.
      help_info-tabname = lv_help_id.
    ENDIF.

*   to return cursor position after closing the Performance Assistant
    EXPORT called_for_field = help_info-fieldname
         called_for_tab = help_info-tabname TO MEMORY ID 'CALLFIELD'.
  ENDIF.

ENDFUNCTION.
