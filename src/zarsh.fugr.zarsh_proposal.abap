FUNCTION zarsh_proposal.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_TABNAME) TYPE  CLIKE
*"     REFERENCE(IV_FIELDNAME) TYPE  CLIKE
*"  EXPORTING
*"     VALUE(EV_TABNAME) TYPE  TABNAME
*"     REFERENCE(ET_FIELD) TYPE  STRINGTAB
*"     REFERENCE(EV_DISTINCT) TYPE  FLAG
*"----------------------------------------------------------------------

  DATA: ls_dd04v           TYPE dd04v,
        lo_rtti            TYPE REF TO cl_abap_structdescr,
        lt_field_list      TYPE ddfields,
        lt_field_list_text TYPE ddfields,
        ls_field_list      TYPE dfies,
        ls_field_list_ref  TYPE dfies,
        lt_field           TYPE TABLE OF string,
        lv_field           TYPE string,
        lv_text_table      TYPE tabname,
        lv_index           TYPE i.

  CLEAR: et_field.
  ev_tabname = iv_tabname.

  " get domain value table.
  ls_dd04v-rollname = cl_abap_typedescr=>describe_by_name( iv_tabname && '-' && iv_fieldname )->get_relative_name( ).
  IF ls_dd04v-rollname IS NOT INITIAL.
    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name     = ls_dd04v-rollname
      IMPORTING
        dd04v_wa = ls_dd04v.
    IF ls_dd04v-entitytab IS NOT INITIAL.
      ev_tabname = ls_dd04v-entitytab.
    ENDIF.
  ENDIF.


  CHECK: ev_tabname IS NOT INITIAL.
  lo_rtti ?= cl_abap_structdescr=>describe_by_name( ev_tabname ).
  IF lo_rtti->get_ddic_header( )-tabform <> 'T'.  " T	Table/view stored transparently in the database
    CLEAR: ev_tabname.
    RETURN.
  ENDIF.
  lt_field_list = lo_rtti->get_ddic_field_list( ).
  SELECT SINGLE tabname
    INTO lv_text_table
    FROM dd08l
    WHERE as4local = 'A'
      AND checktable = ev_tabname
      AND frkart = 'TEXT'.
  IF lv_text_table IS NOT INITIAL.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = lv_text_table
      TABLES
        dfies_tab = lt_field_list_text.
  ENDIF.

  IF ls_dd04v-entitytab IS NOT INITIAL.
    " domain value table
    LOOP AT lt_field_list INTO ls_field_list TO 10.
      lv_field = ls_field_list-fieldname.
      APPEND lv_field TO et_field.
    ENDLOOP.
    IF lo_rtti->has_property( cl_abap_structdescr=>typepropkind_hasclient ).
      READ TABLE lt_field_list INTO ls_field_list WITH KEY datatype = 'CLNT'.
      DELETE et_field WHERE table_line = ls_field_list-fieldname.
    ENDIF.
    READ TABLE lt_field_list INTO ls_field_list WITH KEY domname = ls_dd04v-domname.
    IF sy-subrc EQ 0.
      lv_field = ls_field_list-fieldname.
      DELETE et_field WHERE table_line = lv_field.
      INSERT lv_field INTO et_field INDEX 1.
    ENDIF.
  ELSE.
    " self table one column
    READ TABLE lt_field_list INTO ls_field_list WITH KEY fieldname = iv_fieldname.
    IF sy-subrc <> 0.
      READ TABLE lt_field_list INTO ls_field_list WITH KEY domname = ls_dd04v-domname.
      IF sy-subrc <> 0.
        READ TABLE lt_field_list INTO ls_field_list WITH KEY leng = ls_dd04v-leng.
        IF sy-subrc <> 0.
          READ TABLE lt_field_list INTO ls_field_list INDEX 1.
        ENDIF.
      ENDIF.
    ENDIF.
    lv_index = sy-tabix - 1.
    lv_field = ls_field_list-fieldname.
    APPEND lv_field TO et_field.
    IF ls_field_list-reffield IS NOT INITIAL AND
       line_exists( lt_field_list[ fieldname = ls_field_list-reffield ] ).
      lv_field = ls_field_list-reffield.
      APPEND lv_field TO et_field.
    ENDIF.
    IF ls_field_list-keyflag EQ abap_true AND lv_index > 1.
      LOOP AT lt_field_list INTO ls_field_list TO lv_index WHERE datatype <> 'CLNT'.
        lv_field = ls_field_list-fieldname.
        APPEND lv_field TO lt_field.
      ENDLOOP.
      INSERT LINES OF lt_field INTO et_field INDEX 1.
    ENDIF.

    " DISTINCT on
    ev_distinct = abap_true.
  ENDIF.


  IF lv_text_table IS NOT INITIAL.
    IF lines( et_field ) > 6.
      DELETE et_field FROM 7.
    ENDIF.
    LOOP AT lt_field_list_text INTO ls_field_list WHERE keyflag = abap_false.
      lv_field = ls_field_list-fieldname.
      APPEND lv_field TO et_field.
    ENDLOOP.
  ENDIF.



ENDFUNCTION.
