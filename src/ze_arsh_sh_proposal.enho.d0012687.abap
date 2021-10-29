"Name: \PR:SAPLSDTB\FO:CONVERT_36_WITH_PROP\SE:END\EI
ENHANCEMENT 0 ZE_ARSH_SH_PROPOSAL.
* ZH_ALL_ROUND proposal.
  IF dd35v_tab-shlpname EQ 'ZH_ALL_ROUND' AND
     ( sy-ucomm EQ 'PREP' OR sy-ucomm EQ 'GOON' ).

    DATA: zlv_tabname  TYPE TABNAME,
          zlt_field    TYPE STRINGTAB,
          zlv_distinct TYPE FLAG,
          zlv_count    TYPE i,
          zlv_index    TYPE i.

    CALL FUNCTION 'ZARSH_PROPOSAL'
      EXPORTING
        iv_tabname   = dd35v_tab-tabname
        iv_fieldname = dd35v_tab-fieldname
      IMPORTING
        ev_tabname   = zlv_tabname
        et_field     = zlt_field
        ev_distinct  = zlv_distinct.
    IF zlv_tabname IS INITIAL.
      zlv_tabname = '**table name**'.
      APPEND '**field name**' TO zlt_field.
    ENDIF.
    zlv_count = lines( zlt_field ).

    LOOP AT dd36m_tab ASSIGNING FIELD-SYMBOL(<zls_dd36m>).
      CASE <zls_dd36m>-shlpfield.
        WHEN 'EV_FIELD1'.
          <zls_dd36m>-shtable = <zls_dd36m>-tabname.
          <zls_dd36m>-shfield = <zls_dd36m>-fieldname.
        WHEN 'IV_TABLE'.
          <zls_dd36m>-shtype = 'C'.
          <zls_dd36m>-shtable = |'{ zlv_tabname }'|.
        WHEN 'IV_DISTINCT'.
          IF zlv_distinct EQ abap_true.
            <zls_dd36m>-shtype = 'C'.
            <zls_dd36m>-shtable = |'X'|.
          ENDIF.
*        WHEN 'IV_FIELD1'.
*          <zls_dd36m>-shtype = 'C'.
*          <zls_dd36m>-shtable = |'{ zlt_field[ 1 ] }'|.
        WHEN OTHERS.
          IF <zls_dd36m>-shlpfield CP 'IV_FIELD*'.
            zlv_index = <zls_dd36m>-shlpfield+8.
            IF zlv_index <= zlv_count.
              <zls_dd36m>-shtype = 'C'.
              <zls_dd36m>-shtable = |'{ zlt_field[ zlv_index ] }'|.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDENHANCEMENT.
