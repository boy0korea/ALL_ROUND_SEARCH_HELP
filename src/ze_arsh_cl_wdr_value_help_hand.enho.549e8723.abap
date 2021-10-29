"Name: \PR:CL_WDR_VALUE_HELP_HANDLER=====CP\TY:LCL_DDIC_SUGGEST_HANDLER_2\ME:GET_FIELD_LABEL\SE:BEGIN\EI
ENHANCEMENT 0 ZE_ARSH_CL_WDR_VALUE_HELP_HAND.
* bug fix.
  READ TABLE m_shlp-fielddescr INTO data(zl_dfies) WITH KEY fieldname = component-name.
  IF sy-subrc EQ 0.
    clear field_label.
    field_label = zl_dfies-scrtext_m.
    if field_label is initial.
      field_label = zl_dfies-scrtext_l.
      if field_label is initial.
        field_label = zl_dfies-scrtext_s.
        if field_label is initial.
          field_label = zl_dfies-reptext.
        endif.
      endif.
    endif.
    IF field_label IS NOT INITIAL.
      RETURN.
    ENDIF.
  ENDIF.
ENDENHANCEMENT.
