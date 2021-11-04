"Name: \PR:CL_WD_FREE_VALUE_SUGGEST_UTIL=CP\TY:LCL_DDIC_VALUE_SUGGEST\ME:EXECUTE_SUGGEST\SE:END\EI
ENHANCEMENT 0 ZE_ARSH_CL_WD_FREE_VALUE_SUG_U.
* field label.
  IF l_meta IS NOT INITIAL.
    DATA: zlt_columns_descr TYPE if_wd_value_suggest=>t_list_descr.

    SORT l_meta-fieldprop by shlplispos.
    LOOP AT l_meta-fieldprop INTO DATA(zls_fp) WHERE shlplispos > 0.
      READ TABLE l_meta-fielddescr INTO DATA(zls_fd) WITH KEY fieldname = zls_fp-fieldname.
      IF zls_fd-fieldname EQ lv_f4_field.
        APPEND VALUE #( caption = zls_fd-scrtext_l length = zls_fd-leng type = if_wd_value_suggest=>e_type-value visible = abap_true ) TO zlt_columns_descr.
      ELSE.
        APPEND VALUE #( caption = zls_fd-scrtext_l length = zls_fd-leng type = if_wd_value_suggest=>e_type-description visible = abap_true ) TO zlt_columns_descr.
      ENDIF.
    ENDLOOP.

    IF zlt_columns_descr IS NOT INITIAL.
        mo_suggest_handler->set_suggest_values( exporting values_list = <lt_ddic_shlp_data>
                 f4_column =  |{ lv_f4_field }|
                 columns_descr = zlt_columns_descr
                 highlight_columns = lo_highlight->get_search_columns_for_shlp( exporting i_f4_fieldname = |{ lv_f4_field }| i_shlp = l_meta )
                 highlight_similarity =  lo_highlight->get_similarity_for_shlp( exporting i_shlp = l_meta )
                                              ).
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
