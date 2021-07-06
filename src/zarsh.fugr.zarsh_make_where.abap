FUNCTION zarsh_make_where.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_RANGE_REF) TYPE  WDR_SO_T_RANGE_REF
*"  EXPORTING
*"     REFERENCE(EV_SQL_WHERE) TYPE  STRING
*"----------------------------------------------------------------------
  DATA: ls_range_ref      TYPE wdr_so_s_range_ref,
        lt_grouped_range  TYPE if_sadl_cond_provider_grpd_rng=>tt_grouped_range,
        ls_grouped_range  TYPE if_sadl_cond_provider_grpd_rng=>ty_grouped_range,
        lt_field_info     TYPE if_auth_condition_types=>tt_field_info,
        ls_field_info     TYPE if_auth_condition_types=>ty_field,
        lo_cond_to_sql    TYPE REF TO cl_sadl_cond_to_sql,
        lt_sadl_condition TYPE if_sadl_query_types=>tt_complex_condition,
        lv_sql            TYPE string.
  FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE.

  CLEAR: ev_sql_where.

  CHECK: it_range_ref IS NOT INITIAL.
  LOOP AT it_range_ref INTO ls_range_ref.
    CLEAR: ls_grouped_range, ls_field_info.
    ls_field_info-field_name = ls_field_info-field_alias = ls_range_ref-attribute.
    INSERT ls_field_info INTO TABLE lt_field_info.
    ls_grouped_range-column_name = ls_grouped_range-field_path = ls_range_ref-attribute.
    ASSIGN ls_range_ref-range->* TO <lt_data>.
    CHECK sy-subrc EQ 0.
    MOVE-CORRESPONDING <lt_data> TO ls_grouped_range-t_selopt.
    INSERT ls_grouped_range INTO TABLE lt_grouped_range.
  ENDLOOP.

  IF 1 EQ 2.
    " for test
    lt_grouped_range = VALUE #(
     ( column_name = 'COL1' field_path = 'COL1' t_selopt = VALUE #(
      ( sign = 'I' option = 'EQ' low = 'Val1' )
      ( sign = 'I' option = 'BT' low = '22' high = '33' )
      )
     )
     ( column_name = 'COL2' field_path = 'COL2' t_selopt = VALUE #(
      ( sign = 'E' option = 'EQ' low = 'Val2' )
      )
     )
    ).
    lt_field_info = VALUE #(
     ( field_name = 'COL1' field_alias = 'COL1' )
     ( field_name = 'COL2' field_alias = 'COL2' )
    ).
  ENDIF.

  cl_sadl_condition_provdr_fctry=>create_for_grouped_ranges(
    EXPORTING
      it_ranges         = lt_grouped_range
  )->get_condition(
    IMPORTING
      et_sadl_condition = lt_sadl_condition
  ).

  lo_cond_to_sql = NEW cl_sadl_cond_to_sql( cl_sadl_sql_statement=>create( ) ).
  lo_cond_to_sql->convert_sadl_cond_to_sql(
    EXPORTING
      it_sadl_conditions = lt_sadl_condition
      it_field_info      = lt_field_info
    RECEIVING
      rv_result          = ev_sql_where
  ).




ENDFUNCTION.
