FUNCTION zarsh_f4.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------
* https://github.com/boy0korea/ALL_ROUND_SEARCH_HELP
  DATA: ls_help_info        TYPE help_info,
        ls_selopt           TYPE ddshselopt,
        ls_fieldprop        TYPE ddshfprop,
        lt_fielddescr       TYPE TABLE OF dfies,
        ls_fielddescr       TYPE dfies,
        ls_fieldiface       TYPE ddshiface,
        lt_data_suggest     TYPE TABLE OF zarsh_dummy,
        ls_data_suggest     TYPE zarsh_dummy,
        lv_table            TYPE tabname,
        lv_distinct         TYPE flag,
        lv_no_fuzzy         TYPE flag,
        lt_const            TYPE tihttpnvp,
        ls_const            TYPE ihttpnvp,
        lt_range_ref        TYPE wdr_so_t_range_ref,
        lt_string           TYPE TABLE OF string,
        lv_string           TYPE string,
        lo_result_rtti      TYPE REF TO cl_abap_structdescr,
        lt_result_comp      TYPE abap_component_tab,
        ls_result_comp      TYPE abap_componentdescr,
        lv_sql_where        TYPE string,
        lv_sql_select       TYPE string,
        lv_sql              TYPE string,
        lr_data             TYPE REF TO data,
        lo_result           TYPE REF TO cl_sql_result_set,
        ls_x030l            TYPE x030l,
        lt_field_list       TYPE ddfields,
        lt_field_list_text  TYPE ddfields,
        ls_field_list       TYPE dfies,
        ls_field_list_ref   TYPE dfies,
        lt_field            TYPE TABLE OF string,
        lv_field            TYPE string,
        lv_text_table       TYPE tabname,
        lv_text_filed_exist TYPE flag,
        lv_maxnum           TYPE i,
        lv_num_fields       TYPE i,
        lv_field_length     TYPE i,
        lv_offset           TYPE i,
        lv_mod4             TYPE i,
        lv_index            TYPE i.
  FIELD-SYMBOLS: <lt_data>         TYPE table,
                 <ls_data>         TYPE data,
                 <lv_data>         TYPE data,
                 <lv_data_ref>     TYPE data,
                 <lv_data_suggest> TYPE data,
                 <ls_range_ref>    TYPE wdr_so_s_range_ref,
                 <lt_range>        TYPE sabp_t_range_options,
                 <ls_range>        TYPE sabp_s_range_option,
                 <ls_fielddescr>   TYPE dfies,
                 <ls_fieldprop>    TYPE ddshfprop.

  CHECK: callcontrol-step EQ 'SELECT' OR callcontrol-step EQ 'SELONE'.
  CLEAR: record_tab[].

  lt_field = VALUE #( ( ) ( ) ( ) ( ) ( ) ( ) ( ) ( ) ( ) ( ) ).
  lt_const = VALUE #( ( ) ( ) ( ) ( ) ( ) ( ) ( ) ( ) ( ) ( ) ).

  " shlp-selopt
  LOOP AT shlp-selopt INTO ls_selopt.
    CASE ls_selopt-shlpfield.
      WHEN 'IV_TABLE'.
        lv_table = ls_selopt-low.
      WHEN 'IV_DISTINCT'.
        lv_distinct = ls_selopt-low.
      WHEN 'IV_NO_FUZZY'.
        lv_no_fuzzy = ls_selopt-low.
      WHEN OTHERS.
        IF ls_selopt-shlpfield CP 'IV_FIELD+'.
          lv_index = ls_selopt-shlpfield+8.
          lt_field[ lv_index ] = ls_selopt-low.
        ELSEIF ls_selopt-shlpfield CP 'IV_CONST_FIELD++'.
          lv_index = ls_selopt-shlpfield+14.
          lt_const[ lv_index ]-name = ls_selopt-low.
        ELSEIF ls_selopt-shlpfield CP 'IV_CONST_VALUE++'.
          lv_index = ls_selopt-shlpfield+14.
          lt_const[ lv_index ]-value = ls_selopt-low.
        ELSE.
          AT NEW shlpfield.
            APPEND INITIAL LINE TO lt_range_ref ASSIGNING <ls_range_ref>.
            <ls_range_ref>-attribute = ls_selopt-shlpfield.
            CREATE DATA <ls_range_ref>-range TYPE sabp_t_range_options.
            ASSIGN <ls_range_ref>-range->* TO <lt_range>.
          ENDAT.
          APPEND INITIAL LINE TO <lt_range> ASSIGNING <ls_range>.
          MOVE-CORRESPONDING ls_selopt TO <ls_range>.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  " alpha conversion
  LOOP AT shlp-fielddescr INTO ls_fielddescr WHERE convexit = 'ALPHA' AND leng IS NOT INITIAL.
    READ TABLE lt_range_ref ASSIGNING <ls_range_ref> WITH KEY attribute = ls_fielddescr-fieldname.
    CHECK: sy-subrc EQ 0.
    ASSIGN <ls_range_ref>-range->* TO <lt_range>.
    CHECK: sy-subrc EQ 0 AND <lt_range> IS NOT INITIAL.

    IF lv_field_length <> ls_fielddescr-leng.
      lv_field_length = ls_fielddescr-leng.
      DATA(lo_range_value) = cl_abap_elemdescr=>get_c( lv_field_length ).
      DATA(lo_range_table) = cl_abap_tabledescr=>create(
        cl_abap_structdescr=>create(
          VALUE #(
            ( name = 'SIGN' type = cl_abap_elemdescr=>get_c( 1 ) )
            ( name = 'OPTION' type = cl_abap_elemdescr=>get_c( 2 ) )
            ( name = 'LOW' type = lo_range_value )
            ( name = 'HIGH' type = lo_range_value )
          )
        )
      ).
      CREATE DATA lr_data TYPE HANDLE lo_range_value.
      ASSIGN lr_data->* TO <lv_data>.
      CREATE DATA lr_data TYPE HANDLE lo_range_table.
      ASSIGN lr_data->* TO <lt_data>.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_RANGE_I'
      EXPORTING
        input     = <lv_data>
      IMPORTING
        output    = <lv_data>
      TABLES
        range_int = <lt_data>   " converted data
        range_ext = <lt_range>.

    MOVE-CORRESPONDING <lt_data> TO <lt_range>.
  ENDLOOP.

  " shlp-fieldprop
  LOOP AT shlp-fieldprop INTO ls_fieldprop WHERE defaultval IS NOT INITIAL.
    UNASSIGN <lv_data>.
    CASE ls_fieldprop-fieldname.
      WHEN 'IV_TABLE'.
        ASSIGN lv_table TO <lv_data>.
      WHEN 'IV_DISTINCT'.
        ASSIGN lv_distinct TO <lv_data>.
      WHEN 'IV_NO_FUZZY'.
        ASSIGN lv_no_fuzzy TO <lv_data>.
      WHEN OTHERS.
        IF ls_selopt-shlpfield CP 'IV_FIELD+'.
          lv_index = ls_selopt-shlpfield+8.
          ASSIGN lt_field[ lv_index ] TO <lv_data>.
        ELSEIF ls_selopt-shlpfield CP 'IV_CONST_FIELD++'.
          lv_index = ls_selopt-shlpfield+14.
          ASSIGN lt_const[ lv_index ]-name TO <lv_data>.
        ELSEIF ls_selopt-shlpfield CP 'IV_CONST_VALUE++'.
          lv_index = ls_selopt-shlpfield+14.
          ASSIGN lt_const[ lv_index ]-value TO <lv_data>.
        ENDIF.
    ENDCASE.
    IF <lv_data> IS ASSIGNED AND <lv_data> IS INITIAL.
      <lv_data> = ls_fieldprop-defaultval.
      REPLACE ALL OCCURRENCES OF `'` IN <lv_data> WITH ''.
    ENDIF.
  ENDLOOP.

  " shlp-interface
  LOOP AT shlp-interface INTO ls_fieldiface WHERE value IS NOT INITIAL.
    UNASSIGN <lv_data>.
    CASE ls_fieldiface-shlpfield.
      WHEN 'IV_TABLE'.
        ASSIGN lv_table TO <lv_data>.
      WHEN 'IV_DISTINCT'.
        ASSIGN lv_distinct TO <lv_data>.
      WHEN 'IV_NO_FUZZY'.
        ASSIGN lv_no_fuzzy TO <lv_data>.
      WHEN OTHERS.
        IF ls_selopt-shlpfield CP 'IV_FIELD+'.
          lv_index = ls_selopt-shlpfield+8.
          ASSIGN lt_field[ lv_index ] TO <lv_data>.
        ELSEIF ls_selopt-shlpfield CP 'IV_CONST_FIELD++'.
          lv_index = ls_selopt-shlpfield+14.
          ASSIGN lt_const[ lv_index ]-name TO <lv_data>.
        ELSEIF ls_selopt-shlpfield CP 'IV_CONST_VALUE++'.
          lv_index = ls_selopt-shlpfield+14.
          ASSIGN lt_const[ lv_index ]-value TO <lv_data>.
        ENDIF.
    ENDCASE.
    IF <lv_data> IS ASSIGNED AND <lv_data> IS INITIAL.
      <lv_data> = ls_fieldiface-value.
    ENDIF.
  ENDLOOP.

  DELETE lt_field WHERE table_line IS INITIAL.
  DELETE lt_const WHERE table_line IS INITIAL.

  " case: no parameter
  IF lv_table IS INITIAL.
*    SPLIT shlp-interface[ f4field = abap_true ]-valfield AT '-' INTO lv_table lv_field.

    IMPORT called_for_field = ls_help_info-fieldname
           called_for_tab = ls_help_info-tabname FROM MEMORY ID 'CALLFIELD'.
    lv_table = ls_help_info-tabname.
    lv_field = ls_help_info-fieldname.
    CLEAR: lt_field.

    IF lv_field IS NOT INITIAL.
      CALL FUNCTION 'ZARSH_PROPOSAL'
        EXPORTING
          iv_tabname   = lv_table
          iv_fieldname = lv_field
        IMPORTING
          ev_tabname   = lv_table
          et_field     = lt_field
          ev_distinct  = lv_distinct.
    ENDIF.
  ENDIF.

  IF lv_table IS INITIAL OR
     lt_field IS INITIAL.
    callcontrol-step = 'EXIT'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = lv_table
    IMPORTING
      x030l_wa  = ls_x030l
    TABLES
      dfies_tab = lt_field_list.
  CHECK: ls_x030l-tabform EQ 'T'.  " T  Table/view stored transparently in the database
  SELECT SINGLE tabname
    INTO lv_text_table
    FROM dd08l
    WHERE as4local = 'A'
      AND checktable = lv_table
      AND frkart = 'TEXT'.
  IF lv_text_table IS NOT INITIAL.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = lv_text_table
      TABLES
        dfies_tab = lt_field_list_text.
    LOOP AT lt_field_list_text INTO ls_field_list WHERE keyflag = abap_false.
      APPEND ls_field_list TO lt_field_list.
    ENDLOOP.
  ENDIF.
  SORT lt_field_list BY fieldname.


  CLEAR: lt_string.
  LOOP AT lt_field INTO lv_field.
    READ TABLE lt_field_list INTO ls_field_list WITH KEY fieldname = lv_field BINARY SEARCH.
    IF sy-subrc <> 0.
      DELETE lt_field.
      CONTINUE.
    ENDIF.
    IF ls_field_list-tabname EQ lv_text_table.
      lv_text_filed_exist = abap_true.
    ENDIF.
    APPEND |"{ lv_field }"| TO lt_string.
    ls_result_comp-name = lv_field.
    ls_result_comp-type ?= cl_abap_elemdescr=>describe_by_name( |{ ls_field_list-tabname }-{ ls_field_list-fieldname }| ).
    APPEND ls_result_comp TO lt_result_comp.
  ENDLOOP.
  IF lv_text_table IS NOT INITIAL AND
     lv_text_filed_exist EQ abap_false.
    IF lines( lt_field ) >= 7.
      DELETE lt_field FROM 7.
      DELETE lt_string FROM 7.
      DELETE lt_result_comp FROM 7.
    ENDIF.
    LOOP AT lt_field_list_text INTO ls_field_list WHERE keyflag = abap_false.
      lv_field = ls_field_list-fieldname.
      APPEND lv_field TO lt_field.
      APPEND |"{ lv_field }"| TO lt_string.
      ls_result_comp-name = lv_field.
      ls_result_comp-type ?= cl_abap_elemdescr=>describe_by_name( |{ ls_field_list-tabname }-{ ls_field_list-fieldname }| ).
      APPEND ls_result_comp TO lt_result_comp.
      IF sy-tabix EQ 9.
        " max index of EV_FIELD is 9
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CONCATENATE LINES OF lt_string INTO lv_sql_select SEPARATED BY `, `.
  lo_result_rtti = cl_abap_structdescr=>create( lt_result_comp ).
  CREATE DATA lr_data TYPE HANDLE lo_result_rtti.
  ASSIGN lr_data->* TO <ls_data>.
  CREATE DATA lr_data LIKE TABLE OF <ls_data>.
  ASSIGN lr_data->* TO <lt_data>.


  IF callcontrol-step EQ 'SELECT'.

    IF lv_distinct EQ abap_true.
      lv_sql = |SELECT DISTINCT|.
    ELSE.
      lv_sql = |SELECT|.
    ENDIF.
    lv_sql = |{ lv_sql } { lv_sql_select } FROM "{ lv_table }"|.

    IF lv_text_table IS NOT INITIAL.
      lv_sql = |{ lv_sql } AS A LEFT OUTER JOIN "{ lv_text_table }" AS T|.
      LOOP AT lt_field_list_text INTO ls_field_list WHERE keyflag = abap_true.
        lv_index = sy-tabix.
        IF lv_index EQ 1.
          lv_sql = |{ lv_sql } ON A."{ ls_field_list-fieldname }" = T."{ ls_field_list-fieldname }"|.
        ELSEIF ls_field_list-datatype EQ 'LANG'.
          lv_sql = |{ lv_sql } AND T."{ ls_field_list-fieldname }" = '{ sy-langu }'|.
        ELSE.
          lv_sql = |{ lv_sql } AND A."{ ls_field_list-fieldname }" = T."{ ls_field_list-fieldname }"|.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF cl_dsh_type_ahead_processor=>type_ahead_active EQ abap_true.
      " type ahead = value suggests
      " it runs only textsearch.
      IF shlp-textsearch-request IS INITIAL.
        shlp-textsearch-request = shlp-selopt[ 1 ]-low.
      ENDIF.
    ELSEIF lt_range_ref IS NOT INITIAL.
      LOOP AT lt_range_ref ASSIGNING <ls_range_ref> WHERE attribute CP 'EV_FIELD*'.
        lv_index = <ls_range_ref>-attribute+8.
        <ls_range_ref>-attribute = lt_field[ lv_index ].
      ENDLOOP.
      CALL FUNCTION 'ZARSH_MAKE_WHERE'
        EXPORTING
          it_range_ref = lt_range_ref
        IMPORTING
          ev_sql_where = lv_sql_where.
      IF lv_sql_where IS NOT INITIAL.
        LOOP AT lt_range_ref ASSIGNING <ls_range_ref>.
          lv_field = <ls_range_ref>-attribute.
          REPLACE ALL OCCURRENCES OF | { lv_field } | IN lv_sql_where WITH | "{ lv_field }" |.
        ENDLOOP.
        lv_sql_where = | AND ( { lv_sql_where } )|.
      ENDIF.
    ENDIF.

    " MANDT
    IF ls_x030l-clpos IS NOT INITIAL.
      READ TABLE lt_field_list INTO ls_field_list WITH KEY position = ls_x030l-clpos.
      lv_sql_where = |{ lv_sql_where } AND "{ ls_field_list-fieldname }" = { sy-mandt }|.
    ENDIF.

    " constant field & value
    IF lt_const IS NOT INITIAL.
      SORT lt_const BY name value.
      CLEAR: lt_string.
      LOOP AT lt_const INTO ls_const WHERE name IS NOT INITIAL.
        APPEND |'{ ls_const-value }'| TO lt_string.
        AT END OF name.
          CONCATENATE LINES OF lt_string INTO lv_string SEPARATED BY ','.
          IF lines( lt_string ) EQ 1.
            lv_sql_where = |{ lv_sql_where } AND "{ ls_const-name }" = { lv_string }|.
          ELSE.
            lv_sql_where = |{ lv_sql_where } AND "{ ls_const-name }" IN ({ lv_string })|.
          ENDIF.
          CLEAR: lt_string.
        ENDAT.
      ENDLOOP.
    ENDIF.

    " shlp-textsearch
    IF shlp-textsearch-request IS NOT INITIAL.
      IF lv_no_fuzzy EQ abap_false.
        IF shlp-intdescr-fuzzy_search IS NOT INITIAL AND
           shlp-intdescr-fuzzy_similarity < 1.
          LOOP AT lt_field INTO lv_field.
            READ TABLE lt_field_list TRANSPORTING NO FIELDS WITH KEY fieldname = lv_field inttype = cl_abap_typedescr=>typekind_char BINARY SEARCH.
            IF sy-subrc <> 0.
              lv_no_fuzzy = abap_true.
              EXIT.
            ENDIF.
          ENDLOOP.
        ELSE.
          lv_no_fuzzy = abap_true.
        ENDIF.
      ENDIF.

      lv_sql_where = |{ lv_sql_where } AND CONTAINS(|.
      lv_sql_where = |{ lv_sql_where }( { lv_sql_select }),|.
      lv_string = '*' && shlp-textsearch-request && '*'.
      lv_string = cl_abap_dyn_prg=>quote( lv_string ).
      REPLACE ALL OCCURRENCES OF '%' IN lv_string WITH '\%'.
      lv_sql_where = |{ lv_sql_where }{ lv_string },|.
      IF lv_no_fuzzy EQ abap_false.
        lv_sql_where = |{ lv_sql_where }fuzzy({ shlp-intdescr-fuzzy_similarity },'similarCalculationMode=search'))|.
      ELSE.
        lv_sql_where = |{ lv_sql_where }exact)|.
      ENDIF.
      IF lv_sql NP 'SELECT DISTINCT *'.
        lv_sql_where = |{ lv_sql_where } ORDER BY SCORE() DESC, "{ lt_field[ 1 ] }" ASC|.
      ENDIF.
    ENDIF.

    REPLACE FIRST OCCURRENCE OF | AND | IN lv_sql_where WITH | WHERE |.
    lv_sql = |{ lv_sql } { lv_sql_where }|.
    FIND | ORDER BY | IN lv_sql.
    IF sy-subrc <> 0.
      lv_sql = |{ lv_sql } ORDER BY "{ lt_field[ 1 ] }" ASC|.
    ENDIF.
    callcontrol-sortoff = abap_true.

    " maxrecords
    IF callcontrol-maxrecords IS NOT INITIAL.
      lv_maxnum = callcontrol-maxrecords + 1.
      lv_sql = |{ lv_sql } LIMIT { lv_maxnum }|.
    ENDIF.

    IF lv_text_table IS NOT INITIAL.
      LOOP AT lt_field_list INTO ls_field_list.
        lv_field = ls_field_list-fieldname.
        IF ls_field_list-tabname EQ lv_table.
          REPLACE ALL OCCURRENCES OF | "{ lv_field }"| IN lv_sql WITH | A."{ lv_field }"|.
        ELSEIF ls_field_list-tabname EQ lv_text_table..
          REPLACE ALL OCCURRENCES OF | "{ lv_field }"| IN lv_sql WITH | T."{ lv_field }"|.
        ENDIF.
      ENDLOOP.
    ENDIF.


    TRY .
        NEW cl_sql_statement( )->execute_query(
          EXPORTING
            statement   = lv_sql  " SELECT Statement Being Executed
          RECEIVING
            result_set  = lo_result " Database Cursor
        ).

        lo_result->set_param_table( lr_data ).
        lo_result->next_package( ).
        lo_result->close( ).
      CATCH cx_sql_exception INTO DATA(lo_cx_sql_exception).
*          iv_text = lo_cx_sql_exception->sql_message.
        PERFORM message USING lo_cx_sql_exception->sql_message.
    ENDTRY.

    " maxexceed
    IF callcontrol-maxrecords IS NOT INITIAL AND
      lines( <lt_data> ) > callcontrol-maxrecords.
      DELETE <lt_data> FROM callcontrol-maxrecords + 1.
      callcontrol-maxexceed = abap_true.
    ENDIF.

  ENDIF.


  IF cl_dsh_type_ahead_processor=>type_ahead_active EQ abap_true.
    " type ahead = value suggests

    LOOP AT <lt_data> ASSIGNING <ls_data>.
      CLEAR: ls_data_suggest.
      LOOP AT lt_field INTO lv_field.
        lv_index = sy-tabix.
        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_data> TO <lv_data>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lv_index OF STRUCTURE ls_data_suggest TO <lv_data_suggest>.
        WRITE <lv_data> TO <lv_data_suggest>.
        CONDENSE <lv_data_suggest>.
      ENDLOOP.

      APPEND ls_data_suggest TO lt_data_suggest.
    ENDLOOP.

* field desc
    lv_index = lines( lt_field ) + 1.
    IF lv_index <= 9.
      lv_field = 'EV_FIELD' && lv_index.
      DELETE shlp-fielddescr WHERE fieldname BETWEEN lv_field AND 'EV_FIELD9'.
      DELETE shlp-fieldprop WHERE fieldname BETWEEN lv_field AND 'EV_FIELD9'.
    ENDIF.

    LOOP AT lt_field INTO lv_field.
      lv_index = sy-tabix.
      READ TABLE lt_field_list INTO ls_field_list WITH KEY fieldname = lv_field BINARY SEARCH.
      READ TABLE shlp-fielddescr ASSIGNING <ls_fielddescr> WITH KEY fieldname = lv_field.
      IF sy-subrc EQ 0.
        " label text
        <ls_fielddescr>-reptext = ls_field_list-reptext.
        <ls_fielddescr>-scrtext_s = ls_field_list-scrtext_s.
        <ls_fielddescr>-scrtext_m = ls_field_list-scrtext_m.
        <ls_fielddescr>-scrtext_l = ls_field_list-scrtext_l.
      ENDIF.
      READ TABLE shlp-fielddescr ASSIGNING <ls_fielddescr> WITH KEY fieldname = 'EV_FIELD' && lv_index.
      IF sy-subrc EQ 0.
        " label text
        <ls_fielddescr>-reptext = ls_field_list-reptext.
        <ls_fielddescr>-scrtext_s = ls_field_list-scrtext_s.
        <ls_fielddescr>-scrtext_m = ls_field_list-scrtext_m.
        <ls_fielddescr>-scrtext_l = ls_field_list-scrtext_l.
      ENDIF.

      IF ls_field_list-reffield IS NOT INITIAL.
        READ TABLE lt_field_list INTO ls_field_list_ref WITH KEY fieldname = ls_field_list-reffield BINARY SEARCH.
        IF sy-subrc EQ 0.
          LOOP AT <lt_data> ASSIGNING <ls_data>.
            ASSIGN COMPONENT 'EV_FIELD' && lv_index OF STRUCTURE lt_data_suggest[ sy-tabix ] TO <lv_data_suggest>.
            ASSIGN COMPONENT ls_field_list-fieldname OF STRUCTURE <ls_data> TO <lv_data>.
            ASSIGN COMPONENT ls_field_list_ref-fieldname OF STRUCTURE <ls_data> TO <lv_data_ref>.
            CASE ls_field_list_ref-datatype.
              WHEN 'UNIT'.
                WRITE <lv_data> TO <lv_data_suggest> UNIT <lv_data_ref>.
              WHEN 'CUKY' .
                WRITE <lv_data> TO <lv_data_suggest> CURRENCY <lv_data_ref>.
            ENDCASE.
            CONDENSE <lv_data_suggest>.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.
    LOOP AT shlp-fieldprop ASSIGNING <ls_fieldprop> WHERE shlplispos IS NOT INITIAL.
      <ls_fieldprop>-shlpselpos = <ls_fieldprop>-shlplispos.
    ENDLOOP.


* map
    CALL FUNCTION 'F4UT_RESULTS_MAP'
      TABLES
        shlp_tab    = shlp_tab
        record_tab  = record_tab
        source_tab  = lt_data_suggest
      CHANGING
        shlp        = shlp
        callcontrol = callcontrol.

  ELSE.
    " F4 help

* field desc
    CLEAR: shlp-fielddescr[], ls_fielddescr.

    LOOP AT lt_field INTO lv_field.
      lv_index = sy-tabix.

      READ TABLE lt_field_list INTO ls_fielddescr WITH KEY fieldname = lv_field BINARY SEARCH.

      ls_fielddescr-tabname = lv_table.
      ls_fielddescr-fieldname = lv_field.
      ls_fielddescr-position = lv_index.
      ls_fielddescr-offset = lv_offset.
      IF ls_fielddescr-leng EQ 0 OR ls_fielddescr-leng > 200.
        " max length = 200
        ls_fielddescr-leng = 200.
        ls_fielddescr-intlen = 400.
        ls_fielddescr-outputlen = 200.
      ENDIF.
      lv_offset = ls_fielddescr-offset + ls_fielddescr-intlen.
      lv_mod4 = lv_offset MOD 4.
      IF lv_mod4 > 0.
        lv_offset = lv_offset + 4 - lv_mod4.
      ENDIF.
      IF lv_offset > 2000.
        " cut over
        EXIT.
      ENDIF.
      APPEND ls_fielddescr TO shlp-fielddescr.

    ENDLOOP.

    lv_index = lines( lt_field ) + 1.
    IF lv_index <= 9.
      lv_field = 'EV_FIELD' && lv_index.
      DELETE shlp-fieldprop WHERE fieldname BETWEEN lv_field AND 'EV_FIELD9'.
    ENDIF.

    LOOP AT shlp-fielddescr INTO ls_fielddescr.
      lv_index = sy-tabix.
      READ TABLE shlp-fieldprop TRANSPORTING NO FIELDS WITH KEY fieldname = ls_fielddescr-fieldname.
      IF sy-subrc <> 0.
        CLEAR: ls_fieldprop.
        ls_fieldprop-fieldname = ls_fielddescr-fieldname.
        ls_fieldprop-shlpselpos = lv_index.
        APPEND ls_fieldprop TO shlp-fieldprop.
      ENDIF.

      READ TABLE lt_field TRANSPORTING NO FIELDS WITH KEY table_line = ls_fielddescr-fieldname.
      IF sy-subrc EQ 0.
        lv_index = sy-tabix.
        ls_fielddescr-fieldname = 'EV_FIELD' && lv_index.
        CLEAR: ls_fielddescr-position.
        APPEND ls_fielddescr TO lt_fielddescr.
      ENDIF.
    ENDLOOP.
    APPEND LINES OF lt_fielddescr TO shlp-fielddescr.


* map
    CALL FUNCTION 'F4UT_RESULTS_MAP'
      TABLES
        shlp_tab    = shlp_tab
        record_tab  = record_tab
        source_tab  = <lt_data>
      CHANGING
        shlp        = shlp
        callcontrol = callcontrol.

  ENDIF.



  IF callcontrol-step EQ 'SELECT'.
    callcontrol-step = 'DISP'.
  ENDIF.

  READ TABLE shlp_tab TRANSPORTING NO FIELDS WITH KEY shlpname = shlp-shlpname.
  MODIFY shlp_tab INDEX sy-tabix FROM shlp.

ENDFUNCTION.
