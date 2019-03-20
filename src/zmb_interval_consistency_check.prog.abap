REPORT zmb_interval_consistency_check.

CLASS zcl_mb_ddic_helper DEFINITION
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    CLASS-METHODS does_table_exists
      IMPORTING
        i_tabname       TYPE tabname
      RETURNING
        VALUE(r_result) TYPE abap_bool.
    CLASS-METHODS get_dats_field_from_table
      IMPORTING
        i_table         TYPE tabname
      RETURNING
        VALUE(r_result) TYPE conv_ddic_all_tt.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_mb_ddic_helper IMPLEMENTATION.

  METHOD does_table_exists.

    DATA: return_code TYPE syst_subrc.

    CALL FUNCTION 'DB_EXISTS_TABLE'
      EXPORTING
        tabname = i_tabname
      IMPORTING
        subrc   = return_code.

    r_result = xsdbool( return_code = 0 ).

  ENDMETHOD.


  METHOD get_dats_field_from_table.

    SELECT * FROM dd03l INTO TABLE r_result WHERE tabname = i_table AND datatype = 'DATS'.

  ENDMETHOD.

ENDCLASS.


CLASS zcl_mb_search_help_datefields DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      search_help_date_field
        IMPORTING
                  i_progname   TYPE d020s-prog
                  i_screenname TYPE d020s-dnum
        CHANGING  c_date_field TYPE fieldname.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS get_table_name_from_sel_screen
      IMPORTING
        i_progname      TYPE d020s-prog
        i_screenname    TYPE sychar04
      RETURNING
        VALUE(r_result) TYPE tabname.

    CLASS-METHODS sel_dats_field_sh
      IMPORTING
        i_dats_fields   TYPE conv_ddic_all_tt
      RETURNING
        VALUE(r_result) TYPE fieldname.

ENDCLASS.


CLASS zcl_mb_search_help_datefields IMPLEMENTATION.

  METHOD search_help_date_field.

    DATA(table_name) = get_table_name_from_sel_screen(
        EXPORTING i_progname =  i_progname
                  i_screenname = i_screenname
    ).

    DATA(table_exists) = zcl_mb_ddic_helper=>does_table_exists( i_tabname = table_name ).

    IF table_exists = abap_false.
      MESSAGE: 'Fill parameter tablename first.' TYPE 'I'.
      RETURN.
    ENDIF.

    DATA(dats_fields) = zcl_mb_ddic_helper=>get_dats_field_from_table( i_table = table_name ).

    c_date_field = sel_dats_field_sh( i_dats_fields = dats_fields ).

  ENDMETHOD.

  METHOD get_table_name_from_sel_screen.

    DATA: dyprofields  TYPE TABLE OF dynpread.

    dyprofields = VALUE #( ( fieldname = 'PA_TAB' ) ).

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname     = i_progname
        dynumb     = i_screenname
      TABLES
        dynpfields = dyprofields.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE dyprofields INDEX 1 INTO DATA(dynpro_field).

    r_result = dynpro_field-fieldvalue.

    TRANSLATE  r_result TO UPPER CASE.

  ENDMETHOD.



  METHOD sel_dats_field_sh.

    TYPES: BEGIN OF ty_field,
             field TYPE fieldname,
           END OF ty_field.

    DATA: gwa_field  TYPE ty_field,
          gt_field   TYPE TABLE OF ty_field,
          gt_return  TYPE TABLE OF ddshretval,
          gwa_return TYPE ddshretval.

    LOOP AT  i_dats_fields   ASSIGNING FIELD-SYMBOL(<ls_dd03l>).
      gwa_field-field = <ls_dd03l>-fieldname.
      APPEND gwa_field TO gt_field.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield   = 'fieldname'
        value_org  = 'S'
      TABLES
        value_tab  = gt_field
        return_tab = gt_return.

    READ TABLE gt_return INTO gwa_return INDEX 1.
    IF sy-subrc = 0.
      r_result = gwa_return-fieldval.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS zcl_mb_report_flow DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF parameter,
        table TYPE tabname,
        from  TYPE fieldname,
        to    TYPE fieldname,
      END OF parameter .

    METHODS constructor
      IMPORTING
        !is_parameter TYPE parameter .

    METHODS process .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gs_parameter TYPE parameter.

    METHODS parametercheck
      IMPORTING
                !pa_tab         TYPE tabname
                !pa_from        TYPE fieldname
                !pa_to          TYPE fieldname
      RETURNING VALUE(r_sucess) TYPE abap_bool.

    METHODS db_select
      IMPORTING
        pa_tab    TYPE tabname
        pa_from   TYPE fieldname
        pa_to     TYPE fieldname
      EXPORTING
        et_result TYPE STANDARD TABLE .

    METHODS display
      CHANGING
        it_resulttable TYPE STANDARD TABLE .

    METHODS dats_fields_contains_dat_field
      IMPORTING
        it_datfields    TYPE conv_ddic_all_tt
        iv_datfield     TYPE fieldname
      RETURNING
        VALUE(r_result) TYPE abap_bool.
ENDCLASS.


CLASS zcl_mb_report_flow IMPLEMENTATION.

  METHOD constructor.
    me->gs_parameter = is_parameter.
  ENDMETHOD.

  METHOD parametercheck.

    DATA(table_exists) = zcl_mb_ddic_helper=>does_table_exists( i_tabname = pa_tab ).

    IF table_exists = abap_false.
      r_sucess = abap_false.
      RETURN.
    ENDIF.

    DATA(dats_fields) = zcl_mb_ddic_helper=>get_dats_field_from_table( i_table = pa_tab ).

    IF dats_fields_contains_dat_field(
        it_datfields = dats_fields
        iv_datfield  =  pa_from
        ) = abap_false.
      r_sucess = abap_false.
      RETURN.
    ENDIF.

    IF dats_fields_contains_dat_field(
        it_datfields = dats_fields
        iv_datfield  =  pa_to
        ) = abap_false.
      r_sucess = abap_false.
      RETURN.
    ENDIF.

    r_sucess = abap_true.

  ENDMETHOD.


  METHOD process.

    DATA(parameter_ok) =
        parametercheck(
                   pa_tab = gs_parameter-table
                   pa_from = gs_parameter-from
                   pa_to = gs_parameter-to ).

    IF parameter_ok = abap_false.
      MESSAGE 'Invalid parameters' TYPE 'E'.
      RETURN.
    ENDIF.

    " Allocating result_table
    FIELD-SYMBOLS: <result_table>     TYPE STANDARD TABLE.
    DATA: dref  TYPE REF TO data.
    CREATE DATA dref TYPE STANDARD TABLE OF (gs_parameter-table).
    ASSIGN dref->* TO <result_table>.

    db_select(
        EXPORTING
            pa_tab = gs_parameter-table
            pa_from = gs_parameter-from
            pa_to = gs_parameter-to
        IMPORTING
            et_result = <result_table>
     ).

    display(
        CHANGING
            it_resulttable = <result_table>
    ).

  ENDMETHOD.


  METHOD db_select.

    DATA: wherecondition TYPE string.
    wherecondition = | { pa_from } > { pa_tab }~{ pa_to } |.

    SELECT *
        FROM (pa_tab)
        INTO TABLE  et_result
        WHERE (wherecondition).

  ENDMETHOD.


  METHOD display.

    DATA alv TYPE REF TO cl_salv_table.

    cl_salv_table=>factory(
       IMPORTING
         r_salv_table = alv
       CHANGING
         t_table      =  it_resulttable  ).

    alv->display( ).

  ENDMETHOD.


  METHOD dats_fields_contains_dat_field.

    LOOP AT it_datfields TRANSPORTING NO FIELDS WHERE  fieldname  = iv_datfield.
      EXIT.
    ENDLOOP.
    r_result = xsdbool( sy-subrc = 0 ).

  ENDMETHOD.

ENDCLASS.


SELECTION-SCREEN: BEGIN OF BLOCK w_block_sel WITH FRAME TITLE TEXT-sel.
PARAMETERS: pa_tab  LIKE dd02l-tabname OBLIGATORY,
            pa_from TYPE fieldname OBLIGATORY,
            pa_to   TYPE fieldname OBLIGATORY.
SELECTION-SCREEN END OF BLOCK w_block_sel.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_from.
  zcl_mb_search_help_datefields=>search_help_date_field(
     EXPORTING i_progname =  sy-repid
               i_screenname = sy-dynnr
     CHANGING c_date_field = pa_from
   ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_to.
  zcl_mb_search_help_datefields=>search_help_date_field(
      EXPORTING i_progname =  sy-repid
                i_screenname = sy-dynnr
      CHANGING c_date_field = pa_to
    ).


START-OF-SELECTION.

  NEW zcl_mb_report_flow(
     VALUE zcl_mb_report_flow=>parameter(
         table         = pa_tab
         from         = pa_from
         to           = pa_to )
    )->process( ).
