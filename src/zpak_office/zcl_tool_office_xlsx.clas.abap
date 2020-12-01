CLASS zcl_tool_office_xlsx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_columninfo,
        column       TYPE i,
        column_field TYPE string,
        column_table TYPE string,
        column_name  TYPE string,
      END OF ty_columninfo .
    TYPES:
      ty_t_columninfo TYPE STANDARD TABLE OF ty_columninfo WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_sheet_info,
        name      TYPE string,
        sheet_id  TYPE i,
        comp_info TYPE abap_component_tab,
        col_info  TYPE  ty_t_columninfo,
        content   TYPE REF TO  data,
      END OF ty_sheet_info .
    TYPES:
        ty_t_sheet_info TYPE TABLE OF ty_sheet_info
                      WITH KEY name .
    DATA t_sheet_info TYPE ty_t_sheet_info.

    METHODS import_excel_single_sheet
      IMPORTING
        !iv_filepath          TYPE string
        !iv_sheetname         TYPE string OPTIONAL
        !iv_start_row         TYPE i DEFAULT 2
        !iv_title_row         TYPE i DEFAULT 1
      EXPORTING
        VALUE(et_column_info) TYPE ty_t_columninfo
      CHANGING
        !et_content           TYPE STANDARD TABLE
      RETURNING
        VALUE(rv_message)     TYPE string .

    METHODS import_excel_mult_sheets
      IMPORTING
        !iv_filepath      TYPE string
        !iv_start_row     TYPE i DEFAULT 2
        !iv_title_row     TYPE i DEFAULT 1
      CHANGING
        !et_content       TYPE STANDARD TABLE
      RETURNING
        VALUE(rv_message) TYPE string .

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_components
      IMPORTING
        it_content  TYPE ANY TABLE
      EXPORTING
        et_comp_tab TYPE abap_component_tab.
    METHODS get_column_name
      IMPORTING
        !iv_title_row TYPE i DEFAULT 1
        !icl_sheet    TYPE REF TO if_ehfnd_xlsx_sheet
        it_component  TYPE abap_component_tab
      EXPORTING
        et_col_info   TYPE ty_t_columninfo.

    METHODS get_sheet_content
      IMPORTING
        !iv_start_row TYPE i DEFAULT 2
        !icl_sheet    TYPE REF TO if_ehfnd_xlsx_sheet
      EXPORTING
        et_content    TYPE STANDARD TABLE.

ENDCLASS.



CLASS ZCL_TOOL_OFFICE_XLSX IMPLEMENTATION.


  METHOD get_column_name.
    DATA: ls_col_info TYPE ty_columninfo.
    "return if no sheet in xlsx file
    CHECK NOT icl_sheet IS INITIAL.
    "check file structure, first line of excel file
    DATA(lv_columncount) = icl_sheet->get_last_column_number_in_row( iv_title_row ). "获取第1行列数 （一共几列）
*    if lv_columncount is initial.
*      message e001(00) raising  file_open_error with 'E_未读取到标题行信息' .
*    endif.

    DATA lv_column TYPE i VALUE 1.
    DO lv_columncount TIMES.
      DATA(cellvalue) = icl_sheet->get_cell_content(
                                EXPORTING
                                  iv_row     = iv_title_row
                                  iv_column  = lv_column ).
      APPEND INITIAL LINE TO et_col_info ASSIGNING FIELD-SYMBOL(<fs_col_info>).
      <fs_col_info>-column = lv_column.
      <fs_col_info>-column_name = cellvalue.
      IF line_exists( it_component[ lv_column ] ).
        <fs_col_info>-column_field = it_component[ lv_column ]-name.
      ENDIF.
      lv_column = lv_column + 1.
    ENDDO.


  ENDMETHOD.


  METHOD get_components.

*get the components of structure()
    DATA ls_tab_ref    TYPE REF TO data.
    CREATE DATA ls_tab_ref LIKE LINE OF it_content.
    DATA lcl_tabstructure TYPE REF TO cl_abap_structdescr.
    lcl_tabstructure ?= cl_abap_typedescr=>describe_by_data_ref( ls_tab_ref ). "获取内表的 components
    et_comp_tab = lcl_tabstructure->get_components( ).
  ENDMETHOD.


  METHOD get_sheet_content.

    "按照顺序获取数据
    DATA lv_column TYPE i .
    DATA(lv_row_count) = icl_sheet->get_last_row_number( ).
    DATA(lv_current_row) = iv_start_row.
    DATA(lv_columncount) = icl_sheet->get_last_column_number_in_row( iv_start_row ). "获取一共几列
    WHILE lv_current_row <= lv_row_count.
      APPEND INITIAL LINE TO et_content ASSIGNING FIELD-SYMBOL(<fs_currentrow>) .
      DO lv_columncount TIMES.
        lv_column = lv_column + 1.
        DATA(lv_cellvalue) = icl_sheet->get_cell_content(
                             EXPORTING iv_row = lv_current_row
                                               iv_column  = lv_column ).
        ASSIGN COMPONENT lv_column OF STRUCTURE <fs_currentrow> TO FIELD-SYMBOL(<fs_cellvalue>).
        <fs_cellvalue> = lv_cellvalue.
      ENDDO.
      CLEAR lv_column.
      lv_current_row = lv_current_row + 1.
    ENDWHILE.


  ENDMETHOD.


  METHOD import_excel_mult_sheets.

    DATA: lt_comp_tab TYPE abap_component_tab.
    DATA: lcl_content TYPE REF TO data.
    FIELD-SYMBOLS: <fs_content> TYPE STANDARD TABLE.

*    create data lcl_content like  et_content.
*    assign lcl_content->* to <fs_content>.
* Get components of table et_content
    me->get_components(
      EXPORTING
        it_content  = et_content
      IMPORTING
        et_comp_tab = lt_comp_tab
    ).
* load file
    DATA: lcl_exception TYPE REF TO cx_static_check.
*create a xlsx handler
    DATA(lcl_xlsxhandler) = cl_ehfnd_xlsx=>get_instance( )."创建了句柄 *open xlsx into xstring

*load the xlsx
    TRY.
        DATA(lv_xstring_excel) = cl_openxml_helper=>load_local_file( iv_filepath ). "获取excel路径 为 xstring格式
        DATA(lcl_xlsxdocument) = lcl_xlsxhandler->load_doc( iv_file_data = lv_xstring_excel ). "加载excel
      CATCH cx_openxml_not_found INTO lcl_exception.
        rv_message = | 'E_'  { lcl_exception->get_text( )  }| .
        RETURN.
      CATCH cx_openxml_format INTO lcl_exception.
        rv_message = | 'E_'  { lcl_exception->get_text( )  }| .
        RETURN.
      CATCH cx_openxml_not_allowed INTO lcl_exception.
        rv_message = | 'E_'  { lcl_exception->get_text( )  }| .
        RETURN.
    ENDTRY.

*extract data from sheet ( pi_sheetname)
    TRY.
        DATA(lt_sheets) = lcl_xlsxdocument->get_sheets( ).
        LOOP AT lt_sheets ASSIGNING FIELD-SYMBOL(<fs_sheets>).
          APPEND INITIAL LINE TO t_sheet_info  ASSIGNING FIELD-SYMBOL(<fs_sheet_info>) .
          <fs_sheet_info>-name = <fs_sheets>-name.
          <fs_sheet_info>-sheet_id = <fs_sheets>-sheet_id.
          <fs_sheet_info>-comp_info = lt_comp_tab.
          CREATE DATA <fs_sheet_info>-content LIKE  et_content.
          ASSIGN <fs_sheet_info>-content->* TO <fs_content>.

          DATA(lcl_sheet)  = lcl_xlsxdocument->get_sheet_by_id( <fs_sheets>-sheet_id ).
          me->get_sheet_content(
            EXPORTING
              iv_start_row = iv_start_row
              icl_sheet    = lcl_sheet
              IMPORTING
                et_content   = <fs_content>
          ).
          me->get_column_name(
                    EXPORTING
                      iv_title_row = iv_title_row
                      icl_sheet    = lcl_sheet
                      it_component =  lt_comp_tab
                    IMPORTING
                      et_col_info  =  <fs_sheet_info>-col_info
                  ).
          APPEND LINES OF <fs_content> TO et_content.
        ENDLOOP.
        DELETE  t_sheet_info   WHERE col_info IS INITIAL .
      CATCH cx_openxml_format  INTO lcl_exception.
        rv_message = |E_Error occurs when extract data from specific sheet: {  lcl_exception->get_text( ) } | .
        RETURN.
      CATCH cx_openxml_not_found  INTO lcl_exception.
        rv_message = |E_Error occurs when extract data from specific sheet: {  lcl_exception->get_text( ) } | .
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD import_excel_single_sheet.

    FIELD-SYMBOLS: <fs_content> TYPE STANDARD TABLE.
    APPEND INITIAL LINE TO t_sheet_info ASSIGNING FIELD-SYMBOL(<fs_sheet_info>) .
* Get components of table et_content
    me->get_components(
      EXPORTING
        it_content  = et_content
      IMPORTING
        et_comp_tab = <fs_sheet_info>-comp_info
    ).

* load file
    DATA: lcl_exception TYPE REF TO cx_static_check.
*create a xlsx handler
    DATA(lcl_xlsxhandler) = cl_ehfnd_xlsx=>get_instance( )."创建了句柄 *open xlsx into xstring

*load the xlsx
    TRY.
        DATA(lv_xstring_excel) = cl_openxml_helper=>load_local_file( iv_filepath ). "获取excel路径 为 xstring格式
        DATA(lcl_xlsxdocument) = lcl_xlsxhandler->load_doc( iv_file_data = lv_xstring_excel ). "加载excel
      CATCH cx_openxml_not_found INTO lcl_exception.
        rv_message = | 'E_'  { lcl_exception->get_text( )  }| .
        RETURN.
      CATCH cx_openxml_format INTO lcl_exception.
        rv_message = | 'E_'  { lcl_exception->get_text( )  }| .
        RETURN.
      CATCH cx_openxml_not_allowed INTO lcl_exception.
        rv_message = | 'E_'  { lcl_exception->get_text( )  }| .
        RETURN.
    ENDTRY.

*extract data from sheet ( pi_sheetname)
    TRY.
        DATA(lt_sheets) = lcl_xlsxdocument->get_sheets( ).
        IF iv_sheetname IS NOT INITIAL.
          DATA(lcl_sheet) = lcl_xlsxdocument->get_sheet_by_name( iv_sheet_name = iv_sheetname ).
          <fs_sheet_info>-name = iv_sheetname.
          <fs_sheet_info>-sheet_id = lt_sheets[  name = iv_sheetname ]-sheet_id.
        ELSE.
          lcl_sheet = lcl_xlsxdocument->get_sheet_by_id( iv_sheet_id = 1 ).
          <fs_sheet_info>-name = lt_sheets[  sheet_id = 1 ]-name.
          <fs_sheet_info>-sheet_id = 1.
        ENDIF.

        CREATE DATA <fs_sheet_info>-content LIKE  et_content.
        ASSIGN <fs_sheet_info>-content->* TO <fs_content>.

        me->get_sheet_content(
                 EXPORTING
                   iv_start_row = iv_start_row
                   icl_sheet    = lcl_sheet
                   IMPORTING
                     et_content   = <fs_content>
               ).
        me->get_column_name(
                  EXPORTING
                    iv_title_row = iv_title_row
                    icl_sheet    = lcl_sheet
                    it_component =  <fs_sheet_info>-comp_info
                  IMPORTING
                    et_col_info  =  <fs_sheet_info>-col_info
                ).
        et_column_info = <fs_sheet_info>-col_info .
        APPEND LINES OF <fs_content> TO et_content.

      CATCH cx_openxml_format  INTO lcl_exception.
        rv_message = |E_Error occurs when extract data from specific sheet: {  lcl_exception->get_text( ) } | .
        RETURN.
      CATCH cx_openxml_not_found  INTO lcl_exception.
        rv_message = |E_Error occurs when extract data from specific sheet: {  lcl_exception->get_text( ) } | .
        RETURN.
    ENDTRY.

*    case check_structure.
*      when abap_on.
**        "按照字段对应获取数据
**        while currentrow <= rowcount.
**          append initial line to pt_tab assigning field-symbol(<currentrow>).
**          loop at columnfromfile reference into data(currentcolumn).
**            cellvalue = firstsheet->get_cell_content( exporting iv_row = currentrow
**                                     iv_column  = currentcolumn->*-column ).
**            assign component currentcolumn->*-columnname of structure <currentrow> to field-symbol(<cellvalue>).
**            <cellvalue> = cellvalue.
**          endloop.
**          currentrow = currentrow + 1.
**        endwhile.
*      when others.
*
*    endcase.
  ENDMETHOD.
ENDCLASS.
