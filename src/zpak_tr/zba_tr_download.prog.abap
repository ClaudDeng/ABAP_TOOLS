*&---------------------------------------------------------------------*
*& Report ZBA_TR_DOWNLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZBA_TR_DOWNLOAD.

*======================================================================*
* Initial idea and first release by Igor Yaskevitch (IBS), 2003 *
* Enhancements by Sergey Korolev, 2005 (Added F4 value *
* requests, authority checks, TMS function usage) *
*----------------------------------------------------------------------*
* Function : This is a utility tool for downloading binary *
* files of transport request to a Client PC *
*======================================================================*

parameters:
  p_reqest      type trkorr obligatory,
  p_folder(255) type c lower case, p_sepr obligatory.
data:
  folder        type string,
  retval        like table of ddshretval with header line,
  fldvalue      like help_info-fldvalue,
  transdir      type text255,
  filename(255),
  trfile(20)    type c,
* datatab TYPE TABLE OF text8192 WITH HEADER LINE,
  begin of datatab occurs 0,
    text(8192) type x,
  end of datatab,
  len  type i,
  flen type i.
type-pools: sabc, stms, trwbo.

initialization.
  concatenate sy-sysid 'K*' into p_reqest.
  if sy-opsys = 'Windows NT'.
    p_sepr = '\'.
  else.
    p_sepr = '/'.
  endif.
* CALL FUNCTION 'WSAF_BUILD_SEPARATOR'
* IMPORTING
* separator = p_sepr
* EXCEPTIONS
* separator_not_maintained = 1
* wrong_call = 2
* wsaf_config_not_maintained = 3
* OTHERS = 4.
*
* IF sy-subrc NE 0.
* MESSAGE s001(00)
* WITH
* 'Unable to find out the separator symbol for the system.'(011).
* ENDIF.
at selection-screen on value-request for p_reqest.
  data:
    tt_system           type table of tmscsys with header line,
    es_selected_request type trwbo_request_header,
    es_selected_task    type trwbo_request_header,
    iv_organizer_type   type trwbo_calling_organizer,
    is_selection        type trwbo_selection.
  iv_organizer_type = 'W'. is_selection-reqstatus = 'R'.
  call function 'TR_PRESENT_REQUESTS_SEL_POPUP'
    exporting
      iv_organizer_type   = iv_organizer_type
      is_selection        = is_selection
    importing
      es_selected_request = es_selected_request
      es_selected_task    = es_selected_task.
  p_reqest = es_selected_request-trkorr.

at selection-screen on value-request for p_folder.
  data: title type string.
  title = 'Select target folder'(005).
  call method cl_gui_frontend_services=>directory_browse
    exporting
      window_title    = title
    changing
      selected_folder = folder
    exceptions
      cntl_error      = 1
      error_no_gui    = 2
      others          = 3.
  call function 'CONTROL_FLUSH'
    exceptions
      cntl_system_error = 1
      cntl_error        = 2
      others            = 3.
  p_folder = folder.

at selection-screen on p_reqest.
  data: request_info  type stms_wbo_request,
        request_infos type stms_wbo_requests.
  refresh request_infos.
  call function 'TMS_MGR_READ_TRANSPORT_REQUEST'
    exporting
      iv_request                 = p_reqest
      iv_header_only             = 'X'
    importing
      et_request_infos           = request_infos
    exceptions
      read_config_failed         = 1
      table_of_requests_is_empty = 2
      system_not_available       = 3
      others                     = 4.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  clear request_info.
  read table request_infos into request_info index 1.
  if sy-subrc ne 0
  or request_info-e070-trkorr is initial.
    message e398(00) with 'Request'(006) p_reqest 'not found'(007).
  elseif request_info-e070-trstatus ne 'R'.
    message e398(00)
    with 'You must release request'(008)
    request_info-e070-trkorr
    'before downloading'(009).
  endif.

start-of-selection.
  folder = p_folder.
  concatenate p_reqest+3(7) '.' p_reqest(3) into trfile.
  call function 'RSPO_R_SAPGPARAM'
    exporting
      name  = 'DIR_TRANS'
    importing
      value = transdir
    exceptions
      error = 0
      thers = 0.
  perform copy_file using 'cofiles' trfile.
  trfile(1) = 'R'.
  perform  copy_file using 'data' trfile.
  trfile(1) = 'D'.
  perform copy_file using 'data' trfile.
*---------------------------------------------------------------------*
* FORM. copy_file *
*---------------------------------------------------------------------*
* --> SUBDIR * * --> FNAME *
*---------------------------------------------------------------------*
form copy_file using subdir fname.
  data:
    auth_filename type authb-filename,
    gui_filename  type string.
  concatenate transdir subdir fname
  into filename
  separated by p_sepr.
  refresh datatab.
  clear flen.
  auth_filename = filename.
  call function 'AUTHORITY_CHECK_DATASET'
    exporting
      activity         = sabc_act_read
      filename         = auth_filename
    exceptions
      no_authority     = 1
      activity_unknown = 2
      others           = 3.
  if sy-subrc <> 0.
    format color col_negative.
    write: / 'Read access denied. File'(001),
    filename.
    format color off. exit.
  endif.
  open dataset filename for input in binary mode.
  if sy-subrc ne 0.
    format color col_total.
    write: / 'File open error'(010), filename.
    format color off. exit.
  endif.
  clear flen.
  data: mlen type i.
  mlen = 8192.
  do.
    clear len.
    read dataset filename into datatab maximum length mlen length len.
    flen = flen + len.
    if len > 0. append datatab. endif.
    if sy-subrc ne 0.
      exit.
    endif.
  enddo.
  close dataset filename.
  concatenate p_folder '\' fname into gui_filename.
  call method cl_gui_frontend_services=>gui_download
    exporting
      bin_filesize            = flen
      filename                = gui_filename
      filetype                = 'BIN'
    changing
      data_tab                = datatab[]
    exceptions
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      others                  = 24.
  if sy-subrc = 0.
    write: / 'File'(002), filename, 'downloaded. Length'(003), flen.
  else.
    format color col_negative.
    write: / 'File download error. Filename:'(004), filename.
    format color off.
  endif.
endform. "copy_file
