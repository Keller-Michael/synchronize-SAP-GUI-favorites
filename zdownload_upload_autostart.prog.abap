*&---------------------------------------------------------------------*
*& Report ZDOWNLOAD_UPLOAD_AUTOSTART
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdownload_upload_autostart.

CONSTANTS gc_progname TYPE raldb_repo VALUE 'ZDOWNLOAD_UPLOAD_SETUP'.

DATA: gv_variant TYPE raldb_vari,
      gv_subrc   TYPE sysubrc.

IF sy-tcode = 'ZDOWN_UP_AUTOSTART'.
  gv_variant = sy-uname.

  " check if variant for user and report exists
  CALL FUNCTION 'RS_VARIANT_EXISTS'
    EXPORTING
      report              = gc_progname
      variant             = gv_variant
    IMPORTING
      r_c                 = gv_subrc
    EXCEPTIONS
      not_authorized      = 1
      no_report           = 2
      report_not_existent = 3
      report_not_supplied = 4
      OTHERS              = 5.

  IF gv_subrc = 0 and sy-subrc = 0.
    SUBMIT (gc_progname) USING SELECTION-SET gv_variant AND RETURN.
  ENDIF.
ENDIF.
