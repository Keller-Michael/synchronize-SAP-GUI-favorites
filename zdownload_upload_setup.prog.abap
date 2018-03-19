*&---------------------------------------------------------------------*
*& Report ZDOWNLOAD_UPLOAD_SETUP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdownload_upload_setup.

TABLES sscrfields.

DATA gc_version TYPE text30 VALUE '2018.1-01 (19.03.2018)'.

SELECTION-SCREEN: BEGIN OF BLOCK download WITH FRAME TITLE gv_tx000,
                  BEGIN OF LINE,
                  COMMENT 1(23) gv_tx001,
                  POSITION 25.
PARAMETERS:       pa_ddir TYPE string.
SELECTION-SCREEN: END OF LINE,
                  BEGIN OF LINE.
PARAMETERS:       pa_noup TYPE abap_bool AS CHECKBOX.
SELECTION-SCREEN: COMMENT 3(76) gv_tx010,
                  END OF LINE,
                  END OF BLOCK download.

SELECTION-SCREEN: BEGIN OF BLOCK upload WITH FRAME TITLE gv_tx008,
                  BEGIN OF LINE,
                  COMMENT 1(23) gv_tx002,
                  POSITION 25.
PARAMETERS:       pa_udir TYPE string.
SELECTION-SCREEN: END OF LINE,
                  END OF BLOCK upload.

SELECTION-SCREEN: BEGIN OF BLOCK options WITH FRAME TITLE gv_tx009,
                  BEGIN OF LINE.
PARAMETERS:       pa_msg TYPE abap_bool AS CHECKBOX.
SELECTION-SCREEN: COMMENT 3(76) gv_tx007,
                  END OF LINE,
                  END OF BLOCK options.

SELECTION-SCREEN: BEGIN OF BLOCK start_tcode WITH FRAME TITLE gv_tx003,
                  BEGIN OF LINE,
                  COMMENT 1(11) gv_tx004,
                  POSITION 12.
PARAMETERS:       pa_tcode TYPE tcode VISIBLE LENGTH 10.
SELECTION-SCREEN: COMMENT 26(10) gv_tx005,
                  POSITION 36.
PARAMETERS:       pa_rep TYPE raldb_repo VISIBLE LENGTH 10.
SELECTION-SCREEN: COMMENT 50(11) gv_tx006,
                  POSITION 62.
PARAMETERS:       pa_var TYPE raldb_vari.
SELECTION-SCREEN: END OF LINE,
                  END OF BLOCK start_tcode.

SELECTION-SCREEN: FUNCTION KEY 1.



CLASS lcx_error DEFINITION INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.

ENDCLASS.



CLASS lcl_gui_favorites DEFINITION.

  PUBLIC SECTION.
    TYPES: BEGIN OF file_content_line,
             rtype     TYPE reporttype,
             parent_id TYPE menu_num_5,
             object_id TYPE menu_num_5,
             tcode     TYPE extdreport,
             text      TYPE char100sm,
             sap_guid  TYPE hier_guid,
             url       TYPE agr_url,
           END OF file_content_line.

    TYPES: BEGIN OF file_list_line,
             file_name TYPE string,
             system_id TYPE sysid,
             client    TYPE mandt,
             date      TYPE sydatum,
             time      TYPE syuzeit,
           END OF file_list_line.

    TYPES: file_list    TYPE TABLE OF file_list_line,
           file_content TYPE TABLE OF file_content_line.

    DATA: download_done TYPE abap_bool READ-ONLY,
          upload_done   TYPE abap_bool READ-ONLY,
          favorites     TYPE TABLE OF smen_buffc READ-ONLY,
          gui_favorites TYPE TABLE OF favorites  READ-ONLY,
          links         TYPE TABLE OF smen_buffi READ-ONLY.

    METHODS constructor
      IMPORTING
        iv_download_dir  TYPE string OPTIONAL
        iv_upload_dir    TYPE string OPTIONAL
        iv_refuse_autoup TYPE abap_bool.

    METHODS download_and_upload
      RAISING
        lcx_error.

  PRIVATE SECTION.
    DATA: download_dir  TYPE string,
          upload_dir    TYPE string,
          refuse_autoup TYPE abap_bool.

    METHODS build_file_name_for_download
      EXPORTING
        ev_file_name TYPE string.

    METHODS download_to_directory
      RAISING
        lcx_error.

    METHODS get_file_list_for_upload
      EXPORTING
        et_file_list TYPE file_list
      RAISING
        lcx_error.

    METHODS upload_from_directory
      RAISING
        lcx_error.

ENDCLASS.



CLASS lcl_download_and_upload DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_download_dir  TYPE string
        iv_refuse_autoup TYPE abap_bool
        iv_upload_dir    TYPE string
        iv_tcode         TYPE tcode      OPTIONAL
        iv_report        TYPE raldb_repo OPTIONAL
        iv_variant       TYPE raldb_vari OPTIONAL
      RAISING
        lcx_error.

    METHODS send_greeting_to_programmer.

    CLASS-METHODS show_information.

    METHODS run
      RAISING
        lcx_error.

    METHODS start_report_or_tcode.

  PRIVATE SECTION.
    DATA: gui_favorites TYPE REF TO lcl_gui_favorites,
          download_dir  TYPE string,
          refuse_autoup TYPE abap_bool,
          upload_dir    TYPE string,
          tcode         TYPE tcode,
          report        TYPE raldb_repo,
          variant       TYPE raldb_vari.

    METHODS check_report_existence
      IMPORTING
        iv_report        TYPE raldb_repo
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS check_tcode_existence
      IMPORTING
        iv_tcode         TYPE tcode
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS check_variant_existence
      IMPORTING
        iv_report        TYPE raldb_repo
        iv_variant       TYPE raldb_vari
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS get_report_of_transaction
      IMPORTING
        iv_tcode         TYPE tcode
      RETURNING
        VALUE(rv_report) TYPE raldb_repo.

ENDCLASS.



CLASS lcx_error IMPLEMENTATION.

ENDCLASS.



CLASS lcl_download_and_upload IMPLEMENTATION.

  METHOD check_tcode_existence.

    DATA lv_tcode TYPE tcode.

    SELECT SINGLE tcode
           FROM tstc
           INTO lv_tcode
           WHERE tcode = iv_tcode.

    IF sy-subrc = 0.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_report_existence.

    DATA lv_report TYPE progname.

    " must be active and executable
    SELECT SINGLE progname
           FROM reposrc
           INTO lv_report
           WHERE progname = iv_report
           AND r3state = 'A'
           AND subc = '1'.

    IF sy-subrc = 0.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_variant_existence.

    DATA lv_rc TYPE sysubrc.

    CALL FUNCTION 'RS_VARIANT_EXISTS'
      EXPORTING
        report              = iv_report
        variant             = iv_variant
      IMPORTING
        r_c                 = lv_rc
      EXCEPTIONS
        not_authorized      = 1
        no_report           = 2
        report_not_existent = 3
        report_not_supplied = 4
        OTHERS              = 5.

    IF sy-subrc = 0 AND lv_rc = 0.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    DATA lv_result TYPE abap_bool.

    IF iv_tcode IS NOT INITIAL.
      lv_result = me->check_tcode_existence( EXPORTING iv_tcode = iv_tcode ).
      IF lv_result = abap_true.
        tcode = iv_tcode.
        report = me->get_report_of_transaction( EXPORTING iv_tcode = iv_tcode ).
      ELSE.
        RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Transaction does not exist.'.
      ENDIF.
    ELSEIF iv_report IS NOT INITIAL.
      lv_result = me->check_report_existence( EXPORTING iv_report = iv_report ).
      IF lv_result = abap_true.
        report = iv_report.
      ELSE.
        RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Report does not exist.'.
      ENDIF.
    ENDIF.

    IF iv_variant IS NOT INITIAL.
      lv_result = me->check_variant_existence( EXPORTING iv_report = report iv_variant = iv_variant ).
      IF lv_result = abap_true.
        variant = iv_variant.
      ELSE.
        RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Variant does not exist.'.
      ENDIF.
    ENDIF.

    IF iv_download_dir IS NOT INITIAL.
      CALL METHOD cl_gui_frontend_services=>directory_exist
        EXPORTING
          directory            = iv_download_dir
        RECEIVING
          result               = lv_result
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Error checking download directory.'.
      ELSE.
        IF lv_result = abap_true.
          download_dir = iv_download_dir.
        ELSE.
          RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Download directory does not exists.'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF iv_upload_dir IS NOT INITIAL.
      CALL METHOD cl_gui_frontend_services=>directory_exist
        EXPORTING
          directory            = iv_upload_dir
        RECEIVING
          result               = lv_result
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Error checking upload directory.'.
      ELSE.
        IF lv_result = abap_true.
          upload_dir = iv_upload_dir.
        ELSE.
          RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Upload directory does not exists.'.
        ENDIF.
      ENDIF.
    ENDIF.

    refuse_autoup = iv_refuse_autoup.

    CREATE OBJECT gui_favorites
      EXPORTING
        iv_download_dir  = download_dir
        iv_refuse_autoup = refuse_autoup
        iv_upload_dir    = upload_dir.

    " !!! raising in constructor

    IF gui_favorites IS NOT BOUND.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD get_report_of_transaction.

    DATA lv_report TYPE progname.

    SELECT SINGLE pgmna
           FROM tstc
           INTO lv_report
           WHERE tcode = iv_tcode.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE progname
           FROM reposrc
           INTO lv_report
           WHERE progname = lv_report
           AND r3state = 'A'
           AND subc = '1'.

    IF sy-subrc = 0.
      rv_report = lv_report.
    ENDIF.

  ENDMETHOD.


  METHOD run.

    gui_favorites->download_and_upload( ).

    IF gui_favorites->download_done = abap_true AND gui_favorites->upload_done = abap_true.
      MESSAGE 'SAP GUI favorites downloaded and uploaded.' TYPE 'S'.
    ELSEIF gui_favorites->download_done = abap_true AND gui_favorites->upload_done = abap_false.
      MESSAGE 'SAP GUI favorites downloaded.' TYPE 'S'.
    ELSEIF gui_favorites->download_done = abap_false AND gui_favorites->upload_done = abap_true.
      MESSAGE 'SAP GUI favorites uploaded.' TYPE 'S'.
    ENDIF.

    " Perhaps you want to download and upload your SE80 favorites? Or other important data?
    " Add your source code here ...

  ENDMETHOD.


  METHOD show_information.

    DATA lv_version TYPE text80.

    CONCATENATE 'Version' gc_version INTO lv_version SEPARATED BY space.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Version information'
        txt1  = 'Download and upload your SAP GUI favorites'
        txt2  = lv_version
        txt3  = 'written by Michael Keller'
        txt4  = 'use it, improve it, share it ...'.

  ENDMETHOD.


  METHOD start_report_or_tcode.

    IF report IS NOT INITIAL AND variant IS NOT INITIAL.
      SUBMIT (report) USING SELECTION-SET variant.
    ELSEIF report IS NOT INITIAL AND variant IS INITIAL.
      SUBMIT (report).
    ELSEIF tcode IS NOT INITIAL.
      CALL TRANSACTION tcode.
    ENDIF.

  ENDMETHOD.


  METHOD send_greeting_to_programmer.

    " obviously a hidden message ... and a good chance to say hello and thank you for studying
    " this source code. Please improve it and share it with SAP community. Kind regards

  ENDMETHOD.

ENDCLASS.



CLASS lcl_gui_favorites IMPLEMENTATION.

  METHOD build_file_name_for_download.

    DATA lv_file_name TYPE string.

    CONCATENATE download_dir  '\'
                sy-datum+0(4) '-'
                sy-datum+4(2) '-'
                sy-datum+6(2) '_'
                sy-uzeit+0(2) '-'
                sy-uzeit+2(2) '-'
                sy-uzeit+4(2) '_'
                sy-sysid      '_'
                sy-mandt      INTO lv_file_name.

    IF refuse_autoup = abap_true.
      " NOAUTOUP is our sign to deny automatic upload
      CONCATENATE lv_file_name '_NOAUTOUP_gui_favorites.txt' INTO lv_file_name.
    ELSE.
      CONCATENATE lv_file_name '_gui_favorites.txt' INTO lv_file_name.
    ENDIF.

    ev_file_name = lv_file_name.

  ENDMETHOD.


  METHOD constructor.

    download_dir  = iv_download_dir.
    upload_dir    = iv_upload_dir.
    refuse_autoup = iv_refuse_autoup.

    CALL FUNCTION 'NAVIGATION_LOAD_FAVORITES'
      EXPORTING
        user_name         = sy-uname
      TABLES
        favorites_tab     = favorites
        favorites_for_gui = gui_favorites
        links_list        = links.

  ENDMETHOD.


  METHOD download_and_upload.

    IF download_dir IS NOT INITIAL.
      me->download_to_directory( ).
    ENDIF.
    IF upload_dir IS NOT INITIAL.
      me->upload_from_directory( ).
    ENDIF.

  ENDMETHOD.


  METHOD download_to_directory.

    DATA: ls_links        TYPE smen_buffi,
          ls_favorites    TYPE smen_buffc,
          lt_file_content TYPE file_content,
          ls_file_content TYPE file_content_line,
          lv_file_name    TYPE string.

    " prepare SAP GUI favorites for download
    LOOP AT favorites INTO ls_favorites.
      CLEAR: ls_file_content,
             ls_links.

      ls_file_content-rtype     = ls_favorites-reporttype.
      ls_file_content-parent_id = ls_favorites-parent_id.
      ls_file_content-object_id = ls_favorites-object_id.
      ls_file_content-tcode     = ls_favorites-report.
      ls_file_content-sap_guid  = ls_favorites-sap_guid.
      ls_file_content-text      = ls_favorites-text.

      IF ls_favorites-reporttype EQ 'OT'.
        READ TABLE links WITH KEY object_id = ls_favorites-object_id INTO ls_links.
        IF sy-subrc = 0.
          ls_file_content-url = ls_links-url.
        ENDIF.
      ENDIF.

      APPEND ls_file_content TO lt_file_content.
    ENDLOOP.

    IF lt_file_content IS INITIAL.
      RETURN.
    ENDIF.

    me->build_file_name_for_download( IMPORTING ev_file_name = lv_file_name ).

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
*       bin_filesize            =
        filename                = lv_file_name
*       filetype                = 'ASC'
*       append                  = SPACE
*       write_field_separator   = SPACE
*       header                  = '00'
*       trunc_trailing_blanks   = SPACE
*       write_lf                = 'X'
*       col_select              = SPACE
*       col_select_mask         = SPACE
*       dat_mode                = SPACE
*       confirm_overwrite       = SPACE
*       no_auth_check           = SPACE
*       codepage                = SPACE
*       ignore_cerr             = ABAP_TRUE
*       replacement             = '#'
*       write_bom               = SPACE
*       trunc_trailing_blanks_eol = 'X'
*       wk1_n_format            = SPACE
*       wk1_n_size              = SPACE
*       wk1_t_format            = SPACE
*       wk1_t_size              = SPACE
        show_transfer_status    = space
*       fieldnames              =
*       write_lf_after_last_line  = 'X'
*       virus_scan_profile      = '/SCET/GUI_DOWNLOAD'
*     IMPORTING
*       filelength              =
      CHANGING
        data_tab                = lt_file_content
      EXCEPTIONS
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
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Error download to frontend.'.
    ENDIF.

    download_done = abap_true.

  ENDMETHOD.


  METHOD get_file_list_for_upload.

    DATA: lt_file_info TYPE TABLE OF file_info,
          ls_file_info TYPE file_info,
          lt_file_list TYPE file_list,
          ls_file_list TYPE file_list_line,
          lv_count     TYPE i,
          lt_parts     TYPE TABLE OF string,
          ls_parts     TYPE string.

    CALL METHOD cl_gui_frontend_services=>directory_list_files
      EXPORTING
        directory                   = upload_dir
        filter                      = '*.txt'
        files_only                  = abap_true
*       directories_only            =
      CHANGING
        file_table                  = lt_file_info
        count                       = lv_count
      EXCEPTIONS
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        not_supported_by_gui        = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_file_info INTO ls_file_info.
      CLEAR ls_file_list.

      SPLIT ls_file_info-filename AT '_' INTO TABLE lt_parts.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_parts TRANSPORTING NO FIELDS WITH KEY table_line = 'NOAUTOUP'.
      IF sy-subrc = 0.
        CONTINUE. " no automatic upload allowed
      ENDIF.

      CONCATENATE upload_dir '\' ls_file_info-filename INTO ls_file_list-file_name.

      LOOP AT lt_parts INTO ls_parts.
        CASE sy-tabix.
          WHEN '1'. " date
            REPLACE ALL OCCURRENCES OF '-' IN ls_parts WITH space.
            CONDENSE ls_parts NO-GAPS.
            WRITE ls_parts TO ls_file_list-date.

          WHEN '2'. " time
            REPLACE ALL OCCURRENCES OF '-' IN ls_parts WITH space.
            CONDENSE ls_parts NO-GAPS.
            WRITE ls_parts TO ls_file_list-time.

          WHEN '3'. " system id
            ls_file_list-system_id = ls_parts.

          WHEN '4'. " client
            ls_file_list-client = ls_parts.
        ENDCASE.
      ENDLOOP.

      APPEND ls_file_list TO lt_file_list.
    ENDLOOP.

    " only latest files
    SORT lt_file_list BY system_id client date DESCENDING time DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_file_list COMPARING system_id client.

    " not for this system id and client
    DELETE lt_file_list WHERE system_id = sy-sysid AND client = sy-mandt.

    SORT lt_file_list BY system_id client date.
    et_file_list = lt_file_list.

  ENDMETHOD.


  METHOD upload_from_directory.

    TYPES: BEGIN OF replaced_object_id_entry,
             old_object_id TYPE menu_num_5,
             new_object_id TYPE menu_num_5,
           END OF replaced_object_id_entry.

    DATA: lt_file_list     TYPE file_list,
          ls_file_list     TYPE file_list_line,
          lt_file_content  TYPE file_content,
          ls_file_content  TYPE file_content_line,
          ls_favorites     TYPE smen_buffc,
          lv_folder_name   TYPE ssm_title,
          lv_new_root      TYPE menu_num_5,
          lv_old_parent_id TYPE menu_num_5,
          lt_replaced_id   TYPE TABLE OF replaced_object_id_entry,
          ls_replaced_id   TYPE replaced_object_id_entry,
          lv_first_node    TYPE menu_flag,
          lv_object_id     TYPE menu_num_5,
          lv_target_id     TYPE menu_num_5,
          lv_new_id        TYPE menu_num_5,
          lv_text          TYPE ssm_title,
          lv_tcode         TYPE tcode.

    me->get_file_list_for_upload( IMPORTING et_file_list = lt_file_list ).
    IF lt_file_list IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_file_list INTO ls_file_list.
      CLEAR: lt_file_content,
             lt_replaced_id,
             ls_favorites.

      CALL METHOD cl_gui_frontend_services=>gui_upload
        EXPORTING
          filename                = ls_file_list-file_name
*         filetype                = 'ASC'
*         has_field_separator     = SPACE
*         header_length           = 0
*         read_by_line            = 'X'
*         dat_mode                = SPACE
*         codepage                = SPACE
*         ignore_cerr             = ABAP_TRUE
*         replacement             = '#'
*         virus_scan_profile      =
*       IMPORTING
*         filelength              =
*         header                  =
        CHANGING
          data_tab                = lt_file_content
*         isscanperformed         = SPACE
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          no_batch                = 3
          gui_refuse_filetransfer = 4
          invalid_type            = 5
          no_authority            = 6
          unknown_error           = 7
          bad_data_format         = 8
          header_not_allowed      = 9
          separator_not_allowed   = 10
          header_too_long         = 11
          unknown_dp_error        = 12
          access_denied           = 13
          dp_out_of_memory        = 14
          disk_full               = 15
          dp_timeout              = 16
          not_supported_by_gui    = 17
          error_no_gui            = 18
          OTHERS                  = 19.

      IF sy-subrc <> 0 OR lt_file_content IS INITIAL.
        RAISE EXCEPTION TYPE lcx_error MESSAGE e499(sy) WITH 'Error upload from frontend.'.
      ENDIF.

      CONCATENATE ls_file_list-system_id '-' ls_file_list-client INTO lv_folder_name.

      " Check if folder for system id and client is already there, then delete it.
      " Should be more easy than comparing entries.
      READ TABLE favorites INTO ls_favorites WITH KEY text = lv_folder_name.
      IF sy-subrc = 0.
        CALL FUNCTION 'FAVOS_EVENT_DELETE_FROM_SHELF'
          EXPORTING
            user_name  = sy-uname
            object_id  = ls_favorites-object_id
          TABLES
            user_shelf = favorites
            user_links = links.
      ENDIF.

      " add folder for system id and client, that's our new root
      CALL FUNCTION 'FAVOS_EVENT_ADD_TO_USER_SHELF'
        EXPORTING
          user_name  = sy-uname
          target_id  = '0001'
          reporttype = space
*         REPORT_NAME       =
*         SAP_GUID   =
          text       = lv_folder_name
          first_node = 'X'
*         URL        = ' '
*         BOOK_INFO  = ' '
*         X_POS      = ' '
*         Y_POS      = ' '
*         TARGET_SYST       = ' '
*         PERS_MINI  =
*         PERS_WIN   =
        IMPORTING
          new_id     = lv_new_root
        TABLES
          user_shelf = favorites
          user_links = links.

      " add SAP GUI favorites from the file under new root
      LOOP AT lt_file_content INTO ls_file_content.
        CLEAR: lv_first_node,
               lv_target_id,
               lv_new_id,
               ls_replaced_id,
               lv_text.

        " check if transaction code really exists in this system
        IF ls_file_content-rtype = 'TR'.
          SELECT SINGLE tcode
                 FROM tstc
                 INTO lv_tcode
                 WHERE tcode = ls_file_content-tcode.

          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
        ENDIF.

        IF ls_file_content-parent_id > lv_new_root AND ls_file_content-parent_id <> lv_old_parent_id.
          lv_first_node = 'X'.
        ENDIF.

        lv_old_parent_id = ls_file_content-parent_id.

        " get parent id
        READ TABLE lt_replaced_id INTO ls_replaced_id WITH KEY old_object_id = ls_file_content-parent_id.
        IF sy-subrc = 0.
          lv_target_id = ls_replaced_id-new_object_id.
        ELSE.
          lv_target_id = lv_new_root.
        ENDIF.

        lv_text = ls_file_content-text.

        CALL FUNCTION 'FAVOS_EVENT_ADD_TO_USER_SHELF'
          EXPORTING
            user_name   = sy-uname
            target_id   = lv_target_id
            reporttype  = ls_file_content-rtype
            report_name = ls_file_content-tcode
            sap_guid    = ls_file_content-sap_guid
            text        = lv_text
            first_node  = lv_first_node
            url         = ls_file_content-url
*           BOOK_INFO   = ' '
*           X_POS       = ' '
*           Y_POS       = ' '
*           TARGET_SYST = ' '
*           PERS_MINI   =
*           PERS_WIN    =
          IMPORTING
            NEW_ID      = lv_new_id
          TABLES
            user_shelf  = favorites
            user_links  = links.

        " remember what we switched
        ls_replaced_id-old_object_id = ls_file_content-object_id.
        ls_replaced_id-new_object_id = lv_new_id.
        APPEND ls_replaced_id TO lt_replaced_id.
      ENDLOOP. " file content
    ENDLOOP. " file list

    upload_done = abap_true.

  ENDMETHOD.

ENDCLASS.


INITIALIZATION.
  PERFORM initialize.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_ddir.
  PERFORM choose_directory USING 'DOWNLOAD'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_udir.
  PERFORM choose_directory USING 'UPLOAD'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_tcode.
  PERFORM choose_tcode_or_report USING 'TCODE'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_rep.
  PERFORM choose_tcode_or_report USING 'REPORT'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_var.
  PERFORM choose_variant.

AT SELECTION-SCREEN.
  PERFORM user_command.

START-OF-SELECTION.
  PERFORM run.


FORM run.

  DATA: lr_download_and_upload TYPE REF TO lcl_download_and_upload,
        lr_error               TYPE REF TO lcx_error,
        lv_message             TYPE string.

  TRY.
      CREATE OBJECT lr_download_and_upload
        EXPORTING
          iv_download_dir  = pa_ddir
          iv_refuse_autoup = pa_noup
          iv_upload_dir    = pa_udir
          iv_tcode         = pa_tcode
          iv_report        = pa_rep
          iv_variant       = pa_var.

      lr_download_and_upload->run( ).

    CATCH lcx_error INTO lr_error.
      IF pa_msg = abap_true.
        lv_message = lr_error->get_text( ).
        MESSAGE lv_message TYPE 'I'.
      ENDIF.
      RETURN.
  ENDTRY.

  IF pa_tcode IS NOT INITIAL OR pa_rep IS NOT INITIAL.
    lr_download_and_upload->start_report_or_tcode( ).
  ENDIF.

ENDFORM.


FORM initialize.

  DATA: lv_variant TYPE raldb_vari,
        ls_dyntxt  TYPE smp_dyntxt.

  sy-title = 'Download and upload your SAP GUI favorites'.

  gv_tx000 = 'Settings to download your SAP GUI favorites'.
  gv_tx001 = 'Download to directory'.
  gv_tx002 = 'Upload from directory'.
  gv_tx003 = 'Set transaction/report to start afterwards'.
  gv_tx004 = 'transaction'.
  gv_tx005 = 'or report'.
  gv_tx006 = 'and variant'.
  gv_tx007 = 'show message in case of error'.
  gv_tx008 = 'Settings to upload your SAP GUI favorites'.
  gv_tx009 = 'General settings'.
  gv_tx010 = 'refuse to upload the created file automatically in another system/client'.

  lv_variant = sy-uname.

  CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
    EXPORTING
      report               = sy-repid
      variant              = lv_variant
    EXCEPTIONS
      variant_not_existent = 1
      variant_obsolete     = 2
      OTHERS               = 3.

  IF sy-subrc <> 0. " no error handling due to point in time
  ENDIF.

  CLEAR ls_dyntxt.
  ls_dyntxt-icon_id   = icon_information.
  ls_dyntxt-quickinfo = 'Show documentation'.
  ls_dyntxt-path      = 'T'.
  sscrfields-functxt_01 = ls_dyntxt.

ENDFORM.


FORM choose_directory USING uv_kind TYPE char8.

  DATA: lv_title  TYPE string,
        lv_folder TYPE string.

  CALL METHOD cl_gui_frontend_services=>get_desktop_directory
    CHANGING
      desktop_directory    = lv_folder
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE 'Desktop directory could not be determined.' TYPE 'I'.
    RETURN.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    MESSAGE 'Desktop directory could not be determined.' TYPE 'I'.
    RETURN.
  ENDIF.

  IF uv_kind = 'DOWNLOAD'.
    lv_title = 'Download to directory'.
  ELSEIF uv_kind = 'UPLOAD'.
    lv_title = 'Upload from directory'.
  ENDIF.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = lv_title
      initial_folder       = lv_folder
    CHANGING
      selected_folder      = lv_folder
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE 'Error browsing directories.' TYPE 'I'.
    RETURN.
  ENDIF.

  IF lv_folder IS INITIAL.
    RETURN.
  ENDIF.

  IF uv_kind = 'DOWNLOAD'.
    pa_ddir = lv_folder.
  ELSEIF uv_kind = 'UPLOAD'.
    pa_udir = lv_folder.
  ENDIF.

ENDFORM.


FORM choose_tcode_or_report USING uv_kind TYPE char6.

  DATA: lv_dynpro_field TYPE dynfnam,
        lv_object       TYPE char4,
        lv_progname     TYPE progname.

  IF uv_kind = 'TCODE'.
    lv_dynpro_field = 'PA_TCODE'.
    lv_object = 'T'.
  ELSEIF uv_kind = 'REPORT'.
    lv_dynpro_field = 'PA_REP'.
    lv_object = 'PR'.
  ENDIF.

  lv_progname = sy-repid.

  CALL FUNCTION 'RS_HELP_HANDLING'
    EXPORTING
      dynpfield = lv_dynpro_field
      dynpname  = '1000'
      object    = lv_object
      progname  = lv_progname.

ENDFORM.


FORM choose_variant.

  DATA: lt_fields  TYPE TABLE OF dynpread,
        ls_fields  TYPE dynpread,
        lv_input   TYPE i,
        lv_report  TYPE raldb_repo,
        lv_message TYPE itex132,
        lv_variant TYPE raldb_vari.

  ls_fields-fieldname  = 'PA_TCODE'.
  APPEND ls_fields TO lt_fields.

  ls_fields-fieldname  = 'PA_REP'.
  APPEND ls_fields TO lt_fields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
*     TRANSLATE_TO_UPPER   = ' '
*     REQUEST              = ' '
*     PERFORM_CONVERSION_EXITS             = ' '
*     PERFORM_INPUT_CONVERSION             = ' '
*     DETERMINE_LOOP_INDEX = ' '
*     START_SEARCH_IN_CURRENT_SCREEN       = ' '
*     START_SEARCH_IN_MAIN_SCREEN          = ' '
*     START_SEARCH_IN_STACKED_SCREEN       = ' '
*     START_SEARCH_ON_SCR_STACKPOS         = ' '
*     SEARCH_OWN_SUBSCREENS_FIRST          = ' '
*     SEARCHPATH_OF_SUBSCREEN_AREAS        = ' '
    TABLES
      dynpfields           = lt_fields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  LOOP AT lt_fields INTO ls_fields WHERE fieldvalue IS NOT INITIAL.
    lv_input = lv_input + 1.
  ENDLOOP.

  IF lv_input = 0 OR lv_input = 2.
    MESSAGE 'Please choose between transaction or report.' TYPE 'I'.
    RETURN.
  ENDIF.

  READ TABLE lt_fields INTO ls_fields WITH KEY fieldname  = 'PA_TCODE'.
  IF sy-subrc = 0 AND ls_fields-fieldvalue IS NOT INITIAL.
    SELECT SINGLE pgmna
           FROM tstc
           INTO lv_report
           WHERE tcode = ls_fields-fieldvalue.

    IF sy-subrc <> 0.
      lv_message = 'No report for transaction &1 found.'.
      REPLACE '&1' IN lv_message WITH ls_fields-fieldvalue.
      MESSAGE lv_message TYPE 'I'.
      RETURN.
    ENDIF.
  ENDIF.

  IF lv_report IS INITIAL.
    READ TABLE lt_fields INTO ls_fields WITH KEY fieldname  = 'PA_REP'.
    IF sy-subrc = 0 AND ls_fields-fieldvalue IS NOT INITIAL.
      lv_report = ls_fields-fieldvalue.
    ENDIF.
  ENDIF.

  IF lv_report IS INITIAL.
    MESSAGE 'No combination to get variant catalog.' TYPE 'I'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report               = lv_report
*     NEW_TITLE            = ' '
*     DYNNR                =
*     INTERNAL_CALL        = ' '
*     MASKED               = 'X'
*     VARIANT              = ' '
*     POP_UP               = ' '
    IMPORTING
      sel_variant          = lv_variant
*     SEL_VARIANT_TEXT     =
*   TABLES
*     BELONGING_DYNNR      =
    EXCEPTIONS
      no_report            = 1
      report_not_existent  = 2
      report_not_supplied  = 3
      no_variants          = 4
      no_variant_selected  = 5
      variant_not_existent = 6
      OTHERS               = 7.

  CASE sy-subrc.
    WHEN 0.

    WHEN 2.
      lv_message = 'Report &1 not exists.'.

    WHEN 3.
      lv_message = 'Report &1 cannot be startet with a variant.'.

    WHEN 4.
      lv_message = 'There are no variants for report &1.'.

    WHEN 5.
      RETURN.

    WHEN OTHERS.
      lv_message = 'An unknown error occured.'.
  ENDCASE.

  IF lv_message IS NOT INITIAL.
    REPLACE '&1' IN lv_message WITH lv_report.
    MESSAGE lv_message TYPE 'I'.
    RETURN.
  ENDIF.

  CLEAR lt_fields.
  ls_fields-fieldname = 'PA_VAR'.
  ls_fields-fieldvalue = lv_variant.
  APPEND ls_fields TO lt_fields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_fields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  IF sy-subrc <> 0.
    MESSAGE 'An unknown error occured.' TYPE 'I'.
    RETURN.
  ENDIF.

ENDFORM.


FORM user_command.

  IF sy-dynnr <> '1000'.
    RETURN.
  ENDIF.

  IF sy-ucomm = 'FC01'.
    lcl_download_and_upload=>show_information( ).
  ENDIF.

ENDFORM.
