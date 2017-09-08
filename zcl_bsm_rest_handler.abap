class ZCL_BSM_REST_HANDLER definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  methods CONSTRUCTOR .
protected section.
private section.

  type-pools ABAP .
  data MV_CHECK_CSRF_TOKEN type ABAP_BOOL value ABAP_TRUE. "#EC NOTEXT

  methods HANDLE_GET
    importing
      !SERVER type ref to IF_HTTP_SERVER .
  methods HANDLE_POST
    importing
      !SERVER type ref to IF_HTTP_SERVER .
ENDCLASS.



CLASS ZCL_BSM_REST_HANDLER IMPLEMENTATION.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BSM_REST_HANDLER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CONSTRUCTOR.
        CONSTANTS: co_xsrf_check      TYPE string VALUE '~CHECK_CSRF_TOKEN'.

    DATA: lv_extension   TYPE string,
          lt_param       TYPE iac_parameter_tabtype_2010,
          lr_http_server TYPE REF TO cl_http_server,
          lo_http_server TYPE REF TO if_http_server.

    super->constructor( ).

* Get HTTP Server Entity
    CALL FUNCTION 'HTTP_GET_CURRENT_SERVER_CB'
      IMPORTING
        server_cb = lo_http_server
      EXCEPTIONS
        OTHERS    = 0.


    IF lo_http_server IS NOT BOUND. EXIT. ENDIF. "Unit Test!

* Get CSRF configuration
    lr_http_server ?= lo_http_server.
    CALL METHOD lr_http_server->get_service_extension
      EXPORTING
        kind          = ihttp_icfservice_extension_its
      IMPORTING
        configuration = lv_extension
      EXCEPTIONS
        OTHERS        = 0.

    IF lv_extension IS NOT INITIAL.
      CALL METHOD cl_icf_its_service=>params_string_to_table
        EXPORTING
          paramstr      = lv_extension
        IMPORTING
          paramtab_2010 = lt_param.
      READ TABLE lt_param
        WITH KEY name  = co_xsrf_check
                 value = '0'
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        mv_check_csrf_token     = abap_false.
      ENDIF.
    ENDIF.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BSM_REST_HANDLER->HANDLE_GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD HANDLE_GET.


    DATA: lv_json_in TYPE string.
    DATA: lv_json_out TYPE string.
    DATA: lo_response TYPE REF TO if_http_response.

    lv_json_in = server->request->get_uri_parameter('Json').

    IF strlen( lv_json_in ) > 0.
      lv_json_out = zcl_bsm_util=>json_call(  lv_json_in ).
    ENDIF.
    lo_response = server->response.

    lo_response->set_content_type( content_type = 'application/json' ).
    lo_response->set_cdata( lv_json_out ).
    lo_response->set_status( code = 200 reason = 'OK').

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BSM_REST_HANDLER->HANDLE_POST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method HANDLE_POST.

    DATA: lv_json_in TYPE string.
    DATA: lv_json_out TYPE string.
    DATA: lo_response TYPE REF TO if_http_response.

    lv_json_in = server->request->get_cdata( ).

    IF strlen( lv_json_in ) > 0.
      lv_json_out = zcl_bsm_util=>json_call(  lv_json_in ).
    ENDIF.

    lo_response = server->response.

    lo_response->set_content_type( content_type = 'application/json' ).
    lo_response->set_cdata( lv_json_out ).
    lo_response->set_status( code = 200 reason = 'OK').

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BSM_REST_HANDLER->IF_HTTP_EXTENSION~HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method IF_HTTP_EXTENSION~HANDLE_REQUEST.


 "" make this configurable. Check the implementation in constructor for CSRF token parameter
 "" it can be configured in SICF node using GUI configuration button
 "   server->set_session_stateful(
 "      EXPORTING
 "        stateful = if_http_server=>CO_ENABLED
 "        path     = '' )
 "       .



    if server->request->get_method( ) eq 'GET'.
      handle_get( server ).
      return.
    endif.

   if server->request->get_method( ) eq 'POST'.
      handle_post( server ).
      return.
    endif.

    server->response->set_status( exporting code = 400 reason =  'Not supported' ).


  endmethod.
ENDCLASS.