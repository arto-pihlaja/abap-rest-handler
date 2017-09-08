*----------------------------------------------------------------------*
*       CLASS ZCL_BSM_UTIL DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_BSM_UTIL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF number_t,
      number TYPE char12 ,
      rc type subrc,
      return type bapiret2_tab,
      END OF number_t .
  types:
    BEGIN OF number_req_t,
*      bukrs TYPE bukrs,
      code type char10,
      type TYPE nrsobj,
      END OF number_req_t .

  methods CONSTRUCTOR
    importing
      !I_JSON type STRING .
  class-methods TO_SEARCH_STRING
    changing
      !E_STRING type ANY .
  class-methods JSON_CALL
    importing
      !I_DATA type STRING
    returning
      value(E_DATA) type STRING .
  class-methods JSON_TO_DATA
    importing
      !I_JSON type STRING
    changing
      !E_DATA type ANY .
  class-methods DATA_TO_JSON
    importing
      !I_DATA type ANY
    changing
      value(E_JSON) type STRING .
  class-methods GET_BUKRS_ACTIVE
    importing
      !I_DATA type STRING
    returning
      value(R_DATA) type STRING .
  class-methods GET_NEXT_NUMBER
    importing
      !I_DATA type STRING
    returning
      value(R_DATA) type STRING .
  class-methods GET_NEXT_NUMBER_H
    importing
      !I_DATA type STRING
    returning
      value(R_DATA) type STRING .
protected section.
private section.

  data SUBOBJ type ref to OBJECT .
  data IMPORT_JSON type STRING .

  methods GET_NEXT_NUMBER_INST
    importing
      !I_NUMBER_REQ type NUMBER_REQ_T
    returning
      value(R_NUMBER) type NUMBER_T .
ENDCLASS.



CLASS ZCL_BSM_UTIL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BSM_UTIL->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_JSON                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD constructor.
  DATA: lo_sub TYPE REF TO lcl_sub.
  CREATE OBJECT lo_sub.
  me->subobj = lo_sub.
  me->import_json = i_json.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BSM_UTIL=>DATA_TO_JSON
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DATA                         TYPE        ANY
* | [<-->] E_JSON                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method DATA_TO_JSON.

    DATA: lo_writer TYPE REF TO cl_sxml_string_writer.
  DATA: lv_xjson TYPE xstring.

  lo_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
  CALL TRANSFORMATION id SOURCE data = i_data
                             RESULT XML lo_writer.

  lv_xjson = lo_writer->get_output( ).
  e_json = cl_abap_codepage=>convert_from( lv_xjson ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BSM_UTIL=>GET_BUKRS_ACTIVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DATA                         TYPE        STRING
* | [<-()] R_DATA                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_bukrs_active.
  TYPES: BEGIN OF bukrs_act_t,
           bukrs TYPE bukrs,
          butxt TYPE butxt,
          waers TYPE waers,
          langu_iso TYPE laiso,
          is_russian TYPE string,
    END OF bukrs_act_t.

  DATA: lt_bukrs_act TYPE STANDARD TABLE OF bukrs_act_t,
        lt_bukrs TYPE STANDARD TABLE OF zmdm_bukrs_act_v.
  FIELD-SYMBOLS: <bukrs> TYPE zmdm_bukrs_act_v,
                 <bukrsact> TYPE bukrs_act_t.
  SELECT * FROM zmdm_bukrs_act_v
    INTO TABLE lt_bukrs.

  LOOP AT lt_bukrs ASSIGNING <bukrs>.
    APPEND INITIAL LINE TO lt_bukrs_act ASSIGNING <bukrsact>.
    <bukrsact>-bukrs = <bukrs>-bukrs.
    <bukrsact>-butxt = <bukrs>-butxt.
    <bukrsact>-waers = <bukrs>-waers.

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
      EXPORTING
        input  = <bukrs>-spras
      IMPORTING
        output = <bukrsact>-langu_iso.

    IF <bukrs>-land1 = 'RU'.
      <bukrsact>-is_russian = 'true'.
    ELSE.
      <bukrsact>-is_russian = 'false'.
    ENDIF.
  ENDLOOP.

  data_to_json(
    EXPORTING
      i_data = lt_bukrs_act
    CHANGING
      e_json = r_data ).
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BSM_UTIL=>GET_NEXT_NUMBER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DATA                         TYPE        STRING
* | [<-()] R_DATA                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_next_number.
  DATA: this TYPE REF TO zcl_bsm_util,
        sub TYPE REF TO lif_sub,
        number_req TYPE number_req_t,
        number TYPE number_t.

  CREATE OBJECT this EXPORTING i_json = i_data.
  sub ?= this->subobj.
*  Extract company code and object type (PC, CC, IO)
  json_to_data(
    EXPORTING
      i_json = i_data
    CHANGING
        e_data = number_req ).

  number = this->get_next_number_inst( number_req ).

*  Put number to json
  data_to_json(
    EXPORTING
      i_data = number
    CHANGING
      e_json = r_data ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BSM_UTIL=>GET_NEXT_NUMBER_H
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DATA                         TYPE        STRING
* | [<-()] R_DATA                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_NEXT_NUMBER_H.
  DATA: this TYPE REF TO zcl_bsm_util,
        sub TYPE REF TO lif_sub,
        number_req TYPE number_req_t,
        number TYPE number_t.

  CREATE OBJECT this EXPORTING i_json = i_data.
  sub ?= this->subobj.
*  Extract company code and object type (PC, CC, IO)
  json_to_data(
    EXPORTING
      i_json = i_data
    CHANGING
        e_data = number_req ).

* Test code
  number-number = '00000001'.
*  number = this->get_next_number_inst( number_req ).

*  Put number to json
  data_to_json(
    EXPORTING
      i_data = number
    CHANGING
      e_json = r_data ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BSM_UTIL->GET_NEXT_NUMBER_INST
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NUMBER_REQ                   TYPE        NUMBER_REQ_T
* | [<-()] R_NUMBER                       TYPE        NUMBER_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_next_number_inst.
  DATA: sub TYPE REF TO lif_sub,
        found_nr type boole_d,
        sobj type NRSOBJ.

  FIELD-SYMBOLS: <ret> TYPE bapiret2.
  sub ?= me->subobj.
  sobj = i_number_req-type.
  found_nr = abap_false.
  WHILE found_nr = abap_false.
    r_number = sub->get_nr( i_number_req ).
    IF r_number-rc <> 0.
      EXIT.
    ENDIF.
    IF sub->nr_free( i_number = r_number-number i_subobj = sobj ) = abap_true.
      found_nr = abap_true.
    ENDIF.
  ENDWHILE.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BSM_UTIL=>JSON_CALL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DATA                         TYPE        STRING
* | [<-()] E_DATA                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD JSON_CALL.

    TYPE-POOLS abap.

    DATA: lv_fname TYPE eu_lname.
    DATA: ls_detail TYPE rsfbintfv.
    FIELD-SYMBOLS: <f_para> TYPE rsfbpara.
    DATA: lt_comp TYPE cl_abap_structdescr=>component_table.
    FIELD-SYMBOLS: <f_comp> LIKE LINE OF lt_comp.
    DATA: lo_struct_type TYPE REF TO cl_abap_structdescr.
    DATA: lo_table_struct_type TYPE REF TO cl_abap_structdescr.
    DATA: lo_any_type TYPE REF TO cl_abap_typedescr.
    FIELD-SYMBOLS: <f_params> TYPE any.
    DATA: lo_dref TYPE REF TO data.
    DATA: lt_fm TYPE abap_func_parmbind_tab .
    DATA: ls_fm TYPE LINE OF abap_func_parmbind_tab.
    FIELD-SYMBOLS: <f_value> TYPE any.
    DATA: lv_json_in TYPE string.
    DATA: lv_json_out TYPE string.
    DATA: BEGIN OF ls_call,
            bsm_name      TYPE string,
            bsm_interface TYPE string,
            bsm_language  TYPE string,
          END OF ls_call.
    DATA: lv_langu TYPE t002-spras.
    DATA: lv_class TYPE string.
    DATA: lv_method TYPE string.
    DATA: lt_parmbind TYPE abap_parmbind_tab.
    DATA: ls_parmbind  TYPE abap_parmbind.
    DATA: lo_object TYPE REF TO object.
    CONSTANTS: true TYPE abap_bool VALUE 'X'.
    CONSTANTS: false TYPE abap_bool VALUE ''.
    DATA: lo_object_type TYPE REF TO cl_abap_objectdescr.
    DATA: lt_method TYPE abap_methdescr_tab.
    FIELD-SYMBOLS: <f_method> TYPE abap_methdescr.
    FIELD-SYMBOLS: <f_parameter> TYPE abap_parmdescr.
    DATA: lo_param_type TYPE REF TO cl_abap_datadescr.





    lv_json_in = i_data.

    "call parameters
    zcl_bsm_util=>json_to_data(
            EXPORTING
               i_json = lv_json_in
            CHANGING
               e_data = ls_call )
               .
    IF ls_call-bsm_language NE space.
      SELECT SINGLE spras INTO lv_langu
        FROM t002 WHERE laiso EQ ls_call-bsm_language.
      IF sy-subrc EQ 0.
        SET LOCALE LANGUAGE lv_langu.
        "SCP_MIXED_LANGUAGES_ ?
      ENDIF.
    ENDIF.

    IF ls_call-bsm_interface IS INITIAL.
      ls_call-bsm_interface = 'FUNCTION'.
    ENDIF.

    IF ls_call-bsm_interface EQ 'CLASS' or
       ls_call-bsm_interface EQ 'CLASS_JSON'.
      SPLIT ls_call-bsm_name AT ',' INTO lv_class lv_method.

      IF   ls_call-bsm_interface EQ 'CLASS_JSON' or ls_call-bsm_name CS '_JSON'.

        CALL METHOD (lv_class)=>(lv_method)
          EXPORTING
            i_data = i_data
          RECEIVING
            r_data = e_data.

      ELSE.

        lo_object_type ?= cl_abap_objectdescr=>describe_by_name( lv_class ).

        lt_method = lo_object_type->methods.
        READ TABLE lt_method WITH KEY name = lv_method ASSIGNING <f_method>.
        IF sy-subrc EQ 0.
          "Create structure for all parameters
          CLEAR: lt_comp[].
          LOOP AT <f_method>-parameters ASSIGNING <f_parameter>.
            APPEND INITIAL LINE TO lt_comp ASSIGNING <f_comp>.

            lo_param_type = lo_object_type->get_method_parameter_type(
                    p_method_name = <f_method>-name
                    p_parameter_name = <f_parameter>-name ).


            <f_comp>-name = <f_parameter>-name.
            <f_comp>-type ?= lo_param_type.

          ENDLOOP.
          DESCRIBE TABLE lt_comp.
          IF sy-tfill > 0.
            lo_struct_type =  cl_abap_structdescr=>create( lt_comp ).
            CREATE DATA lo_dref TYPE HANDLE lo_struct_type.
            ASSIGN lo_dref->* TO <f_params>.
          ENDIF.
          "get parameters from json
          IF <f_params> IS ASSIGNED.

            zcl_bsm_util=>json_to_data(
                   EXPORTING
                      i_json = lv_json_in
                   CHANGING
                      e_data = <f_params> ).

          ENDIF.
          "call method
          LOOP AT <f_method>-parameters ASSIGNING <f_parameter>.
            ASSIGN COMPONENT <f_parameter>-name OF STRUCTURE <f_params> TO <f_value>.
            IF <f_value> IS NOT INITIAL OR
               <f_parameter>-is_optional EQ false OR
               <f_parameter>-parm_kind EQ cl_abap_objectdescr=>returning OR
              <f_parameter>-parm_kind EQ cl_abap_objectdescr=>changing  OR
               <f_parameter>-parm_kind EQ cl_abap_objectdescr=>exporting.
              .
              CLEAR: ls_parmbind.
              ls_parmbind-name = <f_parameter>-name.
              IF <f_parameter>-parm_kind EQ cl_abap_objectdescr=>exporting.
                ls_parmbind-kind = cl_abap_objectdescr=>importing.
              ELSEIF <f_parameter>-parm_kind EQ cl_abap_objectdescr=>importing.
                 ls_parmbind-kind = cl_abap_objectdescr=>exporting.
              ELSE.
                ls_parmbind-kind = <f_parameter>-parm_kind.
              ENDIF.
              GET REFERENCE OF <f_value> INTO ls_parmbind-value.
              INSERT ls_parmbind INTO TABLE lt_parmbind.
            ENDIF.
          ENDLOOP.

          IF <f_method>-is_class EQ true.
            CALL METHOD (lv_class)=>(lv_method)
              PARAMETER-TABLE
              lt_parmbind.

          ELSE.
            CREATE OBJECT lo_object TYPE (lv_class).
            CALL METHOD lo_object->(lv_method)
              PARAMETER-TABLE
              lt_parmbind.
          ENDIF.
          "data to json
          IF <f_params> IS ASSIGNED.
            zcl_bsm_util=>data_to_json(
                  EXPORTING
                     i_data = <f_params>
                  CHANGING
                     e_json = lv_json_out ).
          ENDIF.
          e_data = lv_json_out.


        ENDIF.

      ENDIF.
    ENDIF.

    IF ls_call-bsm_interface EQ 'FUNCTION' or
      ls_call-bsm_interface EQ 'FUNCTION_JSON'.

      IF ls_call-bsm_interface EQ 'FUNCTION_JSON' or ls_call-bsm_name CS '_JSON'.
        CALL FUNCTION ls_call-bsm_name
          EXPORTING
            i_data = i_data
          IMPORTING
            e_data = e_data.
      ELSE.
        lv_fname = ls_call-bsm_name.
        cl_fb_function_utility=>meth_get_interface(
          EXPORTING
            im_name             = lv_fname
*           im_mode             = 'DISPLAY'
*           im_active           = 'A'
*           im_read             = 'J'
          IMPORTING
            ex_interface        = ls_detail
          EXCEPTIONS
            error_occured       = 1
            object_not_existing = 2
            OTHERS              = 3 ).

        "Create structure for all parameters
        CLEAR: lt_comp[].
        LOOP AT ls_detail-import ASSIGNING <f_para>.
          APPEND INITIAL LINE TO lt_comp ASSIGNING <f_comp>.
          <f_comp>-name = <f_para>-parameter.
          <f_comp>-type ?= cl_abap_elemdescr=>describe_by_name( <f_para>-structure ).
        ENDLOOP.
        LOOP AT ls_detail-tables ASSIGNING <f_para>.
          APPEND INITIAL LINE TO lt_comp ASSIGNING <f_comp>.
          <f_comp>-name = <f_para>-parameter.
          IF <f_para>-typefield EQ 'TYPE'.
            "table type
            <f_comp>-type ?= cl_abap_elemdescr=>describe_by_name( <f_para>-structure ).
          ELSE.
            "like structure
            lo_table_struct_type ?= cl_abap_elemdescr=>describe_by_name( <f_para>-structure ).
            <f_comp>-type ?= cl_abap_tabledescr=>create( lo_table_struct_type ).
          ENDIF.
        ENDLOOP.
        LOOP AT ls_detail-export ASSIGNING <f_para>.
          APPEND INITIAL LINE TO lt_comp ASSIGNING <f_comp>.
          <f_comp>-name = <f_para>-parameter.
          <f_comp>-type ?= cl_abap_elemdescr=>describe_by_name( <f_para>-structure ).
        ENDLOOP.

        DESCRIBE TABLE lt_comp.
        IF sy-tfill > 0.
          lo_struct_type =  cl_abap_structdescr=>create( lt_comp ).
          CREATE DATA lo_dref TYPE HANDLE lo_struct_type.
          ASSIGN lo_dref->* TO <f_params>.
        ENDIF.

        "get parameters from json
        IF <f_params> IS ASSIGNED.

          zcl_bsm_util=>json_to_data(
                 EXPORTING
                    i_json = lv_json_in
                 CHANGING
                    e_data = <f_params> )
                    .
        ENDIF.

        "call function module
        "set import parameters
        LOOP AT ls_detail-import ASSIGNING <f_para>.
          ASSIGN COMPONENT <f_para>-parameter OF STRUCTURE <f_params> TO <f_value>.
          CLEAR: ls_fm.
          IF <f_value> IS NOT INITIAL OR  <f_para>-optional EQ space.
            ls_fm-name = <f_para>-parameter.
            GET REFERENCE OF <f_value> INTO ls_fm-value.
            ls_fm-kind = abap_func_exporting.
            INSERT ls_fm INTO TABLE lt_fm.
          ENDIF.
        ENDLOOP.
        "set export parameters
        LOOP AT ls_detail-export ASSIGNING <f_para>.
          ASSIGN COMPONENT <f_para>-parameter OF STRUCTURE <f_params> TO <f_value>.
          CLEAR: ls_fm.
          ls_fm-name = <f_para>-parameter.
          GET REFERENCE OF <f_value> INTO ls_fm-value.
          ls_fm-kind = abap_func_importing.
          INSERT ls_fm INTO TABLE lt_fm.
        ENDLOOP.
        "set tables
        LOOP AT ls_detail-tables ASSIGNING <f_para>.
          ASSIGN COMPONENT <f_para>-parameter OF STRUCTURE <f_params> TO <f_value>.
          CLEAR: ls_fm.
          ls_fm-name = <f_para>-parameter.
          GET REFERENCE OF  <f_value> INTO ls_fm-value.
          ls_fm-kind = abap_func_tables.
          INSERT ls_fm INTO TABLE lt_fm.
        ENDLOOP.

        CALL FUNCTION ls_call-bsm_name
          PARAMETER-TABLE lt_fm.

        IF <f_params> IS ASSIGNED.
          zcl_bsm_util=>data_to_json(
                EXPORTING
                   i_data = <f_params>
                CHANGING
                   e_json = lv_json_out ).
        ENDIF.
        e_data = lv_json_out.
      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BSM_UTIL=>JSON_TO_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_JSON                         TYPE        STRING
* | [<-->] E_DATA                         TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method JSON_TO_DATA.

  DATA: lv_xjson TYPE xstring.
  DATA: lo_reader TYPE REF TO cl_sxml_string_reader.


  lv_xjson = cl_abap_codepage=>convert_to( i_json ).
  lo_reader ?= cl_sxml_string_reader=>create( lv_xjson ).


  CALL TRANSFORMATION id SOURCE XML lo_reader
                              RESULT data = e_data.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BSM_UTIL=>TO_SEARCH_STRING
* +-------------------------------------------------------------------------------------------------+
* | [<-->] E_STRING                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method TO_SEARCH_STRING.
  REPLACE ALL OCCURRENCES OF '!' IN e_string WITH '!!'.
  REPLACE ALL OCCURRENCES OF '%' IN e_string WITH '!%'.
  REPLACE ALL OCCURRENCES OF '_' IN e_string WITH '!_'.
  REPLACE ALL OCCURRENCES OF '*' IN e_string WITH '%'.

  IF e_string EQ space.
    e_string = '%'.
  ENDIF.
  endmethod.
ENDCLASS.