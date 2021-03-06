
*----------------------------------------------------------------------*
*       CLASS lcl_mock DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_mock DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_sub.

    DATA: count TYPE i.
ENDCLASS.                    "lcl_mock DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_mock IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_mock IMPLEMENTATION.
  METHOD lif_sub~get_nr.
    IF i_use_h = abap_false. "get with BUKRS
* First test that params are passed OK
      IF i_num_req-code <> '9000' OR i_num_req-type <> 'PC'.
        r_num-rc = 8.
      ENDIF.
      me->count = me->count + 1.
      CASE me->count.
        WHEN 1.
          r_num-number = '0000000001'.
          r_num-rc = 0.
        WHEN 2.
          r_num-number = '0000000002'.
          r_num-rc = 0.
        WHEN 3.
          r_num-number = '0000000003'.
        WHEN OTHERS.
          r_num-rc = 2.
      ENDCASE.
    ELSE.
* Test getting with hierarchy
* First test that test params are passed OK
      IF i_num_req-code <> '123' OR i_num_req-type <> 'PC'.
        r_num-rc = 8.
      ENDIF.
      me->count = me->count + 1.
      CASE me->count.
        WHEN 1.
          r_num-number = '999'.
          r_num-rc = 0.
        WHEN 2.
*    This should be skipped, as it's 'not free'
          r_num-number = '998'.
          r_num-rc = 0.
        WHEN 3.
          r_num-number = '997'.
          r_num-rc = 0.
        WHEN OTHERS.
          r_num-rc = 2.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "lif_sub~get_nr
  METHOD lif_sub~nr_free.
    IF i_number = '0000000001' or i_number = '998'.
      r_free = abap_false.
    ELSE.
      r_free = abap_true.
    ENDIF.
  ENDMETHOD.                    "lif_sub~nr_free
  METHOD lif_sub~get_pchier.
    DEFINE fill_hierset.
*   GROUPNAME       HIERLEVEL     VALCOUNT  DESCR
*    A9000                   0          0  comp Oy
      APPEND INITIAL LINE TO r_hier ASSIGNING &1.
      &1-groupname = &2.
      &1-hierlevel = &3.
      &1-valcount = &4.
    END-OF-DEFINITION.
    FIELD-SYMBOLS: <hier> TYPE bapiset_hier. "zcl_bsm_util=>hiernode_tt.
* Let's build a table with all the parent levels from request node up to the root
* Example structure (lt_hiernodes_all) from the BAPI:
*   GROUPNAME       HIERLEVEL     VALCOUNT  DESCR
*    A9000                   0          0  comp Oy
*    C4000                   1          0  Comp Services BG
*    E4000                   2          0  PS Mail Srvcs BU
*    K400000                 3          0  PS Mail International Co-op
*    Y4000000                4          1  PS Mail International Co-op
*    K400020                 3          0  PS Mail Consumers
*    Y4000020                4          3  PS Mail Consumers
*    K400030                 3          0  PS Mail Corporations
*    P400030                 4          0  PS Mail Corp. Letter Int. srvcs
*    Y4000030                5          1  PS Mail Corp Letter Int srvcs
*    P400035                 4          0  PS Mail Corp. Letter Dom. srvcs
*    Y4000035                5          3  PS Mail Corp Letter Dom srvcs


    fill_hierset <hier> 'A9000' '0' '0'.
    fill_hierset <hier> 'C4000' '1' '0'.
    fill_hierset <hier> 'E4000' '2' '0'.
    fill_hierset <hier> 'K400000' '3' '0'.
    fill_hierset <hier> 'Y4000000' '4' '1'.
    fill_hierset <hier> 'K400020' '3' '0'.
    fill_hierset <hier> 'Y4000020' '4' '3'.
    fill_hierset <hier> 'K400030' '3' '0'.
    fill_hierset <hier> 'P400030' '4' '0'.
    fill_hierset <hier> 'Y4000030' '5' '1'.
    fill_hierset <hier> 'P400035' '4' '0'.
    fill_hierset <hier> 'Y4000035' '5' '3'.
  ENDMETHOD.                    "lif_sub~get_pchier
  METHOD lif_sub~get_cchier.
    r_hier = me->lif_sub~get_pchier( ).
  ENDMETHOD.                    "lif_sub~get_cchier
  METHOD lif_sub~number_get_next.
* Called   sub->number_get_next( i_nrnr = <hier_nr>-nrrangenr i_subobj = i_number_req-type ).
    DATA: num_req TYPE zcl_bsm_util=>number_req_t.
* First check correct parameters passed
    ASSERT i_nrnr = '01'.
    num_req-type = i_subobj.

*    Then reuse previously created mock method
    num_req-code = '123'.
    r_nr = me->lif_sub~get_nr( i_num_req = num_req i_use_h = abap_true ).
  ENDMETHOD.                    "lif_sub~number_get_next
  METHOD lif_sub~get_hier_nr.
    FIELD-SYMBOLS: <hn> type zmdm_pchier_nr.
    APPEND INITIAL LINE TO r_hier_nr ASSIGNING <hn>.
    <hn>-grpname = 'E4000'.
    <hn>-nrrangenr = '01'.
*    CASE i_subobj.
**      WHEN 'PC'.
**      WHEN 'CC'.
*    ENDCASE.
  ENDMETHOD.                    "lif_sub~get_hier_nr
ENDCLASS.                    "lcl_mock IMPLEMENTATION


CLASS ltc_util DEFINITION DEFERRED.
CLASS zcl_bsm_util DEFINITION LOCAL FRIENDS ltc_util.

*----------------------------------------------------------------------*
*       CLASS ltc_Util DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_util DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Util
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_BSM_UTIL
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
* ================
    DATA:
      f_cut TYPE REF TO zcl_bsm_util.  "class under test
    DATA: mock TYPE REF TO lcl_mock.

    METHODS: setup.
    METHODS: teardown.
    METHODS: get_next_number FOR TESTING.
    METHODS: get_next_number_h FOR TESTING.
    METHODS: get_next_number_inst FOR TESTING.

    METHODS: get_next_number_h_inst FOR TESTING.
ENDCLASS.       "ltc_Util


*----------------------------------------------------------------------*
*       CLASS ltc_Util IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_util IMPLEMENTATION.
* ==============================

  METHOD setup.
* =============
    DATA: json TYPE string.
    CREATE OBJECT f_cut
      EXPORTING
        i_json = json.
    CREATE OBJECT mock.
    f_cut->subobj = mock.
  ENDMETHOD.       "setup


  METHOD teardown.
* ================

  ENDMETHOD.       "teardown

  METHOD get_next_number.
*    Testing that the method at least won't terminate
    DATA: i_data TYPE string,
          r_data TYPE string,
          nreq TYPE zcl_bsm_util=>number_req_t.

*    i_data = '{DATA:{"TYPE":"PC", "CODE":"9000"}}'.
    i_data = '{"DATA":{"BSM_NAME":"ZCL_BSM_UTIL,GET_NEXT_NUMBER","BSM_INTERFACE":"CLASS_JSON", "CODE":"9000","TYPE":"PC"}}'.
    r_data = f_cut->get_next_number( i_data ).
*    {"DATA":{"NUMBER":"0059300112","RC":"00","RETURN":[]}}
    cl_abap_unit_assert=>assert_char_cp(
      act = r_data
      exp = '*NUMBER":"0059*'
      msg = 'Failed to get number (assuming certain number range setup in this environment)'
    ).
  ENDMETHOD.                    "get_next_number

  METHOD get_next_number_h.
*    Testing that the method at least won't terminate
    DATA: i_data TYPE string,
          r_data TYPE string,
          nreq TYPE zcl_bsm_util=>number_req_t.

*    i_data = '{DATA:{"TYPE":"PC", "CODE":"9000"}}'.
    i_data = '{"DATA":{"BSM_NAME":"ZCL_BSM_UTIL,GET_NEXT_NUMBER_H","BSM_INTERFACE":"CLASS_JSON", "CODE":"K400000","TYPE":"PC"}}'.
    r_data = f_cut->get_next_number_h( i_data ).
*    {"DATA":{"NUMBER":"0059300112","RC":"00","RETURN":[]}}
    cl_abap_unit_assert=>assert_char_cp(
      act = r_data
      exp = '*NUMBER":"0059*'
      msg = 'Failed to get number (Test assumes A9000 or a suitable child of it is assigned to nr 02 in ZMDM_CCHIER_NR!)'
    ).
  ENDMETHOD.                    "get_next_number

  METHOD get_next_number_inst.
* ============================
    DATA i_number_req TYPE zcl_bsm_util=>number_req_t.
    DATA r_number TYPE zcl_bsm_util=>number_t.
    i_number_req-code = '9000'.
    i_number_req-type = 'PC'.
* In our test setup, the first number is not free, but the second is. We should get the second.
    r_number = f_cut->get_next_number_inst( i_number_req ).

    cl_abap_unit_assert=>assert_equals(
      act   = r_number-number
      exp   = '0000000002'
     msg   = 'Test getting free number'
*     level =
    ).
* In our test setup, we simulate NUMBER_GET_NEXT fails on the third attempt
    r_number = f_cut->get_next_number_inst( i_number_req ).
    r_number = f_cut->get_next_number_inst( i_number_req ).
    cl_abap_unit_assert=>assert_equals(
      act   = r_number-rc
      exp   = 2
     msg   = 'Test handling SNUM errors'
*     level =
    ).
  ENDMETHOD.       "get_Next_Number_Inst

  METHOD get_next_number_h_inst.
* ==============================
    DATA i_number_req TYPE zcl_bsm_util=>number_req_t.
    DATA r_number TYPE zcl_bsm_util=>number_t.
    i_number_req-type = 'PC'.
    i_number_req-code = 'Y4000035'.
    r_number = f_cut->get_next_number_h_inst( i_number_req ).

    cl_abap_unit_assert=>assert_equals(
      act   = r_number-number
      exp   = '999'
     msg   = 'Test getting number with PC hierarchy'
*     level =
    ).

    r_number = f_cut->get_next_number_h_inst( i_number_req ).

    cl_abap_unit_assert=>assert_equals(
      act   = r_number-number
      exp   = '997'
     msg   = 'Test getting number with PC hierarchy, when the first number is taken'
*     level =
    ).
  ENDMETHOD.       "get_Next_Number_H_Inst


ENDCLASS.       "ltc_Util