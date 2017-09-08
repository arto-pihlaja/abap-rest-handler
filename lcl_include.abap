*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
DEFINE fill_error_msg.
  &1-type = 'E'.
  &1-id = 'ZBSM'.
  &1-number = &2.
  message e&2(zbsm) with &3 &4 into &1-message.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       INTERFACE lif_sub
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_sub.
  METHODS:
    get_nr
      IMPORTING i_num_req TYPE zcl_bsm_util=>number_req_t
      RETURNING value(r_num) TYPE zcl_bsm_util=>number_t,
    nr_free
      IMPORTING i_subobj TYPE NRSOBJ i_number TYPE zcl_bsm_util=>number_t-number
      RETURNING value(r_free) TYPE boole_d.


ENDINTERFACE.                    "lif_sub

*----------------------------------------------------------------------*
*       CLASS lcl_sub DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sub DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_sub.
ENDCLASS.                    "lcl_sub DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_sub IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sub IMPLEMENTATION.
  METHOD lif_sub~get_nr.
    DATA: subobj TYPE char6,
          hier_nr TYPE zmdm_hier_nr,
          number TYPE zcl_bsm_util=>number_t,
          bukrs TYPE bukrs.

    FIELD-SYMBOLS: <ret> TYPE bapiret2.

*  Get nrobj etc with ccode and type
    CASE i_num_req-type.
      WHEN 'PC' OR 'CC'.
        subobj = i_num_req-type.
        bukrs = i_num_req-code.
        SELECT SINGLE * FROM zmdm_hier_nr INTO hier_nr
          WHERE bukrs = bukrs
            AND subobject = subobj.
    ENDCASE.
    r_num-rc = sy-subrc.
    IF r_num-rc <> 0.
      APPEND INITIAL LINE TO r_num-return ASSIGNING <ret>.
      fill_error_msg <ret> 006 subobj bukrs.
      RETURN.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = hier_nr-nrrangenr
        object                  = 'ZMDM_NOBJ'
*       QUANTITY                = '1'
        subobject               = subobj
*       TOYEAR                  = '0000'
*       IGNORE_BUFFER           = ' '
      IMPORTING
        number                  = r_num-number
*       QUANTITY                = QUANTITY
*       returncode              = returncode
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    r_num-rc = sy-subrc.
    IF r_num-rc <> 0.
      APPEND INITIAL LINE TO r_num-return ASSIGNING <ret>.
      fill_error_msg <ret> 001 '' ''.
    ENDIF.
  ENDMETHOD.                    "lif_sub~get_nr
  METHOD lif_sub~nr_free.
    DATA: l_kostl TYPE kostl,
          l_prctr TYPE prctr.
    CASE i_subobj.
      WHEN 'CC'.
        l_kostl = i_number.
        SELECT SINGLE kostl FROM csks
          INTO l_kostl
          WHERE kokrs = zcl_bsm_profitcenter=>co_kokrs_posti
          AND kostl = l_kostl
          AND datbi >= sy-datlo
          AND datab <= sy-datlo.
        IF sy-subrc = 0.
          r_free = abap_false.
        ELSE.
          r_free = abap_true.
        ENDIF.
      WHEN 'PC'.
        l_prctr = i_number.
        SELECT SINGLE prctr FROM cepc
          INTO l_prctr
          WHERE
            prctr = l_prctr
            AND datbi >= sy-datlo
            AND kokrs = zcl_bsm_profitcenter=>co_kokrs_posti
            AND datab <= sy-datlo.
        IF sy-subrc = 0.
          r_free = abap_false.
        ELSE.
          r_free = abap_true.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "lif_sub~nr_free
ENDCLASS.                    "lcl_sub IMPLEMENTATION