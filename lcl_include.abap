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
      IMPORTING
          i_num_req TYPE zcl_bsm_util=>number_req_t
          i_use_h TYPE boole_d OPTIONAL
      RETURNING value(r_num) TYPE zcl_bsm_util=>number_t,
    nr_free
      IMPORTING
        i_subobj TYPE nrsobj i_number TYPE zcl_bsm_util=>number_t-number
        i_use_h TYPE boole_d OPTIONAL
      RETURNING value(r_free) TYPE boole_d,
    get_pchier
      RETURNING value(r_hier) TYPE zcl_bsm_util=>hiernode_tt,
    get_cchier
      RETURNING value(r_hier) TYPE zcl_bsm_util=>hiernode_tt,
    get_hier_nr IMPORTING i_subobj TYPE nrsobj
      RETURNING value(r_hier_nr) TYPE zcl_bsm_util=>hier_nr_tt,
    number_get_next
      IMPORTING
        i_nrnr TYPE nrnr
        i_subobj TYPE nrsobj
    RETURNING value(r_nr) TYPE zcl_bsm_util=>number_t.
ENDINTERFACE.                    "lif_sub

*----------------------------------------------------------------------*
*       CLASS lcl_sub DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sub DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_sub.
*    METHODS:
*      get_pcnr
*        IMPORTING
*            i_num_req TYPE zcl_bsm_util=>number_req_t
*            i_use_h TYPE boole_d OPTIONAL
*        RETURNING value(r_num) TYPE zcl_bsm_util=>number_t.
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
          bukrs TYPE bukrs,
          grpname TYPE grpname.

    FIELD-SYMBOLS: <ret> TYPE bapiret2.
    IF i_use_h = abap_false.
*  Get nrobj etc with ccode and type
      CASE i_num_req-type.
        WHEN 'PC' OR 'CC'.
          subobj = i_num_req-type.
          bukrs = i_num_req-code.
          SELECT SINGLE nrrangenr FROM zmdm_hier_nr INTO hier_nr-nrrangenr
            WHERE bukrs = bukrs
              AND subobject = subobj.
      ENDCASE.
      r_num-rc = sy-subrc.
    ELSE.
* NOT IN USE
**  Get nrrange with hierarchy and type. In this case, we have separate tables for PC and CC
**  First, we'll see if there's a nrrange attached directly to the given hierarchy level.
**  If not, we'll try with the parent of the hierarchy level and so on...
*      CASE i_num_req-type.
*        WHEN 'PC' .
*          subobj = i_num_req-type.
*          grpname = i_num_req-code.
*          SELECT SINGLE nrrangenr FROM zmdm_pchier_nr INTO hier_nr
*            WHERE grpname = grpname.
*        WHEN 'CC' .
*          subobj = i_num_req-type.
*          grpname = i_num_req-code.
**          SELECT SINGLE nrrangenr FROM zmdm_cchier_nr INTO hier_nr
**            WHERE grpname = grpname.
*      ENDCASE.
*      r_num-rc = sy-subrc.
    ENDIF.
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
          WHERE kokrs = zcl_bsm_profitcenter=>co_kokrs_comp
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
            AND kokrs = zcl_bsm_profitcenter=>co_kokrs_comp
            AND datab <= sy-datlo.
        IF sy-subrc = 0.
          r_free = abap_false.
        ELSE.
          r_free = abap_true.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "lif_sub~nr_free
  METHOD lif_sub~get_pchier.
    DATA: hierarchyvalues TYPE STANDARD TABLE OF bapi1116_values.
    CALL FUNCTION 'BAPI_PROFITCENTERGRP_GETDETAIL'
      EXPORTING
        controllingarea       = zcl_bsm_profitcenter=>co_kokrs_comp
        groupname             = zcl_bsm_profitcenter=>co_pcgrp_root
*     LANGUAGE              = LANGUAGE
*   IMPORTING
*     RETURN                = RETURN
      TABLES
        hierarchynodes        = r_hier
      hierarchyvalues       = hierarchyvalues
              .
  ENDMETHOD.                    "lif_sub~get_pchier
  METHOD lif_sub~get_cchier.
    DATA: hierarchyvalues TYPE STANDARD TABLE OF bapi1116_values.
    CALL FUNCTION 'BAPI_COSTCENTERGROUP_GETDETAIL'
      EXPORTING
        controllingarea = zcl_bsm_profitcenter=>co_kokrs_comp
        groupname       = zcl_bsm_costcenter=>co_ccgrp_root
*       LANGUAGE        = LANGUAGE
*      IMPORTING
*        return          = r_return
      TABLES
        hierarchynodes  = r_hier
        hierarchyvalues = hierarchyvalues.
  ENDMETHOD.                    "lif_sub~get_cchier
  METHOD lif_sub~number_get_next.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = i_nrnr
        object                  = 'ZMDM_NOBJ'
*       QUANTITY                = '1'
        subobject               = i_subobj
*       TOYEAR                  = '0000'
*       IGNORE_BUFFER           = ' '
      IMPORTING
        number                  = r_nr-number
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
    r_nr-rc = sy-subrc.
  ENDMETHOD.                    "lif_sub~number_get_next
  METHOD lif_sub~get_hier_nr.
    CASE i_subobj.
      WHEN 'PC'.
        SELECT * FROM zmdm_pchier_nr
          INTO TABLE r_hier_nr.
      WHEN 'CC'.
        SELECT * FROM zmdm_cchier_nr
          INTO TABLE r_hier_nr.
    ENDCASE.

  ENDMETHOD.                    "lif_sub~get_hier_nr
ENDCLASS.                    "lcl_sub IMPLEMENTATION