CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE travel.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE travel.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE travel.

    METHODS read FOR READ
      IMPORTING keys FOR READ travel RESULT result.

    METHODS set_status_booked FOR MODIFY
      IMPORTING keys FOR ACTION travel~set_status_booked RESULT result.


ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.


  METHOD delete.
  ENDMETHOD.

METHOD update.
     DATA lt_travel_update TYPE TABLE FOR UPDATE zi_travel_u_xx.

    READ ENTITIES OF zi_travel_u_xx IN LOCAL MODE
     ENTITY travel
       ALL FIELDS WITH CORRESPONDING #( entities )
     RESULT DATA(lt_travel).

    LOOP AT entities INTO DATA(entity).

      READ TABLE lt_travel INTO DATA(ls_travel) WITH KEY entity COMPONENTS traveluuid = entity-traveluuid.

      IF sy-subrc = 0.

        DATA(lv_okay) = abap_true.

        ls_travel = CORRESPONDING #( BASE ( ls_travel ) entity USING CONTROL ).

        APPEND VALUE #( %tky        = entity-%tky
                        %state_area = 'BEGINENDDATE' ) TO reported-travel.

        " Perform some easy Business Logic
        IF ( entity-%control-begindate = if_abap_behv=>mk-on OR
             entity-%control-enddate   = if_abap_behv=>mk-on ) AND
           ( ls_travel-begindate > ls_travel-enddate ).

          lv_okay = abap_false.

          APPEND VALUE #( %tky = entity-%tky ) TO failed-travel.
          APPEND VALUE #( %tky = entity-%tky
                          %msg = NEW zcx_flight_legacy( textid = zcx_flight_legacy=>end_date_before_begin_date
                                                          begin_date = entity-begindate
                                                          end_date   = entity-enddate
                                                          travel_id  = entity-TravelID
                                                          severity = if_abap_behv_message=>severity-error )
                          %element-begindate = if_abap_behv=>mk-on
                          %element-enddate   = if_abap_behv=>mk-on
                          %state_area        = 'BEGINENDDATE' ) TO reported-travel.
        ENDIF.

        IF entity-%control-agencyid = if_abap_behv=>mk-on AND entity-agencyid IS INITIAL.

          lv_okay = abap_false.

          APPEND VALUE #( traveluuid = entity-traveluuid ) TO failed-travel.
          APPEND VALUE #( traveluuid = entity-traveluuid
                          %msg = NEW zcx_flight_legacy( textid    = zcx_flight_legacy=>agency_unkown
                                                          agency_id = entity-AgencyID
                                                          severity  = if_abap_behv_message=>severity-error )
                          %element-agencyid = if_abap_behv=>mk-on ) TO reported-travel.
        ENDIF.

        IF entity-%control-customerid = if_abap_behv=>mk-on AND entity-customerid IS INITIAL.

          lv_okay = abap_false.

          APPEND VALUE #( traveluuid = entity-traveluuid ) TO failed-travel.
          APPEND VALUE #( traveluuid = entity-traveluuid
                          %msg = NEW zcx_flight_legacy( textid    = zcx_flight_legacy=>customer_unkown
                                                          agency_id = entity-CustomerID
                                                          severity  = if_abap_behv_message=>severity-error )
                          %element-customerid = if_abap_behv=>mk-on ) TO reported-travel.
        ENDIF.


        IF lv_okay = abap_true.

          "Update Local and Total ETag
          GET TIME STAMP FIELD ls_travel-lastchangedat.

          APPEND CORRESPONDING #( ls_travel ) TO lt_travel_update.

        ENDIF.


      ELSE.

        APPEND VALUE #( traveluuid = entity-traveluuid ) TO failed-travel.
        APPEND VALUE #( traveluuid = entity-traveluuid
                        %msg = NEW zcx_flight_legacy( textid     = zcx_flight_legacy=>travel_unknown
                                                        travel_id  = ls_travel-TravelID
                                                        severity   = if_abap_behv_message=>severity-error )
                        %element-traveluuid = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

    ENDLOOP.
    zcl_travel_buffer=>get_instance(  )->set_mt_travel_update( CORRESPONDING #( lt_travel_update MAPPING FROM ENTITY ) ).
  ENDMETHOD.


  METHOD read.
  ENDMETHOD.

  METHOD set_status_booked.
  ENDMETHOD.

ENDCLASS.


