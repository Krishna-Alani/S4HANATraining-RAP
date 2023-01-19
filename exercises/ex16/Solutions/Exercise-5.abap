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
 METHOD create.
   DATA lt_travel_create TYPE STANDARD TABLE OF /dmo/a_travel_d.

    LOOP AT entities INTO DATA(entity).

      DATA(lv_okay) = abap_true.

      " Perform some easy Business Logic - Here the full business logic needs to be called!
      IF entity-begindate > entity-enddate.

        lv_okay = abap_false.

        APPEND VALUE #( %cid = entity-%cid ) TO failed-travel.
        APPEND VALUE #( %cid = entity-%cid
                        %msg = NEW zcx_flight_legacy( textid = zcx_flight_legacy=>end_date_before_begin_date
                                                        begin_date = entity-begindate
                                                        end_date   = entity-enddate
                                                        travel_id  = entity-TravelID
                                                        severity = if_abap_behv_message=>severity-error )
                        %element-begindate = if_abap_behv=>mk-on
                        %element-enddate   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

      IF entity-agencyid IS INITIAL.

        lv_okay = abap_false.

        APPEND VALUE #( %cid = entity-%cid ) TO failed-travel.
        APPEND VALUE #( %cid = entity-%cid
                        %msg = NEW zcx_flight_legacy( textid    = zcx_flight_legacy=>agency_unkown
                                                        agency_id = entity-AgencyID
                                                        severity  = if_abap_behv_message=>severity-error )
                        %element-agencyid = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

      IF entity-customerid IS INITIAL.

        lv_okay = abap_false.

        APPEND VALUE #( %cid = entity-%cid ) TO failed-travel.
        APPEND VALUE #( %cid = entity-%cid
                        %msg = NEW zcx_flight_legacy( textid    = zcx_flight_legacy=>customer_unkown
                                                        agency_id = entity-CustomerID
                                                        severity  = if_abap_behv_message=>severity-error )
                        %element-customerid = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

      IF lv_okay = abap_true.

        "Draw UUID if not provided externally
        IF entity-traveluuid IS INITIAL.
          entity-traveluuid = _get_uuid( ).
        ENDIF.

        " Set Local and Total ETag
        GET TIME STAMP FIELD entity-lastchangedat.

        APPEND CORRESPONDING #( entity MAPPING FROM ENTITY ) TO lt_travel_create.


        APPEND VALUE #( %cid       = entity-%cid
                        traveluuid = entity-traveluuid ) TO mapped-travel.
      ENDIF.

    ENDLOOP.

    zcl_travel_buffer=>get_instance(  )->set_mt_travel_create( lt_travel_create ).

  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD set_status_booked.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZI_TRAVEL_U_XX DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS check_before_save REDEFINITION.

    METHODS finalize          REDEFINITION.

    METHODS save              REDEFINITION.

ENDCLASS.

CLASS lsc_ZI_TRAVEL_U_XX IMPLEMENTATION.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD finalize.
  ENDMETHOD.

  METHOD save.

    DELETE /dmo/a_travel_d FROM TABLE @( zcl_travel_buffer=>get_instance(  )->get_mt_travel_delete(  ) ).
    INSERT /dmo/a_travel_d FROM TABLE  @( zcl_travel_buffer=>get_instance(  )->get_mt_travel_create(  ) ).
    UPDATE /dmo/a_travel_d FROM TABLE  @( zcl_travel_buffer=>get_instance(  )->get_mt_travel_update(  ) ).

  ENDMETHOD.

ENDCLASS.
