CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    Methods get_authorizations FOR AUTHORIZATION IMPORTING keys REQUEST requested_authorizations FOR travel RESULT result.
    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE travel.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE travel.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE travel.

    METHODS read FOR READ
      IMPORTING keys FOR READ travel RESULT result.

    METHODS set_status_booked FOR MODIFY
      IMPORTING keys FOR ACTION travel~set_status_booked RESULT result.

    METHODS get_uuid
      RETURNING
        VALUE(r_result) TYPE zi_travel_u_xx-traveluuid.

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
          entity-traveluuid = get_uuid( ).
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

  METHOD delete.
    DATA lt_travel_delete TYPE STANDARD TABLE OF /dmo/a_travel_d.

    READ ENTITIES OF zi_Travel_u_xx IN LOCAL MODE
      ENTITY travel
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travel).


    LOOP AT keys INTO DATA(key).

      READ TABLE lt_travel INTO DATA(ls_travel) WITH KEY entity COMPONENTS traveluuid = key-traveluuid.

      IF sy-subrc = 0.

        APPEND VALUE #( travel_uuid = key-traveluuid ) TO lt_travel_delete.
        "Clear state area for instance
        APPEND VALUE #( traveluuid  = ls_travel-traveluuid
                        %state_area = if_abap_behv=>state_area_all ) TO reported-travel.
      ELSE.

        APPEND VALUE #( traveluuid = ls_travel-traveluuid ) TO failed-travel.
        APPEND VALUE #( traveluuid = ls_travel-traveluuid
                        %msg = NEW zcx_flight_legacy( textid     = zcx_flight_legacy=>travel_unknown
                                                        travel_id  = ls_travel-TravelID
                                                        severity   = if_abap_behv_message=>severity-error )
                        %element-traveluuid = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

    ENDLOOP.
    zcl_travel_buffer=>get_instance(  )->set_mt_travel_delete( lt_travel_delete ).
  ENDMETHOD.

  METHOD read.

    SELECT * FROM /dmo/a_travel_d INTO TABLE @DATA(lt_travel) FOR ALL ENTRIES IN @keys WHERE travel_uuid EQ @keys-traveluuid.

    data(lt_create_buffer) = zcl_travel_buffer=>get_instance(  )->get_mt_travel_create( ).
    data(lt_update_buffer) = zcl_travel_buffer=>get_instance(  )->get_mt_travel_update( ).
    data(lt_delete_buffer) = zcl_travel_buffer=>get_instance(  )->get_mt_travel_delete( ).

    LOOP AT keys INTO data(key).
      READ TABLE lt_create_buffer ASSIGNING FIELD-SYMBOL(<s_create_buffer>) WITH KEY travel_uuid = key-traveluuid.
      IF sy-subrc = 0.
        INSERT <s_create_buffer> INTO TABLE lt_travel.
      ENDIF.

      READ TABLE lt_update_buffer ASSIGNING FIELD-SYMBOL(<s_update_buffer>) WITH KEY travel_uuid = key-traveluuid.
      IF sy-subrc = 0.
        MODIFY TABLE lt_travel FROM <s_update_buffer>.
      ENDIF.

      READ TABLE lt_delete_buffer TRANSPORTING NO FIELDS WITH KEY travel_uuid = key-traveluuid.
      IF sy-subrc = 0.
        DELETE lt_travel WHERE travel_uuid = key-traveluuid.
      ENDIF.

    ENDLOOP.

    result = CORRESPONDING #( lt_travel MAPPING TO ENTITY ).

  ENDMETHOD.


  METHOD set_status_booked.
    " Modify travel instance
    MODIFY ENTITIES OF zi_travel_u_xx IN LOCAL MODE
           ENTITY travel
              UPDATE FIELDS ( status )
              WITH VALUE #( FOR key IN keys ( %tky = key-%tky
                                              status = 'B' ) ) "Booked
           FAILED   failed
           REPORTED reported.

    " Read changed data for action result
    READ ENTITIES OF zi_travel_u_xx IN LOCAL MODE
         ENTITY travel ALL FIELDS WITH
            CORRESPONDING #( keys  )
         RESULT DATA(lt_travel).

    result = VALUE #( FOR travel IN lt_travel ( %tky      = travel-%tky
                                                %param    = travel ) ).
  ENDMETHOD.


  METHOD get_uuid.
    TRY.
        r_result  = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
      CATCH cx_uuid_error.
        ASSERT 1 = 0.
    ENDTRY.
  ENDMETHOD.


  METHOD get_authorizations.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZI_TRAVEL_U_XX DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZI_TRAVEL_U_XX IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.

    DELETE /dmo/a_travel_d FROM TABLE @( zcl_travel_buffer=>get_instance(  )->get_mt_travel_delete(  ) ).
    INSERT /dmo/a_travel_d FROM TABLE  @( zcl_travel_buffer=>get_instance(  )->get_mt_travel_create(  ) ).
    UPDATE /dmo/a_travel_d FROM TABLE  @( zcl_travel_buffer=>get_instance(  )->get_mt_travel_update(  ) ).

  ENDMETHOD.

  METHOD cleanup.
    zcl_travel_buffer=>get_instance(  )->set_mt_travel_delete( VALUE #( ) ).
    zcl_travel_buffer=>get_instance(  )->set_mt_travel_create( VALUE #( ) ).
    zcl_travel_buffer=>get_instance(  )->set_mt_travel_update( VALUE #( ) ).
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.