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
  ENDMETHOD.

  METHOD set_status_booked.
  ENDMETHOD.

ENDCLASS.
