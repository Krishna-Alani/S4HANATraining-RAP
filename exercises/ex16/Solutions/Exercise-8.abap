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
ENDCLASS.
