unmanaged implementation in class zcl_bp_travel_u_xx unique;
strict;
with draft;
define behavior for ZI_TRAVEL_U_XX alias travel draft table zdtravel_u_xx
//late numbering
lock master total etag LastChangedAt
authorization master ( instance )
//etag master <field_name>
{
  create;
  update;
  delete;

  draft determine action Prepare;
  draft action Edit;
  draft action Activate;
  draft action Discard;
  draft action Resume;

  field ( numbering : managed ) TravelUUID;

  action set_status_booked result [1] $self;

  mapping for /dmo/a_travel_d
  {
    TravelUUID    = travel_uuid;
    TravelID      = travel_id;
    AgencyID      = agency_id;
    CustomerID    = customer_id;
    BeginDate     = begin_date;
    EndDate       = end_date;
    BookingFee    = booking_fee;
    TotalPrice    = total_price;
    CurrencyCode  = currency_code;
    Memo          = description;
    Status        = overall_status;
    CreatedBy     = local_created_by;
    CreatedAt     = local_created_at;
    LastChangedBy = local_last_changed_by;
    LastChangedAt = local_last_changed_at;
  }
}