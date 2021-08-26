## Definition
**RTO (Return to Origin)** refers to the non-deliverability of a package and its return to the seller’s address.

## Objective
Need to create a predictive model that is capable of to predict whether a shipment will be RTO or not at the time of order placed (before shipped).

## Data Dictionary

|Dataset columns        |Description
|:-----------------------|:-------------------------------------------------------------------------|
|order_id               |Unique Id of order
|shipment_id            |Unique Id of shipment
|awb_code               |(Airwaybill) Code that provides detailed information about the shipment
|shipment_status        |Status of the shipment
|order_source           |Source of order
|company_id             |Unique id of the company
|channel_base_code      |Channel code by which order arrived
|shipment_total         |The total cost of shipment
|assigned_date_time     |Date and time at which shipment assigned
|awb_shipped_date       |Date and time at which shipment shipped
|awb_delivered_date     |Date and time at which shipment delivered
|awb_rto_initiated_date |Date and time at which shipment RTO initiated
|awb_rto_delivered_date |Date and time at which shipment RTO delivered to their origin.
|gender                 |Gender of buyer
|cod                    |Prepaid shipment=0 | Cod Shipment=1
|ETD                    |Estimate time of delivery
|delivery_postcode      |Shipment delivery postcode
|delivery_city          |Shipment delivery city
|delivery_state         |Shipment Delivery state
|is_rto                 |RTO Flag (Target Column)

## Data Cleaning

## Model Building
