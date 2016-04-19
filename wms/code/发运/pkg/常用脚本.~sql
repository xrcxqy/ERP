select ooh.header_id 订单ID,
       ooh.order_number 订单号,
       ooh.order_type_id 订单类型ID,
       ott.name 订单类型, 
       ac.CUSTOMER_ID 客户ID,
       ac.CUSTOMER_NAME 客户名称,
       ac.CUSTOMER_NUMBER 客户编号, 
       
       ooh.*
  from oe_order_headers_all ooh,
       oe_transaction_types_tl ott,
       ar_customers ac
 where 1 = 1
   and ooh.order_type_id = ott.transaction_type_id(+)
   and ott.language = userenv('LANG')
   and ooh.sold_to_org_id = ac.CUSTOMER_ID
   and ooh.order_number = 11100042461;
