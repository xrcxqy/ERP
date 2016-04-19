CREATE OR REPLACE PACKAGE cux_oe_ship_pkg IS
  PROCEDURE split_line(p_from_detail_id IN NUMBER,
                       p_split_qty      IN NUMBER,
                       x_status         OUT NUMBER,
                       x_msg            OUT VARCHAR2,
                       x_out_detail_id  OUT wsh_delivery_details_grp.action_out_rec_type,
                       X_ID             OUT NUMBER);
  PROCEDURE Autocreate_Deliveries(p_api_version_number IN NUMBER,
                                  x_return_status      OUT NOCOPY VARCHAR2,
                                  x_msg_count          OUT NOCOPY NUMBER,
                                  x_msg_data           OUT NOCOPY VARCHAR2,
                                  p_line_rows          IN WSH_UTIL_CORE.id_tab_type,
                                  x_del_rows           OUT NOCOPY wsh_util_core.id_tab_type);
  PROCEDURE Create_Batch(p_api_version_number    IN NUMBER,
                         x_return_status         OUT NOCOPY VARCHAR2,
                         x_msg_count             OUT NOCOPY NUMBER,
                         x_msg_data              OUT NOCOPY VARCHAR2,
                         p_Delivery_Id           in number,
                         P_pick_grouping_rule_id IN NUMBER,
                         p_header_id             in number);
  PROCEDURE cancel_line(p_header_id IN NUMBER,
                        p_line_id   IN NUMBER,
                        x_status    OUT VARCHAR2,
                        x_msg_count OUT NOCOPY NUMBER,
                        x_msg_data  OUT NOCOPY VARCHAR2);
  PROCEDURE create_line(p_header_id IN NUMBER);
  procedure main(x_errbuf          OUT NOCOPY VARCHAR2,
                 x_retcode         OUT NOCOPY NUMBER,
                 p_header_id       in number,
                 p_ORGANIZATION_ID in number,
                 p_flag            in varchar2,
                 p_txt             in varchar2,
                 p_request_number  in varchar2);

  function get_issues_qty(p_delivery_detail_id in number,
                          p_line_id            in number) return number;
  procedure main_new(p_header_id       in number,
                     p_ORGANIZATION_ID in number,
                     p_flag            in out varchar2,
                     p_txt             in out varchar2,
                     p_request_number  in out varchar2);
  procedure main_old(p_header_id       in number,
                     p_ORGANIZATION_ID in number,
                     p_flag            in out varchar2,
                     p_txt             in out varchar2,
                     p_request_number  in out varchar2);
  /*  procedure main_new1(p_header_id       in number,
  p_ORGANIZATION_ID in number,
  p_flag            in out varchar2,
  p_txt             in out varchar2,
  p_request_number  in out varchar2);  */
  procedure main_check(p_header_id       in number,
                       p_ORGANIZATION_ID in number,
                       p_flag            in out varchar2,
                       p_txt             in out varchar2,
                       p_request_number  in out varchar2);
  procedure main_new_jj(p_header_id       in number,
                        p_ORGANIZATION_ID in number,
                        p_flag            in out varchar2,
                        p_txt             in out varchar2,
                        p_request_number  in out varchar2);

  procedure main_new_dj(p_header_id       in number,
                        p_ORGANIZATION_ID in number,
                        p_flag            in out varchar2,
                        p_txt             in out varchar2,
                        p_request_number  in out varchar2);
  procedure print_error_proc(p_header_id in number);
  procedure process_dup_item(p_header_id       in number,
                             p_organization_id number,
                             x_ret_code        out varchar2,
                             x_ret_msg         out varchar2);
  function get_ship_doc_status(p_line_id in number, p_ship_qty in number)
    return varchar2;
end;
/
CREATE OR REPLACE PACKAGE BODY cux_oe_ship_pkg IS
  PROCEDURE split_line(p_from_detail_id IN NUMBER,
                       p_split_qty      IN NUMBER,
                       x_status         OUT NUMBER,
                       x_msg            OUT VARCHAR2,
                       x_out_detail_id  OUT wsh_delivery_details_grp.action_out_rec_type,
                       x_id             OUT NUMBER) AS
  
    p_api_version_number NUMBER;
    --  p_from_detail_id                   NUMBER;
    x_new_detail_id           NUMBER;
    x_split_quantity          NUMBER;
    x_split_quantity2         NUMBER;
    p_commit                  VARCHAR2(30);
    p_validation_level        NUMBER;
    x_return_status           VARCHAR2(2000);
    x_msg_count               NUMBER;
    x_msg_data                VARCHAR2(2000);
    v_organization_id         number;
    v_inventory_item_id       number;
    v_requested_quantity_uom  VARCHAR2(2000);
    v_requested_quantity_uom2 VARCHAR2(2000);
  BEGIN
  
    p_api_version_number := 1.0;
    --p_from_detail_id      := 3973467;    3973469   66525
    x_split_quantity := p_split_qty;
    select wdd.organization_id,
           wdd.inventory_item_id,
           wdd.requested_quantity_uom,
           wdd.requested_quantity_uom2
      into v_organization_id,
           v_inventory_item_id,
           v_requested_quantity_uom,
           v_requested_quantity_uom2
      from wsh_delivery_details wdd
     where wdd.delivery_detail_id = p_from_detail_id;
  
    x_split_quantity2 := inv_convert.inv_um_convert(item_id         => v_inventory_item_id,
                                                    lot_number      => null,
                                                    organization_id => v_organization_id,
                                                    precision       => null,
                                                    from_quantity   => p_split_qty,
                                                    from_unit       => v_requested_quantity_uom,
                                                    to_unit         => v_requested_quantity_uom2,
                                                    from_name       => null,
                                                    to_name         => null);
  
    wsh_delivery_details_pub.split_line(p_api_version      => p_api_version_number,
                                        p_init_msg_list    => FND_API.G_TRUE,
                                        p_commit           => p_commit,
                                        p_validation_level => p_validation_level,
                                        x_return_status    => x_return_status,
                                        x_msg_count        => x_msg_count,
                                        x_msg_data         => x_msg_data,
                                        p_from_detail_id   => p_from_detail_id,
                                        x_new_detail_id    => x_new_detail_id,
                                        x_split_quantity   => x_split_quantity,
                                        x_split_quantity2  => x_split_quantity2);
  
    IF (x_return_status = WSH_UTIL_CORE.G_RET_STS_SUCCESS) THEN
      COMMIT;
      dbms_output.put_line('Return Status     = ' ||
                           SUBSTR(x_return_status, 1, 255));
      dbms_output.put_line('From Delivery detail Id  = ' ||
                           TO_CHAR(p_from_detail_id));
      dbms_output.put_line('New Delivery detail Id     = ' ||
                           TO_CHAR(x_new_detail_id));
      dbms_output.put_line('Split quantity         = ' || x_split_quantity);
      dbms_output.put_line('Split quantity2     = ' || x_split_quantity2);
    ELSE
      dbms_output.put_line('Return Status     = ' ||
                           SUBSTR(x_return_status, 1, 255));
      dbms_output.put_line('Msg Count     				= ' || TO_CHAR(x_msg_count));
      dbms_output.put_line('From Delivery detail Id	= ' ||
                           TO_CHAR(P_from_detail_id));
      dbms_output.put_line('Msg Data      				= ' ||
                           SUBSTR(x_msg_data, 1, 255));
    
      --IF x_msg_count >1 THEN
      FOR I IN 1 .. 10 LOOP
        dbms_output.put_line(I || '. ' || SUBSTR(FND_MSG_PUB.Get(p_encoded => FND_API.G_FALSE),
                                                 1,
                                                 255));
      END LOOP;
      --END IF;
    END IF;
  
  END split_line;

  PROCEDURE Autocreate_Deliveries(p_api_version_number IN NUMBER,
                                  x_return_status      OUT NOCOPY VARCHAR2,
                                  x_msg_count          OUT NOCOPY NUMBER,
                                  x_msg_data           OUT NOCOPY VARCHAR2,
                                  p_line_rows          IN WSH_UTIL_CORE.id_tab_type,
                                  x_del_rows           OUT NOCOPY wsh_util_core.id_tab_type) as
  begin
    wsh_delivery_details_pub.autocreate_deliveries(p_api_version_number => p_api_version_number,
                                                   p_init_msg_list      => FND_API.G_TRUE,
                                                   p_commit             => FND_API.G_FALSE,
                                                   x_return_status      => x_return_status,
                                                   x_msg_count          => x_msg_count,
                                                   x_msg_data           => x_msg_data,
                                                   p_line_rows          => p_line_rows,
                                                   x_del_rows           => x_del_rows);
    IF (x_return_status = WSH_UTIL_CORE.G_RET_STS_SUCCESS) THEN
      COMMIT;
      dbms_output.put_line('Return Status = ' ||
                           SUBSTR(x_return_status, 1, 255));
      dbms_output.put_line('Delivery Id = ' || TO_CHAR(x_del_rows(1)));
    ELSE
      dbms_output.put_line('Return Status = ' ||
                           SUBSTR(x_return_status, 1, 255));
      dbms_output.put_line('Msg Count = ' || TO_CHAR(x_msg_count));
      dbms_output.put_line('Delivery detail Id = ' ||
                           TO_CHAR(p_line_rows(1)));
      dbms_output.put_line('Msg Data = ' || SUBSTR(x_msg_data, 1, 255));
    end if;
  end;
  PROCEDURE Create_Batch(p_api_version_number    IN NUMBER,
                         x_return_status         OUT NOCOPY VARCHAR2,
                         x_msg_count             OUT NOCOPY NUMBER,
                         x_msg_data              OUT NOCOPY VARCHAR2,
                         p_Delivery_Id           in number,
                         P_pick_grouping_rule_id IN NUMBER,
                         p_header_id             in number) as
    v_oe_header_id    number;
    v_organization_id number;
    l_api_name        VARCHAR2(30) := 'sale_order_pick';
    l_api_version     NUMBER := 1.0;
    l_batch_info_rec  wsh_picking_batches_pub.Batch_Info_rec;
  
    x_batch_id               NUMBER;
    x_pick_result            varchar2(1);
    v_pick_from_subinventory varchar2(100);
    v_released_status        varchar2(10);
    l_msg_return             NUMBER;
    x_request_id             NUMBER;
    v_locator_id             number;
  begin
    select distinct wdd.source_header_id,
                    wdd.organization_id,
                    wdd.released_status
      into v_oe_header_id, v_organization_id, v_released_status
      from wsh_new_deliveries       wnd,
           wsh_delivery_assignments wda,
           wsh_delivery_details     wdd
     where wnd.delivery_id = wda.delivery_id
       and wda.delivery_detail_id = wdd.delivery_detail_id
       and wnd.delivery_id = p_Delivery_Id;
  
    select a.pick_grouping_rule_id,
           a.default_stage_subinventory,
           a.existing_rsvs_only_flag,
           a.include_planned_lines,
           a.autocreate_delivery_flag,
           a.autodetail_pr_flag,
           a.allocation_method,
           a.auto_pick_confirm_flag,
           a.autopack_flag,
           a.default_stage_subinventory,
           a.default_stage_locator_id,
           a.ORGANIZATION_ID
      into l_batch_info_rec.pick_grouping_rule_id,
           l_batch_info_rec.Default_Stage_Subinventory,
           l_batch_info_rec.existing_rsvs_only_flag,
           l_batch_info_rec.include_planned_lines,
           l_batch_info_rec.autocreate_delivery_flag,
           l_batch_info_rec.autodetail_pr_flag,
           l_batch_info_rec.allocation_method,
           l_batch_info_rec.auto_pick_confirm_flag,
           l_batch_info_rec.autopack_flag,
           l_batch_info_rec.Default_Stage_Subinventory,
           l_batch_info_rec.Default_Stage_Locator_Id,
           l_batch_info_rec.Organization_Id
      from WSH_PICKING_RULES_V a
     where a.PICKING_RULE_ID = P_pick_grouping_rule_id;
  
    select a.pick_from_subinventory, a.locator_id
      into v_pick_from_subinventory, v_locator_id
      from cux.cux_oe_ship_headers a
     where header_id = p_header_id;
    select c.customer_id,
           C.CUSTOMER_NUMBER,
           H.HEADER_ID,
           H.ORDER_TYPE_ID,
           H.ORDER_NUMBER,
           7
      INTO l_batch_info_rec.customer_id,
           l_batch_info_rec.customer_number,
           l_batch_info_rec.order_header_id,
           l_batch_info_rec.order_type_id,
           l_batch_info_rec.order_number,
           l_batch_info_rec.document_set_id
      from oe_order_headers_all h, AR_CUSTOMERS C
     where h.header_id = v_oe_header_id
       AND H.SOLD_TO_ORG_ID = C.CUSTOMER_ID;
    if v_released_status <> 'B' then
      l_batch_info_rec.backorders_only_flag := 'E';
    else
      l_batch_info_rec.backorders_only_flag := 'O';
    end if;
    /*    l_batch_info_rec.existing_rsvs_only_flag  := 'N';
    l_batch_info_rec.from_scheduled_ship_date := NULL; --to_date('28-07-2010','DD-MM-YYYY');
    l_batch_info_rec.organization_id          := v_organization_id;
    l_batch_info_rec.include_planned_lines    := 'N';
    l_batch_info_rec.autocreate_delivery_flag := 'Y';
    l_batch_info_rec.autodetail_pr_flag       := 'N';--自动分配
    l_batch_info_rec.allocation_method        := 'I';*/
    if v_locator_id is not null then
      l_batch_info_rec.pick_from_locator_id := v_locator_id; --182;
    end if;
    /*    l_batch_info_rec.auto_pick_confirm_flag   := 'N';
    l_batch_info_rec.autopack_flag            := 'N';*/
    l_batch_info_rec.Pick_From_Subinventory := v_pick_from_subinventory;
    l_batch_info_rec.Delivery_Id            := p_Delivery_Id;
    -- Call Private API
    --更新仓库
    /*      update wsh_delivery_details wdd
     set wdd.subinventory=v_pick_from_subinventory,
         wdd.client_id=v_locator_id
    where wdd.delivery_detail_id in (select a.delivery_detail_id from cux_oe_ship_lines_v a where a.HEADER_ID=p_header_id);*/
  
    wsh_picking_batches_pub.Create_Batch(p_api_version   => l_api_version,
                                         x_return_status => x_return_status,
                                         x_msg_count     => x_msg_count,
                                         x_msg_data      => x_msg_data,
                                         p_batch_rec     => l_batch_info_rec,
                                         x_batch_id      => x_batch_id);
  
    dbms_output.put_line(x_return_status);
    dbms_output.put_line(x_batch_id);
    IF (x_return_status = WSH_UTIL_CORE.G_RET_STS_SUCCESS) THEN
      /*     update WSH_PICKING_BATCHES a
        set a.default_stage_subinventory =l_batch_info_rec.Default_Stage_Subinventory,
            a.default_stage_locator_id =l_batch_info_rec.Default_Stage_Locator_Id
      where a.batch_id=x_batch_id;      */
      wsh_picking_batches_pub.Release_Batch(p_api_version   => l_api_version,
                                            x_return_status => x_return_status,
                                            x_msg_count     => x_msg_count,
                                            x_msg_data      => x_msg_data,
                                            p_batch_id      => x_batch_id,
                                            p_release_mode  => 'ONLINE',
                                            x_request_id    => x_request_id);
    
      IF (x_return_status = WSH_UTIL_CORE.G_RET_STS_SUCCESS) THEN
        COMMIT;
      else
        FOR i IN 1 .. x_msg_count LOOP
          fnd_msg_pub.get(p_msg_index     => i,
                          p_encoded       => 'F',
                          p_data          => x_msg_data,
                          p_msg_index_out => l_msg_return);
          dbms_output.put_line(x_msg_data);
        END LOOP;
      end if;
    else
      FOR i IN 1 .. x_msg_count LOOP
        fnd_msg_pub.get(p_msg_index     => i,
                        p_encoded       => 'F',
                        p_data          => x_msg_data,
                        p_msg_index_out => l_msg_return);
        dbms_output.put_line(x_msg_data);
      END LOOP;
    end if;
  end;
  PROCEDURE cancel_line(p_header_id IN NUMBER,
                        p_line_id   IN NUMBER,
                        x_status    OUT VARCHAR2,
                        x_msg_count OUT NOCOPY NUMBER,
                        x_msg_data  OUT NOCOPY VARCHAR2) as
    l_header_rec         Oe_Order_Pub.header_rec_type;
    l_line_tbl           Oe_Order_Pub.line_tbl_type;
    l_line_tb2           Oe_Order_Pub.line_tbl_type;
    l_action_request_tbl Oe_Order_Pub.request_tbl_type;
    /*    l_return_status          VARCHAR2(1000);
    l_msg_count              NUMBER;
    l_msg_data               VARCHAR2(1000);*/
    v_line_number            VARCHAR2(1000);
    l_index                  varchar2(1000);
    x_header_val_rec         Oe_Order_Pub.header_val_rec_type;
    x_header_adj_tbl         Oe_Order_Pub.header_adj_tbl_type;
    x_header_adj_val_tbl     Oe_Order_Pub.header_adj_val_tbl_type;
    x_header_price_att_tbl   Oe_Order_Pub.header_price_att_tbl_type;
    x_header_adj_att_tbl     Oe_Order_Pub.header_adj_att_tbl_type;
    x_header_adj_assoc_tbl   Oe_Order_Pub.header_adj_assoc_tbl_type;
    x_header_scredit_tbl     Oe_Order_Pub.header_scredit_tbl_type;
    x_header_scredit_val_tbl Oe_Order_Pub.header_scredit_val_tbl_type;
    x_line_val_tbl           Oe_Order_Pub.line_val_tbl_type;
    x_line_adj_tbl           Oe_Order_Pub.line_adj_tbl_type;
    x_line_adj_val_tbl       Oe_Order_Pub.line_adj_val_tbl_type;
    x_line_price_att_tbl     Oe_Order_Pub.line_price_att_tbl_type;
    x_line_adj_att_tbl       Oe_Order_Pub.line_adj_att_tbl_type;
    x_line_adj_assoc_tbl     Oe_Order_Pub.line_adj_assoc_tbl_type;
    x_line_scredit_tbl       Oe_Order_Pub.line_scredit_tbl_type;
    x_line_scredit_val_tbl   Oe_Order_Pub.line_scredit_val_tbl_type;
    x_lot_serial_tbl         Oe_Order_Pub.lot_serial_tbl_type;
    x_lot_serial_val_tbl     Oe_Order_Pub.lot_serial_val_tbl_type;
  begin
    fnd_global.apps_initialize(user_id      => fnd_global.USER_ID,
                               resp_id      => fnd_global.RESP_ID,
                               resp_appl_id => fnd_global.RESP_APPL_ID);
    mo_global.init('ONT'); --r12的安全机制，必须初始化否则报错
    Oe_Msg_Pub.initialize;
    Oe_Debug_Pub.initialize;
    Oe_Debug_Pub.debug_on;
    Oe_Debug_Pub.setdebuglevel(5);
  
    l_header_rec := Oe_Order_Pub.g_miss_header_rec;
    l_header_rec.header_id := p_header_id;
    l_header_rec.operation := Oe_Globals.G_OPR_UPDATE;
    l_line_tbl(1) := Oe_Order_Pub.g_miss_line_rec;
    l_line_tbl(1).line_id := p_line_id;
    l_line_tbl(1).ordered_quantity := 0;
    l_line_tbl(1).cancelled_flag := 'Y';
    l_line_tbl(1).change_reason := 'Not provided';
    l_line_tbl(1).operation := Oe_Globals.G_OPR_UPDATE;
  
    dbms_output.put_line('l_line_tbl:' || l_line_tbl(1).open_flag);
  
    Oe_Order_Pub.process_order(p_api_version_number     => 1.0,
                               p_init_msg_list          => Fnd_Api.g_false,
                               p_return_values          => Fnd_Api.g_false,
                               p_action_commit          => Fnd_Api.g_false,
                               x_return_status          => x_status,
                               x_msg_count              => x_msg_count,
                               x_msg_data               => x_msg_data,
                               p_header_rec             => l_header_rec,
                               p_line_tbl               => l_line_tbl, --就是这里
                               p_action_request_tbl     => l_action_request_tbl,
                               x_header_rec             => l_header_rec,
                               x_header_val_rec         => x_header_val_rec,
                               x_header_adj_tbl         => x_header_adj_tbl,
                               x_header_adj_val_tbl     => x_header_adj_val_tbl,
                               x_header_price_att_tbl   => x_header_price_att_tbl,
                               x_header_adj_att_tbl     => x_header_adj_att_tbl,
                               x_header_adj_assoc_tbl   => x_header_adj_assoc_tbl,
                               x_header_scredit_tbl     => x_header_scredit_tbl,
                               x_header_scredit_val_tbl => x_header_scredit_val_tbl,
                               x_line_tbl               => l_line_tb2, --r11中这里跟p_line_tbl是一样的
                               x_line_val_tbl           => x_line_val_tbl,
                               x_line_adj_tbl           => x_line_adj_tbl,
                               x_line_adj_val_tbl       => x_line_adj_val_tbl,
                               x_line_price_att_tbl     => x_line_price_att_tbl,
                               x_line_adj_att_tbl       => x_line_adj_att_tbl,
                               x_line_adj_assoc_tbl     => x_line_adj_assoc_tbl,
                               x_line_scredit_tbl       => x_line_scredit_tbl,
                               x_line_scredit_val_tbl   => x_line_scredit_val_tbl,
                               x_lot_serial_tbl         => x_lot_serial_tbl,
                               x_lot_serial_val_tbl     => x_lot_serial_val_tbl,
                               x_action_request_tbl     => l_action_request_tbl);
    /* dbms_output.put_line('l_index:' || l_index || 'l_msg_data1:' ||
                         x_msg_data);
    
    IF x_msg_count > 0 THEN
      FOR l_index IN 1 .. x_msg_count LOOP
        x_msg_data := Oe_Msg_Pub.get(p_msg_index => l_index,
                                     p_encoded   => 'F');
      END LOOP;
    END IF;
    
    dbms_output.put_line('l_index:' || l_index || 'l_msg_data:' ||
                         x_msg_data);*/
  
    IF x_status = Fnd_Api.g_ret_sts_success Then
      dbms_output.put_line('OK');
      commit;
    ELSE
      dbms_output.put_line('Failed');
      rollback;
    END IF;
  end;
  PROCEDURE create_line(p_header_id IN NUMBER) as
  begin
    null;
  end;
  function get_issues_qty(p_delivery_detail_id in number,
                          p_line_id            in number) return number as
    v_qty_1          number;
    v_qty_2          number;
    v_request_number varchar2(100);
  begin
  
    begin
      select a.request_number
        into v_request_number
        from cux.cux_oe_ship_headers a, cux.CUX_OE_SHIP_LINES b
       where a.header_id = b.header_id
         and b.line_id = p_line_id;
    exception
      when others then
        v_request_number := null;
    end;
    select nvl(sum(mtl.quantity_delivered), 0)
      into v_qty_1
      from mtl_txn_request_lines mtl, mtl_txn_request_headers mth
     where mtl.txn_source_line_detail_id = p_delivery_detail_id
       and mtl.header_id = mth.header_id
       and mth.request_number = v_request_number;
  
    /*BEGIN   
     SELECT NVL(WDD.PICKED_QUANTITY,0)
       INTO v_qty_1
       FROM WSH_DELIVERY_DETAILS WDD
      WHERE WDD.DELIVERY_DETAIL_ID =p_delivery_detail_id;    
    EXCEPTION
       WHEN OTHERS THEN
         v_qty_1:=0;
    END; */
    /*  select nvl(sum(mmt.transaction_quantity), 0)
     into v_qty_2
     from mtl_txn_request_lines mtl, wsh_delivery_details wdd,mtl_material_transactions mmt
    where  mtl.txn_source_line_detail_id = wdd.delivery_detail_id
      and mtl.organization_id=mmt.organization_id
      and mtl.line_id=mmt.move_order_line_id
      and mmt.transaction_quantity>0
      and wdd.split_from_delivery_detail_id = p_delivery_detail_id;*/
    return v_qty_1;
  end;
  
  procedure main(x_errbuf          OUT NOCOPY VARCHAR2,
                 x_retcode         OUT NOCOPY NUMBER,
                 p_header_id       in number,
                 p_ORGANIZATION_ID in number,
                 p_flag            in varchar2,
                 p_txt             in varchar2,
                 p_request_number  in varchar2) as
  
    --start of modification by bruce on 2015-8-3
    v_error_flag        VARCHAR2(10);
    v_error_exist       VARCHAR2(10);
    v_error_line_id     number := '';
    v_err_txt           varchar2(2000);
    v_process_flag      varchar2(10);
    v_exist_hardware    varchar2(10);
    v_pick_subinv       varchar2(10);
    v_hardware_subinv   varchar2(10);
    v_shipping_org_code varchar2(10);
    v_item_avail_qty    number;
    v_suggest_sub_a     varchar2(10);
    v_count_subinv      number;
    v_current_subinv    varchar2(10);
    v_count_mv_line     number;
  
    cursor cur_req_subinv is
      select distinct cpd.pick_subinv
        from cux_pick_sub_doc cpd
       where cpd.cux_pick_header_id = p_header_id;
  
    --end of modification by bruce on 2015-8-3
  
    --start of add by bruce on 20150925
    v_order_item varchar2(2000);
    
    cursor cur_order_item(p_subinv varchar2) is
      select msib.segment1
        from cux.cux_oe_ship_lines col,
             cux_pick_sub_doc      cpd,
             mtl_system_items_b    msib,
             fnd_lookup_values     flv
       where col.header_id = p_header_id
         and col.line_id = cpd.cux_pick_line_id
         and cpd.cux_pick_header_id = p_header_id
         and cpd.pick_subinv = p_subinv
         and col.inventory_item_id = msib.inventory_item_id
         and msib.organization_id = p_ORGANIZATION_ID
         and msib.segment1 like flv.lookup_code || '%'
         and nvl(col.flag, 'N') = 'Y'
         and flv.lookup_type = 'CUX_ITEM_PER_ORDER'
         and flv.language = userenv('lang');
    --end of add by bruce on 20150925
  
    --start of add by bruce on 20151008
    v_item_remark varchar2(2000);
    cursor cur_item_remark(p_subinv varchar2) is
      select col.remark
        from cux.cux_oe_ship_lines col, cux_pick_sub_doc cpd
       where col.header_id = p_header_id
         and col.line_id = cpd.cux_pick_line_id
         and cpd.cux_pick_header_id = p_header_id
         and cpd.pick_subinv = p_subinv
         and col.remark is not null
         and nvl(col.flag, 'N') = 'Y';
    --end of add by bruce on 20151008
  
    cursor c_2 is
      select --a.PICK_FROM_SUBINVENTORY,
      --a.LOCATOR_ID,
       b.INVENTORY_ITEM_ID,
       msi.segment1,
       b.LINE_NUMBER,
       b.DELIVERY_DETAIL_ID,
       b.QUANTITY,
       --a.SET_ID,
       B.OE_LINE_ID,
       --a.HEADER_ID,
       b.LINE_ID,
       oola.header_id oe_header_id, --a.OE_HEADER_ID, --modified on 20151020
       B.SOFTWARE_FLAG,
       msi.primary_uom_code,
       c.SOURCES_OF_PRODUCTS,
       oola.SHIPPING_INSTRUCTIONS,
       oola.packing_instructions,
       b.QTY_DIFF
        from /*cux_oe_ship_headers_v a,*/ --modified on 20151020
             cux_oe_ship_lines_v   b,
             cux_om_line_interface c,
             mtl_system_items_b    msi,
             oe_order_lines_all    oola
       where /*a.header_id = b.HEADER_ID--modified on 20151020
                                                                                                                                                   and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and c.ORDER_LINE_ID = b.OE_LINE_ID
       and b.OE_LINE_ID = oola.line_id
       and b.FLAG = 'Y'
       and msi.INVENTORY_ITEM_FLAG = 'Y'
       and b.REAL_HEADER_ID = p_header_id --modified on 20151020
      /*and a.REAL_HEADER_ID = p_header_id*/ --modified on 20151020
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020
  
    cursor cur_req_line(p_subinv varchar2) is
      select --a.PICK_FROM_SUBINVENTORY,
      --a.LOCATOR_ID,
       b.INVENTORY_ITEM_ID,
       msi.segment1,
       b.LINE_NUMBER,
       b.DELIVERY_DETAIL_ID,
       b.QUANTITY,
       --a.SET_ID,
       B.OE_LINE_ID,
       --a.HEADER_ID,
       b.LINE_ID,
       -- a.OE_HEADER_ID,
       B.SOFTWARE_FLAG,
       msi.primary_uom_code,
       c.SOURCES_OF_PRODUCTS,
       --oola.SHIPPING_INSTRUCTIONS,
       b.QTY_DIFF
        from --cux_oe_ship_headers_v   a,
             cux_oe_ship_lines_v   b,
             cux_om_line_interface c,
             mtl_system_items_b    msi /*,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               oe_order_lines_all      oola*/
       where 1 = 1 --a.header_id = b.HEADER_ID
         and msi.inventory_item_id = b.INVENTORY_ITEM_ID
         and msi.organization_id = p_ORGANIZATION_ID
         and c.ORDER_LINE_ID = b.OE_LINE_ID
            --and b.OE_LINE_ID = oola.line_id
         and b.FLAG = 'Y'
         and msi.INVENTORY_ITEM_FLAG = 'Y'
         and b.REAL_HEADER_ID = p_header_id
            --start of modification by bruce on 2015-8-3
         and exists (select 1
                from cux_pick_sub_doc cpd
               where cpd.cux_pick_line_id = b.LINE_ID
                 and cpd.cux_pick_header_id = p_header_id
                 and cpd.pick_subinv = p_subinv)
      --end of modification by bruce on 2015-8-3
      /*order by b.LINE_NUMBER*/
      ;
    form_trigger_failure exception;
    L_RETURN_STATUS           varchar2(2000);
    L_MSG_COUNT               number;
    L_MSG_DATA                varchar2(2000);
    v_L_QOH                   number;
    v_L_RQOH                  number;
    v_L_QR                    number;
    v_L_QS                    number;
    v_L_ATT                   number;
    v_L_ATR                   number;
    v_count                   number;
    v_qty                     number;
    p_line_rows               wsh_util_core.id_tab_type;
    I                         number;
    x_return_status           varchar2(2000);
    x_msg_count               number;
    x_msg_data                varchar2(2000);
    x_out_detail_id           wsh_delivery_details_grp.action_out_rec_type;
    x_new_detail_id           number;
    x_split_quantity          number;
    x_split_quantity2         number;
    v_organization_id         number;
    v_inventory_item_id       number;
    v_requested_quantity_uom  VARCHAR2(2000);
    v_requested_quantity_uom2 VARCHAR2(2000);
    p_commit                  VARCHAR2(30);
    p_validation_level        NUMBER;
    v_delivery_id             number;
    p_tabofdeldets            wsh_delivery_details_pub.id_tab_type;
    x_del_rows                wsh_util_core.id_tab_type;
    p_delivery_id             number;
    v_oe_header_id            number;
    v_released_status         varchar2(100);
    l_batch_info_rec          wsh_picking_batches_pub.Batch_Info_rec;
    v_pick_from_subinventory  varchar2(100);
    v_locator_id              number;
    x_batch_id                number;
    x_request_id              number;
    l_msg_return              NUMBER;
    v_request_number          varchar2(100);
    v_set_id                  number;
    V_PRODUCTS_OU_ID          number;
    V_ORG_ID                  NUMBER;
    v_order_org_id            number;
    V_FLAG                    varchar2(100);
    V_SHIP_FROM_ORG_ID        NUMBER;
    v_pick_from_subinventory1 varchar2(100);
    v_from_subinventory_code  varchar2(100);
    ----------
    L_IFACE_REC          MTL_TRANSACTIONS_INTERFACE%ROWTYPE;
    V_TXN_ID             NUMBER;
    L_RESULT             NUMBER;
    L_TRANS_COUNT        NUMBER;
    V_ERROR_CODE         varchar2(100);
    v_header_id          number;
    retcode              varchar2(100);
    ERRBUF               varchar2(100);
    L_REQUEST_ID         number;
    V_LINE_FLAG          varchar2(100) := 'N';
    v_Attribute13        varchar2(100);
    v_inter_flag         varchar2(10);
    l_count_special_char number;
    l_remark             varchar(1000);
  begin
  
    begin
      select coh.remark
        into l_remark
        from cux.cux_oe_ship_headers coh
       where coh.header_id = p_header_id;
    exception
      when others then
        l_remark := '';
    end;
  
    select instr(l_remark, '<') into l_count_special_char from dual;
    if (l_count_special_char > 0) then
      v_err_txt := '备注存在特殊字符';
      raise form_trigger_failure;
    end if;
    select instr(l_remark, '>') into l_count_special_char from dual;
    if (l_count_special_char > 0) then
      v_err_txt := '备注存在特殊字符';
      raise form_trigger_failure;
    end if;
  
    -- 清楚未勾选的数据
    delete from cux_pick_sub_doc cpd
     where cpd.cux_pick_header_id = p_header_id
       and cpd.cux_pick_line_id in
           (select a.line_id
              from cux.cux_oe_ship_lines a
             where a.header_id = p_header_id
               and nvl(a.flag, 'N') = 'N');
  
    delete from cux.cux_oe_ship_lines a
     where a.header_id = p_header_id
       and nvl(a.flag, 'N') = 'N';
       
    fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK FLAG');
    
    -- 是否有选择数据
    select count(1) 
      into v_count
      from cux.cux_oe_ship_lines b
     where b.HEADER_ID = p_header_id
       and b.FLAG = 'Y';
       
    if v_count = 0 then
      v_err_txt := '未勾选记录';
      raise form_trigger_failure;
    end if;
  
    -- 校验组织架构
    fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK ORG');
    select count(distinct NVL(b.sources_of_products, 'NA'))
      into v_count
      from cux.cux_oe_ship_lines a, 
           cux_om_line_interface b
     where a.oe_line_id = b.order_line_id
       and a.header_id = p_header_id
       and a.flag = 'Y';
       
    if v_count > 1 then
      v_err_txt := '勾选物料存在两个组织的产品';
      raise form_trigger_failure;
    end if;
    
    -- 校验货运属性是否一致
    fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK FREIGHT');
    select count(distinct nvl(b.FREIGHT_ATTRIBUTE, 'NA'))
      into v_count
      from cux.cux_oe_ship_lines a, cux_om_line_interface b
     where a.oe_line_id = b.order_line_id
       and a.header_id = p_header_id
       and a.flag = 'Y';
    if v_count > 1 then
      v_err_txt := '勾选物料存在两个不同货运属性';
      raise form_trigger_failure;
    end if;
    
    -- 校验联系人是否一致
    fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK CONTACT');
    select count(distinct nvl(b.INVOICE_TO_CONTACT_ID, -1))
      into v_count
      from cux.cux_oe_ship_lines a, oe_order_lines_all b
     where a.oe_line_id = b.line_id
       and a.header_id = p_header_id
       and a.flag = 'Y';
    if v_count > 1 then
      v_err_txt := '勾选数据存在两个联系人';
      raise form_trigger_failure;
    end if;
  
    -- 校验收货地址是否一致
    fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK SHIP TO ORG');
    select count(distinct nvl(b.ship_to_org_id, -1))
      into v_count
      from cux.cux_oe_ship_lines a, oe_order_lines_all b
     where a.oe_line_id = b.line_id
       and a.header_id = p_header_id
       and a.flag = 'Y';
    if v_count > 1 then
      v_err_txt := '勾选数据存在两个收货地点';
      raise form_trigger_failure;
    end if;
  
    v_inter_flag := '';
    
    for y in c_2 loop
    
      -- 校验状态  
      fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK STATUS');
      select count(1)
        into v_count
        from wsh_delivery_details wdd
       where wdd.delivery_detail_id = y.delivery_detail_id
         and wdd.released_status in ('R', 'B');
      if v_count = 0 then
        v_err_txt       := y.line_number || '状态不是准备发放或者延交';
        v_error_line_id := y.line_id;
        raise form_trigger_failure;
      end if;
      
      -- 组织
      BEGIN
        SELECT decode(y.SOURCES_OF_PRODUCTS, 'JW', 83, 'FW', '84')
          INTO V_SHIP_FROM_ORG_ID
          FROM dual;
      EXCEPTION
        WHEN OTHERS THEN
          V_SHIP_FROM_ORG_ID := p_ORGANIZATION_ID;
      END;
      
      --行
      fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK ATTACHED LINE');
      SELECT COUNT(1)
        INTO V_COUNT
        FROM cux_om_line_interface_v A
       WHERE A.ORDER_LINE_ID = Y.OE_LINE_ID;
      IF V_COUNT > 1 THEN
        v_err_txt       := y.line_number || '存在两个订单附加行';
        v_error_line_id := y.line_id;
        raise form_trigger_failure;
      END IF;
      
      -- 产品来源
      fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK shipping instruc');
      if (y.shipping_instructions is not null) then
        --modified by bruce on 20150810
        if nvl(y.sources_of_products, 'AA') <> y.shipping_instructions then
          v_err_txt       := y.line_number || '产品来源和发运说明不一致';
          v_error_line_id := y.line_id;
          raise form_trigger_failure;
        end if;
        /*ELSE
        if nvl(y.sources_of_products, 'AA') <> y.shipping_instructions then
          v_err_txt           := y.line_number || '产品来源和发运说明不一致';
          v_error_line_id := y.line_id;
          raise form_trigger_failure;*/
      end if;
    
      fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK packing instruc');
      if (y.packing_instructions is not null) then
        --modified by bruce on 20150810
        if nvl(y.sources_of_products, 'AA') <> y.packing_instructions then
          v_err_txt       := y.line_number || '产品来源和包装说明不一致';
          v_error_line_id := y.line_id;
          raise form_trigger_failure;
        end if;
        /*ELSE
        if nvl(y.sources_of_products, 'AA') <> y.shipping_instructions then
          v_err_txt           := y.line_number || '产品来源和发运说明不一致';
          v_error_line_id := y.line_id;
          raise form_trigger_failure;*/
      end if;
    
      fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK SOURCE');
      select nvl(msi.Attribute13, 'CC')
        into v_Attribute13
        from mtl_system_items_b msi
       where msi.organization_id = 85
         and msi.inventory_item_id = y.inventory_item_id;
      if v_Attribute13 <> nvl(y.sources_of_products, 'AA') then
        v_err_txt       := y.line_number || '产品来源和物料的产品所属组织不一致';
        v_error_line_id := y.line_id;
        raise form_trigger_failure;
      end if;
    
      --判断软件是否有默认发货仓库
      /*      if y.software_flag = 'Y' then
        select count(*)
          into v_count
          from fnd_lookup_values flv
         where flv.lookup_type = 'RJ_WIP_ISSUE_INVENTORY'
           AND FLV.DESCRIPTION = Y.SEGMENT1
           AND FLV.ATTRIBUTE1 = V_SHIP_FROM_ORG_ID
           and flv.language = 'ZHS'
           AND FLV.ATTRIBUTE2 IS NOT NULL;
        if v_count = 0 then
          p_txt := y.line_number || '软件没有发货仓';
          raise form_trigger_failure;
        end if;
        if v_count > 1 then
          p_txt := y.line_number || '软件有多个发货仓';
          raise form_trigger_failure;
        end if;
      end if;*/
    
      --validate inter-company, added by bruce on 20150810
      
      -- 判断是否是三角贸易
      fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK INTER COMPANY');
      if (v_inter_flag is null) then
      
        select decode(H.ORG_ID, 81, 84, 82, 83)
          into v_order_org_id
          from oe_order_headers_all h
         where h.header_id = y.oe_header_id;
        --判断是否是三角贸易
      
        v_shipping_org_code := y.SOURCES_OF_PRODUCTS;
      
        if V_SHIP_FROM_ORG_ID = v_order_org_id then
           v_inter_flag := 'N';
        ELSE
           v_inter_flag := 'Y';
        end if;
      
        fnd_file.PUT_LINE(fnd_file.LOG, 'v_inter_flag:' || v_inter_flag);
      end if;
    
      --判断当前物料是否存在多个出货子库，如果存在，则统一改为151子库
      /* begin
        select cpd.pick_subinv
          into v_current_subinv
          from cux_pick_sub_doc cpd
         where cpd.cux_pick_header_id = p_header_id
           and cpd.cux_pick_line_id = y.line_id;
      exception
        when others then
          fnd_file.PUT_LINE(fnd_file.LOG, 'error when get current subinv');
          v_current_subinv := 'NA';
      end;
      
      select count(1)
        into v_count_subinv
        from cux_pick_sub_doc cpd, cux.cux_oe_ship_lines col
       where cpd.cux_pick_header_id = p_header_id
         and cpd.cux_pick_line_id <> y.line_id
         and cpd.cux_pick_line_id = col.line_id
         and col.inventory_item_id = y.inventory_item_id
         and cpd.pick_subinv <> v_current_subinv;
      
      if (v_count_subinv > 0) then
        fnd_file.PUT_LINE(fnd_file.LOG,
                          'multiple subinv there, so update to 151 subinv');
        update CUX_PICK_SUB_DOC cpd
           set cpd.pick_subinv     = decode(v_shipping_org_code,
                                            'FW',
                                            'G151',
                                            'JW',
                                            'H151'),
               cpd.pick_locator_id = '',
               cpd.attribute10     = 'Y'
         where cpd.cux_pick_header_id = p_header_id
           and cpd.pick_subinv not like '%151'
           and cpd.cux_pick_line_id in
               (select col.line_id
                  from cux.cux_oe_ship_lines col
                 where col.inventory_item_id = y.inventory_item_id
                   and col.header_id = p_header_id);
      end if;*/
      
      -- 子库校验
      fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK subinv exist');
    
      select count(1)
        into v_count
        from cux_pick_sub_doc cpd, mtl_secondary_inventories msi
       where msi.secondary_inventory_name = cpd.pick_subinv
         and msi.organization_id = V_SHIP_FROM_ORG_ID
         and cpd.cux_pick_line_id = y.line_id
         and cpd.cux_pick_header_id = p_header_id;
      if (v_count = 0) then
        v_err_txt       := y.line_number || '子库不存在';
        v_error_line_id := y.line_id;
        raise form_trigger_failure;
      end if;
    
    end loop;
    --验证现有量
    /*for x in c_1 loop
      --if p_ORGANIZATION_ID not in (85,86) then 
      BEGIN
        SELECT decode(x.SOURCES_OF_PRODUCTS, 'JW', 83, 'FW', '84')
          INTO V_SHIP_FROM_ORG_ID
          FROM dual;
      EXCEPTION
        WHEN OTHERS THEN
          V_SHIP_FROM_ORG_ID := p_ORGANIZATION_ID;
      END;
      select decode(V_SHIP_FROM_ORG_ID, 84, 'G151', 'H151')
        into v_pick_from_subinventory1
        from dual;
      if x.software_flag = 'Y' then
        select flv.attribute2
          into v_pick_from_subinventory1
          from fnd_lookup_values flv
         where flv.lookup_type = 'RJ_WIP_ISSUE_INVENTORY'
           AND FLV.DESCRIPTION = x.SEGMENT1
           and flv.language = 'ZHS'
           AND FLV.ATTRIBUTE1 = V_SHIP_FROM_ORG_ID
           AND FLV.ATTRIBUTE2 IS NOT NULL;
      end if;
      V_LINE_FLAG := 'Y';
      inv_quantity_tree_pub.clear_quantity_cache;
      INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                             P_INIT_MSG_LST        => NULL,
                                             X_RETURN_STATUS       => L_RETURN_STATUS,
                                             X_MSG_COUNT           => L_MSG_COUNT,
                                             X_MSG_DATA            => L_MSG_DATA,
                                             P_ORGANIZATION_ID     => V_SHIP_FROM_ORG_ID, --库存组织ID
                                             P_INVENTORY_ITEM_ID   => x.inventory_item_id, --物料ID
                                             P_TREE_MODE           => 3,
                                             P_IS_REVISION_CONTROL => FALSE,
                                             P_IS_LOT_CONTROL      => FALSE,
                                             P_IS_SERIAL_CONTROL   => FALSE,
                                             P_REVISION            => NULL,
                                             P_LOT_NUMBER          => NULL,
                                             P_LOT_EXPIRATION_DATE => NULL,
                                             P_SUBINVENTORY_CODE   => v_pick_from_subinventory1, --子库code
                                             P_LOCATOR_ID          => x.LOCATOR_ID,
                                             P_COST_GROUP_ID       => NULL,
                                             P_ONHAND_SOURCE       => 3,
                                             X_QOH                 => v_L_QOH, --现有量
                                             X_RQOH                => v_L_RQOH,
                                             X_QR                  => v_L_QR,
                                             X_QS                  => v_L_QS,
                                             X_ATT                 => v_L_ATT,
                                             X_ATR                 => v_L_ATR);
      if nvl(v_L_ATT, 0) < x.QUANTITY then
        p_txt := x.segment1 || '-现有量不足' || v_pick_from_subinventory1;
        raise form_trigger_failure;
      end if;
      --end if; 
    end loop;*/
    /*    IF nvl(V_LINE_FLAG, 'N') = 'N' THEN
      p_txt := '未维护订单行属性';
      raise form_trigger_failure;
    END IF;*/
  
    --  for y in c_2 loop
    --if p_ORGANIZATION_ID not in (85,86) then
    /*select decode(col.SOURCES_OF_PRODUCTS, 'FW', 84, 'JW', 83)
      into V_SHIP_FROM_ORG_ID
      from cux_om_line_interface_v col
     where col.ORDER_LINE_ID = y.oe_line_id;
    select decode(V_SHIP_FROM_ORG_ID, 84, 'G151', 'H151')
      into v_pick_from_subinventory1
      from dual;
    if y.software_flag = 'Y' then
      select flv.attribute2
        into v_pick_from_subinventory1
        from fnd_lookup_values flv
       where flv.lookup_type = 'RJ_WIP_ISSUE_INVENTORY'
         AND FLV.DESCRIPTION = y.SEGMENT1
         and flv.language = 'ZHS'
         AND FLV.ATTRIBUTE1 = V_SHIP_FROM_ORG_ID
         AND FLV.ATTRIBUTE2 IS NOT NULL;
    end if;
    
    inv_quantity_tree_pub.clear_quantity_cache;
    INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                           P_INIT_MSG_LST        => NULL,
                                           X_RETURN_STATUS       => L_RETURN_STATUS,
                                           X_MSG_COUNT           => L_MSG_COUNT,
                                           X_MSG_DATA            => L_MSG_DATA,
                                           P_ORGANIZATION_ID     => V_SHIP_FROM_ORG_ID, --库存组织ID
                                           P_INVENTORY_ITEM_ID   => y.inventory_item_id, --物料ID
                                           P_TREE_MODE           => 3,
                                           P_IS_REVISION_CONTROL => FALSE,
                                           P_IS_LOT_CONTROL      => FALSE,
                                           P_IS_SERIAL_CONTROL   => FALSE,
                                           P_REVISION            => NULL,
                                           P_LOT_NUMBER          => NULL,
                                           P_LOT_EXPIRATION_DATE => NULL,
                                           P_SUBINVENTORY_CODE   => v_pick_from_subinventory1, --子库code
                                           P_LOCATOR_ID          => y.LOCATOR_ID,
                                           P_COST_GROUP_ID       => NULL,
                                           P_ONHAND_SOURCE       => 3,
                                           X_QOH                 => v_L_QOH, --现有量
                                           X_RQOH                => v_L_RQOH,
                                           X_QR                  => v_L_QR,
                                           X_QS                  => v_L_QS,
                                           X_ATT                 => v_L_ATT,
                                           X_ATR                 => v_L_ATR);
    if nvl(v_L_ATT, 0) < y.QUANTITY then
      p_txt := y.segment1 || '-现有量不足' || v_pick_from_subinventory1;
      raise form_trigger_failure;
    end if;*/
    -- end if;
    /*  select count(*)
      into v_count
      from wsh_delivery_details wdd
     where wdd.delivery_detail_id = y.delivery_detail_id
       and wdd.released_status in ('R', 'B');
    if v_count = 0 then
      p_txt := y.line_number || '状态不是准备发放或者延交';
      raise form_trigger_failure;
    end if;*/
    /*
    if y.qty_diff is null then
      p_txt := y.segment1 || '发运说明为空';
      raise form_trigger_failure;
    end if;*/
  
    --判断软件是否有默认发货仓库
    /*      if y.software_flag = 'Y' then
      select count(*)
        into v_count
        from fnd_lookup_values flv
       where flv.lookup_type = 'RJ_WIP_ISSUE_INVENTORY'
         AND FLV.DESCRIPTION = Y.SEGMENT1
         and flv.language = 'ZHS'
         AND FLV.ATTRIBUTE1 = p_ORGANIZATION_ID
         AND FLV.ATTRIBUTE2 IS NOT NULL;
      if v_count = 0 then
        p_txt := y.line_number || '软件没有发货仓';
        raise form_trigger_failure;
      end if;
      if v_count > 1 then
        p_txt := y.line_number || '软件有多个发货仓';
        raise form_trigger_failure;
      end if;
    end if;*/
    -- end loop;
    --change software subinv to hardware subinv if not only software
  
    --如果一个发通单里存在多行是同一个物料，且建议子库都是B库，但汇总请求数量大于B库可用量的时候，
    --建议子库自动改为A库，并标记ATTRIBUTE
  
    /* for rec_dup_item in CUR_DUP_ITEM(V_SHIP_FROM_ORG_ID) loop
        fnd_file.PUT_LINE(fnd_file.LOG,
                          'duplicate item:' || rec_dup_item.inventory_item_id);
        begin
        
          select nvl(cpd.avail_qty, 0)
            into v_item_avail_qty
            from cux.cux_oe_ship_lines col, cux_pick_sub_doc cpd
           where col.header_id = p_header_id
             and col.line_id = cpd.cux_pick_line_id
             and col.inventory_item_id = rec_dup_item.inventory_item_id
             and cpd.pick_subinv = rec_dup_item.pick_subinv
             and rownum = 1;
        exception
          when others then
            v_item_avail_qty := 0;
          
        end;
      
        if (rec_dup_item.total_req_qty > v_item_avail_qty) then
          --如果物料在B库的需求总量大于可用量，那么都改为从A库出货
          fnd_file.PUT_LINE(fnd_file.LOG,
                            'total req qty greater avail qty, then fetch A subinv');
          \* begin
            select msi.secondary_inventory_name
              into v_suggest_sub_a
              from mtl_secondary_inventories msi
             where msi.organization_id = V_SHIP_FROM_ORG_ID
               and msi.attribute5 = 'A'
               and ROWNUM = 1;
          exception
            when others then
              select decode(v_shipping_org_code, 'FW', 'G151', 'JW', 'H151')
                into v_suggest_sub_a
                from dual;
          end;*\
        
          select decode(v_shipping_org_code, 'FW', 'G151', 'JW', 'H151')
            into v_suggest_sub_a
            from dual;
        
          UPDATE cux_pick_sub_doc cpd
             set cpd.pick_subinv     = v_suggest_sub_a,
                 cpd.pick_locator_id = '',
                 cpd.attribute10     = 'Y' --ATTRIBUTE10打上标记，查询出发货单时显示黄色
           where cpd.cux_pick_header_id = p_header_id
             and cpd.cux_pick_line_id in
                 (select col.line_id
                    from cux.cux_oe_ship_lines col
                   where col.inventory_item_id =
                         rec_dup_item.inventory_item_id
                     and col.header_id = p_header_id)
             and cpd.pick_subinv = rec_dup_item.pick_subinv;
        
        else
          null;
        end if;
      
      end loop;
    */
    
    -- 是否存在非软件物料
    select count(1)
      INTO v_exist_hardware
      from cux_pick_sub_doc cpd
     where cpd.cux_pick_header_id = p_header_id
       and nvl(cpd.software_flag, 'N') = 'N';
       
    -- 如果存在非软件物料,将所有软件发货子库设置为随机要给非软件子库
    if (v_exist_hardware > 0) then
        fnd_file.PUT_LINE(fnd_file.LOG, 'UPDATE SUBINV' || v_hardware_subinv);
        select cpd.pick_subinv
          INTO v_hardware_subinv
          from cux_pick_sub_doc cpd
         where cpd.cux_pick_header_id = p_header_id
           and nvl(cpd.software_flag, 'N') = 'N'
           and rownum = 1;
      
        update cux_pick_sub_doc cpd
           set cpd.soft_subinv     = cpd.pick_subinv,
               cpd.soft_locator_id = cpd.soft_locator_id,
               cpd.pick_subinv     = v_hardware_subinv
         where cpd.cux_pick_header_id = p_header_id
           and cpd.software_flag = 'Y';
    
    end if;
  
    --start to loop subinv,added on 2015-8-3
    v_error_exist := 'N';
    for rec_subinv in cur_req_subinv loop
      fnd_file.PUT_LINE(fnd_file.LOG,
                        'PROCESS SUBINV:' || REC_SUBINV.PICK_SUBINV);
    
      --start of add by bruce on 20150925  
      v_order_item := null;
      for rec_order_item in cur_order_item(rec_subinv.pick_subinv) loop
      
        if (v_order_item is null) then
        
          v_order_item := '按单重检,按项目发货的物料:' || rec_order_item.segment1;
        else
          v_order_item := v_order_item || ';' || rec_order_item.segment1;
        
        end if;
      
      end loop;
    
      --start of add by bruce on 20151008
      -- 物料备注信息,同步更新到 搬运单中(基于子库)
      v_item_remark := null;
      for rec_item_remark in cur_item_remark(rec_subinv.pick_subinv) loop
          if (v_item_remark is null) then
              v_item_remark := rec_item_remark.remark;
          else 
              v_item_remark := v_item_remark || ';' || rec_item_remark.remark;
          end if;
      end loop;
    
      IF (v_order_item IS NOT NULL or v_item_remark is not null) THEN
      
        BEGIN
          update cux_pick_sub_doc coh
             set coh.comments = trim(substrb(v_item_remark || ' ' ||
                                             v_order_item,
                                             1,
                                             1000))
           where coh.cux_pick_header_id = p_header_id
             and coh.pick_subinv = rec_subinv.pick_subinv;
        EXCEPTION
          WHEN OTHERS THEN
            v_err_txt := rec_subinv.pick_subinv || '更新备注出错' || SQLERRM;
            raise form_trigger_failure;
        END;
      
      END IF;
      -- 更新备注结束
      --end of add by bruce on 20151008  
      --end of add by bruce on 20150925                    
    
      p_line_rows.delete;
      --l_batch_info_rec:='';
      v_error_flag              := 'N';
      v_organization_id         := '';
      v_inventory_item_id       := '';
      v_requested_quantity_uom  := '';
      v_requested_quantity_uom2 := '';
      v_delivery_id             := '';
      p_delivery_id             := '';
      v_oe_header_id            := '';
      v_organization_id         := '';
      v_released_status         := '';
      v_pick_from_subinventory  := '';
      v_locator_id              := '';
      v_set_id                  := '';
      v_org_id                  := '';
      x_return_status           := '';
      x_batch_id                := '';
      v_request_number          := '';
      --p_request_number          := '';
      v_header_id := '';
      --p_flag                    := '';
      --p_txt                     := '';
      L_REQUEST_ID := '';
      ------------准备拆行
      I               := 1;
      v_error_line_id := '';
      
      for y in cur_req_line(rec_subinv.pick_subinv) loop
        fnd_file.PUT_LINE(fnd_file.LOG, 'PROCESS LINE' || Y.LINE_ID);
        fnd_file.PUT_LINE(fnd_file.LOG, 'CHECK QUANTITY');
        begin
          select requested_quantity
            into v_qty
            from wsh_delivery_details
           where delivery_detail_id = y.delivery_detail_id;
        exception
          when others then
            v_qty := 0;
        end;
        if v_qty = 0 then
          v_err_txt       := '获取数量失败';
          v_error_line_id := y.line_id;
          --raise form_trigger_failure;
          v_error_flag  := 'Y';
          v_error_exist := 'Y';
          fnd_file.PUT_LINE(fnd_file.LOG, '获取数量失败');
          goto next_loop;
        end if;
      
        -- 如果发运需求数量大于本次发运量,则需要对系统标准发运进行拆行
        -- 分多次发运
        if v_qty <> y.quantity then
          x_split_quantity := v_qty - y.quantity;
          fnd_file.PUT_LINE(fnd_file.LOG, 'SPLIT LINE');
          select wdd.organization_id,
                 wdd.inventory_item_id,
                 wdd.requested_quantity_uom,
                 wdd.requested_quantity_uom2
            into v_organization_id,
                 v_inventory_item_id,
                 v_requested_quantity_uom,
                 v_requested_quantity_uom2
            from wsh_delivery_details wdd
           where wdd.delivery_detail_id = y.delivery_detail_id;
        
          /*x_split_quantity2 := inv_convert.inv_um_convert(item_id         => v_inventory_item_id,
          lot_number      => null,
          organization_id => v_organization_id,
          precision       => null,
          from_quantity   => x_split_quantity,
          from_unit       => v_requested_quantity_uom,
          to_unit         => v_requested_quantity_uom2,
          from_name       => null,
          to_name         => null);*/
        
          wsh_delivery_details_pub.split_line(p_api_version      => '1.0',
                                              p_init_msg_list    => FND_API.G_TRUE,
                                              p_commit           => p_commit,
                                              p_validation_level => p_validation_level,
                                              x_return_status    => x_return_status,
                                              x_msg_count        => x_msg_count,
                                              x_msg_data         => x_msg_data,
                                              p_from_detail_id   => y.delivery_detail_id,
                                              x_new_detail_id    => x_new_detail_id,
                                              x_split_quantity   => x_split_quantity, -- 剩余数量
                                              x_split_quantity2  => x_split_quantity2);
        
          IF (x_return_status = WSH_UTIL_CORE.G_RET_STS_SUCCESS) THEN
              dbms_output.put_line('S');
          ELSE
              v_err_txt     := '拆行失败';
              v_error_flag  := 'Y';
              v_error_exist := 'Y';
              fnd_file.PUT_LINE(fnd_file.LOG, '拆行失败');
              --raise form_trigger_failure;
              v_error_line_id := y.line_id;
              goto next_loop;
          END IF;
        end if;
        
        p_line_rows(i) := y.delivery_detail_id;
        I := I + 1;
        
        ----判断是否已经产生交货批
        fnd_file.PUT_LINE(fnd_file.LOG, 'UNASSIGN DELIVERY');
        begin
          select distinct delivery_id
            into v_delivery_id
            from wsh_delivery_assignments
           where delivery_detail_id = y.delivery_detail_id;
        exception
          when others then
            v_delivery_id := null;
        end;
        if v_delivery_id is not null then
          P_TABOFDELDETS(1) := y.delivery_detail_id;
          wsh_delivery_details_pub.detail_to_delivery(p_api_version      => 1.0,
                                                      p_init_msg_list    => null,
                                                      p_commit           => null,
                                                      p_validation_level => null,
                                                      x_return_status    => x_return_status,
                                                      x_msg_count        => x_msg_count,
                                                      x_msg_data         => x_msg_data,
                                                      p_tabofdeldets     => P_TABOFDELDETS,
                                                      p_action           => 'UNASSIGN',
                                                      p_delivery_id      => v_delivery_id,
                                                      p_delivery_name    => null);
          if x_return_status <> 'S' then
            v_err_txt       := '取消交货号失败';
            v_error_line_id := y.line_id;
            v_error_flag    := 'Y';
            v_error_exist   := 'Y';
            fnd_file.PUT_LINE(fnd_file.LOG, '取消交货号失败');
            --raise form_trigger_failure;
            goto next_loop;
          end if;
        end if;
      end loop;
      
      ----重新创建交货批
      fnd_file.PUT_LINE(fnd_file.LOG, 'CREATE DELIVERY');
      wsh_delivery_details_pub.autocreate_deliveries(p_api_version_number => '1.0',
                                                     p_init_msg_list      => FND_API.G_TRUE,
                                                     p_commit             => FND_API.G_FALSE,
                                                     x_return_status      => x_return_status,
                                                     x_msg_count          => x_msg_count,
                                                     x_msg_data           => x_msg_data,
                                                     p_line_rows          => p_line_rows,
                                                     x_del_rows           => x_del_rows);
                                                     
      IF (x_return_status = WSH_UTIL_CORE.G_RET_STS_SUCCESS) THEN
        dbms_output.put_line('Return Status = ' ||
                             SUBSTR(x_return_status, 1, 255));
        fnd_file.PUT_LINE(fnd_file.LOG,
                          'Delivery Id = ' || TO_CHAR(x_del_rows(1)));
      ELSE
        v_error_flag := 'Y';
        fnd_file.PUT_LINE(fnd_file.LOG, 'CREATE DELIVERY error1');
        v_err_txt := '创建交货号失败';
        --fnd_file.PUT_LINE(fnd_file.LOG, 'CREATE DELIVERY error2');
        --v_error_line_id:=y.line_id;
        v_error_exist := 'Y';
        fnd_file.PUT_LINE(fnd_file.LOG, '创建交货号失败');
        --raise form_trigger_failure;
        goto next_loop;
      end if;
      
      -----开始创建物料搬运单
      fnd_file.PUT_LINE(fnd_file.LOG, 'CREATE move order');
      p_delivery_id := to_char(x_del_rows(1));
      fnd_file.PUT_LINE(fnd_file.LOG, 'p_Delivery_Id' || p_Delivery_Id);
      begin
        select distinct wdd.source_header_id, wdd.organization_id /*,
                                                                                                                                                                                                                                                                                                                                                                                wdd.released_status*/
          into v_oe_header_id, v_organization_id /*, v_released_status*/
          from wsh_new_deliveries       wnd,
               wsh_delivery_assignments wda,
               wsh_delivery_details     wdd
         where wnd.delivery_id = wda.delivery_id
           and wda.delivery_detail_id = wdd.delivery_detail_id
           and wnd.delivery_id = p_Delivery_Id;
        fnd_file.PUT_LINE(fnd_file.LOG, 'get pick rule');
        fnd_file.PUT_LINE(fnd_file.LOG, 'p_header_id:' || p_header_id);
        select a.pick_grouping_rule_id,
               a.default_stage_subinventory,
               a.existing_rsvs_only_flag,
               a.include_planned_lines,
               a.autocreate_delivery_flag,
               a.autodetail_pr_flag,
               a.allocation_method,
               a.auto_pick_confirm_flag,
               a.autopack_flag,
               a.default_stage_subinventory,
               a.default_stage_locator_id,
               a.ORGANIZATION_ID
          into l_batch_info_rec.pick_grouping_rule_id,
               l_batch_info_rec.Default_Stage_Subinventory,
               l_batch_info_rec.existing_rsvs_only_flag,
               l_batch_info_rec.include_planned_lines,
               l_batch_info_rec.autocreate_delivery_flag,
               l_batch_info_rec.autodetail_pr_flag,
               l_batch_info_rec.allocation_method,
               l_batch_info_rec.auto_pick_confirm_flag,
               l_batch_info_rec.autopack_flag,
               l_batch_info_rec.Default_Stage_Subinventory,
               l_batch_info_rec.Default_Stage_Locator_Id,
               l_batch_info_rec.Organization_Id
          from WSH_PICKING_RULES_V a
         where a.PICKING_RULE_ID = (select b.pick_grouping_rule_id
                                      from cux_oe_ship_headers_v b
                                     where b.header_id = p_header_id
                                       and rownum = 1 --added by bruce on 20150812
                                    );
        fnd_file.PUT_LINE(fnd_file.LOG, 'get customer');
        /* select a.pick_from_subinventory, a.locator_id, a.set_id
         into v_pick_from_subinventory, v_locator_id, v_set_id
         from cux.cux_oe_ship_headers a
        where header_id = p_header_id;*/ --commented by bruce on 20150807
        select c.customer_id,
               C.CUSTOMER_NUMBER,
               H.HEADER_ID,
               H.ORDER_TYPE_ID,
               H.ORDER_NUMBER,
               7,
               H.ORG_ID
          INTO l_batch_info_rec.customer_id,
               l_batch_info_rec.customer_number,
               l_batch_info_rec.order_header_id,
               l_batch_info_rec.order_type_id,
               l_batch_info_rec.order_number,
               l_batch_info_rec.document_set_id,
               V_ORG_ID
          from oe_order_headers_all h, AR_CUSTOMERS C
         where h.header_id = v_oe_header_id
           AND H.SOLD_TO_ORG_ID = C.CUSTOMER_ID;
        /* if v_released_status <> 'B' then
          l_batch_info_rec.backorders_only_flag := 'E';
        else
          l_batch_info_rec.backorders_only_flag := 'O';
        end if;*/
        --START OF MODIFICATION BY BRUCE ON 2015-8-3
        /*if v_locator_id is not null then
          l_batch_info_rec.pick_from_locator_id := v_locator_id; --182;
        end if;*/ --commented by bruce on 20150826
      
        --modified by bruce on 20150810
      
        if (v_inter_flag = 'Y') then
        
          SELECT decode(v_shipping_org_code,
                        'FW',
                        'H' || substr(rec_subinv.pick_subinv, 2),
                        'JW',
                        'G' || substr(rec_subinv.pick_subinv, 2))
            INTO l_batch_info_rec.Pick_From_Subinventory
            FROM DUAL;
        else
          l_batch_info_rec.Pick_From_Subinventory := rec_subinv.pick_subinv;
        end if;
        fnd_file.PUT_LINE(fnd_file.LOG,
                          'UPDATE inter flag id and subinv' || p_header_id || '-' ||
                          rec_subinv.pick_subinv);
        update cux_pick_sub_doc cpd
           set cpd.inter_flag   = v_inter_flag,
               cpd.inter_subinv = l_batch_info_rec.Pick_From_Subinventory
         where cpd.cux_pick_header_id = p_header_id
           AND cpd.pick_subinv = rec_subinv.pick_subinv;
      
        --v_pick_from_subinventory;
        --l_batch_info_rec.pick_from_locator_id := v_locator_id;
        --END OF MODIFICATION BY BRUCE ON 2015
        /*      l_batch_info_rec.Ship_Set_Id        := v_set_id;*/
        l_batch_info_rec.Delivery_Id := p_Delivery_Id;
      
        l_batch_info_rec.To_Requested_Date      := NULL;
        l_batch_info_rec.To_Scheduled_Ship_Date := NULL;
        fnd_file.PUT_LINE(fnd_file.LOG, 'CREATE BATCH');
        wsh_picking_batches_pub.Create_Batch(p_api_version   => '1.0',
                                             x_return_status => x_return_status,
                                             x_msg_count     => x_msg_count,
                                             x_msg_data      => x_msg_data,
                                             p_batch_rec     => l_batch_info_rec,
                                             x_batch_id      => x_batch_id);
      
        dbms_output.put_line(x_return_status);
        dbms_output.put_line(x_batch_id);
        IF (x_return_status = WSH_UTIL_CORE.G_RET_STS_SUCCESS) THEN
          fnd_file.PUT_LINE(fnd_file.LOG, 'RELEASE BATCH');
          wsh_picking_batches_pub.Release_Batch(p_api_version   => '1.0',
                                                x_return_status => x_return_status,
                                                x_msg_count     => x_msg_count,
                                                x_msg_data      => x_msg_data,
                                                p_batch_id      => x_batch_id,
                                                p_release_mode  => 'ONLINE',
                                                x_request_id    => x_request_id);
        
          IF (x_return_status = WSH_UTIL_CORE.G_RET_STS_SUCCESS) THEN
            --dbms_output.put_line('S');
            NULL;
          else
            fnd_file.PUT_LINE(fnd_file.LOG,
                              '创建物料搬运单失败2' || x_msg_data);
            v_err_txt     := '创建物料搬运单失败2' || x_msg_data;
            v_error_flag  := 'Y';
            v_error_exist := 'Y';
            --raise form_trigger_failure;
            goto next_loop;
          end if;
        else
          fnd_file.PUT_LINE(fnd_file.LOG,
                            '创建物料搬运单失败1' || x_msg_data);
          v_err_txt     := '创建物料搬运单失败1' || x_msg_data;
          v_error_flag  := 'Y';
          v_error_exist := 'Y';
          --raise form_trigger_failure;
          goto next_loop;
        end if;
      end;
      fnd_file.PUT_LINE(fnd_file.LOG, 'GET MTL REQ HEADER');
      begin
        SELECT distinct MTH.REQUEST_NUMBER, mth.header_id
          into v_request_number, v_header_id
          FROM WSH_DELIVERABLES_V      WDD,
               MTL_TXN_REQUEST_HEADERS MTH,
               MTL_TXN_REQUEST_LINES   MTL
         WHERE MTH.HEADER_ID = MTL.HEADER_ID
           AND WDD.MOVE_ORDER_LINE_ID = MTL.LINE_ID
           AND WDD.delivery_id = p_delivery_id;
      exception
        when others then
          v_request_number := null;
      end;
      if v_request_number is null then
        v_err_txt     := '获取搬运单号失败';
        v_error_flag  := 'Y';
        v_error_exist := 'Y';
        fnd_file.PUT_LINE(fnd_file.LOG, '获取搬运单号失败');
        --raise form_trigger_failure;
        goto next_loop;
      end if;
      --p_request_number := v_request_number;
      -----------------解决客制物料搬运单编号的问题----------------------
      fnd_file.PUT_LINE(fnd_file.LOG, 'UPDATE REQ NUMBER');
      UPDATE wsh_picking_batches batch
         SET batch.attribute1 = batch.NAME, batch.NAME = v_request_number
       WHERE batch.batch_id = x_batch_id;
      ------------------------------------------------------------------     
      v_process_flag := 'S';
    
      --start of modification by bruce on 2015-8-3
      fnd_file.PUT_LINE(fnd_file.LOG, 'UPDATE cux_oe_ship_headers');
      update cux.cux_oe_ship_headers a
         set --a.request_number = v_request_number,--modified on 20151020
             a.attribute2 = p_delivery_id
       where a.header_id = p_header_id;
    
      /*fnd_file.PUT_LINE(fnd_file.LOG, 'UPDATE CUX SUB DOC');
      update cux_pick_sub_doc cpd
         set cpd.request_number = v_request_number,
             cpd.req_header_id  = v_header_id,
             cpd.delivery_id    = p_delivery_id
       where cpd.cux_pick_header_id = p_header_id
         and cpd.pick_subinv = rec_subinv.pick_subinv;*/
      --end of modification by bruce on 2015-8-3
    
      for x in (select mtl.line_number,
                       mtl.txn_source_line_detail_id,
                       mtl.line_id,
                       mtl.header_id,
                       mtl.inventory_item_id,
                       msi.segment1,
                       mtl.Quantity,
                       nvl(msi.attribute12, 'N') SOFTWARE_FLAG
                  from MTL_TXN_REQUEST_HEADERS MTH,
                       MTL_TXN_REQUEST_LINES   MTL,
                       mtl_system_items_b      msi
                 where mth.header_id = mtl.header_id
                   and mtl.inventory_item_id = msi.inventory_item_id
                   and mtl.organization_id = msi.organization_id
                   and mth.request_number = v_request_number) loop
        update cux.cux_oe_ship_lines b
           set b.attribute10 = x.line_number, b.attribute11 = x.line_id
         where b.header_id = p_header_id
           and b.inventory_item_id = x.inventory_item_id
           and b.delivery_detail_id = x.Txn_Source_Line_Detail_Id;
        --修改软件的发货仓库
        if x.SOFTWARE_FLAG = 'Y' then
          /*  select FLV.ATTRIBUTE2
           into v_from_subinventory_code
           from fnd_lookup_values flv
          where flv.lookup_type = 'RJ_WIP_ISSUE_INVENTORY'
            AND FLV.DESCRIPTION = x.SEGMENT1
            AND FLV.ATTRIBUTE1 = p_ORGANIZATION_ID
            and flv.language = 'ZHS'
            AND FLV.ATTRIBUTE2 IS NOT NULL;*/
          update MTL_TXN_REQUEST_LINES mtrl
             set mtrl.from_subinventory_code = decode(p_ORGANIZATION_ID,
                                                      84,
                                                      'G999',
                                                      83,
                                                      'H999') -- v_from_subinventory_code
           where mtrl.line_id = x.line_id;
        
        else
          --added by bruce on 2015-8-3
          if (v_inter_flag = 'N') then
          
            fnd_file.PUT_LINE(fnd_file.LOG, 'UPDATE LOCATOR');
            BEGIN
              merge into MTL_TXN_REQUEST_LINES mrl
              using (select nvl(col.attribute11, 0) mtl_line_id,
                            cpd.pick_locator_id
                       from cux_pick_sub_doc cpd, cux.cux_oe_ship_lines col
                      where cpd.cux_pick_line_id = col.line_id
                        and cpd.cux_pick_header_id = p_header_id
                        and cpd.pick_subinv = rec_subinv.pick_subinv
                        and col.header_id = p_header_id) cux_pick_line
              on (mrl.HEADER_ID = x.header_id and cux_pick_line.mtl_line_id = mrl.line_id)
              when matched then
                update
                   set mrl.from_locator_id = cux_pick_line.pick_locator_id;
            EXCEPTION
              WHEN OTHERS THEN
                fnd_file.PUT_LINE(fnd_file.LOG, '更新货位出错' || sqlerrm);
                v_err_txt     := '更新货位出错' || sqlerrm;
                v_error_flag  := 'Y';
                v_error_exist := 'Y';
                goto next_loop;
            END;
          
          end if;
        
          --added by bruce on 2015-8-3
        end if;
      
      end loop;
    
      --added by bruce on 20151116
      fnd_file.PUT_LINE(fnd_file.LOG, 'UPDATE CUX SUB DOC');
      update cux_pick_sub_doc cpd
         set cpd.request_number = v_request_number,
             cpd.req_header_id  = v_header_id,
             cpd.delivery_id    = p_delivery_id
       where cpd.cux_pick_header_id = p_header_id
         and cpd.pick_subinv = rec_subinv.pick_subinv
         and cpd.cux_pick_line_id in
             (select COL.LINE_ID
                from cux.cux_oe_ship_lines col, mtl_txn_request_lines mtl
               where col.header_id = p_header_id
                 and mtl.header_id = v_header_id
                 and col.attribute11 = MTL.LINE_ID);
      --end of add by bruce on 20151116
    
      /*if   p_flag ='S'then
        for z in c_2 loop
         select decode(col.SOURCES_OF_PRODUCTS,'FW',81,'JW',82)
           into V_PRODUCTS_OU_ID
           from cux_om_line_interface_v col
          where col.ORDER_LINE_ID=z.oe_line_id;
         if  V_PRODUCTS_OU_ID <>V_ORG_ID then
           --子库转移
               SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
                 INTO V_TXN_ID
                 FROM DUAL;
              if p_ORGANIZATION_ID =84 then    
             
               L_IFACE_REC.TRANSACTION_INTERFACE_ID := V_TXN_ID;
               L_IFACE_REC.TRANSACTION_MODE         := 3; --BACKGROUND
               L_IFACE_REC.PROCESS_FLAG             := 1; --TO BE PROCESSED    
               L_IFACE_REC.ORGANIZATION_ID          := 83;
               L_IFACE_REC.Transfer_Organization    := 84;
               L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID  := NULL \*ZG_GT_CSN_RLS_TL_P.GET_INV_MATERIAL_ACCID(MTL_ORG_ID)*\
                ;
               L_IFACE_REC.INVENTORY_ITEM_ID        := z.INVENTORY_ITEM_ID;
               L_IFACE_REC.SUBINVENTORY_CODE        := 'H151'; --一定要给；如果ITEM限制了子库，那么必须给这个ITEM的子库
               L_IFACE_REC.LOCATOR_ID               := null;
               L_IFACE_REC.TRANSACTION_QUANTITY     := -z.quantity;
               L_IFACE_REC.PRIMARY_QUANTITY         := -z.quantity;
               L_IFACE_REC.TRANSACTION_UOM          := z.primary_uom_code;
               L_IFACE_REC.TRANSACTION_DATE         := sysdate;
               L_IFACE_REC.Transfer_Subinventory    := 'G151';
               L_IFACE_REC.TRANSFER_LOCATOR         := null;
               L_IFACE_REC.TRANSACTION_SOURCE_NAME  := NULL;
               L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID  := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
               L_IFACE_REC.SOURCE_CODE              := '三角贸易公司间转移';
               L_IFACE_REC.transaction_reference    := '三角贸易公司间转移,单号:' ||v_request_number;
               L_IFACE_REC.SOURCE_HEADER_ID           := z.HEADER_ID; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
               L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
               L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 28; --子库 转移
               L_IFACE_REC.TRANSACTION_TYPE_ID        := 3; --子库 转移
               --L_IFACE_REC.TRANSACTION_SOURCE_ID      := X.DISPOSITIONS_ID;
               L_IFACE_REC.LAST_UPDATE_DATE      := SYSDATE;
               L_IFACE_REC.LAST_UPDATED_BY       := FND_GLOBAL.USER_ID;
               L_IFACE_REC.CREATION_DATE         := SYSDATE;
               L_IFACE_REC.CREATED_BY            := FND_GLOBAL.USER_ID;
               L_IFACE_REC.LAST_UPDATE_LOGIN     := FND_GLOBAL.LOGIN_ID;
               L_IFACE_REC.TRANSACTION_HEADER_ID := V_TXN_ID;
               L_IFACE_REC.ATTRIBUTE1            := Z.OE_HEADER_ID;
               L_IFACE_REC.ATTRIBUTE2            := Z.Oe_Line_Id;
               L_IFACE_REC.ATTRIBUTE3            := 'Y'; 
               L_IFACE_REC.ATTRIBUTE4            := p_delivery_id;  
            else        
               L_IFACE_REC.TRANSACTION_INTERFACE_ID := V_TXN_ID;
               L_IFACE_REC.TRANSACTION_MODE         := 3; --BACKGROUND
               L_IFACE_REC.PROCESS_FLAG             := 1; --TO BE PROCESSED    
               L_IFACE_REC.ORGANIZATION_ID          := 84;
               L_IFACE_REC.Transfer_Organization    := 83;
               L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID  := NULL \*ZG_GT_CSN_RLS_TL_P.GET_INV_MATERIAL_ACCID(MTL_ORG_ID)*\
                ;
               L_IFACE_REC.INVENTORY_ITEM_ID        := z.INVENTORY_ITEM_ID;
               L_IFACE_REC.SUBINVENTORY_CODE        := 'G151'; --一定要给；如果ITEM限制了子库，那么必须给这个ITEM的子库
               L_IFACE_REC.LOCATOR_ID               := null;
               L_IFACE_REC.TRANSACTION_QUANTITY     := -z.quantity;
               L_IFACE_REC.PRIMARY_QUANTITY         := -z.quantity;
               L_IFACE_REC.TRANSACTION_UOM          := z.primary_uom_code;
               L_IFACE_REC.TRANSACTION_DATE         := sysdate;
               L_IFACE_REC.Transfer_Subinventory    := 'H151';
               L_IFACE_REC.TRANSFER_LOCATOR         := null;
               L_IFACE_REC.TRANSACTION_SOURCE_NAME  := NULL;
               L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID  := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
               L_IFACE_REC.SOURCE_CODE              := '三角贸易公司间转移';
               L_IFACE_REC.transaction_reference    := '三角贸易公司间转移,单号:' ||v_request_number;
               L_IFACE_REC.SOURCE_HEADER_ID           := z.HEADER_ID; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
               L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
               L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 28; --子库 转移
               L_IFACE_REC.TRANSACTION_TYPE_ID        := 3; --子库 转移
               --L_IFACE_REC.TRANSACTION_SOURCE_ID      := X.DISPOSITIONS_ID;
               L_IFACE_REC.LAST_UPDATE_DATE      := SYSDATE;
               L_IFACE_REC.LAST_UPDATED_BY       := FND_GLOBAL.USER_ID;
               L_IFACE_REC.CREATION_DATE         := SYSDATE;
               L_IFACE_REC.CREATED_BY            := FND_GLOBAL.USER_ID;
               L_IFACE_REC.LAST_UPDATE_LOGIN     := FND_GLOBAL.LOGIN_ID;
               L_IFACE_REC.TRANSACTION_HEADER_ID := V_TXN_ID;
               L_IFACE_REC.ATTRIBUTE1            := Z.OE_HEADER_ID;
               L_IFACE_REC.ATTRIBUTE2            := Z.Oe_Line_Id;
               L_IFACE_REC.ATTRIBUTE3            := 'Y';
               L_IFACE_REC.ATTRIBUTE4            := p_delivery_id;     
            end if;   
               INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
          --处理接口表
               L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                                    P_INIT_MSG_LIST    => 'F',
                                                                    P_VALIDATION_LEVEL => 100,
                                                                    P_COMMIT           => 'T',
                                                                    X_RETURN_STATUS    => L_RETURN_STATUS,
                                                                    X_MSG_COUNT        => L_MSG_COUNT,
                                                                    X_MSG_DATA         => L_MSG_DATA,
                                                                    X_TRANS_COUNT      => L_TRANS_COUNT,
                                                                    P_HEADER_ID        => V_TXN_ID);
               IF L_RESULT = 0 THEN
                 UPDATE CUX.CUX_OE_SHIP_LINES B
                    SET B.TRANSACTION_ID = V_TXN_ID
                  WHERE B.LINE_ID = z.LINE_ID;
               ELSE
                 FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                             FROM MTL_TRANSACTIONS_INTERFACE
                            WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
                   V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                                   I.ERROR_EXPLANATION;                    
                 END LOOP;
                 p_txt:='公司间转移失败:'||V_ERROR_CODE;   
              end if;  
         end if;       
        end loop;  
      end  if;*/
    
      --check if all lines have move order line linked
      --added by bruce on 20151116
      select count(1)
        into v_count_mv_line
        from cux.cux_oe_ship_lines col, cux.cux_pick_sub_doc cpd
       where col.header_id = p_header_id
         and col.header_id = cpd.cux_pick_header_id
         and col.line_id = cpd.cux_pick_line_id
         and cpd.pick_subinv = rec_subinv.pick_subinv
         and col.attribute11 is null;
    
      if (v_count_mv_line > 0) then
        fnd_file.PUT_LINE(fnd_file.LOG, 'v_count_mv_line > 0');
        v_error_exist  := 'Y';
        v_process_flag := 'E';
        update cux_pick_sub_doc cpd
           set cpd.process_code = 'E', cpd.error_msg = '搬运单行未产生'
         where cpd.cux_pick_header_id = p_header_id
           and cpd.cux_pick_line_id in
               (select col.line_id
                  from cux.cux_oe_ship_lines col
                 where col.header_id = p_header_id
                   and col.attribute11 is null)
           and cpd.pick_subinv = rec_subinv.pick_subinv
           and cpd.process_code = 'P';
      end if;
      --end of added by bruce on 20151116
      -----抛TMS
      if v_process_flag = 'S' then
        -- Call the procedure
        FND_GLOBAL.APPS_INITIALIZE(fnd_global.USER_ID,
                                   fnd_global.RESP_ID,
                                   fnd_global.RESP_APPL_ID);
        L_REQUEST_ID := APPS.FND_REQUEST.SUBMIT_REQUEST('CUX',
                                                        'CUX_TMS_REPORT',
                                                        NULL,
                                                        NULL,
                                                        FALSE,
                                                        p_ORGANIZATION_ID,
                                                        v_header_id,
                                                        CHR(0),
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '',
                                                        '');
        --p_txt        := L_REQUEST_ID;
      end if;
      commit;
    
      <<next_loop>>
      fnd_file.PUT_LINE(fnd_file.LOG, 'UPDATE SUB DOC END');
      if (v_error_flag = 'Y') then
      
        update cux_pick_sub_doc cpd
           set cpd.process_code = 'E', cpd.error_msg = v_err_txt
         where cpd.cux_pick_header_id = p_header_id
           and cpd.cux_pick_line_id =
               nvl(v_error_line_id, cpd.cux_pick_line_id)
           and cpd.pick_subinv = rec_subinv.pick_subinv
           and cpd.process_code = 'P';
      
      else
        update cux_pick_sub_doc cpd
           set cpd.process_code = 'S'
         where cpd.cux_pick_header_id = p_header_id
           and cpd.pick_subinv = rec_subinv.pick_subinv
           and cpd.process_code = 'P';
      
      end if;
    
    end loop; --loop subinv
    fnd_file.PUT_LINE(fnd_file.LOG, 'print output');
    print_error_proc(p_header_id => p_header_id);
    fnd_file.PUT_LINE(fnd_file.LOG, 'v_error_exist' || v_error_exist);
    if (v_error_exist = 'N') then
      v_process_flag := 'S';
      x_retcode      := 0;
    ELSE
      v_process_flag := 'E';
      x_retcode      := -2;
    end if;
  
    commit;
  exception
    when form_trigger_failure then
      fnd_file.PUT_LINE(fnd_file.LOG, 'form_trigger_failure');
      v_process_flag := 'E';
      x_retcode      := -2;
      update cux_pick_sub_doc cpd
         set cpd.process_code = 'E', cpd.error_msg = v_err_txt
       where cpd.cux_pick_header_id = p_header_id
         and cpd.cux_pick_line_id =
             nvl(v_error_line_id, cpd.cux_pick_line_id)
         and cpd.process_code = 'P';
      commit;
      print_error_proc(p_header_id => p_header_id);
    when others then
      v_process_flag := 'E';
      x_retcode      := -2;
      fnd_file.PUT_LINE(fnd_file.LOG, 'Other error:' || sqlerrm);
  end;

  procedure main_new(p_header_id       in number,
                     p_ORGANIZATION_ID in number,
                     p_flag            in out varchar2,
                     p_txt             in out varchar2,
                     p_request_number  in out varchar2) as
    cursor c_1 is
      select cpd.pick_subinv PICK_FROM_SUBINVENTORY,
             cpd.pick_locator_id LOCATOR_ID,
             b.INVENTORY_ITEM_ID,
             c.SOURCES_OF_PRODUCTS,
             msi.segment1,
             b.software_flag,
             sum(b.ISSUE_QUANTITY) QUANTITY
        from --cux_oe_ship_headers_v   a,
             cux_pick_sub_doc        cpd, --added by bruce on 2015-8-3
             cux_oe_ship_lines_v     b,
             cux_om_line_interface_v c,
             mtl_system_items_b      msi
       where cpd.cux_pick_header_id = b.HEADER_ID
         and cpd.cux_pick_line_id = b.LINE_ID
         and msi.inventory_item_id = b.INVENTORY_ITEM_ID
         and msi.organization_id = p_ORGANIZATION_ID
         and c.ORDER_LINE_ID = b.OE_LINE_ID
         and b.FLAG = 'Y'
         and msi.INVENTORY_ITEM_FLAG = 'Y'
         and cpd.req_header_id = p_header_id --modified by bruce on 2015-8-3
       group by cpd.pick_subinv,
                cpd.pick_locator_id,
                msi.segment1,
                c.SOURCES_OF_PRODUCTS,
                b.software_flag,
                b.INVENTORY_ITEM_ID;
  
    cursor c_2 is
      select /*a.PICK_FROM_SUBINVENTORY,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             a.LOCATOR_ID,*/
       cpd.pick_subinv PICK_FROM_SUBINVENTORY,
       cpd.pick_locator_id LOCATOR_ID,
       b.INVENTORY_ITEM_ID,
       msi.segment1,
       b.LINE_NUMBER,
       b.DELIVERY_DETAIL_ID,
       b.ISSUE_QUANTITY QUANTITY,
       a.SET_ID,
       B.OE_LINE_ID,
       a.HEADER_ID,
       b.LINE_ID,
       a.OE_HEADER_ID,
       a.REQUEST_NUMBER,
       a.attribute2,
       NVL(B.Attribute13, 'N') Attribute13,
       b.ATTRIBUTE11, --搬运单行ID
       b.software_flag,
       msi.primary_uom_code,
       sysdate SHIP_DATE
      /*TO_DATE('2014-09-30','YYYY-MM-DD')SHIP_DATE*/
        from cux_oe_ship_headers_v a,
             cux_oe_ship_lines_v   b,
             cux_pick_sub_doc      cpd, --added by bruce on 2015-8-3
             mtl_system_items_b    msi
       where a.header_id = b.HEADER_ID
         and cpd.cux_pick_header_id = b.HEADER_ID
         and cpd.cux_pick_line_id = b.LINE_ID
         and cpd.req_header_id = p_header_id
         and msi.inventory_item_id = b.INVENTORY_ITEM_ID
         and msi.organization_id = p_ORGANIZATION_ID
         and b.FLAG = 'Y'
         and b.ISSUE_QUANTITY > 0
         and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
       order by b.LINE_NUMBER;
  
    cursor c_21 is
      select cpd.pick_subinv PICK_FROM_SUBINVENTORY,
             cpd.pick_locator_id LOCATOR_ID,
             b.INVENTORY_ITEM_ID,
             msi.segment1,
             b.LINE_NUMBER,
             b.DELIVERY_DETAIL_ID,
             b.ISSUE_QUANTITY QUANTITY,
             a.SET_ID,
             B.OE_LINE_ID,
             a.HEADER_ID,
             b.LINE_ID,
             a.OE_HEADER_ID,
             a.REQUEST_NUMBER,
             a.attribute2,
             NVL(B.Attribute13, 'N') Attribute13,
             b.ATTRIBUTE11, --搬运单ID
             b.software_flag,
             msi.primary_uom_code
        from cux_oe_ship_headers_v a,
             cux_oe_ship_lines_v   b,
             cux_pick_sub_doc      cpd, --added by bruce on 2015-8-3
             mtl_system_items_b    msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
       and msi.INVENTORY_ITEM_FLAG = 'Y'
       and cpd.req_header_id = p_header_id
       and cpd.cux_pick_line_id = b.LINE_ID
       and b.HEADER_ID = a.HEADER_ID
      /*and a.header_id = p_header_id*/
       order by b.LINE_NUMBER;
  
    cursor c_22 is
      select cpd.pick_subinv PICK_FROM_SUBINVENTORY,
             cpd.pick_locator_id LOCATOR_ID,
             b.INVENTORY_ITEM_ID,
             msi.segment1,
             b.LINE_NUMBER,
             b.DELIVERY_DETAIL_ID,
             b.ISSUE_QUANTITY QUANTITY,
             a.SET_ID,
             B.OE_LINE_ID,
             a.HEADER_ID,
             b.LINE_ID,
             a.OE_HEADER_ID,
             a.REQUEST_NUMBER,
             a.attribute2,
             NVL(B.Attribute13, 'N') Attribute13,
             b.ATTRIBUTE11, --搬运单ID
             b.software_flag,
             msi.primary_uom_code
        from cux_oe_ship_headers_v a,
             cux_oe_ship_lines_v   b,
             cux_pick_sub_doc      cpd, --added by bruce on 2015-8-3
             mtl_system_items_b    msi
       where a.header_id = b.HEADER_ID
         and msi.inventory_item_id = b.INVENTORY_ITEM_ID
         and msi.organization_id = p_ORGANIZATION_ID
         and b.FLAG = 'Y'
         and b.ISSUE_QUANTITY = 0
         and msi.INVENTORY_ITEM_FLAG = 'Y'
         and cpd.req_header_id = p_header_id
         and cpd.cux_pick_line_id = b.LINE_ID
         and b.HEADER_ID = a.HEADER_ID
      /*and a.header_id = p_header_id*/
       order by b.LINE_NUMBER;
    v_count number;
    form_trigger_failure exception;
    L_RETURN_STATUS           varchar2(2000);
    L_MSG_COUNT               number;
    L_MSG_DATA                varchar2(2000);
    v_L_QOH                   number;
    v_L_RQOH                  number;
    v_L_QR                    number;
    v_L_QS                    number;
    v_L_ATT                   number;
    v_L_ATR                   number;
    V_SHIP_FROM_ORG_ID        number;
    v_pick_from_subinventory1 varchar2(2000);
    V_ORG_ID                  number;
    V_PRODUCTS_OU_ID          number;
    L_IFACE_REC               MTL_TRANSACTIONS_INTERFACE%ROWTYPE;
    V_TXN_ID                  NUMBER;
    L_RESULT                  NUMBER;
    L_TRANS_COUNT             NUMBER;
    V_ERROR_CODE              varchar2(100);
    V_UNIT_SELLING_PRICE      NUMBER;
    V_TRANSACTIONAL_CURR_CODE varchar2(100);
    v_conversion_rate         NUMBER;
    V_FLAG                    varchar2(100);
    V_TAX_CODE                varchar2(100);
    v_PERCENTAGE_RATE         NUMBER;
    --------------------------------
    v_line_tbl   inv_move_order_pub.trolin_tbl_type;
    l_trolin_tbl inv_move_order_pub.trolin_tbl_type;
    l_trolin_rec inv_move_order_pub.trolin_rec_type;
    l_rsr_type   inv_reservation_global.mtl_reservation_tbl_type;
    l_mold_tbl   inv_mo_line_detail_util.g_mmtt_tbl_type;
    l_api_name       CONSTANT VARCHAR2(30) := 'allocate_move_order';
    l_savepoint_name CONSTANT VARCHAR2(30) := 'allocate_move_order';
    v_return_status         VARCHAR2(30);
    v_msg_count             NUMBER;
    v_msg_data              VARCHAR2(4000);
    v_msg_index_out         NUMBER;
    v_msg_data2             VARCHAR2(2000);
    v_qty_detailed          NUMBER;
    v_qty_delivered         NUMBER;
    i                       number;
    v_return                NUMBER;
    v_REQUEST_NUMBER        VARCHAR2(2000);
    L_API_VERSION           NUMBER := 1.0;
    L_INIT_MSG_LIST         VARCHAR2(2) := FND_API.G_TRUE;
    L_RETURN_VALUES         VARCHAR2(2) := FND_API.G_FALSE;
    L_COMMIT                VARCHAR2(2) := FND_API.G_FALSE;
    L_MOVE_ORDER_TYPE       NUMBER := 3;
    X_MOLD_TBL              INV_MO_LINE_DETAIL_UTIL.G_MMTT_TBL_TYPE;
    X_TROLIN_TBL            INV_MOVE_ORDER_PUB.TROLIN_TBL_TYPE;
    L_MSG_RETURN            NUMBER;
    X_NUMBER_OF_ROWS        NUMBER;
    X_DETAILED_QTY          number;
    X_REVISION              VARCHAR2(20);
    X_LOCATOR_ID            NUMBER;
    X_TRANSFER_TO_LOCATION  NUMBER;
    X_LOT_NUMBER            VARCHAR2(80);
    X_EXPIRATION_DATE       DATE;
    X_TRANSACTION_TEMP_ID   NUMBER;
    P_TRANSACTION_HEADER_ID NUMBER;
    P_TRANSACTION_MODE      NUMBER;
    P_MOVE_ORDER_TYPE       NUMBER := 3;
    v_qty_yj                NUMBER;
  begin
  
    select count(*)
      into v_count
      from cux_oe_ship_lines_v b
     where b.HEADER_ID = p_header_id
       and b.FLAG = 'Y';
    if v_count = 0 then
      p_txt := '未勾选记录';
      raise form_trigger_failure;
    end if;
  
    for x in c_1 loop
      --if p_ORGANIZATION_ID not in (85,86) then 
      BEGIN
        SELECT decode(x.SOURCES_OF_PRODUCTS, 'JW', 83, 'FW', '84')
          INTO V_SHIP_FROM_ORG_ID
          FROM dual;
      EXCEPTION
        WHEN OTHERS THEN
          V_SHIP_FROM_ORG_ID := p_ORGANIZATION_ID;
      END;
      /*  select decode(V_SHIP_FROM_ORG_ID, 84, 'G151', 'H151')
      into v_pick_from_subinventory1
      from dual;*/ --commented by bruce on 2015-8-3
      v_pick_from_subinventory1 := x.pick_from_subinventory;
      if x.software_flag = 'Y' then
        /*        select flv.attribute2
         into v_pick_from_subinventory1
         from fnd_lookup_values flv
        where flv.lookup_type = 'RJ_WIP_ISSUE_INVENTORY'
          AND FLV.DESCRIPTION = x.SEGMENT1
          and flv.language = 'ZHS'
          AND FLV.ATTRIBUTE1 = V_SHIP_FROM_ORG_ID
          AND FLV.ATTRIBUTE2 IS NOT NULL;*/
        select decode(V_SHIP_FROM_ORG_ID, 84, 'G999', 'H999')
          into v_pick_from_subinventory1
          from dual;
        /* else
        select decode(V_SHIP_FROM_ORG_ID, 84, 'G999', 'H999')
         into v_pick_from_subinventory1
         from dual;    */
      end if;
    
      inv_quantity_tree_pub.clear_quantity_cache;
      INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                             P_INIT_MSG_LST        => NULL,
                                             X_RETURN_STATUS       => L_RETURN_STATUS,
                                             X_MSG_COUNT           => L_MSG_COUNT,
                                             X_MSG_DATA            => L_MSG_DATA,
                                             P_ORGANIZATION_ID     => V_SHIP_FROM_ORG_ID, --库存组织ID
                                             P_INVENTORY_ITEM_ID   => x.inventory_item_id, --物料ID
                                             P_TREE_MODE           => 3,
                                             P_IS_REVISION_CONTROL => FALSE,
                                             P_IS_LOT_CONTROL      => FALSE,
                                             P_IS_SERIAL_CONTROL   => FALSE,
                                             P_REVISION            => NULL,
                                             P_LOT_NUMBER          => NULL,
                                             P_LOT_EXPIRATION_DATE => NULL,
                                             P_SUBINVENTORY_CODE   => v_pick_from_subinventory1, --子库code
                                             P_LOCATOR_ID          => x.LOCATOR_ID,
                                             P_COST_GROUP_ID       => NULL,
                                             P_ONHAND_SOURCE       => 3,
                                             X_QOH                 => v_L_QOH, --现有量
                                             X_RQOH                => v_L_RQOH,
                                             X_QR                  => v_L_QR,
                                             X_QS                  => v_L_QS,
                                             X_ATT                 => v_L_ATT,
                                             X_ATR                 => v_L_ATR);
      if nvl(v_L_ATT, 0) < x.QUANTITY and p_header_id <> 3427 then
        p_txt := x.segment1 || '-现有量不足';
        raise form_trigger_failure;
      end if;
      --end if; 
    end loop;
    for y in c_21 loop
      if y.QUANTITY < 0 then
        p_txt := y.segment1 || '-处理量小于零';
        raise form_trigger_failure;
      end if;
    end loop;
  
    ---判断硬件数量是否大于零
    select sum(b.ISSUE_QUANTITY) QUANTITY
      into v_qty_yj
      from cux_oe_ship_headers_v   a,
           cux_oe_ship_lines_v     b,
           cux_om_line_interface_v c,
           mtl_system_items_b      msi
     where a.header_id = b.HEADER_ID
       and msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and c.ORDER_LINE_ID = b.OE_LINE_ID
       and b.FLAG = 'Y'
       and msi.INVENTORY_ITEM_FLAG = 'Y'
       and nvl(msi.attribute12, 'N') = 'N'
       and a.header_id = p_header_id;
    if v_qty_yj <= 0 then
      --comment from bruce: it's a bug, v_qty_yj can be null, but user needs to ship order that only has software, so leave it.
    
      p_txt := '硬件处理量小于等于零';
      raise form_trigger_failure;
    end if;
  
    for y in c_2 loop
      --if p_ORGANIZATION_ID not in (85,86) then
      select decode(col.SOURCES_OF_PRODUCTS, 'FW', 84, 'JW', 83)
        into V_SHIP_FROM_ORG_ID
        from cux_om_line_interface_v col
       where col.ORDER_LINE_ID = y.oe_line_id;
    
      select decode(H.ORG_ID, 81, 84, 82, 83)
        into V_ORG_ID
        from oe_order_headers_all h
       where h.header_id = y.oe_header_id;
      /* 
      select decode(V_SHIP_FROM_ORG_ID, 84, 'G151', 'H151')
        into v_pick_from_subinventory1
        from dual;*/ --COMMENTED BY BRUCE
    
      v_pick_from_subinventory1 := y.PICK_FROM_SUBINVENTORY;
    
      if y.software_flag = 'Y' then
        /*        select flv.attribute2
         into v_pick_from_subinventory1
         from fnd_lookup_values flv
        where flv.lookup_type = 'RJ_WIP_ISSUE_INVENTORY'
          AND FLV.DESCRIPTION = y.SEGMENT1
          and flv.language = 'ZHS'
          AND FLV.ATTRIBUTE1 = V_SHIP_FROM_ORG_ID
          AND FLV.ATTRIBUTE2 IS NOT NULL;*/
        select decode(V_SHIP_FROM_ORG_ID, 84, 'G999', 'H999')
          into v_pick_from_subinventory1
          from dual;
        /* else
        select decode(V_SHIP_FROM_ORG_ID, 84, 'G999', 'H999')
         into v_pick_from_subinventory1
         from dual;     */
      end if;
    
      select count(*)
        into v_count
        from wsh_delivery_details wdd
       where wdd.delivery_detail_id = y.delivery_detail_id
         and wdd.released_status in ('B');
      if v_count > 0 then
        p_txt := y.line_number || '状态是延交';
        raise form_trigger_failure;
      end if;
      inv_quantity_tree_pub.clear_quantity_cache;
      INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                             P_INIT_MSG_LST        => NULL,
                                             X_RETURN_STATUS       => L_RETURN_STATUS,
                                             X_MSG_COUNT           => L_MSG_COUNT,
                                             X_MSG_DATA            => L_MSG_DATA,
                                             P_ORGANIZATION_ID     => V_SHIP_FROM_ORG_ID, --库存组织ID
                                             P_INVENTORY_ITEM_ID   => y.inventory_item_id, --物料ID
                                             P_TREE_MODE           => 3,
                                             P_IS_REVISION_CONTROL => FALSE,
                                             P_IS_LOT_CONTROL      => FALSE,
                                             P_IS_SERIAL_CONTROL   => FALSE,
                                             P_REVISION            => NULL,
                                             P_LOT_NUMBER          => NULL,
                                             P_LOT_EXPIRATION_DATE => NULL,
                                             P_SUBINVENTORY_CODE   => v_pick_from_subinventory1, --子库code
                                             P_LOCATOR_ID          => y.LOCATOR_ID,
                                             P_COST_GROUP_ID       => NULL,
                                             P_ONHAND_SOURCE       => 3,
                                             X_QOH                 => v_L_QOH, --现有量
                                             X_RQOH                => v_L_RQOH,
                                             X_QR                  => v_L_QR,
                                             X_QS                  => v_L_QS,
                                             X_ATT                 => v_L_ATT,
                                             X_ATR                 => v_L_ATR);
      if nvl(v_L_ATT, 0) < y.QUANTITY and y.line_id <> 17467 then
        p_txt := y.segment1 || '-现有量不足';
        raise form_trigger_failure;
      end if;
      --判断是否是三角贸易
      if V_SHIP_FROM_ORG_ID = V_ORG_ID then
        p_txt := '行:' || y.line_number || '物料:' || y.segment1 ||
                 '不是三角贸易的物料';
        raise form_trigger_failure;
      end if;
    end loop;
    ----------------------------------------------通过验证-----------------------------------------
    for z in c_2 loop
      select decode(col.SOURCES_OF_PRODUCTS, 'FW', 84, 'JW', 83)
        into V_PRODUCTS_OU_ID
        from cux_om_line_interface_v col
       where col.ORDER_LINE_ID = z.oe_line_id;
      --子库转移
      SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL INTO V_TXN_ID FROM DUAL;
    
      if V_PRODUCTS_OU_ID = 83 then
        ---83杂发
        IF Z.Attribute13 = 'N' THEN
          SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
            INTO V_TXN_ID
            FROM DUAL;
        
          L_IFACE_REC.TRANSACTION_INTERFACE_ID := V_TXN_ID;
          L_IFACE_REC.TRANSACTION_MODE         := 3; --BACKGROUND
          L_IFACE_REC.PROCESS_FLAG             := 1; --TO BE PROCESSED    
          L_IFACE_REC.ORGANIZATION_ID          := 83;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID  := NULL /*ZG_GT_CSN_RLS_TL_P.GET_INV_MATERIAL_ACCID(MTL_ORG_ID)*/
           ;
          L_IFACE_REC.INVENTORY_ITEM_ID        := Z.INVENTORY_ITEM_ID;
          if z.SOFTWARE_FLAG = 'Y' then
            L_IFACE_REC.Subinventory_Code := 'H999';
          else
            /*L_IFACE_REC.Subinventory_Code := 'H151';*/ --'H151'; --一定要给；如果ITEM限制了子库，那么必须给这个ITEM的子库
            L_IFACE_REC.Subinventory_Code := z.PICK_FROM_SUBINVENTORY;
            L_IFACE_REC.Locator_Id        := z.locator_id;
          end if;
          L_IFACE_REC.TRANSACTION_QUANTITY       := -Z.Quantity;
          L_IFACE_REC.PRIMARY_QUANTITY           := -Z.Quantity;
          L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
          L_IFACE_REC.Transaction_Cost           := NULL;
          L_IFACE_REC.TRANSACTION_DATE           := z.SHIP_DATE;
          L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
          L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂发';
          L_IFACE_REC.transaction_reference      := 'CUX.三角贸易杂发,单号:' ||
                                                    z.request_number;
          L_IFACE_REC.SOURCE_HEADER_ID           := z.header_id; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
          L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
          L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
          L_IFACE_REC.TRANSACTION_TYPE_ID        := 1; --帐户发放          
          L_IFACE_REC.TRANSACTION_SOURCE_ID      := 1032; --发出商品科目
          L_IFACE_REC.LAST_UPDATE_DATE           := SYSDATE;
          L_IFACE_REC.LAST_UPDATED_BY            := FND_GLOBAL.USER_ID;
          L_IFACE_REC.CREATION_DATE              := SYSDATE;
          L_IFACE_REC.CREATED_BY                 := FND_GLOBAL.USER_ID;
          L_IFACE_REC.LAST_UPDATE_LOGIN          := FND_GLOBAL.LOGIN_ID;
          L_IFACE_REC.TRANSACTION_HEADER_ID      := V_TXN_ID;
          L_IFACE_REC.ATTRIBUTE1                 := Z.OE_HEADER_ID;
          L_IFACE_REC.ATTRIBUTE2                 := Z.Oe_Line_Id;
          L_IFACE_REC.ATTRIBUTE3                 := 'Y';
          L_IFACE_REC.ATTRIBUTE4                 := z.attribute2;
          L_IFACE_REC.ATTRIBUTE5                 := p_header_id;
          INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
          L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                               P_INIT_MSG_LIST    => 'F',
                                                               P_VALIDATION_LEVEL => 100,
                                                               P_COMMIT           => 'T',
                                                               X_RETURN_STATUS    => L_RETURN_STATUS,
                                                               X_MSG_COUNT        => L_MSG_COUNT,
                                                               X_MSG_DATA         => L_MSG_DATA,
                                                               X_TRANS_COUNT      => L_TRANS_COUNT,
                                                               P_HEADER_ID        => V_TXN_ID);
          IF L_RESULT = 0 THEN
            UPDATE CUX.CUX_OE_SHIP_LINES B
               SET B.Attribute13 = 'ZF'
             WHERE B.LINE_ID = z.LINE_ID;
            V_FLAG := 'ZF';
          ELSE
            FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                        FROM MTL_TRANSACTIONS_INTERFACE
                       WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
              V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                              I.ERROR_EXPLANATION;
            END LOOP;
            p_txt := '83杂出:' || V_ERROR_CODE;
            RAISE form_trigger_failure;
          END IF;
        END IF;
      
        IF V_FLAG = 'ZF' OR Z.Attribute13 = 'ZF' THEN
          --84杂入
          --获取价格
          SELECT OOLA.UNIT_SELLING_PRICE,
                 OOHA.TRANSACTIONAL_CURR_CODE,
                 OOLA.TAX_CODE
            INTO V_UNIT_SELLING_PRICE,
                 V_TRANSACTIONAL_CURR_CODE,
                 V_TAX_CODE
            FROM OE_ORDER_LINES_ALL OOLA, OE_ORDER_HEADERS_ALL OOHA
           WHERE OOLA.HEADER_ID = OOHA.HEADER_ID
             AND OOLA.LINE_ID = Z.OE_LINE_ID;
          begin
            select c.PERCENTAGE_RATE
              into v_PERCENTAGE_RATE
              from zx_rates_vl c
             where c.TAX_RATE_CODE = v_tax_code;
          exception
            when others then
              v_PERCENTAGE_RATE := 0;
          end;
        
          IF V_TRANSACTIONAL_CURR_CODE <> 'CNY' THEN
          
            select gdr.conversion_rate
              into v_conversion_rate
              from GL_DAILY_RATES gdr
             where gdr.from_currency = V_TRANSACTIONAL_CURR_CODE
               and gdr.to_currency = 'CNY'
               and gdr.conversion_type = 'Corporate'
               and gdr.conversion_date = trunc(SYSDATE);
            V_UNIT_SELLING_PRICE := V_UNIT_SELLING_PRICE *
                                    v_conversion_rate;
          END IF;
        
          V_UNIT_SELLING_PRICE := ROUND(V_UNIT_SELLING_PRICE * 0.9 /
                                        (1 + v_PERCENTAGE_RATE / 100),
                                        6);
        
          SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
            INTO V_TXN_ID
            FROM DUAL;
        
          L_IFACE_REC.TRANSACTION_INTERFACE_ID := V_TXN_ID;
          L_IFACE_REC.TRANSACTION_MODE         := 3; --BACKGROUND
          L_IFACE_REC.PROCESS_FLAG             := 1; --TO BE PROCESSED    
          L_IFACE_REC.ORGANIZATION_ID          := 84;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID  := NULL /*ZG_GT_CSN_RLS_TL_P.GET_INV_MATERIAL_ACCID(MTL_ORG_ID)*/
           ;
          L_IFACE_REC.INVENTORY_ITEM_ID        := Z.INVENTORY_ITEM_ID;
          if z.SOFTWARE_FLAG = 'Y' then
            L_IFACE_REC.Subinventory_Code := 'G999';
          else
            L_IFACE_REC.Subinventory_Code := 'G151'; --'G151'; --一定要给；如果ITEM限制了子库，那么必须给这个ITEM的子库
          end if;
          L_IFACE_REC.TRANSACTION_QUANTITY       := Z.Quantity;
          L_IFACE_REC.PRIMARY_QUANTITY           := Z.Quantity;
          L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
          L_IFACE_REC.Transaction_Cost           := V_UNIT_SELLING_PRICE;
          L_IFACE_REC.TRANSACTION_DATE           := z.SHIP_DATE;
          L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
          L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂入';
          L_IFACE_REC.transaction_reference      := 'CUX.三角贸易杂入,单号:' ||
                                                    z.request_number;
          L_IFACE_REC.SOURCE_HEADER_ID           := z.header_id; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
          L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
          L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
          L_IFACE_REC.TRANSACTION_TYPE_ID        := 40; --帐户发放 
        
          L_IFACE_REC.TRANSACTION_SOURCE_ID := 1022; --应付暂估带往来
        
          L_IFACE_REC.LAST_UPDATE_DATE      := SYSDATE;
          L_IFACE_REC.LAST_UPDATED_BY       := FND_GLOBAL.USER_ID;
          L_IFACE_REC.CREATION_DATE         := SYSDATE;
          L_IFACE_REC.CREATED_BY            := FND_GLOBAL.USER_ID;
          L_IFACE_REC.LAST_UPDATE_LOGIN     := FND_GLOBAL.LOGIN_ID;
          L_IFACE_REC.TRANSACTION_HEADER_ID := V_TXN_ID;
          L_IFACE_REC.ATTRIBUTE1            := Z.OE_HEADER_ID;
          L_IFACE_REC.ATTRIBUTE2            := Z.Oe_Line_Id;
          L_IFACE_REC.ATTRIBUTE3            := 'Y';
          L_IFACE_REC.ATTRIBUTE4            := z.attribute2;
          L_IFACE_REC.ATTRIBUTE5            := p_header_id;
          INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
          L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                               P_INIT_MSG_LIST    => 'F',
                                                               P_VALIDATION_LEVEL => 100,
                                                               P_COMMIT           => 'T',
                                                               X_RETURN_STATUS    => L_RETURN_STATUS,
                                                               X_MSG_COUNT        => L_MSG_COUNT,
                                                               X_MSG_DATA         => L_MSG_DATA,
                                                               X_TRANS_COUNT      => L_TRANS_COUNT,
                                                               P_HEADER_ID        => V_TXN_ID);
          IF L_RESULT = 0 THEN
            UPDATE CUX.CUX_OE_SHIP_LINES B
               SET B.Attribute13 = 'Y', B.TRANSACTION_ID = V_TXN_ID
             WHERE B.LINE_ID = z.LINE_ID;
          ELSE
            FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                        FROM MTL_TRANSACTIONS_INTERFACE
                       WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
              V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                              I.ERROR_EXPLANATION;
            END LOOP;
            p_txt := '84杂入:' || V_ERROR_CODE;
            RAISE form_trigger_failure;
          END IF;
        end if;
      else
        ---84杂发
        IF Z.Attribute13 = 'N' THEN
          SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
            INTO V_TXN_ID
            FROM DUAL;
        
          L_IFACE_REC.TRANSACTION_INTERFACE_ID := V_TXN_ID;
          L_IFACE_REC.TRANSACTION_MODE         := 3; --BACKGROUND
          L_IFACE_REC.PROCESS_FLAG             := 1; --TO BE PROCESSED    
          L_IFACE_REC.ORGANIZATION_ID          := 84;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID  := NULL /*ZG_GT_CSN_RLS_TL_P.GET_INV_MATERIAL_ACCID(MTL_ORG_ID)*/
           ;
          L_IFACE_REC.INVENTORY_ITEM_ID        := Z.INVENTORY_ITEM_ID;
          if z.SOFTWARE_FLAG = 'Y' then
            L_IFACE_REC.Subinventory_Code := 'G999';
          else
            --L_IFACE_REC.Subinventory_Code := 'G151'; --'G151'; --一定要给；如果ITEM限制了子库，那么必须给这个ITEM的子库
            L_IFACE_REC.Subinventory_Code := z.PICK_FROM_SUBINVENTORY;
            L_IFACE_REC.Locator_Id        := z.locator_id;
          end if;
          L_IFACE_REC.TRANSACTION_QUANTITY       := -Z.Quantity;
          L_IFACE_REC.PRIMARY_QUANTITY           := -Z.Quantity;
          L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
          L_IFACE_REC.Transaction_Cost           := NULL;
          L_IFACE_REC.TRANSACTION_DATE           := z.SHIP_DATE;
          L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
          L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂发';
          L_IFACE_REC.transaction_reference      := 'CUX.三角贸易杂发,单号:' ||
                                                    z.request_number;
          L_IFACE_REC.SOURCE_HEADER_ID           := z.header_id; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
          L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
          L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
          L_IFACE_REC.TRANSACTION_TYPE_ID        := 1; --帐户发放          
          L_IFACE_REC.TRANSACTION_SOURCE_ID      := 1025; --发出商品科目
          L_IFACE_REC.LAST_UPDATE_DATE           := SYSDATE;
          L_IFACE_REC.LAST_UPDATED_BY            := FND_GLOBAL.USER_ID;
          L_IFACE_REC.CREATION_DATE              := SYSDATE;
          L_IFACE_REC.CREATED_BY                 := FND_GLOBAL.USER_ID;
          L_IFACE_REC.LAST_UPDATE_LOGIN          := FND_GLOBAL.LOGIN_ID;
          L_IFACE_REC.TRANSACTION_HEADER_ID      := V_TXN_ID;
          L_IFACE_REC.ATTRIBUTE1                 := Z.OE_HEADER_ID;
          L_IFACE_REC.ATTRIBUTE2                 := Z.Oe_Line_Id;
          L_IFACE_REC.ATTRIBUTE3                 := 'Y';
          L_IFACE_REC.ATTRIBUTE4                 := z.attribute2;
          L_IFACE_REC.ATTRIBUTE5                 := p_header_id;
          INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
          L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                               P_INIT_MSG_LIST    => 'F',
                                                               P_VALIDATION_LEVEL => 100,
                                                               P_COMMIT           => 'T',
                                                               X_RETURN_STATUS    => L_RETURN_STATUS,
                                                               X_MSG_COUNT        => L_MSG_COUNT,
                                                               X_MSG_DATA         => L_MSG_DATA,
                                                               X_TRANS_COUNT      => L_TRANS_COUNT,
                                                               P_HEADER_ID        => V_TXN_ID);
          IF L_RESULT = 0 THEN
            UPDATE CUX.CUX_OE_SHIP_LINES B
               SET B.Attribute13 = 'ZF'
             WHERE B.LINE_ID = z.LINE_ID;
            V_FLAG := 'ZF';
          ELSE
            FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                        FROM MTL_TRANSACTIONS_INTERFACE
                       WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
              V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                              I.ERROR_EXPLANATION;
            END LOOP;
            p_txt := '84杂出:' || V_ERROR_CODE;
            RAISE form_trigger_failure;
          END IF;
        END IF;
      
        IF V_FLAG = 'ZF' OR Z.Attribute13 = 'ZF' THEN
          --83杂入
          SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
            INTO V_TXN_ID
            FROM DUAL;
        
          --获取价格
          SELECT OOLA.UNIT_SELLING_PRICE,
                 OOHA.TRANSACTIONAL_CURR_CODE,
                 OOLA.TAX_CODE
            INTO V_UNIT_SELLING_PRICE,
                 V_TRANSACTIONAL_CURR_CODE,
                 V_TAX_CODE
            FROM OE_ORDER_LINES_ALL OOLA, OE_ORDER_HEADERS_ALL OOHA
           WHERE OOLA.HEADER_ID = OOHA.HEADER_ID
             AND OOLA.LINE_ID = Z.OE_LINE_ID;
          begin
            select c.PERCENTAGE_RATE
              into v_PERCENTAGE_RATE
              from zx_rates_vl c
             where c.TAX_RATE_CODE = v_tax_code;
          exception
            when others then
              v_PERCENTAGE_RATE := 0;
          end;
        
          IF V_TRANSACTIONAL_CURR_CODE <> 'CNY' THEN
          
            select gdr.conversion_rate
              into v_conversion_rate
              from GL_DAILY_RATES gdr
             where gdr.from_currency = V_TRANSACTIONAL_CURR_CODE
               and gdr.to_currency = 'CNY'
               and gdr.conversion_type = 'Corporate'
               and gdr.conversion_date = trunc(SYSDATE); --SYSDATE;
            V_UNIT_SELLING_PRICE := V_UNIT_SELLING_PRICE *
                                    v_conversion_rate;
          END IF;
        
          V_UNIT_SELLING_PRICE := ROUND(V_UNIT_SELLING_PRICE * 0.9 /
                                        (1 + v_PERCENTAGE_RATE / 100),
                                        6);
        
          L_IFACE_REC.TRANSACTION_INTERFACE_ID := V_TXN_ID;
          L_IFACE_REC.TRANSACTION_MODE         := 3; --BACKGROUND
          L_IFACE_REC.PROCESS_FLAG             := 1; --TO BE PROCESSED    
          L_IFACE_REC.ORGANIZATION_ID          := 83;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID  := NULL /*ZG_GT_CSN_RLS_TL_P.GET_INV_MATERIAL_ACCID(MTL_ORG_ID)*/
           ;
          L_IFACE_REC.INVENTORY_ITEM_ID        := Z.INVENTORY_ITEM_ID;
          if z.SOFTWARE_FLAG = 'Y' then
            L_IFACE_REC.Subinventory_Code := 'H999';
          else
            L_IFACE_REC.Subinventory_Code := 'H151'; --'H151'; --一定要给；如果ITEM限制了子库，那么必须给这个ITEM的子库
          end if;
          L_IFACE_REC.TRANSACTION_QUANTITY       := Z.Quantity;
          L_IFACE_REC.PRIMARY_QUANTITY           := Z.Quantity;
          L_IFACE_REC.Transaction_Cost           := V_UNIT_SELLING_PRICE;
          L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
          L_IFACE_REC.TRANSACTION_DATE           := z.SHIP_DATE;
          L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
          L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂入';
          L_IFACE_REC.transaction_reference      := 'CUX.三角贸易杂入,单号:' ||
                                                    z.request_number;
          L_IFACE_REC.SOURCE_HEADER_ID           := z.header_id; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
          L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
          L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
          L_IFACE_REC.TRANSACTION_TYPE_ID        := 40; --帐户发放 
        
          L_IFACE_REC.TRANSACTION_SOURCE_ID := 1029; --应付暂估带往来
        
          L_IFACE_REC.LAST_UPDATE_DATE      := SYSDATE;
          L_IFACE_REC.LAST_UPDATED_BY       := FND_GLOBAL.USER_ID;
          L_IFACE_REC.CREATION_DATE         := SYSDATE;
          L_IFACE_REC.CREATED_BY            := FND_GLOBAL.USER_ID;
          L_IFACE_REC.LAST_UPDATE_LOGIN     := FND_GLOBAL.LOGIN_ID;
          L_IFACE_REC.TRANSACTION_HEADER_ID := V_TXN_ID;
          L_IFACE_REC.ATTRIBUTE1            := Z.OE_HEADER_ID;
          L_IFACE_REC.ATTRIBUTE2            := Z.Oe_Line_Id;
          L_IFACE_REC.ATTRIBUTE3            := 'Y';
          L_IFACE_REC.ATTRIBUTE4            := z.attribute2;
          L_IFACE_REC.ATTRIBUTE5            := p_header_id;
          INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
          L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                               P_INIT_MSG_LIST    => 'F',
                                                               P_VALIDATION_LEVEL => 100,
                                                               P_COMMIT           => 'T',
                                                               X_RETURN_STATUS    => L_RETURN_STATUS,
                                                               X_MSG_COUNT        => L_MSG_COUNT,
                                                               X_MSG_DATA         => L_MSG_DATA,
                                                               X_TRANS_COUNT      => L_TRANS_COUNT,
                                                               P_HEADER_ID        => V_TXN_ID);
          IF L_RESULT = 0 THEN
            UPDATE CUX.CUX_OE_SHIP_LINES B
               SET B.Attribute13 = 'Y', B.TRANSACTION_ID = V_TXN_ID
             WHERE B.LINE_ID = z.LINE_ID;
          ELSE
            FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                        FROM MTL_TRANSACTIONS_INTERFACE
                       WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
              V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                              I.ERROR_EXPLANATION;
            END LOOP;
            p_txt := '83杂入:' || V_ERROR_CODE;
            RAISE form_trigger_failure;
          END IF;
        end if;
      end if;
      --判断是否是软件
    
    /*  if z.SOFTWARE_FLAG = 'Y' then
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ---自动分配  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                v_line_tbl(1).line_id := z.Attribute11;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                v_line_tbl(1).organization_id := p_organization_id;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                FOR j IN v_line_tbl.first .. v_line_tbl.last LOOP
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  inv_ppengine_pvt.create_suggestions(p_api_version         => 1.0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      p_init_msg_list       => fnd_api.g_false,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      p_commit              => fnd_api.g_false,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      p_validation_level    => fnd_api.g_valid_level_none,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      x_return_status       => v_return_status,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      x_msg_count           => v_msg_count,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      x_msg_data            => v_msg_data,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      p_transaction_temp_id => v_line_tbl(j)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               .line_id,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      p_reservations        => l_rsr_type,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      p_suggest_serial      => fnd_api.g_true,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      p_plan_tasks          => FALSE,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      p_quick_pick_flag     => 'N',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      p_organization_id     => p_organization_id);
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  IF v_return_status = fnd_api.g_ret_sts_success THEN
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    BEGIN
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      l_trolin_tbl := v_line_tbl;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      IF (l_trolin_tbl.count <> 0) THEN
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        i := l_trolin_tbl.first;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        WHILE i IS NOT NULL LOOP
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          IF (l_trolin_tbl(i)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             .return_status <> fnd_api.g_ret_sts_unexp_error AND l_trolin_tbl(i)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             .return_status <> fnd_api.g_ret_sts_error) THEN
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            l_trolin_rec := inv_trolin_util.query_row(l_trolin_tbl(i)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .line_id);
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            l_trolin_tbl(i) := l_trolin_rec;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            v_qty_detailed := l_trolin_tbl(i).quantity_detailed;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            v_qty_delivered := nvl(l_trolin_tbl(i)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   .quantity_delivered,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   0);
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            IF nvl(v_qty_detailed, 0) = 0 THEN
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              l_mold_tbl := inv_mo_line_detail_util.query_rows(p_line_id => l_trolin_tbl(i)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .line_id);
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              FOR j IN 1 .. l_mold_tbl.count LOOP
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                -- l_mold_tbl(j).transaction_status := 1; --3
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                l_mold_tbl(j).transaction_mode := 1;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                l_mold_tbl(j).source_line_id := l_trolin_tbl(i)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .line_id;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                l_mold_tbl(j).transaction_status := 1;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                inv_mo_line_detail_util.update_row(v_return_status,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   l_mold_tbl(j));
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              END LOOP;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              SELECT transaction_header_id, transaction_quantity
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                INTO l_trolin_tbl(i).transaction_header_id,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     l_trolin_tbl(i).quantity_detailed
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                FROM mtl_material_transactions_temp
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               WHERE move_order_line_id = l_trolin_tbl(i).line_id;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              l_trolin_tbl(i).last_update_date := SYSDATE;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              l_trolin_tbl(i).last_update_login := fnd_global.login_id;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              IF l_trolin_tbl(i).last_update_login = -1 THEN
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                l_trolin_tbl(i).last_update_login := fnd_global.conc_login_id;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              END IF;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              l_trolin_tbl(i).last_updated_by := fnd_global.user_id;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              l_trolin_tbl(i).program_id := fnd_global.conc_program_id;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              l_trolin_tbl(i).program_update_date := SYSDATE;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              l_trolin_tbl(i).request_id := fnd_global.conc_request_id;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              l_trolin_tbl(i).program_application_id := fnd_global.prog_appl_id;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              inv_trolin_util.update_row(l_trolin_tbl(i));
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            END IF;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          END IF;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          i := l_trolin_tbl.next(i);
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        END LOOP;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      END IF;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    END;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  END IF;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                end loop;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              end if;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              commit;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ---处理物料搬运单
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              FOR c1 IN (SELECT transaction_header_id, transaction_temp_id
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           FROM mtl_material_transactions_temp
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          WHERE move_order_line_id = z.Attribute11) LOOP
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                UPDATE mtl_material_transactions_temp mmtt
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   SET mmtt.transaction_status = 1,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       mmtt.transaction_date   = sysdate,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       mmtt.last_update_date   = SYSDATE,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       mmtt.last_updated_by    = fnd_global.user_id,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       mmtt.source_line_id     = z.Attribute11
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 WHERE mmtt.transaction_header_id = c1.transaction_header_id;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                v_return := inv_lpn_trx_pub.process_lpn_trx(p_trx_hdr_id         => c1.transaction_header_id,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p_commit             => 'F',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            x_proc_msg           => v_msg_data,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p_proc_mode          => NULL,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p_process_trx        => 'T',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p_atomic             => 'T',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            p_business_flow_code => NULL);
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                dbms_output.put_line(v_return || ',' || v_msg_data);
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                IF v_return = 0 THEN
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  null;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ELSE
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  RAISE fnd_api.g_exc_unexpected_error;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                END IF;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              END LOOP;*/
    end loop;
    /*select distinct a.REQUEST_NUMBER
     into v_REQUEST_NUMBER
     from cux_oe_ship_headers_v a
    where a.HEADER_ID = p_HEADER_ID;*/ --commented by bruce on 2015-8-3
  
    for z in (select mtrl.line_id,
                     nvl(msi.attribute12, 'N') SOFTWARE_FLAG,
                     cosl.ISSUE_QUANTITY
                from --mtl_txn_request_headers mtrh,
                     mtl_txn_request_lines mtrl,
                     mtl_system_items_b    msi,
                     cux_oe_ship_lines_v   cosl,
                     cux_pick_sub_doc      cpd
               where /*mtrh.header_id = mtrl.header_id
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 and */
               mtrl.organization_id = msi.organization_id
           and mtrl.inventory_item_id = msi.inventory_item_id
           and cpd.req_header_id = p_header_id
           and cpd.cux_pick_line_id = cosl.LINE_ID
           and cosl.DELIVERY_DETAIL_ID = mtrl.txn_source_line_detail_id
              --and cosl.HEADER_ID = p_header_id
           and mtrl.header_id = p_header_id /*mtrh.request_number = v_REQUEST_NUMBER*/
              ) loop
    
      if z.ISSUE_QUANTITY > 0 then
        update mtl_txn_request_lines a
           set a.quantity = z.issue_quantity
         where a.line_id = z.line_id;
        commit;
        --处理物料搬运单
        INV_REPLENISH_DETAIL_PUB.LINE_DETAILS_PUB(P_LINE_ID               => z.line_id,
                                                  X_NUMBER_OF_ROWS        => X_NUMBER_OF_ROWS,
                                                  X_DETAILED_QTY          => X_DETAILED_QTY,
                                                  X_RETURN_STATUS         => v_RETURN_STATUS,
                                                  X_MSG_COUNT             => v_MSG_COUNT,
                                                  X_MSG_DATA              => v_MSG_DATA,
                                                  X_REVISION              => X_REVISION,
                                                  X_LOCATOR_ID            => X_LOCATOR_ID,
                                                  X_TRANSFER_TO_LOCATION  => X_TRANSFER_TO_LOCATION,
                                                  X_LOT_NUMBER            => X_LOT_NUMBER,
                                                  X_EXPIRATION_DATE       => X_EXPIRATION_DATE,
                                                  X_TRANSACTION_TEMP_ID   => X_TRANSACTION_TEMP_ID,
                                                  P_TRANSACTION_HEADER_ID => NULL,
                                                  P_TRANSACTION_MODE      => 1,
                                                  P_MOVE_ORDER_TYPE       => P_MOVE_ORDER_TYPE,
                                                  P_SERIAL_FLAG           => FND_API.G_FALSE,
                                                  P_PLAN_TASKS            => FALSE,
                                                  P_AUTO_PICK_CONFIRM     => FALSE,
                                                  P_COMMIT                => FALSE);
        IF (v_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS) THEN
          p_txt := p_txt || '搬运单保留失败';
        END IF;
        L_TROLIN_TBL := INV_TROLIN_UTIL.QUERY_ROWS(P_LINE_ID => z.line_id);
        L_MOLD_TBL   := INV_MO_LINE_DETAIL_UTIL.QUERY_ROWS(P_LINE_ID => z.line_id);
        INV_PICK_WAVE_PICK_CONFIRM_PUB.PICK_CONFIRM(P_API_VERSION_NUMBER => L_API_VERSION,
                                                    P_INIT_MSG_LIST      => L_INIT_MSG_LIST,
                                                    P_COMMIT             => L_COMMIT,
                                                    X_RETURN_STATUS      => V_RETURN_STATUS,
                                                    X_MSG_COUNT          => V_MSG_COUNT,
                                                    X_MSG_DATA           => V_MSG_DATA,
                                                    P_MOVE_ORDER_TYPE    => L_MOVE_ORDER_TYPE,
                                                    P_TRANSACTION_MODE   => 1,
                                                    P_TROLIN_TBL         => L_TROLIN_TBL,
                                                    P_MOLD_TBL           => L_MOLD_TBL,
                                                    X_MMTT_TBL           => X_MOLD_TBL,
                                                    X_TROLIN_TBL         => X_TROLIN_TBL);
      
        IF (v_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS) THEN
          p_txt := p_txt || '搬运单确认失败';
        END IF;
        commit;
      else
        inv_mo_backorder_pvt.backorder(x_return_status => v_return_status,
                                       x_msg_count     => v_msg_count,
                                       x_msg_data      => v_msg_data,
                                       p_line_id       => z.line_id);
      end if;
    end loop;
    --把零数据设置成已经处理
    for y in c_22 loop
      UPDATE CUX.CUX_OE_SHIP_LINES B
         SET B.Attribute13 = 'Y', B.TRANSACTION_ID = -1
       WHERE B.LINE_ID = y.LINE_ID;
    end loop;
    --update to be 'Y' for what? 在FORM的多角贸易出库里有逻辑
    /* update cux.cux_oe_ship_headers a
      set a.attribute1 = 'Y'
    where a.header_id = p_header_id;*/
    UPDATE CUX_PICK_SUB_DOC CPD
       SET CPD.ATTRIBUTE1 = 'Y'
     where cpd.req_header_id = p_header_id;
    p_flag := 'S';
    commit;
  exception
    when form_trigger_failure then
      p_flag := 'E';
      rollback;
  end;
  procedure main_check(p_header_id       in number,
                       p_ORGANIZATION_ID in number,
                       p_flag            in out varchar2,
                       p_txt             in out varchar2,
                       p_request_number  in out varchar2) as
    cursor c1 is
      select a.OE_LINE_ID, a.ITEM_CODE
        from cux_oe_ship_lines_v a
       where a.HEADER_ID = p_header_id;
    v_count      number;
    V_ERROR_CODE varchar2(250);
    form_trigger_failure exception;
  begin
    for x in c1 loop
      select count(*)
        into v_count
        from cux_om_line_interface_v b
       where b.ORDER_LINE_ID = x.oe_line_id;
      if v_count = 0 then
        p_txt := x.ITEM_CODE || '未维护订单行属性:';
        raise form_trigger_failure;
      end if;
    end loop;
    p_flag := 'S';
  exception
    when form_trigger_failure then
      p_flag := 'E';
      rollback;
  end;
  procedure main_old(p_header_id       in number,
                     p_ORGANIZATION_ID in number,
                     p_flag            in out varchar2,
                     p_txt             in out varchar2,
                     p_request_number  in out varchar2) as
    cursor c_1 is
      select a.PICK_FROM_SUBINVENTORY,
             a.LOCATOR_ID,
             b.INVENTORY_ITEM_ID,
             c.SOURCES_OF_PRODUCTS,
             msi.segment1,
             b.software_flag,
             sum(b.ISSUE_QUANTITY) QUANTITY
        from cux_oe_ship_headers_v   a,
             cux_oe_ship_lines_v     b,
             cux_om_line_interface_v c,
             mtl_system_items_b      msi
       where a.header_id = b.HEADER_ID
         and msi.inventory_item_id = b.INVENTORY_ITEM_ID
         and msi.organization_id = p_ORGANIZATION_ID
         and c.ORDER_LINE_ID = b.OE_LINE_ID
         and b.FLAG = 'Y'
         and msi.INVENTORY_ITEM_FLAG = 'Y'
         and a.header_id = p_header_id
       group by a.PICK_FROM_SUBINVENTORY,
                a.LOCATOR_ID,
                msi.segment1,
                c.SOURCES_OF_PRODUCTS,
                b.software_flag,
                b.INVENTORY_ITEM_ID;
  
    cursor c_2 is
      select a.PICK_FROM_SUBINVENTORY,
             a.LOCATOR_ID,
             b.INVENTORY_ITEM_ID,
             msi.segment1,
             b.LINE_NUMBER,
             b.DELIVERY_DETAIL_ID,
             b.ISSUE_QUANTITY QUANTITY,
             a.SET_ID,
             B.OE_LINE_ID,
             a.HEADER_ID,
             b.LINE_ID,
             a.OE_HEADER_ID,
             a.REQUEST_NUMBER,
             a.attribute2,
             NVL(B.Attribute13, 'N') Attribute13,
             b.ATTRIBUTE11, --搬运单ID
             b.software_flag,
             msi.primary_uom_code,
             /*sysdate SHIP_DATE*/
             TO_DATE('2014-09-30', 'YYYY-MM-DD') SHIP_DATE
        from cux_oe_ship_headers_v a,
             cux_oe_ship_lines_v   b,
             mtl_system_items_b    msi
       where a.header_id = b.HEADER_ID
         and msi.inventory_item_id = b.INVENTORY_ITEM_ID
         and msi.organization_id = p_ORGANIZATION_ID
         and b.FLAG = 'Y'
         and b.ISSUE_QUANTITY > 0
         and msi.INVENTORY_ITEM_FLAG = 'Y'
         and a.header_id = p_header_id
       order by b.LINE_NUMBER;
  
    cursor c_21 is
      select a.PICK_FROM_SUBINVENTORY,
             a.LOCATOR_ID,
             b.INVENTORY_ITEM_ID,
             msi.segment1,
             b.LINE_NUMBER,
             b.DELIVERY_DETAIL_ID,
             b.ISSUE_QUANTITY QUANTITY,
             a.SET_ID,
             B.OE_LINE_ID,
             a.HEADER_ID,
             b.LINE_ID,
             a.OE_HEADER_ID,
             a.REQUEST_NUMBER,
             a.attribute2,
             NVL(B.Attribute13, 'N') Attribute13,
             b.ATTRIBUTE11, --搬运单ID
             b.software_flag,
             msi.primary_uom_code
        from cux_oe_ship_headers_v a,
             cux_oe_ship_lines_v   b,
             mtl_system_items_b    msi
       where a.header_id = b.HEADER_ID
         and msi.inventory_item_id = b.INVENTORY_ITEM_ID
         and msi.organization_id = p_ORGANIZATION_ID
         and b.FLAG = 'Y'
         and msi.INVENTORY_ITEM_FLAG = 'Y'
         and a.header_id = p_header_id
       order by b.LINE_NUMBER;
  
    cursor c_22 is
      select a.PICK_FROM_SUBINVENTORY,
             a.LOCATOR_ID,
             b.INVENTORY_ITEM_ID,
             msi.segment1,
             b.LINE_NUMBER,
             b.DELIVERY_DETAIL_ID,
             b.ISSUE_QUANTITY QUANTITY,
             a.SET_ID,
             B.OE_LINE_ID,
             a.HEADER_ID,
             b.LINE_ID,
             a.OE_HEADER_ID,
             a.REQUEST_NUMBER,
             a.attribute2,
             NVL(B.Attribute13, 'N') Attribute13,
             b.ATTRIBUTE11, --搬运单ID
             b.software_flag,
             msi.primary_uom_code
        from cux_oe_ship_headers_v a,
             cux_oe_ship_lines_v   b,
             mtl_system_items_b    msi
       where a.header_id = b.HEADER_ID
         and msi.inventory_item_id = b.INVENTORY_ITEM_ID
         and msi.organization_id = p_ORGANIZATION_ID
         and b.FLAG = 'Y'
         and b.ISSUE_QUANTITY = 0
         and msi.INVENTORY_ITEM_FLAG = 'Y'
         and a.header_id = p_header_id
       order by b.LINE_NUMBER;
    v_count number;
    form_trigger_failure exception;
    L_RETURN_STATUS           varchar2(2000);
    L_MSG_COUNT               number;
    L_MSG_DATA                varchar2(2000);
    v_L_QOH                   number;
    v_L_RQOH                  number;
    v_L_QR                    number;
    v_L_QS                    number;
    v_L_ATT                   number;
    v_L_ATR                   number;
    V_SHIP_FROM_ORG_ID        number;
    v_pick_from_subinventory1 varchar2(2000);
    V_ORG_ID                  number;
    V_PRODUCTS_OU_ID          number;
    L_IFACE_REC               MTL_TRANSACTIONS_INTERFACE%ROWTYPE;
    V_TXN_ID                  NUMBER;
    L_RESULT                  NUMBER;
    L_TRANS_COUNT             NUMBER;
    V_ERROR_CODE              varchar2(100);
    V_UNIT_SELLING_PRICE      NUMBER;
    V_TRANSACTIONAL_CURR_CODE varchar2(100);
    v_conversion_rate         NUMBER;
    V_FLAG                    varchar2(100);
    V_TAX_CODE                varchar2(100);
    v_PERCENTAGE_RATE         NUMBER;
    --------------------------------
    v_line_tbl   inv_move_order_pub.trolin_tbl_type;
    l_trolin_tbl inv_move_order_pub.trolin_tbl_type;
    l_trolin_rec inv_move_order_pub.trolin_rec_type;
    l_rsr_type   inv_reservation_global.mtl_reservation_tbl_type;
    l_mold_tbl   inv_mo_line_detail_util.g_mmtt_tbl_type;
    l_api_name       CONSTANT VARCHAR2(30) := 'allocate_move_order';
    l_savepoint_name CONSTANT VARCHAR2(30) := 'allocate_move_order';
    v_return_status         VARCHAR2(30);
    v_msg_count             NUMBER;
    v_msg_data              VARCHAR2(4000);
    v_msg_index_out         NUMBER;
    v_msg_data2             VARCHAR2(2000);
    v_qty_detailed          NUMBER;
    v_qty_delivered         NUMBER;
    i                       number;
    v_return                NUMBER;
    v_REQUEST_NUMBER        VARCHAR2(2000);
    L_API_VERSION           NUMBER := 1.0;
    L_INIT_MSG_LIST         VARCHAR2(2) := FND_API.G_TRUE;
    L_RETURN_VALUES         VARCHAR2(2) := FND_API.G_FALSE;
    L_COMMIT                VARCHAR2(2) := FND_API.G_FALSE;
    L_MOVE_ORDER_TYPE       NUMBER := 3;
    X_MOLD_TBL              INV_MO_LINE_DETAIL_UTIL.G_MMTT_TBL_TYPE;
    X_TROLIN_TBL            INV_MOVE_ORDER_PUB.TROLIN_TBL_TYPE;
    L_MSG_RETURN            NUMBER;
    X_NUMBER_OF_ROWS        NUMBER;
    X_DETAILED_QTY          number;
    X_REVISION              VARCHAR2(20);
    X_LOCATOR_ID            NUMBER;
    X_TRANSFER_TO_LOCATION  NUMBER;
    X_LOT_NUMBER            VARCHAR2(80);
    X_EXPIRATION_DATE       DATE;
    X_TRANSACTION_TEMP_ID   NUMBER;
    P_TRANSACTION_HEADER_ID NUMBER;
    P_TRANSACTION_MODE      NUMBER;
    P_MOVE_ORDER_TYPE       NUMBER := 3;
    v_qty_yj                NUMBER;
  begin
    select count(*)
      into v_count
      from cux_oe_ship_lines_v b
     where b.HEADER_ID = p_header_id
       and b.FLAG = 'Y';
    if v_count = 0 then
      p_txt := '未勾选记录';
      raise form_trigger_failure;
    end if;
  
    for x in c_1 loop
      --if p_ORGANIZATION_ID not in (85,86) then 
      BEGIN
        SELECT decode(x.SOURCES_OF_PRODUCTS, 'JW', 83, 'FW', '84')
          INTO V_SHIP_FROM_ORG_ID
          FROM dual;
      EXCEPTION
        WHEN OTHERS THEN
          V_SHIP_FROM_ORG_ID := p_ORGANIZATION_ID;
      END;
      select decode(V_SHIP_FROM_ORG_ID, 84, 'G151', 'H151')
        into v_pick_from_subinventory1
        from dual;
      if x.software_flag = 'Y' then
        /*        select flv.attribute2
         into v_pick_from_subinventory1
         from fnd_lookup_values flv
        where flv.lookup_type = 'RJ_WIP_ISSUE_INVENTORY'
          AND FLV.DESCRIPTION = x.SEGMENT1
          and flv.language = 'ZHS'
          AND FLV.ATTRIBUTE1 = V_SHIP_FROM_ORG_ID
          AND FLV.ATTRIBUTE2 IS NOT NULL;*/
        select decode(V_SHIP_FROM_ORG_ID, 84, 'G999', 'H999')
          into v_pick_from_subinventory1
          from dual;
      else
        select decode(V_SHIP_FROM_ORG_ID, 84, 'G999', 'H999')
          into v_pick_from_subinventory1
          from dual;
      end if;
    
      inv_quantity_tree_pub.clear_quantity_cache;
      INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                             P_INIT_MSG_LST        => NULL,
                                             X_RETURN_STATUS       => L_RETURN_STATUS,
                                             X_MSG_COUNT           => L_MSG_COUNT,
                                             X_MSG_DATA            => L_MSG_DATA,
                                             P_ORGANIZATION_ID     => V_SHIP_FROM_ORG_ID, --库存组织ID
                                             P_INVENTORY_ITEM_ID   => x.inventory_item_id, --物料ID
                                             P_TREE_MODE           => 3,
                                             P_IS_REVISION_CONTROL => FALSE,
                                             P_IS_LOT_CONTROL      => FALSE,
                                             P_IS_SERIAL_CONTROL   => FALSE,
                                             P_REVISION            => NULL,
                                             P_LOT_NUMBER          => NULL,
                                             P_LOT_EXPIRATION_DATE => NULL,
                                             P_SUBINVENTORY_CODE   => v_pick_from_subinventory1, --子库code
                                             P_LOCATOR_ID          => x.LOCATOR_ID,
                                             P_COST_GROUP_ID       => NULL,
                                             P_ONHAND_SOURCE       => 3,
                                             X_QOH                 => v_L_QOH, --现有量
                                             X_RQOH                => v_L_RQOH,
                                             X_QR                  => v_L_QR,
                                             X_QS                  => v_L_QS,
                                             X_ATT                 => v_L_ATT,
                                             X_ATR                 => v_L_ATR);
      if nvl(v_L_ATT, 0) < x.QUANTITY then
        p_txt := x.segment1 || '-现有量不足';
        raise form_trigger_failure;
      end if;
      --end if; 
    end loop;
    for y in c_21 loop
      if y.QUANTITY < 0 then
        p_txt := y.segment1 || '-处理量小于零';
        raise form_trigger_failure;
      end if;
    end loop;
  
    ---判断硬件数量是否大于零
    select sum(b.ISSUE_QUANTITY) QUANTITY
      into v_qty_yj
      from cux_oe_ship_headers_v   a,
           cux_oe_ship_lines_v     b,
           cux_om_line_interface_v c,
           mtl_system_items_b      msi
     where a.header_id = b.HEADER_ID
       and msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and c.ORDER_LINE_ID = b.OE_LINE_ID
       and b.FLAG = 'Y'
       and msi.INVENTORY_ITEM_FLAG = 'Y'
       and nvl(msi.attribute12, 'N') = 'N'
       and a.header_id = p_header_id;
    if v_qty_yj <= 0 then
      p_txt := '硬件处理量小于等于零';
      raise form_trigger_failure;
    end if;
  
    for y in c_2 loop
      --if p_ORGANIZATION_ID not in (85,86) then
      select decode(col.SOURCES_OF_PRODUCTS, 'FW', 84, 'JW', 83)
        into V_SHIP_FROM_ORG_ID
        from cux_om_line_interface_v col
       where col.ORDER_LINE_ID = y.oe_line_id;
      select decode(H.ORG_ID, 81, 84, 82, 83)
        into V_ORG_ID
        from oe_order_headers_all h
       where h.header_id = y.oe_header_id;
      select decode(V_SHIP_FROM_ORG_ID, 84, 'G151', 'H151')
        into v_pick_from_subinventory1
        from dual;
      if y.software_flag = 'Y' then
        /*        select flv.attribute2
         into v_pick_from_subinventory1
         from fnd_lookup_values flv
        where flv.lookup_type = 'RJ_WIP_ISSUE_INVENTORY'
          AND FLV.DESCRIPTION = y.SEGMENT1
          and flv.language = 'ZHS'
          AND FLV.ATTRIBUTE1 = V_SHIP_FROM_ORG_ID
          AND FLV.ATTRIBUTE2 IS NOT NULL;*/
        select decode(V_SHIP_FROM_ORG_ID, 84, 'G999', 'H999')
          into v_pick_from_subinventory1
          from dual;
      else
        select decode(V_SHIP_FROM_ORG_ID, 84, 'G999', 'H999')
          into v_pick_from_subinventory1
          from dual;
      end if;
    
      select count(*)
        into v_count
        from wsh_delivery_details wdd
       where wdd.delivery_detail_id = y.delivery_detail_id
         and wdd.released_status in ('B');
      if v_count > 0 then
        p_txt := y.line_number || '状态是延交';
        raise form_trigger_failure;
      end if;
      inv_quantity_tree_pub.clear_quantity_cache;
      INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                             P_INIT_MSG_LST        => NULL,
                                             X_RETURN_STATUS       => L_RETURN_STATUS,
                                             X_MSG_COUNT           => L_MSG_COUNT,
                                             X_MSG_DATA            => L_MSG_DATA,
                                             P_ORGANIZATION_ID     => V_SHIP_FROM_ORG_ID, --库存组织ID
                                             P_INVENTORY_ITEM_ID   => y.inventory_item_id, --物料ID
                                             P_TREE_MODE           => 3,
                                             P_IS_REVISION_CONTROL => FALSE,
                                             P_IS_LOT_CONTROL      => FALSE,
                                             P_IS_SERIAL_CONTROL   => FALSE,
                                             P_REVISION            => NULL,
                                             P_LOT_NUMBER          => NULL,
                                             P_LOT_EXPIRATION_DATE => NULL,
                                             P_SUBINVENTORY_CODE   => v_pick_from_subinventory1, --子库code
                                             P_LOCATOR_ID          => y.LOCATOR_ID,
                                             P_COST_GROUP_ID       => NULL,
                                             P_ONHAND_SOURCE       => 3,
                                             X_QOH                 => v_L_QOH, --现有量
                                             X_RQOH                => v_L_RQOH,
                                             X_QR                  => v_L_QR,
                                             X_QS                  => v_L_QS,
                                             X_ATT                 => v_L_ATT,
                                             X_ATR                 => v_L_ATR);
      if nvl(v_L_ATT, 0) < y.QUANTITY then
        p_txt := y.segment1 || '-现有量不足';
        raise form_trigger_failure;
      end if;
      --判断是否是三角贸易
      if V_SHIP_FROM_ORG_ID = V_ORG_ID then
        p_txt := '行:' || y.line_number || '物料:' || y.segment1 ||
                 '不是三角贸易的物料';
        raise form_trigger_failure;
      end if;
    end loop;
    ----------------------------------------------通过验证-----------------------------------------
    for z in c_2 loop
      select decode(col.SOURCES_OF_PRODUCTS, 'FW', 84, 'JW', 83)
        into V_PRODUCTS_OU_ID
        from cux_om_line_interface_v col
       where col.ORDER_LINE_ID = z.oe_line_id;
      --子库转移
      SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL INTO V_TXN_ID FROM DUAL;
    
      if V_PRODUCTS_OU_ID = 83 then
        ---83杂发
        IF Z.Attribute13 = 'N' THEN
          SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
            INTO V_TXN_ID
            FROM DUAL;
        
          L_IFACE_REC.TRANSACTION_INTERFACE_ID := V_TXN_ID;
          L_IFACE_REC.TRANSACTION_MODE         := 3; --BACKGROUND
          L_IFACE_REC.PROCESS_FLAG             := 1; --TO BE PROCESSED    
          L_IFACE_REC.ORGANIZATION_ID          := 83;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID  := NULL /*ZG_GT_CSN_RLS_TL_P.GET_INV_MATERIAL_ACCID(MTL_ORG_ID)*/
           ;
          L_IFACE_REC.INVENTORY_ITEM_ID        := Z.INVENTORY_ITEM_ID;
          if z.SOFTWARE_FLAG = 'Y' then
            L_IFACE_REC.Subinventory_Code := 'H999';
          else
            L_IFACE_REC.Subinventory_Code := 'H999'; --'H151'; --一定要给；如果ITEM限制了子库，那么必须给这个ITEM的子库
          end if;
          L_IFACE_REC.TRANSACTION_QUANTITY       := -Z.Quantity;
          L_IFACE_REC.PRIMARY_QUANTITY           := -Z.Quantity;
          L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
          L_IFACE_REC.Transaction_Cost           := NULL;
          L_IFACE_REC.TRANSACTION_DATE           := z.SHIP_DATE;
          L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
          L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂发';
          L_IFACE_REC.transaction_reference      := 'CUX.三角贸易杂发,单号:' ||
                                                    z.request_number;
          L_IFACE_REC.SOURCE_HEADER_ID           := z.header_id; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
          L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
          L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
          L_IFACE_REC.TRANSACTION_TYPE_ID        := 1; --帐户发放          
          L_IFACE_REC.TRANSACTION_SOURCE_ID      := 1032; --发出商品科目
          L_IFACE_REC.LAST_UPDATE_DATE           := SYSDATE;
          L_IFACE_REC.LAST_UPDATED_BY            := FND_GLOBAL.USER_ID;
          L_IFACE_REC.CREATION_DATE              := SYSDATE;
          L_IFACE_REC.CREATED_BY                 := FND_GLOBAL.USER_ID;
          L_IFACE_REC.LAST_UPDATE_LOGIN          := FND_GLOBAL.LOGIN_ID;
          L_IFACE_REC.TRANSACTION_HEADER_ID      := V_TXN_ID;
          L_IFACE_REC.ATTRIBUTE1                 := Z.OE_HEADER_ID;
          L_IFACE_REC.ATTRIBUTE2                 := Z.Oe_Line_Id;
          L_IFACE_REC.ATTRIBUTE3                 := 'Y';
          L_IFACE_REC.ATTRIBUTE4                 := z.attribute2;
          L_IFACE_REC.ATTRIBUTE5                 := p_header_id;
          INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
          L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                               P_INIT_MSG_LIST    => 'F',
                                                               P_VALIDATION_LEVEL => 100,
                                                               P_COMMIT           => 'T',
                                                               X_RETURN_STATUS    => L_RETURN_STATUS,
                                                               X_MSG_COUNT        => L_MSG_COUNT,
                                                               X_MSG_DATA         => L_MSG_DATA,
                                                               X_TRANS_COUNT      => L_TRANS_COUNT,
                                                               P_HEADER_ID        => V_TXN_ID);
          IF L_RESULT = 0 THEN
            UPDATE CUX.CUX_OE_SHIP_LINES B
               SET B.Attribute13 = 'ZF'
             WHERE B.LINE_ID = z.LINE_ID;
            V_FLAG := 'ZF';
          ELSE
            FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                        FROM MTL_TRANSACTIONS_INTERFACE
                       WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
              V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                              I.ERROR_EXPLANATION;
            END LOOP;
            p_txt := '83杂出:' || V_ERROR_CODE;
            RAISE form_trigger_failure;
          END IF;
        END IF;
      
        IF V_FLAG = 'ZF' OR Z.Attribute13 = 'ZF' THEN
          --84杂入
          --获取价格
          SELECT OOLA.UNIT_SELLING_PRICE,
                 OOHA.TRANSACTIONAL_CURR_CODE,
                 OOLA.TAX_CODE
            INTO V_UNIT_SELLING_PRICE,
                 V_TRANSACTIONAL_CURR_CODE,
                 V_TAX_CODE
            FROM OE_ORDER_LINES_ALL OOLA, OE_ORDER_HEADERS_ALL OOHA
           WHERE OOLA.HEADER_ID = OOHA.HEADER_ID
             AND OOLA.LINE_ID = Z.OE_LINE_ID;
          begin
            select c.PERCENTAGE_RATE
              into v_PERCENTAGE_RATE
              from zx_rates_vl c
             where c.TAX_RATE_CODE = v_tax_code;
          exception
            when others then
              v_PERCENTAGE_RATE := 0;
          end;
        
          IF V_TRANSACTIONAL_CURR_CODE <> 'CNY' THEN
          
            select gdr.conversion_rate
              into v_conversion_rate
              from GL_DAILY_RATES gdr
             where gdr.from_currency = V_TRANSACTIONAL_CURR_CODE
               and gdr.to_currency = 'CNY'
               and gdr.conversion_type = 'Corporate'
               and gdr.conversion_date = SYSDATE;
            V_UNIT_SELLING_PRICE := V_UNIT_SELLING_PRICE *
                                    v_conversion_rate;
          END IF;
        
          V_UNIT_SELLING_PRICE := ROUND(V_UNIT_SELLING_PRICE * 0.9 /
                                        (1 + v_PERCENTAGE_RATE / 100),
                                        6);
        
          SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
            INTO V_TXN_ID
            FROM DUAL;
        
          L_IFACE_REC.TRANSACTION_INTERFACE_ID := V_TXN_ID;
          L_IFACE_REC.TRANSACTION_MODE         := 3; --BACKGROUND
          L_IFACE_REC.PROCESS_FLAG             := 1; --TO BE PROCESSED    
          L_IFACE_REC.ORGANIZATION_ID          := 84;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID  := NULL /*ZG_GT_CSN_RLS_TL_P.GET_INV_MATERIAL_ACCID(MTL_ORG_ID)*/
           ;
          L_IFACE_REC.INVENTORY_ITEM_ID        := Z.INVENTORY_ITEM_ID;
          if z.SOFTWARE_FLAG = 'Y' then
            L_IFACE_REC.Subinventory_Code := 'G999';
          else
            L_IFACE_REC.Subinventory_Code := 'G999'; --'G151'; --一定要给；如果ITEM限制了子库，那么必须给这个ITEM的子库
          end if;
          L_IFACE_REC.TRANSACTION_QUANTITY       := Z.Quantity;
          L_IFACE_REC.PRIMARY_QUANTITY           := Z.Quantity;
          L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
          L_IFACE_REC.Transaction_Cost           := V_UNIT_SELLING_PRICE;
          L_IFACE_REC.TRANSACTION_DATE           := z.SHIP_DATE;
          L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
          L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂入';
          L_IFACE_REC.transaction_reference      := 'CUX.三角贸易杂入,单号:' ||
                                                    z.request_number;
          L_IFACE_REC.SOURCE_HEADER_ID           := z.header_id; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
          L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
          L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
          L_IFACE_REC.TRANSACTION_TYPE_ID        := 40; --帐户发放 
        
          L_IFACE_REC.TRANSACTION_SOURCE_ID := 1022; --应付暂估带往来
        
          L_IFACE_REC.LAST_UPDATE_DATE      := SYSDATE;
          L_IFACE_REC.LAST_UPDATED_BY       := FND_GLOBAL.USER_ID;
          L_IFACE_REC.CREATION_DATE         := SYSDATE;
          L_IFACE_REC.CREATED_BY            := FND_GLOBAL.USER_ID;
          L_IFACE_REC.LAST_UPDATE_LOGIN     := FND_GLOBAL.LOGIN_ID;
          L_IFACE_REC.TRANSACTION_HEADER_ID := V_TXN_ID;
          L_IFACE_REC.ATTRIBUTE1            := Z.OE_HEADER_ID;
          L_IFACE_REC.ATTRIBUTE2            := Z.Oe_Line_Id;
          L_IFACE_REC.ATTRIBUTE3            := 'Y';
          L_IFACE_REC.ATTRIBUTE4            := z.attribute2;
          L_IFACE_REC.ATTRIBUTE5            := p_header_id;
          INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
          L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                               P_INIT_MSG_LIST    => 'F',
                                                               P_VALIDATION_LEVEL => 100,
                                                               P_COMMIT           => 'T',
                                                               X_RETURN_STATUS    => L_RETURN_STATUS,
                                                               X_MSG_COUNT        => L_MSG_COUNT,
                                                               X_MSG_DATA         => L_MSG_DATA,
                                                               X_TRANS_COUNT      => L_TRANS_COUNT,
                                                               P_HEADER_ID        => V_TXN_ID);
          IF L_RESULT = 0 THEN
            UPDATE CUX.CUX_OE_SHIP_LINES B
               SET B.Attribute13 = 'Y', B.TRANSACTION_ID = V_TXN_ID
             WHERE B.LINE_ID = z.LINE_ID;
          ELSE
            FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                        FROM MTL_TRANSACTIONS_INTERFACE
                       WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
              V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                              I.ERROR_EXPLANATION;
            END LOOP;
            p_txt := '84杂入:' || V_ERROR_CODE;
            RAISE form_trigger_failure;
          END IF;
        end if;
      else
        ---84杂发
        IF Z.Attribute13 = 'N' THEN
          SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
            INTO V_TXN_ID
            FROM DUAL;
        
          L_IFACE_REC.TRANSACTION_INTERFACE_ID := V_TXN_ID;
          L_IFACE_REC.TRANSACTION_MODE         := 3; --BACKGROUND
          L_IFACE_REC.PROCESS_FLAG             := 1; --TO BE PROCESSED    
          L_IFACE_REC.ORGANIZATION_ID          := 84;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID  := NULL /*ZG_GT_CSN_RLS_TL_P.GET_INV_MATERIAL_ACCID(MTL_ORG_ID)*/
           ;
          L_IFACE_REC.INVENTORY_ITEM_ID        := Z.INVENTORY_ITEM_ID;
          if z.SOFTWARE_FLAG = 'Y' then
            L_IFACE_REC.Subinventory_Code := 'G999';
          else
            L_IFACE_REC.Subinventory_Code := 'G999'; --'G151'; --一定要给；如果ITEM限制了子库，那么必须给这个ITEM的子库
          end if;
          L_IFACE_REC.TRANSACTION_QUANTITY       := -Z.Quantity;
          L_IFACE_REC.PRIMARY_QUANTITY           := -Z.Quantity;
          L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
          L_IFACE_REC.Transaction_Cost           := NULL;
          L_IFACE_REC.TRANSACTION_DATE           := z.SHIP_DATE;
          L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
          L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂发';
          L_IFACE_REC.transaction_reference      := 'CUX.三角贸易杂发,单号:' ||
                                                    z.request_number;
          L_IFACE_REC.SOURCE_HEADER_ID           := z.header_id; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
          L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
          L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
          L_IFACE_REC.TRANSACTION_TYPE_ID        := 1; --帐户发放          
          L_IFACE_REC.TRANSACTION_SOURCE_ID      := 1025; --发出商品科目
          L_IFACE_REC.LAST_UPDATE_DATE           := SYSDATE;
          L_IFACE_REC.LAST_UPDATED_BY            := FND_GLOBAL.USER_ID;
          L_IFACE_REC.CREATION_DATE              := SYSDATE;
          L_IFACE_REC.CREATED_BY                 := FND_GLOBAL.USER_ID;
          L_IFACE_REC.LAST_UPDATE_LOGIN          := FND_GLOBAL.LOGIN_ID;
          L_IFACE_REC.TRANSACTION_HEADER_ID      := V_TXN_ID;
          L_IFACE_REC.ATTRIBUTE1                 := Z.OE_HEADER_ID;
          L_IFACE_REC.ATTRIBUTE2                 := Z.Oe_Line_Id;
          L_IFACE_REC.ATTRIBUTE3                 := 'Y';
          L_IFACE_REC.ATTRIBUTE4                 := z.attribute2;
          L_IFACE_REC.ATTRIBUTE5                 := p_header_id;
          INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
          L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                               P_INIT_MSG_LIST    => 'F',
                                                               P_VALIDATION_LEVEL => 100,
                                                               P_COMMIT           => 'T',
                                                               X_RETURN_STATUS    => L_RETURN_STATUS,
                                                               X_MSG_COUNT        => L_MSG_COUNT,
                                                               X_MSG_DATA         => L_MSG_DATA,
                                                               X_TRANS_COUNT      => L_TRANS_COUNT,
                                                               P_HEADER_ID        => V_TXN_ID);
          IF L_RESULT = 0 THEN
            UPDATE CUX.CUX_OE_SHIP_LINES B
               SET B.Attribute14 = 'ZF'
             WHERE B.LINE_ID = z.LINE_ID;
            V_FLAG := 'ZF';
          ELSE
            FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                        FROM MTL_TRANSACTIONS_INTERFACE
                       WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
              V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                              I.ERROR_EXPLANATION;
            END LOOP;
            p_txt := '84杂出:' || V_ERROR_CODE;
            RAISE form_trigger_failure;
          END IF;
        END IF;
      
        IF V_FLAG = 'ZF' OR Z.Attribute13 = 'ZF' THEN
          --83杂入
          SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
            INTO V_TXN_ID
            FROM DUAL;
        
          --获取价格
          SELECT OOLA.UNIT_SELLING_PRICE,
                 OOHA.TRANSACTIONAL_CURR_CODE,
                 OOLA.TAX_CODE
            INTO V_UNIT_SELLING_PRICE,
                 V_TRANSACTIONAL_CURR_CODE,
                 V_TAX_CODE
            FROM OE_ORDER_LINES_ALL OOLA, OE_ORDER_HEADERS_ALL OOHA
           WHERE OOLA.HEADER_ID = OOHA.HEADER_ID
             AND OOLA.LINE_ID = Z.OE_LINE_ID;
          begin
            select c.PERCENTAGE_RATE
              into v_PERCENTAGE_RATE
              from zx_rates_vl c
             where c.TAX_RATE_CODE = v_tax_code;
          exception
            when others then
              v_PERCENTAGE_RATE := 0;
          end;
        
          IF V_TRANSACTIONAL_CURR_CODE <> 'CNY' THEN
          
            select gdr.conversion_rate
              into v_conversion_rate
              from GL_DAILY_RATES gdr
             where gdr.from_currency = V_TRANSACTIONAL_CURR_CODE
               and gdr.to_currency = 'CNY'
               and gdr.conversion_type = 'Corporate'
               and gdr.conversion_date = SYSDATE; --SYSDATE;
            V_UNIT_SELLING_PRICE := V_UNIT_SELLING_PRICE *
                                    v_conversion_rate;
          END IF;
        
          V_UNIT_SELLING_PRICE := ROUND(V_UNIT_SELLING_PRICE * 0.9 /
                                        (1 + v_PERCENTAGE_RATE / 100),
                                        6);
        
          L_IFACE_REC.TRANSACTION_INTERFACE_ID := V_TXN_ID;
          L_IFACE_REC.TRANSACTION_MODE         := 3; --BACKGROUND
          L_IFACE_REC.PROCESS_FLAG             := 1; --TO BE PROCESSED    
          L_IFACE_REC.ORGANIZATION_ID          := 83;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID  := NULL /*ZG_GT_CSN_RLS_TL_P.GET_INV_MATERIAL_ACCID(MTL_ORG_ID)*/
           ;
          L_IFACE_REC.INVENTORY_ITEM_ID        := Z.INVENTORY_ITEM_ID;
          if z.SOFTWARE_FLAG = 'Y' then
            L_IFACE_REC.Subinventory_Code := 'H999';
          else
            L_IFACE_REC.Subinventory_Code := 'H999'; --'H151'; --一定要给；如果ITEM限制了子库，那么必须给这个ITEM的子库
          end if;
          L_IFACE_REC.TRANSACTION_QUANTITY       := Z.Quantity;
          L_IFACE_REC.PRIMARY_QUANTITY           := Z.Quantity;
          L_IFACE_REC.Transaction_Cost           := V_UNIT_SELLING_PRICE;
          L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
          L_IFACE_REC.TRANSACTION_DATE           := z.SHIP_DATE;
          L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
          L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂入';
          L_IFACE_REC.transaction_reference      := 'CUX.三角贸易杂入,单号:' ||
                                                    z.request_number;
          L_IFACE_REC.SOURCE_HEADER_ID           := z.header_id; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
          L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
          L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
          L_IFACE_REC.TRANSACTION_TYPE_ID        := 40; --帐户发放 
        
          L_IFACE_REC.TRANSACTION_SOURCE_ID := 1029; --应付暂估带往来
        
          L_IFACE_REC.LAST_UPDATE_DATE      := SYSDATE;
          L_IFACE_REC.LAST_UPDATED_BY       := FND_GLOBAL.USER_ID;
          L_IFACE_REC.CREATION_DATE         := SYSDATE;
          L_IFACE_REC.CREATED_BY            := FND_GLOBAL.USER_ID;
          L_IFACE_REC.LAST_UPDATE_LOGIN     := FND_GLOBAL.LOGIN_ID;
          L_IFACE_REC.TRANSACTION_HEADER_ID := V_TXN_ID;
          L_IFACE_REC.ATTRIBUTE1            := Z.OE_HEADER_ID;
          L_IFACE_REC.ATTRIBUTE2            := Z.Oe_Line_Id;
          L_IFACE_REC.ATTRIBUTE3            := 'Y';
          L_IFACE_REC.ATTRIBUTE4            := z.attribute2;
          L_IFACE_REC.ATTRIBUTE5            := p_header_id;
          INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
          L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                               P_INIT_MSG_LIST    => 'F',
                                                               P_VALIDATION_LEVEL => 100,
                                                               P_COMMIT           => 'T',
                                                               X_RETURN_STATUS    => L_RETURN_STATUS,
                                                               X_MSG_COUNT        => L_MSG_COUNT,
                                                               X_MSG_DATA         => L_MSG_DATA,
                                                               X_TRANS_COUNT      => L_TRANS_COUNT,
                                                               P_HEADER_ID        => V_TXN_ID);
          IF L_RESULT = 0 THEN
            UPDATE CUX.CUX_OE_SHIP_LINES B
               SET B.Attribute13 = 'Y', B.TRANSACTION_ID = V_TXN_ID
             WHERE B.LINE_ID = z.LINE_ID;
          ELSE
            FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                        FROM MTL_TRANSACTIONS_INTERFACE
                       WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
              V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                              I.ERROR_EXPLANATION;
            END LOOP;
            p_txt := '83杂入:' || V_ERROR_CODE;
            RAISE form_trigger_failure;
          END IF;
        end if;
      end if;
      --判断是否是软件
    end loop;
    /*select distinct a.REQUEST_NUMBER
      into v_REQUEST_NUMBER
      from cux_oe_ship_headers_v a
     where a.HEADER_ID = p_HEADER_ID;
    
    for z in (select mtrl.line_id,
                     nvl(msi.attribute12, 'N') SOFTWARE_FLAG,
                     cosl.ISSUE_QUANTITY
                from mtl_txn_request_headers mtrh,
                     mtl_txn_request_lines   mtrl,
                     mtl_system_items_b      msi,
                     cux_oe_ship_lines_v     cosl
               where mtrh.header_id = mtrl.header_id
                 and mtrl.organization_id = msi.organization_id
                 and mtrl.inventory_item_id = msi.inventory_item_id
                 and cosl.DELIVERY_DETAIL_ID =
                     mtrl.txn_source_line_detail_id
                 and cosl.HEADER_ID=p_header_id    
                 and mtrh.request_number = v_REQUEST_NUMBER) loop
    
      if z.ISSUE_QUANTITY > 0 then
        update mtl_txn_request_lines a
           set a.quantity = z.issue_quantity
         where a.line_id = z.line_id;
        commit;
        --处理物料搬运单
        INV_REPLENISH_DETAIL_PUB.LINE_DETAILS_PUB(P_LINE_ID               => z.line_id,
                                                  X_NUMBER_OF_ROWS        => X_NUMBER_OF_ROWS,
                                                  X_DETAILED_QTY          => X_DETAILED_QTY,
                                                  X_RETURN_STATUS         => v_RETURN_STATUS,
                                                  X_MSG_COUNT             => v_MSG_COUNT,
                                                  X_MSG_DATA              => v_MSG_DATA,
                                                  X_REVISION              => X_REVISION,
                                                  X_LOCATOR_ID            => X_LOCATOR_ID,
                                                  X_TRANSFER_TO_LOCATION  => X_TRANSFER_TO_LOCATION,
                                                  X_LOT_NUMBER            => X_LOT_NUMBER,
                                                  X_EXPIRATION_DATE       => X_EXPIRATION_DATE,
                                                  X_TRANSACTION_TEMP_ID   => X_TRANSACTION_TEMP_ID,
                                                  P_TRANSACTION_HEADER_ID => NULL,
                                                  P_TRANSACTION_MODE      => 1,
                                                  P_MOVE_ORDER_TYPE       => P_MOVE_ORDER_TYPE,
                                                  P_SERIAL_FLAG           => FND_API.G_FALSE,
                                                  P_PLAN_TASKS            => FALSE,
                                                  P_AUTO_PICK_CONFIRM     => FALSE,
                                                  P_COMMIT                => FALSE);
        IF (v_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS) THEN
          p_txt := p_txt || '搬运单保留失败';
        END IF;
        L_TROLIN_TBL := INV_TROLIN_UTIL.QUERY_ROWS(P_LINE_ID => z.line_id);
        L_MOLD_TBL   := INV_MO_LINE_DETAIL_UTIL.QUERY_ROWS(P_LINE_ID => z.line_id);
        INV_PICK_WAVE_PICK_CONFIRM_PUB.PICK_CONFIRM(P_API_VERSION_NUMBER => L_API_VERSION,
                                                    P_INIT_MSG_LIST      => L_INIT_MSG_LIST,
                                                    P_COMMIT             => L_COMMIT,
                                                    X_RETURN_STATUS      => V_RETURN_STATUS,
                                                    X_MSG_COUNT          => V_MSG_COUNT,
                                                    X_MSG_DATA           => V_MSG_DATA,
                                                    P_MOVE_ORDER_TYPE    => L_MOVE_ORDER_TYPE,
                                                    P_TRANSACTION_MODE   => 1,
                                                    P_TROLIN_TBL         => L_TROLIN_TBL,
                                                    P_MOLD_TBL           => L_MOLD_TBL,
                                                    X_MMTT_TBL           => X_MOLD_TBL,
                                                    X_TROLIN_TBL         => X_TROLIN_TBL);
      
        IF (v_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS) THEN
          p_txt := p_txt || '搬运单确认失败';
        END IF;
        commit;
      else
        inv_mo_backorder_pvt.backorder(x_return_status => v_return_status,
                                       x_msg_count     => v_msg_count,
                                       x_msg_data      => v_msg_data,
                                       p_line_id       => z.line_id);
      end if;
    end loop;*/
    --把零数据设置成已经处理
    for y in c_22 loop
      UPDATE CUX.CUX_OE_SHIP_LINES B
         SET B.Attribute13 = 'Y', B.TRANSACTION_ID = -1
       WHERE B.LINE_ID = y.LINE_ID;
    end loop;
  
    update cux.cux_oe_ship_headers a
       set a.attribute1 = 'Y'
     where a.header_id = p_header_id;
    p_flag := 'S';
    commit;
  exception
    when form_trigger_failure then
      p_flag := 'E';
      rollback;
  end;
  procedure main_new_jj(p_header_id       in number,
                        p_ORGANIZATION_ID in number,
                        p_flag            in out varchar2,
                        p_txt             in out varchar2,
                        p_request_number  in out varchar2) as
  
    cursor cur_old_req is
      SELECT MTRL.LINE_ID,
             COSL.ISSUE_QUANTITY,
             MTRL.FROM_SUBINVENTORY_CODE,
             MTRL.FROM_LOCATOR_ID,
             MTRL.ORGANIZATION_ID,
             MTRL.INVENTORY_ITEM_ID,
             MSI.SEGMENT1,
             COSL.LINE_ID SHIP_LINE_ID
        FROM /*MTL_TXN_REQUEST_HEADERS MTRH,*/ MTL_TXN_REQUEST_LINES MTRL,
             MTL_SYSTEM_ITEMS_B    MSI,
             --CUX_PICK_SUB_DOC      CPD, --ADDED BY BRUCE ON 2015-8-4
             /*CUX_OE_SHIP_LINES_V   COSL*/
             cux.cux_oe_ship_lines cosl
       WHERE MTRL.ORGANIZATION_ID = MSI.ORGANIZATION_ID
         AND MTRL.INVENTORY_ITEM_ID = MSI.INVENTORY_ITEM_ID
            /*AND COSL.HEADER_ID = P_HEADER_ID*/
         AND p_header_id = COSL.HEADER_ID
            --AND CPD.CUX_PICK_LINE_ID(+) = COSL.LINE_ID
         AND COSL.DELIVERY_DETAIL_ID = MTRL.TXN_SOURCE_LINE_DETAIL_ID
         and cosl.ATTRIBUTE11 = mtrl.line_id;
  
    cursor cur_new_req is
      SELECT MTRL.LINE_ID,
             COSL.ISSUE_QUANTITY,
             MTRL.FROM_SUBINVENTORY_CODE,
             MTRL.FROM_LOCATOR_ID,
             MTRL.ORGANIZATION_ID,
             MTRL.INVENTORY_ITEM_ID,
             MSI.SEGMENT1,
             COSL.LINE_ID SHIP_LINE_ID
        FROM /*MTL_TXN_REQUEST_HEADERS MTRH,*/ MTL_TXN_REQUEST_LINES MTRL,
             MTL_SYSTEM_ITEMS_B    MSI,
             CUX_PICK_SUB_DOC      CPD, --ADDED BY BRUCE ON 2015-8-4
             /*CUX_OE_SHIP_LINES_V   COSL*/
             cux.cux_oe_ship_lines cosl
       WHERE MTRL.ORGANIZATION_ID = MSI.ORGANIZATION_ID
         AND MTRL.INVENTORY_ITEM_ID = MSI.INVENTORY_ITEM_ID
            /*AND COSL.HEADER_ID = P_HEADER_ID*/
         AND CPD.REQ_HEADER_ID = P_HEADER_ID
            
         AND CPD.CUX_PICK_LINE_ID = COSL.LINE_ID
         AND COSL.DELIVERY_DETAIL_ID = MTRL.TXN_SOURCE_LINE_DETAIL_ID
         and cosl.ATTRIBUTE11 = mtrl.line_id;
  
    cursor c_2_old is
      select a.PICK_FROM_SUBINVENTORY PICK_FROM_SUBINVENTORY,
             a.LOCATOR_ID             LOCATOR_ID,
             b.INVENTORY_ITEM_ID,
             msi.segment1,
             --b.LINE_NUMBER,
             b.DELIVERY_DETAIL_ID,
             b.ISSUE_QUANTITY     QUANTITY,
             --a.SET_ID,
             B.OE_LINE_ID,
             a.header_id header_id, --a.HEADER_ID,
             b.LINE_ID,
             a.OE_HEADER_ID,
             a.request_number request_number, --a.REQUEST_NUMBER,
             a.attribute2,
             NVL(B.Attribute13, 'N') Attribute13,
             b.ATTRIBUTE11, --搬运单ID
             b.software_flag,
             msi.primary_uom_code,
             sysdate SHIP_DATE
      /*             TO_DATE('2014-09-30','YYYY-MM-DD')SHIP_DATE*/
        from /*cux_oe_ship_headers_v a,*/ cux.cux_oe_ship_headers a, --modified on 20151020
             /*cux_oe_ship_lines_v     b,*/
             cux.cux_oe_ship_lines b,
             --cux_pick_sub_doc      cpd,
             mtl_system_items_b msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                   and */
       a.header_id = b.header_id --modified on 20151021
       and msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
       and b.ISSUE_QUANTITY > 0
       and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
       and b.HEADER_ID = p_header_id
      
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020
  
    cursor c_2_new is
      select cpd.pick_subinv     PICK_FROM_SUBINVENTORY,
             cpd.pick_locator_id LOCATOR_ID,
             b.INVENTORY_ITEM_ID,
             msi.segment1,
             --b.LINE_NUMBER,
             b.DELIVERY_DETAIL_ID,
             b.ISSUE_QUANTITY     QUANTITY,
             --a.SET_ID,
             B.OE_LINE_ID,
             cpd.req_header_id header_id, --a.HEADER_ID,
             b.LINE_ID,
             a.OE_HEADER_ID,
             cpd.request_number request_number, --a.REQUEST_NUMBER,
             a.attribute2,
             NVL(B.Attribute13, 'N') Attribute13,
             b.ATTRIBUTE11, --搬运单ID
             b.software_flag,
             msi.primary_uom_code,
             sysdate SHIP_DATE
      /*             TO_DATE('2014-09-30','YYYY-MM-DD')SHIP_DATE*/
        from /*cux_oe_ship_headers_v a,*/ cux.cux_oe_ship_headers a, --modified on 20151020
             /*cux_oe_ship_lines_v     b,*/
             cux.cux_oe_ship_lines b,
             cux_pick_sub_doc      cpd,
             mtl_system_items_b    msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                   and */
       a.header_id = b.header_id --modified on 20151021
       and msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
       and b.ISSUE_QUANTITY > 0
       and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
       and cpd.req_header_id = p_header_id
       and cpd.cux_pick_line_id = b.LINE_ID
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020
  
    cursor c_21_old is
      select b.ISSUE_QUANTITY QUANTITY, msi.segment1
        from /*cux_oe_ship_headers_v a,*/
             /*cux_oe_ship_lines_v b,*/ cux.cux_oe_ship_lines b, --modified on 20151020
             --cux_pick_sub_doc      cpd,
             mtl_system_items_b msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
       and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
       and b.HEADER_ID = p_header_id
      --and cpd.cux_pick_line_id(+) = b.LINE_ID
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020
  
    cursor c_21_new is
      select b.ISSUE_QUANTITY QUANTITY, msi.segment1
        from /*cux_oe_ship_headers_v a,*/
             /*cux_oe_ship_lines_v b,*/ cux.cux_oe_ship_lines b, --modified on 20151020
             cux_pick_sub_doc      cpd,
             mtl_system_items_b    msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
       and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
       and cpd.req_header_id = p_header_id
       and cpd.cux_pick_line_id = b.LINE_ID
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020
  
    cursor c_22_old is
      select b.LINE_ID
        from /*cux_oe_ship_headers_v a,*/
             /*cux_oe_ship_lines_v b,*/ cux.cux_oe_ship_lines b, --modified on 20151020
             -- cux_pick_sub_doc      cpd,
             mtl_system_items_b msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
       and b.ISSUE_QUANTITY = 0
       and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
       and b.HEADER_ID = p_header_id
      
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020
  
    cursor c_22_new is
      select b.LINE_ID
        from /*cux_oe_ship_headers_v a,*/
             /*cux_oe_ship_lines_v b,*/ cux.cux_oe_ship_lines b, --modified on 20151020
             cux_pick_sub_doc      cpd,
             mtl_system_items_b    msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
       and b.ISSUE_QUANTITY = 0
       and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
       and cpd.req_header_id = p_header_id
       and cpd.cux_pick_line_id = b.LINE_ID
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020
  
    v_count        number;
    v_count_hdr    number;
    v_header_id    number;
    v_locator_qty  number;
    v_reserved_qty number;
    v_locator_att  number;
    form_trigger_failure exception;
    L_RETURN_STATUS           varchar2(2000);
    L_MSG_COUNT               number;
    L_MSG_DATA                varchar2(2000);
    v_L_QOH                   number;
    v_L_RQOH                  number;
    v_L_QR                    number;
    v_L_QS                    number;
    v_L_ATT                   number;
    v_L_ATR                   number;
    V_SHIP_FROM_ORG_ID        number;
    v_pick_from_subinventory1 varchar2(2000);
    V_ORG_ID                  number;
    V_PRODUCTS_OU_ID          number;
    L_IFACE_REC               MTL_TRANSACTIONS_INTERFACE%ROWTYPE;
    V_TXN_ID                  NUMBER;
    L_RESULT                  NUMBER;
    L_TRANS_COUNT             NUMBER;
    V_ERROR_CODE              varchar2(100);
    V_UNIT_SELLING_PRICE      NUMBER;
    V_TRANSACTIONAL_CURR_CODE varchar2(100);
    v_conversion_rate         NUMBER;
    V_FLAG                    varchar2(100);
    V_TAX_CODE                varchar2(100);
    v_PERCENTAGE_RATE         NUMBER;
    --------------------------------
    v_line_tbl   inv_move_order_pub.trolin_tbl_type;
    l_trolin_tbl inv_move_order_pub.trolin_tbl_type;
    l_trolin_rec inv_move_order_pub.trolin_rec_type;
    l_rsr_type   inv_reservation_global.mtl_reservation_tbl_type;
    l_mold_tbl   inv_mo_line_detail_util.g_mmtt_tbl_type;
    l_api_name       CONSTANT VARCHAR2(30) := 'allocate_move_order';
    l_savepoint_name CONSTANT VARCHAR2(30) := 'allocate_move_order';
    v_return_status         VARCHAR2(30);
    v_msg_count             NUMBER;
    v_msg_data              VARCHAR2(4000);
    v_msg_index_out         NUMBER;
    v_msg_data2             VARCHAR2(2000);
    v_qty_detailed          NUMBER;
    v_qty_delivered         NUMBER;
    i                       number;
    v_return                NUMBER;
    v_REQUEST_NUMBER        VARCHAR2(2000);
    L_API_VERSION           NUMBER := 1.0;
    L_INIT_MSG_LIST         VARCHAR2(2) := FND_API.G_TRUE;
    L_RETURN_VALUES         VARCHAR2(2) := FND_API.G_FALSE;
    L_COMMIT                VARCHAR2(2) := FND_API.G_FALSE;
    L_MOVE_ORDER_TYPE       NUMBER := 3;
    X_MOLD_TBL              INV_MO_LINE_DETAIL_UTIL.G_MMTT_TBL_TYPE;
    X_TROLIN_TBL            INV_MOVE_ORDER_PUB.TROLIN_TBL_TYPE;
    L_MSG_RETURN            NUMBER;
    X_NUMBER_OF_ROWS        NUMBER;
    X_DETAILED_QTY          number;
    X_REVISION              VARCHAR2(20);
    X_LOCATOR_ID            NUMBER;
    X_TRANSFER_TO_LOCATION  NUMBER;
    X_LOT_NUMBER            VARCHAR2(80);
    X_EXPIRATION_DATE       DATE;
    X_TRANSACTION_TEMP_ID   NUMBER;
    P_TRANSACTION_HEADER_ID NUMBER;
    P_TRANSACTION_MODE      NUMBER;
    P_MOVE_ORDER_TYPE       NUMBER := 3;
    v_qty_yj                NUMBER;
    V_XQ_QTY                NUMBER;
    V_SUBINVENTORY_CODE     VARCHAR2(80);
    V_SUBINVENTORY_CODE_R   VARCHAR2(80);
    V_LOCATION_ID_R         NUMBER;
  
    V_count_req number;
  begin
  
    begin
      select count(1)
        into v_count_req
        from cux_pick_sub_doc cpd
       where cpd.req_header_id = p_header_id;
    exception
      when others then
        v_count_req := 0;
    end;
  
    if (v_count_req > 0) then
    
      for y in c_21_new loop
      
        if y.QUANTITY < 0 then
          p_txt := y.segment1 || '-处理量小于零';
          raise form_trigger_failure;
        end if;
      end loop;
    
      ---判断硬件数量是否大于零
      select sum(b.ISSUE_QUANTITY) QUANTITY
        into v_qty_yj
        from /*cux_oe_ship_headers_v   a,*/ --modified on 20151020
             /* cux_oe_ship_lines_v     b,*/ --modified on 20151021
              cux.cux_oe_ship_lines b,
             cux.cux_pick_sub_doc  cpd,
             --cux_om_line_interface_v c,
             mtl_system_items_b msi
       where /*a.header_id = b.HEADER_ID--modified on 20151020
                                                                                                                                               and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
      -- and c.ORDER_LINE_ID = b.OE_LINE_ID
       and b.FLAG = 'Y'
       and msi.INVENTORY_ITEM_FLAG = 'Y'
       and nvl(msi.attribute12, 'N') = 'N'
      --and b.HEADER_ID=p_header_id
       and cpd.req_header_id = p_header_id
       and cpd.cux_pick_line_id = b.LINE_ID;
      /*and a.header_id = p_header_id*/
      --modified on 20151020
    
    else
    
      for y in c_21_old loop
      
        if y.QUANTITY < 0 then
          p_txt := y.segment1 || '-处理量小于零';
          raise form_trigger_failure;
        end if;
      end loop;
    
      ---判断硬件数量是否大于零
      select sum(b.ISSUE_QUANTITY) QUANTITY
        into v_qty_yj
        from /*cux_oe_ship_headers_v   a,*/ --modified on 20151020
             /* cux_oe_ship_lines_v     b,*/ --modified on 20151021
              cux.cux_oe_ship_lines b,
             --cux.cux_pick_sub_doc    cpd,
             --cux_om_line_interface_v c,
             mtl_system_items_b msi
       where /*a.header_id = b.HEADER_ID--modified on 20151020
                                                                                                                                               and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
      --and c.ORDER_LINE_ID = b.OE_LINE_ID
       and b.FLAG = 'Y'
       and msi.INVENTORY_ITEM_FLAG = 'Y'
       and nvl(msi.attribute12, 'N') = 'N'
      --and b.HEADER_ID=p_header_id
       and b.header_id = p_header_id;
      --and cpd.cux_pick_line_id = b.LINE_ID;
      /*and a.header_id = p_header_id*/
      --modified on 20151020
    
    end if;
  
    if v_qty_yj <= 0 then
      p_txt := '硬件处理量小于等于零';
      raise form_trigger_failure;
    end if;
  
    if (V_count_req > 0) then
    
      for y in c_2_new loop
      
        --if p_ORGANIZATION_ID not in (85,86) then
        select decode(col.SOURCES_OF_PRODUCTS, 'FW', 84, 'JW', 83)
          into V_SHIP_FROM_ORG_ID
          from cux_om_line_interface_v col
         where col.ORDER_LINE_ID = y.oe_line_id;
        if y.software_flag = 'Y' then
          /*        select flv.attribute2
           into v_pick_from_subinventory1
           from fnd_lookup_values flv
          where flv.lookup_type = 'RJ_WIP_ISSUE_INVENTORY'
            AND FLV.DESCRIPTION = y.SEGMENT1
            and flv.language = 'ZHS'
            AND FLV.ATTRIBUTE1 = V_SHIP_FROM_ORG_ID
            AND FLV.ATTRIBUTE2 IS NOT NULL;*/
          select decode(V_SHIP_FROM_ORG_ID, 84, 'G999', 'H999')
            into v_pick_from_subinventory1
            from dual;
        else
          select decode(V_SHIP_FROM_ORG_ID,
                        84,
                        'G' || SUBSTR(Y.PICK_FROM_SUBINVENTORY, 2),
                        'H' || SUBSTR(Y.PICK_FROM_SUBINVENTORY, 2))
            into v_pick_from_subinventory1
            from dual; --commentted by bruce on 20150804
          -- v_pick_from_subinventory1 := y.pick_from_subinventory;
        end if;
      
        select count(*)
          into v_count
          from wsh_delivery_details wdd
         where wdd.delivery_detail_id = y.delivery_detail_id
           and wdd.released_status in ('B');
        if v_count > 0 then
          p_txt := '状态是延交';
          raise form_trigger_failure;
        end if;
      
        if y.ATTRIBUTE13 = 'N' then
          inv_quantity_tree_pub.clear_quantity_cache;
          INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                                 P_INIT_MSG_LST        => NULL,
                                                 X_RETURN_STATUS       => L_RETURN_STATUS,
                                                 X_MSG_COUNT           => L_MSG_COUNT,
                                                 X_MSG_DATA            => L_MSG_DATA,
                                                 P_ORGANIZATION_ID     => V_SHIP_FROM_ORG_ID, --库存组织ID
                                                 P_INVENTORY_ITEM_ID   => y.inventory_item_id, --物料ID
                                                 P_TREE_MODE           => 3,
                                                 P_IS_REVISION_CONTROL => FALSE,
                                                 P_IS_LOT_CONTROL      => FALSE,
                                                 P_IS_SERIAL_CONTROL   => FALSE,
                                                 P_REVISION            => NULL,
                                                 P_LOT_NUMBER          => NULL,
                                                 P_LOT_EXPIRATION_DATE => NULL,
                                                 P_SUBINVENTORY_CODE   => v_pick_from_subinventory1, --子库code
                                                 P_LOCATOR_ID          => y.LOCATOR_ID,
                                                 P_COST_GROUP_ID       => NULL,
                                                 P_ONHAND_SOURCE       => 3,
                                                 X_QOH                 => v_L_QOH, --现有量
                                                 X_RQOH                => v_L_RQOH,
                                                 X_QR                  => v_L_QR,
                                                 X_QS                  => v_L_QS,
                                                 X_ATT                 => v_L_ATT,
                                                 X_ATR                 => v_L_ATR);
          if nvl(v_L_ATT, 0) < y.QUANTITY then
            p_txt := y.segment1 || '-现有量不足,仓库' || v_pick_from_subinventory1;
            raise form_trigger_failure;
          end if;
        end if;
      
        select decode(H.ORG_ID, 81, 84, 82, 83)
          into V_ORG_ID
          from oe_order_headers_all h
         where h.header_id = y.oe_header_id;
      
        --判断是否是三角贸易
        if V_SHIP_FROM_ORG_ID = V_ORG_ID then
          p_txt := '物料:' || y.segment1 || '不是三角贸易的物料';
          raise form_trigger_failure;
        end if;
      end loop;
    
    else
      --count req
      for y in c_2_old loop
      
        --if p_ORGANIZATION_ID not in (85,86) then
        select decode(col.SOURCES_OF_PRODUCTS, 'FW', 84, 'JW', 83)
          into V_SHIP_FROM_ORG_ID
          from cux_om_line_interface_v col
         where col.ORDER_LINE_ID = y.oe_line_id;
        if y.software_flag = 'Y' then
          /*        select flv.attribute2
           into v_pick_from_subinventory1
           from fnd_lookup_values flv
          where flv.lookup_type = 'RJ_WIP_ISSUE_INVENTORY'
            AND FLV.DESCRIPTION = y.SEGMENT1
            and flv.language = 'ZHS'
            AND FLV.ATTRIBUTE1 = V_SHIP_FROM_ORG_ID
            AND FLV.ATTRIBUTE2 IS NOT NULL;*/
          select decode(V_SHIP_FROM_ORG_ID, 84, 'G999', 'H999')
            into v_pick_from_subinventory1
            from dual;
        else
          select decode(V_SHIP_FROM_ORG_ID,
                        84,
                        'G' || SUBSTR(Y.PICK_FROM_SUBINVENTORY, 2),
                        'H' || SUBSTR(Y.PICK_FROM_SUBINVENTORY, 2))
            into v_pick_from_subinventory1
            from dual; --commentted by bruce on 20150804
          -- v_pick_from_subinventory1 := y.pick_from_subinventory;
        end if;
      
        select count(*)
          into v_count
          from wsh_delivery_details wdd
         where wdd.delivery_detail_id = y.delivery_detail_id
           and wdd.released_status in ('B');
        if v_count > 0 then
          p_txt := '状态是延交';
          raise form_trigger_failure;
        end if;
      
        if y.ATTRIBUTE13 = 'N' then
          inv_quantity_tree_pub.clear_quantity_cache;
          INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                                 P_INIT_MSG_LST        => NULL,
                                                 X_RETURN_STATUS       => L_RETURN_STATUS,
                                                 X_MSG_COUNT           => L_MSG_COUNT,
                                                 X_MSG_DATA            => L_MSG_DATA,
                                                 P_ORGANIZATION_ID     => V_SHIP_FROM_ORG_ID, --库存组织ID
                                                 P_INVENTORY_ITEM_ID   => y.inventory_item_id, --物料ID
                                                 P_TREE_MODE           => 3,
                                                 P_IS_REVISION_CONTROL => FALSE,
                                                 P_IS_LOT_CONTROL      => FALSE,
                                                 P_IS_SERIAL_CONTROL   => FALSE,
                                                 P_REVISION            => NULL,
                                                 P_LOT_NUMBER          => NULL,
                                                 P_LOT_EXPIRATION_DATE => NULL,
                                                 P_SUBINVENTORY_CODE   => v_pick_from_subinventory1, --子库code
                                                 P_LOCATOR_ID          => y.LOCATOR_ID,
                                                 P_COST_GROUP_ID       => NULL,
                                                 P_ONHAND_SOURCE       => 3,
                                                 X_QOH                 => v_L_QOH, --现有量
                                                 X_RQOH                => v_L_RQOH,
                                                 X_QR                  => v_L_QR,
                                                 X_QS                  => v_L_QS,
                                                 X_ATT                 => v_L_ATT,
                                                 X_ATR                 => v_L_ATR);
          if nvl(v_L_ATT, 0) < y.QUANTITY then
            p_txt := y.segment1 || '-现有量不足,仓库' || v_pick_from_subinventory1;
            raise form_trigger_failure;
          end if;
        end if;
      
        select decode(H.ORG_ID, 81, 84, 82, 83)
          into V_ORG_ID
          from oe_order_headers_all h
         where h.header_id = y.oe_header_id;
      
        --判断是否是三角贸易
        if V_SHIP_FROM_ORG_ID = V_ORG_ID then
          p_txt := '物料:' || y.segment1 || '不是三角贸易的物料';
          raise form_trigger_failure;
        end if;
      end loop;
    end if;
  
    ----------------------------------------------通过验证-----------------------------------------
  
    select count(1)
      into v_count_hdr
      from cux_pick_sub_doc cpd
     where cpd.req_header_id = p_header_id
       AND NOT EXISTS (SELECT 1
              FROM CUX.CUX_OE_SHIP_HEADERS COH
             WHERE COH.HEADER_ID = P_HEADER_ID);
  
    if (v_count_hdr = 0) then
      v_header_id := p_header_id;
    else
      select cpd.cux_pick_header_id
        into v_header_id
        from cux_pick_sub_doc cpd
       where cpd.req_header_id = p_header_id
         and rownum = 1;
    end if;
  
    if (V_count_req > 0) then
      FOR Z IN C_2_new LOOP
      
        DELETE FROM CUX.CUX_OE_SHIP_QTY_TMP WHERE LINE_ID = Z.LINE_ID;
        --获取对应的库存组织
        SELECT DECODE(COL.SOURCES_OF_PRODUCTS, 'FW', 84, 'JW', 83)
          INTO V_PRODUCTS_OU_ID
          FROM CUX_OM_LINE_INTERFACE_V COL
         WHERE COL.ORDER_LINE_ID = Z.OE_LINE_ID;
        --DELETE FROM CUX.CUX_OE_SHIP_QTY_TMP WHERE LINE_ID = Z.LINE_ID;--commentted by bruce on 20150804
        --判断行现在的阶段
        IF Z.ATTRIBUTE13 = 'N' THEN
          IF V_PRODUCTS_OU_ID = 83 THEN
            --JW的三角贸易
            --83组织杂发
            --判断对应的仓库是否启用货位管理
            --判断是否软件
            IF Z.SOFTWARE_FLAG = 'Y' THEN
              V_SUBINVENTORY_CODE := 'H999';
              --开始匹配库存
              INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                (LINE_ID,
                 ORGANIZATION_ID,
                 SUBINVENTORY_CODE,
                 LOCATOR_ID,
                 QTY)
              VALUES
                (Z.LINE_ID,
                 V_PRODUCTS_OU_ID,
                 V_SUBINVENTORY_CODE,
                 NULL,
                 Z.QUANTITY);
            ELSE
              --非软件
              V_SUBINVENTORY_CODE := 'H' ||
                                     SUBSTR(Z.PICK_FROM_SUBINVENTORY, 2);
              --V_SUBINVENTORY_CODE := z.PICK_FROM_SUBINVENTORY;
            
              IF (z.locator_id > 0) THEN
                --ADDED BY BRUCE ON 20150804
              
                --前面已验证这个货位的可用量足够
                INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                  (LINE_ID,
                   ORGANIZATION_ID,
                   SUBINVENTORY_CODE,
                   LOCATOR_ID,
                   QTY)
                VALUES
                  (Z.LINE_ID,
                   V_PRODUCTS_OU_ID,
                   V_SUBINVENTORY_CODE,
                   z.locator_id,
                   z.QUANTITY);
              
              else
              
                SELECT COUNT(*)
                  INTO V_COUNT
                  FROM MTL_ITEM_LOCATIONS MIL
                 WHERE MIL.ORGANIZATION_ID = V_PRODUCTS_OU_ID
                   AND MIL.SUBINVENTORY_CODE = V_SUBINVENTORY_CODE;
                IF V_COUNT > 0 THEN
                  V_XQ_QTY := Z.QUANTITY;
                  FOR W IN (SELECT MOQ.LOCATOR_ID,
                                   SUM(MOQ.TRANSACTION_QUANTITY) QTY
                              FROM MTL_ONHAND_QUANTITIES MOQ /*,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 MTL_ITEM_LOCATIONS    MIL*/
                             WHERE MOQ.ORGANIZATION_ID = V_PRODUCTS_OU_ID
                               AND MOQ.INVENTORY_ITEM_ID =
                                   Z.INVENTORY_ITEM_ID
                               AND MOQ.SUBINVENTORY_CODE =
                                   V_SUBINVENTORY_CODE
                            /*AND MIL.INVENTORY_LOCATION_ID = MOQ.LOCATOR_ID
                            AND MIL.SUBINVENTORY_CODE =
                                MOQ.SUBINVENTORY_CODE
                            AND MIL.ORGANIZATION_ID = MOQ.ORGANIZATION_ID*/ --commentted by bruce on 20150804
                             GROUP BY MOQ.LOCATOR_ID
                            /*ORDER BY MOQ.LOCATOR_ID DESC*/
                            /*ORDER BY DECODE(MIL.SEGMENT1,'WJ',1,'ZK',2,3)*/
                            ) LOOP
                    --get att         
                    select nvl(sum(mr.reservation_quantity), 0)
                      into v_reserved_qty
                      from mtl_reservations mr
                     where mr.inventory_item_id = z.INVENTORY_ITEM_ID
                       and mr.subinventory_code = V_SUBINVENTORY_CODE
                       and mr.organization_id = V_PRODUCTS_OU_ID
                       and mr.locator_id = W.LOCATOR_ID;
                  
                    v_locator_att := W.QTY - v_reserved_qty; --added by bruce on 20150804
                  
                    IF V_XQ_QTY <= v_locator_att THEN
                      INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                        (LINE_ID,
                         ORGANIZATION_ID,
                         SUBINVENTORY_CODE,
                         LOCATOR_ID,
                         QTY)
                      VALUES
                        (Z.LINE_ID,
                         V_PRODUCTS_OU_ID,
                         V_SUBINVENTORY_CODE,
                         W.LOCATOR_ID,
                         V_XQ_QTY);
                      V_XQ_QTY := 0;
                    ELSE
                      INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                        (LINE_ID,
                         ORGANIZATION_ID,
                         SUBINVENTORY_CODE,
                         LOCATOR_ID,
                         QTY)
                      VALUES
                        (Z.LINE_ID,
                         V_PRODUCTS_OU_ID,
                         V_SUBINVENTORY_CODE,
                         W.LOCATOR_ID,
                         /*W.QTY*/
                         v_locator_att --modified by bruce on 20150804
                         );
                      V_XQ_QTY := V_XQ_QTY - /*W.QTY*/
                                  v_locator_att; --modified by bruce on 20150804
                    END IF;
                  END LOOP;
                ELSE
                  --开始匹配库存
                  INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                    (LINE_ID,
                     ORGANIZATION_ID,
                     SUBINVENTORY_CODE,
                     LOCATOR_ID,
                     QTY)
                  VALUES
                    (Z.LINE_ID,
                     V_PRODUCTS_OU_ID,
                     V_SUBINVENTORY_CODE,
                     NULL,
                     Z.QUANTITY);
                END IF;
              
              end if; --judge if there is locator specified
            
            END IF;
            --开始杂项事务处理
            --83组织开始杂发
            FOR W IN (SELECT *
                        FROM CUX.CUX_OE_SHIP_QTY_TMP A
                       WHERE A.LINE_ID = Z.LINE_ID
                         AND A.QTY > 0) LOOP
              SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
                INTO V_TXN_ID
                FROM DUAL;
            
              L_IFACE_REC.TRANSACTION_INTERFACE_ID   := V_TXN_ID;
              L_IFACE_REC.TRANSACTION_MODE           := 3; --BACKGROUND
              L_IFACE_REC.PROCESS_FLAG               := 1; --TO BE PROCESSED    
              L_IFACE_REC.ORGANIZATION_ID            := 83;
              L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL;
              L_IFACE_REC.INVENTORY_ITEM_ID          := Z.INVENTORY_ITEM_ID;
              L_IFACE_REC.SUBINVENTORY_CODE          := W.SUBINVENTORY_CODE;
              L_IFACE_REC.LOCATOR_ID                 := W.LOCATOR_ID;
              L_IFACE_REC.TRANSACTION_QUANTITY       := -W.QTY;
              L_IFACE_REC.PRIMARY_QUANTITY           := -W.QTY;
              L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
              L_IFACE_REC.TRANSACTION_COST           := NULL;
              L_IFACE_REC.TRANSACTION_DATE           := Z.SHIP_DATE;
              L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
              L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
              L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂发';
              L_IFACE_REC.TRANSACTION_REFERENCE      := 'CUX.三角贸易杂发,单号:' ||
                                                        z.request_number;
              L_IFACE_REC.SOURCE_HEADER_ID           := Z.HEADER_ID; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
              L_IFACE_REC.SOURCE_LINE_ID             := Z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
              L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
              L_IFACE_REC.TRANSACTION_TYPE_ID        := 1; --帐户发放          
              L_IFACE_REC.TRANSACTION_SOURCE_ID      := 1032; --发出商品科目
              L_IFACE_REC.LAST_UPDATE_DATE           := SYSDATE;
              L_IFACE_REC.LAST_UPDATED_BY            := FND_GLOBAL.USER_ID;
              L_IFACE_REC.CREATION_DATE              := SYSDATE;
              L_IFACE_REC.CREATED_BY                 := FND_GLOBAL.USER_ID;
              L_IFACE_REC.LAST_UPDATE_LOGIN          := FND_GLOBAL.LOGIN_ID;
              L_IFACE_REC.TRANSACTION_HEADER_ID      := V_TXN_ID;
              L_IFACE_REC.ATTRIBUTE1                 := Z.OE_HEADER_ID;
              L_IFACE_REC.ATTRIBUTE2                 := Z.OE_LINE_ID;
              L_IFACE_REC.ATTRIBUTE3                 := 'Y';
              L_IFACE_REC.ATTRIBUTE4                 := Z.ATTRIBUTE2;
              L_IFACE_REC.ATTRIBUTE5                 := V_HEADER_ID;
              INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
              L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                                   P_INIT_MSG_LIST    => 'F',
                                                                   P_VALIDATION_LEVEL => 100,
                                                                   P_COMMIT           => 'T',
                                                                   X_RETURN_STATUS    => L_RETURN_STATUS,
                                                                   X_MSG_COUNT        => L_MSG_COUNT,
                                                                   X_MSG_DATA         => L_MSG_DATA,
                                                                   X_TRANS_COUNT      => L_TRANS_COUNT,
                                                                   P_HEADER_ID        => V_TXN_ID);
              IF L_RESULT = 0 THEN
                NULL;
              ELSE
                FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                            FROM MTL_TRANSACTIONS_INTERFACE
                           WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
                  V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                                  I.ERROR_EXPLANATION;
                END LOOP;
                p_txt := '83杂出,物料:' || Z.SEGMENT1 || V_ERROR_CODE;
                ROLLBACK;
                RAISE form_trigger_failure;
              END IF;
            END LOOP;
            -----84组织杂入
            --获取价格
            SELECT OOLA.UNIT_SELLING_PRICE,
                   OOHA.TRANSACTIONAL_CURR_CODE,
                   OOLA.TAX_CODE
              INTO V_UNIT_SELLING_PRICE,
                   V_TRANSACTIONAL_CURR_CODE,
                   V_TAX_CODE
              FROM OE_ORDER_LINES_ALL OOLA, OE_ORDER_HEADERS_ALL OOHA
             WHERE OOLA.HEADER_ID = OOHA.HEADER_ID
               AND OOLA.LINE_ID = Z.OE_LINE_ID;
            BEGIN
              SELECT C.PERCENTAGE_RATE
                INTO V_PERCENTAGE_RATE
                FROM ZX_RATES_VL C
               WHERE C.TAX_RATE_CODE = V_TAX_CODE;
            EXCEPTION
              WHEN OTHERS THEN
                V_PERCENTAGE_RATE := 0;
            END;
          
            IF V_TRANSACTIONAL_CURR_CODE <> 'CNY' THEN
            
              SELECT GDR.CONVERSION_RATE
                INTO V_CONVERSION_RATE
                FROM GL_DAILY_RATES GDR
               WHERE GDR.FROM_CURRENCY = V_TRANSACTIONAL_CURR_CODE
                 AND GDR.TO_CURRENCY = 'CNY'
                 AND GDR.CONVERSION_TYPE = 'Corporate'
                 AND GDR.CONVERSION_DATE = trunc(SYSDATE);
              V_UNIT_SELLING_PRICE := V_UNIT_SELLING_PRICE *
                                      V_CONVERSION_RATE;
            END IF;
          
            V_UNIT_SELLING_PRICE := ROUND(V_UNIT_SELLING_PRICE * 0.9 /
                                          (1 + V_PERCENTAGE_RATE / 100),
                                          6);
            ---------仓库货位判断
            IF Z.SOFTWARE_FLAG = 'Y' THEN
              V_SUBINVENTORY_CODE_R := 'G999';
              V_LOCATION_ID_R       := NULL;
            ELSE
            
              V_SUBINVENTORY_CODE_R := 'G' ||
                                       SUBSTR(Z.PICK_FROM_SUBINVENTORY, 2); --commentted by bruce on 20150804
            
              IF V_SUBINVENTORY_CODE_R = 'G169' THEN
                SELECT MAX(MIL.INVENTORY_LOCATION_ID)
                  INTO V_LOCATION_ID_R
                  FROM MTL_ITEM_LOCATIONS MIL
                 WHERE MIL.ORGANIZATION_ID = 84
                   AND MIL.SUBINVENTORY_CODE = V_SUBINVENTORY_CODE_R
                   AND MIL.SEGMENT1 = 'WJ';
              ELSE
                BEGIN
                  SELECT MAX(MIL.INVENTORY_LOCATION_ID)
                    INTO V_LOCATION_ID_R
                    FROM MTL_ITEM_LOCATIONS MIL
                   WHERE MIL.ORGANIZATION_ID = 84
                     AND MIL.SUBINVENTORY_CODE = V_SUBINVENTORY_CODE_R;
                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    V_SUBINVENTORY_CODE_R := 'G151';
                    V_LOCATION_ID_R       := NULL;
                END;
              END IF;
            END IF;
          
            SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
              INTO V_TXN_ID
              FROM DUAL;
          
            L_IFACE_REC.TRANSACTION_INTERFACE_ID   := V_TXN_ID;
            L_IFACE_REC.TRANSACTION_MODE           := 3; --BACKGROUND
            L_IFACE_REC.PROCESS_FLAG               := 1; --TO BE PROCESSED    
            L_IFACE_REC.ORGANIZATION_ID            := 84;
            L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL;
            L_IFACE_REC.INVENTORY_ITEM_ID          := Z.INVENTORY_ITEM_ID;
            L_IFACE_REC.SUBINVENTORY_CODE          := V_SUBINVENTORY_CODE_R;
            L_IFACE_REC.LOCATOR_ID                 := V_LOCATION_ID_R;
            L_IFACE_REC.TRANSACTION_QUANTITY       := Z.QUANTITY;
            L_IFACE_REC.PRIMARY_QUANTITY           := Z.QUANTITY;
            L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
            L_IFACE_REC.TRANSACTION_COST           := V_UNIT_SELLING_PRICE;
            L_IFACE_REC.TRANSACTION_DATE           := Z.SHIP_DATE;
            L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
            L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
            L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂入';
            L_IFACE_REC.TRANSACTION_REFERENCE      := 'CUX.三角贸易杂入,单号:' ||
                                                      Z.REQUEST_NUMBER;
            L_IFACE_REC.SOURCE_HEADER_ID           := Z.HEADER_ID; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
            L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
            L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
            L_IFACE_REC.TRANSACTION_TYPE_ID        := 40; --帐户接收 
          
            L_IFACE_REC.TRANSACTION_SOURCE_ID := 1022; --应付暂估带往来
          
            L_IFACE_REC.LAST_UPDATE_DATE      := SYSDATE;
            L_IFACE_REC.LAST_UPDATED_BY       := FND_GLOBAL.USER_ID;
            L_IFACE_REC.CREATION_DATE         := SYSDATE;
            L_IFACE_REC.CREATED_BY            := FND_GLOBAL.USER_ID;
            L_IFACE_REC.LAST_UPDATE_LOGIN     := FND_GLOBAL.LOGIN_ID;
            L_IFACE_REC.TRANSACTION_HEADER_ID := V_TXN_ID;
            L_IFACE_REC.ATTRIBUTE1            := Z.OE_HEADER_ID;
            L_IFACE_REC.ATTRIBUTE2            := Z.OE_LINE_ID;
            L_IFACE_REC.ATTRIBUTE3            := 'Y';
            L_IFACE_REC.ATTRIBUTE4            := Z.ATTRIBUTE2;
            L_IFACE_REC.ATTRIBUTE5            := V_HEADER_ID;
            INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
            L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                                 P_INIT_MSG_LIST    => 'F',
                                                                 P_VALIDATION_LEVEL => 100,
                                                                 P_COMMIT           => 'T',
                                                                 X_RETURN_STATUS    => L_RETURN_STATUS,
                                                                 X_MSG_COUNT        => L_MSG_COUNT,
                                                                 X_MSG_DATA         => L_MSG_DATA,
                                                                 X_TRANS_COUNT      => L_TRANS_COUNT,
                                                                 P_HEADER_ID        => V_TXN_ID);
            IF L_RESULT = 0 THEN
              NULL;
            ELSE
              FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                          FROM MTL_TRANSACTIONS_INTERFACE
                         WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
                V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                                I.ERROR_EXPLANATION;
              END LOOP;
              P_TXT := '84杂入,物料:' || Z.SEGMENT1 || V_ERROR_CODE;
              ROLLBACK;
              RAISE FORM_TRIGGER_FAILURE;
            END IF;
            ---开始标记状态
            UPDATE CUX.CUX_OE_SHIP_LINES B
               SET B.Attribute13 = 'INV'
             WHERE B.LINE_ID = Z.LINE_ID;
          ELSE
            ---FW的三角贸易
            --84组织杂发
            --判断对应的仓库是否启用货位管理
            --判断是否软件
            IF Z.SOFTWARE_FLAG = 'Y' THEN
              V_SUBINVENTORY_CODE := 'G999';
              --开始匹配库存
              INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                (LINE_ID,
                 ORGANIZATION_ID,
                 SUBINVENTORY_CODE,
                 LOCATOR_ID,
                 QTY)
              VALUES
                (Z.LINE_ID,
                 V_PRODUCTS_OU_ID,
                 V_SUBINVENTORY_CODE,
                 NULL,
                 Z.QUANTITY);
            ELSE
              --非软件
              V_SUBINVENTORY_CODE := 'G' ||
                                     SUBSTR(Z.PICK_FROM_SUBINVENTORY, 2); --commentted by bruce on 20150804
            
              --V_SUBINVENTORY_CODE := z.PICK_FROM_SUBINVENTORY;
            
              IF (z.locator_id > 0) THEN
                --ADDED BY BRUCE ON 20150804
              
                INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                  (LINE_ID,
                   ORGANIZATION_ID,
                   SUBINVENTORY_CODE,
                   LOCATOR_ID,
                   QTY)
                VALUES
                  (Z.LINE_ID,
                   V_PRODUCTS_OU_ID,
                   V_SUBINVENTORY_CODE,
                   z.locator_id,
                   z.QUANTITY);
              else
              
                SELECT COUNT(*)
                  INTO V_COUNT
                  FROM MTL_ITEM_LOCATIONS MIL
                 WHERE MIL.ORGANIZATION_ID = V_PRODUCTS_OU_ID
                   AND MIL.SUBINVENTORY_CODE = V_SUBINVENTORY_CODE;
                IF V_COUNT > 0 THEN
                  V_XQ_QTY := Z.QUANTITY;
                  FOR W IN (SELECT MOQ.LOCATOR_ID,
                                   SUM(MOQ.TRANSACTION_QUANTITY) QTY
                              FROM MTL_ONHAND_QUANTITIES MOQ
                             WHERE MOQ.ORGANIZATION_ID = V_PRODUCTS_OU_ID
                               AND MOQ.INVENTORY_ITEM_ID =
                                   Z.INVENTORY_ITEM_ID
                               AND MOQ.SUBINVENTORY_CODE =
                                   V_SUBINVENTORY_CODE
                             GROUP BY MOQ.LOCATOR_ID) LOOP
                  
                    select nvl(sum(mr.reservation_quantity), 0)
                      into v_reserved_qty
                      from mtl_reservations mr
                     where mr.inventory_item_id = z.INVENTORY_ITEM_ID
                       and mr.subinventory_code = V_SUBINVENTORY_CODE
                       and mr.organization_id = V_PRODUCTS_OU_ID
                       and mr.locator_id = W.LOCATOR_ID;
                  
                    v_locator_att := W.QTY - v_reserved_qty; --added by bruce on 20150804          
                  
                    IF V_XQ_QTY <= /*W.QTY*/
                       v_locator_att THEN
                      --modified by bruce on 20150804
                      INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                        (LINE_ID,
                         ORGANIZATION_ID,
                         SUBINVENTORY_CODE,
                         LOCATOR_ID,
                         QTY)
                      VALUES
                        (Z.LINE_ID,
                         V_PRODUCTS_OU_ID,
                         V_SUBINVENTORY_CODE,
                         W.LOCATOR_ID,
                         V_XQ_QTY);
                      V_XQ_QTY := 0;
                    ELSE
                      INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                        (LINE_ID,
                         ORGANIZATION_ID,
                         SUBINVENTORY_CODE,
                         LOCATOR_ID,
                         QTY)
                      VALUES
                        (Z.LINE_ID,
                         V_PRODUCTS_OU_ID,
                         V_SUBINVENTORY_CODE,
                         W.LOCATOR_ID,
                         /*W.QTY*/
                         v_locator_att ----modified by bruce on 20150804
                         );
                      V_XQ_QTY := V_XQ_QTY - v_locator_att; --modified by bruce on 20150804;
                    END IF;
                  END LOOP;
                ELSE
                  --开始匹配库存
                  INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                    (LINE_ID,
                     ORGANIZATION_ID,
                     SUBINVENTORY_CODE,
                     LOCATOR_ID,
                     QTY)
                  VALUES
                    (Z.LINE_ID,
                     V_PRODUCTS_OU_ID,
                     V_SUBINVENTORY_CODE,
                     NULL,
                     Z.QUANTITY);
                END IF;
              end if;
            END IF;
            --开始杂项事务处理
            --84组织开始杂发
            FOR W IN (SELECT *
                        FROM CUX.CUX_OE_SHIP_QTY_TMP A
                       WHERE A.LINE_ID = Z.LINE_ID
                         AND A.QTY > 0) LOOP
              SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
                INTO V_TXN_ID
                FROM DUAL;
            
              L_IFACE_REC.TRANSACTION_INTERFACE_ID   := V_TXN_ID;
              L_IFACE_REC.TRANSACTION_MODE           := 3; --BACKGROUND
              L_IFACE_REC.PROCESS_FLAG               := 1; --TO BE PROCESSED    
              L_IFACE_REC.ORGANIZATION_ID            := 84;
              L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL;
              L_IFACE_REC.INVENTORY_ITEM_ID          := Z.INVENTORY_ITEM_ID;
              L_IFACE_REC.SUBINVENTORY_CODE          := W.SUBINVENTORY_CODE;
              L_IFACE_REC.LOCATOR_ID                 := W.LOCATOR_ID;
              L_IFACE_REC.TRANSACTION_QUANTITY       := -W.QTY;
              L_IFACE_REC.PRIMARY_QUANTITY           := -W.QTY;
              L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
              L_IFACE_REC.TRANSACTION_COST           := NULL;
              L_IFACE_REC.TRANSACTION_DATE           := Z.SHIP_DATE;
              L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
              L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
              L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂发';
              L_IFACE_REC.TRANSACTION_REFERENCE      := 'CUX.三角贸易杂发,单号:' ||
                                                        z.request_number;
              L_IFACE_REC.SOURCE_HEADER_ID           := Z.HEADER_ID; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
              L_IFACE_REC.SOURCE_LINE_ID             := Z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
              L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
              L_IFACE_REC.TRANSACTION_TYPE_ID        := 1; --帐户发放          
              L_IFACE_REC.TRANSACTION_SOURCE_ID      := 1025; --发出商品科目
              L_IFACE_REC.LAST_UPDATE_DATE           := SYSDATE;
              L_IFACE_REC.LAST_UPDATED_BY            := FND_GLOBAL.USER_ID;
              L_IFACE_REC.CREATION_DATE              := SYSDATE;
              L_IFACE_REC.CREATED_BY                 := FND_GLOBAL.USER_ID;
              L_IFACE_REC.LAST_UPDATE_LOGIN          := FND_GLOBAL.LOGIN_ID;
              L_IFACE_REC.TRANSACTION_HEADER_ID      := V_TXN_ID;
              L_IFACE_REC.ATTRIBUTE1                 := Z.OE_HEADER_ID;
              L_IFACE_REC.ATTRIBUTE2                 := Z.OE_LINE_ID;
              L_IFACE_REC.ATTRIBUTE3                 := 'Y';
              L_IFACE_REC.ATTRIBUTE4                 := Z.ATTRIBUTE2;
              L_IFACE_REC.ATTRIBUTE5                 := V_HEADER_ID;
              INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
              L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                                   P_INIT_MSG_LIST    => 'F',
                                                                   P_VALIDATION_LEVEL => 100,
                                                                   P_COMMIT           => 'T',
                                                                   X_RETURN_STATUS    => L_RETURN_STATUS,
                                                                   X_MSG_COUNT        => L_MSG_COUNT,
                                                                   X_MSG_DATA         => L_MSG_DATA,
                                                                   X_TRANS_COUNT      => L_TRANS_COUNT,
                                                                   P_HEADER_ID        => V_TXN_ID);
              IF L_RESULT = 0 THEN
                NULL;
              ELSE
                FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                            FROM MTL_TRANSACTIONS_INTERFACE
                           WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
                  V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                                  I.ERROR_EXPLANATION;
                END LOOP;
                p_txt := '84杂出,物料:' || Z.SEGMENT1 || V_ERROR_CODE;
                ROLLBACK;
                RAISE form_trigger_failure;
              END IF;
            END LOOP;
            -----83组织杂入
            --获取价格
            SELECT OOLA.UNIT_SELLING_PRICE,
                   OOHA.TRANSACTIONAL_CURR_CODE,
                   OOLA.TAX_CODE
              INTO V_UNIT_SELLING_PRICE,
                   V_TRANSACTIONAL_CURR_CODE,
                   V_TAX_CODE
              FROM OE_ORDER_LINES_ALL OOLA, OE_ORDER_HEADERS_ALL OOHA
             WHERE OOLA.HEADER_ID = OOHA.HEADER_ID
               AND OOLA.LINE_ID = Z.OE_LINE_ID;
            BEGIN
              SELECT C.PERCENTAGE_RATE
                INTO V_PERCENTAGE_RATE
                FROM ZX_RATES_VL C
               WHERE C.TAX_RATE_CODE = V_TAX_CODE;
            EXCEPTION
              WHEN OTHERS THEN
                V_PERCENTAGE_RATE := 0;
            END;
          
            IF V_TRANSACTIONAL_CURR_CODE <> 'CNY' THEN
            
              SELECT GDR.CONVERSION_RATE
                INTO V_CONVERSION_RATE
                FROM GL_DAILY_RATES GDR
               WHERE GDR.FROM_CURRENCY = V_TRANSACTIONAL_CURR_CODE
                 AND GDR.TO_CURRENCY = 'CNY'
                 AND GDR.CONVERSION_TYPE = 'Corporate'
                 AND GDR.CONVERSION_DATE = trunc(SYSDATE);
              V_UNIT_SELLING_PRICE := V_UNIT_SELLING_PRICE *
                                      V_CONVERSION_RATE;
            END IF;
          
            V_UNIT_SELLING_PRICE := ROUND(V_UNIT_SELLING_PRICE * 0.9 /
                                          (1 + V_PERCENTAGE_RATE / 100),
                                          6);
            ---------仓库货位判断
            IF Z.SOFTWARE_FLAG = 'Y' THEN
              V_SUBINVENTORY_CODE_R := 'H999';
              V_LOCATION_ID_R       := NULL;
            ELSE
            
              /* if (SUBSTR(Z.PICK_FROM_SUBINVENTORY, 2) = 169) then
                
                  V_SUBINVENTORY_CODE_R := 'H169';
                else
                  V_SUBINVENTORY_CODE_R := 'H151';
                end if;
              */
              V_SUBINVENTORY_CODE_R := 'H' ||
                                       SUBSTR(Z.PICK_FROM_SUBINVENTORY, 2); --commentted by bruce on 20150804
            
              IF V_SUBINVENTORY_CODE_R = 'H169' THEN
                SELECT MAX(MIL.INVENTORY_LOCATION_ID)
                  INTO V_LOCATION_ID_R
                  FROM MTL_ITEM_LOCATIONS MIL
                 WHERE MIL.ORGANIZATION_ID = 83
                   AND MIL.SUBINVENTORY_CODE = V_SUBINVENTORY_CODE_R
                   AND MIL.SEGMENT1 = 'WJ';
              ELSE
                BEGIN
                  SELECT MAX(MIL.INVENTORY_LOCATION_ID)
                    INTO V_LOCATION_ID_R
                    FROM MTL_ITEM_LOCATIONS MIL
                   WHERE MIL.ORGANIZATION_ID = 83
                     AND MIL.SUBINVENTORY_CODE = V_SUBINVENTORY_CODE_R;
                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    V_SUBINVENTORY_CODE_R := 'H151';
                    V_LOCATION_ID_R       := NULL;
                END;
              END IF;
            END IF;
          
            SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
              INTO V_TXN_ID
              FROM DUAL;
          
            L_IFACE_REC.TRANSACTION_INTERFACE_ID   := V_TXN_ID;
            L_IFACE_REC.TRANSACTION_MODE           := 3; --BACKGROUND
            L_IFACE_REC.PROCESS_FLAG               := 1; --TO BE PROCESSED    
            L_IFACE_REC.ORGANIZATION_ID            := 83;
            L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL;
            L_IFACE_REC.INVENTORY_ITEM_ID          := Z.INVENTORY_ITEM_ID;
            L_IFACE_REC.SUBINVENTORY_CODE          := V_SUBINVENTORY_CODE_R;
            L_IFACE_REC.LOCATOR_ID                 := V_LOCATION_ID_R;
            L_IFACE_REC.TRANSACTION_QUANTITY       := Z.QUANTITY;
            L_IFACE_REC.PRIMARY_QUANTITY           := Z.QUANTITY;
            L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
            L_IFACE_REC.TRANSACTION_COST           := V_UNIT_SELLING_PRICE;
            L_IFACE_REC.TRANSACTION_DATE           := Z.SHIP_DATE;
            L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
            L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
            L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂入';
            L_IFACE_REC.TRANSACTION_REFERENCE      := 'CUX.三角贸易杂入,单号:' ||
                                                      Z.REQUEST_NUMBER;
            L_IFACE_REC.SOURCE_HEADER_ID           := Z.HEADER_ID; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
            L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
            L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
            L_IFACE_REC.TRANSACTION_TYPE_ID        := 40; --帐户发放 
          
            L_IFACE_REC.TRANSACTION_SOURCE_ID := 1029; --应付暂估带往来
          
            L_IFACE_REC.LAST_UPDATE_DATE      := SYSDATE;
            L_IFACE_REC.LAST_UPDATED_BY       := FND_GLOBAL.USER_ID;
            L_IFACE_REC.CREATION_DATE         := SYSDATE;
            L_IFACE_REC.CREATED_BY            := FND_GLOBAL.USER_ID;
            L_IFACE_REC.LAST_UPDATE_LOGIN     := FND_GLOBAL.LOGIN_ID;
            L_IFACE_REC.TRANSACTION_HEADER_ID := V_TXN_ID;
            L_IFACE_REC.ATTRIBUTE1            := Z.OE_HEADER_ID;
            L_IFACE_REC.ATTRIBUTE2            := Z.OE_LINE_ID;
            L_IFACE_REC.ATTRIBUTE3            := 'Y';
            L_IFACE_REC.ATTRIBUTE4            := Z.ATTRIBUTE2;
            L_IFACE_REC.ATTRIBUTE5            := V_HEADER_ID;
            INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
            L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                                 P_INIT_MSG_LIST    => 'F',
                                                                 P_VALIDATION_LEVEL => 100,
                                                                 P_COMMIT           => 'T',
                                                                 X_RETURN_STATUS    => L_RETURN_STATUS,
                                                                 X_MSG_COUNT        => L_MSG_COUNT,
                                                                 X_MSG_DATA         => L_MSG_DATA,
                                                                 X_TRANS_COUNT      => L_TRANS_COUNT,
                                                                 P_HEADER_ID        => V_TXN_ID);
            IF L_RESULT = 0 THEN
              NULL;
            ELSE
              FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                          FROM MTL_TRANSACTIONS_INTERFACE
                         WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
                V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                                I.ERROR_EXPLANATION;
              END LOOP;
              P_TXT := '83杂入,物料:' || Z.SEGMENT1 || V_ERROR_CODE;
              ROLLBACK;
              RAISE FORM_TRIGGER_FAILURE;
            END IF;
            ---开始标记状态
            UPDATE CUX.CUX_OE_SHIP_LINES B
               SET B.ATTRIBUTE13 = 'INV'
             WHERE B.LINE_ID = Z.LINE_ID;
          END IF;
        END IF;
      END LOOP;
    else
      --count req
      FOR Z IN C_2_old LOOP
      
        DELETE FROM CUX.CUX_OE_SHIP_QTY_TMP WHERE LINE_ID = Z.LINE_ID;
        --获取对应的库存组织
        SELECT DECODE(COL.SOURCES_OF_PRODUCTS, 'FW', 84, 'JW', 83)
          INTO V_PRODUCTS_OU_ID
          FROM CUX_OM_LINE_INTERFACE_V COL
         WHERE COL.ORDER_LINE_ID = Z.OE_LINE_ID;
        --DELETE FROM CUX.CUX_OE_SHIP_QTY_TMP WHERE LINE_ID = Z.LINE_ID;--commentted by bruce on 20150804
        --判断行现在的阶段
        IF Z.ATTRIBUTE13 = 'N' THEN
          IF V_PRODUCTS_OU_ID = 83 THEN
            --JW的三角贸易
            --83组织杂发
            --判断对应的仓库是否启用货位管理
            --判断是否软件
            IF Z.SOFTWARE_FLAG = 'Y' THEN
              V_SUBINVENTORY_CODE := 'H999';
              --开始匹配库存
              INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                (LINE_ID,
                 ORGANIZATION_ID,
                 SUBINVENTORY_CODE,
                 LOCATOR_ID,
                 QTY)
              VALUES
                (Z.LINE_ID,
                 V_PRODUCTS_OU_ID,
                 V_SUBINVENTORY_CODE,
                 NULL,
                 Z.QUANTITY);
            ELSE
              --非软件
              V_SUBINVENTORY_CODE := 'H' ||
                                     SUBSTR(Z.PICK_FROM_SUBINVENTORY, 2);
              --V_SUBINVENTORY_CODE := z.PICK_FROM_SUBINVENTORY;
            
              IF (z.locator_id > 0) THEN
                --ADDED BY BRUCE ON 20150804
              
                --前面已验证这个货位的可用量足够
                INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                  (LINE_ID,
                   ORGANIZATION_ID,
                   SUBINVENTORY_CODE,
                   LOCATOR_ID,
                   QTY)
                VALUES
                  (Z.LINE_ID,
                   V_PRODUCTS_OU_ID,
                   V_SUBINVENTORY_CODE,
                   z.locator_id,
                   z.QUANTITY);
              
              else
              
                SELECT COUNT(*)
                  INTO V_COUNT
                  FROM MTL_ITEM_LOCATIONS MIL
                 WHERE MIL.ORGANIZATION_ID = V_PRODUCTS_OU_ID
                   AND MIL.SUBINVENTORY_CODE = V_SUBINVENTORY_CODE;
                IF V_COUNT > 0 THEN
                  V_XQ_QTY := Z.QUANTITY;
                  FOR W IN (SELECT MOQ.LOCATOR_ID,
                                   SUM(MOQ.TRANSACTION_QUANTITY) QTY
                              FROM MTL_ONHAND_QUANTITIES MOQ /*,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 MTL_ITEM_LOCATIONS    MIL*/
                             WHERE MOQ.ORGANIZATION_ID = V_PRODUCTS_OU_ID
                               AND MOQ.INVENTORY_ITEM_ID =
                                   Z.INVENTORY_ITEM_ID
                               AND MOQ.SUBINVENTORY_CODE =
                                   V_SUBINVENTORY_CODE
                            /*AND MIL.INVENTORY_LOCATION_ID = MOQ.LOCATOR_ID
                            AND MIL.SUBINVENTORY_CODE =
                                MOQ.SUBINVENTORY_CODE
                            AND MIL.ORGANIZATION_ID = MOQ.ORGANIZATION_ID*/ --commentted by bruce on 20150804
                             GROUP BY MOQ.LOCATOR_ID
                            /*ORDER BY MOQ.LOCATOR_ID DESC*/
                            /*ORDER BY DECODE(MIL.SEGMENT1,'WJ',1,'ZK',2,3)*/
                            ) LOOP
                    --get att         
                    select nvl(sum(mr.reservation_quantity), 0)
                      into v_reserved_qty
                      from mtl_reservations mr
                     where mr.inventory_item_id = z.INVENTORY_ITEM_ID
                       and mr.subinventory_code = V_SUBINVENTORY_CODE
                       and mr.organization_id = V_PRODUCTS_OU_ID
                       and mr.locator_id = W.LOCATOR_ID;
                  
                    v_locator_att := W.QTY - v_reserved_qty; --added by bruce on 20150804
                  
                    IF V_XQ_QTY <= v_locator_att THEN
                      INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                        (LINE_ID,
                         ORGANIZATION_ID,
                         SUBINVENTORY_CODE,
                         LOCATOR_ID,
                         QTY)
                      VALUES
                        (Z.LINE_ID,
                         V_PRODUCTS_OU_ID,
                         V_SUBINVENTORY_CODE,
                         W.LOCATOR_ID,
                         V_XQ_QTY);
                      V_XQ_QTY := 0;
                    ELSE
                      INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                        (LINE_ID,
                         ORGANIZATION_ID,
                         SUBINVENTORY_CODE,
                         LOCATOR_ID,
                         QTY)
                      VALUES
                        (Z.LINE_ID,
                         V_PRODUCTS_OU_ID,
                         V_SUBINVENTORY_CODE,
                         W.LOCATOR_ID,
                         /*W.QTY*/
                         v_locator_att --modified by bruce on 20150804
                         );
                      V_XQ_QTY := V_XQ_QTY - /*W.QTY*/
                                  v_locator_att; --modified by bruce on 20150804
                    END IF;
                  END LOOP;
                ELSE
                  --开始匹配库存
                  INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                    (LINE_ID,
                     ORGANIZATION_ID,
                     SUBINVENTORY_CODE,
                     LOCATOR_ID,
                     QTY)
                  VALUES
                    (Z.LINE_ID,
                     V_PRODUCTS_OU_ID,
                     V_SUBINVENTORY_CODE,
                     NULL,
                     Z.QUANTITY);
                END IF;
              
              end if; --judge if there is locator specified
            
            END IF;
            --开始杂项事务处理
            --83组织开始杂发
            FOR W IN (SELECT *
                        FROM CUX.CUX_OE_SHIP_QTY_TMP A
                       WHERE A.LINE_ID = Z.LINE_ID
                         AND A.QTY > 0) LOOP
              SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
                INTO V_TXN_ID
                FROM DUAL;
            
              L_IFACE_REC.TRANSACTION_INTERFACE_ID   := V_TXN_ID;
              L_IFACE_REC.TRANSACTION_MODE           := 3; --BACKGROUND
              L_IFACE_REC.PROCESS_FLAG               := 1; --TO BE PROCESSED    
              L_IFACE_REC.ORGANIZATION_ID            := 83;
              L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL;
              L_IFACE_REC.INVENTORY_ITEM_ID          := Z.INVENTORY_ITEM_ID;
              L_IFACE_REC.SUBINVENTORY_CODE          := W.SUBINVENTORY_CODE;
              L_IFACE_REC.LOCATOR_ID                 := W.LOCATOR_ID;
              L_IFACE_REC.TRANSACTION_QUANTITY       := -W.QTY;
              L_IFACE_REC.PRIMARY_QUANTITY           := -W.QTY;
              L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
              L_IFACE_REC.TRANSACTION_COST           := NULL;
              L_IFACE_REC.TRANSACTION_DATE           := Z.SHIP_DATE;
              L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
              L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
              L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂发';
              L_IFACE_REC.TRANSACTION_REFERENCE      := 'CUX.三角贸易杂发,单号:' ||
                                                        z.request_number;
              L_IFACE_REC.SOURCE_HEADER_ID           := Z.HEADER_ID; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
              L_IFACE_REC.SOURCE_LINE_ID             := Z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
              L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
              L_IFACE_REC.TRANSACTION_TYPE_ID        := 1; --帐户发放          
              L_IFACE_REC.TRANSACTION_SOURCE_ID      := 1032; --发出商品科目
              L_IFACE_REC.LAST_UPDATE_DATE           := SYSDATE;
              L_IFACE_REC.LAST_UPDATED_BY            := FND_GLOBAL.USER_ID;
              L_IFACE_REC.CREATION_DATE              := SYSDATE;
              L_IFACE_REC.CREATED_BY                 := FND_GLOBAL.USER_ID;
              L_IFACE_REC.LAST_UPDATE_LOGIN          := FND_GLOBAL.LOGIN_ID;
              L_IFACE_REC.TRANSACTION_HEADER_ID      := V_TXN_ID;
              L_IFACE_REC.ATTRIBUTE1                 := Z.OE_HEADER_ID;
              L_IFACE_REC.ATTRIBUTE2                 := Z.OE_LINE_ID;
              L_IFACE_REC.ATTRIBUTE3                 := 'Y';
              L_IFACE_REC.ATTRIBUTE4                 := Z.ATTRIBUTE2;
              L_IFACE_REC.ATTRIBUTE5                 := V_HEADER_ID;
              INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
              L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                                   P_INIT_MSG_LIST    => 'F',
                                                                   P_VALIDATION_LEVEL => 100,
                                                                   P_COMMIT           => 'T',
                                                                   X_RETURN_STATUS    => L_RETURN_STATUS,
                                                                   X_MSG_COUNT        => L_MSG_COUNT,
                                                                   X_MSG_DATA         => L_MSG_DATA,
                                                                   X_TRANS_COUNT      => L_TRANS_COUNT,
                                                                   P_HEADER_ID        => V_TXN_ID);
              IF L_RESULT = 0 THEN
                NULL;
              ELSE
                FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                            FROM MTL_TRANSACTIONS_INTERFACE
                           WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
                  V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                                  I.ERROR_EXPLANATION;
                END LOOP;
                p_txt := '83杂出,物料:' || Z.SEGMENT1 || V_ERROR_CODE;
                ROLLBACK;
                RAISE form_trigger_failure;
              END IF;
            END LOOP;
            -----84组织杂入
            --获取价格
            SELECT OOLA.UNIT_SELLING_PRICE,
                   OOHA.TRANSACTIONAL_CURR_CODE,
                   OOLA.TAX_CODE
              INTO V_UNIT_SELLING_PRICE,
                   V_TRANSACTIONAL_CURR_CODE,
                   V_TAX_CODE
              FROM OE_ORDER_LINES_ALL OOLA, OE_ORDER_HEADERS_ALL OOHA
             WHERE OOLA.HEADER_ID = OOHA.HEADER_ID
               AND OOLA.LINE_ID = Z.OE_LINE_ID;
            BEGIN
              SELECT C.PERCENTAGE_RATE
                INTO V_PERCENTAGE_RATE
                FROM ZX_RATES_VL C
               WHERE C.TAX_RATE_CODE = V_TAX_CODE;
            EXCEPTION
              WHEN OTHERS THEN
                V_PERCENTAGE_RATE := 0;
            END;
          
            IF V_TRANSACTIONAL_CURR_CODE <> 'CNY' THEN
            
              SELECT GDR.CONVERSION_RATE
                INTO V_CONVERSION_RATE
                FROM GL_DAILY_RATES GDR
               WHERE GDR.FROM_CURRENCY = V_TRANSACTIONAL_CURR_CODE
                 AND GDR.TO_CURRENCY = 'CNY'
                 AND GDR.CONVERSION_TYPE = 'Corporate'
                 AND GDR.CONVERSION_DATE = trunc(SYSDATE);
              V_UNIT_SELLING_PRICE := V_UNIT_SELLING_PRICE *
                                      V_CONVERSION_RATE;
            END IF;
          
            V_UNIT_SELLING_PRICE := ROUND(V_UNIT_SELLING_PRICE * 0.9 /
                                          (1 + V_PERCENTAGE_RATE / 100),
                                          6);
            ---------仓库货位判断
            IF Z.SOFTWARE_FLAG = 'Y' THEN
              V_SUBINVENTORY_CODE_R := 'G999';
              V_LOCATION_ID_R       := NULL;
            ELSE
            
              V_SUBINVENTORY_CODE_R := 'G' ||
                                       SUBSTR(Z.PICK_FROM_SUBINVENTORY, 2); --commentted by bruce on 20150804
            
              IF V_SUBINVENTORY_CODE_R = 'G169' THEN
                SELECT MAX(MIL.INVENTORY_LOCATION_ID)
                  INTO V_LOCATION_ID_R
                  FROM MTL_ITEM_LOCATIONS MIL
                 WHERE MIL.ORGANIZATION_ID = 84
                   AND MIL.SUBINVENTORY_CODE = V_SUBINVENTORY_CODE_R
                   AND MIL.SEGMENT1 = 'WJ';
              ELSE
                BEGIN
                  SELECT MAX(MIL.INVENTORY_LOCATION_ID)
                    INTO V_LOCATION_ID_R
                    FROM MTL_ITEM_LOCATIONS MIL
                   WHERE MIL.ORGANIZATION_ID = 84
                     AND MIL.SUBINVENTORY_CODE = V_SUBINVENTORY_CODE_R;
                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    V_SUBINVENTORY_CODE_R := 'G151';
                    V_LOCATION_ID_R       := NULL;
                END;
              END IF;
            END IF;
          
            SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
              INTO V_TXN_ID
              FROM DUAL;
          
            L_IFACE_REC.TRANSACTION_INTERFACE_ID   := V_TXN_ID;
            L_IFACE_REC.TRANSACTION_MODE           := 3; --BACKGROUND
            L_IFACE_REC.PROCESS_FLAG               := 1; --TO BE PROCESSED    
            L_IFACE_REC.ORGANIZATION_ID            := 84;
            L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL;
            L_IFACE_REC.INVENTORY_ITEM_ID          := Z.INVENTORY_ITEM_ID;
            L_IFACE_REC.SUBINVENTORY_CODE          := V_SUBINVENTORY_CODE_R;
            L_IFACE_REC.LOCATOR_ID                 := V_LOCATION_ID_R;
            L_IFACE_REC.TRANSACTION_QUANTITY       := Z.QUANTITY;
            L_IFACE_REC.PRIMARY_QUANTITY           := Z.QUANTITY;
            L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
            L_IFACE_REC.TRANSACTION_COST           := V_UNIT_SELLING_PRICE;
            L_IFACE_REC.TRANSACTION_DATE           := Z.SHIP_DATE;
            L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
            L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
            L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂入';
            L_IFACE_REC.TRANSACTION_REFERENCE      := 'CUX.三角贸易杂入,单号:' ||
                                                      Z.REQUEST_NUMBER;
            L_IFACE_REC.SOURCE_HEADER_ID           := Z.HEADER_ID; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
            L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
            L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
            L_IFACE_REC.TRANSACTION_TYPE_ID        := 40; --帐户接收 
          
            L_IFACE_REC.TRANSACTION_SOURCE_ID := 1022; --应付暂估带往来
          
            L_IFACE_REC.LAST_UPDATE_DATE      := SYSDATE;
            L_IFACE_REC.LAST_UPDATED_BY       := FND_GLOBAL.USER_ID;
            L_IFACE_REC.CREATION_DATE         := SYSDATE;
            L_IFACE_REC.CREATED_BY            := FND_GLOBAL.USER_ID;
            L_IFACE_REC.LAST_UPDATE_LOGIN     := FND_GLOBAL.LOGIN_ID;
            L_IFACE_REC.TRANSACTION_HEADER_ID := V_TXN_ID;
            L_IFACE_REC.ATTRIBUTE1            := Z.OE_HEADER_ID;
            L_IFACE_REC.ATTRIBUTE2            := Z.OE_LINE_ID;
            L_IFACE_REC.ATTRIBUTE3            := 'Y';
            L_IFACE_REC.ATTRIBUTE4            := Z.ATTRIBUTE2;
            L_IFACE_REC.ATTRIBUTE5            := V_HEADER_ID;
            INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
            L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                                 P_INIT_MSG_LIST    => 'F',
                                                                 P_VALIDATION_LEVEL => 100,
                                                                 P_COMMIT           => 'T',
                                                                 X_RETURN_STATUS    => L_RETURN_STATUS,
                                                                 X_MSG_COUNT        => L_MSG_COUNT,
                                                                 X_MSG_DATA         => L_MSG_DATA,
                                                                 X_TRANS_COUNT      => L_TRANS_COUNT,
                                                                 P_HEADER_ID        => V_TXN_ID);
            IF L_RESULT = 0 THEN
              NULL;
            ELSE
              FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                          FROM MTL_TRANSACTIONS_INTERFACE
                         WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
                V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                                I.ERROR_EXPLANATION;
              END LOOP;
              P_TXT := '84杂入,物料:' || Z.SEGMENT1 || V_ERROR_CODE;
              ROLLBACK;
              RAISE FORM_TRIGGER_FAILURE;
            END IF;
            ---开始标记状态
            UPDATE CUX.CUX_OE_SHIP_LINES B
               SET B.Attribute13 = 'INV'
             WHERE B.LINE_ID = Z.LINE_ID;
          ELSE
            ---FW的三角贸易
            --84组织杂发
            --判断对应的仓库是否启用货位管理
            --判断是否软件
            IF Z.SOFTWARE_FLAG = 'Y' THEN
              V_SUBINVENTORY_CODE := 'G999';
              --开始匹配库存
              INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                (LINE_ID,
                 ORGANIZATION_ID,
                 SUBINVENTORY_CODE,
                 LOCATOR_ID,
                 QTY)
              VALUES
                (Z.LINE_ID,
                 V_PRODUCTS_OU_ID,
                 V_SUBINVENTORY_CODE,
                 NULL,
                 Z.QUANTITY);
            ELSE
              --非软件
              V_SUBINVENTORY_CODE := 'G' ||
                                     SUBSTR(Z.PICK_FROM_SUBINVENTORY, 2); --commentted by bruce on 20150804
            
              --V_SUBINVENTORY_CODE := z.PICK_FROM_SUBINVENTORY;
            
              IF (z.locator_id > 0) THEN
                --ADDED BY BRUCE ON 20150804
              
                INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                  (LINE_ID,
                   ORGANIZATION_ID,
                   SUBINVENTORY_CODE,
                   LOCATOR_ID,
                   QTY)
                VALUES
                  (Z.LINE_ID,
                   V_PRODUCTS_OU_ID,
                   V_SUBINVENTORY_CODE,
                   z.locator_id,
                   z.QUANTITY);
              else
              
                SELECT COUNT(*)
                  INTO V_COUNT
                  FROM MTL_ITEM_LOCATIONS MIL
                 WHERE MIL.ORGANIZATION_ID = V_PRODUCTS_OU_ID
                   AND MIL.SUBINVENTORY_CODE = V_SUBINVENTORY_CODE;
                IF V_COUNT > 0 THEN
                  V_XQ_QTY := Z.QUANTITY;
                  FOR W IN (SELECT MOQ.LOCATOR_ID,
                                   SUM(MOQ.TRANSACTION_QUANTITY) QTY
                              FROM MTL_ONHAND_QUANTITIES MOQ
                             WHERE MOQ.ORGANIZATION_ID = V_PRODUCTS_OU_ID
                               AND MOQ.INVENTORY_ITEM_ID =
                                   Z.INVENTORY_ITEM_ID
                               AND MOQ.SUBINVENTORY_CODE =
                                   V_SUBINVENTORY_CODE
                             GROUP BY MOQ.LOCATOR_ID) LOOP
                  
                    select nvl(sum(mr.reservation_quantity), 0)
                      into v_reserved_qty
                      from mtl_reservations mr
                     where mr.inventory_item_id = z.INVENTORY_ITEM_ID
                       and mr.subinventory_code = V_SUBINVENTORY_CODE
                       and mr.organization_id = V_PRODUCTS_OU_ID
                       and mr.locator_id = W.LOCATOR_ID;
                  
                    v_locator_att := W.QTY - v_reserved_qty; --added by bruce on 20150804          
                  
                    IF V_XQ_QTY <= /*W.QTY*/
                       v_locator_att THEN
                      --modified by bruce on 20150804
                      INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                        (LINE_ID,
                         ORGANIZATION_ID,
                         SUBINVENTORY_CODE,
                         LOCATOR_ID,
                         QTY)
                      VALUES
                        (Z.LINE_ID,
                         V_PRODUCTS_OU_ID,
                         V_SUBINVENTORY_CODE,
                         W.LOCATOR_ID,
                         V_XQ_QTY);
                      V_XQ_QTY := 0;
                    ELSE
                      INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                        (LINE_ID,
                         ORGANIZATION_ID,
                         SUBINVENTORY_CODE,
                         LOCATOR_ID,
                         QTY)
                      VALUES
                        (Z.LINE_ID,
                         V_PRODUCTS_OU_ID,
                         V_SUBINVENTORY_CODE,
                         W.LOCATOR_ID,
                         /*W.QTY*/
                         v_locator_att ----modified by bruce on 20150804
                         );
                      V_XQ_QTY := V_XQ_QTY - v_locator_att; --modified by bruce on 20150804;
                    END IF;
                  END LOOP;
                ELSE
                  --开始匹配库存
                  INSERT INTO CUX.CUX_OE_SHIP_QTY_TMP
                    (LINE_ID,
                     ORGANIZATION_ID,
                     SUBINVENTORY_CODE,
                     LOCATOR_ID,
                     QTY)
                  VALUES
                    (Z.LINE_ID,
                     V_PRODUCTS_OU_ID,
                     V_SUBINVENTORY_CODE,
                     NULL,
                     Z.QUANTITY);
                END IF;
              end if;
            END IF;
            --开始杂项事务处理
            --84组织开始杂发
            FOR W IN (SELECT *
                        FROM CUX.CUX_OE_SHIP_QTY_TMP A
                       WHERE A.LINE_ID = Z.LINE_ID
                         AND A.QTY > 0) LOOP
              SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
                INTO V_TXN_ID
                FROM DUAL;
            
              L_IFACE_REC.TRANSACTION_INTERFACE_ID   := V_TXN_ID;
              L_IFACE_REC.TRANSACTION_MODE           := 3; --BACKGROUND
              L_IFACE_REC.PROCESS_FLAG               := 1; --TO BE PROCESSED    
              L_IFACE_REC.ORGANIZATION_ID            := 84;
              L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL;
              L_IFACE_REC.INVENTORY_ITEM_ID          := Z.INVENTORY_ITEM_ID;
              L_IFACE_REC.SUBINVENTORY_CODE          := W.SUBINVENTORY_CODE;
              L_IFACE_REC.LOCATOR_ID                 := W.LOCATOR_ID;
              L_IFACE_REC.TRANSACTION_QUANTITY       := -W.QTY;
              L_IFACE_REC.PRIMARY_QUANTITY           := -W.QTY;
              L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
              L_IFACE_REC.TRANSACTION_COST           := NULL;
              L_IFACE_REC.TRANSACTION_DATE           := Z.SHIP_DATE;
              L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
              L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
              L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂发';
              L_IFACE_REC.TRANSACTION_REFERENCE      := 'CUX.三角贸易杂发,单号:' ||
                                                        z.request_number;
              L_IFACE_REC.SOURCE_HEADER_ID           := Z.HEADER_ID; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
              L_IFACE_REC.SOURCE_LINE_ID             := Z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
              L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
              L_IFACE_REC.TRANSACTION_TYPE_ID        := 1; --帐户发放          
              L_IFACE_REC.TRANSACTION_SOURCE_ID      := 1025; --发出商品科目
              L_IFACE_REC.LAST_UPDATE_DATE           := SYSDATE;
              L_IFACE_REC.LAST_UPDATED_BY            := FND_GLOBAL.USER_ID;
              L_IFACE_REC.CREATION_DATE              := SYSDATE;
              L_IFACE_REC.CREATED_BY                 := FND_GLOBAL.USER_ID;
              L_IFACE_REC.LAST_UPDATE_LOGIN          := FND_GLOBAL.LOGIN_ID;
              L_IFACE_REC.TRANSACTION_HEADER_ID      := V_TXN_ID;
              L_IFACE_REC.ATTRIBUTE1                 := Z.OE_HEADER_ID;
              L_IFACE_REC.ATTRIBUTE2                 := Z.OE_LINE_ID;
              L_IFACE_REC.ATTRIBUTE3                 := 'Y';
              L_IFACE_REC.ATTRIBUTE4                 := Z.ATTRIBUTE2;
              L_IFACE_REC.ATTRIBUTE5                 := V_HEADER_ID;
              INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
              L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                                   P_INIT_MSG_LIST    => 'F',
                                                                   P_VALIDATION_LEVEL => 100,
                                                                   P_COMMIT           => 'T',
                                                                   X_RETURN_STATUS    => L_RETURN_STATUS,
                                                                   X_MSG_COUNT        => L_MSG_COUNT,
                                                                   X_MSG_DATA         => L_MSG_DATA,
                                                                   X_TRANS_COUNT      => L_TRANS_COUNT,
                                                                   P_HEADER_ID        => V_TXN_ID);
              IF L_RESULT = 0 THEN
                NULL;
              ELSE
                FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                            FROM MTL_TRANSACTIONS_INTERFACE
                           WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
                  V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                                  I.ERROR_EXPLANATION;
                END LOOP;
                p_txt := '84杂出,物料:' || Z.SEGMENT1 || V_ERROR_CODE;
                ROLLBACK;
                RAISE form_trigger_failure;
              END IF;
            END LOOP;
            -----83组织杂入
            --获取价格
            SELECT OOLA.UNIT_SELLING_PRICE,
                   OOHA.TRANSACTIONAL_CURR_CODE,
                   OOLA.TAX_CODE
              INTO V_UNIT_SELLING_PRICE,
                   V_TRANSACTIONAL_CURR_CODE,
                   V_TAX_CODE
              FROM OE_ORDER_LINES_ALL OOLA, OE_ORDER_HEADERS_ALL OOHA
             WHERE OOLA.HEADER_ID = OOHA.HEADER_ID
               AND OOLA.LINE_ID = Z.OE_LINE_ID;
            BEGIN
              SELECT C.PERCENTAGE_RATE
                INTO V_PERCENTAGE_RATE
                FROM ZX_RATES_VL C
               WHERE C.TAX_RATE_CODE = V_TAX_CODE;
            EXCEPTION
              WHEN OTHERS THEN
                V_PERCENTAGE_RATE := 0;
            END;
          
            IF V_TRANSACTIONAL_CURR_CODE <> 'CNY' THEN
            
              SELECT GDR.CONVERSION_RATE
                INTO V_CONVERSION_RATE
                FROM GL_DAILY_RATES GDR
               WHERE GDR.FROM_CURRENCY = V_TRANSACTIONAL_CURR_CODE
                 AND GDR.TO_CURRENCY = 'CNY'
                 AND GDR.CONVERSION_TYPE = 'Corporate'
                 AND GDR.CONVERSION_DATE = trunc(SYSDATE);
              V_UNIT_SELLING_PRICE := V_UNIT_SELLING_PRICE *
                                      V_CONVERSION_RATE;
            END IF;
          
            V_UNIT_SELLING_PRICE := ROUND(V_UNIT_SELLING_PRICE * 0.9 /
                                          (1 + V_PERCENTAGE_RATE / 100),
                                          6);
            ---------仓库货位判断
            IF Z.SOFTWARE_FLAG = 'Y' THEN
              V_SUBINVENTORY_CODE_R := 'H999';
              V_LOCATION_ID_R       := NULL;
            ELSE
            
              /* if (SUBSTR(Z.PICK_FROM_SUBINVENTORY, 2) = 169) then
                
                  V_SUBINVENTORY_CODE_R := 'H169';
                else
                  V_SUBINVENTORY_CODE_R := 'H151';
                end if;
              */
              V_SUBINVENTORY_CODE_R := 'H' ||
                                       SUBSTR(Z.PICK_FROM_SUBINVENTORY, 2); --commentted by bruce on 20150804
            
              IF V_SUBINVENTORY_CODE_R = 'H169' THEN
                SELECT MAX(MIL.INVENTORY_LOCATION_ID)
                  INTO V_LOCATION_ID_R
                  FROM MTL_ITEM_LOCATIONS MIL
                 WHERE MIL.ORGANIZATION_ID = 83
                   AND MIL.SUBINVENTORY_CODE = V_SUBINVENTORY_CODE_R
                   AND MIL.SEGMENT1 = 'WJ';
              ELSE
                BEGIN
                  SELECT MAX(MIL.INVENTORY_LOCATION_ID)
                    INTO V_LOCATION_ID_R
                    FROM MTL_ITEM_LOCATIONS MIL
                   WHERE MIL.ORGANIZATION_ID = 83
                     AND MIL.SUBINVENTORY_CODE = V_SUBINVENTORY_CODE_R;
                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    V_SUBINVENTORY_CODE_R := 'H151';
                    V_LOCATION_ID_R       := NULL;
                END;
              END IF;
            END IF;
          
            SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
              INTO V_TXN_ID
              FROM DUAL;
          
            L_IFACE_REC.TRANSACTION_INTERFACE_ID   := V_TXN_ID;
            L_IFACE_REC.TRANSACTION_MODE           := 3; --BACKGROUND
            L_IFACE_REC.PROCESS_FLAG               := 1; --TO BE PROCESSED    
            L_IFACE_REC.ORGANIZATION_ID            := 83;
            L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL;
            L_IFACE_REC.INVENTORY_ITEM_ID          := Z.INVENTORY_ITEM_ID;
            L_IFACE_REC.SUBINVENTORY_CODE          := V_SUBINVENTORY_CODE_R;
            L_IFACE_REC.LOCATOR_ID                 := V_LOCATION_ID_R;
            L_IFACE_REC.TRANSACTION_QUANTITY       := Z.QUANTITY;
            L_IFACE_REC.PRIMARY_QUANTITY           := Z.QUANTITY;
            L_IFACE_REC.TRANSACTION_UOM            := Z.PRIMARY_UOM_CODE;
            L_IFACE_REC.TRANSACTION_COST           := V_UNIT_SELLING_PRICE;
            L_IFACE_REC.TRANSACTION_DATE           := Z.SHIP_DATE;
            L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
            L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
            L_IFACE_REC.SOURCE_CODE                := 'CUX.三角贸易杂入';
            L_IFACE_REC.TRANSACTION_REFERENCE      := 'CUX.三角贸易杂入,单号:' ||
                                                      Z.REQUEST_NUMBER;
            L_IFACE_REC.SOURCE_HEADER_ID           := Z.HEADER_ID; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
            L_IFACE_REC.SOURCE_LINE_ID             := z.LINE_ID; --1234567; 2010.9.15 XDL 用于追溯
            L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 3; --帐户
            L_IFACE_REC.TRANSACTION_TYPE_ID        := 40; --帐户发放 
          
            L_IFACE_REC.TRANSACTION_SOURCE_ID := 1029; --应付暂估带往来
          
            L_IFACE_REC.LAST_UPDATE_DATE      := SYSDATE;
            L_IFACE_REC.LAST_UPDATED_BY       := FND_GLOBAL.USER_ID;
            L_IFACE_REC.CREATION_DATE         := SYSDATE;
            L_IFACE_REC.CREATED_BY            := FND_GLOBAL.USER_ID;
            L_IFACE_REC.LAST_UPDATE_LOGIN     := FND_GLOBAL.LOGIN_ID;
            L_IFACE_REC.TRANSACTION_HEADER_ID := V_TXN_ID;
            L_IFACE_REC.ATTRIBUTE1            := Z.OE_HEADER_ID;
            L_IFACE_REC.ATTRIBUTE2            := Z.OE_LINE_ID;
            L_IFACE_REC.ATTRIBUTE3            := 'Y';
            L_IFACE_REC.ATTRIBUTE4            := Z.ATTRIBUTE2;
            L_IFACE_REC.ATTRIBUTE5            := V_HEADER_ID;
            INSERT INTO MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
            L_RESULT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                                 P_INIT_MSG_LIST    => 'F',
                                                                 P_VALIDATION_LEVEL => 100,
                                                                 P_COMMIT           => 'T',
                                                                 X_RETURN_STATUS    => L_RETURN_STATUS,
                                                                 X_MSG_COUNT        => L_MSG_COUNT,
                                                                 X_MSG_DATA         => L_MSG_DATA,
                                                                 X_TRANS_COUNT      => L_TRANS_COUNT,
                                                                 P_HEADER_ID        => V_TXN_ID);
            IF L_RESULT = 0 THEN
              NULL;
            ELSE
              FOR I IN (SELECT DISTINCT ERROR_EXPLANATION
                          FROM MTL_TRANSACTIONS_INTERFACE
                         WHERE TRANSACTION_HEADER_ID = V_TXN_ID) LOOP
                V_ERROR_CODE := V_ERROR_CODE || '.ERROR:' ||
                                I.ERROR_EXPLANATION;
              END LOOP;
              P_TXT := '83杂入,物料:' || Z.SEGMENT1 || V_ERROR_CODE;
              ROLLBACK;
              RAISE FORM_TRIGGER_FAILURE;
            END IF;
            ---开始标记状态
            UPDATE CUX.CUX_OE_SHIP_LINES B
               SET B.ATTRIBUTE13 = 'INV'
             WHERE B.LINE_ID = Z.LINE_ID;
          END IF;
        END IF;
      END LOOP;
    end if; --count req
  
    COMMIT;
  
    ------------------------------------开始物料搬运单处理-----------------------------
    /*SELECT DISTINCT A.REQUEST_NUMBER
     INTO V_REQUEST_NUMBER
     FROM CUX_OE_SHIP_HEADERS_V A
    WHERE A.HEADER_ID = P_HEADER_ID;*/
  
    if (V_count_req > 0) then
    
      FOR Z IN cur_new_req LOOP
        ---更新物料搬运单数量
        IF Z.ISSUE_QUANTITY > 0 THEN
          UPDATE MTL_TXN_REQUEST_LINES A
             SET A.QUANTITY = Z.ISSUE_QUANTITY
           WHERE A.LINE_ID = Z.LINE_ID;
          COMMIT;
          -----开始库存量验证
          INV_QUANTITY_TREE_PUB.CLEAR_QUANTITY_CACHE;
          INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                                 P_INIT_MSG_LST        => NULL,
                                                 X_RETURN_STATUS       => L_RETURN_STATUS,
                                                 X_MSG_COUNT           => L_MSG_COUNT,
                                                 X_MSG_DATA            => L_MSG_DATA,
                                                 P_ORGANIZATION_ID     => Z.ORGANIZATION_ID, --库存组织ID
                                                 P_INVENTORY_ITEM_ID   => Z.INVENTORY_ITEM_ID, --物料ID
                                                 P_TREE_MODE           => 3,
                                                 P_IS_REVISION_CONTROL => FALSE,
                                                 P_IS_LOT_CONTROL      => FALSE,
                                                 P_IS_SERIAL_CONTROL   => FALSE,
                                                 P_REVISION            => NULL,
                                                 P_LOT_NUMBER          => NULL,
                                                 P_LOT_EXPIRATION_DATE => NULL,
                                                 P_SUBINVENTORY_CODE   => Z.FROM_SUBINVENTORY_CODE, --子库CODE
                                                 P_LOCATOR_ID          => z.From_Locator_Id,
                                                 P_COST_GROUP_ID       => NULL,
                                                 P_ONHAND_SOURCE       => 3,
                                                 X_QOH                 => V_L_QOH, --现有量
                                                 X_RQOH                => V_L_RQOH,
                                                 X_QR                  => V_L_QR,
                                                 X_QS                  => V_L_QS,
                                                 X_ATT                 => V_L_ATT,
                                                 X_ATR                 => V_L_ATR);
          IF NVL(V_L_ATT, 0) < Z.ISSUE_QUANTITY THEN
            P_TXT := Z.SEGMENT1 || '-现有量不足,仓库' || Z.FROM_SUBINVENTORY_CODE;
            RAISE FORM_TRIGGER_FAILURE;
          END IF;
        END IF;
      END LOOP;
    
      FOR Z IN cur_new_req LOOP
        --处理物料搬运单
      
        IF Z.ISSUE_QUANTITY > 0 THEN
          INV_REPLENISH_DETAIL_PUB.LINE_DETAILS_PUB(P_LINE_ID               => Z.LINE_ID,
                                                    X_NUMBER_OF_ROWS        => X_NUMBER_OF_ROWS,
                                                    X_DETAILED_QTY          => X_DETAILED_QTY,
                                                    X_RETURN_STATUS         => V_RETURN_STATUS,
                                                    X_MSG_COUNT             => V_MSG_COUNT,
                                                    X_MSG_DATA              => V_MSG_DATA,
                                                    X_REVISION              => X_REVISION,
                                                    X_LOCATOR_ID            => X_LOCATOR_ID,
                                                    X_TRANSFER_TO_LOCATION  => X_TRANSFER_TO_LOCATION,
                                                    X_LOT_NUMBER            => X_LOT_NUMBER,
                                                    X_EXPIRATION_DATE       => X_EXPIRATION_DATE,
                                                    X_TRANSACTION_TEMP_ID   => X_TRANSACTION_TEMP_ID,
                                                    P_TRANSACTION_HEADER_ID => NULL,
                                                    P_TRANSACTION_MODE      => 1,
                                                    P_MOVE_ORDER_TYPE       => P_MOVE_ORDER_TYPE,
                                                    P_SERIAL_FLAG           => FND_API.G_FALSE,
                                                    P_PLAN_TASKS            => FALSE,
                                                    P_AUTO_PICK_CONFIRM     => FALSE,
                                                    P_COMMIT                => FALSE);
          IF (NVL(v_RETURN_STATUS, 'NA') <> FND_API.G_RET_STS_SUCCESS) THEN
            P_TXT := P_TXT || '搬运单保留失败' || V_MSG_DATA;
            RAISE FORM_TRIGGER_FAILURE;
          END IF;
          -- COMMIT;
        
          L_TROLIN_TBL := INV_TROLIN_UTIL.query_rows(P_LINE_ID => z.line_id);
        
          L_MOLD_TBL := INV_MO_LINE_DETAIL_UTIL.QUERY_ROWS(P_LINE_ID => z.line_id);
        
          INV_PICK_WAVE_PICK_CONFIRM_PUB.PICK_CONFIRM(P_API_VERSION_NUMBER => L_API_VERSION,
                                                      P_INIT_MSG_LIST      => L_INIT_MSG_LIST,
                                                      P_COMMIT             => L_COMMIT,
                                                      X_RETURN_STATUS      => V_RETURN_STATUS,
                                                      X_MSG_COUNT          => V_MSG_COUNT,
                                                      X_MSG_DATA           => V_MSG_DATA,
                                                      P_MOVE_ORDER_TYPE    => L_MOVE_ORDER_TYPE,
                                                      P_TRANSACTION_MODE   => 1,
                                                      P_TROLIN_TBL         => L_TROLIN_TBL,
                                                      P_MOLD_TBL           => L_MOLD_TBL,
                                                      X_MMTT_TBL           => X_MOLD_TBL,
                                                      X_TROLIN_TBL         => X_TROLIN_TBL);
        
          IF (v_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS) THEN
            P_TXT := P_TXT || '搬运单确认失败' || V_MSG_DATA;
            RAISE FORM_TRIGGER_FAILURE;
          END IF;
          UPDATE CUX.CUX_OE_SHIP_LINES B
             SET B.ATTRIBUTE13 = 'Y', B.TRANSACTION_ID = -1
           WHERE B.LINE_ID = Z.SHIP_LINE_ID;
        ELSE
          INV_MO_BACKORDER_PVT.BACKORDER(X_RETURN_STATUS => V_RETURN_STATUS,
                                         X_MSG_COUNT     => V_MSG_COUNT,
                                         X_MSG_DATA      => V_MSG_DATA,
                                         P_LINE_ID       => Z.LINE_ID);
          UPDATE CUX.CUX_OE_SHIP_LINES B
             SET B.ATTRIBUTE13 = 'Y', B.TRANSACTION_ID = -1
           WHERE B.LINE_ID = Z.SHIP_LINE_ID;
        END IF;
      END LOOP;
    
      FOR Y IN C_22_new LOOP
      
        UPDATE CUX.CUX_OE_SHIP_LINES B
           SET B.ATTRIBUTE13 = 'Y', B.TRANSACTION_ID = -1
         WHERE B.LINE_ID = Y.LINE_ID;
      END LOOP;
    
    else
      --count req
      FOR Z IN cur_old_req LOOP
        ---更新物料搬运单数量
        IF Z.ISSUE_QUANTITY > 0 THEN
          UPDATE MTL_TXN_REQUEST_LINES A
             SET A.QUANTITY = Z.ISSUE_QUANTITY
           WHERE A.LINE_ID = Z.LINE_ID;
          COMMIT;
          -----开始库存量验证
          INV_QUANTITY_TREE_PUB.CLEAR_QUANTITY_CACHE;
          INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                                 P_INIT_MSG_LST        => NULL,
                                                 X_RETURN_STATUS       => L_RETURN_STATUS,
                                                 X_MSG_COUNT           => L_MSG_COUNT,
                                                 X_MSG_DATA            => L_MSG_DATA,
                                                 P_ORGANIZATION_ID     => Z.ORGANIZATION_ID, --库存组织ID
                                                 P_INVENTORY_ITEM_ID   => Z.INVENTORY_ITEM_ID, --物料ID
                                                 P_TREE_MODE           => 3,
                                                 P_IS_REVISION_CONTROL => FALSE,
                                                 P_IS_LOT_CONTROL      => FALSE,
                                                 P_IS_SERIAL_CONTROL   => FALSE,
                                                 P_REVISION            => NULL,
                                                 P_LOT_NUMBER          => NULL,
                                                 P_LOT_EXPIRATION_DATE => NULL,
                                                 P_SUBINVENTORY_CODE   => Z.FROM_SUBINVENTORY_CODE, --子库CODE
                                                 P_LOCATOR_ID          => z.From_Locator_Id,
                                                 P_COST_GROUP_ID       => NULL,
                                                 P_ONHAND_SOURCE       => 3,
                                                 X_QOH                 => V_L_QOH, --现有量
                                                 X_RQOH                => V_L_RQOH,
                                                 X_QR                  => V_L_QR,
                                                 X_QS                  => V_L_QS,
                                                 X_ATT                 => V_L_ATT,
                                                 X_ATR                 => V_L_ATR);
          IF NVL(V_L_ATT, 0) < Z.ISSUE_QUANTITY THEN
            P_TXT := Z.SEGMENT1 || '-现有量不足,仓库' || Z.FROM_SUBINVENTORY_CODE;
            RAISE FORM_TRIGGER_FAILURE;
          END IF;
        END IF;
      END LOOP;
    
      FOR Z IN cur_old_req LOOP
        --处理物料搬运单
      
        IF Z.ISSUE_QUANTITY > 0 THEN
          INV_REPLENISH_DETAIL_PUB.LINE_DETAILS_PUB(P_LINE_ID               => Z.LINE_ID,
                                                    X_NUMBER_OF_ROWS        => X_NUMBER_OF_ROWS,
                                                    X_DETAILED_QTY          => X_DETAILED_QTY,
                                                    X_RETURN_STATUS         => V_RETURN_STATUS,
                                                    X_MSG_COUNT             => V_MSG_COUNT,
                                                    X_MSG_DATA              => V_MSG_DATA,
                                                    X_REVISION              => X_REVISION,
                                                    X_LOCATOR_ID            => X_LOCATOR_ID,
                                                    X_TRANSFER_TO_LOCATION  => X_TRANSFER_TO_LOCATION,
                                                    X_LOT_NUMBER            => X_LOT_NUMBER,
                                                    X_EXPIRATION_DATE       => X_EXPIRATION_DATE,
                                                    X_TRANSACTION_TEMP_ID   => X_TRANSACTION_TEMP_ID,
                                                    P_TRANSACTION_HEADER_ID => NULL,
                                                    P_TRANSACTION_MODE      => 1,
                                                    P_MOVE_ORDER_TYPE       => P_MOVE_ORDER_TYPE,
                                                    P_SERIAL_FLAG           => FND_API.G_FALSE,
                                                    P_PLAN_TASKS            => FALSE,
                                                    P_AUTO_PICK_CONFIRM     => FALSE,
                                                    P_COMMIT                => FALSE);
          IF (NVL(v_RETURN_STATUS, 'NA') <> FND_API.G_RET_STS_SUCCESS) THEN
            P_TXT := P_TXT || '搬运单保留失败' || V_MSG_DATA;
            RAISE FORM_TRIGGER_FAILURE;
          END IF;
          -- COMMIT;
        
          L_TROLIN_TBL := INV_TROLIN_UTIL.query_rows(P_LINE_ID => z.line_id);
        
          L_MOLD_TBL := INV_MO_LINE_DETAIL_UTIL.QUERY_ROWS(P_LINE_ID => z.line_id);
        
          INV_PICK_WAVE_PICK_CONFIRM_PUB.PICK_CONFIRM(P_API_VERSION_NUMBER => L_API_VERSION,
                                                      P_INIT_MSG_LIST      => L_INIT_MSG_LIST,
                                                      P_COMMIT             => L_COMMIT,
                                                      X_RETURN_STATUS      => V_RETURN_STATUS,
                                                      X_MSG_COUNT          => V_MSG_COUNT,
                                                      X_MSG_DATA           => V_MSG_DATA,
                                                      P_MOVE_ORDER_TYPE    => L_MOVE_ORDER_TYPE,
                                                      P_TRANSACTION_MODE   => 1,
                                                      P_TROLIN_TBL         => L_TROLIN_TBL,
                                                      P_MOLD_TBL           => L_MOLD_TBL,
                                                      X_MMTT_TBL           => X_MOLD_TBL,
                                                      X_TROLIN_TBL         => X_TROLIN_TBL);
        
          IF (v_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS) THEN
            P_TXT := P_TXT || '搬运单确认失败' || V_MSG_DATA;
            RAISE FORM_TRIGGER_FAILURE;
          END IF;
          UPDATE CUX.CUX_OE_SHIP_LINES B
             SET B.ATTRIBUTE13 = 'Y', B.TRANSACTION_ID = -1
           WHERE B.LINE_ID = Z.SHIP_LINE_ID;
        ELSE
          INV_MO_BACKORDER_PVT.BACKORDER(X_RETURN_STATUS => V_RETURN_STATUS,
                                         X_MSG_COUNT     => V_MSG_COUNT,
                                         X_MSG_DATA      => V_MSG_DATA,
                                         P_LINE_ID       => Z.LINE_ID);
          UPDATE CUX.CUX_OE_SHIP_LINES B
             SET B.ATTRIBUTE13 = 'Y', B.TRANSACTION_ID = -1
           WHERE B.LINE_ID = Z.SHIP_LINE_ID;
        END IF;
      END LOOP;
    
      --把零数据设置成已经处理
      FOR Y IN C_22_old LOOP
      
        UPDATE CUX.CUX_OE_SHIP_LINES B
           SET B.ATTRIBUTE13 = 'Y', B.TRANSACTION_ID = -1
         WHERE B.LINE_ID = Y.LINE_ID;
      END LOOP;
    
    end if;
  
    --判断单据是否处理完成
    SELECT COUNT(*)
      INTO V_COUNT
      FROM CUX_PICK_SUB_DOC CPD
     WHERE CPD.REQ_HEADER_ID = P_HEADER_ID
       AND NOT EXISTS (SELECT 1
              FROM CUX.CUX_OE_SHIP_HEADERS COH
             WHERE COH.HEADER_ID = P_HEADER_ID);
  
    IF V_COUNT = 0 THEN
      UPDATE CUX.CUX_OE_SHIP_HEADERS A
         SET A.ATTRIBUTE1 = 'Y'
       WHERE A.HEADER_ID = P_HEADER_ID;
    ELSE
      UPDATE CUX_PICK_SUB_DOC CPD
         SET CPD.ATTRIBUTE1 = 'Y'
       where cpd.req_header_id = p_header_id;
    END IF;
    P_FLAG := 'S';
    /* ELSE
       P_FLAG := 'E';  
    END IF;  */
    COMMIT;
  EXCEPTION
    WHEN FORM_TRIGGER_FAILURE THEN
      P_FLAG := 'E';
      ROLLBACK;
    when others then
      p_txt  := 'OTHER ERROR-:' || p_txt || SQLERRM;
      P_FLAG := 'E';
      ROLLBACK;
  END;
  procedure main_new_dj(p_header_id       in number,
                        p_ORGANIZATION_ID in number,
                        p_flag            in out varchar2,
                        p_txt             in out varchar2,
                        p_request_number  in out varchar2) as
  
    cursor cur_old_req is
      SELECT MTRL.LINE_ID,
             COSL.ISSUE_QUANTITY,
             MTRL.FROM_SUBINVENTORY_CODE,
             MTRL.FROM_LOCATOR_ID,
             MTRL.ORGANIZATION_ID,
             MTRL.INVENTORY_ITEM_ID,
             MSI.SEGMENT1,
             COSL.LINE_ID SHIP_LINE_ID
        FROM /*MTL_TXN_REQUEST_HEADERS MTRH,*/ MTL_TXN_REQUEST_LINES MTRL,
             MTL_SYSTEM_ITEMS_B    MSI,
             --CUX_PICK_SUB_DOC      CPD, --ADDED BY BRUCE ON 2015-8-4
             /*CUX_OE_SHIP_LINES_V   COSL*/
             cux.cux_oe_ship_lines cosl
       WHERE MTRL.ORGANIZATION_ID = MSI.ORGANIZATION_ID
         AND MTRL.INVENTORY_ITEM_ID = MSI.INVENTORY_ITEM_ID
            /*AND COSL.HEADER_ID = P_HEADER_ID*/
         AND p_header_id = COSL.HEADER_ID
            --AND CPD.CUX_PICK_LINE_ID(+) = COSL.LINE_ID
         AND COSL.DELIVERY_DETAIL_ID = MTRL.TXN_SOURCE_LINE_DETAIL_ID
         and cosl.ATTRIBUTE11 = mtrl.line_id;
  
    cursor cur_new_req is
      SELECT MTRL.LINE_ID,
             COSL.ISSUE_QUANTITY,
             MTRL.FROM_SUBINVENTORY_CODE,
             MTRL.FROM_LOCATOR_ID,
             MTRL.ORGANIZATION_ID,
             MTRL.INVENTORY_ITEM_ID,
             MSI.SEGMENT1,
             COSL.LINE_ID SHIP_LINE_ID
        FROM /*MTL_TXN_REQUEST_HEADERS MTRH,*/ MTL_TXN_REQUEST_LINES MTRL,
             MTL_SYSTEM_ITEMS_B    MSI,
             CUX_PICK_SUB_DOC      CPD, --ADDED BY BRUCE ON 2015-8-4
             /*CUX_OE_SHIP_LINES_V   COSL*/
             cux.cux_oe_ship_lines cosl
       WHERE MTRL.ORGANIZATION_ID = MSI.ORGANIZATION_ID
         AND MTRL.INVENTORY_ITEM_ID = MSI.INVENTORY_ITEM_ID
            /*AND COSL.HEADER_ID = P_HEADER_ID*/
         AND CPD.REQ_HEADER_ID = P_HEADER_ID
            
         AND CPD.CUX_PICK_LINE_ID = COSL.LINE_ID
         AND COSL.DELIVERY_DETAIL_ID = MTRL.TXN_SOURCE_LINE_DETAIL_ID
         and cosl.ATTRIBUTE11 = mtrl.line_id;
  
    cursor c_2_old is
      select a.PICK_FROM_SUBINVENTORY PICK_FROM_SUBINVENTORY,
             a.LOCATOR_ID             LOCATOR_ID,
             b.INVENTORY_ITEM_ID,
             msi.segment1,
             --b.LINE_NUMBER,
             b.DELIVERY_DETAIL_ID,
             b.ISSUE_QUANTITY     QUANTITY,
             --a.SET_ID,
             B.OE_LINE_ID,
             --a.HEADER_ID,
             b.LINE_ID,
             --a.OE_HEADER_ID,
             --a.REQUEST_NUMBER,
             --a.attribute2,
             NVL(B.Attribute13, 'N') Attribute13,
             b.ATTRIBUTE11, --搬运单ID
             b.software_flag,
             msi.primary_uom_code,
             sysdate SHIP_DATE
      /*             TO_DATE('2014-09-30','YYYY-MM-DD')SHIP_DATE*/
        from /*cux_oe_ship_headers_v a,*/ cux.cux_oe_ship_headers a, --modified on 20151020
             --cux_oe_ship_lines_v   b,--modified on 20151021
             cux.cux_oe_ship_lines b, --modified on 20151021
             wsh_delivery_details  wdd, --modified on 20151021
             --cux_pick_sub_doc      cpd, --added by bruce on 2015-8-4
             mtl_system_items_b msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                   and*/
       a.header_id = b.header_id --modified on 20151021
       and msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
      --added on 20151021
       and b.delivery_detail_id = wdd.delivery_detail_id
       and wdd.organization_id = p_ORGANIZATION_ID
      --end of add on 20151021
       and b.ISSUE_QUANTITY > 0
       and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
      
       and b.HEADER_ID = p_header_id
      --and cpd.cux_pick_line_id(+) = b.LINE_ID
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020
  
    cursor c_2_new is
      select cpd.pick_subinv     PICK_FROM_SUBINVENTORY,
             cpd.pick_locator_id LOCATOR_ID,
             b.INVENTORY_ITEM_ID,
             msi.segment1,
             --b.LINE_NUMBER,
             b.DELIVERY_DETAIL_ID,
             b.ISSUE_QUANTITY     QUANTITY,
             --a.SET_ID,
             B.OE_LINE_ID,
             --a.HEADER_ID,
             b.LINE_ID,
             --a.OE_HEADER_ID,
             --a.REQUEST_NUMBER,
             --a.attribute2,
             NVL(B.Attribute13, 'N') Attribute13,
             b.ATTRIBUTE11, --搬运单ID
             b.software_flag,
             msi.primary_uom_code,
             sysdate SHIP_DATE
      /*             TO_DATE('2014-09-30','YYYY-MM-DD')SHIP_DATE*/
        from /*cux_oe_ship_headers_v a,*/ -- cux.cux_oe_ship_headers a, --modified on 20151020
             --cux_oe_ship_lines_v   b,--modified on 20151021
              cux.cux_oe_ship_lines b, --modified on 20151021
             wsh_delivery_details  wdd, --modified on 20151021
             cux_pick_sub_doc      cpd, --added by bruce on 2015-8-4
             mtl_system_items_b    msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                   and*/
      -- a.header_id = b.header_id --modified on 20151021
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
      --added on 20151021
       and b.delivery_detail_id = wdd.delivery_detail_id
       and wdd.organization_id = p_ORGANIZATION_ID
      --end of add on 20151021
       and b.ISSUE_QUANTITY > 0
       and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
       and cpd.req_header_id = p_header_id
       and cpd.cux_pick_line_id = b.LINE_ID
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020   
  
    cursor c_21_old is
      select b.ISSUE_QUANTITY QUANTITY, msi.segment1
        from /*cux_oe_ship_headers_v a,*/
             /*cux_oe_ship_lines_v b,*/ cux.cux_oe_ship_lines b, --modified on 20151020
             --cux_pick_sub_doc      cpd,
             mtl_system_items_b msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
       and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
       and b.HEADER_ID = p_header_id
      
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020
  
    cursor c_21_new is
      select b.ISSUE_QUANTITY QUANTITY, msi.segment1
        from /*cux_oe_ship_headers_v a,*/
             /*cux_oe_ship_lines_v b,*/ cux.cux_oe_ship_lines b, --modified on 20151020
             cux_pick_sub_doc      cpd,
             mtl_system_items_b    msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
       and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
       and cpd.req_header_id = p_header_id
       and cpd.cux_pick_line_id = b.LINE_ID
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020
  
    cursor c_22_old is
      select b.LINE_ID
        from /*cux_oe_ship_headers_v a,*/
             /*cux_oe_ship_lines_v b,*/ cux.cux_oe_ship_lines b, --modified on 20151020
             --cux_pick_sub_doc      cpd,
             mtl_system_items_b msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
       and b.ISSUE_QUANTITY = 0
       and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
       and b.HEADER_ID = p_header_id
      
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020
  
    cursor c_22_new is
      select b.LINE_ID
        from /*cux_oe_ship_headers_v a,*/
             /*cux_oe_ship_lines_v b,*/ cux.cux_oe_ship_lines b, --modified on 20151020
             cux_pick_sub_doc      cpd,
             mtl_system_items_b    msi
       where /*a.header_id = b.HEADER_ID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
       and b.FLAG = 'Y'
       and b.ISSUE_QUANTITY = 0
       and msi.INVENTORY_ITEM_FLAG = 'Y'
      /*and a.header_id = p_header_id*/
       and cpd.req_header_id = p_header_id
       and cpd.cux_pick_line_id = b.LINE_ID
      /*order by b.LINE_NUMBER*/
      ; --modified on 20151020
  
    v_count number;
    form_trigger_failure exception;
    L_RETURN_STATUS           varchar2(2000);
    L_MSG_COUNT               number;
    L_MSG_DATA                varchar2(2000);
    v_L_QOH                   number;
    v_L_RQOH                  number;
    v_L_QR                    number;
    v_L_QS                    number;
    v_L_ATT                   number;
    v_L_ATR                   number;
    V_SHIP_FROM_ORG_ID        number;
    v_pick_from_subinventory1 varchar2(2000);
    V_ORG_ID                  number;
    V_PRODUCTS_OU_ID          number;
    L_IFACE_REC               MTL_TRANSACTIONS_INTERFACE%ROWTYPE;
    V_TXN_ID                  NUMBER;
    L_RESULT                  NUMBER;
    L_TRANS_COUNT             NUMBER;
    V_ERROR_CODE              varchar2(100);
    V_UNIT_SELLING_PRICE      NUMBER;
    V_TRANSACTIONAL_CURR_CODE varchar2(100);
    v_conversion_rate         NUMBER;
    V_FLAG                    varchar2(100);
    V_TAX_CODE                varchar2(100);
    v_PERCENTAGE_RATE         NUMBER;
    --------------------------------
    v_line_tbl   inv_move_order_pub.trolin_tbl_type;
    l_trolin_tbl inv_move_order_pub.trolin_tbl_type;
    l_trolin_rec inv_move_order_pub.trolin_rec_type;
    l_rsr_type   inv_reservation_global.mtl_reservation_tbl_type;
    l_mold_tbl   inv_mo_line_detail_util.g_mmtt_tbl_type;
    l_api_name       CONSTANT VARCHAR2(30) := 'allocate_move_order';
    l_savepoint_name CONSTANT VARCHAR2(30) := 'allocate_move_order';
    v_return_status         VARCHAR2(30);
    v_msg_count             NUMBER;
    v_msg_data              VARCHAR2(4000);
    v_msg_index_out         NUMBER;
    v_msg_data2             VARCHAR2(2000);
    v_qty_detailed          NUMBER;
    v_qty_delivered         NUMBER;
    i                       number;
    v_return                NUMBER;
    v_REQUEST_NUMBER        VARCHAR2(2000);
    L_API_VERSION           NUMBER := 1.0;
    L_INIT_MSG_LIST         VARCHAR2(2) := FND_API.G_TRUE;
    L_RETURN_VALUES         VARCHAR2(2) := FND_API.G_FALSE;
    L_COMMIT                VARCHAR2(2) := FND_API.G_FALSE;
    L_MOVE_ORDER_TYPE       NUMBER := 3;
    X_MOLD_TBL              INV_MO_LINE_DETAIL_UTIL.G_MMTT_TBL_TYPE;
    X_TROLIN_TBL            INV_MOVE_ORDER_PUB.TROLIN_TBL_TYPE;
    L_MSG_RETURN            NUMBER;
    X_NUMBER_OF_ROWS        NUMBER;
    X_DETAILED_QTY          number;
    X_REVISION              VARCHAR2(20);
    X_LOCATOR_ID            NUMBER;
    X_TRANSFER_TO_LOCATION  NUMBER;
    X_LOT_NUMBER            VARCHAR2(80);
    X_EXPIRATION_DATE       DATE;
    X_TRANSACTION_TEMP_ID   NUMBER;
    P_TRANSACTION_HEADER_ID NUMBER;
    P_TRANSACTION_MODE      NUMBER;
    P_MOVE_ORDER_TYPE       NUMBER := 3;
    v_qty_yj                NUMBER;
    V_XQ_QTY                NUMBER;
    V_SUBINVENTORY_CODE     VARCHAR2(80);
    V_SUBINVENTORY_CODE_R   VARCHAR2(80);
    V_LOCATION_ID_R         NUMBER;
  
    v_count_req number;
  begin
    /*select count(*)
      into v_count
      from cux_oe_ship_lines_v b
     where b.HEADER_ID = p_header_id
       and b.FLAG = 'Y';
    if v_count = 0 then
      p_txt := '未勾选记录';
      raise form_trigger_failure;
    end if;*/ --modified on 20151020
  
    /*    for x in c_1 loop
      --if p_ORGANIZATION_ID not in (85,86) then 
     if x.software_flag = 'Y' then
        select decode(p_ORGANIZATION_ID, 84, 'G999', 'H999')
          into x.pick_from_subinventory
          from dual;
     end if;     
      inv_quantity_tree_pub.clear_quantity_cache;
      INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                             P_INIT_MSG_LST        => NULL,
                                             X_RETURN_STATUS       => L_RETURN_STATUS,
                                             X_MSG_COUNT           => L_MSG_COUNT,
                                             X_MSG_DATA            => L_MSG_DATA,
                                             P_ORGANIZATION_ID     => p_ORGANIZATION_ID, --库存组织ID
                                             P_INVENTORY_ITEM_ID   => x.inventory_item_id, --物料ID
                                             P_TREE_MODE           => 3,
                                             P_IS_REVISION_CONTROL => FALSE,
                                             P_IS_LOT_CONTROL      => FALSE,
                                             P_IS_SERIAL_CONTROL   => FALSE,
                                             P_REVISION            => NULL,
                                             P_LOT_NUMBER          => NULL,
                                             P_LOT_EXPIRATION_DATE => NULL,
                                             P_SUBINVENTORY_CODE   => x.pick_from_subinventory, --子库code
                                             P_LOCATOR_ID          => x.LOCATOR_ID,
                                             P_COST_GROUP_ID       => NULL,
                                             P_ONHAND_SOURCE       => 3,
                                             X_QOH                 => v_L_QOH, --现有量
                                             X_RQOH                => v_L_RQOH,
                                             X_QR                  => v_L_QR,
                                             X_QS                  => v_L_QS,
                                             X_ATT                 => v_L_ATT,
                                             X_ATR                 => v_L_ATR);
      if nvl(v_L_ATT, 0) < x.QUANTITY then
        p_txt := x.segment1 || '-现有量不足,仓库'||x.pick_from_subinventory;
        raise form_trigger_failure;
      end if;
    
      
    end loop;*/
  
    begin
      select count(1)
        into v_count_req
        from cux_pick_sub_doc cpd
       where cpd.req_header_id = p_header_id;
    exception
      when others then
        v_count_req := 0;
    end;
  
    --p_txt:='a';
    if (v_count_req > 0) then
    
      for y in c_21_new loop
      
        if y.QUANTITY < 0 then
          p_txt := y.segment1 || '-处理量小于零';
          raise form_trigger_failure;
        end if;
      end loop;
    
      ---判断硬件数量是否大于零
      select sum(b.ISSUE_QUANTITY) QUANTITY
        into v_qty_yj
        from /*cux_oe_ship_headers_v   a,*/ --modified on 20151020
             /* cux_oe_ship_lines_v     b,*/ --modified on 20151021
              cux.cux_oe_ship_lines b,
             cux.cux_pick_sub_doc  cpd,
             --cux_om_line_interface_v c,
             mtl_system_items_b msi
       where /*a.header_id = b.HEADER_ID--modified on 20151020
                                                                                                                                               and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
      -- and c.ORDER_LINE_ID = b.OE_LINE_ID
       and b.FLAG = 'Y'
       and msi.INVENTORY_ITEM_FLAG = 'Y'
       and nvl(msi.attribute12, 'N') = 'N'
      --and b.HEADER_ID=p_header_id
       and cpd.req_header_id = p_header_id
       and cpd.cux_pick_line_id = b.LINE_ID;
      /*and a.header_id = p_header_id*/
      --modified on 20151020
    
    else
    
      for y in c_21_old loop
      
        if y.QUANTITY < 0 then
          p_txt := y.segment1 || '-处理量小于零';
          raise form_trigger_failure;
        end if;
      end loop;
    
      ---判断硬件数量是否大于零
      select sum(b.ISSUE_QUANTITY) QUANTITY
        into v_qty_yj
        from /*cux_oe_ship_headers_v   a,*/ --modified on 20151020
             /* cux_oe_ship_lines_v     b,*/ --modified on 20151021
              cux.cux_oe_ship_lines b,
             --cux.cux_pick_sub_doc    cpd,
             --cux_om_line_interface_v c,
             mtl_system_items_b msi
       where /*a.header_id = b.HEADER_ID--modified on 20151020
                                                                                                                                               and */
       msi.inventory_item_id = b.INVENTORY_ITEM_ID
       and msi.organization_id = p_ORGANIZATION_ID
      --and c.ORDER_LINE_ID = b.OE_LINE_ID
       and b.FLAG = 'Y'
       and msi.INVENTORY_ITEM_FLAG = 'Y'
       and nvl(msi.attribute12, 'N') = 'N'
      --and b.HEADER_ID=p_header_id
       and b.header_id = p_header_id;
      --and cpd.cux_pick_line_id = b.LINE_ID;
      /*and a.header_id = p_header_id*/
      --modified on 20151020
    
    end if;
  
    if v_qty_yj <= 0 then
      p_txt := '硬件处理量小于等于零';
      raise form_trigger_failure;
    end if;
  --p_txt:='b';
    if (v_count_req > 0) then
      for y in c_2_new loop
        select count(*)
          into v_count
          from wsh_delivery_details wdd
         where wdd.delivery_detail_id = y.delivery_detail_id
           and wdd.released_status in ('S');
        if v_count = 0 then
          p_txt := '不是已发放至仓库,物料搬运单已经处理';
          raise form_trigger_failure;
        end if;
      
        select decode(col.SOURCES_OF_PRODUCTS, 'FW', 84, 'JW', 83)
          into V_SHIP_FROM_ORG_ID
          from cux_om_line_interface_v col
         where col.ORDER_LINE_ID = y.oe_line_id;
      
        --判断是否是三角贸易
        if V_SHIP_FROM_ORG_ID <> p_ORGANIZATION_ID then
          p_txt := '物料:' || y.segment1 || '是三角贸易的物料';
          raise form_trigger_failure;
        end if;
      
        if y.software_flag = 'Y' then
          select decode(p_ORGANIZATION_ID, 84, 'G999', 'H999')
            into y.pick_from_subinventory
            from dual;
        end if;
      
      end loop;
    else
      for y in c_2_old loop
        select count(*)
          into v_count
          from wsh_delivery_details wdd
         where wdd.delivery_detail_id = y.delivery_detail_id
           and wdd.released_status in ('S');
        if v_count = 0 then
          p_txt := '不是已发放至仓库,物料搬运单已经处理';
          raise form_trigger_failure;
        end if;
      
        select decode(col.SOURCES_OF_PRODUCTS, 'FW', 84, 'JW', 83)
          into V_SHIP_FROM_ORG_ID
          from cux_om_line_interface_v col
         where col.ORDER_LINE_ID = y.oe_line_id;
      
        --判断是否是三角贸易
        if V_SHIP_FROM_ORG_ID <> p_ORGANIZATION_ID then
          p_txt := '物料:' || y.segment1 || '是三角贸易的物料';
          raise form_trigger_failure;
        end if;
      
        if y.software_flag = 'Y' then
          select decode(p_ORGANIZATION_ID, 84, 'G999', 'H999')
            into y.pick_from_subinventory
            from dual;
        end if;
      
      end loop;
    end if;
  
    ------------------------------------开始物料搬运单处理-----------------------------
    /*SELECT DISTINCT A.REQUEST_NUMBER
     INTO V_REQUEST_NUMBER
     FROM CUX_OE_SHIP_HEADERS_V A
    WHERE A.HEADER_ID = P_HEADER_ID;*/
    --modified by bruce on 2015-8-4
    /* select cpd.request_number
     into v_REQUEST_NUMBER
     from cux_pick_sub_doc cpd
    where cpd.req_header_id = p_header_id
      and rownum = 1;*/
  --p_txt:='c';
    if (v_count_req > 0) then
      FOR Z IN cur_new_req LOOP
        ---更新物料搬运单数量
        IF Z.ISSUE_QUANTITY > 0 THEN
          UPDATE MTL_TXN_REQUEST_LINES A
             SET A.QUANTITY = Z.ISSUE_QUANTITY
           WHERE A.LINE_ID = Z.LINE_ID;
          COMMIT;
          -----开始库存量验证
        --  p_txt:='d';
          INV_QUANTITY_TREE_PUB.CLEAR_QUANTITY_CACHE;
          INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                                 P_INIT_MSG_LST        => NULL,
                                                 X_RETURN_STATUS       => L_RETURN_STATUS,
                                                 X_MSG_COUNT           => L_MSG_COUNT,
                                                 X_MSG_DATA            => L_MSG_DATA,
                                                 P_ORGANIZATION_ID     => Z.ORGANIZATION_ID, --库存组织ID
                                                 P_INVENTORY_ITEM_ID   => Z.INVENTORY_ITEM_ID, --物料ID
                                                 P_TREE_MODE           => 3,
                                                 P_IS_REVISION_CONTROL => FALSE,
                                                 P_IS_LOT_CONTROL      => FALSE,
                                                 P_IS_SERIAL_CONTROL   => FALSE,
                                                 P_REVISION            => NULL,
                                                 P_LOT_NUMBER          => NULL,
                                                 P_LOT_EXPIRATION_DATE => NULL,
                                                 P_SUBINVENTORY_CODE   => Z.FROM_SUBINVENTORY_CODE, --子库CODE
                                                 P_LOCATOR_ID          => /*NULL*/ Z.FROM_LOCATOR_ID,
                                                 P_COST_GROUP_ID       => NULL,
                                                 P_ONHAND_SOURCE       => 3,
                                                 X_QOH                 => V_L_QOH, --现有量
                                                 X_RQOH                => V_L_RQOH,
                                                 X_QR                  => V_L_QR,
                                                 X_QS                  => V_L_QS,
                                                 X_ATT                 => V_L_ATT,
                                                 X_ATR                 => V_L_ATR);
          IF NVL(V_L_ATT, 0) < Z.ISSUE_QUANTITY THEN
            P_TXT := Z.SEGMENT1 || '-现有量不足,仓库' || Z.FROM_SUBINVENTORY_CODE;
            RAISE FORM_TRIGGER_FAILURE;
          END IF;
        END IF;
      END LOOP;
    else
      FOR Z IN cur_old_req LOOP
        ---更新物料搬运单数量
        IF Z.ISSUE_QUANTITY > 0 THEN
          UPDATE MTL_TXN_REQUEST_LINES A
             SET A.QUANTITY = Z.ISSUE_QUANTITY
           WHERE A.LINE_ID = Z.LINE_ID;
          COMMIT;
          -----开始库存量验证
          INV_QUANTITY_TREE_PUB.CLEAR_QUANTITY_CACHE;
          INV_QUANTITY_TREE_PUB.QUERY_QUANTITIES(P_API_VERSION_NUMBER  => 1.1,
                                                 P_INIT_MSG_LST        => NULL,
                                                 X_RETURN_STATUS       => L_RETURN_STATUS,
                                                 X_MSG_COUNT           => L_MSG_COUNT,
                                                 X_MSG_DATA            => L_MSG_DATA,
                                                 P_ORGANIZATION_ID     => Z.ORGANIZATION_ID, --库存组织ID
                                                 P_INVENTORY_ITEM_ID   => Z.INVENTORY_ITEM_ID, --物料ID
                                                 P_TREE_MODE           => 3,
                                                 P_IS_REVISION_CONTROL => FALSE,
                                                 P_IS_LOT_CONTROL      => FALSE,
                                                 P_IS_SERIAL_CONTROL   => FALSE,
                                                 P_REVISION            => NULL,
                                                 P_LOT_NUMBER          => NULL,
                                                 P_LOT_EXPIRATION_DATE => NULL,
                                                 P_SUBINVENTORY_CODE   => Z.FROM_SUBINVENTORY_CODE, --子库CODE
                                                 P_LOCATOR_ID          => /*NULL*/ Z.FROM_LOCATOR_ID,
                                                 P_COST_GROUP_ID       => NULL,
                                                 P_ONHAND_SOURCE       => 3,
                                                 X_QOH                 => V_L_QOH, --现有量
                                                 X_RQOH                => V_L_RQOH,
                                                 X_QR                  => V_L_QR,
                                                 X_QS                  => V_L_QS,
                                                 X_ATT                 => V_L_ATT,
                                                 X_ATR                 => V_L_ATR);
          IF NVL(V_L_ATT, 0) < Z.ISSUE_QUANTITY THEN
            P_TXT := Z.SEGMENT1 || '-现有量不足,仓库' || Z.FROM_SUBINVENTORY_CODE;
            RAISE FORM_TRIGGER_FAILURE;
          END IF;
        END IF;
      END LOOP;
    end if;
  
    if (v_count_req > 0) then
      FOR Z IN cur_new_req LOOP
        --处理物料搬运单
       -- p_txt:='e';
        IF Z.ISSUE_QUANTITY > 0 THEN
         --  p_txt:='f';
          INV_REPLENISH_DETAIL_PUB.LINE_DETAILS_PUB(P_LINE_ID               => Z.LINE_ID,
                                                    X_NUMBER_OF_ROWS        => X_NUMBER_OF_ROWS,
                                                    X_DETAILED_QTY          => X_DETAILED_QTY,
                                                    X_RETURN_STATUS         => V_RETURN_STATUS,
                                                    X_MSG_COUNT             => V_MSG_COUNT,
                                                    X_MSG_DATA              => V_MSG_DATA,
                                                    X_REVISION              => X_REVISION,
                                                    X_LOCATOR_ID            => X_LOCATOR_ID,
                                                    X_TRANSFER_TO_LOCATION  => X_TRANSFER_TO_LOCATION,
                                                    X_LOT_NUMBER            => X_LOT_NUMBER,
                                                    X_EXPIRATION_DATE       => X_EXPIRATION_DATE,
                                                    X_TRANSACTION_TEMP_ID   => X_TRANSACTION_TEMP_ID,
                                                    P_TRANSACTION_HEADER_ID => NULL,
                                                    P_TRANSACTION_MODE      => 1,
                                                    P_MOVE_ORDER_TYPE       => P_MOVE_ORDER_TYPE,
                                                    P_SERIAL_FLAG           => FND_API.G_FALSE,
                                                    P_PLAN_TASKS            => FALSE,
                                                    P_AUTO_PICK_CONFIRM     => FALSE,
                                                    P_COMMIT                => FALSE);
         -- p_txt:='w'||v_RETURN_STATUS||V_MSG_DATA;
          IF (NVL(v_RETURN_STATUS, 'NA') <> FND_API.G_RET_STS_SUCCESS) THEN
            P_TXT := P_TXT || '搬运单保留失败';
            RAISE FORM_TRIGGER_FAILURE;
          END IF;
          --更新事务处理日期为当前日期i
         --  p_txt:='t';
          UPDATE MTL_MATERIAL_TRANSACTIONS_TEMP A
             SET A.TRANSACTION_DATE = SYSDATE
           WHERE A.TRANSACTION_TEMP_ID = X_TRANSACTION_TEMP_ID;
        -- p_txt:='y'||z.line_id;
          L_TROLIN_TBL := INV_TROLIN_UTIL.QUERY_ROWS(P_LINE_ID => z.line_id);
        --  p_txt:='x';
          L_MOLD_TBL   := INV_MO_LINE_DETAIL_UTIL.QUERY_ROWS(P_LINE_ID => z.line_id);
         -- p_txt:='k';
          INV_PICK_WAVE_PICK_CONFIRM_PUB.PICK_CONFIRM(P_API_VERSION_NUMBER => L_API_VERSION,
                                                      P_INIT_MSG_LIST      => L_INIT_MSG_LIST,
                                                      P_COMMIT             => L_COMMIT,
                                                      X_RETURN_STATUS      => V_RETURN_STATUS,
                                                      X_MSG_COUNT          => V_MSG_COUNT,
                                                      X_MSG_DATA           => V_MSG_DATA,
                                                      P_MOVE_ORDER_TYPE    => L_MOVE_ORDER_TYPE,
                                                      P_TRANSACTION_MODE   => 1,
                                                      P_TROLIN_TBL         => L_TROLIN_TBL,
                                                      P_MOLD_TBL           => L_MOLD_TBL,
                                                      X_MMTT_TBL           => X_MOLD_TBL,
                                                      X_TROLIN_TBL         => X_TROLIN_TBL);
        -- p_txt:='u';
          IF (v_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS) THEN
            P_TXT := P_TXT || '搬运单确认失败';
            RAISE FORM_TRIGGER_FAILURE;
          END IF;
          UPDATE CUX.CUX_OE_SHIP_LINES B
             SET B.ATTRIBUTE13 = 'Y', B.TRANSACTION_ID = -1
           WHERE B.LINE_ID = Z.SHIP_LINE_ID;
        ELSE
          INV_MO_BACKORDER_PVT.BACKORDER(X_RETURN_STATUS => V_RETURN_STATUS,
                                         X_MSG_COUNT     => V_MSG_COUNT,
                                         X_MSG_DATA      => V_MSG_DATA,
                                         P_LINE_ID       => Z.LINE_ID);
          UPDATE CUX.CUX_OE_SHIP_LINES B
             SET B.ATTRIBUTE13 = 'Y', B.TRANSACTION_ID = -1
           WHERE B.LINE_ID = Z.SHIP_LINE_ID;
        END IF;
      END LOOP;
    
      --把零数据设置成已经处理
      FOR Y IN c_22_new LOOP
       --p_txt:='q';
        UPDATE CUX.CUX_OE_SHIP_LINES B
           SET B.ATTRIBUTE13 = 'Y', B.TRANSACTION_ID = -1
         WHERE B.LINE_ID = Y.LINE_ID;
      END LOOP;
    
    else
      FOR Z IN cur_old_req LOOP
        --处理物料搬运单
        IF Z.ISSUE_QUANTITY > 0 THEN
          INV_REPLENISH_DETAIL_PUB.LINE_DETAILS_PUB(P_LINE_ID               => Z.LINE_ID,
                                                    X_NUMBER_OF_ROWS        => X_NUMBER_OF_ROWS,
                                                    X_DETAILED_QTY          => X_DETAILED_QTY,
                                                    X_RETURN_STATUS         => V_RETURN_STATUS,
                                                    X_MSG_COUNT             => V_MSG_COUNT,
                                                    X_MSG_DATA              => V_MSG_DATA,
                                                    X_REVISION              => X_REVISION,
                                                    X_LOCATOR_ID            => X_LOCATOR_ID,
                                                    X_TRANSFER_TO_LOCATION  => X_TRANSFER_TO_LOCATION,
                                                    X_LOT_NUMBER            => X_LOT_NUMBER,
                                                    X_EXPIRATION_DATE       => X_EXPIRATION_DATE,
                                                    X_TRANSACTION_TEMP_ID   => X_TRANSACTION_TEMP_ID,
                                                    P_TRANSACTION_HEADER_ID => NULL,
                                                    P_TRANSACTION_MODE      => 1,
                                                    P_MOVE_ORDER_TYPE       => P_MOVE_ORDER_TYPE,
                                                    P_SERIAL_FLAG           => FND_API.G_FALSE,
                                                    P_PLAN_TASKS            => FALSE,
                                                    P_AUTO_PICK_CONFIRM     => FALSE,
                                                    P_COMMIT                => FALSE);
          IF (NVL(v_RETURN_STATUS, 'NA') <> FND_API.G_RET_STS_SUCCESS) THEN
            P_TXT := P_TXT || '搬运单保留失败';
            RAISE FORM_TRIGGER_FAILURE;
          END IF;
          --更新事务处理日期为当前日期i
          UPDATE MTL_MATERIAL_TRANSACTIONS_TEMP A
             SET A.TRANSACTION_DATE = SYSDATE
           WHERE A.TRANSACTION_TEMP_ID = X_TRANSACTION_TEMP_ID;
        
          L_TROLIN_TBL := INV_TROLIN_UTIL.QUERY_ROWS(P_LINE_ID => z.line_id);
          L_MOLD_TBL   := INV_MO_LINE_DETAIL_UTIL.QUERY_ROWS(P_LINE_ID => z.line_id);
          INV_PICK_WAVE_PICK_CONFIRM_PUB.PICK_CONFIRM(P_API_VERSION_NUMBER => L_API_VERSION,
                                                      P_INIT_MSG_LIST      => L_INIT_MSG_LIST,
                                                      P_COMMIT             => L_COMMIT,
                                                      X_RETURN_STATUS      => V_RETURN_STATUS,
                                                      X_MSG_COUNT          => V_MSG_COUNT,
                                                      X_MSG_DATA           => V_MSG_DATA,
                                                      P_MOVE_ORDER_TYPE    => L_MOVE_ORDER_TYPE,
                                                      P_TRANSACTION_MODE   => 1,
                                                      P_TROLIN_TBL         => L_TROLIN_TBL,
                                                      P_MOLD_TBL           => L_MOLD_TBL,
                                                      X_MMTT_TBL           => X_MOLD_TBL,
                                                      X_TROLIN_TBL         => X_TROLIN_TBL);
        
          IF (v_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS) THEN
            P_TXT := P_TXT || '搬运单确认失败';
            RAISE FORM_TRIGGER_FAILURE;
          END IF;
          UPDATE CUX.CUX_OE_SHIP_LINES B
             SET B.ATTRIBUTE13 = 'Y', B.TRANSACTION_ID = -1
           WHERE B.LINE_ID = Z.SHIP_LINE_ID;
        ELSE
          INV_MO_BACKORDER_PVT.BACKORDER(X_RETURN_STATUS => V_RETURN_STATUS,
                                         X_MSG_COUNT     => V_MSG_COUNT,
                                         X_MSG_DATA      => V_MSG_DATA,
                                         P_LINE_ID       => Z.LINE_ID);
          UPDATE CUX.CUX_OE_SHIP_LINES B
             SET B.ATTRIBUTE13 = 'Y', B.TRANSACTION_ID = -1
           WHERE B.LINE_ID = Z.SHIP_LINE_ID;
        END IF;
      END LOOP;
    
      --把零数据设置成已经处理
      FOR Y IN c_22_old LOOP
      
        UPDATE CUX.CUX_OE_SHIP_LINES B
           SET B.ATTRIBUTE13 = 'Y', B.TRANSACTION_ID = -1
         WHERE B.LINE_ID = Y.LINE_ID;
      END LOOP;
    
    end if;
  
    --判断单据是否处理完成
    SELECT COUNT(*)
      INTO V_COUNT
      FROM CUX_PICK_SUB_DOC CPD
     WHERE CPD.REQ_HEADER_ID = P_HEADER_ID
       AND NOT EXISTS (SELECT 1
              FROM CUX.CUX_OE_SHIP_HEADERS COH
             WHERE COH.HEADER_ID = P_HEADER_ID);
  
    IF V_COUNT = 0 THEN
      UPDATE CUX.CUX_OE_SHIP_HEADERS A
         SET A.ATTRIBUTE1 = 'Y'
       WHERE A.HEADER_ID = P_HEADER_ID;
    ELSE
      UPDATE CUX_PICK_SUB_DOC CPD
         SET CPD.ATTRIBUTE1 = 'Y'
       where cpd.req_header_id = p_header_id;
    END IF;
    P_FLAG := 'S';
    /* ELSE
       P_FLAG := 'E';  
    END IF;  */
    COMMIT;
  EXCEPTION
    WHEN FORM_TRIGGER_FAILURE THEN
      P_FLAG := 'E';
      ROLLBACK;
    when others then
      p_txt  := p_txt||'OTHER ERROR1:' || SQLERRM;
      P_FLAG := 'E';
      ROLLBACK;
  END;
  procedure print_error_proc(p_header_id in number) is
  
    cursor cur_err is
      select csh.request_number fh_number,
             cpd.request_number,
             decode(cpd.software_flag,
                    'Y',
                    cpd.soft_subinv,
                    cpd.pick_subinv) pick_subinv,
             --cpd.pick_locator_id,
             --cpd.software_flag,
             csl.inventory_item_id,
             csl.oe_line_id,
             ool.line_number,
             msib.segment1,
             cpd.process_code,
             cpd.error_msg
        from cux_pick_sub_doc        cpd,
             cux.cux_oe_ship_headers csh,
             cux.cux_oe_ship_lines   csl,
             mtl_system_items_b      msib,
             oe_order_lines          ool
       where cpd.cux_pick_header_id = p_header_id
         and cpd.cux_pick_header_id = csh.header_id
         and cpd.cux_pick_line_id = csl.line_id
         and msib.inventory_item_id = csl.inventory_item_id
         and msib.organization_id = ool.ship_from_org_id
         and csl.oe_line_id = ool.line_id
      /* and cpd.process_code = 'E'*/
       ORDER BY cpd.process_code;
  
  begin
    fnd_file.PUT_LINE(fnd_file.OUTPUT,
                      '发货单号        搬运单号        子库     订单行     物料          状态   错误信息');
    for rec_err in cur_err loop
    
      fnd_file.PUT_LINE(fnd_file.OUTPUT,
                        rec_err.fh_number || '        ' ||
                        rec_err.request_number || '        ' ||
                        rec_err.pick_subinv || '     ' ||
                        rec_err.Line_Number || '     ' || rec_err.Segment1 ||
                        '          ' || rec_err.Process_Code || '   ' ||
                        rec_err.error_msg);
    
    end loop;
  exception
    when others then
      NULL;
  end;
  procedure process_dup_item(p_header_id       in number,
                             p_organization_id number,
                             x_ret_code        out varchar2,
                             x_ret_msg         out varchar2) is
    v_current_subinv    varchar2(10);
    v_shipping_org_code varchar2(10);
    v_count_subinv      number;
    V_SHIP_FROM_ORG_ID  number := 0;
    v_suggest_sub_a     varchar2(10);
    v_item_avail_qty    number;
    e_exception exception;
    cursor c_2 is
       select b.INVENTORY_ITEM_ID,
              b.LINE_ID,
              c.SOURCES_OF_PRODUCTS
         from cux.cux_oe_ship_lines b,
              cux_om_line_interface c,
              mtl_system_items_b    msi                                                                                                                                                                                          oe_order_lines_all    oola*/
        where b.header_id = p_header_id
          and msi.inventory_item_id = b.INVENTORY_ITEM_ID
          and msi.organization_id = p_ORGANIZATION_ID
          and c.ORDER_LINE_ID = b.OE_LINE_ID
          and b.FLAG = 'Y'
          and msi.INVENTORY_ITEM_FLAG = 'Y';
  
    CURSOR CUR_DUP_ITEM(p_source_org_id number) IS
       SELECT col.inventory_item_id,
              cpd.pick_subinv,
              sum(col.quantity) total_req_qty
         FROM cux.cux_oe_ship_lines     col,
              cux_pick_sub_doc          cpd,
              mtl_secondary_inventories msi
        where col.header_id = p_header_id
          and cpd.cux_pick_line_id = col.line_id
          and cpd.pick_subinv = msi.secondary_inventory_name
          and msi.organization_id = p_source_org_id
          and msi.attribute5 = 'B'
        group by col.inventory_item_id, cpd.pick_subinv
       having count(*) > 1;
  begin
    x_ret_code := 'S';
    x_ret_msg  := '';
    for y in c_2 loop
    
      if (V_SHIP_FROM_ORG_ID = 0) then
      
         BEGIN
           SELECT decode(y.SOURCES_OF_PRODUCTS, 'JW', 83, 'FW', '84')
             INTO V_SHIP_FROM_ORG_ID
             FROM dual;
         EXCEPTION
            WHEN OTHERS THEN
               x_ret_code := 'E';
               x_ret_msg  := '获取发货组织ID出错' || sqlerrm;
         END;
      end if;
      
      -- 产品来源
      --判断当前物料是否存在多个出货子库，如果存在，则统一改为151子库
      v_shipping_org_code := y.SOURCES_OF_PRODUCTS;
      begin
        select cpd.pick_subinv
          into v_current_subinv
          from cux_pick_sub_doc cpd
         where cpd.cux_pick_header_id = p_header_id
           and cpd.cux_pick_line_id = y.line_id;
      exception
        when others then
          x_ret_code := 'E';
          x_ret_msg  := '获取当前子库出错' || sqlerrm;
      end;
    
      select count(1)
        into v_count_subinv
        from cux_pick_sub_doc cpd, cux.cux_oe_ship_lines col
       where cpd.cux_pick_header_id = p_header_id
         and cpd.cux_pick_line_id <> y.line_id
         and cpd.cux_pick_line_id = col.line_id
         and col.inventory_item_id = y.inventory_item_id
         and cpd.pick_subinv <> v_current_subinv;
    
      if (v_count_subinv > 0) then
        fnd_file.PUT_LINE(fnd_file.LOG,
                          'multiple subinv there, so update to 151 subinv');
        update CUX_PICK_SUB_DOC cpd
           set cpd.pick_subinv     = decode(v_shipping_org_code,
                                            'FW','G151',
                                            'JW','H151'),
               cpd.pick_locator_id = '',
               cpd.attribute10     = 'Y'
         where cpd.cux_pick_header_id = p_header_id
           and cpd.pick_subinv not like '%151'
           and cpd.cux_pick_line_id in
               (select col.line_id
                  from cux.cux_oe_ship_lines col
                 where col.inventory_item_id = y.inventory_item_id
                   and col.header_id = p_header_id);
      end if;
    
    end loop; --y loop
  
    -- 判断物料在B库的总需求量是否大于可用量.
    for rec_dup_item in CUR_DUP_ITEM(V_SHIP_FROM_ORG_ID) loop
      fnd_file.PUT_LINE(fnd_file.LOG,
                        'duplicate item:' || rec_dup_item.inventory_item_id);
      begin
      
        select nvl(cpd.avail_qty, 0)
          into v_item_avail_qty
          from cux.cux_oe_ship_lines col, 
               cux_pick_sub_doc cpd
         where col.header_id = p_header_id
           and col.line_id = cpd.cux_pick_line_id
           and col.inventory_item_id = rec_dup_item.inventory_item_id
           and cpd.pick_subinv = rec_dup_item.pick_subinv
           and rownum = 1;
      exception
        when others then
          v_item_avail_qty := 0;
        
      end;
    
      if (rec_dup_item.total_req_qty > v_item_avail_qty) then
        --如果物料在B库的需求总量大于可用量，那么都改为从A库出货
        fnd_file.PUT_LINE(fnd_file.LOG,
                          'total req qty greater avail qty, then fetch A subinv');
      
        select decode(v_shipping_org_code, 'FW', 'G151', 'JW', 'H151')
          into v_suggest_sub_a
          from dual;
      
        UPDATE cux_pick_sub_doc cpd
           set cpd.pick_subinv     = v_suggest_sub_a,
               cpd.pick_locator_id = '',
               cpd.attribute10     = 'Y' --ATTRIBUTE10打上标记，查询出发货单时显示黄色
         where cpd.cux_pick_header_id = p_header_id
           and cpd.cux_pick_line_id in
               (select col.line_id
                  from cux.cux_oe_ship_lines col
                 where col.inventory_item_id =
                       rec_dup_item.inventory_item_id
                   and col.header_id = p_header_id)
           and cpd.pick_subinv = rec_dup_item.pick_subinv;
      
      else
        null;
      end if;
    
    end loop;
  
  exception
    when e_exception then
      null;
    
    when others then
      x_ret_code := 'E';
      x_ret_msg  := '未定义错误：' || SQLERRM;
  end;
  
  function get_ship_doc_status(p_line_id in number, p_ship_qty in number)
    return varchar2 is
    l_ship_status varchar2(100);
    l_line_status number;
    l_mtl_qty     number;
    l_req_qty     number;
    l_count       number;
  begin
  
    if (p_line_id IS NOT NULL) then
             
      SELECT COUNT(1)
        INTO l_count
        FROM mtl_txn_request_lines mtl, wsh_delivery_details wdd
       where 1 = 1
         and mtl.line_id = p_line_id
         and wdd.move_order_line_id = mtl.line_id;
    
      if (l_count = 0) then
        l_ship_status := '已延交';
      else
      
        begin
          SELECT mtl.line_status
            into l_line_status
            FROM mtl_txn_request_lines mtl, wsh_delivery_details wdd
           where 1 = 1
             and mtl.line_id = p_line_id
             and wdd.move_order_line_id = mtl.line_id
             and rownum = 1;
        exception
          when others then
            l_line_status := 0;
          
        end;
      
        if (l_line_status = 5) then
        
          begin
            SELECT sum(wdd.requested_quantity)
              into l_req_qty
              FROM mtl_txn_request_lines mtl, wsh_delivery_details wdd
             where 1 = 1
               and mtl.line_id = p_line_id
               and wdd.move_order_line_id = mtl.line_id;
          exception
            when others then
              l_line_status := 0;
          end;
        
          if (l_req_qty <> p_ship_qty) then
            l_ship_status := '部分延交';
          end if;
        
        end if;
      
      end if; --l_COUNT>0
    
    
      return l_ship_status;
      else
        return '';
    end if; --p_line_id
  
  exception
    when others then
      l_ship_status := '未定义错误：' || SQLERRM;
      return l_ship_status;
  end;
END cux_oe_ship_pkg;
/
