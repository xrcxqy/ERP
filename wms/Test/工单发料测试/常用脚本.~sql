

-- 工单信息
select wdj.organization_id,
       wdj.wip_entity_id,
       wdj.wip_entity_name,
       wdj.start_quantity,
       wdj.attribute3 车间,  -- 91004
       wdj.*
  from WIP_DISCRETE_JOBS_V wdj 
 where wdj.wip_entity_name = 'TBL02';
 
 
 select wro.inventory_item_id,
        wro.concatenated_segments,
        wro.operation_seq_num,
        wro.wip_supply_type,
        wro.required_quantity 需求数量,
        wro.quantity_issued 已发数量
   from WIP_REQUIREMENT_OPERATIONS_V wro
    
  where wro.wip_entity_id = 50146;
 
 
 select cri.doc_no,
        cri.doc_type,
        cri.doc_status,
        cri.wip_entity_id,
        cri.wip_entity_name,
        cri.supply_subinventory,
        cri.supply_loc_code,
        cri.now_qty,
        cri.inventory_item_id,
        cri.operation_code,
        cri.line_number,
        cri.ready_qty,
        cri.actual_qty,
        cri.quantity_issued,
        cri.transfer_status,
        cri.empty_return_flag,
        cri.* 
   from cux_ready_item cri 
  where cri.wip_entity_id = 50146;
 
 -- 合并备料单 
 select * from cux_wip_combination_details wcd where wcd.wip_entity_id = 50146;
 -- 判断是否齐套
 select * from cux_wip_full_prepare wfp where wfp.combination_id = 1874;
 
 -- 抛转数据
 select * from cux.cux_wip_batch_transfer_doc btd where btd.comb_id = 1874;
 
 -- 分群码 M013
 select * from mtl_system_items_b msi where msi.segment1 = 'V-01010309-103'


 -- 分群码
 select msi.segment1,msi.attribute7 from mtl_system_items_b msi where msi.segment1 = 'V-01010309-103'

