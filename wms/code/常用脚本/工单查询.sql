-- 工单名称
select we.wip_entity_id,
       we.wip_entity_name,
       we.* 
 from wip_entities we 
where we.wip_entity_name = 'TEST8';

-- 工单需求
select wro.concatenated_segments 组件,
       wro.operation_seq_num 序号,
       wro.wip_supply_meaning 物料类型,
       wro.required_quantity 必须数量,
       wro.quantity_open     未完成数量,
       wro.quantity_issued   已发数,
       wro.quantity_per_assembly 单机用量,
       wro.component_yield_factor 产出率,
       -- wro.item_description,
       wro.* 
  from WIP_REQUIREMENT_OPERATIONS_V wro 
 where wro.wip_entity_id = 54146
 order by wip_supply_meaning;

-- 工单需求
select cri.line_id,
       cri.line_number 行号,
       cri.organization_id 组织ID,
       cri.doc_no 单号,
       
       cri.now_qty 下达套数,
       cri.ready_qty 数量,
       cri.actual_qty 实发数量,
       
       cri.operation_seq_num 序号,
       cri.operation_code 工序,
       
       
       cri.doc_type 单据类型,
       cri.doc_status 单据状态,
       cri.wip_entity_name 工单名,
       cri.primary_item_id 组件,
       cri.inventory_item_id 工单名,
       
       cri.supply_subinventory 来源子库,
       cri.supply_loc_code 来源货位,
       cri.supply_locator_id 来源货位ID,
       cri.to_sub 目标子库,
       cri.to_loc_code 目标货位,
       
       
       
       cri.* 
  from cux_ready_item cri 
 where cri.wip_entity_id = 54146;
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
