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
 where wro.wip_entity_id = 54146;


select cri.line_id,
       cri.line_number,
       cri.doc_no,
       cri.doc_type,
       cri.doc_status,
       cri.wip_entity_name,
       cri.primary_item_id,
       cri.supply_subinventory
       cri.* 
  from cux_ready_item cri 
 where cri.wip_entity_id = 54146;
 
 
 
