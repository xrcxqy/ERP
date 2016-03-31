create or replace view cux_ready_item_header_v as
select distinct
       cri.organization_id,
       cri.doc_type,
       cri.doc_no,
       cri.wip_entity_name,
       cri.wip_entity_id,
       cri.doc_date,
       wdj.start_quantity,
       cri.now_qty,
       cri.doc_status,
       cri.transfer_status transfer_status_code,--added by bruce on 20151027
       cri.operation_code,
       cri.created_by
  from cux_ready_item      cri,
       wip_discrete_jobs_v wdj
  where cri.organization_id=wdj.organization_id
  and cri.wip_entity_id=wdj.wip_entity_id
;
