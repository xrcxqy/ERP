select cml.header_id          头ID
      ,cml.line_id            行ID
      ,cml.organization_id    组织ID
      ,cml.inventory_item_id  物料ID
      ,cml.subinventory_code  子库
      ,cml.locator_id         货物ID
      ,cml.onhand_qty         可用量
      ,cml.primary_quantity   处理量
      ,cml.to_organization_id 目标组织
      ,cml.to_subinventory_code 目前仓库
      ,cml.to_locator_id        目标货位
      ,cml.transaction_date   事务处理日期
      ,cml.transaction_id     事务处理ID
      ,cml.reservation_id
      ,cml.status
      ,cml.item_cost
      ,cml.remark             备注
      ,cml.line_number        行号
      ,cml.contract_number    
  from cux_inv_material_lines cml 
 where cml.header_id = 456230
