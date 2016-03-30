PL/SQL Developer Test script 3.0
29
declare
  -- Non-scalar parameters require additional processing 
  p_wip_header Cux_Wms_Material_Trx_Headers%rowtype;
  
  p_wip_lines cux_wms_pub.material_trx_lines_tab;
  
  l_Material_Trx_Line Cux_Wms_Material_Trx_Lines%ROWTYPE;
  new_line Cux_Wms_Material_Trx_Lines%ROWTYPE;
begin
  
  -- 单据号
  p_wip_header.wip_entity_name := 'TEST1';
  p_wip_header.organization_id := 84;
  p_wip_header.source_system := 'WMS';
  -- 单据类型
  p_wip_header.trx_order_type := 'WIP_RLS_PS';
  p_wip_header.trx_order_number := 'RD84646';
  
  l_Material_Trx_Line := new_line;
  l_Material_Trx_Line.Quantity := 100;
  l_Material_Trx_Line.Line_Number := 1;
  p_wip_lines(1) := l_Material_Trx_Line;
  
  -- Call the procedure
  cux_ebs_smtn_integration_pkg.process_wip_doc(p_wip_header => p_wip_header,
                                               p_wip_lines => p_wip_lines,
                                               x_ret_sts => :x_ret_sts,
                                               x_error_msg => :x_error_msg);
end;
2
x_ret_sts
1
锘縎
5
x_error_msg
0
5
0
