CREATE OR REPLACE PACKAGE CUX_EBS_SMTN_INTEGRATION_PKG AUTHID CURRENT_USER IS
  /* $Header: CUX_EBS_SMTN_INTEGRATION_PKG.plb 120.0.1 2015/9/9$ */
  /**************************************************************************
  REM Copyright (c) 2015 ruijie Corporation. All rights reserved.
  REM ***********************************************************************
  REM File name                : 
  REM Doc Ref(s)               : 
  REM Project                  : RUIJIE ERP Project
  REM 
  REM Description: 
  REM Change History Information
  REM --------------------------
  REM 
  REM Version  Date         Author           Change Reference / Description
  REM -------  -----------  ---------------  ----------------------------------
  REM 1.0      9-SEP-15    Bruce        Creation
  REM *************************************************************************/

  --pl/sql table for storing index code/value
  /* TYPE rec_doc_header is record(
    doc_number        varchar2(100),
    source_doc_number varchar2(100),
    source_system     varchar2(30));
  
  TYPE rec_doc_line IS RECORD(
    line_number        number,
    source_line_number number,
    trx_qty            number,
    subinv             varchar2(10),
    locator_id         number,
    comments           varchar2(1000),
    rework_flag        varchar2(10),
    parent_trx_id      number);
  
  TYPE tbl_doc_line IS TABLE OF rec_doc_line INDEX BY BINARY_INTEGER;*/
  PROCEDURE get_dly_line_info(p_dly_line  IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                              x_Ret_Sts   OUT VARCHAR2,
                              x_Error_Msg OUT VARCHAR2);
  PROCEDURE get_rtn_line_info(p_rtn_line  IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                              x_Ret_Sts   OUT VARCHAR2,
                              x_Error_Msg OUT VARCHAR2);
  PROCEDURE get_zr_line_info(p_zr_header Cux_Wms_Material_Trx_Headers%rowtype,
                             p_zr_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                             x_Ret_Sts   OUT VARCHAR2,
                             x_Error_Msg OUT VARCHAR2);
  PROCEDURE get_zf_line_info(p_zf_header Cux_Wms_Material_Trx_Headers%rowtype,
                             p_zf_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                             x_Ret_Sts   OUT VARCHAR2,
                             x_Error_Msg OUT VARCHAR2);
  PROCEDURE get_db_line_info(p_db_header Cux_Wms_Material_Trx_Headers%rowtype,
                             p_db_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                             x_Ret_Sts   OUT VARCHAR2,
                             x_Error_Msg OUT VARCHAR2);
  PROCEDURE get_yddb_line_info(p_yddb_header Cux_Wms_Material_Trx_Headers%rowtype,
                               p_yddb_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                               x_Ret_Sts     OUT VARCHAR2,
                               x_Error_Msg   OUT VARCHAR2);
  PROCEDURE get_xodb_line_info(p_xodb_header Cux_Wms_Material_Trx_Headers%rowtype,
                               p_xodb_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                               x_Ret_Sts     OUT VARCHAR2,
                               x_Error_Msg   OUT VARCHAR2);
  PROCEDURE get_prepare_line_info(p_prepare_header Cux_Wms_Material_Trx_Headers%rowtype,
                                  p_prepare_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                                  x_Ret_Sts        OUT VARCHAR2,
                                  x_Error_Msg      OUT VARCHAR2);
  PROCEDURE get_replenish_line_info(p_replenish_header Cux_Wms_Material_Trx_Headers%rowtype,
                                    p_replenish_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                                    x_Ret_Sts          OUT VARCHAR2,
                                    x_Error_Msg        OUT VARCHAR2);
  PROCEDURE get_over_line_info(p_over_header Cux_Wms_Material_Trx_Headers%rowtype,
                               p_over_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                               x_Ret_Sts     OUT VARCHAR2,
                               x_Error_Msg   OUT VARCHAR2);
  PROCEDURE get_return_line_info(p_return_header Cux_Wms_Material_Trx_Headers%rowtype,
                                 p_return_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                                 x_Ret_Sts       OUT VARCHAR2,
                                 x_Error_Msg     OUT VARCHAR2);
  PROCEDURE get_complete_line_info(p_complete_header Cux_Wms_Material_Trx_Headers%rowtype,
                                   p_complete_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                                   x_Ret_Sts         OUT VARCHAR2,
                                   x_Error_Msg       OUT VARCHAR2);
  PROCEDURE split_rcv_line(p_delivery_header Cux_Wms_Material_Trx_Headers%rowtype,
                           p_delivery_lines  IN Cux_Wms_Pub.Material_Trx_Lines_Tab,
                           x_delivery_lines  OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                           x_Ret_Sts         OUT VARCHAR2,
                           x_Error_Msg       OUT VARCHAR2);
  PROCEDURE split_rtn_line(p_return_header Cux_Wms_Material_Trx_Headers%rowtype,
                           p_return_lines  IN Cux_Wms_Pub.Material_Trx_Lines_Tab,
                           x_return_lines  OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                           x_Ret_Sts       OUT VARCHAR2,
                           x_Error_Msg     OUT VARCHAR2);
  PROCEDURE create_delivery(p_delivery_header Cux_Wms_Material_Trx_Headers%rowtype,
                            p_delivery_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                            x_Ret_Sts         OUT VARCHAR2,
                            x_Error_Msg       OUT VARCHAR2);
  PROCEDURE process_return(p_return_header Cux_Wms_Material_Trx_Headers%rowtype,
                           p_return_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                           x_Ret_Sts       OUT VARCHAR2,
                           x_Error_Msg     OUT VARCHAR2);
  PROCEDURE process_inv_doc(p_inv_header Cux_Wms_Material_Trx_Headers%rowtype,
                            p_inv_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                            x_Ret_Sts    OUT VARCHAR2,
                            x_Error_Msg  OUT VARCHAR2);
  PROCEDURE process_zr(p_zr_header Cux_Wms_Material_Trx_Headers%rowtype,
                       p_zr_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                       x_Ret_Sts   OUT VARCHAR2,
                       x_Error_Msg OUT VARCHAR2);
  PROCEDURE process_zf(p_zf_header Cux_Wms_Material_Trx_Headers%rowtype,
                       p_zf_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                       x_Ret_Sts   OUT VARCHAR2,
                       x_Error_Msg OUT VARCHAR2);
  PROCEDURE process_db(p_db_header Cux_Wms_Material_Trx_Headers%rowtype,
                       p_db_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                       x_Ret_Sts   OUT VARCHAR2,
                       x_Error_Msg OUT VARCHAR2);
  PROCEDURE process_yddb(p_yddb_header Cux_Wms_Material_Trx_Headers%rowtype,
                         p_yddb_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                         x_Ret_Sts     OUT VARCHAR2,
                         x_Error_Msg   OUT VARCHAR2);
  PROCEDURE process_xodb(p_xodb_header Cux_Wms_Material_Trx_Headers%rowtype,
                         p_xodb_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                         x_Ret_Sts     OUT VARCHAR2,
                         x_Error_Msg   OUT VARCHAR2);
  PROCEDURE process_wip_doc(p_wip_header Cux_Wms_Material_Trx_Headers%rowtype,
                            p_wip_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                            x_Ret_Sts    OUT VARCHAR2,
                            x_Error_Msg  OUT VARCHAR2);
  PROCEDURE process_prepare_item(p_prepare_header Cux_Wms_Material_Trx_Headers%rowtype,
                                 p_prepare_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                                 x_Ret_Sts        OUT VARCHAR2,
                                 x_Error_Msg      OUT VARCHAR2);
  PROCEDURE process_replenish_item(p_replenish_header Cux_Wms_Material_Trx_Headers%rowtype,
                                   p_replenish_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                                   x_Ret_Sts          OUT VARCHAR2,
                                   x_Error_Msg        OUT VARCHAR2);
  PROCEDURE process_over_item(p_over_header Cux_Wms_Material_Trx_Headers%rowtype,
                              p_over_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                              x_Ret_Sts     OUT VARCHAR2,
                              x_Error_Msg   OUT VARCHAR2);
  PROCEDURE process_return_item(p_return_header Cux_Wms_Material_Trx_Headers%rowtype,
                                p_return_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                                x_Ret_Sts       OUT VARCHAR2,
                                x_Error_Msg     OUT VARCHAR2);
  PROCEDURE validate_return_item(p_return_header Cux_Wms_Material_Trx_Headers%rowtype,
                                 p_return_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                                 p_source_type   number,
                                 x_return_lines  OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                                 x_Ret_Sts       OUT VARCHAR2,
                                 x_Error_Msg     OUT VARCHAR2);
  PROCEDURE process_complete_item(p_complete_header Cux_Wms_Material_Trx_Headers%rowtype,
                                  p_complete_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                                  x_Ret_Sts         OUT VARCHAR2,
                                  x_Error_Msg       OUT VARCHAR2);
  PROCEDURE combine_rcv_to_smtn(p_header_id number,
                                x_Ret_Sts   OUT VARCHAR2,
                                x_Error_Msg OUT VARCHAR2);
  PROCEDURE combine_rtn_to_smtn(p_header_id number,
                                x_Ret_Sts   OUT VARCHAR2,
                                x_Error_Msg OUT VARCHAR2);
  PROCEDURE delete_reservation(p_header_id number,
                               x_Ret_Sts   OUT VARCHAR2,
                               x_Error_Msg OUT VARCHAR2);
  
    /**
   * 判断单据是否为WMS/SMTN仓库
   */                             
  FUNCTION is_inv_wms_sub(p_header_id       number,
                          p_organization_id number,
                          p_doc_number      varchar2,
                          p_trx_type        varchar2,
                          x_Ret_Sts         OUT VARCHAR2,
                          x_Error_Msg       OUT VARCHAR2) RETURN VARCHAR2;
                                                       
  /**
   * 判断单据是否抛到WMS/SMTN
   */                             
  FUNCTION is_inv_sys(p_header_id       number,
                      p_organization_id number,
                      p_doc_number      varchar2,
                      p_trx_type        varchar2,
                      x_Ret_Sts         OUT VARCHAR2,
                      x_Error_Msg       OUT VARCHAR2) RETURN VARCHAR2;
  
 
                      
                      
  FUNCTION get_trx_type(p_header_id  number,
                        p_doc_number varchar2,
                        p_doc_type   varchar2) RETURN VARCHAR2;
  PROCEDURE transfer_doc_by_batch(x_errbuf       OUT NOCOPY VARCHAR2,
                                  x_retcode      OUT NOCOPY NUMBER,
                                  p_batch_number varchar2);
  --PROCEDURE send_mail(p_doc_type varchar2, p_doc_id number);
END CUX_EBS_SMTN_INTEGRATION_PKG;
/
CREATE OR REPLACE PACKAGE BODY CUX_EBS_SMTN_INTEGRATION_PKG IS
  /* $Header: cux_ebs_qis_integration_pkg.plb 120.0.1 2015/7/2$ */
  /**************************************************************************
  REM Copyright (c) 2015 ruijie Corporation. All rights reserved.
  REM ***********************************************************************
  REM File name                : 
  REM Doc Ref(s)               : 
  REM Project                  : RUIJIE ERP Project
  REM 
  REM Description: 
  REM Change History Information
  REM --------------------------
  REM 
  REM Version  Date         Author           Change Reference / Description
  REM -------  -----------  ---------------  ----------------------------------
  REM 1.0      2-JUL-15    Bruce        Creation
  REM *************************************************************************/
  PROCEDURE get_dly_line_info(p_dly_line  IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                              x_Ret_Sts   OUT VARCHAR2,
                              x_Error_Msg OUT VARCHAR2) is
  
    l_count           number;
    l_comb_tag        varchar2(10);
    l_comb_to_line_id number;
  begin
  
    select count(1)
      into l_count
      from cux_receipts_headers crh
     where crh.receipt_number = p_dly_line.Order_Number
       and crh.receipt_status = 'TRANSFERRED';
  
    if (l_count > 0) then
    
      begin
        select crl.combine_tag, crl.receipt_line_id
          into l_comb_tag, l_comb_to_line_id
          from cux_receipts_headers crh, cux_receipts_lines crl
         where crh.receipt_number = p_dly_line.Order_Number
           and crl.line_number = p_dly_line.Line_Number
           and crh.receipt_id = crl.receipt_id;
      exception
        when others then
          x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
          x_Error_Msg := '获取合并行信息出错：' || sqlerrm;
          raise Fnd_Api.g_Exc_Error;
      end;
    
      if (l_comb_tag = 'O') then
        select crh.receipt_id,
               crl.receipt_line_id,
               crl.item_id,
               crh.organization_id,
               crl.default_subinv,
               crl.default_locator_id,
               crl.receipt_qty,
               /*crl.combine_tag,
               crl.combine_to_line_id,*/
               nvl(crl.delivered_qty, 0) +
               nvl(crl.returned_qty_from_rcv, 0)
          into p_dly_line.Header_Id,
               p_dly_line.Line_Id,
               p_dly_line.Item_Id,
               p_dly_line.To_Organization_Id,
               p_dly_line.To_Subinventory,
               p_dly_line.To_Locator_Id,
               p_dly_line.Orig_Quantity,
               /*p_dly_line.combine_tag,
               p_dly_line.combine_to_line_id,*/
               p_dly_line.Fulfilled_Quantity
          from cux_receipts_lines crl, cux_receipts_headers crh
         where crl.receipt_id = crh.receipt_id
           and crh.receipt_number = p_dly_line.Order_Number
           and crl.line_number = p_dly_line.Line_Number
              --and crl.combine_tag = 'O'
           and crh.receipt_status = 'TRANSFERRED';
      ELSIF (l_comb_tag = 'V') THEN
        select crh.receipt_id,
               crl.receipt_line_id,
               crl.item_id,
               crh.organization_id,
               crl.default_subinv,
               crl.default_locator_id,
               crl.receipt_qty,
               /*crl.combine_tag,
               crl.combine_to_line_id,*/
               
               (SELECT NVL(SUM(CRL2.DELIVERED_QTY), 0) +
                       NVL(SUM(CRL2.RETURNED_QTY_FROM_RCV), 0)
                  FROM CUX.CUX_RECEIPTS_LINES CRL2
                 WHERE CRL2.COMBINE_TO_LINE_ID = l_comb_to_line_id)
          into p_dly_line.Header_Id,
               p_dly_line.Line_Id,
               p_dly_line.Item_Id,
               p_dly_line.To_Organization_Id,
               p_dly_line.To_Subinventory,
               p_dly_line.To_Locator_Id,
               p_dly_line.Orig_Quantity,
               /*p_dly_line.combine_tag,
               p_dly_line.combine_to_line_id,*/
               p_dly_line.Fulfilled_Quantity
          from cux_receipts_lines crl, cux_receipts_headers crh
         where crl.receipt_id = crh.receipt_id
           and crh.receipt_number = p_dly_line.Order_Number
           and crl.line_number = p_dly_line.Line_Number
              --and crl.combine_tag = 'O'
           and crh.receipt_status = 'TRANSFERRED';
      
      end if;
    
    else
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据未抛转';
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误get dly line：' || sqlerrm ||
                     p_dly_line.Order_Number || p_dly_line.Line_Number;
  end;
  PROCEDURE get_rtn_line_info(p_rtn_line  IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                              x_Ret_Sts   OUT VARCHAR2,
                              x_Error_Msg OUT VARCHAR2) is
    l_count number;
  
  begin
  
    select count(1)
      into l_count
      from cux_return_headers crh
     where crh.return_number = p_rtn_line.Order_Number
       and crh.return_status = 'TRANSFERRED';
  
    IF (l_count > 0) THEN
    
      select crh.return_id,
             crl.return_line_id,
             crl.item_id,
             crh.organization_id,
             crl.return_subinv,
             crl.return_locator_id,
             crl.return_qty,
             /*crl.combine_tag,
             crl.combine_to_line_id,*/
             0
        INTO p_rtn_line.Header_Id,
             p_rtn_line.Line_Id,
             p_rtn_line.Item_Id,
             p_rtn_line.From_Organization_Id,
             p_rtn_line.From_Subinventory,
             p_rtn_line.From_Locator_Id,
             p_rtn_line.Orig_Quantity,
             /*p_rtn_line.combine_tag,
             p_rtn_line.combine_to_line_id,*/
             p_rtn_line.Fulfilled_Quantity
        from cux_return_lines crl, cux_return_headers crh /*,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         cux_delivery_lines cdl*/
       where crh.return_id = crl.return_id
         and crh.return_number = p_rtn_line.Order_Number
         and crl.line_number = p_rtn_line.Line_Number
            --and crl.delivery_line_id = cdl.delivery_line_id
         and crh.return_status = 'TRANSFERRED';
    
    ELSE
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据状态不正确';
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE get_zr_line_info(p_zr_header Cux_Wms_Material_Trx_Headers%rowtype,
                             p_zr_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                             x_Ret_Sts   OUT VARCHAR2,
                             x_Error_Msg OUT VARCHAR2) is
    l_count number;
    --l_doc_type varchar2(100);
  begin
  
    /* l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_zr_header.trx_order_number,
    p_doc_type   => p_zr_header.trx_order_type);*/
  
    select count(1)
      into l_count
      from cux_inv_material_lines cml, cux_inv_material_headers cmh
     where cml.header_id = cmh.header_id
       and cmh.req_number = p_zr_line.Order_Number
       and cml.line_number = p_zr_line.Line_Number
          --and cmh.inv_type = l_doc_type --4
       and cmh.organization_id = p_zr_header.organization_id
       and cml.status = '1';
  
    if (l_count > 0) then
    
      SELECT cmh.header_id,
             cml.line_id,
             cml.inventory_item_id,
             cml.organization_id,
             cml.subinventory_code,
             cml.locator_id,
             cml.primary_quantity,
             0
        INTO p_zr_line.Header_Id,
             p_zr_line.Line_Id,
             p_zr_line.Item_Id,
             p_zr_line.to_Organization_Id,
             p_zr_line.to_Subinventory,
             p_zr_line.to_Locator_Id,
             p_zr_line.Orig_Quantity,
             p_zr_line.Fulfilled_Quantity
        from cux_inv_material_headers cmh, cux_inv_material_lines cml
       where cmh.header_id = cml.header_id
         and cmh.req_number = p_zr_line.Order_Number
         and cml.line_number = p_zr_line.Line_Number
         and cmh.organization_id = p_zr_header.organization_id; --4;
    
    else
    
      /*x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据已杂入';
      RAISE Fnd_Api.g_Exc_Error;*/
    
      x_Ret_Sts   := fnd_api.G_RET_STS_SUCCESS;
      x_Error_Msg := '杂入单据已过账';
      --RAISE Fnd_Api.g_Exc_Error;
    
    end if;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE get_zf_line_info(p_zf_header Cux_Wms_Material_Trx_Headers%rowtype,
                             p_zf_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                             x_Ret_Sts   OUT VARCHAR2,
                             x_Error_Msg OUT VARCHAR2) is
  
    l_count number;
    --l_doc_type varchar2(100);
  begin
  
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_zf_header.trx_order_number,
    p_doc_type   => p_zf_header.trx_order_type);*/
  
    select count(1)
      into l_count
      from cux_inv_material_lines cml, cux_inv_material_headers cmh
     where cml.header_id = cmh.header_id
       and cmh.req_number = p_zf_line.Order_Number
       and cml.line_number = p_zf_line.Line_Number
       and cmh.organization_id = p_zf_header.organization_id
          --and cmh.inv_type = l_doc_type --3
       and cml.status = '1';
  
    if (l_count > 0) then
    
      SELECT cmh.header_id,
             cml.line_id,
             cml.inventory_item_id,
             cml.organization_id,
             cml.subinventory_code,
             cml.locator_id,
             cml.primary_quantity,
             0
        INTO p_zf_line.Header_Id,
             p_zf_line.Line_Id,
             p_zf_line.Item_Id,
             p_zf_line.From_Organization_Id,
             p_zf_line.From_Subinventory,
             p_zf_line.From_Locator_Id,
             p_zf_line.Orig_Quantity,
             p_zf_line.Fulfilled_Quantity
        from cux_inv_material_headers cmh, cux_inv_material_lines cml
       where cmh.header_id = cml.header_id
         and cmh.req_number = p_zf_line.Order_Number
         and cml.line_number = p_zf_line.Line_Number
         and cmh.organization_id = p_zf_header.organization_id
      /*and cmh.inv_type = l_doc_type*/
      ;
    
    else
    
      /*x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据已杂发';
      RAISE Fnd_Api.g_Exc_Error;*/
      x_Ret_Sts   := fnd_api.G_RET_STS_SUCCESS;
      x_Error_Msg := '杂发单据已过账！';
      --RAISE Fnd_Api.g_Exc_Error;
    end if;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE get_db_line_info(p_db_header Cux_Wms_Material_Trx_Headers%rowtype,
                             p_db_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                             x_Ret_Sts   OUT VARCHAR2,
                             x_Error_Msg OUT VARCHAR2) is
  
    l_count number;
    --l_doc_type varchar2(100);
  begin
  
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_db_header.trx_order_number,
    p_doc_type   => p_db_header.trx_order_type);*/
  
    select count(1)
      into l_count
      from cux_inv_material_lines cml, cux_inv_material_headers cmh
     where cml.header_id = cmh.header_id
       and cmh.req_number = p_db_line.Order_Number
       and cml.line_number = p_db_line.Line_Number
       and cmh.organization_id = p_db_header.organization_id
          --and cmh.inv_type = l_doc_type --5
       and cml.status = '1';
  
    if (l_count > 0) then
    
      SELECT cmh.header_id,
             cml.line_id,
             cml.inventory_item_id,
             cml.organization_id,
             cml.subinventory_code,
             cml.locator_id,
             cml.primary_quantity,
             cml.to_organization_id,
             cml.to_subinventory_code,
             cml.to_locator_id,
             0
        INTO p_db_line.Header_Id,
             p_db_line.Line_Id,
             p_db_line.Item_Id,
             p_db_line.From_Organization_Id,
             p_db_line.From_Subinventory,
             p_db_line.From_Locator_Id,
             p_db_line.Orig_Quantity,
             p_db_line.To_Organization_Id,
             p_db_line.To_Subinventory,
             p_db_line.To_Locator_Id,
             p_db_line.Fulfilled_Quantity
        from cux_inv_material_headers cmh, cux_inv_material_lines cml
       where cmh.header_id = cml.header_id
         and cmh.req_number = p_db_line.Order_Number
         and cml.line_number = p_db_line.Line_Number
         and cmh.organization_id = p_db_header.organization_id;
    
    else
    
      /*x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据已调拨';
      RAISE Fnd_Api.g_Exc_Error;*/
      x_Ret_Sts   := fnd_api.G_RET_STS_SUCCESS;
      x_Error_Msg := '调拨单据已过账';
      --RAISE Fnd_Api.g_Exc_Error;
    
    end if;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;

  PROCEDURE get_yddb_line_info(p_yddb_header Cux_Wms_Material_Trx_Headers%rowtype,
                               p_yddb_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                               x_Ret_Sts     OUT VARCHAR2,
                               x_Error_Msg   OUT VARCHAR2) is
    l_count number;
    --l_doc_type varchar2(100);
  begin
  
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_yddb_header.trx_order_number,
    p_doc_type   => p_yddb_header.trx_order_type);*/
  
    select count(1)
      into l_count
      from cux_inv_material_lines cml, cux_inv_material_headers cmh
     where cml.header_id = cmh.header_id
       and cmh.req_number = p_yddb_line.Order_Number
       and cml.line_number = p_yddb_line.Line_Number
          --and cmh.inv_type = l_doc_type --6
       and cmh.organization_id = p_yddb_header.organization_id
       and cml.status = '1';
  
    if (l_count > 0) then
    
      SELECT cmh.header_id,
             cml.line_id,
             cml.inventory_item_id,
             cml.organization_id,
             cml.subinventory_code,
             cml.locator_id,
             cml.primary_quantity,
             cml.to_organization_id,
             cml.to_subinventory_code,
             cml.to_locator_id,
             0
        INTO p_yddb_line.Header_Id,
             p_yddb_line.Line_Id,
             p_yddb_line.Item_Id,
             p_yddb_line.From_Organization_Id,
             p_yddb_line.From_Subinventory,
             p_yddb_line.From_Locator_Id,
             p_yddb_line.Orig_Quantity,
             p_yddb_line.To_Organization_Id,
             p_yddb_line.To_Subinventory,
             p_yddb_line.To_Locator_Id,
             p_yddb_line.Fulfilled_Quantity
        from cux_inv_material_headers cmh, cux_inv_material_lines cml
       where cmh.header_id = cml.header_id
         and cmh.req_number = p_yddb_line.Order_Number
         and cml.line_number = p_yddb_line.Line_Number
         and cmh.organization_id = p_yddb_header.organization_id;
    
    else
    
      /*x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据已调拨';
      RAISE Fnd_Api.g_Exc_Error;*/
      x_Ret_Sts   := fnd_api.G_RET_STS_SUCCESS;
      x_Error_Msg := '异地调拨单据已过账';
      --RAISE Fnd_Api.g_Exc_Error;
    end if;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;

  PROCEDURE get_xodb_line_info(p_xodb_header Cux_Wms_Material_Trx_Headers%rowtype,
                               p_xodb_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                               x_Ret_Sts     OUT VARCHAR2,
                               x_Error_Msg   OUT VARCHAR2) is
    l_count number;
    --l_doc_type varchar2(100);
  begin
  
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_xodb_header.trx_order_number,
    p_doc_type   => p_xodb_header.trx_order_type);*/
  
    select count(1)
      into l_count
      from cux_inv_material_lines cml, cux_inv_material_headers cmh
     where cml.header_id = cmh.header_id
       and cmh.req_number = p_xodb_line.Order_Number
       and cml.line_number = p_xodb_line.Line_Number
       and cmh.organization_id = p_xodb_header.organization_id
       and cml.status = '1';
  
    if (l_count > 0) then
    
      SELECT cmh.header_id,
             cml.line_id,
             cml.inventory_item_id,
             cml.organization_id,
             cml.subinventory_code,
             cml.locator_id,
             cml.primary_quantity,
             cml.to_organization_id,
             cml.to_subinventory_code,
             cml.to_locator_id,
             0
        INTO p_xodb_line.Header_Id,
             p_xodb_line.Line_Id,
             p_xodb_line.Item_Id,
             p_xodb_line.From_Organization_Id,
             p_xodb_line.From_Subinventory,
             p_xodb_line.From_Locator_Id,
             p_xodb_line.Orig_Quantity,
             p_xodb_line.To_Organization_Id,
             p_xodb_line.To_Subinventory,
             p_xodb_line.To_Locator_Id,
             p_xodb_line.Fulfilled_Quantity
        from cux_inv_material_headers cmh, cux_inv_material_lines cml
       where cmh.header_id = cml.header_id
         and cmh.req_number = p_xodb_line.Order_Number
         and cml.line_number = p_xodb_line.Line_Number
         and cmh.organization_id = p_xodb_header.organization_id;
    
    else
    
      /*x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据已调拨';
      RAISE Fnd_Api.g_Exc_Error;*/
      x_Ret_Sts   := fnd_api.G_RET_STS_SUCCESS;
      x_Error_Msg := '跨组织调拨单据已过账';
      --RAISE Fnd_Api.g_Exc_Error;
    end if;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;

  PROCEDURE get_prepare_line_info(p_prepare_header Cux_Wms_Material_Trx_Headers%rowtype,
                                  p_prepare_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                                  x_Ret_Sts        OUT VARCHAR2,
                                  x_Error_Msg      OUT VARCHAR2) is
    l_count number;
    --l_doc_type varchar2(100);
  begin
  
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_prepare_header.trx_order_number,
    p_doc_type   => p_prepare_header.trx_order_type);*/
  
    select count(1)
      into l_count
      from cux_ready_item cri
     where cri.doc_no = p_prepare_line.Order_Number
       and cri.line_number = p_prepare_line.Line_Number
       and cri.organization_id = p_prepare_header.organization_id
          -- and cri.doc_type = l_doc_type
          --and cri.doc_status = '未过账'
          --and cri.transfer_status = 'TRANSFERRED';
       and cri.transfer_status in ('TRANSFERRED', 'POSTED');
    if (l_count > 0) then
    
      SELECT '',
             cri.line_id,
             cri.inventory_item_id,
             cri.organization_id,
             cri.supply_subinventory,
             cri.supply_locator_id,
             cri.Ready_Qty,
             cri.organization_id,
             cri.to_sub,
             cri.to_locator_id,
             0
        INTO p_prepare_line.Header_Id,
             p_prepare_line.Line_Id,
             p_prepare_line.Item_Id,
             p_prepare_line.From_Organization_Id,
             p_prepare_line.From_Subinventory,
             p_prepare_line.From_Locator_Id,
             p_prepare_line.Orig_Quantity,
             p_prepare_line.To_Organization_Id,
             p_prepare_line.To_Subinventory,
             p_prepare_line.To_Locator_Id,
             p_prepare_line.Fulfilled_Quantity
        FROM cux_ready_item cri
       where cri.doc_no = p_prepare_line.Order_Number
         and cri.line_number = p_prepare_line.Line_Number
            --and cri.doc_type = l_doc_type
         and cri.organization_id = p_prepare_header.organization_id; --in ('拉式备料', '推式备料');
    else
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据未抛转';
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE get_replenish_line_info(p_replenish_header Cux_Wms_Material_Trx_Headers%rowtype,
                                    p_replenish_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                                    x_Ret_Sts          OUT VARCHAR2,
                                    x_Error_Msg        OUT VARCHAR2) is
    l_count number;
    --l_doc_type varchar2(100);
  begin
  
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_replenish_header.trx_order_number,
    p_doc_type   => p_replenish_header.trx_order_type);*/
  
    select count(1)
      into l_count
      from cux_ready_item cri
     where cri.doc_no = p_replenish_line.Order_Number
       and cri.line_number = p_replenish_line.Line_Number
       and cri.organization_id = p_replenish_header.organization_id
          --and cri.doc_type = l_doc_type --in ('推式补料', '拉式补料')
          --and cri.doc_status = '未过账'
          --and cri.transfer_status = 'TRANSFERRED';
       and cri.transfer_status in ('TRANSFERRED', 'POSTED');
  
    if (l_count > 0) then
    
      SELECT '',
             cri.line_id,
             cri.inventory_item_id,
             cri.organization_id,
             cri.supply_subinventory,
             cri.supply_locator_id,
             cri.Ready_Qty,
             cri.organization_id,
             cri.to_sub,
             cri.to_locator_id,
             0
        INTO p_replenish_line.Header_Id,
             p_replenish_line.Line_Id,
             p_replenish_line.Item_Id,
             p_replenish_line.From_Organization_Id,
             p_replenish_line.From_Subinventory,
             p_replenish_line.From_Locator_Id,
             p_replenish_line.Orig_Quantity,
             p_replenish_line.To_Organization_Id,
             p_replenish_line.To_Subinventory,
             p_replenish_line.To_Locator_Id,
             p_replenish_line.Fulfilled_Quantity
        FROM cux_ready_item cri
       where cri.doc_no = p_replenish_line.Order_Number
         and cri.line_number = p_replenish_line.Line_Number
         and cri.organization_id = p_replenish_header.organization_id
      --and cri.doc_type = l_doc_type
      ; --in ('推式补料', '拉式补料');
    else
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据未抛转';
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE get_over_line_info(p_over_header Cux_Wms_Material_Trx_Headers%rowtype,
                               p_over_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                               x_Ret_Sts     OUT VARCHAR2,
                               x_Error_Msg   OUT VARCHAR2) is
    l_count number;
    --l_doc_type varchar2(100);
  begin
  
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_over_header.trx_order_number,
    p_doc_type   => p_over_header.trx_order_type);*/
  
    select count(1)
      into l_count
      from cux_ready_item cri
     where cri.doc_no = p_over_line.Order_Number
       and cri.line_number = p_over_line.Line_Number
       and cri.organization_id = p_over_header.organization_id
          --and cri.doc_type = l_doc_type --in ('推式超领', '推式超领')
          --and cri.doc_status = '未过账'
          --and cri.transfer_status = 'TRANSFERRED';
       and cri.transfer_status in ('TRANSFERRED', 'POSTED');
  
    if (l_count > 0) then
    
      SELECT '',
             cri.line_id,
             cri.inventory_item_id,
             cri.organization_id,
             cri.supply_subinventory,
             cri.supply_locator_id,
             cri.Ready_Qty,
             cri.organization_id,
             cri.to_sub,
             cri.to_locator_id,
             0
        INTO p_over_line.Header_Id,
             p_over_line.Line_Id,
             p_over_line.Item_Id,
             p_over_line.From_Organization_Id,
             p_over_line.From_Subinventory,
             p_over_line.From_Locator_Id,
             p_over_line.Orig_Quantity,
             p_over_line.To_Organization_Id,
             p_over_line.To_Subinventory,
             p_over_line.To_Locator_Id,
             p_over_line.Fulfilled_Quantity
        FROM cux_ready_item cri
       where cri.doc_no = p_over_line.Order_Number
         and cri.line_number = p_over_line.Line_Number
         and cri.organization_id = p_over_header.organization_id
      --and cri.doc_type = l_doc_type
      ; --in ('推式超领', '推式超领');
    else
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据未抛转';
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE get_return_line_info(p_return_header Cux_Wms_Material_Trx_Headers%rowtype,
                                 p_return_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                                 x_Ret_Sts       OUT VARCHAR2,
                                 x_Error_Msg     OUT VARCHAR2) is
    l_count_push number := 0;
    l_count_pull number := 0;
    --l_judge_type number := 0;
    --l_doc_type   varchar2(100);
  begin
  
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_return_header.trx_order_number,
    p_doc_type   => p_return_header.trx_order_type);*/
  
    /* select count(1)
     into l_judge_type
     from cux_ready_item cri
    where cri.doc_no = p_return_line.Order_Number;*/
  
    if (p_return_header.trx_order_type in ('WIP_NEG_PS', 'WIP_FRG_PS')) then
      --推式退料
      select count(1)
        into l_count_push
        from cux_ready_item cri
       where cri.doc_no = p_return_line.Order_Number
         and cri.line_number = p_return_line.Line_Number
         and cri.organization_id = p_return_header.organization_id
            --and cri.doc_type = l_doc_type --in ('推式退料(负单)', '推式退料(零单)')
            --and cri.doc_status = '未过账'
            --and cri.transfer_status = 'TRANSFERRED';
         and cri.transfer_status in ('TRANSFERRED', 'POSTED');
    elsif (p_return_header.trx_order_type in ('WIP_FUL_PL', 'WIP_FRG_PL')) then
      --拉式退料
    
      select count(1)
        into l_count_pull
        from CUX_WIP_MATERIAL_RETURN cmr
       where cmr.transaction_id = p_return_line.Order_Number
         and cmr.line_number = p_return_line.Line_Number
            --and cmr.source_type in (1,2) 1 是零星退料，2是整单退
            --and cmr.status = '1'
         AND cmr.select_flag = 'Y'
            --and cri.transfer_status = 'TRANSFERRED';
         and cmr.transfer_status in ('TRANSFERRED', 'POSTED');
    
    end if;
  
    if (l_count_push > 0 and
       p_return_header.trx_order_type in ('WIP_NEG_PS', 'WIP_FRG_PS')) then
    
      SELECT '',
             cri.line_id,
             cri.inventory_item_id,
             cri.organization_id,
             cri.supply_subinventory,
             cri.supply_locator_id,
             cri.Ready_Qty,
             cri.organization_id,
             cri.to_sub,
             cri.to_locator_id,
             0
        INTO p_return_line.Header_Id,
             p_return_line.Line_Id,
             p_return_line.Item_Id,
             p_return_line.From_Organization_Id,
             p_return_line.From_Subinventory,
             p_return_line.From_Locator_Id,
             p_return_line.Orig_Quantity,
             p_return_line.To_Organization_Id,
             p_return_line.To_Subinventory,
             p_return_line.To_Locator_Id,
             p_return_line.Fulfilled_Quantity
        FROM cux_ready_item cri
       where cri.doc_no = p_return_line.Order_Number
         and cri.line_number = p_return_line.Line_Number
         and cri.organization_id = p_return_header.organization_id
      --and cri.doc_type = l_doc_type
      ; --in ('推式退料(负单)', '推式退料(零单)');
    elsif (l_count_pull > 0 and
          p_return_header.trx_order_type in ('WIP_FUL_PL', 'WIP_FRG_PL')) then
      SELECT '',
             cmr.line_id,
             cmr.inventory_item_id,
             cmr.organization_id,
             cmr.supply_subinventory,
             cmr.supply_locator_id,
             cmr.return_quantity,
             cmr.organization_id,
             cmr.subinventory,
             cmr.locator_id,
             0
        INTO p_return_line.Header_Id,
             p_return_line.Line_Id,
             p_return_line.Item_Id,
             p_return_line.From_Organization_Id,
             p_return_line.From_Subinventory,
             p_return_line.From_Locator_Id,
             p_return_line.Orig_Quantity,
             p_return_line.To_Organization_Id,
             p_return_line.To_Subinventory,
             p_return_line.To_Locator_Id,
             p_return_line.Fulfilled_Quantity
        FROM cux_wip_material_return cmr
       where cmr.transaction_id = p_return_line.Order_Number
         and cmr.line_number = p_return_line.Line_Number
         AND CMR.SELECT_FLAG = 'Y'
      --and cmr.source_type in (1,2) 1 是零星退料，2是整单退
      ;
    else
    
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据未抛转';
      RAISE Fnd_Api.g_Exc_Error;
    
    end if;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误1：' || sqlerrm;
  end;
  PROCEDURE get_complete_line_info(p_complete_header Cux_Wms_Material_Trx_Headers%rowtype,
                                   p_complete_line   IN OUT Cux_Wms_Pub.Trx_Line_Rec,
                                   x_Ret_Sts         OUT VARCHAR2,
                                   x_Error_Msg       OUT VARCHAR2) is
    l_count number;
    --l_doc_type varchar2(100);
  begin
  
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_complete_header.trx_order_number,
    p_doc_type   => p_complete_header.trx_order_type);*/
  
    select count(1)
      into l_count
      from cux_ready_item cri
     where cri.doc_no = p_complete_line.Order_Number
       and cri.line_number = p_complete_line.Line_Number
       and cri.organization_id = p_complete_header.organization_id
          --and cri.doc_type = l_doc_type --'完工入库'
       and cri.doc_status = '未过账';
  
    if (l_count > 0) then
    
      SELECT '',
             cri.line_id,
             cri.inventory_item_id,
             cri.organization_id,
             cri.supply_subinventory,
             cri.supply_locator_id,
             cri.Ready_Qty,
             cri.organization_id,
             cri.to_sub,
             cri.to_locator_id,
             0
        INTO p_complete_line.Header_Id,
             p_complete_line.Line_Id,
             p_complete_line.Item_Id,
             p_complete_line.From_Organization_Id,
             p_complete_line.From_Subinventory,
             p_complete_line.From_Locator_Id,
             p_complete_line.Orig_Quantity,
             p_complete_line.To_Organization_Id,
             p_complete_line.To_Subinventory,
             p_complete_line.To_Locator_Id,
             p_complete_line.Fulfilled_Quantity
        FROM cux_ready_item cri
       where cri.doc_no = p_complete_line.Order_Number
         and cri.line_number = p_complete_line.Line_Number
         and cri.organization_id = p_complete_header.organization_id
      --and cri.doc_type = l_doc_type
      ; --'完工入库';
    else
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据已过账';
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;

  PROCEDURE split_rcv_line(p_delivery_header Cux_Wms_Material_Trx_Headers%rowtype,
                           p_delivery_lines  IN Cux_Wms_Pub.Material_Trx_Lines_Tab,
                           x_delivery_lines  OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                           x_Ret_Sts         OUT VARCHAR2,
                           x_Error_Msg       OUT VARCHAR2) IS
    l_count_line        number;
    l_remain_qty        number := 0;
    l_dly_qty           number := 0;
    l_index             number := 0;
    l_comb_tag          varchar2(10);
    l_comb_to_line_id   number;
    l_count             number := 0;
    l_receiving_rout_id number;
    l_accepted_qty      number;
    cursor cur_comb_lines(p_comb_line_id number) is
      select *
        from cux_receipts_lines crl
       where crl.combine_to_line_id = p_comb_line_id
         and crl.combine_tag = 'C'
       order by crl.line_number;
  BEGIN
    x_Ret_Sts    := Fnd_Api.g_Ret_Sts_Success;
    l_index      := 1;
    l_count_line := p_delivery_lines.count;
    for i in 1 .. l_count_line loop
      x_Error_Msg := '1';
      --get comb tag 
      begin
        select crl.combine_tag, crl.receipt_line_id
          into l_comb_tag, l_comb_to_line_id
          from cux_receipts_headers crh, cux_receipts_lines crl
         where crh.receipt_id = crl.receipt_id
           and crh.receipt_number = p_delivery_lines(i).TRX_ORDER_NUMBER
           and crl.line_number = p_delivery_lines(i).line_number;
      exception
        when others then
          x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
          x_Error_Msg := '获取合并标签出错：' || sqlerrm;
          raise Fnd_Api.g_Exc_Error;
      end;
      x_Error_Msg := '2';
      if (l_comb_tag = 'V') then
        --if this is comb line, then split into detail lines
        l_remain_qty := p_delivery_lines(i).QUANTITY;
        for rec_comb_lines in cur_comb_lines(l_comb_to_line_id) loop
          x_Error_Msg := '3';
          /*l_dly_qty   := rec_comb_lines.RECEIPT_QTY -
          nvl(rec_comb_lines.delivered_qty, 0) -
          nvl(rec_comb_lines.returned_qty_from_rcv, 0);*/
          begin
            select plla.receiving_routing_id
              into l_receiving_rout_id
              from po_line_locations_all plla
             where plla.line_location_id = rec_comb_lines.po_shipment_id;
          exception
            when others then
              x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
              x_Error_Msg := '获取接收路劲出错：' || sqlerrm;
              raise Fnd_Api.g_Exc_Error;
          end;
        
          if (l_receiving_rout_id = 2) then
            --need check
            begin
              select rt.quantity
                into l_accepted_qty
                from rcv_transactions     rt,
                     cux_receipts_lines   crl,
                     cux_receipts_headers crh
               where crl.receipt_id = crh.receipt_id
                 and crl.line_number = rec_comb_lines.line_number
                 and crh.receipt_number =
                     p_delivery_header.trx_order_number
                 and crl.shipment_line_id = rt.shipment_line_id
                 and crh.sys_receipt_id = rt.shipment_header_id
                 and rt.transaction_type = 'ACCEPT';
            
            exception
              when others then
                /*x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                x_Error_Msg := p_delivery_header.trx_order_number || '行' ||
                               rec_comb_lines.line_number || '获取接受数量出错：' ||
                               SQLERRM;
                RAISE Fnd_Api.g_Exc_Error;*/
                l_accepted_qty := 0;
            end;
          
          else
            --no need check
          
            l_accepted_qty := 0; --in this case,this qty won't be used.
          
          end if;
        
          l_dly_qty := cux_ebs_qis_integration_pkg.get_qty_to_deliver(p_rcv_line_id       => rec_comb_lines.receipt_line_id,
                                                                      p_line_location_id  => rec_comb_lines.po_shipment_id,
                                                                      p_trx_qty           => l_accepted_qty,
                                                                      p_shipment_line_id  => rec_comb_lines.shipment_line_id,
                                                                      p_receving_route_id => l_receiving_rout_id);
        
          if (l_remain_qty <= l_dly_qty) then
            x_Error_Msg := '4';
            x_delivery_lines(l_index).line_number := rec_comb_lines.line_number;
            x_delivery_lines(l_index).quantity := l_remain_qty;
            l_index := l_index + 1;
            l_count := l_count + 1;
            --assign this rec to x dly,but qty should be l_remain_qty
            exit;
          elsif (l_remain_qty > l_dly_qty and l_dly_qty > 0) then
            x_Error_Msg := '5';
            x_delivery_lines(l_index).line_number := rec_comb_lines.line_number;
            x_delivery_lines(l_index).quantity := l_dly_qty;
            --x_delivery_lines(i) := rec_comb_lines(i); --assign the same line to delivery line, the same qty
            l_remain_qty := l_remain_qty - l_dly_qty;
            l_index      := l_index + 1;
            l_count      := l_count + 1;
            x_Error_Msg  := '6';
          else
            null;
          end if;
        
        end loop;
      else
        x_Error_Msg := '7';
        x_delivery_lines(l_index).line_number := p_delivery_lines(i)
                                                 .line_number;
        x_delivery_lines(l_index).quantity := p_delivery_lines(i).quantity;
        l_index := l_index + 1;
        l_count := l_count + 1;
        x_Error_Msg := '8';
      end if;
    
    end loop;
    if (l_count = 0) then
    
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '没有符合入库要求的记录！';
      raise Fnd_Api.g_Exc_Error;
    
    end if;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || x_Error_Msg || sqlerrm;
  end;
  PROCEDURE split_rtn_line(p_return_header Cux_Wms_Material_Trx_Headers%rowtype,
                           p_return_lines  IN Cux_Wms_Pub.Material_Trx_Lines_Tab,
                           x_return_lines  OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                           x_Ret_Sts       OUT VARCHAR2,
                           x_Error_Msg     OUT VARCHAR2) IS
    l_count_line number;
    l_remain_qty number := 0;
    l_index      number := 0;
    --l_dly_qty    number := 0;
    l_comb_tag        varchar2(10);
    l_comb_to_line_id number;
    cursor cur_comb_lines(p_comb_line_id number) is
      select *
        from cux_return_lines crl
       where crl.combine_to_line_id = p_comb_line_id
         and crl.combine_tag = 'C'
       order by crl.line_number;
  BEGIN
  
    x_Ret_Sts    := Fnd_Api.g_Ret_Sts_Success;
    l_index      := 1;
    l_count_line := p_return_lines.count;
    for i in 1 .. l_count_line loop
      x_Error_Msg := '1';
      --get comb tag 
      begin
        select crl.combine_tag, crl.return_line_id
          into l_comb_tag, l_comb_to_line_id
          from cux_return_headers crh, cux_return_lines crl
         where crh.return_id = crl.return_id
           and crh.return_number = p_return_lines(i).TRX_ORDER_NUMBER
           and crl.line_number = p_return_lines(i).line_number;
      exception
        when others then
          x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
          x_Error_Msg := '获取合并标签出错：' || sqlerrm;
          raise Fnd_Api.g_Exc_Error;
      end;
      x_Error_Msg := '2';
      if (l_comb_tag = 'V') then
        x_Error_Msg := '3';
        --if this is comb line, then split into detail lines
        l_remain_qty := p_return_lines(i).QUANTITY;
        for rec_comb_lines in cur_comb_lines(l_comb_to_line_id) loop
          x_Error_Msg := '4';
          if (l_remain_qty <= rec_comb_lines.return_qty) then
            x_Error_Msg := '5';
            x_return_lines(l_index).line_number := rec_comb_lines.line_number;
            x_return_lines(l_index).SUBINVENTORY := rec_comb_lines.return_subinv;
            --x_return_lines(i).locator_name := rec_comb_lines.return_subinv;
            x_return_lines(l_index).quantity := l_remain_qty;
            l_index := l_index + 1;
            --assign this rec to x dly,but qty should be l_remain_qty
            exit;
          else
            x_Error_Msg := '6';
            x_return_lines(l_index).line_number := rec_comb_lines.line_number;
            x_return_lines(l_index).SUBINVENTORY := rec_comb_lines.return_subinv;
            x_return_lines(l_index).quantity := rec_comb_lines.return_qty;
            l_index := l_index + 1;
            --x_delivery_lines(i) := rec_comb_lines(i); --assign the same line to delivery line, the same qty
            l_remain_qty := l_remain_qty - rec_comb_lines.return_qty;
          end if;
        
        end loop;
      else
        x_Error_Msg := '9';
        x_return_lines(l_index).line_number := p_return_lines(i).line_number;
        x_return_lines(l_index).SUBINVENTORY := p_return_lines(i)
                                                .SUBINVENTORY;
        x_return_lines(l_index).quantity := p_return_lines(i).quantity;
        l_index := l_index + 1;
      end if;
    
    end loop;
    null;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || x_Error_Msg || sqlerrm;
  end;
  PROCEDURE create_delivery(p_delivery_header Cux_Wms_Material_Trx_Headers%rowtype,
                            p_delivery_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                            x_Ret_Sts         OUT VARCHAR2,
                            x_Error_Msg       OUT VARCHAR2) is
    l_delivery_lines Cux_Wms_Pub.Material_Trx_Lines_Tab;
    l_User_Resp      Cux_Soa_Pub.Soa_User_Responsibility;
    --l_errbuf              varchar2(1000);
    --l_retcode             number;
    l_parent_req_id       number;
    l_count_line          number := 0;
    l_cux_delivery_id     number;
    l_batch_number        varchar2(30) := to_char(sysdate,
                                                  'YYYYMMDDHH24MISS');
    l_delivery_seq        varchar2(10);
    l_delivery_number     varchar2(30);
    l_organization_id     number;
    l_organization_code   varchar2(10);
    l_sys_receipt_id      number;
    l_cux_receipt_id      number;
    l_vendor_id           number;
    l_vendor_site_id      number;
    l_user_id             number := fnd_global.USER_ID;
    l_login_id            number := fnd_global.LOGIN_ID;
    l_line_number         number := 0;
    l_po_header_id        number;
    l_po_line_id          number;
    l_line_location_id    number;
    l_po_distribution_id  number;
    l_release_number      number;
    l_release_id          number;
    l_receipt_line_id     number;
    l_item_id             number;
    l_delivery_qty        number;
    l_delivery_subinv     varchar2(10);
    l_delivery_locator_id number;
    l_uom                 varchar2(30);
    l_ship_to_location_id number;
    l_parent_trx_id       number;
    l_emp_id              number;
    l_comments            varchar2(2000);
    l_rework_flag         varchar2(10);
    l_header_interface_id number := ''; --populated in qis integration pkg
    l_interface_line_id   number := ''; --populated in qis integration pkg
    l_group_id            number := ''; --populated in qis integration pkg
  
    l_dly_qty              number;
    l_receiving_routing_id number;
    l_shipment_line_id     number;
    l_accepted_qty         number;
    l_last_index           number := 0;
    l_org_id               number;
    --l_count                number;
    l_count_index       number;
    l_index_item_id     number;
    l_index_item_number varchar2(100);
    l_default_subinv    varchar2(10);
    l_default_loc_id    number;
    l_locator_control   number;
  begin
  
    --验证默认收货货位
    l_count_index := p_delivery_lines.count;
    for x in 1 .. l_count_index loop
    
      begin
        select crl.item_id, crl.default_subinv
          into l_index_item_id, l_default_subinv
          from cux_receipts_lines crl, cux_receipts_headers crh
         where crl.line_number = p_delivery_lines(x).line_number
           and crh.receipt_number = p_delivery_header.trx_order_number
           and crl.receipt_id = crh.receipt_id;
      exception
        when others then
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '单据' || p_delivery_header.trx_order_number || '行' || p_delivery_lines(x)
                        .line_number || '获取默认子库出错：' || SQLERRM;
          RAISE Fnd_Api.g_Exc_Error;
      end;
    
      begin
        select msi.locator_type
          into l_locator_control
          from mtl_secondary_inventories msi
         where msi.secondary_inventory_name = l_default_subinv
           and msi.organization_id = p_delivery_header.organization_id;
      exception
        when others then
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '子库' || l_default_subinv || '获取货位控制出错：' || SQLERRM;
          RAISE Fnd_Api.g_Exc_Error;
      end;
    
      if (l_locator_control <> 1) then
        l_default_loc_id := cux_ebs_qis_integration_pkg.get_default_locator(p_item_Id         => l_index_item_id,
                                                                            p_organization_id => p_delivery_header.organization_id);
      
        if (l_default_loc_id > 0) then
          null;
        else
        
          begin
            select msi.segment1
              into l_index_item_number
              from mtl_system_items_b msi
             where msi.inventory_item_id = l_index_item_id
               and msi.organization_id = p_delivery_header.organization_id;
          exception
            when others then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := '获取料号出错X：' || SQLERRM;
              RAISE Fnd_Api.g_Exc_Error;
          end;
        
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '料号' || l_index_item_number || '未设置货位!';
          RAISE Fnd_Api.g_Exc_Error;
        end if;
      end if;
    
    end loop;
    --END OF 验证货位
    --x_Ret_Sts := Fnd_Api.g_Ret_Sts_Success;
  
    split_rcv_line(p_delivery_header => p_delivery_header,
                   p_delivery_lines  => p_delivery_lines,
                   x_delivery_lines  => l_delivery_lines,
                   x_Ret_Sts         => x_Ret_Sts,
                   x_Error_Msg       => x_Error_Msg);
  
    if (x_Ret_Sts <> 'S') then
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '拆行出错' || x_Error_Msg;
      return;
    end if;
    --check if delivery line need check, if need, then fetch system accept and reject record to validate
    l_last_index := l_delivery_lines.count;
  
    for i in 1 .. l_last_index loop
    
      --loop lines to validate
      --validate delivery qty must less or equal accepted qty
      begin
        select plla.receiving_routing_id,
               crl.shipment_line_id,
               crl.po_shipment_id,
               crl.receipt_line_id
          into l_receiving_routing_id,
               l_shipment_line_id,
               l_line_location_id,
               l_receipt_line_id
          from po_line_locations_all plla,
               cux_receipts_lines    crl,
               cux_receipts_headers  crh
         where plla.line_location_id = crl.po_shipment_id
           and crl.line_number = l_delivery_lines(i).line_number
           and crh.receipt_number = p_delivery_header.trx_order_number
           and crh.receipt_id = crl.receipt_id;
      
      exception
        when others then
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '行' || l_delivery_lines(i).line_number ||
                         '获取发运行信息出错：' || SQLERRM;
          RAISE Fnd_Api.g_Exc_Error;
      end;
      if (l_receiving_routing_id = 2) then
        --need check
        begin
          select rt.quantity, rt.transaction_id
            into l_accepted_qty, l_parent_trx_id
            from rcv_transactions     rt,
                 cux_receipts_lines   crl,
                 cux_receipts_headers crh
           where crl.receipt_id = crh.receipt_id
             and crl.line_number = l_delivery_lines(i).line_number
             and crh.receipt_number = p_delivery_header.trx_order_number
             and crl.shipment_line_id = rt.shipment_line_id
             and crh.sys_receipt_id = rt.shipment_header_id
             and rt.transaction_type = 'ACCEPT';
        
        exception
          when others then
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := p_delivery_header.trx_order_number || '行' || l_delivery_lines(i)
                          .line_number || '获取接受数量出错：' || SQLERRM;
            RAISE Fnd_Api.g_Exc_Error;
        end;
      
      else
        --no need check
      
        begin
          select rt.transaction_id
            INTO l_parent_trx_id
            from rcv_transactions     rt,
                 cux_receipts_lines   crl,
                 cux_receipts_headers crh
           where crl.receipt_id = crh.receipt_id
             and crl.line_number = l_delivery_lines(i).line_number
             and crh.receipt_number = p_delivery_header.trx_order_number
             and crl.shipment_line_id = rt.shipment_line_id
             and crh.sys_receipt_id = rt.shipment_header_id
             and rt.transaction_type = 'RECEIVE';
        
        exception
          when others then
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '行' || l_delivery_lines(i).line_number ||
                           '父交易行出错：' || SQLERRM;
            RAISE Fnd_Api.g_Exc_Error;
        end;
      
        l_accepted_qty := 0; --in this case,this qty won't be used.
      
      end if;
    
      l_delivery_lines(i).parent_trx_id := l_parent_trx_id;
    
      l_dly_qty := cux_ebs_qis_integration_pkg.get_qty_to_deliver(p_rcv_line_id       => l_receipt_line_id,
                                                                  p_line_location_id  => l_line_location_id,
                                                                  p_trx_qty           => l_accepted_qty,
                                                                  p_shipment_line_id  => l_shipment_line_id,
                                                                  p_receving_route_id => l_receiving_routing_id);
    
      if (l_dly_qty < l_delivery_lines(i).quantity) then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '行' || l_delivery_lines(i).line_number || '可入库数量为' ||
                       l_dly_qty || ',入库数量' || l_delivery_lines(i).quantity ||
                       '必须小于等于可入库数量。' || l_receipt_line_id || '-' ||
                       l_line_location_id || '-' || l_accepted_qty || '-' ||
                       l_shipment_line_id;
        RAISE Fnd_Api.g_Exc_Error;
      end if;
    
    END LOOP; --END LOOP LINE
  
    --if pass validation
  
    --get inv org id
    begin
      select crh.organization_id,
             crh.sys_receipt_id,
             crh.receipt_id,
             crh.vendor_id,
             crh.vendor_site_id
        into l_organization_id,
             l_sys_receipt_id,
             l_cux_receipt_id,
             l_vendor_id,
             l_vendor_site_id
        from cux_receipts_headers crh
       where crh.receipt_number = p_delivery_header.trx_order_number;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取接收单头信息出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    --get ou id
    begin
      select iov.OPERATING_UNIT
        into l_org_id
        from inv_organization_info_v iov
       where iov.ORGANIZATION_ID = l_organization_id;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取OU出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    --get inv org code
    begin
      select mp.organization_code
        into l_organization_code
        from mtl_parameters mp
       where mp.organization_id = l_organization_id;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取组织代码出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
    --insert into delivery header
    select cux_delivery_headers_s.nextval into l_cux_delivery_id from dual;
  
    SELECT Lpad(To_Number(Nvl(MAX(Substr(cdh.delivery_number, -4)), 0) + 1),
                4,
                '0')
      INTO l_delivery_seq
      FROM cux_delivery_headers cdh
     WHERE cdh.organization_id = l_organization_id
       AND Substr(cdh.delivery_number, 4, 8) = To_Char(SYSDATE, 'YYYYMMDD');
  
    --set delivery number
    --select CUX_DELIVERY_NUMBER_S.Nextval into l_delivery_seq from dual;
    l_delivery_number := 'D' || l_organization_code ||
                         to_char(SYSDATE, 'YYYYMMDD') ||
                         lpad(l_delivery_seq, 4, '0');
  
    begin
      insert into cux_delivery_headers
        (delivery_id,
         delivery_number,
         --sys_trx_id,
         process_status,
         sys_receipt_id,
         cux_receipt_id,
         delivery_date,
         vendor_id,
         vendor_site_id,
         organization_id,
         delivery_status,
         print_times,
         --description,
         batch_number,
         created_by,
         creation_date,
         last_updated_by,
         last_update_date,
         last_update_login)
      values
        (l_cux_delivery_id,
         l_delivery_number,
         --
         'P',
         l_sys_receipt_id,
         l_cux_receipt_id,
         sysdate,
         l_vendor_id,
         l_vendor_site_id,
         l_organization_id,
         'ENTERED',
         0,
         l_batch_number, --to_char(sysdate,'YYYYMMDDHH24MISS'),
         l_user_id,
         sysdate,
         l_user_id,
         sysdate,
         l_login_id);
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '插入交货头信息出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
    --insert into delivery lines using bulk insert 
    --loop lines
    for j in 1 .. l_last_index loop
      l_line_number  := l_line_number + 1;
      l_delivery_qty := l_delivery_lines(j).quantity;
      --l_delivery_subinv     := l_delivery_lines(j).to_subinv;
      --l_delivery_locator_id := l_delivery_lines(j).locator_id;
      l_parent_trx_id := l_delivery_lines(j).parent_trx_id;
      l_emp_id        := 0;
      --l_comments      := l_delivery_lines(j).comments;
      --l_rework_flag         := l_delivery_lines(j).rework_flag;
      --get default subinv from rcv
      begin
        select crl.default_subinv, crl.default_locator_id
          into l_delivery_subinv, l_delivery_locator_id
          from cux_receipts_lines crl, cux_receipts_headers crh
         where crl.receipt_id = crh.receipt_id
           and crh.receipt_number = p_delivery_header.trx_order_number
           and crl.line_number = l_delivery_lines(j).line_number
           and crl.default_subinv is not null /*
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   and crl.default_subinv = l_delivery_lines(j).subinventory*/
        ;
      exception
        when others then
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '获取默认子库出错：' || SQLERRM;
          RAISE Fnd_Api.g_Exc_Error;
      end;
    
      begin
        select crl.po_header_id,
               crl.po_line_id,
               crl.po_shipment_id,
               1, --crl.
               crl.po_release_num,
               crl.po_release_id,
               crh.receipt_id,
               crl.receipt_line_id,
               crl.item_id,
               crl.receipt_uom,
               crl.ship_to_location_id
          into l_po_header_id,
               l_po_line_id,
               l_line_location_id,
               l_po_distribution_id,
               l_release_number,
               l_release_id,
               l_cux_receipt_id,
               l_receipt_line_id,
               --:CUX_CREATE_DELIVERY_BLK.SYS_RECEIPT_ID,
               l_item_id,
               --l_delivery_qty,
               l_uom,
               l_ship_to_location_id
          from cux_receipts_lines crl, cux_receipts_headers crh
         where crl.line_number = l_delivery_lines(j).line_number
           and crh.receipt_number = p_delivery_header.trx_order_number
           and crl.receipt_id = crh.receipt_id;
      exception
        when others then
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '行' || l_delivery_lines(j).line_number ||
                         '获取交货行信息出错：' || SQLERRM;
          RAISE Fnd_Api.g_Exc_Error;
      end;
      begin
        insert into cux_delivery_lines
          (delivery_line_id,
           line_number,
           delivery_id,
           po_header_id,
           po_line_id,
           po_shipment_id,
           --po_distribution_id,
           po_release_num,
           po_release_id,
           receipt_id,
           receipt_line_id,
           --sys_receipt_id,
           item_id,
           delivery_qty,
           delivery_subinv,
           delivery_locator_id,
           delivery_uom,
           ship_to_location_id,
           from_trx_id,
           applicant_person_id,
           comments,
           rework_flag,
           process_status,
           interface_header_id,
           interface_line_id,
           group_id,
           created_by,
           creation_date,
           last_updated_by,
           last_update_date,
           last_update_login)
        values
          (cux_delivery_lines_s.nextval,
           l_line_number,
           l_cux_delivery_id,
           l_po_header_id,
           l_po_line_id,
           l_line_location_id,
           --l_po_distribution_id,
           l_release_number,
           l_release_id,
           l_cux_receipt_id,
           l_receipt_line_id,
           --:CUX_CREATE_DELIVERY_BLK.SYS_RECEIPT_ID,
           l_item_id,
           l_delivery_qty,
           l_delivery_subinv,
           l_delivery_locator_id,
           l_uom,
           l_ship_to_location_id,
           l_parent_trx_id,
           l_emp_id,
           l_comments,
           l_rework_flag,
           'P',
           l_header_interface_id,
           l_interface_line_id,
           l_group_id,
           l_user_id,
           sysdate,
           l_user_id,
           sysdate,
           l_login_id);
      
      exception
        when others then
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '行' || l_delivery_lines(j).line_number ||
                         '插入交货行信息出错：' || SQLERRM;
          RAISE Fnd_Api.g_Exc_Error;
      end;
      l_count_line := l_count_line + 1;
    end loop;
  
    if (l_count_line > 0) then
    
      l_User_Resp.Org_Id    := l_org_id;
      l_User_Resp.Appl_Name := 'PO';
      Cux_Soa_Pub.Set_User_Responsibility(l_User_Resp);
      Fnd_Global.Apps_Initialize(User_Id           => l_User_Resp.User_Id,
                                 Resp_Id           => l_User_Resp.Resp_Id,
                                 Resp_Appl_Id      => l_User_Resp.Resp_Appl_Id,
                                 Security_Group_Id => 0,
                                 Server_Id         => NULL);
      Mo_Global.Set_Policy_Context('S', l_User_Resp.Org_Id);
    
      l_parent_req_id := fnd_request.submit_request('CUX',
                                                    'CUX_CREATE_DLY_DOC',
                                                    null,
                                                    sysdate,
                                                    false,
                                                    l_cux_delivery_id);
      if (l_parent_req_id > 0) then
        null;
      else
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '提交创建交货单请求出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
      end if;
    
    end if;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误create dly：' || sqlerrm;
  end;

  PROCEDURE process_return(p_return_header Cux_Wms_Material_Trx_Headers%rowtype,
                           p_return_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                           x_Ret_Sts       OUT VARCHAR2,
                           x_Error_Msg     OUT VARCHAR2) is
    l_return_lines    Cux_Wms_Pub.Material_Trx_Lines_Tab;
    l_req_id          number;
    l_organization_id NUMBER;
    l_cux_return_id   number;
    l_count           number;
    l_last_index      number := 0;
    l_org_id          number;
    l_User_Resp       Cux_Soa_Pub.Soa_User_Responsibility;
    l_count_post      number;
  begin
  
    begin
      SELECT COUNT(1)
        INTO l_count_post
        FROM cux_return_headers crh, cux.cux_return_lines crl
       where crh.return_id = crl.return_id
         and crh.return_number = p_return_header.trx_order_number
         and crl.posted_flag = 'Y';
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '检查是否过账出错！' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    IF (l_count_post > 0) THEN
      x_Ret_Sts   := fnd_api.G_RET_STS_SUCCESS;
      x_Error_Msg := '已过账';
      RETURN;
    END IF;
  
    split_rtn_line(p_return_header => p_return_header,
                   p_return_lines  => p_return_lines,
                   x_return_lines  => l_return_lines,
                   x_Ret_Sts       => x_Ret_Sts,
                   x_Error_Msg     => x_Error_Msg);
  
    if (x_Ret_Sts <> 'S') then
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '拆行出错' || x_Error_Msg;
      return;
    end if;
    /* x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
    x_Error_Msg := '拆行出错' || x_Error_Msg;
        return;*/
    --start validation
    l_last_index := l_return_lines.count;
  
    select count(1)
      into l_count
      from cux_return_headers crh, cux_return_lines crl
     where crh.return_number = p_return_header.trx_order_number
       and crh.return_id = crl.return_id
       and crl.combine_tag <> 'V';
  
    if (l_count <> l_last_index) then
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '退货行数' || l_last_index || '不等于原始单据行数！';
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  
    for i in 1 .. l_last_index loop
      --validate qty
      select count(1)
        into l_count
        from cux_return_headers crh,
             cux_return_lines   crl,
             cux_delivery_lines cdl
       where crh.return_id = crl.return_id
         and crl.delivery_line_id = cdl.delivery_line_id
         and crh.return_number = p_return_header.trx_order_number
         and crl.line_number = l_return_lines(i).line_number
         and crl.return_qty = l_return_lines(i).quantity;
    
      if (l_count = 0) then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '行' || l_return_lines(i).line_number || '退货数量' || l_return_lines(i)
                      .quantity || '不等于原始单据数量！';
        RAISE Fnd_Api.g_Exc_Error;
      end if;
    
      --validate delivery subinv and locator
      select count(1)
        into l_count
        from cux_return_headers crh,
             cux_return_lines   crl,
             cux_delivery_lines cdl
       where crh.return_id = crl.return_id
         and crl.delivery_line_id = cdl.delivery_line_id
         and crh.return_number = p_return_header.trx_order_number
         and crl.line_number = l_return_lines(i).line_number
         and crl.return_subinv = l_return_lines(i).subinventory;
    
      if (l_count = 0) then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '行' || l_return_lines(i).line_number || '退货仓库' || l_return_lines(i)
                      .subinventory || '与原始单据的仓库不符！';
        RAISE Fnd_Api.g_Exc_Error;
      end if;
    
    end loop;
  
    begin
      select crh.return_id, crh.organization_id
        into l_cux_return_id, l_organization_id
        from cux_return_headers crh
       where crh.return_number = p_return_header.trx_order_number;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取原始单据ID出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    --if pass validation
  
    --get ou id
    begin
      select iov.OPERATING_UNIT
        into l_org_id
        from inv_organization_info_v iov
       where iov.ORGANIZATION_ID = l_organization_id;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取OU出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    begin
    
      l_User_Resp.Org_Id    := l_org_id;
      l_User_Resp.Appl_Name := 'PO';
      Cux_Soa_Pub.Set_User_Responsibility(l_User_Resp);
      Fnd_Global.Apps_Initialize(User_Id           => l_User_Resp.User_Id,
                                 Resp_Id           => l_User_Resp.Resp_Id,
                                 Resp_Appl_Id      => l_User_Resp.Resp_Appl_Id,
                                 Security_Group_Id => 0,
                                 Server_Id         => NULL);
      Mo_Global.Set_Policy_Context('S', l_User_Resp.Org_Id);
    
      l_req_id := fnd_request.submit_request('CUX',
                                             'CUX_CREATE_RTN_DOC',
                                             null,
                                             sysdate,
                                             false,
                                             l_cux_return_id);
    
      COMMIT;
    
    exception
      when others then
        l_req_id := 0;
    END;
  
    if (l_req_id > 0) then
      null;
    else
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '提交创建退货单请求出错：' || SQLERRM;
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE process_inv_doc(p_inv_header Cux_Wms_Material_Trx_Headers%rowtype,
                            p_inv_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                            x_Ret_Sts    OUT VARCHAR2,
                            x_Error_Msg  OUT VARCHAR2) is
    l_header_id          number;
    l_error_code         varchar2(100);
    l_count              number;
    v_count_period       number;
    l_last_index         number;
    l_transfer_status    varchar2(100);
    l_to_organization_id number;
    l_count_post         number;
  begin
    x_Ret_Sts := 'S';
  
    begin
      select count(1)
        into l_count_post
        from cux_inv_material_lines cml, cux_inv_material_headers cmh
       where cml.header_id = cmh.header_id
         and cmh.req_number = p_inv_header.trx_order_number
            --and cml.line_number = p_inv_lines.
            --and cmh.inv_type = l_doc_type --4
         and cmh.organization_id = p_inv_header.organization_id
         and cml.status = '2';
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '检查是否过账出错：' || sqlerrm;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    if (l_count_post > 0) then
      x_Ret_Sts   := fnd_api.G_RET_STS_SUCCESS;
      x_Error_Msg := '单据已过账';
      RETURN;
    end if;
  
    begin
      select count(1)
        into v_count_period
        FROM ORG_ACCT_PERIODS Z, MFG_LOOKUPS X
       WHERE X.LOOKUP_TYPE(+) = 'MTL_ACCT_PERIOD_STATUS'
         AND X.ENABLED_FLAG(+) = 'Y'
         and trunc(sysdate) between Z.PERIOD_START_DATE and
             Z.SCHEDULE_CLOSE_DATE
         and z.organization_id = p_inv_header.organization_id
         and x.LOOKUP_CODE = 3
         AND X.LOOKUP_CODE(+) =
             DECODE(NVL(Z.PERIOD_CLOSE_DATE, SYSDATE),
                    Z.PERIOD_CLOSE_DATE,
                    DECODE(Z.OPEN_FLAG,
                           'N',
                           DECODE(SUMMARIZED_FLAG, 'N', 65, 66),
                           'Y',
                           4,
                           'P',
                           2,
                           4),
                    3);
    exception
      when others then
        v_count_period := 0;
    end;
  
    if (v_count_period = 0) then
      --period is not open
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '期间未打开';
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  
    --验证行数是否一致
    l_last_index := p_inv_lines.count;
  
    select count(1)
      into l_count
      from cux_inv_material_headers cih, cux_inv_material_lines cil
     where cih.req_number = p_inv_header.trx_order_number
       and cih.organization_id = p_inv_header.organization_id
       and cih.header_id = cil.header_id;
  
    if (l_count <> l_last_index) then
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '交易行数' || l_last_index || '不等于原始单据行数！';
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  
    begin
      select cim.header_id, cim.transfer_status, cim.to_organization_id
        into l_header_id, l_transfer_status, l_to_organization_id
        from cux_inv_material_headers cim
       where cim.req_number = p_inv_header.trx_order_number
         and cim.organization_id = p_inv_header.organization_id;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取单号出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    if (p_inv_header.trx_order_type = 'ORG_TRANS') then
      begin
        select count(1)
          into v_count_period
          FROM ORG_ACCT_PERIODS Z, MFG_LOOKUPS X
         WHERE X.LOOKUP_TYPE(+) = 'MTL_ACCT_PERIOD_STATUS'
           AND X.ENABLED_FLAG(+) = 'Y'
           and trunc(sysdate) between Z.PERIOD_START_DATE and
               Z.SCHEDULE_CLOSE_DATE
           and z.organization_id = l_to_organization_id
           and x.LOOKUP_CODE = 3
           AND X.LOOKUP_CODE(+) =
               DECODE(NVL(Z.PERIOD_CLOSE_DATE, SYSDATE),
                      Z.PERIOD_CLOSE_DATE,
                      DECODE(Z.OPEN_FLAG,
                             'N',
                             DECODE(SUMMARIZED_FLAG, 'N', 65, 66),
                             'Y',
                             4,
                             'P',
                             2,
                             4),
                      3);
      exception
        when others then
          v_count_period := 0;
      end;
    
      if (v_count_period = 0) then
        --period is not open
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '目的组织期间未打开';
        RAISE Fnd_Api.g_Exc_Error;
      end if;
    end if;
  
    select count(1)
      into l_count
      from cux_inv_material_lines cml
     where cml.header_id = l_header_id
       and cml.status = '1';
  
    if (l_count = 0) then
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据已过账';
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  
    --update trx date
    begin
      update cux_inv_material_lines cil
         set cil.transaction_date = sysdate
       where cil.header_id = l_header_id;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '更新交易日期出错';
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    if (p_inv_header.trx_order_type in ('L_TRANS', 'M_TRANS', 'ORG_TRANS')) then
    
      if (l_transfer_status = 'TRANSFERRED' and
         p_inv_header.bound_type = '2') then
        begin
          UPDATE CUX_INV_MATERIAL_HEADERS CIH
             set cih.transfer_status = 'ISSUED'
           where cih.header_id = l_header_id;
        exception
          when others then
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '更新单据状态为拨出时出错：' || SQLERRM;
            RAISE Fnd_Api.g_Exc_Error;
        end;
      elsif (l_transfer_status in ('ISSUED', 'TRANSFERRED') and
            p_inv_header.bound_type = '1') then
        CUX_INV_MATERIAL_REQ_PKG.MAIN_CHECK(P_HEADER_ID  => l_header_id,
                                            P_ERROR_CODE => l_error_code);
        IF l_error_code IS NULL THEN
          -- fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理成功');
        
          begin
            UPDATE CUX_INV_MATERIAL_HEADERS CIH
               set cih.transfer_status = 'POSTED'
             where cih.header_id = l_header_id;
          exception
            when others then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := '更新单据状态为过账时出错：' || SQLERRM;
              RAISE Fnd_Api.g_Exc_Error;
          end;
        
        ELSE
          fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理失败');
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '处理失败：' || l_error_code;
          RAISE Fnd_Api.g_Exc_Error;
        END IF;
      else
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '单据类型或者抛转状态不正确1';
        RAISE Fnd_Api.g_Exc_Error;
      end if;
    
    elsif (p_inv_header.trx_order_type in ('MISC_RCV', 'MISC_ISSUE') and --杂入，杂出 不判断方向，直接过账
          l_transfer_status = 'TRANSFERRED') then
      CUX_INV_MATERIAL_REQ_PKG.MAIN_CHECK(P_HEADER_ID  => l_header_id,
                                          P_ERROR_CODE => l_error_code);
      IF l_error_code IS NULL THEN
        -- fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理成功');
        begin
          UPDATE CUX_INV_MATERIAL_HEADERS CIH
             set cih.transfer_status = 'POSTED'
           where cih.header_id = l_header_id;
        exception
          when others then
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '更新单据状态为过账时出错：' || SQLERRM;
            RAISE Fnd_Api.g_Exc_Error;
        end;
      ELSE
        fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理失败');
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '处理失败：' || l_error_code;
        RAISE Fnd_Api.g_Exc_Error;
      END IF;
    else
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据类型或者抛转状态不正确2';
      RAISE Fnd_Api.g_Exc_Error;
    
    end if;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误process inv doc：' || sqlerrm;
  end;
  PROCEDURE process_zr(p_zr_header Cux_Wms_Material_Trx_Headers%rowtype,
                       p_zr_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                       x_Ret_Sts   OUT VARCHAR2,
                       x_Error_Msg OUT VARCHAR2) is
    l_zr_id number;
    --l_doc_type   varchar2(100);
    l_error_code varchar2(100);
  begin
  
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_zr_header.trx_order_number,
    p_doc_type   => p_zr_header.trx_order_type);*/
  
    begin
      select cim.header_id
        into l_zr_id
        from cux_inv_material_headers cim
       where cim.req_number = p_zr_header.trx_order_number
      --and cim.inv_type = l_doc_type
      ; --4;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取杂入单号出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    CUX_INV_MATERIAL_REQ_PKG.MAIN_CHECK(P_HEADER_ID  => l_zr_id,
                                        P_ERROR_CODE => l_error_code);
  
    IF l_error_code IS NULL THEN
      fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理成功');
    ELSE
      fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理失败');
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '杂入单处理失败：' || l_error_code;
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;

  PROCEDURE process_zf(p_zf_header Cux_Wms_Material_Trx_Headers%rowtype,
                       p_zf_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                       x_Ret_Sts   OUT VARCHAR2,
                       x_Error_Msg OUT VARCHAR2) is
    l_zf_id number;
    --l_doc_type   varchar2(100);
    l_error_code varchar2(100);
  begin
  
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_zf_header.trx_order_number,
    p_doc_type   => p_zf_header.trx_order_type);*/
  
    begin
      select cim.header_id
        into l_zf_id
        from cux_inv_material_headers cim
       where cim.req_number = p_zf_header.trx_order_number
      --and cim.inv_type = l_doc_type
      ; --3;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取杂发单号出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    CUX_INV_MATERIAL_REQ_PKG.MAIN_CHECK(P_HEADER_ID  => l_zf_id,
                                        P_ERROR_CODE => l_error_code);
  
    IF l_error_code IS NULL THEN
      fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理成功');
    ELSE
      fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理失败');
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '杂发单处理失败：' || l_error_code;
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;

  PROCEDURE process_db(p_db_header Cux_Wms_Material_Trx_Headers%rowtype,
                       p_db_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                       x_Ret_Sts   OUT VARCHAR2,
                       x_Error_Msg OUT VARCHAR2) is
    l_db_id number;
    --l_doc_type   varchar2(100);
    l_error_code varchar2(100);
  begin
  
    --get doc type
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_db_header.trx_order_number,
    p_doc_type   => p_db_header.trx_order_type);*/
  
    begin
      select cim.header_id
        into l_db_id
        from cux_inv_material_headers cim
       where cim.req_number = p_db_header.trx_order_number
      --and cim.inv_type = l_doc_type
      ; --5;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取调拨单号出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
    --处理程序需注册为并发，让用户可手工提交处理出错的记录
    CUX_INV_MATERIAL_REQ_PKG.MAIN_CHECK(P_HEADER_ID  => l_db_id,
                                        P_ERROR_CODE => l_error_code);
  
    IF l_error_code IS NULL THEN
      fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理成功');
    ELSE
      fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理失败');
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '调拨单处理失败：' || l_error_code;
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;

  PROCEDURE process_yddb(p_yddb_header Cux_Wms_Material_Trx_Headers%rowtype,
                         p_yddb_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                         x_Ret_Sts     OUT VARCHAR2,
                         x_Error_Msg   OUT VARCHAR2) is
    l_yddb_id number;
    --l_doc_type   varchar2(100);
    l_error_code varchar2(100);
  begin
  
    --get doc type
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_yddb_header.trx_order_number,
    p_doc_type   => p_yddb_header.trx_order_type);*/
  
    begin
      select cim.header_id
        into l_yddb_id
        from cux_inv_material_headers cim
       where cim.req_number = p_yddb_header.trx_order_number
      --and cim.inv_type = l_doc_type
      ; --6;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取异地调拨单号出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    CUX_INV_MATERIAL_REQ_PKG.MAIN_CHECK(P_HEADER_ID  => l_yddb_id,
                                        P_ERROR_CODE => l_error_code);
  
    IF l_error_code IS NULL THEN
      fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理成功');
    ELSE
      fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理失败');
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '异地调拨单处理失败：' || l_error_code;
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE process_xodb(p_xodb_header Cux_Wms_Material_Trx_Headers%rowtype,
                         p_xodb_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                         x_Ret_Sts     OUT VARCHAR2,
                         x_Error_Msg   OUT VARCHAR2) is
    l_xodb_id number;
    --l_doc_type   varchar2(100);
    l_error_code varchar2(100);
  begin
  
    --get doc type
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_xodb_header.trx_order_number,
    p_doc_type   => p_xodb_header.trx_order_type);*/
  
    begin
      select cim.header_id
        into l_xodb_id
        from cux_inv_material_headers cim
       where cim.req_number = p_xodb_header.trx_order_number
      --and cim.inv_type = l_doc_type
      ; --7;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取跨组织调拨单号出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
    --处理程序需注册为并发，让用户可手工提交处理出错的记录
    CUX_INV_MATERIAL_REQ_PKG.MAIN_CHECK(P_HEADER_ID  => l_xodb_id,
                                        P_ERROR_CODE => l_error_code);
  
    IF l_error_code IS NULL THEN
      fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理成功');
    ELSE
      fnd_file.PUT_LINE(fnd_file.OUTPUT, '处理失败');
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '异地调拨单处理失败：' || l_error_code;
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;

  PROCEDURE process_wip_doc(p_wip_header Cux_Wms_Material_Trx_Headers%rowtype,
                            p_wip_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                            x_Ret_Sts    OUT VARCHAR2,
                            x_Error_Msg  OUT VARCHAR2) is
    v_count_period   number;
    l_last_index     number;
    l_count          number;
    l_count_post     number;
    l_count_not_post number;
    l_doc_type       varchar2(100);
    l_doc_no         varchar2(100);
    l_process_status varchar2(10); --ADD BY BRUCE ON 20160219 FOR AVOIDING PARALELL PROCESS
  begin
    --validate  
    x_Ret_Sts := 'S';
  
    if (p_wip_header.trx_order_type in ('WIP_FUL_PL', 'WIP_FRG_PL')) then
      --拉式退料
    
      select count(1)
        into l_count_not_post
        from cux.cux_ready_item cri
       where cri.doc_type in ('拉式备料', '拉式补料', '拉式超龄')
         and cri.doc_status = '未过账'
         and cri.wip_entity_name = p_wip_header.wip_entity_name
         and cri.organization_id = p_wip_header.organization_id;
    
      if (l_count_not_post > 0) then
      
        begin
          select cri.doc_type, cri.doc_no
            into l_doc_type, l_doc_no
            from cux_ready_item cri
           where cri.doc_type in ('拉式备料', '拉式补料', '拉式超龄')
             and cri.doc_status = '未过账'
             and cri.wip_entity_name = p_wip_header.wip_entity_name
             and cri.organization_id = p_wip_header.organization_id
             and rownum = 1;
        
        exception
          when others then
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '获取未过账拉式单据出错：' || sqlerrm;
            RAISE Fnd_Api.g_Exc_Error;
        end;
      
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '存在未过账的拉式发料:' || l_doc_type || '-' || l_doc_no;
        RAISE Fnd_Api.g_Exc_Error;
      end if;
    
      select count(1)
        into l_count_post
        from CUX_WIP_MATERIAL_RETURN cmr, wip_entities we
       where cmr.orig_doc_number = p_wip_header.wms_trx_number
         and cmr.orig_system = p_wip_header.source_system
         and we.wip_entity_id = cmr.wip_entity_id
         and we.organization_id = p_wip_header.organization_id
         and we.wip_entity_name = p_wip_header.wip_entity_name
         and cmr.status = '2';
    else
      --推式单据
      select count(1)
        into l_count_post
        from cux_ready_item cri
       where cri.doc_no = p_wip_header.trx_order_number
         and cri.organization_id = p_wip_header.organization_id
            -- and cri.doc_type = l_doc_type
         and cri.doc_status = '已过账';
    end if;
  
    if (l_count_post > 0) then
      x_Ret_Sts   := fnd_api.G_RET_STS_SUCCESS;
      x_Error_Msg := '单据已过账';
      RAISE Fnd_Api.g_Exc_Error; --MODIFIED BY BRUCE ON 20160219 FOR AVOIDING PARALELL PROCESS
    end if;
  
    begin
      select count(1)
        into v_count_period
        FROM ORG_ACCT_PERIODS Z, MFG_LOOKUPS X
       WHERE X.LOOKUP_TYPE(+) = 'MTL_ACCT_PERIOD_STATUS'
         AND X.ENABLED_FLAG(+) = 'Y'
         and trunc(sysdate) between Z.PERIOD_START_DATE and
             Z.SCHEDULE_CLOSE_DATE
         and z.organization_id = p_wip_header.organization_id
         and x.LOOKUP_CODE = 3
         AND X.LOOKUP_CODE(+) =
             DECODE(NVL(Z.PERIOD_CLOSE_DATE, SYSDATE),
                    Z.PERIOD_CLOSE_DATE,
                    DECODE(Z.OPEN_FLAG,
                           'N',
                           DECODE(SUMMARIZED_FLAG, 'N', 65, 66),
                           'Y',
                           4,
                           'P',
                           2,
                           4),
                    3);
    exception
      when others then
        v_count_period := 0;
    end;
    x_Error_Msg := '222';
    if (v_count_period = 0) then
      --period is not open
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '期间未打开' || p_wip_header.organization_id;
      RAISE Fnd_Api.g_Exc_Error;
    end if;
    x_Error_Msg := '333';
    --验证行数是否一致
    l_last_index := p_wip_lines.count;
    x_Error_Msg  := '444';
    if (p_wip_header.trx_order_type in ('WIP_FUL_PL', 'WIP_FRG_PL')) then
      if (p_wip_header.source_system = 'SMTN') then
        l_count := l_last_index;
      ELSE
        select count(1)
          into l_count
          from cux_wip_material_return cwr
         where cwr.transaction_id =
               to_number(p_wip_header.trx_order_number)
           and cwr.select_flag = 'Y';
      end if;
    
    else
      select count(1)
        into l_count
        from cux_ready_item cri
       where cri.doc_no = p_wip_header.trx_order_number
         and cri.organization_id = p_wip_header.organization_id
         and nvl(cri.ready_qty, 0) > 0;
    end if;
    x_Error_Msg := '666';
    if (l_count <> l_last_index) then
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '交易行数' || l_last_index || '不等于原始单据行数！';
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  
    --end of validate
  
    --START OF ADD BY BRUCE ON 20160219 FOR AVOIDING PARALELL PROCESS
  
    if (p_wip_header.trx_order_type in ('WIP_FUL_PL', 'WIP_FRG_PL')) then
      BEGIN
      select nvl(cmr.process_status, 'NA')
        into l_process_status
        from CUX_WIP_MATERIAL_RETURN cmr, wip_entities we
       where cmr.orig_doc_number = p_wip_header.wms_trx_number
         and cmr.orig_system = p_wip_header.source_system
         and we.wip_entity_id = cmr.wip_entity_id
         and we.organization_id = p_wip_header.organization_id
         and we.wip_entity_name = p_wip_header.wip_entity_name
         and cmr.process_status is not null
         and rownum = 1;
      EXCEPTION WHEN OTHERS THEN
        l_process_status:='NA';
      END;
    ELSE
      BEGIN
      select nvl(cri.process_status, 'NA')
        into l_process_status
        from cux_ready_item cri
       where cri.doc_no = p_wip_header.trx_order_number
         and cri.organization_id = p_wip_header.organization_id
         and rownum = 1;
         EXCEPTION WHEN OTHERS THEN
        l_process_status:='NA';
      END;
    END IF;
  
    IF (l_process_status = 'P') THEN
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '已存在处理中的单据：' || p_wip_header.trx_order_number;
      RAISE Fnd_Api.g_Exc_Error;
    else
      if (p_wip_header.trx_order_type in
         ('WIP_FUL_PL', 'WIP_FRG_PL')) then
         --拉式退料
         null;
        /*update CUX_WIP_MATERIAL_RETURN CMR
           set CMR.process_status = 'P'
         where cmr.orig_doc_number = p_wip_header.wms_trx_number
           and cmr.orig_system = p_wip_header.source_system
           and cmr.wip_entity_id =
               (SELECT WE.WIP_ENTITY_ID
                  FROM WIP_ENTITIES WE
                 WHERE WE.WIP_ENTITY_NAME = p_wip_header.wip_entity_name
                   AND WE.ORGANIZATION_ID = p_wip_header.organization_id);*/
      ELSE
        update cux_ready_item cri
           set cri.process_status = 'P'
         where cri.doc_no = p_wip_header.trx_order_number
           and cri.organization_id = p_wip_header.organization_id;
      END IF;
      COMMIT;--立即提交已阻止后面同样的单据被回抛回来
    END IF;
  
    --END OF ADD BY BRUCE ON 20160219 FOR AVOIDING PARALELL PROCESS
  
    if (p_wip_header.trx_order_type in ('WIP_RLS_PS', 'WIP_RLS_PL')) then
      process_prepare_item(p_prepare_header => p_wip_header,
                           p_prepare_lines  => p_wip_lines,
                           x_Ret_Sts        => x_Ret_Sts,
                           x_Error_Msg      => x_Error_Msg);
    elsif (p_wip_header.trx_order_type in ('WIP_RPL_PS', 'WIP_RPL_PL')) then
      process_replenish_item(p_replenish_header => p_wip_header,
                             p_replenish_lines  => p_wip_lines,
                             x_Ret_Sts          => x_Ret_Sts,
                             x_Error_Msg        => x_Error_Msg);
    elsif (p_wip_header.trx_order_type in ('WIP_OVR_PS', 'WIP_OVR_PL')) then
      process_over_item(p_over_header => p_wip_header,
                        p_over_lines  => p_wip_lines,
                        x_Ret_Sts     => x_Ret_Sts,
                        x_Error_Msg   => x_Error_Msg);
    elsif (p_wip_header.trx_order_type in
          ('WIP_NEG_PS', 'WIP_FRG_PS', 'WIP_FUL_PL', 'WIP_FRG_PL')) then
      process_return_item(p_return_header => p_wip_header,
                          p_return_lines  => p_wip_lines,
                          x_Ret_Sts       => x_Ret_Sts,
                          x_Error_Msg     => x_Error_Msg);
    elsif (p_wip_header.trx_order_type = 'WIP_CMPL') then
      process_complete_item(p_complete_header => p_wip_header,
                            p_complete_lines  => p_wip_lines,
                            x_Ret_Sts         => x_Ret_Sts,
                            x_Error_Msg       => x_Error_Msg);
    else
      null;
    end if;
  
    --START OF ADD BY BRUCE ON 20160219 FOR AVOIDING PARALELL PROCESS
  
     if (p_wip_header.trx_order_type in
         ('WIP_FUL_PL', 'WIP_FRG_PL')) then
         --拉式退料
        update CUX_WIP_MATERIAL_RETURN CMR
           set CMR.process_status = ''
         where cmr.orig_doc_number = p_wip_header.wms_trx_number
           and cmr.orig_system = p_wip_header.source_system
           and cmr.wip_entity_id =
               (SELECT WE.WIP_ENTITY_ID
                  FROM WIP_ENTITIES WE
                 WHERE WE.WIP_ENTITY_NAME = p_wip_header.wip_entity_name
                   AND WE.ORGANIZATION_ID = p_wip_header.organization_id);
      ELSE
        update cux_ready_item cri
           set cri.process_status = ''
         where cri.doc_no = p_wip_header.trx_order_number
           and cri.organization_id = p_wip_header.organization_id;
      END IF;
    COMMIT;
    --END OF ADD BY BRUCE ON 20160219 FOR AVOIDING PARALELL PROCESS
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误3：' || x_Error_Msg || sqlerrm;
  end;

  PROCEDURE process_prepare_item(p_prepare_header Cux_Wms_Material_Trx_Headers%rowtype,
                                 p_prepare_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                                 x_Ret_Sts        OUT VARCHAR2,
                                 x_Error_Msg      OUT VARCHAR2) is
    l_doc_status      varchar2(30);
    l_doc_type        varchar2(30);
    l_return_status   varchar2(10);
    l_return_msg      varchar2(2000);
    l_organization_id number;
  begin
    x_Ret_Sts := 'S';
    --get doc type
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_prepare_header.trx_order_number,
    p_doc_type   => p_prepare_header.trx_order_type);*/
  
    begin
      select cri.doc_status, cri.organization_id, cri.doc_type
        into l_doc_status, l_organization_id, l_doc_type
        from cux_ready_item cri
       where cri.doc_no = p_prepare_header.trx_order_number
         and cri.organization_id = p_prepare_header.organization_id
         AND ROWNUM = 1
      --and cri.doc_type = l_doc_type
      ;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取单据状态出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    -- Call the procedure
    if l_doc_status = '已过账' then
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据已过账,不能重复过账.';
      RAISE Fnd_Api.g_Exc_Error;
    else
    
      --update actual quantity
      for i in 1 .. p_prepare_lines.count loop
      
        update cux_ready_item cri
           set cri.temp_qty = p_prepare_lines(i).QUANTITY
         where cri.doc_no = p_prepare_header.trx_order_number
           and cri.line_number = p_prepare_lines(i).LINE_NUMBER
           and cri.organization_id = p_prepare_header.organization_id
        --and cri.doc_type = l_doc_type
        ;
        /* x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '单据xxx.'||p_prepare_lines(i).QUANTITY||p_prepare_header.trx_order_number||p_prepare_lines(i).LINE_NUMBER||p_prepare_header.organization_id;
        RAISE Fnd_Api.g_Exc_Error;*/
      end loop;
    
      cux_wip_transactions_pkg.process_trx(p_organization_id => l_organization_id,
                                           p_doc_no          => p_prepare_header.trx_order_number,
                                           p_doc_type        => l_doc_type,
                                           p_return_status   => l_return_status,
                                           p_return_msg      => l_return_msg);
    
      if l_return_status = 'E' then
        x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
        x_Error_Msg := '过账失败：' || l_return_msg;
      ELSE
        begin
          UPDATE CUX_READY_ITEM CRI
             set cri.transfer_status = 'POSTED'
           where cri.doc_no = p_prepare_header.trx_order_number
             and cri.organization_id = l_organization_id;
        exception
          when others then
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '更新单据状态为过账时出错：' || SQLERRM;
            RAISE Fnd_Api.g_Exc_Error;
        end;
      end if;
    end if;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE process_replenish_item(p_replenish_header Cux_Wms_Material_Trx_Headers%rowtype,
                                   p_replenish_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                                   x_Ret_Sts          OUT VARCHAR2,
                                   x_Error_Msg        OUT VARCHAR2) is
    l_doc_status       varchar2(30);
    l_doc_type         varchar2(30);
    l_return_status    varchar2(10);
    l_link_prepare_doc varchar2(30);
    l_return_msg       varchar2(2000);
    l_organization_id  number;
    l_wip_entity_id    number;
  begin
    x_Ret_Sts := 'S';
    --get doc type
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_replenish_header.trx_order_number,
    p_doc_type   => p_replenish_header.trx_order_type);*/
  
    begin
      select cri.doc_status,
             cri.organization_id,
             cri.wip_entity_id,
             cri.doc_type
        into l_doc_status, l_organization_id, l_wip_entity_id, l_doc_type
        from cux_ready_item cri
       where cri.doc_no = p_replenish_header.trx_order_number
         and cri.organization_id = p_replenish_header.organization_id
         AND ROWNUM = 1
      --and cri.doc_type = l_doc_type
      ;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取单据状态出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    IF cux_wip_transactions_pkg.GET_POST_FLAG(p_org_id => l_organization_id,
                                              p_wip_id => l_wip_entity_id,
                                              p_doc_no => p_replenish_header.trx_order_number) = 'Y' THEN
    
      begin
        select cri.attribute10
          into l_link_prepare_doc
          from cux_ready_item cri
         where cri.organization_id = p_replenish_header.organization_id
           and cri.doc_no = p_replenish_header.trx_order_number
           and rownum = 1;
      exception
        when others then
          l_link_prepare_doc := 'ERROR';
      end;
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '备料单' || l_link_prepare_doc || '未过帐,补料单' ||
                     p_replenish_header.trx_order_number || '不能过帐!';
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
  
    -- Call the procedure
    if l_doc_status = '已过账' then
    
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据已过账,不能重复过账.';
      RAISE Fnd_Api.g_Exc_Error;
    else
    
      --update actual quantity
      for i in 1 .. p_replenish_lines.count loop
      
        update cux_ready_item cri
           set cri.temp_qty = p_replenish_lines(i).QUANTITY
         where cri.doc_no = p_replenish_header.trx_order_number
           and cri.line_number = p_replenish_lines(i).LINE_NUMBER
           and cri.organization_id = p_replenish_header.organization_id
        --and cri.doc_type = l_doc_type
        ;
      
      end loop;
    
      cux_wip_transactions_pkg.process_trx(p_organization_id => l_organization_id,
                                           p_doc_no          => p_replenish_header.trx_order_number,
                                           p_doc_type        => l_doc_type,
                                           p_return_status   => l_return_status,
                                           p_return_msg      => l_return_msg);
    
      if l_return_status = 'E' then
        x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
        x_Error_Msg := '过账失败：' || l_return_msg;
      ELSE
        begin
          UPDATE CUX_READY_ITEM CRI
             set cri.transfer_status = 'POSTED'
           where cri.doc_no = p_replenish_header.trx_order_number
             and cri.organization_id = l_organization_id;
        exception
          when others then
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '更新单据状态为过账时出错：' || SQLERRM;
            RAISE Fnd_Api.g_Exc_Error;
        end;
      end if;
    end if;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE process_over_item(p_over_header Cux_Wms_Material_Trx_Headers%rowtype,
                              p_over_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                              x_Ret_Sts     OUT VARCHAR2,
                              x_Error_Msg   OUT VARCHAR2) is
  
    l_doc_status      varchar2(30);
    l_doc_type        varchar2(30);
    l_return_status   varchar2(10);
    l_return_msg      varchar2(2000);
    l_organization_id number;
  begin
    x_Ret_Sts := 'S';
    --get doc type
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_over_header.trx_order_number,
    p_doc_type   => p_over_header.trx_order_type);*/
  
    begin
      select cri.doc_status, cri.organization_id, cri.doc_type
        into l_doc_status, l_organization_id, l_doc_type
        from cux_ready_item cri
       where cri.doc_no = p_over_header.trx_order_number
         and cri.organization_id = p_over_header.organization_id
         AND ROWNUM = 1
      --and cri.doc_type = l_doc_type
      ;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取单据状态出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    -- Call the procedure
    if l_doc_status = '已过账' then
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据已过账,不能重复过账.';
      RAISE Fnd_Api.g_Exc_Error;
    else
    
      --update actual quantity
      for i in 1 .. p_over_lines.count loop
      
        update cux_ready_item cri
           set cri.temp_qty = p_over_lines(i).QUANTITY
         where cri.doc_no = p_over_header.trx_order_number
           and cri.line_number = p_over_lines(i).LINE_NUMBER
           and cri.organization_id = p_over_header.organization_id
        --and cri.doc_type = l_doc_type
        ;
      
      end loop;
    
      cux_wip_transactions_pkg.process_trx(p_organization_id => l_organization_id,
                                           p_doc_no          => p_over_header.trx_order_number,
                                           p_doc_type        => l_doc_type,
                                           p_return_status   => l_return_status,
                                           p_return_msg      => l_return_msg);
    
      if l_return_status = 'E' then
      
        x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
        x_Error_Msg := '过账失败：' || l_return_msg;
      ELSE
        begin
          UPDATE CUX_READY_ITEM CRI
             set cri.transfer_status = 'POSTED'
           where cri.doc_no = p_over_header.trx_order_number
             and cri.organization_id = l_organization_id;
        exception
          when others then
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '更新单据状态为过账时出错：' || SQLERRM;
            RAISE Fnd_Api.g_Exc_Error;
        end;
      end if;
    end if;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE process_return_item(p_return_header Cux_Wms_Material_Trx_Headers%rowtype,
                                p_return_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                                x_Ret_Sts       OUT VARCHAR2,
                                x_Error_Msg     OUT VARCHAR2) is
    l_doc_status      varchar2(30);
    ltbl_return_lines Cux_Wms_Pub.Material_Trx_Lines_Tab;
    l_doc_type        varchar2(30);
    l_ws_code         varchar2(30);
    x_msg_count       number;
    x_trans_count     number;
    l_count_released  number;
    l_issue_date      date;
    l_ret_status      varchar2(10);
    --l_push_issued_qty       number := 0;
    l_total_issued_qty number := 0;
    l_total_com_qty    number := 0;
    --l_completed_qty         number := 0;
    --l_oper_need_qty         number := 0;
    --l_oper_issued_qty       number := 0;
    l_oper_rtn_qty  number := 0;
    l_avail_rtn_qty number := 0;
    --l_push_total_rtn_qty    number := 0;
    l_remain_rtn_qty number := 0;
    l_return_status  varchar2(10);
    l_return_msg     varchar2(2000);
    --l_wip_entity_name       varchar2(100);
    l_return_count    number;
    l_organization_id number;
    l_item_id         number;
    l_line_number     number;
    l_trx_id          number;
    l_msg_data        varchar2(2000);
    l_supply_type     number;
    l_wip_id          number;
    l_uom             varchar2(100);
    l_user_id         number := fnd_global.USER_ID;
    l_login_id        number := fnd_global.LOGIN_ID;
    l_iface_rec       inv.mtl_transactions_interface%ROWTYPE;
    l_supply_loc_id   number;
    l_supply_SUBINV   varchar2(10);
    l_loc_id          number := 0;
    l_qty_open        number := 0;
    --l_ishave                number;
    l_issued_qty        number := 0;
    l_remain_issued_qty number := 0;
    l_rtn_qty           number := 0;
    l_req_qty           number := 0;
    --l_hook_no               varchar2(100);
    l_count_exist number;
    --l_full_rtn_flag         number;
    l_issue_subinv varchar2(10);
    l_source_type  number;
    l_count_err    number;
    l_count_post   number;
    l_push_flag    varchar2(10);
    --l_count_push            number;
    l_accumulative_quantity NUMBER := 0;
    l_count_rtn             number;
  
    V_TRANSACTION_TYPE_ID number;
    V_DISPOSITIONS_NAME   varchar2(100); -- '拉式物料盘点' ; -- '盘赢调整';  -- 帐户别名
  
    V_ADJ_QTY number;
    -- V_ADJ_QTY := NVL(V_RETURN_QUANTITY,0) - ( NVL(V_QUANTITY_OPEN,0) - V_ALL_RETURN_QTY ) ;
    V_DISPOSITIONS_ID number;
    --l_issued_qty      number;
    cursor cur_return_item(p_trx_id number) is
      select *
        from CUX_WIP_MATERIAL_RETURN cmr
       where cmr.transaction_id = p_trx_id
         AND CMR.SELECT_FLAG = 'Y'
         and cmr.status = '1'
         and NVL(cmr.push_flag, 'N') = 'N';
  
    cursor cur_return_item2(p_trx_id number) is
      select *
        from CUX_WIP_MATERIAL_RETURN cmr
       where cmr.transaction_id = p_trx_id
         AND CMR.SELECT_FLAG = 'Y'
         and cmr.status = '1'
         and NVL(cmr.push_flag, 'N') = 'Y';
  
    cursor cur_item_operation(p_item_id number) is
      select wro.operation_seq_num, WRO.QUANTITY_ISSUED
        from wip_requirement_operations wro /*,wip_operations wo*/
       where wro.wip_entity_id = l_wip_id
         AND wro.organization_id = p_return_header.organization_id
            /*and wro.wip_entity_id=wo.wip_entity_id
            and wro.operation_seq_num=wo.operation_seq_num*/
         and wro.inventory_item_id = p_item_id;
  
  begin
    x_Ret_Sts := 'S';
  
    if (p_return_header.trx_order_type in ('WIP_FUL_PL', 'WIP_FRG_PL')) then
      --拉式退料（整单）,拉式退料（零星）,零星由SMTN发起，整单由ERP发起
    
      if (p_return_header.trx_order_type = 'WIP_FRG_PL') then
        l_source_type := 1;
      else
        l_source_type := 2;
      end if;
      --PASS VALIDATION
      --如果是拉式零星退料，出错再处理时先检查客制单据是否已经产生
    
      BEGIN
        select count(1)
          into l_count_exist
          from cux_wip_material_return cmr, wip_entities we
         where cmr.orig_doc_number = p_return_header.Wms_Trx_Number
           and cmr.wip_entity_id = we.wip_entity_id
           and we.wip_entity_name = p_return_header.wip_entity_name
           and we.organization_id = p_return_header.organization_id;
      EXCEPTION
        WHEN OTHERS THEN
          l_count_exist := 0;
      END;
    
      if (l_count_exist > 0) then
        --l_trx_id := to_number(p_return_header.trx_order_number);
      
        /* BEGIN
          select count(1)
            into l_count_post
            from cux_wip_material_return cmr,wip_entities we
           where cmr.orig_doc_number = p_return_header.Wms_Trx_Number
             and we.wip_entity_id=cmr.wip_entity_id
             and we.organization_id=p_return_header.wip_entity_name
             and we.organization_id=p_return_header.organization_id
             and cmr.status = '2';
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '统计过账记录出错：' || SQLERRM;
            RAISE Fnd_Api.g_Exc_Error;
        END;
        
        if (l_count_post > 0) then
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '单据已过账！';
          RAISE Fnd_Api.g_Exc_Error;
        end if;*/
      
        begin
          SELECT CMR.TRANSACTION_ID
            into l_trx_id
            FROM CUX.CUX_WIP_MATERIAL_RETURN CMR, wip_entities we
           WHERE CMR.ORIG_DOC_NUMBER = p_return_header.Wms_Trx_Number
             and we.wip_entity_id = cmr.wip_entity_id
             and we.organization_id = p_return_header.wip_entity_name
             and we.organization_id = p_return_header.organization_id
             AND ROWNUM = 1;
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '获取单据ID出错：' || SQLERRM;
            RAISE Fnd_Api.g_Exc_Error;
        END;
      
      end if;
    
      if ( /*p_return_header.trx_order_type = 'WIP_FRG_PL' and*/
          l_count_exist = 0) then
        --针对零星,整单拉式，创建单据再过账
      
        --获取SMTN返回的物料以外的其他物料来退，退料数量都给0，以便后面做杂项
        validate_return_item(p_return_header => p_return_header,
                             p_return_lines  => p_return_lines,
                             p_source_type   => l_source_type,
                             x_return_lines  => ltbl_return_lines,
                             x_Ret_Sts       => x_Ret_Sts,
                             x_Error_Msg     => x_Error_Msg);
      
        if (x_Ret_Sts <> 'S') then
        
          RETURN;
        end if;
      
        l_line_number := 0;
        select cux_wip_material_return_h_s.nextval into l_trx_id from dual;
      
        for j in 1 .. ltbl_return_lines.count loop
          /*x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := 'XXe11' ||
                         p_return_header.wip_entity_name;
          RAISE Fnd_Api.g_Exc_Error;*/
          if ltbl_return_lines(j).item_number is null then
          
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '料号为空,不能退料.';
            RAISE Fnd_Api.g_Exc_Error;
          
          end if;
        
          --get item id
          begin
          
            select msib.inventory_item_id
              into l_item_id
              from mtl_system_items_b msib
             where msib.organization_id = p_return_header.organization_id
               and msib.segment1 = ltbl_return_lines(j).item_number;
          
          exception
            when others then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := '获取物料ID出错：' || SQLERRM;
              RAISE Fnd_Api.g_Exc_Error;
          end;
          --get wip id
          begin
          
            select we.wip_entity_id, wdj.attribute3
              into l_wip_id, l_ws_code
              from wip_entities we, wip_discrete_jobs wdj
             where we.wip_entity_name = p_return_header.wip_entity_name
               and we.organization_id = p_return_header.organization_id
               and we.wip_entity_id = wdj.wip_entity_id;
          
          exception
            when others then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := '获取工单ID出错：' || SQLERRM;
              RAISE Fnd_Api.g_Exc_Error;
          end;
        
          --get supply type
          begin
            select wro.wip_supply_type
              into l_supply_type
              from wip_requirement_operations wro
             where wro.organization_id = p_return_header.organization_id
               and wro.wip_entity_id = l_wip_id
               and wro.inventory_item_id = l_item_id
               and rownum = 1;
          exception
            when others then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := '获取供应类型出错：' || SQLERRM;
              RAISE Fnd_Api.g_Exc_Error;
          end;
        
          IF (l_supply_type = 1) THEN
            l_push_flag := 'Y';
          else
            l_push_flag := 'N';
          END IF;
        
          --get supply location id
          if (l_push_flag = 'N') then
            --拉式才获取车间仓库货位
          
            begin
            
              select distinct wro.supply_locator_id
                into l_supply_loc_id
                from wip_requirement_operations wro,
                     mtl_system_items_b         msib
               where 1 = 1
                 and wro.organization_id = p_return_header.organization_id
                 and wro.wip_entity_id = l_wip_id
                 and wro.inventory_item_id = msib.inventory_item_id
                 and msib.segment1 = ltbl_return_lines(j).item_number
                 and msib.organization_id = p_return_header.organization_id
                 and wro.wip_supply_type in (2, 3);
            
            exception
              when others then
                x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                x_Error_Msg := '获取车间货位ID出错：' || SQLERRM;
                RAISE Fnd_Api.g_Exc_Error;
            end;
          
            --GET SUPPLY SUBINV
            BEGIN
              SELECT MIL.SUBINVENTORY_CODE
                INTO l_supply_SUBINV
                FROM MTL_ITEM_LOCATIONS MIL
               WHERE MIL.INVENTORY_LOCATION_ID = l_supply_loc_id;
            EXCEPTION
              WHEN OTHERS THEN
                x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                x_Error_Msg := '获取车间仓库出错：' || SQLERRM;
                RAISE Fnd_Api.g_Exc_Error;
            END;
          
            --get issue subinv
            begin
              select max(mmt.locator_id), max(mmt.subinventory_code)
                into l_loc_id, l_issue_subinv
                from mtl_material_transactions mmt
               where 1 = 1
                 and mmt.transaction_type_id =
                     (select transaction_type_id
                        from mtl_transaction_types
                       where transaction_type_name = '拉式物料发放')
                 and mmt.primary_quantity < 0
                 and nvl(mmt.attribute9, 'N') = 'N'
                 and mmt.organization_id = p_return_header.organization_id
                 and mmt.inventory_item_id = l_item_id
                 and mmt.transfer_locator_id = l_supply_loc_id;
            exception
              when others then
                x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                x_Error_Msg := '获取发料仓库/货位出错：' || SQLERRM;
                RAISE Fnd_Api.g_Exc_Error;
            end;
          
          else
            begin
              /*select cws.location_id
               into l_loc_id
               from cux_sub_loc_set_b cws
              where 1 = 1
                AND cws.organization_id = p_return_header.organization_id
                AND cws.inventory_item_id = l_item_id
                AND cws.ws_code = l_ws_code
                AND cws.sub = ltbl_return_lines(j).Subinventory;*/
            
              select max(mmt.locator_id), max(mmt.subinventory_code)
                into l_loc_id, l_issue_subinv
                from MTL_MATERIAL_TRANSACTIONS mmt
               where transaction_type_id =
                     (select transaction_type_id
                        from mtl_transaction_types
                       where transaction_type_name = 'WIP 发放') --100 /* --2 */
                 and primary_quantity < 0
                 and mmt.organization_id = p_return_header.organization_id
                 and mmt.inventory_item_id = l_item_id
                    --and mmt.transfer_locator_id = l_supply_loc_id
                    --and mmt.locator_id = l_loc_id
                 and mmt.transaction_source_id = l_wip_id;
            exception
              when others then
                x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                x_Error_Msg := '推式发料货位出错：' || SQLERRM;
                RAISE Fnd_Api.g_Exc_Error;
            end;
            l_supply_loc_id := '';
            l_supply_SUBINV := '';
          end if;
        
          if ltbl_return_lines(j).Subinventory is null or ltbl_return_lines(j)
             .Subinventory <> l_issue_subinv then
            --目的子库
          
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '物料ID' || l_item_id || '子库为空或者跟发料子库不一致,不能退料.' || ltbl_return_lines(j)
                          .Subinventory;
            RAISE Fnd_Api.g_Exc_Error;
          
          end if;
        
          --GET QTY OPEN
        
          --get returned qty 
          if (l_source_type = 1) then
            --1, GET ISSUE QTY 零星退
            if (l_push_flag = 'Y') then
              BEGIN
                select nvl(sum(abs(mmt.primary_quantity)), 0) --sum(nvl(abs(mmt.primary_quantity), 0))
                  into l_issued_qty
                  from (select sum(nvl(primary_quantity, 0)) primary_quantity,
                               subinventory_code,
                               transaction_type_id,
                               organization_id,
                               locator_id,
                               Transaction_Source_Id,
                               inventory_item_id,
                               transfer_locator_id
                        -- INTO l_issued_qty
                          from MTL_MATERIAL_TRANSACTIONS
                         where transaction_type_id =
                               (select transaction_type_id
                                  from mtl_transaction_types
                                 where transaction_type_name = 'WIP 发放') --100 /* --2 */
                           and primary_quantity < 0
                         group by subinventory_code,
                                  transaction_type_id,
                                  organization_id,
                                  locator_id,
                                  Transaction_Source_Id,
                                  inventory_item_id,
                                  transfer_locator_id) mmt
                 where mmt.organization_id =
                       p_return_header.organization_id
                   and mmt.inventory_item_id = l_item_id
                      --and mmt.transfer_locator_id = l_supply_loc_id
                      --and mmt.locator_id = l_loc_id
                   and mmt.transaction_source_id = l_wip_id;
              EXCEPTION
                WHEN OTHERS THEN
                  x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                  x_Error_Msg := '获取退料单推式发料数量出错：' || SQLERRM;
                  RAISE Fnd_Api.g_Exc_Error;
              END;
            ELSE
              BEGIN
              
                select nvl(sum(abs(mmt.primary_quantity)), 0) --sum(nvl(abs(mmt.primary_quantity), 0))
                  into l_issued_qty
                  from (select sum(nvl(primary_quantity, 0)) primary_quantity,
                               subinventory_code,
                               transaction_type_id,
                               organization_id,
                               locator_id,
                               transfer_organization_id,
                               inventory_item_id,
                               transfer_locator_id
                        -- INTO l_issued_qty
                          from MTL_MATERIAL_TRANSACTIONS
                         where transaction_type_id =
                               (select transaction_type_id
                                  from mtl_transaction_types
                                 where transaction_type_name = '拉式物料发放') --100 /* --2 */
                           and primary_quantity < 0
                         group by subinventory_code,
                                  transaction_type_id,
                                  organization_id,
                                  locator_id,
                                  transfer_organization_id,
                                  inventory_item_id,
                                  transfer_locator_id) mmt
                 where mmt.organization_id =
                       p_return_header.organization_id
                   and mmt.inventory_item_id = l_item_id
                   and mmt.transfer_locator_id = l_supply_loc_id
                --and mmt.locator_id = l_loc_id
                ;
              EXCEPTION
                WHEN OTHERS THEN
                  x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                  x_Error_Msg := '获取零星退料单发料数量出错：' || SQLERRM;
                  RAISE Fnd_Api.g_Exc_Error;
              END;
            end if;
          
            --get return qty 
            if (l_push_flag = 'N') then
              BEGIN
                select nvl(sum(a.RETURN_QUANTITY), 0)
                  into l_rtn_qty
                  from cux_wip_material_return a
                 where a.SOURCE_TYPE = 1
                   and a.STATUS = '2'
                   and a.ORGANIZATION_ID = p_return_header.organization_id
                   and a.WIP_ENTITY_ID = l_wip_id
                   and a.inventory_item_id = l_item_id;
              
                /*select rtn.RETURN_QUANTITY
                 into l_rtn_qty
                 from (select a.ORGANIZATION_ID,
                              a.WIP_ENTITY_ID,
                              a.INVENTORY_ITEM_ID,
                              nvl(sum(a.RETURN_QUANTITY), 0) RETURN_QUANTITY
                         from cux_wip_material_return_v a
                        where a.SOURCE_TYPE = 1
                          and a.STATUS = '2'
                          and a.ORGANIZATION_ID =
                              p_return_header.organization_id
                          and a.WIP_ENTITY_ID = l_wip_id
                        group by a.ORGANIZATION_ID,
                                 a.WIP_ENTITY_ID,
                                 a.INVENTORY_ITEM_ID) rtn
                where rtn.ORGANIZATION_ID =
                      p_return_header.organization_id
                  and rtn.WIP_ENTITY_ID = l_wip_id
                  and rtn.INVENTORY_ITEM_ID = l_item_id;*/
              EXCEPTION
                WHEN NO_DATA_FOUND THEN
                  l_rtn_qty := 0;
                WHEN OTHERS THEN
                  x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                  x_Error_Msg := '获取退料数量出错：' || SQLERRM;
                  RAISE Fnd_Api.g_Exc_Error;
              END;
            ELSE
              l_rtn_qty := 0;
            end if;
          
          else
            --source type is 2,整单退
            if (l_push_flag = 'Y') then
              begin
                select nvl(sum(nvl(abs(mmt.primary_quantity), 0)), 0)
                  into l_issued_qty
                  from mtl_material_transactions mmt
                 where 1 = 1
                   and mmt.transaction_type_id =
                       (select transaction_type_id
                          from mtl_transaction_types
                         where transaction_type_name = 'WIP 发放')
                   and mmt.primary_quantity < 0
                   and nvl(mmt.attribute9, 'N') = 'N'
                   and mmt.organization_id =
                       p_return_header.organization_id
                   and mmt.inventory_item_id = l_item_id
                   and mmt.Transaction_Source_Id = l_wip_id;
              EXCEPTION
                WHEN OTHERS THEN
                  x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                  x_Error_Msg := '获取整单退料单推式发料数量出错：' || SQLERRM;
                  RAISE Fnd_Api.g_Exc_Error;
              end;
            ELSE
              begin
                select nvl(sum(nvl(abs(mmt.primary_quantity), 0)), 0)
                  into l_issued_qty
                  from mtl_material_transactions mmt
                 where 1 = 1
                   and mmt.transaction_type_id =
                       (select transaction_type_id
                          from mtl_transaction_types
                         where transaction_type_name = '拉式物料发放')
                   and mmt.primary_quantity < 0
                   and nvl(mmt.attribute9, 'N') = 'N'
                   and mmt.organization_id =
                       p_return_header.organization_id
                   and mmt.inventory_item_id = l_item_id
                   and mmt.transfer_locator_id = l_supply_loc_id;
              EXCEPTION
                WHEN OTHERS THEN
                  x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                  x_Error_Msg := '获取整单退料单发料数量出错：' || SQLERRM;
                  RAISE Fnd_Api.g_Exc_Error;
              end;
            end if;
          
            --get rtn qty
            IF (l_push_flag = 'N') THEN
              begin
                select nvl(sum(nvl(cwmr.return_quantity, 0)), 0)
                  into l_rtn_qty
                  from cux_wip_material_return cwmr
                 where cwmr.source_type = 1
                   and cwmr.status = 2
                   and cwmr.wip_entity_id = l_wip_id
                   and cwmr.organization_id =
                       p_return_header.organization_id
                   and cwmr.inventory_item_id = l_item_id;
              EXCEPTION
                WHEN OTHERS THEN
                  x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                  x_Error_Msg := '获取整单退料单退料数量出错：' || SQLERRM;
                  RAISE Fnd_Api.g_Exc_Error;
              end;
            ELSE
              l_rtn_qty := 0; --推式料退了后已扣减ISSUE QTY
            END IF;
          
          end if; --source type
        
          --get req qty 
          IF (l_push_flag = 'N') THEN
          
            BEGIN
            
              select nvl(sum(wro.required_quantity), 0) /*, flvs.ta*/
                into l_req_qty
                from WIP_REQUIREMENT_OPERATIONS wro /*, fnd_lookup_values flvs*/
               where wro.ORGANIZATION_ID = p_return_header.organization_id
                 and wro.WIP_ENTITY_ID = l_wip_id
                 and wro.INVENTORY_ITEM_ID = l_item_id
                 and wro.supply_locator_id = l_supply_loc_id
                    /*  and flvs.lookup_type(+) = 'RJ_GUAGOU'
                    and flvs.language(+) = USERENV('LANG')
                    and flvs.lookup_code(+) = wro.concatenated_segments*/
                 and wro.wip_supply_type in (2, 3);
            EXCEPTION
              WHEN OTHERS THEN
                x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                x_Error_Msg := '获取拉式需求数量出错：' || SQLERRM;
                RAISE Fnd_Api.g_Exc_Error;
            END;
          
          ELSE
            BEGIN
            
              select nvl(sum(wro.required_quantity), 0) --sum(nvl(wro.required_quantity, 0)) /*, flvs.ta*/
                into l_req_qty
                from WIP_REQUIREMENT_OPERATIONS wro /*, fnd_lookup_values flvs*/
               where wro.ORGANIZATION_ID = p_return_header.organization_id
                 and wro.WIP_ENTITY_ID = l_wip_id
                 and wro.INVENTORY_ITEM_ID = l_item_id
                    --and wro.supply_locator_id = l_supply_loc_id
                    /*  and flvs.lookup_type(+) = 'RJ_GUAGOU'
                    and flvs.language(+) = USERENV('LANG')
                    and flvs.lookup_code(+) = wro.concatenated_segments*/
                 and wro.wip_supply_type = 1;
            EXCEPTION
              WHEN OTHERS THEN
                x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                x_Error_Msg := '获取拉式需求数量出错：' || SQLERRM;
                RAISE Fnd_Api.g_Exc_Error;
            END;
          END IF;
        
          l_qty_open := l_issued_qty - l_rtn_qty - l_req_qty;
        
          l_remain_issued_qty := l_issued_qty - l_rtn_qty;
          --get issued qty
        
          if (l_issued_qty <= 0 or
             (l_qty_open <= 0 and ltbl_return_lines(j).quantity = 0)) then
            continue; --发料或者可退数量为0并且没有指定杂入数量的记录不做处理，直接跳到下一行
          end if;
        
          IF (l_push_flag = 'N' and l_source_type = 1 AND ltbl_return_lines(j)
             .quantity > l_qty_open) THEN
            --拉式控制退货数量
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := ltbl_return_lines(j)
                           .item_number || '本次退还数量( ' || ltbl_return_lines(j)
                           .quantity || ' ) 大于
           工单标准剩余量 ( ' || l_qty_open;
          
            RAISE Fnd_Api.g_Exc_Error;
          
          elsif (l_push_flag = 'Y') then
            --推式物料控制退货数量
            /* BEGIN
            
              select nvl(SUM(wro.quantity_issued), 0)
                into l_push_issued_qty
                from wip_requirement_operations wro
               where wro.organization_id = p_return_header.organization_id
                 and wro.wip_entity_id = l_wip_id
                 and wro.inventory_item_id = l_item_id;
            
            EXCEPTION
              WHEN OTHERS THEN
                l_push_issued_qty := 0;
            END;*/
          
            if (l_source_type = 1 and
               l_issued_qty < ltbl_return_lines(j).quantity) then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := ltbl_return_lines(j)
                             .item_number || '本次推式退还数量( ' || ltbl_return_lines(j)
                             .quantity || ' ) 大于
           已发数量 ( ' || l_issued_qty;
            
              RAISE Fnd_Api.g_Exc_Error;
            end if;
          
          else
            null;
          END IF;
        
          begin
            select nvl(sum(RETURN_QUANTITY), 0)
              into l_accumulative_quantity
              from cux_wip_material_return
             where organization_id = p_return_header.organization_id
               and wip_entity_id = l_wip_id
               and inventory_item_id = l_item_id
               and STATUS = 2;
          exception
            when no_data_found then
              l_accumulative_quantity := 0;
          end;
        
          l_line_number := l_line_number + 1;
          begin
            INSERT INTO CUX_WIP_MATERIAL_RETURN
              (ORGANIZATION_ID,
               WIP_ENTITY_ID,
               INVENTORY_ITEM_ID,
               --SKY_HOOKS,
               --ISSUED_DATE,
               ISSUED_QUANTITY,
               REQ_QUANT_DIS,
               QUANTITY_OPEN,
               RETURN_QUANTITY,
               ACCUMULATIVE_QUANTITY,
               RETURN_DATE,
               --RETURN_DIFFERENCE,
               STATUS,
               SELECT_FLAG,
               LAST_UPDATE_DATE,
               LAST_UPDATED_BY,
               CREATION_DATE,
               CREATED_BY,
               LAST_UPDATE_LOGIN,
               SUPPLY_LOCATOR_ID,
               LOCATOR_ID,
               LINE_ID,
               LINE_NUMBER,
               SOURCE_TYPE,
               SUPPLY_SUBINVENTORY,
               SUBINVENTORY,
               TRANSACTION_ID,
               orig_system,
               orig_doc_number,
               PUSH_FLAG,
               process_status)
            VALUES
              (p_return_header.organization_id,
               l_wip_id,
               l_item_id,
               --:CUX_WIP_MATERIAL.SKY_HOOKS,
               --:CUX_WIP_MATERIAL.ISSUED_DATE,
               l_remain_issued_qty, --:CUX_WIP_MATERIAL.ISSUED_QUANTITY,
               l_req_qty, --:CUX_WIP_MATERIAL.REQ_QUANT_DIS,
               l_qty_open, --:CUX_WIP_MATERIAL.QUANTITY_OPEN,
               ltbl_return_lines(j).quantity,
               l_accumulative_quantity, --:CUX_WIP_MATERIAL.ACCUMULATIVE_QUANTITY,
               sysdate, --:CUX_WIP_MATERIAL.RETURN_DATE,
               --:CUX_WIP_MATERIAL.RETURN_DIFFERENCE,
               1, --:CUX_WIP_MATERIAL.STATUS,
               'Y', --:CUX_WIP_MATERIAL.SELECT_FLAG,
               sysdate, --:CUX_WIP_MATERIAL.LAST_UPDATE_DATE,
               -1, --:CUX_WIP_MATERIAL.LAST_UPDATED_BY,
               sysdate, --:CUX_WIP_MATERIAL.CREATION_DATE,
               -1, --:CUX_WIP_MATERIAL.CREATED_BY,
               -1, --:CUX_WIP_MATERIAL.LAST_UPDATE_LOGIN,
               l_supply_loc_id, --:CUX_WIP_MATERIAL.SUPPLY_LOCATOR_ID,
               l_loc_id, --:CUX_WIP_MATERIAL.LOCATOR_ID,
               CUX_WIP_MATERIAL_RETURN_S.NEXTVAL,
               l_line_number,
               l_source_type, --1, --:CUX_WIP_MATERIAL.SOURCE_TYPE,
               l_supply_SUBINV, --:CUX_WIP_MATERIAL.SUPPLY_SUBINVENTORY,
               ltbl_return_lines                (j).SUBINVENTORY, --:CUX_WIP_MATERIAL.SUBINVENTORY,
               l_trx_id, --:CUX_WIP_MATERIAL.TRANSACTION_ID
               p_return_header.source_system,
               p_return_header.Wms_Trx_Number,
               l_push_flag,
               'P'--ADD BY BRUCE ON 20160219 FOR AVOIDING PARALELL PROCESS
               );
          exception
            when others then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := '创建退料单据出错：' || SQLERRM;
              RAISE Fnd_Api.g_Exc_Error;
          end;
        
        end loop;
      end if; --零星拉式
    
      /* begin
        select count(1)
          into l_count_push
          from cux.cux_wip_material_return cmr
         where cmr.transaction_id = l_trx_id
           and cmr.push_flag = 'Y';
      exception
        when others then
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '统计推式料出错：' || SQLERRM;
          RAISE Fnd_Api.g_Exc_Error;
      end;*/
    
      commit;
    
      for rec_return_item in cur_return_item(l_trx_id) loop
        null;
        cux_wip_material_returns_pkg.material_transfer(p_line_id    => rec_return_item.LINE_ID,
                                                       p_user_id    => -1,
                                                       X_RET_STATUS => l_ret_status,
                                                       x_msg_data   => l_msg_data);
      
        if (l_ret_status = 'S') then
          NULL;
        ELSE
          begin
            UPDATE CUX_WIP_MATERIAL_RETURN A
               SET --A.Transfer_Status  = 'POSTED',
                   A.LAST_UPDATE_DATE = SYSDATE,
                   a.attribute14      = l_msg_data,
                   A.LAST_UPDATED_BY  = -1 --,
            
            -- A.SELECT_FLAG = 'N'
             WHERE A.LINE_ID = rec_return_item.LINE_ID;
          exception
            when others then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := '更新错误记录出错：' || SQLERRM;
              RAISE Fnd_Api.g_Exc_Error;
          end;
        
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := x_Error_Msg || '过账拉式退料错误：' || l_msg_data;
          RAISE Fnd_Api.g_Exc_Error;
        end if;
      
      --x_Error_Msg := x_Error_Msg || l_msg_data;
      end loop;
      commit;
    
      --check wip job is closed or not
      begin
      
        select count(1)
          into l_count_released
          from wip_discrete_jobs wdj
         where wdj.wip_entity_id = l_wip_id
           and wdj.status_type = 3;
      
      exception
        when others then
          l_count_released := 0;
      end;
    
      for rec_return_item2 in cur_return_item2(l_trx_id) loop
      
        if (l_count_released = 0) then
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '拉式还/退料结束，推式数据无法还/退料，工单状态不是已发放';
          RAISE Fnd_Api.g_Exc_Error;
        end if;
      
        --SMTN发起的拉式零星退料单中若存在推事供应的物料，则按推式过账
      
        --for rec_item_oper in cur_item_operation(rec_return_item2.inventory_item_id) loop
        if (l_source_type = 1) then
          --推式零退检查数量不能超出完工需要总数量
          BEGIN
          
            select nvl(sum(wro.quantity_issued), 0)
              into l_total_issued_qty
              from wip_requirement_operations wro
             where wro.organization_id = rec_return_item2.organization_id
               and wro.wip_entity_id = rec_return_item2.wip_entity_id
               and wro.inventory_item_id =
                   rec_return_item2.inventory_item_id
            /*and wro.operation_seq_num=rec_item_oper.operation_seq_num*/
            ;
          
          EXCEPTION
            WHEN OTHERS THEN
              l_total_issued_qty := 0;
          END;
        
          --SELECT COUNT(1)
          select nvl(sum(a.wip_com_qty), 0)
          -- INTO l_ishave
            into l_total_com_qty
            FROM (SELECT wo.quantity_completed * wro.quantity_per_assembly /
                         wro.component_yield_factor wip_com_qty /*,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     wro.quantity_issued - nvl(cwm.return_quantity, 0) give_qty*/
                    FROM --cux_ready_item               cri,
                         --cux.cux_wip_material_return  cwm,
                          wip_operations wo,
                         --wip_requirement_operations_v wro
                         wip_requirement_operations wro
                   WHERE 1 = 1
                     AND wo.wip_entity_id = wro.wip_entity_id
                     AND wo.organization_id = wro.organization_id
                     AND nvl(wo.quantity_completed, 0) > 0
                     AND wo.next_operation_seq_num IS NULL
                     AND wro.organization_id =
                         rec_return_item2.organization_id
                        --AND wro.inventory_item_id = cwm.inventory_item_id
                     and wro.inventory_item_id =
                         rec_return_item2.inventory_item_id
                        --and wro.operation_seq_num = wo.operation_seq_num
                        /*AND wro.operation_seq_num =
                        rec_item_oper.operation_seq_num*/
                     AND wro.wip_entity_id = rec_return_item2.wip_entity_id --cwm.wip_entity_id
                  /* AND cwm.organization_id =
                      p_return_header.organization_id
                  AND cwm.transaction_id = l_trx_id
                     \*AND cri.doc_type = '推式退料(零单)'*\
                     --and cwm.source_type = 1
                  and cwm.push_flag = 'Y'*/
                  ) a;
          --WHERE a.give_qty < a.wip_com_qty;
          /* IF l_ishave > 0 THEN
            x_Ret_Sts   := 'E';
            x_Error_Msg := '存在推式料号，检查有组件退料数量大于完工所需最小套数，不能退料。工序：' ||
                           rec_item_oper.operation_seq_num;
            RAISE Fnd_Api.g_Exc_Error;
          END IF;*/
        
          if (l_total_com_qty >
             (l_total_issued_qty - rec_return_item2.return_quantity)) then
            --零退才做此验证
            x_Ret_Sts   := 'E';
            x_Error_Msg := '存在推式料号，检查有组件退料数量大于完工所需最小套数，不能退料。';
            RAISE Fnd_Api.g_Exc_Error;
          end if;
        end if; --source type
      
        --end loop;
      
        begin
        
          SELECT MSIB.PRIMARY_UOM_CODE
            INTO l_uom
            FROM MTL_SYSTEM_ITEMS_B MSIB
           WHERE MSIB.ORGANIZATION_ID = p_return_header.Organization_Id
             AND MSIB.INVENTORY_ITEM_ID =
                 rec_return_item2.inventory_item_id;
        
        exception
          when others then
            x_Ret_Sts   := 'E';
            x_Error_Msg := '获取单位失败：' || SQLERRM;
            RAISE Fnd_Api.g_Exc_Error;
        end;
      
        --
        if (l_source_type = 1) then
          l_remain_rtn_qty := rec_return_item2.return_quantity;
        elsif (l_source_type = 2 and rec_return_item2.return_quantity > 0) then
          l_remain_rtn_qty := rec_return_item2.return_quantity;
        elsif (l_source_type = 2 and rec_return_item2.return_quantity = 0) then
          l_remain_rtn_qty := 0;
        end if;
      
        for rec_item_oper in cur_item_operation(rec_return_item2.inventory_item_id) loop
          /*if (rec_return_item2.quantity_open <= 0) then
            update cux.cux_wip_material_return cmr
               set cmr.attribute14 = '可退数量为0'
             where cmr.line_id = rec_return_item2.line_id;
            --RAISE Fnd_Api.g_Exc_Error;
            exit;
          end if;*/
        
          if (l_remain_rtn_qty = 0 and l_source_type = 1) then
            --推式料 已退完，即使还有工序也退出循环了
            exit;
          end if;
        
          --l_avail_rtn_qty := l_oper_issued_qty - l_oper_need_qty; --可退数量
          if (l_source_type = 1) then
            --零单逻辑
            l_avail_rtn_qty := rec_return_item2.quantity_open;
            --计算每一道工序最终的退货数量
            if (l_remain_rtn_qty > l_avail_rtn_qty) then
              l_oper_rtn_qty   := l_avail_rtn_qty;
              l_remain_rtn_qty := l_remain_rtn_qty - l_oper_rtn_qty;
            else
              l_oper_rtn_qty   := l_remain_rtn_qty;
              l_remain_rtn_qty := 0;
            end if;
          else
            --整单逻辑
            null;
            --计算每一道工序的OPEN QTY可退数量
            begin
              select nvl(wro.quantity_issued, 0) - wro.required_quantity
                into l_oper_rtn_qty
                from wip_requirement_operations wro
               where wro.organization_id = rec_return_item2.organization_id
                 and wro.wip_entity_id = rec_return_item2.wip_entity_id
                 and wro.inventory_item_id =
                     rec_return_item2.inventory_item_id
                 and wro.operation_seq_num =
                     rec_item_oper.operation_seq_num;
            exception
              when others then
                x_Ret_Sts   := 'E';
                x_Error_Msg := '获取工序发料数量X失败：' || SQLERRM;
                RAISE Fnd_Api.g_Exc_Error;
            end;
          
            --l_push_total_rtn_qty := l_push_total_rtn_qty + l_oper_rtn_qty;
            l_remain_rtn_qty := l_remain_rtn_qty - l_oper_rtn_qty;
          end if;
        
          if (l_oper_rtn_qty <= 0) then
            --只会针对到整单还，零退在前面l_remain_rtn_qty = 0的条件已退出
            continue;
          end if;
        
          --推式过账
          --insert interface
          l_iface_rec                     := NULL;
          l_iface_rec.last_update_date    := SYSDATE;
          l_iface_rec.last_updated_by     := l_user_id;
          l_iface_rec.creation_date       := SYSDATE;
          l_iface_rec.created_by          := l_user_id;
          l_iface_rec.last_update_login   := l_login_id;
          l_iface_rec.transaction_type_id := 43; --WIP 退回
          --c1.ready_qty                      := c1.ready_qty;
          l_iface_rec.operation_seq_num     := rec_item_oper.operation_seq_num;
          l_iface_rec.source_code           := '退料单';
          l_iface_rec.transaction_reference := rec_return_item2.source_type ||
                                               '[退料单号:' ||
                                               rec_return_item2.transaction_id || ']';
        
          SELECT mtl_material_transactions_s.nextval
            INTO l_iface_rec.transaction_interface_id
            FROM dual;
          l_iface_rec.transaction_header_id := l_iface_rec.transaction_interface_id;
          l_iface_rec.transaction_mode      := 3;
          l_iface_rec.process_flag          := 1;
          l_iface_rec.inventory_item_id     := rec_return_item2.inventory_item_id;
          l_iface_rec.subinventory_code     := rec_return_item2.subinventory;
          l_iface_rec.locator_id            := rec_return_item2.locator_id;
          l_iface_rec.transaction_quantity  := l_oper_rtn_qty; --rec_return_item2.return_quantity;
          l_iface_rec.primary_quantity      := l_oper_rtn_qty; --rec_return_item2.return_quantity;
          l_iface_rec.transfer_organization := rec_return_item2.organization_id;
          l_iface_rec.organization_id       := rec_return_item2.organization_id;
          l_iface_rec.transaction_uom       := l_uom;
          l_iface_rec.transaction_date      := SYSDATE;
          l_iface_rec.source_header_id      := rec_return_item2.wip_entity_id;
          l_iface_rec.transaction_source_id := rec_return_item2.wip_entity_id;
          l_iface_rec.source_line_id        := rec_return_item2.line_id;
        
          INSERT INTO inv.mtl_transactions_interface VALUES l_iface_rec;
        
          --call api
          fnd_msg_pub.initialize;
          l_return_status := fnd_api.g_ret_sts_success;
          l_return_count  := inv_txn_manager_pub.process_transactions(p_api_version      => 1.0,
                                                                      p_init_msg_list    => fnd_api.g_false,
                                                                      p_commit           => fnd_api.g_false,
                                                                      p_validation_level => fnd_api.g_valid_level_full,
                                                                      x_return_status    => l_return_status,
                                                                      x_msg_count        => x_msg_count,
                                                                      x_msg_data         => x_Error_Msg,
                                                                      x_trans_count      => x_trans_count,
                                                                      p_table            => 1, --1==MTI,2==MMTT
                                                                      p_header_id        => l_iface_rec.transaction_header_id);
          IF l_return_count = -1 OR
             l_return_status <> fnd_api.g_ret_sts_success THEN
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := x_Error_Msg || l_iface_rec.transaction_header_id || ': ';
          
            FOR a_rec IN (SELECT mti.transaction_interface_id,
                                 mti.error_code,
                                 mti.error_explanation
                            FROM mtl_transactions_interface mti
                           WHERE mti.transaction_header_id =
                                 l_iface_rec.transaction_header_id) LOOP
            
              x_Error_Msg := substr(x_Error_Msg || a_rec.error_code || ':' ||
                                    a_rec.error_explanation || chr(10),
                                    1,
                                    255);
            END LOOP;
          
            ROLLBACK;
          
            RETURN;
          ELSE
            null;
          END IF;
          --推式过账
        end loop; --operation loop
      
        --AFTER WIP RETURN
        if (l_remain_rtn_qty > 0 and l_source_type = 2) then
          --经过WIP RETUREN之后如果还有剩余还料数量则杂入。
          --start za ru
          L_IFACE_REC                   := null;
          L_IFACE_REC.LAST_UPDATE_DATE  := SYSDATE;
          L_IFACE_REC.LAST_UPDATED_BY   := L_USER_ID;
          L_IFACE_REC.CREATION_DATE     := SYSDATE;
          L_IFACE_REC.CREATED_BY        := L_USER_ID;
          L_IFACE_REC.LAST_UPDATE_LOGIN := FND_GLOBAL.LOGIN_ID;
        
          V_TRANSACTION_TYPE_ID := 41;
          V_DISPOSITIONS_NAME   := '损耗退库'; -- '拉式物料盘点' ; -- '盘赢调整';  -- 帐户别名
        
          /* V_ADJ_QTY := ABS(NVL(V_RETURN_QUANTITY, 0) -
          NVL(V_QUANTITY_OPEN, 0));*/
          V_ADJ_QTY := l_remain_rtn_qty;
          -- V_ADJ_QTY := NVL(V_RETURN_QUANTITY,0) - ( NVL(V_QUANTITY_OPEN,0) - V_ALL_RETURN_QTY ) ;
        
          begin
            SELECT DISPOSITION_ID
              INTO V_DISPOSITIONS_ID
              FROM MTL_GENERIC_DISPOSITIONS -- 帐户别名 表
             WHERE ENABLED_FLAG = 'Y'
               AND ORGANIZATION_ID = rec_return_item2.organization_id
               AND SEGMENT1 = V_DISPOSITIONS_NAME;
          exception
            when others then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := '获取V_DISPOSITIONS_ID错误：' || SQLERRM;
              RAISE Fnd_Api.g_Exc_Error;
          end;
          SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
            INTO L_IFACE_REC.TRANSACTION_INTERFACE_ID
            FROM DUAL;
        
          L_IFACE_REC.TRANSACTION_INTERFACE_ID   := L_IFACE_REC.TRANSACTION_INTERFACE_ID;
          L_IFACE_REC.TRANSACTION_MODE           := 3; --BACKGROUND
          L_IFACE_REC.PROCESS_FLAG               := 1; --TO BE PROCESSED    
          L_IFACE_REC.ORGANIZATION_ID            := rec_return_item2.organization_id;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; /*ZG_GT_CSN_RLS_TL_P.GET_INV_MATERIAL_ACCID(MTL_ORG_ID)*/
          L_IFACE_REC.INVENTORY_ITEM_ID          := rec_return_item2.inventory_item_id;
          L_IFACE_REC.SUBINVENTORY_CODE          := rec_return_item2.subinventory; --一定要给；如果ITEM限制了子库，那么必须给这个ITEM的子库
          L_IFACE_REC.LOCATOR_ID                 := rec_return_item2.locator_id;
          L_IFACE_REC.TRANSACTION_QUANTITY       := V_ADJ_QTY; ---X.PRIMARY_QUANTITY; ??
          L_IFACE_REC.PRIMARY_QUANTITY           := V_ADJ_QTY; ---X.PRIMARY_QUANTITY; ??
          L_IFACE_REC.TRANSACTION_UOM            := l_uom;
          L_IFACE_REC.TRANSACTION_DATE           := sysdate; --X.TRANSACTION_DATE;
          L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
          L_IFACE_REC.SOURCE_CODE                := '整单退料'; --V_SOURCE_TYPE_NAME;
          L_IFACE_REC.SOURCE_HEADER_ID           := 012345678; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
          L_IFACE_REC.SOURCE_LINE_ID             := rec_return_item2.line_id; --1234567; 2010.9.15 XDL 用于追溯
          L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 6; --帐户别名
          L_IFACE_REC.TRANSACTION_TYPE_ID        := V_TRANSACTION_TYPE_ID; --帐户别名接收
          L_IFACE_REC.TRANSACTION_SOURCE_ID      := V_DISPOSITIONS_ID; -- X.DISPOSITIONS_ID;
          L_IFACE_REC.LAST_UPDATE_DATE           := SYSDATE;
          L_IFACE_REC.LAST_UPDATED_BY            := FND_GLOBAL.USER_ID;
          L_IFACE_REC.CREATION_DATE              := SYSDATE;
          L_IFACE_REC.CREATED_BY                 := FND_GLOBAL.USER_ID;
          L_IFACE_REC.LAST_UPDATE_LOGIN          := FND_GLOBAL.LOGIN_ID;
          L_IFACE_REC.TRANSACTION_HEADER_ID      := L_IFACE_REC.TRANSACTION_INTERFACE_ID;
        
          INSERT INTO INV.MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
        
          FND_MSG_PUB.INITIALIZE;
          l_return_status := FND_API.G_RET_STS_SUCCESS;
          --0 TO INDICATE SUCCESSFUL PROCESSING AND VALUE -1 TO INDICATE FAILURE PROCESSING
          L_RETURN_COUNT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                                     P_INIT_MSG_LIST    => FND_API.G_FALSE,
                                                                     P_COMMIT           => FND_API.G_FALSE,
                                                                     P_VALIDATION_LEVEL => FND_API.G_VALID_LEVEL_FULL,
                                                                     X_RETURN_STATUS    => l_return_status,
                                                                     X_MSG_COUNT        => X_MSG_COUNT,
                                                                     X_MSG_DATA         => x_Error_Msg,
                                                                     X_TRANS_COUNT      => X_TRANS_COUNT,
                                                                     P_TABLE            => 1, --1==MTI,2==MMTT
                                                                     P_HEADER_ID        => L_IFACE_REC.TRANSACTION_HEADER_ID);
        
          IF L_RETURN_COUNT = -1 OR
             l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
            --X_RET_STATUS := 'E';
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := x_Error_Msg || l_iface_rec.transaction_header_id || ': ';
          
            FOR a_rec IN (SELECT mti.transaction_interface_id,
                                 mti.error_code,
                                 mti.error_explanation
                            FROM mtl_transactions_interface mti
                           WHERE mti.transaction_header_id =
                                 l_iface_rec.transaction_header_id) LOOP
            
              x_Error_Msg := substr(x_Error_Msg || a_rec.error_code || ':' ||
                                    a_rec.error_explanation || chr(10),
                                    1,
                                    255);
            
            END LOOP;
          
            ROLLBACK;
            return;
          else
            begin
              UPDATE CUX_WIP_MATERIAL_RETURN A
                 SET A.Transfer_Status  = 'POSTED',
                     A.LAST_UPDATE_DATE = SYSDATE,
                     A.LAST_UPDATED_BY  = -1,
                     A.STATUS           = '2'
              -- A.SELECT_FLAG = 'N'
               WHERE A.LINE_ID = rec_return_item2.LINE_ID
              /*and A.STATUS = '2'*/
              ;
            exception
              when others then
                x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                x_Error_Msg := '更新抛转状态出错2：' || SQLERRM;
                RAISE Fnd_Api.g_Exc_Error;
            end;
          END IF;
        elsif (l_remain_rtn_qty < 0 and l_source_type = 2) then
          V_TRANSACTION_TYPE_ID := 31;
          V_DISPOSITIONS_NAME   := '损耗退库'; --'拉式物料盘点' ; -- '盘亏调整';   -- 帐户别名
        
          /*V_ADJ_QTY := -ABS(NVL(V_RETURN_QUANTITY, 0) -
          NVL(V_QUANTITY_OPEN, 0));*/
          V_ADJ_QTY := l_remain_rtn_qty;
          -- V_ADJ_QTY := NVL(V_RETURN_QUANTITY,0) - ( NVL(V_QUANTITY_OPEN,0) - V_ALL_RETURN_QTY ) ;
          BEGIN
            SELECT DISPOSITION_ID
              INTO V_DISPOSITIONS_ID
              FROM MTL_GENERIC_DISPOSITIONS -- 帐户别名 表
             WHERE ENABLED_FLAG = 'Y'
               AND ORGANIZATION_ID = rec_return_item2.organization_id
               AND SEGMENT1 = V_DISPOSITIONS_NAME;
          exception
            when others then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := '获取V_DISPOSITIONS_ID错误2：' || SQLERRM;
              RAISE Fnd_Api.g_Exc_Error;
          end;
          SELECT MTL_MATERIAL_TRANSACTIONS_S.NEXTVAL
            INTO L_IFACE_REC.TRANSACTION_INTERFACE_ID
            FROM DUAL;
        
          L_IFACE_REC.TRANSACTION_INTERFACE_ID   := L_IFACE_REC.TRANSACTION_INTERFACE_ID;
          L_IFACE_REC.TRANSACTION_MODE           := 3; --BACKGROUND
          L_IFACE_REC.PROCESS_FLAG               := 1; --TO BE PROCESSED    
          L_IFACE_REC.ORGANIZATION_ID            := rec_return_item2.organization_id;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; /*ZG_GT_CSN_RLS_TL_P.GET_INV_MATERIAL_ACCID(MTL_ORG_ID)*/
          L_IFACE_REC.INVENTORY_ITEM_ID          := rec_return_item2.inventory_item_id;
          L_IFACE_REC.SUBINVENTORY_CODE          := rec_return_item2.subinventory; --一定要给；如果ITEM限制了子库，那么必须给这个ITEM的子库
          L_IFACE_REC.LOCATOR_ID                 := rec_return_item2.locator_id;
          L_IFACE_REC.TRANSACTION_QUANTITY       := V_ADJ_QTY; ---X.PRIMARY_QUANTITY; ??
          L_IFACE_REC.PRIMARY_QUANTITY           := V_ADJ_QTY; ---X.PRIMARY_QUANTITY; ??
          L_IFACE_REC.TRANSACTION_UOM            := l_uom;
          L_IFACE_REC.TRANSACTION_DATE           := sysdate; --X.TRANSACTION_DATE;
          L_IFACE_REC.TRANSACTION_SOURCE_NAME    := NULL;
          L_IFACE_REC.DISTRIBUTION_ACCOUNT_ID    := NULL; --A_REC.DISTRIBUTION_ACCOUNT_ID;
          L_IFACE_REC.SOURCE_CODE                := '整单还料'; --V_SOURCE_TYPE_NAME;
          L_IFACE_REC.SOURCE_HEADER_ID           := 012345678; --ANY NUMBER IF TRANSACTION TYPE IS 杂收发/COST UPDATE
          L_IFACE_REC.SOURCE_LINE_ID             := rec_return_item2.line_id; --1234567; 2010.9.15 XDL 用于追溯
          L_IFACE_REC.TRANSACTION_SOURCE_TYPE_ID := 6; --帐户别名
          L_IFACE_REC.TRANSACTION_TYPE_ID        := V_TRANSACTION_TYPE_ID; --帐户别名发放
          L_IFACE_REC.TRANSACTION_SOURCE_ID      := V_DISPOSITIONS_ID; -- X.DISPOSITIONS_ID;
          L_IFACE_REC.LAST_UPDATE_DATE           := SYSDATE;
          L_IFACE_REC.LAST_UPDATED_BY            := FND_GLOBAL.USER_ID;
          L_IFACE_REC.CREATION_DATE              := SYSDATE;
          L_IFACE_REC.CREATED_BY                 := FND_GLOBAL.USER_ID;
          L_IFACE_REC.LAST_UPDATE_LOGIN          := FND_GLOBAL.LOGIN_ID;
          L_IFACE_REC.TRANSACTION_HEADER_ID      := L_IFACE_REC.TRANSACTION_INTERFACE_ID;
        
          INSERT INTO INV.MTL_TRANSACTIONS_INTERFACE VALUES L_IFACE_REC;
        
          FND_MSG_PUB.INITIALIZE;
          L_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
          --0 TO INDICATE SUCCESSFUL PROCESSING AND VALUE -1 TO INDICATE FAILURE PROCESSING
          L_RETURN_COUNT := INV_TXN_MANAGER_PUB.PROCESS_TRANSACTIONS(P_API_VERSION      => 1.0,
                                                                     P_INIT_MSG_LIST    => FND_API.G_FALSE,
                                                                     P_COMMIT           => FND_API.G_FALSE,
                                                                     P_VALIDATION_LEVEL => FND_API.G_VALID_LEVEL_FULL,
                                                                     X_RETURN_STATUS    => L_RETURN_STATUS,
                                                                     X_MSG_COUNT        => X_MSG_COUNT,
                                                                     X_MSG_DATA         => x_Error_Msg,
                                                                     X_TRANS_COUNT      => X_TRANS_COUNT,
                                                                     P_TABLE            => 1, --1==MTI,2==MMTT
                                                                     P_HEADER_ID        => L_IFACE_REC.TRANSACTION_HEADER_ID);
        
          IF L_RETURN_COUNT = -1 OR
             l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
            --X_RET_STATUS := 'E';
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := x_Error_Msg || l_iface_rec.transaction_header_id || ': ';
          
            FOR a_rec IN (SELECT mti.transaction_interface_id,
                                 mti.error_code,
                                 mti.error_explanation
                            FROM mtl_transactions_interface mti
                           WHERE mti.transaction_header_id =
                                 l_iface_rec.transaction_header_id) LOOP
            
              x_Error_Msg := substr(x_Error_Msg || a_rec.error_code || ':' ||
                                    a_rec.error_explanation || chr(10),
                                    1,
                                    255);
            
            END LOOP;
          
            ROLLBACK;
            return;
          else
            begin
              UPDATE CUX_WIP_MATERIAL_RETURN A
                 SET A.Transfer_Status  = 'POSTED',
                     A.LAST_UPDATE_DATE = SYSDATE,
                     A.LAST_UPDATED_BY  = -1,
                     A.STATUS           = '2'
              -- A.SELECT_FLAG = 'N'
               WHERE A.LINE_ID = rec_return_item2.LINE_ID
              /*and A.STATUS = '2'*/
              ;
            exception
              when others then
                x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                x_Error_Msg := '更新抛转状态出错3：' || SQLERRM;
                RAISE Fnd_Api.g_Exc_Error;
            end;
          END IF;
        else
        
          begin
            UPDATE CUX_WIP_MATERIAL_RETURN A
               SET A.Transfer_Status  = 'POSTED',
                   A.LAST_UPDATE_DATE = SYSDATE,
                   A.LAST_UPDATED_BY  = -1,
                   A.STATUS           = '2'
            -- A.SELECT_FLAG = 'N'
             WHERE A.LINE_ID = rec_return_item2.LINE_ID
            /*and A.STATUS = '2'*/
            ;
          exception
            when others then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := '更新抛转状态出错：' || SQLERRM;
              RAISE Fnd_Api.g_Exc_Error;
          end;
        
        end if;
      
        x_Error_Msg := x_Error_Msg || l_msg_data;
      end loop; --push item loop
    
      select count(1)
        into l_count_err
        from cux.cux_wip_material_return cmr
       where cmr.transaction_id = l_trx_id
         and cmr.status = '1';
      if (l_count_err > 0) then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := x_Error_Msg || '-存在未过账行';
      end if;
    elsif (p_return_header.trx_order_type in ('WIP_NEG_PS', 'WIP_FRG_PS')) then
    
      --get doc type
      /*l_doc_type := get_trx_type(p_header_id  => '',
      p_doc_number => p_return_header.trx_order_number,
      p_doc_type   => p_return_header.trx_order_type);*/
    
      begin
        select cri.doc_status, cri.organization_id, cri.doc_type
          into l_doc_status, l_organization_id, l_doc_type
          from cux_ready_item cri
         where cri.doc_no = p_return_header.trx_order_number
           and cri.organization_id = p_return_header.organization_id
           AND ROWNUM = 1
        --and cri.doc_type = l_doc_type
        ;
      exception
        when others then
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '获取单据状态出错：' || SQLERRM;
          RAISE Fnd_Api.g_Exc_Error;
      end;
    
      -- Call the procedure
      if l_doc_status = '已过账' then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '单据已过账,不能重复过账.';
        RAISE Fnd_Api.g_Exc_Error;
      else
        cux_wip_transactions_pkg.process_trx(p_organization_id => l_organization_id,
                                             p_doc_no          => p_return_header.trx_order_number,
                                             p_doc_type        => l_doc_type,
                                             p_return_status   => l_return_status,
                                             p_return_msg      => l_return_msg);
      
        if l_return_status = 'E' then
          x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
          x_Error_Msg := '过账失败：' || l_return_msg;
        else
          begin
            UPDATE CUX_READY_ITEM CRI
               set cri.transfer_status = 'POSTED'
             where cri.doc_no = p_return_header.trx_order_number
               and cri.organization_id = l_organization_id;
          exception
            when others then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := '更新单据状态为过账时出错：' || SQLERRM;
              RAISE Fnd_Api.g_Exc_Error;
          end;
        end if;
      end if;
    else
      --trx type
      null;
    end if; --trx type
    commit;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      ROLLBACK;
      null;
    when others then
      ROLLBACK;
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误2：' || sqlerrm;
  end;

  PROCEDURE validate_return_item(p_return_header Cux_Wms_Material_Trx_Headers%rowtype,
                                 p_return_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                                 p_source_type   number,
                                 x_return_lines  OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                                 x_Ret_Sts       OUT VARCHAR2,
                                 x_Error_Msg     OUT VARCHAR2) is
  
    l_index                  number := 0;
    l_have_issue             number;
    l_wip_id                 number;
    l_real_issue_quantity    number := 0;
    l_mmt_issue_quantity     number := 0;
    l_return_quantity        number := 0;
    l_count_rtn              number;
    l_count_smtn             number;
    l_count_exist            number;
    l_supply_type            number;
    l_return_to_subinventory varchar2(10);
    cursor cur_validate_qty is
      select wdj.wip_entity_id,
             wro.organization_id,
             wro.inventory_item_id,
             msi.segment1 item_number,
             wro.quantity_issued,
             wro.wip_supply_type,
             nvl(wro.required_quantity, 0) required_quantity,
             wro.supply_locator_id,
             nvl(wdj.attribute3, 'NA') ws_code
        from wip_discrete_jobs          wdj,
             wip_requirement_operations wro,
             mtl_system_items_b         msi
       where 1 = 1
         and wdj.organization_id = wro.organization_id
         and wdj.wip_entity_id = wro.wip_entity_id
         and wro.organization_id = msi.organization_id
         and wro.inventory_item_id = msi.inventory_item_id
         and wro.wip_supply_type in (1, 2, 3)
         and wdj.organization_id = p_return_header.organization_id
         and wdj.wip_entity_id = l_wip_id;
  
    /*cursor cur_other_item is
    select wdj.wip_entity_id,
           we.wip_entity_name,
           wro.organization_id,
           wro.inventory_item_id,
           msi.segment1 item_number,
           msi.description item_description,
           wro.quantity_issued,
           wro.required_quantity,
           flvs.tag SKY_HOOKS,
           wro.supply_locator_id,
           mil.subinventory_code,
           mil.segment1 supply_locator_name,
           0 issue_quantity,
           0 surplus_quantity,
           sysdate issue_date,
           '' return_to_subinventory,
           0 return_to_locator_id,
           '' return_locator_name
      from wip_discrete_jobs          wdj,
           wip_entities               we,
           wip_requirement_operations wro,
           mtl_system_items_b         msi,
           mtl_item_locations         mil,
           fnd_lookup_values          flvs
     where 1 = 1
       and wdj.organization_id = we.organization_id(+)
       and wdj.wip_entity_id = we.wip_entity_id(+)
       and wdj.organization_id = wro.organization_id(+)
       and wdj.wip_entity_id = wro.wip_entity_id(+)
       and wro.organization_id = msi.organization_id(+)
       and wro.inventory_item_id = msi.inventory_item_id(+)
       and wro.organization_id = mil.organization_id(+)
       and wro.supply_locator_id = mil.inventory_location_id(+)
       and flvs.lookup_type(+) = 'RJ_GUAGOU'
       and flvs.language(+) = USERENV('LANG')
       and flvs.lookup_code(+) = msi.segment1
       and wro.wip_supply_type in (1, 2, 3)
       and wdj.organization_id = p_return_header.organization_id
       and wdj.wip_entity_id = l_wip_id;*/
    l_num_index   number := 0;
    l_item_id     number;
    l_default_sub varchar2(10);
    l_count_smtn2 number;
  begin
    x_Ret_Sts := 'S';
    /*
    x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := 'XX' ||
                     p_return_header.wip_entity_name;
      RAISE Fnd_Api.g_Exc_Error;*/
  
    begin
    
      select we.wip_entity_id
        into l_wip_id
        from wip_entities we
       where we.wip_entity_name = p_return_header.wip_entity_name
         and we.organization_id = p_return_header.organization_id;
    
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '验证数据时，获取工单ID出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
    --验证是否退过料
    begin
      select count(1)
        into l_count_rtn
        from cux.cux_wip_material_return cmr
       where cmr.wip_entity_id = l_wip_id
         and cmr.source_type = 2;
    exception
      when others then
        l_count_rtn := 1;
    end;
  
    if (l_count_rtn > 0) then
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '工单存在整单退料，不能再退！';
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  
    --验证是否退过料
    begin
      select count(1)
        into l_count_rtn
        from cux.cux_wip_material_return cmr
       where cmr.wip_entity_id = l_wip_id
         and cmr.source_type = 1
         and cmr.status = '1';
    exception
      when others then
        l_count_rtn := 1;
    end;
  
    if (l_count_rtn > 0) then
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '工单存在未过账的零单退料，不能再退！' ||
                     p_return_header.wip_entity_name;
      RAISE Fnd_Api.g_Exc_Error;
    end if;
  
    if (p_source_type = 2) then
    
      delete from cux.cux_validate_rtn_item vr
       where vr.wip_name = p_return_header.wip_entity_name
         and vr.organization_id = p_return_header.organization_id;
    
      l_num_index := p_return_lines.count;
      --insert into tmp table for validation
      FORALL i IN 1 .. l_num_index
        INSERT INTO cux.cux_validate_rtn_item
          (item_number, wip_name, organization_id)
        VALUES
          (p_return_lines(i).item_number,
           p_return_header.wip_entity_name,
           p_return_header.organization_id);
      l_index := 1;
      -- COMMIT;
      for j in 1 .. p_return_lines.count loop
      
        x_return_lines(l_index).item_number := p_return_lines(j).item_number;
        x_return_lines(l_index).subinventory := p_return_lines(j)
                                                .subinventory;
        x_return_lines(l_index).quantity := p_return_lines(j).quantity;
        l_index := l_index + 1;
      end loop;
    
      for rec_validate_qty in cur_validate_qty loop
        begin
          -- mmt 发料
          if (rec_validate_qty.wip_supply_type = 1) then
          
            select nvl(sum(nvl(abs(mmt.primary_quantity), 0)), 0),
                   max(mmt.subinventory_code)
              into l_mmt_issue_quantity, l_return_to_subinventory
              from mtl_material_transactions mmt
             where 1 = 1
               and mmt.transaction_type_id =
                   (select transaction_type_id
                      from mtl_transaction_types
                     where transaction_type_name = 'WIP 发放')
               and mmt.primary_quantity < 0
               and nvl(mmt.attribute9, 'N') = 'N'
               and mmt.organization_id = rec_validate_qty.organization_id
               and mmt.inventory_item_id =
                   rec_validate_qty.inventory_item_id
               and mmt.Transaction_Source_Id = l_wip_id;
          
          else
            select nvl(sum(nvl(abs(mmt.primary_quantity), 0)), 0),
                   max(mmt.subinventory_code)
              into l_mmt_issue_quantity, l_return_to_subinventory
              from mtl_material_transactions mmt
             where 1 = 1
               and mmt.transaction_type_id =
                   (select transaction_type_id
                      from mtl_transaction_types
                     where transaction_type_name = '拉式物料发放')
               and mmt.primary_quantity < 0
               and nvl(mmt.attribute9, 'N') = 'N'
               and mmt.organization_id = rec_validate_qty.organization_id
               and mmt.inventory_item_id =
                   rec_validate_qty.inventory_item_id
               and mmt.transfer_locator_id =
                   rec_validate_qty.supply_locator_id;
          end if;
        
          /* IF (l_return_to_subinventory IS NULL) THEN
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '' || rec_validate_qty.item_number || '找不到发料子库.';
            RAISE Fnd_Api.g_Exc_Error;
          END IF;*/
        
          select count(1)
            into l_count_smtn
            from mtl_secondary_inventories msi
           where msi.organization_id = rec_validate_qty.organization_id
             and msi.secondary_inventory_name = l_return_to_subinventory
             and NVL(msi.attribute6, 'NA') = 'SMTN';
        
          if (l_count_smtn = 0 and l_return_to_subinventory IS not NULL) then
            --l_return_to_subinventory IS NULL 代表替代料或者供应方式修改过导致查不到，直接放到后面验证数量
            continue; --非SMTN子库的推式料不继续处理
          end if;
        
          if (l_return_to_subinventory is null and
             rec_validate_qty.required_quantity > 0) then
            --供应方式修改，料号从未发过料
            --rec_validate_qty.required_quantity > 0排除替代料，替代料放到后面再检查发料数量跟需要数量
            begin
              select csb.sub
                into l_default_sub
                from cux.cux_sub_loc_set_b csb
               where csb.inventory_item_id =
                     rec_validate_qty.inventory_item_id
                 and csb.ws_code = rec_validate_qty.ws_code
                 and csb.organization_id = rec_validate_qty.organization_id;
            exception
              when others then
                x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
                x_Error_Msg := '!' || rec_validate_qty.item_number ||
                               '获取默认子库出错.';
                RAISE Fnd_Api.g_Exc_Error;
            end;
          
            select count(1)
              into l_count_smtn2
              from mtl_secondary_inventories msi
             where msi.organization_id = rec_validate_qty.organization_id
               and msi.secondary_inventory_name = l_default_sub
               and NVL(msi.attribute6, 'NA') = 'SMTN';
          
            if (l_count_smtn2 > 0) then
              x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
              x_Error_Msg := '!' || rec_validate_qty.item_number ||
                             '找不到发料子库.';
              RAISE Fnd_Api.g_Exc_Error;
            else
              --如果是供应方式修改，非109库，直接不处理了
              continue;
            end if;
          
          end if;
        
          select nvl(sum(nvl(cwmr.return_quantity, 0)), 0)
            into l_return_quantity
            from cux_wip_material_return cwmr
           where cwmr.source_type = 1
             and cwmr.status = 2
             and cwmr.wip_entity_id = rec_validate_qty.wip_entity_id
             and cwmr.organization_id = rec_validate_qty.organization_id
             and cwmr.inventory_item_id =
                 rec_validate_qty.inventory_item_id;
        exception
          when others then
            l_mmt_issue_quantity  := 0;
            l_return_quantity     := 0;
            l_real_issue_quantity := 0;
        end;
      
        l_real_issue_quantity := l_mmt_issue_quantity - l_return_quantity;
      
        if l_real_issue_quantity < rec_validate_qty.required_quantity then
        
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '请注意!' || rec_validate_qty.item_number ||
                         '发料不足,不能整单退料.';
          RAISE Fnd_Api.g_Exc_Error;
        else
          if (l_real_issue_quantity = 0 and
             rec_validate_qty.required_quantity = 0) then
            --针对替代料，需求数量为0，则不做还料
            continue;
          end if;
        end if;
      
        --if pass validation ,then insert into tal
        select count(1)
          into l_count_exist
          from cux.cux_validate_rtn_item vr
         where vr.wip_name = p_return_header.wip_entity_name
           and vr.organization_id = p_return_header.organization_id
           and vr.item_number = rec_validate_qty.item_number;
      
        if (l_count_exist = 0) then
          x_return_lines(l_index).item_number := rec_validate_qty
                                                .item_number;
          x_return_lines(l_index).subinventory := l_return_to_subinventory;
          x_return_lines(l_index).quantity := 0;
          l_index := l_index + 1;
        
          INSERT INTO cux.cux_validate_rtn_item
            (item_number, wip_name, organization_id)
          VALUES
            (rec_validate_qty.item_number,
             p_return_header.wip_entity_name,
             p_return_header.organization_id);
        
        end if;
      
      end loop;
    
      /*   for rec_other_item in cur_other_item loop
        null;
      end loop;*/
    else
    
      for x in 1 .. p_return_lines.count loop
      
        begin
          select msib.inventory_item_id
            into l_item_id
            from mtl_system_items_b msib
           where msib.segment1 = p_return_lines(x).item_number
             and msib.organization_id = p_return_header.organization_id;
        exception
          when others then
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '获取料号ID出错：' || SQLERRM;
            RAISE Fnd_Api.g_Exc_Error;
        end;
      
        --get supply type
        begin
          select wro.wip_supply_type
            into l_supply_type
            from wip_requirement_operations wro
           where wro.organization_id = p_return_header.organization_id
             and wro.wip_entity_id = l_wip_id
             and wro.inventory_item_id = l_item_id
             and rownum = 1;
        exception
          when others then
            x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
            x_Error_Msg := '获取供应类型出错：' || SQLERRM;
            RAISE Fnd_Api.g_Exc_Error;
        end;
      
        if (l_supply_type = 1) then
          -------- 查找是否有发过料 --------
          select count(1)
            into l_have_issue
            from WIP_REQUIREMENT_OPERATIONS a, MTL_MATERIAL_TRANSACTIONS b
           where a.organization_id = b.transfer_organization_id
             and a.inventory_item_id = b.inventory_item_id
                --and a.supply_locator_id = b.transfer_locator_id
             and b.transaction_source_id = l_wip_id
             and a.inventory_item_id = l_item_id
             and a.organization_id = p_return_header.organization_id
             and a.wip_entity_id = l_wip_id
             and b.transaction_type_id =
                 (select transaction_type_id
                    from mtl_transaction_types
                   where transaction_type_name = 'WIP 发放') --100 /* --2 */
             and b.primary_quantity < 0;
        else
          -------- 查找是否有发过料 --------
          select count(1)
            into l_have_issue
            from WIP_REQUIREMENT_OPERATIONS a, MTL_MATERIAL_TRANSACTIONS b
           where a.organization_id = b.transfer_organization_id
             and a.inventory_item_id = b.inventory_item_id
             and a.supply_locator_id = b.transfer_locator_id
             and a.inventory_item_id = l_item_id
             and a.organization_id = p_return_header.organization_id
             and a.wip_entity_id = l_wip_id
             and b.transaction_type_id =
                 (select transaction_type_id
                    from mtl_transaction_types
                   where transaction_type_name = '拉式物料发放') --100 /* --2 */
             and b.primary_quantity < 0;
        end if;
      
        if l_have_issue = 0 then
          x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
          x_Error_Msg := '请注意!物料' || p_return_lines(x).item_number ||
                         '没有发料,不能零星退料.';
          RAISE Fnd_Api.g_Exc_Error;
        end if;
      
      end loop;
    
      l_index := 1;
      -- COMMIT;
      for j in 1 .. p_return_lines.count loop
      
        x_return_lines(l_index).item_number := p_return_lines(j).item_number;
        x_return_lines(l_index).subinventory := p_return_lines(j)
                                                .subinventory;
        x_return_lines(l_index).quantity := p_return_lines(j).quantity;
        l_index := l_index + 1;
      end loop;
    
    end if; --source type
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      --ROLLBACK;
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误2：' || sqlerrm;
  end;

  PROCEDURE process_complete_item(p_complete_header Cux_Wms_Material_Trx_Headers%rowtype,
                                  p_complete_lines  IN OUT Cux_Wms_Pub.Material_Trx_Lines_Tab,
                                  x_Ret_Sts         OUT VARCHAR2,
                                  x_Error_Msg       OUT VARCHAR2) is
    l_doc_status      varchar2(30);
    l_doc_type        varchar2(30);
    l_return_status   varchar2(10);
    l_return_msg      varchar2(2000);
    l_organization_id number;
  begin
    x_Ret_Sts := 'S';
    --get doc type
    /*l_doc_type := get_trx_type(p_header_id  => '',
    p_doc_number => p_complete_header.trx_order_number,
    p_doc_type   => p_complete_header.trx_order_type);*/
  
    begin
      select cri.doc_status, cri.organization_id, cri.doc_type
        into l_doc_status, l_organization_id, l_doc_type
        from cux_ready_item cri
       where cri.doc_no = p_complete_header.trx_order_number
         and cri.organization_id = p_complete_header.organization_id
         AND ROWNUM = 1
      --and cri.doc_type = l_doc_type
      ;
    exception
      when others then
        x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
        x_Error_Msg := '获取单据状态出错：' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    end;
    -- 判断是否已经过账过
    if l_doc_status = '已过账' then
    
      x_Ret_Sts   := fnd_api.G_RET_STS_ERROR;
      x_Error_Msg := '单据已过账,不能重复过账.';
      RAISE Fnd_Api.g_Exc_Error;
    
    end if;
  
    Cux_Wip_Trans_Pkg.Post_Complete_Doc(p_organization_id => l_organization_id,
                                        p_doc_no          => p_complete_header.trx_order_number,
                                        p_doc_type        => l_doc_type,
                                        p_return_status   => l_return_status,
                                        p_return_msg      => l_return_msg);
  
    if l_return_status = 'S' OR l_return_msg = '过账成功' then
      cux_mes_pkg.post_completion_2_mes(x_ret_status    => l_return_status,
                                        x_error_msg     => l_return_msg,
                                        p_completion_id => p_complete_lines(1)
                                                           .line_id);
      if l_return_status <> 'S' then
        x_Error_Msg := '回传MES工单入库信息失败：' || chr(10) || l_return_msg;
      end if;
    end if;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE combine_rcv_to_smtn(p_header_id number,
                                x_Ret_Sts   OUT VARCHAR2,
                                x_Error_Msg OUT VARCHAR2) is
    cursor cur_rcv_line is
      select crl.receipt_line_id, crl.item_id
        from cux_receipts_lines crl
       where crl.receipt_id = p_header_id;
    l_count           number;
    l_vir_line_number number;
    l_default_subinv  varchar2(100);
    --l_default_locator_id number;
    l_total_qty       number;
    l_virtual_line_id number;
  begin
    x_Ret_Sts := 'S';
    l_count   := 0;
    for rec_rcv_line in cur_rcv_line loop
    
      select count(1)
        into l_count
        from cux_receipts_lines crl
       where crl.receipt_id = p_header_id
         and crl.item_id = rec_rcv_line.item_id
         and crl.receipt_line_id <> rec_rcv_line.receipt_line_id
         and crl.combine_tag = 'O' /*in ('C', 'O')*/
      /*and not exists (select 1
       from cux_receipts_lines crl2
      where crl2.receipt_id = p_header_id
        and crl2.item_id = rec_rcv_line.item_id
        and crl2.combine_tag = 'V')*/
      ; --V is virtual line
    
      if (l_count > 0) then
        --same item has more lines
        BEGIN
          select sum(crl.receipt_qty),
                 max(crl.line_number) + 10000,
                 max(crl.default_subinv)
          
            into l_total_qty, l_vir_line_number, l_default_subinv
            from cux_receipts_lines crl
           where crl.receipt_id = p_header_id
             and crl.item_id = rec_rcv_line.item_id
             and crl.combine_tag = 'O';
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
            x_Error_Msg := '获取行信息错误：' || sqlerrm;
            RAISE Fnd_Api.g_Exc_Error;
        END;
        select cux_receipts_lines_s.nextval
          into l_virtual_line_id
          from dual;
        BEGIN
          INSERT INTO CUX_RECEIPTS_LINES
            (receipt_id,
             line_number,
             receipt_line_id,
             item_id,
             RECEIPT_QTY,
             Default_Subinv,
             combine_tag)
          values
            (p_header_id,
             l_vir_line_number,
             l_virtual_line_id,
             rec_rcv_line.item_id,
             l_total_qty,
             l_default_subinv,
             'V');
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
            x_Error_Msg := '插入虚拟行信息错误：' || sqlerrm;
            RAISE Fnd_Api.g_Exc_Error;
        END;
        begin
          UPDATE CUX_RECEIPTS_LINES CRL
             SET CRL.COMBINE_TO_LINE_ID = l_virtual_line_id,
                 crl.combine_tag        = 'C'
           where crl.receipt_id = p_header_id
             and crl.item_id = rec_rcv_line.item_id
             and crl.combine_tag = 'O';
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
            x_Error_Msg := '更新合并行信息错误：' || sqlerrm;
            RAISE Fnd_Api.g_Exc_Error;
        END;
      end if;
    
    end loop;
  
    --call interface to transfer smtn
  
    commit;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;

  PROCEDURE combine_rtn_to_smtn(p_header_id number,
                                x_Ret_Sts   OUT VARCHAR2,
                                x_Error_Msg OUT VARCHAR2) is
    cursor cur_rtn_line is
      select crl.return_line_id, crl.item_id
        from cux_return_lines crl
       where crl.return_id = p_header_id;
    l_count           number;
    l_vir_line_number number;
    l_default_subinv  varchar2(100);
    --l_default_locator_id number;
    l_total_qty       number;
    l_virtual_line_id number;
  begin
    x_Ret_Sts := 'S';
    l_count   := 0;
    for rec_rtn_line in cur_rtn_line loop
    
      select count(1)
        into l_count
        from cux_return_lines crl
       where crl.return_id = p_header_id
         and crl.item_id = rec_rtn_line.item_id
         and crl.return_line_id <> rec_rtn_line.return_line_id
         and crl.combine_tag = 'O' /*in ('C', 'O')*/
      /*and not exists (select 1
       from cux_receipts_lines crl2
      where crl2.receipt_id = p_header_id
        and crl2.item_id = rec_rcv_line.item_id
        and crl2.combine_tag = 'V')*/
      ; --V is virtual line
    
      if (l_count > 0) then
        --same item has more lines
        begin
          select sum(crl.return_qty),
                 max(crl.line_number) + 10000,
                 max(crl.return_subinv)
            into l_total_qty, l_vir_line_number, l_default_subinv
            from cux_return_lines crl
           where crl.return_id = p_header_id
             and crl.item_id = rec_rtn_line.item_id
             and crl.combine_tag = 'O';
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
            x_Error_Msg := '获取行信息错误：' || sqlerrm;
            RAISE Fnd_Api.g_Exc_Error;
        END;
      
        select cux_return_lines_s.nextval into l_virtual_line_id from dual;
      
        BEGIN
        
          INSERT INTO cux_return_lines --CUX_RECEIPTS_LINES
            (return_id,
             line_number,
             return_line_id,
             item_id,
             return_qty,
             return_subinv,
             combine_tag)
          values
            (p_header_id,
             l_vir_line_number,
             l_virtual_line_id,
             rec_rtn_line.item_id,
             l_total_qty,
             l_default_subinv,
             'V');
        
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
            x_Error_Msg := '插入虚拟行错误：' || sqlerrm;
            RAISE Fnd_Api.g_Exc_Error;
        END;
      
        begin
          UPDATE cux_return_lines CRL
             SET CRL.COMBINE_TO_LINE_ID = l_virtual_line_id,
                 crl.combine_tag        = 'C'
           where crl.return_id = p_header_id
             and crl.item_id = rec_rtn_line.item_id
             and crl.combine_tag = 'O';
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
            x_Error_Msg := '更新合并行错误：' || sqlerrm;
            RAISE Fnd_Api.g_Exc_Error;
        END;
      end if;
    
    end loop;
  
    --call interface to transfer smtn
  
    commit;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  PROCEDURE delete_reservation(p_header_id number,
                               x_Ret_Sts   OUT VARCHAR2,
                               x_Error_Msg OUT VARCHAR2) is
  
    l_Rec       Inv_Reservation_Global.Mtl_Reservation_Rec_Type;
    l_Dummy_Sn  Inv_Reservation_Global.Serial_Number_Tbl_Type;
    x_Msg_Count NUMBER;
    x_Msg_Data  VARCHAR2(1000);
    x_Status    VARCHAR2(1) := fnd_api.g_ret_sts_success;
  begin
    FOR rec_reservation IN (SELECT cil.Reservation_Id
                              FROM cux.cux_inv_material_lines cil,
                                   mtl_reservations           mr
                             WHERE cil.header_id = p_header_id
                               and cil.reservation_id = mr.reservation_id) LOOP
    
      l_Rec.Reservation_Id := rec_reservation.Reservation_Id; --922676;--922673;--922670;--1422198;
      Inv_Reservation_Pub.Delete_Reservation(p_Api_Version_Number => 1.0,
                                             x_Return_Status      => x_Status,
                                             x_Msg_Count          => x_Msg_Count,
                                             x_Msg_Data           => x_Msg_Data,
                                             p_Rsv_Rec            => l_Rec,
                                             p_Serial_Number      => l_Dummy_Sn);
    END LOOP;
  
    if (x_status <> fnd_api.g_ret_sts_success) then
      x_Error_Msg := x_Msg_Data;
    end if;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      null;
    when others then
      x_Ret_Sts   := Fnd_Api.G_RET_STS_ERROR;
      x_Error_Msg := '其他错误：' || sqlerrm;
  end;
  
  FUNCTION is_inv_wms_sub(p_header_id       number,
                      p_organization_id number,
                      p_doc_number      varchar2,
                      p_trx_type        varchar2,
                      x_Ret_Sts         OUT VARCHAR2,
                      x_Error_Msg       OUT VARCHAR2) RETURN VARCHAR2 IS
    l_inv_sys       varchar2(10);
    l_count         number;
    l_excp_doc_type varchar2(100);
  BEGIN
    l_count   := 0;
    x_Ret_Sts := 'S';
    
    if p_trx_type IN ('WIP_FUL_PL',       -- 拉式退料（整单）
                      'WIP_FRG_PL') then  -- 拉式退料（零星）
                      
    
      select count(1)
        into l_count
        from cux.cux_wip_material_return cmr, mtl_secondary_inventories msi
       where cmr.transaction_id = p_header_id
         and cmr.select_flag = 'Y'
         and cmr.subinventory = msi.secondary_inventory_name
         and msi.organization_id = cmr.organization_id
         and msi.attribute6 in ('SMTN', 'WMS');
         
    elsif p_trx_type in ('MISC_RCV',      -- 杂入单
                         'MISC_ISSUE',    -- 杂出单
                         'L_TRANS',       -- 本地调拨单
                         'M_TRANS') then  -- 异地调拨单
    
      select count(1)
        into l_count
        from cux.cux_inv_material_lines cim, mtl_secondary_inventories msi
       where cim.header_id = p_header_id
         and msi.secondary_inventory_name in
             (cim.subinventory_code, cim.to_subinventory_code)
         and msi.organization_id = cim.organization_id
         and msi.attribute6 in ('SMTN', 'WMS');
    
    elsif p_trx_type = 'ORG_TRANS' then  -- 组织间调拨单
    
      select count(1)
        into l_count
        from cux.cux_inv_material_lines cim, mtl_secondary_inventories msi
       where cim.header_id = p_header_id
         and msi.secondary_inventory_name = cim.subinventory_code
         and msi.organization_id = cim.organization_id
         and msi.attribute6 in ('SMTN', 'WMS');
    
      if (l_count = 0) then
        select count(1)
          into l_count
          from cux.cux_inv_material_lines cim,
               mtl_secondary_inventories  msi
         where cim.header_id = p_header_id
           and msi.secondary_inventory_name = cim.to_subinventory_code
           and msi.organization_id = cim.to_organization_id
           and msi.attribute6 in ('SMTN', 'WMS');
      end if;
    
    elsif p_trx_type in ('WIP_RLS_PS',  -- 推式备料
                         'WIP_RLS_PL',  -- 拉式备料
                         'WIP_RPL_PS',  -- 推式补料
                         'WIP_RPL_PL',  -- 拉式补料
                         'WIP_OVR_PS',  -- 推式超领
                         'WIP_OVR_PL',  -- 拉式超领
                         'WIP_NEG_PS',  -- 推式退料(负单)
                         'WIP_FRG_PS',  -- 推式退料(零单)
                         'WIP_RTN_PL',  -- 拉式退料
                         'WIP_CMPL') then -- 完工入库
        -- 判断是否是SMTN OR WMS 仓库
        select count(1)
          into l_count
          from cux.cux_ready_item cri, mtl_secondary_inventories msi
         where cri.doc_no = p_doc_number
           and cri.organization_id = p_organization_id
           and cri.supply_subinventory = msi.secondary_inventory_name
           and msi.organization_id = cri.organization_id
           and msi.attribute6 in ('SMTN', 'WMS');
    
    else
      select cri.doc_type
        into l_excp_doc_type
        from cux.cux_ready_item cri
       where cri.doc_no = p_doc_number
         and cri.organization_id = p_organization_id
         and rownum = 1;
      if (l_excp_doc_type = '拉式退料') then
        null;
      else
        x_Ret_Sts   := 'E';
        x_Error_Msg := '单据类型异常';
        raise Fnd_Api.g_Exc_Error;
      end if;
    end if;
  
    IF (l_count > 0) THEN
    
      l_inv_sys := 'Y';
    else
      l_inv_sys := 'N';
    
    END IF;
    RETURN l_inv_sys;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      l_inv_sys := 'N';
      RETURN l_inv_sys;
    when others then
      l_inv_sys := 'N';
      RETURN l_inv_sys;
  END;
  
  FUNCTION is_inv_sys(p_header_id       number,
                      p_organization_id number,
                      p_doc_number      varchar2,
                      p_trx_type        varchar2,
                      x_Ret_Sts         OUT VARCHAR2,
                      x_Error_Msg       OUT VARCHAR2) RETURN VARCHAR2 IS
    l_inv_sys       varchar2(10);
    l_count         number;
    l_excp_doc_type varchar2(100);
  BEGIN
    l_count   := 0;
    x_Ret_Sts := 'S';
    
    if p_trx_type IN ('WIP_FUL_PL',       -- 拉式退料（整单）
                      'WIP_FRG_PL') then  -- 拉式退料（零星）
                      
    
      select count(1)
        into l_count
        from cux.cux_wip_material_return cmr, mtl_secondary_inventories msi
       where cmr.transaction_id = p_header_id
         and cmr.select_flag = 'Y'
         and cmr.subinventory = msi.secondary_inventory_name
         and msi.organization_id = cmr.organization_id
         and msi.attribute6 in ('SMTN', 'WMS');
         
    elsif p_trx_type in ('MISC_RCV',      -- 杂入单
                         'MISC_ISSUE',    -- 杂出单
                         'L_TRANS',       -- 本地调拨单
                         'M_TRANS') then  -- 异地调拨单
    
      select count(1)
        into l_count
        from cux.cux_inv_material_lines cim, mtl_secondary_inventories msi
       where cim.header_id = p_header_id
         and msi.secondary_inventory_name in
             (cim.subinventory_code, cim.to_subinventory_code)
         and msi.organization_id = cim.organization_id
         and msi.attribute6 in ('SMTN', 'WMS');
    
    elsif p_trx_type = 'ORG_TRANS' then  -- 组织间调拨单
    
      select count(1)
        into l_count
        from cux.cux_inv_material_lines cim, mtl_secondary_inventories msi
       where cim.header_id = p_header_id
         and msi.secondary_inventory_name = cim.subinventory_code
         and msi.organization_id = cim.organization_id
         and msi.attribute6 in ('SMTN', 'WMS');
    
      if (l_count = 0) then
        select count(1)
          into l_count
          from cux.cux_inv_material_lines cim,
               mtl_secondary_inventories  msi
         where cim.header_id = p_header_id
           and msi.secondary_inventory_name = cim.to_subinventory_code
           and msi.organization_id = cim.to_organization_id
           and msi.attribute6 in ('SMTN', 'WMS');
      end if;
    
    elsif p_trx_type in ('WIP_RLS_PS',  -- 推式备料
                         'WIP_RLS_PL',  -- 拉式备料
                         'WIP_RPL_PS',  -- 推式补料
                         'WIP_RPL_PL',  -- 拉式补料
                         'WIP_OVR_PS',  -- 推式超领
                         'WIP_OVR_PL',  -- 拉式超领
                         'WIP_NEG_PS',  -- 推式退料(负单)
                         'WIP_FRG_PS',  -- 推式退料(零单)
                         'WIP_RTN_PL',  -- 拉式退料
                         'WIP_CMPL') then -- 完工入库
        -- 判断是否是SMTN OR WMS 仓库
        select count(1)
          into l_count
          from cux.cux_ready_item cri, mtl_secondary_inventories msi
         where cri.doc_no = p_doc_number
           and cri.organization_id = p_organization_id
           and cri.supply_subinventory = msi.secondary_inventory_name
           and msi.organization_id = cri.organization_id
           and msi.attribute6 in ('SMTN', 'WMS');
    
    else
      select cri.doc_type
        into l_excp_doc_type
        from cux.cux_ready_item cri
       where cri.doc_no = p_doc_number
         and cri.organization_id = p_organization_id
         and rownum = 1;
      if (l_excp_doc_type = '拉式退料') then
        null;
      else
        x_Ret_Sts   := 'E';
        x_Error_Msg := '单据类型异常';
        raise Fnd_Api.g_Exc_Error;
      end if;
    end if;
  
    IF (l_count > 0) THEN
    
      l_inv_sys := 'Y';
    else
      l_inv_sys := 'N';
    
    END IF;
    RETURN l_inv_sys;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      l_inv_sys := 'N';
      RETURN l_inv_sys;
    when others then
      l_inv_sys := 'N';
      RETURN l_inv_sys;
  END;
  
  FUNCTION get_trx_type(p_header_id  number,
                        p_doc_number varchar2,
                        p_doc_type   varchar2) RETURN VARCHAR2 is
    l_doc_type varchar2(100);
  begin
    --get doc type
    begin
      select flv.meaning
        into l_doc_type
        from fnd_lookup_values flv
       where flv.lookup_type = 'CUX_INV_TRX_TYPE'
         and flv.language = userenv('lang')
         and flv.lookup_code = p_doc_type;
    exception
      when others then
        l_doc_type := '';
        RAISE Fnd_Api.g_Exc_Error;
    end;
  
    return l_doc_type;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      return '';
    when others then
      return '';
  end;

  PROCEDURE transfer_doc_by_batch(x_errbuf       OUT NOCOPY VARCHAR2,
                                  x_retcode      OUT NOCOPY NUMBER,
                                  p_batch_number varchar2) is
  
    /*cursor cur_doc_in_comb is
      select distinct cri.doc_no,
                      cri.wip_entity_id,
                      cri.wip_entity_name,
                      cri.operation_code,
                      cri.supply_subinventory
        from cux_ready_item                  cri,
             cux.cux_wip_combinations        cwc,
             cux.cux_wip_combination_details cwd,
             mtl_secondary_inventories       msi
       where cwc.combination_id = p_comb_id
         and cwc.combination_id = cwd.combination_id
         and cwd.wip_entity_id = cri.wip_entity_id
         and cri.transfer_status = 'WAITING'
         and cri.organization_id = p_organization_id
         and cri.wip_entity_id = nvl(p_wip_id, cri.wip_entity_id)
         and cri.doc_type in
             ('推式备料', '拉式备料', '推式补料', '拉式补料')
         and cri.doc_type = nvl(p_doc_type, cri.doc_type)
         and cri.doc_no = nvl(p_doc_number, cri.doc_no)
         and cri.supply_subinventory = msi.secondary_inventory_name
         and cri.organization_id = msi.organization_id
         and msi.attribute6 in ('WMS', 'SMTN')
       order by cri.operation_code, cri.doc_type;
    cursor cur_doc is
      select distinct cri.doc_no,
                      cri.wip_entity_id,
                      cri.wip_entity_name,
                      cri.operation_code,
                      cri.supply_subinventory
        from cux_ready_item cri, mtl_secondary_inventories msi
       where cri.wip_entity_id = p_wip_id
         and cri.transfer_status = 'WAITING'
         and cri.organization_id = p_organization_id
         and cri.doc_type in
             ('推式备料', '拉式备料', '推式补料', '拉式补料')
         and cri.doc_type = nvl(p_doc_type, cri.doc_type)
         and cri.doc_no = nvl(p_doc_number, cri.doc_no)
         and cri.supply_subinventory = msi.secondary_inventory_name
         and cri.organization_id = msi.organization_id
         and msi.attribute6 in ('WMS', 'SMTN')
       order by cri.operation_code, cri.doc_type;*/
    l_Ret_Sts   varchar2(10);
    l_Error_Msg varchar2(1000);
    --l_count       number := 0;
    --l_count_qitao number := 0;
    --l_comb_number number;
    --l_oper_code   varchar2(100) := 'NA';
    l_count_error number := 0;
    cursor cur_batch_doc is
      select *
        from cux.cux_wip_batch_transfer_doc cbd
       where cbd.batch_number = p_batch_number
         and cbd.process_code = 'P'
       order by cbd.doc_type;
  
  begin
  
    --transfer by comb 
  
    for rec_batch_doc in cur_batch_doc loop
      fnd_file.PUT_LINE(fnd_file.LOG, rec_batch_doc.doc_no);
      fnd_file.PUT_LINE(fnd_file.LOG, rec_batch_doc.Transfer_Id);
      Cux_Wip2wms_Transfer_Pkg.Transfer_Wip_Issue_Data(x_Ret_Sts         => l_Ret_Sts,
                                                       x_Error_Msg       => l_Error_Msg,
                                                       p_Organization_Id => rec_batch_doc.organization_id,
                                                       p_Doc_No          => rec_batch_doc.doc_no,
                                                       p_Full_Flag       => rec_batch_doc.mes_zd_flag);
    
      fnd_file.PUT_LINE(fnd_file.LOG, 'l_Ret_Sts:' || l_Ret_Sts);
      if (l_Ret_Sts = fnd_api.G_RET_STS_SUCCESS) then
        fnd_file.PUT_LINE(fnd_file.LOG,
                          'UPDATE S:' || rec_batch_doc.Transfer_Id);
        fnd_file.PUT_LINE(fnd_file.OUTPUT,
                          rec_batch_doc.doc_no || '抛转成功');
        update cux.cux_wip_batch_transfer_doc cbd
           set cbd.process_code = 'S'
         where cbd.transfer_id = rec_batch_doc.transfer_id;
      else
        l_count_error := l_count_error + 1;
        fnd_file.PUT_LINE(fnd_file.LOG,
                          'UPDATE E:' || rec_batch_doc.Transfer_Id);
        fnd_file.PUT_LINE(fnd_file.OUTPUT,
                          rec_batch_doc.doc_no || '抛转失败：' || l_Error_Msg);
        update cux.cux_wip_batch_transfer_doc cbd
           set cbd.process_code = 'E', cbd.error_msg = l_Error_Msg
         where cbd.transfer_id = rec_batch_doc.transfer_id;
      end if;
    
    end loop;
    commit;
    if (l_count_error > 0) then
      x_retcode := 1;
    end if;
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      --ROLLBACK;
      x_retcode := 1;
    when others then
      x_retcode := 2;
      fnd_file.PUT_LINE(fnd_file.LOG,
                        '其他错误transfer_doc_by_batch：' || sqlerrm);
  end;
END CUX_EBS_SMTN_INTEGRATION_PKG;
/
