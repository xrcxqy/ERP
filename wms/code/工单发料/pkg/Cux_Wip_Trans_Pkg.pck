CREATE OR REPLACE PACKAGE Cux_Wip_Trans_Pkg AS

       
  TYPE CUX_DOC_CLASS_TYPE IS RECORD(       
                            EMPTY_RETURN_FLAG  VARCHAR2(1)
	);  
     
  /*===============================================================
    *    Program Name:   create_preparation_doc
    *    Author      :   LGF
    *    Date        :   2015-07-11
    *    Purpose     :   创建备料单
    *    Parameters  :
    *             in      p_organization_id      组织ID
    *             In       p_wip_name            工单name
                  in       p_wip_type            单据类型
    *             In       p_doc_date            日期
                  in       p_now_qty             下达数量
                  in       p_sub                 子库
                  in       p_loc                 货位
                  out      p_return_status       返回(S/E)
                  out      p_error_mess          返回信息
    *    Update History
    *    Version    Date         Name            Description
    *    --------  ----------  ---------------  --------------------
    *     V1.0     2014-08-11   LGF          Creation
    *
  ===============================================================*/
  PROCEDURE Create_Preparation_Doc(p_Organization_Id NUMBER,
                                   p_wip_id          number,
                                   p_Wip_Name        VARCHAR2,
                                   p_Wip_Type        VARCHAR2,
                                   p_Doc_Date        DATE,
                                   p_Now_Qty         NUMBER,
                                   p_Sub             VARCHAR2,
                                   p_Loc             VARCHAR2,
                                   p_Operation_Code  VARCHAR2,
                                   p_param           CUX_DOC_CLASS_TYPE,
                                   p_Return_Status   OUT VARCHAR2,
                                   p_Return_Msg      OUT VARCHAR2);

  /*===============================================================
    *    Program Name:   create_preparation_doc
    *    Author      :   LGF
    *    Date        :   2015-07-11
    *    Purpose     :   创建备料单( 指定仓库下达 )
    *    Parameters  :
    *             in      p_organization_id      组织ID
    *             In       p_wip_name            工单name
                  in       p_wip_type            单据类型
    *             In       p_doc_date            日期
                  in       p_now_qty             下达数量
                  in       p_sub                 子库
                  in       p_loc                 货位
                  out      p_return_status       返回(S/E)
                  out      p_error_mess          返回信息
    *    Update History
    *    Version    Date         Name            Description
    *    --------  ----------  ---------------  --------------------
    *     V1.0     2014-08-11   LGF          Creation
    *
  ===============================================================*/
  PROCEDURE Create_Preparation_Doc_Ap_Sub(p_Organization_Id NUMBER,
                                          p_wip_id          number,
                                          p_Wip_Name        VARCHAR2,
                                          p_Wip_Type        VARCHAR2,
                                          p_Doc_Date        DATE,
                                          p_Now_Qty         NUMBER,
                                          p_Sub             VARCHAR2,
                                          p_Loc             VARCHAR2,
                                          p_Operation_Code  VARCHAR2,
                                          p_param           CUX_DOC_CLASS_TYPE,
                                          p_Return_Status   OUT VARCHAR2,
                                          p_Return_Msg      OUT VARCHAR2);

  /*===============================================================
    *    Program Name:   Post_Preparation_Doc
    *    Author      :   LGF
    *    Date        :   2015-07-11
    *    Purpose     :   过账单
    *    Parameters  :
    *             in      p_organization_id      组织ID
    *             In      p_doc_no               工单名
                  in      p_doc_type             单据类型
                  out      p_return_status       返回(S/E)
                  out      p_error_mess          返回信息
    *    Update History
    *    Version    Date         Name            Description
    *    --------  ----------  ---------------  --------------------
    *     V1.0     2014-08-11   LGF          Creation
    *
  ===============================================================*/
  PROCEDURE Post_Preparation_Doc(p_Organization_Id NUMBER,
                                 p_doc_no          VARCHAR2,
                                 p_doc_type        VARCHAR2,
                                 p_return_status   OUT VARCHAR2,
                                 p_return_msg      OUT VARCHAR2);

  /*===============================================================
    *    Program Name:   create_feeding_doc
    *    Author      :   LGF
    *    Date        :   2015-07-11
    *    Purpose     :   创建补料单
    *    Parameters  :
    *             in      p_organization_id      组织ID
    *             In       p_wip_name            工单name
    *             in       p_wip_type            单据类型
    *             In       p_doc_date            日期
    *             in       p_now_qty             下达数量
                  in       p_sub                 子库
                  in       p_loc                 货位
                  out      p_return_status       返回(S/E)
                  out      p_error_mess          返回信息
    *    Update History
    *    Version    Date         Name            Description
    *    --------  ----------  ---------------  --------------------
    *     V1.0     2014-08-11   LGF          Creation
    *
  ===============================================================*/
  PROCEDURE Create_Feeding_Doc(p_Organization_Id NUMBER,
                               p_wip_id          number,
                               p_Wip_Name        VARCHAR2,
                               p_Wip_Type        VARCHAR2,
                               p_Doc_Date        DATE,
                               p_Now_Qty         NUMBER,
                               p_Sub             VARCHAR2,
                               p_Loc             VARCHAR2,
                               p_Operation_Code  VARCHAR2,
                               p_param           CUX_DOC_CLASS_TYPE,
                               p_Return_Status   OUT VARCHAR2,
                               p_Return_Msg      OUT VARCHAR2);

  /*===============================================================
    *    Program Name:   create_feeding_doc
    *    Author      :   LGF
    *    Date        :   2015-07-11
    *    Purpose     :   创建补料单( 指定仓库 )
    *    Parameters  :
    *             in      p_organization_id      组织ID
    *             In       p_wip_name            工单name
    *             in       p_wip_type            单据类型
    *             In       p_doc_date            日期
    *             in       p_now_qty             下达数量
                  in       p_sub                 子库
                  in       p_loc                 货位
                  out      p_return_status       返回(S/E)
                  out      p_error_mess          返回信息
    *    Update History
    *    Version    Date         Name            Description
    *    --------  ----------  ---------------  --------------------
    *     V1.0     2014-08-11   LGF          Creation
    *
  ===============================================================*/
  PROCEDURE Create_Feeding_Doc_Ap_Sub(p_Organization_Id NUMBER,
                                      p_wip_id          number,
                                      p_Wip_Name        VARCHAR2,
                                      p_Wip_Type        VARCHAR2,
                                      p_Doc_Date        DATE,
                                      p_Now_Qty         NUMBER,
                                      p_Sub             VARCHAR2,
                                      p_Loc             VARCHAR2,
                                      p_Operation_Code  VARCHAR2,
                                      p_param           CUX_DOC_CLASS_TYPE,
                                      p_Return_Status   OUT VARCHAR2,
                                      p_Return_Msg      OUT VARCHAR2);

  /*===============================================================
    *    Program Name:   Create_Return_Doc
    *    Author      :   LGF
    *    Date        :   2015-08-20
    *    Purpose     :   创建退料单
    *    Parameters  :
    *             in       p_organization_id      组织ID
    *             In       p_wip_name            工单name
    *             in       p_wip_type            单据类型
    *             In       p_doc_date            日期
    *             in       p_now_qty             下达数量
                  in       p_sub                 子库
                  in       p_loc                 货位
                  out      p_return_status       返回(S/E)
                  out      p_error_mess          返回信息
    *    Update History
    *    Version    Date         Name            Description
    *    --------  ----------  ---------------  --------------------
    *     V1.1     2015-07-30   Felix.Liu          Creation
    *
  ===============================================================*/
  PROCEDURE Create_Return_Doc(p_Organization_Id NUMBER,
                              p_wip_id          number,
                              p_Wip_Name        VARCHAR2,
                              p_Wip_Type        VARCHAR2,
                              p_Doc_Date        DATE,
                              p_Now_Qty         NUMBER,
                              p_Sub             VARCHAR2,
                              p_Loc             VARCHAR2,
                              p_Return_Status   OUT VARCHAR2,
                              p_Return_Msg      OUT VARCHAR2);

  /*===============================================================
    *    Program Name:   create_feeding_doc
    *    Author      :   Felix.Liu
    *    Date        :   2015-07-30
    *    Purpose     :   创建完工入库单
    *    Parameters  :
    *             in       p_organization_id      组织ID
    *             In       p_wip_name            工单name
    *             in       p_wip_type            单据类型
    *             In       p_doc_date            日期
    *             in       p_now_qty             下达数量
                  in       p_sub                 子库
                  in       p_loc                 货位
                  out      p_return_status       返回(S/E)
                  out      p_error_mess          返回信息
    *    Update History
    *    Version    Date         Name            Description
    *    --------  ----------  ---------------  --------------------
    *     V1.1     2015-07-30   Felix.Liu          Creation
    *
  ===============================================================*/
  PROCEDURE Create_Complete_Doc(p_Organization_Id NUMBER,
                                p_Wip_Name        VARCHAR2,
                                p_Wip_Type        VARCHAR2,
                                p_Doc_Date        DATE,
                                p_Now_Qty         NUMBER,
                                p_Sub             VARCHAR2,
                                p_Loc             VARCHAR2,
                                p_Return_Status   OUT VARCHAR2,
                                p_Return_Msg      OUT VARCHAR2);
  /*===============================================================
  *    Program Name:   process_trx
  *    Author      :   Felix.Liu
  *    Date        :   2014-08-12
  *    Purpose     :   过帐完工入库单
  *    Parameters  :
  *             in       p_organization_id    组织ID
  *             In       p_doc_no            单据号
                in       p_doc_type          单据类型
  *             out      p_return_status     处理结果
                out      p_return_msg         处理信息
  *    Update History
  *    Version    Date         Name            Description
  *    --------  ----------  ---------------  --------------------
  *     V1.1     2015-07-31   Felix.Liu          Creation
  *
    ===============================================================*/
  PROCEDURE Post_Complete_Doc(p_Organization_Id NUMBER,
                              p_Doc_No          VARCHAR2,
                              p_Doc_Type        VARCHAR2,
                              p_Return_Status   OUT VARCHAR2,
                              p_Return_Msg      OUT VARCHAR2);

  PROCEDURE Add_Available_Msg(p_organization_id   number,
                              p_inventory_item_id number,
                              p_sub               varchar2,
                              p_loc               varchar2,
                              p_Onhand_Qty        number,
                              x_onhand_msg        in out varchar2,
                              x_onhand_count      in out number);
  PROCEDURE transfer_to_wms(p_doc_no          varchar2,
                            p_organization_id number,
                            p_doc_type        varchar2,
                            p_lot             varchar2,
                            p_Return_Status   OUT VARCHAR2,
                            p_Return_Msg      OUT VARCHAR2);
  PROCEDURE process_full_prepared(p_lot             varchar2,
                                  p_organization_id number,
                                  p_wip_id          number,
                                  p_wip_name        varchar2,
                                  p_Return_Status   OUT VARCHAR2,
                                  p_Return_Msg      OUT VARCHAR2);
  PROCEDURE cancel_full_prepared(p_lot             varchar2,
                                 p_organization_id number,
                                 p_wip_id          number,
                                 p_wip_name        varchar2,
                                 p_operation_code  varchar2,
                                 p_subinv          varchar2,
                                 p_Return_Status   OUT VARCHAR2,
                                 p_Return_Msg      OUT VARCHAR2);
  /* FUNCTION validate_full_prepared(p_doc_number      varchar2,
  p_organization_id number,
  p_wip_id          number,
  p_wip_name        varchar2) return varchar2;*/
END Cux_Wip_Trans_Pkg;
/
CREATE OR REPLACE PACKAGE BODY Cux_Wip_Trans_Pkg AS
  PROCEDURE Create_Preparation_Doc(p_Organization_Id NUMBER,
                                   p_wip_id          number,
                                   p_Wip_Name        VARCHAR2,
                                   p_Wip_Type        VARCHAR2,
                                   p_Doc_Date        DATE,
                                   p_Now_Qty         NUMBER,
                                   p_Sub             VARCHAR2,
                                   p_Loc             VARCHAR2,
                                   p_Operation_Code  VARCHAR2,
                                   p_param           CUX_DOC_CLASS_TYPE,
                                   p_Return_Status   OUT VARCHAR2,
                                   p_Return_Msg      OUT VARCHAR2) AS
  
    l_lot varchar2(100);
    CURSOR c_Line IS
    -- 推式备料  拉式备料
      SELECT a.Organization_Id, -- 组织ID
             a.Wip_Entity_Id, -- 工单ID
             a.Wip_Entity_Name, -- 工单
             a.Primary_Item_Id, -- 装配件
             a.Ws_Code, -- 车间
             a.Start_Quantity, -- 生产数量
             a.Inventory_Item_Id, -- 组件ID
             a.Com_Item_Code, -- 组件
             a.Item_Primary_Uom_Code, -- 单位
             a.Operation_Seq_Num, -- 工序ID
             a.Operation_Code, -- 工序
             a.Quantity_Per_Assembly, -- 单机用量
             a.Component_Yield_Factor, -- 产出率
             a.Required_Quantity, -- 需求数量
             a.Quantity_Issued, -- 已发放数量
             a.MATERIAL_BATCH1,
             a.MATERIAL_BATCH2,
             a.SPECIAL_ORDER,
             a.SPECIAL_NUMBER,
             Decode(Sign(a.Iss_Ready_Qty), -- sign 大于0 返回 1,小于0 返回 -1,等于0 返回 0
                    1,
                    a.Iss_Ready_Qty,
                    0,
                    0,
                    -1,
                    0) Iss_Ready_Qty, --
             Cws.Sub, -- 子库
             Cws.Loc_Code Loc, -- 货位
             Cws.Location_Id Inventory_Location_Id,
             To_Char(Systimestamp, 'YYYYMMDDHH24MISSFF4') Create_Lot,
             a.Replace_Flag -- 是否替代
        FROM (SELECT Wdj.Organization_Id,
                     Wdj.Wip_Entity_Id,
                     Wdj.Wip_Entity_Name,
                     Wdj.Primary_Item_Id,
                     Wdj.Attribute3 Ws_Code, --车间
                     Wdj.Start_Quantity,
                     Wro.Inventory_Item_Id,
                     Msi.Segment1 Com_Item_Code,
                     Wro.Item_Primary_Uom_Code,
                     Wro.Operation_Seq_Num,
                     Wov.Operation_Code,
                     Wro.Quantity_Per_Assembly,
                     Wro.Component_Yield_Factor,
                     Wro.Required_Quantity,
                     Wro.Quantity_Issued,
                     wro.attribute1 MATERIAL_BATCH1,
                     wro.attribute2 MATERIAL_BATCH2,
                     wro.attribute3 SPECIAL_ORDER,
                     wro.attribute4 SPECIAL_NUMBER,
                     Ceil(Wro.Quantity_Per_Assembly * p_Now_Qty /
                          Cux_Wip_Transactions_Pkg.Get_Component_Yield_Factor(p_Org_Id                 => Wro.Organization_Id,
                                                                              p_Wip_Id                 => Wro.Wip_Entity_Id,
                                                                              p_Operation_Seq_Num      => Wro.Operation_Seq_Num,
                                                                              p_Item_Id                => Wro.Inventory_Item_Id,
                                                                              p_Component_Yield_Factor => Wro.Component_Yield_Factor)) Iss_Ready_Qty,
                     Cux_Wip_Transactions_Pkg.Get_Replace_Item(p_Org_Id  => Wro.Organization_Id,
                                                               p_Wip_Id  => Wro.Wip_Entity_Id,
                                                               p_Item_Id => Wro.Inventory_Item_Id) Replace_Flag
                FROM Wip_Discrete_Jobs_v          Wdj,
                     Wip_Requirement_Operations_v Wro,
                     Wip_Operations_v             Wov,
                     Mtl_System_Items_Vl          Msi
               WHERE 1 = 1
                 AND Wdj.Organization_Id = Wro.Organization_Id
                 AND Wdj.Wip_Entity_Id = Wro.Wip_Entity_Id
                 AND Wro.Organization_Id = Wov.Organization_Id(+)
                 AND Wro.Operation_Seq_Num = Wov.Operation_Seq_Num(+)
                 AND Wro.Wip_Entity_Id = Wov.Wip_Entity_Id(+)
                 AND Msi.Organization_Id = Wro.Organization_Id
                 AND Msi.Inventory_Item_Id = Wro.Inventory_Item_Id
                    
                 AND Wdj.Organization_Id = p_Organization_Id
                 AND wdj.wip_entity_id = p_wip_id
                 AND Nvl(Wro.Repetitive_Schedule_Id, '-1') =
                     Nvl(Wov.Repetitive_Schedule_Id, '-1')
                 AND Wro.Wip_Supply_Meaning LIKE
                     '%' || Substr(p_Wip_Type, 1, 2) || '%' -- 推式,拉式
              ) a,
             Cux_Sub_Loc_Set_b Cws
       WHERE 1 = 1
         AND Cws.Organization_Id(+) = a.Organization_Id
         AND Cws.Inventory_Item_Id(+) = a.Inventory_Item_Id
         AND Cws.Ws_Code(+) = a.Ws_Code
         AND a.Iss_Ready_Qty <> 0
            
         AND Nvl(Cws.Sub, '1') = Nvl(p_Sub, Nvl(Cws.Sub, '1'))
         AND Nvl(Cws.Loc_Code, '1') = Nvl(p_Loc, Nvl(Cws.Loc_Code, '1'))
            -- 指定工序下达
         AND Nvl(a.Operation_Code, -1) =
             Nvl(p_Operation_Code, Nvl(a.Operation_Code, -1))
      
      --and (a.iss_ready_qty <> 0 and p_wip_type not in ('推式超领', '拉式超领') or p_wip_type in ('推式超领', '拉式超领'));
      
       ORDER BY Operation_Code, Sub, Replace_Flag;
  
    l_Cux_Ready_Item   Cux.Cux_Ready_Item%ROWTYPE;
    Old_Sub            VARCHAR2(100);
    Old_Operation_Code VARCHAR2(100);
    Old_Doc_No         VARCHAR2(100);
    l_Count            NUMBER := 0;
    Is_Next            VARCHAR2(1) := 'Y';
    l_To_Sub           VARCHAR2(100);
    Is_Have            NUMBER;
    l_Ready_Qty        NUMBER;
    l_Onhand_Qty       NUMBER;
    l_Is_Loc_Con       NUMBER := 0;
    l_Index            NUMBER := 0;
    l_line_number      number;
    v_Seq              NUMBER;
    v_Sy_Qty           NUMBER;
    v_Count            NUMBER := 0;
  
    -- 是否启用指定货位
    l_Appoint_Sub VARCHAR2(1) := 'N';
  
    v_onhand_msg   varchar2(2000);
    v_onhand_count number default 0;
  
    l_inv_sys_flag varchar2(10);
  BEGIN
    -- time_mark : 添加记录时间代码
    --Cux_Time_Used_Pkg.create_time_used('新下达,类型 : ' || p_Wip_Type  
    --                                    || ' 工单号 : '|| p_Wip_Name ,l_time_id);  
  
    SELECT Cux.Cux_Doc_Line_Seq.Nextval INTO v_Seq FROM Dual;
  
    -- 检查1 ： 默认子库
    FOR C0 IN c_Line LOOP
      Is_Have := 0;
    
      --检查工单组件子库
      IF C0.Sub IS NULL THEN
        Is_Next         := 'N';
        p_Return_Status := 'E';
      
        IF p_Return_Msg IS NULL THEN
          p_Return_Msg := '单据下达失败,组件子库为空,请检查:' || Chr(10);
        END IF;
        p_Return_Msg := p_Return_Msg || '[' || C0.Com_Item_Code || ']' ||
                        Chr(10);
      
      ELSE
      
        -- 子库存在,在检查货位
        BEGIN
          SELECT Locator_Type
            INTO l_Is_Loc_Con
            FROM Mtl_Secondary_Inventories_Fk_v Msi
           WHERE Msi.Secondary_Inventory_Name = C0.Sub
             AND Msi.Organization_Id = C0.Organization_Id;
        EXCEPTION
          WHEN OTHERS THEN
            l_Is_Loc_Con := 0;
        END;
      
        -- 是否开启货位控制
        IF l_Is_Loc_Con <> 1 AND C0.Inventory_Location_Id IS NULL THEN
        
          Is_Next         := 'N';
          p_Return_Status := 'E';
        
          IF l_Index = 0 THEN
            p_Return_Msg := '单据下达失败,组件货位为空,请检查:' || Chr(10);
            l_Index      := 1;
          END IF;
        
          p_Return_Msg := p_Return_Msg || '[' || C0.Com_Item_Code || ']' ||
                          Chr(10);
        
        END IF;
      
      END IF;
    END LOOP;
  
    -- 检查2 ： 判断是否有货位，但是系统又未指定
    IF p_Sub IS NOT NULL AND l_Appoint_Sub = 'Y' THEN
    
      SELECT COUNT(*)
        INTO v_Count
        FROM Mtl_Item_Locations Mil
       WHERE Mil.Subinventory_Code = p_Sub
         AND Mil.Organization_Id = p_Organization_Id;
    
      IF v_Count > 0 AND p_Loc IS NULL THEN
        p_Return_Status := 'E';
        p_Return_Msg    := '仓库:' || p_Sub || ' 有货位,请指定货位';
        RETURN;
      END IF;
    END IF;
  
    -- 检查3 
    -- 拉式备料对应的接收料子库为线边仓 GX01 与 HX01 货位为对应的工单名
    -- 推式备料无接收子库和货位,直接发货出去,无目标库
    IF p_Wip_Type = '拉式备料' THEN
    
      IF p_Organization_Id = 84 THEN
        --福建锐捷
        l_To_Sub := 'GX01';
      ELSIF p_Organization_Id = 83 THEN
        --北京锐捷
        l_To_Sub := 'HX01';
      END IF;
    
      SELECT COUNT(1)
        INTO Is_Have
        FROM Mtl_Item_Locations Mil
       WHERE Mil.Organization_Id = p_Organization_Id
         AND Nvl(Mil.Enabled_Flag, 'N') = 'Y'
         AND Mil.Subinventory_Code = l_To_Sub
         AND Mil.Segment1 = p_Wip_Name;
    
      IF Is_Have < 1 THEN
        Is_Next         := 'N';
        p_Return_Status := 'E';
        p_Return_Msg    := p_Return_Msg || '单据下达失败,组件子库[' || l_To_Sub ||
                           ']或货位[' || p_Wip_Name || ']为空,请检查:' || Chr(10);
        RETURN;
      END IF;
    END IF;
  
    -- time_mark : 添加记录时间代码
    -- Cux_Time_Used_Pkg.add_time_used('检查完成',l_time_id);
  
    -- 校验不通过,直接return
    IF Is_Next = 'N' THEN
      RETURN;
    END IF;
  
    -- 遍历所有满足条件的行
    FOR C1 IN c_Line LOOP
    
      l_Count          := l_Count + 1;
      l_Cux_Ready_Item := NULL;
    
      -- 确定定单号,根据 子库 , 工序 分出不同的备料单.
      IF (Old_Sub IS NULL AND Old_Operation_Code IS NULL) OR
         (Old_Sub <> C1.Sub) OR
         (Nvl(Old_Operation_Code, -1) <> Nvl(C1.Operation_Code, -1)) THEN
      
        -- 83 福网 ，84 京网
        IF p_Organization_Id = 83 THEN
          l_Cux_Ready_Item.Doc_No := 'RD' || Cux_Doc_No_Seq_Jw.Nextval;
        
        ELSIF p_Organization_Id = 84 THEN
          l_Cux_Ready_Item.Doc_No := 'RD' || Cux_Doc_No_Seq_Fw.Nextval;
        END IF;
      
        Old_Sub            := C1.Sub;
        Old_Operation_Code := C1.Operation_Code;
        Old_Doc_No         := l_Cux_Ready_Item.Doc_No;
      
        l_line_number := 0; --added by bruce on 20151130
      
      ELSE
        l_Cux_Ready_Item.Doc_No := Old_Doc_No;
      END IF;
      l_line_number                    := l_line_number + 1; --added by bruce on 20151130
      l_Cux_Ready_Item.Line_Id         := Cux.Cux_Doc_Line_Seq.Nextval;
      l_Cux_Ready_Item.Line_Number     := l_line_number; --added by bruce on 20151130
      l_Cux_Ready_Item.Organization_Id := C1.Organization_Id;
      l_Cux_Ready_Item.Doc_Type        := p_Wip_Type;
      l_Cux_Ready_Item.Doc_Date        := p_Doc_Date;
      l_Cux_Ready_Item.Doc_Status      := '未过账';
      l_Cux_Ready_Item.Wip_Entity_Id   := C1.Wip_Entity_Id;
      l_Cux_Ready_Item.Wip_Entity_Name := C1.Wip_Entity_Name;
      l_Cux_Ready_Item.Primary_Item_Id := C1.Primary_Item_Id;
    
      l_Cux_Ready_Item.Supply_Subinventory := C1.Sub;
      l_Cux_Ready_Item.Supply_Locator_Id   := C1.Inventory_Location_Id;
      l_Cux_Ready_Item.Supply_Loc_Code     := C1.Loc;
      -- 指定仓库
      IF p_Sub IS NOT NULL AND l_Appoint_Sub = 'Y' THEN
        l_Cux_Ready_Item.Supply_Subinventory := p_Sub;
      
        -- 指定货位
        IF p_Loc IS NOT NULL THEN
          l_Cux_Ready_Item.Supply_Loc_Code := p_Loc;
        
          SELECT Mil.Inventory_Location_Id
            INTO l_Cux_Ready_Item.Supply_Locator_Id
            FROM Mtl_Item_Locations Mil
           WHERE Mil.Organization_Id = p_Organization_Id
             AND Mil.Subinventory_Code = p_Sub
             AND Mil.Segment1 = p_Loc;
        END IF;
      END IF;
    
      l_Cux_Ready_Item.Now_Qty                := p_Now_Qty; -- 下达数
      l_Cux_Ready_Item.Inventory_Item_Id      := C1.Inventory_Item_Id; -- 物料ID
      l_Cux_Ready_Item.Item_Primary_Uom_Code  := C1.Item_Primary_Uom_Code; -- 物料单位
      l_Cux_Ready_Item.Operation_Seq_Num      := C1.Operation_Seq_Num; -- 工序编号
      l_Cux_Ready_Item.Operation_Code         := C1.Operation_Code; -- 工序代码
      l_Cux_Ready_Item.Quantity_Per_Assembly  := C1.Quantity_Per_Assembly; -- 单位需求
      l_Cux_Ready_Item.Component_Yield_Factor := C1.Component_Yield_Factor; -- 产出率
      l_Cux_Ready_Item.Required_Quantity      := C1.Required_Quantity; -- 必需量
      l_Cux_Ready_Item.Quantity_Issued        := C1.Quantity_Issued; -- 已完成量
      l_Cux_Ready_Item.Iss_Ready_Qty          := C1.Iss_Ready_Qty; -- 缺料
      l_Cux_Ready_Item.Creation_Date          := SYSDATE;
      l_Cux_Ready_Item.Created_By             := Fnd_Global.User_Id;
      l_Cux_Ready_Item.Last_Update_Date       := SYSDATE;
      l_Cux_Ready_Item.Last_Updated_By        := Fnd_Global.User_Id;
      l_Cux_Ready_Item.Last_Update_Login      := Fnd_Global.Login_Id;
      l_Cux_Ready_Item.Create_Lot             := C1.Create_Lot;
      l_lot                                   := C1.Create_Lot; --added by bruce on 20151028
      l_Cux_Ready_Item.Transfer_Status        := 'WAITING';
      
      l_Cux_Ready_Item.Material_Batch1        := C1.Material_Batch1;
      l_Cux_Ready_Item.Material_Batch2        := C1.Material_Batch2;
      l_Cux_Ready_Item.Special_Number         := C1.Special_Number;
      l_Cux_Ready_Item.Special_Order          := C1.Special_Order;
      l_Cux_Ready_Item.Empty_Return_Flag      := p_param.EMPTY_RETURN_FLAG;
      -- 拉式物料对应的线边仓对应的货位是每个工单进行命名
      IF p_Wip_Type = '拉式备料' THEN
        IF p_Organization_Id = 84 THEN
          l_Cux_Ready_Item.To_Sub := 'GX01';
        
        ELSIF p_Organization_Id = 83 THEN
          l_Cux_Ready_Item.To_Sub := 'HX01';
        END IF;
      
        BEGIN
          SELECT Mil.Inventory_Location_Id, Mil.Segment1
            INTO l_Cux_Ready_Item.To_Locator_Id,
                 l_Cux_Ready_Item.To_Loc_Code
            FROM Mtl_Item_Locations Mil
           WHERE Mil.Organization_Id = p_Organization_Id
             AND Nvl(Mil.Enabled_Flag, 'N') = 'Y'
             AND Mil.Segment1 = C1.Wip_Entity_Name;
        EXCEPTION
          WHEN OTHERS THEN
            NULL;
        END;
      END IF;
    
      --计算备料数
    
      l_Onhand_Qty := Cux_Wip_Transactions_Pkg.Get_Available_Qty(C1.Organization_Id,
                                                                 C1.Inventory_Item_Id,
                                                                 l_Cux_Ready_Item.Supply_Subinventory,
                                                                 l_Cux_Ready_Item.Supply_Locator_Id);
    
      Add_Available_Msg(p_organization_id   => C1.Organization_Id,
                        p_inventory_item_id => C1.Inventory_Item_Id,
                        p_sub               => l_Cux_Ready_Item.Supply_Subinventory,
                        p_loc               => l_Cux_Ready_Item.Supply_Loc_Code,
                        p_Onhand_Qty        => l_Onhand_Qty,
                        x_onhand_msg        => v_onhand_msg,
                        x_onhand_count      => v_onhand_count);
    
      l_Ready_Qty := 0;
      --现有量取最小整数
    
      -- 判断现有量够不够全部备料
      IF l_Onhand_Qty >= C1.Iss_Ready_Qty THEN
        l_Ready_Qty := C1.Iss_Ready_Qty;
      ELSIF l_Onhand_Qty > 0 THEN
        l_Ready_Qty := l_Onhand_Qty;
      ELSE
        l_Ready_Qty := 0;
      END IF;
    
      l_Cux_Ready_Item.Ready_Qty := l_Ready_Qty;
    
      --获取可以下达数量
      Cux_Wip_Transactions_Pkg.Get_Wip_Qt_Qty(p_Org_Id                => C1.Organization_Id,
                                              p_Wip_Id                => C1.Wip_Entity_Id,
                                              p_Operation_Seq_Num     => C1.Operation_Seq_Num,
                                              p_Item_Id               => C1.Inventory_Item_Id,
                                              p_Wip_Type              => p_Wip_Type,
                                              p_Required_Quantity     => C1.Required_Quantity,
                                              p_Quantity_Issued       => Nvl(C1.Quantity_Issued,
                                                                             0),
                                              p_Doc_No                => l_Cux_Ready_Item.Doc_No,
                                              p_Now_Qty               => p_Now_Qty,
                                              p_Quantity_Per_Assembly => C1.Quantity_Per_Assembly,
                                              p_Ready_Qty             => l_Cux_Ready_Item.Ready_Qty,
                                              p_Qty                   => v_Sy_Qty);
    
      IF l_Cux_Ready_Item.Ready_Qty >= v_Sy_Qty THEN
        l_Cux_Ready_Item.Ready_Qty := v_Sy_Qty;
      END IF;
    
      IF l_Cux_Ready_Item.Ready_Qty <= 0 THEN
        l_Count := l_Count - 1;
      ELSE
        l_Cux_Ready_Item.Iss_Ready_Qty := Ceil(C1.Quantity_Per_Assembly *
                                               p_Now_Qty /
                                               C1.Component_Yield_Factor);
        INSERT INTO Cux_Ready_Item VALUES l_Cux_Ready_Item;
      END IF;
      --保留
    
      Cux_Wip_Transactions_Pkg.Create_Save(l_Cux_Ready_Item.Line_Id,
                                           p_Return_Status,
                                           p_Return_Msg);
    
      IF p_Return_Status <> 'S' THEN
        RETURN;
      END IF;
    
    END LOOP;
  
    /* delete from CUX.CUX_WIP_ITEM_CHECK where seq = v_seq;*/
    COMMIT;
    p_Return_Status := 'S';
    p_Return_Msg    := '下达成功';
  
    IF l_Count = 0 THEN
      p_Return_Status := 'E';
      p_Return_Msg    := '无明细数据或无库存数量,单据未生成！';
    
      -- 现有量不足提示
      if v_onhand_count > 0 then
        p_Return_Msg := '存在' || v_onhand_count || '个物料对应的仓库货物无可用量:' ||
                        v_onhand_msg;
      end if;
    END IF;
  
    --added by bruce on 20151028
  
    if (p_Return_Status = 'S') then
    
      process_full_prepared(p_lot             => '',
                            p_organization_id => p_Organization_Id,
                            p_wip_id          => p_wip_id,
                            p_wip_name        => p_Wip_Name,
                            p_Return_Status   => p_Return_Status,
                            p_Return_Msg      => p_Return_Msg);
    
      if (p_Return_Status = 'S') then
      
        transfer_to_wms(p_doc_no          => '',
                        p_organization_id => p_Organization_Id,
                        p_doc_type        => '',
                        p_lot             => l_lot,
                        p_Return_Status   => p_Return_Status,
                        p_Return_Msg      => p_Return_Msg);
      
      end if;
    end if;
    --end of add by bruce on 20151028
  
    -- time_mark : 添加记录时间代码
    -- Cux_Time_Used_Pkg.add_time_used('下达完成',l_time_id);
  
  END;

  PROCEDURE Create_Preparation_Doc_Ap_Sub(p_Organization_Id NUMBER,
                                          p_wip_id          number,
                                          p_Wip_Name        VARCHAR2,
                                          p_Wip_Type        VARCHAR2,
                                          p_Doc_Date        DATE,
                                          p_Now_Qty         NUMBER,
                                          p_Sub             VARCHAR2,
                                          p_Loc             VARCHAR2,
                                          p_Operation_Code  VARCHAR2,
                                          p_param           CUX_DOC_CLASS_TYPE,
                                          p_Return_Status   OUT VARCHAR2,
                                          p_Return_Msg      OUT VARCHAR2) AS
    --added by bruce on 20151029
  
    l_lot varchar2(100);
  
    --end of add by bruce on 20151029
  
    CURSOR c_Line IS
    -- 推式备料  拉式备料
      SELECT a.Organization_Id, -- 组织ID
             a.Wip_Entity_Id, -- 工单ID
             a.Wip_Entity_Name, -- 工单
             a.Primary_Item_Id, -- 装配件
             a.Ws_Code, -- 车间
             a.Start_Quantity, -- 生产数量
             a.Inventory_Item_Id, -- 组件ID
             a.Com_Item_Code, -- 组件
             a.Item_Primary_Uom_Code, -- 单位
             a.Operation_Seq_Num, -- 工序ID
             a.Operation_Code, -- 工序
             a.Quantity_Per_Assembly, -- 单机用量
             a.Component_Yield_Factor, -- 产出率
             a.Required_Quantity, -- 需求数量
             a.Quantity_Issued, -- 已发放数量
             a.MATERIAL_BATCH1,
             a.MATERIAL_BATCH2,
             a.SPECIAL_ORDER,
             a.SPECIAL_NUMBER,
             Decode(Sign(a.Iss_Ready_Qty), -- sign 大于0 返回 1,小于0 返回 -1,等于0 返回 0
                    1,
                    a.Iss_Ready_Qty,
                    0,
                    0,
                    -1,
                    0) Iss_Ready_Qty, --
             Cws.Sub, -- 子库
             Cws.Loc_Code Loc, -- 货位
             Cws.Location_Id Inventory_Location_Id,
             To_Char(Systimestamp, 'YYYYMMDDHH24MISSFF4') Create_Lot,
             a.Replace_Flag -- 是否替代
        FROM (SELECT Wdj.Organization_Id,
                     Wdj.Wip_Entity_Id,
                     Wdj.Wip_Entity_Name,
                     Wdj.Primary_Item_Id,
                     Wdj.Attribute3 Ws_Code, --车间
                     Wdj.Start_Quantity,
                     Wro.Inventory_Item_Id,
                     Msi.Segment1 Com_Item_Code,
                     Wro.Item_Primary_Uom_Code,
                     Wro.Operation_Seq_Num,
                     Wov.Operation_Code,
                     Wro.Quantity_Per_Assembly,
                     Wro.Component_Yield_Factor,
                     Wro.Required_Quantity,
                     Wro.Quantity_Issued,
                     wro.attribute1 MATERIAL_BATCH1,
                     wro.attribute2 MATERIAL_BATCH2,
                     wro.attribute3 SPECIAL_ORDER,
                     wro.attribute4 SPECIAL_NUMBER,
                     Ceil(Wro.Quantity_Per_Assembly * p_Now_Qty /
                          Cux_Wip_Transactions_Pkg.Get_Component_Yield_Factor(p_Org_Id                 => Wro.Organization_Id,
                                                                              p_Wip_Id                 => Wro.Wip_Entity_Id,
                                                                              p_Operation_Seq_Num      => Wro.Operation_Seq_Num,
                                                                              p_Item_Id                => Wro.Inventory_Item_Id,
                                                                              p_Component_Yield_Factor => Wro.Component_Yield_Factor)) Iss_Ready_Qty,
                     Cux_Wip_Transactions_Pkg.Get_Replace_Item(p_Org_Id  => Wro.Organization_Id,
                                                               p_Wip_Id  => Wro.Wip_Entity_Id,
                                                               p_Item_Id => Wro.Inventory_Item_Id) Replace_Flag
                FROM Wip_Discrete_Jobs_v          Wdj,
                     Wip_Requirement_Operations_v Wro,
                     Wip_Operations_v             Wov,
                     Mtl_System_Items_Vl          Msi
               WHERE 1 = 1
                 AND Wdj.Organization_Id = Wro.Organization_Id
                 AND Wdj.Wip_Entity_Id = Wro.Wip_Entity_Id
                 AND Wro.Organization_Id = Wov.Organization_Id(+)
                 AND Wro.Operation_Seq_Num = Wov.Operation_Seq_Num(+)
                 AND Wro.Wip_Entity_Id = Wov.Wip_Entity_Id(+)
                 AND Msi.Organization_Id = Wro.Organization_Id
                 AND Msi.Inventory_Item_Id = Wro.Inventory_Item_Id
                 AND Wdj.Organization_Id = p_Organization_Id
                 AND wdj.wip_entity_id = p_wip_id
                 AND Nvl(Wro.Repetitive_Schedule_Id, '-1') =
                     Nvl(Wov.Repetitive_Schedule_Id, '-1')
                 AND Wro.Wip_Supply_Meaning LIKE
                     '%' || Substr(p_Wip_Type, 1, 2) || '%' -- 推式,拉式
              ) a,
             Cux_Sub_Loc_Set_b Cws
       WHERE 1 = 1
         AND Cws.Organization_Id(+) = a.Organization_Id
         AND Cws.Inventory_Item_Id(+) = a.Inventory_Item_Id
         AND Cws.Ws_Code(+) = a.Ws_Code
         AND a.Iss_Ready_Qty <> 0
            
            -- 指定工序下达
         AND Nvl(a.Operation_Code, -1) =
             Nvl(p_Operation_Code, Nvl(a.Operation_Code, -1))
      
      --and (a.iss_ready_qty <> 0 and p_wip_type not in ('推式超领', '拉式超领') or p_wip_type in ('推式超领', '拉式超领'));
      
       ORDER BY Operation_Code, Sub, Replace_Flag;
  
    l_Cux_Ready_Item   Cux.Cux_Ready_Item%ROWTYPE;
    Old_Sub            VARCHAR2(100);
    Old_Operation_Code VARCHAR2(100);
    Old_Doc_No         VARCHAR2(100);
    l_Count            NUMBER := 0;
    Is_Next            VARCHAR2(1) := 'Y';
    l_To_Sub           VARCHAR2(100);
    Is_Have            NUMBER;
    l_Ready_Qty        NUMBER;
    l_Onhand_Qty       NUMBER;
    l_Is_Loc_Con       NUMBER := 0;
    l_line_number      NUMBER;
    v_Seq              NUMBER;
    v_Sy_Qty           NUMBER;
  
    -- 是否启用指定货位
    l_Appoint_Sub VARCHAR2(1) := 'Y';
  
    v_onhand_msg   varchar2(2000);
    v_onhand_count number default 0;
  
  BEGIN
  
    -- time_mark : 添加记录时间代码
    --Cux_Time_Used_Pkg.create_time_used('新指定仓库下达,类型 : ' || p_Wip_Type  
    --                                    || ' 工单号 : '|| p_Wip_Name ,l_time_id);  
  
    SELECT Cux.Cux_Doc_Line_Seq.Nextval INTO v_Seq FROM Dual;
  
    -- 判断是否有货位，但是系统又未指定
    IF p_Sub IS NULL THEN
      p_Return_Status := 'E';
      p_Return_Msg    := '未指定仓库';
      RETURN;
    END IF;
  
    IF p_Sub IS NOT NULL AND l_Appoint_Sub = 'Y' THEN
      -- 子库存在,在检查货位
      BEGIN
        SELECT Locator_Type
          INTO l_Is_Loc_Con
          FROM Mtl_Secondary_Inventories_Fk_v Msi
         WHERE Msi.Secondary_Inventory_Name = p_Sub
           AND Msi.Organization_Id = p_Organization_Id;
      EXCEPTION
        WHEN OTHERS THEN
          l_Is_Loc_Con := 0;
      END;
    
      -- 是否开启货位控制
      IF l_Is_Loc_Con <> 1 AND p_Loc IS NULL THEN
      
        p_Return_Status := 'E';
        p_Return_Msg    := '仓库:' || p_Sub || ' 有货位,请指定货位';
        return;
      END IF;
    END IF;
  
    -- 拉式备料对应的接收料子库为线边仓 GX01 与 HX01 货位为对应的工单名
    -- 推式备料无接收子库和货位,直接发货出去,无目标库
    IF p_Wip_Type = '拉式备料' THEN
    
      IF p_Organization_Id = 84 THEN
        --福建锐捷
        l_To_Sub := 'GX01';
      ELSIF p_Organization_Id = 83 THEN
        --北京锐捷
        l_To_Sub := 'HX01';
      END IF;
    
      SELECT COUNT(1)
        INTO Is_Have
        FROM Mtl_Item_Locations Mil
       WHERE Mil.Organization_Id = p_Organization_Id
         AND Nvl(Mil.Enabled_Flag, 'N') = 'Y'
         AND Mil.Subinventory_Code = l_To_Sub
         AND Mil.Segment1 = p_Wip_Name;
    
      IF Is_Have < 1 THEN
        Is_Next         := 'N';
        p_Return_Status := 'E';
        p_Return_Msg    := p_Return_Msg || '单据下达失败,组件子库[' || l_To_Sub ||
                           ']或货位[' || p_Wip_Name || ']为空,请检查:' || Chr(10);
        RETURN;
      END IF;
    END IF;
  
    -- time_mark : 添加记录时间代码
    -- Cux_Time_Used_Pkg.add_time_used('检查完成',l_time_id);
  
    -- 校验不通过,直接return
    IF Is_Next = 'N' THEN
      RETURN;
    END IF;
  
    -- 遍历所有满足条件的行
    FOR C1 IN c_Line LOOP
    
      l_Count          := l_Count + 1;
      l_Cux_Ready_Item := NULL;
    
      -- 确定定单号,根据 子库 , 工序 分出不同的备料单.
      IF (Old_Sub IS NULL AND Old_Operation_Code IS NULL) OR
         (Old_Sub <> C1.Sub) OR
         (Nvl(Old_Operation_Code, -1) <> Nvl(C1.Operation_Code, -1)) THEN
      
        -- 83 福网 ，84 京网
        IF p_Organization_Id = 83 THEN
          l_Cux_Ready_Item.Doc_No := 'RD' || Cux_Doc_No_Seq_Jw.Nextval;
        
        ELSIF p_Organization_Id = 84 THEN
          l_Cux_Ready_Item.Doc_No := 'RD' || Cux_Doc_No_Seq_Fw.Nextval;
        END IF;
      
        Old_Sub            := C1.Sub;
        Old_Operation_Code := C1.Operation_Code;
        Old_Doc_No         := l_Cux_Ready_Item.Doc_No;
      
        l_line_number := 0; --added by bruce on 20151130
      
      ELSE
        l_Cux_Ready_Item.Doc_No := Old_Doc_No;
      END IF;
      l_line_number                    := l_line_number + 1; --added by bruce on 20151130
      l_Cux_Ready_Item.Line_Id         := Cux.Cux_Doc_Line_Seq.Nextval;
      l_Cux_Ready_Item.Line_Number     := l_line_number; --added by bruce on 20151130
      l_Cux_Ready_Item.Organization_Id := C1.Organization_Id;
      l_Cux_Ready_Item.Doc_Type        := p_Wip_Type;
      l_Cux_Ready_Item.Doc_Date        := p_Doc_Date;
      l_Cux_Ready_Item.Doc_Status      := '未过账';
      l_Cux_Ready_Item.Wip_Entity_Id   := C1.Wip_Entity_Id;
      l_Cux_Ready_Item.Wip_Entity_Name := C1.Wip_Entity_Name;
      l_Cux_Ready_Item.Primary_Item_Id := C1.Primary_Item_Id;
    
      -- 加载默认仓库
      l_Cux_Ready_Item.Supply_Subinventory := C1.Sub;
      l_Cux_Ready_Item.Supply_Locator_Id   := C1.Inventory_Location_Id;
      l_Cux_Ready_Item.Supply_Loc_Code     := C1.Loc;
    
      -- 指定仓库
      IF p_Sub IS NOT NULL AND l_Appoint_Sub = 'Y' THEN
        l_Cux_Ready_Item.Supply_Subinventory := p_Sub;
      
        -- 指定货位
        IF p_Loc IS NOT NULL THEN
          l_Cux_Ready_Item.Supply_Loc_Code := p_Loc;
        
          SELECT Mil.Inventory_Location_Id
            INTO l_Cux_Ready_Item.Supply_Locator_Id
            FROM Mtl_Item_Locations Mil
           WHERE Mil.Organization_Id = p_Organization_Id
             AND Mil.Subinventory_Code = p_Sub
             AND Mil.Segment1 = p_Loc;
        ELSE
          -- 判断是否有指定货位
          BEGIN
            SELECT Locator_Type
              INTO l_Is_Loc_Con
              FROM Mtl_Secondary_Inventories_Fk_v Msi
             WHERE Msi.Secondary_Inventory_Name = p_Sub
               AND Msi.Organization_Id = p_Organization_Id;
          EXCEPTION
            WHEN OTHERS THEN
              l_Is_Loc_Con := 0;
          END;
        
          -- 未启用货位控制
          IF l_Is_Loc_Con = 1 THEN
            l_Cux_Ready_Item.Supply_Locator_Id := NULL;
            l_Cux_Ready_Item.Supply_Loc_Code   := NULL;
          END IF;
        END IF;
      END IF;
    
      l_Cux_Ready_Item.Now_Qty                := p_Now_Qty; -- 下达数
      l_Cux_Ready_Item.Inventory_Item_Id      := C1.Inventory_Item_Id; -- 物料ID
      l_Cux_Ready_Item.Item_Primary_Uom_Code  := C1.Item_Primary_Uom_Code; -- 物料单位
      l_Cux_Ready_Item.Operation_Seq_Num      := C1.Operation_Seq_Num; -- 工序编号
      l_Cux_Ready_Item.Operation_Code         := C1.Operation_Code; -- 工序代码
      l_Cux_Ready_Item.Quantity_Per_Assembly  := C1.Quantity_Per_Assembly; -- 单位需求
      l_Cux_Ready_Item.Component_Yield_Factor := C1.Component_Yield_Factor; -- 产出率
      l_Cux_Ready_Item.Required_Quantity      := C1.Required_Quantity; -- 必需量
      l_Cux_Ready_Item.Quantity_Issued        := C1.Quantity_Issued; -- 已完成量
      l_Cux_Ready_Item.Iss_Ready_Qty          := C1.Iss_Ready_Qty; -- 缺料
      l_Cux_Ready_Item.Creation_Date          := SYSDATE;
      l_Cux_Ready_Item.Created_By             := Fnd_Global.User_Id;
      l_Cux_Ready_Item.Last_Update_Date       := SYSDATE;
      l_Cux_Ready_Item.Last_Updated_By        := Fnd_Global.User_Id;
      l_Cux_Ready_Item.Last_Update_Login      := Fnd_Global.Login_Id;
      l_Cux_Ready_Item.Create_Lot             := C1.Create_Lot;
      l_lot                                   := C1.Create_Lot; --added by bruce on 20151029
      l_Cux_Ready_Item.Transfer_Status        := 'WAITING';
      
      l_Cux_Ready_Item.Material_Batch1        := C1.Material_Batch1;
      l_Cux_Ready_Item.Material_Batch2        := C1.Material_Batch2;
      l_Cux_Ready_Item.Special_Number         := C1.Special_Number;
      l_Cux_Ready_Item.Special_Order          := C1.Special_Order;
      l_Cux_Ready_Item.Empty_Return_Flag      := p_param.EMPTY_RETURN_FLAG;
      -- 拉式物料对应的线边仓对应的货位是每个工单进行命名
      IF p_Wip_Type = '拉式备料' THEN
        IF p_Organization_Id = 84 THEN
          l_Cux_Ready_Item.To_Sub := 'GX01';
        
        ELSIF p_Organization_Id = 83 THEN
          l_Cux_Ready_Item.To_Sub := 'HX01';
        END IF;
      
        BEGIN
          SELECT Mil.Inventory_Location_Id, Mil.Segment1
            INTO l_Cux_Ready_Item.To_Locator_Id,
                 l_Cux_Ready_Item.To_Loc_Code
            FROM Mtl_Item_Locations Mil
           WHERE Mil.Organization_Id = p_Organization_Id
             AND Nvl(Mil.Enabled_Flag, 'N') = 'Y'
             AND Mil.Segment1 = C1.Wip_Entity_Name;
        EXCEPTION
          WHEN OTHERS THEN
            NULL;
        END;
      END IF;
    
      --计算备料数
    
      l_Onhand_Qty := Cux_Wip_Transactions_Pkg.Get_Available_Qty(C1.Organization_Id,
                                                                 C1.Inventory_Item_Id,
                                                                 l_Cux_Ready_Item.Supply_Subinventory,
                                                                 l_Cux_Ready_Item.Supply_Locator_Id);
    
      Add_Available_Msg(p_organization_id   => C1.Organization_Id,
                        p_inventory_item_id => C1.Inventory_Item_Id,
                        p_sub               => l_Cux_Ready_Item.Supply_Subinventory,
                        p_loc               => l_Cux_Ready_Item.Supply_Loc_Code,
                        p_Onhand_Qty        => l_Onhand_Qty,
                        x_onhand_msg        => v_onhand_msg,
                        x_onhand_count      => v_onhand_count);
    
      l_Ready_Qty := 0;
      --现有量取最小整数
    
      -- 判断现有量够不够全部备料
      IF l_Onhand_Qty >= C1.Iss_Ready_Qty THEN
        l_Ready_Qty := C1.Iss_Ready_Qty;
      ELSIF l_Onhand_Qty > 0 THEN
        l_Ready_Qty := l_Onhand_Qty;
      ELSE
        l_Ready_Qty := 0;
      END IF;
    
      l_Cux_Ready_Item.Ready_Qty := l_Ready_Qty;
    
      --获取可以下达数量
      Cux_Wip_Transactions_Pkg.Get_Wip_Qt_Qty(p_Org_Id                => C1.Organization_Id,
                                              p_Wip_Id                => C1.Wip_Entity_Id,
                                              p_Operation_Seq_Num     => C1.Operation_Seq_Num,
                                              p_Item_Id               => C1.Inventory_Item_Id,
                                              p_Wip_Type              => p_Wip_Type,
                                              p_Required_Quantity     => C1.Required_Quantity,
                                              p_Quantity_Issued       => Nvl(C1.Quantity_Issued,
                                                                             0),
                                              p_Doc_No                => l_Cux_Ready_Item.Doc_No,
                                              p_Now_Qty               => p_Now_Qty,
                                              p_Quantity_Per_Assembly => C1.Quantity_Per_Assembly,
                                              p_Ready_Qty             => l_Cux_Ready_Item.Ready_Qty,
                                              p_Qty                   => v_Sy_Qty);
    
      IF l_Cux_Ready_Item.Ready_Qty >= v_Sy_Qty THEN
        l_Cux_Ready_Item.Ready_Qty := v_Sy_Qty;
      END IF;
    
      IF l_Cux_Ready_Item.Ready_Qty <= 0 THEN
        l_Count := l_Count - 1;
      ELSE
        l_Cux_Ready_Item.Iss_Ready_Qty := Ceil(C1.Quantity_Per_Assembly *
                                               p_Now_Qty /
                                               C1.Component_Yield_Factor);
        INSERT INTO Cux_Ready_Item VALUES l_Cux_Ready_Item;
      END IF;
      --保留
    
      Cux_Wip_Transactions_Pkg.Create_Save(l_Cux_Ready_Item.Line_Id,
                                           p_Return_Status,
                                           p_Return_Msg);
    
      IF p_Return_Status <> 'S' THEN
        RETURN;
      END IF;
    
    END LOOP;
  
    /* delete from CUX.CUX_WIP_ITEM_CHECK where seq = v_seq;*/
    COMMIT;
    p_Return_Status := 'S';
    p_Return_Msg    := '下达成功';
  
    IF l_Count = 0 THEN
      p_Return_Status := 'E';
      p_Return_Msg    := '无明细数据或无库存数量,单据未生成！';
    
      -- 现有量不足提示
      if v_onhand_count > 0 then
        p_Return_Msg := '存在' || v_onhand_count || '个物料对应的仓库货物无可用量:' ||
                        v_onhand_msg;
      end if;
    END IF;
  
    --added by bruce on 20151028
  
    if (p_Return_Status = 'S') then
    
      process_full_prepared(p_lot             => '',
                            p_organization_id => p_Organization_Id,
                            p_wip_id          => p_wip_id,
                            p_wip_name        => p_Wip_Name,
                            p_Return_Status   => p_Return_Status,
                            p_Return_Msg      => p_Return_Msg);
    
      if (p_Return_Status = 'S') then
      
        transfer_to_wms(p_doc_no          => '',
                        p_organization_id => p_Organization_Id,
                        p_doc_type        => '',
                        p_lot             => l_lot,
                        p_Return_Status   => p_Return_Status,
                        p_Return_Msg      => p_Return_Msg);
      
      end if;
    end if;
  
    --end of add by bruce on 20151028
  
    -- time_mark : 添加记录时间代码
    -- Cux_Time_Used_Pkg.add_time_used('下达完成',l_time_id);  
  END;

  PROCEDURE Post_Preparation_Doc(p_Organization_Id NUMBER,
                                 p_doc_no          VARCHAR2,
                                 p_doc_type        VARCHAR2,
                                 p_return_status   OUT VARCHAR2,
                                 p_return_msg      OUT VARCHAR2) AS
  
    l_iface_rec               inv.mtl_transactions_interface%ROWTYPE;
    l_user_id                 NUMBER := fnd_global.user_id;
    l_return_count            NUMBER;
    x_msg_count               NUMBER;
    xx_msg_data               VARCHAR2(4000);
    x_trans_count             NUMBER;
    l_rem_qty                 NUMBER;
    l_auto_compute_final_comp NUMBER;
    l_ishave                  NUMBER := 0;
  
    -- 过账单据
    CURSOR c_lines IS
      SELECT cri.organization_id,
             cri.line_id,
             cri.doc_no,
             cri.wip_entity_id,
             cri.wip_entity_name,
             cri.supply_subinventory,
             cri.supply_locator_id,
             cri.to_sub,
             cri.to_locator_id,
             cri.ready_qty,
             cri.now_qty,
             cri.item_primary_uom_code,
             cri.inventory_item_id,
             cri.primary_item_id,
             cri.doc_type,
             cri.operation_seq_num
        FROM cux_ready_item cri
       WHERE cri.organization_id = p_organization_id
         AND cri.doc_no = p_doc_no
         AND cri.doc_type = p_doc_type
         AND ((cri.doc_type IN ('推式退料(负单)', '推式退料(零单)') AND
             nvl(cri.attribute1, 'N') = 'Y') OR
             cri.doc_type NOT IN
             ('推式退料(负单)', '推式退料(零单)', '推式超领', '拉式超领') OR
             (cri.doc_type IN ('推式超领', '拉式超领') AND
             nvl(cri.attribute1, 'N') = 'Y'))
         AND ((cri.ready_qty > 0 AND cri.doc_type <> '完工入库') OR
             (cri.now_qty > 0 AND cri.doc_type = '完工入库'))
         AND cri.pro_trx_id IS NULL;
    v_reservation_quantity1 NUMBER;
    v_reservation_quantity  NUMBER;
    v_item_code             VARCHAR2(250);
    v_qty                   NUMBER;
  BEGIN
  
    --mhw add 2014-10-25 添加判断退料量是否大于完工量
    IF p_doc_type = '推式退料(零单)' THEN
      SELECT COUNT(1)
        INTO l_ishave
        FROM (SELECT wo.quantity_completed * cri.quantity_per_assembly /
                     cri.component_yield_factor wip_com_qty,
                     wro.quantity_issued - nvl(cri.ready_qty, 0) give_qty
                FROM cux_ready_item               cri,
                     wip_operations               wo,
                     wip_requirement_operations_v wro
               WHERE 1 = 1
                 AND wo.wip_entity_id = cri.wip_entity_id
                 AND wo.organization_id = cri.organization_id
                 AND nvl(wo.quantity_completed, 0) > 0
                 AND wo.next_operation_seq_num IS NULL
                 AND wro.organization_id = cri.organization_id
                 AND wro.inventory_item_id = cri.inventory_item_id
                 AND wro.operation_seq_num = cri.operation_seq_num
                 AND wro.wip_entity_id = cri.wip_entity_id
                 AND cri.organization_id = p_organization_id
                 AND cri.doc_no = p_doc_no
                 AND cri.doc_type = '推式退料(零单)') a
       WHERE a.give_qty < a.wip_com_qty;
      IF l_ishave > 0 THEN
        p_return_status := 'E';
        p_return_msg    := '检查有组件退料数量大于完工所需最小套数，不能退料。';
        RETURN;
      END IF;
    END IF;
    --mhw end 
  
    ----增加数量验证,判断是否都是足够过账
    FOR c1 IN c_lines LOOP
    
      IF c1.doc_type IN ('拉式备料',
                         '拉式补料',
                         '拉式退料',
                         '拉式超领',
                         '推式备料',
                         '推式补料',
                         '推式超领') THEN
      
        --获取保留数量
        SELECT nvl(SUM(mr.reservation_quantity), 0)
          INTO v_reservation_quantity
          FROM mtl_reservations mr
         WHERE mr.supply_source_name = c1.doc_no
           AND mr.demand_source_header_id = c1.wip_entity_id
           AND mr.demand_source_line_id = c1.line_id
           AND mr.organization_id = p_organization_id;
      
        --获取可用量   
        v_reservation_quantity1 := cux_wip_transactions_pkg.get_available_qty(p_organization_id   => p_organization_id,
                                                                              p_inventory_item_id => c1.inventory_item_id,
                                                                              p_subinventory_code => c1.supply_subinventory,
                                                                              p_loc_id            => c1.supply_locator_id);
      
        v_qty := nvl(v_reservation_quantity, 0) +
                 nvl(v_reservation_quantity1, 0);
      
        -- 判断现有量 是否足够下料                                                                     
        IF v_qty < c1.ready_qty THEN
          SELECT msi.segment1
            INTO v_item_code
            FROM mtl_system_items_b msi
           WHERE msi.inventory_item_id = c1.inventory_item_id
             AND msi.organization_id = p_organization_id;
        
          p_return_msg    := '物料' || v_item_code || '处理数量(' || c1.ready_qty ||
                             ')大于可用量' || '(' || v_qty || ')';
          p_return_status := 'E';
          RETURN;
        END IF;
      
      END IF;
    END LOOP;
  
    /**
    *   进行过账操作
    *   1. 先释放保留
    *   2. 根据实际情况插入接口数据
    */
    FOR c1 IN c_lines LOOP
      --取消保留
      DELETE mtl_reservations mr
       WHERE mr.supply_source_name = c1.doc_no
         AND mr.demand_source_header_id = c1.wip_entity_id
         AND mr.demand_source_line_id = c1.line_id
         AND mr.organization_id = p_organization_id;
      COMMIT;
    
      l_iface_rec                   := NULL;
      l_iface_rec.last_update_date  := SYSDATE;
      l_iface_rec.last_updated_by   := l_user_id;
      l_iface_rec.creation_date     := SYSDATE;
      l_iface_rec.created_by        := l_user_id;
      l_iface_rec.last_update_login := fnd_global.login_id;
    
      -- 设置必要信息
      IF c1.doc_type IN ('拉式备料', '拉式补料', '拉式退料', '拉式超领') THEN
      
        l_iface_rec.transaction_type_id   := 101; --拉式物料发放
        l_iface_rec.transfer_subinventory := c1.to_sub;
        l_iface_rec.transfer_locator      := c1.to_locator_id;
      
      ELSIF c1.doc_type IN ('推式备料', '推式补料', '推式超领') THEN
        l_iface_rec.transaction_type_id := 35; --WIP 发放
        c1.ready_qty                    := c1.ready_qty * -1;
        l_iface_rec.operation_seq_num   := c1.operation_seq_num;
      
      ELSIF c1.doc_type IN ('推式退料(负单)', '推式退料(零单)') THEN
        l_iface_rec.transaction_type_id := 43; --WIP 退回
        c1.ready_qty                    := c1.ready_qty;
        l_iface_rec.operation_seq_num   := c1.operation_seq_num;
      
      ELSE
        p_return_status := 'E';
        p_return_msg    := '单据类型异常：' || c1.doc_type || SQLERRM || '代码行:' ||
                           dbms_utility.format_error_backtrace;
        ROLLBACK;
        RETURN;
      END IF;
    
      -- 设置 source_code 与 transaction_reference 信息
      IF c1.doc_type IN ('拉式备料', '推式备料') THEN
        l_iface_rec.source_code           := '备料单';
        l_iface_rec.transaction_reference := c1.doc_type || '[备料单号:' ||
                                             c1.doc_no || ']';
      ELSIF c1.doc_type IN ('推式补料', '拉式补料') THEN
        l_iface_rec.source_code           := '补料单';
        l_iface_rec.transaction_reference := c1.doc_type || '[补料单号:' ||
                                             c1.doc_no || ']';
      ELSIF c1.doc_type IN ('推式退料(负单)', '推式退料(零单)', '拉式退料') THEN
        l_iface_rec.source_code           := '退料单';
        l_iface_rec.transaction_reference := c1.doc_type || '[退料单号:' ||
                                             c1.doc_no || ']';
      ELSIF c1.doc_type IN ('推式超领', '拉式超领') THEN
        l_iface_rec.source_code           := '超领单';
        l_iface_rec.transaction_reference := c1.doc_type || '[超领单号:' ||
                                             c1.doc_no || ']';
      END IF;
    
      -- 物料事务接口ID
      SELECT mtl_material_transactions_s.nextval
        INTO l_iface_rec.transaction_interface_id
        FROM dual;
    
      l_iface_rec.transaction_header_id := l_iface_rec.transaction_interface_id;
      l_iface_rec.transaction_mode      := 3;
      l_iface_rec.process_flag          := 1;
    
      l_iface_rec.organization_id := c1.organization_id;
    
      l_iface_rec.inventory_item_id    := c1.inventory_item_id;
      l_iface_rec.subinventory_code    := c1.supply_subinventory;
      l_iface_rec.locator_id           := c1.supply_locator_id;
      l_iface_rec.transaction_quantity := c1.ready_qty;
      l_iface_rec.primary_quantity     := c1.ready_qty;
    
      l_iface_rec.transfer_organization := c1.organization_id;
      l_iface_rec.transaction_uom       := c1.item_primary_uom_code;
      l_iface_rec.transaction_date      := SYSDATE;
      l_iface_rec.source_header_id      := c1.wip_entity_id;
      l_iface_rec.transaction_source_id := c1.wip_entity_id; --wip_entity_id
      l_iface_rec.source_line_id        := c1.line_id;
    
      -- 插入接口表
      INSERT INTO inv.mtl_transactions_interface VALUES l_iface_rec;
    
      fnd_msg_pub.initialize;
      p_return_status := fnd_api.g_ret_sts_success;
      l_return_count  := inv_txn_manager_pub.process_transactions(p_api_version      => 1.0,
                                                                  p_init_msg_list    => fnd_api.g_false,
                                                                  p_commit           => fnd_api.g_false,
                                                                  p_validation_level => fnd_api.g_valid_level_full,
                                                                  x_return_status    => p_return_status,
                                                                  x_msg_count        => x_msg_count,
                                                                  x_msg_data         => xx_msg_data,
                                                                  x_trans_count      => x_trans_count,
                                                                  p_table            => 1,
                                                                  p_header_id        => l_iface_rec.transaction_header_id);
    
      -- 接口处理失败
      IF l_return_count = -1 OR
         p_return_status <> fnd_api.g_ret_sts_success THEN
        p_return_msg := p_return_msg || l_iface_rec.transaction_header_id || ': ';
        FOR a_rec IN (SELECT mti.transaction_interface_id,
                             mti.error_code,
                             mti.error_explanation
                        FROM mtl_transactions_interface mti
                       WHERE mti.transaction_header_id =
                             l_iface_rec.transaction_header_id) LOOP
        
          dbms_output.put_line('transaction_interface_id: ' ||
                               a_rec.transaction_interface_id);
          dbms_output.put_line('error_code: ' || a_rec.error_code);
          dbms_output.put_line('error_explanation: ' ||
                               a_rec.error_explanation);
          p_return_status := 'E';
          p_return_msg    := substr(p_return_msg || a_rec.error_code || ':' ||
                                    a_rec.error_explanation || chr(10),
                                    1,
                                    255);
        END LOOP;
        ROLLBACK;
      
        --删除已插入的接口
        /*DELETE inv.mtl_transactions_interface
         WHERE transaction_header_id = l_iface_rec.transaction_header_id;
        
        DELETE CST_COMP_SNAP_INTERFACE
         WHERE TRANSACTION_INTERFACE_ID =L_IFACE_REC.TRANSACTION_INTERFACE_ID;
        commit;*/
        RETURN;
      ELSE
      
        UPDATE cux_ready_item cri
           SET cri.pro_trx_id       = l_iface_rec.transaction_header_id,
               cri.last_updated_by  = l_user_id,
               cri.last_update_date = SYSDATE,
               cri.pro_date         = SYSDATE
         WHERE cri.line_id = c1.line_id;
      END IF;
    
    END LOOP;
  
    UPDATE cux_ready_item cri
       SET cri.doc_status       = '已过账',
           cri.last_updated_by  = l_user_id,
           cri.last_update_date = SYSDATE
     WHERE cri.doc_no = p_doc_no
       AND cri.organization_id = p_organization_id;
  
    p_return_msg := '过账成功';
  
    COMMIT;
  
    --二次处理,防止现有量未释放
    DELETE mtl_reservations mr
     WHERE EXISTS (SELECT 1
              FROM cux_ready_item cri
             WHERE 1 = 1
               AND mr.supply_source_name = cri.doc_no
               AND mr.demand_source_header_id = cri.wip_entity_id
               AND mr.demand_source_line_id = cri.line_id
               AND mr.organization_id = cri.organization_id
               AND cri.doc_status = '已过账');
    COMMIT;
  
  EXCEPTION
    WHEN OTHERS THEN
      p_return_status := 'E';
      p_return_msg    := '事物处理异常' || SQLERRM || '代码行:' ||
                         dbms_utility.format_error_backtrace;
      ROLLBACK;
  end;

  PROCEDURE Create_Feeding_Doc(p_Organization_Id NUMBER,
                               p_wip_id          number,
                               p_Wip_Name        VARCHAR2,
                               p_Wip_Type        VARCHAR2,
                               p_Doc_Date        DATE,
                               p_Now_Qty         NUMBER,
                               p_Sub             VARCHAR2,
                               p_Loc             VARCHAR2,
                               p_Operation_Code  VARCHAR2,
                               p_param           CUX_DOC_CLASS_TYPE,
                               p_Return_Status   OUT VARCHAR2,
                               p_Return_Msg      OUT VARCHAR2) AS
  
    l_lot varchar2(100);
  
    CURSOR c_Line IS
      SELECT a.Organization_Id,
             a.Wip_Entity_Id,
             a.Wip_Entity_Name,
             a.Primary_Item_Id,
             a.Ws_Code, --车间
             a.Start_Quantity,
             a.Inventory_Item_Id,
             a.Com_Item_Code,
             a.Item_Primary_Uom_Code,
             a.Operation_Seq_Num,
             a.Operation_Code,
             a.Quantity_Per_Assembly,
             a.Component_Yield_Factor,
             a.Required_Quantity,
             a.Quantity_Issued,
             a.MATERIAL_BATCH1,
             a.MATERIAL_BATCH2,
             a.SPECIAL_ORDER,
             a.SPECIAL_NUMBER,
             a.Iss_Ready_Qty,
             Cws.Sub,
             Cws.Loc_Code Loc,
             Cws.Location_Id Inventory_Location_Id,
             To_Char(Systimestamp, 'YYYYMMDDHH24MISSFF4') Create_Lot,
             a.Replace_Flag
        FROM (SELECT Wdj.Organization_Id,
                     Wdj.Wip_Entity_Id,
                     Wdj.Wip_Entity_Name,
                     Wdj.Primary_Item_Id,
                     Wdj.Attribute3 Ws_Code, --车间
                     Wdj.Start_Quantity,
                     Wro.Inventory_Item_Id,
                     Msi.Segment1 Com_Item_Code,
                     Wro.Item_Primary_Uom_Code,
                     Wro.Operation_Seq_Num,
                     Wov.Operation_Code,
                     Wro.Quantity_Per_Assembly,
                     Wro.Component_Yield_Factor,
                     Wro.Required_Quantity,
                     Wro.Quantity_Issued,
                     wro.attribute1 MATERIAL_BATCH1,
                     wro.attribute2 MATERIAL_BATCH2,
                     wro.attribute3 SPECIAL_ORDER,
                     wro.attribute4 SPECIAL_NUMBER,
                     Ceil(Wro.Quantity_Per_Assembly * p_Now_Qty /
                          Cux_Wip_Transactions_Pkg.Get_Component_Yield_Factor(p_Org_Id                 => Wro.Organization_Id,
                                                                              p_Wip_Id                 => Wro.Wip_Entity_Id,
                                                                              p_Operation_Seq_Num      => Wro.Operation_Seq_Num,
                                                                              p_Item_Id                => Wro.Inventory_Item_Id,
                                                                              p_Component_Yield_Factor => Wro.Component_Yield_Factor)) -
                     
                     (SELECT Nvl(SUM(Nvl(Decode(Cri.Doc_Type,
                                                '拉式退料',
                                                Cri.Ready_Qty * -1,
                                                '推式退料(负单)',
                                                Cri.Ready_Qty * -1,
                                                '推式退料(零单)',
                                                Cri.Ready_Qty * -1,
                                                Cri.Ready_Qty),
                                         0)),
                                 0)
                        FROM Cux_Ready_Item Cri
                       WHERE Cri.Organization_Id = Wro.Organization_Id
                         AND Cri.Wip_Entity_Id = Wro.Wip_Entity_Id
                         AND Cri.Inventory_Item_Id = Wro.Inventory_Item_Id
                         AND Cri.Operation_Seq_Num = Wro.Operation_Seq_Num
                         AND Cri.Doc_Type NOT LIKE '%超领%') Iss_Ready_Qty,
                     
                     Cux_Wip_Transactions_Pkg.Get_Replace_Item(p_Org_Id  => Wro.Organization_Id,
                                                               p_Wip_Id  => Wro.Wip_Entity_Id,
                                                               p_Item_Id => Wro.Inventory_Item_Id) Replace_Flag
                FROM Wip_Discrete_Jobs_v          Wdj,
                     Wip_Requirement_Operations_v Wro,
                     Wip_Operations_v             Wov,
                     Mtl_System_Items_Vl          Msi
               WHERE 1 = 1
                 AND Wdj.Organization_Id = Wro.Organization_Id
                 AND Wdj.Wip_Entity_Id = Wro.Wip_Entity_Id
                 AND Wro.Organization_Id = Wov.Organization_Id(+)
                 AND Wro.Operation_Seq_Num = Wov.Operation_Seq_Num(+)
                 AND Wro.Wip_Entity_Id = Wov.Wip_Entity_Id(+)
                 AND Msi.Organization_Id = Wro.Organization_Id
                 AND Msi.Inventory_Item_Id = Wro.Inventory_Item_Id
                    
                 AND Wdj.Organization_Id = p_Organization_Id
                 AND wdj.wip_entity_id = p_wip_id
                 AND Nvl(Wro.Repetitive_Schedule_Id, '-1') =
                     Nvl(Wov.Repetitive_Schedule_Id, '-1')
                 AND Wro.Wip_Supply_Meaning LIKE
                     '%' || Substr(p_Wip_Type, 1, 2) || '%') a,
             Cux_Sub_Loc_Set_b Cws
       WHERE 1 = 1
         AND Cws.Organization_Id(+) = a.Organization_Id
         AND Cws.Inventory_Item_Id(+) = a.Inventory_Item_Id
         AND Cws.Ws_Code(+) = a.Ws_Code
         AND Nvl(Cws.Sub, '1') = Nvl(p_Sub, Nvl(Cws.Sub, '1'))
         AND Nvl(Cws.Loc_Code, '1') = Nvl(p_Loc, Nvl(Cws.Loc_Code, '1'))
         AND a.Iss_Ready_Qty > 0
            -- 指定工序下达
         AND Nvl(a.Operation_Code, -1) =
             Nvl(p_Operation_Code, Nvl(a.Operation_Code, -1))
      
       ORDER BY a.Operation_Code, Cws.Sub, a.Replace_Flag;
  
    l_Cux_Ready_Item Cux.Cux_Ready_Item%ROWTYPE;
    l_Count          NUMBER := 0;
    Is_Next          VARCHAR2(1) := 'Y';
    l_To_Sub         VARCHAR2(100);
    Is_Have          NUMBER;
    l_Ready_Qty      NUMBER;
    l_Onhand_Qty     NUMBER;
    l_Is_Loc_Con     NUMBER := 0;
    l_Index          NUMBER := 0;
  
    v_Create_Lot        VARCHAR2(100);
    v_Create_Lot_New    VARCHAR2(100);
    v_Required_Item_Qty NUMBER;
    v_Ready_Item_b_Qty  NUMBER;
    v_Ready_Qty_b       NUMBER;
    v_Ready_Qty_s       NUMBER;
    v_Quantity_Issued   NUMBER;
    v_Doc_No            VARCHAR2(100);
  
    l_old_doc_number varchar2(100);
    l_line_number    number;
  
    v_Ready_Qty_New NUMBER;
  
    v_Qty NUMBER;
  
    v_Count NUMBER;
  
    v_Create_Lot_1 VARCHAR2(100);
  
    v_temp_item varchar2(100);
    -- 是否启用指定货位
    l_Appoint_Sub VARCHAR2(1) := 'N';
    v_Flag        VARCHAR2(1); --是否成品仓标识
  
    v_temp_qty number;
  
    v_onhand_msg   varchar2(2000);
    v_onhand_count number default 0;
  BEGIN
    -- time_mark : 添加记录时间代码
    -- Cux_Time_Used_Pkg.create_time_used('新指定下达,类型 : ' || p_Wip_Type  
    --                                    || ' 工单号 : '|| p_Wip_Name ,l_time_id);  
  
    FOR C0 IN c_Line LOOP
      Is_Have := 0;
    
      ---------------------- 判断来源子存( 发料子库 ) 校验 ----------------------
      IF C0.Sub IS NULL THEN
        Is_Next         := 'N';
        p_Return_Status := 'E';
      
        IF p_Return_Msg IS NULL THEN
          p_Return_Msg := '单据下达失败,组件子库为空,请检查:' || Chr(10);
        END IF;
        p_Return_Msg := p_Return_Msg || '[' || C0.Com_Item_Code || ']' ||
                        Chr(10);
      
      ELSE
        -- 存在来源子库,继续判断是否启用货位控制,如果启用,必须存在货位
        BEGIN
          SELECT Locator_Type
            INTO l_Is_Loc_Con
            FROM Mtl_Secondary_Inventories_Fk_v Msi
           WHERE Msi.Secondary_Inventory_Name = C0.Sub
             AND Msi.Organization_Id = C0.Organization_Id;
        EXCEPTION
          WHEN OTHERS THEN
            l_Is_Loc_Con := 0;
        END;
      
        IF l_Is_Loc_Con <> 1 AND C0.Inventory_Location_Id IS NULL THEN
        
          Is_Next         := 'N';
          p_Return_Status := 'E';
        
          IF l_Index = 0 THEN
            p_Return_Msg := '单据下达失败,组件货位为空,请检查:' || Chr(10);
            l_Index      := 1;
          END IF;
        
          p_Return_Msg := p_Return_Msg || '[' || C0.Com_Item_Code || ']' ||
                          Chr(10);
        
        END IF;
      
      END IF;
    END LOOP;
  
    -- 如果系统指定仓库,则同时校验货位是否必须
    IF p_Sub IS NOT NULL AND l_Appoint_Sub = 'Y' THEN
    
      SELECT COUNT(*)
        INTO v_Count
        FROM Mtl_Item_Locations Mil
       WHERE Mil.Subinventory_Code = p_Sub
         AND Mil.Organization_Id = p_Organization_Id;
    
      IF v_Count > 0 AND p_Loc IS NULL THEN
        p_Return_Status := 'E';
        p_Return_Msg    := '仓库有货位,未指定货位';
        RETURN;
      END IF;
    END IF;
  
    ---------------------- 判断目标子存 ( 接收子库 ) 校验 ----------------------
    -- 推式料无接收子库,直接出库
    IF p_Wip_Type = '拉式补料' THEN
    
      IF p_Organization_Id = 84 THEN
        --福建锐捷
        l_To_Sub := 'GX01';
      ELSIF p_Organization_Id = 83 THEN
        --北京锐捷
        l_To_Sub := 'HX01';
      END IF;
    
      SELECT COUNT(1)
        INTO Is_Have
        FROM Mtl_Item_Locations Mil
       WHERE Mil.Organization_Id = p_Organization_Id
         AND Nvl(Mil.Enabled_Flag, 'N') = 'Y'
         AND Mil.Subinventory_Code = l_To_Sub
         AND Mil.Segment1 = p_Wip_Name;
    
      IF Is_Have < 1 THEN
        Is_Next         := 'N';
        p_Return_Status := 'E';
        p_Return_Msg    := p_Return_Msg || '单据下达失败,组件子库[' || l_To_Sub ||
                           ']或货位[' || p_Wip_Name || ']为空,请检查:' || Chr(10);
      
        RETURN;
      END IF;
    END IF;
  
    -- time_mark : 添加记录时间代码
    -- Cux_Time_Used_Pkg.add_time_used('检查完成',l_time_id);
  
    IF Is_Next = 'N' THEN
      RETURN;
    END IF;
  
    -------------------------------------- 补料的逻辑 --------------------------------------
    l_Count := 0;
    SELECT 'Y' || Cux.Cux_Doc_Line_Seq.Nextval INTO v_Create_Lot FROM Dual;
  
    ----------------------------------------------- 开始预补料 --------------------------------  
    FOR x IN (SELECT Wro.*,
                     Wdj.Attribute3 Ws_Code,
                     Wdj.Primary_Item_Id,
                     Wov.Operation_Code
                FROM Wip_Requirement_Operations_v Wro,
                     Wip_Operations_v             Wov,
                     Wip_Discrete_Jobs            Wdj,
                     Wip_Entities                 We
               WHERE Wdj.Wip_Entity_Id = Wro.Wip_Entity_Id
                 AND Wro.Organization_Id = Wov.Organization_Id(+)
                 AND Wro.Operation_Seq_Num = Wov.Operation_Seq_Num(+)
                 AND Wro.Wip_Entity_Id = Wov.Wip_Entity_Id(+)
                 AND We.Wip_Entity_Id = Wdj.Wip_Entity_Id
                 AND Wro.Organization_Id = p_Organization_Id
                 AND We.Wip_Entity_Name = p_Wip_Name
                 AND Wro.Wip_Supply_Meaning LIKE
                     '%' || Substr(p_Wip_Type, 1, 2) || '%'
                 AND EXISTS
               (SELECT 'x'
                        FROM Cux_Sub_Loc_Set_b Cws
                       WHERE Cws.Organization_Id = Wro.Organization_Id
                         AND Cws.Inventory_Item_Id = Wro.Inventory_Item_Id
                         AND Cws.Ws_Code = Wdj.Attribute3
                         AND (Cws.Sub = p_Sub OR p_Sub IS NULL)
                         AND (Cws.Loc_Code = p_Loc OR p_Loc IS NULL))) LOOP
    
      -- 计算本次的补料总需求数量
      v_Required_Item_Qty := Cux_Wip_Item_Replace_Pkg.Get_Quantity_Required_b(p_Org_Id            => x.Organization_Id,
                                                                              p_Wip_Id            => x.Wip_Entity_Id,
                                                                              p_Item_Id           => x.Inventory_Item_Id,
                                                                              p_Operation_Seq_Num => x.Operation_Seq_Num,
                                                                              p_Now_Qty           => p_Now_Qty);
      -- 已经发料量
      v_Ready_Qty_New := Cux_Wip_Item_Replace_Pkg.Get_Quantity_Issued(p_Org_Id            => x.Organization_Id,
                                                                      p_Wip_Id            => x.Wip_Entity_Id,
                                                                      p_Item_Id           => x.Inventory_Item_Id,
                                                                      p_Operation_Seq_Num => x.Operation_Seq_Num,
                                                                      p_Doc_No            => v_Create_Lot);
      -- 本次已经补料量
      v_Ready_Item_b_Qty := Cux_Wip_Item_Replace_Pkg.Get_Quantity_Issued_b(p_Org_Id            => x.Organization_Id,
                                                                           p_Wip_Id            => x.Wip_Entity_Id,
                                                                           p_Item_Id           => x.Inventory_Item_Id,
                                                                           p_Operation_Seq_Num => x.Operation_Seq_Num,
                                                                           p_Doc_No            => v_Create_Lot);
      -- 物料自己的总下达量
      v_Quantity_Issued := Cux_Wip_Item_Replace_Pkg.Get_Quantity_Issued_z(p_Org_Id            => x.Organization_Id,
                                                                          p_Wip_Id            => x.Wip_Entity_Id,
                                                                          p_Item_Id           => x.Inventory_Item_Id,
                                                                          p_Operation_Seq_Num => x.Operation_Seq_Num);
    
      -- 补料总需求数量 - 已经发料量 - 已经补料量 即 还需要补的数量
      -- 如果还需要补的数量大于本次补料量
      IF v_Required_Item_Qty - v_Ready_Qty_New - v_Ready_Item_b_Qty >
         x.Required_Quantity THEN
      
        v_Ready_Qty_s := x.Required_Quantity - v_Quantity_Issued;
      
      ELSE
        v_Ready_Qty_s := v_Required_Item_Qty - v_Ready_Qty_New -
                         v_Ready_Item_b_Qty;
      
        IF v_Ready_Qty_s > x.Required_Quantity - v_Quantity_Issued THEN
          v_Ready_Qty_s := x.Required_Quantity - v_Quantity_Issued;
        END IF;
      
      END IF;
    
      -- 向上取整
      v_Ready_Qty_s := Ceil(v_Ready_Qty_s);
    
      -- 真正的需要补料的数量
      IF v_Ready_Qty_s > 0 THEN
      
        ------------------------  发料库与货位获取  ------------------------
        --获取默认仓库和货位
      
        BEGIN
          SELECT a.Sub, a.Loc_Code, a.Location_Id
            INTO l_Cux_Ready_Item.Supply_Subinventory,
                 l_Cux_Ready_Item.Supply_Loc_Code,
                 l_Cux_Ready_Item.Supply_Locator_Id
            FROM Cux_Sub_Loc_Set_b a
           WHERE a.Organization_Id = x.Organization_Id
             AND a.Inventory_Item_Id = x.Inventory_Item_Id
             AND a.Ws_Code = x.Ws_Code;
        EXCEPTION
          WHEN OTHERS THEN
          
            select msi.segment1
              into v_temp_item
              from mtl_system_items_b msi
             where msi.organization_id = x.Organization_Id
               and msi.inventory_item_id = x.Inventory_Item_Id;
          
            p_Return_Status := 'E';
            p_Return_Msg    := '默认发料子库设置有误:' || v_temp_item || ' 对应的车间 :' ||
                               x.Ws_Code || ' 未配置或配置多行,请检查!';
            RETURN;
        END;
      
        --如果指定了仓库和货位,则使用指定的
        IF p_Sub IS NOT NULL AND l_Appoint_Sub = 'Y' THEN
          l_Cux_Ready_Item.Supply_Subinventory := p_Sub;
          l_Cux_Ready_Item.Supply_Loc_Code     := NULL;
          l_Cux_Ready_Item.Supply_Locator_Id   := NULL;
        
          -- 指定货位,使用指定的货位
          IF p_Loc IS NOT NULL THEN
            l_Cux_Ready_Item.Supply_Loc_Code := p_Loc;
            SELECT Mil.Inventory_Location_Id
              INTO l_Cux_Ready_Item.Supply_Locator_Id
              FROM Mtl_Item_Locations Mil
             WHERE Mil.Organization_Id = x.Organization_Id
               AND Mil.Subinventory_Code = p_Sub
               AND Mil.Segment1 = p_Loc;
          END IF;
        
        END IF;
      
        ------------------------  接收子库与货位  ------------------------
        -- 拉式补料默认目标仓库为线边仓, 推式料直接补料,无需入库
        IF p_Wip_Type = '拉式补料' THEN
        
          IF p_Organization_Id = 84 THEN
            --福建锐捷
            l_Cux_Ready_Item.To_Sub := 'GX01';
          ELSIF p_Organization_Id = 83 THEN
            --北京锐捷
            l_Cux_Ready_Item.To_Sub := 'HX01';
          END IF;
        
          BEGIN
            SELECT Mil.Inventory_Location_Id, Mil.Segment1
              INTO l_Cux_Ready_Item.To_Locator_Id,
                   l_Cux_Ready_Item.To_Loc_Code
              FROM Mtl_Item_Locations Mil
             WHERE Mil.Organization_Id = p_Organization_Id
               AND Nvl(Mil.Enabled_Flag, 'N') = 'Y'
               AND Mil.Segment1 = p_Wip_Name;
          EXCEPTION
            WHEN OTHERS THEN
              NULL;
          END;
        
        END IF;
      
        -------------------------  可发料量  -------------------------------------
        l_Onhand_Qty := Cux_Wip_Transactions_Pkg.Get_Available_Qty(x.Organization_Id,
                                                                   x.Inventory_Item_Id,
                                                                   l_Cux_Ready_Item.Supply_Subinventory,
                                                                   l_Cux_Ready_Item.Supply_Locator_Id);
      
        -- TODO: 测试,指定现有量为0                                         
        -- l_Onhand_Qty := 0;
      
        Add_Available_Msg(p_organization_id   => x.organization_id,
                          p_inventory_item_id => x.inventory_item_id,
                          p_sub               => l_Cux_Ready_Item.Supply_Subinventory,
                          p_loc               => l_Cux_Ready_Item.Supply_Loc_Code,
                          p_Onhand_Qty        => l_Onhand_Qty,
                          x_onhand_msg        => v_onhand_msg,
                          x_onhand_count      => v_onhand_count);
        l_Ready_Qty := 0;
      
        --现有量取最小整数
        IF l_Onhand_Qty >= v_Ready_Qty_s THEN
          l_Ready_Qty := v_Ready_Qty_s;
        ELSIF l_Onhand_Qty > 0 THEN
          l_Ready_Qty := l_Onhand_Qty;
        ELSE
          l_Ready_Qty := 0;
        END IF;
      
        l_Cux_Ready_Item.Line_Id         := Cux.Cux_Doc_Line_Seq.Nextval;
        l_Cux_Ready_Item.Organization_Id := x.Organization_Id;
        l_Cux_Ready_Item.Doc_Type        := p_Wip_Type;
        l_Cux_Ready_Item.Doc_Date        := p_Doc_Date;
        l_Cux_Ready_Item.Doc_Status      := '未过账';
        l_Cux_Ready_Item.Wip_Entity_Id   := x.Wip_Entity_Id;
        l_Cux_Ready_Item.Wip_Entity_Name := p_Wip_Name;
        l_Cux_Ready_Item.Primary_Item_Id := x.Primary_Item_Id;
      
        l_Cux_Ready_Item.Ready_Qty     := l_Ready_Qty;
        l_Cux_Ready_Item.Now_Qty       := p_Now_Qty;
        l_Cux_Ready_Item.Iss_Ready_Qty := v_Ready_Qty_s;
      
        l_Cux_Ready_Item.Inventory_Item_Id      := x.Inventory_Item_Id;
        l_Cux_Ready_Item.Item_Primary_Uom_Code  := x.Item_Primary_Uom_Code;
        l_Cux_Ready_Item.Operation_Seq_Num      := x.Operation_Seq_Num;
        l_Cux_Ready_Item.Operation_Code         := x.Operation_Code;
        l_Cux_Ready_Item.Quantity_Per_Assembly  := x.Quantity_Per_Assembly;
        l_Cux_Ready_Item.Component_Yield_Factor := x.Component_Yield_Factor;
        l_Cux_Ready_Item.Required_Quantity      := x.Required_Quantity;
        l_Cux_Ready_Item.Quantity_Issued        := x.Quantity_Issued;
        l_Cux_Ready_Item.Creation_Date          := SYSDATE;
        l_Cux_Ready_Item.Created_By             := Fnd_Global.User_Id;
        l_Cux_Ready_Item.Last_Update_Date       := SYSDATE;
        l_Cux_Ready_Item.Last_Updated_By        := Fnd_Global.User_Id;
        l_Cux_Ready_Item.Last_Update_Login      := Fnd_Global.Login_Id;
        l_Cux_Ready_Item.Create_Lot             := v_Create_Lot;
        l_Cux_Ready_Item.Doc_No                 := v_Create_Lot;
        l_Cux_Ready_Item.Material_Batch1        := x.attribute1;
        l_Cux_Ready_Item.Material_Batch2        := x.attribute2;
        l_Cux_Ready_Item.Special_Number         := x.attribute4;
        l_Cux_Ready_Item.Special_Order          := x.attribute3;
        l_Cux_Ready_Item.Empty_Return_Flag      := p_param.EMPTY_RETURN_FLAG;
      
        IF l_Cux_Ready_Item.Ready_Qty > 0 THEN
          INSERT INTO Cux_Ready_Item VALUES l_Cux_Ready_Item;
          ----开始保留
        
          Cux_Wip_Transactions_Pkg.Create_Save(l_Cux_Ready_Item.Line_Id,
                                               p_Return_Status,
                                               p_Return_Msg);
        
          IF p_Return_Status <> 'S' THEN
            RETURN;
          END IF;
        END IF;
      
      END IF;
    END LOOP;
  
    ----------------------------------------------预备料完成------------------------------------------
  
    SELECT 'X' || Cux.Cux_Doc_Line_Seq.Nextval
      INTO v_Create_Lot_New
      FROM Dual;
  
    ----------------------------------------------开始关联备料单-----------------------------------
    -- 将可以进行补料的补料单关联到对应的备料单上,上预补料的单再次进行拆单.进行对应
    FOR x IN (SELECT *
                FROM Cux_Ready_Item a
               WHERE a.Doc_No = v_Create_Lot
               ORDER BY a.Operation_Seq_Num, a.Supply_Subinventory) LOOP
    
      ------判断是否是材料仓库
      -------材料仓补料单需要关联备料单
      -------成品仓补料单不需要关联备料单
      BEGIN
        SELECT Nvl(Msa.Attribute4, 'N')
          INTO v_Flag
          FROM --Mtl_Subinventories_All_v Msa
               mtl_secondary_inventories msa
         WHERE Msa.Organization_Id = x.Organization_Id
           AND Msa.Secondary_Inventory_Name = x.Supply_Subinventory;
      EXCEPTION
        WHEN OTHERS THEN
          v_Flag := 'N';
      END;
    
      IF v_Flag = 'N' THEN
        -- 本次补料数量
        v_Qty := x.Ready_Qty;
      
        FOR y IN (SELECT *
                    FROM Cux_Ready_Item Cri
                   WHERE Cri.Organization_Id = x.Organization_Id
                     AND Cri.Wip_Entity_Name = x.Wip_Entity_Name
                     AND Cri.Operation_Seq_Num = x.Operation_Seq_Num
                     AND Cri.Supply_Subinventory = x.Supply_Subinventory
                     AND Cri.Inventory_Item_Id = x.Inventory_Item_Id
                     AND Cri.Doc_Type LIKE '%备料%') LOOP
        
          -- 备料单的需求数量
          v_Required_Item_Qty := Cux_Wip_Item_Replace_Pkg.Get_Quantity_Required_b(p_Org_Id            => x.Organization_Id,
                                                                                  p_Wip_Id            => x.Wip_Entity_Id,
                                                                                  p_Item_Id           => x.Inventory_Item_Id,
                                                                                  p_Operation_Seq_Num => x.Operation_Seq_Num,
                                                                                  p_Now_Qty           => y.Now_Qty);
          -- 备料单已经备料数量量
          v_Ready_Item_b_Qty := Cux_Wip_Item_Replace_Pkg.Get_Quantity_Issued_b(p_Org_Id            => y.Organization_Id,
                                                                               p_Wip_Id            => y.Wip_Entity_Id,
                                                                               p_Item_Id           => y.Inventory_Item_Id,
                                                                               p_Operation_Seq_Num => y.Operation_Seq_Num,
                                                                               p_Doc_No            => y.Doc_No);
          -- 备料单已经补料的数量
          SELECT Nvl(SUM(Cri.Ready_Qty), 0)
            INTO v_Ready_Qty_b
            FROM Cux_Ready_Item Cri
           WHERE Cri.Wip_Entity_Id = y.Wip_Entity_Id
             AND Cri.Organization_Id = y.Organization_Id
             AND Cri.Operation_Seq_Num = y.Operation_Seq_Num
             AND Cri.Inventory_Item_Id = y.Inventory_Item_Id
             AND Cri.Attribute10 = y.Doc_No
             AND Nvl(Cri.Attribute11, 0) <> -0.0001;
        
          v_temp_qty := v_Required_Item_Qty - v_Ready_Item_b_Qty -
                        v_Ready_Qty_b;
          -- 备料数 >= 备料单的需求数量 -备料单已经备料数量量 - 备料单已经补料的数量
          -- 如果小于0 表示该备料单已经备齐,无须关联补料单
          IF v_temp_qty > 0 THEN
          
            -- 当前补料数量大于备料单需要补料数量。分到多个补料单（备料多次，一次补料时可能发生）
          
            IF v_Qty >= v_temp_qty THEN
            
              l_Cux_Ready_Item.Line_Id                := Cux.Cux_Doc_Line_Seq.Nextval;
              l_Cux_Ready_Item.Organization_Id        := y.Organization_Id;
              l_Cux_Ready_Item.Doc_Type               := p_Wip_Type;
              l_Cux_Ready_Item.Doc_Date               := p_Doc_Date;
              l_Cux_Ready_Item.Doc_Status             := '未过账';
              l_Cux_Ready_Item.Wip_Entity_Id          := y.Wip_Entity_Id;
              l_Cux_Ready_Item.Wip_Entity_Name        := p_Wip_Name;
              l_Cux_Ready_Item.Primary_Item_Id        := y.Primary_Item_Id;
              l_Cux_Ready_Item.Inventory_Item_Id      := y.Inventory_Item_Id;
              l_Cux_Ready_Item.Item_Primary_Uom_Code  := y.Item_Primary_Uom_Code;
              l_Cux_Ready_Item.Operation_Seq_Num      := y.Operation_Seq_Num;
              l_Cux_Ready_Item.Operation_Code         := y.Operation_Code;
              l_Cux_Ready_Item.Quantity_Per_Assembly  := y.Quantity_Per_Assembly;
              l_Cux_Ready_Item.Component_Yield_Factor := y.Component_Yield_Factor;
              l_Cux_Ready_Item.Required_Quantity      := y.Required_Quantity;
              l_Cux_Ready_Item.To_Sub                 := x.To_Sub;
              l_Cux_Ready_Item.Supply_Subinventory    := x.Supply_Subinventory;
              l_Cux_Ready_Item.Supply_Locator_Id      := x.Supply_Locator_Id;
              l_Cux_Ready_Item.Supply_Loc_Code        := x.Supply_Loc_Code;
            
              l_Cux_Ready_Item.Quantity_Issued := y.Quantity_Issued;
              l_Cux_Ready_Item.Now_Qty         := p_Now_Qty;
              l_Cux_Ready_Item.Iss_Ready_Qty   := v_temp_qty;
              l_Cux_Ready_Item.Ready_Qty       := substr(v_temp_qty,0,40);
            
              l_Cux_Ready_Item.Creation_Date     := SYSDATE;
              l_Cux_Ready_Item.Created_By        := Fnd_Global.User_Id;
              l_Cux_Ready_Item.Last_Update_Date  := SYSDATE;
              l_Cux_Ready_Item.Last_Updated_By   := Fnd_Global.User_Id;
              l_Cux_Ready_Item.Last_Update_Login := Fnd_Global.Login_Id;
              l_Cux_Ready_Item.Create_Lot        := v_Create_Lot_New;
              l_Cux_Ready_Item.Doc_No            := v_Create_Lot_New;
              l_Cux_Ready_Item.Attribute10       := y.Doc_No;
              l_Cux_Ready_Item.Attribute11       := v_Qty;
              v_Doc_No                           := y.Doc_No; -- 绑定单号
              
              l_Cux_Ready_Item.Material_Batch1   := x.Material_Batch1;
              l_Cux_Ready_Item.Material_Batch2   := x.Material_Batch2;
              l_Cux_Ready_Item.Special_Number    := x.Special_Number;
              l_Cux_Ready_Item.Special_Order     := x.Special_Order;
              l_Cux_Ready_Item.Empty_Return_Flag := x.empty_return_flag;
              
              v_Qty := v_Qty - v_temp_qty;
            ELSE
            
              l_Cux_Ready_Item.Line_Id                := Cux.Cux_Doc_Line_Seq.Nextval;
              l_Cux_Ready_Item.Organization_Id        := y.Organization_Id;
              l_Cux_Ready_Item.Doc_Type               := p_Wip_Type;
              l_Cux_Ready_Item.Doc_Date               := p_Doc_Date;
              l_Cux_Ready_Item.Doc_Status             := '未过账';
              l_Cux_Ready_Item.Wip_Entity_Id          := y.Wip_Entity_Id;
              l_Cux_Ready_Item.Wip_Entity_Name        := p_Wip_Name;
              l_Cux_Ready_Item.Primary_Item_Id        := y.Primary_Item_Id;
              l_Cux_Ready_Item.Inventory_Item_Id      := y.Inventory_Item_Id;
              l_Cux_Ready_Item.Item_Primary_Uom_Code  := y.Item_Primary_Uom_Code;
              l_Cux_Ready_Item.Operation_Seq_Num      := y.Operation_Seq_Num;
              l_Cux_Ready_Item.Operation_Code         := y.Operation_Code;
              l_Cux_Ready_Item.Quantity_Per_Assembly  := y.Quantity_Per_Assembly;
              l_Cux_Ready_Item.Component_Yield_Factor := y.Component_Yield_Factor;
              l_Cux_Ready_Item.Required_Quantity      := y.Required_Quantity;
              l_Cux_Ready_Item.To_Sub                 := x.To_Sub;
              l_Cux_Ready_Item.Supply_Subinventory    := x.Supply_Subinventory;
              l_Cux_Ready_Item.Supply_Locator_Id      := x.Supply_Locator_Id;
              l_Cux_Ready_Item.Supply_Loc_Code        := x.Supply_Loc_Code;
            
              l_Cux_Ready_Item.Quantity_Issued := y.Quantity_Issued;
              l_Cux_Ready_Item.Iss_Ready_Qty   := v_Qty;
              l_Cux_Ready_Item.Ready_Qty       := v_Qty;
              l_Cux_Ready_Item.Now_Qty         := p_Now_Qty;
            
              l_Cux_Ready_Item.Creation_Date     := SYSDATE;
              l_Cux_Ready_Item.Created_By        := Fnd_Global.User_Id;
              l_Cux_Ready_Item.Last_Update_Date  := SYSDATE;
              l_Cux_Ready_Item.Last_Updated_By   := Fnd_Global.User_Id;
              l_Cux_Ready_Item.Last_Update_Login := Fnd_Global.Login_Id;
              l_Cux_Ready_Item.Create_Lot        := v_Create_Lot_New;
              l_Cux_Ready_Item.Doc_No            := v_Create_Lot_New;
              l_Cux_Ready_Item.Attribute10       := y.Doc_No;
              l_Cux_Ready_Item.Attribute11       := v_Qty;
              v_Doc_No                           := y.Doc_No;
              v_Qty                              := 0;
              l_Cux_Ready_Item.Material_Batch1   := x.Material_Batch1;
              l_Cux_Ready_Item.Material_Batch2   := x.Material_Batch2;
              l_Cux_Ready_Item.Special_Number    := x.Special_Number;
              l_Cux_Ready_Item.Special_Order     := x.Special_Order;
              l_Cux_Ready_Item.Empty_Return_Flag := x.empty_return_flag;
              
            END IF;
          
            -- 有补料数量,插入对应值
            IF l_Cux_Ready_Item.Ready_Qty > 0 THEN
              INSERT INTO Cux_Ready_Item VALUES l_Cux_Ready_Item;
            END IF;
          END IF;
        END LOOP;
      
        -- 未关联到订单,则根据一定规则进行关联
        IF v_Qty > 0 THEN
          -- 根据新逻辑获取订单编号 v_doc_no
        
          -- 1. 根据工序 , 子库进行关联      
          SELECT MIN(a.Doc_No)
            INTO v_Doc_No
            FROM Cux_Ready_Item a
           WHERE a.Wip_Entity_Id = x.Wip_Entity_Id
             AND a.Organization_Id = x.Organization_Id
             AND a.Operation_Seq_Num = x.Operation_Seq_Num
             AND a.Supply_Subinventory = x.Supply_Subinventory
             AND Substr(a.Doc_Type, 1, 2) = Substr(p_Wip_Type, 1, 2)
             AND a.Doc_Type IN ('推式备料', '拉式备料');
        
          -- 2. 不存在,则根据工序进行关联
          IF v_Doc_No IS NULL THEN
            SELECT MIN(a.Doc_No)
              INTO v_Doc_No
              FROM Cux_Ready_Item a
             WHERE a.Wip_Entity_Id = x.Wip_Entity_Id
               AND a.Organization_Id = x.Organization_Id
               AND a.Operation_Seq_Num = x.Operation_Seq_Num
                  -- AND a.supply_subinventory = x.supply_subinventory
               AND Substr(a.Doc_Type, 1, 2) = Substr(p_Wip_Type, 1, 2)
               AND a.Doc_Type IN ('推式备料', '拉式备料');
          END IF;
        
          --3. 不存在,则根据子库进行关联  
          IF v_Doc_No IS NULL THEN
            SELECT MIN(a.Doc_No)
              INTO v_Doc_No
              FROM Cux_Ready_Item a
             WHERE a.Wip_Entity_Id = x.Wip_Entity_Id
               AND a.Organization_Id = x.Organization_Id
                  --  AND a.operation_seq_num = x.operation_seq_num
               AND a.Supply_Subinventory = x.Supply_Subinventory
               AND Substr(a.Doc_Type, 1, 2) = Substr(p_Wip_Type, 1, 2)
               AND a.Doc_Type IN ('推式备料', '拉式备料');
          END IF;
        
          --4. 工序子库都不存在,直接通过备补单进行关联
          IF v_Doc_No IS NULL THEN
            SELECT MIN(a.Doc_No)
              INTO v_Doc_No
              FROM Cux_Ready_Item a
             WHERE a.Wip_Entity_Id = x.Wip_Entity_Id
               AND a.Organization_Id = x.Organization_Id
                  --  AND a.operation_seq_num = x.operation_seq_num
                  --  AND a.supply_subinventory = x.supply_subinventory
               AND Substr(a.Doc_Type, 1, 2) = Substr(p_Wip_Type, 1, 2)
               AND a.Doc_Type IN ('推式备料', '拉式备料');
          END IF;
        
          l_Cux_Ready_Item.Line_Id                := Cux.Cux_Doc_Line_Seq.Nextval;
          l_Cux_Ready_Item.Organization_Id        := x.Organization_Id;
          l_Cux_Ready_Item.Doc_Type               := p_Wip_Type;
          l_Cux_Ready_Item.Doc_Date               := p_Doc_Date;
          l_Cux_Ready_Item.Doc_Status             := '未过账';
          l_Cux_Ready_Item.Wip_Entity_Id          := x.Wip_Entity_Id;
          l_Cux_Ready_Item.Wip_Entity_Name        := p_Wip_Name;
          l_Cux_Ready_Item.Primary_Item_Id        := x.Primary_Item_Id;
          l_Cux_Ready_Item.Now_Qty                := p_Now_Qty;
          l_Cux_Ready_Item.Inventory_Item_Id      := x.Inventory_Item_Id;
          l_Cux_Ready_Item.Item_Primary_Uom_Code  := x.Item_Primary_Uom_Code;
          l_Cux_Ready_Item.Operation_Seq_Num      := x.Operation_Seq_Num;
          l_Cux_Ready_Item.Operation_Code         := x.Operation_Code;
          l_Cux_Ready_Item.Quantity_Per_Assembly  := x.Quantity_Per_Assembly;
          l_Cux_Ready_Item.Component_Yield_Factor := x.Component_Yield_Factor;
          l_Cux_Ready_Item.Required_Quantity      := x.Required_Quantity;
          l_Cux_Ready_Item.To_Sub                 := x.To_Sub;
          l_Cux_Ready_Item.Supply_Subinventory    := x.Supply_Subinventory;
          l_Cux_Ready_Item.Supply_Locator_Id      := x.Supply_Locator_Id;
          l_Cux_Ready_Item.Supply_Loc_Code        := x.Supply_Loc_Code;
          l_Cux_Ready_Item.Quantity_Issued        := x.Quantity_Issued;
          --l_cux_ready_item.iss_ready_qty          := x.iss_ready_qty;     -- 补料数量
          l_Cux_Ready_Item.Iss_Ready_Qty     := v_Qty;
          l_Cux_Ready_Item.Creation_Date     := SYSDATE;
          l_Cux_Ready_Item.Ready_Qty         := v_Qty; -- 补料数量
          l_Cux_Ready_Item.Created_By        := Fnd_Global.User_Id;
          l_Cux_Ready_Item.Last_Update_Date  := SYSDATE;
          l_Cux_Ready_Item.Last_Updated_By   := Fnd_Global.User_Id;
          l_Cux_Ready_Item.Last_Update_Login := Fnd_Global.Login_Id;
          l_Cux_Ready_Item.Create_Lot        := v_Create_Lot_New;
          l_Cux_Ready_Item.Doc_No            := v_Create_Lot_New;
          l_Cux_Ready_Item.Attribute10       := v_Doc_No;
          l_Cux_Ready_Item.Attribute11       := -0.0001;
        
          l_Cux_Ready_Item.Material_Batch1   := x.Material_Batch1;
          l_Cux_Ready_Item.Material_Batch2   := x.Material_Batch2;
          l_Cux_Ready_Item.Special_Number    := x.Special_Number;
          l_Cux_Ready_Item.Special_Order     := x.Special_Order;
          l_Cux_Ready_Item.Empty_Return_Flag := x.empty_return_flag;
              
          INSERT INTO Cux_Ready_Item VALUES l_Cux_Ready_Item;
        
        END IF;
      ELSE
        -- 成品
        v_Qty                                   := x.Ready_Qty;
        l_Cux_Ready_Item.Line_Id                := Cux.Cux_Doc_Line_Seq.Nextval;
        l_Cux_Ready_Item.Organization_Id        := x.Organization_Id;
        l_Cux_Ready_Item.Doc_Type               := p_Wip_Type;
        l_Cux_Ready_Item.Doc_Date               := p_Doc_Date;
        l_Cux_Ready_Item.Doc_Status             := '未过账';
        l_Cux_Ready_Item.Wip_Entity_Id          := x.Wip_Entity_Id;
        l_Cux_Ready_Item.Wip_Entity_Name        := p_Wip_Name;
        l_Cux_Ready_Item.Primary_Item_Id        := x.Primary_Item_Id;
        l_Cux_Ready_Item.Now_Qty                := p_Now_Qty;
        l_Cux_Ready_Item.Inventory_Item_Id      := x.Inventory_Item_Id;
        l_Cux_Ready_Item.Item_Primary_Uom_Code  := x.Item_Primary_Uom_Code;
        l_Cux_Ready_Item.Operation_Seq_Num      := x.Operation_Seq_Num;
        l_Cux_Ready_Item.Operation_Code         := x.Operation_Code;
        l_Cux_Ready_Item.Quantity_Per_Assembly  := x.Quantity_Per_Assembly;
        l_Cux_Ready_Item.Component_Yield_Factor := x.Component_Yield_Factor;
        l_Cux_Ready_Item.Required_Quantity      := x.Required_Quantity;
        l_Cux_Ready_Item.To_Sub                 := x.To_Sub;
        l_Cux_Ready_Item.Supply_Subinventory    := x.Supply_Subinventory;
        l_Cux_Ready_Item.Supply_Locator_Id      := x.Supply_Locator_Id;
        l_Cux_Ready_Item.Supply_Loc_Code        := x.Supply_Loc_Code;
        l_Cux_Ready_Item.Quantity_Issued        := x.Quantity_Issued;
        l_Cux_Ready_Item.Iss_Ready_Qty          := x.Iss_Ready_Qty;
        l_Cux_Ready_Item.Creation_Date          := SYSDATE;
        l_Cux_Ready_Item.Ready_Qty              := v_Qty;
        l_Cux_Ready_Item.Created_By             := Fnd_Global.User_Id;
        l_Cux_Ready_Item.Last_Update_Date       := SYSDATE;
        l_Cux_Ready_Item.Last_Updated_By        := Fnd_Global.User_Id;
        l_Cux_Ready_Item.Last_Update_Login      := Fnd_Global.Login_Id;
        l_Cux_Ready_Item.Create_Lot             := v_Create_Lot_New;
        l_Cux_Ready_Item.Doc_No                 := v_Create_Lot_New;
        l_Cux_Ready_Item.Attribute11            := v_Qty;
        l_Cux_Ready_Item.Attribute10            := NULL; ---备料单号
      
        l_Cux_Ready_Item.Material_Batch1   := x.Material_Batch1;
        l_Cux_Ready_Item.Material_Batch2   := x.Material_Batch2;
        l_Cux_Ready_Item.Special_Number    := x.Special_Number;
        l_Cux_Ready_Item.Special_Order     := x.Special_Order;
        l_Cux_Ready_Item.Empty_Return_Flag := x.empty_return_flag;      
        INSERT INTO Cux_Ready_Item VALUES l_Cux_Ready_Item;
      END IF;
    END LOOP;
    ---------------------------------------------备料单分配完成---------------------------
  
    ----------------------------------------更新补料单号----------------------------------
    SELECT To_Char(Systimestamp, 'YYYYMMDDHH24MISSFF4')
      INTO v_Create_Lot_1
      FROM Dual;
    l_lot := v_Create_Lot_1;
    FOR x IN (SELECT DISTINCT a.Wip_Entity_Name,
                              a.Supply_Subinventory,
                              a.Operation_Seq_Num,
                              a.Organization_Id,
                              Nvl(a.Attribute10, 'ASDFG') Attribute10 -- 关联备料单
                FROM Cux_Ready_Item a
               WHERE a.Create_Lot = v_Create_Lot_New) LOOP
    
      IF x.Organization_Id = 84 THEN
        v_Doc_No := 'AD' || Cux_Doc_Add_Seq_Fw.Nextval;
      ELSE
        v_Doc_No := 'AD' || Cux_Doc_Add_Seq_Jw.Nextval;
      END IF;
    
      UPDATE Cux_Ready_Item a
         SET a.Doc_No          = v_Doc_No,
             a.Create_Lot      = v_Create_Lot_1,
             a.transfer_status = 'WAITING'
       WHERE a.Create_Lot = v_Create_Lot_New
         AND a.Operation_Seq_Num = x.Operation_Seq_Num
         AND a.Supply_Subinventory = x.Supply_Subinventory
         AND a.Organization_Id = x.Organization_Id
         AND a.Wip_Entity_Name = x.Wip_Entity_Name
         AND Nvl(a.Attribute10, 'ASDFG') = x.Attribute10;
    END LOOP;
    --added by bruce on 20151130
    --l_line_number    := 0;
    for rec_upd_line_number in (select cri.line_id, cri.doc_no
                                  from cux_ready_item cri
                                 where cri.create_lot = v_Create_Lot_1
                                 order by cri.doc_no) loop
    
      if (l_old_doc_number is null or
         l_old_doc_number <> rec_upd_line_number.doc_no) then
      
        l_old_doc_number := rec_upd_line_number.doc_no;
        l_line_number    := 0;
      
      end if;
      l_line_number := l_line_number + 1;
      update cux_ready_item cri
         set cri.line_number = l_line_number
       where cri.Line_Id = rec_upd_line_number.line_id;
    
    end loop;
    --end of added by bruce on 20151130
    -------------------------删除预补料的物料占用-----------------------------
    DELETE FROM Mtl_Reservations a
     WHERE a.Supply_Source_Name = v_Create_Lot;
  
    -- 删除有可能导致上面删除保留失败 
    l_onhand_qty := cux_wip_transactions_pkg.get_available_qty(84,
                                                               18852,
                                                               'G106',
                                                               16055);
    dbms_output.put_line(l_onhand_qty);
  
    -- 删除预预料单
    DELETE FROM Cux_Ready_Item a WHERE a.Create_Lot = v_Create_Lot;
  
    ---------------------------------------开始物料占用-------------------------
    l_Count := 0;
    FOR x IN (SELECT a.Line_Id
                FROM Cux_Ready_Item a
               WHERE a.Create_Lot = v_Create_Lot_1
                 AND a.Ready_Qty <> 0) LOOP
    
      Cux_Wip_Transactions_Pkg.Create_Save(x.Line_Id,
                                           p_Return_Status,
                                           p_Return_Msg);
    
      IF p_Return_Status <> 'S' THEN
        RETURN;
      END IF;
    
      l_Count := l_Count + 1;
    END LOOP;
  
    COMMIT;
    p_Return_Status := 'S';
    p_Return_Msg    := '下达成功';
  
    IF l_Count = 0 THEN
      p_Return_Status := 'E';
      p_Return_Msg    := '无明细数据或无库存数量,单据未生成！';
    
      -- 现有量不足提示
      if v_onhand_count > 0 then
        p_Return_Msg := '存在' || v_onhand_count || '个物料对应的仓库货物无可用量:' ||
                        v_onhand_msg;
      end if;
    END IF;
  
    --added by bruce on 20151028
  
    if (p_Return_Status = 'S') then
    
      process_full_prepared(p_lot             => '',
                            p_organization_id => p_Organization_Id,
                            p_wip_id          => p_wip_id,
                            p_wip_name        => p_Wip_Name,
                            p_Return_Status   => p_Return_Status,
                            p_Return_Msg      => p_Return_Msg);
    
      if (p_Return_Status = 'S') then
      
        transfer_to_wms(p_doc_no          => '',
                        p_organization_id => p_Organization_Id,
                        p_doc_type        => '',
                        p_lot             => l_lot,
                        p_Return_Status   => p_Return_Status,
                        p_Return_Msg      => p_Return_Msg);
      
      end if;
    end if;
  
    --end of add by bruce on 20151028
  
    -- time_mark : 添加记录时间代码
    -- Cux_Time_Used_Pkg.add_time_used('补料完成',l_time_id);
  END;

  PROCEDURE Create_Feeding_Doc_Ap_Sub(p_Organization_Id NUMBER,
                                      p_wip_id          number,
                                      p_Wip_Name        VARCHAR2,
                                      p_Wip_Type        VARCHAR2,
                                      p_Doc_Date        DATE,
                                      p_Now_Qty         NUMBER,
                                      p_Sub             VARCHAR2,
                                      p_Loc             VARCHAR2,
                                      p_Operation_Code  VARCHAR2,
                                      p_param           CUX_DOC_CLASS_TYPE,
                                      p_Return_Status   OUT VARCHAR2,
                                      p_Return_Msg      OUT VARCHAR2) AS
    l_lot varchar2(100);
    CURSOR c_Line IS
      SELECT a.Organization_Id,
             a.Wip_Entity_Id,
             a.Wip_Entity_Name,
             a.Primary_Item_Id,
             a.Ws_Code, --车间
             a.Start_Quantity,
             a.Inventory_Item_Id,
             a.Com_Item_Code,
             a.Item_Primary_Uom_Code,
             a.Operation_Seq_Num,
             a.Operation_Code,
             a.Quantity_Per_Assembly,
             a.Component_Yield_Factor,
             a.Required_Quantity,
             a.Quantity_Issued,
             a.Iss_Ready_Qty,
             Cws.Sub,
             Cws.Loc_Code Loc,
             Cws.Location_Id Inventory_Location_Id,
             To_Char(Systimestamp, 'YYYYMMDDHH24MISSFF4') Create_Lot,
             a.Replace_Flag
        FROM (SELECT Wdj.Organization_Id,
                     Wdj.Wip_Entity_Id,
                     Wdj.Wip_Entity_Name,
                     Wdj.Primary_Item_Id,
                     Wdj.Attribute3 Ws_Code, --车间
                     Wdj.Start_Quantity,
                     Wro.Inventory_Item_Id,
                     Msi.Segment1 Com_Item_Code,
                     Wro.Item_Primary_Uom_Code,
                     Wro.Operation_Seq_Num,
                     Wov.Operation_Code,
                     Wro.Quantity_Per_Assembly,
                     Wro.Component_Yield_Factor,
                     Wro.Required_Quantity,
                     Wro.Quantity_Issued,
                     Ceil(Wro.Quantity_Per_Assembly * p_Now_Qty /
                          Cux_Wip_Transactions_Pkg.Get_Component_Yield_Factor(p_Org_Id                 => Wro.Organization_Id,
                                                                              p_Wip_Id                 => Wro.Wip_Entity_Id,
                                                                              p_Operation_Seq_Num      => Wro.Operation_Seq_Num,
                                                                              p_Item_Id                => Wro.Inventory_Item_Id,
                                                                              p_Component_Yield_Factor => Wro.Component_Yield_Factor)) -
                     (SELECT Nvl(SUM(Nvl(Decode(Cri.Doc_Type,
                                                '拉式退料',
                                                Cri.Ready_Qty * -1,
                                                '推式退料(负单)',
                                                Cri.Ready_Qty * -1,
                                                '推式退料(零单)',
                                                Cri.Ready_Qty * -1,
                                                Cri.Ready_Qty),
                                         0)),
                                 0)
                        FROM Cux_Ready_Item Cri
                       WHERE Cri.Organization_Id = Wro.Organization_Id
                         AND Cri.Wip_Entity_Id = Wro.Wip_Entity_Id
                         AND Cri.Inventory_Item_Id = Wro.Inventory_Item_Id
                         AND Cri.Operation_Seq_Num = Wro.Operation_Seq_Num
                         AND Cri.Doc_Type NOT LIKE '%超领%') Iss_Ready_Qty,
                     Cux_Wip_Transactions_Pkg.Get_Replace_Item(p_Org_Id  => Wro.Organization_Id,
                                                               p_Wip_Id  => Wro.Wip_Entity_Id,
                                                               p_Item_Id => Wro.Inventory_Item_Id) Replace_Flag
                FROM Wip_Discrete_Jobs_v          Wdj,
                     Wip_Requirement_Operations_v Wro,
                     Wip_Operations_v             Wov,
                     Mtl_System_Items_Vl          Msi
               WHERE 1 = 1
                 AND Wdj.Organization_Id = Wro.Organization_Id
                 AND Wdj.Wip_Entity_Id = Wro.Wip_Entity_Id
                 AND Wro.Organization_Id = Wov.Organization_Id(+)
                 AND Wro.Operation_Seq_Num = Wov.Operation_Seq_Num(+)
                 AND Wro.Wip_Entity_Id = Wov.Wip_Entity_Id(+)
                 AND Msi.Organization_Id = Wro.Organization_Id
                 AND Msi.Inventory_Item_Id = Wro.Inventory_Item_Id
                    
                 AND Wdj.Organization_Id = p_Organization_Id
                 AND wdj.wip_entity_id = p_wip_id
                 AND Nvl(Wro.Repetitive_Schedule_Id, '-1') =
                     Nvl(Wov.Repetitive_Schedule_Id, '-1')
                 AND Wro.Wip_Supply_Meaning LIKE
                     '%' || Substr(p_Wip_Type, 1, 2) || '%') a,
             Cux_Sub_Loc_Set_b Cws
       WHERE 1 = 1
         AND Cws.Organization_Id(+) = a.Organization_Id
         AND Cws.Inventory_Item_Id(+) = a.Inventory_Item_Id
         AND Cws.Ws_Code(+) = a.Ws_Code
         AND Nvl(Cws.Sub, '1') = Nvl(p_Sub, Nvl(Cws.Sub, '1'))
         AND Nvl(Cws.Loc_Code, '1') = Nvl(p_Loc, Nvl(Cws.Loc_Code, '1'))
         AND a.Iss_Ready_Qty > 0;
  
    l_cux_ready_item    cux.cux_ready_item%ROWTYPE;
    l_count             NUMBER := 0;
    is_next             VARCHAR2(1) := 'Y';
    l_to_sub            VARCHAR2(100);
    is_have             NUMBER;
    l_ready_qty         NUMBER;
    l_onhand_qty        NUMBER;
    l_is_loc_con        NUMBER := 0;
    v_flag              VARCHAR2(10);
    v_qty               NUMBER;
    l_old_doc_number    varchar2(100);
    l_line_number       number;
    v_create_lot        VARCHAR2(100);
    v_create_lot_new    VARCHAR2(100);
    v_ready_qty_new     NUMBER;
    v_required_item_qty NUMBER;
    v_ready_item_b_qty  NUMBER;
    v_ready_qty_b       NUMBER;
    v_ready_qty_s       NUMBER;
    v_quantity_issued   NUMBER;
    v_doc_no            VARCHAR2(100);
    v_create_lot_1      VARCHAR2(100);
    v_temp_item         VARCHAR2(100);
  
    v_onhand_msg   varchar2(2000);
    v_onhand_count number default 0;
  BEGIN
    -- time_mark : 添加记录时间代码
    --Cux_Time_Used_Pkg.create_time_used('新仓库指定下达,类型 : ' || p_Wip_Type  
    --                                    || ' 工单号 : '|| p_Wip_Name ,l_time_id);  
  
    IF p_sub IS NULL THEN
      p_return_status := 'E';
      p_return_msg    := '未指定仓库';
      RETURN;
    END IF;
  
    IF p_sub IS NOT NULL THEN
      BEGIN
        SELECT locator_type
          INTO l_is_loc_con
          FROM mtl_secondary_inventories_fk_v msi
         WHERE msi.secondary_inventory_name = p_sub
           AND msi.organization_id = p_organization_id;
      EXCEPTION
        WHEN OTHERS THEN
          l_is_loc_con := 0;
      END;
      IF l_is_loc_con <> 1 AND p_loc IS NULL THEN
        p_return_status := 'E';
        p_return_msg    := '仓库有货位,未指定货位';
        RETURN;
      END IF;
    
      --判断目标子存和货位
      IF p_wip_type = '拉式补料' THEN
        IF p_organization_id = 84 THEN
          --福建锐捷
          l_to_sub := 'GX01';
        ELSIF p_organization_id = 83 THEN
          --北京锐捷
          l_to_sub := 'HX01';
        END IF;
      
        SELECT COUNT(1)
          INTO is_have
          FROM mtl_item_locations mil
         WHERE mil.organization_id = p_organization_id
           AND nvl(mil.enabled_flag, 'N') = 'Y'
           AND mil.subinventory_code = l_to_sub
           AND mil.segment1 = p_wip_name;
      
        IF is_have < 1 THEN
          is_next         := 'N';
          p_return_status := 'E';
          p_return_msg    := p_return_msg || '单据下达失败,组件子库[' || l_to_sub ||
                             ']或货位[' || p_wip_name || ']为空,请检查:' || chr(10);
          RETURN;
        END IF;
      END IF;
    
      -- time_mark : 添加记录时间代码
      -- Cux_Time_Used_Pkg.add_time_used('校验完成',l_time_id);
    
      IF is_next = 'N' THEN
        p_return_status := 'E';
        p_return_msg    := '校验不通过';
        RETURN;
      END IF;
    
      --补料的逻辑 
      l_count := 0;
    
      SELECT 'Y' || cux.cux_doc_line_seq.nextval
        INTO v_create_lot
        FROM dual;
    
      -----------------------------------------------开始预补料------------------------------------------------
      FOR x IN (SELECT wro.*,
                       wdj.attribute3 ws_code,
                       wdj.primary_item_id,
                       wov.operation_code
                  FROM wip_requirement_operations_v wro,
                       wip_operations_v             wov,
                       wip_discrete_jobs            wdj,
                       wip_entities                 we
                 WHERE wdj.wip_entity_id = wro.wip_entity_id
                   AND wro.organization_id = wov.organization_id(+)
                   AND wro.operation_seq_num = wov.operation_seq_num(+)
                   AND wro.wip_entity_id = wov.wip_entity_id(+)
                   AND we.wip_entity_id = wdj.wip_entity_id
                   AND wro.organization_id = p_organization_id
                   AND we.wip_entity_name = p_wip_name
                   AND wro.wip_supply_meaning LIKE
                       '%' || substr(p_wip_type, 1, 2) || '%') LOOP
      
        --计算本次的补料总需求数量
        v_required_item_qty := cux_wip_item_replace_pkg.get_quantity_required_b(p_org_id            => x.organization_id,
                                                                                p_wip_id            => x.wip_entity_id,
                                                                                p_item_id           => x.inventory_item_id,
                                                                                p_operation_seq_num => x.operation_seq_num,
                                                                                p_now_qty           => p_now_qty);
      
        -----已经发料量
        v_ready_qty_new := cux_wip_item_replace_pkg.get_quantity_issued(p_org_id            => x.organization_id,
                                                                        p_wip_id            => x.wip_entity_id,
                                                                        p_item_id           => x.inventory_item_id,
                                                                        p_operation_seq_num => x.operation_seq_num,
                                                                        p_doc_no            => v_create_lot);
        ------本次已经补料量
        v_ready_item_b_qty := cux_wip_item_replace_pkg.get_quantity_issued_b(p_org_id            => x.organization_id,
                                                                             p_wip_id            => x.wip_entity_id,
                                                                             p_item_id           => x.inventory_item_id,
                                                                             p_operation_seq_num => x.operation_seq_num,
                                                                             p_doc_no            => v_create_lot);
        -----物料自己的总下达量
        v_quantity_issued := cux_wip_item_replace_pkg.get_quantity_issued_z(p_org_id            => x.organization_id,
                                                                            p_wip_id            => x.wip_entity_id,
                                                                            p_item_id           => x.inventory_item_id,
                                                                            p_operation_seq_num => x.operation_seq_num);
      
        IF v_required_item_qty - v_ready_qty_new - v_ready_item_b_qty >
           x.required_quantity THEN
          v_ready_qty_s := x.required_quantity - v_quantity_issued;
        ELSE
          v_ready_qty_s := v_required_item_qty - v_ready_qty_new -
                           v_ready_item_b_qty;
          IF v_ready_qty_s > x.required_quantity - v_quantity_issued THEN
            v_ready_qty_s := x.required_quantity - v_quantity_issued;
          END IF;
        END IF;
        v_ready_qty_s := ceil(v_ready_qty_s);
      
        IF v_ready_qty_s > 0 THEN
          ---获取默认仓库和货位
        
          BEGIN
            SELECT a.sub, a.loc_code, a.location_id
              INTO l_cux_ready_item.supply_subinventory,
                   l_cux_ready_item.supply_loc_code,
                   l_cux_ready_item.supply_locator_id
              FROM cux_sub_loc_set_b a
             WHERE a.organization_id = x.organization_id
               AND a.inventory_item_id = x.inventory_item_id
               AND a.ws_code = x.ws_code;
          EXCEPTION
            WHEN OTHERS THEN
              select msi.segment1
                into v_temp_item
                from mtl_system_items_b msi
               where msi.organization_id = x.Organization_Id
                 and msi.inventory_item_id = x.Inventory_Item_Id;
            
              p_Return_Status := 'E';
              p_Return_Msg    := '默认发料子库设置有误:' || v_temp_item || ' 对应的车间 :' ||
                                 x.Ws_Code || ' 未配置或配置多行,请检查!';
              RETURN;
          END;
        
          --如果指定了仓库和货位
          IF p_sub IS NOT NULL THEN
            l_cux_ready_item.supply_subinventory := p_sub;
            l_cux_ready_item.supply_loc_code     := null;
            l_cux_ready_item.supply_locator_id   := null;
          END IF;
        
          IF p_loc IS NOT NULL THEN
            l_cux_ready_item.supply_loc_code := p_loc;
          
            SELECT mil.inventory_location_id
              INTO l_cux_ready_item.supply_locator_id
              FROM mtl_item_locations mil
             WHERE mil.organization_id = x.organization_id
               AND mil.subinventory_code = p_sub
               AND mil.segment1 = p_loc;
          END IF;
          ----------
          IF p_wip_type IN ('拉式补料') THEN
            IF p_organization_id = 84 THEN
              --福建锐捷
              l_cux_ready_item.to_sub := 'GX01';
            ELSIF p_organization_id = 83 THEN
              --北京锐捷
              l_cux_ready_item.to_sub := 'HX01';
            END IF;
          
            BEGIN
              SELECT mil.inventory_location_id, mil.segment1
                INTO l_cux_ready_item.to_locator_id,
                     l_cux_ready_item.to_loc_code
                FROM mtl_item_locations mil
               WHERE mil.organization_id = p_organization_id
                 AND nvl(mil.enabled_flag, 'N') = 'Y'
                 AND mil.segment1 = p_wip_name;
            EXCEPTION
              WHEN OTHERS THEN
                NULL;
            END;
          
          END IF;
          --------获取现有量
          l_onhand_qty := cux_wip_transactions_pkg.get_available_qty(x.organization_id,
                                                                     x.inventory_item_id,
                                                                     l_cux_ready_item.supply_subinventory,
                                                                     l_cux_ready_item.supply_locator_id);
        
          Add_Available_Msg(p_organization_id   => x.organization_id,
                            p_inventory_item_id => x.inventory_item_id,
                            p_sub               => l_Cux_Ready_Item.Supply_Subinventory,
                            p_loc               => l_Cux_Ready_Item.Supply_Loc_Code,
                            p_Onhand_Qty        => l_Onhand_Qty,
                            x_onhand_msg        => v_onhand_msg,
                            x_onhand_count      => v_onhand_count);
        
          l_ready_qty := 0;
          --现有量取最小整数
          /*            l_onhand_qty := trunc(l_onhand_qty);*/
          IF l_onhand_qty >= v_ready_qty_s THEN
            l_ready_qty := v_ready_qty_s;
          ELSIF l_onhand_qty > 0 THEN
            l_ready_qty := l_onhand_qty;
          ELSE
            l_ready_qty := 0;
          END IF;
          l_cux_ready_item.ready_qty := l_ready_qty;
        
          l_cux_ready_item.line_id                := cux.cux_doc_line_seq.nextval;
          l_cux_ready_item.organization_id        := x.organization_id;
          l_cux_ready_item.doc_type               := p_wip_type;
          l_cux_ready_item.doc_date               := p_doc_date;
          l_cux_ready_item.doc_status             := '未过账';
          l_cux_ready_item.wip_entity_id          := x.wip_entity_id;
          l_cux_ready_item.wip_entity_name        := p_wip_name;
          l_cux_ready_item.primary_item_id        := x.primary_item_id;
          l_cux_ready_item.now_qty                := p_now_qty;
          l_cux_ready_item.inventory_item_id      := x.inventory_item_id;
          l_cux_ready_item.item_primary_uom_code  := x.item_primary_uom_code;
          l_cux_ready_item.operation_seq_num      := x.operation_seq_num;
          l_cux_ready_item.operation_code         := x.operation_code;
          l_cux_ready_item.quantity_per_assembly  := x.quantity_per_assembly;
          l_cux_ready_item.component_yield_factor := x.component_yield_factor;
          l_cux_ready_item.required_quantity      := x.required_quantity;
          l_cux_ready_item.quantity_issued        := x.quantity_issued;
          l_cux_ready_item.iss_ready_qty          := v_ready_qty_s;
          l_cux_ready_item.creation_date          := SYSDATE;
          l_cux_ready_item.created_by             := fnd_global.user_id;
          l_cux_ready_item.last_update_date       := SYSDATE;
          l_cux_ready_item.last_updated_by        := fnd_global.user_id;
          l_cux_ready_item.last_update_login      := fnd_global.login_id;
          l_cux_ready_item.create_lot             := v_create_lot;
          l_cux_ready_item.doc_no                 := v_create_lot;
        
          l_Cux_Ready_Item.Material_Batch1        := x.attribute1; -- 属性号1
          l_Cux_Ready_Item.Material_Batch2        := x.attribute2; -- 属性号2
          l_Cux_Ready_Item.Special_Number         := x.attribute4; -- 特制单号
          l_Cux_Ready_Item.Special_Order          := x.attribute3; -- 特制数量
          l_Cux_Ready_Item.Empty_Return_Flag      := p_param.EMPTY_RETURN_FLAG;
          ----
          IF l_cux_ready_item.ready_qty > 0 THEN
            INSERT INTO cux_ready_item VALUES l_cux_ready_item;
            ----开始保留   
            Cux_Wip_Transactions_Pkg.create_save(l_cux_ready_item.line_id,
                                                 p_return_status,
                                                 p_return_msg);
            IF p_return_status <> 'S' THEN
              RETURN;
            END IF;
          END IF;
        END IF;
      END LOOP;
      ----------------------------------------------预备料完成------------------------------------------
    
      SELECT 'X' || cux.cux_doc_line_seq.nextval --TO_CHAR(systimestamp, 'YYYYMMDDHH24MISSFF4')
        INTO v_create_lot_new
        FROM dual;
    
      ----------------------------------------------开始分配到备料单-----------------------------------
      FOR x IN (SELECT *
                  FROM cux_ready_item a
                 WHERE a.doc_no = v_create_lot
                 ORDER BY a.operation_seq_num, a.supply_subinventory) LOOP
        ------
        ------判断是否是材料仓库
        -------材料仓补料单需要关联备料单
        -------成品仓补料单不需要关联备料单
        BEGIN
          SELECT nvl(msa.attribute4, 'N')
            INTO v_flag
            FROM mtl_subinventories_all_v msa
           WHERE msa.organization_id = x.organization_id
             AND msa.secondary_inventory_name = x.supply_subinventory;
        EXCEPTION
          WHEN OTHERS THEN
            v_flag := 'N';
        END;
        IF v_flag = 'N' THEN
          ---材料仓
          v_qty := x.ready_qty;
          ---开始查找备料单
          FOR y IN (SELECT *
                      FROM cux_ready_item cri
                     WHERE cri.organization_id = x.organization_id
                       AND cri.wip_entity_name = x.wip_entity_name
                       AND cri.operation_seq_num = x.operation_seq_num
                       AND cri.supply_subinventory = x.supply_subinventory
                       AND cri.inventory_item_id = x.inventory_item_id
                       AND cri.doc_type LIKE '%备料%') LOOP
            ----备料单的需求数量
            v_required_item_qty := cux_wip_item_replace_pkg.get_quantity_required_b(p_org_id            => x.organization_id,
                                                                                    p_wip_id            => x.wip_entity_id,
                                                                                    p_item_id           => x.inventory_item_id,
                                                                                    p_operation_seq_num => x.operation_seq_num,
                                                                                    p_now_qty           => y.now_qty);
            ------备料单已经备料数量量
            v_ready_item_b_qty := cux_wip_item_replace_pkg.get_quantity_issued_b(p_org_id            => y.organization_id,
                                                                                 p_wip_id            => y.wip_entity_id,
                                                                                 p_item_id           => y.inventory_item_id,
                                                                                 p_operation_seq_num => y.operation_seq_num,
                                                                                 p_doc_no            => y.doc_no);
            ------备料单已经补料的数量
            SELECT nvl(SUM(cri.ready_qty), 0)
              INTO v_ready_qty_b
              FROM cux_ready_item cri
             WHERE cri.wip_entity_id = y.wip_entity_id
               AND cri.organization_id = y.organization_id
               AND cri.operation_seq_num = y.operation_seq_num
               AND cri.inventory_item_id = y.inventory_item_id
               AND cri.attribute10 = y.doc_no
               AND nvl(cri.attribute11, 0) <> -0.0001; --排除材料仓随便关联的数据
            IF v_required_item_qty - v_ready_item_b_qty - v_ready_qty_b > 0 THEN
              --有数量可以关联
              IF v_qty >=
                 v_required_item_qty - v_ready_item_b_qty - v_ready_qty_b THEN
                l_cux_ready_item.line_id                := cux.cux_doc_line_seq.nextval;
                l_cux_ready_item.organization_id        := y.organization_id;
                l_cux_ready_item.doc_type               := p_wip_type;
                l_cux_ready_item.doc_date               := p_doc_date;
                l_cux_ready_item.doc_status             := '未过账';
                l_cux_ready_item.wip_entity_id          := y.wip_entity_id;
                l_cux_ready_item.wip_entity_name        := p_wip_name;
                l_cux_ready_item.primary_item_id        := y.primary_item_id;
                l_cux_ready_item.now_qty                := p_now_qty;
                l_cux_ready_item.inventory_item_id      := y.inventory_item_id;
                l_cux_ready_item.item_primary_uom_code  := y.item_primary_uom_code;
                l_cux_ready_item.operation_seq_num      := y.operation_seq_num;
                l_cux_ready_item.operation_code         := y.operation_code;
                l_cux_ready_item.quantity_per_assembly  := y.quantity_per_assembly;
                l_cux_ready_item.component_yield_factor := y.component_yield_factor;
                l_cux_ready_item.required_quantity      := y.required_quantity;
                l_cux_ready_item.to_sub                 := x.to_sub;
                l_cux_ready_item.supply_subinventory    := x.supply_subinventory;
                l_cux_ready_item.supply_locator_id      := x.supply_locator_id;
                l_cux_ready_item.supply_loc_code        := x.supply_loc_code;
            
                l_Cux_Ready_Item.Material_Batch1   := x.Material_Batch1;
                l_Cux_Ready_Item.Material_Batch2   := x.Material_Batch2;
                l_Cux_Ready_Item.Special_Number    := x.Special_Number;
                l_Cux_Ready_Item.Special_Order     := x.Special_Order;   
                l_Cux_Ready_Item.Empty_Return_Flag := x.empty_return_flag;
                
                l_cux_ready_item.quantity_issued        := y.quantity_issued;
                l_cux_ready_item.iss_ready_qty          := v_required_item_qty -
                                                           v_ready_item_b_qty -
                                                           v_ready_qty_b;
                l_cux_ready_item.creation_date          := SYSDATE;
                l_cux_ready_item.ready_qty              := v_required_item_qty -
                                                           v_ready_item_b_qty -
                                                           v_ready_qty_b;
                l_cux_ready_item.created_by             := fnd_global.user_id;
                l_cux_ready_item.last_update_date       := SYSDATE;
                l_cux_ready_item.last_updated_by        := fnd_global.user_id;
                l_cux_ready_item.last_update_login      := fnd_global.login_id;
                l_cux_ready_item.create_lot             := v_create_lot_new;
                l_cux_ready_item.doc_no                 := v_create_lot_new;
                l_cux_ready_item.attribute10            := y.doc_no;
                l_cux_ready_item.attribute11            := v_qty;
                v_doc_no                                := y.doc_no;
                v_qty                                   := v_qty -
                                                           (v_required_item_qty -
                                                           v_ready_item_b_qty -
                                                           v_ready_qty_b);
                                                                                                 
              ELSE
                l_cux_ready_item.line_id                := cux.cux_doc_line_seq.nextval;
                l_cux_ready_item.organization_id        := y.organization_id;
                l_cux_ready_item.doc_type               := p_wip_type;
                l_cux_ready_item.doc_date               := p_doc_date;
                l_cux_ready_item.doc_status             := '未过账';
                l_cux_ready_item.wip_entity_id          := y.wip_entity_id;
                l_cux_ready_item.wip_entity_name        := p_wip_name;
                l_cux_ready_item.primary_item_id        := y.primary_item_id;
                l_cux_ready_item.now_qty                := p_now_qty;
                l_cux_ready_item.inventory_item_id      := y.inventory_item_id;
                l_cux_ready_item.item_primary_uom_code  := y.item_primary_uom_code;
                l_cux_ready_item.operation_seq_num      := y.operation_seq_num;
                l_cux_ready_item.operation_code         := y.operation_code;
                l_cux_ready_item.quantity_per_assembly  := y.quantity_per_assembly;
                l_cux_ready_item.component_yield_factor := y.component_yield_factor;
                l_cux_ready_item.required_quantity      := y.required_quantity;
                l_cux_ready_item.to_sub                 := x.to_sub;
                l_cux_ready_item.supply_subinventory    := x.supply_subinventory;
                l_cux_ready_item.supply_locator_id      := x.supply_locator_id;
                l_cux_ready_item.supply_loc_code        := x.supply_loc_code;
                
                l_Cux_Ready_Item.Material_Batch1   := x.Material_Batch1;
                l_Cux_Ready_Item.Material_Batch2   := x.Material_Batch2;
                l_Cux_Ready_Item.Special_Number    := x.Special_Number;
                l_Cux_Ready_Item.Special_Order     := x.Special_Order;   
                l_Cux_Ready_Item.Empty_Return_Flag := x.empty_return_flag;
                
                l_cux_ready_item.quantity_issued        := y.quantity_issued;
                l_cux_ready_item.iss_ready_qty          := v_qty;
                l_cux_ready_item.creation_date          := SYSDATE;
                l_cux_ready_item.ready_qty              := v_qty;
                l_cux_ready_item.created_by             := fnd_global.user_id;
                l_cux_ready_item.last_update_date       := SYSDATE;
                l_cux_ready_item.last_updated_by        := fnd_global.user_id;
                l_cux_ready_item.last_update_login      := fnd_global.login_id;
                l_cux_ready_item.create_lot             := v_create_lot_new;
                l_cux_ready_item.doc_no                 := v_create_lot_new;
                l_cux_ready_item.attribute10            := y.doc_no;
                l_cux_ready_item.attribute11            := v_qty;
                v_doc_no                                := y.doc_no;
                v_qty                                   := 0;
              END IF;
              IF l_cux_ready_item.ready_qty > 0 THEN
                INSERT INTO cux_ready_item VALUES l_cux_ready_item;
              END IF;
            END IF;
          
          END LOOP;
        
          IF v_qty > 0 THEN
            ---未找到物料对应备料单
            SELECT MIN(a.doc_no)
              INTO v_doc_no
              FROM cux_ready_item a
             WHERE a.wip_entity_id = x.wip_entity_id
               AND a.organization_id = x.organization_id
               AND a.operation_seq_num = x.operation_seq_num
               AND a.supply_subinventory = x.supply_subinventory
               AND substr(a.doc_type, 1, 2) = substr(p_wip_type, 1, 2)
               AND a.doc_type IN ('推式备料', '拉式备料');
            l_cux_ready_item.line_id                := cux.cux_doc_line_seq.nextval;
            l_cux_ready_item.organization_id        := x.organization_id;
            l_cux_ready_item.doc_type               := p_wip_type;
            l_cux_ready_item.doc_date               := p_doc_date;
            l_cux_ready_item.doc_status             := '未过账';
            l_cux_ready_item.wip_entity_id          := x.wip_entity_id;
            l_cux_ready_item.wip_entity_name        := p_wip_name;
            l_cux_ready_item.primary_item_id        := x.primary_item_id;
            l_cux_ready_item.now_qty                := p_now_qty;
            l_cux_ready_item.inventory_item_id      := x.inventory_item_id;
            l_cux_ready_item.item_primary_uom_code  := x.item_primary_uom_code;
            l_cux_ready_item.operation_seq_num      := x.operation_seq_num;
            l_cux_ready_item.operation_code         := x.operation_code;
            l_cux_ready_item.quantity_per_assembly  := x.quantity_per_assembly;
            l_cux_ready_item.component_yield_factor := x.component_yield_factor;
            l_cux_ready_item.required_quantity      := x.required_quantity;
            l_cux_ready_item.to_sub                 := x.to_sub;
            l_cux_ready_item.supply_subinventory    := x.supply_subinventory;
            l_cux_ready_item.supply_locator_id      := x.supply_locator_id;
            l_cux_ready_item.supply_loc_code        := x.supply_loc_code;
            l_cux_ready_item.quantity_issued        := x.quantity_issued;
            l_cux_ready_item.iss_ready_qty          := v_qty;
            l_cux_ready_item.creation_date          := SYSDATE;
            l_cux_ready_item.ready_qty              := v_qty;
            l_cux_ready_item.created_by             := fnd_global.user_id;
            l_cux_ready_item.last_update_date       := SYSDATE;
            l_cux_ready_item.last_updated_by        := fnd_global.user_id;
            l_cux_ready_item.last_update_login      := fnd_global.login_id;
            l_cux_ready_item.create_lot             := v_create_lot_new;
            l_cux_ready_item.doc_no                 := v_create_lot_new;
            l_cux_ready_item.attribute10            := v_doc_no;
            l_cux_ready_item.attribute11            := -0.0001;
          
            l_Cux_Ready_Item.Material_Batch1   := x.Material_Batch1;
            l_Cux_Ready_Item.Material_Batch2   := x.Material_Batch2;
            l_Cux_Ready_Item.Special_Number    := x.Special_Number;
            l_Cux_Ready_Item.Special_Order     := x.Special_Order;   
            l_Cux_Ready_Item.Empty_Return_Flag := x.empty_return_flag;
                 
            INSERT INTO cux_ready_item VALUES l_cux_ready_item;
          END IF;
        
          -- 无须关联备料单      
        ELSE
          v_qty                                   := x.ready_qty;
          l_cux_ready_item.line_id                := cux.cux_doc_line_seq.nextval;
          l_cux_ready_item.organization_id        := x.organization_id;
          l_cux_ready_item.doc_type               := p_wip_type;
          l_cux_ready_item.doc_date               := p_doc_date;
          l_cux_ready_item.doc_status             := '未过账';
          l_cux_ready_item.wip_entity_id          := x.wip_entity_id;
          l_cux_ready_item.wip_entity_name        := p_wip_name;
          l_cux_ready_item.primary_item_id        := x.primary_item_id;
          l_cux_ready_item.now_qty                := p_now_qty;
          l_cux_ready_item.inventory_item_id      := x.inventory_item_id;
          l_cux_ready_item.item_primary_uom_code  := x.item_primary_uom_code;
          l_cux_ready_item.operation_seq_num      := x.operation_seq_num;
          l_cux_ready_item.operation_code         := x.operation_code;
          l_cux_ready_item.quantity_per_assembly  := x.quantity_per_assembly;
          l_cux_ready_item.component_yield_factor := x.component_yield_factor;
          l_cux_ready_item.required_quantity      := x.required_quantity;
          l_cux_ready_item.to_sub                 := x.to_sub;
          l_cux_ready_item.supply_subinventory    := x.supply_subinventory;
          l_cux_ready_item.supply_locator_id      := x.supply_locator_id;
          l_cux_ready_item.supply_loc_code        := x.supply_loc_code;
          l_cux_ready_item.quantity_issued        := x.quantity_issued;
          l_cux_ready_item.iss_ready_qty          := x.iss_ready_qty;
          l_cux_ready_item.creation_date          := SYSDATE;
          l_cux_ready_item.ready_qty              := v_qty;
          l_cux_ready_item.created_by             := fnd_global.user_id;
          l_cux_ready_item.last_update_date       := SYSDATE;
          l_cux_ready_item.last_updated_by        := fnd_global.user_id;
          l_cux_ready_item.last_update_login      := fnd_global.login_id;
          l_cux_ready_item.create_lot             := v_create_lot_new;
          l_cux_ready_item.doc_no                 := v_create_lot_new;
          l_cux_ready_item.attribute11            := v_qty;
          l_cux_ready_item.attribute10            := NULL; ---备料单号
                 
          l_Cux_Ready_Item.Material_Batch1   := x.Material_Batch1;
          l_Cux_Ready_Item.Material_Batch2   := x.Material_Batch2;
          l_Cux_Ready_Item.Special_Number    := x.Special_Number;
          l_Cux_Ready_Item.Special_Order     := x.Special_Order;   
          l_Cux_Ready_Item.Empty_Return_Flag := x.empty_return_flag;       
          
          INSERT INTO cux_ready_item VALUES l_cux_ready_item;
        END IF;
      END LOOP;
      ---------------------------------------------备料单分配完成---------------------------
    
      ----------------------------------------更新补料单号----------------------------------
      SELECT to_char(systimestamp, 'YYYYMMDDHH24MISSFF4')
        INTO v_create_lot_1
        FROM dual;
      l_lot := v_create_lot_1;
    
      FOR x IN (SELECT DISTINCT a.wip_entity_name,
                                a.supply_subinventory,
                                a.operation_seq_num,
                                a.organization_id,
                                nvl(a.attribute10, 'ASDFG') attribute10
                  FROM cux_ready_item a
                 WHERE a.create_lot = v_create_lot_new) LOOP
        IF x.organization_id = 84 THEN
          v_doc_no := 'AD' || cux_doc_add_seq_fw.nextval;
        ELSE
          v_doc_no := 'AD' || cux_doc_add_seq_jw.nextval;
        END IF;
      
        UPDATE cux_ready_item a
           SET a.doc_no          = v_doc_no,
               a.create_lot      = v_create_lot_1,
               A.TRANSFER_STATUS = 'WAITING'
         WHERE a.create_lot = v_create_lot_new
           AND a.operation_seq_num = x.operation_seq_num
           AND a.supply_subinventory = x.supply_subinventory
           AND a.organization_id = x.organization_id
           AND a.wip_entity_name = x.wip_entity_name
           AND nvl(a.attribute10, 'ASDFG') = x.attribute10;
      END LOOP;
    
      --added by bruce on 20151130
      for rec_upd_line_number in (select cri.line_id, cri.doc_no
                                    from cux_ready_item cri
                                   where cri.create_lot = v_Create_Lot_New
                                   order by cri.doc_no) loop
      
        if (l_old_doc_number is null or
           l_old_doc_number <> rec_upd_line_number.doc_no) then
        
          l_old_doc_number := rec_upd_line_number.doc_no;
          l_line_number    := 0;
        
        end if;
      
        update cux_ready_item cri
           set cri.line_number = l_line_number + 1
         where cri.doc_no = rec_upd_line_number.doc_no;
      
      end loop;
      --end of added by bruce on 20151130
    
      -------------------------删除预补料的物料占用-----------------------------
      DELETE FROM mtl_reservations a
       WHERE a.supply_source_name = v_create_lot;
    
      -- 删除有可能导致上面删除保留失败 
      l_onhand_qty := cux_wip_transactions_pkg.get_available_qty(84,
                                                                 18852,
                                                                 'G106',
                                                                 16055);
      dbms_output.put_line(l_onhand_qty);
    
      DELETE FROM cux_ready_item a WHERE a.create_lot = v_create_lot;
      ---------------------------------------开始物料占用-------------------------
      l_count := 0;
      FOR x IN (SELECT a.line_id
                  FROM cux_ready_item a
                 WHERE a.create_lot = v_create_lot_1
                   AND a.ready_qty <> 0) LOOP
        Cux_Wip_Transactions_Pkg.create_save(x.line_id,
                                             p_return_status,
                                             p_return_msg);
        IF p_return_status <> 'S' THEN
          RETURN;
        END IF;
        l_count := l_count + 1;
      END LOOP;
      COMMIT;
      p_return_status := 'S';
      p_return_msg    := '下达成功';
    
      IF l_count = 0 THEN
        p_return_status := 'E';
        p_return_msg    := '无明细数据或无库存数量,单据未生成！';
      
        -- 现有量不足提示
        if v_onhand_count > 0 then
          p_Return_Msg := '存在' || v_onhand_count || '个物料对应的仓库货物无可用量:' ||
                          v_onhand_msg;
        end if;
      END IF;
    END IF;
  
    --added by bruce on 20151028
  
    if (p_Return_Status = 'S') then
    
      process_full_prepared(p_lot             => '',
                            p_organization_id => p_Organization_Id,
                            p_wip_id          => p_wip_id,
                            p_wip_name        => p_Wip_Name,
                            p_Return_Status   => p_Return_Status,
                            p_Return_Msg      => p_Return_Msg);
    
      if (p_Return_Status = 'S') then
      
        transfer_to_wms(p_doc_no          => '',
                        p_organization_id => p_Organization_Id,
                        p_doc_type        => '',
                        p_lot             => l_lot,
                        p_Return_Status   => p_Return_Status,
                        p_Return_Msg      => p_Return_Msg);
      
      end if;
    end if;
  
    --end of add by bruce on 20151028
    -- time_mark : 添加记录时间代码
    -- Cux_Time_Used_Pkg.add_time_used('补料完成',l_time_id);
  END;
  PROCEDURE Create_Return_Doc(p_Organization_Id NUMBER,
                              p_wip_id          number,
                              p_Wip_Name        VARCHAR2,
                              p_Wip_Type        VARCHAR2,
                              p_Doc_Date        DATE,
                              p_Now_Qty         NUMBER,
                              p_Sub             VARCHAR2,
                              p_Loc             VARCHAR2,
                              p_Return_Status   OUT VARCHAR2,
                              p_Return_Msg      OUT VARCHAR2) AS
  
    l_lot              varchar2(100);
    l_cux_ready_item   cux.cux_ready_item%ROWTYPE;
    old_sub            VARCHAR2(100);
    old_operation_code VARCHAR2(100);
    old_doc_no         VARCHAR2(100);
    l_count            NUMBER := 0;
    is_next            VARCHAR2(1) := 'Y';
    l_to_sub           VARCHAR2(100);
    is_have            NUMBER;
    l_ready_qty        NUMBER;
    l_onhand_qty       NUMBER;
    l_have             NUMBER := 0;
    l_is_loc_con       NUMBER := 0;
    l_index            NUMBER := 0;
    l_line_number      number;
    v_seq              NUMBER;
    v_sy_qty           NUMBER;
    v_flag             VARCHAR2(10); --是否成品仓标识
  
    CURSOR c_line IS
    
      SELECT a.organization_id,
             a.wip_entity_id,
             a.wip_entity_name,
             a.primary_item_id,
             a.ws_code, --车间
             a.start_quantity,
             a.inventory_item_id,
             a.com_item_code,
             a.item_primary_uom_code,
             a.operation_seq_num,
             a.operation_code,
             a.quantity_per_assembly,
             a.component_yield_factor,
             a.required_quantity,
             a.quantity_issued,
             decode(sign(a.iss_ready_qty), 1, a.iss_ready_qty, 0, 0, -1, 0) iss_ready_qty,
             cws.sub,
             cws.loc_code loc,
             cws.location_id inventory_location_id,
             to_char(systimestamp, 'YYYYMMDDHH24MISSFF4') create_lot,
             a.replace_flag,
             a.MATERIAL_BATCH1,
             a.MATERIAL_BATCH2,
             a.SPECIAL_ORDER,
             a.SPECIAL_NUMBER
        FROM (SELECT wdj.organization_id,
                     wdj.wip_entity_id,
                     wdj.wip_entity_name,
                     wdj.primary_item_id,
                     wdj.attribute3 ws_code,
                     wdj.start_quantity,
                     wro.inventory_item_id,
                     msi.segment1 com_item_code,
                     wro.item_primary_uom_code,
                     wro.operation_seq_num,
                     wov.operation_code,
                     wro.quantity_per_assembly,
                     wro.component_yield_factor,
                     wro.required_quantity,
                     wro.quantity_issued,
                     wro.attribute1 MATERIAL_BATCH1,
                     wro.attribute2 MATERIAL_BATCH2,
                     wro.attribute3 SPECIAL_ORDER,
                     wro.attribute4 SPECIAL_NUMBER,
                     decode(p_wip_type
                            
                           ,
                            '推式退料(负单)',
                            round((nvl(wro.quantity_issued, 0) -
                                  wro.required_quantity -
                                  (SELECT nvl(SUM(nvl(cri.ready_qty, 0)), 0)
                                      FROM cux_ready_item cri
                                     WHERE cri.organization_id =
                                           wro.organization_id
                                       AND cri.wip_entity_id =
                                           wro.wip_entity_id
                                       AND cri.operation_seq_num =
                                           wro.operation_seq_num
                                       AND cri.inventory_item_id =
                                           wro.inventory_item_id
                                       AND ((cri.doc_status = '未过账' AND
                                           cri.doc_type IN
                                           ('推式退料(负单)',
                                              '推式退料(零单)')) OR
                                           ((cri.doc_status = '已过账') AND
                                           cri.doc_type IN
                                           ('推式超领', '拉式超领'))))),
                                  6)
                            
                           ,
                            '推式退料(零单)',
                            wro.quantity_issued
                            
                           ,
                            round(wro.quantity_per_assembly * p_now_qty /
                                  cux_wip_transactions_pkg.get_component_yield_factor(p_org_id                 => wro.organization_id,
                                                                                      p_wip_id                 => wro.wip_entity_id,
                                                                                      p_operation_seq_num      => wro.operation_seq_num,
                                                                                      p_item_id                => wro.inventory_item_id,
                                                                                      p_component_yield_factor => wro.component_yield_factor))) iss_ready_qty
                     
                    ,
                     cux_wip_transactions_pkg.get_replace_item(p_org_id  => wro.organization_id,
                                                               p_wip_id  => wro.wip_entity_id,
                                                               p_item_id => wro.inventory_item_id) replace_flag
                FROM wip_discrete_jobs_v          wdj,
                     wip_requirement_operations_v wro,
                     wip_operations_v             wov,
                     mtl_system_items_vl          msi
               WHERE 1 = 1
                 AND wdj.organization_id = wro.organization_id
                 AND wdj.wip_entity_id = wro.wip_entity_id
                 AND wro.organization_id = wov.organization_id(+)
                 AND wro.operation_seq_num = wov.operation_seq_num(+)
                 AND wro.wip_entity_id = wov.wip_entity_id(+)
                 AND msi.organization_id = wro.organization_id
                 AND msi.inventory_item_id = wro.inventory_item_id
                    
                 AND wdj.organization_id = p_organization_id
                 AND wdj.wip_entity_id = p_wip_id
                 AND nvl(wro.repetitive_schedule_id, '-1') =
                     nvl(wov.repetitive_schedule_id, '-1')
                 AND wro.wip_supply_meaning LIKE
                     '%' || substr(p_wip_type, 1, 2) || '%') a,
             cux_sub_loc_set_b cws
       WHERE 1 = 1
         AND cws.organization_id(+) = a.organization_id
         AND cws.inventory_item_id(+) = a.inventory_item_id
         AND cws.ws_code(+) = a.ws_code
         AND nvl(cws.sub, '1') = nvl(p_sub, nvl(cws.sub, '1'))
         AND nvl(cws.loc_code, '1') = nvl(p_loc, nvl(cws.loc_code, '1'))
         AND (a.iss_ready_qty <> 0 AND p_wip_type NOT IN ('推式超领', '拉式超领') OR
              p_wip_type IN ('推式超领', '拉式超领'))
       ORDER BY operation_code, sub, replace_flag;
  
    v_count             NUMBER;
    v_ready_qty         NUMBER;
    v_create_lot        VARCHAR2(100);
    v_create_lot_new    VARCHAR2(100);
    v_ready_qty_new     NUMBER;
    v_required_quantity NUMBER;
    v_required_item_qty NUMBER;
    v_ready_item_b_qty  NUMBER;
    v_ready_qty_b       NUMBER;
    v_ready_qty_s       NUMBER;
    v_quantity_issued   NUMBER;
    v_doc_no            VARCHAR2(100);
    v_create_lot_1      VARCHAR2(100);
    v_rec               inv_reservation_global.mtl_reservation_rec_type;
    x_msg_count         NUMBER;
    x_msg_data          VARCHAR2(240);
    p_dummy_sn          inv_reservation_global.serial_number_tbl_type;
    x_status            VARCHAR2(1);
  BEGIN
    SELECT cux.cux_doc_line_seq.nextval INTO v_seq FROM dual;
  
    -- 检查一 : 默认子库 货位
    FOR c0 IN c_line LOOP
      is_have := 0;
    
      --判断来源子存和货位,系统不允许为空
      IF c0.sub IS NULL THEN
        is_next         := 'N';
        p_return_status := 'E';
      
        IF p_return_msg IS NULL THEN
          p_return_msg := '单据下达失败,组件子库为空,请检查:' || chr(10);
        END IF;
        p_return_msg := p_return_msg || '[' || c0.com_item_code || ']' ||
                        chr(10);
      
      ELSE
        -- 判断是否有开启货位控制
        BEGIN
          SELECT locator_type
            INTO l_is_loc_con
            FROM mtl_secondary_inventories_fk_v msi
           WHERE msi.secondary_inventory_name = c0.sub
             AND msi.organization_id = c0.organization_id;
        EXCEPTION
          WHEN OTHERS THEN
            l_is_loc_con := 0;
        END;
      
        IF l_is_loc_con <> 1 AND c0.inventory_location_id IS NULL THEN
          is_next         := 'N';
          p_return_status := 'E';
        
          IF l_index = 0 THEN
            p_return_msg := '单据下达失败,组件货位为空,请检查:' || chr(10);
            l_index      := 1;
          END IF;
          p_return_msg := p_return_msg || '[' || c0.com_item_code || ']' ||
                          chr(10);
        END IF;
      END IF;
    END LOOP;
  
    --检查二 : 线边仓判断 判断目标子存和货位
    IF p_wip_type IN ('拉式退料', '拉式超领') THEN
    
      -- 目标仓库都是线边仓
      IF p_organization_id = 84 THEN
        l_to_sub := 'GX01';
      ELSIF p_organization_id = 83 THEN
        l_to_sub := 'HX01';
      END IF;
    
      -- 线边仓对应的货位为工单名
      SELECT COUNT(1)
        INTO is_have
        FROM mtl_item_locations mil
       WHERE mil.organization_id = p_organization_id
         AND nvl(mil.enabled_flag, 'N') = 'Y'
         AND mil.subinventory_code = l_to_sub
         AND mil.segment1 = p_wip_name;
    
      IF is_have < 1 THEN
        is_next         := 'N';
        p_return_status := 'E';
        p_return_msg    := p_return_msg || '单据下达失败,组件子库[' || l_to_sub ||
                           ']或货位[' || p_wip_name || ']为空,请检查:' || chr(10);
        RETURN;
      END IF;
    END IF;
  
    -- 校验通过,开始备料
    IF is_next <> 'Y' THEN
      return;
    END IF;
  
    FOR c1 IN c_line LOOP
      l_count          := l_count + 1;
      l_cux_ready_item := NULL;
    
      IF (old_sub IS NULL AND old_operation_code IS NULL) OR
         (old_sub <> c1.sub) OR (old_operation_code <> c1.operation_code) THEN
      
        IF p_organization_id = 83 AND
           p_wip_type IN ('拉式退料', '推式退料(负单)', '推式退料(零单)') THEN
          l_cux_ready_item.doc_no := 'RN' || cux_doc_rn_seq_jw.nextval;
        
        ELSIF p_organization_id = 84 AND
              p_wip_type IN ('拉式退料', '推式退料(负单)', '推式退料(零单)') THEN
          l_cux_ready_item.doc_no := 'RN' || cux_doc_rn_seq_fw.nextval;
        
        ELSIF p_organization_id = 83 AND
              p_wip_type IN ('推式超领', '拉式超领') THEN
          l_cux_ready_item.doc_no := 'MA' || cux_doc_ma_seq_jw.nextval;
        
        ELSIF p_organization_id = 84 AND
              p_wip_type IN ('推式超领', '拉式超领') THEN
          l_cux_ready_item.doc_no := 'MA' || cux_doc_ma_seq_fw.nextval;
        END IF;
      
        old_sub            := c1.sub;
        old_operation_code := c1.operation_code;
        old_doc_no         := l_cux_ready_item.doc_no;
      
        l_line_number := 0; --added by bruce on 20151130
      
      ELSE
        l_cux_ready_item.doc_no := old_doc_no;
      END IF;
      l_line_number                           := l_line_number + 1; --added by bruce on 20151130
      l_cux_ready_item.line_id                := cux.cux_doc_line_seq.nextval;
      l_Cux_Ready_Item.Line_Number            := l_line_number; --added by bruce on 20151130
      l_cux_ready_item.organization_id        := c1.organization_id;
      l_cux_ready_item.doc_type               := p_wip_type;
      l_cux_ready_item.doc_date               := p_doc_date;
      l_cux_ready_item.doc_status             := '未过账';
      l_cux_ready_item.wip_entity_id          := c1.wip_entity_id;
      l_cux_ready_item.wip_entity_name        := c1.wip_entity_name;
      l_cux_ready_item.primary_item_id        := c1.primary_item_id;
      l_cux_ready_item.supply_subinventory    := c1.sub;
      l_cux_ready_item.supply_locator_id      := c1.inventory_location_id;
      l_cux_ready_item.supply_loc_code        := c1.loc;
      l_cux_ready_item.now_qty                := p_now_qty;
      l_cux_ready_item.inventory_item_id      := c1.inventory_item_id;
      l_cux_ready_item.item_primary_uom_code  := c1.item_primary_uom_code;
      l_cux_ready_item.operation_seq_num      := c1.operation_seq_num;
      l_cux_ready_item.operation_code         := c1.operation_code;
      l_cux_ready_item.quantity_per_assembly  := c1.quantity_per_assembly;
      l_cux_ready_item.component_yield_factor := c1.component_yield_factor;
      l_cux_ready_item.required_quantity      := c1.required_quantity;
      l_cux_ready_item.quantity_issued        := c1.quantity_issued;
      l_cux_ready_item.iss_ready_qty          := c1.iss_ready_qty;
      l_cux_ready_item.creation_date          := SYSDATE;
      l_cux_ready_item.created_by             := fnd_global.user_id;
      l_cux_ready_item.last_update_date       := SYSDATE;
      l_cux_ready_item.last_updated_by        := fnd_global.user_id;
      l_cux_ready_item.last_update_login      := fnd_global.login_id;
      l_cux_ready_item.create_lot             := c1.create_lot;
      l_lot                                   := c1.create_lot; --added by bruce on 20151029
      l_cux_ready_item.transfer_status        := 'WAITING';
      
      l_cux_ready_item.material_batch1        := c1.material_batch1;
      l_cux_ready_item.material_batch2        := c1.material_batch2;
      l_cux_ready_item.special_order          := c1.special_order;
      l_cux_ready_item.special_number         := c1.special_number;
      -- 目标子库货位为线边仓
      IF p_wip_type IN ('拉式退料', '拉式超领') THEN
      
        IF p_organization_id = 84 THEN
          l_cux_ready_item.to_sub := 'GX01';
        ELSIF p_organization_id = 83 THEN
          l_cux_ready_item.to_sub := 'HX01';
        END IF;
      
        BEGIN
          SELECT mil.inventory_location_id, mil.segment1
            INTO l_cux_ready_item.to_locator_id,
                 l_cux_ready_item.to_loc_code
            FROM mtl_item_locations mil
           WHERE mil.organization_id = p_organization_id
             AND nvl(mil.enabled_flag, 'N') = 'Y'
             AND mil.segment1 = c1.wip_entity_name;
        
        EXCEPTION
          WHEN OTHERS THEN
            NULL;
        END;
      END IF;
    
      -- 拉式退料由原先的线边仓退回材料仓
      IF l_cux_ready_item.doc_type = '拉式退料' THEN
        l_cux_ready_item.supply_subinventory := l_cux_ready_item.to_sub;
        l_cux_ready_item.supply_locator_id   := l_cux_ready_item.to_locator_id;
        l_cux_ready_item.supply_loc_code     := l_cux_ready_item.to_loc_code;
      
        l_cux_ready_item.to_locator_id := c1.inventory_location_id;
        l_cux_ready_item.to_loc_code   := c1.loc;
        l_cux_ready_item.to_sub        := c1.sub;
      END IF;
    
      /**
      * 计算备料数
      */
      IF l_cux_ready_item.doc_type IN
         ('推式超领', '拉式超领', '推式退料(零单)') THEN
        l_cux_ready_item.ready_qty := NULL;
      
      ELSE
      
        IF l_cux_ready_item.doc_type = '推式退料(负单)' THEN
          BEGIN
            SELECT nvl(wro.quantity_issued, 0)
              INTO l_onhand_qty
              FROM wip_requirement_operations wro --任务领料需求发放表
             WHERE wro.organization_id = p_organization_id
               AND wro.wip_entity_id = c1.wip_entity_id
               AND wro.inventory_item_id = c1.inventory_item_id
               AND wro.operation_seq_num = c1.operation_seq_num
               AND rownum = 1;
          EXCEPTION
            WHEN OTHERS THEN
              l_onhand_qty := 0;
          END;
        
          l_cux_ready_item.attribute1 := 'Y';
        ELSE
          l_onhand_qty := cux_wip_transactions_pkg.get_available_qty(c1.organization_id,
                                                                     c1.inventory_item_id,
                                                                     l_cux_ready_item.supply_subinventory,
                                                                     l_cux_ready_item.supply_locator_id);
        
        END IF;
        l_ready_qty := 0;
      
        --现有量取最小整数
        IF l_onhand_qty >= c1.iss_ready_qty THEN
          l_ready_qty := c1.iss_ready_qty;
        ELSIF l_onhand_qty > 0 THEN
          l_ready_qty := l_onhand_qty;
        ELSE
          l_ready_qty := 0;
        END IF;
      
        l_cux_ready_item.ready_qty := l_ready_qty;
      
        cux_wip_transactions_pkg.get_wip_qt_qty(p_org_id                => c1.organization_id,
                                                p_wip_id                => c1.wip_entity_id,
                                                p_operation_seq_num     => c1.operation_seq_num,
                                                p_item_id               => c1.inventory_item_id,
                                                p_wip_type              => p_wip_type,
                                                p_required_quantity     => c1.required_quantity,
                                                p_quantity_issued       => nvl(c1.quantity_issued,
                                                                               0),
                                                p_doc_no                => l_cux_ready_item.doc_no,
                                                p_now_qty               => p_now_qty,
                                                p_quantity_per_assembly => c1.quantity_per_assembly,
                                                p_ready_qty             => l_cux_ready_item.ready_qty,
                                                p_qty                   => v_sy_qty);
      
        IF l_cux_ready_item.ready_qty >= v_sy_qty THEN
          l_cux_ready_item.ready_qty := v_sy_qty;
        END IF;
      
      END IF;
    
      -- 判断需求量是否大于0
      IF l_cux_ready_item.ready_qty <= 0 THEN
        l_count := l_count - 1;
      ELSE
      
        l_cux_ready_item.iss_ready_qty := c1.iss_ready_qty;
        INSERT INTO cux_ready_item VALUES l_cux_ready_item;
      
      END IF;
    
      --保留
      IF l_cux_ready_item.doc_type = '拉式退料' THEN
        Cux_Wip_Transactions_Pkg.create_save(l_cux_ready_item.line_id,
                                             p_return_status,
                                             p_return_msg);
        IF p_return_status <> 'S' THEN
          RETURN;
        END IF;
      END IF;
    END LOOP;
  
    --added by bruce on 20151228
    delete from cux.cux_ready_item cri
     where cri.create_lot = l_lot
       and cri.doc_type in ('推式退料(零单)', '推式退料(负单)')
       and exists
     (select 1
              from mtl_secondary_inventories msi
             where msi.organization_id = cri.organization_id
               and msi.secondary_inventory_name = cri.supply_subinventory
               and msi.attribute6 = 'SMTN');
    --end of add by bruce on 20151228
  
    -------
    COMMIT;
  
    p_return_status := 'S';
    p_return_msg    := '下达成功';
  
    IF l_count = 0 THEN
      p_return_status := 'E';
      p_return_msg    := '无明细数据或无库存数量,单据未生成！';
    END IF;
  
    --added by bruce on 20151028
  
    if (p_Return_Status = 'S' AND
       p_Wip_Type IN ('推式退料(负单)',
                       '推式退料(零单)',
                       '推式超领',
                       '拉式超领',
                       '拉式退料')) then
    
      transfer_to_wms(p_doc_no          => '',
                      p_organization_id => p_Organization_Id,
                      p_doc_type        => '',
                      p_lot             => l_lot,
                      p_Return_Status   => p_Return_Status,
                      p_Return_Msg      => p_Return_Msg);
    
    end if;
  
    /*  if (p_Return_Status = 'S' AND p_Wip_Type IN ('推式超领', '拉式超领')) then
    
      process_full_prepared(p_lot             => '',
                            p_organization_id => p_Organization_Id,
                            p_wip_id          => p_wip_id,
                            p_wip_name        => p_Wip_Name,
                            p_Return_Status   => p_Return_Status,
                            p_Return_Msg      => p_Return_Msg);
    
      if (p_Return_Status = 'S') then
        transfer_to_wms(p_doc_no          => '',
                        p_organization_id => p_Organization_Id,
                        p_doc_type        => '',
                        p_lot             => l_lot,
                        p_Return_Status   => p_Return_Status,
                        p_Return_Msg      => p_Return_Msg);
      
      end if;
    end if;*/
  
    --end of add by bruce on 20151028
  
  END;
  /*===============================================================
    *    Program Name:   create_feeding_doc
    *    Author      :   Felix.Liu
    *    Date        :   2015-07-30
    *    Purpose     :   创建完工入库单
    *    Parameters  :
    *             in       p_organization_id      组织ID
    *             In       p_wip_name            工单name
    *             in       p_wip_type            单据类型
    *             In       p_doc_date            日期
    *             in       p_now_qty             下达数量
                  in       p_sub                 子库
                  in       p_loc                 货位
                  out      p_return_status       返回(S/E)
                  out      p_error_mess          返回信息
    *    Update History
    *    Version    Date         Name            Description
    *    --------  ----------  ---------------  --------------------
    *     V1.1     2015-07-30   Felix.Liu          Creation
    *
  ===============================================================*/
  PROCEDURE Create_Complete_Doc(p_Organization_Id NUMBER,
                                p_Wip_Name        VARCHAR2,
                                p_Wip_Type        VARCHAR2,
                                p_Doc_Date        DATE,
                                p_Now_Qty         NUMBER,
                                p_Sub             VARCHAR2,
                                p_Loc             VARCHAR2,
                                p_Return_Status   OUT VARCHAR2,
                                p_Return_Msg      OUT VARCHAR2) AS
    l_lot              varchar2(100);
    l_Cux_Ready_Item   Cux.Cux_Ready_Item%ROWTYPE;
    Old_Sub            VARCHAR2(100);
    Old_Operation_Code VARCHAR2(100);
    Old_Doc_No         VARCHAR2(100);
    l_Count            NUMBER := 0;
    Have_Count         NUMBER := 0;
    Is_Next            VARCHAR2(1) := 'Y';
    l_To_Sub           VARCHAR2(100);
    Is_Have            NUMBER;
    l_Ready_Qty        NUMBER;
    l_Onhand_Qty       NUMBER;
    l_Have             NUMBER := 0;
    l_Over_Qty         NUMBER;
    l_Is_Loc_Con       NUMBER := 0;
    l_Index            NUMBER := 0;
  
    v_Seq    NUMBER;
    v_Qty    NUMBER;
    v_Sy_Qty NUMBER;
    v_Flag   VARCHAR2(10); --是否成品仓标识
  
    CURSOR c_Line IS
      SELECT Wdj.Organization_Id,
             Wdj.Wip_Entity_Id,
             Wdj.Wip_Entity_Name,
             Wdj.Primary_Item_Id,
             Wdj.Attribute3 Ws_Code,
             Wdj.Start_Quantity,
             NULL Inventory_Item_Id,
             NULL Com_Item_Code,
             Msi.Primary_Uom_Code Item_Primary_Uom_Code,
             
             (SELECT Wo.Operation_Seq_Num
                FROM Wip_Operations Wo
               WHERE Wo.Organization_Id = Wdj.Organization_Id
                 AND Wo.Wip_Entity_Id = Wdj.Wip_Entity_Id
                 AND Wo.Next_Operation_Seq_Num IS NULL) Operation_Seq_Num, -- 工序
                 
             NULL Operation_Code,
             NULL Quantity_Per_Assembly,
             NULL Component_Yield_Factor,
             NULL Required_Quantity,
             Wdj.Quantity_Completed Quantity_Issued,
             Cux_Wip_Transactions_Pkg.Get_Wip_Over_Qty(Wdj.Organization_Id,
                                                       Wdj.Wip_Entity_Id) Iss_Ready_Qty,
             (SELECT Subinventory_Code
                FROM Mtl_Item_Sub_Defaults Mis
               WHERE Mis.Organization_Id = Wdj.Organization_Id
                 AND Mis.Inventory_Item_Id = Wdj.Primary_Item_Id
                 AND Rownum = 1) Sub,
                 
             (SELECT Mil.Segment1
                FROM Mtl_Item_Loc_Defaults Mild, Mtl_Item_Locations Mil
               WHERE 1 = 1
                 AND Mild.Locator_Id = Mil.Inventory_Location_Id
                 AND Mild.Organization_Id = Mil.Organization_Id
                 AND Nvl(Mil.Enabled_Flag, 'N') = 'Y'
                 AND Mild.Organization_Id = Wdj.Organization_Id
                 AND Mild.Inventory_Item_Id = Wdj.Primary_Item_Id
                 AND Rownum = 1) Loc
             
            ,
             (SELECT Mild.Locator_Id
                FROM Mtl_Item_Loc_Defaults Mild
               WHERE 1 = 1
                 AND Mild.Organization_Id = Wdj.Organization_Id
                 AND Mild.Inventory_Item_Id = Wdj.Primary_Item_Id
                 AND Rownum = 1) Inventory_Location_Id,
             To_Char(Systimestamp, 'YYYYMMDDHH24MISSFF4') Create_Lot,
             1 Replace_Flag
      
        FROM Wip_Discrete_Jobs_v          Wdj,
             Mtl_System_Items_Vl          Msi,
             Org_Organization_Definitions Ood
       WHERE 1 = 1
         AND Msi.Organization_Id = Wdj.Organization_Id
         AND Msi.Inventory_Item_Id = Wdj.Primary_Item_Id
         AND Ood.Organization_Id = Wdj.Organization_Id
         AND Wdj.Wip_Entity_Name = p_Wip_Name
         AND Wdj.Organization_Id = p_Organization_Id
         AND p_Wip_Type = '完工入库'
       ORDER BY Operation_Code, Sub, Replace_Flag;
  
    v_Count             NUMBER;
    v_Ready_Qty         NUMBER;
    v_Create_Lot        VARCHAR2(100);
    v_Create_Lot_New    VARCHAR2(100);
    v_Ready_Qty_New     NUMBER;
    v_Required_Quantity NUMBER;
    v_Required_Item_Qty NUMBER;
    v_Ready_Item_b_Qty  NUMBER;
    v_Ready_Qty_b       NUMBER;
    v_Ready_Qty_s       NUMBER;
    v_Quantity_Issued   NUMBER;
    v_Doc_No            VARCHAR2(100);
    v_Create_Lot_1      VARCHAR2(100);
    v_Rec               Inv_Reservation_Global.Mtl_Reservation_Rec_Type;
    x_Msg_Count         NUMBER;
    x_Msg_Data          VARCHAR2(240);
    p_Dummy_Sn          Inv_Reservation_Global.Serial_Number_Tbl_Type;
    x_Status            VARCHAR2(1);
  BEGIN
    SELECT Cux.Cux_Doc_Line_Seq.Nextval INTO v_Seq FROM Dual;
  
    -- 校验通过,开始备料
    IF Is_Next = 'Y' THEN
    
      FOR C1 IN c_Line LOOP
        l_Count          := l_Count + 1;
        l_Cux_Ready_Item := NULL;
      
        -- 默认收货子库必须存在
        IF C1.Sub IS NULL THEN
          p_Return_Status := 'E';
          p_Return_Msg    := p_Return_Msg || '单据下达失败,子库[' || C1.Sub ||
                             ']为空,请检查:' || Chr(10);
          ROLLBACK;
          RETURN;
        END IF;
      
        -- 根据 工序,子库进行分单
        IF (Old_Sub IS NULL AND Old_Operation_Code IS NULL) OR
           (Old_Sub <> C1.Sub) OR (Old_Operation_Code <> C1.Operation_Code) THEN
        
          IF p_Organization_Id = 83 AND p_Wip_Type IN ('完工入库') THEN
            l_Cux_Ready_Item.Doc_No := 'OR' || Cux_Doc_Or_Seq_Jw.Nextval;
          
          ELSIF p_Organization_Id = 84 AND p_Wip_Type IN ('完工入库') THEN
            l_Cux_Ready_Item.Doc_No := 'OR' || Cux_Doc_Or_Seq_Fw.Nextval;
          END IF;
        
          Old_Sub            := C1.Sub;
          Old_Operation_Code := C1.Operation_Code;
          Old_Doc_No         := l_Cux_Ready_Item.Doc_No;
        ELSE
          l_Cux_Ready_Item.Doc_No := Old_Doc_No;
        END IF;
      
        l_Cux_Ready_Item.Line_Id         := Cux.Cux_Doc_Line_Seq.Nextval;
        l_Cux_Ready_Item.Organization_Id := C1.Organization_Id;
        l_Cux_Ready_Item.Doc_Type        := p_Wip_Type;                     -- 单据类型
        l_Cux_Ready_Item.Doc_Date        := p_Doc_Date;                     -- 单据日期
        l_Cux_Ready_Item.Doc_Status      := '未过账';                       -- 单据状态
        l_Cux_Ready_Item.Wip_Entity_Id   := C1.Wip_Entity_Id;               -- 工单ID    
        l_Cux_Ready_Item.Wip_Entity_Name := C1.Wip_Entity_Name;             -- 工单名
        l_Cux_Ready_Item.Primary_Item_Id := C1.Primary_Item_Id;             -- 组件ID
      
        l_Cux_Ready_Item.Supply_Subinventory    := C1.Sub;                  -- 默认收货子库
        l_Cux_Ready_Item.Supply_Locator_Id      := C1.Inventory_Location_Id;-- 默认收货货位   
        l_Cux_Ready_Item.Supply_Loc_Code        := C1.Loc;                  -- 默认收货货位   
        l_Cux_Ready_Item.Now_Qty                := p_Now_Qty;               -- 本次入库量
        l_Cux_Ready_Item.Inventory_Item_Id      := C1.Inventory_Item_Id;    -- 装配件ID
        l_Cux_Ready_Item.Item_Primary_Uom_Code  := C1.Item_Primary_Uom_Code;-- 单位 
        l_Cux_Ready_Item.Operation_Seq_Num      := C1.Operation_Seq_Num;    -- 工序
        l_Cux_Ready_Item.Operation_Code         := C1.Operation_Code;       -- 
        l_Cux_Ready_Item.Quantity_Per_Assembly  := C1.Quantity_Per_Assembly;
        l_Cux_Ready_Item.Component_Yield_Factor := C1.Component_Yield_Factor;
        l_Cux_Ready_Item.Required_Quantity      := C1.Required_Quantity;
        l_Cux_Ready_Item.Quantity_Issued        := C1.Quantity_Issued;      -- 已完工数量
        l_Cux_Ready_Item.Iss_Ready_Qty          := C1.Iss_Ready_Qty;        -- 可完工数量
        l_Cux_Ready_Item.Creation_Date          := SYSDATE;
        l_Cux_Ready_Item.Created_By             := Fnd_Global.User_Id;
        l_Cux_Ready_Item.Last_Update_Date       := SYSDATE;
        l_Cux_Ready_Item.Last_Updated_By        := Fnd_Global.User_Id;
        l_Cux_Ready_Item.Last_Update_Login      := Fnd_Global.Login_Id;
        l_Cux_Ready_Item.Create_Lot             := C1.Create_Lot;
        l_lot                                   := C1.Create_Lot; --ADDED BY BRUCE ON 20151030
        l_Cux_Ready_Item.Transfer_Status        := 'WAITING';
      
        l_Cux_Ready_Item.Ready_Qty     := p_Now_Qty;              -- 本次完工数量
        l_Cux_Ready_Item.Iss_Ready_Qty := C1.Quantity_Issued;     -- 已完工数量
      
        IF l_Cux_Ready_Item.Ready_Qty <= 0 THEN
           l_Count := l_Count - 1;
        ELSE
           l_Cux_Ready_Item.Iss_Ready_Qty := C1.Iss_Ready_Qty;
           INSERT INTO Cux_Ready_Item VALUES l_Cux_Ready_Item;
        END IF;
        
      END LOOP;
      -------
      COMMIT;
    
      p_Return_Status := 'S';
      p_Return_Msg    := '下达成功';
      IF l_Count = 0 THEN
        p_Return_Status := 'E';
        p_Return_Msg    := '无明细数据或无库存数量,单据未生成！';
      END IF;
    
      --added by bruce on 20151028
      /*
        if (p_Return_Status = 'S') then
        
          transfer_to_wms(p_doc_no          => '',
                          p_organization_id => p_Organization_Id,
                          p_doc_type        => '',
                          p_lot             => l_lot,
                          p_Return_Status   => p_Return_Status,
                          p_Return_Msg      => p_Return_Msg);
        
        end if;
      */
      --end of add by bruce on 20151028
    
    END IF;
  END;

  /*===============================================================
  *    Program Name:   process_trx
  *    Author      :   Felix.Liu
  *    Date        :   2014-08-12
  *    Purpose     :   过帐完工入库单
  *    Parameters  :
  *             in       p_organization_id    组织ID
  *             In       p_doc_no            单据号
                in       p_doc_type          单据类型
  *             out      p_return_status     处理结果
                out      p_return_msg         处理信息
  *    Update History
  *    Version    Date         Name            Description
  *    --------  ----------  ---------------  --------------------
  *     V1.1     2015-07-31   Felix.Liu          Creation
  *
  ===============================================================*/
  PROCEDURE Post_Complete_Doc(p_Organization_Id NUMBER,
                              p_Doc_No          VARCHAR2,
                              p_Doc_Type        VARCHAR2,
                              p_Return_Status   OUT VARCHAR2,
                              p_Return_Msg      OUT VARCHAR2) IS
    l_Iface_Rec               Inv.Mtl_Transactions_Interface%ROWTYPE;
    l_User_Id                 NUMBER := Fnd_Global.User_Id;
    l_Return_Count            NUMBER;
    x_Msg_Count               NUMBER;
    Xx_Msg_Data               VARCHAR2(4000);
    x_Trans_Count             NUMBER;
    Inv_Qty                   NUMBER;
    l_Rem_Qty                 NUMBER;
    l_Auto_Compute_Final_Comp NUMBER;
  
    CURSOR c_Lines IS
      SELECT Cri.Organization_Id,
             Cri.Line_Id,
             Cri.Doc_No,
             Cri.Wip_Entity_Id,
             Cri.Wip_Entity_Name,
             Cri.Supply_Subinventory,
             Cri.Supply_Locator_Id,
             Cri.To_Sub,
             Cri.To_Locator_Id,
             Cri.Ready_Qty,
             Cri.Now_Qty,
             Cri.Item_Primary_Uom_Code,
             Cri.Inventory_Item_Id,
             Cri.Primary_Item_Id,
             Cri.Doc_Type,
             Cri.Operation_Seq_Num
        FROM Cux_Ready_Item Cri
       WHERE Cri.Organization_Id = p_Organization_Id
         AND Cri.Doc_No = p_Doc_No
         AND Cri.Doc_Type = p_Doc_Type
         AND ((Cri.Doc_Type IN ('推式退料(负单)', '推式退料(零单)') AND
             Nvl(Cri.Attribute1, 'N') = 'Y') OR
             Cri.Doc_Type NOT IN
             ('推式退料(负单)', '推式退料(零单)', '推式超领', '拉式超领') OR
             (Cri.Doc_Type IN ('推式超领', '拉式超领') AND
             Nvl(Cri.Attribute1, 'N') = 'Y'))
         AND ((Cri.Ready_Qty > 0 AND Cri.Doc_Type <> '完工入库') OR
             (Cri.Now_Qty > 0 AND Cri.Doc_Type = '完工入库'))
            
         AND Cri.Pro_Trx_Id IS NULL;
  
  BEGIN
  
    FOR C1 IN c_Lines LOOP
    
      --取消保留
      DELETE Mtl_Reservations Mr
       WHERE Mr.Supply_Source_Name = C1.Doc_No
         AND Mr.Demand_Source_Header_Id = C1.Wip_Entity_Id
         AND Mr.Demand_Source_Line_Id = C1.Line_Id
         AND Mr.Organization_Id = p_Organization_Id;
      COMMIT;
    
      l_Iface_Rec                   := NULL;
      l_Iface_Rec.Last_Update_Date  := SYSDATE;
      l_Iface_Rec.Last_Updated_By   := l_User_Id;
      l_Iface_Rec.Creation_Date     := SYSDATE;
      l_Iface_Rec.Created_By        := l_User_Id;
      l_Iface_Rec.Last_Update_Login := Fnd_Global.Login_Id;
    
      l_Iface_Rec.Transaction_Type_Id   := 44; --WIP 完工
      l_iface_rec.final_completion_flag := 'Y';
      l_Iface_Rec.Operation_Seq_Num     := C1.Operation_Seq_Num;
      l_Iface_Rec.Waybill_Airbill       := C1.Operation_Seq_Num;
    
      BEGIN
        SELECT Nvl(Wp.Auto_Compute_Final_Completion, Wip_Constants.No)
          INTO l_Auto_Compute_Final_Comp
          FROM Wip_Parameters Wp
         WHERE Wp.Organization_Id = l_Iface_Rec.Organization_Id;
      EXCEPTION
        WHEN OTHERS THEN
          l_Auto_Compute_Final_Comp := Wip_Constants.No;
      END;
    
      Cux_Wip_Transactions_Pkg.Get_Rem_Qty(p_Org_Id  => C1.Organization_Id,
                                           p_Wip_Id  => C1.Wip_Entity_Id,
                                           p_Rem_Qty => l_Rem_Qty);
    
      IF l_Auto_Compute_Final_Comp = Wip_Constants.Yes AND
         C1.Now_Qty >= l_Rem_Qty THEN
        l_Iface_Rec.Final_Completion_Flag := 'Y';
      ELSE
        l_Iface_Rec.Final_Completion_Flag := 'N';
      END IF;
    
      --mhw end 2014-10-12
    
      l_Iface_Rec.Source_Code           := '入库单';
      l_Iface_Rec.Transaction_Reference := C1.Doc_Type || '[入库单号:' ||
                                           C1.Doc_No || ']';
    
      SELECT Mtl_Material_Transactions_s.Nextval
        INTO l_Iface_Rec.Transaction_Interface_Id
        FROM Dual;
    
      l_Iface_Rec.Transaction_Header_Id := l_Iface_Rec.Transaction_Interface_Id;
      l_Iface_Rec.Transaction_Mode      := 3;
      l_Iface_Rec.Process_Flag          := 1;
      l_Iface_Rec.Organization_Id       := C1.Organization_Id;
      l_Iface_Rec.Inventory_Item_Id     := C1.Primary_Item_Id;
      l_Iface_Rec.Subinventory_Code     := Nvl(C1.To_Sub,
                                               C1.Supply_Subinventory);
      l_Iface_Rec.Locator_Id            := Nvl(C1.To_Locator_Id,
                                               C1.Supply_Locator_Id);
      l_Iface_Rec.Transaction_Quantity  := C1.Now_Qty;
      l_Iface_Rec.Primary_Quantity      := C1.Now_Qty;
      l_Iface_Rec.Transfer_Organization := C1.Organization_Id;
      l_Iface_Rec.Transaction_Uom       := C1.Item_Primary_Uom_Code;
      l_Iface_Rec.Transaction_Date      := SYSDATE;
      l_Iface_Rec.Source_Header_Id      := C1.Wip_Entity_Id;
      l_Iface_Rec.Transaction_Source_Id := C1.Wip_Entity_Id; --wip_entity_id
      l_Iface_Rec.Source_Line_Id        := C1.Line_Id;
    
      INSERT INTO Inv.Mtl_Transactions_Interface VALUES l_Iface_Rec;
    
      IF C1.Doc_Type IN ('完工入库') THEN
        --平均成本方法下还需要插入以下接口表,而且是每道工序都需要插入，不仅仅是最后一道工序
        INSERT INTO Cst_Comp_Snap_Interface
          (Transaction_Interface_Id,
           Wip_Entity_Id,
           Operation_Seq_Num,
           Last_Update_Date,
           Last_Updated_By,
           Creation_Date,
           Created_By,
           Quantity_Completed,
           Primary_Quantity)
        
          SELECT l_Iface_Rec.Transaction_Interface_Id,
                 Wo.Wip_Entity_Id,
                 Wo.Operation_Seq_Num,
                 SYSDATE,
                 l_User_Id,
                 SYSDATE,
                 l_User_Id,
                 Wo.Quantity_Completed,
                 C1.Now_Qty
            FROM Wip_Operations Wo
           WHERE Wo.Organization_Id = C1.Organization_Id
             AND Wo.Wip_Entity_Id = C1.Wip_Entity_Id
           ORDER BY Wo.Operation_Seq_Num;
      
      END IF;
    
      Fnd_Msg_Pub.Initialize;
      p_Return_Status := Fnd_Api.g_Ret_Sts_Success;
    
      l_Return_Count := Inv_Txn_Manager_Pub.Process_Transactions(p_Api_Version      => 1.0,
                                                                 p_Init_Msg_List    => Fnd_Api.g_False,
                                                                 p_Commit           => Fnd_Api.g_False,
                                                                 p_Validation_Level => Fnd_Api.g_Valid_Level_Full,
                                                                 x_Return_Status    => p_Return_Status,
                                                                 x_Msg_Count        => x_Msg_Count,
                                                                 x_Msg_Data         => Xx_Msg_Data,
                                                                 x_Trans_Count      => x_Trans_Count,
                                                                 p_Table            => 1, --1==MTI,2==MMTT
                                                                 p_Header_Id        => l_Iface_Rec.Transaction_Header_Id);
    
      -- 物料事务处理
      IF l_Return_Count = -1 OR
         p_Return_Status <> Fnd_Api.g_Ret_Sts_Success THEN
      
        p_Return_Msg := p_Return_Msg || l_Iface_Rec.Transaction_Header_Id || ': ';
      
        FOR a_Rec IN (SELECT Mti.Transaction_Interface_Id,
                             Mti.Error_Code,
                             Mti.Error_Explanation
                        FROM Mtl_Transactions_Interface Mti
                       WHERE Mti.Transaction_Header_Id =
                             l_Iface_Rec.Transaction_Header_Id) LOOP
        
          p_Return_Status := 'E';
        
          p_Return_Msg := Substr(p_Return_Msg || a_Rec.Error_Code || ':' ||
                                 a_Rec.Error_Explanation || Chr(10),
                                 1,
                                 255);
        END LOOP;
      
        ROLLBACK;
        RETURN;
      
      ELSE
        FOR a_Rec IN (SELECT Mti.Transaction_Interface_Id,
                             Mti.Error_Code,
                             Mti.Error_Explanation
                        FROM Mtl_Transactions_Interface Mti
                       WHERE Mti.Transaction_Header_Id =
                             l_Iface_Rec.Transaction_Header_Id) LOOP
        
          p_Return_Msg := Substr(p_Return_Msg || a_Rec.Error_Code || ':' ||
                                 a_Rec.Error_Explanation || Chr(10),
                                 2000);
        END LOOP;
      
        IF p_Return_Msg IS NOT NULL THEN
          p_Return_Status := 'E';
          ROLLBACK;
          RETURN;
        END IF;
      
        UPDATE Cux_Ready_Item Cri
           SET Cri.Pro_Trx_Id       = l_Iface_Rec.Transaction_Header_Id,
               Cri.Last_Updated_By  = l_User_Id,
               Cri.Last_Update_Date = SYSDATE,
               Cri.Pro_Date         = SYSDATE
         WHERE Cri.Line_Id = C1.Line_Id;
      
      END IF;
    
    END LOOP;
  
    UPDATE Cux_Ready_Item Cri
       SET Cri.Doc_Status       = '已过账',
           Cri.Last_Updated_By  = l_User_Id,
           Cri.Last_Update_Date = SYSDATE
     WHERE Cri.Doc_No = p_Doc_No
       AND Cri.Organization_Id = p_Organization_Id;
  
    p_Return_Msg := '过账成功';
    COMMIT;
  
    --二次处理
    DELETE Mtl_Reservations Mr
     WHERE EXISTS (SELECT 1
              FROM Cux_Ready_Item Cri
             WHERE 1 = 1
               AND Mr.Supply_Source_Name = Cri.Doc_No
               AND Mr.Demand_Source_Header_Id = Cri.Wip_Entity_Id
               AND Mr.Demand_Source_Line_Id = Cri.Line_Id
               AND Mr.Organization_Id = Cri.Organization_Id
               AND cri.doc_no = p_Doc_No
               AND Cri.Doc_Status = '已过账');
    COMMIT;
  
    -- 异常处理
  EXCEPTION
    WHEN OTHERS THEN
      p_Return_Status := 'E';
      p_Return_Msg    := '事物处理异常' || SQLERRM || '代码行:' ||
                         Dbms_Utility.Format_Error_Backtrace;
      ROLLBACK;
  END Post_Complete_Doc;

  PROCEDURE Add_Available_Msg(p_organization_id   number,
                              p_inventory_item_id number,
                              p_sub               varchar2,
                              p_loc               varchar2,
                              p_Onhand_Qty        number,
                              x_onhand_msg        in out varchar2,
                              x_onhand_count      in out number) as
    v_onhand_item varchar2(50);
  begin
    if p_Onhand_Qty = 0 then
      x_onhand_count := x_onhand_count + 1;
    
      if x_onhand_count < 20 then
        select msi.segment1
          into v_onhand_item
          from mtl_system_items_b msi
         where msi.inventory_item_id = p_inventory_item_id
           and msi.organization_id = p_organization_id;
      
        x_onhand_msg := x_onhand_msg || chr(10) || '物料: ' || v_onhand_item ||
                        '; 子库: ' || p_sub;
      
        -- 是否显示货位
        if p_loc is not null then
          x_onhand_msg := x_onhand_msg || '; 货位: ' || p_loc;
        end if;
      end if;
    end if;
  end;
  PROCEDURE transfer_to_wms(p_doc_no          varchar2,
                            p_organization_id number,
                            p_doc_type        varchar2,
                            p_lot             varchar2,
                            p_Return_Status   OUT VARCHAR2,
                            p_Return_Msg      OUT VARCHAR2) is
    --added by bruce on 20151029
    l_doc_type     varchar2(30);
    l_ret_status   varchar2(10) := 'S';
    l_ret_msg      varchar2(100);
    l_inv_sys_flag varchar2(10);
    l_org_id       number;
    l_count_error  number := 0;
    l_error_msg    varchar2(1000);
    cursor cur_docs(p_lot varchar2) is
      select cri.doc_no, cri.doc_type, cri.supply_subinventory
        from cux.cux_ready_item cri
       where cri.create_lot = p_lot
       group by cri.doc_no, cri.doc_type, cri.supply_subinventory;
    --end of add by bruce on 20151029
  
  begin
  
    begin
      select iiv.OPERATING_UNIT
        into l_org_id
        from inv_organization_info_v iiv
       where iiv.ORGANIZATION_ID = p_organization_id;
    exception
      when others then
        l_org_id := -1;
    end;
    for rec_docs in cur_docs(p_lot) loop
    
      select decode(rec_docs.doc_type,
                    '推式备料',
                    'WIP_RLS_PS',
                    '拉式备料',
                    'WIP_RLS_PL',
                    '推式补料',
                    'WIP_RPL_PS',
                    '拉式补料',
                    'WIP_RPL_PL',
                    '推式超领',
                    'WIP_OVR_PS',
                    '拉式超领',
                    'WIP_OVR_PL',
                    '推式退料(负单)',
                    'WIP_NEG_PS',
                    '推式退料(零单)',
                    'WIP_FRG_PS',
                    '拉式退料',
                    'WIP_RTN_PL',
                    '完工入库',
                    'WIP_CMPL',
                    'NA')
        INTO l_doc_type
        from dual;
    
      l_inv_sys_flag := cux_ebs_smtn_integration_pkg.is_inv_sys(p_header_id       => '',
                                                                p_organization_id => p_organization_id,
                                                                p_doc_number      => rec_docs.doc_no,
                                                                p_trx_type        => l_doc_type,
                                                                x_Ret_Sts         => p_Return_Status,
                                                                x_Error_Msg       => p_Return_Msg);
    
      --p_Return_Msg:='y'||rec_docs.doc_no||' '||p_lot||p_organization_id||l_doc_type;
      if (l_inv_sys_flag = 'Y') then
      
        if (l_doc_type in
           ('WIP_RLS_PS', 'WIP_RLS_PL', 'WIP_RPL_PS', 'WIP_RPL_PL')) then
          NULL;
          --取消自动抛转
          /*begin
            Cux_Wip2wms_Transfer_Pkg.Transfer_Wip_Issue_Data(x_Ret_Sts         => l_ret_status,
                                                             x_Error_Msg       => l_ret_msg,
                                                             p_Organization_Id => p_organization_id,
                                                             p_Doc_No          => rec_docs.doc_no);
          exception
            when others then
              p_Return_Msg := '抛转失败：' || rec_docs.doc_no;
          end;*/
        
        elsif (l_doc_type in ('WIP_NEG_PS' /*, 'WIP_FRG_PS'*/)) then
        
          Cux_Wip2wms_Transfer_Pkg.Transfer_Wip_Return_Data(x_Ret_Sts         => l_ret_status,
                                                            x_Error_Msg       => l_ret_msg,
                                                            p_Organization_Id => p_organization_id,
                                                            p_Doc_No          => rec_docs.doc_no);
        elsif (l_doc_type in ('WIP_OVR_PS', 'WIP_OVR_PL', 'WIP_FRG_PS')) then
          --超领以及推式零推都是界面手工抛转
          /* update cux_ready_item cri
            set cri.transfer_status = 'NO_TRANSFER'
          where cri.doc_no = rec_docs.doc_no
            AND cri.create_lot = p_lot;*/
          NULL;
        elsif (l_doc_type = 'WIP_RTN_PL') then
          --中试车间
          update cux_ready_item cri
             set cri.transfer_status = 'NO_TRANSFER'
           where cri.doc_no = rec_docs.doc_no
             AND cri.create_lot = p_lot;
        
        end if;
      
        if (l_ret_status = 'S') then
          NULL;
        else
          update cux_ready_item cri
             set cri.Attribute9 = l_ret_msg
           where cri.doc_no = rec_docs.doc_no
             AND cri.create_lot = p_lot;
        
          l_count_error := l_count_error + 1;
          l_error_msg   := l_error_msg || ' ' || rec_docs.doc_no;
        end if;
      
      else
      
        if (p_Return_Status = 'E') then
          null;
          --p_Return_Msg    := p_Return_Msg || ' ' || l_ret_msg;
          update cux_ready_item cri
             set cri.Attribute9 = p_Return_Msg
           where cri.doc_no = rec_docs.doc_no
             AND cri.create_lot = p_lot;
        
        else
          -- p_Return_Msg:='y'||rec_docs.doc_no||' '||p_lot||p_organization_id||l_doc_type;
          update cux_ready_item cri
             set cri.transfer_status = 'NO_TRANSFER'
           where cri.doc_no = rec_docs.doc_no
             AND cri.create_lot = p_lot;
        
          /* if(p_Return_Status='S') then
            p_Return_Msg:='下达成功，未抛转';
          end if;*/
        
        end if;
      
      end if;
    end loop;
    p_Return_Status := 'S';
    if (l_count_error > 0) then
      p_Return_Msg := '下达成功 ' || l_error_msg || ' 抛转失败！';
    else
      p_Return_Msg := '下达成功';
    end if;
  
    commit;
  exception
    when others then
      p_Return_Status := 'E';
      p_Return_Msg    := '下达成功，抛转发生异常：' || SQLERRM;
  end;
  PROCEDURE process_full_prepared(p_lot             varchar2,
                                  p_organization_id number,
                                  p_wip_id          number,
                                  p_wip_name        varchar2,
                                  p_Return_Status   OUT VARCHAR2,
                                  p_Return_Msg      OUT VARCHAR2) is
  
    CURSOR CUR_OPER_SUB IS
      select wo.operation_seq_num,
             wo.standard_operation_id,
             cri.supply_subinventory
        from cux_ready_item cri, wip_operations wo, fnd_lookup_values flv
       WHERE cri.wip_entity_id = p_wip_id
         and wo.wip_entity_id = cri.wip_entity_id
         and cri.operation_seq_num = wo.operation_seq_num
         and flv.lookup_type = 'CUX_INV_TRX_TYPE'
         AND flv.language = userenv('lang')
         and flv.meaning = cri.doc_type
         and nvl(flv.attribute3, 'N') = 'Y'
       GROUP BY wo.operation_seq_num,
                wo.standard_operation_id,
                cri.supply_subinventory;
  
    CURSOR CUR_ITEM(p_seq_num number, p_subinv varchar2) IS
      /*SELECT cri.inventory_item_id, sum(cri.ready_qty) ready_qty
        from cux_ready_item cri, fnd_lookup_values flv
       where cri.wip_entity_id = p_wip_id
         and cri.operation_seq_num = p_seq_num
         and cri.supply_subinventory = p_subinv
         and flv.lookup_type = 'CUX_INV_TRX_TYPE'
         AND flv.language = userenv('lang')
         and flv.meaning = cri.doc_type
         and nvl(flv.attribute3, 'N') = 'Y'
       GROUP BY cri.inventory_item_id;*/
        SELECT wro.inventory_item_id,
             nvl(sum(wro.required_quantity), 0) required_qty
        from wip_requirement_operations wro, wip_discrete_jobs wdj
       where wro.wip_entity_id = p_wip_id
         and wro.wip_entity_id = wdj.wip_entity_id
         and wro.organization_id = wdj.organization_id
         and wro.operation_seq_num = p_seq_num
         and exists
       (select 1
                from CUX_SUB_LOC_SET_B cb
               where cb.inventory_item_id = wro.inventory_item_id
                 and cb.organization_id = wro.organization_id
                 and cb.ws_code = nvl(wdj.attribute3, cb.ws_code)
                 and cb.sub = p_subinv)
       GROUP BY wro.inventory_item_id;
  
    CURSOR CUR_OPER_SUB2(p_comb_id number) IS
      select bso.operation_code,
             wo.standard_operation_id,
             cri.supply_subinventory
        from cux_ready_item          cri,
             wip_operations          wo,
             bom_standard_operations bso,
             fnd_lookup_values       flv
       WHERE cri.wip_entity_id in
             (select cmb.wip_entity_id
                from cux.cux_wip_combination_details cmb
               where cmb.combination_id = p_comb_id) --此子查询等罗军修改合并工单表结构后调整
         and wo.wip_entity_id = cri.wip_entity_id
         and cri.operation_seq_num = wo.operation_seq_num
         and WO.STANDARD_OPERATION_ID = BSO.STANDARD_OPERATION_ID(+)
         AND NVL(BSO.OPERATION_TYPE, 1) = 1
         AND BSO.LINE_ID IS NULL
         and flv.lookup_type = 'CUX_INV_TRX_TYPE'
         AND flv.language = userenv('lang')
         and flv.meaning = cri.doc_type
         and nvl(flv.attribute3, 'N') = 'Y'
       GROUP BY bso.operation_code,
                wo.standard_operation_id,
                cri.supply_subinventory;
  
    CURSOR CUR_ITEM2(p_seq_code varchar2,
                     p_subinv   varchar2,
                     p_comb_id  number) IS
    /*SELECT cri.inventory_item_id, sum(cri.ready_qty) ready_qty
                                            from cux_ready_item cri, fnd_lookup_values flv
                                           where cri.wip_entity_id in
                                                 (select cmb.wip_entity_id
                                                    from cux.cux_wip_combination_details cmb
                                                   where cmb.combination_id = p_comb_id) --此子查询等罗军修改合并工单表结构后调整
                                                --and cri.operation_seq_num = p_seq_num
                                             and NVL(cri.Operation_Code, 'NA') = NVL(p_seq_code, 'NA')
                                             and cri.supply_subinventory = p_subinv
                                             and flv.lookup_type = 'CUX_INV_TRX_TYPE'
                                             AND flv.language = userenv('lang')
                                             and flv.meaning = cri.doc_type
                                             and nvl(flv.attribute3, 'N') = 'Y'
                                           GROUP BY cri.inventory_item_id;*/
      SELECT wro.inventory_item_id,
             nvl(sum(wro.required_quantity), 0) required_qty
        from wip_requirement_operations wro,
             wip_discrete_jobs          wdj,
             wip_operations             wo,
             bom_standard_operations    bso
       where wro.wip_entity_id in
             (select cmb.wip_entity_id
                from cux.cux_wip_combination_details cmb
               where cmb.combination_id = p_comb_id)
         and wro.wip_entity_id = wo.wip_entity_id
         and wro.wip_entity_id = wdj.wip_entity_id
         and wro.operation_seq_num=wo.operation_seq_num
         and wo.standard_operation_id = BSO.STANDARD_OPERATION_ID(+)
         AND NVL(BSO.OPERATION_TYPE, 1) = 1
         AND BSO.LINE_ID IS NULL
         and nvl(bso.operation_code, 'NA') = NVL(p_seq_code, 'NA')
         and exists
       (select 1
                from CUX_SUB_LOC_SET_B cb
               where cb.inventory_item_id = wro.inventory_item_id
                 and cb.organization_id = wro.organization_id
                 and cb.ws_code = nvl(wdj.attribute3, cb.ws_code)
                 and cb.sub = p_subinv)
       GROUP BY wro.inventory_item_id;
  
    l_full_flag    varchar2(10) := 'Y';
    l_ready_qty    number;
    l_required_qty number;
    l_count        number;
    l_count_exist  number := 0;
    l_comb_number  varchar2(100);
    l_item_full    varchar2(10) := 'Y';
    l_count_full   number;
    l_comb_id      number := 0;
    l_oper_code    varchar2(100);
    l_conc_req_id  number;
    l_user_id      number := fnd_global.USER_ID;
    l_login_id     number := fnd_global.LOGIN_ID;
  BEGIN
    p_Return_Status := 'S';
  
    begin
      select cmb.combination_id /*,cwc.combination_request_id*/
        into l_comb_id /*,l_conc_req_id*/
        from cux.cux_wip_combination_details cmb /*,cux.cux_wip_combinations cwc*/
       where cmb.wip_entity_id = p_wip_id
         and cmb.active_flag = 'Y'
            /* and cmb.combination_id=cwc.combination_id
            */
         and rownum = 1;
    exception
      when others then
        l_comb_id := 0;
    end;
    --p_Return_Msg:='00';
    if (l_comb_id = 0) then
      --p_Return_Msg:='11';
      --if no combination exist, then insert virtual combination data instead.
      --insert combination header
      select cux.cux_wip_combinations_s.nextval into l_comb_id from dual;
      begin
        insert into cux.cux_wip_combinations
          (combination_id,
           org_id,
           organization_id,
           combination_number,
           combination_name,
           entity_count,
           combination_date,
           active_flag,
           cancel_date,
           combination_request_id,
           cancel_request_id)
        values
          (l_comb_id,
           '',
           p_organization_id,
           p_wip_name,
           p_wip_name,
           1,
           sysdate,
           'Y',
           '',
           0,
           '');
      exception
        when others then
          p_Return_Msg := '插入合并单头报错：' || sqlerrm;
          raise Fnd_Api.g_Exc_Error;
      end;
      --insert combination detail
      begin
        insert into cux.cux_wip_combination_details
          (combination_id,
           sequence_no,
           organization_id,
           wip_entity_id,
           wip_entity_name,
           active_flag)
        values
          (l_comb_id, 1, p_organization_id, p_wip_id, p_wip_name, 'Y');
      exception
        when others then
          p_Return_Msg := '插入合并单明细报错：' || sqlerrm;
          raise Fnd_Api.g_Exc_Error;
      end;
      --p_Return_Msg:='222';
      FOR REC_OPER_SUB in CUR_OPER_SUB loop
        --p_Return_Msg:='33';
        l_item_full := 'Y';
        For rec_item in CUR_ITEM(rec_oper_sub.operation_seq_num,
                                 rec_oper_sub.supply_subinventory) loop
        
          begin
            select nvl(sum(cri.ready_qty), 0)
              into l_ready_qty
              from cux_ready_item cri, fnd_lookup_values flv
             where cri.wip_entity_id = p_wip_id
               AND cri.operation_seq_num = rec_oper_sub.operation_seq_num
               and cri.supply_subinventory =
                   rec_oper_sub.supply_subinventory
               and cri.inventory_item_id = rec_item.inventory_item_id
               and cri.doc_type = flv.meaning
               and flv.lookup_type = 'CUX_INV_TRX_TYPE'
               AND flv.language = userenv('lang')
               and nvl(flv.attribute3, 'N') = 'Y';
          exception
            when others then
              p_Return_Msg := '获取备料数量出错：' || rec_oper_sub.operation_seq_num ||
                              rec_item.inventory_item_id;
              raise Fnd_Api.g_Exc_Error;
          end;
        
          -- if (rec_item.ready_qty < l_required_qty) then
            if (l_ready_qty < rec_item.required_qty) then
            l_item_full := 'N';
          end if;
          --l_count_exist:=l_count_exist+1;
        end loop; --item loop
      
        if (l_item_full = 'Y') then
          --all items are full prepared
          --firstly insert virtual records into wip combination table, then get combination_id
          --p_Return_Msg:='44';
          --get operation code
          begin
            select bso.operation_code
              into l_oper_code
              from BOM_STANDARD_OPERATIONS BSO
             where BSO.STANDARD_OPERATION_ID =
                   rec_oper_sub.standard_operation_id
               AND NVL(BSO.OPERATION_TYPE, 1) = 1
               AND BSO.LINE_ID IS NULL;
          exception
            when others then
              l_oper_code := '';
          end;
          --p_Return_Msg:='55';
        
          begin
            select count(1)
              into l_count_exist
              from cux.cux_wip_full_prepare cp
             where cp.combination_id = l_comb_id
               and cp.operation_code = l_oper_code
               and cp.supply_subinventory =
                   rec_oper_sub.supply_subinventory;
          exception
            when others then
              l_count_exist := 1;
          end;
        
          if (l_count_exist = 0) then
            BEGIN
              insert into cux.cux_wip_full_prepare
                (full_prepare_id,
                 combination_id,
                 operation_code,
                 supply_subinventory,
                 latest_prepared_doc,
                 created_by,
                 creation_date,
                 last_updated_by,
                 last_update_date,
                 last_update_login)
              values
                (cux_wip_full_prepare_s.nextval,
                 l_comb_id,
                 l_oper_code,
                 rec_oper_sub.supply_subinventory,
                 '',
                 l_user_id,
                 sysdate,
                 l_user_id,
                 sysdate,
                 l_login_id);
            EXCEPTION
              WHEN OTHERS THEN
                p_Return_Msg := '插入齐套表报错1：' || sqlerrm;
                raise Fnd_Api.g_Exc_Error;
            END;
          end if; --count exist
        
        end if;
      
      end loop; --OPER SUB LOOP
    end if; --l_comb_id
  
    if (l_comb_id > 0) then
      --合并工单以及单一工单虚拟合并号都在这个判断里执行齐套计算
      FOR REC_OPER_SUB2 in CUR_OPER_SUB2(l_comb_id) loop
        --p_Return_Msg:='66';
        l_item_full := 'Y';
        --p_Return_Msg:=REC_OPER_SUB2.operation_code||REC_OPER_SUB2.supply_subinventory||l_comb_id;
        For rec_item2 in CUR_ITEM2(REC_OPER_SUB2.operation_code,
                                   REC_OPER_SUB2.Supply_Subinventory,
                                   l_comb_id) loop
          --p_Return_Msg:='77';
          begin
            select nvl(sum(cri.ready_qty), 0)
              into l_ready_qty
              from cux_ready_item cri, fnd_lookup_values flv
             where cri.wip_entity_id in
                   (select cmb.wip_entity_id
                      from cux.cux_wip_combination_details cmb
                     where cmb.combination_id = l_comb_id)
               AND cri.operation_code = rec_oper_sub2.operation_code
               and cri.supply_subinventory =
                   rec_oper_sub2.supply_subinventory
               and cri.inventory_item_id = rec_item2.inventory_item_id
               and cri.doc_type = flv.meaning
               and flv.lookup_type = 'CUX_INV_TRX_TYPE'
               AND flv.language = userenv('lang')
               and nvl(flv.attribute3, 'N') = 'Y';
          exception
            when others then
              p_Return_Msg := '获取备料数量出错：' || rec_oper_sub2.operation_code ||
                              rec_item2.inventory_item_id;
              raise Fnd_Api.g_Exc_Error;
          end;
          --p_Return_Msg:='88';
          if (l_ready_qty < rec_item2.required_qty) then
            l_item_full := 'N';
          end if;
          --l_count_exist:=l_count_exist+1;
        end loop; --item loop
        --p_Return_Msg:='99';
      
        begin
          select count(1)
            into l_count_exist
            from cux.cux_wip_full_prepare cp
           where cp.combination_id = l_comb_id
             and cp.operation_code = rec_oper_sub2.operation_code
             and cp.supply_subinventory = REC_OPER_SUB2.supply_subinventory;
        exception
          when others then
            l_count_exist := 1;
        end;
      
        if (l_item_full = 'Y' and l_count_exist = 0) then
          --all items are full prepared
        
          /*--get operation code
          begin
            select bso.operation_code
              into l_oper_code
              from BOM_STANDARD_OPERATIONS BSO
             where BSO.STANDARD_OPERATION_ID =
                   REC_OPER_SUB2.standard_operation_id
               AND NVL(BSO.OPERATION_TYPE, 1) = 1
               AND BSO.LINE_ID IS NULL;
          exception
            when others then
              l_oper_code := '';
          end;*/
          --p_Return_Msg:='00';
          BEGIN
            insert into cux.cux_wip_full_prepare
              (full_prepare_id,
               combination_id,
               operation_code,
               supply_subinventory,
               latest_prepared_doc,
               created_by,
               creation_date,
               last_updated_by,
               last_update_date,
               last_update_login)
            values
              (cux_wip_full_prepare_s.nextval,
               l_comb_id,
               rec_oper_sub2.operation_code,
               rec_oper_sub2.supply_subinventory,
               '',
               l_user_id,
               sysdate,
               l_user_id,
               sysdate,
               l_login_id);
          EXCEPTION
            WHEN OTHERS THEN
              p_Return_Msg := '插入齐套表报错2：' || sqlerrm;
              raise Fnd_Api.g_Exc_Error;
          END;
        end if;
        --p_Return_Msg:='yy';
      end loop; --OPER SUB2 LOOP
    end if; --l_comb_id
  
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      p_Return_Status := 'E';
      p_Return_Msg    := '下达成功 ' || p_Return_Msg;
    when others then
      p_Return_Status := 'E';
      p_Return_Msg    := '下达成功，计算齐套发生异常：' || p_Return_Msg || SQLERRM;
  end;

  PROCEDURE cancel_full_prepared(p_lot             varchar2,
                                 p_organization_id number,
                                 p_wip_id          number,
                                 p_wip_name        varchar2,
                                 p_operation_code  varchar2,
                                 p_subinv          varchar2,
                                 p_Return_Status   OUT VARCHAR2,
                                 p_Return_Msg      OUT VARCHAR2) is
    l_comb_id number := 0;
  begin
  
    begin
      select cmb.combination_id
        into l_comb_id
        from cux.cux_wip_combination_details cmb,
             cux.cux_wip_full_prepare        cwp
       where cmb.wip_entity_id = p_wip_id
         and cwp.combination_id = cmb.combination_id
         and rownum = 1;
    exception
      when others then
        l_comb_id := 0;
    end;
  
    if (l_comb_id > 0) then
    
      delete from cux.cux_wip_full_prepare cwp
       where cwp.combination_id = l_comb_id
         and nvl(cwp.operation_code, 'NA') = nvl(p_operation_code, 'NA')
         and cwp.supply_subinventory = p_subinv;
    
    end if;
    COMMIT;
  exception
    WHEN Fnd_Api.g_Exc_Error THEN
      p_Return_Msg := '清除齐套数据失败';
    when others then
      p_Return_Status := 'E';
      p_Return_Msg    := '清除齐套数据发生异常：' || SQLERRM;
    
  end;
  /*
    FUNCTION validate_full_prepared(p_doc_number      varchar2,
                                    p_organization_id number,
                                    p_wip_id          number,
                                    p_wip_name        varchar2) return varchar2 is
      l_full_flag  varchar2(10) := 'Y';
      l_count_full NUMBER;
      l_ready_qty  number;
      CURSOR CUR_REQ IS
        select WRO.INVENTORY_ITEM_ID,
               nvl(sum(WRO.REQUIRED_QUANTITY), 0) REQUIRED_QUANTITY
          from WIP_REQUIREMENT_OPERATIONS WRO
         WHERE WRO.WIP_ENTITY_ID = p_wip_id
         group by WRO.INVENTORY_ITEM_ID;
    begin
      select count(1)
        into l_count_full
        from cux.cux_wip_combinations cwb
       where cwb.wip_entity_name = p_wip_name
         and cwb.prepared_flag = 'Y';
    
      if (l_count_full = 0) then
        l_full_flag := 'O'; --工单本来就未齐套
        return l_full_flag;
      end if;
    
      if (l_count_full > 0) then
        FOR REC_REQ IN CUR_REQ LOOP
        
          begin
            SELECT SUM(nvl(cri.ready_qty, 0))
              into l_ready_qty
              FROM CUX.CUX_READY_ITEM CRI
             WHERE CRI.WIP_ENTITY_ID = p_wip_id
               AND CRI.ORGANIZATION_ID = p_organization_id
               and cri.doc_no <> p_doc_number
               AND CRI.INVENTORY_ITEM_ID = REC_REQ.INVENTORY_ITEM_ID
               and cri.doc_type in ('推式备料',
                                    '拉式备料',
                                    '推式补料',
                                    '拉式补料',
                                    '推式超领',
                                    '拉式超领');
          exception
            when others then
              l_ready_qty := 0;
          end;
          IF (l_ready_qty < REC_REQ.REQUIRED_QUANTITY) THEN
            l_full_flag := 'N';
            EXIT;
          END IF;
        
        END LOOP;
      end if;
      RETURN l_full_flag;
    exception
      WHEN Fnd_Api.g_Exc_Error THEN
        --p_Return_Msg := '下达成功 ' || p_Return_Msg;
        NULL;
      when others then
        RETURN 'N';
    end;
  */
END Cux_Wip_Trans_Pkg;
/
