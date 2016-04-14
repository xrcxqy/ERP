CREATE OR REPLACE PACKAGE Cux_Tms_Test_Pkg AS

  TYPE Array_Table IS TABLE OF VARCHAR2(500) INDEX BY BINARY_INTEGER;

  PROCEDURE Main(Retcode           IN OUT VARCHAR2,
                 Errbuf            IN OUT VARCHAR2,
                 p_Organization_Id IN NUMBER,
                 p_Header_Id       IN NUMBER);

  PROCEDURE Parser_Resp(p_Xml_Str  IN VARCHAR2,
                        l_Retvalue OUT VARCHAR2,
                        l_Message  OUT VARCHAR2);

  PROCEDURE Data_Validation(p_Arr_List   IN OUT Array_Table,
                            p_Data       IN VARCHAR2,
                            p_Max_Length IN NUMBER,
                            p_Label      IN VARCHAR2,
                            p_Need       IN NUMBER DEFAULT 0);
END;
/
CREATE OR REPLACE PACKAGE BODY Cux_Tms_Test_Pkg AS
  PROCEDURE Main(Retcode           IN OUT VARCHAR2,
                 Errbuf            IN OUT VARCHAR2,
                 p_Organization_Id IN NUMBER,
                 p_Header_Id       IN NUMBER) AS
    CURSOR C1 IS
      SELECT Mth.Header_Id t_Header_Id,
             Mth.Organization_Id,
             Oola.Ship_To_Org_Id,
             Oola.Header_Id,
             Mth.Request_Number,
             Ooha.Sold_To_Org_Id,
             MAX(Oola.Invoice_To_Contact_Id) Invoice_To_Contact_Id,
             Oola.Org_Id,
             Ooha.Order_Number
             --,Oola.Ship_Set_Id
            ,
             MAX(Mtl.From_Subinventory_Code) From_Subinventory_Code,
             Mth.Creation_Date,
             (SELECT He.Full_Name
                FROM Fnd_User Fu, Hr_Employees He
               WHERE Fu.Employee_Id = He.Employee_Id
                 AND Fu.User_Id = Mth.Created_By) Full_Name,
             Ooha.Shipping_Instructions Order_Remark, ---备注
             MAX(Col.Fax1) Fax1,
             MAX(Col.Mobile1) Mobile1,
             MAX(Col.Tel1) Tel1,
             MAX(Col.Fax2) Fax2,
             MAX(Col.Mobile2) Mobile2,
             MAX(Col.Tel2) Tel2,
             MAX(Col.Consignee1) Consignee1,
             MAX(Col.Consignee2) Consignee2,
             MAX(Col.City) City,
             MAX(Col.Final_Address) Final_Address,
             MAX(Col.Zip_Code) Zip_Code,
             MAX(Col.Transfer_Unit) Transfer_Unit,
             MAX(Col.Transfer_Zip_Code) Transfer_Zip_Code,
             MAX(Col.Transfer_City) Transfer_City,
             MAX(Col.Transfer_Address) Transfer_Address,
             MAX(Col.Transfer_Consignee1) Transfer_Consignee1,
             MAX(Col.Transfer_Tel1) Transfer_Tel1,
             MAX(Col.Transfer_Fax1) Transfer_Fax1,
             MAX(Col.Transfer_Mobile1) Transfer_Mobile1,
             MAX(Col.Transfer_Consignee2) Transfer_Consignee2,
             MAX(Col.Transfer_Tel2) Transfer_Tel2,
             MAX(Col.Transfer_Fax2) Transfer_Fax2,
             MAX(Col.Transfer_Mobile2) Transfer_Mobile2,
             MAX(Col.Final_Ship) Final_Ship,
             MAX(Col.Consignee) Consignee,
             Coh.Salesrep_Name,
             Coh.Business_Staff,
             MAX(Col.Floor) Floor,
             MAX(Col.Property) Property,
             MAX(Col.Freight_Attribute) Freight_Attribute, --货运属性代码
             MAX(Col.Freight) Freight, --最终货运方式 名称
             MAX(Col.Common_Carrier) Common_Carrier, --承运商 名称
             40 Release_From_Type,
             Ooha.Order_Type_Id,
             Ooha.Shipping_Instructions, /*MAX(Oola.Shipping_Instructions)*/
             max(col.SOURCES_OF_PRODUCTS) l_Shipping_Instructions
        FROM Mtl_Txn_Request_Headers   Mth,
             Mtl_Txn_Request_Lines     Mtl,
             Oe_Order_Lines_All        Oola,
             Oe_Order_Headers_All      Ooha,
             Cux_Om_Line_Interface_v   Col,
             Cux_Om_Header_Interface_v Coh,
             Mtl_System_Items_b        Msi ---RU
       WHERE Mth.Header_Id = Mtl.Header_Id
         AND Oola.Header_Id = Ooha.Header_Id
         AND Mtl.Txn_Source_Line_Id = Oola.Line_Id
         AND Oola.Line_Id = Col.Order_Line_Id(+)
         AND Ooha.Header_Id = Coh.Order_Header_Id(+)
         AND Mtl.Header_Id = p_Header_Id
         AND Mtl.Organization_Id = p_Organization_Id
         AND Col.Freight_Attribute NOT IN ('20', '40', '50')
         AND Mtl.Inventory_Item_Id = Msi.Inventory_Item_Id ---RU
         AND Mtl.Organization_Id = Msi.Organization_Id ---RU
         AND Nvl(Msi.Attribute12, 'N') = 'N' ---RU
         AND Ooha.Order_Type_Id NOT IN
             (SELECT c.Transaction_Type_Id
                FROM Cux_Om_Transaction_Type_v c
               WHERE c.Name IN ('111借机转销售订单', '112借机转销售订单'))
      /*         AND NOT EXISTS (SELECT 'X'
       FROM CUX.CUX_TMS_HEADER_INTERFACE CTHI
      WHERE CTHI.REQUEST_HEADER_ID = MTH.HEADER_ID
        AND CTHI.RELEASE_FROM_TYPE = 40)*/
       GROUP BY Mth.Header_Id,
                Mth.Organization_Id,
                Oola.Ship_To_Org_Id,
                Oola.Header_Id,
                Mth.Request_Number,
                Ooha.Sold_To_Org_Id,
                /*           OOLA.INVOICE_TO_CONTACT_ID,*/
                Oola.Org_Id,
                Ooha.Order_Number
                --,Oola.Ship_Set_Id
               ,
                Mth.Creation_Date,
                Mth.Created_By,
                Coh.Order_Remark,
                Coh.Salesrep_Name,
                Coh.Business_Staff,
                40,
                Ooha.Order_Type_Id,
                Ooha.Shipping_Instructions
      UNION ALL
      SELECT DISTINCT Mth.Header_Id t_Header_Id,
                      Mth.Organization_Id,
                      NULL Ship_To_Org_Id,
                      NULL Header_Id,
                      Mth.Req_Number Request_Number,
                      NULL Sold_To_Org_Id,
                      NULL Invoice_To_Contact_Id,
                      (SELECT DISTINCT Ood.Operating_Unit
                         FROM Org_Organization_Definitions Ood
                        WHERE Ood.Organization_Id = Mth.Organization_Id) Org_Id,
                      NULL Order_Number
                      -- ,NULL Ship_Set_Id
                     ,
                      Mtl.Subinventory_Code From_Subinventory_Code,
                      Mth.Last_Update_Date Creation_Date,
                      (SELECT He.Full_Name
                         FROM Fnd_User Fu, Hr_Employees He
                        WHERE Fu.Employee_Id = He.Employee_Id
                          AND Fu.User_Id = Mth.Created_By) Full_Name,
                      Mth.Remark Order_Remark,
                      NULL Fax1,
                      NULL Mobile1,
                      NULL Tel1,
                      NULL Fax2,
                      NULL Mobile2,
                      NULL Tel2,
                      NULL Consignee1,
                      NULL Consignee2,
                      NULL City,
                      NULL Final_Address,
                      NULL Zip_Code,
                      Mth.Attribute3 Transfer_Unit,
                      NULL Transfer_Zip_Code,
                      NULL Transfer_City,
                      NULL Transfer_Address,
                      NULL Transfer_Consignee1,
                      NULL Transfer_Tel1,
                      NULL Transfer_Fax1,
                      NULL Transfer_Mobile1,
                      NULL Transfer_Consignee2,
                      NULL Transfer_Tel2,
                      NULL Transfer_Fax2,
                      NULL Transfer_Mobile2,
                      Mtl.To_Subinventory_Code Final_Ship,
                      NULL Consignee,
                      NULL Salesrep_Name,
                      NULL Business_Staff,
                      '10' Floor,
                      '40' Property,
                      '10' Freight_Attribute,
                      Mth.Attribute1 Freight, --最终货运方式 
                      Mth.Attribute2 Common_Carrier, --承运商
                      30 Release_From_Type,
                      10 Order_Type_Id,
                      NULL,
                      NULL
        FROM Cux_Inv_Material_Headers_v Mth, Cux_Inv_Material_Lines_v Mtl
       WHERE Mth.Header_Id = Mtl.Header_Id
         AND Mtl.Header_Id = p_Header_Id
         AND Mtl.Organization_Id = p_Organization_Id
         AND Mth.Inv_Type = 6 --异地调拨
      /*         AND NOT EXISTS (SELECT 'X'
       FROM CUX.CUX_TMS_HEADER_INTERFACE CTHI
      WHERE CTHI.REQUEST_HEADER_ID = MTH.HEADER_ID
        AND CTHI.RELEASE_FROM_TYPE = 30)*/
      ;
    /*UNION ALL
    SELECT DISTINCT MTH.HEADER_ID T_HEADER_ID,
                      MTH.ORGANIZATION_ID,
                      NULL SHIP_TO_ORG_ID,
                      NULL HEADER_ID,
                      MTH.REQUEST_NUMBER,
                      NULL SOLD_TO_ORG_ID,
                      NULL INVOICE_TO_CONTACT_ID,
                      NULL ORG_ID,
                      NULL ORDER_NUMBER,
                      MTL.FROM_SUBINVENTORY_CODE,
                      MTH.LAST_UPDATE_DATE CREATION_DATE,
                      (SELECT HE.FULL_NAME
                         FROM FND_USER FU, HR_EMPLOYEES HE
                        WHERE FU.EMPLOYEE_ID = HE.EMPLOYEE_ID
                          AND FU.USER_ID = MTH.CREATED_BY) FULL_NAME,
                      MTH.DESCRIPTION ORDER_REMARK,
                      NULL FAX1,
                      NULL MOBILE1,
                      NULL TEL1,
                      NULL CONSIGNEE1,
                      NULL CONSIGNEE2,
                      NULL CITY,
                      NULL FINAL_ADDRESS,
                      NULL ZIP_CODE,
                      MTH.ATTRIBUTE3 TRANSFER_UNIT,
                      NULL TRANSFER_ZIP_CODE,
                      NULL TRANSFER_CITY,
                      NULL TRANSFER_ADDRESS,
                      NULL TRANSFER_CONSIGNEE1,
                      NULL TRANSFER_TEL1,
                      NULL TRANSFER_FAX1,
                      NULL TRANSFER_MOBILE1,
                      NULL TRANSFER_CONSIGNEE2,
                      NULL TRANSFER_TEL2,
                      NULL TRANSFER_FAX2,
                      NULL TRANSFER_MOBILE2,
                      NULL FINAL_SHIP,
                      NULL CONSIGNEE,
                      NULL SALESREP_NAME,
                      NULL BUSINESS_STAFF,
                      NULL FLOOR,
                      NULL PROPERTY,
                      NULL FREIGHT_ATTRIBUTE,
                      MTH.ATTRIBUTE1 FREIGHT, --最终货运方式
                      MTH.ATTRIBUTE2 COMMON_CARRIER, --承运商
                      30 RELEASE_FROM_TYPE
        FROM MTL_TXN_REQUEST_HEADERS_V   MTH,
             MTL_TXN_REQUEST_LINES     MTL
       WHERE MTH.HEADER_ID = MTL.HEADER_ID
         AND (MTL.HEADER_ID = P_HEADER_ID OR P_HEADER_ID IS NULL)
         AND MTH.TRANSACTION_TYPE_NAME ='异地调拨单'
         AND NOT EXISTS(SELECT 'X'
                          FROM CUX.CUX_TMS_HEADER_INTERFACE CTHI
                         WHERE CTHI.REQUEST_HEADER_ID=MTH.HEADER_ID);  */
    CURSOR C2(p_Header_Id      IN NUMBER,
              p_Ship_To_Org_Id IN NUMBER,
              p_Type           IN VARCHAR2) IS
      SELECT DISTINCT Oola.Line_Id    Line_Number,
                      Msi.Segment1,
                      Msi.Description,
                      Mtl.Quantity,
                      Mtl.Line_Id,
                      Col.Item_Code,
                      Col.Item_Desc,
                      1001            Product_Line_Gid
        FROM Mtl_Txn_Request_Headers Mth,
             Mtl_Txn_Request_Lines   Mtl,
             Mtl_System_Items_b      Msi,
             Cux_Om_Line_Interface_v Col,
             Oe_Order_Lines_All      Oola
       WHERE Mth.Header_Id = Mtl.Header_Id
         AND Mtl.Txn_Source_Line_Id = Oola.Line_Id
         AND Msi.Organization_Id = Mtl.Organization_Id
         AND Msi.Inventory_Item_Id = Mtl.Inventory_Item_Id
         AND Oola.Line_Id = Col.Order_Line_Id(+)
         AND Mtl.Header_Id = p_Header_Id
         AND Oola.Ship_To_Org_Id = p_Ship_To_Org_Id
         AND p_Type = '40'
         AND Nvl(Msi.Attribute12, 'N') = 'N'
      UNION ALL
      SELECT DISTINCT Oola.Line_Id    Line_Number,
                      Msi.Segment1,
                      Msi.Description,
                      Mtl.Quantity,
                      Mtl.Line_Id,
                      Col.Item_Code,
                      Col.Item_Desc,
                      1003            Product_Line_Gid
        FROM Mtl_Txn_Request_Headers Mth,
             Mtl_Txn_Request_Lines   Mtl,
             Mtl_System_Items_b      Msi,
             Cux_Om_Line_Interface_v Col,
             Oe_Order_Lines_All      Oola
       WHERE Mth.Header_Id = Mtl.Header_Id
         AND Mtl.Txn_Source_Line_Id = Oola.Line_Id
         AND Msi.Organization_Id = Mtl.Organization_Id
         AND Msi.Inventory_Item_Id = Mtl.Inventory_Item_Id
         AND Oola.Line_Id = Col.Order_Line_Id(+)
         AND Mtl.Header_Id = p_Header_Id
         AND p_Type = '40'
         AND Nvl(Msi.Attribute12, 'N') = 'Y'
         AND Oola.Ship_To_Org_Id = p_Ship_To_Org_Id
      /* UNION ALL
      SELECT DISTINCT A.OE_LINE_ID  LINE_NUMBER,
           MSI.SEGMENT1,
           MSI.DESCRIPTION,
           A.QUANTITY,
           A.LINE_ID,
           COL.ITEM_CODE,
           COL.ITEM_DESC,
           1003 PRODUCT_LINE_GID
        FROM CUX_OE_SHIP_LINES_V A,
             CUX_OE_SHIP_HEADERS_V B,
             MTL_TXN_REQUEST_HEADERS MTH,
             CUX_OM_LINE_INTERFACE_V COL,
             MTL_SYSTEM_ITEMS_B MSI
       WHERE A.HEADER_ID=B.HEADER_ID
         AND B.REQUEST_NUMBER =MTH.REQUEST_NUMBER
         AND A.INVENTORY_ITEM_ID=MSI.INVENTORY_ITEM_ID
         AND A.OE_LINE_ID=COL.ORDER_LINE_ID(+)
         AND MTH.HEADER_ID = P_HEADER_ID
         AND NVL(MSI.INVENTORY_ITEM_FLAG,'N')='N'
         AND MSI.ORGANIZATION_ID=P_ORGANIZATION_ID    */
      UNION ALL
      SELECT Rownum               Line_Number,
             Msi.Segment1,
             Msi.Description,
             Mtl.Primary_Quantity Quantity,
             Mtl.Line_Id,
             NULL                 Item_Code,
             NULL                 Item_Desc,
             1001                 Product_Line_Gid
        FROM Cux_Inv_Material_Headers_v Mth,
             Cux_Inv_Material_Lines_v   Mtl,
             Mtl_System_Items_b         Msi
       WHERE Mth.Header_Id = Mtl.Header_Id
         AND Msi.Organization_Id = Mtl.Organization_Id
         AND Msi.Inventory_Item_Id = Mtl.Inventory_Item_Id
         AND p_Type = '30'
         AND Mth.Inv_Type = 6
         AND Mtl.Header_Id = p_Header_Id;
    /*        
    UNION ALL         
    SELECT MTL.LINE_NUMBER,
           MSI.SEGMENT1,
           MSI.DESCRIPTION,
           MTL.QUANTITY,
           MTL.LINE_ID,
           NULL ITEM_CODE,
           NULL ITEM_DESC
      FROM MTL_TXN_REQUEST_HEADERS MTH,
           MTL_TXN_REQUEST_LINES   MTL,
           MTL_SYSTEM_ITEMS_B      MSI,
           CUX_OM_LINE_INTERFACE_V COL,
           OE_ORDER_LINES_ALL      OOLA
     WHERE MTH.HEADER_ID = MTL.HEADER_ID
       AND MSI.ORGANIZATION_ID = MTL.ORGANIZATION_ID
       AND MSI.INVENTORY_ITEM_ID = MTL.INVENTORY_ITEM_ID
       AND MTL.HEADER_ID = P_HEADER_ID;*/
    v_Transfer_Unit_Flag VARCHAR2(10);
    v_Header_Id          NUMBER;
    v_Line_Id            NUMBER;
    v_Src_Location_Gid   VARCHAR2(10);
    v_Customer_Name      VARCHAR2(1000);
    v_Party_Name         VARCHAR2(100);
    i                    NUMBER;
    j                    NUMBER;
    v_Seq                NUMBER;
    v_Organization_Name  VARCHAR2(100);
  
    Doc       Xmldom.Domdocument;
    Et        Xmldom.Domelement;
    Et1       Xmldom.Domelement;
    Et2       Xmldom.Domelement;
    Tempnode  Xmldom.Domnode;
    Tempnode1 Xmldom.Domnode;
    Tempnode4 Xmldom.Domnode;
    Tempnode5 Xmldom.Domnode;
    Tempnode2 Xmldom.Domtext;
    Tempnode3 Xmldom.Domnode;
    /*    STR                 VARCHAR2(30000);*/
    Str        CLOB;
    v_1        CLOB;
    l_Xml_Char CLOB;
    /*    L_XML_CHAR            VARCHAR2(32764);*/
    Req                   Utl_Http.Req;
    Resp                  Utl_Http.Resp;
    l_Replyline           VARCHAR2(1000);
    l_Rep_Str             VARCHAR2(32764);
    l_Retvalue            VARCHAR2(100);
    l_Message             VARCHAR2(100);
    v_Concurrent_Status   BOOLEAN;
    v_Org_Name            VARCHAR2(100);
    v_Party_Id            NUMBER;
    v_Src_Location_Gid2   VARCHAR2(250);
    v_Src_Location_Gid3   VARCHAR2(250);
    v_Sources_Of_Products VARCHAR2(100);
    v_Count               NUMBER;
    l_Clob_Document       CLOB;
    l_Document            VARCHAR2(32764);
    --------
    v_Length       NUMBER;
    Buffer         VARCHAR2(30000);
    v_Buffer_Size  BINARY_INTEGER := 10000;
    v_Offset       INTEGER := 1;
    v_Buffer       VARCHAR2(30000);
    v_Contract     VARCHAR2(250);
    v_Project_Name VARCHAR2(250);
    v_Order_Number VARCHAR2(250);
    v_Lengthb      NUMBER;
    v_Retcode      VARCHAR2(2500);
    v_Errbuf       VARCHAR2(2500);
  
    -- 数据校验结果数组表
    v_Arr_List    Array_Table;
    v_Arr_Cnt     NUMBER;
    v_Request_Url VARCHAR2(400);
  
    l_sub_comments varchar2(1000); --added by bruce on 20150925
  
  BEGIN
    i := 1;
    SELECT Cux_Om_Header_Interface_s.Nextval INTO v_Seq FROM Dual;
    FOR x IN C1 LOOP
      IF x.Release_From_Type = '30' THEN
        IF Nvl(x.Transfer_Unit, 'N') = 'Y' THEN
          v_Transfer_Unit_Flag := 1;
        ELSIF Nvl(x.Transfer_Unit, 'N') = 'N' THEN
          v_Transfer_Unit_Flag := 0;
        END IF;
      ELSE
        IF x.Transfer_Unit IS NOT NULL THEN
          v_Transfer_Unit_Flag := 1;
        ELSE
          v_Transfer_Unit_Flag := 0;
        END IF;
      END IF;
      SELECT Cux_Om_Header_Interface_s.Nextval INTO v_Header_Id FROM Dual;
      IF x.From_Subinventory_Code IS NULL THEN
        SELECT MAX(Mmt.Subinventory_Code)
          INTO x.From_Subinventory_Code
          FROM Mtl_Material_Transactions_Temp Mmt
         WHERE Mmt.Move_Order_Header_Id = x.Header_Id;
      END IF;
    
      BEGIN
        SELECT Substr(Msi.Attribute1, 1, 6)
          INTO v_Src_Location_Gid
          FROM Mtl_Secondary_Inventories Msi
         WHERE Msi.Organization_Id = x.Organization_Id
           AND Msi.Secondary_Inventory_Name = x.From_Subinventory_Code;
      EXCEPTION
        WHEN OTHERS THEN
          v_Src_Location_Gid := NULL;
      END;
      IF x.Release_From_Type = '30' THEN
        BEGIN
          SELECT Substr(Msi.Attribute1, 1, 6)
            INTO v_Src_Location_Gid2
            FROM Mtl_Secondary_Inventories Msi
           WHERE Msi.Organization_Id = x.Organization_Id
             AND Msi.Secondary_Inventory_Name = x.Final_Ship;
        EXCEPTION
          WHEN OTHERS THEN
            v_Src_Location_Gid2 := NULL;
        END;
      ELSE
        v_Src_Location_Gid2 := x.Final_Ship;
      END IF;
      ----       
      v_Src_Location_Gid3 := x.Transfer_Unit;
    
      BEGIN
        SELECT a.Customer_Name
          INTO v_Customer_Name
          FROM Cux_Customers_v a
         WHERE a.Cust_Account_Id = x.Sold_To_Org_Id;
      EXCEPTION
        WHEN OTHERS THEN
          v_Customer_Name := NULL;
      END;
    
      BEGIN
        SELECT Hpsub.Party_Name
          INTO v_Party_Name
          FROM Hz_Cust_Account_Roles Hcar,
               Hz_Relationships      Hr,
               Hz_Parties            Hpsub
         WHERE Hcar.Party_Id = Hr.Party_Id
           AND Hr.Subject_Id = Hpsub.Party_Id
           AND Hcar.Role_Type = 'CONTACT'
           AND Hr.Directional_Flag = 'F'
           AND Hcar.Cust_Account_Role_Id = x.Invoice_To_Contact_Id;
      EXCEPTION
        WHEN OTHERS THEN
          v_Party_Name := NULL;
      END;
      IF x.Release_From_Type <> '30' THEN
        BEGIN
          SELECT Hou.Name
            INTO v_Org_Name
            FROM Hr_Operating_Units Hou
           WHERE Hou.Organization_Id = x.Org_Id;
        EXCEPTION
          WHEN OTHERS THEN
            v_Org_Name := NULL;
        END;
      ELSE
        v_Org_Name := 'Y';
      END IF;
    
      IF x.Final_Address IS NULL THEN
        BEGIN
          SELECT Hz_Format_Pub.Format_Address(Arhzpartysiteseo.Location_Id,
                                              NULL,
                                              NULL,
                                              ' ') Address
            INTO x.Final_Address
            FROM Hz_Party_Sites Arhzpartysiteseo,
                 Hz_Locations   Arhzlocationseo
           WHERE Arhzpartysiteseo.Party_Site_Id IN
                 (SELECT b.Party_Site_Id
                    FROM Hz_Cust_Site_Uses_All a, Hz_Cust_Acct_Sites_All b
                   WHERE a.Cust_Acct_Site_Id = b.Cust_Acct_Site_Id
                     AND a.Site_Use_Id = x.Ship_To_Org_Id)
             AND Arhzpartysiteseo.Location_Id = Arhzlocationseo.Location_Id;
        EXCEPTION
          WHEN OTHERS THEN
            NULL;
        END;
      
      END IF;
    
      IF x.Transfer_Consignee1 IS NULL AND v_Party_Name IS NOT NULL THEN
        SELECT Hcar.Party_Id
          INTO v_Party_Id
          FROM Hz_Cust_Account_Roles Hcar,
               Hz_Relationships      Hr,
               Hz_Parties            Hpsub
         WHERE Hcar.Party_Id = Hr.Party_Id
           AND Hr.Subject_Id = Hpsub.Party_Id
           AND Hcar.Role_Type = 'CONTACT'
           AND Hr.Directional_Flag = 'F'
           AND Hcar.Cust_Account_Role_Id = x.Invoice_To_Contact_Id;
        BEGIN
          SELECT a.Phone_Number
            INTO x.Transfer_Tel1
            FROM Hz_Contact_Points a
           WHERE a.Owner_Table_Id = v_Party_Id
             AND a.Phone_Line_Type = 'PHONE';
        EXCEPTION
          WHEN OTHERS THEN
            NULL;
        END;
        BEGIN
          SELECT a.Phone_Number
            INTO x.Transfer_Fax1
            FROM Hz_Contact_Points a
           WHERE a.Owner_Table_Id = v_Party_Id
             AND a.Phone_Line_Type = 'FAX';
        EXCEPTION
          WHEN OTHERS THEN
            NULL;
        END;
      
        BEGIN
          SELECT a.Phone_Number
            INTO x.Transfer_Mobile1
            FROM Hz_Contact_Points a
           WHERE a.Owner_Table_Id = v_Party_Id
             AND a.Phone_Line_Type = 'MOBILE';
        EXCEPTION
          WHEN OTHERS THEN
            NULL;
        END;
      
      END IF;
      /*      BEGIN
      SELECT DISTINCT A.SOURCES_OF_PRODUCTS
        INTO V_SOURCES_OF_PRODUCTS
        FROM CUX_OM_LINE_INTERFACE A
       WHERE A.SET_ID =X.SHIP_SET_ID;
      EXCEPTION
         WHEN OTHERS THEN
           V_SOURCES_OF_PRODUCTS:=NULL;
      END;*/
      IF x.l_Shipping_Instructions IS NOT NULL THEN
        /*   X.REQUEST_NUMBER:=X.REQUEST_NUMBER||'-'||V_SOURCES_OF_PRODUCTS;*/
        SELECT x.Request_Number || '-' ||
               Decode(x.Org_Id, 81, 'G', 82, 'H') || '-' ||
               Decode(x.l_Shipping_Instructions, 'FW', 'G', 'JW', 'H')
          INTO x.Request_Number
          FROM Dual;
      END IF;
      --判断
      IF x.Release_From_Type = '40' THEN
        BEGIN
          SELECT a.Remark
            INTO x.Order_Remark
            FROM Cux_Oe_Ship_Headers_v a
           WHERE a.Request_Number =
                 (SELECT b.Request_Number
                    FROM Mtl_Txn_Request_Headers b
                   WHERE b.Header_Id = p_Header_Id);
        EXCEPTION
          WHEN OTHERS THEN
            NULL;
        END;
      
        --START OF ADD by bruce ON 20150925
        BEGIN
          SELECT CPD.COMMENTS
            INTO l_sub_comments
            FROM CUX_PICK_SUB_DOC CPD
           WHERE CPD.REQ_HEADER_ID = p_Header_Id
             and rownum = 1;
        EXCEPTION
          WHEN OTHERS THEN
            l_sub_comments := '';
        END;
        x.Order_Remark := x.Order_Remark || ' ' || l_sub_comments;
        --END OF ADD by bruce ON 20150925
      
        BEGIN
          SELECT a.Contract, a.Project_Name
            INTO v_Contract, v_Project_Name
            FROM Cux_Om_Header_Interface a
           WHERE a.Order_Header_Id = x.Header_Id;
        EXCEPTION
          WHEN OTHERS THEN
            v_Contract     := NULL;
            v_Project_Name := NULL;
        END;
        IF v_Contract IS NOT NULL THEN
          v_Order_Number := Substrb(x.Order_Number || '/' || v_Contract,
                                    1,
                                    150);
        END IF;
        IF v_Project_Name IS NOT NULL THEN
        
          v_Customer_Name := v_Customer_Name || '/' || v_Project_Name;
        
          v_Customer_Name := Substrb(v_Customer_Name, 1, 120);
        END IF;
      ELSE
        v_Order_Number := x.Order_Number;
      END IF;
    
      /*  IF X.RELEASE_FROM_TYPE =40 THEN
       BEGIN 
        SELECT DISTINCT A.FINAL_SHIP,A.FINAL_ADDRESS
          INTO X.FINAL_SHIP,X.FINAL_ADDRESS
          FROM CUX_OE_SHIP_LINES_V A,
               CUX_OE_SHIP_HEADERS_V B
         WHERE  A.HEADER_ID=B.HEADER_ID
           AND  B.REQUEST_NUMBER =X.REQUEST_NUMBER;
         EXCEPTION
            WHEN OTHERS THEN
               NULL;
          END;       
      
      END IF;*/
    
      INSERT INTO Cux.Cux_Tms_Header_Interface
        (Seq,
         Factory,
         Order_Release_Gid,
         Order_Release_Id,
         Release_From_Type,
         Erp_Delivery_No,
         Late_Pickup_Date,
         Late_Delivery_Date,
         Transport_Mode_Gid,
         Service_Provider_Gid,
         Cust_Id,
         Is_Transfer,
         Domain_Name,
         Created_By_Name,
         Remark,
         Src_Location_Gid,
         Src_Location_Name,
         Src_Postal_Code,
         Src_Region_Gid,
         Src_Address,
         Src_City_Gid,
         Src_Province,
         Mid_Location_Gid, ------------------------------------------------------
         Mid_Location_Name,
         Mid_Postal_Code,
         Mid_City_Gid,
         Mid_Province,
         Mid_Address,
         Mid_Remark,
         Mid_Contact_Gid,
         Mid_Contact_Name,
         Mid_Tel,
         Mid_Phone,
         Mid_Fax,
         Mid_Email,
         Mid_Contact_Gid1,
         Mid_Contact_Name1,
         Mid_Tel1,
         Mid_Phone1,
         Mid_Fax1,
         Mid_Email1, -----------------------------------------------------------
         Dest_Location_Gid,
         Dest_Location_Name,
         Dest_Postal_Code,
         Dest_Region_Gid,
         Dest_Address,
         Dest_City_Gid,
         Dest_Province,
         Dest_Remark,
         Dest_Contact_Gid,
         Dest_Contact_Name,
         Dest_Phone,
         Dest_Tel,
         Dest_Fax,
         Dest_Email,
         Dest_Contact_Name1,
         Dest_Phone1,
         Dest_Tel1,
         Dest_Fax1,
         Dest_Email1,
         Port_Of_Discharge_Location_Gid,
         Udf1,
         Udf2,
         Udf3,
         Udf4,
         Udf5,
         Udf6,
         Udf7,
         Udf8,
         Udf9,
         Udf10,
         Udf11,
         Udf12,
         Udf13,
         Udf14,
         Udf15,
         Contract_Cust,
         Receive_Letter_By,
         Header_Id,
         Last_Update_Date,
         Last_Updated_By,
         Creation_Date,
         Created_By,
         Last_Update_Login,
         Request_Header_Id)
      VALUES
        (v_Seq,
         x.Org_Id,
         x.Request_Number,
         x.Request_Number,
         x.Release_From_Type,
         v_Order_Number,
         x.Creation_Date,
         x.Creation_Date + 4,
         x.Freight,
         x.Common_Carrier,
         Decode(x.Org_Id, 81, 20, 96),
         v_Transfer_Unit_Flag,
         'STARNET',
         x.Full_Name,
         Nvl(x.Order_Remark, x.Shipping_Instructions),
         v_Src_Location_Gid, --子库存说明弹性字段“仓库物理代码”
         NULL,
         NULL,
         NULL,
         NULL,
         NULL,
         NULL,
         NULL,
         v_Src_Location_Gid3, -----------------------------中转
         x.Transfer_Zip_Code,
         x.Transfer_City,
         NULL,
         x.Transfer_Address,
         NULL,
         x.Transfer_Consignee1,
         x.Transfer_Consignee1,
         x.Transfer_Tel1,
         x.Transfer_Mobile1,
         x.Transfer_Fax1,
         NULL,
         x.Transfer_Consignee2,
         x.Transfer_Consignee2,
         x.Transfer_Tel2,
         x.Transfer_Mobile2,
         x.Transfer_Fax2,
         NULL, ---------------------------------------
         v_Src_Location_Gid2, --X.FINAL_SHIP,   ---------------DEST_LOCATION_GID
         v_Src_Location_Gid2, --X.FINAL_SHIP,
         x.Zip_Code,
         NULL,
         Nvl(x.Final_Address, v_Src_Location_Gid),
         x.City,
         NULL,
         NULL,
         x.Consignee1,
         x.Consignee1,
         x.Mobile1,
         x.Tel1,
         x.Fax1,
         NULL,
         x.Consignee2,
         x.Mobile2,
         x.Tel2,
         x.Fax2,
         NULL,
         NULL,
         NULL, --UDF1
         x.Freight_Attribute, --货运属性
         'N',
         Nvl(x.Order_Number, 139),
         x.Property, --自提属性
         v_Org_Name, --X.ORG_ID, --业务实体对应法人
         x.Order_Type_Id, --订单类型
         NULL, --
         'N', --锁库属性
         x.Floor, --搬楼层信息
         x.Business_Staff, --业务员工号
         x.Salesrep_Name, --业务员姓名
         NULL,
         NULL,
         x.From_Subinventory_Code, --销售订单发放时的来源子库
         v_Customer_Name, --客户名称
         v_Party_Name, --收单联系人
         v_Header_Id,
         SYSDATE,
         Fnd_Global.User_Id,
         SYSDATE,
         Fnd_Global.User_Id,
         Fnd_Global.Login_Id,
         x.t_Header_Id);
      j := 1;
      UPDATE Mtl_Txn_Request_Headers a
         SET a.Attribute15 = x.Request_Number
       WHERE a.Header_Id = p_Header_Id;
    
      FOR y IN C2(x.t_Header_Id, x.Ship_To_Org_Id, x.Release_From_Type) LOOP
        INSERT INTO Cux.Cux_Tms_Line_Interface
          (Order_Release_Gid,
           Line_Id,
           Item_Gid,
           Item_Name,
           Sales_Item_Id,
           Sales_Item_Name,
           Barcode,
           Package_Count,
           Udf1,
           Domain_Name,
           Product_Line_Gid,
           Udf2,
           Header_Id,
           Lines_Id,
           Last_Update_Date,
           Last_Updated_By,
           Creation_Date,
           Created_By,
           Last_Update_Login,
           Request_Line_Id)
        VALUES
          (x.Request_Number,
           y.Line_Id,
           y.Segment1,
           y.Description,
           y.Item_Code,
           y.Item_Desc,
           NULL,
           y.Quantity,
           NULL,
           'STARNET',
           Decode(x.Organization_Id, 84, 'G1', 'H1'),
           y.Product_Line_Gid, --明细类型/传物品属性
           v_Header_Id,
           v_Line_Id,
           SYSDATE,
           Fnd_Global.User_Id,
           SYSDATE,
           Fnd_Global.User_Id,
           Fnd_Global.Login_Id,
           y.Line_Id);
        /*        J := J + 1;*/
      END LOOP;
      /*      I := I + 1;*/
    END LOOP;
    Dbms_Lob.Createtemporary(Lob_Loc => l_Clob_Document,
                             Cache   => TRUE,
                             Dur     => Dbms_Lob.Session);
    Dbms_Lob.Open(Lob_Loc   => l_Clob_Document,
                  Open_Mode => Dbms_Lob.Lob_Readwrite);
  
    FOR z IN (SELECT *
                FROM Cux.Cux_Tms_Header_Interface a
               WHERE a.Seq = v_Seq) LOOP
      l_Document := '<STD_IN>';
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<Factory>G</Factory>';
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<ObjectID>ORDER_RELEASE</ObjectID>';
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<Service>';
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<Data>';
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      -- 101  发货订单号,必填,STARNET.+发货订单号    
      l_Document := '<ORDER_RELEASE_GID>' || 'STARNET.' ||
                    z.Order_Release_Gid || '</ORDER_RELEASE_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List,
                      z.Order_Release_Gid,
                      101,
                      '发货订单号',
                      1);
    
      -- 50  发货订单号,必填                                  
      l_Document := '<ORDER_RELEASE_ID>' || z.Order_Release_Id ||
                    '</ORDER_RELEASE_ID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Order_Release_Id, 50, '发货订单号', 1);
    
      -- 20 发运请求来源类型,40 发货通知单,30 异地调拨单  
      l_Document := '<RELEASE_FROM_TYPE>' || z.Release_From_Type ||
                    '</RELEASE_FROM_TYPE>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List,
                      z.Release_From_Type,
                      20,
                      '发运请求来源类型');
    
      -- 896 销售订单号,必填 
      l_Document := '<ERP_DELIVERY_NO>' || z.Erp_Delivery_No ||
                    '</ERP_DELIVERY_NO>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Erp_Delivery_No, 896, '销售订单号', 1);
    
      -- datetime 要求提货日期,通知日期,必填
      l_Document := '<LATE_PICKUP_DATE>' ||
                    To_Char(z.Late_Pickup_Date, 'YYYY-MM-DD HH24:MI:SS') ||
                    '</LATE_PICKUP_DATE>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      -- datetime 要求到货日期，期望到货日期,必填
      l_Document := '<LATE_DELIVERY_DATE>' ||
                    To_Char(z.Late_Delivery_Date, 'YYYY-MM-DD HH24:MI:SS') ||
                    '</LATE_DELIVERY_DATE>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      -- 101 运输方式，ERP有就传,没有传空
      l_Document := '<TRANSPORT_MODE_GID>' || z.Transport_Mode_Gid ||
                    '</TRANSPORT_MODE_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Transport_Mode_Gid, 101, '运输方式');
    
      -- 101 运输供应商,承运商传名称，ERP有就传,没有不传
      l_Document := '<SERVICE_PROVIDER_GID>' || z.Service_Provider_Gid ||
                    '</SERVICE_PROVIDER_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List,
                      z.Service_Provider_Gid,
                      101,
                      '运输供应商');
    
      -- 101 客户ID,必填,福建网络公司传20,北京京网传96
      l_Document := '<CUST_ID>' || z.Cust_Id || '</CUST_ID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Cust_Id, 101, '客户ID');
    
      -- 10 是否有中转站，1是有转运,0 或者空是无转运
      l_Document := '<IS_TRANSFER>' || z.Is_Transfer || '</IS_TRANSFER>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Is_Transfer, 10, '是否有中转站');
    
      -- 50 域,STARNET
      l_Document := '<DOMAIN_NAME>' || z.Domain_Name || '</DOMAIN_NAME>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Domain_Name, 50, '域');
    
      -- 128 创建人，录入人姓名
      l_Document := '<CREATED_BY>' || z.Created_By_Name || '</CREATED_BY>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Created_By_Name, 128, '创建人');
    
      -- 1000 发货订单备注
      l_Document := '<REMARK>' || z.Remark || '</REMARK>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Remark, 1000, '发货订单备注');
    
      -- 始发地信息
    
      -- 101 物理仓库代码，易拓仓库地址前六位  
      l_Document := '<SRC_LOCATION_GID>' || z.Src_Location_Gid ||
                    '</SRC_LOCATION_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Src_Location_Gid, 101, '物理仓库代码');
    
      -- 128 仓库名称，传空  
      l_Document := '<SRC_LOCATION_NAME>' || z.Src_Location_Name ||
                    '</SRC_LOCATION_NAME>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Src_Location_Name, 128, '仓库名称');
    
      -- 50 仓库邮编，传空
      l_Document := '<SRC_POSTAL_CODE>' || z.Src_Postal_Code ||
                    '</SRC_POSTAL_CODE>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Src_Postal_Code, 50, '仓库邮编');
    
      -- 101 仓库区域，传空
      l_Document := '<SRC_REGION_GID>' || z.Src_Region_Gid ||
                    '</SRC_REGION_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Src_Region_Gid, 101, '仓库区域');
    
      -- 512 仓库地址，传空
      l_Document := '<SRC_ADDRESS>' || z.Src_Address || '</SRC_ADDRESS>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Src_Address, 512, '仓库地址');
    
      -- 101 仓库城市，传空
      l_Document := '<SRC_CITY_GID>' || z.Src_City_Gid || '</SRC_CITY_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Src_City_Gid, 101, '仓库城市');
    
      -- 101 仓库省份，传空
      l_Document := '<SRC_PROVINCE>' || z.Src_Province || '</SRC_PROVINCE>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Src_Province, 101, '仓库省份');
    
      -- 中转站信息,没有传空值,有按实际传
      -- 101 中转站代码
      l_Document := '<MID_LOCATION_GID>' || z.Mid_Location_Gid ||
                    '</MID_LOCATION_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Location_Gid, 101, '中转站代码');
    
      -- 128 中转站名称
      l_Document := '<MID_LOCATION_NAME>' || z.Mid_Location_Name ||
                    '</MID_LOCATION_NAME>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Location_Name, 128, '中转站名称');
    
      -- 50 中转站邮编
      l_Document := '<MID_POSTAL_CODE>' || z.Mid_Postal_Code ||
                    '</MID_POSTAL_CODE>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Postal_Code, 50, '中转站邮编');
    
      -- 101 中转站城市
      l_Document := '<MID_CITY_GID>' || z.Mid_City_Gid || '</MID_CITY_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_City_Gid, 101, '中转站城市');
    
      -- 101 中转站省份
      l_Document := '<MID_PROVINCE>' || z.Mid_Province || '</MID_PROVINCE>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Province, 101, '中转站省份');
    
      -- 512 中转站地址
      l_Document := '<MID_ADDRESS>' || z.Mid_Address || '</MID_ADDRESS>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Address, 512, '中转站地址');
    
      -- 512 备注
      l_Document := '<MID_REMARK>' || z.Mid_Remark || '</MID_REMARK>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Remark, 512, '备注');
    
      -- 101 中转站联系人
      l_Document := '<MID_CONTACT_GID>' || z.Mid_Contact_Gid ||
                    '</MID_CONTACT_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Contact_Gid, 101, '中转站联系人');
    
      -- 128 中转站联系人姓名
      l_Document := '<MID_CONTACT_NAME>' || z.Mid_Contact_Name ||
                    '</MID_CONTACT_NAME>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List,
                      z.Mid_Contact_Name,
                      128,
                      '中转站联系人姓名');
    
      -- 100 中转站联系电话
      l_Document := '<MID_TEL>' || z.Mid_Tel || '</MID_TEL>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Tel, 100, '中转站联系电话');
    
      -- 100 中转站联系手机
      l_Document := '<MID_PHONE>' || z.Mid_Phone || '</MID_PHONE>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Phone, 100, '中转站联系手机');
    
      -- 50 中转站传真
      l_Document := '<MID_FAX>' || z.Mid_Fax || '</MID_FAX>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Fax, 50, '中转站传真');
    
      -- 50 中转站联系人电子邮件
      l_Document := '<MID_EMAIL>' || z.Mid_Email || '</MID_EMAIL>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Email, 50, '中转站联系人电子邮件');
    
      -- 101 中转站联系人1
      l_Document := '<MID_CONTACT_GID1>' || z.Mid_Contact_Gid1 ||
                    '</MID_CONTACT_GID1>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Contact_Gid1, 101, '中转站联系人1');
    
      -- 128 中转站联系人姓名1
      l_Document := '<MID_CONTACT_NAME1>' || z.Mid_Contact_Name1 ||
                    '</MID_CONTACT_NAME1>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List,
                      z.Mid_Contact_Name1,
                      128,
                      '中转站联系人姓名1');
    
      -- 100 中转站联系人手机1
      l_Document := '<MID_PHONE1>' || z.Mid_Phone1 || '</MID_PHONE1>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Phone1, 100, '中转站联系人手机1');
    
      -- 100 中转站联系人电话1
      l_Document := '<MID_TEL1>' || z.Mid_Tel1 || '</MID_TEL1>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Tel1, 100, '中转站联系人电话1');
    
      -- 50 中转站传真1
      l_Document := '<MID_FAX1>' || z.Mid_Fax1 || '</MID_FAX1>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Mid_Fax1, 50, '中转站传真1');
    
      -- 50 中转站联系人电子邮件1
      l_Document := '<MID_EMAIL1>' || z.Mid_Email1 || '</MID_EMAIL1>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List,
                      z.Mid_Email1,
                      50,
                      '中转站联系人电子邮件1');
    
      -- 目的地信息
      -- 101 收货地址代码
      l_Document := '<DEST_LOCATION_GID>' || z.Dest_Location_Gid ||
                    '</DEST_LOCATION_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Location_Gid, 101, '收货地址代码');
    
      -- 128 收货地地址名称
      l_Document := '<DEST_LOCATION_NAME>' || z.Dest_Location_Name ||
                    '</DEST_LOCATION_NAME>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List,
                      z.Dest_Location_Name,
                      128,
                      '收货地地址名称');
    
      -- 50 收货地址邮编
      l_Document := '<DEST_POSTAL_CODE>' || z.Dest_Postal_Code ||
                    '</DEST_POSTAL_CODE>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Postal_Code, 50, '收货地址邮编');
    
      -- 101 收货地区域
      l_Document := '<DEST_REGION_GID>' || z.Dest_Region_Gid ||
                    '</DEST_REGION_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Region_Gid, 101, '收货地区域');
    
      -- 512 收货地址
      l_Document := '<DEST_ADDRESS>' || z.Dest_Address || '</DEST_ADDRESS>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Address, 512, '收货地址');
    
      -- 101 收货城市
      l_Document := '<DEST_CITY_GID>' || z.Dest_City_Gid ||
                    '</DEST_CITY_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_City_Gid, 101, '收货城市');
    
      -- 101 收货省份
      l_Document := '<DEST_PROVINCE>' || z.Dest_Province ||
                    '</DEST_PROVINCE>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Province, 101, '收货省份');
    
      -- 512 收货地址备注
      l_Document := '<DEST_REMARK>' || z.Dest_Remark || '</DEST_REMARK>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Remark, 512, '收货地址备注');
    
      -- 101 收货地址联系人
      l_Document := '<DEST_CONTACT_GID>' || z.Dest_Contact_Gid ||
                    '</DEST_CONTACT_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List,
                      z.Dest_Contact_Gid,
                      101,
                      '收货地址联系人');
    
      -- 128 收货地址联系人姓名
      l_Document := '<DEST_CONTACT_NAME>' || z.Dest_Contact_Name ||
                    '</DEST_CONTACT_NAME>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List,
                      z.Dest_Contact_Name,
                      128,
                      '收货地址联系人姓名');
    
      -- 100 收货联系人手机
      l_Document := '<DEST_PHONE>' || z.Dest_Phone || '</DEST_PHONE>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Phone, 100, '收货联系人手机');
    
      -- 100 收货联系电话
      l_Document := '<DEST_TEL>' || z.Dest_Tel || '</DEST_TEL>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Tel, 100, '收货联系电话');
    
      -- 50 收货地传真
      l_Document := '<DEST_FAX>' || z.Dest_Fax || '</DEST_FAX>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Fax, 50, '收货地传真');
    
      -- 50 收货地电子邮件
      l_Document := '<DEST_EMAIL>' || z.Dest_Email || '</DEST_EMAIL>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Email, 50, '收货地电子邮件');
    
      -- 128 收货地址联系人姓名1
      l_Document := '<DEST_CONTACT_NAME1>' || z.Dest_Contact_Name1 ||
                    '</DEST_CONTACT_NAME1>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List,
                      z.Dest_Contact_Name1,
                      128,
                      '收货地址联系人姓名1');
    
      -- 100 收货联系人手机1
      l_Document := '<DEST_PHONE1>' || z.Dest_Phone1 || '</DEST_PHONE1>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Phone1, 100, '收货联系人手机1');
    
      -- 100 收货联系人电话1
      l_Document := '<DEST_TEL1>' || z.Dest_Tel1 || '</DEST_TEL1>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Tel1, 100, '收货联系人电话1');
    
      -- 50 收货地传真1
      l_Document := '<DEST_FAX1>' || z.Dest_Fax1 || '</DEST_FAX1>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Fax1, 50, '收货地传真1');
    
      -- 50 收货地电子邮件1
      l_Document := '<DEST_EMAIL1>' || z.Dest_Email1 || '</DEST_EMAIL1>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Dest_Email1, 50, '收货地电子邮件1');
    
      -- 101 到站名
      l_Document := '<PORT_OF_DISCHARGE_LOCATION_GID>' ||
                    z.Port_Of_Discharge_Location_Gid ||
                    '</PORT_OF_DISCHARGE_LOCATION_GID>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List,
                      z.Port_Of_Discharge_Location_Gid,
                      101,
                      '到站名');
    
      -- 128 UDF1尚未使用
      l_Document := '<UDF1>' || z.Udf1 || '</UDF1>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf1, 128, 'UDF1');
    
      -- 128 货运属性 必传
      l_Document := '<UDF2>' || z.Udf2 || '</UDF2>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf2, 128, '货运属性', 1);
    
      -- 128 齐套属性,传N
      l_Document := '<UDF3>' || z.Udf3 || '</UDF3>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf3, 128, '齐套属性');
    
      -- 128 合同号，订单合同号
      l_Document := '<UDF4>' || z.Udf4 || '</UDF4>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf4, 128, '合同号');
    
      -- 128 自提属性，必传
      l_Document := '<UDF5>' || z.Udf5 || '</UDF5>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf5, 128, '自提属性');
    
      -- 128 合同客户
      l_Document := '<UDF6>' || z.Udf6 || '</UDF6>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf6, 128, '合同客户');
    
      -- 128 订单类型
      l_Document := '<UDF7>' || z.Udf7 || '</UDF7>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf7, 128, '订单类型');
    
      -- 128 UDF8尚未使用
      l_Document := '<UDF8>' || z.Udf8 || '</UDF8>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf8, 128, 'UDF8');
    
      -- 128 锁库属性，N 
      l_Document := '<UDF9>' || z.Udf9 || '</UDF9>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf9, 128, '锁库属性');
    
      --  128 搬楼层信息,必填项
      l_Document := '<UDF10>' || z.Udf10 || '</UDF10>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf10, 128, '搬楼层信息');
    
      -- 128 业务员代码
      l_Document := '<UDF11>' || z.Udf11 || '</UDF11>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf11, 128, '业务员代码');
    
      -- 128 业务员姓名
      l_Document := '<UDF12>' || z.Udf12 || '</UDF12>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf12, 128, '业务员姓名');
    
      -- 128 业务员手机/电话/邮箱
      l_Document := '<UDF13>' || z.Udf13 || '</UDF13>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf13, 128, '业务员手机/电话/邮箱');
    
      -- 128 软件订单号
      l_Document := '<UDF14>' || z.Udf14 || '</UDF14>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf14, 128, '软件订单号');
    
      -- 128 ERP逻辑仓库号
      l_Document := '<UDF15>' || z.Udf15 || '</UDF15>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Udf15, 128, 'ERP逻辑仓库号');
    
      -- 100 合同客户,ERP合同客户
      l_Document := '<CONTRACT_CUST>' || z.Contract_Cust ||
                    '</CONTRACT_CUST>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List, z.Contract_Cust, 100, '合同客户');
    
      -- 100 收函人,ERP开票收函人
      l_Document := '<RECEIVE_LETTER_BY>' || z.Receive_Letter_By ||
                    '</RECEIVE_LETTER_BY>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      Data_Validation(v_Arr_List,
                      z.Receive_Letter_By,
                      100,
                      '收函人,ERP开票收函人');
    
      -- OrderRelease明细信息
      l_Document := '<ORDER_RELEASE_LINES>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      FOR w IN (SELECT *
                  FROM Cux.Cux_Tms_Line_Interface b
                 WHERE b.Header_Id = z.Header_Id) LOOP
      
        l_Document := '<ORDER_RELEASE_LINE>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        -- 101 发货订单号,必传
        l_Document := '<ORDER_RELEASE_GID>' || w.Order_Release_Gid ||
                      '</ORDER_RELEASE_GID>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        Data_Validation(v_Arr_List,
                        w.Order_Release_Gid,
                        101,
                        '发货订单号',
                        1);
      
        -- 整数,明细行号,必传
        l_Document := '<LINE_ID>' || w.Line_Id || '</LINE_ID>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        Data_Validation(v_Arr_List, w.Line_Id, 100, '明细行号', 1);
      
        -- 101 货品代码,必传
        l_Document := '<ITEM_GID>' || w.Item_Gid || '</ITEM_GID>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        Data_Validation(v_Arr_List, w.Item_Gid, 101, '货品代码');
      
        -- 500 货品名称,必传
        l_Document := '<ITEM_NAME>' || w.Item_Name || '</ITEM_NAME>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        Data_Validation(v_Arr_List, w.Item_Name, 500, '货品名称', 1);
      
        -- 20 销售产品代码
        l_Document := '<SALES_ITEM_ID>' || w.Sales_Item_Id ||
                      '</SALES_ITEM_ID>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        Data_Validation(v_Arr_List, w.Sales_Item_Id, 20, '销售产品代码');
      
        -- 500 销售产品名称
        l_Document := '<SALES_ITEM_NAME>' || w.Sales_Item_Name ||
                      '</SALES_ITEM_NAME>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        Data_Validation(v_Arr_List, w.Sales_Item_Name, 500, '销售产品名称');
      
        -- 20 条码产品型号,传空
        l_Document := '<BARCODE>' || w.Barcode || '</BARCODE>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        Data_Validation(v_Arr_List, w.Barcode, 20, '条码产品型号');
      
        -- 条码产品型号
        --L_DOCUMENT := '<BARCODE>' || W.BARCODE || '</BARCODE>';
        --WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        -- 数字, Item数量,必传
        l_Document := '<PACKAGE_COUNT>' || w.Package_Count ||
                      '</PACKAGE_COUNT>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        Data_Validation(v_Arr_List, w.Package_Count, 50, 'Item数量', 1);
      
        -- 128 父级ITEM,传空
        l_Document := '<UDF1>' || w.Udf1 || '</UDF1>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        Data_Validation(v_Arr_List, w.Udf1, 128, '父级ITEM');
      
        -- 50 域,传STARNET
        l_Document := '<DOMAIN_NAME>' || w.Domain_Name || '</DOMAIN_NAME>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        Data_Validation(v_Arr_List, w.Domain_Name, 50, '域');
      
        -- 128 产品线，传产品线代码,必传
        l_Document := '<PRODUCT_LINE_GID>' || w.Product_Line_Gid ||
                      '</PRODUCT_LINE_GID>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        Data_Validation(v_Arr_List, w.Product_Line_Gid, 128, '产品线');
      
        -- 128 明细类型,传物品属性,如果是软件,传1003
        l_Document := '<UDF2>' || w.Udf2 || '</UDF2>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        Data_Validation(v_Arr_List, w.Udf2, 128, '明细类型');
      
        l_Document := '</ORDER_RELEASE_LINE>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
      END LOOP;
    
      l_Document := '</ORDER_RELEASE_LINES>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '</Data>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '</Service>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '</STD_IN>';
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      Dbms_Lob.Createtemporary(Lob_Loc => Str,
                               Cache   => TRUE,
                               Dur     => Dbms_Lob.Session);
      Dbms_Lob.Append(Str, l_Clob_Document);
    
      -- 判断检验是否通过
      IF v_Arr_List.Count <> 0 THEN
      
        ROLLBACK;
      
        v_Arr_Cnt := v_Arr_List.Count;
        FOR i IN 1 .. v_Arr_Cnt LOOP
          Html_Report_Pkg.Output_Line('<BR>' || v_Arr_List(i) || '</BR>');
          -- dbms_output.put_line(v_arr_list(i)); 
        END LOOP;
      
        Raise_Application_Error('-20001', '数据校验失败,请检查数据 !');
      END IF;
    
      INSERT INTO Cux.Cux_Wip_Mes_Body
        (Wip_Entity_Id, Body_Clob)
      VALUES
        (p_Header_Id, Str);
      COMMIT;
    
      v_1 := '<![CDATA[' || Str || ']]>';
      fnd_file.PUT_LINE(fnd_file.LOG, 'aa');
      ---------------------------------------开始调用Webservice-------------
    
      --传入参数
      l_Xml_Char := '<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <GenerateOrderRelease xmlns="http://tempuri.org/">
      <XMLorderReleasestr>' || v_1 || '</XMLorderReleasestr>
    </GenerateOrderRelease>
  </soap:Body>
</soap:Envelope>';
      fnd_file.PUT_LINE(fnd_file.LOG, 'bb');
      /*      l_xml_char := '<?xml version="1.0" encoding="utf-8"?>
      <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
        <soap:Body>
          <GenerateOrderRelease xmlns="http://tempuri.org/">
            <XMLorderReleasestr></XMLorderReleasestr>
          </GenerateOrderRelease>
        </soap:Body>
      </soap:Envelope>';*/
    
      -- 根据当前系统环境,获取对应的接口连接
      v_Request_Url := Cux_External_Interface_Pkg.Getinterfaceurl('TMS');
      fnd_file.PUT_LINE(fnd_file.LOG, 'cc');
      begin
        Req := Utl_Http.Begin_Request(v_Request_Url,
                                      'POST',
                                      Utl_Http.Http_Version_1_1);
      exception
        when others then
          Retcode := 2;
          fnd_file.PUT_LINE(fnd_file.OUTPUT, 'resp error:' || sqlerrm);
          -- fnd_file.PUT_LINE(fnd_file.LOG,'resp error:'||sqlerrm);
          fnd_file.PUT_LINE(fnd_file.LOG, 'begin req error:' || sqlerrm);
          return;
      end;
      fnd_file.PUT_LINE(fnd_file.LOG, 'dd');
      -- 保持连接状态
      Utl_Http.Set_Persistent_Conn_Support(Req, TRUE);
      fnd_file.PUT_LINE(fnd_file.LOG, 'ee');
      --设置编码
    
      Utl_Http.Set_Header(Req, 'Content-Type', 'text/xml; charset=utf-8');
      fnd_file.PUT_LINE(fnd_file.LOG, 'ff');
      Utl_Http.Set_Header(Req,
                          'SOAPAction',
                          '"http://tempuri.org/GenerateOrderRelease"');
      fnd_file.PUT_LINE(fnd_file.LOG, 'gg');
      Utl_Http.Set_Body_Charset(Req, 'utf-8');
      /*      utl_http.set_header(req, 'Content-Length', lengthb(l_xml_char));*/
      /*      v_lengthb:=lengthb(l_xml_char);*/
      /*      utl_http.set_header(req, 'Content-Length', lengthb(l_xml_char));
      
      utl_http.write_line(req, l_xml_char);*/
    
      /*      utl_http.set_header(req, 'Content-Type', 'text/xml; charset=utf-8');
      
      utl_http.set_header(req,
                          'SOAPAction',
                          '"http://tempuri.org/GenerateOrderRelease"');
      
      utl_http.set_body_charset(req, 'utf-8');      */
    
      v_Lengthb := 0;
      v_Length  := Dbms_Lob.Getlength(l_Xml_Char);
      fnd_file.PUT_LINE(fnd_file.LOG, 'hh');
      WHILE v_Offset < v_Length LOOP
        fnd_file.PUT_LINE(fnd_file.LOG, 'in loop');
        Dbms_Lob.Read(l_Xml_Char, v_Buffer_Size, v_Offset, v_Buffer);
        v_Lengthb := v_Lengthb + Lengthb(v_Buffer);
        v_Offset  := v_Offset + v_Buffer_Size;
      END LOOP;
      fnd_file.PUT_LINE(fnd_file.LOG, 'ii');
      Utl_Http.Set_Header(Req, 'Content-Length', v_Lengthb);
    
      /*    \*  dbms_output.put_line(dbms_lob.getlength(l_xml_char));   *\
      \*      utl_http.set_header(req, 'Content-Length',dbms_lob.getlength(l_xml_char));
            utl_http.write_line(req, l_xml_char);*\
      \*      utl_http.write_raw(req,utl_raw.cast_to_raw(l_xml_char));*\
            v_length := dbms_lob.getlength(l_xml_char);
            while v_offset < v_length loop
                    dbms_lob.read(l_xml_char, v_buffer_size, v_offset, v_buffer);
            
            utl_http.write_raw(req,utl_raw.cast_to_raw(v_buffer));
           utl_http.write_line(req, v_buffer);
             v_offset := v_offset + v_buffer_size;
                  end loop;
      */
      /*            utl_http.write_line(req,
                               '<?xml version="1.0" encoding="utf-8"?>
      <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
        <soap:Body>
          <GenerateOrderRelease xmlns="http://tempuri.org/">
            <XMLorderReleasestr>');*/
      v_Offset := 1;
      v_Length := Dbms_Lob.Getlength(l_Xml_Char);
      fnd_file.PUT_LINE(fnd_file.LOG, 'jj');
      WHILE v_Offset < v_Length LOOP
        fnd_file.PUT_LINE(fnd_file.LOG, 'in loop 2');
        Dbms_Lob.Read(l_Xml_Char, v_Buffer_Size, v_Offset, v_Buffer);
        /*        utl_http.write_raw(req,utl_raw.cast_to_raw(v_buffer)); */
        /*        select replace(v_buffer, '&lt;', null)
        into v_buffer
        from dual; */
        Utl_Http.Write_Raw(Req, Utl_Raw.Cast_To_Raw(v_Buffer));
        /*              utl_http.write_line(req, v_buffer);*/
        v_Offset := v_Offset + v_Buffer_Size;
      END LOOP;
      fnd_file.PUT_LINE(fnd_file.LOG, 'kk');
      /*            utl_http.write_line(req,
                               '</XMLorderReleasestr>
          </GenerateOrderRelease>
        </soap:Body>
      </soap:Envelope>');*/
      begin
        Resp := Utl_Http.Get_Response(Req);
      exception
        when others then
          Retcode := 2;
          fnd_file.PUT_LINE(fnd_file.OUTPUT, 'resp error:' || sqlerrm);
          fnd_file.PUT_LINE(fnd_file.LOG, 'resp error:' || sqlerrm);
          return;
      end;
      fnd_file.PUT_LINE(fnd_file.LOG, 'kk2');
      Dbms_Output.Put_Line('this request status is:' || Resp.Status_Code);
      Dbms_Output.Put_Line('this request status is:' || Utl_Http.Http_Ok);
      fnd_file.PUT_LINE(fnd_file.LOG, 'll');
      IF (Resp.Status_Code = Utl_Http.Http_Ok) THEN
        fnd_file.PUT_LINE(fnd_file.LOG, 'ok');
        BEGIN
        
          LOOP
            Utl_Http.Read_Text(Resp, l_Replyline);
            Dbms_Output.Put_Line(l_Replyline);
            l_Rep_Str := l_Rep_Str || l_Replyline;
            Fnd_File.Put_Line(Fnd_File.Log, l_Replyline);
          END LOOP;
          Utl_Http.End_Response(Resp);
        
        EXCEPTION
          WHEN Utl_Http.End_Of_Body THEN
            begin
              Utl_Http.End_Response(Resp);
            exception
              when others then
                Retcode := 2;
                fnd_file.PUT_LINE(fnd_file.OUTPUT,
                                  'END resp error:' || sqlerrm);
                -- fnd_file.PUT_LINE(fnd_file.LOG,'resp error:'||sqlerrm);
                fnd_file.PUT_LINE(fnd_file.LOG,
                                  'END resp error:' || sqlerrm);
                return;
            end;
          WHEN OTHERS THEN
            Retcode := 2;
            fnd_file.PUT_LINE(fnd_file.OUTPUT,
                              'END resp error2:' || sqlerrm);
            -- fnd_file.PUT_LINE(fnd_file.LOG,'resp error:'||sqlerrm);
            fnd_file.PUT_LINE(fnd_file.LOG,
                              'END resp error2:' || sqlerrm);
            return;
        END;
      ELSE
        --尝试原始调用方法
        fnd_file.PUT_LINE(fnd_file.LOG, 'new pkg failed');
        Cux_Tms_Pkg.Main(Retcode           => v_Retcode,
                         Errbuf            => v_Errbuf,
                         p_Organization_Id => p_Organization_Id,
                         p_Header_Id       => p_Header_Id);
        l_Rep_Str := v_Retcode;
      
      END IF;
      fnd_file.PUT_LINE(fnd_file.LOG, 'mm');
      IF l_Rep_Str IS NOT NULL THEN
        fnd_file.PUT_LINE(fnd_file.LOG, 'nn');
        SELECT REPLACE(l_Rep_Str, '“&lt;”', NULL)
          INTO l_Rep_Str
          FROM Dual;
      
        SELECT REPLACE(l_Rep_Str, '&lt;', '<') INTO l_Rep_Str FROM Dual;
      
        SELECT REPLACE(l_Rep_Str, '&gt;', '>') INTO l_Rep_Str FROM Dual;
        fnd_file.PUT_LINE(fnd_file.LOG, 'oo');
        ---解析返回结果
        Parser_Resp(l_Rep_Str, l_Retvalue, l_Message);
        Dbms_Output.Put_Line(l_Retvalue);
        fnd_file.PUT_LINE(fnd_file.LOG, 'pp');
        IF l_Retvalue = '1' THEN
          fnd_file.PUT_LINE(fnd_file.LOG, '1');
          Html_Report_Pkg.v_Report_Output_Mode := 'F';
          Html_Report_Pkg.Output_Line('<BR>' || '传入TMS成功' || '</BR>');
        ELSE
          fnd_file.PUT_LINE(fnd_file.LOG, '2');
          DELETE FROM Cux.Cux_Tms_Line_Interface b
           WHERE b.Header_Id IN (SELECT a.Header_Id
                                   FROM Cux.Cux_Tms_Header_Interface a
                                  WHERE a.Seq = v_Seq);
          DELETE FROM Cux.Cux_Tms_Header_Interface a WHERE a.Seq = v_Seq;
          COMMIT;
          fnd_file.PUT_LINE(fnd_file.LOG, '3');
          IF l_Retvalue = '2' THEN
            l_Message := '已经传入TMS';
          END IF;
          Html_Report_Pkg.v_Report_Output_Mode := 'F';
          Html_Report_Pkg.Output_Line('<BR>' || '传入TMS接口错误,原因' ||
                                      l_Retvalue || Nvl(l_Message, '-') ||
                                      '</BR>');
          v_Concurrent_Status := Fnd_Concurrent.Set_Completion_Status('WARNING',
                                                                      '传入TMS接口错误!');
          fnd_file.PUT_LINE(fnd_file.LOG, '4');
        END IF;
      ELSE
        fnd_file.PUT_LINE(fnd_file.LOG, 'else ee');
        Html_Report_Pkg.v_Report_Output_Mode := 'F';
        Html_Report_Pkg.Output_Line('<BR>' || '传入TMS接口错误,原因无法传递</BR>');
        v_Concurrent_Status := Fnd_Concurrent.Set_Completion_Status('WARNING',
                                                                    '传入TMS接口错误!');
      END IF;
    
    END LOOP;
    IF Str IS NULL THEN
      fnd_file.PUT_LINE(fnd_file.LOG, 'str');
      --分析原因
      Html_Report_Pkg.v_Report_Output_Mode := 'F';
      Html_Report_Pkg.Output_Line('<BR>' || '单据不满足传TMS条件' || '</BR>');
      SELECT COUNT(*)
        INTO v_Count
        FROM Mtl_Txn_Request_Headers   Mth,
             Mtl_Txn_Request_Lines     Mtl,
             Oe_Order_Lines_All        Oola,
             Oe_Order_Headers_All      Ooha,
             Cux_Om_Line_Interface_v   Col,
             Cux_Om_Header_Interface_v Coh
       WHERE Mth.Header_Id = Mtl.Header_Id
         AND Oola.Header_Id = Ooha.Header_Id
         AND Mtl.Txn_Source_Line_Id = Oola.Line_Id
         AND Oola.Line_Id = Col.Order_Line_Id(+)
         AND Ooha.Header_Id = Coh.Order_Header_Id(+)
         AND Mtl.Header_Id = p_Header_Id
         AND Mtl.Organization_Id = p_Organization_Id
         AND Col.Freight_Attribute IN ('20', '40', '50');
      IF v_Count > 0 THEN
        Html_Report_Pkg.Output_Line('<BR>' || '货运属性不能是不发货有回执,不发货有回执,供应商直发' ||
                                    '</BR>');
      END IF;
    
      SELECT COUNT(*)
        INTO v_Count
        FROM Mtl_Txn_Request_Headers   Mth,
             Mtl_Txn_Request_Lines     Mtl,
             Oe_Order_Lines_All        Oola,
             Oe_Order_Headers_All      Ooha,
             Cux_Om_Line_Interface_v   Col,
             Cux_Om_Header_Interface_v Coh
       WHERE Mth.Header_Id = Mtl.Header_Id
         AND Oola.Header_Id = Ooha.Header_Id
         AND Mtl.Txn_Source_Line_Id = Oola.Line_Id
         AND Oola.Line_Id = Col.Order_Line_Id(+)
         AND Ooha.Header_Id = Coh.Order_Header_Id(+)
         AND Mtl.Header_Id = p_Header_Id
         AND Mtl.Organization_Id = p_Organization_Id
         AND Ooha.Order_Type_Id NOT IN
             (SELECT c.Transaction_Type_Id
                FROM Cux_Om_Transaction_Type_v c
               WHERE c.Name IN ('111借机转销售订单', '112借机转销售订单'));
      IF v_Count > 0 THEN
        Html_Report_Pkg.Output_Line('<BR>' ||
                                    '订单类型不能是111借机转销售订单和112借机转销售订单' ||
                                    '</BR>');
      END IF;
    
      v_Concurrent_Status := Fnd_Concurrent.Set_Completion_Status('WARNING',
                                                                  '单据不满足传TMS条件!');
    END IF;
  
    -- 异常处理
  EXCEPTION
    WHEN OTHERS THEN
      Retcode := 1;
      Errbuf  := SQLERRM;
    
  END;
  PROCEDURE Parser_Resp(p_Xml_Str  IN VARCHAR2,
                        l_Retvalue OUT VARCHAR2,
                        l_Message  OUT VARCHAR2) AS
    l_Doc           Xmldom.Domdocument;
    Et              Xmldom.Domelement;
    Rootnode        Xmldom.Domnode;
    l_Rootnode_Name VARCHAR2(200);
    Myxml           Xmltype;
    l_Nl            Dbms_Xmldom.Domnodelist;
    l_n             Dbms_Xmldom.Domnode;
    l_Count         NUMBER;
  BEGIN
    Myxml    := Xmltype(p_Xml_Str);
    l_Doc    := Xmldom.Newdomdocument(Myxml);
    Rootnode := Xmldom.Makenode(Xmldom.Getdocumentelement(l_Doc));
  
    l_Nl    := Dbms_Xmldom.Getelementsbytagname(l_Doc, 'Service');
    l_Count := Dbms_Xmldom.Getlength(l_Nl);
    FOR Cur_Emp IN 0 .. Dbms_Xmldom.Getlength(l_Nl) - 1 LOOP
      l_n := Dbms_Xmldom.Item(l_Nl, Cur_Emp);
      Dbms_Xslprocessor.Valueof(l_n, 'Status/text()', l_Retvalue);
      Dbms_Xslprocessor.Valueof(l_n, 'Message/text()', l_Message);
    END LOOP;
  END;

  -- 数据校验
  PROCEDURE Data_Validation(p_Arr_List   IN OUT Array_Table,
                            p_Data       IN VARCHAR2,
                            p_Max_Length IN NUMBER,
                            p_Label      IN VARCHAR2,
                            p_Need       IN NUMBER DEFAULT 0) AS
    Result_Value VARCHAR2(500);
  BEGIN
    IF p_Data IS NOT NULL THEN
      Result_Value := Cux_Data_Validation_Pkg.Str_Length_Validation(p_Data,
                                                                    p_Max_Length,
                                                                    p_Label);
    ELSE
      -- 如果数据为空,则进一步判断数据是否是必填项
      IF p_Need != 0 THEN
        Result_Value := p_Label || ' 不能为空.';
      END IF;
    END IF;
  
    -- 如果不为空,才保存数据
    IF Result_Value IS NOT NULL THEN
      p_Arr_List(p_Arr_List.Count + 1) := Result_Value;
    END IF;
  END;
END;
/
