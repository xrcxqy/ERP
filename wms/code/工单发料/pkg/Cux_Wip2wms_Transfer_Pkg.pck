CREATE OR REPLACE PACKAGE APPS.Cux_Wip2wms_Transfer_Pkg IS

  -- Author  : EVAN
  -- Created : 2015/10/14 14:17:02
  -- Purpose : 
  TYPE Wip_Details_Tab IS TABLE OF Cux_Wip_Combination_Details%ROWTYPE INDEX BY PLS_INTEGER;
  PROCEDURE Accept_Wip_Combination(p_Combination_Info IN CLOB,
                                   x_Response         OUT CLOB);
  --获取MES工单号
  FUNCTION Get_Mes_Job_Number(p_Wip_Entity_Id NUMBER) RETURN VARCHAR2;

  PROCEDURE Transfer_Combination_2_Mes(x_Ret_Status      OUT VARCHAR2,
                                       x_Error_Msg       OUT VARCHAR2,
                                       p_Organization_Id NUMBER,
                                       p_Doc_No          VARCHAR2,
                                       p_Full_Flag       VARCHAR2);
  --处理工单发料数据
  PROCEDURE Transfer_Wip_Issue_Data(x_Ret_Sts         OUT VARCHAR2,
                                    x_Error_Msg       OUT VARCHAR2,
                                    p_Organization_Id NUMBER,
                                    p_Doc_No          VARCHAR2,
                                    p_Full_Flag       VARCHAR2);
  --处理工单推式还料数据
  PROCEDURE Transfer_Wip_Return_Data(x_Ret_Sts         OUT VARCHAR2,
                                     x_Error_Msg       OUT VARCHAR2,
                                     p_Organization_Id NUMBER,
                                     p_Doc_No          VARCHAR2);
  --处理工单拉式还退料                      
  PROCEDURE Transfer_Wip_Pull_Return_Data(x_Ret_Sts        OUT VARCHAR2,
                                          x_Error_Msg      OUT VARCHAR2,
                                          p_Transaction_Id NUMBER);
END Cux_Wip2wms_Transfer_Pkg;
/
CREATE OR REPLACE PACKAGE BODY APPS.Cux_Wip2wms_Transfer_Pkg IS
  g_Package_Name VARCHAR2(30) := 'CUX_WIP2WMS_TRANSFER_PKG';

  FUNCTION Get_Mes_Job_Number(p_Wip_Entity_Id NUMBER) RETURN VARCHAR2 IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    l_Wip_Entity_Info Cux_Wip_Entity_Infos%ROWTYPE;
    l_Max_Sequence    NUMBER;
  BEGIN
    BEGIN
      SELECT *
        INTO l_Wip_Entity_Info
        FROM Cux_Wip_Entity_Infos
       WHERE Wip_Entity_Id = p_Wip_Entity_Id;
      RETURN l_Wip_Entity_Info.Mes_Job_Number;
    EXCEPTION
      WHEN No_Data_Found THEN
        NULL;
    END;
    --取得OU信息
    SELECT Ood.Operating_Unit, We.Organization_Id, We.Wip_Entity_Id
      INTO l_Wip_Entity_Info.Org_Id
          ,l_Wip_Entity_Info.Organization_Id
          ,l_Wip_Entity_Info.Wip_Entity_Id
      FROM Wip_Entities We, Org_Organization_Definitions Ood
     WHERE We.Organization_Id = Ood.Organization_Id
       AND We.Wip_Entity_Id = p_Wip_Entity_Id;
    --取得公司代码
    SELECT t.Lookup_Code
      INTO l_Wip_Entity_Info.Company_Code
      FROM Fnd_Lookup_Values_Vl t
     WHERE 'CUX_LEGAL_ENTITY' = t.Lookup_Type
       AND l_Wip_Entity_Info.Org_Id = t.Meaning;
    --取得已使用序号
    l_Wip_Entity_Info.Request_Date := Trunc(SYSDATE);
    SELECT MAX(t.Sequence_No)
      INTO l_Max_Sequence
      FROM Cux_Wip_Entity_Infos t
     WHERE t.Org_Id = l_Wip_Entity_Info.Org_Id
       AND t.Request_Date = l_Wip_Entity_Info.Request_Date;
    --FW:  G1511-年+月+日+四位流水号 ，例如：G1511-1409100001
    --JW:  H1511-年+月+日+四位流水号 ，例如：H1511-1409100001
    l_Wip_Entity_Info.Sequence_No    := Nvl(l_Max_Sequence, 0) + 1;
    l_Wip_Entity_Info.Mes_Job_Number := l_Wip_Entity_Info.Company_Code ||
                                        '1511-' ||
                                        To_Char(l_Wip_Entity_Info.Request_Date,
                                                'YYMMDD') ||
                                        Lpad(To_Char(l_Wip_Entity_Info.Sequence_No),
                                             4,
                                             '0');
  
    l_Wip_Entity_Info.Creation_Date     := SYSDATE;
    l_Wip_Entity_Info.Created_By        := Fnd_Global.User_Id;
    l_Wip_Entity_Info.Last_Update_Date  := SYSDATE;
    l_Wip_Entity_Info.Last_Updated_By   := Fnd_Global.User_Id;
    l_Wip_Entity_Info.Last_Update_Login := Fnd_Global.Login_Id;
    INSERT INTO Cux_Wip_Entity_Infos VALUES l_Wip_Entity_Info;
    COMMIT;
    RETURN l_Wip_Entity_Info.Mes_Job_Number;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END Get_Mes_Job_Number;

  FUNCTION Response_Wip_Combination(p_Ret_Status VARCHAR2,
                                    p_Error_Msg  VARCHAR2,
                                    p_Request    Cux_Soa_Requests%ROWTYPE)
    RETURN CLOB IS
    l_Response_Msg CLOB;
  BEGIN
    l_Response_Msg := l_Response_Msg || '<STD_OUT origin="ERP">';
    l_Response_Msg := l_Response_Msg || '<Service Name="SetData">';
    IF p_Ret_Status = Fnd_Api.g_Ret_Sts_Success THEN
      l_Response_Msg := l_Response_Msg || '<Status>0</Status>';
    ELSE
      l_Response_Msg := l_Response_Msg || '<Status>1</Status>';
    END IF;
    l_Response_Msg := l_Response_Msg || '<Error>' || p_Error_Msg ||
                      '</Error>';
    l_Response_Msg := l_Response_Msg || '<OperDate>' ||
                      To_Char(SYSDATE, 'YYYY-MM-DD HH24:MI:SS') ||
                      '</OperDate>'; --操作时间
    l_Response_Msg := l_Response_Msg || '</Service></STD_OUT>';
  
    UPDATE Cux_Soa_Requests t
       SET t.Response_Date   = SYSDATE
          ,t.Response_Msg    = l_Response_Msg
          ,t.Response_Status = Decode(p_Ret_Status,
                                      Fnd_Api.g_Ret_Sts_Success,
                                      'S',
                                      'F')
          ,t.Entity_Type     = p_Request.Entity_Type
          ,t.Entity_Id       = p_Request.Entity_Id
          ,t.Error_Msg       = Decode(p_Ret_Status,
                                      Fnd_Api.g_Ret_Sts_Success,
                                      NULL,
                                      p_Error_Msg)
     WHERE t.Request_Id = p_Request.Request_Id;
    COMMIT;
    RETURN l_Response_Msg;
  END Response_Wip_Combination;

  PROCEDURE Transfer_Combination_2_Mes(x_Ret_Status      OUT VARCHAR2,
                                       x_Error_Msg       OUT VARCHAR2,
                                       p_Organization_Id NUMBER,
                                       p_Doc_No          VARCHAR2,
                                       p_Full_Flag       VARCHAR2) IS
    l_Assembly_Item      NUMBER;
    l_Wip_Entity_Name    VARCHAR2(30);
    l_Operation_Code     Bom_Standard_Operations.Operation_Code%TYPE;
    l_Wip_Entity_Id      NUMBER;
    l_Count_Tmp          NUMBER;
    l_Combination_Number Cux_Wip_Combinations.Combination_Number%TYPE;
    l_Combination_Name   Cux_Wip_Combinations.Combination_Name%TYPE;
    l_Sequence_No        NUMBER;
    l_Flex_Set_Id        NUMBER;
    l_Item_Number        VARCHAR2(30);
    l_Item_Category_Code VARCHAR2(30);
    l_Item_Made_Type     VARCHAR2(10);
    l_Mes_Job_Number     VARCHAR2(30);
    l_Combination_Flag   VARCHAR2(1);
  
    l_Destination_System  VARCHAR2(30) := 'MES';
    l_Mes_Info            CLOB;
    l_Service_Info        CLOB;
    l_Response_Info       CLOB;
    l_Request             Cux_Soa_Requests%ROWTYPE;
    l_Request_Id          NUMBER;
    l_Service_Url         VARCHAR2(200);
    l_Soa_Method          VARCHAR2(50);
    l_Soa_Action          VARCHAR2(250);
    l_Resp_Parameter_Name VARCHAR2(50);
    l_Content_Ret_Sts     VARCHAR2(10);
    l_Content_Error_Msg   VARCHAR2(500);
  
    l_Ret_Status VARCHAR2(10);
    l_Error_Msg  VARCHAR2(200);
  BEGIN
    x_Ret_Status := Fnd_Api.g_Ret_Sts_Success;
    --取得工单信息
    SELECT We.Primary_Item_Id
          ,We.Wip_Entity_Name
          ,Cri.Operation_Code
          ,We.Wip_Entity_Id
      INTO l_Assembly_Item
          ,l_Wip_Entity_Name
          ,l_Operation_Code
          ,l_Wip_Entity_Id
      FROM Cux_Ready_Item Cri, Wip_Entities We
     WHERE Cri.Organization_Id = p_Organization_Id
       AND Cri.Doc_No = p_Doc_No
       AND Cri.Wip_Entity_Id = We.Wip_Entity_Id
       AND Rownum = 1;
    IF p_Full_Flag = 'Y' THEN
      l_Operation_Code := 'ZD';
    END IF;
    --检查是否已经抛转MES
    SELECT COUNT(*)
      INTO l_Count_Tmp
      FROM Cux_Mes_Issue_Orders t
     WHERE t.Wip_Entity_Id = l_Wip_Entity_Id
       AND t.Operation_Seq = l_Operation_Code;
    IF l_Count_Tmp > 0 THEN
      RETURN;
    END IF;
    --获取合并单信息
    BEGIN
      SELECT h.Combination_Number, h.Combination_Name, d.Sequence_No
        INTO l_Combination_Number, l_Combination_Name, l_Sequence_No
        FROM Cux_Wip_Combination_Details d, Cux_Wip_Combinations h
       WHERE d.Organization_Id = p_Organization_Id
         AND d.Wip_Entity_Id = l_Wip_Entity_Id
         AND d.Active_Flag = 'Y'
         AND d.Combination_Id = h.Combination_Id
         AND h.Combination_Request_Id <> 0;
      l_Combination_Flag := 'Y';
    EXCEPTION
      WHEN OTHERS THEN
        l_Combination_Number := NULL;
        l_Combination_Name   := NULL;
        l_Sequence_No        := NULL;
        l_Combination_Flag   := 'N';
    END;
    --取得料号信息
    SELECT t.Flex_Value_Set_Id
      INTO l_Flex_Set_Id
      FROM Fnd_Flex_Value_Sets t
     WHERE t.Flex_Value_Set_Name = 'CUX_ITEM_CATEGORY_CODE';
  
    SELECT t.Segment1, t.Attribute7
      INTO l_Item_Number, l_Item_Category_Code
      FROM Mtl_System_Items_b t
     WHERE t.Organization_Id = p_Organization_Id
       AND t.Inventory_Item_Id = l_Assembly_Item;
  
    SELECT t.Attribute2
      INTO l_Item_Made_Type
      FROM Fnd_Flex_Values t
     WHERE t.Flex_Value_Set_Id = l_Flex_Set_Id
       AND t.Flex_Value = l_Item_Category_Code;
    IF l_Item_Made_Type IS NULL THEN
      x_Error_Msg := '未维护产品生产分类。';
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
    --获取工单属性
    l_Mes_Job_Number := Get_Mes_Job_Number(l_Wip_Entity_Id);
    IF l_Mes_Job_Number IS NULL THEN
      x_Error_Msg := '获取工单号失败。';
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
    --抛转合并发料信息至MES
    l_Mes_Info := '<?xml version="1.0" encoding="UTF-8"?>';
    l_Mes_Info := l_Mes_Info || '<STD_IN Origin="MES">';
    l_Mes_Info := l_Mes_Info || '<ObjectID>MERGEWONO</ObjectID>';
    l_Mes_Info := l_Mes_Info || '<Data>';
    l_Mes_Info := l_Mes_Info || '<MERGEID>' || l_Combination_Number ||
                  '</MERGEID>';
    l_Mes_Info := l_Mes_Info || '<MERGENO>' || l_Combination_Name ||
                  '</MERGENO>';
    l_Mes_Info := l_Mes_Info || '<STOCKID>' || p_Doc_No || '</STOCKID>';
    l_Mes_Info := l_Mes_Info || '<WONO>' || l_Mes_Job_Number || '</WONO>';
    l_Mes_Info := l_Mes_Info || '<PLANORDER>' || To_Char(l_Sequence_No) ||
                  '</PLANORDER>';
    l_Mes_Info := l_Mes_Info || '<MERGETAG>' || l_Combination_Flag ||
                  '</MERGETAG>';
    l_Mes_Info := l_Mes_Info || '<WOORDERNO>' || l_Wip_Entity_Name ||
                  '</WOORDERNO>';
    l_Mes_Info := l_Mes_Info || '<MATCODE>' || l_Item_Number ||
                  '</MATCODE>';
    l_Mes_Info := l_Mes_Info || '<PRODUCTTYPE>' || l_Item_Made_Type ||
                  '</PRODUCTTYPE>';
    l_Mes_Info := l_Mes_Info || '<GONGXU>' || l_Operation_Code ||
                  '</GONGXU>';
    l_Mes_Info := l_Mes_Info || '</Data>';
    l_Mes_Info := l_Mes_Info || '</STD_IN>';
    --如果有需要发布的数据
    l_Service_Url         := Cux_External_Interface_Pkg.Getinterfaceurl(l_Destination_System);
    l_Soa_Method          := 'GenerateMesInfo';
    l_Resp_Parameter_Name := 'GenerateMesInfoResult';
    --记录SOA请求
    l_Request.Entity_Type     := 'MES_ISSUE';
    l_Request.Entity_Id       := l_Wip_Entity_Name;
    l_Request.Direction       := 'OUTBOUND';
    l_Request.Interface       := 'CUX_WIP2WMS_TRANSFER_PKG';
    l_Request.Method          := 'TRANSFER_COMBINATION_2_MES';
    l_Request.Relative_System := l_Destination_System;
    l_Request.Url             := l_Service_Url;
    l_Request.Request_Date    := SYSDATE;
    l_Request.Request_Msg     := l_Mes_Info;
    Cux_Soa_Pub.Create_Request(l_Request, l_Request_Id);
    --发布接收数据
    l_Service_Info := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:tem="http://tempuri.org/">';
    l_Service_Info := l_Service_Info || Chr(10) || '<soapenv:Header/>';
    l_Service_Info := l_Service_Info || Chr(10) || '<soapenv:Body>';
    l_Service_Info := l_Service_Info || Chr(10) || '<tem:' || l_Soa_Method || '>';
    l_Service_Info := l_Service_Info || Chr(10) || '<tem:stdin><![CDATA[' ||
                      l_Mes_Info || ']]></tem:stdin>';
    l_Service_Info := l_Service_Info || Chr(10) || '</tem:' || l_Soa_Method || '>';
    l_Service_Info := l_Service_Info || Chr(10) || '</soapenv:Body>';
    l_Service_Info := l_Service_Info || Chr(10) || '</soapenv:Envelope>';
  
    l_Soa_Action := 'http://tempuri.org/' || l_Soa_Method;
    --dbms_output.put_line(l_Service_Info);
    Cux_Soa_Pub.Call_Java_Web_Service(p_Service_Url      => l_Service_Url,
                                      p_Charset          => 'utf-8',
                                      p_Name_Space       => 'http://tempuri.org/',
                                      p_Soa_Action       => l_Soa_Action,
                                      p_Message_Content  => l_Service_Info,
                                      p_Response_Node    => l_Resp_Parameter_Name,
                                      x_Ret_Sts          => l_Ret_Status,
                                      x_Error_Msg        => l_Error_Msg,
                                      x_Response_Content => l_Response_Info);
  
    --解析发布结果
    IF l_Ret_Status = Fnd_Api.g_Ret_Sts_Success THEN
      Cux_Soa_Pub.Parser_Resp_Mes(p_Xml_Clob       => l_Response_Info,
                                  x_Sts_Node_Value => l_Content_Ret_Sts,
                                  x_Msg_Node_Value => l_Content_Error_Msg,
                                  x_Ret_Status     => l_Ret_Status,
                                  x_Error_Message  => l_Error_Msg);
    END IF;
    IF l_Ret_Status <> Fnd_Api.g_Ret_Sts_Success THEN
      l_Content_Error_Msg       := Nvl(l_Content_Error_Msg, l_Error_Msg);
      l_Request.Response_Status := 'F';
    ELSIF Nvl(l_Content_Ret_Sts, '1') = '0' THEN
      l_Request.Response_Status := 'S';
      INSERT INTO Cux_Mes_Issue_Orders
      VALUES
        (l_Wip_Entity_Id, l_Operation_Code, SYSDATE);
    ELSE
      l_Request.Response_Status := 'F';
    END IF;
    --返回调用信息
    IF l_Request.Response_Status = 'F' THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := l_Content_Error_Msg;
    END IF;
    --记录SOA请求
    l_Request.Response_Msg := l_Response_Info;
    l_Request.Error_Msg    := l_Content_Error_Msg;
    Cux_Soa_Pub.Update_Request(l_Request);
  
  EXCEPTION
    WHEN Fnd_Api.g_Exc_Error THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
    WHEN OTHERS THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Unexp_Error;
      x_Error_Msg  := SQLERRM;
  END Transfer_Combination_2_Mes;

  PROCEDURE Get_Wip_Combination(x_Ret_Sts         OUT VARCHAR2,
                                x_Error_Msg       OUT VARCHAR2,
                                p_Doc             Dbms_Xmldom.Domdocument,
                                x_Soa_Header      OUT Cux_Soa_Pub.Soa_Context_Header,
                                x_Request         IN OUT Cux_Soa_Requests%ROWTYPE,
                                x_Wip_Combination OUT Cux_Wip_Combinations%ROWTYPE,
                                x_Wip_Detail_Tab  OUT Wip_Details_Tab) IS
    l_Node_List         Dbms_Xmldom.Domnodelist;
    l_Node              Dbms_Xmldom.Domnode;
    l_Node_Count        NUMBER;
    l_Tmp_String        VARCHAR2(500);
    l_Tmp_Count         NUMBER;
    l_Wip_Entity_Detail VARCHAR2(200);
  
    l_Item                 VARCHAR2(30);
    l_Null_Wip_Detail      Cux_Wip_Combination_Details%ROWTYPE;
    l_Wip_Detail           Cux_Wip_Combination_Details%ROWTYPE;
    l_Entity_Count         NUMBER;
    l_Combination_Name     Cux_Wip_Combinations.Combination_Name%TYPE;
    l_Orig_Wip_Combination Cux_Wip_Combinations%ROWTYPE;
  BEGIN
    x_Ret_Sts := Fnd_Api.g_Ret_Sts_Success;
    --读取SOA头信息
    l_Node_List := Dbms_Xmldom.Getelementsbytagname(p_Doc, 'STD_IN');
    l_Node      := Dbms_Xmldom.Item(l_Node_List, 0);
  
    --法人
    l_Item := 'Factory';
    Dbms_Xslprocessor.Valueof(l_Node, 'Factory/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL THEN
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg := '企业法人为空。';
      RETURN;
    ELSE
      x_Soa_Header.Factory := l_Tmp_String;
      BEGIN
        SELECT To_Number(t.Meaning), Ood.Organization_Id
          INTO x_Soa_Header.Org_Id, x_Soa_Header.Organization_Id
          FROM Fnd_Lookup_Values_Vl t, Org_Organization_Definitions Ood
         WHERE t.Lookup_Type = 'CUX_LEGAL_ENTITY'
           AND t.Lookup_Code = l_Tmp_String
           AND t.Tag = Ood.Organization_Code;
        x_Wip_Combination.Org_Id          := x_Soa_Header.Org_Id;
        x_Wip_Combination.Organization_Id := x_Soa_Header.Organization_Id;
      EXCEPTION
        WHEN OTHERS THEN
          x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg := '企业法人错误:' || l_Tmp_String;
          RETURN;
      END;
    END IF;
    --Source 不需要
    /* l_Item := 'Source';
    Dbms_Xslprocessor.Valueof(l_Node, 'Source/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL OR l_Tmp_String NOT IN ('SMTN', 'WMS') THEN
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg := '来源为空或者错误:' || l_Tmp_String;
      RETURN;
    ELSE
      x_Soa_Header.Source_System := l_Tmp_String;
      x_Request.Relative_System  := x_Soa_Header.Source_System;
    END IF;*/
    --ObjectID
    l_Item := 'ObjectID';
    Dbms_Xslprocessor.Valueof(l_Node, 'ObjectID/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL OR l_Tmp_String <> 'SET_HBGD_Oracle' THEN
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg := 'ObjectID为空或者错误:' || l_Tmp_String;
      RETURN;
    END IF;
  
    --读取合并单信息
    l_Node_List  := Dbms_Xmldom.Getelementsbytagname(p_Doc, 'CPHS');
    l_Node_Count := Dbms_Xmldom.Getlength(l_Node_List);
    IF l_Node_Count = 0 THEN
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg := '合并单无数据。';
      RETURN;
    END IF;
    FOR i IN 1 .. l_Node_Count LOOP
      l_Wip_Detail                 := l_Null_Wip_Detail;
      l_Node                       := Dbms_Xmldom.Item(l_Node_List, i - 1);
      l_Wip_Detail.Organization_Id := x_Wip_Combination.Organization_Id;
      --传票号
      l_Item := 'CPH';
      Dbms_Xslprocessor.Valueof(l_Node, l_Item || '/text()', l_Tmp_String);
      IF l_Tmp_String IS NULL THEN
        x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg := '传票号为空。';
        RETURN;
      ELSE
        l_Wip_Detail.Wip_Entity_Name := l_Tmp_String;
        --检查传票号是否存在
        BEGIN
          SELECT t.Wip_Entity_Id
            INTO l_Wip_Detail.Wip_Entity_Id
            FROM Wip_Entities t
           WHERE t.Wip_Entity_Name = l_Wip_Detail.Wip_Entity_Name
             AND t.Organization_Id = l_Wip_Detail.Organization_Id;
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
            x_Error_Msg := l_Wip_Entity_Detail || ':传票号不存在。';
            RETURN;
        END;
        --检查传票号是否已经备料
        SELECT COUNT(*)
          INTO l_Tmp_Count
          FROM Cux_Ready_Item Cri, Fnd_Lookup_Values_Vl Flv
         WHERE Flv.Lookup_Type = 'CUX_INV_TRX_TYPE'
           AND Cri.Doc_Type = Flv.Meaning
           AND Flv.Tag = 'WIP'
           AND Flv.Attribute1 = 'O'
           AND Cri.Organization_Id = l_Wip_Detail.Organization_Id
           AND Cri.Wip_Entity_Id = l_Wip_Detail.Wip_Entity_Id;
      
        IF l_Tmp_Count > 0 THEN
          x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg := l_Wip_Entity_Detail || ':传票号已经备料。';
          RETURN;
        END IF;
      
        BEGIN
          SELECT t.Combination_Id, t.Sequence_No, t.Active_Flag
            INTO l_Wip_Detail.Combination_Id
                ,l_Wip_Detail.Sequence_No
                ,l_Wip_Detail.Active_Flag
            FROM Cux_Wip_Combination_Details t, Cux_Wip_Combinations c
           WHERE t.Combination_Id = c.Combination_Id
             AND t.Organization_Id = l_Wip_Detail.Organization_Id
             AND t.Wip_Entity_Id = l_Wip_Detail.Wip_Entity_Id
             AND t.Active_Flag = 'Y'
             AND c.Combination_Request_Id <> 0;
        EXCEPTION
          WHEN OTHERS THEN
            NULL;
        END;
        IF l_Wip_Detail.Combination_Id IS NOT NULL THEN
          --取消合并单号  
          IF l_Orig_Wip_Combination.Combination_Id IS NULL THEN
            --取得原始合并单号信息
            SELECT *
              INTO l_Orig_Wip_Combination
              FROM Cux_Wip_Combinations c
             WHERE c.Combination_Id = l_Wip_Detail.Combination_Id;
          ELSIF l_Orig_Wip_Combination.Combination_Id <>
                l_Wip_Detail.Combination_Id THEN
            --如果行的合并信息不一致，报错返回
            x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
            x_Error_Msg := l_Wip_Entity_Detail || ':撤销合并的工单不在同一个合并单号下。';
            RETURN;
          END IF;
        END IF;
      END IF;
    
      IF i = 1 THEN
        --合并总单号
        l_Item := 'HBZDH';
        Dbms_Xslprocessor.Valueof(l_Node,
                                  l_Item || '/text()',
                                  x_Wip_Combination.Combination_Number);
      
        --合并单号
        l_Item := 'HBDH';
        Dbms_Xslprocessor.Valueof(l_Node,
                                  l_Item || '/text()',
                                  x_Wip_Combination.Combination_Name);
      
        --合并数量
        l_Item := 'HBSL';
        Dbms_Xslprocessor.Valueof(l_Node,
                                  l_Item || '/text()',
                                  l_Tmp_String);
        BEGIN
          x_Wip_Combination.Entity_Count := To_Number(l_Tmp_String);
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
            x_Error_Msg := '合并数量格式异常';
            RETURN;
        END;
      ELSE
        --合并总单号
        l_Item := 'HBZDH';
        Dbms_Xslprocessor.Valueof(l_Node,
                                  l_Item || '/text()',
                                  l_Tmp_String);
        IF Nvl(x_Wip_Combination.Combination_Number, '#NULL') <>
           Nvl(l_Tmp_String, '#NULL') THEN
          x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg := '合并总单号不一致。';
          RETURN;
        END IF;
      
        --合并单号
        l_Item := 'HBDH';
        Dbms_Xslprocessor.Valueof(l_Node,
                                  l_Item || '/text()',
                                  l_Tmp_String);
        IF Nvl(x_Wip_Combination.Combination_Name, '#NULL') <>
           Nvl(l_Tmp_String, '#NULL') THEN
          x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg := '合并单号不一致。';
          RETURN;
        END IF;
      
        --合并数量
        l_Item := 'HBSL';
        Dbms_Xslprocessor.Valueof(l_Node,
                                  l_Item || '/text()',
                                  l_Tmp_String);
        IF Nvl(To_Char(x_Wip_Combination.Entity_Count), '#NULL') <>
           Nvl(l_Tmp_String, '#NULL') THEN
          x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg := '合并数量不一致。';
          RETURN;
        END IF;
      END IF;
      --合并顺序
      l_Item := 'HBSX';
      Dbms_Xslprocessor.Valueof(l_Node, l_Item || '/text()', l_Tmp_String);
      BEGIN
        l_Wip_Detail.Sequence_No := To_Number(l_Tmp_String);
      EXCEPTION
        WHEN OTHERS THEN
          x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg := '合并顺序格式异常';
          RETURN;
      END;
      --将合并工单信息加入到数组中
      x_Wip_Detail_Tab(x_Wip_Detail_Tab.Count + 1) := l_Wip_Detail;
    END LOOP;
  
    IF x_Wip_Combination.Combination_Number IS NULL THEN
      x_Request.Entity_Type := 'CANCEL_COMBINATION';
      --合并总单号为空，表示需要取消合并单号   
    
      --检查撤销数量和节点数量是否一致
      IF l_Orig_Wip_Combination.Entity_Count <> x_Wip_Detail_Tab.Count THEN
        x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg := '撤销数量与原合并数量不一致。';
        RETURN;
      END IF;
      FOR i IN 1 .. x_Wip_Detail_Tab.Count LOOP
        l_Wip_Detail        := x_Wip_Detail_Tab(i);
        l_Wip_Entity_Detail := l_Wip_Detail.Wip_Entity_Name;
        IF l_Wip_Detail.Combination_Id IS NULL THEN
          --校验传票是否已经被合并        
          x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg := l_Wip_Entity_Detail || ':传票号未合并。';
          RETURN;
        END IF;
      END LOOP;
      x_Wip_Combination.Combination_Id    := l_Wip_Detail.Combination_Id;
      x_Wip_Combination.Active_Flag       := 'N';
      x_Wip_Combination.Cancel_Date       := SYSDATE;
      x_Wip_Combination.Cancel_Request_Id := x_Request.Request_Id;
    ELSE
      /*合并传票*/
      x_Request.Entity_Type := 'CREATE_COMBINATION';
      --检查撤销数量和节点数量是否一致
      IF x_Wip_Combination.Entity_Count <> x_Wip_Detail_Tab.Count THEN
        x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg := '合并数量与工单明细数量不一致。';
        RETURN;
      END IF;
    
      IF x_Wip_Combination.Combination_Number IS NULL THEN
        x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg := '合并单号不能为空。';
        RETURN;
      END IF;
    
      IF x_Wip_Combination.Combination_Name IS NULL THEN
        x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg := '合并总单号不能为空。';
        RETURN;
      END IF;
    
      FOR i IN 1 .. x_Wip_Detail_Tab.Count LOOP
        l_Wip_Detail        := x_Wip_Detail_Tab(i);
        l_Wip_Entity_Detail := l_Wip_Detail.Wip_Entity_Name;
        --校验传票是否被合并
        IF l_Wip_Detail.Combination_Id IS NOT NULL THEN
          x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg := l_Wip_Entity_Detail || ':传票号已经被合并。';
          RETURN;
        END IF;
        --检验合并顺序
        IF l_Wip_Detail.Sequence_No IS NULL THEN
          x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg := l_Wip_Entity_Detail || ':合并顺序不能为空。';
          RETURN;
        ELSIF l_Wip_Detail.Sequence_No > x_Wip_Combination.Entity_Count THEN
          x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg := l_Wip_Entity_Detail || ':合并顺序大于合并数量。';
          RETURN;
        END IF;
      END LOOP;
      --记录合并日期以及请求
      x_Wip_Combination.Active_Flag            := 'Y';
      x_Wip_Combination.Combination_Date       := SYSDATE;
      x_Wip_Combination.Combination_Request_Id := x_Request.Request_Id;
    END IF;
  
    x_Request.Entity_Id := x_Wip_Combination.Combination_Number;
  
  EXCEPTION
    WHEN OTHERS THEN
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Unexp_Error;
      x_Error_Msg := l_Wip_Entity_Detail || ':' || l_Item || ' ' || SQLERRM;
  END Get_Wip_Combination;

  PROCEDURE Process_Wip_Combination(x_Ret_Sts          OUT VARCHAR2,
                                    x_Error_Msg        OUT VARCHAR2,
                                    p_Request          Cux_Soa_Requests%ROWTYPE,
                                    p_Soa_Header       Cux_Soa_Pub.Soa_Context_Header,
                                    x_Wip_Combination  IN OUT NOCOPY Cux_Wip_Combinations%ROWTYPE,
                                    p_Wip_Detail_Tab   Wip_Details_Tab,
                                    p_Combination_Info IN CLOB) IS
    l_Wip_Detail     Cux_Wip_Combination_Details%ROWTYPE;
    l_Mes_Ret_Status VARCHAR2(10);
    l_Mes_Error_Msg  VARCHAR2(200);
    l_Count          NUMBER;
    l_Combination_Id NUMBER;
  BEGIN
    x_Ret_Sts := Fnd_Api.g_Ret_Sts_Success;
    --修正SOA请求来源系统
    UPDATE Cux_Soa_Requests t
       SET t.Relative_System = p_Request.Relative_System
     WHERE t.Request_Id = p_Request.Request_Id;
    IF x_Wip_Combination.Active_Flag = 'N' THEN
      --取消合并工单
      UPDATE Cux_Wip_Combinations
         SET Active_Flag       = x_Wip_Combination.Active_Flag
            ,Cancel_Date       = x_Wip_Combination.Cancel_Date
            ,Cancel_Request_Id = x_Wip_Combination.Cancel_Request_Id
       WHERE Combination_Id = x_Wip_Combination.Combination_Id;
      FOR i IN 1 .. p_Wip_Detail_Tab.Count LOOP
        l_Wip_Detail := p_Wip_Detail_Tab(i);
        UPDATE Cux_Wip_Combination_Details t
           SET t.Active_Flag = 'N'
         WHERE t.Combination_Id = l_Wip_Detail.Combination_Id
           AND t.Wip_Entity_Name = l_Wip_Detail.Wip_Entity_Name
           AND t.Organization_Id = l_Wip_Detail.Organization_Id;
      END LOOP;
      SELECT COUNT(*)
        INTO l_Count
        FROM Cux_Wip_Combination_Details t
       WHERE t.Combination_Id = x_Wip_Combination.Combination_Id
         AND t.Active_Flag = 'Y';
      IF l_Count > 0 THEN
        x_Error_Msg := '存在未撤销合并的工单行';
        RAISE Fnd_Api.g_Exc_Error;
      END IF;
    ELSE
      --合并工单
      x_Wip_Combination.Combination_Id := Cux_Wip_Combinations_s.Nextval;
      INSERT INTO Cux_Wip_Combinations VALUES x_Wip_Combination;
      FOR i IN 1 .. p_Wip_Detail_Tab.Count LOOP
      
        l_Wip_Detail := p_Wip_Detail_Tab(i);
        --撤销虚拟合并工单
        UPDATE Cux_Wip_Combination_Details t
           SET t.Active_Flag = 'N'
         WHERE t.Organization_Id = l_Wip_Detail.Organization_Id
           AND t.Wip_Entity_Id = l_Wip_Detail.Wip_Entity_Id
           AND t.Active_Flag = 'Y'
           AND EXISTS (SELECT 1
                  FROM Cux_Wip_Combinations c
                 WHERE t.Combination_Id = c.Combination_Id
                   AND c.Combination_Request_Id = 0)
        RETURNING t.Combination_Id INTO l_Combination_Id;
        IF l_Combination_Id IS NOT NULL THEN
          UPDATE Cux_Wip_Combinations
             SET Active_Flag       = 'N'
                ,Cancel_Date       = SYSDATE
                ,Cancel_Request_Id = 0
           WHERE Combination_Id = l_Combination_Id;
        END IF;
        --写入合并工单明细      
        l_Wip_Detail.Combination_Id := x_Wip_Combination.Combination_Id;
        l_Wip_Detail.Active_Flag    := 'Y';
        SELECT COUNT(*)
          INTO l_Count
          FROM Cux_Wip_Combination_Details t
         WHERE t.Combination_Id = x_Wip_Combination.Combination_Id
           AND (t.Sequence_No = l_Wip_Detail.Sequence_No OR
               t.Wip_Entity_Name = l_Wip_Detail.Wip_Entity_Name);
        IF l_Count > 0 THEN
          x_Error_Msg := '存在合并序号或者工单号相同的合并明细。';
          RAISE Fnd_Api.g_Exc_Error;
        END IF;
        INSERT INTO Cux_Wip_Combination_Details VALUES l_Wip_Detail;
      END LOOP;
    END IF;
    COMMIT;
  EXCEPTION
    WHEN Fnd_Api.g_Exc_Error THEN
      x_Ret_Sts := Fnd_Api.g_Ret_Sts_Error;
    WHEN OTHERS THEN
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Unexp_Error;
      x_Error_Msg := SQLERRM;
  END Process_Wip_Combination;

  PROCEDURE Accept_Wip_Combination(p_Combination_Info IN CLOB,
                                   x_Response         OUT CLOB) IS
    l_Ret_Status VARCHAR2(10);
    l_Error_Msg  VARCHAR2(200);
  
    l_Request    Cux_Soa_Requests%ROWTYPE;
    l_Request_Id NUMBER;
  
    l_Parser     Dbms_Xmlparser.Parser;
    l_Doc        Dbms_Xmldom.Domdocument;
    l_Soa_Header Cux_Soa_Pub.Soa_Context_Header;
  
    l_Wip_Combination Cux_Wip_Combinations%ROWTYPE;
    l_Wip_Details_Tab Wip_Details_Tab;
  
    l_Step VARCHAR2(30);
  BEGIN
    l_Step := '000';
    --创建SOA请求
    l_Request.Direction       := 'INBOUND';
    l_Request.Interface       := 'CUX_WIP2WMS_TRANSFER_PKG';
    l_Request.Method          := 'ACCEPT_WIP_COMBINATION';
    l_Request.Relative_System := 'SPS';
    l_Request.Url             := NULL;
    l_Request.Request_Date    := SYSDATE;
    l_Request.Request_Msg     := p_Combination_Info;
    Cux_Soa_Pub.Create_Request(l_Request, l_Request_Id);
    l_Step := '010';
    SAVEPOINT Process_Accepting;
  
    --创建XML对象
    l_Parser := Dbms_Xmlparser.Newparser;
    BEGIN
      Dbms_Xmlparser.Parseclob(l_Parser, p_Combination_Info);
      l_Doc := Dbms_Xmlparser.Getdocument(l_Parser);
    EXCEPTION
      WHEN OTHERS THEN
        l_Error_Msg := '解析检验数据失败:' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    END;
    l_Step := '020';
    --获取检验数量
    Get_Wip_Combination(x_Ret_Sts         => l_Ret_Status,
                        x_Error_Msg       => l_Error_Msg,
                        p_Doc             => l_Doc,
                        x_Soa_Header      => l_Soa_Header,
                        x_Request         => l_Request,
                        x_Wip_Combination => l_Wip_Combination,
                        x_Wip_Detail_Tab  => l_Wip_Details_Tab);
    IF l_Ret_Status <> Fnd_Api.g_Ret_Sts_Success THEN
      l_Error_Msg := '获取检验数据失败:' || l_Error_Msg;
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
    l_Step := '030';
    --处理检验数据
    Process_Wip_Combination(x_Ret_Sts          => l_Ret_Status,
                            x_Error_Msg        => l_Error_Msg,
                            p_Request          => l_Request,
                            p_Soa_Header       => l_Soa_Header,
                            x_Wip_Combination  => l_Wip_Combination,
                            p_Wip_Detail_Tab   => l_Wip_Details_Tab,
                            p_Combination_Info => p_Combination_Info);
    IF l_Ret_Status <> Fnd_Api.g_Ret_Sts_Success THEN
      l_Error_Msg := '处理检验数据失败:' || l_Error_Msg;
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
  
    --回馈结果
    x_Response := Response_Wip_Combination(l_Ret_Status,
                                           l_Error_Msg,
                                           l_Request);
  
  EXCEPTION
    WHEN Fnd_Api.g_Exc_Error THEN
      l_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      ROLLBACK TO Process_Accepting;
      x_Response := Response_Wip_Combination(l_Ret_Status,
                                             l_Error_Msg,
                                             l_Request);
    WHEN OTHERS THEN
      l_Ret_Status := Fnd_Api.g_Ret_Sts_Unexp_Error;
      ROLLBACK TO Process_Accepting;
      l_Error_Msg := '执行发生异常(' || l_Step || '):' || SQLERRM;
      x_Response  := Response_Wip_Combination(l_Ret_Status,
                                              l_Error_Msg,
                                              l_Request);
  END Accept_Wip_Combination;

  --处理工单发料数据
  PROCEDURE Transfer_Wip_Issue_Data(x_Ret_Sts         OUT VARCHAR2,
                                    x_Error_Msg       OUT VARCHAR2,
                                    p_Organization_Id NUMBER,
                                    p_Doc_No          VARCHAR2,
                                    p_Full_Flag       VARCHAR2) IS
    l_Program_Name  VARCHAR2(30) := 'TRANSFER_WIP_ISSUE_DATA';
    l_Setp          VARCHAR2(30);
    l_Source_Module VARCHAR2(30) := 'WIP';
    l_Username      VARCHAR2(50);
    l_Password      VARCHAR2(50);
    CURSOR Cur_Wip_Issue_Lines IS
      SELECT Trx_Type.Attribute2 Order_Type
            ,Company.Lookup_Code Company_Code --Customer_Id
            ,Cri.Doc_No Trx_Number --Asn_Reference_1/D_EDI_01
            ,Trx_Type.Lookup_Code Trx_Type --  H_EDI_08
            ,Trx_Type.Description Trx_Type_Name --  Asn_Reference_2
            ,Trx_Type.Attribute3 Kit_Flag --issue_flag
            ,Msi.Attribute7 Wms_Subinventory_Code --Warehouseid
            ,Msi.Attribute6 Wms_System
            ,Msi.Secondary_Inventory_Name --Ckdm LotAtt04
            ,Msib.Segment1 Item_Number --Sku
            ,Cri.Ready_Qty Primary_Quantity --Expected_Qty
            ,Cri.Contract_Number --Lot_Att_08
            ,Cri.Line_Number --d_Edi_02
            ,Ood.Operating_Unit
            ,Cri.Creation_Date
            ,Cri.Now_Qty --added by bruce on 20151223
            ,Assembly.Segment1 Assembly_Item
            ,Decode(Nvl(Cwc.Combination_Request_Id, 0),
                    0,
                    NULL,
                    Cwc.Combination_Number) Combination_Number
            ,Wdj.Attribute3 Workshop
            ,Cri.Wip_Entity_Name
            ,Cri.Operation_Code
            ,Decode(Nvl(Cwc.Combination_Request_Id, 0),
                    0,
                    NULL,
                    Cwc.Entity_Count) Entity_Count
            ,Decode(Nvl(Cwc.Combination_Request_Id, 0),
                    0,
                    NULL,
                    Cwcd.Sequence_No) Sequence_No
            ,Wdj.Class_Code
            ,Cri.Wip_Entity_Id
            ,Wdj.Attribute5 Main_Board_Job
        FROM Cux_Ready_Item               Cri
            ,Org_Organization_Definitions Ood
            ,Fnd_Lookup_Values_Vl         Company
            ,Fnd_Lookup_Values_Vl         Trx_Type
            ,Mtl_Secondary_Inventories    Msi
            ,Mtl_System_Items_b           Msib
            ,Wip_Discrete_Jobs            Wdj
            ,Mtl_System_Items_b           Assembly
            ,Cux_Wip_Combination_Details  Cwcd
            ,Cux_Wip_Combinations         Cwc
       WHERE Cri.Doc_Status = '未过账'
         AND Cri.Transfer_Status IN ('WAITING', 'TRANSFERRING')
         AND Cri.Organization_Id = Ood.Organization_Id
         AND 'CUX_LEGAL_ENTITY' = Company.Lookup_Type
         AND Ood.Operating_Unit = Company.Meaning
         AND 'CUX_INV_TRX_TYPE' = Trx_Type.Lookup_Type
         AND Cri.Doc_Type = Trx_Type.Meaning
         AND l_Source_Module = Trx_Type.Tag
         AND Trx_Type.Attribute1 = 'O'
         AND Cri.Organization_Id = Msi.Organization_Id
         AND Cri.Supply_Subinventory = Msi.Secondary_Inventory_Name
         AND Cri.Organization_Id = Msib.Organization_Id
         AND Cri.Inventory_Item_Id = Msib.Inventory_Item_Id
         AND Cri.Wip_Entity_Id = Wdj.Wip_Entity_Id
         AND Cri.Organization_Id = Wdj.Organization_Id
         AND Wdj.Organization_Id = Assembly.Organization_Id
         AND Wdj.Primary_Item_Id = Assembly.Inventory_Item_Id
         AND Cri.Wip_Entity_Name = Cwcd.Wip_Entity_Name(+)
         AND 'Y' = Cwcd.Active_Flag(+)
         AND Cri.Organization_Id = Cwcd.Organization_Id(+)
         AND Cwcd.Combination_Id = Cwc.Combination_Id(+)
         AND Cri.Ready_Qty > 0
         AND Cri.Organization_Id = p_Organization_Id
         AND Cri.Doc_No = p_Doc_No;
    --锁定单据
    CURSOR Cur_Lock_Issue_Lines IS
      SELECT Cri.Doc_Status
            ,Cri.Organization_Id
            ,Cri.Doc_No
            ,Cri.Transfer_Status
            ,Cri.Supply_Subinventory
            ,Cri.Operation_Code
            ,Cri.Wip_Entity_Name
            ,Cri.Wip_Entity_Id
        FROM Cux_Ready_Item Cri
       WHERE Cri.Doc_Status = '未过账'
         AND Cri.Transfer_Status IN
             ('WAITING', 'TRANSFERRING', 'NO_TRANSFER')
         AND Cri.Organization_Id = p_Organization_Id
         AND Cri.Doc_No = p_Doc_No
         FOR UPDATE NOWAIT;
    --锁定齐套  
    CURSOR Cur_Kit_Doc(p_Combination_Id      NUMBER,
                       p_Operation_Code      VARCHAR2,
                       p_Supply_Subinventory VARCHAR2) IS
      SELECT t.Full_Prepare_Id
        FROM Cux_Wip_Full_Prepare t
       WHERE t.Combination_Id = p_Combination_Id
         AND t.Operation_Code = p_Operation_Code
         AND t.Supply_Subinventory = p_Supply_Subinventory
         FOR UPDATE;
  
    l_Lock_Issue_Lines Cur_Lock_Issue_Lines%ROWTYPE;
    l_Line_Count       NUMBER;
    l_Combination_Id   NUMBER;
    l_Kit_Doc          Cur_Kit_Doc%ROWTYPE;
    l_Kit_Flag         VARCHAR2(1);
    l_Doc_Count        NUMBER; --未传送单据数量
  
    l_Trx_Info      CLOB;
    l_Unique_Trx_No VARCHAR2(30);
  
    l_Destination_System VARCHAR2(30);
    l_Trx_Number         VARCHAR2(30);
    l_Mes_Job_Number     VARCHAR2(30);
    l_Ret_Status         VARCHAR2(10);
    l_Error_Msg          VARCHAR2(200);
    l_Mes_Ret_Status     VARCHAR2(10);
    l_Mes_Error_Msg      VARCHAR2(200);
  BEGIN
    x_Ret_Sts := Fnd_Api.g_Ret_Sts_Success;
  
    --传送MES合并单信息
    Transfer_Combination_2_Mes(x_Ret_Status      => l_Mes_Ret_Status,
                               x_Error_Msg       => l_Mes_Error_Msg,
                               p_Organization_Id => p_Organization_Id,
                               p_Doc_No          => p_Doc_No,
                               p_Full_Flag       => p_Full_Flag);
    IF l_Mes_Ret_Status <> Fnd_Api.g_Ret_Sts_Success THEN
      x_Error_Msg := l_Mes_Error_Msg;
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
  
    --状态更新为抛转中
    BEGIN
      OPEN Cur_Lock_Issue_Lines;
      FETCH Cur_Lock_Issue_Lines
        INTO l_Lock_Issue_Lines;
      CLOSE Cur_Lock_Issue_Lines;
    EXCEPTION
      WHEN OTHERS THEN
        IF Cur_Lock_Issue_Lines%ISOPEN THEN
          CLOSE Cur_Lock_Issue_Lines;
        END IF;
        x_Error_Msg := '单据被其它程序使用。';
        RAISE Fnd_Api.g_Exc_Error;
    END;
    IF l_Lock_Issue_Lines.Transfer_Status = 'NO_TRANSFER' THEN
      ROLLBACK;
      RETURN;
    END IF;
  
    --取得合并工单ID
    BEGIN
      SELECT t.Combination_Id
        INTO l_Combination_Id
        FROM Cux_Wip_Combination_Details t
       WHERE t.Wip_Entity_Name = l_Lock_Issue_Lines.Wip_Entity_Name
         AND t.Organization_Id = l_Lock_Issue_Lines.Organization_Id
         AND t.Active_Flag = 'Y';
    EXCEPTION
      WHEN OTHERS THEN
        x_Error_Msg := '未找到合并单信息。';
        RAISE Fnd_Api.g_Exc_Error;
    END;
  
    --锁定齐套信息
    BEGIN
      OPEN Cur_Kit_Doc(l_Combination_Id,
                       l_Lock_Issue_Lines.Operation_Code,
                       l_Lock_Issue_Lines.Supply_Subinventory);
      FETCH Cur_Kit_Doc
        INTO l_Kit_Doc;
      CLOSE Cur_Kit_Doc;
      IF l_Kit_Doc.Full_Prepare_Id IS NULL THEN
        l_Kit_Doc.Full_Prepare_Id := 0;
      END IF;
    EXCEPTION
      WHEN OTHERS THEN
        IF Cur_Kit_Doc%ISOPEN THEN
          CLOSE Cur_Kit_Doc;
        END IF;
        x_Error_Msg := '合并工单齐套信息错误。';
        RAISE Fnd_Api.g_Exc_Error;
    END;
  
    IF l_Lock_Issue_Lines.Doc_No IS NULL THEN
      ROLLBACK;
      RETURN;
    ELSIF l_Lock_Issue_Lines.Transfer_Status = 'WAITING' THEN
      UPDATE Cux_Ready_Item t
         SET t.Transfer_Status = 'TRANSFERRING'
       WHERE t.Organization_Id = p_Organization_Id
         AND t.Doc_No = p_Doc_No;
    END IF;
    l_Line_Count := 0;
    --获取传送信息
    FOR Rec_Trx_Line IN Cur_Wip_Issue_Lines LOOP
      l_Line_Count := l_Line_Count + 1;
      IF Cur_Wip_Issue_Lines%ROWCOUNT = 1 THEN
        l_Destination_System := Rec_Trx_Line.Wms_System;
        l_Trx_Number         := Rec_Trx_Line.Trx_Number;
        --如果是第一行，构建XML单据头信息
        Cux_Soa_Pub.Get_Service_Account(p_Service_System => l_Destination_System,
                                        x_Username       => l_Username,
                                        x_Password       => l_Password);
        l_Trx_Info       := l_Trx_Info ||
                            '<?xml version="1.0" encoding="utf-8"?>';
        l_Trx_Info       := l_Trx_Info || '<STD_IN Origin="WMS">';
        l_Trx_Info       := l_Trx_Info || '<Tiptop_user>' || l_Username ||
                            '</Tiptop_user>';
        l_Trx_Info       := l_Trx_Info || '<Tiptop_pwd>' || l_Password ||
                            '</Tiptop_pwd>';
        l_Trx_Info       := l_Trx_Info ||
                            '<ServiceName >ISHIPMENTSERVICE</ServiceName>';
        l_Trx_Info       := l_Trx_Info || '<Factory>' ||
                            Rec_Trx_Line.Company_Code || '</Factory >'; --法人
        l_Trx_Info       := l_Trx_Info || '<Source>ERP</Source>';
        l_Trx_Info       := l_Trx_Info ||
                            '<ObjectID>SMTN_stockout_erp</ObjectID>';
        l_Trx_Info       := l_Trx_Info || '<DATA>';
        l_Trx_Info       := l_Trx_Info || '<soInfo>';
        l_Trx_Info       := l_Trx_Info || '<wmsSOHeaders>';
        l_Trx_Info       := l_Trx_Info || '<OrderType>' ||
                            Rec_Trx_Line.Order_Type || '</OrderType>'; --订单类型
        l_Trx_Info       := l_Trx_Info || '<OrderTime></OrderTime>';
        l_Trx_Info       := l_Trx_Info ||
                            '<ExpectedShipmentTime1></ExpectedShipmentTime1>';
        l_Trx_Info       := l_Trx_Info || '<RequiredDeliveryTime>' ||
                            To_Char(Rec_Trx_Line.Creation_Date,
                                    'YYYY/MM/DD HH24:MI:SS') ||
                            '</RequiredDeliveryTime>'; --ERP的交货日期
        l_Trx_Info       := l_Trx_Info || '<CustomerID>' ||
                            Rec_Trx_Line.Company_Code || '1</CustomerID>'; --货主
        l_Trx_Info       := l_Trx_Info || '<Priority></Priority>';
        l_Trx_Info       := l_Trx_Info || '<SOReference1>' ||
                            Rec_Trx_Line.Trx_Number || '</SOReference1>'; --ERP单号
        l_Trx_Info       := l_Trx_Info || '<SOReference2>' ||
                            Rec_Trx_Line.Trx_Type_Name || '</SOReference2>'; --单别名称
        l_Trx_Info       := l_Trx_Info || '<SOReference3></SOReference3>';
        l_Trx_Info       := l_Trx_Info || '<SOReference4></SOReference4>';
        l_Trx_Info       := l_Trx_Info || '<SOReference5></SOReference5>';
        l_Trx_Info       := l_Trx_Info || '<ConsigneeID></ConsigneeID>';
        l_Trx_Info       := l_Trx_Info || '<ConsigneeName></ConsigneeName>';
        l_Trx_Info       := l_Trx_Info || '<C_Address1></C_Address1>';
        l_Trx_Info       := l_Trx_Info || '<C_Address2></C_Address2>';
        l_Trx_Info       := l_Trx_Info || '<C_Address3></C_Address3>';
        l_Trx_Info       := l_Trx_Info || '<C_Contact></C_Contact>';
        l_Trx_Info       := l_Trx_Info || '<C_Mail></C_Mail>';
        l_Trx_Info       := l_Trx_Info || '<C_Fax></C_Fax>';
        l_Trx_Info       := l_Trx_Info || '<C_Tel1></C_Tel1>';
        l_Trx_Info       := l_Trx_Info || '<C_Tel2></C_Tel2>';
        l_Trx_Info       := l_Trx_Info || '<UserDefine1></UserDefine1>';
        l_Trx_Info       := l_Trx_Info || '<UserDefine2></UserDefine2>';
        l_Trx_Info       := l_Trx_Info || '<UserDefine3></UserDefine3>';
        l_Trx_Info       := l_Trx_Info || '<UserDefine4></UserDefine4>';
        l_Trx_Info       := l_Trx_Info || '<UserDefine5></UserDefine5>';
        l_Trx_Info       := l_Trx_Info || '<UserDefine6></UserDefine6>';
        l_Trx_Info       := l_Trx_Info || '<Notes></Notes>';
        l_Trx_Info       := l_Trx_Info || '<H_EDI_01>' ||
                            Rec_Trx_Line.Assembly_Item || '</H_EDI_01>'; --ERP:备料时有对应的成品料号
        l_Trx_Info       := l_Trx_Info || '<H_EDI_02>' ||
                            Rec_Trx_Line.Combination_Number ||
                            '</H_EDI_02>'; --ERP:合并编号
        l_Trx_Info       := l_Trx_Info || '<H_EDI_03>' ||
                            Rec_Trx_Line.Wip_Entity_Name || '</H_EDI_03>'; --ERP:传票号
        l_Mes_Job_Number := Get_Mes_Job_Number(Rec_Trx_Line.Wip_Entity_Id);
        l_Trx_Info       := l_Trx_Info || '<H_EDI_04>' || l_Mes_Job_Number ||
                            '</H_EDI_04>'; --ERP:工单号
        l_Trx_Info       := l_Trx_Info || '<H_EDI_05>' ||
                            Rec_Trx_Line.Operation_Code || '</H_EDI_05>'; --ERP:工序
        l_Trx_Info       := l_Trx_Info || '<H_EDI_06>' ||
                            Rec_Trx_Line.Workshop || '</H_EDI_06>'; --ERP:车间
        --ERP作业编号 H_EDI_07  
        l_Unique_Trx_No := Cux_Wms_Pub.Generate_Unique_Trx_No(p_Org_Id           => Rec_Trx_Line.Operating_Unit,
                                                              p_Trx_Order_Type   => Rec_Trx_Line.Trx_Type,
                                                              p_Trx_Order_Number => Rec_Trx_Line.Trx_Number,
                                                              p_Prog_Appl_Id     => Fnd_Global.Resp_Appl_Id,
                                                              p_Conc_Program_Id  => Fnd_Global.Conc_Program_Id,
                                                              p_Conc_Request_Id  => Fnd_Global.Conc_Request_Id);
        l_Trx_Info      := l_Trx_Info || '<H_EDI_07>' || l_Unique_Trx_No ||
                           '</H_EDI_07>'; --ERP作业ID
        l_Trx_Info      := l_Trx_Info || '<H_EDI_08>' ||
                           Rec_Trx_Line.Trx_Type || '</H_EDI_08>'; --单别代码 
        l_Trx_Info      := l_Trx_Info || '<H_EDI_09>' ||
                           Rec_Trx_Line.Entity_Count || '</H_EDI_09>'; --ERP:并单数
        l_Trx_Info      := l_Trx_Info || '<H_EDI_10></H_EDI_10>';
        l_Trx_Info      := l_Trx_Info || '<WAREHOUSEID>' ||
                           Rec_Trx_Line.Wms_Subinventory_Code ||
                           '</WAREHOUSEID>'; --来源业务仓库    
        l_Trx_Info      := l_Trx_Info || '<Soqty>' || Rec_Trx_Line.Now_Qty ||
                           '</Soqty>'; --任务套数--modified by bruce on 20151223
        --取得齐套标记
        IF l_Kit_Doc.Full_Prepare_Id = 0 OR
           Nvl(Rec_Trx_Line.Kit_Flag, 'N') <> 'Y' THEN
          l_Kit_Flag := 'N';
        ELSE
          SELECT COUNT(*)
            INTO l_Doc_Count
            FROM Cux_Ready_Item              t
                ,Cux_Wip_Combination_Details d
                ,Fnd_Lookup_Values_Vl        Trx_Type
           WHERE t.Organization_Id = d.Organization_Id
             AND d.Wip_Entity_Id = t.Wip_Entity_Id
             AND t.Doc_Status = '未过账'
             AND t.Transfer_Status IN ('WAITING', 'TRANSFERRING')
             AND 'CUX_INV_TRX_TYPE' = Trx_Type.Lookup_Type
             AND t.Doc_Type = Trx_Type.Meaning
             AND l_Source_Module = Trx_Type.Tag
             AND Trx_Type.Attribute1 = 'O'
             AND Trx_Type.Attribute3 = 'Y'
             AND d.Combination_Id = l_Combination_Id
             AND t.Doc_No <> p_Doc_No;
          IF l_Doc_Count = 0 THEN
            l_Kit_Flag := 'Y';
          ELSE
            l_Kit_Flag := 'N';
          END IF;
        END IF;
      
        l_Trx_Info := l_Trx_Info || '<ZDQTBZ>' || l_Kit_Flag || '</ZDQTBZ>'; --整单齐套标志
        l_Trx_Info := l_Trx_Info || '<CKDM>' ||
                      Rec_Trx_Line.Secondary_Inventory_Name || '</CKDM>'; --ERP仓库代码
        l_Trx_Info := l_Trx_Info || '<HBSX>' || Rec_Trx_Line.Sequence_No ||
                      '</HBSX>'; --合并顺序
        l_Trx_Info := l_Trx_Info || '<GDLX>' || Rec_Trx_Line.Class_Code ||
                      '</GDLX>'; --工单类型
        l_Trx_Info := l_Trx_Info || '<Head_Property1>' ||
                      Rec_Trx_Line.Main_Board_Job || '</Head_Property1>'; --大板工单
        l_Trx_Info := l_Trx_Info || '<Head_Property2></Head_Property2>'; --预留属性2
        l_Trx_Info := l_Trx_Info || '<Head_Property3></Head_Property3>'; --预留属性3
        l_Trx_Info := l_Trx_Info || '<Head_Property4></Head_Property4>'; --预留属性4
        l_Trx_Info := l_Trx_Info || '<Head_Property5></Head_Property5>'; --预留属性5
        l_Trx_Info := l_Trx_Info || '<Head_Property6></Head_Property6>'; --预留属性6
        l_Trx_Info := l_Trx_Info || '<Head_Property7></Head_Property7>'; --预留属性7
        l_Trx_Info := l_Trx_Info || '<Head_Property8></Head_Property8>'; --预留属性8
        l_Trx_Info := l_Trx_Info || '<Head_Property9></Head_Property9>'; --预留属性9
      
      END IF;
      --明细信息
      l_Trx_Info := l_Trx_Info || '<detailsItem>';
      l_Trx_Info := l_Trx_Info || '<CustomerID>' ||
                    Rec_Trx_Line.Company_Code || '1</CustomerID>'; --货主（产品线代码）
      l_Trx_Info := l_Trx_Info || '<SKU>' || Rec_Trx_Line.Item_Number ||
                    '</SKU>'; --产品代码
      l_Trx_Info := l_Trx_Info || '<SKUDescrC></SKUDescrC>';
      l_Trx_Info := l_Trx_Info || '<SKUDescrE></SKUDescrE>';
      l_Trx_Info := l_Trx_Info || '<LotAtt01></LotAtt01>';
      l_Trx_Info := l_Trx_Info || '<LotAtt02></LotAtt02>';
      l_Trx_Info := l_Trx_Info || '<LotAtt03></LotAtt03>';
      l_Trx_Info := l_Trx_Info || '<LotAtt04>' ||
                    Rec_Trx_Line.Secondary_Inventory_Name || '</LotAtt04>'; --ERP仓库号
      l_Trx_Info := l_Trx_Info || '<LotAtt05></LotAtt05>';
      l_Trx_Info := l_Trx_Info || '<LotAtt06></LotAtt06>';
      l_Trx_Info := l_Trx_Info || '<LotAtt07></LotAtt07>'; --ERP:原料，特制单号
      l_Trx_Info := l_Trx_Info || '<LotAtt08>' ||
                    Rec_Trx_Line.Contract_Number || '</LotAtt08>'; --属性号，只有成品和半成品用到
      l_Trx_Info := l_Trx_Info || '<LotAtt09></LotAtt09>';
      l_Trx_Info := l_Trx_Info || '<LotAtt10></LotAtt10>';
      l_Trx_Info := l_Trx_Info || '<LotAtt11></LotAtt11>';
      l_Trx_Info := l_Trx_Info || '<LotAtt12></LotAtt12>';
      l_Trx_Info := l_Trx_Info || '<QtyOrdered>' ||
                    To_Char(Rec_Trx_Line.Primary_Quantity) ||
                    '</QtyOrdered>'; --总数量
      l_Trx_Info := l_Trx_Info || '<UserDefine1></UserDefine1>';
      l_Trx_Info := l_Trx_Info || '<UserDefine2></UserDefine2>';
      l_Trx_Info := l_Trx_Info || '<UserDefine3></UserDefine3>';
      l_Trx_Info := l_Trx_Info || '<UserDefine4></UserDefine4>';
      l_Trx_Info := l_Trx_Info || '<UserDefine5></UserDefine5>';
      l_Trx_Info := l_Trx_Info || '<UserDefine6></UserDefine6>';
      l_Trx_Info := l_Trx_Info || '<Notes></Notes>';
      l_Trx_Info := l_Trx_Info || '<Price></Price>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_01>' || Rec_Trx_Line.Trx_Number ||
                    '</D_EDI_01>'; --ERP单号
      l_Trx_Info := l_Trx_Info || '<D_EDI_02>' ||
                    To_Char(Rec_Trx_Line.Line_Number) || '</D_EDI_02>'; --ERP行号
      l_Trx_Info := l_Trx_Info || '<D_EDI_03></D_EDI_03>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_04></D_EDI_04>'; --ERP:是否烧片
      l_Trx_Info := l_Trx_Info || '<D_EDI_05></D_EDI_05>'; --ERP:特制数量
      l_Trx_Info := l_Trx_Info || '<D_EDI_06></D_EDI_06>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_07></D_EDI_07>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_08></D_EDI_08>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_09></D_EDI_09>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_10></D_EDI_10>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_11></D_EDI_11>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_12></D_EDI_12>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_13></D_EDI_13>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_14></D_EDI_14>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_15></D_EDI_15>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_16></D_EDI_16>';
      l_Trx_Info := l_Trx_Info || '<Detail_Property1></Detail_Property1>'; --预留属性1
      l_Trx_Info := l_Trx_Info || '<Detail_Property2></Detail_Property2>'; --预留属性2
      l_Trx_Info := l_Trx_Info || '<Detail_Property3></Detail_Property3>'; --预留属性3
      l_Trx_Info := l_Trx_Info || '<Detail_Property4></Detail_Property4>'; --预留属性4
      l_Trx_Info := l_Trx_Info || '<Detail_Property5></Detail_Property5>'; --预留属性5
      l_Trx_Info := l_Trx_Info || '<Detail_Property6></Detail_Property6>'; --预留属性6
      l_Trx_Info := l_Trx_Info || '<Detail_Property7></Detail_Property7>'; --预留属性7
      l_Trx_Info := l_Trx_Info || '<Detail_Property8></Detail_Property8>'; --预留属性8
      l_Trx_Info := l_Trx_Info || '<Detail_Property9></Detail_Property9>'; --预留属性9
      l_Trx_Info := l_Trx_Info || '</detailsItem>';
    END LOOP;
    IF l_Line_Count = 0 THEN
      RETURN;
    END IF;
    --补全XML文档
    l_Trx_Info := l_Trx_Info || '</wmsSOHeaders>';
    l_Trx_Info := l_Trx_Info || '</soInfo>';
    l_Trx_Info := l_Trx_Info || '</DATA>';
    l_Trx_Info := l_Trx_Info || '</STD_IN>';
    IF l_Destination_System IS NOT NULL THEN
      --如果有需要发布的数据
      Cux_Wms_Pub.Transfer_Service(x_Ret_Status         => l_Ret_Status,
                                   x_Error_Msg          => l_Error_Msg,
                                   p_Destination_System => l_Destination_System,
                                   p_Entity_Type        => l_Source_Module ||
                                                           '_TRX',
                                   p_Operation_Code     => 'SMTN_stockout_erp',
                                   p_Trx_Number         => l_Trx_Number,
                                   p_Interface          => g_Package_Name,
                                   p_Method             => l_Program_Name,
                                   p_Trx_Info           => l_Trx_Info);
    END IF;
    --更新抛转完成状态
    IF l_Ret_Status = Fnd_Api.g_Ret_Sts_Success OR
       l_Destination_System IS NULL THEN
      UPDATE Cux_Ready_Item t
         SET t.Transfer_Status = 'TRANSFERRED'
       WHERE t.Organization_Id = p_Organization_Id
         AND t.Doc_No = p_Doc_No;
      --记录齐套单据
      IF l_Kit_Flag = 'Y' THEN
        UPDATE Cux_Wip_Full_Prepare t
           SET t.Latest_Prepared_Doc = p_Doc_No
         WHERE t.Full_Prepare_Id = l_Kit_Doc.Full_Prepare_Id;
      END IF;
    ELSE
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg := l_Error_Msg;
    END IF;
    COMMIT;
  EXCEPTION
    WHEN Fnd_Api.g_Exc_Error THEN
      ROLLBACK;
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg := l_Program_Name || '@' || l_Setp || ':' || x_Error_Msg;
    WHEN OTHERS THEN
      ROLLBACK;
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Unexp_Error;
      x_Error_Msg := l_Program_Name || '@' || l_Setp || ':' || SQLERRM;
  END Transfer_Wip_Issue_Data;
  --处理工单推式还料数据
  PROCEDURE Transfer_Wip_Return_Data(x_Ret_Sts         OUT VARCHAR2,
                                     x_Error_Msg       OUT VARCHAR2,
                                     p_Organization_Id NUMBER,
                                     p_Doc_No          VARCHAR2) IS
    l_Program_Name  VARCHAR2(30) := 'TRANSFER_WIP_RETURN_DATA';
    l_Setp          VARCHAR2(30);
    l_Source_Module VARCHAR2(30) := 'WIP';
    l_Username      VARCHAR2(50);
    l_Password      VARCHAR2(50);
    CURSOR Cur_Trx_Lines IS
      SELECT Trx_Type.Attribute2 Order_Type
            ,Company.Lookup_Code Company_Code --Customer_Id
            ,Cri.Doc_No Trx_Number --Asn_Reference_1/D_EDI_01
            ,Trx_Type.Lookup_Code Trx_Type --  H_EDI_08
            ,Trx_Type.Description Trx_Type_Name --  Asn_Reference_2
            ,Msi.Attribute7 Wms_Subinventory_Code --Warehouseid
            ,Msi.Attribute6 Wms_System
            ,Msi.Secondary_Inventory_Name --Ckdm LotAtt04
            ,Msib.Segment1 Item_Number --Sku
            ,Cri.Ready_Qty Primary_Quantity --Expected_Qty
            ,Cri.Contract_Number --Lot_Att_08
            ,Cri.Line_Number --d_Edi_02
            ,Ood.Operating_Unit
            ,Cri.Creation_Date
            ,Assembly.Segment1 Assembly_Item
            ,Decode(Nvl(Cwc.Combination_Request_Id, 0),
                    0,
                    NULL,
                    Cwc.Combination_Number) Combination_Number
            ,Wdj.Attribute3 Workshop
            ,Cri.Wip_Entity_Name
            ,Cri.Operation_Code
            ,Decode(Nvl(Cwc.Combination_Request_Id, 0),
                    0,
                    NULL,
                    Cwc.Entity_Count) Entity_Count
            ,Decode(Nvl(Cwc.Combination_Request_Id, 0),
                    0,
                    NULL,
                    Cwcd.Sequence_No) Sequence_No
        FROM Cux_Ready_Item               Cri
            ,Org_Organization_Definitions Ood
            ,Fnd_Lookup_Values_Vl         Company
            ,Fnd_Lookup_Values_Vl         Trx_Type
            ,Mtl_Secondary_Inventories    Msi
            ,Mtl_System_Items_b           Msib
            ,Wip_Discrete_Jobs            Wdj
            ,Mtl_System_Items_b           Assembly
            ,Cux_Wip_Combination_Details  Cwcd
            ,Cux_Wip_Combinations         Cwc
       WHERE Cri.Doc_Status = '未过账'
         AND Cri.Transfer_Status IN ('WAITING', 'TRANSFERRING')
         AND Cri.Organization_Id = Ood.Organization_Id
         AND 'CUX_LEGAL_ENTITY' = Company.Lookup_Type
         AND Ood.Operating_Unit = Company.Meaning
         AND 'CUX_INV_TRX_TYPE' = Trx_Type.Lookup_Type
         AND Cri.Doc_Type = Trx_Type.Meaning
         AND l_Source_Module = Trx_Type.Tag
         AND Trx_Type.Attribute1 = 'I'
         AND Cri.Organization_Id = Msi.Organization_Id
         AND Cri.Supply_Subinventory = Msi.Secondary_Inventory_Name
         AND Cri.Organization_Id = Msib.Organization_Id
         AND Cri.Inventory_Item_Id = Msib.Inventory_Item_Id
         AND Cri.Wip_Entity_Id = Wdj.Wip_Entity_Id
         AND Cri.Organization_Id = Wdj.Organization_Id
         AND Wdj.Organization_Id = Assembly.Organization_Id
         AND Wdj.Primary_Item_Id = Assembly.Inventory_Item_Id
         AND Cri.Wip_Entity_Name = Cwcd.Wip_Entity_Name(+)
         AND 'Y' = Cwcd.Active_Flag(+)
         AND Cri.Organization_Id = Cwcd.Organization_Id(+)
         AND Cwcd.Combination_Id = Cwc.Combination_Id(+)
         AND Cri.Ready_Qty > 0
         AND Cri.Organization_Id = p_Organization_Id
         AND Cri.Doc_No = p_Doc_No;
  
    CURSOR Cur_Lock_Return_Lines IS
      SELECT Cri.Doc_Status
            ,Cri.Organization_Id
            ,Cri.Doc_No
            ,Cri.Transfer_Status
        FROM Cux_Ready_Item Cri
       WHERE Cri.Doc_Status = '未过账'
         AND Cri.Transfer_Status IN ('WAITING', 'TRANSFERRING')
         AND Cri.Organization_Id = p_Organization_Id
         AND Cri.Doc_No = p_Doc_No
         FOR UPDATE NOWAIT;
    l_Lock_Return_Lines Cur_Lock_Return_Lines%ROWTYPE;
    l_Line_Count        NUMBER;
  
    l_Trx_Info      CLOB;
    l_Unique_Trx_No VARCHAR2(30);
  
    l_Destination_System VARCHAR2(30);
    l_Trx_Number         VARCHAR2(30);
    l_Ret_Status         VARCHAR2(10);
    l_Error_Msg          VARCHAR2(200);
  BEGIN
    x_Ret_Sts := Fnd_Api.g_Ret_Sts_Success;
    --状态更新为抛转中
    BEGIN
      OPEN Cur_Lock_Return_Lines;
      FETCH Cur_Lock_Return_Lines
        INTO l_Lock_Return_Lines;
      CLOSE Cur_Lock_Return_Lines;
    EXCEPTION
      WHEN OTHERS THEN
        IF Cur_Lock_Return_Lines%ISOPEN THEN
          CLOSE Cur_Lock_Return_Lines;
        END IF;
        x_Error_Msg := '单据被其它程序使用。';
        RAISE Fnd_Api.g_Exc_Error;
    END;
  
    IF l_Lock_Return_Lines.Doc_No IS NULL THEN
      ROLLBACK;
      RETURN;
    ELSIF l_Lock_Return_Lines.Transfer_Status = 'WAITING' THEN
      UPDATE Cux_Ready_Item t
         SET t.Transfer_Status = 'TRANSFERRING'
       WHERE t.Organization_Id = p_Organization_Id
         AND t.Doc_No = p_Doc_No;
    END IF;
    l_Line_Count := 0;
    --获取传送信息
    FOR Rec_Trx_Line IN Cur_Trx_Lines LOOP
      l_Line_Count := l_Line_Count + 1;
      IF Cur_Trx_Lines%ROWCOUNT = 1 THEN
        l_Destination_System := Rec_Trx_Line.Wms_System;
        l_Trx_Number         := Rec_Trx_Line.Trx_Number;
        --如果是第一行，构建XML单据头信息
        Cux_Soa_Pub.Get_Service_Account(p_Service_System => l_Destination_System,
                                        x_Username       => l_Username,
                                        x_Password       => l_Password);
        l_Trx_Info := '<?xml version="1.0" encoding="utf-8"?>';
        l_Trx_Info := l_Trx_Info || '<STD_IN Origin="WMS">';
        l_Trx_Info := l_Trx_Info || '<Tiptop_user>' || l_Username ||
                      '</Tiptop_user>';
        l_Trx_Info := l_Trx_Info || '<Tiptop_pwd>' || l_Password ||
                      '</Tiptop_pwd>';
        l_Trx_Info := l_Trx_Info || '<ServiceName>IStockIN</ServiceName>';
        l_Trx_Info := l_Trx_Info || '<Factory>' ||
                      Rec_Trx_Line.Company_Code || '</Factory >'; --法人
        l_Trx_Info := l_Trx_Info || '<Source>ERP</Source>';
        l_Trx_Info := l_Trx_Info ||
                      '<ObjectID>SMTN_stockin_normal</ObjectID>';
        l_Trx_Info := l_Trx_Info || '<DATA>';
        l_Trx_Info := l_Trx_Info || '<asnInfo>';
        l_Trx_Info := l_Trx_Info || '<wmsASNHeaders>';
        l_Trx_Info := l_Trx_Info || '<ASNType>' || Rec_Trx_Line.Order_Type ||
                      '</ASNType>'; --预期到货通知类型
        l_Trx_Info := l_Trx_Info || '<CustomerID>' ||
                      Rec_Trx_Line.Company_Code || '1</CustomerID>'; --货主ID
        l_Trx_Info := l_Trx_Info || '<ASNCreationTime></ASNCreationTime>';
        l_Trx_Info := l_Trx_Info ||
                      '<ExpectedArriveTime1></ExpectedArriveTime1>';
        l_Trx_Info := l_Trx_Info || '<ASNReference1>' ||
                      Rec_Trx_Line.Trx_Number || '</ASNReference1>'; --ERP单号，里已包含单别代码
        l_Trx_Info := l_Trx_Info || '<ASNReference2>' ||
                      Rec_Trx_Line.Trx_Type_Name || '</ASNReference2>'; --单别名称
        l_Trx_Info := l_Trx_Info || '<ASNReference3></ASNReference3>';
        l_Trx_Info := l_Trx_Info || '<ASNReference4></ASNReference4>';
        l_Trx_Info := l_Trx_Info || '<ASNReference5></ASNReference5>';
        l_Trx_Info := l_Trx_Info || '<UserDefine1></UserDefine1>';
        l_Trx_Info := l_Trx_Info || '<UserDefine2></UserDefine2>';
        l_Trx_Info := l_Trx_Info || '<UserDefine3></UserDefine3>';
        l_Trx_Info := l_Trx_Info || '<UserDefine4></UserDefine4>';
        l_Trx_Info := l_Trx_Info || '<UserDefine5></UserDefine5>';
        l_Trx_Info := l_Trx_Info || '<UserDefine6></UserDefine6>';
        l_Trx_Info := l_Trx_Info || '<UserDefine7></UserDefine7>';
        l_Trx_Info := l_Trx_Info || '<UserDefine8></UserDefine8>';
        l_Trx_Info := l_Trx_Info || '<Notes></Notes>';
        l_Trx_Info := l_Trx_Info || '<SupplierID></SupplierID>';
        l_Trx_Info := l_Trx_Info || '<Supplier_Name></Supplier_Name>';
        l_Trx_Info := l_Trx_Info || '<H_EDI_01></H_EDI_01>';
        l_Trx_Info := l_Trx_Info || '<H_EDI_02></H_EDI_02>';
        l_Trx_Info := l_Trx_Info || '<H_EDI_03></H_EDI_03>';
        l_Trx_Info := l_Trx_Info || '<H_EDI_04></H_EDI_04>';
        l_Trx_Info := l_Trx_Info || '<H_EDI_05></H_EDI_05>';
        l_Trx_Info := l_Trx_Info || '<H_EDI_06></H_EDI_06>';
        --ERP作业编号 H_EDI_07  
        l_Unique_Trx_No := Cux_Wms_Pub.Generate_Unique_Trx_No(p_Org_Id           => Rec_Trx_Line.Operating_Unit,
                                                              p_Trx_Order_Type   => Rec_Trx_Line.Trx_Type,
                                                              p_Trx_Order_Number => Rec_Trx_Line.Trx_Number,
                                                              p_Prog_Appl_Id     => Fnd_Global.Resp_Appl_Id,
                                                              p_Conc_Program_Id  => Fnd_Global.Conc_Program_Id,
                                                              p_Conc_Request_Id  => Fnd_Global.Conc_Request_Id);
        l_Trx_Info      := l_Trx_Info || '<H_EDI_07>' || l_Unique_Trx_No ||
                           '</H_EDI_07>'; --ERP作业ID
        l_Trx_Info      := l_Trx_Info || '<H_EDI_08>' ||
                           Rec_Trx_Line.Trx_Type || '</H_EDI_08>'; --单别代码 
        l_Trx_Info      := l_Trx_Info || '<H_EDI_09></H_EDI_09>';
        l_Trx_Info      := l_Trx_Info || '<H_EDI_10></H_EDI_10>';
        l_Trx_Info      := l_Trx_Info || '<WAREHOUSEID>' ||
                           Rec_Trx_Line.Wms_Subinventory_Code ||
                           '</WAREHOUSEID>'; --所属仓库
        l_Trx_Info      := l_Trx_Info || '<Priority></Priority>';
        l_Trx_Info      := l_Trx_Info || '<FollowUp></FollowUp>';
        l_Trx_Info      := l_Trx_Info || '<CKDM>' ||
                           Rec_Trx_Line.Secondary_Inventory_Name ||
                           '</CKDM>'; --ERP仓库代码
      END IF;
      --明细信息
      l_Trx_Info := l_Trx_Info || '<detailsItem>';
      l_Trx_Info := l_Trx_Info || '<CustomerID>' ||
                    Rec_Trx_Line.Company_Code || '1</CustomerID>'; --货主（产品线代码）
      l_Trx_Info := l_Trx_Info || '<SKU>' || Rec_Trx_Line.Item_Number ||
                    '</SKU>'; --产品代码
      l_Trx_Info := l_Trx_Info || '<SKUDescrC></SKUDescrC>';
      l_Trx_Info := l_Trx_Info || '<SKUDescrE></SKUDescrE>';
      l_Trx_Info := l_Trx_Info || '<ExpectedQty>' ||
                    To_Char(Rec_Trx_Line.Primary_Quantity) ||
                    '</ExpectedQty>'; --预期数量
      l_Trx_Info := l_Trx_Info || '<LotAtt01></LotAtt01>';
      l_Trx_Info := l_Trx_Info || '<LotAtt02></LotAtt02>';
      l_Trx_Info := l_Trx_Info || '<LotAtt03></LotAtt03>';
      l_Trx_Info := l_Trx_Info || '<LotAtt04>' ||
                    Rec_Trx_Line.Secondary_Inventory_Name || '</LotAtt04>'; --ERP仓库号
      l_Trx_Info := l_Trx_Info || '<LotAtt05></LotAtt05>';
      l_Trx_Info := l_Trx_Info || '<LotAtt06></LotAtt06>';
      l_Trx_Info := l_Trx_Info || '<LotAtt07></LotAtt07>';
      l_Trx_Info := l_Trx_Info || '<LotAtt08>' ||
                    Rec_Trx_Line.Contract_Number || '</LotAtt08>'; --属性号，只有成品和半成品用到
      l_Trx_Info := l_Trx_Info || '<LotAtt09></LotAtt09>';
      l_Trx_Info := l_Trx_Info || '<LotAtt10></LotAtt10>';
      l_Trx_Info := l_Trx_Info || '<LotAtt11></LotAtt11>';
      l_Trx_Info := l_Trx_Info || '<LotAtt12></LotAtt12>';
      l_Trx_Info := l_Trx_Info || '<TotalPrice></TotalPrice>';
      l_Trx_Info := l_Trx_Info || '<UserDefine1></UserDefine1>';
      l_Trx_Info := l_Trx_Info || '<UserDefine2></UserDefine2>';
      l_Trx_Info := l_Trx_Info || '<UserDefine3></UserDefine3>';
      l_Trx_Info := l_Trx_Info || '<UserDefine4></UserDefine4>';
      l_Trx_Info := l_Trx_Info || '<UserDefine5></UserDefine5>';
      l_Trx_Info := l_Trx_Info || '<UserDefine6></UserDefine6>';
      l_Trx_Info := l_Trx_Info || '<NOTES></NOTES>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_01>' || Rec_Trx_Line.Trx_Number ||
                    '</D_EDI_01>'; --ERP单号
      l_Trx_Info := l_Trx_Info || '<D_EDI_02>' ||
                    To_Char(Rec_Trx_Line.Line_Number) || '</D_EDI_02>'; --ERP行号
      l_Trx_Info := l_Trx_Info || '<D_EDI_03></D_EDI_03>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_04></D_EDI_04>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_05></D_EDI_05>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_06></D_EDI_06>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_07></D_EDI_07>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_08></D_EDI_08>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_09></D_EDI_09>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_10></D_EDI_10>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_11></D_EDI_11>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_12></D_EDI_12>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_13></D_EDI_13>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_14></D_EDI_14>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_15></D_EDI_15>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_16></D_EDI_16>';
      l_Trx_Info := l_Trx_Info || '</detailsItem>';
    END LOOP;
    IF l_Line_Count = 0 THEN
      RETURN;
    END IF;
    --补全XML文档
    l_Trx_Info := l_Trx_Info || '</wmsASNHeaders>';
    l_Trx_Info := l_Trx_Info || '</asnInfo>';
    l_Trx_Info := l_Trx_Info || '</DATA>';
    l_Trx_Info := l_Trx_Info || '</STD_IN>';
    IF l_Destination_System IS NOT NULL THEN
      --如果有需要发布的数据
      Cux_Wms_Pub.Transfer_Service(x_Ret_Status         => l_Ret_Status,
                                   x_Error_Msg          => l_Error_Msg,
                                   p_Destination_System => l_Destination_System,
                                   p_Entity_Type        => l_Source_Module ||
                                                           '_TRX',
                                   p_Operation_Code     => 'SMTN_stockin_normal',
                                   p_Trx_Number         => l_Trx_Number,
                                   p_Interface          => g_Package_Name,
                                   p_Method             => l_Program_Name,
                                   p_Trx_Info           => l_Trx_Info);
    END IF;
    --更新抛转完成状态
    IF l_Ret_Status = Fnd_Api.g_Ret_Sts_Success OR
       l_Destination_System IS NULL THEN
      UPDATE Cux_Ready_Item t
         SET t.Transfer_Status = 'TRANSFERRED'
       WHERE t.Organization_Id = p_Organization_Id
         AND t.Doc_No = p_Doc_No;
    ELSE
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg := l_Error_Msg;
    END IF;
    COMMIT;
  EXCEPTION
    WHEN Fnd_Api.g_Exc_Error THEN
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg := l_Program_Name || '@' || l_Setp || ':' || x_Error_Msg;
    WHEN OTHERS THEN
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Unexp_Error;
      x_Error_Msg := l_Program_Name || '@' || l_Setp || ':' || SQLERRM;
  END Transfer_Wip_Return_Data;
  --处理工单拉式还退料                      
  PROCEDURE Transfer_Wip_Pull_Return_Data(x_Ret_Sts        OUT VARCHAR2,
                                          x_Error_Msg      OUT VARCHAR2,
                                          p_Transaction_Id NUMBER) IS
    l_Program_Name  VARCHAR2(30) := 'TRANSFER_WIP_PULL_RETURN_DATA';
    l_Setp          VARCHAR2(30);
    l_Source_Module VARCHAR2(30) := 'WIPR';
    l_Username      VARCHAR2(50);
    l_Password      VARCHAR2(50);
    CURSOR Cur_Trx_Lines IS
      SELECT Trx_Type.Attribute2 Order_Type
            ,Company.Lookup_Code Company_Code --Customer_Id
            ,Wmr.Transaction_Id Trx_Number --Asn_Reference_1/D_EDI_01
            ,Trx_Type.Lookup_Code Trx_Type --  H_EDI_08
            ,Trx_Type.Description Trx_Type_Name --  Asn_Reference_2
            ,Msi.Attribute7 Wms_Subinventory_Code --Warehouseid
            ,Msi.Attribute6 Wms_System
            ,Msi.Secondary_Inventory_Name --Ckdm LotAtt04
            ,Msib.Segment1 Item_Number --Sku
            ,Wmr.Return_Quantity Primary_Quantity --Expected_Qty
            ,Wmr.Contract_Number --Lot_Att_08
            ,Wmr.Line_Number --d_Edi_02
            ,Ood.Operating_Unit
            ,Wmr.Creation_Date
            ,Assembly.Segment1 Assembly_Item
            ,Decode(Nvl(Cwc.Combination_Request_Id, 0),
                    0,
                    NULL,
                    Cwc.Combination_Number) Combination_Number
            ,Wdj.Attribute3 Workshop
            ,We.Wip_Entity_Name
            ,NULL Operation_Code
            ,Decode(Nvl(Cwc.Combination_Request_Id, 0),
                    0,
                    NULL,
                    Cwc.Entity_Count) Entity_Count
            ,Decode(Nvl(Cwc.Combination_Request_Id, 0),
                    0,
                    NULL,
                    Cwcd.Sequence_No) Sequence_No
        FROM Cux_Wip_Material_Return      Wmr
            ,Org_Organization_Definitions Ood
            ,Fnd_Lookup_Values_Vl         Company
            ,Fnd_Lookup_Values_Vl         Trx_Type
            ,Mtl_Secondary_Inventories    Msi
            ,Mtl_System_Items_b           Msib
            ,Wip_Discrete_Jobs            Wdj
            ,Mtl_System_Items_b           Assembly
            ,Wip_Entities                 We
            ,Cux_Wip_Combination_Details  Cwcd
            ,Cux_Wip_Combinations         Cwc
       WHERE Wmr.Status = '1'
         AND Wmr.Transfer_Status IN ('WAITING', 'TRANSFERRING')
         AND Wmr.Organization_Id = Ood.Organization_Id
         AND 'CUX_LEGAL_ENTITY' = Company.Lookup_Type
         AND Ood.Operating_Unit = Company.Meaning
         AND 'CUX_INV_TRX_TYPE' = Trx_Type.Lookup_Type
         AND To_Char(Wmr.Source_Type) = Trx_Type.Meaning
         AND l_Source_Module = Trx_Type.Tag
         AND Trx_Type.Attribute1 = 'I'
         AND Wmr.Organization_Id = Msi.Organization_Id
         AND Wmr.Subinventory = Msi.Secondary_Inventory_Name
         AND Wmr.Organization_Id = Msib.Organization_Id
         AND Wmr.Inventory_Item_Id = Msib.Inventory_Item_Id
         AND Wmr.Wip_Entity_Id = Wdj.Wip_Entity_Id
         AND Wmr.Organization_Id = Wdj.Organization_Id
         AND Wdj.Organization_Id = Assembly.Organization_Id
         AND Wdj.Primary_Item_Id = Assembly.Inventory_Item_Id
         AND Wdj.Wip_Entity_Id = We.Wip_Entity_Id
         AND Wdj.Organization_Id = We.Organization_Id
         AND Wmr.Wip_Entity_Id = Cwcd.Wip_Entity_Id(+)
         AND 'Y' = Cwcd.Active_Flag(+)
         AND Wmr.Organization_Id = Cwcd.Organization_Id(+)
         AND Cwcd.Combination_Id = Cwc.Combination_Id(+)
         AND Wmr.Select_Flag = 'Y'
         AND Wmr.Transaction_Id = p_Transaction_Id;
  
    CURSOR Cur_Lock_Return_Lines IS
      SELECT Wmr.Status
            ,Wmr.Organization_Id
            ,Wmr.Transaction_Id
            ,Wmr.Transfer_Status
        FROM Cux_Wip_Material_Return Wmr
       WHERE Wmr.Status = '1'
         AND Wmr.Transfer_Status IN ('WAITING', 'TRANSFERRING')
         AND Wmr.Transaction_Id = p_Transaction_Id
         FOR UPDATE NOWAIT;
    l_Lock_Return_Lines Cur_Lock_Return_Lines%ROWTYPE;
    l_Line_Count        NUMBER;
  
    l_Trx_Info      CLOB;
    l_Unique_Trx_No VARCHAR2(30);
  
    l_Destination_System VARCHAR2(30);
    l_Trx_Number         VARCHAR2(30);
    l_Ret_Status         VARCHAR2(10);
    l_Error_Msg          VARCHAR2(200);
  BEGIN
    x_Ret_Sts := Fnd_Api.g_Ret_Sts_Success;
    --状态更新为抛转中
    BEGIN
      OPEN Cur_Lock_Return_Lines;
      FETCH Cur_Lock_Return_Lines
        INTO l_Lock_Return_Lines;
      CLOSE Cur_Lock_Return_Lines;
    EXCEPTION
      WHEN OTHERS THEN
        IF Cur_Lock_Return_Lines%ISOPEN THEN
          CLOSE Cur_Lock_Return_Lines;
        END IF;
        x_Error_Msg := '单据被其它程序使用。';
        RAISE Fnd_Api.g_Exc_Error;
    END;
  
    IF l_Lock_Return_Lines.Transaction_Id IS NULL THEN
      ROLLBACK;
      RETURN;
    ELSIF l_Lock_Return_Lines.Transfer_Status = 'WAITING' THEN
      UPDATE Cux_Wip_Material_Return t
         SET t.Transfer_Status = 'TRANSFERRING'
       WHERE t.Transaction_Id = p_Transaction_Id;
    END IF;
    l_Line_Count := 0;
    --获取传送信息
    FOR Rec_Trx_Line IN Cur_Trx_Lines LOOP
      l_Line_Count := l_Line_Count + 1;
      IF Cur_Trx_Lines%ROWCOUNT = 1 THEN
        l_Destination_System := Rec_Trx_Line.Wms_System;
        l_Trx_Number         := Rec_Trx_Line.Trx_Number;
        --如果是第一行，构建XML单据头信息
        Cux_Soa_Pub.Get_Service_Account(p_Service_System => l_Destination_System,
                                        x_Username       => l_Username,
                                        x_Password       => l_Password);
        l_Trx_Info := '<?xml version="1.0" encoding="utf-8"?>';
        l_Trx_Info := l_Trx_Info || '<STD_IN Origin="WMS">';
        l_Trx_Info := l_Trx_Info || '<Tiptop_user>' || l_Username ||
                      '</Tiptop_user>';
        l_Trx_Info := l_Trx_Info || '<Tiptop_pwd>' || l_Password ||
                      '</Tiptop_pwd>';
        l_Trx_Info := l_Trx_Info || '<ServiceName>IStockIN</ServiceName>';
        l_Trx_Info := l_Trx_Info || '<Factory>' ||
                      Rec_Trx_Line.Company_Code || '</Factory >'; --法人
        l_Trx_Info := l_Trx_Info || '<Source>ERP</Source>';
        l_Trx_Info := l_Trx_Info ||
                      '<ObjectID>SMTN_stockin_normal</ObjectID>';
        l_Trx_Info := l_Trx_Info || '<DATA>';
        l_Trx_Info := l_Trx_Info || '<asnInfo>';
        l_Trx_Info := l_Trx_Info || '<wmsASNHeaders>';
        l_Trx_Info := l_Trx_Info || '<ASNType>' || Rec_Trx_Line.Order_Type ||
                      '</ASNType>'; --预期到货通知类型
        l_Trx_Info := l_Trx_Info || '<CustomerID>' ||
                      Rec_Trx_Line.Company_Code || '1</CustomerID>'; --货主ID
        l_Trx_Info := l_Trx_Info || '<ASNCreationTime></ASNCreationTime>';
        l_Trx_Info := l_Trx_Info ||
                      '<ExpectedArriveTime1></ExpectedArriveTime1>';
        l_Trx_Info := l_Trx_Info || '<ASNReference1>' ||
                      Rec_Trx_Line.Trx_Number || '</ASNReference1>'; --ERP单号，里已包含单别代码
        l_Trx_Info := l_Trx_Info || '<ASNReference2>' ||
                      Rec_Trx_Line.Trx_Type_Name || '</ASNReference2>'; --单别名称
        l_Trx_Info := l_Trx_Info || '<ASNReference3></ASNReference3>';
        l_Trx_Info := l_Trx_Info || '<ASNReference4></ASNReference4>';
        l_Trx_Info := l_Trx_Info || '<ASNReference5></ASNReference5>';
        l_Trx_Info := l_Trx_Info || '<UserDefine1></UserDefine1>';
        l_Trx_Info := l_Trx_Info || '<UserDefine2></UserDefine2>';
        l_Trx_Info := l_Trx_Info || '<UserDefine3></UserDefine3>';
        l_Trx_Info := l_Trx_Info || '<UserDefine4></UserDefine4>';
        l_Trx_Info := l_Trx_Info || '<UserDefine5></UserDefine5>';
        l_Trx_Info := l_Trx_Info || '<UserDefine6></UserDefine6>';
        l_Trx_Info := l_Trx_Info || '<UserDefine7></UserDefine7>';
        l_Trx_Info := l_Trx_Info || '<UserDefine8></UserDefine8>';
        l_Trx_Info := l_Trx_Info || '<Notes></Notes>';
        l_Trx_Info := l_Trx_Info || '<SupplierID></SupplierID>';
        l_Trx_Info := l_Trx_Info || '<Supplier_Name></Supplier_Name>';
        l_Trx_Info := l_Trx_Info || '<H_EDI_01></H_EDI_01>';
        l_Trx_Info := l_Trx_Info || '<H_EDI_02></H_EDI_02>';
        l_Trx_Info := l_Trx_Info || '<H_EDI_03></H_EDI_03>';
        l_Trx_Info := l_Trx_Info || '<H_EDI_04></H_EDI_04>';
        l_Trx_Info := l_Trx_Info || '<H_EDI_05></H_EDI_05>';
        l_Trx_Info := l_Trx_Info || '<H_EDI_06></H_EDI_06>';
        --ERP作业编号 H_EDI_07  
        l_Unique_Trx_No := Cux_Wms_Pub.Generate_Unique_Trx_No(p_Org_Id           => Rec_Trx_Line.Operating_Unit,
                                                              p_Trx_Order_Type   => Rec_Trx_Line.Trx_Type,
                                                              p_Trx_Order_Number => Rec_Trx_Line.Trx_Number,
                                                              p_Prog_Appl_Id     => Fnd_Global.Resp_Appl_Id,
                                                              p_Conc_Program_Id  => Fnd_Global.Conc_Program_Id,
                                                              p_Conc_Request_Id  => Fnd_Global.Conc_Request_Id);
        l_Trx_Info      := l_Trx_Info || '<H_EDI_07>' || l_Unique_Trx_No ||
                           '</H_EDI_07>'; --ERP作业ID
        l_Trx_Info      := l_Trx_Info || '<H_EDI_08>' ||
                           Rec_Trx_Line.Trx_Type || '</H_EDI_08>'; --单别代码 
        l_Trx_Info      := l_Trx_Info || '<H_EDI_09></H_EDI_09>';
        l_Trx_Info      := l_Trx_Info || '<H_EDI_10></H_EDI_10>';
        l_Trx_Info      := l_Trx_Info || '<WAREHOUSEID>' ||
                           Rec_Trx_Line.Wms_Subinventory_Code ||
                           '</WAREHOUSEID>'; --所属仓库
        l_Trx_Info      := l_Trx_Info || '<Priority></Priority>';
        l_Trx_Info      := l_Trx_Info || '<FollowUp></FollowUp>';
        l_Trx_Info      := l_Trx_Info || '<CKDM>' ||
                           Rec_Trx_Line.Secondary_Inventory_Name ||
                           '</CKDM>'; --ERP仓库代码
      END IF;
      --明细信息
      l_Trx_Info := l_Trx_Info || '<detailsItem>';
      l_Trx_Info := l_Trx_Info || '<CustomerID>' ||
                    Rec_Trx_Line.Company_Code || '1</CustomerID>'; --货主（产品线代码）
      l_Trx_Info := l_Trx_Info || '<SKU>' || Rec_Trx_Line.Item_Number ||
                    '</SKU>'; --产品代码
      l_Trx_Info := l_Trx_Info || '<SKUDescrC></SKUDescrC>';
      l_Trx_Info := l_Trx_Info || '<SKUDescrE></SKUDescrE>';
      l_Trx_Info := l_Trx_Info || '<ExpectedQty>' ||
                    To_Char(Rec_Trx_Line.Primary_Quantity) ||
                    '</ExpectedQty>'; --预期数量
      l_Trx_Info := l_Trx_Info || '<LotAtt01></LotAtt01>';
      l_Trx_Info := l_Trx_Info || '<LotAtt02></LotAtt02>';
      l_Trx_Info := l_Trx_Info || '<LotAtt03></LotAtt03>';
      l_Trx_Info := l_Trx_Info || '<LotAtt04>' ||
                    Rec_Trx_Line.Secondary_Inventory_Name || '</LotAtt04>'; --ERP仓库号
      l_Trx_Info := l_Trx_Info || '<LotAtt05></LotAtt05>';
      l_Trx_Info := l_Trx_Info || '<LotAtt06></LotAtt06>';
      l_Trx_Info := l_Trx_Info || '<LotAtt07></LotAtt07>';
      l_Trx_Info := l_Trx_Info || '<LotAtt08>' ||
                    Rec_Trx_Line.Contract_Number || '</LotAtt08>'; --属性号，只有成品和半成品用到
      l_Trx_Info := l_Trx_Info || '<LotAtt09></LotAtt09>';
      l_Trx_Info := l_Trx_Info || '<LotAtt10></LotAtt10>';
      l_Trx_Info := l_Trx_Info || '<LotAtt11></LotAtt11>';
      l_Trx_Info := l_Trx_Info || '<LotAtt12></LotAtt12>';
      l_Trx_Info := l_Trx_Info || '<TotalPrice></TotalPrice>';
      l_Trx_Info := l_Trx_Info || '<UserDefine1></UserDefine1>';
      l_Trx_Info := l_Trx_Info || '<UserDefine2></UserDefine2>';
      l_Trx_Info := l_Trx_Info || '<UserDefine3></UserDefine3>';
      l_Trx_Info := l_Trx_Info || '<UserDefine4></UserDefine4>';
      l_Trx_Info := l_Trx_Info || '<UserDefine5></UserDefine5>';
      l_Trx_Info := l_Trx_Info || '<UserDefine6></UserDefine6>';
      l_Trx_Info := l_Trx_Info || '<NOTES></NOTES>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_01>' || Rec_Trx_Line.Trx_Number ||
                    '</D_EDI_01>'; --ERP单号
      l_Trx_Info := l_Trx_Info || '<D_EDI_02>' ||
                    To_Char(Rec_Trx_Line.Line_Number) || '</D_EDI_02>'; --ERP行号
      l_Trx_Info := l_Trx_Info || '<D_EDI_03></D_EDI_03>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_04></D_EDI_04>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_05></D_EDI_05>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_06></D_EDI_06>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_07></D_EDI_07>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_08></D_EDI_08>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_09></D_EDI_09>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_10></D_EDI_10>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_11></D_EDI_11>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_12></D_EDI_12>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_13></D_EDI_13>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_14></D_EDI_14>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_15></D_EDI_15>';
      l_Trx_Info := l_Trx_Info || '<D_EDI_16></D_EDI_16>';
      l_Trx_Info := l_Trx_Info || '</detailsItem>';
    END LOOP;
    IF l_Line_Count = 0 THEN
      RETURN;
    END IF;
    --补全XML文档
    l_Trx_Info := l_Trx_Info || '</wmsASNHeaders>';
    l_Trx_Info := l_Trx_Info || '</asnInfo>';
    l_Trx_Info := l_Trx_Info || '</DATA>';
    l_Trx_Info := l_Trx_Info || '</STD_IN>';
    IF l_Destination_System IS NOT NULL THEN
      --如果有需要发布的数据
      Cux_Wms_Pub.Transfer_Service(x_Ret_Status         => l_Ret_Status,
                                   x_Error_Msg          => l_Error_Msg,
                                   p_Destination_System => l_Destination_System,
                                   p_Entity_Type        => l_Source_Module ||
                                                           '_TRX',
                                   p_Operation_Code     => 'SMTN_stockin_normal',
                                   p_Trx_Number         => l_Trx_Number,
                                   p_Interface          => g_Package_Name,
                                   p_Method             => l_Program_Name,
                                   p_Trx_Info           => l_Trx_Info);
    END IF;
    --更新抛转完成状态
    IF l_Ret_Status = Fnd_Api.g_Ret_Sts_Success OR
       l_Destination_System IS NULL THEN
      UPDATE Cux_Wip_Material_Return t
         SET t.Transfer_Status = 'TRANSFERRED'
       WHERE t.Transaction_Id = p_Transaction_Id;
    ELSE
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg := l_Error_Msg;
    END IF;
    COMMIT;
  EXCEPTION
    WHEN Fnd_Api.g_Exc_Error THEN
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg := l_Program_Name || '@' || l_Setp || ':' || x_Error_Msg;
    WHEN OTHERS THEN
      x_Ret_Sts   := Fnd_Api.g_Ret_Sts_Unexp_Error;
      x_Error_Msg := l_Program_Name || '@' || l_Setp || ':' || SQLERRM;
  END Transfer_Wip_Pull_Return_Data;
END Cux_Wip2wms_Transfer_Pkg;
/
