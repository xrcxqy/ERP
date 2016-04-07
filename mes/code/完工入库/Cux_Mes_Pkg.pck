CREATE OR REPLACE PACKAGE Cux_Mes_Pkg AS

  TYPE Completion_Detail_Tab IS TABLE OF Cux_Mes_Completion_Details%ROWTYPE INDEX BY PLS_INTEGER;

  g_Factory VARCHAR2(10);

  PROCEDURE Cux_Mes_Import(Retcode           IN VARCHAR2,
                           Errbuf            IN VARCHAR2,
                           p_Organization_Id IN NUMBER,
                           p_Wip_Entity_Id   IN NUMBER);

  PROCEDURE E2m_Job(Errbuf            OUT NOCOPY VARCHAR2,
                    Retcode           OUT NOCOPY VARCHAR2,
                    p_Organization_Id IN NUMBER,
                    p_Wip_Entity_Name IN VARCHAR2);

  PROCEDURE Parser_Resp(p_Xml_Str  IN VARCHAR2,
                        l_Retvalue OUT VARCHAR2,
                        l_Message  OUT VARCHAR2);
  PROCEDURE Cux_Mes_Import_Batch(Retcode IN VARCHAR2, Errbuf IN VARCHAR2);
  /*  PROCEDURE CUX_MES_IMPORT_NEW(RETCODE     IN VARCHAR2,
  ERRBUF      IN VARCHAR2,P_ORGANIZATION_ID IN NUMBER, P_WIP_ENTITY_ID IN NUMBER); */
  PROCEDURE Create_Bom(p_Wip_Entity_Id IN NUMBER);

  PROCEDURE Complete_Job(p_Completion_Job IN CLOB, x_Response OUT CLOB);

  --返回过账信息
  PROCEDURE Post_Completion_2_Mes(x_Ret_Status    OUT VARCHAR2,
                                  x_Error_Msg     OUT VARCHAR2,
                                  p_Completion_Id NUMBER);

END;
/
CREATE OR REPLACE PACKAGE BODY Cux_Mes_Pkg AS
  /*PROCEDURE CUX_MES_IMPORT(RETCODE           IN VARCHAR2,
                             ERRBUF            IN VARCHAR2,
                             P_ORGANIZATION_ID IN NUMBER,
                             P_WIP_ENTITY_ID   IN NUMBER) AS
      CURSOR C1 IS
        SELECT WDJ.WIP_ENTITY_ID,
               WDJ.SCHEDULED_START_DATE,
               WDJ.SCHEDULED_COMPLETION_DATE,
               WDJ.SCHEDULED_START_DATE REQUESTED_START_DATE,
               WE.WIP_ENTITY_NAME,
               WE.PRIMARY_ITEM_ID,
               DECODE(WE.ORGANIZATION_ID, 83, 'G1', 'H1') ORGANIZATION_ID,
               WAC.Class_Code DESCRIPTION,
               WDJ.ATTRIBUTE3, ---车间
               --WDJ.ATTRIBUTE3, ---是否改版
               DECODE(WAC.CLASS_CODE, 4, 'Y', 'N') ATTRIBUTE2, ---是否改版
               WDJ.START_QUANTITY,
               MSI.SEGMENT1,
               DECODE((SELECT MIC.SEGMENT1
                        FROM MTL_ITEM_CATEGORIES_V MIC
                       WHERE MIC.ORGANIZATION_ID = MSI.ORGANIZATION_ID
                         AND MIC.STRUCTURE_ID = 101 --物料类别
                         AND MIC.INVENTORY_ITEM_ID = MSI.INVENTORY_ITEM_ID),
                      '01',
                      1,
                      2) ITEM_SEGMENT1
          FROM WIP_DISCRETE_JOBS      WDJ,
               WIP_ACCOUNTING_CLASSES WAC,
               WIP_ENTITIES           WE,
               MTL_SYSTEM_ITEMS_B     MSI
         WHERE WDJ.ORGANIZATION_ID = P_ORGANIZATION_ID
           AND WE.WIP_ENTITY_ID = P_WIP_ENTITY_ID
           AND MSI.ORGANIZATION_ID = WE.ORGANIZATION_ID
           AND MSI.INVENTORY_ITEM_ID = WE.PRIMARY_ITEM_ID
           AND WDJ.WIP_ENTITY_ID = WE.WIP_ENTITY_ID
           AND WAC.ORGANIZATION_ID = WDJ.ORGANIZATION_ID
           AND WAC.CLASS_CODE = WDJ.CLASS_CODE
           AND NVL(WDJ.ATTRIBUTE15, 'N') = 'N' --是否已经成功传入MES接口
           AND WDJ.STATUS_TYPE = '3'; --已发放 
      CURSOR C2(P_WIP_ENTITY_ID IN NUMBER, P_TYPE IN NUMBER) IS
        SELECT WO.OPERATION_SEQ_NUM, WO.OPERATION_CODE
          FROM WIP_OPERATIONS_V WO
         WHERE WO.WIP_ENTITY_ID = P_WIP_ENTITY_ID
           AND WO.ORGANIZATION_ID = P_ORGANIZATION_ID
           AND P_TYPE = 1
           AND WO.OPERATION_CODE IN ('DB')
        UNION
        SELECT WO.OPERATION_SEQ_NUM, WO.OPERATION_CODE
          FROM WIP_OPERATIONS_V WO
         WHERE WO.WIP_ENTITY_ID = P_WIP_ENTITY_ID
           AND WO.ORGANIZATION_ID = P_ORGANIZATION_ID
           AND P_TYPE = 2
           AND WO.OPERATION_CODE IN ('SMT', 'ZB', 'TS');
      CURSOR C3(P_WIP_ENTITY_ID IN NUMBER) IS
        SELECT WRO.OPERATION_SEQ_NUM,
               MSI.SEGMENT1,
               WRO.REQUIRED_QUANTITY,
               WRO.QUANTITY_PER_ASSEMBLY,
               WRO.INVENTORY_ITEM_ID,
               WRO.ORGANIZATION_ID
          FROM WIP_REQUIREMENT_OPERATIONS WRO, MTL_SYSTEM_ITEMS_B MSI
         WHERE WRO.WIP_ENTITY_ID = P_WIP_ENTITY_ID
           AND WRO.ORGANIZATION_ID = P_ORGANIZATION_ID
           AND MSI.INVENTORY_ITEM_ID = WRO.INVENTORY_ITEM_ID
           AND MSI.ORGANIZATION_ID = WRO.ORGANIZATION_ID;
      DOC                 XMLDOM.DOMDOCUMENT;
      ET                  XMLDOM.DOMELEMENT;
      ET1                 XMLDOM.DOMELEMENT;
      ET2                 XMLDOM.DOMELEMENT;
      TEMPNODE            XMLDOM.DOMNODE;
      TEMPNODE1           XMLDOM.DOMNODE;
      TEMPNODE4           XMLDOM.DOMNODE;
      TEMPNODE5           XMLDOM.DOMNODE;
      TEMPNODE6           XMLDOM.DOMNODE;
      TEMPNODE2           XMLDOM.DOMTEXT;
      TEMPNODE3           XMLDOM.DOMNODE;
      STR                 CLOB;
      V_OPERATION_CODE    VARCHAR2(1000);
      V_REQUIRED_QUANTITY NUMBER;
      V_REPLACEMATCODE    VARCHAR2(1000);
      V_WORKORDER         VARCHAR2(1000);
      V_REPLACENUM        NUMBER;
      V_SEQ               NUMBER;
      ------------------------------
      REQ        UTL_HTTP.REQ;
      RESP       UTL_HTTP.RESP;
      L_XML_CHAR VARCHAR2(32764);
      --VALUESS     VARCHAR2(10000);
      L_REPLYLINE VARCHAR2(1000);
      V_3         VARCHAR2(30000);
      V_2         NUMBER;
      \*   V_1                 VARCHAR2(32764);*\
    \*  V_1 CLOB;*\
      v_1 CLOB;
      \*    l_rep_str           VARCHAR2(32764);*\
      l_rep_str                  CLOB;
      l_return_code              VARCHAR2(100);
      L_RETVALUE                 VARCHAR2(100);
      L_MESSAGE                  VARCHAR2(100);
      V_CONCURRENT_STATUS        BOOLEAN;
      V_FLAG                     VARCHAR2(100);
      V_SUBSTITUTE_ITEM_QUANTITY NUMBER;
      V_component_quantity       NUMBER;
      V_SEGMENT1_NEW             VARCHAR2(100);
      V_COUNT                    NUMBER;
    BEGIN
      FOR X IN C1 LOOP
        DOC       := XMLDOM.NEWDOMDOCUMENT;
        ET        := XMLDOM.CREATEELEMENT(DOC, 'STD_IN');
        TEMPNODE  := XMLDOM.APPENDCHILD(XMLDOM.MAKENODE(DOC),
                                        XMLDOM.MAKENODE(ET));
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'EDI_user');
        TEMPNODE3 := XMLDOM.APPENDCHILD(TEMPNODE, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, 'MESUSER');
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'EDI_pwd');
        TEMPNODE3 := XMLDOM.APPENDCHILD(TEMPNODE, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, 'MESUPWD');
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(TEMPNODE2));
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'sfb_file');
        TEMPNODE3 := XMLDOM.APPENDCHILD(TEMPNODE, XMLDOM.MAKENODE(ET2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKORDER'); --任务号
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
        --FW:  G1511-年+月+日+四位流水号 ，例如：G1511-1409100001
        --JW:  H1511-年+月+日+四位流水号 ，例如：H1511-1409100001
        SELECT NVL(MAX(A.SEQ), 0) + 1
          INTO V_SEQ
          FROM CUX.CUX_WIP_MES_BODY A
         WHERE A.WIP_DATE = TO_CHAR(SYSDATE, 'YYMMDD');
      
        V_WORKORDER := X.ORGANIZATION_ID || '511' || '-' ||
                       TO_CHAR(SYSDATE, 'YYMMDD') || LPAD(V_SEQ, 4, '0');
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, V_WORKORDER);
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKSALENO'); --传票号
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, X.WIP_ENTITY_NAME);
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKPLDATE'); --计划生产日期
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC,
                                           TO_CHAR(X.SCHEDULED_START_DATE,
                                                   'YYYY/MM/DD'));
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKPFLDATE'); --计划完工日期
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC,
                                           TO_CHAR(X.SCHEDULED_COMPLETION_DATE,
                                                   'YYYY/MM/DD'));
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKPDNDATE'); --计划下达日期
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC,
                                           TO_CHAR(X.REQUESTED_START_DATE,
                                                   'YYYY/MM/DD'));
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKMATCODE'); --产品编码
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, X.SEGMENT1);
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKQTY'); --数量
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, X.START_QUANTITY);
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKTYPE'); --工单类型
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, X.DESCRIPTION);
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKPRIORITY'); --优先级
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, '');
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKGROUP'); --工单所属组织
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, X.ORGANIZATION_ID);
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKCHILD'); --子公司
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        --传入G/H
        IF P_ORGANIZATION_ID = 83 THEN
          V_FLAG := 'G'; --FW
        ELSE
          V_FLAG := 'H'; --JW
        END IF;
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, V_FLAG);
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'FLAG_CHANGE'); --是否改版
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, X.ATTRIBUTE2);
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WOMAKER'); ---制造商
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, '00');
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        --存放销售订单
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'PROJECTNO'); --项目号
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, '');
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKCENTER'); --ERP抛单车间
        TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
      
        TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, X.ATTRIBUTE3);
      
        TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4, XMLDOM.MAKENODE(TEMPNODE2));
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'ecm_files');
        TEMPNODE3 := XMLDOM.APPENDCHILD(TEMPNODE, XMLDOM.MAKENODE(ET2));
      
        FOR Y IN C2(X.WIP_ENTITY_ID, X.ITEM_SEGMENT1) LOOP
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'ecm_file');
          TEMPNODE6 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKORDER'); --任务号
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, X.WIP_ENTITY_NAME);
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKING'); --任务号
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, y.operation_code);
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKQTY'); --任务号
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, x.START_QUANTITY);
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        END LOOP;
      
        ET2       := XMLDOM.CREATEELEMENT(DOC, 'sfa_files');
        TEMPNODE3 := XMLDOM.APPENDCHILD(TEMPNODE, XMLDOM.MAKENODE(ET2));
      
        FOR Z IN C3(X.WIP_ENTITY_ID) LOOP
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'sfa_file');
          TEMPNODE6 := XMLDOM.APPENDCHILD(TEMPNODE3, XMLDOM.MAKENODE(ET2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKORDER'); --任务号
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, X.WIP_ENTITY_NAME);
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
          BEGIN
            SELECT DISTINCT WO.OPERATION_CODE
              INTO V_OPERATION_CODE
              FROM WIP_OPERATIONS_V WO
             WHERE WO.WIP_ENTITY_ID = P_WIP_ENTITY_ID
               AND WO.ORGANIZATION_ID = P_ORGANIZATION_ID
               AND WO.OPERATION_SEQ_NUM = Z.OPERATION_SEQ_NUM;
          EXCEPTION
            WHEN OTHERS THEN
              V_OPERATION_CODE := NULL;
          END;
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'WORKING'); --工序
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, V_OPERATION_CODE);
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'JOMMATCODE'); --物料编码
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, z.segment1);
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'JOMDOSAGE'); --数量
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, z.quantity_per_assembly);
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'DEVICENO'); --设备
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, '');
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'FEEDERTYPE'); --FEEDER型号
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, '');
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'STATIONID'); --站号
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, '');
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'LOCTIONID'); --位号
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, '');
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'MATTAG'); --物料标识
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, '');
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          V_REPLACEMATCODE := NULL;
          V_REPLACENUM     := NULL;
          \*        FOR W IN (SELECT C.SUBSTITUTE_COMPONENT_ID,
                           MSI.SEGMENT1,
                           MSI.DESCRIPTION
                      FROM BOM_BILL_OF_MATERIALS_V     A,
                           BOM_INVENTORY_COMPONENTS_V  B,
                           BOM_SUBSTITUTE_COMPONENTS_V C,
                           MTL_SYSTEM_ITEMS_B          MSI
                     WHERE A.BILL_SEQUENCE_ID = B.BILL_SEQUENCE_ID
                       AND B.COMPONENT_SEQUENCE_ID = C.COMPONENT_SEQUENCE_ID
                       AND A.ASSEMBLY_ITEM_ID = X.PRIMARY_ITEM_ID
                       AND B.COMPONENT_ITEM_ID = Z.INVENTORY_ITEM_ID
                       AND C.SUBSTITUTE_COMPONENT_ID = MSI.INVENTORY_ITEM_ID
                       AND MSI.ORGANIZATION_ID = Z.ORGANIZATION_ID
                       AND A.ORGANIZATION_ID = MSI.ORGANIZATION_ID) LOOP
            --获取替代料数量
          BEGIN  
            SELECT NVL(SUM(WRO.REQUIRED_QUANTITY), 0)
              INTO V_REQUIRED_QUANTITY
              FROM WIP_REQUIREMENT_OPERATIONS WRO, MTL_SYSTEM_ITEMS_B MSI
             WHERE WRO.WIP_ENTITY_ID = P_WIP_ENTITY_ID
               AND WRO.ORGANIZATION_ID = P_ORGANIZATION_ID
               AND MSI.INVENTORY_ITEM_ID = WRO.INVENTORY_ITEM_ID
               AND MSI.ORGANIZATION_ID = WRO.ORGANIZATION_ID
               AND MSI.INVENTORY_ITEM_ID = W.SUBSTITUTE_COMPONENT_ID;
           EXCEPTION
               WHEN OTHERS THEN
                   V_REQUIRED_QUANTITY:=0;  
               END;       
            IF V_REQUIRED_QUANTITY <> 0 THEN
              V_REPLACEMATCODE := W.SEGMENT1;
              V_REPLACENUM     := V_REQUIRED_QUANTITY /
                                  (V_REQUIRED_QUANTITY + Z.REQUIRED_QUANTITY) * 100;
              EXIT;
            END IF;
          END LOOP;*\
        
          SELECT COUNT(*)
            INTO V_COUNT
            FROM BOM_BILL_OF_MATERIALS_V     A,
                 BOM_INVENTORY_COMPONENTS_V  B,
                 BOM_SUBSTITUTE_COMPONENTS_V C,
                 MTL_SYSTEM_ITEMS_B          MSI,
                 WIP_REQUIREMENT_OPERATIONS  WRO
           WHERE A.BILL_SEQUENCE_ID = B.BILL_SEQUENCE_ID
             AND B.COMPONENT_SEQUENCE_ID = C.COMPONENT_SEQUENCE_ID
             AND A.ASSEMBLY_ITEM_ID = X.PRIMARY_ITEM_ID
             AND C.SUBSTITUTE_COMPONENT_ID = Z.INVENTORY_ITEM_ID
             AND C.SUBSTITUTE_COMPONENT_ID = MSI.INVENTORY_ITEM_ID
             AND MSI.ORGANIZATION_ID = Z.ORGANIZATION_ID
             AND A.ORGANIZATION_ID = MSI.ORGANIZATION_ID
             AND WRO.WIP_ENTITY_ID = P_WIP_ENTITY_ID
             AND WRO.INVENTORY_ITEM_ID = B.COMPONENT_ITEM_ID;
        
          IF V_COUNT = 0 THEN
            --无替代料              
          
            ET2       := XMLDOM.CREATEELEMENT(DOC, 'REPLACEMATCODE'); --被替换物料
            TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
            TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, Z.SEGMENT1);
          
            TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                            XMLDOM.MAKENODE(TEMPNODE2));
          
            ET2       := XMLDOM.CREATEELEMENT(DOC, 'REPLACENUM'); --被替换数量
            TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
          
            TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, 1);
          
            TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                            XMLDOM.MAKENODE(TEMPNODE2));
          ELSE
            SELECT B.component_quantity,
                   C.SUBSTITUTE_ITEM_QUANTITY,
                   MSI1.SEGMENT1
              INTO V_component_quantity,
                   V_SUBSTITUTE_ITEM_QUANTITY,
                   V_SEGMENT1_NEW
              FROM BOM_BILL_OF_MATERIALS_V     A,
                   BOM_INVENTORY_COMPONENTS_V  B,
                   BOM_SUBSTITUTE_COMPONENTS_V C,
                   MTL_SYSTEM_ITEMS_B          MSI,
                   MTL_SYSTEM_ITEMS_B          MSI1,
                   WIP_REQUIREMENT_OPERATIONS  WRO
             WHERE A.BILL_SEQUENCE_ID = B.BILL_SEQUENCE_ID
               AND B.COMPONENT_SEQUENCE_ID = C.COMPONENT_SEQUENCE_ID
               AND A.ASSEMBLY_ITEM_ID = X.PRIMARY_ITEM_ID
               AND C.SUBSTITUTE_COMPONENT_ID = Z.INVENTORY_ITEM_ID
               AND C.SUBSTITUTE_COMPONENT_ID = MSI.INVENTORY_ITEM_ID
               AND MSI.ORGANIZATION_ID = Z.ORGANIZATION_ID
               AND A.ORGANIZATION_ID = MSI.ORGANIZATION_ID
               AND WRO.WIP_ENTITY_ID = P_WIP_ENTITY_ID
               AND WRO.INVENTORY_ITEM_ID = MSI1.INVENTORY_ITEM_ID
               AND MSI1.ORGANIZATION_ID = Z.ORGANIZATION_ID
               AND WRO.INVENTORY_ITEM_ID = B.COMPONENT_ITEM_ID;
          
            ET2       := XMLDOM.CREATEELEMENT(DOC, 'REPLACEMATCODE'); --被替换物料
            TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
            TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, V_SEGMENT1_NEW);
          
            TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                            XMLDOM.MAKENODE(TEMPNODE2));
          
            ET2       := XMLDOM.CREATEELEMENT(DOC, 'REPLACENUM'); --被替换数量
            TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
          
            TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC,
                                               V_SUBSTITUTE_ITEM_QUANTITY /
                                               V_component_quantity);
          
            TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                            XMLDOM.MAKENODE(TEMPNODE2));
          
          END IF;
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'REPLACETAG'); --取替代特性
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, 1);
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'REPLACETYPE'); --替代类型
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, '');
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'FBOMCONTAG'); --部件跟踪标志
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, 1);
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'FBOMMATTAG'); --配置标志
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
          IF V_OPERATION_CODE = 'SMT' THEN
            TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, 0);
          ELSE
            TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, 5);
          END IF;
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'FBOMSTRUCTAG'); --结构标志
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, 0);
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'MATNUMBER'); --应发数量
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, z.required_quantity);
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        
          ET2       := XMLDOM.CREATEELEMENT(DOC, 'SELECTTAG'); --挑料标志
          TEMPNODE4 := XMLDOM.APPENDCHILD(TEMPNODE6, XMLDOM.MAKENODE(ET2));
        
          TEMPNODE2 := XMLDOM.CREATETEXTNODE(DOC, 'N');
        
          TEMPNODE5 := XMLDOM.APPENDCHILD(TEMPNODE4,
                                          XMLDOM.MAKENODE(TEMPNODE2));
        END LOOP;
      
      END LOOP;
      dbms_output.put_line('aa');
  \*    XMLDOM.writeToBuffer(DOC, STR, 'UTF-8');
      XMLDOM.FREEDOCUMENT(DOC);*\
      \*INSERT INTO CUX.CUX_WIP_MES_BODY
        (WIP_ENTITY_ID, BODY_CLOB)
      VALUES
        (P_WIP_ENTITY_ID, STR);    *\
    
  
  DBMS_XMLDOM.WRITETOCLOB(DOC, STR);
  
  
      IF STR IS NOT NULL THEN
        INSERT INTO CUX.CUX_WIP_MES_BODY
          (WIP_ENTITY_ID, BODY_CLOB, WIP_DATE, SEQ)
        VALUES
          (P_WIP_ENTITY_ID, STR, TO_CHAR(SYSDATE, 'YYMMDD'), V_SEQ);
      
        V_1 := '<![CDATA[' || STR || ']]>';
      
        ---------------------------------------开始调用Webservice-------------
      
        --传入参数
        l_xml_char := '<?xml version="1.0" encoding="utf-8"?>
  <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
      <GenerateWorkOrderNew xmlns="http://tempuri.org/">
        <str_workorder>' || v_1 || '</str_workorder>
      </GenerateWorkOrderNew>
    </soap:Body>
  </soap:Envelope>';
      
        ---业务测试 http://192.168.9.54/MESServiceyw/service.asmx
        ---信息中心测试：http://192.168.9.54/MESServiceXXZX/service.asmx 
        req := utl_http.begin_request('http://192.168.9.54/MESServiceyw/service.asmx',
                                      'POST',
                                      utl_http.http_version_1_1);
        -- 保持连接状态
        utl_http.set_persistent_conn_support(req, TRUE);
      
        --设置编码
      
        utl_http.set_header(req, 'Content-Type', 'text/xml; charset=utf-8');
      
        utl_http.set_header(req,
                            'SOAPAction',
                            '"http://tempuri.org/GenerateWorkOrderNew"');
      
        utl_http.set_body_charset(req, 'utf-8');
      
        utl_http.set_header(req, 'Content-Length', lengthb(l_xml_char));
      
        utl_http.write_line(req, l_xml_char);
      
        resp := utl_http.get_response(req);
        dbms_output.put_line('this request status is:' || resp.status_code);
        fnd_file.PUT_LINE(FND_FILE.LOG, resp.status_code);
        dbms_output.put_line('this request status is:' || utl_http.http_ok);
      
        IF (resp.status_code = utl_http.http_ok) THEN
        
          BEGIN
          
            LOOP
              utl_http.read_text(resp, L_replyline);
              dbms_output.put_line(L_replyline);
              fnd_file.PUT_LINE(FND_FILE.LOG, L_replyline);
              l_rep_str := l_rep_str || l_replyline;
            END LOOP;
            utl_http.end_response(resp);
          
          EXCEPTION
            WHEN utl_http.end_of_body THEN
              utl_http.end_response(resp);
          END;
        END IF;
        select replace(l_rep_str, '&lt;', '<') into l_rep_str from dual;
      
        select replace(l_rep_str, '&gt;', '>') into l_rep_str from dual;
      
        ---解析返回结果
        parser_resp(l_rep_str, L_RETVALUE, L_MESSAGE);
        IF L_RETVALUE = '0' THEN
          UPDATE WIP_DISCRETE_JOBS WDJ
             SET WDJ.ATTRIBUTE15 = 'Y'
           WHERE WDJ.WIP_ENTITY_ID = P_WIP_ENTITY_ID;
          COMMIT;
          HTML_REPORT_PKG.V_REPORT_OUTPUT_MODE := 'F';
          HTML_REPORT_PKG.OUTPUT_LINE('<BR>' || '导入成功' || L_MESSAGE ||
                                      '</BR>');
        ELSE
          HTML_REPORT_PKG.V_REPORT_OUTPUT_MODE := 'F';
          HTML_REPORT_PKG.OUTPUT_LINE('<BR>' || '传入MES接口错误,原因' || L_MESSAGE ||
                                      '</BR>');
          V_CONCURRENT_STATUS := FND_CONCURRENT.SET_COMPLETION_STATUS('WARNING',
                                                                      '传入MES接口错误!');
        END IF;
      ELSE
        HTML_REPORT_PKG.V_REPORT_OUTPUT_MODE := 'F';
        HTML_REPORT_PKG.OUTPUT_LINE('<BR>' || '工单不满足传MES条件,工单状态必须是发放且未传入MES' ||
                                    '</BR>');
        V_CONCURRENT_STATUS := FND_CONCURRENT.SET_COMPLETION_STATUS('WARNING',
                                                                    '工单不满足传MES条件!');
      END IF;
    END;*/
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
    v_Retvalue      VARCHAR2(2000);
    v_Message       VARCHAR2(2000);
    l_Xml_Str       VARCHAR2(32670);
  BEGIN
    l_Xml_Str := REPLACE(p_Xml_Str, '“&lt;”', NULL);
    l_Xml_Str := REPLACE(l_Xml_Str, '&lt;', '<');
    l_Xml_Str := REPLACE(l_Xml_Str, '&gt;', '>');
    l_Xml_Str := REPLACE(l_Xml_Str,
                         '<?xml version="1.0" encoding="utf-8"?>',
                         NULL);
  
    Myxml    := Xmltype(l_Xml_Str);
    l_Doc    := Xmldom.Newdomdocument(Myxml);
    Rootnode := Xmldom.Makenode(Xmldom.Getdocumentelement(l_Doc));
  
    l_Nl    := Dbms_Xmldom.Getelementsbytagname(l_Doc, 'STD_OUT');
    l_Count := Dbms_Xmldom.Getlength(l_Nl);
    FOR Cur_Emp IN 0 .. Dbms_Xmldom.Getlength(l_Nl) - 1 LOOP
      l_n := Dbms_Xmldom.Item(l_Nl, Cur_Emp);
      Dbms_Xslprocessor.Valueof(l_n, 'Status/text()', v_Retvalue);
      Dbms_Xslprocessor.Valueof(l_n, 'Error/text()', v_Message);
    END LOOP;
    l_Retvalue := v_Retvalue;
    l_Message  := v_Message;
  END;

  PROCEDURE Parser_Resp_Clob(p_Xml_Clob IN CLOB,
                             x_Xml_Clob OUT CLOB,
                             l_Retvalue OUT VARCHAR2,
                             l_Message  OUT VARCHAR2) AS
    l_Doc      Xmldom.Domdocument;
    Myxml      Xmltype;
    l_Nl       Dbms_Xmldom.Domnodelist;
    l_n        Dbms_Xmldom.Domnode;
    l_Count    NUMBER;
    v_Retvalue VARCHAR2(2000);
    v_Message  VARCHAR2(2000);
    l_Xml_Clob CLOB;
  
    l_Reponse_Node Dbms_Xmldom.Domnode;
  BEGIN
    l_Xml_Clob := REPLACE(p_Xml_Clob, '“&lt;”', NULL);
    l_Xml_Clob := REPLACE(l_Xml_Clob, '&lt;', '<');
    l_Xml_Clob := REPLACE(l_Xml_Clob, '&gt;', '>');
  
    l_Xml_Clob := REPLACE(l_Xml_Clob,
                          '<?xml version="1.0" encoding="utf-8"?>',
                          NULL);
  
    Myxml := Xmltype(l_Xml_Clob);
    l_Doc := Xmldom.Newdomdocument(Myxml);
    --获取返回信息
    l_Nl           := Dbms_Xmldom.Getelementsbytagname(l_Doc,
                                                       'GenerateMesInfoResult');
    l_Reponse_Node := Dbms_Xmldom.Item(l_Nl, 0);
    Dbms_Xmldom.Writetobuffer(Xmldom.Getfirstchild(l_Reponse_Node),
                              x_Xml_Clob);
    --解析返回信息
    l_Nl    := Dbms_Xmldom.Getelementsbytagname(l_Doc, 'STD_OUT');
    l_Count := Dbms_Xmldom.Getlength(l_Nl);
    FOR Cur_Emp IN 0 .. Dbms_Xmldom.Getlength(l_Nl) - 1 LOOP
      l_n := Dbms_Xmldom.Item(l_Nl, Cur_Emp);
      Dbms_Xslprocessor.Valueof(l_n, 'Status/text()', v_Retvalue);
      Dbms_Xslprocessor.Valueof(l_n, 'Error/text()', v_Message);
    END LOOP;
    l_Retvalue := v_Retvalue;
    l_Message  := v_Message;
  END;

  PROCEDURE Cux_Mes_Import(Retcode           IN VARCHAR2,
                           Errbuf            IN VARCHAR2,
                           p_Organization_Id IN NUMBER,
                           p_Wip_Entity_Id   IN NUMBER) AS
    CURSOR C1 IS
      SELECT Wdj.Wip_Entity_Id
            ,Wdj.Scheduled_Start_Date
            ,Wdj.Scheduled_Completion_Date
            ,Wdj.Scheduled_Start_Date Requested_Start_Date
            ,We.Wip_Entity_Name
            ,We.Primary_Item_Id
            ,Decode(We.Organization_Id, 84, 'G1', 'H1') Organization_Id
            ,Wac.Class_Code Description
            ,Wdj.Attribute3
            , ---车间
             --WDJ.ATTRIBUTE3, ---是否改版
             Decode(Wac.Class_Code, 4, 'Y', 'N') Attribute2
            , ---是否改版
             Wdj.Start_Quantity
            ,Msi.Segment1
            ,Decode((SELECT Mic.Segment1
                      FROM Mtl_Item_Categories_v Mic
                     WHERE Mic.Organization_Id = Msi.Organization_Id
                       AND Mic.Structure_Id = 101 --物料类别
                       AND Mic.Inventory_Item_Id = Msi.Inventory_Item_Id),
                    '01',
                    1,
                    2) Item_Segment1
        FROM Wip_Discrete_Jobs      Wdj
            ,Wip_Accounting_Classes Wac
            ,Wip_Entities           We
            ,Mtl_System_Items_b     Msi
       WHERE Wdj.Organization_Id = p_Organization_Id
         AND We.Wip_Entity_Id = p_Wip_Entity_Id
         AND Msi.Organization_Id = We.Organization_Id
         AND Msi.Inventory_Item_Id = We.Primary_Item_Id
         AND Wdj.Wip_Entity_Id = We.Wip_Entity_Id
         AND Wac.Organization_Id = Wdj.Organization_Id
            --AND WDJ.CLASS_CODE NOT IN (13,12) 
            /* updated by luojun 2015-03-06
              将排除工单类型维护在快码上
            */
         AND NOT EXISTS
       (SELECT 1
                FROM Fnd_Lookup_Values Flv
               WHERE Flv.Lookup_Type = 'CUX_WIP_2_MES_EXCLUDED_CLASS'
                 AND Flv.Enabled_Flag = 'Y'
                 AND Trunc(SYSDATE) BETWEEN
                     Nvl(Flv.Start_Date_Active, Trunc(SYSDATE)) AND
                     Nvl(Flv.End_Date_Active, Trunc(SYSDATE))
                 AND Flv.Language = 'US'
                 AND Flv.Lookup_Code = Wdj.Class_Code)
         AND Wdj.Attribute3 NOT IN ('91006')
         AND Msi.Segment1 NOT LIKE 'V-080%'
         AND Wac.Class_Code = Wdj.Class_Code
            /*         AND NVL(WDJ.ATTRIBUTE15, 'N') = 'N' --是否已经成功传入MES接口*/
         AND Wdj.Status_Type = '3'
         AND NOT EXISTS (SELECT 'X'
                FROM Cux.Cux_Wip_Mes_Body Cwmb
               WHERE Cwmb.Wip_Entity_Id = Wdj.Wip_Entity_Id
                 AND Cwmb.Wip_Date IS NOT NULL); --已发放 
    CURSOR C2(p_Wip_Entity_Id IN NUMBER, p_Type IN NUMBER) IS
      SELECT Wo.Operation_Seq_Num, Wo.Operation_Code
        FROM Wip_Operations_v Wo
       WHERE Wo.Wip_Entity_Id = p_Wip_Entity_Id
         AND Wo.Organization_Id = p_Organization_Id
         AND p_Type = 1
         AND Wo.Operation_Code IN ('DB')
      UNION
      SELECT Wo.Operation_Seq_Num, Wo.Operation_Code
        FROM Wip_Operations_v Wo
       WHERE Wo.Wip_Entity_Id = p_Wip_Entity_Id
         AND Wo.Organization_Id = p_Organization_Id
         AND p_Type = 2
         AND Wo.Operation_Code IN ('SMT', 'ZB', 'TS');
    CURSOR C3(p_Wip_Entity_Id IN NUMBER) IS
      SELECT Wro.Operation_Seq_Num
            ,Msi.Segment1
            ,Wro.Required_Quantity
            ,Wro.Quantity_Per_Assembly
            ,Wro.Inventory_Item_Id
            ,Wro.Organization_Id
        FROM Wip_Requirement_Operations Wro, Mtl_System_Items_b Msi
       WHERE Wro.Wip_Entity_Id = p_Wip_Entity_Id
         AND Wro.Organization_Id = p_Organization_Id
         AND Msi.Inventory_Item_Id = Wro.Inventory_Item_Id
         AND Msi.Organization_Id = Wro.Organization_Id;
    CURSOR C4(p_Wip_Entity_Id IN NUMBER) IS
      SELECT a.Operation_Seq_Num
            ,a.Segment1
            ,a.Required_Quantity
            ,a.Quantity_Per_Assembly
            ,a.Inventory_Item_Id
            ,a.Organization_Id
        FROM Cux.Cux_Mes_Bom a
       WHERE Wip_Entity_Id = p_Wip_Entity_Id;
    Doc       Xmldom.Domdocument;
    Et        Xmldom.Domelement;
    Et1       Xmldom.Domelement;
    Et2       Xmldom.Domelement;
    Tempnode  Xmldom.Domnode;
    Tempnode1 Xmldom.Domnode;
    Tempnode4 Xmldom.Domnode;
    Tempnode5 Xmldom.Domnode;
    Tempnode6 Xmldom.Domnode;
    Tempnode2 Xmldom.Domtext;
    Tempnode3 Xmldom.Domnode;
    /*    STR                 VARCHAR2(32764);*/
    Str                 CLOB;
    v_Operation_Code    VARCHAR2(1000);
    v_Required_Quantity NUMBER;
    v_Replacematcode    VARCHAR2(1000);
    v_Workorder         VARCHAR2(1000);
    v_Replacenum        NUMBER;
    v_Seq               NUMBER;
    ------------------------------
    l_Service_Url VARCHAR2(200);
    Req           Utl_Http.Req;
    Resp          Utl_Http.Resp;
    /*    L_XML_CHAR VARCHAR2(32764);*/
    l_Xml_Char  CLOB;
    l_Xml_Char1 VARCHAR2(32764);
    --VALUESS     VARCHAR2(10000);
    l_Replyline VARCHAR2(1000);
    v_3         VARCHAR2(30000);
    v_2         NUMBER;
    /*   V_1                 VARCHAR2(32764);*/
    v_1 CLOB;
    /*    l_rep_str           VARCHAR2(32764);*/
    l_Rep_Str                  CLOB;
    l_Return_Code              VARCHAR2(100);
    l_Retvalue                 VARCHAR2(1000);
    l_Message                  VARCHAR2(1000);
    v_Concurrent_Status        BOOLEAN;
    v_Flag                     VARCHAR2(100);
    v_Substitute_Item_Quantity NUMBER;
    v_Component_Quantity       NUMBER;
    v_Segment1_New             VARCHAR2(100);
    v_Count                    NUMBER;
    l_Clob_Document            CLOB;
    l_Document                 VARCHAR2(30000);
    v_Length                   NUMBER;
    Buffer                     VARCHAR2(30000);
    v_Buffer_Size              BINARY_INTEGER := 1000;
    v_Offset                   INTEGER := 1;
    v_Buffer                   VARCHAR2(30000);
    v_Projectno                VARCHAR2(100);
  BEGIN
    Cux_Mes_Pkg.Create_Bom(p_Wip_Entity_Id => p_Wip_Entity_Id);
  
    Dbms_Lob.Createtemporary(Lob_Loc => l_Clob_Document,
                             Cache   => TRUE,
                             Dur     => Dbms_Lob.Session);
    Dbms_Lob.Open(Lob_Loc   => l_Clob_Document,
                  Open_Mode => Dbms_Lob.Lob_Readwrite);
  
    l_Document := '<STD_IN>';
  
    Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
  
    l_Document := '<EDI_user></EDI_user>';
  
    Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
  
    l_Document := '<MESUSER></MESUSER>';
  
    Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
  
    l_Document := '<EDI_pwd></EDI_pwd>';
  
    Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
  
    l_Document := '<MESUPWD></MESUPWD>';
  
    Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
  
    l_Document := '<sfb_file>';
  
    Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
  
    FOR x IN C1 LOOP
      --FW:  G1511-年+月+日+四位流水号 ，例如：G1511-1409100001
      --JW:  H1511-年+月+日+四位流水号 ，例如：H1511-1409100001
      SELECT Nvl(MAX(a.Seq), 0) + 1
        INTO v_Seq
        FROM Cux.Cux_Wip_Mes_Body a
       WHERE a.Wip_Date = To_Char(SYSDATE, 'YYMMDD');
    
      v_Workorder := x.Organization_Id || '511' || '-' ||
                     To_Char(SYSDATE, 'YYMMDD') || Lpad(v_Seq, 4, '0');
    
      l_Document := '<WORKORDER>' || v_Workorder || '</WORKORDER>';
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<WORKSALENO>' || x.Wip_Entity_Name || '</WORKSALENO>';
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<WORKPLDATE>' ||
                    To_Char(x.Scheduled_Start_Date, 'YYYY/MM/DD') ||
                    '</WORKPLDATE>';
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<WORKPFLDATE>' ||
                    To_Char(x.Scheduled_Completion_Date, 'YYYY/MM/DD') ||
                    '</WORKPFLDATE>'; --计划完工日期
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<WORKPDNDATE>' ||
                    To_Char(x.Requested_Start_Date, 'YYYY/MM/DD') ||
                    '</WORKPDNDATE>'; --计划下达日期
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<WORKMATCODE>' || x.Segment1 || '</WORKMATCODE>'; ---产品编码
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<WORKQTY>' || x.Start_Quantity || '</WORKQTY>'; --数量
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<WORKTYPE>' || x.Description || '</WORKTYPE>'; --工单类型
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<WORKPRIORITY>' || '' || '</WORKPRIORITY>'; --优先级
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<WORKGROUP>' || x.Organization_Id || '</WORKGROUP>'; --工单所属组织
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      --传入G/H
      IF p_Organization_Id = 84 THEN
        v_Flag := 'G'; --FW
      ELSE
        v_Flag := 'H'; --JW
      END IF;
    
      l_Document := '<WORKCHILD>' || v_Flag || '</WORKCHILD>'; --子公司
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<FLAG_CHANGE>' || x.Attribute2 || '</FLAG_CHANGE>'; --是否改版
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<WOMAKER>' || '00' || '</WOMAKER>'; --制造商
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      BEGIN
        SELECT a.Segment1
          INTO v_Projectno
          FROM Wip_Reservations_v a
         WHERE a.Wip_Entity_Id = x.Wip_Entity_Id
           AND a.Segment3 = 'ORDER ENTRY';
      EXCEPTION
        WHEN OTHERS THEN
          v_Projectno := NULL;
      END;
    
      l_Document := '<PROJECTNO>' || v_Projectno || '</PROJECTNO>'; --项目号 --存放销售订单
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<WORKCENTER>' || x.Attribute3 || '</WORKCENTER>'; --ERP抛单车间
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<ecm_files>';
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      FOR y IN C2(x.Wip_Entity_Id, x.Item_Segment1) LOOP
      
        l_Document := '<ecm_file>';
      
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        l_Document := '<WORKORDER>' || v_Workorder || '</WORKORDER>'; --任务号
      
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        l_Document := '<WORKING>' || y.Operation_Code || '</WORKING>'; --工序代码
      
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        l_Document := '<WORKQTY>' || x.Start_Quantity || '</WORKQTY>'; --数量
      
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        l_Document := '</ecm_file>';
      
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      END LOOP;
    
      l_Document := '</ecm_files>';
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      l_Document := '<sfa_files>';
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    
      FOR z IN C4(x.Wip_Entity_Id) LOOP
        --判断是否是替代料
        SELECT COUNT(*)
          INTO v_Count
          FROM Bom_Bill_Of_Materials_v     a
              ,Bom_Inventory_Components_v  b
              ,Bom_Substitute_Components_v c
              ,Mtl_System_Items_b          Msi
              ,Mtl_System_Items_b          Msi1
         WHERE a.Bill_Sequence_Id = b.Bill_Sequence_Id
           AND b.Component_Sequence_Id = c.Component_Sequence_Id
           AND a.Assembly_Item_Id = x.Primary_Item_Id
           AND c.Substitute_Component_Id = z.Inventory_Item_Id
           AND c.Substitute_Component_Id = Msi.Inventory_Item_Id
           AND Msi.Organization_Id = z.Organization_Id
           AND a.Organization_Id = Msi.Organization_Id
           AND b.Component_Item_Id = Msi1.Inventory_Item_Id
           AND Msi1.Organization_Id = z.Organization_Id;
        IF v_Count = 0 THEN
          l_Document := '<sfa_file>';
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<WORKORDER>' || v_Workorder || '</WORKORDER>'; --任务号
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          BEGIN
            SELECT DISTINCT Wo.Operation_Code
              INTO v_Operation_Code
              FROM Wip_Operations_v Wo
             WHERE Wo.Wip_Entity_Id = p_Wip_Entity_Id
               AND Wo.Organization_Id = p_Organization_Id
               AND Wo.Operation_Seq_Num = z.Operation_Seq_Num;
          EXCEPTION
            WHEN OTHERS THEN
              v_Operation_Code := NULL;
          END;
        
          l_Document := '<WORKING>' || v_Operation_Code || '</WORKING>'; --工序
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<JOMMATCODE>' || z.Segment1 || '</JOMMATCODE>'; --物料编码
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<JOMDOSAGE>' || z.Quantity_Per_Assembly ||
                        '</JOMDOSAGE>'; --数量
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<DEVICENO>' || '' || '</DEVICENO>'; --设备
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<FEEDERTYPE>' || '' || '</FEEDERTYPE>'; --FEEDER型号
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<STATIONID>' || '' || '</STATIONID>'; --站号
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<LOCTIONID>' || '' || '</LOCTIONID>'; --位号
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<MATTAG>' || '' || '</MATTAG>'; --物料标识
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<REPLACEMATCODE>' || z.Segment1 ||
                        '</REPLACEMATCODE>'; --被替换物料
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<REPLACENUM>' || '1' || '</REPLACENUM>'; --被替换数量
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          IF z.Required_Quantity = 0 THEN
            l_Document := '<REPLACETAG>' || '0' || '</REPLACETAG>'; --取替代特性
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          ELSE
            SELECT COUNT(*)
              INTO v_Count
              FROM Bom_Bill_Of_Materials_v     a
                  ,Bom_Inventory_Components_v  b
                  ,Bom_Substitute_Components_v c
             WHERE a.Bill_Sequence_Id = b.Bill_Sequence_Id
               AND b.Component_Sequence_Id = c.Component_Sequence_Id
               AND a.Assembly_Item_Id = x.Primary_Item_Id
               AND b.Component_Item_Id = z.Inventory_Item_Id
               AND c.Substitute_Component_Id IN
                   (SELECT d.Inventory_Item_Id
                      FROM Cux.Cux_Mes_Bom d
                     WHERE d.Wip_Entity_Id = p_Wip_Entity_Id)
               AND a.Organization_Id = z.Organization_Id;
            IF v_Count > 0 THEN
              l_Document := '<REPLACETAG>' || '1' || '</REPLACETAG>'; --取替代特性
            
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            ELSE
              l_Document := '<REPLACETAG>' || '0' || '</REPLACETAG>'; --取替代特性
            
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            END IF;
          
          END IF;
        
          l_Document := '<REPLACETYPE>' || '' || '</REPLACETYPE>'; --取替代特性
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<FBOMCONTAG>' || '1' || '</FBOMCONTAG>'; ---部件跟踪标志
        
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          IF v_Operation_Code = 'SMT' THEN
            l_Document := '<FBOMMATTAG>' || '0' || '</FBOMMATTAG>'; --配置标志
          ELSE
            l_Document := '<FBOMMATTAG>' || '5' || '</FBOMMATTAG>'; --配置标志
          END IF;
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<FBOMSTRUCTAG>' || '0' || '</FBOMSTRUCTAG>'; --结构标志
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          l_Document := '<MATNUMBER>' || z.Required_Quantity ||
                        '</MATNUMBER>'; --结构标志
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        
          SELECT COUNT(*)
            INTO v_Count
            FROM Fnd_Lookup_Values a
           WHERE a.Lookup_Type = 'RJ_WIP_ISSUE_INVENTORY'
             AND a.Description = z.Segment1
             AND a.Attribute1 = z.Organization_Id
             AND a.Attribute2 =
                 Decode(z.Organization_Id, 84, 'G109', 'H109');
          IF v_Count > 0 THEN
            SELECT COUNT(*)
              INTO v_Count
              FROM Mtl_System_Items_b Msi
             WHERE Msi.Organization_Id = z.Organization_Id
               AND Msi.Inventory_Item_Id = z.Inventory_Item_Id
               AND Msi.Attribute7 IN ('C044',
                                      'C048',
                                      'C049',
                                      'C053',
                                      'C054',
                                      'C059',
                                      'C060',
                                      'C061',
                                      'C062',
                                      'C090',
                                      'C091',
                                      'C092');
            IF v_Count > 0 THEN
              l_Document := '<SELECTTAG>' || 'Y' || '</SELECTTAG>'; --挑料标志
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            ELSE
              l_Document := '<SELECTTAG>' || 'N' || '</SELECTTAG>'; --挑料标志
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            END IF;
          ELSE
            l_Document := '<SELECTTAG>' || 'N' || '</SELECTTAG>'; --挑料标志
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          END IF;
        
          l_Document := '</sfa_file>';
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        ELSE
          --获取替代
          FOR Z1 IN (SELECT Msi1.Segment1, b.Component_Yield_Factor
                       FROM Bom_Bill_Of_Materials_v     a
                           ,Bom_Inventory_Components_v  b
                           ,Bom_Substitute_Components_v c
                           ,Mtl_System_Items_b          Msi
                           ,Mtl_System_Items_b          Msi1
                      WHERE a.Bill_Sequence_Id = b.Bill_Sequence_Id
                        AND b.Component_Sequence_Id =
                            c.Component_Sequence_Id
                        AND a.Assembly_Item_Id = x.Primary_Item_Id
                        AND c.Substitute_Component_Id = z.Inventory_Item_Id
                        AND c.Substitute_Component_Id =
                            Msi.Inventory_Item_Id
                        AND Msi.Organization_Id = z.Organization_Id
                        AND a.Organization_Id = Msi.Organization_Id
                        AND b.Component_Item_Id = Msi1.Inventory_Item_Id
                        AND Msi1.Organization_Id = z.Organization_Id) LOOP
            l_Document := '<sfa_file>';
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<WORKORDER>' || v_Workorder || '</WORKORDER>'; --任务号
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            BEGIN
              SELECT DISTINCT Wo.Operation_Code
                INTO v_Operation_Code
                FROM Wip_Operations_v Wo
               WHERE Wo.Wip_Entity_Id = p_Wip_Entity_Id
                 AND Wo.Organization_Id = p_Organization_Id
                 AND Wo.Operation_Seq_Num = z.Operation_Seq_Num;
            EXCEPTION
              WHEN OTHERS THEN
                v_Operation_Code := NULL;
            END;
          
            l_Document := '<WORKING>' || v_Operation_Code || '</WORKING>'; --工序
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<JOMMATCODE>' || z.Segment1 || '</JOMMATCODE>'; --物料编码
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<JOMDOSAGE>' || z.Quantity_Per_Assembly ||
                          '</JOMDOSAGE>'; --数量
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<DEVICENO>' || '' || '</DEVICENO>'; --设备
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<FEEDERTYPE>' || '' || '</FEEDERTYPE>'; --FEEDER型号
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<STATIONID>' || '' || '</STATIONID>'; --站号
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<LOCTIONID>' || '' || '</LOCTIONID>'; --位号
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<MATTAG>' || '' || '</MATTAG>'; --物料标识
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<REPLACEMATCODE>' || Z1.Segment1 ||
                          '</REPLACEMATCODE>'; --被替换物料
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<REPLACENUM>' || z.Quantity_Per_Assembly *
                          Z1.Component_Yield_Factor || '</REPLACENUM>'; --被替换数量
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<REPLACETAG>' || '1' || '</REPLACETAG>'; --取替代特性
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<REPLACETYPE>' || '' || '</REPLACETYPE>'; --取替代特性
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<FBOMCONTAG>' || '1' || '</FBOMCONTAG>'; ---部件跟踪标志
          
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            IF v_Operation_Code = 'SMT' THEN
              l_Document := '<FBOMMATTAG>' || '0' || '</FBOMMATTAG>'; --配置标志
            ELSE
              l_Document := '<FBOMMATTAG>' || '5' || '</FBOMMATTAG>'; --配置标志
            END IF;
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<FBOMSTRUCTAG>' || '0' || '</FBOMSTRUCTAG>'; --结构标志
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<MATNUMBER>' || z.Required_Quantity ||
                          '</MATNUMBER>'; --结构标志
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            SELECT COUNT(*)
              INTO v_Count
              FROM Fnd_Lookup_Values a
             WHERE a.Lookup_Type = 'RJ_WIP_ISSUE_INVENTORY'
               AND a.Description = z.Segment1
               AND a.Attribute1 = z.Organization_Id
               AND a.Attribute2 =
                   Decode(z.Organization_Id, 84, 'G109', 'H109');
            IF v_Count > 0 THEN
              SELECT COUNT(*)
                INTO v_Count
                FROM Mtl_System_Items_b Msi
               WHERE Msi.Organization_Id = z.Organization_Id
                 AND Msi.Inventory_Item_Id = z.Inventory_Item_Id
                 AND Msi.Attribute7 IN ('C044',
                                        'C048',
                                        'C049',
                                        'C053',
                                        'C054',
                                        'C059',
                                        'C060',
                                        'C061',
                                        'C062',
                                        'C090',
                                        'C091',
                                        'C092');
              IF v_Count > 0 THEN
                l_Document := '<SELECTTAG>' || 'Y' || '</SELECTTAG>'; --挑料标志
                Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
              ELSE
                l_Document := '<SELECTTAG>' || 'N' || '</SELECTTAG>'; --挑料标志
                Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
              END IF;
            ELSE
              l_Document := '<SELECTTAG>' || 'N' || '</SELECTTAG>'; --挑料标志
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            END IF;
            l_Document := '</sfa_file>';
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          END LOOP;
        
        END IF;
      
      END LOOP;
    
      /*    
      FOR Z IN C3(X.WIP_ENTITY_ID) LOOP
      
        L_DOCUMENT := '<sfa_file>';
      
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        L_DOCUMENT := '<WORKORDER>' || V_WORKORDER || '</WORKORDER>'; --任务号
      
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        BEGIN
          SELECT DISTINCT WO.OPERATION_CODE
            INTO V_OPERATION_CODE
            FROM WIP_OPERATIONS_V WO
           WHERE WO.WIP_ENTITY_ID = P_WIP_ENTITY_ID
             AND WO.ORGANIZATION_ID = P_ORGANIZATION_ID
             AND WO.OPERATION_SEQ_NUM = Z.OPERATION_SEQ_NUM;
        EXCEPTION
          WHEN OTHERS THEN
            V_OPERATION_CODE := NULL;
        END;
      
        L_DOCUMENT := '<WORKING>' || V_OPERATION_CODE || '</WORKING>'; --工序
      
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        L_DOCUMENT := '<JOMMATCODE>' || z.segment1 || '</JOMMATCODE>'; --物料编码
      
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        L_DOCUMENT := '<JOMDOSAGE>' || z.quantity_per_assembly ||
                      '</JOMDOSAGE>'; --数量
      
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        L_DOCUMENT := '<DEVICENO>' || '' || '</DEVICENO>'; --设备
      
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        L_DOCUMENT := '<FEEDERTYPE>' || '' || '</FEEDERTYPE>'; --FEEDER型号
      
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        L_DOCUMENT := '<STATIONID>' || '' || '</STATIONID>'; --站号
      
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        L_DOCUMENT := '<LOCTIONID>' || '' || '</LOCTIONID>'; --位号
      
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        L_DOCUMENT := '<MATTAG>' || '' || '</MATTAG>'; --物料标识
      
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        V_REPLACEMATCODE := NULL;
        V_REPLACENUM     := NULL;
        \*        FOR W IN (SELECT C.SUBSTITUTE_COMPONENT_ID,
                         MSI.SEGMENT1,
                         MSI.DESCRIPTION
                    FROM BOM_BILL_OF_MATERIALS_V     A,
                         BOM_INVENTORY_COMPONENTS_V  B,
                         BOM_SUBSTITUTE_COMPONENTS_V C,
                         MTL_SYSTEM_ITEMS_B          MSI
                   WHERE A.BILL_SEQUENCE_ID = B.BILL_SEQUENCE_ID
                     AND B.COMPONENT_SEQUENCE_ID = C.COMPONENT_SEQUENCE_ID
                     AND A.ASSEMBLY_ITEM_ID = X.PRIMARY_ITEM_ID
                     AND B.COMPONENT_ITEM_ID = Z.INVENTORY_ITEM_ID
                     AND C.SUBSTITUTE_COMPONENT_ID = MSI.INVENTORY_ITEM_ID
                     AND MSI.ORGANIZATION_ID = Z.ORGANIZATION_ID
                     AND A.ORGANIZATION_ID = MSI.ORGANIZATION_ID) LOOP
          --获取替代料数量
        BEGIN  
          SELECT NVL(SUM(WRO.REQUIRED_QUANTITY), 0)
            INTO V_REQUIRED_QUANTITY
            FROM WIP_REQUIREMENT_OPERATIONS WRO, MTL_SYSTEM_ITEMS_B MSI
           WHERE WRO.WIP_ENTITY_ID = P_WIP_ENTITY_ID
             AND WRO.ORGANIZATION_ID = P_ORGANIZATION_ID
             AND MSI.INVENTORY_ITEM_ID = WRO.INVENTORY_ITEM_ID
             AND MSI.ORGANIZATION_ID = WRO.ORGANIZATION_ID
             AND MSI.INVENTORY_ITEM_ID = W.SUBSTITUTE_COMPONENT_ID;
         EXCEPTION
             WHEN OTHERS THEN
                 V_REQUIRED_QUANTITY:=0;  
             END;       
          IF V_REQUIRED_QUANTITY <> 0 THEN
            V_REPLACEMATCODE := W.SEGMENT1;
            V_REPLACENUM     := V_REQUIRED_QUANTITY /
                                (V_REQUIRED_QUANTITY + Z.REQUIRED_QUANTITY) * 100;
            EXIT;
          END IF;
        END LOOP;*\
      
        SELECT COUNT(*)
          INTO V_COUNT
          FROM BOM_BILL_OF_MATERIALS_V     A,
               BOM_INVENTORY_COMPONENTS_V  B,
               BOM_SUBSTITUTE_COMPONENTS_V C,
               MTL_SYSTEM_ITEMS_B          MSI,
               WIP_REQUIREMENT_OPERATIONS  WRO
         WHERE A.BILL_SEQUENCE_ID = B.BILL_SEQUENCE_ID
           AND B.COMPONENT_SEQUENCE_ID = C.COMPONENT_SEQUENCE_ID
           AND A.ASSEMBLY_ITEM_ID = X.PRIMARY_ITEM_ID
           AND C.SUBSTITUTE_COMPONENT_ID = Z.INVENTORY_ITEM_ID
           AND C.SUBSTITUTE_COMPONENT_ID = MSI.INVENTORY_ITEM_ID
           AND MSI.ORGANIZATION_ID = Z.ORGANIZATION_ID
           AND A.ORGANIZATION_ID = MSI.ORGANIZATION_ID
           AND WRO.WIP_ENTITY_ID = P_WIP_ENTITY_ID
           AND WRO.INVENTORY_ITEM_ID = B.COMPONENT_ITEM_ID;
      
        IF V_COUNT = 0 THEN
          --无替代料              
        
          L_DOCUMENT := '<REPLACEMATCODE>' || Z.SEGMENT1 ||
                        '</REPLACEMATCODE>'; --被替换物料
        
          WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
        
          L_DOCUMENT := '<REPLACENUM>' || '1' || '</REPLACENUM>'; --被替换数量
        
          WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
        
          L_DOCUMENT := '<REPLACETAG>' || '0' || '</REPLACETAG>'; --取替代特性
        
          WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
        ELSE
          SELECT B.COMPONENT_QUANTITY,
                 C.SUBSTITUTE_ITEM_QUANTITY,
                 MSI1.SEGMENT1
            INTO V_COMPONENT_QUANTITY,
                 V_SUBSTITUTE_ITEM_QUANTITY,
                 V_SEGMENT1_NEW
            FROM BOM_BILL_OF_MATERIALS_V     A,
                 BOM_INVENTORY_COMPONENTS_V  B,
                 BOM_SUBSTITUTE_COMPONENTS_V C,
                 MTL_SYSTEM_ITEMS_B          MSI,
                 MTL_SYSTEM_ITEMS_B          MSI1,
                 WIP_REQUIREMENT_OPERATIONS  WRO
           WHERE A.BILL_SEQUENCE_ID = B.BILL_SEQUENCE_ID
             AND B.COMPONENT_SEQUENCE_ID = C.COMPONENT_SEQUENCE_ID
             AND A.ASSEMBLY_ITEM_ID = X.PRIMARY_ITEM_ID
             AND C.SUBSTITUTE_COMPONENT_ID = Z.INVENTORY_ITEM_ID
             AND C.SUBSTITUTE_COMPONENT_ID = MSI.INVENTORY_ITEM_ID
             AND MSI.ORGANIZATION_ID = Z.ORGANIZATION_ID
             AND A.ORGANIZATION_ID = MSI.ORGANIZATION_ID
             AND WRO.WIP_ENTITY_ID = P_WIP_ENTITY_ID
             AND WRO.INVENTORY_ITEM_ID = MSI1.INVENTORY_ITEM_ID
             AND MSI1.ORGANIZATION_ID = Z.ORGANIZATION_ID
             AND WRO.INVENTORY_ITEM_ID = B.COMPONENT_ITEM_ID;
        
          L_DOCUMENT := '<REPLACEMATCODE>' || V_SEGMENT1_NEW ||
                        '</REPLACEMATCODE>'; --被替换物料
        
          WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
        
          L_DOCUMENT := '<REPLACENUM>' ||
                        V_SUBSTITUTE_ITEM_QUANTITY / V_component_quantity ||
                        '</REPLACENUM>'; --被替换数量
        
          WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
        
          L_DOCUMENT := '<REPLACETAG>' || '1' || '</REPLACETAG>'; --取替代特性
        
          WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
        
        END IF;
      
        L_DOCUMENT := '<REPLACETYPE>' || '' || '</REPLACETYPE>'; --取替代特性
      
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        L_DOCUMENT := '<FBOMCONTAG>' || '1' || '</FBOMCONTAG>'; ---部件跟踪标志
      
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        IF V_OPERATION_CODE = 'SMT' THEN
          L_DOCUMENT := '<FBOMMATTAG>' || '0' || '</FBOMMATTAG>'; --配置标志
        ELSE
          L_DOCUMENT := '<FBOMMATTAG>' || '5' || '</FBOMMATTAG>'; --配置标志
        END IF;
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        L_DOCUMENT := '<FBOMSTRUCTAG>' || '0' || '</FBOMSTRUCTAG>'; --结构标志
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        L_DOCUMENT := '<MATNUMBER>' || z.required_quantity ||
                      '</MATNUMBER>'; --结构标志
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      
        L_DOCUMENT := '<SELECTTAG>' || 'N' || '</SELECTTAG>'; --挑料标志
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
        L_DOCUMENT := '</sfa_file>';
        WF_NOTIFICATION.WRITETOCLOB(L_CLOB_DOCUMENT, L_DOCUMENT);
      END LOOP;*/
      l_Document := '</sfa_files>';
    
      Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
    END LOOP;
    l_Document := '</sfb_file>';
  
    Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
  
    l_Document := '</STD_IN>';
  
    Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
  
    Dbms_Lob.Createtemporary(Lob_Loc => Str,
                             Cache   => TRUE,
                             Dur     => Dbms_Lob.Session);
    Dbms_Lob.Append(Str, l_Clob_Document);
  
    IF Str IS NOT NULL THEN
      INSERT INTO Cux.Cux_Wip_Mes_Body
        (Wip_Entity_Id, Body_Clob, Wip_Date, Seq)
      VALUES
        (p_Wip_Entity_Id, Str, To_Char(SYSDATE, 'YYMMDD'), v_Seq);
      COMMIT;
    
      v_1 := '<![CDATA[' || Str || ']]>';
    
      ---------------------------------------开始调用Webservice-------------
    
      --传入参数
      l_Xml_Char := '<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <GenerateWorkOrderNew xmlns="http://tempuri.org/">
      <str_workorder>' || v_1 || '</str_workorder>
    </GenerateWorkOrderNew>
  </soap:Body>
</soap:Envelope>';
    
      /* insert into xx_aaa values(l_xml_char);*/
      ---业务测试 http://192.168.9.54/MESServiceyw/service.asmx
      ---信息中心测试：http://192.168.9.54/MESServiceXXZX/service.asmx 
      --l_service_url := 'http://10.18.255.36/MESService/Service.asmx'; --生产环境
      --l_service_url := 'http://192.168.9.54/MESServiceyw/service.asmx';--测试环境
      l_Service_Url := Cux_External_Interface_Pkg.Getinterfaceurl('MES');
    
      Req := Utl_Http.Begin_Request(l_Service_Url,
                                    'POST',
                                    Utl_Http.Http_Version_1_1);
      -- 保持连接状态
      Utl_Http.Set_Persistent_Conn_Support(Req, TRUE);
    
      --设置编码
    
      /*      utl_http.set_header(req, 'Content-Type', 'text/xml; charset=utf-8');*/
    
      Utl_Http.Set_Header(Req, 'Content-Type', 'text/xml; charset=utf-8');
    
      Utl_Http.Set_Header(Req,
                          'SOAPAction',
                          '"http://tempuri.org/GenerateWorkOrderNew"');
    
      Utl_Http.Set_Body_Charset(Req, 'utf-8');
    
      Utl_Http.Set_Header(Req,
                          'Content-Length',
                          Dbms_Lob.Getlength(l_Xml_Char));
    
      Utl_Http.Write_Raw(Req,
                         Utl_Raw.Cast_To_Raw('<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <GenerateWorkOrderNew xmlns="http://tempuri.org/">
      <str_workorder>'));
    
      v_Length := Dbms_Lob.Getlength(v_1);
    
      WHILE v_Offset < v_Length LOOP
        Dbms_Lob.Read(v_1, v_Buffer_Size, v_Offset, v_Buffer);
        /*        utl_http.write_raw(req,utl_raw.cast_to_raw(v_buffer)); */
        /*        select replace(v_buffer, '&lt;', null)
        into v_buffer
        from dual; */
        Utl_Http.Write_Raw(Req, Utl_Raw.Cast_To_Raw(v_Buffer));
        v_Offset := v_Offset + v_Buffer_Size;
      END LOOP;
    
      Utl_Http.Write_Raw(Req,
                         Utl_Raw.Cast_To_Raw('</str_workorder>
    </GenerateWorkOrderNew>
  </soap:Body>
</soap:Envelope>'));
    
      /*    utl_http.write_line(req, l_xml_char);*/
    
      /*   select a1
      into v_a1
      from xx_aaa;*/
    
      /*    utl_http.write_text(req, l_xml_char);*/
      /*      utl_http.write_raw(v_a1);*/
    
      Resp := Utl_Http.Get_Response(Req);
      Dbms_Output.Put_Line('this request status is:' || Resp.Status_Code);
      Fnd_File.Put_Line(Fnd_File.Log, Resp.Status_Code);
      Dbms_Output.Put_Line('this request status is:' || Utl_Http.Http_Ok);
    
      IF (Resp.Status_Code = Utl_Http.Http_Ok) THEN
      
        BEGIN
        
          LOOP
            Utl_Http.Read_Text(Resp, l_Replyline);
            Dbms_Output.Put_Line(l_Replyline);
            Fnd_File.Put_Line(Fnd_File.Log, l_Replyline);
            l_Rep_Str := l_Rep_Str || l_Replyline;
          END LOOP;
          Utl_Http.End_Response(Resp);
        
        EXCEPTION
          WHEN Utl_Http.End_Of_Body THEN
            Utl_Http.End_Response(Resp);
        END;
      END IF;
      SELECT REPLACE(l_Rep_Str, '“&lt;”', NULL) INTO l_Rep_Str FROM Dual;
    
      SELECT REPLACE(l_Rep_Str, '&lt;', '<') INTO l_Rep_Str FROM Dual;
    
      SELECT REPLACE(l_Rep_Str, '&gt;', '>') INTO l_Rep_Str FROM Dual;
    
      ---解析返回结果
      Parser_Resp(l_Rep_Str, l_Retvalue, l_Message);
      IF l_Retvalue = '0' THEN
        UPDATE Wip_Discrete_Jobs Wdj
           SET Wdj.Attribute15 = 'Y'
         WHERE Wdj.Wip_Entity_Id = p_Wip_Entity_Id;
        COMMIT;
        Html_Report_Pkg.v_Report_Output_Mode := 'F';
        Html_Report_Pkg.Output_Line('<BR>' || '导入成功' || l_Message ||
                                    '</BR>');
      ELSE
        Html_Report_Pkg.v_Report_Output_Mode := 'F';
        Html_Report_Pkg.Output_Line('<BR>' || '传入MES接口错误,原因' || l_Message ||
                                    '</BR>');
        /*V_CONCURRENT_STATUS := FND_CONCURRENT.SET_COMPLETION_STATUS('WARNING',
        '传入MES接口错误!');*/
      END IF;
    ELSE
      Html_Report_Pkg.v_Report_Output_Mode := 'F';
      Html_Report_Pkg.Output_Line('<BR>' || '工单不满足传MES条件,工单状态必须是发放且未传入MES' ||
                                  '</BR>');
      /*      V_CONCURRENT_STATUS := FND_CONCURRENT.SET_COMPLETION_STATUS('WARNING',
      '工单不满足传MES条件!');*/
    END IF;
  END;

  PROCEDURE E2m_Job(Errbuf            OUT NOCOPY VARCHAR2,
                    Retcode           OUT NOCOPY VARCHAR2,
                    p_Organization_Id IN NUMBER,
                    p_Wip_Entity_Name IN VARCHAR2) AS
    CURSOR C1 IS
      SELECT Wdj.Wip_Entity_Id
            ,Wdj.Scheduled_Start_Date
            ,Wdj.Scheduled_Completion_Date
            ,Wdj.Scheduled_Start_Date Requested_Start_Date
            ,We.Wip_Entity_Name
            ,We.Primary_Item_Id
            ,We.Organization_Id
            ,Decode(We.Organization_Id, 84, 'G1', 'H1') Organization_Shot
            ,Wac.Class_Code Description
            ,Wdj.Attribute3 ---车间
            ,Decode(Wac.Class_Code, 4, 'Y', 'N') Attribute2
            ,Wdj.Start_Quantity
            ,Msi.Segment1
            ,Decode((SELECT Mic.Segment1
                      FROM Mtl_Item_Categories_v Mic
                     WHERE Mic.Organization_Id = Msi.Organization_Id
                       AND Mic.Structure_Id = 101 --物料类别
                       AND Mic.Inventory_Item_Id = Msi.Inventory_Item_Id),
                    '01',
                    1,
                    2) Item_Segment1
        FROM Wip_Discrete_Jobs      Wdj
            ,Wip_Accounting_Classes Wac
            ,Wip_Entities           We
            ,Mtl_System_Items_b     Msi
       WHERE 1 = 1
         AND Msi.Organization_Id = We.Organization_Id
         AND Msi.Inventory_Item_Id = We.Primary_Item_Id
         AND Wdj.Wip_Entity_Id = We.Wip_Entity_Id
         AND Wac.Organization_Id = Wdj.Organization_Id
         AND Wdj.Class_Code IN
             (SELECT Flv.Lookup_Code
                FROM Fnd_Lookup_Values Flv
               WHERE Flv.Lookup_Type = 'CUX_WIP_2_MES_INCLUDED_CLASS'
                 AND Flv.Enabled_Flag = 'Y'
                 AND Trunc(SYSDATE) BETWEEN
                     Nvl(Flv.Start_Date_Active, Trunc(SYSDATE)) AND
                     Nvl(Flv.End_Date_Active, Trunc(SYSDATE))
                 AND Flv.Language = 'US')
         AND Wdj.Attribute3 IN
             (SELECT Flv.Lookup_Code
                FROM Fnd_Lookup_Values Flv
               WHERE Flv.Lookup_Type = 'CUX_WIP_2_MES_INCLUDED_CLASS1'
                 AND Flv.Enabled_Flag = 'Y'
                 AND Trunc(SYSDATE) BETWEEN
                     Nvl(Flv.Start_Date_Active, Trunc(SYSDATE)) AND
                     Nvl(Flv.End_Date_Active, Trunc(SYSDATE))
                 AND Flv.Language = 'US')
         -- 临时不过滤 V-080  物料  
         --AND Msi.Segment1 NOT LIKE 'V-080%'
         AND Wac.Class_Code = Wdj.Class_Code
         AND Nvl(Wdj.Attribute15, 'N') = 'N' --是否已经成功传入MES接口
         AND Wdj.Status_Type = '3' --已发放
         AND Wdj.Organization_Id =
             Nvl(p_Organization_Id, Wdj.Organization_Id)
         AND We.Wip_Entity_Name =
             Nvl(p_Wip_Entity_Name, We.Wip_Entity_Name);
  
    CURSOR C2(p_Wip_Entity_Id IN NUMBER, p_Type IN NUMBER) IS
      SELECT Wo.Operation_Seq_Num, Wo.Operation_Code
        FROM Wip_Operations_v Wo
       WHERE Wo.Wip_Entity_Id = p_Wip_Entity_Id
         AND p_Type = 1
         AND Wo.Operation_Code IN ('DB')
      UNION
      SELECT Wo.Operation_Seq_Num, Wo.Operation_Code
        FROM Wip_Operations_v Wo
       WHERE Wo.Wip_Entity_Id = p_Wip_Entity_Id
         AND p_Type = 2
         AND Wo.Operation_Code IN ('SMT', 'ZB', 'TS');
  
    /*CURSOR C3(p_Wip_Entity_Id IN NUMBER) IS
    SELECT Wro.Operation_Seq_Num
          ,Msi.Segment1
          ,Wro.Required_Quantity
          ,Wro.Quantity_Per_Assembly
          ,Wro.Inventory_Item_Id
          ,Wro.Organization_Id
      FROM Wip_Requirement_Operations Wro, Mtl_System_Items_b Msi
     WHERE Wro.Wip_Entity_Id = p_Wip_Entity_Id
       AND Msi.Inventory_Item_Id = Wro.Inventory_Item_Id
       AND Msi.Organization_Id = Wro.Organization_Id;*/
  
    CURSOR C4(p_Wip_Entity_Id IN NUMBER) IS
      SELECT a.Operation_Seq_Num
            ,a.Segment1
            ,a.Required_Quantity
            ,a.Quantity_Per_Assembly
            ,a.Inventory_Item_Id
            ,a.Organization_Id
        FROM Cux.Cux_Mes_Bom a
       WHERE a.Wip_Entity_Id = p_Wip_Entity_Id
         AND a.Quantity_Per_Assembly <> 0; -- 排除单机用量为0 ,即完全替代不需要传递过去
  
    Doc                 Xmldom.Domdocument;
    Et                  Xmldom.Domelement;
    Et1                 Xmldom.Domelement;
    Et2                 Xmldom.Domelement;
    Tempnode            Xmldom.Domnode;
    Tempnode1           Xmldom.Domnode;
    Tempnode4           Xmldom.Domnode;
    Tempnode5           Xmldom.Domnode;
    Tempnode6           Xmldom.Domnode;
    Tempnode2           Xmldom.Domtext;
    Tempnode3           Xmldom.Domnode;
    Str                 CLOB;
    v_Operation_Code    VARCHAR2(1000);
    v_Required_Quantity NUMBER;
    v_Replacematcode    VARCHAR2(1000);
    v_Workorder         VARCHAR2(1000);
    v_Replacenum        NUMBER;
    v_Seq               NUMBER;
    ------------------------------
    Req                        Utl_Http.Req;
    Resp                       Utl_Http.Resp;
    l_Xml_Char                 CLOB;
    l_Xml_Char1                VARCHAR2(32764);
    l_Replyline                VARCHAR2(1000);
    l_Rep_Str                  CLOB;
    l_Return_Code              VARCHAR2(100);
    l_Retvalue                 VARCHAR2(1000);
    l_Message                  VARCHAR2(1000);
    v_Concurrent_Status        BOOLEAN;
    v_Flag                     VARCHAR2(100);
    v_Substitute_Item_Quantity NUMBER;
    v_Component_Quantity       NUMBER;
    v_Segment1_New             VARCHAR2(100);
    v_Count                    NUMBER;
    l_Clob_Document            CLOB;
    l_Document                 VARCHAR2(30000);
    v_Length                   NUMBER;
    Buffer                     VARCHAR2(30000);
    v_Buffer_Size              BINARY_INTEGER := 1000;
    v_Offset                   INTEGER := 1;
    v_Buffer                   VARCHAR2(30000);
    v_Projectno                VARCHAR2(100);
  
    l_Request             Cux_Soa_Requests%ROWTYPE;
    l_Request_Id          NUMBER;
    l_Service_Url         VARCHAR2(200);
    l_Service_Content     CLOB;
    l_Soa_Method          VARCHAR2(50);
    l_Resp_Parameter_Name VARCHAR2(50);
    l_Content_Ret_Sts     VARCHAR2(10);
    l_Content_Error_Msg   VARCHAR2(500);
  
    l_Receipt_Number     VARCHAR2(40);
    l_Receipt_Info       CLOB;
    l_Response_Content   CLOB;
    l_Responsed_Msg      CLOB;
    l_Destination_System VARCHAR2(100) := 'MES';
  
    l_Ret_Sts   VARCHAR2(100);
    l_Error_Msg VARCHAR2(4000);
  
    l_Parser_Ret_Sts        VARCHAR2(100);
    l_Parser_Error_Msg      VARCHAR2(4000);
    l_Parser_Log            CLOB;
    l_Parser_Return_Value_1 VARCHAR2(1000);
    l_Parser_Return_Value_2 VARCHAR2(2000);
  
    l_Process_Number NUMBER;
  
  BEGIN
    --FW:  G1511-年+月+日+四位流水号 ，例如：G1511-1409100001
    --JW:  H1511-年+月+日+四位流水号 ，例如：H1511-1409100001
    /* SELECT Nvl(MAX(a.Seq), 0) + 1
     INTO v_Seq
     FROM Cux.Cux_Wip_Mes_Body a
    WHERE a.Wip_Date = To_Char(SYSDATE, 'YYMMDD');  */
    BEGIN
      SELECT COUNT(1)
        INTO v_Seq
        FROM Cux.Cux_Soa_Requests t
       WHERE t.Interface = 'CUX_MES_PKG'
         AND t.Method = 'E2M_JOB'
         AND Trunc(t.Request_Date) = Trunc(SYSDATE);
    END;
  
    l_Process_Number := 0;
  
    -- 工单
    FOR x IN C1 LOOP
      v_Workorder := Cux_Wip2wms_Transfer_Pkg.Get_Mes_Job_Number(x.Wip_Entity_Id);
    
      IF v_Workorder IS NULL THEN
        Retcode := 1;
        Html_Report_Pkg.v_Report_Output_Mode := 'F';
        Html_Report_Pkg.Output_Line('<table><tr><td width=100>工单</td><td width=100>状态</td><td width=300>信息</td></tr>');
        Html_Report_Pkg.Output_Line('<tr><td>' || x.Wip_Entity_Name ||
                                    '</td>');
        Html_Report_Pkg.Output_Line('<td>E</td>');
        Html_Report_Pkg.Output_Line('<td>获取MES工单号失败</td></tr></table>');
      ELSE
        --  
        l_Process_Number := l_Process_Number + 1;
        v_Seq            := v_Seq + 1;
        l_Request        := NULL;
      
        Cux_Mes_Pkg.Create_Bom(p_Wip_Entity_Id => x.Wip_Entity_Id);
        Dbms_Lob.Createtemporary(Lob_Loc => l_Clob_Document,
                                 Cache   => TRUE,
                                 Dur     => Dbms_Lob.Session);
        Dbms_Lob.Open(Lob_Loc   => l_Clob_Document,
                      Open_Mode => Dbms_Lob.Lob_Readwrite);
        l_Document := '<STD_IN>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<EDI_user></EDI_user>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<MESUSER></MESUSER>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<EDI_pwd></EDI_pwd>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<MESUPWD></MESUPWD>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<sfb_file>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        l_Document := '<WORKORDER>' || v_Workorder || '</WORKORDER>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<WORKSALENO>' || x.Wip_Entity_Name ||
                      '</WORKSALENO>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<WORKPLDATE>' ||
                      To_Char(x.Scheduled_Start_Date, 'YYYY/MM/DD') ||
                      '</WORKPLDATE>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<WORKPFLDATE>' ||
                      To_Char(x.Scheduled_Completion_Date, 'YYYY/MM/DD') ||
                      '</WORKPFLDATE>'; --计划完工日期    
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<WORKPDNDATE>' ||
                      To_Char(x.Requested_Start_Date, 'YYYY/MM/DD') ||
                      '</WORKPDNDATE>'; --计划下达日期    
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<WORKMATCODE>' || x.Segment1 || '</WORKMATCODE>'; ---产品编码
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<WORKQTY>' || x.Start_Quantity || '</WORKQTY>'; --数量    
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<WORKTYPE>' || x.Description || '</WORKTYPE>'; --工单类型    
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<WORKPRIORITY>' || '' || '</WORKPRIORITY>'; --优先级    
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<WORKGROUP>' || x.Organization_Shot ||
                      '</WORKGROUP>'; --工单所属组织
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        --传入G/H
        IF x.Organization_Id = 84 THEN
          v_Flag := 'G'; --FW
        ELSE
          v_Flag := 'H'; --JW
        END IF;
      
        l_Document := '<WORKCHILD>' || v_Flag || '</WORKCHILD>'; --子公司    
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<FLAG_CHANGE>' || x.Attribute2 || '</FLAG_CHANGE>'; --是否改版
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<WOMAKER>' || '00' || '</WOMAKER>'; --制造商    
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        BEGIN
          SELECT a.Segment1
            INTO v_Projectno
            FROM Wip_Reservations_v a
           WHERE a.Wip_Entity_Id = x.Wip_Entity_Id
             AND a.Segment3 = 'ORDER ENTRY';
        EXCEPTION
          WHEN OTHERS THEN
            v_Projectno := NULL;
        END;
      
        l_Document := '<PROJECTNO>' || v_Projectno || '</PROJECTNO>'; --项目号 --存放销售订单
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        l_Document := '<WORKCENTER>' || x.Attribute3 || '</WORKCENTER>'; --ERP抛单车间
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        l_Document := '<ecm_files>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        FOR y IN C2(x.Wip_Entity_Id, x.Item_Segment1) LOOP
          l_Document := '<ecm_file>';
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          l_Document := '<WORKORDER>' || v_Workorder || '</WORKORDER>'; --任务号      
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          l_Document := '<WORKING>' || y.Operation_Code || '</WORKING>'; --工序代码      
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          l_Document := '<WORKQTY>' || x.Start_Quantity || '</WORKQTY>'; --数量      
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          l_Document := '</ecm_file>';
          Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        END LOOP;
      
        l_Document := '</ecm_files>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '<sfa_files>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        FOR z IN C4(x.Wip_Entity_Id) LOOP
          --判断是否是替代料
          SELECT COUNT(*)
            INTO v_Count
            FROM Bom_Bill_Of_Materials_v     a
                ,Bom_Inventory_Components_v  b
                ,Bom_Substitute_Components_v c
                ,Mtl_System_Items_b          Msi
                ,Mtl_System_Items_b          Msi1
           WHERE a.Bill_Sequence_Id = b.Bill_Sequence_Id
             AND b.Component_Sequence_Id = c.Component_Sequence_Id
             AND a.Assembly_Item_Id = x.Primary_Item_Id
             AND c.Substitute_Component_Id = z.Inventory_Item_Id
             AND c.Substitute_Component_Id = Msi.Inventory_Item_Id
             AND Msi.Organization_Id = z.Organization_Id
             AND a.Organization_Id = Msi.Organization_Id
             AND b.Component_Item_Id = Msi1.Inventory_Item_Id
             AND Msi1.Organization_Id = z.Organization_Id;
        
          -- 非替代料
          IF v_Count = 0 THEN
            l_Document := '<sfa_file>';
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            l_Document := '<WORKORDER>' || v_Workorder || '</WORKORDER>'; --任务号
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            BEGIN
              SELECT DISTINCT Wo.Operation_Code
                INTO v_Operation_Code
                FROM Wip_Operations_v Wo
               WHERE Wo.Wip_Entity_Id = x.Wip_Entity_Id
                 AND Wo.Operation_Seq_Num = z.Operation_Seq_Num;
            EXCEPTION
              WHEN OTHERS THEN
                v_Operation_Code := NULL;
            END;
          
            l_Document := '<WORKING>' || v_Operation_Code || '</WORKING>'; --工序
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<JOMMATCODE>' || z.Segment1 || '</JOMMATCODE>'; --物料编码
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<JOMDOSAGE>' || z.Quantity_Per_Assembly ||
                          '</JOMDOSAGE>'; --数量        
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<DEVICENO>' || '' || '</DEVICENO>'; --设备        
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<FEEDERTYPE>' || '' || '</FEEDERTYPE>'; --FEEDER型号        
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<STATIONID>' || '' || '</STATIONID>'; --站号        
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<LOCTIONID>' || '' || '</LOCTIONID>'; --位号        
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<MATTAG>' || '' || '</MATTAG>'; --物料标识        
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<REPLACEMATCODE>' || z.Segment1 ||
                          '</REPLACEMATCODE>'; --被替换物料        
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<REPLACENUM>' || '1' || '</REPLACENUM>'; --被替换数量        
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            --取替代特性
            IF z.Required_Quantity = 0 THEN
              l_Document := '<REPLACETAG>' || '0' || '</REPLACETAG>'; --取替代特性
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            ELSE
              SELECT COUNT(*)
                INTO v_Count
                FROM Bom_Bill_Of_Materials_v     a
                    ,Bom_Inventory_Components_v  b
                    ,Bom_Substitute_Components_v c
               WHERE a.Bill_Sequence_Id = b.Bill_Sequence_Id
                 AND b.Component_Sequence_Id = c.Component_Sequence_Id
                 AND a.Assembly_Item_Id = x.Primary_Item_Id
                 AND b.Component_Item_Id = z.Inventory_Item_Id
                 AND c.Substitute_Component_Id IN
                     (SELECT d.Inventory_Item_Id
                        FROM Cux.Cux_Mes_Bom d
                       WHERE d.Wip_Entity_Id = x.Wip_Entity_Id)
                 AND a.Organization_Id = z.Organization_Id;
              IF v_Count > 0 THEN
                l_Document := '<REPLACETAG>' || '1' || '</REPLACETAG>'; --取替代特性
                Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
              ELSE
                l_Document := '<REPLACETAG>' || '0' || '</REPLACETAG>'; --取替代特性
                Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
              END IF;
            END IF;
          
            l_Document := '<REPLACETYPE>' || '' || '</REPLACETYPE>'; --取替代特性
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<FBOMCONTAG>' || '1' || '</FBOMCONTAG>'; ---部件跟踪标志
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            IF v_Operation_Code = 'SMT' THEN
              l_Document := '<FBOMMATTAG>' || '0' || '</FBOMMATTAG>'; --配置标志
            ELSE
              l_Document := '<FBOMMATTAG>' || '5' || '</FBOMMATTAG>'; --配置标志
            END IF;
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<FBOMSTRUCTAG>' || '0' || '</FBOMSTRUCTAG>'; --结构标志
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
            l_Document := '<MATNUMBER>' || z.Required_Quantity ||
                          '</MATNUMBER>'; --结构标志
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          
          
           select nvl(msi.wip_supply_type,1)
             into v_Count
             from mtl_system_items_b msi 
            where msi.organization_id = z.organization_id
              and msi.inventory_item_id = z.inventory_item_id; 
          
           if v_Count = 2 then
              l_Document := '<SELECTTAG>' || 'Y' || '</SELECTTAG>'; --挑料标志
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
           else
              l_Document := '<SELECTTAG>' || 'N' || '</SELECTTAG>'; --挑料标志
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
           end if;
          
          
            /*SELECT COUNT(*)
              INTO v_Count
              FROM Fnd_Lookup_Values a
             WHERE a.Lookup_Type = 'RJ_WIP_ISSUE_INVENTORY'
               AND a.Description = z.Segment1
               AND a.Attribute1 = z.Organization_Id
               AND a.Attribute2 =
                   Decode(z.Organization_Id, 84, 'G109', 'H109');     
            --挑料标志 规则
            IF v_Count > 0 THEN
              SELECT COUNT(*)
                INTO v_Count
                FROM Mtl_System_Items_b Msi
               WHERE Msi.Organization_Id = z.Organization_Id
                 AND Msi.Inventory_Item_Id = z.Inventory_Item_Id
                 AND Msi.Attribute7 IN ('C044',
                                        'C048',
                                        'C049',
                                        'C053',
                                        'C054',
                                        'C059',
                                        'C060',
                                        'C061',
                                        'C062',
                                        'C090',
                                        'C091',
                                        'C092');
              IF v_Count > 0 THEN
                l_Document := '<SELECTTAG>' || 'Y' || '</SELECTTAG>'; --挑料标志
                Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
              ELSE
                l_Document := '<SELECTTAG>' || 'N' || '</SELECTTAG>'; --挑料标志
                Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
              END IF;
            ELSE
              l_Document := '<SELECTTAG>' || 'N' || '</SELECTTAG>'; --挑料标志
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            END IF;*/
          
            l_Document := '</sfa_file>';
            Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
          ELSE
            --获取替代
            FOR Z1 IN (SELECT Msi1.Segment1, b.Component_Yield_Factor
                         FROM Bom_Bill_Of_Materials_v     a
                             ,Bom_Inventory_Components_v  b
                             ,Bom_Substitute_Components_v c
                             ,Mtl_System_Items_b          Msi
                             ,Mtl_System_Items_b          Msi1
                        WHERE a.Bill_Sequence_Id = b.Bill_Sequence_Id
                          AND b.Component_Sequence_Id =
                              c.Component_Sequence_Id
                          AND a.Assembly_Item_Id = x.Primary_Item_Id
                          AND c.Substitute_Component_Id =
                              z.Inventory_Item_Id
                          AND c.Substitute_Component_Id =
                              Msi.Inventory_Item_Id
                          AND Msi.Organization_Id = z.Organization_Id
                          AND a.Organization_Id = Msi.Organization_Id
                          AND b.Component_Item_Id = Msi1.Inventory_Item_Id
                          AND Msi1.Organization_Id = z.Organization_Id) LOOP
              l_Document := '<sfa_file>';
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
              l_Document := '<WORKORDER>' || v_Workorder || '</WORKORDER>'; --任务号          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
              BEGIN
                SELECT DISTINCT Wo.Operation_Code
                  INTO v_Operation_Code
                  FROM Wip_Operations_v Wo
                 WHERE Wo.Wip_Entity_Id = x.Wip_Entity_Id
                   AND Wo.Operation_Seq_Num = z.Operation_Seq_Num;
              EXCEPTION
                WHEN OTHERS THEN
                  v_Operation_Code := NULL;
              END;
            
              l_Document := '<WORKING>' || v_Operation_Code || '</WORKING>'; --工序          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<JOMMATCODE>' || z.Segment1 || '</JOMMATCODE>'; --物料编码          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<JOMDOSAGE>' || z.Quantity_Per_Assembly ||
                            '</JOMDOSAGE>'; --数量          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<DEVICENO>' || '' || '</DEVICENO>'; --设备          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<FEEDERTYPE>' || '' || '</FEEDERTYPE>'; --FEEDER型号          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<STATIONID>' || '' || '</STATIONID>'; --站号          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<LOCTIONID>' || '' || '</LOCTIONID>'; --位号          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<MATTAG>' || '' || '</MATTAG>'; --物料标识
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<REPLACEMATCODE>' || Z1.Segment1 ||
                            '</REPLACEMATCODE>'; --被替换物料          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<REPLACENUM>' || z.Quantity_Per_Assembly *
                            Z1.Component_Yield_Factor || '</REPLACENUM>'; --被替换数量          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<REPLACETAG>' || '1' || '</REPLACETAG>'; --取替代特性          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<REPLACETYPE>' || '' || '</REPLACETYPE>'; --取替代特性          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<FBOMCONTAG>' || '1' || '</FBOMCONTAG>'; ---部件跟踪标志          
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              IF v_Operation_Code = 'SMT' THEN
                l_Document := '<FBOMMATTAG>' || '0' || '</FBOMMATTAG>'; --配置标志
              ELSE
                l_Document := '<FBOMMATTAG>' || '5' || '</FBOMMATTAG>'; --配置标志
              END IF;
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<FBOMSTRUCTAG>' || '0' || '</FBOMSTRUCTAG>'; --结构标志
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              l_Document := '<MATNUMBER>' || z.Required_Quantity ||
                            '</MATNUMBER>'; --结构标志
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            
              select nvl(msi.wip_supply_type,1)
                into v_Count
                from mtl_system_items_b msi 
               where msi.organization_id = z.organization_id
                 and msi.inventory_item_id = z.inventory_item_id; 
          
             if v_Count = 2 then
                l_Document := '<SELECTTAG>' || 'Y' || '</SELECTTAG>'; --挑料标志
                Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
             else
                l_Document := '<SELECTTAG>' || 'N' || '</SELECTTAG>'; --挑料标志
                Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
             end if;
           
              /*SELECT COUNT(*)
                INTO v_Count
                FROM Fnd_Lookup_Values a
               WHERE a.Lookup_Type = 'RJ_WIP_ISSUE_INVENTORY'
                 AND a.Description = z.Segment1
                 AND a.Attribute1 = z.Organization_Id
                 AND a.Attribute2 =
                     Decode(z.Organization_Id, 84, 'G109', 'H109');
            
              IF v_Count > 0 THEN
                SELECT COUNT(*)
                  INTO v_Count
                  FROM Mtl_System_Items_b Msi
                 WHERE Msi.Organization_Id = z.Organization_Id
                   AND Msi.Inventory_Item_Id = z.Inventory_Item_Id
                   AND Msi.Attribute7 IN ('C044',
                                          'C048',
                                          'C049',
                                          'C053',
                                          'C054',
                                          'C059',
                                          'C060',
                                          'C061',
                                          'C062',
                                          'C090',
                                          'C091',
                                          'C092');
                IF v_Count > 0 THEN
                  l_Document := '<SELECTTAG>' || 'Y' || '</SELECTTAG>'; --挑料标志
                  Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
                ELSE
                  l_Document := '<SELECTTAG>' || 'N' || '</SELECTTAG>'; --挑料标志
                  Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
                END IF;
              ELSE
                l_Document := '<SELECTTAG>' || 'N' || '</SELECTTAG>'; --挑料标志
                Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
              END IF;*/
              
              l_Document := '</sfa_file>';
              Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
            END LOOP;
          
          END IF;
        
        END LOOP;
      
        l_Document := '</sfa_files>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        l_Document := '</sfb_file>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
        l_Document := '</STD_IN>';
        Wf_Notification.Writetoclob(l_Clob_Document, l_Document);
      
        --如果有需要发布的数据
        l_Service_Url         := Cux_External_Interface_Pkg.Getinterfaceurl(l_Destination_System);
        l_Soa_Method          := 'GenerateWorkOrderNew';
        l_Resp_Parameter_Name := 'GenerateWorkOrderNewResponse';
        --记录SOA请求
        l_Request.Entity_Type     := 'MES_JOB';
        l_Request.Entity_Id       := x.Wip_Entity_Id;
        l_Request.Direction       := 'OUTBOUND';
        l_Request.Interface       := 'CUX_MES_PKG';
        l_Request.Method          := 'E2M_JOB';
        l_Request.Relative_System := l_Destination_System;
        l_Request.Url             := l_Service_Url;
        l_Request.Request_Date    := SYSDATE;
        l_Request.Request_Msg     := l_Clob_Document;
        Cux_Soa_Pub.Create_Request(l_Request, l_Request_Id);
      
        l_Service_Content := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:tem="http://tempuri.org/">' ||
                             Chr(10) || '<soapenv:Header/>' || Chr(10) ||
                             '<soapenv:Body>' || Chr(10) ||
                             '<tem:GenerateWorkOrderNew>' || Chr(10) ||
                             '<tem:str_workorder>' || Chr(10) ||
                             '<![CDATA[' || l_Clob_Document || ']]>' ||
                             Chr(10) || '</tem:str_workorder>' || Chr(10) ||
                             '</tem:GenerateWorkOrderNew>' || Chr(10) ||
                             '</soapenv:Body>' || Chr(10) ||
                             '</soapenv:Envelope>';
      
        Cux_Soa_Pub.Call_Web_Service(p_Service_Url      => l_Service_Url,
                                     p_Name_Space       => 'http://tempuri.org/',
                                     p_Soa_Method       => l_Soa_Method,
                                     p_Message_Content  => l_Service_Content,
                                     x_Ret_Sts          => l_Ret_Sts,
                                     x_Error_Msg        => l_Error_Msg,
                                     x_Response_Content => l_Response_Content);
      
        Fnd_File.Put_Line(Fnd_File.Log,
                          To_Char(SYSDATE, 'yyyy-mm-dd hh24:mi:ss') ||
                          '调用WS返回l_Ret_Sts:' || l_Ret_Sts);
        Fnd_File.Put_Line(Fnd_File.Log,
                          To_Char(SYSDATE, 'yyyy-mm-dd hh24:mi:ss') ||
                          '调用WS返回l_Error_Msg:' || l_Error_Msg);
      
        --解析发布结果
        l_Request.Response_Msg := l_Response_Content;
        IF l_Ret_Sts = Fnd_Api.g_Ret_Sts_Success THEN
          Cux_Soa_Pub.Parser_Resp_Mes(p_Xml_Clob            => l_Response_Content,
                                      p_Resp_Parameter_Name => 'GenerateWorkOrderNewResponse',
                                      p_Tagname_1           => 'Status',
                                      p_Tagname_2           => 'Message',
                                      x_Xml_Clob            => l_Parser_Log,
                                      x_Return_Value_1      => l_Parser_Return_Value_1,
                                      x_Return_Value_2      => l_Parser_Return_Value_2,
                                      x_Ret_Status          => l_Parser_Ret_Sts,
                                      x_Error_Message       => l_Parser_Error_Msg);
        
          Fnd_File.Put_Line(Fnd_File.Log,
                            To_Char(SYSDATE, 'yyyy-mm-dd hh24:mi:ss') ||
                            '解析l_parser_Ret_Sts:' || l_Parser_Ret_Sts);
          Fnd_File.Put_Line(Fnd_File.Log,
                            To_Char(SYSDATE, 'yyyy-mm-dd hh24:mi:ss') ||
                            '解析l_parser_Error_Msg:' || l_Parser_Error_Msg);
          Fnd_File.Put_Line(Fnd_File.Log,
                            To_Char(SYSDATE, 'yyyy-mm-dd hh24:mi:ss') ||
                            '解析l_Parser_Return_Value_1:' ||
                            l_Parser_Return_Value_1);
          Fnd_File.Put_Line(Fnd_File.Log,
                            To_Char(SYSDATE, 'yyyy-mm-dd hh24:mi:ss') ||
                            '解析l_Parser_Return_Value_2:' ||
                            l_Parser_Return_Value_2);
        
          IF l_Parser_Ret_Sts = Fnd_Api.g_Ret_Sts_Success THEN
            IF l_Parser_Return_Value_1 = '0' THEN
              l_Request.Response_Status := Fnd_Api.g_Ret_Sts_Success;
              --更新传送标记
              UPDATE Wip_Discrete_Jobs Wdj
                 SET Wdj.Attribute15 = 'Y'
               WHERE Wdj.Wip_Entity_Id = x.Wip_Entity_Id;
            ELSE
              l_Request.Response_Status := Fnd_Api.g_Ret_Sts_Error;
              l_Request.Error_Msg       := l_Parser_Return_Value_2;
            END IF;
          ELSIF l_Parser_Ret_Sts = Fnd_Api.g_Ret_Sts_Error THEN
            l_Responsed_Msg           := l_Response_Content;
            l_Request.Response_Status := Fnd_Api.g_Ret_Sts_Error;
            l_Request.Error_Msg       := '解析返回报文失败。';
          END IF;
        ELSE
          l_Request.Response_Status := Fnd_Api.g_Ret_Sts_Error;
          l_Request.Error_Msg       := '调用失服务失败。';
        END IF;
      
        Fnd_File.Put_Line(Fnd_File.Log,
                          To_Char(SYSDATE, 'yyyy-mm-dd hh24:mi:ss') ||
                          'l_Request.Response_Status:' ||
                          l_Request.Response_Status);
      
        IF l_Request.Response_Status <> Fnd_Api.g_Ret_Sts_Success THEN
          Retcode := 1;
        END IF;
      
        --记录SOA请求      
        Cux_Soa_Pub.Update_Request(l_Request);
      
        Html_Report_Pkg.v_Report_Output_Mode := 'F';
        Html_Report_Pkg.Output_Line('<table><tr><td width=100>工单</td><td width=100>状态</td><td width=300>信息</td></tr>');
        Html_Report_Pkg.Output_Line('<tr><td>' || x.Wip_Entity_Name ||
                                    '</td>');
        Html_Report_Pkg.Output_Line('<td>' || l_Request.Response_Status ||
                                    '</td>');
        Html_Report_Pkg.Output_Line('<td>' || l_Request.Error_Msg ||
                                    '</td></tr></table>');
      
        COMMIT;
      END IF;
    END LOOP;
  
    IF l_Process_Number = 0 THEN
      Html_Report_Pkg.v_Report_Output_Mode := 'F';
      Html_Report_Pkg.Output_Line('没有可传送的工单！');
    END IF;
  
  END;

  PROCEDURE Create_Bom(p_Wip_Entity_Id IN NUMBER) AS
    l_Num_Group_Id    NUMBER; --排程品番BOM分解
    v_Err_Code        VARCHAR2(100);
    v_Err_Msg         VARCHAR2(2000);
    v_Organization_Id NUMBER;
    v_Primary_Item_Id NUMBER;
  BEGIN
    ---把工单需求写入临时表
    DELETE FROM Cux.Cux_Mes_Bom WHERE Wip_Entity_Id = p_Wip_Entity_Id;
  
    INSERT INTO Cux.Cux_Mes_Bom
      (Operation_Seq_Num
      ,Segment1
      ,Required_Quantity
      ,Quantity_Per_Assembly
      ,Inventory_Item_Id
      ,Organization_Id
      ,Wip_Entity_Id)
      SELECT Wro.Operation_Seq_Num
            ,Msi.Segment1
            ,Wro.Required_Quantity
            ,Wro.Quantity_Per_Assembly
            ,Wro.Inventory_Item_Id
            ,Wro.Organization_Id
            ,Wro.Wip_Entity_Id
        FROM Wip_Requirement_Operations Wro, Mtl_System_Items_b Msi
       WHERE Wro.Wip_Entity_Id = p_Wip_Entity_Id
         AND Msi.Inventory_Item_Id = Wro.Inventory_Item_Id
         AND Msi.Organization_Id = Wro.Organization_Id;
  
    SELECT Wdj.Organization_Id, Wdj.Primary_Item_Id
      INTO v_Organization_Id, v_Primary_Item_Id
      FROM Wip_Discrete_Jobs Wdj
     WHERE Wdj.Wip_Entity_Id = p_Wip_Entity_Id;
    SELECT Cux_Mrp_Simulation_s.Nextval INTO l_Num_Group_Id FROM Dual;
    ---展开BOM
    Bompexpl.Exploder_Userexit(Verify_Flag       => 0,
                               Org_Id            => v_Organization_Id,
                               Order_By          => 1,
                               Grp_Id            => l_Num_Group_Id,
                               Session_Id        => 0,
                               Levels_To_Explode => 1,
                               Bom_Or_Eng        => 1,
                               Impl_Flag         => 1,
                               Plan_Factor_Flag  => 2,
                               Explode_Option    => 2, -- 2,
                               Module            => 2,
                               Cst_Type_Id       => 2,
                               Std_Comp_Flag     => 2,
                               Expl_Qty          => 1,
                               Item_Id           => v_Primary_Item_Id,
                               Alt_Desg          => '', --替代
                               Comp_Code         => '',
                               Rev_Date          => '',
                               Err_Msg           => v_Err_Msg,
                               ERROR_CODE        => v_Err_Code);
  
    IF v_Err_Code <> 0 THEN
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
  
    FOR y IN (SELECT a.Component_Item_Id
                    ,a.Operation_Seq_Num
                    ,Msi.Segment1
                    ,a.Component_Quantity
                    ,Msi.Organization_Id
                FROM Bom_Explosion_Temp a, Mtl_System_Items_b Msi
               WHERE a.Group_Id = l_Num_Group_Id
                 AND a.Organization_Id = Msi.Organization_Id
                 AND a.Component_Item_Id = Msi.Inventory_Item_Id
                 AND a.Plan_Level <> 0
                 AND NOT EXISTS
               (SELECT 'X'
                        FROM Cux.Cux_Mes_Bom c
                       WHERE c.Wip_Entity_Id = p_Wip_Entity_Id
                         AND c.Inventory_Item_Id = a.Component_Item_Id)
               ORDER BY a.Plan_Level) LOOP
      INSERT INTO Cux.Cux_Mes_Bom
        (Operation_Seq_Num
        ,Segment1
        ,Required_Quantity
        ,Quantity_Per_Assembly
        ,Inventory_Item_Id
        ,Organization_Id
        ,Wip_Entity_Id)
      VALUES
        (y.Operation_Seq_Num
        ,y.Segment1
        ,0
        ,y.Component_Quantity
        ,y.Component_Item_Id
        ,y.Organization_Id
        ,p_Wip_Entity_Id);
    END LOOP;
    COMMIT;
  END;

  PROCEDURE Cux_Mes_Import_Batch(Retcode IN VARCHAR2, Errbuf IN VARCHAR2) AS
    CURSOR C1 IS
      SELECT Wdj.Wip_Entity_Id, Wdj.Organization_Id
        FROM Wip_Discrete_Jobs      Wdj
            ,Wip_Accounting_Classes Wac
            ,Wip_Entities           We
            ,Mtl_System_Items_b     Msi
       WHERE Msi.Organization_Id = We.Organization_Id
         AND Msi.Inventory_Item_Id = We.Primary_Item_Id
         AND Wdj.Wip_Entity_Id = We.Wip_Entity_Id
         AND Wac.Organization_Id = Wdj.Organization_Id
         AND Wac.Class_Code = Wdj.Class_Code
            --AND WDJ.CLASS_CODE NOT IN (13,12) 
            /* updated by luojun 2015-03-06
              将排除工单类型维护在快码上
            */
         AND NOT EXISTS
       (SELECT 1
                FROM Fnd_Lookup_Values Flv
               WHERE Flv.Lookup_Type = 'CUX_WIP_2_MES_EXCLUDED_CLASS'
                 AND Flv.Enabled_Flag = 'Y'
                 AND Trunc(SYSDATE) BETWEEN
                     Nvl(Flv.Start_Date_Active, Trunc(SYSDATE)) AND
                     Nvl(Flv.End_Date_Active, Trunc(SYSDATE))
                 AND Flv.Language = 'US'
                 AND Flv.Lookup_Code = Wdj.Class_Code)
         AND Wdj.Attribute3 NOT IN ('91006')
         AND Msi.Segment1 NOT LIKE 'V-080%'
            /*         AND NVL(WDJ.ATTRIBUTE15, 'N') = 'N' --是否已经成功传入MES接口*/
         AND Wdj.Status_Type = '3'
         AND NOT EXISTS (SELECT 'X'
                FROM Cux.Cux_Wip_Mes_Body Cwmb
               WHERE Cwmb.Wip_Entity_Id = Wdj.Wip_Entity_Id
                 AND Cwmb.Wip_Date IS NOT NULL); --已发放 
  BEGIN
    FOR x IN C1 LOOP
      NULL;
      Cux_Mes_Pkg.Cux_Mes_Import(Retcode           => Retcode,
                                 Errbuf            => Errbuf,
                                 p_Organization_Id => x.Organization_Id,
                                 p_Wip_Entity_Id   => x.Wip_Entity_Id);
    END LOOP;
  END;

  --webservice 取消完工反馈信息
  FUNCTION Response_Cancel_Completion(p_Ret_Status     VARCHAR2,
                                      p_Error_Msg      VARCHAR2,
                                      p_Completion_Job Cux_Mes_Complete_Jobs%ROWTYPE)
    RETURN CLOB IS
    l_Response_Msg CLOB;
  BEGIN
    l_Response_Msg := l_Response_Msg || '<STD_OUT origin="TIPTOP">';
    l_Response_Msg := l_Response_Msg || '<Service Name="SetData">';
    IF p_Ret_Status = Fnd_Api.g_Ret_Sts_Success THEN
      l_Response_Msg := l_Response_Msg || '<Status>0</Status>';
    ELSE
      l_Response_Msg := l_Response_Msg || '<Status>1</Status>';
    END IF;
    l_Response_Msg := l_Response_Msg || '<Error>' || p_Error_Msg ||
                      '</Error>';
    l_Response_Msg := l_Response_Msg || '<OperDate>' ||
                      To_Char(SYSDATE, 'YYYY/MM/DD HH24:MI:SS') ||
                      '</OperDate>'; --操作时间
    l_Response_Msg := l_Response_Msg || '</Service></STD_OUT>';
  
    UPDATE Cux_Soa_Requests t
       SET t.Response_Date   = SYSDATE
          ,t.Response_Msg    = l_Response_Msg
          ,t.Response_Status = Decode(p_Ret_Status,
                                      Fnd_Api.g_Ret_Sts_Success,
                                      'S',
                                      'F')
          ,t.Entity_Type     = 'WIP_COMPLETE'
          ,t.Entity_Id       = p_Completion_Job.Mes_Completion_Number
          ,t.Error_Msg       = Decode(p_Ret_Status,
                                      Fnd_Api.g_Ret_Sts_Success,
                                      NULL,
                                      p_Error_Msg)
     WHERE t.Request_Id = p_Completion_Job.Request_Id;
    COMMIT;
    RETURN l_Response_Msg;
  END Response_Cancel_Completion;
  --webservice 完工反馈信息
  FUNCTION Response_Completion(p_Ret_Status     VARCHAR2,
                               p_Error_Msg      VARCHAR2,
                               p_Completion_Job Cux_Mes_Complete_Jobs%ROWTYPE)
    RETURN CLOB IS
    l_Response_Msg CLOB;
  BEGIN
    l_Response_Msg := l_Response_Msg || '<STD_OUT origin="TIPTOP">';
    l_Response_Msg := l_Response_Msg || '<Service Name="SetData">';
    IF p_Ret_Status = Fnd_Api.g_Ret_Sts_Success THEN
      l_Response_Msg := l_Response_Msg || '<Status>0</Status>';
    ELSE
      l_Response_Msg := l_Response_Msg || '<Status>1</Status>';
    END IF;
    l_Response_Msg := l_Response_Msg || '<Error>' || p_Error_Msg ||
                      '</Error>';
    l_Response_Msg := l_Response_Msg || '<OperDate>' ||
                      To_Char(SYSDATE, 'YYYY/MM/DD HH24:MI:SS') ||
                      '</OperDate>'; --操作时间
    l_Response_Msg := l_Response_Msg || '<RKDH>' || g_Factory ||
                      p_Completion_Job.Erp_Completion_Number || '</RKDH>'; --入库单号  
    l_Response_Msg := l_Response_Msg || '<RKLX>' ||
                      p_Completion_Job.Completion_Type || '</RKLX>'; --入库类型
    l_Response_Msg := l_Response_Msg || '<DJRQ>' ||
                      To_Char(p_Completion_Job.Erp_Completion_Date,
                              'YYYY/MM/DD HH24:MI:SS') || '</DJRQ>'; --单据日期
    l_Response_Msg := l_Response_Msg || '<CKDM>' ||
                      p_Completion_Job.Subinventory_Code || '</CKDM>'; --仓库代码
    l_Response_Msg := l_Response_Msg || '<CKMC>' ||
                      p_Completion_Job.Subinventory_Name || '</CKMC>'; --仓库名称
    l_Response_Msg := l_Response_Msg || '<BMMC>' ||
                      p_Completion_Job.Department_Code || '</BMMC>'; --部门名称
    l_Response_Msg := l_Response_Msg || '<GYSMC>' ||
                      p_Completion_Job.Vendor_Name || '</GYSMC>'; --供应商名称
    l_Response_Msg := l_Response_Msg || '<ZBBZ>' || p_Completion_Job.Memo ||
                      '</ZBBZ>'; --主表备注
    l_Response_Msg := l_Response_Msg || '<GDBH>' ||
                      p_Completion_Job.Note_Number || '</GDBH>'; --MES工单编号
    l_Response_Msg := l_Response_Msg || '<CPH>' ||
                      p_Completion_Job.Wip_Entity_Name || '</CPH>'; --传票号(ERP工单号)
    l_Response_Msg := l_Response_Msg || '<HW>' ||
                      p_Completion_Job.Locator_Name || '</HW>'; --货位
    l_Response_Msg := l_Response_Msg || '<CPDM>' ||
                      p_Completion_Job.Item_Number || '</CPDM>'; --产品代码
    l_Response_Msg := l_Response_Msg || '<CPMC>' ||
                      p_Completion_Job.Item_Desp || '</CPMC>'; --产品名称
    l_Response_Msg := l_Response_Msg || '<RKSL>' ||
                      p_Completion_Job.Complete_Quantity || '</RKSL>'; --入库数量
    l_Response_Msg := l_Response_Msg || '<YJBBH>' ||
                      p_Completion_Job.Hardware_Version || '</YJBBH>'; --硬件版本号
    l_Response_Msg := l_Response_Msg || '<RJBBH>' ||
                      p_Completion_Job.Software_Version || '</RJBBH>'; --软件版本号
    l_Response_Msg := l_Response_Msg || '<DW>' || p_Completion_Job.Uom ||
                      '</DW>'; --单位
    l_Response_Msg := l_Response_Msg || '<MXBZ>' ||
                      p_Completion_Job.Spec_Memo || '</MXBZ>'; --明细备注
    l_Response_Msg := l_Response_Msg || '</Service></STD_OUT>';
  
    UPDATE Cux_Soa_Requests t
       SET t.Response_Date   = p_Completion_Job.Erp_Completion_Date
          ,t.Response_Msg    = l_Response_Msg
          ,t.Response_Status = Decode(p_Ret_Status,
                                      Fnd_Api.g_Ret_Sts_Success,
                                      'S',
                                      'F')
          ,t.Entity_Type     = 'WIP_COMPLETE'
          ,t.Entity_Id       = p_Completion_Job.Mes_Completion_Number
          ,t.Error_Msg       = Decode(p_Ret_Status,
                                      Fnd_Api.g_Ret_Sts_Success,
                                      NULL,
                                      p_Error_Msg)
     WHERE t.Request_Id = p_Completion_Job.Request_Id;
    COMMIT;
    RETURN l_Response_Msg;
  
  END Response_Completion;

  --解析MES完工入库信息
  PROCEDURE Get_Cancel_Completion(p_Doc          Dbms_Xmldom.Domdocument,
                                  p_Request_Id   NUMBER,
                                  x_Ret_Status   OUT VARCHAR2,
                                  x_Error_Msg    OUT VARCHAR2,
                                  x_Complete_Job OUT Cux_Mes_Complete_Jobs%ROWTYPE) IS
    l_Node_List              Dbms_Xmldom.Domnodelist;
    l_Node                   Dbms_Xmldom.Domnode;
    l_Node_Count             NUMBER;
    l_Tmp_String             VARCHAR2(500);
    l_Tmp_Count              NUMBER;
    l_Quantity_Not_Completed NUMBER; --工单未完工数量
    l_Completing_Quantity    NUMBER; --已开完工单未过账数量
  
    l_Completion_Detail      Cux_Mes_Completion_Details%ROWTYPE;
    l_Null_Completion_Detail Cux_Mes_Completion_Details%ROWTYPE;
  
    l_Detail_Title VARCHAR2(50);
  BEGIN
    x_Ret_Status                  := Fnd_Api.g_Ret_Sts_Success;
    x_Complete_Job.Request_Id     := p_Request_Id;
    x_Complete_Job.Operation_Code := 'CANCEL';
    --读取工单信息
    l_Node_List := Dbms_Xmldom.Getelementsbytagname(p_Doc, 'STD_IN');
    l_Node      := Dbms_Xmldom.Item(l_Node_List, 0);
    --MES完工入库单号
    Dbms_Xslprocessor.Valueof(l_Node, 'DJBH/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := 'MES完工入库单号为空。';
      RETURN;
    ELSE
      x_Complete_Job.Mes_Completion_Number := l_Tmp_String;
      SELECT COUNT(*)
        INTO l_Tmp_Count
        FROM Cux_Ready_Item t
       WHERE t.Source_System = 'MES'
         AND t.Source_Document_Id = l_Tmp_String;
      IF l_Tmp_Count = 0 THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := 'MES完工入库单不存在系统中:' || l_Tmp_String;
        RETURN;
      END IF;
    END IF;
    --法人
    Dbms_Xslprocessor.Valueof(l_Node, 'Factory/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := '企业法人为空。';
      RETURN;
    ELSE
    
      SELECT COUNT(*)
        INTO l_Tmp_Count
        FROM Fnd_Lookup_Values_Vl t, Org_Organization_Definitions Ood
       WHERE t.Lookup_Type = 'CUX_LEGAL_ENTITY'
         AND t.Lookup_Code = l_Tmp_String
         AND t.Tag = Ood.Organization_Code;
      IF l_Tmp_Count = 0 THEN
      
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := '企业法人错误:' || l_Tmp_String;
        RETURN;
      END IF;
    END IF;
    --MES
    Dbms_Xslprocessor.Valueof(l_Node, 'Source/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL OR l_Tmp_String <> 'MES' THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := '来源为空或者错误:' || l_Tmp_String;
      RETURN;
    END IF;
    --ObjectID
    Dbms_Xslprocessor.Valueof(l_Node, 'ObjectID/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL OR l_Tmp_String <> 'WGRKQX_INTERFACE' THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := 'ObjectID为空或者错误:' || l_Tmp_String;
      RETURN;
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Unexp_Error;
      x_Error_Msg  := SQLERRM;
  END Get_Cancel_Completion;
  --解析MES完工入库信息
  PROCEDURE Get_Completion(p_Doc                   Dbms_Xmldom.Domdocument,
                           p_Request_Id            NUMBER,
                           x_Ret_Status            OUT VARCHAR2,
                           x_Error_Msg             OUT VARCHAR2,
                           x_Complete_Job          OUT Cux_Mes_Complete_Jobs%ROWTYPE,
                           x_Completion_Detail_Tab OUT Completion_Detail_Tab) IS
    l_Node_List              Dbms_Xmldom.Domnodelist;
    l_Node                   Dbms_Xmldom.Domnode;
    l_Node_Count             NUMBER;
    l_Tmp_String             VARCHAR2(500);
    l_Tmp_Count              NUMBER;
    l_Quantity_Not_Completed NUMBER; --工单未完工数量
    l_Completing_Quantity    NUMBER; --已开完工单未过账数量
  
    l_Completion_Detail      Cux_Mes_Completion_Details%ROWTYPE;
    l_Null_Completion_Detail Cux_Mes_Completion_Details%ROWTYPE;
  
    l_Detail_Title VARCHAR2(50);
    l_Item         VARCHAR2(30);
  BEGIN
    x_Ret_Status                  := Fnd_Api.g_Ret_Sts_Success;
    x_Complete_Job.Request_Id     := p_Request_Id;
    x_Complete_Job.Operation_Code := 'CREATE';
    --读取工单信息
    l_Node_List := Dbms_Xmldom.Getelementsbytagname(p_Doc, 'STD_IN');
    l_Node      := Dbms_Xmldom.Item(l_Node_List, 0);
    --MES完工入库单号
    l_Item := 'DJBH';
    Dbms_Xslprocessor.Valueof(l_Node, 'DJBH/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := 'MES完工入库单号为空。';
      RETURN;
    ELSE
      x_Complete_Job.Mes_Completion_Number := l_Tmp_String;
      SELECT COUNT(*)
        INTO l_Tmp_Count
        FROM Cux_Mes_Complete_Jobs t
       WHERE t.Mes_Completion_Number = l_Tmp_String;
      IF l_Tmp_Count > 0 THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := 'MES完工入库单已经抛转过:' || l_Tmp_String;
        RETURN;
      END IF;
    END IF;
    --法人
    l_Item := 'Factory';
    Dbms_Xslprocessor.Valueof(l_Node, 'Factory/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := '企业法人为空。';
      RETURN;
    ELSE
      BEGIN
        g_Factory := l_Tmp_String;
        SELECT To_Number(t.Meaning), Ood.Organization_Id
          INTO x_Complete_Job.Org_Id, x_Complete_Job.Organization_Id
          FROM Fnd_Lookup_Values_Vl t, Org_Organization_Definitions Ood
         WHERE t.Lookup_Type = 'CUX_LEGAL_ENTITY'
           AND t.Lookup_Code = l_Tmp_String
           AND t.Tag = Ood.Organization_Code;
      EXCEPTION
        WHEN OTHERS THEN
          x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg  := '企业法人错误:' || l_Tmp_String;
          RETURN;
      END;
    END IF;
    --MES
    l_Item := 'Source';
    Dbms_Xslprocessor.Valueof(l_Node, 'Source/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL OR l_Tmp_String <> 'MES' THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := '来源为空或者错误:' || l_Tmp_String;
      RETURN;
    END IF;
    --ObjectID
    l_Item := 'ObjectID';
    Dbms_Xslprocessor.Valueof(l_Node, 'ObjectID/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL OR l_Tmp_String <> 'WGRK_INTERFACE' THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := 'ObjectID为空或者错误:' || l_Tmp_String;
      RETURN;
    END IF;
    --传票号(ERP工单号)
    l_Item := 'CPH';
    Dbms_Xslprocessor.Valueof(l_Node, 'CPH/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := '传票号为空。';
      RETURN;
    ELSE
      BEGIN
        SELECT Wdj.Start_Quantity - Wdj.Quantity_Completed -
               Wdj.Quantity_Scrapped
              ,Wdj.Attribute3
              ,t.Wip_Entity_Id
          INTO l_Quantity_Not_Completed
              ,x_Complete_Job.Workshop
              ,x_Complete_Job.Wip_Entity_Id
          FROM Wip_Entities t, Wip_Discrete_Jobs Wdj
         WHERE t.Organization_Id = x_Complete_Job.Organization_Id
           AND t.Wip_Entity_Name = l_Tmp_String
           AND t.Wip_Entity_Id = Wdj.Wip_Entity_Id
           AND Wdj.Status_Type = 3;
        x_Complete_Job.Wip_Entity_Name := l_Tmp_String;
      EXCEPTION
        WHEN OTHERS THEN
          x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg  := '传票号不存在Oracle系统或者状态不为发放:' || l_Tmp_String;
          RETURN;
      END;
    END IF;
    --MES工单编号
    l_Item := 'GDBH';
    Dbms_Xslprocessor.Valueof(l_Node, 'GDBH/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := '工单编号为空。';
      RETURN;
    ELSE
      x_Complete_Job.Note_Number := l_Tmp_String;
    END IF;
  
    --车间代码
    l_Item := 'CJDM';
    Dbms_Xslprocessor.Valueof(l_Node, 'CJDM/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := '车间代码为空。';
      RETURN;
    END IF;
  
    --入库日期
    l_Item := 'RKRQ';
    Dbms_Xslprocessor.Valueof(l_Node, 'RKRQ/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := '入库日期为空。';
      RETURN;
    ELSE
      BEGIN
        x_Complete_Job.Mes_Complete_Date := To_Date(l_Tmp_String,
                                                    'YYYY/MM/DD HH24:MI:SS');
      EXCEPTION
        WHEN OTHERS THEN
          x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg  := '入库日期格式不对:' || l_Tmp_String;
          RETURN;
      END;
    END IF;
  
    --产品代码
    l_Item := 'CPDM';
    Dbms_Xslprocessor.Valueof(l_Node, 'CPDM/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := '产品代码为空。';
      RETURN;
    ELSE
      x_Complete_Job.Item_Number := l_Tmp_String;
      BEGIN
        SELECT t.Inventory_Item_Id, t.Description
          INTO x_Complete_Job.Item_Id, x_Complete_Job.Item_Desp
          FROM Mtl_System_Items_b t
         WHERE t.Organization_Id = x_Complete_Job.Organization_Id
           AND t.Segment1 = l_Tmp_String;
      EXCEPTION
        WHEN OTHERS THEN
          x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg  := '产品代码不存在:' || l_Tmp_String;
          RETURN;
      END;
    END IF;
  
    --入库数量
    l_Item := 'RKSL';
    Dbms_Xslprocessor.Valueof(l_Node, 'RKSL/text()', l_Tmp_String);
    IF l_Tmp_String IS NULL THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := '入库数量为空。';
      RETURN;
    ELSE
      BEGIN
        x_Complete_Job.Complete_Quantity := To_Number(l_Tmp_String);
      EXCEPTION
        WHEN OTHERS THEN
          x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
          x_Error_Msg  := '入库数量格式不正确:' || l_Tmp_String;
          RETURN;
      END;
      SELECT Nvl(SUM(t.Ready_Qty), 0)
        INTO l_Completing_Quantity
        FROM Cux_Ready_Item t
       WHERE t.Organization_Id = x_Complete_Job.Organization_Id
         AND t.Wip_Entity_Id = x_Complete_Job.Wip_Entity_Id
         AND t.Doc_Status = '未过账'
         AND t.Doc_Type = '完工入库';
      IF l_Quantity_Not_Completed - l_Completing_Quantity <
         x_Complete_Job.Complete_Quantity THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := '入库数量(' || x_Complete_Job.Complete_Quantity ||
                        ')大于未完工数量(包含未过账完工单):' ||
                        (l_Quantity_Not_Completed - l_Completing_Quantity);
        RETURN;
      END IF;
    END IF;
  
    --录入人
    l_Item := 'LRR';
    Dbms_Xslprocessor.Valueof(l_Node,
                              'LRR/text()',
                              x_Complete_Job.Entry_User);
    --属性号1
    l_Item := 'MATERIAL_BATCH1';
    Dbms_Xslprocessor.Valueof(l_Node,
                              'MATERIAL_BATCH1/text()',
                              x_Complete_Job.Material_Batch1);
    --属性号2
    l_Item := 'MATERIAL_BATCH2';
    Dbms_Xslprocessor.Valueof(l_Node,
                              'MATERIAL_BATCH2/text()',
                              x_Complete_Job.Material_Batch2);
    --获取箱号信息
    l_Node_List  := Dbms_Xmldom.Getelementsbytagname(p_Doc, 'BAR_BOX');
    l_Node_Count := Dbms_Xmldom.Getlength(l_Node_List);
    FOR i IN 1 .. l_Node_Count LOOP
      l_Completion_Detail := l_Null_Completion_Detail;
      l_Node              := Dbms_Xmldom.Item(l_Node_List, i - 1);
      SELECT Cux_Mes_Completion_Details_s.Nextval
        INTO l_Completion_Detail.Detail_Id
        FROM Dual;
      l_Completion_Detail.Request_Id            := p_Request_Id;
      l_Completion_Detail.Mes_Completion_Number := x_Complete_Job.Mes_Completion_Number;
      l_Completion_Detail.Entry_Type            := 'BAR_BOX';
      --箱号
      l_Item := 'Box:BOX_NO';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'BOX_NO/text()',
                                l_Completion_Detail.Box_No);
      IF l_Completion_Detail.Box_No IS NULL THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := '箱号为空。';
        RETURN;
      END IF;
      l_Detail_Title := '箱号(' || l_Completion_Detail.Box_No || ')';
      --SN号
      l_Item := 'Box:BARCODE_NO';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'BARCODE_NO/text()',
                                l_Completion_Detail.Barcode_No);
      IF l_Completion_Detail.Barcode_No IS NULL THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || 'SN号为空。';
        RETURN;
      END IF;
      --栈板号
      l_Item := 'Box:PALLET_NO';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'PALLET_NO/text()',
                                l_Completion_Detail.Pallet_No);
      --装栈时间
      l_Item := 'Box:PALLET_DATE';
      Dbms_Xslprocessor.Valueof(l_Node, 'PALLET_DATE/text()', l_Tmp_String);
      IF l_Tmp_String IS NOT NULL THEN
        BEGIN
          l_Completion_Detail.Pallet_Date := To_Date(l_Tmp_String,
                                                     'YYYY/MM/DD HH24:MI:SS');
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
            x_Error_Msg  := l_Detail_Title || '装栈时间日期格式不对:' || l_Tmp_String;
            RETURN;
        END;
      END IF;
      --装箱数量
      l_Item := 'Box:BOX_COUNT';
      Dbms_Xslprocessor.Valueof(l_Node, 'BOX_COUNT/text()', l_Tmp_String);
      IF l_Tmp_String IS NULL THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '装箱数量为空。';
        RETURN;
      ELSE
        BEGIN
          l_Completion_Detail.Box_Count := To_Number(l_Tmp_String);
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
            x_Error_Msg  := l_Detail_Title || '装箱数量数字格式不对:' || l_Tmp_String;
            RETURN;
        END;
      END IF;
      --MES调度号
      l_Item := 'Box:SOURCE_NO';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'SOURCE_NO/text()',
                                l_Completion_Detail.Source_No);
      --是否厂内SN规则
      l_Item := 'Box:SNRULE_FLAG';
      Dbms_Xslprocessor.Valueof(l_Node, 'SNRULE_FLAG/text()', l_Tmp_String);
      IF l_Tmp_String IS NULL THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '是否厂内SN规则为空。';
        RETURN;
      ELSIF l_Tmp_String NOT IN ('Y', 'N') THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '是否厂内SN规则格式不对:' || l_Tmp_String;
        RETURN;
      ELSE
        l_Completion_Detail.Snrule_Flag := l_Tmp_String;
      END IF;
    
      --箱号有效性
      l_Item := 'Box:BOX_VALID_FLAG';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'BOX_VALID_FLAG/text()',
                                l_Tmp_String);
      IF l_Tmp_String IN ('Y', 'N') THEN
        l_Completion_Detail.Box_Valid_Flag := l_Tmp_String;
      ELSE
        l_Completion_Detail.Snrule_Flag := 'Y';
      END IF;
    
      --SN有效性
      l_Item := 'Box:SN_FLAG';
      Dbms_Xslprocessor.Valueof(l_Node, 'SN_FLAG/text()', l_Tmp_String);
      IF l_Tmp_String IN ('Y', 'N') THEN
        l_Completion_Detail.Sn_Flag := l_Tmp_String;
      ELSE
        l_Completion_Detail.Sn_Flag := 'Y';
      END IF;
    
      --特殊箱号
      l_Item := 'Box:SPECIAL_BOX_NO';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'SPECIAL_BOX_NO/text()',
                                l_Completion_Detail.Special_Box_No);
      --数据来源
      l_Item := 'Box:SOURCE_TYPE';
      Dbms_Xslprocessor.Valueof(l_Node, 'SOURCE_TYPE/text()', l_Tmp_String);
      IF Nvl(l_Tmp_String, 'MES') = 'MES' THEN
        l_Completion_Detail.Source_Type := 'MES';
      ELSE
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '数据来源与单据头不一致:' ||
                        l_Completion_Detail.Material_Batch1;
        RETURN;
      END IF;
      --工单编号
      l_Item := 'Box:CPH';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'CPH/text()',
                                l_Completion_Detail.Wip_Entity_Name);
      IF Nvl(l_Completion_Detail.Wip_Entity_Name, '#NULL') <>
         Nvl(x_Complete_Job.Wip_Entity_Name, '#NULL') THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '传票号与单据头不一致:' ||
                        l_Completion_Detail.Wip_Entity_Name;
        RETURN;
      END IF;
      --传票号
      l_Item := 'Box:GDBH';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'GDBH/text()',
                                l_Completion_Detail.Note_Number);
      IF Nvl(l_Completion_Detail.Note_Number, '#NULL') <>
         Nvl(x_Complete_Job.Note_Number, '#NULL') THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '工单编号与单据头不一致:' ||
                        l_Completion_Detail.Note_Number;
        RETURN;
      END IF;
      --产品代码
      l_Item := 'Box:CPDM';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'CPDM/text()',
                                l_Completion_Detail.Item_Number);
      IF Nvl(l_Completion_Detail.Item_Number, '') <>
         Nvl(x_Complete_Job.Item_Number, '') THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '产品代码与单据头不一致:' ||
                        l_Completion_Detail.Item_Number;
        RETURN;
      END IF;
      --产品名称
      l_Item := 'Box:CPMC';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'CPMC/text()',
                                l_Completion_Detail.Item_Desp);
      IF l_Completion_Detail.Item_Desp IS NULL THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '产品名称不能为空。';
        RETURN;
      END IF;
      --硬件版本  
      l_Item := 'Box:HARDWARE_VERSION';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'HARDWARE_VERSION/text()',
                                l_Completion_Detail.Hardware_Version);
      --软件版本  
      l_Item := 'Box:SOFTWARE_VERSION';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'SOFTWARE_VERSION/text()',
                                l_Completion_Detail.Hardware_Version);
      --确认单号  
      l_Item := 'Box:AFFIRM_ORDER_NO';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'AFFIRM_ORDER_NO/text()',
                                l_Completion_Detail.Affirm_Order_No);
      --预留栏位1  
      l_Item := 'Box:MEMOS';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'MEMO1/text()',
                                l_Completion_Detail.Memo1);
      --预留栏位2  
      Dbms_Xslprocessor.Valueof(l_Node,
                                'MEMO2/text()',
                                l_Completion_Detail.Memo2);
      --预留栏位3  
      Dbms_Xslprocessor.Valueof(l_Node,
                                'MEMO3/text()',
                                l_Completion_Detail.Memo3);
      --预留栏位4  
      Dbms_Xslprocessor.Valueof(l_Node,
                                'MEMO4/text()',
                                l_Completion_Detail.Memo4);
      --预留栏位5 
      Dbms_Xslprocessor.Valueof(l_Node,
                                'MEMO5/text()',
                                l_Completion_Detail.Memo5);
      --预留栏位6  
      Dbms_Xslprocessor.Valueof(l_Node,
                                'MEMO6/text()',
                                l_Completion_Detail.Memo6);
      --预留栏位7 
      Dbms_Xslprocessor.Valueof(l_Node,
                                'MEMO7/text()',
                                l_Completion_Detail.Memo7);
      --预留栏位8 
      Dbms_Xslprocessor.Valueof(l_Node,
                                'MEMO8/text()',
                                l_Completion_Detail.Memo8);
      --预留栏位9  
      Dbms_Xslprocessor.Valueof(l_Node,
                                'MEMO9/text()',
                                l_Completion_Detail.Memo9);
      --预留栏位10 
      Dbms_Xslprocessor.Valueof(l_Node,
                                'MEMO10/text()',
                                l_Completion_Detail.Memo10);
    
      --属性号1
      l_Item := 'Box:MATERIAL_BATCH1';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'MATERIAL_BATCH1/text()',
                                l_Completion_Detail.Material_Batch1);
      IF Nvl(l_Completion_Detail.Material_Batch1, '') <>
         Nvl(x_Complete_Job.Material_Batch1, '') THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '属性号1与单据头不一致:' ||
                        l_Completion_Detail.Material_Batch1;
        RETURN;
      END IF;
      --属性号2
      l_Item := 'Box:MATERIAL_BATCH2';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'MATERIAL_BATCH2/text()',
                                l_Completion_Detail.Material_Batch2);
      IF Nvl(l_Completion_Detail.Material_Batch2, '') <>
         Nvl(x_Complete_Job.Material_Batch2, '') THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '属性号2与单据头不一致:' ||
                        l_Completion_Detail.Material_Batch2;
        RETURN;
      END IF;
      x_Completion_Detail_Tab(x_Completion_Detail_Tab.Count + 1) := l_Completion_Detail;
    END LOOP;
  
    --获取子件信息 
    l_Node_List  := Dbms_Xmldom.Getelementsbytagname(p_Doc, 'BAR_BOX_SUB');
    l_Node_Count := Dbms_Xmldom.Getlength(l_Node_List);
    FOR i IN 1 .. l_Node_Count LOOP
      l_Completion_Detail := l_Null_Completion_Detail;
      l_Node              := Dbms_Xmldom.Item(l_Node_List, i - 1);
      SELECT Cux_Mes_Completion_Details_s.Nextval
        INTO l_Completion_Detail.Detail_Id
        FROM Dual;
      l_Completion_Detail.Request_Id            := p_Request_Id;
      l_Completion_Detail.Mes_Completion_Number := x_Complete_Job.Mes_Completion_Number;
      l_Completion_Detail.Entry_Type            := 'BAR_BOX_SUB';
    
      --子件条码
      l_Item := 'Su Box:SUB_BARCODE_NO';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'SUB_BARCODE_NO/text()',
                                l_Completion_Detail.Sub_Barcode_No);
      IF l_Completion_Detail.Sub_Barcode_No IS NULL THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '子件条码为空。';
        RETURN;
      END IF;
      l_Detail_Title := '子件条码(' || l_Completion_Detail.Sub_Barcode_No || ')';
      --箱号
      l_Item := 'Su Box:BOX_NO';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'BOX_NO/text()',
                                l_Completion_Detail.Box_No);
      IF l_Completion_Detail.Box_No IS NULL THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '箱号为空。';
        RETURN;
      END IF;
      --SN号
      l_Item := 'Su Box:BARCODE_NO';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'BARCODE_NO/text()',
                                l_Completion_Detail.Barcode_No);
      IF l_Completion_Detail.Barcode_No IS NULL THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || 'SN号为空。';
        RETURN;
      END IF;
      --子件料号
      l_Item := 'Su Box:SUB_ITEM_CODE';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'SUB_ITEM_CODE/text()',
                                l_Completion_Detail.Item_Number);
      IF l_Completion_Detail.Item_Number IS NULL THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '子件料号不能为空。';
        RETURN;
      END IF;
      --子件料号名称
      l_Item := 'Su Box:SUB_ITEM_NAME';
      Dbms_Xslprocessor.Valueof(l_Node,
                                'SUB_ITEM_NAME/text()',
                                l_Completion_Detail.Item_Desp);
      IF l_Completion_Detail.Item_Desp IS NULL THEN
        x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
        x_Error_Msg  := l_Detail_Title || '子件料号名称不能为空。';
        RETURN;
      END IF;
      --扫描时间
      l_Item := 'Su Box:SCAN_DATE';
      Dbms_Xslprocessor.Valueof(l_Node, 'SCAN_DATE/text()', l_Tmp_String);
      IF l_Tmp_String IS NOT NULL THEN
        BEGIN
          l_Completion_Detail.Scan_Date := To_Date(l_Tmp_String,
                                                   'YYYY/MM/DD HH24:MI:SS');
        EXCEPTION
          WHEN OTHERS THEN
            x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
            x_Error_Msg  := l_Detail_Title || '扫描时间日期格式不对:' || l_Tmp_String;
            RETURN;
        END;
      END IF;
    
      --SN有效性
      l_Item := 'Su Box:SN_FLAG';
      Dbms_Xslprocessor.Valueof(l_Node, 'SN_FLAG/text()', l_Tmp_String);
      IF l_Tmp_String IN ('Y', 'N') THEN
        l_Completion_Detail.Sn_Flag := l_Tmp_String;
      ELSE
        l_Completion_Detail.Sn_Flag := 'Y';
      END IF;
      x_Completion_Detail_Tab(x_Completion_Detail_Tab.Count + 1) := l_Completion_Detail;
    END LOOP;
  
  EXCEPTION
    WHEN OTHERS THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Unexp_Error;
      x_Error_Msg  := l_Item || ' ' || SQLERRM;
  END Get_Completion;
  --创建完工入库单
  PROCEDURE Create_Completion(x_Ret_Status   OUT VARCHAR2,
                              x_Error_Msg    OUT VARCHAR2,
                              p_Request      Cux_Soa_Requests%ROWTYPE,
                              x_Complete_Job IN OUT NOCOPY Cux_Mes_Complete_Jobs%ROWTYPE) IS
  
    l_Ready_Item     Cux_Ready_Item%ROWTYPE;
    l_Step           VARCHAR2(30);
    l_Available_Qty  NUMBER; --可完工数量
    l_Mmt_Over_Qty   NUMBER; --已经过账的完工数量
    l_Ready_Over_Qty NUMBER; --未过账的完工数量
  BEGIN
    x_Ret_Status := Fnd_Api.g_Ret_Sts_Success;
    --完工数量
    l_Available_Qty := Cux_Wip_Transactions_Pkg.Get_Wip_Over_Qty(x_Complete_Job.Organization_Id,
                                                                 x_Complete_Job.Wip_Entity_Id);
  
    IF x_Complete_Job.Complete_Quantity > l_Available_Qty THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := '完工入库数量大于可完工数量。';
      RETURN;
    END IF;
  
    l_Step := 'C10';
    BEGIN
      SELECT Subinventory_Code
        INTO l_Ready_Item.Supply_Subinventory
        FROM Mtl_Item_Sub_Defaults Mis
       WHERE Mis.Organization_Id = x_Complete_Job.Organization_Id
         AND Mis.Inventory_Item_Id = x_Complete_Job.Item_Id
         AND Mis.Default_Type = 2;
    EXCEPTION
      WHEN OTHERS THEN
        NULL;
    END;
    l_Step := 'C15';
    BEGIN
      SELECT Mil.Segment1, Mild.Locator_Id
        INTO l_Ready_Item.Supply_Loc_Code, l_Ready_Item.Supply_Locator_Id
        FROM Mtl_Item_Loc_Defaults Mild, Mtl_Item_Locations Mil
       WHERE 1 = 1
         AND Mild.Locator_Id = Mil.Inventory_Location_Id
         AND Mild.Organization_Id = Mil.Organization_Id
         AND Nvl(Mil.Enabled_Flag, 'N') = 'Y'
         AND Mild.Organization_Id = x_Complete_Job.Organization_Id
         AND Mild.Inventory_Item_Id = x_Complete_Job.Item_Id
         AND Mild.Default_Type = 2
         AND Mild.Subinventory_Code = x_Complete_Job.Subinventory_Code;
    EXCEPTION
      WHEN No_Data_Found THEN
        NULL;
    END;
    l_Step                       := 'C20';
    l_Ready_Item.Doc_No          := 'OR' || Cux_Doc_Or_Seq_Fw.Nextval;
    l_Ready_Item.Line_Id         := Cux.Cux_Doc_Line_Seq.Nextval;
    l_Ready_Item.Organization_Id := x_Complete_Job.Organization_Id;
    l_Ready_Item.Doc_Type        := '完工入库';
    l_Ready_Item.Doc_Date        := x_Complete_Job.Mes_Complete_Date;
    l_Ready_Item.Doc_Status      := '未过账';
    l_Ready_Item.Wip_Entity_Id   := x_Complete_Job.Wip_Entity_Id;
    l_Ready_Item.Wip_Entity_Name := x_Complete_Job.Wip_Entity_Name;
    l_Ready_Item.Primary_Item_Id := x_Complete_Job.Item_Id;
    l_Ready_Item.Ready_Qty       := x_Complete_Job.Complete_Quantity;
  
    l_Ready_Item.Now_Qty           := x_Complete_Job.Complete_Quantity; -- 下达数
    l_Ready_Item.Inventory_Item_Id := NULL; -- 物料ID
    -- 物料单位
    SELECT t.Primary_Uom_Code
      INTO l_Ready_Item.Item_Primary_Uom_Code
      FROM Mtl_System_Items_b t
     WHERE t.Organization_Id = x_Complete_Job.Organization_Id
       AND t.Inventory_Item_Id = x_Complete_Job.Item_Id;
    -- 工序编号
    SELECT Wo.Operation_Seq_Num
      INTO l_Ready_Item.Operation_Seq_Num
      FROM Wip_Operations Wo
     WHERE Wo.Organization_Id = x_Complete_Job.Organization_Id
       AND Wo.Wip_Entity_Id = x_Complete_Job.Wip_Entity_Id
       AND Wo.Next_Operation_Seq_Num IS NULL;
  
    l_Ready_Item.Operation_Code         := NULL; -- 工序代码
    l_Ready_Item.Quantity_Per_Assembly  := NULL; -- 单位需求
    l_Ready_Item.Component_Yield_Factor := NULL; -- 产出率
    l_Ready_Item.Required_Quantity      := NULL; -- 必需量
    -- 已完成量
  
    SELECT Nvl(SUM(Mmt.Transaction_Quantity), 0)
      INTO l_Mmt_Over_Qty
      FROM Mtl_Material_Transactions Mmt
     WHERE Mmt.Organization_Id = x_Complete_Job.Organization_Id
       AND Mmt.Transaction_Source_Id = x_Complete_Job.Wip_Entity_Id
       AND Mmt.Transaction_Type_Id IN (44, 17);
  
    --已生产单据完工数   
    SELECT Nvl(SUM(Nvl(Cri.Ready_Qty, 0)), 0)
      INTO l_Ready_Over_Qty
      FROM Cux_Ready_Item Cri
     WHERE Cri.Organization_Id = x_Complete_Job.Organization_Id
       AND Cri.Wip_Entity_Id = x_Complete_Job.Wip_Entity_Id
       AND Cri.Doc_Status = '未过账'
       AND Cri.Doc_Type = '完工入库';
  
    l_Ready_Item.Iss_Ready_Qty := l_Mmt_Over_Qty + l_Ready_Over_Qty;
  
    l_Ready_Item.Creation_Date      := SYSDATE;
    l_Ready_Item.Created_By         := Fnd_Global.User_Id;
    l_Ready_Item.Last_Update_Date   := SYSDATE;
    l_Ready_Item.Last_Updated_By    := Fnd_Global.User_Id;
    l_Ready_Item.Last_Update_Login  := Fnd_Global.Login_Id;
    l_Ready_Item.Create_Lot         := To_Char(Systimestamp,
                                               'YYYYMMDDHH24MISSFF4');
    l_Ready_Item.Source_System      := p_Request.Relative_System;
    l_Ready_Item.Source_Document_Id := x_Complete_Job.Mes_Completion_Number; --记录                                       
    INSERT INTO Cux_Ready_Item VALUES l_Ready_Item;
    --回写ERP完工单信息
    x_Complete_Job.Erp_Completion_Number := l_Ready_Item.Doc_No;
    x_Complete_Job.Completion_Type       := l_Ready_Item.Doc_Type;
    x_Complete_Job.Erp_Completion_Date   := l_Ready_Item.Creation_Date;
    x_Complete_Job.Subinventory_Code     := l_Ready_Item.Supply_Subinventory;
    SELECT t.Description
      INTO x_Complete_Job.Subinventory_Name
      FROM Mtl_Secondary_Inventories t
     WHERE t.Organization_Id = x_Complete_Job.Organization_Id
       AND t.Secondary_Inventory_Name = x_Complete_Job.Subinventory_Code;
    x_Complete_Job.Locator_Name := l_Ready_Item.Supply_Loc_Code;
    x_Complete_Job.Uom          := l_Ready_Item.Item_Primary_Uom_Code;
    IF Instr(x_Complete_Job.Item_Number, '-') > 0 THEN
      x_Complete_Job.Hardware_Version := Substr(x_Complete_Job.Item_Number,
                                                -3);
      x_Complete_Job.Software_Version := NULL;
    ELSE
      x_Complete_Job.Hardware_Version := Substr(x_Complete_Job.Item_Number,
                                                -5,
                                                3);
      x_Complete_Job.Software_Version := Substr(x_Complete_Job.Item_Number,
                                                -2);
    END IF;
  
  EXCEPTION
    WHEN OTHERS THEN
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg  := l_Step || '@创建完工入库单错误:' || SQLERRM;
  END Create_Completion;

  PROCEDURE Job_Complettion(p_Doc            Dbms_Xmldom.Domdocument,
                            p_Completion_Job IN CLOB,
                            x_Response       OUT CLOB) IS
    l_Request Cux_Soa_Requests%ROWTYPE;
  
    l_Request_Id NUMBER;
    l_Parser     Dbms_Xmlparser.Parser;
    l_Doc        Dbms_Xmldom.Domdocument;
  
    l_Complete_Job      Cux_Mes_Complete_Jobs%ROWTYPE;
    l_Completion_Detail Cux_Mes_Completion_Details%ROWTYPE;
  
    l_Completion_Detail_Tab Completion_Detail_Tab;
    l_Ret_Status            VARCHAR2(10);
    l_Error_Msg             VARCHAR2(200);
  
    l_Step VARCHAR2(30);
  BEGIN
    --如果取消完工入库
  
    l_Step := '000';
    --创建SOA请求
    l_Request.Direction       := 'INBOUND';
    l_Request.Interface       := 'CUX_MES_PKG';
    l_Request.Method          := 'COMPLETE_JOB';
    l_Request.Relative_System := 'MES';
    l_Request.Url             := NULL;
    l_Request.Request_Date    := SYSDATE;
    l_Request.Request_Msg     := p_Completion_Job;
    Cux_Soa_Pub.Create_Request(l_Request, l_Request_Id);
    l_Step := '010';
    SAVEPOINT Process_Completion;
  
    --获取完工信息
    Get_Completion(p_Doc                   => p_Doc,
                   p_Request_Id            => l_Request_Id,
                   x_Ret_Status            => l_Ret_Status,
                   x_Error_Msg             => l_Error_Msg,
                   x_Complete_Job          => l_Complete_Job,
                   x_Completion_Detail_Tab => l_Completion_Detail_Tab);
    IF l_Ret_Status <> Fnd_Api.g_Ret_Sts_Success THEN
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
    l_Step := '020';
    --创建完工入库单
    Create_Completion(x_Ret_Status   => l_Ret_Status,
                      x_Error_Msg    => l_Error_Msg,
                      p_Request      => l_Request,
                      x_Complete_Job => l_Complete_Job);
    IF l_Ret_Status <> Fnd_Api.g_Ret_Sts_Success THEN
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
    --写入数据
    INSERT INTO Cux_Mes_Complete_Jobs VALUES l_Complete_Job;
  
    FOR i IN 1 .. l_Completion_Detail_Tab.Count LOOP
      INSERT INTO Cux_Mes_Completion_Details
      VALUES l_Completion_Detail_Tab
        (i);
    END LOOP;
  
    COMMIT;
    x_Response := Response_Completion(l_Ret_Status,
                                      l_Error_Msg,
                                      l_Complete_Job);
  EXCEPTION
    WHEN Fnd_Api.g_Exc_Error THEN
      l_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      ROLLBACK TO Process_Completion;
      l_Error_Msg := l_Step || ':' || l_Error_Msg;
      x_Response  := Response_Completion(l_Ret_Status,
                                         l_Error_Msg,
                                         l_Complete_Job);
    WHEN OTHERS THEN
      l_Ret_Status := Fnd_Api.g_Ret_Sts_Unexp_Error;
      ROLLBACK TO Process_Completion;
      l_Error_Msg := '执行发生异常(' || l_Step || '):' || SQLERRM;
      x_Response  := Response_Completion(l_Ret_Status,
                                         l_Error_Msg,
                                         l_Complete_Job);
    
  END Job_Complettion;

  PROCEDURE Cancel_Completion(p_Doc            Dbms_Xmldom.Domdocument,
                              p_Completion_Job IN CLOB,
                              x_Response       OUT CLOB) IS
    l_Request    Cux_Soa_Requests%ROWTYPE;
    l_Request_Id NUMBER;
  
    l_Complete_Job Cux_Mes_Complete_Jobs%ROWTYPE;
  
    l_Ret_Status VARCHAR2(10);
    l_Error_Msg  VARCHAR2(200);
  
    CURSOR Cur_Completion_Job IS
      SELECT t.Line_Id
        FROM Cux_Ready_Item t
       WHERE t.Source_System = 'MES'
         AND t.Source_Document_Id = l_Complete_Job.Mes_Completion_Number
         AND t.Doc_Status = '未过账'
         FOR UPDATE NOWAIT;
    l_Completion_Id NUMBER;
    Exp_Resource_Busy EXCEPTION;
    PRAGMA EXCEPTION_INIT(Exp_Resource_Busy, -54);
  
    l_Step VARCHAR2(30);
  BEGIN
    --创建SOA请求
    l_Request.Direction       := 'INBOUND';
    l_Request.Interface       := 'CUX_MES_PKG';
    l_Request.Method          := 'CANCEL_COMPLETION';
    l_Request.Relative_System := 'MES';
    l_Request.Url             := NULL;
    l_Request.Request_Date    := SYSDATE;
    l_Request.Request_Msg     := p_Completion_Job;
    Cux_Soa_Pub.Create_Request(l_Request, l_Request_Id);
    SAVEPOINT Process_Cancel_Completion;
  
    --获取取消完工信息
    Get_Cancel_Completion(p_Doc          => p_Doc,
                          p_Request_Id   => l_Request_Id,
                          x_Ret_Status   => l_Ret_Status,
                          x_Error_Msg    => l_Error_Msg,
                          x_Complete_Job => l_Complete_Job);
    IF l_Ret_Status <> Fnd_Api.g_Ret_Sts_Success THEN
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
  
    --处理完工信息
    BEGIN
      OPEN Cur_Completion_Job;
      FETCH Cur_Completion_Job
        INTO l_Completion_Id;
    EXCEPTION
      WHEN Exp_Resource_Busy THEN
        l_Error_Msg := '完工单正在被处理:' || l_Complete_Job.Mes_Completion_Number;
        RAISE Fnd_Api.g_Exc_Error;
      WHEN OTHERS THEN
        l_Error_Msg := '读取系统完工单失败:' || SQLERRM;
        RAISE Fnd_Api.g_Exc_Error;
    END;
  
    IF l_Completion_Id IS NOT NULL THEN
      UPDATE Cux_Ready_Item t
         SET t.Doc_Status = '已过账', t.Now_Qty = 0, t.Ready_Qty = 0
       WHERE t.Line_Id = l_Completion_Id;
    ELSE
      l_Error_Msg := '单据已过账，无法取消。';
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
    CLOSE Cur_Completion_Job;
  
    COMMIT;
    x_Response := Response_Cancel_Completion(l_Ret_Status,
                                             l_Error_Msg,
                                             l_Complete_Job);
  EXCEPTION
    WHEN Fnd_Api.g_Exc_Error THEN
      IF Cur_Completion_Job%ISOPEN THEN
        CLOSE Cur_Completion_Job;
      END IF;
      l_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
      ROLLBACK TO Process_Cancel_Completion;
      l_Error_Msg := l_Step || ':' || l_Error_Msg;
      x_Response  := Response_Cancel_Completion(l_Ret_Status,
                                                l_Error_Msg,
                                                l_Complete_Job);
    WHEN OTHERS THEN
      IF Cur_Completion_Job%ISOPEN THEN
        CLOSE Cur_Completion_Job;
      END IF;
      l_Ret_Status := Fnd_Api.g_Ret_Sts_Unexp_Error;
      ROLLBACK TO Process_Cancel_Completion;
      l_Error_Msg := '执行发生异常(' || l_Step || '):' || SQLERRM;
      x_Response  := Response_Cancel_Completion(l_Ret_Status,
                                                l_Error_Msg,
                                                l_Complete_Job);
  END Cancel_Completion;

  PROCEDURE Complete_Job(p_Completion_Job IN CLOB, x_Response OUT CLOB) IS
    l_Completion_Job CLOB;
    l_Request        Cux_Soa_Requests%ROWTYPE;
    l_Request_Id     NUMBER;
  
    l_Parser Dbms_Xmlparser.Parser;
    l_Doc    Dbms_Xmldom.Domdocument;
  
    l_Node_List  Dbms_Xmldom.Domnodelist;
    l_Node       Dbms_Xmldom.Domnode;
    l_Tmp_String VARCHAR2(500);
    l_Status     VARCHAR2(1);
  BEGIN
    l_Status         := Fnd_Api.g_Ret_Sts_Success;
    l_Completion_Job := p_Completion_Job;
    --创建临时SOA记录，解析成功后删除
    l_Request.Direction       := 'INBOUND';
    l_Request.Interface       := 'CUX_MES_PKG';
    l_Request.Method          := 'COMPLETION_IFACE';
    l_Request.Relative_System := 'MES';
    l_Request.Url             := NULL;
    l_Request.Request_Date    := SYSDATE;
    l_Request.Request_Msg     := l_Completion_Job;
    Cux_Soa_Pub.Create_Request(l_Request, l_Request_Id);
  
    --创建XML对象
    l_Parser := Dbms_Xmlparser.Newparser;
    BEGIN
      Dbms_Xmlparser.Parseclob(l_Parser, l_Completion_Job);
      l_Doc := Dbms_Xmlparser.Getdocument(l_Parser);
    
      l_Node_List := Dbms_Xmldom.Getelementsbytagname(l_Doc, 'STD_IN');
      l_Node      := Dbms_Xmldom.Item(l_Node_List, 0);
      --ObjectID
      Dbms_Xslprocessor.Valueof(l_Node, 'ObjectID/text()', l_Tmp_String);
      --解析成功后删除临时SOA记录
      IF l_Tmp_String IN ('WGRK_INTERFACE', 'WGRKQX_INTERFACE') THEN
        DELETE Cux_Soa_Requests t WHERE t.Request_Id = l_Request_Id;
        COMMIT;
      END IF;
    EXCEPTION
      WHEN OTHERS THEN
        l_Status := Fnd_Api.g_Ret_Sts_Error;
    END;
  
    IF Nvl(l_Tmp_String, 'NULL') = 'WGRK_INTERFACE' THEN
      --完工入库
      Job_Complettion(l_Doc, p_Completion_Job, x_Response);
    ELSIF Nvl(l_Tmp_String, 'NULL') = 'WGRKQX_INTERFACE' THEN
      --取消完工入库
      Cancel_Completion(l_Doc, p_Completion_Job, x_Response);
    ELSE
      x_Response := x_Response || '<STD_OUT origin="TIPTOP">';
      x_Response := x_Response || '<Service Name="SetData">';
      x_Response := x_Response || '<Status>1</Status>';
      IF l_Status = Fnd_Api.g_Ret_Sts_Error THEN
        x_Response := x_Response || '<Error>解析完工入库数据失败</Error>';
      ELSE
        x_Response := x_Response || '<Error>ObjectID为空或者错误:' ||
                      l_Tmp_String || '</Error>';
      END IF;
      x_Response := x_Response || '<OperDate>' ||
                    To_Char(SYSDATE, 'YYYY/MM/DD HH24:MI:SS') ||
                    '</OperDate>'; --操作时间
      x_Response := x_Response || '</Service></STD_OUT>';
    END IF;
  END Complete_Job;

  PROCEDURE Update_Transfer_Request(p_Request Cux_Soa_Requests%ROWTYPE) IS
  BEGIN
    UPDATE Cux_Soa_Requests t
       SET t.Response_Date   = SYSDATE
          ,t.Response_Msg    = p_Request.Response_Msg
          ,t.Response_Status = p_Request.Response_Status
          ,t.Entity_Type     = p_Request.Entity_Type
          ,t.Entity_Id       = p_Request.Entity_Id
          ,t.Error_Msg       = p_Request.Error_Msg
     WHERE t.Request_Id = p_Request.Request_Id;
    COMMIT;
  END Update_Transfer_Request;

  --返回过账信息
  PROCEDURE Post_Completion_2_Mes(x_Ret_Status    OUT VARCHAR2,
                                  x_Error_Msg     OUT VARCHAR2,
                                  p_Completion_Id NUMBER) IS
    CURSOR Cur_Completion IS
      SELECT t.Doc_Status
            ,t.Transfer_Mes_Flag
            ,t.Source_Document_Id
            ,t.Source_System
            ,t.Now_Qty
        FROM Cux_Ready_Item t
       WHERE t.Line_Id = p_Completion_Id;
    l_Doc_Status            VARCHAR2(30);
    l_Transfer_Mes_Flag     VARCHAR2(1);
    l_Mes_Completion_Number VARCHAR2(30);
    l_Source_System         VARCHAR2(10);
    l_Now_Qty               NUMBER;
  
    l_Context_Msg    CLOB;
    l_Request_Msg    CLOB;
    l_Request_Length NUMBER;
  
    l_Service_Url VARCHAR2(200);
    l_Soa_Method  VARCHAR2(50);
  
    l_Http_Req  Utl_Http.Req;
    l_Http_Resp Utl_Http.Resp;
  
    l_Buffer_Size  BINARY_INTEGER := 1000;
    l_Offset       INTEGER;
    l_Buffer       VARCHAR2(3000);
    l_Resp_Clob    CLOB;
    l_Resp_Str     VARCHAR2(1000);
    l_Resp_Code    VARCHAR2(10);
    l_Resp_Message VARCHAR2(500);
  
    l_Request    Cux_Soa_Requests%ROWTYPE;
    l_Request_Id NUMBER;
  BEGIN
    x_Ret_Status := Fnd_Api.g_Ret_Sts_Success;
    --获取URL
    l_Service_Url := Cux_External_Interface_Pkg.Getinterfaceurl('MES');
    l_Soa_Method  := 'GenerateMesInfo';
  
    OPEN Cur_Completion;
    FETCH Cur_Completion
      INTO l_Doc_Status
          ,l_Transfer_Mes_Flag
          ,l_Mes_Completion_Number
          ,l_Source_System
          ,l_Now_Qty;
    IF Cur_Completion%ROWCOUNT = 0 THEN
      x_Error_Msg := '系统不存在该记录。';
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
    --如果来源非MES系统，不需要回传信息
    IF Nvl(l_Source_System, '#NULL') <> 'MES' OR
       l_Mes_Completion_Number IS NULL THEN
      RETURN;
    END IF;
  
    IF l_Doc_Status <> '已过账' THEN
      x_Error_Msg := '该单据未过账。';
      RAISE Fnd_Api.g_Exc_Error;
    ELSIF Nvl(l_Transfer_Mes_Flag, 'N') = 'Y' THEN
      x_Error_Msg := '该单据已经抛转MES。';
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
    CLOSE Cur_Completion;
    --内容信息
  
    l_Context_Msg := '<?xml version="1.0" encoding="utf-8" ?><STD_IN>';
    l_Context_Msg := l_Context_Msg || '<EDI_user>MESUSER</EDI_user>';
    l_Context_Msg := l_Context_Msg || '<EDI_pwd>MESUPWD</EDI_pwd>';
    l_Context_Msg := l_Context_Msg ||
                     '<ServiceName>GenerateMesInfo</ServiceName>';
    l_Context_Msg := l_Context_Msg || '<OperationCode></OperationCode>';
    l_Context_Msg := l_Context_Msg || '<ObjectID>InStock</ObjectID>';
    l_Context_Msg := l_Context_Msg || '<Service>';
    l_Context_Msg := l_Context_Msg || '<Data>';
    l_Context_Msg := l_Context_Msg || '<DJBH>' || l_Mes_Completion_Number ||
                     '</DJBH>';
    --取消数量为0，回传0
    --过账成功回传1
    l_Context_Msg := l_Context_Msg || '<ZT>' || Sign(l_Now_Qty) || '</ZT>';
  
    l_Context_Msg := l_Context_Msg || '</Data>';
    l_Context_Msg := l_Context_Msg || '</Service>';
    l_Context_Msg := l_Context_Msg || '</STD_IN>';
  
    --记录SOA请求
    l_Request.Entity_Type     := 'WIP_COMPLETE';
    l_Request.Entity_Id       := l_Mes_Completion_Number;
    l_Request.Direction       := 'OUTBOUND';
    l_Request.Interface       := 'CUX_MES_PKG';
    l_Request.Method          := 'POST_COMPLETION_2_MES';
    l_Request.Relative_System := 'MES';
    l_Request.Url             := l_Service_Url;
    l_Request.Request_Date    := SYSDATE;
    l_Request.Request_Msg     := l_Context_Msg;
    Cux_Soa_Pub.Create_Request(l_Request, l_Request_Id);
  
    --请求信息
    l_Request_Msg    := '<?xml version="1.0" encoding="utf-8"?>';
    l_Request_Msg    := l_Request_Msg ||
                        '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">';
    l_Request_Msg    := l_Request_Msg || '<soap:Body>';
    l_Request_Msg    := l_Request_Msg || '<' || l_Soa_Method ||
                        ' xmlns="http://tempuri.org/">';
    l_Request_Msg    := l_Request_Msg || '<stdin>';
    l_Request_Msg    := l_Request_Msg || '<![CDATA[' || l_Context_Msg ||
                        ']]>';
    l_Request_Msg    := l_Request_Msg || '</stdin>';
    l_Request_Msg    := l_Request_Msg || '</' || l_Soa_Method || '>';
    l_Request_Msg    := l_Request_Msg || '</soap:Body>';
    l_Request_Msg    := l_Request_Msg || '</soap:Envelope>';
    l_Request_Length := Dbms_Lob.Getlength(l_Request_Msg);
  
    l_Http_Req := Utl_Http.Begin_Request(l_Service_Url,
                                         'POST',
                                         Utl_Http.Http_Version_1_1);
    -- 保持连接状态
    Utl_Http.Set_Persistent_Conn_Support(l_Http_Req, TRUE);
    --设置编码
    Utl_Http.Set_Header(l_Http_Req,
                        'Content-Type',
                        'text/xml; charset=utf-8');
    Utl_Http.Set_Header(l_Http_Req,
                        'SOAPAction',
                        '"http://tempuri.org/' || l_Soa_Method || '"');
    Utl_Http.Set_Body_Charset(l_Http_Req, 'utf-8');
    Utl_Http.Set_Header(l_Http_Req, 'Content-Length', l_Request_Length);
  
    l_Offset := 1;
    WHILE l_Offset < l_Request_Length LOOP
      Dbms_Lob.Read(l_Request_Msg, l_Buffer_Size, l_Offset, l_Buffer);
      Utl_Http.Write_Raw(l_Http_Req, Utl_Raw.Cast_To_Raw(l_Buffer));
      l_Offset := l_Offset + l_Buffer_Size;
    END LOOP;
  
    l_Http_Resp := Utl_Http.Get_Response(l_Http_Req);
  
    IF (l_Http_Resp.Status_Code = Utl_Http.Http_Ok) THEN
      BEGIN
        LOOP
          Utl_Http.Read_Text(l_Http_Resp, l_Resp_Str, 1000);
          l_Resp_Clob := l_Resp_Clob || l_Resp_Str;
        END LOOP;
        Utl_Http.End_Response(l_Http_Resp);
      EXCEPTION
        WHEN Utl_Http.End_Of_Body THEN
          Utl_Http.End_Response(l_Http_Resp);
      END;
    ELSE
      x_Error_Msg := '提交service失败:' || l_Http_Resp.Status_Code;
      RAISE Fnd_Api.g_Exc_Error;
    END IF;
  
    l_Request.Response_Date := SYSDATE;
    --l_Request.Response_Msg  := l_Resp_Clob;
    ---解析返回结果
    Parser_Resp_Clob(l_Resp_Clob,
                     l_Request.Response_Msg,
                     l_Resp_Code,
                     l_Resp_Message);
  
    IF Nvl(l_Resp_Code, '1') = '0' THEN
      UPDATE Cux_Ready_Item t
         SET t.Transfer_Mes_Flag = 'Y'
       WHERE t.Line_Id = p_Completion_Id;
      COMMIT;
      l_Request.Response_Status := 'S';
      l_Request.Error_Msg       := l_Resp_Message;
    ELSE
      x_Ret_Status              := Fnd_Api.g_Ret_Sts_Error;
      x_Error_Msg               := l_Resp_Message;
      l_Request.Response_Status := 'F';
      l_Request.Error_Msg       := l_Resp_Message;
    END IF;
  
    --记录SOA请求
    Update_Transfer_Request(l_Request);
  EXCEPTION
    WHEN Fnd_Api.g_Exc_Error THEN
      IF Cur_Completion%ISOPEN THEN
        CLOSE Cur_Completion;
      END IF;
      l_Request.Error_Msg := x_Error_Msg;
      Update_Transfer_Request(l_Request);
      x_Ret_Status := Fnd_Api.g_Ret_Sts_Error;
    WHEN OTHERS THEN
      IF Cur_Completion%ISOPEN THEN
        CLOSE Cur_Completion;
      END IF;
      x_Ret_Status        := Fnd_Api.g_Ret_Sts_Unexp_Error;
      x_Error_Msg         := SQLERRM;
      l_Request.Error_Msg := x_Error_Msg;
      Update_Transfer_Request(l_Request);
  END Post_Completion_2_Mes;
END;
/
