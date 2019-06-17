{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Definition Module : Print                                                     +}
{+  Author            : Steve Brown / Tony Malerich                               +}
{+  Last Modified     : September 2004                                            +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

DEFINITION MODULE Print;

FROM GTable  IMPORT TableObj;
FROM Objects IMPORT strArray;

TYPE
  timeType = FIXED ARRAY [1..10] OF REAL;
  PROCEDURE PrintInputTable (IN printee : TableObj;  IN type,action : STRING);     
  PROCEDURE PrintSystemInput (IN printArray : strArray);
  PROCEDURE SaveSystemInput  (IN printArray : strArray);
  PROCEDURE PrintLogisticsOutput (IN printee : TableObj);
  PROCEDURE SaveLogisticsOutput (IN printee : TableObj);
  PROCEDURE PrintSummaryOutput (IN printee : TableObj);
  PROCEDURE SaveSummaryOutput (IN printee : TableObj);
  PROCEDURE PrintSparingOutput (IN printee : TableObj);
  PROCEDURE SaveSparingOutput (IN printee : TableObj);
  PROCEDURE PrintCapacityOutput (IN printee : TableObj);
  PROCEDURE SaveCapacityOutput (IN printee : TableObj);
  PROCEDURE PrintAnalOutput (IN printee : TableObj; IN Input : STRING);
  PROCEDURE SaveAnalOutput(IN printee : TableObj; IN Input : STRING);
  PROCEDURE PrintCostOutput (IN printee : TableObj);
  PROCEDURE SaveCostOutput (IN printee : TableObj);
  PROCEDURE PrintCostInput (IN printee : TableObj);
  PROCEDURE SaveCostInput (IN printee : TableObj);
  PROCEDURE PrintEventPhases (IN printee : TableObj);
  PROCEDURE SaveEventPhases (IN printee : TableObj);
  PROCEDURE PrintNodePhases (IN printee : TableObj);
  PROCEDURE SaveNodePhases (IN printee : TableObj);
  PROCEDURE PrintBlockPhases (IN printee : TableObj);
  PROCEDURE SaveBlockPhases (IN printee : TableObj);
  PROCEDURE PrintHierPhases (IN printee : TableObj);
  PROCEDURE SaveHierPhases (IN printee : TableObj);
  PROCEDURE ExportRBD(IN print : BOOLEAN; IN numPages,printLevel : INTEGER; IN dir : STRING); 
  PROCEDURE PrintSelected; 
  PROCEDURE PrintPhases (IN printee : TableObj);        
  PROCEDURE SavePhases (IN printee : TableObj); 
  PROCEDURE PrintFMECA (IN printee : strArray); 
  PROCEDURE PrintTree;
  PROCEDURE FillTreeLevels (IN tab : STRING; IN parent : INTEGER; IN textStrings : strArray);  
 
END MODULE. {dmod Print}

