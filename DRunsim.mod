{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Definition Module     : DRunsim                                               +}
{+  Author                : Chuck Carter / Tony Malerich                          +}
{+  Last Modified         : Feb 06                                                +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

DEFINITION MODULE Runsim;

FROM Objects IMPORT realArray,intArray,strArray;
FROM StatMod IMPORT RStatObj,ITimedStatObj;
FROM SimMod  IMPORT SimControlObj,ActivityGroup,TriggerObj; 
FROM IOMod   IMPORT StreamObj;
FROM ResMod  IMPORT ResourceObj;
FROM Intract IMPORT ResultsBoxObj;
FROM GrpMod  IMPORT QueueObj; 
FROM Check   IMPORT CheckBoxObj;
FROM Analyze IMPORT RandomGenObj;
  
TYPE
  SystemStatusType=(GGreen,YYellow,RRed,DDone); 
  MissionStatusType=(Mission,NonMission);                            
  FinalArrayType = ARRAY INTEGER, INTEGER OF STRING;
   
SimBasicObj = OBJECT;
  objectNum,connectIntoNum,GoodPathsRequired,EFPAConnectOut,MinGoodPathsReq,
  NumGoodPaths,NumActivePaths                                                 : INTEGER;
  usesPhasing                                                                 : BOOLEAN;
  connectToIDs,EFPAConnectPath,phase                                          : ARRAY INTEGER OF INTEGER;
END OBJECT;

SystemObj = OBJECT;
  redCost          : REAL;
  Status           : SystemStatusType;
  missionStatus    : MissionStatusType;
  ASK METHOD ChangeStatus(IN newColor:SystemStatusType; IN newStatus : MissionStatusType);
  ASK METHOD Initialize;   
  ASK METHOD SetToInitValues;   
  ASK METHOD FEVPhaseChange(IN newPhase : INTEGER);
  ASK METHOD SetMissionStatus (IN newValue : MissionStatusType);
  ASK METHOD WriteCumParamFile();                                  {cmc 3/6/07}
  TELL METHOD EndSimulation(IN abortstatus,keepStats : BOOLEAN); 
  TELL METHOD CallEndSimulation(IN abortstatus,keepStats : BOOLEAN); 
  TELL METHOD UpdateSimStatus(IN termTime : REAL);
  TELL METHOD ControlPhaseChanges();
  TELL METHOD ConductPhaseChange(IN newPhase : INTEGER);
  TELL METHOD ResetAllStatistics(); 
  TELL METHOD GenerateAoPlotData();
  TELL METHOD CalculateInterimCost();
  TELL METHOD UpdateCumulativeValues(IN intervalTime : REAL);      {cmc 3/6/07}
END OBJECT;

PoolObj = OBJECT(ResourceObj); 
  UsedUp,NumEmerOrdered    :  INTEGER;
  ASK METHOD Initialize;
  ASK METHOD IncrementUsedUp;
  ASK METHOD IncNumEmerOrdered;
  ASK METHOD DecNumEmerOrdered;      
  TELL METHOD GenerateSpares(IN i : INTEGER);      
  TELL METHOD OrderASpare(IN i : INTEGER);                                                             
  TELL METHOD OrderStock(IN i, quantity : INTEGER); 
END OBJECT;  {PoolObj}

RBDSimControlObj = OBJECT(SimControlObj);
OVERRIDE
  ASK METHOD ChooseNext(IN group : ActivityGroup) : ACTID;     
OVERRIDE
  ASK METHOD TimeAdvance(IN newTime : REAL) : REAL;   
END OBJECT;

PROCEDURE StartEngine(IN GraphicsOutput,SysFailTimeFile,SysRepTimeFile,EndSimTimeFile,paramFile,
                         AvailPlotFile,CapFile,ResultsFile,EORFile,EventsFile,CostFile,AvailGraph    : BOOLEAN; 
                      IN CostPlotInterval,AoPlotInterval,simLength,startStat,TimeSlice,rMesh         : REAL;
                      IN NumRuns,RInterval                                                           : INTEGER);
PROCEDURE OpenOutFiles (INOUT RInterval : INTEGER);
PROCEDURE CloseOutFiles(IN RInterval : INTEGER);
PROCEDURE ProduceCostResults();
PROCEDURE REALTOSigFigSTR (IN inVal : REAL;IN sigFig : INTEGER)               : STRING;
PROCEDURE REALTOSN (IN inVal :REAL; IN  strSize,sigFig,justification:INTEGER) : STRING;
PROCEDURE REALTOCOST (IN inVal :REAL;IN strSize,justification : INTEGER)      : STRING;
PROCEDURE COSTTOREAL (IN inVal : STRING)                                      : REAL;
PROCEDURE PopToSample (IN sigmaPop : REAL; IN  n : INTEGER)                   : REAL;
PROCEDURE FillStatusBar();
PROCEDURE UpdateNodeArray;
PROCEDURE UpdateBlockArray;
PROCEDURE UpdateEventArray;
PROCEDURE UpdateHierArray;
PROCEDURE StartFEVmode;
PROCEDURE EndFEVmode;
PROCEDURE GetDisplayStr  (IN newReal : REAL; IN count,a,b,c : INTEGER; IN key : STRING) : STRING; 
PROCEDURE FormatStats (IN stat : REAL)                                        : STRING;
PROCEDURE CreateFMECAarray;
PROCEDURE WriteFMECAtoFile;
PROCEDURE RefreshAllFEV;
PROCEDURE RefreshPhase;
PROCEDURE WriteSPFfile;
PROCEDURE CalculateInterimRams();
PROCEDURE OutputToFR(IN inStr : STRING) : STRING;
PROCEDURE RecordReliabilityFileData;
    
    
VAR
  NumCurrentlyFailed,phaseNumber,NumRunsCompleted,termCounter, lastFlow  {cmc capFix} : INTEGER;       
  StopCriteria,StartStats,TimeOfCompFailure,LastUpDownChange,
  StatsStartTime                                               : REAL;
  System                                                       : SystemObj; 
  StatsReset,exploded,GraphicsOutput,SystemUp,SysRepTimeFile,
  EventsFile,CostFile,CapFile,StatsStarted,OutOfSpares,
  PhaseChangeInProgress,runCompleted,simInProgress,AvailGraph,
  autoStopped,FEVmode,CountFailure                             : BOOLEAN;   
  dontShow                                                     : CheckBoxObj;
  FinalArray,SparesArray,BlockCostArray,EventCostArray,
  CapacityArray,NodeArray,ResourcesArray,SpPoolsArray,
  BlockArray,LogArray,EventArray,HierArray                     : FinalArrayType; 
  resultsBox                                                   : ResultsBoxObj;
  LinkValue,SparesUsed,currNumWaits,currNumArrivals,
  currInitNumber,blockMCompleted,eventMCompleted,
  nodeMCompleted,hierDE                                        : ARRAY INTEGER OF INTEGER;
  BlockOpCost,BlockRepCost,BlockSpCost,BlockIdleCost,
  BlockHoldCost,BlockSBCost,BlockDoneCost,BlockESCost,
  BlockDispCost,NodeGreenTime,NodeYellowTime,NodeRedTime,
  NodeBlueTime,NodeBrownTime,NodeOrangeTime,PoolAddCost,
  PoolESCost,BlockPMCost,BlockpmHoldCost,BlockRunTime,
  BlockStandbyTime,BlockRepairTime,BlockRepHoldTime,BlockIdleTime,
  BlockPMTime,BlockPMHoldTime,BlockDoneTime,EventRunTime,
  EventStandbyTime,EventRepairTime,EventOpCost,EventRepCost    : ARRAY INTEGER OF REAL;
  FailTimeStream,RepTimeStream,CapacityStream,EventStream,
  C130Stream   {cmc}                                                : StreamObj;
  FailStream,RepairStream,SysStream,EventFailStream            : ARRAY INTEGER OF RandomGenObj; 
  BlockDepGroup,NodeDepGroup,ColdChangeGroup,NodeFailureGroup,
  pmDeferGroup,FevDepGroup,nodeHasDepsGroup, blockHasDepsGroup,
  eventHasDepsGroup                                            : QueueObj;       
  CompTimeToFail,CompTimeToDoPM,CompTimeToRepair,
  SysTimeToRepair                                              : LMONITORED REAL BY RStatObj;
  BlockTrigger                                                 : ARRAY INTEGER OF TriggerObj;
  PooledSpares                                                 : ARRAY INTEGER OF PoolObj;  
  CustomSpares                                                 : ARRAY INTEGER OF ResourceObj;   
  SparesAvailable,PoolResAvailable,NodeFlow,NodeCapacity       : ARRAY INTEGER OF LMONITORED INTEGER BY ITimedStatObj;
  tab                                                          : CHAR;   
  redString                                                    : STRING;
  blockMissionBool,nodeMissionBool,eventMissionBool            : ARRAY INTEGER OF BOOLEAN;
  pmSyncTrigger                                                : TriggerObj;
  FMECAarray                                                   : strArray;
END MODULE.




