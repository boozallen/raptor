{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Implementation Module : IRunsim                                               +}
{+  Author                : Chuck Carter / Tony Malerich                          +}
{+  Last Modified         : June 2007 CMC                                         +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
                                                                                         
IMPLEMENTATION MODULE Runsim; 
                                                                
FROM MathMod IMPORT LOG10,POWER,FLOOR,SQRT;
FROM SimMod  IMPORT SimTime,StartSimulation,StopSimulation,Timescale,ResetSimTime,ActivityGroup,ActivityOwner,TriggerObj,
                    SimControlObj,ActivityName,Interrupt;  
FROM StatMod IMPORT ITimedStatObj,RStatObj,IStatObj;
FROM IOMod   IMPORT StreamObj, FileUseType(Input, Output, Update, Append);
FROM GrpMod  IMPORT QueueObj;   
FROM Display IMPORT window,nameOfFile,simMenuBar,sound,soundIsOn,startId,endId,simulated,root,totalBlocks,totalNodes,
                    totalLinks,totalPools,PoolArray,sysStreams,phaseTimes,activePhases,AoGraph,IntAoGraph,MultiRunGraph, 
                    currentGraph,dialogs,AoGraphWindow,faceBoxOpen,menubar,soundsPath,costAnalysis,weakAnalysis, 
                    capacityAnalysis,systemRedCost,flowGenerated,statusBarOn,dZeroFail,simToolBar,hitXinSim,sysLostCost, 
                    phaseObjArray,diagnostics,diagnosticsStream,{installPath,}diagFile,termType,DomainTree,CoreFile,
                    blockGroup,eventGroup,hierGroup,linkGroup,totalEvents,password,C130File;
FROM Intract IMPORT SendAlert,returnToRBD;
FROM Label   IMPORT LabelObj;
FROM Objects IMPORT RBDBlockObj,RBDNodeObj,RBDHierObj,LinkObj,ALL BlockStatusType,ALL NodeStatusType,ALL ActiveStatusType,
                    ALL LinkStatusType,DetermineMaxCapacity,FlowingAtMaxCap,ALL SparingType,totalSpares,totalCold, 
                    totalRes,poolGroup,SparePoolObj,ALL directionType,AnalyzeSystem,CheckColds,SystemStateChange,
                    RapTriggerObj,triggerGroup,PhaseObj,WriteEvents,critFactor,RBDEventObj,ALL EventStatusType,
                    RBDBasicObj, deferTrig;
FROM GTypes  IMPORT ALL ColorType,TextBufferType,ALL SysCursorType;
FROM GTable  IMPORT TableObj;
FROM Menu    IMPORT MenuItemObj;
FROM GProcs  IMPORT HandleEvents;
FROM OSMod   IMPORT FileExists;
FROM FileFx  IMPORT SaveAsFile;
FROM Text    IMPORT TextObj;
FROM Analyze IMPORT BlockAlphaSort,NodeAlphaSort,totalCapNodes,FetchRaptorSeed,CreateDomainTree,
                    EventAlphaSort,HierAlphaSort,randNumCount;
FROM UtilMod IMPORT DateTime;
FROM GPalet  IMPORT PaletteButtonObj;
FROM Menubar IMPORT inStepMode,inJumpMode,inEndState,goToNext,FEVStream;


VAR
   runNumber,NumRuns,missionsCompleted,missionsAttempted,missionsBegan,
   currentPhase,startTag                                                : INTEGER;
   SimCompleted,SpareMonsCreated,result,BlockCostMonsSet,PoolCostMonsSet,
   EndSpMonCreated,capacityMonsSet,weakAnalysisMonsSet,Aborted,
   KeepSimStats,KeepRunStats,NodeFile,keyParamFile,SysFailTimeFile,
   SparesFile, EORFile, ResultsFile,TieBreaking, EndSimTimeFile,
   AvailPlotFile,EndSimCalled,ZeroInitCost,MTBDEsusp,MTBMsusp,MTBMsupdated,
   MTBMuupdated,MTBDEupdated,MDTupdated,MTBMupdated,MMTsupdated,MMTupdated,
   MMTuupdated,MTBMssusp,MTBMususp,preFEVcost, preFEVnode,preFEVgraphics,
   missionSuccess,earlyAbort,jumpStop,didPM                             : BOOLEAN;      
   SysTimeToFail,GreenTime,YellowTime,RedTime,Availability,MTBDE,MDT,
   MTBMu,MTBM,MTBMs,MMTs,MMT,MMTu,Term1Condition,GreenPercent,YellowPercent,
   RedPercent,GTotal,SystemEndRedCost,SystemEndLostCost,UnitsProduced,
   MissionGrnTime,MissionYelTime,MissionRedTime,Dependability,MissionGrnPer,
   MissionYelPer,MissionRedPer,Reliability,RCond,Term2Cond,Term3Cond    : LMONITORED REAL BY RStatObj; 
   LastColorChange,AoPlotInterval,CostPlotInterval,TotalTime,
   LastMissionChange,MissionTotalTime,TotalRedCost,cyclesCount,relMesh,kpfTimeMesh : REAL;       {cmc 3/6/07}
   SparesTrackingGroup,BlockPhaseChangeGroup,NodePhaseChangeGroup,
   BlockPhaseImproveGroup,NodePhaseImproveGroup,BlockPhaseDegradeGroup,
   NodePhaseDegradeGroup,BlockPhaseStressGroup                          : QueueObj; 
   AveNumSpares,NodeEndGreen,NodeEndYellow,NodeEndRed,NodeEndBrown,
   NodeEndBlue,NodeEndAo,NodeEndDo,NodeEndR,BlockEndOpCost,
   BlockEndRepCost,BlockEndSpCost,BlockEndIdleCost,BlockEndHoldCost,     
   BlockEndSBCost,BlockEndDoneCost,BlockEndDispCost,PoolEndAddCost,
   PoolEndESCost,MultiRunData,BlockEndESCost,NodeEndOrange,
   NodeUsage,NodeAveFlow,NodeAveCap,NodeCapability,
   BlockEndPMCost,BlockEndpmHoldCost,BlockEndRun,BlockEndStandby,
   BlockEndRep,BlockEndRepHold,BlockEndIdle,BlockEndPM,BlockEndPMHold,
   BlockEndDone,BlockEndAo,BlockEndDo,BlockEndR,hMTBDE,hMDT,
   EventEndOpCost,EventEndRepCost,EventEndRun,EventEndStandby,
   EventEndRep,EventEndAo,EventEndDo,EventEndR,
   CumAoByTime,CumMTBDEbyTime,CumMDTbyTime,CumMTBMbyTime,CumMMTbyTime  {cmc 3/6/07}  : ARRAY INTEGER OF LMONITORED REAL BY RStatObj;     
   MinNumSpares,MaxNumSpares,NumOfWaits,EndingSpares,EndSparesUsed,
   InitNumSpares,NewSpares,NodeMinFlow,NodeMaxFlow,NodeMinCap,NodeMaxCap: ARRAY INTEGER OF LMONITORED INTEGER BY IStatObj;    
   MinNumSparesMon,MaxNumSparesMon, NumOfWaitsMon, EndingSparesMon,
   InitNumSparesMon,NodeMinFlowMon,NodeMaxFlowMon,NodeMinCapMon,
   NodeMaxCapMon,EndSparesUsedMon,NewSparesMon                          : IStatObj;  
   SysFailMon,SysRepMon,CompFailMon,CompRepairMon,CompPMMon,GreenMon,RedMon,
   YellowMon,NodeGreenMon,NodeYellowMon,NodeRedMon,NodeBlueMon,NodeBrownMon, 
   SystemCostMon,SystemLostCostMon,OpCostMon,RepCostMon,BlockSpCostMon,
   BlockESCostMon,BlockIdleCostMon,BlockHoldCostMon,BlockSBCostMon,
   BlockDoneCostMon,PoolAddCostMon,PoolESCostMon,MultiRunDataMon,AvailMon,
   MTBDEMon,MDTMon,MTBMuMon,MTBMsMon,MTBMMon,MMTsMon,MMTMon,MMTuMon,
   Term1Mon,GreenPercentMon,YellowPercentMon,RedPercentMon,GTotalMon,
   BlockDispCostMon,NodeAveFlowMon,NodeUsageMon,NodeAveCapMon,
   NodeCapabilityMon,AveNumSparesMon,UnitsProducedMon,MissionGrnMon,
   MissionYelMon,MissionRedMon,DependMon,NodeOrangeMon,MissionGrnPerMon,
   MissionYelPerMon,MissionRedPerMon,BlockPMCostMon,BlockpmHoldCostMon,
   RMon,RCondMon,RunMon,StandbyMon,RepMon,BlockRepHoldMon,BlockIdleMon,
   BlockPMMon,BlockPMHoldMon,BlockDoneMon,Term2Mon,Term3Mon,hMTBDEMon,
   hMDTMon,AoMon,DoMon,CompRMon,
   CumAoByTimeMon,CumMTBDEbyTimeMon,CumMDTbyTimeMon,CumMTBMbyTimeMon,CumMMTbyTimeMon        : RStatObj;     {cmc 3/6/07}
   CustSpareMon,PoolMon,NodeFlowMon,NodeCapMon                          : ARRAY INTEGER OF ITimedStatObj;
   EventController                                                      : RBDSimControlObj;  
   keyStream,SpareStream,EndOfRunStream,EndSimTimeStream,NodeStream,
   CostStream,AoPlotStream,ResultsStream,RamStream,relStream,kpfTimeStream  : StreamObj;  {cmc 3/6/07}
   message                                                              : TextBufferType;
   Date                                                                 : STRING;
   reliabilityFileData                                                  : ARRAY INTEGER OF INTEGER;


PROCEDURE StartEngine(IN useGraphics,sysFailFile,sysRepFile,endSimFile,paramFile,
                         AoPlotFile,capFile,resultsFile,eorFile,eventsFile,costFile,
                         availGraph                                                          : BOOLEAN; 
                      IN costPlotInterval,aoPlotInterval,simLength,startStat,TimeSlice,rMesh : REAL;
                      IN numTrials,RamInterval                                               : INTEGER);
  VAR        
     nextString,printString,header       : STRING;  
     block                               : RBDBlockObj;
     event                               : RBDEventObj;
     node                                : RBDNodeObj;
     hier                                : RBDHierObj;
     i,j,rejectedFlow, totalIntervals    : INTEGER;
     flowLabel                           : TextObj;
     button                              : PaletteButtonObj;
     item                                : MenuItemObj;
     trig                                : RapTriggerObj;
     phase                               : PhaseObj;
     link                                : LinkObj;
     clickedOn                           : ANYOBJ;
     thing                               : RBDBasicObj;
     
  BEGIN 
 
  DateTime(Date);
  IF diagnostics 
     ASK diagnosticsStream TO Open(diagFile, Append);
     nextString:="StartSimulation  " + Date;
     ASK diagnosticsStream TO WriteString(nextString);   
     ASK diagnosticsStream TO WriteLn; 
     ASK diagnosticsStream TO Close; 
  END IF; 
  GraphicsOutput:=useGraphics;
  SysFailTimeFile:=sysFailFile;
  SysRepTimeFile:=sysRepFile;
  EndSimTimeFile:=endSimFile;
  keyParamFile:=paramFile;
  AvailPlotFile:=AoPlotFile;
  CapFile:=capFile;
  ResultsFile:=resultsFile;
  EORFile:=eorFile;
  EventsFile:=eventsFile;
  CostFile:=costFile;
  AvailGraph:=availGraph;
  CostPlotInterval:=costPlotInterval;
  AoPlotInterval:=aoPlotInterval;  
  tab:=9C;{ASCII character for a tab}
  StopCriteria:=simLength; 
  StartStats:=startStat;
  NumRuns:=numTrials;  
 { IF (rMesh>0.0)      {cmc 3/6/07}
     relMesh:=rMesh;  
     kpfTimeMesh:=0.0;
  ELSIF (rMesh<0.0)
     relMesh:=0.0;  
     kpfTimeMesh:=-rMesh;
  ELSE
     relMesh:=0.0;  
     kpfTimeMesh:=0.0;
  END IF;  }
  IF (rMesh>0.0)      {cmc 3/6/07}
     relMesh:=rMesh;  
     kpfTimeMesh:=rMesh;
  ELSE
     relMesh:=0.0;  
     kpfTimeMesh:=0.0;
  END IF;  
  exploded:=FALSE;
  Aborted:=FALSE;
  SimCompleted:=FALSE;
  KeepRunStats:=TRUE;
  KeepSimStats:=TRUE;
  StatsReset:=FALSE;
  SpareMonsCreated:=FALSE;
  EndSpMonCreated:=FALSE;
  OutOfSpares:=FALSE; 
  MTBDEsusp:=FALSE;
  MTBMsusp:=FALSE;
  MTBMususp:=FALSE;
  MTBMssusp:=FALSE;
  didPM:=FALSE;         {tony 12-04: to get N/As in MTBMs only when appropriate}
  CountFailure:=TRUE;
  ZeroInitCost:=FALSE;
  jumpStop:=FALSE;
  IF password
     randNumCount:=0;
  END IF;   
  NEW(EventController);
  NEW(System);
  NEW(BlockDepGroup);
  NEW(NodeDepGroup);
  NEW(ColdChangeGroup);
  NEW(SparesTrackingGroup);
  IF activePhases>0
     NEW(BlockPhaseChangeGroup);
     NEW(BlockPhaseImproveGroup);
     NEW(BlockPhaseDegradeGroup);
     NEW(BlockPhaseStressGroup);
     NEW(NodePhaseChangeGroup);
     NEW(NodePhaseImproveGroup);
     NEW(NodePhaseDegradeGroup); 
     NEW(pmDeferGroup);
     NEW(pmSyncTrigger);
     IF weakAnalysis
       NEW(NodeFailureGroup);
     END IF;
  END IF;
  NEW(SysStream,1..11);
  FOR i:=1 TO 11
     NEW(SysStream[i]);
     ASK SysStream[i] TO SetSeed(FetchRaptorSeed(ABS(sysStreams[i])));
     IF sysStreams[i]<0
        ASK SysStream[i] TO SetAntithetic(TRUE);
     END IF;   
  END FOR;
  IF totalBlocks>0
     NEW(FailStream,1..totalBlocks);
     NEW(RepairStream,1..totalBlocks); 
     NEW(BlockTrigger,1..totalBlocks);  
  END IF;
  IF totalEvents>0
     NEW(EventFailStream,1..totalEvents);
  END IF;
  OpenOutFiles(RamInterval); 
  FOREACH block IN blockGroup
     NEW(BlockTrigger[block.seqNum]);
     ASK block TO Initialize;
     IF ((block.usesPhasing) AND (activePhases>0))
        ASK BlockPhaseChangeGroup TO Add(block);        
     END IF;  
     IF (block.sparingType = Custom)
        ASK SparesTrackingGroup TO Add(block);             
     END IF;           
     IF (block.failStream<70)
        NEW(FailStream[block.seqNum]);
        ASK FailStream[block.seqNum] TO SetSeed(FetchRaptorSeed(ABS(block.failStream))); 
        IF block.failStream<0
           ASK FailStream[block.seqNum] TO SetAntithetic(TRUE);
        END IF;           
        NEW(RepairStream[block.seqNum]);
        ASK RepairStream[block.seqNum] TO SetSeed(FetchRaptorSeed(ABS(block.repairStream))); 
        IF block.repairStream<0
           ASK RepairStream[block.seqNum] TO SetAntithetic(TRUE);
        END IF;           
     END IF;        
     ASK block TO CheckForDeps;          
  END FOREACH;
  FOREACH event IN eventGroup
     ASK event TO Initialize;
     IF ((event.usesPhasing) AND (activePhases>0))
        ASK BlockPhaseChangeGroup TO Add(event);        
     END IF;  
     IF (event.failStream<70)
        NEW(EventFailStream[event.seqNum]);
        ASK EventFailStream[event.seqNum] TO SetSeed(FetchRaptorSeed(ABS(event.failStream))); 
        IF event.failStream<0
           ASK EventFailStream[event.seqNum] TO SetAntithetic(TRUE);
        END IF;           
     END IF;        
  END FOREACH;
  FOREACH node IN nodeGroup
     IF node.Id=startId
        startTag:=node.seqNum;
     END IF;   
     ASK node TO Initialize();          
     IF ((node.usesPhasing) AND (activePhases>0))
        ASK NodePhaseChangeGroup TO Add(node);
     END IF;
     IF node.coldStandby
        ASK node TO IdAndSortColds;          {csb speed change   removed line }
     END IF;
     IF capacityAnalysis AND (node.CapConnectTo<>NILARRAY)
        ASK node TO SortCapacityArrays;
     END IF;
     ASK node TO CheckForDeps;          
  END FOREACH;
  NEW(nodeHasDepsGroup);
  FOREACH node IN nodeGroup
     IF node.hasDepObjects
        ASK nodeHasDepsGroup TO Add(node);
     END IF;
  END FOREACH;
  NEW(blockHasDepsGroup);
  FOREACH block IN blockGroup
     IF block.hasDepObjects
        ASK blockHasDepsGroup TO Add(block);
     END IF;
  END FOREACH;
  NEW(eventHasDepsGroup);
  FOREACH event IN eventGroup
     IF event.hasDepObjects
        ASK eventHasDepsGroup TO Add(event);
     END IF;
  END FOREACH;
  IF totalBlocks>0
     NEW(SparesUsed,1..totalBlocks);
     NEW(EndSparesUsed,1..totalBlocks);     
  END IF;   
  IF ((SparesTrackingGroup.numberIn>0) OR (totalPools>0))
     NEW(InitNumSpares,1..(totalBlocks+totalPools));
     NEW(AveNumSpares,1..(totalBlocks+totalPools));
     NEW(MinNumSpares,1..(totalBlocks+totalPools));  
     NEW(MaxNumSpares,1..(totalBlocks+totalPools));
     NEW(NumOfWaits,1..(totalBlocks+totalPools));
     NEW(EndingSpares,1..(totalBlocks+totalPools));     
     NEW(NewSpares,1..(totalBlocks+totalPools)); 
     NEW(currNumWaits,1..(totalBlocks+totalPools));     
     NEW(currNumArrivals,1..(totalBlocks+totalPools));
     NEW(currInitNumber,1..(totalBlocks+totalPools));
  END IF;  
  IF (SparesTrackingGroup.numberIn>0)
     NEW(CustomSpares,1..totalBlocks);
     NEW(SparesAvailable,1..totalBlocks); 
     NEW(CustSpareMon,1..totalBlocks);
     FOREACH block IN SparesTrackingGroup 
        j:=block.seqNum;
        NEW(CustomSpares[j]); 
        CustSpareMon[j]:=GETMONITOR(SparesAvailable[j],ITimedStatObj);
        ASK CustSpareMon[j] TO Reset;
     END FOREACH;  
  END IF;
  IF (totalPools>0)
     NEW(PooledSpares,1..totalPools); 
     NEW(PoolResAvailable,1..totalPools);
     NEW(PoolMon,1..totalPools);
     FOR i:= 1 TO totalPools
        NEW(PooledSpares[i]); 
        PoolMon[i]:=GETMONITOR(PoolResAvailable[i],ITimedStatObj);
        ASK PoolMon[i] TO Reset;
     END FOR;
  END IF;
  IF weakAnalysis
     NEW(NodeGreenTime,1..totalNodes);
     NEW(NodeYellowTime,1..totalNodes);
     NEW(NodeRedTime,1..totalNodes);
     NEW(NodeBlueTime,1..totalNodes);
     NEW(NodeBrownTime,1..totalNodes);
     NEW(NodeOrangeTime,1..totalNodes);
     NEW(NodeEndGreen,1..totalNodes);
     NEW(NodeEndYellow,1..totalNodes);
     NEW(NodeEndRed,1..totalNodes);
     NEW(NodeEndBlue,1..totalNodes);
     NEW(NodeEndBrown,1..totalNodes);
     NEW(NodeEndOrange,1..totalNodes);
     NEW(NodeEndAo,1..totalNodes);
     NEW(NodeEndDo,1..totalNodes);
     NEW(NodeEndR,1..totalNodes);
     IF totalBlocks>0
        NEW(BlockRunTime,1..totalBlocks);
        NEW(BlockStandbyTime,1..totalBlocks);
        NEW(BlockRepairTime,1..totalBlocks);
        NEW(BlockRepHoldTime,1..totalBlocks);
        NEW(BlockIdleTime,1..totalBlocks);
        NEW(BlockPMTime,1..totalBlocks);
        NEW(BlockPMHoldTime,1..totalBlocks);
        NEW(BlockDoneTime,1..totalBlocks);    
        NEW(BlockEndRun,1..totalBlocks);
        NEW(BlockEndStandby,1..totalBlocks);
        NEW(BlockEndRep,1..totalBlocks);
        NEW(BlockEndRepHold,1..totalBlocks);
        NEW(BlockEndIdle,1..totalBlocks);
        NEW(BlockEndPM,1..totalBlocks);
        NEW(BlockEndPMHold,1..totalBlocks);
        NEW(BlockEndDone,1..totalBlocks); 
        NEW(BlockEndAo,1..totalBlocks);
        NEW(BlockEndDo,1..totalBlocks);
        NEW(BlockEndR,1..totalBlocks);
        NEW(blockMissionBool,1..totalBlocks);
        NEW(blockMCompleted,1..totalBlocks);
     END IF;
     NEW(nodeMissionBool,1..totalNodes);
     NEW(nodeMCompleted,1..totalNodes);
     IF totalEvents>0
        NEW(EventRunTime,1..totalEvents);
        NEW(EventStandbyTime,1..totalEvents);
        NEW(EventRepairTime,1..totalEvents);
        NEW(EventEndRun,1..totalEvents);
        NEW(EventEndStandby,1..totalEvents);
        NEW(EventEndRep,1..totalEvents);
        NEW(EventEndAo,1..totalEvents);
        NEW(EventEndDo,1..totalEvents);
        NEW(EventEndR,1..totalEvents);
        NEW(eventMissionBool,1..totalEvents);
        NEW(eventMCompleted,1..totalEvents);
     END IF;
     IF totalHiers>0
        NEW(hMTBDE,1..totalHiers);
        NEW(hMDT,1..totalHiers);
        NEW(hierDE,1..totalHiers);
     END IF;
  END IF;
  IF costAnalysis
     IF totalBlocks>0
        NEW(BlockRepCost,1..totalBlocks);
        NEW(BlockOpCost,1..totalBlocks);  
        NEW(BlockSpCost,1..totalBlocks);
        NEW(BlockESCost,1..totalBlocks);
        NEW(BlockIdleCost,1..totalBlocks);
        NEW(BlockHoldCost,1..totalBlocks);
        NEW(BlockPMCost,1..totalBlocks);
        NEW(BlockpmHoldCost,1..totalBlocks);
        NEW(BlockSBCost,1..totalBlocks);
        NEW(BlockDoneCost,1..totalBlocks);
        NEW(BlockDispCost,1..totalBlocks);
        NEW(BlockEndOpCost,1..totalBlocks);
        NEW(BlockEndRepCost,1..totalBlocks);
        NEW(BlockEndSpCost,1..totalBlocks);
        NEW(BlockEndIdleCost,1..totalBlocks);
        NEW(BlockEndHoldCost,1..totalBlocks);
        NEW(BlockEndPMCost,1..totalBlocks);
        NEW(BlockEndpmHoldCost,1..totalBlocks);
        NEW(BlockEndSBCost,1..totalBlocks);
        NEW(BlockEndESCost,1..totalBlocks);
        NEW(BlockEndDoneCost,1..totalBlocks);     
        NEW(BlockEndDispCost,1..totalBlocks); 
     END IF;   
     IF totalEvents>0
        NEW(EventOpCost,1..totalEvents);
        NEW(EventRepCost,1..totalEvents);
        NEW(EventEndOpCost,1..totalEvents);
        NEW(EventEndRepCost,1..totalEvents);
     END IF;   
     SystemCostMon:=GETMONITOR(SystemEndRedCost,RStatObj); 
     ASK SystemCostMon TO Reset;
     SystemLostCostMon:=GETMONITOR(SystemEndLostCost,RStatObj);
     ASK SystemLostCostMon TO Reset;
  END IF;
  IF capacityAnalysis
     NEW(NodeAveFlow,1..totalNodes);
     NEW(NodeMinFlow,1..totalNodes);
     NEW(NodeMaxFlow,1..totalNodes);
     NEW(NodeUsage,1..totalNodes);
     NEW(NodeAveCap,1..totalNodes);
     NEW(NodeMinCap,1..totalNodes);
     NEW(NodeMaxCap,1..totalNodes);
     NEW(NodeCapability,1..totalNodes);
     FlowingAtMaxCap:=FALSE;
     DetermineMaxCapacity(i);
     IF flowGenerated=i
        FlowingAtMaxCap:=TRUE;
     END IF;
  END IF;
  IF ((totalPools>0) AND costAnalysis)
     NEW(PoolAddCost,1..totalPools);
     NEW(PoolESCost,1..totalPools);
     NEW(PoolEndAddCost,1..totalPools);
     NEW(PoolEndESCost,1..totalPools);
  END IF;
  SysFailMon:=GETMONITOR(SysTimeToFail,RStatObj); 
  ASK SysFailMon TO Reset;
  SysRepMon:=GETMONITOR(SysTimeToRepair,RStatObj); 
  ASK SysRepMon TO Reset;
  CompFailMon:=GETMONITOR(CompTimeToFail,RStatObj);
  ASK CompFailMon TO Reset;
  CompRepairMon:=GETMONITOR(CompTimeToRepair,RStatObj);
  ASK CompRepairMon TO Reset;
  CompPMMon:=GETMONITOR(CompTimeToDoPM,RStatObj);
  ASK CompPMMon TO Reset;
  GreenMon:=GETMONITOR(GreenTime,RStatObj);
  ASK GreenMon TO Reset;
  YellowMon:=GETMONITOR(YellowTime,RStatObj);
  ASK YellowMon TO Reset;
  RedMon:=GETMONITOR(RedTime,RStatObj);
  ASK RedMon TO Reset;
  MissionGrnMon:= GETMONITOR(MissionGrnTime,RStatObj);
  ASK MissionGrnMon TO Reset;
  MissionYelMon:= GETMONITOR(MissionYelTime,RStatObj);
  ASK MissionYelMon TO Reset;
  MissionRedMon:= GETMONITOR(MissionRedTime,RStatObj);
  ASK MissionRedMon TO Reset;
  AvailMon:=GETMONITOR(Availability,RStatObj);
  ASK AvailMon TO Reset;
  DependMon:=GETMONITOR(Dependability,RStatObj);
  ASK DependMon TO Reset;
  GTotalMon:=GETMONITOR(GTotal,RStatObj);
  ASK GTotalMon TO Reset;
  MTBDEMon:=GETMONITOR(MTBDE,RStatObj);
  ASK MTBDEMon TO Reset;
  MDTMon:=GETMONITOR(MDT,RStatObj);
  ASK MDTMon TO Reset;
  MTBMuMon:=GETMONITOR(MTBMu,RStatObj);
  ASK MTBMuMon TO Reset;
  MTBMsMon:=GETMONITOR(MTBMs,RStatObj);
  ASK MTBMsMon TO Reset;
  MTBMMon:=GETMONITOR(MTBM,RStatObj);
  ASK MTBMMon TO Reset;
  MMTsMon:=GETMONITOR(MMTs,RStatObj); 
  ASK MMTsMon TO Reset;
  MMTMon:=GETMONITOR(MMT,RStatObj); 
  ASK MMTMon TO Reset;
  MMTuMon:=GETMONITOR(MMTu,RStatObj); 
  ASK MMTuMon TO Reset;
  Term1Mon:=GETMONITOR(Term1Condition,RStatObj);
  ASK Term1Mon TO Reset;
  Term2Mon:=GETMONITOR(Term2Cond,RStatObj);
  ASK Term2Mon TO Reset;
  Term3Mon:=GETMONITOR(Term3Cond,RStatObj);
  ASK Term3Mon TO Reset;
  GreenPercentMon:=GETMONITOR(GreenPercent,RStatObj);
  ASK GreenPercentMon TO Reset;
  YellowPercentMon:=GETMONITOR(YellowPercent,RStatObj);
  ASK YellowPercentMon TO Reset;
  RedPercentMon:=GETMONITOR(RedPercent,RStatObj);  
  ASK RedPercentMon TO Reset;
  MissionGrnPerMon:=GETMONITOR(MissionGrnPer,RStatObj);
  ASK MissionGrnPerMon TO Reset;
  MissionYelPerMon:=GETMONITOR(MissionYelPer,RStatObj);
  ASK MissionYelPerMon TO Reset;
  MissionRedPerMon:=GETMONITOR(MissionRedPer,RStatObj); 
  ASK MissionRedPerMon TO Reset;
  RMon:=GETMONITOR(Reliability,RStatObj);
  ASK RMon TO Reset;
  RCondMon:=GETMONITOR(RCond,RStatObj);
  ASK RCondMon TO Reset;
  header := nameOfFile;
  Timescale:=TimeSlice; 
  IF EventsFile    
     ASK EventStream TO WriteString("Event_Log "+tab+"for "+tab+header+tab+" "+tab+Date);  
     ASK EventStream TO WriteLn;
     ASK EventStream TO WriteLn;
  END IF; 
  IF EORFile    
     ASK EndOfRunStream TO WriteString("End_Of_Run Report for "+header+"  "+Date);
     ASK EndOfRunStream TO WriteLn; 
     ASK EndOfRunStream TO WriteLn;
  END IF; 
  IF CapFile    
     ASK CapacityStream TO WriteString("SimulationTime        Flow       Capacity");
     ASK CapacityStream TO WriteLn; 
  END IF; 
  IF CostFile    
     ASK CostStream TO WriteString("Interim Cost Report for "+header+"  "+Date);
     ASK CostStream TO WriteLn;
     ASK CostStream TO WriteLn;
  END IF; 
  IF keyParamFile    
     ASK keyStream TO WriteString("Trial     Ao           Do           R            RCond"+               
       "        MTBDE                MDT                MTBM                MMT");
     ASK keyStream TO WriteLn;
  END IF; 
  IF AvailPlotFile    
     ASK AoPlotStream TO WriteString
     ("SimulationTime        Ao        Green     Yellow    Red       Interval");
     ASK AoPlotStream TO WriteLn;
  END IF; 
  IF AvailGraph
     totalIntervals:=TRUNC(simLength/AoPlotInterval); 
     NEW(MultiRunData, 1..totalIntervals);
  END IF;
  IF ResultsFile
        ASK ResultsStream TO WriteString("Final_Results_Report for "+header+"  "+Date);
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;  
  END IF;
  IF (RamInterval>0)
     ASK RamStream TO WriteString("Trials    Ao           Ao_StDev    Ao_SEM      Do           Do_StDev    Do_SEM      " + 
              "R            R_StDev     R_SEM       R_SEP       RCond        RCond_StDev RCond_SEM           " +
              "MTBDE               MTBDE_StDev          MTBDE_SEM           MDT                 MDT_StDev            MDT_SEM     MDT_Updates        "+
              "MTBM                MTBM_StDev           MTBM_SEM            MMT                 MMT_StDev            MMT_SEMS    MMT_Updates");
     ASK RamStream TO WriteLn;
  END IF;
  IF (relMesh>0.0)
     NEW(reliabilityFileData,0..(TRUNC(simLength/relMesh)));
  END IF;
  
  IF (kpfTimeMesh>0.0)                                   {cmc 3/6/07}
       NEW(CumAoByTime,1..(TRUNC(simLength/kpfTimeMesh)));
       NEW(CumMTBDEbyTime,1..(TRUNC(simLength/kpfTimeMesh)));
       NEW(CumMDTbyTime,1..(TRUNC(simLength/kpfTimeMesh)));  
       NEW(CumMTBMbyTime,1..(TRUNC(simLength/kpfTimeMesh)));
       NEW(CumMMTbyTime,1..(TRUNC(simLength/kpfTimeMesh)));     
  END IF;
  IF ((DomainTree) OR (CoreFile))
     CreateDomainTree;
  END IF;   
  runNumber:=1;
  IF EventsFile 
     ASK EventStream TO WriteLn; 
     WriteEvents(0.000000,"System","Begin_Run_"+INTTOSTR(runNumber),"--","--");
  END IF;  
  ASK EventController TO SetTieBreaking(FALSE);        {cmc}
  TieBreaking:=TRUE;
  PoolCostMonsSet:=FALSE;
  BlockCostMonsSet:=FALSE;
  capacityMonsSet:=FALSE;
  weakAnalysisMonsSet:=FALSE;  
  IF NodeArray<>NILARRAY
     DISPOSE(NodeArray);
  END IF;
  IF BlockArray<>NILARRAY
     DISPOSE(BlockArray);
  END IF;
  IF FinalArray<>NILARRAY
     DISPOSE(FinalArray);
  END IF; 
  IF HierArray<>NILARRAY
     DISPOSE(HierArray);
  END IF;   
  IF SparesArray<>NILARRAY
      DISPOSE(SparesArray);
  END IF;
  IF CapacityArray<>NILARRAY
     DISPOSE(CapacityArray);
  END IF;
  IF BlockCostArray<>NILARRAY
     DISPOSE(BlockCostArray);
  END IF;
  IF EventArray<>NILARRAY
     DISPOSE(EventArray);
  END IF;
  WHILE runNumber <=NumRuns 
     IF GraphicsOutput AND faceBoxOpen
        ASK window TO DisplayFace(1); 
     END IF;
     IF statusBarOn
        ASK window TO ShowStatus(1,"Run " + INTTOSTR(runNumber) + " of " + INTTOSTR(NumRuns));
        IF termType=1
           ASK window TO ShowStatus(2,"0% complete"); 
        ELSIF termType=2
           ASK window TO ShowStatus(2,"0 system failure(s)");
        ELSE   {termType=3}
           ASK window TO ShowStatus(2,"0 cycles completed");
        END IF; 
     END IF;
     StatsStarted:=FALSE;
     PhaseChangeInProgress:=FALSE;
     ResetSimTime(0.0);  
     termCounter:=0;
     cyclesCount:=0.0;
     LastColorChange:=0.0;
     LastMissionChange:=0.0;
     LastUpDownChange:=0.0;
     TimeOfCompFailure:=0.0; 
     StatsStartTime:=0.0;
     autoStopped:=FALSE;
     EndSimCalled:=FALSE;
     ASK System TO Initialize; 
     NumCurrentlyFailed:=0; 
     ASK EventController TO SetTimeAdvance(TRUE);  
     phaseNumber:=1;
     IF ((statusBarOn) AND (activePhases>0))
        phase:=phaseObjArray[1];
        ASK window TO ShowStatus(3,phase.phaseName);
        IF phase.mission
           ASK window TO ShowStatus(4,"MISSION"); 
           IF EventsFile
              WriteEvents(SimTime,"System","State_change","Mission","Began_a_mission");
           END IF;   
        ELSE
           ASK window TO ShowStatus(4,"Nonmission"); 
        END IF;           
     END IF;
     FOREACH link IN linkGroup
        ASK link TO SetToInitValues;
     END FOREACH;
     SystemStateChange:=FALSE;
     IF capacityAnalysis
        NEW(NodeFlow,1..totalNodes);
        NEW(NodeCapacity,1..totalNodes);
        NEW(NodeFlowMon,1..totalNodes);
        NEW(NodeCapMon,1..totalNodes);
        lastFlow:=-1;       {cmc capFix}
     END IF;
     FOREACH node IN nodeGroup 
        ASK node TO SetToInitValues();
        IF node.coldStandby                    {csb speed change}
           ASK ColdChangeGroup TO Add(node);
           IF (NOT(node.priorityReturn) AND (runNumber > 1)) 
              ASK node TO IdAndSortColds;   
           END IF;
        END IF;
        IF weakAnalysis
           nodeMissionBool[node.seqNum]:=TRUE;
           nodeMCompleted[node.seqNum]:=0;
        END IF;  
        IF capacityAnalysis
           NodeFlowMon[node.seqNum]:=GETMONITOR(NodeFlow[node.seqNum],ITimedStatObj);
           ASK NodeFlowMon[node.seqNum] TO Reset;
           NodeCapMon[node.seqNum]:=GETMONITOR(NodeCapacity[node.seqNum],ITimedStatObj);        
           ASK NodeCapMon[node.seqNum] TO Reset;
        END IF;
     END FOREACH;
     ASK System TO SetToInitValues();
     runCompleted:=FALSE;
     FOREACH hier IN hierGroup
        ASK hier TO SetToInitValues;
     END FOREACH;
     FOREACH block IN blockGroup 
        IF weakAnalysis
           blockMissionBool[block.seqNum]:=TRUE;
           blockMCompleted[block.seqNum]:=0;
        END IF; 
        ASK block TO GetStartCond;
        ASK block TO SetToInitValues();  
        IF (SparesTrackingGroup.numberIn>0)   
           SparesAvailable[block.seqNum]:=block.initStock;   
        END IF;
        IF (SparesTrackingGroup.Includes(block)) 
           ASK CustomSpares[block.seqNum] TO Create(block.initStock);  
           ASK CustomSpares[block.seqNum] TO SetPendStats(TRUE); 
           SparesAvailable[block.seqNum]:=CustomSpares[block.seqNum].Resources;
           ASK CustSpareMon[block.seqNum] TO Reset;
           IF (block.routineSpareOrdering)
              TELL block TO GenerateSpares(); 
           END IF;
           currNumWaits[block.seqNum]:=0;
           currNumArrivals[block.seqNum]:=0;
           currInitNumber[block.seqNum]:=CustomSpares[block.seqNum].Resources;
        END IF;
        SparesUsed[block.seqNum]:=0;
     END FOREACH;
     FOREACH event IN eventGroup
        IF weakAnalysis
           eventMissionBool[event.seqNum]:=TRUE;
           eventMCompleted[event.seqNum]:=0;
        END IF;   
        ASK event TO SetToInitValues();  
     END FOREACH;
     IF (totalPools>0)        
        FOR i:= 1 TO totalPools 
           ASK PooledSpares[i] TO Create(PoolArray[i].initialSpares);   
           ASK PooledSpares[i] TO SetPendStats(TRUE);  
           PoolResAvailable[i]:=PooledSpares[i].Resources;
           ASK PoolMon[i] TO Reset;
           IF (PoolArray[i].routineSpareOrdering)
              TELL PooledSpares[i] TO GenerateSpares(i);
           END IF;
           ASK PooledSpares[i] TO Initialize;
           IF costAnalysis
              PoolAddCost[i]:=0.0;
              PoolESCost[i]:=0.0;
           END IF;
           currNumWaits[i+totalBlocks]:=0;
           currNumArrivals[i+totalBlocks]:=0;
           currInitNumber[i+totalBlocks]:=PooledSpares[i].Resources;
        END FOR;
     END IF;
     IF AvailPlotFile OR AvailGraph
        IF AvailGraph
           ASK AoGraph TO ClearDataSet(1);
           ASK AoGraph TO ClearDataSet(2);
           ASK AoGraph TO ClearDataSet(3);
           ASK IntAoGraph TO ClearDataSet(1);
           ASK MultiRunGraph TO ClearDataSet(1);
           ASK MultiRunGraph TO ClearDataSet(2);
           ASK MultiRunGraph TO ClearDataSet(3);   
           ASK AoGraph TO Plot(1, 0.0, 1.0);
           ASK AoGraph TO Plot(2, 0.0, 1.0);               
           ASK AoGraph TO Plot(3, 0.0, 1.0);    
        END IF;
        TELL System TO GenerateAoPlotData();
     END IF;
     IF (kpfTimeMesh>0.0)                                   {cmc 3/6/07}
         TELL System TO UpdateCumulativeValues(kpfTimeMesh);
     END IF;
          
     IF termType=1 
        IF statusBarOn
           TELL System TO UpdateSimStatus(StopCriteria); 
        END IF;
        TELL System TO CallEndSimulation(FALSE,TRUE) IN StopCriteria; 
        IF startStat>0.0000001
           TELL System TO ResetAllStatistics IN startStat;
        ELSE
           StatsStarted:=TRUE;
        END IF;
     ELSE    {termType=2 or 3}
        IF TRUNC(startStat)=0;
           StatsStarted:=TRUE;
        END IF;
     END IF;
     IF costAnalysis AND CostFile
        TELL System TO CalculateInterimCost;
     END IF;     
     ASK window TO SetSysCursor(NormalCursor);       
     IF activePhases>0
        TELL System TO ControlPhaseChanges(); 
     END IF;    
     simInProgress:=TRUE;  {this line needs to be before dep. backflow}
     FOREACH node IN nodeGroup 
        IF node.typeNode=5
           IF ((node.usesPhasing) AND (activePhases>0))
              IF node.phase[1]=-1
                 ASK node TO ChangeNodeState(node.Status,Cut);
              ELSIF node.phase[1]=0
                 ASK node TO ChangeNodeState(node.Status,Linked);
              END IF;
           END IF;
        ELSE
           IF node.GoodPathsRequired>node.EconnectIntoNum
              ASK node TO ReconfigureNode(0,0,0,0,0,0);
           END IF;
        END IF;
       IF node.Status=Down
           ASK node TO BackFlowDependencies(Down);
        END IF;      
     END FOREACH;
     FOREACH block IN blockGroup 
        {IF ((block.opStatus=RepHold) OR (block.opStatus=Repairing) OR (block.opStatus=PM) 
            OR (block.opStatus=PMhold) OR (block.opStatus=Done) OR (block.activeStatus=Cut))
                TELL block TO InitTimeZeroDeps IN 0.0;            {cmc 12/13/06}
        END IF;   }
        IF block.startCond="Running"
           ASK block TO TurnOffStartCond;
           TELL block TO Run IN 0.0; 
        ELSE
           TELL block TO Repair IN 0.0;
        END IF; 
        IF ((block.usesPM) AND (block.pmTriggered))
           TELL block TO ControlPMtriggers IN 0.0;
        END IF;
     END FOREACH;
     FOREACH block IN blockGroup 
        IF ((block.opStatus=RepHold) OR (block.opStatus=Repairing) OR (block.opStatus=PM)     {cmc 12/13/06}
            OR (block.opStatus=PMhold) OR (block.opStatus=Done) OR (block.activeStatus=Cut))
                TELL block TO InitTimeZeroDeps IN 0.0;
        END IF;
     END FOREACH;
     FOREACH event IN eventGroup                                     {cmc 12/13/06}
        IF ((event.opStatus=Failure) OR (event.activeStatus=Cut))
           TELL event TO InitTimeZeroDeps IN 0.0;
        END IF;
     END FOREACH;    
     FOREACH node IN nodeGroup                                {cmc 12/13/06}
        IF node.Status=Down
           TELL node TO InitTimeZeroDeps IN 0.0;
        END IF;
     END FOREACH;   
          
     
     CheckColds;
{*****}
     FOREACH trig IN triggerGroup
        TELL trig TO PullRapTrigger IN 0.0; 
     END FOREACH;
     IF inJumpMode
        jumpStop:=TRUE;
        {REPEAT 
           jumpStopped:=TRUE;
           FillStatusBar;
           goToNext := FALSE;
           clickedOn := ASK window TO AcceptInput();
        UNTIL goToNext; }
     END IF;
     StartSimulation;
{*************************************************************************************************************************}
     simInProgress:=FALSE;
     runCompleted:=TRUE;
     earlyAbort:=FALSE;
     IF Aborted
        IF (runNumber>1)
            NumRunsCompleted:=runNumber-1; 
            KeepRunStats:=FALSE; 
        ELSE
           NumRunsCompleted:=1;
           earlyAbort:=TRUE;
           IF KeepSimStats
              KeepRunStats:=TRUE;
           ELSE
              KeepRunStats:=FALSE;
           END IF;
        END IF;
     ELSE
        NumRunsCompleted:=runNumber; 
        IF (SimTime=0.0)
           AnalyzeSystem;           
           IF (inStepMode)
              FillStatusBar;
              REPEAT
                 goToNext := FALSE;
                 button:=ASK simToolBar TO AcceptInput();
              UNTIL goToNext;
           END IF;
        END IF;
     END IF;                              
     ASK System TO ChangeStatus(DDone, NonMission);  
     IF EventsFile
        IF Aborted  
           WriteEvents(SimTime,"System","Simulation_Aborted","--","--");
        ELSE       
           WriteEvents(SimTime,"System","Simulation_Terminated","--","--");
        END IF; 
     END IF;
     IF KeepRunStats
        MTBDEupdated:=TRUE;
        MDTupdated:=TRUE;
        MTBMupdated:=TRUE;
        MTBMuupdated:=TRUE;
        MTBMsupdated:=TRUE;   {used for End of Run report}
        MMTsupdated:=TRUE;
        MMTupdated:=TRUE;
        MMTuupdated:=TRUE;
        {IF  (RedMon.Count>0) }
        IF ((GreenMon.Sum+YellowMon.Sum+RedMon.Sum) >0.)
           Availability:=(GreenMon.Sum+YellowMon.Sum)/(GreenMon.Sum+YellowMon.Sum+RedMon.Sum);  
        ELSE
           Availability:=1.0;
        END IF;
        IF ((MissionGrnMon.Sum+MissionYelMon.Sum+MissionRedMon.Sum) >0.)
           Dependability:=(MissionGrnMon.Sum+MissionYelMon.Sum)/(MissionGrnMon.Sum+MissionYelMon.Sum+MissionRedMon.Sum);  
        END IF;
        IF missionsAttempted>0
           Reliability:=FLOAT(missionsCompleted)/FLOAT(missionsAttempted);
        END IF;
        IF missionsBegan>0
           RCond:=FLOAT(missionsCompleted)/FLOAT(missionsBegan);
        END IF;   
        IF ((SysFailMon.Count>0) AND (SimTime>0.0)) 
           MTBDE:=(GreenMon.Sum+YellowMon.Sum)/FLOAT(SysFailMon.Count);  
        ELSE 
           IF (GreenMon.Sum+YellowMon.Sum)>0.0
              MTBDE:=(GreenMon.Sum+YellowMon.Sum); 
              MTBDEsusp:=TRUE;
           ELSE
              MTBDEupdated:=FALSE;
           END IF;
        END IF;        
        IF autoStopped   {could not reach required number of failures}
           IF ((RedMon.Count>1) AND (SimTime>0.0))  
              MDT:=RedMon.Mean * FLOAT(RedMon.Count)/(FLOAT(RedMon.Count)-1.0); 
           ELSE
              MDTupdated:=FALSE;        
           END IF;        
        ELSE
           IF ((RedMon.Count>0) AND (SimTime>0.0))   
              MDT:=RedMon.Mean; 
           ELSE
              MDTupdated:=FALSE;        
           END IF;
        END IF;
        IF (GreenMon.Sum+YellowMon.Sum > 0.0)
           IF ((CompFailMon.Count>0) AND (CompPMMon.Count>0))
              MTBMu:=(GreenMon.Sum+YellowMon.Sum)/FLOAT(CompFailMon.Count);
              MTBM :=(GreenMon.Sum+YellowMon.Sum)/(FLOAT(CompFailMon.Count)+FLOAT(CompPMMon.Count));
              MTBMs:=(GreenMon.Sum+YellowMon.Sum)/FLOAT(CompPMMon.Count);
              didPM:=TRUE;         {Beta118}
           ELSIF (CompPMMon.Count>0)
              MTBM:= (GreenMon.Sum+YellowMon.Sum)/(FLOAT(CompPMMon.Count));
              MTBMs:=(GreenMon.Sum+YellowMon.Sum)/FLOAT(CompPMMon.Count);
              MTBMu :=(GreenMon.Sum+YellowMon.Sum); 
              MTBMususp:=TRUE;
              didPM:=TRUE;         {Beta118}
           ELSIF (CompFailMon.Count>0)
              MTBMu:= (GreenMon.Sum+YellowMon.Sum)/(FLOAT(CompFailMon.Count));
              MTBM :=(GreenMon.Sum+YellowMon.Sum)/FLOAT(CompFailMon.Count); 
              MTBMs :=(GreenMon.Sum+YellowMon.Sum); 
              MTBMsupdated:=FALSE;
              MTBMssusp:=TRUE;
           ELSE 
              MTBM :=(GreenMon.Sum+YellowMon.Sum); 
              MTBMu:=(GreenMon.Sum+YellowMon.Sum); 
              MTBMsusp:=TRUE;
              MTBMususp:=TRUE;
              MTBMs :=(GreenMon.Sum+YellowMon.Sum);    
              MTBMsupdated:=FALSE;
              MTBMssusp:=TRUE;
           END IF;
        ELSE
           MTBMupdated:=FALSE;
           MTBMuupdated:=FALSE;
           MTBMsupdated:=FALSE;
        END IF;     
 
        IF autoStopped 
           IF ((CompRepairMon.Count>1) AND (CompPMMon.Count>1))
              MMTs:=CompPMMon.Mean * FLOAT(CompPMMon.Count)/(FLOAT(CompPMMon.Count)-1.0); 
              MMTu:=CompRepairMon.Mean * FLOAT(CompRepairMon.Count)/(FLOAT(CompRepairMon.Count)-1.0); 
              MMT:= (  MMTu* FLOAT(CompRepairMon.Count-1) + MMTs* FLOAT(CompPMMon.Count-1)  )/
                    FLOAT( (CompRepairMon.Count-1)+(CompPMMon.Count-1) );
           ELSIF (CompRepairMon.Count>1)
              MMTu:=CompRepairMon.Mean * FLOAT(CompRepairMon.Count)/(FLOAT(CompRepairMon.Count)-1.0); 
              MMT:= MMTu;  
              MMTsupdated:=FALSE;
           ELSIF (CompPMMon.Count>1)   
              MMTs:=CompPMMon.Mean * FLOAT(CompPMMon.Count)/(FLOAT(CompPMMon.Count)-1.0); 
              MMT:= MMTs;
              MMTuupdated:=FALSE;
           ELSE
              MMTupdated:=FALSE;        
              MMTsupdated:=FALSE;        
              MMTuupdated:=FALSE;        
           END IF;
        ELSE
           IF ((CompRepairMon.Count>0) AND (CompPMMon.Count>0))
              MMTs:=CompPMMon.Mean;
              MMTu:=CompRepairMon.Mean; 
              MMT:= (MMTu*FLOAT(CompRepairMon.Count)+MMTs*FLOAT(CompPMMon.Count))/
                     FLOAT(CompRepairMon.Count+CompPMMon.Count);
           ELSIF (CompRepairMon.Count>0)
              MMTu:=CompRepairMon.Mean; 
              MMT:= CompRepairMon.Mean;
              MMTsupdated:=FALSE;
           ELSIF (CompPMMon.Count>0)   
              MMTs:=CompPMMon.Mean; 
              MMT:= CompPMMon.Mean;
              MMTuupdated:=FALSE;
           ELSE
              MMTupdated:=FALSE;        
              MMTsupdated:=FALSE;        
              MMTuupdated:=FALSE;        
           END IF;
        END IF;
        TotalTime:=GreenMon.Sum+YellowMon.Sum+RedMon.Sum; 
        MissionTotalTime:=MissionGrnMon.Sum+MissionYelMon.Sum+MissionRedMon.Sum;
        IF (TotalTime>0.0)
           GreenPercent:=100.*GreenMon.Sum/TotalTime;
           YellowPercent:=100.*YellowMon.Sum/TotalTime;
           RedPercent:=100.*RedMon.Sum/TotalTime;
           IF (MissionTotalTime>0.0)
              MissionGrnPer:=100.*MissionGrnMon.Sum/MissionTotalTime;
              MissionYelPer:=100.*MissionYelMon.Sum/MissionTotalTime;
              MissionRedPer:=100.*MissionRedMon.Sum/MissionTotalTime;
           END IF;
           IF AvailGraph
              IF currentGraph=1
                 ASK AoGraph TO SetCoordinate(1, SimTime, 1.0);
                 ASK AoGraph TO SetCoordinate(2, SimTime, Availability);
                 ASK AoGraph TO SetCoordinate(3, SimTime, GreenPercent/100.);
                 ASK AoGraph TO Draw();
              END IF;   
           END IF;
        END IF;
        Term1Condition:=FLOAT(SysFailMon.Count); 
        Term2Cond:=SimTime-StatsStartTime; 
        Term3Cond:=cyclesCount;   
        IF (StatsStarted AND AvailPlotFile) 
           nextString:=SUBSTR(1,20,REALTOSTR(SimTime)+"                    ")+"  ";
           ASK AoPlotStream TO WriteString(nextString);
{           ASK AoPlotStream TO WriteReal(SimTime,20,6);
           ASK AoPlotStream TO WriteString("  ");} 
           ASK AoPlotStream TO WriteReal(Availability,8,6); 
           ASK AoPlotStream TO WriteString("  "); 
           ASK AoPlotStream TO WriteReal(GreenPercent/100.0,8,6); 
           ASK AoPlotStream TO WriteString("  "); 
           ASK AoPlotStream TO WriteReal(YellowPercent/100.0,8,6); 
           ASK AoPlotStream TO WriteString("  "); 
           ASK AoPlotStream TO WriteReal(RedPercent/100.0,8,6); 
           ASK AoPlotStream TO WriteString("  "); 
           ASK AoPlotStream TO WriteLn;
        END IF;
        DoEndOfRunUpdate();
        IF (RamInterval <> 0)
           IF ((runNumber MOD RamInterval) = 0)
              CalculateInterimRams;
           END IF;   
        END IF;
     ELSE
        printString:="Statistics from this run were not included in the Final Results";
        IF EventsFile 
           ASK EventStream TO WriteLn;  
           ASK EventStream TO WriteString(printString); 
           ASK EventStream TO WriteLn; 
        END IF;    {EventsFile}        
        IF CapFile 
           ASK CapacityStream TO WriteLn;  
           ASK CapacityStream TO WriteString(printString); 
           ASK CapacityStream TO WriteLn; 
        END IF;  
        IF AvailPlotFile 
           ASK AoPlotStream TO WriteLn;  
           ASK AoPlotStream TO WriteString(printString); 
           ASK AoPlotStream TO WriteLn; 
        END IF; 
        IF CostFile
           ASK CostStream TO WriteLn;  
           ASK CostStream TO WriteString(printString); 
           ASK CostStream TO WriteLn;         
        END IF;  
     END IF;
     ASK SysFailMon TO Reset;
     ASK SysRepMon TO Reset;
     ASK CompFailMon TO Reset;
     ASK CompRepairMon TO Reset;
     ASK CompPMMon TO Reset;
     ASK GreenMon TO Reset;
     ASK YellowMon TO Reset;
     ASK RedMon TO Reset;
     ASK MissionGrnMon TO Reset;
     ASK MissionYelMon TO Reset;
     ASK MissionRedMon TO Reset;
     FOREACH node IN ColdChangeGroup           {csb speed change}      
        ASK ColdChangeGroup TO RemoveThis(node);    
     END FOREACH; 
     IF capacityAnalysis
        FOREACH node IN nodeGroup
           ASK NodeFlowMon[node.seqNum] TO Reset;
           ASK NodeCapMon[node.seqNum] TO Reset;
        END FOREACH;
        DISPOSE(NodeFlow);
        DISPOSE(NodeCapacity);
        DISPOSE(NodeFlowMon);
        DISPOSE(NodeCapMon);        
        IF GraphicsOutput
           node:=ASK root Child("RBDNode", startId);
           flowLabel:=ASK node Child("RBDNodeKofN", 0);
           ASK flowLabel TO SetText("");       
           ASK flowLabel TO Draw;
        END IF;        
     END IF;
     IF activePhases>0
        FOREACH block IN  BlockPhaseImproveGroup              
           ASK BlockPhaseImproveGroup TO RemoveThis(block);
        END FOREACH;
        FOREACH block IN  BlockPhaseDegradeGroup
           ASK BlockPhaseDegradeGroup TO RemoveThis(block);
        END FOREACH;
        FOREACH block IN  BlockPhaseStressGroup
           ASK BlockPhaseStressGroup TO RemoveThis(block);
        END FOREACH;
        FOREACH node IN  NodePhaseImproveGroup
           ASK NodePhaseImproveGroup TO RemoveThis(node);
        END FOREACH;
        FOREACH node IN  NodePhaseDegradeGroup
           ASK NodePhaseDegradeGroup TO RemoveThis(node);
        END FOREACH;        
        FOREACH block IN pmDeferGroup 
           ASK pmDeferGroup TO RemoveThis(block);
        END FOREACH;   
        IF weakAnalysis
           FOREACH node IN NodeFailureGroup                 
              ASK NodeFailureGroup TO RemoveThis(node);    
           END FOREACH; 
        END IF;                    
     END IF;
     IF (TieBreaking=TRUE) 
        ASK EventController TO SetTieBreaking(FALSE);
        TieBreaking:=FALSE; 
     END IF;         
     IF CapFile 
        ASK CapacityStream TO WriteLn; 
        ASK CapacityStream TO WriteString
        ("******************************End_of_Run_"+INTTOSTR(runNumber)+"******************************");
        ASK CapacityStream TO WriteLn; 
        ASK CapacityStream TO WriteLn; 
     END IF;  
     IF AvailPlotFile 
        ASK AoPlotStream TO WriteLn; 
        ASK AoPlotStream TO WriteString
        ("******************************End_of_Run_"+INTTOSTR(runNumber)+"******************************");
        ASK AoPlotStream TO WriteLn; 
        ASK AoPlotStream TO WriteLn; 
     END IF; 
     IF CostFile
        ASK CostStream TO WriteLn; 
        ASK CostStream TO WriteString
        ("****************************************End_of_Run_"+INTTOSTR(runNumber)+"****************************************");
        ASK CostStream TO WriteLn; 
        ASK CostStream TO WriteLn; 
     END IF;  
     IF Aborted
        runNumber:=NumRuns+1;    
     ELSE 
        runNumber:=runNumber+1;           
     END IF;
     IF ((EventsFile) AND (runNumber <= NumRuns)) 
        ASK EventStream TO WriteLn; 
        WriteEvents(0.000000,"System","Begin_Run_"+INTTOSTR(runNumber),"--","--");
     END IF;  
  END WHILE;
  SimCompleted:=TRUE;
  IF diagnostics 
     ASK diagnosticsStream TO Open(diagFile, Append);
     DateTime(Date);
     nextString:="EndSimulation  " + Date;
     ASK diagnosticsStream TO WriteString(nextString);   
     ASK diagnosticsStream TO WriteLn; 
     ASK diagnosticsStream TO Close; 
  END IF; 
  IF KeepSimStats 
     DoEndOfSimUpdate(); 
     simulated:=TRUE; 
     IF (kpfTimeMesh>0.0)                                   {cmc 3/6/07}
         ASK System TO WriteCumParamFile();
     END IF;
  ELSE
     simulated:=FALSE;
  END IF; 
  FOREACH link IN linkGroup
     IF ((link.Status<>LinkUp) OR capacityAnalysis)
        ASK link TO ChangeLinkStatus(LinkUp);
     END IF;
  END FOREACH;
  FOREACH block IN blockGroup
     ASK block TO ChangeBlockState(Running,Active,"Back_To_GUI");
     ASK BlockTrigger[block.seqNum] TO ObjTerminate;
     IF (block.failStream<70)
        DISPOSE(FailStream[block.seqNum]);     
        DISPOSE(RepairStream[block.seqNum]);       
     END IF; 
     IF (weakAnalysis AND KeepSimStats)
        ASK block TO ShowAnalColor;
     END IF;
  END FOREACH;
  DISPOSE(BlockTrigger);  
  FOREACH event IN eventGroup
     ASK event TO ChangeEventState(Success,Active,"Back_To_GUI");
     IF (event.failStream<70)
        DISPOSE(EventFailStream[event.seqNum]);     
     END IF; 
     IF (weakAnalysis AND KeepSimStats)
        ASK event TO ShowAnalColor;
     END IF;
  END FOREACH;
  FOREACH node IN nodeGroup
     IF (node.Status<>AllUp)
        ASK node TO ChangeNodeState(AllUp,Active);
     END IF;
     ASK node TO DisposeSimVars;
     IF (weakAnalysis AND KeepSimStats)
        ASK node TO ShowAnalColor;
     END IF;
  END FOREACH;
  IF ((SparesTrackingGroup.numberIn>0) OR (totalPools>0)) 
     IF SpareMonsCreated 
        ASK InitNumSparesMon TO Reset;
        ASK MinNumSparesMon TO Reset;
        ASK MaxNumSparesMon TO Reset;
        ASK NumOfWaitsMon TO Reset;
        ASK EndingSparesMon TO Reset; 
        ASK NewSparesMon TO Reset; 
     END IF;
     DISPOSE(InitNumSpares);
     DISPOSE(AveNumSpares);
     DISPOSE(MinNumSpares);
     DISPOSE(MaxNumSpares);
     DISPOSE(NumOfWaits);
     DISPOSE(EndingSpares);  
     DISPOSE(NewSpares);
     DISPOSE(currNumWaits);  
     DISPOSE(currNumArrivals);
     DISPOSE(currInitNumber);
  END IF;
  DISPOSE(SparesUsed);
  IF EndSpMonCreated
     ASK EndSparesUsedMon TO Reset;
  END IF;
  IF (SparesTrackingGroup.numberIn>0) 
     FOREACH block IN SparesTrackingGroup 
        j:=block.seqNum;
        IF (SparesTrackingGroup.Includes(block))
           ASK CustomSpares[j] TO ObjTerminate;
           ASK CustSpareMon[j] TO Reset;
        END IF;
     END FOREACH;  
     DISPOSE(CustomSpares); 
     DISPOSE(SparesAvailable);
     DISPOSE(CustSpareMon);
  END IF; 
  IF (totalPools>0) 
     FOR i:=1 TO totalPools
        ASK PooledSpares[i] TO ObjTerminate;
        ASK PoolMon[i] TO Reset;
     END FOR;
     DISPOSE(PooledSpares);
     DISPOSE(PoolResAvailable);
     DISPOSE(PoolMon);
  END IF; 
  FOREACH node IN NodeDepGroup                 
     ASK NodeDepGroup TO RemoveThis(node);    
  END FOREACH; 
  FOREACH node IN nodeHasDepsGroup                 
     ASK nodeHasDepsGroup TO RemoveThis(node);    
  END FOREACH; 
  FOREACH block IN BlockDepGroup                 
     ASK BlockDepGroup TO RemoveThis(block);    
  END FOREACH; 
  FOREACH block IN blockHasDepsGroup                 
     ASK blockHasDepsGroup TO RemoveThis(block);    
  END FOREACH; 
  FOREACH event IN eventHasDepsGroup                 
     ASK eventHasDepsGroup TO RemoveThis(event);    
  END FOREACH; 
  FOREACH block IN SparesTrackingGroup                         
     ASK SparesTrackingGroup TO RemoveThis(block);    
  END FOREACH;  
  IF activePhases>0
     FOREACH thing IN BlockPhaseChangeGroup 
        ASK BlockPhaseChangeGroup TO RemoveThis(thing);    
     END FOREACH;    
     FOREACH node IN NodePhaseChangeGroup 
        ASK node TO SetKofN(node.goodPaths,node.connectIntoNum);        
        ASK NodePhaseChangeGroup TO RemoveThis(node);    
     END FOREACH; 
     ASK pmSyncTrigger TO ObjTerminate;
  END IF;
  CloseOutFiles(RamInterval); 
  ASK BlockDepGroup TO ObjTerminate;
  ASK NodeDepGroup TO ObjTerminate;
  ASK nodeHasDepsGroup TO ObjTerminate;
  ASK blockHasDepsGroup TO ObjTerminate;
  ASK eventHasDepsGroup TO ObjTerminate;
  ASK ColdChangeGroup TO ObjTerminate;
  ASK SparesTrackingGroup TO ObjTerminate;
  IF (activePhases>0)
     ASK BlockPhaseChangeGroup TO ObjTerminate;
     ASK BlockPhaseImproveGroup TO ObjTerminate;
     ASK BlockPhaseDegradeGroup TO ObjTerminate;
     ASK BlockPhaseStressGroup TO ObjTerminate;
     ASK NodePhaseChangeGroup TO ObjTerminate;
     ASK NodePhaseImproveGroup TO ObjTerminate;
     ASK NodePhaseDegradeGroup TO ObjTerminate;
     ASK pmDeferGroup TO ObjTerminate;  
     IF weakAnalysis
        ASK NodeFailureGroup TO ObjTerminate;
     END IF;     
  END IF;
  DISPOSE(System);
  IF totalBlocks>0
     DISPOSE(FailStream);
     DISPOSE(RepairStream);
  END IF;
  IF totalEvents>0
     DISPOSE(EventFailStream);
  END IF;
  FOR i:=1 TO 11
     DISPOSE(SysStream[i]);
  END FOR;
  DISPOSE(SysStream);
  IF weakAnalysisMonsSet
     ASK AoMon TO Reset;
     ASK DoMon TO Reset;
     ASK CompRMon TO Reset;
  END IF;  
  IF weakAnalysis 
     DISPOSE(NodeGreenTime);
     DISPOSE(NodeYellowTime);
     DISPOSE(NodeRedTime);
     DISPOSE(NodeBlueTime);
     DISPOSE(NodeBrownTime);
     DISPOSE(NodeOrangeTime);
     DISPOSE(NodeEndGreen);
     DISPOSE(NodeEndYellow);
     DISPOSE(NodeEndRed);
     DISPOSE(NodeEndBlue);
     DISPOSE(NodeEndBrown);
     DISPOSE(NodeEndOrange);
     DISPOSE(NodeEndAo);
     DISPOSE(NodeEndDo);
     DISPOSE(NodeEndR);
     DISPOSE(BlockRunTime);
     DISPOSE(BlockStandbyTime);
     DISPOSE(BlockRepairTime);
     DISPOSE(BlockRepHoldTime);
     DISPOSE(BlockIdleTime);
     DISPOSE(BlockPMTime);
     DISPOSE(BlockPMHoldTime);
     DISPOSE(BlockDoneTime);
     DISPOSE(BlockEndRun);
     DISPOSE(BlockEndStandby);
     DISPOSE(BlockEndRep);
     DISPOSE(BlockEndRepHold);
     DISPOSE(BlockEndIdle);
     DISPOSE(BlockEndPM);
     DISPOSE(BlockEndPMHold);
     DISPOSE(BlockEndDone); 
     DISPOSE(BlockEndAo);
     DISPOSE(BlockEndDo);
     DISPOSE(BlockEndR);
     DISPOSE(blockMissionBool);
     DISPOSE(blockMCompleted);  
     DISPOSE(nodeMissionBool);
     DISPOSE(nodeMCompleted);
     IF totalEvents>0
        DISPOSE(EventRunTime);
        DISPOSE(EventStandbyTime);
        DISPOSE(EventRepairTime);
        DISPOSE(EventEndRun);
        DISPOSE(EventEndStandby);
        DISPOSE(EventEndRep);
        DISPOSE(EventEndAo);
        DISPOSE(EventEndDo);
        DISPOSE(EventEndR);
        DISPOSE(eventMCompleted);
     END IF;
     IF totalHiers>0
        DISPOSE(hMTBDE);
        DISPOSE(hMDT);
        DISPOSE(hierDE);
     END IF;
  END IF;
  IF BlockCostMonsSet
     ASK OpCostMon TO Reset;
     ASK RepCostMon TO Reset;
     IF totalBlocks>0
        ASK BlockIdleCostMon TO Reset;
        ASK BlockHoldCostMon TO Reset;
        ASK BlockPMCostMon TO Reset;
        ASK BlockpmHoldCostMon TO Reset;
        ASK BlockSBCostMon TO Reset;
        ASK BlockDoneCostMon TO Reset;
        ASK BlockDispCostMon TO Reset;
        ASK BlockSpCostMon TO Reset;
        ASK BlockESCostMon TO Reset;
     END IF;
  END IF;
  IF PoolCostMonsSet
     ASK PoolAddCostMon TO Reset;
     ASK PoolESCostMon TO Reset;
  END IF;
  IF capacityMonsSet
     ASK NodeAveFlowMon TO Reset;
     ASK NodeMinFlowMon TO Reset;
     ASK NodeMaxFlowMon TO Reset;
     ASK NodeUsageMon TO Reset;
     ASK NodeAveCapMon TO Reset;
     ASK NodeMinCapMon TO Reset;
     ASK NodeMaxCapMon TO Reset;
     ASK NodeCapabilityMon TO Reset;
     ASK UnitsProducedMon TO Reset;
  END IF;
  IF costAnalysis
     DISPOSE(BlockRepCost);
     DISPOSE(BlockOpCost);  
     DISPOSE(BlockSpCost);  
     DISPOSE(BlockESCost);  
     DISPOSE(BlockIdleCost);
     DISPOSE(BlockHoldCost);
     DISPOSE(BlockPMCost);
     DISPOSE(BlockpmHoldCost);
     DISPOSE(BlockSBCost); 
     DISPOSE(BlockDoneCost);
     DISPOSE(BlockDispCost);
     DISPOSE(BlockEndOpCost);
     DISPOSE(BlockEndRepCost);
     DISPOSE(BlockEndSpCost);
     DISPOSE(BlockEndESCost);
     DISPOSE(BlockEndIdleCost);
     DISPOSE(BlockEndHoldCost);
     DISPOSE(BlockEndPMCost);
     DISPOSE(BlockEndpmHoldCost);
     DISPOSE(BlockEndSBCost);
     DISPOSE(BlockEndDoneCost);
     DISPOSE(BlockEndDispCost);
     IF totalEvents>0
        DISPOSE(EventEndOpCost);
        DISPOSE(EventEndRepCost);
     END IF;
     ASK SystemCostMon TO Reset;            
     ASK SystemLostCostMon TO Reset;
  END IF;
  IF capacityAnalysis
     DISPOSE(NodeAveFlow);
     DISPOSE(NodeMinFlow);
     DISPOSE(NodeMaxFlow);
     DISPOSE(NodeUsage);
     DISPOSE(NodeAveCap);
     DISPOSE(NodeMinCap);
     DISPOSE(NodeMaxCap);
     DISPOSE(NodeCapability);  
  END IF;
  IF ((totalPools>0) AND costAnalysis)
     DISPOSE(PoolAddCost);
     DISPOSE(PoolEndAddCost);
     DISPOSE(PoolESCost);
     DISPOSE(PoolEndESCost);
  END IF;
  IF AvailGraph
     ASK MultiRunDataMon TO Reset;
     DISPOSE(MultiRunData);  
  END IF;
  IF (relMesh>0.0)
     DISPOSE(reliabilityFileData);
  END IF;  
  IF (kpfTimeMesh>0.0)            {cmc 3/6/07}
     DISPOSE(CumAoByTime);
     DISPOSE(CumMTBDEbyTime);
     DISPOSE(CumMDTbyTime); 
     DISPOSE(CumMTBMbyTime);
     DISPOSE(CumMMTbyTime);
  END IF;
 
  ASK AvailMon TO Reset;
  ASK DependMon TO Reset;
  ASK GTotalMon TO Reset;
  ASK MTBDEMon TO Reset;
  ASK MDTMon TO Reset;
  ASK MTBMuMon TO Reset;
  ASK MTBMsMon TO Reset;
  ASK MTBMMon TO Reset;
  ASK MMTMon TO Reset;
  ASK MMTsMon TO Reset;
  ASK MMTuMon TO Reset;
  ASK GreenPercentMon TO Reset;
  ASK YellowPercentMon TO Reset;
  ASK RedPercentMon TO Reset; 
  ASK MissionGrnPerMon TO Reset;
  ASK MissionYelPerMon TO Reset;
  ASK MissionRedPerMon TO Reset;
  ASK Term1Mon TO Reset; 
  ASK Term2Mon TO Reset; 
  ASK Term3Mon TO Reset; 
  ASK RMon TO Reset;
  ASK RCondMon TO Reset;
  DISPOSE(EventController);  
  IF NOT exploded
     returnToRBD := FALSE;
     inStepMode:=FALSE;
     REPEAT
        HandleEvents(TRUE);
     UNTIL(returnToRBD);
  ELSE
     exploded := FALSE;
  END IF;
  FOREACH block IN blockGroup 
     ASK block TO CleanUp;
  END FOREACH;
  FOREACH event IN eventGroup
     ASK event TO CleanUp
  END FOREACH;   
  FOREACH node IN nodeGroup 
     ASK node TO CleanUp;
  END FOREACH;
  FOREACH hier IN hierGroup
     ASK hier TO CleanUp;
  END FOREACH;
END PROCEDURE;     {StartEngine} 


PROCEDURE StartFEVmode;
   VAR
     i     : INTEGER;
     phase : PhaseObj;
     link  : LinkObj;
     block : RBDBlockObj;
     event : RBDEventObj;
     node  : RBDNodeObj;
     hier  : RBDHierObj;
   BEGIN
     FEVmode:=TRUE;
     preFEVcost:=costAnalysis;
     costAnalysis:=FALSE;
     preFEVnode:=weakAnalysis;
     weakAnalysis:=FALSE;
     preFEVgraphics:=GraphicsOutput;
     GraphicsOutput:=TRUE;
     NEW(System);
     NEW(BlockDepGroup);
     NEW(FevDepGroup);
     NEW(NodeDepGroup);
     NEW(ColdChangeGroup);
     IF activePhases>0
        NEW(BlockPhaseChangeGroup);
        NEW(BlockPhaseImproveGroup);
        NEW(BlockPhaseDegradeGroup);
        NEW(BlockPhaseStressGroup);
        NEW(NodePhaseChangeGroup);
        NEW(NodePhaseImproveGroup);
        NEW(NodePhaseDegradeGroup);  
     END IF;
     IF totalBlocks>0
        NEW(BlockTrigger,1..totalBlocks); 
     END IF; 
     FOREACH hier IN hierGroup
        ASK hier TO SetToInitValues;
     END FOREACH;
     FOREACH block IN blockGroup
        NEW(BlockTrigger[block.seqNum]);
        ASK block TO fevInitialize;
        IF ((block.usesPhasing) AND (activePhases>0))
           ASK BlockPhaseChangeGroup TO Add(block);        
        END IF;  
        ASK block TO CheckForDeps;          
     END FOREACH;
     FOREACH event IN eventGroup
        ASK event TO fevInitialize;
        IF ((event.usesPhasing) AND (activePhases>0))
           ASK BlockPhaseChangeGroup TO Add(event);        
        END IF;  
        ASK event TO CheckForDeps;          
     END FOREACH;
     FOREACH node IN nodeGroup
        ASK node TO Initialize();          
        IF ((node.usesPhasing)  AND (activePhases>0))
           ASK NodePhaseChangeGroup TO Add(node);
        END IF;
        IF node.coldStandby
           ASK node TO IdAndSortColds;
           ASK ColdChangeGroup TO Add(node);
        END IF;
        IF capacityAnalysis AND (node.CapConnectTo<>NILARRAY)
           ASK node TO SortCapacityArrays;
        END IF;
        ASK node TO CheckForDeps;          
     END FOREACH;
     NEW(nodeHasDepsGroup);
     FOREACH node IN nodeGroup
        IF node.hasDepObjects
           ASK nodeHasDepsGroup TO Add(node);
        END IF;
     END FOREACH;
     NEW(blockHasDepsGroup);
     FOREACH block IN blockGroup
        IF block.hasDepObjects
           ASK blockHasDepsGroup TO Add(block);
        END IF;
     END FOREACH;
     NEW(eventHasDepsGroup);
     FOREACH event IN eventGroup
        IF event.hasDepObjects
           ASK eventHasDepsGroup TO Add(event);
        END IF;
     END FOREACH;

     PhaseChangeInProgress:=FALSE;
     ASK System TO Initialize; 
     NumCurrentlyFailed:=0; 
     phaseNumber:=1;
     IF (activePhases>0)
        phase:=phaseObjArray[1];
        ASK window TO ShowStatus(3,phase.phaseName);  
     END IF;
     FOREACH link IN linkGroup
        ASK link TO SetToInitValues;
     END FOREACH;
     SystemStateChange:=FALSE;
     IF capacityAnalysis
        NEW(NodeFlow,1..totalNodes);
     END IF;
     ASK System TO SetToInitValues();
     runCompleted:=FALSE;
     FOREACH block IN blockGroup 
        ASK block TO fevReset();  
     END FOREACH;
     FOREACH event IN eventGroup
        ASK event TO fevReset();  
     END FOREACH;
     {Phase1}
     simInProgress:=TRUE;  {this line needs to be before dep. backflow}
     FOREACH node IN nodeGroup 
        ASK node TO SetToInitValues();
     END FOREACH;
     FOREACH block IN blockGroup 
        ASK block TO fevStartCond();
     END FOREACH;
     FOREACH event IN eventGroup
        ASK event TO fevStartCond();
     END FOREACH;
     FOREACH node IN nodeGroup 
        IF node.typeNode=5
           IF ((node.usesPhasing) AND (activePhases>0))
              IF node.phase[1]=-1
                 ASK node TO ChangeNodeState(node.Status,Cut);
              ELSIF node.phase[1]=0
                 ASK node TO ChangeNodeState(node.Status,Linked);
              END IF;
           END IF;
        ELSE
           IF node.GoodPathsRequired>node.EconnectIntoNum
              ASK node TO ReconfigureNode(0,0,0,0,0,0); 
           END IF;
        END IF;
        IF node.Status=Down
           ASK node TO BackFlowDependencies(Down);
        END IF;
     END FOREACH;  
     CheckColds;
     AnalyzeSystem;  
{*****}
END PROCEDURE;   {StartFEVmode}

PROCEDURE EndFEVmode;
VAR
   i                              : INTEGER;
   flowLabel                      : TextObj;
   block                          : RBDBlockObj; 
   link                           : LinkObj;
   node                           : RBDNodeObj;
   hier                           : RBDHierObj;
   event                          : RBDEventObj;
  thing                           : RBDBasicObj;

BEGIN
     simInProgress:=FALSE;
     runCompleted:=TRUE;
     ASK System TO ChangeStatus(DDone,NonMission);  
     IF capacityAnalysis
        FOREACH node IN nodeGroup
           ASK node TO ChangeNodeState(AllUp,Active);
           ASK node TO ShowFlow;
        END FOREACH;
        node:=ASK root Child("RBDNode", startId);
        flowLabel:=ASK node Child("RBDNodeKofN", 0);
        ASK flowLabel TO SetText("");       
        ASK flowLabel TO Draw;
        DISPOSE(NodeFlow);
     END IF;
     IF activePhases>0
        FOREACH block IN  BlockPhaseImproveGroup
           ASK BlockPhaseImproveGroup TO RemoveThis(block);
        END FOREACH;
        FOREACH block IN  BlockPhaseDegradeGroup
           ASK BlockPhaseDegradeGroup TO RemoveThis(block);
        END FOREACH;
        FOREACH block IN  BlockPhaseStressGroup
           ASK BlockPhaseStressGroup TO RemoveThis(block);
        END FOREACH;
        FOREACH node IN  NodePhaseImproveGroup
           ASK NodePhaseImproveGroup TO RemoveThis(node);
        END FOREACH;
        FOREACH node IN  NodePhaseDegradeGroup
           ASK NodePhaseDegradeGroup TO RemoveThis(node);
        END FOREACH;        
        IF weakAnalysis
           FOREACH node IN NodeFailureGroup                 
              ASK NodeFailureGroup TO RemoveThis(node);    
           END FOREACH; 
        END IF;                    
     END IF;
  SimCompleted:=TRUE;
  FOREACH link IN linkGroup
     IF ((link.Status<>LinkUp) OR capacityAnalysis)
        ASK link TO ChangeLinkStatus(LinkUp);
     END IF;
  END FOREACH;
  FOREACH block IN blockGroup
     ASK block TO fevCleanUp;                     {chuck}
     ASK BlockTrigger[block.seqNum] TO ObjTerminate;
     ASK block TO CleanUp;
  END FOREACH;
  DISPOSE(BlockTrigger);         
  FOREACH event IN eventGroup
     ASK event TO fevCleanUp
  END FOREACH;   
  FOREACH node IN nodeGroup
     IF (node.Status<>AllUp)
        ASK node TO ChangeNodeState(AllUp,Active);
     END IF;
     ASK node TO DisposeSimVars;
     ASK node TO CleanUp;
  END FOREACH;
  FOREACH hier IN hierGroup
     ASK hier TO CleanUp;
  END FOREACH;
  FOREACH node IN NodeDepGroup                 
     ASK NodeDepGroup TO RemoveThis(node);    
  END FOREACH; 
  FOREACH block IN BlockDepGroup                 
     ASK BlockDepGroup TO RemoveThis(block);    
  END FOREACH; 
  FOREACH block IN FevDepGroup                 
     ASK FevDepGroup TO RemoveThis(block);    
  END FOREACH; 
  FOREACH node IN nodeHasDepsGroup                 
     ASK nodeHasDepsGroup TO RemoveThis(node);    
  END FOREACH; 
  FOREACH block IN blockHasDepsGroup                 
     ASK blockHasDepsGroup TO RemoveThis(block);    
  END FOREACH; 
  FOREACH event IN eventHasDepsGroup                
     ASK eventHasDepsGroup TO RemoveThis(event);    
  END FOREACH; 
  FOREACH node IN ColdChangeGroup                 
     ASK ColdChangeGroup TO RemoveThis(node);    
  END FOREACH; 
  IF activePhases>0
     FOREACH thing IN BlockPhaseChangeGroup 
        ASK BlockPhaseChangeGroup TO RemoveThis(thing);    
     END FOREACH;
     FOREACH node IN NodePhaseChangeGroup 
        ASK node TO SetKofN(node.goodPaths,node.connectIntoNum);        
        ASK NodePhaseChangeGroup TO RemoveThis(node);    
     END FOREACH; 
  END IF;
  ASK BlockDepGroup TO ObjTerminate;
  ASK NodeDepGroup TO ObjTerminate;
  ASK nodeHasDepsGroup TO ObjTerminate;
  ASK blockHasDepsGroup TO ObjTerminate;
  ASK eventHasDepsGroup TO ObjTerminate;

  ASK ColdChangeGroup TO ObjTerminate;
  IF (activePhases>0)
     ASK BlockPhaseChangeGroup TO ObjTerminate;
     ASK BlockPhaseImproveGroup TO ObjTerminate;
     ASK BlockPhaseDegradeGroup TO ObjTerminate;
     ASK BlockPhaseStressGroup TO ObjTerminate;
     ASK NodePhaseChangeGroup TO ObjTerminate;
     ASK NodePhaseImproveGroup TO ObjTerminate;
     ASK NodePhaseDegradeGroup TO ObjTerminate;
  END IF;
  DISPOSE(System);
  costAnalysis:=preFEVcost;
  weakAnalysis:=preFEVnode;
  GraphicsOutput:=preFEVgraphics;
  FEVmode:=FALSE;  
END PROCEDURE;   {EndFEVmode}

PROCEDURE RefreshAllFEV;
VAR
   block                          : RBDBlockObj;
   event                          : RBDEventObj;
   link                           : LinkObj;
   node                           : RBDNodeObj;
   hier                           : RBDHierObj;
   phase                          : PhaseObj;          {chuck}
BEGIN
     runCompleted:=TRUE;
     FOREACH hier IN hierGroup
        ASK hier TO SetToInitValues;
     END FOREACH;
     PhaseChangeInProgress:=FALSE;
     NumCurrentlyFailed:=0; 
     FOREACH link IN linkGroup
        ASK link TO SetToInitValues;
     END FOREACH;
     phaseNumber:=1;
     IF activePhases>0
        phase:=phaseObjArray[phaseNumber];         {chuck}
        ASK window TO ShowStatus(3,phase.phaseName);
     END IF;
     IF (FevDepGroup.numberIn>0)                          
        FOREACH block IN FevDepGroup
           ASK block TO SetFevDepMode(FALSE);
           ASK FevDepGroup TO RemoveThis(block);
        END FOREACH;
     END IF;
     FOREACH block IN blockGroup 
        ASK block TO fevReset();  
     END FOREACH;
     FOREACH event IN eventGroup
        ASK event TO fevReset();
     END FOREACH;   
     FOREACH node IN nodeGroup 
        ASK node TO SetToInitValues();
        IF node.coldStandby                    {csb speed change}
           ASK ColdChangeGroup TO Add(node);
        END IF;        
     END FOREACH;
     SystemStateChange:=FALSE;
     ASK System TO SetToInitValues();
     ASK window TO DisplayFace(1);
     runCompleted:=FALSE;
     FOREACH block IN blockGroup 
        ASK block TO fevStartCond();    
     END FOREACH;
     FOREACH event IN eventGroup 
        ASK event TO fevStartCond();    
     END FOREACH;
     FOREACH node IN nodeGroup 
        IF node.typeNode=5
           IF ((node.usesPhasing) AND (activePhases>0))
              IF node.phase[1]=-1
                 ASK node TO ChangeNodeState(node.Status,Cut);
              ELSIF node.phase[1]=0
                 ASK node TO ChangeNodeState(node.Status,Linked);
              END IF;
           END IF;
        ELSE
           IF node.GoodPathsRequired>node.EconnectIntoNum
              ASK node TO ReconfigureNode(0,0,0,0,0,0); 
           END IF;
        END IF;
        IF node.Status=Down
           ASK node TO BackFlowDependencies(Down);
        END IF;
     END FOREACH;  
  
     
     CheckColds;
     AnalyzeSystem;
END PROCEDURE;   {RefreshAllFEV}
    
PROCEDURE RefreshPhase;
VAR
   block : RBDBlockObj;
   event : RBDEventObj;
BEGIN
  IF (FevDepGroup.numberIn>0)                          
     FOREACH block IN FevDepGroup
        ASK block TO SetFevDepMode(FALSE);
        ASK FevDepGroup TO RemoveThis(block);
     END FOREACH;
  END IF;
  FOREACH block IN blockGroup 
     CASE block.opStatus
        WHEN Repairing:
           ASK block TO ChangeBlockState(Running,block.activeStatus,"");
        WHEN PM:
           ASK block TO ChangeBlockState(Running,block.activeStatus,"");
        OTHERWISE             
         {do nothing}
     END CASE; 
  END FOREACH;
  FOREACH event IN eventGroup
     IF (event.opStatus = Failure)
        ASK event TO ChangeEventState(Success,event.activeStatus,"")
     END IF;
  END FOREACH;
  CheckColds;
  AnalyzeSystem;
END PROCEDURE;   {RefreshPhase}
    
PROCEDURE CreateFMECAarray;
VAR
   CutGroup, LinkGroup, FailedGroup,DegradedGroup,
   InducedFailures,IdleBlocks,PMBlocks,FailedEvents,
   FailedSubs, DegradedSubs                                  : QueueObj;
   i,lineNum, stringSize, nameLength, maxSize                : INTEGER;
   block                                                     : RBDBlockObj;
   event                                                     : RBDEventObj;
   node                                                      : RBDNodeObj;
   hier                                                      : RBDHierObj;
   tempString, startString, nameString                       : STRING;
   phase                                                     : PhaseObj;
   object                                                    : RBDBasicObj;
BEGIN
   maxSize:=80;
   NEW(CutGroup);
   NEW(LinkGroup);
   NEW(FailedGroup);
   NEW(DegradedGroup);
   NEW(InducedFailures);
   NEW(IdleBlocks);
   NEW(PMBlocks);
   NEW(FailedEvents);
   NEW(FailedSubs);
   NEW(DegradedSubs);
   IF FMECAarray<>NILARRAY
      DISPOSE(FMECAarray);
   END IF;
   NEW(FMECAarray,1..200);
   lineNum:=0;
   FOREACH block IN blockGroup
      IF block.activeStatus=Cut
         ASK CutGroup TO Add(block);
      ELSIF block.activeStatus=Linked
         ASK LinkGroup TO Add(block);
      ELSIF (block.opStatus=Repairing) 
         IF block.FevDepMode
            ASK InducedFailures TO Add(block);
         ELSE
            ASK FailedGroup TO Add(block);
         END IF;
      ELSIF (block.opStatus=Idle)
         ASK IdleBlocks TO Add(block);
      ELSIF (block.opStatus=PM)
         ASK PMBlocks TO Add(block);
      END IF;   
   END FOREACH;
   FOREACH event IN eventGroup
      IF event.activeStatus=Cut
         ASK CutGroup TO Add(event);
      ELSIF event.activeStatus=Linked
         ASK LinkGroup TO Add(event);
      ELSIF (event.opStatus=Failure)
         ASK FailedEvents TO Add(event);
      END IF;   
   END FOREACH;
   IF activePhases>0
      phase:=phaseObjArray[phaseNumber];
      FMECAarray[1]:="PHASING SCENARIO";
      FMECAarray[2]:="";
      FMECAarray[3]:="     Phase Name:         "+phase.phaseName;
      FMECAarray[4]:="";
      FMECAarray[5]:="     Phase Number:       "+INTTOSTR(phaseNumber);
      FMECAarray[6]:="";
      lineNum:=6;      
      tempString   :="     Cut Components:     ";
      startString:=tempString;
      stringSize:=25;
      IF CutGroup.numberIn>0
         FOREACH object IN CutGroup 
            nameString:=object.name;
            nameLength:=STRLEN(nameString);
            IF (tempString = startString)
               tempString:=tempString+nameString;
               stringSize:=stringSize+nameLength;
            ELSE
               IF (stringSize+2+nameLength)<=maxSize
                  tempString:=tempString+", "+nameString;
                  stringSize:=stringSize+2+nameLength;
               ELSE
                  INC(lineNum)
                  FMECAarray[lineNum]:=tempString;
                  tempString:="                         "+nameString;
                  stringSize:=25+nameLength;
               END IF;
            END IF;
            ASK CutGroup TO RemoveThis(object);    
         END FOREACH;
      ELSE
         tempString:=tempString+"None";      
      END IF;
      INC(lineNum);
      FMECAarray[lineNum]:=tempString;
      INC(lineNum);
      FMECAarray[lineNum]:="";
      tempString :="     Linked Components:  ";
      startString:=tempString;
      stringSize:=25;
      IF LinkGroup.numberIn>0
         FOREACH object IN LinkGroup 
            nameString:=object.name;
            nameLength:=STRLEN(nameString);
            IF (tempString = startString)
               tempString:=tempString+nameString;
               stringSize:=stringSize+nameLength;
            ELSE
               IF (stringSize+2+nameLength)<=maxSize
                  tempString:=tempString+", "+nameString;
                  stringSize:=stringSize+2+nameLength;
               ELSE
                  INC(lineNum)
                  FMECAarray[lineNum]:=tempString;
                  tempString:="                         "+nameString;
                  stringSize:=25+nameLength;
               END IF;
            END IF;
            ASK LinkGroup TO RemoveThis(object);    
         END FOREACH;
      ELSE
         tempString:=tempString+"None";      
      END IF;
      INC(lineNum);
      FMECAarray[lineNum]:=tempString;
      INC(lineNum);
      FMECAarray[lineNum]:="";
      
      INC(lineNum);
      FMECAarray[lineNum]:="";
   END IF;    {Phasing}
   
   INC(lineNum);
   FMECAarray[lineNum]:="FAILURES";
   INC(lineNum);
   FMECAarray[lineNum]:="";
   tempString   :="     Failed Components:  ";
   startString:=tempString;
   stringSize:=25;
   IF FailedGroup.numberIn>0
      FOREACH block IN FailedGroup 
         nameString:=block.name;
         nameLength:=STRLEN(nameString);
         IF (tempString = startString)
            tempString:=tempString+nameString;
            stringSize:=stringSize+nameLength;
         ELSE
            IF (stringSize+2+nameLength)<=maxSize
               tempString:=tempString+", "+nameString;
               stringSize:=stringSize+2+nameLength;
            ELSE
               INC(lineNum)
               FMECAarray[lineNum]:=tempString;
               tempString:="                         "+nameString;
               stringSize:=25+nameLength;
            END IF;
         END IF;
         ASK FailedGroup TO RemoveThis(block);    
      END FOREACH;
   ELSE
      tempString:=tempString+"None";      
   END IF;
   INC(lineNum);
   FMECAarray[lineNum]:=tempString;
   INC(lineNum);
   FMECAarray[lineNum]:="";

   IF FailedEvents.numberIn>0      
      tempString   :="     Failed Events:      ";
      startString:=tempString;
      stringSize:=25;
      FOREACH event IN FailedEvents 
         nameString:=event.name;
         nameLength:=STRLEN(nameString);
         IF (tempString = startString)
            tempString:=tempString+nameString;
            stringSize:=stringSize+nameLength;
         ELSE
            IF (stringSize+2+nameLength)<=maxSize
               tempString:=tempString+", "+nameString;
               stringSize:=stringSize+2+nameLength;
            ELSE
               INC(lineNum)
               FMECAarray[lineNum]:=tempString;
               tempString:="                         "+nameString;
               stringSize:=25+nameLength;
            END IF;
         END IF;
         ASK FailedEvents TO RemoveThis(event);    
      END FOREACH;
      INC(lineNum);
      FMECAarray[lineNum]:=tempString;
      INC(lineNum);
      FMECAarray[lineNum]:="";
   END IF;
   IF InducedFailures.numberIn>0      
      tempString   :="     Induced Failures:   ";
      startString:=tempString;
      stringSize:=25;
      FOREACH block IN InducedFailures 
         nameString:=block.name;
         nameLength:=STRLEN(nameString);
         IF (tempString = startString)
            tempString:=tempString+nameString;
            stringSize:=stringSize+nameLength;
         ELSE
            IF (stringSize+2+nameLength)<=maxSize
               tempString:=tempString+", "+nameString;
               stringSize:=stringSize+2+nameLength;
            ELSE
               INC(lineNum)
               FMECAarray[lineNum]:=tempString;
               tempString:="                         "+nameString;
               stringSize:=25+nameLength;
            END IF;
         END IF;
         ASK InducedFailures TO RemoveThis(block);    
      END FOREACH;
      INC(lineNum);
      FMECAarray[lineNum]:=tempString;
      INC(lineNum);
      FMECAarray[lineNum]:="";
   END IF;
   IF IdleBlocks.numberIn>0      
      tempString   :="     Idle Components:    ";
      startString:=tempString;
      stringSize:=25;
      FOREACH block IN IdleBlocks 
         nameString:=block.name;
         nameLength:=STRLEN(nameString);
         IF (tempString = startString)
            tempString:=tempString+nameString;
            stringSize:=stringSize+nameLength;
         ELSE
            IF (stringSize+2+nameLength)<=maxSize
               tempString:=tempString+", "+nameString;
               stringSize:=stringSize+2+nameLength;
            ELSE
               INC(lineNum)
               FMECAarray[lineNum]:=tempString;
               tempString:="                         "+nameString;
               stringSize:=25+nameLength;
            END IF;
         END IF;
         ASK IdleBlocks TO RemoveThis(block);    
      END FOREACH;
      INC(lineNum);
      FMECAarray[lineNum]:=tempString;
      INC(lineNum);
      FMECAarray[lineNum]:="";
   END IF;
   IF PMBlocks.numberIn>0      
      tempString   :="     PM Components:      ";
      startString:=tempString;
      stringSize:=25;
      FOREACH block IN PMBlocks 
         nameString:=block.name;
         nameLength:=STRLEN(nameString);
         IF (tempString = startString)
            tempString:=tempString+nameString;
            stringSize:=stringSize+nameLength;
         ELSE
            IF (stringSize+2+nameLength)<=maxSize
               tempString:=tempString+", "+nameString;
               stringSize:=stringSize+2+nameLength;
            ELSE
               INC(lineNum)
               FMECAarray[lineNum]:=tempString;
               tempString:="                         "+nameString;
               stringSize:=25+nameLength;
            END IF;
         END IF;
         ASK PMBlocks TO RemoveThis(block);    
      END FOREACH;
      INC(lineNum);
      FMECAarray[lineNum]:=tempString;
      INC(lineNum);
      FMECAarray[lineNum]:="";
   END IF;   
   FOREACH node IN nodeGroup
      IF (node.typeNode=2)  
         IF ((node.Status=Down) OR (node.Status=NodeIdle) OR (node.Status=NodePM))
             ASK FailedGroup TO Add(node);
         ELSIF ((node.Status=Degraded) OR (node.Status=NodeIdle) OR (node.Status=NodePM))
             ASK DegradedGroup TO Add(node);
         END IF;
      ELSIF (node.typeNode=5)
         IF ((node.Status=Down) OR (node.Status=NodeIdle) OR (node.Status=NodePM))
             ASK FailedSubs TO Add(node);
         ELSIF ((node.Status=Degraded) OR (node.Status=NodeIdle) OR (node.Status=NodePM))
             ASK DegradedSubs TO Add(node);
         END IF;
      END IF;
   END FOREACH;
   
   INC(lineNum);
   FMECAarray[lineNum]:="EFFECTS";
   INC(lineNum);
   FMECAarray[lineNum]:="";
   IF FailedSubs.numberIn>0      
      tempString   :="     Down Subsystems:     ";
      startString:=tempString;
      stringSize:=25;
      FOREACH node IN FailedSubs 
         hier:=ASK root Child("RBDHier",node.parentID);
         nameString:=hier.name;
         nameLength:=STRLEN(nameString);
         IF (tempString = startString)
            tempString:=tempString+nameString;
            stringSize:=stringSize+nameLength;
         ELSE
            IF (stringSize+2+nameLength)<=maxSize
               tempString:=tempString+", "+nameString;
               stringSize:=stringSize+2+nameLength;
            ELSE
               INC(lineNum)
               FMECAarray[lineNum]:=tempString;
               tempString:="                         "+nameString;
               stringSize:=25+nameLength;
            END IF;
         END IF;
         ASK FailedSubs TO RemoveThis(node);    
      END FOREACH;
      INC(lineNum);
      FMECAarray[lineNum]:=tempString;
      INC(lineNum);
      FMECAarray[lineNum]:="";
   END IF;   
   IF DegradedSubs.numberIn>0      
      tempString   :="     Degraded Subsystems: ";
      startString:=tempString;
      stringSize:=25;
      FOREACH node IN DegradedSubs 
         hier:=ASK root Child("RBDHier",node.parentID);
         nameString:=hier.name;
         nameLength:=STRLEN(nameString);
         IF (tempString = startString)
            tempString:=tempString+nameString;
            stringSize:=stringSize+nameLength;
         ELSE
            IF (stringSize+2+nameLength)<=maxSize
               tempString:=tempString+", "+nameString;
               stringSize:=stringSize+2+nameLength;
            ELSE
               INC(lineNum)
               FMECAarray[lineNum]:=tempString;
               tempString:="                         "+nameString;
               stringSize:=25+nameLength;
            END IF;
         END IF;
         ASK DegradedSubs TO RemoveThis(node);    
      END FOREACH;
      INC(lineNum);
      FMECAarray[lineNum]:=tempString;
      INC(lineNum);
      FMECAarray[lineNum]:="";
   END IF;   
   tempString   :="     Down Nodes:         ";
   startString:=tempString;
   stringSize:=25;
   IF FailedGroup.numberIn>0
      FOREACH node IN FailedGroup 
         nameString:=node.name;
         nameLength:=STRLEN(nameString);
         IF (tempString = startString)
            tempString:=tempString+nameString;
            stringSize:=stringSize+nameLength;
         ELSE
            IF (stringSize+2+nameLength)<=maxSize
               tempString:=tempString+", "+nameString;
               stringSize:=stringSize+2+nameLength;
            ELSE
               INC(lineNum)
               FMECAarray[lineNum]:=tempString;
               tempString:="                         "+nameString;
               stringSize:=25+nameLength;
            END IF;
         END IF;
         ASK FailedGroup TO RemoveThis(node);    
      END FOREACH;
   ELSE
      tempString:=tempString+"None";      
   END IF;
   INC(lineNum);
   FMECAarray[lineNum]:=tempString;
   INC(lineNum);
   FMECAarray[lineNum]:="";
   tempString   :="     Degraded Nodes:     ";
   startString:=tempString;
   stringSize:=25;
   IF DegradedGroup.numberIn>0
      FOREACH node IN DegradedGroup 
         nameString:=node.name;
         nameLength:=STRLEN(nameString);
         IF (tempString = startString)
            tempString:=tempString+nameString;
            stringSize:=stringSize+nameLength;
         ELSE
            IF (stringSize+2+nameLength)<=maxSize
               tempString:=tempString+", "+nameString;
               stringSize:=stringSize+2+nameLength;
            ELSE
               INC(lineNum)
               FMECAarray[lineNum]:=tempString;
               tempString:="                         "+nameString;
               stringSize:=25+nameLength;
            END IF;
         END IF;
         ASK DegradedGroup TO RemoveThis(node);    
      END FOREACH;
   ELSE
      tempString:=tempString+"None";      
   END IF;
   INC(lineNum);
   FMECAarray[lineNum]:=tempString;
   INC(lineNum);
   FMECAarray[lineNum]:="";
   INC(lineNum);
   FMECAarray[lineNum]:="CRITICALITY";
   INC(lineNum);
   FMECAarray[lineNum]:="";
   tempString:="     System Status = "      
   CASE System.Status
      WHEN GGreen:
         tempString:="Green";
      WHEN YYellow:
         tempString:="Yellow";
      WHEN RRed:
         tempString:="Red";   
   END CASE; 
   INC(lineNum);
   FMECAarray[lineNum]:="     System Status = "+tempString;
   INC(lineNum);
   FMECAarray[lineNum]:="";
   IF password
      INC(lineNum);
      IF tempString="Red"
         FMECAarray[lineNum]:="     Criticality = "+REALTOSN(critFactor,15,6,0); 
      ELSE
         FMECAarray[lineNum]:="     Criticality = 0.0";   
      END IF;
   END IF;
   INC(lineNum);
   FMECAarray[lineNum]:="END_OF_ARRAY";
   ASK CutGroup TO ObjTerminate;
   ASK LinkGroup TO ObjTerminate;
   ASK FailedGroup TO ObjTerminate;
   ASK DegradedGroup TO ObjTerminate;
   ASK InducedFailures TO ObjTerminate;
   ASK IdleBlocks TO ObjTerminate;
   ASK PMBlocks TO ObjTerminate;
   ASK FailedEvents TO ObjTerminate;
   ASK FailedSubs TO ObjTerminate;
   ASK DegradedSubs TO ObjTerminate;
END PROCEDURE;  {CreateFMECAarray}

PROCEDURE WriteFMECAtoFile;
VAR
   i       : INTEGER;
   active  : BOOLEAN;
BEGIN
   ASK FEVStream TO WriteLn;  
   i:=1;
   active:=TRUE;
   WHILE active
      IF FMECAarray[i]="END_OF_ARRAY"
         active:=FALSE;
      ELSE
         ASK FEVStream TO WriteString(FMECAarray[i]);
         ASK FEVStream TO WriteLn;
      END IF; 
      INC(i);
   END WHILE;
   ASK FEVStream TO WriteLn;   
   ASK FEVStream TO WriteString("*****************************************************************");
   ASK FEVStream TO WriteLn;   
   ASK FEVStream TO WriteLn;   
END PROCEDURE; {WriteFMECAtoFile}

PROCEDURE WriteSPFfile;
  VAR
     i,j,k,day                                  : INTEGER;
     element                                    : ANYOBJ;
     outString,today,month,year                 : STRING;
     SPFstream,CFFstream                        : StreamObj;
     block,tempBlock                            : RBDBlockObj;
     event                                      : RBDEventObj;
     phase                                      : PhaseObj;
     filter, nameOfFile, pathName               : STRING;
     message                                    : TextBufferType; 
     goodFile                                   : BOOLEAN;
  BEGIN
     filter := "*.txt";   
     NEW(message,1..2);
     NEW(SPFstream);
     SaveAsFile(nameOfFile, pathName, filter, "SPF Data File");
     IF nameOfFile = "NoFile"
        goodFile:=FALSE;
     ELSE
        ASK SPFstream TO Open(pathName + nameOfFile, Output);     
        IF SPFstream.ioResult <> 0
           message[1]:="The file "+nameOfFile+" could not be opened!     "; 
           message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
           result:=SendAlert(message,FALSE, FALSE, TRUE);
           goodFile:=FALSE;
        ELSE
           goodFile:=TRUE;
           {Insert "System is busy" indicator here if necessary}
        END IF;
     END IF;
     DISPOSE(message); 
     IF goodFile
        ASK window TO SetSysCursor(BusyCursor);      {beta153}
        RefreshAllFEV;
        GraphicsOutput:=FALSE; 
        DateTime(today);
        day := STRTOINT(SUBSTR(9, 10, today));
        month := SUBSTR(5, 7, today);
        year := SUBSTR(STRLEN(today)-3, STRLEN(today), today);
        outString:="            " + INTTOSTR(day) + " " + month + " " + year;
        ASK SPFstream TO WriteString("Single Point Failure File        " 
                 + nameOfFile + outString);   
        ASK SPFstream TO WriteLn; 
        ASK SPFstream TO WriteLn; 
        ASK SPFstream TO WriteLn;
        {IF CompFailFile    
           NEW(CFFstream);
           ASK CFFstream TO Open(installPath+"reports\" + "CompFile.TXT", Output);
           ASK CFFstream TO WriteString("Comprehensive Failure File for "+nameOfFile);   
           ASK CFFstream TO WriteLn; 
           ASK CFFstream TO WriteLn;
        END IF;    }
        IF activePhases>0
           IF password
              ASK SPFstream TO WriteString
              ("   Phase                  Block                  Criticality ");  
           ELSE
              ASK SPFstream TO WriteString
              ("   Phase                  Block");  
           END IF
           ASK SPFstream TO WriteLn;            
           ASK SPFstream TO WriteLn;            
           FOR i:= 1 TO activePhases
              ASK System TO FEVPhaseChange(i);
              phase:=phaseObjArray[i];
              k:=0;
              FOREACH block IN blockGroup
                 critFactor:=1.0;
                 IF ((block.opStatus=Running) OR (block.opStatus=Standby))
                    ASK block TO ChangeBlockState(Repairing,block.activeStatus,"");
                    CheckColds;
                    AnalyzeSystem;
                    IF System.Status=RRed
                       ASK SPFstream TO WriteString("   ");                   
                       ASK SPFstream TO WriteString(SUBSTR(1,20,phase.phaseName+"                    "));  
                       ASK SPFstream TO WriteString("   ");                   
                       ASK SPFstream TO WriteString(SUBSTR(1,20,block.name+"                    "));  
                       IF password
                          ASK SPFstream TO WriteString("   ");                   
                          ASK SPFstream TO WriteString(REALTOSN(critFactor,15,6,0));  
                       END IF;
                       ASK SPFstream TO WriteLn; 
                       INC(k);
                    END IF;
                    RefreshPhase; { ASK block TO ChangeBlockState(Running,block.activeStatus,""); }
                    IF (FevDepGroup.numberIn>0)                          
                       FOREACH tempBlock IN FevDepGroup
                          ASK tempBlock TO SetFevDepMode(FALSE);
                          ASK FevDepGroup TO RemoveThis(tempBlock);
                       END FOREACH;
                    END IF;
                    CheckColds;
                    AnalyzeSystem;
                 END IF;
              END FOREACH;
              FOREACH event IN eventGroup
                 critFactor:=1.0;
                 ASK event TO ChangeEventState(Failure,event.activeStatus,"");
                 CheckColds;
                 AnalyzeSystem;
                 IF System.Status=RRed
                    ASK SPFstream TO WriteString("   ");                   
                    ASK SPFstream TO WriteString(SUBSTR(1,20,phase.phaseName+"                    "));  
                    ASK SPFstream TO WriteString("   ");                   
                    ASK SPFstream TO WriteString(SUBSTR(1,20,event.name+"                    "));
                    IF password
                       ASK SPFstream TO WriteString("   ");                   
                       ASK SPFstream TO WriteString(REALTOSN(critFactor,15,6,0));  
                    END IF;
                    ASK SPFstream TO WriteLn; 
                    INC(k);
                 END IF;
                 RefreshPhase;   {ASK event TO ChangeEventState(Success,event.activeStatus,"");}
                 IF (FevDepGroup.numberIn>0)                          
                    FOREACH tempBlock IN FevDepGroup
                       ASK tempBlock TO SetFevDepMode(FALSE);
                       ASK FevDepGroup TO RemoveThis(tempBlock);
                    END FOREACH;
                 END IF;
                 CheckColds;
                 AnalyzeSystem;
              END FOREACH;
              IF k=0
                 ASK SPFstream TO WriteString("   No single point failures");   
                 ASK SPFstream TO WriteLn;                       
              END IF;
              ASK System TO FEVPhaseChange(1);
           END FOR;
        ELSE
           k:=0;
           IF password
              ASK SPFstream TO WriteString
              ("Block                  Criticality ");  
           ELSE
              ASK SPFstream TO WriteString
              ("Block");  
           END IF;
           ASK SPFstream TO WriteLn;            
           FOREACH block IN blockGroup
              critFactor:=1.0;
              IF ((block.opStatus=Running) OR (block.opStatus=Standby))
                 ASK block TO ChangeBlockState(Repairing,block.activeStatus,"");
                 CheckColds;
                 AnalyzeSystem;
                 IF System.Status=RRed
                    ASK SPFstream TO WriteString("   ");                   
                    ASK SPFstream TO WriteString(SUBSTR(1,20,block.name+"                    "));  
                    IF password
                       ASK SPFstream TO WriteString("   ");                   
                       ASK SPFstream TO WriteString(REALTOSN(critFactor,15,6,0));  
                    END IF;
                    ASK SPFstream TO WriteLn; 
                    INC(k);
                 END IF;              
                 RefreshAllFEV;   {ASK block TO ChangeBlockState(Running,block.activeStatus,"");}
                 IF (FevDepGroup.numberIn>0)                          
                    FOREACH tempBlock IN FevDepGroup
                       ASK tempBlock TO SetFevDepMode(FALSE);
                       ASK FevDepGroup TO RemoveThis(tempBlock);
                    END FOREACH;
                 END IF;
                 CheckColds;
                 AnalyzeSystem;
              END IF;
           END FOREACH;
           FOREACH event IN eventGroup
              critFactor:=1.0;
              ASK event TO ChangeEventState(Success,event.activeStatus,"");
              CheckColds;
              AnalyzeSystem;
              IF System.Status=RRed
                 ASK SPFstream TO WriteString("   ");                   
                 ASK SPFstream TO WriteString(SUBSTR(1,20,event.name+"                    ")); 
                 IF password
                    ASK SPFstream TO WriteString("   ");                   
                    ASK SPFstream TO WriteString(REALTOSN(critFactor,15,6,0));  
                 END IF;
                 ASK SPFstream TO WriteLn; 
                 INC(k);
              END IF;
              RefreshAllFEV;   {ASK event TO ChangeEventState(Success,event.activeStatus,"");}
              IF (FevDepGroup.numberIn>0)                          
                 FOREACH tempBlock IN FevDepGroup
                    ASK tempBlock TO SetFevDepMode(FALSE);
                    ASK FevDepGroup TO RemoveThis(tempBlock);
                 END FOREACH;
              END IF;
              CheckColds;
              AnalyzeSystem;
           END FOREACH;
           IF k=0
              ASK SPFstream TO WriteString("   No single point failures");   
              ASK SPFstream TO WriteLn;                       
           END IF; 
        END IF;
        ASK SPFstream TO Close;
        {IF CompFailFile    
           ASK CFFstream TO Close;
           DISPOSE(CFFstream);
         END IF;    }
        GraphicsOutput:=TRUE;   
        ASK window TO SetSysCursor(NormalCursor);      {beta153}
     END IF; {goodFile}
     DISPOSE(SPFstream);
END PROCEDURE;

PROCEDURE OpenOutFiles(INOUT RamInterval : INTEGER);
VAR
   filter,nameOfFile,pathName    : STRING;
   usedNames                     : ARRAY INTEGER OF STRING;
   badName                       : BOOLEAN;
   i                             : INTEGER;
BEGIN
   filter := "*.txt";   
   NEW(usedNames, 1..16);
   NEW(message,1..2);
   IF keyParamFile 
      NEW(keyStream); 
      REPEAT
         badName := FALSE;
         SaveAsFile(nameOfFile, pathName, filter, "Key Parameters");  
         IF nameOfFile = "NoFile"  
            keyParamFile := FALSE;
            DISPOSE(keyStream);
         ELSE 
            ASK keyStream TO Open(pathName + nameOfFile, Output);  
            IF keyStream.ioResult = 0
               usedNames[1] := nameOfFile;
            ELSE
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;  
   IF SysFailTimeFile
      NEW(FailTimeStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "System Failure Times");
         FOR i := 1 TO 1
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               SysFailTimeFile := FALSE;
               DISPOSE(FailTimeStream);
            ELSE
               ASK FailTimeStream TO Open(pathName + nameOfFile, Output);     
               IF FailTimeStream.ioResult = 0
                  usedNames[2] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF; 
   IF SysRepTimeFile
      NEW(RepTimeStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "System Down Times");
         FOR i := 1 TO 2
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               SysRepTimeFile := FALSE;
               DISPOSE(RepTimeStream);
            ELSE
               ASK RepTimeStream TO Open(pathName + nameOfFile, Output);     
               IF RepTimeStream.ioResult = 0
                  usedNames[3] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;
   IF SparesFile 
      NEW(SpareStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "Spare Data File Name");
         FOR i := 1 TO 3
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               SparesFile := FALSE;
               DISPOSE(SpareStream);
            ELSE
               ASK SpareStream TO Open(pathName + nameOfFile, Output);     
               IF SpareStream.ioResult = 0
                  usedNames[4] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;
   IF EventsFile
      NEW(EventStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "Detailed Event Log");
         FOR i := 1 TO 4
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               EventsFile := FALSE;
               DISPOSE(EventStream);
            ELSE
               ASK EventStream TO Open(pathName + nameOfFile, Output);     
               IF EventStream.ioResult = 0
                  usedNames[5] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;
   IF EORFile
      NEW(EndOfRunStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "End Of Run Report");
         FOR i := 1 TO 5
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               EORFile := FALSE;
               DISPOSE(EndOfRunStream);
            ELSE
               ASK EndOfRunStream TO Open(pathName + nameOfFile, Output);     
               IF EndOfRunStream.ioResult = 0
                  usedNames[6] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;
   IF ResultsFile
      NEW(ResultsStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "Final Results Report");
         FOR i := 1 TO 6
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               ResultsFile := FALSE;
               DISPOSE(ResultsStream);
            ELSE
               ASK ResultsStream TO Open(pathName + nameOfFile, Output);     
               IF ResultsStream.ioResult = 0
                  usedNames[7] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;
   IF CapFile
      NEW(CapacityStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "Capacity Plot Data");
         FOR i := 1 TO 7
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               CapFile := FALSE;
               DISPOSE(CapacityStream);
            ELSE
               ASK CapacityStream TO Open(pathName + nameOfFile, Output);     
               IF CapacityStream.ioResult = 0
                  usedNames[8] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;
   IF EndSimTimeFile
      NEW(EndSimTimeStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "Ending Sim Times");
         FOR i := 1 TO 8
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               EndSimTimeFile := FALSE;
               DISPOSE(EndSimTimeStream);
            ELSE
               ASK EndSimTimeStream TO Open(pathName + nameOfFile, Output);     
               IF EndSimTimeStream.ioResult = 0
                  usedNames[9] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;
   IF NodeFile
      weakAnalysis:=TRUE;
      NEW(NodeStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "Node Report File Name");
         FOR i := 1 TO 9
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               NodeFile := FALSE;
               DISPOSE(NodeStream);
            ELSE
               ASK NodeStream TO Open(pathName + nameOfFile, Output);     
               IF NodeStream.ioResult = 0
                  usedNames[10] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;
   IF CostFile
      NEW(CostStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "Interim Cost Report");
         FOR i := 1 TO 10
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               CostFile := FALSE;
               DISPOSE(CostStream);
            ELSE
               ASK CostStream TO Open(pathName + nameOfFile, Output);     
               IF CostStream.ioResult = 0
                  usedNames[11] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;
   IF AvailPlotFile
      NEW(AoPlotStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "Availability Plot Data");
         FOR i := 1 TO 11
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName 
            IF nameOfFile = "NoFile"
               AvailPlotFile := FALSE;
               DISPOSE(AoPlotStream);
            ELSE
               ASK AoPlotStream TO Open(pathName + nameOfFile, Output); 
               IF AoPlotStream.ioResult = 1
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               ELSE
                  usedNames[12] := nameOfFile;                              
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;  
   IF (RamInterval>0)
      NEW(RamStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "Cumulative By Trial File");
         FOR i := 1 TO 12
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               RamInterval:=0;
               DISPOSE(RamStream);
            ELSE
               ASK RamStream TO Open(pathName + nameOfFile, Output);     
               IF RamStream.ioResult = 0
                  usedNames[13] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;
 {  IF (relMesh>0.0)
      NEW(relStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "Reliability File");
         FOR i := 1 TO 13
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               {RamInterval:=0; }
               relMesh:=0.0;      {cmc 3/6/07}
               DISPOSE(relStream);
            ELSE
               ASK relStream TO Open(pathName + nameOfFile, Output);     
               IF relStream.ioResult = 0
                  usedNames[14] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;   }
   IF (kpfTimeMesh>0.0)
      NEW(kpfTimeStream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "Cumulative By Time File");
         FOR i := 1 TO 14
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               kpfTimeMesh:=0.0;
               DISPOSE(kpfTimeStream);
            ELSE
               ASK kpfTimeStream TO Open(pathName + nameOfFile, Output);     
               IF kpfTimeStream.ioResult = 0
                  usedNames[15] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;
   IF C130File   
      NEW(C130Stream);
      badName := FALSE;
      REPEAT
         SaveAsFile(nameOfFile, pathName, filter, "Mission File");
         FOR i := 1 TO 15
            IF nameOfFile = usedNames[i]
               badName := TRUE;
               message[1]:="The file "+nameOfFile+" could not be opened!     "; 
               message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
               result:=SendAlert(message,FALSE, FALSE, TRUE);
            ELSE
               badName := FALSE;
            END IF;
         END FOR;
         IF NOT badName
            IF nameOfFile = "NoFile"
               C130File := FALSE;
               DISPOSE(C130Stream);
            ELSE
               ASK C130Stream TO Open(pathName + nameOfFile, Output);     
               IF C130Stream.ioResult = 0
                  usedNames[16] := nameOfFile;
               ELSE
                  badName := TRUE;
                  message[1]:="The file "+nameOfFile+" could not be opened!     "; 
                  message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
               END IF;
            END IF;
         END IF;
      UNTIL NOT badName;
   END IF;
   DISPOSE(message);   
   DISPOSE(usedNames);
END PROCEDURE; {OpenOutFiles}  

PROCEDURE CloseOutFiles(IN RamInterval : INTEGER);
BEGIN
  IF keyParamFile
    ASK keyStream TO Close; 
    DISPOSE(keyStream);  
  END IF;
  IF SysFailTimeFile
    ASK FailTimeStream TO Close;     
    DISPOSE(FailTimeStream);
  END IF;
  IF SysRepTimeFile
    ASK RepTimeStream TO Close;     
    DISPOSE(RepTimeStream);
  END IF;
  IF SparesFile
    ASK SpareStream TO Close;     
    DISPOSE(SpareStream);
  END IF;
  IF EventsFile
    ASK EventStream TO Close;     
    DISPOSE(EventStream);
  END IF; 
  IF EORFile
    ASK EndOfRunStream TO Close;     
    DISPOSE(EndOfRunStream);
  END IF;  
  IF ResultsFile
    ASK ResultsStream TO Close;     
    DISPOSE(ResultsStream);
  END IF;  
  IF CapFile
    ASK CapacityStream TO Close;     
    DISPOSE(CapacityStream);
  END IF;  
  IF EndSimTimeFile
    ASK EndSimTimeStream TO Close;     
    DISPOSE(EndSimTimeStream);
  END IF; 
  IF NodeFile
    ASK NodeStream TO Close;     
    DISPOSE(NodeStream);
  END IF; 
  IF CostFile
    ASK CostStream TO Close;     
    DISPOSE(CostStream);
  END IF; 
  IF AvailPlotFile
    ASK AoPlotStream TO Close;     
    DISPOSE(AoPlotStream);
  END IF; 
  IF (RamInterval>0)
     ASK RamStream TO Close;
     DISPOSE(RamStream);
  END IF;
{  IF (relMesh>0.0)
     ASK relStream TO Close;
     DISPOSE(relStream);
  END IF;       }
  IF (kpfTimeMesh>0.0)      {cmc 3/6/07}
     ASK kpfTimeStream TO Close;
     DISPOSE(kpfTimeStream);
  END IF;
  IF C130File    
    ASK C130Stream TO WriteString("-2 -2 -2");  
    ASK C130Stream TO WriteLn;     
    ASK C130Stream TO Close;     
    DISPOSE(C130Stream);
  END IF; 
END PROCEDURE; {CloseOutFiles}     


PROCEDURE DoEndOfRunUpdate();
VAR
  AoString,sigmaString,DoString,RString,RCondString       : STRING;   
  i,j,numAllocated,totalAllocated,systemCapIndex          : INTEGER; 
  bict,boct,brct,bcsct,bhct,bidct,bdct,bspct,besct,bsict,
  bdfct,btoct,btsct,btct,eict,eoct,efct,etct,pict,pact,
  pesct,ptct,puc,ptc,rict,rpuct,rptct,rtct,grandTotal,
  totalTime,sigma,depTotal,AveLostFlow,AoDenom,DoDenom,
  pmc,pmhc,bpmct,bpmhct                                   : REAL;    
  block                                                   : RBDBlockObj;
  node                                                    : RBDNodeObj;
  OperatingCost,SparingCost,sic,ic,icEv,icPool            : ARRAY INTEGER OF REAL;
  HierCosts                                               : ARRAY INTEGER, INTEGER OF REAL;
  hier,mamaHier                                           : RBDHierObj;
  event                                                   : RBDEventObj;
  hMDTstring                                              : ARRAY INTEGER OF STRING;
BEGIN
   IF statusBarOn
      IF (totalBlocks>0)
         ASK window TO ShowStatus(5,"Ao: "+REALTOSN(AvailMon.Mean,15,9,0));      
         ASK window TO ShowStatus(6,"MTBDE: "+SUBSTR(1,10,REALTOSN(MTBDEMon.Mean,15,6,0)));
         IF MDTMon.Count>0
           ASK window TO ShowStatus(7,"MDT: "+SUBSTR(1,10,REALTOSN(MDTMon.Mean,15,6,0)));
         ELSE
            ASK window TO ShowStatus(7,"MDT: "+"N/A");      
         END IF;
      END IF;
      ASK window TO ShowStatus(8,"R: "+REALTOSN(RMon.Mean,15,9,0));
   END IF; 
   AoString:=FormatStats(Availability);
   IF missionsAttempted=0
      DoString:="        N/A";
      RString:="        N/A";
      RCondString:="        N/A";
   ELSE   
      DoString:=FormatStats(Dependability);
      RString:=FormatStats(Reliability);
      RCondString:=FormatStats(RCond);
   END IF;   
   IF keyParamFile 
   
      ASK keyStream TO WriteString(SUBSTR(1,10,INTTOSTR(runNumber)+"          "));   
   
      ASK keyStream TO WriteString(AoString+"  ");   
      ASK keyStream TO WriteString(DoString+"  ");   
      ASK keyStream TO WriteString(RString+"  ");   
      ASK keyStream TO WriteString(RCondString+"  "); 
      IF MTBDEupdated
         ASK keyStream TO WriteReal(MTBDE,19,9);
         ASK keyStream TO WriteString(" ");
      ELSE
         ASK keyStream TO WriteString("                N/A ");
      END IF; 
      IF MDTupdated
         ASK keyStream TO WriteReal(MDT,19,9);   
         ASK keyStream TO WriteString(" ");
      ELSE
         ASK keyStream TO WriteString("                N/A ");
      END IF;  
      IF MTBMupdated
         ASK keyStream TO WriteReal(MTBM,19,9);
         ASK keyStream TO WriteString(" ");
      ELSE
         ASK keyStream TO WriteString("                N/A ");
      END IF;  
      IF MMTupdated
         ASK keyStream TO WriteReal(MMT,19,9);
         ASK keyStream TO WriteString(" ");
      ELSE
         ASK keyStream TO WriteString("                N/A ");
      END IF;   
      ASK keyStream TO WriteLn;      
   END IF;
   IF EndSimTimeFile
      ASK EndSimTimeStream TO WriteReal(SimTime,19,9);   
      ASK EndSimTimeStream TO WriteLn; 
   END IF;

  IF totalBlocks>0
     IF costAnalysis
        NEW(OperatingCost,1..totalBlocks);
        NEW(SparingCost,1..totalBlocks);
     END IF;   
     NEW(sic,1..totalBlocks);
     NEW(ic,1..totalBlocks);
     IF (totalHiers>0)
        NEW(HierCosts,1..totalHiers,1..6);
     END IF;
  END IF;
  FOREACH block IN blockGroup
     EndSparesUsed[block.seqNum]:=SparesUsed[block.seqNum];
     i:=block.seqNum;
     IF ((SparesTrackingGroup.numberIn>0) AND (SparesTrackingGroup.Includes(block)))
        MinNumSpares[i]:=CustSpareMon[i].Minimum;
        AveNumSpares[i]:=CustSpareMon[i].Mean;
        MaxNumSpares[i]:=CustSpareMon[i].Maximum;
        EndingSpares[i]:=CustomSpares[i].Resources; 
        InitNumSpares[i]:=currInitNumber[i];
        NumOfWaits[i]:=currNumWaits[i];            
        IF (CustomSpares[i].PendingList.numberIn> 0)
           ASK CustomSpares[i].PendingList TO Empty;
        END IF;
        IF (CustomSpares[i].AllocationList.numberIn> 0)
           numAllocated:=CustomSpares[i].NumberAllocatedTo(block);
           IF numAllocated>0;                     
              ASK CustomSpares[i] TO TakeBack(block,numAllocated);                  
           END IF;
        END IF; 
        ASK CustomSpares[i] TO ResetPendingStats;
        ASK CustomSpares[i] TO Reset;          
        NewSpares[i]:=currNumArrivals[i];
     END IF;   
     IF (costAnalysis)
        BlockEndOpCost[i]:=BlockOpCost[i];   
        BlockEndRepCost[i]:=BlockRepCost[i]; 
        BlockEndSpCost[i]:=BlockSpCost[i];
        BlockEndESCost[i]:=BlockESCost[i];
        BlockEndIdleCost[i]:=BlockIdleCost[i];   
        BlockEndHoldCost[i]:=BlockHoldCost[i];
        BlockEndPMCost[i]:=BlockPMCost[i];
        BlockEndpmHoldCost[i]:=BlockpmHoldCost[i];
        BlockEndSBCost[i]:=BlockSBCost[i];
        BlockEndDoneCost[i]:=BlockDoneCost[i];         
        BlockEndDispCost[i]:=BlockDispCost[i];
        OperatingCost[i]:=BlockEndOpCost[i]+BlockEndRepCost[i]+BlockEndIdleCost[i]+BlockEndHoldCost[i]+
                           BlockEndSBCost[i]+BlockEndDoneCost[i]+BlockEndPMCost[i]+BlockEndpmHoldCost[i];
        IF ((block.sparingType=Custom) AND (NOT ZeroInitCost))  
           sic[i]:=FLOAT(block.initStock)*block.spareCost;   
        ELSE
           sic[i]:=0.0;
        END IF;
        SparingCost[i]:=sic[i]+BlockEndSpCost[i]+BlockEndESCost[i];                   
        boct:=boct+BlockEndOpCost[i];
        brct:=brct+BlockEndRepCost[i];
        bidct:=bidct+BlockEndIdleCost[i];
        bhct:=bhct+BlockEndHoldCost[i];
        bcsct:=bcsct+BlockEndSBCost[i];
        bdct:=bdct+BlockEndDoneCost[i];
        bpmct:=bpmct+BlockEndPMCost[i];
        bpmhct:=bpmhct+BlockEndpmHoldCost[i];
        IF ZeroInitCost
           ic[i]:=0.0;
        ELSE
           ic[i]:=block.initialCost;
        END IF;
        bict:=bict+ic[i];
        btoct:=btoct+OperatingCost[i];
        btsct:=btsct+SparingCost[i];
        bdfct:=bdfct+BlockEndDispCost[i];
        btct:=btct+ic[i]+OperatingCost[i]+SparingCost[i]+BlockEndDispCost[i];
        IF ((totalHiers>0) AND (block.parentID>0))
           hier := ASK root Child("RBDHier",block.parentID);
           HierCosts[hier.seqNum,1]:=HierCosts[hier.seqNum,1]+ic[i];
           HierCosts[hier.seqNum,2]:=HierCosts[hier.seqNum,2]+OperatingCost[i];
           HierCosts[hier.seqNum,3]:=HierCosts[hier.seqNum,3]+SparingCost[i];
           HierCosts[hier.seqNum,4]:=HierCosts[hier.seqNum,4]+BlockEndDispCost[i];
           HierCosts[hier.seqNum,6]:=HierCosts[hier.seqNum,6]+ic[i]+OperatingCost[i]+SparingCost[i]+BlockEndDispCost[i];
        END IF;
     END IF; {costAnalysis}
     IF weakAnalysis
        AoDenom:=(BlockRunTime[i]+BlockStandbyTime[i]+BlockIdleTime[i]+BlockRepairTime[i]+BlockRepHoldTime[i]
                     +BlockPMTime[i]+BlockPMHoldTime[i]+BlockDoneTime[i]);
        DoDenom:=(BlockRunTime[i]+BlockRepairTime[i]+BlockRepHoldTime[i]+BlockDoneTime[i]);
        IF AoDenom>0.0
           BlockEndAo[i]:=(BlockRunTime[i]+BlockStandbyTime[i])/AoDenom; 
           IF DoDenom>0.0
              BlockEndDo[i]:=(BlockRunTime[i])/ DoDenom;
           ELSE
              BlockEndDo[i]:=1.0;
           END IF;
        ELSE
           BlockEndAo[i]:=1.0;
           BlockEndDo[i]:=1.0;
        END IF;  
        IF missionsAttempted>0
           BlockEndR[i]:=FLOAT(blockMCompleted[i])/FLOAT(missionsAttempted);
        ELSE
           BlockEndR[i]:=1.0;
        END IF;            
        IF AoDenom>0.0
           BlockEndRun[i]:=(BlockRunTime[i]/AoDenom)*100.0;
           BlockEndStandby[i]:=(BlockStandbyTime[i]/AoDenom)*100.0;
           BlockEndIdle[i]:=(BlockIdleTime[i]/AoDenom)*100.0;
           BlockEndRep[i]:=(BlockRepairTime[i]/AoDenom)*100.0;
           BlockEndRepHold[i]:=(BlockRepHoldTime[i]/AoDenom)*100.0;
           BlockEndPM[i]:=(BlockPMTime[i]/AoDenom)*100.0;
           BlockEndPMHold[i]:=(BlockPMHoldTime[i]/AoDenom)*100.0;
           BlockEndDone[i]:=(BlockDoneTime[i]/AoDenom)*100.0;
        ELSE
           BlockEndRun[i]:=100.0;
           BlockEndStandby[i]:=0.0;
           BlockEndIdle[i]:=0.0;
           BlockEndRep[i]:=0.0;
           BlockEndRepHold[i]:=0.0;
           BlockEndPM[i]:=0.0;
           BlockEndPMHold[i]:=0.0;
           BlockEndDone[i]:=0.0;
        END IF;  
     END IF; {weakAnalysis}
  END FOREACH;
  IF totalEvents>0
     IF costAnalysis
        NEW(icEv,1..totalEvents);
     END IF;
  END IF;   
  FOREACH event IN eventGroup
     i:=event.seqNum
     IF costAnalysis
        IF ZeroInitCost
           icEv[i]:=0.0;
        ELSE
           icEv[i]:=event.initialCost;
        END IF;
        EventEndOpCost[i]:=EventOpCost[i];   
        EventEndRepCost[i]:=EventRepCost[i]; 
        eict:=eict+icEv[i];
        eoct:=eoct+EventEndOpCost[i];
        efct:=efct+EventEndRepCost[i];
        etct:=etct+(icEv[i]+EventEndOpCost[i]+EventEndRepCost[i]);
        IF ((totalHiers>0) AND (event.parentID>0))
           hier := ASK root Child("RBDHier",event.parentID);
           HierCosts[hier.seqNum,5]:=HierCosts[hier.seqNum,5]+(icEv[i]+EventEndOpCost[i]+EventEndRepCost[i]);
           HierCosts[hier.seqNum,6]:=HierCosts[hier.seqNum,6]+(icEv[i]+EventEndOpCost[i]+EventEndRepCost[i]);
        END IF;
     END IF;
     IF weakAnalysis
        AoDenom:=(EventRunTime[i]+EventStandbyTime[i]+EventRepairTime[i]);
        DoDenom:=(EventRunTime[i]+EventRepairTime[i]);
        IF AoDenom>0.0
           EventEndRun[i]:=(EventRunTime[i]/AoDenom)*100.0;
           EventEndStandby[i]:=(EventStandbyTime[i]/AoDenom)*100.0;
           EventEndRep[i]:=(EventRepairTime[i]/AoDenom)*100.0;
           EventEndAo[i]:=(EventRunTime[i]+EventStandbyTime[i])/AoDenom; 
           IF DoDenom>0.0
              EventEndDo[i]:=(EventRunTime[i])/DoDenom;
           ELSE
              EventEndDo[i]:=1.0;
           END IF;
        ELSE
           EventEndRun[i]:=100.0;
           EventEndStandby[i]:=0.0;
           EventEndRep[i]:=0.0;
           EventEndAo[i]:=1.0;
           EventEndDo[i]:=1.0;
        END IF;   
        IF missionsAttempted>0
           EventEndR[i]:=FLOAT(eventMCompleted[i])/FLOAT(missionsAttempted);
        ELSE
           EventEndR[i]:=1.0;
        END IF;            
     END IF;
  END FOREACH;
  IF ((weakAnalysis) AND (totalHiers>0))
     NEW(hMDTstring,1..totalHiers);
  END IF;
  FOREACH node IN nodeGroup
     i:=node.seqNum;
     IF (capacityAnalysis)
        IF node.capNode
           NodeAveFlow[i]:=NodeFlowMon[i].Mean;
           NodeMinFlow[i]:=NodeFlowMon[i].Minimum;
           NodeMaxFlow[i]:=NodeFlowMon[i].Maximum;
           NodeAveCap[i]:=NodeCapMon[i].Mean;
           NodeMinCap[i]:=NodeCapMon[i].Minimum;
           NodeMaxCap[i]:=NodeCapMon[i].Maximum;
           IF NodeAveCap[i]<>0.0
              NodeUsage[i]:=NodeAveFlow[i]/NodeAveCap[i];
           ELSE
              NodeUsage[i]:=0.0;
           END IF;
           IF NodeMaxCap[i]<>0
              NodeCapability[i]:=NodeAveCap[i]/FLOAT(NodeMaxCap[i]);
           ELSE
              NodeCapability[i]:=0.0;
           END IF;
        END IF;
        IF node.typeNode=1
           systemCapIndex:=node.seqNum;
           UnitsProduced:=NodeAveFlow[i]*(SimTime-StatsStartTime);
           AveLostFlow:=FLOAT(flowGenerated)-NodeAveFlow[i];
        END IF;
     END IF;  {capacityAnalysis}
     IF weakAnalysis
        IF node.typeNode<>1
           totalTime:=(NodeGreenTime[i]+NodeYellowTime[i]+NodeRedTime[i]+
                        NodeBlueTime[i]+NodeBrownTime[i]+NodeOrangeTime[i]);
           depTotal:=(NodeGreenTime[i]+NodeYellowTime[i]+NodeRedTime[i]);
           IF totalTime>0.0
              NodeEndGreen[i]:=(NodeGreenTime[i]/totalTime)*100.0;
              NodeEndYellow[i]:=(NodeYellowTime[i]/totalTime)*100.0;
              NodeEndRed[i]:=(NodeRedTime[i]/totalTime)*100.0;
              NodeEndBlue[i]:=(NodeBlueTime[i]/totalTime)*100.0;
              NodeEndBrown[i]:=(NodeBrownTime[i]/totalTime)*100.0;
              NodeEndOrange[i]:=(NodeOrangeTime[i]/totalTime)*100.0;
              NodeEndAo[i]:=(NodeGreenTime[i]+NodeYellowTime[i]+NodeBlueTime[i])/totalTime; 
           ELSE
              NodeEndGreen[i]:=100.0;
              NodeEndYellow[i]:=0.0;
              NodeEndRed[i]:=0.0;
              NodeEndBlue[i]:=0.0;
              NodeEndBrown[i]:=0.0;
              NodeEndOrange[i]:=0.0;
              NodeEndAo[i]:=1.0;
           END IF;   
           IF depTotal>0.0
              NodeEndDo[i]:=(NodeGreenTime[i]+NodeYellowTime[i])/ depTotal;
           ELSE
              NodeEndDo[i]:=1.0;
           END IF;
           IF missionsAttempted>0
              NodeEndR[i]:=FLOAT(nodeMCompleted[i])/FLOAT(missionsAttempted);
           ELSE
              NodeEndR[i]:=1.0;
           END IF;
        END IF; {node.typeNode}   
        IF (node.typeNode=5)
           hier:= ASK root Child("RBDHier",node.parentID);
           IF (hierDE[hier.seqNum]>0)
              hMTBDE[hier.seqNum]:=(NodeGreenTime[i]+NodeYellowTime[i]+NodeBlueTime[i])/(FLOAT(hierDE[hier.seqNum])); 
              hMDT[hier.seqNum]:=(NodeRedTime[i]+NodeOrangeTime[i])/(FLOAT(hierDE[hier.seqNum]));
              hMDTstring[hier.seqNum]:=SUBSTR(1,10,"    "+REALTOSTR(hMDT[hier.seqNum])+"  ");
           ELSE
              hMTBDE[hier.seqNum]:=(NodeGreenTime[i]+NodeYellowTime[i]+NodeBlueTime[i]);
              hMDTstring[hier.seqNum]:="N/A";
           END IF; 
        END IF;      
     END IF; {weakAnalysis}      
  END FOREACH;
  FOREACH hier IN hierGroup
     i:=hier.seqNum;
     IF ((costAnalysis) AND (hier.parentID<>0))
        mamaHier := ASK root Child("RBDHier",hier.parentID);
        HierCosts[mamaHier.seqNum,1]:=HierCosts[mamaHier.seqNum,1]+HierCosts[i,1];
        HierCosts[mamaHier.seqNum,2]:=HierCosts[mamaHier.seqNum,2]+HierCosts[i,2];
        HierCosts[mamaHier.seqNum,3]:=HierCosts[mamaHier.seqNum,3]+HierCosts[i,3];
        HierCosts[mamaHier.seqNum,4]:=HierCosts[mamaHier.seqNum,4]+HierCosts[i,4];
        HierCosts[mamaHier.seqNum,5]:=HierCosts[mamaHier.seqNum,5]+HierCosts[i,5];
        HierCosts[mamaHier.seqNum,6]:=HierCosts[mamaHier.seqNum,6]+HierCosts[i,6];
     END IF;
  END FOREACH;
  IF (totalPools>0)
     IF costAnalysis
        NEW(icPool,1..totalPools);
     END IF;
     FOR i:= 1 TO totalPools        
        j:=i+totalBlocks;
        PoolMon[i]:=GETMONITOR(PoolResAvailable[i],ITimedStatObj); 
        MinNumSpares[j]:=PoolMon[i].Minimum;  
        AveNumSpares[j]:=PoolMon[i].Mean; 
        MaxNumSpares[j]:=PoolMon[i].Maximum;  
        EndingSpares[j]:=PooledSpares[i].Resources; 
        InitNumSpares[j]:=currInitNumber[j];
        NumOfWaits[j]:=currNumWaits[j];
        IF (PooledSpares[i].PendingList.numberIn> 0)
           ASK PooledSpares[i].PendingList TO Empty;
        END IF;
        IF (PooledSpares[i].AllocationList.numberIn> 0)
           totalAllocated:=0;
           FOREACH block IN blockGroup
              numAllocated:=PooledSpares[i].NumberAllocatedTo(block);
              totalAllocated:=totalAllocated+numAllocated;
              IF numAllocated>0;                     
                 ASK PooledSpares[i] TO TakeBack(block,numAllocated);                  
              END IF;
           END FOREACH;
        END IF;         
        NewSpares[j]:=currNumArrivals[j];
        IF costAnalysis
           IF PoolArray[i].sparingType<>ColdPool
              PoolEndAddCost[i]:=PoolAddCost[i];
              PoolEndESCost[i]:=PoolESCost[i];
           END IF;
           IF ZeroInitCost
              icPool[i]:=0.0;
           ELSE
              icPool[i]:=FLOAT(PoolArray[i].initialSpares)*PoolArray[i].spareCost;
           END IF;
           IF PoolArray[i].sparingType=SparePool
              pict:=pict+icPool[i];
              pact:=pact+PoolEndAddCost[i];
              pesct:=pesct+PoolEndESCost[i];
              ptct:=ptct+icPool[i]+PoolEndAddCost[i]+PoolEndESCost[i];
           ELSIF PoolArray[i].sparingType=Resource
              rict:=rict+icPool[i];
              rptct:=rptct+PoolEndAddCost[i];
              rpuct:=rpuct+PoolEndESCost[i];
              rtct:=rtct+icPool[i]+PoolEndAddCost[i]+PoolEndESCost[i];
           END IF;          
        END IF; {costAnalysis}
     END FOR;
     ASK PooledSpares[i] TO ResetPendingStats;
     ASK PooledSpares[i] TO Reset;
  END IF;
  IF costAnalysis
     IF ((ResultsFile) AND  (System.redCost>0.0))
        SystemEndRedCost:=TotalRedCost;
     END IF;
     IF capacityAnalysis
        SystemEndLostCost:=AveLostFlow*(SimTime-StatsStartTime)*sysLostCost;
     ELSE
        SystemEndLostCost:=0.0;
     END IF;
     GTotal:=btct+etct+ptct+rtct+TotalRedCost+SystemEndLostCost;
  END IF;

  IF EORFile  
     ASK EndOfRunStream TO WriteString("***Run_"+INTTOSTR(runNumber)+"*********************************************************************"); 
     ASK EndOfRunStream TO WriteLn;        
     ASK EndOfRunStream TO WriteLn; 
     ASK EndOfRunStream TO WriteLn;
     ASK EndOfRunStream TO WriteString("***SUMMARY***"); 
     ASK EndOfRunStream TO WriteLn; 
     ASK EndOfRunStream TO WriteLn;
     ASK EndOfRunStream TO WriteString("Availability               "+AoString);
     ASK EndOfRunStream TO WriteLn;
     IF MTBDEupdated
        ASK EndOfRunStream TO WriteString("MTBDE                      "+REALTOSTR(MTBDE));
     ELSE
        ASK EndOfRunStream TO WriteString("MTBDE                      "+"N/A");       
     END IF;
     ASK EndOfRunStream TO WriteLn; 
     IF  MDTupdated 
        ASK EndOfRunStream TO WriteString("MDT                        "+REALTOSTR(MDT));
     ELSE
        ASK EndOfRunStream TO WriteString("MDT                        "+"N/A");
     END IF;
     ASK EndOfRunStream TO WriteLn;
     IF (activePhases>0)
        ASK EndOfRunStream TO WriteString("Dependability              "+DoString);
        ASK EndOfRunStream TO WriteLn;
     END IF;  
     ASK EndOfRunStream TO WriteString("Reliability                "+RString);
     ASK EndOfRunStream TO WriteLn;
     ASK EndOfRunStream TO WriteString("ConditionalReliability     "+RCondString);
     ASK EndOfRunStream TO WriteLn;
     ASK EndOfRunStream TO WriteString(SUBSTR(1,25,"EndingSimTime"+"                            ")+"  "+REALTOSTR(SimTime)); 
     ASK EndOfRunStream TO WriteLn;
     ASK EndOfRunStream TO WriteString(SUBSTR(1,25,"NumberOfSystemFailures"+"                            ")+"  "+INTTOSTR(SysFailMon.Count)); 
     ASK EndOfRunStream TO WriteLn;
     ASK EndOfRunStream TO WriteString(SUBSTR(1,25,"NumberOfComponentFailures"+"                            ")+"  "+INTTOSTR(CompFailMon.Count));
     ASK EndOfRunStream TO WriteLn;  
     ASK EndOfRunStream TO WriteLn;
     ASK EndOfRunStream TO WriteLn; 
     ASK EndOfRunStream TO WriteLn;
     ASK EndOfRunStream TO WriteString("***LOGISTICS***"); 
     ASK EndOfRunStream TO WriteLn; 
     ASK EndOfRunStream TO WriteLn;
     IF MTBMupdated
        ASK EndOfRunStream TO WriteString("MTBM                  "+REALTOSTR(MTBM)); 
     ELSE
        ASK EndOfRunStream TO WriteString("MTBM                  "+"N/A"); 
     END IF;
     ASK EndOfRunStream TO WriteLn; 
     IF MTBMuupdated
        ASK EndOfRunStream TO WriteString("MTBMu                 "+REALTOSTR(MTBMu)); 
     ELSE
        ASK EndOfRunStream TO WriteString("MTBMu                 "+"N/A"); 
     END IF;
     ASK EndOfRunStream TO WriteLn; 
     IF MTBMsupdated
        ASK EndOfRunStream TO WriteString("MTBMs                 "+REALTOSTR(MTBMs)); 
     ELSE
        ASK EndOfRunStream TO WriteString("MTBMs                 "+"N/A"); 
     END IF;
     ASK EndOfRunStream TO WriteLn; 
     IF  MMTupdated 
        ASK EndOfRunStream TO WriteString("MMT                   "+REALTOSTR(MMT));
     ELSE
        ASK EndOfRunStream TO WriteString("MMT                   "+"N/A");
     END IF;
     ASK EndOfRunStream TO WriteLn; 
     IF  MMTuupdated 
        ASK EndOfRunStream TO WriteString("MMTu                  "+REALTOSTR(MMTu));
     ELSE
        ASK EndOfRunStream TO WriteString("MMTu                  "+"N/A");
     END IF;
     ASK EndOfRunStream TO WriteLn; 
     IF  MMTsupdated 
        ASK EndOfRunStream TO WriteString("MMTs                  "+REALTOSTR(MMTs));
     ELSE
        ASK EndOfRunStream TO WriteString("MMTs                  "+"N/A");
     END IF;
     ASK EndOfRunStream TO WriteLn; 
     ASK EndOfRunStream TO WriteString("GreenPercent          "+REALTOSTR(GreenPercent)); 
     ASK EndOfRunStream TO WriteLn; 
     ASK EndOfRunStream TO WriteString("YellowPercent         "+REALTOSTR(YellowPercent)); 
     ASK EndOfRunStream TO WriteLn; 
     ASK EndOfRunStream TO WriteString("RedPercent            "+REALTOSTR(RedPercent)); 
     ASK EndOfRunStream TO WriteLn; 
     IF activePhases>0
        ASK EndOfRunStream TO WriteString("MissionGreenPercent   "+REALTOSTR(MissionGrnPer)); 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteString("MissionYellowPercent  "+REALTOSTR(MissionYelPer)); 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteString("MissionRedPercent     "+REALTOSTR(MissionRedPer)); 
        ASK EndOfRunStream TO WriteLn; 
     END IF;
     ASK EndOfRunStream TO WriteLn; 
     ASK EndOfRunStream TO WriteString("                                Minimum           Mean        Maximum         St.Dev"); 
     ASK EndOfRunStream TO WriteLn; 
     IF (SysFailMon.Count>0)   
        ASK EndOfRunStream TO WriteString(SUBSTR(1,23,"TBDE"+"                          ")+"  "); 
        ASK EndOfRunStream TO WriteString(REALTOSN(SysFailMon.Minimum,14,6,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(SysFailMon.Mean,14,6,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(SysFailMon.Maximum,14,6,1)+" ");
        IF (SysFailMon.Count>1) 
           sigma:=PopToSample(SysFailMon.StdDev,SysFailMon.Count);
           ASK EndOfRunStream TO WriteString(REALTOSN(sigma,14,6,1));
        ELSE
           ASK EndOfRunStream TO WriteString("           N/A");   
        END IF;
     ELSE
        ASK EndOfRunStream TO WriteString("No System Failures!");   
     END IF;        
     ASK EndOfRunStream TO WriteLn; 
     IF (RedMon.Count>0) 
        ASK EndOfRunStream TO WriteString(SUBSTR(1,23,"DT"+"                      ")+"  "); 
        ASK EndOfRunStream TO WriteString(REALTOSN(RedMon.Minimum,14,6,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(RedMon.Mean,14,6,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(RedMon.Maximum,14,6,1)+" ");
        IF (RedMon.Count>1) 
           sigma:=PopToSample(RedMon.StdDev,RedMon.Count);
           ASK EndOfRunStream TO WriteString(REALTOSN(sigma,14,6,1));
        ELSE
           ASK EndOfRunStream TO WriteString("           N/A");   
        END IF;
     ELSE
        ASK EndOfRunStream TO WriteString("No System Repairs!");  
     END IF;  
     ASK EndOfRunStream TO WriteLn;            
     IF (CompFailMon.Count>0)
        ASK EndOfRunStream TO WriteString(SUBSTR(1,23,"TBM"+"                      ")+"  "); 
        ASK EndOfRunStream TO WriteString(REALTOSN(CompFailMon.Minimum,14,6,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(CompFailMon.Mean,14,6,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(CompFailMon.Maximum,14,6,1)+" ");
        IF (CompFailMon.Count>1) 
           sigma:=PopToSample(CompFailMon.StdDev,CompFailMon.Count);
           ASK EndOfRunStream TO WriteString(REALTOSN(sigma,14,6,1));
        ELSE
           ASK EndOfRunStream TO WriteString("           N/A");   
        END IF;
     ELSE
        ASK EndOfRunStream TO WriteString("No Component Failures!"); 
     END IF;  
     ASK EndOfRunStream TO WriteLn;            
     IF (CompRepairMon.Count>0)
        ASK EndOfRunStream TO WriteString(SUBSTR(1,23,"RT"+"                      ")+"  "); 
        ASK EndOfRunStream TO WriteString(REALTOSN(CompRepairMon.Minimum,14,6,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(CompRepairMon.Mean,14,6,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(CompRepairMon.Maximum,14,6,1)+" ");
        IF (CompRepairMon.Count>1) 
           sigma:=PopToSample(CompRepairMon.StdDev,CompRepairMon.Count);
           ASK EndOfRunStream TO WriteString(REALTOSN(sigma,14,6,1));
        ELSE
           ASK EndOfRunStream TO WriteString("           N/A");   
        END IF;
     ELSE
        ASK EndOfRunStream TO WriteString("No Component Repairs!"); 
     END IF;  
     ASK EndOfRunStream TO WriteLn;            
     ASK EndOfRunStream TO WriteLn;            
     ASK EndOfRunStream TO WriteLn;            
     ASK EndOfRunStream TO WriteLn;
     IF (totalBlocks>0)
        ASK EndOfRunStream TO WriteString("***SPARING***"); 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteString("Spare_Utilization"); 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteString("blockName             Failures   SpareSource           Consumed");
        ASK EndOfRunStream TO WriteLn;
        FOR j:=1 TO totalBlocks
           block := ASK root Child("RBDBlock",BlockAlphaSort[j]);
           i:=block.seqNum;
           ASK EndOfRunStream TO WriteString(SUBSTR(1,20,block.name+"                          ")); 
           ASK EndOfRunStream TO WriteInt((block.FailureCounter-block.discardedFailures),9); 
           ASK EndOfRunStream TO WriteString("   ");
           IF block.sparingType=Infinite
              ASK EndOfRunStream TO WriteString("Infinite             ");
           ELSIF block.sparingType=Custom
              ASK EndOfRunStream TO WriteString("Custom               ");
           ELSIF block.sparingType=SparePool
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,block.poolName + "                    ")+" ");
           ELSIF block.sparingType=None
              ASK EndOfRunStream TO WriteString("None                 ");
           END IF;
           ASK EndOfRunStream TO WriteInt(SparesUsed[i],9); 
           ASK EndOfRunStream TO WriteLn;
        END FOR;
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteLn;
     END IF;    {totalBlocks>0}
     IF (SparesTrackingGroup.numberIn>0)
        ASK EndOfRunStream TO WriteString("Custom_Sparing_Information"); 
        ASK EndOfRunStream TO WriteLn;  
        ASK EndOfRunStream TO WriteLn;                            
        ASK EndOfRunStream TO WriteString("blockName            InitNumSpares  NewSpares   MinStock   AveStock   MaxStock   EndStock  NumDelays");
        ASK EndOfRunStream TO WriteLn; 
        FOR j:=1 TO totalBlocks
           block := ASK root Child("RBDBlock",BlockAlphaSort[j]);
           IF block.sparingType=Custom
              i:=block.seqNum;
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,block.name+"                          ")); 
              ASK EndOfRunStream TO WriteInt(InitNumSpares[i],9);
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteInt(NewSpares[i],9);
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteInt(MinNumSpares[i],9);
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteString(REALTOSN(AveNumSpares[i],11,2,1));
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteInt(MaxNumSpares[i],9);
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteInt(EndingSpares[i],9);
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteInt(NumOfWaits[i],9);
              ASK EndOfRunStream TO WriteString(" ");
              ASK EndOfRunStream TO WriteLn; 
           END IF;   
        END FOR;     
        ASK EndOfRunStream TO WriteLn; 
     END IF;   {(SparesTrackingGroup.numberIn>0)}
     IF (totalSpares>0)
        ASK EndOfRunStream TO WriteString("SparePool_Sparing_Information"); 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteString("poolName             InitNumSpares  NewSpares   MinStock     AveStock   MaxStock   EndStock  NumDelays");
        ASK EndOfRunStream TO WriteLn; 
        FOR i:= 1 TO totalPools        
           j:=i+totalBlocks;
           IF (PoolArray[i].sparingType=SparePool)
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,PoolArray[i].poolName + "                    ")+"     "); 
              ASK EndOfRunStream TO WriteInt(InitNumSpares[j],9);
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteInt(NewSpares[j],9);
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteInt(MinNumSpares[j],9);
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteString(REALTOSN(AveNumSpares[j],11,2,1));
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteInt(MaxNumSpares[j],9);
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteInt(EndingSpares[j],9);
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteInt(NumOfWaits[j],9);
              ASK EndOfRunStream TO WriteString(" ");
              ASK EndOfRunStream TO WriteLn; 
           END IF;
        END FOR;
        ASK EndOfRunStream TO WriteLn;       
        ASK EndOfRunStream TO WriteLn;       
        ASK EndOfRunStream TO WriteLn;       
     END IF;            
     IF (totalRes>0)
        ASK EndOfRunStream TO WriteString("***RESOURCES***"); 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteString("Resource_Information"); 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteString("resourceName            InitNumRes    MinNumRes    AveNumRes    MaxNumRes EndingNumRes    NumDelays");
        ASK EndOfRunStream TO WriteLn; 
        FOR i:= 1 TO totalPools   
           j:=i+totalBlocks;
           IF (PoolArray[i].sparingType=Resource)
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,PoolArray[i].poolName + "                    ")+"     "); 
              ASK EndOfRunStream TO WriteInt(InitNumSpares[j],9);
              ASK EndOfRunStream TO WriteString("    ");
              ASK EndOfRunStream TO WriteInt(MinNumSpares[j],9);
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteString(REALTOSN(AveNumSpares[j],11,2,1));
              ASK EndOfRunStream TO WriteString("    ");
              ASK EndOfRunStream TO WriteInt(MaxNumSpares[j],9);
              ASK EndOfRunStream TO WriteString("    ");
              ASK EndOfRunStream TO WriteInt(EndingSpares[j],9);
              ASK EndOfRunStream TO WriteString("  ");
              ASK EndOfRunStream TO WriteInt(NumOfWaits[j],9);
              ASK EndOfRunStream TO WriteString(" ");
              ASK EndOfRunStream TO WriteLn; 
              IF (PooledSpares[i].PendingList.numberIn> 0)
                 ASK PooledSpares[i].PendingList TO Empty;
              END IF;
           END IF;
        END FOR;
        ASK EndOfRunStream TO WriteLn;       
        ASK EndOfRunStream TO WriteLn;       
        ASK EndOfRunStream TO WriteLn;       
     END IF;            
     IF (capacityAnalysis)
        ASK EndOfRunStream TO WriteString("***CAPACITY***"); 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("FlowGenerated           ");
        ASK EndOfRunStream TO WriteInt(flowGenerated, 9);
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("UnitsProduced        ");
        ASK EndOfRunStream TO WriteString(REALTOSN(UnitsProduced,12,2,1));
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("FlowLoss              MinLostFlow  AveLostFlow  MaxLostFlow");
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("                     ");       
        ASK EndOfRunStream TO WriteInt(flowGenerated-NodeMaxFlow[startTag], 12);  {min lost}
        ASK EndOfRunStream TO WriteString(" ");
        ASK EndOfRunStream TO WriteReal(AveLostFlow, 12,2);                    {AveLostFlow}
        ASK EndOfRunStream TO WriteString(" ");
        ASK EndOfRunStream TO WriteInt(flowGenerated-NodeMinFlow[startTag], 12);  {max lost}
        ASK EndOfRunStream TO WriteLn;  
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("                          MinFlow      AveFlow      MaxFlow    UsageRate    MinCapacity  AveCapacity  MaxCapacity  CapableRate");
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString(SUBSTR(1,20,"SystemCapacity" + "                    ")+" ");
        ASK EndOfRunStream TO WriteInt(NodeMinFlow[systemCapIndex],12);  
        ASK EndOfRunStream TO WriteString(" "+REALTOSN(NodeAveFlow[systemCapIndex],12,2,1)+" ");  
        ASK EndOfRunStream TO WriteInt(NodeMaxFlow[systemCapIndex],12);
        ASK EndOfRunStream TO WriteString(" "+REALTOSN(NodeUsage[systemCapIndex],12,2,1)+"   ");
        ASK EndOfRunStream TO WriteInt(NodeMinCap[systemCapIndex],12);
        ASK EndOfRunStream TO WriteString(" "+REALTOSN(NodeAveCap[systemCapIndex],12,2,1)+" ");
        ASK EndOfRunStream TO WriteInt(NodeMaxCap[systemCapIndex],12);
        ASK EndOfRunStream TO WriteString(" "+REALTOSN(NodeCapability[systemCapIndex],12,2,1)+"   ");
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("NodeCapacity");
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("                          MinFlow      AveFlow      MaxFlow    UsageRate    MinCapacity  AveCapacity  MaxCapacity  CapableRate");
        ASK EndOfRunStream TO WriteLn;
        FOR i:=1 TO totalNodes
           node := ASK root Child("RBDNode",NodeAlphaSort[i]);
           j:=node.seqNum;
           IF ((node.capNode) AND (node.typeNode<>1))
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,node.name+"                          "));
              ASK EndOfRunStream TO WriteInt(NodeMinFlow[j],12);
              ASK EndOfRunStream TO WriteString(" "+REALTOSN(NodeAveFlow[j],12,2,1)+" ");
              ASK EndOfRunStream TO WriteInt(NodeMaxFlow[j],12);
              IF node.fullFlow
                 ASK EndOfRunStream TO WriteString(" "+"         N/A"+"   ");               
                 ASK EndOfRunStream TO WriteString("         N/A");               
                 ASK EndOfRunStream TO WriteString(" "+"         N/A"+" ");               
                 ASK EndOfRunStream TO WriteString("         N/A");               
                 ASK EndOfRunStream TO WriteString(" "+"         N/A"+"   ");               
              ELSE
                 ASK EndOfRunStream TO WriteString(" "+REALTOSN(NodeUsage[j],12,2,1)+"   ");
                 ASK EndOfRunStream TO WriteInt(NodeMinCap[j],12);
                 ASK EndOfRunStream TO WriteString(" "+REALTOSN(NodeAveCap[j],12,2,1)+" ");
                 ASK EndOfRunStream TO WriteInt(NodeMaxCap[j],12);
                 ASK EndOfRunStream TO WriteString(" "+REALTOSN(NodeCapability[j],12,2,1)+"   ");
              END IF;
              ASK EndOfRunStream TO WriteLn;
           END IF;   {node.capNode}
        END FOR;         
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteLn;
     END IF;  {capacityAnalysis}
     IF (costAnalysis)
        ASK EndOfRunStream TO WriteString("***COST***"); 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("Operating");
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("Block                 InHier       Good      Repair        Idle     RepHold          PM      PMHold     Standby        Done");
        ASK EndOfRunStream TO WriteLn;
        FOR i:=1 TO totalBlocks
           block := ASK root Child("RBDBlock",BlockAlphaSort[i]);
           j:=block.seqNum;
           ASK EndOfRunStream TO WriteString(SUBSTR(1,20,block.name+"                          ")+" "); 
           ASK EndOfRunStream TO WriteInt(block.parentID,4);  
           ASK EndOfRunStream TO WriteString("   "+REALTOSN(BlockEndOpCost[j],11,2,1)+" ");
           ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndRepCost[j],11,2,1)+" ");
           ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndIdleCost[j],11,2,1)+" ");
           ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndHoldCost[j],11,2,1)+" ");
           ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndPMCost[j],11,2,1)+" ");
           ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndpmHoldCost[j],11,2,1)+" ");
           ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndSBCost[j],11,2,1)+" ");
           ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndDoneCost[j],11,2,1)+" ");
           ASK EndOfRunStream TO WriteLn;
        END FOR;
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("Totals                      ");
        ASK EndOfRunStream TO WriteString(REALTOSN(boct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(brct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(bidct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(bhct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(bpmct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(bpmhct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(bcsct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(bdct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("Sparing");
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("Block                 InitSpares   NewSpares    EmerShip");
        ASK EndOfRunStream TO WriteLn;
        FOR j:=1 TO totalBlocks
           block := ASK root Child("RBDBlock",BlockAlphaSort[j]);
           i:=block.seqNum;
           bsict:=bsict+sic[i];
           bspct:=bspct+BlockEndSpCost[i];
           besct:=besct+BlockEndESCost[i];
           IF (block.sparingType<>SparePool)
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,block.name+"                          ")); 
              ASK EndOfRunStream TO WriteString(REALTOSN(sic[i],11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndSpCost[i],11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndESCost[i],11,2,1)+" ");
              ASK EndOfRunStream TO WriteLn; 
           END IF;
        END FOR;
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("Totals              ");
        ASK EndOfRunStream TO WriteString(REALTOSN(bsict,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(bspct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(besct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteLn;         
        ASK EndOfRunStream TO WriteLn;         
        ASK EndOfRunStream TO WriteString("Life_cycle_cost");
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("Block                   Purchase   Operating     Sparing    Disposal       Total");
        ASK EndOfRunStream TO WriteLn;
        FOR i:=1 TO totalBlocks
           block := ASK root Child("RBDBlock",BlockAlphaSort[i]);
           j:=block.seqNum;
           ASK EndOfRunStream TO WriteString(SUBSTR(1,20,block.name+"                          ")+" "); 
           ASK EndOfRunStream TO WriteString(REALTOSN(ic[j],11,2,1)+" ");
           ASK EndOfRunStream TO WriteString(REALTOSN(OperatingCost[j],11,2,1)+" ");   
           IF (block.sparingType<>SparePool)
              ASK EndOfRunStream TO WriteString(REALTOSN(SparingCost[j],11,2,1)+" ");
           ELSE
              ASK EndOfRunStream TO WriteString("        N/A ");
           END IF;   
           ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndDispCost[j],11,2,1)+" ");
           ASK EndOfRunStream TO WriteString(REALTOSN(ic[j]+OperatingCost[j]+SparingCost[j]+BlockEndDispCost[j],11,2,1)+" ");
           ASK EndOfRunStream TO WriteLn;
        END FOR;
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("Block_Totals         ");
        ASK EndOfRunStream TO WriteString(REALTOSN(bict,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(btoct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(btsct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(bdfct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteString(REALTOSN(btct,11,2,1)+" ");
        ASK EndOfRunStream TO WriteLn;         
        ASK EndOfRunStream TO WriteLn;         
        ASK EndOfRunStream TO WriteString("________________________________________________________________________________");
        ASK EndOfRunStream TO WriteLn;         
        ASK EndOfRunStream TO WriteLn;
        IF totalEvents>0
           ASK EndOfRunStream TO WriteString("Event                 InHier    Initial     Success     Failure       Total");
           ASK EndOfRunStream TO WriteLn;
           FOR i:=1 TO totalEvents
              event := ASK root Child("RBDEvent",EventAlphaSort[i]);
              j:=event.seqNum;
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,event.name+"                          ")+" "); 
              ASK EndOfRunStream TO WriteInt(event.parentID,4);
              ASK EndOfRunStream TO WriteString("   "+REALTOSN(icEv[j],11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(EventEndOpCost[j],11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(EventEndRepCost[j],11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN((icEv[j]+EventEndOpCost[j]+EventEndRepCost[j]),11,2,1)+" ");
              ASK EndOfRunStream TO WriteLn; 
           END FOR;
           ASK EndOfRunStream TO WriteLn;
           ASK EndOfRunStream TO WriteString("Event_Totals                ");
           ASK EndOfRunStream TO WriteString(REALTOSN(eict,11,2,1)+" ");
           ASK EndOfRunStream TO WriteString(REALTOSN(eoct,11,2,1)+" ");
           ASK EndOfRunStream TO WriteString(REALTOSN(efct,11,2,1)+" ");
           ASK EndOfRunStream TO WriteString(REALTOSN(etct,11,2,1)+" ");
           ASK EndOfRunStream TO WriteLn;         
           ASK EndOfRunStream TO WriteLn;         
           ASK EndOfRunStream TO WriteString("________________________________________________________________________________");
           ASK EndOfRunStream TO WriteLn;         
           ASK EndOfRunStream TO WriteLn;  
        END IF;   {eventsExist}
        IF (totalHiers>0)
           ASK EndOfRunStream TO WriteLn;
           ASK EndOfRunStream TO WriteString("Hierarchy            HierNum    Purchase   Operating     Sparing    Disposal      Events       Total");
           ASK EndOfRunStream TO WriteLn;
           FOR j:=1 TO totalHiers  
              hier := ASK root Child("RBDHier",HierAlphaSort[j]);
              i:=hier.seqNum;
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,hier.name+"                          ")+" "); 
              ASK EndOfRunStream TO WriteInt(hier.Id,4);
              ASK EndOfRunStream TO WriteString("    "+REALTOSN(HierCosts[i,1],11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(HierCosts[i,2],11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(HierCosts[i,3],11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(HierCosts[i,4],11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(HierCosts[i,5],11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(HierCosts[i,6],11,2,1)+" ");
              ASK EndOfRunStream TO WriteLn; 
           END FOR;      
           ASK EndOfRunStream TO WriteLn;         
           ASK EndOfRunStream TO WriteLn;         
           ASK EndOfRunStream TO WriteString("____________________________________________________________________________________________________");
           ASK EndOfRunStream TO WriteLn;         
           ASK EndOfRunStream TO WriteLn; 
        END IF;  {totalHiers>0}
        IF (totalPools>0)
           IF totalSpares>0
              ASK EndOfRunStream TO WriteString("SparePools               Initial   NewSpares    EmerShip       Total");
              ASK EndOfRunStream TO WriteLn; 
              FOR i:=1 TO totalPools
                 IF PoolArray[i].sparingType=SparePool
                    ASK EndOfRunStream TO WriteString(SUBSTR(1,20,PoolArray[i].poolName + "                    ")+" ");
                    ASK EndOfRunStream TO WriteString(REALTOSN(icPool[i],11,2,1)+" ");
                    ASK EndOfRunStream TO WriteString(REALTOSN(PoolEndAddCost[i],11,2,1)+" ");
                    ASK EndOfRunStream TO WriteString(REALTOSN(PoolEndESCost[i],11,2,1)+" ");
                    ASK EndOfRunStream TO WriteString(REALTOSN(icPool[i]+PoolEndAddCost[i]+PoolEndESCost[i],11,2,1)+" ");
                    ASK EndOfRunStream TO WriteLn; 
                 END IF; 
              END FOR;
              ASK EndOfRunStream TO WriteLn;
              ASK EndOfRunStream TO WriteString("SparePool_Totals     ");
              ASK EndOfRunStream TO WriteString(REALTOSN(pict,11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(pact,11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(pesct,11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(ptct,11,2,1)+" ");
              ASK EndOfRunStream TO WriteLn;         
              ASK EndOfRunStream TO WriteLn;         
              ASK EndOfRunStream TO WriteString("________________________________________________________________________________");
              ASK EndOfRunStream TO WriteLn;         
              ASK EndOfRunStream TO WriteLn; 
           END IF;   {SparePools}
           IF totalRes>0
              ASK EndOfRunStream TO WriteString("Resources                Initial  PerUseCost PerTimeCost       Total");
              ASK EndOfRunStream TO WriteLn;
              FOR i:=1 TO totalPools
                 IF PoolArray[i].sparingType=Resource
                    ASK EndOfRunStream TO WriteString(SUBSTR(1,20,PoolArray[i].poolName + "                    ")+" ");
                    ASK EndOfRunStream TO WriteString(REALTOSN(icPool[i],11,2,1)+" ");
                    ASK EndOfRunStream TO WriteString(REALTOSN(PoolEndESCost[i],11,2,1)+" ");
                    ASK EndOfRunStream TO WriteString(REALTOSN(PoolEndAddCost[i],11,2,1)+" ");
                    ASK EndOfRunStream TO WriteString(REALTOSN((icPool[i]+PoolEndESCost[i]+PoolEndAddCost[i]),11,2,1)+" ");
                    ASK EndOfRunStream TO WriteLn; 
                 END IF;
              END FOR;
              ASK EndOfRunStream TO WriteLn;
              ASK EndOfRunStream TO WriteString("Resource_Totals      ");
              ASK EndOfRunStream TO WriteString(REALTOSN(rict,11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(rpuct,11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(rptct,11,2,1)+" ");
              ASK EndOfRunStream TO WriteString(REALTOSN(rtct,11,2,1)+" ");
              ASK EndOfRunStream TO WriteLn;         
              ASK EndOfRunStream TO WriteLn;         
              ASK EndOfRunStream TO WriteString("________________________________________________________________________________");
              ASK EndOfRunStream TO WriteLn;         
              ASK EndOfRunStream TO WriteLn;
           END IF;   {totalRes>0}
        END IF;  {pools Exist}
        ASK EndOfRunStream TO WriteString("Cost_Summary");
        ASK EndOfRunStream TO WriteLn;  
        ASK EndOfRunStream TO WriteLn;  
        ASK EndOfRunStream TO WriteString("Blocks               "+REALTOSN(btct,11,2,1));
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("Events               "+REALTOSN(etct,11,2,1));
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("SparePools           "+REALTOSN(ptct,11,2,1));
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("Resources            "+REALTOSN(rtct,11,2,1));
        ASK EndOfRunStream TO WriteLn;
        IF System.redCost>0.0;
           ASK EndOfRunStream TO WriteString("SystemDownCost       "+REALTOSN(TotalRedCost,11,2,1));
           ASK EndOfRunStream TO WriteLn;
        END IF;  
        IF capacityAnalysis;
           ASK EndOfRunStream TO WriteString("SystemLostFlowCost   "+REALTOSN(SystemEndLostCost,11,2,1));
           ASK EndOfRunStream TO WriteLn;
        END IF;  
        ASK EndOfRunStream TO WriteLn;      
        ASK EndOfRunStream TO WriteString("________________________________");
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteString("Grand_Total          "+REALTOSN(GTotal,11,2,1));
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteLn;
        ASK EndOfRunStream TO WriteLn;          
        ASK EndOfRunStream TO WriteLn;          
     END IF;  {costAnalysis}
     IF (weakAnalysis)
        ASK EndOfRunStream TO WriteString("***WEAK_LINK_ANALYSIS***"); 
        ASK EndOfRunStream TO WriteLn; 
        ASK EndOfRunStream TO WriteLn; 
        IF (totalNodes>(2+2*totalHiers))
           ASK EndOfRunStream TO WriteString("NODE_RESULTS"); 
           ASK EndOfRunStream TO WriteLn; 
           ASK EndOfRunStream TO WriteLn; 
           ASK EndOfRunStream TO WriteString("Node                             Ao           Do            R"+
                "     %Green    %Yellow       %Red    %Orange      %Blue     %Brown");
           ASK EndOfRunStream TO WriteLn;
           FOR i:=1 TO totalNodes
              node := ASK root Child("RBDNode",NodeAlphaSort[i]);
              j:=node.seqNum;
              IF ((node.typeNode=2) AND (node.reportNodeAnal))
                 ASK EndOfRunStream TO WriteString(SUBSTR(1,20,node.name+"                          ")+"  ");
                 ASK EndOfRunStream TO WriteString("  "+FormatStats(NodeEndAo[j]));
                 ASK EndOfRunStream TO WriteString("  "+FormatStats(NodeEndDo[j]));
                 ASK EndOfRunStream TO WriteString("  "+FormatStats(NodeEndR[j]));
                 ASK EndOfRunStream TO WriteString(REALTOSN(NodeEndGreen[j],11,6,1));
                 ASK EndOfRunStream TO WriteString(REALTOSN(NodeEndYellow[j],11,6,1));
                 ASK EndOfRunStream TO WriteString(REALTOSN(NodeEndRed[j],11,6,1));
                 ASK EndOfRunStream TO WriteString(REALTOSN(NodeEndOrange[j],11,6,1));
                 ASK EndOfRunStream TO WriteString(REALTOSN(NodeEndBlue[j],11,6,1));
                 ASK EndOfRunStream TO WriteString(REALTOSN(NodeEndBrown[j],11,6,1));
                 ASK EndOfRunStream TO WriteLn;  
              END IF;
           END FOR;
        END IF;
        ASK EndOfRunStream TO WriteLn;          
        ASK EndOfRunStream TO WriteLn;          
        ASK EndOfRunStream TO WriteLn;          
        IF (totalBlocks > 0)
           ASK EndOfRunStream TO WriteString("BLOCK_RESULTS"); 
           ASK EndOfRunStream TO WriteLn; 
           ASK EndOfRunStream TO WriteLn; 
           ASK EndOfRunStream TO WriteString("Block                            Ao           Do            R");
           ASK EndOfRunStream TO WriteLn;
           FOR j:=1 TO totalBlocks
              block := ASK root Child("RBDBlock",BlockAlphaSort[j]);
              i:=block.seqNum;
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,block.name+"                          ")+"  ");
              ASK EndOfRunStream TO WriteString("  "+FormatStats(BlockEndAo[i]));
              ASK EndOfRunStream TO WriteString("  "+FormatStats(BlockEndDo[i]));
              ASK EndOfRunStream TO WriteString("  "+FormatStats(BlockEndR[i]));
              ASK EndOfRunStream TO WriteLn;  
           END FOR;
           ASK EndOfRunStream TO WriteLn; 
           ASK EndOfRunStream TO WriteString("Block                     %Running    %Standby       %Idle"+    
                   "  %Repairing    %RepHold         %PM     %PMHold       %Done");
           ASK EndOfRunStream TO WriteLn;
           FOR j:=1 TO totalBlocks
              block := ASK root Child("RBDBlock",BlockAlphaSort[j]);
              i:=block.seqNum;
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,block.name+"                          ")+"  ");
              ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndRun[i],12,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndStandby[i],12,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndIdle[i],12,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndRep[i],12,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndRepHold[i],12,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndPM[i],12,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndPMHold[i],12,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(BlockEndDone[i],12,6,1));
              ASK EndOfRunStream TO WriteLn;  
           END FOR;
           ASK EndOfRunStream TO WriteLn;          
           ASK EndOfRunStream TO WriteLn;          
           ASK EndOfRunStream TO WriteLn;          
        END IF;  {totalBlocks > 0}
        IF (totalEvents>0)
           ASK EndOfRunStream TO WriteString("EVENT_RESULTS"); 
           ASK EndOfRunStream TO WriteLn; 
           ASK EndOfRunStream TO WriteLn; 
           ASK EndOfRunStream TO WriteString("Event                            Ao           Do            R"+
                                          "      %Success      %Armed    %Failure");
           ASK EndOfRunStream TO WriteLn;
           FOR j:=1 TO totalEvents
              event := ASK root Child("RBDEvent",EventAlphaSort[j]);
              i:=event.seqNum;
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,event.name+"                          ")+"  ");
              ASK EndOfRunStream TO WriteString("  "+FormatStats(EventEndAo[i]));
              ASK EndOfRunStream TO WriteString("  "+FormatStats(EventEndDo[i]));
              ASK EndOfRunStream TO WriteString("  "+FormatStats(EventEndR[i]));
              ASK EndOfRunStream TO WriteString("  "+REALTOSN(EventEndRun[i],12,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(EventEndStandby[i],12,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(EventEndRep[i],12,6,1));
              ASK EndOfRunStream TO WriteLn;  
           END FOR;
        END IF {totalEvents > 0};
        IF (totalHiers>0)
           ASK EndOfRunStream TO WriteLn;          
           ASK EndOfRunStream TO WriteString("HIERARCHY_RESULTS"); 
           ASK EndOfRunStream TO WriteLn; 
           ASK EndOfRunStream TO WriteLn; 
           ASK EndOfRunStream TO WriteString("Hierarchy                        Ao           Do            R       MTBDE       MDT");
           ASK EndOfRunStream TO WriteLn;
           FOR i:=1 TO totalHiers
              hier := ASK root Child("RBDHier",HierAlphaSort[i]);
              node := ASK root Child("RBDNode", hier.outID);
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,hier.name+"                    ")+"  ");
              ASK EndOfRunStream TO WriteString("  "+FormatStats(NodeEndAo[node.seqNum]));
              ASK EndOfRunStream TO WriteString("  "+FormatStats(NodeEndDo[node.seqNum]));
              ASK EndOfRunStream TO WriteString("  "+FormatStats(NodeEndR[node.seqNum]));
              ASK EndOfRunStream TO WriteString(SUBSTR(1,12,"    "+REALTOSTR(hMTBDE[hier.seqNum])+"  "));
              ASK EndOfRunStream TO WriteString(hMDTstring[hier.seqNum]);
              ASK EndOfRunStream TO WriteLn;  
           END FOR;
           ASK EndOfRunStream TO WriteLn; 
           ASK EndOfRunStream TO WriteString("Hierarchy                  %Green    %Yellow       %Red    %Orange      %Blue     %Brown");
           ASK EndOfRunStream TO WriteLn;
           FOR i:=1 TO totalHiers
              hier := ASK root Child("RBDHier",HierAlphaSort[i]);
              node := ASK root Child("RBDNode",hier.outID);
              ASK EndOfRunStream TO WriteString(SUBSTR(1,20,hier.name+"                    ")+"  ");
              ASK EndOfRunStream TO WriteString(REALTOSN(NodeEndGreen[node.seqNum],11,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(NodeEndYellow[node.seqNum],11,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(NodeEndRed[node.seqNum],11,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(NodeEndOrange[node.seqNum],11,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(NodeEndBlue[node.seqNum],11,6,1));
              ASK EndOfRunStream TO WriteString(REALTOSN(NodeEndBrown[node.seqNum],11,6,1));
              ASK EndOfRunStream TO WriteLn;  
           END FOR;
           ASK EndOfRunStream TO WriteLn;          
        END IF;   {(totalHiers>0)}
     END IF;   {weakAnalysis}
     ASK EndOfRunStream TO WriteLn;          
     ASK EndOfRunStream TO WriteLn; 
  END IF;  {EORFile}
  IF costAnalysis
     IF totalBlocks>0                                    
        DISPOSE(OperatingCost);
        DISPOSE(SparingCost);
     END IF;  
     DISPOSE(sic);
     DISPOSE(ic);
  END IF;
  IF totalHiers>0
     IF costAnalysis
        DISPOSE(HierCosts)
     END IF; 
     IF weakAnalysis
        DISPOSE(hMDTstring);
     END IF;
  END IF;
  IF ((totalPools>0) AND (costAnalysis))
     DISPOSE(icPool);
  END IF;
END PROCEDURE; {DoEndOfRunUpdate}     

PROCEDURE DoEndOfSimUpdate();
   VAR  
      pool                                      : SparePoolObj;
      block                                     : RBDBlockObj;
      i,j,k,tempInt,endSysFail,resultsSize      : INTEGER;
      resultLabel, mrLabel, bogusLabel          : LabelObj;
      resultsTable,bogusTable                   : TableObj;
      endSimTime,sigma                          : REAL;
      node                                      : RBDNodeObj;
      tempString,printString,fileString         : STRING;
      hier                                      : RBDHierObj;
      gtNote1,xRunsNote1,gtNote2,xRunsNote2     : BOOLEAN;
      event                                     : RBDEventObj;
      
      
      s1,s2,s3 : STRING;   {temp}
      
      
  BEGIN
     resultsSize:=7;
     IF costAnalysis
        resultsSize:=resultsSize+1;
     END IF;
     IF capacityAnalysis
        resultsSize:=resultsSize+3;
     END IF;
     IF (activePhases>0)
        resultsSize:=resultsSize+1;
     END IF;
     IF FinalArray<>NILARRAY
        DISPOSE(FinalArray);
     END IF;  
     NEW(FinalArray, 0..resultsSize, 1..6);
     k:=0;
     IF termType=1
        IF (NOT earlyAbort)
           FinalArray[k,2]:=REALTOSTR(StopCriteria);
        ELSE
           FinalArray[k,2]:=REALTOSTR(SimTime);
        END IF;
     ELSE
        IF (NOT earlyAbort)
           FinalArray[k,2]:=INTTOSTR(TRUNC(StopCriteria));   
        ELSE
           IF termType=2
              FinalArray[k,2]:=INTTOSTR(TRUNC(Term1Condition));
           ELSIF termType=3
              FinalArray[k,2]:=INTTOSTR(TRUNC(cyclesCount));
           END IF;
        END IF;
     END IF;   
     FinalArray[k,3]:="";
     FinalArray[k,4]:="";
     FinalArray[k,5]:="StDev"; 
     FinalArray[k,6]:="SEM";       
     IF termType=1   
        IF KeepRunStats
           endSimTime:=SimTime;
        ELSE 
           endSimTime:=StopCriteria;
        END IF; 
      {  FinalArray[k,1]:="test1234";  }
     ELSIF termType=2
        IF (OutOfSpares=TRUE)
           IF (SimTime>0.0)
            FinalArray[k,1]:="Unable to complete "+INTTOSTR(TRUNC(StopCriteria))+
                          " failure/repair cycles on at least one run  (out of spares)";    
           END IF;
        ELSE
        {   FinalArray[k,1]:="test 45678";   }
        END IF;
     END IF;
     IF (RMon.Count<>AvailMon.Count)
        tempString:="Reliability, Conditional Reliability based on "+INTTOSTR(RMon.Count)+" runs."
        IF (activePhases>0)
           tempString:="Dependability, "+tempString;
        END IF;
        FinalArray[k,1]:=tempString;
     END IF;
     IF costAnalysis
        INC(k);
        FinalArray[k,1]:="Total Cost";
        FinalArray[k,2]:=REALTOSN(GTotalMon.Minimum,14,2,3);
        FinalArray[k,3]:=REALTOSN(GTotalMon.Mean,14,2,3);
        FinalArray[k,4]:=REALTOSN(GTotalMon.Maximum,14,2,3);
        printString:=FinalArray[k,3];
        IF (GTotalMon.Count>1)
           sigma:=PopToSample(GTotalMon.StdDev,GTotalMon.Count);
           FinalArray[k,5] := REALTOSN(sigma,14,2,3);
           FinalArray[k,6] := REALTOSN(sigma/SQRT(FLOAT(GTotalMon.Count)),14,2,3);
        ELSE
           FinalArray[k,5]:="N/A"; 
           FinalArray[k,6]:="N/A";            
        END IF;    
     END IF;
     IF capacityAnalysis
        capacityMonsSet:=TRUE;
        NodeAveFlowMon:=GETMONITOR(NodeAveFlow[startTag],RStatObj);
        NodeAveCapMon:=GETMONITOR(NodeAveCap[startTag],RStatObj);
        UnitsProducedMon:=GETMONITOR(UnitsProduced,RStatObj);
        INC(k);
        FinalArray[k,1]:="Units Produced";
        FinalArray[k,2]:=REALTOSN(UnitsProducedMon.Minimum,14,2,3);
        FinalArray[k,3]:=REALTOSN(UnitsProducedMon.Mean,14,2,3);
        FinalArray[k,4]:=REALTOSN(UnitsProducedMon.Maximum,14,2,3);
        FinalArray[k,5]:=GetDisplayStr(UnitsProducedMon.StdDev,UnitsProducedMon.Count,14,2,3,"stdev");
        FinalArray[k,6]:=GetDisplayStr(UnitsProducedMon.StdDev,UnitsProducedMon.Count,14,2,3,"sem");
        INC(k);
        FinalArray[k,1]:="Mean Flow";
        FinalArray[k,2]:=REALTOSN(NodeAveFlowMon.Minimum,14,6,3);
        FinalArray[k,3]:=REALTOSN(NodeAveFlowMon.Mean,14,6,3);
        FinalArray[k,4]:=REALTOSN(NodeAveFlowMon.Maximum,14,6,3);
        FinalArray[k,5]:=GetDisplayStr(NodeAveFlowMon.StdDev,NodeAveFlowMon.Count,14,6,3,"stdev");
        FinalArray[k,6]:=GetDisplayStr(NodeAveFlowMon.StdDev,NodeAveFlowMon.Count,14,6,3,"sem");
        INC(k);
        FinalArray[k,1]:="Mean Capacity";
        FinalArray[k,2]:=REALTOSN(NodeAveCapMon.Minimum,14,6,3);
        FinalArray[k,3]:=REALTOSN(NodeAveCapMon.Mean,14,6,3);
        FinalArray[k,4]:=REALTOSN(NodeAveCapMon.Maximum,14,6,3);
        FinalArray[k,5]:=GetDisplayStr(NodeAveCapMon.StdDev,NodeAveCapMon.Count,14,6,3,"stdev");
        FinalArray[k,6]:=GetDisplayStr(NodeAveCapMon.StdDev,NodeAveCapMon.Count,14,6,3,"sem");
     END IF;
     INC(k);
     FinalArray[k,1]:="Availability";
     FinalArray[k,2]:=FormatStats(AvailMon.Minimum);
     FinalArray[k,3]:=FormatStats(AvailMon.Mean);
     FinalArray[k,4]:=FormatStats(AvailMon.Maximum);     
     FinalArray[k,5]:=GetDisplayStr(AvailMon.StdDev,AvailMon.Count,11,9,1,"stdev");
     FinalArray[k,6]:=GetDisplayStr(AvailMon.StdDev,AvailMon.Count,11,9,1,"sem");
     s1:=FinalArray[k,2];     
     s2:=FinalArray[k,3];     
     s3:=FinalArray[k,4];     
     INC(k);

     IF MTBDEMon.Count>0
        IF (MTBDEMon.Count=AvailMon.Count) 
           FinalArray[k,1] := "MTBDE";      
        ELSE 
           FinalArray[k,1]:="MTBDE("+INTTOSTR(MTBDEMon.Count)+"-runs)";
           xRunsNote1:=TRUE;
        END IF; 
        FinalArray[k,2]:=REALTOSN(MTBDEMon.Minimum,14,6,3); 
        IF (MTBDEMon.Count=AvailMon.Count)
           IF MTBDEsusp
              FinalArray[k,3]:=">"+REALTOSN(MTBDEMon.Mean,13,6,3);      
              FinalArray[k,4]:=">"+REALTOSN(MTBDEMon.Maximum,13,6,3);   
              tempString:=FinalArray[k,4];
              FinalArray[k,5]:="N/A";        
              FinalArray[k,6]:="N/A";
              gtNote1:=TRUE;
           ELSE
              FinalArray[k,3]:=REALTOSN(MTBDEMon.Mean,14,6,3);   
              FinalArray[k,4]:=REALTOSN(MTBDEMon.Maximum,14,6,3); 
              IF (MTBDEMon.Count>1)
                 sigma:=PopToSample(MTBDEMon.StdDev,MTBDEMon.Count);
                 FinalArray[k,5] := REALTOSN(sigma,14,6,3);
                 FinalArray[k,6] := REALTOSN(sigma/SQRT(FLOAT(MTBDEMon.Count)),14,6,3);
              ELSE
                 FinalArray[k,5]:="N/A";
                 FinalArray[k,6]:="N/A";
              END IF;               
           END IF;
        ELSE
           FinalArray[k,3]:=">"+REALTOSN(MTBDEMon.Mean,13,6,3);         
           FinalArray[k,4]:=">"+REALTOSN(MTBDEMon.Maximum,13,6,3);      
           FinalArray[k,5]:="N/A";
           FinalArray[k,6]:="N/A";
        END IF;
     ELSE
        FinalArray[k,1] := "MTBDE";      
        FinalArray[k,2]:="N/A"; 
        FinalArray[k,3]:="N/A";   
        FinalArray[k,4]:="N/A"; 
        FinalArray[k,5]:="N/A";      
        FinalArray[k,6]:="N/A";      
     END IF;
     INC(k);
     IF MDTMon.Count>0
        IF (MDTMon.Count=AvailMon.Count) 
           FinalArray[k,1] := "MDT";      
        ELSE 
           FinalArray[k,1]:="MDT("+INTTOSTR(MDTMon.Count)+"-runs)"; 
           xRunsNote1:=TRUE;
        END IF; 
        FinalArray[k,2]:=REALTOSN(MDTMon.Minimum,14,6,3); 
        FinalArray[k,3]:=REALTOSN(MDTMon.Mean,14,6,3);  
        FinalArray[k,4]:=REALTOSN(MDTMon.Maximum,14,6,3); 
        IF (MDTMon.Count>1)
           sigma:=PopToSample(MDTMon.StdDev,MDTMon.Count);
           FinalArray[k,5] := REALTOSN(sigma,14,6,3);
           FinalArray[k,6] := REALTOSN(sigma/SQRT(FLOAT(MDTMon.Count)),14,6,3);
        ELSE
           FinalArray[k,5]:="N/A";
           FinalArray[k,6]:="N/A";
        END IF;    
     ELSE
        FinalArray[k,1] := "MDT";      
        FinalArray[k,2]:="N/A"; 
        FinalArray[k,3]:="N/A";   
        FinalArray[k,4]:="N/A"; 
        FinalArray[k,5]:="N/A";      
        FinalArray[k,6]:="N/A";      
     END IF;
     INC(k);
     IF (activePhases>0)     
        FinalArray[k,1] := "Dependability";      
        FinalArray[k,2]:=FormatStats(DependMon.Minimum);
        FinalArray[k,3]:=FormatStats(DependMon.Mean);
        FinalArray[k,4]:=FormatStats(DependMon.Maximum);
        FinalArray[k,5]:=GetDisplayStr(DependMon.StdDev,DependMon.Count,11,9,1,"stdev");
        FinalArray[k,6]:=GetDisplayStr(DependMon.StdDev,DependMon.Count,11,9,1,"sem");
        INC(k);
     END IF;
     FinalArray[k,1]:="Reliability";
     FinalArray[k,2]:=FormatStats(RMon.Minimum);
     FinalArray[k,3]:=FormatStats(RMon.Mean);
     FinalArray[k,4]:=FormatStats(RMon.Maximum);
     FinalArray[k,5]:=GetDisplayStr(RMon.StdDev,RMon.Count,11,9,1,"stdev");
     FinalArray[k,6]:=GetDisplayStr(RMon.StdDev,RMon.Count,11,9,1,"sem");          
     
     INC(k);
     FinalArray[k,1]:="Conditional Reliability";
     FinalArray[k,2]:=FormatStats(RCondMon.Minimum);
     FinalArray[k,3]:=FormatStats(RCondMon.Mean);
     FinalArray[k,4]:=FormatStats(RCondMon.Maximum);
     FinalArray[k,5]:=GetDisplayStr(RCondMon.StdDev,RCondMon.Count,11,9,1,"stdev");
     FinalArray[k,6]:=GetDisplayStr(RCondMon.StdDev,RCondMon.Count,11,9,1,"sem");
     IF ((termType=1) OR (termType=3))  {system Failures}
        INC(k);
        IF StatsReset 
           FinalArray[k,1]:="Failures After Reset";
        ELSE
           FinalArray[k,1]:="System Failures";        
        END IF;
        FinalArray[k,2]:=INTTOSTR(TRUNC(Term1Mon.Minimum)); 
        FinalArray[k,3]:=REALTOSTR(Term1Mon.Mean); 
        FinalArray[k,4]:=INTTOSTR(TRUNC(Term1Mon.Maximum)); 
        FinalArray[k,5]:=GetDisplayStr(Term1Mon.StdDev,Term1Mon.Count,14,6,3,"stdev");
        FinalArray[k,6]:=GetDisplayStr(Term1Mon.StdDev,Term1Mon.Count,14,6,3,"sem");
     END IF;   
     IF ((termType=2) OR (termType=3))       {ending sim time}
        INC(k);
        IF StatsReset                  
           FinalArray[k,1]:="Active Stat Time";
        ELSE       
           FinalArray[k,1]:="Ending Sim Time";
        END IF;
        FinalArray[k,2]:=REALTOSTR(Term2Mon.Minimum); 
        FinalArray[k,3]:=REALTOSTR(Term2Mon.Mean); 
        FinalArray[k,4]:=REALTOSTR(Term2Mon.Maximum); 
        FinalArray[k,5]:=GetDisplayStr(Term2Mon.StdDev,Term2Mon.Count,14,6,3,"stdev");
        FinalArray[k,6]:=GetDisplayStr(Term2Mon.StdDev,Term2Mon.Count,14,6,3,"sem");
     END IF; 
     IF (((termType=1) OR (termType=2)) AND (activePhases>0))    {cycles completed}
        INC(k);
        IF StatsReset 
           FinalArray[k,1]:="Cycles Since Reset";
        ELSE
           FinalArray[k,1]:="Cycles Completed";        
        END IF;
        FinalArray[k,2]:=INTTOSTR(TRUNC(Term3Mon.Minimum)); 
        FinalArray[k,3]:=REALTOSTR(Term3Mon.Mean); 
        FinalArray[k,4]:=INTTOSTR(ROUND(Term3Mon.Maximum));   
        FinalArray[k,5]:=GetDisplayStr(Term3Mon.StdDev,Term3Mon.Count,14,6,3,"stdev");
        FinalArray[k,6]:=GetDisplayStr(Term3Mon.StdDev,Term3Mon.Count,14,6,3,"sem");
     END IF;
     IF StatsReset
        CASE termType
           WHEN 1:
              tempString:="Stats started at time "+REALTOSTR(StartStats);
           WHEN 2:
              tempString:="Stats started after "+INTTOSTR(TRUNC(StartStats))+" failure";
           WHEN 3:
              tempString:="Stats started after "+INTTOSTR(TRUNC(StartStats))+" cycle";
        END CASE;
        IF ( ((termType=2) OR (termType=3)) AND (StartStats>1.0))
           tempString:=tempString+"s";
        END IF;   
        IF FinalArray[0,1]=""
           FinalArray[0,1]:=tempString;
        ELSE
           FinalArray[0,1]:=tempString+"; "+FinalArray[0,1];
        END IF;
     END IF;
     IF xRunsNote1
        FinalArray[0,3]:="Note: Some parameter calculations are based on less than the specified number of trials.";
     END IF;
     IF gtNote1
        FinalArray[0,4]:="Note: Parameters marked with a greater than sign are likely to be poor estimators.";
     END IF;
     IF ResultsFile
        IF NumRunsCompleted<>1
           tempString:=" runs";
        ELSE 
           tempString:=" run";
        END IF;
        IF termType=1
           tempString:=tempString+" of sim time "+FinalArray[0,2]+":";
        ELSIF termType=2
           IF (STRTOINT(FinalArray[0,2])) <> 1
              tempString:=tempString+" to " +FinalArray[0,2]+" failures:";
           ELSE   
              tempString:=tempString+" to 1 failure:";
           END IF;
        ELSE
           IF (STRTOINT(FinalArray[0,2])) <> 1
              tempString:=tempString+" of " +FinalArray[0,2]+" cycles:";
           ELSE   
              tempString:=tempString+" of 1 cycle:";
           END IF;   
        END IF;   
        ASK ResultsStream TO WriteString("Results from " + INTTOSTR(NumRunsCompleted) + tempString);
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn;
        IF password
           ASK ResultsStream TO WriteString("Random draws: "+INTTOSTR(randNumCount));
           ASK ResultsStream TO WriteLn;
           ASK ResultsStream TO WriteLn;
        END IF;
        ASK ResultsStream TO WriteString("***SUMMARY***"); 
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn;
        tempString:=SUBSTR(1,25,"                                   ");
        tempString:=tempString+"Minimum         Mean            Maximum         St.Dev          SEM";     
        ASK ResultsStream TO WriteString(tempString); 
        ASK ResultsStream TO WriteLn; 
        FOR i:=1 TO k
           fileString:=SUBSTR(1,25,OutputToFR(FinalArray[i,1])+"                          "); 
           fileString:=fileString+SUBSTR(1,14,FinalArray[i,2]+"              ")+"  "; 
           fileString:=fileString+SUBSTR(1,14,FinalArray[i,3]+"              ")+"  "; 
           fileString:=fileString+SUBSTR(1,14,FinalArray[i,4]+"              ")+"  "; 
           fileString:=fileString+SUBSTR(1,14,FinalArray[i,5]+"              ")+"  "; 
           fileString:=fileString+SUBSTR(1,14,FinalArray[i,6]+"              ")+"  ";
           ASK ResultsStream TO WriteString(fileString); 
           ASK ResultsStream TO WriteLn;
        END FOR;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString(FinalArray[0,1]); 
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;
     END IF;
     IF LogArray<>NILARRAY
        DISPOSE(LogArray);
     END IF;    
     NEW(LogArray,0..12,1..6);  
     k:=0;
     LogArray[k,3]:="Mean";
     LogArray[k,4]:="Max";
     LogArray[k,5]:="StDev"; 
     LogArray[k,6]:="SEM";       
     INC(k);
     IF MTBMMon.Count>0
        IF (MTBMMon.Count=AvailMon.Count) 
           LogArray[k,1] := "MTBM";      
        ELSE 
           LogArray[k,1]:="MTBM("+INTTOSTR(MTBMMon.Count)+"-runs)";
           xRunsNote2:=TRUE;
        END IF; 
        LogArray[k,2]:=REALTOSN(MTBMMon.Minimum,14,6,3);        
        IF (MTBMMon.Count=AvailMon.Count)
           IF MTBMsusp
              LogArray[k,3]:=">"+REALTOSN(MTBMMon.Mean,13,6,3);       
              LogArray[k,4]:=">"+REALTOSN(MTBMMon.Maximum,13,6,3);        
              LogArray[k,5]:="N/A";        
              LogArray[k,6]:="N/A";
              gtNote2:=TRUE;
           ELSE
              LogArray[k,3]:=REALTOSN(MTBMMon.Mean,14,6,3);   
              LogArray[k,4]:=REALTOSN(MTBMMon.Maximum,14,6,3); 
              LogArray[k,5]:=GetDisplayStr(MTBMMon.StdDev,MTBMMon.Count,14,6,3,"stdev");
              LogArray[k,6]:=GetDisplayStr(MTBMMon.StdDev,MTBMMon.Count,14,6,3,"sem");
           END IF;
        ELSE
           LogArray[k,3]:=">"+REALTOSN(MTBMMon.Mean,13,6,3);         
           LogArray[k,4]:=">"+REALTOSN(MTBMMon.Maximum,13,6,3);       
           LogArray[k,5]:="N/A";
           LogArray[k,6]:="N/A";        
        END IF;
     ELSE
        LogArray[k,1] := "MTBM";      
        LogArray[k,2]:="N/A"; 
        LogArray[k,3]:="N/A";   
        LogArray[k,4]:="N/A"; 
        LogArray[k,5]:="N/A";      
        LogArray[k,6]:="N/A";        
     END IF;
     INC(k);
     IF MTBMuMon.Count>0
        IF (MTBMuMon.Count=AvailMon.Count) 
           LogArray[k,1] := "MTBMu";      
        ELSE 
           LogArray[k,1]:="MTBMu("+INTTOSTR(MTBMuMon.Count)+"-runs)";
           xRunsNote2:=TRUE;
        END IF; 
        LogArray[k,2]:=REALTOSN(MTBMuMon.Minimum,14,6,3);        
        IF (MTBMuMon.Count=AvailMon.Count)
           IF MTBMususp
              LogArray[k,3]:=">"+REALTOSN(MTBMuMon.Mean,13,6,3);       
              LogArray[k,4]:=">"+REALTOSN(MTBMuMon.Maximum,13,6,3);       
              LogArray[k,5]:="N/A";        
              LogArray[k,6]:="N/A";  
              gtNote2:=TRUE;
           ELSE
              LogArray[k,3]:=REALTOSN(MTBMuMon.Mean,14,6,3);   
              LogArray[k,4]:=REALTOSN(MTBMuMon.Maximum,14,6,3); 
              LogArray[k,5]:=GetDisplayStr(MTBMuMon.StdDev,MTBMuMon.Count,14,6,3,"stdev");
              LogArray[k,6]:=GetDisplayStr(MTBMuMon.StdDev,MTBMuMon.Count,14,6,3,"sem");
           END IF;
        ELSE
           LogArray[k,3]:=">"+REALTOSN(MTBMuMon.Mean,13,6,3);          
           LogArray[k,4]:=">"+REALTOSN(MTBMuMon.Maximum,13,6,3);        
           LogArray[k,5]:="N/A";
           LogArray[k,6]:="N/A"; 
           gtNote2:=TRUE;
        END IF;
     ELSE
        LogArray[k,1] := "MTBMu";      
        LogArray[k,2]:="N/A"; 
        LogArray[k,3]:="N/A";   
        LogArray[k,4]:="N/A"; 
        LogArray[k,5]:="N/A";      
        LogArray[k,6]:="N/A";        
     END IF;
     INC(k);
     IF didPM=TRUE              {TONY 12-04 MTBMsMon.Count>0 for Beta118}
        IF (MTBMsMon.Count=AvailMon.Count) 
           LogArray[k,1] := "MTBMs";      
        ELSE 
           LogArray[k,1]:="MTBMs("+INTTOSTR(MTBMsMon.Count)+"-runs)";
           xRunsNote2:=TRUE;
        END IF; 
        LogArray[k,2]:=REALTOSN(MTBMsMon.Minimum,14,6,3);        
        IF (MTBMsMon.Count=AvailMon.Count)
           IF MTBMssusp
              LogArray[k,3]:=">"+REALTOSN(MTBMsMon.Mean,13,6,3);     
              LogArray[k,4]:=">"+REALTOSN(MTBMsMon.Maximum,13,6,3);    
              LogArray[k,5]:="N/A";        
              LogArray[k,6]:="N/A"; 
              gtNote2:=TRUE;
           ELSE
              LogArray[k,3]:=REALTOSN(MTBMsMon.Mean,14,6,3);   
              LogArray[k,4]:=REALTOSN(MTBMsMon.Maximum,14,6,3); 
              LogArray[k,5]:=GetDisplayStr(MTBMsMon.StdDev,MTBMsMon.Count,14,6,3,"stdev");
              LogArray[k,6]:=GetDisplayStr(MTBMsMon.StdDev,MTBMsMon.Count,14,6,3,"sem");
           END IF;
        ELSE
           LogArray[k,3]:=">"+REALTOSN(MTBMsMon.Mean,13,6,3);        
           LogArray[k,4]:=">"+REALTOSN(MTBMsMon.Maximum,13,6,3);
           LogArray[k,5]:="N/A";
           LogArray[k,6]:="N/A"; 
           gtNote2:=TRUE;
        END IF;
     ELSE
        LogArray[k,1] := "MTBMs";      
        LogArray[k,2]:="N/A"; 
        LogArray[k,3]:="N/A";   
        LogArray[k,4]:="N/A"; 
        LogArray[k,5]:="N/A";      
        LogArray[k,6]:="N/A";        
     END IF;
     INC(k);
     IF MMTMon.Count>0
        IF (MMTMon.Count=AvailMon.Count) 
           LogArray[k,1] := "MMT";      
        ELSE 
           LogArray[k,1]:="MMT("+INTTOSTR(MMTMon.Count)+"-runs)";
           xRunsNote2:=TRUE;
        END IF; 
        LogArray[k,2]:=REALTOSN(MMTMon.Minimum,14,6,3); 
        LogArray[k,3]:=REALTOSN(MMTMon.Mean,14,6,3);   
        LogArray[k,4]:=REALTOSN(MMTMon.Maximum,14,6,3); 
        LogArray[k,5]:=GetDisplayStr(MMTMon.StdDev,MMTMon.Count,14,6,3,"stdev");
        LogArray[k,6]:=GetDisplayStr(MMTMon.StdDev,MMTMon.Count,14,6,3,"sem");
     ELSE
        LogArray[k,1] := "MMT";      
        LogArray[k,2] := "N/A";      
        LogArray[k,3] := "N/A";      
        LogArray[k,4] := "N/A";      
        LogArray[k,5] := "N/A";           
        LogArray[k,6] := "N/A";
     END IF;
     INC(k);     
     IF MMTuMon.Count>0
        IF (MMTuMon.Count=AvailMon.Count) 
           LogArray[k,1] := "MMTu";      
        ELSE 
           LogArray[k,1]:="MMTu("+INTTOSTR(MMTuMon.Count)+"-runs)";
           xRunsNote2:=TRUE;
        END IF; 
        LogArray[k,2]:=REALTOSN(MMTuMon.Minimum,14,6,3); 
        LogArray[k,3]:=REALTOSN(MMTuMon.Mean,14,6,3);   
        LogArray[k,4]:=REALTOSN(MMTuMon.Maximum,14,6,3); 
        LogArray[k,5]:=GetDisplayStr(MMTuMon.StdDev,MMTuMon.Count,14,6,3,"stdev");
        LogArray[k,6]:=GetDisplayStr(MMTuMon.StdDev,MMTuMon.Count,14,6,3,"sem");
     ELSE
        LogArray[k,1] := "MMTu";      
        LogArray[k,2] := "N/A";      
        LogArray[k,3] := "N/A";      
        LogArray[k,4] := "N/A";      
        LogArray[k,5] := "N/A";           
        LogArray[k,6] := "N/A";
     END IF;
     INC(k);     
     IF MMTsMon.Count>0
        IF (MMTsMon.Count=AvailMon.Count) 
           LogArray[k,1] := "MMTs";      
        ELSE 
           LogArray[k,1]:="MMTs("+INTTOSTR(MMTsMon.Count)+"-runs)";
           xRunsNote2:=TRUE;
        END IF; 
        LogArray[k,2]:=REALTOSN(MMTsMon.Minimum,14,6,3); 
        LogArray[k,3]:=REALTOSN(MMTsMon.Mean,14,6,3);   
        LogArray[k,4]:=REALTOSN(MMTsMon.Maximum,14,6,3); 
        LogArray[k,5]:=GetDisplayStr(MMTsMon.StdDev,MMTsMon.Count,14,6,3,"stdev");
        LogArray[k,6]:=GetDisplayStr(MMTsMon.StdDev,MMTsMon.Count,14,6,3,"sem");
     ELSE
        LogArray[k,1] := "MMTs";      
        LogArray[k,2] := "N/A";      
        LogArray[k,3] := "N/A";      
        LogArray[k,4] := "N/A";      
        LogArray[k,5] := "N/A";           
        LogArray[k,6] := "N/A";
     END IF;
     INC(k);     
     LogArray[k,1] := "Green Percent";
     IF GreenPercentMon.Count>0
        LogArray[k,2]:=REALTOSTR(GreenPercentMon.Minimum);
        LogArray[k,3]:=REALTOSTR(GreenPercentMon.Mean);   
        LogArray[k,4]:=REALTOSTR(GreenPercentMon.Maximum); 
        IF (GreenPercentMon.Count>1)
        LogArray[k,5]:=GetDisplayStr(GreenPercentMon.StdDev,GreenPercentMon.Count,14,6,3,"stdev");
        LogArray[k,6]:=GetDisplayStr(GreenPercentMon.StdDev,GreenPercentMon.Count,14,6,3,"sem");
        ELSE
           LogArray[k,5]:="N/A"; 
           LogArray[k,6]:="N/A"; 
        END IF;
     ELSE
        LogArray[k,2]:="N/A";
        LogArray[k,3]:="N/A";   
        LogArray[k,4]:="N/A"; 
        LogArray[k,5]:="N/A"; 
        LogArray[k,6]:="N/A"; 
     END IF;
     INC(k);
     LogArray[k,1] := "Yellow Percent";
     IF YellowPercentMon.Count>0
        LogArray[k,2]:=REALTOSTR(YellowPercentMon.Minimum);
        LogArray[k,3]:=REALTOSTR(YellowPercentMon.Mean);   
        LogArray[k,4]:=REALTOSTR(YellowPercentMon.Maximum); 
        IF (YellowPercentMon.Count>1)
        LogArray[k,5]:=GetDisplayStr(YellowPercentMon.StdDev,YellowPercentMon.Count,14,6,3,"stdev");
        LogArray[k,6]:=GetDisplayStr(YellowPercentMon.StdDev,YellowPercentMon.Count,14,6,3,"sem");
        ELSE
           LogArray[k,5]:="N/A"; 
           LogArray[k,6]:="N/A"; 
        END IF;
     ELSE
        LogArray[k,2]:="N/A";
        LogArray[k,3]:="N/A";   
        LogArray[k,4]:="N/A"; 
        LogArray[k,5]:="N/A"; 
        LogArray[k,6]:="N/A"; 
     END IF;
     INC(k);
     LogArray[k,1] := "Red Percent";
     IF RedPercentMon.Count>0
        LogArray[k,2]:=REALTOSTR(RedPercentMon.Minimum);
        LogArray[k,3]:=REALTOSTR(RedPercentMon.Mean);   
        LogArray[k,4]:=REALTOSTR(RedPercentMon.Maximum); 
        IF (RedPercentMon.Count>1)
        LogArray[k,5]:=GetDisplayStr(RedPercentMon.StdDev,RedPercentMon.Count,14,6,3,"stdev");
        LogArray[k,6]:=GetDisplayStr(RedPercentMon.StdDev,RedPercentMon.Count,14,6,3,"sem");
        ELSE
           LogArray[k,5]:="N/A"; 
           LogArray[k,6]:="N/A"; 
        END IF;
     ELSE
        LogArray[k,2]:="N/A";
        LogArray[k,3]:="N/A";   
        LogArray[k,4]:="N/A"; 
        LogArray[k,5]:="N/A"; 
        LogArray[k,6]:="N/A"; 
     END IF;
     INC(k);
     IF (activePhases>0)
        LogArray[k,1] := "Mission Green Percent";
        IF MissionGrnPerMon.Count>0
           LogArray[k,2]:=REALTOSTR(MissionGrnPerMon.Minimum);
           LogArray[k,3]:=REALTOSTR(MissionGrnPerMon.Mean);   
           LogArray[k,4]:=REALTOSTR(MissionGrnPerMon.Maximum); 
           IF (MissionGrnPerMon.Count>1)
              LogArray[k,5]:=GetDisplayStr(MissionGrnPerMon.StdDev,MissionGrnPerMon.Count,14,6,3,"stdev");
              LogArray[k,6]:=GetDisplayStr(MissionGrnPerMon.StdDev,MissionGrnPerMon.Count,14,6,3,"sem");
           ELSE
              LogArray[k,5]:="N/A"; 
              LogArray[k,6]:="N/A"; 
           END IF;
        ELSE
           LogArray[k,2]:="N/A";
           LogArray[k,3]:="N/A";   
           LogArray[k,4]:="N/A"; 
           LogArray[k,5]:="N/A"; 
           LogArray[k,6]:="N/A"; 
        END IF;
        INC(k);
        LogArray[k,1] := "Mission Yellow Percent";
        IF MissionYelPerMon.Count>0
           LogArray[k,2]:=REALTOSTR(MissionYelPerMon.Minimum);
           LogArray[k,3]:=REALTOSTR(MissionYelPerMon.Mean);   
           LogArray[k,4]:=REALTOSTR(MissionYelPerMon.Maximum); 
           IF (MissionYelPerMon.Count>1)
              LogArray[k,5]:=GetDisplayStr(MissionYelPerMon.StdDev,MissionYelPerMon.Count,14,6,3,"stdev");
              LogArray[k,6]:=GetDisplayStr(MissionYelPerMon.StdDev,MissionYelPerMon.Count,14,6,3,"sem");
           ELSE
              LogArray[k,5]:="N/A"; 
              LogArray[k,6]:="N/A"; 
           END IF;
        ELSE
           LogArray[k,2]:="N/A";
           LogArray[k,3]:="N/A";   
           LogArray[k,4]:="N/A"; 
           LogArray[k,5]:="N/A"; 
           LogArray[k,6]:="N/A"; 
        END IF;
        INC(k);
        LogArray[k,1] := "Mission Red Percent";
        IF MissionRedPerMon.Count>0
           LogArray[k,2]:=REALTOSTR(MissionRedPerMon.Minimum);
           LogArray[k,3]:=REALTOSTR(MissionRedPerMon.Mean);   
           LogArray[k,4]:=REALTOSTR(MissionRedPerMon.Maximum); 
           IF (MissionRedPerMon.Count>1)
              LogArray[k,5]:=GetDisplayStr(MissionRedPerMon.StdDev,MissionRedPerMon.Count,14,6,3,"stdev");
              LogArray[k,6]:=GetDisplayStr(MissionRedPerMon.StdDev,MissionRedPerMon.Count,14,6,3,"sem");
           ELSE
              LogArray[k,5]:="N/A"; 
              LogArray[k,6]:="N/A"; 
           END IF;
        ELSE
           LogArray[k,2]:="N/A";
           LogArray[k,3]:="N/A";   
           LogArray[k,4]:="N/A"; 
           LogArray[k,5]:="N/A"; 
           LogArray[k,6]:="N/A"; 
        END IF;
     END IF;  {activePhases>0}
     IF xRunsNote2
        LogArray[0,1]:="Note: Some parameter calculations are based on less than the specified number of trials.";
     END IF;
     IF gtNote2
        LogArray[0,2]:="Note: Parameters marked with a greater than sign are likely to be poor estimators.";
     END IF;

     IF ResultsFile
        ASK ResultsStream TO WriteString("***LOGISTICS***"); 
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn;
        printString:=SUBSTR(1,25,"                                   ");
        printString:=printString+"Minimum         Mean            Maximum         St.Dev          SEM";     
        ASK ResultsStream TO WriteString(printString); 
        ASK ResultsStream TO WriteLn; 
        FOR i:=1 TO k
           fileString:=SUBSTR(1,25,OutputToFR(LogArray[i,1])+"                          "); 
           fileString:=fileString+SUBSTR(1,14,LogArray[i,2]+"              ")+"  "; 
           fileString:=fileString+SUBSTR(1,14,LogArray[i,3]+"              ")+"  "; 
           fileString:=fileString+SUBSTR(1,14,LogArray[i,4]+"              ")+"  "; 
           fileString:=fileString+SUBSTR(1,14,LogArray[i,5]+"              ")+"  "; 
           fileString:=fileString+SUBSTR(1,14,LogArray[i,6]+"              ")+"  "; 
           ASK ResultsStream TO WriteString(fileString); 
           ASK ResultsStream TO WriteLn;
        END FOR;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;     
     END IF;
     IF Aborted
        ASK window TO ShowStatus(1,"Simulation Abridged");
     ELSE
        ASK window TO ShowStatus(1,"All runs complete");
     END IF;
     ASK window TO ShowStatus(2,"");
     ASK window TO ShowStatus(3,"");
     IF soundIsOn AND (FileExists(soundsPath + "EndSim.wav"))
        ASK sound TO PlayMe(soundsPath + "EndSim.wav");
     END IF;     
     IF ((ResultsFile) AND (totalBlocks>0))
        ASK ResultsStream TO WriteString("***SPARING***"); 
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteString("Spares_Utilization"); 
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteString("blockName             SpareSource            MinUsed      AveUsed   MaxUsed        StDev");
        ASK ResultsStream TO WriteLn;
        EndSpMonCreated:=TRUE;
     END IF;
     IF SparesArray<>NILARRAY
         DISPOSE(SparesArray);
     END IF;
     NEW(SparesArray,0..totalBlocks,1..7);
     SparesArray[0,1]:=INTTOSTR(totalBlocks);
     SparesArray[0,2]:="SpareSource";
     SparesArray[0,3]:="MinUsed";
     SparesArray[0,4]:="AveUsed";
     SparesArray[0,5]:="MaxUsed";
     SparesArray[0,6]:="StDev";
     k:=0;
     FOR j:=1 TO totalBlocks
        block := ASK window Descendant("RBDBlock", BlockAlphaSort[j]);
        EndSparesUsedMon:=GETMONITOR(EndSparesUsed[block.seqNum],IStatObj);
           INC(k);
           IF block.sparingType=Infinite
              tempString:="Infinite             ";
           ELSIF block.sparingType=Custom
              tempString:="Custom               ";
           ELSIF block.sparingType=SparePool
              tempString:= SUBSTR(1,20,block.poolName + "                    ")+" ";
           ELSIF block.sparingType=None
              tempString:="None                 ";
           END IF;
           SparesArray[k,1]:=SUBSTR(1,20,block.name + "                    ");
           SparesArray[k,2]:=tempString;
           SparesArray[k,3]:=INTTOSTR(EndSparesUsedMon.Minimum);
           SparesArray[k,4]:=REALTOSTR(EndSparesUsedMon.Mean);
           SparesArray[k,5]:=INTTOSTR(EndSparesUsedMon.Maximum);
           IF EndSparesUsedMon.Count>1
              sigma:=PopToSample(EndSparesUsedMon.StdDev,EndSparesUsedMon.Count);
              SparesArray[k,6]:=REALTOSTR(sigma);
           ELSE
              SparesArray[k,6]:="N/A";
           END IF;
           SparesArray[k,7]:=INTTOSTR(block.parentID);
           IF ResultsFile
              ASK ResultsStream TO WriteString(SUBSTR(1,20,block.name + "                    ")+"  "); 
              ASK ResultsStream TO WriteString(tempString); 
              ASK ResultsStream TO WriteInt(EndSparesUsedMon.Minimum,9); 
              ASK ResultsStream TO WriteString(" ");
              ASK ResultsStream TO WriteString(REALTOSN(EndSparesUsedMon.Mean,12,2,1));
              ASK ResultsStream TO WriteString(" ");
              ASK ResultsStream TO WriteInt(EndSparesUsedMon.Maximum,9); 
              ASK ResultsStream TO WriteString(" ");
              IF EndSparesUsedMon.Count>1
                 sigma:=PopToSample(EndSparesUsedMon.StdDev,EndSparesUsedMon.Count);
                 ASK ResultsStream TO WriteString(REALTOSN(sigma,12,2,1));
              ELSE
                 ASK ResultsStream TO WriteString("         N/A");              
              END IF;
              ASK ResultsStream TO WriteString(" ");
              ASK ResultsStream TO WriteLn;
           END IF;
     END FOR; 
     IF ResultsFile
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;
     END IF;
     IF (SparesTrackingGroup.numberIn>0)
        IF ResultsFile
           ASK ResultsStream TO WriteString("Custom_Sparing"); 
           ASK ResultsStream TO WriteLn;  
           ASK ResultsStream TO WriteLn;                            
           ASK ResultsStream TO WriteString("blockName            InitNumSpares    NewSpares     MinStock     AveStock     MaxStock     EndStock    NumDelays");
           ASK ResultsStream TO WriteLn; 
        END IF;
        FOR j:=1 TO totalBlocks
           block := ASK root Child("RBDBlock", BlockAlphaSort[j]);
           IF SparesTrackingGroup.Includes(block)
              i:=block.seqNum;
              InitNumSparesMon:=GETMONITOR(InitNumSpares[i],IStatObj);
              AveNumSparesMon:=GETMONITOR(AveNumSpares[i],RStatObj);        
              MinNumSparesMon:=GETMONITOR(MinNumSpares[i],IStatObj);
              MaxNumSparesMon:=GETMONITOR(MaxNumSpares[i],IStatObj);
              NumOfWaitsMon:=GETMONITOR(NumOfWaits[i],IStatObj);
              EndingSparesMon:=GETMONITOR(EndingSpares[i],IStatObj);  
              NewSparesMon:=GETMONITOR(NewSpares[i],IStatObj); 
              IF ResultsFile
                 ASK ResultsStream TO WriteString(SUBSTR(1,20,block.name + "                    ")+"  "); 
                 ASK ResultsStream TO WriteString(REALTOSN(InitNumSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(NewSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(MinNumSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(AveNumSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(MaxNumSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(EndingSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(NumOfWaitsMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteLn; 
              END IF;
           END IF;
        END FOR;
     END IF;
     IF ResultsFile
        ASK ResultsStream TO WriteLn;
     END IF;
     IF totalSpares>0
        IF ResultsFile
           ASK ResultsStream TO WriteLn;    
           ASK ResultsStream TO WriteString("SparePool_Sparing_Information"); 
           ASK ResultsStream TO WriteLn; 
           ASK ResultsStream TO WriteLn; 
           ASK ResultsStream TO WriteString("poolName             InitNumSpares    NewSpares     MinStock     AveStock     MaxStock     EndStock    NumDelays");
          ASK ResultsStream TO WriteLn;
        END IF;
        IF SpPoolsArray<>NILARRAY
            DISPOSE(SpPoolsArray);
        END IF;
        NEW(SpPoolsArray,0..totalSpares,1..8);
        SpPoolsArray[0,1]:="poolName";
        SpPoolsArray[0,2]:="InitSpares";
        SpPoolsArray[0,3]:="NewSpares";
        SpPoolsArray[0,4]:="MinStock";
        SpPoolsArray[0,5]:="AveStock";
        SpPoolsArray[0,6]:="MaxStock";
        SpPoolsArray[0,7]:="EndStock";
        SpPoolsArray[0,8]:="NumDelays";
        k:=0;
        FOR i:= 1 TO totalPools
           j:=i+totalBlocks;
           IF (PoolArray[i].sparingType=SparePool)
              INC(k);
              InitNumSparesMon:=GETMONITOR(InitNumSpares[j],IStatObj);
              AveNumSparesMon:=GETMONITOR(AveNumSpares[j],RStatObj);        
              MinNumSparesMon:=GETMONITOR(MinNumSpares[j],IStatObj);
              MaxNumSparesMon:=GETMONITOR(MaxNumSpares[j],IStatObj);
              NumOfWaitsMon:=GETMONITOR(NumOfWaits[j],IStatObj);
              EndingSparesMon:=GETMONITOR(EndingSpares[j],IStatObj); 
              NewSparesMon:=GETMONITOR(NewSpares[j],IStatObj);
              SpPoolsArray[k,1]:=SUBSTR(1,20,PoolArray[i].poolName + "                    ");
              SpPoolsArray[k,2]:=REALTOSTR(InitNumSparesMon.Mean);
              SpPoolsArray[k,3]:=REALTOSTR(NewSparesMon.Mean);
              SpPoolsArray[k,4]:=REALTOSTR(MinNumSparesMon.Mean);
              SpPoolsArray[k,5]:=REALTOSTR(AveNumSparesMon.Mean);
              SpPoolsArray[k,6]:=REALTOSTR(MaxNumSparesMon.Mean);
              SpPoolsArray[k,7]:=REALTOSTR(EndingSparesMon.Mean);
              SpPoolsArray[k,8]:=REALTOSTR(NumOfWaitsMon.Mean);
              IF ResultsFile
                 ASK ResultsStream TO WriteString(SUBSTR(1,20,PoolArray[i].poolName + "                    ")+"  "); 
                 ASK ResultsStream TO WriteString(REALTOSN(InitNumSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(NewSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(MinNumSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(AveNumSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(MaxNumSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(EndingSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(NumOfWaitsMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteLn;
              END IF;
           END IF;
        END FOR;
        IF ResultsFile
           ASK ResultsStream TO WriteLn; 
           ASK ResultsStream TO WriteLn; 
           ASK ResultsStream TO WriteLn;
        END IF;
     END IF; 
   IF totalRes>0
      IF ResultsFile
         ASK ResultsStream TO WriteString("***RESOURCES***"); 
         ASK ResultsStream TO WriteLn; 
         ASK ResultsStream TO WriteLn; 
         ASK ResultsStream TO WriteString("Resource_Information"); 
         ASK ResultsStream TO WriteLn; 
         ASK ResultsStream TO WriteLn; 
         ASK ResultsStream TO WriteString("resourceName            InitNumRes    MinNumRes    AveNumRes    MaxNumRes    EndNumRes    NumDelays");
         ASK ResultsStream TO WriteLn; 
      END IF;
        IF ResourcesArray<>NILARRAY
            DISPOSE(ResourcesArray);
        END IF;
        NEW(ResourcesArray,0..totalRes,1..7);
        ResourcesArray[0,1]:="resourceName";
        ResourcesArray[0,2]:="InitNumRes";
        ResourcesArray[0,3]:="MinNumRes";
        ResourcesArray[0,4]:="AveNumRes";
        ResourcesArray[0,5]:="MaxNumRes";
        ResourcesArray[0,6]:="EndNumRes";
        ResourcesArray[0,7]:="NumDelays";
        k:=0;
        FOR i:= 1 TO totalPools        
           j:=i+totalBlocks;
           IF (PoolArray[i].sparingType=Resource)
              INC(k);
              InitNumSparesMon:=GETMONITOR(InitNumSpares[j],IStatObj);
              AveNumSparesMon:=GETMONITOR(AveNumSpares[j],RStatObj);        
              MinNumSparesMon:=GETMONITOR(MinNumSpares[j],IStatObj);
              MaxNumSparesMon:=GETMONITOR(MaxNumSpares[j],IStatObj);
              NumOfWaitsMon:=GETMONITOR(NumOfWaits[j],IStatObj);
              EndingSparesMon:=GETMONITOR(EndingSpares[j],IStatObj); 
              ResourcesArray[k,1]:=SUBSTR(1,20,PoolArray[i].poolName + "                    ");
              ResourcesArray[k,2]:=REALTOSTR(InitNumSparesMon.Mean);
              ResourcesArray[k,3]:=REALTOSTR(MinNumSparesMon.Mean);
              ResourcesArray[k,4]:=REALTOSTR(AveNumSparesMon.Mean);
              ResourcesArray[k,5]:=REALTOSTR(MaxNumSparesMon.Mean);
              ResourcesArray[k,6]:=REALTOSTR(EndingSparesMon.Mean);
              ResourcesArray[k,7]:=REALTOSTR(NumOfWaitsMon.Mean);
              IF ResultsFile
                 ASK ResultsStream TO WriteString(SUBSTR(1,20,PoolArray[i].poolName + "                    ")+"  "); 
                 ASK ResultsStream TO WriteString(REALTOSN(InitNumSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(MinNumSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(AveNumSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(MaxNumSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(EndingSparesMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(NumOfWaitsMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteLn;
              END IF;
           END IF;
        END FOR;
        IF ResultsFile
           ASK ResultsStream TO WriteLn; 
           ASK ResultsStream TO WriteLn;
        END IF;
     END IF; 
  IF capacityAnalysis
     capacityMonsSet:=TRUE;
     IF ResultsFile
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteString("***CAPACITY***"); 
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn;
     END IF;
     NodeAveFlowMon:=GETMONITOR(NodeAveFlow[startTag],RStatObj);
     NodeMinFlowMon:=GETMONITOR(NodeMinFlow[startTag],IStatObj);
     NodeMaxFlowMon:=GETMONITOR(NodeMaxFlow[startTag],IStatObj);
     NodeUsageMon:=GETMONITOR(NodeUsage[startTag],RStatObj);
     NodeAveCapMon:=GETMONITOR(NodeAveCap[startTag],RStatObj);
     NodeMinCapMon:=GETMONITOR(NodeMinCap[startTag],IStatObj);
     NodeMaxCapMon:=GETMONITOR(NodeMaxCap[startTag],IStatObj);
     NodeCapabilityMon:=GETMONITOR(NodeCapability[startTag],RStatObj);
     UnitsProducedMon:=GETMONITOR(UnitsProduced,RStatObj);
     IF ResultsFile        
        ASK ResultsStream TO WriteString("FlowGenerated           ");
        ASK ResultsStream TO WriteInt(flowGenerated, 9);
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("UnitsProduced        ");
        ASK ResultsStream TO WriteString(REALTOSN(UnitsProducedMon.Mean,12,2,1));
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("FlowLoss              MinLostFlow  AveLostFlow  MaxLostFlow");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("                     ");
        ASK ResultsStream TO WriteReal(FLOAT(flowGenerated)-NodeMaxFlowMon.Mean, 12,2);
        ASK ResultsStream TO WriteString(" ");
        ASK ResultsStream TO WriteReal(FLOAT(flowGenerated)-NodeAveFlowMon.Mean, 12,2);
        ASK ResultsStream TO WriteString(" ");
        ASK ResultsStream TO WriteReal(FLOAT(flowGenerated)-NodeMinFlowMon.Mean, 12,2);
        ASK ResultsStream TO WriteLn;  
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("                          MinFlow      AveFlow      MaxFlow    UsageRate    MinCapacity  AveCapacity  MaxCapacity  CapableRate");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString(SUBSTR(1,20,"SystemCapacity" + "                    ")+" ");
        ASK ResultsStream TO WriteString(REALTOSN(NodeMinFlowMon.Mean,12,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(NodeAveFlowMon.Mean,12,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(NodeMaxFlowMon.Mean,12,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(NodeUsageMon.Mean,12,2,1)+("   "));
        ASK ResultsStream TO WriteString(REALTOSN(NodeMinCapMon.Mean,12,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(NodeAveCapMon.Mean,12,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(NodeMaxCapMon.Mean,12,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(NodeCapabilityMon.Mean,12,2,1)+("   "));
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("NodeCapacity");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("                          MinFlow      AveFlow      MaxFlow    UsageRate    MinCapacity  AveCapacity  MaxCapacity  CapableRate");
        ASK ResultsStream TO WriteLn;
     END IF;
     IF CapacityArray<>NILARRAY
        DISPOSE(CapacityArray);
     END IF;
     NEW(CapacityArray,0..totalCapNodes,1..11);
     CapacityArray[0,1]:="nodeName";
     CapacityArray[0,2]:="MinFlow";
     CapacityArray[0,3]:="AveFlow";
     CapacityArray[0,4]:="MaxFlow";
     CapacityArray[0,5]:="UsageRate";
     CapacityArray[0,6]:="MinCapacity";
     CapacityArray[0,7]:="AveCapacity";
     CapacityArray[0,8]:="MaxCapacity";
     CapacityArray[0,9]:="CapableRate";
     CapacityArray[1,1]:="*System*";
     CapacityArray[1,2]:=REALTOSN(NodeMinFlowMon.Mean,9,2,1);
     CapacityArray[1,3]:=REALTOSN(NodeAveFlowMon.Mean,9,2,1);
     CapacityArray[1,4]:=REALTOSN(NodeMaxFlowMon.Mean,9,2,1);
     CapacityArray[1,5]:=REALTOSN(NodeUsageMon.Mean,9,3,1);
     CapacityArray[1,6]:=REALTOSN(NodeMinCapMon.Mean,9,2,1);
     CapacityArray[1,7]:=REALTOSN(NodeAveCapMon.Mean,9,2,1);
     CapacityArray[1,8]:=REALTOSN(NodeMaxCapMon.Mean,9,2,1);
     CapacityArray[1,9]:=REALTOSN(NodeCapabilityMon.Mean,9,3,1);
     CapacityArray[1,10]:=INTTOSTR(startTag); 
     CapacityArray[1,11]:=INTTOSTR(0);
     k:=1;
     FOR i:=1 TO totalNodes
        node := ASK root Child("RBDNode", NodeAlphaSort[i]);
        IF node.capNode
           IF (node.typeNode<>1)
              INC(k);
              NodeAveFlowMon:=GETMONITOR(NodeAveFlow[node.seqNum],RStatObj);
              NodeMinFlowMon:=GETMONITOR(NodeMinFlow[node.seqNum],IStatObj);
              NodeMaxFlowMon:=GETMONITOR(NodeMaxFlow[node.seqNum],IStatObj);
              NodeUsageMon:=GETMONITOR(NodeUsage[node.seqNum],RStatObj);
              NodeAveCapMon:=GETMONITOR(NodeAveCap[node.seqNum],RStatObj);
              NodeMinCapMon:=GETMONITOR(NodeMinCap[node.seqNum],IStatObj);
              NodeMaxCapMon:=GETMONITOR(NodeMaxCap[node.seqNum],IStatObj);
              NodeCapabilityMon:=GETMONITOR(NodeCapability[node.seqNum],RStatObj);
              CapacityArray[k,1]:=SUBSTR(1,20,node.name + "                    ");
              CapacityArray[k,2]:=REALTOSN(NodeMinFlowMon.Mean,9,2,1);
              CapacityArray[k,3]:=REALTOSN(NodeAveFlowMon.Mean,9,2,1);
              CapacityArray[k,4]:=REALTOSN(NodeMaxFlowMon.Mean,9,2,1);
              IF node.fullFlow
                 CapacityArray[k,5]:="      N/A";               
              ELSE
                 CapacityArray[k,5]:=REALTOSN(NodeUsageMon.Mean,9,3,1);
              END IF;
              IF node.fullFlow
                 CapacityArray[k,6]:="      N/A";               
              ELSE
                 CapacityArray[k,6]:=REALTOSN(NodeMinCapMon.Mean,9,2,1);
              END IF;
              IF node.fullFlow
                 CapacityArray[k,7]:="      N/A";               
              ELSE
                 CapacityArray[k,7]:=REALTOSN(NodeAveCapMon.Mean,9,2,1);
              END IF;
              IF node.fullFlow
                 CapacityArray[k,8]:="      N/A";               
              ELSE
                 CapacityArray[k,8]:=REALTOSN(NodeMaxCapMon.Mean,9,2,1);
              END IF;
              IF node.fullFlow
                 CapacityArray[k,9]:="      N/A";               
              ELSE
                 CapacityArray[k,9]:=REALTOSN(NodeCapabilityMon.Mean,9,3,1);
              END IF;
              CapacityArray[k,10]:=INTTOSTR(node.Id);
              CapacityArray[k,11]:=INTTOSTR(node.parentID);
              IF (ResultsFile AND (node.capNode))
                 ASK ResultsStream TO WriteString(SUBSTR(1,20,node.name + "                    ")+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(NodeMinFlowMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(NodeAveFlowMon.Mean,12,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(NodeMaxFlowMon.Mean,12,2,1)+" ");
                 IF node.fullFlow
                    ASK ResultsStream TO WriteString("         N/A"+"   ");               
                 ELSE
                    ASK ResultsStream TO WriteString(REALTOSN(NodeUsageMon.Mean,12,2,1)+"   ");
                 END IF;
                 IF node.fullFlow
                    ASK ResultsStream TO WriteString("         N/A"+" ");               
                 ELSE
                    ASK ResultsStream TO WriteString(REALTOSN(NodeMinCapMon.Mean,12,2,1)+" ");
                 END IF;
                 IF node.fullFlow
                    ASK ResultsStream TO WriteString("         N/A"+" ");               
                 ELSE
                    ASK ResultsStream TO WriteString(REALTOSN(NodeAveCapMon.Mean,12,2,1)+" ");
                 END IF;
                 IF node.fullFlow
                    ASK ResultsStream TO WriteString("         N/A"+" ");               
                 ELSE
                    ASK ResultsStream TO WriteString(REALTOSN(NodeMaxCapMon.Mean,12,2,1)+" ");
                 END IF;
                 IF node.fullFlow
                    ASK ResultsStream TO WriteString("         N/A"+"   ");               
                 ELSE
                    ASK ResultsStream TO WriteString(REALTOSN(NodeCapabilityMon.Mean,12,2,1)+"   ");
                 END IF;
                 ASK ResultsStream TO WriteLn;
              END IF;    {ResultsFile}
           END IF;   {node.typeNode<>1}
        END IF;  {node.capNode}
     END FOR;
     IF ResultsFile
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;     
     END IF;
  END IF;  {capacityAnalysis}        
  IF costAnalysis
     ProduceCostResults;
  END IF;
  IF weakAnalysis
     weakAnalysisMonsSet:=TRUE;
     IF ResultsFile
        ASK ResultsStream TO WriteString("***WEAK_LINK_ANALYSIS***");
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn; 
        IF (totalNodes>(2+2*totalHiers))
           ASK ResultsStream TO WriteString("NODE_ANALYSIS"); 
           ASK ResultsStream TO WriteLn; 
           ASK ResultsStream TO WriteLn; 
        END IF;   
     END IF;
     IF NodeArray<>NILARRAY
        DISPOSE(NodeArray);
     END IF;
     IF (totalNodes>(2+2*totalHiers))
        i:=0;
        FOREACH node IN nodeGroup     {need to do this to get the correct size for the NodeArray}
           IF ((node.typeNode=2) AND (node.reportNodeAnal))
              INC(i);
           END IF;
        END FOREACH;
        IF i>0
           NEW(NodeArray,0..i,1..19);
           NodeArray[0,1]:="Node";
           NodeArray[0,2]:="Color";
           NodeArray[0,3]:="Ao Min";
           NodeArray[0,4]:="Ao Mean";
           NodeArray[0,5]:="Ao Max";
           NodeArray[0,6]:="Ao Stdev";
           NodeArray[0,7]:="Ao SEM";
           NodeArray[0,8]:="Do Min";
           NodeArray[0,9]:="Do Mean";
           NodeArray[0,10]:="Do Max";
           NodeArray[0,11]:="Do Stdev";
           NodeArray[0,12]:="Do SEM";
           NodeArray[0,13]:="R Min";
           NodeArray[0,14]:="R Mean";
           NodeArray[0,15]:="R Max";
           NodeArray[0,16]:="R Stdev";
           NodeArray[0,17]:="R SEM";
           NodeArray[0,18]:="NodeId";
        END IF;   
        k:=0;
        FOR i:=1 TO totalNodes
           node := ASK root Child("RBDNode",NodeAlphaSort[i]);
           IF (node.typeNode<>1)
              j:=node.seqNum;
              AoMon:=GETMONITOR(NodeEndAo[j],RStatObj);
              DoMon:=GETMONITOR(NodeEndDo[j],RStatObj);
              CompRMon:=GETMONITOR(NodeEndR[j],RStatObj);
              ASK node TO SetAoDoR(REALTOSigFigSTR(AoMon.Mean,9),REALTOSigFigSTR(DoMon.Mean,9),REALTOSigFigSTR(CompRMon.Mean,9));
              IF ((node.typeNode=2) AND (node.reportNodeAnal))
                 INC(k);
                 NodeArray[k,1]:=SUBSTR(1,20,node.name + "                    ");
                 NodeArray[k,2]:="Green";
                 NodeArray[k,3]:=REALTOSN(AoMon.Minimum,11,9,1); 
                 NodeArray[k,4]:=REALTOSN(AoMon.Mean,11,9,1);
                 NodeArray[k,5]:=REALTOSN(AoMon.Maximum,11,9,1);
                 NodeArray[k,6]:=GetDisplayStr(AoMon.StdDev,AoMon.Count,11,9,1,"stdev"); 
                 NodeArray[k,7]:=GetDisplayStr(AoMon.StdDev,AoMon.Count,11,9,1,"sem");
                 NodeArray[k,8]:=REALTOSN(DoMon.Minimum,11,9,1); 
                 NodeArray[k,9]:=REALTOSN(DoMon.Mean,11,9,1);
                 NodeArray[k,10]:=REALTOSN(DoMon.Maximum,11,9,1);
                 NodeArray[k,11]:=GetDisplayStr(DoMon.StdDev,DoMon.Count,11,9,1,"stdev"); 
                 NodeArray[k,12]:=GetDisplayStr(DoMon.StdDev,DoMon.Count,11,9,1,"sem");
                 NodeArray[k,13]:=REALTOSN(CompRMon.Minimum,11,9,1); 
                 NodeArray[k,14]:=REALTOSN(CompRMon.Mean,11,9,1);
                 NodeArray[k,15]:=REALTOSN(CompRMon.Maximum,11,9,1);
                 NodeArray[k,16]:=GetDisplayStr(CompRMon.StdDev,CompRMon.Count,11,9,1,"stdev"); 
                 NodeArray[k,17]:=GetDisplayStr(CompRMon.StdDev,CompRMon.Count,11,9,1,"sem");
                 NodeArray[k,18]:=INTTOSTR(node.Id);
                 NodeArray[k,19]:=INTTOSTR(node.parentID);
              END IF;
           END IF;   {node.typeNode<>1}      
        END FOR;  
     END IF;    {totalNodes>2}
     IF ((ResultsFile) AND (totalNodes>(2+2*totalHiers)))         
        ASK ResultsStream TO WriteString("Availability_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Node                   AoMin         AoMean        AoMax         AoStdDev      AoSEM");
        ASK ResultsStream TO WriteLn;
        k:=0;
        FOR i:=1 TO totalNodes
           node := ASK root Child("RBDNode",NodeAlphaSort[i]);
           IF ((node.typeNode=2) AND (node.reportNodeAnal))
              INC(k);
              ASK ResultsStream TO WriteString(SUBSTR(1,20,node.name+"                    "));
              ASK ResultsStream TO WriteString("   "+NodeArray[k,3]);
              ASK ResultsStream TO WriteString("   "+NodeArray[k,4]);
              ASK ResultsStream TO WriteString("   "+NodeArray[k,5]);
              ASK ResultsStream TO WriteString(SUBSTR(1,14,"   "+NodeArray[k,6]+"          "));
              ASK ResultsStream TO WriteString("   "+NodeArray[k,7]);
              ASK ResultsStream TO WriteLn;
           END IF;   {(node.typeNode<>3) AND (node.reportNodeAnal)}
        END FOR;         
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Dependability_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Node                   DoMin         DoMean        DoMax         DoStdDev      DoSEM");
        ASK ResultsStream TO WriteLn;
        k:=0;         
        FOR i:=1 TO totalNodes
           node := ASK root Child("RBDNode",NodeAlphaSort[i]);
           IF ((node.typeNode=2) AND (node.reportNodeAnal))
              INC(k);
              ASK ResultsStream TO WriteString(SUBSTR(1,20,node.name+"                    "));
              ASK ResultsStream TO WriteString("   "+NodeArray[k,8]);
              ASK ResultsStream TO WriteString("   "+NodeArray[k,9]);
              ASK ResultsStream TO WriteString("   "+NodeArray[k,10]);
              ASK ResultsStream TO WriteString(SUBSTR(1,14,"   "+NodeArray[k,11]+"          "));
              ASK ResultsStream TO WriteString("   "+NodeArray[k,12]);
              ASK ResultsStream TO WriteLn;
           END IF;   {(node.typeNode<>3) AND (node.reportNodeAnal)}
        END FOR; 
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Reliability_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Node                   RMin          RMean         RMax          RStdDev       RSEM");
        ASK ResultsStream TO WriteLn;
        k:=0;            
        FOR i:=1 TO totalNodes
           node := ASK root Child("RBDNode",NodeAlphaSort[i]);
           IF ((node.typeNode=2) AND (node.reportNodeAnal))
              INC(k);
              ASK ResultsStream TO WriteString(SUBSTR(1,20,node.name+"                    "));
              ASK ResultsStream TO WriteString("   "+NodeArray[k,13]);
              ASK ResultsStream TO WriteString("   "+NodeArray[k,14]);
              ASK ResultsStream TO WriteString("   "+NodeArray[k,15]);
              ASK ResultsStream TO WriteString(SUBSTR(1,14,"   "+NodeArray[k,16]+"          "));
              ASK ResultsStream TO WriteString("   "+NodeArray[k,17]);
              ASK ResultsStream TO WriteLn;
           END IF;   {(node.typeNode<>3) AND (node.reportNodeAnal)}
        END FOR;     
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("State_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Node                      %Green     %Yellow        %Red       %Blue      %Brown     %Orange");    
        ASK ResultsStream TO WriteLn;
        k:=0;           
        FOR i:=1 TO totalNodes
           node := ASK root Child("RBDNode",NodeAlphaSort[i]);
           IF ((node.typeNode=2) AND (node.reportNodeAnal))
              INC(k);
              NodeGreenMon:=GETMONITOR(NodeEndGreen[node.seqNum],RStatObj);
              NodeYellowMon:=GETMONITOR(NodeEndYellow[node.seqNum],RStatObj);
              NodeRedMon:=GETMONITOR(NodeEndRed[node.seqNum],RStatObj);
              NodeBlueMon:=GETMONITOR(NodeEndBlue[node.seqNum],RStatObj);
              NodeBrownMon:=GETMONITOR(NodeEndBrown[node.seqNum],RStatObj);
              NodeOrangeMon:=GETMONITOR(NodeEndOrange[node.seqNum],RStatObj);
              ASK ResultsStream TO WriteString(SUBSTR(1,20,node.name+"                    "));
              ASK ResultsStream TO WriteString(REALTOSN(NodeGreenMon.Mean,12,6,1));
              ASK ResultsStream TO WriteString(REALTOSN(NodeYellowMon.Mean,12,6,1));
              ASK ResultsStream TO WriteString(REALTOSN(NodeRedMon.Mean,12,6,1));
              ASK ResultsStream TO WriteString(REALTOSN(NodeBlueMon.Mean,12,6,1));
              ASK ResultsStream TO WriteString(REALTOSN(NodeBrownMon.Mean,12,6,1));
              ASK ResultsStream TO WriteString(REALTOSN(NodeOrangeMon.Mean,12,6,1));
              ASK ResultsStream TO WriteLn;
           END IF;   {(node.typeNode<>3) AND (node.reportNodeAnal)}
        END FOR;
        IF k>0
           ASK NodeGreenMon TO Reset;      {tony 2-12}
           ASK NodeYellowMon TO Reset;
           ASK NodeRedMon TO Reset;
           ASK NodeBlueMon TO Reset;
           ASK NodeBrownMon TO Reset;
           ASK NodeOrangeMon TO Reset;
           ASK ResultsStream TO WriteLn;
        END IF;
     END IF;   {resultsFile} 
     IF ((totalNodes>(2+2*totalHiers)) AND (NodeArray<>NILARRAY))
        UpdateNodeArray;
     END IF;   

     IF ((ResultsFile) AND (totalBlocks>0))
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteString("BLOCK_ANALYSIS"); 
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn; 
     END IF;
     IF BlockArray<>NILARRAY
        DISPOSE(BlockArray);
     END IF;
     IF (totalBlocks>0)
        NEW(BlockArray,0..totalBlocks,1..19);
        BlockArray[0,1]:="Block";
        BlockArray[0,2]:="Color";
        BlockArray[0,3]:="Ao Min";
        BlockArray[0,4]:="Ao Mean";
        BlockArray[0,5]:="Ao Max";
        BlockArray[0,6]:="Ao Stdev";
        BlockArray[0,7]:="Ao SEM";
        BlockArray[0,8]:="Do Min";
        BlockArray[0,9]:="Do Mean";
        BlockArray[0,10]:="Do Max";
        BlockArray[0,11]:="Do Stdev";
        BlockArray[0,12]:="Do SEM";
        BlockArray[0,13]:="R Min";
        BlockArray[0,14]:="R Mean";
        BlockArray[0,15]:="R Max";
        BlockArray[0,16]:="R Stdev";
        BlockArray[0,17]:="R SEM";
        BlockArray[0,18]:="BlockId";
        k:=0;
        FOR i:=1 TO totalBlocks
           block := ASK root Child("RBDBlock",BlockAlphaSort[i]);
           j:=block.seqNum;
           AoMon:=GETMONITOR(BlockEndAo[j],RStatObj);
           DoMon:=GETMONITOR(BlockEndDo[j],RStatObj);
           CompRMon:=GETMONITOR(BlockEndR[j],RStatObj);
           ASK block TO SetAoDoR(REALTOSigFigSTR(AoMon.Mean,9),REALTOSigFigSTR(DoMon.Mean,9),REALTOSigFigSTR(CompRMon.Mean,9));    
           INC(k);
           BlockArray[k,1]:=SUBSTR(1,20,block.name + "                    ");
           BlockArray[k,2]:="Green";
           BlockArray[k,3]:=REALTOSN(AoMon.Minimum,11,9,1); 
           BlockArray[k,4]:=REALTOSN(AoMon.Mean,11,9,1);
           BlockArray[k,5]:=REALTOSN(AoMon.Maximum,11,9,1);
           BlockArray[k,6]:=GetDisplayStr(AoMon.StdDev,AoMon.Count,11,9,1,"stdev"); 
           BlockArray[k,7]:=GetDisplayStr(AoMon.StdDev,AoMon.Count,11,9,1,"sem");
           BlockArray[k,8]:=REALTOSN(DoMon.Minimum,11,9,1); 
           BlockArray[k,9]:=REALTOSN(DoMon.Mean,11,9,1);
           BlockArray[k,10]:=REALTOSN(DoMon.Maximum,11,9,1);
           BlockArray[k,11]:=GetDisplayStr(DoMon.StdDev,DoMon.Count,11,9,1,"stdev"); 
           BlockArray[k,12]:=GetDisplayStr(DoMon.StdDev,DoMon.Count,11,9,1,"sem");
           BlockArray[k,13]:=REALTOSN(CompRMon.Minimum,11,9,1); 
           BlockArray[k,14]:=REALTOSN(CompRMon.Mean,11,9,1);
           BlockArray[k,15]:=REALTOSN(CompRMon.Maximum,11,9,1);
           BlockArray[k,16]:=GetDisplayStr(CompRMon.StdDev,CompRMon.Count,11,9,1,"stdev"); 
           BlockArray[k,17]:=GetDisplayStr(CompRMon.StdDev,CompRMon.Count,11,9,1,"sem");
           BlockArray[k,18]:=INTTOSTR(block.Id);
           BlockArray[k,19]:=INTTOSTR(block.parentID);
        END FOR;
     END IF;  {totalBlocks>0}   
     IF ((ResultsFile) AND (totalBlocks>0))
        ASK ResultsStream TO WriteString("Availability_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Block                  AoMin         AoMean        AoMax         AoStdDev      AoSEM");
        ASK ResultsStream TO WriteLn;
        k:=0;         
        FOR i:=1 TO totalBlocks
           block := ASK root Child("RBDBlock",BlockAlphaSort[i]);
           INC(k);
           ASK ResultsStream TO WriteString(SUBSTR(1,20,block.name+"                    "));
           ASK ResultsStream TO WriteString("   "+BlockArray[k,3]);
           ASK ResultsStream TO WriteString("   "+BlockArray[k,4]);
           ASK ResultsStream TO WriteString("   "+BlockArray[k,5]);
           ASK ResultsStream TO WriteString(SUBSTR(1,14,"   "+BlockArray[k,6]+"          "));
           ASK ResultsStream TO WriteString("   "+BlockArray[k,7]);
           ASK ResultsStream TO WriteLn;
        END FOR;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Dependability_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Block                  DoMin         DoMean        DoMax         DoStdDev      DoSEM");
        ASK ResultsStream TO WriteLn;
        k:=0;         
        FOR i:=1 TO totalBlocks
           block := ASK root Child("RBDBlock",BlockAlphaSort[i]);
           INC(k);
           ASK ResultsStream TO WriteString(SUBSTR(1,20,block.name+"                    "));
           ASK ResultsStream TO WriteString("   "+BlockArray[k,8]);
           ASK ResultsStream TO WriteString("   "+BlockArray[k,9]);
           ASK ResultsStream TO WriteString("   "+BlockArray[k,10]);
           ASK ResultsStream TO WriteString(SUBSTR(1,14,"   "+BlockArray[k,11]+"          "));
           ASK ResultsStream TO WriteString("   "+BlockArray[k,12]);
           ASK ResultsStream TO WriteLn;
        END FOR;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Reliability_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Block                  RMin          RMean         RMax          RStdDev       RSEM");
        ASK ResultsStream TO WriteLn;
        k:=0;         
        FOR i:=1 TO totalBlocks
           block := ASK root Child("RBDBlock",BlockAlphaSort[i]);
           INC(k);
           ASK ResultsStream TO WriteString(SUBSTR(1,20,block.name+"                    "));
           ASK ResultsStream TO WriteString("   "+BlockArray[k,13]);
           ASK ResultsStream TO WriteString("   "+BlockArray[k,14]);
           ASK ResultsStream TO WriteString("   "+BlockArray[k,15]);
           ASK ResultsStream TO WriteString(SUBSTR(1,14,"   "+BlockArray[k,16]+"          "));
           ASK ResultsStream TO WriteString("   "+BlockArray[k,17]);
           ASK ResultsStream TO WriteLn;
        END FOR;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("State_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Block                   %Running    %Standby       %Idle  %Repairing    %RepHold         %PM     %PMHold       %Done");
        ASK ResultsStream TO WriteLn;
        k:=0;         
        FOR i:=1 TO totalBlocks
           block := ASK root Child("RBDBlock",BlockAlphaSort[i]);
           INC(k);
           RunMon:=GETMONITOR(BlockEndRun[block.seqNum],RStatObj);
           StandbyMon:=GETMONITOR(BlockEndStandby[block.seqNum],RStatObj);
           BlockIdleMon:=GETMONITOR(BlockEndIdle[block.seqNum],RStatObj);
           RepMon:=GETMONITOR(BlockEndRep[block.seqNum],RStatObj);
           BlockRepHoldMon:=GETMONITOR(BlockEndRepHold[block.seqNum],RStatObj);
           BlockPMMon:=GETMONITOR(BlockEndPM[block.seqNum],RStatObj);
           BlockPMHoldMon:=GETMONITOR(BlockEndPMHold[block.seqNum],RStatObj);
           BlockDoneMon:=GETMONITOR(BlockEndDone[block.seqNum],RStatObj);
           ASK ResultsStream TO WriteString(SUBSTR(1,20,block.name+"                    "));
           ASK ResultsStream TO WriteString(REALTOSN(RunMon.Mean,12,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(StandbyMon.Mean,12,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(BlockIdleMon.Mean,12,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(RepMon.Mean,12,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(BlockRepHoldMon.Mean,12,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(BlockPMMon.Mean,12,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(BlockPMHoldMon.Mean,12,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(BlockDoneMon.Mean,12,6,1));
           ASK ResultsStream TO WriteLn;
        END FOR;
        ASK RunMon TO Reset;          {tony 2-12}
        ASK StandbyMon TO Reset;
        ASK BlockIdleMon TO Reset;
        ASK RepMon TO Reset;
        ASK BlockRepHoldMon TO Reset;
        ASK BlockPMMon TO Reset;
        ASK BlockPMHoldMon TO Reset;
        ASK BlockDoneMon TO Reset;
        ASK ResultsStream TO WriteLn;
     END IF;  {ResultsFile}
     IF (totalBlocks>0)
        UpdateBlockArray;
     END IF;   
     
     IF ((ResultsFile) AND (totalEvents>0))
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteString("EVENT_ANALYSIS"); 
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn; 
     END IF;
     IF EventArray<>NILARRAY
        DISPOSE(EventArray);
     END IF;
     IF (totalEvents>0)
        NEW(EventArray,0..totalEvents,1..19);
        EventArray[0,1]:="Event";
        EventArray[0,2]:="Color";
        EventArray[0,3]:="Ao Min";
        EventArray[0,4]:="Ao Mean";
        EventArray[0,5]:="Ao Max";
        EventArray[0,6]:="Ao Stdev";
        EventArray[0,7]:="Ao SEM";
        EventArray[0,8]:="Do Min";
        EventArray[0,9]:="Do Mean";
        EventArray[0,10]:="Do Max";
        EventArray[0,11]:="Do Stdev";
        EventArray[0,12]:="Do SEM";
        EventArray[0,13]:="R Min";
        EventArray[0,14]:="R Mean";
        EventArray[0,15]:="R Max";
        EventArray[0,16]:="R Stdev";
        EventArray[0,17]:="R SEM";
        EventArray[0,18]:="BlockId";
        k:=0;
        FOR i:=1 TO totalEvents
           event := ASK root Child("RBDEvent",EventAlphaSort[i]);
           j:=event.seqNum;
           AoMon:=GETMONITOR(EventEndAo[j],RStatObj);
           DoMon:=GETMONITOR(EventEndDo[j],RStatObj);
           CompRMon:=GETMONITOR(EventEndR[j],RStatObj);
           ASK event TO SetAoDoR(REALTOSigFigSTR(AoMon.Mean,9),REALTOSigFigSTR(DoMon.Mean,9),REALTOSigFigSTR(CompRMon.Mean,9));    
           INC(k);
           EventArray[k,1]:=SUBSTR(1,20,event.name + "                    ");
           EventArray[k,2]:="Green";
           EventArray[k,3]:=REALTOSN(AoMon.Minimum,11,9,1); 
           EventArray[k,4]:=REALTOSN(AoMon.Mean,11,9,1);
           EventArray[k,5]:=REALTOSN(AoMon.Maximum,11,9,1);
           EventArray[k,6]:=GetDisplayStr(AoMon.StdDev,AoMon.Count,11,9,1,"stdev"); 
           EventArray[k,7]:=GetDisplayStr(AoMon.StdDev,AoMon.Count,11,9,1,"sem");
           EventArray[k,8]:=REALTOSN(DoMon.Minimum,11,9,1); 
           EventArray[k,9]:=REALTOSN(DoMon.Mean,11,9,1);
           EventArray[k,10]:=REALTOSN(DoMon.Maximum,11,9,1);
           EventArray[k,11]:=GetDisplayStr(DoMon.StdDev,DoMon.Count,11,9,1,"stdev"); 
           EventArray[k,12]:=GetDisplayStr(DoMon.StdDev,DoMon.Count,11,9,1,"sem");
           EventArray[k,13]:=REALTOSN(CompRMon.Minimum,11,9,1); 
           EventArray[k,14]:=REALTOSN(CompRMon.Mean,11,9,1);
           EventArray[k,15]:=REALTOSN(CompRMon.Maximum,11,9,1);
           EventArray[k,16]:=GetDisplayStr(CompRMon.StdDev,CompRMon.Count,11,9,1,"stdev"); 
           EventArray[k,17]:=GetDisplayStr(CompRMon.StdDev,CompRMon.Count,11,9,1,"sem");
           EventArray[k,18]:=INTTOSTR(event.Id);
           EventArray[k,19]:=INTTOSTR(event.parentID);
        END FOR;
     END IF;  {totalEvents>0}   
     IF ((ResultsFile) AND (totalEvents>0))
        ASK ResultsStream TO WriteString("Availability_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Event                  AoMin         AoMean        AoMax         AoStdDev      AoSEM");
        ASK ResultsStream TO WriteLn;
        k:=0;
        FOR i:=1 TO totalEvents
           event := ASK root Child("RBDEvent",EventAlphaSort[i]);
           INC(k);
           ASK ResultsStream TO WriteString(SUBSTR(1,20,event.name+"                    "));
           ASK ResultsStream TO WriteString("   "+EventArray[k,3]);
           ASK ResultsStream TO WriteString("   "+EventArray[k,4]);
           ASK ResultsStream TO WriteString("   "+EventArray[k,5]);
           ASK ResultsStream TO WriteString(SUBSTR(1,14,"   "+EventArray[k,6]+"          "));
           ASK ResultsStream TO WriteString("   "+EventArray[k,7]);
           ASK ResultsStream TO WriteLn;
        END FOR; 
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Dependability_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Event                  DoMin         DoMean        DoMax         DoStdDev      DoSEM");
        ASK ResultsStream TO WriteLn;
        k:=0;
        FOR i:=1 TO totalEvents
           event := ASK root Child("RBDEvent",EventAlphaSort[i]);
           INC(k);
           ASK ResultsStream TO WriteString(SUBSTR(1,20,event.name+"                    "));
           ASK ResultsStream TO WriteString("   "+EventArray[k,8]);
           ASK ResultsStream TO WriteString("   "+EventArray[k,9]);
           ASK ResultsStream TO WriteString("   "+EventArray[k,10]);
           ASK ResultsStream TO WriteString(SUBSTR(1,14,"   "+EventArray[k,11]+"          "));
           ASK ResultsStream TO WriteString("   "+EventArray[k,12]);
           ASK ResultsStream TO WriteLn;
        END FOR;      
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Reliability_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Event                  RMin          RMean         RMax          RStdDev       RSEM");
        ASK ResultsStream TO WriteLn;
        k:=0;
        FOR i:=1 TO totalEvents
           event := ASK root Child("RBDEvent",EventAlphaSort[i]);
           INC(k);
           ASK ResultsStream TO WriteString(SUBSTR(1,20,event.name+"                    "));
           ASK ResultsStream TO WriteString("   "+EventArray[k,13]);
           ASK ResultsStream TO WriteString("   "+EventArray[k,14]);
           ASK ResultsStream TO WriteString("   "+EventArray[k,15]);
           ASK ResultsStream TO WriteString(SUBSTR(1,14,"   "+EventArray[k,16]+"          "));
           ASK ResultsStream TO WriteString("   "+EventArray[k,17]);
           ASK ResultsStream TO WriteLn;
        END FOR; 
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("State_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Event                     %Success        %Armed      %Failure ");
        ASK ResultsStream TO WriteLn;
        FOR i:=1 TO totalEvents
           event := ASK root Child("RBDEvent",EventAlphaSort[i]);
           RunMon:=GETMONITOR(EventEndRun[event.seqNum],RStatObj);
           StandbyMon:=GETMONITOR(EventEndStandby[event.seqNum],RStatObj);
           RepMon:=GETMONITOR(EventEndRep[event.seqNum],RStatObj);
           ASK ResultsStream TO WriteString(SUBSTR(1,20,event.name+"                    "));
           ASK ResultsStream TO WriteString(REALTOSN(RunMon.Mean,14,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(StandbyMon.Mean,14,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(RepMon.Mean,14,6,1));
           ASK ResultsStream TO WriteLn;
        END FOR;      
        ASK ResultsStream TO WriteLn;
        ASK RunMon TO Reset;
        ASK StandbyMon TO Reset;
        ASK RepMon TO Reset;
     END IF; {(ResultsFile) AND (totalEvents>0)}
     IF (totalEvents>0)
        UpdateEventArray;
     END IF;   
     
     IF HierArray<>NILARRAY
        DISPOSE(HierArray);
     END IF;
     IF (totalHiers>0)
        NEW(HierArray,0..totalHiers,1..18);
        HierArray[0,1]:="Hier";
        HierArray[0,2]:="Color";
        HierArray[0,3]:="Ao Min";
        HierArray[0,4]:="Ao Mean";
        HierArray[0,5]:="Ao Max";
        HierArray[0,6]:="Ao Stdev";
        HierArray[0,7]:="Ao SEM";
        HierArray[0,8]:="Do Min";
        HierArray[0,9]:="Do Mean";
        HierArray[0,10]:="Do Max";
        HierArray[0,11]:="Do Stdev";
        HierArray[0,12]:="Do SEM";
        HierArray[0,13]:="R Min";
        HierArray[0,14]:="R Mean";
        HierArray[0,15]:="R Max";
        HierArray[0,16]:="R Stdev";
        HierArray[0,17]:="R SEM";
        HierArray[0,18]:="NodeId";
        k:=0;
        FOREACH hier IN hierGroup
           node := ASK root Child("RBDNode",hier.outID);
           AoMon:=GETMONITOR(NodeEndAo[node.seqNum],RStatObj);
           DoMon:=GETMONITOR(NodeEndDo[node.seqNum],RStatObj);
           CompRMon:=GETMONITOR(NodeEndR[node.seqNum],RStatObj);
           INC(k);
           HierArray[k,1]:=SUBSTR(1,20,hier.name + "                    ");
           HierArray[k,2]:="Green";
           HierArray[k,3]:=REALTOSN(AoMon.Minimum,11,9,1); 
           HierArray[k,4]:=REALTOSN(AoMon.Mean,11,9,1);
           HierArray[k,5]:=REALTOSN(AoMon.Maximum,11,9,1);
           HierArray[k,6]:=GetDisplayStr(AoMon.StdDev,AoMon.Count,11,9,1,"stdev"); 
           HierArray[k,7]:=GetDisplayStr(AoMon.StdDev,AoMon.Count,11,9,1,"sem");
           HierArray[k,8]:=REALTOSN(DoMon.Minimum,11,9,1); 
           HierArray[k,9]:=REALTOSN(DoMon.Mean,11,9,1);
           HierArray[k,10]:=REALTOSN(DoMon.Maximum,11,9,1);
           HierArray[k,11]:=GetDisplayStr(DoMon.StdDev,DoMon.Count,11,9,1,"stdev"); 
           HierArray[k,12]:=GetDisplayStr(DoMon.StdDev,DoMon.Count,11,9,1,"sem");
           HierArray[k,13]:=REALTOSN(CompRMon.Minimum,11,9,1); 
           HierArray[k,14]:=REALTOSN(CompRMon.Mean,11,9,1);
           HierArray[k,15]:=REALTOSN(CompRMon.Maximum,11,9,1);
           HierArray[k,16]:=GetDisplayStr(CompRMon.StdDev,CompRMon.Count,11,9,1,"stdev"); 
           HierArray[k,17]:=GetDisplayStr(CompRMon.StdDev,CompRMon.Count,11,9,1,"sem");
           HierArray[k,18]:=INTTOSTR(node.Id);
        END FOREACH;  
     END IF;    {totalHiers>0}
     IF ((ResultsFile) AND (totalHiers>0))         
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteString("HIERARCHY_ANALYSIS"); 
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteString(SUBSTR(1,23,"                                 ")+"MTBDE         MDT "); 
        ASK ResultsStream TO WriteLn; 
        FOREACH hier IN hierGroup
           hMTBDEMon:=GETMONITOR(hMTBDE[hier.seqNum],RStatObj);
           hMDTMon:=GETMONITOR(hMDT[hier.seqNum],RStatObj);
           ASK ResultsStream TO WriteString(SUBSTR(1,23,hier.name+"                        "));
           ASK ResultsStream TO WriteString(REALTOSN(hMTBDEMon.Mean,10,2,0)+"    ");
           IF (hMDTMon.Count>0)
              ASK ResultsStream TO WriteString(REALTOSN(hMDTMon.Mean,10,2,0));
           ELSE
              ASK ResultsStream TO WriteString("N/A");
           END IF;   
           ASK ResultsStream TO WriteLn;
   {tony - Mons are reset later, why do now?}
        END FOREACH;
        ASK hMTBDEMon TO Reset;
        ASK hMDTMon TO Reset;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Availability_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Hierarchy              AoMin         AoMean        AoMax         AoStdDev      AoSEM");
        ASK ResultsStream TO WriteLn;
        k:=0;
        FOREACH hier IN hierGroup
           INC(k);
           ASK ResultsStream TO WriteString(SUBSTR(1,20,hier.name+"                    "));
           ASK ResultsStream TO WriteString("   "+HierArray[k,3]);
           ASK ResultsStream TO WriteString("   "+HierArray[k,4]);
           ASK ResultsStream TO WriteString("   "+HierArray[k,5]);
           ASK ResultsStream TO WriteString(SUBSTR(1,14,"   "+HierArray[k,6]+"          "));
           ASK ResultsStream TO WriteString("   "+HierArray[k,7]);
           ASK ResultsStream TO WriteLn;
        END FOREACH;         
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Dependability_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Hierarchy              DoMin         DoMean        DoMax         DoStdDev      DoSEM");
        ASK ResultsStream TO WriteLn;
        k:=0;         
        FOREACH hier IN hierGroup
           INC(k);
           ASK ResultsStream TO WriteString(SUBSTR(1,20,hier.name+"                    "));
           ASK ResultsStream TO WriteString("   "+HierArray[k,8]);
           ASK ResultsStream TO WriteString("   "+HierArray[k,9]);
           ASK ResultsStream TO WriteString("   "+HierArray[k,10]);
           ASK ResultsStream TO WriteString(SUBSTR(1,14,"   "+HierArray[k,11]+"          "));
           ASK ResultsStream TO WriteString("   "+HierArray[k,12]);
           ASK ResultsStream TO WriteLn;
        END FOREACH; 
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Reliability_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Hierarchy              RMin          RMean         RMax          RStdDev       RSEM");
        ASK ResultsStream TO WriteLn;
        k:=0;            
        FOREACH hier IN hierGroup
           INC(k);
           ASK ResultsStream TO WriteString(SUBSTR(1,20,hier.name+"                    "));
           ASK ResultsStream TO WriteString("   "+HierArray[k,13]);
           ASK ResultsStream TO WriteString("   "+HierArray[k,14]);
           ASK ResultsStream TO WriteString("   "+HierArray[k,15]);
           ASK ResultsStream TO WriteString(SUBSTR(1,14,"   "+HierArray[k,16]+"          "));
           ASK ResultsStream TO WriteString("   "+HierArray[k,17]);
           ASK ResultsStream TO WriteLn;
        END FOREACH; 
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("State_Data");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Hierarchy                 %Green     %Yellow        %Red       %Blue      %Brown     %Orange");    
        ASK ResultsStream TO WriteLn;
        FOREACH hier IN hierGroup
           node := ASK root Child("RBDNode",hier.outID);
           NodeGreenMon:=GETMONITOR(NodeEndGreen[node.seqNum],RStatObj);
           NodeYellowMon:=GETMONITOR(NodeEndYellow[node.seqNum],RStatObj);
           NodeRedMon:=GETMONITOR(NodeEndRed[node.seqNum],RStatObj);
           NodeBlueMon:=GETMONITOR(NodeEndBlue[node.seqNum],RStatObj);
           NodeBrownMon:=GETMONITOR(NodeEndBrown[node.seqNum],RStatObj);
           NodeOrangeMon:=GETMONITOR(NodeEndOrange[node.seqNum],RStatObj);
           ASK ResultsStream TO WriteString(SUBSTR(1,20,hier.name+"                    "));
           ASK ResultsStream TO WriteString(REALTOSN(NodeGreenMon.Mean,12,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(NodeYellowMon.Mean,12,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(NodeRedMon.Mean,12,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(NodeBlueMon.Mean,12,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(NodeBrownMon.Mean,12,6,1));
           ASK ResultsStream TO WriteString(REALTOSN(NodeOrangeMon.Mean,12,6,1));
           ASK ResultsStream TO WriteLn;   
        END FOREACH;
        ASK NodeGreenMon TO Reset;         {tony 2-12}
        ASK NodeYellowMon TO Reset;
        ASK NodeRedMon TO Reset;
        ASK NodeBlueMon TO Reset;
        ASK NodeBrownMon TO Reset;
        ASK NodeOrangeMon TO Reset;
        ASK ResultsStream TO WriteLn;   
     END IF;   {resultsFile} 
     IF totalHiers>0
        UpdateHierArray;
     END IF;   
  END IF;  {weakLinkAnalysis}
  ASK simMenuBar TO ChangeState("FinishSim");   
     
  {tony 01-06: special reliability file}
{  IF (relMesh>0.0)
     FOR i:=1 TO TRUNC(StopCriteria/relMesh)
        ASK relStream TO WriteString(REALTOSN( FLOAT(i-1)*relMesh,15,6,0 ) );
        ASK relStream TO WriteString(REALTOSN( ( FLOAT(reliabilityFileData[i-1])/FLOAT(NumRunsCompleted) ),12,9,0));
        ASK relStream TO WriteLn;
     END FOR;
     ASK relStream TO WriteString(REALTOSN(StopCriteria,15,6,0));
     ASK relStream TO WriteString(REALTOSN(RMon.Mean,12,9,0));
     ASK relStream TO WriteLn;
  END IF;     }       {cmc 6/19/07}
END PROCEDURE; {DoEndOfSimUpdate}    

PROCEDURE FillStatusBar();
VAR
    compPercent: REAL;
    outString  : STRING;
    phase      : PhaseObj;
BEGIN
    IF SimTime<1000.0
       outString:=REALTOSTR(SimTime);
    ELSE
       outString:=REALTOSN(SimTime,10,2,1);
    END IF;   
    ASK window TO ShowStatus(0,"Sim Time : "+outString);
    ASK window TO ShowStatus(1,"Run " + INTTOSTR(runNumber) + " of " + INTTOSTR(NumRuns));
        IF termType=1
           compPercent:=100.*SimTime/StopCriteria;
           ASK window TO ShowStatus(2,REALTOSN(compPercent,6,2,0)+"% complete"); 
        ELSIF termType=2
           ASK window TO ShowStatus(2,INTTOSTR(termCounter)+" system failure(s)"); 
        ELSE
           IF termCounter=1
              ASK window TO ShowStatus(2,INTTOSTR(termCounter)+" cycle completed"); 
           ELSE   
              ASK window TO ShowStatus(2,INTTOSTR(termCounter)+" cycles completed"); 
           END IF;   
        END IF;
    IF (activePhases>0)
       phase:=phaseObjArray[phaseNumber];
       ASK window TO ShowStatus(3,phase.phaseName);
    END IF;
    IF runNumber>1
       IF (totalBlocks>0)
          ASK window TO ShowStatus(5,"Ao: "+REALTOSN(AvailMon.Mean,15,9,0));
          ASK window TO ShowStatus(6,"MTBDE: "+REALTOSN(MTBDEMon.Mean,15,6,0));
          IF MDTMon.Count>0
            ASK window TO ShowStatus(7,"MDT: "+REALTOSN(MDTMon.Mean,15,6,0));
          ELSE
             ASK window TO ShowStatus(7,"MDT: "+"N/A");      
          END IF;
          ASK window TO ShowStatus(8,"R: "+REALTOSN(RMon.Mean,15,9,0));
       END IF;
    END IF;
END PROCEDURE;
   
   
PROCEDURE ProduceCostResults();
VAR
  i,j,k                                           : INTEGER;
  ic,oc,rc,csc,hc,idc,dc,spc,ac,esc,sic,dfc,tc,
  {init,op,rep,cold,hold,idle,done,spare,add,emerShip,spInit,disposal}
  bict,boct,brct,bcsct,bhct,bidct,bdct,bspct,besct,bsict,bdfct,btoct,btsct,btct,    
  eict,eoct,efct,etct,pict,pact,pesct,ptct,puc,ptc,rict,rpuct,rptct,rtct,pmc,pmhc,bpmct,bpmhct,
  grandTotal,sysRedCost,totalLostCost             : REAL;
  {resource - perUseCost,perTimeCost}
  OperatingCost,SparingCost                       : ARRAY INTEGER OF REAL;
  block                                           : RBDBlockObj;
  event                                           : RBDEventObj;
  hier,papa                                       : RBDHierObj;
  HierCosts                                       : ARRAY INTEGER, INTEGER OF REAL;
BEGIN    
  IF totalBlocks>0
     NEW(OperatingCost,1..totalBlocks);
     NEW(SparingCost,1..totalBlocks);
  END IF;   
  IF totalBlocks>0   
     BlockCostMonsSet:=TRUE;
     IF ResultsFile
        ASK ResultsStream TO WriteString("***COST***"); 
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Operating");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Block                 InHier        Good      Repair        Idle"+
                                               "     RepHold          PM      PMHold     Standby        Done");
        ASK ResultsStream TO WriteLn;
     END IF;
     IF (totalHiers>0)
        IF (HierCosts<>NILARRAY)
           DISPOSE(HierCosts);
        END IF;
        NEW(HierCosts,1..totalHiers,1..6);
     END IF;
     FOR i:=1 TO totalBlocks
        block := ASK window Descendant("RBDBlock", BlockAlphaSort[i]); 
        j:=block.seqNum;
        OpCostMon:=GETMONITOR(BlockEndOpCost[j],RStatObj);
        RepCostMon:=GETMONITOR(BlockEndRepCost[j],RStatObj);
        BlockIdleCostMon:=GETMONITOR(BlockEndIdleCost[j],RStatObj);
        BlockHoldCostMon:=GETMONITOR(BlockEndHoldCost[j],RStatObj);
        BlockPMCostMon:=GETMONITOR(BlockEndPMCost[j],RStatObj);
        BlockpmHoldCostMon:=GETMONITOR(BlockEndpmHoldCost[j],RStatObj);
        BlockSBCostMon:=GETMONITOR(BlockEndSBCost[j],RStatObj);
        BlockDoneCostMon:=GETMONITOR(BlockEndDoneCost[j],RStatObj);
        oc:=OpCostMon.Mean;
        rc:=RepCostMon.Mean;
        idc:=BlockIdleCostMon.Mean;
        hc:=BlockHoldCostMon.Mean;
        csc:=BlockSBCostMon.Mean;
        dc:=BlockDoneCostMon.Mean;
        pmc:=BlockPMCostMon.Mean;
        pmhc:=BlockpmHoldCostMon.Mean;
        OperatingCost[j]:=oc+rc+idc+hc+csc+dc+pmc+pmhc;
        boct:=boct+oc;
        brct:=brct+rc;
        bidct:=bidct+idc;
        bhct:=bhct+hc;
        bcsct:=bcsct+csc;
        bdct:=bdct+dc;
        bpmct:=bpmct+pmc;
        bpmhct:=bpmhct+pmhc;
        IF ResultsFile
           ASK ResultsStream TO WriteString(SUBSTR(1,20,block.name + "                    ")+" "); 
           ASK ResultsStream TO WriteInt(block.parentID,4);
           ASK ResultsStream TO WriteString("    ");
           ASK ResultsStream TO WriteString(REALTOSN(oc,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(rc,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(idc,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(hc,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(pmc,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(pmhc,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(csc,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(dc,11,2,1)+" ");
           ASK ResultsStream TO WriteLn; 
        END IF;
     END FOR;
     IF ResultsFile
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Totals                       ");
        ASK ResultsStream TO WriteString(REALTOSN(boct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(brct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(bidct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(bhct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(bpmct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(bpmhct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(bcsct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(bdct,11,2,1)+" ");
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Sparing");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Block                 InitSpares   NewSpares    EmerShip");
        ASK ResultsStream TO WriteLn;
     END IF;
     FOR i:=1 TO totalBlocks
        block := ASK window Descendant("RBDBlock",BlockAlphaSort[i]); 
        j:=block.seqNum;
        BlockSpCostMon:=GETMONITOR(BlockEndSpCost[j],RStatObj);
        BlockESCostMon:=GETMONITOR(BlockEndESCost[j],RStatObj);
        IF ((block.sparingType=Custom) AND (NOT ZeroInitCost))
           sic:=FLOAT(block.initStock)*block.spareCost;
        ELSE
           sic:=0.0;
        END IF;
        spc:=BlockSpCostMon.Mean;
        esc:=BlockESCostMon.Mean;
        bsict:=bsict+sic;
        bspct:=bspct+spc;
        besct:=besct+esc;
        SparingCost[j]:=sic+spc+esc;
        IF ((ResultsFile) AND (block.sparingType<>SparePool))
           ASK ResultsStream TO WriteString(SUBSTR(1,20,block.name + "                    ")+" "); 
           ASK ResultsStream TO WriteString(REALTOSN(sic,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(spc,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(esc,11,2,1)+" ");
           ASK ResultsStream TO WriteLn; 
        END IF;
     END FOR;
     IF ResultsFile
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Totals               ");
        ASK ResultsStream TO WriteString(REALTOSN(bsict,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(bspct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(besct,11,2,1)+" ");
        ASK ResultsStream TO WriteLn;         
        ASK ResultsStream TO WriteLn;         
        ASK ResultsStream TO WriteString("Life_cycle_cost");
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteLn; 
        ASK ResultsStream TO WriteString("Block                   Purchase   Operating     Sparing    Disposal       Total");
        ASK ResultsStream TO WriteLn;
     END IF;
     IF BlockCostArray<>NILARRAY
        DISPOSE(BlockCostArray);
     END IF;
     NEW(BlockCostArray,0..totalBlocks,1..7);
     BlockCostArray[0,1]:="blockName";
     BlockCostArray[0,2]:="Purchase";
     BlockCostArray[0,3]:="Operating";
     BlockCostArray[0,4]:="Sparing";
     BlockCostArray[0,5]:="Disposal";
     BlockCostArray[0,6]:="Total";
     k:=0;
     FOR i:=1 TO totalBlocks
        block := ASK window Descendant("RBDBlock", BlockAlphaSort[i]); 
        INC(k);
        j:=block.seqNum;
        BlockDispCostMon:=GETMONITOR(BlockEndDispCost[j],RStatObj);
        IF ZeroInitCost
           ic:=0.0;
        ELSE
           ic:=block.initialCost;
        END IF;
        dfc:=BlockDispCostMon.Mean;
        tc:=ic+OperatingCost[j]+SparingCost[j]+dfc;
        bict:=bict+ic;
        btoct:=btoct+OperatingCost[j];
        btsct:=btsct+SparingCost[j];
        bdfct:=bdfct+dfc;
        btct:=btct+tc;
        BlockCostArray[k,1]:=SUBSTR(1,20,block.name + "                    ");
        BlockCostArray[k,2]:=REALTOCOST(ic,13,3);
        BlockCostArray[k,3]:=REALTOCOST(OperatingCost[j],13,3);
        IF (block.sparingType<>SparePool)
           BlockCostArray[k,4]:=REALTOCOST(SparingCost[j],13,3);
        ELSE
           BlockCostArray[k,4]:="N/A";
        END IF;   
        BlockCostArray[k,5]:=REALTOCOST(dfc,13,3);
        BlockCostArray[k,6]:=REALTOCOST(tc,13,3);
        BlockCostArray[k,7]:=INTTOSTR(block.parentID);
        IF ResultsFile
           ASK ResultsStream TO WriteString(SUBSTR(1,20,block.name + "                    ")+" "); 
           ASK ResultsStream TO WriteString(REALTOSN(ic,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(OperatingCost[j],11,2,1)+" ");
           IF (block.sparingType<>SparePool)
              ASK ResultsStream TO WriteString(REALTOSN(SparingCost[j],11,2,1)+" ");
           ELSE
              ASK ResultsStream TO WriteString("        N/A ");
           END IF;   
           ASK ResultsStream TO WriteString(REALTOSN(dfc,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(tc,11,2,1)+" ");
           ASK ResultsStream TO WriteLn; 
        END IF;
        IF ((totalHiers>0) AND (block.parentID>0))
           hier := ASK root Child("RBDHier",block.parentID);
           HierCosts[hier.seqNum,1]:=HierCosts[hier.seqNum,1]+ic;
           HierCosts[hier.seqNum,2]:=HierCosts[hier.seqNum,2]+OperatingCost[j];
           HierCosts[hier.seqNum,3]:=HierCosts[hier.seqNum,3]+SparingCost[j];
           HierCosts[hier.seqNum,4]:=HierCosts[hier.seqNum,4]+dfc;
           HierCosts[hier.seqNum,6]:=HierCosts[hier.seqNum,6]+tc;
        END IF;
     END FOR;
     IF ResultsFile
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Block_Totals         ");
        ASK ResultsStream TO WriteString(REALTOSN(bict,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(btoct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(btsct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(bdfct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(btct,11,2,1)+" ");
        ASK ResultsStream TO WriteLn;         
        ASK ResultsStream TO WriteLn;         
        ASK ResultsStream TO WriteString("________________________________________________________________________________");
        ASK ResultsStream TO WriteLn;         
        ASK ResultsStream TO WriteLn; 
     END IF;
  END IF;
  IF totalEvents>0
     BlockCostMonsSet:=TRUE;
     IF ResultsFile
        ASK ResultsStream TO WriteString("Event                 InHier    Initial     Success     Failure       Total");
        ASK ResultsStream TO WriteLn;
     END IF;
     k:=0;
     FOR i:=1 TO totalEvents
        event := ASK window Descendant("RBDEvent", EventAlphaSort[i]); 
        INC(k);
        j:=event.seqNum;
        OpCostMon:=GETMONITOR(EventEndOpCost[j],RStatObj);
        RepCostMon:=GETMONITOR(EventEndRepCost[j],RStatObj);
        IF ZeroInitCost
           ic:=0.0;
        ELSE           
           ic:=event.initialCost;
        END IF;
        oc:=OpCostMon.Mean;
        rc:=RepCostMon.Mean;
        tc:= ic+oc+rc;
        eict:=eict+ic;
        eoct:=eoct+oc;
        efct:=efct+rc;
        etct:=etct+tc;
        IF ResultsFile
           ASK ResultsStream TO WriteString(SUBSTR(1,20,event.name + "                    ")+" "); 
           ASK ResultsStream TO WriteInt(event.parentID,4);
           ASK ResultsStream TO WriteString("   ");
           ASK ResultsStream TO WriteString(REALTOSN(ic,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(oc,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(rc,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(tc,11,2,1)+" ");
           ASK ResultsStream TO WriteLn; 
        END IF;
        IF ((totalHiers>0) AND (event.parentID>0))
           hier := ASK root Child("RBDHier",event.parentID);
           HierCosts[hier.seqNum,5]:=HierCosts[hier.seqNum,5]+tc;
           HierCosts[hier.seqNum,6]:=HierCosts[hier.seqNum,6]+tc;
        END IF;
     END FOR;
     IF ResultsFile
        ASK ResultsStream TO WriteLn;
        ASK ResultsStream TO WriteString("Event_Totals                ");
        ASK ResultsStream TO WriteString(REALTOSN(eict,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(eoct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(efct,11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(etct,11,2,1)+" ");
        ASK ResultsStream TO WriteLn;         
        ASK ResultsStream TO WriteLn;         
        ASK ResultsStream TO WriteString("________________________________________________________________________________");
        ASK ResultsStream TO WriteLn;         
        ASK ResultsStream TO WriteLn; 
     END IF;
  END IF;
  IF ((totalHiers>0) AND (ResultsFile))
     FOR i:=totalHiers DOWNTO 1
        hier:=ASK root Child("RBDHier",HierAlphaSort[i]);
        IF (hier.parentID<>0)
           papa := ASK root Child("RBDHier",hier.parentID); 
           HierCosts[papa.seqNum,1]:=HierCosts[papa.seqNum,1]+HierCosts[i,1];
           HierCosts[papa.seqNum,2]:=HierCosts[papa.seqNum,2]+HierCosts[i,2];
           HierCosts[papa.seqNum,3]:=HierCosts[papa.seqNum,3]+HierCosts[i,3];
           HierCosts[papa.seqNum,4]:=HierCosts[papa.seqNum,4]+HierCosts[i,4];
           HierCosts[papa.seqNum,5]:=HierCosts[papa.seqNum,5]+HierCosts[i,5];
           HierCosts[papa.seqNum,6]:=HierCosts[papa.seqNum,6]+HierCosts[i,6];
         {TONY - will a subhierarchy always have a lower ID number?}
        END IF;
     END FOR;
     ASK ResultsStream TO WriteLn;
     ASK ResultsStream TO WriteString("Hierarchy            HierNum    Purchase   Operating     Sparing    Disposal      Events       Total");
     ASK ResultsStream TO WriteLn;
     FOR i:=1 TO totalHiers      
        hier:=ASK root Child("RBDHier",HierAlphaSort[i]);
        ASK ResultsStream TO WriteString(SUBSTR(1,20,hier.name + "                    ")+" "); 
        ASK ResultsStream TO WriteInt(hier.Id,4);
        ASK ResultsStream TO WriteString("    ");
        ASK ResultsStream TO WriteString(REALTOSN(HierCosts[i,1],11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(HierCosts[i,2],11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(HierCosts[i,3],11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(HierCosts[i,4],11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(HierCosts[i,5],11,2,1)+" ");
        ASK ResultsStream TO WriteString(REALTOSN(HierCosts[i,6],11,2,1)+" ");
        ASK ResultsStream TO WriteLn; 
     END FOR;      
     ASK ResultsStream TO WriteLn;         
     ASK ResultsStream TO WriteLn;         
     ASK ResultsStream TO WriteString("____________________________________________________________________________________________________");
     ASK ResultsStream TO WriteLn;         
     ASK ResultsStream TO WriteLn; 
  END IF;  {totalHiers>0}
  IF (totalPools>0)
     PoolCostMonsSet:=TRUE;
     IF totalSpares>0
        IF ResultsFile
           ASK ResultsStream TO WriteString("SparePools               Initial   NewSpares    EmerShip       Total");
           ASK ResultsStream TO WriteLn;
        END IF;
        FOR i:=1 TO totalPools
           IF PoolArray[i].sparingType=SparePool
              PoolAddCostMon:=GETMONITOR(PoolEndAddCost[i],RStatObj);
              PoolESCostMon:=GETMONITOR(PoolEndESCost[i],RStatObj);
              IF ZeroInitCost
                 ic:=0.0;
              ELSE
                 ic:=FLOAT(PoolArray[i].initialSpares)*PoolArray[i].spareCost;
              END IF;
              ac:=PoolAddCostMon.Mean;
              esc:=PoolESCostMon.Mean;
              tc:= ic+ac+esc;
              pict:=pict+ic;
              pact:=pact+ac;
              pesct:=pesct+esc;
              ptct:=ptct+tc;
              IF ResultsFile
                 ASK ResultsStream TO WriteString(SUBSTR(1,20,PoolArray[i].poolName + "                    ")+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(ic,11,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(ac,11,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(esc,11,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(tc,11,2,1));
                 ASK ResultsStream TO WriteLn; 
              END IF;
           END IF;
        END FOR;
        IF ResultsFile
           ASK ResultsStream TO WriteLn;
           ASK ResultsStream TO WriteString("SparePool_Totals     ");
           ASK ResultsStream TO WriteString(REALTOSN(pict,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(pact,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(pesct,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(ptct,11,2,1)+" ");
           ASK ResultsStream TO WriteLn;         
           ASK ResultsStream TO WriteLn;         
           ASK ResultsStream TO WriteString("________________________________________________________________________________");
           ASK ResultsStream TO WriteLn;         
           ASK ResultsStream TO WriteLn; 
        END IF;
     END IF;   {SparePools}
     IF totalRes>0
        IF ResultsFile
           ASK ResultsStream TO WriteString("Resources                Initial  PerUseCost PerTimeCost       Total");
           ASK ResultsStream TO WriteLn; 
        END IF;
        FOR i:=1 TO totalPools
           IF PoolArray[i].sparingType=Resource
              PoolAddCostMon:=GETMONITOR(PoolEndAddCost[i],RStatObj);
              PoolESCostMon:=GETMONITOR(PoolEndESCost[i],RStatObj);
              IF ZeroInitCost
                 ic:=0.0;
              ELSE
                 ic:=FLOAT(PoolArray[i].initialSpares)*PoolArray[i].spareCost;
              END IF;
              puc:=PoolESCostMon.Mean; 
              {reused variable-- PoolESCost used for emerShipping(pools) and perUseCost(resources)}
              ptc:=PoolAddCostMon.Mean;
              {reused variable-- PoolAddCost used for new Spares(pools) and perTimeCost(resources)}
              tc:= ic+ptc+puc;
              rict:=rict+ic;
              rptct:=rptct+ptc;
              rpuct:=rpuct+puc;
              rtct:=rtct+tc;
              IF ResultsFile
                 ASK ResultsStream TO WriteString(SUBSTR(1,20,PoolArray[i].poolName + "                    ")+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(ic,11,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(puc,11,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(ptc,11,2,1)+" ");
                 ASK ResultsStream TO WriteString(REALTOSN(tc,11,2,1)+" ");
                ASK ResultsStream TO WriteLn; 
             END IF;
           END IF;
        END FOR;
        IF ResultsFile
           ASK ResultsStream TO WriteLn;
           ASK ResultsStream TO WriteString("Resource_Totals      ");
           ASK ResultsStream TO WriteString(REALTOSN(rict,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(rpuct,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(rptct,11,2,1)+" ");
           ASK ResultsStream TO WriteString(REALTOSN(rtct,11,2,1)+" ");
           ASK ResultsStream TO WriteLn;         
           ASK ResultsStream TO WriteLn;         
           ASK ResultsStream TO WriteString("________________________________________________________________________________");
           ASK ResultsStream TO WriteLn;         
           ASK ResultsStream TO WriteLn; 
        END IF;
     END IF;   {SparePools}
  END IF;  {pools Exist}
  IF ResultsFile
     ASK ResultsStream TO WriteString("Cost_Summary");
     ASK ResultsStream TO WriteLn;  
     ASK ResultsStream TO WriteLn;  
     ASK ResultsStream TO WriteString("Blocks               ");
     ASK ResultsStream TO WriteString(REALTOSN(btct,11,2,1));
     ASK ResultsStream TO WriteLn;
     ASK ResultsStream TO WriteString("Events               ");
     ASK ResultsStream TO WriteString(REALTOSN(etct,11,2,1));
     ASK ResultsStream TO WriteLn;
     ASK ResultsStream TO WriteString("SparePools           ");
     ASK ResultsStream TO WriteString(REALTOSN(ptct,11,2,1));
     ASK ResultsStream TO WriteLn;
     ASK ResultsStream TO WriteString("Resources            ");
     ASK ResultsStream TO WriteString(REALTOSN(rtct,11,2,1));
     ASK ResultsStream TO WriteLn;
     IF System.redCost>0.0;
        sysRedCost:=SystemCostMon.Mean;
        ASK ResultsStream TO WriteString("SystemDownCost       ");
        ASK ResultsStream TO WriteString(REALTOSN(sysRedCost,11,2,1));
        ASK ResultsStream TO WriteLn;
     ELSE
        sysRedCost:=0.0;
     END IF;
     IF capacityAnalysis;
        totalLostCost:=SystemLostCostMon.Mean;
        ASK ResultsStream TO WriteString("SystemLostFlowCost   ");
        ASK ResultsStream TO WriteString(REALTOSN(totalLostCost,11,2,1));
        ASK ResultsStream TO WriteLn;
     ELSE
        totalLostCost:=0.0;
     END IF;  
     ASK ResultsStream TO WriteLn;
     ASK ResultsStream TO WriteString("________________________________");
     ASK ResultsStream TO WriteLn;
     ASK ResultsStream TO WriteString("Grand_Total          ");
     grandTotal:=btct+etct+ptct+rtct+sysRedCost+totalLostCost;
     ASK ResultsStream TO WriteString(REALTOSN(grandTotal,11,2,1));
     ASK ResultsStream TO WriteLn;
     ASK ResultsStream TO WriteLn;
     ASK ResultsStream TO WriteLn;
     ASK ResultsStream TO WriteLn;
  END IF;
  DISPOSE(OperatingCost);
  DISPOSE(SparingCost);
END PROCEDURE; {ProduceCostResults}

PROCEDURE UpdateNodeArray;
VAR
   i            : INTEGER;
   displayValue : REAL;
BEGIN
   FOR i:=1 TO HIGH(NodeArray)
      IF weakLinkAnalType=1
         displayValue:=STRTOREAL(NodeArray[i,4]);
      ELSIF weakLinkAnalType=2
         displayValue:=STRTOREAL(NodeArray[i,9]);
      ELSIF weakLinkAnalType=3
         displayValue:=STRTOREAL(NodeArray[i,14]);
      END IF;   
      IF displayValue >= GYthreshold
         NodeArray[i,2] := "Green";
      ELSIF displayValue >= YRthreshold
         NodeArray[i,2] := "Yellow";
      ELSE
         NodeArray[i,2] := "Red";
      END IF;
   END FOR;
END PROCEDURE; {UpdateNodeArray}

PROCEDURE UpdateBlockArray;
VAR
   i            : INTEGER;
   displayValue : REAL;
BEGIN
   FOR i:=1 TO HIGH(BlockArray)
      IF weakLinkAnalType=1
         displayValue:=STRTOREAL(BlockArray[i,4]);
      ELSIF weakLinkAnalType=2
         displayValue:=STRTOREAL(BlockArray[i,9]);
      ELSIF weakLinkAnalType=3
         displayValue:=STRTOREAL(BlockArray[i,14]);
      END IF;   
      IF displayValue >= GYthreshold
         BlockArray[i,2] := "Green";
      ELSIF displayValue >= YRthreshold
         BlockArray[i,2] := "Yellow";
      ELSE
         BlockArray[i,2] := "Red";
      END IF;
   END FOR;
END PROCEDURE; {UpdateBlockArray}


PROCEDURE UpdateEventArray;
VAR
   i            : INTEGER;
   displayValue : REAL;
BEGIN
   FOR i:=1 TO HIGH(EventArray)
      IF weakLinkAnalType=1
         displayValue:=STRTOREAL(EventArray[i,4]);
      ELSIF weakLinkAnalType=2
         displayValue:=STRTOREAL(EventArray[i,9]);
      ELSIF weakLinkAnalType=3
         displayValue:=STRTOREAL(EventArray[i,14]);
      END IF;   
      IF displayValue >= GYthreshold
         EventArray[i,2] := "Green";
      ELSIF displayValue >= YRthreshold
         EventArray[i,2] := "Yellow";
      ELSE
         EventArray[i,2] := "Red";
      END IF;
   END FOR;
END PROCEDURE; {UpdateEventArray}

PROCEDURE UpdateHierArray;
VAR
   i            : INTEGER;
   displayValue : REAL;
BEGIN
   FOR i:=1 TO HIGH(HierArray)
      IF weakLinkAnalType=1
         displayValue:=STRTOREAL(HierArray[i,4]);
      ELSIF weakLinkAnalType=2
         displayValue:=STRTOREAL(HierArray[i,9]);
      ELSIF weakLinkAnalType=3
         displayValue:=STRTOREAL(HierArray[i,14]);
      END IF;   
      IF displayValue >= GYthreshold
         HierArray[i,2] := "Green";
      ELSIF displayValue >= YRthreshold
         HierArray[i,2] := "Yellow";
      ELSE
         HierArray[i,2] := "Red";
      END IF;
   END FOR;
END PROCEDURE; {UpdateHierArray}

PROCEDURE REALTOSigFigSTR(IN inVal :REAL;IN sigFig:INTEGER):STRING;
VAR
   wholeAsInt, i          :INTEGER;
   fracAsReal             :REAL;
   intArray               :ARRAY INTEGER OF INTEGER;
   rounding,negative      :BOOLEAN;
   sigFigSTR              :STRING;
BEGIN
   IF sigFig<1
      RETURN(INTTOSTR(ROUND(inVal)));   
   END IF;
   IF inVal<0.0
      negative:=TRUE;
      inVal:=-inVal;
   END IF;
   wholeAsInt:=TRUNC(inVal);
   fracAsReal:=inVal-FLOAT(TRUNC(inVal));
   NEW(intArray, 1..sigFig+1);
   FOR i:=1 TO sigFig+1
     fracAsReal:=fracAsReal*10.0;
     intArray[i]:=TRUNC(fracAsReal);
     fracAsReal:=fracAsReal-FLOAT(TRUNC(fracAsReal));
   END FOR;
   IF intArray[sigFig+1]>=5 
      i:=sigFig;
      rounding:=TRUE;
      WHILE rounding
         IF (intArray[i]<9)
            INC(intArray[i]);
            rounding:=FALSE;
         ELSE
            intArray[i]:=0;
            IF i=1
               INC(wholeAsInt);
            rounding:=FALSE;
            ELSE
               DEC(i);
            END IF;
         END IF;            
      END WHILE;
   END IF;   
   sigFigSTR:=INTTOSTR(wholeAsInt)+".";
   FOR i:=1 TO sigFig
      sigFigSTR:=sigFigSTR+INTTOSTR(intArray[i]);
   END FOR;   
   IF negative
      sigFigSTR:="("+sigFigSTR+")";
   END IF;
   RETURN sigFigSTR;
END PROCEDURE;  {REALTOSigFigSTR} 

PROCEDURE REALTOSN(IN inVal :REAL;IN strSize,sigFig,justification:INTEGER):STRING;

{Rules for use of this procedure:
    1) For Scientific Notation to be enabled, 
       the string size must be >= (sigFig+6)
    2) 0<sigFig<=9 
    
     Justification:  0=left, 1=right, 2=center , 3=no spaces   
     sigFig means the number of digits after the decimal point}

VAR
   logVal,prefixVal,tooBigLevel,tooSmallLevel : REAL;
   i,exponent,preDot,postDot,
   tempInt,outSize,decInt,zeroes,numLength    : INTEGER;
   prefix,suffix,intPart,decPart,
   spaces,oddSpace,outVal                     : STRING;
   negative, validInput,canUseSN              : BOOLEAN;
BEGIN
   validInput:=TRUE;
   negative:=FALSE;
   numLength:=strSize;
   IF inVal<0.0
      negative:=TRUE;
      inVal:=-inVal;
      numLength:=numLength-2;
   END IF;
   IF sigFig>10
      sigFig:=10;
   END IF;
   IF ((numLength-sigFig)>10)
      numLength:=10+sigFig;
   END IF;
   IF numLength>=(sigFig+6)
      canUseSN:=TRUE;
   END IF;
   IF validInput
      preDot:=numLength-sigFig-1;
      tooBigLevel:=0.0;
      FOR i:=1 TO preDot
         tooBigLevel:=tooBigLevel+9.*POWER(10.0,FLOAT(i-1));
      END FOR;
      FOR i:=1 TO sigFig
         tooBigLevel:=tooBigLevel+9.*POWER(10.0,FLOAT(-i));
      END FOR;
      tooSmallLevel:=POWER(10.0,FLOAT(-sigFig));
      IF ((inVal>0.0) AND ((inVal>tooBigLevel) OR (inVal<tooSmallLevel)) AND canUseSN)
         IF numLength<=15
            postDot:=numLength-6;
         ELSE
            postDot:=9;
         END IF;
         logVal:=LOG10(inVal);
         IF inVal>1.0
            exponent:=TRUNC(logVal);  {the "Characteristic"}
         ELSE
            exponent:=FLOOR(logVal);  {the "Characteristic"}
         END IF;
         prefixVal:=inVal/POWER(10.0,FLOAT(exponent));
         intPart:=INTTOSTR(TRUNC(prefixVal));
         decPart:=REALTOSigFigSTR(prefixVal-FLOAT(TRUNC(prefixVal)),postDot);
         IF SUBSTR(1,1,decPart)="1"
            IF intPart="9"
               intPart:="1";
               exponent:=exponent+1;
            ELSE
               intPart:=INTTOSTR(STRTOINT(intPart)+1);
            END IF;
         END IF;
         decPart:=SUBSTR(2,postDot+2,decPart);
         IF exponent<-99
            suffix:="E-99";
         ELSIF exponent<-9
            suffix:="E"+INTTOSTR(exponent);
         ELSIF exponent<0
            suffix:="E-0"+INTTOSTR(ABS(exponent));
         ELSIF exponent<10
            suffix:="E+0"+INTTOSTR(exponent);
         ELSIF  exponent<100
            suffix:="E+"+INTTOSTR(exponent);
         ELSE
            suffix:="E+99";
         END IF;    
         IF exponent<-99
            outVal:="<1E-99";
         ELSIF  exponent<100
            outVal:=intPart+decPart+suffix;
         ELSE
            outVal:=">1E+99";
         END IF;
      ELSE
         outVal:=REALTOSigFigSTR(inVal,sigFig);
      END IF;
      IF negative
        outVal:="("+outVal+")";
      END IF;
      outSize:=STRLEN(outVal);
      IF outSize<strSize
         tempInt:=strSize-outSize;
         IF ODD(tempInt)
            oddSpace:=" ";
            DEC(tempInt);
         ELSE
            oddSpace:="";
         END IF;
         IF tempInt=0
            spaces:="";
         ELSE
            spaces:=SUBSTR(1,tempInt DIV 2,"                                    ");
         END IF;
         IF justification=0     {leftJustified}
            prefix:="";
            suffix:=spaces+spaces+oddSpace;
         ELSIF justification=1  {right Justified}
            prefix:=spaces+spaces+oddSpace;
            suffix:="";
         ELSIF justification=2  {centered}
            prefix:=spaces+oddSpace;
            suffix:=spaces;
         ELSIF justification=3  {none}
            prefix:="";
            suffix:="";         
         END IF;
            outVal:=prefix+outVal+suffix;
      ELSIF outSize>strSize
         outVal:=SUBSTR(1,strSize,"###################################################");
      END IF;
   END IF; 
   RETURN outVal;
END PROCEDURE;  {REALTOSN} 


PROCEDURE REALTOCOST(IN inVal :REAL;IN strSize,justification:INTEGER):STRING;

{Rules for use of this procedure:
    1) The string size must be >=7 for positive numbers,
        >=9 for negative numbers.
    
    2) Justification:  0=left, 1=right, 2=center, 3=no spaces    
     sigFig means the number of digits after the decimal point

    3) If K,B, or M are used valued are substringed, not rounded}
VAR
   dollarReal                                       : REAL;
   maxSize,fullLength,
   numChars,cents,
   dotPosition,lastDigitPos,numLeft                 : INTEGER;
   spaces,tempString,outString,outVal,
   dollarString,centString,suffix,prefix,oddSpace   : STRING;
   negative                                         : BOOLEAN;
BEGIN
   negative:=FALSE;
   maxSize:=strSize;
   IF inVal<0.0
      negative:=TRUE;
      inVal:=-inVal;
      maxSize:=maxSize-2;
   END IF;      
   tempString:=REALTOSTR(inVal);
   dotPosition := POSITION(tempString, ".");
   dollarString:= SUBSTR(1, dotPosition-1, tempString);
   dollarReal:=STRTOREAL(dollarString);
   cents:=ROUND((inVal-dollarReal)*100.0);
   IF cents=100
      dollarReal:=dollarReal+1.0;
      cents:=0;
      tempString:=REALTOSTR(dollarReal);
      dotPosition := POSITION(tempString, ".");
      dollarString:= SUBSTR(1, dotPosition-1, tempString);
   END IF;
   IF cents<10
      centString:="0"+INTTOSTR(cents);
   ELSE
      centString:=INTTOSTR(cents);
   END IF;
   IF maxSize<7
      outVal:=SUBSTR(1,strSize,"##########");
      RETURN outVal;   
   END IF;
   IF inVal>999999999999.995
      outVal:=">1T";
      RETURN outVal;   
   END IF;  
   IF  dollarReal<10.
      fullLength:=5;
      outString:="$"+dollarString+"."+centString;
   
   ELSIF dollarReal<100.
      fullLength:=6;
      outString:="$"+dollarString+"."+centString;
   
   ELSIF dollarReal<1000.
      fullLength:=7;
      outString:="$"+dollarString+"."+centString;
   
   ELSIF dollarReal<10000.       {$x,xxx.xx}
      fullLength:=9;
      IF fullLength<=maxSize
         outString:="$"+SUBSTR(1,1,dollarString)+","+SUBSTR(2,4,dollarString)+"."+centString;
      ELSE
         tempString:=dollarString+centString;
         lastDigitPos:=1+maxSize-4;
         IF lastDigitPos<STRLEN(tempString);
            outString:="$"+SUBSTR(1,1,tempString)+"."+SUBSTR(2,lastDigitPos,tempString)+"K";         
         ELSE
            outString:="$"+SUBSTR(1,1,tempString)+"."+SUBSTR(2,STRLEN(tempString),tempString)+"K";
         END IF;
      END IF;
   
   ELSIF dollarReal<100000.      {$xx,xxx.xx}
      fullLength:=10;
      IF fullLength<=maxSize
         outString:="$"+SUBSTR(1,2,dollarString)+","+SUBSTR(3,5,dollarString)+"."+centString;
      ELSE
         tempString:=dollarString+centString;
         lastDigitPos:=2+maxSize-5;
         IF lastDigitPos<STRLEN(tempString);
            outString:="$"+SUBSTR(1,2,tempString)+"."+SUBSTR(3,lastDigitPos,tempString)+"K";         
         ELSE
            outString:="$"+SUBSTR(1,2,tempString)+"."+SUBSTR(3,STRLEN(tempString),tempString)+"K";
         END IF;
      END IF;
   
   ELSIF dollarReal<1000000.     {$xxx,xxx.xx}
      fullLength:=11;
      IF fullLength<=maxSize
         outString:="$"+SUBSTR(1,3,dollarString)+","+SUBSTR(4,6,dollarString)+"."+centString;
      ELSE
         tempString:=dollarString+centString;
         lastDigitPos:=3+maxSize-6;
         IF lastDigitPos<STRLEN(tempString);
            outString:="$"+SUBSTR(1,3,tempString)+"."+SUBSTR(4,lastDigitPos,tempString)+"K";         
         ELSE
            outString:="$"+SUBSTR(1,3,tempString)+"."+SUBSTR(4,STRLEN(tempString),tempString)+"K";
         END IF;
      END IF;
      
   ELSIF dollarReal<10000000.    {$x,xxx,xxx.xx}
      fullLength:=13;
      IF fullLength<=maxSize
         outString:="$"+SUBSTR(1,1,dollarString)+","+SUBSTR(2,4,dollarString)+","+SUBSTR(5,7,dollarString)+"."+centString;
      ELSE
         tempString:=dollarString+centString;
         lastDigitPos:=1+maxSize-4;
         IF lastDigitPos<STRLEN(tempString);
            outString:="$"+SUBSTR(1,1,tempString)+"."+SUBSTR(2,lastDigitPos,tempString)+"M";         
         ELSE
            outString:="$"+SUBSTR(1,1,tempString)+"."+SUBSTR(2,STRLEN(tempString),tempString)+"M";
         END IF;
      END IF;
      
   ELSIF dollarReal<100000000.    {$xx,xxx,xxx.xx}
      fullLength:=14;
      IF fullLength<=maxSize
         outString:="$"+SUBSTR(1,2,dollarString)+","+SUBSTR(3,5,dollarString)+","+SUBSTR(6,8,dollarString)+"."+centString;
      ELSE
         tempString:=dollarString+centString;
         lastDigitPos:=2+maxSize-5;
         IF lastDigitPos<STRLEN(tempString);
            outString:="$"+SUBSTR(1,2,tempString)+"."+SUBSTR(3,lastDigitPos,tempString)+"M";         
         ELSE
            outString:="$"+SUBSTR(1,2,tempString)+"."+SUBSTR(3,STRLEN(tempString),tempString)+"M";
         END IF;
      END IF;
      
   ELSIF dollarReal<1000000000.    {$xxx,xxx,xxx.xx}
      fullLength:=15;
      IF fullLength<=maxSize
         outString:="$"+SUBSTR(1,3,dollarString)+","+SUBSTR(4,6,dollarString)+","+SUBSTR(7,9,dollarString)+"."+centString;
      ELSE
         tempString:=dollarString+centString;
         lastDigitPos:=3+maxSize-6;
         IF lastDigitPos<STRLEN(tempString);
            outString:="$"+SUBSTR(1,3,tempString)+"."+SUBSTR(4,lastDigitPos,tempString)+"M";         
         ELSE
            outString:="$"+SUBSTR(1,3,tempString)+"."+SUBSTR(4,STRLEN(tempString),tempString)+"M";
         END IF;
      END IF;
      
   ELSIF dollarReal<10000000000.    {$x,xxx,xxx,xxx.xx}
      fullLength:=17;
      IF fullLength<=maxSize
         outString:="$"+SUBSTR(1,1,dollarString)+","+SUBSTR(2,4,dollarString)+","+SUBSTR(5,7,dollarString)+","
                       +SUBSTR(8,10,dollarString)+"."+centString;
      ELSE
         tempString:=dollarString+centString;
         lastDigitPos:=1+maxSize-4;
         IF lastDigitPos<STRLEN(tempString);
            outString:="$"+SUBSTR(1,1,tempString)+"."+SUBSTR(2,lastDigitPos,tempString)+"B";         
         ELSE
            outString:="$"+SUBSTR(1,1,tempString)+"."+SUBSTR(2,STRLEN(tempString),tempString)+"B";
         END IF;
      END IF;
      
   ELSIF dollarReal<100000000000.    {$xx,xxx,xxx,xxx.xx}
      fullLength:=18;
      IF fullLength<=maxSize
         outString:="$"+SUBSTR(1,2,dollarString)+","+SUBSTR(3,5,dollarString)+","+SUBSTR(6,8,dollarString)+","
                       +SUBSTR(9,11,dollarString)+"."+centString;
      ELSE
         tempString:=dollarString+centString;
         lastDigitPos:=2+maxSize-5;
         IF lastDigitPos<STRLEN(tempString);
            outString:="$"+SUBSTR(1,2,tempString)+"."+SUBSTR(3,lastDigitPos,tempString)+"B";         
         ELSE
            outString:="$"+SUBSTR(1,2,tempString)+"."+SUBSTR(3,STRLEN(tempString),tempString)+"B";
         END IF;
      END IF;
      
   ELSIF dollarReal<1000000000000.    {$xxx,xxx,xxx.xx}
      fullLength:=19;
      IF fullLength<=maxSize
         outString:="$"+SUBSTR(1,3,dollarString)+","+SUBSTR(4,6,dollarString)+","+SUBSTR(7,9,dollarString)+","
                       +SUBSTR(10,12,dollarString)+"."+centString;
      ELSE
         tempString:=dollarString+centString;
         lastDigitPos:=3+maxSize-6;
         IF lastDigitPos<STRLEN(tempString);
            outString:="$"+SUBSTR(1,3,tempString)+"."+SUBSTR(4,lastDigitPos,tempString)+"B";         
         ELSE
            outString:="$"+SUBSTR(1,3,tempString)+"."+SUBSTR(4,STRLEN(tempString),tempString)+"B";
         END IF;
      END IF;
   ELSE
      fullLength:=100;
   END IF;
   IF negative
      outString:="("+outString+")";
   END IF;
   numChars:=STRLEN(outString);
   IF numChars<strSize
      numLeft:=strSize-numChars;
      IF ODD(numLeft)
         oddSpace:=" ";
         DEC(numLeft);
      ELSE
         oddSpace:="";
      END IF;
      IF numLeft=0
         spaces:="";
      ELSE
         spaces:=SUBSTR(1,numLeft DIV 2,"                                    ");
      END IF;
      IF justification=0     {left Justified}
         prefix:="";
         suffix:=spaces+spaces+oddSpace;
      ELSIF justification=1  {right Justified}
         prefix:=spaces+spaces+oddSpace;
         suffix:="";
      ELSIF justification=2  {centered}
         prefix:=spaces+oddSpace;
         suffix:=spaces;
      ELSIF justification=3  {none}
         prefix:="";
         suffix:="";         
      END IF;
      outVal:=prefix+outString+suffix;
   ELSE
      outVal:=outString;
   END IF;   
   RETURN outVal;
END PROCEDURE;     {REALTOCOST}

PROCEDURE COSTTOREAL(IN inVal :STRING):REAL;

VAR
  length,i,j          : INTEGER;
  nextChar,realString : STRING;  
  outVal,outReal      : REAL;
  negative            : BOOLEAN;
BEGIN
  length:=STRLEN(inVal);
  realString:="";
  FOR i:=1 TO length    
     nextChar:=SUBSTR(i,i,inVal);
     IF ((nextChar="$") OR (nextChar=",") OR (nextChar=" ") OR (nextChar=")"))
         {do nothing}
     ELSIF nextChar="K"
        outReal:=STRTOREAL(realString)*1000.0;
        realString:=REALTOSTR(outReal);
     ELSIF nextChar="M"
        outReal:=STRTOREAL(realString)*1000000.0;
        realString:=REALTOSTR(outReal);
     ELSIF nextChar="B"     
        outReal:=STRTOREAL(realString)*1000000000.0;
        realString:=REALTOSTR(outReal);
     ELSIF nextChar="("     
        negative:=TRUE;
     ELSE
        realString:=realString+nextChar;
     END IF;
  END FOR;
   outVal:=STRTOREAL(realString);
   IF negative
      outVal:=0.0-outVal;
   END IF;
   RETURN outVal;
END PROCEDURE;    {COSTTOREAL}

PROCEDURE PopToSample(IN sigmaPop :REAL; IN  n :INTEGER)  : REAL;
VAR
   sigmaSamp : REAL;
BEGIN
   sigmaSamp:=sigmaPop*SQRT(FLOAT(n)/FLOAT(n-1));
   RETURN sigmaSamp;
END PROCEDURE;

PROCEDURE GetDisplayStr(IN newReal : REAL; IN count,a,b,c : INTEGER; IN key : STRING) : STRING; 
{Formats a standard deviation or SEM for display}
VAR
   displayString : STRING;
   sigma         : REAL;
BEGIN
   displayString:="Hello Bootsy";
   IF count<2
      displayString:="N/A";
   ELSE   
      sigma:=PopToSample(newReal,count);
      IF key="stdev"
         displayString:=REALTOSN(sigma,a,b,c); 
      ELSIF key="sem"
         displayString:=REALTOSN(sigma/SQRT(FLOAT(count)),a,b,c);
      END IF;
   END IF;
   RETURN displayString;
END PROCEDURE;

PROCEDURE FormatStats(IN stat : REAL) : STRING;
VAR
   displayString  : STRING;
BEGIN 
  {tony:}
  {removed two spaces from the front of these strings 1-5-04 - why were they there?} 
  {corresponded to some spacing problems in EOR, is that all?}
  IF (stat=1.)
     displayString:="1.000000000";
  ELSE      
     IF (SUBSTR(1,1,INTTOSTR(ROUND((1.0+stat)*1000000000.0)))="2")      {cmc 6/20/07}
         displayString:="1.000000000";     
     ELSE   
       displayString:="0."+SUBSTR(2,10,INTTOSTR(ROUND((1.0+stat)*1000000000.0)));
     END IF
  END IF;      
  RETURN(displayString);
END PROCEDURE;

PROCEDURE CalculateInterimRams();
VAR
   SEP,R : REAL;
BEGIN

  ASK RamStream TO WriteString(SUBSTR(1,10,INTTOSTR(runNumber)+"          "));   
  ASK RamStream TO WriteString(FormatStats(AvailMon.Mean)+"  ");   
  ASK RamStream TO WriteString(GetDisplayStr(AvailMon.StdDev,AvailMon.Count,11,9,1,"stdev")+" ");
  ASK RamStream TO WriteString(GetDisplayStr(AvailMon.StdDev,AvailMon.Count,11,9,1,"sem")+" ");     {cmc 6/19/07}  
  ASK RamStream TO WriteString(FormatStats(DependMon.Mean)+"  ");   
  ASK RamStream TO WriteString(GetDisplayStr(DependMon.StdDev,DependMon.Count,11,9,1,"stdev")+" ");
  ASK RamStream TO WriteString(GetDisplayStr(DependMon.StdDev,DependMon.Count,11,9,1,"sem")+" ");    {cmc 6/19/07}
  ASK RamStream TO WriteString(FormatStats(RMon.Mean)+"  ");    
  ASK RamStream TO WriteString(GetDisplayStr(RMon.StdDev,RMon.Count,11,9,1,"stdev")+" ");
  ASK RamStream TO WriteString(GetDisplayStr(RMon.StdDev,RMon.Count,11,9,1,"sem")+" ");
  R:= RMon.Mean;
  SEP:= SQRT(  R*(1.0-R)/FLOAT(runNumber));
  ASK RamStream TO WriteReal(SEP,11,9);     
  ASK RamStream TO WriteString(" ");
  
  ASK RamStream TO WriteString(FormatStats(RCondMon.Mean)+"  "); 
  ASK RamStream TO WriteString(GetDisplayStr(RCondMon.StdDev,RCondMon.Count,11,9,1,"stdev")+" ");
  ASK RamStream TO WriteString(GetDisplayStr(RCondMon.StdDev,RCondMon.Count,11,9,1,"sem")+" ");
          
  IF (MTBDEMon.Count>0)
     ASK RamStream TO WriteReal(MTBDEMon.Mean,19,9);     
  ELSE
     ASK RamStream TO WriteString("                N/A ");
  END IF; 
  IF ((MTBDEMon.Count>1) AND (MDTMon.Count=runNumber))  {if some runs were suspension don't report StDev or SEM}
     ASK RamStream TO WriteString(" "+GetDisplayStr(MTBDEMon.StdDev,MTBDEMon.Count,19,9,1,"stdev")+" ");
     ASK RamStream TO WriteString(" "+GetDisplayStr(MTBDEMon.StdDev,MTBDEMon.Count,19,9,1,"sem")+" ");
  ELSE
     ASK RamStream TO WriteString("                N/A ");
     ASK RamStream TO WriteString("                N/A ");
  END IF;
 
  IF (MDTMon.Count>0)
     ASK RamStream TO WriteReal(MDTMon.Mean,19,9);   
  ELSE
     ASK RamStream TO WriteString("                N/A ");
  END IF;    
  IF (MDTMon.Count>1)
     ASK RamStream TO WriteString(" "+GetDisplayStr(MDTMon.StdDev,MDTMon.Count,19,9,1,"stdev")+" ");
     ASK RamStream TO WriteString(" "+GetDisplayStr(MDTMon.StdDev,MDTMon.Count,19,9,1,"sem")+" ");
  ELSE
     ASK RamStream TO WriteString("                N/A ");
     ASK RamStream TO WriteString("                N/A ");
  END IF; 
  
  ASK RamStream TO WriteString("  "); 
  ASK RamStream TO WriteInt(MDTMon.Count,9);
 
  IF (MTBMMon.Count>0) 
     ASK RamStream TO WriteReal(MTBMMon.Mean,19,9);
  ELSE
     ASK RamStream TO WriteString("                N/A ");
  END IF;  
  IF ((MTBMMon.Count>1) AND (MMTMon.Count=runNumber))
     ASK RamStream TO WriteString(" "+GetDisplayStr(MTBMMon.StdDev,MTBMMon.Count,19,9,1,"stdev")+" ");
     ASK RamStream TO WriteString(" "+GetDisplayStr(MTBMMon.StdDev,MTBMMon.Count,19,9,1,"sem")+" ");
  ELSE
     ASK RamStream TO WriteString("                N/A ");
     ASK RamStream TO WriteString("                N/A ");
  END IF;  
  
  IF (MMTMon.Count>0)
     ASK RamStream TO WriteReal(MMTMon.Mean,19,9);
  ELSE
     ASK RamStream TO WriteString("                N/A ");
  END IF;   
  IF (MMTMon.Count>1)
     ASK RamStream TO WriteString(" "+GetDisplayStr(MMTMon.StdDev,MMTMon.Count,19,9,1,"stdev")+" ");
     ASK RamStream TO WriteString(" "+GetDisplayStr(MMTMon.StdDev,MMTMon.Count,19,9,1,"sem")+" ");
  ELSE
     ASK RamStream TO WriteString("                N/A ");
     ASK RamStream TO WriteString("                N/A ");
  END IF;
  ASK RamStream TO WriteString("  "); 
  ASK RamStream TO WriteInt(MMTMon.Count,9);
  ASK RamStream TO WriteLn;      
END PROCEDURE;

PROCEDURE OutputToFR(IN inStr : STRING) : STRING;
VAR
  i                           : INTEGER;
  outStr,nextChar             : STRING;
BEGIN
  outStr:=SUBSTR(1,1,inStr);
  FOR i:=2 TO STRLEN(inStr)
     IF SUBSTR(i,i,inStr)=" "
        nextChar:="_";
     ELSE
        nextChar:=SUBSTR(i,i,inStr);
     END IF;
     outStr:=outStr+nextChar;
  END FOR;
  RETURN(outStr);
END PROCEDURE;

   
OBJECT PoolObj;
  ASK METHOD Initialize;   
     BEGIN
        UsedUp:=0;
        NumEmerOrdered:=0;
  END METHOD;   {Initialize}
  
  ASK METHOD IncrementUsedUp;   
     BEGIN
        UsedUp:=UsedUp+1;
  END METHOD;   {IncrementUsedUp}
            
  TELL METHOD GenerateSpares(IN i:INTEGER);   
     BEGIN
        LOOP
           WAIT DURATION PoolArray[i].newSparesArrival; 
           END WAIT;
           IncrementResourcesBy(PoolArray[i].newSpares);
           PoolResAvailable[i]:=Resources;
           currNumArrivals[i+totalBlocks]:=currNumArrivals[i+totalBlocks]+PoolArray[i].newSpares;
           IF costAnalysis
              PoolAddCost[i]:=PoolAddCost[i]+FLOAT(PoolArray[i].newSpares)*PoolArray[i].spareCost;
           END IF;
           IF EventsFile
              WriteEvents(SimTime,"Pool",PoolArray[i].poolName,"Sparing",
                 INTTOSTR(PoolArray[i].newSpares) + "_New_spare(s)_arrived_(stock="+INTTOSTR(PoolResAvailable[i])+")");
           END IF;
        END LOOP;
  END METHOD;   {GenerateSpares}
  
  TELL METHOD OrderASpare(IN i:INTEGER); 
     BEGIN 
        IF costAnalysis
           PoolAddCost[i]:=PoolAddCost[i]+PoolArray[i].spareCost;
           PoolESCost[i]:=PoolESCost[i]+PoolArray[i].emerShippingCost;
        END IF;
        IF EventsFile
         WriteEvents(SimTime,"Pool",PoolArray[i].poolName,
             "Sparing","Spare_ordered_(stock="+INTTOSTR(PoolResAvailable[i])+")");
        END IF;
        WAIT DURATION PoolArray[i].emergencyTime; 
        END WAIT; 
        IncrementResourcesBy(1);  
        PoolResAvailable[i]:=Resources; 
        currNumArrivals[i+totalBlocks]:=currNumArrivals[i+totalBlocks]+1;
        IF EventsFile
           WriteEvents(SimTime,"Pool",PoolArray[i].poolName,"Sparing","Ordered_spare_arrives_(stock="+INTTOSTR(PoolResAvailable[i])+")");
        END IF;
  END METHOD;   {OrderASpare}
  
  ASK METHOD IncNumEmerOrdered;
  BEGIN
     INC(NumEmerOrdered);
  END METHOD;

  ASK METHOD DecNumEmerOrdered;
  BEGIN
     DEC(NumEmerOrdered);
  END METHOD;
  
  TELL METHOD OrderStock(IN i, quantity : INTEGER); 
     BEGIN 
        IF costAnalysis
           PoolAddCost[i]:=PoolAddCost[i]+
             PoolArray[i].spareCost*FLOAT(quantity);
        END IF;
        IF EventsFile
           WriteEvents(SimTime,"Pool",PoolArray[i].poolName,"Sparing",
              INTTOSTR(quantity)+"_Spare(s)_ordered_(stock="+INTTOSTR(PoolResAvailable[i])+")");   
        END IF;
        WAIT DURATION PoolArray[i].SLOTime; 
        END WAIT; 
        IncrementResourcesBy(quantity);  
        PoolResAvailable[i]:=Resources; 
        currNumArrivals[i+totalBlocks]:=currNumArrivals[i+totalBlocks]+quantity;
        IF EventsFile
           WriteEvents(SimTime,"Pool",PoolArray[i].poolName,"Sparing",
                INTTOSTR(quantity)+"_Spare(s)_arrived_(stock="+INTTOSTR(PoolResAvailable[i])+")");
        END IF;
  END METHOD;   {OrderStock}  
END OBJECT;

OBJECT SystemObj; 
  ASK METHOD Initialize;
     BEGIN
        Status:=GGreen;
        redCost:=systemRedCost;
  END METHOD; 

  ASK METHOD SetToInitValues;
     VAR
        phase                                 :   PhaseObj;
     BEGIN
        Status:=GGreen;
        SystemUp:=TRUE;
        TotalRedCost:=0.0;
        missionsCompleted:=0;
        missionsBegan:=0;         {tony what the hell?}
        missionSuccess:=TRUE;
        missionsBegan:=1;         {tony what the hell?}
        missionsAttempted:=1;
        IF (activePhases>0)
           phase:=phaseObjArray[1];
           IF phase.mission
              missionStatus:=Mission;
           ELSE
              missionStatus:=NonMission;
              missionsAttempted:=0;
              missionsBegan:=0;
           END IF;
        ELSE
           missionStatus:=Mission;
        END IF;
  END METHOD; 
    
  ASK METHOD ChangeStatus(IN newColor : SystemStatusType; IN newStatus : MissionStatusType);
  {color refers to systemStatus, while status refers to missionStatus}
     VAR
        statusString                                  : STRING;     
        colorTime,missionTime                         : REAL;
        oldColor                                      : SystemStatusType;
        oldStatus                                     : MissionStatusType;
        block                                         : RBDBlockObj;
        event                                         : RBDEventObj;
        node                                          : RBDNodeObj;
     BEGIN
        oldColor:=Status;
        oldStatus:=missionStatus;
        IF (oldColor<>newColor)     
           Status:=newColor;
        END IF;        
        IF ((oldColor=newColor) AND (oldStatus=newStatus))
           NEW(message,1..1);
           message[1]:="Error in system ChangeStatus!     "; 
           result:=SendAlert(message,FALSE, FALSE, TRUE);
           DISPOSE(message); 
        END IF;
        IF NOT FEVmode  
           IF (oldColor<>newColor)     
              colorTime:=SimTime-LastColorChange;
              CASE oldColor
                 WHEN GGreen:
                    GreenTime:=colorTime;
                 WHEN YYellow:
                    YellowTime:=colorTime;
                 WHEN RRed:         
                    RedTime:=colorTime;
                    TotalRedCost:=TotalRedCost+redCost*colorTime;
                 OTHERWISE             
                    NEW(message,1..1);
                    message[1]:="Unknown system status color!     "; 
                    result:=SendAlert(message,FALSE, FALSE, TRUE);
                    DISPOSE(message);   
              END CASE;     
              LastColorChange:=SimTime;  
              IF (Status=RRed)
                 IF ((missionStatus=Mission) AND (missionSuccess))
                    missionSuccess:=FALSE;
                    IF (relMesh>0.0)
                       RecordReliabilityFileData;
                    END IF;   
                 END IF;   
                 IF  ((SimTime=0.0) AND dZeroFail)
                    CountFailure:=FALSE;
                 ELSE
                    SysTimeToFail:=SimTime-LastUpDownChange; 
                    IF SysFailTimeFile AND StatsStarted 
                       ASK FailTimeStream TO WriteReal(SysTimeToFail,19,9);
                       ASK FailTimeStream TO WriteLn;  
                    END IF;
                 END IF;
                 LastUpDownChange:=SimTime;
                 IF (TieBreaking=TRUE) 
                    ASK EventController TO SetTieBreaking(FALSE);  
                    TieBreaking:=FALSE; 
                 END IF; 
              END IF;
           END IF;     {oldColor<>newColor}     
           IF ((oldStatus<>newStatus) OR (oldColor<>newColor))
              missionStatus:=newStatus;
              missionTime:=SimTime-LastMissionChange;
              LastMissionChange:=SimTime;
              IF oldStatus=Mission
                 CASE oldColor
                    WHEN GGreen:
                       MissionGrnTime:=missionTime;
                    WHEN YYellow:
                       MissionYelTime:=missionTime;
                    WHEN RRed:         
                       MissionRedTime:=missionTime;
                    OTHERWISE             
                       NEW(message,1..1);
                       message[1]:="Unknown system status color!     "; 
                       result:=SendAlert(message,FALSE, FALSE, TRUE);
                       DISPOSE(message);   
                 END CASE;     
              END IF;    {oldStatus=Mission}
              IF ((oldStatus=NonMission) AND (newStatus=Mission))   {beginning a mission}
                 INC(missionsAttempted);
                 IF newColor<>RRed
                    missionSuccess:=TRUE;
                    INC(missionsBegan);
                 ELSE
                    missionSuccess:=FALSE;
                 END IF; 
                 IF weakAnalysis
                    FOREACH block IN blockGroup
                       IF ((block.opStatus=Repairing) OR (block.opStatus=RepHold) 
                              OR (block.opStatus=PM) OR (block.opStatus=PMhold) OR 
                              (block.opStatus=Done))
                          blockMissionBool[block.seqNum]:=FALSE;
                       ELSE
                          blockMissionBool[block.seqNum]:=TRUE;
                       END IF;       
                    END FOREACH;
                    FOREACH event IN eventGroup
                       IF (event.opStatus=Failure)
                          eventMissionBool[event.seqNum]:=FALSE;
                       ELSE
                          eventMissionBool[event.seqNum]:=TRUE;
                       END IF;       
                    END FOREACH;
                    FOREACH node IN nodeGroup
                       IF ((node.Status=Down) OR (node.Status=NodePM))
                          nodeMissionBool[node.seqNum]:=FALSE;
                       ELSE
                          nodeMissionBool[node.seqNum]:=TRUE;
                       END IF;       
                    END FOREACH;
                 END IF; {weakAnalysis} 
                 IF EventsFile
                    WriteEvents(SimTime,"System","Phase_change","Mission","Began_a_mission");
                 END IF;   
              END IF;
              IF ((oldStatus=Mission) AND (newStatus=NonMission))     {finished a mission}
                 IF missionSuccess
                    INC(missionsCompleted);
                    statusString:="Mission_success";
                 ELSE
                    statusString:="Mission_failure";
                 END IF;  
                 IF weakAnalysis
                    FOREACH block IN blockGroup
                       IF (blockMissionBool[block.seqNum]=TRUE)
                          INC(blockMCompleted[block.seqNum]);
                       END IF;       
                    END FOREACH;
                    FOREACH event IN eventGroup
                       IF (eventMissionBool[event.seqNum]=TRUE)
                          INC(eventMCompleted[event.seqNum]);
                       END IF;       
                    END FOREACH;
                    FOREACH node IN nodeGroup
                       IF (nodeMissionBool[node.seqNum]=TRUE)
                          INC(nodeMCompleted[node.seqNum]);
                       END IF;       
                   END FOREACH;
                 END IF; {weakAnalysis}   
                 IF EventsFile
                    WriteEvents(SimTime,"System","Phase_change",statusString,"Finished_a_mission");
                 END IF;   
              END IF;
           END IF;     {(oldStatus<>newStatus) OR (oldColor<>newColor)}
        END IF;   
        IF ((statusBarOn) AND (activePhases>0))
           IF (((oldStatus=Mission) AND (newStatus=NonMission)) OR ((oldStatus=NonMission) AND (newStatus=Mission)))  
             IF newStatus=Mission
                ASK window TO ShowStatus(4,"MISSION");  
             ELSE
                ASK window TO ShowStatus(4,"Nonmission");  
             END IF;          
           END IF;
        END IF;
        IF GraphicsOutput AND faceBoxOpen 
           CASE Status  
              WHEN GGreen:
                 ASK window TO DisplayFace(1);  
              WHEN YYellow:
                 ASK window TO DisplayFace(2);
              WHEN RRed:
                 ASK window TO DisplayFace(3);
              WHEN DDone:
                 ASK window TO DisplayFace(1);
              OTHERWISE
           END CASE; 
        END IF;
        IF ((EventsFile) AND (System.Status<>DDone)) 
           redString:="";
           FOREACH block IN blockGroup
              IF block.activeStatus=Cut
                 redString:=redString+"_c"+INTTOSTR(block.Id); 
              ELSE                            
                 IF block.opStatus=Repairing 
                    redString:=redString+"_d"+INTTOSTR(block.Id); 
                 ELSIF block.opStatus=RepHold
                    redString:=redString+"_d"+INTTOSTR(block.Id); 
                 ELSIF block.opStatus=Done
                    redString:=redString+"_d"+INTTOSTR(block.Id); 
                 END IF;
              END IF;              
           END FOREACH;
           CASE Status
              WHEN GGreen:
                 statusString:="Green";
              WHEN YYellow:
                 statusString:="Yellow";
              WHEN RRed:
                 statusString:="Red";   
            END CASE; 
           IF (redString<>"")
              WriteEvents(SimTime,"System","State_change",statusString,redString);
           ELSE 
              WriteEvents(SimTime,"System","State_change",statusString,"system_change_status");
           END IF;   
        END IF;    {EventsFile}
        IF ((newColor=DDone) AND (missionSuccess))
           IF (relMesh>0.0)
              RecordReliabilityFileData;
           END IF;   
        END IF;            
  END METHOD;     {system:  ChangeStatus}
  
  
TELL METHOD ResetAllStatistics();
  VAR
     i      : INTEGER; 
     block  : RBDBlockObj;
     event  : RBDEventObj;
     node   : RBDNodeObj;
     hier   : RBDHierObj;
  BEGIN 
     StatsStarted:=TRUE;
     StatsStartTime:=SimTime;
     IF EventsFile 
        ASK EventStream TO WriteLn;               
        ASK EventStream TO WriteString(REALTOSTR(SimTime)+"  Started Gathering Statistics"); 
        ASK EventStream TO WriteLn;           
     END IF;   
     LastColorChange:=SimTime;
     LastUpDownChange:=SimTime;
     TimeOfCompFailure:=SimTime; 
     ASK SysFailMon TO Reset;
     ASK SysRepMon TO Reset;
     ASK CompFailMon TO Reset;
     ASK CompRepairMon TO Reset;
     ASK GreenMon TO Reset;
     ASK YellowMon TO Reset;
     ASK RedMon TO Reset;
     ASK MissionGrnMon TO Reset;
     ASK MissionYelMon TO Reset;
     ASK MissionRedMon TO Reset;
     cyclesCount:=-1.0;   {tony 4-22 set to -1.0 because ControlPhaseChange occurs AFTER this procedure}
     missionsCompleted:=0;
     missionsBegan:=0;
     IF Status<>RRed
        missionsBegan:=1;
        missionSuccess:=TRUE;
     END IF; 
     LastMissionChange:=SimTime;
     missionsAttempted:=1;
     IF (activePhases>0)
        IF System.missionStatus=NonMission
           missionsAttempted:=0;
           missionsBegan:=0;
        END IF;
     END IF;
     FOREACH node IN nodeGroup
        i:=node.seqNum;
        ASK node TO ResetStateStart;
        IF (weakAnalysis)
           NodeGreenTime[i]:=0.0;
           NodeYellowTime[i]:=0.0;
           NodeRedTime[i]:=0.0; 
           NodeBlueTime[i]:=0.0;
           NodeBrownTime[i]:=0.0;
           NodeOrangeTime[i]:=0.0;
           IF ((node.Status=Down) OR (node.Status=NodePM))
              nodeMissionBool[i]:=FALSE;
           ELSE
              nodeMissionBool[i]:=TRUE;
           END IF;  
           nodeMCompleted[i]:=0;
           IF (node.typeNode=5)
              hier := ASK root Child("RBDHier",node.parentID);
              hierDE[hier.seqNum]:=0;
           END IF;
        END IF;
        IF (capacityAnalysis)
           ASK NodeFlowMon[i] TO Reset;
           ASK NodeCapMon[i] TO Reset;        
           NodeFlow[i]:=NodeFlow[i];  {used to initialize a value for the monitor}
           NodeCapacity[i]:=NodeCapacity[i];  {same}      
        END IF;
     END FOREACH;
     FOREACH block IN blockGroup
        i:=block.seqNum;
        SparesUsed[i]:=0;
        ASK block TO ResetStateStart;
        ASK block TO ResetStats;
        IF (SparesTrackingGroup.Includes(block)) 
           ASK CustSpareMon[i] TO Reset;
           currNumWaits[i]:=0;
           currNumArrivals[i]:=0;
           currInitNumber[i]:=CustomSpares[i].Resources;
        END IF;
        IF costAnalysis
           BlockOpCost[i]:=0.0;
           BlockRepCost[i]:=0.0;
           BlockIdleCost[i]:=0.0;
           BlockHoldCost[i]:=0.0;
           BlockPMCost[i]:=0.0;
           BlockpmHoldCost[i]:=0.0;
           BlockSBCost[i]:=0.0;
           BlockDoneCost[i]:=0.0;
           BlockDispCost[i]:=0.0;
           BlockSpCost[i]:=0.0;
           BlockESCost[i]:=0.0;
        END IF;
        IF weakAnalysis
           BlockRunTime[i]:=0.0;
           BlockStandbyTime[i]:=0.0;
           BlockIdleTime[i]:=0.0;
           BlockRepairTime[i]:=0.0;
           BlockRepHoldTime[i]:=0.0;
           BlockPMTime[i]:=0.0;
           BlockPMHoldTime[i]:=0.0;
           BlockDoneTime[i]:=0.0;
           IF ((block.opStatus=Repairing) OR (block.opStatus=RepHold) OR (block.opStatus=PM) OR (block.opStatus=PMhold) 
                      OR (block.opStatus=Done))
              blockMissionBool[i]:=FALSE;
           ELSE
              blockMissionBool[i]:=TRUE;
           END IF;       
           blockMCompleted[i]:=0;
        END IF;
     END FOREACH;
     FOREACH event IN eventGroup
        i:=event.seqNum;
        ASK event TO ResetStateStart;
        ASK event TO ResetStats;
        IF costAnalysis
           EventOpCost[i]:=0.0;
           EventRepCost[i]:=0.0;
        END IF;
        IF weakAnalysis
           EventRunTime[i]:=0.0;
           EventStandbyTime[i]:=0.0;
           EventRepairTime[i]:=0.0;
           IF (event.opStatus=Failure)
              eventMissionBool[i]:=FALSE;
           ELSE
              eventMissionBool[i]:=TRUE;
           END IF;       
           eventMCompleted[i]:=0;
        END IF;
     END FOREACH;
     IF (totalPools>0)        
        FOR i:= 1 TO totalPools 
           ASK PoolMon[i] TO Reset;
           currNumWaits[i+totalBlocks]:=0;
           currNumArrivals[i+totalBlocks]:=0;
           currInitNumber[i+totalBlocks]:=PooledSpares[i].Resources;
           IF costAnalysis
              PoolAddCost[i]:=0.0;
              PoolESCost[i]:=0.0;        
           END IF;
        END FOR;
     END IF;
     IF costAnalysis
        ZeroInitCost:=TRUE;
        TotalRedCost:=0.0;
     END IF;
     StatsReset:=TRUE;
END METHOD;     {ResetAllStatistics}

TELL METHOD CalculateInterimCost();
VAR
  i,j,k                                                                               : INTEGER;
  ic,oc,rc,csc,hc,idc,dc,spc,ac,esc,sic,dfc,tc,
  {init,op,rep,cold,hold,idle,done,spare,add,emerShip,spInit,disposal}
  bict,boct,brct,bcsct,bhct,bidct,bdct,bspct,besct,bsict,bdfct,btoct,btsct,btct,    
  eict,eoct,efct,etct,pict,pact,pesct,ptct,puc,ptc,rict,rpuct,rptct,rtct,stateTime,
  pmc,pmhc,bpmct,bpmhct,sysIntRedCost,IntTotal,AveLostFlow,sysIntLostCost             : REAL;
  OperatingCost,SparingCost,idleCostArray,standbyCostArray,holdCostArray,repCostArray,
  pmCostArray,pmHoldCostArray,opCostArray,doneCostArray,poolNewCost,poolShipCost      : ARRAY INTEGER OF REAL; 
  block                                                                               : RBDBlockObj;
  event                                                                               : RBDEventObj;
  hier,papaHier                                                                       : RBDHierObj;
  HierCosts                                                                           : ARRAY INTEGER, INTEGER OF REAL;
BEGIN
  LOOP
     WAIT DURATION CostPlotInterval; 
     END WAIT;
     NEW(idleCostArray,1..totalBlocks);
     NEW(standbyCostArray,1..totalBlocks);
     NEW(holdCostArray,1..totalBlocks);
     NEW(repCostArray,1..totalBlocks);
     NEW(opCostArray,1..totalBlocks);
     NEW(doneCostArray,1..totalBlocks);
     NEW(pmCostArray,1..totalBlocks);
     NEW(pmHoldCostArray,1..totalBlocks);
     IF (totalPools>0)
        NEW(poolNewCost,1..totalPools);
        NEW(poolShipCost,1..totalPools);  
        FOR i:=1 TO totalPools
           poolNewCost[i]:=PoolAddCost[i];
           poolShipCost[i]:=PoolESCost[i];
        END FOR;
     END IF;
     bict:=0.0;boct:=0.0;brct:=0.0;bcsct:=0.0;bhct:=0.0;bidct:=0.0;
     bdct:=0.0;bspct:=0.0;besct:=0.0;bsict:=0.0;bdfct:=0.0;btoct:=0.0;
     btsct:=0.0;btct:=0.0;eict:=0.0;eoct:=0.0;efct:=0.0;etct:=0.0;
     pict:=0.0;pact:=0.0;pesct:=0.0;ptct:=0.0;puc:=0.0;ptc:=0.0;
     rict:=0.0;rpuct:=0.0;rptct:=0.0;rtct:=0.0;pmc:=0.0;pmhc:=0.0;bpmct:=0.0;
     bpmhct:=0.0;
     FOREACH block IN blockGroup
        i:=block.seqNum;
        idleCostArray[i]:=BlockIdleCost[i];
        standbyCostArray[i]:=BlockSBCost[i];
        holdCostArray[i]:=BlockHoldCost[i];
        repCostArray[i]:=BlockRepCost[i];
        opCostArray[i]:=BlockOpCost[i];
        pmCostArray[i]:=BlockPMCost[i];
        pmHoldCostArray[i]:=BlockpmHoldCost[i];
        doneCostArray[i]:=0.0;  
        stateTime:=SimTime-block.stateStartTime;
        IF block.opStatus=Idle                   
           idleCostArray[i]:=idleCostArray[i]+block.idleCost*stateTime;              
        ELSIF block.opStatus=RepHold
           holdCostArray[i]:=holdCostArray[i]+block.repHoldCost*stateTime; 
        ELSIF block.opStatus=Repairing
           repCostArray[i]:=repCostArray[i]+block.repairingCost*stateTime+
           block.repFixedCost;
           IF block.numDiffRes=1                                 
              poolNewCost[block.resPoolNum]:=poolNewCost[block.resPoolNum]+
              stateTime*PoolArray[block.resPoolNum].costPerTime*
              FLOAT(block.numRes1); 
           END IF;              
        ELSIF block.opStatus=PMhold
           pmHoldCostArray[i]:=pmHoldCostArray[i]+block.pmHoldCost*stateTime; 
        ELSIF block.opStatus=PM
           pmCostArray[i]:=pmCostArray[i]+block.pmCost*stateTime+
           block.pmFixedCost;
           IF block.numDiffRes=1                                 
              poolNewCost[block.resPoolNum]:=poolNewCost[block.resPoolNum]+
              stateTime*PoolArray[block.resPoolNum].costPerTime*
              FLOAT(block.numRes1PM); 
           END IF;              
               {spare costs charged when repair starts}    
        ELSIF block.opStatus=Running
           IF block.activeStatus=Active
              opCostArray[i]:=opCostArray[i]+block.operatingCost*stateTime;              
           ELSIF block.activeStatus=Linked
              standbyCostArray[i]:=standbyCostArray[i]+block.standbyCost*stateTime;               
           ELSE 
              idleCostArray[i]:=idleCostArray[i]+block.idleCost*stateTime;              
           END IF;
        ELSIF block.opStatus=Standby
           standbyCostArray[i]:=standbyCostArray[i]+block.standbyCost*stateTime;                                
        ELSIF (block.opStatus=Done) 
           doneCostArray[i]:=block.doneCost*stateTime; 
        END IF;
     END FOREACH;
     sysIntRedCost:=TotalRedCost;
     IF System.Status=RRed
        stateTime:=SimTime-LastColorChange;
        sysIntRedCost:=sysIntRedCost+System.redCost*stateTime;
     END IF;
     IF capacityAnalysis                    
     {   NodeFlow[startId]:=NodeFlow[startId];   tony 3-04 what in the hell would this do?}
        AveLostFlow:=FLOAT(flowGenerated)-NodeFlowMon[startTag].Mean;
        sysIntLostCost:=AveLostFlow*(SimTime-StatsStartTime)*sysLostCost;
     ELSE
        sysIntLostCost:=0.0;
     END IF;     
     ASK CostStream TO WriteString("SimTime=  ");
     ASK CostStream TO WriteReal(SimTime,16,6);
     ASK CostStream TO WriteLn;
     ASK CostStream TO WriteLn;
     IF totalBlocks>0  
        NEW(OperatingCost,1..totalBlocks);
        NEW(SparingCost,1..totalBlocks);
        ASK CostStream TO WriteString("Operating");
        ASK CostStream TO WriteLn;
        ASK CostStream TO WriteLn;
        ASK CostStream TO WriteString("Block                 InHier        Good      Repair        Idle"+
                                               "     RepHold          PM      PMHold     Standby        Done");
        ASK CostStream TO WriteLn;
        IF (totalHiers>0)
           IF (HierCosts<>NILARRAY)
              DISPOSE(HierCosts);
           END IF;
           NEW(HierCosts,1..totalHiers,1..6);
        END IF;
        FOR i:=1 TO totalBlocks
           block := ASK root Child("RBDBlock", BlockAlphaSort[i]);
           j:=block.seqNum;
           oc:=opCostArray[j];;
           rc:=repCostArray[j];
           idc:=idleCostArray[j];
           hc:=holdCostArray[j];
           csc:=standbyCostArray[j];
           dc:=doneCostArray[j];
           pmc:=pmCostArray[j];
           pmhc:=pmHoldCostArray[j];
           OperatingCost[j]:=oc+rc+idc+hc+csc+dc+pmc+pmhc;
           boct:=boct+oc;
           brct:=brct+rc;
           bidct:=bidct+idc;
           bhct:=bhct+hc;
           bcsct:=bcsct+csc;
           bdct:=bdct+dc;
           bpmct:=bpmct+pmc;
           bpmhct:=bpmhct+pmhc;
           ASK CostStream TO WriteString(SUBSTR(1,20,block.name + "                    ")+" "); 
           ASK CostStream TO WriteInt(block.parentID,4);
           ASK CostStream TO WriteString("    ");
           ASK CostStream TO WriteString(REALTOSN(oc,11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(rc,11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(idc,11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(hc,11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(pmc,11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(pmhc,11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(csc,11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(dc,11,2,1)+" ");
           ASK CostStream TO WriteLn;
        END FOR;
        ASK CostStream TO WriteLn;
        ASK CostStream TO WriteString("Totals                       ");
        ASK CostStream TO WriteString(REALTOSN(boct,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(brct,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(bidct,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(bhct,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(bpmct,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(bpmhct,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(bcsct,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(bdct,11,2,1)+" ");
        ASK CostStream TO WriteLn; 
        ASK CostStream TO WriteLn;
        ASK CostStream TO WriteString("Sparing");
        ASK CostStream TO WriteLn;
        ASK CostStream TO WriteLn;
        ASK CostStream TO WriteString
          ("Block                 InitSpares   NewSpares    EmerShip");
        ASK CostStream TO WriteLn;
        FOR i:=1 TO totalBlocks
           block := ASK root Child("RBDBlock", BlockAlphaSort[i]);
           j:=block.seqNum;
           IF ((block.sparingType=Custom) AND (NOT ZeroInitCost))
              sic:=FLOAT(block.initStock)*block.spareCost; 
           ELSE
              sic:=0.0;
           END IF;
           spc:=BlockSpCost[j];
           esc:=BlockESCost[j];
           bsict:=bsict+sic;
           bspct:=bspct+spc;
           besct:=besct+esc;
           SparingCost[j]:=sic+spc+esc;
           IF (block.sparingType<>SparePool)
              ASK CostStream TO WriteString(SUBSTR(1,20,block.name + "                    ")+" "); 
              ASK CostStream TO WriteString(REALTOSN(sic,11,2,1)+" ");
              ASK CostStream TO WriteString(REALTOSN(spc,11,2,1)+" ");
              ASK CostStream TO WriteString(REALTOSN(esc,11,2,1)+" ");
              ASK CostStream TO WriteLn; 
           END IF;
        END FOR;
        ASK CostStream TO WriteLn;
        ASK CostStream TO WriteString("Totals               ");
        ASK CostStream TO WriteString(REALTOSN(bsict,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(bspct,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(besct,11,2,1)+" ");
        ASK CostStream TO WriteLn;         
        ASK CostStream TO WriteLn;         
        ASK CostStream TO WriteString("Life_cycle_cost");
        ASK CostStream TO WriteLn;
        ASK CostStream TO WriteLn;
        ASK CostStream TO WriteString("Block                   Purchase   Operating     Sparing    Disposal       Total");
        ASK CostStream TO WriteLn;
        FOR i:=1 TO totalBlocks
           block := ASK root Child("RBDBlock", BlockAlphaSort[i]);
           j:=block.seqNum;
           IF ZeroInitCost
              ic:=0.0;
           ELSE
              ic:=block.initialCost;
           END IF;
           dfc:=0.0;
           tc:=ic+OperatingCost[j]+SparingCost[j]+dfc;
           bict:=bict+ic;
           btoct:=btoct+OperatingCost[j];
           btsct:=btsct+SparingCost[j];
           bdfct:=bdfct+dfc;
           btct:=btct+tc;
           IF ((totalHiers>0) AND (block.parentID>0))
              hier := ASK root Child("RBDHier",block.parentID);
              HierCosts[hier.seqNum,1]:=HierCosts[hier.seqNum,1]+ic;
              HierCosts[hier.seqNum,2]:=HierCosts[hier.seqNum,2]+OperatingCost[j];
              HierCosts[hier.seqNum,3]:=HierCosts[hier.seqNum,3]+SparingCost[j];
              HierCosts[hier.seqNum,4]:=HierCosts[hier.seqNum,4]+dfc;
              HierCosts[hier.seqNum,6]:=HierCosts[hier.seqNum,6]+tc;
           END IF;
           ASK CostStream TO WriteString(SUBSTR(1,20,block.name + "                    ")+" "); 
           ASK CostStream TO WriteString(REALTOSN(ic,11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(OperatingCost[j],11,2,1)+" ");
           IF (block.sparingType<>SparePool)
              ASK CostStream TO WriteString(REALTOSN(SparingCost[j],11,2,1)+" ");;
           ELSE
              ASK CostStream TO WriteString("        N/A ");
           END IF;   
           ASK CostStream TO WriteString(REALTOSN(dfc,11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(tc,11,2,1)+" ");
           ASK CostStream TO WriteLn;
        END FOR;
        ASK CostStream TO WriteLn;
        ASK CostStream TO WriteString("Block_Totals         ");
        ASK CostStream TO WriteString(REALTOSN(bict,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(btoct,11,2,1)+" ");  {total Operating Cost total}
        ASK CostStream TO WriteString(REALTOSN(btsct,11,2,1)+" ");  {total Sparing Cost total}
        ASK CostStream TO WriteString(REALTOSN(bdfct,11,2,1)+" "); {done fixed cost total(Disposal)}
        ASK CostStream TO WriteString(REALTOSN(btct,11,2,1)+" "); {done fixed cost total(Disposal)}
        ASK CostStream TO WriteLn;         
        ASK CostStream TO WriteLn;         
        ASK CostStream TO WriteString("________________________________________________________________________________");
        ASK CostStream TO WriteLn;         
        ASK CostStream TO WriteLn;
        DISPOSE(OperatingCost);
        DISPOSE(SparingCost);
     END IF;  {op Blocks exist}      
     IF totalEvents>0
        ASK CostStream TO WriteString("Event                 InHier     Initial     Success     Failure       Total");
        ASK CostStream TO WriteLn;
        FOR i:=1 TO totalEvents
           event := ASK root Child("RBDEvent", EventAlphaSort[i]);
           j:=event.seqNum;
           IF ZeroInitCost
              ic:=0.0;
           ELSE
              ic:=event.initialCost;
           END IF;
           oc:=EventOpCost[j];
           rc:=EventRepCost[j];
           tc:= ic+oc+rc;
           eict:=eict+ic;   
           eoct:=eoct+oc;
           efct:=efct+rc;
           etct:=etct+tc;
           IF ((totalHiers>0) AND (event.parentID>0))
              hier := ASK root Child("RBDHier",event.parentID);
              HierCosts[hier.seqNum,5]:=HierCosts[hier.seqNum,5]+tc;
              HierCosts[hier.seqNum,6]:=HierCosts[hier.seqNum,6]+tc;
           END IF;
           ASK CostStream TO WriteString(SUBSTR(1,20,event.name + "                    ")+" "); 
           ASK CostStream TO WriteInt(event.parentID,4);
           ASK CostStream TO WriteString("    ");
           ASK CostStream TO WriteString(REALTOSN(ic,11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(oc,11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(rc,11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(tc,11,2,1)+" ");
           ASK CostStream TO WriteLn; 
        END FOR;
        ASK CostStream TO WriteLn;
        ASK CostStream TO WriteString("Event_Totals                 ");
        ASK CostStream TO WriteString(REALTOSN(eict,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(eoct,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(efct,11,2,1)+" ");
        ASK CostStream TO WriteString(REALTOSN(etct,11,2,1)+" ");
        ASK CostStream TO WriteLn;         
        ASK CostStream TO WriteLn;         
        ASK CostStream TO WriteString("________________________________________________________________________________");
        ASK CostStream TO WriteLn;         
        ASK CostStream TO WriteLn;  
     END IF;
     IF (totalHiers>0)
        FOR i:=totalHiers DOWNTO 1
           hier:=ASK root Child("RBDHier",HierAlphaSort[i]);    {tony 3-26 this relies on higher numbers coming first to get correct totals}
           IF (hier.parentID<>0)
              papaHier := ASK root Child("RBDHier",hier.parentID);
              HierCosts[papaHier.seqNum,1]:=HierCosts[papaHier.seqNum,1]+HierCosts[i,1];
              HierCosts[papaHier.seqNum,2]:=HierCosts[papaHier.seqNum,2]+HierCosts[i,2];
              HierCosts[papaHier.seqNum,3]:=HierCosts[papaHier.seqNum,3]+HierCosts[i,3];
              HierCosts[papaHier.seqNum,4]:=HierCosts[papaHier.seqNum,4]+HierCosts[i,4];
              HierCosts[papaHier.seqNum,5]:=HierCosts[papaHier.seqNum,5]+HierCosts[i,5];
              HierCosts[papaHier.seqNum,6]:=HierCosts[papaHier.seqNum,6]+HierCosts[i,6];
           END IF;
        END FOR;
        ASK CostStream TO WriteLn;
        ASK CostStream TO WriteString("Hierarchy            HierNum    Purchase   Operating     Sparing    Disposal      Events       Total");
        ASK CostStream TO WriteLn;
        FOR i:=1 TO totalHiers      
           hier:=ASK root Child("RBDHier",HierAlphaSort[i]);
           ASK CostStream TO WriteString(SUBSTR(1,20,hier.name + "                    ")+" "); 
           ASK CostStream TO WriteInt(hier.Id,4);
           ASK CostStream TO WriteString("    ");
           ASK CostStream TO WriteString(REALTOSN(HierCosts[i,1],11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(HierCosts[i,2],11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(HierCosts[i,3],11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(HierCosts[i,4],11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(HierCosts[i,5],11,2,1)+" ");
           ASK CostStream TO WriteString(REALTOSN(HierCosts[i,6],11,2,1)+" ");
           ASK CostStream TO WriteLn; 
        END FOR;      
        ASK CostStream TO WriteLn;         
        ASK CostStream TO WriteLn;         
        ASK CostStream TO WriteString
            ("____________________________________________________________________________________________________");
        ASK CostStream TO WriteLn;                    
        ASK CostStream TO WriteLn; 
     END IF;  {totalHiers>0}
      IF (totalPools>0)
         IF totalSpares>0
            ASK CostStream TO WriteString
           ("SparePools               Initial   NewSpares    EmerShip       Total");
            ASK CostStream TO WriteLn; 
            FOR i:=1 TO totalPools
               IF PoolArray[i].sparingType=SparePool
                  IF ZeroInitCost
                     ic:=0.0;
                  ELSE
                     ic:=FLOAT(PoolArray[i].initialSpares)*PoolArray[i].spareCost;
                  END IF;
                  ac:=poolNewCost[i];
                  esc:=poolShipCost[i];
                  tc:= ic+ac+esc;
                  pict:=pict+ic;
                  pact:=pact+ac;
                  pesct:=pesct+esc;
                  ptct:=ptct+tc;
                  ASK CostStream TO WriteString(SUBSTR(1,20,PoolArray[i].poolName + "                    ")+" ");
                  ASK CostStream TO WriteString(REALTOSN(ic,11,2,1)+" ");
                  ASK CostStream TO WriteString(REALTOSN(ac,11,2,1)+" ");
                  ASK CostStream TO WriteString(REALTOSN(esc,11,2,1)+" ");
                  ASK CostStream TO WriteString(REALTOSN(tc,11,2,1)+" ");
                  ASK CostStream TO WriteLn; 
               END IF;
            END FOR;
            ASK CostStream TO WriteLn;
            ASK CostStream TO WriteString("SparePool_Totals     ");
            ASK CostStream TO WriteString(REALTOSN(pict,11,2,1)+" ");
            ASK CostStream TO WriteString(REALTOSN(pact,11,2,1)+" ");
            ASK CostStream TO WriteString(REALTOSN(pesct,11,2,1)+" ");
            ASK CostStream TO WriteString(REALTOSN(ptct,11,2,1)+" ");
            ASK CostStream TO WriteLn;         
            ASK CostStream TO WriteLn;         
            ASK CostStream TO WriteString("________________________________________________________________________________");
            ASK CostStream TO WriteLn;         
            ASK CostStream TO WriteLn; 
         END IF;   {SparePools}
         IF totalRes>0
            ASK CostStream TO WriteString("Resources                Initial  PerUseCost PerTimeCost       Total");
            ASK CostStream TO WriteLn;
            FOR i:=1 TO totalPools
               IF PoolArray[i].sparingType=Resource
                  IF ZeroInitCost
                     ic:=0.0;
                  ELSE
                     ic:=FLOAT(PoolArray[i].initialSpares)*PoolArray[i].spareCost;
                  END IF;
                  puc:=poolNewCost[i]; 
                  ptc:=poolShipCost[i];
                  {reused variables, ES used for emerShipping(pools) and 
                    perUseCost(resources)}
                  tc:= ic+ptc+puc;
                  rict:=rict+ic;
                  rptct:=rptct+ptc;
                  rpuct:=rpuct+puc;
                  rtct:=rtct+tc;
                  ASK CostStream TO WriteString(SUBSTR(1,20,PoolArray[i].poolName + "                    ")+" ");
                  ASK CostStream TO WriteString(REALTOSN(ic,11,2,1)+" ");
                  ASK CostStream TO WriteString(REALTOSN(puc,11,2,1)+" ");
                  ASK CostStream TO WriteString(REALTOSN(ptc,11,2,1)+" ");
                  ASK CostStream TO WriteString(REALTOSN(tc,11,2,1)+" ");
                  ASK CostStream TO WriteLn; 
               END IF;
            END FOR;
            ASK CostStream TO WriteLn;
            ASK CostStream TO WriteString("Resource_Totals      ");
            ASK CostStream TO WriteString(REALTOSN(rict,11,2,1)+" ");
            ASK CostStream TO WriteString(REALTOSN(rpuct,11,2,1)+" ");
            ASK CostStream TO WriteString(REALTOSN(rptct,11,2,1)+" ");
            ASK CostStream TO WriteString(REALTOSN(rtct,11,2,1)+" ");
            ASK CostStream TO WriteLn;         
            ASK CostStream TO WriteLn;         
            ASK CostStream TO WriteString("________________________________________________________________________________");
            ASK CostStream TO WriteLn;         
            ASK CostStream TO WriteLn;
         END IF;   {SparePools}
      END IF;  {pools Exist}
      IntTotal:=btct+etct+ptct+rtct+sysIntRedCost+sysIntLostCost;
      ASK CostStream TO WriteString("Cost_Summary");
      ASK CostStream TO WriteLn;  
      ASK CostStream TO WriteLn;  
      ASK CostStream TO WriteString("Blocks               ");
      ASK CostStream TO WriteString(REALTOSN(btct,11,2,1));
      ASK CostStream TO WriteLn;
      ASK CostStream TO WriteString("Events               ");
      ASK CostStream TO WriteString(REALTOSN(etct,11,2,1));
      ASK CostStream TO WriteLn;
      ASK CostStream TO WriteString("SparePools           ");
      ASK CostStream TO WriteString(REALTOSN(ptct,11,2,1));
      ASK CostStream TO WriteLn;
      ASK CostStream TO WriteString("Resources            ");
      ASK CostStream TO WriteString(REALTOSN(rtct,11,2,1));
      ASK CostStream TO WriteLn;
      ASK CostStream TO WriteString("SystemDownCost       ");
      ASK CostStream TO WriteString(REALTOSN(sysIntRedCost,11,2,1));
      ASK CostStream TO WriteLn;
      ASK CostStream TO WriteString("SystemLostFlowCost   ");
      ASK CostStream TO WriteString(REALTOSN(sysIntLostCost,11,2,1));
      ASK CostStream TO WriteLn;
      ASK CostStream TO WriteLn;      
      ASK CostStream TO WriteString("________________________________");
      ASK CostStream TO WriteLn;
      ASK CostStream TO WriteString("Grand_Total          ");      
      ASK CostStream TO WriteString(REALTOSN(IntTotal,11,2,1));
      ASK CostStream TO WriteLn;
      ASK CostStream TO WriteLn;
      ASK CostStream TO WriteString("*****************************");      
      ASK CostStream TO WriteLn;          
      ASK CostStream TO WriteLn;          
      DISPOSE(idleCostArray);
      DISPOSE(standbyCostArray);
      DISPOSE(holdCostArray);
      DISPOSE(repCostArray);
      DISPOSE(opCostArray);
      DISPOSE(doneCostArray);
      IF totalPools>0
         DISPOSE(poolNewCost);
         DISPOSE(poolShipCost);
      END IF;
   END LOOP;  
END METHOD;  {interim cost results}

  TELL METHOD CallEndSimulation(IN abortstatus, keepStats : BOOLEAN);
  BEGIN
    EndSimCalled:=TRUE;
    ASK EventController TO SetTimeAdvance(FALSE);  
    IF (TieBreaking=FALSE) 
       ASK EventController TO SetTieBreaking(TRUE);
       TieBreaking:=TRUE; 
    END IF; 
    IF hitXinSim
       keepStats:=FALSE;
    END IF;
    EndSimulation(abortstatus, keepStats);
  END METHOD; 

  TELL METHOD EndSimulation(IN abortstatus, keepStats :BOOLEAN);
  VAR
     i                  : INTEGER;
     trig               : RapTriggerObj;
     block              : RBDBlockObj;
     node               : RBDNodeObj;
     displayString      : STRING;
     event              : RBDEventObj;
  BEGIN
     runCompleted:=TRUE; 
     cyclesCount:=cyclesCount+(FLOAT(currentPhase)/FLOAT(activePhases));
     IF C130File   
        ASK C130Stream TO WriteString("-1 -1 -1");  
        ASK C130Stream TO WriteLn;     
     END IF;
     IF ((statusBarOn) AND (inStepMode))
        IF SimTime<1000.0
           displayString:=REALTOSTR(SimTime);
        ELSE
           displayString:=REALTOSN(SimTime,10,2,1);
        END IF;   
        ASK window TO ShowStatus(0,"Sim Time : "+displayString);
     END IF;            
     FOREACH node IN nodeGroup
        ASK node TO ChangeNodeState(AllUp,Active);
        IF capacityAnalysis AND GraphicsOutput
           ASK node TO ShowFlow;
        END IF;
     END FOREACH;
     FOREACH block IN blockGroup
        ASK block TO ChangeBlockState(Done,block.activeStatus,"End_sim");
     END FOREACH;
     FOREACH event IN eventGroup
        ASK event TO ChangeEventState(Armed,event.activeStatus,"End_sim");   
     END FOREACH;
     Aborted:=abortstatus; 
     KeepSimStats:=keepStats;                       
     FOR i:= 1 TO totalBlocks
        ASK BlockTrigger[i] TO Release;
     END FOR;
     FOREACH trig IN triggerGroup
        ASK trig TO Release;
     END FOREACH;   
     IF SystemStateChange
        AnalyzeSystem;
        SystemStateChange:=FALSE;
     END IF; 
     StopSimulation;       
  END METHOD; 

  TELL METHOD ControlPhaseChanges();
  VAR
     newPhase                              :   INTEGER;
     phaseDuration                         :   REAL;
     phase                                 :   PhaseObj;
  BEGIN
     currentPhase:=1;
     phase:=phaseObjArray[currentPhase];
     ASK SysStream[11] TO DrawNumber(phase.Dist,phase.Params,phase.phaseName,phaseDuration); 
     LOOP
        WAIT DURATION phaseDuration;
        END WAIT;
        IF  ( (termType=1) AND ( ABS(StopCriteria-SimTime)<0.0000001) )
           EXIT;     {skip phase change if sim is over}
        END IF; 
        IF (currentPhase=activePhases)
           newPhase:=1;
           cyclesCount:=cyclesCount+1.0;
           IF (termType=3)
              INC(termCounter);       
              IF (termCounter=TRUNC(StopCriteria))
                 CallEndSimulation(FALSE,TRUE);
                 EXIT;
              ELSIF (termCounter=TRUNC(StartStats))
                 ResetAllStatistics;
              END IF; 
              IF statusBarOn
                 IF termCounter=1
                    ASK window TO ShowStatus(2,INTTOSTR(termCounter)+" cycle completed"); 
                 ELSE   
                    ASK window TO ShowStatus(2,INTTOSTR(termCounter)+" cycles completed"); 
                 END IF;   
              END IF;
           END IF;   
        ELSE
           newPhase:=currentPhase+1;
        END IF;
        currentPhase:=newPhase;
        phase:=phaseObjArray[newPhase];
        ASK SysStream[11] TO DrawNumber(phase.Dist,phase.Params,phase.phaseName,phaseDuration); 
        IF EventsFile
           WriteEvents(SimTime,"System","Phase_change","Phase_change","Begin_phase_"+phase.phaseName);
        END IF;   
        IF statusBarOn
           ASK window TO ShowStatus(3,phase.phaseName);  
        END IF;
        IF (phaseDuration > 0.0)
           PhaseChangeInProgress:=TRUE;
           ConductPhaseChange(newPhase);
        ELSE
           ASK pmSyncTrigger TO Release;
        END IF;
        {Note: phaseNumber does not get incremented for a zero length phase}
     END LOOP;      
  END METHOD;
 
  TELL METHOD ConductPhaseChange(IN newPhase : INTEGER);
  VAR
     oldPhase,improves,degrades,stresses,i,oldVal,newVal    : INTEGER;
     oldType,newType                                        : STRING;
     oldValue,newValue                                      : REAL;
     block                                                  : RBDBlockObj;
     event                                                  : RBDEventObj;
     node                                                   : RBDNodeObj;
     thing                                                  : RBDBasicObj;
     phase                                                  : PhaseObj;
     trig                                                   : RapTriggerObj;
  BEGIN  
     phase:=phaseObjArray[currentPhase];
     IF ((NOT phase.mission) AND (System.missionStatus=Mission))    {were Mission, going to Non-Mission}
        ChangeStatus(Status,NonMission);
        IF deferTrig = "phaseTrigger"     {avant garde tab feature - using deferred maint}
           FOREACH trig IN triggerGroup
              IF trig.TrigName = deferTrig
                 ASK trig TO Release;
              END IF;
           END FOREACH;
        END IF;           
     END IF;
     improves:=0;
     degrades:=0;
     stresses:=0;
     oldPhase:=phaseNumber;
     phaseNumber:=newPhase;
     FOREACH thing IN BlockPhaseChangeGroup     {tony 3-26 think about changing name of group}
        IF OBJTYPENAME(thing)="RBDBlockObj"
           block:= ASK root Child("RBDBlock",thing.Id);
           oldValue:= block.phaseValue[oldPhase];
           oldType:= block.phaseType[oldPhase];
           newValue:= block.phaseValue[newPhase];
           newType:= block.phaseType[newPhase];
           IF ((oldValue <> newValue) OR (oldType<>newType))
              IF (oldType="C")
                 ASK BlockPhaseImproveGroup TO Add(block);
              ELSIF (oldType="L")
                 ASK BlockPhaseDegradeGroup TO Add(block);
              ELSE  {oldType="A"}
                 IF newType="L"
                    ASK BlockPhaseImproveGroup TO Add(block);
                 ELSIF newType="C"
                    ASK BlockPhaseDegradeGroup TO Add(block);                    
                 ELSE {newType="A", stress change only}   
                    ASK BlockPhaseStressGroup TO Add(block);                                        
                 END IF;
              END IF;
           END IF;
        ELSIF OBJTYPENAME(thing)="RBDEventObj"
           event:= ASK root Child("RBDEvent",thing.Id);
           oldValue:= event.phaseValue[oldPhase];
           oldType:= event.phaseType[oldPhase];
           newValue:= event.phaseValue[newPhase];
           newType:= event.phaseType[newPhase];
           IF ((oldValue <> newValue) OR (oldType<>newType))
              IF (oldType="C")
                 ASK BlockPhaseImproveGroup TO Add(event);
              ELSIF (oldType="L")
                 ASK BlockPhaseDegradeGroup TO Add(event);
              ELSE  {oldType="A"}
                 IF newType="L"
                    ASK BlockPhaseImproveGroup TO Add(event);
                 ELSIF newType="C"
                    ASK BlockPhaseDegradeGroup TO Add(event);
                 ELSIF newType="A"     {fire with another p-value}
                    IF (event.opStatus=Success)
                       ASK BlockPhaseDegradeGroup TO Add(event);
                    ELSIF event.opStatus=Armed
                       ASK BlockPhaseDegradeGroup TO Add(event);
                    ELSIF event.opStatus=Failure;
                       ASK BlockPhaseImproveGroup TO Add(event);
                    END IF; 
                 ELSIF newType="F"  
                    IF (event.opStatus=Success)
                       ASK BlockPhaseDegradeGroup TO Add(event);
                    ELSIF event.opStatus=Armed
                       ASK BlockPhaseDegradeGroup TO Add(event);
                    ELSIF event.opStatus=Failure;
                       ASK BlockPhaseImproveGroup TO Add(event);
                    END IF; 
                 ELSE {newType="P"}
                     {do nothing}
                 END IF;
              END IF;
           ELSE  
              IF ((oldType="F") OR (oldType="A"))  {firing in successive phases}
                 IF event.opStatus=Success
                    ASK BlockPhaseDegradeGroup TO Add(event);
                 ELSIF event.opStatus=Armed
                    ASK BlockPhaseDegradeGroup TO Add(event);
                 ELSIF event.opStatus=Failure;
                    ASK BlockPhaseImproveGroup TO Add(event);
                 END IF; 
              END IF;
           END IF;
        END IF;   {OBJTYPENAME(thing)}
     END FOREACH;
     FOREACH node IN NodePhaseChangeGroup
        oldVal:= node.phase[oldPhase];
        IF oldVal=-1                {node was cut}
           oldVal:=node.connectIntoNum+1;
        END IF;
        newVal:= node.phase[newPhase];
        IF newVal=-1                {node will be cut}
           newVal:=node.connectIntoNum+1;
        END IF;   
        IF oldVal <> newVal
           IF (oldVal > newVal)
              ASK NodePhaseImproveGroup TO Add(node);
           ELSE
              ASK NodePhaseDegradeGroup TO Add(node);
           END IF; 
        END IF;
     END FOREACH;
     improves:=BlockPhaseImproveGroup.numberIn+NodePhaseImproveGroup.numberIn;
     degrades:=BlockPhaseDegradeGroup.numberIn+NodePhaseDegradeGroup.numberIn;
     stresses:=BlockPhaseStressGroup.numberIn;
     IF ((System.Status=GGreen) OR (System.Status=YYellow))
        FOREACH thing IN BlockPhaseImproveGroup
           IF OBJTYPENAME(thing)="RBDBlockObj"
              block:=ASK root Child("RBDBlock",thing.Id)
              oldValue:= block.phaseValue[oldPhase];
              oldType:= block.phaseType[oldPhase];
              newValue:= block.phaseValue[newPhase];
              newType:= block.phaseType[newPhase];
              ASK block TO ChangePhases(oldValue,newValue,oldType,newType);
           ELSIF OBJTYPENAME(thing)="RBDEventObj"
              event:=ASK root Child("RBDEvent",thing.Id)
              oldValue:= event.phaseValue[oldPhase];
              oldType:= event.phaseType[oldPhase];
              newValue:= event.phaseValue[newPhase];
              newType:= event.phaseType[newPhase];
              ASK event TO ChangePhases(oldValue,newValue,oldType,newType);
           END IF;
           FOR i:=1 TO 5
              WAIT DURATION 0.0;     
              END WAIT;
           END FOR;
           ASK BlockPhaseImproveGroup TO RemoveThis(thing);
        END FOREACH;
        FOREACH node IN NodePhaseImproveGroup
           oldVal:= node.phase[oldPhase];
           IF oldVal=-1
              oldVal:=node.connectIntoNum+1;
           END IF;
           newVal:= node.phase[newPhase];
           IF newVal=-1
              newVal:=node.connectIntoNum+1;
           END IF;   
           ASK node TO ChangePhases(oldVal,newVal);
           WAIT DURATION 0.0;     
           END WAIT;             
           ASK NodePhaseImproveGroup TO RemoveThis(node);
        END FOREACH;
        FOREACH thing IN  BlockPhaseStressGroup
           IF OBJTYPENAME(thing)="RBDBlockObj"
              block := ASK root Child("RBDBlock",thing.Id);
              oldValue:= block.phaseValue[oldPhase];
              oldType:= block.phaseType[oldPhase];
              newValue:= block.phaseValue[newPhase];
              newType:= block.phaseType[newPhase];
              ASK block TO ChangePhases(oldValue,newValue,oldType,newType);
           END IF;
           FOR i:=1 TO 5
              WAIT DURATION 0.0;     
              END WAIT;
           END FOR;
           ASK BlockPhaseStressGroup TO RemoveThis(thing);
        END FOREACH;
        FOREACH thing IN  BlockPhaseDegradeGroup
           IF OBJTYPENAME(thing)="RBDBlockObj"
              block:= ASK root Child("RBDBlock",thing.Id);
              oldValue:= block.phaseValue[oldPhase];
              oldType:= block.phaseType[oldPhase];
              newValue:= block.phaseValue[newPhase];
              newType:= block.phaseType[newPhase];
              ASK block TO ChangePhases(oldValue,newValue,oldType,newType);
           ELSIF OBJTYPENAME(thing)="RBDEventObj"
              event:=ASK root Child("RBDEvent",thing.Id)
              oldValue:= event.phaseValue[oldPhase];
              oldType:= event.phaseType[oldPhase];
              newValue:= event.phaseValue[newPhase];
              newType:= event.phaseType[newPhase];
              ASK event TO ChangePhases(oldValue,newValue,oldType,newType);
           END IF;   
           FOR i:=1 TO 5
              WAIT DURATION 0.0;     
              END WAIT;
           END FOR;
           ASK BlockPhaseDegradeGroup TO RemoveThis(thing);
        END FOREACH;
        FOREACH node IN  NodePhaseDegradeGroup
           oldVal:= node.phase[oldPhase];
           IF oldVal=-1
              oldVal:=node.connectIntoNum+1;
           END IF;
           newVal:= node.phase[newPhase];
           IF newVal=-1
              newVal:=node.connectIntoNum+1;
           END IF;   
           ASK node TO ChangePhases(oldVal,newVal);
           FOR i:=1 TO 5
              WAIT DURATION 0.0;     
              END WAIT;
           END FOR;
           ASK NodePhaseDegradeGroup TO RemoveThis(node);
        END FOREACH;
     ELSE   {System.Status:=RRed}
        FOREACH thing IN  BlockPhaseDegradeGroup
           IF OBJTYPENAME(thing)="RBDBlockObj"
              block:= ASK root Child("RBDBlock",thing.Id);
              oldValue:= block.phaseValue[oldPhase];
              oldType:= block.phaseType[oldPhase];
              newValue:= block.phaseValue[newPhase];
              newType:= block.phaseType[newPhase];
              ASK block TO ChangePhases(oldValue,newValue,oldType,newType);
           ELSIF OBJTYPENAME(thing)="RBDEventObj"
              event:=ASK root Child("RBDEvent",thing.Id)
              oldValue:= event.phaseValue[oldPhase];
              oldType:= event.phaseType[oldPhase];
              newValue:= event.phaseValue[newPhase];
              newType:= event.phaseType[newPhase];
              ASK event TO ChangePhases(oldValue,newValue,oldType,newType);
           END IF;      
           FOR i:=1 TO 5
              WAIT DURATION 0.0;     
              END WAIT;
           END FOR;
           ASK BlockPhaseDegradeGroup TO RemoveThis(thing);
        END FOREACH;
        FOREACH node IN  NodePhaseDegradeGroup
           oldVal:= node.phase[oldPhase];
           IF oldVal=-1
              oldVal:=node.connectIntoNum+1;
           END IF;
           newVal:= node.phase[newPhase];
           IF newVal=-1
              newVal:=node.connectIntoNum+1;
           END IF;   
           ASK node TO ChangePhases(oldVal,newVal);
           WAIT DURATION 0.0;     
           END WAIT;            
           ASK NodePhaseDegradeGroup TO RemoveThis(node);
        END FOREACH;
        FOREACH thing IN  BlockPhaseStressGroup
           IF OBJTYPENAME(thing)="RBDBlockObj"
              block:= ASK root Child("RBDBlock",thing.Id);
              oldValue:= block.phaseValue[oldPhase];
              oldType:= block.phaseType[oldPhase];
              newValue:= block.phaseValue[newPhase];
              newType:= block.phaseType[newPhase];
              ASK block TO ChangePhases(oldValue,newValue,oldType,newType);
           ELSIF OBJTYPENAME(thing)="RBDEventObj"
              event:=ASK root Child("RBDEvent",thing.Id)
              oldValue:= event.phaseValue[oldPhase];
              oldType:= event.phaseType[oldPhase];
              newValue:= event.phaseValue[newPhase];
              newType:= event.phaseType[newPhase];
              ASK event TO ChangePhases(oldValue,newValue,oldType,newType);
           END IF;   
           FOR i:=1 TO 5
              WAIT DURATION 0.0;     
              END WAIT;
           END FOR;
           ASK BlockPhaseStressGroup TO RemoveThis(thing);
        END FOREACH;
        FOREACH thing IN  BlockPhaseImproveGroup
           IF OBJTYPENAME(thing)="RBDBlockObj"
              block:= ASK root Child("RBDBlock",thing.Id);
              oldValue:= block.phaseValue[oldPhase];
              oldType:= block.phaseType[oldPhase];
              newValue:= block.phaseValue[newPhase];
              newType:= block.phaseType[newPhase];
              ASK block TO ChangePhases(oldValue,newValue,oldType,newType);
           ELSIF OBJTYPENAME(thing)="RBDEventObj"
              event:=ASK root Child("RBDEvent",thing.Id)
              oldValue:= event.phaseValue[oldPhase];
              oldType:= event.phaseType[oldPhase];
              newValue:= event.phaseValue[newPhase];
              newType:= event.phaseType[newPhase];
              ASK event TO ChangePhases(oldValue,newValue,oldType,newType);
           END IF;   
           FOR i:=1 TO 5
              WAIT DURATION 0.0;     
              END WAIT;
           END FOR;
           ASK BlockPhaseImproveGroup TO RemoveThis(thing);
        END FOREACH;
        FOREACH node IN  NodePhaseImproveGroup
           oldVal:= node.phase[oldPhase];
           IF oldVal=-1
              oldVal:=node.connectIntoNum+1;
           END IF;
           newVal:= node.phase[newPhase];
           IF newVal=-1
              newVal:=node.connectIntoNum+1;
           END IF;  
           ASK node TO ChangePhases(oldVal,newVal);
           WAIT DURATION 0.0;  
           END WAIT;            
           ASK NodePhaseImproveGroup TO RemoveThis(node);
        END FOREACH;
     END IF;
     IF (missionStatus=NonMission)    {in a NonMission phase, so do deferred PM}   
        FOREACH block IN pmDeferGroup 
           ASK block TO SetDeferPM(FALSE);
           ASK pmDeferGroup TO RemoveThis(block);
           IF block.FailWait
              ASK block TO SetInterruptReason("Start_Deferred_PM");
              Interrupt(block,"Run");
           ELSIF block.ZeroWait
              ASK block TO SetInterruptReason("Start_Deferred_PM");
              ASK BlockTrigger[block.seqNum] TO Release;           {cmc}
           END IF;
        END FOREACH; 
     END IF;   
     IF ((improves+degrades)>0)  {not just stress changes}
        CheckColds;
        IF weakAnalysis
           FOREACH node IN NodeFailureGroup 
              IF (node.Status=Down)
                 nodeMissionBool[node.seqNum]:=FALSE;
              END IF;
              ASK NodeFailureGroup TO RemoveThis(node);
           END FOREACH;
        END IF;
     END IF;
     IF ((phase.mission) AND (missionStatus=NonMission))
        AnalyzeSystem;   {Chuck 12/14/04 investigate if this is necessary}
        ChangeStatus(Status,Mission);
     END IF;   
     PhaseChangeInProgress:=FALSE; 
     ASK pmSyncTrigger TO Release; 
  END METHOD;   {TELL METHOD ConductPhaseChange}
 
  ASK METHOD FEVPhaseChange(IN newPhase : INTEGER);
  VAR
     oldPhase,oldVal,newVal,improves,degrades    : INTEGER;
     printString,outString,ChangeBlockName,
     oldType,newType                            : STRING;
     node                                       : RBDNodeObj;
     oldValue,newValue                          : REAL;
     phase                                      : PhaseObj;
     block                                      : RBDBlockObj;
     event                                      : RBDEventObj;
     thing                                      : RBDBasicObj;
  BEGIN   
     improves:=0;
     degrades:=0;
     oldPhase:=phaseNumber;
     phaseNumber:=newPhase;
     IF statusBarOn
        phase:=phaseObjArray[phaseNumber];
        ASK window TO ShowStatus(3,"phase.phaseName);  
     END IF;
     FOREACH thing IN BlockPhaseChangeGroup
        IF OBJTYPENAME(thing)="RBDBlockObj"
           block:= ASK root Child("RBDBlock",thing.Id);
           ChangeBlockName:=block.name;
           oldValue:= block.phaseValue[oldPhase];
           oldType:= block.phaseType[oldPhase];
           newValue:= block.phaseValue[newPhase];
           newType:= block.phaseType[newPhase];
           IF ((oldValue <> newValue) OR (oldType<>newType))
              IF (oldType="C")
                 ASK BlockPhaseImproveGroup TO Add(block);
              ELSIF (oldType="L")
                 ASK BlockPhaseDegradeGroup TO Add(block);
              ELSE  {oldType="A"}
                 IF newType="L"
                    ASK BlockPhaseImproveGroup TO Add(block);
                 ELSIF newType="C"
                    ASK BlockPhaseDegradeGroup TO Add(block);                    
                 ELSE {newType="A", stress change only}   
                    ASK BlockPhaseStressGroup TO Add(block);                                        
                 END IF;
              END IF;
           END IF;
        ELSIF OBJTYPENAME(thing)="RBDEventObj"
           event:= ASK root Child("RBDEvent",thing.Id);
           ChangeBlockName:=event.name;
           oldValue:= event.phaseValue[oldPhase];
           oldType:= event.phaseType[oldPhase];
           newValue:= event.phaseValue[newPhase];
           newType:= event.phaseType[newPhase];
           IF ((oldValue <> newValue) OR (oldType<>newType))
              IF (oldType="C")
                 ASK BlockPhaseImproveGroup TO Add(event);
              ELSIF (oldType="L")
                 ASK BlockPhaseDegradeGroup TO Add(event);
              ELSE  {oldType="A"}
                 IF newType="L"
                    ASK BlockPhaseImproveGroup TO Add(event);
                 ELSIF newType="C"
                    ASK BlockPhaseDegradeGroup TO Add(event);
                 ELSIF newType="A"  
                    IF (event.opStatus=Success)
                       ASK BlockPhaseDegradeGroup TO Add(event);
                    ELSIF event.opStatus=Armed
                       ASK BlockPhaseDegradeGroup TO Add(event);
                    ELSIF event.opStatus=Failure;
                       ASK BlockPhaseImproveGroup TO Add(event);
                    END IF; 
                 ELSIF newType="F"  
                    IF (event.opStatus=Success)
                       ASK BlockPhaseDegradeGroup TO Add(event);
                    ELSIF event.opStatus=Armed
                       ASK BlockPhaseDegradeGroup TO Add(event);
                    ELSIF event.opStatus=Failure;
                       ASK BlockPhaseImproveGroup TO Add(event);
                    END IF; 
                 ELSE {newType="P"}
                     {do nothing}
                 END IF;
              END IF;
           ELSE  
              IF ((oldType="F") OR (oldType="A")) 
                 IF event.opStatus=Success
                    ASK BlockPhaseDegradeGroup TO Add(event);
                 ELSIF event.opStatus=Armed
                    ASK BlockPhaseDegradeGroup TO Add(event);
                 ELSIF event.opStatus=Failure;
                    ASK BlockPhaseImproveGroup TO Add(event);
                 END IF; 
              END IF;
           END IF;
        END IF;
     END FOREACH;
     FOREACH node IN NodePhaseChangeGroup
        ChangeBlockName:=node.name;
        oldVal:= node.phase[oldPhase];
        IF oldVal=-1
           oldVal:=node.connectIntoNum+1;
        END IF;
        newVal:= node.phase[newPhase];
        IF newVal=-1
           newVal:=node.connectIntoNum+1;
        END IF;   
        IF oldVal <> newVal
           IF (oldVal > newVal)
              ASK NodePhaseImproveGroup TO Add(node);
           ELSE
              ASK NodePhaseDegradeGroup TO Add(node);
           END IF; 
        END IF;
     END FOREACH;
     improves:=BlockPhaseImproveGroup.numberIn+NodePhaseImproveGroup.numberIn;
     degrades:=BlockPhaseDegradeGroup.numberIn+NodePhaseDegradeGroup.numberIn;
     PhaseChangeInProgress:=TRUE;
     IF ((System.Status=GGreen) OR (System.Status=YYellow))
        FOREACH thing IN BlockPhaseImproveGroup
           IF OBJTYPENAME(thing)="RBDBlockObj"
              block := ASK root Child("RBDBlock",thing.Id);
              oldValue:= block.phaseValue[oldPhase];
              oldType:= block.phaseType[oldPhase];
              newValue:= block.phaseValue[newPhase];
              newType:= block.phaseType[newPhase];
              ASK block TO ChangePhases(oldValue,newValue,oldType,newType);
           ELSIF OBJTYPENAME(thing)="RBDEventObj"
              event := ASK root Child("RBDEvent",thing.Id);
              oldValue:= event.phaseValue[oldPhase];
              oldType:= event.phaseType[oldPhase];
              newValue:= event.phaseValue[newPhase];
              newType:= event.phaseType[newPhase];
              ASK event TO ChangePhases(oldValue,newValue,oldType,newType);
           END IF;
           ASK BlockPhaseImproveGroup TO RemoveThis(thing);
        END FOREACH;
        FOREACH node IN NodePhaseImproveGroup
           oldVal:= node.phase[oldPhase];
           IF oldVal=-1
              oldVal:=node.connectIntoNum+1;
           END IF;
           newVal:= node.phase[newPhase];
           IF newVal=-1
              newVal:=node.connectIntoNum+1;
           END IF;   
           ASK node TO ChangePhases(oldVal,newVal);
           ASK NodePhaseImproveGroup TO RemoveThis(node);
        END FOREACH;
        FOREACH thing IN  BlockPhaseStressGroup
           IF OBJTYPENAME(thing)="RBDBlockObj"
              block := ASK root Child("RBDBlock",thing.Id);
              oldValue:= block.phaseValue[oldPhase];
              oldType:= block.phaseType[oldPhase];
              newValue:= block.phaseValue[newPhase];
              newType:= block.phaseType[newPhase];
              ASK block TO ChangePhases(oldValue,newValue,oldType,newType);
           ELSIF OBJTYPENAME(thing)="RBDEventObj"
              event := ASK root Child("RBDEvent",thing.Id);
              oldValue:= event.phaseValue[oldPhase];
              oldType:= event.phaseType[oldPhase];
              newValue:= event.phaseValue[newPhase];
              newType:= event.phaseType[newPhase];
              ASK event TO ChangePhases(oldValue,newValue,oldType,newType);
           END IF;      
           ASK BlockPhaseStressGroup TO RemoveThis(thing);
        END FOREACH;
        FOREACH thing IN  BlockPhaseDegradeGroup
           IF OBJTYPENAME(thing)="RBDBlockObj"
              block := ASK root Child("RBDBlock",thing.Id);
              oldValue:= block.phaseValue[oldPhase];
              oldType:= block.phaseType[oldPhase];
              newValue:= block.phaseValue[newPhase];
              newType:= block.phaseType[newPhase];
              ASK block TO ChangePhases(oldValue,newValue,oldType,newType);
           ELSIF OBJTYPENAME(thing)="RBDEventObj"
              event := ASK root Child("RBDEvent",thing.Id);
              oldValue:= event.phaseValue[oldPhase];
              oldType:= event.phaseType[oldPhase];
              newValue:= event.phaseValue[newPhase];
              newType:= event.phaseType[newPhase];
              ASK event TO ChangePhases(oldValue,newValue,oldType,newType);
           END IF;   
           ASK BlockPhaseDegradeGroup TO RemoveThis(thing);
        END FOREACH;
        FOREACH node IN  NodePhaseDegradeGroup
           oldVal:= node.phase[oldPhase];
           IF oldVal=-1
              oldVal:=node.connectIntoNum+1;
           END IF;
           newVal:= node.phase[newPhase];
           IF newVal=-1
              newVal:=node.connectIntoNum+1;
           END IF;   
           ASK node TO ChangePhases(oldVal,newVal);
           ASK NodePhaseDegradeGroup TO RemoveThis(node);
        END FOREACH;
     ELSE   {System.Status:=RRed}
        FOREACH thing IN  BlockPhaseDegradeGroup
           IF OBJTYPENAME(thing)="RBDBlockObj"
              block := ASK root Child("RBDBlock",thing.Id);
              oldValue:= block.phaseValue[oldPhase];
              oldType:= block.phaseType[oldPhase];
              newValue:= block.phaseValue[newPhase];
              newType:= block.phaseType[newPhase];
              ASK block TO ChangePhases(oldValue,newValue,oldType,newType);
           ELSIF OBJTYPENAME(thing)="RBDEventObj"
              event := ASK root Child("RBDEvent",thing.Id);
              oldValue:= event.phaseValue[oldPhase];
              oldType:= event.phaseType[oldPhase];
              newValue:= event.phaseValue[newPhase];
              newType:= event.phaseType[newPhase];
              ASK event TO ChangePhases(oldValue,newValue,oldType,newType);
           END IF;      
           ASK BlockPhaseDegradeGroup TO RemoveThis(thing);
        END FOREACH;
        FOREACH node IN  NodePhaseDegradeGroup
           oldVal:= node.phase[oldPhase];
           IF oldVal=-1
              oldVal:=node.connectIntoNum+1;
           END IF;
           newVal:= node.phase[newPhase];
           IF newVal=-1
              newVal:=node.connectIntoNum+1;
           END IF;   
           ASK node TO ChangePhases(oldVal,newVal);
           ASK NodePhaseDegradeGroup TO RemoveThis(node);
        END FOREACH;
        FOREACH thing IN  BlockPhaseStressGroup
           IF OBJTYPENAME(thing)="RBDBlockObj"
              block := ASK root Child("RBDBlock",thing.Id);
              oldValue:= block.phaseValue[oldPhase];
              oldType:= block.phaseType[oldPhase];
              newValue:= block.phaseValue[newPhase];
              newType:= block.phaseType[newPhase];
              ASK block TO ChangePhases(oldValue,newValue,oldType,newType);
           ELSIF OBJTYPENAME(thing)="RBDEventObj"
              event := ASK root Child("RBDEvent",thing.Id);
              oldValue:= event.phaseValue[oldPhase];
              oldType:= event.phaseType[oldPhase];
              newValue:= event.phaseValue[newPhase];
              newType:= event.phaseType[newPhase];
              ASK event TO ChangePhases(oldValue,newValue,oldType,newType);
           END IF;      
           ASK BlockPhaseStressGroup TO RemoveThis(thing);
        END FOREACH;
        FOREACH thing IN  BlockPhaseImproveGroup
           IF OBJTYPENAME(thing)="RBDBlockObj"
              block := ASK root Child("RBDBlock",thing.Id);
              oldValue:= block.phaseValue[oldPhase];
              oldType:= block.phaseType[oldPhase];
              newValue:= block.phaseValue[newPhase];
              newType:= block.phaseType[newPhase];
              ASK block TO ChangePhases(oldValue,newValue,oldType,newType);
           ELSIF OBJTYPENAME(thing)="RBDEventObj"
              event := ASK root Child("RBDEvent",thing.Id);
              oldValue:= event.phaseValue[oldPhase];
              oldType:= event.phaseType[oldPhase];
              newValue:= event.phaseValue[newPhase];
              newType:= event.phaseType[newPhase];
              ASK event TO ChangePhases(oldValue,newValue,oldType,newType);
           END IF;      
           ASK BlockPhaseImproveGroup TO RemoveThis(thing);
        END FOREACH;
        FOREACH node IN  NodePhaseImproveGroup
           oldVal:= node.phase[oldPhase];
           IF oldVal=-1
              oldVal:=node.connectIntoNum+1;
           END IF;
           newVal:= node.phase[newPhase];
           IF newVal=-1
              newVal:=node.connectIntoNum+1;
           END IF;  
           ASK node TO ChangePhases(oldVal,newVal);
           ASK NodePhaseImproveGroup TO RemoveThis(node);
        END FOREACH;
     END IF;
     IF ((improves+degrades)>0)  {not just stress changes}
        IF weakAnalysis
           FOREACH node IN NodeFailureGroup 
              IF ((node.Status=Down) AND (node.trialSuccess=TRUE))
                 ASK node TO SetTrialSuccess(FALSE);
              END IF;
              ASK NodeFailureGroup TO RemoveThis(node);
           END FOREACH;
        END IF;
     END IF;
     CheckColds;
     AnalyzeSystem;
     PhaseChangeInProgress:=FALSE;                 
  END METHOD;  {ASK METHOD FEVPhaseChange}
 
  TELL METHOD UpdateSimStatus(IN termTime:REAL);   
  VAR
     counter       : INTEGER;
     waitInterval  : REAL;
  BEGIN       
     waitInterval:=0.05*termTime;  
     FOR counter:=1 TO 19        
        WAIT DURATION waitInterval; 
        END WAIT;  
        ASK window TO ShowStatus(2,INTTOSTR(counter*5) + "% complete");  
     END FOR;    
  END METHOD;  {UpdateSimStatus} 

  TELL METHOD GenerateAoPlotData();   
     VAR
        cumGreen,cumYellow,cumRed,intGreen,intYellow,
        intRed,currentAo,intAo,greenAo,redAo            : REAL;
        intervalNum                                     : INTEGER;
        printString                                     : STRING;
     BEGIN
        redAo:=1.0;
        IF AvailGraph
           MultiRunDataMon:=GETMONITOR(MultiRunData[1],RStatObj);
        END IF;
        LOOP
           WAIT DURATION AoPlotInterval; 
           END WAIT;
           INC(intervalNum); 
           intGreen:=cumGreen;  {temporarily store current cumulative totals}
           intYellow:=cumYellow;
           intRed:=cumRed;
           cumGreen:=GreenMon.Sum;   {get current values for G, R, and Y sums}
           cumYellow:=YellowMon.Sum;
           cumRed:=RedMon.Sum;
           CASE Status              {get leftover time since last change}
              WHEN GGreen:
                 cumGreen:=cumGreen+(SimTime-LastColorChange);
              WHEN YYellow:
                 cumYellow:=cumYellow+(SimTime-LastColorChange);
              WHEN RRed:         
                 cumRed:=cumRed+(SimTime-LastColorChange);      
              OTHERWISE             
                 NEW(message,1..1);
                 message[1]:="Error in GenerateAoPlotData!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                 DISPOSE(message);   
           END CASE; 
           intGreen:=cumGreen-intGreen; 
           intYellow:=cumYellow-intYellow;
           intRed:=cumRed-intRed;           
           IF ((cumGreen+cumYellow+cumRed) >0.)
              greenAo:=(cumGreen)/(cumGreen+cumYellow+cumRed);             
              currentAo:=(cumGreen+cumYellow)/(cumGreen+cumYellow+cumRed);
              intAo:=(intGreen+intYellow)/(intGreen+intYellow+intRed);
              IF (StatsStarted AND AvailPlotFile) 
                 printString:=SUBSTR(1,20,REALTOSTR(SimTime)+"                    ")+"  ";
                 ASK AoPlotStream TO WriteString(printString);
                 ASK AoPlotStream TO WriteReal(currentAo,8,6); 
                 ASK AoPlotStream TO WriteString("  ");
                 ASK AoPlotStream TO WriteReal(cumGreen/(cumGreen+cumYellow+cumRed),8,6);
                 ASK AoPlotStream TO WriteString("  ");
                 ASK AoPlotStream TO WriteReal(cumYellow/(cumGreen+cumYellow+cumRed),8,6);
                 ASK AoPlotStream TO WriteString("  ");
                 ASK AoPlotStream TO WriteReal(cumRed/(cumGreen+cumYellow+cumRed),8,6);
                 ASK AoPlotStream TO WriteString("  ");
                 ASK AoPlotStream TO WriteReal(intAo,8,6); 
                 ASK AoPlotStream TO WriteLn;
              END IF;
           ELSE
              greenAo:=1.0;
              currentAo:=1.0;
              intAo:=1.0;
           END IF;  
           IF AvailGraph
              ASK AoGraph TO Plot(1, SimTime, redAo);
              ASK AoGraph TO Plot(2, SimTime, currentAo);
              ASK AoGraph TO Plot(3, SimTime, greenAo);
              ASK IntAoGraph TO Plot(1, SimTime, intAo);
              MultiRunData[intervalNum]:=currentAo;
              MultiRunDataMon:=GETMONITOR(MultiRunData[intervalNum],RStatObj);
              ASK MultiRunGraph TO Plot(1, SimTime, MultiRunDataMon.Minimum);
              ASK MultiRunGraph TO Plot(2, SimTime, MultiRunDataMon.Maximum);
              ASK MultiRunGraph TO Plot(3, SimTime, MultiRunDataMon.Mean);  
           END IF;
        END LOOP;
  END METHOD;   {GenerateAoPlotData}

  ASK METHOD SetMissionStatus (IN newValue : MissionStatusType);
  BEGIN
     missionStatus:= newValue;
  END METHOD;   
  
  
  TELL METHOD UpdateCumulativeValues(IN intervalTime : REAL);      {cmc 3/6/07}
     VAR
        cumGreen,cumYellow,cumRed   : REAL;
        intervalNum                 : INTEGER;
        systemFailures              :INTEGER;
     BEGIN
         intervalNum:=0;
         LOOP
           WAIT DURATION intervalTime; 
           END WAIT;
           INC(intervalNum); 
           cumGreen:=GreenMon.Sum;   {get current values for G, R, and Y sums}
           cumYellow:=YellowMon.Sum;
           cumRed:=RedMon.Sum;
           systemFailures:=RedMon.Count;
           
           CASE Status              {get leftover time since last change}
              WHEN GGreen:
                 cumGreen:=cumGreen+(SimTime-LastColorChange);
              WHEN YYellow:
                 cumYellow:=cumYellow+(SimTime-LastColorChange);
              WHEN RRed:         
                 cumRed:=cumRed+(SimTime-LastColorChange); 
                 INC(systemFailures);
              OTHERWISE             
                 NEW(message,1..1);
                 message[1]:="Error in GenerateAoPlotData!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                 DISPOSE(message);   
           END CASE; 
           IF ((cumGreen+cumYellow+cumRed) >0.)
              CumAoByTime[intervalNum]:=(cumGreen+cumYellow)/(cumGreen+cumYellow+cumRed);
           ELSE
              CumAoByTime[intervalNum]:=1.0;
           END IF; 
           IF ((systemFailures) >0)
              CumMTBDEbyTime[intervalNum]:=(cumGreen+cumYellow)/FLOAT(systemFailures);
           ELSE
              CumMTBDEbyTime[intervalNum]:=(cumGreen+cumYellow);
           END IF; 
           IF ((systemFailures) >0)
              CumMDTbyTime[intervalNum]:=cumRed/FLOAT(systemFailures);
           END IF; 
           
           IF (cumGreen+cumYellow > 0.0)
           
              IF ((CompFailMon.Count>0) AND (CompPMMon.Count>0))
                 CumMTBMbyTime[intervalNum] :=(cumGreen+cumYellow)/(FLOAT(CompFailMon.Count)+FLOAT(CompPMMon.Count));
              ELSIF (CompPMMon.Count>0)
                 CumMTBMbyTime[intervalNum]:= (cumGreen+cumYellow)/(FLOAT(CompPMMon.Count));
              ELSIF (CompFailMon.Count>0)
                 CumMTBMbyTime[intervalNum] :=(cumGreen+cumYellow)/FLOAT(CompFailMon.Count); 
              ELSE 
                 CumMTBMbyTime[intervalNum] :=(cumGreen+cumYellow); 
             END IF;
           END IF;    
                      
           IF ((CompRepairMon.Count>0) AND (CompPMMon.Count>0))
              CumMMTbyTime[intervalNum]:= (CompRepairMon.Mean*FLOAT(CompRepairMon.Count)+CompPMMon.Mean*FLOAT(CompPMMon.Count))/
                     FLOAT(CompRepairMon.Count+CompPMMon.Count);
           ELSIF (CompRepairMon.Count>0)
              CumMMTbyTime[intervalNum]:= CompRepairMon.Mean;
           ELSIF (CompPMMon.Count>0)   
              CumMMTbyTime[intervalNum]:= CompPMMon.Mean;
           END IF;
                                                     
        END LOOP;
  END METHOD;  {UpdateCumulativeValues} 
  
  ASK METHOD WriteCumParamFile();             {cmc 3/6/07}
  VAR
     sigma,SEM, R,SEP,numTrials, numSuccesses :REAL;
     i,n:INTEGER;
     
     
  BEGIN  
  
     ASK kpfTimeStream TO WriteString("SimTime        Ao            Ao_StDev      Ao_SEM        "+
                            "MTBDE           MTBDE_StDev     MTBDE_SEM       "+
                            "MDT             MDT_StDev       MDT_SEM       MDT_Updates          " +
                            "MTBM            MTBM_StDev      MTBM_SEM        "+
                            "MMT             MMT_StDev       MMT_SEM       MMT_Updates   " +
                            "R           R_StDev     R_SEM       R_SEP        Trials=");
     ASK kpfTimeStream TO WriteInt(NumRuns,12);        
     ASK kpfTimeStream TO WriteLn;       
     FOR i:=1 TO TRUNC(StopCriteria/kpfTimeMesh)
        CumAoByTimeMon:=GETMONITOR(CumAoByTime[i],RStatObj);
        CumMTBDEbyTimeMon:=GETMONITOR(CumMTBDEbyTime[i],RStatObj);
        CumMDTbyTimeMon:=GETMONITOR(CumMDTbyTime[i],RStatObj);
        CumMTBMbyTimeMon:=GETMONITOR(CumMTBMbyTime[i],RStatObj);
        CumMMTbyTimeMon:=GETMONITOR(CumMMTbyTime[i],RStatObj);
     
        ASK kpfTimeStream TO WriteString(REALTOSN( FLOAT(i)*kpfTimeMesh,15,4,0) );
        
        ASK kpfTimeStream TO WriteString(REALTOSN(CumAoByTimeMon.Mean,14,9,0) );        
        IF (CumAoByTimeMon.Count>1)
           sigma:=PopToSample(CumAoByTimeMon.StdDev,CumAoByTimeMon.Count);
           ASK kpfTimeStream TO WriteString(REALTOSN(sigma,14,9,0));
           SEM:=sigma/SQRT(FLOAT(NumRuns));
           ASK kpfTimeStream TO WriteString(REALTOSN(SEM,14,9,0));                      
        ELSE
           ASK kpfTimeStream TO WriteString("N/A           ");
           ASK kpfTimeStream TO WriteString("N/A           ");
        END IF;
        
        ASK kpfTimeStream TO WriteString(REALTOSN(CumMTBDEbyTimeMon.Mean,16,6,0) );        
        IF (CumMTBDEbyTimeMon.Count>1)
           IF (CumMDTbyTimeMon.Count=NumRuns)         {All runs had at least 1 failure}
              sigma:=PopToSample(CumMTBDEbyTimeMon.StdDev,CumMTBDEbyTimeMon.Count);
              ASK kpfTimeStream TO WriteString(REALTOSN(sigma,16,6,0));
              SEM:=sigma/SQRT(FLOAT(NumRuns));
              ASK kpfTimeStream TO WriteString(REALTOSN(SEM,16,6,0));    
           ELSE                                       {Some runs had suspensions}
              ASK kpfTimeStream TO WriteString("N/A             ");
              ASK kpfTimeStream TO WriteString("N/A             ");
           END IF;
        ELSE
           ASK kpfTimeStream TO WriteString("N/A             ");
           ASK kpfTimeStream TO WriteString("N/A             ");
        END IF;
        
        ASK kpfTimeStream TO WriteString(REALTOSN(CumMDTbyTimeMon.Mean,16,6,0) );        
        IF (CumMDTbyTimeMon.Count>1)
           sigma:=PopToSample(CumMDTbyTimeMon.StdDev,CumMDTbyTimeMon.Count);
           ASK kpfTimeStream TO WriteString(REALTOSN(sigma,16,6,0));
           SEM:=sigma/SQRT(FLOAT(CumMDTbyTimeMon.Count));
           ASK kpfTimeStream TO WriteString(REALTOSN(SEM,16,6,0));    
        ELSE
           ASK kpfTimeStream TO WriteString("N/A             ");
           ASK kpfTimeStream TO WriteString("N/A             ");
        END IF;
        
        ASK kpfTimeStream TO WriteInt(CumMDTbyTimeMon.Count,9);                                
        ASK kpfTimeStream TO WriteString("          ");
        
        ASK kpfTimeStream TO WriteString(REALTOSN(CumMTBMbyTimeMon.Mean,16,6,0) );                
        IF (CumMTBMbyTimeMon.Count>1)
           IF (CumMMTbyTimeMon.Count=NumRuns)         {All runs had at least 1 maintenance action}
              sigma:=PopToSample(CumMTBMbyTimeMon.StdDev,CumMTBMbyTimeMon.Count);
              ASK kpfTimeStream TO WriteString(REALTOSN(sigma,16,6,0));
              SEM:=sigma/SQRT(FLOAT(NumRuns));
              ASK kpfTimeStream TO WriteString(REALTOSN(SEM,16,6,0));    
           ELSE                                       {Some runs had suspensions}
              ASK kpfTimeStream TO WriteString("N/A             ");
              ASK kpfTimeStream TO WriteString("N/A             ");
           END IF;
        ELSE
           ASK kpfTimeStream TO WriteString("N/A             ");
           ASK kpfTimeStream TO WriteString("N/A             ");
        END IF;
        
        ASK kpfTimeStream TO WriteString(REALTOSN(CumMMTbyTimeMon.Mean,16,6,0) );        
        IF (CumMMTbyTimeMon.Count>1)
           sigma:=PopToSample(CumMMTbyTimeMon.StdDev,CumMMTbyTimeMon.Count);
           ASK kpfTimeStream TO WriteString(REALTOSN(sigma,16,6,0));
           SEM:=sigma/SQRT(FLOAT(CumMMTbyTimeMon.Count));
           ASK kpfTimeStream TO WriteString(REALTOSN(SEM,16,6,0));    
        ELSE
           ASK kpfTimeStream TO WriteString("N/A             ");
           ASK kpfTimeStream TO WriteString("N/A             ");
        END IF;
        ASK kpfTimeStream TO WriteInt(CumMMTbyTimeMon.Count,9); 
        ASK kpfTimeStream TO WriteString("   "); 
        numSuccesses:= FLOAT(reliabilityFileData[i]);
        numTrials:=FLOAT(NumRuns);                      
        R:=numSuccesses/numTrials;
        ASK kpfTimeStream TO WriteString(REALTOSN( R,12,9,0));        
        sigma:= SQRT( (numTrials*numSuccesses-numSuccesses*numSuccesses)/(numTrials*(numTrials-1.0))  );
        ASK kpfTimeStream TO WriteString(REALTOSN( sigma,12,9,0));
        SEM:=sigma/SQRT(numTrials);
        ASK kpfTimeStream TO WriteString(REALTOSN( SEM,12,9,0));
        SEP:= SQRT( R*(1.0-R)/numTrials );
        ASK kpfTimeStream TO WriteString(REALTOSN(SEP,12,9,0));        
        ASK kpfTimeStream TO WriteLn;        
     END FOR;          
  END METHOD;  {WriteCumParamFile} 
  
  
END OBJECT;
  
OBJECT RBDSimControlObj;  
  ASK METHOD ChooseNext(IN group : ActivityGroup) : ACTID; 
     VAR 
        owner           : ANYOBJ; 
        ActNum,doFirst  : ACTID;  
        ownerFirst      : ANYOBJ;  
        ActName         : STRING;
        temp1set        : BOOLEAN;
        temp1, temp2    : RBDBlockObj;

     BEGIN    
        ActNum:=group.First; 
        doFirst:=ActNum;  
        owner:= ActivityOwner(ActNum); 
        doFirst:=ActNum;
        IF (OBJTYPENAME(owner)="RBDBlockObj")
           temp1:=owner;
           temp1set:=TRUE;
        END IF;
        WHILE ActNum<>group.Last  
           IF (OBJTYPENAME(owner)<>"RBDBlockObj")   
              ActName:=ActivityName(ActNum);
              IF (ActName="EndSimulation")
                 ActNum:=group.Next(ActNum); 
                 owner:=ActivityOwner(ActNum);
                 IF (OBJTYPENAME(owner)="RBDBlockObj")
                    IF NOT temp1set
                       temp1:=owner;
                       doFirst:=ActNum;
                    END IF;
                 ELSE
                    doFirst:=ActNum;
                    RETURN doFirst;
                 END IF;
              ELSE
                 doFirst:=ActNum;
                 RETURN doFirst; 
              END IF;
           ELSE  
              temp2:=owner;  
             IF (temp2.Id < temp1.Id) 
                 temp1:=owner; 
                 doFirst:=ActNum;  
              END IF;  
              ActNum:=group.Next(ActNum); 
              owner:=ActivityOwner(ActNum); 
           END IF;  
        END WHILE; 
        RETURN doFirst;
 END METHOD;
 
 ASK METHOD TimeAdvance(IN newTime : REAL) : REAL;
    VAR
    nextEventTime  : REAL;
    clickedOn      : ANYOBJ;
     block                               : RBDBlockObj;
     event                               : RBDEventObj;
     node                                : RBDNodeObj;
     hier                                : RBDHierObj;

    

BEGIN            
      IF ((TieBreaking=TRUE) AND (newTime>0.0))
         ASK EventController TO SetTieBreaking(FALSE);  
         TieBreaking:=FALSE; 
      END IF; 
      IF SystemStateChange
         AnalyzeSystem;
         SystemStateChange:=FALSE;
         IF (inStepMode OR jumpStop)
            jumpStop:=FALSE;
            FillStatusBar;
            REPEAT
               goToNext := FALSE;
               clickedOn:=ASK window TO AcceptInput();
            UNTIL goToNext;
         END IF;
      END IF;      
      IF (inEndState)
         RETURN SimTime;
      ELSE
         RETURN newTime;
      END IF;
 END METHOD;
END OBJECT;

PROCEDURE RecordReliabilityFileData();
VAR
   i,reliableIs : INTEGER; 
BEGIN
  IF (termType<>1)
     NEW(message,1..2);
     message[1]:="You are attempting to write a reliability file on a non-time terminated sim, Sherlock. "; 
     message[2]:="                     Bad idea - hope you don't crash!                             "; 
     result:=SendAlert(message,FALSE, FALSE, TRUE);
     DISPOSE(message);
  END IF;
  IF missionSuccess         {came here at the end of the simulation trial}
     reliableIs:=TRUNC((StopCriteria+0.0000001)/relMesh);      {must add 0.0000001 to SimTime due to round-off problems}
  ELSE                      {came here at a failure in the middle of a trial}
     reliableIs:= TRUNC((SimTime+0.0000001)/relMesh);
  END IF;   
  FOR i:=0 TO reliableIs    
    reliabilityFileData[i]:=reliabilityFileData[i]+1;
  END FOR;   
END PROCEDURE; {RecordReliabilityFileData}

END MODULE.
