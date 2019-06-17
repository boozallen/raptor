

PROCEDURE StartEngine(IN useGraphics,sysFailFile,sysRepFile,endSimFile,paramFile,
                         AoPlotFile,capFile,resultsFile,eorFile,eventsFile,costFile,
                         availGraph                                                    : BOOLEAN; 
                      IN costPlotInterval,aoPlotInterval,simLength,startStat,TimeSlice : REAL;
                      IN numTrials,RamInterval                                         : INTEGER);
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
     ASK keyStream TO WriteString("Ao           Do           R            RCond"+               
       "         MTBDE              MDT             MTBM              MMT");
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
     ASK RamStream TO WriteString("Ao           Sigma       Do           Sigma       R            Sigma       RCond        Sigma             "+
              "       MTBDE          Sigma               MDT          Sigma              MTBM          Sigma               MMT          Sigma");
     ASK RamStream TO WriteLn;
  END IF;
  IF ((DomainTree) OR (CoreFile))
     CreateDomainTree;
  END IF;   
  runNumber:=1;
  IF EventsFile 
     ASK EventStream TO WriteLn; 
     WriteEvents(0.000000,"System","Begin_Run_"+INTTOSTR(runNumber),"--","--");
  END IF;  
  ASK EventController TO SetTieBreaking(TRUE);
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
        IF ((block.opStatus=RepHold) OR (block.opStatus=Repairing) OR (block.opStatus=PM) 
            OR (block.opStatus=PMhold) OR (block.opStatus=Done) OR (block.activeStatus=Cut))
           ASK block TO BackFlowDependencies(Down);
        END IF;
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
     FOREACH event IN eventGroup
        IF ((event.opStatus=Failure) OR (event.activeStatus=Cut))
           ASK event TO BackFlowDependencies(Down);
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

