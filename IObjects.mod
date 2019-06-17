
{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Implementation Module : IObjects                                              +}
{+  Author                : Chuck Carter / Steve Brown / Tony Malerich            +}
{+  Last Modified         : November 2004                                         +}
{+                                                                                +}
{+  Description:  This module contains the procedures for blocks, nodes, links,   +}
{+                and pools.                                                      +}
{+                                                                                +}     
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}


IMPLEMENTATION MODULE Objects;

FROM MathMod  IMPORT SIN, COS, ATAN2, SQRT, POWER, pi, LN, EXP;  
FROM GTypes   IMPORT PointArrayType, ALL ColorType, TextBufferType, 
                     FillStyleType(SolidFill, NarrowCrosshatchFill),OptionListType;
FROM Text     IMPORT TextObj;
FROM Intract  IMPORT SendAlert,GetNumParams,GammaFunction,ComputeStats, fEmpArray, rEmpArray;
FROM Fill     IMPORT FillVObj;
FROM GProcs   IMPORT Transform;
FROM Display  IMPORT window,root,totalLinks,totalNodes,startId,endId,activePhases,fontSize,typeOfCursor,PoolArray,
                     somethingChanged, totalBlocks,flowGenerated,defaultBlock,statusBarOn,weakLinkAnalType,GYthreshold,
                     capacityAnalysis,weakAnalysis,costAnalysis,YRthreshold,blockGUIColor,linkWorkColor,totalTriggers,
                     selectGroup,selectedLinksGroup,termType,writeBlkEvs,writeNdEvs,writeSysEvs,writePhsEvs,
                     writeOnlyItemEvs,nodesIn,blocksIn,hiersIn,nextLinkId,nextId,blockGroup,eventGroup,nodeGroup,hierGroup,
                     linkGroup,totalEvents,copied,copiedFromPrevFile,detComp, C130File;
FROM Runsim   IMPORT BlockDepGroup,FevDepGroup,NodeDepGroup,BlockRepCost,BlockOpCost,BlockSpCost,BlockIdleCost,
                     BlockHoldCost,BlockSBCost,BlockDoneCost,BlockESCost,BlockDispCost,NodeGreenTime,NodeYellowTime,
                     NodeRedTime,NodeBrownTime,NodeBlueTime,NodeOrangeTime,BlockTrigger,System,RepTimeStream, EventStream,
                     SysStream,FailStream,RepairStream,SystemUp,redString,EventFailStream,SysRepTimeFile,EventsFile,
                     CapFile,StatsStarted,SysTimeToRepair,LastUpDownChange,OutOfSpares,currNumWaits,currNumArrivals,
                     StartStats,StopCriteria,runCompleted,phaseNumber, PooledSpares, CustomSpares, CompTimeToFail,
                     SparesUsed,CompTimeToRepair, CompTimeToDoPM,SparesAvailable, PoolResAvailable,TimeOfCompFailure,
                     ALL SystemStatusType,PoolAddCost,PoolESCost,ColdChangeGroup,simInProgress,NodeFlow,NodeCapacity,
                     CapacityStream,hierDE,CountFailure,autoStopped,pmSyncTrigger,REALTOSigFigSTR, PhaseChangeInProgress,
                     NodeFailureGroup, FEVmode,NumCurrentlyFailed,pmDeferGroup,BlockPMCost,BlockpmHoldCost,BlockRunTime,
                     BlockStandbyTime,BlockRepairTime,BlockRepHoldTime,BlockIdleTime,BlockPMTime,BlockPMHoldTime,
                     BlockDoneTime,blockMissionBool,nodeMissionBool,termCounter,EventRunTime,EventStandbyTime,
                     EventRepairTime,EventOpCost,EventRepCost,eventMissionBool,nodeHasDepsGroup,blockHasDepsGroup,
                     eventHasDepsGroup, C130Stream, lastFlow     {cmc capFix}  ; 
FROM SimMod   IMPORT Interrupt, SimTime;
FROM Button   IMPORT ButtonObj;
FROM Value    IMPORT ValueBoxObj;
FROM Label    IMPORT LabelObj;
FROM TextBox  IMPORT ComboBoxObj;
FROM OSMod    IMPORT Delay;
FROM Analyze  IMPORT LoopCheck,LoopError,PropagatedGroup,DownstreamGroup,analysisType,TestElement,RandomGenObj,
                     totalCapNodes;


VAR
   message                                                                : TextBufferType;
   result,coldsAffected,checkingSystemCap,skipBackflow, specialBackFlow   : BOOLEAN; 
   startUpFailures    :BOOLEAN;                        {cmc StartUpFailures}
              
PROCEDURE InitFactorySettings;
BEGIN
NEW(factBools, 1..18);
NEW(factReals, 1..28);  
NEW(factInts,  1..17);
NEW(factStrs,  1..6);
NEW(factFparams,1..2);
NEW(factRparams,1..2);
NEW(factPreParams,1..1);
NEW(factPostParams,1..1);
NEW(factPMParams,1..1);
   {see Procedure SetBlockData for a description of each item}
   factBools[1] := TRUE; factBools[2] := FALSE; factBools[3] := FALSE; factBools[4] := FALSE;
   factBools[5] := FALSE; factBools[6] := FALSE; factBools[7] := FALSE; factBools[8] := FALSE;
   factBools[9] := FALSE; factBools[10] := FALSE; factBools[11]:= FALSE; factBools[12]:= TRUE;
   factBools[13]:= TRUE;  factBools[14] := FALSE;  factBools[15]:= TRUE;  factBools[16]:= FALSE;
   factBools[17]:= TRUE;  factBools[18] := FALSE;
   factReals[1] := 720.; factReals[2] := 24.;  factReals[3] := 24.;
   factReals[4] := 0.;   factReals[5] := 1.;   factReals[6] := 1.;
   factReals[7] := 1.;   factReals[8] := 1.;   factReals[9] := 1.;
   factReals[10] := 1.;  factReals[11] := 1.;  factReals[12] := 1.;
   factReals[13] := 1.;  factReals[14] := 1.;  factReals[15] := 1.;
   factReals[16] := 1.;  factReals[17] := 1.;  factReals[18] := 1.;
   factReals[19] := 0.;  factReals[20] := 0.;  factReals[21] := 100.;
   factReals[22] := 1.;  factReals[23] := 1.;  factReals[24] := 0.;
   factReals[25] := 100.; factReals[26] := 0.; factReals[27] :=0.;
   factReals[28] := 75.;
   factInts[1]  := 4;    factInts[2]  := 19;    factInts[3]  := 7;
   factInts[4]  := 19;    factInts[5]  := 100;  factInts[6]  := 1;
   factInts[7]  := 0;    factInts[8]  := 1;    factInts[9]  := 0;
   factInts[10]  := 0;   factInts[11]  := 203;   factInts[12]  := 203;
   factInts[13]  := 3;   factInts[14]  := 1;   factInts[15] := 0; 
   factInts[16] := 19;   factInts[17]  := 1;
   factStrs[1]  := "unnamed"; factStrs[2] := "unnamed"; factStrs[3] := "unnamed";
   factStrs[4]  := "none";    factStrs[5] :="";
   factFparams[1]    := 100.;   factFparams[2]    := 0.;
   factRparams[1]    := 10.;   factRparams[2]    := 2.;
   factSparing := Infinite;
   factPreParams[1]:=0.;
   factPostParams[1]:=0.;
   factPMParams[1]:=1.;
   ASK defaultBlock TO SetBlockData(factBools, factInts, factReals,factStrs, factFparams, 
                    factRparams, factPreParams,factPostParams,factPMParams,factSparing);
END PROCEDURE; {InitFactorySettings}

PROCEDURE arrowhead (IN  connectInType                : STRING;
                     IN  startX, startY, endX, endY   : REAL;
                     OUT p1X, p1Y, p2X, p2Y, p3X, p3Y : REAL);
CONST 
   oldphi = 15.; {arrowhead angle}
   radius = 0.2;
VAR
   delta, phi,lineX,lineY,unitX,unitY,theta,
   leftRelX,leftRelY,rightRelX,rightRelY, z   : REAL;   
BEGIN  
   IF (startX = endX) AND (startY = endY)
      p1X := startX;
      p1Y := startY;
      p2X := startX;
      p2Y := startY;
      p3X := startX;
      p3Y := startY;
   ELSE
      lineX := endX - startX;
      lineY := endY - startY; 
      unitX := lineX / SQRT(POWER(lineX,2.) + POWER(lineY,2.));
      unitY := lineY / SQRT(POWER(lineX,2.) + POWER(lineY,2.));  
      phi := oldphi * pi / 180.;
      theta := ATAN2(lineY, lineX); 
      IF connectInType = "RBDBlock"
         IF (theta >= -0.3925) AND (theta < 0.3925)
            delta := 0.25; {distance the tip of the arrowhead is away from the end of the line}
         ELSIF (theta >= 0.3925)  AND (theta < 1.178)
            delta := 0.4;
         ELSIF (theta >= 1.178) AND (theta < 1.963)
            delta := 0.25;
         ELSIF (theta >= 1.963) AND (theta < 2.748)
            delta := 0.4;
         ELSIF (theta >= -1.178) AND (theta < -0.3925)
            delta := 0.4;
         ELSIF (theta >= -1.963) AND (theta < -1.178)
            delta := 0.22;
         ELSIF (theta >= -2.748) AND (theta < -1.963)
            delta := 0.4;
         ELSE
            delta := 0.3;
         END IF;
      ELSIF connectInType = "RBDHier"
         z:=ABS(theta);
         IF z>1.5708
            z:=3.1415-z;
         END IF;   
         IF (z >= -0.1) AND (z < 0.2618)
            delta := 0.65;   {0-15 degrees}
         ELSIF (z >= 0.2618) AND (z < 0.5236)
            delta := 0.55;  {15-30 degrees}
         ELSIF (z >= 0.5236) AND (z < 0.6)
            delta := 0.5;  {30-45 degrees}
         ELSIF (z >= 0.6) AND (z < 0.7854)
            delta := 0.4;  {30-45 degrees}
         ELSIF (z >= 0.7854) AND (z < 1.0472)
            delta := 0.35;  {45-60 degrees}
         ELSIF (z >= 1.0472) AND (z < 1.3090)
            delta := 0.30;  {60-75 degrees}
         ELSIF (z >= 1.3090) AND (z < 1.58)
            delta := 0.25;  {75-90 degrees}
         ELSE
            delta := 2.65;  {shouldn't happen}
         END IF;
     ELSE
         IF (theta >= -0.3925) AND (theta < 0.3925)
            delta := 0.25;
         ELSIF (theta >= 0.3925)  AND (theta < 1.178)
            delta := 0.24;
         ELSIF (theta >= 1.178) AND (theta < 1.963)
            delta := 0.24;
         ELSIF (theta >= 1.963) AND (theta < 2.748)
            delta := 0.23;
         ELSIF (theta >= -1.178) AND (theta < -0.3925)
            delta := 0.28;
         ELSIF (theta >= -1.963) AND (theta < -1.178)
            delta := 0.26;
         ELSIF (theta >= -2.748) AND (theta < -1.963)
            delta := 0.28;
         ELSE
            delta := 0.22;
         END IF;
      END IF;
      leftRelX := -radius * COS(theta) * COS(phi) - radius * SIN(theta) * SIN(phi);
      leftRelY := -radius * SIN(theta) * COS(phi) + radius * COS(theta) * SIN(phi);  
      rightRelX := -radius * COS(theta) * COS(phi) + radius * SIN(theta) * SIN(phi);
      rightRelY := -radius * SIN(theta) * COS(phi) - radius * COS(theta) * SIN(phi); 
      p1X := endX - delta * unitX;
      p1Y := endY - delta * unitY; 
      p2X := p1X + leftRelX;
      p2Y := p1Y + leftRelY;  
      p3X := p1X + rightRelX;
      p3Y := p1Y + rightRelY; 
   END IF;
END PROCEDURE; {arrowhead}

PROCEDURE CheckColds;
VAR
   node                        : RBDNodeObj;
   i,path                      : INTEGER;  
   link                        : LinkObj;
   someColdsChanged,completed  : BOOLEAN;
BEGIN
   completed:=FALSE;
   skipBackflow:=FALSE;   
   WHILE NOT completed
      someColdsChanged:=FALSE;
      WHILE coldsAffected
         someColdsChanged:=TRUE;
         coldsAffected:=FALSE;
         FOREACH node IN ColdChangeGroup
               ASK node TO RunColdLogic;
               ASK ColdChangeGroup TO RemoveThis(node);    {CSB Speed Change}
         END FOREACH;  
      END WHILE;
      completed:=TRUE;
      IF someColdsChanged
         skipBackflow:=TRUE;  {Insure blocks connected to standby node stay idle after ResendBackFlow}
         ResendBackFlow;         
      END IF;
      IF coldsAffected
         completed:=FALSE;
      END IF;            
   END WHILE;
   skipBackflow:=FALSE;
END PROCEDURE;    {CheckColds}

PROCEDURE AnalyzeSystem;
VAR
   i,j,currentCap,LostFlow                       : INTEGER;
   node,startNode,endNode                        : RBDNodeObj;
   block                                         : RBDBlockObj;
   link,tempLink                                 : LinkObj;
   currentSysStatus,newSysStatus                 : SystemStatusType;
   oldTimescale                                  : REAL;
   outString,statusString,commentString          : STRING;
   flowLabel                                     : TextObj;
   pathFound                                     : BOOLEAN;  
   hier                                          : RBDHierObj;
BEGIN
   currentSysStatus:=System.Status;
   IF ((NumCurrentlyFailed=0) AND (SystemUp))  
      newSysStatus:=GGreen;
   ELSIF ((NumCurrentlyFailed>0) AND (SystemUp))
      newSysStatus:=YYellow;
   ELSE
      newSysStatus:=RRed;
   END IF;
   IF totalHiers>0
       FOREACH hier IN hierGroup
           node:=ASK root Child("RBDNode", hier.outID);
           IF ((hier.numCurrentlyFailed>0) AND (node.Status=AllUp))
              ASK node TO ChangeNodeState(Degraded,node.activeStatus);
           ELSIF ((hier.numCurrentlyFailed=0) AND (node.Status=Degraded))
              ASK node TO ChangeNodeState(AllUp,node.activeStatus);
           END IF;      
       END FOREACH;   
   END IF; 
   IF currentSysStatus<>newSysStatus
      ASK System TO ChangeStatus(newSysStatus,System.missionStatus);
   ELSIF ((currentSysStatus=RRed) AND (newSysStatus=RRed))
      CapAnalysisRequired:=FALSE;
      IF (SimTime>LastUpDownChange)
         IF ((EventsFile) AND (System.Status<>DDone) AND (NOT runCompleted)) 
            commentString:="";
            FOREACH block IN blockGroup
               IF block.activeStatus=Cut
                  commentString:=commentString+"_c"+INTTOSTR(block.Id); 
               ELSE
                  IF block.opStatus=Repairing 
                     commentString:=commentString+"_r"+INTTOSTR(block.Id); 
                  ELSIF ((block.opStatus=RepHold) OR (block.opStatus=PMhold))
                     commentString:=commentString+"_h"+INTTOSTR(block.Id); 
                  ELSIF block.opStatus=Done
                     commentString:=commentString+"_d"+INTTOSTR(block.Id); 
                  ELSIF block.opStatus=PM
                     commentString:=commentString+"_p"+INTTOSTR(block.Id); 
                  END IF;
               END IF;              
            END FOREACH;
            IF commentString<>redString
               redString:=commentString; 
              IF commentString = ""
                 WriteEvents(SimTime,"System","Red_change","Red","System_status_change");
              ELSE
                 WriteEvents(SimTime,"System","Red_change","Red",commentString);
              END IF;   
            END IF;
         END IF;    {EventsFile}
      END IF;  {simTime>LastUpDownChange}
   END IF;
   IF ((SimTime=0.0) OR FEVmode)
      CapAnalysisRequired:=TRUE;
   END IF;
   IF (capacityAnalysis AND (NOT runCompleted) AND (CapAnalysisRequired))
       startNode:=ASK root Child("RBDNode",startId);
       endNode := ASK root Child("RBDNode",endId);
       FOREACH link IN capLinkGroup
          ASK link TO SetActualFlow(0);
          ASK link TO SetPathFull(FALSE);
       END FOREACH;
       FOREACH node IN capNodeGroup
          ASK node TO ResetNodeFlow;
          IF (GraphicsOutput AND (node.typeNode=2))
             ASK node TO ShowFlow;
          END IF;
       END FOREACH;
       IF  FEVmode
          ASK startNode TO DistributeFlow(flowGenerated, LostFlow);
       ELSE
          IF (newSysStatus<>RRed)
             IF (NOT FlowingAtMaxCap) 
                checkingSystemCap:=TRUE;
                ASK startNode TO DistributeFlow(999999999, LostFlow);
                checkingSystemCap:=FALSE;
                NodeCapacity[startNode.seqNum]:=999999999-LostFlow;       
                FOREACH link IN capLinkGroup
                   ASK link TO SetActualFlow(0);
                   ASK link TO SetPathFull(FALSE);
                END FOREACH;
                FOREACH node IN capNodeGroup
                   ASK node TO ResetNodeFlow;
                END FOREACH;             
             END IF;
             ASK startNode TO DistributeFlow(flowGenerated, LostFlow);
             IF FlowingAtMaxCap
                NodeCapacity[startNode.seqNum]:=flowGenerated-LostFlow;
             END IF;
          ELSE
             NodeCapacity[startNode.seqNum]:=0;
             LostFlow:=flowGenerated;
          END IF;
       END IF;
       IF GraphicsOutput
          flowLabel:=ASK startNode Child("RBDNodeKofN", 0);
          ASK flowLabel TO SetText("Flow="+INTTOSTR(flowGenerated-LostFlow));
          ASK flowLabel TO Draw;
       END IF;
       IF NOT FEVmode
          FOREACH node IN capNodeGroup
             IF (node.Id<>endId)
                NodeFlow[node.seqNum]:=node.nodeFlow;
             END IF; 
             currentCap:=0;
             IF ((node.Status=AllUp) OR (node.Status=Degraded))          
                IF node.fullFlow
                   currentCap:=999999999;
                ELSE
                   FOR j:=1 TO node.EconnectIntoNum 
                      link := ASK root Child("RBDLink",node.inPathsArray[j]);
                      IF link.Status=LinkUp
                         currentCap:=currentCap+link.simMaxFlow;
                      END IF;
                   END FOR;
                END IF;
             ELSE
                currentCap:=0;
             END IF;
             IF node.Id<>startId
                NodeCapacity[node.seqNum]:=currentCap;
             END IF;
          END FOREACH;
          NodeFlow[endNode.seqNum]:=NodeFlow[startNode.seqNum];     {set NodeFlow for end node equal to start node's}
       END IF;
      { IF GraphicsOutput
          FOREACH link IN linkGroup   {capLinkGroup}
             ASK link TO ShowFlowStatus;
          END FOREACH;
       END IF;  }
       IF CapFile         
             startNode:=ASK root Child("RBDNode",startId);
             outString:=SUBSTR(1,20,REALTOSTR(SimTime)+"                    ")+"  ";
             outString:=outString+SUBSTR(1,9,INTTOSTR(NodeFlow[startNode.seqNum])+"          ")+"  ";
             outString:=outString+SUBSTR(1,9,INTTOSTR(NodeCapacity[startNode.seqNum])+"          ")+"  ";
             IF (NodeFlow[startNode.seqNum]<>lastFlow)         {cmc capFix}    
                ASK CapacityStream TO WriteString(outString);
                ASK CapacityStream TO WriteLn;
                lastFlow:= NodeFlow[startNode.seqNum];         {cmc capFix}
             END IF;
       END IF;
    END IF;
    CapAnalysisRequired:=FALSE;
END PROCEDURE;       {AnalyzeSystem - new version}

PROCEDURE DetermineMaxCapacity(OUT maxCap : INTEGER);
VAR
   node                           : RBDNodeObj;
   link                           : LinkObj;
   LostFlow                       : INTEGER;
BEGIN
  FOREACH link IN capLinkGroup
     ASK link TO SetActualFlow(0);
     ASK link TO SetPathFull(FALSE);
  END FOREACH;
  FOREACH node IN capNodeGroup
     ASK node TO ResetNodeFlow;
  END FOREACH;
  node:=ASK root Child("RBDNode", startId);
  checkingSystemCap:=TRUE;
  ASK node TO DistributeFlow(999999999, LostFlow);
  checkingSystemCap:=FALSE;
  maxCap:=999999999-LostFlow; 
  FOREACH link IN capLinkGroup
     ASK link TO SetActualFlow(0);
     ASK link TO SetPathFull(FALSE);
  END FOREACH;
  FOREACH node IN capNodeGroup
     ASK node TO ResetNodeFlow;
  END FOREACH;
END PROCEDURE;

PROCEDURE WriteEvents   (IN time                        : REAL;
                         IN typeString,nameString,
                            actionString,commentString  : STRING);
VAR
   timeString,outString,tab                             : STRING;
   print                                                : BOOLEAN;
BEGIN
  print:=FALSE;
  IF (detComp<>"")
     IF (nameString=detComp)
        print:=TRUE;
     END IF;   
  END IF;   
  CASE typeString
     WHEN "Block":
        IF (writeBlkEvs)
           print:=TRUE;
        END IF;   
     WHEN "Node":      
        IF (writeNdEvs)
           print:=TRUE;
        END IF;   
     WHEN "System":    
         IF ((nameString="Phase_change")  AND (writePhsEvs))
            print:=TRUE;
         ELSIF (writeSysEvs)
            print:=TRUE;
         END IF;   
     WHEN "Pool":
         IF (writeSysEvs)
            print:=TRUE;
         END IF;   
     WHEN "Trigger":
         IF (writeSysEvs)
            print:=TRUE;
         END IF;   
     WHEN "Hier":
         IF (writeSysEvs)
            print:=TRUE;
         END IF; 
     OTHERWISE
        {do nothing}
  END CASE;   
  IF ((NOT writeBlkEvs) AND (NOT writeNdEvs) AND (NOT writeSysEvs) AND (NOT detComp<>"") AND (NOT writePhsEvs))
     print:=TRUE;         {tony - very awkward - can improve?}
  END IF;
  
  IF print
     tab:=9C;
     timeString:=REALTOSTR(time)+"                ";
     timeString:=SUBSTR(1,16,timeString+"                ")+"  "+tab;
     typeString:=SUBSTR(1,7,typeString+"     ")+"  "+tab;
     nameString:= SUBSTR(1,20,nameString+"                    ")+"  "+tab;
     actionString:=SUBSTR(1,15,actionString + "             ")+"  "+tab;
     outString:=timeString+typeString+nameString+actionString+commentString;
     ASK EventStream TO WriteString(outString); 
     ASK EventStream TO WriteLn; 
  END IF;   
END PROCEDURE;                          

PROCEDURE GetStream      (IN usesStream             : INTEGER;
                          IN typeString             : STRING;
                          IN idStream               : INTEGER;
                          OUT drawStream            : RandomGenObj);
VAR
  arrayIndex   : INTEGER;
BEGIN
  IF usesStream > 200
     arrayIndex:=usesStream-200;
     drawStream:=SysStream[arrayIndex];
  ELSIF (typeString="Event")
     drawStream:=EventFailStream[idStream];
  ELSIF (typeString="Fail")
     drawStream:=FailStream[idStream];  
  ELSIF (typeString="Repair")
     drawStream:=RepairStream[idStream];
  END IF; 
END PROCEDURE;  
                        
PROCEDURE RunBogusRBD;
VAR
   bogus        :     BogusRBD;
BEGIN
   NEW(bogus);
   TELL bogus TO Run IN 0.0;
   StartSimulation;
   DISPOSE(bogus);               
END PROCEDURE;


PROCEDURE ResendBackFlow;  
VAR
     tempBlock : RBDBlockObj;
     tempNode  : RBDNodeObj;
     tempEvent : RBDEventObj;
BEGIN
  specialBackFlow:=TRUE;  
  FOREACH tempBlock IN blockHasDepsGroup
     IF tempBlock.activeStatus=Cut
        ASK tempBlock TO BackFlowDependencies(Down);
     ELSIF  tempBlock.activeStatus=Active
        IF ((tempBlock.opStatus=Repairing) OR (tempBlock.opStatus=PM)  
                     OR (tempBlock.opStatus=RepHold) OR (tempBlock.opStatus=PMhold))
           ASK tempBlock TO BackFlowDependencies(Down);
        END IF;
     END IF; 
  END FOREACH;
  FOREACH tempEvent IN eventHasDepsGroup
     IF tempEvent.activeStatus=Cut
           ASK tempEvent TO BackFlowDependencies(Down);      
     ELSIF tempEvent.activeStatus=Active   
        IF (tempEvent.opStatus=Failure)
           ASK tempEvent TO BackFlowDependencies(Down);
        END IF;
     END IF;
  END FOREACH;
  FOREACH tempNode IN nodeHasDepsGroup
     IF (tempNode.Status=Down)
        ASK tempNode TO BackFlowDependencies(Down);
     END IF;
  END FOREACH;
  specialBackFlow:=FALSE;
END PROCEDURE;     {ResendBackFlow}

                        
OBJECT BogusRBD;
   TELL METHOD Run;
      BEGIN
         WAIT DURATION 1.0;
         END WAIT;
         StopSimulation;
   END METHOD;
END OBJECT;            
                        
OBJECT RBDHierObj;      
   ASK METHOD SetName (IN newName : STRING);                              
   VAR
     label : TextObj;
   BEGIN
      name := newName;
      ToolTip:=name;
      label := Child("HierLabel", 0);
      ASK label TO SetText(newName);
      ASK label TO SetColor(Black);
      IF fontSize = 1
         ASK label TO SetSysFont("SmallFonts",fontSize,70,0); {add this line}
         ASK label TO SetHidden(TRUE);
      ELSE
         ASK label TO SetSysFont("SmallFonts",fontSize,70,0);
      END IF;
      ASK label TO Draw;
   END METHOD; 

   ASK METHOD SetConnectToNode(IN isConnected : BOOLEAN);
   BEGIN
      isConnectedNode := isConnected;
   END METHOD; {SetConnectToNode}

   ASK METHOD SetZoom (IN newZoom :  REAL);                              
   BEGIN
      zoom := newZoom;
   END METHOD; {SetZoom}  
 
   ASK METHOD SetLevel (IN newLevel : INTEGER);                              
   BEGIN
      level := newLevel;
   END METHOD; {SetZoom}  
 
   ASK METHOD SetInID(IN newValue : INTEGER);
   BEGIN
      inID:=newValue;
   END METHOD; 

   ASK METHOD SetOutID(IN newValue : INTEGER);
   BEGIN
      outID:=newValue;
   END METHOD; 

  ASK METHOD CreateChildGroup;
  BEGIN
     NEW(childGroup);
  END METHOD;
  
  ASK METHOD AugmentSelectGroup;
  VAR 
     child                    : ANYOBJ;
     tempHier                 : RBDHierObj;
     i                        : INTEGER;
  BEGIN
     FOREACH child IN childGroup
        IF OBJTYPENAME(child) = "LinkObj";
           IF selectedLinksGroup.Includes(child);
           ELSE
              ASK selectedLinksGroup TO Add(child);
              INC(linksIn);
           END IF;
        ELSIF OBJTYPENAME(child) = "RBDBlockObj"
           IF selectGroup.Includes(child);
           ELSE
              ASK selectGroup TO Add(child);
              INC(blocksIn);
           END IF;
        ELSIF OBJTYPENAME(child) = "RBDEventObj"
           IF selectGroup.Includes(child);
           ELSE
              ASK selectGroup TO Add(child);
              INC(eventsIn);
           END IF;
        ELSIF OBJTYPENAME(child) = "RBDNodeObj"
           IF selectGroup.Includes(child);
           ELSE
              ASK selectGroup TO Add(child);
              INC(nodesIn);
           END IF;
        ELSIF OBJTYPENAME(child) = "RBDHierObj"
           IF selectGroup.Includes(child);
           ELSE
              ASK selectGroup TO Add(child);
              INC(hiersIn);
           END IF;
        END IF;
     END FOREACH;
     FOREACH tempHier IN hierGroup
        IF tempHier.parentID = Id
           ASK tempHier TO AugmentSelectGroup;
        END IF;
     END FOREACH;
  END METHOD;

  ASK METHOD ChangeStatus(IN newcolor : NodeStatusType);
     VAR
        hierImage,dsi           : ImageObj;
        tempHier                : RBDHierObj;
        oldDSIsignal,oldStatus  : NodeStatusType;
        gre,yel,sie,blu,red,ora : INTEGER;
     BEGIN
        oldStatus:=Status;
        Status:=newcolor;
        IF (GraphicsOutput AND (oldStatus<>newcolor))   
           hierImage := Descendant("Hier", 603); 
           CASE newcolor
              WHEN AllUp:
                 ASK hierImage TO SetColor(LimeGreen);
                 ASK hierImage TO SetTranslation(hierImage.Translation.x, hierImage.Translation.y);
              WHEN Degraded:
                 ASK hierImage TO SetColor(Yellow);
                 ASK hierImage TO SetTranslation(hierImage.Translation.x, hierImage.Translation.y);
              WHEN NodeIdle:
                 ASK hierImage TO SetColor(Sienna);
                 ASK hierImage TO SetTranslation(hierImage.Translation.x, hierImage.Translation.y);
              WHEN NodeStandby:
                 ASK hierImage TO SetColor(Blue);
                 ASK hierImage TO SetTranslation(hierImage.Translation.x, hierImage.Translation.y);
              WHEN Down:
                 ASK hierImage TO SetColor(Red);
                 ASK hierImage TO SetTranslation(hierImage.Translation.x, hierImage.Translation.y);
              WHEN NodePM:
                 ASK hierImage TO SetColor(Orange);
                 ASK hierImage TO SetTranslation(hierImage.Translation.x, hierImage.Translation.y);
              OTHERWISE 
                 NEW(message,1..1);
                 message[1]:="Unknown hierarchy status color!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                 DISPOSE(message);   
           END CASE;
           Draw; 
           IF NOT runCompleted
              IF (parentID > 0)
                 oldDSIsignal:=DSIsignal;
                 IF (Status > dsiState)     {newColor worse than }
                    DSIsignal:=Status;
                 ELSE
                    DSIsignal:=dsiState;
                 END IF;
                 tempHier := ASK root Child("RBDHier", parentID);
                 IF ( ((oldStatus=AllUp) OR (oldStatus=Degraded)  OR (oldStatus=NodeStandby)) AND
                      ((Status=Down) OR (Status=NodeIdle) OR (Status=NodePM))   ) 
                    ASK tempHier TO IncNumFailed;
                 ELSIF ( ((Status=AllUp) OR (Status=Degraded)  OR (Status=NodeStandby)) AND
                      ((oldStatus=Down) OR (oldStatus=NodeIdle) OR (oldStatus=NodePM))   ) 
                    ASK tempHier TO DecNumFailed;
                 END IF;
                 IF DSIsignal<>oldDSIsignal;              
                    CASE oldDSIsignal
                       WHEN AllUp:
                          gre:=-1;
                       WHEN Degraded:
                          yel:=-1;
                       WHEN NodeIdle:
                          sie:=-1;
                       WHEN NodeStandby:
                          blu:=-1;
                       WHEN Down:
                          red:=-1;
                       WHEN NodePM:
                          ora:=-1;
                       OTHERWISE 
                          NEW(message,1..1);
                          message[1]:="Unknown hierarchy status color!     "; 
                          result:=SendAlert(message,FALSE, FALSE, TRUE);
                          DISPOSE(message);   
                    END CASE;
                    CASE DSIsignal
                       WHEN AllUp:
                          gre:=1;
                       WHEN Degraded:
                          yel:=1;
                       WHEN NodeIdle:
                          sie:=1;
                       WHEN NodeStandby:
                          blu:=1;
                       WHEN Down:
                          red:=1;
                       WHEN NodePM:
                          ora:=1;
                       OTHERWISE 
                          NEW(message,1..1);
                          message[1]:="Unknown hierarchy status color!     "; 
                          result:=SendAlert(message,FALSE, FALSE, TRUE);
                          DISPOSE(message);   
                    END CASE;
                    ASK tempHier TO ChangeDSIStatus(gre,blu,yel,sie,ora,red);                 
                 END IF;
              END IF;  {parentId>0}
           END IF;  {runCompleted}
        END IF; {GraphicsOutput AND (oldStatus<>newcolor)}
        IF (GraphicsOutput AND runCompleted)
            dsi:= Descendant("HierMid", 0);
            ASK dsi TO SetColor(LimeGreen);
            ASK dsi TO Draw;   {cmc}
        END IF;
  END METHOD;  {HierObj - ChangeStatus}
  
  ASK METHOD ChangeDSIStatus(IN gre,blu,yel,sie,ora,red         : INTEGER);
  VAR
     dsi                                   : ImageObj;
     tempHier                              : RBDHierObj;
     newState,oldState, oldDSIsignal       : NodeStatusType;
     node                                  : RBDNodeObj;
  BEGIN
     oldState:=dsiState;
     dsiBlues:=dsiBlues+blu;
     dsiYellows:=dsiYellows+yel;
     dsiSiennas:=dsiSiennas+sie;
     dsiOranges:=dsiOranges+ora;
     dsiReds:=dsiReds+red;  
     IF dsiReds>0
        newState:=Down;
     ELSIF dsiOranges>0;
        newState:=NodePM;
     ELSIF dsiSiennas>0;
        newState:=NodeIdle;
     ELSIF dsiYellows>0;
        newState:=Degraded;
     ELSIF dsiBlues>0;
        newState:=NodeStandby;
     ELSE;
        newState:=AllUp;
     END IF;     
     dsi:= Descendant("HierMid", 0);
     node:=ASK root Child("RBDNode",outID);
     IF (node.activeStatus=Linked)    
        ASK dsi TO SetColor(White);
     ELSIF (node.activeStatus=Cut)    
        ASK dsi TO SetColor(Black);
     ELSE
        CASE newState
           WHEN AllUp:
              ASK dsi TO SetColor(LimeGreen);
           WHEN Degraded:
              ASK dsi TO SetColor(Yellow);
           WHEN NodeIdle:
              ASK dsi TO SetColor(Sienna);
           WHEN NodeStandby:
              ASK dsi TO SetColor(Blue);
           WHEN Down:
              ASK dsi TO SetColor(Red);
           WHEN NodePM:
              ASK dsi TO SetColor(Orange);
           OTHERWISE 
              NEW(message,1..1);
              message[1]:="Unknown DSI status color!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message);   
        END CASE;
     END IF;
     Draw; 
     IF newState<>oldState
        dsiState:=newState;  
        IF (parentID > 0)
           oldDSIsignal:=DSIsignal;
           IF (Status > dsiState)     {own color worse than descendents}
              DSIsignal:=Status;
           ELSE
              DSIsignal:=dsiState;
           END IF;
           IF DSIsignal<>oldDSIsignal; 
              gre:=0;
              yel:=0;
              sie:=0;
              blu:=0;
              red:=0;
              ora:=0;
              CASE oldDSIsignal
                 WHEN AllUp:
                    gre:=-1;
                 WHEN Degraded:
                    yel:=-1;
                 WHEN NodeIdle:
                    sie:=-1;
                 WHEN NodeStandby:
                    blu:=-1;
                 WHEN Down:
                    red:=-1;
                 WHEN NodePM:
                    ora:=-1;
                 OTHERWISE 
                    NEW(message,1..1);
                    message[1]:="Unknown hierarchy status color!     "; 
                    result:=SendAlert(message,FALSE, FALSE, TRUE);
                    DISPOSE(message);   
              END CASE;
              CASE DSIsignal
                 WHEN AllUp:
                    gre:=1;
                 WHEN Degraded:
                    yel:=1;
                 WHEN NodeIdle:
                    sie:=1;
                 WHEN NodeStandby:
                    blu:=1;
                 WHEN Down:
                    red:=1;
                 WHEN NodePM:
                    ora:=1;
                 OTHERWISE 
                    NEW(message,1..1);
                    message[1]:="Unknown hierarchy status color!     "; 
                    result:=SendAlert(message,FALSE, FALSE, TRUE);
                    DISPOSE(message);   
              END CASE;
              tempHier := ASK root Child("RBDHier", parentID);
              ASK tempHier TO ChangeDSIStatus(gre,blu,yel,sie,ora,red);
           END IF;
        END IF;
     END IF;
  END METHOD;     {ChangeDSIStatus}
  
  ASK METHOD IncNumFailed;
  BEGIN
     INC(numCurrentlyFailed);
  END METHOD;
  
  ASK METHOD DecNumFailed;
  BEGIN
     DEC(numCurrentlyFailed);
  END METHOD;
  
  ASK METHOD SetToInitValues;
  VAR
     dsi, hierImage       : ImageObj;
  BEGIN
     Status:=AllUp;
     numCurrentlyFailed:=0;
     IF GraphicsOutput
        dsi:= Descendant("HierMid", 0);
        hierImage := Descendant("Hier", 603); 
        ASK dsi TO SetColor(LimeGreen);
        ASK hierImage TO SetColor(LimeGreen);
        Draw;
        dsiBlues:=0;
        dsiYellows:=0;
        dsiSiennas:=0;
        dsiOranges:=0;
        dsiReds:=0;
        DSIsignal:=AllUp;
     END IF;
  END METHOD;

  ASK METHOD CleanUp;
  VAR
     dsi, hierImage       : ImageObj;
  BEGIN
     dsi:= Descendant("HierMid", 0);
     hierImage := Descendant("Hier", 603); 
     ASK dsi TO SetColor(Black);         
     ASK hierImage TO SetColor(blockGUIColor); 
     Draw;
     numCurrentlyFailed:=0;
     dsiBlues:=0;
     dsiYellows:=0;
     dsiSiennas:=0;
     dsiOranges:=0;
     dsiReds:=0;
     DSIsignal:=AllUp;
     Status:=AllUp;
  END METHOD;
  
  ASK METHOD SetxCenter (IN newValue                              : REAL);  
  BEGIN 
     xCenter:=newValue;
  END METHOD;   
      
  ASK METHOD SetyCenter (IN newValue                              : REAL);        
  BEGIN
     yCenter:=newValue;
  END METHOD;   
   
  ASK METHOD SetLocation(IN xLoc, yLoc               : REAL);
  BEGIN
     xPosition := xLoc;
     yPosition := yLoc;
  END METHOD;

  ASK METHOD SetOrigin    (IN X, Y                                : REAL);
  BEGIN
     xOrigin := X;
     yOrigin := Y;
  END METHOD;
  
   ASK METHOD SetOldID(IN newValue : INTEGER);
   BEGIN
      oldID:=newValue;
   END METHOD; 
   
  ASK METHOD SetmyDepth   (IN newValue                            : INTEGER);
  BEGIN
     myDepth:=newValue;
  END METHOD; 
  
  ASK METHOD CalculateDepth (OUT depth : INTEGER);   
   VAR
      tempHier  : RBDHierObj;
      child     : ANYOBJ;
      currDepth : INTEGER;
   BEGIN
      depth := level;
      currDepth := 0;
      FOREACH child IN childGroup
         IF OBJTYPENAME(child) = "RBDHierObj"
            tempHier := RBDHierObj(child);
            ASK tempHier TO CalculateDepth(currDepth);
         END IF;
         IF currDepth > depth 
            depth := currDepth;
         END IF;
      END FOREACH;
   END METHOD;
   
   ASK METHOD SetChildLevels;
   VAR
      child : ANYOBJ;
      tempHier, tempHier2 : RBDHierObj;
   BEGIN
      FOREACH child IN childGroup
         IF OBJTYPENAME(child) = "RBDHierObj"
            tempHier := RBDHierObj(child);
            IF tempHier.level = deepestLevel
               deepestLevel := 0;
            END IF;
            ASK tempHier TO SetLevel(level+1);
            IF deepestLevel = 0
               FOREACH tempHier2 IN hierGroup
                  IF tempHier2.level > deepestLevel
                     deepestLevel := tempHier2.level
                  END IF;
               END FOREACH;
            ELSIF tempHier.level > deepestLevel
               deepestLevel := tempHier.level;
            END IF;
            ASK tempHier TO SetChildLevels;
         END IF;
      END FOREACH;      
   END METHOD; 
  
   ASK METHOD MassEditHier(IN changeP, useP, changeDep, changeName : BOOLEAN; 
                           IN newDep                                  : INTEGER;
                           IN newDepType, nameChangeType, newName           : STRING;
                           OUT namemsg                                      : BOOLEAN); 
   VAR        
      i          : INTEGER;
      tempName   : STRING;
      hierLabel  : TextObj;
   BEGIN   
      IF changeP 
         usesPhasing:=useP;
         somethingChanged := TRUE;
      END IF;
      IF changeDep AND (newDep <> DependencyNum)
         DependencyNum := newDep;
         depType := newDepType;
         somethingChanged := TRUE;
      END IF;
      IF changeName
         IF nameChangeType = "prefix"
            tempName := newName + name;
         ELSIF nameChangeType = "suffix"
            tempName := name + newName;
         ELSIF nameChangeType = "replace"
            tempName := newName;
         END IF;
         IF STRLEN(tempName) > 20
            namemsg := TRUE;
         ELSE
            name := tempName;
            hierLabel := Child("HierLabel", 0);
            ASK hierLabel TO SetText(name);
         END IF;
      END IF;
   END METHOD;

END OBJECT; {RBDHierObj}


OBJECT SparePoolObj;

   ASK METHOD SetData(IN name                                               : STRING;
                      IN initStock,RSONum,SLOlvl,SLOquan                    : INTEGER;
                      IN RSO,ESO,SLO                                        : BOOLEAN;
                      IN RSOArrivalTime,emerTime,SLOtime,spCost,emerShipCost: REAL);
   VAR
      i         : INTEGER;
      costArray : realArray;
      intsArray : intArray;
      tempBlock : RBDBlockObj;
   BEGIN
      poolName := name;
      INC(totalSpares);
      sparingType      := SparePool;
      initialSpares    := initStock;
      routineSpareOrdering:=RSO;
      newSpares        := RSONum;
      newSparesArrival := RSOArrivalTime;
      stockLevelOrdering:=SLO;
      SLOOrderLevel:=SLOlvl;
      SLONewSpares:=SLOquan;
      SLOTime:=SLOtime;
      emerSpareOrdering:=ESO;
      emergencyTime    := emerTime;
      IF (spCost <> spareCost) OR (emerShippingCost <> emerShipCost)
         spareCost        := spCost;
         emerShippingCost := emerShipCost;
         FOREACH tempBlock IN blockGroup
            IF tempBlock.poolName = name
               ASK tempBlock TO SetspareCost(spCost);                
               ASK tempBlock TO SetemerShippingCost(emerShipCost);                              
            END IF;
         END FOREACH;
      END IF;
      ASK poolGroup TO Add(SELF);
      somethingChanged := TRUE;
   END METHOD;{SetData}

   
   ASK METHOD SetResData(IN name                      : STRING;
                         IN initSp                    : INTEGER;
                         IN initCst,fixedCst,timeCst  : REAL;
                         IN phases                    : BOOLEAN);
   BEGIN
     poolName := name;
     INC(totalRes);
     sparingType      := Resource;
     initialSpares    := initSp;
     spareCost        := initCst;
     fixedPerUse      := fixedCst;
     costPerTime      := timeCst;
     usesPhasing      := phases;
     somethingChanged := TRUE;
     ASK poolGroup TO Add(SELF);
   END METHOD;{SetResData}
      
   ASK METHOD RemovePool(IN name : STRING;
                         IN type : SparingType);
   VAR
      i, j     : INTEGER;
      tempList : OptionListType;
   BEGIN
      IF type = SparePool
         IF totalSpares > 1
            NEW(tempList, 1..totalSpares - 1);
            j := 1;
            FOR i := 1 TO totalSpares
               IF spareList[i] <> name
                  tempList[j] := spareList[i];
                  INC(j);
               END IF;
            END FOR;
            DISPOSE(spareList);
            totalSpares := totalSpares - 1;
            NEW(spareList,1..totalSpares);
            FOR i := 1 TO totalSpares
               spareList[i] := tempList[i];
            END FOR;
            DISPOSE(tempList);
         ELSE
            totalSpares  := 0;
            spareList[1] := "unnamed";
         END IF;
     ELSIF type = Resource
         IF totalRes > 1
            NEW(tempList, 1..totalRes - 1);
            j := 1;
            FOR i := 1 TO totalRes
               IF resList[i] <> name
                  tempList[j] := resList[i];
                  INC(j);
               END IF;
            END FOR;
            DISPOSE(resList);
            totalRes := totalRes - 1;
            NEW(resList,1..totalRes);
            FOR i := 1 TO totalRes
               resList[i] := tempList[i];
            END FOR;
            DISPOSE(tempList);
         ELSE
            totalRes   := 0;
            resList[1] := "unnamed";
         END IF;
      END IF;
      somethingChanged := TRUE;
   END METHOD;{RemovePool}
END OBJECT;{SparePoolObj}


OBJECT PhaseObj;
                                       
   ASK METHOD SetPhaseData        (IN newString                  : STRING;
                                  IN newDist,newID               : INTEGER;
                                  IN newParams                   : realArray;
                                  IN newMission                  : BOOLEAN);

   VAR
      i,numParams                                       : INTEGER;
      tempList                                          : OptionListType;

   BEGIN
      DISPOSE(Params);
      phaseName:=newString;
      Dist:=newDist;
      GetNumParams(Dist,numParams);
      NEW(Params,1..numParams);
      FOR i := 1 TO numParams
         Params[i] := newParams[i];
      END FOR;
      mission:=newMission;
      ID:=newID;
     
   END METHOD;

END OBJECT;{PhaseObj}


OBJECT RBDBasicObj;
   ASK METHOD SetID(IN RBDObjName                  : STRING;
                    IN RBDIdNum                    : INTEGER);
   BEGIN
      Id := RBDIdNum;
      ReferenceName := RBDObjName;
      IF RBDIdNum >= nextId
         nextId := RBDIdNum+1;
{      ELSE   
         INC(nextId);  }        {tony10-04 does not recommend this}
      END IF;       
   END METHOD; {SetID}

   ASK METHOD SetCopiedFromId(IN copiedFrom : INTEGER);
   BEGIN
      copiedFromId := copiedFrom;
   END METHOD; {SetCopiedToId}

   ASK METHOD CheckForDeps;
   VAR
     tempBlock : RBDBlockObj;
     tempNode  : RBDNodeObj;
     tempEvent : RBDEventObj;
   BEGIN     
      IF ((depType="RBDNode") OR (DependencyNum<0) OR (depType="RBDHier"))
         tempNode:=ASK root Child("RBDNode", simDependencyNum);
         ASK tempNode TO SetHasDepObjects(TRUE);
      ELSIF (depType="RBDBlock")
         tempBlock:=ASK root Child("RBDBlock", simDependencyNum);
         ASK tempBlock TO SetHasDepObjects(TRUE);  
      ELSIF (depType="RBDEvent")
         tempEvent:=ASK root Child("RBDEvent",simDependencyNum);
         ASK tempEvent TO SetHasDepObjects(TRUE);
      END IF;
   END METHOD; {CheckForDeps}

   ASK METHOD SetHasDepObjects (IN newSetting : BOOLEAN);
   BEGIN
     hasDepObjects:=newSetting;
   END METHOD; {SetHasDepObjects}

   ASK METHOD SetSelected(IN isSelected : BOOLEAN);
   BEGIN
      Selected := isSelected;
   END METHOD; {SetSelected}

   ASK METHOD DisplayAt(IN x, y : REAL);
   BEGIN
      INHERITED DisplayAt(x, y);
      xPosition := x;
      yPosition := y;
   END METHOD; {DisplayAt}

   ASK METHOD ResetLinks(IN totalLinks, activeBlock : INTEGER;
                         IN xPos, yPos              : REAL;
                         IN activeRef               : STRING;
                         IN noRedraw,nowPasting     : BOOLEAN);
   VAR
      i               : INTEGER;
      xOffset,yOffset : REAL;
      currentLink     : LinkObj;
      linkPoints      : PointArrayType;
   BEGIN
      IF nowPasting
         xOffset := 12.;
         yOffset := 8.;
      END IF;
      
      IF activeRef="RBDHier"
         xOffset:=xOffset+0.3;  
      END IF;
      IF totalLinks > 0
         FOREACH currentLink IN linkGroup
            IF (currentLink.connectFromId = activeBlock) AND (currentLink.connectFRef = activeRef)
               NEW(linkPoints, 1..6);
               arrowhead(currentLink.connectTRef, xPos+0.26+xOffset, yPos-0.21-yOffset, currentLink.Points[6].x,
                         currentLink.Points[6].y, linkPoints[2].x, linkPoints[2].y,
                         linkPoints[3].x, linkPoints[3].y, linkPoints[4].x, linkPoints[4].y);
               linkPoints[1].x := xPos + 0.26 + xOffset;
               linkPoints[1].y := yPos - 0.21 - yOffset;
               linkPoints[5].x := linkPoints[2].x;
               linkPoints[5].y := linkPoints[2].y;
               linkPoints[6].x := currentLink.Points[6].x;
               linkPoints[6].y := currentLink.Points[6].y;
               ASK currentLink TO SetPoints(linkPoints);
                IF NOT noRedraw
                  ASK currentLink TO Update;
               END IF;
               DISPOSE(linkPoints);
            ELSIF (currentLink.connectToId = activeBlock) AND (currentLink.connectTRef =
                   activeRef)
               NEW(linkPoints, 1..6);
               arrowhead(currentLink.connectTRef, currentLink.Points[1].x, currentLink.Points[1].y, 
                         xPos+0.26+xOffset, yPos-0.21-yOffset, linkPoints[2].x, linkPoints[2].y, linkPoints[3].x,
                         linkPoints[3].y, linkPoints[4].x, linkPoints[4].y);
               linkPoints[1].x := currentLink.Points[1].x;
               linkPoints[1].y := currentLink.Points[1].y;
               linkPoints[5].x := linkPoints[2].x;
               linkPoints[5].y := linkPoints[2].y;
               linkPoints[6].x := xPos + 0.26 + xOffset;
               linkPoints[6].y := yPos - 0.21 - yOffset;
               ASK currentLink TO SetPoints(linkPoints);
               IF NOT noRedraw
                  ASK currentLink TO Update;
               END IF;
               DISPOSE(linkPoints);
            END IF;
         END FOREACH;         
      END IF;
   END METHOD; {ResetLinks}

   
   ASK METHOD ResetStateStart;
   BEGIN
      stateStartTime:=SimTime;                       
   END METHOD;
   
   
   ASK METHOD BackFlowDependencies(IN changeType : NodeStatusType);
     VAR 
        tempBlock                                 : RBDBlockObj;
        tempNode                                  : RBDNodeObj;
        action                                    : STRING;
        depVals                                   : realArray;
        depDraw                                   : REAL;
     BEGIN
        IF hasDepObjects    
          IF changeType=Down
             FOREACH tempBlock IN BlockDepGroup 
                IF ((tempBlock.simDependencyNum=Id) AND (NOT tempBlock.IgnoreDep)) 
                   IF ((tempBlock.opStatus=Running) OR (tempBlock.opStatus=Standby))
                      IF (NOT FEVmode)
                      
                         IF specialBackFlow    {added 5-24-05 to fix Rap7 error 999}
                            IF ((tempBlock.prevDepState="GoIdle") AND (NOT tempBlock.returnFromMaint))
                               ASK tempBlock TO SetInterruptReason("Dependence_Idle");
                               ASK tempBlock TO ChangeBlockState(Idle,tempBlock.activeStatus,"Dependence"); 
                               IF tempBlock.FailWait
                                  Interrupt(tempBlock,"Run");
                               ELSIF tempBlock.ZeroWait
                                  ASK BlockTrigger[tempBlock.seqNum] TO Release;
                               END IF;
                            ELSIF ((tempBlock.prevDepState="") AND (NOT tempBlock.returnFromMaint))
                               IF (tempBlock.defDepStateIdle)  
                                  ASK tempBlock TO SetInterruptReason("Dependence_Idle");
                                  ASK tempBlock TO ChangeBlockState(Idle,tempBlock.activeStatus,"Dependence");
                                  IF tempBlock.FailWait
                                     Interrupt(tempBlock,"Run");
                                  ELSIF tempBlock.ZeroWait
                                     ASK BlockTrigger[tempBlock.seqNum] TO Release;
                                  END IF;
                               END IF;
                            END IF;
                      
                         ELSIF (NOT tempBlock.returnFromMaint)                   
                            IF (tempBlock.DepIdlePerc=100.)
                               action:="GoIdle";
                            ELSIF (tempBlock.DepNothingPerc = 100.)  
                               action:="DoNothing";
                            ELSIF (tempBlock.DepPMPerc = 100.)  
                               action:="GoToPM";
                            ELSIF (tempBlock.DepFailPerc = 100.)
                               action:="InducedFailure";
                            ELSE   {draw number to determine induced state} 
                               IF ((SimTime=0.0) AND (tempBlock.TzeroAction<>""))   {cmc 12/13/06}
                                    action:=tempBlock.TzeroAction;          {cmc 12/13/06}                           
                               ELSE                            
                                  NEW(depVals,1..2);
                                  depVals[1]:=0.0;
                                  depVals[2]:=1.0;
                                  GetStream(tempBlock.failStream,"Fail",tempBlock.seqNum,randStream);
                                  ASK randStream TO DrawNumber(10,depVals,tempBlock.name,depDraw);
                                  depDraw:=depDraw*100.;
                                  IF (depDraw < tempBlock.DepNothingPerc)
                                     action:="DoNothing";
                                  ELSIF (depDraw < (tempBlock.DepNothingPerc + tempBlock.DepIdlePerc))
                                     action:="GoIdle";
                                  ELSIF (depDraw < (tempBlock.DepNothingPerc+tempBlock.DepIdlePerc+tempBlock.DepPMPerc))   
                                     action:="GoToPM";
                                  ELSE
                                     action:="InducedFailure";
                                  END IF;  
                                  DISPOSE(depVals);
                                  IF (SimTime=0.0)                              {cmc 12/13/06}
                                     ASK tempBlock TO SetTzeroAction(action);    {cmc 12/13/06}
                                  END IF;                                       {cmc 12/13/06}
                               END IF                                                               
                            END IF;
                            ASK tempBlock TO SetPrevDepState(action);
                            IF (action="GoIdle")
                                  ASK tempBlock TO SetInterruptReason("Dependence_Idle");
                                  ASK tempBlock TO ChangeBlockState(Idle,tempBlock.activeStatus,"Dependence"); 
                                  IF tempBlock.FailWait
                                     Interrupt(tempBlock,"Run");
                                  ELSIF tempBlock.ZeroWait
                                     ASK BlockTrigger[tempBlock.seqNum] TO Release;
                                  END IF;
                            ELSIF (action="DoNothing")  
                               {do nothing}
                            ELSIF (action="GoToPM") 
                               IF tempBlock.blockHasStarted       {cmc 12/13/06}
                                  IF ( (tempBlock.usesPM) AND 
                                          ( NOT( (tempBlock.pmMisDefer) AND (System.missionStatus=Mission) AND (activePhases>0)) ) )
                                     ASK tempBlock TO SetInterruptReason("OBPM");
                                     IF tempBlock.FailWait
                                        Interrupt(tempBlock,"Run");
                                     ELSIF tempBlock.ZeroWait
                                        ASK BlockTrigger[tempBlock.seqNum] TO Release;
                                     END IF;
                                     {   ASK tempBlock TO SetreturnFromMaint(TRUE);  tony 9-04: set returnFromMaint below.  This fixes Beta error }
                                  END IF;
                               END IF;
                            ELSIF (action="InducedFailure")
                               IF tempBlock.blockHasStarted        {cmc 12/13/06}
                                  ASK tempBlock TO SetInterruptReason("Induced_Failure");
                                  IF tempBlock.FailWait
                                     Interrupt(tempBlock,"Run");
                                  ELSIF tempBlock.ZeroWait
                                     ASK BlockTrigger[tempBlock.seqNum] TO Release;
                                  END IF;

                                  ASK tempBlock TO SetreturnFromMaint(TRUE);
                               END IF;                                                              
                             END IF;   
                         ELSE   
                            IF tempBlock.InterruptReason=""
                               IF ((tempBlock.DepIdlePerc=100.0) OR (tempBlock.defDepStateIdle))  
                                  ASK tempBlock TO ChangeBlockState(Idle,tempBlock.activeStatus,"");
                               ELSE  
                                  ASK tempBlock TO ChangeBlockState(Running,tempBlock.activeStatus,"");
                               END IF;
                               ASK tempBlock TO SetreturnFromMaint(FALSE);
                            END IF;
                         END IF;  {NOT tempBlock.returnFromMaint} 
                      ELSE  {in FEVmode}
                         IF tempBlock.DepNothingPerc<>100.0
                            ASK tempBlock TO SetFevDepMode(TRUE); 
                            IF NOT (FevDepGroup.Includes(tempBlock))
                               ASK FevDepGroup TO Add(tempBlock);
                            END IF;   
                            IF ((System.Status=GGreen) OR (System.Status=YYellow))
                               critFactor:=critFactor*(tempBlock.DepFailPerc+tempBlock.DepPMPerc+tempBlock.DepIdlePerc)/100.0;
                            END IF;
                         END IF;
                         IF (tempBlock.DepFailPerc > 0.)
                            ASK tempBlock TO ChangeBlockState(Repairing,tempBlock.activeStatus,"");
                         ELSIF (tempBlock.DepPMPerc >0.) 
                            ASK tempBlock TO ChangeBlockState(PM,tempBlock.activeStatus,"");
                         ELSIF (tempBlock.DepIdlePerc>0.)
                            ASK tempBlock TO ChangeBlockState(Idle,tempBlock.activeStatus,"");
                         END IF;
                      END IF;
                   END IF;
                END IF;
             END FOREACH;                            
             FOREACH tempNode IN NodeDepGroup 
                IF  ((tempNode.Status<>Down) AND (NOT tempNode.IgnoreDep))                
                   IF ((tempNode.simDependencyNum=Id) {AND (tempNode.hasDepObjects)})        {chuck 12/15/04}
                      IF (tempNode.coldStandby)                   {chuck 11-09-04}
                         ASK tempNode TO SetHigherCold(TRUE);
                      END IF;
                      ASK tempNode TO BackFlowDependencies(Down);
                   END IF;
                END IF;
             END FOREACH; 
          ELSIF changeType=AllUp
             FOREACH tempBlock IN BlockDepGroup
                IF ((tempBlock.simDependencyNum=Id) AND (NOT tempBlock.IgnoreDep))  
                   IF ((tempBlock.opStatus=Idle) OR (tempBlock.opStatus=Standby))   
                      IF (tempBlock.opStatus=Standby)                                {startUpFailures    CMC    start}
                         ASK tempBlock TO SetInterruptReason("Dependence_SB_turnOn");
                      ELSE
                         ASK tempBlock TO SetInterruptReason("Dependence_All_Up");
                      END IF;                                                        {startUpFailures    CMC    end}                       
                      ASK tempBlock TO ChangeBlockState(Running,tempBlock.activeStatus,"Dependence"); 
                      IF tempBlock.FailWait
                         Interrupt(tempBlock,"Run");
                      ELSIF tempBlock.ZeroWait
                         ASK BlockTrigger[tempBlock.seqNum] TO Release;
                      END IF;
                   END IF;
                END IF;
             END FOREACH; 
             FOREACH tempNode IN NodeDepGroup
                IF  ((tempNode.Status<>Down) AND (NOT tempNode.IgnoreDep))                
                   IF ((tempNode.simDependencyNum=Id) { AND (tempNode.hasDepObjects)} ) 
                      IF tempNode.higherCold
                         ASK tempNode TO SetHigherCold(FALSE);
                      END IF;
                      ASK tempNode TO BackFlowDependencies(AllUp); 
                      IF tempNode.coldStandby         
                         ASK tempNode TO RunColdLogic;
                      END IF;
                   END IF;
                END IF;
             END FOREACH; 
          ELSIF changeType=NodeStandby
             FOREACH tempBlock IN BlockDepGroup 
                IF ((tempBlock.simDependencyNum=Id) AND (NOT tempBlock.IgnoreDep))  
                   IF ((tempBlock.opStatus=Running) OR (tempBlock.opStatus=Idle))
                      ASK tempBlock TO SetInterruptReason("Dependence_Standby");
                      ASK tempBlock TO ChangeBlockState(Standby,tempBlock.activeStatus,"Dependence"); 
                      IF tempBlock.FailWait
                         Interrupt(tempBlock,"Run");
                      ELSIF tempBlock.ZeroWait
                         ASK BlockTrigger[tempBlock.seqNum] TO Release;
                      END IF;
                   END IF;
                END IF;
             END FOREACH; 
             FOREACH tempNode IN NodeDepGroup 
                IF  ((tempNode.Status<>Down) AND (NOT tempNode.IgnoreDep))                
                  IF (tempNode.simDependencyNum=Id)  
                    IF (tempNode.coldStandby)                   {chuck 11-04}
                       ASK tempNode TO SetHigherCold(TRUE);
                    END IF;
                     ASK tempNode TO BackFlowDependencies(NodeStandby);                      
                   {  IF tempNode.coldStandby        
                        ASK tempNode TO RunColdLogic;
                     END IF;  }
                  END IF;
               END IF;
            END FOREACH;   
          END IF;
        END IF;
   END METHOD;
   
   ASK METHOD SetColdDep;
   BEGIN
      coldDep:=TRUE;
   END METHOD;

   ASK METHOD SetIgnoreDep(IN IgDepStatus:BOOLEAN);
   BEGIN
      IgnoreDep:=IgDepStatus;
   END METHOD;
   
   ASK METHOD SetEconnectIntoNum  (IN newValue                          : INTEGER);
   BEGIN
      EconnectIntoNum:=newValue;
   END METHOD;    
   
   ASK METHOD SetEconnectOutOfNum (IN newValue                          : INTEGER);
   BEGIN
      EconnectOutOfNum:=newValue;
   END METHOD;       
   
   ASK METHOD SetDep (IN depNum :INTEGER; IN dType : STRING);                              
   BEGIN
      DependencyNum:=depNum;
      depType := dType;
   END METHOD;
   
   ASK METHOD SetParentID  (IN newID                               : INTEGER); {eliz}
   BEGIN
      parentID:=newID;
   END METHOD;    

   ASK METHOD SetGoodPathsRequired (IN newValue                    : INTEGER);
   BEGIN
      GoodPathsRequired:=newValue;
   END METHOD; 

   ASK METHOD SetusesPhasing (IN newBool                          : BOOLEAN);
   BEGIN
      usesPhasing:=newBool;
   END METHOD; 

   ASK METHOD SetComment               (IN newValue   :STRING);
   BEGIN
      comment:=newValue;
   END METHOD;
   
  ASK METHOD ResetConnectToInfo;
  BEGIN
     IF connectToIds <>NILARRAY
        DISPOSE(connectToIds);
     END IF;
     IF connectToRefs <>NILARRAY
        DISPOSE(connectToRefs);
     END IF;
  END METHOD;   {ResetConnectToIds}
   
  ASK METHOD UpdateConnectToInfo(IN newLink,newElement            : INTEGER;
                                 IN conTRef                       : STRING);
  VAR
     oldSize, i     : INTEGER;
     tempArray      : ARRAY INTEGER OF INTEGER;
     tempStrArray   : ARRAY INTEGER OF STRING;
  BEGIN
     IF connectToIds = NILARRAY
        NEW(connectToIds, 1..1);
        connectToIds[1]:=newElement;
     ELSE
        oldSize := HIGH(connectToIds);
        NEW(tempArray,1..oldSize);
        FOR i:=1 TO oldSize
           tempArray[i]:=connectToIds[i];          
        END FOR;
        DISPOSE(connectToIds);
        NEW(connectToIds,1..oldSize+1);
        FOR i:=1 TO oldSize
           connectToIds[i]:=tempArray[i];          
        END FOR;
        connectToIds[oldSize+1]:=newElement;
        DISPOSE(tempArray);
     END IF;
     IF connectToRefs = NILARRAY
        NEW(connectToRefs, 1..1);
        connectToRefs[1]:=conTRef;
     ELSE
        oldSize := HIGH(connectToRefs); 
        NEW(tempStrArray,1..oldSize);
        FOR i:=1 TO oldSize
           tempStrArray[i]:=connectToRefs[i];          
        END FOR;
        DISPOSE(connectToRefs);
        NEW(connectToRefs,1..oldSize+1);
        FOR i:=1 TO oldSize
           connectToRefs[i]:=tempStrArray[i];          
        END FOR;
        connectToRefs[oldSize+1]:=conTRef;
        DISPOSE(tempStrArray);
     END IF;
  END METHOD;    {UpdateConnectToIds}
   
   ASK METHOD SetSequenceNum(IN newValue                           : INTEGER);  
   BEGIN
       seqNum:=newValue;
   END METHOD;

  ASK METHOD IncLink(IN direction : directionType);        {tony 4-2-04 combined to method of BasicObj}
  VAR
     i                     : INTEGER;
     phaseValueChanged     : BOOLEAN;
     tempNode              : RBDNodeObj;
  BEGIN
     CASE direction
        WHEN INTO:
           connectIntoNum := connectIntoNum + 1;
        WHEN OUTOF:
           connectOutOfNum := connectOutOfNum + 1;
        WHEN MINUSINTO:
           connectIntoNum := connectIntoNum - 1;
           IF (OBJTYPENAME(SELF)="RBDNodeObj") 
              tempNode := RBDNodeObj(SELF); 
              IF ((tempNode.usesPhasing) AND (tempNode.phase<>NILARRAY))
                 FOR i:=1 TO activePhases
                    IF tempNode.phase[i]>connectIntoNum
                       tempNode.phase[i]:=connectIntoNum;
                       phaseValueChanged:=TRUE;
                    END IF;
                 END FOR;
              END IF;   
              IF phaseValueChanged
                 NEW(message, 1..3);
                 message[1] := "The node this link connects to is phased and used this link in     ";
                 message[2] := "one or more of its phases.  The k value for these phases have    ";
                 message[3] := "been reduced to the new k! Re-evaluate the information as necessary.     ";
                 result := SendAlert(message, FALSE, FALSE, TRUE);
                 DISPOSE(message);
              END IF;
              IF (tempNode.connectIntoNum=1)
                 ASK tempNode TO UncheckCold;
              END IF;
           END IF;            
        WHEN MINUSOUTOF:
           connectOutOfNum := connectOutOfNum - 1;
        OTHERWISE
           NEW(message, 1..1);
           message[1] := "ERROR: Unknown direction type!";
           result := SendAlert(message, FALSE, FALSE, TRUE);
           DISPOSE(message);
     END CASE;
  END METHOD; {IncLink}

  ASK METHOD SetAoDoR      (IN newAo,newDo,newR                   : STRING);  
  BEGIN
     Ao:=newAo;
     Do:=newDo;
     R:=newR;
  END METHOD;   
   
END OBJECT; {RBDBasicObj}
   
   
   
OBJECT RBDNodeObj;


   TELL METHOD InitTimeZeroDeps;   
   BEGIN
       BackFlowDependencies(Down);   
   END METHOD;


   ASK METHOD Init1(IN intsArray : intArray);
   BEGIN
      IF intsArray[1] = 1
         coldStandby:= TRUE;
      ELSE
         coldStandby:= FALSE;
      END IF;
      IF intsArray[2] = 1
         priorityReturn:= TRUE;
      ELSE
         priorityReturn:= FALSE;
      END IF;
      IF intsArray[3] = 1
         checkAutosFirst:= TRUE;
      ELSE
         checkAutosFirst:= FALSE;
      END IF;
      KStar:=intsArray[4]; 
      IF intsArray[5] = 1
         usesPhasing:= TRUE;
      ELSE
         usesPhasing:= FALSE;
      END IF;
      DependencyNum:=intsArray[6]; 
   END METHOD; {Init1}

   ASK METHOD SetType(IN nodeType : INTEGER);
   VAR
      nodeImage : ImageObj;
   BEGIN
      typeNode := nodeType;
      nodeImage := Child("Node", 602);
      CASE typeNode
         WHEN 1: {start node}
            ASK nodeImage TO SetColor(Black);
         WHEN 2: {connect node}
            ASK nodeImage TO SetColor(blockGUIColor);
         WHEN 3: {end node}
            ASK nodeImage TO SetColor(Black);
         WHEN 4: {in node}
            ASK nodeImage TO SetColor(DarkSlateBlue);
         WHEN 5: {out node}
            ASK nodeImage TO SetColor(DarkSlateBlue);
         OTHERWISE
            NEW(message, 1..1);
            message[1] := "ERROR: Unknown node type!";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
            ASK nodeImage TO SetColor(LimeGreen);
      END CASE;
      SetTranslation(Translation.x, Translation.y);
      Draw;
   END METHOD; {SetType}
   
   ASK METHOD SetGoodPaths(IN k : INTEGER);
   BEGIN
      goodPaths := k;
   END METHOD;

   ASK METHOD SetName(IN newName : STRING);
   BEGIN
      name := newName;
      ToolTip := name;
   END METHOD;

   ASK METHOD SetNum(IN label : INTEGER);
   VAR
      text    : TextObj;
      rename  : BOOLEAN;
   BEGIN
      IF (nodeName=name) OR (name="")
         rename:=TRUE;
      END IF;
      text := Child("RBDNodeNum", 0);      
      IF label = -1
         ASK text TO SetText("start");         
         nodeName := "start";
         name := "start";
      ELSIF label = -2
         ASK text TO SetText(" end");
         nodeName := "end";
         name := "end";
      ELSE
         nodeName := "n" + INTTOSTR(label);
      END IF;
      IF rename
         name:=nodeName;
      END IF;
      ASK text TO SetText(name);
      IF fontSize = 1
         ASK text TO SetSysFont("SmallFonts",fontSize,70,0);
         ASK text TO SetHidden(TRUE);
      ELSE
         ASK text TO SetSysFont("SmallFonts",fontSize,70,0);
      END IF;
      ASK text TO Draw;
      ToolTip := name;
   END METHOD;

  ASK METHOD SetKofN(IN k, n : INTEGER);
  VAR
      text : TextObj;
  BEGIN
     IF (typeNode = 2) AND (connectIntoNum > 1) AND (goodPaths > 0)
        text := Child("RBDNodeKofN", 0);
        ASK text TO SetText(INTTOSTR(k) + " / " + INTTOSTR(n));
        IF fontSize = 1
           ASK text TO SetSysFont("SmallFonts",fontSize,70,0);
           ASK text TO SetHidden(TRUE);
        ELSE
           ASK text TO SetSysFont("SmallFonts",fontSize,70,0);
        END IF;
        ASK text TO Draw;
     END IF;
  END METHOD;
      
  ASK METHOD SetKStar      (IN kStar : INTEGER);
  BEGIN
     KStar:= kStar;
  END METHOD;
   
   ASK METHOD SetReportNodeAnal(IN newSetting                      : BOOLEAN);
   BEGIN
      reportNodeAnal:=newSetting;
   END METHOD;

   ASK METHOD SetFullFlow(IN newSetting                      : BOOLEAN);
   BEGIN
      fullFlow:=newSetting;
   END METHOD;
   
   ASK METHOD SetAnyPath(IN newSetting                      : BOOLEAN);
   BEGIN
      anyPath:=newSetting;
   END METHOD;
   
   ASK METHOD UncheckCold;
   BEGIN
      IF coldStandby
          coldStandby:=FALSE;
          NEW(message, 1..3);
          message[1] := "The node this link connects to was set to cold standby.     ";
          message[2] := "Cold Standby nodes require at least 2 inbound links.  Cold     ";
          message[3] := "Standby is now being set to unchecked for this node.     ";
          result := SendAlert(message, FALSE, FALSE, TRUE);
          DISPOSE(message);
      END IF;
   END METHOD;

   ASK METHOD SetHigherCold(IN newSetting: BOOLEAN);  
   BEGIN
      higherCold:=newSetting;
   END METHOD;
   
   ASK METHOD SetTrialSuccess(IN newSetting: BOOLEAN);
   BEGIN
      trialSuccess:=newSetting;
   END METHOD;
   
   ASK METHOD SetEFPAtested(IN newSetting: BOOLEAN);
   BEGIN
      EFPAtested:=newSetting;
   END METHOD;  
   
   ASK METHOD SetCapNode (IN newSetting                     : BOOLEAN);
   BEGIN
      capNode:=newSetting;
   END METHOD;
  
   ASK METHOD SetCapTested (IN newSetting                     : BOOLEAN);
   BEGIN
      capTested:=newSetting;
   END METHOD;  
 
 ASK METHOD ShowAnalColor();
  VAR
     aoText, nameText             : TextObj;
     nodeImage, hierImage, dsiImage          : ImageObj;
     displayColor       : ColorType;
     displayValue       : REAL;
     tempHier : RBDHierObj;
  BEGIN
     IF weakLinkAnalType=1 
        displayValue := STRTOREAL(Ao);
     ELSIF weakLinkAnalType=2 
        displayValue := STRTOREAL(Do);
     ELSE 
        displayValue := STRTOREAL(R);
     END IF;
     IF displayValue >= GYthreshold
        displayColor := Green;
     ELSIF displayValue >= YRthreshold
        displayColor := Yellow;
     ELSE
        displayColor := Red;
     END IF;
     IF typeNode = 5
        tempHier := ASK root Child("RBDHier", parentID);
        aoText := ASK tempHier Child("HierAoText",0);
        nameText := ASK tempHier Child("HierLabel", 0);
        hierImage := ASK tempHier Child("Hier", 603);
        dsiImage := ASK tempHier Child("HierMid", 0);
     ELSE
        aoText := Child("NodeAoText",0);
        nameText := Child("RBDNodeNum", 0);
        nodeImage := Child("Node", 602);
     END IF;
     IF ((typeNode = 2) OR (typeNode = 5))
        ASK aoText TO SetColor(Blue);
        IF typeNode = 5
           ASK hierImage TO SetColor(displayColor);
           ASK dsiImage TO SetColor(displayColor);
           ASK hierImage TO Draw;
           ASK dsiImage TO Draw;
        ELSE
           ASK nodeImage TO SetColor(displayColor);
        END IF;
        IF displayValue<0.9985
           ASK aoText TO SetText(SUBSTR(2,5,REALTOSigFigSTR(displayValue,3)));
        ELSIF displayValue=1.0
           ASK aoText TO SetText("1.00");
        ELSE
           ASK aoText TO SetText(".999");
        END IF;
        IF (typeNode = 5) 
           IF tempHier.parentID = activeWindow
              ASK aoText TO SetHidden(FALSE);
              ASK tempHier TO SetHidden(FALSE); {needed}
           END IF;
        ELSE
           IF parentID = activeWindow
              ASK aoText TO SetHidden(FALSE);
              SetHidden(FALSE); {needed}
           END IF;
        END IF; 
        IF (fontSize > 1) AND (displayValue >= 0.)
           ASK aoText TO SetHidden(FALSE);
           ASK aoText TO SetSysFont("SmallFonts",fontSize-1,70,0);
        ELSE
           ASK nameText TO SetHidden(TRUE);
           ASK aoText TO SetHidden(TRUE);
        END IF;
        IF typeNode=5 
           IF tempHier.parentID <> activeWindow
              ASK aoText TO SetHidden(TRUE);
              ASK tempHier TO SetHidden(TRUE); {needed}
           END IF;
        ELSE
           IF parentID <> activeWindow
              ASK aoText TO SetHidden(TRUE);
              SetHidden(TRUE); {needed}
           END IF;
        END IF;
     ELSE  {start, in, and end markers}
        ASK aoText TO SetText("");
        ASK aoText TO SetHidden(TRUE);
     END IF;
     ASK aoText TO Draw;
      {ASK nodeImage TO Draw;}
     Draw;
  END METHOD;

  ASK METHOD ShowFlow ();
   VAR
      aoText             : TextObj;
      nodeImage : ImageObj;
      realFlow: REAL;
      flowString:STRING;
   BEGIN
      aoText := Child("NodeAoText",0);
      nodeImage := Child("Node", 602);
      IF runCompleted
         ASK aoText TO SetText("");
         ASK aoText   TO SetHidden(TRUE);
         {ASK aoText TO Draw;}
         Draw;
         RETURN;
      END IF;
      IF typeNode <> 3
         IF (fontSize > 1) 
            ASK aoText TO SetSysFont("SmallFonts",fontSize-1,70,0);
            IF nodeFlow<10000   {1-9999}
               flowString:=INTTOSTR(nodeFlow);
            ELSIF nodeFlow<999500  {10K-999K}
               realFlow:=FLOAT(nodeFlow)/FLOAT(1000);               
               flowString:=INTTOSTR(ROUND(realFlow))+"K";
            ELSIF nodeFlow<9950000 {1.0M to 9.9M}
               realFlow:=FLOAT(ROUND(FLOAT(nodeFlow)/FLOAT(100000)))/10.0;            
               flowString:=SUBSTR(1,3,REALTOSTR(realFlow))+"M";
            ELSIF nodeFlow<999500000 {10M to 999M}
               realFlow:=FLOAT(nodeFlow)/FLOAT(1000000);
               flowString:=INTTOSTR(ROUND(realFlow))+"M";
            ELSIF nodeFlow<9950000000   {1.0B to 9.9B}
               realFlow:=FLOAT(ROUND(FLOAT(nodeFlow)/FLOAT(100000000)))/10.0;            
               flowString:=SUBSTR(1,3,REALTOSTR(realFlow))+"M";
            ELSIF nodeFlow<100000000000  {10B to 999B}
               realFlow:=FLOAT(nodeFlow)/FLOAT(1000000000);
               flowString:=INTTOSTR(ROUND(realFlow))+"G";
            END IF;
            ASK aoText TO SetText(flowString);
            IF typeNode = 2
               ASK aoText TO SetColor(Black);
            ELSE
               ASK aoText TO SetColor(Black);
            END IF;
            ASK aoText TO SetHidden(FALSE);
         ELSE
            ASK aoText TO SetHidden(TRUE);
         END IF;
      ELSE
         ASK aoText   TO SetHidden(TRUE);
      END IF;
      {should this follow same logic as ShowAnalColor above? -EAG}
      IF parentID <> activeWindow 
         ASK aoText TO SetHidden(TRUE);
         SetHidden(TRUE); {needed}
      ELSE
         ASK aoText TO SetHidden(FALSE);
         SetHidden(FALSE); {needed}
      END IF;
      {ASK aoText TO Draw;    }
      Draw;
      
  END METHOD;   {ShowFlow}
      
   ASK METHOD SetNRBDvalues     (IN NRBDOutNum                          : INTEGER;
                            IN NRBDDownstreamArray, NRBDPathArray  : intArray);
   VAR
      j  : INTEGER;
   BEGIN
      NRBDConnectOut := NRBDOutNum; 
      IF NRBDConnectTo <> NILARRAY
         DISPOSE(NRBDConnectTo);
      END IF;
      IF NRBDConnectPath <> NILARRAY
         DISPOSE(NRBDConnectPath);
      END IF;
      NEW(NRBDConnectTo, 1..NRBDConnectOut);
      NEW(NRBDConnectPath,1..NRBDConnectOut);
      FOR j:=1 TO NRBDConnectOut
         NRBDConnectTo[j] := NRBDDownstreamArray[j];
         NRBDConnectPath[j]:=NRBDPathArray[j];
      END FOR;
   END METHOD;

   ASK METHOD SetEFPAvalues     (IN EFPAOutNum                          : INTEGER;
                                 IN EFPADownstreamArray, EFPAPathArray  : intArray);
   VAR
      j  : INTEGER;
   BEGIN
      EFPAConnectOut := EFPAOutNum; 
      IF EFPAConnectTo <> NILARRAY
         DISPOSE(EFPAConnectTo);
      END IF;
      IF EFPAConnectPath <> NILARRAY
         DISPOSE(EFPAConnectPath);
      END IF;
      NEW(EFPAConnectTo, 1..EFPAConnectOut);
      NEW(EFPAConnectPath,1..EFPAConnectOut);
      FOR j:=1 TO EFPAConnectOut
         EFPAConnectTo[j] := EFPADownstreamArray[j];
         EFPAConnectPath[j]:=EFPAPathArray[j];
      END FOR;
   END METHOD;
   
   ASK METHOD SetCapValues (IN CapOutNum                          : INTEGER;
                            IN CapDownstreamArray, CapPathArray  : intArray);
   VAR
      j  : INTEGER;
   BEGIN
      CapConnectOut := CapOutNum; 
      IF CapConnectTo <> NILARRAY
         DISPOSE(CapConnectTo);
      END IF;
      IF CapConnectPath <> NILARRAY
         DISPOSE(CapConnectPath);
      END IF;
      NEW(CapConnectTo, 1..CapConnectOut);
      NEW(CapConnectPath,1..CapConnectOut);
      FOR j:=1 TO CapConnectOut
         CapConnectTo[j] := CapDownstreamArray[j];
         CapConnectPath[j]:=CapPathArray[j];
      END FOR;
   END METHOD;
      
   ASK METHOD SetLocation(IN xLoc, yLoc               : REAL);
   BEGIN
      xPosition := xLoc;
      yPosition := yLoc;
   END METHOD;

   ASK METHOD SetGoBack(IN back: BOOLEAN);      
   BEGIN
      goBack:=back;
   END METHOD;
   
   ASK METHOD MassEditNode(IN changeP, useP, changeK, changeDep, changeName : BOOLEAN; 
                           IN newK, newDep                                  : INTEGER;
                           IN newDepType, nameChangeType, newName           : STRING;
                           OUT namemsg                                      : BOOLEAN); 
   VAR
      i          : INTEGER;
      tempName   : STRING;
      nodeLabel  : TextObj;
   BEGIN   
      IF changeP AND (useP <> usesPhasing)
         SetPhases(useP,NILARRAY);
         somethingChanged := TRUE;
      END IF;
      IF ((changeK) AND (newK<>goodPaths))
         IF ((newK=0) AND (connectIntoNum <>0))  {set k = n}
            SetGoodPaths(connectIntoNum);
            SetKofN(newK,connectIntoNum);         
         ELSE
            SetGoodPaths(newK);
            SetKofN(newK,connectIntoNum);
            IF (changeK AND coldStandby AND (newK<>1))   {chuck}
               coldStandby := FALSE;
            END IF;
         END IF;
         somethingChanged := TRUE;
      END IF;    
      IF changeDep AND (newDep <> DependencyNum)
         DependencyNum := newDep;
         depType := newDepType;
         somethingChanged := TRUE;
      END IF;
      IF changeName
         IF nameChangeType = "prefix"
            tempName := newName + name;
         ELSIF nameChangeType = "suffix"
            tempName := name + newName;
         ELSIF nameChangeType = "replace"
            tempName := newName;
         END IF;
         IF STRLEN(tempName) > 20
            namemsg := TRUE;
         ELSE
            name := tempName;
            nodeLabel := Child("RBDNodeNum", 0);
            ASK nodeLabel TO SetText(name);
         END IF;
      END IF;
   END METHOD;   {MassEditNode}

   ASK METHOD SetPhases(IN phasing : BOOLEAN; 
                        IN array   : intArray);
   VAR
      i         : INTEGER;
      tempImage : FillVObj;
      tempPhase : intArray;
      tempHier  : RBDHierObj;
   BEGIN
      tempImage := Child("Node",602);
      usesPhasing := phasing;
      IF (activePhases > 0) AND usesPhasing
         IF array<>NILARRAY  
            NEW(tempPhase,1..activePhases);
            FOR i:=1 TO activePhases
               tempPhase[i]:=array[i];
            END FOR;
         END IF;
         IF phase<>NILARRAY  
            IF NOT somethingChanged
               IF tempPhase <> NILARRAY
                  IF HIGH(tempPhase) = HIGH(phase)
                     FOR i := 1 TO HIGH(phase)
                        IF phase[i] <> tempPhase[i]
                           somethingChanged := TRUE;
                        END IF;
                     END FOR;
                  ELSE
                     somethingChanged := TRUE;
                  END IF;
               END IF;
            END IF;
            DISPOSE(phase);
         END IF;
         IF tempPhase <> NILARRAY
            NEW(phase,1..activePhases);
            FOR i:=1 TO activePhases
               phase[i]:=tempPhase[i];
            END FOR;
         ELSIF activePhases > 0
            NEW(phase,1..activePhases);
            FOR i:=1 TO activePhases
               phase[i]:=goodPaths;
            END FOR;
         END IF;
         IF (typeNode = 5)
            tempHier := ASK root Child("RBDHier", parentID);
            IF (tempHier <> NILOBJ)
               tempImage := ASK tempHier Child("Hier", 603);
               ASK tempImage TO SetStyle(NarrowCrosshatchFill);
               ASK tempImage TO Draw;
            END IF;                  
         ELSE
            ASK tempImage TO SetStyle(NarrowCrosshatchFill);
         END IF;
      ELSE
         IF (typeNode = 5) AND (NOT copiedFromPrevFile)
            tempHier := ASK root Child("RBDHier", parentID);
            IF (tempHier <> NILOBJ)
               tempImage := ASK tempHier Child("Hier", 603);
               ASK tempImage TO SetStyle(SolidFill);
               ASK tempImage TO Draw;
            END IF;
         ELSE
            ASK tempImage TO SetStyle(SolidFill);
         END IF;
      END IF;   
      IF (typeOfCursor <> nodeC) AND (NOT resetting)
         ASK tempImage TO Draw;
         SetHighlighted(TRUE);
         SetHighlighted(FALSE);
         Draw;
      END IF;
   END METHOD;   {SetPhases}


   ASK METHOD ReconfigureNode(IN RunChange,FailChange,IdleChange,
                                 StandbyChange,pmChange,inPath    : INTEGER);  
     VAR
        arrayIndex,path,i,runs,fails,idles,standbys,pms           : INTEGER;  
        block                                                     : RBDBlockObj; 
        tempnode                                                  : RBDNodeObj;
        link                                                      : LinkObj;
        oldLinkStatus,newLinkStatus                               : LinkStatusType;
        newNodeStatus                                             : NodeStatusType;
        linkStatusChanged                                         : BOOLEAN;
        statusString,linkString                                   : STRING;
        hier                                                      : RBDHierObj;
     BEGIN 
        linkStatusChanged:=FALSE;
        newNodeStatus:=Status;
        {determine status of the link specified by variable inPath}
        IF inPath<>0    {0 if called due to phasing}       
           FOR i:=1 TO EconnectIntoNum
              IF inPath = inPathsArray[i]
                 arrayIndex:=i;
              END IF;
           END FOR;
           IF arrayIndex=0
              NEW(message,1..1);
              message[1]:="Error in ReconfigureNode!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message);         
           END IF;
           pathFailValue[arrayIndex]:=pathFailValue[arrayIndex]+FailChange;
           fails:=pathFailValue[arrayIndex];
           pathIdleValue[arrayIndex]:=pathIdleValue[arrayIndex]+IdleChange;
           idles:=pathIdleValue[arrayIndex];
           pathStandbyValue[arrayIndex]:=pathStandbyValue[arrayIndex]+StandbyChange;
           standbys:=pathStandbyValue[arrayIndex];  
           pathPMValue[arrayIndex]:=pathPMValue[arrayIndex]+pmChange;
           pms:=pathPMValue[arrayIndex];  
           IF fails>0          
              newLinkStatus:=LinkDown; 
           ELSIF pms>0
              newLinkStatus:=LinkPM;
           ELSIF idles>0 
              newLinkStatus:=LinkIdle;
           ELSIF standbys>0
              newLinkStatus:=LinkStandby;         
           ELSE
              newLinkStatus:=LinkUp;
           END IF;
           {change link status if necessary}
           link := ASK root Child("RBDLink", inPath);    
           oldLinkStatus:=link.Status;
           IF oldLinkStatus<>newLinkStatus
              ASK link TO ChangeLinkStatus(newLinkStatus);
              linkStatusChanged:=TRUE;              
              IF ((link.switchStatus="autoInProgress") OR (link.switchStatus="manualInProgress"))
                 Interrupt(link,"PerformSwitch");
              END IF;
              
              IF (coldStandby AND  (oldLinkStatus=LinkUp) AND (newLinkStatus<>LinkUp))        {Chuck 8/21/04} {start}
                 IF link.EconnectFRef="RBDBlock"                                               {error 32 fix}
                    block := ASK root Child("RBDBlock",link.EconnectFromId);
                    IF block.opStatus=Running;
                       ASK block TO ChangeBlockState(Standby,block.activeStatus,"");
                       IF block.hasDepObjects
                          ASK block TO BackFlowDependencies(NodeStandby);
                       END IF;
                       IF SimTime>0.0
                          ASK block TO SetInterruptReason("Switched_Off");
                          IF block.FailWait
                             Interrupt(block,"Run");
                          ELSIF block.ZeroWait
                             ASK BlockTrigger[block.seqNum] TO Release;
                          END IF;
                       END IF;     
                    END IF;
                 END IF;
              END IF;                                                                               {end}
           END IF;
        END IF;   
        IF (  ((linkStatusChanged) OR (inPath=0))  AND    ((NOT coldStandby) OR (higherCold))  )   
         {now we examine all links directly connected to the node}
           runs  :=0;    {these locals reused in a different sense than as above}
           fails :=0;
           idles :=0;
           standbys :=0;
           pms  :=0;  
           NumGoodPaths:=0;
           linkString:="";
           FOR i:=1 TO EconnectIntoNum
              path:=inPathsArray[i];
              link := ASK root Child("RBDLink", path);                         
              CASE link.Status            
                 WHEN LinkUp:
                    IF ((link.switchStatus="manualInProgress") OR (link.switchStatus="autoInProgress"))
                       INC(fails);
                       linkString:=linkString+"_dsw"+INTTOSTR(path);                 
                    ELSE
                       INC(runs);
                       linkString:=linkString+"_ur"+INTTOSTR(path);
                    END IF;                                             
                 WHEN LinkDown:
                    INC(fails);
                    linkString:=linkString+"_df"+INTTOSTR(path);
                 WHEN LinkIdle:
                    INC(idles);
                    linkString:=linkString+"_di"+INTTOSTR(path);
                 WHEN LinkStandby: 
                    IF ((link.switchStatus="manualInProgress") OR (link.switchStatus="autoInProgress"))
                       INC(fails);
                       linkString:=linkString+"_dsw"+INTTOSTR(path);                 
                    ELSE
                       INC(standbys);                    
                       linkString:=linkString+"_us"+INTTOSTR(path);
                    END IF;
                 WHEN LinkPM: 
                    INC(pms);
                    linkString:=linkString+"_dp"+INTTOSTR(path);
                 WHEN LinkDone:
                    INC(fails);
                    linkString:=linkString+"_dd"+INTTOSTR(path);
                 OTHERWISE 
                    NEW(message,1..1);
                    message[1]:="Error in link status logic!     "; 
                    result:=SendAlert(message,FALSE, FALSE, TRUE);
                    DISPOSE(message); 
              END CASE;
           END FOR;
           NumGoodPaths:=runs;           
           {(AllUp,Degraded,Down,NodeIdle,NodeStandby);}
           IF GoodPathsRequired>EconnectIntoNum     {node is cut}
              newNodeStatus:=Down;
              statusString:="Bad";
           ELSIF runs=EconnectIntoNum
              newNodeStatus:=AllUp;
              statusString:="Good";
           ELSIF runs>=GoodPathsRequired
              IF (runs+standbys)=EconnectIntoNum    {to fix error 999}
                 newNodeStatus:=AllUp;
                 statusString:="Good";
              ELSE
                 newNodeStatus:=Degraded;
                 statusString:="Degraded";
              END IF;
           ELSIF runs+standbys>=GoodPathsRequired
              newNodeStatus:=NodeStandby;
              statusString:="Standby";
           ELSIF idles+runs+standbys>=GoodPathsRequired
              newNodeStatus:=NodeIdle;
              statusString:="Idle";
           ELSIF pms+idles+runs+standbys>=GoodPathsRequired
              newNodeStatus:=NodePM;
              statusString:="NodePM";
           ELSIF fails>0
              newNodeStatus:=Down;
              statusString:="Bad";
           ELSE
              NEW(message,1..1);
              message[1]:="Error in node status logic!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message); 
           END IF; 
           IF EventsFile
              IF (typeNode=5)
                 hier := ASK root Child("RBDHier", parentID);
                 WriteEvents(SimTime,"Hier",hier.name,statusString,INTTOSTR(NumGoodPaths)+"-"+INTTOSTR(GoodPathsRequired)+
                                 "/"+INTTOSTR(EconnectIntoNum)+linkString);
              ELSE
                 WriteEvents(SimTime,"Node",name,statusString,INTTOSTR(NumGoodPaths)+"-"+INTTOSTR(GoodPathsRequired)+"/"+
                               INTTOSTR(EconnectIntoNum)+linkString);
              END IF;    
           END IF;
           IF Status<>newNodeStatus
              ChangeNodeState(newNodeStatus,activeStatus);
           END IF;   {node status has changed}
        ELSIF (  ((linkStatusChanged) OR (inPath=0))  AND    (coldStandby) ) 
            coldsAffected:=TRUE;
            IF NOT (ColdChangeGroup.Includes(SELF))      {csb speed change}
               ASK ColdChangeGroup TO Add(SELF);
            END IF;
        END IF;  {link status has changed}
    END METHOD;   {ReconfigureNode}
  
  ASK METHOD ReconfigureSystem(IN inPath: INTEGER);
  VAR
     DownstreamNode,path,PathLink,
     priorActivePaths,i,j,k          : INTEGER;  
     tempnode                        : RBDNodeObj;
     link                            : LinkObj;
  BEGIN
     priorActivePaths:=NumActivePaths;
     link := ASK root Child("RBDLink",inPath);
     ASK link TO SetActiveLink(FALSE);
     NumActivePaths:=0;
     FOR i:=1 TO EconnectIntoNum
        path:=inPathsArray[i];
        link := ASK root Child("RBDLink",path);
        IF link.active;
           INC(NumActivePaths);
        END IF;
     END FOR;           
     IF (NumActivePaths=MinGoodPathsReq-1) AND (priorActivePaths<>NumActivePaths)     
        IF (typeNode=2)
           finished:=TRUE;
           IF coldStandby
              BackFlowDependencies(Down); 
           END IF;
        END IF; 
        IF (typeNode=3)
           finished:=TRUE;
           IF EventsFile
              WriteEvents(SimTime,"System","Permanently_Down","Out_of_spares","System_change");
           END IF;
           IF termType=2  
              IF (TRUNC(StopCriteria)>1)
                 OutOfSpares:=TRUE;
              END IF;
              autoStopped:=TRUE;
              TELL System TO CallEndSimulation(FALSE,TRUE);  
           END IF;                                       
        ELSE           
           FOR j:=1 TO EFPAConnectOut  
              DownstreamNode:=EFPAConnectTo[j]; 
              PathLink:=EFPAConnectPath[j];
              tempnode := ASK window Descendant("RBDNode", DownstreamNode);              
              ASK tempnode TO ReconfigureSystem(PathLink); 
           END FOR; 
        END IF;
     END IF; 
  END METHOD;   {ReconfigureSystem} 

  ASK METHOD Initialize();
     VAR
        DownstreamID,DownstreamLink,i,j,k,path  : INTEGER;
        tempBlock                               : RBDBlockObj;
        tempNode                                : RBDNodeObj;
        tempString                              : STRING;
        tempImage                               : FillVObj;
        hier                                    : RBDHierObj;
     BEGIN
     IF (phase <> NILARRAY) 
        IF (typeNode=5)
           hier := ASK root Child("RBDHier",parentID); 
           tempImage:=ASK hier Child("Hier",603);                                    
           ASK tempImage TO SetStyle(SolidFill);
        ELSE
           tempImage := Child("Node", 602); 
           ASK tempImage TO SetStyle(SolidFill);
        END IF;
        ASK tempImage TO Draw;
     END IF;
     IF ((typeNode = 2) OR (typeNode=5))
        CASE DependencyNum
           WHEN -2:
              sysDepend:=TRUE;
              simDependencyNum:=endId;
           WHEN -1:
              locDepend:=TRUE;
              IF HIGH(EFPAConnectTo)>1
                 simDependencyNum:=endId;
              ELSE
                 simDependencyNum:=EFPAConnectTo[1];
              END IF;        
           OTHERWISE
              simDependencyNum:=DependencyNum;
        END CASE;
        IF (simDependencyNum>0)
           ASK NodeDepGroup TO Add(SELF);
        END IF;
     END IF;     
     IF ((usesPhasing)  AND (activePhases>0))
        MinGoodPathsReq:=phase[1];
        FOR j:=1 TO activePhases
           IF phase[j] < MinGoodPathsReq
              MinGoodPathsReq:=phase[j];
           END IF;
        END FOR;
     ELSE
        MinGoodPathsReq:=goodPaths;
     END IF;
     IF (typeNode<>1)
        NEW(pathFailValue,1..EconnectIntoNum);
        NEW(pathIdleValue,1..EconnectIntoNum);
        NEW(pathStandbyValue,1..EconnectIntoNum);
        NEW(pathPMValue,1..EconnectIntoNum);
     END IF;
     SetHigherCold(FALSE);
     activeStatus:=Active;
  END METHOD;      {Node - Initialize}
  
  ASK METHOD SetToInitValues();   {node}
  VAR
     i    : INTEGER;
     hier : RBDHierObj;
  BEGIN
     activeStatus:=Active;
     NumGoodPaths:=EconnectIntoNum; 
     NumActivePaths:=EconnectIntoNum; 
     GoodPathsRequired:=goodPaths;
     ChangeNodeState(AllUp,Active);
     IF ((usesPhasing) AND (activePhases>0))
        IF typeNode<>5
           IF phase[1]=-1
              GoodPathsRequired:=connectIntoNum+1;
           ELSE   
              GoodPathsRequired:=phase[1];
           END IF;  
           IF GraphicsOutput 
              SetKofN(GoodPathsRequired,EconnectIntoNum);
           END IF; 
        END IF;
     END IF;
     IF ((typeNode<>1) AND weakAnalysis)
        stateStartTime:=0.0;
        NodeGreenTime[seqNum]:=0.0;
        NodeYellowTime[seqNum]:=0.0;
        NodeRedTime[seqNum]:=0.0;
        NodeBlueTime[seqNum]:=0.0;
        NodeBrownTime[seqNum]:=0.0;
        NodeOrangeTime[seqNum]:=0.0;
     END IF;
     IF (typeNode<>1)
        FOR i:=1 TO EconnectIntoNum
           pathFailValue[i]:=0;
           pathIdleValue[i]:=0;
           pathStandbyValue[i]:=0;
           pathPMValue[i]:=0;
      {test only}        {pathRunValue[i]:=pathMaxValue[i]; }
        END FOR;
     END IF;
     IF coldStandby
        coldsAffected:=TRUE;
     END IF;
     IgnoreDep:=FALSE;
     higherCold:=FALSE;
     trialSuccess:=TRUE;
     finished:=FALSE;
     IF ((typeNode=5) AND (weakAnalysis))
        hier := ASK root Child("RBDHier",parentID);
        hierDE[hier.seqNum]:=0;
     END IF;
  END METHOD;
 
  ASK METHOD IdAndSortColds;
  VAR
    i,j,swapIndex,swapObject,testObj,
    testObjPriority,topPriority                 :INTEGER;
    tempLink                                    :LinkObj;
    priority                                    :intArray;
    HigherPriorityFound                         :BOOLEAN;
    tempBlock                                   :RBDBlockObj;
    tempNode                                    :RBDNodeObj;
  BEGIN
     IF coldLinks <>NILARRAY    {happens if run>1 and priority return is FALSE}
        DISPOSE(coldLinks);
     END IF;    
     {Identify upstream cold object and fill array}
     NEW(coldLinks,1..EconnectIntoNum);
     NEW(priority,1..EconnectIntoNum);     
     FOR i:=1 TO EconnectIntoNum
        tempLink := ASK root Child("RBDLink",inPathsArray[i]);
        ASK tempLink TO SetSBlink(TRUE);
        coldLinks[i]:=tempLink.Id;
        priority[i]:=tempLink.coldPriority;
        IF tempLink.EconnectFRef="RBDBlock"                 
           tempBlock := ASK root Child("RBDBlock",tempLink.EconnectFromId);
           ASK tempBlock TO SetColdDep;
           {the variable coldDep used to be set to TRUE for blocks connected to a cold standby
            node and dependent on that node.  It now is set to TRUE even if it is not 
            dependent on the node}           
        ELSIF tempLink.EconnectFRef="RBDNode"
           tempNode:=ASK root Child("RBDNode", tempLink.EconnectFromId);
           ASK tempNode TO SetColdDep;
        END IF;
     END FOR;
     {Sort Array}
     FOR i:=1 TO EconnectIntoNum
        HigherPriorityFound:=FALSE;
        topPriority:=priority[i];
        FOR j:=i TO EconnectIntoNum
           IF priority[j]<topPriority
              HigherPriorityFound:=TRUE;
              topPriority:=priority[j];
              swapObject:=coldLinks[j];
              swapIndex:=j;
           END IF;
        END FOR;
        IF HigherPriorityFound
           testObj:=coldLinks[i];
           testObjPriority:=priority[i];
           coldLinks[i]:=swapObject;
           priority[i]:=topPriority;
           coldLinks[swapIndex]:=testObj;
           priority[swapIndex]:=testObjPriority;           
        END IF;
     END FOR;
     DISPOSE(priority);     
  END METHOD;
  
  ASK METHOD ReOrderColds(IN lastLink                           : INTEGER);
  VAR
    i                 : INTEGER;
    linkFound         : BOOLEAN;
  BEGIN
     FOR i:= 1 TO (EconnectIntoNum-1)
        IF ((coldLinks[i]=lastLink) OR linkFound)
           coldLinks[i]:=coldLinks[i+1];
           IF NOT linkFound
              linkFound:=TRUE;
           END IF; 
        END IF;
     END FOR;
     coldLinks[EconnectIntoNum]:=lastLink;  
  END METHOD; 
    

  ASK METHOD SortCapacityArrays;
  VAR
    i,j,k,currentPath, currentNode,
    swapNode,swapIndex,swapPath                 :INTEGER;
    tempLink                                    :LinkObj;
    priority                                    :intArray;
    HigherPriorityFound                         :BOOLEAN;        
  BEGIN
     NEW(priority,1..CapConnectOut);     
     FOR i:=1 TO CapConnectOut
        tempLink := ASK root Child("RBDLink",CapConnectPath[i]);
        priority[i]:=tempLink.simCapPriority;
     END FOR;
     {Sort Array}
     FOR i:=1 TO CapConnectOut
        currentPath:=CapConnectPath[i];
        currentNode:=CapConnectTo[i];
        HigherPriorityFound:=FALSE;
        swapIndex:=i;
        FOR j:=i TO CapConnectOut
           IF priority[j]<priority[swapIndex]
              HigherPriorityFound:=TRUE;
              swapPath:=CapConnectPath[j];
              swapNode:=CapConnectTo[j];
              swapIndex:=j;
           END IF;
        END FOR;
        IF HigherPriorityFound
           CapConnectPath[i]:=swapPath;
           CapConnectTo[i]:=swapNode;
           CapConnectPath[swapIndex]:=currentPath;
           CapConnectTo[swapIndex]:=currentNode;
           priority[swapIndex]:=priority[i];           
        END IF;
     END FOR;
     DISPOSE(priority);     
  END METHOD;
    
  ASK METHOD DistributeFlow (IN  sent                        : INTEGER; 
                             OUT rejected                    : INTEGER);  
     VAR
        link,tempLink,sibling                               : LinkObj;
        linkFlowValue,
        j,k,amountLeft,returnAmount,numStillAvail,
        groupStart,groupEnd,nextGroupStart,groupPriority,
        shareAmount,firstNode,commonTotal,returnShare,
        amountToDistribute, DownStreamBlockage              :INTEGER;
        tempNode                                            :RBDNodeObj;
        findingGroup,allNomsFilled,allMaxFilled,
        nomAvailable,maxAvailable,groupPrioritySet,
        commonNode,nodeIsFull                               :BOOLEAN;
     BEGIN
        amountLeft:=sent;
        allNomsFilled:=FALSE;
        allMaxFilled:=FALSE;
        nextGroupStart:=1;
        IF typeNode<>1
           DownStreamBlockage:=0;
           FOR j:= 1 TO EFPAConnectOut
               link:=ASK root Child("RBDLink", EFPAConnectPath[j]);
               tempNode := ASK window Descendant("RBDNode",EFPAConnectTo[j]);
               IF ( (NOT link.Status=LinkUp) OR  (NOT ((tempNode.Status=AllUp) OR (tempNode.Status=Degraded)) ))
                  INC(DownStreamBlockage);    
               END IF;     
           END FOR;
           IF (DownStreamBlockage=EFPAConnectOut)
               allNomsFilled:=TRUE;
               allMaxFilled:=TRUE;        
           END IF;
        END IF;
     
        WHILE ((amountLeft>0) AND (NOT allNomsFilled))   
           groupStart:=nextGroupStart;
           groupEnd:=groupStart;
           numStillAvail:=0;
           nomAvailable:=FALSE;
           groupPrioritySet:=FALSE;
           j:=0;
           firstNode:=0;
           commonNode:=TRUE;
           IF groupStart<=CapConnectOut
              findingGroup:=TRUE;
              WHILE findingGroup           
                 link:=ASK root Child("RBDLink", CapConnectPath[groupStart+j]);
                 IF groupPrioritySet
                    IF link.simCapPriority>groupPriority
                       nextGroupStart:=groupStart+j;
                       findingGroup:=FALSE;
                       EXIT;
                    ELSE
                       groupEnd:=groupStart+j;
                    END IF;
                 ELSE
                    groupEnd:=groupStart+j;
                 END IF;
                 IF ((link.Status=LinkUp) AND (NOT link.nomReached) AND (NOT link.pathFull))
                    tempNode := ASK window Descendant("RBDNode", CapConnectTo[groupStart+j]);
                    IF ((tempNode.Status=AllUp) OR (tempNode.Status=Degraded))
                       INC(numStillAvail);
                       nomAvailable:=TRUE;
                       ASK link TO SetAvailFlow(link.simNomFlow-link.actualFlow);
                       IF (NOT groupPrioritySet)
                          groupPriority:=link.simCapPriority;
                          groupPrioritySet:=TRUE;
                          firstNode:=tempNode.Id;
                       ELSE
                          IF tempNode.Id<>firstNode
                             commonNode:=FALSE;
                          END IF;
                       END IF;
                    ELSE
                      { ASK link TO SetPathFull(TRUE);  }
                       IF tempNode.Id<>firstNode
                          commonNode:=FALSE;
                       END IF;
                    END IF;
                 END IF;  
                 IF (groupStart+j)=CapConnectOut
                    findingGroup:=FALSE;
                 ELSE
                    INC(j);
                 END IF;
              END WHILE;
           END IF;
        {try to send nominal amount each way}
           WHILE ((amountLeft>0) AND nomAvailable)
              k:=numStillAvail;
              nomAvailable:=FALSE;
              IF commonNode
                 commonTotal:=0;
                 amountToDistribute:=amountLeft;
                 FOR j:=groupStart TO groupEnd 
                    link:=ASK root Child("RBDLink", CapConnectPath[j]);
                    IF ((link.Status=LinkUp) AND (NOT link.nomReached) AND (NOT link.pathFull))
                       shareAmount:=ROUND(FLOAT(amountToDistribute)/FLOAT(k));
                       amountToDistribute:=amountToDistribute-shareAmount;
                       DEC(k);
                       IF shareAmount>link.availFlow
                          ASK link TO SetSendAmount(link.availFlow);
                       ELSE
                          ASK link TO SetSendAmount(shareAmount);
                       END IF;
                       commonTotal:=commonTotal+link.sendAmount;
                    END IF;
                 END FOR;
                 nodeIsFull:=FALSE;
                 IF (tempNode.typeNode<>3)
                     ASK tempNode TO DistributeFlow(commonTotal,returnAmount);
                     amountLeft:=amountLeft-commonTotal+returnAmount;
                 ELSE
                     amountLeft:=amountLeft-commonTotal;
                     returnAmount:=0;
                 END IF; 
                 IF returnAmount>0
                    nodeIsFull:=TRUE;
                 END IF;
                 k:=numStillAvail;
                 numStillAvail:=0;
                 FOR j:=groupStart TO groupEnd 
                    link:=ASK root Child("RBDLink", CapConnectPath[j]);
                    IF ((link.Status=LinkUp) AND (NOT link.nomReached) AND (NOT link.pathFull))
                       returnShare:=ROUND(FLOAT(returnAmount)/FLOAT(k));
                       DEC(k);
                       returnAmount:=returnAmount-returnShare;
                       IF nodeIsFull
                          ASK link TO SetPathFull(TRUE);
                       END IF;                                                                  
                       linkFlowValue:=link.sendAmount-returnShare+link.actualFlow;
                       ASK link TO SetActualFlow(linkFlowValue);
                       IF ((link.depLink>0) AND (GraphicsOutput))
                           tempLink := ASK root Child("RBDLink",link.depLink); 
                           ASK tempLink TO SetActualFlow(linkFlowValue);
                           IF tempLink.hasSiblings
                              FOREACH sibling IN  tempLink.siblingsGroup
                                 ASK sibling TO SetActualFlow(linkFlowValue);
                              END FOREACH;         
                           END IF;
                       END IF;
                       IF ((NOT link.nomReached) AND (NOT link.pathFull))
                          nomAvailable:=TRUE;
                          INC(numStillAvail);
                          ASK link TO SetAvailFlow(link.simNomFlow-link.actualFlow);
                       END IF;
                    END IF;
                 END FOR;              
              ELSE    {no node in common}
              
                 numStillAvail:=0;
                 FOR j:=groupStart TO groupEnd 
                    IF amountLeft>0
                       link:=ASK root Child("RBDLink", CapConnectPath[j]);
                       IF ((link.Status=LinkUp) AND (NOT link.nomReached) AND (NOT link.pathFull))
                          tempNode := ASK window Descendant("RBDNode", CapConnectTo[j]);
                          IF ((tempNode.Status=AllUp) OR (tempNode.Status=Degraded))                    
                             shareAmount:=ROUND(FLOAT(amountLeft)/FLOAT(k));
                             DEC(k);
                             IF shareAmount>link.availFlow
                                ASK link TO SetSendAmount(link.availFlow);
                             ELSE
                                ASK link TO SetSendAmount(shareAmount);
                             END IF;
                             IF (tempNode.typeNode<>3)
                                ASK tempNode TO DistributeFlow(link.sendAmount,returnAmount);
                                amountLeft:=amountLeft-link.sendAmount+returnAmount;
                                IF returnAmount>0
                                   ASK link TO SetPathFull(TRUE);
                                END IF;                                           
                             ELSE
                                amountLeft:=amountLeft-link.sendAmount;
                             END IF;                       
                             linkFlowValue:=link.sendAmount-returnAmount+link.actualFlow;
                             ASK link TO SetActualFlow(linkFlowValue);
                             IF link.depLink>0
                                tempLink := ASK root Child("RBDLink",link.depLink); 
                                ASK tempLink TO SetActualFlow(linkFlowValue);
                                IF tempLink.hasSiblings
                                   FOREACH sibling IN  tempLink.siblingsGroup
                                      ASK sibling TO SetActualFlow(linkFlowValue);
                                   END FOREACH;         
                                END IF;
                             END IF;
                             IF ((NOT link.nomReached) AND (NOT link.pathFull))
                                 nomAvailable:=TRUE;
                                 INC(numStillAvail);
                                 ASK link TO SetAvailFlow(link.simNomFlow-link.actualFlow);
                             END IF;
                          END IF;
                       END IF;
                    END IF;
                 END FOR;
              END IF;
           END WHILE;
           IF groupEnd=CapConnectOut
              allNomsFilled:=TRUE;
           END IF;
        END WHILE;   {filling paths up to nominal values}        
        nextGroupStart:=1;
        WHILE ((amountLeft>0) AND (NOT allMaxFilled))   
           groupStart:=nextGroupStart;
           numStillAvail:=0;
           maxAvailable:=FALSE;
           groupPrioritySet:=FALSE;
           groupEnd:=groupStart;
           j:=0;
           firstNode:=0;
           commonNode:=TRUE;
           IF groupStart<=CapConnectOut
              findingGroup:=TRUE;
              WHILE findingGroup           
                 link:=ASK root Child("RBDLink", CapConnectPath[groupStart+j]);
                 IF groupPrioritySet
                    IF link.simCapPriority>groupPriority
                       nextGroupStart:=groupStart+j;
                       findingGroup:=FALSE;
                       EXIT;
                    ELSE
                       groupEnd:=groupStart+j;
                    END IF;
                 ELSE
                    groupEnd:=groupStart+j;
                 END IF;
                 IF ((link.Status=LinkUp) AND (NOT link.maxReached) AND (NOT link.pathFull))
                    tempNode := ASK window Descendant("RBDNode", CapConnectTo[groupStart+j]);           
                    IF ((tempNode.Status=AllUp) OR (tempNode.Status=Degraded))                    
                       INC(numStillAvail);
                       maxAvailable:=TRUE;
                       ASK link TO SetAvailFlow(link.simMaxFlow-link.actualFlow);
                       IF (NOT groupPrioritySet)
                          groupPriority:=link.simCapPriority;
                          groupPrioritySet:=TRUE;
                          firstNode:=tempNode.Id;
                       ELSE
                          IF tempNode.Id<>firstNode
                             commonNode:=FALSE;
                          END IF;
                       END IF;
                    END IF;
                 END IF;                 
                 IF (groupStart+j)=CapConnectOut
                    findingGroup:=FALSE;
                 ELSE
                    INC(j);
                 END IF;
              END WHILE;
           END IF;
                  {try to send flow each way until max reached}
           WHILE ((amountLeft>0) AND maxAvailable)
              k:=numStillAvail;
              maxAvailable:=FALSE;
              IF commonNode
                 commonTotal:=0;
                 amountToDistribute:=amountLeft;
                 FOR j:=groupStart TO groupEnd 
                    link:=ASK root Child("RBDLink", CapConnectPath[j]);
                    IF ((link.Status=LinkUp) AND (NOT link.maxReached) AND (NOT link.pathFull))
                       shareAmount:=ROUND(FLOAT(amountToDistribute)/FLOAT(k));
                       amountToDistribute:=amountToDistribute-shareAmount;
                       DEC(k);
                       IF shareAmount>link.availFlow
                          ASK link TO SetSendAmount(link.availFlow);
                       ELSE
                          ASK link TO SetSendAmount(shareAmount);
                       END IF;
                       commonTotal:=commonTotal+link.sendAmount;
                    END IF;
                 END FOR;
                 nodeIsFull:=FALSE;
                 IF (tempNode.typeNode<>3)
                     ASK tempNode TO DistributeFlow(commonTotal,returnAmount);
                     amountLeft:=amountLeft-commonTotal+returnAmount;
                 ELSE
                     amountLeft:=amountLeft-commonTotal;
                     returnAmount:=0;
                 END IF; 
                 IF returnAmount>0
                    nodeIsFull:=TRUE;
                 END IF;
                 k:=numStillAvail; 
                 numStillAvail:=0;
                 FOR j:=groupStart TO groupEnd 
                    link:=ASK root Child("RBDLink", CapConnectPath[j]);
                    IF ((link.Status=LinkUp) AND (NOT link.maxReached) AND (NOT link.pathFull))
                       returnShare:=ROUND(FLOAT(returnAmount)/FLOAT(k));
                       DEC(k);
                       returnAmount:=returnAmount-returnShare;
                       IF nodeIsFull
                          ASK link TO SetPathFull(TRUE);
                       END IF;                                                                  
                       linkFlowValue:=link.sendAmount-returnShare+link.actualFlow;
                       ASK link TO SetActualFlow(linkFlowValue);
                       IF ((link.depLink>0) AND (GraphicsOutput))
                           tempLink := ASK root Child("RBDLink",link.depLink); 
                           ASK tempLink TO SetActualFlow(linkFlowValue);
                           IF tempLink.hasSiblings
                              FOREACH sibling IN  tempLink.siblingsGroup
                                 ASK sibling TO SetActualFlow(linkFlowValue);
                              END FOREACH;         
                           END IF;
                       END IF;
                       IF ((NOT link.maxReached) AND (NOT link.pathFull))
                          maxAvailable:=TRUE;
                          INC(numStillAvail);
                          ASK link TO SetAvailFlow(link.simMaxFlow-link.actualFlow);
                       END IF;
                    END IF;
                 END FOR;                            
              ELSE
                 numStillAvail:=0;
                 FOR j:=groupStart TO groupEnd 
                    IF amountLeft>0
                       link:=ASK root Child("RBDLink", CapConnectPath[j]);
                       IF ((link.Status=LinkUp) AND (NOT link.maxReached) AND (NOT link.pathFull))
                          tempNode := ASK window Descendant("RBDNode", CapConnectTo[j]);
                          IF ((tempNode.Status=AllUp) OR (tempNode.Status=Degraded))                                           
                             shareAmount:=ROUND(FLOAT(amountLeft)/FLOAT(k));
                             DEC(k);
                             IF shareAmount>link.availFlow
                                ASK link TO SetSendAmount(link.availFlow);
                             ELSE
                                ASK link TO SetSendAmount(shareAmount);
                             END IF;
                             IF (tempNode.typeNode<>3)
                                ASK tempNode TO DistributeFlow(link.sendAmount,returnAmount);
                                amountLeft:=amountLeft-link.sendAmount+returnAmount;
                                IF returnAmount>0
                                   ASK link TO SetPathFull(TRUE);
                                END IF;
                             ELSE
                                amountLeft:=amountLeft-link.sendAmount;
                             END IF;                       
                             linkFlowValue:=link.sendAmount-returnAmount+link.actualFlow;
                             IF link.depLink>0
                                tempLink := ASK root Child("RBDLink",link.depLink); 
                                ASK tempLink TO SetActualFlow(linkFlowValue);
                                IF tempLink.hasSiblings
                                   FOREACH sibling IN  tempLink.siblingsGroup
                                      ASK sibling TO SetActualFlow(linkFlowValue);
                                   END FOREACH;         
                                END IF;
                             ELSE
                                ASK link TO SetActualFlow(linkFlowValue);
                             END IF;
                             IF ((NOT link.maxReached) AND (NOT link.pathFull))
                                 maxAvailable:=TRUE;
                                 INC(numStillAvail);
                                 ASK link TO SetAvailFlow(link.simMaxFlow-link.actualFlow);
                             END IF;
                          END IF;
                       END IF;
                    END IF;
                 END FOR;
              END IF;
           END WHILE;
           IF groupEnd=CapConnectOut
              allMaxFilled:=TRUE;
           END IF;
        END WHILE;        
        rejected:=amountLeft;
        nodeFlow:=nodeFlow+sent-rejected;
        IF ((GraphicsOutput) AND (NOT checkingSystemCap) AND (typeNode=2))
           ShowFlow;
        END IF;
  END METHOD;   {DistributeFlow}


  ASK METHOD ResetNodeFlow;
  BEGIN
    nodeFlow:=0;
  END METHOD;
 
 
  ASK METHOD RunColdLogic;     {NEW}
  VAR
     upPaths,downPaths,idlePaths,standbyPaths,pmPaths,onPaths,path,i, j,
     switchingPaths,readyPaths,arrayIndex,
     switchPaths,availPaths,unavailablePaths,autoSwitchPaths, nonePaths,
     manSwitchPaths, RunsRequired, switchesRequired, numSwitching    : INTEGER;
     link                                                            : LinkObj;
     block                                                           : RBDBlockObj;
     node                                                            : RBDNodeObj;
     tempHier                                                        : RBDHierObj;
     priorStatus,newNodeStatus                                       : NodeStatusType;
     statusString,linkString,name1String,name2String                  : STRING;
     kMin, kMax  : INTEGER;                       {cmc  6/19/06}
  BEGIN
     IF NOT higherCold
        priorStatus:=Status;
        
        IF (NOT skipBackflow)
           FOR i:=1 TO EconnectIntoNum
              path:=coldLinks[i];
              link := ASK root Child("RBDLink", path);                         
              IF link.EconnectFRef="RBDBlock" 
                 block := ASK root Child("RBDBlock",link.EconnectFromId);
                 IF block.opStatus=Standby
                    ASK block TO SetIgnoreDep(TRUE);
                 ELSIF ((block.opStatus=Idle) AND (block.simDependencyNum<>Id))  
                    ASK block TO ChangeBlockState(Running,block.activeStatus,"Dependence"); 
                 END IF;
              ELSIF link.EconnectFRef="RBDNode"
                 node:=ASK root Child("RBDNode", link.EconnectFromId);
                 IF node.Status=NodeStandby
                    ASK node TO SetIgnoreDep(TRUE);  
                 END IF;
              END IF;                       
           END FOR;        

           BackFlowDependencies(AllUp);      {prepping blocks for link survey}
           FOR i:=1 TO EconnectIntoNum
              path:=coldLinks[i];
              link := ASK root Child("RBDLink", path);                         
              IF ( (link.Status=LinkStandby) AND 
                      NOT ((link.switchStatus="manualInProgress")  OR  
                           (link.switchStatus="autoInProgress")) )
                 IF link.EconnectFRef="RBDBlock" 
                    block := ASK root Child("RBDBlock",link.EconnectFromId);
                    ASK block TO SetIgnoreDep(FALSE);      
                 ELSIF link.EconnectFRef="RBDNode"
                    node:=ASK root Child("RBDNode", link.EconnectFromId);
                    ASK node TO SetIgnoreDep(FALSE);      
                 END IF;                       
              END IF;
           END FOR; 
        END IF;  {NOT skipbackflow}      
  {Set variables}   
        readyPaths:=0;
        switchingPaths:=0;
        autoSwitchPaths:=0;
        manSwitchPaths:=0;  
        nonePaths:=0;
        unavailablePaths:=0;
        upPaths  :=0;
        downPaths:=0;
        idlePaths:=0;
        standbyPaths:=0;
        switchPaths:=0;
        pmPaths:=0;
        NumGoodPaths:=0; 
        availPaths:=0;
        linkString:="";
        
        
  {Survey inbound links}   
        FOR i:=1 TO EconnectIntoNum
           path:=coldLinks[i];
           link := ASK root Child("RBDLink", path);                         
           CASE link.Status            
              WHEN LinkUp:
                 IF link.switchStatus="ready"
                    INC(readyPaths);
                 ELSIF   ((link.switchStatus="autoInProgress") OR (link.switchStatus="manualInProgress"))
                    INC(switchingPaths);
                 ELSIF  (link.switchStatus="auto") 
                    INC(autoSwitchPaths);
                 ELSIF  (link.switchStatus="manual")
                    INC(manSwitchPaths);
                 ELSIF  (link.switchStatus="none")
                    INC(nonePaths);
                 END IF;
              WHEN LinkDown:
                 INC(unavailablePaths);
              WHEN LinkIdle:
                 INC(unavailablePaths);
              WHEN LinkDone:
                 INC(unavailablePaths);
              WHEN LinkPM:  
                 INC(unavailablePaths);
              WHEN LinkStandby:  
                 IF link.switchStatus="ready"
                    INC(readyPaths);
                 ELSIF   ((link.switchStatus="autoInProgress") OR (link.switchStatus="manualInProgress"))
                    INC(switchingPaths);
                 ELSIF  (link.switchStatus="auto") 
                    INC(autoSwitchPaths);
                 ELSIF  (link.switchStatus="manual")
                    INC(manSwitchPaths);
                 ELSIF  (link.switchStatus="none")
                    INC(nonePaths);
                 END IF;
              OTHERWISE 
                 NEW(message,1..1);
                 message[1]:="Error in link status logic (cold logic)!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                 DISPOSE(message); 
           END CASE;
        END FOR;
        
        
        kMin:=GoodPathsRequired;               {cmc 6/19/06}
        kMax:=KStar;
        IF ((usesPhasing) AND (activePhases>0)) 
            IF KStar<GoodPathsRequired             {cmc 12/21/06}
               kMax:=GoodPathsRequired;
            END IF;                        
        END IF;
        
{removed}        
        {IF ((usesPhasing) AND (activePhases>0))         {cmc 6/19/06}
            RunsRequired:=GoodPathsRequired;
        ELSE  }

           IF readyPaths>=kMax        
              RunsRequired:=kMax;
           ELSIF readyPaths>kMin;
              RunsRequired:=readyPaths;
           ELSE
              RunsRequired:=kMin;
           END IF;
{removed}  {END IF;}                                          {cmc 6/19/06}
{enough paths available - node is up}     
        IF readyPaths>=RunsRequired        
           onPaths:=0;
           FOR i:=1 TO EconnectIntoNum
              path:=coldLinks[i];
              link := ASK root Child("RBDLink", path); 
              IF (link.Status = LinkUp)             
                 IF ((link.switchStatus="ready") AND (onPaths<RunsRequired))
                    INC(onPaths);                     
                 ELSE   {paths is up and switchable}
                    IF link.EconnectFRef="RBDBlock" 
                       block := ASK root Child("RBDBlock",link.EconnectFromId);
                       IF (block.opStatus=Running)        {IF added on 1/7/05 chuck}   {block could be link-failed}
                          ASK block TO ChangeBlockState(Standby,block.activeStatus,"Too_many_on");
                          IF block.hasDepObjects
                             ASK block TO BackFlowDependencies(NodeStandby);
                          END IF;
                          IF SimTime>0.0
                             ASK block TO SetInterruptReason("Switched_Off");
                             IF block.FailWait
                                Interrupt(block,"Run");
                             ELSIF block.ZeroWait
                                ASK BlockTrigger[block.seqNum] TO Release;
                             END IF;
                          END IF;   
                       END IF;
                    ELSIF link.EconnectFRef="RBDEvent"
                       ASK link TO SetStuckOn(TRUE);
                       coldsAffected:=TRUE;                                  
                    ELSIF link.EconnectFRef="RBDNode"
                       node:=ASK root Child("RBDNode", link.EconnectFromId);
                       IF node.coldStandby
                          ASK node TO SetHigherCold(TRUE);
                       END IF;
                       ASK node TO BackFlowDependencies(NodeStandby);
                       coldsAffected:=TRUE;
                    END IF;
                 END IF;
              ELSIF (link.Status = LinkStandby)  
                 IF ((link.switchStatus="ready") AND (onPaths<RunsRequired))
                    INC(onPaths);
                    IF link.EconnectFRef="RBDBlock"                 
                       block := ASK root Child("RBDBlock",link.EconnectFromId);  
                       IF (block.opStatus=Standby)      {block could be linked}
                          ASK block TO ChangeBlockState(Running,block.activeStatus,"Prior=Standby");
                          IF block.hasDepObjects
                             ASK block TO BackFlowDependencies(AllUp);
                          END IF;
                          ASK block TO SetInterruptReason("Switched_On");
                          IF block.FailWait
                            Interrupt(block,"Run");
                          ELSIF block.ZeroWait
                            ASK BlockTrigger[block.seqNum] TO Release;
                          END IF;
                       END IF;
                    ELSIF link.EconnectFRef="RBDNode"
                       node:=ASK root Child("RBDNode", link.EconnectFromId);
                       IF node.coldStandby
                          ASK node TO SetHigherCold(FALSE);
                       END IF;
                       ASK node TO BackFlowDependencies(AllUp);  
                    END IF;
                 ELSIF   ((link.switchStatus="autoInProgress") OR (link.switchStatus="manualInProgress"))
                    IF RunsRequired>=kMax               {cmc 6/19/06   Do not interrupt if kMax is not met}
                       Interrupt(link,"PerformSwitch");
                    END IF;                    
                 ELSIF   ((link.switchStatus="none") AND (onPaths<RunsRequired))
                    INC(NumCurrentlyFailed);
                    IF parentID>0              
                       tempHier := ASK root Child("RBDHier", parentID);  
                       ASK tempHier TO IncNumFailed;
                    END IF;
                    ASK link TO ChangeLinkStatus(LinkDone);
                    FOR j:=1 TO EconnectIntoNum
                       IF link.Id = inPathsArray[j]                    
                          arrayIndex:=j;
                       END IF;
                    END FOR;
                    pathFailValue[arrayIndex]:=pathFailValue[arrayIndex]+1;
                    pathStandbyValue[arrayIndex]:=pathStandbyValue[arrayIndex]-1; 
                    name1String:=name;
                    IF link.EconnectFRef="RBDBlock"                 
                       block := ASK root Child("RBDBlock",link.EconnectFromId);
                       name2String:=block.name;
                    ELSIF link.EconnectFRef="RBDNode"
                       node:=ASK root Child("RBDNode", link.EconnectFromId);
                       name2String:=node.name;
                    END IF;
                    IF EventsFile
                       WriteEvents(SimTime,"Node",name1String,"Switch_Failed","Unable_to_switch_to_"+name2String);
                    END IF;
                 ELSE
                    { block is ready but enough have been turned on 
                       or block is switchable with t>0  }
                    {do nothing};
                 END IF;
              END IF;           
           END FOR;  

{WAS 
 
{not enough paths available - node is down}     
        ELSE  {readyPaths<RunsRequired}
           switchesRequired:=(RunsRequired-readyPaths);
}           

{Start new}
        END IF;
        
        
        { Section below used to be ELSE from above section  (readypaths>= kMin)
          The only time both sections are hit is when (( kStar>k) and (k is met but kStar isn't)
         }
     
        IF  (readyPaths<kMax)                      
           switchesRequired:=(kMax-readyPaths);
           
{End new       cmc}           
           
           IF switchingPaths>switchesRequired   
      {too many switching}
              numSwitching:=0;           
              FOR i:=1 TO EconnectIntoNum
                 path:=coldLinks[i];
                 link := ASK root Child("RBDLink", path); 
                 IF (link.Status = LinkStandby)             
                    IF ((link.switchStatus="autoInProgress") OR (link.switchStatus="manualInProgress"))
                       IF numSwitching<switchesRequired
                          INC(numSwitching);
                       ELSE
                          Interrupt(link,"PerformSwitch");                                            
                       END IF;
                    END IF;
                 END IF;
              END FOR;
                           
           ELSIF (  (switchingPaths<switchesRequired) AND ( (autoSwitchPaths + manSwitchPaths + nonePaths)>0 )  )
       {not enough switching}
              IF checkAutosFirst 
                 IF (autoSwitchPaths+nonePaths>0)   {time saver if manuals only}
                    FOR i:=1 TO EconnectIntoNum
                       path:=coldLinks[i];
                       link := ASK root Child("RBDLink", path); 
                       IF (switchingPaths<switchesRequired)
                          IF (  ((link.Status = LinkStandby) OR (link.Status = LinkUp)) 
                               AND (link.switchStatus="auto") )
                             ASK link TO PrepForSwitch; 
                             TELL link TO PerformSwitch;
                             INC(switchingPaths);
                          ELSIF  (  ((link.Status = LinkStandby) OR (link.Status = LinkUp)) 
                               AND (link.switchStatus="none") )
                             ReconfigureSystem(link.Id);
                             INC(NumCurrentlyFailed);
                             IF parentID>0              
                                tempHier := ASK root Child("RBDHier", parentID);  
                                ASK tempHier TO IncNumFailed;
                             END IF;
                             ASK link TO ChangeLinkStatus(LinkDone);
                             FOR j:=1 TO EconnectIntoNum
                                IF link.Id = inPathsArray[j]                    
                                   arrayIndex:=j;
                                END IF;
                             END FOR;
                             pathFailValue[arrayIndex]:=pathFailValue[arrayIndex]+1;
                             pathStandbyValue[arrayIndex]:=pathStandbyValue[arrayIndex]-1; }
                             name1String:=name;
                             IF link.EconnectFRef="RBDBlock"                 
                                block := ASK root Child("RBDBlock",link.EconnectFromId);
                                name2String:=block.name;
                             ELSIF link.EconnectFRef="RBDNode"
                                node:=ASK root Child("RBDNode", link.EconnectFromId);
                                name2String:=node.name;
                             END IF;
                             IF EventsFile
                                WriteEvents(SimTime,"Node",name1String,"Switch_Failed","Unable_to_switch_to_"+name2String);
                             END IF;
                          END IF;
                       END IF;
                    END FOR;                
                 END IF;
                 IF (switchingPaths<switchesRequired)    {time saver if enough autos already switched}
                    FOR i:=1 TO EconnectIntoNum
                       path:=coldLinks[i];
                       link := ASK root Child("RBDLink", path); 
                       IF (switchingPaths<switchesRequired)
                          IF (  ((link.Status = LinkStandby) OR (link.Status = LinkUp)) 
                                AND (link.switchStatus="manual") )
                             ASK link TO PrepForSwitch;   
                             TELL link TO PerformSwitch;
                             INC(switchingPaths);
                          ELSE
                             {path is already standby and not needed}
                          END IF;
                       END IF;
                    END FOR;
                 END IF;
              ELSE    {not checkAutosFirst}
                 FOR i:=1 TO EconnectIntoNum
                    path:=coldLinks[i];
                    link := ASK root Child("RBDLink", path); 
                    IF (switchingPaths<switchesRequired)
                       IF (  ((link.Status = LinkStandby) OR (link.Status = LinkUp)) AND    
                          ((link.switchStatus="auto") OR (link.switchStatus="manual"))  )
                          ASK link TO PrepForSwitch;
                          TELL link TO PerformSwitch;
                          INC(switchingPaths);
                       ELSIF  (  ((link.Status = LinkStandby) OR (link.Status = LinkUp)) 
                             AND (link.switchStatus="none") )
                          ReconfigureSystem(link.Id);
                          INC(NumCurrentlyFailed);
                          IF parentID>0              
                             tempHier := ASK root Child("RBDHier", parentID);  
                             ASK tempHier TO IncNumFailed;
                          END IF;
                          ASK link TO ChangeLinkStatus(LinkDone);
                          FOR j:=1 TO EconnectIntoNum
                             IF link.Id = inPathsArray[j]                    
                                arrayIndex:=j;
                             END IF;
                          END FOR;
                          pathFailValue[arrayIndex]:=pathFailValue[arrayIndex]+1;
                          pathStandbyValue[arrayIndex]:=pathStandbyValue[arrayIndex]-1; }
                          name1String:=name;
                          IF link.EconnectFRef="RBDBlock"                 
                             block := ASK root Child("RBDBlock",link.EconnectFromId);
                             name2String:=block.name;
                          ELSIF link.EconnectFRef="RBDNode"
                             node:=ASK root Child("RBDNode", link.EconnectFromId);
                             name2String:=node.name;
                          END IF;
                          IF EventsFile
                             WriteEvents(SimTime,"Node",name1String,"Switch_Failed","Unable_to_switch_to_"+name2String);
                          END IF;
                       END IF;
                    END IF;
                 END FOR;
              END IF;   {CheckAutosFirst}   
           END IF;  {switchingPaths>switchesRequired}
           IF ((priorStatus=Down) OR (priorStatus=NodePM)) 
              specialBackFlow:=TRUE;   {the setting of variable specialBackFlow added 5-24-05 to fix Rap7 error 999}
           END IF;   
           BackFlowDependencies(Down); 
           specialBackFlow:=FALSE;
        END IF;  {readyPaths>RunsRequired}
                     
{Resurvey inbound links}     
          
        FOR i:=1 TO EconnectIntoNum
           path:=coldLinks[i];
           link := ASK root Child("RBDLink", path);                         
           CASE link.Status            
              WHEN LinkUp:
                 IF ((link.switchStatus="manualInProgress") OR (link.switchStatus="autoInProgress"))
                    INC(downPaths);
                    linkString:=linkString+"_dsw"+INTTOSTR(path);                 
                 ELSE
                    INC(upPaths);
                    linkString:=linkString+"_up"+INTTOSTR(path);        
                 END IF;
              WHEN LinkDown:
                 INC(downPaths);
                 linkString:=linkString+"_df"+INTTOSTR(path);
              WHEN LinkIdle:
                 INC(idlePaths);
                 linkString:=linkString+"_di"+INTTOSTR(path);
              WHEN LinkDone:
                 INC(downPaths);
                 linkString:=linkString+"_dd"+INTTOSTR(path);
              WHEN LinkPM:  
                 INC(pmPaths);         {this case missing tony 5-10-04}
                 linkString:=linkString+"_dpm"+INTTOSTR(path);
              WHEN LinkStandby: 
                 IF ((link.switchStatus="manualInProgress") OR (link.switchStatus="autoInProgress"))
                    INC(downPaths);
                    linkString:=linkString+"_dsw"+INTTOSTR(path);                 
                 ELSE
                    INC(standbyPaths);                    
                    linkString:=linkString+"_s"+INTTOSTR(path);
                 END IF;
              OTHERWISE 
                 NEW(message,1..1);
                 message[1]:="Error in link status logic (cold logic)!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                 DISPOSE(message); 
           END CASE;
        END FOR;        
     
{Establish node color}     
     
        IF GoodPathsRequired>EconnectIntoNum  {Node is CUT (set to k=n+1)}
           newNodeStatus:=Down;
           statusString:="Bad";
        ELSIF upPaths=EconnectIntoNum
           newNodeStatus:=AllUp;
           statusString:="Good";
        ELSIF upPaths>=GoodPathsRequired
           IF (upPaths+standbyPaths)=EconnectIntoNum
              newNodeStatus:=AllUp;
              statusString:="Good";              
           ELSE
              newNodeStatus:=Degraded;
              statusString:="Degraded";
           END IF;
          { ELSIF upPaths+standbyPaths>=GoodPathsRequired
              newNodeStatus:=NodeStandby;
              statusString:="Standby"; }
           {ELSIF idlePaths+upPaths+standbyPaths>=GoodPathsRequired }
        ELSIF idlePaths+upPaths>=GoodPathsRequired
           newNodeStatus:=NodeIdle;
           statusString:="Idle";
           {ELSIF pmPaths+idlePaths+upPaths+standbyPaths>=GoodPathsRequired   {tony 5-12} }
        ELSIF pmPaths+idlePaths+upPaths>=GoodPathsRequired   {tony 5-12}
           newNodeStatus:=NodePM;
           statusString:="NodePM";
        ELSIF downPaths >0
           newNodeStatus:=Down;
           statusString:="Bad";
        ELSIF switchPaths >0
           newNodeStatus:=Down;
           statusString:="Bad";
        ELSE
           NEW(message,1..1);
           message[1]:="Error in node status logic (cold logic)!     "; 
           result:=SendAlert(message,FALSE, FALSE, TRUE);
           DISPOSE(message); 
        END IF; 
        {AllUp,Degraded,Down,NodeIdle,NodeStandby}
        IF EventsFile
           WriteEvents(SimTime,"Node",name,statusString,INTTOSTR(upPaths)+"/"+INTTOSTR(EconnectIntoNum)+linkString);
        END IF;
        IF priorStatus<>newNodeStatus
           ChangeNodeState(newNodeStatus,activeStatus);
        END IF;   {node status has changed}
     ELSE
        ReconfigureNode(0,0,0,0,0,0);
     END IF;
  END METHOD;    {RunColdLogic}
  
  ASK METHOD DisposeSimVars;
  BEGIN
     IF (typeNode<>1)
        DISPOSE(pathFailValue);
        DISPOSE(pathIdleValue);
        DISPOSE(pathStandbyValue);
        DISPOSE(pathPMValue);
     END IF;
     IF coldStandby
        DISPOSE(coldLinks);      
     END IF;
  END METHOD;
  
  ASK METHOD fevInit;
  VAR      
     nodeImage         : ImageObj;
  BEGIN
     nodeImage := Descendant("Node", 602); 
     IF typeNode = 1
        ASK nodeImage TO SetColor(Black); 
        ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
     ELSIF typeNode = 3
        ASK nodeImage TO SetColor(Black);  
        ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
     ELSIF typeNode = 4
        ASK nodeImage TO SetColor(DarkSlateBlue); 
        ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
     ELSIF typeNode = 5
        ASK nodeImage TO SetColor(LimeGreen);  
        ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
     ELSE                       
        ASK nodeImage TO SetColor(LimeGreen);
        ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
     END IF;
     Draw; 
  END METHOD;
    
  ASK METHOD CleanUp;
  VAR      
     nodeImage           : ImageObj;
     tempImage           : FillVObj;
     hier                : RBDHierObj;
  BEGIN
     nodeImage := Descendant("Node", 602); 
     IF typeNode = 1
        ASK nodeImage TO SetColor(Black); 
        ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
     ELSIF typeNode = 3
        ASK nodeImage TO SetColor(Black);  
        ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
     ELSIF typeNode = 4
        ASK nodeImage TO SetColor(DarkSlateBlue); 
        ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
     ELSIF typeNode = 5
        ASK nodeImage TO SetColor(DarkSlateBlue);  
        ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
     ELSE                       
        ASK nodeImage TO SetColor(blockGUIColor);
        ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
     END IF;
     IF ((usesPhasing) AND (phase <> NILARRAY))                                   
        IF (typeNode=5)
           hier := ASK root Child("RBDHier",parentID);      
           tempImage:=ASK hier Child("Hier",603);
        ELSE
           tempImage:=Child("Node",602);
        END IF;
        ASK tempImage TO SetStyle(NarrowCrosshatchFill);
     END IF;
     Draw; 
  END METHOD;
  
  ASK METHOD ChangeNodeState (IN newcolor               : NodeStatusType;
                              IN newActiveStatus        : ActiveStatusType);
  VAR
     nodeImage                                          : ImageObj;
     priorStatus                                        : NodeStatusType;
     statusTime                                         : REAL;
     hier                                               : RBDHierObj;
     runChange,failChange,idleChange,standbyChange,
     pmChange,gre,yel,sie,blu,red,ora,j,DownstreamNode, 
     PathLink                                           : INTEGER;
     oldActiveStatus                                    : ActiveStatusType;
     tempnode                                           : RBDNodeObj;
  BEGIN
     priorStatus:=Status;
     Status:=newcolor; 
     oldActiveStatus:=activeStatus;
     activeStatus:=newActiveStatus;
     SystemStateChange:=TRUE;
     IF Status=Down 
        IF (weakAnalysis)
           IF PhaseChangeInProgress
              IF NOT (NodeFailureGroup.Includes(SELF))
                 ASK NodeFailureGroup TO Add(SELF);
              END IF;
           ELSE
              nodeMissionBool[seqNum]:=FALSE;
           END IF;
        END IF;
     END IF;
     IF ((Status=NodePM) AND (weakAnalysis))
        nodeMissionBool[seqNum]:=FALSE;
     END IF;   
     IF (weakAnalysis AND (typeNode<>1))
        statusTime:=SimTime-stateStartTime;
        CASE priorStatus
           WHEN AllUp:
               NodeGreenTime[seqNum]:=NodeGreenTime[seqNum]+statusTime;
           WHEN Degraded:
               NodeYellowTime[seqNum]:=NodeYellowTime[seqNum]+statusTime;
           WHEN Down:
               NodeRedTime[seqNum]:=NodeRedTime[seqNum]+statusTime;
               IF typeNode=5
                  hier := ASK root Child("RBDHier",parentID);
                  INC(hierDE[hier.seqNum]);
               END IF;   
           WHEN NodeStandby:
               NodeBlueTime[seqNum]:=NodeBlueTime[seqNum]+statusTime;
           WHEN NodeIdle:
               NodeBrownTime[seqNum]:=NodeBrownTime[seqNum]+statusTime;
           WHEN NodePM:
               NodeOrangeTime[seqNum]:=NodeOrangeTime[seqNum]+statusTime;
               IF typeNode=5
                  hier := ASK root Child("RBDHier",parentID);
                  INC(hierDE[hier.seqNum]);
               END IF;   
           OTHERWISE
        END CASE; 
     END IF;
     stateStartTime:=SimTime;
     IF GraphicsOutput   
        nodeImage := Descendant("Node", 602); 
        IF  ((typeNode<>1) AND (typeNode<>3))
         CASE Status
           WHEN AllUp:
              IF typeNode = 4
                 ASK nodeImage TO SetColor(DarkSlateBlue); 
                 ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
              ELSIF typeNode = 5
                 ASK nodeImage TO SetColor(LimeGreen);  
                 ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
              ELSE                       
                 ASK nodeImage TO SetColor(LimeGreen);
                 ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
              END IF;
           WHEN Degraded:
              ASK nodeImage TO SetColor(Yellow);
              ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
           WHEN NodeIdle:
              ASK nodeImage TO SetColor(Sienna);
              ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
           WHEN NodeStandby:
              ASK nodeImage TO SetColor(Blue);
              ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
           WHEN NodePM:
              ASK nodeImage TO SetColor(Orange);
              ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
           WHEN Down:
              ASK nodeImage TO SetColor(Red);
              ASK nodeImage TO SetTranslation(nodeImage.Translation.x, nodeImage.Translation.y);
           OTHERWISE 
              NEW(message,1..1);
              message[1]:="Unknown node status color!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message);   
         END CASE;
        END IF;
        Draw; 
        IF typeNode=5      {eliz}
           hier:=ASK root Child("RBDHier", parentID);
           ASK hier TO ChangeStatus(Status);              
           IF NOT runCompleted    
              gre:=0;
              yel:=0;
              sie:=0;
              blu:=0;
              red:=0;
              ora:=0;
              IF priorStatus<>Status; 
                 CASE priorStatus
                    WHEN AllUp:
                       gre:=-1;
                    WHEN Degraded:
                       yel:=-1;
                    WHEN NodeIdle:
                       sie:=-1;
                    WHEN NodeStandby:
                       blu:=-1;
                    WHEN Down:
                       red:=-1;
                    WHEN NodePM:
                       ora:=-1;
                    OTHERWISE 
                       NEW(message,1..1);
                       message[1]:="Unknown hierarchy status color!     "; 
                       result:=SendAlert(message,FALSE, FALSE, TRUE);
                       DISPOSE(message);   
                 END CASE;
                 CASE Status
                    WHEN AllUp:
                       gre:=1;
                    WHEN Degraded:
                       yel:=1;
                    WHEN NodeIdle:
                       sie:=1;
                    WHEN NodeStandby:
                       blu:=1;
                    WHEN Down:
                       red:=1;
                    WHEN NodePM:
                       ora:=1;
                    OTHERWISE 
                       NEW(message,1..1);
                       message[1]:="Unknown hierarchy status color!     "; 
                       result:=SendAlert(message,FALSE, FALSE, TRUE);
                       DISPOSE(message);   
                 END CASE;
              END IF;
              ASK hier TO ChangeDSIStatus(gre,blu,yel,sie,ora,red);
           END IF;
        END IF; {TypeNode=5}
     END IF;   {GraphicsOutput}
     { END IF;  priorStatus<>Status}
          
     IF NOT runCompleted    
        IF typeNode=3
           IF ( ((priorStatus=AllUp) OR (priorStatus=Degraded) OR (priorStatus=NodeStandby)) AND  {error fix 999}
                   ((Status=Down) OR (Status=NodeIdle) OR (Status=NodePM))   ) 
         {was up,now down}
              SystemUp:=FALSE;
              AnalyzeSystem;
           ELSIF ( ((priorStatus=Down) OR (priorStatus=NodeIdle) OR (priorStatus=NodePM)) AND
                   ((Status=AllUp) OR (Status=Degraded) OR (Status=NodeStandby))   )                                   
         {was down,now up}
              SystemUp:=TRUE;
              IF ((CountFailure) AND (termType=2))
                 termCounter:=termCounter+1;
              ELSE
                 CountFailure:=TRUE;
              END IF;
              SysTimeToRepair:=SimTime-LastUpDownChange;  
              IF SysRepTimeFile AND StatsStarted
                 ASK RepTimeStream TO WriteReal(SysTimeToRepair,19,9);  
                 ASK RepTimeStream TO WriteLn;   
              END IF;
              IF ((termType=2) AND (NOT FEVmode))    
                 IF statusBarOn
                    ASK window TO ShowStatus(2,INTTOSTR(termCounter) + " system failure(s)");
                 END IF;
                 IF ((termCounter>0) AND (termCounter=TRUNC(StartStats)))
                    TELL System ResetAllStatistics;
                 END IF;
                 IF (termCounter=TRUNC(StopCriteria)) 
                    TELL System TO CallEndSimulation(FALSE,TRUE) IN 0.0;  
                 END IF;
              END IF;
              LastUpDownChange:=SimTime;
           END IF;
        ELSE  
           IF ((oldActiveStatus=Linked) AND (newActiveStatus=Linked))
              {do nothing}
           ELSIF ((oldActiveStatus=Cut) AND (newActiveStatus=Cut))
              {do nothing}        
           ELSIF ((oldActiveStatus=Cut) AND (newActiveStatus=Linked))
              runChange:=1;
              failChange:=-1;
           ELSIF ((oldActiveStatus=Linked) AND (newActiveStatus=Cut))
              runChange:=-1;
              failChange:=1;
           ELSIF (oldActiveStatus=Linked)     {now Active}           
              IF ((Status=AllUp) OR (Status=Degraded))       
                 {do nothing}
              ELSIF (Status=NodeStandby) 
                 runChange:=-1;
                 standbyChange:=1;
              ELSIF (Status=NodeIdle) 
                 runChange:=-1;
                 idleChange:=1;
              ELSIF (Status=NodePM) 
                 runChange:=-1;
                 pmChange:=1;
              ELSIF (Status=Down) 
                 runChange:=-1;
                 failChange:=1;
              END IF;
           ELSIF (oldActiveStatus=Cut)     {now Active}
              IF ((Status=AllUp) OR (Status=Degraded))       
                 failChange:=-1;
                 runChange:=1;
              ELSIF (Status=NodeStandby) 
                 failChange:=-1;
                 standbyChange:=1;
              ELSIF (Status=NodeIdle) 
                 failChange:=-1;
                 idleChange:=1;
              ELSIF (Status=NodePM) 
                 failChange:=-1;
                 pmChange:=1;
              ELSIF (Status=Down) 
                 {do nothing}
              END IF;
           ELSIF ((priorStatus=AllUp) OR (priorStatus=Degraded))     {were Active}
              IF (newActiveStatus=Linked)      
                 {do nothing}
              ELSIF (newActiveStatus=Cut)       
                 runChange:=-1;
                 failChange:=1;
              ELSIF ((Status=AllUp) OR (Status=Degraded))       
                 {do nothing}
              ELSIF (Status=NodeStandby) 
                 runChange:=-1;
                 standbyChange:=1;
              ELSIF (Status=NodeIdle) 
                 runChange:=-1;
                 idleChange:=1;
              ELSIF (Status=NodePM) 
                 runChange:=-1;
                 pmChange:=1;
              ELSIF (Status=Down) 
                 runChange:=-1;
                 failChange:=1;
              END IF;
           ELSIF (priorStatus=NodeStandby)     
              IF (newActiveStatus=Linked)      
                 standbyChange:=-1;
                 runChange:=1;
              ELSIF (newActiveStatus=Cut)       
                 standbyChange:=-1;
                 failChange:=1;
              ELSIF ((Status=AllUp) OR (Status=Degraded))       
                 standbyChange:=-1;
                 runChange:=1;
              ELSIF (Status=NodeStandby) 
                 {do nothing};
              ELSIF (Status=NodeIdle) 
                 standbyChange:=-1;
                 idleChange:=1;
              ELSIF (Status=NodePM) 
                 standbyChange:=-1;
                 pmChange:=1;
              ELSIF (Status=Down) 
                 standbyChange:=-1;
                 failChange:=1;
              END IF;
           ELSIF (priorStatus=NodeIdle)     
              IF (newActiveStatus=Linked)      
                 idleChange:=-1;
                 runChange:=1;
              ELSIF (newActiveStatus=Cut)       
                 idleChange:=-1;
                 failChange:=1;
              ELSIF ((Status=AllUp) OR (Status=Degraded))       
                 idleChange:=-1;
                 runChange:=1;
              ELSIF (Status=NodeStandby) 
                 idleChange:=-1;
                 standbyChange:=1;
              ELSIF (Status=NodeIdle) 
                 {do nothing}
              ELSIF (Status=NodePM) 
                 idleChange:=-1;
                 pmChange:=1;
              ELSIF (Status=Down) 
                 idleChange:=-1;
                 failChange:=1;
              END IF;
           ELSIF (priorStatus=NodePM)     
              IF (newActiveStatus=Linked)      
                 pmChange:=-1;
                 runChange:=1;
              ELSIF (newActiveStatus=Cut)       
                 pmChange:=-1;
                 failChange:=1;
              ELSIF ((Status=AllUp) OR (Status=Degraded))       
                 pmChange:=-1;
                 runChange:=1;
              ELSIF (Status=NodeStandby) 
                 pmChange:=-1;
                 standbyChange:=1;
              ELSIF (Status=NodeIdle) 
                 pmChange:=-1;
                 idleChange:=1;
              ELSIF (Status=NodePM) 
                 {do nothing}
              ELSIF (Status=Down) 
                 pmChange:=-1;
                 failChange:=1;
              END IF;
           ELSIF (priorStatus=Down)     
              IF (newActiveStatus=Linked)      
                 failChange:=-1;
                 runChange:=1;
              ELSIF (newActiveStatus=Cut)       
                 {do nothing}
              ELSIF ((Status=AllUp) OR (Status=Degraded))       
                 failChange:=-1;
                 runChange:=1;
              ELSIF (Status=NodeStandby) 
                 failChange:=-1;
                 standbyChange:=1;
              ELSIF (Status=NodeIdle) 
                 failChange:=-1;
                 idleChange:=1;
              ELSIF (Status=NodePM) 
                 failChange:=-1;
                 pmChange:=1;
              ELSIF (Status=Down) 
                 {do nothing}
              END IF;
           END IF
           IF (ABS(runChange)+ABS(failChange)+ABS(idleChange)+ABS(standbyChange)+ABS(pmChange)=2)
              FOR j:=1 TO EFPAConnectOut  
                 DownstreamNode:=EFPAConnectTo[j]; 
                 PathLink:=EFPAConnectPath[j];                 
                 tempnode := ASK window Descendant("RBDNode", DownstreamNode);
                 ASK tempnode TO ReconfigureNode(runChange,failChange,idleChange,standbyChange,pmChange,PathLink);
              END FOR;
           ELSIF (ABS(runChange)+ABS(failChange)+ABS(idleChange)+ABS(standbyChange)+ABS(pmChange)<>0)
              NEW(message,1..1);
              message[1]:="Error in ChangeNodeState!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message);
           END IF;
        END IF;   {typeNode=3}
        IF ((simInProgress) AND (hasDepObjects) AND (priorStatus<>Status) AND (NOT coldStandby))
           IF ((priorStatus=Down) OR (priorStatus=NodePM)) 
              IF ((Status=NodeStandby) OR (higherCold))
                  BackFlowDependencies(NodeStandby);
              ELSIF ((Status=AllUp) OR (Status=Degraded) OR (Status=NodeIdle))
                  BackFlowDependencies(AllUp);
              END IF;
           ELSIF ((Status=Down) OR (Status=NodePM))
              BackFlowDependencies(Down);              
           END IF;
        END IF;
     END IF;  {NOT runCompleted}
  END METHOD;   {ChangeNodeState}

  ASK METHOD ChangePhases(IN oldVal,newVal : INTEGER);
  BEGIN
     IF typeNode=5
        IF (newVal=0)  {Link}
           ChangeNodeState(Status,Linked);
        ELSIF (newVal=2) {Cut}
           ChangeNodeState(Status,Cut);
        ELSE
           ChangeNodeState(Status,Active);
        END IF;        
     ELSE     
        GoodPathsRequired:=newVal;
        IF GraphicsOutput
           SetKofN(GoodPathsRequired,EconnectIntoNum);
        END IF;           
        ReconfigureNode(0,0,0,0,0,0);
     END IF;
  END METHOD;

  ASK METHOD TestFail (IN mama          : INTEGER);     
     VAR
        j                               : INTEGER;
        link                            : LinkObj;
        node                            : RBDNodeObj;
        hier                            : RBDHierObj;
     BEGIN 
        IF Id=TestElement    
           INC(LoopCheck);
           IF LoopCheck=2
              LoopError:=TRUE;
              RETURN;
           END IF;
        END IF;
        NumGoodPaths:=NumGoodPaths-1;
        IF (NumGoodPaths=GoodPathsRequired-1)     
          { IF ( (typeNode=2) OR ( (typeNode=5) AND ((parentID>mama) OR (Id=TestElement)) )  )  }
               {tony 5-17-04: this parentID>mama seems to be causing the trouble in EAGs crash}
           
           IF typeNode=5
              hier := ASK root Child("RBDHier",parentID);
           END IF;
           
           IF ( (typeNode=2) OR ( (typeNode=5) AND ((hier.level>=mama) OR (Id=TestElement)) )  ) 
              ASK PropagatedGroup TO Add(SELF);              
              IF EFPAtested
                 FOR j:=1 TO EFPAConnectOut  
                    link := ASK root Child("RBDLink", EFPAConnectPath[j]);
                    ASK DownstreamGroup TO Add(link);
                    node := ASK root Child("RBDNode",EFPAConnectTo[j]);
                    ASK node TO TestFail(mama); 
                 END FOR;                
              ELSE
                 FOR j:=1 TO NRBDConnectOut  
                    link := ASK root Child("RBDLink", NRBDConnectPath[j]);
                    ASK DownstreamGroup TO Add(link);
                    node := ASK root Child("RBDNode", NRBDConnectTo[j]);
                    ASK node TO TestFail(mama); 
                 END FOR; {1 to #ds} 
              END IF; 
           ELSIF (typeNode=4)
              ASK PropagatedGroup TO Add(SELF); 
              hier := ASK root Child("RBDHier",parentID);
              node := ASK root Child("RBDNode", hier.outID);
              ASK node TO TestFail(mama);
           ELSIF (typeNode=5)
              {stop here to keep subsystem from propagating}
           END IF;  {Type Not a Stop}  
        END IF; {GoodPaths}         
  END METHOD;   {TestFail}      
  
  ASK METHOD SetNumGoodPaths(IN numGood : INTEGER);
       BEGIN
          NumGoodPaths:=numGood;      
  END METHOD;

  ASK METHOD FindCapPaths(IN  priority                          : INTEGER);
     VAR
        i,j                             : INTEGER;
        link                            : LinkObj;
        node                            : RBDNodeObj;
        tempString                      : STRING;
        EFPAfound                       : BOOLEAN;
     BEGIN
        capTested:=TRUE;
        ASK PropagatedGroup TO Add(SELF);
        FOR i:= 1 TO NRBDConnectOut
            node := ASK root Child("RBDNode",NRBDConnectTo[i]);
            link := ASK root Child("RBDLink", NRBDConnectPath[i]);            
            IF Id=TestElement 
               priority:=link.simCapPriority
            END IF;
            tempString:=node.name;
            EFPAfound:=FALSE;
            IF (((NOT node.fullFlow) OR (NOT node.anyPath)) OR (node.typeNode=3))
               IF (NOT DownstreamGroup.Includes(link)) 
                  ASK DownstreamGroup TO Add(link);
                  ASK link TO SetSimCapPriority(priority);
               END IF;
            ELSE
               IF (NOT node.capTested)
                   ASK node TO FindCapPaths(priority);
               END IF;    
            END IF;
        END FOR;  
   END METHOD;

   ASK METHOD ResetLocalPathArrays;
   BEGIN  
      IF outPathsArray <>NILARRAY
         DISPOSE(outPathsArray);
      END IF;
      IF inPathsArray <>NILARRAY
         DISPOSE(inPathsArray);
      END IF;
   END METHOD;

   ASK METHOD UpdateOutPathsArray(IN newLink        : INTEGER);
   VAR
      oldSize, i, priority    : INTEGER;
      tempArray               : ARRAY INTEGER OF INTEGER;
      link                    : LinkObj;
   BEGIN
      IF outPathsArray = NILARRAY
         oldSize:=0;
         NEW(outPathsArray, 1..1);
         outPathsArray[1]:=newLink;
      ELSE
         oldSize:=HIGH(outPathsArray);
         NEW(tempArray,1..oldSize);
         FOR i:=1 TO oldSize
            tempArray[i]:=outPathsArray[i];          
         END FOR;
         DISPOSE(outPathsArray);
         NEW(outPathsArray,1..oldSize+1);
         FOR i:=1 TO oldSize
            outPathsArray[i]:=tempArray[i];          
         END FOR;
         outPathsArray[oldSize+1]:=newLink;
         DISPOSE(tempArray);
      END IF;
      link:=ASK root Child("RBDLink",newLink); 
      IF anyPath   {box on capacity db is checked}
         priority:=oldSize+1;
      ELSE   {user defined priority}
         priority:=link.capPriority;
      END IF;
      ASK link TO SetSimCapPriority(priority);
   END METHOD;    {UpdateOutPathsArray}

   ASK METHOD UpdateInPathsArray(IN newLink          : INTEGER);
   VAR
      oldSize, i : INTEGER;
      tempArray : ARRAY INTEGER OF INTEGER;
   BEGIN
      IF inPathsArray = NILARRAY
         NEW(inPathsArray, 1..1);
         inPathsArray[1]:=newLink;
      ELSE
         oldSize:=HIGH(inPathsArray);
         NEW(tempArray,1..oldSize);
         FOR i:=1 TO oldSize
            tempArray[i]:=inPathsArray[i];          
         END FOR;
         DISPOSE(inPathsArray);
         NEW(inPathsArray,1..oldSize+1);
         FOR i:=1 TO oldSize
            inPathsArray[i]:=tempArray[i];          
         END FOR;
         inPathsArray[oldSize+1]:=newLink;
         DISPOSE(tempArray);
      END IF;
   END METHOD;   {UpdateInPathsArray}
   
   ASK METHOD GetDownNum(IN   inPath                             : INTEGER; 
                         OUT  downNum                            : INTEGER);
   VAR
      arrayIndex, i : INTEGER;
   BEGIN
      FOR i:=1 TO EconnectIntoNum
         IF inPath = inPathsArray[i]
            arrayIndex:=i;
         END IF;
      END FOR;
      IF arrayIndex=0
         NEW(message,1..1);
         message[1]:="Error in ReconfigureNode!     "; 
         result:=SendAlert(message,FALSE, FALSE, TRUE);
         DISPOSE(message);         
      END IF;
      downNum:=  pathFailValue[arrayIndex]+ pathPMValue[arrayIndex];  
   END METHOD;   {GetDownNum}
    
END OBJECT; {RBDNodeObj}


OBJECT LinkObj;
   ASK METHOD LinkInit      (IN intsArray  : intArray;
                             IN realsArray : realArray);
   BEGIN      
      coldPriority:= intsArray[1];
      capPriority:= intsArray[2];
      nomFlow:= intsArray[3];
      maxFlow:= intsArray[4];
      autoSwitchProb:=realsArray[1];
      autoSwitchTime:=realsArray[2];
      manualSwitchTime:=realsArray[3];
      SetColor(linkWorkColor);
   END METHOD; {Init1}

   ASK METHOD SetID(IN RBDObjName  : STRING;
                    IN RBDObjNum   : INTEGER);
   BEGIN         
      Id := RBDObjNum;
      ReferenceName := RBDObjName;
      SetColor(linkWorkColor);
      IF RBDObjNum >= nextLinkId      {beta145} {link numbering change 12-04}
         nextLinkId := RBDObjNum+1;
      END IF;       
   END METHOD; {SetID}

   ASK METHOD ChangeRoots (IN draggin      : BOOLEAN;
                           IN bRoot        : ImageObj);
   VAR
      i           : INTEGER;
      tempX,tempY : REAL;
   BEGIN
      FOR i := 1 TO 6
         IF draggin
            tempX := Points[i].x;
            tempY := Points[i].y;
            Transform(root,bRoot,Points[i].x,Points[i].y,Points[i].x,Points[i].y);
            tempX := Points[i].x;
            tempY := Points[i].y;
         ELSE
            Transform(bRoot,root,Points[i].x,Points[i].y,Points[i].x,Points[i].y);
         END IF;
      END FOR;
   END METHOD; {ChangeRoots}      

   ASK METHOD SetSelected(IN isSelected : BOOLEAN);
   BEGIN
      Selected := isSelected;
   END METHOD; {SetSelected}

   ASK METHOD SetConnections(IN fromConnect, toConnect : INTEGER;
                             IN fromConRef, toConRef   : STRING);
   BEGIN
      connectFromId := fromConnect;
      connectToId := toConnect;
      connectFRef := fromConRef;
      connectTRef := toConRef;
   END METHOD; {SetConnections}

   ASK METHOD CheckValidLink(IN objectToCheck : ImageObj;
                             OUT isValid      : BOOLEAN;
                             OUT errorMessage : STRING;
                             IN linkDirection : directionType);
   VAR
      block  : RBDBlockObj;
      node   : RBDNodeObj;
      hier   : RBDHierObj;
      event  : RBDEventObj;
   BEGIN
      IF connectFRef = "RBDBlock"
         block := ASK root Child("RBDBlock", connectFromId);
         IF block.connectOutOfNum > 1
            errorMessage := "A block can only connect to one object!     ";
            isValid := FALSE;
            RETURN;
         END IF;
      END IF;
      IF connectFRef = "RBDEvent"
         event := ASK root Child("RBDEvent", connectFromId);
         IF event.connectOutOfNum > 1
            errorMessage := "An event can only connect to one object!     ";
            isValid := FALSE;
            RETURN;
         END IF;
      END IF;
      IF connectFRef = "RBDHier"
         hier := ASK root Child("RBDHier", connectFromId);
         IF hier.connectOutOfNum > 1
            errorMessage := "A hierarchy item can only connect to one object!     ";
            isValid := FALSE;
            RETURN;
         END IF;
      END IF;
      IF OBJTYPENAME(objectToCheck) = "RBDBlockObj"
         block := RBDBlockObj(objectToCheck);
         IF block.connectIntoNum > 1
            isValid := FALSE;
            errorMessage := "Cannot connect more than one link to a block!     ";
         ELSE
            isValid := TRUE;
         END IF;
      ELSIF OBJTYPENAME(objectToCheck) = "RBDEventObj"
         event := RBDEventObj(objectToCheck);
         IF event.connectIntoNum > 1
            isValid := FALSE;
            errorMessage := "Cannot connect more than one link to an event!     ";
         ELSE
            isValid := TRUE;
         END IF;
      ELSIF OBJTYPENAME(objectToCheck) = "RBDHierObj"
         hier := RBDHierObj(objectToCheck);
         IF hier.connectIntoNum > 1
            isValid := FALSE;
            errorMessage := "Cannot connect more than one link to a hierarchy item!     ";
         ELSE
            isValid := TRUE;
         END IF;
      ELSIF OBJTYPENAME(objectToCheck) = "RBDNodeObj"
         node := RBDNodeObj(objectToCheck);
         isValid := TRUE;
        CASE node.typeNode
            WHEN 1: {start node}
               IF linkDirection = INTO
                  errorMessage := "Cannot connect to a Start Marker from any object!     ";
                  isValid := FALSE;
               ELSIF (linkDirection = OUTOF) AND (node.connectOutOfNum > 1)
                  errorMessage := "Cannot have more than one connection from a Start Node!     ";
                  isValid := FALSE;
               END IF;
            WHEN 2:  {connect node}
            WHEN 3:  {end node}
               IF linkDirection = OUTOF
                  errorMessage := "Cannot connect to any object from an End Marker!     ";
                  isValid := FALSE;
               ELSIF (linkDirection = INTO) AND (node.connectIntoNum > 1)
                  errorMessage := "Cannot have more than one connection to an End Node     ";
                  isValid := FALSE;
               END IF;
            WHEN 4: {in node} {eliz}
               IF linkDirection = INTO
                  errorMessage := "Cannot connect to an In Marker from any object!     ";
                  isValid := FALSE;
               ELSIF (linkDirection = OUTOF) AND (node.connectOutOfNum > 1)
                  errorMessage := "Cannot have more than one connection from a Start Node!     ";
                  isValid := FALSE;
               END IF;
            WHEN 5:  {out node}
               IF linkDirection = OUTOF
                  errorMessage := "Cannot connect to any object from an Out Marker!     ";
                  isValid := FALSE;
               ELSIF (linkDirection = INTO) AND (node.connectIntoNum > 1)
                  errorMessage := "Cannot have more than one connection to an End Node     ";
                  isValid := FALSE;
               END IF;
            OTHERWISE
         END CASE;
      END IF;
   END METHOD; {CheckValidLink}
   
   ASK METHOD ChangeLinkStatus  (IN newStatus           : LinkStatusType);
   VAR
      priorStatus : LinkStatusType;
      sibling     : LinkObj;
      node        : RBDNodeObj;
   BEGIN
      priorStatus:=Status;
      Status:=newStatus;
      IF SBlink 
         IF ((Status=LinkStandby) AND (NOT FEVmode))
               SetSwitchTime;
         ELSIF (Status=LinkDone)
            switchStatus:="Failed";
         END IF;
         node:=ASK root Child("RBDNode",connectToId);  
         IF NOT (node.priorityReturn)
            IF  ( (priorStatus=LinkUp) AND 
               ((Status=LinkDown) OR (Status=LinkPM) OR (Status=LinkDone)) )
               ASK node TO ReOrderColds(Id);
            END IF;
         END IF;
      END IF;
      startStateTime:=SimTime;
      IF ((priorStatus<>LinkDone) OR runCompleted)
         IF GraphicsOutput 
            CASE newStatus
               WHEN LinkUp:
                  SetColor(Black);
               WHEN LinkDown:
                  SetColor(Red);  
               WHEN LinkIdle:
                  SetColor(Sienna);   
               WHEN LinkStandby: 
                  SetColor(Blue);
               WHEN LinkPM:
                  SetColor(Orange);
               WHEN LinkDone:
                  SetColor(Red);
               OTHERWISE 
                  NEW(message,1..1);
                  message[1]:="Unknown link status color!     "; 
                  result:=SendAlert(message,FALSE, FALSE, TRUE);
                  DISPOSE(message); 
            END CASE;
            Draw;
         END IF;
         IF hasSiblings
            FOREACH sibling IN  siblingsGroup
              ASK sibling TO ChangeLinkStatus(Status);
            END FOREACH;         
         END IF;
      END IF;
      IF (capacityAnalysis AND (NOT CapAnalysisRequired) )
         IF  ((priorStatus<>LinkUp) AND (newStatus=LinkUp)) OR ((priorStatus=LinkUp) AND (newStatus<>LinkUp))
           CapAnalysisRequired:=TRUE;
         END IF;       
      END IF;
   END METHOD; {ChangeLinkStatus}

   ASK METHOD ShowFlowStatus;      {Not used anymore}
   VAR
      priorStatus : LinkStatusType;
      tempLink,
      sibling     : LinkObj;
   BEGIN
      IF actualFlow>0
         flowing:=TRUE;
         IF actualFlow>simNomFlow
            SetColor(Yellow);
         ELSE
            SetColor(LimeGreen);
         END IF;
         Draw; 
      ELSE   {no flow}          
         IF flowing=TRUE   {was previously flowing}
            flowing:=FALSE;
            IF (Status=LinkUp)   {was up, still up}
               SetColor(Black);
               Draw; 
            END IF;   {IF link is not up, color was already changed}
         END IF;
      END IF; 
   END METHOD;
   
   ASK METHOD SetToInitValues;    {link}
   BEGIN
      startStateTime:=0.0;
      actualFlow:=0;
      IF Status<>LinkUp
         ChangeLinkStatus(LinkUp);
      END IF;
      flowing:=FALSE;
      active:=TRUE;
      switchTime:=0.0;
      switchStatus:="ready";
   END METHOD;

   ASK METHOD SetActualFlow (IN newValue  : INTEGER);
   BEGIN
      actualFlow:=newValue;      
      IF actualFlow<simNomFlow
         nomReached:=FALSE;
         maxReached:=FALSE;
      ELSIF actualFlow<simMaxFlow
         nomReached:=TRUE;
         maxReached:=FALSE;
      ELSE
         nomReached:=TRUE;
         maxReached:=TRUE;
      END IF;
   END METHOD;
   
   ASK METHOD SetPathFull (IN fullStatus  : BOOLEAN);
   BEGIN
      pathFull:=fullStatus;      
   END METHOD;
 
   ASK METHOD SetAvailFlow (IN amount  : INTEGER);
   BEGIN
      availFlow:=amount;      
   END METHOD;
      
   ASK METHOD SetSendAmount(IN newValue : INTEGER);
   BEGIN
      sendAmount:=newValue;      
   END METHOD;

  
   ASK METHOD SetColdVals (IN priority                             : INTEGER;
                           IN autoProb,autoTime,manTime            : REAL);
   BEGIN
      coldPriority:=priority;
      autoSwitchProb:=autoProb;
      autoSwitchTime:=autoTime;
      manualSwitchTime:=manTime;
   END METHOD;   
   
   ASK METHOD AddFamilyMember(IN sibling      : LinkObj);
   BEGIN
      IF hasSiblings=FALSE
         hasSiblings:=TRUE;
         NEW(siblingsGroup);
      END IF;
      ASK siblingsGroup TO Add(sibling);
   END METHOD;

   ASK METHOD SetDepLink (IN downLink: INTEGER);
   BEGIN
      depLink:=downLink;
   END METHOD;

   ASK METHOD SetFlowInVals (IN nom,max                            : INTEGER);
   BEGIN
      nomFlow:=nom;
      maxFlow:=max;
   END METHOD;
   
   ASK METHOD SetSimFlowVals (IN nom,max                           : INTEGER);   
   BEGIN
      simNomFlow:=nom;
      simMaxFlow:=max;
   END METHOD;

   ASK METHOD SetSimCapPriority (IN newSetting                           : INTEGER);   
   BEGIN
      simCapPriority:=newSetting;
   END METHOD;
      
   ASK METHOD SetCapPriority(IN priority                           : INTEGER); 
   BEGIN
      capPriority:=priority;
   END METHOD;

   ASK METHOD CleanUp;
   VAR
      sibling:LinkObj;
   BEGIN
      IF hasSiblings=TRUE
         hasSiblings:=FALSE;
         FOREACH sibling IN  siblingsGroup
           ASK siblingsGroup TO RemoveThis(sibling);
         END FOREACH;         
         DISPOSE(siblingsGroup);
      END IF;
      SBlink:=FALSE;
   END METHOD;
      
   ASK METHOD SetStuckOn    (IN stuckStatus : BOOLEAN);
   BEGIN
       stuckOn:=stuckStatus;
   END METHOD;

   ASK METHOD SetEconnectToId    (IN newValue                     : INTEGER);
   BEGIN
       EconnectToId:=newValue;
   END METHOD;
   
   ASK METHOD SetEconnectFromId  (IN newValue                     : INTEGER);
   BEGIN
       EconnectFromId:=newValue;
   END METHOD;
   ASK METHOD SetEconnectTRef(IN toConRef   : STRING);
   BEGIN
       EconnectTRef:=toConRef;
   END METHOD;
   
   ASK METHOD SetEconnectFRef  (IN fromConRef  : STRING);
   BEGIN
       EconnectFRef:=fromConRef;
   END METHOD;

   ASK METHOD SetActiveLink(IN newVal                           : BOOLEAN);
   BEGIN
       active:=newVal;
   END METHOD;
  
   ASK METHOD SetSwitchTime; 
   VAR
      tempArray  : realArray;
      randDraw   : REAL;
      name       : STRING;
   BEGIN
      IF (autoSwitchProb=1.0)
         switchTime:=autoSwitchTime;
         IF switchTime>0.0
            switchStatus:="auto";
         ELSE
            switchStatus:="ready";
         END IF;
      ELSIF (autoSwitchProb=0.0)
         IF manualSwitchTime=-1.0
            switchTime:=-1.0;
            switchStatus:="none";
         ELSIF manualSwitchTime>0.0  
            switchTime:=manualSwitchTime;
            switchStatus:="manual";
         ELSE
            switchTime:=manualSwitchTime;
            switchStatus:="ready";                 
         END IF;
      ELSE
         NEW(tempArray,1..2);
         tempArray[1]:=0.0;
         tempArray[2]:=1.0;
         name:="link "+INTTOSTR(Id);
         ASK SysStream[11] TO DrawNumber(10,tempArray,name,randDraw);  
         DISPOSE(tempArray);
         IF (autoSwitchProb>randDraw)
            switchTime:=autoSwitchTime;
            IF switchTime>0.0
               switchStatus:="auto";
            ELSE
               switchStatus:="ready";
            END IF;
         ELSE
            IF manualSwitchTime=-1.0
               switchTime:=-1.0;
               switchStatus:="none";
            ELSIF manualSwitchTime>0.0  
               switchTime:=manualSwitchTime;
               switchStatus:="manual";
            ELSE
               switchTime:=manualSwitchTime;
               switchStatus:="ready";                 
            END IF;
         END IF;           
      END IF;
   END METHOD;

   ASK METHOD SetSBlink    (IN newVal   : BOOLEAN);
   BEGIN
       SBlink :=newVal;
   END METHOD;
   
   ASK METHOD PrepForSwitch;
   VAR
      node                                  : RBDNodeObj;
      block                                 : RBDBlockObj;
   BEGIN
      IF switchStatus="manual"
         switchStatus:="manualInProgress"
      ELSIF switchStatus="auto"
         switchStatus:="autoInProgress"
      ELSE
         NEW(message,1..1);
         message[1]:="Error in PerformSwitch!     "; 
         result:=SendAlert(message,FALSE, FALSE, TRUE);
         DISPOSE(message); 
      END IF;
      switchCompleted:=FALSE;
      IF (GraphicsOutput)
         SetColor(Orchid);
         Draw;
      END IF;            
      IF EconnectFRef="RBDBlock" 
         block := ASK root Child("RBDBlock",EconnectFromId);
         ASK block TO SetIgnoreDep(TRUE);      
      ELSIF EconnectFRef="RBDNode"
         node:=ASK root Child("RBDNode", EconnectFromId);
         ASK node TO SetIgnoreDep(TRUE);      
      END IF;                       
   END METHOD;
 
   TELL METHOD PerformSwitch;
   VAR
      node                                  : RBDNodeObj;
      block                                 : RBDBlockObj;
   BEGIN
      WAIT DURATION switchTime
         switchStatus:="ready";
      ON INTERRUPT
         switchStatus:="Interrupted";
         IF GraphicsOutput
            IF (Status=LinkStandby)
               SetColor(Blue);
               Draw;
            END IF;
         END IF;
         SetSwitchTime;
      END WAIT; 
      node:=ASK root Child("RBDNode",connectToId);         
      IF NOT (ColdChangeGroup.Includes(node))      {csb speed change}
         ASK ColdChangeGroup TO Add(node);
      END IF; 
      IF EconnectFRef="RBDBlock" 
         block := ASK root Child("RBDBlock",EconnectFromId);
         ASK block TO SetIgnoreDep(FALSE);      
      ELSIF EconnectFRef="RBDNode"
         node:=ASK root Child("RBDNode", EconnectFromId);
         ASK node TO SetIgnoreDep(FALSE);      
      END IF;                    
      switchCompleted:=TRUE;
      coldsAffected:=TRUE;
      CheckColds;
   END METHOD;

   ASK METHOD SetParentID  (IN newValue                            : INTEGER);{eliz}
   BEGIN
       parentID:=newValue;
   END METHOD;
   
END OBJECT; {LinkObj}


OBJECT RBDBlockObj;

   TELL METHOD InitTimeZeroDeps;   
   BEGIN
       BackFlowDependencies(Down);   
   END METHOD;


   ASK METHOD SetBlockData(INOUT boolsArray : boolArray; INOUT intsArray : intArray;
                           INOUT realsArray : realArray; INOUT strsArray : strArray;
                           INOUT fVals, rVals,preVals,postVals,pmVals : realArray;
                           INOUT sparing : SparingType);
   VAR
      i,numParams: INTEGER;
      blockLabel : TextObj;
      blockImage, innerSquare : ImageObj;
   BEGIN
      infiniteSpares       := boolsArray[1];
      routineSpareOrdering := boolsArray[2];
      stockLevelOrdering   := boolsArray[3];
      emerSpareOrdering    := boolsArray[4];
      alwaysAddDoneCost    := boolsArray[5];
      usesPM               := boolsArray[11];
      pmSpareNeeded        := boolsArray[12];
      pmRefresh            := boolsArray[13];
      pmMisDefer           := boolsArray[14];
      pmFailReset          := boolsArray[15];
      pmTriggered          := boolsArray[16];
      pmReqDefer           := boolsArray[17];
      defDepStateIdle      := boolsArray[18];
      failDistro           := intsArray[1];
      preDist              := intsArray[2];
      repairDistro         := intsArray[3];
      postDist             := intsArray[4];
      initStock            := intsArray[5];
      newSpares            := intsArray[6];
      SLOOrderLevel        := intsArray[7]; 
      SLONewSpares         := intsArray[8]; 
      DependencyNum        := intsArray[9];
      GDType               := intsArray[10];
      failStream           := intsArray[11];
      repairStream         := intsArray[12];  
      simStartType         := intsArray[13];
      numRes1              := intsArray[14]; 
      numDiffRes           := intsArray[15];
      pmDist               := intsArray[16];
      numRes1PM            := intsArray[17];
      arrivalRate          := realsArray[1];
      SLOTime              := realsArray[2];
      emerTime             := realsArray[3];
      sbStress             := realsArray[4];
      pmFixedCost          := realsArray[5];
      initialCost          := realsArray[6];
      operatingCost        := realsArray[7];
      repairingCost        := realsArray[8];
      repFixedCost         := realsArray[9];
      spareCost            := realsArray[10];
      emerShippingCost     := realsArray[11];
      idleCost             := realsArray[12];
      repHoldCost          := realsArray[13];
      standbyCost          := realsArray[14];
      doneCost             := realsArray[15];
      doneFixedCost        := realsArray[16];
      GDRate               := realsArray[17];   
      GDLimit              := realsArray[18];
      amountExhausted      := realsArray[19];
      pmStagger            := realsArray[20];
      pmFreq               := realsArray[21];
      pmCost               := realsArray[22];
      pmHoldCost           := realsArray[23];
      DepNothingPerc       := realsArray[24];
      DepIdlePerc          := realsArray[25];
      DepPMPerc            := realsArray[26];
      DepFailPerc          := realsArray[27]; 
      DepPMThreshold       := realsArray[28];
      name                 := strsArray[1];
      poolName             := strsArray[2];      
      res1Name             := strsArray[3];
      pmTrig               := strsArray[4];
      comment              := strsArray[5];
      depType              := strsArray[6];
      sparingType          := sparing;
      SetPhases(boolsArray[7],phaseValue,phaseType);  
      DISPOSE(failVals);
      DISPOSE(repairVals);
      DISPOSE(preParams);
      DISPOSE(postParams);
      IF failDistro <> 16
         GetNumParams(failDistro,numParams);
      ELSE
         numParams := HIGH(fEmpArray);
      END IF;
      numFailParams:=numParams;
      NEW(failVals, 1..numParams);
      FOR i := 1 TO numParams
         failVals[i] := fVals[i];
      END FOR;
      IF repairDistro <> 16 
         GetNumParams(repairDistro,numParams);
      ELSE
         numParams := HIGH(rEmpArray);
      END IF;
      numRepairParams:=numParams;
      NEW(repairVals, 1..numParams);
      FOR i := 1 TO numParams
         repairVals[i] := rVals[i];
      END FOR;      
      GetNumParams(preDist,numParams);
      NEW(preParams, 1..numParams);
      FOR i := 1 TO numParams
         preParams[i] := preVals[i];
      END FOR;      
      GetNumParams(postDist,numParams);
      NEW(postParams, 1..numParams);
      FOR i := 1 TO numParams
         postParams[i] := postVals[i];
      END FOR;      
      GetNumParams(pmDist,numParams);
      NEW(pmParams, 1..numParams);
      FOR i := 1 TO numParams
         pmParams[i] := pmVals[i];
      END FOR;      
      blockLabel := Child("RBDBlockLabel", 0);
      ASK blockLabel TO SetText(name);
      IF fontSize = 1
         ASK blockLabel TO SetSysFont("SmallFonts",fontSize,70,0);
         ASK blockLabel TO SetHidden(TRUE);
      ELSE
         ASK blockLabel TO SetSysFont("SmallFonts",fontSize,70,0);
      END IF;
      innerSquare:= Descendant("InnerSquare", 0);
      ASK innerSquare TO SetHidden(TRUE);                      
      ASK blockLabel TO Draw;
      SetStats();
      ToolTip := name;
   END METHOD; {SetBlockData}

   ASK METHOD SetStats();
   {Computes the mean and standard deviation, and writes them to the block}
   { Standard deviation calculations added 6-23.  Check these, especially
     the new ones (error, extreme value, laplace) }
   VAR
      i,j,distroInUse                   : INTEGER;
      newMean,sum,x,y,z,z2,newSTDev     : REAL;
      valsInUse                         : realArray;
   BEGIN
      FOR i := 1 TO 2
         IF i = 1
            distroInUse := failDistro;
            valsInUse := CLONE(failVals);
         ELSE
            distroInUse := repairDistro;
            valsInUse := CLONE(repairVals);
         END IF;
         CASE distroInUse
            WHEN 1:  {Beta}
               newMean := valsInUse[1]/(valsInUse[1]+valsInUse[2]);
               newSTDev:= SQRT(valsInUse[1]*valsInUse[2]/
                (POWER(valsInUse[1]+valsInUse[2],2.)*(valsInUse[1]+valsInUse[2]+1.)))
            WHEN 2:  {Chi-square}
               newMean := valsInUse[1];
               newSTDev:=SQRT(2.*valsInUse[1]);
            WHEN 3:  {Binomial}
               newMean := valsInUse[1]*valsInUse[2];
               newSTDev:= SQRT(valsInUse[1]*valsInUse[2]*(1.-valsInUse[2]));
            WHEN 4:  {Exponential}
               newMean := valsInUse[1]+valsInUse[2];
               newSTDev:= valsInUse[1];
            WHEN 5:  {Erlang}
               newMean := valsInUse[1]*valsInUse[2];
               newSTDev:= SQRT(valsInUse[1]*POWER(valsInUse[2],2.));
            WHEN 6:  {Gamma}
               newMean := valsInUse[1]*valsInUse[2]+valsInUse[3];
               newSTDev:= SQRT(valsInUse[1]*POWER(valsInUse[2],2.));
            WHEN 7:  {Lognormal}
               newMean := valsInUse[1];
               newSTDev:= valsInUse[2];
            WHEN 8:  {Normal}
               newMean := valsInUse[1];
               newSTDev:= valsInUse[2];
            WHEN 9:  {Uniform Integer}
               newMean := (valsInUse[1]+valsInUse[2])/2.;
               newSTDev:= SQRT(POWER(valsInUse[2]-valsInUse[1],2.)/12.);
            WHEN 10: {Uniform Real}
               newMean := (valsInUse[1]+valsInUse[2])/2.;
               newSTDev:= SQRT(POWER(valsInUse[2]-valsInUse[1],2.)/12.);
            WHEN 11: {Pearson 5}
               IF valsInUse[1] <= 1.
                  newMean := 12345.6789;   {Murphy identifier for out of range Pearson 5 mean} 
                  newSTDev:= 12345.6789;   {Murphy identifier for out of range Pearson 5 mean}
               ELSE
                  newMean := (valsInUse[2]/(valsInUse[1]-1.))+valsInUse[3];
                  IF valsInUse[1]<=2.    
                    newSTDev:= 12345.6789; {Murphy identifier for out of range Pearson 5 std dev} 
                  ELSE  
                    newSTDev:= valsInUse[2]/(valsInUse[1]-1.)/SQRT(valsInUse[1]-2.);
                  END IF;   
               END IF;
            WHEN 12: {Pearson 6}
               IF valsInUse[2] <= 1.
                  newMean := 12345.6789;   {Murphy identifier for out of range Pearson 6 mean}
                  newSTDev:= 12345.6789;   {Murphy identifier for out of range Pearson 6 std dev}
               ELSE
                  newMean := ((valsInUse[3]*valsInUse[1])/(valsInUse[2]-1.));
                  IF valsInUse[2]<=2.    
                    newSTDev:= 12345.6789;   {Murphy identifier for out of range Pearson 6 std dev} 
                  ELSE  
                    newSTDev:= SQRT((POWER(valsInUse[3],2.)*valsInUse[1]*(valsInUse[1]+valsInUse[2]-1.))
                        /(POWER(valsInUse[2]-1.,2.)*(valsInUse[2]-2.)));
                  END IF;   
               END IF;
            WHEN 13: {Poisson}
               newMean := valsInUse[1];
               newSTDev:= SQRT(valsInUse[1]);
            WHEN 14: {Triangular}
               newMean := (valsInUse[1]+valsInUse[2]+valsInUse[3])/3.;
               newSTDev:= SQRT((POWER(valsInUse[1],2.)+POWER(valsInUse[2],2.)+POWER(valsInUse[3],2.)
                    -valsInUse[1]*valsInUse[2]-valsInUse[1]*valsInUse[3]-valsInUse[2]*valsInUse[3])/18.);
            WHEN 15: {Weibull}
               IF valsInUse[1] < 0.05
                  newMean := 12345.6789;  {Murphy identifier for out of range Weibull mean}
                  newSTDev:= 12345.6789;  {Murphy identifier for out of range Weibull std dev}
               ELSIF valsInUse[1] = 1.   
                  newMean := valsInUse[2] + valsInUse[3];
                  newSTDev:= valsInUse[2];
               ELSIF valsInUse[1] > 10000000.
                  newMean := valsInUse[2] + valsInUse[3];
                  newSTDev:= 0.;
               ELSE
                  x := (1.+(1./valsInUse[1]));
                  z:= GammaFunction(x);
                  newMean := (valsInUse[2]*z)+valsInUse[3];
                  y:= (1.+(2./valsInUse[1]));
                  z2:= GammaFunction(y);
                  newSTDev:= valsInUse[2]*SQRT(z2 - POWER(z,2.));
               END IF;
            WHEN 16: {Empirical}
               IF i = 1
                  FOR j := 1 TO numFailParams
                     sum := sum+failVals[j];
                  END FOR;
                  newMean := sum/FLOAT(numFailParams);
                  sum:=0.;
                  FOR j:=1 TO numFailParams
                    sum:= sum+POWER(failVals[j]-newMean,2.);
                  END FOR;
                  newSTDev:=SQRT(sum/FLOAT(numFailParams));
               ELSE
                  sum := 0.;
                  FOR j := 1 TO numRepairParams
                     sum := sum+repairVals[j];
                  END FOR;
                  newMean := sum/FLOAT(numRepairParams);
                  sum:=0.;
                  FOR j:=1 TO numRepairParams
                    sum:= sum+POWER(repairVals[j]-newMean,2.);
                  END FOR;
                  newSTDev:=SQRT(sum/FLOAT(numRepairParams));
               END IF;
            WHEN 17: {Point Estimate}
               newMean := 12345.6788;  {Murphy identifier for no mean}
               newSTDev:= 12345.6788;  {Murphy identifier for no standard deviation}
            WHEN 18: {[None]}
               newMean := 12345.6788;  {Murphy identifier for no mean}
               newSTDev:= 12345.6788;  {Murphy identifier for no standard deviation}
            WHEN 19: {[Fixed]}
               newMean :=valsInUse[1]; 
               newSTDev:=0.;
            WHEN 20: {Error}
            {Shape is var[1], scale is var[2], and location is var[3]}
               newMean := valsInUse[3];
               x:=3.*valsInUse[1]/2.;
               z:=GammaFunction(x);
               y:=valsInUse[1]/2.;
               z2:=GammaFunction(y);
               newSTDev:=SQRT(POWER(2.,valsInUse[1])*POWER(valsInUse[2],2.)*z/z2);
            WHEN 21: {Extreme Value}
            {Scale is var[1], location is var[2]}
               newMean := valsInUse[2]+.57721*valsInUse[1];
               newSTDev:= SQRT(POWER(valsInUse[1],2.)*POWER(pi,2.)/6.);
            WHEN 22: {Laplace}
            {Scale is var[1], location is var[2]}
               newMean := valsInUse[2];
               newSTDev:= SQRT(2.*POWER(valsInUse[1],2.));
            OTHERWISE
         END CASE;
         IF i = 1
            failMean := newMean;
            failSTDev:=newSTDev;
         ELSE
            repairMean := newMean;
            repairSTDev:=newSTDev;
         END IF;
      END FOR;
   END METHOD; {SetStats}
   
   ASK METHOD SetNewBlockParams(INOUT fVals, rVals : realArray);
   VAR
      i : INTEGER;
   BEGIN
      FOR i := 1 TO numFailParams
         failVals[i] := fVals[i];
      END FOR;
      FOR i := 1 TO numRepairParams
         repairVals[i] := rVals[i];
      END FOR;
   END METHOD; {SetNewBlockParams}


  ASK METHOD ResetForPasting;
  BEGIN
     resetting := TRUE;
     res1Name := "unnamed";
     numRes1 := 1;
     numRes1PM:=1;
     resPoolNum := 0;
     numDiffRes := 0;
     SetPhases(FALSE,NILARRAY,NILARRAY);
     IF poolName <> "unnamed"
        poolName := "unnamed";
        sparePoolNum := 0;
        sparingType := Infinite;
        infiniteSpares := TRUE;
     END IF;
     resetting := FALSE;
     IF pmTriggered
        usesPM:=FALSE;
     END IF;   
     pmTriggered:=FALSE;
     IF (DependencyNum > 0) {If elementally dependent, set to independent}
        DependencyNum := 0;
        depType := "";
     END IF;
   END METHOD;

   ASK METHOD GetCloneOf(IN cloner  : RBDBlockObj;
                         IN copying : BOOLEAN);
   VAR
      i, numParams          : INTEGER;
      blockLabel            : TextObj;
      innerSquare           : ImageObj;
   BEGIN
      infiniteSpares       := cloner.infiniteSpares;
      routineSpareOrdering := cloner.routineSpareOrdering;
      stockLevelOrdering   := cloner.stockLevelOrdering;
      emerSpareOrdering    := cloner.emerSpareOrdering;
      alwaysAddDoneCost    := cloner.alwaysAddDoneCost;
      failDistro           := cloner.failDistro;
      numFailParams        := cloner.numFailParams;
      failStream           := cloner.failStream;
      repairDistro         := cloner.repairDistro;
      numRepairParams      := cloner.numRepairParams;
      repairStream         := cloner.repairStream;     
      initStock            := cloner.initStock;
      newSpares            := cloner.newSpares;
      SLOOrderLevel        := cloner.SLOOrderLevel;
      SLONewSpares         := cloner.SLONewSpares;
      DependencyNum        := cloner.DependencyNum;
      GDType               := cloner.GDType;
      simStartType         := cloner.simStartType;
      numRes1              := cloner.numRes1;      
      arrivalRate          := cloner.arrivalRate;
      emerTime             := cloner.emerTime;
      SLOTime              := cloner.SLOTime;
      preDist              := cloner.preDist;
      postDist             := cloner.postDist; 
      name                 := cloner.name;
      poolName             := cloner.poolName;      
      res1Name             := cloner.res1Name;
      initialCost          := cloner.initialCost;
      repairingCost        := cloner.repairingCost;
      operatingCost        := cloner.operatingCost;
      repFixedCost         := cloner.repFixedCost;
      emerShippingCost     := cloner.emerShippingCost;
      idleCost             := cloner.idleCost;
      repHoldCost          := cloner.repHoldCost;
      standbyCost          := cloner.standbyCost;
      doneCost             := cloner.doneCost;
      doneFixedCost        := cloner.doneFixedCost;
      spareCost            := cloner.spareCost;
      GDRate               := cloner.GDRate;
      GDLimit              := cloner.GDLimit;
      amountExhausted      := cloner.amountExhausted;
      sparingType          := cloner.sparingType;
      usesPM               := cloner.usesPM;
      pmSpareNeeded        := cloner.pmSpareNeeded;
      pmRefresh            := cloner.pmRefresh;
      pmMisDefer           := cloner.pmMisDefer;
      pmFailReset          := cloner.pmFailReset;
      pmTriggered          := cloner.pmTriggered;
      pmDist               := cloner.pmDist;
      pmStagger            := cloner.pmStagger;
      pmFreq               := cloner.pmFreq;
      pmCost               := cloner.pmCost;
      pmFixedCost          := cloner.pmFixedCost;
      pmHoldCost           := cloner.pmHoldCost;
      pmTrig               := cloner.pmTrig;
      numDiffRes           := cloner.numDiffRes;
      numRes1PM            := cloner.numRes1PM;
      pmReqDefer           := cloner.pmReqDefer;
      comment              := cloner.comment;
      DepNothingPerc       := cloner.DepNothingPerc;
      DepIdlePerc          := cloner.DepIdlePerc;
      DepPMPerc            := cloner.DepPMPerc;
      DepFailPerc          := cloner.DepFailPerc;
      DepPMThreshold       := cloner.DepPMThreshold;
      defDepStateIdle      := cloner.defDepStateIdle;  
      depType              := cloner.depType
      SetPhases(cloner.usesPhasing,cloner.phaseValue,cloner.phaseType);
      NEW(failVals,   1..numFailParams);
      NEW(repairVals, 1..numRepairParams);
      FOR i := 1 TO numFailParams
         failVals[i] := cloner.failVals[i];
      END FOR;
      FOR i := 1 TO numRepairParams
         repairVals[i] := cloner.repairVals[i];
      END FOR;
      GetNumParams(cloner.preDist,numParams);
      NEW(preParams, 1..numParams);
      FOR i := 1 TO numParams
         preParams[i] := cloner.preParams[i];
      END FOR;
      GetNumParams(cloner.postDist,numParams);
      NEW(postParams, 1..numParams);
      FOR i := 1 TO numParams
         postParams[i] := cloner.postParams[i];
      END FOR;
      GetNumParams(cloner.pmDist,numParams);
      NEW(pmParams, 1..numParams);
      FOR i := 1 TO numParams
         pmParams[i] := cloner.pmParams[i];
      END FOR;
      IF copying
         blockLabel := Child("RBDBlockLabel", 0);
         ASK blockLabel TO SetText(name);
         IF fontSize = 1
            ASK blockLabel TO SetSysFont("SmallFonts",fontSize,70,0);
            ASK blockLabel TO SetHidden(TRUE);
         ELSE
            ASK blockLabel TO SetSysFont("SmallFonts",fontSize,70,0);
         END IF;
         ASK blockLabel TO Draw;
      END IF;
      innerSquare:= Descendant("InnerSquare", 0);
      ASK innerSquare TO SetHidden(TRUE);
   END METHOD; {GetCloneOf}

   ASK METHOD SetCopyBlockStreams;
   BEGIN
      failStream   := 9;
      repairStream := 9;
   END METHOD; {SetCopyBlockStreams}
   
   ASK METHOD SetConnectToNode(IN isConnected : BOOLEAN);
   BEGIN
      isConnectedNode := isConnected;
   END METHOD; {SetConnectToNode}

   ASK METHOD CopyRestOfData(IN intoNum, outOfNum : INTEGER;
                             IN xPos, yPos        : REAL);
   BEGIN
      xPosition := xPos;
      yPosition := yPos;
      connectIntoNum := intoNum;
      connectOutOfNum := outOfNum;
   END METHOD; {CopyRestOfData}

   ASK METHOD SetPoolNum(IN i : INTEGER;
                         IN setSpare : BOOLEAN);
   BEGIN
      IF setSpare
         sparePoolNum := i;
      ELSE
         resPoolNum := i;
      END IF;
   END METHOD; {SetPoolNum}
   
   ASK METHOD SetSparingInfinite;
   BEGIN
      infiniteSpares := TRUE;
      sparingType    := Infinite;
      poolName       := "unnamed";
   END METHOD; {SetSparingInfinite}

   ASK METHOD SetNoResources;
   BEGIN
      numDiffRes    := 0;
      res1Name   := "unnamed";
      numRes1   := 1;
      numRes1PM := 1;
   END METHOD; {SetNoResources} 

   ASK METHOD SetEFPAvalues(IN EFPAdownNode, EFPApath                  : INTEGER);
   BEGIN
      EFPAnode:=EFPAdownNode; 
      EFPAlink:=EFPApath;
   END METHOD;

   ASK METHOD MassEditBlock(IN  changeSD, changeP, useP, changeRS, changeIS, 
                                changeNR, changeRes, changeName               : BOOLEAN;
                            IN  newRS, sysNum                                 : INTEGER;
                            IN  newDepType, nameChangeType, newName           : STRING;
                            OUT namemsg                                       : BOOLEAN); 
   VAR
      i           : INTEGER;
      tempName    : STRING;
      blockLabel  : TextObj;
   BEGIN   
      IF changeSD AND (DependencyNum <> sysNum)  {change system dependence}
         DependencyNum:=sysNum;
         depType := newDepType;
         somethingChanged := TRUE;
      END IF;
      IF changeP AND (useP <> usesPhasing)
         SetPhases(useP,NILARRAY,NILARRAY);
         somethingChanged := TRUE;
      END IF;
      IF changeRS AND ((failStream <> newRS) OR (repairStream <> newRS)) {change Random Stream}
         failStream:=newRS;
         repairStream:=newRS;
         somethingChanged := TRUE;
      END IF;
      IF changeIS AND (NOT infiniteSpares)  {change Infinite Spares}
         SetSparingInfinite;
      END IF;  
      IF changeNR AND (repairDistro <> 18)  {change repair distribution to [None]}
         repairDistro:=18;
         numRepairParams:=1;
         DISPOSE(repairVals);
         NEW(repairVals, 1..numRepairParams);
         repairVals[1] := 0.0;  {bogus value}
         somethingChanged := TRUE;
      END IF;
      IF changeRes AND (res1Name <> "unnamed")  {change resources to [None]}
         numRes1     := 1;
         numRes1PM   :=1;
         numDiffRes  := 0;
         res1Name    := "unnamed";
         somethingChanged := TRUE;
      END IF;
      IF changeName
         IF nameChangeType = "prefix"
            tempName := newName + name;
         ELSIF nameChangeType = "suffix"
            tempName := name + newName;
         ELSIF nameChangeType = "replace"
            tempName := newName;
         END IF;
         IF STRLEN(tempName) > 20
            namemsg := TRUE;
         ELSE
            name := tempName;
            blockLabel := Child("RBDBlockLabel", 0);
            ASK blockLabel TO SetText(name);
         END IF;
      END IF;
   END METHOD;   {MassEditBlock}

   ASK METHOD SetPhases        (IN phasing                         : BOOLEAN; 
                                IN arrayR                          : realArray;
                                IN arrayS                          : strArray);
   VAR
      i         : INTEGER;
      tempImage : FillVObj;
      tempReals : realArray;
      tempString: strArray;
   BEGIN
      tempImage := Child("BasicBlock",601);
      usesPhasing := phasing;
      IF (activePhases > 0) AND usesPhasing
         DISPOSE(tempReals);
         DISPOSE(tempString);
         IF arrayR <> NILARRAY
            NEW(tempReals,1..HIGH(arrayR));
            NEW(tempString,1..HIGH(arrayR));
            FOR i:=1 TO HIGH(arrayR);
               tempReals[i]:=arrayR[i];
               tempString[i]:=arrayS[i];
            END FOR;  
         END IF;
         IF phaseValue<>NILARRAY {checking to set somethingChanged}
            IF NOT somethingChanged
               IF arrayR <> NILARRAY
                  IF HIGH(arrayR) = HIGH(phaseValue)
                     FOR i := 1 TO HIGH(phaseValue)
                        IF phaseValue[i] <> arrayR[i]
                           somethingChanged := TRUE;
                        END IF;
                        IF phaseType[i] <> arrayS[i]
                           somethingChanged := TRUE;
                        END IF;
                     END FOR;
                  ELSE
                     somethingChanged := TRUE;
                  END IF;
               END IF;
            END IF;
            DISPOSE(phaseValue);
            DISPOSE(phaseType);
         ELSIF arrayR <> NILARRAY
            somethingChanged:=TRUE;
         END IF; {phaseValue<>NILARRAY }
         NEW(phaseValue,1..activePhases);
         NEW(phaseType,1..activePhases);
         IF tempReals <> NILARRAY
            FOR i:=1 TO activePhases
               phaseValue[i]:=tempReals[i];
               phaseType[i]:=tempString[i];
            END FOR;
         ELSIF activePhases > 0
            FOR i:=1 TO activePhases
               phaseValue[i]:=1.00;
               phaseType[i]:="A";
            END FOR;
         END IF;
         ASK tempImage TO SetStyle(NarrowCrosshatchFill);
      ELSE
         ASK tempImage TO SetStyle(SolidFill);
      END IF;   {(activePhases > 0) AND usesPhasing}
      IF (typeOfCursor <> blockC) AND (NOT resetting)
         ASK tempImage TO Draw;
         SetHighlighted(TRUE);
         SetHighlighted(FALSE);
         Draw;
      END IF;
   END METHOD;

   ASK METHOD ReconfigureBlock(IN RunChange,FailChange,IdleChange,StandbyChange,pmChange: INTEGER);  
    VAR
        tempBlock                                 : RBDBlockObj;
        tempNode                                  : RBDNodeObj;
    BEGIN
    IF ((hasDepObjects) AND (simInProgress))
       IF    ((FailChange=1) OR (pmChange=1))  {block just failed or was cut}
                  BackFlowDependencies(Down);
       ELSIF (StandbyChange=1)
                  BackFlowDependencies(NodeStandby);
       ELSIF ((RunChange=1) AND ((FailChange=-1) OR (StandbyChange=-1) OR (pmChange=-1)))
                  BackFlowDependencies(AllUp);
       ELSIF ((IdleChange=1) AND ((FailChange=-1) OR (StandbyChange=-1) OR (pmChange=1)))
                  BackFlowDependencies(AllUp);
       END IF;
    END IF;
    tempNode := ASK window Descendant("RBDNode", EFPAnode);                       
    ASK tempNode TO ReconfigureNode(RunChange,FailChange,IdleChange,StandbyChange,pmChange,EFPAlink);  
   END METHOD;                      
     
  ASK METHOD ReconfigureSystem(IN inPath: INTEGER);
  VAR
     tempnode                      : RBDNodeObj;
  BEGIN
     NumActivePaths:=NumActivePaths-1; 
     tempnode := ASK window Descendant("RBDNode", EFPAnode);              
     ASK tempnode TO ReconfigureSystem(EFPAlink);        
  END METHOD;      {ReconfigureSystem}     
 
  
  TELL METHOD Run;
  VAR
     GDstress,PhaseStress,StandbyStress,GDFactorLim,
     CombinedStress,TimeToMaint                         : REAL;
     failMaint, returnToTop                             : BOOLEAN;
     trig                                               : RapTriggerObj;
     
     randDraw1                                          : REAL;              {startUpFailures   cmc}
     tempArray                                          : realArray;          {startUpFailures   cmc}
     
     
  BEGIN
     blockHasStarted:=TRUE; 
     returnToTop:=TRUE;
     WHILE returnToTop
        returnToTop:=FALSE;
        IF ((opStatus=Idle) AND (FailTimeLeft=0.0))    {keeps random draws in same order as previous versions}
           ZeroWait:=TRUE;
           WAIT FOR BlockTrigger[seqNum] TO Fire;
           ON INTERRUPT
           END WAIT;
           WAIT DURATION 0.0;  {to mimic Raptor6's}
           END WAIT;
           ZeroWait:=FALSE;
        END IF;
     
        IF (FailTimeLeft=0.0)
           GetStream(failStream,"Fail",seqNum,randStream);
           ASK randStream TO DrawNumber(failDistro,failVals,name,FailTimeLeft);
           OperatingTime:=0.0;
        END IF;
        IF GDType>0             {growth/decay}
           GDFactorLim:=GDLimit/failMean;           
           IF GDType=1
              GDstress:=GDRate*FLOAT(FailureCounter)+1.0;
           ELSIF GDType=2
              GDstress:=POWER(GDRate,FLOAT(FailureCounter));
           ELSIF GDType=3
              GDstress:=GDFactorLim+(1.0-GDFactorLim)*
                       EXP(-1.0*GDRate*FLOAT(FailureCounter));
           END IF;
           IF ((GDFactorLim>1.0) AND (GDstress>GDFactorLim))
              GDstress:=GDFactorLim;                    
           ELSIF ((GDFactorLim<1.0) AND (GDstress<GDFactorLim))
           GDstress:=GDFactorLim;
           END IF;
        ELSE
           GDstress:=1.;
        END IF;
        IF ((usesPhasing) AND (activePhases>0))
           PhaseStress:=phaseValue[phaseNumber];
        ELSE
           PhaseStress:=1.0;
        END IF; 
        IF (opStatus=Standby)
           StandbyStress:=sbStress;
           PhaseStress:=1.0;
        ELSE
           StandbyStress:=1.0;
        END IF; 
        CombinedStress:=PhaseStress*StandbyStress/GDstress;
        IF ((GraphicsOutput) AND NOT (opStatus=Idle))
           ChangeRunningColor(CombinedStress);           {sets shades of green and blue}
        END IF; 
        IF ((CombinedStress<=0.0) OR (opStatus=Idle))   {= changed to <=   BINS}
           TimeHack:=SimTime;   {added for BINS - needed to calculate refill of bin}
           ZeroWait:=TRUE;
           WAIT FOR BlockTrigger[seqNum] TO Fire;        
           ON INTERRUPT
           END WAIT; 
           ZeroWait:=FALSE;
           IF (InterruptReason="Obtained_PM_Prereqs")
              DetermineNewState("Obtained PM Prereqs");   
           ELSIF (InterruptReason="Phase_Change")
           
              IF (phaseValue[phaseNumber]>0.0)          
                  IF (startUpFailures AND (initialCost<1.0))             {startUpFailures    CMC    start}
                     NEW(tempArray,1..2);
                     tempArray[1]:=0.0;
                     tempArray[2]:=1.0;
                     ASK SysStream[10] TO DrawNumber(10,tempArray,name,randDraw1);
                     DISPOSE(tempArray);
                     IF (randDraw1>initialCost)
                        FailTimeLeft:=0.000001;
                     END IF;
                 END IF;                                              {startUpFailures    CMC    end}              
              END IF;               
           
              returnToTop:=TRUE;
              {DetermineNewState("Phase Change");}     
           ELSIF (InterruptReason="Start_Deferred_PM")
              DetermineNewState("PM is due");
           ELSIF (InterruptReason="Triggered_PM_Due")
              DetermineNewState("PM is due"); 
           ELSIF (InterruptReason="Standby_Switch_Failed")
              {do nothing}
           ELSIF (InterruptReason="Switched_On")
              {start bin experiment BINS}
              IF (sbStress<0.0)
                 FailTimeLeft:=FailTimeLeft-((SimTime-TimeHack)*sbStress); 
                 IF (FailTimeLeft>failMean)             {failMean OK since mostly unifReal(x,x) is used}
                    FailTimeLeft:=failMean;
                 END IF;
              END IF;
              {end bin experiment BINS}
              
              IF (startUpFailures AND (initialCost<1.0))             {startUpFailures    CMC    start}
                 NEW(tempArray,1..2);
                 tempArray[1]:=0.0;
                 tempArray[2]:=1.0;
                 ASK SysStream[10] TO DrawNumber(10,tempArray,name,randDraw1);
                 DISPOSE(tempArray);
                 IF (randDraw1>initialCost)
                    FailTimeLeft:=0.000001;
                 END IF;
              END IF;                                              {startUpFailures    CMC    end}              
           
              returnToTop:=TRUE;
              {DetermineNewState("Switched On");} {was being sent right back to RUN}
           ELSIF  (InterruptReason="Switched_Off")
              returnToTop:=TRUE;
              {DetermineNewState("Switched Off");} 
           ELSIF (SUBSTR(1,10,InterruptReason)="Dependence")
                      
              IF ((InterruptReason="Dependence_SB_turnOn") AND startUpFailures AND (initialCost<1.0))             {startUpFailures    CMC    start}
                 NEW(tempArray,1..2);
                 tempArray[1]:=0.0;
                 tempArray[2]:=1.0;
                 ASK SysStream[10] TO DrawNumber(10,tempArray,name,randDraw1);
                 DISPOSE(tempArray);
                 IF (randDraw1>initialCost)
                    FailTimeLeft:=0.000001;
                 END IF;
              END IF;                                              {startUpFailures    CMC    end}              
                                 
              returnToTop:=TRUE;
           ELSIF (InterruptReason="OBPM")
              IF (OperatingTime >  (DepPMThreshold*failMean/100.0))
                 returnFromMaint:=TRUE;
                 DetermineNewState("PM is due");
              ELSE
                 returnFromMaint:=FALSE;
                 IF (defDepStateIdle)
                    ChangeBlockState(Idle,activeStatus,"Dependence");
                 END IF;
                 returnToTop:=TRUE;
              END IF;
           ELSIF (InterruptReason="Induced_Failure")
              CompTimeToFail:=SimTime-TimeOfCompFailure; 
              TimeOfCompFailure:=SimTime; 
              IF pmFailReset
                 PMTimeLeft:=0.0;
              ELSE           
                 PMTimeLeft:=PMTimeLeft-(SimTime-TimeHack); 
              END IF;
              FailTimeLeft:=0.0;
              INC(FailureCounter);
              DetermineNewState("Block Has Failed");  
           ELSE
              NEW(message,1..1);
              message[1]:="Error in Run Interrupt!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message); 
           END IF;           
           
        ELSE    {accumulating life}
           IF ((EventsFile) AND (CombinedStress<>1.0))
              WriteEvents(SimTime,"Block",name,"StessLevel","CombStress="+REALTOSTR(CombinedStress));
           END IF;     
           FailTimeLeft:=FailTimeLeft/CombinedStress;
           IF ((usesPM) AND (NOT pmTriggered))
              IF ((NOT deferPM) AND (NOT WaitingForPrereqs))
                 IF (PMTimeLeft<=0.0)
                    PMTimeLeft:=pmFreq;
                 END IF;
              END IF;   
           END IF;
           TimeHack:=SimTime;
           TimeToMaint:=FailTimeLeft;
           failMaint:=TRUE;
           IF (usesPM AND (NOT pmTriggered))
              IF ((PMTimeLeft<FailTimeLeft) AND (NOT skipPM) AND (NOT deferPM) AND (NOT WaitingForPrereqs))
                 TimeToMaint:=PMTimeLeft;
                 failMaint:=FALSE;
              END IF;
           END IF;
           FailWait:=TRUE;
           WAIT DURATION TimeToMaint; 
              FailWait:=FALSE;           
              OperatingTime:=SimTime-TimeHack+OperatingTime;
              IF (failMaint)
                 CompTimeToFail:=SimTime-TimeOfCompFailure; 
                 TimeOfCompFailure:=SimTime; 
                 IF pmFailReset
                    PMTimeLeft:=0.0;
                 ELSE           
                    PMTimeLeft:=PMTimeLeft-(SimTime-TimeHack); 
                 END IF;
                 FailTimeLeft:=0.0;
                 INC(FailureCounter);
                 IF ((comment="maintDefer") AND (deferTrig<>""))   {avant garde tab feature}
                    IF EventsFile
                       WriteEvents(SimTime,"Block",name,"Failed-deferring","Operating_time="+REALTOSTR(OperatingTime));
                    END IF; 
                    ChangeBlockState(RepHold,activeStatus,"deferred maintenance");                    
                    FOREACH trig IN triggerGroup
                       IF trig.TrigName = deferTrig                       
                          IF NOT ((deferTrig="phaseTrigger") AND (System.missionStatus=NonMission))
                             WAIT FOR trig TO Fire;
                             END WAIT;
                          END IF;   {block set to mission defer are not held if non-mission}                        
                       END IF;
                    END FOREACH;                         
                 END IF;
                 DetermineNewState("Block Has Failed");  
              ELSE   {PM is due}
                 IF ((activePhases>0) AND (pmMisDefer))
                    WAIT DURATION 0.0;
                    END WAIT;
                    IF PhaseChangeInProgress
                       WAIT FOR pmSyncTrigger TO Fire;
                       END WAIT;
                    END IF;
                 END IF;            
                 FailTimeLeft:=(FailTimeLeft-(SimTime-TimeHack))*CombinedStress;
                 InterruptReason:="Scheduled_PM";   {for detailed events log}
                 DetermineNewState("PM is due");   
              END IF;
           ON INTERRUPT        
              FailWait:=FALSE;
              PMTimeLeft:=PMTimeLeft-(SimTime-TimeHack); 
              FailTimeLeft:=(FailTimeLeft-(SimTime-TimeHack))*CombinedStress;
              OperatingTime:=SimTime-TimeHack+OperatingTime;
              IF (InterruptReason="Obtained_PM_Prereqs")
                 DetermineNewState("Obtained PM Prereqs");   
              ELSIF (InterruptReason="Phase_Change")
                 returnToTop:=TRUE;
                 {DetermineNewState("Phase Change");}  
              ELSIF (InterruptReason="Start_Deferred_PM")
                 DetermineNewState("PM is due");
              ELSIF (InterruptReason="Triggered_PM_Due")
                 DetermineNewState("PM is due"); 
              ELSIF (InterruptReason="Standby_Switch_Failed")
                 {do nothing}
              ELSIF (InterruptReason="Switched_On")
                 returnToTop:=TRUE;
                 {DetermineNewState("Switched On");} 
              ELSIF  (InterruptReason="Switched_Off")
                 returnToTop:=TRUE;
                 {DetermineNewState("Switched Off");} 
              ELSIF (SUBSTR(1,10,InterruptReason)="Dependence")
                 returnToTop:=TRUE;
              ELSIF (InterruptReason="OBPM")
                 IF (OperatingTime >  (DepPMThreshold*failMean/100.0))
                    returnFromMaint:=TRUE;
                    DetermineNewState("PM is due");
                 ELSE
                    returnFromMaint:=FALSE;
                    IF (defDepStateIdle)
                       ChangeBlockState(Idle,activeStatus,"Dependence");
                    END IF;
                    returnToTop:=TRUE;
                 END IF;
              ELSIF (InterruptReason="Induced_Failure")
                 CompTimeToFail:=SimTime-TimeOfCompFailure; 
                 TimeOfCompFailure:=SimTime; 
                 IF pmFailReset
                    PMTimeLeft:=0.0;
                 ELSE           
                    PMTimeLeft:=PMTimeLeft-(SimTime-TimeHack); 
                 END IF;
                 FailTimeLeft:=0.0;
                 INC(FailureCounter);
                 DetermineNewState("Block Has Failed");  
              ELSE
                 NEW(message,1..1);
                 message[1]:="Error in Run Interrupt!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                 DISPOSE(message); 
              END IF;
           END WAIT;
        END IF;    
        InterruptReason:="";
     END WHILE;     {returnToTop}
  END METHOD;        {Run}
  
  TELL METHOD Repair; 
  VAR
     WaitingForSpare,WaitingForResource,Bool1,Bool2          : BOOLEAN;
     LDTwait,TimeToRepair                                    : REAL;
     oldStatus                                               : BlockStatusType;
     poolSLO                                                 : INTEGER;
     commentString                                           : STRING;
  BEGIN
     IF ((sparingType=SparePool) AND (NOT spareObtained))
        IF (PooledSpares[sparePoolNum].Resources>0)       
           WAIT FOR PooledSpares[sparePoolNum] TO Give(SELF,1);
              PoolResAvailable[sparePoolNum]:=PooledSpares[sparePoolNum].Resources;
           END WAIT;
           spareObtained:=TRUE;
           INC(SparesUsed[seqNum]);  
           IF (PoolArray[sparePoolNum].stockLevelOrdering)
              poolSLO:=0;
              IF (PooledSpares[sparePoolNum].Resources=PoolArray[sparePoolNum].SLOOrderLevel) 
                 poolSLO:= PoolArray[sparePoolNum].SLONewSpares; 
              ELSIF ( PooledSpares[sparePoolNum].Resources<=(PoolArray[sparePoolNum].SLOOrderLevel-PoolArray[sparePoolNum].SLONewSpares) ) 
                 poolSLO:= 1;
              END IF;
           END IF; 
           IF emerSpareOrdered
              ASK PooledSpares[sparePoolNum] TO DecNumEmerOrdered;
           END IF;
        END IF;    
     END IF;
                
     CASE startCond
        WHEN "Off":
           IF (  NOT( (preDist=19) AND (preParams[1]=0.0) )  )    {pre-LDT not fixed[0]}
              IF opStatus<>RepHold  
                 ChangeBlockState(RepHold,activeStatus,"LDT");
              END IF;
              IF coldsAffected   {csb speed change}
                 CheckColds;
              END IF;
              GetStream(repairStream,"Repair",seqNum,randStream);
              ASK randStream TO DrawNumber(preDist,preParams,name,LDTwait);
              WAIT DURATION LDTwait;  
              END WAIT;
              IF EventsFile
                 WriteEvents(SimTime,"Block",name,"End_Pre-LDT","Time="+REALTOSTR(LDTwait));
              END IF;
           END IF;
           IF (NOT spareObtained)
              CASE sparingType
                 WHEN Infinite:
                    IF costAnalysis
                       BlockSpCost[seqNum] :=  BlockSpCost[seqNum] + spareCost;
                    END IF;
                    INC(SparesUsed[seqNum]);
                 WHEN Custom:     
                    IF (CustomSpares[seqNum].Resources=0)                   
                       WaitingForSpare:=TRUE;
                       INC(currNumWaits[seqNum]);
                       IF opStatus<>RepHold
                          ChangeBlockState(RepHold,activeStatus,"Waiting_for_Spare");
                          IF coldsAffected   {csb speed change}
                             CheckColds;
                          END IF;
                       END IF;
                    END IF;
                    WAIT FOR CustomSpares[seqNum] TO Give(SELF,1);  
                    END WAIT;
                    INC(SparesUsed[seqNum]);
                    IF stockLevelOrdering
                       IF (CustomSpares[seqNum].Resources=SLOOrderLevel) 
                          OrderStock(SLONewSpares);
                       ELSIF ( CustomSpares[seqNum].Resources<=(SLOOrderLevel-SLONewSpares) )
                          OrderStock(1);  
                       END IF;
                    END IF;
                    IF WaitingForSpare
                       WaitingForSpare:=FALSE;
                       IF EventsFile
                          WriteEvents(SimTime,"Block",name,"Sparing","Spare_Arrived");
                       END IF;
                    END IF;
                    SparesAvailable[seqNum]:=CustomSpares[seqNum].Resources;
                 WHEN SparePool:
                    IF (PooledSpares[sparePoolNum].Resources=0)                   
                       WaitingForSpare:=TRUE;
                       INC(currNumWaits[sparePoolNum+totalBlocks]);
                       IF opStatus<>RepHold
                          ChangeBlockState(RepHold,activeStatus,"Waiting_for_Spare");
                          IF coldsAffected   {csb speed change}
                             CheckColds;
                          END IF;
                       END IF;
                    END IF;
                    WAIT FOR PooledSpares[sparePoolNum] TO Give(SELF,1);
                       PoolResAvailable[sparePoolNum]:=PooledSpares[sparePoolNum].Resources;
                    END WAIT;
                    IF emerSpareOrdered
                       ASK PooledSpares[sparePoolNum] TO DecNumEmerOrdered;
                    END IF;
                    INC(SparesUsed[seqNum]);
                    IF WaitingForSpare
                       WaitingForSpare:=FALSE;
                       IF EventsFile
                          WriteEvents(SimTime,"Block",name,"Sparing","Spare_Arrived");
                       END IF;
                    END IF;
                    IF (PoolArray[sparePoolNum].stockLevelOrdering)
                       IF (PooledSpares[sparePoolNum].Resources=PoolArray[sparePoolNum].SLOOrderLevel) 
                          TELL PooledSpares[sparePoolNum] TO OrderStock(sparePoolNum,PoolArray[sparePoolNum].SLONewSpares );    
                       ELSIF ( PooledSpares[sparePoolNum].Resources<=(PoolArray[sparePoolNum].SLOOrderLevel-PoolArray[sparePoolNum].SLONewSpares) ) 
                          TELL PooledSpares[sparePoolNum] TO OrderStock(sparePoolNum,1); 
                       END IF;
                    END IF; 
                 WHEN None:      {remnant}           
              END CASE;
           ELSE  {spare already obtained}
              IF ((sparingType=SparePool) AND (poolSLO>0))
                 TELL PooledSpares[sparePoolNum] TO OrderStock(sparePoolNum,poolSLO); 
              END IF;   
           END IF; {NOT spareObtained}
           IF numDiffRes=1 
              IF ((PooledSpares[resPoolNum].Resources<numRes1) OR (PooledSpares[resPoolNum].ReportNumberPending>0)) 
                 INC(currNumWaits[resPoolNum+totalBlocks]);
                 IF opStatus<>RepHold
                    ChangeBlockState(RepHold,activeStatus,"Waiting_for_resource(s)");
                    IF coldsAffected   {csb speed change}
                       CheckColds;
                    END IF;
                 END IF;
                 WaitingForResource:=TRUE;
              END IF;
              WAIT FOR PooledSpares[resPoolNum] TO Give(SELF,numRes1); 
              END WAIT; 
              WAIT DURATION 0.0;  {to get X to jive with previous Raptors}
              END WAIT;
              IF costAnalysis
                 PoolESCost[resPoolNum]:=PoolESCost[resPoolNum]+PoolArray[resPoolNum].fixedPerUse*FLOAT(numRes1); 
              END IF;
              PoolResAvailable[resPoolNum]:=PooledSpares[resPoolNum].Resources;
              IF WaitingForResource
                 WaitingForResource:=FALSE;
                 IF EventsFile
                    WriteEvents(SimTime,"Block",name,"Sparing","Obtained_"+INTTOSTR(numRes1)+"_Resource(s)");
                 END IF;
              END IF;
              commentString:=INTTOSTR(numRes1)+PoolArray[resPoolNum].poolName;  {see ChangeBlockState}
           ELSE
              commentString:="No_resources_used";                                 
           END IF; 
           GetStream(repairStream,"Repair",seqNum,randStream);
           ASK randStream TO DrawNumber(repairDistro,repairVals,name,TimeToRepair);
           oldStatus:=opStatus;   
           ChangeBlockState(Repairing,activeStatus,commentString);
           IF ((oldStatus<>RepHold) AND coldsAffected)   {csb speed change}
              CheckColds;
           END IF;   
           RepWait:=TRUE;
           WAIT DURATION TimeToRepair;
              ON INTERRUPT
              NEW(message,1..1);
              message[1]:="Error - Repair Interrupted!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message);
           END WAIT; 
           RepWait:=FALSE;
           IF numDiffRes=1                                 
              ASK PooledSpares[resPoolNum] TO TakeBack(SELF,numRes1);  
              PoolResAvailable[resPoolNum]:=PooledSpares[resPoolNum].Resources; 
           END IF; 
           CompTimeToRepair:=TimeToRepair;
           IF EventsFile
              WriteEvents(SimTime,"Block",name,"Repaired","MaintTime="+REALTOSTR(TimeToRepair));
           END IF;
           IF NOT((postDist=19) AND (postParams[1]=0.0))   {have postLDT}              
              ChangeBlockState(RepHold,activeStatus,"postLDT"); 
              GetStream(repairStream,"Repair",seqNum,randStream);
              ASK randStream TO DrawNumber(postDist,postParams,name,LDTwait);
              WAIT DURATION LDTwait;  
              END WAIT;
              IF EventsFile
                 WriteEvents(SimTime,"Block",name,"End_Post-LDT","Time="+REALTOSTR(LDTwait));
              END IF;
           END IF;
      
        WHEN "preLDT":
           CheckForSpare(Bool1,Bool2);
           IF Bool2    {out of spares}
               ChangeBlockState(Done,activeStatus,"No_spares");
               IF AllowReconfigureSystem                               
                  ReconfigureSystem(0);             
               END IF;
               RETURN;
           END IF;
           WAIT DURATION firstPreLDT;  
           END WAIT;
           IF EventsFile
              WriteEvents(SimTime,"Block",name,"End_Partial_pre-LDT","Time="+REALTOSTR(startVal));
           END IF;
           IF NOT spareObtained
              CASE sparingType
                 WHEN Infinite:
                    IF costAnalysis
                       BlockSpCost[seqNum] :=  BlockSpCost[seqNum] + spareCost;
                    END IF;
                    INC(SparesUsed[seqNum]);
                 WHEN Custom:     
                    IF (CustomSpares[seqNum].Resources=0)                   
                       WaitingForSpare:=TRUE;
                       INC(currNumWaits[seqNum]);
                       IF opStatus<>RepHold
                          ChangeBlockState(RepHold,activeStatus,"Waiting_for_Spare");
                          IF coldsAffected   {csb speed change}
                             CheckColds;
                          END IF;
                       END IF;
                    END IF;
                    WAIT FOR CustomSpares[seqNum] TO Give(SELF,1);  
                    END WAIT;
                    INC(SparesUsed[seqNum]);
                    IF stockLevelOrdering
                       IF (CustomSpares[seqNum].Resources=SLOOrderLevel) 
                          OrderStock(SLONewSpares);
                       ELSIF ( CustomSpares[seqNum].Resources<=(SLOOrderLevel-SLONewSpares) )
                          OrderStock(1);  
                       END IF;
                    END IF;
                    IF WaitingForSpare
                       WaitingForSpare:=FALSE;
                       IF EventsFile
                          WriteEvents(SimTime,"Block",name,"Sparing","Spare_Arrived");
                       END IF;
                    END IF;
                    SparesAvailable[seqNum]:=CustomSpares[seqNum].Resources;
                 WHEN SparePool:
                    IF (PooledSpares[sparePoolNum].Resources=0)                   
                       WaitingForSpare:=TRUE;
                       INC(currNumWaits[sparePoolNum+totalBlocks]);
                       IF opStatus<>RepHold
                          ChangeBlockState(RepHold,activeStatus,"Waiting_for_Spare");
                          IF coldsAffected   {csb speed change}
                             CheckColds;
                          END IF;
                       END IF;
                    END IF;
                    WAIT FOR PooledSpares[sparePoolNum] TO Give(SELF,1);
                       PoolResAvailable[sparePoolNum]:=PooledSpares[sparePoolNum].Resources;
                    END WAIT;
                    IF emerSpareOrdered
                       ASK PooledSpares[sparePoolNum] TO DecNumEmerOrdered;
                    END IF;
                    INC(SparesUsed[seqNum]);
                    IF WaitingForSpare
                       WaitingForSpare:=FALSE;
                       IF EventsFile
                          WriteEvents(SimTime,"Block",name,"Sparing","Spare_Arrived");
                       END IF;
                    END IF;
                    IF (PoolArray[sparePoolNum].stockLevelOrdering)
                       IF (PooledSpares[sparePoolNum].Resources=PoolArray[sparePoolNum].SLOOrderLevel) 
                          TELL PooledSpares[sparePoolNum] TO OrderStock(sparePoolNum,PoolArray[sparePoolNum].SLONewSpares );    
                       ELSIF ( PooledSpares[sparePoolNum].Resources<=(PoolArray[sparePoolNum].SLOOrderLevel-PoolArray[sparePoolNum].SLONewSpares) ) 
                          TELL PooledSpares[sparePoolNum] TO OrderStock(sparePoolNum,1); 
                       END IF;
                    END IF; 
                 WHEN None:                 
              END CASE;
           ELSE  {spare already obtained}
              IF ((sparingType=SparePool) AND (poolSLO>0))
                 TELL PooledSpares[sparePoolNum] TO OrderStock(sparePoolNum,poolSLO); 
              END IF;   
           END IF; {NOT spareObtained}
           IF numDiffRes=1 
              IF ((PooledSpares[resPoolNum].Resources<numRes1) OR (PooledSpares[resPoolNum].ReportNumberPending>0)) 
                 INC(currNumWaits[resPoolNum+totalBlocks]);
                 IF opStatus<>RepHold
                    ChangeBlockState(RepHold,activeStatus,"Waiting_for_resource(s)");
                    IF coldsAffected   {csb speed change}
                       CheckColds;
                    END IF;
                 END IF;
                 WaitingForResource:=TRUE;
              END IF;
              WAIT FOR PooledSpares[resPoolNum] TO Give(SELF,numRes1); 
              END WAIT; 
              WAIT DURATION 0.0;  {to get X to jive with previous Raptors}
              END WAIT;
              IF costAnalysis
                 PoolESCost[resPoolNum]:=PoolESCost[resPoolNum]+
                 PoolArray[resPoolNum].fixedPerUse*
                 FLOAT(numRes1); 
              END IF;
              PoolResAvailable[resPoolNum]:=PooledSpares[resPoolNum].Resources;
              IF WaitingForResource
                 WaitingForResource:=FALSE;
                 IF EventsFile
                    WriteEvents(SimTime,"Block",name,"Sparing","Obtained_"+INTTOSTR(numRes1)+"_Resource(s)");
                 END IF;
              END IF;
              commentString:=INTTOSTR(numRes1)+PoolArray[resPoolNum].poolName;  {see ChangeBlockState}
           ELSE
              commentString:="No_resources_used";                                 
           END IF; 
           TimeToRepair:=startVal;
           oldStatus:=opStatus;   
           ChangeBlockState(Repairing,activeStatus,commentString);
           IF ((oldStatus<>RepHold) AND coldsAffected)   {csb speed change}
              CheckColds;
           END IF;   
           RepWait:=TRUE;
           WAIT DURATION TimeToRepair;
              ON INTERRUPT
              NEW(message,1..1);
              message[1]:="Error - Repair Interrupted!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message);
           END WAIT; 
           RepWait:=FALSE;
           IF numDiffRes=1                                 
              ASK PooledSpares[resPoolNum] TO TakeBack(SELF,numRes1);  
              PoolResAvailable[resPoolNum]:=PooledSpares[resPoolNum].Resources; 
           END IF; 
           CompTimeToRepair:=TimeToRepair;
           IF EventsFile
              WriteEvents(SimTime,"Block",name,"Repaired","MaintTime="+REALTOSTR(TimeToRepair));
           END IF;
           IF NOT((postDist=19) AND (postParams[1]=0.0))                 
              ChangeBlockState(RepHold,activeStatus,"postLDT"); 
              GetStream(repairStream,"Repair",seqNum,randStream);
              ASK randStream TO DrawNumber(postDist,postParams,name,LDTwait);
              WAIT DURATION LDTwait;  
              END WAIT;
              IF EventsFile
                 WriteEvents(SimTime,"Block",name,"End_Post-LDT","Time="+REALTOSTR(LDTwait));
              END IF;
           END IF;
 
        WHEN "repairing":
           CheckForSpare(Bool1,Bool2);
           IF Bool2
               ChangeBlockState(Done,activeStatus,"No_spares");
               IF AllowReconfigureSystem                               
                  ReconfigureSystem(0);             
               END IF;
               RETURN;
           END IF;
           IF NOT spareObtained
              CASE sparingType
                 WHEN Infinite:
                    IF costAnalysis
                       BlockSpCost[seqNum] :=  BlockSpCost[seqNum] + spareCost;
                    END IF;
                    INC(SparesUsed[seqNum]);
                 WHEN Custom:     
                    IF (CustomSpares[seqNum].Resources=0) 
                       IF ((stockLevelOrdering) AND (SLOOrderLevel=0))  {to take care of special case}
                          OrderStock(SLONewSpares);
                       END IF;
                       WaitingForSpare:=TRUE;
                       INC(currNumWaits[seqNum]);
                       IF opStatus<>RepHold
                          ChangeBlockState(RepHold,activeStatus,"Waiting_for_Spare");
                          IF coldsAffected   {csb speed change}
                             CheckColds;
                          END IF;
                       END IF;
                    END IF;
                    WAIT FOR CustomSpares[seqNum] TO Give(SELF,1);  
                    END WAIT;
                    INC(SparesUsed[seqNum]);
                    IF stockLevelOrdering
                       IF (CustomSpares[seqNum].Resources=SLOOrderLevel) 
                          OrderStock(SLONewSpares);
                       ELSIF ( CustomSpares[seqNum].Resources<=(SLOOrderLevel-SLONewSpares) )
                          OrderStock(1);  
                       END IF;
                    END IF;
                    IF WaitingForSpare
                       WaitingForSpare:=FALSE;
                       IF EventsFile
                          WriteEvents(SimTime,"Block",name,"Sparing","Spare_Arrived");
                       END IF;
                    END IF;
                    SparesAvailable[seqNum]:=CustomSpares[seqNum].Resources;
                 WHEN SparePool:
                    IF (PooledSpares[sparePoolNum].Resources=0)  
                       IF ((PoolArray[sparePoolNum].stockLevelOrdering) AND (PoolArray[sparePoolNum].SLOOrderLevel=0))
                          TELL PooledSpares[sparePoolNum] TO OrderStock(sparePoolNum,PoolArray[sparePoolNum].SLONewSpares );    
                       END IF; 
                       WaitingForSpare:=TRUE;
                       INC(currNumWaits[sparePoolNum+totalBlocks]);
                       IF opStatus<>RepHold
                          ChangeBlockState(RepHold,activeStatus,"Waiting_for_Spare");
                          IF coldsAffected   {csb speed change}
                             CheckColds;
                          END IF;
                       END IF;
                    END IF;
                    WAIT FOR PooledSpares[sparePoolNum] TO Give(SELF,1);
                       PoolResAvailable[sparePoolNum]:=PooledSpares[sparePoolNum].Resources;
                    END WAIT;
                    IF emerSpareOrdered
                       ASK PooledSpares[sparePoolNum] TO DecNumEmerOrdered;
                    END IF;
                    INC(SparesUsed[seqNum]);
                    IF WaitingForSpare
                       WaitingForSpare:=FALSE;
                       IF EventsFile
                          WriteEvents(SimTime,"Block",name,"Sparing","Spare_Arrived");
                       END IF;
                    END IF;
                    IF (PoolArray[sparePoolNum].stockLevelOrdering)
                       IF (PooledSpares[sparePoolNum].Resources=PoolArray[sparePoolNum].SLOOrderLevel) 
                          TELL PooledSpares[sparePoolNum] TO OrderStock(sparePoolNum,PoolArray[sparePoolNum].SLONewSpares );    
                       ELSIF ( PooledSpares[sparePoolNum].Resources<=(PoolArray[sparePoolNum].SLOOrderLevel-PoolArray[sparePoolNum].SLONewSpares) ) 
                          TELL PooledSpares[sparePoolNum] TO OrderStock(sparePoolNum,1); 
                       END IF;
                    END IF; 
                 WHEN None:                 
              END CASE;
           ELSE  {spare already obtained}
              IF ((sparingType=SparePool) AND (poolSLO>0))
                 TELL PooledSpares[sparePoolNum] TO OrderStock(sparePoolNum,poolSLO); 
              END IF;   
           END IF; {NOT spareObtained}
           IF numDiffRes=1 
              IF ((PooledSpares[resPoolNum].Resources<numRes1) OR (PooledSpares[resPoolNum].ReportNumberPending>0)) 
                 INC(currNumWaits[resPoolNum+totalBlocks]);
                 IF opStatus<>RepHold
                    ChangeBlockState(RepHold,activeStatus,"Waiting_for_resource(s)");
                    IF coldsAffected   {csb speed change}
                       CheckColds;
                    END IF;
                 END IF;
                 WaitingForResource:=TRUE;
              END IF;
              WAIT FOR PooledSpares[resPoolNum] TO Give(SELF,numRes1); 
              END WAIT; 
              WAIT DURATION 0.0;  {to get X to jive with previous Raptors}
              END WAIT;
              IF costAnalysis
                 PoolESCost[resPoolNum]:=PoolESCost[resPoolNum]+
                 PoolArray[resPoolNum].fixedPerUse*FLOAT(numRes1); 
              END IF;
              PoolResAvailable[resPoolNum]:=PooledSpares[resPoolNum].Resources;
              IF WaitingForResource
                 WaitingForResource:=FALSE;
                 IF EventsFile
                    WriteEvents(SimTime,"Block",name,"Sparing","Obtained_"+INTTOSTR(numRes1)+"_Resource(s)");
                 END IF;
              END IF;
              commentString:=INTTOSTR(numRes1)+PoolArray[resPoolNum].poolName;  {see ChangeBlockState}
           ELSE
              commentString:="No_resources_used";                                 
           END IF; 
           TimeToRepair:=startVal;
           oldStatus:=opStatus;   
           ChangeBlockState(Repairing,activeStatus,commentString);    {tony10-04}
           IF ((oldStatus<>RepHold) AND coldsAffected)   {csb speed change}
              CheckColds;
           END IF;   
           RepWait:=TRUE;
           WAIT DURATION TimeToRepair;
              ON INTERRUPT
              NEW(message,1..1);
              message[1]:="Error - Repair Interrupted!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message);
           END WAIT; 
           RepWait:=FALSE;
           IF numDiffRes=1                                 
              ASK PooledSpares[resPoolNum] TO TakeBack(SELF,numRes1);  
              PoolResAvailable[resPoolNum]:=PooledSpares[resPoolNum].Resources; 
           END IF; 
           CompTimeToRepair:=TimeToRepair;
           IF EventsFile
              WriteEvents(SimTime,"Block",name,"Repaired","MaintTime="+REALTOSTR(TimeToRepair));
           END IF;
           IF NOT((postDist=19) AND (postParams[1]=0.0))                 
              ChangeBlockState(RepHold,activeStatus,"postLDT"); 
              GetStream(repairStream,"Repair",seqNum,randStream);
              ASK randStream TO DrawNumber(postDist,postParams,name,LDTwait);
              WAIT DURATION LDTwait;  
              END WAIT;
              IF EventsFile
                 WriteEvents(SimTime,"Block",name,"End_Post-LDT","Time="+REALTOSTR(LDTwait));
              END IF;
           END IF;
        
        WHEN "postLDT":
           WAIT DURATION startVal;  
           END WAIT;
           IF EventsFile
              WriteEvents(SimTime,"Block",name,"End_Partial_post-LDT","Time="+REALTOSTR(startVal));
           END IF;
     END CASE;   {startCond}
     
     startCond:="Off";
     spareObtained:=FALSE;
     DetermineNewState("Maintenance Completed");        
     IF (deferPM) 
        IF pmDeferGroup.Includes(SELF)
          ASK pmDeferGroup TO RemoveThis(SELF);
        END IF;
        deferPM:=FALSE;
     END IF;
  END METHOD;   {Repair}
  
  TELL METHOD PerformPM; 
  VAR
     maintTime  : REAL;
     oldStatus  : BlockStatusType;
  BEGIN
     oldStatus:=opStatus; {oldStatus must be saved because opStatus is changed in CBS}
     PMTimeLeft:=0.0;         
     ChangeBlockState(PM,activeStatus,"beginning_PM"); 
     IF ((oldStatus<>PMhold) AND coldsAffected)   {csb speed change}
        CheckColds;
     END IF;   
     GetStream(repairStream,"Repair",seqNum,randStream);
     ASK randStream TO DrawNumber(pmDist,pmParams,name,maintTime);
     PMWait:=TRUE;
     WAIT DURATION maintTime;
     ON INTERRUPT
        NEW(message,1..1);
        message[1]:="Error - PM Interrupted!     "; 
        result:=SendAlert(message,FALSE, FALSE, TRUE);
        DISPOSE(message);
     END WAIT; 
     CompTimeToDoPM:=maintTime;
     PMWait:=FALSE;
     spareObtained:=FALSE;  
     IF ((numDiffRes=1) AND (numRes1PM>0))          {beta140}                                 
        ASK PooledSpares[resPoolNum] TO TakeBack(SELF,numRes1PM);  
        PoolResAvailable[resPoolNum]:=PooledSpares[resPoolNum].Resources; 
     END IF; 
     IF EventsFile
         WriteEvents(SimTime,"Block",name,"PM_complete","MaintTime="+REALTOSTR(maintTime));
     END IF;
     IF pmRefresh
        FailTimeLeft:=0.0;
     END IF;
     DetermineNewState("Maintenance Completed");     
  END METHOD;     {PerformPM}
  
  ASK METHOD DetermineNewState (IN previousState : STRING);
     VAR
        depBlock                        : RBDBlockObj;
        depNode,tempNode                : RBDNodeObj;
        depEvent                        : RBDEventObj;
        spareAvail,sparesOut            : BOOLEAN;
        downNum                         : INTEGER;
     BEGIN  
  
        CASE previousState
           WHEN "Block Has Failed":
              IF EventsFile
                 WriteEvents(SimTime,"Block",name,"Failed","Operating_time="+REALTOSTR(OperatingTime));
              END IF; 
              IF WaitingForPrereqs
                 Interrupt(SELF, "ObtainPMprereqs");
                 WaitingForPrereqs:=FALSE;
              END IF;              
              IF (NOT spareObtained)
                 CheckForSpare(spareAvail,sparesOut); {order spare if necessary}
              ELSE
                 sparesOut:=FALSE;
              END IF;
              IF ((repairDistro = 18) OR (sparesOut))
                 IF (repairDistro = 18)
                    ChangeBlockState(Done,activeStatus,"No_repair");
                 ELSE
                    ChangeBlockState(Done,activeStatus,"No_spares");
                 END IF;
                 IF AllowReconfigureSystem                               
                    ReconfigureSystem(0);             
                 END IF;
                 {Block will exit since no further procedures are called}   
                 IF coldsAffected   {chuck   8/21/04}  {Error 999 beta val 6-004 Fix}
                    CheckColds;
                 END IF;
              ELSE
                 Repair;   {allows block to begin PreLDT,  Will hold up until spare and resources are obtained}
              END IF;
           WHEN "PM is due" :
              IF EventsFile
                 WriteEvents(SimTime,"Block",name,"PM_is_due",InterruptReason);
              END IF;   
              IF (activePhases>0)
                 IF ((pmMisDefer) AND (System.missionStatus=Mission))
                    deferPM:=TRUE;
                    IF NOT (pmDeferGroup.Includes(SELF))
                       ASK pmDeferGroup TO Add(SELF);
                    END IF;
                 END IF;
              END IF;
              IF NOT deferPM
                 ObtainPMprereqs;
              ELSE
                 Run;
              END IF;   
           WHEN "Obtained PM Prereqs" :  
              PerformPM;
           WHEN "Switched On":   {not used due to returnToTop in Run}
              Run;
           WHEN "Switched Off":  {not used due to returnToTop in Run}
              Run;
           WHEN "Dependence":    {not used due to returnToTop in Run}
              Run;
           WHEN "Run Method Interrupted":    {probable extraneous code -research then remove}
              CASE InterruptReason
                 WHEN  "PM_Resources_Arrived":
                    PerformPM;
                 OTHERWISE
              END CASE;
           WHEN "Maintenance Completed":
              returnFromMaint:=TRUE;                
              IF depType="RBDBlock"
                 depBlock := ASK root Child("RBDBlock",simDependencyNum);
                 IF depBlock.activeStatus=Linked
                    ChangeBlockState(Running,activeStatus,"");
                 ELSIF depBlock.activeStatus=Cut
                    IF DepIdlePerc=100.0  {repaired normally to find depBlock is down}
                       ChangeBlockState(Idle,activeStatus,"");
                    ELSE  {repaired from an induced maintenance}
                       IF defDepStateIdle
                          ChangeBlockState(Idle,activeStatus,"");
                       ELSE
                          ChangeBlockState(Running,activeStatus,"");
                       END IF;
                    END IF;
                 ELSE {block is not active and depBlock is Active}
                    IF ((depBlock.opStatus=Running) OR (depBlock.opStatus=Idle))
                          ChangeBlockState(Running,activeStatus,"");                         
                    ELSIF (depBlock.opStatus=Standby)
                       ChangeBlockState(Standby,activeStatus,"");                         
                    ELSE   {depBlock is RepHold, Repairing,Failure,PM,PMhold or Done}
                       IF DepIdlePerc=100.0  {repaired normally to find depObject is down}
                          ChangeBlockState(Idle,activeStatus,"");
                       ELSE  {repaired from an induced maintenance}
                          IF defDepStateIdle
                             ChangeBlockState(Idle,activeStatus,"");
                          ELSE
                             ChangeBlockState(Running,activeStatus,"");
                          END IF;
                       END IF;
                    END IF;
                 END IF;
              ELSIF depType="RBDEvent"
                 depEvent := ASK root Child("RBDEvent",simDependencyNum);
                 IF depEvent.activeStatus=Linked
                    ChangeBlockState(Running,activeStatus,"");
                 ELSIF depEvent.activeStatus=Cut
                    IF DepIdlePerc=100.0  {repaired normally to find depBlock is down}
                       ChangeBlockState(Idle,activeStatus,"");
                    ELSE  {repaired from an induced maintenance}
                       IF defDepStateIdle
                          ChangeBlockState(Idle,activeStatus,"");
                       ELSE
                          ChangeBlockState(Running,activeStatus,"");
                       END IF;
                    END IF;
                 ELSE {block is not active and depEvent is Active}
                    IF ((depEvent.opStatus=Armed) OR (depEvent.opStatus=Success))
                          ChangeBlockState(Running,activeStatus,"");                         
                    ELSE   {depEvent opStatus=Failure}
                       IF DepIdlePerc=100.0  {repaired normally to find depObject is down}
                          ChangeBlockState(Idle,activeStatus,"");
                       ELSE  {repaired from an induced maintenance}
                          IF defDepStateIdle
                             ChangeBlockState(Idle,activeStatus,"");
                          ELSE
                             ChangeBlockState(Running,activeStatus,"");
                          END IF;
                       END IF;
                    END IF;
                 END IF;
              ELSIF ((depType="RBDNode") OR (DependencyNum<0) OR (depType="RBDHier"))
                 depNode := ASK root Child("RBDNode",simDependencyNum);
                 IF ((activeStatus=Cut) OR (activeStatus=Linked))     
                             {downstream node's status is not affected by block's status}
                    IF depNode.higherCold
                       ChangeBlockState(Standby,activeStatus,"Dep-"+depNode.name);
                    ELSE
                       IF ((depNode.Status=AllUp) OR (depNode.Status=Degraded))
                          ChangeBlockState(Running,activeStatus,"");                         
                       ELSIF (depNode.Status=NodeStandby)
                          ChangeBlockState(Standby,activeStatus,"");                         
                       ELSE   {depNode is Down or Finished or PM}
                          IF DepIdlePerc=100.0  {repaired normally to find depObject is down}
                             ChangeBlockState(Idle,activeStatus,"");
                          ELSE  {repaired from an induced maintenance}
                             IF defDepStateIdle
                                ChangeBlockState(Idle,activeStatus,"");
                             ELSE
                                ChangeBlockState(Running,activeStatus,"");
                             END IF;
                          END IF;
                       END IF;
                    END IF;
                 ELSE  {block is active and depends on a node}
                    IF depNode.higherCold
                       ChangeBlockState(Standby,activeStatus,"Dep-"+depNode.name);
                    ELSE
                       IF ((depNode.Status=AllUp) OR (depNode.Status=Degraded))
                          ChangeBlockState(Running,activeStatus,"");                         
                       ELSIF (depNode.Status=NodeStandby)
                          ChangeBlockState(Standby,activeStatus,"Dep-"+depNode.name);
                       ELSE  {(depNode.Status=Down) OR standby or finished or PM  }
                          {***special case***}
                          {check to see if node is down because of block itself}
                          tempNode := ASK window Descendant("RBDNode", EFPAnode);  
                          ASK tempNode TO GetDownNum(EFPAlink, downNum);
                          IF (downNum>1)
                             IF DepIdlePerc=100.0  {repaired normally to find depObject is down}
                                ChangeBlockState(Idle,activeStatus,"");
                             ELSE  {repaired from an induced maintenance}
                                IF defDepStateIdle
                                   ChangeBlockState(Idle,activeStatus,"");
                                ELSE
                                   ChangeBlockState(Running,activeStatus,"");
                                END IF;
                             END IF;
                          ELSE
                             {fix block, then resend backflow  }
                             {necessary due to possiblility of dependece of other blocks
                              in the the same string on something not in the string}
                             ChangeBlockState(Running,activeStatus,""); 
                             IF coldsAffected   {csb speed change}
                                CheckColds;
                             END IF;
                             IF ((depNode.Status=Down) OR (depNode.Status=NodePM))   
                                IF defDepStateIdle
                                  ChangeBlockState(Idle,activeStatus,"");  
                                  prevDepState:="GoIdle";
                                END IF;
                             ELSE
                                IF defDepStateIdle
                                  prevDepState:="GoIdle";
                                END IF;
                                returnFromMaint:=FALSE; 
                             END IF;          
                             ResendBackFlow;  {not removed 05-24-05 cc/tm because of error}
                          END IF;
                       END IF;
                    END IF;
                 END IF;
              ELSE  {depType=""}
                 ChangeBlockState(Running,activeStatus,"");
              END IF;  {depType}
              returnFromMaint:=FALSE;
              IF coldsAffected   {csb speed change}
                 CheckColds;
              END IF;
              Run;
           WHEN "Phase Change":     {not used due to returnToTop in Run}
              Run;
           OTHERWISE
              NEW(message,1..1);
              message[1]:="Error in DetermineNewState!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message); 
        END CASE;
  END METHOD;    {DetermineNewState}
  
  ASK METHOD CheckForSpare      (OUT  spareAvail, sparesOut  : BOOLEAN);
  BEGIN
     sparesOut:=FALSE;
     IF (sparingType=Custom)   
        IF (CustomSpares[seqNum].Resources=0)  
           spareAvail:=FALSE;
           IF EventsFile
              WriteEvents(SimTime,"Block",name,"Sparing","No_spare_in_stock-Custom");
           END IF;  {Events file}
           IF (emerSpareOrdering) 
              OrderASpare();   
           ELSE  
              IF (NOT stockLevelOrdering) AND (NOT routineSpareOrdering)
                 sparesOut:=TRUE;
              END IF;   {Any due to come in}
           END IF;  {Allowed to Order} 
        ELSE
           spareAvail:=TRUE;
        END IF;   {Spare not available}
     ELSIF (sparingType=SparePool)
        spareObtained:=FALSE;
        emerSpareOrdered:=FALSE;
        IF (PooledSpares[sparePoolNum].Resources=0) 
           spareAvail:=FALSE;
           IF EventsFile
              WriteEvents(SimTime,"Block",name,"Sparing","No_spare_in_stock-"+PoolArray[sparePoolNum].poolName);
           END IF;
           IF (PoolArray[sparePoolNum].emerSpareOrdering)
              TELL PooledSpares[sparePoolNum] TO OrderASpare(sparePoolNum); 
              ASK PooledSpares[sparePoolNum] TO IncNumEmerOrdered;
              emerSpareOrdered:=TRUE;
           ELSE {if you cannot order a spare}
              IF ((NOT PoolArray[sparePoolNum].stockLevelOrdering) AND (NOT PoolArray[sparePoolNum].routineSpareOrdering))   
                 sparesOut:=TRUE;
              END IF;   {Any due to come in}
           END IF;  {Allowed to Order}  
        ELSE
           spareAvail:=TRUE;
           IF (PooledSpares[sparePoolNum].Resources<=PooledSpares[sparePoolNum].NumEmerOrdered)
              IF EventsFile
                 WriteEvents(SimTime,"Block",name,"Sparing","Used_Ordered_Spare-"+PoolArray[sparePoolNum].poolName);
              END IF;
              TELL PooledSpares[sparePoolNum] TO OrderASpare(sparePoolNum); 
           END IF;
        END IF;   {Spare not available}
     ELSIF (sparingType=None)      {not used - thoroughly research and then remove}
        sparesOut:=TRUE;
        spareAvail:=FALSE;
     ELSIF (sparingType=Infinite)
        spareAvail:=TRUE;
     END IF;   {sparingType}
  END METHOD;    {CheckForSpare}

  TELL METHOD ObtainPMprereqs;
  VAR
     WaitingForSpare,WaitingForResource,sparesDone,spareAvail, resAvail     : BOOLEAN;
  BEGIN
     sparesDone:=FALSE;
     spareAvail:=TRUE;
     WaitingForPrereqs:=TRUE;

     IF pmSpareNeeded
        CASE sparingType
           WHEN Infinite:
              IF costAnalysis
                 BlockSpCost[seqNum] :=  BlockSpCost[seqNum] + spareCost;
              END IF;
              INC(SparesUsed[seqNum]);
              spareObtained:=TRUE;
           WHEN Custom:     
              IF (CustomSpares[seqNum].Resources=0)  
                 spareAvail:=FALSE;
                 IF (emerSpareOrdering) 
                     OrderASpare();   
                 ELSE  
                    IF (NOT stockLevelOrdering) AND (NOT routineSpareOrdering)   {will one come?}
                       sparesDone:=TRUE;
                    END IF;   {Any due to come in}
                 END IF;  {Allowed to Order} 
              END IF;
              IF NOT sparesDone {else block leaves this method and returns to Run}
                 IF (NOT spareAvail)
                    IF pmReqDefer
                       IF EventsFile
                           WriteEvents(SimTime,"Block",name,"pmReqDefer","No_spare_in_stock-Custom");
                       END IF;  {Events file}
                       Run;
                    ELSE
                       ChangeBlockState(PMhold, activeStatus,"No_spare_in_stock-Custom");
                    END IF;
                    INC(currNumWaits[seqNum]);
                    WaitingForSpare:=TRUE;                    
                 END IF;
                 WAIT FOR CustomSpares[seqNum] TO Give(SELF,1);  
                 ON INTERRUPT {Could be interrupted by DetermineNewState if the block failed}
                    ASK CustomSpares[seqNum] TO Cancel(SELF,1);
                    WaitingForSpare:=FALSE;
                    RETURN;
                 END WAIT;
                 INC(SparesUsed[seqNum]);
                 SparesAvailable[seqNum]:=CustomSpares[seqNum].Resources;
                 spareObtained:=TRUE;
                 IF stockLevelOrdering
                    IF (CustomSpares[seqNum].Resources=SLOOrderLevel) 
                       OrderStock(SLONewSpares);
                    ELSIF (CustomSpares[seqNum].Resources<=(SLOOrderLevel-SLONewSpares) )
                       OrderStock(1); {prevents pool from going too far below SLO order level} 
                    END IF;
                 END IF;
                 IF WaitingForSpare
                    WaitingForSpare:=FALSE;
                    IF EventsFile
                       WriteEvents(SimTime,"Block",name,"Sparing","Spare_Arrived");
                    END IF;
                 END IF;
              END IF; {NOT sparesDone}
                
           WHEN SparePool:
              emerSpareOrdered:=FALSE;
              IF (PooledSpares[sparePoolNum].Resources=0) 
                 spareAvail:=FALSE;
                 IF (PoolArray[sparePoolNum].emerSpareOrdering)
                    TELL PooledSpares[sparePoolNum] TO OrderASpare(sparePoolNum); 
                    ASK PooledSpares[sparePoolNum] TO IncNumEmerOrdered;
                    emerSpareOrdered:=TRUE;
                 ELSE {if you cannot order a spare}
                    IF ((NOT PoolArray[sparePoolNum].stockLevelOrdering) AND (NOT PoolArray[sparePoolNum].routineSpareOrdering))   
                       sparesDone:=TRUE;
                    END IF;   {Any due to come in}
                 END IF;  {Allowed to Order}  
              END IF;
              IF (NOT sparesDone)
                 IF (NOT spareAvail)
                    IF pmReqDefer
                       IF EventsFile
                           WriteEvents(SimTime,"Block",name,"pmReqDefer","No_spare_in_stock-"+
                                                                                 PoolArray[sparePoolNum].poolName);
                       END IF;  {Events file}
                       Run;
                    ELSE
                       ChangeBlockState(PMhold, activeStatus,"pmReqDefer");
                    END IF;
                    INC(currNumWaits[sparePoolNum+totalBlocks]);
                    WaitingForSpare:=TRUE;                    
                 END IF;
                 WAIT FOR PooledSpares[sparePoolNum] TO Give(SELF,1);
                 ON INTERRUPT {Could be interrupted by DetermineNewState if the block failed}
                    ASK PooledSpares[sparePoolNum] TO Cancel(SELF,1);
                    WaitingForSpare:=FALSE;
                    RETURN;
                 END WAIT;
                 IF emerSpareOrdered
                    ASK PooledSpares[sparePoolNum] TO DecNumEmerOrdered;
                 END IF;
                 PoolResAvailable[sparePoolNum]:=PooledSpares[sparePoolNum].Resources;
                 INC(SparesUsed[seqNum]);
                 spareObtained:=TRUE;
                 IF (PoolArray[sparePoolNum].stockLevelOrdering)
                    IF (PooledSpares[sparePoolNum].Resources=PoolArray[sparePoolNum].SLOOrderLevel) 
                       TELL PooledSpares[sparePoolNum] TO OrderStock(sparePoolNum,PoolArray[sparePoolNum].SLONewSpares );    
                    ELSIF ( PooledSpares[sparePoolNum].Resources<=(PoolArray[sparePoolNum].SLOOrderLevel-PoolArray[sparePoolNum].SLONewSpares) ) 
                       TELL PooledSpares[sparePoolNum] TO OrderStock(sparePoolNum,1); 
                    END IF;
                 END IF; 
                 IF WaitingForSpare
                    WaitingForSpare:=FALSE;
                    IF EventsFile
                       WriteEvents(SimTime,"Block",name,"Sparing","Spare_Arrived");
                    END IF;
                 END IF;
              END IF; {NOT sparesDone}
           WHEN None: {may be remnant}
              sparesDone:=TRUE;
        END CASE;
     END IF; {pmSpareNeeded}
        
     IF sparesDone
        skipPM:=TRUE;
        Run;
        RETURN;
     END IF;
        
     IF ((numDiffRes=1) AND (numRes1PM>0))       {PM Resource Needed}
        IF ((PooledSpares[resPoolNum].Resources<numRes1PM) OR (PooledSpares[resPoolNum].ReportNumberPending>0)) 
           {Either the pool doesn't have enough OR it does but they've already been claimed}
           INC(currNumWaits[resPoolNum+totalBlocks]);
           resAvail:=FALSE;
           WaitingForResource:=TRUE;
        ELSE
           resAvail:=TRUE;
        END IF;           
        IF (NOT resAvail)
           IF pmReqDefer
              IF ((NOT FailWait) AND (NOT ZeroWait))
                 IF EventsFile
                    WriteEvents(SimTime,"Block",name,"pmReqDefer","No_resource_in_stock-"+PoolArray[resPoolNum].poolName);
                 END IF;  {Events file}
                 Run; {This makes block active in two methods}
              END IF;
           ELSE
              IF opStatus<>PMhold
                 ChangeBlockState(PMhold, activeStatus,"pmReqDefer");
              END IF;
           END IF;
           INC(currNumWaits[resPoolNum+totalBlocks]); {walkthrough error -- was sparePoolNum}
        END IF;
        WAIT FOR PooledSpares[resPoolNum] TO Give(SELF,numRes1PM); 
        ON INTERRUPT
           ASK PooledSpares[resPoolNum] TO Cancel(SELF,numRes1PM);
           WaitingForResource:=FALSE;
           RETURN;
        END WAIT; 
        IF costAnalysis {Variable PoolESCost for resource is its fixed cost so as to not waste the electrons}
           PoolESCost[resPoolNum]:=PoolESCost[resPoolNum]+PoolArray[resPoolNum].fixedPerUse*FLOAT(numRes1PM); 
        END IF;
        PoolResAvailable[resPoolNum]:=PooledSpares[resPoolNum].Resources;
        IF WaitingForResource
           WaitingForResource:=FALSE;
           IF EventsFile
              WriteEvents(SimTime,"Block",name,"Resources","Obtained_"+INTTOSTR(numRes1PM)+"_Resource(s)");
           END IF;
        END IF;
     END IF; {End PM Resource Needed} 
     WaitingForPrereqs:=FALSE;      
     IF opStatus=PMhold
        DetermineNewState("Obtained PM Prereqs");
     ELSE {Came clean through (no holds) or pmReqDefer}
        IF ((activePhases>0) AND (pmMisDefer))
           WAIT DURATION 0.0;
           END WAIT;
           IF PhaseChangeInProgress
              WAIT FOR pmSyncTrigger TO Fire;
              END WAIT;
           END IF;
           IF (System.missionStatus=Mission) {PM will be postponed -- give everything back}
              ASK pmDeferGroup TO Add(SELF);
              deferPM:=TRUE;
              IF pmSpareNeeded
                 CASE sparingType
                    WHEN Infinite:
                       IF costAnalysis
                          BlockSpCost[seqNum] :=  BlockSpCost[seqNum] - spareCost;
                       END IF;
                       DEC(SparesUsed[seqNum]);
                    WHEN Custom:     
                       DEC(SparesUsed[seqNum]);
                       ASK CustomSpares[seqNum] TO TakeBack(SELF,1);  
                       SparesAvailable[seqNum]:=CustomSpares[seqNum].Resources;
                    WHEN SparePool:
                       ASK PooledSpares[sparePoolNum] TO TakeBack(SELF,1);  
                       DEC(SparesUsed[seqNum]);  
                       PoolResAvailable[sparePoolNum]:=PooledSpares[sparePoolNum].Resources;
                    WHEN None:   {remnant}              
                 END CASE;
                 spareObtained:=FALSE;
              END IF; {pmSpareNeeded}
              IF ((numDiffRes=1) AND (numRes1PM>0))   {give back resource}
                 ASK PooledSpares[resPoolNum] TO TakeBack(SELF,numRes1PM);  
                 PoolResAvailable[resPoolNum]:=PooledSpares[resPoolNum].Resources;
                 IF EventsFile
                    WriteEvents(SimTime,"Block",name,"Resources","Returned_"+INTTOSTR(numRes1PM)+"_Resource(s)");
                 END IF;
                 IF costAnalysis     {Variable PoolESCost is fixed cost for a resource}
                    PoolESCost[resPoolNum]:=PoolESCost[resPoolNum]-PoolArray[resPoolNum].fixedPerUse* FLOAT(numRes1PM); 
                 END IF; 
              END IF;    {continue running}
           ELSE    {nonMission time}        
              IF FailWait {pmReqDefer}
                 InterruptReason:="Obtained_PM_Prereqs";
                 Interrupt(SELF,"Run");
              ELSIF ZeroWait {pmReqDefer}
                 InterruptReason:="Obtained_PM_Prereqs";
                 ASK BlockTrigger[seqNum] TO Release;
              ELSE {Came clean through}
                 DetermineNewState("Obtained PM Prereqs");
              END IF;
           END IF;  {(pmMisDefer) AND (System.missionStatus=Mission}
        ELSE     {not misDefer}       
           IF FailWait
              InterruptReason:="Obtained_PM_Prereqs";
              Interrupt(SELF,"Run");
           ELSIF ZeroWait
              InterruptReason:="Obtained_PM_Prereqs";
              ASK BlockTrigger[seqNum] TO Release;
           ELSE
              DetermineNewState("Obtained PM Prereqs");
           END IF;
        END IF;  {(activePhases>0) AND (pmMisDefer)}
     END IF; {End opStatus = PMHold} 
  END METHOD;   {ObtainPMprereqs}
  
  TELL METHOD GenerateSpares();   
     BEGIN
        LOOP
           WAIT DURATION arrivalRate; 
           END WAIT;
           IF costAnalysis
              BlockSpCost[seqNum]:=BlockSpCost[seqNum]+spareCost*FLOAT(newSpares);
           END IF;
           ASK CustomSpares[seqNum] TO IncrementResourcesBy(newSpares);
           currNumArrivals[seqNum]:=currNumArrivals[seqNum]+newSpares;
           SparesAvailable[seqNum]:=CustomSpares[seqNum].Resources;   
           IF EventsFile
              WriteEvents(SimTime,"Block",name,"Sparing",INTTOSTR(newSpares) + "_new_spare(s)_arrived_(stock="+
                             INTTOSTR(SparesAvailable[seqNum])+")");
           END IF;
        END LOOP;
  END METHOD;   {GenerateSpares}
  
  TELL METHOD OrderASpare(); 
     BEGIN
        IF costAnalysis
           BlockSpCost[seqNum]:=BlockSpCost[seqNum]+spareCost;
           BlockESCost[seqNum]:=BlockESCost[seqNum]+emerShippingCost;
        END IF;
           IF EventsFile
              WriteEvents(SimTime,"Block",name,"Sparing","Spare_ordered_(stock="+INTTOSTR(SparesAvailable[seqNum])+")");
           END IF;
        WAIT DURATION emerTime; 
        END WAIT; 
           ASK CustomSpares[seqNum] TO IncrementResourcesBy(1);   
           SparesAvailable[seqNum]:=CustomSpares[seqNum].Resources; 
           currNumArrivals[seqNum]:=currNumArrivals[seqNum]+1;
           IF EventsFile
              WriteEvents(SimTime,"Block",name,"Sparing","Ordered_spare_arrives_(stock="+INTTOSTR(SparesAvailable[seqNum])+")");
           END IF;
  END METHOD;   {OrderASpare}

  TELL METHOD OrderStock(IN quantity : INTEGER);  
  BEGIN
     IF costAnalysis
        BlockSpCost[seqNum]:=BlockSpCost[seqNum]+spareCost*FLOAT(quantity);
     END IF;
     IF EventsFile
        WriteEvents(SimTime,"Block",name,"Sparing",INTTOSTR(quantity)+"_Spare(s)_ordered_(stock="+INTTOSTR(SparesAvailable[seqNum])+")");
     END IF;
     WAIT DURATION SLOTime; 
     END WAIT; 
     ASK CustomSpares[seqNum] TO IncrementResourcesBy(quantity);   
     SparesAvailable[seqNum]:=CustomSpares[seqNum].Resources; 
     currNumArrivals[seqNum]:=currNumArrivals[seqNum]+quantity;
     IF EventsFile
        WriteEvents(SimTime,"Block",name,"Sparing",INTTOSTR(quantity)+"_Spare(s)_arrived_(stock="+INTTOSTR(SparesAvailable[seqNum])+")");
     END IF;
  END METHOD;   {OrderStock}
  
  ASK METHOD Initialize();
  VAR
     j                     : INTEGER;
     innerSquare           : ImageObj;
     tempImage             : FillVObj;
  BEGIN   
     tempImage := Child("BasicBlock", 601);            
     IF ((activePhases>0) AND (usesPhasing)) 
        IF (parentID = activeWindow)
           innerSquare:= Descendant("InnerSquare", 0);
           ASK innerSquare TO SetHidden(FALSE);                      
        END IF;
        ASK tempImage TO SetStyle(SolidFill);
     END IF;
     ASK tempImage TO Draw; 
     GoodPathsRequired:=1;
     MinGoodPathsReq:=1;
     sysDepend:=FALSE;
     locDepend:=FALSE;
     CASE DependencyNum
        WHEN -2:
           sysDepend:=TRUE;
           simDependencyNum:=endId;
        WHEN -1:
           locDepend:=TRUE;
           simDependencyNum:=EFPAnode;
        OTHERWISE
           simDependencyNum:=DependencyNum;
     END CASE;
     IF (simDependencyNum>0)
        ASK BlockDepGroup TO Add(SELF);
     END IF;
     coldDep:=FALSE;
     opStatus:=Running;
     IF ((usesPhasing) AND (activePhases>0)) 
        AllowReconfigureSystem:=TRUE;
        FOR j:=1 TO activePhases
        {   IF (phaseValue[j] =0.)                     {changed for phaseValue}      }
           IF (phaseType[j]="L")  {tony 3-06 replaced commented line to solve error 999}       
              AllowReconfigureSystem:=FALSE;
           END IF;
        END FOR;     
     ELSE
        AllowReconfigureSystem:=TRUE;
     END IF;
  END METHOD;

  ASK METHOD SetToInitValues();       {block}
  BEGIN
     stateStartTime:=0.0;
     IF costAnalysis
        BlockOpCost[seqNum]:=0.0;
        BlockRepCost[seqNum]:=0.0;
        BlockIdleCost[seqNum]:=0.0;
        BlockHoldCost[seqNum]:=0.0;
        BlockPMCost[seqNum]:=0.0;
        BlockpmHoldCost[seqNum]:=0.0;
        BlockSBCost[seqNum]:=0.0;
        BlockDoneCost[seqNum]:=0.0;
        BlockDispCost[seqNum]:=0.0;
        BlockSpCost[seqNum]:=0.0;
        BlockESCost[seqNum]:=0.0;
     END IF;
     IF weakAnalysis
        BlockRunTime[seqNum]:=0.0;
        BlockStandbyTime[seqNum]:=0.0;
        BlockIdleTime[seqNum]:=0.0;
        BlockRepairTime[seqNum]:=0.0;
        BlockRepHoldTime[seqNum]:=0.0;
        BlockPMTime[seqNum]:=0.0;
        BlockPMHoldTime[seqNum]:=0.0;
        BlockDoneTime[seqNum]:=0.0;
     END IF;   
     NumGoodPaths:=EconnectIntoNum; 
     NumActivePaths:=EconnectIntoNum; 
     GoodPathsRequired:=1;
     FailureCounter:=0;
     discardedFailures:=0;
     activeStatus:=Active;
     IF ((usesPhasing) AND (activePhases>0))            
        PhaseStress:=phaseValue[1];
     ELSE
        PhaseStress:=1.0;
     END IF;
     opStatus:=Running;
     IF ( (usesPhasing) AND (activePhases>0) AND ((phaseType[1]="L") OR (phaseType[1]="C")) )     {can we have thsi?}       
        IF (phaseType[1]="L")                                   {Block is linked in Phase1}
           IF (startCond="Running")
              ChangeBlockState(Running,Linked,"Initialize");
           ELSIF ((startCond="preLDT") OR (startCond="postLDT"))
              ChangeBlockState(RepHold,Linked,"Initialize");
           ELSE
              ChangeBlockState(Repairing,Linked,"Initialize");
           END IF;              
        ELSIF (phaseType[1]="C")                                {Block is cut in Phase1}
           IF (startCond="Running")
              ChangeBlockState(Running,Cut,"Initialize");
           ELSIF ((startCond="preLDT") OR (startCond="postLDT"))
              ChangeBlockState(RepHold,Cut,"Initialize");
           ELSE
              ChangeBlockState(Repairing,Cut,"Initialize");
           END IF;              
        END IF;
     ELSE                                                     {Block is active in Phase1}
        IF (startCond="Running")
           ChangeBlockState(Running,Active,"Initialize");
        ELSIF ((startCond="preLDT") OR (startCond="postLDT"))
           ChangeBlockState(RepHold,Active,"Initialize");
        ELSE
           ChangeBlockState(Repairing,Active,"Initialize");
        END IF;              
     END IF; 
     FailWait:=FALSE;
     RepWait:=FALSE;
     PMWait:=FALSE;
     PMTimeLeft:=pmFreq-pmStagger;
     skipPM:= FALSE;
     deferPM:=FALSE;
     ZeroWait:=FALSE;
     IgnoreDep:=FALSE; 
     spareObtained:=FALSE;
     returnFromMaint:=FALSE;
     WaitingForPrereqs:=FALSE;
     OperatingTime:=0.0;
     InterruptReason:="";
     prevDepState:="";
     blockHasStarted:=FALSE;          {cmc 12/13/06}
     TzeroAction:="";       {cmc 12/13/06}

  END METHOD; 

  ASK METHOD GetStartCond;
  VAR
         randDraw1,randDraw2,TimeToFail,TimeToRepair,cycleTime,
         downPercent,preLDT,postLDT,dummy                               : REAL;
         gettingDraw                                                    : BOOLEAN;
         i                                                              : INTEGER;         
         tempArray                                                      : realArray;
  BEGIN
        FailTimeLeft:=0.0;          
        ComputeStats(preDist,preParams,preLDT,dummy);
        ComputeStats(postDist,postParams,postLDT,dummy);
        IF ((simStartType=1) OR (simStartType=2) OR (simStartType=4)) 
           NEW(tempArray,1..2);
           tempArray[1]:=0.0;
           tempArray[2]:=1.0;
           GetStream(failStream,"Fail",seqNum,randStream);
           ASK randStream TO DrawNumber(10,tempArray,name,randDraw1);
           GetStream(failStream,"Fail",seqNum,randStream);
           ASK randStream TO DrawNumber(10,tempArray,name,randDraw2);
           DISPOSE(tempArray);
        END IF;
        GetStream(failStream,"Fail",seqNum,randStream);
        ASK randStream TO DrawNumber(failDistro,failVals,name,TimeToFail);
        CASE simStartType
           WHEN 1:  {completely random}
              IF repairDistro=18
                 cycleTime:=failMean;
              ELSE
                 cycleTime:=failMean+preLDT+repairMean+postLDT;
              END IF;
              IF (randDraw1<(failMean/cycleTime))
                 startCond:="Running";
                 FailTimeLeft:=TimeToFail*randDraw2;
              ELSIF (randDraw1<((failMean+preLDT)/cycleTime))
                 GetStream(repairStream,"Repair",seqNum,randStream);
                 ASK randStream TO DrawNumber(preDist,preParams,name,preLDT);
                 GetStream(repairStream,"Repair",seqNum,randStream);
                 ASK randStream TO DrawNumber(repairDistro,repairVals,name,TimeToRepair);
                 startCond:="preLDT";
                 firstPreLDT:=preLDT*randDraw2;
                 startVal:=TimeToRepair;
              ELSIF (randDraw1<((failMean+preLDT+repairMean)/cycleTime))
                 GetStream(repairStream,"Repair",seqNum,randStream);
                 ASK randStream TO DrawNumber(repairDistro,repairVals,name,TimeToRepair);
                 TimeToRepair:=TimeToRepair*randDraw2;
                 startCond:="repairing";
                 startVal:=TimeToRepair;
              ELSE
                 GetStream(repairStream,"Repair",seqNum,randStream);
                 ASK randStream TO DrawNumber(postDist,postParams,name,postLDT);
                 startCond:="postLDT";
                 startVal:=postLDT*randDraw2;
              END IF;                    
           WHEN 2:  {up random}
              TimeToFail:=TimeToFail*randDraw1;
              startCond:="Running";
              FailTimeLeft:=TimeToFail;
           WHEN 3:  {up, some complete}
              gettingDraw:=TRUE;
              i:=1;
              WHILE gettingDraw
                 IF ((TimeToFail>amountExhausted))
                    gettingDraw:=FALSE;
                    TimeToFail:=TimeToFail-amountExhausted;
                 ELSE
                    IF i<100
                       GetStream(failStream,"Fail",seqNum,randStream);
                       ASK randStream TO DrawNumber(failDistro,failVals,name,TimeToFail);
                       INC(i);
                    ELSE;
                       gettingDraw:=FALSE;
                       TimeToFail:=0.000001;
                    END IF;
                 END IF;
              END WHILE;
              startCond:="Running";
              FailTimeLeft:=TimeToFail;                    
           WHEN 4:  {down random} 
              IF repairDistro=18
                 startCond:="repairing";
              ELSE
                 cycleTime:=preLDT+repairMean+postLDT;
                 IF (randDraw1<(preLDT/cycleTime))
                    GetStream(repairStream,"Repair",seqNum,randStream);
                    ASK randStream TO DrawNumber(preDist,preParams,name,preLDT);
                    GetStream(repairStream,"Repair",seqNum,randStream);
                    ASK randStream TO DrawNumber(repairDistro,repairVals,name,TimeToRepair);
                    startCond:="preLDT";
                    firstPreLDT:=preLDT*randDraw2;
                    startVal:=TimeToRepair;
                 ELSIF (randDraw1<((preLDT+repairMean)/cycleTime))
                    GetStream(repairStream,"Repair",seqNum,randStream);
                    ASK randStream TO DrawNumber(repairDistro,repairVals,name,TimeToRepair);
                    TimeToRepair:=TimeToRepair*randDraw2;
                    startCond:="repairing";
                    startVal:=TimeToRepair;
                 ELSE
                    GetStream(repairStream,"Repair",seqNum,randStream);
                    ASK randStream TO DrawNumber(postDist,postParams,name,postLDT);
                    startCond:="postLDT";
                    startVal:=postLDT*randDraw2;
                 END IF;
              END IF;
           WHEN 5:  {down, some complete}
              IF repairDistro=18
                 startCond:="repairing";
              ELSE
                 GetStream(repairStream,"Repair",seqNum,randStream);
                 ASK randStream TO DrawNumber(preDist,preParams,name,preLDT);
                 GetStream(repairStream,"Repair",seqNum,randStream);
                 ASK randStream TO DrawNumber(repairDistro,repairVals,name,TimeToRepair);
                 GetStream(repairStream,"Repair",seqNum,randStream);
                 ASK randStream TO DrawNumber(postDist,postParams,name,postLDT);
                 cycleTime:=preLDT+TimeToRepair+postLDT;
                 downPercent:=amountExhausted/100.0;
                 IF (downPercent<(preLDT/cycleTime))
                    startCond:="preLDT";
                    firstPreLDT:=preLDT-downPercent*cycleTime;
                    startVal:=TimeToRepair;
                 ELSIF (downPercent<((preLDT+TimeToRepair)/cycleTime))
                    TimeToRepair:=preLDT+TimeToRepair-downPercent*cycleTime;
                    startCond:="repairing";
                    startVal:=TimeToRepair;
                 ELSE
                    startCond:="postLDT";
                    startVal:=preLDT+TimeToRepair+postLDT-downPercent*cycleTime;
                 END IF;
              END IF;
           WHEN 6:  {down, start of repair}                
              IF repairDistro=18
                 startCond:="repairing";
              ELSE
                 GetStream(repairStream,"Repair",seqNum,randStream);
                 ASK randStream TO DrawNumber(repairDistro,repairVals,name,TimeToRepair);
                 startCond:="repairing";
                 startVal:=TimeToRepair;
              END IF;
           OTHERWISE             
              NEW(message,1..1);
              message[1]:="Unknown simStartType!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message);
        END CASE;
  END METHOD;    {GetStartCond}
     
   ASK METHOD ResetStats;
   BEGIN 
      discardedFailures:=FailureCounter;
   END METHOD;
     
   ASK METHOD CleanUp;
     VAR
        blockImage, innerSquare    : ImageObj;
        tempImage                  : FillVObj;
   BEGIN   
        blockImage := Descendant("BasicBlock", 601);            
        innerSquare:= Descendant("InnerSquare", 0);
        ASK innerSquare TO SetColor(blockGUIColor);
        ASK innerSquare TO SetHidden(TRUE);
        ASK blockImage TO SetColor(blockGUIColor);
        ASK blockImage TO SetTranslation(blockImage.Translation.x,
                                            blockImage.Translation.y);
        tempImage:=Child("BasicBlock",601);                                    
        IF ((usesPhasing) AND (phaseValue <> NILARRAY))                                   
           ASK tempImage TO SetStyle(NarrowCrosshatchFill);
        END IF;
        {ASK blockImage TO Draw;}
        Draw;
   END METHOD;
   
   ASK METHOD ChangeBlockState (IN newOpStatus          : BlockStatusType;
                                IN newActiveStatus      : ActiveStatusType;
                                IN comment              : STRING);      
     VAR
        blockImage,innerSquare                                  : ImageObj;
        oldOpStatus                                             : BlockStatusType;
        stateTime                                               : REAL;
        activeString,statusString,actionString,commentString,
        outString                                               : STRING;
        oldActiveStatus                                         : ActiveStatusType; 
        outerColor, innerColor                                  : ColorType;
        incFails,decFails                                       : BOOLEAN;
        tempHier                                                : RBDHierObj;
        RunChange,FailChange,IdleChange,StandbyChange,pmChange  : INTEGER;
        tempNode                                                : RBDNodeObj;
     BEGIN     
        oldOpStatus:=opStatus;
        oldActiveStatus:=activeStatus;
        opStatus:=newOpStatus; 
        activeStatus:=newActiveStatus;
        SystemStateChange:=TRUE;
        IF GraphicsOutput
           innerSquare:= Descendant("InnerSquare", 0);
           blockImage := Descendant("BasicBlock", 601);            
           CASE opStatus
              WHEN Running:
                 outerColor:=LimeGreen;
              WHEN Repairing:
                 outerColor:=Red;
              WHEN RepHold:    
                 outerColor:=Firebrick;
              WHEN PM:
                 outerColor:=Orange;
              WHEN PMhold:    
                 outerColor:=Gold;
              WHEN Idle:
                 outerColor:=Sienna;
              WHEN Standby:                  
                 outerColor:=Blue;
              WHEN Done:              
                 IF NOT runCompleted
                    outerColor:=IndianRed;
                 ELSE
                    outerColor:=LimeGreen;
                 END IF;  
              OTHERWISE             
                 NEW(message,1..1);
                 message[1]:="Unknown block status color!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                DISPOSE(message);
           END CASE;
           ASK blockImage TO SetColor(outerColor);
           ASK blockImage TO SetTranslation(blockImage.Translation.x,blockImage.Translation.y);
           CASE activeStatus
              WHEN Linked:
                 innerColor:=White;
              WHEN Cut:                                     
                 innerColor:=Black;
              WHEN Active:                                     
                 innerColor:=outerColor;
              OTHERWISE
                    NEW(message,1..1);
                    message[1]:="Unknown block active status color!     "; 
                    result:=SendAlert(message,FALSE, FALSE, TRUE);
                    DISPOSE(message);
           END CASE;
           
           ASK innerSquare TO SetColor(innerColor);
           IF ((oldActiveStatus=Active) AND (newActiveStatus<>Active) AND (parentID=activeWindow))
              ASK innerSquare TO SetHidden(FALSE); 
           ELSIF ((oldActiveStatus<>Active) AND (newActiveStatus=Active) AND (parentID=activeWindow))
              ASK innerSquare TO SetHidden(TRUE); 
           END IF;
           Draw;
        END IF;
        {Cost analysis}
        stateTime:=SimTime-stateStartTime;
        IF costAnalysis
           IF oldOpStatus=Idle                   
              BlockIdleCost[seqNum]:=BlockIdleCost[seqNum]+idleCost*stateTime;              
           ELSIF oldOpStatus=RepHold
              BlockHoldCost[seqNum]:=BlockHoldCost[seqNum]+repHoldCost*stateTime; 
           ELSIF oldOpStatus=Repairing
              BlockRepCost[seqNum]:=BlockRepCost[seqNum]+repairingCost*stateTime+repFixedCost;
              IF numDiffRes=1                                 
                 PoolAddCost[resPoolNum]:=PoolAddCost[resPoolNum]+
                    stateTime*PoolArray[resPoolNum].costPerTime*FLOAT(numRes1); 
              END IF;              
              {spare costs charged when repair starts}    
              IF (RepWait)               
                 IF EventsFile
                    WriteEvents(SimTime,"Block",name,"Partial_Repair","RepairTime="+REALTOSTR(stateTime));
                 END IF;
              END IF;
           ELSIF oldOpStatus=Running
              BlockOpCost[seqNum]:=BlockOpCost[seqNum]+operatingCost*stateTime;              
           ELSIF oldOpStatus=Standby
              BlockSBCost[seqNum]:=BlockSBCost[seqNum]+standbyCost*stateTime;                                
           ELSIF oldOpStatus=PMhold
              BlockpmHoldCost[seqNum]:=BlockpmHoldCost[seqNum]+pmHoldCost*stateTime;                          
           ELSIF oldOpStatus=PM
              BlockPMCost[seqNum]:=BlockPMCost[seqNum]+pmCost*stateTime+pmFixedCost;                            
              IF numDiffRes=1                                                    {chuck 11/10/04}
                 PoolAddCost[resPoolNum]:=PoolAddCost[resPoolNum]+
                    stateTime*PoolArray[resPoolNum].costPerTime*FLOAT(numRes1PM); 
              END IF;              
           ELSIF ((oldOpStatus=Done)  AND (opStatus=Done))
              BlockDoneCost[seqNum]:=doneCost*stateTime; 
              BlockDispCost[seqNum]:=doneFixedCost;
           ELSIF ((oldOpStatus=Done)  AND (opStatus<>Done))
              {do nothing}
           ELSE
              NEW(message,1..1);
              message[1]:="Error in cost analysis section!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message);
           END IF;
           IF ((opStatus=Done) AND (oldOpStatus<>Done))
              IF  runCompleted AND alwaysAddDoneCost
                 BlockDispCost[seqNum]:=doneFixedCost;               
              END IF;  
           END IF;
        END IF;
        IF weakAnalysis
           CASE oldOpStatus
              WHEN Running:
                 BlockRunTime[seqNum]:=BlockRunTime[seqNum]+stateTime;
              WHEN Repairing:
                 BlockRepairTime[seqNum]:=BlockRepairTime[seqNum]+stateTime;
              WHEN RepHold:    
                 BlockRepHoldTime[seqNum]:=BlockRepHoldTime[seqNum]+stateTime;
              WHEN PM:
                 BlockPMTime[seqNum]:=BlockPMTime[seqNum]+stateTime;
              WHEN PMhold:    
                 BlockPMHoldTime[seqNum]:=BlockPMHoldTime[seqNum]+stateTime;
              WHEN Idle:
                 BlockIdleTime[seqNum]:=BlockIdleTime[seqNum]+stateTime;
              WHEN Standby:                  
                 BlockStandbyTime[seqNum]:=BlockStandbyTime[seqNum]+stateTime;
              WHEN Done:
                 BlockDoneTime[seqNum]:=BlockDoneTime[seqNum]+stateTime;  
              OTHERWISE             
                 NEW(message,1..1);
                 message[1]:="Unknown block status for Weak Link Analysis!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                DISPOSE(message);
           END CASE; 
           IF System.missionStatus=Mission
              IF ((newOpStatus=Repairing) OR (newOpStatus=RepHold) OR (newOpStatus=PM) OR
                  (newOpStatus=PMhold) OR (newOpStatus=Done))
                 IF comment<>"End_sim"
                    blockMissionBool[seqNum]:=FALSE;
                 END IF;   
              END IF;    
           END IF;    
        END IF;
        stateStartTime:=SimTime; 
        IF EventsFile  
           CASE activeStatus
              WHEN Cut:
                 activeString:="Cut";
              WHEN Linked:
                 activeString:="Linked";
              WHEN Active:
                 activeString:="Active";
           END CASE;
           CASE opStatus
              WHEN Running:
                 statusString:="Good";
              WHEN Repairing:
                 statusString:="Repairing";
              WHEN RepHold:    
                 statusString:="RepHold";
              WHEN PM:
                 statusString:="PM";
              WHEN PMhold:    
                 statusString:="pmHold";
              WHEN Idle:
                 statusString:="Idle";
              WHEN Standby:                  
                 statusString:="Standby";
              WHEN Done:
                 statusString:="Done";
              OTHERWISE             
                 NEW(message,1..1);
                 message[1]:="Unknown block status color!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                 DISPOSE(message);
           END CASE;
           commentString:=comment;
           IF activeStatus=Active
               actionString:=statusString;
               IF opStatus=Running
                  commentString:=commentString+"Stress="+REALTOSTR(PhaseStress);
               END IF;
           ELSE
              actionString:=activeString;
           END IF;
           IF (comment<>"Back_To_GUI")
              IF commentString<>""
                 WriteEvents(SimTime,"Block",name,actionString,commentString);
              ELSE
                 WriteEvents(SimTime,"Block",name,actionString,"change_block_state");
              END IF;   
           END IF;
        END IF;    {EventsFile}  
        
        
{chuck  11/22/04}        
    IF FEVmode 
        IF ((opStatus=Standby) AND (sbStress>0.0))
           ChangeRunningColor(sbStress);
        ELSIF ((opStatus=Running) AND (usesPhasing) AND (activePhases>0)) 
           ChangeRunningColor(phaseValue[phaseNumber]);
        END IF;
    END IF;
        
        
{ReconfigureBlock(IN RunChange,FailChange,IdleChange,StandbyChange,pmChange}      
{this section reflects the system changes caused by the block's new state}  
     IF NOT runCompleted    
        incFails:=FALSE;
        decFails:=FALSE;
        RunChange:=0;
        FailChange:=0;
        IdleChange:=0;
        StandbyChange:=0;
        pmChange:=0;
        IF (((oldOpStatus=Running) OR (oldOpStatus=Standby)) AND
              ((newOpStatus=Repairing) OR (newOpStatus=Done) OR (newOpStatus=Idle) 
                 OR (newOpStatus=PM) OR (newOpStatus=PMhold) OR (newOpStatus=RepHold)))
           incFails:=TRUE;
        ELSIF (((newOpStatus=Running) OR (newOpStatus=Standby)) AND
              ((oldOpStatus=Repairing) OR (oldOpStatus=Done) OR (oldOpStatus=Idle) 
                 OR (oldOpStatus=PM) OR (oldOpStatus=PMhold) OR (oldOpStatus=RepHold)))
              decFails:=TRUE;
        END IF;
        IF ((oldActiveStatus=Linked) AND (newActiveStatus=Linked))
           {do nothing}
        ELSIF ((oldActiveStatus=Cut) AND (newActiveStatus=Cut))
           {do nothing}        
        ELSIF ((oldActiveStatus=Cut) AND (newActiveStatus=Linked))
           RunChange:=1;
           FailChange:=-1;
        ELSIF ((oldActiveStatus=Linked) AND (newActiveStatus=Cut))
           RunChange:=-1;
           FailChange:=1;
        ELSIF (oldActiveStatus=Linked)     {now Active}
            IF (newOpStatus=Running)       
               {do nothing}
            ELSIF ((newOpStatus=Repairing) OR (newOpStatus=RepHold) OR (newOpStatus=Done))
               RunChange:=-1;
               FailChange:=1;
            ELSIF ((newOpStatus=PM) OR (newOpStatus=PMhold))
               RunChange:=-1;
               pmChange:=1;
            ELSIF (newOpStatus=Idle)
               RunChange:=-1;
               IdleChange:=1;
            ELSIF (newOpStatus=Standby)
               RunChange:=-1;
               StandbyChange:=1;
            END IF;
        ELSIF (oldActiveStatus=Cut)     {now Active}
            IF (newOpStatus=Running)       
               RunChange:=1;
               FailChange:=-1;
            ELSIF ((newOpStatus=Repairing) OR (newOpStatus=RepHold) OR (newOpStatus=Done))
               {do nothing}  
            ELSIF ((newOpStatus=PM) OR (newOpStatus=PMhold))
               FailChange:=-1;
               pmChange:=1;
            ELSIF (newOpStatus=Idle)
               FailChange:=-1;
               IdleChange:=1;
            ELSIF (newOpStatus=Standby)
               FailChange:=-1;
               StandbyChange:=1;
            END IF;
        ELSIF (oldOpStatus=Running)     
            IF (newActiveStatus=Linked)
               {do nothing}
            ELSIF (newActiveStatus=Cut)
               RunChange:=-1;
               FailChange:=1;
            ELSIF ((newOpStatus=Repairing) OR (newOpStatus=RepHold) OR (newOpStatus=Done))
               RunChange:=-1;
               FailChange:=1;
            ELSIF ((newOpStatus=PM) OR (newOpStatus=PMhold))
               RunChange:=-1;
               pmChange:=1;
            ELSIF (newOpStatus=Idle)
               RunChange:=-1;
               IdleChange:=1;
            ELSIF (newOpStatus=Standby)
               RunChange:=-1;
               StandbyChange:=1;
            END IF;
        ELSIF (oldOpStatus=Repairing)     
            IF (newActiveStatus=Linked)
               RunChange:=1;
               FailChange:=-1;
            ELSIF (newActiveStatus=Cut)
               {do nothing}  
            ELSIF (newOpStatus=Running)
               RunChange:=1;
               FailChange:=-1;
            ELSIF (newOpStatus=RepHold)
               {do nothing}  
            ELSIF (newOpStatus=Idle)
               FailChange:=-1;
               IdleChange:=1;
            ELSIF (newOpStatus=Standby)
               FailChange:=-1;
               StandbyChange:=1;
            END IF;
        ELSIF (oldOpStatus=RepHold)     
            IF (newActiveStatus=Linked)
               RunChange:=1;
               FailChange:=-1;
            ELSIF (newActiveStatus=Cut)
               {do nothing}  
            ELSIF (newOpStatus=Running)
               RunChange:=1;
               FailChange:=-1;
            ELSIF (newOpStatus=Repairing)
               {do nothing}  
            ELSIF (newOpStatus=Idle)
               FailChange:=-1;
               IdleChange:=1;
            ELSIF (newOpStatus=Standby)
               FailChange:=-1;
               StandbyChange:=1;
            END IF;
        ELSIF (oldOpStatus=PM)     
            IF (newActiveStatus=Linked)
               RunChange:=1;
               pmChange:=-1;
            ELSIF (newActiveStatus=Cut)
               FailChange:=1;
               pmChange:=-1;
            ELSIF (newOpStatus=Running)
               RunChange:=1;
               pmChange:=-1;
            ELSIF (newOpStatus=Idle)
               IdleChange:=1;
               pmChange:=-1;
            ELSIF (newOpStatus=Standby)
               StandbyChange:=1;
               pmChange:=-1;
            END IF;
        ELSIF (oldOpStatus=PMhold)     
            IF (newActiveStatus=Linked)
               RunChange:=1;
               pmChange:=-1;
            ELSIF (newActiveStatus=Cut)
               FailChange:=1;
               pmChange:=-1;
            ELSIF (newOpStatus=PM)
               {do nothing} 
            END IF;
        ELSIF (oldOpStatus=Idle)     
            IF (newActiveStatus=Linked)
               RunChange:=1;
               IdleChange:=-1;
            ELSIF (newActiveStatus=Cut)
               FailChange:=1;
               IdleChange:=-1;
            ELSIF (newOpStatus=Running)
               RunChange:=1;
               IdleChange:=-1;
            ELSIF ((newOpStatus=PM) OR (newOpStatus=PMhold))
               pmChange:=1;
               IdleChange:=-1;
            ELSIF (newOpStatus=Standby)
               IdleChange:=-1;
               StandbyChange:=1;
            END IF;
        ELSIF (oldOpStatus=Standby)     
            IF (newActiveStatus=Linked)
               RunChange:=1;
               StandbyChange:=-1;
            ELSIF (newActiveStatus=Cut)
               FailChange:=1;
               StandbyChange:=-1;
            ELSIF (newOpStatus=Running)
               RunChange:=1;
               StandbyChange:=-1;
            ELSIF ((newOpStatus=Repairing) OR (newOpStatus=RepHold) OR (newOpStatus=Done))
               FailChange:=1;
               StandbyChange:=-1;
            ELSIF ((newOpStatus=PM) OR (newOpStatus=PMhold))
               StandbyChange:=-1;
               pmChange:=1;
            ELSIF (newOpStatus=Idle)
               IdleChange:=1;
               StandbyChange:=-1;
            END IF;
        ELSIF (oldOpStatus=Done)     
            IF (newActiveStatus=Linked)
               RunChange:=1;
               FailChange:=-1;
            ELSIF (newActiveStatus=Cut)
               {do nothing}        
            END IF;
        END IF; 
        IF incFails
           INC(NumCurrentlyFailed);
           IF parentID>0              
              tempHier := ASK root Child("RBDHier", parentID);  
              ASK tempHier TO IncNumFailed;
           END IF;
        END IF;
        IF decFails
           DEC(NumCurrentlyFailed);
           IF parentID>0              
               tempHier := ASK root Child("RBDHier", parentID);  
               ASK tempHier TO DecNumFailed;
           END IF;
        END IF;
        IF ((oldActiveStatus=newActiveStatus) AND (oldActiveStatus<>Active))  
        {block's status changed while Cut or Linked)}
           IF (coldDep AND (newOpStatus=Running))  {Block may need to go to standby}
              tempNode := ASK window Descendant("RBDNode", EFPAnode);                       
              coldsAffected:=TRUE;
              IF NOT (ColdChangeGroup.Includes(tempNode)) 
                 ASK ColdChangeGroup TO Add(tempNode);
              END IF;
           END IF;
        END IF;
        IF (ABS(RunChange)+ABS(FailChange)+ABS(IdleChange)+ABS(StandbyChange)+ABS(pmChange)=2)
           ReconfigureBlock(RunChange,FailChange,IdleChange,StandbyChange,pmChange);
        ELSIF (ABS(RunChange)+ABS(FailChange)+ABS(IdleChange)+ABS(StandbyChange)+ABS(pmChange)<>0)
           NEW(message,1..1);
           message[1]:="Error2 in ChangeBlockState!     "; 
           result:=SendAlert(message,FALSE, FALSE, TRUE);
           DISPOSE(message);
        END IF;
        
        IF (incFails AND C130File)
             IF ((System.Status=GGreen) OR (System.Status=YYellow))                 
                outString:=INTTOSTR(Id)+"  "+REALTOSTR(SimTime)+"  "+"0";
             ELSE
                outString:=INTTOSTR(Id)+"  "+REALTOSTR(SimTime)+"  "+"1";
             END IF;
             ASK C130Stream TO WriteString(outString); 
             ASK C130Stream TO WriteLn; 
        END IF;        
     END IF;
  END METHOD;     {ChangeBlockState}

  ASK METHOD ChangeRunningColor      (IN newValue   :REAL);      
  VAR
     blockImage, innerSquare          : ImageObj;
     outerColor                       : ColorType;
  BEGIN
    blockImage := Descendant("BasicBlock", 601);            
    innerSquare:= Descendant("InnerSquare", 0);
    IF opStatus= Running
       IF newValue <1.0
         outerColor:=DarkGreen;
       ELSIF newValue =1.0
         outerColor:=LimeGreen;
       ELSIF newValue >1.0
         outerColor:=MediumSpringGreen;
       END IF;
    ELSIF opStatus=Standby   
       IF newValue=0.0
         outerColor:=Blue;
       ELSIF newValue <1.0
         outerColor:=SkyBlue;
       ELSE
         outerColor:=Turquoise;
       END IF;
    ELSE
         outerColor:=Plum;
    END IF;   
    ASK blockImage TO SetColor(outerColor);  
    IF activeStatus=Active
       ASK innerSquare TO SetColor(outerColor);  
    END IF;   
    ASK blockImage TO SetTranslation(blockImage.Translation.x,
                                                     blockImage.Translation.y);
    ASK blockImage TO Draw;
  END METHOD;

   ASK METHOD ShowAnalColor();
   VAR
      aoText, nameText                : TextObj;
      blockImage, innerSquare         : ImageObj;
      displayColor                    : ColorType;
      i                               : INTEGER;
      displayValue                    : REAL;
   BEGIN
      innerSquare:= Descendant("InnerSquare", 0);
      IF weakLinkAnalType=1 
         displayValue := STRTOREAL(Ao);
      ELSIF weakLinkAnalType=2 
         displayValue := STRTOREAL(Do);
      ELSE 
         displayValue := STRTOREAL(R);
      END IF;
      IF displayValue >= GYthreshold
         displayColor := Green;
      ELSIF displayValue >= YRthreshold
         displayColor := Yellow;
      ELSE
         displayColor := Red;
      END IF;
      aoText := Child("BlockAoText",0);
      nameText := Child("RBDBlockLabel", 0);
      blockImage := Child("BasicBlock", 601);      
      ASK aoText TO SetColor(Blue);
      ASK blockImage TO SetColor(displayColor);
      IF displayValue<0.9985
         ASK aoText TO SetText(SUBSTR(2,5,REALTOSigFigSTR(displayValue,3)));
      ELSIF displayValue=1.0
         ASK aoText TO SetText("1.00");
      ELSE
         ASK aoText TO SetText(".999");
      END IF;
      IF parentID = activeWindow
         ASK aoText TO SetHidden(FALSE);
         SetHidden(FALSE);
      END IF;
      IF (fontSize > 1) AND (displayValue >= 0.)
         ASK aoText TO SetHidden(FALSE);
         ASK aoText TO SetSysFont("SmallFonts",fontSize-1,70,0);
      ELSE
         ASK nameText TO SetHidden(TRUE);
         ASK aoText TO SetHidden(TRUE);
      END IF;
      IF parentID <> activeWindow
         ASK aoText TO SetHidden(TRUE);
         SetHidden(TRUE); 
      END IF;
      ASK innerSquare TO SetColor(displayColor);   {necessary to insure printing works}
      ASK innerSquare TO SetHidden(TRUE);  
      Draw;
   END METHOD;

  ASK METHOD ChangePhases     (IN oldVal,newVal                   : REAL;
                               IN oldType,newType                 : STRING);
  BEGIN
     IF ((oldType="C") AND (newType="L"))      {Cut to Link}
        ChangeBlockState(opStatus,Linked,"Phase_Change");              
     ELSIF ((oldType="C") AND (newType="A"))   {Cut to Active}           
        ChangeBlockState(opStatus,Active,"Phase_Change");
     ELSIF ((oldType="C") AND (newType="C"))   {Cut to Cut}
           {do nothing};
     ELSIF ((oldType="L") AND (newType="C")) {Link to Cut}
        ChangeBlockState(opStatus,Cut,"Phase_Change");              
     ELSIF ((oldType="L") AND (newType="A"))   {Link to Active}
        ChangeBlockState(opStatus,Active,"Phase_Change");
     ELSIF ((oldType="L") AND (newType="L"))
           {do nothing}
     ELSIF ((oldType="A") AND (newType="C"))  {Active to Cut}
        ChangeBlockState(opStatus,Cut,"Phase_Change");
     ELSIF ((oldType="A") AND (newType="L"))    {Active to link}
        ChangeBlockState(opStatus,Linked,"Phase_Change");
     ELSIF ((oldType="A") AND (newType="A")) 
         {do nothing}
         IF (FEVmode AND (opStatus=Running))    {Chuck 11/22/04}
           ChangeRunningColor(phaseValue[phaseNumber]);
         END IF;
     END IF; 
     IF FailWait
        InterruptReason:="Phase_Change";
        Interrupt(SELF,"Run");
     ELSIF ZeroWait
        InterruptReason:="Phase_Change";
        ASK BlockTrigger[seqNum] TO Release;
     END IF;
  END METHOD;
   
   ASK METHOD fevInitialize;
   VAR
      blockImage,innerSquare             : ImageObj;
      tempImage                          : FillVObj;
     BEGIN        
        GoodPathsRequired:=1;
        MinGoodPathsReq:=1;
        sysDepend:=FALSE;
        locDepend:=FALSE;
        CASE DependencyNum
           WHEN -2:
              sysDepend:=TRUE;
              simDependencyNum:=endId;
           WHEN -1:
              locDepend:=TRUE;
              simDependencyNum:=EFPAnode;
           OTHERWISE
              simDependencyNum:=DependencyNum;
        END CASE;
        IF (simDependencyNum>0)
           ASK BlockDepGroup TO Add(SELF);
        END IF;
        coldDep:=FALSE;
        blockImage := Descendant("BasicBlock", 601);            
        tempImage:=Child("BasicBlock",601);                                    
        IF ((activePhases>0) AND (usesPhasing))  
           ASK tempImage TO SetStyle(SolidFill);
           IF parentID=0;
              innerSquare:= Descendant("InnerSquare", 0);          
              ASK innerSquare TO SetHidden(FALSE);  
           END IF;
        END IF;
        ASK blockImage TO SetColor(LimeGreen);
        ASK blockImage TO SetTranslation(blockImage.Translation.x,blockImage.Translation.y);
        ASK blockImage TO Draw;
        startCond:="Running";        
{fevphasing}        
  END METHOD;
  
  ASK METHOD fevCleanUp;
  VAR
     blockImage, innerSquare    : ImageObj;
     tempImage                  : FillVObj;
  BEGIN   
     blockImage := Descendant("BasicBlock", 601);            
     innerSquare:= Descendant("InnerSquare", 0);
     ASK innerSquare TO SetColor(blockGUIColor);
     ASK innerSquare TO SetHidden(TRUE);
     ASK blockImage TO SetColor(blockGUIColor);
     ASK blockImage TO SetTranslation(blockImage.Translation.x,blockImage.Translation.y);
     tempImage:=Child("BasicBlock",601);                                    
     IF ((usesPhasing) AND (phaseValue <> NILARRAY))                                   
        ASK tempImage TO SetStyle(NarrowCrosshatchFill);
     ELSE
        ASK tempImage TO SetStyle(SolidFill);
     END IF;
     Draw;
  END METHOD;

  ASK METHOD fevReset;
     VAR 
        blockImage,innerSquare :   ImageObj;
     BEGIN
        stateStartTime:=0.0;
        NumGoodPaths:=EconnectIntoNum; 
        NumActivePaths:=EconnectIntoNum; 
        GoodPathsRequired:=1;
        FailureCounter:=0;
        discardedFailures:=0;
        activeStatus:=Active;
        FailWait:=FALSE;
        RepWait:=FALSE;
        PMWait:=FALSE;
        ZeroWait:=FALSE;
        IgnoreDep:=FALSE;
        opStatus:=Running;
        IF FevDepMode
           SetFevDepMode(FALSE);
        END IF;
        IF GraphicsOutput
           innerSquare:= Descendant("InnerSquare", 0); 
           ASK innerSquare TO SetColor(LimeGreen);
           ASK innerSquare TO SetHidden(TRUE);
           blockImage := Descendant("BasicBlock", 601);            
           ASK blockImage TO SetColor(LimeGreen);
           ASK blockImage TO SetTranslation(blockImage.Translation.x,
                                       blockImage.Translation.y);                                       
           ASK blockImage TO Draw;
        END IF;
     END METHOD; 

  ASK METHOD fevStartCond;
  BEGIN
     IF ((usesPhasing) AND (activePhases>0))         
        IF (phaseType[1]="L")
           ChangeBlockState(Running,Linked,"Initialize");
        ELSIF (phaseType[1]="C")       
           ChangeBlockState(Running,Cut,"Initialize");
        ELSE
           ChangeBlockState(Running,Active,"Initialize");
        END IF;
     ELSE
        ChangeBlockState(Running,Active,"Initialize");
     END IF;
  END METHOD;
   
   ASK METHOD SetFevDepMode           (IN newValue   : BOOLEAN);
   VAR
      blockImage,innerSquare     : ImageObj;
      tempImage                  : FillVObj;
   BEGIN
      FevDepMode:=newValue;
      IF GraphicsOutput
         tempImage:=Child("BasicBlock",601);                                    
         IF FevDepMode
            ASK tempImage TO SetStyle(NarrowCrosshatchFill);
            ASK tempImage TO Draw;
         ELSE
            ASK tempImage TO SetStyle(SolidFill);
            ASK tempImage TO Draw;
         END IF;
      END IF;
   END METHOD;  {SetFevDepMode}
     
   ASK METHOD SetInitName            (IN newValue   :STRING);
   VAR
      blockLabel : TextObj;
      innerSquare : ImageObj;
   BEGIN
      name:=newValue;
      ToolTip := newValue;
      innerSquare:= Descendant("InnerSquare", 0);
      ASK innerSquare TO SetHidden(TRUE);                            
      blockLabel := Child("RBDBlockLabel", 0);
      ASK blockLabel TO SetText(newValue);
      IF fontSize = 1
         ASK blockLabel TO SetSysFont("SmallFonts",fontSize,70,0);
         ASK blockLabel TO SetHidden(TRUE);
      ELSE
         ASK blockLabel TO SetSysFont("SmallFonts",fontSize,70,0);
      END IF;
      ASK blockLabel TO Draw;
   END METHOD;
      
   ASK METHOD SetsimStartType         (IN newValue   :INTEGER);
   BEGIN
      simStartType:=newValue;
   END METHOD;
   
   ASK METHOD SetamountExhausted      (IN newValue   :REAL);      
   BEGIN
      amountExhausted:=newValue;
   END METHOD;
   
   ASK METHOD SetfailDistro           (IN newValue   :INTEGER);
   BEGIN
      failDistro:=newValue;
   END METHOD;
   
   ASK METHOD SetnumFailParams           (IN newValue   :INTEGER);
   BEGIN
      numFailParams:=newValue;
   END METHOD;
   
   ASK METHOD SetfailVals             (IN newVals    :realArray);
   VAR
     i  : INTEGER;
   BEGIN
      DISPOSE(failVals);
      NEW(failVals, 1..numFailParams);
      FOR i := 1 TO numFailParams
         failVals[i] := newVals[i];
      END FOR;
   END METHOD;
   
   ASK METHOD SetfailStream           (IN newValue   :INTEGER);
   BEGIN
      failStream:=newValue;
   END METHOD;
   
   ASK METHOD SetrepairDistro         (IN newValue   :INTEGER);
   BEGIN
      repairDistro:=newValue;
   END METHOD;
   
   ASK METHOD SetnumRepairParams           (IN newValue   :INTEGER);
   BEGIN
      numRepairParams:=newValue;
   END METHOD;
   
   ASK METHOD SetrepairVals           (IN newVals    :realArray);
   VAR
     i  : INTEGER;
   BEGIN
      DISPOSE(repairVals);
      NEW(repairVals, 1..numRepairParams);
      FOR i := 1 TO numRepairParams
         repairVals[i] := newVals[i];
      END FOR;      
   END METHOD;
   
   ASK METHOD SetrepairStream         (IN newValue   :INTEGER);
   BEGIN
      repairStream:=newValue;
   END METHOD;
   
   ASK METHOD SetnumDiffRes           (IN newValue   :INTEGER);
   BEGIN
      numDiffRes:=newValue;
   END METHOD;
   
   ASK METHOD SetnumRes1              (IN newValue   :INTEGER);
   BEGIN
      numRes1:=newValue;
   END METHOD;
   
   ASK METHOD SetnumRes1PM              (IN newValue   :INTEGER);
   BEGIN
      numRes1PM:=newValue;
   END METHOD;

   ASK METHOD Setres1Name             (IN newValue   :STRING);
   BEGIN
      res1Name:=newValue;
   END METHOD;
   
   ASK METHOD SetinfiniteSpares       (IN newValue   :BOOLEAN);
   BEGIN
      infiniteSpares:=newValue;
   END METHOD;
   
   ASK METHOD SetsparingType          (IN newValue   :SparingType);
   BEGIN
      sparingType:=newValue;
   END METHOD;
   
   ASK METHOD SetroutineSpareOrdering (IN newValue   :BOOLEAN);
   BEGIN
      routineSpareOrdering:=newValue;
   END METHOD;
   
   ASK METHOD SetinitStock            (IN newValue   :INTEGER);
   BEGIN
      initStock:=newValue;
   END METHOD;
   
   ASK METHOD SetnewSpares            (IN newValue   :INTEGER);
   BEGIN
      newSpares:=newValue;
   END METHOD;
   
   ASK METHOD SetarrivalRate          (IN newValue   :REAL);      
   BEGIN
      arrivalRate:=newValue;
   END METHOD;
   
   ASK METHOD SetemerSpareOrdering    (IN newValue   :BOOLEAN);
   BEGIN
      emerSpareOrdering:=newValue;
   END METHOD;
   
   ASK METHOD SetemerTime             (IN newValue   :REAL);      
   BEGIN
      emerTime:=newValue;
   END METHOD;
   
   ASK METHOD SetstockLevelOrdering   (IN newValue   :BOOLEAN);
   BEGIN
      stockLevelOrdering:=newValue;
   END METHOD;
   
   ASK METHOD SetSLOOrderLevel        (IN newValue   :INTEGER);
   BEGIN
      SLOOrderLevel:=newValue;
   END METHOD;
   
   ASK METHOD SetSLONewSpares         (IN newValue   :INTEGER);
   BEGIN
      SLONewSpares:=newValue;
   END METHOD;
   
   ASK METHOD SetSLOTime              (IN newValue   :REAL);      
   BEGIN
      SLOTime:=newValue;
   END METHOD;
   
   ASK METHOD SetGDType               (IN newValue   :INTEGER); 
   BEGIN
      GDType:=newValue;     {1=linear,2=geometric, 3=asymptotic}
   END METHOD;
   
   ASK METHOD SetGDRate               (IN newValue   :REAL);      
   BEGIN
      GDRate:=newValue;
   END METHOD;
   
   ASK METHOD SetGDLimit              (IN newValue   :REAL);      
   BEGIN
      GDLimit:=newValue;
   END METHOD;
   
   ASK METHOD SetpoolName             (IN newValue   :STRING);
   BEGIN
      poolName:=newValue;
   END METHOD;
   
   ASK METHOD SetinitialCost          (IN newValue   :REAL);      
   BEGIN
      initialCost:=newValue;
   END METHOD;
   
   
   ASK METHOD SetOperatingCost         (IN newValue   :REAL);      
   BEGIN
      operatingCost:=newValue;
   END METHOD;


   ASK METHOD SetstandbyCost          (IN newValue   :REAL);      
   BEGIN
      standbyCost:=newValue;
   END METHOD;
   
   ASK METHOD SetidleCost             (IN newValue   :REAL);      
   BEGIN
      idleCost:=newValue;
   END METHOD;

   ASK METHOD SetrepHoldCost             (IN newValue   :REAL);      
   BEGIN
      repHoldCost:=newValue;
   END METHOD;
   
   ASK METHOD SetpmHoldCost            (IN newValue   :REAL);      
   BEGIN
      pmHoldCost:=newValue;
   END METHOD;
      
   ASK METHOD SetpmFixedCost            (IN newValue   :REAL);      
   BEGIN
      pmFixedCost:=newValue;
   END METHOD;
      
   ASK METHOD SetspareCost            (IN newValue   :REAL);      
   BEGIN
      spareCost:=newValue;
   END METHOD;
   
   ASK METHOD SetrepairingCost        (IN newValue   :REAL);      
   BEGIN
      repairingCost:=newValue;
   END METHOD;
   
   ASK METHOD SetrepFixedCost         (IN newValue   :REAL);      
   BEGIN
      repFixedCost:=newValue;
   END METHOD;
   
   ASK METHOD SetdoneCost             (IN newValue   :REAL);      
   BEGIN
      doneCost:=newValue;
   END METHOD;
   
   ASK METHOD SetdoneFixedCost        (IN newValue   :REAL);      
   BEGIN
      doneFixedCost:=newValue;
   END METHOD;
   
   ASK METHOD SetemerShippingCost     (IN newValue   :REAL);      
   BEGIN
      emerShippingCost:=newValue;
   END METHOD;
      
   ASK METHOD SetalwaysAddDoneCost    (IN newValue   :BOOLEAN);
   BEGIN
      alwaysAddDoneCost:=newValue;
   END METHOD;
       
   ASK METHOD SetSBstress               (IN newValue   :REAL);  
    BEGIN
      sbStress:=newValue;
   END METHOD;
   
   ASK METHOD SetpreDist              (IN newValue   :INTEGER);   
    BEGIN
      preDist:=newValue;
   END METHOD;
   
   ASK METHOD SetpreParams            (IN newVals    :realArray);
   VAR
     i,numParams  : INTEGER;
   BEGIN
      DISPOSE(preParams);
      GetNumParams(preDist,numParams);
      NEW(preParams, 1..numParams);
      FOR i := 1 TO numParams
         preParams[i] := newVals[i];
      END FOR;
   END METHOD;
   
   ASK METHOD SetpostDist              (IN newValue   :INTEGER);   
   BEGIN
      postDist:=newValue;
   END METHOD;
   
   ASK METHOD SetpostParams            (IN newVals    :realArray);
   VAR
     i,numParams  : INTEGER;
   BEGIN
      DISPOSE(postParams);
      GetNumParams(postDist,numParams);
      NEW(postParams, 1..numParams);
      FOR i := 1 TO numParams
         postParams[i] := newVals[i];
      END FOR;
   END METHOD;

   ASK METHOD SetUsesPM                 (IN newValue   :BOOLEAN);
   BEGIN
      usesPM:=newValue;
   END METHOD;
   
   ASK METHOD SetpmSpare                (IN newValue   :BOOLEAN);
   BEGIN
      pmSpareNeeded:=newValue;
   END METHOD;
   
   ASK METHOD SetpmRefresh              (IN newValue   :BOOLEAN);
   BEGIN
      pmRefresh:=newValue;
   END METHOD;
   
   ASK METHOD SetpmMisDefer             (IN newValue   :BOOLEAN);
   BEGIN
      pmMisDefer:=newValue;
   END METHOD;
   
   ASK METHOD SetpmFailReset            (IN newValue   :BOOLEAN);
   BEGIN
      pmFailReset:=newValue;
   END METHOD;

   ASK METHOD SetpmReqDefer             (IN newValue   :BOOLEAN);
   BEGIN
      pmReqDefer:=newValue;
   END METHOD;
      
   ASK METHOD SetpmTriggered          (IN newValue   :BOOLEAN);
   BEGIN
      pmTriggered:=newValue;
   END METHOD;
     
   ASK METHOD SetpmTrig               (IN newValue   :STRING);
   BEGIN
      pmTrig:=newValue;
   END METHOD;
   
   ASK METHOD SetpmFreq               (IN newValue   :REAL);
   BEGIN
      pmFreq:=newValue;
   END METHOD;

   ASK METHOD SetpmStagger              (IN newValue   :REAL);     
   BEGIN
      pmStagger:=newValue;
   END METHOD;
       
   ASK METHOD SetopStatus               (IN newValue   :STRING);      
   BEGIN
      IF newValue="Running"
         opStatus:=Running;
      ELSIF newValue="Repairing"
         opStatus:=Repairing;
      END IF;
   END METHOD;
   
   ASK METHOD SetpmDist                 (IN newValue   :INTEGER); 
   BEGIN
      pmDist:=newValue;
   END METHOD;      

   ASK METHOD SetpmParams               (IN newValue   :realArray);
   VAR
     i,numParams  : INTEGER;
   BEGIN
      DISPOSE(pmParams);
      GetNumParams(pmDist,numParams);
      NEW(pmParams, 1..numParams);
      FOR i := 1 TO numParams
         pmParams[i] := newValue[i];
      END FOR;
   END METHOD;

   ASK METHOD SetpmCost                 (IN newValue   :REAL);      
   BEGIN
      pmCost:=newValue;
   END METHOD; 
   
   ASK METHOD SetInterruptReason      (IN newValue   :STRING);
   BEGIN
      InterruptReason:=newValue;
   END METHOD; 
   
   ASK METHOD SetDeferPM                 (IN newValue   :BOOLEAN);
   BEGIN
      deferPM:=newValue;
   END METHOD; 
   
   ASK METHOD SetDepVals              (IN val1,val2,val3,val4,val5  : REAL;
                                       IN bool1                     : BOOLEAN);
   BEGIN
      DepNothingPerc:=val1;
      DepIdlePerc:=val2;
      DepPMPerc:=val3;
      DepFailPerc:=val4;
      DepPMThreshold:=val5;
      defDepStateIdle:=bool1;
   END METHOD; 
   
   ASK METHOD SetreturnFromMaint       (IN newValue   : BOOLEAN);
   BEGIN
      returnFromMaint:=newValue;
   END METHOD; 
 
   TELL METHOD ControlPMtriggers;
   VAR
      trig,myTrig :    RapTriggerObj;
   BEGIN
      FOREACH trig IN triggerGroup
         IF trig.TrigName=pmTrig
            myTrig:=trig;
         END IF;
      END FOREACH;
      LOOP
         WAIT FOR myTrig TO Fire;
         
          IF NOT ((myTrig.TrigName="BMDS-ShutdoWn") AND (opStatus=Running))     {cmc BMDS}
                 
            IF ((NOT WaitingForPrereqs) AND (NOT deferPM))
               IF ((activePhases>0) AND (pmMisDefer))
                  WAIT DURATION 0.0;
                  END WAIT;
                  IF PhaseChangeInProgress
                     WAIT FOR pmSyncTrigger TO Fire;
                     END WAIT;
                  END IF;
               END IF;            
               IF ((opStatus=Running) OR (opStatus=Idle) OR (opStatus=Standby))
                  InterruptReason:="Triggered_PM_Due";
                  IF FailWait
                     Interrupt(SELF,"Run");
                  ELSIF ZeroWait
                     ASK BlockTrigger[seqNum] TO Release;
                  END IF;
               END IF; 
            END IF;
          END IF;     {cmc BMDS} 
            
         ON INTERRUPT
         END WAIT;
         
         
      END LOOP;
   END METHOD; 
   
  ASK METHOD TurnOffStartCond;
  BEGIN
     startCond:="Off";
  END METHOD;

  ASK METHOD SetDefaultBlockName     (IN newValue   : STRING);
  BEGIN
     name:="newValue";
  END METHOD;
  
  
  ASK METHOD SetPrevDepState         (IN newValue   : STRING);
  BEGIN
     prevDepState:=newValue;
  END METHOD;

  ASK METHOD SetTzeroAction         (IN newValue   : STRING);        {cmc 12/13/06}
  BEGIN
     TzeroAction:=newValue;
  END METHOD;

  
END OBJECT; {RBDBlockObj}

OBJECT RBDEventObj; 
     
   TELL METHOD InitTimeZeroDeps;   
   BEGIN
       BackFlowDependencies(Down);   
   END METHOD;
     
     
  ASK METHOD SetEventData        (INOUT usesPhase        : BOOLEAN; 
                                  INOUT fDist,fStrm      : INTEGER;
                                  INOUT opCost,repCost,initCost,fVal : REAL; 
                                  INOUT eName,comm       : STRING);
  VAR
     eventLabel              : TextObj;
     eventImage,innerSquare  : ImageObj;
  BEGIN
     failDistro           := fDist;
     failStream           := fStrm;
     initialCost          := initCost;
     operatingCost        := opCost;
     repairingCost        := repCost;
     name                 := eName;
     comment              := comm;
     SetPhases(usesPhase,phaseValue,phaseType);  
     DISPOSE(failVals);
     NEW(failVals, 1..1);
     failVals[1] := fVal;
     eventLabel := Child("RBDEventLabel", 0);
     ASK eventLabel TO SetText(name);
     IF fontSize = 1
        ASK eventLabel TO SetSysFont("SmallFonts",fontSize,70,0);
        ASK eventLabel TO SetHidden(TRUE);
     ELSE
        ASK eventLabel TO SetSysFont("SmallFonts",fontSize,70,0);
     END IF;
     innerSquare:= Descendant("InnerSquare", 0);
     ASK innerSquare TO SetHidden(TRUE);                      
     ASK eventLabel TO Draw;
     ToolTip := name;
  END METHOD; {SetEventData}
      
  ASK METHOD SetNewEventParams(INOUT fVal : REAL);
  VAR
  BEGIN
     failVals[1] := fVal;
  END METHOD;
      
  ASK METHOD GetCloneOf(IN cloner  : RBDEventObj;
                        IN copying : BOOLEAN);
  VAR
     eventLabel            : TextObj;
     innerSquare           : ImageObj;
  BEGIN
     failDistro           := cloner.failDistro;
     failStream           := cloner.failStream;
     name                 := cloner.name;
     initialCost          := cloner.initialCost;
     repairingCost        := cloner.repairingCost;
     operatingCost        := cloner.operatingCost;
     comment              := cloner.comment;
     SetPhases(cloner.usesPhasing,cloner.phaseValue,cloner.phaseType);
     NEW(failVals,   1..1);
     failVals[1] := cloner.failVals[1];
     IF copying
        eventLabel := Child("RBDEventLabel", 0);
        ASK eventLabel TO SetText(name);
        IF fontSize = 1
           ASK eventLabel TO SetSysFont("SmallFonts",fontSize,70,0);
           ASK eventLabel TO SetHidden(TRUE);
        ELSE
           ASK eventLabel TO SetSysFont("SmallFonts",fontSize,70,0);
        END IF;
        ASK eventLabel TO Draw;
     END IF;
     innerSquare:= Descendant("InnerSquare", 0);
     ASK innerSquare TO SetHidden(TRUE);
  END METHOD; {GetCloneOf}
      
  ASK METHOD SetConnectToNode(IN isConnected : BOOLEAN);
  BEGIN
     isConnectedNode := isConnected;
  END METHOD; {SetConnectToNode}
      
  ASK METHOD CopyRestOfData(IN intoNum, outOfNum : INTEGER;
                            IN xPos, yPos        : REAL);
  BEGIN
     xPosition := xPos;
     yPosition := yPos;
     connectIntoNum := intoNum;
     connectOutOfNum := outOfNum;
  END METHOD; {CopyRestOfData}
      
  ASK METHOD MassEditEvent(IN  changeSP, changeP, useP, changeRS, changeName : BOOLEAN;
                           IN  newRS                                         : INTEGER;
                           IN  newSP                                         : REAL;
                           IN  nameChangeType, newName                       : STRING;
                           OUT namemsg                                       : BOOLEAN);
  VAR
     i          : INTEGER;
     tempName   : STRING;
     eventLabel : TextObj;
  BEGIN   
     IF changeSP AND (failVals[1] <> newSP)  {change success probability}
        failVals[1]:=newSP;
        somethingChanged := TRUE;
     END IF;
     IF changeP AND (useP <> usesPhasing)
        SetPhases(useP,NILARRAY,NILARRAY);
        somethingChanged := TRUE;
     END IF;
     IF ((changeRS) AND (failStream <> newRS)) {change Random Stream}
        failStream:=newRS;
        somethingChanged := TRUE;
     END IF;
     IF changeName
        IF nameChangeType = "prefix"
           tempName := newName + name;
        ELSIF nameChangeType = "suffix"
           tempName := name + newName;
        ELSIF nameChangeType = "replace"
           tempName := newName;
        END IF;
        IF STRLEN(tempName) > 20
           namemsg := TRUE;
        ELSE
           name := tempName;
           eventLabel := Child("RBDEventLabel", 0);
           ASK eventLabel TO SetText(name);
        END IF;
     END IF;
  END METHOD;    {MassEditEvent}
      
  ASK METHOD SetPhases        (IN phasing                         : BOOLEAN; 
                               IN arrayR                          : realArray;
                               IN arrayS                          : strArray);
  VAR
     i         : INTEGER;
     tempImage : FillVObj;
     tempReals : realArray;
     tempString: strArray;
  BEGIN
     tempImage := Child("BasicBlock",601);
     usesPhasing := phasing;
     IF (activePhases > 0) AND usesPhasing
        DISPOSE(tempReals);
        DISPOSE(tempString);
        IF arrayR <> NILARRAY
           NEW(tempReals,1..HIGH(arrayR));
           NEW(tempString,1..HIGH(arrayR));
           FOR i:=1 TO HIGH(arrayR);
              tempReals[i]:=arrayR[i];
              tempString[i]:=arrayS[i];
           END FOR;  
        END IF;
        IF phaseValue<>NILARRAY {checking to set somethingChanged}
           IF NOT somethingChanged
              IF arrayR <> NILARRAY
                 IF HIGH(arrayR) = HIGH(phaseValue)
                    FOR i := 1 TO HIGH(phaseValue)
                       IF phaseValue[i] <> arrayR[i]
                          somethingChanged := TRUE;
                       END IF;
                       IF phaseType[i] <> arrayS[i]
                          somethingChanged := TRUE;
                       END IF;
                    END FOR;
                 ELSE
                    somethingChanged := TRUE;
                 END IF;
              END IF;
           END IF;
           DISPOSE(phaseValue);
           DISPOSE(phaseType);
        ELSIF arrayR <> NILARRAY
           somethingChanged:=TRUE;
        END IF; {phaseValue<>NILARRAY }
        NEW(phaseValue,1..activePhases);
        NEW(phaseType,1..activePhases);
        IF tempReals <> NILARRAY
           FOR i:=1 TO activePhases
              phaseValue[i]:=tempReals[i];
              phaseType[i]:=tempString[i];
           END FOR;
        ELSIF activePhases > 0
              FOR i:=1 TO activePhases
                 phaseType[i]:="P";
              END FOR;
              phaseType[1]:="F";
        END IF;
        ASK tempImage TO SetStyle(NarrowCrosshatchFill);
     ELSE
        ASK tempImage TO SetStyle(SolidFill);
     END IF;   {(activePhases > 0) AND usesPhasing}
     IF (typeOfCursor <> blockC) AND (NOT resetting)
        ASK tempImage TO Draw;
        SetHighlighted(TRUE);
        SetHighlighted(FALSE);
        Draw;
     END IF;
  END METHOD;
      
  ASK METHOD ReconfigureEvent (IN RunChange,FailChange            : INTEGER);  
     VAR
        tempNode                                  : RBDNodeObj;
     BEGIN
     IF ((hasDepObjects) AND (simInProgress))
        IF (FailChange=1)  
           BackFlowDependencies(Down);
        ELSIF (RunChange=1)
           BackFlowDependencies(AllUp);
        END IF;
     END IF;
     tempNode := ASK window Descendant("RBDNode", EFPAnode);                       
     ASK tempNode TO ReconfigureNode(RunChange,FailChange,0,0,0,EFPAlink);  
  END METHOD;                      
     
  ASK METHOD ReconfigureSystem(IN inPath: INTEGER);
     VAR
        tempnode                      : RBDNodeObj;
     BEGIN
        NumActivePaths:=NumActivePaths-1; 
        tempnode := ASK window Descendant("RBDNode", EFPAnode);              
        ASK tempnode TO ReconfigureSystem(EFPAlink);        
  END METHOD;      {ReconfigureSystem}     
            
  ASK METHOD ChangeEventState (IN newOpStatus                     : EventStatusType;
                               IN newActiveStatus                 : ActiveStatusType;
                               IN comment                         : STRING);      
  VAR
     eventImage,innerSquare      : ImageObj;
     oldOpStatus                 : EventStatusType;
     stateTime                   : REAL;
     activeString,statusString,
     actionString,commentString  : STRING;
     oldActiveStatus             : ActiveStatusType; 
     outerColor, innerColor      : ColorType;
     incFails,decFails           : BOOLEAN;
     tempHier                    : RBDHierObj;
     RunChange,FailChange        : INTEGER;
  BEGIN
     oldOpStatus:=opStatus;
     oldActiveStatus:=activeStatus;
     opStatus:=newOpStatus; 
     activeStatus:=newActiveStatus;
     SystemStateChange:=TRUE;
     IF GraphicsOutput
        innerSquare:= Descendant("InnerSquare", 0);
        eventImage := Descendant("BasicBlock", 601);            
        CASE opStatus
           WHEN Armed:
              outerColor:=Cyan;
           WHEN Success: 
              outerColor:=LimeGreen;
           WHEN Failure:        
              outerColor:=Red;
           OTHERWISE             
              NEW(message,1..1);
              message[1]:="Unknown event status color!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
             DISPOSE(message);
        END CASE;
        ASK eventImage TO SetColor(outerColor);
        ASK eventImage TO SetTranslation(eventImage.Translation.x,eventImage.Translation.y);
        CASE activeStatus
           WHEN Linked:
              innerColor:=White;
           WHEN Cut:                                     
              innerColor:=Black;
           WHEN Active:                                     
              innerColor:=outerColor;
           OTHERWISE
                 NEW(message,1..1);
                 message[1]:="Unknown event active status color!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                 DISPOSE(message);
        END CASE;
        ASK innerSquare TO SetColor(innerColor);
        IF ((oldActiveStatus=Active) AND (newActiveStatus<>Active) AND (parentID=activeWindow))
           ASK innerSquare TO SetHidden(FALSE); 
        ELSIF ((oldActiveStatus<>Active) AND (newActiveStatus=Active) AND (parentID=activeWindow))
           ASK innerSquare TO SetHidden(TRUE); 
        END IF;
        Draw;
     END IF;
     stateTime:=SimTime-stateStartTime;
     IF weakAnalysis
        CASE oldOpStatus
           WHEN Armed:
              EventStandbyTime[seqNum]:=EventStandbyTime[seqNum]+stateTime;
           WHEN Success: 
              EventRunTime[seqNum]:=EventRunTime[seqNum]+stateTime;
           WHEN Failure:        
              EventRepairTime[seqNum]:=EventRepairTime[seqNum]+stateTime;
           OTHERWISE             
              NEW(message,1..1);
              message[1]:="Unknown event status for Weak Link Analysis!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message);
        END CASE; 
        IF System.missionStatus=Mission
           IF (newOpStatus=Failure)
              IF comment<>"End_sim"
                 eventMissionBool[seqNum]:=FALSE;
              END IF;   
           END IF;    
        END IF;    
     END IF;
     stateStartTime:=SimTime; 
     IF EventsFile  
        CASE activeStatus
           WHEN Cut:
              activeString:="Cut";
           WHEN Linked:
              activeString:="Linked";
           WHEN Active:
              activeString:="Active";
        END CASE;
        CASE opStatus
           WHEN Armed:
              statusString:="Armed";
           WHEN Success: 
              statusString:="Success";
           WHEN Failure:        
              statusString:="Failed";
           OTHERWISE             
              NEW(message,1..1);
              message[1]:="Unknown event status color!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message);
        END CASE;
        commentString:=comment;
        IF activeStatus=Active
           actionString:=statusString;
        ELSE
           actionString:=activeString;
        END IF;
        IF (comment<>"Back_To_GUI")
           IF (commentString<>"")
              WriteEvents(SimTime,"Event",name,actionString,commentString);
           ELSE
              WriteEvents(SimTime,"Event",name,actionString,"change_event_state");
           END IF;
        END IF;
     END IF;    {EventsFile}  
{this section reflects the system changes caused by the block's new state}  
     IF NOT runCompleted    
        incFails:=FALSE;
        decFails:=FALSE;
        RunChange:=0;
        FailChange:=0;
        IF (((oldOpStatus=Armed) OR (oldOpStatus=Success)) AND (newOpStatus=Failure))
           incFails:=TRUE;
        ELSIF (((newOpStatus=Armed) OR (newOpStatus=Success)) AND (oldOpStatus=Failure))
              decFails:=TRUE;
        END IF;
        IF ((oldActiveStatus=Linked) AND (newActiveStatus=Linked))
           {do nothing}
        ELSIF ((oldActiveStatus=Cut) AND (newActiveStatus=Cut))
           {do nothing}        
        ELSIF ((oldActiveStatus=Cut) AND (newActiveStatus=Linked))
           RunChange:=1;
           FailChange:=-1;
        ELSIF ((oldActiveStatus=Linked) AND (newActiveStatus=Cut))
           RunChange:=-1;
           FailChange:=1;
        ELSIF (oldActiveStatus=Linked)     {now Active}
            IF ((newOpStatus=Running) OR (newOpStatus=Armed) OR (newOpStatus=Success))        
               {do nothing}
            ELSIF (newOpStatus=Failure)
               RunChange:=-1;
               FailChange:=1;
            END IF;
        ELSIF (oldActiveStatus=Cut)     {now Active}
            IF ((newOpStatus=Armed) OR (newOpStatus=Success))        
               RunChange:=1;
               FailChange:=-1;
            ELSIF (newOpStatus=Failure)
               {do nothing}  
            END IF;
        ELSIF (oldOpStatus=Armed)     
            IF (newActiveStatus=Linked)
               {do nothing}        
            ELSIF (newActiveStatus=Cut)
               RunChange:=-1;
               FailChange:=1;
            ELSIF (newOpStatus=Success)
               {do nothing}
            ELSIF (newOpStatus=Failure)
               RunChange:=-1;
               FailChange:=1;
            END IF;
        ELSIF (oldOpStatus=Success)     
            IF (newActiveStatus=Linked)
               {do nothing}        
            ELSIF (newActiveStatus=Cut)
               RunChange:=-1;
               FailChange:=1;
            ELSIF (newOpStatus=Failure)
               RunChange:=-1;
               FailChange:=1;
            END IF;
        ELSIF (oldOpStatus=Failure)     
            IF (newActiveStatus=Linked)
               RunChange:=1;
               FailChange:=-1;
            ELSIF (newActiveStatus=Cut)
               {do nothing}
            ELSIF (newOpStatus=Success)
               RunChange:=1;
               FailChange:=-1;
            END IF;
        END IF; 
        IF incFails
           INC(NumCurrentlyFailed);
           IF parentID>0              
              tempHier := ASK root Child("RBDHier", parentID);  
              ASK tempHier TO IncNumFailed;
           END IF;
        END IF;
        IF decFails
           DEC(NumCurrentlyFailed);
           IF parentID>0              
               tempHier := ASK root Child("RBDHier", parentID);  
               ASK tempHier TO DecNumFailed;
           END IF;
        END IF;
        IF (ABS(RunChange)+ABS(FailChange)=2)
           ReconfigureEvent(RunChange,FailChange);
        ELSIF (ABS(RunChange)+ABS(FailChange)<>0)
           NEW(message,1..1);
           message[1]:="Error2 in ChangeEventState!     "; 
           result:=SendAlert(message,FALSE, FALSE, TRUE);
           DISPOSE(message);
        END IF;
     END IF;
  END METHOD;     {ChangeEventState}

  ASK METHOD Initialize();
  VAR
     j                     : INTEGER;
     innerSquare           : ImageObj;
     tempImage             : FillVObj;
  BEGIN   
     tempImage := Child("BasicBlock", 601);            
     IF ((activePhases>0) AND (usesPhasing)) 
        IF (parentID = activeWindow)
           innerSquare:= Descendant("InnerSquare", 0);
           ASK innerSquare TO SetHidden(FALSE);                      
        END IF;
        ASK tempImage TO SetStyle(SolidFill);
     END IF;
     ASK tempImage TO Draw; 
     GoodPathsRequired:=1;
     MinGoodPathsReq:=1;
     IF ((activePhases>0) AND (usesPhasing))     
        AllowReconfigureSystem:=TRUE;
        FOR j:=1 TO activePhases
        {   IF ((phaseType[j] ="A") OR (phaseType[j] ="L"))      {changed for phaseValue}   }
           IF ((phaseType[j] ="F") OR (phaseType[j] ="L"))   {tony 3-06 replaced the line above}   {changed for phaseValue}
              AllowReconfigureSystem:=FALSE;
           END IF;                      
        END FOR;     
     ELSE
        AllowReconfigureSystem:=TRUE;
     END IF;
  END METHOD;
      
  ASK METHOD SetToInitValues();
  BEGIN
     stateStartTime:=0.0;
     IF costAnalysis
        EventOpCost[seqNum]:=0.0;
        EventRepCost[seqNum]:=0.0;
     END IF;
     IF weakAnalysis
        EventRunTime[seqNum]:=0.0;
        EventStandbyTime[seqNum]:=0.0;
        EventRepairTime[seqNum]:=0.0;
     END IF;   
     NumGoodPaths:=EconnectIntoNum; 
     NumActivePaths:=EconnectIntoNum; 
     GoodPathsRequired:=1;
     FailureCounter:=0;
     discardedFailures:=0;
     activeStatus:=Active;
     opStatus:=Armed;
     IF ((usesPhasing) AND (activePhases>0)) 
        IF (phaseType[1]="L")
           ChangeEventState(Armed,Linked,"Initialize");
        ELSIF (phaseType[1]="C")         {do we still have -1 as value?}
           ChangeEventState(Armed,Cut,"Initialize");
        ELSIF (phaseType[1]="P")
           ChangeEventState(Armed,Active,"Initialize");
        ELSIF ((phaseType[1]="F") OR (phaseType[1]="A"))
           activeStatus:=Active;
           ChangeEventState(Armed,Active,"Initialize");
           ConductEvent;              
        END IF;
     ELSE
        ChangeEventState(Armed,Active,"Initialize");
        ConductEvent;
     END IF;
  END METHOD; 
      
  ASK METHOD ChangePhases     (IN oldVal,newVal                   : REAL;
                               IN oldType,newType                 : STRING);
  BEGIN
     IF ((oldType="C") AND (newType="L"))      {Cut to Link}
        ChangeEventState(opStatus,Linked,"Phase_Change");              
     ELSIF ((oldType="C") AND (newType="P"))  {Cut to Prior}
        ChangeEventState(opStatus,Active,"Phase_Change");
     ELSIF ((oldType="C") AND (newType="F"))     {Cut to Fire}
        ConductEvent;
     ELSIF ((oldType="C") AND (newType="A"))     {Cut to Alternate}
        ConductEvent;
     ELSIF ((oldType="C") AND (newType="C"))     {Cut to Cut}
        {do nothing}
     ELSIF ((oldType="L") AND (newType="C"))   {Link to Cut}
        ChangeEventState(opStatus,Cut,"Phase_Change");
     ELSIF ((oldType="L") AND (newType="P"))   {Link to Prior}
        ChangeEventState(opStatus,Active,"Phase_Change");
     ELSIF ((oldType="L") AND (newType="F"))   {Link to Fire}
        ConductEvent;        
     ELSIF ((oldType="L") AND (newType="A"))   {Link to Alternate}
        ConductEvent;        
     ELSIF ((oldType="L") AND (newType="L"))   {Link to Link}
        {do nothing}
     ELSIF (((oldType="F") OR (oldType="P") OR (oldType="A")) AND (newType="C"))   {Fire,Prior,Alternate to Cut}
        ChangeEventState(opStatus,Cut,"Phase_Change");
     ELSIF (((oldType="F") OR (oldType="P") OR (oldType="A")) AND (newType="L"))   {Fire,Prior,Alternate to Link}
        ChangeEventState(opStatus,Linked,"Phase_Change");              
     ELSIF (((oldType="F") OR (oldType="P") OR (oldType="A")) AND (newType="P"))   {Fire,Prior,Alternate to Prior}
        {do nothing}
     ELSIF (((oldType="F") OR (oldType="P") OR (oldType="A")) AND ((newType="F") OR (newType="A")))  
                                                                            {Fire,Prior,Alternate to Fire,Alternate}
        ConductEvent; 
     ELSE
        NEW(message,1..1);
        message[1]:="Error in 'ChangePhases' - isEvent section!     "; 
        result:=SendAlert(message,FALSE, FALSE, TRUE);
        DISPOSE(message); 
     END IF;
  END METHOD;
      
  ASK METHOD SetEventDefs();
  VAR
     i          : INTEGER;
     blockLabel : TextObj;
     blockImage : ImageObj;
  BEGIN
     usesPhasing      := FALSE;
     failDistro       := 17;
     failStream       := 203;
     initialCost      := 1.;
     operatingCost    := 1.;
     repairingCost    := 1.;
     name             := "unnamed";
     NEW(failVals, 1..1);
     failVals[1] := 0.8;
  END METHOD; 
      
  ASK METHOD SetCopyEventStream;
  BEGIN
     failStream   := 9;
  END METHOD;
      
  ASK METHOD ResetForPasting;
  BEGIN
     resetting := TRUE;
     SetPhases(FALSE,NILARRAY,NILARRAY);
  END METHOD;

  ASK METHOD SetEFPAvalues(IN EFPAdownNode, EFPApath                  : INTEGER);
  BEGIN
     EFPAnode:=EFPAdownNode;    
     EFPAlink:=EFPApath;
  END METHOD;
      
  ASK METHOD ConductEvent(); 
     VAR
        j                                       : INTEGER;
        draw                                    : REAL;
        oldOpStatus                             : EventStatusType;
        fireVals                                : realArray;
        tempHier                                : RBDHierObj;
     BEGIN
        IF FEVmode
           IF opStatus=Success
               draw:=1.0;
           ELSE
               draw:=0.0;
           END IF;
        ELSE
           NEW(fireVals,1..1);
           fireVals[1]:=failVals[1];
           IF ((usesPhasing) AND (activePhases>0))
              IF phaseType[phaseNumber]="A"
                 fireVals[1]:=phaseValue[phaseNumber];
              END IF;
           END IF;
           GetStream(failStream,"Event",seqNum,randStream);
           ASK randStream TO DrawNumber(failDistro,fireVals,name,draw);
           DISPOSE(fireVals);
        END IF;
        oldOpStatus:=opStatus;
        IF (draw=1.0)   {success}
           CASE oldOpStatus  {old Status}
              WHEN Failure:                 
                 IF ((usesPhasing) AND (activePhases>0) AND (activeStatus=Linked))
                    ChangeEventState(Success,Active,"Fired");                
                 ELSE                   
                    ChangeEventState(Success,Active,"Fired");                
                 END IF;
              WHEN Armed:
                 IF ((usesPhasing) AND (activePhases>0) AND (activeStatus=Cut))
                    ChangeEventState(Success,Active,"Fired");
                 ELSE
                    ChangeEventState(Success,Active,"Fired");
                 END IF;
              WHEN Success:
                 IF ((usesPhasing) AND (activePhases>0) AND (activeStatus=Cut))
                    ChangeEventState(Success,Active,"Fired");
                 ELSE 
                    ChangeEventState(Success,Active,"Fired");                       
                 END IF;
              OTHERWISE             
                 NEW(message,1..1);
                 message[1]:="Error - Conduct Event Case Statement!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                 DISPOSE(message);
           END CASE;
           IF costAnalysis
              EventOpCost[seqNum]:=EventOpCost[seqNum]+operatingCost;
           END IF;
        ELSE    {failure}
           CASE opStatus
              WHEN Failure:
                 IF ((usesPhasing) AND (activePhases>0) AND (activeStatus=Linked))
                    ChangeEventState(Failure,Active,"Fired");
                 ELSE
                    ChangeEventState(Failure,Active,"Fired");                    
                 END IF;
              WHEN Armed:
                 IF ((usesPhasing) AND (activePhases>0))
                    IF activeStatus=Linked
                       ChangeEventState(Failure,Active,"Fired");
                    ELSIF activeStatus=Cut
                       ChangeEventState(Failure,Active,"Fired");
                    ELSE
                       ChangeEventState(Failure,Active,"Fired");
                    END IF;
                 ELSE                 
                    ChangeEventState(Failure,Active,"Fired");
                    IF AllowReconfigureSystem                               
                       ReconfigureSystem(0);             
                    END IF;
                 END IF;
              WHEN Success:
                 IF ((usesPhasing) AND (activePhases>0))
                    IF activeStatus=Cut
                       ChangeEventState(Failure,Active,"Fired");
                    ELSE
                       ChangeEventState(Failure,Active,"Fired");
                    END IF;
                 ELSE 
                    ChangeEventState(Failure,Active,"Fired");
                 END IF;
              OTHERWISE             
                 NEW(message,1..1);
                 message[1]:="Error - Conduct Event Case Statement!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                 DISPOSE(message);
           END CASE;
           IF costAnalysis
              EventRepCost[seqNum]:=EventRepCost[seqNum]+repairingCost;
           END IF;
        END IF;
  END METHOD;   {ConductEvent}
      
  ASK METHOD ResetStats;
  BEGIN 
     discardedFailures:=FailureCounter;
  END METHOD;
      
  ASK METHOD CleanUp;
  VAR
     blockImage, innerSquare    : ImageObj;
     tempImage                  : FillVObj;
  BEGIN   
     blockImage := Descendant("BasicBlock", 601);            
     innerSquare:= Descendant("InnerSquare", 0);
     ASK innerSquare TO SetColor(blockGUIColor);
     ASK innerSquare TO SetHidden(TRUE);
     ASK blockImage TO SetColor(blockGUIColor);
     ASK blockImage TO SetTranslation(blockImage.Translation.x,blockImage.Translation.y);
     tempImage:=Child("BasicBlock",601);                                    
     IF ((usesPhasing) AND (phaseValue <> NILARRAY))                                   
        ASK tempImage TO SetStyle(NarrowCrosshatchFill);
     END IF;
     Draw;
  END METHOD;
      
  ASK METHOD SetInitName            (IN newValue   :STRING);
  VAR
     eventLabel : TextObj;
     innerSquare : ImageObj;
  BEGIN
     name:=newValue;
     ToolTip := newValue;
     innerSquare:= Descendant("InnerSquare", 0);
     ASK innerSquare TO SetHidden(TRUE);                            
     eventLabel := Child("RBDEventLabel", 0);
     ASK eventLabel TO SetText(newValue);
     IF fontSize = 1
        ASK eventLabel TO SetSysFont("SmallFonts",fontSize,70,0);
        ASK eventLabel TO SetHidden(TRUE);
     ELSE
        ASK eventLabel TO SetSysFont("SmallFonts",fontSize,70,0);
     END IF;
     ASK eventLabel TO Draw;
  END METHOD;
      
  ASK METHOD SetfailDistro           (IN newValue   :INTEGER);
  BEGIN
     failDistro:=newValue;
  END METHOD;
   
  ASK METHOD SetfailVals             (IN newVal    : REAL);
  VAR
     i  : INTEGER;
  BEGIN
     DISPOSE(failVals);
     NEW(failVals, 1..1);
     failVals[1] := newVal;
  END METHOD;
      
  ASK METHOD SetfailStream           (IN newValue   :INTEGER);
  BEGIN
     failStream:=newValue;
  END METHOD;
      
  ASK METHOD SetinitialCost          (IN newValue   :REAL);      
  BEGIN
     initialCost:=newValue;
  END METHOD;
   
  ASK METHOD SetOperatingCost         (IN newValue   :REAL);      
  BEGIN
     operatingCost:=newValue;
  END METHOD;
      
  ASK METHOD SetrepairingCost        (IN newValue   : REAL);      
  BEGIN
     repairingCost:=newValue;
  END METHOD;
      
  ASK METHOD fevInitialize;
  VAR
     eventImage,innerSquare             : ImageObj;
     tempImage                  : FillVObj;
  BEGIN        
     GoodPathsRequired:=1;
     MinGoodPathsReq:=1;
     eventImage := Descendant("BasicBlock", 601);            
     tempImage:=Child("BasicBlock",601);                                    
     IF ((activePhases>0) AND (usesPhasing))  
        ASK tempImage TO SetStyle(SolidFill);
        IF parentID=0;
           innerSquare:= Descendant("InnerSquare", 0);          
           ASK innerSquare TO SetHidden(FALSE);  
        END IF;
     END IF;
     ASK eventImage TO SetColor(LimeGreen);
     ASK eventImage TO SetTranslation(eventImage.Translation.x,eventImage.Translation.y);
     ASK eventImage TO Draw;
  END METHOD;
      
  ASK METHOD fevStartCond;
  BEGIN
     IF ((usesPhasing) AND (activePhases>0)) 
        IF (phaseType[1]="L")
           ChangeEventState(Success,Linked,"Initialize");
        ELSIF (phaseType[1]="C")   
           ChangeEventState(Success,Cut,"Initialize");
        ELSIF (phaseType[1]="P")
           ChangeEventState(Success,Active,"Initialize");
        ELSIF ((phaseType[1]="F") OR (phaseType[1]="A"))
           activeStatus:=Active;
           ChangeEventState(Success,Active,"Initialize");
        END IF;
     ELSE
        ChangeEventState(Success,Active,"Initialize");
     END IF;
  END METHOD;
      
  ASK METHOD fevCleanUp;
  VAR
     blockImage, innerSquare    : ImageObj;
     tempImage                  : FillVObj;
  BEGIN   
     blockImage := Descendant("BasicBlock", 601);            
     innerSquare:= Descendant("InnerSquare", 0);
     ASK innerSquare TO SetColor(blockGUIColor);
     ASK innerSquare TO SetHidden(TRUE);
     ASK blockImage TO SetColor(blockGUIColor);
     ASK blockImage TO SetTranslation(blockImage.Translation.x,blockImage.Translation.y);
     tempImage:=Child("BasicBlock",601);                                    
     IF ((usesPhasing) AND (phaseValue <> NILARRAY))                                   
        ASK tempImage TO SetStyle(NarrowCrosshatchFill);
     ELSE
        ASK tempImage TO SetStyle(SolidFill);
     END IF;
     Draw;
  END METHOD;
      
  ASK METHOD fevReset;
  VAR 
     eventImage,innerSquare :   ImageObj;
  BEGIN
     NumGoodPaths:=EconnectIntoNum; 
     NumActivePaths:=EconnectIntoNum; 
     GoodPathsRequired:=1;
     FailureCounter:=0;
     discardedFailures:=0;
     activeStatus:=Active;
     opStatus:=Success;
     IF GraphicsOutput
        innerSquare:= Descendant("InnerSquare", 0); 
        ASK innerSquare TO SetColor(LimeGreen);
        ASK innerSquare TO SetHidden(TRUE);
        eventImage := Descendant("BasicBlock", 601);            
        ASK eventImage TO SetColor(LimeGreen);
        ASK eventImage TO SetTranslation(eventImage.Translation.x,eventImage.Translation.y);                                       
        ASK eventImage TO Draw;
     END IF;
  END METHOD; 
      
      
  ASK METHOD SetopStatus             (IN newValue   : STRING);  
  BEGIN         
      IF newValue="Success"
         opStatus:=Success;
      ELSIF newValue="Failure"
         opStatus:=Failure;
      END IF;
  END METHOD;
      
  ASK METHOD ShowAnalColor();
  VAR
     aoText, nameText                : TextObj;
     eventImage, innerSquare         : ImageObj;
     displayColor                    : ColorType;
     displayValue                    : REAL;
  BEGIN
     innerSquare:= Descendant("InnerSquare", 0);
     IF weakLinkAnalType=1 
        displayValue := STRTOREAL(Ao);
     ELSIF weakLinkAnalType=2 
        displayValue := STRTOREAL(Do);
     ELSE 
        displayValue := STRTOREAL(R);
     END IF;
     IF displayValue >= GYthreshold
        displayColor := Green;
     ELSIF displayValue >= YRthreshold
        displayColor := Yellow;
     ELSE
        displayColor := Red;
     END IF;
     aoText := Child("EventAoText",0);
     nameText := Child("RBDEventLabel", 0);
     eventImage := Child("BasicBlock", 601);      
     ASK aoText TO SetColor(Blue);
     ASK eventImage TO SetColor(displayColor);
     IF displayValue<0.9985
        ASK aoText TO SetText(SUBSTR(2,5,REALTOSigFigSTR(displayValue,3)));
     ELSIF displayValue=1.0
        ASK aoText TO SetText("1.00");
     ELSE
        ASK aoText TO SetText(".999");
     END IF;
     IF parentID = activeWindow
        ASK aoText TO SetHidden(FALSE);
        SetHidden(FALSE);
     END IF;
     IF (fontSize > 1) AND (displayValue >= 0.)
        ASK aoText TO SetHidden(FALSE);
        ASK aoText TO SetSysFont("SmallFonts",fontSize-1,70,0);
     ELSE
        ASK nameText TO SetHidden(TRUE);
        ASK aoText TO SetHidden(TRUE);
     END IF;
     IF parentID <> activeWindow
        ASK aoText TO SetHidden(TRUE);
        SetHidden(TRUE);
     END IF;   
      ASK innerSquare TO SetColor(displayColor);   {necessary to insure printing works}
      ASK innerSquare TO SetHidden(TRUE);  
     Draw;
  END METHOD;
      
END OBJECT; {RBDEventObj}



OBJECT RapTriggerObj;
   ASK METHOD SetTrigData        (IN newString                   : STRING;
                                  IN newDist                     : INTEGER;
                                  IN newParams                   : realArray;
                                  IN newRepeats                  : BOOLEAN;
                                  IN newUsed                     : REAL);
   VAR
      i,numParams                                       : INTEGER;
      tempList                                          : OptionListType;

   BEGIN
      DISPOSE(TrigParams);
      TrigName:=newString;
      TrigDist:=newDist;
      GetNumParams(TrigDist,numParams);
      NEW(TrigParams,1..numParams);
      FOR i := 1 TO numParams
         TrigParams[i] := newParams[i];
      END FOR;
      Repeats:=newRepeats;
      InitUsed:=newUsed;
      ASK triggerGroup TO Add(SELF);
      
      IF (TrigName="StartUpFaiLures")         {cmc StartUpFailures}
         startUpFailures:=TRUE;
      END IF;
     
   END METHOD;

   ASK METHOD RemoveTrig            (IN name    : STRING);
   VAR
      i, j     : INTEGER;
      tempList : OptionListType;
   BEGIN    
      IF totalTriggers > 1
         NEW(tempList, 1..totalTriggers - 1);
         j := 1;
         FOR i := 1 TO totalTriggers
            IF trigList[i] <> name
               tempList[j] := trigList[i];
               INC(j);
            END IF;
         END FOR;      
         DISPOSE(trigList);
         totalTriggers := totalTriggers - 1;
         NEW(trigList,1..totalTriggers);
         FOR i := 1 TO totalTriggers
            trigList[i] := tempList[i];
         END FOR;
         DISPOSE(tempList);
      ELSE
         totalTriggers  := 0;
         trigList[1] := "unnamed";
      END IF;   
      somethingChanged := TRUE;    
      IF (TrigName="StartUpFaiLures")         {cmc StartUpFailures}
         startUpFailures:=FALSE;
      END IF;

   
    END METHOD;{RemoveTrig} 
   
      
   
   TELL METHOD PullRapTrigger;
   VAR
      waitTime  : REAL;
      continue  : BOOLEAN;
      counter   : INTEGER;
   BEGIN
      continue:=TRUE;
      counter:=0;
      WHILE continue
         ASK SysStream[11] TO DrawNumber(TrigDist,TrigParams,TrigName,waitTime); 
         waitTime:=waitTime-InitUsed;
         IF waitTime>0.0
            continue:=FALSE;
         ELSE
            INC(counter);
            IF counter>=100
               waitTime:=0.00001;
               continue:=FALSE;
            END IF;
         END IF;  
      END WHILE;   
      LOOP
         WAIT DURATION waitTime; 
         END WAIT;
         Release;
         IF EventsFile
            WriteEvents(SimTime,"Trigger",TrigName,"Fired","System_event");
         END IF;   
         ASK SysStream[11] TO DrawNumber(TrigDist,TrigParams,TrigName,waitTime);
         IF waitTime<=0.0
            waitTime:=.000001;
         END IF;   
         IF (NOT Repeats)
            EXIT;
         END IF;
      END LOOP;
   END METHOD;                  
   
END OBJECT; {RapTriggerObj}

   
   
END MODULE. {imod RBDBlock}
