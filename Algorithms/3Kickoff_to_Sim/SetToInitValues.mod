

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
  END METHOD; 

