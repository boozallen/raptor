

   ASK METHOD ChangeBlockState (IN newOpStatus          : BlockStatusType;
                                IN newActiveStatus      : ActiveStatusType;
                                IN comment              : STRING);      
     VAR
        blockImage,innerSquare                                  : ImageObj;
        oldOpStatus                                             : BlockStatusType;
        stateTime                                               : REAL;
        activeString,statusString,actionString,commentString    : STRING;
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
     END IF;
  END METHOD;     {ChangeBlockState}

