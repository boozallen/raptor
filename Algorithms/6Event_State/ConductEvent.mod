

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

