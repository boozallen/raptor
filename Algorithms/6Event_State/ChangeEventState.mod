

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

