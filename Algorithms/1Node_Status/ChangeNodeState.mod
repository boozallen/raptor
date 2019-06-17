

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
                 ASK RepTimeStream TO WriteReal(SysTimeToRepair,12,6);  
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

