

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
                         IF (NOT tempBlock.returnFromMaint)                   
                            IF (tempBlock.DepIdlePerc=100.)
                               action:="GoIdle";
                            ELSIF (tempBlock.DepNothingPerc = 100.)  
                               action:="DoNothing";
                            ELSIF (tempBlock.DepPMPerc = 100.)  
                               action:="GoToPM";
                            ELSIF (tempBlock.DepFailPerc = 100.)
                               action:="InducedFailure";
                            ELSE   {draw number to determine induced state} 
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
                            END IF;
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
                               IF specialBackFlow
                                  IF tempBlock.InterruptReason=""
                                     IF tempBlock.defDepStateIdle
                                        ASK tempBlock TO ChangeBlockState(Idle,tempBlock.activeStatus,"");
                                     ELSE
                                        ASK tempBlock TO ChangeBlockState(Running,tempBlock.activeStatus,"");
                                     END IF;
                                  END IF;
                               ELSE
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
                               IF specialBackFlow
                                  IF tempBlock.InterruptReason=""
                                     IF tempBlock.defDepStateIdle
                                        ASK tempBlock TO ChangeBlockState(Idle,tempBlock.activeStatus,"");
                                     ELSE
                                        ASK tempBlock TO ChangeBlockState(Running,tempBlock.activeStatus,"");
                                     END IF;
                                  END IF;
                               ELSE
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
                      ASK tempBlock TO SetInterruptReason("Dependence_All_Up");
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

