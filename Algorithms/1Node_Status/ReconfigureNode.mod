

   ASK METHOD ReconfigureNode(IN RunChange,FailChange,IdleChange,
                                 StandbyChange,pmChange,inPath    : INTEGER);  
     VAR
        arrayIndex,path,i,runs,fails,idles,
        standbys,pms                                : INTEGER;  
        block                                       : RBDBlockObj; 
        tempnode                                    : RBDNodeObj;
        link                                        : LinkObj;
        oldLinkStatus,newLinkStatus                 : LinkStatusType;
        newNodeStatus                               : NodeStatusType;
        linkStatusChanged                           : BOOLEAN;
        statusString,linkString                     : STRING;
        hier                                        : RBDHierObj;
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

