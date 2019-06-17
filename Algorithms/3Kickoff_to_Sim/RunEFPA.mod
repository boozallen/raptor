

PROCEDURE RunEFPA(INOUT LoopID : INTEGER);
  VAR    
     i,j,k,EFPAOutNum,NRBDOutNum,CapOutNum,DownStreamNode,
     DownStreamLink,NextElement,nominalFlow,FromID,maximumFlow,
     priority,flowShare,StartElement,linkId,newCompletePercent,
     completePercent,mamaLevel                                    : INTEGER; 
     block                                                        : RBDBlockObj;
     event                                                        : RBDEventObj;
     node,groupMember,tempNode                                    : RBDNodeObj;
     hier                                                         : RBDHierObj;
     EFPADownstreamArray,EFPAPathArray, NRBDDownstreamArray,
     NRBDPathArray,CapDownstreamArray,CapPathArray                : intArray;
     name,DownStreamName,FromReference,ToReference,tempString     : STRING;
     checkingForCSB, lookingForLink, lookingForNode               : BOOLEAN;
     link,sisterLink,tempLink                                     : LinkObj; 
     element                                                      : RBDBasicObj;
     findLinkGroup                                                : QueueObj;
     
BEGIN
  completePercent:=40;
  {***}
  IF simDetails 
     ASK simDetailsStream TO Open("simDetails.TXT", Append);
     ASK simDetailsStream TO WriteString("Begining Procedure RunEFPA......");   
     ASK simDetailsStream TO WriteLn; 
     ASK simDetailsStream TO WriteLn; 
  END IF; 
  {***}
  FOREACH node IN nodeGroup
     ASK node TO SetGoodPathsRequired(1);
     ASK node TO SetNumGoodPaths(node.EconnectIntoNum);
     ASK node TO SetEFPAtested(FALSE);
     IF capacityAnalysis
        ASK node TO SetCapNode(FALSE);
        ASK node TO SetCapTested(FALSE);
     END IF;     
     {ASK node TO SetHasDepObjects(FALSE);   } {chuck}
  END FOREACH;
  LoopError:=FALSE;
  LoopID:=0;
  NEW(DownstreamGroup);   
  NEW(PropagatedGroup);   
  IF ((NOT configFrozen) OR capacityAnalysis)
     {Determines NRBD values and determines block EFPA settings}
     NEW(findLinkGroup);
     FOREACH link IN linkGroup
        ASK findLinkGroup TO Add(link);
     END FOREACH;
     i:=1;
     FOREACH node IN nodeGroup;
        NRBDOutNum:=node.EconnectOutOfNum; 
        IF node.typeNode<>3
           NEW(NRBDDownstreamArray,1..NRBDOutNum);
           NEW(NRBDPathArray,1..NRBDOutNum);        
           FOR j:=1 TO NRBDOutNum 
              lookingForNode:=TRUE;
              StartElement:=node.Id;
              NextElement:=node.connectToIds[j]; 
              ToReference:=node.connectToRefs[j];
              IF capacityAnalysis
                 link:= ASK root Child("RBDLink",node.outPathsArray[j]);
                 priority:=link.simCapPriority;
              END IF;
              WHILE lookingForNode
                 IF ToReference="RBDBlock"
                    block := ASK root Child("RBDBlock", NextElement);
                    ASK PropagatedGroup TO Add(block);  
                    StartElement:=block.Id;
                    NextElement:=block.connectToIds[1]; 
                    ToReference:=block.connectToRefs[1];
                 ELSIF ToReference="RBDEvent"
                    event := ASK root Child("RBDEvent", NextElement);
                    ASK PropagatedGroup TO Add(event);  
                    StartElement:=event.Id;
                    NextElement:=event.connectToIds[1]; 
                    ToReference:=event.connectToRefs[1];
                 ELSE
                   lookingForNode:=FALSE;   
                 END IF;
              END WHILE;
              FOREACH tempLink IN findLinkGroup
                 IF ((tempLink.EconnectTRef="RBDBlock") OR (tempLink.EconnectTRef="RBDEvent"))
                    ASK findLinkGroup TO RemoveThis(tempLink);
                 ELSIF ((tempLink.EconnectFromId=StartElement) AND (tempLink.EconnectToId=NextElement))
                    link:=tempLink;
                    IF capacityAnalysis
                       ASK link TO SetSimCapPriority(priority); {extends out path priority to next node}
                    END IF;
                    ASK findLinkGroup TO RemoveThis(tempLink);
                    EXIT;
                 END IF;
              END FOREACH;      
              NRBDDownstreamArray[j]:=NextElement;
              NRBDPathArray[j]:=link.Id;  
              FOREACH element IN PropagatedGroup
                 IF OBJTYPENAME(element)="RBDBlockObj";
                    block := ASK root Child("RBDBlock", element.Id);              
                    ASK block TO SetEFPAvalues(NextElement,link.Id); 
                 ELSIF OBJTYPENAME(element)="RBDEventObj";
                    event := ASK root Child("RBDEvent", element.Id);              
                    ASK event TO SetEFPAvalues(NextElement,link.Id); 
                 END IF;   
                 ASK PropagatedGroup TO RemoveThis(element);
              END FOREACH;
           END FOR;
           ASK node TO SetNRBDvalues(NRBDOutNum,NRBDDownstreamArray,NRBDPathArray); 
           DISPOSE(NRBDDownstreamArray);
           DISPOSE(NRBDPathArray);
        END IF;        
        newCompletePercent:=ROUND(40.0+20.0*FLOAT(i)/FLOAT(totalNodes));
        IF newCompletePercent <> completePercent
           completePercent := newCompletePercent;
           ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
        END IF;
        INC(i);
     END FOREACH;     
     {60% complete} {NRBD determined}
     
     FOREACH tempLink IN findLinkGroup
        ASK findLinkGroup TO RemoveThis(tempLink);
     END FOREACH;
     ASK findLinkGroup TO ObjTerminate;
     
     {next section - test fail each node}
     i:=1;  
     FOREACH node IN nodeGroup
        tempString:=node.name;
        TestElement:=node.Id;
        LoopCheck:=0;
        IF ((node.typeNode=2) OR {(node.typeNode=4) OR }(node.typeNode=5))     {tony}
           analysisType:="Full";
           ASK node TO SetNumGoodPaths(node.GoodPathsRequired);
           IF node.parentID<>0
              hier := ASK root Child("RBDHier",node.parentID);
              IF node.typeNode=5
                 mamaLevel:=hier.level;
              ELSE
                 mamaLevel:=hier.level+1;
              END IF;
           ELSE
              mamaLevel:=0;
           END IF;   
           ASK node TO TestFail(mamaLevel);
           IF LoopError
              LoopID:=node.seqNum;
              FOREACH link IN DownstreamGroup
                 ASK DownstreamGroup TO RemoveThis(link);
              END FOREACH;
              FOREACH groupMember IN PropagatedGroup
                 ASK PropagatedGroup TO RemoveThis(groupMember);
              END FOREACH;
              EXIT;
           END IF;
           FOREACH link IN DownstreamGroup
              tempNode:=ASK root Child("RBDNode",link.EconnectToId);
              tempString:=tempNode.name;
              IF (PropagatedGroup.Includes(tempNode))
                 ASK DownstreamGroup TO RemoveThis(link);
              END IF;
           END FOREACH;        
           EFPAOutNum:=DownstreamGroup.numberIn;
 
           IF simDetails 
              IF node.typeNode=5
                 hier:=ASK root Child("RBDHier", node.parentID);           
                 ASK simDetailsStream TO WriteString(node.name + "-" + hier.name+"  EFPAout=" + INTTOSTR(EFPAOutNum));               
              ELSE
                 ASK simDetailsStream TO WriteString(node.name + "  EFPAout=" + INTTOSTR(EFPAOutNum)); 
              END IF;
              ASK simDetailsStream TO WriteLn; 
           END IF; 

           NEW(EFPADownstreamArray,1..EFPAOutNum);
           NEW(EFPAPathArray,1..EFPAOutNum);
           j:=1;
           FOREACH link IN DownstreamGroup
              DownStreamNode:=link.EconnectToId;
              DownStreamLink:=link.Id;
              EFPADownstreamArray[j]:=DownStreamNode;
              EFPAPathArray[j]:=DownStreamLink;
              IF simDetails 
                 tempNode:=ASK root Child("RBDNode",DownStreamNode);
                 ASK simDetailsStream TO WriteString("   " + tempNode.name + "  through link "+INTTOSTR(DownStreamLink));   
                 ASK simDetailsStream TO WriteLn; 
              END IF; 
              INC(j);
           END FOREACH;
           ASK node TO SetEFPAvalues(EFPAOutNum,EFPADownstreamArray,EFPAPathArray); 
           ASK node TO SetEFPAtested(TRUE);           
           
           FOREACH node IN nodeGroup 
              ASK node TO SetNumGoodPaths(node.EconnectIntoNum);
           END FOREACH;
           DISPOSE(EFPADownstreamArray);
           DISPOSE(EFPAPathArray);
           FOREACH link IN DownstreamGroup
              ASK DownstreamGroup TO RemoveThis(link);
           END FOREACH;
           FOREACH groupMember IN PropagatedGroup
              ASK PropagatedGroup TO RemoveThis(groupMember);
           END FOREACH;
        END IF; 
        newCompletePercent:=ROUND(60.0+20.0*FLOAT(i)/FLOAT(totalNodes));
        IF newCompletePercent <> completePercent
           completePercent := newCompletePercent;
           ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
        END IF;
        INC(i);
     END FOREACH;
  END IF;
  {80% complete}
  
  {next section - fixes blocks connected into an IN node, changes to that OUT node's EFPA}
  i:=1;
  FOREACH block IN blockGroup;
     node := ASK root Child("RBDNode",block.EFPAnode);   
     IF node.typeNode=4                                    {an In node}
        hier:=ASK root Child("RBDHier", node.parentID);  
        node:= ASK root Child("RBDNode",hier.outID);       {the Out node}
        ASK block TO SetEFPAvalues(node.EFPAConnectTo[1],node.EFPAConnectPath[1]);
        node:= ASK root Child("RBDNode",node.EFPAConnectTo[1]);  {the Out node's EFPA node}
     END IF;
     IF simDetails 
        ASK simDetailsStream TO WriteString(block.name + "  EFPAout=" + "1"); 
        ASK simDetailsStream TO WriteLn; 
        IF node.typeNode=5
           hier:=ASK root Child("RBDHier", node.parentID);           
           ASK simDetailsStream TO WriteString("   " + node.name +  "-" + hier.name +
                                    "  through link "+INTTOSTR(block.EFPAlink));
        ELSIF node.name="End"
           ASK simDetailsStream TO WriteString("   " + node.name +
                                    "  through link "+INTTOSTR(block.EFPAlink)); 
        ELSE
           ASK simDetailsStream TO WriteString("   " + node.name +
                                    "  through link "+INTTOSTR(block.EFPAlink));   
        END IF;
        ASK simDetailsStream TO WriteLn; 
     END IF; 
     newCompletePercent:=ROUND(80.0+9.0*FLOAT(i)/FLOAT(totalBlocks));
     IF newCompletePercent <> completePercent
        completePercent := newCompletePercent;
        ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
     END IF;
     INC(i);
  END FOREACH;
  {89% complete}

  {next section - fixes events connected into an IN node, changes to that OUT node's EFPA}
  i:=1;
  FOREACH event IN eventGroup;
     node := ASK root Child("RBDNode",event.EFPAnode);   
     IF node.typeNode=4                                    {an In node}
        hier:=ASK root Child("RBDHier", node.parentID);  
        node:= ASK root Child("RBDNode",hier.outID);       {the Out node}
        ASK event TO SetEFPAvalues(node.EFPAConnectTo[1],node.EFPAConnectPath[1]);
        node:= ASK root Child("RBDNode",node.EFPAConnectTo[1]);  {the Out node's EFPA node}
     END IF;
     IF simDetails 
        ASK simDetailsStream TO WriteString(event.name + "  EFPAout=" + "1"); 
        ASK simDetailsStream TO WriteLn; 
        IF node.typeNode=5
           hier:=ASK root Child("RBDHier", node.parentID);           
           ASK simDetailsStream TO WriteString("   " + node.name +  "-" + hier.name +
                                    "  through link "+INTTOSTR(event.EFPAlink));
        ELSIF node.name="End"
           ASK simDetailsStream TO WriteString("   " + node.name +
                                    "  through link "+INTTOSTR(event.EFPAlink)); 
        ELSE
           ASK simDetailsStream TO WriteString("   " + node.name +
                                    "  through link "+INTTOSTR(event.EFPAlink));   
        END IF;
        ASK simDetailsStream TO WriteLn; 
     END IF; 
     newCompletePercent:=ROUND(89.0+1.0*FLOAT(i)/FLOAT(totalEvents));
     IF newCompletePercent <> completePercent
        completePercent := newCompletePercent;
        ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
     END IF;
     INC(i);
  END FOREACH;
  {90% complete}
  ASK window TO ShowStatus(1,"90% complete");
  
  
  IF simDetails 
     ASK simDetailsStream TO Close; 
  END IF;
  
  
  {start capacity section}
  {this section finds the downstream nodes for each node and outbound link to that node(s) }
  {Capacity II}
  totalCapNodes:=0;
  IF (capacityAnalysis AND (NOT LoopError))
     IF simDetails 
        ASK simDetailsStream TO Open("simDetails.TXT", Append);
        ASK simDetailsStream TO WriteLn; 
        ASK simDetailsStream TO WriteLn; 
        ASK simDetailsStream TO WriteString("Capacity Algorithm Results");                 
        ASK simDetailsStream TO WriteLn; 
     END IF;
     i:=1;
     FOREACH node IN nodeGroup    {stepping through all nodes}
        name:=node.name;
        IF (node.typeNode=3)
           ASK node TO SetCapNode(TRUE);            
           INC(totalCapNodes); 
        ELSIF ((node.typeNode=1) OR (NOT node.fullFlow) OR (NOT node.anyPath))  
           ASK node TO SetCapNode(TRUE);
           INC(totalCapNodes);
           TestElement:=node.Id;           
           ASK node TO FindCapPaths(0);
           CapOutNum:=DownstreamGroup.numberIn;
           IF simDetails 
              ASK simDetailsStream TO WriteString(node.name + "  CapOutNum=" + INTTOSTR(CapOutNum));   
              ASK simDetailsStream TO WriteLn; 
           END IF; 
           NEW(CapDownstreamArray,1..CapOutNum);
           NEW(CapPathArray,1..CapOutNum);
           j:=1;
           FOREACH link IN DownstreamGroup
              DownStreamNode:=link.EconnectToId;
              DownStreamLink:=link.Id;
              CapDownstreamArray[j]:=DownStreamNode;
              CapPathArray[j]:=DownStreamLink;
              IF simDetails 
                 tempNode:=ASK root Child("RBDNode",DownStreamNode);
                 ASK simDetailsStream TO WriteString("   " + tempNode.name + 
                    "  through link "+INTTOSTR(DownStreamLink) + 
                    "  Priority = "+ INTTOSTR(link.simCapPriority));   
                 ASK simDetailsStream TO WriteLn; 
              END IF; 
              INC(j);
              ASK DownstreamGroup TO RemoveThis(link);
              IF NOT capLinkGroup.Includes(link)
                 ASK capLinkGroup TO Add(link);
              END IF;
           END FOREACH;
           ASK node TO SetCapValues(CapOutNum,CapDownstreamArray,CapPathArray); 
           DISPOSE(CapDownstreamArray);
           DISPOSE(CapPathArray); 
        END IF;
        FOREACH tempNode IN PropagatedGroup
           ASK tempNode TO SetCapTested(FALSE);               
           ASK PropagatedGroup TO RemoveThis(tempNode);
        END FOREACH;
        IF node.capNode
           ASK capNodeGroup TO Add(node);
        END IF;        
        newCompletePercent:=ROUND(90.0+4.0*FLOAT(i)/FLOAT(totalNodes));
        IF newCompletePercent <> completePercent
           completePercent := newCompletePercent;
           ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
        END IF;
        INC(i);
     END FOREACH;     
     IF simDetails 
        ASK simDetailsStream TO WriteString("CAP STATS:  CapNodes = " + INTTOSTR(capNodeGroup.numberIn));   
        ASK simDetailsStream TO WriteLn; 
        ASK simDetailsStream TO WriteString("            CapLinks = " + INTTOSTR(capLinkGroup.numberIn));   
        ASK simDetailsStream TO WriteLn; 
        ASK simDetailsStream TO Close; 
     END IF; 
  END IF;    {end capacity II }
  completePercent:=94; 
  ASK window TO ShowStatus(1,"94% complete");
  {next section - sets sim flow values and priorities if nom flow / max flow options 
   are general and based on Flow Generated} 
  IF NOT LoopError
     IF capacityAnalysis
        i:=1;
        FOREACH node IN capNodeGroup
           tempString:=node.name;
           flowShare:=CEIL(FLOAT(flowGenerated)/FLOAT(node.EconnectIntoNum));
           IF node.typeNode<>1
              FOR j:=1 TO node.EconnectIntoNum
                 link := ASK root Child("RBDLink", node.inPathsArray[j]);
                 linkId:=link.Id;
                 IF node.fullFlow   {box on capacity db is checked}
                     maximumFlow:=999999999;
                     nominalFlow:=999999999;
                 ELSE   {user defined flow}
                    IF link.maxFlow=-1
                        maximumFlow:=flowGenerated;
                    ELSIF link.maxFlow=-2
                       maximumFlow:=flowShare;
                    ELSE
                       maximumFlow:=link.maxFlow;
                    END IF;
                    IF link.nomFlow=-1
                       nominalFlow:=maximumFlow;
                    ELSIF link.nomFlow=-2              
                       nominalFlow:=CEIL(FLOAT(maximumFlow)/FLOAT(node.EconnectIntoNum));
                    ELSE
                       IF link.nomFlow>maximumFlow
                          nominalFlow:=maximumFlow;
                       ELSE
                          nominalFlow:=link.nomFlow;
                       END IF;
                    END IF;
                 END IF;
                 ASK link TO SetSimFlowVals(nominalFlow, maximumFlow);
              END FOR;
           END IF;
           IF ((node.typeNode<>3) AND (node.anyPath))
              FOR j:=1 TO node.CapConnectOut
                 link := ASK root Child("RBDLink", node.CapConnectPath[j]);
                 ASK link TO SetSimCapPriority(j);
              END FOR;
           END IF; 
           {IF node.typeNode<>3
              FOR j:=1 TO node.EconnectOutOfNum
                 link := ASK root Child("RBDLink", node.outPathsArray[j]);
                 linkId:=link.Id;
                 IF node.anyPath   {box on capacity db is checked}
                    priority:=j;
                    {priority:=50;}
                 ELSE   {user defined priority}
                    priority:=link.capPriority;
                 END IF;
                 ASK link TO SetSimCapPriority(priority);
              END FOR;
           END IF; }
           newCompletePercent:=ROUND(94.0+2.0*FLOAT(i)/FLOAT(capNodeGroup.numberIn));
           IF newCompletePercent <> completePercent
              completePercent := newCompletePercent;
              ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
           END IF;
           INC(i);
        END FOREACH; 
     END IF;
     completePercent:=96; 
     ASK window TO ShowStatus(1,"96% complete");     
     i:=1;
     FOREACH link IN linkGroup
        ASK link TO SetDepLink(0);
        IF  (link.EconnectTRef ="RBDBlock")
           block := ASK root Child("RBDBlock", link.EconnectToId);
           sisterLink:= ASK root Child("RBDLink",block.EFPAlink);
           ASK link TO SetDepLink(block.EFPAlink);
           ASK sisterLink TO AddFamilyMember(link);           
           {IF (link.EconnectFRef = "RBDNode") 
              ASK sisterLink TO SetSimCapPriority(link.capPriority);
           END IF;  }
        ELSIF  (link.EconnectTRef ="RBDEvent")
           event := ASK root Child("RBDEvent", link.EconnectToId);
           sisterLink:= ASK root Child("RBDLink",event.EFPAlink);
           ASK link TO SetDepLink(event.EFPAlink);
           ASK sisterLink TO AddFamilyMember(link);           
           {IF (link.EconnectFRef = "RBDNode") 
              ASK sisterLink TO SetSimCapPriority(link.capPriority);
           END IF;}
        END IF; 
        newCompletePercent:=ROUND(96.0+2.0*FLOAT(i)/FLOAT(totalLinks));
        IF newCompletePercent <> completePercent
           completePercent := newCompletePercent;
           ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
        END IF;
        INC(i);
     END FOREACH;
     ASK window TO ShowStatus(1,"98% complete"); 
     IF totalBlocks>0          
        SortBlocks;
     END IF; 
     IF totalEvents>0
        SortEvents;
     END IF;
     IF totalHiers>0
        SortHiers;
     END IF;   
     ASK window TO ShowStatus(1,"99% complete");     
     SortNodes;  
     ASK window TO ShowStatus(1,"100% complete");      
  END IF;
  ASK DownstreamGroup TO ObjTerminate;
  ASK PropagatedGroup TO ObjTerminate;  
  ASK window TO ShowStatus(0,""); 
  ASK window TO ShowStatus(1,"");  
END PROCEDURE;     {RunEFPA} 


