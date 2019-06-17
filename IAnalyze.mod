{+++++++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++++++++++}
{+                                                                                             +}
{+  Implementation module file name : ANALYZE                                                  +}
{+  Author                          : Chuck Carter/Tony Malerich                               +}
{+  Date                            : January 2004                                             +}
{+                                                                                             +}
{+  Description Here                   .                                                       +}
{+                                                                                             +}
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

IMPLEMENTATION MODULE Analyze;

FROM Display  IMPORT totalBlocks,totalEvents,totalNodes,totalHiers, totalLinks,root,totalObjects, 
                     capacityAnalysis,flowGenerated,window,devVersion,
                     simDetails, simDetailsStream,negShutUp,dialogs,endId,totalHiers,
                     nameOfFile,DomainTree,CoreFile,capNodeGroup,capLinkGroup;
FROM Objects  IMPORT RBDBlockObj,RBDNodeObj,RBDHierObj,RBDEventObj,
                     LinkObj,intArray,strArray,RBDBasicObj;
FROM GTypes   IMPORT TextBufferType;
FROM MathMod  IMPORT SIN, COS, ATAN2, SQRT, POWER, pi, LN, EXP,CEIL;  
FROM Intract  IMPORT SendAlert,HelpBoxObj;
FROM IOMod    IMPORT FileUseType(Output,Input, Append, BinaryInput, BinaryOutput),StreamObj; 
FROM SimMod   IMPORT SimTime;
FROM Runsim   IMPORT dontShow;
FROM Button   IMPORT ButtonObj;
FROM UtilMod  IMPORT DateTime;
 
VAR       
   sibling                : LinkObj;
   message                : TextBufferType;
   TreeStream,CoreStream  : StreamObj;
   DescendantsGroup       : ARRAY INTEGER OF QueueObj;
   
PROCEDURE RunEFPA(INOUT LoopID : INTEGER);
  VAR    
     i,j,k,EFPAOutNum,NRBDOutNum,CapOutNum,DownStreamNode,
     DownStreamLink,NextElement,nominalFlow,FromID,maximumFlow,
     priority,flowShare,StartElement,linkId,newCompletePercent,
     completePercent,mamaLevel,         NextType,NextPath    {cmc 5/1/09}             : INTEGER; 
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

     

     {Hierarchy EFPA Fix Treat hierarchy out nodes like blocks to determine EFPA node}    {cmc 5/1/09}        
      IF totalHiers>0     
         FOREACH node IN nodeGroup;
            IF node.typeNode=5
                 NextElement:=node.NRBDConnectTo[1]; 
                 NextPath := node.NRBDConnectPath[1];
                 tempNode:=ASK root Child("RBDNode",NextElement);
                 NextType := tempNode.typeNode;
                 
                 lookingForNode:=TRUE;                           
                 WHILE lookingForNode
                   IF NextType=4                 
                        hier:=ASK root Child("RBDHier", tempNode.parentID);  
                        tempNode:= ASK root Child("RBDNode",hier.outID);       {the Out node}
                        NextElement:=tempNode.NRBDConnectTo[1]; 
                        NextPath := tempNode.NRBDConnectPath[1];
                        tempNode:=ASK root Child("RBDNode",NextElement);       {the next one after the out node}
                        NextType := tempNode.typeNode;
                    ELSE
                      lookingForNode:=FALSE;   
                    END IF;
                 END WHILE;                                    
                 EFPAOutNum:=1;
                 NEW(EFPADownstreamArray,1..EFPAOutNum);
                 NEW(EFPAPathArray,1..EFPAOutNum);
                 EFPADownstreamArray[1]:=NextElement;
                 EFPAPathArray[1]:=NextPath;
                 ASK node TO SetEFPAvalues(EFPAOutNum,EFPADownstreamArray,EFPAPathArray); 
                 ASK node TO SetEFPAtested(TRUE);  
                 IF simDetails 
                    hier:=ASK root Child("RBDHier", node.parentID);           
                    ASK simDetailsStream TO WriteString(node.name + "-" + hier.name+"  EFPAout=" + INTTOSTR(EFPAOutNum));               
                    ASK simDetailsStream TO WriteLn; 
                    ASK simDetailsStream TO WriteString("   " + tempNode.name + "  through link "+INTTOSTR(NextPath));   
                    ASK simDetailsStream TO WriteLn; 
                 END IF;          
            END IF;
         END FOREACH;
      END IF;

      {End Hierarchy EFPA Fix}}       {cmc 5/1/09}
     
     
     
     
     {next section - test fail each node}
     i:=1;  
     FOREACH node IN nodeGroup
        tempString:=node.name;
        TestElement:=node.Id;
        LoopCheck:=0;
     {   IF ((node.typeNode=2) OR (node.typeNode=4) { OR (node.typeNode=5) })     {tony}         }     {cmc 5/1/09} 
        IF (node.typeNode=2) {cmc 5/1/09}  
       
        
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


PROCEDURE SortBlocks;
  VAR
    i,j,swapIndex,currentId           : INTEGER;    
    tempArray                         : strArray;
    HigherPriorityFound               : BOOLEAN; 
    currentString,swapName            : STRING;
    block                             : RBDBlockObj;
  BEGIN
   IF BlockAlphaSort<>NILARRAY
      DISPOSE(BlockAlphaSort);
   END IF;
   NEW(BlockAlphaSort,1..totalBlocks);
   NEW(tempArray,1..totalBlocks);
   i:=1;
   FOREACH block IN blockGroup
     BlockAlphaSort[i]:=block.Id;
     tempArray[i]:=LOWER(block.name);
     INC(i);
   END FOREACH;
   
     {Sort Array}  
     FOR i:= 1 TO totalBlocks
        currentId:=BlockAlphaSort[i];
        currentString:=tempArray[i];
        HigherPriorityFound:=FALSE;
        swapIndex:=i;
        FOR j:=i TO totalBlocks
           IF tempArray[j]<tempArray[swapIndex]
              HigherPriorityFound:=TRUE;
              swapIndex:=j;
              swapName:=tempArray[j];
           END IF;
        END FOR;
        IF HigherPriorityFound           
           BlockAlphaSort[i]:=BlockAlphaSort[swapIndex];
           tempArray[i]:=swapName;
           BlockAlphaSort[swapIndex]:=currentId;
           tempArray[swapIndex]:=currentString;           
        END IF;
     END FOR;
     DISPOSE(tempArray);     
  END PROCEDURE;   {SortBlocks}

  PROCEDURE SortNodes;
  VAR
    i,j,swapIndex,currentId           : INTEGER;    
    tempArray                         : strArray;
    HigherPriorityFound               : BOOLEAN; 
    currentString,swapName            : STRING;
    node                              : RBDNodeObj;
  BEGIN
   IF NodeAlphaSort<>NILARRAY
      DISPOSE(NodeAlphaSort);
   END IF;
   NEW(NodeAlphaSort,1..totalNodes);
   NEW(tempArray,1..totalNodes);
   i:=1;
   FOREACH node IN nodeGroup
     NodeAlphaSort[i]:=node.Id;
     tempArray[i]:=LOWER(node.name);
     INC(i);
   END FOREACH;
     {Sort Array}
     FOR i:=1 TO totalNodes
        currentId:=NodeAlphaSort[i];
        currentString:=tempArray[i];
        HigherPriorityFound:=FALSE;
        swapIndex:=i;
        FOR j:=i TO totalNodes
           IF tempArray[j]<tempArray[swapIndex]
              HigherPriorityFound:=TRUE;
              swapIndex:=j;
              swapName:=tempArray[j];
           END IF;
        END FOR;
        IF HigherPriorityFound           
           NodeAlphaSort[i]:=NodeAlphaSort[swapIndex];
           tempArray[i]:=swapName;
           NodeAlphaSort[swapIndex]:=currentId;
           tempArray[swapIndex]:=currentString;           
        END IF;
     END FOR;
     DISPOSE(tempArray);     
  END PROCEDURE;    {SortNodes}
  
PROCEDURE SortEvents;
  VAR
    i,j,swapIndex,currentId           : INTEGER;    
    tempArray                         : strArray;
    HigherPriorityFound               : BOOLEAN; 
    currentString,swapName            : STRING;
    event                             : RBDEventObj;
  BEGIN
   IF EventAlphaSort<>NILARRAY
      DISPOSE(EventAlphaSort);
   END IF;
   NEW(EventAlphaSort,1..totalEvents);
   NEW(tempArray,1..totalEvents);
   i:=1;
   FOREACH event IN eventGroup
     EventAlphaSort[i]:=event.Id;
     tempArray[i]:=LOWER(event.name);
     INC(i);
   END FOREACH;
   
     {Sort Array}  
     FOR i:= 1 TO totalEvents
        currentId:=EventAlphaSort[i];
        currentString:=tempArray[i];
        HigherPriorityFound:=FALSE;
        swapIndex:=i;
        FOR j:=i TO totalEvents
           IF tempArray[j]<tempArray[swapIndex]
              HigherPriorityFound:=TRUE;
              swapIndex:=j;
              swapName:=tempArray[j];
           END IF;
        END FOR;
        IF HigherPriorityFound           
           EventAlphaSort[i]:=EventAlphaSort[swapIndex];
           tempArray[i]:=swapName;
           EventAlphaSort[swapIndex]:=currentId;
           tempArray[swapIndex]:=currentString;           
        END IF;
     END FOR;
     DISPOSE(tempArray);     
  END PROCEDURE;   {SortEvents}

PROCEDURE SortHiers;
  VAR
    i,j,swapIndex,currentId           : INTEGER;    
    tempArray                         : strArray;
    HigherPriorityFound               : BOOLEAN; 
    currentString,swapName            : STRING;
    hier                              : RBDHierObj;
  BEGIN
   IF HierAlphaSort<>NILARRAY
      DISPOSE(HierAlphaSort);
   END IF;
   NEW(HierAlphaSort,1..totalHiers);
   NEW(tempArray,1..totalHiers);
   i:=1;
   FOREACH hier IN hierGroup
     HierAlphaSort[i]:=hier.Id;
     tempArray[i]:=LOWER(hier.name);
     INC(i);
   END FOREACH;
   
     {Sort Array}  
     FOR i:= 1 TO totalHiers
        currentId:=HierAlphaSort[i];
        currentString:=tempArray[i];
        HigherPriorityFound:=FALSE;
        swapIndex:=i;
        FOR j:=i TO totalHiers
           IF tempArray[j]<tempArray[swapIndex]
              HigherPriorityFound:=TRUE;
              swapIndex:=j;
              swapName:=tempArray[j];
           END IF;
        END FOR;
        IF HigherPriorityFound           
           HierAlphaSort[i]:=HierAlphaSort[swapIndex];
           tempArray[i]:=swapName;
           HierAlphaSort[swapIndex]:=currentId;
           tempArray[swapIndex]:=currentString;           
        END IF;
     END FOR;
     DISPOSE(tempArray);     
  END PROCEDURE;   {SortHiers}

       
  PROCEDURE FetchRaptorSeed (IN seedNum : INTEGER) : INTEGER;
  VAR
     SeedArray   : ARRAY INTEGER OF INTEGER;
     seed        : INTEGER;
  BEGIN
   
   NEW(SeedArray,1..110); 
 
    
   SeedArray[1]:=2116429302;SeedArray[2]:=683743814;SeedArray[3]:=964393174;
   SeedArray[4]:=1217426631;SeedArray[5]:=618433579;SeedArray[6]:=1157240309;
   SeedArray[7]:=15726055;SeedArray[8]:=48108509;SeedArray[9]:=1797920909;
   SeedArray[10]:=477424540;   

   SeedArray[11]:=1973272912;SeedArray[12]:=281629770;SeedArray[13]:=20006270;
   SeedArray[14]:=1280689831;SeedArray[15]:=2096730329;SeedArray[16]:=1933576050;
   SeedArray[17]:=913566091;SeedArray[18]:=246780520;SeedArray[19]:=1363774876;
   SeedArray[20]:=604901985;   
    
   SeedArray[21]:=1511192140;SeedArray[22]:=1259851944;SeedArray[23]:=824064364;
   SeedArray[24]:=150493284;SeedArray[25]:=242708531;SeedArray[26]:=75253171;
   SeedArray[27]:=1964472944;SeedArray[28]:=1202299975;SeedArray[29]:=233217322;
   SeedArray[30]:=1911216000;   
    
   SeedArray[31]:=726370533;SeedArray[32]:=403498145;SeedArray[33]:=993232223;
   SeedArray[34]:=1103205531;SeedArray[35]:=762430696;SeedArray[36]:=1922803170;
   SeedArray[37]:=1385516923;SeedArray[38]:=76271663;SeedArray[39]:=413682397;
   SeedArray[40]:=726466604;   

   SeedArray[41]:=336157058;SeedArray[42]:=1432650381;SeedArray[43]:=1120463904;
   SeedArray[44]:=595778810;SeedArray[45]:=877722890;SeedArray[46]:=1046574445;
   SeedArray[47]:=68911991;SeedArray[48]:=2088367019;SeedArray[49]:=748545416;
   SeedArray[50]:=622401386;   

   SeedArray[51]:=2122378830;SeedArray[52]:=640690903;SeedArray[53]:=1774806513;
   SeedArray[54]:=2132545692;SeedArray[55]:=2079249579;SeedArray[56]:=78130110;
   SeedArray[57]:=852776735;SeedArray[58]:=1187867272;SeedArray[59]:=1351423507;
   SeedArray[60]:=1645973084;   

   SeedArray[61]:=1997049139;SeedArray[62]:=922510944;SeedArray[63]:=2045512870;
   SeedArray[64]:=898585771;SeedArray[65]:=243649545;SeedArray[66]:=1004818771;
   SeedArray[67]:=773686062;SeedArray[68]:=403188473;SeedArray[69]:=372279877;
   SeedArray[70]:=1901633463;   

   SeedArray[71]:=498067494;SeedArray[72]:=2087759558;SeedArray[73]:=493157915;
   SeedArray[74]:=597104727;SeedArray[75]:=1530940798;SeedArray[76]:=1814496276;
   SeedArray[77]:=536444882;SeedArray[78]:=1663153658;SeedArray[79]:=855503735;
   SeedArray[80]:=67784357;   

   SeedArray[81]:=1432404475;SeedArray[82]:=619691088;SeedArray[83]:=119025595;
   SeedArray[84]:=880802310;SeedArray[85]:=176192644;SeedArray[86]:=1116780070;
   SeedArray[87]:=277854671;SeedArray[88]:=1366580350;SeedArray[89]:=1142483975;
   SeedArray[90]:=2026948561;   

   SeedArray[91]:=1053920743;SeedArray[92]:=786262391;SeedArray[93]:=1792203830;
   SeedArray[94]:=1494667770;SeedArray[95]:=1923011392;SeedArray[96]:=1433700034;
   SeedArray[97]:=1244184613;SeedArray[98]:=1147297105;SeedArray[99]:=539712780;
   SeedArray[100]:=1545929719;   

   SeedArray[101]:=190641742;SeedArray[102]:=1645390429;SeedArray[103]:=264907697;
   SeedArray[104]:=620389253;SeedArray[105]:=1502074852;SeedArray[106]:=927711160;
   SeedArray[107]:=364849192;SeedArray[108]:=2049576050;SeedArray[109]:=638580085;
   SeedArray[110]:=547070247;   

   seed:=SeedArray[seedNum];
   RETURN seed;
   
  END PROCEDURE; {FetchRaptorSeed}
       
       
  PROCEDURE CreateDomainTree;
  VAR
     i,j,day                                    : INTEGER;
     element                                    : ANYOBJ;
     outString,tab,today,month,year             : STRING;
     block                                      : RBDBlockObj;
     node,tempNode                              : RBDNodeObj;
     event                                      : RBDEventObj;
  BEGIN
  
     DateTime(today);
     day := STRTOINT(SUBSTR(9, 10, today));
     month := SUBSTR(5, 7, today);
     year := SUBSTR(STRLEN(today)-3, STRLEN(today), today);
     outString:="Raptor day month year = " + INTTOSTR(day) + " " + month + " " + year;
     IF DomainTree
        NEW(TreeStream);
        ASK TreeStream TO Open("reports\" + "DomainTree.TXT", Output);
        ASK TreeStream TO WriteString(outString);            
        ASK TreeStream TO WriteLn; 
        ASK TreeStream TO WriteString("devVersion= "+devVersion);   
        ASK TreeStream TO WriteLn; 
        ASK TreeStream TO WriteString("Domain Tree for "+nameOfFile);   
        ASK TreeStream TO WriteLn; 
        ASK TreeStream TO WriteLn;
     END IF;   
     IF CoreFile
        NEW(CoreStream);
        ASK CoreStream TO Open("reports\" + "CoreFile.TXT", Output);
        ASK CoreStream TO WriteString(outString);            
        ASK CoreStream TO WriteLn; 
        ASK CoreStream TO WriteString("devVersion= "+devVersion);   
        ASK CoreStream TO WriteLn; 
        ASK CoreStream TO WriteString("Kings and Serfs File for "+nameOfFile);   
        ASK CoreStream TO WriteLn; 
        ASK CoreStream TO WriteLn;
     END IF;   
  
     NEW(DescendantsGroup,1..totalNodes);
     FOR i:=1 TO totalNodes
        NEW(DescendantsGroup[i]);
     END FOR;
     FOREACH block IN blockGroup
        node := ASK root Child("RBDNode", block.EFPAnode);
        ASK DescendantsGroup[node.seqNum] TO Add(block);
     END FOREACH;
     FOREACH event IN eventGroup      
        node := ASK root Child("RBDNode", event.EFPAnode);
        ASK DescendantsGroup[node.seqNum] TO Add(event);
     END FOREACH;
     FOREACH node IN nodeGroup
        FOR j:=1 TO node.EFPAConnectOut
           tempNode:= ASK root Child("RBDNode", node.EFPAConnectTo[j]);
           ASK DescendantsGroup[tempNode.seqNum] TO Add(node);
        END FOR;
     END FOREACH;
     
     IF DomainTree
        tab:="   ";
        node := ASK root Child("RBDNode", endId);
        outString:=node.name;
        ASK TreeStream TO WriteString("nd "); 
        ASK TreeStream TO WriteString(outString); 
        ASK TreeStream TO WriteLn; 
        WriteDomains(node.seqNum,tab);
        ASK TreeStream TO Close;
        DISPOSE(TreeStream);
     END IF;
     IF CoreFile
        tab:="   ";
        node := ASK root Child("RBDNode", endId);
        outString:=node.name;
        ASK CoreStream TO WriteString("nd "); 
        ASK CoreStream TO WriteString(outString); 
        ASK CoreStream TO WriteLn; 
        WriteCores(node.seqNum,tab);
        ASK CoreStream TO Close;
        DISPOSE(CoreStream);
     END IF;
     
     FOR i:=1 TO totalNodes
         
        FOREACH block IN blockGroup
           IF (DescendantsGroup[i].Includes(block)) ;
              ASK DescendantsGroup[i] TO RemoveThis(block);
           END IF;
        END FOREACH;
        FOREACH event IN eventGroup      
           IF (DescendantsGroup[i].Includes(event)) ;
              ASK DescendantsGroup[i] TO RemoveThis(event);
           END IF;
        END FOREACH;
        FOREACH node IN nodeGroup
           IF (DescendantsGroup[i].Includes(node)) ;
              ASK DescendantsGroup[i] TO RemoveThis(node);
           END IF;
        END FOREACH;
      
        FOREACH element IN DescendantsGroup[i]               {cmc 12/13/06 this shouldn't need to be here but}
           ASK DescendantsGroup[i] TO RemoveThis(element);   {it solves a crash.  Using this section alone without}
        END FOREACH;                                        {the one above it doesn't work either (for complex rbd's}
        ASK DescendantsGroup[i] TO ObjTerminate;  
     END FOR;   
     DISPOSE(DescendantsGroup);
  END PROCEDURE;
  
  PROCEDURE WriteDomains     (IN seqIndex   : INTEGER;
                              IN tab  : STRING);
  VAR
     outString,tablet      : STRING;
     member                : RBDBasicObj;
     block                 : RBDBlockObj;
     event                 : RBDEventObj;
     node                  : RBDNodeObj;
     hier                  : RBDHierObj;
  BEGIN

     tablet:=tab;  
     FOREACH member IN DescendantsGroup[seqIndex]
        IF (OBJTYPENAME(member)="RBDBlockObj")
           block := ASK root Child("RBDBlock", member.Id);
           outString:=tab+"bl "+block.name;
           ASK TreeStream TO WriteString(outString); 
           ASK TreeStream TO WriteLn; 
        ELSIF (OBJTYPENAME(member)="RBDEventObj")
           event := ASK root Child("RBDEvent", member.Id);
           outString:=tab+"ev "+event.name;               {cmc 12/13/06}
           ASK TreeStream TO WriteString(outString); 
           ASK TreeStream TO WriteLn; 
        ELSIF (OBJTYPENAME(member)="RBDNodeObj")
           node := ASK root Child("RBDNode", member.Id);
           IF node.typeNode=5
              hier := ASK root Child("RBDHier", node.parentID);
              outString:=tab+"hr "+hier.name;
           ELSE
              outString:=tab+"nd "+node.name;
           END IF;   
           tablet:=tab+"   ";
           ASK TreeStream TO WriteString(outString); 
           ASK TreeStream TO WriteLn; 
           WriteDomains(member.seqNum,tablet)
        END IF;
     END FOREACH;     
  END PROCEDURE;
       
  PROCEDURE WriteCores       (IN seqIndex   : INTEGER;
                              IN tab        : STRING);
  VAR
     outString,tablet      : STRING;
     member                : RBDBasicObj;
     block                 : RBDBlockObj;
     event                 : RBDEventObj;
     node                  : RBDNodeObj;
     hier                  : RBDHierObj;
  BEGIN

     {tablet:=tab;} 
     FOREACH member IN DescendantsGroup[seqIndex]
        IF (OBJTYPENAME(member)="RBDBlockObj")
           block := ASK root Child("RBDBlock", member.Id);
           outString:=tab+"bl "+block.name;
           ASK CoreStream TO WriteString(outString); 
           ASK CoreStream TO WriteLn; 
        ELSIF (OBJTYPENAME(member)="RBDEventObj")
           event := ASK root Child("RBDEvent", member.Id);
           outString:=tab+"ev "+event.name;                    {cmc 12/13/06 changed block.name to event.name}
           ASK CoreStream TO WriteString(outString); 
           ASK CoreStream TO WriteLn; 
        ELSIF (OBJTYPENAME(member)="RBDNodeObj")
           node := ASK root Child("RBDNode", member.Id);
           IF node.EconnectIntoNum>1
              IF node.typeNode=5
                 hier := ASK root Child("RBDHier", node.parentID);
                 outString:=tab+"hr "+hier.name;
              ELSE
                 outString:=tab+"nd "+node.name;
              END IF;   
            {  tablet:=tab+"   "; }
              ASK CoreStream TO WriteString(outString); 
              ASK CoreStream TO WriteLn; 
              WriteCores(member.seqNum,tab+"   ")
           ELSE
              WriteCores(member.seqNum,tab)
           END IF;   
        END IF;
     END FOREACH;
  END PROCEDURE;
  
  
OBJECT RandomGenObj;
   ASK METHOD DrawNumber(IN currentDistro : INTEGER;
                         IN dataArray     : realArray;
                         IN gettingDraw   : STRING;
                         OUT drawn        : REAL);
   VAR
      negExpBox                           : HelpBoxObj; 
      button                              : ButtonObj;         
   BEGIN
      CASE currentDistro
         WHEN 1: {Beta}
            drawn := Beta (dataArray[1], dataArray[2]);
         WHEN 2: {Chi-Square}
            drawn := ChiSquare(ROUND(dataArray[1]));
         WHEN 3: {Binomial}
            drawn := FLOAT(Binomial (ROUND(dataArray[1]), dataArray[2]));
         WHEN 4: {Exponential}
            drawn := dataArray[2] + Exponential(dataArray[1]);
         WHEN 5: {Erlang}
            drawn := Erlang(dataArray[1] * dataArray[2], ROUND(dataArray[1]));
         WHEN 6: {Gamma}
            drawn := dataArray[3] + Gamma(dataArray[1] * dataArray[2], dataArray[1]);
         WHEN 7: {Lognormal}
            drawn := LogNormal(dataArray[1], dataArray[2]);
         WHEN 8: {Normal}
            drawn := Normal (dataArray[1], dataArray[2]);
         WHEN 9: {Uniform Integer}
            drawn := FLOAT(UniformInt (ROUND(dataArray[1]), ROUND(dataArray[2])));
         WHEN 10: {Uniform Real}
            drawn := UniformReal (dataArray[1], dataArray[2]);
         WHEN 11: {Pearson 5}
            drawn := Pearson5 (dataArray[2], dataArray[1], dataArray[3]);
         WHEN 12: {Pearson 6}
            drawn := Pearson6 (dataArray[3], dataArray[1], dataArray[2]);
         WHEN 13: {Poisson}
            drawn := FLOAT(Poisson (dataArray[1]));
         WHEN 14: {Triangular}
            drawn := Triangular (dataArray[1], dataArray[2], dataArray[3]);
         WHEN 15: {Weibull}
            drawn := dataArray[3] + Weibull(dataArray[1], dataArray[2]);
         WHEN 16: {Empirical}
            drawn := Empirical(dataArray);
         WHEN 17: {Bernoulli}
            drawn := FLOAT(Bernoulli(dataArray[1]));
         WHEN 19: {Fixed}
            drawn := dataArray[1];
         WHEN 20: {Error}
            drawn := 3.14159;
         WHEN 21: {Extreme Value}
            drawn := ExtremeValue(dataArray[1],dataArray[2]);
         WHEN 22: {Laplace}
            drawn := Laplace(dataArray[1],dataArray[2]);
         OTHERWISE
            NEW(message, 1..1);
            message[1] := "ERROR: Attempt to draw random number from unkown distribution!";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
      END CASE;
      IF (drawn<0.0) 
         IF (NOT negShutUp)
            NEW(negExpBox);
            ASK negExpBox TO LoadFromLibrary(dialogs,"NegExpBox");
            ASK window TO AddGraphic(negExpBox);
            dontShow := ASK negExpBox Descendant("DontShowBox", 100);
            button := ASK negExpBox TO AcceptInput();
            IF dontShow.Checked()
               negShutUp := TRUE;
            END IF;
            DISPOSE(negExpBox);
         END IF;
         drawn:=0.000001;     
      END IF;
      IF (currentDistro<>19)
         INC(randNumCount);
      END IF;   
      IF (simDetails AND (currentDistro<>19)) 
         ASK simDetailsStream TO Open("simDetails.TXT", Append);
         ASK simDetailsStream TO WriteString(REALTOSTR(SimTime)+"  "+gettingDraw+"  "+REALTOSTR(drawn));   
         ASK simDetailsStream TO WriteLn; 
         ASK simDetailsStream TO Close; 
      END IF; 

   END METHOD; {DrawNumber}

   ASK METHOD ChiSquare (IN shape : INTEGER) : REAL;
   VAR
      i     : INTEGER;
      total : REAL;
   BEGIN
     FOR i := 1 TO shape
         total := total + POWER(Normal(0., 1.), 2.);
     END FOR;
     RETURN total;
   END METHOD; {ChiSquare}

   ASK METHOD Bernoulli (IN probability : REAL):INTEGER; 
   BEGIN
      IF (probability >= 0.) AND (probability <= 1.)
         IF (UniformReal(0.,1.) <= probability)
            RETURN 1;
         ELSE
            RETURN 0;
         END IF;
      ELSE
         OUTPUT("Error:  Bernoulli probability not between 0 and 1");
      END IF;      
   END METHOD; {Bernoulli}  

   ASK METHOD Pearson5 (IN scale,shape,location : REAL) : REAL;
   VAR
      gammaDraw, mean : REAL;
   BEGIN
      IF (scale > 0.) AND (shape > 0.)
         mean := shape/scale;
         gammaDraw := Gamma(mean,shape);
         RETURN location + 1./gammaDraw;
      ELSE
         NEW(message, 1..1);
         message[1] := "Shape or scale for Pearson 5 draw <= 0!";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      END IF;                                                       
   END METHOD; {Pearson5}

   ASK METHOD Pearson6 (IN scale,shape,shape2 : REAL) : REAL;
   VAR
      gamma1, gamma2 : REAL;
   BEGIN
      gamma1 := Gamma (shape, shape);
      gamma2 := Gamma (shape2, shape2);
      RETURN (EXP(LN(scale) + LN(gamma1) - LN(gamma2)));
   END METHOD; {Pearson6}

   ASK METHOD Empirical (IN data          : realArray) : REAL;
   VAR
      i                    : INTEGER;
      X,drawNum,position,n : REAL;
   BEGIN
      X := UniformReal(0.,1.);
      n := FLOAT(HIGH(data));
      IF n=1.0
         drawNum := data[1];
      ELSE
         position := (n - 1.) * X;
         i := TRUNC(position) + 1;
         drawNum := data[i] + ((position - FLOAT(i) + 1.) * (data[i + 1] - data[i]));
      END IF;
      RETURN drawNum;
   END METHOD; {Empirical}

   ASK METHOD LogNormal (IN mean, stDev : REAL) : REAL;
   VAR
      normalMean, normalStDev,
      logMean2, logVar         : REAL;
   BEGIN
      logMean2 := POWER(mean, 2.);
      logVar := POWER(stDev, 2.);
      normalMean := LN(logMean2/SQRT(logVar + logMean2));
      normalStDev := SQRT(LN((logVar + logMean2)/logMean2));
      RETURN EXP(Normal(normalMean, normalStDev)); 
   END METHOD; {LogNormal}

   ASK METHOD Poisson (IN mu : REAL) : INTEGER;
   
   VAR
      x, y, sigma : REAL;      
      returnValue : INTEGER;
   BEGIN
      IF mu <= 0.0  {mu should not be less than zero}
         NEW(message, 1..1);
         message[1] := "Poisson mu less than or equal to zero!";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      ELSIF mu > 6.0  {Where the MODSIM II Idiots decided the Poisson distro approachs the normal}
         sigma := SQRT(mu);
         REPEAT
            returnValue := ROUND(Normal(mu, sigma));
         UNTIL returnValue >= 0;
      ELSE
         y := EXP(-mu);
         x := 1.0;
         LOOP
            x := x * Sample();
            IF x < y
               EXIT;
            ELSE
               returnValue := returnValue + 1;
            END IF;
         END LOOP;
      END IF;
      RETURN returnValue;
   END METHOD; {Poisson}
   
   ASK METHOD ExponentialPower  (IN scale,shape,location : REAL)       : REAL;
   VAR
   BEGIN
      RETURN(3.14159);
   
   END METHOD;
   


   ASK METHOD ExtremeValue (IN  scale,location           : REAL)       : REAL;
   VAR
      X                     : REAL;
   BEGIN
      X := UniformReal(0.,1.);
      RETURN(location - scale*LN(-LN(X)));    {tony}
   END METHOD;
   
   ASK METHOD Laplace      (IN  scale,location           : REAL)       : REAL;
   VAR
      X                     : REAL;
   BEGIN
      X := UniformReal(0.,1.);
      IF (X<0.5)
         RETURN(location + scale*LN(2.0*X));  {tony - my own calc based on F(x)} 
      ELSE
         RETURN(location - scale*LN(2.0-2.0*X));
      END IF;   
   END METHOD;
      
END OBJECT; {RandomGenObj}
              
END MODULE.

