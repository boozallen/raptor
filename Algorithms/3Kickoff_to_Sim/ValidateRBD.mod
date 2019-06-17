

PROCEDURE ValidateRBD(OUT valid  :BOOLEAN);
VAR
   j,errors,firstBackNode,completePercent,
   newCompletePercent,errorNodeId, oldActiveWindow             : INTEGER;
   goodPaths                                  : REAL;
   OKToSimulate,goodNodeValue, goingBack, notDone, changedWindow      : BOOLEAN;
   button                                     : ButtonObj; 
   pool                                       : SparePoolObj;
   KofNlabel                                  : TextObj;
   text                                       : ARRAY INTEGER OF STRING;
   tempBlock                                  : RBDBlockObj;
   tempEvent                                  : RBDEventObj;
   tempNode                                   : RBDNodeObj;
   tempHier                                   : RBDHierObj;
   value                                      : ValueBoxObj;
   label                                      : LabelObj;  
   nextString                                 : STRING;
   dialogBox                                  : HelpBoxObj;
BEGIN
   valid:=FALSE;
   NEW(message, 1..1);
   NEW(text,1..40);
   ASK menubar TO Disable1Thru8;
   typeOfCursor := dialogC;
   OKToSimulate := TRUE;
   ASK window TO ShowStatus(0,"Analyzing System...");
   ASK window TO ShowStatus(1,"0% Complete");
   IF ((totalBlocks = 0) AND (totalEvents = 0))
      OKToSimulate := FALSE;
      INC(errors);
      text[errors] := "There must be at least 1 block or event to simulate!     ";
   END IF;
   i:=1;
   FOREACH node IN nodeGroup
      ASK node TO SetHasDepObjects(FALSE);
      ASK node TO SetGoBack(FALSE);  {used later}
      IF node.connectIntoNum < 1
         IF ((node.typeNode <> 1) AND (node.typeNode <> 4))
            OKToSimulate := FALSE;
            INC(errors);
            IF errors = 40
               text[errors] := "Additional errors have been detected but are not shown.     "; 
            ELSIF errors > 40 
               errors := 40;
            ELSE
               text[errors] := "To simulate, node '" + node.name + "' must connect from something!     ";
            END IF;
         END IF; 
      END IF;
      IF node.connectOutOfNum < 1
         IF ((node.typeNode <> 3) AND (node.typeNode <> 5))
            OKToSimulate := FALSE;
            INC(errors);
            IF errors = 40
               text[errors] := "Additional errors have been detected but are not shown.     ";
            ELSIF errors > 40 
               errors := 40;
            ELSE
               text[errors] := "To simulate, node '" + node.name + "' must connect to something!     ";
            END IF;
         END IF;
      END IF;
      IF node.usesPhasing
         IF node.phase = NILARRAY
            ASK node TO SetPhases(TRUE,NILARRAY);
         END IF;
         phasingInUse := TRUE;
      END IF;
      IF ROUND(2.0*FLOAT(i)/FLOAT(totalNodes)) <> completePercent
         completePercent := ROUND(2.0*FLOAT(i)/FLOAT(totalNodes));
         ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
      END IF;
      IF OKToSimulate
         IF (node.typeNode=4)
           ASK node TO SetEconnectIntoNum(1);
           ASK node TO SetEconnectOutOfNum(node.connectOutOfNum);
         ELSIF (node.typeNode=5)
           ASK node TO SetEconnectIntoNum(node.connectIntoNum);
           ASK node TO SetEconnectOutOfNum(1);
         ELSE
           ASK node TO SetEconnectIntoNum(node.connectIntoNum);
           ASK node TO SetEconnectOutOfNum(node.connectOutOfNum);
         END IF;
         ASK node TO ResetConnectToInfo;
         ASK node TO ResetLocalPathArrays;
         ASK node TO SetSequenceNum(i);
      END IF;
      IF ROUND(2.0*FLOAT(i)/FLOAT(totalNodes)) <> completePercent
         completePercent := ROUND(2.0*FLOAT(i)/FLOAT(totalNodes));
         ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
      END IF;
      INC(i);
   END FOREACH;
   {2% complete}
   i:=1;
   FOREACH block IN blockGroup
      ASK block TO SetHasDepObjects(FALSE);
      IF block.connectIntoNum < 1
         OKToSimulate := FALSE;
         INC(errors);
         IF errors = 40
            text[errors] := "Additional errors have been detected but are not shown.     "; 
         ELSIF errors > 40 
            errors := 40;
         ELSE
            text[errors] := "To simulate, block '" + block.name + "' must connect from something!     ";
         END IF;
      END IF;
      IF block.connectOutOfNum < 1
         OKToSimulate := FALSE;
         INC(errors);
         IF errors = 40
            text[errors] := "Additional errors have been detected but are not shown.     "; 
         ELSIF errors > 40 
            errors := 40;
         ELSE
            text[errors] := "To simulate, block '" + block.name + "' must connect to something!     ";
         END IF;
      END IF;
      IF block.usesPhasing
         IF block.phaseValue = NILARRAY
            ASK block TO SetPhases(TRUE,NILARRAY, NILARRAY);
         END IF;
         phasingInUse := TRUE;
      END IF;  
      IF OKToSimulate
         ASK block TO SetEconnectIntoNum(block.connectIntoNum);
         ASK block TO SetEconnectOutOfNum(block.connectOutOfNum);
         ASK block TO ResetConnectToInfo; 
         ASK block TO SetGoodPathsRequired(1);
         ASK block TO SetSequenceNum(i);
      END IF;
      newCompletePercent:=ROUND(2.0+2.0*FLOAT(i)/FLOAT(totalBlocks));
      IF newCompletePercent <> completePercent
         completePercent := newCompletePercent;
         ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
      END IF;
      INC(i);
   END FOREACH;
   {4% complete}
   i:=1;
   FOREACH event IN eventGroup
      ASK event TO SetHasDepObjects(FALSE);
      IF event.connectIntoNum < 1
         OKToSimulate := FALSE;
         INC(errors);
         IF errors = 40
            text[errors] := "Additional errors have been detected but are not shown.     "; 
         ELSIF errors > 40 
            errors := 40;
         ELSE
            text[errors] := "To simulate, event '" + event.name + "' must connect from something!     ";
         END IF;
      END IF;
      IF event.connectOutOfNum < 1
         OKToSimulate := FALSE;
         INC(errors);
         IF errors = 40
            text[errors] := "Additional errors have been detected but are not shown.     "; 
         ELSIF errors > 40 
            errors := 40;
         ELSE
            text[errors] := "To simulate, event '" + event.name + "' must connect to something!     ";
         END IF;
      END IF;
      IF event.usesPhasing
         IF event.phaseValue = NILARRAY
            ASK event TO SetPhases(TRUE,NILARRAY, NILARRAY);
         END IF;
         phasingInUse := TRUE;
      END IF;
      IF OKToSimulate
         ASK event TO SetEconnectIntoNum(event.connectIntoNum);
         ASK event TO SetEconnectOutOfNum(event.connectOutOfNum);
         ASK event TO ResetConnectToInfo;         
         ASK event TO SetSequenceNum(i);
         ASK event TO SetGoodPathsRequired(1);
      END IF;
      newCompletePercent:=ROUND(4.0+2.0*FLOAT(i)/FLOAT(totalEvents));
      IF newCompletePercent <> completePercent
         completePercent := newCompletePercent;
         ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
      END IF;
      INC(i);
   END FOREACH;
   {6% complete}
   i:=1;
   FOREACH hier IN hierGroup
      IF hier.connectIntoNum < 1
         OKToSimulate := FALSE;
         INC(errors);
         IF errors = 40
            text[errors] := "Additional errors have been detected but are not shown.     "; 
         ELSIF errors > 40 
            errors := 40;
         ELSE
            text[errors] := "To simulate, hier '" + hier.name + "' must connect from something!     ";
         END IF;
      END IF;
      IF hier.connectOutOfNum < 1
         OKToSimulate := FALSE;
         INC(errors);
         IF errors = 40
            text[errors] := "Additional errors have been detected but are not shown.     "; 
         ELSIF errors > 40 
            errors := 40;
         ELSE
            text[errors] := "To simulate, hier '" + hier.name + "' must connect to something!     ";
         END IF;
      END IF;
      IF OKToSimulate
         ASK hier TO SetSequenceNum(i);
      END IF;
      newCompletePercent:=ROUND(6.0+2.0*FLOAT(i)/FLOAT(totalHiers));
      IF newCompletePercent <> completePercent
         completePercent := newCompletePercent;
         ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
      END IF;
      INC(i);
   END FOREACH;
   {8% complete}
   IF errors > 1
      NEW(message, 1..errors+2);
      message[1] := "The following errors must be corrected:   ";
      message[2] := "";
      FOR i := 1 TO errors
         message[i+2] := "   "+INTTOSTR(i)+". "+text[i];
      END FOR;
      result := SendAlert(message, FALSE, FALSE, TRUE);
      DISPOSE(message);
   ELSIF errors = 1
      NEW(message, 1..1);
      message[1] := text[1];
      result := SendAlert(message, FALSE, FALSE, TRUE);
      DISPOSE(message);
   END IF;
   ClearAllBlocks;
   changedZoom := FALSE;
   changedWindow := FALSE; {zoomissue - nodepaths}
   oldActiveWindow := activeWindow;
   IF activeWindow <> 0
      tempHier := ASK root Child("RBDHier", activeWindow);
      oldx := tempHier.xOrigin;
      oldy := tempHier.yOrigin;
      oldCusVal := tempHier.zoom;
   ELSE
      oldx := xOrigin;
      oldy := yOrigin;
      oldCusVal := cusZoomVal;
   END IF;
   
   {At this point the basic ground rules of a valid RBD have been met, the next section checks if the k of n 
    info has been specified for all nodes}
   
   IF OKToSimulate AND (NOT configFrozen)
      node := nodeGroup.First;
      j := 1; {don't change to i as it is a global variable and gets changed in the call to ChangeWindow below}
      notDone := TRUE;
      WHILE(notDone)
         KofNlabel := ASK node Child("RBDNodeKofN", 0);
         IF ((node.typeNode = 2) AND (node.connectIntoNum <> 1) AND 
            ((KofNlabel.String="") OR (KofNlabel.String=" "))) OR (node.goBack)
            ASK node TO SetGoBack(TRUE);
            IF firstBackNode=0
               firstBackNode:=j;
            END IF;
            IF (node.parentID <> activeWindow)
               changedWindow := TRUE; {zoomissue - nodepaths}
               IF node.parentID = 0
                  ChangeWindow(0, 0);
               ELSE
                  tempHier := ASK root Child("RBDHier", node.parentID);
                  ChangeWindow(tempHier.Id, tempHier.level);
               END IF;
            END IF;
            SelectNode;
            dontChangeXY := TRUE; {zoomissue - nodepaths} {used in SetView}
            SetView(20., (node.xPosition-20./2.),(node.yPosition+20.*13.1/40.)); 
            dontChangeXY := FALSE; {used in SetView}
            changedZoom := TRUE;
            NEW(dialogBox);
            ASK dialogBox TO LoadFromLibrary(dialogs,"NodePathBox");
            ASK window TO AddGraphic(dialogBox);
            label := ASK dialogBox Child("Text1Label",831);
            value := ASK dialogBox Child("PathValueBox", 833);
            ASK label TO SetLabel("Node '"+node.name + "' has " + INTTOSTR(node.connectIntoNum) + " paths connecting into it.");
            IF node.goodPaths < 1
               ASK value TO SetValue(1.);
            ELSE
               ASK value TO SetValue(FLOAT(node.goodPaths));
            END IF;
            ASK dialogBox TO SetPositioning(BottomLeft);
            ASK dialogBox TO SetTranslation(20., 10.);
            ASK dialogBox TO Draw;
            IF j = firstBackNode
               button := ASK dialogBox Child("GoBack", 0);
               ASK button TO Deactivate;
            END IF;
            REPEAT
               button := ASK dialogBox TO AcceptInput();
               ASK value TO ReceiveFocus;
               IF ASK button ReferenceName = "OKButton";
                  goodPaths := ASK value Value();
                  IF (TRUNC(goodPaths) > node.connectIntoNum) OR (TRUNC(goodPaths) < 1)
                     message[1] := "Good paths (k) must be between 1 and "+INTTOSTR(node.connectIntoNum)+"!     ";
                     result := SendAlert(message, FALSE, FALSE, TRUE);
                  ELSIF node.coldStandby AND (TRUNC(goodPaths) > node.KStar) {enforce: k<=k*}
                     message[1] := "Good paths (k) must be less than or equal to maximum number running (k* = " + INTTOSTR(node.KStar) + ")!     ";
                     result := SendAlert(message, FALSE, FALSE, TRUE);
                  ELSE
                     goodNodeValue := TRUE;
                     ASK node TO SetGoodPaths(TRUNC(goodPaths));
                     ASK node TO SetKofN(node.goodPaths, node.connectIntoNum);
                  END IF;
                  {IF (TRUNC(goodPaths) <= node.connectIntoNum) AND (TRUNC(goodPaths) >= 1)
                     goodNodeValue := TRUE;
                     ASK node TO SetGoodPaths(TRUNC(goodPaths));
                     ASK node TO SetKofN(node.goodPaths, node.connectIntoNum);
                  ELSE
                     message[1] := "Good paths must be between 1 and "+INTTOSTR(node.connectIntoNum)+"!     ";
                     result := SendAlert(message, FALSE, FALSE, TRUE);
                  END IF;}
               ELSIF ASK button ReferenceName = "GoBack"
                  REPEAT                        
                     DEC(j);
                     node := nodeGroup.Prev(node);
                     goingBack := TRUE;
                  UNTIL (node.goBack);
                  DEC(j);
                  goodNodeValue := TRUE;
               ELSIF ASK button ReferenceName = "CancelButton"
                  OKToSimulate := FALSE;
                  goodNodeValue := TRUE;
                  dontChangeXY := TRUE; {zoomissue - nodepaths}{used in SetView and ChangeWindow (calls SetView)}
                  IF changedZoom
                     SetView(oldCusVal, oldx, oldy);
                     changedZoom := FALSE;
                  END IF;                
                  IF changedWindow
                     IF oldActiveWindow = 0
                        ChangeWindow(0, 0);
                     ELSE
                        tempHier := ASK root Child("RBDHier", oldActiveWindow);
                        ChangeWindow(tempHier.Id, tempHier.level);
                     END IF;
                     changedWindow := FALSE;
                  END IF;
                  dontChangeXY := FALSE; {used in SetView and ChangeWindow (calls SetView)}
                  ASK menubar TO Enable2Thru6;
                  typeOfCursor := nilC;
                  DISPOSE(dialogBox);
                  ClearAllBlocks;
                  RETURN;
               END IF;
            UNTIL goodNodeValue;
            goodNodeValue := FALSE;
            DISPOSE(dialogBox);
            ClearAllBlocks;
         ELSIF node.typeNode = 1
            ASK node TO SetGoodPaths(0);
         ELSIF node.typeNode = 3
            ASK node TO SetGoodPaths(1);
         ELSIF (node.typeNode = 2) AND (node.connectIntoNum = 1)
            ASK node TO SetGoodPaths(1);
         END IF;
         newCompletePercent:=ROUND(8.0+2.0*FLOAT(i)/FLOAT(totalNodes));
         IF newCompletePercent <> completePercent
            completePercent := newCompletePercent;
            ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
         END IF;
         IF j = totalNodes
            notDone := FALSE;
         END IF;
         IF ((NOT goingBack) AND (j <= totalNodes))
            node := nodeGroup.Next(node);
         ELSE
            goingBack := FALSE;
         END IF;
         INC(j);
      END WHILE;
      {10% complete}
      dontChangeXY := TRUE; {zoomissue - nodepaths} {used in SetView and ChangeWindow (calls SetView)}
      IF changedZoom
         SetView(oldCusVal, oldx, oldy);
         changedZoom := FALSE;
      END IF;                
      IF changedWindow
         IF oldActiveWindow = 0
            ChangeWindow(0, 0);
         ELSE
            tempHier := ASK root Child("RBDHier", oldActiveWindow);
            ChangeWindow(tempHier.Id, tempHier.level);
         END IF;
         changedWindow := FALSE;
      END IF;
      dontChangeXY := FALSE; {used in SetView and ChangeWindow (calls SetView)}
   END IF;
   
   {all k of n info is now set, the next section will analyze the rbd and figure out which elements
    connect to each other}
   IF OKToSimulate
      IF diagnostics 
         ASK diagnosticsStream TO Open(diagFile, Append);
         DateTime(nextString);
         nextString:="Start Pre-Sim Analysis" + nextString;
         ASK diagnosticsStream TO WriteString(nextString);   
         ASK diagnosticsStream TO WriteLn; 
         ASK diagnosticsStream TO Close; 
      END IF; 
      NEW(linkMessage);
      ASK linkMessage TO LoadFromLibrary(images,"LinkText");
      ASK window TO AddGraphic(linkMessage);
      linkText := ASK linkMessage Child("LinkText", 851);
      ASK linkText TO SetText("Preparing to simulate...  please wait...");
      ASK linkText TO SetFont(SystemText);
      ASK linkText TO SetColor(Black);
      ASK linkMessage TO DisplayAt(300.,619.);
      ASK window TO SetSysCursor(BusyCursor);
      totalPools := poolGroup.numberIn;
      IF totalPools > 0
         NEW(PoolArray,1..totalPools);
         i := 1;
         FOREACH pool IN poolGroup
            NEW(PoolArray[i]);
            PoolArray[i] := pool;
            INC(i);
         END FOREACH;
         i:=1;
         FOREACH block IN blockGroup
            FOR j := 1 TO poolGroup.numberIn
               IF ((PoolArray[j].sparingType = SparePool) AND (block.sparingType = SparePool))
                  IF PoolArray[j].poolName = block.poolName
                     ASK block TO SetPoolNum(j,TRUE);
                  END IF;
               END IF;
               IF ((PoolArray[j].sparingType = ColdPool) AND (block.sparingType = ColdPool))
                  IF PoolArray[j].poolName = block.poolName
                     ASK block TO SetPoolNum(j, TRUE);
                  END IF;
               END IF;
               IF ((PoolArray[j].sparingType = Resource) AND (block.numDiffRes=1))
                  IF PoolArray[j].poolName = block.res1Name
                     ASK block TO SetPoolNum(j, FALSE);
                  END IF;
               END IF;
            END FOR;
            newCompletePercent:=ROUND(10.0+5.0*FLOAT(i)/FLOAT(totalBlocks));
            IF newCompletePercent <> completePercent
               completePercent := newCompletePercent;
               ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
            END IF;
            INC(i);
         END FOREACH;    
      END IF;
      completePercent := 15;
      {15% complete}
      ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
      
      {This section bypasses the hierarchy elements}
      i:=1;
      FOREACH link IN linkGroup
         ASK link TO SetEconnectToId(link.connectToId); 
         ASK link TO SetEconnectFromId(link.connectFromId); 
         IF (link.connectTRef="RBDHier")
            hier:=ASK root Child("RBDHier", link.connectToId);  
            ASK link TO SetEconnectTRef("RBDNode");
            ASK link TO SetEconnectToId(hier.inID); 
         ELSE
            ASK link TO SetEconnectTRef(link.connectTRef);
         END IF;      
         IF (link.connectFRef="RBDHier")
            hier:=ASK root Child("RBDHier", link.connectFromId); 
            ASK link TO SetEconnectFRef("RBDNode");
            ASK link TO SetEconnectFromId(hier.outID);
         ELSE
            ASK link TO SetEconnectFRef(link.connectFRef);
         END IF;
         newCompletePercent:=ROUND(15.0+5.0*FLOAT(i)/FLOAT(totalLinks));
         IF newCompletePercent <> completePercent
            completePercent := newCompletePercent;
            ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
         END IF;
         INC(i);
      END FOREACH;
      {20% complete}
   
   
      {This section determines the local arrays inPathsArray, outPathsArray and 
        connectToIds based on the non-hierarchy Engine RBD}
      i:=1;
      FOREACH link IN linkGroup                  
         IF (link.EconnectFRef="RBDBlock")
             block := ASK root Child("RBDBlock", link.EconnectFromId);
             ASK block TO UpdateConnectToInfo(link.Id,link.EconnectToId, link.EconnectTRef);
         ELSIF (link.EconnectFRef="RBDEvent")
             event := ASK root Child("RBDEvent", link.EconnectFromId);
             ASK event TO UpdateConnectToInfo(link.Id,link.EconnectToId, link.EconnectTRef);
         ELSIF (link.EconnectFRef="RBDNode")
             node := ASK root Child("RBDNode", link.EconnectFromId);
             ASK node TO UpdateConnectToInfo(link.Id,link.EconnectToId, link.EconnectTRef);
             IF capacityAnalysis
                ASK node TO UpdateOutPathsArray(link.Id);
             END IF;
         END IF;         
         IF (link.EconnectTRef="RBDNode")
             node := ASK root Child("RBDNode", link.EconnectToId);
             ASK node TO UpdateInPathsArray(link.Id);
         END IF;
         newCompletePercent:=ROUND(20.0+20.0*FLOAT(i)/FLOAT(totalLinks));
         IF newCompletePercent <> completePercent
            completePercent := newCompletePercent;
            ASK window TO ShowStatus(1,INTTOSTR(completePercent)+"% Complete");
         END IF;
         INC(i);
      END FOREACH;
      {40% complete}
     
      IF simDetails 
         ASK simDetailsStream TO Open(installPath + "simDetails.TXT", Append);
         ASK simDetailsStream TO WriteString("Calling Procedure RunEFPA......");   
         ASK simDetailsStream TO WriteLn; 
         ASK simDetailsStream TO Close; 
      END IF; 
   {***}
      RunEFPA(errorNodeId);
      IF diagnostics 
         ASK diagnosticsStream TO Open(diagFile, Append);
         DateTime(nextString);
         nextString:="End Pre-Sim Analysis" + nextString;
         IF errorNodeId <> 0
             nextString:=nextString + "  -" + "Errors Detected";
         ELSE
             nextString:=nextString + "  -" + "No Errors Detected";
         END IF;
         ASK diagnosticsStream TO WriteString(nextString);   
         ASK diagnosticsStream TO WriteLn; 
         ASK diagnosticsStream TO Close; 
      END IF; 
      IF errorNodeId <> 0
         OKToSimulate := FALSE;
         tempBlock := ASK root Child("RBDBlock", errorNodeId);
         tempEvent := ASK root Child("RBDEvent", errorNodeId);
         tempNode := ASK root Child("RBDNode", errorNodeId);
         tempHier := ASK root Child("RBDHier", errorNodeId);
         IF tempBlock <> NILOBJ
            message[1] := "Block '"+tempBlock.name+"' is in an infinite loop which is an invalid RBD configuration!     ";
         ELSIF tempEvent <> NILOBJ
            message[1] := "Event '"+tempEvent.name+"' is in an infinite loop which is an invalid RBD configuration!     ";
         ELSIF tempNode <> NILOBJ
            message[1] := "Node '"+tempNode.name+"' is in an infinite loop which is an invalid RBD configuration!     ";
         ELSIF tempHier <> NILOBJ
            message[1] := "Hier '"+tempHier.name+"' is in an infinite loop which is an invalid RBD configuration!     ";
         END IF;   
         result := SendAlert(message, FALSE, FALSE, TRUE);
      END IF;
      DISPOSE(linkMessage);
      ASK window TO SetSysCursor(NormalCursor);
      valid:=OKToSimulate;
   END IF;
   typeOfCursor := nilC;
   IF NOT valid
      ASK menubar TO Enable2Thru6;
   END IF;

END PROCEDURE;   {ValidateRBD}

