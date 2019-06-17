{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Implementation Module : FileFx                                                +}
{+  Author        : Chuck Carter/Steve Brown/Tony Malerich                        +}
{+  Last Modified : October 2008   CMC                                            +}
{+  Description   : This module handles all io functions involved with opening,   +}
{+                  saving, or closing .RBD and .CFG (except Prefs) files.        +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

IMPLEMENTATION MODULE FileFx; 

FROM IOMod    IMPORT StreamObj, FileUseType(Input, Output, Update, Append),ReadKey;
FROM Menu     IMPORT MenuItemObj;
FROM Objects  IMPORT intArray,boolArray,strArray,ALL SparingType,RBDBlockObj,RBDNodeObj,LinkObj,SparePoolObj,
                     RapTriggerObj,poolGroup,triggerGroup,arrowhead,ALL directionType, totalSpares,totalRes,factBools, 
                     factInts,factReals,factStrs,factFparams,factRparams,factPreParams,factPostParams,factPMParams,
                     factSparing,PhaseObj,InitFactorySettings,RBDHierObj,spareList,resList,trigList,deferTrig,RBDEventObj,
                     RBDBasicObj;
FROM GTypes   IMPORT TextBufferType, ALL SysCursorType, PointArrayType;
FROM Intract  IMPORT SendAlert, HelpBoxObj,GetNumParams,GetSpareList,GetResList,GetTrigList;
FROM Display  IMPORT menubar,dTimeTrunc,dTimeStartTime,dCycleStartTime,dFailTrunc,dCycleTrunc,root,grid,dFailStartTime,
                     dNumberOfRuns,dTimeSlice, cusZoomVal, yMin, yMax,xOrigin,yOrigin,systemUnits,sysStreams,window,
                     systemImage,images,dialogs,totalLinks,flowGenerated,openMenuFile,totalTriggers,systemRedCost,
                     activePhases,phaseTimes,sysLostCost,compileType,weakAnalysis,costAnalysis,capacityAnalysis,
                     devVersion, SetView,defaultBlock,dZeroFail,reportPath,libraryPath,rbdPath,nameOfFile,weakLinkAnalType,
                     GYthreshold,YRthreshold,demoCrippleLimit,phaseObjArray,diagnosticsStream,diagFile,diagnostics,AddWindow,
                     sysComment,startId,endId,nextId,nextLinkId,blockGroup,eventGroup,nodeGroup,hierGroup,linkGroup,
                     AddHier,importing,levelLimit,deepestLevel,activeWindow,somethingChanged,termType,ClearPasteBuffer,
                     studentCrippleLimit,simOptionChanged;          
FROM Form     IMPORT FileDialogBoxObj;
FROM Image    IMPORT ImageObj;
FROM OSMod    IMPORT GetProgDir,FileExists;
FROM Runsim   IMPORT FinalArray;
FROM Button   IMPORT ButtonObj;
FROM UtilMod  IMPORT DateTime;
FROM GrpMod   IMPORT QueueObj;
FROM Menubar  IMPORT ResetNewFile;
FROM Text     IMPORT TextObj;


VAR
   message                : TextBufferType;
   dontClose,studentFile  : BOOLEAN;


PROCEDURE OpenFile(IN    startOpen,append                                          : BOOLEAN;
                   INOUT totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents  : INTEGER;
                   IN    appendingLevel                                            : INTEGER;
                   INOUT gridIsOn, fileIsOpen                                      : BOOLEAN;
                   INOUT fileName, pathName, filter                                : STRING;
                   OUT   nixed                                                     : BOOLEAN);
VAR
  pathString,fromRef,toRef,extension,nextString,nextString1,version,defFile        : STRING;
  i,j,k,numEmpParam,dotPosition,rap4ColdPools,numSpPools,numResPools,phaseGroups,
  phaseRemainder,nextInt,nextInt1,nextInt2,nextInt3,nextInt4,nextInt5,nextInt6,nextInt7,
  existingBlocks,existingNodes,existingHiers,existingLinks,trigsToRead,numPhases,
  blocksEvents,existingEvents,appendNum,sparesToRead,resToRead,appendingDepth,
  defaultToRead,importHierId                                                       : INTEGER;
  linkStartX,linkStartY,linkEndX,linkEndY,newSpArr,emerTime,nextReal,spCost,
  SLOtime,initCst,fixedCst,timeCst,emerCst,nextReal1,nextReal2,nextReal3,nextReal4,
  nextReal5                                                                        : REAL;
  cancelled,saveCancelled,validFileName,goodFile,RSO,ESO,SLO,phases,newBool,
  validVersion,foundStart,foundEnd,keepPhasing,failEmp,repEmp,sbMessage,
  pearson6Used,addPoolObj                                                          : BOOLEAN;
  saveFile, empFile                                                                : StreamObj;
  block                                                                            : RBDBlockObj;
  event                                                                            : RBDEventObj;
  node                                                                             : RBDNodeObj;
  hier,papaHier                                                                    : RBDHierObj;
  link                                                                             : LinkObj;
  pool                                                                             : SparePoolObj;
  trig                                                                             : RapTriggerObj;
  phase                                                                            : PhaseObj;
  linkPoints                                                                       : PointArrayType;
  failVars,repVars,preVals,postVals,pmVals,posArray,realsArray,params              : realArray;
  boolsArray,isBlockArray,usesPhasingArray                                         : boolArray;
  idsArray,intsArray,phaseArray                                                    : intArray;
  stringArray,typeArray                                                            : strArray;
  sparing                                                                          : SparingType;
  temp,blockImage                                                                  : ImageObj;
  changedArray                                                                     : ARRAY INTEGER, INTEGER OF STRING;  
  hierFileGp                                                                       : QueueObj;
BEGIN
   IF (NOT append)
      CloseFFile(gridIsOn,fileIsOpen,saveCancelled,fileName,pathName,filter,totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents);
   ELSE
      saveCancelled:=FALSE;
      openMenuFile:=FALSE;
   END IF;           
   IF NOT saveCancelled
      IF NOT openMenuFile
         REPEAT
            IF startOpen = FALSE;
               GetFileName(fileName, pathName, "*.rbd", "Select File Name");
               IF fileName = "NoFile" 
                  cancelled := TRUE;
                  EXIT;                                                                     
               END IF;   
               dotPosition := POSITION(fileName, ".");
               extension := SUBSTR((dotPosition + 1), (dotPosition + 3), fileName);
               extension := LOWER(extension);
            END IF;
            IF (extension <> "rbd") OR (dotPosition = 0)
               NEW(message, 1..1);
               message[1] := "Not a valid RBD file!     ";
               result := SendAlert(message, FALSE, FALSE, TRUE);
               DISPOSE(message);
               validFileName := FALSE;
            ELSE
               validFileName := TRUE;
               pathString := pathName + fileName;
               NEW(saveFile);
               ASK saveFile TO Open(pathString, Input);       {cmc 10/8/08}
               IF (saveFile.ioResult <> 0) 
                   NEW(message, 1..3);
                   message[1] := "RAPTOR cannot open the document '"+pathString+"'.";
                   message[2] := "It may have been moved or deleted from the current directory.     ";                   
                   result := SendAlert(message, FALSE, FALSE, TRUE);
                   DISPOSE(message);
                   startOpen := FALSE;
               ELSE
                  ASK saveFile TO Close;
                  ASK saveFile TO Open(pathString, Input);
               END IF;
            END IF;
         UNTIL ((validFileName) AND (saveFile.ioResult = 0));
      ELSE
         NEW(saveFile);
         ASK saveFile TO Open(menuPath+menuFile, Input);     {cmc 10/8/08}
         dotPosition := POSITION(menuFile, ".");
         IF (saveFile.ioResult <> 0)
            NEW(message, 1..3);
            message[1] := "RAPTOR cannot open the document '"+menuPath+menuFile+"'.";
            message[2] := "It may have been moved or deleted from the current directory.     ";
                                             {cmc 10/8/08}
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
            ASK window TO SetTitle(versionStr + " - ");
            ASK menubar TO Enable(6);
            ASK window TO Draw;
            ResetNewFile;
            RETURN;
         ELSE
            ASK saveFile TO Close;
            ASK saveFile TO Open(menuPath+menuFile, Input);               
         END IF;
         pathName := menuPath;
         fileName := menuFile;
      END IF;
     IF NOT cancelled
        {at this point a valid RBD file is open}
        ASK window TO SetSysCursor(BusyCursor);
        IF gridIsOn
           ASK grid TO Colorize("Normal");
        ELSE
           ASK grid TO Colorize("Hide");
        END IF;
        fileIsOpen := TRUE;        
        ASK saveFile TO ReadString(version);
        validVersion:=TRUE;
        studentFile := FALSE;  
        IF (SUBSTR(1,8,version)="Version7")
           IF (  (STRTOREAL (SUBSTR(10,12,version))) >  6.3  )
              rapVersion:=7;
              nextString:=SUBSTR(10,12,version);
              nextReal:= STRTOREAL(nextString);
              IF nextReal < 6.5
                 validVersion:=FALSE;
                 NEW(message, 1..4);
                 message[1] := "                  Raptor can not open this file!!!   ";
                 message[2] := "                             BETA ALERT!      ";
                 message[3] := "This beta version file is not supported. If you would like to     ";
                 message[4] := "use this file contact Mr. Kenneth Murphy at RAPTORTECH@arinc.com.";
                 ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                    existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
                 RETURN;                   
              END IF;
              IF ((nextReal = 6.5) AND (compileType = "demo"))
                 validVersion:=FALSE;
               NEW(message, 1..3);
               message[1] := "This is the demo version of Raptor.  It ";
               message[2] := "is incompatible with this file version,";
               message[3] := "and is unable to open this RBD";
               ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                 existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
               CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
               RETURN;
              END IF;
              IF ((nextReal > 6.5) AND (compileType = "gmd"))
                 validVersion:=FALSE;
                 NEW(message, 1..4);
                 message[1] := "                  Raptor can not open this file!!!   ";
                 message[2] := "                             FILE ALERT!      ";
                 message[3] := "This file is not supported by Raptor 6.5. If you would like to     ";
                 message[4] := "use this file contact Mr. Kenneth Murphy at RAPTORTECH@arinc.com.";
                 ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                    existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
                 RETURN;                   
              END IF;
        {      ASK saveFile TO ReadString(nextString);    tony 1-05: removed because we no longer write a devName}
              ASK saveFile TO ReadString(nextString);   {looking to see if student version file}    
              IF nextString="Student"
                 studentFile:=TRUE;
              END IF;
           ELSE
              validVersion:=FALSE;
              NEW(message, 1..4);
              message[1] := "                        MARS ATTACKS ALERT!!!     ";
              message[2] := "    ";
              message[3] := "This beta version file is not supported. If you would like to     ";
              message[4] := "use this file contact Mr. Kenneth Murphy at RAPTORTECH@arinc.com.";
              ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                   existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
              RETURN;
           END IF;   
        ELSIF (SUBSTR(1,8,version)="Version6")
           rapVersion:=6;
           IF SUBSTR(10,10,version) < "N"
              validVersion:=FALSE;
              NEW(message, 1..4);
              message[1] := "                        GORGON ALERT!!!     ";
              message[2] := "    ";
              message[3] := "This beta version file is not supported. If you would like to     ";
              message[4] := "use this file contact Mr. Kenneth Murphy at RAPTORTECH@arinc.com.";
              ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                   existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
              RETURN;
           END IF;
        ELSIF (SUBSTR(1,8,version)="Version5")
           rapVersion:=6;
        ELSIF (version="Version+") 
           rapVersion:=5;
        ELSE
           validVersion:=FALSE;
           NEW(message, 1..4);
           message[1] := "The file does not appear to properly formatted.  It may be from ";
           message[2] := "an incompatible version of RAPTOR that is not supported by      ";
           message[3] := versionStr+".  If you would like an old file converted to     ";
           message[4] := versionStr+" email the file to RAPTORTECH@arinc.com.";
           ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
             existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
           RETURN;
        END IF;
         IF ((append) AND (rapVersion<>7))
            validVersion:=FALSE;
            NEW(message, 1..3);
            message[1] := "                      IMPORT ALERT!!!     ";
            message[2] := "The Import RBD feature is only valid with a "+versionStr+" file.";
            message[3] := " Please save the file as a "+versionStr+" before importing.";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
            ASK window TO SetSysCursor(NormalCursor);
            RETURN;
         END IF;
         IF (fileName <> "Unnamed RBD");
            ASK menubar TO AddFileName(pathName, fileName);
            rbdPath:=pathName;            
         END IF;
         ASK saveFile TO ReadLine(nextString);         
         NEW(posArray,1..2);
         NEW(idsArray, 1..3);             
         NEW(boolsArray, 1..18);  
         NEW(intsArray,  1..17);
         NEW(realsArray, 1..28);
         NEW(stringArray,  1..6);
        {***}
        IF diagnostics 
           ASK diagnosticsStream TO Open(diagFile, Append);
           ASK diagnosticsStream TO WriteLn; 
           nextString:="Opened file "+pathName+fileName;
           ASK diagnosticsStream TO WriteString(nextString);   
           ASK diagnosticsStream TO WriteLn; 
           ASK diagnosticsStream TO Close; 
        END IF; 
        {***}
        IF append
           existingBlocks:=totalBlocks;
           existingEvents:=totalEvents;
           existingNodes:=totalNodes;
           existingHiers:=totalHiers;
           existingLinks:=totalLinks;
           keepPhasing:=FALSE;
           appendNum:=nextId;
           defaultToRead:=0;
        ELSE
           existingBlocks:=0;
           existingEvents:=0;
           existingNodes:=0;
           existingHiers:=0;
           existingLinks:=0;
           keepPhasing:=TRUE;
           appendNum:=0;
           nextId:=1;
           nextLinkId:=1;    {beta145}
           defaultToRead:=1;
        END IF;   
{begin read hierarchy rbd}         
        IF  rapVersion=7   
           NEW(hierFileGp);
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadInt(nextInt); {activePhases}
           IF ((append) AND (nextInt=activePhases))  {eag - phasing kept if # of phases is same in imported rbd as original}
              keepPhasing:=TRUE; 
           ELSIF (NOT append)
              activePhases:=nextInt;
           END IF;
           numPhases:=nextInt;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadInt(nextInt);
           totalBlocks:=existingBlocks+nextInt;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadInt(nextInt);
           totalEvents:=existingEvents+nextInt;      
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadInt(nextInt);
           totalNodes:=existingNodes+nextInt;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadInt(nextInt);
           totalLinks:=existingLinks+nextInt;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadInt(sparesToRead);
           IF (NOT append)
              numSpPools:=sparesToRead;
           ELSE
              numSpPools:=totalSpares;
           END IF;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadInt(resToRead);
           IF (NOT append)
              numResPools:=resToRead;
           END IF;
           ASK saveFile TO ReadString(nextString);
           ASK saveFile TO ReadInt(trigsToRead);
           IF (NOT append)
              totalTriggers:=trigsToRead;
           END IF;   
           ASK saveFile TO ReadString(nextString);
           ASK saveFile TO ReadInt(nextInt);
           totalHiers:=existingHiers+nextInt;
           ASK saveFile TO ReadString(nextString);
           ASK saveFile TO ReadInt(nextInt);
           IF (NOT append)
              deepestLevel:=nextInt;
              IF ((nextInt) > levelLimit)  
                 NEW(message, 1..3);
                 message[1] := "       EXCEEDS LEVEL LIMIT!!!     ";      
                 message[2] := versionStr+" cannot open this RBD because it     ";
                 message[3] := "exceeds the maximum possible levels.     ";
                 ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                    existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
                 CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
                 RETURN;
              END IF;
           ELSE
              appendingDepth:=nextInt;
           END IF;
           IF ((totalBlocks > demoCrippleLimit) AND (compileType = "demo"))
              NEW(message, 1..3);
              message[1] := "This demo version limits the number of blocks on screen ";
              IF (NOT append)
                 message[2] := "to "+INTTOSTR(demoCrippleLimit)+".  The file you are attempting to open contains";
                 message[3] := "more than "+INTTOSTR(demoCrippleLimit)+" blocks.     ";
              ELSE
                 message[2] := "to "+INTTOSTR(demoCrippleLimit)+".  The file you are attempting to import would increase";
                 message[3] := "the total to more than "+INTTOSTR(demoCrippleLimit)+" blocks.     ";
              END IF;
              ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
              CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
              RETURN;
           ELSIF ((compileType="student") AND (totalBlocks > studentCrippleLimit))  
              NEW(message, 1..3);
              NEW(message, 1..3);
              message[1] := "This student version limits the number of blocks on screen ";
              IF (NOT append)
                 message[2] := "to "+INTTOSTR(studentCrippleLimit)+".  The file you are attempting to open contains";
                 message[3] := "more than "+INTTOSTR(studentCrippleLimit)+" blocks.     ";
              ELSE
                 message[2] := "to "+INTTOSTR(studentCrippleLimit)+".  The file you are attempting to import would increase";
                 message[3] := "the total to more than "+INTTOSTR(studentCrippleLimit)+" blocks.     ";
              END IF;
              ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
              CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
              RETURN;
           ELSIF ((compileType="student") AND (NOT studentFile))  
              NEW(message, 1..3);
              message[1] := "This is the student version of Raptor.  It ";
              message[2] := "is incompatible with the full release version";
              message[3] := "of Raptor, so is unable to open this file";
              ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
              CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
              RETURN;
           ELSIF ((compileType="demo") AND (studentFile))  
              NEW(message, 1..3);
              message[1] := "This is the demo version of Raptor.  It ";
              message[2] := "is incompatible with the student version,";
              message[3] := "and is unable to open this file";
              ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
              CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
              RETURN;
           ELSIF ((compileType="gmd") AND (studentFile))  
              NEW(message, 1..3);
              message[1] := "This is Raptor version 6.5.  It ";
              message[2] := "is incompatible with the student version,";
              message[3] := "and is unable to open this file";
              ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
              CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
              RETURN;
           END IF; 
           IF (NOT append)
              activeWindow:=0;
              DISPOSE(phaseTimes);
              IF activePhases <> 0
                 NEW(phaseTimes, 1..activePhases);
                 NEW(phaseArray, 1..activePhases);
              ELSE
                 NEW(phaseTimes, 1..1);
                 NEW(phaseArray, 1..1);
              END IF;   
              ASK saveFile TO ReadString(nextString);{/}                  
              ASK saveFile TO ReadString(nextString); {System Data}                 
              ASK saveFile TO ReadString(nextString); {System Data}                 
              ASK saveFile TO ReadLine(nextString); {}   {Start of System section }                  
              ASK saveFile TO ReadString(systemImage);
              ASK saveFile TO ReadString(nextString);
              IF nextString = "TimeSim"
                 termType := 1;
                 ASK saveFile TO ReadReal(dTimeTrunc);
                 ASK saveFile TO ReadReal(dTimeStartTime);
                 dFailTrunc:=1.0;
                 dCycleTrunc:=10.0;
              ELSIF nextString = "FailSim"
                 termType := 2;
                 ASK saveFile TO ReadReal(dFailTrunc);
                 ASK saveFile TO ReadReal(dFailStartTime);
                 dTimeTrunc:=1000.0;
                 dCycleTrunc:=10.0;
              ELSIF nextString = "CycleSim"
                 termType := 3;
                 ASK saveFile TO ReadReal(dCycleTrunc);
                 ASK saveFile TO ReadReal(dCycleStartTime);
                 dTimeTrunc:=1000.0;
                 dFailTrunc:=1.0;
              END IF;
              ASK saveFile TO ReadReal(dNumberOfRuns);
              ASK saveFile TO ReadReal(dTimeSlice);
              ASK saveFile TO ReadReal(yMin);
              ASK saveFile TO ReadReal(yMax); 
              ASK saveFile TO ReadReal(cusZoomVal);  {zoom}
              ASK saveFile TO ReadString(systemUnits);
              ASK saveFile TO ReadString(nextString); 
              IF nextString = "GraphicsOn"
                 dSimWithGraph := TRUE;
              ELSE         
                 dSimWithGraph := FALSE;
              END IF;
              FOR i:=1 TO 11
                 ASK saveFile TO ReadInt(sysStreams[i]);
              END FOR;
              ASK saveFile TO ReadReal(xOrigin);   {x center}
              ASK saveFile TO ReadReal(yOrigin);   {y center} 
              ASK saveFile TO ReadInt(nextInt);   
              IF (nextInt=0)
                 negShutUp:=FALSE;
              ELSE
                 negShutUp:=TRUE;
              END IF;
              ASK saveFile TO ReadInt(nextInt);   
              IF (nextInt=0)
                 dZeroFail:=FALSE;
              ELSE
                 dZeroFail:=TRUE;
              END IF;
              ASK saveFile TO ReadInt(flowGenerated);                  
              ASK saveFile TO ReadReal(systemRedCost);   {system redCost}
              ASK saveFile TO ReadReal(sysLostCost);
              ASK saveFile TO ReadInt(nextInt);
              IF (nextInt=0)
                 weakAnalysis:=FALSE;
              ELSE
                 weakAnalysis:=TRUE;
              END IF;
              ASK saveFile TO ReadInt(nextInt);
              IF (nextInt=0)
                 costAnalysis:=FALSE;
              ELSE
                 costAnalysis:=TRUE;
              END IF;
              ASK saveFile TO ReadInt(nextInt);
              IF (nextInt=0)
                 capacityAnalysis:=FALSE;
              ELSE
                 capacityAnalysis:=TRUE;
              END IF;
              ASK saveFile TO ReadString(nextString); 
              IF nextString="Availability"
                 weakLinkAnalType:=1;
              ELSIF nextString="Dependability"
                 weakLinkAnalType:=2;
              ELSE
                 weakLinkAnalType:=3;
              END IF;
              ASK saveFile TO ReadReal(GYthreshold);
              ASK saveFile TO ReadReal(YRthreshold);
              ASK saveFile TO ReadInt(nextInt);       {not used}
              IF (nextInt=-1)
                 ProtectedFile := TRUE;                
                 NEW(message, 1..3);
                 message[1] := "The file is read only.  You may view it and ";
                 message[2] := "simulate it, but you will not be able to save";
                 message[3] := "any changes.";
              ELSIF ((nextInt=1) AND (compileType="demo"))
                 NEW(message, 1..3);
                 message[1] := "This is the demo version of Raptor.";
                 message[2] := "It is incompatible with the student version,";
                 message[3] := "and is unable to open this file";
                 ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                    existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
                 CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
                 RETURN;
              ELSE
                 ProtectedFile := FALSE;
              END IF;
              ASK saveFile TO ReadLine(nextString);
              ASK saveFile TO ReadLine(nextString);    {EndOfSystemData}
              IF nextString<>"EndOfSystemData"
                 {add any new variables here}
              END IF;
              loadingFile:=TRUE;
              SetView(cusZoomVal,xOrigin,yOrigin); 
              loadingFile:=FALSE;
           ELSE {append}
              FOR i:=1 TO 6
                 ASK saveFile TO ReadLine(nextString);
                 IF i=4
                    FOR j:=1 TO 8    {to get zoom value from appending rbd}
                       ASK saveFile TO ReadString(nextString);
                    END FOR;
                    ASK saveFile TO ReadReal(nextReal);
                    FOR j:=1 TO 26   {to check read only value}
                       ASK saveFile TO ReadString(nextString);   
                    END FOR;
                    ASK saveFile TO ReadInt(nextInt3);
                 END IF;
              END FOR; 
              IF activeWindow>0
                 hier := ASK root Child("RBDHier",activeWindow);
                 nextInt:=hier.level
              ELSE
                 nextInt:=0;
              END IF;
              IF ((nextInt+appendingDepth) >= levelLimit)  {>= is appropriate}
                 NEW(message, 1..3);
                 message[1] := "       EXCEEDS LEVEL LIMIT!!!     ";   
                 message[2] := "Importing this rbd as a hierarchy would     ";
                 message[3] := "exceed the maximum possible levels.     ";
                 ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                   existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
                 CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
                 RETURN;
              END IF;
              
              IF ((nextInt + appendingDepth + 1) > deepestLevel)
                 deepestLevel := nextInt + appendingDepth + 1;
              END IF; {eag}
              
              IF (nextInt3 = -1)
                 NEW(message, 1..2);
                 message[1] := "       This rbd is read only.     ";   
                 message[2] := "It cannot be imported as a hierarchy     ";
                 ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                   existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
                 CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
                 RETURN;
              END IF;
              somethingChanged:=TRUE;
              importing:=TRUE;
              AddHier;
              INC(existingHiers);
              hier := ASK window Child("RBDHier",nextId-1);
              importHierId:=nextId-1;
              ASK hier TO SetName(SUBSTR(1,dotPosition-1,fileName));
              ASK hier TO SetZoom(nextReal);
           END IF; {NOT append}
           ASK saveFile TO ReadLine(nextString);  {__HIERARCHY_DATA}                                                        
           ASK saveFile TO ReadLine(nextString);  {column header}
           FOR i := (existingHiers+1) TO totalHiers   {read in hierarchy table in .rbd file} 
              NEW(hier);
              ASK hierGroup TO Add(hier);
              ASK hierFileGp TO Add(hier);
              ASK hier TO LoadFromLibrary (images, "RBDHier");
              ASK saveFile TO ReadString(nextString);
              ASK saveFile TO ReadInt(nextInt);     {Id}
              ASK saveFile TO ReadReal(nextReal1);  {xPosition}               
              ASK saveFile TO ReadReal(nextReal2);  {yPosition}
              ASK hier TO SetID("RBDHier", (appendNum+nextInt));    
              ASK root TO AddAfterGraphic((ASK root Child("RBDBlock",0)),hier);            
              ASK saveFile TO ReadInt(nextInt);  {parentID}
              ASK hier TO SetParentID(nextInt+appendNum);
              IF hier.parentID=activeWindow          
                 ASK hier TO DisplayAt(nextReal1/1.25, nextReal2/1.25); {x & y coordinates}
                 ASK hier TO Draw;     {only if hier on top level}
              ELSE
                 ASK hier TO SetHidden(TRUE);
                 ASK hier TO DisplayAt(nextReal1/1.25, nextReal2/1.25); {x & y coordinates}
              END IF;             
              ASK hier TO SetName(nextString);
              ASK saveFile TO ReadInt(nextInt1);  {connectIntoNum} 
              ASK saveFile TO ReadInt(nextInt2);  {ConnectOutOfNum}  
              FOR j := 1 TO nextInt1
                 ASK hier TO IncLink(INTO);
              END FOR;
              FOR j := 1 TO nextInt2
                 ASK hier TO IncLink(OUTOF);
              END FOR;
              ASK saveFile TO ReadInt(nextInt);
              IF nextInt=1
                 ASK hier TO SetConnectToNode(TRUE);
              ELSE
                 ASK hier TO SetConnectToNode(FALSE);
              END IF;   
              ASK saveFile TO ReadReal(nextReal);
              ASK hier TO SetZoom(nextReal);
              ASK saveFile TO ReadReal(nextReal);
              ASK hier TO SetxCenter(nextReal);
              ASK saveFile TO ReadReal(nextReal);
              ASK hier TO SetyCenter(nextReal);
              ASK saveFile TO ReadInt(nextInt);
              ASK hier TO SetInID(nextInt+appendNum);
              ASK saveFile TO ReadInt(nextInt);
              ASK hier TO SetOutID(nextInt+appendNum);
              ASK saveFile TO ReadInt(nextInt);
              ASK hier TO SetLevel(nextInt+appendingLevel);   
              ASK saveFile TO ReadInt(nextInt);
              IF (nextInt=1) AND (keepPhasing) {eag}
                 ASK hier TO SetusesPhasing(TRUE);
              ELSE
                 ASK hier TO SetusesPhasing(FALSE);
              END IF;   
              ASK hier TO CreateChildGroup;
              ASK saveFile TO ReadReal(nextReal1);  {xOrigin}               
              ASK saveFile TO ReadReal(nextReal2);  {yOrigin}
              nextReal:= STRTOREAL(SUBSTR(12,15,version));
              IF nextReal = 4.2    {Otto, hier x-yOrigins not set}
                 nextReal1:=0.0; 
                 nextReal2:=80.0;
              END IF;
              ASK hier TO SetOrigin(nextReal1,nextReal2);
              ASK saveFile TO ReadInt(nextInt);     {myDepth}
              ASK hier TO SetmyDepth(nextInt);
              ASK saveFile TO ReadInt(nextInt);     {emptyInt}
              ASK saveFile TO ReadInt(nextInt);     {emptyInt}
              ASK saveFile TO ReadReal(nextReal);   {emptyReal}
              ASK saveFile TO ReadReal(nextReal);   {emptyReal}
           END FOR;
           FOREACH hier IN hierFileGp
              IF hier.parentID <> activeWindow
                 papaHier := ASK root Child("RBDHier", hier.parentID);
                 IF papaHier=NILOBJ
                    papaHier := ASK window Child("RBDHier",hier.parentID);
                 END IF;
                 ASK papaHier.childGroup TO Add(hier);
              END IF;
           END FOREACH;
           ASK saveFile TO ReadString(nextString);
           ASK saveFile TO ReadLine(nextString); {EndofHierarchyData}                 
           ASK saveFile TO ReadLine(nextString); {__BLOCK_DATA}
           ASK saveFile TO ReadLine(nextString);  {column header}
           FOR i := (existingBlocks+1) TO (totalBlocks+defaultToRead)   {read in first table in .rbd file}  
              ASK saveFile TO ReadString(nextString);  {block.blockName}
              ASK saveFile TO ReadInt(nextInt1);  {block.Id} 
              IF nextInt1=0
                 block := ASK root Child("RBDBlock",0);
              ELSE   
                 NEW(block);
              END IF;   
              ASK saveFile TO ReadReal(nextReal1);  {xPosition}               
              ASK saveFile TO ReadReal(nextReal2);  {yPosition}
              ASK saveFile TO ReadInt(nextInt2);  {parentID}
              ASK saveFile TO ReadInt(nextInt3);  {block.connectIntoNum} 
              ASK saveFile TO ReadInt(nextInt4);  {block.ConnectOutOfNum}  
              IF nextInt1<>0
                 ASK block TO LoadFromLibrary (images, "RBDBlock");   {tony10-04-roys goofy rbd}
              END IF;   
              ASK block TO SetID("RBDBlock", (appendNum+nextInt1));   {block number}  
              IF nextInt1<>0
                 ASK blockGroup TO Add(block);
                 ASK root TO AddAfterGraphic((ASK root Child("RBDBlock",0)),block);            
                 ASK block TO SetParentID(appendNum+nextInt2);    
                 IF (block.parentID=activeWindow)
                    ASK block TO DisplayAt(nextReal1/1.25, nextReal2/1.25); {x & y coordinates}
                    ASK block TO Draw;    {only display if block on top level}
                 ELSE
                    ASK block TO SetHidden(TRUE);
                    papaHier := ASK root Child("RBDHier", block.parentID);
                    IF papaHier=NILOBJ
                       papaHier := ASK window Child("RBDHier",block.parentID);
                    END IF;
                    ASK papaHier.childGroup TO Add(block);
                    ASK block TO DisplayAt(nextReal1/1.25, nextReal2/1.25); {x & y coordinates}
                 END IF;             
                 FOR j := 1 TO nextInt3
                    ASK block TO IncLink(INTO);
                 END FOR;
                 FOR j := 1 TO nextInt4
                    ASK block TO IncLink(OUTOF);
                 END FOR;
              END IF;   
              IF nextInt1<>0
                 ASK block TO SetDefaultBlockName(nextString);   {tony10-04-roys goofy rbd}
              END IF;   
              ASK block TO SetInitName(nextString);
              ASK saveFile TO ReadInt(nextInt);  {usesPhasing} 
              IF (nextInt = 1)
                 ASK block TO SetusesPhasing(TRUE);
              ELSE
                 ASK block TO SetusesPhasing(FALSE);
              END IF;      
              ASK saveFile TO ReadInt(nextInt);  {simStartType} 
              ASK block TO SetsimStartType(nextInt);
              ASK saveFile TO ReadReal(nextReal);  {amountExhausted}
              ASK block TO SetamountExhausted(nextReal);
              ASK saveFile TO ReadInt(nextInt1);  {failDistro} 
              ASK block TO SetfailDistro(nextInt1);               
              ASK saveFile TO ReadInt(nextInt2);  {numFailParams} 
              ASK block TO SetnumFailParams(nextInt2);
              ASK saveFile TO ReadInt(nextInt);  {failStream}  
              ASK block TO SetfailStream(nextInt);
              IF nextInt1=16
                 failEmp:=TRUE;
              ELSE
                 NEW(failVars, 1..nextInt2);
              END IF;
              FOR j := 1 TO 3
                 ASK saveFile TO ReadReal(nextReal);  {failure distribution parameters}
                 IF ((j<=nextInt2) AND (NOT failEmp))
                    failVars[j] := nextReal;
                 END IF;   
              END FOR;
              IF (NOT failEmp)
                 ASK block TO SetfailVals(failVars);
              END IF;   
              DISPOSE(failVars);       
              ASK saveFile TO ReadInt(nextInt1);  {repairDistro}   
              ASK block TO SetrepairDistro(nextInt1);               
              ASK saveFile TO ReadInt(nextInt2);  {numRepairParams}  
              ASK block TO SetnumRepairParams(nextInt2);
              ASK saveFile TO ReadInt(nextInt);  {repairStream}                
              ASK block TO SetrepairStream(nextInt); 
              IF nextInt1=16
                 repEmp:=TRUE;
              ELSE
                 NEW(repVars, 1..nextInt2);
              END IF;
              FOR j := 1 TO 3
                 ASK saveFile TO ReadReal(nextReal); {repair distribuiton parameters}
                 IF ((j <= nextInt2) AND (NOT repEmp))
                    repVars[j] := nextReal;
                 END IF;   
              END FOR;
              IF (NOT repEmp);
                 ASK block TO SetrepairVals(repVars);
              END IF;   
              DISPOSE(repVars);         
              ASK saveFile TO ReadReal(nextReal);  {SBstress} 
              ASK block TO SetSBstress(nextReal);
              ASK saveFile TO ReadInt(nextInt);  {GDType}  {1=linear,2=geometric, 3=asymptotic}
              ASK block TO SetGDType(nextInt);
              ASK saveFile TO ReadReal(nextReal);  {GDRate}  
              ASK block TO SetGDRate(nextReal);
              ASK saveFile TO ReadReal(nextReal);  {GDLimit} 
              ASK block TO SetGDLimit(nextReal);              
              ASK saveFile TO ReadString(nextString); {poolName}                 
              ASK block TO SetpoolName(nextString);
              ASK saveFile TO ReadString(nextString); {sparingType}  
              IF nextString = "Infinite"
                 sparing := Infinite;
                 ASK block TO SetinfiniteSpares(TRUE);
               ELSIF nextString = "Pooled"
                 sparing := SparePool;
              ELSIF nextString = "Custom"
                 sparing := Custom;
              ELSE
                 sparing := None;
              END IF;
              ASK block TO SetsparingType(sparing);
              ASK saveFile TO ReadInt(nextInt);  {RSO}  
              IF nextInt = 1
                 ASK block TO SetroutineSpareOrdering(TRUE);
              ELSE
                 ASK block TO SetroutineSpareOrdering(FALSE);
              END IF;
              ASK saveFile TO ReadInt(nextInt);  {initStock} 
              ASK block TO SetinitStock(nextInt);               
              ASK saveFile TO ReadInt(nextInt);  {newSpares}                
              ASK block TO SetnewSpares(nextInt);
              ASK saveFile TO ReadReal(nextReal);{arriveEvery}               
              ASK block TO SetarrivalRate(nextReal);
              ASK saveFile TO ReadInt(nextInt);  {ESO} 
              IF nextInt = 1
                 ASK block TO SetemerSpareOrdering(TRUE);
              ELSE
                 ASK block TO SetemerSpareOrdering(FALSE);
              END IF;
              ASK saveFile TO ReadReal(nextReal);  {emerTime} 
              ASK block TO SetemerTime(nextReal);                
              ASK saveFile TO ReadInt(nextInt);  {SLO} 
              IF nextInt = 1
                 ASK block TO SetstockLevelOrdering(TRUE);
              ELSE
                 ASK block TO SetstockLevelOrdering(FALSE);
              END IF;               
              ASK saveFile TO ReadInt(nextInt);  {level} 
              ASK block TO SetSLOOrderLevel(nextInt);               
              ASK saveFile TO ReadInt(nextInt);  {quantity}                
              ASK block TO SetSLONewSpares(nextInt);
              ASK saveFile TO ReadReal(nextReal);  {SLOTime}
              ASK block TO SetSLOTime(nextReal);                
              ASK saveFile TO ReadInt(nextInt);  {preDist} 
              IF nextInt=0                    
                  nextInt:=19;
              END IF; 
              ASK block TO SetpreDist(nextInt); 
              GetNumParams(block.preDist,nextInt);  {numPreLdtParams}
              NEW(failVars, 1..nextInt);
              FOR j := 1 TO 3
                 ASK saveFile TO ReadReal(nextReal);  {preLDT distribuiton parameters}
                 IF j<=nextInt
                    failVars[j] := nextReal;
                 END IF;   
              END FOR;
              ASK block TO SetpreParams(failVars);
              DISPOSE(failVars);
              ASK saveFile TO ReadInt(nextInt);  {postDist} 
              IF nextInt=0                    
                  nextInt:=19;
              END IF; 
              ASK block TO SetpostDist(nextInt); 
              GetNumParams(block.postDist,nextInt);  {numPosrLDTParams}
              NEW(failVars, 1..nextInt);
              FOR j := 1 TO 3
                 ASK saveFile TO ReadReal(nextReal);  {postLDT distribuiton parameters}
                 IF j<=nextInt
                    failVars[j] := nextReal;
                 END IF;   
              END FOR;
              ASK block TO SetpostParams(failVars);
              DISPOSE(failVars);
              ASK saveFile TO ReadInt(nextInt);  {numDiffRes}                
              ASK saveFile TO ReadInt(nextInt1);  {numRes1}                
              ASK saveFile TO ReadInt(nextInt2);  {numRes1PM}                
              ASK saveFile TO ReadString(nextString); {res1Name}                 
              ASK block TO SetnumDiffRes(nextInt);
              ASK block TO SetnumRes1(nextInt1);
              ASK block TO SetnumRes1PM(nextInt2);
              ASK block TO Setres1Name(nextString);
              ASK saveFile TO ReadInt(nextInt);  {UsesPM} 
              IF nextInt = 1
                 ASK block TO SetUsesPM(TRUE);
              ELSE
                 ASK block TO SetUsesPM(FALSE);
              END IF;               
              ASK saveFile TO ReadInt(nextInt);  {pmSpare} 
              IF nextInt = 1
                 ASK block TO SetpmSpare(TRUE);
              ELSE
                 ASK block TO SetpmSpare(FALSE);
              END IF;               
              ASK saveFile TO ReadInt(nextInt);  {pmRefresh} 
              IF nextInt = 1
                 ASK block TO SetpmRefresh(TRUE);
              ELSE
                 ASK block TO SetpmRefresh(FALSE);
              END IF;               
              ASK saveFile TO ReadInt(nextInt);  {pmMisDefer} 
              IF nextInt = 1
                 ASK block TO SetpmMisDefer(TRUE);
              ELSE
                 ASK block TO SetpmMisDefer(FALSE);
              END IF;               
              ASK saveFile TO ReadInt(nextInt);  {pmFailReset} 
              IF nextInt = 1
                 ASK block TO SetpmFailReset(TRUE);
              ELSE
                 ASK block TO SetpmFailReset(FALSE);
              END IF;               
              ASK saveFile TO ReadInt(nextInt);  {pmReqDefer} 
              IF nextInt = 1
                 ASK block TO SetpmReqDefer(TRUE);
              ELSE
                 ASK block TO SetpmReqDefer(FALSE);
              END IF;               
              ASK saveFile TO ReadReal(nextReal);  {pmStagger}  
              ASK block TO SetpmStagger(nextReal);
              ASK saveFile TO ReadReal(nextReal);  {pmFreq}  
              ASK block TO SetpmFreq(nextReal);
              ASK saveFile TO ReadInt(nextInt);  {pmTriggered} 
              IF nextInt = 1
                 ASK block TO SetpmTriggered(TRUE);
              ELSE
                 ASK block TO SetpmTriggered(FALSE);
              END IF; 
              ASK saveFile TO ReadString(nextString);
              ASK block TO SetpmTrig(nextString);
              ASK saveFile TO ReadInt(nextInt);  {postDist} 
              IF nextInt=0                    
                  nextInt:=19;
              END IF; 
              ASK block TO SetpmDist(nextInt); 
              GetNumParams(block.pmDist,nextInt);  {numPMParams}
              NEW(failVars, 1..nextInt);
              FOR j := 1 TO 3
                 ASK saveFile TO ReadReal(nextReal);  {pm distribution parameters}
                 IF j<=nextInt
                    failVars[j] := nextReal;
                 END IF;   
              END FOR;
              ASK block TO SetpmParams(failVars);
              DISPOSE(failVars);
              ASK saveFile TO ReadInt(nextInt);     {DependencyNum}
              IF ((nextInt>0) AND (append))
                 nextInt:=nextInt+appendNum;
              END IF;
              ASK saveFile TO ReadReal(nextReal);   {DepNothingPerc}               
              ASK saveFile TO ReadReal(nextReal1);  {DepIdlePerc}               
              ASK saveFile TO ReadReal(nextReal2);  {DepPMPerc}               
              ASK saveFile TO ReadReal(nextReal3);  {DepFailPerc}               
              ASK saveFile TO ReadReal(nextReal4);  {DepPMThreshold} 
              ASK saveFile TO ReadInt(nextInt1);
              IF nextInt1 = 1
                 newBool:=TRUE;
              ELSE
                 newBool:=FALSE;
              END IF;  
              ASK saveFile TO ReadString(nextString1);  {DepType}
              IF NOT(nextInt>0)
                 nextString1:="";
              END IF;        
              ASK block TO SetDep(nextInt,nextString1);       
              ASK block TO SetDepVals(nextReal,nextReal1,nextReal2,nextReal3,nextReal4,newBool);
              ASK saveFile TO ReadReal(nextReal);  {initialCost}               
              ASK block TO SetinitialCost(nextReal);                
              ASK saveFile TO ReadReal(nextReal);  {Running operating Cost}               
              ASK block TO SetOperatingCost(nextReal);                
              ASK saveFile TO ReadReal(nextReal);  {standbyCost}               
              ASK block TO SetstandbyCost(nextReal);                
              ASK saveFile TO ReadReal(nextReal);  {idleCost}               
              ASK block TO SetidleCost(nextReal);                
              ASK saveFile TO ReadReal(nextReal);  {repHoldCost}               
              ASK block TO SetrepHoldCost(nextReal);                
              ASK saveFile TO ReadReal(nextReal);  {repPerTime}               
              ASK block TO SetrepairingCost(nextReal);                
              ASK saveFile TO ReadReal(nextReal);  {repFixed}               
              ASK block TO SetrepFixedCost(nextReal);                             
              ASK saveFile TO ReadReal(nextReal);  {donePerTime}               
              ASK block TO SetdoneCost(nextReal);                               
              ASK saveFile TO ReadReal(nextReal);  {doneFixed}               
              ASK block TO SetdoneFixedCost(nextReal);                
              ASK saveFile TO ReadReal(nextReal);  {pmHoldCost}               
              ASK block TO SetpmHoldCost(nextReal);   
              ASK saveFile TO ReadReal(nextReal);  {pmCost}
              ASK block TO SetpmCost(nextReal);
              ASK saveFile TO ReadReal(nextReal);  {pmFixedCost}               
              ASK block TO SetpmFixedCost(nextReal);   
              ASK saveFile TO ReadReal(nextReal);  {spareCost}               
              ASK block TO SetspareCost(nextReal);                
              ASK saveFile TO ReadReal(nextReal);  {emerShippingCost} 
              ASK block TO SetemerShippingCost(nextReal);                              
              ASK saveFile TO ReadInt(nextInt);    {alwaysAddDoneCost} 
              IF nextInt = 1
                 ASK block TO SetalwaysAddDoneCost(TRUE);
              ELSE
                 ASK block TO SetalwaysAddDoneCost(FALSE);
              END IF;               
              IF ((numPhases>0) AND (block.usesPhasing))
                 NEW(params,1..numPhases);
                 NEW(typeArray,1..numPhases);
                 FOR j := 1 TO numPhases
                    ASK saveFile TO ReadString(nextString);  {phase values}
                    typeArray[j]:=SUBSTR(1,1,nextString);    
                    ASK saveFile TO ReadReal(nextReal);       
                    params[j]:=nextReal;
                 END FOR;
                 IF keepPhasing
                    ASK block TO SetPhases(TRUE, params,typeArray);
                 ELSE
                    ASK block TO SetusesPhasing(FALSE);
                 END IF;
                 DISPOSE(params);
                 DISPOSE(typeArray);
              END IF;    {(numPhases>0) AND (block.usesPhasing)}
              IF failEmp
                 ASK saveFile TO ReadLine(nextString);
                 NEW(failVars,1..block.numFailParams);
                 FOR j:=1 TO block.numFailParams
                    ASK saveFile TO ReadReal(nextReal);
                    failVars[j]:=nextReal;
                 END FOR;
                 ASK block TO SetfailVals(failVars);
                 DISPOSE(failVars);
                 failEmp:=FALSE;
              END IF;
              IF repEmp
                 ASK saveFile TO ReadLine(nextString);
                 NEW(repVars,1..block.numRepairParams);
                 FOR j:=1 TO block.numRepairParams
                    ASK saveFile TO ReadReal(nextReal);
                    repVars[j]:=nextReal;
                 END FOR;
                 ASK block TO SetrepairVals(repVars);
                 DISPOSE(repVars);
                 repEmp:=FALSE;
              END IF;
              ASK block TO SetStats();
           END FOR;   
           IF append
              ASK saveFile TO ReadString(nextString);
              ASK saveFile TO ReadLine(nextString);  {default block}
           END IF; 
           ASK saveFile TO ReadString(nextString);    
           ASK saveFile TO ReadLine(nextString);    {EndOfBlockData}
           IF nextString<>"EndOfBlockData"
              {add any new variables here}
           END IF;   
           ASK saveFile TO ReadLine(nextString);   {EVENT_DATA________}
           ASK saveFile TO ReadLine(nextString); 
           FOR i:= (existingEvents+1) TO totalEvents
              ASK saveFile TO ReadString(nextString);  {eventName}
              ASK saveFile TO ReadInt(nextInt);  {event.Id} 
              ASK saveFile TO ReadReal(nextReal1);  {xPosition}               
              ASK saveFile TO ReadReal(nextReal2);  {yPosition}
              NEW(event);
              ASK eventGroup TO Add(event);
              ASK event TO LoadFromLibrary (images, "RBDEvent");
              ASK event TO SetID("RBDEvent", (appendNum+nextInt));   {id number}   
              ASK saveFile TO ReadInt(nextInt);  {parentID}
              ASK event TO SetParentID(appendNum+nextInt);     
              ASK root TO AddAfterGraphic((ASK root Child("RBDBlock",0)),event);            
              IF (event.parentID=activeWindow)
                 ASK event TO DisplayAt(nextReal1/1.25, nextReal2/1.25); {x & y coordinates}
                 ASK event TO Draw;    {only display if event on top level}
              ELSE
                 ASK event TO SetHidden(TRUE);
                 papaHier := ASK root Child("RBDHier", event.parentID);
                 IF papaHier=NILOBJ
                    papaHier := ASK window Child("RBDHier",event.parentID);
                 END IF;
                 ASK papaHier.childGroup TO Add(event);
                 ASK event TO DisplayAt(nextReal1/1.25, nextReal2/1.25); {x & y coordinates}
                 ASK event TO Draw;
              END IF;  
              ASK event TO SetInitName(nextString);
              ASK saveFile TO ReadInt(nextInt1);  {connectIntoNum} 
              ASK saveFile TO ReadInt(nextInt2);  {ConnectOutOfNum}  
              FOR j := 1 TO nextInt1
                 ASK event TO IncLink(INTO);
              END FOR;
              FOR j := 1 TO nextInt2
                 ASK event TO IncLink(OUTOF);
              END FOR;
              ASK saveFile TO ReadInt(nextInt);  {usesPhasing} 
              IF (nextInt = 1)
                 ASK event TO SetusesPhasing(TRUE);
              ELSE
                 ASK event TO SetusesPhasing(FALSE);
              END IF;      
              ASK saveFile TO ReadInt(nextInt);    {dist}
              ASK saveFile TO ReadInt(nextInt1);   {stream}
              ASK saveFile TO ReadReal(nextReal);  {failVals}
              ASK saveFile TO ReadReal(nextReal1); {initCost}
              ASK saveFile TO ReadReal(nextReal2); {OpCost}
              ASK saveFile TO ReadReal(nextReal3); {RepCost}
              ASK event TO SetfailDistro(nextInt);
              ASK event TO SetfailStream(nextInt1)
              ASK event TO SetfailVals(nextReal);
              ASK event TO SetinitialCost(nextReal1);
              ASK event TO SetOperatingCost(nextReal2);
              ASK event TO SetrepairingCost(nextReal3);
              IF ((numPhases>0) AND (event.usesPhasing))
                 NEW(params,1..numPhases);
                 NEW(typeArray,1..numPhases);
                 FOR j := 1 TO numPhases
                    ASK saveFile TO ReadString(nextString);  {phase values}
                    typeArray[j]:=SUBSTR(1,1,nextString);    
                    ASK saveFile TO ReadReal(nextReal);       
                    params[j]:=nextReal;
                 END FOR;
                 IF keepPhasing
                    ASK event TO SetPhases(TRUE, params,typeArray);
                 ELSE
                    ASK event TO SetusesPhasing(FALSE);
                 END IF;    {(keepPhasing)}
                 DISPOSE(params);
                 DISPOSE(typeArray);
              END IF;           
           END FOR;
           ASK saveFile TO ReadString(nextString);
           ASK saveFile TO ReadLine(nextString);    {EndOfEventData}
           ASK saveFile TO ReadLine(nextString);    {___NODE_DATA____ }
           ASK saveFile TO ReadLine(nextString);    {Name ID xPos yPos    .....}
           FOR i := (existingNodes+1) TO totalNodes         {Start of node section }
              NEW(node);
              ASK nodeGroup TO Add(node);
              ASK saveFile TO ReadString(nextString);   {shownName} 
              ASK saveFile TO ReadInt(nextInt);   {id}
              FOR j := 1 TO 2                     {translation}
                 ASK saveFile TO ReadReal(nextReal);
                 realsArray[j] := nextReal;
              END FOR;
              ASK saveFile TO ReadInt(nextInt1);  {parentID}
              ASK node TO SetParentID(appendNum+nextInt1);
              ASK saveFile TO ReadInt(nextInt1);  {typeNode}
              IF ((append) AND (nextInt1=1))
                 nextInt1:=4;
                 nextString:="in";
                 hier:= ASK window Child("RBDHier",node.parentID);
                 ASK hier TO SetInID(nextInt+appendNum);  
              ELSIF ((append) AND (nextInt1=3))
                 nextInt1:=5;
                 nextString:="out";
                 hier:= ASK window Child("RBDHier",node.parentID);
                 ASK hier TO SetOutID(nextInt+appendNum);  
              END IF;
              ASK node TO SetName(nextString);
              CASE nextInt1
                 WHEN 1:
                    ASK node TO LoadFromLibrary(images,"RBDStartNode");
                 WHEN 2:
                    ASK node TO LoadFromLibrary (images, "RBDNode");
                 WHEN 3:
                    ASK node TO LoadFromLibrary(images,"RBDEndNode");
                 WHEN 4:
                    ASK node TO LoadFromLibrary (images, "RBDInNode");
                 WHEN 5:
                    ASK node TO LoadFromLibrary (images, "RBDOutNode");
              END CASE;
              ASK node TO SetID("RBDNode", (appendNum+nextInt));   
              ASK root TO AddAfterGraphic((ASK root Child("RBDBlock",0)),node);
              ASK node TO SetSnapShot(FALSE);
              ASK node TO SetTranslation(node.Translation.x, node.Translation.y);
              IF node.parentID=activeWindow        {only display upon opening if NODE on top level}
                 ASK node TO DisplayAt(realsArray[1]/1.25, realsArray[2]/1.25);
                 ASK node TO Draw;
              ELSE
                 ASK node TO SetHidden(TRUE);
                 papaHier := ASK root Child("RBDHier", node.parentID);
                 IF papaHier=NILOBJ
                    papaHier := ASK window Child("RBDHier",node.parentID);
                 END IF;
                 ASK papaHier.childGroup TO Add(node);
                 ASK node TO DisplayAt(realsArray[1]/1.25, realsArray[2]/1.25);
              END IF;             
              ASK node TO SetType(nextInt1);
              ASK saveFile TO ReadInt(nextInt2);
              IF nextInt2=-1
                 ASK node TO SetGoodPaths(1);   {this means KofN not specified before saving}
              ELSE   
                 ASK node TO SetGoodPaths(nextInt2);
              END IF;   
              ASK saveFile TO ReadInt(nextInt);
              FOR j := 1 TO nextInt
                 ASK node TO IncLink(INTO);
              END FOR;
              ASK saveFile TO ReadInt(nextInt);
              FOR j := 1 TO nextInt
                 ASK node TO IncLink(OUTOF);
              END FOR;                
              IF node.typeNode = 1
                 ASK node TO SetNum(-1);
                 startId := node.Id;
              ELSIF node.typeNode = 3
                 ASK node TO SetNum(-2);
                 endId := node.Id;
              ELSE
                 ASK node TO SetNum(node.Id);
              END IF;                           
              IF ((node.connectIntoNum > 1) AND (nextInt2>0))    {do not set KofN if not specified before}
                 ASK node TO SetKofN(node.goodPaths, node.connectIntoNum);
              END IF;
              ASK saveFile TO ReadInt(intsArray[1]);  {coldStandby}                
              ASK saveFile TO ReadInt(intsArray[2]);  {priorityReturn}                
              ASK saveFile TO ReadInt(intsArray[3]);  {checkAutosFirst}                
              ASK saveFile TO ReadInt(intsArray[4]);  {KStar}
              ASK saveFile TO ReadInt(intsArray[5]);  {usesPhasing}
              ASK node TO Init1(intsArray);
              ASK saveFile TO ReadInt(nextInt);  {DependencyNum} 
              ASK saveFile TO ReadString(nextString);  {depType}
              IF nextInt>0
                 nextInt:=nextInt+appendNum;
              ELSE
                 nextString := "";
              END IF;                       
              ASK node TO SetDep(nextInt,nextString);
              ASK saveFile TO ReadInt(nextInt);  {anyPath}
              IF nextInt=0
                 ASK node TO SetAnyPath(FALSE);
              ELSE
                 ASK node TO SetAnyPath(TRUE);
              END IF;
              ASK saveFile TO ReadInt(nextInt);  {fullFlow}
              IF nextInt=0
                 ASK node TO SetFullFlow(FALSE);
              ELSE
                 ASK node TO SetFullFlow(TRUE);
              END IF;
              ASK saveFile TO ReadInt(nextInt);  {reportCapacity, currently unused}
              ASK saveFile TO ReadInt(nextInt);  {reportNodeAnal}
              IF ((nextInt=0) OR (node.typeNode=4) OR (node.typeNode=5))
                 ASK node TO SetReportNodeAnal(FALSE);
              ELSE
                 ASK node TO SetReportNodeAnal(TRUE);
              END IF;
              IF ((numPhases>0) AND (node.usesPhasing))
                 IF (append) 
                    NEW(phaseArray,1..numPhases);
                 END IF;   
                 FOR j := 1 TO numPhases
                    ASK saveFile TO ReadString(nextString);  {phase values}
                    IF nextString="C"
                       phaseArray[j]:=-1;
                    ELSIF nextString="L"
                       phaseArray[j]:=0;
                    ELSE
                       phaseArray[j]:=STRTOINT(nextString);
                    END IF;   
                 END FOR;
                 IF keepPhasing
                    ASK node TO SetPhases(TRUE, phaseArray);
                 ELSE
                    ASK node TO SetusesPhasing(FALSE);
                 END IF;
              END IF;
           END FOR;
           ASK saveFile TO ReadLine(nextString);    {EndOfNodeData}
           IF nextString<>"EndOfNodeData"
              {add any new variables here}
           END IF;
           DISPOSE(phaseArray);
           ASK saveFile TO ReadLine(nextString);
           ASK saveFile TO ReadLine(nextString);  {______COMMENTS}
           ASK saveFile TO ReadLine(nextString);  {Type Id ....}
           ASK saveFile TO ReadString(nextString);
           WHILE nextString<>"/"   {read in comments in .rbd file}  
              IF nextString="hier"
                 ASK saveFile TO ReadInt(nextInt);
                 hier := ASK root Child("RBDHier", nextInt+appendNum);
                 ASK saveFile TO ReadString(nextString);  {name}
                 ASK saveFile TO ReadString(nextString1);
                 ASK saveFile TO ReadLine(nextString);  {comment}
                 nextString:=ReadWriteCR("Read",nextString1+nextString);
                 ASK hier TO SetComment(nextString);
              ELSIF nextString="block"
                 ASK saveFile TO ReadInt(nextInt);
                 block := ASK root Child("RBDBlock", nextInt+appendNum);
                 ASK saveFile TO ReadString(nextString);  {name}
                 ASK saveFile TO ReadString(nextString1);
                 ASK saveFile TO ReadLine(nextString);  {comment}
                 nextString:=ReadWriteCR("Read",nextString1+nextString);
                 ASK block TO SetComment(nextString);
              ELSIF nextString="event"
                 ASK saveFile TO ReadInt(nextInt);
                 event := ASK root Child("RBDEvent", nextInt+appendNum);
                 ASK saveFile TO ReadString(nextString);  {name}
                 ASK saveFile TO ReadString(nextString1);
                 ASK saveFile TO ReadLine(nextString);  {comment}
                 nextString:=ReadWriteCR("Read",nextString1+nextString);
                 ASK event TO SetComment(nextString);
              ELSIF nextString="node"   
                 ASK saveFile TO ReadInt(nextInt);
                 node := ASK root Child("RBDNode", nextInt+appendNum);
                 ASK saveFile TO ReadString(nextString);  {name}
                 ASK saveFile TO ReadString(nextString1);
                 ASK saveFile TO ReadLine(nextString);  {comment}
                 nextString:=ReadWriteCR("Read",nextString1+nextString);
                 ASK node TO SetComment(nextString);
              ELSIF nextString="System"
                 ASK saveFile TO ReadInt(nextInt);
                 ASK saveFile TO ReadString(nextString);  {RBDname}
                 ASK saveFile TO ReadString(nextString1);
                 ASK saveFile TO ReadLine(nextString);  {comment}
                 nextString:=ReadWriteCR("Read",nextString1+nextString);
                 IF (NOT append)                         {to fix error 999}
                    sysComment:=nextString;
                 ELSE
                    hier := ASK window Child("RBDHier",importHierId);
                    ASK hier TO SetComment(nextString);                    
                 END IF;
                 IF (STRLEN(sysComment)>=21)
                    IF SUBSTR(1,21,sysComment)="The defer trigger is "
                       REPLACE(sysComment,1,21,"");
                       deferTrig:=sysComment;
                    END IF;
                 END IF;   
             {   ELSE }  {then the comment contained carriage returns}
   
              END IF;   
              ASK saveFile TO ReadString(nextString);  {objectType or /}
           END WHILE;  
           ASK saveFile TO ReadLine(nextString);                                                        
           ASK saveFile TO ReadLine(nextString);   {LINK_DATA_____}                                                       
           ASK saveFile TO ReadLine(nextString);   {link header}
           FOR i := (existingLinks+1) TO totalLinks
              NEW (link);
              ASK linkGroup TO Add(link);
              ASK saveFile TO ReadInt(nextInt);      {Id}
              ASK link TO SetID("RBDLink",nextLinkId);    {beta145}    {beta999}
              ASK root TO AddBeforeGraphic((ASK root Child("RBDBlock",0)), link);
              ASK saveFile TO ReadString(nextString);   {fromRef}  
              ASK saveFile TO ReadInt(nextInt);     {connectFromId}
              fromRef:="RBD"+nextString;
              intsArray[1]:=appendNum+nextInt;
              ASK saveFile TO ReadString(nextString);                                                          
              ASK saveFile TO ReadInt(nextInt);     {connectToId}
              toRef:="RBD"+nextString;
              intsArray[2]:=appendNum+nextInt;
              ASK link TO SetConnections(intsArray[1], intsArray[2], fromRef, toRef);
              NEW(linkPoints, 1..6);
              IF toRef = "RBDBlock"
                 block := ASK root Child(toRef, intsArray[2]);
                 linkEndX := block.Translation.x + 0.26;
                 linkEndY := block.Translation.y - 0.21;
              ELSIF toRef = "RBDEvent"
                 event := ASK root Child(toRef, intsArray[2]);
                 linkEndX := event.Translation.x + 0.26;
                 linkEndY := event.Translation.y - 0.21;
              ELSIF toRef = "RBDNode"
                 node := ASK root Child(toRef, intsArray[2]);
                 linkEndX := node.Translation.x + 0.26;
                 linkEndY := node.Translation.y - 0.21;
              ELSE
                 hier := ASK root Child(toRef, intsArray[2]);
                 linkEndX := hier.Translation.x + 0.56;
                 linkEndY := hier.Translation.y - 0.21;
              END IF;
              IF fromRef = "RBDBlock"
                 block := ASK root Child(fromRef, intsArray[1]);
                 linkStartX := block.Translation.x + 0.26;
                 linkStartY := block.Translation.y - 0.21;
              ELSIF fromRef = "RBDEvent"
                 event := ASK root Child(fromRef, intsArray[1]);
                 linkStartX := event.Translation.x + 0.26;
                 linkStartY := event.Translation.y - 0.21;
              ELSIF fromRef = "RBDNode"
                 node := ASK root Child(fromRef, intsArray[1]);
                 linkStartX := node.Translation.x + 0.26;
                 linkStartY := node.Translation.y - 0.21;
              ELSE
                 hier := ASK root Child(fromRef, intsArray[1]);
                 linkStartX := hier.Translation.x + 0.56;
                 linkStartY := hier.Translation.y - 0.21;
              END IF;
              IF (toRef = "RBDNode") AND (fromRef = "RBDBlock")
                 ASK block TO SetConnectToNode(TRUE);
              ELSIF (toRef = "RBDNode") AND (fromRef = "RBDHier")
                 ASK hier TO SetConnectToNode(TRUE);
              ELSIF (toRef = "RBDNode") AND (fromRef = "RBDEvent")
                 ASK event TO SetConnectToNode(TRUE);
              END IF;
              ASK saveFile TO ReadInt(nextInt);
              ASK link TO SetParentID(appendNum+nextInt);                
              arrowhead(toRef, linkStartX, linkStartY, linkEndX, linkEndY,
                          linkPoints[2].x, linkPoints[2].y, linkPoints[3].x, linkPoints[3].y,
                          linkPoints[4].x, linkPoints[4].y);
              linkPoints[5].x := linkPoints[2].x;
              linkPoints[5].y := linkPoints[2].y;
              linkPoints[1].x := linkStartX;
              linkPoints[1].y := linkStartY;
              linkPoints[6].x := linkEndX;
              linkPoints[6].y := linkEndY;
              ASK link TO SetPoints(linkPoints);
              IF link.parentID=activeWindow
                 ASK link TO Draw;
              ELSE
                 ASK link TO SetHidden(TRUE);
                 papaHier := ASK root Child("RBDHier", link.parentID);
                 IF papaHier=NILOBJ
                    papaHier := ASK window Child("RBDHier",link.parentID);
                 END IF;
                 ASK papaHier.childGroup TO Add(link);
              END IF;             
              DISPOSE(linkPoints);
              ASK saveFile TO ReadInt(intsArray[1]);  {coldPriority}
              ASK saveFile TO ReadReal(realsArray[1]); {autoSwitchProb}                 
              ASK saveFile TO ReadReal(realsArray[2]); {autoSwitchTime}                 
              ASK saveFile TO ReadReal(realsArray[3]); {manualSwitchTime}                 
              ASK saveFile TO ReadInt(intsArray[2]);  {capPriority}
              ASK saveFile TO ReadInt(intsArray[3]);  {nomFlow}
              ASK saveFile TO ReadInt(intsArray[4]);  {maxFlow}
              ASK link TO LinkInit(intsArray, realsArray);
           END FOR;
           ASK saveFile TO ReadString(nextString);   {end of slash line}
           ASK saveFile TO ReadLine(nextString);    {EndOfLinkData}
           IF nextString<>"EndOfLinkData"
              {add any new variables here}
           END IF;
           ASK saveFile TO ReadLine(nextString); {__SPARE_POOL}
           ASK saveFile TO ReadLine(nextString);  {column header}
           FOR i := 1 TO sparesToRead {spare pool data}
              addPoolObj:=TRUE;
              ASK saveFile TO ReadString(nextString);    {poolName}
              sparing := SparePool;
              ASK saveFile TO ReadInt(nextInt);
              IF nextInt=1
                 RSO:=TRUE;
              ELSE
                 RSO:=FALSE;
              END IF;
              ASK saveFile TO ReadInt(nextInt1);         {initSp}
              ASK saveFile TO ReadInt(nextInt2);         {newSp}
              ASK saveFile TO ReadReal(nextReal);        {newSpArr}
              ASK saveFile TO ReadInt(nextInt3);         
              IF nextInt3=1
                 ESO:=TRUE;
              ELSE
                 ESO:=FALSE;
              END IF;
              ASK saveFile TO ReadReal(nextReal1);       {emerTime}
              ASK saveFile TO ReadInt(nextInt4);
              IF nextInt4=1
                 SLO:=TRUE;
              ELSE
                 SLO:=FALSE;
              END IF;
              ASK saveFile TO ReadInt(nextInt5);         {SLOlvl}
              ASK saveFile TO ReadInt(nextInt6);         {SLOquan}
              ASK saveFile TO ReadReal(nextReal2);       {SLOtime}         
              ASK saveFile TO ReadReal(nextReal3);       {spCost}
              ASK saveFile TO ReadReal(nextReal4);       {emerCst}
              IF append
                 FOR j:=1 TO numSpPools
                    IF spareList[j]=nextString
                       addPoolObj:=FALSE;
                    END IF;
                 END FOR;
              END IF;
              IF addPoolObj
                 NEW(pool);
                 ASK pool TO SetData(nextString,nextInt1,nextInt2,nextInt5,nextInt6,RSO,ESO,SLO,nextReal, 
                                       nextReal1,nextReal2,nextReal3,nextReal4);
              END IF;   
           END FOR; 
           ASK saveFile TO ReadString(nextString);
           ASK saveFile TO ReadLine(nextString);
           IF nextString<>"EndOfSparePoolData"
              {add any new variables here}
           END IF;
           ASK saveFile TO ReadLine(nextString);                                                          
           ASK saveFile TO ReadLine(nextString);                                                          
           FOR i := 1 TO resToRead                  {RESOURCES}
              ASK saveFile TO ReadString(nextString);
              sparing := Resource;
              addPoolObj:=TRUE;
              ASK saveFile TO ReadInt(nextInt);                
              ASK saveFile TO ReadReal(nextReal);
              ASK saveFile TO ReadReal(nextReal1);
              ASK saveFile TO ReadReal(nextReal2);
              ASK saveFile TO ReadInt(nextInt1);
              IF nextInt1=1
                 phases:=TRUE;
              ELSE
                 phases:=FALSE;
              END IF;
              IF append
                 FOR j:=1 TO totalRes
                    IF resList[j]=nextString
                       addPoolObj:=FALSE;
                    END IF;
                 END FOR;
              END IF;
              IF addPoolObj
                 NEW(pool);
                 ASK pool TO SetResData(nextString,nextInt,nextReal,nextReal1,nextReal2,phases);
                 GetResList(resList);{cmc 03/12/07}
              END IF;   
           END FOR; 
           ASK saveFile TO ReadString(nextString);
           ASK saveFile TO ReadLine(nextString);   {EndOfResourceData}
           IF nextString<>"EndOfResourceData"
                {add any new variables here}
           END IF;
           ASK saveFile TO ReadLine(nextString);  {__PHASES}         
           ASK saveFile TO ReadLine(nextString);  {column header}
           IF (NOT append)
              IF (numPhases>0)
                 NEW(phaseObjArray,1..activePhases);
              ELSE
                 NEW(phaseObjArray,1..1);
              END IF;  
           END IF;
           FOR i := 1 TO numPhases             {phase data}
              NEW(phase);
              ASK saveFile TO ReadString(nextString);  {phaseName}   
              ASK saveFile TO ReadInt(nextInt);        {IDnum}    
              ASK saveFile TO ReadInt(nextInt1);       {mission}
              IF nextInt1=1
                 newBool:=TRUE;
              ELSE
                 newBool:=FALSE;
              END IF;
              ASK saveFile TO ReadInt(nextInt1);        {Dist}   
              GetNumParams(nextInt1,nextInt2);
              FOR j:=1 TO 3                      {Params}
                 ASK saveFile TO ReadReal(nextReal1);
                 IF j<=nextInt2
                    realsArray[j]:=nextReal1;
                 END IF;   
              END FOR;   
              IF (NOT append)
                 ASK phase TO SetPhaseData(nextString,nextInt1,nextInt,realsArray,newBool);
                 phaseObjArray[i]:=phase;
              END IF;   
           END FOR; 
           ASK saveFile TO ReadString(nextString);
           ASK saveFile TO ReadLine(nextString);   {EndOfPhaseData}
           IF nextString<>"EndOfPhaseData"
              {add any new variables here}
           END IF;
           ASK saveFile TO ReadLine(nextString);  {__TRIGGERS}                                                        
           ASK saveFile TO ReadLine(nextString);  {column header}
           FOR i := 1 TO trigsToRead             {trigger data}
              addPoolObj:=TRUE;
              ASK saveFile TO ReadString(nextString);  {TrigName}   
              ASK saveFile TO ReadInt(nextInt);        {IDnum}    
              ASK saveFile TO ReadInt(nextInt1);       {Repeats}
              IF nextInt1=1
                 newBool:=TRUE;
              ELSE
                 newBool:=FALSE;
              END IF;
              ASK saveFile TO ReadReal(nextReal);      {InitUsed}
              ASK saveFile TO ReadInt(nextInt1);       {TrigDist}   
              GetNumParams(nextInt1,nextInt2);
              FOR j:=1 TO 3                            {TrigParams}
                 ASK saveFile TO ReadReal(nextReal1);
                 IF j<=nextInt2
                    realsArray[j]:=nextReal1;
                 END IF;   
              END FOR; 
              IF append
                 FOR j:=1 TO totalTriggers
                    IF trigList[j]=nextString
                       addPoolObj:=FALSE;
                    END IF;
                 END FOR;
              END IF;
              IF addPoolObj
                 NEW(trig);
                 ASK trig TO SetTrigData(nextString,nextInt1,realsArray,newBool,nextReal);
                 IF append
                    INC(totalTriggers);
                    GetTrigList(trigList);
                 END IF;
              END IF;   
           END FOR;
           ASK saveFile TO ReadLine(nextString);   {EndOfTriggerData}
           IF nextString<>"EndOfTriggerData"
              {add any new variables here}
           END IF;
           IF append
              GetSpareList(spareList);  
              GetResList(resList);
           END IF;   
           ASK saveFile TO Close;
           DISPOSE(saveFile);
           DISPOSE(failVars);   
           DISPOSE(repVars);
           FOREACH hier IN hierFileGp
              ASK hierFileGp TO RemoveThis(hier);
           END FOREACH;
           ASK hierFileGp TO ObjTerminate;
           ASK window TO SetSysCursor(NormalCursor);
           IF (NOT append)
              somethingChanged := FALSE;
           END IF;   
           simOptionChanged := FALSE;
{end of read hierarchy rbd}                 
         
         
         ELSIF rapVersion=6
             nextId:=1;
             nextLinkId:=1;    {beta145}
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadInt(activePhases);
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadInt(blocksEvents);
             totalBlocks:=0;
             totalEvents:=0;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadInt(totalNodes);
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadInt(totalLinks);         
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadInt(numSpPools);
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadInt(numResPools);
             sbMessage:=FALSE;
           IF ((blocksEvents > demoCrippleLimit) AND (compileType = "demo"))
              NEW(message, 1..3);
              message[1] := "This demo version limits the number of blocks on screen ";
              message[2] := "to "+INTTOSTR(demoCrippleLimit)+".  The file you are attempting to open contains";
              message[3] := "more than "+INTTOSTR(demoCrippleLimit)+" blocks.     ";
              ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
              CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
              RETURN;
           ELSIF (compileType = "student")  
              NEW(message, 1..2);
              message[1] := "This is the student version of Raptor.";
              message[2] := "It cannot open a Raptor6 file.";
              ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                 existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
              CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
              RETURN;
             END IF;                          
             DISPOSE(phaseTimes);
             IF activePhases <> 0
                NEW(phaseTimes, 1..activePhases);
                NEW(phaseArray, 1..activePhases);
             ELSE
                NEW(phaseTimes, 1..1);
                NEW(phaseArray, 1..1);
             END IF;
             NEW(params,1..1);
             IF activePhases>0
                NEW(phaseObjArray,1..activePhases);
             ELSE   
                NEW(phaseObjArray,1..1);
             END IF;
             FOR i := 1 TO activePhases   
                NEW(phase);
                IF i=1
                   nextReal1:=0.0;
                END IF;   
                ASK saveFile TO ReadString(nextString);                  
                ASK saveFile TO ReadReal(nextReal);
                params[1]:=nextReal-nextReal1;
                nextReal1:=nextReal;
                ASK phase TO SetPhaseData(nextString,19,i,params,TRUE);
                phaseObjArray[i]:=phase;
             END FOR;
             DISPOSE(params);
             ASK saveFile TO ReadString(nextString);{/}                  
             ASK saveFile TO ReadString(nextString); {System Data}                 
             ASK saveFile TO ReadString(nextString); {System Data}                 
             ASK saveFile TO ReadLine(nextString); {}   {Start of System section }                  
             ASK saveFile TO ReadLine(nextString);
             ASK saveFile TO ReadString(nextString);
             IF ((nextString="Nuclear_Power_Plant") OR (nextString="Robot") OR (nextString="Derrick"))
                systemImage:="Equalizer";
             ELSE
                systemImage:=nextString;
             END IF;
             ASK saveFile TO ReadString(nextString);
             IF nextString = "TimeSim"
                termType := 1;
                ASK saveFile TO ReadReal(dTimeTrunc);
                ASK saveFile TO ReadReal(dTimeStartTime);
                dFailTrunc:=1.0;
                dCycleTrunc:=10.0;
             ELSIF nextString = "FailSim"
                termType := 2;
                ASK saveFile TO ReadReal(dFailTrunc);
                ASK saveFile TO ReadReal(dFailStartTime);
                dTimeTrunc:=1000.0;
                dCycleTrunc:=10.0;
             ELSE
             {tony 7-04: this cannot happen for a Raptor6 rbd
                termType := 3;
                ASK saveFile TO ReadReal(dCycleTrunc);
                ASK saveFile TO ReadReal(dCycleStartTime);
                dTimeTrunc:=1000.0;
                dFailTrunc:=1.0;    }
             END IF;
             ASK saveFile TO ReadReal(dNumberOfRuns);
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             ASK saveFile TO ReadReal(dTimeSlice);
             ASK saveFile TO ReadReal(yMin);
             ASK saveFile TO ReadReal(yMax); 
             ASK saveFile TO ReadReal(nextReal);  {zoom}
             IF nextReal <> 120.0
                cusZoomVal:=nextReal;
             ELSE
                cusZoomVal:=118.0;
             END IF;
             ASK saveFile TO ReadString(systemUnits);
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             ASK saveFile TO ReadString(nextString);                  
             IF nextString = "GraphicsOn"
                dSimWithGraph := TRUE;
             ELSE         
                dSimWithGraph := FALSE;
             END IF;
             FOR i:=1 TO 4
                ASK saveFile TO ReadInt(sysStreams[i]);
             END FOR;
                sysStreams[5] := 70;  sysStreams[6] := 71;
                sysStreams[7] := 72;  sysStreams[8] := 73;
                sysStreams[9] := 74;  sysStreams[10] :=75;   sysStreams[11] :=101;
             ASK saveFile TO ReadReal(xOrigin);   {x center}
             ASK saveFile TO ReadReal(yOrigin);   {y center} 
             ASK saveFile TO ReadInt(nextInt);   
             IF (nextInt=0)
                negShutUp:=FALSE;
             ELSE
                negShutUp:=TRUE;
             END IF;
             ASK saveFile TO ReadInt(nextInt);   
             IF (nextInt=0)
                dZeroFail:=FALSE;
             ELSE
                dZeroFail:=TRUE;
             END IF;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             ASK saveFile TO ReadInt(flowGenerated);                  
             ASK saveFile TO ReadReal(systemRedCost);   {system redCost}
             ASK saveFile TO ReadReal(sysLostCost);
             ASK saveFile TO ReadInt(nextInt);
             IF (nextInt=0)
                weakAnalysis:=FALSE;
             ELSE
                weakAnalysis:=TRUE;
             END IF;
             ASK saveFile TO ReadInt(nextInt);
             IF (nextInt=0)
                costAnalysis:=FALSE;
             ELSE
                costAnalysis:=TRUE;
             END IF;
             ASK saveFile TO ReadInt(nextInt);
             IF (nextInt=0)
                capacityAnalysis:=FALSE;
             ELSE
                capacityAnalysis:=TRUE;
             END IF;
             ASK saveFile TO ReadString(nextString); {/}                 
             ASK saveFile TO ReadLine(nextString);   {end of slash line}               
             ASK saveFile TO ReadLine(nextString);   {label string}
             ASK saveFile TO ReadString(nextString); 
             IF nextString="Availability"
                weakLinkAnalType:=1;
             ELSIF nextString="Dependability"
                weakLinkAnalType:=2;
             ELSE
                weakLinkAnalType:=3;
             END IF;
             ASK saveFile TO ReadReal(GYthreshold);
             ASK saveFile TO ReadReal(YRthreshold);
             ASK saveFile TO ReadString(nextString); {/}                 
             ASK saveFile TO ReadLine(nextString);   {end of slash line}
             ASK saveFile TO ReadLine(nextString);    {EndOfSystemData}
             IF nextString<>"EndOfSystemData"
                {add any new variables here}
             END IF;
             loadingFile:=TRUE;
             SetView(cusZoomVal,xOrigin,yOrigin); 
             loadingFile:=FALSE;
             ASK saveFile TO ReadString(nextString); {/}                 
             ASK saveFile TO ReadString(nextString); {__BLOCK_DATA}
             ASK saveFile TO ReadString(nextString); {/}
             ASK saveFile TO ReadLine(nextString);  {}                                                        
             ASK saveFile TO ReadLine(nextString);  {column header}
             IF blocksEvents>0
                NEW(isBlockArray,1..blocksEvents);
                NEW(usesPhasingArray,1..blocksEvents);
             END IF;   
             FOR i := 1 TO blocksEvents   {read in first table in .rbd file}             
                ASK saveFile TO ReadString(nextString);  {block.blockName}
                ASK saveFile TO ReadInt(nextInt1);  {block.Id}                 
                ASK saveFile TO ReadReal(nextReal1);  {xPosition}               
                ASK saveFile TO ReadReal(nextReal2);  {yPosition}
                ASK saveFile TO ReadInt(nextInt2);  {isEvent}  
                ASK saveFile TO ReadInt(nextInt3);  {block.connectIntoNum} 
                ASK saveFile TO ReadInt(nextInt4);  {block.ConnectOutOfNum}  
                ASK saveFile TO ReadInt(nextInt5);  {DependencyNum} 
                ASK saveFile TO ReadInt(nextInt6);  {usesPhasing} 
                ASK saveFile TO ReadInt(nextInt7);  {simStartType} 
                ASK saveFile TO ReadReal(nextReal3);  {amountExhausted}
                IF nextInt2=1   
                   NEW(event);
                   isBlockArray[i] := FALSE;
                   INC(totalEvents);
                   ASK event TO LoadFromLibrary (images, "RBDEvent");
                   ASK event TO SetID("RBDEvent", i);  
                   ASK eventGroup TO Add(event);
                   ASK root TO AddAfterGraphic((ASK root Child("RBDBlock",0)),event);
                   ASK event TO DisplayAt(nextReal1/1.25, nextReal2/1.25); {x & y coordinates}
                   ASK event TO Draw;
                   ASK event TO SetInitName(nextString); 
                   FOR j := 1 TO nextInt3
                      ASK event TO IncLink(INTO);
                   END FOR;
                   FOR j := 1 TO nextInt4
                      ASK event TO IncLink(OUTOF);
                   END FOR;
                   IF nextInt6 = 1
                      ASK event TO SetusesPhasing(TRUE);
                      usesPhasingArray[i]:=TRUE;
                   ELSE
                      ASK event TO SetusesPhasing(FALSE);
                      usesPhasingArray[i]:=FALSE;
                   END IF; 
                ELSE
                   NEW(block);
                   isBlockArray[i] := TRUE;
                   INC(totalBlocks);
                   ASK block TO LoadFromLibrary (images, "RBDBlock");
                   ASK block TO SetID("RBDBlock", i);   {block number}
                   ASK blockGroup TO Add(block);
                   ASK root TO AddAfterGraphic((ASK root Child("RBDBlock",0)),block);
                   ASK block TO DisplayAt(nextReal1/1.25, nextReal2/1.25); {x & y coordinates}
                   ASK block TO Draw;
                   ASK block TO SetInitName(nextString); 
                   FOR j := 1 TO nextInt3
                      ASK block TO IncLink(INTO);
                   END FOR;
                   FOR j := 1 TO nextInt4
                      ASK block TO IncLink(OUTOF);
                   END FOR;
                   ASK block TO SetDep(nextInt5,""); 
                   IF nextInt6 = 1
                      ASK block TO SetusesPhasing(TRUE);
                      usesPhasingArray[i]:=TRUE;
                   ELSE
                      ASK block TO SetusesPhasing(FALSE);
                      usesPhasingArray[i]:=FALSE;
                   END IF; 
                   ASK block TO SetsimStartType(nextInt7);
                   ASK block TO SetamountExhausted(nextReal3);
                END IF;   {event or block}
             END FOR;
           FOR i:=1 TO blocksEvents   {to set depType}
              IF isBlockArray[i]
                 block := ASK root Child("RBDBlock",i);   
                 IF (block.DependencyNum < 1)
                    nextString1 := "";
                 ELSIF (block.DependencyNum <= blocksEvents)
                    IF (isBlockArray[block.DependencyNum])
                       nextString1 := "RBDBlock";
                    ELSE
                       nextString1 := "RBDEvent";
                    END IF;
                 ELSE
                    nextString1 := "RBDNode";
                 END IF; 
                 ASK block TO SetDep(block.DependencyNum,nextString1); 
              END IF;
           END FOR;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             FOR i:=1 TO blocksEvents   {read in Failure Distribution table in .rbd file}  
                ASK saveFile TO ReadString(nextString);                  
                ASK saveFile TO ReadInt(nextInt);  {failDistro} 
                ASK saveFile TO ReadInt(nextInt1);  {numFailParams} 
                ASK saveFile TO ReadInt(nextInt2);  {failStream}  
                IF nextInt2>6
                   nextInt2:=nextInt2+194;       {system streams are now numbered 201-215}
                END IF;
                IF (isBlockArray[i])
                    block := ASK window Descendant("RBDBlock", i);
                    ASK block TO SetfailDistro(nextInt);               
                    ASK block TO SetfailStream(nextInt2);
                    IF (nextInt<>12)
                       ASK block TO SetnumFailParams(nextInt1);
                       NEW(failVars, 1..block.numFailParams);
                       FOR j := 1 TO block.numFailParams
                          ASK saveFile TO ReadReal(nextReal);  {failure distribution parameters}
                          failVars[j] := nextReal;
                       END FOR;
                    ELSE                   {tm 02-06: 7.0 error 999}
                       pearson6Used:=TRUE;
                       ASK block TO SetnumFailParams(3);
                       NEW(failVars, 1..block.numFailParams);
                       FOR j := 1 TO 3
                          ASK saveFile TO ReadReal(nextReal);  {failure distribution parameters}
                          failVars[j] := nextReal;
                       END FOR;
                       ASK saveFile TO ReadReal(nextReal);  {4th Pearson 6 param; discarded}
                    END IF;
                    ASK block TO SetfailVals(failVars);
                    DISPOSE(failVars);
                ELSE
                   event := ASK window Descendant("RBDEvent", i);
                   ASK event TO SetfailDistro(nextInt);               
                   ASK event TO SetfailStream(nextInt2);
                   NEW(failVars, 1..1);
                   ASK saveFile TO ReadReal(nextReal);  {failure distribuiton parameters}
                   ASK event TO SetfailVals(nextReal);
                   DISPOSE(failVars);
                END IF;
             END FOR;           
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             FOR i:=1 TO blocksEvents   {read in Repair Distribution table in .rbd file}                
                ASK saveFile TO ReadString(nextString);                  
                ASK saveFile TO ReadInt(nextInt);  {repairDistro}   
                ASK saveFile TO ReadInt(nextInt1);  {numRepairParams}  
                ASK saveFile TO ReadInt(nextInt2);  {repairStream}  
                IF nextInt2>6
                   nextInt2:=nextInt2+194;       {system streams are now numbered 201-215}
                END IF;
                IF (isBlockArray[i])
                   block := ASK window Descendant("RBDBlock", i);
                   ASK block TO SetrepairDistro(nextInt);               
                   ASK block TO SetnumRepairParams(nextInt1);
                   ASK block TO SetrepairStream(nextInt2);             
                   NEW(repVars, 1..block.numRepairParams);
                   FOR j := 1 TO block.numRepairParams
                      ASK saveFile TO ReadReal(nextReal); {repair distribuiton parameters}
                      repVars[j] := nextReal;
                   END FOR;
                   ASK block TO SetrepairVals(repVars);
                   DISPOSE(repVars);
                ELSE
                   ASK saveFile TO ReadReal(nextReal);
                END IF;
             END FOR;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             FOR i:=1 TO blocksEvents   {read in SBStress/GDType table in .rbd file}                
                ASK saveFile TO ReadString(nextString);                  
                ASK saveFile TO ReadInt(nextInt);  {GDType}  {1=linear,2=geometric, 3=asymptotic}
                ASK saveFile TO ReadReal(nextReal);  {GDRate}  
                ASK saveFile TO ReadReal(nextReal1);  {GDLimit} 
                ASK saveFile TO ReadReal(nextReal2);  {preLDT} 
                ASK saveFile TO ReadReal(nextReal3);  {postLDT} 
                ASK saveFile TO ReadString(nextString); {poolName}                 
                IF (isBlockArray[i])
                   block := ASK root Child("RBDBlock", i);
                   ASK block TO SetGDType(nextInt);
                   ASK block TO SetGDRate(nextReal);
                   ASK block TO SetGDLimit(nextReal1);              
                   NEW(failVars,1..1);
                   failVars[1]:=nextReal2;
                   ASK block TO SetpreDist(19);
                   ASK block TO SetpreParams(failVars);
                   failVars[1]:=nextReal3;
                   ASK block TO SetpostDist(19);
                   ASK block TO SetpostParams(failVars);
                   DISPOSE(failVars);
                   ASK block TO SetpoolName(nextString);
                ELSE
                   {don't need this data for events so we do nothing}
                END IF;
             END FOR;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             FOR i:=1 TO blocksEvents   {read in Sparing table in .rbd file}                
                ASK saveFile TO ReadString(nextString);                  
                ASK saveFile TO ReadString(nextString); {sparingType}  
                ASK saveFile TO ReadInt(nextInt);   {RSO}  
                ASK saveFile TO ReadInt(nextInt1);  {initStock} 
                ASK saveFile TO ReadInt(nextInt2);  {newSpares}                
                ASK saveFile TO ReadReal(nextReal); {arriveEvery}               
                ASK saveFile TO ReadInt(nextInt3);  {ESO} 
                ASK saveFile TO ReadReal(nextReal1);  {emerTime} 
                ASK saveFile TO ReadInt(nextInt4);  {SLO} 
                ASK saveFile TO ReadInt(nextInt5);  {level} 
                ASK saveFile TO ReadInt(nextInt6);  {quantity}                
                ASK saveFile TO ReadReal(nextReal2);  {SLOTime}
                IF (isBlockArray[i])
                   block := ASK root Child("RBDBlock", i);
                   IF nextString = "Infinite"
                      sparing := Infinite;
                      ASK block TO SetinfiniteSpares(TRUE);
                   ELSIF nextString = "Pooled"
                      sparing := SparePool;
                   ELSIF nextString = "Custom"
                      sparing := Custom;
                   ELSE
                      sparing := None;
                   END IF;
                   ASK block TO SetsparingType(sparing);
                   IF nextInt = 1
                      ASK block TO SetroutineSpareOrdering(TRUE);
                   ELSE
                      ASK block TO SetroutineSpareOrdering(FALSE);
                   END IF;
                   ASK block TO SetinitStock(nextInt1); 
                   ASK block TO SetnewSpares(nextInt2);
                   ASK block TO SetarrivalRate(nextReal);
                   IF nextInt3 = 1
                      ASK block TO SetemerSpareOrdering(TRUE);
                   ELSE
                      ASK block TO SetemerSpareOrdering(FALSE);
                   END IF;
                   ASK block TO SetemerTime(nextReal1);                
                   IF nextInt4 = 1
                      ASK block TO SetstockLevelOrdering(TRUE);
                   ELSE
                      ASK block TO SetstockLevelOrdering(FALSE);
                   END IF;               
                   ASK block TO SetSLOOrderLevel(nextInt5);               
                   ASK block TO SetSLONewSpares(nextInt6);
                   ASK block TO SetSLOTime(nextReal2);  
                ELSE
                   {don't need this data for events so we do nothing}
                END IF;
             END FOR;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             FOR i := 1 TO blocksEvents   {read in Resource table in .rbd file}                
                ASK saveFile TO ReadString(nextString);                  
                ASK saveFile TO ReadInt(nextInt);  {numDiffRes}                
                ASK saveFile TO ReadInt(nextInt1);  {numRes1}                
                ASK saveFile TO ReadString(nextString); {res1Name}                 
                ASK saveFile TO ReadInt(nextInt2);  {numRes2} 
                ASK saveFile TO ReadString(nextString1); {res2Name} 
                ASK saveFile TO ReadInt(nextInt3);  {numRes3} 
                ASK saveFile TO ReadString(nextString1); {res3Name} 
                IF (isBlockArray[i])
                   block := ASK root Child("RBDBlock", i);
                   ASK block TO SetnumDiffRes(nextInt);
                   ASK block TO SetnumRes1(nextInt1);
                   ASK block TO Setres1Name(nextString);
                ELSE
                   {events don't use this data}
                END IF;
             END FOR;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             FOR i := 1 TO blocksEvents   {read in first costing table in .rbd file}                
                ASK saveFile TO ReadString(nextString);
                ASK saveFile TO ReadReal(nextReal);  {initialCost}               
                ASK saveFile TO ReadReal(nextReal1);  {operatingCosts}               
                ASK saveFile TO ReadReal(nextReal2);  {standbyCost}               
                ASK saveFile TO ReadReal(nextReal3);  {idleCost}               
                ASK saveFile TO ReadReal(nextReal4);  {holdCost}               
                IF (isBlockArray[i])
                   block := ASK root Child("RBDBlock", i);
                   ASK block TO SetinitialCost(nextReal);                
                   ASK block TO SetOperatingCost(nextReal1); 
                   ASK block TO SetstandbyCost(nextReal2);                
                   ASK block TO SetidleCost(nextReal3);                
                   ASK block TO SetrepHoldCost(nextReal4);    
                ELSE
                   event := ASK root Child("RBDEvent", i);
                   ASK event TO SetinitialCost(nextReal);                
                   ASK event TO SetOperatingCost(nextReal1); 
                END IF;
             END FOR;             
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             FOR i := 1 TO blocksEvents   {read in 2nd costing table in .rbd file}                
                ASK saveFile TO ReadString(nextString);
                ASK saveFile TO ReadReal(nextReal);  {spareCost}               
                ASK saveFile TO ReadReal(nextReal1);  {repairingCost}               
                ASK saveFile TO ReadReal(nextReal2);  {repFixedCost}               
                ASK saveFile TO ReadReal(nextReal3);  {doneCost}               
                ASK saveFile TO ReadReal(nextReal4);  {doneFixedCost}               
                ASK saveFile TO ReadReal(nextReal5);  {emerShippingCost} 
                ASK saveFile TO ReadInt(nextInt);    {alwaysAddDoneCost} 
                IF (isBlockArray[i])
                   block := ASK root Child("RBDBlock", i);
                   ASK block TO SetspareCost(nextReal);                
                   ASK block TO SetrepairingCost(nextReal1);                
                   ASK block TO SetrepFixedCost(nextReal2);                
                   ASK block TO SetdoneCost(nextReal3);                
                   ASK block TO SetdoneFixedCost(nextReal4);                
                   ASK block TO SetemerShippingCost(nextReal5);                              
                   IF nextInt = 1
                      ASK block TO SetalwaysAddDoneCost(TRUE);
                   ELSE
                      ASK block TO SetalwaysAddDoneCost(FALSE);
                   END IF;               
                   ASK block TO SetStats();
                ELSE
                   event := ASK root Child("RBDEvent", i);
                   ASK event TO SetrepairingCost(nextReal1)
                END IF;
             END FOR;
             IF activePhases>0
                ASK saveFile TO ReadString(nextString);                  
                ASK saveFile TO ReadLine(nextString);                  
                phaseGroups:=(activePhases DIV 10)+1;
                phaseRemainder:=activePhases MOD 10;
                IF phaseRemainder=0
                   phaseGroups:=phaseGroups-1;
                   phaseRemainder:=10;      
                END IF; 
                FOR i:= 1 TO phaseGroups
                   ASK saveFile TO ReadLine(nextString); 
                END FOR;
                NEW(params,1..activePhases);
                NEW(typeArray,1..activePhases);
                FOR i := 1 TO blocksEvents   {read in phasing table in .rbd file}                
                   IF (usesPhasingArray[i])
                      ASK saveFile TO ReadString(nextString);                  
                      FOR j := 1 TO activePhases
                         ASK saveFile TO ReadInt(nextInt);  {phase values}
                         IF ((nextInt=2) AND (NOT isBlockArray[i]))
                            params[j]:=0.0;
                            typeArray[j]:="F";
                         ELSIF ((nextInt=1) AND (NOT isBlockArray[i]))
                            params[j]:=0.0;
                            typeArray[j]:="P";
                         ELSIF nextInt=0
                            params[j]:=0.0;
                            typeArray[j]:="L";
                         ELSIF nextInt=-1
                            params[j]:=0.0;
                            typeArray[j]:="C";
                         ELSE 
                            IF nextInt>99999
                               params[j]:=999.999;
                               typeArray[j]:="A";
                               {Put error message here:     tony
                               your Raptor6 file has been changed!}
                               NEW(message, 1..2);
                               message[1] := "ALERT!  Phase stress level exceeds maximum!";
                               message[2] := "Your Raptor6 file has been changed!";
                               result := SendAlert(message, FALSE, FALSE, TRUE);
                               DISPOSE(message);
                            ELSE
                               params[j]:=FLOAT(nextInt)*.01;
                               typeArray[j]:="A";
                            END IF;   
                         END IF;   
                      END FOR;
                      IF (isBlockArray[i])
                         block := ASK root Child("RBDBlock",i);
                         ASK block TO SetPhases(TRUE, params,typeArray);
                      ELSE
                         event := ASK root Child("RBDEvent",i);
                         ASK event TO SetPhases(TRUE,params,typeArray);
                      END IF;   
                   END IF;
                END FOR;
                DISPOSE(params);
                DISPOSE(typeArray);
             END IF;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);
             ASK saveFile TO ReadLine(nextString);    {EndOfBlockData}
             IF nextString<>"EndOfBlockData"
                {add any new variables here}
             END IF;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);    {Start of node section }                  
             ASK saveFile TO ReadLine(nextString); 
             foundStart:=FALSE;
             foundEnd:=FALSE;
             FOR i := 1 TO totalNodes
                NEW(node);
                ASK nodeGroup TO Add(node);
                ASK node TO LoadFromLibrary (images, "RBDNode");
                ASK saveFile TO ReadString(nextString);   {shownName} 
                ASK node TO SetName(nextString);
                ASK saveFile TO ReadInt(nextInt);    {id}
                FOR j := 1 TO 2                      {translation}
                   ASK saveFile TO ReadReal(nextReal);
                   realsArray[j] := nextReal;
                END FOR;
                ASK saveFile TO ReadInt(nextInt1);   {typeNode}
                IF nextInt1=1
                   ASK node TO LoadFromLibrary(images,"RBDStartNode");
                   foundStart:=TRUE;
                ELSIF nextInt1=3
                   ASK node TO LoadFromLibrary(images,"RBDEndNode");
                   foundEnd:=TRUE;
                ELSE
                   ASK node TO LoadFromLibrary (images, "RBDNode");
                END IF;
                ASK node TO SetID("RBDNode", (blocksEvents+nextInt));    
                ASK root TO AddAfterGraphic((ASK root Child("RBDBlock",0)),node);
                ASK node TO DisplayAt(realsArray[1]/1.25, realsArray[2]/1.25);
                ASK node TO SetSnapShot(FALSE);
                ASK node TO SetTranslation(node.Translation.x, node.Translation.y);
                ASK node TO Draw;
                ASK node TO SetType(nextInt1);
                ASK saveFile TO ReadInt(nextInt);
                ASK node TO SetGoodPaths(nextInt);
                ASK saveFile TO ReadInt(nextInt);
                FOR j := 1 TO nextInt
                   ASK node TO IncLink(INTO);
                END FOR;
                ASK saveFile TO ReadInt(nextInt);
                FOR j := 1 TO nextInt
                   ASK node TO IncLink(OUTOF);
                END FOR;                
                IF node.typeNode = 1
                   ASK node TO SetNum(-1);
                   startId := node.Id;
                   foundStart:=TRUE;
                ELSIF node.typeNode = 3
                   ASK node TO SetNum(-2);
                   endId := node.Id;
                   foundEnd:=TRUE;
                ELSE
                   ASK node TO SetNum(node.Id);
                END IF;
                IF ((node.connectIntoNum > 1) AND (node.goodPaths<>0))
                   ASK node TO SetKofN(node.goodPaths, node.connectIntoNum);
                END IF;
                ASK saveFile TO ReadInt(intsArray[1]);  {coldStandby}  
                IF intsArray[1]=1
                   sbMessage:=TRUE;
                END IF;
                ASK saveFile TO ReadInt(intsArray[2]);  {priorityReturn}                
                ASK saveFile TO ReadInt(intsArray[3]);  {checkAutosFirst}                
                ASK saveFile TO ReadInt(intsArray[4]);  {not used}                
                ASK saveFile TO ReadInt(intsArray[5]);  {usesPhasing}                
                ASK saveFile TO ReadInt(intsArray[6]);  {DependencyNum}                
                ASK node TO Init1(intsArray);
             END FOR;
             ASK saveFile TO ReadString(nextString); {capacity and reporting Booleans section}               
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
           FOR i:=1 TO totalNodes   {need to set depType since that did not exist in Raptor6}  {error 999}
              node := ASK root Child("RBDNode", (blocksEvents+i));   
              IF (node.DependencyNum < 1)
                 nextString1 := "";
              ELSIF (node.DependencyNum <= blocksEvents)
                 IF (isBlockArray[node.DependencyNum])
                    nextString1 := "RBDBlock";
                 ELSE
                    nextString1 := "RBDEvent";
                 END IF;
              ELSE
                 nextString1 := "RBDNode";
              END IF; 
              ASK node TO SetDep(node.DependencyNum,nextString1); 
           END FOR;
             FOR i := 1 TO totalNodes  
                node := ASK root Child("RBDNode", (blocksEvents+i));
                ASK saveFile TO ReadString(nextString);   {shownName}
                ASK saveFile TO ReadInt(nextInt);  {anyPath}
                IF nextInt=0
                   ASK node TO SetAnyPath(FALSE);
                ELSE
                   ASK node TO SetAnyPath(TRUE);
                END IF;
                ASK saveFile TO ReadInt(nextInt);  {fullFlow}
                IF nextInt=0
                   ASK node TO SetFullFlow(FALSE);
                ELSE
                   ASK node TO SetFullFlow(TRUE);
                END IF;
                ASK saveFile TO ReadInt(nextInt);  {reportCapacity, currently not used}
               ASK saveFile TO ReadInt(nextInt);  {reportNodeAnal}
                IF ((nextInt=0) OR (node.typeNode<>2))
                   ASK node TO SetReportNodeAnal(FALSE);
                ELSE
                   ASK node TO SetReportNodeAnal(TRUE);
                END IF;
             END FOR;
             IF activePhases>0
                NEW(phaseArray,1..activePhases);
                ASK saveFile TO ReadString(nextString);                  
                ASK saveFile TO ReadLine(nextString); 
                FOR i:= 1 TO phaseGroups
                   ASK saveFile TO ReadLine(nextString); 
                END FOR;
                FOR i := 1 TO totalNodes                
                   node := ASK root Child("RBDNode", (blocksEvents+i));
                   IF node.usesPhasing
                      ASK saveFile TO ReadString(nextString);                  
                      FOR j := 1 TO activePhases
                         ASK saveFile TO ReadInt(nextInt);  {phase values}
                         phaseArray[j]:=nextInt;
                      END FOR;
                      ASK node TO SetPhases(TRUE, phaseArray);
                   END IF;
                END FOR;
                DISPOSE(phaseArray);
             END IF;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);
             ASK saveFile TO ReadLine(nextString);    {EndOfNodeData}
             IF ((NOT foundStart) OR (NOT foundEnd))
                AutoAddStartEnd(foundStart,foundEnd);
             END IF;   
             IF nextString<>"EndOfNodeData"
                {add any new variables here}
             END IF;
             DISPOSE(phaseArray);
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadString(nextString);
             ASK saveFile TO ReadLine(nextString);   {start of Link data}                                                       
             ASK saveFile TO ReadLine(nextString); 
             FOR i := 1 TO totalLinks
                NEW (link);
                ASK linkGroup TO Add(link);
                ASK saveFile TO ReadInt(nextInt);      {Id}
                ASK link TO SetID("RBDLink", nextInt);
                ASK root TO AddBeforeGraphic((ASK root Child("RBDBlock",0)), link);
                ASK saveFile TO ReadString(nextString);   {fromRef}                                                       
                ASK saveFile TO ReadInt(nextInt1);     {connectFromId}
                IF nextString = "Block"
                   IF (isBlockArray[nextInt1])
                      fromRef := "RBDBlock";
                   ELSE
                      fromRef := "RBDEvent";
                   END IF;   
                   intsArray[1]:=nextInt1;
                ELSE
                   fromRef := "RBDNode";
                   intsArray[1]:=nextInt1+blocksEvents;
                END IF;
                ASK saveFile TO ReadString(nextString);                                                          
                ASK saveFile TO ReadInt(nextInt2);     {connectToId}
                IF nextString = "Block"                  {toRef}
                   IF (isBlockArray[nextInt2])
                      toRef := "RBDBlock";
                   ELSE
                      toRef := "RBDEvent";
                   END IF;   
                   intsArray[2]:=nextInt2;
                ELSE
                   toRef := "RBDNode";
                   intsArray[2]:=nextInt2+blocksEvents;
                END IF;
                ASK link TO SetConnections(intsArray[1], intsArray[2], fromRef, toRef);
                NEW(linkPoints, 1..6);
                IF toRef = "RBDBlock"
                   block := ASK root Child(toRef, intsArray[2]);
                   linkEndX := block.Translation.x + 0.26;
                   linkEndY := block.Translation.y - 0.21;
                ELSIF toRef = "RBDEvent"
                   event := ASK root Child(toRef, intsArray[2]);
                   linkEndX := event.Translation.x + 0.26;
                   linkEndY := event.Translation.y - 0.21;
                ELSIF (toRef = "RBDNode")
                   node := ASK root Child(toRef, intsArray[2]);
                   linkEndX := node.Translation.x + 0.26;
                   linkEndY := node.Translation.y - 0.21;
                END IF;
                IF fromRef = "RBDBlock"
                   block := ASK root Child(fromRef, intsArray[1]);
                   linkStartX := block.Translation.x + 0.26;
                   linkStartY := block.Translation.y - 0.21;
                ELSIF fromRef = "RBDEvent"
                   event := ASK root Child(fromRef, intsArray[1]);
                   linkStartX := event.Translation.x + 0.26;
                   linkStartY := event.Translation.y - 0.21;
                ELSIF fromRef = "RBDNode"
                   node := ASK root Child(fromRef, intsArray[1]);
                   linkStartX := node.Translation.x + 0.26;
                   linkStartY := node.Translation.y - 0.21;
                END IF;
                IF (toRef = "RBDNode") AND (fromRef = "RBDBlock")
                   ASK block TO SetConnectToNode(TRUE);
                END IF;
                arrowhead(toRef, linkStartX, linkStartY, linkEndX, linkEndY,
                          linkPoints[2].x, linkPoints[2].y, linkPoints[3].x, linkPoints[3].y,
                          linkPoints[4].x, linkPoints[4].y);
                linkPoints[5].x := linkPoints[2].x;
                linkPoints[5].y := linkPoints[2].y;
                linkPoints[1].x := linkStartX;
                linkPoints[1].y := linkStartY;
                linkPoints[6].x := linkEndX;
                linkPoints[6].y := linkEndY;
                ASK link TO SetPoints(linkPoints);
                ASK link TO Draw;
                DISPOSE(linkPoints);
                ASK saveFile TO ReadInt(intsArray[1]);  {coldPriority}
                ASK saveFile TO ReadReal(realsArray[1]); {autoSwitchProb}                 
                ASK saveFile TO ReadReal(realsArray[2]); {autoSwitchTime}                 
                ASK saveFile TO ReadReal(realsArray[3]); {manualSwitchTime}                 
                ASK saveFile TO ReadInt(intsArray[2]);  {capPriority}
                ASK saveFile TO ReadInt(intsArray[3]);  {nomFlow}
                ASK saveFile TO ReadInt(intsArray[4]);  {maxFlow}
                ASK link TO LinkInit(intsArray, realsArray);
             END FOR;
             ASK saveFile TO ReadString(nextString); {/}                 
             ASK saveFile TO ReadLine(nextString);   {end of slash line}
             ASK saveFile TO ReadLine(nextString);    {EndOfLinkData}
             IF nextString<>"EndOfLinkData"
                {add any new variables here}
             END IF;
             ASK saveFile TO ReadString(nextString); {/}                 
             ASK saveFile TO ReadString(nextString); {__SPARE_POOL}
             ASK saveFile TO ReadString(nextString); {/}
             ASK saveFile TO ReadLine(nextString);  {}                                                        
             ASK saveFile TO ReadLine(nextString);  {column header}
             FOR i := 1 TO numSpPools             {spare pool data}
                ASK saveFile TO ReadString(nextString);
                sparing := SparePool;
                ASK saveFile TO ReadInt(nextInt);
                IF nextInt=1
                   RSO:=TRUE;
                ELSE
                   RSO:=FALSE;
                END IF;
                ASK saveFile TO ReadInt(nextInt1);
                ASK saveFile TO ReadInt(nextInt2);
                ASK saveFile TO ReadReal(nextReal);
                ASK saveFile TO ReadInt(nextInt3);
                IF nextInt3=1
                   ESO:=TRUE;
                ELSE
                   ESO:=FALSE;
                END IF;
                ASK saveFile TO ReadReal(nextReal1);
                ASK saveFile TO ReadInt(nextInt4);
                IF nextInt4=1
                   SLO:=TRUE;
                ELSE
                   SLO:=FALSE;
                END IF;
                ASK saveFile TO ReadInt(nextInt5);
                ASK saveFile TO ReadInt(nextInt6);
                ASK saveFile TO ReadReal(nextReal2);                
                ASK saveFile TO ReadReal(nextReal3);
                ASK saveFile TO ReadReal(nextReal4);
                NEW(pool);
                ASK pool TO SetData(nextString,nextInt1,nextInt2,nextInt5,nextInt6,RSO,ESO,SLO,nextReal, 
                                     nextReal1,nextReal2,nextReal3,nextReal4);
             END FOR; 
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             ASK saveFile TO ReadLine(nextString);
             IF nextString<>"EndOfSparePoolData"
                {add any new variables here}
             END IF;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadString(nextString);
             ASK saveFile TO ReadLine(nextString);                                                          
             ASK saveFile TO ReadLine(nextString);                                                          
             FOR i := 1 TO numResPools                  {resource pool data RAPTOR 5a}
                ASK saveFile TO ReadString(nextString);
                sparing := Resource;
                ASK saveFile TO ReadInt(nextInt);                
                ASK saveFile TO ReadReal(nextReal);
                ASK saveFile TO ReadReal(nextReal1);
                ASK saveFile TO ReadReal(nextReal2);
                ASK saveFile TO ReadInt(nextInt1);
                IF nextInt=1
                   phases:=TRUE;
                ELSE
                   phases:=FALSE;
                END IF;
                NEW(pool);
                ASK pool TO SetResData(nextString,nextInt,nextReal,nextReal1,nextReal2,phases); 
             END FOR; 
             ASK saveFile TO ReadString(nextString); {/}                  
             ASK saveFile TO ReadLine(nextString);   {end of slash line}
             ASK saveFile TO ReadLine(nextString);   {EndOfResourceData}
             IF nextString<>"EndOfResourceData"
                {add any new variables here}
             END IF;
             UpdateVersion(blocksEvents,totalNodes);     {Adds in new fields to update to Raptor6.5}
             ASK saveFile TO ReadString(nextString); {/}                 
             ASK saveFile TO ReadString(nextString); {__DEFAULT_BLOCK_SETTINGS}
             ASK saveFile TO ReadString(nextString); {/}
             ASK saveFile TO ReadLine(nextString);  {}                                                        
             ASK saveFile TO ReadLine(nextString);  {column header}  
             ASK saveFile TO ReadString(stringArray[1]);  {block.blockName}
             ASK saveFile TO ReadInt(nextInt);  {isEvent}                           
             boolsArray[10]:=FALSE;
             ASK saveFile TO ReadInt(intsArray[9]);  {DependencyNum}                
             ASK saveFile TO ReadInt(nextInt);  {usesPhasing}                
             IF nextInt=1                
                boolsArray[7]:=TRUE;
             ELSE
                boolsArray[7]:=FALSE;
             END IF;
             ASK saveFile TO ReadInt(intsArray[13]);  {simStartType} 
             ASK saveFile TO ReadReal(realsArray[19]); {AmtExhausted}        
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             ASK saveFile TO ReadInt(intsArray[1]);  {failDistro}                
             ASK saveFile TO ReadInt(intsArray[2]);  {numFailParams}               
             ASK saveFile TO ReadInt(nextInt);  {failStream}                
             IF nextInt>6
                nextInt:=nextInt+194;       {system streams are now numbered 201-215}
             END IF;
             intsArray[11]:=nextInt;
             NEW(failVars, 1..intsArray[2]);
             FOR j := 1 TO intsArray[2]
                ASK saveFile TO ReadReal(nextReal);  {failure distribuiton parameters}
                failVars[j] := nextReal;
             END FOR;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             ASK saveFile TO ReadInt(intsArray[3]);  {repairDistro}                
             ASK saveFile TO ReadInt(intsArray[4]);  {numRepairParams}               
             ASK saveFile TO ReadInt(nextInt);  {repairStream}
             IF nextInt>6
                nextInt:=nextInt+194;       {system streams are now numbered 201-215}
             END IF;
             intsArray[12]:=nextInt;
             NEW(repVars, 1..intsArray[4]);
             FOR j := 1 TO intsArray[4]
                ASK saveFile TO ReadReal(nextReal); {repair distribuiton parameters}
                repVars[j] := nextReal;
             END FOR;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             ASK saveFile TO ReadInt(intsArray[10]);  {GDType}                
             ASK saveFile TO ReadReal(realsArray[17]);  {GDRate}               
             ASK saveFile TO ReadReal(realsArray[18]);  {GDLimit}               
             ASK saveFile TO ReadReal(realsArray[4]);  {preLDT}  
             intsArray[2]:=19;
             NEW(preVals,1..1);
             preVals[1]:=realsArray[4];
             ASK saveFile TO ReadReal(realsArray[5]);  {postLDT}               
             intsArray[4]:=19;
             NEW(postVals,1..1);
             postVals[1]:=realsArray[5];
             ASK saveFile TO ReadString(stringArray[2]); {poolName}                 
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             ASK saveFile TO ReadString(nextString); {sparingType} 
             IF nextString = "Infinite"
                sparing := Infinite;
             ELSIF nextString = "Pooled"
                sparing := SparePool;
             ELSIF nextString = "Custom"
                sparing := Custom;
             ELSE
                sparing := None;
             END IF;
             IF sparing=Infinite
                boolsArray[1]:=TRUE;
             ELSE
                boolsArray[1]:=FALSE;
             END IF;
             ASK saveFile TO ReadInt(nextInt);  {RSO}                
             IF nextInt=1                
                boolsArray[2]:=TRUE;
             ELSE
                boolsArray[2]:=FALSE;
             END IF;
             ASK saveFile TO ReadInt(intsArray[5]);  {initStock}                
             ASK saveFile TO ReadInt(intsArray[6]);  {newSpares}                
             ASK saveFile TO ReadReal(realsArray[1]);{arriveEvery}               
             ASK saveFile TO ReadInt(nextInt);       {ESO}                
             IF nextInt=1                
                boolsArray[4]:=TRUE;
             ELSE
                boolsArray[4]:=FALSE;
             END IF;
             ASK saveFile TO ReadReal(realsArray[3]);  {emerTime}                
             ASK saveFile TO ReadInt(nextInt);  {SLO}                
             IF nextInt=1                
                boolsArray[3]:=TRUE;
             ELSE
                boolsArray[3]:=FALSE;
             END IF;
             ASK saveFile TO ReadInt(intsArray[7]);  {SLO level}                
             ASK saveFile TO ReadInt(intsArray[8]);  {SLO quantity}                
             ASK saveFile TO ReadReal(realsArray[2]);  {SLOTime}               
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             ASK saveFile TO ReadInt(intsArray[15]);  {numDiffRes}                
             ASK saveFile TO ReadInt(intsArray[14]);  {numRes1}                
             ASK saveFile TO ReadString(stringArray[3]); {res1Name}                 
             ASK saveFile TO ReadInt(nextInt);  {numRes2}                
             ASK saveFile TO ReadString(nextString); {res2Name}                 
             ASK saveFile TO ReadInt(nextInt);  {numRes3}                
             ASK saveFile TO ReadString(nextString); {res3Name}                 
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             ASK saveFile TO ReadReal(realsArray[6]);  {initialCost}               
             ASK saveFile TO ReadReal(realsArray[7]);  {operatingCost}               
             ASK saveFile TO ReadReal(realsArray[14]);  {standbyCost}               
             ASK saveFile TO ReadReal(realsArray[12]);  {idleCost}               
             ASK saveFile TO ReadReal(realsArray[13]);  {holdCost}               
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadLine(nextString);                  
             ASK saveFile TO ReadLine(nextString); 
             ASK saveFile TO ReadReal(realsArray[10]);  {spareCost}               
             ASK saveFile TO ReadReal(realsArray[8]);  {repairingCost}               
             ASK saveFile TO ReadReal(realsArray[9]);  {repFixedCost}               
             ASK saveFile TO ReadReal(realsArray[15]);  {doneCost}               
             ASK saveFile TO ReadReal(realsArray[16]);  {doneFixedCost}               
             ASK saveFile TO ReadReal(realsArray[11]);  {emerShippingCost}               
             ASK saveFile TO ReadInt(nextInt);          {alwaysAddDoneCost}                
             IF nextInt=1                
                boolsArray[5]:=TRUE;
             ELSE
                boolsArray[5]:=FALSE;
             END IF;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadString(nextString);
             IF nextString<>"EndOfDefaultBlockData"
                    {add any new variables here}
             END IF;
             ASK saveFile TO Close;
             {tony - Set new default fields for Raptor7 - no longer necessary because of call to InitFactorySettings}
             realsArray[4]:=factReals[4];      {sbStress}
             boolsArray[11]:=factBools[11];    {usesPM}
             boolsArray[12]:=factBools[12];    {pmSpareNeeded}
             boolsArray[13]:=factBools[13];    {pmRefresh}
             boolsArray[14]:=factBools[14];    {pmMisDefer}
             boolsArray[15]:=factBools[15];    {pmFailReset}
             boolsArray[17]:=factBools[17];    {pmReqDefer}
             boolsArray[18]:=factBools[18];
             realsArray[20]:=factReals[20];    {pmStagger}
             realsArray[21]:=factReals[21];    {pmFreq}
             boolsArray[16]:=factBools[16];    {pmTriggered}
             stringArray[4]:=factStrs[4];      {pmTrig}
             stringArray[5]:=factStrs[5];      {comment}
             intsArray[16] :=factInts[16];     {pmDist}
             realsArray[23]:=factReals[23];    {pmHoldCost}
             realsArray[22]:=factReals[22];    {pmCost}
             realsArray[5] :=factReals[5];     {pmFixedCost}
             intsArray[17] :=factInts[17];     {numRes1PM}
             realsArray[24]:=factReals[24];
             realsArray[25]:=factReals[25];
             realsArray[26]:=factReals[26];
             realsArray[27]:=factReals[27];
             realsArray[28]:=factReals[28];
             NEW(pmVals,1..1);
             pmVals[1]:= 1.;        
             ASK defaultBlock TO SetBlockData(boolsArray, intsArray, realsArray,stringArray, 
                                            failVars, repVars,preVals,postVals,pmVals, sparing);
             IF ( ((totalBlocks>0) AND (totalEvents>0)) OR (sbMessage) )
                NEW(message, 1..7);
                message[1] := "ALERT!  Simulations that contain both blocks and events,";
                message[2] := "and simulations which contain standby nodes, may not"
                message[3] := "produce identical results to previous versions of Raptor,";
                message[4] := "since the order in which they draw random numbers may be";
                message[5] := "different.  The results will still be accurate.  Reference";
                message[6] := "the Raptor webpage and see the paper 'How Long Should I";
                message[7] := "Simulate, and for How Many Trials?' for more information.";
                result := SendAlert(message, FALSE, FALSE, TRUE);
                DISPOSE(message);
             END IF;
             DISPOSE(saveFile);
             DISPOSE(failVars);   
             DISPOSE(repVars);
             DISPOSE(preVals);
             DISPOSE(postVals);
             DISPOSE(pmVals);
             DISPOSE(isBlockArray);
             DISPOSE(usesPhasingArray);
             ASK window TO SetSysCursor(NormalCursor);
             somethingChanged := FALSE;
             simOptionChanged := FALSE;
                         
             
         ELSIF rapVersion=5
             defFile:=SUBSTR(1, dotPosition, fileName)+"def";
             OpenDefFile(defFile,pathName, goodFile);
             IF NOT goodFile
                NEW(message, 1..4);
                message[1] := "RAPTOR cannot open the document '"+defFile+"' which contains user";
                message[2] := "specified values for new blocks and the current simulation settings.";
                message[3] := "The file may have been moved or deleted from the current directory, or ";
                message[4] := "it is set to read only.  These values will be set to default values.";
                result := SendAlert(message, FALSE, FALSE, TRUE);
                DISPOSE(message);
                startOpen := FALSE;  
                systemImage:="Equalizer";
                termType := 1;
                dTimeTrunc:=1000.0;
                dFailTrunc:=1.0;
                dCycleTrunc:=10.0;
                dTimeStartTime:=0.0;
                dNumberOfRuns:=1.0;
                dTimeSlice:=10000000.000000;
                yMin:=0.0;
                yMax:=1.0; 
                cusZoomVal:=24.0;  {zoom}
                systemUnits:="units";
                dSimWithGraph := TRUE;
                sysStreams[1] := 7;   sysStreams[3] := 9;
                sysStreams[2] := 8;   sysStreams[4] := 10;
                sysStreams[5] := 70;  sysStreams[6] := 71;
                sysStreams[7] := 72;  sysStreams[8] := 73;
                sysStreams[9] := 74;  sysStreams[10] :=75;   sysStreams[11] :=101;
                xOrigin:=0.0;   {x center}
                yOrigin:=80.0;   {y center} 
                negShutUp:=FALSE;
                flowGenerated:=1;                  
                systemRedCost:=0.0;   {system redCost}
                sysLostCost:=0.0;
                weakAnalysis:=FALSE;
                capacityAnalysis:=FALSE;
                loadingFile:=TRUE;
                   SetView(cusZoomVal,xOrigin,yOrigin); 
                loadingFile:=FALSE;
             END IF;
             nextId:=1;
             nextLinkId:=1;    {beta145}
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadInt(activePhases);
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadInt(blocksEvents);
             totalBlocks:=0;
             totalEvents:=0;
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadInt(totalNodes);
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadInt(totalLinks);         
             ASK saveFile TO ReadString(nextString);                  
             ASK saveFile TO ReadInt(numSpPools);
             IF ((blocksEvents > demoCrippleLimit) AND (compileType = "demo"))
                NEW(message, 1..3);
                message[1] := "This demo version limits the number of blocks on screen ";
                message[2] := "to "+INTTOSTR(demoCrippleLimit)+".  The file you are attempting to open contains";
                message[3] := "more than "+INTTOSTR(demoCrippleLimit)+" blocks.     ";
                ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                    existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
                CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
                RETURN;
           ELSIF ((compileType="student") AND (blocksEvents > studentCrippleLimit))  
              NEW(message, 1..3);
              message[1] := "This student version limits the number of blocks on screen ";
              message[2] := "to "+INTTOSTR(studentCrippleLimit)+".  The file you are attempting to open contains";
              message[3] := "more than "+INTTOSTR(studentCrippleLimit)+" blocks.     ";
              ErrorOpeningFile(totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents,existingBlocks,existingNodes,
                existingLinks,existingHiers,existingEvents,totalTriggers,numResPools,saveFile,fileIsOpen,saveCancelled,append,nixed);
              CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
              RETURN;
             END IF;                          
             DISPOSE(phaseTimes);
             IF activePhases <> 0
                NEW(phaseTimes, 1..activePhases);
                NEW(phaseArray, 1..activePhases);
             ELSE
                NEW(phaseTimes, 1..1);
                NEW(phaseArray, 1..1);
             END IF;
             NEW(params,1..1);
             IF activePhases>0
                NEW(phaseObjArray,1..activePhases);
             ELSE   
                NEW(phaseObjArray,1..1);
             END IF;
             FOR i := 1 TO activePhases   
                NEW(phase);
                IF i=1
                   nextReal1:=0.0;
                END IF;   
                ASK saveFile TO ReadString(nextString);                  
                ASK saveFile TO ReadReal(nextReal);
                params[1]:=nextReal-nextReal1;
                nextReal1:=nextReal;
                ASK phase TO SetPhaseData(nextString,19,i,params,TRUE);
                phaseObjArray[i]:=phase;
             END FOR;
             DISPOSE(params);
           ASK saveFile TO ReadString(nextString);                 
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);  
           NEW(isBlockArray,1..blocksEvents);
           FOR i := 1 TO blocksEvents   {read in first table in .rbd file}             
              ASK saveFile TO ReadInt(nextInt1);  {block.Id}                 
              ASK saveFile TO ReadReal(nextReal1);  {xPosition}               
              ASK saveFile TO ReadReal(nextReal2);  {yPosition}
              ASK saveFile TO ReadInt(nextInt2);  {isEvent}                            
              ASK saveFile TO ReadString(nextString);  {block.blockName}
              IF nextInt2=1                
                 NEW(event);
                 isBlockArray[i] := FALSE;
                 INC(totalEvents);
                 ASK event TO LoadFromLibrary (images, "RBDEvent");
                 ASK event TO SetID("RBDEvent",nextInt1);  
                 ASK eventGroup TO Add(event);
                 ASK root TO AddAfterGraphic((ASK root Child("RBDBlock",0)),event);
                 ASK event TO DisplayAt(nextReal1/1.25, nextReal2/1.25); {x & y coordinates}
                 ASK event TO Draw;
                 ASK event TO SetInitName(nextString); 
              ELSE
                 NEW(block);
                 isBlockArray[i]:=TRUE;
                 INC(totalBlocks);
                 ASK block TO LoadFromLibrary (images, "RBDBlock");
                 ASK block TO SetID("RBDBlock", nextInt1);   {block number}     
                 ASK root TO AddAfterGraphic((ASK root Child("RBDBlock",0)),block);            
                 ASK block TO DisplayAt(nextReal1/1.25, nextReal2/1.25); {x & y coordinates}
                 ASK block TO Draw;
                 ASK block TO SetInitName(nextString); 
                 ASK block TO SetsimStartType(3);
                 ASK block TO SetamountExhausted(0.0);
                 ASK blockGroup TO Add(block);
              END IF;
           END FOR;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           FOR i := 1 TO blocksEvents   {read in second table in .rbd file}                
              ASK saveFile TO ReadString(nextString); 
              ASK saveFile TO ReadInt(nextInt1);  {block.connectIntoNum} 
              ASK saveFile TO ReadInt(nextInt2);  {block.ConnectOutOfNum}  
              ASK saveFile TO ReadInt(nextInt3);  {DependencyNum}  
              IF isBlockArray[i]                
                 block :=   ASK root Child("RBDBlock",i);
                 FOR j := 1 TO nextInt2
                    ASK block TO IncLink(OUTOF);
                 END FOR;
                 FOR j := 1 TO nextInt1
                    ASK block TO IncLink(INTO);
                 END FOR;
                 IF nextInt3 = 1
                    ASK block TO SetDep(-2,"");              
                 ELSE
                    ASK block TO SetDep(0,"");              
                 END IF;
              ELSE
                 event := ASK root Child("RBDEvent",i);
                 FOR j := 1 TO nextInt2
                    ASK event TO IncLink(OUTOF);
                 END FOR;
                 FOR j := 1 TO nextInt1
                    ASK event TO IncLink(INTO);
                 END FOR;
              END IF;     
           END FOR;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           FOR i := 1 TO blocksEvents   {read in third table in .rbd file}                
              ASK saveFile TO ReadString(nextString);                  
              ASK saveFile TO ReadInt(nextInt);  {failDistro} 
              ASK saveFile TO ReadInt(nextInt1);  {numFailParams} 
              IF nextInt=12           {tm 02-06: 7.0 error 999}
                 pearson6Used:=TRUE;
                 nextInt1:=3;
              END IF;
              ASK saveFile TO ReadInt(nextInt2);  {failStream}  
              IF nextInt2>6
                 nextInt2:=nextInt2+194;       {system streams are now numbered 201-215}
              END IF;
              ASK saveFile TO ReadInt(nextInt3);  {repairDistro}   
              ASK saveFile TO ReadInt(nextInt4);  {numRepairParams}  
              ASK saveFile TO ReadInt(nextInt5);  {repairStream}  
              IF nextInt5>6
                 nextInt5:=nextInt5+194;       {system streams are now numbered 201-215}
              END IF;              
              IF isBlockArray[i]
                 block := ASK root Child("RBDBlock",i);
                 ASK block TO SetfailDistro(nextInt);               
                 ASK block TO SetnumFailParams(nextInt1);
                 ASK block TO SetfailStream(nextInt2);
                 ASK block TO SetrepairDistro(nextInt3);               
                 ASK block TO SetnumRepairParams(nextInt4);
                 ASK block TO SetrepairStream(nextInt5);  
              ELSE
                 event := ASK root Child("RBDEvent",i);
                 ASK event TO SetfailDistro(nextInt);               
                 ASK event TO SetfailStream(nextInt2);
              END IF;
           END FOR;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);   
           FOR i := 1 TO blocksEvents   {read in fourth table in .rbd file}                
              ASK saveFile TO ReadString(nextString);                  
              ASK saveFile TO ReadInt(nextInt1);       {useResource}  
              ASK saveFile TO ReadInt(nextInt2);       {numResources}               
              ASK saveFile TO ReadString(nextString);  {resourceName}                 
              IF isBlockArray[i]
                 block := ASK root Child("RBDBlock",i);
                 IF nextInt1 = 1
                    ASK block TO SetnumDiffRes(1);
                    ASK block TO SetnumRes1(nextInt2);
                    ASK block TO Setres1Name(nextString);
                 ELSE
                    ASK block TO SetnumDiffRes(0);
                    ASK block TO SetnumRes1(1);
                    ASK block TO Setres1Name("None                ");
                 END IF;
              ELSE
                 {events don't use resources}
              END IF;
           END FOR;
           {More fields for a Raptor5}
           InitFactorySettings;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           FOR i := 1 TO blocksEvents   {read in fifth table in .rbd file}
              ASK saveFile TO ReadString(nextString);
              ASK saveFile TO ReadInt(nextInt);  {infiniteSpares}                
              ASK saveFile TO ReadReal(nextReal);  {ALDTDelay}  
              ASK saveFile TO ReadString(nextString); {sparingType}  
              IF nextString = "Infinite"
                 sparing := Infinite;
              ELSIF nextString = "Cold"
                 sparing := Infinite;
              ELSIF nextString = "Spare"
                 sparing := SparePool;
              ELSIF nextString = "Custom"
                 sparing := Custom;
              ELSE
                 sparing := None;
              END IF;
              ASK saveFile TO ReadString(nextString); {poolName}   
              IF isBlockArray[i]
                 block := ASK root Child("RBDBlock",i);
                 IF nextInt = 1
                    ASK block TO SetinfiniteSpares(TRUE);
                 ELSE
                    ASK block TO SetinfiniteSpares(TRUE);
                 END IF;                
                 NEW(failVars,1..1);
                 failVars[1]:=nextReal;   {Set preLDT fields to update to Raptor7}
                 ASK block TO SetpreDist(19);
                 ASK block TO SetpreParams(failVars);
                 failVars[1]:=0.0;
                 ASK block TO SetpostDist(19);
                 ASK block TO SetpostParams(failVars);
                 DISPOSE(failVars);
                 ASK block TO SetsparingType(sparing);               
                 ASK block TO SetpoolName(nextString);
              ELSE
                 {not relevant for events}
              END IF;
           END FOR;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           FOR i := 1 TO blocksEvents   {read in sixth table in .rbd file}                
              ASK saveFile TO ReadString(nextString);    
              ASK saveFile TO ReadInt(nextInt);  {initStock} 
              ASK saveFile TO ReadInt(nextInt1);  {newSpares}                
              ASK saveFile TO ReadReal(nextReal);{arriveEvery}               
              ASK saveFile TO ReadReal(nextReal1);  {emerTime} 
              IF isBlockArray[i]
                 block := ASK root Child("RBDBlock",i);
                 ASK block TO SetinitStock(nextInt);               
                 ASK block TO SetarrivalRate(nextReal);
                 ASK block TO SetnewSpares(nextInt1);
                 ASK block TO SetemerTime(nextReal1);
                 IF (block.newSpares=0) OR (block.arrivalRate<0.00001)      
                    ASK block TO SetroutineSpareOrdering(FALSE);
                    ASK block TO SetnewSpares(factInts[6]);
                    ASK block TO SetarrivalRate(factReals[1]);
                 ELSE
                    IF block.sparingType=Custom
                       ASK block TO SetroutineSpareOrdering(TRUE);
                    ELSE
                       ASK block TO SetroutineSpareOrdering(FALSE);
                    END IF;
                 END IF;
                 IF (block.emerTime<0.00001)
                    ASK block TO SetemerSpareOrdering(FALSE);
                    ASK block TO SetemerTime(factReals[3]);
                 ELSE
                   IF block.sparingType=Custom      
                    ASK block TO SetemerSpareOrdering(TRUE);
                   ELSE
                    ASK block TO SetemerSpareOrdering(FALSE);
                   END IF;
                 END IF;                
                 ASK block TO SetstockLevelOrdering(FALSE);
                 ASK block TO SetSLOOrderLevel(factInts[7]);               
                 ASK block TO SetSLONewSpares(factInts[8]);
                 ASK block TO SetSLOTime( factReals[2]);  
              END IF;
           END FOR;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           FOR i := 1 TO blocksEvents   {read in seventh table in .rbd file}                
              ASK saveFile TO ReadString(nextString); 
              IF isBlockArray[i]   
                 block := ASK root Child("RBDBlock", i);
                 NEW(failVars, 1..block.numFailParams);
                 FOR j := 1 TO block.numFailParams
                    ASK saveFile TO ReadReal(nextReal);  {failure distribuiton parameters}
                    failVars[j] := nextReal;
                 END FOR;
                 IF block.failDistro=12
                    ASK saveFile TO ReadReal(nextReal); {tm 02-06: 4th Pearson6 param, discarded}
                 END IF;
                 ASK block TO SetfailVals(failVars);
                 DISPOSE(failVars);
              ELSE
                 event := ASK root Child("RBDEvent",i);
                 ASK saveFile TO ReadReal(nextReal);
                 ASK event TO SetfailVals(nextReal);
              END IF;
           END FOR;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           FOR i := 1 TO blocksEvents   {read in eighth table in .rbd file}                
              ASK saveFile TO ReadString(nextString);                  
              IF isBlockArray[i]    
                 block := ASK root Child("RBDBlock", i);
                 NEW(repVars, 1..block.numRepairParams);
                 FOR j := 1 TO block.numRepairParams
                    ASK saveFile TO ReadReal(nextReal); {repair distribuiton parameters}
                    repVars[j] := nextReal;
                 END FOR;
                 ASK block TO SetrepairVals(repVars);
                 DISPOSE(repVars);
              ELSE
                 ASK saveFile TO ReadReal(nextReal);
              END IF;
           END FOR;             
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);  
           FOR i := 1 TO blocksEvents   {read in ninth table in .rbd file}                
              ASK saveFile TO ReadString(nextString);                  
              ASK saveFile TO ReadInt(nextInt);
              IF nextInt=1        {usesPhasing}
                 IF activePhases > 0
                    NEW(params,1..activePhases);
                    NEW(typeArray,1..activePhases);
                    FOR j := 1 TO activePhases
                       ASK saveFile TO ReadInt(nextInt);  {phase values}
                       IF ((nextInt=2) AND (NOT isBlockArray[i])) 
                          params[j]:=0.0;
                          typeArray[j]:="F";
                       ELSIF ((nextInt=1) AND (NOT isBlockArray[i]))
                          params[j]:=0.0;
                          typeArray[j]:="P";
                       ELSIF nextInt=0
                          params[j]:=0.0;
                          typeArray[j]:="L";
                       ELSIF nextInt=-1
                          params[j]:=0.0;
                          typeArray[j]:="C";
                       ELSE 
                          IF nextInt>99999
                             params[j]:=999.999;
                             typeArray[j]:="A";
                             {Put error message here:     tony
                             your Raptor5 file has been changed!}
                             NEW(message, 1..2);
                             message[1] := "WARNING!  Phase stress level exceeds maximum!";
                             message[2] := "Your Raptor5 file has been changed!";
                             result := SendAlert(message, FALSE, FALSE, TRUE);
                             DISPOSE(message);
                          ELSE
                             params[j]:=FLOAT(nextInt)*.01;
                             typeArray[j]:="A";
                          END IF;   
                       END IF;   
                    END FOR;
                    IF isBlockArray[i]
                       block := ASK root Child("RBDBlock",i);
                       ASK block TO SetPhases(TRUE, params,typeArray);
                    ELSE
                       event := ASK root Child("RBDEvent",i);
                       ASK event TO SetPhases(TRUE,params,typeArray);
                    END IF;
                    DISPOSE(params);
                    DISPOSE(typeArray);
                 ELSE   {no activePhases defined, but block usesPhasing}
                    IF isBlockArray[i]
                       block := ASK root Child("RBDBlock",i);
                       ASK block TO SetPhases(TRUE, NILARRAY,NILARRAY);
                    ELSE
                       event := ASK root Child("RBDEvent",i);
                       ASK event TO SetPhases(TRUE, NILARRAY,NILARRAY);
                    END IF;
                 END IF;   
              ELSE
                 IF isBlockArray[i]
                    block := ASK root Child("RBDBlock",i);
                    ASK block TO SetusesPhasing(FALSE);
                 ELSE
                    event := ASK root Child("RBDEvent",i);
                    ASK event TO SetusesPhasing(FALSE);
                 END IF;
              END IF;
           END FOR;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           FOR i := 1 TO blocksEvents   {read in ninth table in .rbd file}                
              ASK saveFile TO ReadString(nextString); 
              ASK saveFile TO ReadInt(nextInt);    {blockDegrades}
              ASK saveFile TO ReadReal(nextReal);  {degradefactor}                 
              ASK saveFile TO ReadInt(nextInt1);   {obe autoSwitching}
              ASK saveFile TO ReadReal(nextReal1); {obe autoSwitchProb}                 
              ASK saveFile TO ReadReal(nextReal2); {obe autoSwitchTime}                 
              ASK saveFile TO ReadInt(nextInt2);   {obe manualSwitching}
              ASK saveFile TO ReadReal(nextReal3); {obe manualSwitchTime} 
              IF isBlockArray[i]
                 block := ASK root Child("RBDBlock",i);
                 IF nextInt=1
                    ASK block TO SetGDType(2);  {2=geometric}
                    ASK block TO SetGDRate(nextReal);
                 ELSE
                    ASK block TO SetGDType(0);  {2=geometric}
                    ASK block TO SetGDRate(nextReal);
                 END IF;
                 IF block.GDRate<=1.0
                    ASK block TO SetGDLimit(0.000001);              
                 ELSE
                    ASK block TO SetGDLimit(999999999.999999);              
                 END IF;
              END IF;
           END FOR;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           FOR i := 1 TO blocksEvents   {read in tenth table in .rbd file} 
              ASK saveFile TO ReadString(nextString); 
              ASK saveFile TO ReadReal(nextReal);  {initialCost}               
              ASK saveFile TO ReadReal(nextReal1);  {operatingCost}               
              ASK saveFile TO ReadReal(nextReal2);  {repairingCost}               
              ASK saveFile TO ReadReal(nextReal3);  {spareCost}               
              IF isBlockArray[i]
                 block := ASK root Child("RBDBlock", i);
                 ASK block TO SetinitialCost(nextReal);                
                 ASK block TO SetOperatingCost(nextReal1);                
                 ASK block TO SetrepairingCost(nextReal2);                
                 ASK block TO SetspareCost(nextReal3);   
                 ASK block TO SetstandbyCost(1.0);                
                 ASK block TO SetidleCost(1.0);                
                 ASK block TO SetrepHoldCost(1.0);                
                 ASK block TO SetrepFixedCost(1.0);                
                 ASK block TO SetdoneCost(1.0);                
                 ASK block TO SetdoneFixedCost(1.0);                
                 ASK block TO SetemerShippingCost(1.0);                              
                 ASK block TO SetalwaysAddDoneCost(FALSE);
                 ASK block TO SetStats();
              ELSE
                 event := ASK root Child("RBDEvent",i);
                 ASK event TO SetinitialCost(nextReal);                
                 ASK event TO SetOperatingCost(nextReal1);                
                 ASK event TO SetrepairingCost(nextReal2);                
              END IF;
           END FOR;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);
           foundStart:=FALSE;
           foundEnd:=FALSE;                                
           FOR i := 1 TO totalNodes
              NEW(node);
              ASK saveFile TO ReadInt(nextInt);     {Id}
              ASK saveFile TO ReadString(nextString);   {shownName} 
              FOR j := 1 TO 2
                 ASK saveFile TO ReadReal(nextReal);
                 realsArray[j] := nextReal;
              END FOR;
              ASK saveFile TO ReadInt(nextInt1);    {typeNode}
              IF nextInt1=1
                 ASK node TO LoadFromLibrary(images,"RBDStartNode");
                 foundStart:=TRUE;
              ELSIF nextInt1=3
                 ASK node TO LoadFromLibrary(images,"RBDEndNode");
                 foundEnd:=TRUE;
              ELSE
                 ASK node TO LoadFromLibrary (images, "RBDNode");
              END IF;
              ASK node TO SetID("RBDNode", (blocksEvents+nextInt));
              ASK root TO AddAfterGraphic((ASK root Child("RBDBlock",0)),node);
              ASK node TO SetName(nextString);
              ASK node TO DisplayAt(realsArray[1]/1.25, realsArray[2]/1.25);
              ASK node TO SetSnapShot(FALSE);
              ASK node TO SetTranslation(node.Translation.x, node.Translation.y);
            {  ASK node TO Draw;     }
              ASK node TO SetType(nextInt1);
              ASK saveFile TO ReadInt(nextInt);
              ASK node TO SetGoodPaths(nextInt);
              ASK saveFile TO ReadInt(nextInt);
              FOR j := 1 TO nextInt
                 ASK node TO IncLink(INTO);
              END FOR;
              ASK saveFile TO ReadInt(nextInt);
              FOR j := 1 TO nextInt
                 ASK node TO IncLink(OUTOF);
              END FOR;                
              IF node.typeNode = 1
                 ASK node TO SetNum(-1);
                 startId := node.Id;
              ELSIF node.typeNode = 3
                 ASK node TO SetNum(-2);
                 endId := node.Id;
              ELSE
                 ASK node TO SetNum(node.Id);
              END IF;
              IF ((node.connectIntoNum > 1) AND (node.goodPaths<>0))
                 ASK node TO SetKofN(node.goodPaths, node.connectIntoNum);
              END IF;
              intsArray[1]:=0;  {coldStandby}                
              intsArray[2]:=1;  {priorityReturn}                
              intsArray[3]:=1;  {checkAutosFirst}                
              intsArray[4]:=0;  {not used}                
              intsArray[5]:=0;  {usesPhasing} 
              intsArray[6]:=0;  {DependencyNum}
              ASK node TO Init1(intsArray);
              ASK node TO SetAnyPath(TRUE);
              ASK node TO SetFullFlow(TRUE);
              IF (node.typeNode=2)
                 ASK node TO SetReportNodeAnal(TRUE);
              ELSE
                 ASK node TO SetReportNodeAnal(FALSE);              
              END IF;
              ASK nodeGroup TO Add(node);
           END FOR;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);  
           FOR i := 1 TO totalNodes             
              ASK saveFile TO ReadString(nextString); 
              node := ASK root Child("RBDNode", (blocksEvents+i));                
              ASK saveFile TO ReadInt(nextInt);
              IF nextInt=1    {node uses Phasing}
                 IF activePhases>0
                    FOR j := 1 TO activePhases
                       ASK saveFile TO ReadInt(phaseArray[j]);
                    END FOR;                   
                    ASK node TO SetPhases(TRUE,phaseArray); 
                 ELSE
                    ASK node TO SetPhases(TRUE,NILARRAY); 
                 END IF;
              END IF;  {uses Phasing}
           END FOR;
           DISPOSE(phaseArray);
           IF ((NOT foundStart) OR (NOT foundEnd))
              AutoAddStartEnd(foundStart,foundEnd);
           END IF;   
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                                                          
           FOR i := 1 TO totalLinks
              NEW (link);
              ASK saveFile TO ReadInt(nextInt);      {Id}
              ASK link TO SetID("RBDLink", nextInt);
              ASK root TO AddBeforeGraphic((ASK root Child("RBDBlock",0)), link);
              ASK saveFile TO ReadString(nextString);   {fromRef}                                                       
              fromRef := "RBD"+nextString;
              ASK saveFile TO ReadInt(intsArray[1]);     {connectFromId}
              ASK saveFile TO ReadString(nextString);                                                          
              toRef := "RBD"+nextString;
              ASK saveFile TO ReadInt(intsArray[2]);     {connectToId}
              NEW(linkPoints, 1..6);
              IF toRef = "RBDBlock"
                 IF isBlockArray[intsArray[2]]
                    block := ASK root Child(toRef, intsArray[2]);
                    linkEndX := block.Translation.x + 0.26;
                    linkEndY := block.Translation.y - 0.21;
                 ELSE 
                    toRef := "RBDEvent";
                    event := ASK root Child(toRef, intsArray[2]);
                    linkEndX := event.Translation.x + 0.26;
                    linkEndY := event.Translation.y - 0.21;
                 END IF;   
              ELSE
                 intsArray[2]:=intsArray[2]+blocksEvents;
                 node := ASK root Child(toRef, intsArray[2]);
                 linkEndX := node.Translation.x + 0.26;
                 linkEndY := node.Translation.y - 0.21;
              END IF;
              IF fromRef = "RBDBlock"
                 IF isBlockArray[intsArray[1]]
                    block := ASK root Child(fromRef, intsArray[1]);
                    linkStartX := block.Translation.x + 0.26;
                    linkStartY := block.Translation.y - 0.21;
                 ELSE 
                    fromRef := "RBDEvent";
                    event := ASK root Child(fromRef,intsArray[1]);
                    linkStartX := event.Translation.x + 0.26;
                    linkStartY := event.Translation.y - 0.21;
                 END IF;   
              ELSE
                 intsArray[1]:=intsArray[1]+blocksEvents;
                 node := ASK root Child(fromRef, intsArray[1]);
                 linkStartX := node.Translation.x + 0.26;
                 linkStartY := node.Translation.y - 0.21;
              END IF;
              IF (toRef = "RBDNode") AND (fromRef = "RBDBlock")
                 ASK block TO SetConnectToNode(TRUE);
              END IF;
              IF ((toRef = "RBDNode") AND (fromRef = "RBDEvent"))
                 ASK event TO SetConnectToNode(TRUE);
              END IF;
              ASK link TO SetConnections(intsArray[1], intsArray[2], fromRef, toRef);
              arrowhead(toRef, linkStartX, linkStartY, linkEndX, linkEndY,
                        linkPoints[2].x, linkPoints[2].y, linkPoints[3].x, linkPoints[3].y,
                        linkPoints[4].x, linkPoints[4].y);
              linkPoints[5].x := linkPoints[2].x;
              linkPoints[5].y := linkPoints[2].y;
              linkPoints[1].x := linkStartX;
              linkPoints[1].y := linkStartY;
              linkPoints[6].x := linkEndX;
              linkPoints[6].y := linkEndY;
              ASK link TO SetPoints(linkPoints);
              ASK link TO Draw;
              DISPOSE(linkPoints);
              intsArray[1]:=50;  {coldPriority}
              realsArray[1]:=1.0; {autoSwitchProb}                 
              realsArray[2]:=0.0; {autoSwitchTime}                 
              realsArray[3]:=1.0; {manualSwitchTime}                 
              intsArray[2]:=50;  {capPriority}
              intsArray[3]:=-1;  {nomFlow}  
              intsArray[4]:=-1;  {maxFlow}
              ASK link TO LinkInit(intsArray, realsArray);
              ASK linkGroup TO Add(link);
           END FOR;
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString);                  
           ASK saveFile TO ReadString(nextString); 
           rap4ColdPools:=0;                                            
           FOR i := 1 TO numSpPools
              ASK saveFile TO ReadString(nextString);   {poolType}
              IF nextString = "SparePool"
                 sparing := SparePool;
              ELSIF nextString = "ColdPool"  
                 {pool will be deleted}
              ELSE
                 sparing := Resource;
              END IF;
              ASK saveFile TO ReadInt(nextInt);        {initSp}
              ASK saveFile TO ReadInt(nextInt1);       {newSp}
              ASK saveFile TO ReadReal(nextReal);      {newSpArr}
              ASK saveFile TO ReadReal(nextReal1);     {emerTime}
              ASK saveFile TO ReadReal(nextReal2);     {spCost}
              ASK saveFile TO ReadString(nextString1); {poolName}
              IF nextString = "ColdPool"
                 INC(rap4ColdPools);
              ELSIF nextString = "SparePool"
                 NEW(pool);
                 IF (nextInt1=0) OR (nextReal<0.00001)      
                    RSO:= FALSE;
                    nextInt1:=1;
                    nextReal:=720.;
                 ELSE
                    RSO:= TRUE;
                 END IF;
                 IF (nextReal1>0.00001)
                    ESO:= TRUE;
                 ELSE
                    ESO:= FALSE;
                    nextReal1:=24.0;
                 END IF;
                 ASK pool TO SetData(nextString1,nextInt,nextInt1,0,1,RSO,ESO,FALSE,nextReal,nextReal1,720.0,nextReal2,0.0);
              ELSE
                 NEW(pool);
                 ASK pool TO SetResData(nextString1,nextInt,0.0,0.0,nextReal2,FALSE); 
              END IF;
           END FOR; 
           numSpPools:=numSpPools-rap4ColdPools;
           UpdateVersion(blocksEvents,totalNodes);     {Adds the necessary fields to become a Raptor7 rbd}
           DISPOSE(saveFile);
           DISPOSE(isBlockArray);
           ASK window TO SetSysCursor(NormalCursor);
           somethingChanged := FALSE;
           simOptionChanged := FALSE; 
        END IF;  {RAPTOR5}
        IF pearson6Used
           NEW(message, 1..3);
           message[1] := "    One or more of your blocks uses the Pearson 6 distribution.";
           message[2] := "    In Raptor 7.0, the Pearson 6 supports only three parameters.";
           message[3] := "We recommend editing the blocks that use the Pearson 6 distribution.";
           result := SendAlert(message, FALSE, FALSE, FALSE);
           DISPOSE(message);
        END IF;
        CleanArrays(posArray,realsArray,idsArray,intsArray,boolsArray,stringArray);
     ELSE
        IF NOT append
           ResetNewFile; 
        END IF;
     END IF;  {saveCancelled}
     IF (compileType = "student")
        weakAnalysis := FALSE;
        costAnalysis := FALSE;
        capacityAnalysis := FALSE;
        FOREACH block IN blockGroup
           ASK block TO SetUsesPM(FALSE);
           ASK block TO SetusesPhasing(FALSE);
        END FOREACH;
     END IF;
  END IF;
  cancelled := FALSE;
  nixed := saveCancelled;
END PROCEDURE; {OpenFile}

PROCEDURE SaveFile(IN fileName, pathName, filter                               : STRING;
                   IN totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents : INTEGER);
VAR
   i,j,temp                                                                    : INTEGER;          
   Date,pathString,tempString                                                  : STRING;
   saveFile                                                                    : StreamObj;        
   block                                                                       : RBDBlockObj;      
   node                                                                        : RBDNodeObj; 
   link                                                                        : LinkObj;          
   pool                                                                        : SparePoolObj; 
   trig                                                                        : RapTriggerObj;
   phase                                                                       : PhaseObj;
   tab                                                                         : CHAR;
   result,failEmp,repEmp                                                       : BOOLEAN;
   hier                                                                        : RBDHierObj;
   event                                                                       : RBDEventObj;
   KofNlabel                                                                   : TextObj;
BEGIN
   result:=TRUE;
   IF (rapVersion=5) OR (rapVersion=6)
      NEW(message,1..4);
      IF rapVersion=5                  
         message[1] := "This file is a RAPTOR 5.0 file.  If you save it, it will be     ";
         message[3] := "RAPTOR 5.0.  Hit 'Cancel' and SaveAs using a different name     ";
      ELSIF rapVersion=6
         message[1] := "This file is a RAPTOR 6.0 file.  If you save it, it will be     ";
         message[3] := "RAPTOR 6.0.  Hit 'Cancel' and SaveAs using a different name     ";
      END IF;   
      message[2] := "saved as a "+versionStr+" file and can no longer be used with     ";
      message[4] := "to preserve the original file. 'OK' saves as a "+versionStr+" file.     ";
      result := SendAlert(message,TRUE, FALSE, FALSE);
      DISPOSE(message);
   END IF;
  IF ((studentFile) AND (compileType="release"))      {beta999}
     NEW(message,1..4);
     message[1] := "This file is a RAPTOR 7.0 student file.  If you save it, it will be saved as a     ";  
     message[2] := ""+versionStr+" full release file and can no longer be used with the student version     ";
     message[3] := "of Raptor.  Hit 'Cancel' and then select SaveAs using a different name to     ";
     message[4] := "preserve the original file. 'OK' saves as a "+versionStr+" release file.     ";
     result := SendAlert(message,TRUE, FALSE, FALSE);
     DISPOSE(message);
  END IF;
  IF NOT result
     dontClose := TRUE;
     RETURN;
  ELSE
     rapVersion := 7;
     studentFile := FALSE;  
  END IF;
  IF ProtectedFile
     NEW(message, 1..2);
     message[1] := "There file is overwrite protected.   ";
     message[2] := "To save use SaveAs and select another name or directory  ";
     result := SendAlert(message, FALSE, FALSE, FALSE);
     DISPOSE(message);
     RETURN;
  END IF;
   tab:=9C;
   ASK window TO SetSysCursor(BusyCursor);
   pathString := pathName + fileName;
   NEW(saveFile);
   ASK saveFile TO Open(pathString, Output);
   IF saveFile.ioResult <> 0
      NEW(message, 1..3);
      message[1] := "There is a problem saving the "+fileName+" file.     ";
      message[2] := "Make sure this file has not been set to read only     ";
      message[3] := "and that you have permission to write to the save directory.    ";
      result := SendAlert(message, FALSE, FALSE, FALSE);
      DISPOSE(message);
      ASK window TO SetSysCursor(NormalCursor);   {cmc 10/8/08}
      RETURN;
   END IF;
   DateTime(Date);
  ASK saveFile TO WriteString("Version7_"+devVersion+"        "+fileName+"        "+Date);
   ASK saveFile TO WriteLn;
   ASK saveFile TO WriteString("Phases=   ");
   ASK saveFile TO WriteInt(activePhases, 5);
   ASK saveFile TO WriteLn;
   ASK saveFile TO WriteString("Blocks=   ");
   ASK saveFile TO WriteInt(totalBlocks, 5);
   ASK saveFile TO WriteLn;
   ASK saveFile TO WriteString("Events=   ");
   ASK saveFile TO WriteInt(totalEvents, 5);
   ASK saveFile TO WriteLn;
   ASK saveFile TO WriteString("Nodes=    ");
   ASK saveFile TO WriteInt(totalNodes, 5);
   ASK saveFile TO WriteLn;
   ASK saveFile TO WriteString("Links=    ");
   ASK saveFile TO WriteInt(totalLinks, 5);
   ASK saveFile TO WriteLn;
   ASK saveFile TO WriteString("SpPools=  ");
   ASK saveFile TO WriteInt(totalSpares, 5);
   ASK saveFile TO WriteLn;
   ASK saveFile TO WriteString("ResPools= ");
   ASK saveFile TO WriteInt(totalRes, 5);
   ASK saveFile TO WriteLn;
   ASK saveFile TO WriteString("Triggers= ");
   ASK saveFile TO WriteInt(totalTriggers, 5);
   ASK saveFile TO WriteLn;
   ASK saveFile TO WriteString("Hiers=    ");
   ASK saveFile TO WriteInt(totalHiers, 5);
   ASK saveFile TO WriteLn;
   ASK saveFile TO WriteString("HierDepths=");
   ASK saveFile TO WriteInt(deepestLevel, 4);
   ASK saveFile TO WriteLn;
   ASK saveFile TO WriteString("/");
   ASK saveFile TO WriteLn;
  ASK saveFile TO WriteString("/___________________________________SYSTEM_DATA"+"______________________________________________________________________________"); 
  ASK saveFile TO WriteLn;
  ASK saveFile TO WriteString("imageString   TerminationType         TermCondition   StartStats      NumRuns ");
  ASK saveFile TO WriteString(" graphTimeSlice        graphYmin       graphYmax         zoomVal   units ");
  ASK saveFile TO WriteString("   graphicsUse     SysStreamsArray                                    xOrigin       yOrigin  ShowNegErrMsg  IgnoreStartFail ");
  ASK saveFile TO WriteString("flowGenerated systemRedCost     sysLostCost weakAnalysis costAnalysis capacityAnalysis ");
  ASK saveFile TO WriteString("nodeAnalType   GYthreshold  YRthreshold   NotUsed ");    
  ASK saveFile TO WriteLn;  
  ASK saveFile TO WriteString(SUBSTR(1,14,systemImage + "                    "));
  IF termType = 1
     ASK saveFile TO WriteString("TimeSim          ");
     ASK saveFile TO WriteReal(dTimeTrunc, 15, 6);
     ASK saveFile TO WriteString(" ");
     ASK saveFile TO WriteReal(dTimeStartTime, 15, 6);
     ASK saveFile TO WriteString(" ");
  ELSIF termType = 2
     ASK saveFile TO WriteString("FailSim          ");
     ASK saveFile TO WriteReal(dFailTrunc, 15, 6);
     ASK saveFile TO WriteString(" ");
     ASK saveFile TO WriteReal(dFailStartTime, 15, 6); 
     ASK saveFile TO WriteString(" ");
  ELSIF termType = 3
     ASK saveFile TO WriteString("CycleSim         ");
     ASK saveFile TO WriteReal(dCycleTrunc, 15, 6);
     ASK saveFile TO WriteString(" ");
     ASK saveFile TO WriteReal(dCycleStartTime, 15, 6); 
     ASK saveFile TO WriteString(" ");
  END IF;      
  ASK saveFile TO WriteReal(dNumberOfRuns, 15, 6);
  ASK saveFile TO WriteString(" ");
  ASK saveFile TO WriteReal(dTimeSlice, 15, 6);
  ASK saveFile TO WriteString(" ");
  ASK saveFile TO WriteReal(yMin, 15, 6); 
  ASK saveFile TO WriteString(" ");
  ASK saveFile TO WriteReal(yMax, 15, 6);
  ASK saveFile TO WriteString(" ");
  ASK saveFile TO WriteReal(cusZoomVal, 15, 6);  {zoom}
  ASK saveFile TO WriteString("   ");
  ASK saveFile TO WriteString(SUBSTR(1,8,systemUnits + "                    ")+"  ");
  IF dSimWithGraph
     ASK saveFile TO WriteString("GraphicsOn");
  ELSE
     ASK saveFile TO WriteString("GraphicsOff");
  END IF;  
  ASK saveFile TO WriteString("   ");
  FOR i:=1 TO 11
     ASK saveFile TO WriteInt(sysStreams[i], 4);
  END FOR;
  ASK saveFile TO WriteReal(xOrigin, 15, 6);   {x center}
  ASK saveFile TO WriteReal(yOrigin, 15, 6);   {y center} 
  ASK saveFile TO WriteString("       ");
  IF negShutUp
     ASK saveFile TO WriteInt(1,6);
  ELSE
     ASK saveFile TO WriteInt(0,6);
  END IF;
  ASK saveFile TO WriteString("       ");
  IF dZeroFail
     ASK saveFile TO WriteInt(1,6);
  ELSE
     ASK saveFile TO WriteInt(0,6);
  END IF;
  ASK saveFile TO WriteString("     ");
  ASK saveFile TO WriteInt(flowGenerated, 9);
  ASK saveFile TO WriteString("   ");
  ASK saveFile TO WriteReal(systemRedCost, 15, 6);
  ASK saveFile TO WriteString(" ");
  ASK saveFile TO WriteReal(sysLostCost, 15, 6);
  IF weakAnalysis
     ASK saveFile TO WriteInt(1,6);
  ELSE
     ASK saveFile TO WriteInt(0,6);
  END IF;
  IF costAnalysis
     ASK saveFile TO WriteInt(1,13);
  ELSE
     ASK saveFile TO WriteInt(0,13);
  END IF;
  IF capacityAnalysis
     ASK saveFile TO WriteInt(1,13);
  ELSE
     ASK saveFile TO WriteInt(0,13);
  END IF;
  ASK saveFile TO WriteString("             ");
  IF (weakLinkAnalType=1)
     ASK saveFile TO WriteString("Availability ");
  ELSIF (weakLinkAnalType=2)
     ASK saveFile TO WriteString("Dependability");
  ELSE 
     ASK saveFile TO WriteString("Reliability  ");
  END IF;  
  ASK saveFile TO WriteReal(GYthreshold, 13, 6);
  ASK saveFile TO WriteReal(YRthreshold, 13, 6);
  ASK saveFile TO WriteString("     ");
  IF compileType="student"
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;   
  ASK saveFile TO WriteLn; 
  ASK saveFile TO WriteString("EndOfSystemData");
  ASK saveFile TO WriteLn;
  ASK saveFile TO WriteString("/____________________________________HIERARCHY_DATA______________________________________________________________________________"); 
  ASK saveFile TO WriteLn;   
  ASK saveFile TO WriteString("hierarchyName             ID        xPos"+
                   "          yPos  Parent     In     Out isCNd    zoom    xCenter    yCenter    in   out level Phs");
  ASK saveFile TO WriteString("   xOrigin       yOrigin"+
                   "          myDepth    extraInt1  extraInt2  extraReal1     extraReal2");
  ASK saveFile TO WriteLn;   
  FOREACH hier IN hierGroup
     ASK saveFile TO WriteString(SUBSTR(1,20,hier.name + "                    ")+"  ");
     ASK saveFile TO WriteInt(hier.Id, 6);
     ASK saveFile TO WriteReal(hier.xPosition*1.25, 12, 6);
     ASK saveFile TO WriteString("  ");
     ASK saveFile TO WriteReal(hier.yPosition*1.25, 12, 6); 
     ASK saveFile TO WriteString("   ");
     ASK saveFile TO WriteInt(hier.parentID,3);
     ASK saveFile TO WriteString("     ");
     ASK saveFile TO WriteInt(hier.connectIntoNum, 4);
     ASK saveFile TO WriteString("    ");
     ASK saveFile TO WriteInt(hier.connectOutOfNum, 4);
     ASK saveFile TO WriteString("    ");
     IF hier.isConnectedNode
        ASK saveFile TO WriteInt(1, 1);
     ELSE
        ASK saveFile TO WriteInt(0, 1);
     END IF;
     ASK saveFile TO WriteString(" ");
     ASK saveFile TO WriteReal(hier.zoom,9, 2);
     ASK saveFile TO WriteString(" ");
     ASK saveFile TO WriteReal(hier.xCenter,10, 6);
     ASK saveFile TO WriteString(" ");
     ASK saveFile TO WriteReal(hier.yCenter,10, 6);
     ASK saveFile TO WriteString("  ");
     ASK saveFile TO WriteInt(hier.inID, 4);
     ASK saveFile TO WriteString(" ");
     ASK saveFile TO WriteInt(hier.outID, 4);
     ASK saveFile TO WriteInt(hier.level, 5);
     ASK saveFile TO WriteString("    ");
     IF hier.usesPhasing
        ASK saveFile TO WriteInt(1, 1);
     ELSE
        ASK saveFile TO WriteInt(0, 1);
     END IF;
     ASK saveFile TO WriteReal(hier.xOrigin, 12, 6);    {xOrigin}
     ASK saveFile TO WriteString("  ");
     ASK saveFile TO WriteReal(hier.yOrigin, 12, 6);    {yOrigin}
     ASK saveFile TO WriteString("        ");
     ASK saveFile TO WriteInt(hier.myDepth, 6);
     ASK saveFile TO WriteString("     ");
     ASK saveFile TO WriteInt(hier.Id, 6);
     ASK saveFile TO WriteString("     ");
     ASK saveFile TO WriteInt(hier.Id, 6);
     ASK saveFile TO WriteString("     ");
     ASK saveFile TO WriteReal(hier.xOrigin,10, 6);    {extraReal}
     ASK saveFile TO WriteString("     ");
     ASK saveFile TO WriteReal(hier.yOrigin,10, 6);    {extraReal}
     ASK saveFile TO WriteString("  ");
     ASK saveFile TO WriteLn;
  END FOREACH;
  ASK saveFile TO WriteString("EndOfHierarchyData");
  ASK saveFile TO WriteLn;
  ASK saveFile TO WriteString("/____________________________________BLOCK_DATA______________________________________________________________________________"); 
  ASK saveFile TO WriteLn;   
  ASK saveFile TO WriteString("blockName                 ID        xPos "+
   "            yPos    ParentId    In     Out  Phasing  SimStart     AmtExhausted ");
  ASK saveFile TO WriteString("FDist #FParam  FStream         FParam1           FParam2           FParam3 ");
  ASK saveFile TO WriteString("  RDist #RParam  RStream         RParam1           RParam2           RParam3 ");
  ASK saveFile TO WriteString("  SBStress      GDType       GDRate             GDLimit           SparePoolName ");
  ASK saveFile TO WriteString("       SpType    RSO  Stock    NewSp     ArriveEvery    "+
             "ESO       EmerTime     SLO  Level   Quan         SLOTime ");
  ASK saveFile TO WriteString("Pre-LDTDist         Param1            Param2            Param3 ");
  ASK saveFile TO WriteString("Post-LDTDist         Param1            Param2            Param3 ");
  ASK saveFile TO WriteString("NumRes  #Res1Rep    #Res1pmRep   Resource1Name ");
  ASK saveFile TO WriteString("    usesPM    PMSpare  PMRefresh  PMMisDefer  PMFailReset  "+
                   "PMReqDefer   PMStaggerTime           PMFreq ");
  ASK saveFile TO WriteString("pmTriggered    pmTrig             Dist              Param1           "+
                      " Param2            Param3 ");
  ASK saveFile TO WriteString("  DepNum     DepNothing    DepIdle"+
                                 "      DepPM    DepFail  DepPMThresh  defDepStateIdle   DepType ");
  ASK saveFile TO WriteString("      initial         Running           standby          idle ");
  ASK saveFile TO WriteString("    repHold         repPerTime      repFixed   donePerTime     doneFixed ");
  ASK saveFile TO WriteString(" pmHoldCost      pmCost   pmFixedCost   "+
                "  perSpare   emerShipping   addDone ");
  FOR j:=1 TO activePhases
     IF ( j  < 10 )
        ASK saveFile TO WriteString("P 00"+INTTOSTR(j)+"       ");
     ELSIF (j < 100)   
        ASK saveFile TO WriteString("P 0"+INTTOSTR(j)+"       ");
     ELSE
        ASK saveFile TO WriteString("P "+INTTOSTR(j)+"       ");
     END IF;   
  END FOR;
  ASK saveFile TO WriteLn;   
  FOREACH block IN blockGroup
     ASK saveFile TO WriteString(SUBSTR(1,20,block.name + "                    ")+"  ");
     ASK saveFile TO WriteInt(block.Id, 6);
     ASK saveFile TO WriteReal(block.xPosition*1.25, 12, 6);
     ASK saveFile TO WriteString("    ");
     ASK saveFile TO WriteReal(block.yPosition*1.25, 12, 6); 
     ASK saveFile TO WriteString("      ");
     ASK saveFile TO WriteInt(block.parentID, 3);
     ASK saveFile TO WriteString("        ");
     ASK saveFile TO WriteInt(block.connectIntoNum, 1);
     ASK saveFile TO WriteString("      ");
     ASK saveFile TO WriteInt(block.connectOutOfNum, 1);
     ASK saveFile TO WriteString("      ");
     IF block.usesPhasing
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("        ");      
     ASK saveFile TO WriteInt(block.simStartType, 1);
     ASK saveFile TO WriteString("         ");      
     ASK saveFile TO WriteReal(block.amountExhausted, 12, 6);
     ASK saveFile TO WriteString("    ");      
     ASK saveFile TO WriteInt(block.failDistro, 2);
     ASK saveFile TO WriteString("       ");      
     ASK saveFile TO WriteInt(block.numFailParams, 1);
     ASK saveFile TO WriteString("      ");      
     ASK saveFile TO WriteInt(block.failStream, 2);
     ASK saveFile TO WriteString(" ");      
     FOR j := 1 TO 3
        IF (block.failDistro=16)    {empirical}
           failEmp:=TRUE;
        END IF;
        IF ((j<=block.numFailParams) AND (NOT failEmp))
           ASK saveFile TO WriteReal(block.failVals[j], 16, 6);
        ELSE
           ASK saveFile TO WriteReal(0.0,16,6);
        END IF;
        ASK saveFile TO WriteString("  ");  
     END FOR;
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteInt(block.repairDistro, 2);
     ASK saveFile TO WriteString("       ");      
     ASK saveFile TO WriteInt(block.numRepairParams,1);
     ASK saveFile TO WriteString("      ");      
     ASK saveFile TO WriteInt(block.repairStream,2);
     ASK saveFile TO WriteString("  ");      
     FOR j := 1 TO 3
        IF (block.repairDistro=16)    {empirical}
           repEmp:=TRUE;
        END IF;
        IF ((j<=block.numRepairParams) AND (NOT repEmp))
           ASK saveFile TO WriteReal(block.repairVals[j], 16, 6);
        ELSE
           ASK saveFile TO WriteReal(0.0,16,6);
        END IF;
        ASK saveFile TO WriteString("  ");  
     END FOR;
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteReal(block.sbStress, 7,3);
     ASK saveFile TO WriteString("        ");     
     ASK saveFile TO WriteInt(block.GDType, 1);
     ASK saveFile TO WriteString(" ");      
     ASK saveFile TO WriteReal(block.GDRate,16,6);
     ASK saveFile TO WriteString("   ");      
     ASK saveFile TO WriteReal(block.GDLimit,16,6);
     ASK saveFile TO WriteString("           ");           
     ASK saveFile TO WriteString(SUBSTR(1,20,block.poolName+"                        ")+" ");
     IF block.sparingType = Infinite
        ASK saveFile TO WriteString("Infinite   ");
     ELSIF block.sparingType = SparePool
        ASK saveFile TO WriteString("Pooled     ");
     ELSIF block.sparingType = Custom
        ASK saveFile TO WriteString("Custom     ");
     ELSE
        ASK saveFile TO WriteString("None       ");
     END IF;
     IF block.routineSpareOrdering
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("   ");      
     ASK saveFile TO WriteInt(block.initStock, 5);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteInt(block.newSpares, 5);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteReal(block.arrivalRate,16,6);
     ASK saveFile TO WriteString("     ");
     IF block.emerSpareOrdering
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteReal(block.emerTime,16,6);      
     ASK saveFile TO WriteString("      ");      
     IF block.stockLevelOrdering
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteInt(block.SLOOrderLevel, 5);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteInt(block.SLONewSpares, 5);
     ASK saveFile TO WriteString(" ");      
     ASK saveFile TO WriteReal(block.SLOTime,16,6);
     ASK saveFile TO WriteString("   ");
     ASK saveFile TO WriteInt(block.preDist, 2);
     ASK saveFile TO WriteString("      ");   
     GetNumParams(block.preDist,temp);   
     FOR j := 1 TO 3
        IF j<=temp
           ASK saveFile TO WriteReal(block.preParams[j], 16, 6);
        ELSE
           ASK saveFile TO WriteReal(0.0,16,6);
        END IF;   
        ASK saveFile TO WriteString("  ");  
     END FOR;                
     ASK saveFile TO WriteString("  ");   
     ASK saveFile TO WriteInt(block.postDist, 2);
     ASK saveFile TO WriteString("      ");   
     GetNumParams(block.postDist,temp);      
     FOR j := 1 TO 3
        IF j<=temp
           ASK saveFile TO WriteReal(block.postParams[j], 16, 6);
        ELSE
           ASK saveFile TO WriteReal(0.0,16,6);
        END IF;   
        ASK saveFile TO WriteString("  ");  
     END FOR;              
     ASK saveFile TO WriteInt(block.numDiffRes,1);
     ASK saveFile TO WriteString("    ");      
     ASK saveFile TO WriteInt(block.numRes1,5);
     ASK saveFile TO WriteString("        ");
     ASK saveFile TO WriteInt(block.numRes1PM,5);
     ASK saveFile TO WriteString("         ");
     IF block.numDiffRes>0
        ASK saveFile TO WriteString(SUBSTR(1,20,block.res1Name + "                    ")+"  ");
     ELSE
        ASK saveFile TO WriteString("None                "+"  ");
     END IF;
     IF block.usesPM
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("         "); 
     IF block.pmSpareNeeded
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("         "); 
     IF block.pmRefresh
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("         "); 
     IF block.pmMisDefer
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("           "); 
     IF block.pmFailReset
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("             "); 
     IF block.pmReqDefer
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("    "); 
     ASK saveFile TO WriteReal(block.pmStagger,16,6);
     ASK saveFile TO WriteString(" ");
     ASK saveFile TO WriteReal(block.pmFreq,16,6);
     ASK saveFile TO WriteString("    ");
     IF block.pmTriggered
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("           "); 
     ASK saveFile TO WriteString(SUBSTR(1,20,block.pmTrig + "                    "));
     ASK saveFile TO WriteInt(block.pmDist, 2);
     ASK saveFile TO WriteString("     ");   
     GetNumParams(block.pmDist,temp);    
     FOR j := 1 TO 3
        IF j<=temp
           ASK saveFile TO WriteReal(block.pmParams[j], 16, 6);
        ELSE
           ASK saveFile TO WriteReal(0.0,16,6);
        END IF;   
        ASK saveFile TO WriteString("  ");  
     END FOR;  
     ASK saveFile TO WriteInt(block.DependencyNum, 5);
     ASK saveFile TO WriteString("        ");
     ASK saveFile TO WriteReal(block.DepNothingPerc, 9, 2);
     ASK saveFile TO WriteString("  "); 
     ASK saveFile TO WriteReal(block.DepIdlePerc, 9, 2);
     ASK saveFile TO WriteString("  "); 
     ASK saveFile TO WriteReal(block.DepPMPerc, 9, 2);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteReal(block.DepFailPerc, 9, 2);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteReal(block.DepPMThreshold, 9, 2);
     ASK saveFile TO WriteString("         ");      
     IF block.defDepStateIdle
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("            "); 
     IF (block.depType<>"")
        ASK saveFile TO WriteString(SUBSTR(1,9,block.depType+"      "));
     ELSE
        ASK saveFile TO WriteString("None     ");
     END IF;
     ASK saveFile TO WriteReal(block.initialCost, 12, 2);
     ASK saveFile TO WriteString("    "); 
     ASK saveFile TO WriteReal(block.operatingCost, 12, 2);
     ASK saveFile TO WriteString("      "); 
     ASK saveFile TO WriteReal(block.standbyCost, 12, 2);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteReal(block.idleCost, 12, 2);
     ASK saveFile TO WriteReal(block.repHoldCost, 12, 2);
     ASK saveFile TO WriteString("       ");      
     ASK saveFile TO WriteReal(block.repairingCost, 12, 2);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteReal(block.repFixedCost, 12, 2);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteReal(block.doneCost, 12, 2);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteReal(block.doneFixedCost, 12, 2);
     ASK saveFile TO WriteReal(block.pmHoldCost,12,2);
     ASK saveFile TO WriteReal(block.pmCost,12,2);
     ASK saveFile TO WriteString("  ");
     ASK saveFile TO WriteReal(block.pmFixedCost,12,2);
     ASK saveFile TO WriteString(" ");
     ASK saveFile TO WriteReal(block.spareCost, 12, 2);  
     ASK saveFile TO WriteString("   ");
     ASK saveFile TO WriteReal(block.emerShippingCost, 12, 2);      
     ASK saveFile TO WriteString("      ");      
     IF block.alwaysAddDoneCost
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("      ");      
     IF block.usesPhasing
        IF block.phaseValue=NILARRAY
           ASK block TO SetPhases(TRUE,NILARRAY,NILARRAY);
        END IF;
        FOR j := 1 TO activePhases
           ASK saveFile TO WriteString(block.phaseType[j]+": ");
           ASK saveFile TO WriteReal(block.phaseValue[j],7,3);               
           ASK saveFile TO WriteString("  ");
        END FOR;
     END IF;
     IF failEmp
        ASK saveFile TO WriteLn;
        ASK saveFile TO WriteString("                         ");
        FOR i:=1 TO block.numFailParams
           ASK saveFile TO WriteReal(block.failVals[i], 16, 6);
           ASK saveFile TO WriteString("  ");  
        END FOR;
        failEmp:=FALSE;
     END IF;
     IF repEmp
        ASK saveFile TO WriteLn;
        ASK saveFile TO WriteString("                         ");
        FOR i:=1 TO block.numRepairParams
           ASK saveFile TO WriteReal(block.repairVals[i], 16, 6);
           ASK saveFile TO WriteString("  ");  
        END FOR;
        repEmp:=FALSE;
     END IF;
     ASK saveFile TO WriteLn;
  END FOREACH;
  ASK saveFile TO WriteString(SUBSTR(1,20,defaultBlock.name + "                    ")+"  ");
  ASK saveFile TO WriteInt(0,6);
  ASK saveFile TO WriteReal(0.0,12,6);
  ASK saveFile TO WriteString("    ");
  ASK saveFile TO WriteReal(0.0,12,6);
  ASK saveFile TO WriteString("      ");
  ASK saveFile TO WriteInt(0,3);
  ASK saveFile TO WriteString("        ");
  ASK saveFile TO WriteInt(0,1);
  ASK saveFile TO WriteString("      ");
  ASK saveFile TO WriteInt(0,1);
  ASK saveFile TO WriteString("      ");
  IF defaultBlock.usesPhasing
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  ASK saveFile TO WriteString("        ");      
  ASK saveFile TO WriteInt(defaultBlock.simStartType, 1);
  ASK saveFile TO WriteString("         ");      
  ASK saveFile TO WriteReal(defaultBlock.amountExhausted,12,6);
  ASK saveFile TO WriteString("    ");
  ASK saveFile TO WriteInt(defaultBlock.failDistro, 2);
  ASK saveFile TO WriteString("       ");      
  ASK saveFile TO WriteInt(defaultBlock.numFailParams, 1);
  ASK saveFile TO WriteString("      ");      
  ASK saveFile TO WriteInt(defaultBlock.failStream, 2);
  ASK saveFile TO WriteString(" ");      
  FOR j := 1 TO 3
     IF (defaultBlock.failDistro=16)    {empirical}
        failEmp:=TRUE;
     END IF;
     IF ((j<=defaultBlock.numFailParams) AND (NOT failEmp))
        ASK saveFile TO WriteReal(defaultBlock.failVals[j], 16, 6);
     ELSE
        ASK saveFile TO WriteReal(0.0,16,6);
     END IF;
     ASK saveFile TO WriteString("  ");  
  END FOR;
  ASK saveFile TO WriteString("  ");      
  ASK saveFile TO WriteInt(defaultBlock.repairDistro, 2);
  ASK saveFile TO WriteString("       ");      
  ASK saveFile TO WriteInt(defaultBlock.numRepairParams,1);
  ASK saveFile TO WriteString("      ");      
  ASK saveFile TO WriteInt(defaultBlock.repairStream,2);
  ASK saveFile TO WriteString("  ");      
  FOR j := 1 TO 3
     IF (defaultBlock.repairDistro=16)    {empirical}
        repEmp:=TRUE;
     END IF;
     IF ((j<=defaultBlock.numRepairParams) AND (NOT repEmp))
        ASK saveFile TO WriteReal(defaultBlock.repairVals[j], 16, 6);
     ELSE
        ASK saveFile TO WriteReal(0.0,16,6);
     END IF;
     ASK saveFile TO WriteString("  ");  
  END FOR;
  ASK saveFile TO WriteString("  ");      
  ASK saveFile TO WriteReal(defaultBlock.sbStress,7,3);
  ASK saveFile TO WriteString("        ");      
  ASK saveFile TO WriteInt(defaultBlock.GDType, 1);
  ASK saveFile TO WriteString(" ");      
  ASK saveFile TO WriteReal(defaultBlock.GDRate,16,6);
  ASK saveFile TO WriteString("   ");      
  ASK saveFile TO WriteReal(defaultBlock.GDLimit,16,6);
  ASK saveFile TO WriteString("           ");           
  ASK saveFile TO WriteString(SUBSTR(1,20,defaultBlock.poolName+"                        ")+" ");
  IF defaultBlock.sparingType = Infinite
     ASK saveFile TO WriteString("Infinite   ");
  ELSIF defaultBlock.sparingType = SparePool
     ASK saveFile TO WriteString("Pooled     ");
  ELSIF defaultBlock.sparingType = Custom
     ASK saveFile TO WriteString("Custom     ");
  ELSE
     ASK saveFile TO WriteString("None       ");
  END IF;
  IF defaultBlock.routineSpareOrdering
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  ASK saveFile TO WriteString("   ");      
  ASK saveFile TO WriteInt(defaultBlock.initStock, 5);
  ASK saveFile TO WriteString("  ");      
  ASK saveFile TO WriteInt(defaultBlock.newSpares, 5);
  ASK saveFile TO WriteString("  ");      
  ASK saveFile TO WriteReal(defaultBlock.arrivalRate,16,6);
  ASK saveFile TO WriteString("     ");
  IF defaultBlock.emerSpareOrdering
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  ASK saveFile TO WriteString("");      
  ASK saveFile TO WriteReal(defaultBlock.emerTime,16,6);      
  ASK saveFile TO WriteString("      ");      
  IF defaultBlock.stockLevelOrdering
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  ASK saveFile TO WriteString(" ");      
  ASK saveFile TO WriteInt(defaultBlock.SLOOrderLevel, 5);
  ASK saveFile TO WriteString("   ");      
  ASK saveFile TO WriteInt(defaultBlock.SLONewSpares, 5);
  ASK saveFile TO WriteString(" ");      
  ASK saveFile TO WriteReal(defaultBlock.SLOTime,16,6);
  ASK saveFile TO WriteString("   ");
  ASK saveFile TO WriteInt(defaultBlock.preDist, 2);
  ASK saveFile TO WriteString("      ");   
  GetNumParams(defaultBlock.preDist,temp);
  FOR j := 1 TO 3
     IF j<=temp
        ASK saveFile TO WriteReal(defaultBlock.preParams[j], 16, 6);
     ELSE
        ASK saveFile TO WriteReal(0.0,16,6);
     END IF;   
     ASK saveFile TO WriteString("  ");           
  END FOR;
  ASK saveFile TO WriteString("  ");
  ASK saveFile TO WriteInt(defaultBlock.postDist, 2);
  ASK saveFile TO WriteString("      ");   
  GetNumParams(defaultBlock.postDist,temp);
  FOR j := 1 TO 3
     IF j<=temp
        ASK saveFile TO WriteReal(defaultBlock.postParams[j], 16, 6);
     ELSE
        ASK saveFile TO WriteReal(0.0,16,6);
     END IF;   
     ASK saveFile TO WriteString("  ");           
  END FOR;
  ASK saveFile TO WriteInt(defaultBlock.numDiffRes,1);
  ASK saveFile TO WriteString("    ");      
  ASK saveFile TO WriteInt(defaultBlock.numRes1,5);
  ASK saveFile TO WriteString("        ");      
  ASK saveFile TO WriteInt(defaultBlock.numRes1PM,5);
  ASK saveFile TO WriteString("         ");
  IF defaultBlock.numDiffRes>0
     ASK saveFile TO WriteString(SUBSTR(1,20,defaultBlock.res1Name + "                    ")+"  ");
  ELSE
     ASK saveFile TO WriteString("None                "+"  ");
  END IF;
  IF defaultBlock.usesPM
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  ASK saveFile TO WriteString("         ");
  IF defaultBlock.pmSpareNeeded
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  ASK saveFile TO WriteString("         ");
  IF defaultBlock.pmRefresh
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  ASK saveFile TO WriteString("         ");
  IF defaultBlock.pmMisDefer
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  ASK saveFile TO WriteString("           ");
  IF defaultBlock.pmFailReset
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  ASK saveFile TO WriteString("             ");
  IF defaultBlock.pmReqDefer
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  ASK saveFile TO WriteString("        ");
  ASK saveFile TO WriteReal(defaultBlock.pmStagger, 12, 6);
  ASK saveFile TO WriteString("     ");      
  ASK saveFile TO WriteReal(defaultBlock.pmFreq, 12, 6);
  ASK saveFile TO WriteString("    ");      
  IF defaultBlock.pmTriggered
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  ASK saveFile TO WriteString("           ");
  ASK saveFile TO WriteString(SUBSTR(1,20,defaultBlock.pmTrig + "                       "));
  ASK saveFile TO WriteInt(defaultBlock.pmDist, 2);
  ASK saveFile TO WriteString("     ");   
  GetNumParams(defaultBlock.pmDist,temp);
  FOR j := 1 TO 3
     IF j<=temp
        ASK saveFile TO WriteReal(defaultBlock.pmParams[j], 16, 6);
     ELSE
        ASK saveFile TO WriteReal(0.0,16,6);
     END IF;   
     ASK saveFile TO WriteString("  ");           
  END FOR;
  ASK saveFile TO WriteInt(defaultBlock.DependencyNum, 5);
  ASK saveFile TO WriteString("        ");
  ASK saveFile TO WriteReal(defaultBlock.DepNothingPerc, 9, 2);
  ASK saveFile TO WriteString("  "); 
  ASK saveFile TO WriteReal(defaultBlock.DepIdlePerc, 9, 2);
  ASK saveFile TO WriteString("  "); 
  ASK saveFile TO WriteReal(defaultBlock.DepPMPerc, 9, 2);
  ASK saveFile TO WriteString("  ");      
  ASK saveFile TO WriteReal(defaultBlock.DepFailPerc, 9, 2);
  ASK saveFile TO WriteString("  ");      
  ASK saveFile TO WriteReal(defaultBlock.DepPMThreshold, 9, 2);
  ASK saveFile TO WriteString("         ");   
  IF defaultBlock.defDepStateIdle
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  ASK saveFile TO WriteString("            ");      
  IF (defaultBlock.depType<>"")
     ASK saveFile TO WriteString(SUBSTR(1,9,defaultBlock.depType+"      "));
  ELSE
     ASK saveFile TO WriteString("None     ");
  END IF;
  ASK saveFile TO WriteReal(defaultBlock.initialCost, 12, 2);
  ASK saveFile TO WriteString("    ");      
  ASK saveFile TO WriteReal(defaultBlock.operatingCost, 12, 2);
  ASK saveFile TO WriteString("      ");      
  ASK saveFile TO WriteReal(defaultBlock.standbyCost, 12, 2);
  ASK saveFile TO WriteString("  ");      
  ASK saveFile TO WriteReal(defaultBlock.idleCost, 12, 2);
  ASK saveFile TO WriteString("");      
  ASK saveFile TO WriteReal(defaultBlock.repHoldCost, 12, 2);      
  ASK saveFile TO WriteString("       ");      
  ASK saveFile TO WriteReal(defaultBlock.repairingCost, 12, 2);      
  ASK saveFile TO WriteString("  ");      
  ASK saveFile TO WriteReal(defaultBlock.repFixedCost, 12, 2);      
  ASK saveFile TO WriteString("  ");      
  ASK saveFile TO WriteReal(defaultBlock.doneCost, 12, 2);
  ASK saveFile TO WriteString("  ");      
  ASK saveFile TO WriteReal(defaultBlock.doneFixedCost, 12, 2);
  ASK saveFile TO WriteString("");      
  ASK saveFile TO WriteReal(defaultBlock.pmHoldCost, 12, 2);
  ASK saveFile TO WriteString("");      
  ASK saveFile TO WriteReal(defaultBlock.pmCost, 12, 2);
  ASK saveFile TO WriteString("  ");      
  ASK saveFile TO WriteReal(defaultBlock.pmFixedCost, 12, 2);
  ASK saveFile TO WriteString(" ");      
  ASK saveFile TO WriteReal(defaultBlock.spareCost, 12, 2);      
  ASK saveFile TO WriteString("   ");      
  ASK saveFile TO WriteReal(defaultBlock.emerShippingCost, 12, 2);      
  ASK saveFile TO WriteString("      ");      
  IF defaultBlock.alwaysAddDoneCost
     ASK saveFile TO WriteInt(1,1);
  ELSE
     ASK saveFile TO WriteInt(0,1);
  END IF;
  IF failEmp
     ASK saveFile TO WriteLn;
     ASK saveFile TO WriteString("                         ");
     FOR i:=1 TO defaultBlock.numFailParams
        ASK saveFile TO WriteReal(defaultBlock.failVals[i], 16, 6);
        ASK saveFile TO WriteString("  ");  
     END FOR;
     failEmp:=FALSE;
  END IF;
  IF repEmp
     ASK saveFile TO WriteLn;
     ASK saveFile TO WriteString("                         ");
     FOR i:=1 TO defaultBlock.numRepairParams
        ASK saveFile TO WriteReal(defaultBlock.repairVals[i], 16, 6);
        ASK saveFile TO WriteString("  ");  
     END FOR;
     repEmp:=FALSE;
  END IF;
  ASK saveFile TO WriteLn;
  ASK saveFile TO WriteString("EndOfBlockData");
  ASK saveFile TO WriteLn;
  ASK saveFile TO WriteString("/_____________________________________EVENT_DATA______________________________________________________________________________"); 
  ASK saveFile TO WriteLn;   
  ASK saveFile TO WriteString("eventName                 ID        xPos            yPos    ParentId   In     Out  Phasing ");
  ASK saveFile TO WriteString(" Distro    failStream     failVal         InitCost           OpCost          RepCost     ");
  FOR j:=1 TO activePhases
     IF ( j  < 10 )
        ASK saveFile TO WriteString("P 00"+INTTOSTR(j)+"       ");
     ELSIF (j < 100)   
        ASK saveFile TO WriteString("P 0"+INTTOSTR(j)+"       ");
     ELSE
        ASK saveFile TO WriteString("P "+INTTOSTR(j)+"       ");
     END IF;   
  END FOR;
  ASK saveFile TO WriteLn;   
  FOREACH event IN eventGroup
     ASK saveFile TO WriteString(SUBSTR(1,20,event.name + "                    ")+"  ");
     ASK saveFile TO WriteInt(event.Id, 6);
     ASK saveFile TO WriteReal(event.xPosition*1.25, 12, 6);
     ASK saveFile TO WriteString("    ");
     ASK saveFile TO WriteReal(event.yPosition*1.25, 12, 6); 
     ASK saveFile TO WriteString("     ");
     ASK saveFile TO WriteInt(event.parentID, 3);
     ASK saveFile TO WriteString("        ");
     ASK saveFile TO WriteInt(event.connectIntoNum, 1);
     ASK saveFile TO WriteString("      ");
     ASK saveFile TO WriteInt(event.connectOutOfNum, 1);
     ASK saveFile TO WriteString("       ");
     IF event.usesPhasing
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteInt(event.failDistro,6);
     ASK saveFile TO WriteString("     ");
     ASK saveFile TO WriteInt(event.failStream,6);
     ASK saveFile TO WriteString("       ");
     ASK saveFile TO WriteReal(event.failVals[1],12,6);
     ASK saveFile TO WriteString("     ");
     ASK saveFile TO WriteReal(event.initialCost,12,6);
     ASK saveFile TO WriteString("     ");
     ASK saveFile TO WriteReal(event.operatingCost,12,6);
     ASK saveFile TO WriteString("     ");
     ASK saveFile TO WriteReal(event.repairingCost,12,6);
     ASK saveFile TO WriteString("     ");
     IF event.usesPhasing
        IF event.phaseValue=NILARRAY
           ASK event TO SetPhases(TRUE,NILARRAY,NILARRAY);
        END IF;
        FOR j := 1 TO activePhases
           ASK saveFile TO WriteString(event.phaseType[j]+": ");
           ASK saveFile TO WriteReal(event.phaseValue[j],7,3);               
           ASK saveFile TO WriteString("  ");
        END FOR;
     END IF;
     ASK saveFile TO WriteLn;
  END FOREACH;
  ASK saveFile TO WriteString("EndOfEventData");
  ASK saveFile TO WriteLn;   
  ASK saveFile TO WriteString("/_____________________________________NODE_DATA______________________________________________________________________________"); 
  ASK saveFile TO WriteLn;   
  ASK saveFile TO WriteString("Name                     ID          xPos        "+
             "  yPos    parent Type   k     n   Out   Cold   PR  Autos  k*  Phases ");
  ASK saveFile TO WriteString("depNum  depType    divFlow fullflow repCap  repNA ");
  FOR j:=1 TO activePhases
     IF ( j  < 10 )
        ASK saveFile TO WriteString("P00"+INTTOSTR(j)+"        ");
     ELSIF (j < 100)   
        ASK saveFile TO WriteString("P0"+INTTOSTR(j)+"        ");
     ELSE
        ASK saveFile TO WriteString("P"+INTTOSTR(j)+"        ");
     END IF;   
  END FOR;
  ASK saveFile TO WriteLn;
  FOREACH node IN nodeGroup
     ASK saveFile TO WriteString(SUBSTR(1,20,node.name+"                    "));
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteInt(node.Id, 5);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteReal(node.xPosition*1.25, 12, 6);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteReal(node.yPosition*1.25, 12, 6);   
     ASK saveFile TO WriteString("     ");      
     ASK saveFile TO WriteInt(node.parentID, 3);
     ASK saveFile TO WriteString("    ");      
     ASK saveFile TO WriteInt(node.typeNode, 1);
     ASK saveFile TO WriteString("   "); 
     KofNlabel := ASK node Child("RBDNodeKofN", 0);     {this section is here to know that KofN was NOT set}
     IF  ( ((KofNlabel.String="") OR (KofNlabel.String=" ")) AND (node.connectIntoNum>0))
        ASK saveFile TO WriteInt(-1,3);      {"-1" means KofN was not set before saving}
     ELSE
        ASK saveFile TO WriteInt(node.goodPaths, 3);
     END IF;
     ASK saveFile TO WriteString("   ");      
     ASK saveFile TO WriteInt(node.connectIntoNum, 3);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteInt(node.connectOutOfNum, 3);
     ASK saveFile TO WriteString("     ");  
     IF node.coldStandby
        ASK saveFile TO WriteInt(1, 1);
     ELSE
        ASK saveFile TO WriteInt(0, 1);
     END IF;
     ASK saveFile TO WriteString("     ");  
     IF node.priorityReturn
        ASK saveFile TO WriteInt(1, 1);
     ELSE
        ASK saveFile TO WriteInt(0, 1);
     END IF;
     ASK saveFile TO WriteString("     ");  
     IF node.checkAutosFirst
        ASK saveFile TO WriteInt(1, 1);
     ELSE
        ASK saveFile TO WriteInt(0, 1);
     END IF;
     ASK saveFile TO WriteString("  ");  
     ASK saveFile TO WriteInt(node.KStar, 3);  {KStar}
     ASK saveFile TO WriteString("     ");  
     IF node.usesPhasing
        ASK saveFile TO WriteInt(1, 1);
     ELSE
        ASK saveFile TO WriteInt(0, 1);
     END IF;    
     ASK saveFile TO WriteString("  ");  
     ASK saveFile TO WriteInt(node.DependencyNum,5);
     IF node.depType<>""
        ASK saveFile TO WriteString(SUBSTR(1,18,"     "+node.depType+"                   "));
     ELSE
        ASK saveFile TO WriteString(SUBSTR(1,18,"     None               "));
     END IF;   
     IF node.anyPath
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("       ");      
     IF node.fullFlow
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     ASK saveFile TO WriteString("        ");      
     { node.reportCapacity, not used  }
     ASK saveFile TO WriteInt(0,1);
     ASK saveFile TO WriteString("      ");      
     IF node.reportNodeAnal
        ASK saveFile TO WriteInt(1,1);
     ELSE
        ASK saveFile TO WriteInt(0,1);
     END IF;
     IF node.usesPhasing
        IF node.phase=NILARRAY
           ASK node TO SetPhases(TRUE,NILARRAY);
        END IF;
        FOR j := 1 TO activePhases
           IF node.phase[j]=-1
              ASK saveFile TO WriteString("    C       ");
           ELSIF node.phase[j]=0
              ASK saveFile TO WriteString("    L       ");
           ELSE
              ASK saveFile TO WriteInt(node.phase[j], 5);
              ASK saveFile TO WriteString("       ");
           END IF;   
        END FOR;
     END IF;
     ASK saveFile TO WriteLn;
  END FOREACH;
  ASK saveFile TO WriteString("EndOfNodeData");
  ASK saveFile TO WriteLn;
  ASK saveFile TO WriteString("/_______________________COMMENTS____________________________________________________________________________________________"); 
  ASK saveFile TO WriteLn;   
  ASK saveFile TO WriteString("Type      ID  Name                 Comment  "); 
  ASK saveFile TO WriteLn;
  IF deferTrig<>""
     sysComment:="The defer trigger is "+deferTrig;
  END IF;
  IF ((sysComment<>"") AND (sysComment<>"Comment"))
     ASK saveFile TO WriteString("System "); 
     ASK saveFile TO WriteInt(35,5);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteString(SUBSTR(1,20,fileName + "                    ")+" "); 
     tempString:=ReadWriteCR("Write",sysComment);
     ASK saveFile TO WriteString(tempString);
     ASK saveFile TO WriteLn;
  END IF;
  FOREACH hier IN hierGroup   {write comments to .rbd file}             
     IF ((hier.comment<>"") AND (hier.comment<>"Comment"))
        ASK saveFile TO WriteString("hier   "); 
        ASK saveFile TO WriteInt(hier.Id, 5);
        ASK saveFile TO WriteString("  ");      
        ASK saveFile TO WriteString(SUBSTR(1,20,hier.name + "                    ")+" "); 
        tempString:=ReadWriteCR("Write",hier.comment);
        ASK saveFile TO WriteString(tempString);
        ASK saveFile TO WriteLn;
     END IF;   
  END FOREACH;   
  FOREACH block IN blockGroup   {write comments to .rbd file}             
     IF ((block.comment<>"") AND (block.comment<>"Comment"))
        ASK saveFile TO WriteString("block  "); 
        ASK saveFile TO WriteInt(block.Id, 5);
        ASK saveFile TO WriteString("  ");      
        ASK saveFile TO WriteString(SUBSTR(1,20,block.name + "                    ")+" "); 
        tempString:=ReadWriteCR("Write",block.comment);
        ASK saveFile TO WriteString(tempString);
        ASK saveFile TO WriteLn;
     END IF;   
  END FOREACH;  
  FOREACH event IN eventGroup
     IF ((event.comment<>"") AND (event.comment<>"Comment"))
        ASK saveFile TO WriteString("event  ");
        ASK saveFile TO WriteInt(event.Id,5);
        ASK saveFile TO WriteString("  ");
        ASK saveFile TO WriteString(SUBSTR(1,20,event.name + "                   ")+" ");
        tempString:=ReadWriteCR("Write",event.comment);
        ASK saveFile TO WriteString(tempString);
        ASK saveFile TO WriteLn;
     END IF;
  END FOREACH;
  FOREACH node IN nodeGroup   {write comments to .rbd file}             
     IF ((node.comment<>"") AND (node.comment<>"Comment"))
        ASK saveFile TO WriteString("node   "); 
        ASK saveFile TO WriteInt(node.Id, 5);
        ASK saveFile TO WriteString("  ");      
        ASK saveFile TO WriteString(SUBSTR(1,20,node.name + "                    ")+" "); 
        tempString:=ReadWriteCR("Write",node.comment);
        ASK saveFile TO WriteString(tempString);
        ASK saveFile TO WriteLn;
     END IF;   
  END FOREACH;   
  ASK saveFile TO WriteString("/");
  ASK saveFile TO WriteLn;
  ASK saveFile TO WriteString("/_____________________________________LINK_DATA______________________________________________________________________________"); 
  ASK saveFile TO WriteLn;   
  ASK saveFile TO WriteString("   ID  FromType  FromID  ToType  ToID   pID ColdPri      AProb"+
   "        AutoTime           ManTime  CapPri    nomFlow    MaxFlow "); 
  ASK saveFile TO WriteLn;
  FOREACH link IN linkGroup 
     ASK saveFile TO WriteInt(link.Id, 5);  
     IF link.connectFRef = "RBDBlock"
        ASK saveFile TO WriteString("   Block     ");
     ELSIF link.connectFRef = "RBDEvent"
        ASK saveFile TO WriteString("   Event     ");
     ELSIF link.connectFRef = "RBDNode"
        ASK saveFile TO WriteString("   Node      ");
     ELSE
        ASK saveFile TO WriteString("   Hier      ");
     END IF;
     ASK saveFile TO WriteInt(link.connectFromId, 5);
     IF link.connectTRef = "RBDBlock"
        ASK saveFile TO WriteString("  Block  ");
     ELSIF link.connectTRef = "RBDEvent"
        ASK saveFile TO WriteString("  Event  ");
     ELSIF link.connectTRef = "RBDNode"
        ASK saveFile TO WriteString("  Node   ");
     ELSE
        ASK saveFile TO WriteString("  Hier   ");
     END IF;
     ASK saveFile TO WriteInt(link.connectToId,5);
     ASK saveFile TO WriteString("   ");
     ASK saveFile TO WriteInt(link.parentID,2);
     ASK saveFile TO WriteString("    ");
     ASK saveFile TO WriteInt(link.coldPriority,2);
     ASK saveFile TO WriteString("      ");      
     ASK saveFile TO WriteReal(link.autoSwitchProb,8,6);   
     ASK saveFile TO WriteString("");      
     ASK saveFile TO WriteReal(link.autoSwitchTime,16,6);   
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteReal(link.manualSwitchTime,16,6);   
     ASK saveFile TO WriteString("   ");      
     ASK saveFile TO WriteInt(link.capPriority,2);   
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteInt(link.nomFlow,9);
     ASK saveFile TO WriteString("  ");      
     ASK saveFile TO WriteInt(link.maxFlow,9);
     ASK saveFile TO WriteLn;
  END FOREACH;   
  ASK saveFile TO WriteString("EndOfLinkData");
  ASK saveFile TO WriteLn;   
  ASK saveFile TO WriteString("/_______________________________SPARE_POOL_DATA______________________________________________________________________________"); 
  ASK saveFile TO WriteLn;   
  ASK saveFile TO WriteString("PoolName           RSO Stock NewSp    ArriveEvery  ESO       EmerTime SLO"+
    " Level Quan      SLOTime         SpCost   emerShipCost"); 
  ASK saveFile TO WriteLn;
  FOREACH pool IN poolGroup
     IF pool.sparingType = SparePool
        ASK saveFile TO WriteString(SUBSTR(1,20,pool.poolName + "                    ")+" ");
        IF pool.routineSpareOrdering
           ASK saveFile TO WriteInt(1, 1);
        ELSE
           ASK saveFile TO WriteInt(0, 1);
        END IF;
        ASK saveFile TO WriteString("");
        ASK saveFile TO WriteInt(pool.initialSpares, 5);
        ASK saveFile TO WriteString(" ");      
        ASK saveFile TO WriteInt(pool.newSpares, 5);
        ASK saveFile TO WriteString("");      
        ASK saveFile TO WriteReal(pool.newSparesArrival,16,6);
        ASK saveFile TO WriteString("   ");
        IF pool.emerSpareOrdering
           ASK saveFile TO WriteInt(1,1);
        ELSE
           ASK saveFile TO WriteInt(0,1);
        END IF;
        ASK saveFile TO WriteString("");      
        ASK saveFile TO WriteReal(pool.emergencyTime,16,6);      
        ASK saveFile TO WriteString(" ");      
        IF pool.stockLevelOrdering
           ASK saveFile TO WriteInt(1,1);
        ELSE
           ASK saveFile TO WriteInt(0,1);
        END IF;
        ASK saveFile TO WriteString(" ");      
        ASK saveFile TO WriteInt(pool.SLOOrderLevel, 5);
        ASK saveFile TO WriteString(" ");      
        ASK saveFile TO WriteInt(pool.SLONewSpares, 5);
        ASK saveFile TO WriteString("");      
        ASK saveFile TO WriteReal(pool.SLOTime,16,6);
        ASK saveFile TO WriteString(" ");
        ASK saveFile TO WriteReal(pool.spareCost,12,2);
        ASK saveFile TO WriteString("   ");
        ASK saveFile TO WriteReal(pool.emerShippingCost,12,2);
        ASK saveFile TO WriteLn;   
     END IF;
  END FOREACH;
  ASK saveFile TO WriteString("EndOfSparePoolData");
  ASK saveFile TO WriteLn;   
  ASK saveFile TO WriteString("/_________________________________RESOURCE_DATA______________________________________________________________________________");
  ASK saveFile TO WriteLn;
  ASK saveFile TO WriteString("PoolName                 NumIn    SpareCost   FixedPerUse   PerTimeUnit  Phases"); 
  ASK saveFile TO WriteLn;
  FOREACH pool IN poolGroup
     IF pool.sparingType = Resource
        ASK saveFile TO WriteString(SUBSTR(1,20,pool.poolName + "                    ")+"  ");
        ASK saveFile TO WriteInt(pool.initialSpares, 6);
        ASK saveFile TO WriteString("   ");
        ASK saveFile TO WriteReal(pool.spareCost,12,2);
        ASK saveFile TO WriteString("  ");
        ASK saveFile TO WriteReal(pool.fixedPerUse,12,2);
        ASK saveFile TO WriteString("  ");
        ASK saveFile TO WriteReal(pool.costPerTime,12,2);
        ASK saveFile TO WriteString("     ");
        IF pool.usesPhasing
           ASK saveFile TO WriteInt(1, 1);
        ELSE
           ASK saveFile TO WriteInt(0, 1);
        END IF;    
        ASK saveFile TO WriteLn;
     END IF;
  END FOREACH;
  ASK saveFile TO WriteString("EndOfResourceData");
  ASK saveFile TO WriteLn;         
  ASK saveFile TO WriteString("/______________________________PHASES________________________________________________________________________");
  ASK saveFile TO WriteLn;
  ASK saveFile TO WriteString("PhaseName             Num  Mission  Dist           Param1           Param2           Param3");
  ASK saveFile TO WriteLn;
  FOR i:=1 TO activePhases
     phase:=phaseObjArray[i];
     ASK saveFile TO WriteString(SUBSTR(1,20,phase.phaseName + "                    ")+"  ");
     ASK saveFile TO WriteInt(phase.ID, 2);
     ASK saveFile TO WriteString("     "); 
     IF phase.mission=FALSE
       ASK saveFile TO WriteInt(0, 2);
     ELSE
       ASK saveFile TO WriteInt(1,2);
     END IF;   
     ASK saveFile TO WriteString("      ");   
     ASK saveFile TO WriteInt(phase.Dist, 2);
     ASK saveFile TO WriteString("  ");   
     GetNumParams(phase.Dist,temp);    
     FOR j := 1 TO 3
        IF j<=temp
           ASK saveFile TO WriteReal(phase.Params[j], 16, 6);
        ELSE
           ASK saveFile TO WriteReal(0.0,16,6);
        END IF;   
        ASK saveFile TO WriteString(" ");  
     END FOR;  
     ASK saveFile TO WriteLn;    
  END FOR;
  ASK saveFile TO WriteString("EndOfPhasesData");
  ASK saveFile TO WriteLn;         
  ASK saveFile TO WriteString("/______________________________TRIGGERS________________________________________________________________________");
  ASK saveFile TO WriteLn;  
  ASK saveFile TO WriteString("TriggerName         IDnum  Repeats     InitUsed   Dist           Param1           "+
                      "Param2           Param3     ");
  ASK saveFile TO WriteLn;
  FOREACH trig IN triggerGroup
     ASK saveFile TO WriteString(SUBSTR(1,20,trig.TrigName + "                    ")+" ");
     ASK saveFile TO WriteInt(trig.IDnum, 2);
     ASK saveFile TO WriteString("      "); 
     IF trig.Repeats=FALSE
       ASK saveFile TO WriteInt(0,2);
     ELSE
       ASK saveFile TO WriteInt(1,2);
     END IF;   
     ASK saveFile TO WriteString("");   
     ASK saveFile TO WriteReal(trig.InitUsed, 16,6);
     ASK saveFile TO WriteString("    ");         
     ASK saveFile TO WriteInt(trig.TrigDist, 2);
     ASK saveFile TO WriteString("  ");   
     GetNumParams(trig.TrigDist,temp);    
     FOR j := 1 TO 3
        IF j<=temp
           ASK saveFile TO WriteReal(trig.TrigParams[j], 16, 6);
        ELSE
           ASK saveFile TO WriteReal(0.0,16,6);
        END IF;   
        ASK saveFile TO WriteString(" ");  
     END FOR;  
     ASK saveFile TO WriteLn;    
  END FOREACH;
  ASK saveFile TO WriteString("EndOfTriggerData");
  ASK saveFile TO WriteLn;   
  ASK saveFile TO Close;
  DISPOSE(saveFile);
  ASK window TO Draw;
  somethingChanged := FALSE;
  simOptionChanged := FALSE;
  ASK window TO SetSysCursor(NormalCursor);
END PROCEDURE; {SaveFile}

PROCEDURE SaveAsFile(OUT fileName, pathName   : STRING;
                     IN  filter, title        : STRING);
VAR
   fileIsThere            : BOOLEAN;
   dialogBox              : HelpBoxObj;
   dotPosition, extLength : INTEGER;
   extension              : STRING;
   button                 : ButtonObj;
BEGIN
  GetFileName(fileName, pathName, filter, title);
  IF fileName <> "NoFile"
     IF ((studentFile) AND (compileType="release"))      {beta999}
        studentFile:=FALSE;
     END IF;
     dotPosition := POSITION(fileName, ".");
     IF dotPosition = 0
        fileName := fileName + "." + SUBSTR(STRLEN(filter)-2, STRLEN(filter), filter);
     ELSE
        extLength := STRLEN(fileName) - dotPosition;
        IF extLength > 3
           extension := LOWER(SUBSTR((dotPosition + 1), (dotPosition + 3), fileName));
           fileName := SUBSTR(1, dotPosition, fileName) + extension;
        END IF;
     END IF;
     fileIsThere := FileExists(pathName + fileName);
     IF fileIsThere
        NEW(dialogBox);
        ASK dialogBox TO LoadFromLibrary(dialogs, "OverwriteBox");
        ASK window TO AddGraphic(dialogBox);
        ASK dialogBox TO Draw;
        button := ASK dialogBox TO AcceptInput();
        IF ASK button ReferenceName = "OKButton"
           DISPOSE(dialogBox);
        ELSIF ASK button ReferenceName = "NoButton"
           DISPOSE(dialogBox);
           SaveAsFile(fileName, pathName, filter, title);
        END IF;
     END IF;
     IF filter = "*.rbd"
        IF (fileName <> "Unnamed RBD");
           ASK menubar TO AddFileName(pathName, fileName);
        END IF;
        ASK window TO SetTitle(versionStr + " - " + fileName);
        ASK window TO Draw;
     END IF;
  END IF;
END PROCEDURE; {SaveAsFile}

PROCEDURE SavePathsCFG(IN  defPath : STRING);
VAR
   pathStream : StreamObj;
BEGIN
   NEW(pathStream);
   ASK pathStream TO Open((defPath + "paths.cfg"), Output);
   IF pathStream.ioResult <> 0
      NEW(message, 1..2);
      message[1] := "There is a problem opening the paths.cfg file.     ";
      message[2] := "Make sure this file has not been set to read only.     ";
      result := SendAlert(message, FALSE, FALSE, FALSE);
      DISPOSE(message);
      RETURN;
   END IF;
   ASK pathStream TO WriteString("Version6"+" "); 
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteString(menubar.path1);      
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteString(menubar.file1);      
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteString(menubar.path2);      
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteString(menubar.file2);      
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteString(menubar.path3);      
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteString(menubar.file3);      
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteString(menubar.path4);      
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteString(menubar.file4);      
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteString(menubar.path5);      
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteString(menubar.file5);      
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteString(menubar.path6);      
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteString(menubar.file6);      
   ASK pathStream TO WriteLn;
   ASK pathStream TO WriteInt(0,6);
   ASK pathStream TO Close;
   DISPOSE(pathStream);
END PROCEDURE; {SavePathsCFG}

PROCEDURE OpenPathsCFG(IN  defPath,exampPath : STRING);
VAR
   defPath1,defFile1,defPath2,
   defFile2,defPath3,defFile3,
   defPath4,defFile4,defPath5,
   defFile5,defPath6,defFile6,
   nextString                      : STRING;    
   menuitem                        : MenuItemObj;
   pathStream                      : StreamObj;
BEGIN
   NEW(pathStream);
   ASK pathStream TO Open((defPath + "paths.cfg"), Input);
   IF pathStream.ioResult <> 0
      NEW(message, 1..2);
      message[1] := "There is a problem opening the paths.cfg file.     ";
      message[2] := "Make sure this file has not been set to read only.     ";
      result := SendAlert(message, FALSE, FALSE, FALSE);
      DISPOSE(message);
      RETURN;
   END IF;
   ASK pathStream TO ReadString(nextString);
   IF ((nextString="Version4") OR (nextString="Version6"))
      ASK pathStream TO ReadLine(defPath1);
      ASK pathStream TO ReadLine(defPath1);
      IF NOT((SUBSTR(1,2,defPath1) = "A:") OR (SUBSTR(1,2,defPath1) = "a:"))
         pathName := defPath1;
      END IF;
      ASK pathStream TO ReadLine(defFile1);
      ASK pathStream TO ReadLine(defPath2);
      ASK pathStream TO ReadLine(defFile2);
      ASK pathStream TO ReadLine(defPath3);
      ASK pathStream TO ReadLine(defFile3);
      ASK pathStream TO ReadLine(defPath4);
      ASK pathStream TO ReadLine(defFile4);
      ASK pathStream TO ReadLine(defPath5);
      IF defPath5 <> "     0";
         ASK pathStream TO ReadLine(defFile5);
         ASK pathStream TO ReadLine(defPath6);
         ASK pathStream TO ReadLine(defFile6);
      ELSE
         defPath5 := exampPath;
         defFile5 := "CostAoB.rbd";
         defPath6 := exampPath;
         defFile6 := "DepCold.rbd";
      END IF;
     
      {If one of the file names is missing the "." (as in ".rbd"), the default paths will be used -- EAG Error# 72}
      IF((POSITION(defFile1, ".") <= 0) OR (POSITION(defFile2, ".") <= 0) OR (POSITION(defFile3, ".") <= 0) 
         OR (POSITION(defFile4, ".") <= 0) OR (POSITION(defFile5, ".") <= 0) OR (POSITION(defFile6, ".") <= 0));
         IF compileType = "demo" {set to demo files} 
            ASK menubar TO SetFileNames(exampPath, "BasicDemo.rbd", exampPath, "EventsDemo.rbd",
                                        exampPath, "FactoryDemo.rbd", exampPath, "PhasingDemo.rbd",
                                        exampPath, "PreventMxDemo.rbd", exampPath, "MotorDemo.rbd");
         ELSIF compileType = "student" {set to student files}
            ASK menubar TO SetFileNames(exampPath, "Cascade.rbd", exampPath, "Complex.rbd",
                                        exampPath, "ElectricMotor.rbd", exampPath, "Events.rbd",
                                        exampPath, "Hierarchy.rbd", exampPath, "SpaceStation.rbd");
         ELSIF compileType = "gmd" {set to 6.5 gmd files}
            ASK menubar TO SetFileNames(exampPath, "Basic.rbd", exampPath, "Distributions.rbd",
                                        exampPath, "Logistics.rbd", exampPath, "Events.rbd",
                                        exampPath, "Dependence.rbd", exampPath, "Standby.rbd");
         ELSE {set to release files}
            ASK menubar TO SetFileNames(exampPath, "Basic1.rbd", exampPath, "Distributions1.rbd",
                                        exampPath, "Logistics1.rbd", exampPath, "Events1.rbd",
                                        exampPath, "Dependence1.rbd", exampPath, "Standby1.rbd");
         END IF;
      ELSE
         ASK menubar TO SetFileNames(defPath1, defFile1, defPath2, defFile2,
                                     defPath3, defFile3, defPath4, defFile4,
                                     defPath5, defFile5, defPath6, defFile6);
      END IF;
   END IF;                               
   ASK pathStream TO Close;
   DISPOSE(pathStream);      
END PROCEDURE; {OpenPathsCFG}

PROCEDURE OpenDefFile(IN defFileName, defPath : STRING;
                       INOUT ioResult          : BOOLEAN);
VAR
   nextInt,numFparams,numRparams,i,j : INTEGER;
   nextString,version                : STRING;
   nextReal                          : REAL;
   sparing                           : SparingType;
   defaultStream                     : StreamObj;
   fParams,rParams,realsArray,
   preVals, postVals,pmVals          : realArray;
   intsArray                         : intArray;
   strsArray                         : strArray;
   boolsArray                        : boolArray;
BEGIN
   pathName := defPath;
   NEW(defaultStream);
   ASK defaultStream TO Open((defPath + defFileName), Update);
   IF defaultStream.ioResult <> 0
      ioResult := FALSE;
      RETURN;
   ELSE
      ASK defaultStream TO Close;
      ASK defaultStream TO Open((defPath + defFileName),Input); 
      ioResult := TRUE;
   END IF;
   NEW(boolsArray, 1..18);  NEW(intsArray,  1..17);
   NEW(realsArray, 1..28);  NEW(strsArray,  1..6);
   ASK defaultStream TO ReadString(version);
      FOR i := 1 TO 3
         ASK defaultStream TO ReadString(nextString);
         strsArray[i] := nextString;
      END FOR;
      ASK defaultStream TO ReadString(nextString);
      IF nextString = "Infinite"
         sparing := Infinite;
      ELSIF nextString = "SparePool"
         sparing := SparePool;
      ELSIF nextString = "ColdPool"
         sparing := Custom;
      ELSIF nextString = "Custom"
         sparing := Custom;
      ELSE
         sparing := None;
      END IF;      
      ASK defaultStream TO ReadInt(nextInt);  {sysDep}
      IF nextInt = 1
         boolsArray[6] := TRUE;
         intsArray[9]:=-2;
      ELSE
         boolsArray[6] := FALSE;
         intsArray[9]:=0;
      END IF;
      ASK defaultStream TO ReadInt(nextInt);  {infSpares}
      IF nextInt = 1
         boolsArray[1] := TRUE;
      ELSE
         boolsArray[1] := FALSE;
      END IF;
      ASK defaultStream TO ReadInt(nextInt);  {useResourced}
      IF nextInt = 1
         intsArray[15]:=1;
      ELSE
         intsArray[15]:=0;
      END IF;
      ASK defaultStream TO ReadInt(nextInt);  {buttSize - no longer used}
      ASK defaultStream TO ReadInt(nextInt);  {autoSwitch}
      ASK defaultStream TO ReadInt(nextInt);  {manualSwitch}
      ASK defaultStream TO ReadInt(nextInt);  {usesPhasing}
      IF nextInt = 1
         boolsArray[7] := TRUE;
      ELSE
         boolsArray[7] := FALSE;
      END IF;
      ASK defaultStream TO ReadInt(nextInt);  {blockDegrades}
      IF nextInt = 1
         intsArray[10] := 2;
      ELSE
         intsArray[10] := 0;
      END IF;      
      ASK defaultStream TO ReadInt(intsArray[1]);  {fdist}
      ASK defaultStream TO ReadInt(intsArray[2]);  {numfparams}
      ASK defaultStream TO ReadInt(intsArray[11]);  {failStream}
      IF intsArray[11]>6
         intsArray[11]:=intsArray[11]+194;       {system streams are now numbered 201-215}
      END IF;
      ASK defaultStream TO ReadInt(intsArray[3]);  {rdist}
      ASK defaultStream TO ReadInt(intsArray[4]);  {numRparams}
      ASK defaultStream TO ReadInt(intsArray[12]);  {repStream}
      IF intsArray[12]>6
         intsArray[12]:=intsArray[12]+194;       {system streams are now numbered 201-215}
      END IF;      
      ASK defaultStream TO ReadInt(intsArray[5]);  {initStock}
      ASK defaultStream TO ReadInt(intsArray[6]);  {newSpares}
      ASK defaultStream TO ReadInt(intsArray[14]);  {numres1}
      IF intsArray[15]=0
         intsArray[14]:=1;
      END IF;
      ASK defaultStream TO ReadReal(realsArray[1]); {arrivalRate}
      ASK defaultStream TO ReadReal(realsArray[3]); {emerTime}
      ASK defaultStream TO ReadReal(realsArray[4]); {preLdt}
      intsArray[2]:=19;
      NEW(preVals,1..1);
      preVals[1]:= realsArray[4];
      intsArray[4]:=19;
      NEW(postVals,1..1);
      postVals[1]:= 0.0;
      
      ASK defaultStream TO ReadReal(nextReal); {autoSwitchProb}
      ASK defaultStream TO ReadReal(nextReal); {autoSwitchTime}
      ASK defaultStream TO ReadReal(nextReal); {manualSwitchTime}
      ASK defaultStream TO ReadReal(realsArray[17]); {degradeFactor}
      ASK defaultStream TO ReadReal(realsArray[6]); {initialCost}
      ASK defaultStream TO ReadReal(realsArray[7]); {operatingCost}
      ASK defaultStream TO ReadReal(realsArray[8]); {repairCost}
      ASK defaultStream TO ReadReal(realsArray[10]); {spareCost}
      IF (intsArray[6]=0) OR (realsArray[1]<0.00001)      
         boolsArray[2]:=FALSE;
         intsArray[6]:=1;
         realsArray[1]:=720.0;
      ELSE
         boolsArray[2]:=TRUE;
      END IF;
      IF (realsArray[3]>0.00001)
         boolsArray[4]:= TRUE;
      ELSE
         boolsArray[4]:= FALSE;
         realsArray[3]:=24.0;
      END IF;
      boolsArray[3]:=FALSE;   {SLO}
      boolsArray[5]:=FALSE;  {alwaysAddDone}
      intsArray[7]:=0;  {SLOlevel}
      intsArray[8]:=1;  {SLOnewSpares}
      realsArray[2]:=720.;{SLOTime}
      realsArray[5]:=0.;  {postLDT}
      realsArray[9]:=0.;  {repFixedCost}
      realsArray[11]:=0.;  {emerShippingCost}
      realsArray[12]:=0.;  {idleCost}
      realsArray[13]:=0.;  {holdCost}
      realsArray[14]:=0.;  {standbyCost}
      realsArray[15]:=0.;  {doneCost}
      realsArray[16]:=0.;  {doneFixedCost}
      IF realsArray[17]<=1.0
         realsArray[18]:=0.001;  {GDLimit}
      ELSE
         realsArray[18] :=999999999.999;
      END IF;
      realsArray[19]:=0.0;  {amountExhausted}      
      intsArray[13]:=3;    {simStartType}
      NEW(fParams,    1..intsArray[2]);  NEW(rParams,    1..intsArray[4]);
      FOR i := 1 TO intsArray[2]
         ASK defaultStream TO ReadReal(nextReal);
         fParams[i] := nextReal;
      END FOR;
      FOR i := 1 TO intsArray[4]
         ASK defaultStream TO ReadReal(nextReal);
         rParams[i] := nextReal;
      END FOR;
      {Set new default fields for Raptor7}
      realsArray[4]:=factReals[4];      {sbStress}
      boolsArray[11]:=factBools[11];    {usesPM}
      boolsArray[12]:=factBools[12];    {pmSpareNeeded}
      boolsArray[13]:=factBools[13];    {pmRefresh}
      boolsArray[14]:=factBools[14];    {pmMisDefer}
      boolsArray[15]:=factBools[15];    {pmFailReset}
      boolsArray[17]:=factBools[17];    {pmReqDefer}
      realsArray[20]:=factReals[20];    {pmStagger}
      realsArray[21]:=factReals[21];    {pmFreq}
      boolsArray[16]:=factBools[16];    {pmTriggered}
      strsArray[4]  :=factStrs[4];      {pmTrig}
      strsArray[5]  :=factStrs[5];      {comment}
      intsArray[16] :=factInts[16];     {pmDist}
      realsArray[23]:=factReals[23];    {pmHoldCost}
      realsArray[22]:=factReals[22];    {pmCost}
      intsArray[17] :=factInts[17];     {numRes1PM}
      NEW(pmVals,1..1);
      pmVals[1]:= 5.;
      ASK defaultBlock TO SetBlockData(boolsArray, intsArray, realsArray,strsArray, fParams, 
                                    rParams, preVals, postVals,pmVals, sparing);
      DISPOSE(fParams);
      DISPOSE(rParams);
      DISPOSE(preVals);
      DISPOSE(postVals);
      DISPOSE(pmVals);
     ASK defaultStream TO ReadString(nextString);
     IF ((nextString="Nuclear_Power_Plant") OR (nextString="Robot") OR (nextString="Derrick"))
        systemImage:="Equalizer";
     ELSE
        systemImage:=nextString;
     END IF;
      ASK defaultStream TO ReadString(nextString);   {old freezing boolean, now units}
      IF (nextString = "roomTemp") OR (nextString = "frozenSolid") OR (nextString = "oldFrozenBoolean")
         systemUnits := "units";
      ELSE
         systemUnits := nextString;
      END IF;
      ASK defaultStream TO ReadString(nextString);
      IF nextString = "TimeSim"
         termType := 1;
         ASK defaultStream TO ReadReal(dTimeTrunc);
         ASK defaultStream TO ReadReal(dTimeStartTime);
         dFailTrunc:=1.0;
         dCycleTrunc:=10.0;
      ELSIF nextString = "FailSim"
         termType := 2;
         ASK defaultStream TO ReadReal(dFailTrunc);
         ASK defaultStream TO ReadReal(dFailStartTime);
         dTimeTrunc:=1000.0;
         dCycleTrunc:=10.0;
      ELSE
         termType := 3;
         ASK defaultStream TO ReadReal(dCycleTrunc);
         ASK defaultStream TO ReadReal(dCycleStartTime);
         dTimeTrunc:=1000.0;
         dFailTrunc:=1.0;
      END IF;
      ASK defaultStream TO ReadReal(dNumberOfRuns);
      ASK defaultStream TO ReadReal(dTimeSlice);
      ASK defaultStream TO ReadString(nextString);
      IF nextString = "management"
         dSimWithGraph := TRUE;
      ELSE         
         dSimWithGraph := FALSE;
      END IF;
      FOR i:=1 TO 4
         ASK defaultStream TO ReadInt(sysStreams[i]);
      END FOR;
      sysStreams[5] := 70;  sysStreams[6] := 71;
      sysStreams[7] := 72;  sysStreams[8] := 73;
      sysStreams[9] := 74;  sysStreams[10] :=75;   sysStreams[11] :=101;
      ASK defaultStream TO ReadInt(i);           {not used}        
      ASK defaultStream TO ReadReal(cusZoomVal);  {zoom}
      ASK defaultStream TO ReadReal(xOrigin);   {x center}
      ASK defaultStream TO ReadReal(yOrigin);   {y center} 
      ASK defaultStream TO ReadInt(nextInt);   
      IF (nextInt=0)
         negShutUp:=FALSE;
      ELSE
         negShutUp:=TRUE;
      END IF;
      ASK defaultStream TO ReadReal(yMin);
      ASK defaultStream TO ReadReal(yMax); 
      ASK defaultStream TO Close;
      systemRedCost:=0.0;
      sysLostCost:=1.0;
      flowGenerated:=1;
      weakAnalysis:=FALSE;
      costAnalysis:=FALSE;
      capacityAnalysis:=FALSE;
      dZeroFail:=FALSE;
      DISPOSE(defaultStream);
   DISPOSE(boolsArray);
   DISPOSE(intsArray);  
   DISPOSE(realsArray);
   DISPOSE(strsArray);
   DISPOSE(preVals);
   DISPOSE(postVals);
   loadingFile:=TRUE;
   SetView(cusZoomVal,xOrigin,yOrigin); 
   loadingFile:=FALSE;
END PROCEDURE; {OpenDefFile}

PROCEDURE ReadData(IN  dataFile      : STRING;
                   OUT numDataPoints : INTEGER;
                   OUT tempArray     : realArray);
VAR
   i,last,consecZeros : INTEGER;
   tempStrm           : StreamObj;
   endOfFile          : BOOLEAN;
   number             : REAL;
BEGIN
   numDataPoints := 0;
   NEW(tempStrm);
   ASK tempStrm TO Open(dataFile, Input); {open empirical data file}
   IF tempStrm.ioResult <> 0
      NEW(message, 1..2);
      message[1] := "There is a problem opening the empirical data file.     ";
      message[2] := "Make sure this file is not set to read only.     ";
      result := SendAlert(message, FALSE, FALSE, FALSE);
      DISPOSE(message);
      RETURN;
   END IF;
   REPEAT
      ASK tempStrm TO ReadReal(number); {Read number from data file}
      INC(i);
      IF number <> 0.
         last := i;
         consecZeros := 0;
      ELSE
         INC(consecZeros);
      END IF;
      IF tempStrm.eof OR (consecZeros = 999)
         endOfFile := TRUE;
         numDataPoints := last;
      END IF;
   UNTIL endOfFile; 
   IF numDataPoints > 0
      NEW(tempArray, 1..numDataPoints);
      ASK tempStrm TO Position(0);
      FOR i := 1 TO numDataPoints 
         ASK tempStrm TO ReadReal(number); {Read number from data file}
         IF number < 0.
            NEW(message, 1..1);
            message[1] := "Negative number not allowed. Data Point "+INTTOSTR(i)+" being entered as zero!     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
            tempArray[i] := 0.;     {store number in temporary array}
         ELSIF number > 999999999.999999
            NEW(message, 1..1);
            message[1] := "Data point "+INTTOSTR(i)+" being reduced to the max number of 999,999,999.999999!     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
            tempArray[i] := 999999999.999999;     {store number in temporary array}
         ELSE
            tempArray[i] := number;     {store number in temporary array}
         END IF;
      END FOR;
   ELSE
      NEW(message, 1..2);
      message[1] := "There is no valid data in the selected file.     ";
      message[2] := "Make sure this file contains text numbers.     ";
      result := SendAlert(message, FALSE, FALSE, FALSE);
      DISPOSE(message);
      RETURN;
   END IF;
   ASK tempStrm TO Close;
   DISPOSE(tempStrm);            {close stream object and dispose of it}
END PROCEDURE; {ReadData}

PROCEDURE NewFFile(IN gridIsOn                                                     : BOOLEAN;
                   INOUT fileIsOpen                                                : BOOLEAN;
                   INOUT fileName, pathName, filter                                : STRING;
                   INOUT totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents  : INTEGER;
                   OUT   nixed                                                     : BOOLEAN);
VAR
   saveCancelled : BOOLEAN;
   pool          : SparePoolObj;
   trig          : RapTriggerObj;

BEGIN
   CloseFFile(gridIsOn, fileIsOpen, saveCancelled, fileName, pathName, filter, totalBlocks,
              totalNodes, totalLinks,totalHiers,totalEvents);
   IF NOT saveCancelled
      fileName := "Unnamed RBD";
      ASK window TO SetTitle(versionStr + " - " + fileName);
      IF gridIsOn
         ASK grid TO Colorize("Normal");
      ELSE
         ASK grid TO Colorize("Hide");
      END IF;
      InitFactorySettings;  {to reset default values}
      cusZoomVal := 24.;
      fileIsOpen := TRUE;
      somethingChanged := FALSE;
      simOptionChanged := FALSE;
      FOREACH pool IN poolGroup
         ASK pool TO RemovePool(pool.poolName, pool.sparingType);
         ASK poolGroup TO RemoveThis(pool);
      END FOREACH;
      FOREACH trig IN triggerGroup
         ASK trig TO RemoveTrig(trig.TrigName);
         ASK triggerGroup TO RemoveThis(trig);
      END FOREACH;
      startId      := 99999;
      endId        := 99999;
      totalBlocks  := 0;
      totalNodes   := 0;
      totalLinks   := 0;
      totalSpares  := 0;
      totalCold    := 0;
      totalRes     := 0;
      activePhases := 0;
   END IF;
   nixed := saveCancelled;
END PROCEDURE; {NewFFile}

PROCEDURE CloseFFile(INOUT gridIsOn, fileIsOpen, saveCancelled                       : BOOLEAN;
                     INOUT fileName, pathName, filter                                : STRING;
                     INOUT totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents  : INTEGER);
VAR
  i                      : INTEGER;
  block                  : RBDBlockObj;
  event                  : RBDEventObj;
  node                   : RBDNodeObj;
  hier,tempHier          : RBDHierObj;
  link                   : LinkObj;
  pool                   : SparePoolObj;
  trig                   : RapTriggerObj;
  phase                  : PhaseObj;
  menuitem               : MenuItemObj;
  child                  : ANYOBJ;
BEGIN
  IF fileIsOpen
     IF (somethingChanged OR simOptionChanged) AND (totalObjects > 2) AND (compileType <> "demo")
        AskForSave(saveCancelled,fileIsOpen,fileName,pathName,filter,totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents);
     END IF;
     IF NOT saveCancelled
        {IF (fileName <> "Unnamed RBD") AND (NOT openFromIcon);
           ASK menubar TO AddFileName(pathName, fileName);
        END IF;  }                                               {cmc 10/13/08}
        ASK window TO SetSysCursor(BusyCursor);
        ASK window TO SetDeferral(TRUE);
        FOREACH hier IN hierGroup
           FOREACH child IN hier.childGroup
              ASK hier.childGroup TO RemoveThis(child);
           END FOREACH;
           ASK hierGroup TO RemoveThis(hier);
           IF (hier.parentID > 0)
              tempHier := ASK root Child("RBDHier", hier.parentID);
              IF (tempHier <> NILOBJ) 
                 IF (tempHier.childGroup.Includes(hier));
                    ASK tempHier.childGroup TO RemoveThis(hier);
                 END IF;
              END IF;
           END IF;
           DISPOSE(hier);
        END FOREACH;        
        FOREACH block IN blockGroup
           ASK blockGroup TO RemoveThis(block);
           DISPOSE(block);
        END FOREACH;
        FOREACH event IN eventGroup
           ASK eventGroup TO RemoveThis(event);
           DISPOSE(event);
        END FOREACH;   
        FOREACH node IN nodeGroup
           ASK nodeGroup TO RemoveThis(node);
           DISPOSE(node);
        END FOREACH;
        FOREACH link IN linkGroup
           ASK linkGroup TO RemoveThis(link);
           DISPOSE(link);
        END FOREACH;
        FOREACH pool IN poolGroup
           ASK pool TO RemovePool(pool.poolName, pool.sparingType);
           ASK poolGroup TO RemoveThis(pool);
        END FOREACH;   
        FOREACH trig IN triggerGroup
           ASK trig TO RemoveTrig(trig.TrigName);
           ASK triggerGroup TO RemoveThis(trig);
        END FOREACH;
        FOR i:=1 TO activePhases
           DISPOSE(phaseObjArray[i]);
        END FOR;
        DISPOSE(phaseObjArray);
        totalBlocks  := 0;
        totalEvents  := 0; 
        totalNodes   := 0;
        totalLinks   := 0;
        totalHiers   := 0;
        totalSpares  := 0;
        totalCold    := 0;
        totalRes     := 0;
        activePhases := 0;
        hierLevel    := 0;
        activeWindow := 0;
        sysComment   := ""; {beta160}
        IF NOT openMenuFile
           ASK window TO SetTitle(versionStr);
           IF NOT gridIsOn
              ASK grid TO Colorize("Normal");
           END IF;
        END IF;
        ASK window TO SetDeferral(FALSE);
        ASK window TO Draw;
        fileIsOpen := FALSE;
        somethingChanged := FALSE;
        simOptionChanged := FALSE;
        IF simulated
           simulated := FALSE;
           DISPOSE(FinalArray);
        END IF;
        ASK window TO SetSysCursor(NormalCursor);
        IF ProtectedFile
           ClearPasteBuffer;
        END IF;
        ProtectedFile:=FALSE;
     ELSE
        somethingChanged := TRUE;
        simOptionChanged := TRUE;
     END IF;
  ELSE
     somethingChanged := FALSE;
     simOptionChanged := FALSE;
  END IF;
  nodesAnalysed:=FALSE;
END PROCEDURE; {CloseFFile}

PROCEDURE AskForSave(INOUT saveCancelled, fileIsOpen                             : BOOLEAN;
                     INOUT fileName, pathName, filter                            : STRING;
                     IN totalBlocks,totalNodes,totalLinks,totalHiers,totalEvents : INTEGER);
VAR
   dialogBox     : HelpBoxObj;
   button        : ButtonObj;
BEGIN
   NEW(dialogBox);
   ASK dialogBox TO LoadFromLibrary(dialogs, "CloseFileBox");
   ASK window TO AddGraphic(dialogBox);
   ASK dialogBox TO Draw;
   button := ASK dialogBox TO AcceptInput();
   dontClose := FALSE;
   IF ASK button ReferenceName = "OKButton"
      DISPOSE(dialogBox);
      IF (fileName = "Unnamed RBD") OR (fileName = "NoFile")
         SaveAsFile(fileName, pathName, filter, "Save RBD File");
      END IF;
      IF fileName <> "NoFile"
         SaveFile(fileName, pathName, filter, totalBlocks, totalNodes, totalLinks,totalHiers,totalEvents);
      END IF;
   ELSIF ASK button ReferenceName = "CancelButton"
      DISPOSE(dialogBox);
      saveCancelled := TRUE;
   ELSE
      DISPOSE(dialogBox);
   END IF;
   IF dontClose
      saveCancelled := TRUE;
   END IF;
   IF fileName = "NoFile"
      saveCancelled := TRUE;
      fileName := "Unnamed RBD";
   END IF;
END PROCEDURE; {AskForSave}

PROCEDURE GetFileName(OUT FileName, PathName : STRING;
                      IN  filter, titleText  : STRING);
VAR
   p, stringLength        : INTEGER;
   OKSelected             : BOOLEAN;
   fileDialog             : FileDialogBoxObj;
   configStream           : StreamObj;
   lookInDir              : STRING;
BEGIN
   IF filter="*.rbd"
      lookInDir:=rbdPath;
   ELSIF  filter="*.bmp"
      lookInDir:=rbdPath;
   ELSIF  filter="*.txt"
      lookInDir:=reportPath;
   ELSIF filter = "*.rbl"
      lookInDir := libraryPath;
   ELSIF filter = "*.rbz"
      lookInDir := libraryPath";
      filter := SUBSTR(1, (STRLEN(nameOfFile)-1), nameOfFile)+"l";
   END IF;   
   NEW(fileDialog);
   ASK window TO AddGraphic(fileDialog);
   ASK fileDialog TO SetLabel(titleText);
   ASK fileDialog TO SetFilter(filter);
   IF lookInDir = ""
      ASK fileDialog TO SetDirectory("");
   ELSE
      ASK fileDialog TO SetDirectory(lookInDir);
   END IF;
   OKSelected := ASK fileDialog TO AcceptSysInput;
   FileName := fileDialog.File;
   PathName := fileDialog.Path;
   IF ((OKSelected = FALSE) OR (FileName = "") OR (FileName = "*"))
      FileName := "NoFile";
     IF compileType = "release"
        pathName := LOWER(GetProgDir("Raptor7.exe"));
     ELSIF compileType = "demo"
        pathName := LOWER(GetProgDir("Raptor7Demo.exe"));
     ELSIF compileType = "student"
        pathName := LOWER(GetProgDir("Raptor7Student.exe"));
     ELSIF compileType = "gmd"
        pathName := LOWER(GetProgDir("Raptor65.exe"));
     END IF;
   ELSE
      IF filter="*.rbd"
         rbdPath:=PathName;
      ELSIF  filter="*.txt"
         reportPath:=PathName;
      ELSIF  filter="*.bmp"
         rbdPath:=PathName;
      ELSIF filter = "*.rbl"
         libraryPath:=PathName;
      ELSIF filter = "*.rbz"
         libraryPath:=PathName;
       END IF;   
   END IF;
   DISPOSE(fileDialog);
END PROCEDURE; {GetFileName}


PROCEDURE UpdateVersion (IN blocksEvents,totalNodes  : INTEGER);
{Adds attributes that are lacking in both Raptor6 and Raptor5 rbds that 
require updating}
VAR
    i             : INTEGER;
    block         : RBDBlockObj;
    pmVars        : realArray;
    node          : RBDNodeObj;
BEGIN
    FOREACH block IN blockGroup
        ASK block TO SetSBstress(0.);
        ASK block TO SetUsesPM(FALSE);
        ASK block TO SetpmSpare(TRUE);
        ASK block TO SetpmRefresh(TRUE);
        ASK block TO SetpmMisDefer(FALSE);
        ASK block TO SetpmFailReset(TRUE);
        ASK block TO SetpmReqDefer(TRUE);
        ASK block TO SetpmDist(19);
        NEW(pmVars,1..1);
        pmVars[1]:=1.0;
        ASK block TO SetpmParams(pmVars);
        DISPOSE(pmVars);
        ASK block TO SetnumRes1PM(1);
        ASK block TO SetpmFreq(100.0);
        ASK block TO SetpmTriggered(FALSE);
        ASK block TO SetpmTrig("None");
        ASK block TO SetpmStagger(0.0);
        ASK block TO SetpmCost(1.0);
        ASK block TO SetpmHoldCost(1.0);
        ASK block TO SetpmFixedCost(1.0);
        ASK block TO SetComment("");
        ASK block TO SetDepVals(0.0,100.0,0.0,0.0,75.0,TRUE);
    END FOREACH; 
    FOR i:=1 TO totalNodes
       node := ASK root Child("RBDNode", (blocksEvents+i));
       ASK node TO SetKStar(node.goodPaths);
    END FOR;  
    deepestLevel:=0;
END PROCEDURE; {UpdateVersion}


PROCEDURE AutoAddStartEnd (IN start,end  : BOOLEAN);
   
VAR
  i                      : INTEGER;
  intsArray              : intArray;
  xMin,xMax,yMin,yMax    : REAL; 
  block                  : RBDBlockObj;
  node                   : RBDNodeObj;
  event                  : RBDEventObj;
BEGIN
  {first find x,y position of existing objects}
  xMin:=9999.99;
  xMax:=0.0;
  yMin:=9999.99;
  yMax:=0.0;
  FOREACH block IN blockGroup      {beta154}
     IF (block.xPosition < xMin)
        xMin:=block.xPosition;
     END IF;   
     IF (block.xPosition > xMax)
        xMax:=block.xPosition;
     END IF;
     IF (block.yPosition < yMin)
        yMin:=block.yPosition;
     END IF;   
     IF (block.yPosition > yMax)
        yMax:=block.yPosition;
     END IF;   
  END FOREACH;
  FOREACH event IN eventGroup
     IF (event.xPosition < xMin)
        xMin:=event.xPosition;
     END IF;   
     IF (event.xPosition > xMax)
        xMax:=event.xPosition;
     END IF;
     IF (event.yPosition < yMin)
        yMin:=event.yPosition;
     END IF;   
     IF (event.yPosition > yMax)
        yMax:=event.yPosition;
     END IF;   
  END FOREACH;
  FOREACH node IN nodeGroup
     IF (node.xPosition < xMin)
        xMin:=node.xPosition;
     END IF;   
     IF (node.xPosition > xMax)
        xMax:=node.xPosition;
     END IF;
     IF (node.yPosition < yMin)
        yMin:=node.yPosition;
     END IF;   
     IF (node.yPosition > yMax)
        yMax:=node.yPosition;
     END IF;   
  END FOREACH;
  
  NEW(intsArray,1..6);  
  FOR i:=1 TO 6
     intsArray[i]:=0;
  END FOR;   
  IF (NOT start)
     INC(totalNodes);
     NEW(node);
     ASK nodeGroup TO Add(node);
     ASK node TO LoadFromLibrary (images, "RBDStartNode");
     ASK node TO SetName("start");
     ASK node TO SetID("RBDNode", nextId);
     ASK root TO AddAfterGraphic((ASK root Child("RBDBlock",0)),node);
     IF xMin>2.0
        ASK node TO DisplayAt((xMin-2.0), (yMin+yMax)/2.0);
     ELSE
        ASK node TO DisplayAt(0.0, (yMin+yMax)/2.0);
     END IF;
     ASK node TO SetSnapShot(FALSE);
     ASK node TO SetTranslation(node.Translation.x, node.Translation.y); 
     ASK node TO Draw; 
     ASK node TO SetType(1);
     ASK node TO SetNum(-1);
     startId := node.Id;
     ASK node TO Init1(intsArray);
     ASK node TO SetAnyPath(FALSE);
     ASK node TO SetFullFlow(FALSE);
     ASK node TO SetReportNodeAnal(FALSE);
  END IF;
  IF (NOT end)
     INC(totalNodes);
     NEW(node);
     ASK nodeGroup TO Add(node);
     ASK node TO LoadFromLibrary (images, "RBDEndNode");
     ASK node TO SetName("end");
     ASK node TO SetID("RBDNode", nextId);
     ASK root TO AddAfterGraphic((ASK root Child("RBDBlock",0)),node);
     IF xMax<117.1
        ASK node TO DisplayAt((xMax+2.0), ((yMin+yMax)/2.0));
     ELSE
        ASK node TO DisplayAt(119., (yMin+yMax)/2.0);
     END IF;
     ASK node TO SetSnapShot(FALSE);
     ASK node TO SetTranslation(node.Translation.x, node.Translation.y); 
     ASK node TO Draw; 
     ASK node TO SetType(3);
     ASK node TO SetNum(-2);
     endId := node.Id;
     ASK node TO Init1(intsArray);
     ASK node TO SetAnyPath(FALSE);
     ASK node TO SetFullFlow(FALSE);
     ASK node TO SetReportNodeAnal(FALSE);
  END IF;
  DISPOSE(intsArray);
END PROCEDURE;  {AutoAddStartEnd}

PROCEDURE ReadWriteCR   (IN action,inString : STRING);                           
VAR
  i,asciiChar            : INTEGER;
  nextChar               : CHAR;
BEGIN
  FOR i:=1 TO STRLEN(inString)
     nextChar:=SCHAR(inString,i);
     asciiChar:=ORD(nextChar);
     IF ((asciiChar=126) AND (action="Read"))
        REPLACE(inString,i,i,CHR(10));
     END IF;
     IF ((asciiChar=10) AND (action="Write"))
        REPLACE(inString,i,i,CHR(126));
     END IF;
  END FOR;
  RETURN(inString);
END PROCEDURE;  {ReadWriteCR}

PROCEDURE ErrorOpeningFile (INOUT totBs,totNs,totLs,totHs,totEs,existBs,existNs,existLs,existHs,
                                existEs,existTs,numResPools                                      : INTEGER;
                          INOUT saveFile                                                         : StreamObj;      
                          INOUT fileIsOpen                                                       : BOOLEAN;
                          IN    saveCancelled,append                                             : BOOLEAN;
                          OUT   nixed                                                            : BOOLEAN);
   
BEGIN
  result := SendAlert(message, FALSE, FALSE, TRUE);
  DISPOSE(message);
  ASK saveFile TO Close;
  DISPOSE(saveFile); 
  IF (append)
     totBs:=existBs;      {reset totals}
     totEs:=existEs;      
     totNs:=existNs;
     totLs:=existLs;
     totalTriggers:=existTs;
     totHs:=existHs;
  ELSE    {NOT append}
     ASK window TO SetTitle(versionStr+" - Unnamed RBD");
     ASK menubar TO Enable(6);          
     ASK window TO Draw;
     fileIsOpen:=FALSE;
     somethingChanged := FALSE;
     simOptionChanged := FALSE;         
     activePhases:=0;
     totBs:=0;
     totEs:=0;
     totNs:=0;
     totHs:=0;
     totLs:=0;         
     totalPools:=0;
     numResPools:=0;
     ResetNewFile;       {tony10-04: to fix beta error 999}
     nixed := saveCancelled;   
  END IF;   
  ASK window TO SetSysCursor(NormalCursor);
  rapVersion := 7;
END PROCEDURE;                     
                     
PROCEDURE CleanArrays   (INOUT posArray,realsArray     : realArray;
                         INOUT idsArray,intsArray      : intArray;
                         INOUT boolsArray              : boolArray;
                         INOUT stringArray             : strArray);
BEGIN
  DISPOSE(posArray);
  DISPOSE(realsArray);
  DISPOSE(idsArray);
  DISPOSE(intsArray);
  DISPOSE(boolsArray);
  DISPOSE(stringArray);               
END PROCEDURE;
                         
END MODULE. {imod file_fx}


