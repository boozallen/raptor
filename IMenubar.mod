{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Implementation Module : Menubar                                               +}
{+  Author        : Steve Brown                                                   +}
{+  Modified By   : Elizabeth Grimes                                              +}
{+  Last Modified : 8/18/08   wds/TES                                             +}
{+  Description   : This module controls all menubars in RAPTOR - RBD workspace,  +}
{+                  simulation, and graph.   Handles all actions from selections. +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

IMPLEMENTATION MODULE Menubar;

FROM SimMod   IMPORT Timescale;
FROM Check    IMPORT CheckBoxObj;
FROM Button   IMPORT ButtonObj;         
FROM Radio    IMPORT RadioBoxObj, RadioButtonObj;
FROM Intract  IMPORT AboutBoxObj, SystemInputBoxObj, BlockInputBoxObj, NodeInputBoxObj, EventInputBoxObj, 
                     HierInputBoxObj, TablesOutObj, SystemBoxObj, PrefsBoxObj, MassEditBoxObj, 
                     PrintTablesBoxObj, BlockPropObj, PhaseBoxObj, SendAlert, CallHelp, HelpBoxObj,
                     SparePoolsBoxObj, ResPoolsBoxObj, threeWay, loadCanceled, PrintWinObj,
                     TrigBoxObj, TreeBoxObj, FailEffectsBoxObj, GotoHierBoxObj, FailRepairBoxObj, GetNumParams;
FROM Value    IMPORT ValueBoxObj;
FROM Menu     IMPORT MenuItemObj, MenuObj;
FROM Objects  IMPORT realArray, RBDNodeObj, RBDBlockObj, RBDEventObj, RBDHierObj, PhaseObj;
FROM GPalet   IMPORT PaletteButtonObj;
FROM Graphic  IMPORT GraphicVObj;
FROM Chart    IMPORT ChartObj, ChartAxisFieldType(ChartYMax,ChartYMin,ChartYIntercept);
FROM Display  IMPORT window, root, grid, dialogs, images, systemImage, ClearAllBlocks,
                     SetView, soundPath, gridIsOn, fileIsOpen, nameOfFile, pathName, filter, 
                     totalBlocks, totalEvents, totalNodes, totalLinks, xOrigin, yOrigin, sound, startStep,
                     defaultBlock, linkCursor, selectGroup, soundIsOn, sysStreams, soundsPath,
                     AoGraph, IntAoGraph, MultiRunGraph, AoGraphWindow, DisplayAoGraph, simZoomX,
                     simZoomY, yMin, yMax, simToolBar, timeSlice, capacityAnalysis, block, event, node, hier,
                     weakAnalysis, costAnalysis, saveInc, saveIsOn, lastSave, exampPath, AoVisible, 
                     statusBarOn, flowGenerated, cusZoomVal, ZoomPercent,
                     sysLostCost, systemRedCost, menuTool, fevToolBar, menubar, simMenuBar, weakMenuBar, fevMenuBar,
                     SelectBlock, SelectEvent, SelectNode, dSimWithGraph, ShowAnalView, analUp, weakToolBar,
                     analViewAvail, configFrozen, hitXinSim, {changedZoom,} compileType, pastingMultiple,
                     ValidateRBD, StartFailureEffects, EndFailureEffects, greenFace, yellowFace, redFace, 
                     faceVisible, activePhases, blockGUIColor, symbolScale, symbolOffset, graphYOff 
                     ,totalHiers, hierLevel, deepestLevel, activeWindow, ChangeWindow, HandleLinks, phaseObjArray, currentView,
                     CollapseIntoHier, ZoomFit, {installPath,} password, dontChangeXY, ignoreMouse {eag error 51 fix};
FROM FileFx   IMPORT menuPath, menuFile, NewFFile, SaveFile, CloseFFile, SaveAsFile, OpenFile,
                     SavePathsCFG, rapVersion, GetFileName;
FROM GTypes   IMPORT ALL ColorType, TextBufferType, ALL SysCursorType, ALL GraphPartType,
                     TextFontType(SystemFont,RomanFont,CourierFont,SystemText), OptionListType;
FROM OSMod    IMPORT SystemCall, FileExists, GetProgDir, Delay, GetOSType;
FROM GPrint   IMPORT PrinterObj, ALL PrintOrientationType;
FROM Image    IMPORT ImageObj;
FROM Text     IMPORT TextObj;
FROM UtilMod  IMPORT DateTime, ClockRealSecs;
FROM IOMod    IMPORT StreamObj,FileUseType(Output,Input);  
FROM Runsim   IMPORT AvailGraph, System, FillStatusBar, phaseNumber, RefreshAllFEV, RefreshPhase, 
                     CreateFMECAarray, WriteFMECAtoFile, simInProgress, WriteSPFfile, exploded;
FROM Print    IMPORT ExportRBD, PrintSelected, PrintTree;
FROM GProcs   IMPORT HandleEvents;
FROM Button   IMPORT ButtonObj;
FROM TextBox  IMPORT ComboBoxObj;
FROM Menu     IMPORT MenuObj, MenuItemObj;
FROM Control  IMPORT ControlVObj;
FROM Form     IMPORT DialogBoxObj;

FROM Display  IMPORT userPath;  { wds/TES, 8/18/08 }

VAR
   i, j, k                                   : INTEGER;
   message                                   : TextBufferType;
   stepped, notSimming                       : BOOLEAN; 
   button                                    : ButtonObj;
   oldSpeed                                  : REAL;
   menuItem, pauseItem, stepItem, jumpItem, stopItem,
   returnItem, resimItem, viewItem, printItem,
   slowItem, fastItem, statusItem, colorItem,
   smallItem, medItem, largeItem, grandeItem,
   homeItem, upItem, gotoItem                : MenuItemObj;
   statusIndItem                             : ARRAY INTEGER OF MenuItemObj;
   buttItem, pauseBut, stepBut, jumpBut, stopBut,
   returnBut, resimBut, viewBut, printBut,
   slowBut, fastBut, colorBut                : PaletteButtonObj;
   homeBut, upBut, gotoBut                   : PaletteButtonObj;
   checkedWindow                             : MenuItemObj;



PROCEDURE ReadLib (IN libPath  : STRING; 
                   IN append   : BOOLEAN;
                   OUT success : BOOLEAN);
VAR
   tempStrm                                           : StreamObj;
   endOfFile, blankLines, goodName, firstMatch        : BOOLEAN;
   name, theRest, origName                            : STRING;
   last, consecBlanks, nextPos, inInt, tempPos,
   numLibBlocks,first,position,matches,j,libSize      : INTEGER;
   tempName                                           : OptionListType;
   tempLib                                            : ARRAY INTEGER, INTEGER OF STRING;
BEGIN
   first := 1;
   NEW(tempStrm);
   ASK tempStrm TO Open(libPath, Input); 
   IF tempStrm.ioResult <> 0
      success := FALSE;
      NEW(message, 1..2);
      message[1] := "There is a problem opening the block library file.     ";
      message[2] := "Make sure this file is not set to read only.     ";
      result := SendAlert(message, FALSE, FALSE, FALSE);
      DISPOSE(message);
      RETURN;
   END IF;
   ASK tempStrm TO ReadLine(theRest);
   IF SUBSTR(1,9,theRest)="Raptor6.0"      {tony}
      libVersion:=6;
      libSize:=16;
   ELSIF SUBSTR(1,9,theRest)="Raptor7.0"
      libVersion:=7;
      libSize:=22;
   ELSE
      NEW(message, 1..2);
      message[1] := "There is a problem opening the block library file.     ";
      message[2] := "It does not appear to be properly formatted.     ";
      result := SendAlert(message, FALSE, FALSE, FALSE);
      DISPOSE(message);
      RETURN;
   END IF;   
   ASK tempStrm TO ReadLine(theRest);
   REPEAT
      ASK tempStrm TO ReadString(name);
      ASK tempStrm TO ReadLine(theRest);
      IF name <> ""
         INC(last);
         consecBlanks := 0;
      ELSE
         INC(consecBlanks);
      END IF;
      IF tempStrm.eof OR (consecBlanks = 499)
         endOfFile := TRUE;
         numLibBlocks := last;
      END IF;
   UNTIL endOfFile; 
   IF numLibBlocks > 0
      success := TRUE;
      IF append
         first := HIGH(libArray)+1;
         last := first+last-1;
         NEW(tempName, 1..HIGH(nameArray));
         NEW(tempLib, 1..HIGH(nameArray), 1..libSize);
         FOR i := 1 TO HIGH(nameArray)
            tempName[i] := nameArray[i];
            FOR j := 1 TO libSize
               tempLib[i,j] := libArray[i,j];
            END FOR;
         END FOR;
         DISPOSE(nameArray);
         DISPOSE(libArray);
         NEW(nameArray, 1..last);
         NEW(libArray, 1..last, 1..libSize);
         FOR i := 1 TO HIGH(tempName)
            nameArray[i] := tempName[i];
            FOR j := 1 TO libSize
               libArray[i,j] := tempLib[i,j];
            END FOR;
         END FOR;
      ELSE
         IF libArray <> NILARRAY
            DISPOSE(nameArray);
            DISPOSE(libArray);
         END IF;
         NEW(nameArray, 1..last);
         NEW(libArray, 1..last, 1..libSize);
      END IF;
      ASK tempStrm TO Position(0);
      ASK tempStrm TO ReadLine(theRest);
      ASK tempStrm TO ReadLine(theRest);
      nextPos := ASK tempStrm TO GetPosition;
      FOR i := first TO last 
         ASK tempStrm TO Position(nextPos);
         ASK tempStrm TO ReadString(name);
         position := ASK tempStrm TO GetPosition;
         ASK tempStrm TO ReadLine(theRest);
         nextPos := ASK tempStrm TO GetPosition;
         ASK tempStrm TO ReadString(theRest);
         ASK tempStrm TO Position(position);
         IF name = ""
            blankLines := TRUE;
         ELSE
            firstMatch := TRUE;
            REPEAT
               goodName := TRUE;
               FOR j := 1 TO i
                  IF name = nameArray[j]
                     IF firstMatch
                        firstMatch := FALSE;
                        origName := name;
                        matches := 1;
                     END IF;
                     INC(matches);
                     name := origName+"-"+INTTOSTR(matches);
                     goodName := FALSE;
                  END IF;
               END FOR;
            UNTIL goodName;
            libArray[i,1] := name; 
            nameArray[i] := name;
            tempPos := ASK tempStrm TO GetPosition;
            IF tempPos < nextPos
               ASK tempStrm TO ReadInt(inInt);            {FailDistro}
               {eag start}
               IF ((inInt = 16) OR (inInt < 1) OR (inInt > 22))
                  success := FALSE;
                  NEW(message, 1..3);
                  message[1] := "There is an invalid distribution in the library     ";
                  message[2] := "file.  Contact raptortech@arinc.com or call     ";
                  message[3] := "505.248.0718 for further assistance.     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
                  DISPOSE(libArray);
                  DISPOSE(nameArray);
                  RETURN;
               END IF;
               {eag end}
               IF (inInt >=1) AND (inInt <= 22)
                  libArray[i,2] := INTTOSTR(inInt);
                  ASK tempStrm TO ReadString(libArray[i,3]);  {FVal 1}
                  ASK tempStrm TO ReadString(libArray[i,4]);  {FVal 2}
                  ASK tempStrm TO ReadString(libArray[i,5]);  {FVal 3}
                  IF libVersion=6
                     ASK tempStrm TO ReadString(theRest);  {FVal 4}
                  END IF;   
               ELSE
                  libArray[i,2] := "4";
                  libArray[i,3] := "100";  {FVal 1}
               END IF;
            ELSE
               libArray[i,2] := "4";
               libArray[i,3] := "100";  {FVal 1}
            END IF;
            tempPos := ASK tempStrm TO GetPosition;
            IF tempPos < nextPos
               ASK tempStrm TO ReadInt(inInt);            {RepDistro}
               {eag start}
               IF ((inInt = 16) OR (inInt < 1) OR (inInt > 22))
                  success := FALSE;
                  NEW(message, 1..3);
                  message[1] := "There is an invalid distribution in the library     ";
                  message[2] := "file.  Contact raptortech@arinc.com or call     ";
                  message[3] := "505.248.0718 for further assistance.     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
                  DISPOSE(libArray);
                  DISPOSE(nameArray);
                  RETURN;
               END IF;
               {eag end}
               IF ((inInt >=1) AND (inInt <= 22)) 
                  libArray[i,7] := INTTOSTR(inInt);
                  ASK tempStrm TO ReadString(libArray[i,8]);  {RVal 1}
                  ASK tempStrm TO ReadString(libArray[i,9]);  {RVal 2}
                  ASK tempStrm TO ReadString(libArray[i,10]); {RVal 3}
                  IF libVersion=6
                     ASK tempStrm TO ReadString(theRest); {RVal 4}
                  END IF;   
               ELSE
                  libArray[i,7] := "7";
                  libArray[i,8] := "10";  {RVal 1}
                  libArray[i,9] := "2";   {RVal 2}
               END IF;
            ELSE
               libArray[i,7] := "7";
               libArray[i,8] := "10";  {RVal 1}
               libArray[i,9] := "2";   {RVal 2}
            END IF;            
            tempPos := ASK tempStrm TO GetPosition;
            IF tempPos < nextPos
               ASK tempStrm TO ReadString(libArray[i,12]); {Pre LDT dist}
            ELSE
               libArray[i,12] := "0";
            END IF;
            tempPos := ASK tempStrm TO GetPosition;
            IF tempPos < nextPos
               ASK tempStrm TO ReadString(libArray[i,13]); {Post LDT dist}
            ELSE
               libArray[i,13] := "0";
            END IF;
            tempPos := ASK tempStrm TO GetPosition;
            IF tempPos < nextPos
               ASK tempStrm TO ReadString(theRest);     {Dependency}
               IF LOWER(theRest) = "yes"
                  libArray[i,14] := "TRUE";
               ELSE
                  libArray[i,14] := "FALSE";
               END IF;
            ELSE
               libArray[i,14] := "FALSE";
            END IF;
            IF libVersion=7            
               tempPos := ASK tempStrm TO GetPosition;
               IF tempPos < nextPos
                  ASK tempStrm TO ReadString(libArray[i,15]);  {PreVal 1}
                  ASK tempStrm TO ReadString(libArray[i,16]);  {PreVal 2}
                  ASK tempStrm TO ReadString(libArray[i,17]);  {PreVal 3}
               ELSE
                  {tony}
               END IF;            
               tempPos := ASK tempStrm TO GetPosition;
               IF tempPos < nextPos
                  ASK tempStrm TO ReadString(libArray[i,18]);  {PostVal 1}
                  ASK tempStrm TO ReadString(libArray[i,19]);  {PostVal 2}
                  ASK tempStrm TO ReadString(libArray[i,20]);  {PostVal 3}
               ELSE
                  {tony}
               END IF;            
            END IF;
         END IF;
      END FOR;
      FOR i := 1 TO HIGH(nameArray)     {error checking for fail/repair dist inputs}
         FOR j := 1 TO 2
            k := 2 + ((j-1)*4);         {index for fail/repair dist in libArray}
            CASE libArray[i,k]
               WHEN "1":     {Beta}
                  IF (STRTOREAL(libArray[i,k+1]) < 0.000001) OR (STRTOREAL(libArray[i,k+1]) > 999999999.999999)
                     libArray[i,k+1] := "1";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+2]) < 0.000001) OR (STRTOREAL(libArray[i,k+2]) > 999999999.999999)
                     libArray[i,k+2] := "1";
                  END IF;
               WHEN "2":      {Chi-square}
                  IF FLOAT(TRUNC(STRTOREAL(libArray[i,k+1]))) <> STRTOREAL(libArray[i,k+1])
                     libArray[i,k+1] := INTTOSTR(TRUNC(STRTOREAL(libArray[i,k+1])));
                  END IF;
                  IF STRTOREAL(libArray[i,k+1]) > 999999999.
                     libArray[i,k+1] := "999999999";
                  END IF;
               WHEN "3":      {Binomial}
                  IF FLOAT(TRUNC(STRTOREAL(libArray[i,k+1]))) <> STRTOREAL(libArray[i,k+1])
                     libArray[i,k+1] := INTTOSTR(TRUNC(STRTOREAL(libArray[i,k+1])));
                  END IF;
                  IF STRTOREAL(libArray[i,k+1]) > 999999999.
                     libArray[i,k+1] := "999999999";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+2]) < 0.000001) OR (STRTOREAL(libArray[i,k+2]) > 1.0)
                     libArray[i,k+2] := "0.8";
                  END IF;
               WHEN "4":      {Exponential}
                  IF (STRTOREAL(libArray[i,k+1]) < 0.000001) OR (STRTOREAL(libArray[i,k+1]) > 999999999.999999)
                     libArray[i,k+1] := "100";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+2]) < 0.0) OR (STRTOREAL(libArray[i,k+2]) > 999999999.999999)
                     libArray[i,k+2] := "0";
                  END IF;
               WHEN "5":      {Erlang}
                  IF FLOAT(TRUNC(STRTOREAL(libArray[i,k+1]))) <> STRTOREAL(libArray[i,k+1])
                     libArray[i,k+1] := INTTOSTR(TRUNC(STRTOREAL(libArray[i,k+1])));
                  END IF;
                  IF STRTOREAL(libArray[i,k+1]) > 999999999.
                     libArray[i,k+1] := "999999999";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+2]) < 0.000001) OR (STRTOREAL(libArray[i,k+2]) > 999999999.999999)
                     libArray[i,k+2] := "1";
                  END IF;     {Gamma, Pearson 5, Weibull}
               WHEN "6", "11", "15":
                  IF (STRTOREAL(libArray[i,k+1]) < 0.000001) OR (STRTOREAL(libArray[i,k+1]) > 999999999.999999)
                     libArray[i,k+1] := "1";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+2]) < 0.000001) OR (STRTOREAL(libArray[i,k+2]) > 999999999.999999)
                     libArray[i,k+2] := "1";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+3]) < 0.0) OR (STRTOREAL(libArray[i,k+3]) > 999999999.999999)
                     libArray[i,k+3] := "0";
                  END IF;
               WHEN "7":       {Lognormal}
                  IF (STRTOREAL(libArray[i,k+1]) < 0.000001) OR (STRTOREAL(libArray[i,k+1]) > 999999999.999999)
                     libArray[i,k+1] := "10";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+2]) < 0.000001) OR (STRTOREAL(libArray[i,k+2]) > 999999999.999999)
                     libArray[i,k+2] := "2";
                  END IF;
               WHEN "8":
                  IF (STRTOREAL(libArray[i,k+1]) < 0.0) OR (STRTOREAL(libArray[i,k+1]) > 999999999.999999)
                     libArray[i,k+1] := "1";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+2]) < 0.000001) OR (STRTOREAL(libArray[i,k+2]) > 999999999.999999)
                     libArray[i,k+2] := "1";
                  END IF;
               WHEN "9":       {Uniform Integer}
                  IF FLOAT(TRUNC(STRTOREAL(libArray[i,k+1]))) <> STRTOREAL(libArray[i,k+1])
                     libArray[i,k+1] := INTTOSTR(TRUNC(STRTOREAL(libArray[i,k+1])));
                  END IF;
                  IF STRTOREAL(libArray[i,k+1]) < 0.0
                     libArray[i,k+1] := "0";
                  END IF;
                  IF FLOAT(TRUNC(STRTOREAL(libArray[i,k+2]))) <> STRTOREAL(libArray[i,k+2])
                     libArray[i,k+2] := INTTOSTR(TRUNC(STRTOREAL(libArray[i,k+2])));
                  END IF;
                  IF (STRTOREAL(libArray[i,k+2]) <= 0.0) OR (STRTOREAL(libArray[i,k+2]) > 999999999.)
                     libArray[i,k+1] := "1";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+1])) > (STRTOREAL(libArray[i,k+2]))
                     libArray[i,k+1] := libArray[i,k+2];
                  END IF;
               WHEN "10":      {Uniform Real}
                  IF STRTOREAL(libArray[i,k+1]) < 0.0
                     libArray[i,k+1] := "0";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+2]) <= 0.0) OR (STRTOREAL(libArray[i,k+2]) > 999999999.999999)
                     libArray[i,k+2] := "1";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+1])) > (STRTOREAL(libArray[i,k+2]))
                     libArray[i,k+1] := libArray[i,k+2];
                  END IF;
               WHEN "12":      {Pearson 6}
                  IF (STRTOREAL(libArray[i,k+1]) < 0.000001) OR (STRTOREAL(libArray[i,k+1]) > 999999999.999999)
                     libArray[i,k+1] := "1";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+2]) < 0.000001) OR (STRTOREAL(libArray[i,k+2]) > 999999999.999999)
                     libArray[i,k+2] := "1";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+3]) < 0.000001) OR (STRTOREAL(libArray[i,k+3]) > 999999999.999999)
                     libArray[i,k+3] := "1";
                  END IF;
                  {IF (STRTOREAL(libArray[i,k+4]) < 0.0) OR (STRTOREAL(libArray[i,k+4]) > 999999999.999999)
                     libArray[i,k+4] := "0";
                  END IF;}
               WHEN "13":      {Poisson}
                  IF (STRTOREAL(libArray[i,k+1]) < 0.000001) OR (STRTOREAL(libArray[i,k+1]) > 999999999.999999)
                     libArray[i,k+1] := "1";
                  END IF;
               WHEN "14":      {Triangular}
                  IF STRTOREAL(libArray[i,k+1]) < 0.0
                     libArray[i,k+1] := "0";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+3]) <= 0.0) OR (STRTOREAL(libArray[i,k+3]) > 999999999.999999)
                     libArray[i,k+3] := "999999999";
                  END IF;
                  IF (STRTOREAL(libArray[i,k+1])) >= (STRTOREAL(libArray[i,k+2]))
                     libArray[i,k+1] := REALTOSTR((STRTOREAL(libArray[i,k+2])/2.));
                  END IF;
                  IF (STRTOREAL(libArray[i,k+2])) >= (STRTOREAL(libArray[i,k+3]))
                     libArray[i,k+2] := REALTOSTR((STRTOREAL(libArray[i,k+3])/2.));
                     IF (STRTOREAL(libArray[i,k+1])) >= (STRTOREAL(libArray[i,k+2]))
                        libArray[i,k+1] := REALTOSTR((STRTOREAL(libArray[i,k+2])/2.));
                     END IF;
                  END IF;
               WHEN "18":     {None}
               OTHERWISE
            END CASE;
         END FOR;
         IF libVersion=6
            IF (STRTOREAL(libArray[i,12]) < 0.00000) OR (STRTOREAL(libArray[i,12]) > 999999999.999999)
               libArray[i,12] := "0";
            END IF;
            IF (STRTOREAL(libArray[i,13]) < 0.00000) OR (STRTOREAL(libArray[i,13]) > 999999999.999999)
               libArray[i,13] := "0";
            END IF;
         ELSIF libVersion=7
            IF ( (STRTOINT(libArray[i,12])<1) OR (STRTOINT(libArray[i,12])>22) ) 
               libArray[i,12] := "0";
 {           ELSE 
               libArray[i,12]:=STRTOINT(libArray[i,12]); }   {tony}
            END IF;      
            IF (STRTOINT(libArray[i,13]) < 1) OR (STRTOINT(libArray[i,13]) > 22)
               libArray[i,13] := "0";
 {           ELSE
               libArray[i,13]:=STRTOINT(libArray[i,13]);   }
            END IF;
         END IF;   
      END FOR;                  
      IF blankLines
         NEW(message, 1..2);
         message[1] := "There were lines in the file which began with     ";
         message[2] := "blank characters.  Those lines were not used.     ";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      END IF;
   ELSE
      success := TRUE;
      NEW(message, 1..2);
      message[1] := "There are no valid blocks in the selected file.     ";
      message[2] := "Make sure this file contains text numbers.     ";
      result := SendAlert(message, FALSE, FALSE, FALSE);
      DISPOSE(message);
      RETURN;
   END IF;
   ASK tempStrm TO Close;
   DISPOSE(tempStrm); 
   DISPOSE(tempName);
   DISPOSE(tempLib);
END PROCEDURE; {ReadLib}

PROCEDURE FEVOpenFile;

VAR
   filter, nameOfFile, pathName    : STRING;
   message                         : TextBufferType; 
  
BEGIN
   filter := "*.txt";   
   NEW(message,1..2);
   NEW(FEVStream);
   SaveAsFile(nameOfFile, pathName, filter, "FEV Data File");
   IF nameOfFile = "NoFile"
      DISPOSE(FEVStream);
   ELSE
      ASK FEVStream TO Open(pathName + nameOfFile, Output);     
      IF FEVStream.ioResult <> 0
         message[1]:="The file "+nameOfFile+" could not be opened!     "; 
         message[2]:="It is already open, is set to read-only, or is corrupted!     "; 
         result:=SendAlert(message,FALSE, FALSE, TRUE);
      ELSE
         buttItem := ASK fevToolBar Descendant("OpenFile",903);
         ASK buttItem TO Deactivate;
         buttItem := ASK fevToolBar Descendant("EditFile",904);
         ASK buttItem TO Activate;
         buttItem := ASK fevToolBar Descendant("CloseFile", 905);
         ASK buttItem TO Activate;
         menuItem := ASK fevMenuBar Descendant("OpenItem",101);
         ASK menuItem TO SetLabel("&Close Interactive Log");
         menuItem := ASK fevMenuBar Descendant("WriteItem",102);
         ASK menuItem TO Activate;
      END IF;
   END IF;
   DISPOSE(message);   
END PROCEDURE; {FEVOpenFile}
 
  
PROCEDURE FEVCloseFile;

BEGIN
   ASK FEVStream TO Close;     
   DISPOSE(FEVStream); {need to dispose of FEVStream as soon as Close for Method ShowRightClickMenu in IDisplay}
   buttItem := ASK fevToolBar Descendant("OpenFile",903);
   ASK buttItem TO Activate;
   buttItem := ASK fevToolBar Descendant("EditFile",904);
   ASK buttItem TO Deactivate;
   buttItem := ASK fevToolBar Descendant("CloseFile", 905);
   ASK buttItem TO Deactivate;
   menuItem := ASK fevMenuBar Descendant("OpenItem",101);
   ASK menuItem TO SetLabel("&Open Interactive Log...");
   menuItem := ASK fevMenuBar Descendant("WriteItem",102);
   ASK menuItem TO Deactivate;
END PROCEDURE; {FEVCloseFile}


PROCEDURE FEVIndicateStatus;

BEGIN
   statusItem := ASK fevMenuBar Descendant("StatusItem", 302);
   IF statusItem.Label() = "Hide Status Indicator"
      faceBoxOpen := FALSE;
      ASK statusItem TO SetLabel("Show Status Indicator");
      ASK greenFace  TO Erase;
      ASK yellowFace TO Erase;
      ASK redFace    TO Erase;
   ELSE
      faceBoxOpen := TRUE;
      ASK statusItem  TO SetLabel("Hide Status Indicator");
      ASK faceVisible TO Draw;
   END IF;
END PROCEDURE; {FEVIndicateStatus}

PROCEDURE DisplayFailureEffects;

VAR
   failEffectsBox                 : FailEffectsBoxObj;
   
BEGIN
   typeOfCursor := dialogC;            
   NEW(failEffectsBox);
   ASK failEffectsBox TO LoadFromLibrary(dialogs, "FailureEffectsBox");
   ASK window TO AddGraphic(failEffectsBox);
   ASK failEffectsBox TO ReceiveData;
   typeOfCursor := fevC;
   DISPOSE(failEffectsBox);
         
END PROCEDURE; {DisplayFailureEffects}
 
 
PROCEDURE SaveTables;
VAR
   printTablesBox                                        : PrintTablesBoxObj;
BEGIN
   typeOfCursor := dialogC;
   NEW(printTablesBox);
   ASK printTablesBox TO LoadFromLibrary(dialogs, "PrintTablesBox");
   ASK window TO AddGraphic(printTablesBox);
   ASK printTablesBox TO SetLabel("Save .TXT Options"); 
   ASK printTablesBox TO Draw;
   ASK printTablesBox TO GetPreferences(TRUE);
   DISPOSE(printTablesBox);
   typeOfCursor := nilC;
END PROCEDURE;

PROCEDURE FindItem;
VAR
   itemList                            : OptionListType;
   tempString                          : STRING;
   tempBlock                           : RBDBlockObj;
   tempEvent                           : RBDEventObj;
   locateBox                           : HelpBoxObj;
   tempNode                            : RBDNodeObj;
   tempHier                            : RBDHierObj;
   itemCombo                           : ComboBoxObj;
   zoomer, id, i                       : INTEGER;
   tempX, tempY, localZoom             : REAL;
      
BEGIN 
   i := 1;
   {Make itemList 2 elements shorter for each hier cuz won't show in/out nodes in list}
   NEW(itemList, 1..(totalObjects-(totalHiers*2)));
   FOREACH tempBlock IN blockGroup
      id := tempBlock.Id;
      tempString := INTTOSTR(id);
      IF id < 10
         tempString := "00" + tempString;
      ELSIF id < 100
         tempString := "0" + tempString;
      END IF;
      itemList[i] := tempBlock.name+" Block - " + tempString;
      INC(i);
   END FOREACH;    
   FOREACH tempEvent IN eventGroup
      id := tempEvent.Id;
      tempString := INTTOSTR(id);
      IF id < 10
         tempString := "00" + tempString;
      ELSIF id < 100
         tempString := "0" + tempString;
      END IF;
      itemList[i] := tempEvent.name+" Event - " + tempString;
      INC(i);
   END FOREACH;    
   FOREACH tempNode IN nodeGroup
      id := tempNode.Id;
      tempString := INTTOSTR(id);
      IF id < 10
         tempString := "00" + tempString;
      ELSIF id < 100
         tempString := "0" + tempString;
      END IF;
      IF ((tempNode.typeNode <> 4) AND (tempNode.typeNode <> 5))
         itemList[i] := tempNode.name+" Node - " + tempString;
         INC(i);
      END IF;
   END FOREACH;    
   FOREACH tempHier IN hierGroup
      id := tempHier.Id;
      tempString := INTTOSTR(id);
      IF id < 10
         tempString := "00" + tempString;
      ELSIF id < 100
         tempString := "0" + tempString;
      END IF;
      itemList[i] := tempHier.name+" Hier - " + tempString;
      INC(i);
   END FOREACH;    
      
   ClearAllBlocks;
   typeOfCursor := dialogC;            
   NEW(locateBox);
   ASK locateBox TO LoadFromLibrary(dialogs, "LocateBox");
   ASK window TO AddGraphic(locateBox);
   itemCombo := ASK locateBox Child("LocateCombo", 100);
   ASK itemCombo TO SetOptions(itemList);
   ASK itemCombo TO ReceiveFocus;
   button := ASK locateBox TO AcceptInput();
   IF button.ReferenceName = "OKButton"
      i := STRLEN(itemCombo.Text);
      REPEAT
         DEC(i);
         tempString := SUBSTR(i,i,itemCombo.Text);
      UNTIL tempString = "-";
      zoomer := STRTOINT(SUBSTR(i+1,STRLEN(itemCombo.Text),itemCombo.Text));
      block := ASK root Child("RBDBlock", zoomer);
      node := ASK root Child("RBDNode", zoomer);
      hier := ASK root Child("RBDHier", zoomer);
      event := ASK root Child("RBDEvent", zoomer);
      IF block <> NILOBJ
         tempX :=  block.xPosition;
         tempY :=  block.yPosition;
         IF (block.parentID <> activeWindow)
            IF block.parentID = 0
               ChangeWindow(0, 0);
            ELSE
               tempHier := ASK root Child("RBDHier", block.parentID);
               ChangeWindow(tempHier.Id, tempHier.level);
            END IF;
         END IF;
         SelectBlock;
         HandleLinks(TRUE);
      ELSIF event <> NILOBJ
         tempX :=  event.xPosition;
         tempY :=  event.yPosition;
         IF (event.parentID <> activeWindow)
            IF event.parentID = 0
               ChangeWindow(0, 0);
            ELSE
               tempHier := ASK root Child("RBDHier", event.parentID);
               ChangeWindow(tempHier.Id, tempHier.level);
            END IF;
         END IF;
         SelectEvent;
         HandleLinks(TRUE);
      ELSIF node <> NILOBJ
         tempX :=  node.xPosition;
         tempY :=  node.yPosition;
         IF (node.parentID <> activeWindow)
            IF node.parentID = 0
               ChangeWindow(0, 0);
            ELSE
               tempHier := ASK root Child("RBDHier", node.parentID);
               ChangeWindow(tempHier.Id, tempHier.level);
            END IF;
         END IF;
         SelectNode;
         HandleLinks(TRUE);
      ELSIF hier <> NILOBJ;
         tempX :=  hier.xPosition;
         tempY :=  hier.yPosition;
         IF (hier.parentID <> activeWindow)
            IF hier.parentID = 0
               ChangeWindow(0, 0);
            ELSE
               tempHier := ASK root Child("RBDHier", hier.parentID);
               ChangeWindow(tempHier.Id, tempHier.level);
            END IF;
         END IF;
         SelectHier;
         HandleLinks(TRUE);
      END IF;
      IF currentView = "workspace"  
         IF activeWindow <> 0
            tempHier := ASK root Child("RBDHier", activeWindow);
            localZoom := tempHier.zoom;
         ELSE
            localZoom := cusZoomVal;
         END IF;
         dontChangeXY := TRUE;
         SetView(localZoom, (tempX-localZoom/2.), (tempY+localZoom*13.2/40.));
         dontChangeXY := FALSE;
         ASK menubar TO Enable(1);
         ASK menubar TO Enable2Thru6;
         ASK menubar TO Enable(7);
         ASK menubar TO Enable(8);
         ASK menubar TO Enable(10);
      END IF;
   END IF;
   IF currentView = "workspace"
      typeOfCursor := nilC;
   {ELSIF currentView = "simulation"
      typeOfCursor := simC;
   ELSIF currentView = "fev"
      typeOfCursor := fevC;
   ELSIF currentView = "weaklink"
      typeOfCursor := weakC;}
   END IF;
   DISPOSE(locateBox);
   DISPOSE(itemList); 
END PROCEDURE;

PROCEDURE PrintSetup;
VAR 
   opSys : STRING;
   pID   : INTEGER;
BEGIN
   opSys := GetOSType();
   IF GetProgDir("control.exe") <> ""
      IF POSITION(opSys, "UNICODE") > 0
         IF FileExists("c:\winnt\system32\control.exe")
            pID := SystemCall("c:\winnt\system32\control.exe printers",0);
         ELSIF ((POSITION(GetProgDir("control.exe"),"win") > 0)
                 OR (POSITION(GetProgDir("control.exe"),"WIN") > 0))
            pID := SystemCall(GetProgDir("control.exe")+"control.exe printers",0);
         ELSE
            NEW(message, 1..2);
            message[1] := "Control.exe found but not in a windows directory. If the control panel    ";
            message[2] := "doesn't come up, the printer will have to be set outside of RAPTOR.     ";
            result := SendAlert(message, FALSE, FALSE, FALSE);
            DISPOSE(message);
            pID := SystemCall(GetProgDir("control.exe")+"control.exe printers",0);
         END IF;
      ELSIF POSITION(opSys, "WINDOWS") > 0
         IF FileExists("c:\windows\control.exe")
            pID := SystemCall("c:\windows\control.exe printers",0);
         ELSIF POSITION(GetProgDir("control.exe"),"win") > 0
            pID := SystemCall(GetProgDir("control.exe")+"control.exe  printers",0);
         ELSE
            NEW(message, 1..2);
            message[1] := "Control.exe found but not in a windows directory. If the control panel    ";
            message[2] := "doesn't come up, the printer will have to be set outside of RAPTOR.     ";
            result := SendAlert(message, FALSE, FALSE, FALSE);
            DISPOSE(message);
            pID := SystemCall(GetProgDir("control.exe")+"control.exe printers",0);
         END IF;
      ELSE
         NEW(message, 1..1);
         message[1] := "Unable to identify the operating system!  Control panel not found.     ";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      END IF;
   ELSE
      NEW(message, 1..1);
      message[1] := "Unable to locate the control.exe file on this hard drive!     ";
      result := SendAlert(message, FALSE, FALSE, TRUE);
      DISPOSE(message);
   END IF;
END PROCEDURE;

PROCEDURE PrintWindow;
VAR
   tempBlock                       : RBDBlockObj;
   tempEvent                       : RBDEventObj;
   tempNode                        : RBDNodeObj;
   tempHier                        : RBDHierObj;
   printWinBox                     : PrintWinObj;
   numPages                            : ValueBoxObj;
   dirRadBox                           : RadioBoxObj;
   landButton, porButton               : RadioButtonObj;
   printPages : INTEGER;
   dir : STRING;

BEGIN
   NEW(printWinBox);
   ASK printWinBox TO LoadFromLibrary(dialogs, "PrintWinBox");
   ASK window TO AddGraphic(printWinBox);
   ASK printWinBox TO Draw;
   numPages := ASK printWinBox Descendant("NumPages", 102);
   dirRadBox := ASK printWinBox Descendant("DirRadBox", 104);
   landButton := ASK printWinBox Descendant("LandButton", 1041);
   porButton := ASK printWinBox Descendant("PorButton", 1042);
   ASK dirRadBox TO SetSelectedButton(landButton);
   ASK dirRadBox TO Draw;
   button := ASK printWinBox TO AcceptInput();
   IF button.ReferenceName = "OKButton"
      printPages := ROUND(numPages.Value());
      IF printPages < 1 
         printPages := 1;
      END IF;
      IF printPages > 20 
         printPages := 20;
      END IF;
      IF dirRadBox.SelectedButton = landButton
         dir := "Landscape";
      ELSE
         dir := "Portrait";
      END IF;
      ExportRBD(TRUE, printPages, activeWindow, dir);
   END IF;
END PROCEDURE;

PROCEDURE ReturnToWorkspace;
VAR
   tempBlock                           : RBDBlockObj;
   tempEvent                           : RBDEventObj;
   tempNode                            : RBDNodeObj;
   tempHier                            : RBDHierObj;
   nodeImage, blockImage, eventImage, hierImage, dsi, innerSquare   : ImageObj;
   aoText                              : TextObj;

BEGIN
   IF weakAnalysis OR analUp
      FOREACH tempNode IN nodeGroup
         nodeImage := ASK tempNode Child("Node", 602);
         aoText := ASK tempNode Child("NodeAoText",0);
         IF tempNode.typeNode = 2
            ASK aoText TO SetText("");
            ASK aoText TO SetHidden(TRUE);
            ASK nodeImage TO SetColor(blockGUIColor);
            ASK aoText TO Draw;
            ASK nodeImage TO Draw;
         ELSIF tempNode.typeNode = 3   
            ASK aoText TO SetText("");
            ASK aoText TO SetHidden(TRUE);
            ASK nodeImage TO SetColor(Black);
            ASK aoText TO Draw;
            ASK nodeImage TO Draw;
         END IF;
      END FOREACH;
      FOREACH tempBlock IN blockGroup
         blockImage := ASK tempBlock Child("BasicBlock", 601);
         aoText := ASK tempBlock Child("BlockAoText",0);
         innerSquare := ASK tempBlock Child("InnerSquare",0);
         ASK aoText TO SetText("");
         ASK aoText TO SetHidden(TRUE);
         ASK innerSquare TO SetColor(blockGUIColor);
         ASK innerSquare TO SetHidden(TRUE);
         ASK blockImage TO SetColor(blockGUIColor);
         ASK aoText TO Draw;
         ASK blockImage TO Draw;
      END FOREACH;
      FOREACH tempEvent IN eventGroup
         eventImage := ASK tempEvent Child("BasicBlock", 601);
         aoText := ASK tempEvent Child("EventAoText",0);
         innerSquare := ASK tempEvent Child("InnerSquare",0);
         ASK aoText TO SetText("");
         ASK aoText TO SetHidden(TRUE);
         ASK innerSquare TO SetColor(blockGUIColor);
         ASK innerSquare TO SetHidden(TRUE);
         ASK eventImage TO SetColor(blockGUIColor);
         ASK aoText TO Draw;
         ASK eventImage TO Draw;
      END FOREACH;
      FOREACH tempHier IN hierGroup
         hierImage := ASK tempHier Child("Hier", 603);
         ASK hierImage TO SetColor(blockGUIColor);
         aoText := ASK tempHier Child("HierAoText",0);
         ASK aoText TO SetText("");
         ASK aoText TO SetHidden(TRUE);
         dsi:=ASK tempHier Descendant("HierMid",0);
         ASK dsi TO SetColor(Black);
         ASK aoText TO Draw;
         ASK hierImage TO Draw;
         ASK dsi TO Draw;
      END FOREACH;
      AoVisible := FALSE;
      IF analUp AND notSimming
         ShowAnalView(FALSE);
         notSimming := FALSE;
      END IF;
   END IF;
   analUp := FALSE;
   returnToRBD := TRUE; 
   currentView := "workspace";
   ClearAllBlocks;
END PROCEDURE;

PROCEDURE ViewOutputs;
VAR
   tabOutBox                           : TablesOutObj;
BEGIN
   NEW(tabOutBox);
   ASK tabOutBox TO LoadFromLibrary(dialogs, "ViewOutputBox");
   ASK window TO AddGraphic(tabOutBox);
   ASK tabOutBox TO InitSelf(FALSE);
   button := ASK tabOutBox TO AcceptInput();
   DISPOSE(tabOutBox); 
END PROCEDURE;

PROCEDURE ColorPrefs;
VAR                                     
   cancelled        : BOOLEAN;
   sysBox                              : SystemBoxObj;
   tempBlock                           : RBDBlockObj;
   tempEvent                           : RBDEventObj;
   tempNode                            : RBDNodeObj;
   tempHier                            : RBDHierObj;
   nodeImage, blockImage               : ImageObj;
   aoText                              : TextObj;
BEGIN
   NEW(sysBox);
   ASK sysBox TO LoadFromLibrary(dialogs, "SystemBox");
   ASK window  TO AddGraphic(sysBox);
   ASK sysBox TO Draw;
   ASK sysBox TO GetColors(cancelled);
   DISPOSE(sysBox);
   IF NOT cancelled
      FOREACH tempNode IN nodeGroup
         ASK tempNode TO ShowAnalColor;
      END FOREACH;
      FOREACH tempBlock IN blockGroup
         ASK tempBlock TO ShowAnalColor;
      END FOREACH;
      FOREACH tempEvent IN eventGroup
         ASK tempEvent TO ShowAnalColor;
      END FOREACH;
   END IF;
END PROCEDURE   
   
PROCEDURE UpOneLevel;
VAR
   tempHier                    : RBDHierObj;
   newId : INTEGER;
BEGIN
   ClearAllBlocks
   tempHier := ASK root Descendant("RBDHier", activeWindow);
   newId := tempHier.parentID;
   IF newId > 0
      tempHier := ASK root Descendant("RBDHier", newId);
      ChangeWindow(newId, hierLevel-1);
   ELSE
      ChangeWindow(0, 0);
   END IF;
END PROCEDURE;

PROCEDURE Home;
BEGIN
   ClearAllBlocks;
   ChangeWindow(0, 0);
END PROCEDURE;

PROCEDURE GotoHier;
VAR
   gotoHierBox : GotoHierBoxObj;
BEGIN
   ClearAllBlocks;
   NEW(gotoHierBox);
   ASK gotoHierBox TO LoadFromLibrary(dialogs, "GotoHierBox");
   ASK window TO AddGraphic(gotoHierBox);
   ASK gotoHierBox TO ReceiveData;
END PROCEDURE;

PROCEDURE Step;
BEGIN
   goToNext := TRUE;
   stepped := TRUE;
   inStepMode := TRUE;
   inJumpMode := FALSE;
   ASK simMenuBar TO ChangeState("Stepped");
END PROCEDURE;

PROCEDURE Jump;
BEGIN
   goToNext := TRUE;
   stepped  := TRUE;
   inStepMode := FALSE;
   inJumpMode := TRUE;
END PROCEDURE;

PROCEDURE Zoom(IN dir : STRING);
VAR
   zoomOutButton, zoomInButton       : PaletteButtonObj;
   zoomOutItem, zoomInItem           : MenuItemObj
   tempHier : RBDHierObj;
   tempZoom, tempXOrig, tempYOrig : REAL;
BEGIN                                      
   IF currentView = "workspace"
      zoomOutButton := ASK menuTool Descendant("ZoomOutButton", 925);
      zoomInButton  := ASK menuTool Descendant("ZoomInButton", 926);
      zoomOutItem   := ASK menubar Descendant("ZoomOutItem", 405);
      zoomInItem    := ASK menubar Descendant("ZoomInItem", 404);
   ELSIF currentView = "simulation"
      zoomOutButton := ASK simToolBar Descendant("ZoomOutButton", 915);
      zoomInButton  := ASK simToolBar Descendant("ZoomInButton", 916);
      zoomOutItem   := ASK simMenuBar Descendant("ZoomOutItem", 405);
      zoomInItem    := ASK simMenuBar Descendant("ZoomInItem", 404);
   ELSIF currentView = "weaklink"
      zoomOutButton := ASK weakToolBar Descendant("ZoomOutButton", 908);
      zoomInButton  := ASK weakToolBar Descendant("ZoomInButton", 909);
      zoomOutItem   := ASK weakMenuBar Descendant("ZoomOutItem", 305);
      zoomInItem    := ASK weakMenuBar Descendant("ZoomInItem", 304);
   ELSIF currentView = "fev"
      zoomOutButton := ASK fevToolBar Descendant("ZoomOut",914);
      zoomInButton  := ASK fevToolBar Descendant("ZoomIn",915);
      zoomOutItem   := ASK fevMenuBar Descendant("ZoomOutItem", 405);
      zoomInItem    := ASK fevMenuBar Descendant("ZoomInItem", 404);
   END IF;
   IF activeWindow > 0
      tempHier := ASK root Child("RBDHier", activeWindow);
      tempZoom := tempHier.zoom;
      tempXOrig := tempHier.xOrigin;
      tempYOrig := tempHier.yOrigin;
   ELSE
      tempZoom := cusZoomVal;
      tempXOrig := xOrigin;
      tempYOrig := yOrigin;
   END IF;
   IF dir = "out"
      tempZoom := tempZoom+6.;
      IF tempZoom >=118.
         tempZoom := 118.;
         ASK zoomOutButton TO Deactivate;
         ASK zoomOutItem TO Deactivate;
      ELSIF tempZoom >= 7.
         ASK zoomInButton TO Activate;
         ASK zoomInItem TO Activate;
      END IF;
   ELSIF dir = "in"
      tempZoom := tempZoom-6.;
      IF tempZoom <= 6.
         tempZoom := 6.;
         ASK zoomInButton TO Deactivate;
         ASK zoomInItem TO Deactivate;
      ELSIF tempZoom < 117.
         ASK zoomOutButton TO Activate;
         ASK zoomOutItem TO Activate;
      END IF;
   END IF;
   IF (activeWindow > 0)
      tempHier := ASK root Child("RBDHier", activeWindow);
      ASK tempHier TO SetZoom(tempZoom);
   ELSE
      cusZoomVal := tempZoom;
   END IF;
   SetView(tempZoom, tempXOrig, tempYOrig);
END PROCEDURE; {Zoom}

PROCEDURE VisitRaptorWebpage;
VAR
   pID            : INTEGER;
   result         : BOOLEAN;
   str : STRING;
BEGIN
   pID := SystemCall({installPath + }"webCall.exe",0);
   {pID := SystemCall((GetProgDir("Autosend.html") + "Autosend.html"),0);}
   IF pID = -1
      NEW(message, 1..2);
      message[1] := "ERROR: Unable to load Raptor web page.     ";
      message[2] := "Please visit www.raptorplus.com through your favorite browser.     ";
      result := SendAlert(message, FALSE, FALSE, TRUE);
      DISPOSE(message);
   END IF;
END PROCEDURE;

PROCEDURE OpenRaptorRef;
VAR
   pID              : INTEGER;
   result           : BOOLEAN;
BEGIN
   pID := SystemCall("Help\Raptor70-01Ed.pdf",0);
   {pID := SystemCall((GetProgDir("Help\Raptor70-01Ed.pdf") + "Raptor70-01Ed.pdf"),0);}
   IF pID = -1
      NEW(message, 1..3);
      message[1] := "ERROR: Unable to open Raptor Reference Manual.     ";
      message[2] := "NOTE: Adobe Acrobat reader is required to view this document.     ";
      result := SendAlert(message, FALSE, FALSE, TRUE);
      DISPOSE(message);
   END IF;
END PROCEDURE;

PROCEDURE MassEdit;
VAR
   massEditDialog            : MassEditBoxObj;
BEGIN
   typeOfCursor := dialogC;
   ASK menubar TO Disable1Thru8;
   NEW(massEditDialog);
   ASK massEditDialog TO LoadFromLibrary(dialogs, "MassEditBox");
   ASK window TO AddGraphic(massEditDialog);
   ASK massEditDialog TO MakeChanges(selectGroup);
   typeOfCursor := nilC;
   DISPOSE(massEditDialog);
   ClearAllBlocks;
   ASK menubar TO Enable2Thru6;
END PROCEDURE;

PROCEDURE PauseSim;
VAR
   lastChosen                          : GraphicVObj;
BEGIN
   FillStatusBar;
   ASK simMenuBar TO ChangeState("Paused");
   stepped := FALSE;
   inStepMode := TRUE;
   inJumpMode := FALSE;
   notPaused := FALSE;
   IF dSimWithGraph
      oldSpeed := timeSlice;
      Timescale := .0000001;
      timeSlice := Timescale;
   END IF;
   REPEAT
      lastChosen := ASK simMenuBar LastPicked;
      Delay(1);
      IF lastChosen = NILOBJ
         lastChosen := ASK simMenuBar Child("PauseItem", 201);
      END IF;
      HandleEvents(FALSE);
   UNTIL (notPaused) OR stepped;
END PROCEDURE;

PROCEDURE ResumeSim;
VAR
   lastChosen                          : GraphicVObj;
BEGIN            
   IF dSimWithGraph
      Timescale := oldSpeed;
      timeSlice := oldSpeed;
      ASK simMenuBar TO ChangeState("ChangeSpeed");
   END IF;
   IF stepped AND (NOT inEndState)
      ASK simMenuBar TO ChangeState("Unstepped");
   END IF;
   goToNext := TRUE;
   inStepMode := FALSE;
   inJumpMode := FALSE;
   notPaused := TRUE;
   ASK simMenuBar TO ChangeState("Unpaused");
   IF NOT statusBarOn
      FOR i := 0 TO 8
         ASK window TO ShowStatus(i,"");
      END FOR;
      ASK window TO ShowStatus(1,"Simulating...");   
   ELSE
      ASK window TO ShowStatus(0,"Sim Speed: "+SUBSTR(1,8,REALTOSTR(1./Timescale)));   
   END IF;
END PROCEDURE; 

PROCEDURE DisplayTree;
VAR
   treeDialog : TreeBoxObj;
BEGIN
   NEW(treeDialog);
   ASK treeDialog TO LoadFromLibrary(dialogs, "TreeBox");
   ASK window TO AddGraphic(treeDialog);
   ASK treeDialog TO SetLabel(nameOfFile);
   ASK treeDialog TO ReceiveData;
  
END PROCEDURE;

PROCEDURE ResetNewFile;
VAR
   saveCancelled : BOOLEAN;
BEGIN
   ClearAllBlocks;
   ASK menubar TO Disable1Thru8;
   menuPath := pathName;
   menuFile := nameOfFile;
   openMenuFile := TRUE;
   NewFFile(gridIsOn, fileIsOpen, nameOfFile, pathName, filter, totalBlocks,
            totalNodes, totalLinks, totalHiers, totalEvents, saveCancelled);
   IF NOT saveCancelled
      totalObjects := 0; {start and end nodes}
      activeWindow := 0;
      ASK window TO SetSysCursor(BusyCursor);
      { ASK menubar TO Enable2Thru6;}
      IF copied 
         ASK menubar TO Enable(3);
         ResetPasted;
      END IF;
      ASK menubar TO Enable(5);
      ASK menubar TO Enable(6);
      ASK menubar TO Enable(15);
      ASK menubar TO Enable(18);
      hierLevel := 0;
      deepestLevel := 0;
      nextId := 1; {default block already set to 0}
      nextLinkId := 1;
      AddStartEndNodes;
      cusZoomVal := 24.;
      xOrigin := 0.;
      yOrigin := 80.;
      SetView(cusZoomVal, 0.,80.);
      blueObjRef := "";
      blueObjId  := -1;
      dTimeTrunc     := 1000.0;
      dTimeStartTime := 0.0;
      dNumberOfRuns  := 1.0;
      dTimeSlice     := 10000000.0;
      dFailTrunc     := 25.0;
      dFailStartTime := 0.0;
      dCycleTrunc    := 5.;
      dCycleStartTime := 0.0;
      dSimWithGraph  := TRUE;
      statusBarOn    := TRUE;
      termType       := 1;
      negShutUp      := FALSE;
      systemImage    := globalImage;
      systemUnits    := globalUnits;
      sysStreams[1] := 7;   sysStreams[3] := 9;
      sysStreams[2] := 8;   sysStreams[4] := 10;
      sysStreams[5] := 70;  sysStreams[6] := 71;
      sysStreams[7] := 72;  sysStreams[8] := 73;
      sysStreams[9] := 74;  sysStreams[10] :=75;
      sysStreams[11] := 101;
      weakLinkAnalType := 1;
      GYthreshold    := 0.95;
      YRthreshold    := 0.90;
      yMin := 0.0;
      yMax := 1.0;
      flowGenerated := 1;  
      systemRedCost := 0.0;
      sysLostCost := 0.0;
      weakAnalysis := FALSE;
      capacityAnalysis := FALSE;
      configFrozen  := FALSE;
      analViewAvail := FALSE;
      costAnalysis := FALSE;
      dZeroFail := FALSE;
      ASK menubar TO SetChecks;
      rapVersion := 7;
      InitFactorySettings;
      somethingChanged := FALSE;
   ELSE
      ASK menubar TO Enable2Thru6;
   END IF;
   typeOfCursor := nilC;
   ASK window TO SetSysCursor(NormalCursor);
END PROCEDURE;
 
{*** Main Menu ***} 
OBJECT mainMenuObj;
   ASK METHOD BeSelected;
   VAR
      tempName,{ tempUnits, tempImage, }tempPath, tempFile,
      lineOne                            : STRING;
      diff, checksOut, j, longestName, tempActiveWindow, tempHierLevel, numParams    : INTEGER;
      cancelled, {tempSave,}
      fileIsThere, goodName, success, 
      matchedName, validRBD, saveCancelled,somethingChangedSave, test : BOOLEAN;
      defaultStream, checkFile, exportFile            : StreamObj;
      check1, check2, check3, check4                  : CheckBoxObj;
      button                                          : ButtonObj;
      aboutBox                                        : AboutBoxObj;
      systemInputBox                                  : SystemInputBoxObj;
      blockInputBox                                   : BlockInputBoxObj;
      nodeInputBox                                    : NodeInputBoxObj;
      eventInputBox                                   : EventInputBoxObj;
      hierInputBox                                    : HierInputBoxObj;
      tabOutBox                                       : TablesOutObj;
      prefBox                                         : PrefsBoxObj;
      sysBox                                          : SystemBoxObj;
      printTablesBox                                  : PrintTablesBoxObj;
      detailsDialog                                   : BlockPropObj;
      trigBox                                         : TrigBoxObj;
      phaseBox                                        : PhaseBoxObj;
      realsArray, tempArray                           : realArray;
      tempZoom                          : REAL;
      tempNode                                        : RBDNodeObj;
      tempHier                                        : RBDHierObj;
      sparePoolsBox                                   : SparePoolsBoxObj;
      resPoolsBox                                     : ResPoolsBoxObj;
      dialogBox                            : HelpBoxObj;
      tempBlk                                         : RBDBlockObj;
   BEGIN
      IF ignoreMouse {eag error 51 fix}
         RETURN;
      END IF;
      IF popupMenu.Visible
         ASK window TO KillPopup;
      END IF;
      IF (compileType <> "demo") AND saveIsOn AND ((TRUNC(ClockRealSecs)-lastSave) >= saveInc) AND (totalBlocks > 1)
         AND (typeOfCursor = nilC) AND (NOT pastingMultiple)
         lastSave := TRUNC(ClockRealSecs);
         typeOfCursor := autoC;
         ASK window TO ShowStatus(0,"Autosaving...");
         SaveFile("cksrule.rbd", exampPath, "*.rbd", totalBlocks, totalNodes, totalLinks, totalHiers, totalEvents);
         typeOfCursor := nilC;
         ASK window TO ShowStatus(0,"");
      END IF; 
      IF (typeOfCursor <> autoC)
      CASE ASK LastPicked Id
         {*** File Menu ***}
         WHEN 101: {New}
            ResetNewFile;
         WHEN 102: {Open}
            ClearAllBlocks;
            Disable1Thru8;
            menuPath := pathName;
            menuFile := nameOfFile;
            statusBarOn  := TRUE;
            openMenuFile := FALSE;
            OpenFile(FALSE, FALSE, totalBlocks, totalNodes, totalLinks, totalHiers, totalEvents, 0, gridIsOn, fileIsOpen,
                     nameOfFile, pathName, filter, cancelled);
            IF NOT cancelled
               hierLevel := 0;
               activeWindow := 0;
               CheckOpenFileStatus;
               SetChecks;
               IF copied
                  Enable(3);
                  ResetPasted;
               END IF;
            ELSE
               Enable2Thru6;
            END IF;
            analViewAvail := FALSE;
            configFrozen := FALSE;
            typeOfCursor := nilC;
            ASK window TO Update;
         WHEN 103: {Save}
            ClearAllBlocks;
            Disable1Thru8;
            typeOfCursor := dialogC;
            IF fileIsOpen
               tempPath := pathName;
               tempName := nameOfFile;
               IF (nameOfFile = "Unnamed RBD") OR (nameOfFile = "NoFile")
                   SaveAsFile(nameOfFile, pathName, filter, "Save RBD File");
               END IF;
               IF nameOfFile <> "NoFile"
                  SaveFile(nameOfFile, pathName, filter, totalBlocks, totalNodes, totalLinks, 
                           totalHiers,totalEvents);
               ELSE
                  pathName := tempPath;
                  nameOfFile := tempName;
               END IF;
            END IF;
            typeOfCursor := nilC;
            Enable2Thru6;
         WHEN 104: {Save As}
            ClearAllBlocks;
            Disable1Thru8;
            typeOfCursor := dialogC;
            IF fileIsOpen
               tempPath := pathName;
               tempName := nameOfFile;
               SaveAsFile(nameOfFile, pathName, "*.rbd", "Save RBD File");
               IF nameOfFile <> "NoFile"
                  rapVersion := 7;
                  SaveFile(nameOfFile, pathName, "*.rbd", totalBlocks, totalNodes, totalLinks,
                           totalHiers,totalEvents);                  
               ELSE
                  pathName := tempPath;
                  nameOfFile := tempName;
               END IF;
            END IF;
            typeOfCursor := nilC;
            Enable2Thru6;
         WHEN 105: {Save RBD as BMP}
            ExportRBD(FALSE, 0, activeWindow, "Don't Matter");
         WHEN 106: {Save Tables}
            SaveTables;
         WHEN 1071: {Print Tables}
            typeOfCursor := dialogC;
            NEW(printTablesBox);
            ASK printTablesBox TO LoadFromLibrary(dialogs, "PrintTablesBox");
            ASK window TO AddGraphic(printTablesBox);
            ASK printTablesBox TO Draw;
            ASK printTablesBox TO GetPreferences(FALSE);
            DISPOSE(printTablesBox);
            typeOfCursor := nilC;
         WHEN 1072: {Print Window}
            PrintWindow;
         WHEN 1073: {Print Tree}
            PrintTree;
         WHEN 1074: {Print Selected}
            PrintSelected;
         WHEN 108: {Print Setup}
            PrintSetup;
        WHEN 1091: {Import Lib}
            result := FALSE;
            loadCanceled := FALSE;
            IF libArray <> NILARRAY
               NEW(message,1..2);
               message[1] := "A block library is currently loaded.  Click yes to     ";
               message[2] := "append to current library, no to overwrite current library?     ";
               threeWay := TRUE;
               result := SendAlert(message, FALSE, TRUE, FALSE);
               threeWay := FALSE;
               DISPOSE(message);
            END IF;
            IF NOT loadCanceled
               filter := "*.rbl";
               GetFileName(tempFile, tempPath, "*.rbl", "Select Block Library File Name");
               IF tempFile <> "NoFile"
                  NEW(checkFile);
                  ASK checkFile TO Open((tempPath + tempFile), Input);
                  checksOut := checkFile.ioResult;
                  ASK checkFile TO Close;
                  DISPOSE(checkFile);   
                  IF checksOut <> 0
                     NEW(message, 1..1);
                     message[1] := "Cannot find specified file!     ";
                     result := SendAlert(message, FALSE, FALSE, TRUE);
                     DISPOSE(message);
                  ELSE
                     IF result
                        ReadLib(tempPath + tempFile, TRUE, success);
                     ELSE
                        ReadLib(tempPath + tempFile, FALSE, success);
                     END IF;
                  END IF;
               END IF;
               IF success AND (totalBlocks > 0)
                  FOREACH tempBlk IN blockGroup
                     FOR j := 1 TO HIGH(nameArray)
                        IF tempBlk.name = nameArray[j]
                           matchedName := TRUE;
                           EXIT;
                        END IF;
                     END FOR;
                  END FOREACH;
                  IF matchedName
                     NEW(dialogBox);
                     ASK dialogBox TO LoadFromLibrary(dialogs, "LibraryBox");
                     ASK window TO AddGraphic(dialogBox);
                     ASK dialogBox TO Draw;
                     check1 := ASK dialogBox Descendant("FailChkBox",101);
                     check2 := ASK dialogBox Descendant("RepChkBox",102);
                     check3 := ASK dialogBox Descendant("LDTChkBox",103);
                     check4 := ASK dialogBox Descendant("DepChkBox",104);
                     button := ASK dialogBox TO AcceptInput();
                     IF button.ReferenceName = "OKButton"
                        NEW(tempArray,1..1);
                        FOREACH tempBlk IN blockGroup
                           FOR j := 1 TO HIGH(nameArray)
                              IF tempBlk.name = nameArray[j]
                                 somethingChanged := TRUE;
                                 IF check1.Checked
                                    NEW(realsArray, 1..4);
                                    realsArray[1] := STRTOREAL(libArray[j,3]);{failVal 1}
                                    realsArray[2] := STRTOREAL(libArray[j,4]);{failVal 2}
                                    realsArray[3] := STRTOREAL(libArray[j,5]);{failVal 3}
                                    ASK tempBlk TO SetfailDistro(STRTOINT(libArray[j,2]));               
                                    GetNumParams(tempBlk.failDistro,numParams);
                                    ASK tempBlk TO SetnumFailParams(numParams);   
                                    ASK tempBlk TO SetfailStream(tempBlk.failStream);
                                    ASK tempBlk TO SetfailVals(realsArray);
                                    DISPOSE(realsArray);
                                 END IF;
                                 IF check2.Checked
                                    NEW(realsArray, 1..4);
                                    realsArray[1] := STRTOREAL(libArray[j,8]);{repairVal 1}
                                    realsArray[2] := STRTOREAL(libArray[j,9]);{repairVal 2}
                                    realsArray[3] := STRTOREAL(libArray[j,10]);{repairVal 3}
                                    ASK tempBlk TO SetrepairDistro(STRTOINT(libArray[j,7]));   
                                    GetNumParams(tempBlk.repairDistro,numParams);
                                    ASK tempBlk TO SetnumRepairParams(numParams);   
                                    ASK tempBlk TO SetrepairStream(tempBlk.repairStream);
                                    ASK tempBlk TO SetrepairVals(realsArray);
                                    DISPOSE(realsArray);
                                 END IF;
                                 IF check3.Checked
                                    IF libVersion=6
                                       ASK tempBlk TO SetpreDist(19);
                                       tempArray[1]:=STRTOREAL(libArray[j,12]);
                                       ASK tempBlk TO SetpreParams(tempArray);  
                                       ASK tempBlk TO SetpostDist(19);
                                       tempArray[1]:=STRTOREAL(libArray[j,13]);
                                       ASK tempBlk TO SetpostParams(tempArray);  
                                    ELSIF libVersion=7
                                       NEW(realsArray, 1..3);
                                       ASK tempBlk TO SetpreDist(STRTOINT(libArray[j,12]));
                                       realsArray[1] := STRTOREAL(libArray[j,15]);{preVal 1}
                                       realsArray[2] := STRTOREAL(libArray[j,16]);{preVal 2}
                                       realsArray[3] := STRTOREAL(libArray[j,17]);{preVal 3}
                                       ASK tempBlk TO SetpreParams(realsArray);  
                                       ASK tempBlk TO SetpostDist(STRTOINT(libArray[j,13]));
                                       realsArray[1] := STRTOREAL(libArray[j,18]);{postVal 1}
                                       realsArray[2] := STRTOREAL(libArray[j,19]);{postVal 2}
                                       realsArray[3] := STRTOREAL(libArray[j,20]);{postVal 3}
                                       ASK tempBlk TO SetpostParams(realsArray);  
                                       DISPOSE(realsArray);
                                    ELSE
                                       {neither 6 or 7 library}
                                    END IF;   
                                 END IF;
                                 IF check4.Checked
                                    IF libArray[j,14] = "TRUE"
                                       ASK tempBlk TO SetDep(-2, "");
                                    ELSE
                                       ASK tempBlk TO SetDep(0, "");
                                    END IF;
                                 END IF;
                                 ASK tempBlk TO SetStats();
                              END IF;
                           END FOR;
                        END FOREACH;
                        DISPOSE(tempArray);
                     END IF;
                  END IF;
               END IF;
            END IF;
            filter := "*.rbd";
         WHEN 1092: {Export Lib}
            FOREACH tempBlk IN blockGroup
               IF (STRLEN(tempBlk.name) > longestName)
                  longestName := STRLEN(tempBlk.name);
               END IF;
            END FOREACH;
            IF longestName > 0
               filter := "*.rbz";
               REPEAT
                  GetFileName(tempFile, tempPath, "*.rbl", "Save Block Library");
                  fileIsThere := FileExists(tempPath + tempFile);
                  goodName := TRUE;
                  IF fileIsThere
                     NEW(dialogBox);
                     ASK dialogBox TO LoadFromLibrary(dialogs, "OverwriteBox");
                     ASK window TO AddGraphic(dialogBox);
                     ASK dialogBox TO Draw;
                     button := ASK dialogBox TO AcceptInput();
                     IF ASK button ReferenceName = "NoButton"
                        goodName := FALSE;
                     END IF;
                     DISPOSE(dialogBox);
                  END IF;
               UNTIL goodName;
               IF tempFile <> "NoFile"
                  NEW(exportFile);
                  ASK exportFile TO Open((tempPath + tempFile), Output);
                  IF exportFile.ioResult <> 0
                     NEW(message, 1..2);
                     message[1] := "There is a problem opening the selected library file.     ";
                     message[2] := "Make sure this file has not been set to read only.     ";
                     result := SendAlert(message, FALSE, FALSE, FALSE);
                     DISPOSE(message);
                  ELSE
                     DateTime(lineOne);
                     lineOne := SUBSTR(1,3,lineOne)+"_"+SUBSTR(5,7,lineOne)+"_"+SUBSTR(9,10,lineOne)+"_"
                                +SUBSTR(12,19,lineOne)+"_"+SUBSTR(21,24,lineOne);
                     lineOne := "Raptor7.0/"+nameOfFile+"/"+lineOne;
                     ASK exportFile TO WriteString(lineOne);
                     ASK exportFile TO WriteLn;
                     ASK exportFile TO WriteString("Block_Name    FDist  FVal1  FVal2  FVal3  RDist  RVal1 "+
                        " Rval2  RVal3  PreLDT  PostLDT  Depend PreVal1 PreVal2 PreVal3  PostVal1 PostVal2 PostVal3");
                     ASK exportFile TO WriteLn;
                     FOREACH tempBlk IN blockGroup
                        ASK exportFile TO WriteString(tempBlk.name);
                        FOR j := STRLEN(tempBlk.name) TO longestName+1
                           ASK exportFile TO WriteString(" ");
                        END FOR;
                        IF tempBlk.failDistro <> 16 
                           ASK exportFile TO WriteString(INTTOSTR(tempBlk.failDistro)+"  ");
                           FOR j := 1 TO HIGH(tempBlk.failVals)
                              ASK exportFile TO WriteString(REALTOSTR(tempBlk.failVals[j])+"  ");
                           END FOR;
                           FOR j := HIGH(tempBlk.failVals)+1 TO 3
                              ASK exportFile TO WriteString("0.0  ");
                           END FOR;
                        ELSE   {new}
                           ASK exportFile TO WriteString("4  ");
                           ASK exportFile TO WriteString("100.000000  ");
                           ASK exportFile TO WriteString("0.000000  ");
                           ASK exportFile TO WriteString("0.0  ");
                        END IF;
                        IF tempBlk.repairDistro <> 16
                           ASK exportFile TO WriteString(INTTOSTR(tempBlk.repairDistro)+"  ");
                           FOR j := 1 TO HIGH(tempBlk.repairVals)
                              ASK exportFile TO WriteString(REALTOSTR(tempBlk.repairVals[j])+"  ");
                           END FOR;
                           FOR j := HIGH(tempBlk.repairVals)+1 TO 3
                              ASK exportFile TO WriteString("0.0  ");
                           END FOR;
                        ELSE   {new}
                           ASK exportFile TO WriteString("7  ");
                           ASK exportFile TO WriteString("10.000000  ");
                           ASK exportFile TO WriteString("2.000000  ");
                           ASK exportFile TO WriteString("0.0  ");
                        END IF; 
                        ASK exportFile TO WriteString(INTTOSTR(tempBlk.preDist)+"  ");
                        ASK exportFile TO WriteString(INTTOSTR(tempBlk.postDist)+"  ");
                        IF tempBlk.DependencyNum = -2
                           ASK exportFile TO WriteString("Yes  ");
                        ELSE
                           ASK exportFile TO WriteString("No   ");
                        END IF;
                        FOR j := 1 TO HIGH(tempBlk.preParams)
                           ASK exportFile TO WriteString(REALTOSTR(tempBlk.preParams[j])+"  ");
                        END FOR;
                        FOR j := HIGH(tempBlk.preParams)+1 TO 3
                           ASK exportFile TO WriteString("0.0  ");
                        END FOR;
                        FOR j := 1 TO HIGH(tempBlk.postParams)
                           ASK exportFile TO WriteString(REALTOSTR(tempBlk.postParams[j])+"  ");
                        END FOR;
                        FOR j := HIGH(tempBlk.postParams)+1 TO 3
                           ASK exportFile TO WriteString("0.0  ");
                        END FOR;
                        ASK exportFile TO WriteLn;
                     END FOREACH;
                  END IF;
                  ASK exportFile TO Close;
               END IF;
            ELSE
               NEW(message, 1..1);
               message[1] := "There are no blocks to export.     ";
               result := SendAlert(message, FALSE, FALSE, FALSE);
               DISPOSE(message);
            END IF;            
            filter := "*.rbd";
         WHEN 110: {Import RBD as Hierarchy}
            Disable1Thru8;
            OpenFile(FALSE, TRUE, totalBlocks, totalNodes, totalLinks, totalHiers, totalEvents, hierLevel+1 {eag}, gridIsOn, fileIsOpen,
                     menuFile, menuPath, filter, saveCancelled);
            CheckOpenFileStatus;
            {do we need more?}
         WHEN 111: {Close}
            tempActiveWindow := activeWindow;
            tempHierLevel := hierLevel;
            activeWindow := 0;
            hierLevel := 0;
            ClearAllBlocks;
            typeOfCursor := dialogC;
            Disable1Thru8;
            Disable(9);
            nowInitialing := TRUE;
            openMenuFile := FALSE;
            menuPath := pathName;
            menuFile := nameOfFile;
            CloseFFile(gridIsOn, fileIsOpen, cancelled, nameOfFile, pathName, filter,
                       totalBlocks, totalNodes, totalLinks, totalHiers,totalEvents);
            IF cancelled
               activeWindow := tempActiveWindow;
               hierLevel := tempHierLevel;
               Enable2Thru5;
            ELSE
               totalBlocks  := 0;
               totalNodes   := 0;
               totalLinks   := 0;
               totalPools   := 0;
               totalObjects := 0;
               blueObjRef   := "";
               blueObjId    := -1;
               rapVersion := 7;
               configFrozen := FALSE;
               analViewAvail := FALSE;
               ASK grid TO Colorize("Hide");
               typeOfCursor := dialogC;
            END IF;
            Enable(6);
            Enable(9);
            nowInitialing := FALSE;
         WHEN 112: {Exit}
            ASK window TO BeClosed;
         WHEN 115,116,117,118,119,120:
            ClearAllBlocks;
            IF (ASK LastPicked Id) = 115
               menuPath := path1;
               menuFile := file1;
            ELSIF (ASK LastPicked Id) = 116
               menuPath := path2;
               menuFile := file2;
            ELSIF (ASK LastPicked Id) = 117
               menuPath := path3;
               menuFile := file3;
            ELSIF (ASK LastPicked Id) = 118
               menuPath := path4;
               menuFile := file4;
            ELSIF (ASK LastPicked Id) = 119
               menuPath := path5;
               menuFile := file5;
            ELSIF (ASK LastPicked Id) = 120
               menuPath := path6;
               menuFile := file6;
            END IF;
            statusBarOn := TRUE;
            IF menuPath <> "file"
               Disable1Thru8;
               statusBarOn := TRUE;
               openMenuFile := TRUE;
               OpenFile(FALSE, FALSE, totalBlocks, totalNodes, totalLinks, totalHiers,totalEvents,0,gridIsOn, fileIsOpen,
                        nameOfFile, pathName, filter, cancelled);
               IF NOT cancelled
                  hierLevel := 0;
                  activeWindow := 0;
                  CheckOpenFileStatus; 
                  SetChecks;
                  IF copied
                     Enable(3);
                     ResetPasted;
                  END IF;
               ELSE
                  Enable2Thru6;
               END IF;
            END IF;
            configFrozen := FALSE;
            analViewAvail := FALSE;
            typeOfCursor := nilC;
            ASK window TO Update;
         {*** Edit Menu ***}
         WHEN 2011: {Add Block}
            buttItem := ASK menuTool Descendant("BlockButton",908);
            ASK buttItem TO SetSelected(TRUE);
            ASK buttItem TO Draw;
            AddBlock;
         WHEN 2012: {Add Node}
            buttItem := ASK menuTool Descendant("NodeButton",909);
            ASK buttItem TO SetSelected(TRUE);
            ASK buttItem TO Draw;
            AddNode;
         WHEN 2013: {Add Connector}
            buttItem := ASK menuTool Descendant("LinkButton",910);
            ASK buttItem TO SetSelected(TRUE);
            ASK buttItem TO Draw;
            NEW(linkCursor);
            ASK root TO AddGraphic(linkCursor);
            AddConnector;
            IF linkCancelled
               DISPOSE(linkCursor);
               Enable(9);
               linkCancelled := FALSE;
            END IF;
         WHEN 2014: {Add Event}
            buttItem := ASK menuTool Descendant("EventButton",911);
            ASK buttItem TO SetSelected(TRUE);
            ASK buttItem TO Draw;
            AddEvent;
         WHEN 2015: {Add Hier}
            buttItem := ASK menuTool Descendant("HierButton",912);
            ASK buttItem TO SetSelected(TRUE);
            ASK buttItem TO Draw;
            AddHier;
         WHEN 202: {Select All}
            SelectAll;
         WHEN 203: {Cut}
            CopyObject;
            ClearObject;
         WHEN 204: {Copy}
            CopyObject; 
         WHEN 205: {Paste}
            PasteObject;
         WHEN 206: {Clear}
            ClearObject; 
         WHEN 207: {Edit Properties}
            buttItem := ASK menuTool Descendant("EditButton",940);
            ASK buttItem TO SetSelected(TRUE);
            ASK buttItem TO Draw;
            EditDetails;
         WHEN 208: {Collapse}
            CollapseIntoHier;
         WHEN 209: {Block Defaults}
            ClearAllBlocks;
            typeOfCursor := dialogC;
            Disable1Thru8;
            NEW(detailsDialog);
            ASK detailsDialog TO LoadFromLibrary(dialogs, "BlockPropBox");
            ASK window TO AddGraphic(detailsDialog);
            ASK detailsDialog TO Draw;
            ASK detailsDialog TO ReceiveData(defaultBlock, "952", cancelled);
            DISPOSE(detailsDialog);
            {This code commented out to see if block defaults really need to be saved to file
             when changed.  If no errors arise, remove this code}
            {IF NOT cancelled
               IF fileIsOpen
                  tempPath := pathName;
                  tempName := nameOfFile;
                  IF (nameOfFile = "Unnamed RBD") OR (nameOfFile = "NoFile")
                     NEW(message,1..3);
                     message[1] := "Block defaults are saved to an individual RBDs configuration     ";
                     message[2] := "files.  The current RBD in use has not been saved yet.";
                     message[3] := "Do you wish to save the current RBD at this time?";
                     result := SendAlert(message, FALSE, TRUE, FALSE);
                     DISPOSE(message);
                     IF result
                        SaveAsFile(nameOfFile, pathName, filter, "Save RBD File");
                     END IF;
                  END IF;
                  IF (nameOfFile <> "NoFile") AND (nameOfFile <> "Unnamed RBD")
                     SaveFile(nameOfFile, pathName, filter, totalBlocks, totalNodes, totalLinks);
                  ELSE
                     pathName := tempPath;
                     nameOfFile := tempName;
                  END IF;
               END IF;
            END IF;}
            Enable2Thru6;
            IF totalObjects = 0
               Disable(2);
               Disable(3);
               Disable(4);
               Disable(10);
               Disable(12);
               Disable(14);
            END IF;
            typeOfCursor := nilC;
         WHEN 210: {Mass Edit}
            MassEdit;
         {*** Options Menu ***}
         WHEN 3011: {View Block Inputs}
            buttItem := ASK menuTool Descendant("ViewInButton",913);
            ASK buttItem TO SetSelected(TRUE);
            ASK buttItem TO Draw;
            ClearAllBlocks;
            typeOfCursor := dialogC;            
            NEW(blockInputBox);
            ASK blockInputBox TO LoadFromLibrary(dialogs, "BlockInputBox");
            ASK window TO AddGraphic(blockInputBox);
            ASK blockInputBox TO InitSelf(FALSE);
            button := ASK blockInputBox TO AcceptInput();
            typeOfCursor := nilC;
            DISPOSE(blockInputBox);
            buttItem := ASK menuTool Descendant("ViewInButton",913);
            ASK buttItem TO SetSelected(FALSE);
            ASK buttItem TO Draw;
         WHEN 3012: {View Node Inputs}
            ClearAllBlocks;
            typeOfCursor := dialogC;            
            NEW(nodeInputBox);
            ASK nodeInputBox TO LoadFromLibrary(dialogs, "NodeInputBox");
            ASK window TO AddGraphic(nodeInputBox);
            ASK nodeInputBox TO InitSelf(FALSE);
            button := ASK nodeInputBox TO AcceptInput();
            typeOfCursor := nilC;
            DISPOSE(nodeInputBox);
         WHEN 3013: {View Event Inputs}
            ClearAllBlocks;
            typeOfCursor := dialogC;            
            NEW(eventInputBox);
            ASK eventInputBox TO LoadFromLibrary(dialogs, "EventInputBox");
            ASK window TO AddGraphic(eventInputBox);
            ASK eventInputBox TO InitSelf(FALSE);
            button := ASK eventInputBox TO AcceptInput();
            typeOfCursor := nilC;
            DISPOSE(eventInputBox);
         WHEN 3014: {View Hierarchy Inputs}
            ClearAllBlocks;
            typeOfCursor := dialogC;            
            NEW(hierInputBox);
            ASK hierInputBox TO LoadFromLibrary(dialogs, "HierInputBox");
            ASK window TO AddGraphic(hierInputBox);
            ASK hierInputBox TO InitSelf;
            button := ASK hierInputBox TO AcceptInput();
            typeOfCursor := nilC;
            DISPOSE(hierInputBox);
         WHEN 3015:          {View Input Tables}
            ClearAllBlocks;
            typeOfCursor := dialogC;            
            NEW(systemInputBox);
            ASK systemInputBox TO LoadFromLibrary(dialogs, "SystemInputBox");
            ASK window TO AddGraphic(systemInputBox);
            ASK systemInputBox TO InitSelf(FALSE);
            button := ASK systemInputBox TO AcceptInput();
            typeOfCursor := nilC;
            DISPOSE(systemInputBox);
         WHEN 3016:          {View Output Tables}
            buttItem := ASK menuTool Descendant("ViewOutButton",914);
            ASK buttItem TO SetSelected(TRUE);
            ASK buttItem TO Draw;
            ClearAllBlocks;
            typeOfCursor := dialogC;            
            NEW(tabOutBox);
            ASK tabOutBox TO LoadFromLibrary(dialogs, "ViewOutputBox");
            ASK window TO AddGraphic(tabOutBox);
            ASK tabOutBox TO InitSelf(FALSE);
            button := ASK tabOutBox TO AcceptInput();
            typeOfCursor := nilC;
            DISPOSE(tabOutBox);  
            buttItem := ASK menuTool Descendant("ViewOutButton",914);
            ASK buttItem TO SetSelected(FALSE);
            ASK buttItem TO Draw;
         WHEN 3017:     {Save Tables as Text Files}
            SaveTables;
         WHEN 302:            {Spare Pools...}
            NEW(sparePoolsBox);
            ASK sparePoolsBox TO LoadFromLibrary(dialogs, "SparePoolsBox");
            ASK window TO AddGraphic(sparePoolsBox);
            ASK sparePoolsBox TO ReceiveData;
            DISPOSE(sparePoolsBox);
         WHEN 303: {Resource Pools...}
            NEW(resPoolsBox);
            ASK resPoolsBox TO LoadFromLibrary(dialogs, "ResPoolsBox");
            ASK window TO AddGraphic(resPoolsBox);
            ASK resPoolsBox TO ReceiveData;
            DISPOSE(resPoolsBox);
         WHEN 304: {Triggers}
            NEW(trigBox);
            ASK trigBox TO LoadFromLibrary(dialogs, "TrigBox");
            ASK window TO AddGraphic(trigBox);
            ASK trigBox TO ReceiveData;
            DISPOSE(trigBox);
         WHEN 305: {System Settings}
            NEW(sysBox);
            ASK sysBox TO LoadFromLibrary(dialogs, "SystemBox");
            ASK window  TO AddGraphic(sysBox);
            ASK sysBox TO Draw;
            ASK sysBox TO ReceiveData;
            DISPOSE(sysBox);
         WHEN 306:            {Phases...}
            typeOfCursor := dialogC;            
            NEW(phaseBox);
            ASK phaseBox TO LoadFromLibrary(dialogs, "PhaseBox");
            ASK window TO AddGraphic(phaseBox);
            ASK phaseBox TO InitSelf;
            button := ASK phaseBox TO AcceptInput();      
            typeOfCursor := nilC;
            DISPOSE(phaseBox);
         WHEN 3071: {Weak Link Analysis}
            menuItem := Descendant("WeakAnalItem",3071);
            buttItem := ASK menuTool Descendant("NodeAnalButton",919);
            IF weakAnalysis
               ASK menuItem TO SetLabel("Enable Weak Link");
               ASK buttItem TO SetSelected(FALSE);
               weakAnalysis := FALSE;
            ELSE
               ASK menuItem TO SetLabel("Disable Weak Link");
               ASK buttItem TO SetSelected(TRUE);
               weakAnalysis := TRUE;
            END IF;
            ASK menuItem TO Draw;
            ASK buttItem TO Draw;
         WHEN 3072: {Capacity}
            menuItem := Descendant("CapAnalItem",3072);
            buttItem := ASK menuTool Descendant("CapButton",920);
            IF capacityAnalysis
               ASK menuItem TO SetLabel("Enable Capacity");
               ASK buttItem TO SetSelected(FALSE);
               capacityAnalysis := FALSE;
            ELSE
               ASK menuItem TO SetLabel("Disable Capacity");
               ASK buttItem TO SetSelected(TRUE);
               capacityAnalysis := TRUE;
            END IF;
            ASK menuItem TO Draw;
            ASK buttItem TO Draw;
         WHEN 3073: {Cost Analysis}
            menuItem := Descendant("CostAnalItem",3073);
            buttItem := ASK menuTool Descendant("CostButton",921);
            IF costAnalysis
               ASK menuItem TO SetLabel("Enable Cost");
               ASK buttItem TO SetSelected(FALSE);
               costAnalysis := FALSE;
            ELSE
               ASK menuItem TO SetLabel("Disable Cost");
               ASK buttItem TO SetSelected(TRUE);
               costAnalysis := TRUE;
            END IF;
            ASK menuItem TO Draw;
            ASK buttItem TO Draw;
         WHEN 308:       {Preferences}
            NEW(prefBox);
            ASK prefBox TO LoadFromLibrary(dialogs, "PrefsBox");
            ASK window  TO AddGraphic(prefBox);
            ASK prefBox TO Draw;
            ASK prefBox TO ReceiveData();
            DISPOSE(prefBox);
         {*** Navigate Menu ***}
         WHEN 401: {Home}
            Home;
         WHEN 402: {Up}
            UpOneLevel;
         WHEN 403: {Goto}
            GotoHier;
         WHEN 404: {Zoom In}
            Zoom("in");
         WHEN 405: {Zoom Out}
            Zoom("out");
         WHEN 406: {Zoom Percent}
            IF activeWindow > 0
               tempHier := ASK root Child("RBDHier", activeWindow)
               ZoomPercent(tempHier.xOrigin, tempHier.yOrigin, FALSE);
            ELSE
               ZoomPercent(xOrigin, yOrigin, FALSE);
            END IF;
         WHEN 407: {Find Item}
            FindItem;
         WHEN 408: {Display Tree}
            DisplayTree;
         {*** Launch Menu ***}
         WHEN 501: {Simulate}
            ValidateRBD(validRBD);
            IF validRBD
               currentView := "simulation";
               SendToEngine;
            END IF;
         WHEN 502: {Failure Effects View}
            ValidateRBD(validRBD);
            IF validRBD
               currentView := "fev";
               StartFailureEffects;
            END IF;
         WHEN 503: {Show Weak Link Anal View}
            {changedZoom := FALSE;}
            notSimming := TRUE;
            currentView := "weaklink";
            ShowAnalView(TRUE);
         {*** Help Menu ***}
         WHEN 601:       {Help}
            CallHelp(0);
         WHEN 602: {About}
            ClearAllBlocks;
            Disable1Thru8;
            typeOfCursor := dialogC;
            NEW(aboutBox);
            ASK aboutBox TO LoadFromLibrary(dialogs, "AboutBox");
            ASK window TO AddGraphic(aboutBox);
            {IF soundIsOn AND (FileExists(soundsPath + "Credits.wav"))
              ASK sound TO PlayMe(soundsPath + "Credits.wav");
            END IF;}
            ASK aboutBox TO ScrollCredits;
            DISPOSE(aboutBox);
            typeOfCursor := nilC;
            IF fileIsOpen
               Enable2Thru5;
            END IF;
            Enable(6);
            Enable(12);
            Enable(15);
         WHEN 603: {Web Page}
            VisitRaptorWebpage;
         WHEN 604: {Raptor Reference Manual}
            OpenRaptorRef;
         OTHERWISE;
      END CASE;
      END IF;
   END METHOD; {BeSelected}

   ASK METHOD SetNavigation;
   BEGIN
      IF ((totalHiers > 0) AND (typeOfCursor <> blockC) AND (typeOfCursor <> nodeC) 
                           AND (typeOfCursor <> eventC) AND (typeOfCursor <> hierC))
         menuItem := Descendant("GotoItem", 403);
         ASK menuItem TO Activate;
      ELSE
         menuItem := Descendant("GotoItem", 403);
         ASK menuItem TO Deactivate;
      END IF;
      IF activeWindow = 0 
         menuItem := Descendant("HomeItem",401);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("UpItem",402);
         ASK menuItem TO Deactivate;
      ELSIF ((typeOfCursor <> blockC) AND (typeOfCursor <> nodeC) AND (typeOfCursor <> eventC) AND (typeOfCursor <> hierC))
         menuItem := Descendant("HomeItem",401);
         ASK menuItem TO Activate;
         menuItem := Descendant("UpItem",402);
         ASK menuItem TO Activate;
      END IF;
   END METHOD; {SetNavigation}

   ASK METHOD Disable(IN greyLevel : INTEGER);
   BEGIN
      CASE greyLevel
      WHEN 1:
         menuItem := Descendant("PrintSelectItem",1074);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("CutItem",203);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("CutButton",905);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("CopyItem",204);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("CopyButton",906);
         ASK buttItem TO Deactivate;
      WHEN 2:  {save, save as, print and view table}
         menuItem := Descendant("SaveItem",103);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("SaveButton",903);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("SaveAsItem",104);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("PrintItem",1072);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("PrintButton",904);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("SaveRBDItem",105);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("BlockInputItem",3011);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("NodeInputItem",3012);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("EventInputItem",3013);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("HierInputItem",3014);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("InputItem",3015);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("ViewInButton",913);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("OutputItem",3016);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("WeakViewItem",503);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("ViewOutButton",914);
         ASK buttItem TO Deactivate;
         buttItem := ASK menuTool Descendant("AnalViewButton",931);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("SaveTXTItem",3017);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("SaveTableItem", 106);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("PrintTableItem", 1071);
         ASK menuItem TO Deactivate;
      WHEN 3:  {paste}
         menuItem := Descendant("PasteItem",205);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("PasteButton",907);
         ASK buttItem TO Deactivate;
      WHEN 4:  {simulate}
         menuItem := Descendant("SimItem",501);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("SimButton",929);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("WeakAnalItem",3071);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("NodeAnalButton",919);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("CapAnalItem",3072);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("CapButton",920);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("CostAnalItem",3073);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("CostButton",921);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("FEVItem",502);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("FEVButton",930);
         ASK buttItem TO Deactivate;
         IF compileType = "student"
            buttItem := ASK menuTool Descendant("PhaseButton", 918);
            ASK buttItem TO Deactivate;
            menuItem := Descendant("PhaseItem", 306);
            ASK menuItem TO Deactivate;
         END IF;
      WHEN 5:
         menuItem := Descendant("CloseItem",111);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("BlockItem",2011);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("BlockButton",908);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("NodeItem",2012);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("NodeButton",909);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("ConnectorItem",2013);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("LinkButton",910);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("EventItem",2014);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("EventButton",911);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("HierItem",2015);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("HierButton",912);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("ZoomInItem",404);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("ZoomInButton",926);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("ZoomOutItem",405);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("ZoomOutButton",925);
         ASK buttItem TO Deactivate;
         buttItem := ASK menuTool Descendant("ZoomFitButton",941);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("DefaultsItem",209);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("PhaseItem",306);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("PhaseButton",918);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("SpareItem", 302);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("SpareButton",915);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("ResItem", 303);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("ResButton",916);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("TrigItem", 304);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("TrigButton",917);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("PrefItem", 308);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("TreeItem", 408);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("TreeButton", 928);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("PrintTreeItem", 1073);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("ImportRBDItem", 110);
         ASK menuItem TO Deactivate;
      WHEN 6:  {new, open, about}
         menuItem := Descendant("NewItem", 101);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("NewButton",901);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("OpenItem", 102);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("OpenButton",902);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("ImportItem",1091);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("AboutItem", 602);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("File1", 115);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("File2", 116);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("File3", 117);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("File4", 118);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("File5", 119);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("File6", 120);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("GotoItem", 403);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("GotoButton", 924); 
         ASK buttItem TO Deactivate;
         menuItem := Descendant("HomeItem",401);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("HomeButton",922);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("UpItem",402);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("UpButton",923);
         ASK buttItem TO Deactivate;
      WHEN 7:  {clear}
         menuItem := Descendant("ClearItem",206);
         ASK menuItem TO Deactivate;
      WHEN 8:  {edit properties}
         buttItem := ASK menuTool Descendant("EditButton",940);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("PropItem",207);                                   
         ASK menuItem TO Deactivate;
      WHEN 9:  {exit}
         menuItem := Descendant("ExitItem", 112);
         ASK menuItem TO Deactivate;
      WHEN 10:
         menuItem := Descendant("CollapseItem", 208);
         ASK menuItem TO Deactivate;
      WHEN 12:  {Select All}
         menuItem := Descendant("ExportItem",1092);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("SelectAllItem",202);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("FindItem",407);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("FindButton",927);
         ASK buttItem TO Deactivate;
      WHEN 13:  {Demo deactivate}
         menuItem := Descendant("SaveItem",103);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("SaveAsItem",104);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("SaveRBDItem",105);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("SaveTableItem", 106);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("PrintSetupItem", 108);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("SaveTXTItem",3017);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("PrintTableItem", 1071);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("PrintItem",1072);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("PrintTreeItem", 1073);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("PrintSelectItem",1074);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("SaveButton",903);
         ASK buttItem TO Deactivate;
         buttItem := ASK menuTool Descendant("PrintButton",904);
         ASK buttItem TO Deactivate;
      WHEN 14:
         menuItem := Descendant("MassEdItem",210);
         ASK menuItem TO Deactivate; 
      WHEN 15:
         menuItem := Descendant("PrintSetupItem",108);
         ASK menuItem TO Deactivate;
      WHEN 16:
         menuItem := Descendant("ConnectorItem",2013);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("LinkButton",910);
         ASK buttItem TO Deactivate;
      WHEN 17:
         menuItem := Descendant("WeakViewItem",503);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("AnalViewButton",931);
         ASK buttItem TO Deactivate;
      WHEN 18: {Can't add/import new hier cuz reached levellimit}
         menuItem := Descendant("HierItem",2015);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("HierButton",912);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("ImportRBDItem", 110);
         ASK menuItem TO Deactivate;
      WHEN 19:
         menuItem := Descendant("HomeItem",401);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("HomeButton",922);
         ASK buttItem TO Deactivate;
         menuItem := Descendant("UpItem",402);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("UpButton",923);
         ASK buttItem TO Deactivate;
         IF totalHiers = 0
            menuItem := Descendant("GotoItem", 403);
            ASK menuItem TO Deactivate;
            buttItem := ASK menuTool Descendant("GotoButton", 924);
            ASK buttItem TO Deactivate;
         END IF;
      OTHERWISE
         NEW(message, 1..1);
         message[1] := "ERROR: Unknown disable condition!     ";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      END CASE;
   END METHOD; {Disable}
   
   ASK METHOD Disable1Thru8;
   BEGIN
      Disable(1);
      Disable(2);
      Disable(3);
      Disable(4);
      Disable(5);
      Disable(6);
      Disable(7);
      Disable(8);
      Disable(10);
      Disable(12);
      Disable(14);
      Disable(15);
   END METHOD; {Disable1Thru8}

   ASK METHOD SetChecks();
   BEGIN
      menuItem := Descendant("WeakAnalItem",3071);
      IF weakAnalysis
         ASK menuItem TO SetLabel("Disable Weak Link");
      ELSE
         ASK menuItem TO SetLabel("Enable Weak Link");
      END IF;
      buttItem := ASK menuTool Descendant("NodeAnalButton",919);
      ASK buttItem TO SetSelected(weakAnalysis);;
      menuItem := Descendant("CapAnalItem",3072);
      IF capacityAnalysis
         ASK menuItem TO SetLabel("Disable Capacity");
      ELSE
         ASK menuItem TO SetLabel("Enable Capacity");
      END IF;
      buttItem := ASK menuTool Descendant("CapButton",920);
      ASK buttItem TO SetSelected(capacityAnalysis);;
      menuItem := Descendant("CostAnalItem",3073);
      IF costAnalysis
         ASK menuItem TO SetLabel("Disable Cost");
      ELSE
         ASK menuItem TO SetLabel("Enable Cost");
      END IF;
      buttItem := ASK menuTool Descendant("CostButton",921);
      ASK buttItem TO SetSelected(costAnalysis);
      checkedWindow := Descendant("HomeItem", 401);
      ASK checkedWindow TO Deactivate;
      buttItem := ASK menuTool Descendant("HomeButton",922);
      ASK buttItem TO Deactivate;
      menuItem := Descendant("UpItem",402);
      ASK menuItem TO Deactivate;
      buttItem := ASK menuTool Descendant("UpButton",923);
      ASK buttItem TO Deactivate;
      IF totalHiers = 0
         menuItem := Descendant("GotoItem", 403);
         ASK menuItem TO Deactivate;
         buttItem := ASK menuTool Descendant("GotoButton", 924);
         ASK buttItem TO Deactivate;
      END IF;
   END METHOD; {Set Checks}

   ASK METHOD Enable(IN greyLevel : INTEGER);
   VAR
      numEvents, deepestDepth, currDepth : INTEGER;
      winMenu : MenuObj;
      block : RBDBlockObj;
      tempHier1, tempHier2 : RBDHierObj;
      obj : ANYOBJ;
   BEGIN
      CASE greyLevel
      WHEN 1:
         IF compileType <> "demo"
            menuItem := Descendant("PrintSelectItem",1074);
            ASK menuItem TO Activate;
         END IF;
         menuItem := Descendant("CutItem",203);
         ASK menuItem TO Activate;
         buttItem := ASK menuTool Descendant("CutButton",905);
         ASK buttItem TO Activate;
         menuItem := Descendant("CopyItem",204);
         ASK menuItem TO Activate;
         buttItem := ASK menuTool Descendant("CopyButton",906);
         ASK buttItem TO Activate;
      WHEN 2:
         IF compileType <> "demo"
            menuItem := Descendant("SaveItem",103);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("SaveButton",903);
            ASK buttItem TO Activate;
            menuItem := Descendant("SaveAsItem",104);
            ASK menuItem TO Activate;
            menuItem := Descendant("PrintItem",1072);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("PrintButton",904);
            ASK buttItem TO Activate;
            menuItem := Descendant("SaveRBDItem",105);
            ASK menuItem TO Activate;
         END IF;
         menuItem := Descendant("BlockInputItem",3011);
         buttItem := ASK menuTool Descendant("ViewInButton",913);
         IF (totalBlocks > 0)
            ASK menuItem TO Activate;
            ASK buttItem TO Activate;
         ELSE
            ASK menuItem TO Deactivate;
            ASK buttItem TO Deactivate;
         END IF;
         menuItem := Descendant("NodeInputItem",3012);
         IF ((totalNodes - totalHiers*2 - 2) > 0)
            ASK menuItem TO Activate;
         ELSE
            ASK menuItem TO Deactivate;
         END IF;
         menuItem := Descendant("EventInputItem",3013);
         IF (totalEvents > 0)
            ASK menuItem TO Activate;
         ELSE
            ASK menuItem TO Deactivate;
         END IF;
         menuItem := Descendant("HierInputItem",3014);
         IF (totalHiers > 0)
            ASK menuItem TO Activate;
         ELSE
            ASK menuItem TO Deactivate;
         END IF;
         menuItem := Descendant("InputItem",3015);
         ASK menuItem TO Activate;
         IF simulated
            menuItem := Descendant("OutputItem",3016);
            ASK menuItem TO Activate;
            IF analViewAvail
               menuItem := Descendant("WeakViewItem",503);
               ASK menuItem TO Activate;
               buttItem := ASK menuTool Descendant("AnalViewButton",931);
               ASK buttItem TO Activate;
            END IF;
            buttItem := ASK menuTool Descendant("ViewOutButton",914);
            ASK buttItem TO Activate;
         END IF;
         IF compileType <> "demo"
            menuItem := Descendant("SaveTXTItem",3017);
            ASK menuItem TO Activate;
            menuItem := Descendant("SaveTableItem", 106);
            ASK menuItem TO Activate;
            menuItem := Descendant("PrintTableItem", 1071);
            ASK menuItem TO Activate;
         END IF;
      WHEN 3:
         menuItem := Descendant("PasteItem",205);
         ASK menuItem TO Activate;
         buttItem := ASK menuTool Descendant("PasteButton",907);
         ASK buttItem TO Activate;
      WHEN 4:
         IF (totalObjects > 2)
            menuItem := Descendant("SimItem",501);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("SimButton",929);
            ASK buttItem TO Activate;
            IF (compileType <> "student") AND (compileType <> "gmd")
               menuItem := Descendant("FEVItem",502);
               ASK menuItem TO Activate;
               buttItem := ASK menuTool Descendant("FEVButton",930);
               ASK buttItem TO Activate;
            END IF;               
         END IF;
         IF compileType <> "student"
            menuItem := Descendant("WeakAnalItem",3071);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("NodeAnalButton",919);
            ASK buttItem TO Activate;
            menuItem := Descendant("CapAnalItem",3072);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("CapButton",920);
            ASK buttItem TO Activate;
            menuItem := Descendant("CostAnalItem",3073);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("CostButton",921);
            ASK buttItem TO Activate;
         END IF;
      WHEN 5:
         menuItem := Descendant("CloseItem",111);
         ASK menuItem TO Activate;
         menuItem := Descendant("BlockItem",2011);
         ASK menuItem TO Activate;
         buttItem := ASK menuTool Descendant("BlockButton",908);
         ASK buttItem TO Activate;
         menuItem := Descendant("NodeItem",2012);
         ASK menuItem TO Activate;
         buttItem := ASK menuTool Descendant("NodeButton",909);
         ASK buttItem TO Activate;
         IF totalObjects > 1
            menuItem := Descendant("ConnectorItem",2013);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("LinkButton",910);
            ASK buttItem TO Activate;
         END IF;
         menuItem := Descendant("EventItem",2014);
         ASK menuItem TO Activate;
         IF ((hierLevel < levelLimit) AND (typeOfCursor <> blockC) AND (typeOfCursor <> nodeC) 
                                      AND (typeOfCursor <> eventC) AND ((typeOfCursor <> hierC) OR collapsing OR importing))
            menuItem := Descendant("HierItem",2015);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("HierButton",912);
            ASK buttItem TO Activate;
         END IF; 
         buttItem := ASK menuTool Descendant("EventButton",911);
         ASK buttItem TO Activate;
         IF cusZoomVal < 118.
            menuItem := Descendant("ZoomOutItem",405);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("ZoomOutButton",925);
            ASK buttItem TO Activate;
         END IF;
         IF cusZoomVal > 4.
            menuItem := Descendant("ZoomInItem",404);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("ZoomInButton",926);
            ASK buttItem TO Activate;
         END IF;
         buttItem := ASK menuTool Descendant("ZoomFitButton", 941);
         ASK buttItem TO Activate;
         IF compileType <> "student"
            menuItem := Descendant("PhaseItem",306);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("PhaseButton",918);
            ASK buttItem TO Activate;
         END IF;
         menuItem := Descendant("SpareItem", 302);
         ASK menuItem TO Activate;
         buttItem := ASK menuTool Descendant("SpareButton", 915);
         ASK buttItem TO Activate;
         menuItem := Descendant("ResItem", 303);
         ASK menuItem TO Activate;
         buttItem := ASK menuTool Descendant("ResButton", 916);
         ASK buttItem TO Activate;
         menuItem := Descendant("TrigItem", 304);
         ASK menuItem TO Activate;
         buttItem := ASK menuTool Descendant("TrigButton", 917);
         ASK buttItem TO Activate;
         menuItem := Descendant("PrefItem", 308);
         ASK menuItem TO Activate;
         menuItem := Descendant("DefaultsItem",209);
         ASK menuItem TO Activate;
         buttItem := ASK menuTool Descendant("TreeButton", 928);
         ASK buttItem TO Activate;
         menuItem := Descendant("TreeItem", 408);
         ASK menuItem TO Activate;
         IF compileType <> "demo"
            menuItem := Descendant("PrintTreeItem", 1073);
            ASK menuItem TO Activate;
         END IF;
         menuItem := Descendant("ImportRBDItem", 110);
         ASK menuItem TO Activate;
      WHEN 6:
         menuItem := Descendant("NewItem", 101);
         ASK menuItem TO Activate;
         buttItem := ASK menuTool Descendant("NewButton",901);
         ASK buttItem TO Activate;
         menuItem := Descendant("OpenItem", 102);
         ASK menuItem TO Activate;
         buttItem := ASK menuTool Descendant("OpenButton",902);
         ASK buttItem TO Activate;
         menuItem := Descendant("ImportItem", 1091);
         ASK menuItem TO Activate;
         menuItem := Descendant("AboutItem", 602);
         ASK menuItem TO Activate;
         menuItem := Descendant("File1", 115);
         ASK menuItem TO Activate;
         menuItem := Descendant("File2", 116);
         ASK menuItem TO Activate;
         menuItem := Descendant("File3", 117);
         ASK menuItem TO Activate;
         menuItem := Descendant("File4", 118);
         ASK menuItem TO Activate;
         menuItem := Descendant("File5", 119);
         ASK menuItem TO Activate;
         menuItem := Descendant("File6", 120);
         ASK menuItem TO Activate;
         IF totalHiers > 0
            menuItem := Descendant("GotoItem", 403);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("GotoButton", 924); 
            ASK buttItem TO Activate;
         END IF;            
         IF activeWindow > 0
            menuItem := Descendant("HomeItem",401);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("HomeButton",922);
            ASK buttItem TO Activate;
            menuItem := Descendant("UpItem",402);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("UpButton",923);
            ASK buttItem TO Activate;
         END IF;
      WHEN 7:
         menuItem := Descendant("ClearItem",206);
         ASK menuItem TO Activate;
      WHEN 8:
         menuItem := Descendant("PropItem",207);
         ASK menuItem TO Activate;
         buttItem := ASK menuTool Descendant("EditButton",940);
         ASK buttItem TO Activate;
      WHEN 9:
         menuItem := Descendant("ExitItem", 112);
         ASK menuItem TO Activate;
      WHEN 10:
         menuItem := Descendant("CollapseItem", 208);
         ASK menuItem TO Activate;
         IF (hierLevel >= levelLimit)
            ASK menuItem TO Deactivate;
         ELSIF ((hiersIn > 0) AND (hierLevel = (levelLimit - 1))) {collapsing hier at next to bottom level}
            ASK menuItem TO Deactivate;
         ELSIF (hiersIn > 0) {collapsing hier with child that is also a hier}
            deepestDepth := 0;
            FOREACH obj IN selectGroup
               IF OBJTYPENAME(obj) = "RBDHierObj"
                  tempHier1 := RBDHierObj(obj);
                  ASK tempHier1 TO CalculateDepth(currDepth);
                  IF currDepth > deepestDepth
                     deepestDepth := currDepth;
                  END IF;
               END IF;
            END FOREACH;
            IF (deepestDepth >= levelLimit)
            ASK menuItem TO Deactivate;
            END IF;
         END IF;
      WHEN 12:
         IF totalObjects > 0
            menuItem := Descendant("ExportItem",1092);
            ASK menuItem TO Activate;
            menuItem := Descendant("SelectAllItem",202);
            ASK menuItem TO Activate;
            menuItem := Descendant("FindItem",407);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("FindButton",927);
            ASK buttItem TO Activate;
         END IF;
      WHEN 14:
         menuItem := Descendant("MassEdItem",210);
         ASK menuItem TO Activate;
      WHEN 15:
         IF compileType <> "demo"
            menuItem := Descendant("PrintSetupItem",108);
            ASK menuItem TO Activate;
         END IF;
      WHEN 18:
         IF ((typeOfCursor <> blockC) AND (typeOfCursor <> nodeC) AND (typeOfCursor <> eventC) AND (typeOfCursor <> hierC))
            menuItem := Descendant("HierItem",2015);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("HierButton",912);
            ASK buttItem TO Activate;
            menuItem := Descendant("ImportRBDItem", 110);
            ASK menuItem TO Activate;
         END IF;
      WHEN 19:
         IF activeWindow > 0
            menuItem := Descendant("HomeItem",401);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("HomeButton",922);
            ASK buttItem TO Activate;
            menuItem := Descendant("UpItem",402);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("UpButton",923);
            ASK buttItem TO Activate;
         END IF;
         IF totalHiers > 0
            menuItem := Descendant("GotoItem", 403);
            ASK menuItem TO Activate;
            buttItem := ASK menuTool Descendant("GotoButton", 924);
            ASK buttItem TO Activate;
         END IF;
      OTHERWISE
         NEW(message, 1..1);
         message[1] := "ERROR: Unknown enable condition!     ";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      END CASE;
   END METHOD; {Enable}

   ASK METHOD Enable2Thru5;
   BEGIN
      IF totalObjects > 0
         Enable(2);
         Enable(4);
      END IF;
      IF copied
         Enable(3);
      END IF;
      Enable(5);
   END METHOD; {Enable2Thru5}

   ASK METHOD Enable2Thru6;
   BEGIN
      Enable(2);
      IF copied
         Enable(3);
      END IF;
      Enable(4);
      Enable(5);
      Enable(6);
      Enable(12);
      Enable(15);
   END METHOD; {Enable2Thru6}

   ASK METHOD AddFileName(IN path, file : STRING);
   VAR
      length         : INTEGER;
      defaultPath    : STRING;
   BEGIN
      IF ((LOWER(file) <> "unnamed rbd") AND (file <> "NoFile") AND (file <> "cksrule.rbd") AND (LOWER(file) <> "unnamed.rbd"))
         IF (path+file = path1+file1)
            ;
         ELSIF (LOWER(path+file) = LOWER(path2+file2))
            path2 := path1;
            file2 := file1;
            path1 := path;
            file1 := file;
         ELSIF (LOWER(path+file) = LOWER(path3+file3))
            path3 := path2;
            file3 := file2;
            path2 := path1;
            file2 := file1;
            path1 := path;
            file1 := file;
         ELSIF (LOWER(path+file) = LOWER(path4+file4))
            path4 := path3;
            file4 := file3;
            path3 := path2;
            file3 := file2;
            path2 := path1;
            file2 := file1;
            path1 := path;
            file1 := file;
         ELSIF (LOWER(path+file) = LOWER(path5+file5))
            path5 := path4;
            file5 := file4;
            path4 := path3;
            file4 := file3;
            path3 := path2;
            file3 := file2;
            path2 := path1;
            file2 := file1;
            path1 := path;
            file1 := file;
         ELSE
            path6 := path5;
            file6 := file5;
            path5 := path4;
            file5 := file4;
            path4 := path3;
            file4 := file3;
            path3 := path2;
            file3 := file2;
            path2 := path1;
            file2 := file1;
            path1 := path;
            file1 := file;
         END IF;
         length := STRLEN(file1);      
         menuItem := Descendant("File1", 115);
         ASK menuItem TO SetLabel(path1+SUBSTR(1, (length-4), file1));
         length := STRLEN(file2);      
         menuItem := Descendant("File2", 116);
         ASK menuItem TO SetLabel(path2+SUBSTR(1, (length-4), file2));
         length := STRLEN(file3);      
         menuItem := Descendant("File3", 117);
         ASK menuItem TO SetLabel(path3+SUBSTR(1, (length-4), file3));
         length := STRLEN(file4);      
         menuItem := Descendant("File4", 118);
         ASK menuItem TO SetLabel(path4+SUBSTR(1, (length-4), file4));
         length := STRLEN(file5);      
         menuItem := Descendant("File5", 119);
         ASK menuItem TO SetLabel(path5+SUBSTR(1, (length-4), file5));
         length := STRLEN(file6);      
         menuItem := Descendant("File6", 120);
         ASK menuItem TO SetLabel(path6+SUBSTR(1, (length-4), file6));
         { defaultPath := GetProgDir("Raptor7.exe"); }   { wds/TES, 8/18/08 }
         SavePathsCFG(userPath);   { wds/TES, 8/18/08 }
         Update;
      END IF;
   END METHOD; {AddFileName}
   
   ASK METHOD SetFileNames(IN defPath1, defFile1, defPath2, defFile2,
                              defPath3, defFile3, defPath4, defFile4,
                              defPath5, defFile5, defPath6, defFile6  : STRING);
   VAR
      dotPosition : INTEGER;
   BEGIN      
      path1 := defPath1;
      file1 := defFile1;
      path2 := defPath2;
      file2 := defFile2;
      path3 := defPath3;
      file3 := defFile3;
      path4 := defPath4;
      file4 := defFile4;
      path5 := defPath5;
      file5 := defFile5;
      path6 := defPath6;
      file6 := defFile6;
      dotPosition := POSITION(defFile1, ".");
      menuItem := Descendant("File1", 115);
      ASK menuItem TO SetLabel(defPath1+SUBSTR(1, dotPosition-1, defFile1));
      dotPosition := POSITION(defFile2, ".");
      menuItem := Descendant("File2", 116);
      ASK menuItem TO SetLabel(defPath2+SUBSTR(1, dotPosition-1, defFile2));
      dotPosition := POSITION(defFile3, ".");
      menuItem := Descendant("File3", 117);
      ASK menuItem TO SetLabel(defPath3+SUBSTR(1, dotPosition-1, defFile3));
      dotPosition := POSITION(defFile4, ".");
      menuItem := Descendant("File4", 118);
      ASK menuItem TO SetLabel(defPath4+SUBSTR(1, dotPosition-1, defFile4));
      dotPosition := POSITION(defFile5, ".");
      menuItem := Descendant("File5", 119);
      ASK menuItem TO SetLabel(defPath5+SUBSTR(1, dotPosition-1, defFile5));
      dotPosition := POSITION(defFile6, ".");
      menuItem := Descendant("File6", 120);
      ASK menuItem TO SetLabel(defPath6+SUBSTR(1, dotPosition-1, defFile6));
      Update;
   END METHOD; {SetFileNames}

   ASK METHOD SetZoomButts;
   BEGIN      
      buttItem := ASK menuTool Descendant("ZoomInButton",926);
      menuItem := Descendant("ZoomInItem", 404);
      IF cusZoomVal > 4. 
         ASK buttItem TO Activate;
         ASK menuItem TO Activate;
      ELSE
         ASK buttItem TO Deactivate;
         ASK menuItem TO Deactivate;
      END IF;
      buttItem := ASK menuTool Descendant("ZoomOutButton",925);
      menuItem := Descendant("ZoomOutItem", 405);
      IF cusZoomVal < 118. 
         ASK buttItem TO Activate;
         ASK menuItem TO Activate;
      ELSE
         ASK buttItem TO Deactivate;
         ASK buttItem TO Deactivate;
      END IF;
   END METHOD;
END OBJECT; {mainMenuObj}

OBJECT mainToolObj;
   ASK METHOD BeSelected;
   VAR
      saveCancelled, validRBD        : BOOLEAN;
      node                           : RBDNodeObj;
      blockInputBox : BlockInputBoxObj;
      tabOutBox                      : TablesOutObj;
      tempPath, tempName             : STRING;
      printTablesBox                 : PrintTablesBoxObj;
      trigBox                        : TrigBoxObj;
      phaseBox                       : PhaseBoxObj;
      sparePoolsBox                  : SparePoolsBoxObj;
      resPoolsBox                    : ResPoolsBoxObj;
   BEGIN
      IF ignoreMouse {eag error 51 fix}
         RETURN;
      END IF;
      IF popupMenu.Visible
         ASK window TO KillPopup;
      END IF;
      IF (compileType <> "demo") AND saveIsOn AND ((TRUNC(ClockRealSecs)-lastSave) >= saveInc) AND (totalBlocks > 1)
         AND (typeOfCursor = nilC) AND (NOT pastingMultiple)
         lastSave := TRUNC(ClockRealSecs);
         typeOfCursor := autoC;
         ASK window TO ShowStatus(0,"Autosaving...");
         SaveFile("cksrule.rbd", exampPath, "*.rbd", totalBlocks, totalNodes, totalLinks,totalHiers,totalEvents);
         typeOfCursor := nilC;
         ASK window TO ShowStatus(0,"");
      END IF; 
      IF (typeOfCursor <> autoC) AND (LastPicked <> NILOBJ)
         CASE ASK LastPicked Id
            WHEN 901: {New}
               ResetNewFile;
            WHEN 902: {Open}
               ClearAllBlocks;
               ASK menubar TO Disable1Thru8;
               menuPath := pathName;
               menuFile := nameOfFile;
               statusBarOn  := TRUE;
               openMenuFile := FALSE;
               OpenFile(FALSE, FALSE, totalBlocks, totalNodes, totalLinks, totalHiers, totalEvents, 0, gridIsOn, fileIsOpen,
                        nameOfFile, pathName, filter, saveCancelled);
               IF NOT saveCancelled
                  activeWindow := 0;
                  hierLevel := 0;
                  CheckOpenFileStatus;
                  ASK menubar TO SetChecks;
                  IF copied
                     ASK menubar TO Enable(3);
                     ResetPasted;
                  END IF;
               ELSE
                  ASK menubar TO Enable2Thru6;
               END IF;
               analViewAvail := FALSE;
               configFrozen := FALSE;
               typeOfCursor := nilC;
               ASK window TO Update;
            WHEN 903: {Save}
               ClearAllBlocks;
               ASK menubar TO Disable1Thru8;
               typeOfCursor := dialogC;
               IF fileIsOpen
                  tempPath := pathName;
                  tempName := nameOfFile;
                  IF (nameOfFile = "Unnamed RBD") OR (nameOfFile = "NoFile")
                     SaveAsFile(nameOfFile, pathName, filter, "Save RBD File");
                  END IF;
                  IF nameOfFile <> "NoFile"
                     SaveFile(nameOfFile, pathName, filter, totalBlocks, totalNodes, totalLinks,
                               totalHiers, totalEvents);
                  ELSE
                     pathName := tempPath;
                     nameOfFile := tempName;
                  END IF;
               END IF;
               typeOfCursor := nilC;
               ASK menubar TO Enable2Thru6;
            WHEN 904: {Print Window}
               PrintWindow;
            WHEN 905: {Cut}
               CopyObject;
               ClearObject;
            WHEN 906: {Copy}
               CopyObject; 
            WHEN 907: {Paste}
               PasteObject;
            WHEN 940: {Edit Properties}
               EditDetails;
            WHEN 908: {Add Block}
               AddBlock;
            WHEN 909: {Add Node}
               AddNode;
            WHEN 910: {Add Connector}
               NEW(linkCursor);
               ASK root TO AddGraphic(linkCursor);
               AddConnector;
               IF linkCancelled
                  DISPOSE(linkCursor);
                  ASK menubar TO Enable(9);
                  linkCancelled := FALSE;
               END IF;
            WHEN 911: {Add Event}
               AddEvent;
            WHEN 912: {Add Hier}
               AddHier;
            WHEN 913: {View Block Input Tables}
               ClearAllBlocks;
               typeOfCursor := dialogC;            
               NEW(blockInputBox);
               ASK blockInputBox TO LoadFromLibrary(dialogs, "BlockInputBox");
               ASK window TO AddGraphic(blockInputBox);
               ASK blockInputBox TO InitSelf(FALSE);
               button := ASK blockInputBox TO AcceptInput();
               typeOfCursor := nilC;
               DISPOSE(blockInputBox);
               buttItem := Descendant("ViewInButton",913);
               ASK buttItem TO SetSelected(FALSE);
               ASK buttItem TO Draw;
            WHEN 914: {View Output Tables}
               ClearAllBlocks;
               typeOfCursor := dialogC;            
               NEW(tabOutBox);
               ASK tabOutBox TO LoadFromLibrary(dialogs, "ViewOutputBox");
               ASK window TO AddGraphic(tabOutBox);
               ASK tabOutBox TO InitSelf(FALSE);
               button := ASK tabOutBox TO AcceptInput();
               typeOfCursor := nilC;
               DISPOSE(tabOutBox);  
               buttItem := Descendant("ViewOutButton",914);
               ASK buttItem TO SetSelected(FALSE);
               ASK buttItem TO Draw;
            WHEN 915: {Spares}
               NEW(sparePoolsBox);
               ASK sparePoolsBox TO LoadFromLibrary(dialogs, "SparePoolsBox");
               ASK window TO AddGraphic(sparePoolsBox);
               ASK sparePoolsBox TO ReceiveData;
               DISPOSE(sparePoolsBox);
            WHEN 916: {Resources}
               NEW(resPoolsBox);
               ASK resPoolsBox TO LoadFromLibrary(dialogs, "ResPoolsBox");
               ASK window TO AddGraphic(resPoolsBox);
               ASK resPoolsBox TO ReceiveData;
               DISPOSE(resPoolsBox);
            WHEN 917: {Triggers}
               NEW(trigBox);
               ASK trigBox TO LoadFromLibrary(dialogs, "TrigBox");
               ASK window TO AddGraphic(trigBox);
               ASK trigBox TO ReceiveData;
               DISPOSE(trigBox);
            WHEN 918: {Phasing}
               typeOfCursor := dialogC;            
               NEW(phaseBox);
               ASK phaseBox TO LoadFromLibrary(dialogs, "PhaseBox");
               ASK window TO AddGraphic(phaseBox);
               ASK phaseBox TO InitSelf;
               button := ASK phaseBox TO AcceptInput();      
               typeOfCursor := nilC;
               DISPOSE(phaseBox);
            WHEN 919: {Node Analysis}
               menuItem := ASK menubar Descendant("WeakAnalItem",3071);
               IF weakAnalysis
                  ASK menuItem TO SetLabel("Enable Weak Link");
                  weakAnalysis := FALSE;
               ELSE
                  ASK menuItem TO SetLabel("Disable Weak Link");
                  weakAnalysis := TRUE;
               END IF;
               ASK menuItem TO Draw;
            WHEN 920: {Capacity}
               menuItem := ASK menubar Descendant("CapAnalItem",3072);
               IF capacityAnalysis
                  ASK menuItem TO SetLabel("Enable Capacity");
                  capacityAnalysis := FALSE;
               ELSE
                  ASK menuItem TO SetLabel("Disable Capacity");
                  capacityAnalysis := TRUE;
               END IF;
               ASK menuItem TO Draw;
            WHEN 921: {Cost Analysis}
               menuItem := ASK menubar Descendant("CostAnalItem",3073);
               IF costAnalysis
                  ASK menuItem TO SetLabel("Enable Cost");
                  costAnalysis := FALSE;
               ELSE
                  ASK menuItem TO SetLabel("Disable Cost");
                  costAnalysis := TRUE;
               END IF;
               ASK menuItem TO Draw;
            WHEN 922:  {Home}
               Home;
            WHEN 923:  {Up}
               UpOneLevel;
            WHEN 924: {Goto}
               GotoHier;
            WHEN 925: {Zoom Out}
               Zoom("out");
            WHEN 926: {Zoom In}
               Zoom("in");
            WHEN 941: {Zoom Fit}
               ZoomFit;
            WHEN 927: {Find Item}
               FindItem;
            WHEN 928: {Display Tree}
               DisplayTree;
            WHEN 929: {Simulate}
               ValidateRBD(validRBD);
               IF validRBD
                  currentView := "simulation";
                  SendToEngine;
               END IF;
            WHEN 930:
               ValidateRBD(validRBD);
               IF validRBD
                  currentView := "fev";
                  StartFailureEffects;
               END IF; 
            WHEN 931: {Show Node Anal View}
               {changedZoom := FALSE;}
               notSimming := TRUE;
               currentView := "weaklink";
               ShowAnalView(TRUE);
            WHEN 932: {Help}
               CallHelp(0);
            OTHERWISE;
         END CASE;
      END IF;
   END METHOD; {BeSelected}

   ASK METHOD SetNavigation;
   BEGIN
      IF ((totalHiers > 0) AND (typeOfCursor <> blockC) AND (typeOfCursor <> nodeC) 
                           AND (typeOfCursor <> eventC) AND (typeOfCursor <> hierC))
         buttItem := Descendant("GotoButton", 924);
         ASK buttItem TO Activate;
      ELSE
         buttItem := Descendant("GotoButton", 924);
         ASK buttItem TO Deactivate;
      END IF;
      IF activeWindow = 0
         buttItem := Descendant("HomeButton",922);
         ASK buttItem TO Deactivate;
         buttItem := Descendant("UpButton",923);
         ASK buttItem TO Deactivate;
      ELSIF ((typeOfCursor <> blockC) AND (typeOfCursor <> nodeC) AND (typeOfCursor <> eventC) AND (typeOfCursor <> hierC))
         buttItem := Descendant("HomeButton",922);
         ASK buttItem TO Activate;
         buttItem := Descendant("UpButton",923);
         ASK buttItem TO Activate;
      END IF;
   END METHOD;     
END OBJECT; {mainToolObj}

{*** Graph Menu ***}
OBJECT graphMenuObj;
   ASK METHOD BeSelected;
   VAR
      errors                    : INTEGER;
      valuesOK                  : BOOLEAN;
      setYBox                   : HelpBoxObj;      
      yLowVal, yHiVal           : ValueBoxObj;
      button                    : ButtonObj;
      text                      : ARRAY INTEGER OF STRING;
      printItem                 : PrinterObj;
      leftDot, selected, upDot  : ImageObj;
      dupedGraph                : ChartObj;
      graphTitle, dateItem      : TextObj;
      date                      : STRING;
   BEGIN
      CASE ASK LastPicked Id
         {*** File Menu ***}
         WHEN 1011: {Print Per Trial Ao Graph}
            NEW(selected);
            ASK selected TO SetWorld(0.,0.,1000.,770.);
            NEW(printItem);
            NEW(leftDot);
            NEW(upDot);
            NEW(graphTitle);
            NEW(dateItem);
            ASK printItem TO SetUseDialog(FALSE);
            ASK printItem TO SetOrientation(LandscapeOrientation);
            ASK leftDot TO LoadFromLibrary(images, "WhiteDot"); 
            ASK selected TO AddGraphic(leftDot);
            ASK upDot TO LoadFromLibrary(images, "WhiteDot"); 
            ASK selected TO AddGraphic(upDot);
            dupedGraph := CLONE(AoGraph);
            ASK dupedGraph TO SetHidden(FALSE);
            ASK dupedGraph TO SetTranslation(50.,200.);
            ASK dupedGraph TO SetPartTextSysFont(ChartXTitle,1,"Times New Roman",9,75,0);
            ASK dupedGraph TO SetPartTextSysFont(ChartYTitle,1,"Times New Roman",9,75,0);  
            ASK selected TO AddGraphic(dupedGraph);
            ASK leftDot TO SetTranslation(0.,0.);
            ASK upDot TO SetTranslation(990.,610.);
            ASK graphTitle TO SetText("Ao Graph  for  " + nameOfFile);
            ASK graphTitle TO SetFont(SystemText);
            ASK graphTitle TO SetSysFont("Times New Roman",11,75,0);
            ASK graphTitle TO SetTranslation(420.,440.);
            ASK selected TO AddGraphic(graphTitle);
            ASK graphTitle TO SetColor(Black);
            DateTime(date);
            ASK dateItem TO SetText(date);
            ASK dateItem TO SetFont(SystemText);
            ASK dateItem TO SetSysFont("Times New Roman",8,50,0);
            ASK dateItem TO SetTranslation(800.,50.);
            ASK selected TO AddGraphic(dateItem);
            ASK dateItem TO SetColor(Black);
            ASK root TO AddGraphic(selected);
            ASK selected TO Draw;
            ASK printItem TO Print(selected);
            DISPOSE(selected);;
            DISPOSE(printItem);
         WHEN 1012: {Print Interval Ao Graph}
            NEW(selected);
            ASK selected TO SetWorld(0.,0.,1000.,770.);
            NEW(printItem);
            NEW(leftDot);
            NEW(upDot);
            NEW(graphTitle);
            NEW(dateItem);
            ASK printItem TO SetUseDialog(FALSE);
            ASK printItem TO SetOrientation(LandscapeOrientation);
            ASK leftDot TO LoadFromLibrary(images, "WhiteDot"); 
            ASK selected TO AddGraphic(leftDot);
            ASK upDot TO LoadFromLibrary(images, "WhiteDot"); 
            ASK selected TO AddGraphic(upDot);
            dupedGraph := CLONE(IntAoGraph);
            ASK dupedGraph TO SetHidden(FALSE);
            ASK dupedGraph TO SetTranslation(50.,200.);
            ASK dupedGraph TO SetPartTextSysFont(ChartXTitle,1,"Times New Roman",9,75,0);
            ASK dupedGraph TO SetPartTextSysFont(ChartYTitle,1,"Times New Roman",9,75,0);  
            ASK selected TO AddGraphic(dupedGraph);
            ASK leftDot TO SetTranslation(0.,0.);
            ASK upDot TO SetTranslation(990.,610.);
            ASK graphTitle TO SetText("Interval Ao Graph  for  " + nameOfFile);
            ASK graphTitle TO SetFont(SystemText);
            ASK graphTitle TO SetSysFont("Times New Roman",11,75,0);
            ASK graphTitle TO SetTranslation(400.,440.);
            ASK selected TO AddGraphic(graphTitle);
            ASK graphTitle TO SetColor(Black);
            DateTime(date); 
            ASK dateItem TO SetText(date);
            ASK dateItem TO SetFont(SystemText);
            ASK dateItem TO SetSysFont("Times New Roman",8,50,0);
            ASK dateItem TO SetTranslation(800.,50.);
            ASK selected TO AddGraphic(dateItem);
            ASK dateItem TO SetColor(Black);
            ASK root TO AddGraphic(selected);
            ASK selected TO Draw;
            ASK printItem TO Print(selected);
            DISPOSE(selected);;
            DISPOSE(printItem);
         WHEN 1013: {Print Multi-run Ao Graph}
            NEW(selected);
            ASK selected TO SetWorld(0.,0.,1000.,770.);
            NEW(printItem);
            NEW(leftDot);
            NEW(upDot);
            NEW(graphTitle);
            NEW(dateItem);
            ASK printItem TO SetUseDialog(FALSE);
            ASK printItem TO SetOrientation(LandscapeOrientation);
            ASK leftDot TO LoadFromLibrary(images, "WhiteDot"); 
            ASK selected TO AddGraphic(leftDot);
            ASK upDot TO LoadFromLibrary(images, "WhiteDot"); 
            ASK selected TO AddGraphic(upDot);
            dupedGraph := CLONE(MultiRunGraph);
            ASK dupedGraph TO SetHidden(FALSE);
            ASK dupedGraph TO SetTranslation(50.,200.);
            ASK dupedGraph TO SetPartTextSysFont(ChartXTitle,1,"Times New Roman",9,75,0);
            ASK dupedGraph TO SetPartTextSysFont(ChartYTitle,1,"Times New Roman",9,75,0);  
            ASK selected TO AddGraphic(dupedGraph);
            ASK leftDot TO SetTranslation(0.,0.);
            ASK upDot TO SetTranslation(990.,610.);
            ASK graphTitle TO SetText("Multi-Run Ao Graph  for  " + nameOfFile);
            ASK graphTitle TO SetFont(SystemText);
            ASK graphTitle TO SetSysFont("Times New Roman",11,75,0);
            ASK graphTitle TO SetTranslation(400.,440.);
            ASK selected TO AddGraphic(graphTitle);
            ASK graphTitle TO SetColor(Black);
            DateTime(date);
            ASK dateItem TO SetText(date);
            ASK dateItem TO SetFont(SystemText);
            ASK dateItem TO SetSysFont("Times New Roman",8,50,0);
            ASK dateItem TO SetTranslation(800.,50.);
            ASK selected TO AddGraphic(dateItem);
            ASK dateItem TO SetColor(Black);
            ASK root TO AddGraphic(selected);
            ASK selected TO Draw;
            ASK printItem TO Print(selected);
            DISPOSE(selected);;
            DISPOSE(printItem);
         WHEN 1014: {Print All Graphs}
            NEW(selected);
            ASK selected TO SetWorld(0.,0.,1000.,770.);
            NEW(printItem);
            NEW(leftDot);
            NEW(upDot);
            NEW(graphTitle);
            NEW(dateItem);
            ASK printItem TO SetUseDialog(FALSE);
            ASK printItem TO SetOrientation(LandscapeOrientation);
            ASK leftDot TO LoadFromLibrary(images, "WhiteDot"); 
            ASK selected TO AddGraphic(leftDot);
            ASK upDot TO LoadFromLibrary(images, "WhiteDot"); 
            ASK selected TO AddGraphic(upDot);
            dupedGraph := CLONE(AoGraph);
            ASK dupedGraph TO SetHidden(FALSE);
            ASK dupedGraph TO SetTranslation(50.,200.);
            ASK dupedGraph TO SetPartTextSysFont(ChartXTitle,1,"Times New Roman",9,75,0);
            ASK dupedGraph TO SetPartTextSysFont(ChartYTitle,1,"Times New Roman",9,75,0);  
            ASK selected TO AddGraphic(dupedGraph);
            ASK leftDot TO SetTranslation(0.,0.);
            ASK upDot TO SetTranslation(990.,610.);
            ASK graphTitle TO SetText("Ao Graph  for  " + nameOfFile);
            ASK graphTitle TO SetFont(SystemText);
            ASK graphTitle TO SetSysFont("Times New Roman",11,75,0);
            ASK graphTitle TO SetTranslation(420.,440.);
            ASK selected TO AddGraphic(graphTitle);
            ASK graphTitle TO SetColor(Black);
            DateTime(date);
            ASK dateItem TO SetText(date);
            ASK dateItem TO SetFont(SystemText);
            ASK dateItem TO SetSysFont("Times New Roman",8,50,0);
            ASK dateItem TO SetTranslation(800.,50.);
            ASK selected TO AddGraphic(dateItem);
            ASK dateItem TO SetColor(Black);
            ASK root TO AddGraphic(selected);
            ASK selected TO Draw;
            ASK printItem TO Print(selected);
            DISPOSE(selected);;
            DISPOSE(printItem);
            NEW(selected);
            ASK selected TO SetWorld(0.,0.,1000.,770.);
            NEW(printItem);
            NEW(leftDot);
            NEW(upDot);
            NEW(graphTitle);
            NEW(dateItem);
            ASK printItem TO SetUseDialog(FALSE);
            ASK printItem TO SetOrientation(LandscapeOrientation);
            ASK leftDot TO LoadFromLibrary(images, "WhiteDot"); 
            ASK selected TO AddGraphic(leftDot);
            ASK upDot TO LoadFromLibrary(images, "WhiteDot"); 
            ASK selected TO AddGraphic(upDot);
            dupedGraph := CLONE(IntAoGraph);
            ASK dupedGraph TO SetHidden(FALSE);
            ASK dupedGraph TO SetTranslation(50.,200.);
            ASK dupedGraph TO SetPartTextSysFont(ChartXTitle,1,"Times New Roman",9,75,0);
            ASK dupedGraph TO SetPartTextSysFont(ChartYTitle,1,"Times New Roman",9,75,0);  
            ASK selected TO AddGraphic(dupedGraph);
            ASK leftDot TO SetTranslation(0.,0.);
            ASK upDot TO SetTranslation(990.,610.);
            ASK graphTitle TO SetText("Interval Ao Graph  for  " + nameOfFile);
            ASK graphTitle TO SetFont(SystemText);
            ASK graphTitle TO SetSysFont("Times New Roman",11,75,0);
            ASK graphTitle TO SetTranslation(400.,440.);
            ASK selected TO AddGraphic(graphTitle);
            ASK graphTitle TO SetColor(Black);
            DateTime(date);
            ASK dateItem TO SetText(date);
            ASK dateItem TO SetFont(SystemText);
            ASK dateItem TO SetSysFont("Times New Roman",8,50,0);
            ASK dateItem TO SetTranslation(800.,50.);
            ASK selected TO AddGraphic(dateItem);
            ASK dateItem TO SetColor(Black);
            ASK root TO AddGraphic(selected);
            ASK selected TO Draw;
            ASK printItem TO Print(selected);
            DISPOSE(selected);;
            DISPOSE(printItem);
            NEW(selected);
            ASK selected TO SetWorld(0.,0.,1000.,770.);
            NEW(printItem);
            NEW(leftDot);
            NEW(upDot);
            NEW(graphTitle);
            NEW(dateItem);
            ASK printItem TO SetUseDialog(FALSE);
            ASK printItem TO SetOrientation(LandscapeOrientation);
            ASK leftDot TO LoadFromLibrary(images, "WhiteDot"); 
            ASK selected TO AddGraphic(leftDot);
            ASK upDot TO LoadFromLibrary(images, "WhiteDot"); 
            ASK selected TO AddGraphic(upDot);
            dupedGraph := CLONE(MultiRunGraph);
            ASK dupedGraph TO SetHidden(FALSE);
            ASK dupedGraph TO SetTranslation(50.,200.);
            ASK dupedGraph TO SetPartTextSysFont(ChartXTitle,1,"Times New Roman",9,75,0);
            ASK dupedGraph TO SetPartTextSysFont(ChartYTitle,1,"Times New Roman",9,75,0);  
            ASK selected TO AddGraphic(dupedGraph);
            ASK leftDot TO SetTranslation(0.,0.);
            ASK upDot TO SetTranslation(990.,610.);
            ASK graphTitle TO SetText("Multi-Run Ao Graph  for  " + nameOfFile);
            ASK graphTitle TO SetFont(SystemText);
            ASK graphTitle TO SetSysFont("Times New Roman",11,75,0);
            ASK graphTitle TO SetTranslation(400.,440.);
            ASK selected TO AddGraphic(graphTitle);
            ASK graphTitle TO SetColor(Black);
            DateTime(date);
            ASK dateItem TO SetText(date);
            ASK dateItem TO SetFont(SystemText);
            ASK dateItem TO SetSysFont("Times New Roman",8,50,0);
            ASK dateItem TO SetTranslation(800.,50.);
            ASK selected TO AddGraphic(dateItem);
            ASK dateItem TO SetColor(Black);
            ASK root TO AddGraphic(selected);
            ASK selected TO Draw;
            ASK printItem TO Print(selected);
            DISPOSE(selected);;
            DISPOSE(printItem);
         WHEN 102: {Print Setup}
            PrintSetup;
         WHEN 103: {Remove Graph}
            DisplayAoGraph := FALSE;
            AvailGraph := FALSE;
            ASK window TO SetDeferral(TRUE);
            ASK window TO SetSize(100., 100.);
            ASK window TO SetTranslation(0., 0.);
            SetView(cusZoomVal, simZoomX,simZoomY);
            ASK window TO SetDeferral(FALSE);
            ASK window TO Update;
            DISPOSE(AoGraph);
            DISPOSE(IntAoGraph);
            DISPOSE(MultiRunGraph);
            DISPOSE(AoGraphWindow);
            IF dSimWithGraph
               ASK greenFace TO SetTranslation(19.+symbolOffset,635.-symbolOffset);
               ASK yellowFace TO SetTranslation(19.+symbolOffset,635.-symbolOffset);
               ASK redFace TO SetTranslation(19.+symbolOffset,635.-symbolOffset);
               ASK faceVisible TO Draw;
            END IF;
         {*** Control Menu ***}
         WHEN 201: {Set Y-Axis}
            NEW(text,1..3);
            NEW(setYBox);
            ASK setYBox TO LoadFromLibrary(dialogs,"YAxisBox");
            ASK window TO AddGraphic(setYBox);
            yLowVal := ASK setYBox Descendant("YAxisMin", 101);
           { yHiVal  := ASK setYBox Descendant("YAxisMax", 100);}
            ASK yLowVal TO SetValue(yMin);
           { ASK yHiVal  TO SetValue(yMax);}
            REPEAT
               ASK setYBox TO Draw;
               valuesOK := FALSE;
               button := ASK setYBox TO AcceptInput();
               IF button.ReferenceName = "OKButton"
                  errors := 0;
                  IF yLowVal.Value() < 0.0
                     INC(errors);
                     text[errors] := "Y-axis minimum must be greater than or equal to 0.0.   ";
                     ASK yLowVal TO SetValue(0.0);
                  ELSIF yLowVal.Value() > 0.999999
                     INC(errors);
                     text[errors] := "Y-axis minimum must be less than 1.0.   ";
                     ASK yLowVal TO SetValue(0.999999);
                  END IF;
                  {IF yHiVal.Value() > 1.0
                     INC(errors);
                     text[errors] := "Y-axis maximum must be less than or equal to 1.0.    ";
                     ASK yHiVal TO SetValue(1.0);
                  ELSIF yHiVal.Value() < 0.000001
                     INC(errors);
                     text[errors] := "Y-axis maximum must be greater than 0.0.    ";
                     ASK yHiVal TO SetValue(0.000001);
                  END IF;
                  IF yLowVal.Value() >= yHiVal.Value()
                     INC(errors);
                     text[errors] := "Y-axis minimum must be less than Y-axis maximum.     ";
                     ASK yLowVal TO SetValue(yHiVal.Value()-.1);
                  END IF;}
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
                  ELSE
                     valuesOK := TRUE;
                  END IF;
               ELSE
                  valuesOK := TRUE;
               END IF;
            UNTIL valuesOK;
            IF button.ReferenceName = "OKButton"
               IF yMin <> yLowVal.Value()
                  yMin := yLowVal.Value();
                  ASK AoGraph TO SetAxisField(ChartYMin,yLowVal.Value());
                  ASK IntAoGraph TO SetAxisField(ChartYMin,yLowVal.Value());
                  ASK MultiRunGraph TO SetAxisField(ChartYMin,yLowVal.Value());
                  ASK AoGraph TO SetAxisField(ChartYIntercept,yLowVal.Value());
                  ASK IntAoGraph TO SetAxisField(ChartYIntercept,yLowVal.Value());
                  ASK MultiRunGraph TO SetAxisField(ChartYIntercept,yLowVal.Value());
                  somethingChanged := TRUE;
               END IF;
               {IF yMax <> yHiVal.Value()
                  yMax := yHiVal.Value();
                  ASK AoGraph TO SetAxisField(ChartYMax,yHiVal.Value());
                  ASK IntAoGraph TO SetAxisField(ChartYMax,yHiVal.Value());
                  ASK MultiRunGraph TO SetAxisField(ChartYMax,yHiVal.Value());
                  somethingChanged := TRUE;
               END IF;}
               ASK AoGraph TO Draw;
               ASK IntAoGraph TO Draw;
               ASK MultiRunGraph TO Draw;
               ASK AoGraphWindow TO Draw;
            END IF;
            DISPOSE(text);
            DISPOSE(setYBox);
         WHEN 202: {View Per Trial Ao Graph}
            currentGraph := 1;
            ASK AoGraphWindow TO SetTitle("Per Trial Ao Graph");
            ASK AoGraph TO SetHidden(FALSE);
            ASK IntAoGraph TO SetHidden(TRUE);
            ASK MultiRunGraph TO SetHidden(TRUE);
            ASK AoGraphWindow TO Draw;
         WHEN 203: {View Interval Ao Graph}
            currentGraph := 2;
            ASK AoGraphWindow TO SetTitle("Interval Ao Graph");
            ASK AoGraph TO SetHidden(TRUE);
            ASK IntAoGraph TO SetHidden(FALSE);
            ASK MultiRunGraph TO SetHidden(TRUE);
            ASK AoGraphWindow TO Draw;
         WHEN 204: {View Multi-run Ao Graph}
            currentGraph := 3;
            ASK AoGraphWindow TO SetTitle("Multi-run Ao Graph");
            ASK AoGraph TO SetHidden(TRUE);
            ASK IntAoGraph TO SetHidden(TRUE);
            ASK MultiRunGraph TO SetHidden(FALSE);
            ASK AoGraphWindow TO Draw;
         {*** Help Menu ***}
         WHEN 301: {Help Index}
            CallHelp(74);
         WHEN 302: {Visit Raptor Webpage}
            VisitRaptorWebpage;
         OTHERWISE;   
      END CASE;    
   END METHOD; {BeSelected}
END OBJECT; {GraphMenuObj}

{*** Simulation Menu ***}            
OBJECT simMenuObj;
   ASK METHOD ChangeState(IN state : STRING);
   VAR
      winMenu  : ControlVObj;
      tempHier : RBDHierObj;
   BEGIN
      CASE state
      WHEN "InitSim":
         inEndState := FALSE;
         notPaused  := TRUE;
         printItem  := Descendant("PrintItem",101);
         returnItem := Descendant("ReturnItem",104);
         pauseItem  := Descendant("PauseItem",201);
         stepItem   := Descendant("StepItem",202);
         jumpItem   := Descendant("JumpItem", 203);
         stopItem   := Descendant("StopItem",204);
         slowItem   := Descendant("SlowItem",207);
         fastItem   := Descendant("FastItem",206);
         resimItem  := Descendant("ResimItem",205);
         viewItem   := Descendant("ViewOutItem",301);
         colorItem  := Descendant("ColorPrefItem", 302);
         statusItem := Descendant("StatusItem", 303);
         smallItem  := Descendant("SmallItem", 3041);
         medItem    := Descendant("MedItem", 3042);
         largeItem  := Descendant("LargeItem", 3043);
         grandeItem := Descendant("GrandeItem", 3044);
         homeItem   := Descendant("HomeItem", 401);
         upItem     := Descendant("UpItem", 402);
         gotoItem   := Descendant("GotoItem", 403);
         NEW(statusIndItem, 1..20);
         FOR i:=1 TO 19
            statusIndItem[i] := Descendant("StatusIndItem", 3050 + i);
         END FOR;
         printBut   := ASK simToolBar Descendant("PrintButton",901);
         returnBut  := ASK simToolBar Descendant("ReturnButton",902);
         pauseBut   := ASK simToolBar Descendant("PauseButton",903);
         stepBut    := ASK simToolBar Descendant("StepButton",904);
         jumpBut    := ASK simToolBar Descendant("JumpButton", 905);
         stopBut    := ASK simToolBar Descendant("StopButton",906);
         resimBut   := ASK simToolBar Descendant("ResimButton",907);
         slowBut    := ASK simToolBar Descendant("SlowButton",908);
         fastBut    := ASK simToolBar Descendant("FastButton",909);
         viewBut    := ASK simToolBar Descendant("ViewOutButton",910);
         colorBut   := ASK simToolBar Descendant("ColorPrefButton", 911);
         homeBut    := ASK simToolBar Descendant("HomeButton", 912);
         upBut      := ASK simToolBar Descendant("UpButton", 913);
         gotoBut    := ASK simToolBar Descendant("GotoButton", 914);
         IF dSimWithGraph
            ASK statusItem TO SetLabel("Hide Status Indicator");
            ASK statusItem TO Activate;
            ASK smallItem TO Activate;
            ASK medItem TO Activate;
            ASK largeItem TO Activate;
            ASK grandeItem TO Activate;
            FOR i:=1 TO 19
               ASK statusIndItem[i] TO Activate;
            END FOR;
         ELSE
            ASK statusItem TO SetLabel("Show Status Indicator");
            ASK statusItem TO Deactivate;
            ASK smallItem TO Deactivate;
            ASK medItem TO Deactivate;
            ASK largeItem TO Deactivate;
            ASK grandeItem TO Deactivate;
            FOR i:=1 TO 19
               ASK statusIndItem[i] TO Deactivate;
            END FOR;
         END IF;
         IF hierLevel = 0
            ASK upBut TO Deactivate;
            ASK upItem TO Deactivate;
            ASK homeBut TO Deactivate;
            ASK homeItem TO Deactivate;
         ELSE
            ASK upBut TO Activate;
            ASK upItem TO Activate;
            ASK homeBut TO Activate;
            ASK homeItem TO Activate;
         END IF;
         IF totalHiers = 0
            ASK gotoBut TO Deactivate;
            ASK gotoItem TO Deactivate;
         ELSE
            ASK gotoBut TO Activate;
            ASK gotoItem TO Activate;
         END IF;
         ASK pauseBut   TO Activate;
         ASK pauseItem  TO Activate;
         ASK colorItem  TO Deactivate;
         ASK stopBut    TO Activate;
         ASK stopItem   TO Activate;
         ASK returnBut  TO Deactivate;
         ASK resimBut   TO Deactivate;
         ASK viewBut    TO Deactivate;
         ASK printBut   TO Deactivate;
         ASK slowBut    TO Activate;
         ASK fastBut    TO Activate;
         ASK colorBut   TO Deactivate;
         ASK returnItem TO Deactivate;
         ASK resimItem  TO Deactivate;
         ASK viewItem   TO Deactivate;
         ASK printItem  TO Deactivate;
         ASK slowItem   TO Activate;
         ASK fastItem   TO Activate;
         IF startStep
            oldSpeed := timeSlice;
            Timescale := .0000001;
            timeSlice := Timescale;
            ASK pauseBut  TO SetSelected(TRUE);
            ASK stepBut   TO Activate;
            ASK jumpBut   TO Activate;
            ASK stepItem  TO Activate;
            ASK jumpItem  TO Activate;
            ASK printBut  TO Activate;
            ASK printItem TO Activate;
         ELSE   
            ASK pauseBut TO SetSelected(FALSE);
            ASK stepBut  TO Deactivate;
            ASK jumpBut  TO Deactivate;
            ASK stepItem TO Deactivate;
            ASK jumpItem TO Deactivate;
         END IF;
         IF (timeSlice >= 1000000.) OR (NOT dSimWithGraph)
            ASK slowBut  TO Deactivate;
            ASK slowItem TO Deactivate;
         END IF;
         IF (timeSlice <= .000001) OR (NOT dSimWithGraph)
            ASK fastBut  TO Deactivate;
            ASK fastItem TO Deactivate;
         END IF;
      WHEN "Paused":
         ASK pauseBut TO SetSelected(TRUE);
         IF dSimWithGraph
            ASK stepBut  TO Activate;
            ASK jumpBut  TO Activate;
            ASK stepItem TO Activate; {FIX}
            ASK jumpItem TO Activate;
         END IF;
         IF compileType <> "demo"
            ASK printBut TO Activate;
         END IF;
         ASK slowBut   TO Deactivate;
         ASK fastBut   TO Deactivate;
         ASK pauseItem TO SetLabel("&Resume");
         IF compileType <> "demo"
            ASK printItem TO Activate;
         END IF;
         ASK slowItem TO Deactivate;
         ASK fastItem TO Deactivate;
      WHEN "Unpaused":
         ASK pauseBut  TO SetSelected(FALSE);
         ASK stepBut   TO Deactivate;
         ASK jumpBut   TO Deactivate;
         ASK stepItem  TO Deactivate;
         ASK jumpItem  TO Deactivate;
         ASK printBut  TO Deactivate;
         ASK pauseItem TO SetLabel("&Pause");
         ASK printItem TO Deactivate;
      WHEN "Unstepped":
         ASK pauseItem TO Activate;
         ASK stepItem  TO Activate;
         ASK jumpItem  TO Activate;
         ASK stopItem  TO Activate;
      WHEN "Stepped":
         {ASK pauseItem TO Deactivate;
         ASK stepItem TO Deactivate;FIX
         ASK stopItem TO Deactivate;}
      WHEN "ChangeSpeed":
         IF timeSlice >= 1000000.
            ASK slowBut  TO Deactivate;
            ASK slowItem TO Deactivate;
         ELSE
            ASK slowBut  TO Activate;
            ASK slowItem TO Activate;
         END IF;
         IF timeSlice <= .000001
            ASK fastBut  TO Deactivate;
            ASK fastItem TO Deactivate;
         ELSE
            ASK fastBut  TO Activate;
            ASK fastItem TO Activate;
         END IF;    
      WHEN "FinishSim":
         inEndState := TRUE;
         ASK pauseBut  TO SetSelected(FALSE);
         ASK pauseBut  TO Deactivate;
         ASK stepBut   TO Deactivate;
         ASK jumpBut   TO Deactivate;
         ASK stepItem  TO Deactivate;
         ASK jumpItem  TO Deactivate;
         ASK stopBut   TO Deactivate;
         ASK returnBut TO Activate;
         ASK resimBut  TO Activate;
         ASK viewBut   TO Activate;
         IF compileType <> "demo"
            ASK printBut TO Activate;
         END IF;
         ASK slowBut TO Deactivate;
         ASK fastBut TO Deactivate;
         IF weakAnalysis 
            ASK colorBut TO Activate;
         END IF;
         ASK pauseItem  TO SetLabel("&Pause");
         ASK pauseItem  TO Deactivate;
         ASK stopItem   TO Deactivate;
         ASK returnItem TO Activate;
         ASK resimItem  TO Activate;
         ASK viewItem   TO Activate;
         IF compileType <> "demo"
            ASK printItem TO Activate;
         END IF;
         ASK slowItem TO Deactivate;
         ASK fastItem TO Deactivate;
         IF weakAnalysis AND (NOT hitXinSim)  
            AoVisible := TRUE;
            ASK colorItem TO Activate;
            analViewAvail := TRUE;
            analUp := TRUE;
         END IF;
      WHEN "AnalView":
         {FinEndState := TRUE;
         printItem  := Descendant("PrintItem",11);
         returnItem := Descendant("ReturnItem",14);
         pauseItem  := Descendant("PauseItem",21);
         stepItem   := Descendant("StepItem",22);
         stopItem   := Descendant("StopItem",23);
         slowItem   := Descendant("SlowItem",24);
         fastItem   := Descendant("FastItem",25);
         resimItem  := Descendant("ResimItem",26);
         viewItem   := Descendant("ViewOutItem",31);
         colorItem  := Descendant("ColorPrefItem", 32);
         statusItem := Descendant("StatusItem", 33);
         ASK pauseItem  TO Deactivate;
         ASK stepItem   TO Deactivate;
         ASK stopItem   TO Deactivate;
         ASK resimItem  TO Deactivate;
         ASK statusItem TO Deactivate;
         ASK fastItem   TO Deactivate;
         ASK slowItem   TO Deactivate;
         ASK returnItem TO Activate;
         ASK viewItem   TO Activate;
         ASK colorItem  TO Activate;
         IF NOT demoVersion
            ASK printItem TO Activate;
         END IF;
         pauseBut  := ASK simToolBar Descendant("PauseButton",901);
         stepBut   := ASK simToolBar Descendant("StepButton",902);
         stopBut   := ASK simToolBar Descendant("StopButton",903);
         returnBut := ASK simToolBar Descendant("ReturnButton",904);
         resimBut  := ASK simToolBar Descendant("ResimButton",905);
         viewBut   := ASK simToolBar Descendant("ViewOutButton",906);
         printBut  := ASK simToolBar Descendant("PrintButton",907);
         slowBut   := ASK simToolBar Descendant("SlowButton",908);
         fastBut   := ASK simToolBar Descendant("FastButton",909);
         colorBut  := ASK simToolBar Descendant("ColorPrefButton", 911);
         ASK pauseBut  TO Deactivate;
         ASK stepBut   TO Deactivate;
         ASK stopBut   TO Deactivate;
         ASK resimBut  TO Deactivate;
         ASK slowBut   TO Deactivate;
         ASK fastBut   TO Deactivate;
         ASK returnBut TO Activate;
         ASK viewBut   TO Activate;
         ASK colorBut  TO Activate;
         IF NOT demoVersion
            ASK printBut TO Activate;
         END IF; }
      OTHERWISE
      END CASE;
      ASK pauseBut TO Draw;
      ASK pauseItem TO Draw;
      DISPOSE(statusIndItem);
   END METHOD;
   
   ASK METHOD KillSim;
   BEGIN
      exploded   := TRUE;
      notPaused  := TRUE;
      simulated  := TRUE;
      inEndState := TRUE;
      IF simInProgress
         IF (NOT inStepMode) 
            TELL System TO CallEndSimulation(TRUE,FALSE);
         END IF;
      ELSE
         ReturnToWorkspace;
      END IF;
   END METHOD;

   ASK METHOD BeSelected;
   VAR
      cancelled, statusIndChanged         : BOOLEAN;
      lastChosen                          : GraphicVObj;
      tabOutBox                           : TablesOutObj;
      tempBlock                           : RBDBlockObj;
      tempEvent                           : RBDEventObj;
      tempNode                            : RBDNodeObj;
      tempHier                            : RBDHierObj;
      dir, opSys                          : STRING;
      nodeImage, blockImage, eventImage, hierImage               : ImageObj;
      aoText                              : TextObj;
      
   BEGIN
      IF ignoreMouse {eag error 51 fix}
         RETURN;
      END IF;
      CASE ASK LastPicked Id
      {*** File Menu ***}
      WHEN 101:  {Print Window}
         PrintWindow;
      WHEN 102: {Print Setup}
         PrintSetup;
      WHEN 103: {Save RBD as BMP}
         ExportRBD(FALSE, 0, activeWindow, "Don't Matter");
      WHEN 104: {Return To RBD}
         ReturnToWorkspace;
      {*** Control Menu ***}
      WHEN 201: {Pause/Resume Sim}
         IF pauseItem.Label = "&Pause"
            PauseSim;
         ELSE
            ResumeSim;
         END IF;
      WHEN 202: {Next Step}
         goToNext := TRUE;
         stepped := TRUE;
         inStepMode := TRUE;
         inJumpMode := FALSE;
         ChangeState("Stepped")
      WHEN 203: {Jump}
         Jump;
      WHEN 204: {Early Term}
         notPaused := TRUE;
         inStepMode := FALSE;
         inJumpMode := FALSE;
         goToNext := TRUE;
         simulated := TRUE;     
         TELL System TO CallEndSimulation(TRUE,TRUE);
         ChangeState("FinishSim");
      WHEN 205: {Restart Sim}
         doneSimming := FALSE;
         IF weakAnalysis OR analUp
            FOREACH tempNode IN nodeGroup
               nodeImage := ASK tempNode Child("Node", 602);
               aoText := ASK tempNode Child("NodeAoText",0);
               IF tempNode.typeNode = 2
                  ASK aoText TO SetText("");
                  ASK aoText TO SetHidden(TRUE);
                  ASK nodeImage TO SetColor(LimeGreen);
                  ASK aoText TO Draw;
                  ASK nodeImage TO Draw;
               ELSIF tempNode.typeNode = 3
                  ASK aoText TO SetText("");
                  ASK aoText TO SetHidden(TRUE);
                  ASK nodeImage TO SetColor(Magenta);
                  ASK aoText TO Draw;
                  ASK nodeImage TO Draw;
               END IF;
            END FOREACH;
            FOREACH tempBlock IN blockGroup
               blockImage := ASK tempBlock Child("BasicBlock", 601);
               aoText := ASK tempBlock Child("BlockAoText",0);
               ASK aoText TO SetText("");
               ASK aoText TO SetHidden(TRUE);
               ASK blockImage TO SetColor(LimeGreen);
               ASK aoText TO Draw;
               ASK blockImage TO Draw;
            END FOREACH;
            FOREACH tempEvent IN eventGroup
               eventImage := ASK tempEvent Child("BasicBlock", 601);
               aoText := ASK tempEvent Child("EventAoText",0);
               ASK aoText TO SetText("");
               ASK aoText TO SetHidden(TRUE);
               ASK eventImage TO SetColor(LimeGreen);
               ASK aoText TO Draw;
               ASK eventImage TO Draw;
            END FOREACH;
            FOREACH tempHier IN hierGroup
               hierImage := ASK tempHier Child("Hier", 603);
               aoText := ASK tempHier Child("HierAoText",0);
               ASK aoText TO SetText("");
               ASK aoText TO SetHidden(TRUE);
               ASK hierImage TO SetColor(LimeGreen);
               ASK aoText TO Draw;
               ASK hierImage TO Draw;
            END FOREACH;
            AoVisible := FALSE;
            IF analUp AND notSimming
               ShowAnalView(FALSE);
               notSimming := FALSE;
            END IF;
         END IF;
         analUp := FALSE;
         returnToRBD := TRUE;
         ChangeState("InitSim");
      WHEN 206: {Increase Speed}
         Timescale := Timescale/10.;
         timeSlice := Timescale;
         ChangeState("ChangeSpeed");
         ASK window TO ShowStatus(0,"Sim Speed: "+SUBSTR(1,8,REALTOSTR(1./Timescale)));
      WHEN 207: {Decrease Speed}
         Timescale := Timescale*10.;
         timeSlice := Timescale;
         ChangeState("ChangeSpeed");
         ASK window TO ShowStatus(0,"Sim Speed: "+SUBSTR(1,8,REALTOSTR(1./Timescale)));
      {*** Options Menu ***}
      WHEN 301: {View Outputs}
         ViewOutputs;
      WHEN 302: {Color Prefs}
         ColorPrefs;
      WHEN 303: {Hide/Show Status Symbol}
         NEW(statusIndItem, 1..20);
         FOR i:=1 TO 19
            statusIndItem[i] := Descendant("StatusIndItem", 3050 + i);
         END FOR;
         IF statusItem.Label = "Hide Status Indicator";
            faceBoxOpen := FALSE;
            ASK statusItem TO SetLabel("Show Status Indicator");
            ASK greenFace TO Erase;
            ASK yellowFace TO Erase;
            ASK redFace TO Erase;
            ASK smallItem TO Deactivate;
            ASK medItem TO Deactivate;
            ASK largeItem TO Deactivate;
            ASK grandeItem TO Deactivate;
            FOR i:=1 TO 19
               ASK statusIndItem[i] TO Deactivate;
            END FOR;
         ELSE
            faceBoxOpen := TRUE;
            ASK statusItem TO SetLabel("Hide Status Indicator");
            ASK faceVisible TO Draw;
            ASK statusItem TO Activate;
            ASK smallItem TO Activate;
            ASK medItem TO Activate;
            ASK largeItem TO Activate;
            ASK grandeItem TO Activate;
            FOR i:=1 TO 19
               ASK statusIndItem[i] TO Activate;
            END FOR;
         END IF;
         DISPOSE(statusIndItem);
      WHEN 3041: {Small}
         symbolScale := .3;
         symbolOffset := 34.;
         ASK greenFace   TO SetScaling(symbolScale,symbolScale);
         ASK greenFace   TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK yellowFace  TO SetScaling(symbolScale,symbolScale);
         ASK yellowFace  TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK redFace     TO SetScaling(symbolScale,symbolScale);
         ASK redFace     TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK greenFace   TO Erase;
         ASK yellowFace  TO Erase;
         ASK redFace     TO Erase;   
         ASK faceVisible TO Draw; 
      WHEN 3042: {Medium}
         symbolScale := .45;
         symbolOffset := 47.;
         ASK greenFace   TO SetScaling(symbolScale,symbolScale);
         ASK greenFace   TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK yellowFace  TO SetScaling(symbolScale,symbolScale);
         ASK yellowFace  TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK redFace     TO SetScaling(symbolScale,symbolScale);
         ASK redFace     TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK greenFace   TO Erase;
         ASK yellowFace  TO Erase;
         ASK redFace     TO Erase;   
         ASK faceVisible TO Draw; 
      WHEN 3043: {Large}
         symbolScale := .6;
         symbolOffset := 62.;
         ASK greenFace   TO SetScaling(symbolScale,symbolScale);
         ASK greenFace   TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK yellowFace  TO SetScaling(symbolScale,symbolScale);
         ASK yellowFace  TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK redFace     TO SetScaling(symbolScale,symbolScale);
         ASK redFace     TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK greenFace   TO Erase;
         ASK yellowFace  TO Erase;
         ASK redFace     TO Erase;   
         ASK faceVisible TO Draw; 
      WHEN 3044: {Grande}
         symbolScale := .75;
         symbolOffset := 76.;
         ASK greenFace   TO SetScaling(symbolScale,symbolScale);
         ASK greenFace   TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK yellowFace  TO SetScaling(symbolScale,symbolScale);
         ASK yellowFace  TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK redFace     TO SetScaling(symbolScale,symbolScale);
         ASK redFace     TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK greenFace   TO Erase;
         ASK yellowFace  TO Erase;
         ASK redFace     TO Erase;   
         ASK faceVisible TO Draw; 
      WHEN 3051:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Airc_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Airc_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Airc_R");
         systemImage := "Aircraft";
      WHEN 3052:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Arti_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Arti_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Arti_R");
         systemImage := "Artillery";
      WHEN 3053:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Circ_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Circ_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Circ_R");
         systemImage := "Circuit_Card";
      WHEN 3054:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Comp_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Comp_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Comp_R");
         systemImage := "Computer";
      WHEN 3055:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Bull_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Bull_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Bull_R");
         systemImage := "Bull's_eye";
      WHEN 3056:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Face_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Face_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Face_R");
         systemImage := "Face";
      WHEN 3057:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Heli_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Heli_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Heli_R");
         systemImage := "Helicopter";
      WHEN 3058:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Miss_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Miss_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Miss_R");
         systemImage := "Missile";
      WHEN 3059:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Netw_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Netw_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Netw_R");
         systemImage := "Network";
      WHEN 3060:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Equa_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Equa_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Equa_R");
         systemImage := "Equalizer";
      WHEN 3061:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Radi_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Radi_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Radi_R");
         systemImage := "Radio";
      WHEN 3062:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Rado_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Rado_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Rado_R");
         systemImage := "Radome";
      WHEN 3063:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Mete_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Mete_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Mete_R");
         systemImage := "Meter";
      WHEN 3064:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Sate_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Sate_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Sate_R");
         systemImage := "Satellite";
      WHEN 3065:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Ship_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Ship_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Ship_R");
         systemImage := "Ship";
      WHEN 3066:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Stop_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Stop_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Stop_R");
         systemImage := "Stoplight";
      WHEN 3067:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Tank_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Tank_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Tank_R");
         systemImage := "Tank";
      WHEN 3068:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Trai_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Trai_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Trai_R");
         systemImage := "Train";
      WHEN 3069:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Truc_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Truc_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Truc_R");
         systemImage := "Truck";
      {*** Navigate Menu ***}
      WHEN 401: {Home}
         Home;
      WHEN 402: {Up}
         UpOneLevel;
      WHEN 403: {Goto}
         GotoHier;
      WHEN 404: {Zoom In}
         Zoom("in");
      WHEN 405: {Zoom Out}
         Zoom("out");
      WHEN 406: {Zoom Percent}
         IF activeWindow > 0
            tempHier := ASK root Child("RBDHier", activeWindow)
            ZoomPercent(tempHier.xOrigin, tempHier.yOrigin, FALSE);
         ELSE
            ZoomPercent(xOrigin, yOrigin, FALSE);
         END IF;
      WHEN 408: {Display Tree}
         DisplayTree;
      {*** Help Menu ***}
      WHEN 501:       {Help}
         CallHelp(71);
      WHEN 502: {Visit Raptor Webpage}
         VisitRaptorWebpage;
      OTHERWISE
      END CASE;
      IF statusIndChanged
         ASK greenFace TO SetScaling(symbolScale,symbolScale);
         ASK greenFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK yellowFace TO SetScaling(symbolScale,symbolScale);
         ASK yellowFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK redFace TO SetScaling(symbolScale,symbolScale);
         ASK redFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK greenFace TO Erase;
         ASK yellowFace TO Erase;
         ASK redFace TO Erase;  
         ASK faceVisible TO Draw;
         statusIndChanged := FALSE;
         somethingChanged := TRUE;
      END IF;
      Update;
   END METHOD; {BeSelected}
   
   ASK METHOD SetNavigation;
   BEGIN
      IF totalHiers > 0
         menuItem := Descendant("GotoItem", 403);
         ASK menuItem TO Activate;
      ELSE
         menuItem := Descendant("GotoItem", 403);
         ASK menuItem TO Deactivate;
      END IF;
      IF activeWindow = 0
         menuItem := Descendant("HomeItem",401);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("UpItem",402);
         ASK menuItem TO Deactivate;
      ELSE
         menuItem := Descendant("HomeItem",401);
         ASK menuItem TO Activate;
         menuItem := Descendant("UpItem",402);
         ASK menuItem TO Activate;
      END IF;
   END METHOD; {SetNavigation}
END OBJECT; {SimMenuObj}

{*** Simulation Toolbar ***}
OBJECT simToolObj;
   ASK METHOD BeSelected;
   VAR
      newId                          : INTEGER;
      lastChosen                          : GraphicVObj;
      tabOutBox                           : TablesOutObj;
      tempBlock                           : RBDBlockObj;
      tempEvent                           : RBDEventObj;
      tempNode                            : RBDNodeObj;
      tempHier                            : RBDHierObj;
      cancelled                           : BOOLEAN;
      nodeImage, blockImage, eventImage, hierImage, dsi   : ImageObj;
      aoText                              : TextObj;
      dir, newName                                 : STRING;
   BEGIN
      IF ignoreMouse {eag error 51 fix}
         RETURN;
      END IF;
      IF LastPicked <> NILOBJ    
         CASE ASK LastPicked Id
         WHEN 901:  {PrintRBD}
            PrintWindow;
         WHEN 902: {Return to RBD}
            ReturnToWorkspace;
         WHEN 903: {Pause/Resume Sim}
            IF pauseBut.Selected
               FillStatusBar;
               ASK simMenuBar TO ChangeState("Paused");
               notPaused := FALSE;
               inStepMode := TRUE;
               inJumpMode := FALSE;
               stepped := FALSE;
               IF dSimWithGraph
                  oldSpeed := timeSlice;
                  Timescale := .0000001;
                  timeSlice := Timescale;
               END IF;
               REPEAT
                  lastChosen := ASK SELF LastPicked;
                  Delay(1);
                  IF lastChosen = NILOBJ
                     lastChosen := Child("PauseItem", 901);
                  END IF;
                  HandleEvents(FALSE);
               UNTIL notPaused OR stepped;
            ELSE
               IF dSimWithGraph
                  Timescale := oldSpeed;
                  timeSlice := oldSpeed;
                  ASK simMenuBar TO ChangeState("ChangeSpeed");
               END IF;
               IF stepped AND (NOT inEndState)
                  ASK simMenuBar TO ChangeState("Unstepped");
               END IF;
               goToNext := TRUE;
               inStepMode := FALSE;
               inJumpMode := FALSE;
               notPaused := TRUE;
               ASK simMenuBar TO ChangeState("Unpaused");
               IF NOT statusBarOn
                  FOR i := 0 TO 8
                     ASK window TO ShowStatus(i,"");
                  END FOR;
                  ASK window TO ShowStatus(1,"Simulating...");   
               ELSE
                  ASK window TO ShowStatus(0,"Sim Speed: "+SUBSTR(1,8,REALTOSTR(1./Timescale)));   
               END IF;
            END IF;
         WHEN 904: {Next Step}
            Step;
         WHEN 905: {Jump}
            Jump;
         WHEN 906: {Early Term}
            notPaused := TRUE;
            inStepMode := FALSE;
            inJumpMode := FALSE;
            goToNext := TRUE;
            simulated := TRUE;
            TELL System TO CallEndSimulation(TRUE,TRUE);
            ASK simMenuBar TO ChangeState("FinishSim");
         WHEN 907: {Restart Sim}
            doneSimming := FALSE;
            IF weakAnalysis OR analUp
               FOREACH tempNode IN nodeGroup
                  nodeImage := ASK tempNode Child("Node", 602);
                  aoText := ASK tempNode Child("NodeAoText",0);
                  IF tempNode.typeNode = 2
                     ASK aoText TO SetText("");
                     ASK aoText TO SetHidden(TRUE);
                     ASK nodeImage TO SetColor(LimeGreen);
                     ASK aoText TO Draw;
                     ASK nodeImage TO Draw;
                  ELSIF tempNode.typeNode = 3
                     ASK aoText TO SetText("");
                     ASK aoText TO SetHidden(TRUE);
                     ASK nodeImage TO SetColor(Magenta);
                     ASK aoText TO Draw;
                     ASK nodeImage TO Draw;
                  END IF;
               END FOREACH;
               FOREACH tempBlock IN blockGroup
                  blockImage := ASK tempBlock Child("BasicBlock", 601);
                  aoText := ASK tempBlock Child("BlockAoText",0);
                  ASK aoText TO SetText("");
                  ASK aoText TO SetHidden(TRUE);
                  ASK blockImage TO SetColor(LimeGreen);
                  ASK aoText TO Draw;
                  ASK blockImage TO Draw;
               END FOREACH;
               FOREACH tempEvent IN eventGroup
                  eventImage := ASK tempEvent Child("BasicBlock", 601);
                  aoText := ASK tempEvent Child("EventAoText",0);
                  ASK aoText TO SetText("");
                  ASK aoText TO SetHidden(TRUE);
                  ASK eventImage TO SetColor(LimeGreen);
                  ASK aoText TO Draw;
                  ASK eventImage TO Draw;
               END FOREACH;
               FOREACH tempHier IN hierGroup
                  hierImage := ASK tempHier Child("Hier", 603);
                  aoText := ASK tempHier Child("HierAoText",0);
                  ASK aoText TO SetText("");
                  ASK aoText TO SetHidden(TRUE);
                  ASK hierImage TO SetColor(LimeGreen);
                  ASK aoText TO Draw;
                  dsi:= ASK tempHier Descendant("HierMid", 0);
                  ASK dsi TO SetColor(LimeGreen);
                  ASK hierImage TO Draw;
               END FOREACH;
               AoVisible := FALSE;
               IF analUp AND notSimming
                  ShowAnalView(FALSE);
                  notSimming := FALSE;
               END IF;
            END IF;
            analUp := FALSE;
            returnToRBD := TRUE;
            ASK simMenuBar TO ChangeState("InitSim");
         WHEN 908: {Decrease Speed}
            Timescale := Timescale*10.;
            timeSlice := Timescale;
            ASK simMenuBar TO ChangeState("ChangeSpeed");
            ASK window TO ShowStatus(0,"Sim Speed: "+SUBSTR(1,8,REALTOSTR(1./Timescale)));
         WHEN 909: {Increase Speed}
            Timescale := Timescale/10.;
            timeSlice := Timescale;
            ASK simMenuBar TO ChangeState("ChangeSpeed");
            ASK window TO ShowStatus(0,"Sim Speed: "+SUBSTR(1,8,REALTOSTR(1./Timescale)));
         WHEN 910: {View Outputs}
            ViewOutputs;
         WHEN 911: {Color Prefs}
            ColorPrefs;
         WHEN 912: {Home}
            Home;
         WHEN 913:  {Up}
            UpOneLevel;
         WHEN 914: {Goto}
            GotoHier;
         WHEN 915: {Zoom Out}
            Zoom("out");
         WHEN 916: {Zoom In}
            Zoom("in");
         WHEN 941: {Zoom Fit}
            ZoomFit;
         WHEN 918: {Display Tree}
            DisplayTree;
         WHEN 919: {Help}
            CallHelp(81);
         OTHERWISE
         END CASE;
      END IF; 
   END METHOD; {BeSelected}
  
   ASK METHOD SetNavigation;
   BEGIN
      IF totalHiers > 0
         buttItem := Descendant("GotoButton", 914);
         ASK buttItem TO Activate;
      ELSE
         buttItem := Descendant("GotoButton", 914);
         ASK buttItem TO Deactivate;
      END IF;
      IF activeWindow = 0
         buttItem := Descendant("HomeButton",912);
         ASK buttItem TO Deactivate;
         buttItem := Descendant("UpButton",913);
         ASK buttItem TO Deactivate;
      ELSE
         buttItem := Descendant("HomeButton",912);
         ASK buttItem TO Activate;
         buttItem := Descendant("UpButton",913);
         ASK buttItem TO Activate;
      END IF;
     END METHOD; {SetNavigation}
END OBJECT; {SimToolObj}

{*** Weak Link Menu ***}
OBJECT weakMenuObj;
   ASK METHOD BeSelected;
   VAR
      tempHier : RBDHierObj;
   BEGIN
      IF ignoreMouse {eag error 51 fix}
         RETURN;
      END IF;
      CASE ASK LastPicked Id
      {*** File Menu ***}
      WHEN 101:  {Print Window}
         PrintWindow;
      WHEN 102: {Print Setup}
         PrintSetup;
      WHEN 103: {Save RBD as BMP}
         ExportRBD(FALSE, 0, activeWindow, "Don't Matter");
      WHEN 104: {Return To RBD}
         ReturnToWorkspace;
      {*** Options Menu ***}
      WHEN 201: {View Outputs}
         ViewOutputs;
      WHEN 202: {Show Color Prefs}
         ColorPrefs;
      {*** Navigate Menu ***}
      WHEN 301: {Home}
         Home;
      WHEN 302: {Up}
         UpOneLevel;
      WHEN 303: {Goto}
         GotoHier;
      WHEN 304: {Zoom In}
         Zoom("in");
      WHEN 305: {Zoom Out}
         Zoom("out");
      WHEN 306: {Zoom Percent}
         IF activeWindow > 0
            tempHier := ASK root Child("RBDHier", activeWindow)
            ZoomPercent(tempHier.xOrigin, tempHier.yOrigin, FALSE);
         ELSE
            ZoomPercent(xOrigin, yOrigin, FALSE);
         END IF;
      WHEN 308: {Display Tree}
         DisplayTree;
      {*** Help Menu ***}
      WHEN 401:       {Help}
         CallHelp(72);
      WHEN 402: {Visit Raptor Webpage}
         VisitRaptorWebpage;
      OTHERWISE;
      END CASE;
   END METHOD;
   
   ASK METHOD SetNavigation;
   BEGIN
      IF totalHiers > 0
         menuItem := Descendant("GotoItem", 303);
         ASK menuItem TO Activate;
      ELSE
         menuItem := Descendant("GotoItem", 303);
         ASK menuItem TO Deactivate;
      END IF;
      IF activeWindow = 0
         menuItem := Descendant("HomeItem",301);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("UpItem",302);
         ASK menuItem TO Deactivate;
      ELSE
         menuItem := Descendant("HomeItem",301);
         ASK menuItem TO Activate;
         menuItem := Descendant("UpItem",302);
         ASK menuItem TO Activate;
      END IF;
   END METHOD; {SetNavigation}

END OBJECT; {WeakMenuObj}

OBJECT weakToolObj;
   ASK METHOD BeSelected;
   BEGIN
      IF ignoreMouse {eag error 51 fix}
         RETURN;
      END IF;
      IF LastPicked <> NILOBJ    
         CASE ASK LastPicked Id
         WHEN 901:  {PrintRBD}
            PrintWindow;
         WHEN 902: {Return to Workspace}
            ReturnToWorkspace;
         WHEN 903: {View Outputs}
            ViewOutputs;
         WHEN 904: {Show Color Prefs}
            ColorPrefs;
         WHEN 905: {Home}
            Home;
         WHEN 906:  {Up}
            UpOneLevel;
         WHEN 907: {Goto}
            GotoHier;
         WHEN 908: {Zoom Out}
            Zoom("out");
         WHEN 909: {Zoom In}
            Zoom("in");
         WHEN 941: {Zoom Fit}
            ZoomFit;
         WHEN 911: {Display Tree}
            DisplayTree;
         WHEN 912: {Help}
            CallHelp(82);
         OTHERWISE
         END CASE;
      END IF;
   END METHOD;

   ASK METHOD SetNavigation;
   BEGIN
      IF totalHiers > 0
         buttItem := Descendant("GotoButton", 907);
         ASK buttItem TO Activate;
      ELSE
         buttItem := Descendant("GotoButton", 907);
         ASK buttItem TO Deactivate;
      END IF;
      IF activeWindow = 0
         buttItem := Descendant("HomeButton",905);
         ASK buttItem TO Deactivate;
         buttItem := Descendant("UpButton",906);
         ASK buttItem TO Deactivate;
      ELSE
         buttItem := Descendant("HomeButton",905);
         ASK buttItem TO Activate;
         buttItem := Descendant("UpButton",906);
         ASK buttItem TO Activate;
      END IF;
   END METHOD; {SetNavigation}
END OBJECT; {WeakToolObj}

{*** FEV Menu ***}
OBJECT fevMenuObj;
   ASK METHOD BeSelected;
   VAR
      menu              : MenuObj;
      i, phaseId        : INTEGER;
      lastClicked       : GraphicVObj;
      failRepairBox     : FailRepairBoxObj;
      statusIndChanged  : BOOLEAN;
      tempHier          : RBDHierObj;
   BEGIN
      IF ignoreMouse {eag error 51 fix}
         RETURN;
      END IF;
      CASE ASK LastPicked Id
      {*** File Menu ***}
      WHEN 101: {Open/Close Interactive Log}
         menuItem := Descendant("OpenItem", 101);
         buttItem := ASK fevToolBar Descendant("OpenFile", 903);
         IF (menuItem.Label = "&Open Interactive Log...")
            FEVOpenFile;
         ELSE
            FEVCloseFile;
         END IF;
      WHEN 102: {Write To Interactive Log}
         CreateFMECAarray;
         WriteFMECAtoFile;
      WHEN 103: {Create Single Point Failure File}
         WriteSPFfile;
      WHEN 104: {Print Window}
         PrintWindow;
      WHEN 105: {Print Setup}
         PrintSetup;
      WHEN 106: {Save Window as BMP}
         ExportRBD(FALSE, 0, activeWindow, "Don't Matter");
      WHEN 107: {Return To Workspace}
         menuItem := Descendant("OpenItem", 101);
         IF (menuItem.Label = "&Close Interactive Log")
            FEVCloseFile;
         END IF;
         EndFailureEffects;
         ShowAnalView(FALSE);
         returnToRBD := TRUE;
      {*** Control Menu ***}
      WHEN 201: {Fail Item}
         typeOfCursor := dialogC;
         NEW(failRepairBox);
         ASK failRepairBox TO LoadFromLibrary(dialogs, "FailRepairBox");
         ASK window TO AddGraphic(failRepairBox);
         ASK failRepairBox TO SetLabel("Fail Item");
         ASK failRepairBox TO ReceiveData("fail");
         DISPOSE(failRepairBox);
         typeOfCursor := fevC;
      WHEN 202: {Repair Item}
         typeOfCursor := dialogC;
         NEW(failRepairBox);
         ASK failRepairBox TO LoadFromLibrary(dialogs, "FailRepairBox");
         ASK window TO AddGraphic(failRepairBox);
         ASK failRepairBox TO SetLabel("Repair Item");
         ASK failRepairBox TO ReceiveData("repair");
         DISPOSE(failRepairBox);
         typeOfCursor := fevC;
      WHEN 203: {Decrease Phase}
         IF phaseNumber=1
            ASK System TO FEVPhaseChange(activePhases);
         ELSE
            ASK System TO FEVPhaseChange(phaseNumber-1);
         END IF;
      WHEN 204: {Increase Phase}
         IF phaseNumber=activePhases
            ASK System TO FEVPhaseChange(1);
         ELSE
            ASK System TO FEVPhaseChange(phaseNumber+1);
         END IF;
      WHEN 205: {GoTo Phase}
         menu := Descendant("GotoPhaseMenu",205);
         lastClicked := ASK menu LastPicked 
         FOR j:=1 TO activePhases
            IF (lastClicked.Id = (2050+j))
               menuItem := ASK menu Child(INTTOSTR(j), 2050+j);
               i := POSITION(menuItem.Label(), " - ");
               phaseId := STRTOINT(SUBSTR(i+3, i+12, menuItem.Label + "      "));
               ASK System TO FEVPhaseChange(phaseId);
            END IF;
         END FOR;
      WHEN 206: {Refresh Phase}
         RefreshPhase;
      WHEN 207: {Refresh All}
         RefreshAllFEV;
      {*** Options Menu ***}
      WHEN 301: {Display Failure Effects}
         CreateFMECAarray;
         DisplayFailureEffects;
      WHEN 302: {Hide/Show Status Symbol}
         FEVIndicateStatus;
      WHEN 3031: {Small}
         symbolScale := .3;
         symbolOffset := 34.;
         ASK greenFace   TO SetScaling(symbolScale,symbolScale);
         ASK greenFace   TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK yellowFace  TO SetScaling(symbolScale,symbolScale);
         ASK yellowFace  TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK redFace     TO SetScaling(symbolScale,symbolScale);
         ASK redFace     TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK greenFace   TO Erase;
         ASK yellowFace  TO Erase;
         ASK redFace     TO Erase;   
         ASK faceVisible TO Draw; 
      WHEN 3032: {Medium}
         symbolScale := .45;
         symbolOffset := 47.;
         ASK greenFace   TO SetScaling(symbolScale,symbolScale);
         ASK greenFace   TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK yellowFace  TO SetScaling(symbolScale,symbolScale);
         ASK yellowFace  TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK redFace     TO SetScaling(symbolScale,symbolScale);
         ASK redFace     TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK greenFace   TO Erase;
         ASK yellowFace  TO Erase;
         ASK redFace     TO Erase;   
         ASK faceVisible TO Draw; 
      WHEN 3033: {Large}
         symbolScale := .6;
         symbolOffset := 62.;
         ASK greenFace   TO SetScaling(symbolScale,symbolScale);
         ASK greenFace   TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK yellowFace  TO SetScaling(symbolScale,symbolScale);
         ASK yellowFace  TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK redFace     TO SetScaling(symbolScale,symbolScale);
         ASK redFace     TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK greenFace   TO Erase;
         ASK yellowFace  TO Erase;
         ASK redFace     TO Erase;   
         ASK faceVisible TO Draw; 
      WHEN 3034: {Grande}
         symbolScale := .75;
         symbolOffset := 76.;
         ASK greenFace   TO SetScaling(symbolScale,symbolScale);
         ASK greenFace   TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK yellowFace  TO SetScaling(symbolScale,symbolScale);
         ASK yellowFace  TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK redFace     TO SetScaling(symbolScale,symbolScale);
         ASK redFace     TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK greenFace   TO Erase;
         ASK yellowFace  TO Erase;
         ASK redFace     TO Erase;   
         ASK faceVisible TO Draw; 
      WHEN 3041:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Airc_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Airc_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Airc_R");
         systemImage := "Aircraft";
      WHEN 3042:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Arti_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Arti_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Arti_R");
         systemImage := "Artillery";
      WHEN 3043:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Circ_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Circ_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Circ_R");
         systemImage := "Circuit_Card";
      WHEN 3044:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Comp_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Comp_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Comp_R");
         systemImage := "Computer";
      WHEN 3045:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Bull_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Bull_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Bull_R");
         systemImage := "Bull's_eye";
      WHEN 3046:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Face_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Face_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Face_R");
         systemImage := "Face";
      WHEN 3047:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Heli_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Heli_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Heli_R");
         systemImage := "Helicopter";
      WHEN 3048:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Miss_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Miss_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Miss_R");
         systemImage := "Missile";
      WHEN 3049:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Netw_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Netw_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Netw_R");
         systemImage := "Network";
      WHEN 3050:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Equa_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Equa_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Equa_R");
         systemImage := "Equalizer";
      WHEN 3051:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Radi_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Radi_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Radi_R");
         systemImage := "Radio";
      WHEN 3052:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Rado_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Rado_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Rado_R");
         systemImage := "Radome";
      WHEN 3053:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Mete_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Mete_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Mete_R");
         systemImage := "Meter";
      WHEN 3054:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Sate_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Sate_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Sate_R");
         systemImage := "Satellite";
      WHEN 3055:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Ship_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Ship_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Ship_R");
         systemImage := "Ship";
      WHEN 3056:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Stop_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Stop_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Stop_R");
         systemImage := "Stoplight";
      WHEN 3057:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Tank_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Tank_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Tank_R");
         systemImage := "Tank";
      WHEN 3058:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Trai_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Trai_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Trai_R");
         systemImage := "Train";
      WHEN 3059:
         statusIndChanged := TRUE;
         ASK greenFace TO LoadFromLibrary(images, "Truc_G"); 
         ASK yellowFace TO LoadFromLibrary(images, "Truc_Y"); 
         ASK redFace TO LoadFromLibrary(images, "Truc_R");
         systemImage := "Truck";
      {*** Navigate Menu ***}
      WHEN 401: {Home}
         Home;
      WHEN 402: {Up}
         UpOneLevel;
      WHEN 403: {Goto}
         GotoHier;
      WHEN 404: {Zoom In}
         Zoom("in");
      WHEN 405: {Zoom Out}
         Zoom("out");
      WHEN 406: {Zoom Percent}
         IF activeWindow > 0
            tempHier := ASK root Child("RBDHier", activeWindow)
            ZoomPercent(tempHier.xOrigin, tempHier.yOrigin, FALSE);
         ELSE
            ZoomPercent(xOrigin, yOrigin, FALSE);
         END IF;
      WHEN 408: {Display Tree}
         DisplayTree;
      {*** Help Menu ***}
      WHEN 501: {Help Index}
         CallHelp(73);
      WHEN 502: {Visit Raptor Webpage}
         VisitRaptorWebpage;
      OTHERWISE
      END CASE;
      IF statusIndChanged
         ASK greenFace TO SetScaling(symbolScale,symbolScale);
         ASK greenFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK yellowFace TO SetScaling(symbolScale,symbolScale);
         ASK yellowFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK redFace TO SetScaling(symbolScale,symbolScale);
         ASK redFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff));
         ASK greenFace TO Erase;
         ASK yellowFace TO Erase;
         ASK redFace TO Erase;  
         ASK faceVisible TO Draw;
         statusIndChanged := FALSE;
         somethingChanged := TRUE;
      END IF;
      Update;
   END METHOD; {BeSelected}
   
   ASK METHOD SetNavigation;
   BEGIN
      IF totalHiers > 0
         menuItem := Descendant("GotoItem", 403);
         ASK menuItem TO Activate;
      ELSE
         menuItem := Descendant("GotoItem", 403);
         ASK menuItem TO Deactivate;
      END IF;
      IF activeWindow = 0
         menuItem := Descendant("HomeItem",401);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("UpItem",402);
         ASK menuItem TO Deactivate;
      ELSE
         menuItem := Descendant("HomeItem",401);
         ASK menuItem TO Activate;
         menuItem := Descendant("UpItem",402);
         ASK menuItem TO Activate;
      END IF;
   END METHOD; {SetNavigation}
 
   ASK METHOD Initialize;
   VAR
      menu : MenuObj;
      tempHier : RBDHierObj;
      phase : PhaseObj;
      tempString : STRING;
      bla : ANYOBJ;
   BEGIN
      SetNavigation;
      menuItem := Descendant("WriteItem",102);
      ASK menuItem TO Deactivate; 
      menuItem := Descendant("StatusItem", 302); 
      ASK menuItem TO SetLabel("Hide Status Indicator");
      IF activePhases = 0
         menuItem := Descendant("IncPhaseItem",204);
         ASK menuItem TO Deactivate;
         menuItem := Descendant("DecPhaseItem",203);
         ASK menuItem TO Deactivate;
         menu := Descendant("GotoPhaseMenu",205);
         FOR i := 1 TO menu.numberGraphicsIn
            menuItem := ASK menu FirstGraphic();
            ASK menu TO RemoveThisGraphic(menuItem);
            DISPOSE(menuItem);
         END FOR;
         ASK menu TO Deactivate;
         menuItem := Descendant("RefPhaseItem",206);
         ASK menuItem TO Deactivate;
      ELSE
         menuItem := Descendant("IncPhaseItem",204);
         ASK menuItem TO Activate;
         menuItem := Descendant("DecPhaseItem",203);
         ASK menuItem TO Activate;
         menu := Descendant("GotoPhaseMenu",205);
         ASK menu TO Activate;
         menuItem := Descendant("RefPhaseItem",206);
         ASK menuItem TO Activate;
         FOR i := 1 TO menu.numberGraphicsIn
            menuItem := ASK menu FirstGraphic();
            ASK menu TO RemoveThisGraphic(menuItem);
            DISPOSE(menuItem);
         END FOR;
         FOR i := 1 TO activePhases
            phase := phaseObjArray[i];
            IF phase.ID < 10
               tempString := "00" + INTTOSTR(phase.ID);
            ELSIF phase.ID < 100
               tempString := "0" + INTTOSTR(phase.ID);
            ELSE
               tempString := INTTOSTR(phase.ID);
            END IF;
            NEW(menuItem);
            ASK menuItem TO SetLabel(phase.phaseName + " - " + tempString); 
            ASK menuItem TO SetReferenceName(INTTOSTR(i));
            ASK menuItem TO SetId(2050 + i); 
            ASK menu TO AddGraphic(menuItem);
         END FOR;
         Draw;
         menuItem := Descendant("RefAllItem",207);
         ASK menuItem TO Activate;
      END IF;
   END METHOD; {Initialize}
END OBJECT; {fevMenuObj}

{*** FEV Toolbar ***}
OBJECT fevToolObj;
   ASK METHOD BeSelected;
   BEGIN
      IF ignoreMouse {eag error 51 fix}
         RETURN;
      END IF;
      CASE ASK LastPicked Id
      WHEN 901: {Print RBD}
         PrintWindow;    
      WHEN 902: {Return Button}
         menuItem := ASK fevMenuBar Descendant("OpenItem", 101);
         IF (menuItem.Label = "&Close Interactive Log")
            FEVCloseFile;
         END IF;
         EndFailureEffects;
         ShowAnalView(FALSE);
         returnToRBD := TRUE;   
      WHEN 903: {Open Interactive File}
         FEVOpenFile;         
      WHEN 904: {Write To Interactive File}
         CreateFMECAarray;
         WriteFMECAtoFile;
      WHEN 905: {Close Interactive File}
         FEVCloseFile;
      WHEN 906: {DecrementPhase}
         IF phaseNumber=1
            ASK System TO FEVPhaseChange(activePhases);
         ELSE
            ASK System TO FEVPhaseChange(phaseNumber-1);
         END IF;
      WHEN 907: {Increment Phase}
         IF phaseNumber=activePhases
            ASK System TO FEVPhaseChange(1);
         ELSE
            ASK System TO FEVPhaseChange(phaseNumber+1);
         END IF;
      WHEN 908: {Refresh Phase}
         RefreshPhase;
      WHEN 909: {Refresh All}
         RefreshAllFEV;
      WHEN 910: {Display Failure Effects}
         CreateFMECAarray;
         DisplayFailureEffects;
      WHEN 911: {Home}
         Home;
      WHEN 912: {Up}
         UpOneLevel;
      WHEN 913: {Goto}
         GotoHier;
      WHEN 914: {Zoom Out}
         Zoom("out");
      WHEN 915: {Zoom In}
         Zoom("in");
      WHEN 941: {Zoom Fit}
         ZoomFit;
      WHEN 917: {Display Tree}
         DisplayTree;
      WHEN 918: {Help}
         CallHelp(83);
      END CASE;
      Update;
   END METHOD; {BeSelected}

   ASK METHOD SetNavigation;
   BEGIN
      IF totalHiers > 0
         buttItem := Descendant("GotoButton", 913);
         ASK buttItem TO Activate;
      ELSE
         buttItem := Descendant("GotoButton", 913);
         ASK buttItem TO Deactivate;
      END IF;
      IF activeWindow = 0
         buttItem := Descendant("HomeButton",911);
         ASK buttItem TO Deactivate;
         buttItem := Descendant("UpButton",912);
         ASK buttItem TO Deactivate;
      ELSE
         buttItem := Descendant("HomeButton",911);
         ASK buttItem TO Activate;
         buttItem := Descendant("UpButton",912);
         ASK buttItem TO Activate;
      END IF;
   END METHOD;     
   
   ASK METHOD Initialize;
   BEGIN
      SetNavigation;
      buttItem := Descendant("CloseFile",905);
      ASK buttItem TO Deactivate;
      buttItem := Descendant("EditFile",904);
      ASK buttItem TO Deactivate;
      IF activePhases = 0
         buttItem := Descendant("DecPhase",906);
         ASK buttItem TO Deactivate;
         buttItem := Descendant("IncPhase",907);
         ASK buttItem TO Deactivate;
         buttItem := Descendant("ClearAll",908);
         ASK buttItem TO Deactivate;
      ELSE
         buttItem := Descendant("DecPhase",906);
         ASK buttItem TO Activate;
         buttItem := Descendant("IncPhase",907);
         ASK buttItem TO Activate;
         buttItem := Descendant("ClearAll",908);
         ASK buttItem TO Activate;
      END IF;
   END METHOD; {Initialize}
END OBJECT; {fevToolObj}

END MODULE. {imod menubar}


