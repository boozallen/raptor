 {++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Implementation Module : Display                                               +}
{+  Author        : Steve Brown                                                   +}
{+  Modified      : Sept 4, 2008  wds/TES                                         +}
{+  Description   : This module initializes the grid area and handles all actions +}
{+                  that occur on the grid - mouseclicks, mouse movement, adding  +}
{+                  or removing items, selecting items, and the pop up menu.      +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

IMPLEMENTATION MODULE Display;  

FROM GTypes   IMPORT ALL ColorType, DBPositionType(BottomLeft), ALL SysCursorType,
                     TextBufferType, WindowMapType(XMajorMap), PointType, PointArrayType,
                     TextFontType(SystemFont,RomanFont,CourierFont,SystemText), 
                     ALL FillStyleType, ALL GraphPartType, ALL WindowStateType,
                     ALL LineStyleType, TextHorizType(HorizLeft),TextVertType(VertMiddle),
                     ALL SnapHorizType, ALL SnapVertType, OptionListType;  
FROM GProcs   IMPORT Transform;                 
FROM GTab     IMPORT TabObj;
FROM Control  IMPORT ControlVObj;
FROM Button   IMPORT ButtonObj;         
FROM TextBox  IMPORT ComboBoxObj, TextBoxObj;
FROM MathMod  IMPORT POWER,CEIL;               
FROM Radio    IMPORT RadioBoxObj, RadioButtonObj;
FROM Value    IMPORT ValueBoxObj;
FROM Cursor   IMPORT RectCursorObj, FixedAspCursorObj;
FROM Check    IMPORT CheckBoxObj;
FROM Label    IMPORT LabelObj;
FROM Text     IMPORT TextObj;
FROM Line     IMPORT PolylineObj; 
FROM UtilMod  IMPORT ClockTimeSecs,ClockRealSecs,GetNumArgs,GetCmdLineArg, DateTime;
FROM OSMod    IMPORT Delay, GetProgDir, FileExists, StartBGTask, GetOSType, SystemCall,
                     CheckBGTask, DirName, BaseName, AppendSlash, DeleteFile;
FROM IOMod    IMPORT StreamObj,FileUseType(Output,Input, Append, BinaryInput, BinaryOutput);  
FROM Runsim   IMPORT StartEngine, FinalArray, AvailGraph,
                     StartFEVmode, EndFEVmode, FEVmode,FevDepGroup; 
FROM Analyze  IMPORT RunEFPA;  
FROM FileFx   IMPORT CloseFFile, OpenFile, SaveFile, OpenPathsCFG, menuPath, menuFile, loadingFile, versionStr; 
FROM Intract  IMPORT HelpBoxObj, NodeBoxObj, PrefsBoxObj, SimBoxObj, BlockPropObj, SendAlert,
                     EventsBoxObj, MassEditBoxObj, ConvertToString, ResimBoxObj, GetSpareList, GetResList, 
                     GetTrigList, doorOpen, HierBoxObj, MakeDistString;                     
FROM Objects  IMPORT arrowhead, RBDBasicObj, directionType(INTO, OUTOF, MINUSINTO, MINUSOUTOF),
                     resetting, poolGroup, triggerGroup, spareList, resList, trigList, coldList,
                     boolArray, ALL SparingType, InitFactorySettings, RunBogusRBD,ALL BlockStatusType,
                     ALL EventStatusType, critFactor, deferTrig, CheckColds, PhaseObj {cmc};
FROM Fill     IMPORT FillVObj;
FROM GSnap    IMPORT SnapShotObj; 
FROM Menu     IMPORT MenuItemObj;
FROM Menubar  IMPORT inStepMode, inEndState, UpOneLevel, Home, FindItem, MassEdit, PauseSim, ResumeSim,
                     ColorPrefs, ViewOutputs, notPaused, DisplayFailureEffects, GotoHier, Step, Jump, FEVStream, 
                     ReturnToWorkspace;
FROM Form     IMPORT DialogBoxObj, MessageDialogBoxObj;

FROM OSMod    IMPORT AppendSlash, MakeDir;   { wds/TES, 8/18/08 }
   
VAR
   fromBlockRef                                            : STRING;
   fromBlockId, i ,lastMove, pasteBlocks, pasteEvents, displaySetting, pasteHiers, 
   startHier, startOut, rtn                                                 : INTEGER;
   lastHeight, lastWidth, oldX, oldY, 
   linkStartX, linkStartY, linkEndX, linkEndY, upLimit, downLimit,
   rightLimit, leftLimit, refX, refY, boxStartX, boxStartY, scale,
   boxEndX, boxEndY, oldClickX, oldClickY, sampleRate, yShift, 
   curOffX, curOffY, lastResize, screenOffset, oldx, oldy, lastZoom, outxdiff              : REAL;
   editFlag, linkMsgExists, draggingStuff, draggingblock,draggingevent,draggingnode,dragginghier,
   draggingSymbol, drawingBox, 
   drawingRed, linkSelected, awaitingResponse, scrollScreen, workingPaste, 
   depMsg, noCapVals, phasingInUse                                     : BOOLEAN;  
   linkMessage                                                         : ImageObj;
   linkText                                                            : TextObj;
   message                                                             : TextBufferType;
   zoomRubber                                                          : FixedAspCursorObj;
   rubberBox                                                           : RectCursorObj;
   partialFromGroup, partialToGroup                                    : QueueObj;
   current, parent                                                             : RBDBasicObj;
   selected, currentObj                                                : ImageObj;
   copyLinks                                                           : ARRAY INTEGER OF LinkObj;
   copyNodes                                                           : ARRAY INTEGER OF RBDNodeObj;
   copyBlocks                                                          : ARRAY INTEGER OF RBDBlockObj;
   copyEvents                                                          : ARRAY INTEGER OF RBDEventObj;
   copyHiers                                                           : ARRAY INTEGER OF RBDHierObj;
   widescreen : BOOLEAN;           {cmc 5/10/07}
   widescreenOffset : REAL;         {cmc 5/10/07}
   
{wds/TES - mods for finding user's "Application Data" directory, 8/14/08}
PROCEDURE getUserFolderPathAux(IN cstr: CArrayType);
BEGIN
   userPath := AppendSlash( CHARTOSTR( cstr )) + AppendSlash( "Raptor7" ); { convert to MODSIM string }
END PROCEDURE;

{wds/TES - mods for finding user's "MyDocuments" directory, 9/4/08}
PROCEDURE getUserMyDocsPathAux(IN cstr: CArrayType);
BEGIN
   docsPath := AppendSlash( CHARTOSTR( cstr )) + AppendSlash( "Raptor7" ); { convert to MODSIM string }
END PROCEDURE;


PROCEDURE InitDisplay(OUT exploded : BOOLEAN);
VAR
   fileToOpen, nextString, opSys,
   month, year, today, bmpToLoad, pword   : STRING;
   kill, day                              : INTEGER;
   saveCancelled, picUp, recovering, done, cjm : BOOLEAN;
   
   defaultStream                          : StreamObj;
   menuitem                               : MenuItemObj;
   buttitem                               : PaletteButtonObj;
   testBox                                : DialogBoxObj;
   testLabel                              : LabelObj;
   bitmap                                 : SnapShotObj;
   dialogBox : HelpBoxObj;  {remove with unreliable box}
   button    : ButtonObj;   {remove with unreliable box}
   radBox    : RadioBoxObj; {remove with unreliable box}
   screenX, screenY :INTEGER;                               {cmc 5/10/07}

   retval                                 : INTEGER;        { wds/TES, 8/18/08 }

BEGIN 
  
   NEW(blockGroup);
   NEW(nodeGroup);
   NEW(eventGroup);
   NEW(hierGroup);
   NEW(linkGroup);
   NEW(capLinkGroup);
   NEW(capNodeGroup);
   DateTime(today);
   GetPassword(today, pword);                                                                               
   IF GetNumArgs > 0
      REPEAT
         INC(i);
         GetCmdLineArg(i,nextString);
         IF nextString = "/d"
            diagnostics:=TRUE;
         ELSIF nextString = "/sd"
            simDetails:=TRUE;
         ELSIF nextString = "/cjm"
            cjm:=TRUE;
         ELSIF nextString = "/dev"
            devmode:=TRUE;
         ELSIF nextString = "/C130"     {cmc}
            C130File:=TRUE;             {cmc}                       
         ELSIF SUBSTR(1, 9, nextString) = "/password"
            IF nextString = "/password-"+ pword
               password := TRUE;
            ELSE
               password := FALSE;
            END IF;
         ELSIF nextString <> ""
            fileToOpen := fileToOpen+nextString+" ";
            openFromIcon := TRUE;
         END IF;
      UNTIL nextString = "";
   END IF;
   day := STRTOINT(SUBSTR(9, 10, today));
   month := SUBSTR(5, 7, today);
   year := SUBSTR(STRLEN(today)-3, STRLEN(today), today);
   versionStr := "RAPTOR 7.0";
   compileType:=CompileType;
   IF compileType = "demo"
      devVersion := "7.0.12 Demo"; 
   ELSIF compileType = "student"
      devVersion := "7.0.12 Student"; 
   ELSIF compileType = "release"
      devVersion := "7.0.12"; {Pip}               
   ELSIF compileType = "gmd"
      devVersion := "6.5.4"; {}
      versionStr := "RAPTOR 6.5";
   ELSE
      RETURN;
   END IF;
   devDate := "June 2009";                          { wds/TES, 8/18/08 }
   nowInitialing := TRUE;
   nameOfFile := "Unnamed RBD";
   
   {+++++++++++ Find the User's Application Data directory, wds/TES 8/14/08 +++++++++++}
   getUserFolderPath();
   retval := MakeDir( userPath );  { if directory already exists, it appears to do nothing }
   getUserMyDocsPath();
   retval := MakeDir( docsPath );  { if directory already exists, it appears to do nothing }  
   
   
   IF compileType = "release"
      pathName := LOWER(GetProgDir("Raptor7.exe"));
   ELSIF compileType = "demo"
      pathName := LOWER(GetProgDir("Raptor7Demo.exe"));
   ELSIF compileType = "student"
      pathName := LOWER(GetProgDir("Raptor7Student.exe"));
   ELSIF compileType = "gmd"
      pathName := LOWER(GetProgDir("Raptor65.exe"));
   END IF;
   
   graphicsPath := pathName+"graphics\";
   installPath := pathName;  {constant}
   {rbdPath := docsPath {+"examples\"};       { wds/TES, 9/4/08 }     } {cmc 10/8/08}
   reportPath := docsPath  {+"reports\"};    { wds/TES, 9/4/08 }
   libraryPath:= pathName+"Library\";
   exampPath := pathName+"examples\";
   soundsPath := pathName+"sounds\";
   soundPath := GetProgDir("Sndrec32.exe");
   filter := "*.rbd";
   simColor:= White;
   workColor:=MediumGoldenrod;
   goggleColor:=LightBlue;
   gridColor:= Goldenrod;
   linkWorkColor:= Black;
   textWorkColor:= Black;
   blockGUIColor:= DarkGreen;
   globalUnits := "hours";
   systemUnits := globalUnits;
   gridIsOn := TRUE;
   hierLevel := 0;
   deepestLevel := 0;
   nextId := 0; {default block is first one given id}
   nextLinkId := 1;
   currentView := "workspace"
   {***}
   IF diagnostics 
      NEW(diagnosticsStream); 
      diagFile:=installPath + INTTOSTR(day)+month+SUBSTR(12,13,today)+SUBSTR(15,16,today)+".TXT";
      ASK diagnosticsStream TO Open(diagFile, Output);{Change 'Output' to 'Append' if writing more}
      nextString:="System today String =  " + today;
      ASK diagnosticsStream TO WriteString(nextString);   
      ASK diagnosticsStream TO WriteLn; 
      nextString:="Raptor day month year = " + INTTOSTR(day) + " " + month + " " + year;
      ASK diagnosticsStream TO WriteString(nextString);   
      ASK diagnosticsStream TO WriteLn; 
      nextString:="devVersion " + devVersion;
      ASK diagnosticsStream TO WriteString(nextString);   
      ASK diagnosticsStream TO WriteLn; 
      nextString:="installPath = " + installPath;
      ASK diagnosticsStream TO WriteString(nextString);   
      ASK diagnosticsStream TO WriteLn; 
      nextString:="fileToOpen = " + fileToOpen;
      ASK diagnosticsStream TO WriteString(nextString);   
      ASK diagnosticsStream TO WriteLn; 
      ASK diagnosticsStream TO Close;
   END IF; 
   
   
   IF simDetails 
      NEW(simDetailsStream); 
      ASK simDetailsStream TO Open(installPath + "simDetails.TXT", Output);{Change 'Output' to 'Append' if writing more}
      nextString:="System today String =  " + today;
      ASK simDetailsStream TO WriteString(nextString);   
      ASK simDetailsStream TO WriteLn; 
      nextString:="Raptor day month year = " + INTTOSTR(day) + " " + month + " " + year;
      ASK simDetailsStream TO WriteString(nextString);   
      ASK simDetailsStream TO WriteLn; 
      nextString:="devVersion " + devVersion;
      ASK simDetailsStream TO WriteString(nextString);   
      ASK simDetailsStream TO WriteLn; 
      ASK simDetailsStream TO Close; 
   END IF; 
      
   {***}
   
   
   
   NEW(window);
   ASK window TO SetSize(100.,100.);
   lastWidth := 100.;
   lastHeight := 100.;
   IF compileType = "demo"
      ASK window TO SetTitle("RAPTOR 7.0 DEMO - "+nameOfFile);
   ELSIF compileType = "student"
      ASK window TO SetTitle("RAPTOR 7.0 STUDENT - "+nameOfFile);
   ELSIF compileType = "gmd"
      ASK window TO SetTitle("RAPTOR 6.5 - "+nameOfFile);
   ELSE
      ASK window TO SetTitle("RAPTOR 7.0 - "+nameOfFile);
   END IF;
   ASK window TO ShowWorld(0.,0.,960.,960.);
   ASK window TO SetMappingMode(XMajorMap);
   ASK window TO SetMoveMonitoring(TRUE);
   ASK window TO SetSysCursor(BusyCursor);
   ASK window TO SetPanes;
   ASK window TO SetScrollable(TRUE,TRUE);

   screenX:= getXres();                            {cmc 5/10/07}   {start}
   screenY:= getYres();
   IF ( FLOAT(screenX)/FLOAT(screenY) > 1.5)
      widescreen:=TRUE;
      widescreenOffset := 120.0;         
   ELSE
      widescreen:=FALSE;
      widescreenOffset := 0.0;         
   END IF;                                        {cmc 5/10/07}    {end}
   

IF diagnostics
      ASK diagnosticsStream TO Open(diagFile, Append);
      ASK diagnosticsStream TO WriteString("Screen Width is: ");  
      ASK diagnosticsStream TO WriteString(INTTOSTR(screenX));
      ASK diagnosticsStream TO WriteLn; 
      ASK diagnosticsStream TO WriteString("Screen Height is: ");
      ASK diagnosticsStream TO WriteString(INTTOSTR(screenY));
      ASK diagnosticsStream TO WriteLn; 
      IF window.PixelWidth > 1.800 {640 X 480}
         nextString := "640 X 480"; 
      ELSIF window.PixelWidth > 1.469 {800 X 600}
         nextString := "800 X 600"; 
      ELSIF window.PixelWidth > 1.212 {1024 X 768}
         nextString := "1024 X 768"; 
      ELSIF window.PixelWidth > 1.095
         nextString := "???"; 
      ELSIF window.PixelWidth > 1.00
         nextString := "???"; 
      ELSIF window.PixelWidth > 0.891 {1280 X 1024}
         nextString := "1280 X 1024"; 
      ELSE
         nextString := "anything else";
      END IF;
      ASK diagnosticsStream TO WriteString("Resolution assumed to be " + nextString);
      ASK diagnosticsStream TO WriteLn; 
      ASK diagnosticsStream TO Close; 
   END IF;
   IF window.PixelWidth > 1.800 {640 X 480}
      displaySetting := 640;
      NEW(message, 1..2);
      message[1] := "The screen resolution in use is not supported.  Some dialog    ";
      message[2] := "boxes will be clipped and controls may not be accessible.     ";
      result := SendAlert(message, FALSE, FALSE, FALSE);
      DISPOSE(message);      
      screenOffset := 66000000.;
   ELSIF window.PixelWidth > 1.469 {800 X 600}
      displaySetting := 800;
      screenOffset := 59000000.;
   ELSIF window.PixelWidth > 1.212 {1024 X 768}
      displaySetting := 1024;
      screenOffset := 7000000.;
   ELSIF window.PixelWidth > 1.095
      {displaySetting := ????;}
      screenOffset := -16500000.;
   ELSIF window.PixelWidth > 1.00
      {displaySetting := ????;}
      screenOffset := -33500000.;
   ELSIF window.PixelWidth > 0.891 {1280 X 1024}
      displaySetting := 1280;
      screenOffset := -130000000.; {-130000000.}
   ELSE
      {displaySetting := ????;}
      screenOffset := -63000000.;
   END IF;
   cusZoomVal := 24.;
   ASK window TO Draw;
      
   {***}
   IF diagnostics 
      ASK diagnosticsStream TO Open(diagFile, Append);
      nextString:="Raptor main window initialized......";
      ASK diagnosticsStream TO WriteString(nextString);   
      ASK diagnosticsStream TO WriteLn; 
      ASK diagnosticsStream TO Close; 
   END IF; 
   {***}
         
   NEW(bitmap);
   ASK window TO AddGraphic(bitmap);
   CASE month
      WHEN "Jul":
         IF (day >= 2) AND (day <= 4)
            bmpToLoad := "IntroFlag";
         END IF;
      WHEN "Dec": 
         IF (day >= 18) AND (day <= 31)
            bmpToLoad := "IntroXmas";
         END IF;
      OTHERWISE
         bmpToLoad := "";
   END CASE;
   IF bmpToLoad = ""
      IF compileType = "demo"
         bmpToLoad := "IntroDemo"; {IntroDemo}
      ELSIF compileType = "student"
         bmpToLoad := "IntroStudent"; {IntroStudent}
      ELSIF compileType = "gmd"
         bmpToLoad := "Intro65";
      ELSE   
         bmpToLoad := "Intro7";
      END IF;
   END IF;
   IF FileExists(graphicsPath + bmpToLoad+".bmp");
      ASK bitmap TO SetFile(graphicsPath + bmpToLoad); 
      ASK bitmap TO SetAlignment(SnapHorizCentered, SnapVertMiddle);
      ASK bitmap TO SetTranslation(480.,340.);
      ASK bitmap TO Draw;
      IF (NOT devmode)
         picUp := TRUE;
      END IF;
      {***}
      IF diagnostics 
         ASK diagnosticsStream TO Open(diagFile, Append);
         nextString:="Displayed bitmap = "+ bmpToLoad+".bmp" ;
         ASK diagnosticsStream TO WriteString(nextString);   
         ASK diagnosticsStream TO WriteLn; 
         ASK diagnosticsStream TO Close; 
      END IF; 
      {***}
   END IF;
      
   NEW(baseroot);
   ASK window TO AddGraphic(baseroot);
   NEW(scaleroot);
   ASK baseroot TO AddGraphic(scaleroot);
   NEW(root);
   ASK scaleroot TO AddGraphic(root);
   typeOfCursor := dialogC;
   
   NEW(sound);
   fileIsOpen := TRUE;
   IF FileExists(exampPath + "cksrule.rbd");
      NEW(message,1..2);
      message[1] := "RAPTOR appears to have been abnormally exited during the last session.     ";
      message[2] := "would you like to recover the .rbd from that session?     ";
      result := SendAlert(message, FALSE, TRUE, FALSE);
      DISPOSE(message);
      IF result
         fileToOpen := exampPath + "cksrule.rbd";
         recovering := TRUE;
      ELSE
         kill := DeleteFile(exampPath + "cksrule.rbd");
      END IF;
   END IF;
   IF FileExists(userPath + "prefs70.cfg")   { wds/TES, 8/15/08 }
      NEW(defaultStream);
      ASK defaultStream TO Open((userPath + "prefs70.cfg"), Input);   { wds/TES, 8/15/08 }
      IF defaultStream.ioResult <> 0
         globalImage := "Equalizer";
         systemImage := globalImage;
         saveIsOn := FALSE;
         saveInc := 15;
         soundIsOn:=TRUE;
         lambdaMode:=FALSE;
         muSigmaMode:=FALSE; 
         globalUnits := "hours";
         systemUnits := globalUnits;
         RETURN;
      ELSE
         ASK defaultStream TO ReadLine(nextString);
         IF SUBSTR(1,15,nextString) = "Raptor 7.0 Pref"  
            ASK defaultStream TO ReadLine(nextString);
            ASK defaultStream TO ReadLine(nextString);
            globalImage := SUBSTR(29, STRLEN(nextString), nextString);
            systemImage := globalImage;
            ASK defaultStream TO ReadLine(nextString);
            globalUnits := SUBSTR(26, STRLEN(nextString), nextString);
            systemUnits := globalUnits;
            ASK defaultStream TO ReadLine(nextString);
            IF SUBSTR(13, STRLEN(nextString), nextString) = "ON"
               saveIsOn:=TRUE;
            END IF;
            ASK defaultStream TO ReadLine(nextString);
            saveInc := STRTOINT(SUBSTR(26, 31, nextString))*60;
            ASK defaultStream TO ReadLine(nextString);
            IF SUBSTR(10, STRLEN(nextString), nextString) = "ON"
               soundIsOn:=TRUE;
            END IF;
            ASK defaultStream TO ReadLine(nextString);
            IF SUBSTR(16, STRLEN(nextString), nextString) = "ON"
               lambdaMode:=TRUE;
            END IF;
            ASK defaultStream TO ReadLine(nextString);
            IF SUBSTR(18, STRLEN(nextString), nextString) = "ON"
               muSigmaMode:=TRUE;
            END IF;
            ASK defaultStream TO ReadLine(nextString);
         ELSE
            globalImage := "Equalizer";
            systemImage := globalImage;
            saveIsOn := FALSE;
            saveInc := 15;
            soundIsOn:=TRUE;
            lambdaMode:=FALSE;
            muSigmaMode:=FALSE;      
            globalUnits := "hours";
            systemUnits := globalUnits;
         END IF;
      END IF;
      DISPOSE(defaultStream);
      {***}
      IF diagnostics 
         ASK diagnosticsStream TO Open(diagFile, Append);
         nextString:="Prefs70.cfg opened and read";
         ASK diagnosticsStream TO WriteString(nextString);   
         ASK diagnosticsStream TO WriteLn; 
         ASK diagnosticsStream TO Close; 
      END IF; 
      {***}
   ELSE
      globalImage := "Equalizer";
      systemImage := globalImage;
      globalUnits := "hours";
      systemUnits := globalUnits;
      saveIsOn := FALSE;
      saveInc := 15;
      opSys := GetOSType();
      IF (POSITION(opSys, "UNICODE") > 0) OR (soundPath = "")
         soundIsOn := FALSE;
      ELSE 
         soundIsOn:=TRUE;
      END IF;
      lambdaMode:=FALSE;
      muSigmaMode:=FALSE;      
      {***}
      IF diagnostics 
         ASK diagnosticsStream TO Open(diagFile, Append);
         nextString:="Prefs70.cfg not found...defaults set";
         ASK diagnosticsStream TO WriteString(nextString);   
         ASK diagnosticsStream TO WriteLn; 
         ASK diagnosticsStream TO Close; 
      END IF; 
      {***}
   END IF;
   flowGenerated := 1;
   sysLostCost := 0.0;
   systemRedCost := 0.0;
   lastSave := TRUNC(ClockRealSecs);
   IF soundIsOn AND (FileExists(soundsPath + "Intro.wav"))
      ASK sound TO PlayMe(soundsPath + "Intro.wav");
   END IF;
   IF picUp
      Delay(3);
   END IF;
   NEW(dialogs);
   IF (compileType = "release") AND (NOT training) {eag training}
      ASK dialogs TO ReadFromFile(pathName + "graphics\dialogs70release.sg2");
      NEW(testBox);
      ASK testBox TO LoadFromLibrary(dialogs,"CompileTypeBox");
      testLabel := ASK testBox Child("CompileType", 0);
      IF testLabel.Label <> "release"
         exploded := TRUE;
         NEW(message,1..3);
         message[1] := "Mismatched DLLs.  Contact the Raptor Technical Support     ";
         message[2] := "Team at raptortech@arinc.com or call 1-505-248-0718 to";
         message[3] := "obtain a licensed version.";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
         RETURN;
      END IF;
   ELSIF ((compileType = "release") AND training) OR (compileType = "demo") {eag training}
      ASK dialogs TO ReadFromFile(pathName + "graphics\dialogs70demo.sg2");
   ELSIF compileType = "student"
      ASK dialogs TO ReadFromFile(pathName + "graphics\dialogs70student.sg2");
   ELSIF compileType = "gmd"
      ASK dialogs TO ReadFromFile(pathName + "graphics\dialogs70gmd.sg2");
   END IF;      IF cjm compileType:="release"; END IF;
   ASK window TO SetColor(workColor);
   NEW(images);
   ASK images TO ReadFromFile(pathName + "graphics\images70.sg2");
   NEW(menubar);
   ASK menubar TO LoadFromLibrary(dialogs,"MenuBar");
   ASK window TO AddGraphic(menubar);
   NEW(menuTool);
   ASK menuTool TO LoadFromLibrary(dialogs,"MenuTools");
   ASK window TO AddGraphic(menuTool);
   NEW(grid);
   typeOfCursor := nilC;
   NEW(simMenuBar);
   ASK simMenuBar TO LoadFromLibrary(dialogs,"SimMenuBar");
   ASK window TO AddGraphic(simMenuBar);
   ASK simMenuBar TO SetHidden(TRUE);
   NEW(simToolBar);
   ASK simToolBar TO LoadFromLibrary(dialogs,"SimToolBar");
   ASK window TO AddGraphic(simToolBar);
   ASK simToolBar TO SetHidden(TRUE);  

   NEW(weakMenuBar);
   ASK weakMenuBar TO LoadFromLibrary(dialogs,"WeakMenuBar");
   ASK window TO AddGraphic(weakMenuBar);
   ASK weakMenuBar TO SetHidden(TRUE);
   NEW(weakToolBar);
   ASK weakToolBar TO LoadFromLibrary(dialogs,"WeakToolBar");
   ASK window TO AddGraphic(weakToolBar);
   ASK weakToolBar TO SetHidden(TRUE);  

   NEW(fevMenuBar);
   ASK fevMenuBar TO LoadFromLibrary(dialogs,"FEVMenuBar");
   ASK window TO AddGraphic(fevMenuBar);
   ASK fevMenuBar TO SetHidden(TRUE);
   NEW(fevToolBar);
   ASK fevToolBar TO LoadFromLibrary(dialogs,"FEVToolBar");
   ASK window TO AddGraphic(fevToolBar);
   ASK fevToolBar TO SetHidden(TRUE);  
   

   NEW(defaultBlock);
   ASK defaultBlock TO LoadFromLibrary(images, "RBDBlock");
   ASK defaultBlock TO SetID("RBDBlock", 0); 
   ASK root TO AddGraphic(defaultBlock);
   ASK defaultBlock TO SetHidden(TRUE);
      {***}
      IF diagnostics 
         ASK diagnosticsStream TO Open(diagFile, Append);
         nextString:="SG2 file opened and read";
         ASK diagnosticsStream TO WriteString(nextString);   
         ASK diagnosticsStream TO WriteLn; 
         ASK diagnosticsStream TO Close; 
      END IF; 
      {***}
   InitFactorySettings;
   IF FileExists(userPath + "PATHS.CFG")    { wds/TES, 8/15/08 }
      OpenPathsCFG(userPath, exampPath);    { wds/TES, 8/15/08 }
      {***}
       rbdPath := pathName;       { cmc 10/8/08 }
      
      IF diagnostics 
         ASK diagnosticsStream TO Open(diagFile, Append);
         nextString:="Paths.cfg opened and read";
         ASK diagnosticsStream TO WriteString(nextString);   
         ASK diagnosticsStream TO WriteLn; 
         ASK diagnosticsStream TO Close; 
      END IF; 
      {***}
   ELSE   
      IF compileType = "demo"  
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
      ELSE
         ASK menubar TO SetFileNames(exampPath, "Basic1.rbd", exampPath, "Distributions1.rbd",
                                    exampPath, "Logistics1.rbd", exampPath, "Events1.rbd",
                                    exampPath, "Dependence1.rbd", exampPath, "Standby1.rbd");
       END IF;
      {***}
      rbdPath := exampPath;; 
      
      IF diagnostics 
         ASK diagnosticsStream TO Open(diagFile, Append);
         nextString:="Paths.cfg file not found...defaults set";
         ASK diagnosticsStream TO WriteString(nextString);   
         ASK diagnosticsStream TO WriteLn; 
         ASK diagnosticsStream TO Close; 
      END IF; 
      {***}
   END IF;   
   DISPOSE(bitmap);
   ASK root TO AddBeforeGraphic(defaultBlock, grid);
   ASK grid TO Colorize("Normal");
   ASK menubar TO Draw; 
   xOrigin := 0.;
   yOrigin := 80.;
   SetView(cusZoomVal,0.,80.);  
   AddStartEndNodes;
   ASK grid TO Draw;  
   menuitem := ASK menubar Descendant("separator", 0);
   ASK menuitem TO Deactivate;
   menuitem := ASK menubar Descendant("InputItem",3015);
   ASK menuitem TO Deactivate;
   menuitem := ASK menubar Descendant("HierInputItem",3014);
   ASK menuitem TO Deactivate;
   IF compileType <> "release"
      menuitem := ASK menubar Descendant("RefItem", 604);
      ASK menuitem TO Deactivate;
   END IF;
   {IF NOT password
      menuitem := ASK menubar Descendant("FEVItem",502);
      ASK menuitem TO Deactivate;
      buttitem := ASK menuTool Descendant("FEVButton", 930);
      ASK buttitem TO Deactivate;
   END IF;}
   ASK menubar TO Disable(1);
   ASK menubar TO Disable(2);
   ASK menubar TO Disable(3);
   ASK menubar TO Disable(4);
   ASK menubar TO Disable(7);
   ASK menubar TO Disable(8);
   ASK menubar TO Disable(10);
   ASK menubar TO Disable(12);
   ASK menubar TO Disable(13);
   ASK menubar TO Disable(14);
   ASK menubar TO Disable(16);
   ASK menubar TO SetChecks;
   ASK window TO SetSysCursor(NormalCursor);
   
   dTimeTrunc     := 1000.;
   dTimeStartTime := 0.;
   dNumberOfRuns  := 1.;
   dTimeSlice     := 10000000.0;
   dFailTrunc     := 25.;
   dFailStartTime := 0.;
   dCycleTrunc    := 0.;
   dCycleStartTime := 0.;
   dSimWithGraph  := TRUE;                     
   termType   := 1; {time = 1, failure = 2, cycle = 3}
   negShutUp      := FALSE;
   statusBarOn    := TRUE;
   analViewAvail := FALSE;
   configFrozen := FALSE;  
   doorOpen := FALSE;
   weakLinkAnalType := 1;
   GYthreshold := 0.95;
   YRthreshold := 0.90;
   yMin := 0.0;
   yMax := 1.0;
   NEW(poolGroup);
   NEW(triggerGroup);
   NEW(spareList, 1..1);
   NEW(coldList, 1..1);
   NEW(resList, 1..1);
   NEW(trigList, 1..1);
   spareList[1] := "unnamed";
   coldList[1]  := "unnamed";
   resList[1]   := "unnamed";
   trigList[1] := "unnamed";
   NEW(sysStreams, 1..11);
   sysStreams[1] := 7;  sysStreams[3] := 9;
   sysStreams[2] := 8;  sysStreams[4] := 10;
   sysStreams[5] := 70;  sysStreams[6] := 71;
   sysStreams[7] := 72;  sysStreams[8] := 73;
   sysStreams[9] := 74;  sysStreams[10] :=75;
   sysStreams[11] := 101;
   activePhases := 0;
   NEW(selectGroup);
   NEW(selectedLinksGroup);
   NEW(partialFromGroup);
   NEW(partialToGroup);
   NEW(current);
   NEW(popupMenu);
   ASK window TO AddGraphic(popupMenu);
   ASK popupMenu TO Erase;
   nowInitialing  := FALSE;
   
   {IF NOT devmode
      NEW(dialogBox);
      ASK dialogBox TO LoadFromLibrary(dialogs, "UnreliableBox");
      ASK window TO AddGraphic(dialogBox);
      ASK dialogBox TO Draw;
      radBox := ASK dialogBox Descendant("RadBox", 0);
      button := ASK dialogBox TO AcceptInput();
      IF ASK button ReferenceName = "OKButton"
         IF radBox.SelectedButton.Id = 2
            ASK window TO BeClosed;
         END IF;
      END IF;
   END IF; }
   
   RunBogusRBD;
   
   IF fileToOpen <> ""
      REPEAT
         IF SUBSTR(STRLEN(fileToOpen), STRLEN(fileToOpen), fileToOpen) = " "
            fileToOpen := SUBSTR(1, STRLEN(fileToOpen)-1, fileToOpen);
         ELSE
            done := TRUE;
         END IF;
      UNTIL done;
      openMenuFile := TRUE;
      menuFile := BaseName(fileToOpen);
      menuPath := AppendSlash(DirName(fileToOpen));
      ASK menubar TO Disable1Thru8;
      OpenFile(TRUE, FALSE, totalBlocks, totalNodes, totalLinks, totalHiers, totalEvents, 0, gridIsOn, fileIsOpen,
               menuFile, menuPath, filter, saveCancelled);
      SetView(cusZoomVal, xOrigin, yOrigin);
      nameOfFile := menuFile;
      IF recovering
         nameOfFile := "Unnamed RBD";
      END IF;
      CheckOpenFileStatus;
      ASK menubar TO SetChecks;
   ELSE
      IF diagnostics
         IF compileType = "demo"
            ASK window TO SetTitle("RAPTOR 7.0 DEMO Diagnostics - " + nameOfFile);  
         ELSIF compileType = "student"
            ASK window TO SetTitle("RAPTOR 7.0 STUDENT Diagnostics - " + nameOfFile);
         ELSIF compileType = "gmd"
            ASK window TO SetTitle("RAPTOR 6.5 Diagnostics - " + nameOfFile);
         ELSE
            ASK window TO SetTitle("RAPTOR 7.0 Diagnostics - " + nameOfFile);      
         END IF;
      ELSE
         IF compileType = "demo"
            ASK window TO SetTitle("RAPTOR 7.0 DEMO - " + nameOfFile);
         ELSIF compileType = "student"
            ASK window TO SetTitle("RAPTOR 7.0 STUDENT - " + nameOfFile);
         ELSIF compileType = "gmd"
            ASK window TO SetTitle("RAPTOR 6.5 - " + nameOfFile);
         ELSE
            ASK window TO SetTitle("RAPTOR 7.0 - " + nameOfFile);
         END IF;
      END IF;
   END IF;
   IF password
      ASK window TO ShowStatus(0,"Open");
   END IF;
   
   ASK window TO Update;
END PROCEDURE; {InitDisplay}

PROCEDURE GetPassword(IN today : STRING; OUT pword :STRING);      

VAR
    nextNumber,nextLetter, month :STRING;
BEGIN
    month := SUBSTR(5, 7, today);
    IF month="Jan"
        pword:="zt";
    ELSIF month="Feb"
        pword:="zn";
    ELSIF month="Mar"
        pword:="zm";
    ELSIF month="Apr"
        pword:="zr";
    ELSIF month="May"
        pword:="zl";
    ELSIF month="Jun"
        pword:="zs";
    ELSIF month="Jul"
        pword:="zk";
    ELSIF month="Aug"
        pword:="zv";
    ELSIF month="Sep"
        pword:="zb";
    ELSIF month="Oct"
        pword:="tz";
    ELSIF month="Nov"
        pword:="tt";
    ELSIF month="Dec"
        pword:="tn";
    END IF;         
    nextLetter := SUBSTR(9, 9, today);
    ConvertNumber(nextLetter);
    pword:=pword+nextLetter;
    nextLetter := SUBSTR(10, 10, today);
    ConvertNumber(nextLetter);
    pword:=pword+nextLetter;
    nextLetter := SUBSTR(23, 23, today);
    ConvertNumber(nextLetter);
    pword:=pword+nextLetter;
    nextLetter := SUBSTR(24, 24, today);
    ConvertNumber(nextLetter);
    pword:=pword+nextLetter;    
END PROCEDURE;

PROCEDURE ConvertNumber(INOUT a : STRING);   
BEGIN
    IF a="1"
        a:="t";
    ELSIF a="2"
        a:="n";
    ELSIF a="3"
        a:="m";
    ELSIF a="4"
        a:="r";
    ELSIF a="5"
        a:="l";
    ELSIF a="6"
        a:="s";
    ELSIF a="7"
        a:="k";
    ELSIF a="8"
        a:="v";
    ELSIF a="9"
        a:="b";
    ELSIF a="0"
        a:="z";
    END IF;         
END PROCEDURE;

PROCEDURE AddStartEndNodes;
VAR
   node               : RBDNodeObj;
BEGIN
   somethingChanged :=TRUE;
   NEW(node);   {Start Node}
   ASK nodeGroup TO Add(node);
   INC(totalNodes);
   INC(totalObjects);
   ASK node TO LoadFromLibrary (images, "RBDStartNode");
   ASK node TO SetID("RBDNode", nextId);
   startId := node.Id;
   ASK root TO AddAfterGraphic(defaultBlock,node);
   ASK node TO SetScaling(1.,1.);
   {ASK node TO SetScaling(scale,scale);
   ASK window TO AddGraphic(node); }
   ASK node TO DisplayAt(1., 73.);
   {ASK node TO SetSnapShot(FALSE);}
   ASK node TO SetName("start");
   {ASK node TO SetTranslation(node.Translation.x, node.Translation.y);}
   ASK node TO SetType(1);
   ASK node TO SetNum(node.Id);
   ASK node TO SetParentID(0);
   ASK node TO SetGoodPaths(1);
   ASK node TO SetFullFlow(TRUE);
   ASK node TO SetAnyPath(TRUE);
   ASK node TO Draw;
   
   NEW(node);   {End Node}
   ASK nodeGroup TO Add(node);
   INC(totalNodes);
   INC(totalObjects);
   ASK node TO LoadFromLibrary (images, "RBDEndNode");
   ASK node TO SetID("RBDNode", nextId);
   endId := node.Id;
   ASK root TO AddAfterGraphic(defaultBlock,node);
   ASK node TO SetScaling(1.,1.);
   {ASK node TO SetScaling(scale,scale);
   ASK window TO AddGraphic(node); }
   ASK node TO DisplayAt(22., 73.);
   {ASK node TO SetSnapShot(FALSE);}
   ASK node TO SetName("end");
   {ASK node TO SetTranslation(node.Translation.x, node.Translation.y);}
   ASK node TO SetType(3);
   ASK node TO SetNum(node.Id);
   ASK node TO SetParentID(0);
   ASK node TO SetGoodPaths(1);
   ASK node TO SetFullFlow(TRUE);
   ASK node TO SetAnyPath(TRUE);
   ASK node TO Draw;
   
END PROCEDURE;

PROCEDURE CheckTimeBomb(OUT exploded : BOOLEAN);

VAR
  Today                               : STRING;
  JDay,LastUsed,ElimiDate             : INTEGER;
  expired, diddledClock, diddledFile  : BOOLEAN;
  securityFileStream                  : StreamObj;
BEGIN;
  DateTime(Today);
  JDay :=0;
  CASE SUBSTR(5,7,Today)
     WHEN "Jan" : JDay := 0;
     WHEN "Feb" : JDay := 31;
     WHEN "Mar" : JDay := 31+28;
     WHEN "Apr" : JDay := 31+28+31;
     WHEN "May" : JDay := 31+28+31+30;
     WHEN "Jun" : JDay := 31+28+31+30+31;
     WHEN "Jul" : JDay := 31+28+31+30+31+30;
     WHEN "Aug" : JDay := 31+28+31+30+31+30+31;
     WHEN "Sep" : JDay := 31+28+31+30+31+30+31+31;
     WHEN "Oct" : JDay := 31+28+31+30+31+30+31+31+30;
     WHEN "Nov" : JDay := 31+28+31+30+31+30+31+31+30+31;
     WHEN "Dec" : JDay := 31+28+31+30+31+30+31+31+30+31+30;
  END CASE;
  JDay := JDay + STRTOINT(SUBSTR(9,10,Today));
  JDay := JDay + (STRTOINT(SUBSTR(21,24,Today))-2001)*365;
  NEW(securityFileStream);
  { Modifications by SJP 12/4/08 - Change check to look in userPath and create .stf file if it does not exist }
  ASK securityFileStream TO Open(userPath+"raptor.stf", BinaryInput);
  IF securityFileStream.ioResult <> 0
      ElimiDate:=JDay+elimiDays;
      ASK securityFileStream TO Open(userPath+"raptor.stf", BinaryOutput);
      ASK securityFileStream TO WriteInt(JDay,3);
      ASK securityFileStream TO WriteInt(ElimiDate,3);
  ELSE   
     ASK securityFileStream TO ReadInt(LastUsed);
     ASK securityFileStream TO ReadInt(ElimiDate);
     IF (LastUsed > JDay)  OR  (JDay < 0)
        diddledClock := TRUE;
        NEW(message,1..3);
        message[1] := "The system clock has been reset to a date     ";
        message[2] := "prior to the last date Raptor was used.";
        message[3] := "Raptor will not work.";
        result := SendAlert(message,FALSE,FALSE,TRUE);
        DISPOSE(message);
     END IF;   
     IF NOT(diddledClock)   
        ASK securityFileStream TO Open(userPath+"raptor.stf", BinaryOutput);
        ASK securityFileStream TO WriteInt(JDay,3);
        ASK securityFileStream TO WriteInt(ElimiDate,3);
     END IF;   
  END IF;  
  IF ((compileType="demo") OR (compileType="student")) 
     IF JDay > ElimiDate 
       expired := TRUE;
       NEW(message, 1..2);
       message[1] := "This version of Raptor has expired.  Please contact     ";
       message[2] := "raptortech@arinc.com for extensions or more information.";
       result := SendAlert(message, FALSE, FALSE, FALSE);
       DISPOSE(message);      
     END IF;   
  ELSIF training  
     IF JDay > (ElimiDate - 60) {expireDate -- eag changed, expireDate isn't used anymore right (did thorough search
                          of all modules and found no other use of the "expireDate" variable except for initial
                          definition in DDisplay)?, used to never hit this code if "release"}
        expired := TRUE;
        NEW(message, 1..2);
        message[1] := "This Training version of Raptor has expired. Please contact";
        message[2] := "raptortech@arinc.com for more information.                 ";
        result := SendAlert(message, FALSE, FALSE, FALSE);
        DISPOSE(message);      
     END IF;
  END IF;
  ASK securityFileStream TO Close
  exploded := (diddledClock OR diddledFile OR expired);
END PROCEDURE;

PROCEDURE AddHier;
VAR
   tempHier     : RBDHierObj;
BEGIN
   ClearAllBlocks;
   ASK menubar TO Disable1Thru8;
   totalHiers := totalHiers + 1;
   totalObjects := totalObjects + 1;
   typeOfCursor := hierC;
   FOR i := 1 TO 6
      ASK window TO ShowStatus(i,"");
   END FOR;            
   NEW(hier);
   ASK hier TO LoadFromLibrary(images, "RBDHier");
   curOffX := -2.;
   curOffY := 2.;
   ASK hier TO SetScaling(scale,scale);
   ASK window TO AddGraphic(hier);
   scrollScreen := TRUE;
   ASK hier TO SetID("RBDHier", nextId);
   ASK hier TO SetParentID(activeWindow);
   ASK hier TO SetLevel(hierLevel+1);
   IF hier.level > deepestLevel
      deepestLevel := hier.level;
   END IF;
   ASK hier TO CreateChildGroup;
   ASK window TO SetCursorOffset(curOffX,curOffY);
   ASK window TO SetCursor(hier);
   ASK hier TO Draw;
   IF hier.parentID > 0
      tempHier := ASK root Child("RBDHier", hier.parentID);
      ASK tempHier.childGroup TO Add(hier);
   END IF;
   ASK hier TO SetZoom(24.);
   ASK hier TO SetOrigin(0.,80.);
   ASK hierGroup TO Add(hier);
END PROCEDURE; {AddHier}


PROCEDURE AddBlock;
VAR
   buttItem : PaletteButtonObj;
BEGIN
   IF (totalBlocks >= demoCrippleLimit) AND (compileType = "demo")
      NEW(message,1..2);
      message[1] := "This is a demo version which only allows "+INTTOSTR(demoCrippleLimit)+" blocks.  Please visit    ";
      message[2] := "the Raptor website at www.raptorplus.com to obtain a full version.     ";
      result := SendAlert(message,TRUE, FALSE, FALSE);
      DISPOSE(message);
      typeOfCursor := nilC;
      buttItem := ASK menuTool Descendant("BlockButton", 908);
      ASK buttItem TO SetSelected(FALSE);
      ASK buttItem TO Draw;
      buttItem := ASK menuTool Descendant("EventButton", 911);
      ASK buttItem TO SetSelected(FALSE);
      ASK buttItem TO Draw;
   ELSIF (totalBlocks >= studentCrippleLimit) AND (compileType = "student")
      NEW(message,1..2);
      message[1] := "This is a student version which only allows "+INTTOSTR(studentCrippleLimit)+" blocks.  Please visit    ";
      message[2] := "the Raptor website at www.raptorplus.com to obtain a full version.     ";
      result := SendAlert(message,TRUE, FALSE, FALSE);
      DISPOSE(message);
      typeOfCursor := nilC;
      buttItem := ASK menuTool Descendant("BlockButton", 908);
      ASK buttItem TO SetSelected(FALSE);
      ASK buttItem TO Draw;
      buttItem := ASK menuTool Descendant("EventButton", 911);
      ASK buttItem TO SetSelected(FALSE);
      ASK buttItem TO Draw;
   ELSE
      ClearAllBlocks;
      ASK menubar TO Disable1Thru8;
      totalBlocks := totalBlocks + 1;
      totalObjects := totalObjects + 1;
      typeOfCursor := blockC;
      FOR i := 1 TO 6
         ASK window TO ShowStatus(i,"");
      END FOR;
      NEW(block);
      ASK block TO LoadFromLibrary(images, "RBDBlock");
      ASK block TO SetID("RBDBlock", nextId);
      ASK block TO GetCloneOf(defaultBlock, FALSE);  
      curOffX := 0.;
      curOffY := 0.;
      ASK block TO SetScaling(scale,scale);
      ASK window TO AddGraphic(block);
      scrollScreen := TRUE; 
      ASK window TO SetCursorOffset(curOffX,curOffY);
      ASK window TO SetCursor(block);
      ASK block TO SetParentID(activeWindow);
      ASK block TO Draw;
      IF block.parentID > 0
         hier := ASK root Child("RBDHier", block.parentID);
         ASK hier.childGroup TO Add(block);
      END IF;
      ASK blockGroup TO Add(block);
   END IF;
END PROCEDURE; {AddBlock}

PROCEDURE AddEvent;
VAR
   buttItem : PaletteButtonObj;
BEGIN
   ClearAllBlocks;
   ASK menubar TO Disable1Thru8;
   INC(totalEvents);
   INC(totalObjects);
   typeOfCursor := eventC;
   FOR i := 1 TO 6
      ASK window TO ShowStatus(i,"");
   END FOR;
   NEW(event);
   ASK event TO LoadFromLibrary(images, "RBDEvent");
   ASK event TO SetID("RBDEvent", nextId);
   ASK event TO SetEventDefs();
   curOffX := -2.;
   curOffY := 2.;
   ASK event TO SetScaling(scale,scale);
   ASK window TO AddGraphic(event);
   scrollScreen := TRUE; 
   ASK window TO SetCursorOffset(curOffX,curOffY);
   ASK window TO SetCursor(event);
   ASK event TO SetParentID(activeWindow);
   ASK event TO Draw;
   IF event.parentID > 0
      hier := ASK root Child("RBDHier", event.parentID);
      ASK hier.childGroup TO Add(event);
   END IF;
   ASK eventGroup TO Add(event);
END PROCEDURE; {AddEvent}

PROCEDURE AddConnector;
BEGIN
   ClearAllBlocks;
   IF totalObjects > 1
      ASK menubar TO Disable1Thru8;
      IF NOT linkMsgExists
         NEW(linkMessage);
         ASK linkMessage TO LoadFromLibrary(images,"LinkText");
         ASK window TO AddGraphic(linkMessage);
         linkText := ASK linkMessage Child("LinkText", 851);
         ASK linkText TO SetText("Click right mouse button to stop adding links");
         ASK linkText TO SetFont(SystemText);
         ASK linkText TO SetColor(Black);
         ASK linkMessage TO DisplayAt(300.,619.); 
         linkMsgExists := TRUE;
      END IF;
      totalLinks := totalLinks + 1;
      typeOfCursor := connectC;
      FOR i := 1 TO 6
         ASK window TO ShowStatus(i,"");
      END FOR;
      NEW(link);
      ASK link TO SetID("RBDLink", nextLinkId);
      ASK link TO SetParentID(activeWindow);
      ASK root TO AddBeforeGraphic(defaultBlock, link);
   ELSE
      NEW(message, 1..1);
      message[1] := "Must have at least 2 objects to link!";
      result := SendAlert(message, FALSE, FALSE, TRUE);
      DISPOSE(message);
      linkCancelled := TRUE;
   END IF;
   IF link.parentID > 0
      hier := ASK root Child("RBDHier", link.parentID);
      ASK hier.childGroup TO Add(link);
   END IF;
   ASK linkGroup TO Add(link);
END PROCEDURE; {AddConnector}

PROCEDURE AddNode;
BEGIN
   ClearAllBlocks;
   ASK menubar TO Disable1Thru8;
   INC(totalNodes);
   INC(totalObjects);
   typeOfCursor := nodeC;
   FOR i := 1 TO 6
      ASK window TO ShowStatus(i,"");
   END FOR;
   NEW(node);
   ASK node TO LoadFromLibrary (images, "RBDNode");
   ASK node TO SetID("RBDNode", nextId);
   ASK node TO SetScaling(scale,scale);
   ASK window TO AddGraphic(node);
   scrollScreen := TRUE; 
   ASK window TO SetCursorOffset(-3.2,2.4);
   ASK window TO SetCursor(node);
   ASK node TO SetParentID(activeWindow);
   ASK node TO SetFullFlow(TRUE);
   ASK node TO SetAnyPath(TRUE);
   ASK node TO Draw;
   IF node.parentID > 0
      hier := ASK root Child("RBDHier", node.parentID);
      ASK hier.childGroup TO Add(node);
   END IF;
   ASK nodeGroup TO Add(node);
END PROCEDURE; {AddNode}

PROCEDURE ClearAllBlocks;
VAR
   tempNode           : RBDNodeObj;
   tempBlock          : RBDBlockObj;
   tempEvent          : RBDEventObj;
   tempHier           : RBDHierObj;
   tempLink           : LinkObj;
BEGIN
   IF (blueObjRef <> "") OR (blueObjId <> -1)
      WHILE selectGroup.numberIn > 0
         IF OBJTYPENAME(ASK selectGroup First()) = "RBDNodeObj"   
            tempNode := ASK selectGroup First();
            blueObjRef := "RBDNode";
            blueObjId := tempNode.Id;
         ELSIF OBJTYPENAME(ASK selectGroup First()) = "RBDBlockObj"
            tempBlock := ASK selectGroup First();
            blueObjRef := "RBDBlock";
            blueObjId := tempBlock.Id;
         ELSIF OBJTYPENAME(ASK selectGroup First()) = "RBDEventObj"
            tempEvent := ASK selectGroup First();
            blueObjRef := "RBDEvent";
            blueObjId := tempEvent.Id;
         ELSIF OBJTYPENAME(ASK selectGroup First()) = "RBDHierObj"
            tempHier := ASK selectGroup First();
            blueObjRef := "RBDHier";
            blueObjId := tempHier.Id;  
         ELSIF OBJTYPENAME(ASK selectGroup First()) = "LinkObj"
            tempLink := ASK selectGroup First();
            blueObjRef := "RBDLink";
            blueObjId := tempLink.Id;
         END IF;
         IF blueObjRef = "RBDBlock"
            tempBlock := ASK window Descendant("RBDBlock", blueObjId);
            ASK tempBlock TO SetSelected(FALSE);
            ASK tempBlock TO SetHighlighted(FALSE);
            ASK tempBlock TO Draw;
            ASK selectGroup TO RemoveThis(tempBlock);               
         ELSIF blueObjRef = "RBDEvent"
            tempEvent := ASK window Descendant("RBDEvent", blueObjId);
            ASK tempEvent TO SetSelected(FALSE);
            ASK tempEvent TO SetHighlighted(FALSE);
            ASK tempEvent TO Draw;
            ASK selectGroup TO RemoveThis(tempEvent);               
         ELSIF blueObjRef = "RBDNode"
            tempNode := ASK window Descendant("RBDNode", blueObjId);
            ASK tempNode TO SetSelected(FALSE);
            ASK tempNode TO SetHighlighted(FALSE);
            ASK tempNode TO Draw;
            ASK selectGroup TO RemoveThis(tempNode);    
         ELSIF blueObjRef = "RBDHier"
            tempHier := ASK window Descendant("RBDHier", blueObjId);
            ASK tempHier TO SetSelected(FALSE);
            ASK tempHier TO SetHighlighted(FALSE);
            ASK tempHier TO Draw;
            ASK selectGroup TO RemoveThis(tempHier);    
         ELSIF blueObjRef = "RBDLink"
            tempLink := ASK window Descendant("RBDLink", blueObjId);
            ASK tempLink TO SetSelected(FALSE);
            ASK tempLink TO SetHighlighted(FALSE);
            ASK tempLink TO Draw;
            ASK selectGroup TO RemoveThis(tempLink);    
            linkSelected := FALSE;
         END IF;
      END WHILE;
      blueObjRef := "";
      blueObjId := -1;
   END IF;
   IF partialFromGroup.numberIn>0
      FOREACH tempLink IN partialFromGroup
         ASK partialFromGroup TO RemoveThis(tempLink);
      END FOREACH;
   END IF;
   IF partialToGroup.numberIn>0
      FOREACH tempLink IN partialToGroup
         ASK partialToGroup TO RemoveThis(tempLink);
      END FOREACH;
   END IF;
   IF selectedLinksGroup.numberIn>0
      FOREACH tempLink IN selectedLinksGroup
         ASK selectedLinksGroup TO RemoveThis(tempLink);
      END FOREACH;
   END IF;
   ASK menubar TO Disable(1);
   ASK menubar TO Disable(7);
   ASK menubar TO Disable(8);
   ASK menubar TO Disable(10);
   ASK menubar TO Disable(14);
   nodesIn := 0;
   blocksIn := 0;
   eventsIn := 0;
   linksIn := 0;
   hiersIn := 0;
END PROCEDURE; {ClearAllBlocks}

PROCEDURE HandleLinks (IN addingItem : BOOLEAN);
VAR
   tempLink  : LinkObj;
BEGIN
   {If link connects from/to this obj and also connects to/from another obj also in select group, 
    get entire link.  Otherwise, put in partialFrom/ToGroup -- Adding links to select group}
   IF addingItem
      IF blueObjRef = "RBDBlock"
         FOREACH link IN linkGroup
            IF (link.connectFromId=block.Id) AND (link.connectFRef = "RBDBlock")
               IF partialToGroup.Includes(link)
                  ASK partialToGroup TO RemoveThis(link);
                  ASK selectedLinksGroup TO Add(link);
                  INC(linksIn);
               ELSE
                  ASK partialFromGroup TO Add(link);
               END IF;
            ELSIF (link.connectToId=block.Id) AND (link.connectTRef = "RBDBlock")
               IF partialFromGroup.Includes(link)
                  ASK partialFromGroup TO RemoveThis(link);
                  ASK selectedLinksGroup TO Add(link);
                  INC(linksIn);
               ELSE
                  ASK partialToGroup TO Add(link);
               END IF;
            END IF;
         END FOREACH;
      ELSIF blueObjRef = "RBDEvent"
         FOREACH link IN linkGroup
            IF (link.connectFromId=event.Id) AND (link.connectFRef = "RBDEvent")
               IF partialToGroup.Includes(link)
                  ASK partialToGroup TO RemoveThis(link);
                  ASK selectedLinksGroup TO Add(link);
                  INC(linksIn);
               ELSE
                  ASK partialFromGroup TO Add(link);
               END IF;
            ELSIF (link.connectToId=event.Id) AND (link.connectTRef = "RBDEvent")
               IF partialFromGroup.Includes(link)
                  ASK partialFromGroup TO RemoveThis(link);
                  ASK selectedLinksGroup TO Add(link);
                  INC(linksIn);
               ELSE
                  ASK partialToGroup TO Add(link);
               END IF;
            END IF;
         END FOREACH;
      ELSIF blueObjRef = "RBDHier"
         FOREACH link IN linkGroup
            IF (link.connectFromId=hier.Id) AND (link.connectFRef = "RBDHier")
               IF partialToGroup.Includes(link)
                  ASK partialToGroup TO RemoveThis(link);
                  ASK selectedLinksGroup TO Add(link);
                  INC(linksIn);
               ELSE
                  ASK partialFromGroup TO Add(link);
               END IF;
            ELSIF (link.connectToId=hier.Id) AND (link.connectTRef = "RBDHier")
               IF partialFromGroup.Includes(link)
                  ASK partialFromGroup TO RemoveThis(link);
                  ASK selectedLinksGroup TO Add(link);
                  INC(linksIn);
               ELSE
                  ASK partialToGroup TO Add(link);
               END IF;
            END IF;
         END FOREACH;
      ELSE
         FOREACH link IN linkGroup
            IF (link.connectFromId=node.Id) AND (link.connectFRef = "RBDNode")
               IF partialToGroup.Includes(link)
                  ASK partialToGroup TO RemoveThis(link);
                  ASK selectedLinksGroup TO Add(link);
                  INC(linksIn);
               ELSE
                  ASK partialFromGroup TO Add(link);
               END IF;
            ELSIF (link.connectToId=node.Id) AND (link.connectTRef = "RBDNode")
               IF partialFromGroup.Includes(link)
                  ASK partialFromGroup TO RemoveThis(link);
                  ASK selectedLinksGroup TO Add(link);
                  INC(linksIn);
               ELSE
                  ASK partialToGroup TO Add(link);
               END IF;
            END IF;
         END FOREACH;
      END IF;
   ELSE
   {If link connects from/to this obj and is also in select group, remove from select group 
    and put in partialTo/FromGroup.  Otherwise, remove from partialFrom/ToGroup -- Removing links from select group}
      IF blueObjRef = "RBDBlock"
         DEC(blocksIn);
         FOREACH link IN linkGroup
            IF (link.connectFromId=block.Id) AND (link.connectFRef = "RBDBlock")
               IF selectedLinksGroup.Includes(link)
                  ASK selectedLinksGroup TO RemoveThis(link);
                  ASK partialToGroup TO Add(link);
                  DEC(linksIn);
               ELSE
                  ASK partialFromGroup TO RemoveThis(link);
               END IF;
            ELSIF (link.connectToId=block.Id) AND (link.connectTRef = "RBDBlock")
               IF selectedLinksGroup.Includes(link)
                  ASK selectedLinksGroup TO RemoveThis(link);
                  ASK partialFromGroup TO Add(link);
                  DEC(linksIn);
               ELSE
                  ASK partialToGroup TO RemoveThis(link);
               END IF;
            END IF;
         END FOREACH;
      ELSIF blueObjRef = "RBDEvent"
         DEC(eventsIn);
         FOREACH link IN linkGroup
            IF (link.connectFromId=event.Id) AND (link.connectFRef = "RBDEvent")
               IF selectedLinksGroup.Includes(link)
                  ASK selectedLinksGroup TO RemoveThis(link);
                  ASK partialToGroup TO Add(link);
                  DEC(linksIn);
               ELSE
                  ASK partialFromGroup TO RemoveThis(link);
               END IF;
            ELSIF (link.connectToId=event.Id) AND (link.connectTRef = "RBDEvent")
               IF selectedLinksGroup.Includes(link)
                  ASK selectedLinksGroup TO RemoveThis(link);
                  ASK partialFromGroup TO Add(link);
                  DEC(linksIn);
               ELSE
                  ASK partialToGroup TO RemoveThis(link);
               END IF;
            END IF;
         END FOREACH;
      ELSIF blueObjRef = "RBDHier"
         DEC(hiersIn);
         FOREACH link IN linkGroup
            IF (link.connectFromId=hier.Id) AND (link.connectFRef = "RBDHier")
               IF selectedLinksGroup.Includes(link)
                  ASK selectedLinksGroup TO RemoveThis(link);
                  ASK partialToGroup TO Add(link);
                  DEC(linksIn);
               ELSE
                  ASK partialFromGroup TO RemoveThis(link);
               END IF;
            ELSIF (link.connectToId=hier.Id) AND (link.connectTRef = "RBDHier")
               IF selectedLinksGroup.Includes(link)
                  ASK selectedLinksGroup TO RemoveThis(link);
                  ASK partialFromGroup TO Add(link);
                  DEC(linksIn);
               ELSE
                  ASK partialToGroup TO RemoveThis(link);
               END IF;
            END IF;
         END FOREACH; 
      ELSE
         DEC(nodesIn);
         FOREACH link IN linkGroup
            IF (link.connectFromId=node.Id) AND (link.connectFRef = "RBDNode")
               IF selectedLinksGroup.Includes(link)
                  ASK selectedLinksGroup TO RemoveThis(link);
                  ASK partialToGroup TO Add(link);
                  DEC(linksIn);
               ELSE
                  ASK partialFromGroup TO RemoveThis(link);
               END IF;
            ELSIF (link.connectToId=node.Id) AND (link.connectTRef = "RBDNode")
               IF selectedLinksGroup.Includes(link)
                  ASK selectedLinksGroup TO RemoveThis(link);
                  ASK partialFromGroup TO Add(link);
                  DEC(linksIn);
               ELSE
                  ASK partialToGroup TO RemoveThis(link);
               END IF;
            END IF;
         END FOREACH;
      END IF;
   END IF;
   ASK window TO SetCursor(NILOBJ);
END PROCEDURE {HandleLinks};

PROCEDURE ResetDepNums(INOUT item : RBDBasicObj);
VAR
   tempBlock : RBDBlockObj;
   tempEvent : RBDEventObj;
   tempNode, outNode : RBDNodeObj;
   tempHier : RBDHierObj;
BEGIN
   FOREACH tempBlock IN blockGroup
      IF tempBlock.DependencyNum = item.Id
         ASK tempBlock TO SetDep(0, "");
      END IF;
   END FOREACH;
   FOREACH tempEvent IN eventGroup
      IF tempEvent.DependencyNum = item.Id
         ASK tempEvent TO SetDep(0, "");
      END IF;
   END FOREACH;
   FOREACH tempNode IN nodeGroup 
      IF tempNode.DependencyNum = item.Id
         ASK tempNode TO SetDep(0, "");
      END IF;
   END FOREACH;
   FOREACH tempHier IN hierGroup
      outNode := ASK root Child("RBDNode", tempHier.outID);
      IF outNode.DependencyNum = item.Id
         ASK outNode TO SetDep(0, "");
      END IF;
   END FOREACH;
END PROCEDURE;

PROCEDURE ClearObject; 
VAR
   nodeRedux, linkRedux, oldId, 
   hierRedux, blockRedux, eventRedux, tempId, test   : INTEGER;
   label                                       : TextObj;
   tempBlock                                   : RBDBlockObj;
   tempNode                                    : RBDNodeObj;
   tempHier                      : RBDHierObj;
   hier2Group                     : QueueObj;
   basic : RBDBasicObj;
   child, obj : ANYOBJ;
   linkChild : LinkObj;
   basicChild : RBDBasicObj;
   menuItem : MenuItemObj;
   buttItem : PaletteButtonObj; 
BEGIN
   FOREACH obj IN selectGroup
      IF OBJTYPENAME(obj) = "RBDNodeObj"
         tempNode := RBDNodeObj(obj);
         IF tempNode.typeNode <> 2
            NEW(message, 1..1);
            IF tempNode.typeNode = 1
               message[1] := "Cannot delete start marker     ";
            ELSIF tempNode.typeNode = 3
               message[1] := "Cannot delete end marker     ";
            ELSIF tempNode.typeNode = 4
               message[1] := "Cannot delete in marker     ";
            ELSIF tempNode.typeNode = 5
               message[1] := "Cannot delete out marker     ";
            END IF;
            result := SendAlert(message, FALSE, FALSE, TRUE);
            RETURN;
         END IF;
      END IF;
   END FOREACH;

   ASK window TO SetSysCursor(BusyCursor);
   NEW(hier2Group);
   FOREACH hier IN hierGroup
      IF selectGroup.Includes(hier)
         ASK hier2Group TO Add(hier);
      END IF;
   END FOREACH;
   FOREACH tempHier IN hier2Group
      ASK tempHier TO AugmentSelectGroup;
      ASK hier2Group TO RemoveThis(tempHier);
   END FOREACH;
   DISPOSE(hier2Group);
   
   {Single selected link has been deleted}
   IF blueObjRef = "RBDLink"
      oldId := 99999;
      link := ASK root Child("RBDLink", blueObjId);
      IF link.connectFRef = "RBDBlock"
         block := ASK root Child("RBDBlock", link.connectFromId);
         IF link.connectTRef = "RBDNode"
            IF block.isConnectedNode
               ASK block TO SetConnectToNode(FALSE);
            END IF;
         END IF;
         ASK block TO IncLink(MINUSOUTOF);
      ELSIF link.connectFRef = "RBDEvent"
         event := ASK root Child("RBDEvent", link.connectFromId);
         IF link.connectTRef = "RBDNode"
            IF event.isConnectedNode
               ASK event TO SetConnectToNode(FALSE);
            END IF;
         END IF;
         ASK event TO IncLink(MINUSOUTOF);
      ELSIF link.connectFRef = "RBDHier"
         hier := ASK root Child("RBDHier", link.connectFromId);
         IF link.connectTRef = "RBDNode"
            IF hier.isConnectedNode
               ASK hier TO SetConnectToNode(FALSE);
            END IF;
         END IF;
         ASK hier TO IncLink(MINUSOUTOF);
      ELSIF link.connectFRef = "RBDNode"
         node := ASK root Child("RBDNode", link.connectFromId);
         ASK node TO IncLink(MINUSOUTOF);
      END IF;
      IF link.connectTRef = "RBDBlock"
         block := ASK root Child("RBDBlock", link.connectToId);
         ASK block TO IncLink(MINUSINTO);
      ELSIF link.connectTRef = "RBDEvent"
         event := ASK root Child("RBDEvent", link.connectToId);
         ASK event TO IncLink(MINUSINTO);
      ELSIF link.connectTRef = "RBDHier"
         hier := ASK root Child("RBDHier", link.connectToId);
         ASK hier TO IncLink(MINUSINTO);
      ELSIF link.connectTRef = "RBDNode"
         node := ASK root Child("RBDNode", link.connectToId);
         ASK node TO IncLink(MINUSINTO);
         IF node.connectIntoNum = 0
            ASK node TO SetGoodPaths(0);
         ELSE
            ASK node TO SetGoodPaths(1);
         END IF;
         label := ASK node Child("RBDNodeKofN", 0);
         ASK label TO SetText("");
         ASK node TO SetKStar(node.goodPaths);
         ASK node TO SetAoDoR("0", "0", "0");
         ASK node TO Draw;
      END IF;
      oldId := link.Id;
      IF link.parentID > 0
         hier := ASK root Child("RBDHier", link.parentID);
         IF hier <> NILOBJ
            IF hier.childGroup.Includes(link);
               ASK hier.childGroup TO RemoveThis(link);
            END IF;
         END IF;
      END IF;
      ASK selectGroup TO RemoveThis(link); 
      INC(linkRedux);
      ASK linkGroup TO RemoveThis(link);
      DISPOSE(link);
      totalLinks := totalLinks - 1;
      linkSelected := FALSE;
   ELSE
      FOREACH basic IN selectGroup
         {If current (being deleted) had stuff dependent on it, change
          objs dependent on current to have dependency num of 0 (independent)}
         ResetDepNums(basic);
      END FOREACH;
      FOREACH current IN selectGroup
         ASK selectGroup TO RemoveThis(current);
         IF OBJTYPENAME(current) = "RBDNodeObj"
            node := ASK root Child("RBDNode", current.Id);
            IF ((node.typeNode = 2) OR (((node.typeNode = 4) OR (node.typeNode = 5)) AND (activeWindow <> node.parentID)))
               IF node.parentID > 0
                  tempHier := ASK root Child("RBDHier", node.parentID);
                  IF tempHier <> NILOBJ
                     IF tempHier.childGroup.Includes(node)
                        ASK tempHier.childGroup TO RemoveThis(node);
                     END IF;                                
                  END IF;
               END IF;
               ASK nodeGroup TO RemoveThis(node);
               INC(nodeRedux);
               DISPOSE(node);
            END IF;
         ELSIF OBJTYPENAME(current) = "RBDBlockObj"
            block := ASK root Child("RBDBlock", current.Id);
            IF block.parentID > 0
               tempHier := ASK root Child("RBDHier", block.parentID);
               IF tempHier <> NILOBJ
                  IF tempHier.childGroup.Includes(block);
                     ASK tempHier.childGroup TO RemoveThis(block);
                  END IF;
               END IF;
            END IF;
            ASK blockGroup TO RemoveThis(block);
            INC(blockRedux);
            DISPOSE(block);
         ELSIF OBJTYPENAME(current) = "RBDEventObj"
            event := ASK root Child("RBDEvent", current.Id);
            IF event.parentID > 0
               tempHier := ASK root Child("RBDHier", event.parentID);
               IF tempHier <> NILOBJ
                  IF tempHier.childGroup.Includes(event);
                     ASK tempHier.childGroup TO RemoveThis(event);
                  END IF;
               END IF;
            END IF;
            ASK eventGroup TO RemoveThis(event);
            INC(eventRedux);
            DISPOSE(event);
         ELSE
            hier := ASK root Child("RBDHier", current.Id);
            IF hier.parentID > 0
               tempHier := ASK root Child("RBDHier", hier.parentID);
               IF tempHier <> NILOBJ
                  IF tempHier.childGroup.Includes(hier);
                     ASK tempHier.childGroup TO RemoveThis(hier);
                  END IF;
               END IF;
            END IF;
            FOREACH child IN hier.childGroup
               ASK hier.childGroup TO RemoveThis(child);
            END FOREACH;
            ASK hierGroup TO RemoveThis(hier);
            {fix deepest level}
            IF hier.level = deepestLevel
               deepestLevel := 0;
               FOREACH tempHier IN hierGroup
                  IF tempHier.level > deepestLevel
                     deepestLevel := tempHier.level
                  END IF;
               END FOREACH;
            END IF;
            INC(hierRedux);
            DISPOSE(hier);
            IF hierGroup.numberIn = 0
               menuItem := ASK menubar Descendant("GotoItem", 403);
               buttItem := ASK menuTool Descendant("GotoButton", 924);
               ASK menuItem TO Deactivate;
               ASK buttItem TO Deactivate;
            END IF;
         END IF;
      END FOREACH;
     
      {Element connected to link either coming in or out has been deleted; 
       partial links need to be deleted as well}
      FOREACH link IN selectedLinksGroup 
            ASK selectedLinksGroup TO RemoveThis(link);
            IF link.parentID > 0
               tempHier := ASK root Child("RBDHier", link.parentID);
               IF tempHier <> NILOBJ
                  IF tempHier.childGroup.Includes(link);
                     ASK tempHier.childGroup TO RemoveThis(link);
                  END IF;
               END IF;
            END IF;
            ASK linkGroup TO RemoveThis(link);
            INC(linkRedux);
            DISPOSE(link);
         END FOREACH;
         FOREACH link IN partialFromGroup
            ASK partialFromGroup TO RemoveThis(link);
            IF link.connectTRef = "RBDBlock"
               block := ASK root Child("RBDBlock", link.connectToId);
               ASK block TO IncLink(MINUSINTO);
            ELSIF link.connectTRef = "RBDEvent"
               event := ASK root Child("RBDEvent", link.connectToId);
               ASK event TO IncLink(MINUSINTO);
            ELSIF link.connectTRef = "RBDHier"
               hier := ASK root Child("RBDHier", link.connectToId);
               ASK hier TO IncLink(MINUSINTO); 
            ELSE
               node := ASK root Child("RBDNode", link.connectToId);
               ASK node TO SetGoodPaths(1);
               label := ASK node Child("RBDNodeKofN", 0);
               ASK label TO SetText("");
               ASK node TO SetKStar(node.goodPaths);
               ASK node TO SetAoDoR("0", "0", "0");
               ASK node TO Draw;
               ASK node TO IncLink(MINUSINTO);
            END IF;
            IF link.parentID > 0
               tempHier := ASK root Child("RBDHier", link.parentID);
               IF tempHier <> NILOBJ
                  IF tempHier.childGroup.Includes(link)
                     ASK tempHier.childGroup TO RemoveThis(link);
                  END IF;
               END IF;
            END IF;
            ASK linkGroup TO RemoveThis(link);
            INC(linkRedux);
            DISPOSE(link);
         END FOREACH;
         FOREACH link IN partialToGroup
            ASK partialToGroup TO RemoveThis(link);
            IF link.connectFRef = "RBDBlock"
               block := ASK root Child("RBDBlock", link.connectFromId);
               IF link.connectTRef = "RBDNode"
                  ASK block TO SetConnectToNode(FALSE);
               END IF;
               ASK block TO IncLink(MINUSOUTOF);
            ELSIF link.connectFRef = "RBDEvent"
               event := ASK root Child("RBDEvent", link.connectFromId);
               IF link.connectTRef = "RBDNode"
                  ASK event TO SetConnectToNode(FALSE);
               END IF;
               ASK event TO IncLink(MINUSOUTOF);
            ELSIF link.connectFRef = "RBDHier"
               hier := ASK root Child("RBDHier", link.connectFromId);
               IF link.connectTRef = "RBDNode"
                  ASK hier TO SetConnectToNode(FALSE);
               END IF;
               ASK hier TO IncLink(MINUSOUTOF);
            ELSE
               node := ASK root Child("RBDNode", link.connectFromId);
               ASK node TO IncLink(MINUSOUTOF);
            END IF;
            IF link.parentID > 0
               tempHier := ASK root Child("RBDHier", link.parentID);
               IF tempHier <> NILOBJ
                  IF tempHier.childGroup.Includes(link)
                     ASK tempHier.childGroup TO RemoveThis(link);
                  END IF;
               END IF;
            END IF;
            ASK linkGroup TO RemoveThis(link);
            INC(linkRedux);
            DISPOSE(link);
      END FOREACH; 
      
      totalLinks:=totalLinks-linkRedux;
      totalNodes := totalNodes - nodeRedux;
      totalHiers := totalHiers - hierRedux;
      totalBlocks := totalBlocks - blockRedux;
      totalEvents := totalEvents - eventRedux;
      totalObjects := totalObjects - (nodeRedux + hierRedux + blockRedux + eventRedux);
   END IF;
   block := NILOBJ;
   event := NILOBJ;
   node := NILOBJ;                                                                      
   hier := NILOBJ;
   ASK menubar TO Disable(1);
   ASK menubar TO Disable(7);
   ASK menubar TO Disable(8);
   ASK menubar TO Disable(10);
   ASK menubar TO Disable(14);
   ASK menubar TO Disable(17);
   analViewAvail := FALSE;
   somethingChanged := TRUE;
   configFrozen := FALSE;
   blueObjRef := "";
   blueObjId := -1;
   IF totalObjects = 0
      ASK menubar TO Disable(2);
      ASK menubar TO Disable(3);
      ASK menubar TO Disable(4);
      ASK menubar TO Disable(12);
   ELSIF totalObjects = 1
      ASK menubar TO Disable(16);
      ASK menubar TO Enable(2);
   ELSE
      ASK menubar TO Enable(2);
   END IF;
   IF copied
      ASK menubar TO Enable(3);
   END IF;
   ASK window TO SetSysCursor(NormalCursor);
  
END PROCEDURE; {ClearObject}

PROCEDURE SelectAll;
VAR
   tempLink : LinkObj;
BEGIN
   ClearAllBlocks;
   FOREACH block IN blockGroup
      IF block.parentID = activeWindow
         SelectBlock;
      END IF;
   END FOREACH;
   FOREACH event IN eventGroup
      IF event.parentID = activeWindow
         SelectEvent;
      END IF;
   END FOREACH;
   FOREACH node IN nodeGroup
      IF node.parentID = activeWindow
         SelectNode;
      END IF;
   END FOREACH;
   FOREACH hier IN hierGroup
      IF hier.parentID = activeWindow
         SelectHier;
      END IF;
   END FOREACH;
   FOREACH link IN linkGroup
      IF link.parentID = activeWindow
         INC(linksIn);
         ASK selectedLinksGroup TO Add(link);
      END IF;
   END FOREACH; 
   ASK menubar TO Enable(1);
   ASK menubar TO Enable(7);
   IF (nodesIn + blocksIn + eventsIn + hiersIn) = 1
      ASK menubar TO Enable(8);
   ELSIF (nodesIn + blocksIn + eventsIn + hiersIn) > 1
      ASK menubar TO Enable(10);
      ASK menubar TO Enable(14);
   END IF;
   IF (hiersIn = 1)
      ASK menubar TO Enable(10);
      ASK menubar TO Enable(14);
   END IF;
END PROCEDURE; {SelectAll}
   
PROCEDURE EditDetails;
VAR
   detailsDialog                        : BlockPropObj;
   eventsDialog                         : EventsBoxObj;
   nodeDetails                          : NodeBoxObj;
   hierDetails      : HierBoxObj;
   cancelled                            : BOOLEAN;
   buttItem                             : PaletteButtonObj;
BEGIN
   IF totalObjects > 0
      editFlag := TRUE;
      typeOfCursor := dialogC;
      IF blueObjRef = "RBDBlock"
         block := ASK window Descendant("RBDBlock", blueObjId); 
         ASK window TO SetCursor(NILOBJ);
         ASK block TO DisplayAt(block.xPosition, block.yPosition);
         ASK block TO Draw; 
         NEW(detailsDialog);
         ASK detailsDialog TO LoadFromLibrary(dialogs, "BlockPropBox");
         ASK window TO AddGraphic(detailsDialog);
         ASK detailsDialog TO Draw;
         ASK detailsDialog TO ReceiveData(block, "951", cancelled);
         DISPOSE(detailsDialog);
         ASK block TO SetSelected(FALSE);
         ASK block TO SetHighlighted(FALSE);
         ASK block TO Draw;
         ASK selectGroup TO RemoveThis(block);
      ELSIF blueObjRef = "RBDEvent"
         event := ASK window Descendant("RBDEvent", blueObjId); 
         ASK window TO SetCursor(NILOBJ);
         ASK event TO DisplayAt(event.xPosition, event.yPosition);
         ASK event TO Draw; 
         NEW(eventsDialog);
         ASK eventsDialog TO LoadFromLibrary(dialogs, "EventPropBox");
         ASK window TO AddGraphic(eventsDialog);
         ASK eventsDialog TO Draw;
         ASK eventsDialog TO ReceiveData(event, cancelled);
         DISPOSE(eventsDialog);
         ASK event TO SetSelected(FALSE);
         ASK event TO SetHighlighted(FALSE);
         ASK event TO Draw;
         ASK selectGroup TO RemoveThis(event);
      ELSIF blueObjRef = "RBDNode"
         node := ASK window Descendant("RBDNode", blueObjId);
         ASK window TO SetCursor(NILOBJ);
         ASK node TO DisplayAt(node.xPosition, node.yPosition);
         ASK node TO Draw;
         IF node.typeNode = 2
            NEW(nodeDetails);
            ASK nodeDetails TO LoadFromLibrary(dialogs,"NodePropBox");
            ASK window TO AddGraphic(nodeDetails);
            ASK nodeDetails TO Draw;
            ASK nodeDetails TO ReceiveData(cancelled,FALSE,node,node.typeNode);
            DISPOSE(nodeDetails);
         END IF;
         ASK node TO SetSelected(FALSE);
         ASK node TO SetHighlighted(FALSE);
         ASK node TO Draw;
         ASK selectGroup TO RemoveThis(node);
      ELSE
         hier := ASK window Descendant("RBDHier", blueObjId);
         ASK window TO SetCursor(NILOBJ);
         ASK hier TO DisplayAt(hier.xPosition, hier.yPosition);
         ASK hier TO Draw;
         NEW(hierDetails);
         ASK hierDetails TO LoadFromLibrary(dialogs,"HierBox");
         ASK window TO AddGraphic(hierDetails);
         ASK hierDetails TO Draw;
         ASK hierDetails TO ReceiveData(hier, cancelled);
         DISPOSE(hierDetails);
         ASK hier TO SetSelected(FALSE);
         ASK hier TO SetHighlighted(FALSE);
         ASK hier TO Draw;
         ASK selectGroup TO RemoveThis(hier); 
      END IF;
      ASK menubar TO Disable(1);
      ASK menubar TO Disable(7);
      ASK menubar TO Disable(8);
      blueObjRef := "";
      blueObjId := -1;
      typeOfCursor := nilC;
      editFlag := FALSE;
      {somethingChanged := TRUE; }
   END IF;
END PROCEDURE; {EditDetails}

PROCEDURE ClearPasteBuffer;
BEGIN
   IF copied
      IF copyBlocks <> NILARRAY
         DISPOSE(copyBlocks);
      END IF;
      IF copyEvents <> NILARRAY
         DISPOSE(copyEvents);
      END IF;
      IF copyNodes <> NILARRAY
         DISPOSE(copyNodes);
      END IF;
      IF copyLinks <> NILARRAY
         DISPOSE(copyLinks);
      END IF;
      IF copyHiers <> NILARRAY
         DISPOSE(copyHiers);
      END IF;
   END IF;
   copied := FALSE;
END PROCEDURE;

PROCEDURE CopyObject;
VAR
   hier2Group                     : QueueObj;
   tempHier : RBDHierObj;
   test1 : INTEGER;
   obj1 : RBDBasicObj;
   tempBlock : RBDBlockObj;
   tempEvent : RBDEventObj;
   tempNode : RBDNodeObj;
   tempLink : LinkObj;
   tempCopyNodes : ARRAY INTEGER OF RBDNodeObj;
   tempSelectGroup : QueueObj;
   obj : ANYOBJ;
BEGIN
   FOREACH obj IN selectGroup
      IF OBJTYPENAME(obj) = "RBDNodeObj"
         tempNode := RBDNodeObj(obj);
         IF tempNode.typeNode <> 2
            NEW(message, 1..1);
            IF tempNode.typeNode = 1
               message[1] := "Cannot copy start marker     ";
            ELSIF tempNode.typeNode = 3
               message[1] := "Cannot copy end marker     ";
            ELSIF tempNode.typeNode = 4
               message[1] := "Cannot copy in marker     ";
            ELSIF tempNode.typeNode = 5
               message[1] := "Cannot copy out marker     ";
            END IF;
            result := SendAlert(message, FALSE, FALSE, TRUE);
            RETURN;
         END IF;
      END IF;
   END FOREACH;
   copiedFromPrevFile := FALSE;
   NEW(tempSelectGroup);
   FOREACH obj1 IN selectGroup
      ASK tempSelectGroup TO Add(obj1);
   END FOREACH;
   copyWindow := activeWindow;
   workingPaste := TRUE;
   IF copied
      IF copyBlocks <> NILARRAY
         DISPOSE(copyBlocks);
      END IF;
      IF copyEvents <> NILARRAY
         DISPOSE(copyEvents);
      END IF;
      IF copyNodes <> NILARRAY
         DISPOSE(copyNodes);
      END IF;
      IF copyLinks <> NILARRAY
         DISPOSE(copyLinks);
      END IF;
      IF copyHiers <> NILARRAY
         DISPOSE(copyHiers);
      END IF;
   END IF;
   NEW(hier2Group);
   FOREACH hier IN hierGroup
      IF selectGroup.Includes(hier)
         ASK hier2Group TO Add(hier);
      END IF;
   END FOREACH;
   FOREACH tempHier IN hier2Group
      ASK tempHier TO AugmentSelectGroup;
   END FOREACH;
   FOREACH tempHier IN hier2Group
      ASK hier2Group TO RemoveThis(tempHier);
   END FOREACH;
   DISPOSE(hier2Group);
   IF hiersIn > 0
      i := 1;
      NEW(copyHiers, 1..hiersIn);      
      FOREACH current IN selectGroup
         IF OBJTYPENAME(current) = "RBDHierObj"   
            blueObjId := current.Id;
            hier := ASK window Descendant("RBDHier", blueObjId);
            copyHiers[i] := CLONE(hier);
            ASK copyHiers[i] TO SetCopiedFromId(hier.Id);
            INC(i);
         END IF;
      END FOREACH;
   END IF;     
   IF nodesIn > 0
      i := 1;
      NEW(copyNodes, 1..nodesIn);      
      FOREACH current IN selectGroup
         IF OBJTYPENAME(current) = "RBDNodeObj"   
            blueObjId := current.Id;
            node := ASK window Descendant("RBDNode", blueObjId);
            copyNodes[i] := CLONE(node);
            ASK copyNodes[i] TO SetCopiedFromId(node.Id);
            INC(i);
         END IF;
      END FOREACH;
   END IF;
   IF blocksIn > 0
      i := 1;
      NEW(copyBlocks, 1..blocksIn);      
      FOREACH current IN selectGroup
         IF OBJTYPENAME(current) = "RBDBlockObj"   
            blueObjId := current.Id;
            block := ASK window Descendant("RBDBlock", blueObjId);
            copyBlocks[i] := CLONE(block);
            ASK copyBlocks[i] TO SetCopiedFromId(block.Id);
            INC(i);
         END IF;
      END FOREACH;
   END IF;     
   IF eventsIn > 0
      i := 1;
      NEW(copyEvents, 1..eventsIn);      
      FOREACH current IN selectGroup
         IF OBJTYPENAME(current) = "RBDEventObj"   
            blueObjId := current.Id;
            event := ASK window Descendant("RBDEvent", blueObjId);
            copyEvents[i] := CLONE(event);
            ASK copyEvents[i] TO SetCopiedFromId(event.Id);
            INC(i);
         END IF;
      END FOREACH;
   END IF;     
   IF linksIn > 0
      i := 1;
      NEW(copyLinks, 1..linksIn);  
      FOREACH link IN selectedLinksGroup
         copyLinks[i] := CLONE(link);
         INC(i);
      END FOREACH;
   END IF;     
   ASK menubar TO Enable(3);
   copied := TRUE;
   workingPaste := FALSE;
   FOREACH obj1 IN selectGroup
      IF NOT(tempSelectGroup.Includes(obj1))
         ASK selectGroup TO RemoveThis(obj1);
         IF OBJTYPENAME(obj1) = "RBDNodeObj" 
            DEC(nodesIn);
         ELSIF OBJTYPENAME(obj1) = "RBDBlockObj"
            DEC(blocksIn);
         ELSIF OBJTYPENAME(obj1) = "RBDEventObj"
            DEC(eventsIn);
         ELSIF OBJTYPENAME(obj1) = "RBDHierObj"
            DEC(hiersIn);
         ELSIF OBJTYPENAME(obj1) = "LinkObj"
            DEC(linksIn);
         END IF;
      ELSE
         ASK tempSelectGroup TO RemoveThis(obj1);
      END IF;
   END FOREACH;
   ASK tempSelectGroup TO ObjTerminate; 
END PROCEDURE; {CopyObject}

PROCEDURE PasteObject;
VAR
   j, fromId, toId, startingTotalBlocks, startingTotalEvents, 
   startingTotalNodes, startingTotalHiers, test1, cBlocks, cEvents, cNodes, cHiers, currDepth, deepestDepth : INTEGER;
   fromType, toType                       : STRING;
   attachX, attachY, newx, newy, deltaX, 
   deltaY, tempX, tempY                   : REAL;
   linkPoints                             : PointArrayType;
   tempPhase, linkInts, intsArray, depChange         : intArray;
   partialCopyNode, test                        : BOOLEAN;
   linkReals                              : realArray;
   tempHier, tempHier1, depHier : RBDHierObj;
   tempBlock, depBlock, testblock : RBDBlockObj;
   tempEvent, depEvent : RBDEventObj;
   tempNode, depNode : RBDNodeObj;
   copiedGroup, copiedLinksGroup : QueueObj;
   obj1, obj2 : RBDBasicObj;
   tempLink, testlink : LinkObj;
   innerSquare : ImageObj;
   tempImage : FillVObj;
   nameText : TextObj;    testI:INTEGER;
BEGIN
   IF (compileType = "demo") AND (copyBlocks <> NILARRAY) 
      IF (totalBlocks + HIGH(copyBlocks)) > demoCrippleLimit
         NEW(message,1..2);
         message[1] := "This is a demo version which only allows "+INTTOSTR(demoCrippleLimit)+" blocks.  Please visit    ";
         message[2] := "the Raptor website at www.raptorplus.com to obtain a full version.     ";
         result := SendAlert(message,FALSE, FALSE, FALSE);
         DISPOSE(message);
         RETURN;
      END IF;
   ELSIF (compileType = "student") AND (copyBlocks <> NILARRAY) 
      IF (totalBlocks + HIGH(copyBlocks)) > studentCrippleLimit
         NEW(message,1..2);
         message[1] := "This is a student version which only allows "+INTTOSTR(studentCrippleLimit)+" blocks.  Please visit    ";
         message[2] := "the Raptor website at www.raptorplus.com to obtain a full version.     ";
         result := SendAlert(message,FALSE, FALSE, FALSE);
         DISPOSE(message);
         RETURN;
      END IF;
   END IF;
   IF (copyHiers <> NILARRAY)
      IF (hierLevel >= levelLimit) 
         NEW(message,1..1);
         message[1] := "Hierarchy items cannot be placed below level " + INTTOSTR(levelLimit) + "     ";
         result := SendAlert(message,FALSE, FALSE, FALSE);
         DISPOSE(message);
         RETURN;
      ELSE 
         deepestDepth := 0;
         FOR i := 1 TO (HIGH(copyHiers))
            ASK copyHiers[i] TO CalculateDepth(currDepth);
            IF (currDepth - copyHiers[i].level) > deepestDepth
               deepestDepth := (currDepth - copyHiers[i].level);;
            END IF;
         END FOR;
         IF (deepestDepth >= (levelLimit - hierLevel))
            NEW(message,1..2);
            message[1] := "Hierarchy items of depth " + INTTOSTR(deepestDepth+1) + " cannot     "
            message[2] := "be placed on this level.     ";
            result := SendAlert(message,FALSE, FALSE, FALSE);
            DISPOSE(message);
            RETURN;
         END IF;
      END IF;
   END IF; 
   depMsg := FALSE;
   nowPasting := TRUE;
   workingPaste := TRUE;
   typeOfCursor := dialogC;
   ClearAllBlocks;
   startingTotalBlocks:=totalBlocks;
   startingTotalHiers := totalHiers;
   startingTotalNodes:=totalNodes;
   ASK menubar TO Disable1Thru8;
   NEW(linkMessage);
   ASK linkMessage TO LoadFromLibrary(images,"LinkText");
   ASK window TO AddGraphic(linkMessage);
   linkText := ASK linkMessage Child("LinkText", 851);
   ASK linkText TO SetText("Pasting multiple items...  please wait...");
   ASK linkText TO SetFont(SystemText);
   ASK linkText TO SetColor(Black);
   ASK linkMessage TO DisplayAt(300.,619.);
   ASK window TO SetSysCursor(BusyCursor);
   ASK window TO SetDeferral(TRUE);
   NEW(copiedGroup);
   NEW(copiedLinksGroup);
   
   IF copyBlocks <> NILARRAY
      cBlocks := HIGH(copyBlocks);
   END IF;
   IF copyEvents <> NILARRAY
      cEvents := HIGH(copyEvents);
   END IF;
   IF copyNodes <> NILARRAY
      cNodes := HIGH(copyNodes);
   END IF;
   IF copyHiers <> NILARRAY
      cHiers := HIGH(copyHiers);
   END IF;
   IF NOT copiedFromPrevFile
      NEW(depChange, 1..(nextId + cBlocks + cEvents + cNodes + cHiers));
      FOR i := 1 TO (nextId + cBlocks + cEvents + cNodes + cHiers);
         depChange[i] := i;
      END FOR;  
   END IF;
   IF copyBlocks <> NILARRAY;
      pasteBlocks := HIGH(copyBlocks);
      FOR i := HIGH(copyBlocks) DOWNTO 1 
         INC(totalBlocks);
         INC(totalObjects);
         NEW(block);
         ASK blockGroup TO Add(block);
         ASK copiedGroup TO Add(block);
         ASK block TO LoadFromLibrary(images, "RBDBlock");
         ASK root TO AddAfterGraphic(defaultBlock,block);
         ASK block TO SetID("RBDBlock", nextId);
         IF NOT copiedFromPrevFile
            depChange[copyBlocks[i].Id] := block.Id;
         END IF;
         ASK block TO SetParentID(copyBlocks[i].parentID);
         ASK block TO GetCloneOf(copyBlocks[i], TRUE);
         ASK block TO SetConnectToNode(FALSE);
         ASK block TO CopyRestOfData(0, 0, copyBlocks[i].xPosition, copyBlocks[i].yPosition);
         ASK block TO SetStats();
         ASK block TO DisplayAt(block.xPosition, block.yPosition);
         IF block.parentID = copyWindow
            SelectBlock;
            ASK block TO SetHidden(FALSE);
            innerSquare := ASK block Child("InnerSquare", 0);
            ASK innerSquare TO SetHidden(TRUE);
            ASK innerSquare TO Draw;
            nameText := ASK block Child("RBDBlockLabel", 0);
            IF fontSize <= 1
               ASK nameText TO SetHidden(TRUE);
            END IF;
            ASK block TO Draw;
         ELSE
            ASK block TO SetHidden(TRUE);
            ASK block TO Draw;
         END IF;
      END FOR;
   END IF;
   IF copyEvents <> NILARRAY;
      pasteEvents := HIGH(copyEvents);
      FOR i := HIGH(copyEvents) DOWNTO 1 
         INC(totalEvents);
         INC(totalObjects);
         NEW(event);
         ASK eventGroup TO Add(event);
         ASK copiedGroup TO Add(event);
         ASK event TO LoadFromLibrary(images, "RBDEvent");
         ASK root TO AddAfterGraphic(defaultBlock,event);
         ASK event TO SetID("RBDEvent", nextId);
         IF NOT copiedFromPrevFile
            depChange[copyEvents[i].Id] := event.Id;
         END IF;
         ASK event TO SetParentID(copyEvents[i].parentID);
         ASK event TO GetCloneOf(copyEvents[i], TRUE);
         ASK event TO SetConnectToNode(FALSE);
         ASK event TO CopyRestOfData(0, 0, copyEvents[i].xPosition, copyEvents[i].yPosition);
         ASK event TO DisplayAt(event.xPosition, event.yPosition);
         IF event.parentID = copyWindow
            SelectEvent;
            ASK event TO SetHidden(FALSE);
            innerSquare := ASK event Child("InnerSquare", 0);
            ASK innerSquare TO SetHidden(TRUE);
            ASK innerSquare TO Draw;
            nameText := ASK event Child("RBDEventLabel", 0);
            IF fontSize <= 1
               ASK nameText TO SetHidden(TRUE);
            END IF;
            ASK event TO Draw;
         ELSE
            ASK event TO SetHidden(TRUE);
            ASK event TO Draw;
         END IF;
      END FOR;
   END IF;
   IF copyHiers <> NILARRAY; {moved 3/1/04 from below copyNodes}
      pasteHiers := HIGH(copyHiers);
      FOR i := HIGH(copyHiers) DOWNTO 1 
         INC(totalHiers);
         INC(totalObjects);
         NEW(hier);
         ASK hierGroup TO Add(hier);
         ASK hier TO CreateChildGroup;
         ASK copiedGroup TO Add(hier);
         ASK hier TO LoadFromLibrary(images, "RBDHier");
         ASK root TO AddAfterGraphic(defaultBlock,hier);
         ASK hier TO SetOldID(copyHiers[i].Id);
         ASK hier TO SetID("RBDHier", nextId);
         ASK hier TO SetDep(copyHiers[i].DependencyNum, copyHiers[i].depType);
         IF NOT copiedFromPrevFile
            depChange[copyHiers[i].Id] := hier.Id;
         END IF;
         ASK hier TO SetParentID(copyHiers[i].parentID);
         ASK hier TO SetName(copyHiers[i].name);
         ASK hier TO SetLevel(copyHiers[i].level);
         IF hier.level > deepestLevel
            deepestLevel := hier.level;
         END IF;
         IF copyHiers[i].usesPhasing
            ASK hier TO SetusesPhasing(TRUE);
            tempImage := ASK hier Child("Hier",603);
            IF (activePhases > 0)
               ASK tempImage TO SetStyle(NarrowCrosshatchFill);
            END IF;
         END IF;
         ASK hier TO SetConnectToNode(FALSE);
         ASK hier TO SetLocation(copyHiers[i].xPosition, copyHiers[i].yPosition);
         ASK hier TO DisplayAt(hier.xPosition, hier.yPosition);
         IF hier.parentID = copyWindow
            SelectHier;
            ASK hier TO SetHidden(FALSE);
            ASK hier TO Draw;
            nameText := ASK hier Child("HierLabel", 0);
            IF fontSize <= 1
               ASK nameText TO SetHidden(TRUE);
            END IF;
         ELSE
            ASK hier TO SetHidden(TRUE);
            ASK hier TO Draw;
         END IF;
         ASK hier TO SetZoom(copyHiers[i].zoom);
         ASK hier TO SetOrigin(copyHiers[i].xOrigin, copyHiers[i].yOrigin);
      END FOR;               
   END IF;
   IF copyNodes <> NILARRAY;
      FOR i := HIGH(copyNodes) DOWNTO 1
         INC(totalNodes);
         INC(totalObjects);
         NEW(node);
         ASK nodeGroup TO Add(node);
         ASK copiedGroup TO Add(node);
         IF copyNodes[i].typeNode = 4
            ASK node TO LoadFromLibrary(images, "RBDInNode");
         ELSIF copyNodes[i].typeNode = 5
            ASK node TO LoadFromLibrary(images, "RBDOutNode");
         ELSIF copyNodes[i].typeNode = 2
            ASK node TO LoadFromLibrary(images, "RBDNode");
         END IF;
         ASK root TO AddAfterGraphic(defaultBlock,node);
         ASK node TO SetID("RBDNode", nextId);
         ASK node TO SetDep(copyNodes[i].DependencyNum, copyNodes[i].depType);
         IF NOT copiedFromPrevFile
            depChange[copyNodes[i].Id] := node.Id;
         END IF;
         ASK node TO SetParentID(copyNodes[i].parentID);
         ASK node TO SetFullFlow(TRUE);
         ASK node TO SetAnyPath(TRUE);         
         ASK node TO SetReportNodeAnal(copyNodes[i].reportNodeAnal);
         IF copyNodes[i].name <> copyNodes[i].nodeName
            ASK node TO SetName(copyNodes[i].name);
         END IF;
         ASK node TO SetType(copyNodes[i].typeNode);
         ASK node TO SetNum(node.Id);
         IF copyNodes[i].usesPhasing
            IF copyNodes[i].phase <> NILARRAY
               NEW(tempPhase, 1..HIGH(copyNodes[i].phase));
               FOR j := 1 TO HIGH(copyNodes[i].phase)
                  tempPhase[j] := copyNodes[i].phase[j];
               END FOR;
               ASK node TO SetPhases(TRUE,tempPhase);
            ELSE
               ASK node TO SetPhases(TRUE,NILARRAY);
            END IF;
            IF tempPhase <> NILARRAY
               DISPOSE(tempPhase);
            END IF;
         ELSE
            ASK node TO SetPhases(FALSE,NILARRAY);
         END IF;
         ASK node TO SetAoDoR("0", "0", "0");
         ASK node TO SetLocation(copyNodes[i].xPosition, copyNodes[i].yPosition);
         ASK node TO DisplayAt(node.xPosition, node.yPosition);
         IF node.parentID = copyWindow
            SelectNode;
            ASK node TO SetHidden(FALSE);
            nameText := ASK node Child("RBDNodeNum", 0);
            IF fontSize <= 1
               ASK nameText TO SetHidden(TRUE);
            END IF;
            ASK node TO Draw;
         ELSE
            ASK node TO SetHidden(TRUE);
            ASK node TO Draw;
         END IF;
      END FOR;
   END IF;

   NEW(selected);
   ASK window TO AddGraphic(selected);
   IF copyNodes <> NILARRAY
      cNodes := HIGH(copyNodes);
   END IF;
   IF copyBlocks <> NILARRAY
      cBlocks := HIGH(copyBlocks);
   END IF;
   IF copyEvents <> NILARRAY
      cEvents := HIGH(copyEvents);
   END IF;
   IF copyHiers <> NILARRAY
      cHiers := HIGH(copyHiers);
   END IF;
   
   IF copyLinks <> NILARRAY
      FOR i :=1 TO HIGH(copyLinks)
         totalLinks := totalLinks + 1;
         NEW(link);
         ASK linkGroup TO Add(link);
         ASK link TO SetID("RBDLink", nextLinkId); 
         ASK link TO SetParentID(copyLinks[i].parentID);
         NEW(linkInts,1..4);
         NEW(linkReals,1..3);
         linkInts[1] := copyLinks[i].coldPriority;
         linkInts[2] := copyLinks[i].capPriority;
         linkInts[3] := copyLinks[i].nomFlow;
         linkInts[4] := copyLinks[i].maxFlow;
         linkReals[1] := copyLinks[i].autoSwitchProb;
         linkReals[2] := copyLinks[i].autoSwitchTime;
         linkReals[3] := copyLinks[i].manualSwitchTime;
         ASK link TO LinkInit(linkInts,linkReals);
         ASK copiedLinksGroup TO Add(link);
         fromType:=copyLinks[i].connectFRef;
         toType:=copyLinks[i].connectTRef; 
     
         IF fromType="RBDBlock"
            FOR j:= 1 TO HIGH(copyBlocks)
               testblock := copyBlocks[j];
               testlink := copyLinks[i];
               IF ((copyBlocks[j].copiedFromId)=(copyLinks[i].connectFromId))
                  fromId := nextId - cEvents - cHiers - cNodes - j;  
                  block := ASK window Descendant("RBDBlock", fromId);
                  ASK block TO IncLink(OUTOF);
                  linkStartX := block.Translation.x + 0.26;
                  linkStartY := block.Translation.y - 0.21;
               END IF;
            END FOR;            
         ELSIF fromType="RBDEvent"
            FOR j:=HIGH(copyEvents) DOWNTO 1
               IF ((copyEvents[j].copiedFromId)=(copyLinks[i].connectFromId))
                  fromId := nextId - cHiers - cNodes - j; 
                  event := ASK window Descendant("RBDEvent", fromId);
                  ASK event TO IncLink(OUTOF);
                  linkStartX := event.Translation.x + 0.26;
                  linkStartY := event.Translation.y - 0.21;
               END IF;
            END FOR;            
         ELSIF fromType="RBDNode"
            FOR j:= HIGH(copyNodes) DOWNTO 1
               IF ((copyNodes[j].copiedFromId)=(copyLinks[i].connectFromId))
                  fromId := nextId - j;
                  node := ASK window Descendant("RBDNode",fromId); 
                  ASK node TO IncLink(OUTOF);
                  linkStartX := node.Translation.x + 0.26;
                  linkStartY := node.Translation.y - 0.21;
                  IF (NOT copyNodes[j].anyPath)
                     {ASK link TO SetCapPriority(copyLinks[i].capPriority);  }
                     ASK node TO SetAnyPath(FALSE);
                  END IF;  
               END IF;
            END FOR;  
         ELSE
            FOR j:=HIGH(copyHiers) DOWNTO 1
               IF ((copyHiers[j].copiedFromId)=(copyLinks[i].connectFromId))
                  fromId := nextId - cNodes - j;
                  hier := ASK window Descendant("RBDHier",fromId); 
                  ASK hier TO IncLink(OUTOF);
                  linkStartX := hier.Translation.x + 0.56;
                  linkStartY := hier.Translation.y - 0.21;
               END IF;
            END FOR; 
         END IF;
         IF toType="RBDBlock"
            FOR j:= 1 TO HIGH(copyBlocks) 
               IF ((copyBlocks[j].copiedFromId)=(copyLinks[i].connectToId))
                  toId := nextId - cEvents - cHiers - cNodes - j; 
                  block := ASK window Descendant("RBDBlock", toId); 
                  ASK block TO IncLink(INTO);
                  linkEndX := block.Translation.x + 0.26;
                  linkEndY := block.Translation.y - 0.21;
               END IF;
            END FOR; 
         ELSIF toType="RBDEvent"
            FOR j:= 1 TO HIGH(copyEvents) 
               IF ((copyEvents[j].copiedFromId)=(copyLinks[i].connectToId))
                  toId := nextId - cHiers - cNodes - j;  
                  event := ASK window Descendant("RBDEvent", toId); 
                  ASK event TO IncLink(INTO);
                  linkEndX := event.Translation.x + 0.26;
                  linkEndY := event.Translation.y - 0.21;
               END IF;
            END FOR; 
         ELSIF toType="RBDNode"
            FOR j:= 1 TO HIGH(copyNodes)
               IF ((copyNodes[j].copiedFromId)=(copyLinks[i].connectToId))
                  toId := nextId - j;  
                  node := ASK window Descendant("RBDNode",toId); 
                  ASK node TO IncLink(INTO);
                  linkEndX := node.Translation.x + 0.26;
                  linkEndY := node.Translation.y - 0.21;
                  IF (NOT copyNodes[j].fullFlow)
                     {ASK link TO SetFlowInVals(copyLinks[i].nomFlow, copyLinks[i].maxFlow);}
                     ASK node TO SetFullFlow(FALSE);
                  END IF;
               END IF;
            END FOR;
         ELSE
            FOR j:=HIGH(copyHiers) DOWNTO 1
               IF ((copyHiers[j].copiedFromId)=(copyLinks[i].connectToId))
                  toId := nextId - cNodes - j;
                  hier := ASK window Descendant("RBDHier",toId); 
                  ASK hier TO IncLink(INTO);
                  linkEndX := hier.Translation.x + 0.56;
                  linkEndY := hier.Translation.y - 0.21;
               END IF;
            END FOR;
         END IF; 
         ASK root TO AddBeforeGraphic(defaultBlock,link);
         ASK link TO SetConnections(fromId,toId,fromType,toType);
         IF (fromType = "RBDBlock") AND (toType = "RBDNode")
            block := ASK root Child("RBDBlock",fromId);
            ASK block TO SetConnectToNode(TRUE);
         END IF;
         IF (fromType = "RBDHier") AND (toType = "RBDNode")
            hier := ASK root Child("RBDHier",fromId);
            ASK hier TO SetConnectToNode(TRUE);
         END IF;
         NEW(linkPoints, 1..6);
         arrowhead(toType, linkStartX, linkStartY, linkEndX,linkEndY, linkPoints[2].x,
            linkPoints[2].y, linkPoints[3].x, linkPoints[3].y, linkPoints[4].x, linkPoints[4].y);
            linkPoints[5].x := linkPoints[2].x;
{                                  3 \        }
{points 5 and 2 are the same:  1---+--2,5--6  }
{                                  4 /        }
         linkPoints[5].y := linkPoints[2].y;
         linkPoints[1].x := linkStartX;
         linkPoints[1].y := linkStartY;
         linkPoints[6].x := linkEndX;
         linkPoints[6].y := linkEndY;
         ASK link TO SetPoints(linkPoints);

         
         IF link.parentID = copyWindow
            ASK selectedLinksGroup TO Add(link);
            ASK link TO SetHighlightColor(Blue);
            ASK link TO SetHighlighted(TRUE);
            ASK root TO RemoveThisGraphic(link);
            ASK link TO ChangeRoots(TRUE, baseroot); 
            ASK selected TO AddGraphic(link);
            ASK link TO Draw;
            INC(linksIn);
         ELSE
            ASK link TO SetHidden(TRUE);
         END IF;

         
         DISPOSE(linkInts);
         DISPOSE(linkReals);
         DISPOSE(linkPoints);
      END FOR;      
   END IF;   

 FOREACH link IN linkGroup
       i:=link.Id;
       j:=link.nomFlow;
 END FOREACH;
   
   
   
   
   {Need to do this after links are copied so connectIntoNum and goodPaths are set}
   IF (copyNodes <> NILARRAY)
      i := HIGH(copyNodes);
      FOREACH obj1 IN copiedGroup
         IF OBJTYPENAME(obj1) = "RBDNodeObj"
            tempNode := ASK root Child("RBDNode", obj1.Id);
            NEW(intsArray, 1..6);
            intsArray[2] := 1;
            intsArray[3] := 1;
            IF tempNode.usesPhasing
               intsArray[5] := 1;
            END IF;
            IF ((tempNode.connectIntoNum)=(copyNodes[i].connectIntoNum)) AND (tempNode.connectIntoNum > 0)
               ASK tempNode TO SetGoodPaths(copyNodes[i].goodPaths);
               ASK tempNode TO SetKofN(tempNode.goodPaths,tempNode.connectIntoNum);
               IF copyNodes[i].coldStandby
                  intsArray[1] := 1;
                  intsArray[4] := copyNodes[i].KStar;
               END IF;
               IF NOT(copyNodes[i].priorityReturn)
                  intsArray[2] := 0;
               END IF;
               IF NOT(copyNodes[i].checkAutosFirst)
                  intsArray[3] := 0;
               END IF;
            ELSIF tempNode.connectIntoNum = 0
               ASK tempNode TO SetGoodPaths(0);
               IF tempNode.parentID = copyWindow
                  ASK tempNode TO SetSelected(TRUE); 
                  ASK tempNode TO SetHighlightColor(Blue);
                  ASK tempNode TO SetHighlighted(TRUE);
               END IF;
            ELSE
               ASK tempNode TO SetGoodPaths(1);
               IF tempNode.usesPhasing
                  FOR j:= 1 TO activePhases
                     IF tempNode.phase[j]>tempNode.connectIntoNum
                        partialCopyNode:=TRUE;
                     END IF;
                  END FOR;
                  IF partialCopyNode
                     ASK tempNode TO SetPhases(TRUE, NILARRAY);
                     IF tempNode.parentID = copyWindow
                        ASK tempNode TO SetSelected(TRUE); 
                        ASK tempNode TO SetHighlightColor(Blue);
                        ASK tempNode TO SetHighlighted(TRUE);
                     END IF;
                  END IF;
               END IF;         
            END IF;                  
            intsArray[6] := copyNodes[i].DependencyNum;
            ASK tempNode TO Init1(intsArray);
            DEC(i);
         END IF;
      END FOREACH;
   END IF;
   {for each hier just copied (copiedGroup)
      go through all objects just copied (copiedGroup)
      if object's parent id is set to hier's old id
         set object's parent id to hier's new id
         add object to hier's childgroup}
   FOREACH obj1 IN copiedGroup
      IF OBJTYPENAME(obj1) = "RBDHierObj"
         tempHier1 := ASK root Child("RBDHier", obj1.Id);
         IF tempHier1.parentID = copyWindow
            ASK tempHier1 TO SetParentID(activeWindow);
            ASK tempHier1 TO SetLevel(hierLevel+1);
         END IF;
         FOREACH obj2 IN copiedGroup 
            IF OBJTYPENAME(obj2) = "RBDBlockObj"     
               tempBlock := ASK root Child("RBDBlock",obj2.Id);
               IF (tempBlock.parentID = tempHier1.oldID)
                  ASK tempBlock TO SetParentID(tempHier1.Id);
                  ASK tempHier1.childGroup TO Add(tempBlock);
               END IF;
            ELSIF OBJTYPENAME(obj2) = "RBDEventObj"     
               tempEvent := ASK root Child("RBDEvent",obj2.Id);
               IF (tempEvent.parentID = tempHier1.oldID)
                  ASK tempEvent TO SetParentID(tempHier1.Id);
                  ASK tempHier1.childGroup TO Add(tempEvent);
               END IF;
            ELSIF OBJTYPENAME(obj2) = "RBDNodeObj" {node}
               tempNode := ASK root Child("RBDNode",obj2.Id);
               IF (tempNode.parentID = tempHier1.oldID)
                  ASK tempNode TO SetParentID(tempHier1.Id);
                  IF tempNode.typeNode = 4
                     ASK tempHier1 TO SetInID(tempNode.Id);
                  ELSIF tempNode.typeNode = 5
                     ASK tempHier1 TO SetOutID(tempNode.Id);
                  END IF;
                  ASK tempHier1.childGroup TO Add(tempNode);
               END IF;
            ELSE {hier}
               tempHier := ASK root Child("RBDHier",obj2.Id);
               IF (tempHier.parentID = tempHier1.oldID) AND (tempHier.Id <> tempHier1.Id) {and it's not itself}
                  ASK tempHier TO SetParentID(tempHier1.Id);
                  ASK tempHier1.childGroup TO Add(tempHier);
               END IF;
            END IF;
         END FOREACH;
         FOREACH tempLink IN copiedLinksGroup
            IF (tempLink.parentID = tempHier1.oldID)
               ASK tempLink TO SetParentID(tempHier1.Id);
               ASK tempHier1.childGroup TO Add(tempLink);
            END IF;
         END FOREACH;      
      END IF;
   END FOREACH;
   {adjust dependency for blocks, nodes, hiers -- events not dependent}
   FOREACH obj1 IN copiedGroup
      IF OBJTYPENAME(obj1) = "RBDBlockObj"
         tempBlock := ASK root Child("RBDBlock", obj1.Id);
         IF tempBlock.DependencyNum > 0
            IF tempBlock.depType = "RBDBlock"
               depBlock := ASK root Child("RBDBlock", tempBlock.DependencyNum);
               IF (depBlock = NILOBJ) AND (tempBlock.DependencyNum = depChange[tempBlock.DependencyNum])
                  ASK tempBlock TO SetDep(0, "");
               ELSE
                  ASK tempBlock TO SetDep(depChange[tempBlock.DependencyNum], "RBDBlock");
               END IF;
            ELSIF tempBlock.depType = "RBDEvent"
               depEvent := ASK root Child("RBDEvent", tempBlock.DependencyNum);
               IF (depEvent = NILOBJ) AND (tempBlock.DependencyNum = depChange[tempBlock.DependencyNum])
                  ASK tempBlock TO SetDep(0, "");
               ELSE
                  ASK tempBlock TO SetDep(depChange[tempBlock.DependencyNum], "RBDEvent");
               END IF;
            ELSIF tempBlock.depType = "RBDNode"
               depNode := ASK root Child("RBDNode", tempBlock.DependencyNum);
               {ex: phasing2, copy SolarP_900W, BatteriesNaS, Bus_FR. delete 900path node. paste}
               IF (depNode = NILOBJ) AND (tempBlock.DependencyNum = depChange[tempBlock.DependencyNum])
                  ASK tempBlock TO SetDep(0, "");
               ELSE {ex: phasing2, copy SolarP_900W, BatteriesNaS, Bus_FR, 900path. delete 900path node. paste}
                  ASK tempBlock TO SetDep(depChange[tempBlock.DependencyNum], "RBDNode");
               END IF;
            ELSIF tempBlock.depType = "RBDHier"  
               depHier := ASK root Child("RBDHier", tempBlock.DependencyNum)
               IF (depHier = NILOBJ) AND (tempBlock.DependencyNum = depChange[tempBlock.DependencyNum])
                  ASK tempBlock TO SetDep(0, "");
               ELSE
                  ASK tempBlock TO SetDep(depChange[tempBlock.DependencyNum], "RBDHier");
               END IF;
            END IF;
         END IF;
      ELSIF OBJTYPENAME(obj1) = "RBDNodeObj"
         tempNode := ASK root Child("RBDNode", obj1.Id);
         IF tempNode.DependencyNum > 0
            IF tempNode.depType = "RBDBlock"
               depBlock := ASK root Child("RBDBlock", tempNode.DependencyNum);
               IF (depBlock = NILOBJ) AND (tempNode.DependencyNum = depChange[tempNode.DependencyNum])
                  ASK tempNode TO SetDep(0, "");
               ELSE
                  ASK tempNode TO SetDep(depChange[tempNode.DependencyNum], "RBDBlock");
               END IF;
            ELSIF tempNode.depType = "RBDEvent"
               depEvent := ASK root Child("RBDEvent", tempNode.DependencyNum);
               IF (depEvent = NILOBJ) AND (tempNode.DependencyNum = depChange[tempNode.DependencyNum])
                  ASK tempNode TO SetDep(0, "");
               ELSE
                  ASK tempNode TO SetDep(depChange[tempNode.DependencyNum], "RBDEvent");
               END IF;
            ELSIF tempNode.depType = "RBDNode"
               depNode := ASK root Child("RBDNode", tempNode.DependencyNum);
               IF (depNode = NILOBJ) AND (tempNode.DependencyNum = depChange[tempNode.DependencyNum])
                  ASK tempNode TO SetDep(0, "");
               ELSE
                  ASK tempNode TO SetDep(depChange[tempNode.DependencyNum], "RBDNode");
               END IF;
            ELSIF tempNode.depType = "RBDHier"
               depHier := ASK root Child("RBDHier", tempNode.DependencyNum);
               IF (depHier = NILOBJ) AND (tempNode.DependencyNum = depChange[tempNode.DependencyNum])
                  ASK tempNode TO SetDep(0, "");
               ELSE
                  ASK tempNode TO SetDep(depChange[tempNode.DependencyNum], "RBDHier");
               END IF;
            END IF;
         END IF;
      ELSIF OBJTYPENAME(obj1) = "RBDHierObj"
         tempHier := ASK root Child("RBDHier", obj1.Id);
         IF tempHier.DependencyNum > 0
            IF tempHier.depType = "RBDBlock"
               depBlock := ASK root Child("RBDBlock", tempNode.DependencyNum);
               IF (depBlock = NILOBJ) AND (tempHier.DependencyNum = depChange[tempHier.DependencyNum])
                  ASK tempHier TO SetDep(0, "");
               ELSE
                  ASK tempHier TO SetDep(depChange[tempHier.DependencyNum], "RBDBlock");
               END IF;
            ELSIF tempHier.depType = "RBDEvent"
               depEvent := ASK root Child("RBDEvent", tempNode.DependencyNum);
               IF (depEvent = NILOBJ) AND (tempHier.DependencyNum = depChange[tempHier.DependencyNum])
                  ASK tempHier TO SetDep(0, "");
               ELSE
                  ASK tempHier TO SetDep(depChange[tempHier.DependencyNum], "RBDEvent");
               END IF;
            ELSIF tempHier.depType = "RBDNode"
               depNode := ASK root Child("RBDNode", tempNode.DependencyNum);
               IF (depNode = NILOBJ) AND (tempHier.DependencyNum = depChange[tempHier.DependencyNum])
                  ASK tempHier TO SetDep(0, "");
               ELSE
                  ASK tempHier TO SetDep(depChange[tempHier.DependencyNum], "RBDNode");
               END IF;
            ELSIF tempHier.depType = "RBDHier"
               depHier := ASK root Child("RBDHier", tempNode.DependencyNum);
               IF (depHier = NILOBJ) AND (tempHier.DependencyNum = depChange[tempHier.DependencyNum])
                  ASK tempHier TO SetDep(0, "");
               ELSE
                  ASK tempHier TO SetDep(depChange[tempHier.DependencyNum], "RBDHier");
               END IF;
            END IF;
         END IF;
      END IF;         
   END FOREACH;     
   
   oldClickX := window.ClickX; {and} oldClickY := window.ClickY;
   Transform(NILOBJ,baseroot, window.PointerX, window.PointerY, attachX, attachY);
   current := ASK selectGroup First();
   FOR i := 1 TO selectGroup.numberIn
      ASK root TO RemoveThisGraphic(current);
      Transform(root,baseroot,current.xPosition,current.yPosition,newx,newy);
      ASK current TO SetScaling(scale,scale);
      ASK current TO SetTranslation(newx,newy);
      ASK selected TO AddGraphic(current);
      ASK current TO Draw; 
      current := ASK selectGroup Next(current);
   END FOR;
   ASK selected TO GetBoundingBox(refX,refY,tempX,tempY);
   Transform(NILOBJ,baseroot,refX,refY,refX,refY);
   Transform(NILOBJ,baseroot,tempX,tempY,tempX,tempY);
   deltaX := attachX-refX;
   deltaY := attachY-tempY;
   current := ASK selectGroup First();
   FOR i := 1 TO selectGroup.numberIn    
      ASK current TO SetTranslation(current.Translation.x+deltaX,current.Translation.y+deltaY);
      ASK current TO ResetLinks(totalLinks, current.Id, current.Translation.x,current.Translation.y, 
                                current.ReferenceName, draggingStuff, nowPasting); 
      current := ASK selectGroup Next(current);
   END FOR;   
   ASK window TO SetCursor(selected);
   ASK window TO SetCursorOffset(-1.*attachX,-1.*attachY);  
   Transform(baseroot,root,refX,refY,refX,refY);
   scrollScreen := TRUE; 
   DISPOSE(linkMessage);
   typeOfCursor := nilC;
   ASK window TO SetSysCursor(NormalCursor);
   ASK window TO SetDeferral(FALSE);
   ASK selected TO Draw;
   ASK window TO Update;
   draggingStuff := TRUE;
   somethingChanged := TRUE;
   workingPaste := FALSE;
   nowPasting := FALSE;
   pastingMultiple := TRUE;
   FOREACH obj1 IN copiedGroup
      ASK copiedGroup TO RemoveThis(obj1);
   END FOREACH;
   DISPOSE(copiedGroup);
   FOREACH link IN copiedLinksGroup
      ASK copiedLinksGroup TO RemoveThis(link);
   END FOREACH;
   DISPOSE(copiedLinksGroup);
END PROCEDURE; {PasteObject}

PROCEDURE CollapseIntoHier;
VAR   
   current : RBDBasicObj;
   tempBlock : RBDBlockObj;
   tempEvent : RBDEventObj;
   tempNode, startNode, endNode, outNode : RBDNodeObj;
   tempHier, tempHier2 : RBDHierObj;
   tempLink, tempLink2, tempLinkS, tempLinkPF, tempLinkPT : LinkObj;
   tempSelectGroup, tempLinkGroup, hier2Group : QueueObj;
   skip, foundLink, result : BOOLEAN;
   label : TextObj;
   oldId, newId, i, j, xloId, xhiId, yhiId, tempaw  : INTEGER;
   obj : ANYOBJ;
   xlo, xhi, yhi, ylo, xdiff, ydiff, newx, newy : REAL;
BEGIN
   FOREACH obj IN selectGroup
      IF OBJTYPENAME(obj) = "RBDNodeObj"
         tempNode := RBDNodeObj(obj);
         IF tempNode.typeNode <> 2
            NEW(message, 1..1);
            IF tempNode.typeNode = 1
               message[1] := "Cannot collapse start marker into a hierarchy     ";
            ELSIF tempNode.typeNode = 3
               message[1] := "Cannot collapse end marker into a hierarchy     ";
            ELSIF tempNode.typeNode = 4             
               message[1] := "Cannot collapse in marker into a hierarchy     ";
            ELSIF tempNode.typeNode = 5
               message[1] := "Cannot collapse out marker into a hierarchy     ";
            END IF;
            result := SendAlert(message, FALSE, FALSE, TRUE);
            RETURN;
         END IF;
      END IF;
   END FOREACH;
  
   NEW(hier2Group);
   FOREACH hier IN hierGroup
      IF selectGroup.Includes(hier)
         ASK hier2Group TO Add(hier);
      END IF;
   END FOREACH;
   FOREACH tempHier IN hier2Group
      ASK tempHier TO AugmentSelectGroup;
      ASK hier2Group TO RemoveThis(tempHier);
   END FOREACH;
   DISPOSE(hier2Group);
   NEW(tempSelectGroup);
   NEW(tempLinkGroup);
   FOREACH current IN selectGroup
      ASK tempSelectGroup TO Add(current);
      IF (OBJTYPENAME(current) = "RBDBlockObj") 
         tempBlock := ASK root Descendant("RBDBlock", current.Id);
         ASK tempBlock TO SetHidden(TRUE);
         ASK tempBlock TO Erase;
      ELSIF (OBJTYPENAME(current) = "RBDEventObj") 
         tempEvent := ASK root Descendant("RBDEvent", current.Id);
         ASK tempEvent TO SetHidden(TRUE);
         ASK tempEvent TO Erase;
      ELSIF (OBJTYPENAME(current) = "RBDNodeObj") 
         tempNode := ASK root Descendant("RBDNode", current.Id);
         ASK tempNode TO SetHidden(TRUE);
         ASK tempNode TO Erase;
      ELSIF (OBJTYPENAME(current) = "RBDHierObj") 
         tempHier := ASK root Descendant("RBDHier", current.Id);
         ASK tempHier TO SetHidden(TRUE);
         ASK tempHier TO Erase;
      END IF;
   END FOREACH;
   FOREACH tempLink2 IN selectedLinksGroup
      ASK tempLinkGroup TO Add(tempLink2);
      ASK tempLink2 TO SetHidden(TRUE);
      ASK tempLink2 TO Erase;
   END FOREACH; 
   FOREACH tempLink IN linkGroup
      i := tempLink.Id;
      FOREACH tempLinkS IN selectedLinksGroup;
         IF tempLinkS.Id = i
            skip := TRUE;
         END IF;
      END FOREACH;
      IF NOT skip
         FOREACH tempLinkPF IN partialFromGroup;
            IF tempLinkPF.Id = i
               skip := TRUE;
               IF tempLinkPF.connectFRef = "RBDBlock"
                  FOREACH current IN selectGroup;
                     IF (OBJTYPENAME(current) = "RBDBlockObj") AND (current.Id = tempLinkPF.connectFromId)
                        tempBlock := RBDBlockObj(current);
                        EXIT;
                     END IF;
                  END FOREACH;
                  IF tempLinkPF.connectTRef = "RBDNode"
                     IF tempBlock.isConnectedNode
                        ASK tempBlock TO SetConnectToNode(FALSE);
                     END IF;
                  END IF;
                  ASK tempBlock TO IncLink(MINUSOUTOF);
               ELSIF tempLinkPF.connectFRef = "RBDEvent"
                  FOREACH current IN selectGroup;
                     IF (OBJTYPENAME(current) = "RBDEventObj") AND (current.Id = tempLinkPF.connectFromId)
                        tempEvent := RBDEventObj(current);
                        EXIT;
                     END IF;
                  END FOREACH;
                  IF tempLinkPF.connectTRef = "RBDNode"
                     IF tempEvent.isConnectedNode
                        ASK tempEvent TO SetConnectToNode(FALSE);
                     END IF;
                  END IF;
                  ASK tempEvent TO IncLink(MINUSOUTOF);
               ELSIF tempLinkPF.connectFRef = "RBDHier"
                  FOREACH current IN selectGroup;
                     IF (OBJTYPENAME(current) = "RBDHierObj") AND (current.Id = tempLinkPF.connectFromId)
                        tempHier := RBDHierObj(current);
                        EXIT;
                     END IF;
                  END FOREACH;
                  IF tempLinkPF.connectTRef = "RBDNode"
                     IF tempHier.isConnectedNode
                        ASK tempHier TO SetConnectToNode(FALSE);
                     END IF;
                  END IF;
                  ASK tempHier TO IncLink(MINUSOUTOF);
               ELSIF tempLinkPF.connectFRef = "RBDNode"
                  FOREACH current IN selectGroup;
                     IF (OBJTYPENAME(current) = "RBDNodeObj") AND (current.Id = tempLinkPF.connectFromId)
                        tempNode := RBDNodeObj(current);
                        EXIT;
                     END IF;
                  END FOREACH;
                  ASK tempNode TO IncLink(MINUSOUTOF);
               END IF;
               IF tempLinkPF.connectTRef = "RBDBlock"
                  tempBlock := ASK root Child("RBDBlock", tempLinkPF.connectToId);
                  ASK tempBlock TO IncLink(MINUSINTO);
               ELSIF tempLinkPF.connectTRef = "RBDEvent"
                  tempEvent := ASK root Child("RBDEvent", tempLinkPF.connectToId);
                  ASK tempEvent TO IncLink(MINUSINTO);
               ELSIF tempLinkPF.connectTRef = "RBDHier"
                  tempHier := ASK root Child("RBDHier", tempLinkPF.connectToId);
                  ASK tempHier TO IncLink(MINUSINTO);
               ELSIF tempLinkPF.connectTRef = "RBDNode"
                  tempNode := ASK root Child("RBDNode", tempLinkPF.connectToId);
                  ASK tempNode TO IncLink(MINUSINTO);
                  IF tempNode.connectIntoNum = 0
                     ASK tempNode TO SetGoodPaths(0);
                  ELSE
                     ASK tempNode TO SetGoodPaths(1);
                  END IF;
                  label := ASK tempNode Child("RBDNodeKofN", 0);
                  ASK label TO SetText("");
                  ASK tempNode TO SetAoDoR("0", "0", "0");
                  ASK tempNode TO Draw;
               END IF;
               oldId := tempLinkPF.Id;
               ASK partialFromGroup TO RemoveThis(tempLinkPF); 
               IF tempLinkPF.parentID > 0
                  tempHier := ASK root Child("RBDHier", tempLinkPF.parentID);
                  ASK tempHier.childGroup TO RemoveThis(tempLinkPF);
               END IF;
               ASK linkGroup TO RemoveThis(tempLinkPF);
               DISPOSE(tempLinkPF);
               totalLinks := totalLinks - 1;
               linkSelected := FALSE;            
            END IF;
         END FOREACH;
      END IF;
      IF NOT skip
         FOREACH tempLinkPT IN partialToGroup;
            IF tempLinkPT.Id = i
               skip := TRUE;
               IF tempLinkPT.connectFRef = "RBDBlock"
                  tempBlock := ASK root Child("RBDBlock", tempLinkPT.connectFromId);
                  IF tempLinkPT.connectTRef = "RBDNode"
                     IF tempBlock.isConnectedNode
                        ASK tempBlock TO SetConnectToNode(FALSE);
                     END IF;
                  END IF;
                  ASK tempBlock TO IncLink(MINUSOUTOF);
               ELSIF tempLinkPT.connectFRef = "RBDEvent"
                  tempEvent := ASK root Child("RBDEvent", tempLinkPT.connectFromId);
                  IF tempLinkPT.connectTRef = "RBDNode"
                     IF tempEvent.isConnectedNode
                        ASK tempEvent TO SetConnectToNode(FALSE);
                     END IF;
                  END IF;
                  ASK tempEvent TO IncLink(MINUSOUTOF);
               ELSIF tempLinkPT.connectFRef = "RBDHier"
                  tempHier := ASK root Child("RBDHier", tempLinkPT.connectFromId);
                  IF tempLinkPT.connectTRef = "RBDNode"
                     IF tempHier.isConnectedNode
                        ASK tempHier TO SetConnectToNode(FALSE);
                     END IF;
                  END IF;
                  ASK tempHier TO IncLink(MINUSOUTOF);
               ELSIF tempLinkPT.connectFRef = "RBDNode"
                  tempNode := ASK root Child("RBDNode", tempLinkPT.connectFromId);
                  ASK tempNode TO IncLink(MINUSOUTOF);
               END IF;
               IF tempLinkPT.connectTRef = "RBDBlock"
                  FOREACH current IN selectGroup;
                     IF (OBJTYPENAME(current) = "RBDBlockObj") AND (current.Id = tempLinkPT.connectToId)
                        tempBlock := RBDBlockObj(current);
                        EXIT;
                     END IF;
                  END FOREACH;
                  ASK tempBlock TO IncLink(MINUSINTO);
               ELSIF tempLinkPT.connectTRef = "RBDEvent"
                  FOREACH current IN selectGroup;
                     IF (OBJTYPENAME(current) = "RBDEventObj") AND (current.Id = tempLinkPT.connectToId)
                        tempEvent := RBDEventObj(current);
                        EXIT;
                     END IF;
                  END FOREACH;
                  ASK tempEvent TO IncLink(MINUSINTO);
               ELSIF tempLinkPT.connectTRef = "RBDHier"
                  FOREACH current IN selectGroup;
                     IF (OBJTYPENAME(current) = "RBDHierObj") AND (current.Id = tempLinkPT.connectToId)
                         tempHier := RBDHierObj(current);
                         EXIT;
                     END IF;
                  END FOREACH;
                  ASK tempHier TO IncLink(MINUSINTO);
               ELSIF tempLinkPT.connectTRef = "RBDNode"
                  FOREACH current IN selectGroup;
                     IF (OBJTYPENAME(current) = "RBDNodeObj") AND (current.Id = tempLinkPT.connectToId)
                        tempNode := RBDNodeObj(current);
                        EXIT;
                     END IF;
                  END FOREACH;
                  ASK tempNode TO IncLink(MINUSINTO);
                  IF tempNode.connectIntoNum = 0
                     ASK tempNode TO SetGoodPaths(0);
                  ELSE
                     ASK tempNode TO SetGoodPaths(1);
                  END IF;
                  label := ASK tempNode Child("RBDNodeKofN", 0);
                  ASK label TO SetText("");
                  ASK tempNode TO SetAoDoR("0", "0", "0");
                  ASK tempNode TO Draw;
               END IF;
               oldId := tempLinkPT.Id;
               ASK partialToGroup TO RemoveThis(tempLinkPT);    
               IF tempLinkPT.parentID > 0
                  hier := ASK root Child("RBDHier", tempLinkPT.parentID);
                  ASK hier.childGroup TO RemoveThis(tempLinkPT);
               END IF;
               ASK linkGroup TO RemoveThis(tempLinkPT);
               DISPOSE(tempLinkPT); {******}
               totalLinks := totalLinks - 1;
               linkSelected := FALSE;            
            END IF;
         END FOREACH;
      END IF;
      IF skip
         skip := FALSE;
      ELSE
         INC(i);
         IF tempLink.parentID = activeWindow
            ASK tempLink TO SetHidden(FALSE);
            ASK tempLink TO Draw;
         ELSE
            ASK tempLink TO SetHidden(TRUE);
            ASK tempLink TO Erase;
         END IF;
      END IF;
   END FOREACH; 
   AddHier;
   {get the last hier just created}
   tempHier := ASK window Child("RBDHier", nextId - 1);
   xlo := 100.;
   ylo := 100.;
   xhi := 3.;
   yhi := 0.;
   FOREACH current IN tempSelectGroup
      IF current.parentID = activeWindow {current being collapsed is not the child of some hier being collapsed}
         IF current.xPosition < xlo
            xlo := current.xPosition;
            xloId := current.Id;
         END IF;
         IF current.xPosition > xhi
            xhi := current.xPosition;
            xhiId := current.Id;
         END IF;
         IF current.yPosition < ylo
            ylo := current.yPosition;
         END IF;
         IF current.yPosition > yhi
            yhi := current.yPosition;
            yhiId := current.Id;
         END IF;
      END IF;
   END FOREACH;
   xdiff := xlo - 3.;
   ydiff := 79. - yhi;
   FOREACH current IN tempSelectGroup
      IF OBJTYPENAME(current) = "RBDHierObj"
         tempHier2 := ASK root Child("RBDHier", current.Id);
         ASK tempHier2 TO SetLevel(tempHier2.level+1);
         IF tempHier2.level > deepestLevel
            deepestLevel := tempHier2.level;
         END IF;
      END IF;
      IF current.parentID = activeWindow {current being collapsed is not the child of some hier being collapsed}
         newx := current.xPosition - xdiff;
         newy := current.yPosition + ydiff;
         ASK current TO DisplayAt(newx, newy);
         ASK current TO ResetLinks(totalLinks, current.Id, current.Translation.x, current.Translation.y, 
                                      current.ReferenceName, draggingStuff, nowPasting);
         IF current.Id = xhiId
            {Adjustment for out marker}
            outxdiff := current.xPosition-20.;
         END IF;
         IF current.parentID > 0 {if current belongs to a hier, remove it from that hier's childgroup}
            tempHier2 := ASK root Child("RBDHier", current.parentID);
            ASK tempHier2.childGroup TO RemoveThis(current);
         END IF;
         ASK current TO SetParentID(tempHier.Id); {set current's parentID to new hier that it's being collapsed into}
         ASK tempHier.childGroup TO Add(current); {add current to new hier's childgroup}
      END IF;
      ASK tempSelectGroup TO RemoveThis(current);
   END FOREACH;
   IF (xhi-xdiff) < 20.
      outxdiff := 0.
   END IF;
   FOREACH tempLink IN tempLinkGroup
      IF tempLink.parentID = activeWindow
         IF tempLink.parentID > 0 {if current belongs to a hier, remove it from that hier's childgroup}
            tempHier2 := ASK root Child("RBDHier", tempLink.parentID);
            ASK tempHier2.childGroup TO RemoveThis(tempLink);
         END IF;
         ASK tempLink TO SetParentID(tempHier.Id);
         ASK tempHier.childGroup TO Add(tempLink);
      END IF;
      ASK tempLinkGroup TO RemoveThis(tempLink);
   END FOREACH;
   collapsing := TRUE;
   DISPOSE(tempSelectGroup);
   DISPOSE(tempLinkGroup);
END PROCEDURE; {CollapseIntoHier}

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
                  ClearAllBlocks;
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
                  {ClearAllBlocks; }
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


PROCEDURE SendToEngine;
VAR
   errors, index                              : INTEGER;
   startStat, tempZoom, tempX, tempY, rpmTemp : REAL;
   tempString                                 : STRING;
   OKToSimulate, cEOR, cFinal,  cEvnt, 
   hatchChange, resim, validStartState,
   cancelled                                  : BOOLEAN;
   button                                     : ButtonObj;
   simBox                                     : SimBoxObj;
   resimBox                                   : ResimBoxObj;
   avtGrdTab                                  : TabObj;
   text                                       : ARRAY INTEGER OF STRING;
   tempImage                                  : FillVObj;
   tempBlock                                  : RBDBlockObj;
   tempEvent                                  : RBDEventObj;
   tempNode                                   : RBDNodeObj; 
   tempHier                                   : RBDHierObj;
   stopRadBox                                 : RadioBoxObj;
   cycleButton                                : RadioButtonObj;
   simFailVal, simRunsVal, simTimeVal, simCycleVal,
   startTVal, sampleVal, timeSly, freqVal, 
   cumKeyParam, relPlotMesh, notUsed4            : ValueBoxObj;
   stopLabel1, stopLabel2, stopLabel3, startT2, label1,
   label2, freqLabel, unitsLabel6, trigText,
   relPlotMeshText, notUsedText4, notUsedText5 : LabelObj;
   graphicsChk, failTimesChk, repTimesChk,
   endTimesChk, keyParamsChk, availPlotChk, 
   capPlotChk, resultsChk, EOROutChk, 
   eventLogChk, costRepChk, stepMode,
   availGraph, statusChk, zeroFailChk, 
   A, B, C, D, E, F, G, H, I, J, notUsed1, notUsed2, detByCompChk : CheckBoxObj;
   trigs, components : OptionListType;
   trigComboBox, detEventComp : ComboBoxObj;
   notUsed5 : TextBoxObj;
BEGIN
   NEW(text,1..40);
   OKToSimulate:=TRUE;
   NEW(simBox);
   ASK simBox TO LoadFromLibrary(dialogs,"SimOptionsBox");
   ASK window TO AddGraphic(simBox);
   stopRadBox  := ASK simBox Descendant("StopRadBox", 101);
   simRunsVal  := ASK simBox Descendant("SimRunsVal", 102);
   graphicsChk := ASK simBox Descendant("GraphicsChkBox", 103);
   stopLabel1  := ASK simBox Descendant("StopLabel1", 104);
   stopLabel2  := ASK simBox Descendant("StopLabel2", 105);
   stopLabel3  := ASK simBox Descendant("StopLabel3", 110);
   simTimeVal  := ASK simBox Descendant("SimTimeVal", 106);
   simFailVal  := ASK simBox Descendant("SimFailVal", 107);
   statusChk   := ASK simBox Descendant("StatusChkBox", 108);
   simCycleVal := ASK simBox Descendant("SimCycleVal", 111);
   failTimesChk := ASK simBox Descendant("FailTimesBox", 201);
   repTimesChk  := ASK simBox Descendant("RepTimesBox", 202);
   endTimesChk  := ASK simBox Descendant("EndTimesBox", 203);
   keyParamsChk := ASK simBox Descendant("KeyParamsChk", 204);
   availPlotChk := ASK simBox Descendant("AvailPlotBox", 209);
   capPlotChk   := ASK simBox Descendant("CapPlotBox", 210);
   resultsChk   := ASK simBox Descendant("ResultsBox", 211);
   EOROutChk    := ASK simBox Descendant("EOROutBox", 212);
   eventLogChk  := ASK simBox Descendant("EventLogBox", 213);
   costRepChk   := ASK simBox Descendant("CostReportBox", 214);
   freqLabel    := ASK simBox Descendant("FreqLabel", 215);
   freqVal      := ASK simBox Descendant("FreqVal", 216);
   startTVal    := ASK simBox Descendant("StartTimeVal", 302);
   startT2      := ASK simBox Descendant("StartLabel2", 303);
   zeroFailChk  := ASK simBox Descendant("ZeroFailChk",305);
   availGraph   := ASK simBox Descendant("AvailGraphBox", 306); 
   sampleVal    := ASK simBox Descendant("SampleVal",307);
   unitsLabel6  := ASK simBox Descendant("UnitsLabel6", 308);
   stepMode     := ASK simBox Descendant("StepModeBox", 309); 
   label1       := ASK simBox Descendant("TimeSliceLabel1", 310);
   timeSly      := ASK simBox Descendant("TimeSliceVal", 311);
   label2       := ASK simBox Descendant("TimeSliceLabel2", 312);
   avtGrdTab    := ASK simBox Child("AvtGrdTab", 400);
   detByCompChk := ASK simBox Descendant("DetByCompChk", 402);
   trigComboBox := ASK simBox Descendant("TrigComboBox", 0);
   trigText     := ASK simBox Descendant("TrigText", 0);
   cumKeyParam  := ASK simBox Descendant("CumKeyParam", 0);
   detEventComp := ASK simBox Descendant("DetEventComp", 0);
   notUsed1     := ASK simBox Descendant("NotUsed", 1);
   notUsed2     := ASK simBox Descendant("NotUsed", 2);
   IF password {eag error 50 fix}
      relPlotMesh     := ASK simBox Descendant("RelPlotMesh", 3);
      relPlotMeshText := ASK simBox Descendant("RelPlotMeshText", 3);
   END IF;
   notUsed4     := ASK simBox Descendant("NotUsed", 4);
   notUsedText4 := ASK simBox Descendant("NotUsedText", 4);
   notUsed5     := ASK simBox Descendant("NotUsed", 5);
   notUsedText5 := ASK simBox Descendant("NotUsedText", 5);
   IF password
      ASK simBox TO Draw;
      IF totalTriggers > 0
         NEW(trigs, 1..totalTriggers);
         GetTrigList(trigs);
         ASK trigComboBox TO SetOptions(trigs);
         IF deferTrig <> ""
            ASK trigComboBox TO SetText(deferTrig);
         END IF;
      ELSE
         ASK trigText TO Deactivate;
         ASK trigComboBox TO Deactivate;
      END IF;
      IF totalObjects > 0
         NEW(components, 1..totalObjects);
         index := 1;
         FOREACH tempBlock IN blockGroup
            tempString := INTTOSTR(tempBlock.Id);
            IF tempBlock.Id < 10
               tempString := "00" + tempString;
            ELSIF tempBlock.Id < 100
               tempString := "0" + tempString;
            END IF;
            components[index] := tempBlock.name + " block - " + tempString;
            INC(index);
         END FOREACH;
         FOREACH tempNode IN nodeGroup
            tempString := INTTOSTR(tempNode.Id);
            IF tempNode.Id < 10
               tempString := "00" + tempString;
            ELSIF tempNode.Id < 100
               tempString := "0" + tempString;
            END IF;
            components[index] := tempNode.name + " node - " + tempString;";
            INC(index);
         END FOREACH;
         FOREACH tempEvent IN eventGroup
            tempString := INTTOSTR(tempEvent.Id);
            IF tempEvent.Id < 10
               tempString := "00" + tempString;
            ELSIF tempEvent.Id < 100
               tempString := "0" + tempString;
            END IF;
            components[index] := tempEvent.name + " event - " + tempString;";
            INC(index);
         END FOREACH;
         FOREACH tempHier IN hierGroup
            tempString := INTTOSTR(tempHier.Id);
            IF tempHier.Id < 10
               tempString := "00" + tempString;
            ELSIF tempHier.Id < 100
               tempString := "0" + tempString;
            END IF;
            components[index] := tempHier.name + " hier - " + tempString;";
            INC(index);
         END FOREACH;
         ASK detEventComp TO SetOptions(components);   
         ASK detEventComp TO Deactivate;
      END IF;
      ASK notUsed1 TO Deactivate;
      ASK notUsed2 TO Deactivate;
      ASK notUsed4 TO Deactivate;
      ASK notUsedText4 TO Deactivate;
      ASK notUsed5 TO Deactivate;
      ASK notUsedText5 TO Deactivate;
   ELSE
      ASK avtGrdTab TO SetHidden(TRUE); {Hide secret tab}
   END IF;

   A := ASK simBox Descendant("A", 0);
   B := ASK simBox Descendant("B", 0);
   C := ASK simBox Descendant("C", 0);
   D := ASK simBox Descendant("D", 0);
   E := ASK simBox Descendant("E", 0);
   F := ASK simBox Descendant("F", 0);
   G := ASK simBox Descendant("G", 0);
   H := ASK simBox Descendant("H", 0);
   I := ASK simBox Descendant("I", 0);
   J := ASK simBox Descendant("J", 0);
   ASK cumKeyParam TO SetValue(0.);
   IF password {eag error 50 fix}
      ASK relPlotMesh TO SetValue(0.);
   END IF;
  
   ASK simTimeVal  TO SetValue(dTimeTrunc);
   ASK simRunsVal TO SetValue(dNumberOfRuns);
   ASK timeSly TO SetValue(dTimeSlice);
   ASK simFailVal TO SetValue(dFailTrunc);
   ASK simCycleVal TO SetValue(dCycleTrunc);
   ASK stopLabel1 TO SetLabel(systemUnits);
   ASK unitsLabel6 TO SetLabel(systemUnits);
   ASK freqVal TO SetValue(720.0);
   ASK statusChk TO SetCheck(statusBarOn);
   IF NOT dSimWithGraph
      ASK timeSly TO SetSelectable(FALSE);
      ASK label1 TO SetSelectable(FALSE);
      ASK label2 TO SetSelectable(FALSE);
   END IF;
   ASK simBox TO Draw;
   IF dSimWithGraph
      ASK graphicsChk TO SetCheck(TRUE);
   ELSE
      ASK graphicsChk TO SetCheck(FALSE);
      ASK stepMode TO Deactivate;
   END IF;
   IF termType = 1 {time truncated}
      ASK startT2 TO SetLabel(systemUnits);
      ASK stopRadBox TO SetSelectedButton(ASK stopRadBox Child("TimeButton", 1011));
      ASK simFailVal TO SetSelectable(FALSE);
      ASK simCycleVal TO SetSelectable(FALSE);
      ASK stopLabel2 TO Deactivate;
      ASK stopLabel3 TO Deactivate;
      ASK availPlotChk TO Activate;
      ASK availGraph TO Activate;
      ASK sampleVal TO Activate;
      ASK unitsLabel6 TO Activate;
      ASK startTVal TO SetValue(dTimeStartTime);
   ELSIF termType = 2 {failure truncated}
      ASK startT2 TO SetLabel("failures");
      ASK stopRadBox TO SetSelectedButton(ASK stopRadBox Child("FailButton", 1012));
      ASK simTimeVal  TO SetSelectable(FALSE);
      ASK simCycleVal TO SetSelectable(FALSE);
      IF dFailTrunc = 1.
         ASK startTVal TO SetSelectable(FALSE);
      END IF;
      ASK stopLabel1 TO Deactivate;
      ASK stopLabel3 TO Deactivate;
      ASK availPlotChk TO Deactivate;
      ASK availGraph TO Deactivate;
      ASK sampleVal TO Deactivate;
      ASK unitsLabel6 TO Deactivate;
      ASK startTVal TO SetValue(dFailStartTime);
   ELSIF termType = 3 {cycle truncated}
      IF activePhases = 0
         NEW(message, 1..3);
         message[1] := "You cannot use cycle terminated without phases defined.    "
         message[2] := "The termination type has been returned to the default     ";
         message[3] := "of time terminated.     ";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
         termType := 1;
         ASK startT2 TO SetLabel(systemUnits);
         ASK stopRadBox TO SetSelectedButton(ASK stopRadBox Child("TimeButton", 1011));
         ASK simFailVal TO SetSelectable(FALSE);
         ASK simCycleVal TO SetSelectable(FALSE);
         ASK stopLabel2 TO Deactivate;
         ASK stopLabel3 TO Deactivate;
         ASK availPlotChk TO Activate;
         ASK availGraph TO Activate;
         ASK sampleVal TO Activate;
         ASK unitsLabel6 TO Activate;
         ASK startTVal TO SetValue(dTimeStartTime);
      ELSE
         ASK startT2 TO SetLabel("cycles");
         ASK stopRadBox TO SetSelectedButton(ASK stopRadBox Child("CycleButton", 1013));
         ASK simTimeVal TO SetSelectable(FALSE);
         ASK simFailVal TO SetSelectable(FALSE);
         ASK stopLabel1 TO Deactivate;
         ASK stopLabel2 TO Deactivate;
         ASK availPlotChk TO Deactivate;
         ASK availGraph TO Deactivate;
         ASK sampleVal TO Deactivate;
         ASK unitsLabel6 TO Deactivate;
         ASK startTVal TO SetValue(dCycleStartTime);
      END IF;
   END IF;
   IF activePhases = 0
      cycleButton := ASK stopRadBox Child("CycleButton", 1013);
      ASK cycleButton TO Deactivate;
   END IF;
   ASK zeroFailChk TO SetCheck(dZeroFail);
   IF sampleRate <> 0.
      ASK sampleVal TO SetValue(sampleRate);
   END IF;
   IF NOT capacityAnalysis
      ASK capPlotChk TO Deactivate;
   END IF;
   IF NOT costAnalysis
      ASK costRepChk TO Deactivate;
      ASK freqLabel TO Deactivate;
   END IF;
   ASK freqVal TO Deactivate;
   ASK sampleVal TO Deactivate;
   IF compileType = "demo"
      ASK failTimesChk TO Deactivate;
      ASK repTimesChk TO Deactivate;
      ASK endTimesChk TO Deactivate;
      ASK keyParamsChk TO Deactivate;
      ASK availPlotChk TO Deactivate;
      ASK capPlotChk TO Deactivate;
      ASK resultsChk TO Deactivate;
      ASK EOROutChk TO Deactivate;
      ASK eventLogChk TO Deactivate;
      ASK costRepChk TO Deactivate;
      ASK freqLabel TO Deactivate;
      ASK freqVal TO Deactivate;
   END IF;
   ASK simBox TO Draw;
   REPEAT
      validStartState := TRUE;
      errors := 0;
      button := ASK simBox TO AcceptInput();
      IF ASK button ReferenceName = "OKButton";
         writeBlkEvs  := A.Checked;
         writeNdEvs   := B.Checked;
         writeSysEvs  := C.Checked;
         writePhsEvs  := D.Checked;
         writeOnlyItemEvs  := E.Checked;
         DomainTree  := F.Checked;
         CoreFile    := G.Checked;
         {avtGrdBools[8]  := H.Checked;
         avtGrdBools[9]  := I.Checked;
         avtGrdBools[10] := J.Checked;}
         deferTrig := trigComboBox.Text();
         IF (password AND detByCompChk.Checked)
            index := POSITION(detEventComp.Text(), " ");
            detComp := detEventComp.Text();
            detComp := SUBSTR(1, index-1, detComp);
         END IF;
         IF dSimWithGraph <> graphicsChk.Checked
            dSimWithGraph := graphicsChk.Checked;
            simOptionChanged := TRUE;
         END IF;
         IF (simRunsVal.Value() < 1.) OR (simRunsVal.Value() > 999999999.)
            INC(errors);
            text[errors] := "Number of runs must be between 1 and 999,999,999!     ";
            ASK simRunsVal TO DisplayValue(dNumberOfRuns);
            validStartState := FALSE;
         ELSE
            numberOfRuns := TRUNC(simRunsVal.Value());
            IF (numberOfRuns<>TRUNC(dNumberOfRuns))
               simOptionChanged := TRUE;
               dNumberOfRuns := FLOAT(numberOfRuns);
            END IF;
         END IF;   
         IF stopRadBox.SelectedButton.Id = 1011
            IF termType <> 1
               simOptionChanged := TRUE;
            END IF;
            simLength := simTimeVal.Value();
            startStat := startTVal.Value();
            IF (simTimeVal.Value() > 999999999.999999) OR (simTimeVal.Value() < 0.000001)
               INC(errors);
               text[errors] := "Stop simulation time must be between 0.000001 and 999,999,999.999999!     ";
               ASK simTimeVal TO DisplayValue(dTimeTrunc);
               validStartState := FALSE;
            END IF;
            IF (startTVal.Value() > 999999999.999998) OR (startTVal.Value() < 0.0)
               INC(errors);
               text[errors] := "Start statistics time must be between 0 and 999,999,999.999998!     ";
               ASK startTVal TO DisplayValue(dTimeStartTime);
               validStartState := FALSE;
            END IF;
            IF simLength <= startStat
               INC(errors);
               text[errors] := "Start statistics time must be less than stop simulation time!     ";
               ASK startTVal TO DisplayValue(dTimeStartTime);
               validStartState := FALSE;
            END IF;
            IF (startStat<>dTimeStartTime) AND validStartState
               simOptionChanged := TRUE;
               dTimeStartTime := startStat;
            END IF;
            IF (simLength<>dTimeTrunc) AND validStartState
               simOptionChanged := TRUE;
               dTimeTrunc := simLength;
            END IF;
            termType := 1;
         ELSIF stopRadBox.SelectedButton.Id = 1012
            IF termType <> 2
               simOptionChanged := TRUE;
            END IF;
            simLength := simFailVal.Value();
            startStat := startTVal.Value();
            IF (simLength < 1.) OR (simLength > 999999999.)
               INC(errors);
               text[errors] := "Stop simulation failure must be between 1 and 999,999,999!     ";
               ASK simFailVal TO DisplayValue(dFailTrunc);
               validStartState := FALSE;
            END IF;
            IF (startStat < 0.) OR (startStat > 999999998.)
               INC(errors);
               text[errors] := "Start statisics failure must be between 0 and 999,999,998!     ";
               ASK startTVal TO DisplayValue(dFailStartTime);
               validStartState := FALSE;
            END IF;
            IF simLength <= startStat
               INC(errors);
               text[errors] := "Start statistics failure must be less than stop simulation failure!     ";
               ASK startTVal TO DisplayValue(dFailStartTime);
               validStartState := FALSE;
            END IF;
            IF (simLength<>dFailTrunc) AND validStartState
               simOptionChanged := TRUE;
               dFailTrunc := simLength;
            END IF;
            IF (startStat<>dFailStartTime) AND validStartState
               simOptionChanged := TRUE;
               dFailStartTime := FLOAT(ROUND(startStat));
            END IF;
            termType := 2;
         ELSIF stopRadBox.SelectedButton.Id = 1013
            IF termType <> 3
               simOptionChanged := TRUE;
            END IF;
            simLength := simCycleVal.Value();
            startStat := startTVal.Value();
            IF (simLength < 1.) OR (simLength > 999999999.)
               INC(errors);
               text[errors] := "Stop simulation cycle must be between 1 and 999,999,999!     ";
               ASK simCycleVal TO DisplayValue(dCycleTrunc);
               validStartState := FALSE;
            END IF;
            IF (startStat < 0.) OR (startStat > 999999998.)
               INC(errors);
               text[errors] := "Start statisics cycle must be between 0 and 999,999,998!     ";
               ASK startTVal TO DisplayValue(dCycleStartTime);
               validStartState := FALSE;
            END IF;
            IF simLength <= startStat
               INC(errors);
               text[errors] := "Start statistics cycle must be less than stop simulation cycle!     ";
               ASK startTVal TO DisplayValue(dCycleStartTime);
               validStartState := FALSE;
            END IF;
            IF (simLength<>dCycleTrunc) AND validStartState
               simOptionChanged := TRUE;
               dCycleTrunc := simLength;
            END IF;
            IF (startStat<>dCycleStartTime) AND validStartState
               simOptionChanged := TRUE;
               dCycleStartTime := FLOAT(ROUND(startStat));
            END IF;
            termType := 3;
         END IF;        
         IF dZeroFail <> zeroFailChk.Checked
            dZeroFail := zeroFailChk.Checked;
            simOptionChanged := TRUE;
         END IF; 
         sampleRate := sampleVal.Value();
         IF (sampleRate > simLength) AND availGraph.Checked
            INC(errors);
            text[errors] := "Ao plot sample rate must be less than the simulation length!     ";
            ASK sampleVal TO DisplayValue(simLength/10.);
            validStartState := FALSE;
         END IF;
         IF (sampleRate < 0.000001) OR (sampleRate > 999999999.999998)
            INC(errors);
            text[errors] := "Ao plot sample rate must be between 0.000001 and 999,999,999.999998!     ";
            ASK sampleVal TO DisplayValue(10.);
            validStartState := FALSE;
         END IF;
         IF (simLength/sampleRate > 1000.) AND availGraph.Checked AND (simLength > 0.) AND (sampleRate > 0.);
            NEW(message,1..3);
            message[1] := "Sample rate too low; graph cannot exceed 1000 data points.";
            message[2] := "Maximum sample rate for full length graph is "+INTTOSTR(CEIL(simLength/1000.))+".     ";
            message[3] := "Do you wish to continue with the current settings?";
            result := SendAlert(message, FALSE, TRUE, FALSE);
            DISPOSE(message);
            IF NOT result
               validStartState := FALSE;
            END IF;
         END IF;   
         IF (timeSly.Value() < 0.000001) OR (timeSly.Value() > 10000000.)
            INC(errors);
            text[errors] := "Time slice must be between 0.000001 and 10,000,000!     ";
            ASK timeSly TO DisplayValue(dTimeSlice);
            validStartState := FALSE;
         END IF;        
         IF (freqVal.Value <= 0.0) OR (freqVal.Value > 999999999.99)
            INC(errors);
            text[errors] := "The interim cost file save frequency must be greater than 0 and less than 999,999,999.99!     ";
            ASK freqVal TO DisplayValue(720.0);
            validStartState := FALSE;
         END IF;   
         IF stepMode.Checked
            startStep := TRUE;
            inStepMode := TRUE;
         ELSE
            startStep := FALSE;
            inStepMode := FALSE;
         END IF;
         cFinal := resultsChk.Checked; 
         cEvnt  := eventLogChk.Checked;
         cEOR   := EOROutChk.Checked;
         statusBarOn := statusChk.Checked;
      ELSIF ASK button ReferenceName = "CancelButton"
         currentView := "workspace";
         OKToSimulate := FALSE;
         validStartState := TRUE;
         DISPOSE(simBox);  
         FOREACH link IN linkGroup 
            ASK link TO CleanUp;
         END FOREACH;                      
      END IF;   
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
   UNTIL validStartState;   
   DISPOSE(text);
   IF OKToSimulate
      ASK window TO SetSysCursor(BusyCursor);
      hitXinSim := FALSE;
      analViewAvail := FALSE;
      doneSimming := TRUE;
      cancelled := FALSE;
      REPEAT
         IF NOT doneSimming
            doneSimming := TRUE;
            NEW(resimBox);
            resim := TRUE;
            ASK resimBox TO LoadFromLibrary(dialogs,"ResimBox");
            ASK window TO AddGraphic(resimBox);
            ASK resimBox TO GetPreferences(simLength,termType,numberOfRuns,cancelled,phasingInUse,
                                           inStepMode, startStep,statusBarOn, dSimWithGraph,cEvnt,cEOR,cFinal);
            IF (numberOfRuns<>TRUNC(dNumberOfRuns))
               simOptionChanged := TRUE;
               dNumberOfRuns := FLOAT(numberOfRuns);
            END IF;
            FOR i := 0 TO 8
               ASK window TO ShowStatus(i,"");
            END FOR;
            IF dSimWithGraph 
               faceBoxOpen := TRUE;
               {unhide face in case hidden in change zoom}
               ASK greenFace TO SetHidden(FALSE);
               ASK yellowFace TO SetHidden(FALSE);
               ASK redFace TO SetHidden(FALSE);
               ASK window TO DisplayFace(1);
               ASK window TO ShowStatus(0,"Sim Speed : "+SUBSTR(1,8,REALTOSTR(dTimeSlice)));
               IF timeSlice <  0.000001
                  timeSlice := 1./dTimeSlice;
               END IF;
            ELSE
               faceBoxOpen := FALSE;
               ASK window TO DisplayFace(4);
               timeSlice := 0.0000001;
            END IF;
            IF termType = 1
               IF (simLength<>dTimeTrunc) AND (NOT cancelled)
                  simOptionChanged := TRUE;
                  dTimeTrunc := simLength;
               END IF;
               termType := 1;
            ELSIF termType = 2
               IF (simLength<>dFailTrunc) AND (NOT cancelled)
                  simOptionChanged := TRUE;
                  dFailTrunc := simLength;
               END IF;
               termType := 2;
            ELSIF termType = 3
               IF (simLength<>dCycleTrunc) AND (NOT cancelled)
                  simOptionChanged := TRUE;
                  dCycleTrunc := simLength;
               END IF;
               termType := 3;
            END IF;
            DISPOSE(resimBox);
            ASK window TO ShowStatus(1,"Simulating...");
            ASK simMenuBar TO ChangeState("InitSim");
         ELSE
            DisplayAoGraph := availGraph.Checked;
            timeSlice := timeSly.Value();
            IF timeSlice <> dTimeSlice
               simOptionChanged := TRUE;
               dTimeSlice := timeSlice;
            END IF;
            IF NOT dSimWithGraph
               timeSlice := 0.0000001;
            ELSIF timeSlice <> 0.
               timeSlice := 1. / timeSlice;
            END IF;
            nowSimulating := TRUE;
            ASK window TO InitSimDisplay();
            IF simulated
               DISPOSE(FinalArray);
            END IF;
            IF DisplayAoGraph   {Ao plot}
               NEW(AoGraphWindow);
               ASK AoGraphWindow TO SetColor(Grey);
               ASK AoGraphWindow TO Startup;
               NEW(AoGraph);    
               NEW(IntAoGraph);
               NEW(MultiRunGraph);   
               ASK AoGraph TO LoadFromLibrary(images, "Ao Chart");   
               ASK IntAoGraph TO LoadFromLibrary(images, "IntAo Chart");
               ASK MultiRunGraph TO LoadFromLibrary(images, "MultiRun Chart");
               ASK AoGraphWindow TO AddGraphic(AoGraph);   
               ASK AoGraphWindow TO AddGraphic(IntAoGraph);
               ASK AoGraphWindow TO AddGraphic(MultiRunGraph);  
               ASK AoGraph TO SetRanges(0.,simLength,yMin,yMax);  
               ASK IntAoGraph TO SetRanges(0.,simLength,yMin,yMax);
               ASK MultiRunGraph TO SetRanges(0.,simLength,yMin,yMax);   
               ASK MultiRunGraph TO SetPartLineStyle(ChartDataSet,1,SolidLine,.15);
               ASK MultiRunGraph TO SetPartLineStyle(ChartDataSet,2,SolidLine,.15);
               ASK MultiRunGraph TO SetPartLineStyle(ChartDataSet,3,SolidLine,.15);
               IF currentGraph = 1
                  ASK AoGraphWindow TO SetTitle("Per Trial Ao Graph");
                  ASK AoGraph TO SetHidden(FALSE);
                  ASK IntAoGraph TO SetHidden(TRUE);
                  ASK MultiRunGraph TO SetHidden(TRUE);    
               ELSIF currentGraph = 2 
                  ASK AoGraphWindow TO SetTitle("Interval Ao Graph");
                  ASK AoGraph TO SetHidden(TRUE); 
                  ASK IntAoGraph TO SetHidden(FALSE);
                  ASK MultiRunGraph TO SetHidden(TRUE);
               ELSE             
                  ASK AoGraphWindow TO SetTitle("Multi-run Ao Graph");
                  ASK AoGraph TO SetHidden(TRUE);
                  ASK IntAoGraph TO SetHidden(TRUE);
                  ASK MultiRunGraph TO SetHidden(FALSE);
                  ASK AoGraphWindow TO Draw;       
               END IF;
               ASK AoGraphWindow TO Draw;
            END IF;
         END IF;
         IF NOT cancelled
            ASK window TO SetSysCursor(BusyCursor);
            typeOfCursor := simC;
            FOREACH tempNode IN nodeGroup
               IF ((tempNode.typeNode <> 5) AND (tempNode.usesPhasing) AND (activePhases > 0))
                  tempImage := ASK tempNode Child("Node",602);
                  ASK tempImage TO SetStyle(SolidFill);
                  hatchChange := TRUE;
               END IF;
            END FOREACH; 
            FOREACH tempBlock IN blockGroup
               IF tempBlock.usesPhasing AND (activePhases > 0)
                  tempImage := ASK tempBlock Child("BasicBlock",601);
                  ASK tempImage TO SetStyle(SolidFill);
                  hatchChange := TRUE;
               END IF;
            END FOREACH;
            FOREACH tempEvent IN eventGroup
               IF tempEvent.usesPhasing AND (activePhases > 0)
                  tempImage := ASK tempEvent Child("BasicBlock",601);
                  ASK tempImage TO SetStyle(SolidFill);
                  hatchChange := TRUE;
               END IF;
            END FOREACH;
            FOREACH tempHier IN hierGroup
               IF tempHier.usesPhasing AND (activePhases > 0)
                  tempImage := ASK tempHier Child("Hier", 603);
                  ASK tempImage TO SetStyle(SolidFill);
                  hatchChange := TRUE;
               END IF;
            END FOREACH;  
            DISPOSE(FinalArray);
            ASK menuTool TO SetHidden(TRUE);
            ASK simToolBar TO SetHidden(FALSE); 
            ASK menubar TO SetHidden(TRUE);
            ASK simMenuBar TO SetHidden(FALSE);
            IF DisplayAoGraph  {this will make it really slow for large rbd's using this graph}
               ASK menuTool TO Draw;
               ASK simToolBar TO Draw;
               ASK menubar TO Draw;
               ASK simMenuBar TO Draw;
            END IF;
  
{NEW(slowMsgBox);
ASK slowMsgBox TO LoadFromLibrary(dialogs, "SlowMsgBox");
ASK window TO AddGraphic(slowMsgBox);
ASK slowMsgBox TO Draw;
      button := ASK slowMsgBox TO AcceptInput();
Delay(5);
ASK slowMsgBox TO Erase;
DISPOSE(slowMsgBox);   }

{            NEW(message,1..1);
               message[1] := "this may take some time ...";
  
   NEW(messageBox);
   ASK messageBox TO SetTextBuffer(message);
   ASK window TO AddGraphic(messageBox);
   ASK messageBox TO Draw;
   Delay(5);
   ASK messageBox TO Erase;
   DISPOSE(messageBox);}
            ASK window TO SetSysCursor(BusyCursor); {eag error 51 fix}
            ignoreMouse := TRUE; 
            ASK window TO Update;
            ignoreMouse := FALSE;
            ASK root TO Draw; 
            ASK simMenuBar TO ChangeState("InitSim");
            ASK window TO SetSysCursor(NormalCursor); 
            IF password {eag error 50 fix}
               rpmTemp := relPlotMesh.Value();
            END IF;
            IF resim
               StartEngine(dSimWithGraph,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,cFinal,cEOR,cEvnt,
                           FALSE,availGraph.Checked,freqVal.Value,sampleRate,simLength,
                           startStat,timeSlice,rpmTemp,numberOfRuns,TRUNC(cumKeyParam.Value()));  
            ELSE
               StartEngine(dSimWithGraph,failTimesChk.Checked,repTimesChk.Checked,endTimesChk.Checked,
                           keyParamsChk.Checked,availPlotChk.Checked,capPlotChk.Checked,cFinal,cEOR,
                           cEvnt,costRepChk.Checked,availGraph.Checked,freqVal.Value, 
                           sampleRate,simLength,startStat,timeSlice,rpmTemp,numberOfRuns,TRUNC(cumKeyParam.Value()));  
            END IF;
         END IF;
      UNTIL doneSimming;
      ASK window TO SetSysCursor(BusyCursor);
      FOREACH link IN linkGroup
         ASK link TO CleanUp;
      END FOREACH;                      
      DISPOSE(simBox);
      ASK window TO ShowStatus(0, "");
      typeOfCursor := dialogC;
      ASK window TO SetScrollable(TRUE,TRUE);
      IF totalPools > 0
         DISPOSE(PoolArray);
      END IF;
      IF DisplayAoGraph   {Ao plot}
         DISPOSE (graphMenuBar);                                                                                         
         DISPOSE (AoGraph);
         DISPOSE (IntAoGraph);
         DISPOSE (MultiRunGraph);
         DISPOSE (AoGraphWindow);
         ASK window TO SetSize(100.,100.);
         DisplayAoGraph := FALSE;
         IF activeWindow <> 0 
            tempHier := ASK root Child("RBDHier", activeWindow);
            tempZoom := tempHier.zoom;
            tempX := tempHier.xOrigin;
            tempY := tempHier.yOrigin;
         ELSE
            tempZoom := cusZoomVal;
            tempX := xOrigin;
            tempY := yOrigin;
         END IF;
         noCapVals := TRUE;
         SetView(tempZoom, xOrigin, yOrigin);
         noCapVals := FALSE;
      END IF;
      {cusZoomVal := oldCusVal; {remove after ISO}
      IF changedZoom
         noCapVals := TRUE;
         SetView(cusZoomVal, oldx, oldy);
         noCapVals := FALSE;
      END IF;}
      configFrozen := TRUE;
      nowSimulating := FALSE;
      IF greenFace <> NILOBJ;
         DISPOSE(greenFace);
         DISPOSE(yellowFace);
         DISPOSE(redFace);
      END IF;
      ASK window TO SetPanes;       
      {IF hatchChange
         FOREACH node IN nodeGroup
            IF node.usesPhasing AND (activePhases > 0)
               tempImage := ASK node Child("Node",602);
               ASK tempImage TO SetStyle(NarrowCrosshatchFill);
            END IF;
         END FOREACH;
         FOREACH block IN blockGroup
            IF block.usesPhasing AND (activePhases > 0)
               tempImage := ASK block Child("BasicBlock",601);
               ASK tempImage TO SetStyle(NarrowCrosshatchFill);
            END IF;
         END FOREACH;
         FOREACH event IN eventGroup
            IF event.usesPhasing AND (activePhases > 0)
               tempImage := ASK event Child("BasicBlock",601);
               ASK tempImage TO SetStyle(NarrowCrosshatchFill);
            END IF;
         END FOREACH;
         FOREACH hier IN hierGroup
            IF hier.usesPhasing AND (activePhases > 0)
               tempImage := ASK hier Child("Hier", 603);
               ASK tempImage TO SetStyle(NarrowCrosshatchFill);
            END IF;
         END FOREACH;
      END IF; }
      ASK simMenuBar TO SetHidden(TRUE); 
      ASK menubar TO SetHidden(FALSE);
      ASK menuTool TO SetHidden(FALSE);
      ASK simToolBar TO SetHidden(TRUE);  
      nowSimulating := TRUE;
      ASK grid TO Colorize("UnSim");
      ASK window TO SetDeferral(FALSE);
      ASK window TO SetSysCursor(BusyCursor); {eag error 51 fix}
      ignoreMouse := TRUE; 
      ASK window TO Update;
      ignoreMouse := FALSE;
      ASK window TO SetSysCursor(NormalCursor); 
      nowSimulating := FALSE;
      currentView := "workspace";
   END IF;       
   typeOfCursor := nilC;
   inEndState := FALSE;
   ASK menubar TO Enable2Thru6;
   DISPOSE(message);  
   inStepMode := FALSE;
   IF capacityAnalysis
      FOREACH link IN capLinkGroup
         ASK capLinkGroup TO RemoveThis(link);
      END FOREACH;
      FOREACH node IN capNodeGroup
         ASK capNodeGroup TO RemoveThis(node);
      END FOREACH;
   END IF;
   ASK window TO SetSysCursor(NormalCursor);
END PROCEDURE; {SendToEngine}


PROCEDURE StartFailureEffects;
VAR
   tempBlock                                  : RBDBlockObj;
   tempNode                                   : RBDNodeObj;
BEGIN
    FOR i := 0 TO 6
       ASK window TO ShowStatus(i,"");
    END FOR;
    ASK window TO InitFEVDisplay();
    typeOfCursor:=fevC;
    ASK window TO SetPanes;
    FOREACH tempNode IN nodeGroup
       ASK tempNode TO fevInit;
    END FOREACH;
    ASK window TO SetSysCursor(BusyCursor); {eag error 51 fix}
    ignoreMouse := TRUE; 
    ASK window TO Update;
    ignoreMouse := FALSE; 
    ASK fevToolBar TO SetNavigation;
    ASK fevToolBar TO Initialize;
    ASK fevMenuBar TO SetNavigation;
    ASK fevMenuBar TO Initialize;
    ASK window TO SetSysCursor(NormalCursor); {eag error 51 fix -- moved from before window Update}
    StartFEVmode;
END PROCEDURE; {StartFailureEffects}

PROCEDURE EndFailureEffects;
BEGIN
    EndFEVmode;
    FOREACH link IN linkGroup 
       ASK link TO CleanUp;
    END FOREACH;                      
    ASK window TO ShowStatus(0, "");
    typeOfCursor := dialogC;
    ASK window TO SetScrollable(TRUE,TRUE);
  {  cusZoomVal := oldCusVal; {zoomissue - nodepaths -sets zoom back if changed during fev (don't want this anymore)}
    IF changedZoom
       noCapVals := TRUE;
       SetView(cusZoomVal, oldx, oldy);
       noCapVals := FALSE;
    END IF;              }
    configFrozen := TRUE;
    nowSimulating := FALSE;
    IF greenFace <> NILOBJ;
       DISPOSE(greenFace);
       DISPOSE(yellowFace);
       DISPOSE(redFace);
    END IF;
    ASK window TO SetPanes;
    ASK fevMenuBar TO SetHidden(TRUE); 
    ASK menubar TO SetHidden(FALSE);
    ASK menuTool TO SetHidden(FALSE);
    ASK fevToolBar TO SetHidden(TRUE);  
    nowSimulating := TRUE;
    ASK grid TO Colorize("UnSim");
    ASK window TO SetDeferral(FALSE);
    ASK window TO SetSysCursor(BusyCursor); {eag error 51 fix}
    ignoreMouse := TRUE;
    ASK window TO Update;
    ignoreMouse := FALSE;
    ASK window TO SetSysCursor(NormalCursor);
    nowSimulating := FALSE;
    typeOfCursor := nilC;
    currentView := "workspace";
    inEndState := FALSE;
    ASK menubar TO Enable2Thru6;
    IF capacityAnalysis
      FOREACH link IN capLinkGroup
         ASK capLinkGroup TO RemoveThis(link);
      END FOREACH;
      FOREACH node IN capNodeGroup
         ASK capNodeGroup TO RemoveThis(node);
      END FOREACH;
   END IF;

END PROCEDURE; {EndFailureEffects}


PROCEDURE ResetPasted; 
BEGIN
   IF copyBlocks <> NILARRAY
      copiedFromPrevFile := TRUE;
      FOR i := 1 TO HIGH(copyBlocks)
         IF ((copyBlocks[i].poolName<>"unnamed") OR (copyBlocks[i].res1Name<>"unnamed") 
             OR (copyBlocks[i].usesPhasing) OR (copyBlocks[i].pmTriggered))
            ASK copyBlocks[i] TO ResetForPasting;
         END IF;
      END FOR;
   END IF;
   IF copyEvents <> NILARRAY
      copiedFromPrevFile := TRUE;
      FOR i := 1 TO HIGH(copyEvents)
         IF (copyEvents[i].usesPhasing) 
            ASK copyEvents[i] TO ResetForPasting;
         END IF;
      END FOR;
   END IF;
   IF copyHiers <> NILARRAY
      copiedFromPrevFile := TRUE;
      FOR i := 1 TO HIGH(copyHiers)
         ASK copyHiers[i] TO SetusesPhasing(FALSE);
      END FOR;
   END IF;  
   IF copyNodes <> NILARRAY
      copiedFromPrevFile := TRUE;
      resetting := TRUE;
      FOR i := 1 TO HIGH(copyNodes)
         ASK copyNodes[i] TO SetPhases(FALSE,NILARRAY);
         IF (copyNodes[i].DependencyNum > 0)
            ASK copyNodes[i] TO SetDep(0, "");
         END IF;
      END FOR;
      resetting := FALSE;
   END IF;
END PROCEDURE;

PROCEDURE SelectBlock;
BEGIN
   ASK block TO SetSelected(TRUE);
   ASK block TO SetHighlightColor(Blue);
   ASK block TO SetHighlighted(TRUE);
   ASK block TO SetTranslation(block.Translation.x, block.Translation.y);
   ASK block TO Draw;
   blueObjRef := "RBDBlock";
   blueObjId := block.Id;
   ASK selectGroup TO Add(block);
   INC(blocksIn);
END PROCEDURE; {SelectBlock}

PROCEDURE SelectEvent;
BEGIN
   ASK event TO SetSelected(TRUE);
   ASK event TO SetHighlightColor(Blue);
   ASK event TO SetHighlighted(TRUE);
   ASK event TO SetTranslation(event.Translation.x, event.Translation.y);
   ASK event TO Draw;
   blueObjRef := "RBDEvent";
   blueObjId := event.Id;
   ASK selectGroup TO Add(event);
   INC(eventsIn);
END PROCEDURE; {SelectEvent}

PROCEDURE SelectHier;
BEGIN
   ASK hier TO SetSelected(TRUE);
   ASK hier TO SetHighlightColor(Blue);
   ASK hier TO SetHighlighted(TRUE);
   ASK hier TO SetTranslation(hier.Translation.x, hier.Translation.y);
   ASK hier TO Draw;
   blueObjRef := "RBDHier";
   blueObjId := hier.Id;
   ASK selectGroup TO Add(hier);
   INC(hiersIn);
END PROCEDURE; {SelectHier}

PROCEDURE SelectNode;
BEGIN
   ASK node TO SetSelected(TRUE); 
   ASK node TO SetHighlightColor(Blue);
   ASK node TO SetHighlighted(TRUE);      
   ASK node TO SetTranslation(node.Translation.x, node.Translation.y); 
   ASK node TO Draw;  
   blueObjRef := "RBDNode";
   blueObjId := node.Id;
   ASK selectGroup TO Add(node);
   INC(nodesIn);
END PROCEDURE; {SelectNode}

PROCEDURE FindOffset(IN inZoom : INTEGER;   OUT outOffset : REAL);
VAR
   index, modulus       : INTEGER; 
   offsetArray          : ARRAY INTEGER OF REAL; 
BEGIN
   NEW(offsetArray,0..20);
   IF displaySetting = 1280 {1280 X 1024}
      offsetArray[0]:=  0.85;       {cusZoomVal = 0}    {0%}                                 
      offsetArray[1]:=  0.85;       {cusZoomVal = 6}    {5%}                                 
      offsetArray[2]:=  0.98;       {cusZoomVal = 12}   {10%}                                  
      offsetArray[3]:=  1.06;       {cusZoomVal = 18}   {15%}                                  
      offsetArray[4]:=  1.13;       {cusZoomVal = 24}   {20%}                                  
      offsetArray[5]:=  1.17;       {cusZoomVal = 30}   {25%}                                  
      offsetArray[6]:=  1.2;        {cusZoomVal = 36}   {30%}                                  
      offsetArray[7]:=  1.2;        {cusZoomVal = 42}   {35%}                                  
      offsetArray[8]:=  1.2;        {cusZoomVal = 48}   {40%}                                  
      offsetArray[9]:=  1.21;       {cusZoomVal = 54}   {45%}                                  
      offsetArray[10]:= 1.21;       {cusZoomVal = 60}   {50%}                                  
      offsetArray[11]:= 1.2;        {cusZoomVal = 66}   {55%}                                  
      offsetArray[12]:= 1.2;        {cusZoomVal = 72}   {60%}                                  
      offsetArray[13]:= 1.215;      {cusZoomVal = 78}   {65%}                                  
      offsetArray[14]:= 1.22;       {cusZoomVal = 84}   {70%}                                  
      offsetArray[15]:= 1.225;      {cusZoomVal = 90}   {75%}                                  
      offsetArray[16]:= 1.24;       {cusZoomVal = 96}   {80%}                                  
      offsetArray[17]:= 1.26;       {cusZoomVal = 102}  {85%}                                   
      offsetArray[18]:= 1.26;       {cusZoomVal = 108}  {90%}                                   
      offsetArray[19]:= 1.26;       {cusZoomVal = 114}  {95%}    
      offsetArray[20]:= 1.26;       {cusZoomVal = 120}  {100%}    
   ELSE
      offsetArray[0]:=  -.52;   {cusZoomVal = 0}    {0%}                                 
      offsetArray[1]:=  -.52;   {cusZoomVal = 6}    {5%}                                 
      offsetArray[2]:=  -.38;   {cusZoomVal = 12}   {10%}                                  
      offsetArray[3]:=  -.29;   {cusZoomVal = 18}   {15%}                                  
      offsetArray[4]:=  -.25;   {cusZoomVal = 24}   {20%}                                  
      offsetArray[5]:=  -.2;    {cusZoomVal = 30}   {25%}                                  
      offsetArray[6]:=  -.18;   {cusZoomVal = 36}   {30%}                                  
      offsetArray[7]:=  -.15;   {cusZoomVal = 42}   {35%}                                  
      offsetArray[8]:=  -.14;   {cusZoomVal = 48}   {40%}                                  
      offsetArray[9]:=  -.17;   {cusZoomVal = 54}   {45%}                                  
      offsetArray[10]:= -.16;   {cusZoomVal = 60}   {50%}                                  
      offsetArray[11]:= -.15;   {cusZoomVal = 66}   {55%}                                  
      offsetArray[12]:= -.16;   {cusZoomVal = 72}   {60%}                                  
      offsetArray[13]:= -.16;   {cusZoomVal = 78}   {65%}                                  
      offsetArray[14]:= -.16;   {cusZoomVal = 84}   {70%}                                  
      offsetArray[15]:= -.13;   {cusZoomVal = 90}   {75%}                                  
      offsetArray[16]:= -.12;   {cusZoomVal = 96}   {80%}                                  
      offsetArray[17]:= -.1;    {cusZoomVal = 102}  {85%}                                   
      offsetArray[18]:= -.1;    {cusZoomVal = 108}  {90%}                                   
      offsetArray[19]:= -.1;    {cusZoomVal = 114}  {95%} 
      offsetArray[20]:= -.1;    {cusZoomVal = 120}  {100%} 
   END IF;
   index := inZoom DIV 6;
   modulus := inZoom MOD 6;                                                                
   IF (modulus = 0)
      outOffset:= offsetArray[index];
   ELSE  
      outOffset:= offsetArray[index] + (FLOAT(modulus)/6.0)*(offsetArray[index+1]-offsetArray[index]);                        
   END IF; 
   DISPOSE(offsetArray);
END PROCEDURE;

PROCEDURE ZoomFit;
VAR
   xlo, xhi, ylo, yhi, localZoom, fittedZoom : REAL;
   tempBlock : RBDBlockObj;
   tempEvent : RBDEventObj;
   tempNode  : RBDNodeObj;
   tempHier  : RBDHierObj;
BEGIN
   IF activeWindow > 0
      tempHier := ASK root Child("RBDHier", activeWindow);
      localZoom := tempHier.zoom;
   ELSE
      localZoom := cusZoomVal;
   END IF;
   xlo := 100.; xhi := 0.;
   ylo := 100.; yhi := 0.;
   FOREACH tempBlock IN blockGroup
      IF tempBlock.parentID = activeWindow
         IF tempBlock.xPosition < xlo
            xlo := tempBlock.xPosition;
         ELSIF tempBlock.xPosition > xhi
            xhi := tempBlock.xPosition;
         END IF;
         IF tempBlock.yPosition < ylo
            ylo := tempBlock.yPosition;
         ELSIF tempBlock.yPosition > yhi
            yhi := tempBlock.yPosition;
         END IF;
      END IF;
   END FOREACH;
   FOREACH tempEvent IN eventGroup
      IF tempEvent.parentID = activeWindow
         IF tempEvent.xPosition < xlo
            xlo := tempEvent.xPosition;
         ELSIF tempEvent.xPosition > xhi
            xhi := tempEvent.xPosition;
         END IF;
         IF tempEvent.yPosition < ylo
            ylo := tempEvent.yPosition;
         ELSIF tempEvent.yPosition > yhi
            yhi := tempEvent.yPosition;
         END IF;
      END IF;
   END FOREACH;
   FOREACH tempNode IN nodeGroup
      IF tempNode.parentID = activeWindow
         IF tempNode.xPosition < xlo
            xlo := tempNode.xPosition;
         ELSIF tempNode.xPosition > xhi
            xhi := tempNode.xPosition;
         END IF;
         IF tempNode.yPosition < ylo
            ylo := tempNode.yPosition;
         ELSIF tempNode.yPosition > yhi
            yhi := tempNode.yPosition;
         END IF;
      END IF;
   END FOREACH;
   FOREACH tempHier IN hierGroup
      IF tempHier.parentID = activeWindow
         IF tempHier.xPosition < xlo
            xlo := tempHier.xPosition;
         ELSIF tempHier.xPosition > xhi
            xhi := tempHier.xPosition;
         END IF;
         IF tempHier.yPosition < ylo
            ylo := tempHier.yPosition;
         ELSIF tempHier.yPosition > yhi
            yhi := tempHier.yPosition;
         END IF;
      END IF;
   END FOREACH;
   xhi := xhi + 2.;
   ylo := ylo - 2.;
   IF (xhi-xlo)*12.85/20. > (yhi-ylo)
      fittedZoom := xhi-xlo;
   ELSE
      fittedZoom := (yhi-ylo)*20./12.85;
   END IF;
   IF fittedZoom < 10.;
      fittedZoom := 10.;
   END IF;
   simZoomX := xlo;
   simZoomY := yhi;    

   IF fittedZoom > (xhi-xlo)
      simZoomX := simZoomX - ((fittedZoom-(xhi-xlo))/2.);
   END IF;
   IF simZoomX < 2.
      simZoomX := 0.;
      fittedZoom := fittedZoom + 1.;
   ELSIF simZoomX > (120.-fittedZoom)
      simZoomX := 120.-fittedZoom;
   END IF;
   IF (fittedZoom*12.85/20.) > (yhi-ylo)
      simZoomY := simZoomY + (((fittedZoom*12.85/20.)-(yhi-ylo))/2.);
   END IF;
   IF simZoomY < 0.
      simZoomY := 0.;
   ELSIF simZoomY > 80.
      simZoomY := 80.;
   END IF;
   IF fittedZoom <> localZoom
      localZoom := fittedZoom;
      IF activeWindow > 0
         tempHier := ASK root Child("RBDHier", activeWindow);
         ASK tempHier TO SetZoom(localZoom);
      ELSE
         cusZoomVal := localZoom;
      END IF;
      IF NOT collapsing
      SetView(localZoom, simZoomX,simZoomY);
      END IF;
      changedZoom := TRUE;
   ELSE
      ASK window TO Update;
   END IF; 
END METHOD;

PROCEDURE ZoomPercent(IN ClickX, ClickY : REAL; IN center : BOOLEAN);
VAR
   centerX, centerY, tempZoom, tempXOrig, tempYOrig: REAL;
   zoomBox                                    : HelpBoxObj;
   zoomDropBox                                : ComboBoxObj;
   button                                     : ButtonObj;
   goodZoom   : BOOLEAN;
   tempHier : RBDHierObj;
BEGIN
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
   Transform(NILOBJ, root, ClickX, ClickY, centerX, centerY);
   ClearAllBlocks;
   typeOfCursor := dialogC;
   NEW(zoomBox);
   ASK zoomBox TO LoadFromLibrary(dialogs, "ZoomBox");
   ASK window TO AddGraphic(zoomBox);
   IF center
      ASK zoomBox TO SetLabel("Center Zoom");
   ELSE
      ASK zoomBox TO SetLabel("Zoom");
   END IF;
   zoomDropBox := ASK zoomBox Child("ZoomDropBox",100);
   ASK zoomDropBox TO SetText(INTTOSTR(TRUNC(tempZoom*5./6.)));
   ASK zoomBox TO Draw;
   button := ASK zoomBox Child("OKButton",0);
   ASK button TO SetDefault(TRUE);
   ASK button TO Draw;
   REPEAT
      button := ASK zoomBox TO AcceptInput();
      IF button.ReferenceName = "OKButton"
         IF ((STRTOREAL(zoomDropBox.Text()) < 5.) OR (STRTOREAL(zoomDropBox.Text()) > 100.))
            NEW(message,1..1);
            message[1] := "Custom zoom value must be between 5 and 100!     ";
            result := SendAlert(message,FALSE, FALSE, TRUE);
            DISPOSE(message);
            ASK zoomDropBox TO SetText(INTTOSTR(TRUNC(tempZoom*5./6.)));
            goodZoom := FALSE;
         ELSE
            IF (STRTOREAL(zoomDropBox.Text) = 100.) {If 100% selected, force 99%}
               tempZoom := 118.;
            ELSE
               tempZoom := STRTOREAL(zoomDropBox.Text())*6./5.;
            END IF;
            goodZoom := TRUE;
         END IF;
      ELSE
         goodZoom := TRUE;
      END IF;
   UNTIL goodZoom;
   IF button.ReferenceName = "OKButton"
      IF activeWindow > 0 {start}     {moved this section into "if OKButton" where it should be}
         tempHier := ASK root Child("RBDHier", activeWindow);
         tempHier.SetZoom(tempZoom);
         IF center
            ASK tempHier TO SetOrigin(centerX-tempZoom/2., centerY+tempZoom*13.2/40.);
         ELSE
            ASK tempHier TO SetOrigin(tempXOrig, tempYOrig);
         END IF;
      ELSE
         cusZoomVal := tempZoom;
         IF center
            xOrigin := centerX-tempZoom/2.;
            yOrigin := centerY+tempZoom*13.2/40.;
         ELSE
            xOrigin := tempXOrig;
            yOrigin := tempYOrig;
         END IF;  
      END IF;                    {end}
      IF center
         SetView(tempZoom, (centerX-tempZoom/2.), (centerY+tempZoom*13.2/40.));
      ELSE
         SetView(tempZoom, tempXOrig, tempYOrig);
      END IF;
      ASK menubar TO SetZoomButts;
   END IF;
   DISPOSE(zoomBox); 
   IF currentView = "simulation"
      typeOfCursor := simC;
   ELSIF currentView = "workspace"
      typeOfCursor := nilC;
   ELSIF currentView = "fev";
      typeOfCursor := fevC;
   ELSIF currentView = "weaklink"
      typeOfCursor := weakC;
   END IF;
END PROCEDURE; {ZoomPercent}

PROCEDURE SetGraphZoom(INOUT gZoom, simX, simY  : REAL);
VAR
   xlo, xhi, ylo, yhi : REAL;
BEGIN
   xlo := 100.; xhi := 0.;
   ylo := 100.; yhi := 0.;
   FOREACH block IN blockGroup
      IF block.parentID = activeWindow
         IF block.xPosition < xlo
            xlo := block.xPosition;
         ELSIF block.xPosition > xhi
            xhi := block.xPosition;
         END IF;
         IF block.yPosition < ylo
            ylo := block.yPosition;
         ELSIF block.yPosition > yhi
             yhi := block.yPosition;
         END IF;
      END IF;
   END FOREACH;
   FOREACH event IN eventGroup
      IF event.parentID = activeWindow
         IF event.xPosition < xlo
            xlo := event.xPosition;
         ELSIF event.xPosition > xhi
            xhi := event.xPosition;
         END IF;
         IF event.yPosition < ylo
            ylo := event.yPosition;
         ELSIF event.yPosition > yhi
            yhi := event.yPosition;
         END IF;
      END IF;
   END FOREACH;
   FOREACH node IN nodeGroup
      IF node.parentID = activeWindow
         IF node.xPosition < xlo
            xlo := node.xPosition;
         ELSIF node.xPosition > xhi
            xhi := node.xPosition;
         END IF;
         IF node.yPosition < ylo
            ylo := node.yPosition;
         ELSIF node.yPosition > yhi
            yhi := node.yPosition;
         END IF;
      END IF;
   END FOREACH;
   FOREACH hier IN hierGroup
      IF hier.parentID = activeWindow
         IF hier.xPosition < xlo
            xlo := hier.xPosition;
         ELSIF hier.xPosition > xhi
            xhi := hier.xPosition;
         END IF;
         IF hier.yPosition < ylo
            ylo := hier.yPosition;
         ELSIF hier.yPosition > yhi
            yhi := hier.yPosition;
         END IF;
      END IF;
   END FOREACH;
   xhi := xhi + 1.;
   ylo := ylo - 1.;
   IF (xhi-xlo)*6.9/20. > (yhi-ylo)
      gZoom := xhi-xlo;
   ELSE
      gZoom := (yhi-ylo)*20./6.9;
   END IF;
   IF gZoom < 10.;
      gZoom := 10.;
   END IF;
   simX := xlo;
   simY := yhi;    
   IF gZoom > (xhi-xlo)
      simX := simX - ((gZoom-(xhi-xlo))/2.);
   END IF;
   IF simX < 2.
      simX := 0.;
   ELSIF simX > (120.-gZoom)
      simX := 120.-gZoom;
   END IF;
   IF (gZoom*6.9/20.) > (yhi-ylo)
      simY := simY + (((gZoom*6.9/20.)-(yhi-ylo))/2.);
   END IF;
   IF simY < 0.
      simY := 0.;
   ELSIF simY > 80.
      simY := 80.;
   END IF;  
END PROCEDURE;

PROCEDURE SetView(IN cusZoom, xIn, yIn : REAL);
VAR
   temp, offset         : REAL;
   tempHier : RBDHierObj;
BEGIN
   IF nowSimulating AND DisplayAoGraph
      yShift := 6.9;
   ELSIF (cusZoom = 120.) OR nowSimulating
      yShift := 12.85;
   ELSE
      yShift := 12.75;
   END IF;
   xIn := MAXOF(0.,xIn);
   IF (xIn+cusZoom) > 120.
      xIn := xIn+120.-(xIn+cusZoom);
   END IF;
   yIn := MINOF(80.,yIn);
   IF (yIn-(yShift/20.*cusZoom)) < 0.
      yIn :=yShift/20.*cusZoom;   
   END IF;
   IF (NOT dontChangeXY)  {zoomissue - nodepaths}
      IF activeWindow <> 0
         tempHier := ASK root Child("RBDHier", activeWindow);
         ASK tempHier TO SetOrigin(xIn, yIn);
      ELSE
         xOrigin := xIn;
         yOrigin := yIn;
      END IF;
   END IF;
   IF nowSimulating 
      ASK simToolBar TO SetHidden(FALSE);
   ELSIF analUp
      ASK weakToolBar TO SetHidden(FALSE);
   ELSIF currentView = "fev"
      ASK fevToolBar TO SetHidden(FALSE);
   ELSE
      ASK menuTool TO SetHidden(FALSE);
   END IF;
   offset := ((-2.03*POWER(cusZoom,4.))+(625.*POWER(cusZoom,3.))-(67900.*POWER(cusZoom,2.))+
             (3130000.*cusZoom)-61000000.-screenOffset)/100000000.; 
   ASK scaleroot TO SetWorld(0.,0.,cusZoom+(cusZoom*.004),(cusZoom+(cusZoom*.004))*aspectRatio);
   ASK root TO SetTranslation(-1.*xIn,-1.*(yIn-((yShift+offset)*(cusZoom/20.))));
   {IF NOT (draggingStuff OR draggingnode OR draggingblock OR draggingevent OR dragginghier)}
      SetTextSize;
{   END IF;                                                                                   }
   IF NOT nowSimulating
      IF (cusZoom = 120.) AND (lastZoom <> 120.)
         ASK window TO Update;
         ASK window TO SetScrollable(FALSE,FALSE);
      ELSIF (cusZoom < 120.) AND (lastZoom = 120.)
         ASK window TO Update;
         ASK window TO SetScrollable(TRUE,TRUE);
         ASK window TO SetScroll;
      ELSE
         ASK window TO SetScroll;
      END IF;
      lastZoom := cusZoom;
   END IF; 
   IF nowSimulating AND (NOT dSimWithGraph) 
      ASK greenFace TO SetHidden(TRUE);
      ASK yellowFace TO SetHidden(TRUE);
      ASK redFace TO SetHidden(TRUE);
   END IF;
   ASK window TO Update; 
   Transform(scaleroot, baseroot, 1., 1., scale, temp);
END METHOD; {SetView}

PROCEDURE SetTextSize;
VAR
   blockLabel, text1, text2, text3 : TextObj;
   tempNode                        : RBDNodeObj;
   tempBlock                       : RBDBlockObj;
   tempHier                        : RBDHierObj;
   tempEvent                       : RBDEventObj;
   localZoom                       : REAL;
BEGIN
   IF (activeWindow > 0)
      tempHier := ASK root Child("RBDHier", activeWindow);
      localZoom := tempHier.zoom;
   ELSE
      localZoom := cusZoomVal;
   END IF;
   IF (localZoom >= 4.0) AND (localZoom < 18.0) 
      fontSize := 25-TRUNC(localZoom);
   ELSIF (localZoom >= 18.0) AND (localZoom < 27.0) 
      fontSize := 7;
   ELSIF (localZoom >= 27.0) AND (localZoom < 40.0) 
      fontSize := 6;
   ELSIF (localZoom >= 40.0) AND (localZoom <= 48.0) 
      fontSize := 4;
   ELSE 
      fontSize := 1;
   END IF;
   IF NOT loadingFile
      IF fontSize > 1
         FOREACH tempBlock IN blockGroup
            blockLabel :=  ASK tempBlock Child("RBDBlockLabel", 0);
            ASK blockLabel TO SetSysFont("SmallFonts",fontSize,70,0);
            IF ((tempBlock.parentID = activeWindow) OR selectGroup.Includes(tempBlock))
               ASK blockLabel TO SetHidden(FALSE);
            END IF;
            text3 := ASK tempBlock Descendant("BlockAoText", 0);
            ASK text3 TO SetSysFont("SmallFonts",fontSize-1,70,0);            
            IF ((analUp AND weakAnalysis) AND (tempBlock.parentID = activeWindow))
               ASK text3 TO SetHidden(FALSE);
            ELSE
               ASK text3 TO SetHidden(TRUE);
            END IF;
         END FOREACH;   
         FOREACH tempEvent IN eventGroup
            blockLabel :=  ASK tempEvent Child("RBDEventLabel", 0);
            ASK blockLabel TO SetSysFont("SmallFonts",fontSize,70,0);
            IF ((tempEvent.parentID = activeWindow) OR selectGroup.Includes(tempEvent))
               ASK blockLabel TO SetHidden(FALSE);
            END IF;
            text3 := ASK tempEvent Descendant("EventAoText", 0);
            ASK text3 TO SetSysFont("SmallFonts",fontSize-1,70,0);            
            IF ((analUp AND weakAnalysis) AND (tempEvent.parentID = activeWindow))
               ASK text3 TO SetHidden(FALSE);
            ELSE
               ASK text3 TO SetHidden(TRUE);
            END IF;
         END FOREACH;   
         FOREACH tempHier IN hierGroup
            blockLabel :=  ASK tempHier Child("HierLabel", 0);
            text3 := ASK tempHier Descendant("HierAoText", 0);
            ASK blockLabel TO SetSysFont("SmallFonts",fontSize,70,0);
            ASK text3 TO SetSysFont("SmallFonts",fontSize-1,70,0);
            IF ((analUp AND weakAnalysis) AND (tempHier.parentID = activeWindow))
               ASK text3 TO SetHidden(FALSE);
            ELSE
               ASK text3 TO SetHidden(TRUE);
            END IF;
            IF ((tempHier.parentID = activeWindow) OR selectGroup.Includes(tempHier))
               ASK blockLabel TO SetHidden(FALSE);
            ELSE
               ASK text3 TO SetHidden(TRUE);
            END IF;
         END FOREACH;   
         FOREACH tempNode IN nodeGroup
            text1 := ASK tempNode Child("RBDNodeNum", 0);
            text2 := ASK tempNode Child("RBDNodeKofN", 0);
            text3 := ASK tempNode Descendant("NodeAoText", 0);
            ASK text1 TO SetSysFont("SmallFonts",fontSize,70,0);
            ASK text2 TO SetSysFont("SmallFonts",fontSize,70,0);
            ASK text3 TO SetSysFont("SmallFonts",fontSize-1,70,0);      
            IF (analUp AND weakAnalysis) OR (capacityAnalysis AND ((nowSimulating) OR FEVmode)
               AND (NOT noCapVals)) AND (tempNode.parentID = activeWindow)
               ASK text3 TO SetHidden(FALSE);
            ELSE
               ASK text3 TO SetHidden(TRUE);
            END IF;
            IF ((tempNode.parentID = activeWindow) OR selectGroup.Includes(tempNode))
               ASK text1 TO SetHidden(FALSE);
               ASK text2 TO SetHidden(FALSE);
               ASK tempNode TO SetHidden(FALSE);
            ELSE
               ASK text1 TO SetHidden(TRUE);
               ASK text2 TO SetHidden(TRUE);
               ASK text3 TO SetHidden(TRUE);
               ASK tempNode TO SetHidden(TRUE);
            END IF;
         END FOREACH;
      ELSE   
         FOREACH tempBlock IN blockGroup
            blockLabel :=  ASK tempBlock Child("RBDBlockLabel", 0);
            ASK blockLabel TO SetSysFont("SmallFonts",fontSize,70,0);{0,70,0);??}
            ASK blockLabel TO SetHidden(TRUE);
            text3 := ASK tempBlock Child("BlockAoText", 0);
            ASK text3 TO SetSysFont("SmallFonts",fontSize,70,0);
            ASK text3 TO SetHidden(TRUE);
         END FOREACH;   
         FOREACH tempEvent IN eventGroup
            blockLabel :=  ASK tempEvent Child("RBDEventLabel", 0);
            ASK blockLabel TO SetSysFont("SmallFonts",fontSize,70,0);{0,70,0);??}
            ASK blockLabel TO SetHidden(TRUE);
            text3 := ASK tempEvent Child("EventAoText", 0);
            ASK text3 TO SetSysFont("SmallFonts",fontSize,70,0);
            ASK text3 TO SetHidden(TRUE);
         END FOREACH;   
         FOREACH tempHier IN hierGroup
            blockLabel :=  ASK tempHier Child("HierLabel", 0);
            ASK blockLabel TO SetSysFont("SmallFonts",fontSize,70,0);
            ASK blockLabel TO SetHidden(TRUE);
            text3 := ASK tempHier Child("HierAoText", 0);
            ASK text3 TO SetSysFont("SmallFonts",fontSize,70,0);
            ASK text3 TO SetHidden(TRUE);
         END FOREACH;   
         FOREACH tempNode IN nodeGroup
            text1 := ASK tempNode Child("RBDNodeNum", 0);
            ASK text1 TO SetSysFont("SmallFonts",fontSize,70,0);
            text2 := ASK tempNode Child("RBDNodeKofN", 0);
            ASK text2 TO SetSysFont("SmallFonts",fontSize,70,0);
            text3 := ASK tempNode Child("NodeAoText", 0);
            ASK text3 TO SetSysFont("SmallFonts",fontSize,70,0);
            ASK text1 TO SetHidden(TRUE);
            ASK text2 TO SetHidden(TRUE);
            ASK text3 TO SetHidden(TRUE);
         END FOREACH;
      END IF;
   END IF;
   IF localZoom > 48.
      ASK window TO SetShowToolTips(TRUE);
   ELSE
      ASK window TO SetShowToolTips(FALSE);
   END IF;
END METHOD;

PROCEDURE CheckOpenFileStatus;
BEGIN
   ASK window TO SetSysCursor(BusyCursor);
   IF fileIsOpen
      totalObjects := totalBlocks + totalEvents + totalNodes + totalHiers;
      ASK menubar TO Enable2Thru6;
      IF compileType = "demo"
         ASK window TO SetTitle("RAPTOR 7.0 DEMO - " + nameOfFile);
      ELSIF compileType = "student"
         ASK window TO SetTitle("RAPTOR 7.0 STUDENT - " + nameOfFile);
      ELSIF compileType = "gmd"
         ASK window TO SetTitle("RAPTOR 6.5 - " + nameOfFile);
      ELSE
         ASK window TO SetTitle("RAPTOR 7.0 - " + nameOfFile);
      END IF;
      GetSpareList(spareList);
      GetResList(resList);
      GetTrigList(trigList);
   ELSE
      totalObjects := 0;
      totalBlocks  := 0;
      totalLinks   := 0;
      totalNodes   := 0;
      simOptionChanged := FALSE;
      ASK menubar TO Enable(6);
   END IF;
   blueObjRef := "";
   blueObjId := -1;
   ASK window TO SetSysCursor(NormalCursor);
   simOptionChanged := FALSE;
END PROCEDURE; {CheckOpenFileStatus}

PROCEDURE ShowAnalView(IN display : BOOLEAN);
VAR
   tempNode              : RBDNodeObj;
   tempBlock             : RBDBlockObj;
   tempEvent             : RBDEventObj;
   tempHier              : RBDHierObj;
   tempImage             : FillVObj;
   menuitem                               : MenuItemObj;
   buttitem                               : PaletteButtonObj;
BEGIN
   ASK window TO SetSysCursor(BusyCursor); {eag error 51 fix}
   ignoreMouse := TRUE; 
   IF display
      AoVisible := TRUE;
      typeOfCursor := weakC;
      ClearAllBlocks;
      ASK grid TO Colorize("NodeAnal");
      ASK menuTool TO SetHidden(TRUE);
      ASK menuTool TO Draw;
      ASK simToolBar TO SetHidden(TRUE); 
      ASK menubar TO SetHidden(TRUE);
      ASK menubar TO Draw;  
      ASK simMenuBar TO SetHidden(TRUE); 
      ASK weakToolBar TO SetHidden(FALSE);
      ASK weakToolBar TO Draw;
      ASK weakToolBar TO SetNavigation;
      ASK weakMenuBar TO SetHidden(FALSE);
      ASK weakMenuBar TO Draw;
      ASK weakMenuBar TO SetNavigation;
      analUp := TRUE;
      FOREACH tempNode IN nodeGroup
         IF tempNode.usesPhasing AND (activePhases > 0)
            tempImage := ASK tempNode Child("Node",602);
            ASK tempImage TO SetStyle(SolidFill);
         END IF;
         ASK tempNode TO ShowAnalColor;
      END FOREACH;
      FOREACH tempBlock IN blockGroup
         IF tempBlock.usesPhasing AND (activePhases > 0)
            tempImage := ASK tempBlock Child("BasicBlock",601);
            ASK tempImage TO SetStyle(SolidFill);
         END IF;
         ASK tempBlock TO ShowAnalColor;
      END FOREACH;
      FOREACH tempEvent IN eventGroup
         IF tempEvent.usesPhasing AND (activePhases > 0)
            tempImage := ASK tempEvent Child("BasicBlock",601);
            ASK tempImage TO SetStyle(SolidFill);
         END IF;
         ASK tempEvent TO ShowAnalColor;
      END FOREACH;
      FOREACH tempHier IN hierGroup
         IF tempHier.usesPhasing AND (activePhases > 0)
            tempImage := ASK tempHier Child("Hier", 603);
            ASK tempImage TO SetStyle(SolidFill);
         END IF;
      END FOREACH;
      oldX := xOrigin;
      oldY := yOrigin;
      IF compileType = "demo"
         ASK weakToolBar TO Draw;
         ASK weakMenuBar TO Draw;
         menuitem := ASK weakMenuBar Descendant("PrintItem", 101);
         ASK menuitem TO Deactivate;
         menuitem := ASK weakMenuBar Descendant("SetupItem", 102);
         ASK menuitem TO Deactivate;
         menuitem := ASK weakMenuBar Descendant("SaveBMPItem", 103);
         ASK menuitem TO Deactivate;
         buttitem := ASK weakToolBar Descendant("PrintButton", 901);
         ASK buttitem TO Deactivate;
      END IF;
   ELSE
      ASK window TO SetDeferral(TRUE);
      typeOfCursor := nilC;
      ASK simMenuBar TO SetHidden(TRUE); 
      ASK weakMenuBar TO SetHidden(TRUE);
      ASK weakMenuBar TO Draw;
      ASK menubar TO SetHidden(FALSE);
      ASK menubar TO Draw;
      ASK menubar TO SetNavigation;
      ASK simToolBar TO SetHidden(TRUE);  
      ASK weakToolBar TO SetHidden(TRUE);
      ASK weakToolBar TO Draw;
      ASK menuTool TO SetHidden(FALSE);
      ASK menuTool TO Draw;
      ASK menuTool TO SetNavigation;
      IF NOT nowSimulating
         ASK grid TO Colorize("UnSim");
      END IF;
      analUp := FALSE;
      inEndState := FALSE;
      FOREACH tempNode IN nodeGroup
         IF tempNode.typeNode <> 5
            IF tempNode.usesPhasing AND (activePhases > 0)
               tempImage := ASK tempNode Child("Node",602);
               ASK tempImage TO SetStyle(NarrowCrosshatchFill);
            END IF;
         END IF;
      END FOREACH;
      FOREACH tempBlock IN blockGroup
         IF tempBlock.usesPhasing AND (activePhases > 0)
            tempImage := ASK tempBlock Child("BasicBlock",601);
            ASK tempImage TO SetStyle(NarrowCrosshatchFill);
            ASK tempBlock TO SetSelectable(FALSE);
         END IF;
      END FOREACH;
      FOREACH tempEvent IN eventGroup
         IF tempEvent.usesPhasing AND (activePhases > 0)
            tempImage := ASK tempEvent Child("BasicBlock",601);
            ASK tempImage TO SetStyle(NarrowCrosshatchFill);
         END IF;
      END FOREACH;
      FOREACH tempHier IN hierGroup
         IF tempHier.usesPhasing AND (activePhases > 0)
            tempImage := ASK tempHier Child("Hier", 603);
            ASK tempImage TO SetStyle(NarrowCrosshatchFill);
            ASK tempHier TO SetSelectable(TRUE);
         END IF;
      END FOREACH;
      IF NOT nowSimulating
         ASK window TO SetDeferral(FALSE);
      END IF;
   END IF;
   ASK window TO Update;
   ignoreMouse := FALSE; {eag error 51 fix}
   ASK window TO SetSysCursor(NormalCursor); 
END PROCEDURE; {ShowAnalView}

PROCEDURE AddWindow(IN newId : INTEGER);
VAR
   tempBlock          : RBDBlockObj;
   tempNode           : RBDNodeObj;
   tempHier           : RBDHierObj;
BEGIN
   IF (NOT importing)
      NEW(node);   {In Node}
      ASK nodeGroup TO Add(node);
      INC(totalNodes);
      INC(totalObjects);
      ASK node TO LoadFromLibrary (images, "RBDInNode");
      ASK node TO SetID("RBDNode", nextId);
      ASK root TO AddAfterGraphic(defaultBlock, node);
      ASK node TO SetHidden(TRUE);
      ASK node TO DisplayAt(1., 73.);
      ASK node TO SetName("in");
      ASK node TO SetType(4);
      ASK node TO SetNum(node.Id);
      ASK node TO SetParentID(newId);
      ASK node TO SetGoodPaths(1);
      ASK node TO SetFullFlow(TRUE);
      ASK node TO SetAnyPath(TRUE);
      hier := ASK root Child("RBDHier",newId);
      ASK hier TO SetInID(node.Id);
      ASK hier.childGroup TO Add(node);
                 
      NEW(node);   {Out Node}
      ASK nodeGroup TO Add(node);
      INC(totalNodes);
      INC(totalObjects);
      ASK node TO LoadFromLibrary (images, "RBDOutNode");
      ASK node TO SetID("RBDNode", nextId);
      ASK root TO AddAfterGraphic(defaultBlock, node);
      ASK node TO SetHidden(TRUE);
      ASK node TO DisplayAt(22., 73.);
      ASK node TO SetName("out");
      ASK node TO SetType(5);
      ASK node TO SetNum(node.Id);
      ASK node TO SetParentID(newId);
      ASK node TO SetGoodPaths(1);
      ASK node TO SetFullFlow(TRUE);
      ASK node TO SetAnyPath(TRUE);
      ASK hier TO SetOutID(node.Id);
      ASK hier.childGroup TO Add(node);
   END IF;
   node := ASK root Child("RBDNode", hier.outID); {for importing}
   IF hier.usesPhasing {hier.usesPhasing set only so can set its outnode}
      node.SetusesPhasing(TRUE);
      ASK node TO SetPhases(TRUE,NILARRAY);
   END IF;
   ASK node TO SetDep(hier.DependencyNum, hier.depType); {hier.DependencyNum set only so can set its outnode}
END PROCEDURE {AddWindow};


PROCEDURE ChangeWindow(IN newWindow, newLevel : INTEGER);
VAR
   tempBlock : RBDBlockObj;
   tempEvent : RBDEventObj;
   tempNode  : RBDNodeObj;
   tempHier, tempHier2, newHier  : RBDHierObj;
   obj : ANYOBJ;
   tempLink, tempLink2, tempLinkS, tempLinkPF, tempLinkPT  : LinkObj;
   j, oldWindow, test : INTEGER;
   skip, foundLink : BOOLEAN;
   label, AoText : TextObj;
   tempImage, innerSquare : ImageObj;
   oldId : INTEGER;
   tempZoom, tempXOrig, tempYOrig, graphZoom : REAL;
   element : ImageObj;
BEGIN
   FOREACH obj IN selectGroup
      IF OBJTYPENAME(obj) = "RBDNodeObj"
         tempNode := RBDNodeObj(obj);
         IF tempNode.typeNode <> 2
            NEW(message, 1..1);
            IF tempNode.typeNode = 1
               message[1] := "Cannot move start marker into hierarchy    ";
            ELSIF tempNode.typeNode = 3
               message[1] := "Cannot move end marker into hierarchy     ";
            ELSIF tempNode.typeNode = 4
               message[1] := "Cannot move in marker into hierarchy     ";
            ELSIF tempNode.typeNode = 5
               message[1] := "Cannot move out marker into hierarchy     ";
            END IF;
            result := SendAlert(message, FALSE, FALSE, TRUE);
            RETURN;
         END IF;
      END IF;
   END FOREACH; 
   ASK window TO SetSysCursor(BusyCursor);
   IF (newLevel = levelLimit)
      {Disable add new hier}
      ASK menubar TO Disable(18); 
   ELSIF (newLevel < levelLimit)
      {Enable add new hier}
   IF ((NOT draggingStuff) AND (NOT draggingblock) AND (NOT draggingnode) AND (NOT draggingevent) AND (NOT dragginghier))
      ASK menubar TO Enable(18); 
      END IF;
   END IF;
   IF (newLevel = 0)
      {Disable navigation tools}
      ASK menubar TO Disable(19);
   ELSIF (newLevel > 0)
      {Enable navigation tools}
   IF ((NOT draggingStuff) AND (NOT draggingblock) AND (NOT draggingnode) AND (NOT draggingevent) AND (NOT dragginghier))
      ASK menubar TO Enable(19);
      END IF;
   END IF;
   oldWindow := activeWindow;
   activeWindow := newWindow; 
   hierLevel := newLevel;
   IF ((NOT draggingStuff) AND (NOT draggingblock) AND (NOT draggingnode) AND (NOT draggingevent) AND (NOT dragginghier))
   IF nowSimulating
      ASK simToolBar TO SetNavigation;
      ASK simMenuBar TO SetNavigation;
   ELSIF currentView = "workspace"
      ASK menuTool TO SetNavigation;
      ASK menubar TO SetNavigation;
   ELSIF currentView = "fev"
      ASK fevToolBar TO SetNavigation;
      ASK fevMenuBar TO SetNavigation;
   ELSIF currentView = "weaklink"
      ASK weakToolBar TO SetNavigation;
      ASK weakMenuBar TO SetNavigation;
   END IF;
   END IF;
   IF hierLevel > 0
      tempHier := ASK root Child("RBDHier", newWindow);
      tempZoom := tempHier.zoom;
      tempXOrig := tempHier.xOrigin;
      tempYOrig := tempHier.yOrigin;
      IF compileType = "demo"
         ASK window TO SetTitle("RAPTOR 7.0 DEMO - " + nameOfFile + " - " + tempHier.name);
      ELSIF compileType = "student"
         ASK window TO SetTitle("RAPTOR 7.0 STUDENT - " + nameOfFile + " - " + tempHier.name);
      ELSIF compileType = "gmd"
         ASK window TO SetTitle("RAPTOR 6.5 - " + nameOfFile + " - " + tempHier.name);
      ELSE
         ASK window TO SetTitle("RAPTOR 7.0 - " + nameOfFile + " - " + tempHier.name);
      END IF;
   ELSE
      IF compileType = "demo"
         ASK window TO SetTitle("RAPTOR 7.0 DEMO - " + nameOfFile);
      ELSIF compileType = "student"
         ASK window TO SetTitle("RAPTOR 7.0 STUDENT - " + nameOfFile);
      ELSIF compileType = "gmd"
         ASK window TO SetTitle("RAPTOR 6.5 - " + nameOfFile);
      ELSE
         ASK window TO SetTitle("RAPTOR 7.0 - " + nameOfFile);
      END IF;
   END IF;
   IF (typeOfCursor = blockC)
      tempBlock := ASK window Child("RBDBlock", nextId-1);
      ASK tempBlock TO SetParentID(activeWindow);
      {If block is in a hier's childgroup, remove it}
      FOREACH newHier IN hierGroup
         IF (newHier.childGroup.Includes(tempBlock))
            ASK newHier.childGroup TO RemoveThis(tempBlock);
         END IF;
      END FOREACH;
      {If block has been moved into a hier, put it in its childgroup}
      IF activeWindow > 0
         newHier := ASK root Child("RBDHier", activeWindow);
         ASK newHier.childGroup TO Add(tempBlock);
      END IF;
   ELSIF (typeOfCursor = eventC)
      tempEvent := ASK window Child("RBDEvent", nextId-1);
      ASK tempEvent TO SetParentID(activeWindow);
      FOREACH newHier IN hierGroup
         IF (newHier.childGroup.Includes(tempEvent))
            ASK newHier.childGroup TO RemoveThis(tempEvent);
         END IF;
      END FOREACH;
      IF activeWindow > 0
         newHier := ASK root Child("RBDHier", activeWindow);
         ASK newHier.childGroup TO Add(tempEvent);
      END IF;
   ELSIF (typeOfCursor = nodeC)
      tempNode := ASK window Child("RBDNode", nextId-1);
      ASK tempNode TO SetParentID(activeWindow);
      FOREACH newHier IN hierGroup
         IF (newHier.childGroup.Includes(tempNode))
            ASK newHier.childGroup TO RemoveThis(tempNode);
         END IF;
      END FOREACH;
      IF activeWindow > 0
         newHier := ASK root Child("RBDHier", activeWindow);
         ASK newHier.childGroup TO Add(tempNode);
      END IF;
   ELSIF (typeOfCursor = hierC)
      tempHier := ASK window Child("RBDHier", nextId-1);
      ASK tempHier TO SetParentID(activeWindow);
      FOREACH newHier IN hierGroup
         IF (newHier.childGroup.Includes(tempHier))
            ASK newHier.childGroup TO RemoveThis(tempHier);
         END IF;
      END FOREACH;
      IF activeWindow > 0
         newHier := ASK root Child("RBDHier", activeWindow);
         ASK newHier.childGroup TO Add(tempHier);
      END IF;
      IF tempHier.level = deepestLevel
         deepestLevel := 0;
         FOREACH tempHier2 IN hierGroup
            IF tempHier2.level > deepestLevel
               deepestLevel := tempHier2.level
            END IF;
         END FOREACH;
      END IF;
      ASK tempHier TO SetLevel(hierLevel+1);
      IF tempHier.level > deepestLevel
         deepestLevel := tempHier.level
      END IF;
   END IF; 
 
   FOREACH tempBlock IN blockGroup
      IF NOT selectGroup.Includes(tempBlock)
         ASK tempBlock TO SetHidden(TRUE);
      END IF;
   END FOREACH;
   FOREACH tempNode IN nodeGroup
      IF NOT selectGroup.Includes(tempNode)
         ASK tempNode TO SetHidden(TRUE);
      END IF;
   END FOREACH;
   FOREACH tempEvent IN eventGroup
      IF NOT selectGroup.Includes(tempEvent)
         ASK tempEvent TO SetHidden(TRUE);
      END IF;
   END FOREACH;
   FOREACH tempHier IN hierGroup
      IF NOT selectGroup.Includes(tempHier)
         ASK tempHier TO SetHidden(TRUE);
      END IF;
   END FOREACH;
   FOREACH tempLink IN linkGroup
      IF NOT selectedLinksGroup.Includes(tempLink)
         ASK tempLink TO SetHidden(TRUE);
      END IF;
   END FOREACH;
   IF greenFace <> NILOBJ
      IF nowSimulating AND (NOT dSimWithGraph)
         ASK window TO DisplayFace(4);
      END IF;
   END IF;
   ASK window TO Draw;
   IF hierLevel > 0
      SetView(tempZoom, tempXOrig, tempYOrig);
   ELSE   
      SetView(cusZoomVal, xOrigin, yOrigin);
   END IF;   
   FOREACH tempBlock IN blockGroup
      IF ((blocksIn > 0) AND (draggingStuff OR draggingblock OR draggingevent OR draggingnode OR dragginghier))
         FOREACH current IN selectGroup;
            IF (OBJTYPENAME(current) = "RBDBlockObj") AND (current.Id = tempBlock.Id)
               skip := TRUE;
            END IF;
         END FOREACH;
         IF skip
            skip := FALSE;
         ELSE
            innerSquare := ASK tempBlock Descendant("InnerSquare", 0); 
            IF tempBlock.parentID = newWindow
               ASK tempBlock TO SetHidden(FALSE);
               IF ((currentView = "workspace") OR (tempBlock.activeStatus = Active))
                  ASK innerSquare TO SetHidden(TRUE);
               END IF;
               {ASK tempBlock TO Draw;}
            ELSE
               ASK tempBlock TO SetHidden(TRUE);
               {ASK tempBlock TO Erase;}
            END IF;
         END IF;
      ELSE
         innerSquare := ASK tempBlock Descendant("InnerSquare", 0);
         IF tempBlock.parentID = newWindow
            ASK tempBlock TO SetHidden(FALSE);
            IF ((currentView = "workspace") OR (tempBlock.activeStatus = Active))
               ASK innerSquare TO SetHidden(TRUE);
            END IF;
         ELSE
            ASK tempBlock TO SetHidden(TRUE);
         END IF;
      END IF;
      ASK tempBlock TO Draw;
   END FOREACH; 
   FOREACH tempEvent IN eventGroup
      IF ((eventsIn > 0) AND (draggingStuff OR draggingblock OR draggingevent OR draggingnode OR dragginghier))
         FOREACH current IN selectGroup;
            IF (OBJTYPENAME(current) = "RBDEventObj") AND (current.Id = tempEvent.Id)
               skip := TRUE;
            END IF;
         END FOREACH;
         IF skip
            skip := FALSE;
         ELSE
            innerSquare := ASK tempEvent Descendant("InnerSquare", 0); 
            IF tempEvent.parentID = newWindow
               ASK tempEvent TO SetHidden(FALSE);
               IF ((currentView = "workspace") OR (tempEvent.activeStatus = Active))
                  ASK innerSquare TO SetHidden(TRUE);
               END IF;
               {ASK tempEvent TO Draw;}
            ELSE
               ASK tempEvent TO SetHidden(TRUE);
               {ASK tempEvent TO Erase;}
            END IF;
         END IF;
      ELSE
         innerSquare := ASK tempEvent Descendant("InnerSquare", 0);
         IF tempEvent.parentID = newWindow
            ASK tempEvent TO SetHidden(FALSE);
            IF ((currentView = "workspace") OR (tempEvent.activeStatus = Active))
               ASK innerSquare TO SetHidden(TRUE);
            END IF;
         ELSE
            ASK tempEvent TO SetHidden(TRUE);
         END IF;
      END IF;
      ASK tempEvent TO Draw;
   END FOREACH;
   FOREACH tempHier IN hierGroup 
      IF ((hiersIn > 0) AND (draggingStuff OR draggingblock OR draggingevent OR draggingnode OR dragginghier))
         FOREACH current IN selectGroup;
            IF (OBJTYPENAME(current) = "RBDHierObj") AND (current.Id = tempHier.Id)
               skip := TRUE;
            END IF;
         END FOREACH;
         IF skip
            skip := FALSE;
         ELSE
            IF tempHier.parentID = newWindow
               ASK tempHier TO SetHidden(FALSE);
               {ASK tempHier TO Draw;}
            ELSE
               ASK tempHier TO SetHidden(TRUE);
               {ASK tempHier TO Erase;}
            END IF;
         END IF;
      ELSE
         IF tempHier.parentID = newWindow
            ASK tempHier TO SetHidden(FALSE);
         ELSE
            ASK tempHier TO SetHidden(TRUE);
         END IF;
      END IF;
      ASK tempHier TO Draw;
   END FOREACH;   
   FOREACH tempNode IN nodeGroup 
      IF ((nodesIn > 0) AND (draggingStuff OR draggingblock OR draggingevent OR draggingnode OR dragginghier))
         FOREACH current IN selectGroup;
            IF (OBJTYPENAME(current) = "RBDNodeObj") AND (current.Id = tempNode.Id)
               skip := TRUE;
            END IF;
         END FOREACH;
         IF skip
            skip := FALSE;
         ELSE 
            IF tempNode.parentID = newWindow
               ASK tempNode TO SetHidden(FALSE);
               {ASK tempNode TO Draw;}
            ELSE
               ASK tempNode TO SetHidden(TRUE);
               {ASK tempNode TO Erase;}
            END IF;
         END IF;
      ELSE
         IF tempNode.parentID = newWindow
            ASK tempNode TO SetHidden(FALSE);
         ELSE
            ASK tempNode TO SetHidden(TRUE);
         END IF;
      END IF;
      ASK tempNode TO Draw;
   END FOREACH;
 
 
   FOREACH tempLink IN linkGroup
      i := tempLink.Id;
      IF (draggingStuff OR draggingblock OR draggingevent OR draggingnode OR dragginghier)
         FOREACH tempLinkS IN selectedLinksGroup;
            IF tempLinkS.Id = i
               skip := TRUE;
            END IF;
         END FOREACH;
         IF NOT skip
            FOREACH tempLinkPF IN partialFromGroup;
               IF tempLinkPF.Id = i
                  skip := TRUE;
                  IF tempLinkPF.connectFRef = "RBDBlock"
                     {If tempLinkPF is connected from a block in the select group, find that block}
                     FOREACH current IN selectGroup;
                        IF (OBJTYPENAME(current) = "RBDBlockObj") AND (current.Id = tempLinkPF.connectFromId)
                           tempBlock := RBDBlockObj(current);
                           EXIT;
                        END IF;
                     END FOREACH;
                     {If tempLinkPF currently connects to a node, set the block it's connected from to be not
                      connected to a node (SetConnectToNode(FALSE))}
                     IF tempLinkPF.connectTRef = "RBDNode"
                        IF tempBlock.isConnectedNode
                           ASK tempBlock TO SetConnectToNode(FALSE);
                        END IF;
                     END IF;
                     ASK tempBlock TO IncLink(MINUSOUTOF);
                  ELSIF tempLinkPF.connectFRef = "RBDEvent"
                     FOREACH current IN selectGroup;
                        IF (OBJTYPENAME(current) = "RBDEventObj") AND (current.Id = tempLinkPF.connectFromId)
                           tempEvent := RBDEventObj(current);
                           EXIT;
                        END IF;
                     END FOREACH;
                     IF tempLinkPF.connectTRef = "RBDNode"
                        IF tempEvent.isConnectedNode
                           ASK tempEvent TO SetConnectToNode(FALSE);
                        END IF;
                     END IF;
                     ASK tempEvent TO IncLink(MINUSOUTOF);
                  ELSIF tempLinkPF.connectFRef = "RBDHier"
                     FOREACH current IN selectGroup;
                        IF (OBJTYPENAME(current) = "RBDHierObj") AND (current.Id = tempLinkPF.connectFromId)
                           tempHier := RBDHierObj(current);
                           EXIT;
                        END IF;
                     END FOREACH;
                     IF tempLinkPF.connectTRef = "RBDNode"
                        IF tempHier.isConnectedNode
                           ASK tempHier TO SetConnectToNode(FALSE);
                        END IF;
                     END IF;
                     ASK tempHier TO IncLink(MINUSOUTOF);
                  ELSIF tempLinkPF.connectFRef = "RBDNode"
                     FOREACH current IN selectGroup;
                        IF (OBJTYPENAME(current) = "RBDNodeObj") AND (current.Id = tempLinkPF.connectFromId)
                           tempNode := RBDNodeObj(current);
                           EXIT;
                        END IF;
                     END FOREACH;
                     ASK tempNode TO IncLink(MINUSOUTOF);
                  END IF;
                  IF tempLinkPF.connectTRef = "RBDBlock"
                     tempBlock := ASK root Child("RBDBlock", tempLinkPF.connectToId);
                     ASK tempBlock TO IncLink(MINUSINTO);
                  ELSIF tempLinkPF.connectTRef = "RBDEvent"
                     tempEvent := ASK root Child("RBDEvent", tempLinkPF.connectToId);
                     ASK tempEvent TO IncLink(MINUSINTO);
                  ELSIF tempLinkPF.connectTRef = "RBDHier"
                     tempHier := ASK root Child("RBDHier", tempLinkPF.connectToId);
                     ASK tempHier TO IncLink(MINUSINTO);
                  ELSIF tempLinkPF.connectTRef = "RBDNode"
                     tempNode := ASK root Child("RBDNode", tempLinkPF.connectToId);
                     ASK tempNode TO IncLink(MINUSINTO);
                     IF tempNode.connectIntoNum = 0
                        ASK tempNode TO SetGoodPaths(0);
                     ELSE
                        ASK tempNode TO SetGoodPaths(1);
                     END IF;
                     label := ASK tempNode Child("RBDNodeKofN", 0);
                     ASK label TO SetText("");
                     ASK tempNode TO SetAoDoR("0", "0", "0");
                     {ASK tempNode TO Draw;}
                  END IF;
                  oldId := tempLinkPF.Id;
                  ASK partialFromGroup TO RemoveThis(tempLinkPF); 
                  IF tempLinkPF.parentID > 0
                     tempHier := ASK root Child("RBDHier", tempLinkPF.parentID);
                     ASK tempHier.childGroup TO RemoveThis(tempLinkPF);
                  END IF;
                  ASK linkGroup TO RemoveThis(tempLinkPF);
                  DISPOSE(tempLinkPF);
                  totalLinks := totalLinks - 1;
                  linkSelected := FALSE;            
               END IF;
            END FOREACH;
         END IF;
         IF NOT skip
            FOREACH tempLinkPT IN partialToGroup;
               IF tempLinkPT.Id = i
                  skip := TRUE;
                  IF tempLinkPT.connectFRef = "RBDBlock"
                     tempBlock := ASK root Child("RBDBlock", tempLinkPT.connectFromId);
                     IF tempLinkPT.connectTRef = "RBDNode"
                        IF tempBlock.isConnectedNode
                           ASK tempBlock TO SetConnectToNode(FALSE);
                        END IF;
                     END IF;
                     ASK tempBlock TO IncLink(MINUSOUTOF);
                  ELSIF tempLinkPT.connectFRef = "RBDEvent"
                     tempEvent := ASK root Child("RBDEvent", tempLinkPT.connectFromId);
                     IF tempLinkPT.connectTRef = "RBDNode"
                        IF tempEvent.isConnectedNode
                           ASK tempEvent TO SetConnectToNode(FALSE);
                        END IF;
                     END IF;
                     ASK tempEvent TO IncLink(MINUSOUTOF);
                  ELSIF tempLinkPT.connectFRef = "RBDHier"
                     tempHier := ASK root Child("RBDHier", tempLinkPT.connectFromId);
                     IF tempLinkPT.connectTRef = "RBDNode"
                        IF tempHier.isConnectedNode
                           ASK tempHier TO SetConnectToNode(FALSE);
                        END IF;
                     END IF;
                     ASK tempHier TO IncLink(MINUSOUTOF);
                  ELSIF tempLinkPT.connectFRef = "RBDNode"
                     tempNode := ASK root Child("RBDNode", tempLinkPT.connectFromId);
                     ASK tempNode TO IncLink(MINUSOUTOF);
                  END IF;
                  IF tempLinkPT.connectTRef = "RBDBlock"
                     FOREACH current IN selectGroup;
                        IF (OBJTYPENAME(current) = "RBDBlockObj") AND (current.Id = tempLinkPT.connectToId)
                           tempBlock := RBDBlockObj(current);
                           EXIT;
                        END IF;
                     END FOREACH;
                     ASK tempBlock TO IncLink(MINUSINTO);
                  ELSIF tempLinkPT.connectTRef = "RBDEvent"
                     FOREACH current IN selectGroup;
                        IF (OBJTYPENAME(current) = "RBDEventObj") AND (current.Id = tempLinkPT.connectToId)
                           tempEvent := RBDEventObj(current);
                           EXIT;
                        END IF;
                     END FOREACH;
                     ASK tempEvent TO IncLink(MINUSINTO);
                  ELSIF tempLinkPT.connectTRef = "RBDHier"
                     FOREACH current IN selectGroup;
                        IF (OBJTYPENAME(current) = "RBDHierObj") AND (current.Id = tempLinkPT.connectToId)
                           tempHier := RBDHierObj(current);
                           EXIT;
                        END IF;
                     END FOREACH;
                     ASK tempHier TO IncLink(MINUSINTO);
                  ELSIF tempLinkPT.connectTRef = "RBDNode"
                     FOREACH current IN selectGroup;
                        IF (OBJTYPENAME(current) = "RBDNodeObj") AND (current.Id = tempLinkPT.connectToId)
                           tempNode := RBDNodeObj(current);
                           EXIT;
                        END IF;
                     END FOREACH;
                     ASK tempNode TO IncLink(MINUSINTO);
                     IF tempNode.connectIntoNum = 0
                        ASK tempNode TO SetGoodPaths(0);
                     ELSE
                        ASK tempNode TO SetGoodPaths(1);
                     END IF;
                     label := ASK tempNode Child("RBDNodeKofN", 0);
                     ASK label TO SetText("");
                     ASK tempNode TO SetAoDoR("0", "0", "0");
                    { ASK tempNode TO Draw;}
                  END IF;
                  oldId := tempLinkPT.Id;
                  ASK partialToGroup TO RemoveThis(tempLinkPT);    
                  IF tempLinkPT.parentID > 0
                     tempHier := ASK root Child("RBDHier", tempLinkPT.parentID);
                     ASK tempHier.childGroup TO RemoveThis(tempLinkPT);
                  END IF;
                  ASK linkGroup TO RemoveThis(tempLinkPT);
                  DISPOSE(tempLinkPT); {******}
                  totalLinks := totalLinks - 1;
                  linkSelected := FALSE;            
               END IF;
            END FOREACH;
         END IF;
         IF skip
            skip := FALSE;
         ELSE
            INC(i);
            IF tempLink.parentID = newWindow
               ASK tempLink TO SetHidden(FALSE);
               {ASK tempLink TO Draw;}
            ELSE
               ASK tempLink TO SetHidden(TRUE);
               {ASK tempLink TO Erase;}
            END IF;
         END IF;
      ELSE
         INC(i);
         IF tempLink.parentID = newWindow
            ASK tempLink TO SetHidden(FALSE);
         ELSE
            ASK tempLink TO SetHidden(TRUE);
         END IF;
      END IF;
   END FOREACH; 

   IF hierLevel > 0   {zoomissue - aograph (ChangeWindow)}
      IF DisplayAoGraph
         SetGraphZoom(graphZoom, simZoomX, simZoomY);
         IF graphZoom > tempZoom
            SetView(graphZoom, simZoomX, simZoomY);
         ELSE
            SetView(tempZoom, simZoomX, simZoomY);
         END IF;
      ELSE
         SetView(tempZoom, tempXOrig, tempYOrig);
      END IF;
   ELSE   
      IF DisplayAoGraph
         SetGraphZoom(graphZoom, simZoomX, simZoomY);
         IF graphZoom > cusZoomVal
            SetView(graphZoom, simZoomX, simZoomY);
         ELSE
            SetView(cusZoomVal, simZoomX, simZoomY);
         END IF;
      ELSE
         SetView(cusZoomVal, xOrigin, yOrigin);
      END IF;
   END IF;
   ASK window TO SetScroll;
   ASK window TO SetSysCursor(NormalCursor);
   ASK window TO Update;
   
   
   
END PROCEDURE {ChangeWindow};


OBJECT GridObj;
   ASK METHOD ObjInit;
   VAR
      xMin,yMin,xMax,yMax  : REAL;
      line                 : PolylineObj;
      point1, point2       : PointType;
      coords               : PointArrayType;                        
   BEGIN
      INHERITED ObjInit;
      Id := 666;
      ReferenceName := "Grid";
      NEW(internal);
      NEW(border);
      xMin := 0.;
      yMin := 0.;
      xMax := 120.;
      yMax := 80.;
      NEW(coords, 1..2);
      FOR i := 0 TO 201  {there are 202 lines in the grid, 121 wide by 81 high}
         NEW(line);
         IF i < 121  {for the 121 vertical lines}
            IF i=0
               ASK line TO SetWidth(0.14); {left border}
               ASK border TO AddGraphic(line);
            ELSIF i=120
               ASK line TO SetWidth(0.10);  {right border}
               ASK border TO AddGraphic(line);
            ELSE
               ASK line TO SetWidth(0.02); 
               ASK internal TO AddGraphic(line);
            END IF;
            point1.x := xMin + FLOAT(i);
            point1.y := yMax;
            point2.x := xMin + FLOAT(i);
            point2.y := yMin;
         ELSIF i >= 121  {the 81 horizontal lines}
            IF i=121
               ASK line TO SetWidth(0.06);       {top border}
               ASK border TO AddGraphic(line);
            ELSIF i=201
               ASK line TO SetWidth(0.08);       {bottom border}
               ASK border TO AddGraphic(line);
            ELSE
               ASK line TO SetWidth(0.02); 
               ASK internal TO AddGraphic(line);
            END IF;
            point1.x := xMax;
            point1.y := yMax - (FLOAT(i - 121));
            point2.x := xMin;
            point2.y := yMax - (FLOAT(i - 121));
         END IF;
         coords[1] := point1;
         coords[2] := point2;
         ASK line TO SetPoints(coords);
      END FOR;
      DISPOSE(coords);
      AddChild(internal,"internal",0);
      ASK internal TO SetTranslation(0.,.1);
      ASK border TO SetColor(LightGrey);
      AddChild(border,"border",0);
      ASK border TO SetTranslation(0.,.1);
   END METHOD; {ObjInit}
   
   ASK METHOD Colorize(IN case : STRING);
   VAR
      link                : LinkObj;
      tempNode            : RBDNodeObj;
      tempBlock           : RBDBlockObj;
      tempEvent           : RBDEventObj;
      tempHier            : RBDHierObj;
      text1, text2, text3 : TextObj;
   BEGIN
      internal := Child("internal",0);
      IF (case = "Sim") OR (case = "NodeAnal")
         {FOREACH link IN linkGroup
            ASK link TO SetColor(Black);
         END FOREACH;
         FOREACH tempNode IN nodeGroup
            text1 := ASK tempNode Child("RBDNodeNum", 0);
            text2 := ASK tempNode Child("RBDNodeKofN", 0);
            ASK text1 TO SetColor(Black);
            ASK text2 TO SetColor(Black);
         END FOREACH; 
         FOREACH tempBlock IN blockGroup
            text3 :=  ASK tempBlock Child("RBDBlockLabel", 0);
            ASK text3 TO SetColor(Black);
         END FOREACH;
         FOREACH tempEvent IN eventGroup
            text3 :=  ASK tempEvent Child("RBDEventLabel", 0);
            ASK text3 TO SetColor(Black);
         END FOREACH;
         FOREACH tempHier IN hierGroup
            text3 :=  ASK tempHier Child("HierLabel", 0);
            ASK text3 TO SetColor(Black);
         END FOREACH;}
         IF case = "Sim"
            ASK internal TO SetColor(simColor);
            ASK window TO SetColor(simColor);
         ELSE
            ASK internal TO SetColor(goggleColor);
            ASK window TO SetColor(goggleColor);
         END IF;
        { ASK window TO Update;}
      ELSIF case = "UnSim"
         FOREACH link IN linkGroup
            ASK link TO SetColor(linkWorkColor);
         END FOREACH;
         FOREACH tempNode IN nodeGroup
            text1 := ASK tempNode Child("RBDNodeNum", 0);
            text2 := ASK tempNode Child("RBDNodeKofN", 0);
            ASK text1 TO SetColor(textWorkColor);
            ASK text2 TO SetColor(textWorkColor);
         END FOREACH; 
         FOREACH tempBlock IN blockGroup
            text3 :=  ASK tempBlock Child("RBDBlockLabel", 0);
            ASK text3 TO SetColor(textWorkColor);
         END FOREACH;
         FOREACH tempEvent IN eventGroup
            text3 :=  ASK tempEvent Child("RBDEventLabel", 0);
            ASK text3 TO SetColor(textWorkColor);
         END FOREACH;
         FOREACH tempHier IN hierGroup
            text3 :=  ASK tempHier Child("HierLabel", 0);
            ASK text3 TO SetColor(textWorkColor);
         END FOREACH;
         ASK internal TO SetColor(gridColor);
         ASK window TO SetColor(workColor);
      ELSIF (case = "FEV")
        { FOREACH link IN linkGroup
            ASK link TO SetColor(Black);
         END FOREACH;
         FOREACH tempNode IN nodeGroup
            text1 := ASK tempNode Child("RBDNodeNum", 0);
            text2 := ASK tempNode Child("RBDNodeKofN", 0);
            ASK text1 TO SetColor(Black);
            ASK text2 TO SetColor(Black);
         END FOREACH; 
         FOREACH tempBlock IN blockGroup
            text3 :=  ASK tempBlock Child("RBDBlockLabel", 0);
            ASK text3 TO SetColor(Black);
         END FOREACH;
         FOREACH tempEvent IN eventGroup
            text3 :=  ASK tempEvent Child("RBDEventLabel", 0);
            ASK text3 TO SetColor(Black);
         END FOREACH;
         FOREACH tempHier IN hierGroup
            text3 :=  ASK tempHier Child("HierLabel", 0);
            ASK text3 TO SetColor(Black);
         END FOREACH; }
         ASK internal TO SetColor(simColor);
         ASK window TO SetColor(simColor);
      ELSIF case = "Hide"
         IF window.Color <> workColor
            ASK window TO SetColor(workColor);
            ASK window TO Update;
         END IF;
         ASK internal TO SetColor(workColor);
         ASK internal TO Draw;
      ELSE
         ASK internal TO SetColor(gridColor);
         IF NOT nowInitialing
            ASK internal TO Draw;
         END IF;
      END IF;
   END METHOD;
END OBJECT; {GridObj}

OBJECT safeSoundObj;
   ASK METHOD PlayMe(IN fileName : STRING);    
   VAR 
      cmdLine       : STRING;
      done,a        : INTEGER;
   BEGIN
      CheckBGTask(done,a,a);
      IF done<>0
         IF soundPath <> ""
            cmdLine := soundPath + "sndrec32.exe /play /close ";   
            pID := StartBGTask(cmdLine+fileName,1);
         END IF;
      END IF;
   END METHOD;
END OBJECT {safeSoundObj};

OBJECT mainWindowObj;
   ASK METHOD BeClosed;
   VAR
      saveCancelled : BOOLEAN;
      kill, i          : INTEGER;
      child         : ANYOBJ;
      
   BEGIN
      IF popupMenu.Visible
         KillPopup;
      END IF;
      IF inStepMode
         {do nothing}
      ELSIF nowSimulating
         hitXinSim := TRUE;
         ASK simMenuBar TO KillSim;
      ELSIF FEVmode
         EndFailureEffects;
      ELSIF currentView = "weaklink"  
         ReturnToWorkspace;
      ELSE
         FOREACH hier IN hierGroup
            FOREACH child IN hier.childGroup
               ASK hier.childGroup TO RemoveThis(child);
            END FOREACH;
         END FOREACH;
         IF typeOfCursor = blockC
            SetCursor(NILOBJ);
            ASK blockGroup TO RemoveThis(block);
            DISPOSE(block);
            totalBlocks := totalBlocks - 1;
            totalObjects := totalObjects - 1;
         ELSIF typeOfCursor = eventC
            SetCursor(NILOBJ);
            ASK eventGroup TO RemoveThis(event);
            DISPOSE(event);
            totalEvents := totalEvents - 1;
            totalObjects := totalObjects - 1;
         ELSIF typeOfCursor = nodeC
            SetCursor(NILOBJ);  
            ASK nodeGroup TO RemoveThis(node);
            DISPOSE(node);
            totalNodes := totalNodes - 1;
            totalObjects := totalObjects - 1;
         ELSIF typeOfCursor = hierC
            SetCursor(NILOBJ);
            ASK hierGroup TO RemoveThis(hier);
            DISPOSE(hier);
            totalHiers := totalHiers - 1;
            totalObjects := totalObjects - 1;
         ELSIF typeOfCursor = connectC
            IF draggingblock
               block := ASK root Child("RBDBlock", fromBlockId);
               ASK block TO IncLink(MINUSOUTOF);
            ELSIF draggingevent
               event := ASK root Child("RBDEvent", fromBlockId);
               ASK event TO IncLink(MINUSOUTOF);
            ELSIF draggingnode
               node := ASK root Child("RBDNode", fromBlockId);
               ASK node TO IncLink(MINUSOUTOF);
            ELSIF dragginghier
               hier := ASK root Child("RBDHier", fromBlockId);
               ASK hier TO IncLink(MINUSOUTOF); 
            END IF;
            SetCursor(NILOBJ);
            DISPOSE(linkCursor);
            ASK linkGroup TO RemoveThis(link);
            DISPOSE(link);
            totalLinks := totalLinks - 1;
            typeOfCursor := nilC;
            linkText := Descendant("LinkText",851);
            DISPOSE(linkText);
            DISPOSE(linkMessage);
            linkMsgExists := FALSE;
            draggingblock := FALSE;
            draggingevent := FALSE;
            draggingnode  := FALSE;
            dragginghier := FALSE;
         ELSIF (typeOfCursor = simC)
            FOREACH link IN linkGroup 
               ASK link TO CleanUp;
            END FOREACH;
            IF (NOT analUp)
               DISPOSE(greenFace);
               DISPOSE(yellowFace);
               DISPOSE(redFace);
            END IF;
         END IF;
         nowInitialing := TRUE;
         ClearAllBlocks;
         ASK menubar TO Disable1Thru8;
         ASK menubar TO Disable(9);
         typeOfCursor := dialogC;
         menuPath := pathName;
         menuFile := nameOfFile;
         openMenuFile := FALSE;
         CloseFFile(gridIsOn, fileIsOpen, saveCancelled, nameOfFile, pathName, filter,
                    totalBlocks, totalNodes, totalLinks, totalHiers, totalEvents);
         IF NOT saveCancelled
            DISPOSE(popupMenu);
            DISPOSE(root);
            DISPOSE(menubar);
            DISPOSE(menuTool);
            DISPOSE(simMenuBar);
            DISPOSE(simToolBar);
            DISPOSE(dialogs);
            DISPOSE(images);
            IF FileExists(exampPath + "cksrule.rbd");
               kill := DeleteFile(exampPath + "cksrule.rbd");
            END IF;
            INHERITED BeClosed;
         ELSE
            ASK menubar TO Enable2Thru6;
            ASK menubar TO Enable(9);
            nowInitialing := FALSE;
            typeOfCursor := nilC;
         END IF;
      END IF;
   END METHOD; {BeClosed}

   ASK METHOD MouseClick(IN x, y : REAL;
                         IN buttondown : BOOLEAN);
   VAR
      errorText               : STRING;
      newId, tempaw                           : INTEGER;
      displayX, displayY, tempX, tempY,
      varSwitch, winX, winY, pointX, pointY, 
      newx, newy, offsetX, offsetY, localZoomVal               : REAL;
      cancelled, validLink, badLink, fromSelected,
      toSelected, messageUp, startToEnd, 
      somethingChangedSave, outClick, changeView : BOOLEAN;
      selectedObj                                : ImageObj;
      linkPoints                                 : PointArrayType;
      label                                      : TextObj;
      detailsDialog                              : BlockPropObj;
      eventsDialog                               : EventsBoxObj;
      hierBox                                    : HierBoxObj;
      tempLink                                   : LinkObj;
      nodeBox                                    : NodeBoxObj;
      tempBlock,fevBlock                         : RBDBlockObj;
      tempNode, outNode                                   : RBDNodeObj;
      tempHier, tempHier2, oldHier, newHier, zoomHier                 : RBDHierObj;
      linkInts                                   : intArray;
      linkReals                                  : realArray;
      buttItem                                   : PaletteButtonObj;
      cancelButt                                 : ButtonObj;
   BEGIN
      IF ignoreMouse {eag error 51 fix}
         RETURN;
      END IF;
      IF popupMenu.Visible
         KillPopup;
      END IF;    
      IF (compileType <> "demo") AND saveIsOn AND ((TRUNC(ClockRealSecs)-lastSave) >= saveInc) AND (totalBlocks > 1) 
         AND (typeOfCursor = nilC) AND (NOT pastingMultiple)
         lastSave := TRUNC(ClockRealSecs);
         typeOfCursor := autoC;
         ShowStatus(0,"Autosaving...");
         somethingChangedSave := somethingChanged;
         rapVersion := 7;
         SaveFile("cksrule.rbd", exampPath, "*.rbd", totalBlocks, totalNodes, totalLinks, totalHiers, totalEvents);
         somethingChanged := somethingChangedSave;
         typeOfCursor := nilC;
         ShowStatus(0,"");
      END IF; 
      winX := x; winY := y;
      Transform(baseroot, root, x, y, x, y);
      IF activeWindow > 0
         zoomHier := ASK root Child("RBDHier", activeWindow);
         localZoomVal := zoomHier.zoom;
      ELSE
         localZoomVal := cusZoomVal;
      END IF;
      CASE typeOfCursor
         WHEN nilC:
            IF Button = 0
               IF buttondown
                  selectedObj := ASK root TO Select(x,y);
                  IF (selectedObj <> NILOBJ) AND (NOT nowSimulating) 
                     IF (draggingStuff OR draggingblock OR draggingevent OR draggingnode OR dragginghier)
                     ELSIF drawingBox
                     ELSIF workingPaste
                     ELSIF (OBJTYPENAME(selectedObj)="GridObj") 
                        ClearAllBlocks;
                        NEW(rubberBox);
                        ASK rubberBox TO SetWidth(2.0);
                        ASK rubberBox TO SetColor(MediumAquamarine); {FIX}
                        SetCursor(rubberBox);
                        Transform(NILOBJ, baseroot, ClickX, ClickY, boxStartX, boxStartY);
                        ASK rubberBox TO SetAnchor(boxStartX,boxStartY); 
                        Transform(NILOBJ, root, ClickX, ClickY, boxStartX, boxStartY);
                        ASK menubar TO Disable1Thru8;
                        drawingBox := TRUE;
                        drawingRed := TRUE;
                        scrollScreen := TRUE; 
                     ELSIF (ASK selectGroup Includes(selectedObj)) AND (selectGroup.numberIn > 1) AND (NOT CtrlClick)
                        oldClickX := ClickX; {and} oldClickY := ClickY;
                        Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                        NEW(selected);
                        AddGraphic(selected);
                        current := ASK selectGroup First();
                        FOR i := 1 TO selectGroup.numberIn
                           ASK root TO RemoveThisGraphic(current);
                           Transform(root,baseroot,current.xPosition,current.yPosition,newx,newy);
                           ASK current TO SetScaling(scale,scale);
                           ASK current TO SetTranslation(newx,newy);
                           ASK selected TO AddGraphic(current);
                           current := ASK selectGroup Next(current);
                        END FOR;
                        IF linksIn > 0
                           FOREACH link IN selectedLinksGroup
                              ASK link TO SetHighlightColor(Blue);
                              ASK link TO SetHighlighted(TRUE);
                              ASK root TO RemoveThisGraphic(link);
                              ASK link TO ChangeRoots(TRUE, baseroot);
                              ASK selected TO AddGraphic(link);
                           END FOREACH;
                        END IF;      
                        FOREACH link IN partialFromGroup
                           ASK link TO SetHidden(TRUE);
                           ASK link TO Draw;
                        END FOREACH;
                        FOREACH link IN partialToGroup
                           ASK link TO SetHidden(TRUE);
                           ASK link TO Draw;
                        END FOREACH;
                        ASK selected TO Draw;
                        ASK selected TO GetBoundingBox(refX,refY,tempX,tempY);
                        Transform(NILOBJ,root,refX,refY,refX,refY);
                        SetCursorOffset(-1.*displayX,-1.*displayY); 
                        SetCursor(selected);
                        ASK menubar TO Disable1Thru8;
                        scrollScreen := TRUE; 
                        draggingStuff := TRUE;
                     ELSIF (ASK selectGroup Includes(selectedObj)) AND CtrlClick
                        IF OBJTYPENAME(selectedObj) = "RBDBlockObj" {control click on block}
                           block := RBDBlockObj(selectedObj);
                           ASK block TO SetSelected(FALSE);
                           ASK block TO SetHighlighted(FALSE);
                           ASK block TO Draw;
                           ASK selectGroup TO RemoveThis(block);               
                           blueObjRef := "RBDBlock";
                           blueObjId := block.Id;
                        ELSIF OBJTYPENAME(selectedObj) = "RBDEventObj" {control click on event}
                           event := RBDEventObj(selectedObj);
                           ASK event TO SetSelected(FALSE);
                           ASK event TO SetHighlighted(FALSE);
                           ASK event TO Draw;
                           ASK selectGroup TO RemoveThis(event);               
                           blueObjRef := "RBDEvent";
                           blueObjId := event.Id;
                        ELSIF OBJTYPENAME(selectedObj) = "RBDHierObj" {control click on hier}
                           hier := RBDHierObj(selectedObj);
                           ASK hier TO SetSelected(FALSE);
                           ASK hier TO SetHighlighted(FALSE);
                           ASK hier TO Draw;
                           ASK selectGroup TO RemoveThis(hier);               
                           blueObjRef := "RBDHier";
                           blueObjId := hier.Id;
                        ELSIF OBJTYPENAME(selectedObj) = "RBDNodeObj" {control click on node}
                           node := RBDNodeObj(selectedObj);
                           ASK node TO SetSelected(FALSE);
                           ASK node TO SetHighlighted(FALSE);
                           ASK node TO Draw;
                           ASK selectGroup TO RemoveThis(node);               
                           blueObjRef := "RBDNode";
                           blueObjId := node.Id;
                        ELSIF OBJTYPENAME(selectedObj) = "LinkObj" {control click on link}
                           RETURN;
                        END IF;
                        justCtrled := TRUE;
                        HandleLinks(FALSE);
                     ELSIF (OBJTYPENAME(selectedObj) = "RBDBlockObj") {single click on block}
                        IF (NOT CtrlClick) OR linkSelected  
                           ClearAllBlocks;
                        END IF;
                        block := RBDBlockObj(selectedObj);
                        IF block.parentID = activeWindow
                           Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                           SelectBlock;
                           HandleLinks(TRUE);
                           IF NOT CtrlClick
                              oldX := block.Translation.x;
                              oldY := block.Translation.y;
                              ASK root TO RemoveThisGraphic(block);
                              Transform (root, baseroot, oldX, oldY, newx, newy) ;
                              ASK block TO SetTranslation (newx, newy) ;
                              AddGraphic(block);
                              ASK block TO SetScaling(scale,scale);
                              ASK block TO Draw ;
                              curOffX := newx-displayX;
                              curOffY := newy-displayY;
                              SetCursorOffset(curOffX, curOffY);
                              SetCursor(block);
                              IF NOT SecondClick
                                 FOREACH link IN partialFromGroup
                                    ASK link TO SetHidden(TRUE);
                                    ASK link TO Draw;
                                 END FOREACH;
                                 FOREACH link IN partialToGroup
                                    ASK link TO SetHidden(TRUE);
                                    ASK link TO Draw;
                                 END FOREACH;
                              END IF;
                              draggingblock := TRUE;
                              scrollScreen := TRUE; 
                           ELSE
                              justCtrled := TRUE;
                           END IF;
                           ASK menubar TO Disable1Thru8;
                        END IF;
                     ELSIF (OBJTYPENAME(selectedObj) = "RBDEventObj") {single click on event}
                        IF (NOT CtrlClick) OR linkSelected  
                           ClearAllBlocks;
                        END IF;
                        event := RBDEventObj(selectedObj);
                        IF event.parentID = activeWindow
                           Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                           SelectEvent;
                           HandleLinks(TRUE);
                           IF NOT CtrlClick
                              oldX := event.Translation.x;
                              oldY := event.Translation.y;
                              ASK root TO RemoveThisGraphic(event);
                              Transform (root, baseroot, oldX, oldY, newx, newy) ;
                              ASK event TO SetTranslation (newx, newy) ;
                              AddGraphic(event);
                              ASK event TO SetScaling(scale,scale);
                              ASK event TO Draw ;
                              curOffX := newx-displayX;
                              curOffY := newy-displayY;
                              SetCursorOffset(curOffX, curOffY);
                              SetCursor(event);
                              IF NOT SecondClick
                                 FOREACH link IN partialFromGroup
                                    ASK link TO SetHidden(TRUE);
                                    ASK link TO Draw;
                                 END FOREACH;
                                 FOREACH link IN partialToGroup
                                    ASK link TO SetHidden(TRUE);
                                    ASK link TO Draw;
                                 END FOREACH;
                              END IF;
                              draggingevent := TRUE;
                              scrollScreen := TRUE; 
                           ELSE
                              justCtrled := TRUE;
                           END IF;
                           ASK menubar TO Disable1Thru8;
                        END IF;
                     ELSIF (OBJTYPENAME(selectedObj) = "RBDHierObj") {single click on hier}
                        IF (NOT CtrlClick) OR linkSelected  
                           ClearAllBlocks;
                        END IF;
                        hier := RBDHierObj(selectedObj);
                        IF hier.parentID = activeWindow
                           Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                           SelectHier;
                           HandleLinks(TRUE);
                           IF NOT CtrlClick
                              oldX := hier.Translation.x;
                              oldY := hier.Translation.y;
                              ASK root TO RemoveThisGraphic(hier);
                              Transform (root, baseroot, oldX, oldY, newx, newy) ;
                              ASK hier TO SetTranslation (newx, newy) ;
                              AddGraphic(hier);
                              ASK hier TO SetScaling(scale,scale);
                              ASK hier TO Draw ;
                              curOffX := newx-displayX;
                              curOffY := newy-displayY;
                              SetCursorOffset(curOffX, curOffY);
                              SetCursor(hier);
                              IF NOT SecondClick
                                 FOREACH link IN partialFromGroup
                                    ASK link TO SetHidden(TRUE);
                                    ASK link TO Draw;
                                 END FOREACH;
                                 FOREACH link IN partialToGroup
                                    ASK link TO SetHidden(TRUE);
                                    ASK link TO Draw;
                                 END FOREACH;
                              END IF;
                              dragginghier := TRUE;
                              scrollScreen := TRUE; 
                           ELSE
                              justCtrled := TRUE;
                           END IF;
                           ASK menubar TO Disable1Thru8;
                           ASK menubar TO Enable(14);
                        END IF;
                     ELSIF (OBJTYPENAME(selectedObj) = "RBDNodeObj") {single click on node}
                        IF (NOT CtrlClick) OR linkSelected
                           ClearAllBlocks;
                        END IF;
                        node := RBDNodeObj(selectedObj);
                        IF node.parentID = activeWindow
                           Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                           SelectNode;
                           IF node.typeNode = 2
                              HandleLinks(TRUE);
                           END IF;
                           IF NOT CtrlClick
                              oldX := node.Translation.x;
                              oldY := node.Translation.y;
                              ASK root TO RemoveThisGraphic(node);
                              Transform (root, baseroot, oldX, oldY, newx, newy) ;
                              ASK node TO SetTranslation (newx, newy) ;   
                              AddGraphic(node);
                              ASK node TO SetScaling(scale,scale);
                              ASK node TO Draw;     
                              curOffX := newx-displayX;
                              curOffY := newy-displayY;
                              SetCursorOffset(curOffX, curOffY);
                              SetCursor(node);
                              IF NOT SecondClick
                                 FOREACH link IN partialFromGroup
                                    ASK link TO SetHidden(TRUE);
                                    ASK link TO Draw;
                                 END FOREACH;
                                 FOREACH link IN partialToGroup
                                    ASK link TO SetHidden(TRUE);
                                    ASK link TO Draw;
                                 END FOREACH;
                              END IF;
                              draggingnode := TRUE;
                              scrollScreen := TRUE; 
                           ELSE
                              justCtrled := TRUE;
                           END IF;
                           ASK menubar TO Disable1Thru8; 
                        END IF;
                     ELSIF (OBJTYPENAME(selectedObj) = "LinkObj") {single click on link}
                        ClearAllBlocks;
                        link := LinkObj(selectedObj);
                        IF link.parentID = activeWindow
                           ASK link TO SetSelected(TRUE); 
                           ASK link TO SetHighlightColor(Blue);
                           ASK link TO SetHighlighted(TRUE);
                           ASK link TO SetTranslation(link.Translation.x, link.Translation.y);
                           ASK selectGroup TO Add(link);
                           ASK link TO Draw;
                           blueObjRef := "RBDLink";
                           blueObjId := link.Id;
                           linkSelected := TRUE;
                           ASK menubar TO Enable(7);
                        END IF;
                     ELSE
                        ClearAllBlocks;
                     END IF;
                  END IF;
               ELSE {upClick -- just moved obj}
                  IF workingPaste
                  ELSIF justCtrled
                     ASK menubar TO Enable(1);
                     ASK menubar TO Enable2Thru6;
                     ASK menubar TO Enable(7);
                     IF (nodesIn + blocksIn + eventsIn + hiersIn) > 0
                        ASK menubar TO Enable(10);
                     END IF;
                     IF (nodesIn + blocksIn + eventsIn + hiersIn) > 1
                        ASK menubar TO Enable(14);
                        ASK menubar TO Disable(8);
                     ELSIF (nodesIn + blocksIn + eventsIn + hiersIn) = 1
                        ASK menubar TO Enable(8);
                        ASK menubar TO Disable(14);
                     END IF;
                     IF (hiersIn = 1)
                        ASK menubar TO Enable(10);
                        ASK menubar TO Enable(14);
                     END IF;
                     justCtrled := FALSE;
                  ELSIF draggingblock
                     Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                     displayX := displayX + curOffX;
                     displayY := displayY + curOffY;
                     Transform(baseroot, root, displayX, displayY, displayX, displayY);
                     IF (ABS(oldX-displayX) > 0.15) OR (ABS(oldY-displayY) > 0.15)
                        displayX := displayX + 0.02;
                        displayY := displayY - 0.072;
                        displayX := FLOAT(TRUNC(displayX));
                        displayY := FLOAT(TRUNC(displayY)+1);
                        IF displayY > 80.
                           displayY := 80.;
                        END IF;
                        IF displayX > 119.
                           displayX := 119.;
                        END IF;
                        SetCursor(NILOBJ);
                        RemoveThisGraphic(block);
                        ASK root TO AddAfterGraphic(defaultBlock,block);
                        ASK block TO SetScaling(1.,1.);
                        ASK block TO DisplayAt(displayX, displayY);
                        ASK block TO Draw;
                        somethingChanged := TRUE;
                     ELSE
                        SetCursor(NILOBJ);
                        RemoveThisGraphic(block);
                        ASK root TO AddAfterGraphic(defaultBlock,block);
                        ASK block TO SetScaling(1.,1.);
                        ASK block TO DisplayAt(oldX, oldY);
                        ASK block TO Draw;
                     END IF;
                     IF ((block.parentID <> activeWindow) OR (copyWindow <> activeWindow))
                        {if block belongs to a hier, remove from old hier}      
                        IF block.parentID > 0 
                           oldHier := ASK root Child("RBDHier", block.parentID);
                           IF oldHier.childGroup.Includes(block);
                              ASK oldHier.childGroup TO RemoveThis(block);
                           END IF;
                        END IF;
                        {if new window is a heir, add block to new hier}
                        IF activeWindow > 0
                           newHier := ASK root Child("RBDHier", activeWindow);
                           ASK newHier.childGroup TO Add(block);
                        END IF;
                        ASK block TO SetParentID(activeWindow);
                        {HandleLinks(FALSE);}
                     END IF;
                     FOREACH link IN partialToGroup
                        ASK link TO SetHidden(FALSE);
                        ASK link TO Draw;
                     END FOREACH;
                     FOREACH link IN partialFromGroup
                        ASK link TO SetHidden(FALSE);
                        ASK link TO Draw;
                     END FOREACH;
                     {Redraw links attached to block based on where it is now}
                     ASK block TO ResetLinks(totalLinks, block.Id, block.Translation.x, block.Translation.y, 
                                             block.ReferenceName, draggingStuff, nowPasting);
                     draggingblock := FALSE;
                     scrollScreen := FALSE; 
                     ASK menubar TO Enable(1);
                     ASK menubar TO Enable2Thru6;
                     ASK menubar TO Enable(7);
                     ASK menubar TO Enable(8);
                     ASK menubar TO Enable(10);
                  ELSIF draggingevent
                     Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                     displayX := displayX + curOffX;
                     displayY := displayY + curOffY;
                     Transform(baseroot, root, displayX, displayY, displayX, displayY);
                     IF (ABS(oldX-displayX) > 0.15) OR (ABS(oldY-displayY) > 0.15)
                        displayX := displayX + 0.02;
                        displayY := displayY - 0.072;
                        displayX := FLOAT(TRUNC(displayX));
                        displayY := FLOAT(TRUNC(displayY)+1);
                        IF displayY > 80.
                           displayY := 80.;
                        END IF;
                        IF displayX > 119.
                           displayX := 119.;
                        END IF;
                        SetCursor(NILOBJ);
                        RemoveThisGraphic(event);
                        ASK root TO AddAfterGraphic(defaultBlock,event);
                       { ASK event TO SetParentID(activeWindow);}
                        ASK event TO SetScaling(1.,1.);
                        ASK event TO DisplayAt(displayX, displayY);
                        ASK event TO Draw;
                        somethingChanged := TRUE;
                     ELSE
                        SetCursor(NILOBJ);
                        RemoveThisGraphic(event);
                        ASK root TO AddAfterGraphic(defaultBlock,event);
                        ASK event TO SetScaling(1.,1.);
                        ASK event TO DisplayAt(oldX, oldY);
                        ASK event TO Draw;
                     END IF;
                     IF ((event.parentID <> activeWindow) OR (copyWindow <> activeWindow))
                        {if event belongs to a hier, remove from old hier}      
                        IF event.parentID > 0 
                           oldHier := ASK root Child("RBDHier", event.parentID);
                           IF oldHier.childGroup.Includes(event);
                              ASK oldHier.childGroup TO RemoveThis(event);
                           END IF;
                        END IF;
                        {if new window is a heir, add event to new hier}
                        IF activeWindow > 0
                           newHier := ASK root Child("RBDHier", activeWindow);
                           ASK newHier.childGroup TO Add(event);
                        END IF;
                        ASK event TO SetParentID(activeWindow);
                        {HandleLinks(FALSE);}
                     END IF;
                     FOREACH link IN partialToGroup
                        ASK link TO SetHidden(FALSE);
                        ASK link TO Draw;
                     END FOREACH;
                     FOREACH link IN partialFromGroup
                        ASK link TO SetHidden(FALSE);
                        ASK link TO Draw;
                     END FOREACH;
                     {Redraw links attached to block based on where it is now}
                     ASK event TO ResetLinks(totalLinks, event.Id, event.Translation.x, event.Translation.y, 
                                             event.ReferenceName, draggingStuff, nowPasting);
                     draggingevent := FALSE;
                     scrollScreen := FALSE; 
                     ASK menubar TO Enable(1);
                     ASK menubar TO Enable2Thru6;
                     ASK menubar TO Enable(7);
                     ASK menubar TO Enable(10);
                     ASK menubar TO Enable(8);
                  ELSIF dragginghier
                     Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                     displayX := displayX + curOffX;
                     displayY := displayY + curOffY;
                     Transform(baseroot, root, displayX, displayY, displayX, displayY);
                     IF (ABS(oldX-displayX) > 0.15) OR (ABS(oldY-displayY) > 0.15)
                        displayX := displayX + 0.02;
                        displayY := displayY - 0.072;
                        displayX := FLOAT(TRUNC(displayX));
                        displayY := FLOAT(TRUNC(displayY)+1);
                        IF displayY > 80.
                           displayY := 80.;
                        END IF;
                        IF displayX > 119.
                           displayX := 119.;
                        END IF;
                        SetCursor(NILOBJ);
                        RemoveThisGraphic(hier);
                        ASK root TO AddAfterGraphic(defaultBlock,hier);
                        ASK hier TO SetScaling(1.,1.);
                        ASK hier TO DisplayAt(displayX, displayY);
                        ASK hier TO Draw;
                        somethingChanged := TRUE;
                     ELSE
                        SetCursor(NILOBJ);
                        RemoveThisGraphic(hier);
                        ASK root TO AddAfterGraphic(defaultBlock,hier);
                        ASK hier TO SetScaling(1.,1.);
                        ASK hier TO DisplayAt(oldX, oldY);
                        ASK hier TO Draw;
                     END IF;
                     IF ((hier.parentID <> activeWindow) OR (copyWindow <> activeWindow))
                        {if hier belongs to a hier, remove from old hier}      
                        IF hier.parentID > 0 
                           oldHier := ASK root Child("RBDHier", hier.parentID);
                           IF oldHier.childGroup.Includes(hier);
                              ASK oldHier.childGroup TO RemoveThis(hier);
                           END IF;
                        END IF;
                        {if new window is a hier, add "hier" to new hier}
                        IF activeWindow > 0
                           newHier := ASK root Child("RBDHier", activeWindow);
                           ASK newHier.childGroup TO Add(hier);
                           ASK hier TO SetParentID(newHier.Id);
                           ASK hier TO SetLevel(newHier.level+1);
                           ASK hier TO SetChildLevels;
                           IF hier.level > deepestLevel
                              deepestLevel := hier.level
                           END IF;
                        ELSE
                           IF hier.level = deepestLevel
                              deepestLevel := 0;
                           END IF;
                           ASK hier TO SetLevel(1);
                           IF deepestLevel = 0
                              FOREACH tempHier IN hierGroup
                                 IF tempHier.level > deepestLevel
                                    deepestLevel := tempHier.level
                                 END IF;
                              END FOREACH;
                           END IF;
                           ASK hier TO SetChildLevels;
                        END IF;
                        ASK hier TO SetParentID(activeWindow);
                     END IF;
                     FOREACH link IN partialToGroup
                        ASK link TO SetHidden(FALSE);
                        ASK link TO Draw;
                     END FOREACH;
                     FOREACH link IN partialFromGroup
                        ASK link TO SetHidden(FALSE);
                        ASK link TO Draw;
                     END FOREACH;
                     ASK hier TO ResetLinks(totalLinks, hier.Id, hier.Translation.x,
                                             hier.Translation.y, hier.ReferenceName,
                                             draggingStuff, nowPasting);
                     dragginghier := FALSE;
                     scrollScreen := FALSE; 
                     ASK menubar TO Enable(1);
                     ASK menubar TO Enable2Thru6;
                     ASK menubar TO Enable(7);
                     ASK menubar TO Enable(10);
                     ASK menubar TO Enable(8);
                  ELSIF draggingStuff {moving multple items -- just dropped multiple elements on screen OR just
                                       pasted single element or multiple elements}
                     IF selectGroup.numberIn > 10
                        NEW(linkMessage);
                        ASK linkMessage TO LoadFromLibrary(images,"LinkText");
                        AddGraphic(linkMessage);
                        linkText := ASK linkMessage Child("LinkText", 851);
                        ASK linkText TO SetText("Dropping multiple items...  please wait...");
                        ASK linkText TO SetFont(SystemText);
                        ASK linkText TO SetColor(Black);
                        ASK linkMessage TO DisplayAt(300.,619.);
                        messageUp := TRUE;
                     END IF;
                     SetDeferral(TRUE);
                     typeOfCursor := dialogC;
                     SetSysCursor(BusyCursor);
                     SetCursor(NILOBJ);
                     IF ((ClickX <> oldClickX) AND (ClickY <> oldClickY)) OR pastingMultiple
                        ASK selected TO GetBoundingBox(boxStartX,boxStartY,boxEndX,boxEndY);
                        Transform(NILOBJ, root, boxStartX, boxStartY, boxStartX, boxStartY);
                        Transform(NILOBJ, root, boxEndX, boxEndY, boxEndX, boxEndY);
                        IF boxStartX < 0.
                           offsetX := 1. - refX;
                        ELSIF boxEndX > 119.
                           offsetX := (boxStartX-refX) - (boxEndX-120.);
                        ELSE
                           offsetX := boxStartX-refX;
                        END IF;
                        IF boxStartY < 1.
                           offsetY := 1. - refY;
                        ELSIF boxEndY > 80.
                           offsetY := (boxStartY-refY) - (boxEndY-80.);
                        ELSE
                           offsetY := boxStartY-refY;
                        END IF;
                        offsetX := offsetX + 0.02;
                        offsetY := offsetY - 0.07;
                        IF offsetX >= 0.
                           offsetX := FLOAT(TRUNC(offsetX));
                        ELSE
                           offsetX := FLOAT(TRUNC(offsetX))-1.;
                        END IF;
                        IF offsetY >= 0.
                           offsetY := FLOAT(TRUNC(offsetY))+1.;
                        ELSE
                           offsetY := FLOAT(TRUNC(offsetY));
                        END IF;
                     ELSE
                        offsetX := 0.0;
                        offsetY := 0.0;
                     END IF;
                     FOREACH current IN selectGroup                        
                        ASK selected TO RemoveThisGraphic(current);
                        Transform(baseroot,root,current.xPosition,current.yPosition,newx,newy);
                        ASK current TO SetScaling(1.,1.);
                        ASK current TO SetTranslation(newx,newy);
                        ASK root TO AddAfterGraphic(defaultBlock,current);
                        {if object belongs to a hier, remove from old hier}      
                        IF current.parentID > 0 
                           oldHier := ASK root Child("RBDHier", current.parentID);
                           IF (oldHier <> NILOBJ) {copied into new RBD and oldHier no longer exists}
                              IF oldHier.childGroup.Includes(current);
                                 ASK oldHier.childGroup TO RemoveThis(current);
                              END IF;
                           END IF;
                        END IF;
                        {if new window is a hier, add to new hier}
                        IF activeWindow > 0
                           newHier := ASK root Child("RBDHier", activeWindow);
                           IF (NOT(newHier.childGroup.Includes(current)))
                              ASK newHier.childGroup TO Add(current);
                              IF OBJTYPENAME(current) = "RBDHierObj"
                                 tempHier2 := ASK root Child("RBDHier", current.Id);
                                 ASK tempHier2 TO SetLevel(newHier.level+1);
                                 ASK tempHier2 TO SetChildLevels;
                                 IF tempHier2.level > deepestLevel
                                    deepestLevel := tempHier2.level
                                 END IF;
                              END IF;
                           END IF;
                        ELSE  {this may be called unnecessarily if dragging stuff includes hiers inside a hier}
                           IF ((current.parentID <> activeWindow) OR (copyWindow <> activeWindow))
                              IF OBJTYPENAME(current) = "RBDHierObj"
                                 tempHier2 := ASK root Child("RBDHier", current.Id);
                                 IF tempHier2.level = deepestLevel
                                    deepestLevel := 0;
                                 END IF;
                                 ASK tempHier2 TO SetLevel(1);
                                 IF deepestLevel = 0
                                    FOREACH tempHier IN hierGroup
                                       IF tempHier.level > deepestLevel
                                          deepestLevel := tempHier.level
                                       END IF;
                                    END FOREACH;
                                 END IF;
                                 ASK tempHier2 TO SetChildLevels;
                              END IF;
                           END IF;
                        END IF;
                        ASK current TO SetParentID(activeWindow);
                        ASK current TO DisplayAt(current.xPosition+offsetX,current.yPosition+offsetY);
                     END FOREACH;
                     IF linksIn > 0
                        FOREACH link IN selectedLinksGroup
                           ASK link TO SetHighlighted(FALSE);
                           IF selected.IncludesGraphic(link);
                           ASK selected TO RemoveThisGraphic(link);
                           END IF;
                           ASK link TO ChangeRoots(FALSE, baseroot);
                           ASK root TO AddBeforeGraphic(defaultBlock, link);
                           IF ((link.parentID <> activeWindow) OR (copyWindow <> activeWindow))
                              {if object belongs to a hier, remove from old hier}      
                              IF link.parentID > 0 
                                  IF (oldHier <> NILOBJ)
                                     oldHier := ASK root Child("RBDHier", link.parentID);
                                     IF oldHier.childGroup.Includes(link);
                                       ASK oldHier.childGroup TO RemoveThis(link);
                                     END IF;
                                  END IF;
                              END IF;
                              {if new window is a heir, add to new hier}
                              IF activeWindow > 0
                                 newHier := ASK root Child("RBDHier", activeWindow);
                                 ASK newHier.childGroup TO Add(link);
                              END IF;
                              ASK link TO SetParentID(activeWindow);
                           END IF;
                        END FOREACH;
                     END IF;      
                     FOREACH link IN partialFromGroup
                        ASK link TO SetHidden(FALSE);
                        ASK link TO Draw;
                     END FOREACH;
                     FOREACH link IN partialToGroup
                        ASK link TO SetHidden(FALSE);
                        ASK link TO Draw;
                     END FOREACH;
                     FOREACH current IN selectGroup;
                        ASK current TO ResetLinks(totalLinks, current.Id, current.Translation.x,current.Translation.y, 
                                                  current.ReferenceName, draggingStuff, nowPasting);
                     END FOREACH;
                     ASK root TO Draw;
                     RemoveThisGraphic(selected);
                     DISPOSE(selected);
                     SetCursorOffset(0.0,0.0);
                     IF pastingMultiple
                        {FOR i := 1 TO totalObjects
                           IF i <= totalBlocks      
                              tempBlock := ASK root Child("RBDBlock",i);
                              IF tempBlock.DependencyNum > (totalBlocks-pasteBlocks) 
                                 ASK tempBlock TO SetDepNum(tempBlock.DependencyNum+pasteBlocks);
                              END IF;
                           ELSE      
                              tempNode := ASK root Child("RBDNode",i-totalBlocks);
                              IF tempNode.DependencyNum > (totalBlocks-pasteBlocks)
                                 ASK tempNode TO SetDepNum(tempNode.DependencyNum+pasteBlocks);
                              END IF;
                           END IF;
                        END FOR; }
                        IF depMsg
                           NEW(message,1..2);
                           message[1] := "At least one item being copied was using elemental dependency. That     ";
                           message[2] := "property does not transfer.  The new item has been set to independent.     ";
                           result := SendAlert(message, FALSE, FALSE, FALSE);
                           DISPOSE(message);
                        END IF;
                        pastingMultiple := FALSE;
                     END IF;
                     somethingChanged := TRUE;
                     draggingStuff := FALSE;
                     scrollScreen := FALSE; 
                     ASK menubar TO Enable(1);
                     ASK menubar TO Enable2Thru6;
                     ASK menubar TO Enable(7);
                     IF (nodesIn + blocksIn + eventsIn + hiersIn) > 0
                        ASK menubar TO Enable(10);
                     END IF;
                     IF (nodesIn + blocksIn + eventsIn + hiersIn) > 1
                        ASK menubar TO Enable(14);
                     ELSIF (nodesIn + blocksIn + eventsIn + hiersIn) = 1
                        ASK menubar TO Enable(8);
                     END IF;
                     IF (hiersIn = 1)
                        ASK menubar TO Enable(14);
                     END IF;
                     IF messageUp
                        DISPOSE(linkMessage);
                        messageUp := FALSE;
                     END IF;
                     typeOfCursor := nilC;
                     SetSysCursor(NormalCursor);
                     SetDeferral(FALSE);
                     Draw;
                  ELSIF drawingBox
                     Transform(NILOBJ, root, ClickX, ClickY, boxEndX, boxEndY);
                     IF boxStartX > boxEndX
                        varSwitch := boxEndX;
                        boxEndX := boxStartX;
                        boxStartX := varSwitch;
                     END IF;
                     IF boxStartY > boxEndY
                        varSwitch := boxEndY;
                        boxEndY := boxStartY;
                        boxStartY := varSwitch;
                     END IF;
                     IF boxStartX <= 1.
                        boxStartX := 0.;
                     END IF;
                     IF boxStartY <= 1.
                        boxStartY := 0.;
                     END IF;
                     IF boxEndX >= 119.
                        boxEndX := 120.;
                     END IF;
                     IF boxEndY >= 79.
                        boxEndY := 80.;
                     END IF;
                     FOREACH node IN nodeGroup
                        IF (node.xPosition>=boxStartX) AND ((node.xPosition+0.8)<=boxEndX) AND ((node.yPosition-0.6)>=boxStartY) AND (node.yPosition<=boxEndY)
                           IF node.parentID = activeWindow 
                              SelectNode;
                           END IF;
                        END IF;
                     END FOREACH;
                     FOREACH block IN blockGroup
                        IF (block.xPosition>=boxStartX) AND ((block.xPosition+0.8)<=boxEndX) AND ((block.yPosition-0.6)>=boxStartY) AND (block.yPosition<=boxEndY)                     
                           IF block.parentID = activeWindow
                              SelectBlock;
                           END IF;
                        END IF;
                     END FOREACH;
                     FOREACH event IN eventGroup
                        IF (event.xPosition>=boxStartX) AND ((event.xPosition+0.8)<=boxEndX) AND ((event.yPosition-0.6)>=boxStartY) AND (event.yPosition<=boxEndY)                     
                           IF event.parentID = activeWindow
                              SelectEvent;
                           END IF;
                        END IF;
                     END FOREACH;
                     FOREACH hier IN hierGroup
                        IF (hier.xPosition>=boxStartX) AND ((hier.xPosition+0.8)<=boxEndX) AND ((hier.yPosition-0.6)>=boxStartY) AND (hier.yPosition<=boxEndY)
                           IF hier.parentID = activeWindow 
                              SelectHier;
                           END IF;
                        END IF;
                     END FOREACH;
                     IF selectGroup.numberIn > 0
                        FOREACH link IN linkGroup
                           IF link.parentID = activeWindow
                              fromSelected:=FALSE;
                              toSelected:=FALSE;                        
                              IF link.connectFRef="RBDBlock"
                                 block := ASK root Child("RBDBlock",link.connectFromId);
                                 IF block.Selected
                                    fromSelected:=TRUE;
                                 END IF;
                              ELSIF link.connectFRef="RBDEvent"
                                 event := ASK root Child("RBDEvent",link.connectFromId);
                                 IF event.Selected
                                    fromSelected:=TRUE;
                                 END IF;
                              ELSIF link.connectFRef="RBDNode"
                                 node := ASK root Child("RBDNode",link.connectFromId);
                                 IF node.Selected
                                    fromSelected:=TRUE;
                                 END IF;
                              ELSE
                                 hier := ASK root Child("RBDHier",link.connectFromId);
                                 IF hier.Selected
                                    fromSelected:=TRUE;
                                 END IF;
                              END IF;
                              IF link.connectTRef="RBDBlock"
                                 block := ASK root Child("RBDBlock",link.connectToId);
                                 IF block.Selected
                                    toSelected:=TRUE;
                                 END IF;
                              ELSIF link.connectTRef="RBDEvent"
                                 event := ASK root Child("RBDEvent",link.connectToId);
                                 IF event.Selected
                                    toSelected:=TRUE;
                                 END IF;
                              ELSIF link.connectTRef="RBDNode"
                                 node := ASK root Child("RBDNode",link.connectToId);
                                 IF node.Selected
                                    toSelected:=TRUE;
                                 END IF;
                              ELSE
                                 hier := ASK root Child("RBDHier",link.connectToId);
                                 IF hier.Selected
                                    toSelected:=TRUE;
                                 END IF;
                              END IF;
                              IF (fromSelected AND toSelected)
                                 INC(linksIn);
                                 ASK selectedLinksGroup TO Add(link);
                              ELSIF (fromSelected) 
                                 ASK partialFromGroup TO Add(link);
                              ELSIF (toSelected)   
                                 ASK partialToGroup TO Add(link);
                              END IF;
                           END IF;
                        END FOREACH;
                     END IF;
                     SetCursor(NILOBJ);
                     DISPOSE(rubberBox);
                     IF (nodesIn + blocksIn + eventsIn + hiersIn) = 1
                        ASK menubar TO Enable2Thru6;
                        ASK menubar TO Enable(1);
                        ASK menubar TO Enable(7);
                        ASK menubar TO Enable(10);
                        ASK menubar TO Enable(8);
                     ELSIF (nodesIn + blocksIn + eventsIn + hiersIn) > 1
                        ASK menubar TO Enable2Thru6;
                        ASK menubar TO Enable(1);
                        ASK menubar TO Enable(7);
                        ASK menubar TO Enable(10);
                        ASK menubar TO Enable(14);
                     ELSE
                        IF totalObjects = 0
                           ASK menubar TO Enable(5);
                           ASK menubar TO Enable(6);
                           ASK menubar TO Enable(15);
                        ELSE
                           ASK menubar TO Enable2Thru6;
                        END IF;
                     END IF;
                     IF hiersIn = 1
                        ASK menubar TO Enable(10);
                        ASK menubar TO Enable(14);
                     END IF;
                     scrollScreen := FALSE; 
                     drawingBox := FALSE;
                     drawingRed := FALSE;
                     Draw;
                  ELSIF draggingnode
                     Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                     displayX := displayX + curOffX;
                     displayY := displayY + curOffY;
                     Transform(baseroot, root, displayX, displayY, displayX, displayY);
                     IF (ABS(oldX-displayX) > 0.15) OR (ABS(oldY-displayY) > 0.15)
                        displayX := displayX + 0.036;
                        displayY := displayY - 0.045;
                        displayX := FLOAT(TRUNC(displayX));
                        displayY := FLOAT(TRUNC(displayY)+1);
                        IF displayY > 80.
                           displayY := 80.;
                        END IF;
                        IF displayX > 120.
                           displayX := 120.;
                        END IF;
                        SetCursor(NILOBJ);
                        RemoveThisGraphic(node);
                        ASK root TO AddAfterGraphic(defaultBlock,node);
                        ASK node TO SetScaling(1.,1.);
                        ASK node TO DisplayAt(displayX, displayY);
                        ASK node TO Draw;
                        somethingChanged := TRUE;
                     ELSE
                        SetCursor(NILOBJ);
                        RemoveThisGraphic(node);
                        ASK root TO AddAfterGraphic(defaultBlock,node);
                        ASK node TO SetScaling(1.,1.);
                        ASK node TO DisplayAt(oldX, oldY);
                        ASK node TO Draw;
                     END IF;
                     IF ((node.parentID <> activeWindow) OR (copyWindow <> activeWindow))
                        {if node belongs to a hier, remove from old hier}      
                        IF node.parentID > 0 
                           oldHier := ASK root Child("RBDHier", node.parentID);
                           IF oldHier.childGroup.Includes(node);
                              ASK oldHier.childGroup TO RemoveThis(node);
                           END IF;
                        END IF;
                        {if new window is a heir, add node to new hier}
                        IF activeWindow > 0
                           newHier := ASK root Child("RBDHier", activeWindow);
                           ASK newHier.childGroup TO Add(node);
                        END IF;
                        ASK node TO SetParentID(activeWindow);
                        {HandleLinks(FALSE);}
                     END IF;
                     FOREACH link IN partialFromGroup
                        ASK link TO SetHidden(FALSE);
                        ASK link TO Draw;
                     END FOREACH;
                     FOREACH link IN partialToGroup
                        ASK link TO SetHidden(FALSE);
                        ASK link TO Draw;
                     END FOREACH;
                     ASK node TO ResetLinks(totalLinks, node.Id, node.Translation.x,
                                            node.Translation.y, node.ReferenceName, draggingStuff, nowPasting);
                     draggingnode := FALSE;
                     scrollScreen := FALSE; 
                     ASK menubar TO Enable(1);
                     ASK menubar TO Enable2Thru6;
                     ASK menubar TO Enable(7);
                     ASK menubar TO Enable(10);
                     ASK menubar TO Enable(8);
                  END IF;
               END IF;
               IF SecondClick AND buttondown AND (selectGroup.numberIn = 1) AND (NOT CtrlClick) AND (NOT linkSelected)
                  IF dragginghier
                     ASK menubar TO Enable2Thru6;
                     RemoveThisGraphic(hier);
                     ASK root TO AddAfterGraphic(defaultBlock,hier);
                     ASK hier TO SetScaling(1.,1.);
                     ASK window TO SetCursor(NILOBJ);
                     ASK hier TO DisplayAt(hier.xPosition, hier.yPosition);
                     ASK hier TO SetSelected(FALSE);
                     ASK hier TO SetHighlighted(FALSE);
                     ASK hier TO Draw;
                     ASK selectGroup TO RemoveThis(hier);
                     dragginghier := FALSE;
                     scrollScreen := FALSE;
                     ChangeWindow(hier.Id,hier.level); 
                  ELSE 
                     outClick := FALSE;
                     IF draggingblock
                        RemoveThisGraphic(block);
                        ASK root TO AddAfterGraphic(defaultBlock,block);
                        ASK block TO SetScaling(1.,1.);
                     ELSIF draggingevent
                        RemoveThisGraphic(event);
                        ASK root TO AddAfterGraphic(defaultBlock,event);
                        ASK event TO SetScaling(1.,1.);
                     ELSIF dragginghier
                        RemoveThisGraphic(hier);
                        ASK root TO AddAfterGraphic(defaultBlock,hier);
                        ASK hier TO SetScaling(1.,1.); 
                     ELSIF draggingnode
                        RemoveThisGraphic(node);
                        ASK root TO AddAfterGraphic(defaultBlock,node);
                        ASK node TO SetScaling(1.,1.);
                        IF node.typeNode = 5
                           outClick := TRUE;
                        END IF;
                     END IF;
                     draggingblock := FALSE;
                     draggingevent := FALSE;
                     draggingnode := FALSE;
                     dragginghier := FALSE;
                     scrollScreen := FALSE;
                     IF outClick
                        ASK menubar TO Enable2Thru6;
                        ASK window TO SetCursor(NILOBJ);
                        ASK node TO DisplayAt(node.xPosition, node.yPosition);
                        ASK node TO SetSelected(FALSE);
                        ASK node TO SetHighlighted(FALSE);
                        ASK node TO Draw;
                        ASK selectGroup TO RemoveThis(node);
                        IF hierLevel = 1
                           ChangeWindow(0,0);
                        ELSE
                           tempHier := ASK root Child("RBDHier", node.parentID);
                           ChangeWindow(tempHier.parentID,hierLevel-1);
                        END IF;   
                     ELSE
                        EditDetails;
                     END IF;
                     ASK menubar TO Enable2Thru6;
                  END IF;
               ELSIF SecondClick AND (NOT buttondown) AND (selected <> NILOBJ) AND (NOT CtrlClick)
               END IF;
            ELSIF (Button = 1) AND (NOT draggingblock) AND (NOT draggingevent) AND (NOT draggingnode) AND (NOT dragginghier) AND (NOT draggingStuff) AND (NOT drawingRed)
               IF buttondown 
                  ClearAllBlocks;
                  NEW(zoomRubber);
                  ASK zoomRubber TO SetWidth(2.);
                  ASK zoomRubber TO SetColor(Yellow);
                  ASK zoomRubber TO SetRatio (.7) ;
                  SetCursor(zoomRubber);
                  Transform(NILOBJ, baseroot, ClickX, ClickY, boxStartX, boxStartY);
                  ASK zoomRubber TO SetAnchor(boxStartX,boxStartY); 
                  Transform(NILOBJ, root, ClickX, ClickY, boxStartX, boxStartY);
                  ASK menubar TO Disable1Thru8;
                  drawingBox := TRUE;
                  scrollScreen := TRUE; 
               ELSE
                  IF drawingBox
                     SetCursor (NILOBJ) ;
                     ASK zoomRubber TO GetEndPoint(boxEndX, boxEndY);
                     Transform(baseroot,root, boxEndX, boxEndY, tempX, tempY);
                     IF (tempX > 120.) OR (tempY > 80.)
                        Transform(NILOBJ,root, boxEndX, boxEndY, boxEndX, boxEndY);
                     ELSE
                        Transform(baseroot,root, boxEndX, boxEndY, boxEndX, boxEndY);
                     END IF;
                     IF (boxStartX = boxEndX) OR (boxStartY = boxEndY) 
                        SetView(localZoomVal, (boxStartX-localZoomVal/2.), (boxStartY+localZoomVal*13.2/40.));
                     ELSE
                        localZoomVal := MAXOF(boxEndX,boxStartX) - MINOF(boxStartX,boxEndX);
                        localZoomVal := MAXOF(5.,localZoomVal);
                        IF localZoomVal <> cusZoomVal {zoompossibleproblem}
                           cusZoomVal := localZoomVal;
                        ELSE
                           ASK zoomHier TO SetZoom(localZoomVal);
                        END IF;
                        SetView(localZoomVal, MINOF(boxStartX,boxEndX),MAXOF(boxStartY,boxEndY));
                     END IF;
                     DISPOSE(zoomRubber);
                     drawingBox := FALSE;
                     scrollScreen := FALSE;
                     IF totalObjects = 0
                        ASK menubar TO Enable(5);
                        ASK menubar TO Enable(6);
                        ASK menubar TO Enable(15);
                     ELSE
                        ASK menubar TO Enable2Thru6;
                     END IF;
                  ELSE
                     Transform(NILOBJ, root, ClickX, ClickY, tempX, tempY);
                     SetView(localZoomVal, (tempX-localZoomVal/2.), (tempY+localZoomVal*13.2/40.));
                  END IF;
               END IF;
            ELSIF (Button = 2) AND buttondown AND (NOT drawingBox) AND (NOT draggingblock) AND (NOT draggingevent) AND (NOT draggingnode) AND (NOT dragginghier)
               AND (NOT draggingStuff) AND (NOT nowSimulating) {right click}
               selectedObj := ASK root TO Select(x,y);
               IF selectedObj <> NILOBJ
                  IF (OBJTYPENAME(selectedObj)="GridObj") 
                     ClearAllBlocks;
                     ShowRightClick(1); {right click workspace view}
                  ELSIF (ASK selectGroup Includes(selectedObj)) AND (selectGroup.numberIn > 1)
                     ShowRightClick(3); {right click multiple elements}
                  ELSIF (OBJTYPENAME(selectedObj) = "RBDBlockObj")
                     ClearAllBlocks;
                     block := RBDBlockObj(selectedObj);
                     IF block.parentID = activeWindow
                        SelectBlock;
                        HandleLinks(TRUE);
                        ASK menubar TO Enable(1);
                        ASK menubar TO Enable2Thru6;
                        ASK menubar TO Enable(7);
                        ASK menubar TO Enable(8);
                        ASK menubar TO Enable(10);
                        ShowRightClick(2); {right click on block}
                     END IF;
                  ELSIF (OBJTYPENAME(selectedObj) = "RBDEventObj")
                     ClearAllBlocks;
                     event := RBDEventObj(selectedObj);
                     IF event.parentID = activeWindow
                        SelectEvent;
                        HandleLinks(TRUE);
                        ASK menubar TO Enable(1);
                        ASK menubar TO Enable2Thru6;
                        ASK menubar TO Enable(7);
                        ASK menubar TO Enable(8);
                        ASK menubar TO Enable(10);
                        ShowRightClick(2); {right click on event}
                     END IF;
                  ELSIF (OBJTYPENAME(selectedObj) = "RBDHierObj")
                     ClearAllBlocks;
                     hier := RBDHierObj(selectedObj);
                     IF hier.parentID = activeWindow
                        SelectHier;
                        HandleLinks(TRUE);
                        ASK menubar TO Enable(1);
                        ASK menubar TO Enable2Thru6;
                        ASK menubar TO Enable(7);
                        ASK menubar TO Enable(8);
                        ASK menubar TO Enable(10);
                        ShowRightClick(2); {right click on hierarchy}
                     END IF;
                  ELSIF (OBJTYPENAME(selectedObj) = "RBDNodeObj")
                     ClearAllBlocks;
                     node := RBDNodeObj(selectedObj);
                     IF node.parentID = activeWindow
                        SelectNode;
                        IF node.typeNode = 2
                           HandleLinks(TRUE);
                        END IF;
                        ASK menubar TO Enable(1);
                        ASK menubar TO Enable2Thru6;
                        ASK menubar TO Enable(7);
                        ASK menubar TO Enable(8);
                        ASK menubar TO Enable(10);
                        IF node.typeNode = 2
                           ShowRightClick(2); {right click on node}
                        END IF;
                     END IF;
                  END IF;
               END IF;             
            END IF;
         WHEN blockC:
            IF Button = 2  {right click}
               SetCursor(NILOBJ);
               IF (block.parentID > 0)
                  tempHier := ASK root Child("RBDHier", block.parentID);
                  IF (tempHier <> NILOBJ)
                     IF tempHier.childGroup.Includes(block);
                        ASK tempHier.childGroup TO RemoveThis(block);
                     END IF;
                  END IF;
               END IF;
               DEC(nextId);
               ASK blockGroup TO RemoveThis(block);
               DISPOSE(block);
               totalBlocks := totalBlocks - 1;
               totalObjects := totalObjects - 1;
               typeOfCursor := nilC;
               IF totalObjects = 0
                  ASK menubar TO Enable(5);
                  ASK menubar TO Enable(6);
                  ASK menubar TO Enable(15);
               ELSE
                  ASK menubar TO Enable2Thru6;
               END IF;
               scrollScreen := FALSE; 
               buttItem := ASK menuTool Descendant("BlockButton", 908);
               ASK buttItem TO SetSelected(FALSE);
               ASK buttItem TO Draw;
               buttItem := ASK menuTool Descendant("EventButton", 911);
               ASK buttItem TO SetSelected(FALSE);
               ASK buttItem TO Draw;
            ELSIF Button = 0
               IF buttondown
                  Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                  scrollScreen := FALSE;
                  displayX := displayX + curOffX;
                  displayY := displayY + curOffY;
                  Transform(baseroot, root, displayX, displayY, displayX, displayY);
                  displayX := displayX + 0.02;
                  displayY := displayY - 0.072;  
                  displayX := FLOAT(TRUNC(displayX));
                  displayY := FLOAT(TRUNC(displayY)+1);
                  IF displayY > 80.
                     displayY := 80.;
                  END IF;
                  IF displayX > 119.
                     displayX := 119.;
                  END IF;
                  SetCursor(NILOBJ);
                  typeOfCursor := dialogC;
                  RemoveThisGraphic(block);
                  ASK root TO AddAfterGraphic(defaultBlock,block);
                  ASK block TO SetScaling(1.,1.);
                  ASK block TO DisplayAt(displayX, displayY);
                  ASK block TO Draw;
                  NEW(detailsDialog);
                  ASK detailsDialog TO LoadFromLibrary(dialogs, "BlockPropBox");
                  AddGraphic(detailsDialog);
                  ASK detailsDialog TO Draw;
                  ASK detailsDialog TO ReceiveData(block, "950", cancelled);
                  DISPOSE(detailsDialog);
                  ASK block TO Draw;
                  IF cancelled
                     IF (block.parentID > 0)
                        tempHier := ASK root Child("RBDHier", block.parentID);
                        IF (tempHier <> NILOBJ)
                           IF tempHier.childGroup.Includes(block);
                              ASK tempHier.childGroup TO RemoveThis(block);
                           END IF;
                        END IF;
                     END IF;
                     ASK blockGroup TO RemoveThis(block);
                     DEC(nextId);
                     DISPOSE(block);
                     totalBlocks := totalBlocks - 1;
                     totalObjects := totalObjects - 1;
                  ELSE
                     ASK menubar TO Disable(17);
                     analViewAvail := FALSE;
                     configFrozen := FALSE;
                  END IF;
                  AddBlock;
               END IF;
            END IF;
         WHEN eventC:
            IF Button = 2  {right click}
               SetCursor(NILOBJ);
               IF (event.parentID > 0)
                  tempHier := ASK root Child("RBDHier", event.parentID);
                  IF (tempHier <> NILOBJ)
                     IF tempHier.childGroup.Includes(event);
                        ASK tempHier.childGroup TO RemoveThis(event);
                     END IF;
                  END IF;
               END IF;
               ASK eventGroup TO RemoveThis(event);
               DEC(nextId);
               DISPOSE(event);
               totalEvents := totalEvents - 1;
               totalObjects := totalObjects - 1;
               typeOfCursor := nilC;
               IF totalObjects = 0
                  ASK menubar TO Enable(5);
                  ASK menubar TO Enable(6);
                  ASK menubar TO Enable(15);
               ELSE
                  ASK menubar TO Enable2Thru6;
               END IF;
               scrollScreen := FALSE; 
               buttItem := ASK menuTool Descendant("EventButton", 911);
               ASK buttItem TO SetSelected(FALSE);
               ASK buttItem TO Draw;
            ELSIF Button = 0
               IF buttondown
                  Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                  scrollScreen := FALSE;
                  displayX := displayX + curOffX;
                  displayY := displayY + curOffY;
                  Transform(baseroot, root, displayX, displayY, displayX, displayY);
                  displayX := displayX + 0.02;
                  displayY := displayY - 0.072;  
                  displayX := FLOAT(TRUNC(displayX));
                  displayY := FLOAT(TRUNC(displayY)+1);
                  IF displayY > 80.
                     displayY := 80.;
                  END IF;
                  IF displayX > 119.
                     displayX := 119.;
                  END IF;
                  SetCursor(NILOBJ);
                  typeOfCursor := dialogC;
                  RemoveThisGraphic(event);
                  ASK root TO AddAfterGraphic(defaultBlock,event);
                  ASK event TO SetScaling(1.,1.);
                  ASK event TO DisplayAt(displayX, displayY);
                  ASK event TO Draw;
                  NEW(eventsDialog);
                  ASK eventsDialog TO LoadFromLibrary(dialogs, "EventPropBox");
                  AddGraphic(eventsDialog);
                  ASK eventsDialog TO Draw;
                  ASK eventsDialog TO ReceiveData(event, cancelled);
                  DISPOSE(eventsDialog);
                  ASK event TO Draw;
                  IF cancelled
                     IF (event.parentID > 0)
                        tempHier := ASK root Child("RBDHier", event.parentID);
                        IF (tempHier <> NILOBJ)
                           IF tempHier.childGroup.Includes(event);
                              ASK tempHier.childGroup TO RemoveThis(event);
                           END IF;
                        END IF;
                     END IF;
                     ASK eventGroup TO RemoveThis(event);
                     DEC(nextId);
                     DISPOSE(event);
                     totalEvents := totalEvents - 1;
                     totalObjects := totalObjects - 1;
                  ELSE
                     ASK menubar TO Disable(17);
                     analViewAvail := FALSE;
                     configFrozen := FALSE;
                  END IF;
                  AddEvent;
               END IF;
            END IF;
         WHEN hierC:
            IF collapsing OR importing
               ASK menubar TO Enable2Thru6;
            END IF;   
            IF ((Button = 2) AND (NOT collapsing) AND (NOT importing)) {right click}
               SetCursor(NILOBJ);
               IF (hier.parentID > 0)
                  tempHier := ASK root Child("RBDHier", hier.parentID);
                  IF (tempHier <> NILOBJ)
                     IF tempHier.childGroup.Includes(hier);
                        ASK tempHier.childGroup TO RemoveThis(hier);
                     END IF;
                  END IF;
               END IF;
               IF hier.level = deepestLevel
                  deepestLevel := 0;
                  FOREACH tempHier IN hierGroup
                     IF tempHier.level > deepestLevel
                        deepestLevel := tempHier.level;
                     END IF;
                  END FOREACH;
               END IF;
               ASK hierGroup TO RemoveThis(hier);
               DEC(nextId);
               DISPOSE(hier);
               totalHiers := totalHiers - 1;
               totalObjects := totalObjects - 1;
               typeOfCursor := nilC;
               IF totalObjects = 0
                  ASK menubar TO Enable(5);
                  ASK menubar TO Enable(6);
                  ASK menubar TO Enable(15);
               ELSE
                  ASK menubar TO Enable2Thru6;
               END IF;
               scrollScreen := FALSE; 
               buttItem := ASK menuTool Descendant("HierButton", 912);
               ASK buttItem TO SetSelected(FALSE);
               ASK buttItem TO Draw;
            ELSIF Button = 0 {dropping new hier on screen}
               IF buttondown
                  Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                  scrollScreen := FALSE;
                  displayX := displayX + curOffX;
                  displayY := displayY + curOffY;
                  Transform(baseroot, root, displayX, displayY, displayX, displayY);
                  displayX := displayX + 0.02;
                  displayY := displayY - 0.072;
                  displayX := FLOAT(TRUNC(displayX));
                  displayY := FLOAT(TRUNC(displayY)+1);
                  IF displayY > 80.
                     displayY := 80.;
                  END IF;
                  IF displayX > 119.
                     displayX := 119.;
                  END IF;
                  SetCursor(NILOBJ);
                  typeOfCursor := dialogC;
                  RemoveThisGraphic(hier);
                  ASK root TO AddAfterGraphic(defaultBlock,hier);
                  ASK hier TO SetScaling(1.,1.);
                  ASK hier TO DisplayAt(displayX, displayY);
                  ASK hier TO Draw;
                  NEW(hierBox);
                  ASK hierBox TO LoadFromLibrary(dialogs, "HierBox");
                  AddGraphic(hierBox);
                  cancelButt := ASK hierBox Descendant("CancelButton", 0);
                  IF (collapsing OR importing)
                     ASK cancelButt TO SetHidden(TRUE);
                  END IF;
                  ASK hierBox TO Draw;
                  ASK hierBox TO ReceiveData(hier, cancelled);
                  DISPOSE(hierBox);
                  IF cancelled
                     IF (hier.parentID > 0)
                        tempHier := ASK root Child("RBDHier", hier.parentID);
                        IF (tempHier <> NILOBJ)
                           IF tempHier.childGroup.Includes(hier);
                              ASK tempHier.childGroup TO RemoveThis(hier);
                           END IF;
                        END IF;
                     END IF;
                     IF hier.level = deepestLevel
                        deepestLevel := 0;
                        FOREACH tempHier IN hierGroup
                           IF tempHier.level > deepestLevel
                              deepestLevel := tempHier.level;
                           END IF;
                        END FOREACH;
                     END IF;
                     ASK hierGroup TO RemoveThis(hier);
                     DEC(nextId);
                     DISPOSE(hier);
                     totalHiers := totalHiers - 1;
                     totalObjects := totalObjects - 1;
                  ELSE
                     AddWindow(hier.Id);
                     ASK menubar TO Disable(17);
                     analViewAvail := FALSE;
                     configFrozen := FALSE;
                  END IF;
                  IF ((NOT collapsing) AND (NOT importing))
                     AddHier;
                  ELSIF collapsing
                     SetCursor(NILOBJ);
                     typeOfCursor := nilC;
                     {temporarily set activeWindow to new hier's id so can zoomfit elements within
                      hier without having to change windows to it}
                     tempaw := activeWindow
                     activeWindow := hier.Id
                     outNode := ASK root Child("RBDNode", hier.outID);
                     {Adjust outnode}
                     ASK outNode TO DisplayAt(outNode.xPosition+outxdiff, outNode.yPosition);
                     ZoomFit;
                     activeWindow := tempaw;
                     collapsing := FALSE;
                 ELSE 
                     collapsing := FALSE;
                     importing := FALSE;                    
                     SetCursor(NILOBJ);
                     typeOfCursor := nilC;
                  END IF;
               END IF;
            END IF;
         WHEN nodeC:
            IF Button = 2
               SetCursor(NILOBJ);
               IF (node.parentID > 0)
                  tempHier := ASK root Child("RBDHier", node.parentID);
                  IF (tempHier <> NILOBJ)
                     IF tempHier.childGroup.Includes(node);
                        ASK tempHier.childGroup TO RemoveThis(node);
                     END IF;
                  END IF;
               END IF;
               ASK nodeGroup TO RemoveThis(node);
               DEC(nextId);
               DISPOSE(node);
               totalNodes := totalNodes - 1;
               totalObjects := totalObjects - 1;
               typeOfCursor := nilC;
               IF totalObjects = 0
                  ASK menubar TO Enable(5);
                  ASK menubar TO Enable(6);
                  ASK menubar TO Enable(15);
               ELSE
                  ASK menubar TO Enable2Thru6;
               END IF;
               scrollScreen := FALSE; 
               buttItem := ASK menuTool Descendant("NodeButton", 909);
               ASK buttItem TO SetSelected(FALSE);
               ASK buttItem TO Draw;
            ELSIF Button = 0
               IF buttondown
                  Transform(NILOBJ, baseroot, ClickX, ClickY, displayX, displayY);
                  scrollScreen := FALSE;
                  displayX := displayX + curOffX;
                  displayY := displayY + curOffY;
                  Transform(baseroot, root, displayX, displayY, displayX, displayY);
                  displayX := displayX + 0.02;
                  displayY := displayY - 0.072;
                  displayX := FLOAT(TRUNC(displayX));
                  displayY := FLOAT(TRUNC(displayY)+1);
                  IF displayY > 80.
                     displayY := 80.;
                  END IF;
                  IF displayX > 119.
                     displayX := 119.;
                  END IF;
                  SetCursor(NILOBJ);
                  typeOfCursor := dialogC;
                  RemoveThisGraphic(node);
                  ASK root TO AddAfterGraphic(defaultBlock,node);
                  ASK node TO SetScaling(1.,1.);
                  ASK node TO DisplayAt(displayX, displayY);
                  ASK node TO Draw;
                  cancelled := FALSE;
                  NEW(nodeBox);
                  ASK nodeBox TO LoadFromLibrary(dialogs, "NodePropBox");
                  AddGraphic(nodeBox);
                  ASK nodeBox TO Draw;
                  ASK nodeBox TO ReceiveData(cancelled, TRUE, node,4);
                  DISPOSE(nodeBox);
                  ASK node TO Draw;
                  IF cancelled
                     IF (node.parentID > 0)
                        tempHier := ASK root Child("RBDHier", node.parentID);
                        IF (tempHier <> NILOBJ)
                           IF tempHier.childGroup.Includes(node);
                              ASK tempHier.childGroup TO RemoveThis(node);
                           END IF;
                        END IF;
                     END IF;
                     ASK nodeGroup TO RemoveThis(node);
                     DEC(nextId);
                     DISPOSE(node);
                     totalNodes := totalNodes - 1;
                     totalObjects := totalObjects - 1;
                  ELSE
                     ASK menubar TO Disable(17);
                     analViewAvail := FALSE;
                     configFrozen := FALSE;
                  END IF;
                  AddNode;
               END IF;
            END IF;
         WHEN connectC:
            IF Button = 0
               IF buttondown
                  IF draggingblock
                     block := ASK root Child("RBDBlock", fromBlockId);
                     ASK block TO IncLink(MINUSOUTOF);
                     draggingblock := FALSE;
                  ELSIF draggingevent
                     event := ASK root Child("RBDEvent", fromBlockId);
                     ASK event TO IncLink(MINUSOUTOF);
                     draggingevent := FALSE;
                  ELSIF draggingnode
                     node := ASK root Child("RBDNode", fromBlockId);
                     ASK node TO IncLink(MINUSOUTOF);
                     draggingnode := FALSE;
                  ELSIF dragginghier
                     hier := ASK root Child("RBDHier", fromBlockId);
                     ASK hier TO IncLink(MINUSOUTOF);
                     dragginghier := FALSE;
                  END IF;
                  SetCursor(NILOBJ);  
                  selectedObj := ASK root TO Select(x,y);
                  IF selectedObj <> NILOBJ
                     IF OBJTYPENAME(selectedObj) = "RBDBlockObj"
                        block := RBDBlockObj(selectedObj);
                        ASK block TO IncLink(OUTOF);
                        fromBlockId := block.Id;
                        fromBlockRef := block.ReferenceName;
                        scrollScreen := TRUE; 
                        SetCursorOffset(0., 0.);
                        SetCursor(linkCursor);  
                        linkStartX := block.Translation.x + 0.26;
                        linkStartY := block.Translation.y - 0.21;
                        Transform(root, baseroot, linkStartX, linkStartY, tempX, tempY);
                        ASK linkCursor TO SetAnchor(tempX,tempY);
                        draggingblock := TRUE;
                     ELSIF OBJTYPENAME(selectedObj) = "RBDEventObj"
                        event := RBDEventObj(selectedObj);
                        ASK event TO IncLink(OUTOF);
                        fromBlockId := event.Id;
                        fromBlockRef := event.ReferenceName;
                        scrollScreen := TRUE; 
                        SetCursorOffset(0., 0.);
                        SetCursor(linkCursor);  
                        linkStartX := event.Translation.x + 0.26;
                        linkStartY := event.Translation.y - 0.21;
                        Transform(root, baseroot, linkStartX, linkStartY, tempX, tempY);
                        ASK linkCursor TO SetAnchor(tempX,tempY);
                        draggingevent := TRUE;
                     ELSIF OBJTYPENAME(selectedObj) = "RBDHierObj"
                        hier := RBDHierObj(selectedObj);
                        ASK hier TO IncLink(OUTOF);
                        fromBlockId := hier.Id;
                        fromBlockRef := hier.ReferenceName;
                        scrollScreen := TRUE; 
                        SetCursorOffset(0., 0.);
                        SetCursor(linkCursor);  
                        linkStartX := hier.Translation.x + 0.56;
                        linkStartY := hier.Translation.y - 0.21;
                        Transform(root, baseroot, linkStartX, linkStartY, tempX, tempY);
                        ASK linkCursor TO SetAnchor(tempX,tempY);
                        dragginghier := TRUE; 
                     ELSIF OBJTYPENAME(selectedObj) = "RBDNodeObj"
                        node := RBDNodeObj(selectedObj);
                        ASK node TO IncLink(OUTOF);
                        ASK link TO CheckValidLink(selectedObj, validLink, errorText, OUTOF);
                        IF validLink
                           fromBlockId := node.Id;
                           fromBlockRef := node.ReferenceName;
                           scrollScreen := TRUE; 
                           SetCursorOffset(0., 0.);
                           SetCursor(linkCursor);
                           linkStartX := node.Translation.x + 0.26;
                           linkStartY := node.Translation.y - 0.21;
                           Transform(root, baseroot, linkStartX, linkStartY, tempX, tempY);
                           ASK linkCursor TO SetAnchor(tempX,tempY);
                           draggingnode := TRUE;
                        ELSE
                           typeOfCursor := dialogC;
                           ASK node TO IncLink(MINUSOUTOF);
                           SetCursor(NILOBJ);
                           IF link.parentID > 0
                              hier := ASK root Child("RBDHier", link.parentID);
                              ASK hier.childGroup TO RemoveThis(link);
                           END IF;
                           ASK linkGroup TO RemoveThis(link);
                           DISPOSE(link);
                           totalLinks := totalLinks - 1;
                           NEW(message, 1..1);
                           message[1] := errorText;
                           result := SendAlert(message, FALSE, FALSE, TRUE);
                           DISPOSE(message);
                           AddConnector;
                        END IF;
                     ELSE
                        SetCursor(NILOBJ);
                        IF link.parentID > 0
                           hier := ASK root Child("RBDHier", link.parentID);
                           ASK hier.childGroup TO RemoveThis(link);
                        END IF;
                        ASK linkGroup TO RemoveThis(link);
                        DISPOSE(link);
                        totalLinks := totalLinks - 1;
                        AddConnector;
                     END IF;
                  END IF;
               ELSE
                  IF draggingblock OR draggingevent OR draggingnode OR dragginghier
                     selectedObj := ASK root TO Select(x,y);
                     IF selectedObj <> NILOBJ
                        startToEnd := FALSE;
                        FOREACH tempLink IN linkGroup
                           IF (OBJTYPENAME(selectedObj) = (tempLink.connectFRef+"Obj")) AND
                              (selectedObj.Id = tempLink.connectFromId) AND
                              (tempLink.connectTRef = fromBlockRef) AND
                              (tempLink.connectToId = fromBlockId)
                              badLink := TRUE;
                              EXIT;
                           ELSIF (OBJTYPENAME(selectedObj) = (tempLink.connectTRef+"Obj")) AND
                              (selectedObj.Id = tempLink.connectToId) AND
                              (tempLink.connectFRef = fromBlockRef) AND
                              (tempLink.connectFromId = fromBlockId)
                              badLink := TRUE;
                              EXIT;
                           END IF;
                        END FOREACH;
                        IF draggingnode
                           IF (fromBlockId = startId) AND (OBJTYPENAME(selectedObj) = "RBDNodeObj")
                              tempNode := RBDNodeObj(selectedObj);
                              IF tempNode.typeNode = 3
                                 badLink := TRUE;
                                 startToEnd := TRUE;
                              END IF;
                           END IF;
                        END IF;
                        IF NOT badLink
                           IF OBJTYPENAME(selectedObj) = "RBDBlockObj"
                              block := RBDBlockObj(selectedObj);
                              IF (fromBlockId <> block.Id) OR (fromBlockRef <> block.ReferenceName)
                                 ASK block TO IncLink(INTO);
                                 ASK link TO SetConnections(fromBlockId, block.Id, fromBlockRef, 
                                                            block.ReferenceName);
                                 ASK link TO CheckValidLink(selectedObj, validLink, errorText, INTO);
                                 IF validLink
                                    SetCursor(NILOBJ);
                                    NEW(linkPoints, 1..6);
                                    arrowhead(link.connectTRef, linkStartX, linkStartY, block.Translation.x + 0.26,
                                              block.Translation.y - 0.21, linkPoints[2].x,
                                              linkPoints[2].y, linkPoints[3].x, linkPoints[3].y,
                                              linkPoints[4].x, linkPoints[4].y);
                                    linkPoints[5].x := linkPoints[2].x;
{                                  3 \        }
{points 5 and 2 are the same:  1---+--2,5--6  }
{                                  4 /        }
                                    linkPoints[5].y := linkPoints[2].y;
                                    linkEndX := block.Translation.x + 0.26;
                                    linkEndY := block.Translation.y - 0.21;
                                    linkPoints[1].x := linkStartX;
                                    linkPoints[1].y := linkStartY;
                                    linkPoints[6].x := linkEndX;
                                    linkPoints[6].y := linkEndY;
                                    ASK link TO SetPoints(linkPoints);
                                    NEW(linkInts,1..4);
                                    NEW(linkReals,1..3);
                                    linkInts[1] := 50;
                                    linkInts[2] := 50;
                                    linkInts[3] := -2;
                                    linkInts[4] := -1;
                                    linkReals[1] := 1.;
                                    linkReals[2] := 0.;
                                    linkReals[3] := 0.;
                                    ASK link TO LinkInit(linkInts,linkReals);
                                    ASK link TO Draw;
                                    DISPOSE(linkPoints);
                                    DISPOSE(linkInts);
                                    DISPOSE(linkReals);
                                    somethingChanged := TRUE;
                                 ELSE
                                    ASK block TO IncLink(MINUSINTO);
                                    IF fromBlockRef = "RBDBlock"
                                       block := ASK root Child("RBDBlock", fromBlockId);
                                       ASK block TO IncLink(MINUSOUTOF);
                                    ELSIF fromBlockRef = "RBDEvent"
                                       event := ASK root Child("RBDEvent", fromBlockId);
                                       ASK event TO IncLink(MINUSOUTOF);
                                    ELSIF fromBlockRef = "RBDHier"
                                       hier := ASK root Child("RBDHier", fromBlockId);
                                       ASK hier TO IncLink(MINUSOUTOF);
                                    ELSIF fromBlockRef = "RBDNode"
                                       node := ASK root Child("RBDNode", fromBlockId);
                                       ASK node TO IncLink(MINUSOUTOF);
                                    END IF;
                                    typeOfCursor := dialogC;
                                    SetCursor(NILOBJ);
                                    IF link.parentID > 0
                                       hier := ASK root Child("RBDHier", link.parentID);
                                       IF hier <> NILOBJ
                                          IF hier.childGroup.Includes(link)
                                             ASK hier.childGroup TO RemoveThis(link);
                                          END IF;
                                       END IF;
                                    END IF;
                                    ASK linkGroup TO RemoveThis(link);
                                    DISPOSE(link); {***}
                                    totalLinks := totalLinks - 1;
                                    NEW(message, 1..1);
                                    message[1] := errorText;
                                    result := SendAlert(message, FALSE, FALSE, TRUE);
                                    DISPOSE(message);
                                 END IF;
                              ELSE
                                 block := ASK root Child("RBDBlock", fromBlockId);
                                 ASK block TO IncLink(MINUSOUTOF);
                                 SetCursor(NILOBJ);
                                 IF link.parentID > 0
                                    hier := ASK root Child("RBDHier", link.parentID);
                                    ASK hier.childGroup TO RemoveThis(link);
                                 END IF;
                                 ASK linkGroup TO RemoveThis(link);
                                 DISPOSE(link);
                                 totalLinks := totalLinks - 1;
                              END IF;                                                                 
                           ELSIF OBJTYPENAME(selectedObj) = "RBDEventObj"
                              event := RBDEventObj(selectedObj);
                              IF (fromBlockId <> event.Id) OR (fromBlockRef <> event.ReferenceName)
                                 ASK event TO IncLink(INTO);
                                 ASK link TO SetConnections(fromBlockId, event.Id, fromBlockRef, 
                                                            event.ReferenceName);
                                 ASK link TO CheckValidLink(selectedObj, validLink, errorText, INTO);
                                 IF validLink
                                    SetCursor(NILOBJ);
                                    NEW(linkPoints, 1..6);
                                    arrowhead(link.connectTRef, linkStartX, linkStartY, event.Translation.x + 0.26,
                                              event.Translation.y - 0.21, linkPoints[2].x,
                                              linkPoints[2].y, linkPoints[3].x, linkPoints[3].y,
                                              linkPoints[4].x, linkPoints[4].y);
                                    linkPoints[5].x := linkPoints[2].x;
{                                  3 \        }
{points 5 and 2 are the same:  1---+--2,5--6  }
{                                  4 /        }
                                    linkPoints[5].y := linkPoints[2].y;
                                    linkEndX := event.Translation.x + 0.26;
                                    linkEndY := event.Translation.y - 0.21;
                                    linkPoints[1].x := linkStartX;
                                    linkPoints[1].y := linkStartY;
                                    linkPoints[6].x := linkEndX;
                                    linkPoints[6].y := linkEndY;
                                    ASK link TO SetPoints(linkPoints);
                                    NEW(linkInts,1..4);
                                    NEW(linkReals,1..3);
                                    linkInts[1] := 50;
                                    linkInts[2] := 50;
                                    linkInts[3] := -2;
                                    linkInts[4] := -1;
                                    linkReals[1] := 1.;
                                    linkReals[2] := 0.;
                                    linkReals[3] := 0.;
                                    ASK link TO LinkInit(linkInts,linkReals);
                                    ASK link TO Draw;
                                    DISPOSE(linkPoints);
                                    DISPOSE(linkInts);
                                    DISPOSE(linkReals);
                                    somethingChanged := TRUE;
                                 ELSE
                                    ASK event TO IncLink(MINUSINTO);
                                    IF fromBlockRef = "RBDBlock"
                                       block := ASK root Child("RBDBlock", fromBlockId);
                                       ASK block TO IncLink(MINUSOUTOF);
                                    ELSIF fromBlockRef = "RBDEvent"
                                       event := ASK root Child("RBDEvent", fromBlockId);
                                       ASK event TO IncLink(MINUSOUTOF);
                                    ELSIF fromBlockRef = "RBDHier"
                                       hier := ASK root Child("RBDHier", fromBlockId);
                                       ASK hier TO IncLink(MINUSOUTOF);
                                    ELSIF fromBlockRef = "RBDNode"
                                       node := ASK root Child("RBDNode", fromBlockId);
                                       ASK node TO IncLink(MINUSOUTOF);
                                    END IF;
                                    typeOfCursor := dialogC;
                                    SetCursor(NILOBJ);
                                    IF link.parentID > 0
                                       hier := ASK root Child("RBDHier", link.parentID);
                                       IF (hier <> NILOBJ)
                                          ASK hier.childGroup TO RemoveThis(link);
                                       END IF;
                                    END IF;
                                    ASK linkGroup TO RemoveThis(link);
                                    DISPOSE(link); {***}
                                    totalLinks := totalLinks - 1;
                                    NEW(message, 1..1);
                                    message[1] := errorText;
                                    result := SendAlert(message, FALSE, FALSE, TRUE);
                                    DISPOSE(message);
                                 END IF;
                              ELSE
                                 event := ASK root Child("RBDEvent", fromBlockId);
                                 ASK event TO IncLink(MINUSOUTOF);
                                 SetCursor(NILOBJ);
                                 IF link.parentID > 0
                                    hier := ASK root Child("RBDHier", link.parentID);
                                    ASK hier.childGroup TO RemoveThis(link);
                                 END IF;
                                 ASK linkGroup TO RemoveThis(link);
                                 DISPOSE(link);
                                 totalLinks := totalLinks - 1;
                              END IF;                                                                 
                           ELSIF OBJTYPENAME(selectedObj) = "RBDHierObj"
                              hier := RBDHierObj(selectedObj);
                              IF (fromBlockId <> hier.Id) OR (fromBlockRef <> hier.ReferenceName)
                                 ASK hier TO IncLink(INTO);
                                 ASK link TO SetConnections(fromBlockId, hier.Id, fromBlockRef, hier.ReferenceName);
                                 ASK link TO CheckValidLink(selectedObj, validLink, errorText, INTO);
                                 IF validLink
                                    SetCursor(NILOBJ);
                                    NEW(linkPoints, 1..6);
                                    arrowhead(link.connectTRef, linkStartX, linkStartY, hier.Translation.x + 0.56,
                                              hier.Translation.y - 0.21, linkPoints[2].x,
                                              linkPoints[2].y, linkPoints[3].x, linkPoints[3].y,
                                              linkPoints[4].x, linkPoints[4].y);
                                    linkPoints[5].x := linkPoints[2].x;
{                                  3 \        }
{points 5 and 2 are the same:  1---+--2,5--6  }
{                                  4 /        }
                                    linkPoints[5].y := linkPoints[2].y;
                                    linkEndX := hier.Translation.x + 0.56;
                                    linkEndY := hier.Translation.y - 0.21;
                                    linkPoints[1].x := linkStartX;
                                    linkPoints[1].y := linkStartY;
                                    linkPoints[6].x := linkEndX;
                                    linkPoints[6].y := linkEndY;
                                    ASK link TO SetPoints(linkPoints);
                                    NEW(linkInts,1..4);
                                    NEW(linkReals,1..3);
                                    linkInts[1] := 50;
                                    linkInts[2] := 50;
                                    linkInts[3] := -2;
                                    linkInts[4] := -1;
                                    linkReals[1] := 1.;
                                    linkReals[2] := 0.;
                                    linkReals[3] := 0.;
                                    ASK link TO LinkInit(linkInts,linkReals);
                                    ASK link TO Draw;
                                    DISPOSE(linkPoints);
                                    DISPOSE(linkInts);
                                    DISPOSE(linkReals);
                                    somethingChanged := TRUE;
                                 ELSE
                                    ASK hier TO IncLink(MINUSINTO);
                                    IF fromBlockRef = "RBDBlock"
                                       block := ASK root Child("RBDBlock", fromBlockId);
                                       ASK block TO IncLink(MINUSOUTOF);
                                    ELSIF fromBlockRef = "RBDEvent"
                                       event := ASK root Child("RBDEvent", fromBlockId);
                                       ASK event TO IncLink(MINUSOUTOF);
                                    ELSIF fromBlockRef = "RBDHier"
                                       hier := ASK root Child("RBDHier", fromBlockId);
                                       ASK hier TO IncLink(MINUSOUTOF);
                                    ELSIF fromBlockRef = "RBDNode"
                                       node := ASK root Child("RBDNode", fromBlockId);
                                       ASK node TO IncLink(MINUSOUTOF);
                                    END IF;
                                    typeOfCursor := dialogC;
                                    SetCursor(NILOBJ);
                                    IF link.parentID > 0
                                       hier := ASK root Child("RBDHier", link.parentID);
                                       IF hier <> NILOBJ
                                          IF hier.childGroup.Includes(link)
                                             ASK hier.childGroup TO RemoveThis(link);
                                          END IF;
                                       END IF;
                                    END IF;
                                    ASK linkGroup TO RemoveThis(link);
                                    DISPOSE(link); {***}
                                    totalLinks := totalLinks - 1;
                                    NEW(message, 1..1);
                                    message[1] := errorText;
                                    result := SendAlert(message, FALSE, FALSE, TRUE);
                                    DISPOSE(message);
                                 END IF;
                              ELSE
                                 hier := ASK root Child("RBDHier", fromBlockId);
                                 ASK hier TO IncLink(MINUSOUTOF);
                                 SetCursor(NILOBJ);
                                 IF link.parentID > 0
                                    hier := ASK root Child("RBDHier", link.parentID);
                                    IF hier <> NILOBJ
                                       IF hier.childGroup.Includes(link)
                                          ASK hier.childGroup TO RemoveThis(link);
                                       END IF;
                                    END IF;
                                 END IF;
                                 ASK linkGroup TO RemoveThis(link);
                                 DISPOSE(link); {***}
                                 totalLinks := totalLinks - 1;
                              END IF;                                                                 
                           ELSIF OBJTYPENAME(selectedObj) = "RBDNodeObj"
                              node := RBDNodeObj(selectedObj);
                              IF (fromBlockId <> node.Id) OR (fromBlockRef <> node.ReferenceName)
                                 ASK node TO IncLink(INTO);
                                 ASK link TO SetConnections(fromBlockId, node.Id, fromBlockRef,
                                                            node.ReferenceName);
                                 ASK link TO CheckValidLink(selectedObj, validLink, errorText, INTO);
                                 IF validLink
                                    IF fromBlockRef = "RBDBlock"
                                       block := ASK root Child("RBDBlock", fromBlockId);
                                       ASK block TO SetConnectToNode(TRUE);
                                    END IF;
                                    SetCursor(NILOBJ);
                                    NEW(linkPoints, 1..6);
                                    arrowhead(link.connectTRef, linkStartX, linkStartY, node.Translation.x + 0.26,
                                              node.Translation.y - 0.21, linkPoints[2].x,
                                              linkPoints[2].y, linkPoints[3].x, linkPoints[3].y,
                                              linkPoints[4].x, linkPoints[4].y);
                                    linkPoints[5].x := linkPoints[2].x;
                                    linkPoints[5].y := linkPoints[2].y;
                                    linkEndX := node.Translation.x + 0.26;
                                    linkEndY := node.Translation.y - 0.21;
                                    linkPoints[1].x := linkStartX;
                                    linkPoints[1].y := linkStartY;
                                    linkPoints[6].x := linkEndX;
                                    linkPoints[6].y := linkEndY;
                                    ASK link TO SetPoints(linkPoints);
                                    NEW(linkInts,1..4);
                                    NEW(linkReals,1..3);
                                    linkInts[1] := 50;
                                    linkInts[2] := 50;
                                    linkInts[3] := -2;
                                    linkInts[4] := -1;
                                    linkReals[1] := 1.;
                                    linkReals[2] := 0.;
                                    linkReals[3] := 0.;
                                    ASK link TO LinkInit(linkInts,linkReals);
                                    ASK link TO Draw;
                                    DISPOSE(linkInts);
                                    DISPOSE(linkReals);
                                    DISPOSE(linkPoints);
                                    somethingChanged := TRUE;
                                    label := ASK node Child("RBDNodeKofN", 0);
                                    ASK label TO SetText("");
                                    ASK node TO SetAoDoR("0", "0", "0");
                                    IF node.goodPaths = 0
                                       ASK node TO SetGoodPaths(1);
                                    END IF;
                                    ASK node TO Draw;
                                 ELSE
                                    typeOfCursor := dialogC;
                                    ASK node TO IncLink(MINUSINTO);
                                    SetCursor(NILOBJ);
                                    IF link.parentID > 0
                                       hier := ASK root Child("RBDHier", link.parentID);
                                       ASK hier.childGroup TO RemoveThis(link);
                                    END IF;
                                    ASK linkGroup TO RemoveThis(link);
                                    DISPOSE(link);
                                    totalLinks := totalLinks - 1;
                                    IF fromBlockRef = "RBDBlock"
                                       block := ASK root Child("RBDBlock", fromBlockId);
                                       ASK block TO IncLink(MINUSOUTOF);
                                    ELSIF fromBlockRef = "RBDEvent"
                                       event := ASK root Child("RBDEvent", fromBlockId);
                                       ASK event TO IncLink(MINUSOUTOF);
                                    ELSIF fromBlockRef = "RBDNode"
                                       node := ASK root Child("RBDNode", fromBlockId);
                                       ASK node TO IncLink(MINUSOUTOF);
                                    ELSIF fromBlockRef = "RBDHier"
                                       hier := ASK root Child("RBDHier", fromBlockId);
                                       ASK hier TO IncLink(MINUSOUTOF);
                                    END IF;
                                    NEW(message, 1..1);
                                    message[1] := errorText;
                                    result := SendAlert(message, FALSE, FALSE, TRUE);
                                    DISPOSE(message);
                                 END IF;
                              ELSE
                                 node := ASK root Child("RBDNode", fromBlockId);
                                 ASK node TO IncLink(MINUSOUTOF);
                                 SetCursor(NILOBJ);
                                 IF link.parentID > 0
                                    hier := ASK root Child("RBDHier", link.parentID);
                                    ASK hier.childGroup TO RemoveThis(link);
                                 END IF;
                                 ASK linkGroup TO RemoveThis(link);
                                 DISPOSE(link);
                                 totalLinks := totalLinks - 1;
                              END IF;
                           ELSE
                              IF draggingblock
                                 block := ASK root Child("RBDBlock", fromBlockId);
                                 ASK block TO IncLink(MINUSOUTOF);
                              ELSIF draggingevent
                                 event := ASK root Child("RBDEvent", fromBlockId);
                                 ASK event TO IncLink(MINUSOUTOF);
                              ELSIF dragginghier
                                 hier := ASK root Child("RBDHier", fromBlockId);
                                 ASK hier TO IncLink(MINUSOUTOF);
                              ELSIF draggingnode
                                 node := ASK root Child("RBDNode", fromBlockId);
                                 ASK node TO IncLink(MINUSOUTOF);
                              END IF;
                              SetCursor(NILOBJ);
                              IF link.parentID > 0
                                 hier := ASK root Child("RBDHier", link.parentID);
                                 ASK hier.childGroup TO RemoveThis(link);
                              END IF;
                              ASK linkGroup TO RemoveThis(link);
                              DISPOSE(link);
                              totalLinks := totalLinks - 1;
                           END IF;
                        ELSE
                           typeOfCursor := dialogC;
                           SetCursor(NILOBJ);
                           IF link.parentID > 0
                              hier := ASK root Child("RBDHier", link.parentID);
                              IF hier <> NILOBJ
                                 IF hier.childGroup.Includes(link)
                                    ASK hier.childGroup TO RemoveThis(link);
                                 END IF;
                              END IF;
                           END IF;
                           ASK linkGroup TO RemoveThis(link);
                           DISPOSE(link); {***}
                           totalLinks := totalLinks - 1;
                           IF fromBlockRef = "RBDBlock"
                              block := ASK root Child("RBDBlock", fromBlockId);
                              ASK block TO IncLink(MINUSOUTOF);
                           ELSIF fromBlockRef = "RBDEvent"
                              event := ASK root Child("RBDEvent", fromBlockId);
                              ASK event TO IncLink(MINUSOUTOF);
                           ELSIF fromBlockRef = "RBDHier"
                              hier := ASK root Child("RBDHier", fromBlockId);
                              ASK hier TO IncLink(MINUSOUTOF);
                           ELSIF fromBlockRef = "RBDNode"
                              node := ASK root Child("RBDNode", fromBlockId);
                              ASK node TO IncLink(MINUSOUTOF);
                           END IF;
                           NEW(message, 1..1);
                           IF startToEnd
                              message[1] := "The start marker cannot be connected to the end marker!     ";
                           ELSE
                              message[1] := "Only one link is allowed between the same two objects!     ";
                           END IF;
                           result := SendAlert(message, FALSE, FALSE, TRUE);
                           DISPOSE(message);
                        END IF;
                        draggingblock := FALSE;
                        draggingevent := FALSE;
                        dragginghier := FALSE;
                        draggingnode := FALSE;
                        ASK menubar TO Disable(17);
                        analViewAvail := FALSE;
                        configFrozen := FALSE;
                        AddConnector;
                     END IF;
                     scrollScreen := FALSE; 
                  END IF;
               END IF;
            ELSIF Button = 2
               IF draggingblock
                  block := ASK root Child("RBDBlock", fromBlockId);
                  ASK block TO IncLink(MINUSOUTOF);
               ELSIF draggingevent
                  event := ASK root Child("RBDEvent", fromBlockId);
                  ASK event TO IncLink(MINUSOUTOF);
               ELSIF dragginghier
                  hier := ASK root Child("RBDHier", fromBlockId);
                  ASK hier TO IncLink(MINUSOUTOF); 
               ELSIF draggingnode
                  node := ASK root Child("RBDNode", fromBlockId);
                  ASK node TO IncLink(MINUSOUTOF);
               END IF;
               SetCursor(NILOBJ);
               DISPOSE(linkCursor);
               IF link.parentID > 0
                  hier := ASK root Child("RBDHier", link.parentID);
                  ASK hier.childGroup TO RemoveThis(link);
               END IF;
               ASK linkGroup TO RemoveThis(link);
               DEC(nextLinkId);
               DISPOSE(link);
               totalLinks := totalLinks - 1;
               typeOfCursor := nilC;
               linkText := Descendant("LinkText",851);
               DISPOSE(linkText);
               DISPOSE(linkMessage);
               linkSelected := FALSE;
               linkMsgExists := FALSE;
               ASK menubar TO Enable2Thru6;
               draggingblock := FALSE;
               draggingevent := FALSE;
               dragginghier  := FALSE;
               draggingnode  := FALSE;
               buttItem := ASK menuTool Descendant("LinkButton",910);
               ASK buttItem TO SetSelected(FALSE);
               ASK buttItem TO Draw;
            END IF;
         WHEN dialogC:
         WHEN autoC:
         WHEN fevC:
            IF Button = 0 {left click}
               IF buttondown
                  changeView := FALSE;
                  selectedObj := ASK root TO Select(x,y);
                  IF (selectedObj <> NILOBJ) AND (OBJTYPENAME(selectedObj) <> "GridObj")
                     IF (OBJTYPENAME(selectedObj) = "RBDBlockObj")
                        block := RBDBlockObj(selectedObj);
                        IF block.FevDepMode
                           ASK block TO SetIgnoreDep(TRUE);  {Chuck 9/6/05}
                           IF (block.opStatus=Running)                           
                              IF (block.DepFailPerc > 0.)
                              ASK block TO ChangeBlockState(Repairing,block.activeStatus,"");
                                 ShowStatus(4, "Repairing");
                              ELSIF (block.DepPMPerc >0.) 
                                 ASK block TO ChangeBlockState(PM,block.activeStatus,"");
                                 ShowStatus(4, "PM");
                              ELSIF (block.DepIdlePerc>0.)
                                 ASK block TO ChangeBlockState(Idle,block.activeStatus,"");
                                 ShowStatus(4, "Idle");
                              END IF;
                           ELSIF  (block.opStatus=Repairing)
                              IF (block.DepPMPerc >0.) 
                                 ASK block TO ChangeBlockState(Running,block.activeStatus,"");
                                 ASK block TO ChangeBlockState(PM,block.activeStatus,"");
                                 ShowStatus(4, "PM");
                              ELSIF (block.DepIdlePerc>0.)
                                 ASK block TO ChangeBlockState(Idle,block.activeStatus,"");
                                 ShowStatus(4, "Idle");
                              ELSIF (block.DepNothingPerc > 0.)
                                 ASK block TO ChangeBlockState(Running,block.activeStatus,"");
                                 ShowStatus(4, "Operating");
                              END IF;
                           ELSIF  (block.opStatus=PM)
                              IF (block.DepIdlePerc>0.)
                                 ASK block TO ChangeBlockState(Running,block.activeStatus,"");
                                 ASK block TO ChangeBlockState(Idle,block.activeStatus,"");
                                 ShowStatus(4, "Idle");
                              ELSIF (block.DepNothingPerc>0.)
                                 ASK block TO ChangeBlockState(Running,block.activeStatus,"");
                                 ShowStatus(4, "Operating");
                              ELSIF (block.DepFailPerc > 0.)
                                 ASK block TO ChangeBlockState(Running,block.activeStatus,"");
                                 ASK block TO ChangeBlockState(Repairing,block.activeStatus,"");
                                 ShowStatus(4, "Repairing");
                              END IF;
                           ELSIF  (block.opStatus=Idle)
                              IF (block.DepNothingPerc >0.) 
                                 ASK block TO ChangeBlockState(Running,block.activeStatus,"");
                                 ShowStatus(4, "Operating");
                              ELSIF (block.DepFailPerc > 0.)
                                 ASK block TO ChangeBlockState(Running,block.activeStatus,"");
                                 ASK block TO ChangeBlockState(Repairing,block.activeStatus,"");
                                 ShowStatus(4, "Repairing");
                              ELSIF (block.DepPMPerc>0.)
                                 ASK block TO ChangeBlockState(Running,block.activeStatus,"");
                                 ASK block TO ChangeBlockState(PM,block.activeStatus,"");
                                 ShowStatus(4, "PM");
                              END IF;
                           END IF;
                           ASK block TO SetIgnoreDep(FALSE);  {Chuck 9/6/05}                           
                        ELSE
                           IF (FevDepGroup.numberIn>0)                          
                              FOREACH fevBlock IN FevDepGroup
                                 ASK fevBlock TO SetFevDepMode(FALSE);
                                 ASK FevDepGroup TO RemoveThis(fevBlock);
                              END FOREACH;
                           END IF;
                           IF (block.opStatus=Running)
                              critFactor := 1.0;
                              ASK block TO ChangeBlockState(Repairing,block.activeStatus,"");
                              ShowStatus(4, "Repairing");
                           ELSIF ((block.opStatus=Standby) AND (block.sbStress>0.0))
                              critFactor := 1.0;
                              ASK block TO ChangeBlockState(Repairing,block.activeStatus,"");
                              ShowStatus(4, "Repairing");
                           ELSIF (block.opStatus=Repairing)
                              ASK block TO ChangeBlockState(Running,block.activeStatus,"");
                              ShowStatus(4, "Operating");
                           ELSIF (block.opStatus = PM)
                              ASK block TO ChangeBlockState(Running,block.activeStatus,"");
                              ShowStatus(4, "Operating");
                           END IF;
                        END IF;  
                        IF password
                           ShowStatus(5, "Criticality = " + REALTOSTR(critFactor));
                        END IF;
                     ELSIF (OBJTYPENAME(selectedObj) = "RBDEventObj")        {chuck}
                           event := RBDEventObj(selectedObj);
                           IF (FevDepGroup.numberIn>0)                          
                              FOREACH fevBlock IN FevDepGroup
                                 ASK fevBlock TO SetFevDepMode(FALSE);
                                 ASK FevDepGroup TO RemoveThis(fevBlock);
                              END FOREACH;
                           END IF;
                           IF (event.opStatus=Success)
                              critFactor := 1.0;
                              ASK event TO ChangeEventState(Failure,event.activeStatus,"");
                              ShowStatus(4, "Failure");
                           ELSIF (event.opStatus=Failure)
                              ASK event TO ChangeEventState(Success,event.activeStatus,"");
                              ShowStatus(4, "Success");
                           END IF;
                     END IF;
                     CheckColds;
                     AnalyzeSystem;
                  END IF;
               END IF;
               IF SecondClick AND buttondown {double click}
                  changeView := FALSE;
                  IF OBJTYPENAME(selectedObj) = "RBDNodeObj"
                     tempNode := RBDNodeObj(selectedObj); 
                     IF tempNode.typeNode = 5
                        changeView := TRUE;
                     END IF;
                  END IF;
                  IF OBJTYPENAME(selectedObj) = "RBDHierObj"
                     tempHier := RBDHierObj(selectedObj); 
                     ChangeWindow(tempHier.Id, tempHier.level);
                  ELSIF changeView
                     tempHier := ASK root Descendant("RBDHier", activeWindow);
                     newId := tempHier.parentID;
                     IF newId > 0
                        tempHier := ASK root Descendant("RBDHier", newId);
                        ChangeWindow(newId, hierLevel-1);
                     ELSE
                        ChangeWindow(0, 0);
                     END IF;
                  END IF;
               END IF; 
            ELSIF (Button = 1) AND (NOT draggingblock) AND (NOT draggingevent) AND (NOT draggingnode) AND (NOT draggingStuff) AND (NOT drawingRed)
               IF buttondown 
                  ClearAllBlocks;
                  NEW(zoomRubber);
                  ASK zoomRubber TO SetWidth(2.);
                  ASK zoomRubber TO SetColor(Yellow);
                  ASK zoomRubber TO SetRatio (.7) ;
                  SetCursor(zoomRubber);
                  Transform(NILOBJ, baseroot, ClickX, ClickY, boxStartX, boxStartY);
                  ASK zoomRubber TO SetAnchor(boxStartX,boxStartY); 
                  Transform(NILOBJ, root, ClickX, ClickY, boxStartX, boxStartY);
                  ASK menubar TO Disable1Thru8;
                  drawingBox := TRUE;
                  scrollScreen := TRUE; 
               ELSE
                  IF drawingBox
                     SetCursor (NILOBJ) ;
                     ASK zoomRubber TO GetEndPoint(boxEndX, boxEndY);
                     Transform(baseroot,root, boxEndX, boxEndY, tempX, tempY);
                     IF (tempX > 120.) OR (tempY > 80.)
                        Transform(NILOBJ,root, boxEndX, boxEndY, boxEndX, boxEndY);
                     ELSE
                        Transform(baseroot,root, boxEndX, boxEndY, boxEndX, boxEndY);
                     END IF;
                     IF (boxStartX = boxEndX) OR (boxStartY = boxEndY) 
                        SetView(localZoomVal, (boxStartX-localZoomVal/2.), (boxStartY+localZoomVal*13.2/40.));
                     ELSE
                        localZoomVal := MAXOF(boxEndX,boxStartX) - MINOF(boxStartX,boxEndX);
                        localZoomVal := MAXOF(5.,localZoomVal);
                        IF localZoomVal <> cusZoomVal  {zoompossibleproblem}
                           cusZoomVal := localZoomVal;
                        ELSE
                           ASK zoomHier TO SetZoom(localZoomVal);
                        END IF;
                        SetView(localZoomVal, MINOF(boxStartX,boxEndX),MAXOF(boxStartY,boxEndY));
                     END IF;
                     DISPOSE(zoomRubber);
                     drawingBox := FALSE;
                     scrollScreen := FALSE;
                     IF totalObjects = 0
                        ASK menubar TO Enable(5);
                        ASK menubar TO Enable(6);
                        ASK menubar TO Enable(15);
                     ELSE
                        ASK menubar TO Enable2Thru6;
                     END IF;
                  ELSE
                     Transform(NILOBJ, root, ClickX, ClickY, tempX, tempY);
                     SetView(localZoomVal, (tempX-localZoomVal/2.), (tempY+localZoomVal*13.2/40.));
                  END IF;
               END IF; 
            ELSIF (Button = 2) AND buttondown {fev right click}
               selectedObj := ASK root TO Select(x,y);
               {IF ((selectedObj <> NILOBJ) AND (OBJTYPENAME(selectedObj) <> "GridObj"))}            
                  ShowRightClick(4);
               {END IF;}   
            END IF;
            {end of Fail Effect Mode}
         WHEN simC:
            Transform(NILOBJ, root, ClickX, ClickY, pointX, pointY);
            IF NOT nowSimulating
               changeView := FALSE;
               selectedObj := ASK root TO Select(x,y);
               IF OBJTYPENAME(selectedObj) = "RBDNodeObj"
                  tempNode := RBDNodeObj(selectedObj); 
                  IF tempNode.typeNode = 5
                     changeView := TRUE;
                  END IF;
               END IF;
               IF OBJTYPENAME(selectedObj) = "RBDHierObj"
                  IF SecondClick AND (NOT buttondown)
                     tempHier := RBDHierObj(selectedObj); 
                     ChangeWindow(tempHier.Id, tempHier.level);
                  END IF;
               ELSIF changeView
                  IF SecondClick AND (NOT buttondown)
                     tempHier := ASK root Descendant("RBDHier", activeWindow);
                     newId := tempHier.parentID;
                     IF newId > 0
                        tempHier := ASK root Descendant("RBDHier", newId);
                        ChangeWindow(newId, hierLevel-1);
                     ELSE
                        ChangeWindow(0, 0);
                     END IF;
                  END IF;
               ELSE   
                  IF NOT (((ClickX > 0.) AND (ClickX < 12000.)) AND (ClickY > 20400.))
                     changedZoom := TRUE;
                     IF (Button = 0) AND buttondown AND (localZoomVal >= 10.)
                        localZoomVal:= localZoomVal-5.;
                        IF localZoomVal <> cusZoomVal  {zoompossibleproblem (nickelzoomdefiniteproblem)}
                           cusZoomVal := localZoomVal;
                        ELSE
                           ASK zoomHier TO SetZoom(localZoomVal);
                        END IF;
                     ELSIF (Button = 2) AND buttondown AND (localZoomVal <= 90.)
                        localZoomVal:= localZoomVal+5.;  
                        IF localZoomVal <> cusZoomVal  {zoompossibleproblem (nickelzoomdefiniteproblem)}
                           cusZoomVal := localZoomVal;
                        ELSE
                           ASK zoomHier TO SetZoom(localZoomVal);
                        END IF;
                     END IF;
                     IF DisplayAoGraph AND buttondown
                        SetView(localZoomVal, (pointX-localZoomVal/2.),(pointY+localZoomVal/2.*6.9/20.));
                     ELSIF buttondown
                        SetView(localZoomVal, (pointX-localZoomVal/2.),(pointY+localZoomVal/2.*12.85/20.));
                     END IF;
                  END IF;
               END IF;
            ELSIF (winX > greenFace.Translation.x-(symbolScale*100.)) AND 
                  (winX < greenFace.Translation.x+(symbolScale*100.)) AND
                  (winY > greenFace.Translation.y-(symbolScale*100.)) AND
                  (winY < greenFace.Translation.y+(symbolScale*100.)) AND
               faceBoxOpen
               IF Button = 2 {right click on status image}
                  IF buttondown
                     IF symbolScale = .3
                        symbolScale := .45;
                        symbolOffset := 47.;
                     ELSIF symbolScale = .45
                        symbolScale := .6;
                        symbolOffset := 62.;
                     ELSIF symbolScale = .6
                        symbolScale := .75;
                        symbolOffset := 76.;
                     ELSE
                        symbolScale := .3;
                        symbolOffset := 34.;
                     END IF;
                  END IF;
                  ASK greenFace TO SetScaling(symbolScale,symbolScale);
                  ASK greenFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff)-widescreenOffset);      {cmc 5/10/07}
                  ASK yellowFace TO SetScaling(symbolScale,symbolScale);
                  ASK yellowFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff)-widescreenOffset);    {cmc 5/10/07}
                  ASK redFace TO SetScaling(symbolScale,symbolScale);
                  ASK redFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff)-widescreenOffset);       {cmc 5/10/07}
                  ASK greenFace TO Erase;
                  ASK yellowFace TO Erase;
                  ASK redFace TO Erase;   
                  ASK faceVisible TO Draw; 
               ELSIF Button = 0 {left click on status image}
                  IF buttondown
                     CASE systemImage
                        WHEN "Truck":
                           ASK greenFace TO LoadFromLibrary(images, "Airc_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Airc_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Airc_R");
                           systemImage := "Aircraft";
                        WHEN "Aircraft":
                           ASK greenFace TO LoadFromLibrary(images, "Arti_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Arti_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Arti_R");
                           systemImage := "Artillery";
                        WHEN "Artillery":
                           ASK greenFace TO LoadFromLibrary(images, "Bull_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Bull_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Bull_R");
                           systemImage := "Bull's_eye";
                        WHEN "Bull's_eye":
                           ASK greenFace TO LoadFromLibrary(images, "Circ_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Circ_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Circ_R");
                           systemImage := "Circuit_Card";
                        WHEN "Circuit_Card":
                           ASK greenFace TO LoadFromLibrary(images, "Comp_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Comp_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Comp_R");
                           systemImage := "Computer";
                        WHEN "Computer":
                           ASK greenFace TO LoadFromLibrary(images, "Equa_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Equa_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Equa_R");
                           systemImage := "Equalizer";
                        WHEN "Equalizer":
                           ASK greenFace TO LoadFromLibrary(images, "Face_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Face_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Face_R");
                           systemImage := "Face";
                        WHEN "Face":
                           ASK greenFace TO LoadFromLibrary(images, "Heli_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Heli_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Heli_R");
                           systemImage := "Helicopter";
                        WHEN "Helicopter":
                           ASK greenFace TO LoadFromLibrary(images, "Mete_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Mete_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Mete_R");
                           systemImage := "Meter";
                        WHEN "Meter":
                           ASK greenFace TO LoadFromLibrary(images, "Miss_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Miss_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Miss_R");
                           systemImage := "Missile";
                        WHEN "Missile":
                           ASK greenFace TO LoadFromLibrary(images, "Netw_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Netw_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Netw_R");
                           systemImage := "Network";
                        WHEN "Network":
                           ASK greenFace TO LoadFromLibrary(images, "Radi_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Radi_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Radi_R");
                           systemImage := "Radio";
                        WHEN "Radio":
                           ASK greenFace TO LoadFromLibrary(images, "Rado_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Rado_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Rado_R");
                           systemImage := "Radome";
                        WHEN "Radome":
                           ASK greenFace TO LoadFromLibrary(images, "Sate_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Sate_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Sate_R");
                           systemImage := "Satellite";
                        WHEN "Satellite":
                           ASK greenFace TO LoadFromLibrary(images, "Ship_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Ship_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Ship_R");
                           systemImage := "Ship";
                        WHEN "Ship":
                           ASK greenFace TO LoadFromLibrary(images, "Stop_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Stop_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Stop_R");
                           systemImage := "Stoplight";
                        WHEN "Stoplight":
                           ASK greenFace TO LoadFromLibrary(images, "Tank_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Tank_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Tank_R");
                           systemImage := "Tank";
                        WHEN "Tank":
                           ASK greenFace TO LoadFromLibrary(images, "Trai_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Trai_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Trai_R");
                           systemImage := "Train";
                        WHEN "Train":
                           ASK greenFace TO LoadFromLibrary(images, "Truc_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Truc_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Truc_R");
                           systemImage := "Truck";
                        OTHERWISE
                           ASK greenFace TO LoadFromLibrary(images, "Equa_G"); 
                           ASK yellowFace TO LoadFromLibrary(images, "Equa_Y"); 
                           ASK redFace TO LoadFromLibrary(images, "Equa_R");
                           systemImage := "Equalizer";
                     END CASE;
                     ASK greenFace TO SetScaling(symbolScale,symbolScale);
                     ASK greenFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff)-widescreenOffset);        {cmc 5/10/07}
                     ASK yellowFace TO SetScaling(symbolScale,symbolScale);
                     ASK yellowFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff)-widescreenOffset);      {cmc 5/10/07}
                     ASK redFace TO SetScaling(symbolScale,symbolScale);
                     ASK redFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff)-widescreenOffset);         {cmc 5/10/07}
                     ASK greenFace TO Erase;
                     ASK yellowFace TO Erase;
                     ASK redFace TO Erase;  
                     ASK faceVisible TO Draw;
                  END IF;
               END IF;
            ELSIF draggingSymbol
               SetCursor(NILOBJ);
               DISPOSE(selected);
               AddGraphic(greenFace);
               AddGraphic(yellowFace);
               AddGraphic(redFace);
               ASK greenFace TO Draw;
               draggingSymbol := FALSE;
            ELSE
               changeView := FALSE;
               selectedObj := ASK root TO Select(x,y);
               IF selectedObj <> NILOBJ {zoomissue - cv22}
                  IF OBJTYPENAME(selectedObj) = "RBDNodeObj"
                     tempNode := RBDNodeObj(selectedObj); 
                     IF tempNode.typeNode = 5
                        changeView := TRUE;
                     END IF;
                  END IF;
                  IF OBJTYPENAME(selectedObj) = "RBDHierObj"
                     {double click on hier}
                     IF SecondClick AND (NOT buttondown)
                        tempHier := RBDHierObj(selectedObj); 
                        ChangeWindow(tempHier.Id, tempHier.level);
                     END IF;
                  ELSIF changeView
                     IF SecondClick AND (NOT buttondown)
                        tempHier := ASK root Descendant("RBDHier", activeWindow);
                        newId := tempHier.parentID;
                        IF newId > 0
                           tempHier := ASK root Descendant("RBDHier", newId);
                           ChangeWindow(newId, hierLevel-1);
                        ELSE
                           ChangeWindow(0, 0);
                        END IF;
                     END IF;
                  ELSE
                     IF (Button = 2) AND buttondown {right click}
                        ShowRightClick(5);
                        ShowStatus(0,"Sim Speed : "+SUBSTR(1,8,REALTOSTR(dTimeSlice)));
                     END IF ;
                     {IF DisplayAoGraph AND buttondown
                        SetView((pointX-cusZoomVal/2.),(pointY+cusZoomVal/2.*6.9/20.));
                     ELSIF buttondown
                        SetView((pointX-cusZoomVal/2.),(pointY+cusZoomVal/2.*12.85/20.));
                     END IF;}
                  END IF;
               END IF;
            END IF;
         WHEN weakC:
            IF Button = 0 {left click}
               selectedObj := ASK root TO Select(x,y);
               IF SecondClick AND buttondown {double click}
                  changeView := FALSE;
                  IF OBJTYPENAME(selectedObj) = "RBDNodeObj"
                     tempNode := RBDNodeObj(selectedObj); 
                     IF tempNode.typeNode = 5
                        changeView := TRUE;
                     END IF;
                  END IF;
                  IF OBJTYPENAME(selectedObj) = "RBDHierObj"
                     tempHier := RBDHierObj(selectedObj); 
                     ChangeWindow(tempHier.Id, tempHier.level);
                  ELSIF changeView
                     tempHier := ASK root Descendant("RBDHier", activeWindow);
                     newId := tempHier.parentID;
                     IF newId > 0
                        tempHier := ASK root Descendant("RBDHier", newId);
                        ChangeWindow(newId, hierLevel-1);
                     ELSE
                        ChangeWindow(0, 0);
                     END IF;
                  END IF;
               END IF; 
            ELSIF (Button = 2) AND buttondown {right click}
               ShowRightClick(6);   
            END IF;
         OTHERWISE
            NEW(message, 1..1);
            message[1] := "ERROR: Unknown cursor type!";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
      END CASE;
      INHERITED MouseClick(x,y,buttondown);
   END METHOD; {MouseClick}

   ASK METHOD MouseMove(IN x, y : REAL);
   VAR
      tempX, tempY, offset, xIncrement, xBase, 
      yIncrement, yBase, xPosition, yPosition, 
      downLim, localZoomVal, tempXOrig, tempYOrig   : REAL;
      moveScreen, somethingChangedSave, hasparent         : BOOLEAN;
      fDistro, rDistro, budName                : STRING;
      selectedObj                              : ImageObj;
      scanNode, tempNode, outNode, scanOutNode : RBDNodeObj;
      scanBlock, tempBlock, block              : RBDBlockObj;
      scanHier, tempHier, tempHier1, tempHier2 : RBDHierObj;
      scanEvent, tempEvent                     : RBDEventObj;
      basic                                    : RBDBasicObj;
      knText                                   : TextObj;
      child, obj : ANYOBJ;
      scanLink : LinkObj;
      numBlocks, numEvents, numNodes, numHiers, deepestDepth, currDepth : INTEGER;
      phase : PhaseObj; {cmc}
   BEGIN
      IF ignoreMouse {eag error 51 fix}
         RETURN;
      END IF;
      IF (compileType <> "demo") AND saveIsOn AND ((TRUNC(ClockRealSecs)-lastSave) >= saveInc) AND (totalBlocks > 1)
         AND (typeOfCursor = nilC) AND (NOT pastingMultiple)
         lastSave := TRUNC(ClockRealSecs);
         typeOfCursor := autoC;
         ShowStatus(0,"Autosaving...");
         somethingChangedSave := somethingChanged;
         rapVersion := 7;
         SaveFile("cksrule.rbd", exampPath, "*.rbd", totalBlocks, totalNodes, totalLinks, totalHiers, totalEvents);
         somethingChanged := somethingChangedSave;
         typeOfCursor := nilC;
         ShowStatus(0,"");
      END IF; 
      Transform(NILOBJ,root,PointerX,PointerY,xPosition,yPosition);
      IF activeWindow > 0
         tempHier := ASK root Child("RBDHier", activeWindow);
         localZoomVal := tempHier.zoom;
         tempXOrig := tempHier.xOrigin;
         tempYOrig := tempHier.yOrigin;
      ELSE
         localZoomVal := cusZoomVal;
         tempXOrig := xOrigin;
         tempYOrig := yOrigin;
      END IF;
      IF scrollScreen AND (NOT nowSimulating) AND (typeOfCursor <> autoC)
         xBase := 0.1;
         yBase := 0.1;
         xIncrement := xBase;
         yIncrement := yBase;
         leftLimit  := tempXOrig+(localZoomVal*.03);
         rightLimit := tempXOrig+localZoomVal-(localZoomVal*.03);
         upLimit  := tempYOrig-(localZoomVal*yShift/20.*.03);
         downLimit    := tempYOrig-(localZoomVal*yShift/20.)+(localZoomVal*yShift/20.*.03);
         IF ((xPosition < leftLimit) AND (leftLimit > (localZoomVal*0.03))) OR 
            ((xPosition > rightLimit) AND (rightLimit < (120.-localZoomVal*0.03)))
            IF xPosition < leftLimit
               xBase := -1.*xBase;
               xIncrement := xBase;
            END IF;
            moveScreen := TRUE;
         ELSE
            xIncrement := 0.;
         END IF;
         IF ((yPosition < downLimit) AND (downLimit > (localZoomVal*0.03))) OR 
            ((yPosition > upLimit) AND (upLimit < (80.-localZoomVal*0.03)))
            IF yPosition < downLimit
               yBase := -1.*yBase;
               yIncrement := yBase;
            END IF;
            moveScreen := TRUE;
         ELSE
            yIncrement := 0.;
         END IF;
         IF moveScreen
            lastMove := ClockTimeSecs;
            REPEAT
               tempXOrig := tempXOrig+xIncrement;
               tempYOrig := tempYOrig+yIncrement;
               IF activeWindow > 0
                  tempHier := ASK root Child("RBDHier", activeWindow);
                  ASK tempHier TO SetOrigin(tempXOrig, tempYOrig);
               ELSE
                  xOrigin := tempXOrig;
                  yOrigin := tempYOrig;
               END IF; 
               leftLimit  := tempXOrig+(localZoomVal*.03);
               rightLimit := tempXOrig+localZoomVal-(localZoomVal*.03);
               upLimit  := tempYOrig-(localZoomVal*yShift/20.*.03);
               downLimit    := tempYOrig-(localZoomVal*yShift/20.)+(localZoomVal*yShift/20.*.03);
               IF leftLimit < (localZoomVal*0.03)
                  rightLimit := rightLimit + ((localZoomVal*0.03)-leftLimit);
                  leftLimit := localZoomVal*0.03;
                  tempXOrig := 0.0;
               ELSIF rightLimit > (120.- (localZoomVal*0.03))
                  leftLimit := leftLimit - (rightLimit-(120.-localZoomVal*0.03));
                  rightLimit := 120.- (localZoomVal*0.03);
                  tempXOrig := 120.-localZoomVal;
               END IF;   
               IF downLimit < (localZoomVal*yShift/20.*.03)
                  upLimit := upLimit + ((localZoomVal*0.03)-downLimit);
                  downLimit := localZoomVal*0.03;
                  tempYOrig := localZoomVal*yShift/20.;
               ELSIF upLimit > (80.-(localZoomVal*yShift/20.*.03))
                  downLimit := downLimit - (upLimit-(80.-localZoomVal*0.03));
                  upLimit := (80.-localZoomVal*0.03);
                  tempYOrig := 80.;
               END IF;   
               FindOffset(ROUND(localZoomVal), offset);
               downLim := -1.*(tempYOrig-((yShift+offset)*(localZoomVal/20.)));
               IF downLim > .023019
                  downLim := 0.023019;
               END IF;
               ASK root TO SetTranslation(-1.*tempXOrig,downLim);
               SetThumbPosition(tempXOrig/120.,(80.-tempYOrig)/80.);
               IF drawingBox
                  ASK rubberBox TO Erase;
               END IF;
               IF typeOfCursor = connectC
                  Transform(root, baseroot, linkStartX, linkStartY, tempX, tempY);
                  ASK linkCursor TO SetAnchor(tempX,tempY);
                  ASK link TO Erase;
                  SetCursor(NILOBJ);
               END IF;
               Draw;
               Transform(NILOBJ, root, PointerX, PointerY, xPosition, yPosition);
               IF xBase < 0.
                  xIncrement := POWER(3.,FLOAT(TRUNC((leftLimit-xPosition)/.25)))* xBase;
               ELSIF xBase > 0.
                  xIncrement := POWER(3.,FLOAT(TRUNC((xPosition-rightLimit)/.25)))* xBase;
               END IF;
               IF yBase < 0.
                  yIncrement := POWER(3.,FLOAT(TRUNC((downLimit-yPosition)/.25)))* yBase;
               ELSIF yBase > 0.
                  yIncrement := POWER(3.,FLOAT(TRUNC((yPosition-upLimit)/.25)))* yBase;
               END IF;
            UNTIL ((xPosition >= leftLimit) OR (leftLimit <= 0.75)) AND ((xPosition <= rightLimit) OR (rightLimit >= 119.25))
              AND ((yPosition >= downLimit) OR (downLimit <= 0.75)) AND ((yPosition <= upLimit) OR (upLimit >= 79.25));
            moveScreen := FALSE;
            IF typeOfCursor = connectC
               Transform(root, baseroot, linkStartX, linkStartY, tempX, tempY);
               SetCursorOffset(0., 0.); 
               ASK linkCursor TO SetAnchor(tempX,tempY);
               SetCursor(linkCursor);  
               Draw;
            END IF;
         ELSE          
            selectedObj := ASK root TO Select(xPosition,yPosition);
            IF (selectedObj <> NILOBJ)
               IF (OBJTYPENAME(selectedObj) = "RBDHierObj") AND (typeOfCursor <> connectC) AND (NOT drawingBox)
                  IF startHier = 0
                     startHier := ClockTimeSecs;
                  ELSIF (ClockTimeSecs - startHier) >= 1
                     IF (NOT importing)
                        tempHier := RBDHierObj(selectedObj);
                        IF ((hiersIn > 0) OR (typeOfCursor = hierC)) 
                           IF (tempHier.level >= levelLimit) 
                              NEW(message,1..1);
                              message[1] := "Hierarchy items cannot be placed below level " + INTTOSTR(levelLimit) + "     ";
                              result := SendAlert(message,FALSE, FALSE, FALSE);
                              DISPOSE(message);
                              RETURN;
                           ELSE
                              deepestDepth := 0;
                              FOREACH obj IN selectGroup
                                 IF OBJTYPENAME(obj) = "RBDHierObj"
                                    tempHier2 := RBDHierObj(obj);
                                    ASK tempHier2 TO CalculateDepth(currDepth);
                                    IF (currDepth - tempHier2.level) > deepestDepth
                                       deepestDepth := (currDepth - tempHier2.level);
                                    END IF;
                                 END IF;
                              END FOREACH;
                              IF (deepestDepth >= (levelLimit - tempHier.level))
                                 NEW(message,1..2);
                                 message[1] := "Hierarchy items of depth " + INTTOSTR(deepestDepth+1) + " cannot     "
                                 message[2] := "be placed on this level.     ";
                                 result := SendAlert(message,FALSE, FALSE, FALSE);
                                 DISPOSE(message);
                                 RETURN;
                              END IF;
                           END IF;
                        END IF;
                        ChangeWindow(tempHier.Id, tempHier.level);
                     END IF;
                  END IF;
               ELSE
                  startHier := 0;
               END IF;
               IF (activeWindow > 0) AND (OBJTYPENAME(selectedObj) = "RBDNodeObj") AND (typeOfCursor <> connectC)
                  tempNode := RBDNodeObj(selectedObj);
                  IF tempNode.typeNode = 5
                     IF startOut = 0
                        startOut := ClockTimeSecs;
                     ELSIF (ClockTimeSecs - startOut) >= 1
                        IF (NOT importing)
                           tempHier := ASK root Child("RBDHier", tempNode.parentID);
                           ChangeWindow(tempHier.parentID, tempHier.level-1);
                        END IF;
                     END IF;
                  ELSE
                     startOut := 0;
                  END IF;
               ELSE
                  startOut := 0;
               END IF;
            END IF;
         END IF;
      ELSIF typeOfCursor = nilC
         selectedObj := ASK root TO Select(xPosition,yPosition);
         IF (selectedObj <> currentObj) AND (selectedObj <> NILOBJ)
            IF OBJTYPENAME(selectedObj) = "RBDBlockObj"
               scanBlock := RBDBlockObj(selectedObj);
               IF devmode
                  ShowStatus(0, "parentID = " + INTTOSTR(scanBlock.parentID));
                  ShowStatus(2, "Id = " + INTTOSTR(scanBlock.Id));
                  IF scanBlock.parentID > 0
                     tempHier := ASK root Child("RBDHier", scanBlock.parentID);
                     IF tempHier.childGroup.Includes(scanBlock);
                        ShowStatus(3, "parent is " + tempHier.name);
                     END IF;
                  END IF;
                  ShowStatus(4, "con out = " + INTTOSTR(scanBlock.connectOutOfNum));
                  ShowStatus(5, "");
                  ShowStatus(6, "b-" + INTTOSTR(totalBlocks) + " n-" + INTTOSTR(totalNodes) +
                             " h-" +INTTOSTR(totalHiers) + " T-" + INTTOSTR(totalObjects));
               ELSE
                  ConvertToString(scanBlock.failDistro,fDistro); 
                  ConvertToString(scanBlock.repairDistro,rDistro); 
                  ShowStatus(1,"Fail Distro: "+fDistro);
                  IF scanBlock.failMean = 12345.6789
                     ShowStatus(2,"Fail Mean: Undefined");
                  ELSIF scanBlock.failMean = 12345.6788
                     ShowStatus(2,"Fail Mean: N/A");
                  ELSE
                     ShowStatus(2,"Fail Mean: "+SUBSTR(1,11,REALTOSTR(scanBlock.failMean)));
                  END IF;
                  ShowStatus(3,"Repair Distro: "+rDistro);
                  IF scanBlock.repairMean = 12345.6789
                     ShowStatus(4,"Repair Mean: Undefined");
                  ELSIF scanBlock.repairMean = 12345.6788
                     ShowStatus(4,"Repair Mean: N/A");
                  ELSE
                     ShowStatus(4,"Repair Mean: "+SUBSTR(1,11,REALTOSTR(scanBlock.repairMean)));
                  END IF;
                  IF scanBlock.sparingType = Infinite
                     ShowStatus(5,"Infinite");
                  ELSIF scanBlock.sparingType = SparePool
                     ShowStatus(5,"Spare Pool");
                  ELSIF scanBlock.sparingType = ColdPool
                     ShowStatus(5,"Standby");
                  ELSE
                     ShowStatus(5,"Custom");
                  END IF;
                  IF scanBlock.DependencyNum > 0
                     IF scanBlock.depType = "RBDBlock"
                        tempBlock := ASK root Child("RBDBlock", scanBlock.DependencyNum);
                        budName := tempBlock.name;
                     ELSIF scanBlock.depType = "RBDEvent"
                        tempEvent := ASK root Child("RBDEvent", scanBlock.DependencyNum);
                        budName := tempEvent.name;
                     ELSIF scanBlock.depType = "RBDNode"
                        tempNode := ASK root Child("RBDNode", scanBlock.DependencyNum);
                        budName := tempNode.name;
                     ELSIF scanBlock.depType = "RBDHier"
                        outNode := ASK root Child("RBDNode", scanBlock.DependencyNum);
                        tempHier := ASK root Child("RBDHier", outNode.parentID);
                        budName := tempHier.name;
                     END IF;
                     ShowStatus(6,"Depends: "+budName);
                  ELSIF scanBlock.DependencyNum=-1
                     ShowStatus(6,"Locally Dependent");
                  ELSIF scanBlock.DependencyNum=-2
                     ShowStatus(6,"System Dependent");
                  ELSE
                     ShowStatus(6,"Independent");
                  END IF;
               END IF;
            ELSIF OBJTYPENAME(selectedObj) = "RBDEventObj"
               scanEvent := RBDEventObj(selectedObj);
               IF devmode
                  ShowStatus(0, "parentID = " + INTTOSTR(scanEvent.parentID));
                  ShowStatus(2, "Id = " + INTTOSTR(scanEvent.Id));
                  IF scanEvent.parentID > 0
                     tempHier := ASK root Child("RBDHier", scanEvent.parentID);
                     IF tempHier.childGroup.Includes(scanEvent);
                        ShowStatus(3, "parent is " + tempHier.name);
                     END IF;
                  END IF;
                  ShowStatus(4, "con out = " + INTTOSTR(scanEvent.connectOutOfNum));
                  ShowStatus(5, "");
                  ShowStatus(6, "b-" + INTTOSTR(totalBlocks) + " n-" + INTTOSTR(totalNodes) +
                             " h-" +INTTOSTR(totalHiers) + " T-" + INTTOSTR(totalObjects));
              ELSE                      
                  ShowStatus(1,"Success Probability:");
                  ShowStatus(2,SUBSTR(1,8,REALTOSTR(scanEvent.failVals[1])));
                  ShowStatus(3,"");
                  ShowStatus(4,"");
                  ShowStatus(5,"");
                  ShowStatus(6,"");
               END IF;
            ELSIF OBJTYPENAME(selectedObj) = "RBDNodeObj"
               scanNode := RBDNodeObj(selectedObj);
               IF devmode
                  ShowStatus(0, "parentID = " + INTTOSTR(scanNode.parentID));
                  ShowStatus(2, "Id = " + INTTOSTR(scanNode.Id));
                  ShowStatus(3, "typeNode = " + INTTOSTR(scanNode.typeNode));
                  ShowStatus(5, "con out = " + INTTOSTR(scanNode.connectOutOfNum));
                  ShowStatus(6,"b-" + INTTOSTR(totalBlocks) + " n-" + INTTOSTR(totalNodes) +
                             " h-" +INTTOSTR(totalHiers) + " T-" + INTTOSTR(totalObjects));
                  IF scanNode.parentID > 0
                     tempHier := ASK root Child("RBDHier", scanNode.parentID);
                     IF tempHier.childGroup.Includes(scanNode);
                        ShowStatus(4, "parent is " + tempHier.name);
                     END IF;
                  END IF;
             ELSE
                  IF (scanNode.typeNode = 2)
                     ShowStatus(3,"");
                     ShowStatus(4,"");
                     ShowStatus(5,"");
                     ShowStatus(6,"");
                     knText := ASK scanNode Child("RBDNodeKofN", 0);
                     IF knText.String <> ""
                        ShowStatus(1,INTTOSTR(scanNode.goodPaths)+" out of "+INTTOSTR(scanNode.connectIntoNum));
                     ELSIF scanNode.connectIntoNum = 1
                        ShowStatus(1,"1 out of 1");
                     ELSE
                        ShowStatus(1,"Undefined");
                     END IF;
                     ShowStatus(2,"Links Out: "+INTTOSTR(scanNode.connectOutOfNum));
                     IF scanNode.coldStandby
                        ShowStatus(3, "kstar: " + INTTOSTR(scanNode.KStar));
                        ShowStatus(5,"Standby");
                     ELSE
                        ShowStatus(3,"");
                        ShowStatus(5,"");
                     END IF;
                     IF scanNode.DependencyNum > 0
                        IF scanNode.depType = "RBDBlock"
                           tempBlock := ASK root Child("RBDBlock", scanNode.DependencyNum);
                           budName := tempBlock.name;
                        ELSIF scanNode.depType = "RBDEvent"
                           tempEvent := ASK root Child("RBDEvent", scanNode.DependencyNum);
                           budName := tempEvent.name;
                        ELSIF scanNode.depType = "RBDNode"
                           tempNode := ASK root Child("RBDNode", scanNode.DependencyNum);
                           budName := tempNode.name;
                        ELSIF scanNode.depType = "RBDHier"
                           outNode := ASK root Child("RBDNode", scanNode.DependencyNum);
                           tempHier := ASK root Child("RBDHier", outNode.parentID);
                           budName := tempHier.name;
                        END IF;
                        ShowStatus(6,"Depends: "+budName);
                     ELSIF scanNode.DependencyNum=-1
                        ShowStatus(6,"Locally Dependent");
                     ELSIF scanNode.DependencyNum=-2
                        ShowStatus(6,"System Dependent");
                     ELSE
                        ShowStatus(6,"Independent");
                     END IF;
                  END IF;
               END IF;   
            ELSIF OBJTYPENAME(selectedObj) = "RBDHierObj"
               scanHier := RBDHierObj(selectedObj);
               IF devmode
                  ShowStatus(0, "parentID = " + INTTOSTR(scanHier.parentID) + "     hierLevel = " + INTTOSTR(hierLevel));
                  ShowStatus(1, "deepestLevel = " + INTTOSTR(deepestLevel));
                  ShowStatus(2, "Id = " + INTTOSTR(scanHier.Id));
                  ShowStatus(4, "con out = " + INTTOSTR(scanHier.connectOutOfNum));
                  ShowStatus(5, "level = " + INTTOSTR(scanHier.level));
                  ShowStatus(6,"b-" + INTTOSTR(totalBlocks) + " n-" + INTTOSTR(totalNodes) +
                             " h-" +INTTOSTR(totalHiers) + " T-" + INTTOSTR(totalObjects));
                  IF scanHier.parentID > 0
                     tempHier := ASK root Child("RBDHier", scanHier.parentID);
                     IF tempHier.childGroup.Includes(scanHier);
                        ShowStatus(3, "parent is " + tempHier.name);
                     END IF;
                  END IF;
               ELSE 
                  ShowStatus(1, "");
                  ShowStatus(2, "Level = " + INTTOSTR(scanHier.level));
                  ShowStatus(3, "");
                  ShowStatus(4, "");
                  ShowStatus(5, "");
                  scanOutNode := ASK root Child("RBDNode", scanHier.outID);
                  IF scanOutNode.DependencyNum > 0
                     IF scanOutNode.depType = "RBDBlock"
                        tempBlock := ASK root Child("RBDBlock", scanOutNode.DependencyNum);
                        budName := tempBlock.name;
                     ELSIF scanOutNode.depType = "RBDEvent"
                        tempEvent := ASK root Child("RBDEvent", scanOutNode.DependencyNum);
                        budName := tempEvent.name;
                     ELSIF scanOutNode.depType = "RBDNode"
                        tempNode := ASK root Child("RBDNode", scanOutNode.DependencyNum);
                        budName := tempNode.name;
                     ELSIF scanOutNode.depType = "RBDHier"
                        outNode := ASK root Child("RBDNode", scanOutNode.DependencyNum);
                        tempHier := ASK root Child("RBDHier", outNode.parentID);
                        budName := tempHier.name;
                     END IF;
                     ShowStatus(6,"Depends: "+budName);
                  ELSIF scanOutNode.DependencyNum=-1
                     ShowStatus(6,"Locally Dependent");
                  ELSIF scanOutNode.DependencyNum=-2
                     ShowStatus(6,"System Dependent");
                  ELSE
                     ShowStatus(6,"Independent");
                  END IF;
               END IF;
            ELSIF OBJTYPENAME(selectedObj) = "LinkObj"
               scanLink := LinkObj(selectedObj);
               IF devmode
                  ShowStatus(0, "parentID = " + INTTOSTR(scanLink.parentID) + ", link Id = " + INTTOSTR(scanLink.Id));
                  ShowStatus(2, scanLink.connectFRef);
                  ShowStatus(3, INTTOSTR(scanLink.connectFromId));
                  ShowStatus(4, scanLink.connectTRef);
                  ShowStatus(5, INTTOSTR(scanLink.connectToId));
                  ShowStatus(6, "L-" + INTTOSTR(totalLinks));
                  IF scanLink.parentID > 0
                     tempHier := ASK root Child("RBDHier", scanLink.parentID);
                     IF tempHier.childGroup.Includes(scanLink);
                        ShowStatus(1, "parent is " + tempHier.name);
                     END IF;
                  END IF;
               END IF;
            ELSE {GridObj or LinkObj}
               ShowStatus(0,"");
               ShowStatus(1,"");
               ShowStatus(2,"");
               ShowStatus(3,"");
               ShowStatus(4,"");
               ShowStatus(5,"");
               ShowStatus(6,"");
            END IF;
            currentObj := selectedObj;
         END IF;
      ELSIF typeOfCursor = fevC
         selectedObj := ASK root TO Select(xPosition,yPosition);
         IF (selectedObj <> currentObj) AND (selectedObj <> NILOBJ)
            IF OBJTYPENAME(selectedObj) = "RBDBlockObj"
               scanBlock := RBDBlockObj(selectedObj);
               ShowStatus(1, scanBlock.name);
               MakeDistString(scanBlock.failDistro, scanBlock.failVals, fDistro); 
               MakeDistString(scanBlock.repairDistro, scanBlock.repairVals, rDistro); 
               ShowStatus(2,"F: "+fDistro);
               ShowStatus(3,"R: "+rDistro);
               {put this code when single left click on block so updates on click as well as move}
               IF scanBlock.opStatus = Running
                  ShowStatus(4, "Operating");
               ELSIF scanBlock.opStatus = Repairing
                  ShowStatus(4, "Repairing");
               ELSIF scanBlock.opStatus = Done
                  ShowStatus(4, "Done");
               ELSIF scanBlock.opStatus = Idle
                  ShowStatus(4, "Idle");
               ELSIF scanBlock.opStatus = Standby
                  ShowStatus(4, "Standby");
               ELSIF scanBlock.opStatus = PM
                  ShowStatus(4, "PM");
               ELSIF scanBlock.opStatus = PMhold
                  ShowStatus(4, "PM-Hold");
               ELSIF scanBlock.opStatus = RepHold
                  ShowStatus(4, "Hold");
               END IF;
               IF password
                  ShowStatus(5, "Criticality = " + REALTOSTR(critFactor));
               END IF;
               IF scanBlock.activeStatus = Active
                  ShowStatus(6, "Active");
               ELSIF scanBlock.activeStatus = Linked
                  ShowStatus(6, "Linked");
               ELSIF scanBlock.activeStatus = Cut
                  ShowStatus(6, "Cut");
               END IF;
            ELSIF OBJTYPENAME(selectedObj) = "RBDEventObj"
               scanEvent := RBDEventObj(selectedObj);
               ShowStatus(1, scanEvent.name);
               ShowStatus(2, "");
               ShowStatus(3, "");
               ShowStatus(4, "");
               ShowStatus(5, "");
               ShowStatus(6, "");
               IF scanEvent.opStatus = Armed
                  ShowStatus(4, "Armed");
               ELSIF scanEvent.opStatus = Success
                  ShowStatus(4, "Success");
               ELSIF scanEvent.opStatus = Failure
                  ShowStatus(4, "Failure");
               END IF;
               IF scanEvent.activeStatus = Active
                  ShowStatus(6, "Active");
               ELSIF scanEvent.activeStatus = Linked
                  ShowStatus(6, "Linked");
               ELSIF scanEvent.activeStatus = Cut
                  ShowStatus(6, "Cut");
               END IF;
            ELSIF OBJTYPENAME(selectedObj) = "RBDNodeObj"
               scanNode := RBDNodeObj(selectedObj);
               IF (scanNode.typeNode = 2)
                  ShowStatus(1, scanNode.name);
                  ShowStatus(2, "");
                  ShowStatus(3, "");
                  ShowStatus(4, "");
                  ShowStatus(5, "");
                  ShowStatus(6, "");
                  knText := ASK scanNode Child("RBDNodeKofN", 0);
                  IF knText.String <> ""
                     ShowStatus(2, INTTOSTR(scanNode.goodPaths)+" out of "+INTTOSTR(scanNode.connectIntoNum));
                     ShowStatus(3, "Up = " + INTTOSTR(scanNode.NumGoodPaths)); 
                  ELSIF scanNode.connectIntoNum = 1
                     ShowStatus(2, "1 out of 1");
                  ELSE
                     ShowStatus(2, "");
                  END IF;
                  IF scanNode.Status = AllUp
                     ShowStatus(4, "Good");
                  ELSIF scanNode.Status = NodeStandby
                     ShowStatus(4, "Standby");
                  ELSIF scanNode.Status = Degraded
                     ShowStatus(4, "Degraded");
                  ELSIF scanNode.Status = NodeIdle
                     ShowStatus(4, "Idle");
                  ELSIF scanNode.Status = NodePM
                     ShowStatus(4, "PM");
                  ELSIF scanNode.Status = Down
                     ShowStatus(4, "Bad");
                  END IF;
                  IF scanNode.activeStatus = Active
                     ShowStatus(6, "Active");
                  ELSIF scanNode.activeStatus = Linked
                     ShowStatus(6, "Linked");
                  ELSIF scanNode.activeStatus = Cut
                     ShowStatus(6, "Cut");
                  END IF;
               END IF;   
            ELSIF OBJTYPENAME(selectedObj) = "RBDHierObj"
               scanHier := RBDHierObj(selectedObj);
               outNode := ASK root Child("RBDNode", scanHier.outID);
               ShowStatus(1, scanHier.name);
               ShowStatus(2, "");
               ShowStatus(3, "");
               ShowStatus(4, "");
               ShowStatus(5, "");
               ShowStatus(6, "");
               ShowStatus(2, "Level = " + INTTOSTR(scanHier.level));
               {ShowStatus(3, "Depth = " + INTTOSTR(scanHier.myDepth));}
               IF outNode.Status = AllUp
                  ShowStatus(4, "Good");
               ELSIF outNode.Status = NodeStandby
                  ShowStatus(4, "Standby");
               ELSIF outNode.Status = Degraded
                  ShowStatus(4, "Degraded");
               ELSIF outNode.Status = NodeIdle
                  ShowStatus(4, "Idle");
               ELSIF outNode.Status = NodePM
                  ShowStatus(4, "PM");
               ELSIF outNode.Status = Down
                  ShowStatus(4, "Bad");
               END IF;
               IF outNode.activeStatus = Active
                  ShowStatus(6, "Active");
               ELSIF outNode.activeStatus = Linked
                  ShowStatus(6, "Linked");
               ELSIF outNode.activeStatus = Cut
                  ShowStatus(6, "Cut");
               END IF;
            ELSE {GridObj or LinkObj}
               ShowStatus(0,"");
               ShowStatus(1,"");
               ShowStatus(2,"");
               IF ((statusBarOn) AND (activePhases > 0)) {cmc}
                  phase:=phaseObjArray[phaseNumber];        
                  ASK window TO ShowStatus(3,"phase.phaseName); 
               ELSE
                  ShowStatus(3,"");
               END IF;
               ShowStatus(4,"");
               ShowStatus(5,"");
               ShowStatus(6,"");
            END IF;
            currentObj := selectedObj;
         END IF;
      ELSIF typeOfCursor = weakC
         selectedObj := ASK root TO Select(xPosition,yPosition);
         IF (selectedObj <> currentObj) AND (selectedObj <> NILOBJ)
            IF OBJTYPENAME(selectedObj) = "RBDBlockObj"
               scanBlock := RBDBlockObj(selectedObj);
               ShowStatus(1, scanBlock.name + " Block");
               IF weakLinkAnalType=1 
                  ShowStatus(2, scanBlock.Ao);
               ELSIF weakLinkAnalType=2 
                  ShowStatus(2, scanBlock.Do);
               ELSE 
                  ShowStatus(2, scanBlock.R);
               END IF;
            ELSIF OBJTYPENAME(selectedObj) = "RBDEventObj"
               scanEvent := RBDEventObj(selectedObj);
               ShowStatus(1, scanEvent.name + " Event");
               IF weakLinkAnalType=1 
                  ShowStatus(2, scanEvent.Ao);
               ELSIF weakLinkAnalType=2 
                  ShowStatus(2, scanEvent.Do);
               ELSE 
                  ShowStatus(2, scanEvent.R);
               END IF;
            ELSIF OBJTYPENAME(selectedObj) = "RBDNodeObj"
               scanNode := RBDNodeObj(selectedObj);
               ShowStatus(1, scanNode.name + " Node");
               IF weakLinkAnalType=1 
                  ShowStatus(2, scanNode.Ao);
               ELSIF weakLinkAnalType=2 
                  ShowStatus(2, scanNode.Do);
               ELSE 
                  ShowStatus(2, scanNode.R);
               END IF;
            ELSIF OBJTYPENAME(selectedObj) = "RBDHierObj"
               scanHier := RBDHierObj(selectedObj);
               ShowStatus(1, scanHier.name + " Hier");
               outNode := ASK root Child("RBDNode", scanHier.outID);
               IF weakLinkAnalType=1 
                  ShowStatus(2, outNode.Ao);
               ELSIF weakLinkAnalType=2 
                  ShowStatus(2, outNode.Do);
               ELSE 
                  ShowStatus(2, outNode.R);
               END IF;
            END IF;
         END IF;
      END IF;
      INHERITED MouseMove(x,y);
   END METHOD; {Mousemove} 
      
   ASK METHOD ShowRightClick(IN whatToShow : INTEGER);
   VAR
      tempUnits, tempImage                       : STRING;
      defaultStream                              : StreamObj;
      centerX, centerY, tempZoomVal              : REAL;
      cutItem, copyItem, pasteItem, deleteItem,
      addBItem, addNItem, addLItem, addHItem, 
      zoomItem, simItem, addEItem, prefItem,
      propItem, printItem, homeItem, upItem,
      findItem, collapseItem, masseditItem,
      writeItem, effectsItem, refphaseItem,
      refallItem, pauseItem, stepItem, jumpItem,
      gotoItem, outItem, colorItem               : MenuItemObj;
      picked                                     : ControlVObj;
      zoomBox                                    : HelpBoxObj;
      zoomDropBox                                : ComboBoxObj;
      button                                     : ButtonObj;
      goodZoom, canceled, player, tempGrid, 
      tempSave, {needsDrawn,} validRBD           : BOOLEAN;
      prefBox                                    : PrefsBoxObj;
      diff, deepestDepth, currDepth              : INTEGER;
      tempHier1, tempHier2, tempHier             : RBDHierObj;
      obj                                        : ANYOBJ;
   BEGIN
      IF NOT awaitingResponse
         NEW(cutItem);     NEW(copyItem);     NEW(addEItem);
         NEW(pasteItem);   NEW(deleteItem);   NEW(addBItem);
         NEW(addNItem);    NEW(addLItem);     NEW(propItem);
         NEW(zoomItem);    NEW(simItem);      NEW(prefItem);
         NEW(printItem);   NEW(addHItem);     NEW(homeItem);
         NEW(upItem);      NEW(findItem);     NEW(collapseItem);
         NEW(masseditItem);NEW(writeItem);    NEW(effectsItem);
         NEW(refphaseItem);NEW(refallItem);   NEW(pauseItem);
         NEW(stepItem);    NEW(gotoItem);     NEW(jumpItem);
         NEW(outItem);     NEW(colorItem);   
         ASK cutItem    TO SetLabel("Cut");
         ASK copyItem   TO SetLabel("Copy");
         ASK pasteItem  TO SetLabel("Paste");
         ASK deleteItem TO SetLabel("Clear");
         ASK addBItem   TO SetLabel("Add Block");
         ASK addNItem   TO SetLabel("Add Node");
         ASK addLItem   TO SetLabel("Add Link");
         ASK addEItem   TO SetLabel("Add Event");
         ASK addHItem   TO SetLabel("Add Hierarchy");
         ASK propItem   TO SetLabel("Properties");
         ASK zoomItem   TO SetLabel("Center-zoom...");
         ASK simItem    TO SetLabel("Simulate...");
         ASK prefItem   TO SetLabel("Preferences");
         ASK printItem  TO SetLabel("Print Selected");
         ASK homeItem   TO SetLabel("Home");
         ASK upItem     TO SetLabel("Up");
         ASK findItem   TO SetLabel("Find Item...");
         ASK collapseItem TO SetLabel("Collapse");
         ASK masseditItem TO SetLabel("Mass Edit...");
         ASK writeItem    TO SetLabel("Write to Log");
         ASK effectsItem  TO SetLabel("Display Failure Effects...");
         ASK refphaseItem TO SetLabel("Refresh Phase");
         ASK refallItem   TO SetLabel("Refresh All");
         ASK pauseItem    TO SetLabel("Pause");
         ASK stepItem     TO SetLabel("Step");
         ASK jumpItem     TO SetLabel("Jump");
         ASK gotoItem     TO SetLabel("Goto...");
         ASK outItem      TO SetLabel("View Outputs...");
         ASK colorItem    TO SetLabel("Color Thresholds...");
         {right click menus}
         CASE whatToShow
            WHEN 1: {workspace}
               ASK popupMenu TO AddChild(addBItem, "block", 0);       {Add Block}
               ASK popupMenu TO AddChild(addNItem, "node", 0);        {Add Node}
               ASK popupMenu TO AddChild(addLItem, "link", 0);        {Add Link}
               IF totalObjects < 2
                  ASK addLItem TO Deactivate;;
               END IF;
               ASK popupMenu TO AddChild(addEItem, "event",0);        {Add Event}
               ASK popupMenu TO AddChild(addHItem, "hier",0);         {Add Hierarchy}
               IF hierLevel >= levelLimit
                  ASK addHItem TO Deactivate;
               END IF;
               ASK popupMenu TO AddChild(pasteItem, "paste", 0);   {Paste}
               IF NOT copied 
                  ASK pasteItem TO Deactivate;
               END IF;
               ASK popupMenu TO AddChild(homeItem, "home", 0);        {Home}
               ASK popupMenu TO AddChild(upItem, "up", 0);            {Up}
               IF activeWindow < 1
                  ASK homeItem TO Deactivate;
                  ASK upItem TO Deactivate;
               END IF;
               ASK popupMenu TO AddChild(zoomItem, "zoom", 0);        {Center-zoom...}
               ASK popupMenu TO AddChild(findItem, "find", 0);        {Find Item...}
               ASK popupMenu TO AddChild(simItem, "sim", 0);          {Simulate...}
               ASK simItem TO Deactivate;
               IF (totalObjects > 2)
                  ASK simItem TO Activate;
               END IF;
            WHEN 2: {element}
               ASK popupMenu TO AddChild(cutItem, "cut", 0);          {Cut}
               ASK popupMenu TO AddChild(copyItem, "copy", 0);        {Copy}
               ASK popupMenu TO AddChild(deleteItem, "delete", 0);    {Clear}
               ASK popupMenu TO AddChild(collapseItem, "collapse", 0); {Collapse}
               IF (hierLevel >= levelLimit)
                  ASK collapseItem TO Deactivate;
               ELSIF ((hiersIn > 0) AND (hierLevel = (levelLimit - 1)))
                  ASK collapseItem TO Deactivate;
               ELSIF (hiersIn > 0)
                  deepestDepth := 0;
                  FOREACH obj IN selectGroup
                     IF OBJTYPENAME(obj) = "RBDHierObj"
                        tempHier := RBDHierObj(obj);
                        ASK tempHier TO CalculateDepth(currDepth);
                        IF currDepth > deepestDepth
                           deepestDepth := currDepth;
                        END IF;
                     END IF;
                  END FOREACH;
                  IF (deepestDepth >= levelLimit)
                     ASK collapseItem TO Deactivate;
                  END IF;
               END IF;
               ASK popupMenu TO AddChild(zoomItem, "zoom", 0);        {Center-zoom...}
               ASK popupMenu TO AddChild(propItem, "properties", 0);  {Properties}
               IF (hiersIn > 0)
                  ASK popupMenu TO AddChild(masseditItem, "massedit", 0); {Mass Edit...}
               END IF;
            WHEN 3: {multiple elements}
               ASK popupMenu TO AddChild(cutItem, "cut", 0);           {Cut}
               ASK popupMenu TO AddChild(copyItem, "copy", 0);         {Copy}
               ASK popupMenu TO AddChild(deleteItem, "delete", 0);     {Clear}
               ASK popupMenu TO AddChild(collapseItem, "collapse", 0); {Collapse}
               IF (hierLevel >= levelLimit)
                  ASK collapseItem TO Deactivate;
               ELSIF ((hiersIn > 0) AND (hierLevel = (levelLimit - 1)))
                  ASK collapseItem TO Deactivate;
               ELSIF (hiersIn > 0)
                  deepestDepth := 0;
                  FOREACH obj IN selectGroup
                     IF OBJTYPENAME(obj) = "RBDHierObj"
                        tempHier := RBDHierObj(obj);
                        ASK tempHier TO CalculateDepth(currDepth);
                        IF currDepth > deepestDepth
                           deepestDepth := currDepth;
                        END IF;
                     END IF;
                  END FOREACH;
                  IF (deepestDepth >= levelLimit)
                     ASK collapseItem TO Deactivate;
                  END IF;
               END IF;
               ASK popupMenu TO AddChild(zoomItem, "zoom", 0);        {Center-zoom...}
               ASK popupMenu TO AddChild(masseditItem, "massedit", 0); {Mass Edit...}
            WHEN 4: {fev}
               ASK popupMenu TO AddChild(writeItem, "write", 0);       {Write to Log}
               IF FEVStream = NILOBJ
                  ASK writeItem TO Deactivate;
               END IF
               ASK popupMenu TO AddChild(effectsItem, "effects", 0);   {Display Failure Effects...}
               ASK popupMenu TO AddChild(refphaseItem, "refphase", 0); {Refresh Phase}
               IF activePhases = 0
                  ASK refphaseItem TO Deactivate;
               END IF;
               ASK popupMenu TO AddChild(refallItem, "refall", 0);     {Refresh All}
               ASK popupMenu TO AddChild(homeItem, "home", 0);         {Home}
               ASK popupMenu TO AddChild(upItem, "up", 0);             {Up}
               IF activeWindow < 1
                  ASK homeItem TO Deactivate;
                  ASK upItem TO Deactivate;
               END IF;
               ASK popupMenu TO AddChild(zoomItem, "zoom", 0);         {Center-zoom...}
               {ASK popupMenu TO AddChild(findItem, "find", 0);         {Find Item...} }
            WHEN 5: {simulation}
               ASK popupMenu TO AddChild(pauseItem, "pause", 0);       {Pause}
               IF (NOT notPaused)
                  ASK pauseItem TO SetLabel("Resume");
               END IF;
               ASK popupMenu TO AddChild(stepItem, "step", 0);         {Step}
               ASK popupMenu TO AddChild(jumpItem, "jump", 0);         {Jump}
               IF inEndState
                  ASK pauseItem TO Deactivate;
                  ASK stepItem TO Deactivate;
                  ASK jumpItem TO Deactivate;
               END IF;
               ASK popupMenu TO AddChild(gotoItem, "goto", 0);         {Goto...}
               IF totalHiers = 0
                  ASK gotoItem TO Deactivate;
               END IF;
               ASK popupMenu TO AddChild(homeItem, "home", 0);         {Home}
               ASK popupMenu TO AddChild(upItem, "up", 0);             {Up}
               IF activeWindow < 1
                  ASK homeItem TO Deactivate;
                  ASK upItem TO Deactivate;
               END IF;
               {ASK popupMenu TO AddChild(findItem, "find", 0);         {Find Item...} 
               IF (NOT inEndState)
                  ASK findItem TO Deactivate;
               END IF;}
               ASK popupMenu TO AddChild(zoomItem, "zoom", 0);         {Center-zoom...}
            WHEN 6: {weak link}
               ASK popupMenu TO AddChild(outItem, "out", 0);           {View Outputs...}
               ASK popupMenu TO AddChild(colorItem, "color", 0);       {Color Thresholds...}
               ASK popupMenu TO AddChild(gotoItem, "goto", 0);         {Goto...}
               IF totalHiers = 0
                  ASK gotoItem TO Deactivate;
               END IF;
               ASK popupMenu TO AddChild(homeItem, "home", 0);         {Home}
               ASK popupMenu TO AddChild(upItem, "up", 0);             {Up}
               IF activeWindow < 1
                  ASK homeItem TO Deactivate;
                  ASK upItem TO Deactivate;
               END IF;
               ASK popupMenu TO AddChild(zoomItem, "zoom", 0);         {Center-zoom...}
               {ASK popupMenu TO AddChild(findItem, "find", 0);         {Find Item...} }
            OTHERWISE
         END CASE;    
         ASK popupMenu TO SetTranslation(ClickX, ClickY);
         awaitingResponse := TRUE;
         picked := ASK popupMenu TO AcceptInput();      
         awaitingResponse := FALSE;
         IF picked <> NILOBJ
            CASE picked.ReferenceName
               WHEN "block" :
                  AddBlock;
               WHEN "node"  :
                  AddNode;
               WHEN "link"  :
                  NEW(linkCursor);
                  ASK root TO AddGraphic(linkCursor);
                  AddConnector;
                  IF linkCancelled
                     DISPOSE(linkCursor);
                     ASK menubar TO Enable(9);
                     linkCancelled := FALSE;
                  END IF;
               WHEN "event" :
                  AddEvent;
               WHEN "hier" :
                  AddHier;
               WHEN "cut"   :
                  CopyObject;
                  ClearObject;
               WHEN "copy"  :
                  CopyObject; 
               WHEN "paste" :
                  PasteObject;
               WHEN "delete":
                  ClearObject;
               WHEN "properties"  :
                  EditDetails;
               WHEN "sim":
                  ValidateRBD(validRBD);
                  IF validRBD
                     KillPopup;
                     SendToEngine;
                  END IF;
               WHEN "home":
                  IF whatToShow = 1
                     Home;
                  ELSIF whatToShow = 4
                     Home;
                  ELSIF whatToShow = 5
                     Home;
                  ELSIF whatToShow = 6
                     Home;
                  END IF;
               WHEN "up":
                  IF whatToShow = 1
                     UpOneLevel;
                  ELSIF whatToShow = 4
                     UpOneLevel;
                  ELSIF whatToShow = 5
                     UpOneLevel;
                  ELSIF whatToShow = 6
                     UpOneLevel;
                  END IF;
               WHEN "find":
                  IF whatToShow = 1
                     FindItem;
                 { ELSIF whatToShow = 4
                     FindItem;
                  ELSIF whatToShow = 5
                     FindItem;
                  ELSIF whatToShow = 6
                     FindItem;}
                  END IF;
               WHEN "collapse":
                  CollapseIntoHier;
               WHEN "massedit":
                  MassEdit;
               WHEN "write":
                  CreateFMECAarray;
                  WriteFMECAtoFile;
               WHEN "effects":
                  CreateFMECAarray;
                  DisplayFailureEffects;
               WHEN "refphase":
                  RefreshPhase;
               WHEN "refall":
                  RefreshAllFEV;
               WHEN "pause":
                  IF pauseItem.Label = "Pause"
                     KillPopup;
                     PauseSim;
                  ELSE
                     KillPopup;
                     ResumeSim;
                  END IF;
               WHEN "step":
                  Step;
               WHEN "jump":
                  Jump;
               WHEN "goto":
                  GotoHier;
               WHEN "out":
                  ViewOutputs;
               WHEN "color":
                  ColorPrefs;
               WHEN "print":
                  PrintSelected;
               WHEN "prefs":
                  IF soundPath = ""
                     player := FALSE;
                  ELSE
                     player := TRUE;
                  END IF;
                  tempZoomVal := cusZoomVal;
                  tempUnits := globalUnits;
                  tempSave := saveIsOn;
                  tempImage := globalImage;
                  NEW(prefBox);
                  ASK prefBox TO LoadFromLibrary(dialogs, "PrefsBox");
                  ASK window  TO AddGraphic(prefBox);
                  ASK prefBox TO Draw;
                  ASK prefBox TO ReceiveData;
                  DISPOSE(prefBox);
               WHEN "zoom"  :
                  ZoomPercent(ClickX, ClickY, TRUE);
               OTHERWISE
            END CASE;
         END IF;
         KillPopup;
      END IF;
   END METHOD; {ShowRightClick}

   ASK METHOD KillPopup;
   VAR
      firstItem : MenuItemObj;
   BEGIN
      FOR i := 1 TO popupMenu.numberGraphicsIn
         firstItem := ASK popupMenu FirstGraphic();
         ASK popupMenu TO RemoveThisGraphic(firstItem);
         DISPOSE(firstItem);
      END FOR;
      ASK popupMenu TO Erase;
   END METHOD; {KillPopup}

   ASK METHOD BeResized;
   VAR
      diff : REAL;
   BEGIN
      diff := ClockRealSecs-lastResize;
      lastResize := ClockRealSecs;
      IF (NOT nowSimulating) AND (NOT nowInitialing)
         IF (Width < (lastWidth-1.0)) OR (Width > (lastWidth+1.0))
            SetSize(Width,Width);
            lastWidth := Width;
            lastHeight := Width;
         ELSIF (Height < (lastHeight-1.0)) OR (Height > (lastHeight+1.0))
            SetSize(Height,Height);
            lastWidth := Height;
            lastHeight := Height;
         ELSIF diff > 1.0
            SetSize(100.,100.);
            lastWidth := 100.;
            lastHeight := 100.;
         END IF;
         Update;
      END IF; 
   END METHOD; {BeResized}

   ASK METHOD BeScrolled(IN horizThumbPos, vertThumbPos : REAL;
                         IN direction : ScrollDirectionType);
   VAR
      offset, localZoomVal, tempXOrig, tempYOrig : REAL;
      zoomHier             : RBDHierObj;
   BEGIN
      IF activeWindow > 0
         zoomHier := ASK root Child("RBDHier", activeWindow);
         localZoomVal := zoomHier.zoom;
         tempXOrig := zoomHier.xOrigin;
         tempYOrig := zoomHier.yOrigin;
      ELSE
         localZoomVal := cusZoomVal;
         tempXOrig := xOrigin;
         tempYOrig := yOrigin;
      END IF;
      FindOffset(ROUND(localZoomVal), offset);
      tempXOrig := 120.*horizThumbPos;
      tempYOrig := 80.-(80.*vertThumbPos);
      IF direction = HorizontalScroll
         ASK root TO SetTranslation(-1.*tempXOrig,-1.*(tempYOrig-((yShift+offset)*(localZoomVal/20.))));
      ELSE
         ASK root TO SetTranslation(-1.*tempXOrig,-1.*(tempYOrig-((yShift+offset)*(localZoomVal/20.))));
      END IF;
      Update;
      IF activeWindow > 0
         zoomHier := ASK root Child("RBDHier", activeWindow);
         ASK zoomHier TO SetOrigin(tempXOrig, tempYOrig);
      ELSE
         xOrigin := tempXOrig;
         yOrigin := tempYOrig;
      END IF;
   END METHOD;

   ASK METHOD SetScroll;
   VAR
      xThumb, yThumb, localZoomVal, tempXOrig, tempYOrig : REAL;
      zoomHier                     : RBDHierObj;
   BEGIN
      IF activeWindow > 0
         zoomHier := ASK root Child("RBDHier", activeWindow);
         localZoomVal := zoomHier.zoom;
         tempXOrig := zoomHier.xOrigin;
         tempYOrig := zoomHier.yOrigin;
      ELSE
         localZoomVal := cusZoomVal;
         tempXOrig := xOrigin;
         tempYOrig := yOrigin;
      END IF;
      xThumb := localZoomVal/120.;
      yThumb := (localZoomVal*13.1/20.)/80.; 
      SetThumbSize(xThumb,yThumb);
      SetThumbStep(xThumb/7.,yThumb/7.);
      SetThumbPosition(tempXOrig/120.,(80.-tempYOrig)/80.);
   END METHOD; {SetScroll}

   ASK METHOD SetPanes(); 
   BEGIN
      IF nowSimulating
         SetNumPanes(9);
         FOR i := 0 TO 8
            ShowStatus(i,"");
         END FOR;
         SetPaneWidth(1,20);
         SetPaneWidth(2,16);
         SetPaneWidth(3,20);
         SetPaneWidth(4,12);
         SetPaneWidth(5,16);
         SetPaneWidth(6,20);
         SetPaneWidth(7,18);
         SetPaneWidth(8,16);
      ELSIF typeOfCursor = fevC;
         SetNumPanes(7);
         FOR i := 0 TO 6
            ShowStatus(i,"");
         END FOR;
         SetPaneWidth(1,20);
         SetPaneWidth(2,21);
         SetPaneWidth(3,21);
         SetPaneWidth(4,21);
         SetPaneWidth(5,23);
         SetPaneWidth(6,18);
      ELSE
         SetNumPanes(7);
         FOR i := 0 TO 6
            ShowStatus(i,"");
         END FOR;
         SetPaneWidth(1,24);
         SetPaneWidth(2,21);
         SetPaneWidth(3,25);
         SetPaneWidth(4,24);
         SetPaneWidth(5,12);
         SetPaneWidth(6,18);
      END IF;
   END METHOD; {SetPanes}

   ASK METHOD InitSimDisplay();
   VAR
      xlo, xhi, ylo, yhi, localZoom, graphZoom : REAL;
      tempHier : RBDHierObj;
      menuitem                               : MenuItemObj;
      buttitem                               : PaletteButtonObj;
   BEGIN
      IF activeWindow > 0
         tempHier := ASK root Child("RBDHier", activeWindow);
         localZoom := tempHier.zoom;
      ELSE
         localZoom := cusZoomVal;
      END IF;
      graphYOff := 0.;
      IF DisplayAoGraph
         SetGraphZoom(graphZoom, simZoomX, simZoomY); {zoomissue - aograph}
      END IF;
      SetScrollable(TRUE,TRUE);
      SetScroll;
      SetPanes;
      ASK grid TO Colorize("Sim");
      NEW(greenFace);
      NEW(yellowFace);
      NEW(redFace);
      ASK greenFace TO LoadFromLibrary(images,SUBSTR(1,4,systemImage)+"_G"); 
      ASK yellowFace TO LoadFromLibrary(images,SUBSTR(1,4,systemImage)+"_Y"); 
      ASK redFace TO LoadFromLibrary(images,SUBSTR(1,4,systemImage)+"_R"); 
      AddGraphic(greenFace);
      AddGraphic(yellowFace);
      AddGraphic(redFace);
      symbolScale := .45;
      symbolOffset := 48.;
      IF DisplayAoGraph
         graphYOff := 250.;
      END IF;
      ASK greenFace TO SetScaling(symbolScale,symbolScale);
      ASK greenFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff)-widescreenOffset);       {cmc 5/10/07}
      ASK yellowFace TO SetScaling(symbolScale,symbolScale);
      ASK yellowFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff)-widescreenOffset);      {cmc 5/10/07}
      ASK redFace TO SetScaling(symbolScale,symbolScale);
      ASK redFace TO SetTranslation(19.+symbolOffset,635.-(symbolOffset+graphYOff)-widescreenOffset);         {cmc 5/10/07}
      IF DisplayAoGraph
         IF widescreen                    {cmc 5/10/07  graph}    
            SetSize(100., 60.);           {cmc 5/10/07  graph}
            SetTranslation(0., 40.);      {cmc 5/10/07  graph}
         ELSE                             {cmc 5/10/07  graph}
            SetSize(100., 65.);         
            SetTranslation(0., 35.); 
         END IF;                          {cmc 5/10/07  graph}
         
         
        { IF graphZoom > localZoom
            localZoom := graphZoom;
            IF activeWindow > 0
               tempHier := ASK root Child("RBDHier", activeWindow);
               ASK tempHier TO SetZoom(localZoom);
            ELSE
               cusZoomVal := localZoom;
            END IF;
         END IF;}
         IF graphZoom < localZoom {only adjust zoom for graph if calculated graphzoom is larger than current localzoom}
            graphZoom := localZoom;
         END IF;
         dontChangeXY := TRUE; {zoomissue - aograph}
         SetView(graphZoom, simZoomX, simZoomY);
         dontChangeXY := FALSE;
         changedZoom := TRUE;
         graphYOff := 250.;
         ASK window TO Update;
      END IF;  
      IF dSimWithGraph
         faceBoxOpen := TRUE;
         DisplayFace(1);
         ShowStatus(0,"Sim Speed : "+SUBSTR(1,8,REALTOSTR(dTimeSlice)));
      END IF; 
      ShowStatus(1,"Simulating...");
      IF compileType = "demo"
         ASK simToolBar TO Draw;
         ASK simMenuBar TO Draw;
         menuitem := ASK simMenuBar Descendant("PrintItem", 101);
         ASK menuitem TO Deactivate;
         menuitem := ASK simMenuBar Descendant("SetupItem", 102);
         ASK menuitem TO Deactivate;
         menuitem := ASK simMenuBar Descendant("SaveBMPItem", 103);
         ASK menuitem TO Deactivate;
         buttitem := ASK simToolBar Descendant("PrintButton", 901);
         ASK buttitem TO Deactivate;
      END IF;
   END METHOD; {InitSimDisplay}
   

ASK METHOD InitFEVDisplay();
   VAR
      xlo,xhi,ylo,yhi : REAL;
      menuitem                               : MenuItemObj;
      buttitem                               : PaletteButtonObj;
   BEGIN
      SetPanes;
      ASK grid TO Colorize("FEV");
      NEW(greenFace);
      NEW(yellowFace);
      NEW(redFace);
      ASK greenFace TO LoadFromLibrary(images,SUBSTR(1,4,systemImage)+"_G"); 
      ASK yellowFace TO LoadFromLibrary(images,SUBSTR(1,4,systemImage)+"_Y"); 
      ASK redFace TO LoadFromLibrary(images,SUBSTR(1,4,systemImage)+"_R"); 
      AddGraphic(greenFace);
      AddGraphic(yellowFace);
      AddGraphic(redFace);
      symbolScale := .45;
      symbolOffset := 48.;
      ASK greenFace TO SetScaling(symbolScale,symbolScale);
      ASK greenFace TO SetTranslation(19.+symbolOffset,635.-symbolOffset-widescreenOffset);         {cmc 5/10/07}
      ASK yellowFace TO SetScaling(symbolScale,symbolScale);
      ASK yellowFace TO SetTranslation(19.+symbolOffset,635.-symbolOffset-widescreenOffset);        {cmc 5/10/07}
      ASK redFace TO SetScaling(symbolScale,symbolScale);
      ASK redFace TO SetTranslation(19.+symbolOffset,635.-symbolOffset-widescreenOffset);           {cmc 5/10/07}
      ASK menuTool TO SetHidden(TRUE);
      ASK menubar TO SetHidden(TRUE);
      ASK fevToolBar TO SetHidden(FALSE); 
      ASK fevMenuBar TO SetHidden(FALSE); 
      faceBoxOpen := TRUE;
      DisplayFace(1);
      ShowStatus(0,"Failure Effects View...");
      IF compileType = "demo"
         ASK fevToolBar TO Draw;
         ASK fevMenuBar TO Draw;
         menuitem := ASK fevMenuBar Descendant("OpenItem", 101);
         ASK menuitem TO Deactivate;
         menuitem := ASK fevMenuBar Descendant("WriteItem", 102);
         ASK menuitem TO Deactivate;
         menuitem := ASK fevMenuBar Descendant("CreateSingleItem", 103);
         ASK menuitem TO Deactivate;
         menuitem := ASK fevMenuBar Descendant("PrintWinItem", 104);
         ASK menuitem TO Deactivate;
         menuitem := ASK fevMenuBar Descendant("SetupItem", 105);
         ASK menuitem TO Deactivate;
         menuitem := ASK fevMenuBar Descendant("SaveBMPItem", 106);
         ASK menuitem TO Deactivate;
         buttitem := ASK fevToolBar Descendant("PrintButton", 901);
         ASK buttitem TO Deactivate;
         buttitem := ASK fevToolBar Descendant("OpenFile", 903);
         ASK buttitem TO Deactivate;
         buttitem := ASK fevToolBar Descendant("EditFile", 904);
         ASK buttitem TO Deactivate;
         buttitem := ASK fevToolBar Descendant("CloseFile", 905);
         ASK buttitem TO Deactivate;
      END IF;
   END METHOD; {InitFEVDisplay}

   
   
   
   ASK METHOD DisplayFace(IN faceColor : INTEGER);
   BEGIN
      CASE faceColor
         WHEN 1:
            ASK yellowFace TO SetHidden(TRUE);
            ASK yellowFace TO Draw;
            ASK redFace TO SetHidden(TRUE);
            ASK redFace TO Draw;
            ASK greenFace TO SetHidden(FALSE);
            ASK greenFace TO Draw;
            faceVisible := greenFace;
         WHEN 2:
            ASK greenFace TO SetHidden(TRUE);
            ASK greenFace TO Draw;
            ASK redFace TO SetHidden(TRUE);
            ASK redFace TO Draw;
            ASK yellowFace TO SetHidden(FALSE);
            ASK yellowFace TO Draw;
            faceVisible := yellowFace;
         WHEN 3:
            ASK greenFace TO SetHidden(TRUE);
            ASK greenFace TO Draw;
            ASK yellowFace TO SetHidden(TRUE);
            ASK yellowFace TO Draw;
            ASK redFace TO SetHidden(FALSE);
            ASK redFace TO Draw;
            faceVisible := redFace;
         WHEN 4: {hide all faces}
            ASK greenFace TO SetHidden(TRUE);
            ASK greenFace TO Draw;
            ASK yellowFace TO SetHidden(TRUE);
            ASK yellowFace TO Draw;
            ASK redFace TO SetHidden(TRUE);
            ASK redFace TO Draw;
         WHEN 5:
            DISPOSE(greenFace);
            DISPOSE(yellowFace);
            DISPOSE(redFace);
      END CASE;                   
   END METHOD; {DisplayFace}
END OBJECT; {mainWindowObj}

OBJECT graphWindowObj;
   ASK METHOD Startup;
   BEGIN
      NEW(graphMenuBar);
      ASK graphMenuBar TO LoadFromLibrary(dialogs,"GraphMenuBar");
      ASK AoGraphWindow TO AddGraphic(graphMenuBar);
      
      IF widescreen                               {cmc 5/10/07  graph}  
         ASK AoGraphWindow TO SetSize(100.,40.);  {cmc 5/10/07  graph}    
      ELSE                                        {cmc 5/10/07  graph}
         ASK AoGraphWindow TO SetSize(100.,35.);       
      END IF;                                     {cmc 5/10/07  graph}
                       
      ASK AoGraphWindow TO SetTranslation(0., 0.);
      ASK AoGraphWindow TO SetMappingMode(XMajorMap);
      ASK AoGraphWindow TO ShowWorld(0., 0., 100., 100.);
      ASK AoGraphWindow TO ZoomIn(0.,0.,100.,140.);
      ASK AoGraphWindow TO SetColor(simColor);
      ASK graphMenuBar TO Draw;
   END METHOD; {Startup}
   
   ASK METHOD MouseClick(IN x, y : REAL;
                         IN buttondown : BOOLEAN);
   BEGIN
      IF (Button = 2) AND buttondown
         IF currentGraph = 1
            currentGraph := 2;
            ASK AoGraphWindow TO SetTitle("Interval Ao Graph");
            ASK AoGraph TO SetHidden(TRUE);
            ASK IntAoGraph TO SetHidden(FALSE);
            ASK MultiRunGraph TO SetHidden(TRUE);  
         ELSIF currentGraph = 2 
            currentGraph := 3;
            ASK AoGraphWindow TO SetTitle("Multi-run Ao Graph");
            ASK AoGraph TO SetHidden(TRUE);
            ASK IntAoGraph TO SetHidden(TRUE);
            ASK MultiRunGraph TO SetHidden(FALSE);
         ELSE             
            currentGraph := 1;
            ASK AoGraphWindow TO SetTitle("Per Trial Ao Graph");
            ASK AoGraph TO SetHidden(FALSE);
            ASK IntAoGraph TO SetHidden(TRUE);
            ASK MultiRunGraph TO SetHidden(TRUE);
         END IF;
         ASK AoGraphWindow TO Draw;
      END IF;  
   END METHOD {MouseClick};

   ASK METHOD BeClosed;
   VAR
      tempHier : RBDHierObj;
      tempZoom, tempX, tempY : REAL;
   BEGIN
      DisplayAoGraph := FALSE;
      AvailGraph := FALSE;
      ASK window TO SetDeferral(TRUE);
      ASK window TO SetSize(100., 100.);
      ASK window TO SetTranslation(0., 0.);
      IF activeWindow <> 0
         tempHier := ASK root Child("RBDHier", activeWindow);
         tempZoom := tempHier.zoom;
         tempX := tempHier.xOrigin;
         tempY := tempHier.yOrigin;
      ELSE
         tempZoom := cusZoomVal;
         tempX := xOrigin;
         tempY := yOrigin;
      END IF;
      SetView(tempZoom, tempX, tempY); 
      ASK window TO SetDeferral(FALSE);
      ASK window TO Update;
      DISPOSE(AoGraph);
      DISPOSE(IntAoGraph);
      DISPOSE(MultiRunGraph);
      DISPOSE(AoGraphWindow);
      IF dSimWithGraph
         ASK greenFace TO SetTranslation(19.+symbolOffset,635.-symbolOffset-widescreenOffset);       {cmc 5/10/07}
         ASK yellowFace TO SetTranslation(19.+symbolOffset,635.-symbolOffset-widescreenOffset);      {cmc 5/10/07}
         ASK redFace TO SetTranslation(19.+symbolOffset,635.-symbolOffset-widescreenOffset);         {cmc 5/10/07}
         ASK faceVisible TO Draw;
      END IF;
   END METHOD; {BeClosed}
END OBJECT; {graphWindowObj}

OBJECT selectGroupObj;
   ASK METHOD Rank(IN a,b : ANYOBJ) : INTEGER;
   VAR
      tempA, tempB  : RBDBasicObj;
   BEGIN
      tempA := a;
      tempB := b;
      {IF tempA.objectNum < tempB.objectNum
         RETURN 1;
      ELSE}
         RETURN -1;
      {END IF;}
   END METHOD; {Rank}
END OBJECT; {selectGroupObj}
END MODULE. {imod display}


