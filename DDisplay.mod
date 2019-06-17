{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Definition Module : Display                                                   +}
{+  Author            : Steve Brown                                               +}             
{+  Last Modified     : 22 August 08  wds/TES                                     +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

DEFINITION MODULE Display;

FROM GMedia  IMPORT SoundObj;
FROM GrpMod  IMPORT QueueObj;
FROM Image   IMPORT ImageObj;
FROM Window  IMPORT WindowObj;
FROM GTypes  IMPORT ScrollDirectionType(HorizontalScroll, VerticalScroll),  ColorType;
FROM GrpMod  IMPORT RankedObj;
FROM Cursor  IMPORT LineCursorObj;
FROM Menu    IMPORT PopupMenuObj;
FROM Objects IMPORT RBDBlockObj,RBDEventObj,LinkObj,RBDNodeObj,RBDHierObj,intArray,realArray,SparePoolObj,PhaseObj;
FROM GPalet  IMPORT PaletteButtonObj;
FROM Graphic IMPORT GraphicLibObj;
FROM Chart   IMPORT ChartObj;
FROM Menubar IMPORT simMenuObj, mainMenuObj, graphMenuObj, simToolObj,
                    fevToolObj,fevMenuObj,mainToolObj, weakMenuObj, weakToolObj;
FROM IOMod   IMPORT StreamObj;

TYPE
   cursorType = (blockC, nodeC, eventC, connectC, nilC, dialogC, simC, fevC, weakC, autoC, hierC);
   CArrayType = ARRAY INTEGER OF CHAR;   { wds/TES 8/14/08}

   GridObj = OBJECT(ImageObj);
      ASK METHOD Colorize(IN case : STRING);
      OVERRIDE
         ASK METHOD ObjInit;
   END OBJECT;   

   safeSoundObj = OBJECT(SoundObj);
      pID : INTEGER;
      ASK METHOD PlayMe(IN fileName : STRING);
   END OBJECT;

   mainWindowObj = OBJECT(WindowObj);
      {draggingblock,draggingnode,dragginghier,}
      justCtrled,cancelAddMode            : BOOLEAN;
      ASK METHOD KillPopup;
      ASK METHOD ShowRightClick(IN whatToShow    : INTEGER);
      ASK METHOD SetScroll;
      ASK METHOD SetPanes;
      ASK METHOD InitSimDisplay();
      ASK METHOD InitFEVDisplay();      
      ASK METHOD DisplayFace(IN faceColor : INTEGER);
      OVERRIDE
         ASK METHOD BeResized; 
         ASK METHOD BeClosed;
         ASK METHOD MouseClick(IN x, y       : REAL;
                               IN buttondown : BOOLEAN);
         ASK METHOD MouseMove (IN x, y  : REAL);
         ASK METHOD BeScrolled(IN horizThumbPos, vertThumbPos : REAL;
                               IN direction : ScrollDirectionType);
   END OBJECT; {mainWindowObj}

   selectGroupObj = OBJECT(RankedObj);
      OVERRIDE
         ASK METHOD Rank(IN a,b : ANYOBJ) : INTEGER;
   END OBJECT; {selectGroupObj}

   graphWindowObj = OBJECT(WindowObj);
      ASK METHOD Startup;
      OVERRIDE
         ASK METHOD BeClosed;
         ASK METHOD MouseClick(IN x, y       : REAL;
                               IN buttondown : BOOLEAN);
   END OBJECT; {outputWindowObj}

   PROCEDURE SetTextSize;
   PROCEDURE InitDisplay   (OUT exploded   : BOOLEAN);
   PROCEDURE AddStartEndNodes;
   PROCEDURE AddBlock;
   PROCEDURE AddEvent;
   PROCEDURE AddHier;
   PROCEDURE AddConnector;
   PROCEDURE AddNode;
   PROCEDURE ClearAllBlocks;
   PROCEDURE ClearObject;
   PROCEDURE EditDetails;
   PROCEDURE CopyObject;
   PROCEDURE SelectAll;
   PROCEDURE PasteObject;
   PROCEDURE ValidateRBD   (OUT valid      :BOOLEAN);
   PROCEDURE SendToEngine;
   PROCEDURE StartFailureEffects;
   PROCEDURE EndFailureEffects;
   PROCEDURE ChangeWindow(IN newId, newLevel : INTEGER);
   PROCEDURE AddWindow(IN newId : INTEGER);
   PROCEDURE SelectBlock;
   PROCEDURE SelectEvent;
   PROCEDURE SelectHier;
   PROCEDURE SelectNode;
   PROCEDURE CheckOpenFileStatus;
   PROCEDURE InitAoGraph;
   PROCEDURE HandleLinks   (IN addingItem  : BOOLEAN);
   PROCEDURE ResetPasted;
   PROCEDURE CheckTimeBomb (OUT exploded   : BOOLEAN);
   PROCEDURE SetView       (IN cusZoom,xIn,yIn     : REAL);
   PROCEDURE CheckTimeBomb (OUT exploded   : BOOLEAN);
   PROCEDURE ShowAnalView  (IN display     : BOOLEAN);
   PROCEDURE GetPassword(IN today : STRING; OUT pword :STRING);      
   PROCEDURE ConvertNumber(INOUT a : STRING);   
   PROCEDURE ZoomPercent(IN ClickX, ClickY : REAL; IN center : BOOLEAN);
   PROCEDURE ZoomFit;
   PROCEDURE CollapseIntoHier;
   PROCEDURE ClearPasteBuffer;
   
   
   PROCEDURE getXres() : INTEGER ; NONMODSIM "CPP";  
   PROCEDURE getYres() : INTEGER ; NONMODSIM "CPP";            
   
   {wds/TES - mods for finding user's "Application Data" directory, 8/14/08}
   PROCEDURE getUserFolderPath();  NONMODSIM "CPP";
   PROCEDURE getUserMyDocsPath();  NONMODSIM "CPP";
  
   
   

CONST
   aspectRatio = 2.9/3.; {y to x}
   CompileType = "release"; {*** coordinate levelLimit with compileType ***}   
   training = FALSE; {Set to TRUE to build training version}
   expireDate  = 1491;  {Julian date starting from Jan 31, 2001} {1491 ~ 31 Jan 2005} {not used}
   elimiDays = 120; 
   demoCrippleLimit = 10;  {Number of demo version blocks allowed}
   studentCrippleLimit = 20; {Number of student version blocks allowed}
   levelLimit = 5; {*** compileType = "release" or "student" or "demo" ***}
   {levelLimit = 2;} {*** compileType = "gmd" ***};
   
VAR
   numberOfRuns,totalObjects,totalBlocks,totalEvents,totalHiers,totalLinks,totalNodes,endId,
   totalPools,startId,fontSize,activePhases,currentGraph,blueObjId,
   flowGenerated, saveInc, lastSave, weakLinkAnalType, linksIn, 
   totalTriggers, hierLevel, activeWindow, deepestLevel, termType,
   nodesIn, blocksIn, eventsIn, hiersIn, copyWindow, nextId, nextLinkId              : INTEGER;
   cusZoomVal, oldCusVal, fevZoomVal, xOrigin,yOrigin,simLength,dTimeTrunc,
   dNumberOfRuns,dTimeSlice,yMin,yMax,dTimeStartTime,dFailStartTime,
   dFailTrunc,dCycleStartTime, dCycleTrunc, graphYOff,symbolOffset,simZoomX,simZoomY,
   timeSlice, systemRedCost, sysLostCost, GYthreshold, YRthreshold,
   symbolScale                                                           : REAL;
   simulated,somethingChanged,simOptionChanged,fileIsOpen,
   nowSimulating,nodesAnalysed,gridIsOn,
   openMenuFile,lambdaMode,muSigmaMode,soundIsOn,copied,
   dSimWithGraph,negShutUp,result,linkCancelled,AoVisible,
   DisplayAoGraph,addingEvent,nowInitialing,faceBoxOpen,editor,
   doneSimming, nowPasting, capacityAnalysis,costAnalysis,weakAnalysis, 
   startStep, saveIsOn, statusBarOn, dZeroFail, analUp,
   analViewAvail, configFrozen, hitXinSim, changedZoom, openFromIcon,
   pastingMultiple, diagnostics, simDetails, devmode, password, writeBlkEvs,writeNdEvs,writeSysEvs,
   writePhsEvs,writeOnlyItemEvs,DomainTree,CoreFile, importing, collapsing, 
   copiedFromPrevFile, dontChangeXY, ignoreMouse {eag error 51 fix}, C130File {cmc} : BOOLEAN;
   pathName, graphicsPath, nameOfFile, filter, blueObjRef, {imageString,} systemImage, globalImage,
   systemUnits, globalUnits, soundsPath, exampPath, soundPath, flowUnits, devVersion, compileType,
   devDate,installPath,rbdPath,reportPath,libraryPath,diagFile,sysComment, currentView, detComp : STRING;
   linkCursor                                                            : LineCursorObj;
   typeOfCursor                                                          : cursorType;
   grid                                                                  : GridObj;
   window                                                                : mainWindowObj;
   hier                                                                  : RBDHierObj;
   {hier,}block,defaultBlock                                               : RBDBlockObj;
   event                                                                 : RBDEventObj;
   node                                                                  : RBDNodeObj;
   link                                                                  : LinkObj;
   popupMenu                                                             : PopupMenuObj;
   sound                                                                 : safeSoundObj;
   selectGroup                                                           : selectGroupObj; 
   greenButton,yellowButton,redButton                                    : PaletteButtonObj;
   baseroot,root,scaleroot,faceVisible,greenFace,redFace,yellowFace,
   internal,border                                                       : ImageObj;
   dialogs,images                                                        : GraphicLibObj;
   AoGraph,IntAoGraph,MultiRunGraph                                      : ChartObj;
   AoGraphWindow                                                         : graphWindowObj;
   PoolArray                                                             : ARRAY INTEGER OF SparePoolObj;
   phaseTimes                                                            : realArray;
   phaseObjArray                                                         : ARRAY INTEGER OF PhaseObj;
   sysStreams                                                            : intArray;
   simMenuBar                                                            : simMenuObj;
   weakMenuBar                                                           : weakMenuObj;
   fevMenuBar                                                            : fevMenuObj;
   menubar                                                               : mainMenuObj;
   graphMenuBar                                                          : graphMenuObj;
   simToolBar                                                            : simToolObj;
   weakToolBar                                                           : weakToolObj;
   fevToolBar                                                            : fevToolObj;
   menuTool                                                              : mainToolObj;
   selectedLinksGroup,blockGroup, nodeGroup, eventGroup, 
   hierGroup, linkGroup, capLinkGroup, capNodeGroup                      : QueueObj;
   diagnosticsStream, simDetailsStream                                   : StreamObj;
   simColor,workColor,goggleColor,fevColor,gridColor,
   linkWorkColor, textWorkColor, blockGUIColor                           : ColorType;
   
   {wds/TES - mods for finding user's "Application Data" directory}
   userPath                                                              : STRING;
   {wds/TES - mods for finding user's "MyDocuments" directory}
   docsPath                                                              : STRING;
   

END MODULE. {Display}   

