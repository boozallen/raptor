{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Implementation Module : Intract                                               +}
{+  Author        : Steve Brown / Elizabeth Grimes                                +}
{+  Last Modified : 08/22/08  wds/TES                                             +}
{+  Description   : This module takes care of the dialog boxes used in RAPTOR     +}
{+                  (except simulation).  DBs are initialized here, actions       +}
{+                  controlled, data pulled off and error checked.                +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

IMPLEMENTATION MODULE Intract;

FROM Graphic IMPORT GraphicVObj;
FROM Form    IMPORT MessageDialogBoxObj, MessageStyleType(AlertMessage), 
                    MessageResponseType(YesNoResponse,YesNoCancelResponse),
                    MessageButtonType(OkButton,CancelButton,YesButton,NoButton);
FROM GProcs  IMPORT HandleEvents;
FROM OSMod   IMPORT Delay, MicroDelay, ViewHelp, FileExists, GetProgDir, SystemCall, StartBGTask;
FROM IOMod   IMPORT StreamObj, FileUseType(Input, Output);
FROM GTypes  IMPORT DBPositionType(BottomLeft), TextBufferType, ALL SysCursorType;
FROM Runsim  IMPORT FinalArray, LogArray, resultsBox, SparesArray, BlockCostArray,
                    CapacityArray, NodeArray, BlockArray, EventArray, HierArray, REALTOCOST, COSTTOREAL, UpdateNodeArray,
                    UpdateBlockArray, NumRunsCompleted, FinalArrayType, WriteFMECAtoFile, FMECAarray, FevDepGroup;
FROM Display IMPORT dialogs, window, simulated, root, somethingChanged, activePhases, phaseObjArray, 
                    totalBlocks, totalEvents, totalNodes, totalHiers, totalLinks, startId, endId, lambdaMode, muSigmaMode, sound, 
                    totalObjects, systemUnits, soundsPath, dSimWithGraph, 
                    dTimeTrunc, dFailTrunc, dCycleTrunc, dTimeStartTime, dFailStartTime, dCycleStartTime, sysLostCost, 
                    flowGenerated, systemRedCost, devDate, systemImage, sysStreams, negShutUp,
                    devVersion, costAnalysis, weakLinkAnalType, GYthreshold, YRthreshold, installPath, nextId, 
                    compileType, activeWindow,termType, ChangeWindow, totalTriggers, dNumberOfRuns,sysComment,
                    saveIsOn, soundIsOn, soundPath, saveInc, lastSave, globalUnits, globalImage, muSigmaMode, lambdaMode;
FROM Print   IMPORT ExportRBD, PrintSummaryOutput, PrintSparingOutput, PrintCapacityOutput,
                    PrintAnalOutput, PrintCostOutput, PrintCostInput, SaveSummaryOutput,
                    SaveSparingOutput, SaveCapacityOutput, SaveAnalOutput, SaveCostOutput,
                    SaveCostInput, PrintEventPhases, SaveEventPhases, PrintNodePhases,
                    SaveNodePhases, PrintBlockPhases, SaveBlockPhases, PrintPhases, PrintInputTable,
                    PrintFMECA, SaveSystemInput, PrintSystemInput, PrintLogisticsOutput, SaveLogisticsOutput,
                    PrintHierPhases, SaveHierPhases, PrintTree, SavePhases;
FROM MathMod IMPORT POWER, LN, EXP, SQRT, pi;
FROM Objects IMPORT RBDBlockObj, LinkObj, RBDNodeObj, RBDBasicObj, SparePoolObj, RapTriggerObj, poolGroup, 
                    totalSpares, totalRes, spareList, resList, RapTriggerObj, triggerGroup,
                    trigList, PhaseObj, RBDHierObj, ALL BlockStatusType, ALL EventStatusType, CheckColds, AnalyzeSystem;
FROM FileFx  IMPORT GetFileName, ReadData;
FROM Menubar IMPORT libArray, nameArray, libVersion;
FROM GTree   IMPORT TreeObj, TreeItemObj;
FROM ListBox IMPORT ListBoxObj, ListBoxItemObj;
FROM Menu    IMPORT MenuItemObj;
FROM GrpMod  IMPORT QueueObj;
FROM UtilMod IMPORT ClockRealSecs;
{FROM RoboHelpCSH IMPORT ShowHelp;}

FROM Display IMPORT userPath;  { wds/TES, 8/22/08 }

VAR
   fEmpMean, rEmpMean                                       : REAL;
   tempTotalPhases, tempOldPhases, tablesFull,
   errors, flag1, distType, nParm, numDataPoints,
   phaseBlocks, phaseNodes, phaseEvents, phaseHiers, distnum,
   oldFailNum, oldRepNum, npars, tempPhaseDist, anth        : INTEGER;
   lastClicked                                              : GraphicVObj;
   button, insertButton                                     : ButtonObj;
   message                                                  : TextBufferType;
   result, empFlag, failType, printPhase, cellSelected      : BOOLEAN;
   {fEmpArray, rEmpArray,} tempPhaseParams                    : realArray;
   axedPhases                                               : ARRAY INTEGER OF INTEGER;
   spCostText, failText, repairText, timeLabel2,
   stockTxt1, emerText, timeLabel1, newSparesText, 
   stockTxt2, initStock, failMean, timeLabel3, stockTxt3,
   timeLabel4, timeLabel5, shipCostText, idleCostText,
   holdCostText, timeLabel6, standbyCostTxt, degradeTxt1,
   repairMean, repairSTDev, degradeTxt2, degradeTxt3,
   SBStressText, ancStreamText,
   res1Txt1, res2Txt1, res3Txt1, res4Txt1, res5Txt1,
   res1Txt2, res2Txt2, res3Txt2, res4Txt2, res5Txt2,
   res1Txt3, res2Txt3, res3Txt3, res4Txt3, res5Txt3, 
   unitsLabel1, unitsLabel2, unitsLabel3, unitsLabel4, 
   unitsLabel5, pTextBox, resAcqText, poolTypeText, 
   afterFailText, forPMText, defineDepText, opText, 
   idleText, PMText, failedText, blockTypeText, BPhaseLabel, 
   NPhaseLabel, EPhaseLabel, HPhaseLabel, distLabel, unitsLabel,
   PMText2, defaultDepText                                  : LabelObj;
   coldList1, coldList2, capList1, capList2, capList3,
   buddyList2                                               : OptionListType;
   sort1, sort2, sort3, sort4, sort5, sort6, sort7, sort8, 
   sort9, tableUp, lastType, distribution, tempString       : STRING;
   SBStressVal, res1ReqPM, res2ReqPM, res3ReqPM, res4ReqPM, 
   res5ReqPM, res1Req, res2Req, res3Req, res4Req, res5Req,
   pValBox, priorityValBox, opValBox, idleValBox, PMValBox,
   failValBox, lifeExBox                                    : ValueBoxObj;
   hoardRadBox, blockRadBox                                 : RadioBoxObj;
   hoardRadButton, antiRadButton, priorityRadButton,
   idleRadButton, opRadButton                               : RadioButtonObj;
  
PROCEDURE CallHelp(IN helpCode   : INTEGER);
VAR
   procID      : INTEGER;
   pID         : INTEGER;   {wds/TES, 8/26/08 }
   
BEGIN
   {helpfile needs installPath if open raptor by file associated}
   IF (compileType = "release")    
      { procID := ViewHelp(installPath + "help\HelpFile70.hlp", helpCode); wds/TES, 8/22/08 }
      pID := SystemCall("Help\HelpFile70.chm",0);   { wds/TES, 8/26/08 }
   ELSIF (compileType = "gmd")
      pID := SystemCall("Help\HelpFile70.chm",0);   { wds/TES, 8/26/08 }
   ELSIF (compileType = "student")
      pID := SystemCall("Help\HelpFile70Demo.chm",0);   { wds/TES, 8/26/08 }
   ELSE
      { procID := ViewHelp(installPath + "help\HelpFile70Demo.hlp", 0); wds/TES, 8/22/08 }
      pID := SystemCall("Help\HelpFile70Demo.chm",0);   { wds/TES, 8/26/08 }
   END IF;
   { wds/TES, 8/26/08 - added check below }
   IF pID = -1
      NEW(message, 1..3);
      message[1] := "ERROR: Unable to open Raptor Help file.     ";
      result := SendAlert(message, FALSE, FALSE, TRUE);
      DISPOSE(message);
   END IF;
END PROCEDURE; {CallHelp}

PROCEDURE ConvertToString(IN  distroNum  : INTEGER;
                          OUT distroName : STRING);
BEGIN
   CASE distroNum
      WHEN 1:  distroName := "Beta";
      WHEN 2:  distroName := "Chi-square";
      WHEN 3:  distroName := "Binomial";
      WHEN 4:  distroName := "Exponential";
      WHEN 5:  distroName := "Erlang";
      WHEN 6:  distroName := "Gamma";
      WHEN 7:  distroName := "Lognormal";
      WHEN 8:  distroName := "Normal";
      WHEN 9:  distroName := "Uniform Integer";
      WHEN 10: distroName := "Uniform Real";
      WHEN 11: distroName := "Pearson 5";
      WHEN 12: distroName := "Pearson 6";
      WHEN 13: distroName := "Poisson";
      WHEN 14: distroName := "Triangular";
      WHEN 15: distroName := "Weibull";
      WHEN 16: distroName := "Empirical";
      WHEN 17: distroName := "Point Estimate";
      WHEN 18: distroName := "None";
      WHEN 19: distroName := "Fixed";
      WHEN 20: distroName := "Exponential Power";
      WHEN 21: distroName := "Extreme Value";
      WHEN 22: distroName := "Laplace";
      OTHERWISE
         NEW(message, 1..1);
         message[1] := "ERROR: Unknown Distribution Type!     ";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
         distroName := "UNKNOWN";
   END CASE;
END PROCEDURE; {ConvertToString}

PROCEDURE ConvertToInteger(IN distro : STRING;
                           OUT distroNum, nPars : INTEGER);
BEGIN
   CASE distro
      WHEN "Beta":              distroNum := 1; nPars := 2;
      WHEN "Chi-square":        distroNum := 2; nPars := 1;
      WHEN "Binomial":          distroNum := 3; nPars := 2;
      WHEN "Exponential":       distroNum := 4; nPars := 2;
      WHEN "Erlang":            distroNum := 5; nPars := 2;
      WHEN "Gamma":             distroNum := 6; nPars := 3;
      WHEN "Lognormal":         distroNum := 7; nPars := 2;
      WHEN "Normal":            distroNum := 8; nPars := 2;
      WHEN "Uniform Integer":   distroNum := 9; nPars := 2;
      WHEN "Uniform Real":      distroNum := 10; nPars := 2;
      WHEN "Pearson 5":         distroNum := 11; nPars := 3;
      WHEN "Pearson 6":         distroNum := 12; nPars := 3;
      WHEN "Poisson":           distroNum := 13; nPars := 1;
      WHEN "Triangular":        distroNum := 14; nPars := 3;
      WHEN "Weibull":           distroNum := 15; nPars := 3;
      WHEN "Empirical":         distroNum := 16; nPars := 0;
      WHEN "Point Estimate":    distroNum := 17; nPars := 1;
      WHEN "None":              distroNum := 18; nPars := 1;         
      WHEN "Fixed":             distroNum := 19; nPars := 1; 
      WHEN "Exponential Power": distroNum := 20; nPars := 3;
      WHEN "Extreme Value"    : distroNum := 21; nPars := 2;
      WHEN "Laplace"          : distroNum := 22; nPars := 2;
      OTHERWISE
         NEW(message, 1..1);
         message[1] := "ERROR: Unknown distribution selected!     ";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
         distroNum := 4;
         nPars := 2;
   END CASE;
END PROCEDURE; {ConvertToInteger}
   
PROCEDURE GetEmpirical();
VAR
   swap                                     : REAL;
   i, pass                                  : INTEGER;
   validData, changed                       : BOOLEAN;
   dataBox                                  : DataBoxObj;       
   numPtsText,ptsInText                     : LabelObj;
   dataTable                                : TableObj;
   dataEntryBox                             : ValueBoxObj;
   deleteButton                             : ButtonObj;
BEGIN
   NEW(dataBox);
   ASK dataBox TO LoadFromLibrary(dialogs, "EmpDataBox");
   ASK window TO AddGraphic(dataBox);
   deleteButton := ASK dataBox Child("DeleteButton", 100);
   dataTable := ASK dataBox Child("DataTable", 101);
   ptsInText := ASK dataBox Child("PtsInText", 103);
   numPtsText := ASK dataBox Child("NumPtsText", 104);
   dataEntryBox := ASK dataBox Child("DataEntryBox", 105);
   IF (failType AND (oldFailNum = 16))
      numDataPoints := HIGH(fEmpArray);
      ASK numPtsText TO SetLabel(INTTOSTR(numDataPoints));
      FOR i := 1 TO numDataPoints
         ASK dataTable TO SetText(REALTOSTR(fEmpArray[i]), 2, i);
      END FOR;
   ELSIF ((NOT failType) AND (oldRepNum = 16))
      numDataPoints := HIGH(rEmpArray);
      ASK numPtsText TO SetLabel(INTTOSTR(numDataPoints));
      FOR i := 1 TO numDataPoints
         ASK dataTable TO SetText(REALTOSTR(rEmpArray[i]), 2, i);
      END FOR;
   ELSE
      numDataPoints := 0;
   END IF;
   FOR i := 1 TO dataTable.NumberRows
      ASK dataTable TO SetText(INTTOSTR(i)+".", 1, i);
   END FOR;
   ASK dataBox TO Draw;
   ASK deleteButton TO Deactivate;
   ASK dataEntryBox TO ReceiveFocus;
   button := ASK dataBox TO AcceptInput();
   IF ASK button ReferenceName = "OKButton"
      IF (numDataPoints > 0) AND failType
         fEmpMean := 0.;
         IF fEmpArray <> NILARRAY
            DISPOSE(fEmpArray);
         END IF;
         NEW (fEmpArray, 1..numDataPoints);
         FOR i := 1 TO numDataPoints
            fEmpArray[i] := STRTOREAL(ASK dataTable Text(2,i));
            fEmpMean := fEmpMean + STRTOREAL(ASK dataTable Text(2,i));
         END FOR;
         FOR pass := 1 TO numDataPoints-1
            changed := FALSE;
            FOR i := 1 TO numDataPoints-1
               IF fEmpArray[i] > fEmpArray[i+1]
                  swap := fEmpArray[i];
                  fEmpArray[i] := fEmpArray[i+1];
                  fEmpArray[i+1] := swap;
                  changed := TRUE;
               END IF;
            END FOR;
            IF NOT changed
               EXIT;
            END IF;
         END FOR;
         fEmpMean := fEmpMean/FLOAT(numDataPoints);
         oldFailNum := 16;
      ELSIF (numDataPoints > 0) AND (NOT failType)
         rEmpMean := 0.;
         IF rEmpArray <> NILARRAY
            DISPOSE(rEmpArray);
         END IF;
         NEW (rEmpArray, 1..numDataPoints);
         FOR i := 1 TO numDataPoints
            rEmpArray[i] := STRTOREAL(ASK dataTable Text(2,i));
            rEmpMean := rEmpMean + STRTOREAL(ASK dataTable Text(2,i));
         END FOR;
         FOR pass := 1 TO numDataPoints-1
            changed := FALSE;
            FOR i := 1 TO numDataPoints-1
               IF rEmpArray[i] > rEmpArray[i+1]
                  swap := rEmpArray[i];
                  rEmpArray[i] := rEmpArray[i+1];
                  rEmpArray[i+1] := swap;
                  changed := TRUE;
               END IF;
            END FOR;
            IF NOT changed
               EXIT;
            END IF;
         END FOR;
         rEmpMean := rEmpMean/FLOAT(numDataPoints);
         oldRepNum := 16;
      ELSE
         empFlag := TRUE;
      END IF;
   ELSIF (button.ReferenceName = "CancelButton") OR (numDataPoints = 0)
      IF NOT ((oldRepNum =16) OR (oldFailNum = 16))
         numDataPoints := 0;
      END IF;
      empFlag := TRUE;
   END IF;
   DISPOSE(dataBox);
END PROCEDURE; {GetEmpirical}

PROCEDURE GammaFunction(IN x : REAL) : REAL;
VAR
   y      : REAL;
BEGIN
   y := (1.000000000190015+(76.18009172947146/(x+1.))-(86.50532032941677/(x+2.))
         +(24.01409824083091/(x+3.))-(1.231739572450155/(x+4.))+
          (0.001208650973866179/(x+5.))-(0.000005395239384953/(x+6.)));
   RETURN ((POWER((x+5.5),(x+0.5))*EXP(-1.*(x+5.5))*SQRT(2.*pi)*y)/x);     
END METHOD;        


{ComputeStats -- Compute the mean and standard deviation}
PROCEDURE ComputeStats(IN distro : INTEGER;
                       IN params : realArray;
                       OUT mean, stdev : REAL);
BEGIN
VAR
   i,j, numParams        : INTEGER;
   sum,x,y,z,z2          : REAL;
     
BEGIN
   CASE distro
      WHEN 1:  {Beta}
         mean := params[1]/(params[1]+params[2]);
         stdev := SQRT(params[1]*params[2]/
                      (POWER(params[1]+params[2],2.)*(params[1]+params[2]+1.)));
      WHEN 2:  {Chi-square}
         mean := params[1];
         stdev :=SQRT(2.*params[1]);
      WHEN 3:  {Binomial}
         mean := params[1]*params[2];
         stdev := SQRT(params[1]*params[2]*(1.-params[2]));
      WHEN 4:  {Exponential}
         mean := params[1]+params[2];
         stdev := params[1];
      WHEN 5:  {Erlang}
         mean := params[1]*params[2];
         stdev := SQRT(params[1]*POWER(params[2],2.));
      WHEN 6:  {Gamma}
         mean := params[1]*params[2]+params[3];
         stdev := SQRT(params[1]*POWER(params[2],2.));
      WHEN 7:  {Lognormal}
         mean := params[1];
         stdev := params[2];
      WHEN 8:  {Normal}
         mean := params[1];
         stdev := params[2];
      WHEN 9:  {Uniform Integer}
         mean := (params[1]+params[2])/2.;
         stdev := SQRT(POWER(params[2]-params[1],2.)/12.);
      WHEN 10: {Uniform Real}
         mean := (params[1]+params[2])/2.;
         stdev := SQRT(POWER(params[2]-params[1],2.)/12.);
      WHEN 11: {Pearson 5}
         IF params[1] <= 1.
            mean := 12345.6789;   {Murphy identifier for out of range Pearson 5 mean} 
            stdev := 12345.6789;   {Murphy identifier for out of range Pearson 5 mean}
         ELSE
            mean := (params[2]/(params[1]-1.))+params[3];
            IF params[1]<=2.    
               stdev := 12345.6789; {Murphy identifier for out of range Pearson 5 std dev} 
            ELSE  
               stdev := params[2]/(params[1]-1.)/SQRT(params[1]-2.);
            END IF;   
         END IF;
      WHEN 12: {Pearson 6}
         IF params[2] <= 1.
            mean := 12345.6789;   {Murphy identifier for out of range Pearson 6 mean}
            stdev := 12345.6789;   {Murphy identifier for out of range Pearson 6 std dev}
         ELSE
            mean := ((params[3]*params[1])/(params[2]-1.));
            IF params[2]<=2.    
               stdev := 12345.6789;   {Murphy identifier for out of range Pearson 6 std dev} 
            ELSE  
               stdev := SQRT((POWER(params[3],2.)*params[1]*(params[1]+params[2]-1.))
                        /(POWER(params[2]-1.,2.)*(params[2]-2.)));
            END IF;   
         END IF;
      WHEN 13: {Poisson}
         mean := params[1];
         stdev := SQRT(params[1]);
      WHEN 14: {Triangular}
         mean := (params[1]+params[2]+params[3])/3.;
         stdev := SQRT((POWER(params[1],2.)+POWER(params[2],2.)+POWER(params[3],2.)
                    -params[1]*params[2]-params[1]*params[3]-params[2]*params[3])/18.);
      WHEN 15: {Weibull}
         IF params[1] < 0.05
            mean := 12345.6789;  {Murphy identifier for out of range Weibull mean}
            stdev := 12345.6789;  {Murphy identifier for out of range Weibull std dev}
         ELSIF params[1] = 1.   
            mean := params[2] + params[3];
            stdev := params[2];
         ELSIF params[1] > 10000000.
            mean := params[2] + params[3];
            stdev := 0.;
         ELSE
            x := (1.+(1./params[1]));
            z := GammaFunction(x);
            mean := (params[2]*z)+params[3];
            y := (1.+(2./params[1]));
            z2 := GammaFunction(y);
            stdev := params[2]*SQRT(z2 - POWER(z,2.));
         END IF;
      WHEN 16: {Empirical}
         numParams := HIGH(params);
         FOR j := 1 TO numParams
            sum := sum+params[j];
         END FOR;
         mean := sum/FLOAT(numParams);
         sum :=0.;
         FOR j:=1 TO numParams
            sum := sum+POWER(params[j]-mean,2.);
         END FOR;
         stdev :=SQRT(sum/FLOAT(numParams));
      WHEN 17: {Point Estimate}
         mean := 12345.6788;  {Murphy identifier for no mean}
         stdev := 12345.6788;  {Murphy identifier for no standard deviation}
      WHEN 18: {[None]}
         mean := 12345.6788;  {Murphy identifier for no mean}
         stdev := 12345.6788;  {Murphy identifier for no standard deviation}
      WHEN 19: {[Fixed]}
         mean :=params[1]; 
         stdev :=0.;
      WHEN 20: {Exponential Power/Error}
         {Shape is var[1], scale is var[2], and location is var[3]}
         mean := params[3];
         x := 3.*params[1]/2.;
         z := GammaFunction(x);
         y := params[1]/2.;
         z2 := GammaFunction(y);
         stdev := SQRT(POWER(2.,params[1])*POWER(params[2],2.)*z/z2);
      WHEN 21: {Extreme Value}
         {Scale is var[1], location is var[2]}
         mean := params[2]+.57721*params[1];
         stdev := SQRT(POWER(params[1],2.)*POWER(pi,2.)/6.);
      WHEN 22: {Laplace}
         {Scale is var[1], location is var[2]}
         mean := params[2];
         stdev := SQRT(2.*POWER(params[1],2.));
      OTHERWISE
   END CASE;
END PROCEDURE; {ComputeStats}
 

{ShowDistBox}                      
PROCEDURE ShowDistBox (INOUT distType                    : INTEGER;
                       INOUT distParams                  : realArray);

                      
VAR
   distBox          : DistBoxObj;
  
BEGIN
   NEW(distBox);
   ASK distBox TO LoadFromLibrary(dialogs, "DistBox");
   ASK window TO AddGraphic(distBox);
   ASK distBox TO Draw;         
   ASK distBox TO ReceiveData(distType, distParams);
   DISPOSE(distBox);
    
END PROCEDURE; {ShowDistBox}
 

PROCEDURE GetDistMean(IN distString : STRING; OUT distMean : REAL);
VAR
   ch, meanStr : STRING;
   i  : INTEGER;
BEGIN
   FOR i := 1 TO STRLEN(distString)
      ch := SUBSTR(i, i, distString);
      IF (ch = "(")
         INC(i);
         ch := SUBSTR(i, i, distString);
         WHILE(ch <> ")")
            meanStr := meanStr + ch;
            INC(i);
            ch := SUBSTR(i, i, distString);
         END WHILE;
         EXIT;
      END IF;
   END FOR;
   distMean := STRTOREAL(meanStr);
END PROCEDURE;

PROCEDURE MakeDistString(IN distro : INTEGER;
                         IN params : realArray;
                         OUT distroString : STRING);
VAR
   distroName, tempString          : STRING;
   tempMean, tempStdev             : REAL;  
  
BEGIN
   CASE distro
      WHEN 1:  distroName := "Beta";
      WHEN 2:  distroName := "ChiS";
      WHEN 3:  distroName := "Bnml";
      WHEN 4:  distroName := "Expo";
      WHEN 5:  distroName := "Erl";
      WHEN 6:  distroName := "Gamm";
      WHEN 7:  distroName := "LogN";
      WHEN 8:  distroName := "Norm";
      WHEN 9:  distroName := "UnIn";
      WHEN 10: distroName := "UnRl";
      WHEN 11: distroName := "Pea5";
      WHEN 12: distroName := "Pea6";
      WHEN 13: distroName := "Pois";
      WHEN 14: distroName := "Trng";
      WHEN 15: distroName := "Weib";
      WHEN 16: distroName := "Emp";
      WHEN 17: distroName := "PtEs";
      WHEN 18: distroName := "None";
      WHEN 19: distroName := "Fixd";
      WHEN 20: distroName := "ExpP";
      WHEN 21: distroName := "Extr";
      WHEN 22: distroName := "Lapl";
      OTHERWISE
         NEW(message, 1..1);
         message[1] := "ERROR: Unknown Distribution Type!     ";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
   END CASE;
   tempString := distroName + " (";
   
   ComputeStats(distro, params, tempMean, tempStdev);
   IF tempMean = 12345.6789
       tempString := tempString + "Undefined)";
   ELSE
       tempString := tempString + REALTOSTR(tempMean) + ")";
   END IF;
   {  tempMean := {out of range}
   ELSIF tempMean = 12345.6788
     tempMean := {N/A}
    }
   
   distroString := tempString;
    
END PROCEDURE; {MakeDistString}


   
              
PROCEDURE GetParameters(IN distroType                 : INTEGER;
                        IN typeOfDistro               : STRING;
                        INOUT params, xUnits          : strArray;
                        INOUT xOffset                 : realArray);
BEGIN
   CASE distroType
      WHEN 1: {Beta}
         params[1]  := "Shape";
         xOffset[1] := 12.16;
         xUnits[1]   := "";
         params[2]  := "Shape2";
         xOffset[2] := 11.17;
         xUnits[2]   := "";
         params[3]  := "";
      WHEN 2: {Chi-square}
         params[1]  := "Shape";
         xOffset[1] := 12.16;
         xUnits[1]   := systemUnits;                 
         params[2]  := "";
         params[3]  := "";
      WHEN 3: {Binomial}
         params[1]  := "N Trials";
         xOffset[1] := 11.33;
         xUnits[1]   := systemUnits;
         params[2]  := "Probability";
         xOffset[2] :=9.33;
         xUnits[2]   := "";
         params[3]  := "";
      WHEN 4: {Exponential}
      
         IF (lambdaMode AND (typeOfDistro <> "other"))
            params[1]  := "lambda";
            xOffset[1] := 11.67;
            xUnits[1]   := typeOfDistro + " per "+systemUnits;
         ELSE
            params[1]  := "Mean";
            xOffset[1] := 12.83;
            xUnits[1]   := systemUnits;
         END IF;
         params[2]  := "Location";
         xOffset[2] := 10.5;
         xUnits[2]   := systemUnits;
         params[3]  := "";
      WHEN 5: {Erlang}
         params[1]  := "Shape";
         xOffset[1] := 12.16;
         xUnits[1]   := "";
         params[2]  := "Scale";
         xOffset[2] := 12.83;
         xUnits[2]   := systemUnits;
         params[3]  := "";
      WHEN 6, 11, 15:  {Gamma, Pearson 5, Weibull}
         params[1]  := "Shape";
         xOffset[1] := 12.16;
         xUnits[1]   := "";
         params[2]  := "Scale";
         xOffset[2] := 12.83;
         xUnits[2]   := systemUnits;
         params[3]  := "Location";
         xOffset[3] := 10.5;
         xUnits[3]   := systemUnits;
      WHEN 7: {Lognormal}
         IF muSigmaMode
            params[1]  := "mu";
            xOffset[1] := 15.;
            xUnits[1]   := "ln("+systemUnits+")";
            params[2]  := "sigma";
            xOffset[2] := 12.83;
            xUnits[2]   := "ln("+systemUnits+")";
         ELSE
            params[1]  := "Mean";
            xOffset[1] := 12.83;
            xUnits[1]   := systemUnits;
            params[2]  := "Standard Dev";
            xOffset[2] := 6.33;
            xUnits[2]   := systemUnits;
         END IF;
         params[3] := "";
      WHEN 8: {Normal}
         params[1]  := "Mean";
         xOffset[1] := 12.83; 
         xUnits[1]   := systemUnits;
         params[2]  := "Standard Dev";
         xOffset[2] := 6.33;
         xUnits[2]   := systemUnits;
         params[3]  := "";
      WHEN 9, 10:  {Uniform Int, Uniform Real}
         params[1]  := "Min";
         xOffset[1] := 14.5;
         xUnits[1]   := systemUnits;
         params[2]  := "Max";
         xOffset[2] := 14.;
         xUnits[2]   := systemUnits;
         params[3]  := "";
      WHEN 12: {Pearson 6}
         params[1]  := "Shape";
         xOffset[1] := 12.16;
         xUnits[1]   := "";
         params[2]  := "Shape2";
         xOffset[2] := 11.17;
         xUnits[2]   := "";
         params[3]  := "Scale";
         xOffset[3] := 12.83;
         xUnits[3]   := systemUnits;
      WHEN 13: {Poisson}
         params[1]  := "Mean";
         xOffset[1] := 12.83;
         xUnits[1]   := systemUnits;
         params[2]  := "";
         params[3]  := "";
      WHEN 14: {Triangular}
         params[1]  := "Min";
         xOffset[1] := 14.5;
         xUnits[1]   := systemUnits;
         params[2]  := "Mode";
         xOffset[2] := 12.83;
         xUnits[2]   := systemUnits;
         params[3]  := "Max";
         xOffset[3] := 14.;
         xUnits[3]   := systemUnits;
      WHEN 16: {Empirical}
         params[1]  := "";
         params[2]  := "";
         params[3]  := "";
      WHEN 17: {Point Estimate}
         params[1]  := "P Success";
         xOffset[1] := 8.83;
         xUnits[1]   := systemUnits;
         params[2]  := "";
         params[3]  := "";
      WHEN 18: {None}
         params[1] := "";
         params[2] := "";
         params[3] := "";
      WHEN 19: {Fixed}
         params[1] := "Constant";
         xOffset[1] := 10.4;
         xUnits[1]   := systemUnits;
         params[2]  := "";
         params[3]  := "";
      WHEN 20: {Exponential Power}  
         params[1] := "Shape";
         xOffset[1] := 12.16;
         xUnits[1]   := "";
         params[2] := "Scale";
         xOffset[2] := 12.83;
         xUnits[2]   := systemUnits;
         params[3] := "Location";
         xOffset[3] := 10.5;
         xUnits[3]   := systemUnits;
      WHEN 21: {Extreme Value}
         params[1] := "Scale"; 
         xOffset[1] := 12.83;
         xUnits[1]   := systemUnits;
         params[2] := "Location";
         xOffset[2] := 10.5;
         xUnits[2]   := systemUnits;
      WHEN 22: {Laplace}
         params[1] := "Scale";
         xOffset[1] := 12.83;
         xUnits[1]   := systemUnits;
         params[2] := "Location";
         xOffset[2] := 10.5;
         xUnits[2]   := systemUnits;
         
      OTHERWISE
   END CASE;
END PROCEDURE;{GetParameters}

PROCEDURE SendAlert(IN message : TextBufferType;
                    IN cancel, yesNo, error  : BOOLEAN) : BOOLEAN;
VAR
   messageBox : MessageDialogBoxObj;
  
BEGIN
   IF soundIsOn AND (FileExists(soundsPath + "Error.wav")) AND error
     ASK sound TO PlayMe(soundsPath + "Error.wav");
   ELSIF soundIsOn AND (FileExists(soundsPath + "Info.wav")) AND (NOT error)
     ASK sound TO PlayMe(soundsPath + "Info.wav");
   END IF;  
   NEW(messageBox);
   ASK messageBox TO SetTextBuffer(message);
   ASK messageBox TO SetStyle(AlertMessage);
   IF yesNo AND (NOT threeWay)
      ASK messageBox TO SetResponses(YesNoResponse);
   ELSIF yesNo AND threeWay
      ASK messageBox TO SetResponses(YesNoCancelResponse);
   END IF;
   IF cancel
      ASK messageBox TO SetCancel(TRUE,TRUE);
   END IF;
   ASK window TO AddGraphic(messageBox);
   result := ASK messageBox TO AcceptSysInput();
   IF messageBox.SelectedButton = CancelButton
      loadCanceled := TRUE;
   ELSE
     
      loadCanceled := FALSE;
   END IF;
   DISPOSE(messageBox);
   RETURN result;
END PROCEDURE; {SendAlert}

PROCEDURE GetSpareList(INOUT spares : OptionListType);
VAR
   i, j  : INTEGER;
   pool  : SparePoolObj;
BEGIN
   IF totalSpares > 0
      DISPOSE(spares);
      NEW(spares, 1..totalSpares);
      i := 1;
   END IF;
   FOREACH pool IN poolGroup
      IF pool.sparingType = SparePool
         spares[i] := pool.poolName;
         INC(i);
      END IF;
   END FOREACH;
END PROCEDURE;

PROCEDURE GetResList(INOUT res : OptionListType);
VAR
   i, j  : INTEGER;
   pool  : SparePoolObj;

BEGIN
   IF totalRes > 0
      DISPOSE(res);
      NEW(res, 1..totalRes);
      j := 1;
   END IF;
   FOREACH pool IN poolGroup
      IF pool.sparingType = Resource
         res[j] := pool.poolName;
         INC(j);
      END IF;
   END FOREACH;
END PROCEDURE;

PROCEDURE GetTrigList(INOUT trigs : OptionListType);
VAR
    i           : INTEGER;
    trig        : RapTriggerObj;
BEGIN

   IF totalTriggers > 0
      DISPOSE(trigs);
      NEW(trigs, 1..totalTriggers);
      i := 1;
   END IF;
  
   FOREACH trig IN triggerGroup
      trigs[i] := trig.TrigName;
      INC(i);
   END FOREACH;
END PROCEDURE;


PROCEDURE ChopZeros(IN num    : REAL; 
                    IN length : INTEGER) : STRING;
VAR
   chopped           : BOOLEAN;
   testChar, sigDigs : INTEGER;
   chopee            : STRING;
BEGIN
   chopee := REALTOSTR(num);
   testChar := STRLEN(chopee);
   REPEAT
      IF SUBSTR(testChar, testChar, chopee) = "0"
         DEC(testChar);
      ELSIF SUBSTR(testChar, testChar, chopee) = "."
         chopee := SUBSTR(1, testChar+1, chopee);
         sigDigs := testChar-1;
         chopped := TRUE;
      ELSE
         chopee := SUBSTR(1, testChar+1, chopee);
         sigDigs := testChar;
         chopped := TRUE;
      END IF;
   UNTIL chopped;
   IF STRLEN(chopee) > length
      IF sigDigs <= length
         chopee := SUBSTR(1, sigDigs, chopee);
      ELSE
         chopee := SUBSTR(1, length, chopee);
      END IF;
   END IF;
   RETURN chopee;
END PROCEDURE;

PROCEDURE GetNumParams (IN distroNum : INTEGER;
                        OUT nPars    : INTEGER);
BEGIN
   CASE distroNum
      WHEN 1:   nPars := 2;
      WHEN 2:   nPars := 1;
      WHEN 3:   nPars := 2;
      WHEN 4:   nPars := 2;
      WHEN 5:   nPars := 2;
      WHEN 6:   nPars := 3;
      WHEN 7:   nPars := 2;
      WHEN 8:   nPars := 2;
      WHEN 9:   nPars := 2;
      WHEN 10:  nPars := 2;
      WHEN 11:  nPars := 3;
      WHEN 12:  nPars := 3;
      WHEN 13:  nPars := 1;
      WHEN 14:  nPars := 3;
      WHEN 15:  nPars := 3;
      WHEN 16:  nPars := 0;
      WHEN 17:  nPars := 1;
      WHEN 18:  nPars := 1;         
      WHEN 19:  nPars := 1; 
      WHEN 20:  nPars := 3;
      WHEN 21:  nPars := 2;
      WHEN 22:  nPars := 2;

   OTHERWISE
      NEW(message, 1..1);
      message[1] := "ERROR: Unknown distribution selected!     ";
      result := SendAlert(message, FALSE, FALSE, TRUE);
      DISPOSE(message);
      distroNum := 4;
      nPars := 2;
   END CASE;
                              
END PROCEDURE;

PROCEDURE InitHierFilter(INOUT hierList : OptionListType);
VAR
   i,j, minloc : INTEGER;
   tempHier : RBDHierObj;
   min, temp : STRING;
BEGIN
   i := 1;
   FOREACH tempHier IN hierGroup
      hierList[i+3] := tempHier.name + " - " + INTTOSTR(tempHier.Id);
      INC(i);
   END FOREACH;
   {sort the list, then add first three }
   FOR j := 4 TO totalHiers+3
      min := hierList[j];
      minloc := j;
      FOR i := j+1 TO totalHiers+3
         IF hierList[i] < min
            min := hierList[i];
            minloc := i;
         END IF;
      END FOR;
      temp := hierList[j];
      hierList[j] := hierList[minloc];
      hierList[minloc] := temp;
   END FOR;
   hierList[1] := "All";
   hierList[2] := "Home";
   hierList[3] := "--------------------------------------";
END PROCEDURE;

PROCEDURE HierFilter(IN tableType: STRING; INOUT table : TableObj; IN arr : FinalArrayType; 
                     IN arraySize : INTEGER; IN hierIdLoc : INTEGER; IN hierName : STRING;
                     IN printing : BOOLEAN; OUT displayArray : FinalArrayType; OUT hiercount : INTEGER);
VAR
   i, j, k, lastColumn, counter, tableSize : INTEGER;
   cost : REAL;
   test : STRING;
BEGIN
   IF NOT printing
      ASK table TO Activate;
   END IF;
   IF ((tableType = "sparesOut") OR (tableType = "blockcostOut"))
      tableSize := 13;
      lastColumn := 6;
   ELSIF (tableType = "maint")
      tableSize := 15;
      lastColumn := 6;
   ELSIF (tableType = "capacityOut")
      tableSize := 13;
      lastColumn := 9;
   ELSIF (tableType = "distro")
      tableSize := 11;
      lastColumn := 11;
   ELSIF ((tableType = "nodegen") OR (tableType = "standby") OR (tableType = "bspares") OR (tableType = "pm"))
      tableSize := 15;
      lastColumn := 9;
   ELSIF (tableType = "eventgen")
      tableSize := 15;
      lastColumn := 7;
   ELSIF ((tableType = "blockanalOut") OR (tableType = "nodeanalOut") OR (tableType = "eventanalOut")
          OR (tableType = "hieranalOut"))
      tableSize := 13;
      lastColumn := 7;
   ELSIF (tableType = "cost")
      tableSize := 15;
      lastColumn := 16;
   ELSIF (tableType = "dep")
      tableSize := 15;
      lastColumn := 8;
   ELSIF (tableType = "adv")
      tableSize := 15;
      lastColumn := 10;
   ELSIF (tableType = "cap")
      tableSize := 15;
      lastColumn := 5;
   END IF;
   IF (hierName <> "All")
      i := POSITION(hierName, " -");
      hierName := INTTOSTR(STRTOINT(SUBSTR(i+2, i+11, hierName + "      ")));
   END IF;
   counter := 0;
   FOR i := 1 TO arraySize
   test := arr[i,hierIdLoc];
      IF arr[i,hierIdLoc] = hierName
         INC(counter);
      END IF;
   END FOR;
   hiercount := 0;
   FOR i := 1 TO arraySize
      test := arr[i,hierIdLoc];
      IF arr[i,hierIdLoc] <> "0" 
         INC(hiercount);
      END IF;
   END FOR;
   IF (counter > tableSize)
      IF tableType = "cost"
         ASK table TO SetSize(lastColumn-1, counter);
      ELSIF tableType = "distro"
         ASK table TO SetSize(lastColumn-2, counter);
      ELSE
         ASK table TO SetSize(lastColumn, counter);
      END IF;
   END IF; 

   IF hierName = "All" 
      IF arraySize > tableSize
         IF tableType = "cost"
            ASK table TO SetSize(lastColumn-1, arraySize);
         ELSIF tableType = "distro"
            ASK table TO SetSize(lastColumn-2, arraySize);
         ELSE
            ASK table TO SetSize(lastColumn, arraySize);
         END IF;
      ELSE
         IF tableType = "cost"
            ASK table TO SetSize(lastColumn-1, tableSize);
         ELSIF tableType = "distro"
            ASK table TO SetSize(lastColumn-2, tableSize);
         ELSE
            ASK table TO SetSize(lastColumn, tableSize);
         END IF;
      END IF;
      NEW(displayArray, 1..arraySize, 1..lastColumn);
      j := arraySize+1;
      FOR i := 1 TO arraySize
         FOR k := 1 TO lastColumn         
            IF ((tableType = "blockanalOut") OR (tableType = "nodeanalOut") OR (tableType = "eventanalOut")
                                             OR (tableType = "hieranalOut"))
               IF k<=2
                  displayArray[i, k] := arr[i, k];                
               ELSE
                  displayArray[i, k] := arr[i, k+5*(weakLinkAnalType-1)];   
               END IF;          
            ELSE
               displayArray[i, k] := arr[i, k];
            END IF;
            IF tableType = "cost"
               ASK table TO SetText(displayArray[i, k], k-1, i);
            ELSIF tableType = "distro"
               IF k > lastColumn-2
               ELSE
                  ASK table TO SetText(displayArray[i, k], k, i);
               END IF;
            ELSE
               ASK table TO SetText(displayArray[i, k], k, i);
            END IF;
         END FOR;
         IF tableType = "distro"
            translateArray[i] := STRTOINT(displayArray[i,11]); 
         END IF; 
      END FOR;
   ELSE
      IF (counter > 0)
         NEW(displayArray, 1..counter, 1..lastColumn);
         j := 1;
         FOR i := 1 TO arraySize
            IF arr[i,hierIdLoc] = hierName
               FOR k := 1 TO lastColumn
                  IF ((tableType = "blockanalOut") OR (tableType = "nodeanalOut") OR (tableType = "eventanalOut")
                                             OR (tableType = "hieranalOut"))
                     IF k<=2
                        displayArray[j, k] := arr[i, k];                
                     ELSE
                        displayArray[j, k] := arr[i, k+5*(weakLinkAnalType-1)];   
                     END IF;   
                  ELSE
                     displayArray[j, k] := arr[i, k];
                  END IF;
               END FOR;
               IF tableType = "distro"
                  translateArray[j] := STRTOINT(displayArray[j,11]);
               END IF;
               INC(j);
            END IF;
         END FOR;
         FOR i := 1 TO j-1
            FOR k := 1 TO lastColumn
               IF tableType = "cost"
                  ASK table TO SetText(displayArray[i, k], k-1, i);
               ELSIF tableType = "distro"
                  IF k > lastColumn-2
                  ELSE
                     ASK table TO SetText(displayArray[i, k], k, i);
                  END IF;
               ELSE
                  ASK table TO SetText(displayArray[i, k], k, i);
               END IF;
            END FOR;
         END FOR;
      ELSE
         IF NOT printing
            ASK table TO Deactivate;
         END IF;
         j := 1;
      END IF;
   END IF;
   FOR i := j TO tableSize
      FOR k := 1 TO lastColumn
         IF tableType = "cost"
            ASK table TO SetText("", k-1, i);
         ELSIF tableType = "distro"
            IF k > lastColumn-2
            ELSE
               ASK table TO SetText("", k, i);
            END IF;
         ELSE
            ASK table TO SetText("", k, i);
         END IF;
      END FOR;
   END FOR;
   IF ((counter <= tableSize) AND (hierName <> "All"))
      IF tableType = "cost"
         ASK table TO SetSize(lastColumn-1, tableSize);
      ELSIF tableType = "distro"
         ASK table TO SetSize(lastColumn-2, tableSize);
      ELSE
         ASK table TO SetSize(lastColumn, tableSize);
      END IF;
   END IF;
END PROCEDURE; {HierFilter}

OBJECT AboutBoxObj;
   ASK METHOD ScrollCredits;
   VAR
      i,n,numLabels : INTEGER;
      highY         : REAL;
      posArray      : ARRAY INTEGER, INTEGER OF REAL;
      labels        : ARRAY INTEGER OF LabelObj;
      version,date  : LabelObj;
      temp1,temp2:REAL;
      delayInterrupted : BOOLEAN;
   BEGIN
      version := Descendant("VersionLabel", 0);
      date := Descendant("DateLabel", 0);
      ASK version TO SetLabel(devVersion);
      ASK date TO SetLabel(devDate);
      numLabels := 39;
      NEW(posArray, 1..2, 1..numLabels);
      NEW(labels, 1..numLabels);
      button := Child("OKButton", 0);
      FOR i := 1 TO numLabels
         NEW(labels[i]);
         AddGraphic(labels[i]);
         posArray[2, i] := (5. + FLOAT(i));
         ASK labels[i] TO SetHidden(TRUE);
      END FOR;
      Draw;
      ASK labels[1] TO SetLabel("Lead Developers:");
      ASK labels[2] TO SetLabel("Simulation Engine:");
      ASK labels[3] TO SetLabel("   Chuck Carter");
      ASK labels[4] TO SetLabel("   Tony Malerich");
      ASK labels[5] TO SetLabel("Graphical User Interface:");
      ASK labels[6] TO SetLabel("   Elizabeth Grimes");
      ASK labels[7] TO SetLabel(" ");
      ASK labels[8] TO SetLabel("Concept Development & Program Mgt");
      ASK labels[9] TO SetLabel("Kenneth E. Murphy");
      ASK labels[10] TO SetLabel(" ");
      ASK labels[11] TO SetLabel("Past Developers:");
      ASK labels[12] TO SetLabel("Steve Brown");
      ASK labels[13] TO SetLabel("Jeff Jacobs");
      ASK labels[14] TO SetLabel("Lee Ochoa");
      ASK labels[15] TO SetLabel("");      
      ASK labels[16] TO SetLabel("External Testers:");
      ASK labels[17] TO SetLabel("Bob Gass");
      ASK labels[18] TO SetLabel("Bill Wessels");
      ASK labels[19] TO SetLabel("");      
      ASK labels[20] TO SetLabel("");
      ASK labels[21] TO SetLabel("");
      ASK labels[22] TO SetLabel("");      
      ASK labels[23] TO SetLabel("Special Thanks:");
      ASK labels[24] TO SetLabel("H. Paul Barringer");
      ASK labels[25] TO SetLabel("Smoky Burgess");
      ASK labels[26] TO SetLabel("George Washington");
      ASK labels[27] TO SetLabel("Dr. Evil and O.B.");
      ASK labels[28] TO SetLabel("Tony, Merle, Skiv, & Buzz");
      ASK labels[29] TO SetLabel(" ");
      ASK labels[30] TO SetLabel("Let's Roll !");
      ASK labels[31] TO SetLabel(" ");
      ASK labels[32] TO SetLabel(" ");
      ASK labels[33] TO SetLabel("ARINC Engineering Services, LLC");
      ASK labels[34] TO SetLabel("6565 Americas Pkwy NE, Suite 770"); {eag}
      ASK labels[35] TO SetLabel("Albuquerque, NM 87110");
      ASK labels[36] TO SetLabel("505.248.0718");
      ASK labels[37] TO SetLabel(" ");
      ASK labels[38] TO SetLabel(" ");
      ASK labels[39] TO SetLabel(" ");
      FOR i := 2 TO numLabels
         posArray[1, i] := 5.;
      END FOR;
      posArray[1, 1] := 3.;
      posArray[1, 8] := 3.;
      posArray[1, 11] := 3.;
      posArray[1, 16] := 3.;
      posArray[1, 20] := 3.;
      posArray[1, 23] := 3.;
      posArray[1, 30] := 14.;
      posArray[1, 31] := 10.;
      posArray[1, 32] := 10.;
      FOR i := 1 TO numLabels
         IF posArray[2, i] <= 14.
            ASK labels[i] TO SetHidden(FALSE);
            ASK labels[i] TO DisplayAt(posArray[1, i], posArray[2, i]);
         END IF;
      END FOR;
   {   FOR i := 1 TO 30
         IF LastPicked <> button
            MicroDelay(1000);
         END IF;
      END FOR;    }
      i:=1;
      WHILE i<50
         MicroDelay(50000);
         INC(i);
         HandleEvents(FALSE);
         IF LastPicked = button
            i:=100;
         END IF;
      END WHILE;
      IF LastPicked <> button
         REPEAT
            FOR n := 1 TO numLabels
               posArray[2, n] := posArray[2, n] - 0.1;
               IF posArray[2, n] <= 14.
                  ASK labels[n] TO SetHidden(FALSE); 
                  ASK labels[n] TO DisplayAt(posArray[1, n], posArray[2, n]);
               END IF; 
               IF posArray[2, n] < 6. 
                  highY := 0.;
                  FOR i := 1 TO numLabels
                     IF posArray[2, i] > highY
                        highY := posArray[2, i];
                     END IF;
                  END FOR;
                  posArray[2, n] := highY + 1.;
                  ASK labels[n] TO SetHidden(TRUE);
                  ASK labels[n] TO Erase;
               END IF; 
            END FOR; 
            temp1:= posArray[2,1];
            IF ((posArray[2,1] > 14.99) AND (posArray[2,1] < 15.01))
               i:=1;
               WHILE i<100
                  MicroDelay(50000);
                  INC(i);
                  HandleEvents(FALSE);
                  IF LastPicked = button
                    i:=100;
                  END IF;
               END WHILE;
            ELSE  
               MicroDelay(50000);
            END IF;  
            HandleEvents(FALSE);
         UNTIL LastPicked = button;
      END IF;
      DISPOSE(posArray);
      DISPOSE(labels);
   END METHOD; {ScrollCredits}
END OBJECT; {AboutBoxObj}
      
OBJECT HelpBoxObj;
   ASK METHOD CheckValidInput(IN distro, numParam : INTEGER;
                              IN values           : realArray;
                              IN distroUse        : STRING;
                              OUT flag            : BOOLEAN;
                              INOUT errorText     : strArray);
   VAR
      i         : INTEGER;
      parameter : STRING;  
      type      : ARRAY INTEGER, INTEGER OF STRING;
   BEGIN
      NEW(type, 1..22, 1..3);  {FIX}
      type[1,1] := "shape";    type[1,2] := "shape2";
      type[3,1] := "N Trials"; type[3,2] := "probability";
      type[4,1] := "mean";     type[4,2] := "location";
      type[5,2] := "scale";    type[5,1] := "Erlang";
      type[6,1] := "shape";    type[6,2] := "scale";        type[6,3]  := "location";
      IF muSigmaMode
         type[7,1] := "mu";    type[7,2] := "sigma";
      ELSE                      
         type[7,1] := "mean";  type[7,2] := "standard deviation";
      END IF;
      type[11,1]:= "shape";    type[11,2]:= "scale";        type[11,3] := "location";
      type[12,1]:= "shape";    type[12,2]:= "shape2";       type[12,3] := "scale";    
      type[13,1]:= "mean";      
      type[15,1]:= "shape";    type[15,2]:= "scale";        type[15,3] := "location";
      type[17,1]:= "P Success";
      type[2,1] := "Chi-square";
      type[8,1] := "Normal";   type[8,2] := "standard deviation";
      type[9,1] := "Uniform Integer";
      type[10,1]:= "Uniform Real";
      type[14,1]:= "Triangular";
      type[18,1]:= "None";
      type[18,2]:= "";
      type[19,1]:= "Fixed";
      type[20,1]:= "shape";    type[20,2]:= "scale";        type[20,3]:= "location";
      type[21,1]:= "scale";    type[21,2]:= "location";
      type[22,1]:= "scale";    type[22,2]:= "location";
      flag := TRUE;
      NEW(message, 1..1);
      FOR i := 1 TO numParam
         IF values[i] > 999999999.999999
            INC(errors);
            errorText[errors] := distroUse + type[distro, i] + " must be less than 999,999,999.999999!     ";
            flag := FALSE;
         ELSE
            parameter := type[distro, i];
            CASE parameter
               WHEN "mean", "scale", "N Trials", "standard deviation", "shape", "shape2":
                  IF values[i] < 0.000001
                     INC(errors);
                     errorText[errors] := distroUse + type[distro, i] + " must be greater than or equal to 0.000001!     ";
                     flag := FALSE;
                  END IF;
                  IF ((parameter = "N Trials") AND (FLOAT(ROUND(values[1])) <> values[1]))
                     INC(errors);
                     errorText[errors] := distroUse + "N Trials must be an integer!     ";
                     flag := FALSE;
                  END IF;
               WHEN "probability", "P Success":
                  IF ((values[i] < 0.000001) OR (values[i] > 1.))
                     INC(errors);
                     errorText[errors] := distroUse + type[distro, i] + " must be greater than or equal to 0.000001 and less than or equal to 1!     ";
                     flag := FALSE;
                  END IF;
               WHEN "location":
                  IF (values[i] < 0.)
                     INC(errors);
                      errorText[errors] := distroUse + type[distro, i] + " must be greater than or equal to 0!     ";
                      flag := FALSE;
                  END IF;
               WHEN "mu":
                  IF (values[i] > 20.)
                     INC(errors);
                     errorText[errors] := "The "+ distroUse +"mu must be less than or equal to 10!     ";
                     flag := FALSE;
                  END IF;
               WHEN "sigma":
                  IF (values[i] < 0.)
                     INC(errors);
                     errorText[errors] := "The "+ distroUse +"sigma must be greater than or equal to 0!     ";
                     flag := FALSE;
                  ELSIF (values[i] > 20.)
                     INC(errors);
                     errorText[errors] := "The "+ distroUse +"sigma must be less than or equal to 10!     ";
                     flag := FALSE;
                  END IF;
               WHEN "Chi-square":
                  IF (FLOAT(ROUND(values[1])) <> values[1])
                     INC(errors);
                     errorText[errors] := distroUse + "shape parameter must be an integer!     ";
                     flag := FALSE;
                  END IF;
                  IF values[1] < 0.000001
                     INC(errors);
                     errorText[errors] := distroUse + "shape parameter must be greater than 0!     ";
                     flag := FALSE;
                  END IF;
               WHEN "Erlang":
                  IF (FLOAT(ROUND(values[1])) <> values[1])
                     INC(errors);
                     errorText[errors] := distroUse + "shape parameter must be an integer!     ";
                     flag := FALSE;
                  END IF;
                  IF values[1] < 0.000001
                     INC(errors);
                     errorText[errors] := distroUse + "shape parameter must be greater than or equal to 0.000001!     ";
                     flag := FALSE;
                  END IF;
               WHEN "Normal":
                  IF values[1] < 0.
                     INC(errors);
                     errorText[errors] := distroUse + "mean must be greater than or equal to 0!     ";
                     flag := FALSE;
                  END IF;            
               WHEN "Fixed":
                  IF values[1] < 0.
                     INC(errors);
                     errorText[errors] := "The constant value must be greater than or equal to 0!     ";
                     flag := FALSE;
                  END IF;            
               WHEN "Uniform Integer":
                  IF ((values[1] < 0.) OR (values[1] > values[2]))
                     INC(errors);
                     errorText[errors] := distroUse + "min must be less than or equal to max and greater than or equal to 0!     ";
                     flag := FALSE;
                  END IF;
                  IF ((values[1] > 999999999.999999) OR (values[2] > 999999999.999999))
                     INC(errors);
                     errorText[errors] := distroUse + "min and max must be less than 999,999,999.999999!     ";
                     flag := FALSE;
                  ELSIF (FLOAT(ROUND(values[1])) <> values[1]) OR (FLOAT(ROUND(values[2])) <> values[2])
                     INC(errors);
                     errorText[errors] := distroUse + "min and max must be integers!     ";
                     flag := FALSE;
                  END IF;
                  IF (TRUNC(values[1]) = 0) AND (TRUNC(values[2]) = 0)
                     INC(errors);
                     errorText[errors] := distroUse + "min and max cannot both be equal to 0!     ";
                     flag := FALSE;
                  END IF;
                  EXIT;
               WHEN "Uniform Real":
                  IF ((values[1] < 0.) OR (values[1] > values[2]))
                     INC(errors);
                     errorText[errors] := distroUse + "min must be less than or equal to max and greater than or equal to 0!     ";
                     flag := FALSE;
                  END IF;
                  IF (((values[1] >= 0.) AND (values[1] < 0.000001)) AND ((values[2] >= 0.) AND (values[2] < 0.000001)))
                     INC(errors);
                     errorText[errors] := distroUse + "min and max cannot both be equal to 0!     ";
                     flag := FALSE;
                  END IF;
                  EXIT;
               WHEN "Triangular":
                  IF (values[1] >= values[2]) OR (values[1] >= values[3]) OR (values[2] > values[3])
                     INC(errors);
                     errorText[errors] := distroUse + "Min < Mode < Max must be observed!     ";
                     flag := FALSE;
                  END IF;
                  IF values[1] < 0.
                     INC(errors);
                     errorText[errors] := distroUse + "min must be greater than or equal to 0!     ";
                     flag := FALSE;
                  END IF;
                  EXIT;
               WHEN "None":
               OTHERWISE
                  INC(errors);
                  errorText[errors] := "ERROR: Unknown parameter value box!     ";
                  flag := FALSE;
            END CASE;
         END IF;
      END FOR;
      DISPOSE(message);
      DISPOSE(type);
   END METHOD; {CheckValidInput}

   ASK METHOD BeSelected;
   BEGIN
      lastClicked := ASK SELF LastPicked;
      IF lastClicked.ReferenceName = "HelpButton"
         CallHelp(lastClicked.Id);
      END IF;
   END METHOD; {BeSelected}
END OBJECT; {HelpBoxObj}

OBJECT EventsBoxObj;
   ASK METHOD ReceiveData(INOUT event : RBDEventObj;
                          OUT   cancel : BOOLEAN);
   VAR
      i, fStream, fDist               : INTEGER;
      validData, phasing              : BOOLEAN;
      nextString, eName, comment      : STRING;
      sCost, fCost, iCost, fVal       : REAL;
      text                            : strArray;
   BEGIN
   
      SetLabel("Event Properties - " + event.name + " - " + INTTOSTR(event.Id));
   
      eventName         := Descendant("EventName", 101);
      probVal           := Descendant("ProbVal", 102);
      commentBox        := Descendant("CommentBox", 104);
      costTab           := Child("CostTab", 200);
      initCost          := Descendant("InitCost", 201);
      successCost       := Descendant("SuccessCost", 202);
      failCost          := Descendant("FailCost", 203);
      advTab            := Child("AdvTab", 300);
      streamRadBox      := Descendant("StreamRadBox", 301);
      sysButton         := ASK streamRadBox Child("SysButton", 3011);
      indButton         := ASK streamRadBox Child("IndepButton", 3012);
      sysCombo          := Descendant("SysComboBox", 302);
      indCombo          := Descendant("IndComboBox", 303);
      phaseChk          := Descendant("PhaseCheck", 304);
      indChkBox         := Descendant("IndChkBox", 306);
{**************************** General Tab *********************************}      
      ASK eventName TO SetText(event.name);
      ASK probVal   TO DisplayValue(event.failVals[1]);
      IF event.comment <> ""
         ASK commentBox TO SetText(event.comment);
      ELSE
         ASK commentBox TO SetText("Comment");
      END IF;
{****************************** Cost Tab **********************************}      
      ASK initCost     TO DisplayValue(event.initialCost);  
      ASK successCost  TO DisplayValue(event.operatingCost);
      ASK failCost     TO DisplayValue(event.repairingCost); 
{**************************** Advanced Tab ********************************}
      IF event.failStream > 200
         ASK streamRadBox TO SetSelectedButton(sysButton);
         IF event.failStream = 201
            ASK sysCombo TO SetText("A");
         ELSIF event.failStream = 202
            ASK sysCombo TO SetText("B");
         ELSIF event.failStream = 203
            ASK sysCombo TO SetText("C");
         ELSIF event.failStream = 204
            ASK sysCombo TO SetText("D");
         ELSIF event.failStream = 205
            ASK sysCombo TO SetText("E");
         ELSIF event.failStream = 206
            ASK sysCombo TO SetText("F");
         ELSIF event.failStream = 207
            ASK sysCombo TO SetText("G");
         ELSIF event.failStream = 208
            ASK sysCombo TO SetText("H");
         ELSIF event.failStream = 209
            ASK sysCombo TO SetText("I");
         ELSIF event.failStream = 210
            ASK sysCombo TO SetText("J");
         END IF;
         ASK indCombo  TO Deactivate;
         ASK indChkBox TO Deactivate;
      ELSE 
         ASK streamRadBox TO SetSelectedButton(indButton);
         ASK sysCombo     TO Deactivate;
         IF event.failStream < 0
            ASK indChkBox TO SetCheck(TRUE); 
            ASK indCombo  TO SetText(INTTOSTR(-1*event.failStream));
         ELSE
            ASK indCombo  TO SetText(INTTOSTR(event.failStream));
         END IF;
      END IF;
      IF event.usesPhasing
         ASK phaseChk TO DisplayCheck(TRUE);
      END IF;
      IF compileType = "student"
         ASK phaseChk TO Deactivate;
         ASK costTab TO Deactivate;
      END IF;
{########################## ASK FOR USER INPUT ############################}
      ASK eventName TO ReceiveFocus;
      Draw;
      NEW(message,   1..1);
      NEW(text,1..10);
      REPEAT
         cancel    := FALSE;
         validData := TRUE;
         errors    := 0;
         button := AcceptInput();
         IF button.ReferenceName = "CancelButton"   {User clicked cancel}
            cancel := TRUE;
         ELSE                                       {User clicked OK}
            nextString := eventName.Text();
            IF ((nextString = "") OR (POSITION(nextString, " ") <> 0))
               INC(errors);
               text[errors] := "'Event Name' field can't be blank or have blank spaces!     ";
               ASK eventName TO SetText(event.name);
               validData := FALSE;
            ELSIF STRLEN(nextString) > 20
               INC(errors);
               text[errors] := "'Event Name' must be no greater than 20 characters!     ";
               ASK eventName TO SetText(SUBSTR(1,20,nextString));
               validData := FALSE;
            ELSE
               eName := nextString;
            END IF;
            IF (probVal.Value() < 0.) OR (probVal.Value() > 1.)
               INC(errors);
               text[errors] := "Success probability must be between 0 and 1!     ";
               ASK probVal TO SetValue(0.80);
               validData := FALSE;
            ELSE
               fVal := probVal.Value();
            END IF;
            fDist := 17;
            comment := commentBox.Text();
{Advanced stuff}
            IF streamRadBox.SelectedButton.Id = 3011
               nextString := sysCombo.Text();
               IF nextString = "A"
                  fStream := 201;
               ELSIF nextString = "B"
                  fStream := 202;
               ELSIF nextString = "C"
                  fStream := 203;
               ELSIF nextString = "D"
                  fStream := 204;
               ELSIF nextString = "E"
                  fStream := 205;
               ELSIF nextString = "F"
                  fStream := 206;
               ELSIF nextString = "G"
                  fStream := 207;
               ELSIF nextString = "H"
                  fStream := 208;
               ELSIF nextString = "I"
                  fStream := 209;
               ELSIF nextString = "J"
                  fStream := 210;
               END IF;
            ELSE
               IF indChkBox.Checked
                  fStream := -1*STRTOINT(indCombo.Text());
               ELSE
                  fStream := STRTOINT(indCombo.Text());
               END IF;
            END IF;
            IF phaseChk.Checked
               phasing := TRUE;
            ELSE
               phasing := FALSE;
            END IF;
            IF (initCost.Value() < 0.) OR (initCost.Value() > 999999999.)
               INC(errors);
               text[errors] := "Initial cost must be between 0 and 999,999,999!     ";
               ASK initCost TO SetValue(1.);
               validData := FALSE;
            ELSE
               iCost  := initCost.Value();
            END IF;
            IF (successCost.Value() < 0.) OR (successCost.Value() > 999999999.)
               INC(errors);
               text[errors] := "Cost per success must be between 0 and 999,999,999!     ";
               ASK successCost TO SetValue(1.);
               validData := FALSE;
            ELSE
               sCost  := successCost.Value();
            END IF;
            IF (failCost.Value() < 0.) OR (failCost.Value() > 999999999.)
               INC(errors);
               text[errors] := "Cost per failure must be between 0 and 999,999,999!     ";
               ASK failCost TO SetValue(1.);
               validData := FALSE;
            ELSE
               fCost := failCost.Value();
            END IF;
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
      UNTIL validData;
      IF NOT cancel
         somethingChanged := TRUE;
         ASK event TO SetEventData(phasing, fDist, fStream, sCost, fCost, iCost, fVal, eName, comment);
      END IF;
      DISPOSE(message);
      DISPOSE(text);
   END METHOD; {ReceiveData}

   ASK METHOD BeSelected;
   VAR
      tabClicked             : GraphicVObj;
      distroStr              : STRING;
      i,distroNum,numParams  : INTEGER;
      tags                   : strArray;
   BEGIN   
      tabClicked := LastPicked;
      CASE tabClicked.Id
         WHEN 300:{Advanced tab}
            lastClicked := ASK advTab LastPicked;
            CASE lastClicked.Id
               WHEN 301:{Stream radio box}
                  IF streamRadBox.SelectedButton.Id = 3011 {System stream}
                     ASK sysCombo TO Activate;
                     ASK indCombo TO Deactivate;
                     ASK indChkBox TO Deactivate;
                  ELSE
                     ASK sysCombo TO Deactivate;
                     ASK indCombo TO Activate;
                     ASK indChkBox TO Activate;
                  END IF;
                  Update;
               OTHERWISE
            END CASE;
         OTHERWISE
      END CASE;      
      INHERITED BeSelected;
   END METHOD; {BeSelected}
END OBJECT;{EventsBoxObj}

OBJECT BlockPropObj;
   ASK METHOD ReceiveData     (INOUT dBlock            : RBDBlockObj;
                               IN  helpCase            : STRING;
                               OUT cancel              : BOOLEAN);
   VAR
      validData,operating,fFlag,rFlag                  : BOOLEAN;
      i,numInPool,distroNum,nextInt,nextInt1,k, j,
      numParams, counter                               : INTEGER;
      nextString, fDistro, rDistro, dbUnits,buddyPos,
      distroString                                     : STRING;
      nextReal,mu,sigma2,logMean2,logVar,startfVal1, 
      startfVal2,startrVal1,startrVal2,tempMean,x,y,z  : REAL;
      fTags, rTags, strsArray, fUnits, rUnits          : strArray;
      intsArray                                        : intArray;
      boolsArray                                       : boolArray;
      sparing                                          : SparingType;
      pool                                             : SparePoolObj;
      fVals, rVals, fOff, rOff                         : realArray;
      tempBlock                                        : RBDBlockObj;
      tempEvent                                        : RBDEventObj;
      tempNode, temp2Node                              : RBDNodeObj;
      tempHier                                         : RBDHierObj;
      tempLib                                          : OptionListType;
      text                                             : strArray;
      sbStressMin                                      : REAL;    {BINS    cmc}
       
      
   BEGIN
      helpNum          := STRTOINT(helpCase);
      
      {Failure && Repair Distribution Tab}
      distroTab        := Child("DistroTab", 100);
      nameBox          := Descendant("NameBox", 101);
      blockTypeCombo   := Descendant("BlockTypeCombo", 102);
      blockTypeText    := Descendant("BlockTypeText", 0);
      commentBox       := Descendant("CommentBox", 103);
      failCombo        := Descendant("FailCombo", 104);
      repairCombo      := Descendant("RepairCombo", 105);
      failMean         := Descendant("FailMean", 106);
      failSTDev        := Descendant("FailSTDev", 107);
      repairMean       := Descendant("RepairMean", 108);
      repairSTDev      := Descendant("RepairSTDev", 109);
      updateButton     := Descendant("UpdateButton", 110); 
      
      NEW(failLabels, 1..3);
      NEW(failVals, 1..3);
      NEW(failUnits, 1..3);
      NEW(repairLabels, 1..3);
      NEW(repairVals, 1..3);
      NEW(repairUnits, 1..3);
      NEW(fUnits, 1..3);
      NEW(rUnits, 1..3);
      NEW(costVals, 1..14);
      tempPreDist := dBlock.preDist;
      GetNumParams(tempPreDist, numParams);
      NEW(preVals, 1..numParams);
      FOR i:=1 TO numParams
         preVals[i] := dBlock.preParams[i];
      END FOR;
      tempPostDist := dBlock.postDist;
      GetNumParams(tempPostDist, numParams);
      NEW(postVals, 1..numParams);
      FOR i:=1 TO numParams
         postVals[i] := dBlock.postParams[i];
      END FOR;

      FOR i := 1 TO 3
         failLabels[i]   := Descendant("FailLabel", i);
         failVals[i]     := Descendant("FailVal", i);
         failUnits[i]    := Descendant("FailUnit", i);
         repairLabels[i] := Descendant("RepairLabel", i);
         repairVals[i]   := Descendant("RepairVal", i);
         repairUnits[i]  := Descendant("RepairUnit", i);
      END FOR;
      
      {Maintenance Information Tab}
      maintTab           := Child("MaintTab", 200);
      maintRadBox        := Descendant("MaintRadBox", 201);
         infRadButton    := ASK maintRadBox Child("InfRadButton", 2011);
         poolRadButton   := ASK maintRadBox Child("PoolRadButton", 2012);
         cusRadButton    := ASK maintRadBox Child("CusRadButton", 2013);
      poolCombo          := Descendant("PoolComboBox", 202);
      initStock          := Descendant("InitStockLabel",203);
      initStockVal       := Descendant("InitStockVal", 204);
      newChk             := Descendant("NewChkBox", 205);
      newSparesVal       := Descendant("NewSparesVal", 206);
      newSparesText      := Descendant("NewSparesText", 207);
      arrRateVal         := Descendant("ArrRateVal", 208);
      timeLabel1         := Descendant("TimeLabel1", 209);
      stockChk           := Descendant("StockChkBox", 210);
      stockTxt1          := Descendant("StockTxt1", 211);
      stockVal           := Descendant("StockValBox", 212);
      stockTxt2          := Descendant("StockTxt2", 213);
      stockAmount        := Descendant("StockAmtVal", 214);
      stockTxt3          := Descendant("StockTxt3", 215);
      stockRate          := Descendant("StockRateVal", 216);
      timeLabel4         := Descendant("TimeLabel4", 217);
      emerChk            := Descendant("EmerChkBox", 218);
      emerText           := Descendant("EmerText", 219);
      emerVal            := Descendant("EmerValBox", 220);
      timeLabel2         := Descendant("TimeLabel2", 221);
      preTable           := Descendant("ALDTTable", 224); 
      timeLabel3         := Descendant("TimeLabel3", 225);
      postTable          := Descendant("PostTable", 228);      
      timeLabel5         := Descendant("TimeLabel5", 229);
      createSpare        := Descendant("CreateSpare", 230);
      { Preventive Maintenance Tab}
      prevMaintTab       := Child("PrevMaintTab", 300);
      pmActivitiesChkBox := Descendant("PMActivitiesChkBox", 301);
      freqLabel          := Descendant("FreqLabel", 302);
      freqRadBox         := Descendant("FreqRadBox", 303);
         hourRadButton   := ASK freqRadBox Child("HourRadButton", 3031);
         trigRadButton   := ASK freqRadBox Child("TrigRadButton", 3032);
      hourValBox         := Descendant("HourValBox", 304);
      unitsLabel1        := Descendant("UnitsLabel1", 305);
      trigComboBox       := Descendant("TrigComboBox", 306);
      firesLabel         := Descendant("FiresLabel", 307);
      durationLabel      := Descendant("DurationLabel", 308);
      durTable           := Descendant("DurTable", 309);
      unitsLabel2        := Descendant("UnitsLabel2", 310);
      staggerLabel       := Descendant("StaggerLabel", 311);
      staggerVal         := Descendant("StaggerVal", 312);
      unitsLabel3        := Descendant("UnitsLabel3", 313);
      optionsLabel       := Descendant("OptionsLabel", 314);
      spareChkBox        := Descendant("SpareChkBox", 315);
      refreshChkBox      := Descendant("RefreshChkBox", 316);
      misDefChkBox       := Descendant("MisDefChkBox", 317);
      failChkBox         := Descendant("FailChkBox", 318);
      resDefChkBox       := Descendant("ResDefChkBox", 319);
      createTrigger      := Descendant("CreateTrigger", 320);
      distLabel          := Descendant("DistLabel", 0);
      
      tempDurDist := dBlock.pmDist;
      GetNumParams(tempDurDist, numParams);
      NEW(tempDurParams, 1..numParams);
      FOR i:=1 TO numParams
        tempDurParams[i] := dBlock.pmParams[i];
      END FOR;


      
      {Resources Tab}
      resTab             := Child("ResTab", 400);
      res1Txt1           := Descendant("Res1Txt1", 401);
      res1Combo          := Descendant("Res1ComboBox", 402);
      res1Txt2           := Descendant("Res1Txt2", 403);
      res1Req            := Descendant("Res1ReqVal", 404);
      res1Txt3           := Descendant("Res1Txt3", 405);
      res1ReqPM          := Descendant("Res1ReqPMVal", 406);
      
      res2Txt1           := Descendant("Res2Txt1", 407);
      res2Combo          := Descendant("Res2ComboBox", 408);
      res2Txt2           := Descendant("Res2Txt2", 409);
      res2Req            := Descendant("Res2ReqVal", 410);
      res2Txt3           := Descendant("Res2Txt3", 411);
      res2ReqPM          := Descendant("Res2ReqPMVal", 412);
      
      res3Txt1           := Descendant("Res3Txt1", 413);
      res3Combo          := Descendant("Res3ComboBox", 414);
      res3Txt2           := Descendant("Res3Txt2", 415);
      res3Req            := Descendant("Res3ReqVal", 416);
      res3Txt3           := Descendant("Res3Txt3", 417);
      res3ReqPM          := Descendant("Res3ReqPMVal", 418);

      res4Txt1           := Descendant("Res4Txt1", 419);
      res4Combo          := Descendant("Res4ComboBox", 420);
      res4Txt2           := Descendant("Res4Txt2", 421);
      res4Req            := Descendant("Res4ReqVal", 422);
      res4Txt3           := Descendant("Res4Txt3", 423);
      res4ReqPM          := Descendant("Res4ReqPMVal", 424);

      res5Txt1           := Descendant("Res5Txt1", 425);
      res5Combo          := Descendant("Res5ComboBox", 426);
      res5Txt2           := Descendant("Res5Txt2", 427);
      res5Req            := Descendant("Res5ReqVal", 428);
      res5Txt3           := Descendant("Res5Txt3", 429);
      res5ReqPM          := Descendant("Res5ReqPMVal", 430);
      
      resAcqText         := Descendant("ResAcqText", 431);
      hoardRadBox        := Descendant("HoardRadBox", 432);
      hoardRadButton     := Descendant("HoardRadButton", 4321);
      antiRadButton      := Descendant("AntiRadButton", 4322);
      priorityRadButton  := Descendant("PriorityRadButton", 4323);
      priorityValBox     := Descendant("PriorityValBox", 433);
      
      poolTypeText       := Descendant("PoolTypeText", 434);
      afterFailText      := Descendant("AfterFailText", 435);
      forPMText          := Descendant("ForPMText", 436);
      createResources    := Descendant("CreateResources", 437);
      
      defButton          := Descendant("ResetButton", 9022);     
      {Costs Tab}
      costTab            := Child("CostTab", 500);
      unitsLabel4        := Descendant("UnitsLabel4", 301);
     
      costVals[1]        := Descendant("OpsCostVal", 304);
      costVals[2]        := Descendant("StandbyCostVal", 308);
      standbyCostTxt     := Descendant("StandbyCostText", 309);
      unitsLabel5        := Descendant("UnitsLabel5", 310);
      costVals[3]        := Descendant("RepCostVal", 311);
      costVals[4]        := Descendant("FixCostVal", 313);
      costVals[5]        := Descendant("PMCostVal", 314);
      costVals[6]        := Descendant("PMFixedVal", 316);
      costVals[7]        := Descendant("HoldCostVal", 317);
      holdCostText       := Descendant("HoldCostText", 318); 
      costVals[8]        := Descendant("PMHoldsVal", 319);
      costVals[9]        := Descendant("IdleCostVal", 321);
      idleCostText       := Descendant("IdleCostText", 322);
      costVals[10]       := Descendant("DoneCostVal", 323);
      costVals[11]       := Descendant("InitCostVal", 325);
      costVals[12]       := Descendant("SpCostVal", 327);
      spCostText         := Descendant("SpCostText", 328);
      costVals[13]       := Descendant("ShipCostVal", 329);
      shipCostText       := Descendant("ShipCostText", 330);
      costVals[14]       := Descendant("RebOrDisVal", 331);
      doneBool           := Descendant("DoneBoolean", 333);
      
      {Dependency Tab}
      dependTab          := Child("DependTab", 600);
      dependRadBox       := Descendant("DependRadBox", 601);
         indepButton     := ASK dependRadBox Child("IndepButton", 6011);
         localButton     := ASK dependRadBox Child("LocalButton", 6012);
         sysButton       := ASK dependRadBox Child("SysButton", 6013);
         itemButton      := ASK dependRadBox Child("ItemButton", 6014);
      buddyCombo         := Descendant("BuddyCombo", 602);
      defineDepText      := Descendant("DefineDepText", 605);
      opValBox           := Descendant("OpValBox", 606);
      opText             := Descendant("OpText", 607);
      idleValBox         := Descendant("IdleValBox", 608);
      idleText           := Descendant("IdleText", 609);
      PMValBox           := Descendant("PMValBox", 610);
      PMText             := Descendant("PMText", 611);
      failValBox         := Descendant("FailValBox", 612);
      failedText         := Descendant("FailText", 613);
      lifeExBox          := Descendant("LifeExBox", 614);
      blockRadBox        := Descendant("BlockRadBox", 615);
         opRadButton        := ASK blockRadBox Child("OpRadButt", 6151);
         idleRadButton      := ASK blockRadBox Child("IdleRadButt", 6152);
      PMText2            := Descendant("PMText2", 616);
      defaultDepText     := Descendant("DefaultDepText", 617);
      
      {Simulation Theory Tab}
      simTheoryTab       := Child("SimTheoryTab", 700);
      streamRadBox       := Descendant("StreamRadBox", 701);
         sysStrmButton   := ASK streamRadBox Child("SysStreamButton", 7011);
         indStrmButton   := ASK streamRadBox Child("IndStreamButton", 7012);
      sysCombo           := Descendant("SysComboBox", 702);
      failText           := Descendant("FailText", 703);
      indFCombo          := Descendant("IndFailCombo", 704);
      failAntiChkBox     := Descendant("FailAntiChkBox", 705);
      repairText         := Descendant("RepairText", 706);
      indRCombo          := Descendant("IndRepCombo", 707);
      repairAntiChkBox   := Descendant("RepairAntiChkBox", 708);
      startStateText     := Descendant("StartStateText", 0);
      startRadBox        := Descendant("StartRadBox", 709);
         upButton        := ASK startRadBox Child("UpButton", 7091);
         downButton      := ASK startRadBox Child("DownButton", 7092);
         randButton      := ASK startRadBox Child("RandButton", 7093);
      failedRadBox       := Descendant("FailedRadBox", 710);
         repairButton    := ASK failedRadBox Child("RepairButton", 7101);
         randomButton    := ASK failedRadBox Child("RandomButton", 7102);
         preButton       := ASK failedRadBox Child("PreButton", 7103);
      expiredVal         := Descendant("ExpiredVal", 711);
      timeLabel6         := Descendant("TimeLabel6", 712);
      
      {Advanced Tab}
      advTab             := Child("AdvTab", 800);
      phaseChk           := Descendant("PhaseChkBox", 801);
      degradeChk         := Descendant("DegradeChkBox", 803);
      degradeRadBox      := Descendant("DegradeRadBox", 805);
         linearButton    := Descendant("LinearButton", 8051);
         geoButton       := Descendant("GeoButton", 8052);
         assButton       := Descendant("AssButton", 8053);
      degradeTxt1        := Descendant("DegradeTxt1", 806);
      degradeVal1        := Descendant("DegradeVal1", 807);
      degradeTxt2        := Descendant("DegradeTxt2", 808);
      degradeVal2        := Descendant("DegradeVal2", 809);
      degradeTxt3        := Descendant("DegradeTxt3", 810);
      SBStressText       := Descendant("SBStressText", 811);
      SBStressVal        := Descendant("SBStressVal", 812);
      
      
      NEW(fTags, 1..3);
      NEW(rTags, 1..3);
      NEW(fOff, 1..3);
      NEW(rOff, 1..3);
      {NEW(eventList, 1..4);}
      IF totalObjects > 1
         NEW(buddyList, 1..(totalObjects-1));
      END IF;
      {GetResList(resList);}
      NEW(tempResList, 1..totalRes+1);
      tempResList[1] := "None";
      FOR i := 1 TO totalRes
         tempResList[i+1] := resList[i];
      END FOR;
     { eventList[1] := "Normal";     eventList[2] := "Point Estimate";
      eventList[3] := "Triangular"; eventList[4] := "Uniform Real";
      NEW(opList, 1..17);
      opList[1]  := "Beta";         opList[2]  := "Binomial";
      opList[3]  := "Chi-square";   opList[4]  := "Empirical";
      opList[5]  := "Erlang";       opList[6]  := "Exponential";
      opList[7]  := "Gamma";        opList[8]  := "Lognormal";
      opList[9]  := "Normal";       opList[10] := "Pearson 5";
      opList[11] := "Pearson 6";    opList[12] := "Poisson";
      opList[13] := "Triangular";   opList[14] := "Uniform Integer";
      opList[15] := "Uniform Real"; opList[16] := "Weibull";}
      dbUnits := SUBSTR(1, STRLEN(systemUnits)-1, systemUnits);
      IF helpNum <> 952
         ASK defButton TO SetHidden(TRUE);
      ELSE
         ASK createTrigger TO SetHidden(TRUE);
         ASK createResources TO SetHidden(TRUE);
         ASK createSpare TO SetHidden(TRUE);
      END IF;
{******************************* Distribution Tab *******************************}            
      ASK nameBox TO SetText(dBlock.name);
      lastType := "Default";
      IF libArray = NILARRAY
         ASK blockTypeText TO Deactivate;
         ASK blockTypeCombo TO Deactivate;
      ELSE
         NEW(tempLib, 1.. HIGH(libArray)+1);
         tempLib[1] := "Default";
         FOR i := 1 TO HIGH(nameArray)
            tempLib[i+1] := nameArray[i];
         END FOR;
         ASK blockTypeCombo TO SetOptions(tempLib);
         ASK blockTypeCombo TO SetText("Default");
      END IF;
      SetLabel("Block Properties - "+dBlock.name + " - " + INTTOSTR(dBlock.Id));
 
      ASK commentBox TO SetText(dBlock.comment);
      IF dBlock.comment = ""
         ASK commentBox TO SetText("Comment");
      END IF;
      ConvertToString(dBlock.failDistro, fDistro);
      GetParameters(dBlock.failDistro, "failure", fTags, fUnits, fOff);
      ConvertToString(dBlock.repairDistro, rDistro);
      GetParameters(dBlock.repairDistro, "repair", rTags, rUnits, rOff);
      ASK failCombo TO DisplayText(fDistro);
      ASK repairCombo TO DisplayText(rDistro);
      FOR i := 1 TO 3
         ASK failLabels[i] TO DisplayAt(fOff[i], (5.+FLOAT(i*2)));
         ASK failLabels[i] TO SetLabel(fTags[i]);
         ASK failUnits[i] TO SetLabel(fUnits[i]);
         IF fTags[i] = ""
            ASK failVals[i] TO SetHidden(TRUE);
            ASK failUnits[i] TO SetHidden(TRUE);
         END IF;
         ASK repairLabels[i] TO DisplayAt(rOff[i]+44., (5.+FLOAT(i*2)));
         ASK repairLabels[i] TO SetLabel(rTags[i]);
         ASK repairUnits[i]  TO SetLabel(rUnits[i]);
         IF rTags[i] = ""
            ASK repairVals[i] TO SetHidden(TRUE);
            ASK repairUnits[i] TO SetHidden(TRUE);
         END IF;
      END FOR;
      
      IF dBlock.failMean = 12345.6789
         ASK failMean TO SetLabel("Undefined");
      ELSIF dBlock.failMean = 12345.6788
         ASK failMean TO SetLabel("N/A");
      ELSE
         ASK failMean TO SetLabel(REALTOSTR(dBlock.failMean));
      END IF;
      IF dBlock.failSTDev = 12345.6789
         ASK failSTDev TO SetLabel("Undefined");
      ELSIF dBlock.failSTDev = 12345.6788
         ASK failSTDev TO SetLabel("N/A");
      ELSE
         ASK failSTDev TO SetLabel(REALTOSTR(dBlock.failSTDev));
      END IF;
 
      IF dBlock.repairMean = 12345.6789
         ASK repairMean TO SetLabel("Undefined");
      ELSIF dBlock.repairMean = 12345.6788
         ASK repairMean TO SetLabel("N/A");
      ELSE
         ASK repairMean TO SetLabel(REALTOSTR(dBlock.repairMean));
      END IF;
      IF dBlock.repairSTDev = 12345.6789
         ASK repairSTDev TO SetLabel("Undefined");
      ELSIF dBlock.repairSTDev = 12345.6788
         ASK repairSTDev TO SetLabel("N/A");
      ELSE
         ASK repairSTDev TO SetLabel(REALTOSTR(dBlock.repairSTDev));
      END IF;
       
      FOR i := 1 TO dBlock.numFailParams
         IF ((lambdaMode) AND (dBlock.failDistro=4) AND (i=1))
            ASK failVals[i] TO DisplayValue(1.0 / dBlock.failVals[i]);
            startfVal1:=failVals[1].Value();
         ELSIF ((muSigmaMode) AND (dBlock.failDistro=7) AND (i=1))
            logMean2 := POWER(dBlock.failVals[1], 2.);
            logVar := POWER(dBlock.failVals[2], 2.);
            ASK failVals[i] TO DisplayValue(LN(logMean2/SQRT(logVar + logMean2))); 
            startfVal1:=failVals[1].Value();           
         ELSIF ((muSigmaMode) AND (dBlock.failDistro=7) AND (i=2))
            ASK failVals[i] TO DisplayValue(SQRT(LN((logVar + logMean2)/logMean2)));        
            startfVal2:=failVals[2].Value();           
         ELSIF dBlock.failDistro=16
         ELSE
            ASK failVals[i] TO DisplayValue(dBlock.failVals[i]);
         END IF;
      END FOR;
      FOR i := 1 TO dBlock.numRepairParams
         IF ((lambdaMode) AND (dBlock.repairDistro=4) AND (i=1))
            ASK repairVals[i] TO DisplayValue(1.0 / dBlock.repairVals[i]);
            startrVal1:=repairVals[1].Value();           
         ELSIF ((muSigmaMode) AND (dBlock.repairDistro=7) AND (i=1))
            logMean2 := POWER(dBlock.repairVals[1], 2.);
            logVar := POWER(dBlock.repairVals[2], 2.);
            ASK repairVals[i] TO DisplayValue(LN(logMean2/SQRT(logVar + logMean2)));        
            startrVal1:=repairVals[1].Value();           
         ELSIF ((muSigmaMode) AND (dBlock.repairDistro=7) AND (i=2))
            ASK repairVals[i] TO DisplayValue(SQRT(LN((logVar + logMean2)/logMean2)));        
            startrVal2:=repairVals[2].Value();           
         ELSIF dBlock.repairDistro=16
         ELSE
            ASK repairVals[i] TO DisplayValue(dBlock.repairVals[i]);
         END IF;
      END FOR;
{******************************* Maintenance tab *******************************}
                              
      ASK preTable TO SetSize(1,0);
      ASK preTable TO SetVisibleSize(23,1);
      ASK preTable TO SetText("Fixd (0.000000)", 1,0);
      ASK postTable TO SetSize(1,0);
      ASK postTable TO SetVisibleSize(23,1);
      ASK postTable TO SetText("Fixd (0.000000)", 1,0);
     
      ASK timeLabel1 TO SetLabel(systemUnits);
      ASK timeLabel2 TO SetLabel(systemUnits);
      ASK timeLabel3 TO SetLabel(systemUnits);
      ASK timeLabel4 TO SetLabel(systemUnits);
      ASK timeLabel5 TO SetLabel(systemUnits);
      poolInUse := dBlock.poolName;
      IF dBlock.sparingType = SparePool
         ASK poolCombo     TO SetOptions(spareList);
         ASK maintRadBox   TO SetSelectedButton(poolRadButton);
         ASK poolCombo     TO DisplayText(dBlock.poolName);
         ASK initStockVal  TO SetHidden(TRUE);
         ASK initStock     TO SetHidden(TRUE);
         ASK newChk        TO SetHidden(TRUE);
         ASK newSparesVal  TO SetHidden(TRUE);
         ASK newSparesText TO SetHidden(TRUE);
         ASK arrRateVal    TO SetHidden(TRUE);
         ASK timeLabel1    TO SetHidden(TRUE);
         ASK stockChk      TO SetHidden(TRUE);
         ASK stockTxt1     TO SetHidden(TRUE);
         ASK stockVal      TO SetHidden(TRUE);
         ASK stockTxt2     TO SetHidden(TRUE);
         ASK stockAmount   TO SetHidden(TRUE);
         ASK stockTxt3     TO SetHidden(TRUE);
         ASK stockRate     TO SetHidden(TRUE);
         ASK timeLabel4    TO SetHidden(TRUE);
         ASK emerChk       TO SetHidden(TRUE);
         ASK emerText      TO SetHidden(TRUE);
         ASK emerVal       TO SetHidden(TRUE);
         ASK timeLabel2    TO SetHidden(TRUE);
         ASK costVals[12]        TO Deactivate;
         ASK costVals[13]      TO Deactivate;
      ELSIF ((dBlock.sparingType = Custom) OR (dBlock.sparingType = None))
         ASK maintRadBox   TO SetSelectedButton(cusRadButton);
         ASK poolCombo     TO SetHidden(TRUE);
         oldSpareCost := dBlock.spareCost;
         oldEmerSpCost := dBlock.emerShippingCost;
      ELSIF dBlock.infiniteSpares
         ASK maintRadBox   TO SetSelectedButton(infRadButton);
         ASK poolCombo     TO SetHidden(TRUE);
         ASK initStockVal  TO SetHidden(TRUE);
         ASK initStock     TO SetHidden(TRUE);
         ASK newChk        TO SetHidden(TRUE);
         ASK newSparesVal  TO SetHidden(TRUE);
         ASK newSparesText TO SetHidden(TRUE);
         ASK arrRateVal    TO SetHidden(TRUE);
         ASK timeLabel1    TO SetHidden(TRUE);
         ASK stockChk      TO SetHidden(TRUE);
         ASK stockTxt1     TO SetHidden(TRUE);
         ASK stockVal      TO SetHidden(TRUE);
         ASK stockTxt2     TO SetHidden(TRUE);
         ASK stockAmount   TO SetHidden(TRUE);
         ASK stockTxt3     TO SetHidden(TRUE);
         ASK stockRate     TO SetHidden(TRUE);
         ASK timeLabel4    TO SetHidden(TRUE);
         ASK emerChk       TO SetHidden(TRUE);
         ASK emerText      TO SetHidden(TRUE);
         ASK emerVal       TO SetHidden(TRUE);
         ASK timeLabel2    TO SetHidden(TRUE);
         oldSpareCost := dBlock.spareCost;
         oldEmerSpCost := dBlock.emerShippingCost;
      ELSE
         NEW(message, 1..1);
         message[1] := "ERROR: Unknown Sparing Type!     ";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      END IF;
      IF totalSpares = 0
         ASK poolRadButton TO Deactivate;
      END IF;
      IF dBlock.routineSpareOrdering
         ASK newChk TO DisplayCheck(TRUE);
      ELSE
         ASK newChk TO DisplayCheck(FALSE);
         ASK newSparesVal TO Deactivate;
         ASK arrRateVal   TO Deactivate;
      END IF;
      IF dBlock.stockLevelOrdering
         ASK stockChk TO DisplayCheck(TRUE);
      ELSE   
         ASK stockChk TO DisplayCheck(FALSE);
         ASK stockVal TO Deactivate;
         ASK stockAmount TO Deactivate;
         ASK stockRate TO Deactivate;
      END IF;
      IF dBlock.emerSpareOrdering
         ASK emerChk  TO DisplayCheck(TRUE);
      ELSE
         ASK emerChk  TO DisplayCheck(FALSE);
         ASK emerVal  TO Deactivate;
      END IF;
      ASK initStockVal TO DisplayValue(FLOAT(dBlock.initStock));
      ASK newSparesVal TO DisplayValue(FLOAT(dBlock.newSpares));
      MakeDistString(dBlock.preDist, dBlock.preParams, distroString); 
      ASK preTable TO SetText(distroString,1,0);
      MakeDistString(dBlock.postDist, dBlock.postParams, distroString); 
      ASK postTable TO SetText(distroString,1,0);
      
      ASK res1Req      TO DisplayValue(FLOAT(dBlock.numRes1));
      ASK res1ReqPM    TO DisplayValue(FLOAT(dBlock.numRes1PM));
      ASK arrRateVal   TO DisplayValue(dBlock.arrivalRate);
      ASK stockVal     TO DisplayValue(FLOAT(dBlock.SLOOrderLevel));
      ASK stockAmount  TO DisplayValue(FLOAT(dBlock.SLONewSpares));;
      ASK stockRate    TO DisplayValue(dBlock.SLOTime);;
      ASK emerVal      TO DisplayValue(dBlock.emerTime);
      
{************************ Preventive Maintenance tab ************************}  
      ASK pmActivitiesChkBox TO DisplayCheck(dBlock.usesPM); 
      ASK hourValBox TO SetValue(dBlock.pmFreq);
      ASK trigComboBox TO SetText(dBlock.pmTrig);
      GetTrigList(trigList);
      {IF trigList[1] = "unnamed"
         trigList[1] := "";
      END IF;}
      ASK trigComboBox TO SetOptions(trigList);
      IF totalTriggers = 0
         ASK trigRadButton TO Deactivate;
      END IF;
      IF dBlock.pmTriggered                       
        ASK freqRadBox TO SetSelectedButton(trigRadButton);
        ASK hourValBox TO Deactivate;
        ASK staggerLabel TO Deactivate;
        ASK staggerVal TO Deactivate;
        ASK unitsLabel3 TO Deactivate;
      ELSE
        ASK freqRadBox TO SetSelectedButton(hourRadButton);
        ASK trigComboBox TO Deactivate;
      END IF;
      
      MakeDistString(tempDurDist, tempDurParams, distroString);
      ASK durTable TO SetText(distroString, 1, 0);
      ASK durTable TO SetSize(1,0);
      ASK durTable TO SetVisibleSize(23,1);    
      Draw;

      
      ASK unitsLabel1 TO SetLabel(systemUnits);
      ASK unitsLabel2 TO SetLabel(systemUnits);
      ASK unitsLabel3 TO SetLabel(systemUnits);
                            
      ASK staggerVal TO SetValue(dBlock.pmStagger);
      ASK spareChkBox TO DisplayCheck(dBlock.pmSpareNeeded);
      ASK refreshChkBox TO DisplayCheck(dBlock.pmRefresh);
      ASK misDefChkBox TO DisplayCheck(dBlock.pmMisDefer);
      ASK failChkBox TO DisplayCheck(dBlock.pmFailReset);
      ASK resDefChkBox TO DisplayCheck(dBlock.pmReqDefer);
     
      IF dBlock.usesPM <> TRUE      
        ASK freqRadBox TO Deactivate;
        ASK freqLabel TO Deactivate;
        ASK hourRadButton TO Deactivate;
        ASK trigRadButton TO Deactivate;
        ASK hourValBox TO Deactivate;
        ASK unitsLabel1 TO Deactivate;    
        ASK trigComboBox TO Deactivate;
        ASK firesLabel TO Deactivate;
        ASK durationLabel TO Deactivate;
        ASK durTable TO Deactivate;
        ASK durTable TO SetHidden(TRUE);
        ASK distLabel TO SetHidden(FALSE);
        ASK distLabel TO SetLabel(durTable.Text(1,0));
        ASK distLabel TO Deactivate;
        ASK unitsLabel2 TO Deactivate;
        ASK optionsLabel TO Deactivate;
        ASK spareChkBox TO Deactivate;
        ASK refreshChkBox TO Deactivate;
        ASK misDefChkBox TO Deactivate;
        ASK resDefChkBox TO Deactivate;
        ASK failChkBox TO Deactivate;
        ASK unitsLabel3 TO Deactivate;
        ASK staggerLabel TO Deactivate;
        ASK staggerVal TO Deactivate;
      ELSE
         ASK distLabel TO SetHidden(TRUE);
         IF activePhases = 0
            ASK misDefChkBox TO Deactivate;
         END IF;
         IF dBlock.pmTriggered
            ASK failChkBox TO Deactivate;
         END IF;
      END IF;
      IF compileType = "student"
         ASK prevMaintTab TO Deactivate;
      END IF;
        
{*********************************** Cost Tab *********************************}
      ASK unitsLabel4 TO SetLabel(systemUnits);
      ASK unitsLabel5 TO SetLabel(systemUnits);
      IF compileType = "student"
         ASK unitsLabel4 TO Deactivate;
         ASK unitsLabel5 TO Deactivate;
      END IF;      
      ASK costVals[1] TO DisplayValue(dBlock.operatingCost);
      ASK costVals[2] TO DisplayValue(dBlock.standbyCost);
      ASK costVals[3] TO DisplayValue(dBlock.repairingCost);
      ASK costVals[4] TO DisplayValue(dBlock.repFixedCost);
      ASK costVals[5] TO DisplayValue(dBlock.pmCost);     
      ASK costVals[6] TO DisplayValue(dBlock.pmFixedCost);
      ASK costVals[7] TO DisplayValue(dBlock.repHoldCost);
      ASK costVals[8] TO DisplayValue(dBlock.pmHoldCost);
      ASK costVals[9] TO DisplayValue(dBlock.idleCost);
      ASK costVals[10] TO DisplayValue(dBlock.doneCost);
      ASK costVals[11] TO DisplayValue(dBlock.initialCost);  
      ASK costVals[12] TO DisplayValue(dBlock.spareCost);
      ASK costVals[13] TO DisplayValue(dBlock.emerShippingCost);
      ASK costVals[14] TO DisplayValue(dBlock.doneFixedCost);
         
      IF dBlock.alwaysAddDoneCost
         ASK doneBool TO DisplayCheck(TRUE);
      END IF;
{******************************** Dependency Tab *******************************}
      IF ((totalBlocks-1) = 0) AND ((totalNodes-2) = 0) AND (totalHiers = 0) AND (totalEvents = 0)
         ASK itemButton TO Deactivate;
         ASK buddyCombo TO Deactivate;
      END IF;
      IF helpNum <> 952
         i := 1;
         FOREACH tempBlock IN blockGroup
            IF tempBlock.Id <> dBlock.Id
               tempString := INTTOSTR(tempBlock.Id);
               IF tempBlock.Id < 10
                  tempString := "00" + tempString;
               ELSIF tempBlock.Id < 100
                  tempString := "0" + tempString;
               END IF;
               buddyList[i] := tempBlock.name + " block - " + tempString;
               IF (dBlock.DependencyNum > 0) AND (dBlock.depType = "RBDBlock") AND 
                  (tempBlock.Id = dBlock.DependencyNum)
                  buddy := tempBlock.name + " block - " + tempString;
               END IF;
               INC(i);
            END IF;
         END FOREACH;
         FOREACH tempEvent IN eventGroup
            tempString := INTTOSTR(tempEvent.Id);
            IF tempEvent.Id < 10
               tempString := "00" + tempString;
            ELSIF tempEvent.Id < 100
               tempString := "0" + tempString;
            END IF;
            buddyList[i] := tempEvent.name + " event - " + tempString;
            IF (dBlock.DependencyNum > 0) AND (dBlock.depType = "RBDEvent") AND 
               (tempEvent.Id = dBlock.DependencyNum)
               buddy := tempEvent.name + " event - " + tempString;
            END IF;
            INC(i);
         END FOREACH;
         FOREACH tempNode IN nodeGroup
            tempString := INTTOSTR(tempNode.Id);
            IF tempNode.Id < 10
               tempString := "00" + tempString;
            ELSIF tempNode.Id < 100
               tempString := "0" + tempString;
            END IF;
            IF ((tempNode.name = "end") OR (tempNode.name = "start")
                OR (tempNode.typeNode = 4))
               INC(j);
            ELSE
               IF tempNode.typeNode = 5
                  tempHier := ASK root Child("RBDHier", tempNode.parentID);
                  tempString := INTTOSTR(tempNode.parentID);
                  IF tempNode.parentID < 10
                     tempString := "00" + tempString;
                  ELSIF tempNode.parentID < 100
                     tempString := "0" + tempString;
                  END IF;
                  buddyList[i-j] := tempHier.name + " hier - " + tempString;
                  IF (dBlock.DependencyNum > 0) AND (dBlock.depType = "RBDHier")  
                      AND (tempNode.Id = dBlock.DependencyNum)
                     buddy := tempHier.name + " hier - " + tempString;
                  END IF;
               ELSE
                  buddyList[i-j] := tempNode.name + " node - " + tempString;
               END IF;
            END IF;
            IF (tempNode.typeNode <> 5) AND (dBlock.DependencyNum > 0) AND (dBlock.depType = "RBDNode") 
                AND (tempNode.Id = dBlock.DependencyNum)
               buddy := tempNode.name + " node - " + tempString;
            END IF;
            INC(i);
         END FOREACH;
         
         IF totalObjects > 1
            counter := 0;
            FOR i:= 1 TO HIGH(buddyList)
               IF (buddyList[i] = "")
                  INC(counter);
               END IF;
            END FOR;
            IF (HIGH(buddyList) > counter)
               NEW(buddyList2, 1..(HIGH(buddyList) - counter));
            ELSE
               NEW(buddyList2, 1..(HIGH(buddyList)));
            END IF;
            FOR i:= 1 TO HIGH(buddyList2)
               buddyList2[i] := buddyList[i];
            END FOR;
            ASK buddyCombo TO SetOptions(buddyList2);
            IF buddy <> ""
               ASK buddyCombo TO SetText(buddy);
            ELSE
               ASK buddyCombo TO SetText(buddyList2[1]);
            END IF;
         ELSE
            ASK buddyCombo TO Deactivate;
            ASK itemButton TO Deactivate;
         END IF;
         IF dBlock.DependencyNum  = -2
            ASK dependRadBox TO SetSelectedButton(sysButton);
            ASK buddyCombo TO Deactivate;
         ELSIF dBlock.DependencyNum  = 0
            ASK dependRadBox TO SetSelectedButton(indepButton);
            ASK buddyCombo TO Deactivate;
            ASK defineDepText TO Deactivate; 
            ASK opText TO Deactivate;         
            ASK opValBox TO Deactivate;        
            ASK idleValBox TO Deactivate;         
            ASK idleText TO Deactivate;
            ASK PMValBox TO Deactivate;
            ASK PMText TO Deactivate;
            ASK failValBox TO Deactivate;         
            ASK failedText TO Deactivate;
            ASK lifeExBox TO Deactivate;
            ASK blockRadBox TO Deactivate; 
            ASK PMText2 TO Deactivate;
            ASK defaultDepText TO Deactivate;
         ELSIF dBlock.DependencyNum  = -1
            ASK dependRadBox TO SetSelectedButton(localButton);
            ASK buddyCombo TO Deactivate;
         ELSE
            IF totalObjects > 1
               ASK dependRadBox TO SetSelectedButton(itemButton);
               ASK buddyCombo TO SetText(buddy);
               ASK buddyCombo TO Activate;
            ELSE
               ASK dependRadBox TO SetSelectedButton(localButton);
               ASK buddyCombo TO Deactivate;
            END IF;
         END IF;
      ELSIF helpNum = 952
         ASK itemButton TO Deactivate;
         IF dBlock.DependencyNum  = -2
            ASK dependRadBox TO SetSelectedButton(sysButton);
            ASK buddyCombo TO Deactivate;
         ELSIF dBlock.DependencyNum  = 0
            ASK dependRadBox TO SetSelectedButton(indepButton);
            ASK buddyCombo TO Deactivate;
            ASK defineDepText TO Deactivate; 
            ASK opText TO Deactivate;         
            ASK opValBox TO Deactivate;        
            ASK idleValBox TO Deactivate;         
            ASK idleText TO Deactivate;
            ASK PMValBox TO Deactivate;
            ASK PMText TO Deactivate;
            ASK failValBox TO Deactivate;         
            ASK failedText TO Deactivate;
            ASK lifeExBox TO Deactivate;
            ASK blockRadBox TO Deactivate;
            ASK PMText2 TO Deactivate;
            ASK defaultDepText TO Deactivate;
         ELSE
            ASK dependRadBox TO SetSelectedButton(localButton);
            ASK buddyCombo TO Deactivate;
         END IF;
      END IF;
      ASK opValBox   TO DisplayValue(dBlock.DepNothingPerc);
      ASK idleValBox TO DisplayValue(dBlock.DepIdlePerc);
      ASK PMValBox   TO DisplayValue(dBlock.DepPMPerc);
      ASK failValBox TO DisplayValue(dBlock.DepFailPerc);
      ASK lifeExBox  TO DisplayValue(dBlock.DepPMThreshold);
      IF dBlock.defDepStateIdle
         ASK blockRadBox TO SetSelectedButton(idleRadButton);
      ELSE
         ASK blockRadBox TO SetSelectedButton(opRadButton);
      END IF;    
           
{******************************** Resource Tab ********************************}
      resInUse := dBlock.res1Name;
      ASK res1Combo TO SetOptions(tempResList);
      
      ASK res2Txt1 TO Deactivate;        
      ASK res2Combo TO Deactivate;
      ASK res2Txt2 TO Deactivate;
      ASK res2Req TO Deactivate;
      ASK res2Txt3 TO Deactivate;
      ASK res2ReqPM TO Deactivate;
      
      ASK res3Txt1 TO Deactivate;        
      ASK res3Combo TO Deactivate;
      ASK res3Txt2 TO Deactivate;
      ASK res3Req TO Deactivate;
      ASK res3Txt3 TO Deactivate;
      ASK res3ReqPM TO Deactivate;

      ASK res4Txt1 TO Deactivate;
      ASK res4Combo TO Deactivate;
      ASK res4Txt2 TO Deactivate;
      ASK res4Req TO Deactivate;
      ASK res4Txt3 TO Deactivate;
      ASK res4ReqPM TO Deactivate;

      ASK res5Txt1 TO Deactivate;
      ASK res5Combo TO Deactivate;
      ASK res5Txt2 TO Deactivate;
      ASK res5Req TO Deactivate;
      ASK res5Txt3 TO Deactivate;
      ASK res5ReqPM TO Deactivate;
      
      ASK resAcqText TO Deactivate;
      ASK hoardRadBox TO Deactivate;
      ASK priorityValBox TO Deactivate;

      IF dBlock.numDiffRes>0
         ASK res1Combo TO DisplayText(dBlock.res1Name);
         ASK res1Req TO DisplayValue(FLOAT(dBlock.numRes1));
         ASK res1ReqPM TO DisplayValue(FLOAT(dBlock.numRes1PM));
      ELSE
         ASK afterFailText TO Deactivate;
         ASK forPMText TO Deactivate;
         ASK res1Combo TO DisplayText("None");
         ASK res1Txt2 TO Deactivate;
         ASK res1Req TO Deactivate;
         ASK res1Txt3 TO Deactivate;
         ASK res1ReqPM TO Deactivate;
      END IF;
      IF totalRes = 0
         ASK res1Txt1 TO Deactivate;
         ASK poolTypeText TO Deactivate;
         ASK res1Combo TO Deactivate;
      END IF;
{*************************** Simulation Theory Tab ****************************}
      IF dBlock.failStream > 200  {here}
         ASK streamRadBox TO SetSelectedButton(sysStrmButton);
         IF dBlock.failStream = 201
            ASK sysCombo TO SetText("A");
         ELSIF dBlock.failStream = 202
            ASK sysCombo TO SetText("B");
         ELSIF dBlock.failStream = 203
            ASK sysCombo TO SetText("C");
         ELSIF dBlock.failStream = 204
            ASK sysCombo TO SetText("D");
         ELSIF dBlock.failStream = 205
            ASK sysCombo TO SetText("E");
         ELSIF dBlock.failStream = 206
            ASK sysCombo TO SetText("F");
         ELSIF dBlock.failStream = 207
            ASK sysCombo TO SetText("G");
         ELSIF dBlock.failStream = 208
            ASK sysCombo TO SetText("H");
         ELSIF dBlock.failStream = 209
            ASK sysCombo TO SetText("I");
         ELSIF dBlock.failStream = 210
            ASK sysCombo TO SetText("J");
         END IF;
         ASK indFCombo  TO Deactivate;
         ASK failText   TO Deactivate;
         ASK indRCombo  TO Deactivate;
         ASK repairText TO Deactivate;
         ASK repairAntiChkBox TO Deactivate;
         ASK failAntiChkBox TO Deactivate;
      ELSE
         ASK streamRadBox TO SetSelectedButton(indStrmButton);
         IF dBlock.failStream < 0
            ASK failAntiChkBox TO SetCheck(TRUE); 
            ASK indFCombo  TO SetText(INTTOSTR(-1*dBlock.failStream));
         ELSE
            ASK indFCombo  TO SetText(INTTOSTR(dBlock.failStream));
         END IF;
         IF dBlock.repairStream < 0
            ASK repairAntiChkBox TO SetCheck(TRUE);
            ASK indRCombo  TO SetText(INTTOSTR(-1*dBlock.repairStream));         
         ELSE
            ASK indRCombo  TO SetText(INTTOSTR(dBlock.repairStream));
         END IF;
         
         ASK indFCombo  TO Activate;
         ASK failText   TO Activate;
         ASK indRCombo  TO Activate;
         ASK repairText TO Activate;
         ASK repairAntiChkBox TO Activate;
         ASK failAntiChkBox TO Activate;
         ASK sysCombo   TO Deactivate;
      END IF;
      ASK expiredVal TO SetValue(dBlock.amountExhausted);
      IF dBlock.simStartType = 1
         ASK startRadBox TO SetSelectedButton(randButton);
         ASK failedRadBox TO SetHidden(TRUE);
         ASK expiredVal TO SetHidden(TRUE);
         ASK timeLabel6 TO SetHidden(TRUE);
         ASK preButton TO SetHidden(TRUE);
      ELSIF dBlock.simStartType = 2
         ASK startRadBox TO SetSelectedButton(upButton);
         ASK failedRadBox TO SetSelectedButton(randomButton);
         ASK repairButton TO SetLabel("Some life exhausted:");
         ASK expiredVal TO SetHidden(TRUE);
         ASK timeLabel6 TO SetHidden(TRUE);
         ASK preButton TO SetHidden(TRUE);
      ELSIF dBlock.simStartType = 3
         ASK startRadBox TO SetSelectedButton(upButton);
         ASK failedRadBox TO SetSelectedButton(repairButton);
         ASK repairButton TO SetLabel("Some life exhausted:");
         ASK expiredVal TO DisplayValue(dBlock.amountExhausted);
         ASK timeLabel6 TO SetLabel(systemUnits);
         ASK preButton TO SetHidden(TRUE);
      ELSIF dBlock.simStartType = 4
         ASK startRadBox TO SetSelectedButton(downButton);
         ASK failedRadBox TO SetSelectedButton(randomButton);
         ASK expiredVal TO SetHidden(TRUE);
         ASK timeLabel6 TO SetHidden(TRUE);
      ELSIF dBlock.simStartType = 5
         ASK startRadBox TO SetSelectedButton(downButton);
         ASK failedRadBox TO SetSelectedButton(repairButton);
         ASK expiredVal TO DisplayValue(dBlock.amountExhausted);
         ASK timeLabel6 TO SetLabel("% complete");
      ELSE 
         ASK startRadBox TO SetSelectedButton(downButton);
         ASK failedRadBox TO SetSelectedButton(preButton);
         ASK expiredVal TO SetHidden(TRUE);
         ASK timeLabel6 TO SetHidden(TRUE);
      END IF;

{********************************* Advanced Tab ********************************}
      IF dBlock.usesPhasing
         ASK phaseChk TO DisplayCheck(TRUE);
      ELSE
         ASK phaseChk TO DisplayCheck(FALSE);
      END IF;
      IF dBlock.Id = 0                  {sjp 4/29/2009}
         ASK phaseChk TO Deactivate();  {sjp 4/29/2009}
      END IF;                           {sjp 4/29/2009}
      IF dBlock.GDType = 0
         ASK degradeChk TO DisplayCheck(FALSE);
         ASK degradeRadBox TO Deactivate;
         ASK degradeTxt1 TO SetHidden(TRUE);
         ASK degradeVal1 TO SetValue(0.);
         ASK degradeVal1 TO SetHidden(TRUE);
         ASK degradeVal2 TO SetValue(dBlock.failMean);
         ASK degradeTxt2 TO SetHidden(TRUE);
         ASK degradeVal2 TO SetHidden(TRUE);
         ASK degradeTxt3 TO SetHidden(TRUE);
      ELSIF dBlock.GDType = 1
         ASK degradeChk TO DisplayCheck(TRUE);
         ASK degradeRadBox TO SetSelectedButton(linearButton);
         ASK degradeTxt1 TO SetLabel("Slope");
         ASK degradeVal1 TO DisplayValue(dBlock.GDRate);
         ASK degradeVal2 TO DisplayValue(dBlock.GDLimit);
         ASK degradeTxt3 TO SetLabel("formula: (Sx+1)*M)");
      ELSIF dBlock.GDType = 2
         ASK degradeChk TO DisplayCheck(TRUE);
         ASK degradeRadBox TO SetSelectedButton(geoButton);
         ASK degradeTxt1 TO SetLabel("Base");
         ASK degradeVal1 TO DisplayValue(dBlock.GDRate);
         ASK degradeVal2 TO DisplayValue(dBlock.GDLimit);
         ASK degradeTxt3 TO SetLabel("formula: (B^x)*M)");
      ELSE
         ASK degradeChk TO DisplayCheck(TRUE);
         ASK degradeRadBox TO SetSelectedButton(assButton);
         ASK degradeTxt1 TO SetLabel("Rate");
         ASK degradeVal1 TO DisplayValue(dBlock.GDRate);
         ASK degradeVal2 TO DisplayValue(dBlock.GDLimit);
         ASK degradeTxt3 TO SetLabel("formula: L+(M-L)*e^(-xR)");
      END IF;

      ASK SBStressVal TO DisplayValue(dBlock.sbStress);
      ASK nameBox TO ReceiveFocus;
      IF compileType = "student"
         ASK advTab TO Deactivate;
         ASK costTab TO Deactivate;
         {dependency stuff}
         ASK defineDepText TO Deactivate; 
         ASK opText TO Deactivate;         
         ASK opValBox TO Deactivate;        
         ASK idleValBox TO Deactivate;         
         ASK idleText TO Deactivate;
         ASK PMValBox TO Deactivate;
         ASK PMText TO Deactivate;
         ASK failValBox TO Deactivate;         
         ASK failedText TO Deactivate;
         ASK lifeExBox TO Deactivate;
         ASK blockRadBox TO Deactivate; 
         ASK PMText2 TO Deactivate;
         ASK defaultDepText TO Deactivate;
         {block start state}
         ASK startStateText TO Deactivate;
         ASK startRadBox TO Deactivate;
         ASK failedRadBox TO Deactivate;
         ASK expiredVal TO Deactivate;
         ASK timeLabel6 TO Deactivate;
      END IF;
      Draw;
     
     
{########################## ASK FOR USER INPUT ############################}
      NEW(strsArray, 1..6);
      NEW(intsArray, 1..17);
      NEW(boolsArray,1..18);
      NEW(realsArray,1..28);
      NEW(fVals,     1..3);
      NEW(rVals,     1..3);
      DISPOSE(text);
      NEW(text, 1..30);
      boolsArray[1] := dBlock.infiniteSpares; 
      boolsArray[2] := dBlock.routineSpareOrdering; 
      boolsArray[3] := dBlock.stockLevelOrdering; 
      boolsArray[4] := dBlock.emerSpareOrdering; 
      boolsArray[10] := FALSE;
      boolsArray[18] := dBlock.defDepStateIdle;
      intsArray[5] := dBlock.initStock;
      intsArray[6] := dBlock.newSpares;
      intsArray[7] := dBlock.SLOOrderLevel;
      intsArray[8] := dBlock.SLONewSpares;
      intsArray[9] := dBlock.DependencyNum;
      intsArray[10] := dBlock.GDType;
      intsArray[11] := dBlock.failStream;
      intsArray[12] := dBlock.repairStream;
      intsArray[13] := dBlock.simStartType;
      intsArray[14] := dBlock.numRes1;      
      realsArray[1] := dBlock.arrivalRate;
      realsArray[2] := dBlock.SLOTime;
      realsArray[3] := dBlock.emerTime;
      realsArray[17] := dBlock.GDRate;
      realsArray[18] := dBlock.GDLimit;
      realsArray[19] := dBlock.amountExhausted;
      realsArray[24] := dBlock.DepNothingPerc;
      realsArray[25] := dBlock.DepIdlePerc;
      realsArray[26] := dBlock.DepPMPerc;
      realsArray[27] := dBlock.DepFailPerc;
      realsArray[28] := dBlock.DepPMThreshold; 
      strsArray[2] := dBlock.poolName;      
      strsArray[3] := dBlock.res1Name;
      strsArray[4] := dBlock.pmTrig;
      strsArray[5] := dBlock.comment;
      strsArray[6] := dBlock.depType;
      IF dBlock.failDistro = 16
         NEW(fEmpArray, 1..HIGH(dBlock.failVals));
         fEmpArray := CLONE(dBlock.failVals);
      END IF;
      IF dBlock.repairDistro = 16
         NEW(rEmpArray, 1..HIGH(dBlock.repairVals));
         rEmpArray := CLONE(dBlock.repairVals);
      END IF;
      oldFailNum := dBlock.failDistro;
      oldRepNum := dBlock.repairDistro;
      REPEAT
         errors := 0;
         cancel    := FALSE;
         validData := TRUE;
         button := AcceptInput();
         IF button.ReferenceName = "CancelButton"   {User clicked cancel}
            cancel    := TRUE;
            validData := TRUE;
            fFlag     := TRUE;
            rFlag     := TRUE;
         ELSE                                       {User clicked OK}
            nextString := nameBox.Text();
            IF ((nextString = "") OR (POSITION(nextString, " ") <> 0))
               INC(errors);
               text[errors] := "'Block Name' field can't be blank or have blank spaces!     ";
               ASK nameBox TO SetText(dBlock.name);
               SetLabel("Block Properties - "+dBlock.name + " - " + INTTOSTR(dBlock.Id));
               validData := FALSE;
            ELSIF STRLEN(nextString) > 20
               INC(errors);
               text[errors] := "'Block Name' must be no greater than 20 characters!     ";
               ASK nameBox TO SetText(SUBSTR(1,20,nextString));
               SetLabel("Block Properties - "+SUBSTR(1,20,nextString) + " - " + INTTOSTR(dBlock.Id));
               validData := FALSE;
            ELSE
               strsArray[1] := nextString;
            END IF;
            strsArray[5] := commentBox.Text();
   {Get failure distribution and parameters}         
            fDistro := failCombo.Text();
            ConvertToInteger(fDistro, distroNum, numDistroParm);
            IF distroNum = 16            {If failure is empirical}
               fFlag := TRUE;
               intsArray[1] := 16;
               {intsArray[2] := HIGH(fEmpArray);}
               {numDistroParm := HIGH(fEmpArray);}
               fVals := CLONE(fEmpArray);
            ELSE                         {If failure is not empirical}
               intsArray[1] := distroNum;
               {intsArray[2] := numDistroParm;}
               FOR i := 1 TO numDistroParm
                  fVals[i] := failVals[i].Value();
               END FOR;
               CheckValidInput(distroNum, numDistroParm, fVals,"Failure " ,fFlag, text);
               IF ((lambdaMode) AND (distroNum=4))
                  IF (failVals[1].Value() <> startfVal1)
                     fVals[1] := (1.0 / failVals[1].Value());
                  ELSE
                     fVals[1] :=dBlock.failVals[1]; 
                  END IF;
               ELSIF ((muSigmaMode) AND (distroNum=7))
                  IF ((failVals[1].Value() <> startfVal1) 
                          OR (failVals[2].Value() <> startfVal2))
                     mu:=failVals[1].Value();
                     sigma2:=POWER(failVals[2].Value(),2.0);
                     fVals[1] := EXP(mu+sigma2/2.0);
                     fVals[2] := SQRT(EXP(2.0*mu+sigma2)*(EXP(sigma2)-1.));
                  ELSE
                     fVals[1] :=dBlock.failVals[1];
                     fVals[2] :=dBlock.failVals[2];
                  END IF;
               END IF;
            END IF;
   {Get repair distribution and parameters}         
            rDistro := repairCombo.Text();
            ConvertToInteger(rDistro, distroNum, numDistroParm);
            IF distroNum = 16
               rFlag := TRUE;
               intsArray[3] := 16;
               {intsArray[4] := HIGH(rEmpArray);}
               {numDistroParm := HIGH(rEmpArray);}
               rVals := CLONE(rEmpArray);
            ELSE
               intsArray[3] := distroNum;
               {intsArray[4] := numDistroParm;}
               FOR i := 1 TO numDistroParm
                  rVals[i] := repairVals[i].Value();
               END FOR;
               CheckValidInput(distroNum, numDistroParm, rVals, "Repair ", rFlag, text);
               IF ((lambdaMode) AND (distroNum=4))
                  IF (repairVals[1].Value() <> startrVal1)
                     rVals[1] := (1.0 / repairVals[1].Value());
                  ELSE
                     rVals[1] :=dBlock.repairVals[1]; 
                  END IF;
               ELSIF ((muSigmaMode) AND (distroNum=7))
                  IF ((repairVals[1].Value() <> startrVal1) 
                             OR (repairVals[2].Value() <> startrVal2))
                     mu:=repairVals[1].Value();
                     sigma2:=POWER(repairVals[2].Value(),2.0);
                     rVals[1] := EXP(mu+sigma2/2.0);
                     rVals[2] := SQRT(EXP(2.0*mu+sigma2)*(EXP(sigma2)-1.));
                  ELSE
                     rVals[1] :=dBlock.repairVals[1]; 
                     rVals[2] :=dBlock.repairVals[2];
                  END IF;
               END IF;
            END IF;
{Get replacement type}            
            CASE maintRadBox.SelectedButton.Id
               WHEN 2011:                {Infinite spares}
                  boolsArray[1] := TRUE;
                  sparing       := Infinite;
                  strsArray[2]  := "unnamed";
               WHEN 2012:                {Spare Pool}
                  boolsArray[1] := FALSE;
                  sparing       := SparePool;
                  strsArray[2]  := poolCombo.Text();
               WHEN 2013: {Custom}
                  boolsArray[1] := FALSE;
                  sparing       := Custom;
                  strsArray[2]  := "unnamed";
{Get initial Stock}
                  nextReal := initStockVal.Value();
                  IF (nextReal < 0.) OR (nextReal > 999999999.)
                     INC(errors);
                     text[errors] := "Initial stock must be between 0 and 999,999,999!     ";
                     ASK initStockVal TO SetValue(FLOAT(dBlock.initStock));
                     validData  := FALSE;
                  ELSE            
                     intsArray[5] := ROUND(nextReal);
                  END IF;
{Get new spares and arrival rate}                     
                  boolsArray[2] := newChk.Checked;
                  IF newChk.Checked
                     nextReal := newSparesVal.Value();
                     IF (nextReal <= 0.) OR (nextReal > 99999.)
                        INC(errors);
                        text[errors] := "New spares value must be between 1 and 99,999!     ";
                        ASK newSparesVal TO SetValue(FLOAT(dBlock.newSpares));
                        validData  := FALSE;
                     ELSE
                        intsArray[6] := ROUND(nextReal);
                     END IF;
                     nextReal := arrRateVal.Value();
                     IF (nextReal <= 0.0) OR (nextReal > 999999999.999999);
                        INC(errors);  
                        text[errors] := "New spares arrival rate value must be between 0.000001 and 999,999,999.999999!     ";
                        ASK arrRateVal TO SetValue(dBlock.arrivalRate);
                        validData  := FALSE;
                     ELSE
                        realsArray[1] := nextReal;
                     END IF;
                  ELSE                 
                     {intsArray[6]  := ROUND(newSparesVal.Value());
                     realsArray[1] := arrRateVal.Value();}
                  END IF;
{Get stock level info}
                  boolsArray[3] := stockChk.Checked;
                  IF stockChk.Checked
                     nextReal := stockVal.Value();
                     IF (nextReal < 0.) OR (nextReal > 99999.)
                        INC(errors);
                        text[errors] := "Stock level must be between 0 and 99,999!     ";
                        ASK stockVal TO SetValue(FLOAT(dBlock.SLOOrderLevel));
                        validData  := FALSE;
                     ELSE
                        intsArray[7] := ROUND(nextReal);
                     END IF;
                     nextReal := stockAmount.Value();
                     IF (nextReal <= 0.) OR (nextReal > 99999.)
                        INC(errors);
                        text[errors] := "Stock order amount must be between 1 and 99,999!     ";
                        ASK stockAmount TO SetValue(FLOAT(dBlock.SLONewSpares));
                        validData  := FALSE;
                     ELSE
                        intsArray[8] := ROUND(nextReal);
                     END IF;
                     nextReal := stockRate.Value();
                     IF (nextReal <= 0.0) OR (nextReal > 999999999.999999);
                        INC(errors);  
                        text[errors] := "Stock arrival rate value must be between 0.000001 and 999,999,999.999999!     ";
                        ASK stockRate TO SetValue(dBlock.SLOTime);
                        validData  := FALSE;
                     ELSE
                        realsArray[2] := nextReal;
                     END IF;
                  ELSE                 
                     {intsArray[7]  := 0;
                     intsArray[8]  := 0;
                     realsArray[2] := 0.0;}
                  END IF;
{Get emergency spare delay}               
                  boolsArray[4] := emerChk.Checked;
                  IF emerChk.Checked
                     nextReal := emerVal.Value();
                     IF (nextReal <= 0.0) OR (nextReal > 999999999.999999)
                        INC(errors);
                        text[errors] := "Emergency spare delay value must between 0.000001 and 999,999,999.999999!     ";
                        ASK emerVal TO SetValue(dBlock.emerTime);
                        validData  := FALSE;
                     ELSE
                        realsArray[3] := nextReal;
                     END IF;
                  ELSE
                     {realsArray[3] := nextReal;}
                  END IF;
{Check for 'None' sparing type}                     
                  IF ((intsArray[5]=0)AND(intsArray[6]=0)AND(realsArray[3]=0.0))
                     sparing := None;
                  END IF;
               OTHERWISE
                  INC(errors);
                  text[errors] := "Unknown replacement type selected!     ";
                  validData  := FALSE;
            END CASE;
                        
{Get Preventive Maintenance stuff}
            boolsArray[11] := pmActivitiesChkBox.Checked;
            
            IF freqRadBox.SelectedButton.Id = 3031
               IF ((hourValBox.Value() < 0.000001) OR (hourValBox.Value() > 999999999.999999))
                  INC(errors);
                  text[errors] := "Frequency of PM must be between 0.000001 and 999,999,999.999999!";
                  ASK hourValBox TO SetValue(dBlock.pmFreq);
                  validData := FALSE;
               ELSE
                  boolsArray[16] := FALSE;
                  realsArray[21] := hourValBox.Value();
               END IF;
            ELSE
               boolsArray[16] := TRUE;
               strsArray[4] := trigComboBox.Text();
               realsArray[21] := hourValBox.Value(); {so doesn't come back as 0}
            END IF; 
    
            intsArray[16] := tempDurDist;

            IF ((staggerVal.Value() < 0.) OR (staggerVal.Value() > 999999999.999999))
               INC(errors);
               text[errors] := "PM stagger value must be between 0 and 999,999,999.999999!";
               ASK staggerVal TO SetValue(dBlock.pmStagger);
               validData := FALSE;
            ELSIF ((freqRadBox.SelectedButton.Id = 3031) AND (staggerVal.Value() >= hourValBox.Value()))
               INC(errors);
               text[errors] := "PM stagger value cannot be greater than or equal to PM frequency!     ";
               ASK staggerVal TO SetValue(dBlock.pmStagger);
               validData := FALSE;
            ELSE
               realsArray[20] := staggerVal.Value();
            END IF;
            
            boolsArray[12] := spareChkBox.Checked;
            boolsArray[13] := refreshChkBox.Checked;
            boolsArray[14] := misDefChkBox.Checked;
            boolsArray[15] := failChkBox.Checked;
            boolsArray[17] := resDefChkBox.Checked;
                
{Get cost info}
            IF (costVals[1].Value() < 0.) OR (costVals[1].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Cost to operate block must be between 0 and 999,999,999.99!     ";
               ASK costVals[1] TO SetValue(dBlock.operatingCost);
               validData := FALSE;
            ELSE
               realsArray[7]  := costVals[1].Value(); {Running}
            END IF;
            IF (costVals[2].Value() < 0.) OR (costVals[2].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Cost of this block being on standby must be between 0 and 999,999,999.99!     ";
               ASK costVals[2] TO SetValue(dBlock.standbyCost);
               validData := FALSE;
            ELSE
               realsArray[14] := costVals[2].Value(); {Standby}
            END IF;
            IF (costVals[3].Value() < 0.) OR (costVals[3].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Cost to repair block must be between 0 and 999,999,999.99!     ";
               ASK costVals[3] TO SetValue(dBlock.repairingCost);
               validData := FALSE;
            ELSE
               realsArray[8] := costVals[3].Value(); {Maintenance/Repair}
            END IF;
            IF (costVals[4].Value() < 0.) OR (costVals[4].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Fixed cost to repair block must be between 0 and 999,999,999.99!     ";
               ASK costVals[4] TO SetValue(dBlock.repFixedCost);
               validData := FALSE;
            ELSE
               realsArray[9] := costVals[4].Value(); {Repair fixed cost}
            END IF;
            IF (costVals[5].Value() < 0.) OR (costVals[5].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Preventive maintenance of this block must be between 0 and 999,999,999.99!     ";
               ASK costVals[5] TO SetValue(dBlock.pmCost);
               validData := FALSE;
            ELSE
               realsArray[22] := costVals[5].Value(); {PM cost}
            END IF;
            IF (costVals[6].Value() < 0.) OR (costVals[6].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Fixed preventive maintenance of this block must be between 0 and 999,999,999.99!     ";
               ASK costVals[6] TO SetValue(dBlock.pmFixedCost);
               validData := FALSE;
            ELSE
               realsArray[5] := costVals[6].Value(); {PM fixed cost}
            END IF;
            IF (costVals[7].Value() < 0.) OR (costVals[7].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Cost of this block being on hold must be between 0 and 999,999,999.99!     ";
               ASK costVals[7] TO SetValue(dBlock.repHoldCost);
               validData := FALSE;
            ELSE
               realsArray[13] := costVals[7].Value(); {Logistical holds}
            END IF;
            IF (costVals[8].Value() < 0.) OR (costVals[8].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Cost of this block being on PM hold must be between 0 and 999,999,999.99!     ";
               ASK costVals[8] TO SetValue(dBlock.pmHoldCost);
               validData := FALSE;
            ELSE
               realsArray[23] := costVals[8].Value(); {PM hold}
            END IF;
            IF (costVals[9].Value() < 0.) OR (costVals[9].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Cost of this block being idle must be between 0 and 999,999,999.99!     ";
               ASK costVals[9] TO SetValue(dBlock.idleCost);
               validData := FALSE;
            ELSE
               realsArray[12] := costVals[9].Value(); {Idle downtime}
            END IF;
            IF (costVals[10].Value() < 0.) OR (costVals[10].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Cost of this block being done must be between 0 and 999,999,999.99!     ";
               ASK costVals[10] TO SetValue(dBlock.doneCost);
               validData := FALSE;
            ELSE
               realsArray[15] := costVals[10].Value(); {Permanently done}
            END IF;
            IF (costVals[11].Value() < 0.) OR (costVals[11].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Cost of block must be between 0 and 999,999,999.99!     ";
               ASK costVals[11] TO SetValue(dBlock.initialCost);
               validData := FALSE;
            ELSE
               realsArray[6]  := costVals[11].Value(); {Initial acquisition}
            END IF;
            IF (costVals[12].Value() < 0.) OR (costVals[12].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Cost of providing a spare for this block must be between 0 and 999,999,999.99!     ";
               ASK costVals[12] TO SetValue(dBlock.spareCost);
               validData := FALSE;
            ELSE
               realsArray[10] := costVals[12].Value(); {Spare}
            END IF;
            IF (costVals[13].Value() < 0.) OR (costVals[13].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Emergency spare additional cost for this block must be between 0 and 999,999,999.99!     ";
               ASK costVals[13] TO SetValue(dBlock.emerShippingCost);
               validData := FALSE;
            ELSE
               realsArray[11] := costVals[13].Value(); {Emergency spare}
            END IF;
            IF (costVals[14].Value() < -999999999.99) OR (costVals[14].Value() > 999999999.99)
               INC(errors);
               text[errors] := "Fixed cost of this block being done must be between -999,999,999.99 and 999,999,999.99!     ";
               ASK costVals[14] TO SetValue(dBlock.doneFixedCost);
               validData := FALSE;
            ELSE
              realsArray[16] := costVals[14].Value(); {Rebate or disposal}
            END IF;
            boolsArray[5] := doneBool.Checked; {Assess rebate or disposal}
            
{Determine Dependency}         
            IF dependRadBox.SelectedButton.Id = 6011
               intsArray[9] := 0;
               boolsArray[6] := FALSE;
               strsArray[6] := "";
            ELSIF dependRadBox.SelectedButton.Id = 6012
               intsArray[9] := -1;
               boolsArray[6] := FALSE;
               strsArray[6] := "";
            ELSIF dependRadBox.SelectedButton.Id = 6013
               intsArray[9] := -2;
               boolsArray[6] := TRUE;
               strsArray[6] := "";
            ELSE
               i := POSITION(buddyCombo.Text, " -");
               intsArray[9] := STRTOINT(SUBSTR(i+2, i+11, buddyCombo.Text + "      "));
               IF (SUBSTR(i-1,i-1, buddyCombo.Text) = "r")
                  tempHier := ASK root Child("RBDHier", intsArray[9]);
                  strsArray[6] := "RBDHier";
                  intsArray[9] := tempHier.outID;
               ELSIF (SUBSTR(i-1,i-1, buddyCombo.Text) = "k")
                  strsArray[6] := "RBDBlock";
               ELSIF (SUBSTR(i-1,i-1, buddyCombo.Text) = "e")
                  strsArray[6] := "RBDNode";
               ELSIF (SUBSTR(i-1,i-1, buddyCombo.Text) = "t")
                  strsArray[6] := "RBDEvent";
               END IF;
               {REPEAT
                  INC(i);
                  buddyPos := SUBSTR(i,i, buddyCombo.Text);
               UNTIL buddyPos = "-";
               intsArray[9] := STRTOINT(SUBSTR(1,i-2,buddyCombo.Text));}
               boolsArray[6] := FALSE;
            END IF;
            {Error checking for dependent state percentages}
            IF ((opValBox.Value() < 0.) OR (opValBox.Value() > 100.))
               INC(errors);
               text[errors] := "Percent operating time must be between 0 and 100!     ";
               ASK opValBox TO SetValue(dBlock.DepNothingPerc); 
               validData := FALSE;
            ELSE
               realsArray[24] := opValBox.Value();
            END IF;
            IF ((idleValBox.Value() < 0.) OR (idleValBox.Value() > 100.))
               INC(errors);
               text[errors] := "Percent idle time must be between 0 and 100!     ";
               ASK idleValBox TO SetValue(dBlock.DepIdlePerc); 
               validData := FALSE;
            ELSE
               realsArray[25] := idleValBox.Value();
            END IF;
            IF ((PMValBox.Value() < 0.) OR (PMValBox.Value() > 100.))
               INC(errors);
               text[errors] := "Percent PM time must be between 0 and 100!     ";
               ASK PMValBox TO SetValue(dBlock.DepPMPerc); 
               validData := FALSE;
            ELSE
               realsArray[26] := PMValBox.Value();
            END IF;
            IF ((failValBox.Value() < 0.) OR (failValBox.Value() > 100.))
               INC(errors);
               text[errors] := "Percent failed time must be between 0 and 100!     ";
               ASK failValBox TO SetValue(dBlock.DepFailPerc); 
               validData := FALSE;
            ELSE
               realsArray[27] := failValBox.Value();
            END IF;
            IF ((lifeExBox.Value() < 0.) OR (lifeExBox.Value() > 100.))
               INC(errors);
               text[errors] := "Life exhausted must be between 0 and 100!     ";
               ASK failValBox TO SetValue(dBlock.DepPMThreshold); 
               validData := FALSE;
            ELSE
               realsArray[28] := lifeExBox.Value();
            END IF;
            IF blockRadBox.SelectedButton.Id = 6151
               boolsArray[18] := FALSE;  
            ELSIF blockRadBox.SelectedButton.Id = 6152
               boolsArray[18] := TRUE;  
            END IF;
            IF ((opValBox.Value() + idleValBox.Value() + PMValBox.Value() + failValBox.Value()) <> 100.) 
               INC(errors);
               text[errors] := "Total percentage time must add up to 100!     ";
               ASK opValBox TO SetValue(dBlock.DepNothingPerc); 
               ASK idleValBox TO SetValue(dBlock.DepIdlePerc); 
               ASK PMValBox TO SetValue(dBlock.DepPMPerc); 
               ASK failValBox TO SetValue(dBlock.DepFailPerc); 
               validData := FALSE;
            END IF;            
{Advanced stuff}
            boolsArray[7] := phaseChk.Checked;
            CASE failCombo.Text
               WHEN "Beta": 
                  tempMean := fVals[1]/(fVals[1]+fVals[2]);
               WHEN "Chi-square":
                  tempMean := fVals[1];
               WHEN "Binomial":
                  tempMean := fVals[1]*fVals[2];
               WHEN "Exponential":
                  tempMean := fVals[1]+fVals[2];
               WHEN "Erlang":
                  tempMean := fVals[1]*fVals[2];
               WHEN "Gamma":
                  tempMean := fVals[1]*fVals[2]+fVals[3];
               WHEN "Lognormal":
                  tempMean := fVals[1];
               WHEN "Normal":
                  tempMean := fVals[1];
               WHEN "Uniform Integer":
                  tempMean := (fVals[1]+fVals[2])/2.;
               WHEN "Uniform Real":
                  tempMean := (fVals[1]+fVals[2])/2.;
               WHEN "Pearson 5":
                  IF fVals[1] <= 1.
                     tempMean := -1.;
                  ELSE
                     tempMean := (fVals[2]/(fVals[1]-1.))+fVals[3];
                  END IF;
               WHEN "Pearson 6":
                  IF fVals[2] <= 1.
                     tempMean := -1.; 
                  ELSE
                     tempMean := ((fVals[3]*fVals[1])/(fVals[2]-1.));
                  END IF;
               WHEN "Poisson":
                  tempMean := fVals[1];
               WHEN "Triangular":
                  tempMean := (fVals[1]+fVals[2]+fVals[3])/3.;
               WHEN "Weibull":
                  IF fVals[1] < 0.05
                     tempMean := -1.;
                  ELSIF (fVals[1] = 1.) OR (fVals[1] > 10000000.)
                     tempMean := fVals[2] + fVals[3];
                  ELSE
                     x := (1.+(1./fVals[1]));
                     y := (1.000000000190015+(76.18009172947146/(x+1.))-(86.50532032941677/(x+2.))
                         +(24.01409824083091/(x+3.))-(1.231739572450155/(x+4.))+(0.001208650973866179/(x+5.))
                         -(0.000005395239384953/(x+6.)));
                     z := (POWER((x+5.5),(x+0.5))*EXP(-1.*(x+5.5))*SQRT(2.*pi)*y)/x;
                     tempMean := (fVals[2]*z)+fVals[3];
                  END IF;
               WHEN "Empirical":
                  {IF i:=1}
                  tempMean := fEmpMean;
                  {ELSE
                     tempMean := rEmpMean;
                  END IF;}
               WHEN "Point Estimate":
                  tempMean := -1.;
               WHEN "Exponential Power":
                  tempMean := fVals[3];
               WHEN "Extreme Value":
                  tempMean := fVals[2]+.57721*fVals[1];
               WHEN "Laplace":
                  tempMean := fVals[2];
               OTHERWISE
            END CASE;
            boolsArray[8] := degradeChk.Checked;
            IF degradeChk.Checked
               IF tempMean < 0.
                  INC(errors);
                  text[errors] := "Selected failure distribution/parameters cannot be used with block degrade!     ";
                  ASK degradeChk TO SetCheck(FALSE);
                  validData := FALSE;
               ELSE   
                  IF degradeRadBox.SelectedButton = linearButton
                     IF (degradeVal1.Value < -999999999.999999) OR (degradeVal1.Value > 999999999.999)
                        INC(errors);
                        text[errors] := "The slope must be between +/- 999,999,999.999999!     ";
                        ASK degradeVal1 TO SetValue(0.);
                        validData := FALSE;
                     ELSIF degradeVal1.Value < 0.
                        IF (degradeVal2.Value < 0.000002) OR (degradeVal2.Value > tempMean)
                           INC(errors);
                           text[errors] := "The limit must be between 0.000001 and "+REALTOSTR(tempMean)+"!     ";
                           ASK degradeVal2 TO SetValue(tempMean/2.);
                           validData := FALSE;
                        ELSE
                           intsArray[10] := 1;
                           realsArray[17] := degradeVal1.Value;
                           realsArray[18] := degradeVal2.Value;
                        END IF;
                     ELSE
                        IF (degradeVal2.Value < tempMean) OR (degradeVal2.Value >= 999999999.999999)
                           INC(errors);
                           text[errors] := "The limit must be between "+REALTOSTR(tempMean)+" and 999,999,999.999999!     ";
                           ASK degradeVal2 TO SetValue(tempMean*2.);
                           validData := FALSE;
                        ELSE
                           intsArray[10] := 1;
                           realsArray[17] := degradeVal1.Value;
                           realsArray[18] := degradeVal2.Value;
                        END IF;
                     END IF;
                  ELSIF degradeRadBox.SelectedButton = geoButton
                     IF degradeVal1.Value <= 0.000001 
                        INC(errors);
                        text[errors] := "The base must be greater than 0.000001!     ";
                        ASK degradeVal1 TO SetValue(1.);
                        validData := FALSE;
                     ELSIF degradeVal1.Value < 1.
                        IF (degradeVal2.Value < 0.000002) OR (degradeVal2.Value > tempMean)
                           INC(errors);
                           text[errors] := "The limit must be between 0.000001 and "+REALTOSTR(tempMean)+"!     ";
                           ASK degradeVal2 TO SetValue(tempMean/2.);
                           validData := FALSE;
                        ELSE
                           intsArray[10] := 2;
                           realsArray[17] := degradeVal1.Value;
                           realsArray[18] := degradeVal2.Value;
                        END IF;
                     ELSE
                        IF (degradeVal2.Value < tempMean) OR (degradeVal2.Value >= 999999999.999999)
                           INC(errors);
                           text[errors] := "The limit must be between "+REALTOSTR(tempMean)+" and 999,999,999.999999!     ";
                           ASK degradeVal2 TO SetValue(tempMean*2.);
                           validData := FALSE;
                        ELSE
                           intsArray[10] := 2;
                           realsArray[17] := degradeVal1.Value;
                           realsArray[18] := degradeVal2.Value;
                        END IF;
                     END IF;
                  ELSE      
                     IF (degradeVal1.Value < 0.) OR (degradeVal1.Value >= 999999999.999)
                        INC(errors);
                        text[errors] := "The rate must be between 0.000001 and 999,999,999.999999!     ";
                        ASK degradeVal1 TO SetValue(tempMean/2.);
                        validData := FALSE;
                     END IF;
                     IF (degradeVal2.Value < 0.) OR (degradeVal2.Value >= 999999999.999)
                        INC(errors);
                        text[errors] := "The limit must be between 0.000001 and 999,999,999.999999!     ";
                        ASK degradeVal2 TO SetValue(tempMean/2.);
                        validData := FALSE;
                     END IF;
                     IF validData
                        intsArray[10] := 3;
                        realsArray[17] := degradeVal1.Value;
                        realsArray[18] := degradeVal2.Value;
                     END IF;   
                  END IF;
               END IF;
            ELSE
               intsArray[10] := 0;
               realsArray[17] := 0.0;
               realsArray[18] := tempMean;
            END IF;
            IF streamRadBox.SelectedButton = sysStrmButton
               nextString := sysCombo.Text();
               IF nextString = "A"
                  intsArray[11] := 201;
                  intsArray[12] := 201;
               ELSIF nextString = "B"
                  intsArray[11] := 202;
                  intsArray[12] := 202;
               ELSIF nextString = "C"
                  intsArray[11] := 203;
                  intsArray[12] := 203;
               ELSIF nextString = "D"
                  intsArray[11] := 204;
                  intsArray[12] := 204;
               ELSIF nextString = "E"
                  intsArray[11] := 205;
                  intsArray[12] := 205;
               ELSIF nextString = "F"
                  intsArray[11] := 206;
                  intsArray[12] := 206;
               ELSIF nextString = "G"
                  intsArray[11] := 207;
                  intsArray[12] := 207;
               ELSIF nextString = "H"
                  intsArray[11] := 208;
                  intsArray[12] := 208;
               ELSIF nextString = "I"
                  intsArray[11] := 209;
                  intsArray[12] := 209;
               ELSIF nextString = "J"
                  intsArray[11] := 210;
                  intsArray[12] := 210;
               END IF;
            ELSE
               nextString := indFCombo.Text();  
               IF failAntiChkBox.Checked;
                  intsArray[11] := -1*STRTOINT(nextString);
               ELSE
                  intsArray[11] := STRTOINT(nextString);
               END IF;
               nextString := indRCombo.Text();
               IF repairAntiChkBox.Checked;           
                  intsArray[12] := -1*STRTOINT(nextString);
               ELSE
                  intsArray[12] := STRTOINT(nextString);
               END IF;
            END IF;
            IF startRadBox.SelectedButton = randButton
               intsArray[13] := 1;
               realsArray[19] := 0.;
            ELSIF startRadBox.SelectedButton = upButton
               IF failedRadBox.SelectedButton = randomButton
                  intsArray[13] := 2;
                  realsArray[19] := 0.;
               ELSE
                  IF (expiredVal.Value < 0.) OR (expiredVal.Value >= 999999999.999)
                     INC(errors);
                     text[errors] := "The amount of life exhausted must be between 0.0 and 999,999,999.999999!     ";
                     ASK degradeVal1 TO SetValue(100.);
                     validData := FALSE;
                  ELSE
                     intsArray[13] := 3;
                     realsArray[19] := expiredVal.Value;
                  END IF;
               END IF;
            ELSE
               IF failedRadBox.SelectedButton = randomButton
                  intsArray[13] := 4;
                  realsArray[19] := 0.;
               ELSIF failedRadBox.SelectedButton = repairButton
                  IF (expiredVal.Value < 0.) OR (expiredVal.Value > 99.999999)
                     INC(errors);
                     text[errors] := "The percent of repair exhausted must be between 0.0 and 99.999999!     ";
                     ASK degradeVal1 TO SetValue(50.);
                     validData := FALSE;
                  ELSE
                     intsArray[13] := 5;
                     realsArray[19] := expiredVal.Value;
                  END IF;
               ELSE
                  intsArray[13] := 6;
                  realsArray[19] := 0.;
               END IF;
            END IF;
            nextString := nameBox.Text()+"      ";                                         {BINS}
            IF  ((SUBSTR(1,4,nextString)="Bin-") OR (SUBSTR(1,6,nextString)="Timer-"))                                         {BINS}
                sbStressMin:=-999.999;                                                   {BINS}
            ELSE                                                                         {BINS}
                sbStressMin:=0.0;                                                        {BINS}
            END IF;                                                                      {BINS}
            IF ((SBStressVal.Value() < sbStressMin) OR (SBStressVal.Value() > 999.999))  {BINS}
               INC(errors);
               text[errors] := "The standby stress must be between 0 and 999.999!";
               ASK SBStressVal TO SetValue(dBlock.sbStress);
            ELSE
               realsArray[4] := SBStressVal.Value();
            END IF;
{Check for resources}    
            IF (res1Combo.Text() <> "None") AND (res1Combo.Text() <> "unnamed")
               nextString    := res1Combo.Text();
               nextInt       := ROUND(res1Req.Value());
               strsArray[3]  := nextString;
               intsArray[14]  := nextInt;
               intsArray[15] := 1;
               nextInt1 := ROUND(res1ReqPM.Value());
               intsArray[17] := nextInt1;
               boolsArray[9] := TRUE;
               FOREACH pool IN poolGroup
                  IF ((pool.sparingType = Resource) AND (pool.poolName = nextString))
                     numInPool := pool.initialSpares;
                     IF (numInPool < nextInt) 
                        INC(errors);
                        text[errors] := INTTOSTR(nextInt)+" "+pool.poolName+" were requested for repair from a pool of "
                            +INTTOSTR(numInPool)+".  Please select "+INTTOSTR(numInPool)+" or less!     ";
                        IF dBlock.res1Name = res1Combo.Text()
                           ASK res1Req TO SetValue(FLOAT(dBlock.numRes1));
                        ELSE
                           ASK res1Req TO SetValue(1.);
                        END IF;
                        validData := FALSE;
                     END IF;
                     IF (numInPool < nextInt1) 
                        INC(errors);
                        text[errors] := INTTOSTR(nextInt1)+" "+pool.poolName+" were requested for PM from a pool of "
                            +INTTOSTR(numInPool)+".  Please select "+INTTOSTR(numInPool)+" or less!     ";
                        IF dBlock.res1Name = res1Combo.Text()
                           ASK res1ReqPM TO SetValue(FLOAT(dBlock.numRes1PM));
                        ELSE
                           ASK res1ReqPM TO SetValue(1.);
                        END IF;
                        validData := FALSE;
                     END IF;

                  END IF;
               END FOREACH;
               IF (nextInt < 0) OR (nextInt > 999999999) 
                  INC(errors);
                  text[errors] := "Block can only use between 0 and " + INTTOSTR(numInPool) + " resources after a failure!     ";
                  ASK res1Req TO SetValue(FLOAT(dBlock.numRes1));
                  validData := FALSE;               
               END IF;
               IF (nextInt1 < 0) OR (nextInt1 > 999999999) 
                  INC(errors);
                  text[errors] := "Block can only use between 0 and " + INTTOSTR(numInPool) + " resources for preventive maintenance!     ";
                  ASK res1ReqPM TO SetValue(FLOAT(dBlock.numRes1PM));
                  validData := FALSE;               
               END IF;

            ELSE
               strsArray[3]  := "unnamed";
               intsArray[15] := 0;
               intsArray[14]  := 1;
               intsArray[17] := 1;
               boolsArray[9] := FALSE;
            END IF;
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
      UNTIL (validData AND fFlag AND rFlag);
      IF NOT cancel
         somethingChanged := TRUE;
         intsArray[2] := tempPreDist;
         intsArray[4] := tempPostDist;
         ASK dBlock TO SetBlockData(boolsArray, intsArray, realsArray,
                                    strsArray, fVals, rVals, preVals, postVals, tempDurParams, sparing);
      END IF;
      DISPOSE(failLabels);
      DISPOSE(failVals);
      DISPOSE(repairLabels);
      DISPOSE(repairVals);
      DISPOSE(fTags);
      DISPOSE(rTags);
      DISPOSE(strsArray);
      DISPOSE(intsArray);
      DISPOSE(boolsArray);
      DISPOSE(realsArray);
      DISPOSE(fVals);
      DISPOSE(rVals);
     { DISPOSE(eventList);
      DISPOSE(opList);}
      DISPOSE(buddyList);
      DISPOSE(buddyList2);
      DISPOSE(text);
      DISPOSE(fUnits);
      DISPOSE(rUnits);
   END METHOD; {ReceiveData}
   
   ASK METHOD LoadDefs (IN type : INTEGER);
   VAR
      fDistro,rDistro,distString, tempStr   : STRING;
      i,numParams                           : INTEGER;
      tags,failTags,repTags, funits, runits : strArray;
      fOff, rOff                            : realArray;
      logMean2,logVar,startfVal1,
      startfVal2,startrVal1,startrVal2      : REAL;
   BEGIN
      NEW(fOff, 1..3);
      NEW(rOff, 1..3);
      NEW(runits, 1..3);
      NEW(funits, 1..3);
      NEW(failTags, 1..3);
      NEW(repTags, 1..3);
      IF type > 0
         IF libArray[type,14] = "TRUE"
            ASK dependRadBox TO SetSelectedButton(sysButton);
         ELSE
            ASK dependRadBox TO SetSelectedButton(indepButton);
         END IF;
         IF libVersion=6
            tempPreDist := 19;
            numParams:=1;
            NEW(preVals, 1..numParams);
            preVals[1] := STRTOREAL(libArray[type,12]);
            tempPostDist := 19;
            numParams:=1;
            NEW(postVals, 1..numParams);
            postVals[1] := STRTOREAL(libArray[type,13]);
         ELSIF libVersion=7
            tempPreDist:=STRTOINT(libArray[type,12]);
            GetNumParams(tempPreDist,numParams);
            NEW(preVals, 1..numParams);
            FOR i:=1 TO numParams
               preVals[i]:=STRTOREAL(libArray[type,(14+i)]);
            END FOR;
            tempPostDist:=STRTOINT(libArray[type,13]);
            GetNumParams(tempPostDist,numParams);
            NEW(postVals, 1..numParams);
            FOR i:=1 TO numParams
               postVals[i]:=STRTOREAL(libArray[type,(17+i)]);
            END FOR;
         END IF;
         MakeDistString(tempPreDist, preVals,  distString);
         ASK preTable TO SetText(distString,1,0);
         MakeDistString(tempPostDist, postVals,  distString);
         ASK postTable TO SetText(distString,1,0);
         ASK nameBox TO SetText(libArray[type,1]);
         ASK nameBox TO Draw;    
         ConvertToString(STRTOINT(libArray[type,2]), fDistro);
         GetParameters(STRTOINT(libArray[type,2]), "failure", failTags, funits, fOff);
         ConvertToString(STRTOINT(libArray[type,7]), rDistro);
         GetParameters(STRTOINT(libArray[type,7]), "repair", repTags, runits, rOff);
         {Strip off block id from label for new label}
         tempStr := Label();
         tempStr := SUBSTR(POSITION(tempStr, " - ")+3, STRLEN(tempStr), tempStr);
         tempStr := SUBSTR(POSITION(tempStr, " - "), STRLEN(tempStr), tempStr);
         SetLabel("Block Properties - "+nameBox.Text() + tempStr);
      ELSE
         tempPreDist := 19;
         numParams:=1;
         NEW(preVals, 1..numParams);
         preVals[1] := 0.0;
         tempPostDist := 19;
         numParams:=1;
         NEW(postVals, 1..numParams);
         postVals[1] := 0.0;
         MakeDistString(tempPreDist, preVals,  distString);
         ASK preTable TO SetText(distString,1,0);
         MakeDistString(tempPostDist, postVals,  distString);
         ASK postTable TO SetText(distString,1,0);
         ASK dependRadBox TO SetSelectedButton(indepButton);
         ASK nameBox TO SetText("unnamed");
         ASK nameBox TO Draw;    
         ConvertToString(4, fDistro);
         GetParameters(4, "failure", failTags, funits, fOff);
         ConvertToString(7, rDistro);
         GetParameters(7, "repair", repTags, runits, rOff);
         SetLabel("Block Properties - "+nameBox.Text() + " - 0");
      END IF;
      ASK failCombo TO DisplayText(fDistro);
      FOR i := 1 TO 3
         ASK failVals[i] TO SetHidden(FALSE);
         ASK failUnits[i] TO SetHidden(FALSE);
         ASK failLabels[i] TO DisplayAt(fOff[i], (5.+FLOAT(i*2)));
         ASK failLabels[i] TO SetLabel(failTags[i]);
         ASK failUnits[i] TO SetLabel(funits[i]);
         IF failTags[i] = ""
            ASK failVals[i] TO SetHidden(TRUE);
            ASK failUnits[i] TO SetHidden(TRUE);
         END IF;
      END FOR;
      ASK repairCombo TO DisplayText(rDistro);
      FOR i := 1 TO 3
         ASK repairVals[i] TO SetHidden(FALSE);
         ASK repairUnits[i] TO SetHidden(FALSE);
         ASK repairLabels[i] TO DisplayAt(rOff[i]+44., (5.+FLOAT(i*2)));
         ASK repairLabels[i] TO SetLabel(repTags[i]);
         ASK repairUnits[i] TO SetLabel(runits[i]);
         IF repTags[i] = ""
            ASK repairVals[i] TO SetHidden(TRUE);
            ASK repairUnits[i] TO SetHidden(TRUE);
         END IF;
      END FOR;
      IF type = 0
         ASK failVals[1] TO DisplayValue(100.);
         ASK failVals[2] TO DisplayValue(0.);
         ASK repairVals[1] TO DisplayValue(10.);
         ASK repairVals[2] TO DisplayValue(2.);
         ASK failVals[1] TO Draw;
         ASK repairVals[1] TO Draw;
         ASK repairVals[2] TO Draw;
      ELSE
         FOR i := 1 TO 3
            IF failTags[i] <> ""
               ASK failVals[i] TO DisplayValue(STRTOREAL(libArray[type,2+i]));
            END IF;
            IF repTags[i] <> ""
               ASK repairVals[i] TO DisplayValue(STRTOREAL(libArray[type,7+i]));
            END IF;
            ASK failVals[i] TO Draw;
            ASK repairVals[i] TO Draw;
         END FOR;
      END IF;
      ASK commentBox TO SetText("Comment");
      poolInUse := "unnamed";
      ASK maintRadBox   TO SetSelectedButton(infRadButton);
      ASK newChk        TO DisplayCheck(FALSE);
      ASK stockChk      TO DisplayCheck(FALSE);
      ASK emerChk       TO DisplayCheck(FALSE);
      ASK arrRateVal    TO DisplayValue(720.);
      ASK newSparesVal  TO DisplayValue(1.);
      ASK stockVal      TO DisplayValue(0.);
      ASK stockAmount   TO DisplayValue(1.);
      ASK stockRate     TO DisplayValue(24.);
      ASK emerVal       TO DisplayValue(24.);
      ASK initStockVal  TO DisplayValue(100.);
      ASK poolCombo     TO SetHidden(TRUE);
      ASK initStockVal  TO SetHidden(TRUE);
      ASK initStock     TO SetHidden(TRUE);
      ASK newChk        TO SetHidden(TRUE);
      ASK newSparesVal  TO SetHidden(TRUE);
      ASK newSparesText TO SetHidden(TRUE);
      ASK arrRateVal    TO SetHidden(TRUE);
      ASK timeLabel1    TO SetHidden(TRUE);
      ASK emerChk       TO SetHidden(TRUE);
      ASK emerText      TO SetHidden(TRUE);
      ASK emerVal       TO SetHidden(TRUE);
      ASK timeLabel2    TO SetHidden(TRUE);
      ASK stockChk      TO SetHidden(TRUE);
      ASK stockTxt1     TO SetHidden(TRUE);
      ASK stockVal      TO SetHidden(TRUE);
      ASK stockTxt2     TO SetHidden(TRUE);
      ASK stockAmount   TO SetHidden(TRUE);
      ASK stockTxt3     TO SetHidden(TRUE);
      ASK stockRate     TO SetHidden(TRUE);
      ASK timeLabel4    TO SetHidden(TRUE);
      
      {Preventive Maintenance Defaults}
      ASK pmActivitiesChkBox TO DisplayCheck(FALSE); 
      ASK freqRadBox TO SetSelectedButton(hourRadButton);
      ASK hourValBox TO SetValue(100.);
      tempDurDist := 19;
      NEW(tempDurParams, 1..1);
      tempDurParams[1] := 1.;
      MakeDistString(tempDurDist, tempDurParams, distString);
      ASK durTable TO SetText(distString, 1, 0);
      ASK durTable TO SetSize(1,0);
      ASK durTable TO SetVisibleSize(23,1);    
      ASK staggerVal TO DisplayValue(0.);
      ASK spareChkBox TO DisplayCheck(TRUE);
      ASK refreshChkBox TO DisplayCheck(TRUE);
      ASK misDefChkBox TO DisplayCheck(FALSE);
      ASK failChkBox TO DisplayCheck(TRUE);
      ASK resDefChkBox TO DisplayCheck(TRUE);
      ASK freqRadBox TO Deactivate;
      ASK freqLabel TO Deactivate;
      ASK hourRadButton TO Deactivate;
      ASK trigRadButton TO Deactivate;
      ASK hourValBox TO Deactivate;
      ASK unitsLabel1 TO Deactivate;    
      ASK trigComboBox TO Deactivate;
      ASK firesLabel TO Deactivate;
      ASK durationLabel TO Deactivate;
      ASK durTable TO Deactivate;
      ASK durTable TO SetHidden(TRUE);
      ASK distLabel TO SetHidden(FALSE);
      ASK distLabel TO SetLabel(durTable.Text(1,0));
      ASK distLabel TO Deactivate;
      ASK unitsLabel2 TO Deactivate;
      ASK optionsLabel TO Deactivate;
      ASK spareChkBox TO Deactivate;
      ASK refreshChkBox TO Deactivate;
      ASK misDefChkBox TO Deactivate;
      ASK resDefChkBox TO Deactivate;
      ASK failChkBox TO Deactivate;
      ASK unitsLabel3 TO Deactivate;
      ASK staggerLabel TO Deactivate;
      ASK staggerVal TO Deactivate;
            
      {Resources Defaults}
      resInUse := "None";
      ASK res1Combo TO DisplayText("None");
      ASK res1Req   TO DisplayValue(1.);
      ASK res1ReqPM TO DisplayValue(1.);
      ASK res1Req   TO Deactivate;
      ASK res1ReqPM TO Deactivate;
      ASK res1Txt2  TO Deactivate;
      ASK res1Txt3  TO Deactivate;
      
      {Cost Defaults}
      ASK costVals[1] TO DisplayValue(1.);
      ASK costVals[2] TO DisplayValue(1.);
      ASK costVals[3] TO DisplayValue(1.);
      ASK costVals[4] TO DisplayValue(1.);
      ASK costVals[5] TO DisplayValue(1.);
      ASK costVals[6] TO DisplayValue(1.);
      ASK costVals[7] TO DisplayValue(1.);
      ASK costVals[8] TO DisplayValue(1.);
      ASK costVals[9] TO DisplayValue(1.);
      ASK costVals[10] TO DisplayValue(1.);
      ASK costVals[11] TO DisplayValue(1.);
      ASK costVals[12] TO DisplayValue(1.);
      ASK costVals[13] TO DisplayValue(1.);  
      ASK costVals[14] TO DisplayValue(1.);
      ASK costVals[12] TO Activate;
      ASK costVals[13] TO Activate;
      
      ASK doneBool TO DisplayCheck(FALSE);
      ASK buddyCombo TO SetHidden(TRUE);
      ASK phaseChk TO DisplayCheck(FALSE);
      ASK degradeChk TO DisplayCheck(FALSE);
      ASK degradeRadBox TO Deactivate;
      ASK degradeTxt1 TO SetHidden(TRUE);
      ASK degradeVal1 TO SetHidden(TRUE);
      ASK degradeTxt2 TO SetHidden(TRUE);
      ASK degradeVal2 TO SetHidden(TRUE);
      ASK degradeTxt3 TO SetHidden(TRUE);
      ASK streamRadBox TO SetSelectedButton(sysStrmButton);
      ASK sysCombo TO SetText("C");
      ASK sysCombo TO Activate;
      ASK indFCombo TO Deactivate;
      ASK failText TO Deactivate;
      ASK failAntiChkBox TO Deactivate;
      ASK indRCombo TO Deactivate;
      ASK repairText TO Deactivate;    
      ASK repairAntiChkBox TO Deactivate;
      ASK startRadBox TO SetSelectedButton(upButton);
      ASK failedRadBox TO SetHidden(FALSE);
      ASK preButton TO SetHidden(TRUE);
      ASK repairButton TO SetLabel("Some life exhausted:");
      ASK timeLabel6 TO SetLabel(systemUnits);
      ASK expiredVal TO SetHidden(FALSE);
      ASK expiredVal TO DisplayValue(0.);
      ASK SBStressVal TO DisplayValue(0.);
      DISPOSE(failTags);
      DISPOSE(repTags);
      DISPOSE(fOff);
      DISPOSE(rOff);
      DISPOSE(runits);
      DISPOSE(funits);
      Draw;
   END METHOD; {Load Defs}
   
   
   ASK METHOD BeSelected;
   VAR
      tabClicked                        : GraphicVObj;
      fDistro,rDistro,distroStr,
      tempDistro, tempRes, tempPool,
      tempStr                           : STRING;
      i,distroNum,numParams,j,
      tempDistroNum: INTEGER;
      tags,failTags,repTags, units      : strArray;
      resThere,poolMissing              : BOOLEAN;
      logMean2,logVar,startfVal1,
      startfVal2,startrVal1,startrVal2,
      tempMean, tempSTDev, x, y, z, sum : REAL;
      tempVal                           : realArray; {ARRAY INTEGER OF REAL;}
      pool                              : SparePoolObj;
      fOff, rOff                        : realArray;
      sparePoolsBox                     : SparePoolsBoxObj;
      resPoolsBox                       : ResPoolsBoxObj;
      trigBox                           : TrigBoxObj;
    tempBlock   : RBDBlockObj;
   
   BEGIN  
      NEW(fOff, 1..3);
      NEW(rOff, 1..3);
      NEW(units, 1..3);
      tabClicked := LastPicked;
      CASE tabClicked.Id
         WHEN 900:{Distro tab}
            CallHelp(21);
         WHEN 100:{Distro tab}
            lastClicked := ASK tabClicked LastPicked;
            CASE lastClicked.Id
               WHEN 110:   {Update Button}
                  IF LOWER(nameBox.Text()) = "dirtydish"
                     doorOpen := TRUE;
                  END IF;
                  NEW(tempVal, 1..3);
                  {Update block name: Strip off block id from label for new label}
                  tempStr := Label();
                  tempStr := SUBSTR(POSITION(tempStr, " - ")+3, STRLEN(tempStr), tempStr);
                  tempStr := SUBSTR(POSITION(tempStr, " - "), STRLEN(tempStr), tempStr);
                  SetLabel("Block Properties - "+nameBox.Text() + tempStr);
                  
                  {Compute Fail mean and standard deviation}
                  tempDistro := failCombo.Text();
                  ConvertToInteger(tempDistro, tempDistroNum, numParams);
                  tempVal[1] := failVals[1].Value();
                  tempVal[2] := failVals[2].Value();
                  tempVal[3] := failVals[3].Value();
                  IF ((lambdaMode) AND (tempDistro="Exponential"))
                     tempVal[1] := (1.0 / failVals[1].Value());
                  ELSIF ((muSigmaMode) AND (tempDistro="Lognormal"))
                     tempVal[1] := EXP(failVals[1].Value()+POWER(failVals[2].Value(),2.0)/2.0);
                     tempVal[2] := SQRT(EXP(2.0*failVals[1].Value()+POWER(failVals[2].Value(),2.0))*(EXP(POWER(failVals[2].Value(),2.0))-1.));
                  END IF;
                 
                  IF (tempDistro = "Empirical")
                     ComputeStats(tempDistroNum, fEmpArray, tempMean, tempSTDev);
                  ELSE
                     ComputeStats(tempDistroNum, tempVal, tempMean, tempSTDev);
                  END IF;
                  
                  IF tempMean = 12345.6789
                    ASK failMean TO SetLabel("Undefined");
                  ELSIF tempMean = 12345.6788
                    ASK failMean TO SetLabel("N/A");
                  ELSE
                    ASK failMean TO SetLabel(REALTOSTR(tempMean));
                  END IF;
                  
                  IF tempSTDev = 12345.6789      
                    ASK failSTDev TO SetLabel("Undefined");
                  ELSIF tempSTDev = 12345.6788
                    ASK failSTDev TO SetLabel("N/A");
                  ELSE
                    ASK failSTDev TO SetLabel(REALTOSTR(tempSTDev));
                  END IF;

                  {Compute Repair mean and standard deviation}
                  tempDistro := repairCombo.Text();
                  ConvertToInteger(tempDistro, tempDistroNum, numParams);
                  tempVal[1] := repairVals[1].Value();
                  tempVal[2] := repairVals[2].Value();
                  tempVal[3] := repairVals[3].Value();
                  IF ((lambdaMode) AND (tempDistro="Exponential"))
                    tempVal[1] := (1.0 / repairVals[1].Value());
                  ELSIF ((muSigmaMode) AND (tempDistro="Lognormal"))
                    tempVal[1] := EXP(repairVals[1].Value()+POWER(repairVals[2].Value(),2.0)/2.0);
                    tempVal[2] := SQRT(EXP(2.0*repairVals[1].Value()+POWER(repairVals[2].Value(),2.0))*(EXP(POWER(repairVals[2].Value(),2.0))-1.));
                  END IF;    

                  IF (tempDistro = "Empirical")
                     ComputeStats(tempDistroNum, rEmpArray, tempMean, tempSTDev);
                  ELSE
                     ComputeStats(tempDistroNum, tempVal, tempMean, tempSTDev);
                  END IF;

                  IF tempMean = 12345.6789
                    ASK repairMean TO SetLabel("Undefined");
                  ELSIF tempMean = 12345.6788
                    ASK repairMean TO SetLabel("N/A");
                  ELSE
                    ASK repairMean TO SetLabel(REALTOSTR(tempMean));
                  END IF;
                     
                  IF tempSTDev = 12345.6789      
                    ASK repairSTDev TO SetLabel("Undefined");
                  ELSIF tempSTDev = 12345.6788
                    ASK repairSTDev TO SetLabel("N/A");
                  ELSE
                    ASK repairSTDev TO SetLabel(REALTOSTR(tempSTDev));
                  END IF;

                  DISPOSE(tempVal);
                  Update;
               WHEN 104:{Failure distribution combo box}
                  distroStr := failCombo.Text();
                  IF distroStr <> ""
                     NEW(tags, 1..3);
                     ConvertToInteger(distroStr, distroNum, numParams);
                     GetParameters(distroNum, "failure", tags, units, fOff);
                     FOR i := 1 TO 3
                        ASK failLabels[i] TO DisplayAt(fOff[i], (5.+FLOAT(i*2)));
                        ASK failLabels[i] TO SetLabel(tags[i]);
                        ASK failUnits[i] TO SetLabel(units[i]);
                     END FOR;
                     Update;
                     IF distroNum <> 16
                        FOR i := 1 TO numParams
                           ASK failVals[i] TO SetHidden(FALSE);
                           ASK failUnits[i] TO SetHidden(FALSE);
                           IF (failLabels[i].Label = "Location") AND (failCombo.Text <> "Exponential")
                               AND (failCombo.Text <> "Extreme Value") AND (failCombo.Text <> "Laplace")
                              ASK failVals[i] TO DisplayValue(0.0);
                           ELSIF ((failLabels[i].Label = "Probability") OR (failLabels[i].Label = "P Success"))
                              ASK failVals[i] TO DisplayValue(0.8);
                           ELSIF (failCombo.Text = "Lognormal") AND (i=2)
                              IF muSigmaMode
                                 logMean2 := POWER(10., 2.);
                                 logVar := POWER(2., 2.);
                                 ASK failVals[1] TO DisplayValue(LN(logMean2/SQRT(logVar + logMean2)));        
                                 startfVal1:=failVals[1].Value();           
                                 ASK failVals[2] TO DisplayValue(SQRT(LN((logVar + logMean2)/logMean2)));        
                                 startfVal2:=failVals[2].Value();           
                              ELSE
                                 ASK failVals[1] TO DisplayValue(10.);
                                 ASK failVals[2] TO DisplayValue(2.);
                              END IF;
                           ELSIF (failCombo.Text = "Exponential") AND (i=2)
                              IF lambdaMode
                                 ASK failVals[1] TO DisplayValue(1.0 / 100.);
                                 startfVal1:=failVals[1].Value();
                              ELSE
                                 ASK failVals[1] TO DisplayValue(100.);
                              END IF;
                              ASK failVals[2] TO DisplayValue(0.);
                           ELSIF (failCombo.Text = "Triangular") AND (i>1)
                              IF i=2
                                 ASK failVals[2] TO DisplayValue(1.5);
                              ELSE
                                 ASK failVals[3] TO DisplayValue(2.0);
                              END IF;
                           ELSIF (failCombo.Text = "Pearson 5")
                              ASK failVals[1] TO DisplayValue(4.);
                              ASK failVals[2] TO DisplayValue(1.);
                           ELSIF (failCombo.Text = "Pearson 6")
                              ASK failVals[1] TO DisplayValue(4.);
                              ASK failVals[2] TO DisplayValue(4.);
                              ASK failVals[3] TO DisplayValue(1.);
                           ELSIF (failCombo.Text = "Normal")
                              ASK failVals[1] TO DisplayValue(100.);
                              ASK failVals[2] TO DisplayValue(10.);
                           ELSIF (failCombo.Text = "Exponential Power")
                              {defaults here}
                           ELSIF (failCombo.Text = "Extreme Value")
                              ASK failVals[1] TO DisplayValue(10.);
                              ASK failVals[2] TO DisplayValue(95.);
                           ELSIF (failCombo.Text = "Laplace")
                              ASK failVals[1] TO DisplayValue(10.);
                              ASK failVals[2] TO DisplayValue(100.);
                           ELSE
                              ASK failVals[i] TO DisplayValue(1.0);
                           END IF;
                        END FOR;
                        FOR i := (numParams+1) TO 3
                          ASK failVals[i] TO DisplayValue(0.0);
                          ASK failVals[i] TO SetHidden(TRUE);
                          ASK failUnits[i] TO SetHidden(TRUE);
                        END FOR;
                     ELSE
                        FOR i := 1 TO 3
                           ASK failVals[i] TO DisplayValue(0.0);
                           ASK failVals[i] TO SetHidden(TRUE);
                           ASK failUnits[i] TO SetHidden(TRUE);
                        END FOR;
                        failType := TRUE;
                        empFlag := FALSE;
                        GetEmpirical();
                        IF empFlag AND (numDataPoints = 0)
                           ConvertToString(4, fDistro);
                           GetParameters(4, "failure", tags, units, fOff);
                           ASK failCombo TO DisplayText(fDistro);
                           ASK failLabels[1] TO SetLabel(tags[1]);
                           ASK failLabels[1] TO DisplayAt(fOff[1], 7.);
                           ASK failUnits[1] TO SetLabel(units[1]);
                           ASK failLabels[2] TO SetLabel(tags[2]);
                           ASK failLabels[2] TO DisplayAt(fOff[2], 9.);
                           ASK failUnits[2] TO SetLabel(units[2]);
                           ASK failVals[1] TO SetHidden(FALSE);
                           ASK failUnits[1] TO SetHidden(FALSE);
                           ASK failVals[1] TO DisplayValue(100.0);
                           ASK failVals[2] TO SetHidden(FALSE);
                           ASK failUnits[2] TO SetHidden(FALSE);
                           ASK failVals[2] TO DisplayValue(0.0);
                           ASK failVals[3] TO SetHidden(TRUE);
                           ASK failUnits[3] TO SetHidden(TRUE);
                           oldFailNum := 1;
                        END IF;
                     END IF;
                     Draw;
                     DISPOSE(tags);
                  END IF;
               WHEN 105:{Repair distribution combo box}
                  distroStr := repairCombo.Text();
                  IF distroStr <> ""
                     NEW(tags, 1..3);
                     ConvertToInteger(distroStr, distroNum, numParams);
                     GetParameters(distroNum, "repair", tags, units, rOff);
                     FOR i := 1 TO 3
                        ASK repairLabels[i] TO DisplayAt(rOff[i]+44., (5.+FLOAT(i*2)));
                        ASK repairLabels[i] TO SetLabel(tags[i]);
                        ASK repairUnits[i] TO SetLabel(units[i]);
                     END FOR;
                     Update;
                     IF ((distroNum = 16) OR (distroNum = 18))
                        FOR i := 1 TO 3
                           ASK repairVals[i] TO DisplayValue(0.0);
                           ASK repairVals[i] TO SetHidden(TRUE);
                           ASK repairUnits[i] TO SetHidden(TRUE);
                        END FOR;
                        IF distroNum = 16
                           failType := FALSE;
                           empFlag := FALSE;
                           GetEmpirical();
                           IF empFlag AND (numDataPoints = 0)
                              ConvertToString(7, rDistro);
                              GetParameters(7, "repair", tags, units, rOff);
                              ASK repairCombo TO DisplayText(rDistro);
                              ASK repairLabels[1] TO SetLabel(tags[1]);
                              ASK repairLabels[1] TO DisplayAt(rOff[1]+44., 7.);
                              ASK repairUnits[1] TO SetLabel(units[1]);
                              ASK repairLabels[2] TO SetLabel(tags[2]);
                              ASK repairLabels[2] TO DisplayAt(rOff[2]+44., 9.);
                              ASK repairUnits[2] TO SetLabel(units[2]);
                              ASK repairVals[1] TO SetHidden(FALSE);
                              ASK repairUnits[1] TO SetHidden(FALSE);
                              ASK repairVals[1] TO DisplayValue(10.0);
                              ASK repairUnits[2] TO SetHidden(FALSE);
                              ASK repairVals[2] TO SetHidden(FALSE);
                              ASK repairVals[2] TO DisplayValue(2.0);
                              ASK repairVals[3] TO SetHidden(TRUE);
                              ASK repairUnits[3] TO SetHidden(TRUE);
                              oldRepNum := 1;
                           END IF;
                        END IF;
                     ELSE
                        FOR i := 1 TO numParams
                           ASK repairVals[i] TO SetHidden(FALSE);
                           ASK repairUnits[i] TO SetHidden(FALSE);
                           IF (repairLabels[i].Label = "Location") AND (repairCombo.Text <> "Exponential")
                               AND (repairCombo.Text <> "Extreme Value") AND (repairCombo.Text <> "Laplace")
                              ASK repairVals[i] TO DisplayValue(0.0);
                           ELSIF repairLabels[i].Label = "Probability"
                              ASK repairVals[i] TO DisplayValue(0.8);
                           ELSIF (repairCombo.Text = "Lognormal") AND (i=2)
                              IF muSigmaMode
                                 logMean2 := POWER(10., 2.);
                                 logVar := POWER(2., 2.);
                                 ASK repairVals[1] TO DisplayValue(LN(logMean2/SQRT(logVar + logMean2)));        
                                 startrVal1:=repairVals[1].Value();           
                                 ASK repairVals[2] TO DisplayValue(SQRT(LN((logVar + logMean2)/logMean2)));        
                                 startrVal2:=repairVals[2].Value();           
                              ELSE
                                 ASK repairVals[1] TO DisplayValue(10.);
                                 ASK repairVals[2] TO DisplayValue(2.);
                              END IF;
                           ELSIF (repairCombo.Text = "Exponential") AND (i=2)
                              IF lambdaMode
                                 ASK repairVals[1] TO DisplayValue(1.0 / 100.);
                                 startrVal1:=repairVals[1].Value();
                              ELSE
                                 ASK repairVals[1] TO DisplayValue(100.);
                              END IF;
                              ASK repairVals[2] TO DisplayValue(0.);
                           ELSIF (repairCombo.Text = "Triangular") AND (i>1)
                              IF i=2
                                 ASK repairVals[2] TO DisplayValue(1.5);
                              ELSE
                                 ASK repairVals[3] TO DisplayValue(2.0);
                              END IF;
                           ELSIF (repairCombo.Text = "Pearson 5")
                              ASK repairVals[1] TO DisplayValue(4.);
                              ASK repairVals[2] TO DisplayValue(1.);
                           ELSIF (repairCombo.Text = "Pearson 6")
                              ASK repairVals[1] TO DisplayValue(4.);
                              ASK repairVals[2] TO DisplayValue(4.);
                              ASK repairVals[3] TO DisplayValue(1.);
                           ELSIF (repairCombo.Text = "Normal")
                              ASK repairVals[1] TO DisplayValue(100.);
                              ASK repairVals[2] TO DisplayValue(10.);
                           ELSIF (repairCombo.Text = "Exponential Power")
                              {defaults here}
                           ELSIF (repairCombo.Text = "Extreme Value")
                              ASK repairVals[1] TO DisplayValue(10.);
                              ASK repairVals[2] TO DisplayValue(95.);
                           ELSIF (repairCombo.Text = "Laplace")
                              ASK repairVals[1] TO DisplayValue(10.);
                              ASK repairVals[2] TO DisplayValue(100.);
                           ELSE
                              ASK repairVals[i] TO DisplayValue(1.0);
                           END IF;
                        END FOR;
                        FOR i := (numParams+1) TO 3
                           ASK repairVals[i] TO DisplayValue(0.0);
                           ASK repairVals[i] TO SetHidden(TRUE);
                           ASK repairUnits[i] TO SetHidden(TRUE);
                        END FOR;     
                       
                     END IF;
                     Draw;
                     DISPOSE(tags);
                  END IF;
               WHEN 102:{Block type combo box}
                  IF lastType <> blockTypeCombo.Text();
                     lastType := blockTypeCombo.Text();
                     FOR i := 1 TO HIGH(nameArray)
                        IF nameArray[i] = lastType;
                           EXIT;
                        END IF;
                     END FOR;
                     LoadDefs(i);
                  END IF;
               OTHERWISE
            END CASE;
         WHEN 200:{Maintenance tab}
            lastClicked := ASK maintTab LastPicked;
            CASE lastClicked.Id
               WHEN 201:{Sparing type radio box}
                  IF maintRadBox.SelectedButton.Id = 2011 {Infinite}
                     ASK poolCombo     TO SetHidden(TRUE);
                     ASK initStockVal  TO SetHidden(TRUE);
                     ASK initStock     TO SetHidden(TRUE);
                     ASK newChk        TO SetHidden(TRUE);
                     ASK newSparesVal  TO SetHidden(TRUE);
                     ASK newSparesText TO SetHidden(TRUE);
                     ASK arrRateVal    TO SetHidden(TRUE);
                     ASK timeLabel1    TO SetHidden(TRUE);
                     ASK emerChk       TO SetHidden(TRUE);
                     ASK emerVal       TO SetHidden(TRUE);
                     ASK emerText      TO SetHidden(TRUE);
                     ASK timeLabel2    TO SetHidden(TRUE);
                     ASK stockChk      TO SetHidden(TRUE);
                     ASK stockTxt1     TO SetHidden(TRUE);
                     ASK stockVal      TO SetHidden(TRUE);
                     ASK stockTxt2     TO SetHidden(TRUE);
                     ASK stockAmount   TO SetHidden(TRUE);
                     ASK stockTxt3     TO SetHidden(TRUE);
                     ASK stockRate     TO SetHidden(TRUE);
                     ASK timeLabel4    TO SetHidden(TRUE);
                     ASK emerChk       TO SetHidden(TRUE);
                     ASK emerText      TO SetHidden(TRUE);
                     ASK emerVal       TO SetHidden(TRUE);
                     ASK timeLabel2    TO SetHidden(TRUE);
                     ASK costVals[12]        TO Activate;
                     ASK costVals[12]        TO DisplayValue(oldSpareCost);
                     ASK costVals[13]      TO Activate;
                     ASK costVals[13]      TO DisplayValue(oldEmerSpCost);
                  ELSIF maintRadBox.SelectedButton.Id = 2012 {Spare Pool}
                     ASK poolCombo     TO SetOptions(spareList);
                     ASK poolCombo     TO DisplayText(spareList[1]);
                     ASK poolCombo     TO SetHidden(FALSE);
                     ASK poolCombo     TO Activate;
                     ASK initStockVal  TO SetHidden(TRUE);
                     ASK initStock     TO SetHidden(TRUE);
                     ASK newChk        TO SetHidden(TRUE);
                     ASK newSparesVal  TO SetHidden(TRUE);
                     ASK newSparesText TO SetHidden(TRUE);
                     ASK arrRateVal    TO SetHidden(TRUE);
                     ASK timeLabel1    TO SetHidden(TRUE);
                     ASK stockChk      TO SetHidden(TRUE);
                     ASK stockTxt1     TO SetHidden(TRUE);
                     ASK stockVal      TO SetHidden(TRUE);
                     ASK stockTxt2     TO SetHidden(TRUE);
                     ASK stockAmount   TO SetHidden(TRUE);
                     ASK stockTxt3     TO SetHidden(TRUE);
                     ASK stockRate     TO SetHidden(TRUE);
                     ASK timeLabel4    TO SetHidden(TRUE);
                     ASK emerChk       TO SetHidden(TRUE);
                     ASK emerText      TO SetHidden(TRUE);
                     ASK emerVal       TO SetHidden(TRUE);
                     ASK timeLabel2    TO SetHidden(TRUE);
                     ASK costVals[12]        TO Deactivate;
                     ASK stockVal      TO SetHidden(TRUE);
                     ASK costVals[13]      TO Deactivate;
                     FOREACH pool IN poolGroup
                        IF pool.poolName = poolCombo.Text()
                           ASK costVals[12] TO DisplayValue(pool.spareCost);
                           ASK costVals[13] TO DisplayValue(pool.emerShippingCost);
                           EXIT;
                        END IF;
                     END FOREACH;
                  ELSE                                         {Custom}
                     ASK poolCombo    TO SetHidden(TRUE);
                     ASK initStockVal TO SetHidden(FALSE);
                     ASK newChk     TO SetHidden(FALSE);
                     ASK newSparesVal TO SetHidden(FALSE);
                     ASK arrRateVal   TO SetHidden(FALSE);
                     ASK timeLabel1   TO SetHidden(FALSE);
                     ASK emerChk    TO SetHidden(FALSE);
                     ASK emerVal      TO SetHidden(FALSE);
                     ASK timeLabel2   TO SetHidden(FALSE);
                     ASK initStock    TO SetHidden(FALSE);
                     ASK stockChk    TO SetHidden(FALSE);
                     ASK stockTxt1     TO SetHidden(FALSE);
                     ASK stockVal      TO SetHidden(FALSE);
                     ASK stockTxt2     TO SetHidden(FALSE);
                     ASK stockAmount   TO SetHidden(FALSE);
                     ASK stockTxt3     TO SetHidden(FALSE);
                     ASK stockRate     TO SetHidden(FALSE);
                     ASK timeLabel4    TO SetHidden(FALSE);
                     ASK newSparesText    TO SetHidden(FALSE);
                     ASK emerText   TO SetHidden(FALSE);
                     ASK costVals[12]        TO Activate;
                     ASK costVals[12]        TO DisplayValue(oldSpareCost);
                     ASK costVals[13]      TO Activate;
                     ASK costVals[13]      TO DisplayValue(oldEmerSpCost);
                     IF newChk.Checked
                        ASK newSparesVal TO Activate;
                        ASK arrRateVal   TO Activate;
                     ELSE
                        ASK newSparesVal TO Deactivate;
                        ASK arrRateVal   TO Deactivate;
                     END IF;
                     IF stockChk.Checked
                        ASK stockVal TO Activate;
                        ASK stockAmount TO Activate;
                        ASK stockRate TO Activate;
                     ELSE
                        ASK stockVal TO Deactivate;
                        ASK stockAmount TO Deactivate;
                        ASK stockRate TO Deactivate;
                     END IF;
                     IF emerChk.Checked
                        ASK emerVal TO Activate;
                     ELSE
                        ASK emerVal TO Deactivate;
                     END IF;
                  END IF;
                  Update;
               WHEN 202:
                  IF maintRadBox.SelectedButton.Id = 2012
                     FOREACH pool IN poolGroup
                        IF pool.poolName = poolCombo.Text()
                           ASK costVals[12] TO DisplayValue(pool.spareCost);
                           ASK spCostText TO Update;
                           EXIT;
                        END IF;
                     END FOREACH;
                  END IF;
               WHEN 205:{New spares check box}
                  IF newChk.Checked
                     ASK newSparesVal TO Activate;
                     ASK arrRateVal   TO Activate;  
                  ELSE
                     ASK newSparesVal TO Deactivate;
                     ASK arrRateVal   TO Deactivate;
                  END IF;
               WHEN 210: {Stock Level check box}
                  IF stockChk.Checked
                     ASK stockVal TO Activate;
                     ASK stockAmount TO Activate;
                     ASK stockRate TO Activate;
                  ELSE
                     ASK stockVal TO Deactivate;
                     ASK stockAmount TO Deactivate;
                     ASK stockRate TO Deactivate;
                  END IF;
               WHEN 218: {Emergency check box}
                  IF emerChk.Checked
                     ASK emerVal TO Activate;
                  ELSE
                     ASK emerVal TO Deactivate;
                  END IF;
               WHEN 224: {ALDT Distribution}
                  ShowDistBox(tempPreDist, preVals);
                  MakeDistString(tempPreDist, preVals, distribution);
                  ASK preTable TO SetText(distribution,1,0);
                  Draw;
               WHEN 228: {PostLDT Distribution}
                  ShowDistBox(tempPostDist, postVals);
                  MakeDistString(tempPostDist, postVals, distribution);
                  ASK postTable TO SetText(distribution,1,0);
                  Draw;
               WHEN 230: {Create Spare Pool}
                  tempPool := poolCombo.Text;
                  NEW(sparePoolsBox);
                  ASK sparePoolsBox TO LoadFromLibrary(dialogs, "SparePoolsBox");
                  ASK window TO AddGraphic(sparePoolsBox);
                  ASK sparePoolsBox TO ReceiveData;
                  DISPOSE(sparePoolsBox);
                  {IF ((totalSpares > 0) AND (pmActivitiesChkBox.Checked))
                     ASK resDefChkBox TO Activate;
                  ELSIF ((totalRes = 0) AND (totalSpares = 0))
                     ASK resDefChkBox TO Deactivate;
                  END IF;}
                  GetSpareList(spareList);
                  IF (maintRadBox.SelectedButton.Id = 2012)
                     FOREACH pool IN poolGroup
                        IF (pool.poolName = poolCombo.Text()) 
                           ASK costVals[12] TO DisplayValue(pool.spareCost);
                           ASK costVals[13] TO DisplayValue(pool.emerShippingCost);
                           EXIT;
                        END IF;
                     END FOREACH;
                  END IF;
                  IF totalSpares > 0
                     ASK poolRadButton TO Activate;
                     {Block using a spare, check to see if deleted spare in use}
                     IF maintRadBox.SelectedButton.Id = 2012
                        ASK poolCombo TO SetOptions(spareList);
                        ASK poolCombo TO DisplayText(tempPool);
                        FOR i := 1 TO totalSpares
                           IF spareList[i] = tempPool
                              poolMissing := TRUE;
                           END IF;
                        END FOR;
                        poolMissing := NOT(poolMissing);
                     END IF;      
                  ELSE
                     ASK poolRadButton TO Deactivate;
                     IF maintRadBox.SelectedButton.Id = 2012
                        poolMissing := TRUE;
                     END IF;
                  END IF;
                  {Spare pool block was using was deleted}
                  IF poolMissing
                     ASK maintRadBox TO SetSelectedButton(infRadButton);
                     ASK poolCombo     TO SetHidden(TRUE);
                     ASK costVals[12]  TO Activate;
                     ASK costVals[12]  TO SetValue(1.);
                     ASK costVals[13]  TO Activate;
                     ASK costVals[13]  TO SetValue(1.);
                  END IF;
                  Draw;
               OTHERWISE
            END CASE;
           
         WHEN 300: {Preventive Maintenance Tab}
           IF activePhases = 0
              ASK misDefChkBox TO Deactivate;
           END IF;
           
           lastClicked := ASK prevMaintTab LastPicked;
           CASE lastClicked.Id
             WHEN 301: {PM Activities Check Box}
               IF pmActivitiesChkBox.Checked
                 ASK freqRadBox TO Activate;
                 ASK freqLabel TO Activate;
                 ASK unitsLabel1 TO Activate;    
                 ASK firesLabel TO Activate;
                 ASK durationLabel TO Activate;
                 ASK durTable TO Activate;
                 ASK durTable TO SetHidden(FALSE);
                 ASK distLabel TO SetHidden(TRUE);
                 ASK unitsLabel2 TO Activate;
                 ASK optionsLabel TO Activate;
                 ASK hourRadButton TO Activate;
                 ASK spareChkBox TO Activate;
                 ASK refreshChkBox TO Activate;
                 IF activePhases > 0 
                    ASK misDefChkBox TO Activate;
                 END IF;
                 IF (totalTriggers > 0) 
                    ASK trigRadButton TO Activate;
                 END IF;
                 IF ((freqRadBox.SelectedButton.Id = 3032) AND (totalTriggers > 0))
                    ASK trigComboBox TO Activate;
                 ELSE                 
                    ASK trigComboBox TO Deactivate;
                    ASK unitsLabel3 TO Activate;
                    ASK staggerLabel TO Activate;
                    ASK staggerVal TO Activate;
                    ASK failChkBox TO Activate;
                    ASK hourValBox TO Activate;
                    ASK freqRadBox TO SetSelectedButton(hourRadButton);
                 END IF;
                 ASK resDefChkBox TO Activate;
                  
              ELSE
                 ASK freqRadBox TO Deactivate;
                 ASK freqLabel TO Deactivate;
                 ASK hourRadButton TO Deactivate;
                 ASK trigRadButton TO Deactivate;
                 ASK hourValBox TO Deactivate;
                 ASK unitsLabel1 TO Deactivate;    
                 ASK trigComboBox TO Deactivate;
                 ASK firesLabel TO Deactivate;
                 ASK durationLabel TO Deactivate;
                 ASK durTable TO Deactivate;
                 ASK durTable TO SetHidden(TRUE);
                 ASK distLabel TO SetLabel(durTable.Text(1,0));
                 ASK distLabel TO SetHidden(FALSE);
                 ASK distLabel TO Deactivate;
                 ASK unitsLabel2 TO Deactivate;
                 ASK optionsLabel TO Deactivate;
                 ASK spareChkBox TO Deactivate;
                 ASK refreshChkBox TO Deactivate;
                 ASK misDefChkBox TO Deactivate;
                 ASK failChkBox TO Deactivate;
                 ASK resDefChkBox TO Deactivate;
                 ASK unitsLabel3 TO Deactivate;
                 ASK staggerLabel TO Deactivate;
                 ASK staggerVal TO Deactivate;
                 {ASK createTrigger TO Deactivate;}

               END IF;
             WHEN 303: {Frequency radio box}
               IF freqRadBox.SelectedButton.Id = 3031 { Every ... }
                 ASK hourValBox TO Activate;
                 ASK trigComboBox TO Deactivate;
                 ASK staggerLabel TO Activate;
                 ASK staggerVal TO Activate;
                 ASK unitsLabel3 TO Activate;
                 ASK failChkBox TO Activate;
               ELSE
                 ASK trigComboBox TO Activate;
                 ASK hourValBox TO Deactivate;
                 ASK staggerLabel TO Deactivate;
                 ASK staggerVal TO Deactivate;
                 ASK unitsLabel3 TO Deactivate;
                 ASK failChkBox TO Deactivate;
               END IF;
             WHEN 309: {Duration Distribution}
                ShowDistBox(tempDurDist, tempDurParams);
                MakeDistString(tempDurDist, tempDurParams, tempString);
                ASK durTable TO SetText(tempString, 1, 0);
                Draw;
                {distChanged:= TRUE;}
             WHEN 320:
                NEW(trigBox);
                ASK trigBox TO LoadFromLibrary(dialogs, "TrigBox");
                ASK window TO AddGraphic(trigBox);
                ASK trigBox TO ReceiveData;
                DISPOSE(trigBox);
                GetTrigList(trigList);
                ASK trigComboBox TO SetOptions(trigList);
                IF (trigComboBox.Text() = pmTrigName)
                     blockUsesTrig := TRUE;
                END IF; 
   
                {FIX - this should be: 
                  IF pmActivitiesChkBox.Checked
                     IF totalTriggers > 0
                      ...
                     ELSE
                      ...
                     END IF;
                  END IF; (else, do nothing)}
                IF ((totalTriggers > 0) AND (pmActivitiesChkBox.Checked)) 
                    {AND (NOT(blockUsesTrig)) }
                   ASK trigRadButton TO Activate;
                ELSIF (pmActivitiesChkBox.Checked) 
                   IF ((blockUsesTrig) AND (freqRadBox.SelectedButton.Id = 3032))
                      {Removed trigger used by this block}
                      ASK pmActivitiesChkBox TO SetCheck(FALSE);
                      ASK freqRadBox TO Deactivate;
                      ASK freqLabel TO Deactivate;
                      ASK hourRadButton TO Deactivate;
                      ASK hourValBox TO Deactivate;
                      ASK trigRadButton TO Deactivate;
                      ASK trigComboBox TO Deactivate;
                      ASK unitsLabel1 TO Deactivate;    
                      ASK firesLabel TO Deactivate;
                      ASK durationLabel TO Deactivate;
                      ASK durTable TO Deactivate;
                      ASK durTable TO SetHidden(TRUE);
                      ASK distLabel TO SetLabel(durTable.Text(1,0));
                      ASK distLabel TO SetHidden(FALSE);
                      ASK distLabel TO Deactivate;
                      ASK unitsLabel2 TO Deactivate;
                      ASK optionsLabel TO Deactivate;
                      ASK spareChkBox TO Deactivate;
                      ASK refreshChkBox TO Deactivate;
                      ASK misDefChkBox TO Deactivate;
                      ASK failChkBox TO Deactivate;
                      ASK resDefChkBox TO Deactivate;
                      ASK unitsLabel3 TO Deactivate;
                      ASK staggerLabel TO Deactivate;
                      ASK staggerVal TO Deactivate;
                   ELSE
                      ASK trigRadButton TO Deactivate;
                      ASK trigComboBox TO Deactivate;
                      ASK freqRadBox TO SetSelectedButton(hourRadButton);
                      ASK staggerVal TO Activate;
                      ASK staggerLabel TO Activate;
                      ASK unitsLabel3 TO Activate;
                      ASK hourRadButton TO Activate;
                      ASK hourValBox TO Activate;
                      ASK failChkBox TO Activate;
                   END IF;
                END IF;
             OTHERWISE
           END CASE;
           Update;

         WHEN 400: {Resource Tab}
            lastClicked := ASK resTab LastPicked;
            CASE lastClicked.Id
            WHEN 402:
               IF res1Combo.Text = "None"
                  ASK res1Txt2 TO Deactivate;
                  ASK res1Req TO SetValue(1.);
                  ASK res1Req TO Deactivate;
                  ASK res1ReqPM TO Deactivate;
                  ASK res1Txt3 TO Deactivate;
               ELSE
                  ASK poolTypeText TO Activate;
                  ASK afterFailText TO Activate;
                  ASK forPMText TO Activate;
                  ASK res1Txt1 TO Activate;
                  ASK res1Combo TO Activate;
                  ASK res1Txt2 TO Activate;
                  ASK res1Req TO Activate;
                  ASK res1ReqPM TO Activate;
                  ASK res1Txt3 TO Activate;
               END IF;
            WHEN 437: {Create Resources}
               tempRes := res1Combo.Text;
               NEW(resPoolsBox);
               ASK resPoolsBox TO LoadFromLibrary(dialogs, "ResPoolsBox");
               ASK window TO AddGraphic(resPoolsBox);
               ASK resPoolsBox TO ReceiveData;
               DISPOSE(resPoolsBox);
               {IF ((totalRes > 0) AND (pmActivitiesChkBox.Checked))
                  ASK resDefChkBox TO Activate;
               ELSIF ((totalRes = 0) AND (totalSpares = 0))
                  ASK resDefChkBox TO Deactivate;
               END IF;}
               GetResList(resList);
               DISPOSE(tempResList);
               NEW(tempResList, 1..totalRes+1);
               tempResList[1] := "None";
               FOR i := 1 TO totalRes
                  tempResList[i+1] := resList[i];
               END FOR;
               IF totalRes > 0
                  ASK poolTypeText TO Activate;
                  ASK afterFailText TO Activate;
                  ASK forPMText TO Activate;
                  ASK res1Txt1 TO Activate;
                  ASK res1Combo TO Activate;
                  ASK res1Combo TO SetOptions(tempResList);
                  ASK res1Combo TO SetText(tempRes);
               ELSE
                  ASK poolTypeText TO Deactivate;
                  ASK afterFailText TO Deactivate;
                  ASK forPMText TO Deactivate;
                  ASK res1Txt1 TO Deactivate;
                  ASK res1Combo TO Deactivate;
                  ASK res1Txt2 TO Deactivate;
                  ASK res1Req   TO Deactivate;
                  ASK res1ReqPM TO Deactivate;
                  ASK res1Txt3 TO Deactivate;
               END IF;
               IF res1Combo.Text() <> "None"
                  FOR i := 1 TO totalRes+1
                     IF tempRes = tempResList[i]
                        resThere := TRUE;
                     END IF;
                  END FOR;
                  {Resource selected was deleted}
                  IF NOT resThere
                     FOR i := 1 TO totalRes+1
                        IF resInUse = tempResList[i]
                           resThere := TRUE;
                           ASK res1Combo TO DisplayText(resInUse);
                        END IF;
                     END FOR;
                  END IF;
                  IF NOT resThere
                     ASK res1Combo TO DisplayText("None");
                     ASK res1Req TO SetValue(1.);
                     ASK res1ReqPM TO SetValue(1.);
                  END IF;
               END IF;
               Draw;
               OTHERWISE
            END CASE;
         WHEN 600: {Dependency Tab}
            lastClicked := ASK dependTab LastPicked;
            IF lastClicked.Id = 601
               IF dependRadBox.SelectedButton.Id = 6013
                  ASK dependRadBox TO SetSelectedButton(sysButton);
                  ASK buddyCombo TO Deactivate;
                  IF compileType <> "student"
                     ASK defineDepText TO Activate; 
                     ASK opText TO Activate;         
                     ASK opValBox TO Activate;        
                     ASK idleValBox TO Activate;         
                     ASK idleText TO Activate;
                     ASK PMValBox TO Activate;
                     ASK PMText TO Activate;
                     ASK failValBox TO Activate;         
                     ASK failedText TO Activate;
                     ASK lifeExBox TO Activate;
                     ASK blockRadBox TO Activate;
                     ASK PMText2 TO Activate;
                     ASK defaultDepText TO Activate;
                  END IF;
               ELSIF dependRadBox.SelectedButton.Id = 6011
                  ASK dependRadBox TO SetSelectedButton(indepButton);
                  ASK buddyCombo TO Deactivate;
                  ASK defineDepText TO Deactivate; 
                  ASK opText TO Deactivate;         
                  ASK opValBox TO Deactivate;        
                  ASK idleValBox TO Deactivate;         
                  ASK idleText TO Deactivate;
                  ASK PMValBox TO Deactivate;
                  ASK PMText TO Deactivate;
                  ASK failValBox TO Deactivate;         
                  ASK failedText TO Deactivate;
                  ASK lifeExBox TO Deactivate;
                  ASK blockRadBox TO Deactivate;
                  ASK PMText2 TO Deactivate;
                  ASK defaultDepText TO Deactivate;
               ELSIF dependRadBox.SelectedButton.Id = 6012
                  ASK dependRadBox TO SetSelectedButton(localButton);
                  ASK buddyCombo TO Deactivate;
                  IF compileType <> "student"
                     ASK defineDepText TO Activate; 
                     ASK opText TO Activate;         
                     ASK opValBox TO Activate;        
                     ASK idleValBox TO Activate;         
                     ASK idleText TO Activate;
                     ASK PMValBox TO Activate;
                     ASK PMText TO Activate;
                     ASK failValBox TO Activate;         
                     ASK failedText TO Activate;
                     ASK lifeExBox TO Activate;
                     ASK blockRadBox TO Activate;
                     ASK PMText2 TO Activate;
                     ASK defaultDepText TO Activate;
                  END IF;
               ELSE
                  ASK dependRadBox TO SetSelectedButton(itemButton);
                  ASK buddyCombo TO Activate;
                  IF compileType <> "student"
                     ASK defineDepText TO Activate; 
                     ASK opText TO Activate;         
                     ASK opValBox TO Activate;        
                     ASK idleValBox TO Activate;         
                     ASK idleText TO Activate;
                     ASK PMValBox TO Activate;
                     ASK PMText TO Activate;
                     ASK failValBox TO Activate;         
                     ASK failedText TO Activate;
                     ASK lifeExBox TO Activate;
                     ASK blockRadBox TO Activate;
                     ASK PMText2 TO Activate;
                     ASK defaultDepText TO Activate;
                  END IF;
               END IF;
            END IF;
          WHEN 700: {Simulation Theory Tab}
            lastClicked := ASK simTheoryTab LastPicked;
            CASE lastClicked.Id
               WHEN 701:{Stream radio box}
                  IF streamRadBox.SelectedButton.Id = 7011 {System stream}
                     ASK sysCombo   TO Activate;
                     ASK indFCombo  TO Deactivate;
                     ASK failText   TO Deactivate;
                     ASK indRCombo  TO Deactivate;
                     ASK repairText TO Deactivate;
                     ASK repairAntiChkBox TO Deactivate;
                     ASK failAntiChkBox TO Deactivate;
                  ELSE
                     ASK sysCombo   TO Deactivate;
                     ASK indFCombo  TO Activate;
                     ASK failText   TO Activate;
                     ASK indRCombo  TO Activate;
                     ASK repairText TO Activate;
                     ASK repairAntiChkBox TO Activate;
                     ASK failAntiChkBox TO Activate;
                  END IF;
               WHEN 709:{Start Sim radio box}
                  IF startRadBox.SelectedButton.Id = 7093 {Completely random}
                     ASK failedRadBox TO SetHidden(TRUE);
                     ASK expiredVal TO SetHidden(TRUE);
                     ASK timeLabel6 TO SetHidden(TRUE);
                  ELSIF startRadBox.SelectedButton.Id = 7091 {Starts Up}
                     ASK failedRadBox TO SetHidden(FALSE);
                     ASK preButton TO SetHidden(TRUE);
                     ASK repairButton TO SetLabel("Some life exhausted:");
                     ASK timeLabel6 TO SetLabel(systemUnits);
                     IF failedRadBox.SelectedButton.Id = 7103
                        ASK failedRadBox TO SetSelectedButton(repairButton);
                     END IF;
                     IF failedRadBox.SelectedButton.Id = 7101
                        ASK expiredVal TO SetHidden(FALSE);
                        ASK timeLabel6 TO SetHidden(FALSE);
                     ELSE
                        ASK expiredVal TO SetHidden(TRUE);
                        ASK timeLabel6 TO SetHidden(TRUE);
                     END IF;
                  ELSE
                     ASK failedRadBox TO SetHidden(FALSE); {Starts Down}
                     ASK preButton TO SetHidden(FALSE);
                     ASK timeLabel6 TO SetLabel("% complete");
                     ASK repairButton TO SetLabel("Repair cycle is:"); 
                     IF failedRadBox.SelectedButton.Id = 7101
                        ASK expiredVal TO SetHidden(FALSE);
                        ASK timeLabel6 TO SetHidden(FALSE);
                     ELSE
                        ASK expiredVal TO SetHidden(TRUE);
                        ASK timeLabel6 TO SetHidden(TRUE);
                     END IF;
                  END IF;
               WHEN 710:{Failed State radio box}
                  IF failedRadBox.SelectedButton.Id = 7102 {Random}
                     ASK expiredVal TO SetHidden(TRUE);
                     ASK timeLabel6 TO SetHidden(TRUE);
                  ELSIF failedRadBox.SelectedButton.Id = 7101 {Some Expired}
                     ASK expiredVal TO SetHidden(FALSE);
                     ASK timeLabel6 TO SetHidden(FALSE);
                  ELSE
                     ASK expiredVal TO SetHidden(TRUE);
                     ASK timeLabel6 TO SetHidden(TRUE);
                  END IF;
                OTHERWISE;
             END CASE;
             Update;
          WHEN 800:{Advanced tab}
            lastClicked := ASK advTab LastPicked;
            CASE lastClicked.Id
               WHEN 803:{Repairs less than new check box}
                  IF degradeChk.Checked
                     ASK degradeRadBox TO Activate;
                     ASK degradeTxt1 TO SetHidden(FALSE);
                     ASK degradeVal1 TO SetHidden(FALSE);
                     ASK degradeTxt2 TO SetHidden(FALSE);
                     ASK degradeVal2 TO SetHidden(FALSE);
                     ASK degradeTxt3 TO SetHidden(FALSE);
                  ELSE
                     ASK degradeRadBox TO Deactivate;
                     ASK degradeTxt1 TO SetHidden(TRUE);
                     ASK degradeVal1 TO SetHidden(TRUE);
                     ASK degradeTxt2 TO SetHidden(TRUE);
                     ASK degradeVal2 TO SetHidden(TRUE);
                     ASK degradeTxt3 TO SetHidden(TRUE);
                  END IF;
               WHEN 805:{Degrade Rad Box}
                  IF degradeRadBox.SelectedButton.Id = 8051 {Linear}
                     ASK degradeTxt1 TO SetLabel("Slope");
                     ASK degradeTxt3 TO SetLabel("formula: (Sx+1)*M)");
                  ELSIF degradeRadBox.SelectedButton.Id = 8052 {Geometric}
                     ASK degradeTxt1 TO SetLabel("Base");
                     ASK degradeTxt3 TO SetLabel("formula: (B^x)*M)");
                  ELSE {Asymptotic}
                     ASK degradeTxt1 TO SetLabel("Rate");
                     ASK degradeTxt3 TO SetLabel("formula: L+(M-L)*e^(-xR)");
                  END IF;
               OTHERWISE
            END CASE;
            Update;
         WHEN 9022: {Reset Defaults Button}
            LoadDefs(0);
         OTHERWISE
      END CASE;      
      DISPOSE(fOff);
      DISPOSE(rOff);
      DISPOSE(units);
   END METHOD; {BeSelected}
END OBJECT;{BlockPropObj}

OBJECT MassEditBoxObj;
   ASK METHOD MakeChanges (IN changeGroup    : selectGroupObj);
   VAR
      eTab, nTab, bTab, hTab, validData,
      nodePhase,setK1,eventPhase, coldmsg,
      bNameMsg, eNameMsg, nNameMsg, hNameMsg,
      blockPhase,hierPhase,sysInd, fillElemCombo : BOOLEAN;
      newSP                       : REAL;
      eventRS,blockRS,nodeDep,
      blockDep, hierDep, i        : INTEGER;
      nextString, tempString      : STRING;
      current, buddy              : RBDBasicObj;
      block, tempBlock            : RBDBlockObj;
      tempEvent, event            : RBDEventObj;
      node, tempNode, outNode     : RBDNodeObj;
      hier, tempHier              : RBDHierObj;
      hierGroup2, buddyGroup      : QueueObj; 
      buddyList                   : OptionListType;
      nodeDepType, blockDepType, 
      hierDepType, bNameChangeType,
      bNewName, eNameChangeType,
      eNewName, nNameChangeType,
      nNewName, hNameChangeType,
      hNewName                    : STRING;
   BEGIN
      blockTab         := Child("BlockTab",100);
         bReqPhaseChk  := Descendant("BReqPhaseBox",101);
         bPhaseRadBox  := Descendant("BPhaseRadBox",102); 
         sysDepChk     := Descendant("SysDepBox",103);
         dependRadBox  := Descendant("DependRadBox",104);
         bElementRad   := ASK dependRadBox Child("ElementItem", 1044);
         bElementCombo := Descendant("BElementCombo", 105);
         bSysStreamChk := Descendant("BSysStreamBox",106);
         bSysCombo     := Descendant("BSysComboBox",107);   
         noRepairChk   := Descendant("NoRepairBox",108);
         infSparesChk  := Descendant("InfSpareBox",109);   
         noResChk      := Descendant("NoResBox",110);
         bNameChk      := Descendant("BNameChk", 111);
         bNameTextBox  := Descendant("BNameTextBox", 112);
         bNameRadBox   := Descendant("BNameRadBox", 113);
         bSuffixRadButton := Descendant("BSuffixRadButton", 1132);
      eventTab         := Child("EventTab",200);
         eReqPhaseChk  := Descendant("EReqPhaseBox",201);
         ePhaseRadBox  := Descendant("EPhaseRadBox",202);
         successChk    := Descendant("SuccessBox",203);
         successValBox := Descendant("SuccessValBox",204);   
         eSysStreamChk := Descendant("ESysStreamBox",205);
         eSysCombo     := Descendant("ESysComboBox",206);         
         eNameChk      := Descendant("ENameChk", 207);
         eNameTextBox  := Descendant("ENameTextBox", 208);
         eNameRadBox   := Descendant("ENameRadBox", 209);
         eSuffixRadButton := Descendant("ESuffixRadButton", 2092);
      nodeTab          := Child("NodeTab",300);
         nReqPhaseChk  := Descendant("NReqPhaseBox",301);
         nPhaseRadBox  := Descendant("NPhaseRadBox",302); 
         nodeDepBox    := Descendant("NodeDependBox",303);
         nodeDepRad    := Descendant("NodeDependRadBox",304);
         nElementRad   := ASK nodeDepRad Child("NElementItem", 3044);
         nElementCombo := Descendant("NElementCombo", 305);
         setKChk       := Descendant("SetKBox",306);
         setKRadBox    := Descendant("SetKRadBox",307);                         
         nNameChk      := Descendant("NNameChk", 308);
         nNameTextBox  := Descendant("NNameTextBox", 309);
         nNameRadBox   := Descendant("NNameRadBox", 310);
         nSuffixRadButton := Descendant("NSuffixRadButton", 3102);
      hierTab          := Child("HierTab",400);
         hReqPhaseChk  := Descendant("HReqPhaseBox",401);
         hPhaseRadBox  := Descendant("HPhaseRadBox",402); 
         hierDepBox    := Descendant("HierDependBox",403);
         hierDepRad    := Descendant("HierDependRadBox",404);
         hElementRad   := ASK hierDepRad Child("HElementItem", 4044);
         hElementCombo := Descendant("HElementCombo", 405);
         hNameChk      := Descendant("HNameChk", 406);
         hNameTextBox  := Descendant("HNameTextBox", 407);
         hNameRadBox   := Descendant("HNameRadBox", 408);
         hSuffixRadButton := Descendant("HSuffixRadButton", 4082);
      NEW(hierGroup2);
      FOREACH hier IN hierGroup
         IF changeGroup.Includes(hier)
            ASK hierGroup2 TO Add(hier);
         END IF;
      END FOREACH;
      FOREACH tempHier IN hierGroup2
         ASK tempHier TO AugmentSelectGroup; {changeGroup is selectGroup}
         ASK hierGroup2 TO RemoveThis(tempHier);
      END FOREACH;
      DISPOSE(hierGroup2);
      FOREACH current IN changeGroup
         IF (OBJTYPENAME(current) = "RBDNodeObj")
            node := ASK root Child("RBDNode",current.Id);
            IF node.typeNode = 2
               nTab := TRUE;
            ELSE
               ASK changeGroup TO RemoveThis(node);
               ASK node TO SetSelected(FALSE);
               ASK node TO SetHighlighted(FALSE);
               ASK node TO Draw;
            END IF;
         ELSIF (OBJTYPENAME(current) = "RBDBlockObj")
            block := ASK root Child("RBDBlock",current.Id);
            bTab := TRUE;
         ELSIF (OBJTYPENAME(current) = "RBDEventObj")
            event := ASK root Child("RBDEvent",current.Id);
            eTab := TRUE;
         ELSIF (OBJTYPENAME(current) = "RBDHierObj")
            hier := ASK root Child("RBDHier",current.Id);
            hTab := TRUE;
         END IF;
      END FOREACH;
      IF NOT(nTab OR eTab OR bTab OR hTab)
         NEW(message, 1..1);
         message[1] := "Cannot mass edit the end node or start node!     ";
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
         RETURN;
      END IF;  
      Draw;
      
      NEW(buddyGroup);
      FOREACH tempBlock IN blockGroup
         IF (NOT(changeGroup.Includes(tempBlock)))
            ASK buddyGroup TO Add(tempBlock);
         END IF;
      END FOREACH;
      FOREACH tempEvent IN eventGroup
         IF (NOT(changeGroup.Includes(tempEvent)))
            ASK buddyGroup TO Add(tempEvent);
         END IF;
      END FOREACH;
      FOREACH tempNode IN nodeGroup
         IF (NOT(changeGroup.Includes(tempNode)))
            IF (tempNode.typeNode = 2)
               ASK buddyGroup TO Add(tempNode);
            END IF;
         END IF;
      END FOREACH;
      FOREACH tempHier IN hierGroup
         IF (NOT(changeGroup.Includes(tempHier)))
            ASK buddyGroup TO Add(tempHier);
         END IF;
      END FOREACH;
      IF buddyGroup.numberIn = 0
         fillElemCombo := FALSE;
      ELSE
         fillElemCombo := TRUE;
         NEW(buddyList, 1..buddyGroup.numberIn);
         i := 1;
         FOREACH buddy IN buddyGroup
            tempString := INTTOSTR(buddy.Id);
            IF buddy.Id < 10
               tempString := "00" + tempString;
            ELSIF buddy.Id < 100
               tempString := "0" + tempString;
            END IF;
            IF OBJTYPENAME(buddy) = "RBDBlockObj"
               tempBlock := ASK root Child("RBDBlock", buddy.Id);
               buddyList[i] := tempBlock.name + " block - " + tempString;
            ELSIF OBJTYPENAME(buddy) = "RBDEventObj"
               tempEvent := ASK root Child("RBDEvent", buddy.Id);
               buddyList[i] := tempEvent.name + " event - " + tempString;
            ELSIF OBJTYPENAME(buddy) = "RBDNodeObj"
               tempNode := ASK root Child("RBDNode", buddy.Id);
               buddyList[i] := tempNode.name + " node - " + tempString;
            ELSIF OBJTYPENAME(buddy) = "RBDHierObj"
               tempHier := ASK root Child("RBDHier", buddy.Id);
               buddyList[i] := tempHier.name + " hier - " + tempString;
            END IF;
            INC(i);
            ASK buddyGroup TO RemoveThis(buddy);
         END FOREACH;
      END IF;  
      DISPOSE(buddyGroup);
      IF bTab
         ASK bSysCombo TO Deactivate;
         ASK dependRadBox TO Deactivate;
         ASK bPhaseRadBox TO Deactivate;
         IF fillElemCombo
            ASK bElementCombo TO SetOptions(buddyList);
         ELSE
            ASK bElementRad TO Deactivate;
         END IF;
         ASK bElementCombo TO Deactivate;
         ASK bNameTextBox TO SetText("unnamed");
         ASK bNameTextBox TO Deactivate;
         ASK bNameRadBox TO Deactivate;
         ASK bNameRadBox TO SetSelectedButton(bSuffixRadButton);
      ELSE
         ASK blockTab TO SetHidden(TRUE);
      END IF;
      IF eTab
         ASK successValBox TO Deactivate;
         ASK eSysCombo TO Deactivate;
         ASK ePhaseRadBox TO Deactivate;
         ASK eNameTextBox TO SetText("unnamed");
         ASK eNameTextBox TO Deactivate;
         ASK eNameRadBox TO Deactivate;
         ASK eNameRadBox TO SetSelectedButton(eSuffixRadButton);
     ELSE
         ASK eventTab TO SetHidden(TRUE);
      END IF;
      IF nTab
         ASK setKRadBox TO Deactivate;
         ASK nPhaseRadBox TO Deactivate;
         ASK nodeDepRad TO Deactivate;
         IF fillElemCombo
            ASK nElementCombo TO SetOptions(buddyList);
         ELSE
            ASK nElementRad TO Deactivate;
         END IF;
         ASK nElementCombo TO Deactivate;
         ASK nNameTextBox TO SetText("unnamed");
         ASK nNameTextBox TO Deactivate;
         ASK nNameRadBox TO Deactivate;
         ASK nNameRadBox TO SetSelectedButton(nSuffixRadButton);
      ELSE
         ASK nodeTab TO SetHidden(TRUE);
      END IF;
      IF hTab
         ASK hierDepRad TO Deactivate;
         ASK hPhaseRadBox TO Deactivate;
         IF fillElemCombo
            ASK hElementCombo TO SetOptions(buddyList);
         ELSE
            ASK hElementRad TO Deactivate;
         END IF;
         ASK hElementCombo TO Deactivate;
         ASK hNameTextBox TO SetText("unnamed");
         ASK hNameTextBox TO Deactivate;
         ASK hNameRadBox TO Deactivate;
         ASK hNameRadBox TO SetSelectedButton(hSuffixRadButton);
      ELSE
         ASK hierTab TO SetHidden(TRUE);
      END IF;
      Draw;
      REPEAT
         validData := TRUE;
         button := AcceptInput();
         IF button.ReferenceName = "OKButton";
            {------------------ Get Block changes ------------------}
            {Phasing}
            IF bPhaseRadBox.SelectedButton.Id = 1021
               blockPhase := TRUE;
            END IF;
            {Dependency}
            IF dependRadBox.SelectedButton.Id = 1041
               blockDep := 0;
            ELSIF dependRadBox.SelectedButton.Id = 1042
               blockDep := -1;
            ELSIF dependRadBox.SelectedButton.Id = 1043
               blockDep := -2;
            ELSE
               i := POSITION(bElementCombo.Text, " -");
               blockDep := STRTOINT(SUBSTR(i+2, i+11, bElementCombo.Text + "      "));
               IF (SUBSTR(i-1,i-1, bElementCombo.Text) = "r")
                  blockDepType := "RBDHier";
                  tempHier := ASK root Child("RBDHier", blockDep);
                  blockDep := tempHier.outID;
               ELSIF (SUBSTR(i-1,i-1, bElementCombo.Text) = "k")
                  blockDepType := "RBDBlock";
               ELSIF (SUBSTR(i-1,i-1, bElementCombo.Text) = "e")
                  blockDepType := "RBDNode";
               ELSIF (SUBSTR(i-1,i-1, bElementCombo.Text) = "t")
                  blockDepType := "RBDEvent";
               END IF;
            END IF;
            {System Stream}
            nextString := bSysCombo.Text();
            IF nextString = "A"
               blockRS := 201;
            ELSIF nextString = "B"
               blockRS := 202;
            ELSIF nextString = "C"
               blockRS := 203;
            ELSIF nextString = "D"
               blockRS := 204;
            ELSIF nextString = "E"
               blockRS := 205;
            ELSIF nextString = "F"
               blockRS := 206;
            ELSIF nextString = "G"
               blockRS := 207;
            ELSIF nextString = "H"
               blockRS := 208;
            ELSIF nextString = "I"
               blockRS := 209;
            ELSIF nextString = "J"
               blockRS := 210;
            END IF;
            {Block name}
            IF bNameChk.Checked
               IF bNameRadBox.SelectedButton.Id = 1131
                  bNameChangeType := "prefix";
               ELSIF bNameRadBox.SelectedButton.Id =1132
                  bNameChangeType := "suffix";
               ELSIF bNameRadBox.SelectedButton.Id = 1133
                  bNameChangeType := "replace";
               END IF;
               bNewName := bNameTextBox.Text();
               IF ((bNewName = "") OR (POSITION(bNewName, " ") <> 0))
                  NEW(message, 1..1);
                  message[1] := "'Block Name' field can't be blank or have blank spaces!     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
                  validData := FALSE;
               ELSIF STRLEN(bNewName) > 20
                  NEW(message, 1..1);
                  message[1] := "'Block Name' must be no greater than 20 characters!     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
                  validData := FALSE;
               END IF;         
            END IF;
            {------------------ Get Event changes ------------------}
            {Phasing}
            IF ePhaseRadBox.SelectedButton.Id = 2021
               eventPhase := TRUE;
            END IF;
            {Success probability}
            newSP := successValBox.Value();
            IF (successValBox.Value() < 0.) OR (successValBox.Value() > 1.)
               NEW(message, 1..1);
               message[1] := "Success probability for events must be between 0 and 1!     ";
               result := SendAlert(message, FALSE, FALSE, TRUE);
               DISPOSE(message);
               ASK successValBox TO SetValue(0.80);
               validData := FALSE;
            END IF;
            {System stream}
            nextString := eSysCombo.Text();
            IF nextString = "A"
               eventRS := 201;
            ELSIF nextString = "B"
               eventRS := 202;
            ELSIF nextString = "C"
               eventRS := 203;
            ELSIF nextString = "D"
               eventRS := 204;
            ELSIF nextString = "E"
               eventRS := 205;
            ELSIF nextString = "F"
               eventRS := 206;
            ELSIF nextString = "G"
               eventRS := 207;
            ELSIF nextString = "H"
               eventRS := 208;
            ELSIF nextString = "I"
               eventRS := 209;
            ELSIF nextString = "J"
               eventRS := 210;
            END IF;
            {Event name}
            IF eNameChk.Checked
               IF eNameRadBox.SelectedButton.Id = 2091
                  eNameChangeType := "prefix";
               ELSIF eNameRadBox.SelectedButton.Id =2092
                  eNameChangeType := "suffix";
               ELSIF eNameRadBox.SelectedButton.Id = 2093
                  eNameChangeType := "replace";
               END IF;
               eNewName := eNameTextBox.Text();
               IF ((eNewName = "") OR (POSITION(eNewName, " ") <> 0))
                  NEW(message, 1..1);
                  message[1] := "'Event Name' field can't be blank or have blank spaces!     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
                  validData := FALSE;
               ELSIF STRLEN(eNewName) > 20
                  NEW(message, 1..1);
                  message[1] := "'Event Name' must be no greater than 20 characters!     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
                  validData := FALSE;
               END IF;         
            END IF;
            {------------------ Get Node changes ------------------}
            {Phasing}
            IF nPhaseRadBox.SelectedButton.Id = 3021
               nodePhase := TRUE;
            END IF;
            {Dependency}
            IF nodeDepRad.SelectedButton.Id = 3041
               nodeDep := 0;
            ELSIF nodeDepRad.SelectedButton.Id = 3042
               nodeDep := -1;
            ELSIF nodeDepRad.SelectedButton.Id = 3043
               nodeDep := -2;
            ELSE   
               i := POSITION(nElementCombo.Text, " -");
               nodeDep := STRTOINT(SUBSTR(i+2, i+11, nElementCombo.Text + "      "));
               IF (SUBSTR(i-1,i-1, nElementCombo.Text) = "r")
                  nodeDepType := "RBDHier";
                  tempHier := ASK root Child("RBDHier", nodeDep);
                  nodeDep := tempHier.outID;
               ELSIF (SUBSTR(i-1,i-1, nElementCombo.Text) = "k")
                  nodeDepType := "RBDBlock";
               ELSIF (SUBSTR(i-1,i-1, nElementCombo.Text) = "e")
                  nodeDepType := "RBDNode";
               ELSIF (SUBSTR(i-1,i-1, nElementCombo.Text) = "t")
                  nodeDepType := "RBDEvent";
               END IF;
            END IF;
            {Set k}
            IF setKRadBox.SelectedButton.Id = 3071
               setK1 := TRUE;
            END IF;
            {Node name}
            IF nNameChk.Checked
               IF nNameRadBox.SelectedButton.Id = 3101
                  nNameChangeType := "prefix";
               ELSIF nNameRadBox.SelectedButton.Id =3102
                  nNameChangeType := "suffix";
               ELSIF nNameRadBox.SelectedButton.Id = 3103
                  nNameChangeType := "replace";
               END IF;
               nNewName := nNameTextBox.Text();
               IF ((nNewName = "") OR (POSITION(nNewName, " ") <> 0))
                  NEW(message, 1..1);
                  message[1] := "'Node Name' field can't be blank or have blank spaces!     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
                  validData := FALSE;
               ELSIF STRLEN(nNewName) > 20
                  NEW(message, 1..1);
                  message[1] := "'Node Name' must be no greater than 20 characters!     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
                  validData := FALSE;
               END IF;         
            END IF;
            {------------------ Get Hier changes ------------------}
            {Phasing}
            IF hPhaseRadBox.SelectedButton.Id = 4021
               hierPhase := TRUE;
            END IF;
            {Dependency}
            IF hierDepRad.SelectedButton.Id = 4041
               hierDep := 0;
            ELSIF hierDepRad.SelectedButton.Id = 4042
               hierDep := -1;
            ELSIF hierDepRad.SelectedButton.Id = 4043
               hierDep := -2;
            ELSE
               i := POSITION(hElementCombo.Text, " -");
               hierDep := STRTOINT(SUBSTR(i+2, i+11, hElementCombo.Text + "      "));
               IF (SUBSTR(i-1,i-1, hElementCombo.Text) = "r")
                  hierDepType := "RBDHier";
                  tempHier := ASK root Child("RBDHier", hierDep);
                  hierDep := tempHier.outID;
               ELSIF (SUBSTR(i-1,i-1, hElementCombo.Text) = "k")
                  hierDepType := "RBDBlock";
               ELSIF (SUBSTR(i-1,i-1, hElementCombo.Text) = "e")
                  hierDepType := "RBDNode";
               ELSIF (SUBSTR(i-1,i-1, hElementCombo.Text) = "t")
                  hierDepType := "RBDEvent";
               END IF;
            END IF;
            {Hier name}
            IF hNameChk.Checked
               IF hNameRadBox.SelectedButton.Id = 4081
                  hNameChangeType := "prefix";
               ELSIF hNameRadBox.SelectedButton.Id = 4082
                  hNameChangeType := "suffix";
               ELSIF hNameRadBox.SelectedButton.Id = 4083
                  hNameChangeType := "replace";
               END IF;
               hNewName := hNameTextBox.Text();
               IF ((hNewName = "") OR (POSITION(hNewName, " ") <> 0))
                  NEW(message, 1..1);
                  message[1] := "'Hierarchy Name' field can't be blank or have blank spaces!     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
                  validData := FALSE;
               ELSIF STRLEN(hNewName) > 20
                  NEW(message, 1..1);
                  message[1] := "'Hierarchy Name' must be no greater than 20 characters!     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
                  validData := FALSE;
               END IF;         
            END IF;
         ELSE
            validData := TRUE;
         END IF;
      UNTIL validData;
      IF button.ReferenceName <> "CancelButton"   {User did not click cancel}
         {---------------------- Make changes to selected elements --------------------}
         somethingChanged := TRUE;
         FOREACH current IN changeGroup
            IF OBJTYPENAME(current) = "RBDNodeObj"
               node := ASK root Child("RBDNode",current.Id);
               IF node.typeNode = 2
                  IF (setKChk.Checked AND node.coldStandby AND (NOT setK1) AND (node.connectIntoNum<>node.goodPaths))  {chuck}
                     coldmsg := TRUE;
                  END IF;
                  IF setK1
                     ASK node TO MassEditNode(nReqPhaseChk.Checked, nodePhase, setKChk.Checked,
                                              nodeDepBox.Checked, nNameChk.Checked, 1, nodeDep, nodeDepType,
                                              nNameChangeType, nNewName, nNameMsg);
                  ELSE
                     ASK node TO MassEditNode(nReqPhaseChk.Checked, nodePhase, setKChk.Checked,
                                              nodeDepBox.Checked, nNameChk.Checked, node.connectIntoNum, 
                                              nodeDep, nodeDepType, nNameChangeType, nNewName, nNameMsg);
                  END IF;
               END IF;
            ELSIF OBJTYPENAME(current) = "RBDBlockObj"
               block := ASK root Child("RBDBlock",current.Id);
               ASK block TO MassEditBlock(sysDepChk.Checked, bReqPhaseChk.Checked, blockPhase, bSysStreamChk.Checked,
                                          infSparesChk.Checked, noRepairChk.Checked, noResChk.Checked, 
                                          bNameChk.Checked, blockRS,blockDep, blockDepType, bNameChangeType, 
                                          bNewName, bNameMsg);
            ELSIF OBJTYPENAME(current) = "RBDEventObj"
               event := ASK root Child("RBDEvent",current.Id);
               ASK event TO MassEditEvent(successChk.Checked,eReqPhaseChk.Checked,eventPhase,eSysStreamChk.Checked,
                                          eNameChk.Checked, eventRS, newSP, eNameChangeType, eNewName, eNameMsg);
            ELSIF OBJTYPENAME(current) = "RBDHierObj"
               hier := ASK root Child("RBDHier", current.Id);
               ASK hier TO MassEditHier(hReqPhaseChk.Checked, hierPhase, hierDepBox.Checked, hNameChk.Checked, 
                                        hierDep, hierDepType, hNameChangeType, hNewName, hNameMsg);                                       
               outNode := ASK root Child("RBDNode", hier.outID);
               ASK outNode TO MassEditNode(hReqPhaseChk.Checked, hierPhase, FALSE, hierDepBox.Checked, 
                                           FALSE, 1, hierDep, hierDepType, nNameChangeType,
                                           nNewName, nNameMsg);
            END IF;
         END FOREACH;
         IF bNameMsg
            NEW(message, 1..3);
            message[1] := "At least one block name was not changed     ";
            message[2] := "as it would have resulted in a block name     ";
            message[3] := "greater than 20 characters in length.     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
         IF eNameMsg
            NEW(message, 1..3);
            message[1] := "At least one event name was not changed     ";
            message[2] := "as it would have resulted in an event name     ";
            message[3] := "greater than 20 characters in length.     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
         IF nNameMsg
            NEW(message, 1..3);
            message[1] := "At least one node name was not changed     ";
            message[2] := "as it would have resulted in a node name     ";
            message[3] := "greater than 20 characters in length.     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
         IF hNameMsg
            NEW(message, 1..3);
            message[1] := "At least one hierarchy name was not changed     ";
            message[2] := "as it would have resulted in a hierarchy name     ";
            message[3] := "greater than 20 characters in length.     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
         IF coldmsg
            NEW(message, 1..3);
            message[1] := "Standby was turned off for at least one     ";
            message[2] := "selected node due to an incompatibility with     ";
            message[3] := "the new k-out-of-n setting.     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
      END IF;
   END METHOD; {MakeChanges}

   ASK METHOD BeSelected;
   VAR
      tabClicked : GraphicVObj;
   BEGIN
      tabClicked := LastPicked;
      CASE tabClicked.Id
        WHEN 100: {Block Tab}
           lastClicked := ASK blockTab LastPicked;
           CASE lastClicked.Id
              WHEN 101: {Require Phasing Checkbox}
                 IF bReqPhaseChk.Checked
                    ASK bPhaseRadBox TO Activate;
                 ELSE
                    ASK bPhaseRadBox TO Deactivate;
                 END IF;
              WHEN 103: {System Dep Checkbox}
                 IF sysDepChk.Checked
                    ASK dependRadBox TO Activate;
                 ELSE
                    ASK dependRadBox TO Deactivate;
                 END IF;
              WHEN 104: {Depend Radio Box}
                 IF dependRadBox.SelectedButton.Id = 1044
                    ASK bElementCombo TO Activate;
                 ELSE
                    ASK bElementCombo TO Deactivate;
                 END IF;
              WHEN 106: {System Stream Checkbox}
                 IF bSysStreamChk.Checked
                    ASK bSysCombo TO Activate;
                 ELSE
                    ASK bSysCombo TO Deactivate;
                 END IF;
              WHEN 111: {Block Name Checkbox}
                 IF bNameChk.Checked
                    ASK bNameTextBox TO Activate;
                    ASK bNameRadBox TO Activate;
                 ELSE
                    ASK bNameTextBox TO Deactivate;
                    ASK bNameRadBox TO Deactivate;
                 END IF;
              OTHERWISE
           END CASE;
        WHEN 200: {Event Tab}
           lastClicked := ASK eventTab LastPicked;
           CASE lastClicked.Id
              WHEN 203: {Success Checkbox}
                 IF successChk.Checked
                    ASK successValBox TO Activate;
                 ELSE
                    ASK successValBox TO Deactivate;
                 END IF;
              WHEN 201: {Require Phasing Checkbox}
                 IF eReqPhaseChk.Checked
                    ASK ePhaseRadBox TO Activate;
                 ELSE
                    ASK ePhaseRadBox TO Deactivate;
                 END IF;
              WHEN 205: {System Stream Checkbox}
                 IF eSysStreamChk.Checked
                    ASK eSysCombo TO Activate;
                 ELSE
                    ASK eSysCombo TO Deactivate;
                 END IF;
              WHEN 207: {Event Name Checkbox}
                 IF eNameChk.Checked
                    ASK eNameTextBox TO Activate;
                    ASK eNameRadBox TO Activate;
                 ELSE
                    ASK eNameTextBox TO Deactivate;
                    ASK eNameRadBox TO Deactivate;
                 END IF;
              OTHERWISE
           END CASE;
        WHEN 300: {Node Tab}
           lastClicked := ASK nodeTab LastPicked;
           CASE lastClicked.Id
              WHEN 301: {Require Phasing Checkbox}
                 IF nReqPhaseChk.Checked
                    ASK nPhaseRadBox TO Activate;
                 ELSE
                    ASK nPhaseRadBox TO Deactivate;
                 END IF;
              WHEN 303: {System Dep Checkbox}
                 IF nodeDepBox.Checked
                    ASK nodeDepRad TO Activate;
                 ELSE
                    ASK nodeDepRad TO Deactivate;
                 END IF;
              WHEN 304: {Depend Radio Box}
                 IF nodeDepRad.SelectedButton.Id = 3044
                    ASK nElementCombo TO Activate;
                 ELSE
                    ASK nElementCombo TO Deactivate;
                 END IF;
              WHEN 306: {Set K Checkbox}
                 IF setKChk.Checked
                    ASK setKRadBox TO Activate;
                 ELSE
                    ASK setKRadBox TO Deactivate;
                 END IF;
              WHEN 308: {Node Name Checkbox}
                 IF nNameChk.Checked
                    ASK nNameTextBox TO Activate;
                    ASK nNameRadBox TO Activate;
                 ELSE
                    ASK nNameTextBox TO Deactivate;
                    ASK nNameRadBox TO Deactivate;
                 END IF;
              OTHERWISE
           END CASE;
        WHEN 400: {Hier Tab}
           lastClicked := ASK hierTab LastPicked;
           CASE lastClicked.Id
              WHEN 401: {Require Phasing Checkbox}
                 IF hReqPhaseChk.Checked
                    ASK hPhaseRadBox TO Activate;
                 ELSE
                    ASK hPhaseRadBox TO Deactivate;
                 END IF;
              WHEN 403: {System Dep Checkbox}
                 IF hierDepBox.Checked
                    ASK hierDepRad TO Activate;
                 ELSE
                    ASK hierDepRad TO Deactivate;
                 END IF;
              WHEN 404: {Depend Radio Box}
                 IF hierDepRad.SelectedButton.Id = 4044
                    ASK hElementCombo TO Activate;
                 ELSE
                    ASK hElementCombo TO Deactivate;
                 END IF;
              WHEN 406: {Hier Name Checkbox}
                 IF hNameChk.Checked
                    ASK hNameTextBox TO Activate;
                    ASK hNameRadBox TO Activate;
                 ELSE
                    ASK hNameTextBox TO Deactivate;
                    ASK hNameRadBox TO Deactivate;
                 END IF;
              OTHERWISE
           END CASE;
        OTHERWISE
      END CASE;     
      Update;
      INHERITED BeSelected;
   END METHOD; {BeSelected}
END OBJECT; {MassEditBoxObj}

OBJECT NodeBoxObj;
   ASK METHOD ReceiveData (INOUT cancelled           : BOOLEAN;
                           IN adding                 : BOOLEAN;
                           IN tempNode               : RBDNodeObj;
                           IN initType               : INTEGER);
   VAR
      newNum,errors,oldType,
      toId,fromId,i, j,
      counter, k, tempNom, tempMax     : INTEGER;
      toRef,fromRef,newName, buddy, deptype : STRING; 
      validData,
      phaseCheck                       : BOOLEAN;
      goodPaths, tempSw                : REAL;
      nodeDeltaArray                   : ARRAY INTEGER OF INTEGER;
      bogusNode, temp2Node                        : RBDNodeObj;
      bogusLink, tempLink              : LinkObj;
      tempBlock                        : RBDBlockObj;
      tempEvent                        : RBDEventObj;
      bogusHier, tempHier              : RBDHierObj;
      init1Array                       : intArray;
      buddyList, buddyList2            : OptionListType;
      buddyPos                         : STRING;
      text                             : strArray;
   BEGIN
      IF tempNode.name <> ""
         SetLabel("Node Properties - "+tempNode.name + " - " + INTTOSTR(tempNode.Id));
      ELSE
         SetLabel("Node Properties");
      END IF;
   
      nodeName        := Descendant("NodeName", 101);
      pathsText       := Descendant("PathsText", 103);
      pathsReqBox     := Descendant("PathsReqBox", 104);
      commentBox      := Descendant("CommentBox", 105);
      coldTab         := Child("ColdTab", 200);
      coldChk         := Descendant("ColdCheck", 201);
      kStarText       := Descendant("KStarText", 202);
      kStarVal        := Descendant("KStarVal", 203);
      priorChk        := Descendant("PriorCheck", 204);
      orderChk        := Descendant("OrderCheck", 205);
      coldTable       := Descendant("ColdTable", 206);
      coldChangeTxt1  := Descendant("ColdChangeText1", 207);
      coldChangeTxt2  := Descendant("ColdChangeText2", 208);
      coldChangeVal   := Descendant("ColdChangeVal", 209);
      coldButton      := Descendant("ColdButton", 210);
      capTab          := Child("CapTab", 300);
      flowInTxt       := Descendant("FlowInText", 301); 
      flowOutTxt      := Descendant("FlowOutText", 302);    
      flowInChk       := Descendant("FlowInCheck", 303);    
      flowOutChk      := Descendant("FlowOutCheck", 304);   
      flowInTable     := Descendant("FlowInTable", 305);
      flowOutTable    := Descendant("FlowOutTable", 306);
      flowGenText     := Descendant("FlowGenText", 307);
      capButton       := Descendant("CapButton", 308);
      capChangeVal    := Descendant("CapChangeVal", 309);
      capChangeText   := Descendant("CapChangeText", 310);
      dependTab       := Child("DependTab", 400);
      dependRadBox    := Descendant("DependRadBox", 401);
        indepButton   := ASK dependRadBox Child("IndepButton", 4011);
        sysButton     := ASK dependRadBox Child("SysButton", 4013);
        localButton   := ASK dependRadBox Child("LocalButton", 4012);
        itemButton    := ASK dependRadBox Child("ItemButton", 4014);
      buddyCombo      := Descendant("BuddyCombo", 402);
      advTab          := Child("AdvTab", 500);
      phaseChk        := Descendant("PhaseCheck", 501);
      nodeInfoChk     := Descendant("NodeInfoCheck", 502);
   
   
      ASK commentBox TO SetText(tempNode.comment);
      IF tempNode.comment = ""
         ASK commentBox TO SetText("Comment");
      END IF;
      ASK flowGenText TO SetLabel("Flow generated = "+INTTOSTR(flowGenerated));
      NEW(text,1..10);
      NEW(init1Array, 1..6);
      IF totalObjects > 1
         NEW(buddyList, 1..(totalObjects-1));
      END IF;
      nodeUp := tempNode;
      oldNum := tempNode.Id;
      totalOut := tempNode.connectOutOfNum;
      totalIn := tempNode.connectIntoNum;
      
      NEW(capList1, 1..1);
      NEW(capList2, 1..3); 
      NEW(capList3, 1..3); 
      NEW(coldList1, 1..1);
      NEW(coldList2, 1..2);
      capList1[1] := "50";
      capList2[1] := "1";
      capList2[2] := "MaxFlow";
      capList2[3] := ("MaxFlow/"+INTTOSTR(totalIn));
      capList3[1] := INTTOSTR(flowGenerated);
      capList3[2] := "FullFlow";
      capList3[3] := ("FullFlow/"+INTTOSTR(totalIn));
      coldList1[1] := "50";
      coldList2[1] := "0.0";
      coldList2[2] := "No Manual Switching";
      ASK pathsText TO SetLabel("out-of-"+INTTOSTR(tempNode.connectIntoNum));
      INC(blueNodes); {for start node}
      INC(blueNodes); { for end node}
      IF startId < tempNode.Id
         DEC(changeNum);
      END IF;
      IF endId < tempNode.Id
         DEC(changeNum);
      END IF;
      IF (totalBlocks = 0) AND ((totalNodes-blueNodes-1) <= 0)
         ASK itemButton TO Deactivate;
         ASK buddyCombo TO Deactivate;
      END IF;
{**************************** Fill Tables ************************************}
      IF totalIn > 9
         ASK coldTable TO SetSize(6,totalIn);
      END IF;   
      IF totalIn > 10
        ASK flowInTable TO SetSize(4,totalIn);
      END IF;   
      IF totalOut > 10
         ASK flowOutTable TO SetSize(3,totalOut);
      END IF;
      FOREACH tempLink IN linkGroup
         IF (tempLink.connectTRef = "RBDNode") AND (tempLink.connectToId = tempNode.Id)
            IF tempLink.connectFRef = "RBDBlock" {new}
               FOREACH tempBlock IN blockGroup
                  IF tempBlock.Id = tempLink.connectFromId
                     INC(connectIn);
                     ASK coldTable TO SetText(tempBlock.name, 1, connectIn);
                     ASK flowInTable TO SetText(tempBlock.name, 1, connectIn);
                     ASK coldTable TO SetText("Block", 2, connectIn);
                     ASK flowInTable TO SetText("Block", 2, connectIn);
                     EXIT;
                  END IF;
               END FOREACH;
            ELSIF tempLink.connectFRef = "RBDEvent" 
               FOREACH tempEvent IN eventGroup
                  IF tempEvent.Id = tempLink.connectFromId
                     INC(connectIn);
                     ASK coldTable TO SetText(tempEvent.name, 1, connectIn);
                     ASK flowInTable TO SetText(tempEvent.name, 1, connectIn);
                     ASK flowInTable TO SetText("Event", 2, connectIn);
                     ASK coldTable TO SetText("Event", 2, connectIn);
                     EXIT;
                  END IF;
               END FOREACH;
            ELSIF (tempLink.connectFRef = "RBDNode")
               FOREACH bogusNode IN nodeGroup
                  IF bogusNode.Id = tempLink.connectFromId
                     INC(connectIn);
                     ASK coldTable TO SetText(bogusNode.name, 1, connectIn);
                     ASK coldTable TO SetText("Node", 2, connectIn);
                     ASK flowInTable TO SetText(bogusNode.name, 1, connectIn);
                     ASK flowInTable TO SetText("Node", 2, connectIn);
                     EXIT;
                  END IF;
               END FOREACH;
            ELSE
               FOREACH bogusHier IN hierGroup
                  IF bogusHier.Id = tempLink.connectFromId
                     INC(connectIn);
                     ASK coldTable TO SetText(bogusHier.name, 1, connectIn);
                     ASK coldTable TO SetText("Hier", 2, connectIn);
                     ASK flowInTable TO SetText(bogusHier.name, 1, connectIn);
                     ASK flowInTable TO SetText("Hier", 2, connectIn);
                     EXIT;
                  END IF;
               END FOREACH;
            END IF;
            ASK coldTable TO SetText(INTTOSTR(tempLink.coldPriority), 3, connectIn);
            ASK coldTable TO SetText(REALTOSTR(tempLink.autoSwitchProb), 4, connectIn);
            ASK coldTable TO SetText(REALTOSTR(tempLink.autoSwitchTime), 5, connectIn);
            IF tempLink.manualSwitchTime > 0.
               ASK coldTable TO SetText(REALTOSTR(tempLink.manualSwitchTime), 6, connectIn);
            ELSE
               ASK coldTable TO SetText("N/A", 6, connectIn);
            END IF;
            IF tempLink.nomFlow = -2
               ASK flowInTable TO SetText("MaxFlow/"+INTTOSTR(totalIn), 3, connectIn);
            ELSIF tempLink.nomFlow = -1
               ASK flowInTable TO SetText("MaxFlow", 3, connectIn);
            ELSE
               ASK flowInTable TO SetText(INTTOSTR(tempLink.nomFlow), 3, connectIn);
            END IF; 
            IF tempLink.maxFlow = -2
               ASK flowInTable TO SetText("FullFlow/"+INTTOSTR(totalIn), 4, connectIn);
            ELSIF tempLink.maxFlow = -1
               ASK flowInTable TO SetText("FullFlow", 4, connectIn);
            ELSE
               ASK flowInTable TO SetText(INTTOSTR(tempLink.maxFlow), 4, connectIn);
            END IF; 
         END IF;
         IF (tempLink.connectFRef = "RBDNode") AND (tempLink.connectFromId = tempNode.Id)
            IF tempLink.connectTRef = "RBDBlock"
               FOREACH tempBlock IN blockGroup
                  IF tempBlock.Id = tempLink.connectToId
                     INC(connectOut);
                     ASK flowOutTable TO SetText(tempBlock.name, 1, connectOut);
                     ASK flowOutTable TO SetText("Block", 2, connectOut);
                     EXIT;
                  END IF;
               END FOREACH;
            ELSIF tempLink.connectTRef = "RBDEvent"
               FOREACH tempEvent IN eventGroup
                  IF tempEvent.Id = tempLink.connectToId
                     INC(connectOut);
                     ASK flowOutTable TO SetText(tempEvent.name, 1, connectOut);
                     ASK flowOutTable TO SetText("Event", 2, connectOut);
                     EXIT;
                  END IF;
               END FOREACH;
            ELSIF (tempLink.connectTRef = "RBDNode")
               FOREACH bogusNode IN nodeGroup
                  IF bogusNode.Id = tempLink.connectToId
                     INC(connectOut);
                     ASK flowOutTable TO SetText(bogusNode.name, 1, connectOut);
                     ASK flowOutTable TO SetText("Node", 2, connectOut);
                     EXIT;
                  END IF;
               END FOREACH;
            ELSE
               FOREACH bogusHier IN hierGroup
                  IF bogusHier.Id = tempLink.connectToId
                     INC(connectOut);
                     ASK flowOutTable TO SetText(bogusHier.name, 1, connectOut);
                     ASK flowOutTable TO SetText("Hier", 2, connectOut);
                     EXIT;
                  END IF;
               END FOREACH;
            END IF;
            ASK flowOutTable TO SetText(INTTOSTR(tempLink.capPriority), 3, connectOut);
         END IF;
      END FOREACH;
      j := 0;
      i := 1;
      FOREACH tempBlock IN blockGroup
         tempString := INTTOSTR(tempBlock.Id);
         IF tempBlock.Id < 10
            tempString := "00" + tempString;
         ELSIF tempBlock.Id < 100
            tempString := "0" + tempString;
         END IF;
         buddyList[i] := tempBlock.name + " block - " + tempString;
         IF (tempNode.DependencyNum > 0) AND (tempNode.depType = "RBDBlock") AND 
            (tempBlock.Id = tempNode.DependencyNum)
            buddy := tempBlock.name + " block - " + tempString;
         END IF; 
         INC(i);
      END FOREACH;
      FOREACH tempEvent IN eventGroup
         tempString := INTTOSTR(tempEvent.Id);
         IF tempEvent.Id < 10
            tempString := "00" + tempString;
         ELSIF tempEvent.Id < 100
            tempString := "0" + tempString;
         END IF;
         buddyList[i] := tempEvent.name + " event - " + tempString;
         IF (tempNode.DependencyNum > 0) AND (tempNode.depType = "RBDEvent") AND 
            (tempEvent.Id = tempNode.DependencyNum)
            buddy := tempEvent.name + " event - " + tempString;
         END IF; 
         INC(i);
      END FOREACH;
      FOREACH bogusNode IN nodeGroup
         tempString := INTTOSTR(bogusNode.Id);
         IF bogusNode.Id < 10
            tempString := "00" + tempString;
         ELSIF bogusNode.Id < 100
            tempString := "0" + tempString;
         END IF;
         IF tempNode.Id <> bogusNode.Id
            IF ((bogusNode.name = "end") OR (bogusNode.name = "start")
                OR (bogusNode.typeNode = 4))
               INC(j);
            ELSE
               IF bogusNode.typeNode = 5
                  tempHier := ASK root Child("RBDHier", bogusNode.parentID);
                  tempString := INTTOSTR(bogusNode.parentID);
                  IF bogusNode.parentID < 10
                     tempString := "00" + tempString;
                  ELSIF bogusNode.parentID < 100
                     tempString := "0" + tempString;
                  END IF;
                  buddyList[i-j] := tempHier.name + " hier - " + tempString;
                  IF (tempNode.DependencyNum > 0) AND (tempNode.depType = "RBDHier") AND 
                     (bogusNode.Id = tempNode.DependencyNum)
                     buddy := tempHier.name + " hier - " + tempString;
                  END IF;
               ELSE
                  buddyList[i-j] := bogusNode.name + " node - " + tempString; 
               END IF;
            END IF;
            IF (bogusNode.typeNode <> 5) AND (tempNode.DependencyNum > 0) AND (tempNode.depType = "RBDNode") AND 
               (bogusNode.Id = tempNode.DependencyNum)
               buddy := bogusNode.name + " node - " + tempString;
            END IF;
            INC(i);
         END IF
      END FOREACH; 
      
{**************************** Fill Tabs ************************************}
     { ASK kStarText TO Deactivate;}
      
      ASK nodeInfoChk TO SetCheck(tempNode.reportNodeAnal);
      ASK coldButton TO SetHidden(TRUE);
      ASK coldChangeVal TO SetHidden(TRUE);
      ASK coldChangeTxt1 TO SetHidden(TRUE);
      ASK coldChangeTxt2 TO SetHidden(TRUE);
      ASK capButton TO SetHidden(TRUE);
      ASK capChangeVal TO SetHidden(TRUE);
      ASK capChangeText TO SetHidden(TRUE);
      IF totalObjects > 1
         counter := 0;
         FOR i:= 1 TO HIGH(buddyList)
            IF (buddyList[i] = "")
               INC(counter);
            END IF;
         END FOR;
         IF (HIGH(buddyList) > counter)
            NEW(buddyList2, 1..(HIGH(buddyList) - counter));
         ELSE
            NEW(buddyList2, 1..(HIGH(buddyList)));
         END IF;
         FOR i:= 1 TO HIGH(buddyList2)
            buddyList2[i] := buddyList[i];
         END FOR;
         ASK buddyCombo TO SetOptions(buddyList2);
         IF buddy <> ""
            ASK buddyCombo TO SetText(buddy);
         ELSE
            ASK buddyCombo TO SetText(buddyList2[1]);
         END IF;
      END IF;
      ASK buddyCombo TO Deactivate;
      IF tempNode.name = ""
         ASK nodeName TO SetText(tempNode.nodeName);
      ELSE
         ASK nodeName TO SetText(tempNode.name);
      END IF;
      IF adding
         ASK nodeInfoChk TO SetCheck(TRUE);
      END IF;   
      IF totalIn = 0
         ASK pathsReqBox TO SetValue(0.);
         ASK pathsText TO Deactivate;
         ASK pathsReqBox TO Deactivate;
         ASK coldChk TO Deactivate;
         ASK kStarText TO Deactivate;
         ASK kStarVal TO Deactivate;
         ASK coldTab TO SetHidden(TRUE); 
      ELSIF tempNode.connectIntoNum = 1
         ASK pathsReqBox TO SetValue(1.);
         ASK pathsText TO Deactivate;
         ASK pathsReqBox TO Deactivate;
         ASK coldChk TO Deactivate;
         ASK kStarText TO Deactivate;
         ASK kStarVal TO Deactivate;
         ASK coldTab TO SetHidden(TRUE); 
      ELSE
         ASK pathsReqBox TO SetValue(FLOAT(tempNode.goodPaths));
         ASK priorChk TO SetCheck(tempNode.priorityReturn);
         ASK orderChk TO SetCheck(tempNode.checkAutosFirst);
         IF tempNode.coldStandby 
            ASK coldChk TO SetCheck(TRUE);
            ASK kStarVal TO SetValue(FLOAT(tempNode.KStar));
           {ASK kStarText TO Activate;}
            ASK kStarVal TO Activate;
         ELSE
            ASK kStarVal TO SetValue(FLOAT(tempNode.goodPaths));
            ASK kStarVal TO Deactivate;  
            ASK priorChk TO Deactivate;
            ASK orderChk TO Deactivate;
            ASK coldTable TO SetHidden(TRUE); 
         END IF; 
      END IF;
      IF tempNode.usesPhasing
         ASK phaseChk TO DisplayCheck(TRUE);
      ELSE
         ASK phaseChk TO DisplayCheck(FALSE);
      END IF;
      IF totalOut < 2
         ASK flowOutChk TO Deactivate;
         ASK flowOutTable TO SetHidden(TRUE);
      ELSE
         IF NOT tempNode.anyPath
            ASK flowOutChk TO SetCheck(FALSE);
         ELSE
            ASK flowOutChk TO SetCheck(TRUE);
            ASK flowOutTable TO SetHidden(TRUE);
         END IF;
      END IF;   
      IF totalIn = 0
         ASK flowInChk TO Deactivate;
         ASK flowInTable TO SetHidden(TRUE);
      ELSE
         IF NOT tempNode.fullFlow
            ASK flowInChk TO SetCheck(FALSE);
         ELSE
            ASK flowInChk TO SetCheck(TRUE);
            ASK flowInTable TO SetHidden(TRUE);
         END IF;
      END IF;   
      IF tempNode.DependencyNum  = -2
         ASK dependRadBox TO SetSelectedButton(sysButton);
         ASK buddyCombo TO Deactivate;
      ELSIF tempNode.DependencyNum  = 0
         ASK dependRadBox TO SetSelectedButton(indepButton);
         ASK buddyCombo TO Deactivate;
      ELSIF tempNode.DependencyNum  = -1
         ASK dependRadBox TO SetSelectedButton(localButton);
         ASK buddyCombo TO Deactivate;
      ELSE
         IF totalObjects > 1
            ASK dependRadBox TO SetSelectedButton(itemButton);
            ASK buddyCombo TO SetText(buddy);
            ASK buddyCombo TO Activate;
         ELSE
            ASK dependRadBox TO SetSelectedButton(localButton);
            ASK buddyCombo TO Deactivate;
         END IF;
      END IF;
    
      IF compileType = "student"
         ASK advTab TO Deactivate;
         ASK capTab TO Deactivate;
         ASK flowGenText TO Deactivate;
      END IF;
    
      REPEAT
         validData := TRUE;
         Draw;
         ASK nodeName TO ReceiveFocus;
         button := AcceptInput();
         errors := 0;
         goodPaths := ASK pathsReqBox Value();
         IF ASK button ReferenceName = "OKButton";
            newName := nodeName.Text();
            IF ((POSITION(newName, " ") <> 0) AND (NOT(newName = "")))
               INC(errors);
               text[errors] := "'Node Name' field can't be blank or have blank spaces!     ";
               ASK nodeName TO SetText(tempNode.name);
               validData := FALSE;
            ELSIF STRLEN(newName) > 20
               INC(errors);
               text[errors] := "'Node Name' must be no greater than 20 characters!     ";
               ASK nodeName TO SetText(SUBSTR(1,20,newName));
               validData := FALSE;
            ELSIF (LOWER(newName) = "start")
               INC(errors);
               text[errors] := "The name start is a reserved node name!     ";
               ASK nodeName TO SetText("");
               validData := FALSE;
            ELSIF (LOWER(newName) = "end")
               INC(errors);
               text[errors] := "The name end is a reserved node name!     ";
               ASK nodeName TO SetText("");
               validData := FALSE;
            END IF;
            IF (goodPaths > 9999.)
               INC(errors);
               text[errors] := "Good paths must be between 1 and 9,999!     ";
               validData := FALSE;
            ELSE
               IF TRUNC(goodPaths) <> tempNode.goodPaths                  
                  IF (TRUNC(goodPaths) < 1) OR (TRUNC(goodPaths) > tempNode.connectIntoNum)
                     INC(errors);
                     text[errors] := "Good paths must be between 1 and "+INTTOSTR(tempNode.connectIntoNum)+"!     ";
                     ASK pathsReqBox TO SetValue(FLOAT(tempNode.goodPaths));
                     validData := FALSE;
                  END IF;
               END IF;
            END IF;
            IF coldChk.Checked
               init1Array[1] := 1;
               IF (goodPaths > kStarVal.Value()) {enforce: k<=k*}
                  INC(errors);
                  text[errors] := "Maximum number running (k* on Standby tab) must be greater than or equal to good paths (k on General tab)!     ";
                  IF tempNode.KStar > 0
                     ASK kStarVal TO SetValue(FLOAT(tempNode.KStar));
                  ELSE
                     ASK kStarVal TO SetValue(1.);
                  END IF;
                  validData := FALSE;
               ELSIF (kStarVal.Value() > FLOAT(tempNode.connectIntoNum)) {enforce: k*<=n}
                  INC(errors);
                  text[errors] := "Maximum number running (k* on Standby tab) must be less than or equal to total paths!     ";
                  IF tempNode.KStar > 0
                     ASK kStarVal TO SetValue(FLOAT(tempNode.KStar));
                  ELSE
                     ASK kStarVal TO SetValue(1.);
                  END IF;
                  validData := FALSE;
               ELSE
                  init1Array[4]:=TRUNC(kStarVal.Value());   {set kstar value} {chuck}
               END IF;
            ELSE
               init1Array[1] := 0;
            END IF;
            IF priorChk.Checked
               init1Array[2] := 1;
            ELSE
               init1Array[2] := 0;
            END IF;
            IF orderChk.Checked
               init1Array[3] := 1;
            ELSE
               init1Array[3] := 0;
            END IF;
            {init1Array[4] := 0;  {Dummy variable}} {chuck}
            IF phaseChk.Checked
               init1Array[5] := 1;
            ELSE
               init1Array[5] := 0;
            END IF;
            deptype := "";
            IF dependRadBox.SelectedButton = indepButton
               init1Array[6] := 0;
            ELSIF dependRadBox.SelectedButton = sysButton
               init1Array[6] := -2;
            ELSIF dependRadBox.SelectedButton = localButton
               init1Array[6] := -1;
            ELSE   
               i := POSITION(buddyCombo.Text, " -");
               init1Array[6] := STRTOINT(SUBSTR(i+2, i+11, buddyCombo.Text + "      "));
               IF (SUBSTR(i-1,i-1, buddyCombo.Text) = "r")
                  deptype := "RBDHier";
                  tempHier := ASK root Child("RBDHier", init1Array[6]);
                  init1Array[6] := tempHier.outID;
               ELSIF (SUBSTR(i-1,i-1, buddyCombo.Text) = "k")
                  deptype := "RBDBlock";
               ELSIF (SUBSTR(i-1,i-1, buddyCombo.Text) = "e")
                  deptype := "RBDNode";
               ELSIF (SUBSTR(i-1,i-1, buddyCombo.Text) = "t")
                  deptype := "RBDEvent";
               END IF;
            END IF; 
            phaseCheck := phaseChk.Checked;
         ELSE
            validData := TRUE;
            cancelled := TRUE;
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
      UNTIL validData;
      DISPOSE(text);
      IF NOT cancelled
         ASK tempNode TO SetDep(init1Array[6], deptype);
         ASK tempNode TO SetType(2);
         ASK tempNode TO SetComment(commentBox.Text());
         ASK tempNode TO SetReportNodeAnal(nodeInfoChk.Checked);
         ASK tempNode TO SetFullFlow(flowInChk.Checked);
         ASK tempNode TO SetAnyPath(flowOutChk.Checked);
         ASK tempNode TO SetNum(node.Id);
         ASK tempNode TO SetGoodPaths(TRUNC(pathsReqBox.Value));
         ASK tempNode TO SetKofN(tempNode.goodPaths, tempNode.connectIntoNum);
         IF tempNode.usesPhasing <> phaseChk.Checked
            ASK tempNode TO SetPhases(phaseChk.Checked,NILARRAY);
         END IF;
         ASK tempNode TO Init1(init1Array);
         IF nodeName.Text <> tempNode.name
            ASK tempNode TO SetName(nodeName.Text);
            ASK tempNode TO SetNum(tempNode.Id);
         END IF;
         counter := 0;
         IF NOT flowOutChk.Checked
            FOREACH tempLink IN linkGroup
               IF (tempLink.connectFRef = "RBDNode") AND (tempLink.connectFromId = tempNode.Id)
                  INC(counter);
                  ASK tempLink TO SetCapPriority(STRTOINT(flowOutTable.Text(3,counter)));    
               END IF;
            END FOREACH;
         END IF;
         counter := 0;
         IF NOT flowInChk.Checked
            FOREACH tempLink IN linkGroup
               IF (tempLink.connectTRef = "RBDNode") AND (tempLink.connectToId = tempNode.Id)
                  INC(counter);
                  IF flowInTable.Text(3,counter) = "MaxFlow"
                     tempNom := -1;
                  ELSIF flowInTable.Text(3,counter) = ("MaxFlow/"+INTTOSTR(totalIn))
                     tempNom := -2;
                  ELSE
                     tempNom := STRTOINT(flowInTable.Text(3,counter));
                  END IF;
                  IF flowInTable.Text(4,counter) = "FullFlow"
                     tempMax := -1;
                  ELSIF flowInTable.Text(4,counter) = ("FullFlow/"+INTTOSTR(totalIn))
                     tempMax := -2;
                  ELSE
                     tempMax := STRTOINT(flowInTable.Text(4,counter));
                  END IF;
                  ASK tempLink TO SetFlowInVals(tempNom,tempMax);    
               END IF;
            END FOREACH;
         END IF;      
         counter := 0;
         IF coldChk.Checked
            FOREACH tempLink IN linkGroup
               IF (tempLink.connectTRef = "RBDNode") AND (tempLink.connectToId = tempNode.Id)
                  INC(counter);
                  IF coldTable.Text(6,counter) = "N/A"
                     tempSw := -1.;
                  ELSE
                     tempSw := STRTOREAL(coldTable.Text(6,counter));
                  END IF;
                  ASK tempLink TO SetColdVals(STRTOINT(coldTable.Text(3,counter)),
                      STRTOREAL(coldTable.Text(4,counter)),
                      STRTOREAL(coldTable.Text(5,counter)),tempSw);  
               END IF;
            END FOREACH;
         END IF;   
         ASK tempNode TO Draw;
         somethingChanged := TRUE;
      END IF;
      DISPOSE(init1Array);
   END METHOD; {GetNodeInput}

   ASK METHOD BeSelected;
   VAR
      tabClicked                  : GraphicVObj;
      i, num                      : INTEGER;
      tempBlock                   : RBDBlockObj;
      leaveCold, leaveCap, capErr : BOOLEAN;
      text                        : strArray;
   BEGIN
      leaveCold := TRUE;
      leaveCap := TRUE;
      tabClicked := LastPicked;
      NEW(text, 1..10);
      errors := 0;
      CASE tabClicked.Id
         WHEN 100: {General Tab}
            leaveCold := FALSE;
            leaveCap := FALSE;
            lastClicked := ASK tabClicked LastPicked;
         WHEN 200: {Cold Tab}
            leaveCap := FALSE;
            lastClicked := ASK tabClicked LastPicked;
            CASE lastClicked.Id
               WHEN 201: {Cold Standby Check}
                  IF coldChk.Checked
                     {ASK kStarText TO Activate;}
                     ASK kStarVal TO Activate;
                     ASK priorChk TO Activate;
                     ASK orderChk TO Activate;
                     ASK coldTable TO SetHidden(FALSE);
                  ELSE
                     {ASK kStarText TO Deactivate;}
                     ASK kStarVal TO Deactivate;
                     ASK priorChk TO Deactivate;
                     ASK orderChk TO Deactivate;
                     ASK coldTable TO SetHidden(TRUE);
                  END IF; 
               WHEN 206:
                  IF coldButton.Visible AND (coldList1[1] <> "")
                     IF (coldTable.SelectedRow > 0) AND (coldTable.SelectedRow <= connectIn)
                        IF coldTable.SelectedColumn <= 3
                           ASK coldTable TO SetSelected(3,coldTable.SelectedRow);
                           ASK coldChangeTxt1 TO SetLabel(coldTable.Text(2,coldTable.SelectedRow)+" "+coldTable.Text(1,coldTable.SelectedRow));
                           ASK coldChangeTxt2 TO SetLabel("Priority");
                           IF coldChangeVal.Text = "No Manual Switching"
                           ELSIF(STRTOREAL(coldChangeVal.Text) < 1.) OR (STRTOREAL(coldChangeVal.Text) > 99.)
                              {INC(errors);
                              text[errors] := "Priority must be between 1 and 99!     ";
                              ASK coldChangeVal TO SetText("50");}
                              coldList2[1] := coldTable.Text(3,coldTable.SelectedRow);
                              coldList1[1] := coldTable.Text(3,coldTable.SelectedRow) ;
                              ASK coldChangeVal TO SetOptions(coldList1);
                              ASK coldChangeVal TO SetText(coldTable.Text(3,coldTable.SelectedRow));
                              ASK coldChangeTxt1 TO SetLabel(coldTable.Text(2,coldTable.SelectedRow)+" "+coldTable.Text(1,coldTable.SelectedRow));
                              ASK coldChangeTxt2 TO SetLabel("Priority");
                           ELSE
                              coldList2[1] := coldChangeVal.Text;
                              coldList1[1] := coldChangeVal.Text;
                              ASK coldChangeVal TO SetOptions(coldList1);
                              ASK coldTable TO SetText(INTTOSTR(ROUND(STRTOREAL(coldChangeVal.Text))),3,coldTable.SelectedRow);
                           END IF;
                        ELSIF coldTable.SelectedColumn = 4
                           ASK coldChangeTxt1 TO SetLabel(coldTable.Text(2,coldTable.SelectedRow)+" "+coldTable.Text(1,coldTable.SelectedRow));
                           ASK coldChangeTxt2 TO SetLabel("Auto switch probability");
                           IF coldChangeVal.Text = "No Manual Switching"
                           ELSIF (STRTOREAL(coldChangeVal.Text) <= 0.) OR (STRTOREAL(coldChangeVal.Text) > 1.)
                              coldList2[1] := coldTable.Text(4,coldTable.SelectedRow);
                              coldList1[1] := coldTable.Text(4,coldTable.SelectedRow);
                              ASK coldChangeVal TO SetOptions(coldList1);
                              ASK coldChangeVal TO SetText(coldTable.Text(4,coldTable.SelectedRow));
                              ASK coldChangeTxt1 TO SetLabel("Node "+coldTable.Text(1,coldTable.SelectedRow));
                              ASK coldChangeTxt2 TO SetLabel("Auto switch probability");
                              {INC(errors);
                              text[errors] := "Auto switch probablity must be greater than 0 and less than or equal to 1!     ";  }
                           ELSE
                              coldList2[1] := coldChangeVal.Text;
                              coldList1[1] := coldChangeVal.Text;
                              ASK coldChangeVal TO SetOptions(coldList1);
                              ASK coldTable TO SetText(SUBSTR(1,8,coldChangeVal.Text),4,coldTable.SelectedRow);
                           END IF;
                        ELSIF coldTable.SelectedColumn = 5
                           ASK coldChangeTxt1 TO SetLabel(coldTable.Text(2,coldTable.SelectedRow)+" "+coldTable.Text(1,coldTable.SelectedRow));
                           ASK coldChangeTxt2 TO SetLabel("Auto switch time");
                           IF coldChangeVal.Text = "No Manual Switching"
                           ELSIF (STRTOREAL(coldChangeVal.Text) < 0.) OR (STRTOREAL(coldChangeVal.Text) > 999999999.)
                              INC(errors);
                              text[errors] := "Auto switch time must be between 0 and 999,999,999!     ";
                              ASK coldChangeVal TO SetOptions(coldList1);
                           ELSE
                              coldList2[1] := coldChangeVal.Text;
                              coldList1[1] := coldChangeVal.Text;
                              ASK coldChangeVal TO SetOptions(coldList1);
                              ASK coldTable TO SetText(REALTOSTR(STRTOREAL(coldChangeVal.Text)),5,coldTable.SelectedRow);
                           END IF;
                        ELSE
                           ASK coldChangeTxt1 TO SetLabel(coldTable.Text(2,coldTable.SelectedRow)+" "+coldTable.Text(1,coldTable.SelectedRow));
                           ASK coldChangeTxt2 TO SetLabel("Manual switch time");
                           IF coldChangeVal.Text = "No Manual Switching"
                              ASK coldTable TO SetText("N/A",6,coldTable.SelectedRow);
                           ELSIF ((STRTOREAL(coldChangeVal.Text) < 0.) OR (STRTOREAL(coldChangeVal.Text) > 999999999.))
                              INC(errors);
                              text[errors] := "Manual switch time must be between 0 and 999,999,999!     ";
                              ASK coldChangeVal TO SetOptions(coldList2);
                           ELSE
                              coldList2[1] := coldChangeVal.Text;
                              coldList1[1] := coldChangeVal.Text;
                              ASK coldChangeVal TO SetOptions(coldList2);
                              ASK coldTable TO SetText(REALTOSTR(STRTOREAL(coldChangeVal.Text)),6,coldTable.SelectedRow);
                           END IF;
                        END IF;
                     END IF;
                  ELSE
                     IF (coldTable.SelectedRow > 0) AND (coldTable.SelectedRow <= connectIn)
                        ASK coldButton TO SetHidden(FALSE);
                        ASK coldChangeVal TO SetHidden(FALSE);
                        ASK coldChangeTxt1 TO SetHidden(FALSE);
                        ASK coldChangeTxt2 TO SetHidden(FALSE);
                        IF coldTable.SelectedColumn <=3
                           ASK coldTable TO SetSelected(3,coldTable.SelectedRow);
                           ASK coldChangeVal TO SetText(coldTable.Text(3,coldTable.SelectedRow));
                           ASK coldChangeTxt1 TO SetLabel("Node "+coldTable.Text(1,coldTable.SelectedRow));
                           ASK coldChangeTxt2 TO SetLabel("Priority");
                           ASK coldChangeVal TO SetOptions(coldList1);
                        ELSE
                           ASK coldChangeVal TO SetText(coldTable.Text(coldTable.SelectedColumn,coldTable.SelectedRow));
                           IF coldTable.SelectedColumn = 4
                              ASK coldChangeTxt1 TO SetLabel(coldTable.Text(2,coldTable.SelectedRow)+" "+coldTable.Text(1,coldTable.SelectedRow));
                              ASK coldChangeTxt2 TO SetLabel("Auto switch probability");
                              ASK coldChangeVal TO SetOptions(coldList1);
                           ELSIF coldTable.SelectedColumn = 5
                              ASK coldChangeTxt1 TO SetLabel(coldTable.Text(2,coldTable.SelectedRow)+" "+coldTable.Text(1,coldTable.SelectedRow));
                              ASK coldChangeTxt2 TO SetLabel("Auto switch time");
                              ASK coldChangeVal TO SetOptions(coldList1);
                           ELSE
                              ASK coldChangeTxt1 TO SetLabel(coldTable.Text(2,coldTable.SelectedRow)+" "+coldTable.Text(1,coldTable.SelectedRow));
                              ASK coldChangeTxt2 TO SetLabel("Manual switch time");
                              ASK coldChangeVal TO SetOptions(coldList2);
                           END IF;
                        END IF;
                     END IF;
                  END IF;
               WHEN 210: {Accept Changes}
                  IF coldTable.SelectedRow <> 0
                     IF coldTable.SelectedColumn = 3
                        IF(STRTOREAL(coldChangeVal.Text) < 1.) OR (STRTOREAL(coldChangeVal.Text) > 99.)
                           INC(errors);
                           text[errors] := "Priority must be between 1 and 99!     ";
                           ASK coldChangeVal TO SetText("50");
                        ELSE
                           ASK coldTable TO SetText(INTTOSTR(ROUND(STRTOREAL(coldChangeVal.Text))),3,coldTable.SelectedRow);
                           coldList2[1] := coldChangeVal.Text;
                           coldList1[1] := coldChangeVal.Text;
                           ASK coldChangeVal TO SetOptions(coldList1);
                           ASK coldChangeVal TO SetText(coldChangeVal.Text);
                        END IF;
                     ELSIF coldTable.SelectedColumn = 4
                        IF (STRTOREAL(coldChangeVal.Text) <= 0.) OR (STRTOREAL(coldChangeVal.Text) > 1.)
                           INC(errors);
                           text[errors] := "Auto switch probablity must be greater than 0 and less than or equal to 1!     ";
                           ASK coldChangeVal TO SetText("1");
                        ELSE
                           ASK coldTable TO SetText(SUBSTR(1,8,coldChangeVal.Text),4,coldTable.SelectedRow);
                           coldList2[1] := coldChangeVal.Text;
                           coldList1[1] := coldChangeVal.Text;
                           ASK coldChangeVal TO SetOptions(coldList1);
                           ASK coldChangeVal TO SetText(coldChangeVal.Text);
                        END IF;
                     ELSIF coldTable.SelectedColumn = 5
                        IF (STRTOREAL(coldChangeVal.Text) < 0.) OR (STRTOREAL(coldChangeVal.Text) > 999999999.)
                           INC(errors);
                           text[errors] := "Auto switch time must be between 0 and 999,999,999!     ";
                           ASK coldChangeVal TO SetText("0.00");
                        ELSE
                           ASK coldTable TO SetText(SUBSTR(1,8,REALTOSTR(STRTOREAL(coldChangeVal.Text))),5,coldTable.SelectedRow);
                           coldList2[1] := coldChangeVal.Text;
                           coldList1[1] := coldChangeVal.Text;
                           ASK coldChangeVal TO SetOptions(coldList1);
                           ASK coldChangeVal TO SetText(coldChangeVal.Text);
                        END IF;
                     ELSE
                        IF coldChangeVal.Text = "No Manual Switching"
                           ASK coldTable TO SetText("N/A",6,coldTable.SelectedRow);
                        ELSIF ((STRTOREAL(coldChangeVal.Text) < 0.) OR (STRTOREAL(coldChangeVal.Text) > 999999999.))
                           INC(errors);
                           text[errors] := "Manual switch time must be between 0 and 999,999,999!     ";
                           ASK coldChangeVal TO SetText("0.00");
                        ELSE
                           ASK coldTable TO SetText(SUBSTR(1,8,REALTOSTR(STRTOREAL(coldChangeVal.Text))),6,coldTable.SelectedRow);
                           coldList2[1] := coldChangeVal.Text;
                           coldList1[1] := coldChangeVal.Text;
                           ASK coldChangeVal TO SetOptions(coldList2);
                           ASK coldChangeVal TO SetText(coldChangeVal.Text);
                        END IF;
                     END IF;
                  END IF;
               OTHERWISE
            END CASE;         
         WHEN 300: {Capacity Tab}
            leaveCold := FALSE;
            lastClicked := ASK tabClicked LastPicked;
            CASE lastClicked.Id
               WHEN 303:
                  IF flowInChk.Checked
                     ASK flowInTable TO SetHidden(TRUE);
                  ELSE
                     ASK flowInTable TO SetHidden(FALSE);
                  END IF;    
               WHEN 304:
                  IF flowOutChk.Checked
                     ASK flowOutTable TO SetHidden(TRUE);
                  ELSE
                     ASK flowOutTable TO SetHidden(FALSE);
                  END IF;    
               WHEN 305:
                  tableUp := "In";
                  IF capButton.Visible AND (capList1[1] <> "")
                     IF (flowInTable.SelectedRow > 0) AND (flowInTable.SelectedRow <= connectIn)
                        IF flowInTable.SelectedColumn <= 3
                           ASK flowInTable TO SetSelected(3,flowInTable.SelectedRow);
                           IF SUBSTR(1,4,capChangeVal.Text) = "Full"
                              ASK flowInTable TO SetSelected(3,flowInTable.SelectedRow);
                              IF (SUBSTR(1,3,flowInTable.Text(3,flowInTable.SelectedRow)) <> "Max") AND
                                 (SUBSTR(1,3,flowInTable.Text(3,flowInTable.SelectedRow)) <> "Ful")
                                 capList1[1] := flowInTable.Text(3,flowInTable.SelectedRow);
                                 capList2[1] := flowInTable.Text(3,flowInTable.SelectedRow);
                                 capList3[1] := flowInTable.Text(3,flowInTable.SelectedRow);
                              END IF;
                              ASK capChangeVal TO SetOptions(capList2);
                              ASK capChangeVal TO SetText(flowInTable.Text(3,flowInTable.SelectedRow));
                              ASK capChangeText TO SetLabel(flowInTable.Text(1,flowInTable.SelectedRow)+" Nominal Flow");
                           ELSIF SUBSTR(1,3,capChangeVal.Text) = "Max"
                              ASK flowInTable TO SetText(capChangeVal.Text,3,flowInTable.SelectedRow);
                              ASK capChangeText TO SetLabel(flowInTable.Text(1,flowInTable.SelectedRow)+" Nominal Flow");
                              ASK capChangeVal TO SetOptions(capList2);
                              ASK capChangeVal TO SetText(flowInTable.Text(3,flowInTable.SelectedRow));
                           ELSE
                              IF (flowInTable.Text(4,flowInTable.SelectedRow) = "FullFlow") 
                                 IF (STRTOREAL(capChangeVal.Text) > FLOAT(flowGenerated))
                                    INC(errors);
                                    text[errors] := "Nominal flow must be less than or equal to the max flow! ";
                                 ELSE
                                    capList1[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                                    capList2[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                                    capList3[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                                    ASK capChangeVal TO SetOptions(capList2);
                                    ASK flowInTable TO SetText(INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text))),3,flowInTable.SelectedRow);
                                    ASK capChangeText TO SetLabel(flowInTable.Text(1,flowInTable.SelectedRow)+" Nominal Flow");
                                    ASK capChangeVal TO SetOptions(capList2);
                                 END IF;
                              ELSIF (flowInTable.Text(4,flowInTable.SelectedRow) = ("FullFlow/"+INTTOSTR(totalIn)))
                                 IF (STRTOREAL(capChangeVal.Text) > FLOAT(flowGenerated)/FLOAT(totalOut))
                                    INC(errors);
                                    text[errors] := "Nominal flow must be less than or equal to the max flow!     ";
                                 ELSE
                                    capList1[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                                    capList2[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                                    capList3[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                                    ASK capChangeVal TO SetOptions(capList2);
                                    ASK flowInTable TO SetText(INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text))),3,flowInTable.SelectedRow);
                                    ASK capChangeText TO SetLabel(flowInTable.Text(1,flowInTable.SelectedRow)+" Nominal Flow");
                                    ASK capChangeVal TO SetOptions(capList2);
                                 END IF;
                              ELSIF STRTOREAL(capChangeVal.Text) > STRTOREAL(flowInTable.Text(4,flowInTable.SelectedRow))
                                 INC(errors);
                                 text[errors] := "Nominal flow must be less than or equal to the max flow!     ";
                              ELSIF (STRTOREAL(capChangeVal.Text) < 0.) {OR (STRTOREAL(capChangeVal.Text) > ??))}
                                 INC(errors);
                                 text[errors] := "Nominal flow must be greater than 0!     ";
                              ELSE
                                 capList1[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                                 capList2[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                                 capList3[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                                 ASK flowInTable TO SetText(INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text))),3,flowInTable.SelectedRow);
                                 ASK capChangeText TO SetLabel(flowInTable.Text(1,flowInTable.SelectedRow)+" Nominal Flow");
                                 ASK capChangeVal TO SetOptions(capList2);
                              END IF;
                           END IF;
                        ELSE
                           IF SUBSTR(1,3,capChangeVal.Text) = "Max"
                              IF (SUBSTR(1,3,flowInTable.Text(4,flowInTable.SelectedRow)) <> "Max") AND
                                 (SUBSTR(1,3,flowInTable.Text(4,flowInTable.SelectedRow)) <> "Ful")
                                 capList1[1] := flowInTable.Text(4,flowInTable.SelectedRow);
                                 capList2[1] := flowInTable.Text(4,flowInTable.SelectedRow);
                                 capList3[1] := flowInTable.Text(4,flowInTable.SelectedRow);
                              END IF;
                              ASK capChangeVal TO SetOptions(capList3);
                              ASK capChangeVal TO SetText(flowInTable.Text(4,flowInTable.SelectedRow));
                              ASK capChangeText TO SetLabel(flowInTable.Text(1,flowInTable.SelectedRow)+" MaxFlow");
                           ELSIF SUBSTR(1,4,capChangeVal.Text) = "Full"
                              ASK capChangeText TO SetLabel(flowInTable.Text(1,flowInTable.SelectedRow)+" MaxFlow");
                              ASK flowInTable TO SetText(capChangeVal.Text,4,flowInTable.SelectedRow);
                           ELSE
                              IF (flowInTable.Text(3,flowInTable.SelectedRow) <> "MaxFlow") AND (flowInTable.Text(3,flowInTable.SelectedRow) <> "MaxFlow/"+INTTOSTR(totalIn))
                                 AND (STRTOREAL(capChangeVal.Text) < STRTOREAL(flowInTable.Text(3,flowInTable.SelectedRow)))
                                 INC(errors);
                                 text[errors] := "The max flow must be greater than the nominal flow!     ";
                              ELSIF STRTOREAL(capChangeVal.Text) < 0.
                                 INC(errors);
                                 text[errors] := "The max flow must be greater than 0!     ";
                              ELSE
                                 capList1[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                                 capList2[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                                 capList3[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                                 ASK capChangeText TO SetLabel(flowInTable.Text(1,flowInTable.SelectedRow)+" MaxFlow");
                                 ASK capChangeVal TO SetOptions(capList3);
                                 ASK flowInTable TO SetText(INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text))),4,flowInTable.SelectedRow);
                              END IF;
                           END IF;
                        END IF;
                     END IF;
                  ELSE
                     IF (flowInTable.SelectedRow > 0) AND (flowInTable.SelectedRow <= connectIn)
                        ASK capButton TO SetHidden(FALSE);
                        ASK capChangeVal TO SetHidden(FALSE);
                        ASK capChangeText TO SetHidden(FALSE);
                        IF flowInTable.SelectedColumn <=3
                           ASK flowInTable TO SetSelected(3,flowInTable.SelectedRow);
                           IF (SUBSTR(1,3,flowInTable.Text(3,flowInTable.SelectedRow)) <> "Max") AND
                              (SUBSTR(1,3,flowInTable.Text(3,flowInTable.SelectedRow)) <> "Ful")
                              capList1[1] := flowInTable.Text(3,flowInTable.SelectedRow);
                              capList2[1] := flowInTable.Text(3,flowInTable.SelectedRow);
                              capList3[1] := flowInTable.Text(3,flowInTable.SelectedRow);
                           END IF;
                           ASK capChangeVal TO SetOptions(capList2);
                           ASK capChangeVal TO SetText(flowInTable.Text(3,flowInTable.SelectedRow));
                           ASK capChangeText TO SetLabel(flowInTable.Text(1,flowInTable.SelectedRow)+" Nominal Flow");
                        ELSE
                           IF (SUBSTR(1,3,flowInTable.Text(4,flowInTable.SelectedRow)) <> "Max") AND
                              (SUBSTR(1,3,flowInTable.Text(4,flowInTable.SelectedRow)) <> "Ful")
                              capList1[1] := flowInTable.Text(4,flowInTable.SelectedRow);
                              capList2[1] := flowInTable.Text(4,flowInTable.SelectedRow);
                              capList3[1] := flowInTable.Text(4,flowInTable.SelectedRow);
                           END IF;
                           ASK capChangeVal TO SetOptions(capList3);
                           ASK capChangeVal TO SetText(flowInTable.Text(4,flowInTable.SelectedRow));
                           ASK capChangeText TO SetLabel(flowInTable.Text(1,flowInTable.SelectedRow)+" MaxFlow");
                        END IF;
                     END IF;
                  END IF;
               WHEN 306:
                  tableUp := "Out";
                  IF capButton.Visible AND (capList1[1] <> "")
                     IF (flowOutTable.SelectedRow > 0) AND (flowOutTable.SelectedRow <= connectOut)
                        ASK flowOutTable TO SetSelected(3,flowOutTable.SelectedRow);
                        ASK capChangeText TO SetLabel(flowOutTable.Text(1,flowOutTable.SelectedRow)+" Priority");
                        IF (SUBSTR(1,4,capChangeVal.Text) = "Full") OR (SUBSTR(1,3,capChangeVal.Text) = "Max")
                           capList1[1] := flowOutTable.Text(3,flowOutTable.SelectedRow);
                           capList2[1] := flowOutTable.Text(3,flowOutTable.SelectedRow);
                           capList3[1] := flowOutTable.Text(3,flowOutTable.SelectedRow);
                           ASK capChangeVal TO SetText(flowOutTable.Text(3,flowOutTable.SelectedRow));
                           ASK capChangeVal TO SetOptions(capList1);
                        ELSE
                           IF (STRTOREAL(capChangeVal.Text) < 1.) OR (STRTOREAL(capChangeVal.Text) > 99.)
                              capList1[1] := flowOutTable.Text(3,flowOutTable.SelectedRow);
                              capList2[1] := flowOutTable.Text(3,flowOutTable.SelectedRow);
                              capList3[1] := flowOutTable.Text(3,flowOutTable.SelectedRow);
                              ASK capChangeVal TO SetText(flowOutTable.Text(3,flowOutTable.SelectedRow));
                              ASK capChangeVal TO SetOptions(capList1);
                           ELSE
                              capList1[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                              capList2[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                              capList3[1] := INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text)));
                              ASK flowOutTable TO SetText(INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text))),3,flowOutTable.SelectedRow);
                           END IF;
                        END IF;
                     END IF;
                  ELSE
                     IF (flowOutTable.SelectedRow > 0) AND (flowOutTable.SelectedRow <= connectOut)
                        ASK capButton TO SetHidden(FALSE);
                        ASK capChangeVal TO SetHidden(FALSE);
                        ASK capChangeText TO SetHidden(FALSE);
                        ASK flowOutTable TO SetSelected(3,flowOutTable.SelectedRow);
                        ASK capChangeVal TO SetText(flowOutTable.Text(3,flowOutTable.SelectedRow));
                        ASK capChangeText TO SetLabel(flowOutTable.Text(1,flowOutTable.SelectedRow)+" Priority");
                        ASK capChangeVal TO SetOptions(capList1);
                    END IF;
                  END IF;
               WHEN 308: {Accept Changes}
                  capErr := FALSE;
                  IF tableUp = "Out"
                     IF flowOutTable.SelectedRow > 0
                        IF (STRTOREAL(capChangeVal.Text) < 1.) OR (STRTOREAL(capChangeVal.Text) > 99.)
                           INC(errors);
                           text[errors] := "Priority must be between 1 and 99!     ";
                           ASK capChangeVal TO SetText("50.0");
                        ELSE
                           ASK flowOutTable TO SetText(INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text))),3,flowOutTable.SelectedRow);
                           capList1[1] := capChangeVal.Text;
                           capList2[1] := capChangeVal.Text;
                           capList3[1] := capChangeVal.Text;
                           ASK capChangeVal TO SetText(capChangeVal.Text);
                        END IF;
                     END IF;
                  ELSE
                     IF flowInTable.SelectedRow > 0
                        IF flowInTable.SelectedColumn = 3
                           IF (flowInTable.Text(4,flowInTable.SelectedRow) = "FullFlow") 
                              IF (STRTOREAL(capChangeVal.Text) > FLOAT(flowGenerated))
                                 INC(errors);
                                 text[errors] := "Nominal flow must be less than or equal to the max flow!     ";
                                 ASK capChangeVal TO SetText(flowInTable.Text(4,flowInTable.SelectedRow));
                                 capErr := TRUE;
                              END IF;
                           ELSIF (flowInTable.Text(4,flowInTable.SelectedRow) = ("FullFlow/"+INTTOSTR(totalIn)))
                              IF (STRTOREAL(capChangeVal.Text) > FLOAT(flowGenerated)/FLOAT(totalIn))
                                 INC(errors);
                                 text[errors] := "Nominal flow must be less than or equal to the max flow!     ";
                                 ASK capChangeVal TO SetText(flowInTable.Text(4,flowInTable.SelectedRow));
                                 capErr := TRUE;
                              END IF;
                           ELSIF STRTOREAL(capChangeVal.Text) > STRTOREAL(flowInTable.Text(4,flowInTable.SelectedRow))
                              INC(errors);
                              text[errors] := "Nominal flow must be less than or equal to the max flow!     ";
                              ASK capChangeVal TO SetText(flowInTable.Text(4,flowInTable.SelectedRow));
                              capErr := TRUE;
                           ELSIF STRTOREAL(capChangeVal.Text) < 0.
                              INC(errors);
                              text[errors] := "Nominal flow must be greater than 0!     ";
                              ASK capChangeVal TO SetText("1.0");
                              capErr := TRUE;
                           END IF;
                           IF NOT capErr
                              IF SUBSTR(1,3,capChangeVal.Text) = "Max"
                                 ASK flowInTable TO SetText(capChangeVal.Text,3,flowInTable.SelectedRow);
                              ELSIF SUBSTR(1,3,capChangeVal.Text) <> "Ful"
                                 ASK flowInTable TO SetText(INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text))),3,flowInTable.SelectedRow);
                                 capList1[1] := capChangeVal.Text;
                                 capList2[1] := capChangeVal.Text;
                                 capList3[1] := capChangeVal.Text;
                                 ASK capChangeVal TO SetOptions(capList3);
                                 ASK capChangeVal TO SetText(capChangeVal.Text);
                              END IF;
                           END IF;
                        ELSE
                           IF SUBSTR(1,3,capChangeVal.Text) = "Ful"
                              ASK flowInTable TO SetText(capChangeVal.Text,4,flowInTable.SelectedRow);
                           ELSIF (flowInTable.Text(3,flowInTable.SelectedRow) <> "MaxFlow") AND (flowInTable.Text(3,flowInTable.SelectedRow) <> "MaxFlow/"+INTTOSTR(totalIn))
                              AND (STRTOREAL(capChangeVal.Text) < STRTOREAL(flowInTable.Text(3,flowInTable.SelectedRow)))
                              INC(errors);
                              text[errors] := "The max flow must be greater than the nominal flow!     ";
                              ASK capChangeVal TO SetText(flowInTable.Text(3,flowInTable.SelectedRow));
                           ELSIF (STRTOREAL(capChangeVal.Text) > FLOAT(flowGenerated)) AND (flowInTable.Text(3,flowInTable.SelectedRow) <> "MaxFlow")
                              AND (flowInTable.Text(3,flowInTable.SelectedRow) <> "MaxFlow/"+INTTOSTR(totalIn))
                              INC(errors);
                              text[errors] := "The max flow cannot be greater than the flow generated!     ";
                              ASK capChangeVal TO SetText(INTTOSTR(flowGenerated));
                           ELSIF ((STRTOREAL(capChangeVal.Text) < 0.) OR (STRTOREAL(capChangeVal.Text) > 999999999.))
                              INC(errors);
                              text[errors] := "The max flow must be between 0 and 999,999,999!     ";
                              ASK capChangeVal TO SetText("1.0");
                           ELSE
                              IF SUBSTR(1,3,capChangeVal.Text) <> "Max"
                                 ASK flowInTable TO SetText(INTTOSTR(ROUND(STRTOREAL(capChangeVal.Text))),4,flowInTable.SelectedRow);
                                 capList1[1] := capChangeVal.Text;
                                 capList2[1] := capChangeVal.Text;
                                 capList3[1] := capChangeVal.Text;
                                 ASK capChangeVal TO SetOptions(capList3);
                                 ASK capChangeVal TO SetText(capChangeVal.Text);
                              END IF;
                           END IF;
                        END IF;
                     END IF;
                  END IF;
               OTHERWISE
            END CASE;         
         WHEN 400: {Dependency Tab}
            leaveCold := FALSE;
            leaveCap := FALSE;
            lastClicked := ASK dependTab LastPicked;
            IF lastClicked.Id = 401
               IF dependRadBox.SelectedButton.Id = 4013
                  ASK dependRadBox TO SetSelectedButton(sysButton);
                  ASK buddyCombo TO Deactivate;
               ELSIF dependRadBox.SelectedButton.Id = 4011
                  ASK dependRadBox TO SetSelectedButton(indepButton);
                  ASK buddyCombo TO Deactivate;
               ELSIF dependRadBox.SelectedButton.Id = 4012
                  ASK dependRadBox TO SetSelectedButton(localButton);
                  ASK buddyCombo TO Deactivate;
               ELSE
                  ASK dependRadBox TO SetSelectedButton(itemButton);
                  ASK buddyCombo TO Activate;
               END IF;
            END IF;
         OTHERWISE
      END CASE;
      IF NOT leaveCold
         ASK coldButton TO SetHidden(TRUE);
         ASK coldChangeVal TO SetHidden(TRUE);
         ASK coldChangeTxt1 TO SetHidden(TRUE);
         ASK coldChangeTxt2 TO SetHidden(TRUE);
      END IF;
      IF NOT leaveCap
         ASK capButton TO SetHidden(TRUE);
         ASK capChangeVal TO SetHidden(TRUE);
         ASK capChangeText TO SetHidden(TRUE);
      END IF;
      Draw;
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
      INHERITED BeSelected;
   END METHOD; {BeSelected}
END OBJECT; {NodeBoxObj}

OBJECT PhaseBoxObj;
   ASK METHOD InitSelf;
   VAR
      i, j, tempPhaseNum, numParams            : INTEGER;
      tempBlock    : RBDBlockObj;
      tempEvent    : RBDEventObj;
      tempNode     : RBDNodeObj;
      tempHier     : RBDHierObj;
      phase        : PhaseObj;
      tempString   : STRING;
      
   BEGIN
      phaseBlocks     :=0;
      phaseNodes      :=0;
      phaseEvents     :=0;
      phaseHiers      :=0;
      dTable          := Descendant("DefineTable", 101);
      phaseNum        := Descendant("PhaseNum", 102);
      phaseName       := Descendant("PhaseName", 110);
      phaseDistTable  := Descendant("PhaseDistTable", 111);
      unitsLabel      := Descendant("UnitsLabel", 112);
      missionChkBox   := Descendant("MissionChkBox", 113);
      insertButton    := Descendant("InsertButton", 114);
      startTime       := Descendant("StartTime", 103);
      endTimeBox      := Descendant("EndTimeBox", 104);
      editButton      := Descendant("EditButton", 105);
      applyButton     := Descendant("ApplyButton", 106);
      deleteButton    := Descendant("DeleteButton", 107);
      clrAllButton    := Descendant("ClearButton", 108);
      bTab            := Descendant("BlockTab", 200);
      b1Butt          := Descendant("BMarkRow", 203);
      b2Butt          := Descendant("BResetRow", 204);
      b3Butt          := Descendant("BMarkCol", 205);
      b4Butt          := Descendant("BResetCol", 206);
      nTab            := Descendant("NodeTab", 300);
      n4Butt          := Descendant("ResetRow", 306);
      eTab            := Descendant("EventTab", 400);
      e1Butt          := Descendant("EMarkRow", 402);
      e2Butt          := Descendant("EResetRow", 403);
      e3Butt          := Descendant("EMarkCol", 404);
      e4Butt          := Descendant("EResetCol", 405);
      hTab            := Descendant("HierTab", 500);
      h1Butt          := Descendant("HMarkRow", 503);
      h2Butt          := Descendant("HResetRow", 505);
      h3Butt          := Descendant("HMarkCol", 504);
      h4Butt          := Descendant("HResetCol", 506);
      okButton        := Descendant("OKButton", 90);
   
      pTextBox   := Descendant("pTextBox", 410);
      pValBox    := Descendant("pValBox", 411);
      ASK unitsLabel TO SetLabel(systemUnits);
      NEW(phaseParamsArray, 1..999);
      FOR i:=1 TO 999
         NEW(phaseParamsArray[i], 1..3);
      END FOR;
      NEW(phaseDistArray, 1..999);
      
      { Default Phase Values }
      tempPhaseDist := 19;
      NEW(tempPhaseParams, 1..1);
      tempPhaseParams[1] := 100.;
      MakeDistString(tempPhaseDist, tempPhaseParams, tempString);
      ASK phaseDistTable TO SetText(tempString, 1, 0);
   
      { Phase name tags }
      BPhaseLabel := Descendant("BPhaseLabel", 0);
      ASK BPhaseLabel TO SetHidden(TRUE);
      NPhaseLabel := Descendant("NPhaseLabel", 0);
      ASK NPhaseLabel TO SetHidden(TRUE);
      EPhaseLabel := Descendant("EPhaseLabel", 0);
      ASK EPhaseLabel TO SetHidden(TRUE);
      HPhaseLabel := Descendant("HPhaseLabel", 0);
      ASK HPhaseLabel TO SetHidden(TRUE);
      
      tempTotalPhases := activePhases;
      tempOldPhases   := activePhases;
      tablesFull := 0;
      IF activePhases = 0
         Draw;
         ASK bTab TO Deactivate;
         ASK nTab TO Deactivate;
         ASK eTab TO Deactivate;
         ASK hTab TO Deactivate;
      END IF;
      IF NOT printPhase
         Draw;
         ASK n4Butt TO Deactivate;
         ASK b1Butt TO Deactivate;
         ASK b2Butt TO Deactivate;
         ASK b3Butt TO Deactivate;
         ASK b4Butt TO Deactivate;
         ASK e1Butt TO Deactivate;
         ASK e2Butt TO Deactivate;
         ASK e3Butt TO Deactivate;
         ASK e4Butt TO Deactivate;
         ASK h1Butt TO Deactivate;
         ASK h2Butt TO Deactivate;
         ASK h3Butt TO Deactivate;
         ASK h4Butt TO Deactivate;
      END IF;
      FOREACH tempBlock IN blockGroup
         IF tempBlock.usesPhasing
            INC(phaseBlocks);
         END IF;
      END FOREACH;
      FOREACH tempEvent IN eventGroup
         IF tempEvent.usesPhasing
            INC(phaseEvents);
         END IF;
      END FOREACH;
      FOREACH tempNode IN nodeGroup
         IF ((tempNode.typeNode <> 5) AND (tempNode.usesPhasing))
            INC(phaseNodes);
         END IF;
      END FOREACH;  
      FOREACH tempHier IN hierGroup
         IF tempHier.usesPhasing
            INC(phaseHiers);
         END IF;
      END FOREACH;
      
      ASK phaseDistTable TO SetSize(1,0);
      ASK phaseDistTable TO SetVisibleSize(23,1);
      
      IF activePhases > 0
         NEW(axedPhases,1..activePhases);
         {Fill in phase table}
         IF activePhases > 13
            ASK dTable TO SetSize(4, activePhases+1);
            ASK dTable TO SetVisibleSize(57,15);
         ELSE
            ASK dTable TO SetSize(4, 14);
            ASK dTable TO SetVisibleSize(54,15);
         END IF;
         FOR i := 1 TO activePhases
            {Get phase and fill in the table with it's values}
            phase := phaseObjArray[i];
            ASK dTable TO SetText(phase.phaseName, 1, i);
            IF phase.ID < 10
               tempString := "00" + INTTOSTR(phase.ID);
            ELSIF phase.ID < 100
               tempString := "0" + INTTOSTR(phase.ID);
            ELSE
               tempString := INTTOSTR(phase.ID);
            END IF;
            ASK dTable TO SetText(tempString, 2, i);
            MakeDistString(phase.Dist, phase.Params, tempString);
            ASK dTable TO SetText(tempString,3,i);
            GetNumParams(phase.Dist, numParams);
            phaseDistArray[i] := phase.Dist;
            FOR j:=1 TO numParams
               phaseParamsArray[i,j] := phase.Params[j];
            END FOR;
            IF phase.mission = TRUE;
               ASK dTable TO SetText("Yes", 4, i);
            ELSE
               ASK dTable TO SetText("No", 4, i);
            END IF;
         END FOR;
      ELSE 
         ASK clrAllButton TO Deactivate;
      END IF;
      ShowTabs;
      {Set up corner info for first phase if at least one phase defined}
      IF activePhases > 0
         tempPhaseNum := STRTOINT(dTable.Text(2,1));
         IF tempPhaseNum < 10
            ASK phaseNum TO SetLabel(" Phase # 00" + INTTOSTR(tempPhaseNum) + " ");
         ELSIF tempPhaseNum < 100
            ASK phaseNum TO SetLabel(" Phase # 0" + INTTOSTR(tempPhaseNum) + " ");
         ELSE
            ASK phaseNum TO SetLabel(" Phase # " + INTTOSTR(tempPhaseNum) + " ");
         END IF;
         ASK phaseName TO SetText(dTable.Text(1, 1));
         ASK phaseDistTable TO SetText(dTable.Text(3, 1),1,0);
         ASK missionChkBox TO SetCheck(dTable.Text(4, 1) = "Yes");
         ASK dTable TO SetSelected(1,1);
         tempPhaseDist := phaseDistArray[1];
         GetNumParams(tempPhaseDist, numParams);
         NEW(tempPhaseParams, 1..numParams);
         FOR i:=1 TO numParams
            tempPhaseParams[i] := phaseParamsArray[1,i];
         END FOR;
      ELSE
         {Otherwise, set up default corner info for next new phase to be entered}
         ASK phaseName TO SetText("unnamed");
         tempPhaseNum := activePhases+1;
         IF tempPhaseNum < 10
            ASK phaseNum TO SetLabel(" Phase # 00" + INTTOSTR(tempPhaseNum) + " ");
         ELSIF tempPhaseNum < 100
            ASK phaseNum TO SetLabel(" Phase # 0" + INTTOSTR(tempPhaseNum) + " ");
         ELSE
            ASK phaseNum TO SetLabel(" Phase # " + INTTOSTR(tempPhaseNum) + " ");
         END IF;
         phaseDistArray[activePhases+1] := 19;
         phaseParamsArray[activePhases+1,1] := 100.;
         ASK missionChkBox TO SetCheck(TRUE);
         ASK phaseName TO ReceiveFocus;
         IF NOT printPhase
            ASK editButton TO Deactivate;
            ASK deleteButton TO Deactivate;
            ASK applyButton TO Deactivate;
            ASK dTable TO SetSelected(1,1);
            Draw;
         END IF;
      END IF;   
      IF NOT printPhase
         ASK applyButton TO Deactivate;
         ASK dTable TO SetSelected(1,1);
         Draw;
      END IF;
   END METHOD; {InitSelf}
   
   ASK METHOD ShowTabs;
   VAR
      i,j,k,numCols,counter : INTEGER;
      tempBlock             : RBDBlockObj;
      tempEvent             : RBDEventObj;
      tempNode              : RBDNodeObj;
      tempHier              : RBDHierObj;
      cell, cellDisplay                  : STRING;
   BEGIN
      bTab            := Descendant("BlockTab", 200);
      nTab            := Descendant("NodeTab", 300);
      eTab            := Descendant("EventTab", 400);
      hTab            := Descendant("HierTab", 500);
      bTable          := Descendant("BlockTable", 201);
      nTable          := Descendant("NodeTable", 301);
      eTable          := Descendant("EventTable", 401);
      hTable          := Descendant("HierTable", 501);
      cellAlign       := HorizCentered;
     
      numCols := 5;
      IF tempTotalPhases > 5   
         numCols := tempTotalPhases;
         {Adjust table sizes for scrolls}
         IF phaseBlocks > 14
            ASK bTable TO SetVisibleSize(70, 16);
         ELSE
            ASK bTable TO SetVisibleSize(67, 16);
         END IF;
         IF phaseNodes > 14 
            ASK nTable TO SetVisibleSize(74, 16);
         ELSE
            ASK nTable TO SetVisibleSize(71, 16);
         END IF;
         IF phaseEvents > 14 
            ASK eTable TO SetVisibleSize(74, 16);
         ELSE
            ASK eTable TO SetVisibleSize(71, 16);
         END IF;
         IF phaseHiers > 14 
            ASK hTable TO SetVisibleSize(70, 16);
         ELSE
            ASK hTable TO SetVisibleSize(67, 16);
         END IF;
      ELSE
         IF phaseBlocks > 14
            ASK bTable TO SetVisibleSize(70, 15);
         ELSE
            ASK bTable TO SetVisibleSize(67, 15);
         END IF;
         IF phaseNodes > 14 
            ASK nTable TO SetVisibleSize(74, 15);
         ELSE
            ASK nTable TO SetVisibleSize(71, 15);
         END IF;
         IF phaseEvents > 14 
            ASK eTable TO SetVisibleSize(74, 15);
         ELSE
            ASK eTable TO SetVisibleSize(71, 15);
         END IF;
         IF phaseHiers > 14 
            ASK hTable TO SetVisibleSize(70, 15);
         ELSE
            ASK hTable TO SetVisibleSize(67, 15);
         END IF;
      END IF; 
      
      
      IF phaseBlocks = 0
         IF NOT printPhase
            ASK bTab TO Deactivate;
         END IF;
      ELSE
         IF NOT printPhase
            {ASK bTab TO Activate;}
         END IF;
         ASK bTable TO SetSize(numCols, phaseBlocks);
      END IF;
      IF phaseNodes = 0
         IF NOT printPhase
            ASK nTab TO Deactivate;
         END IF;
      ELSE
         IF NOT printPhase
            {ASK nTab TO Activate;}
         END IF;
         ASK nTable TO SetSize(numCols+2, phaseNodes);
      END IF;
      IF phaseEvents = 0
         IF NOT printPhase
            ASK eTab TO Deactivate;
         END IF;
      ELSE
         IF NOT printPhase
            {ASK eTab TO Activate;}
         END IF;
         ASK eTable TO SetSize(numCols+1, phaseEvents);
      END IF;
      IF phaseHiers = 0
         IF NOT printPhase
            ASK hTab TO Deactivate;
         END IF;
      ELSE
         ASK hTable TO SetSize(numCols, phaseHiers);
      END IF;
      
      FillTables;
      IF phaseBlocks > 0 
         FOR i := 1 TO numCols
            IF i < 10
               ASK bTable TO SetText("P00"+INTTOSTR(i), i, 0);
            ELSIF i < 100
               ASK bTable TO SetText("P0"+INTTOSTR(i), i, 0);
            ELSE
               ASK bTable TO SetText("P"+INTTOSTR(i), i, 0);
            END IF;
            IF i > 5
               ASK bTable TO SetColumnWidth(i,10);
               ASK bTable TO SetColumnAlignment(i,cellAlign);
            END IF;
            FOR j := 1 TO phaseBlocks
               IF i > tablesFull
                  ASK bTable TO SetText("-", i, j);
               END IF;
            END FOR;
         END FOR;
         FOR i := 1 TO phaseBlocks
            counter := 0;
            tempBlock := ASK root Child("RBDBlock", translateBArray[i,2]);
            FOR j := 1 TO tempTotalPhases
               IF j <= tempOldPhases
                  IF axedPhases[j] = 1
                     INC(counter);
                  END IF;
                  IF tempBlock.phaseType[j+counter] = "C"
                     cellDisplay := "C:";
                  ELSIF tempBlock.phaseType[j+counter] = "L"
                     cellDisplay := "L:";
                  ELSE
                     cellDisplay := "A:";
                  END IF;
                  cell := REALTOSTR(tempBlock.phaseValue[j+counter]);
                  cell := SUBSTR(1, POSITION(cell, ".")+3, cell);
                  cellDisplay := cellDisplay + cell;
                  ASK bTable TO SetText(cellDisplay, j, i);
               ELSE
                  IF j > tablesFull
                     ASK bTable TO SetText("A:1.000", j, i);
                  END IF;
               END IF;
            END FOR;
         END FOR;
      END IF;
      
      IF phaseEvents > 0
         FOR i := 2 TO numCols+1
            IF i < 11
               ASK eTable TO SetText("P00"+INTTOSTR(i-1), i, 0);
            ELSIF i < 101
               ASK eTable TO SetText("P0"+INTTOSTR(i-1), i, 0);
            ELSE
               ASK eTable TO SetText("P"+INTTOSTR(i-1), i, 0);
            END IF;
            IF i > 5
               ASK eTable TO SetColumnWidth(i,9);
               ASK eTable TO SetColumnAlignment(i,cellAlign);
            END IF;
            FOR j := 1 TO phaseEvents
               IF i > tablesFull
                  ASK eTable TO SetText("-", i, j);
               END IF;
            END FOR;
         END FOR;
         
         FOR i := 1 TO phaseEvents
            counter := 0;
            tempEvent := ASK root Child("RBDEvent", translateEArray[i,2]);
            FOR j := 1 TO tempTotalPhases
               IF j <= tempOldPhases
                  IF axedPhases[j] = 1
                     INC(counter);
                  END IF;
                  IF tempEvent.phaseType[j+counter] = "C";
                     cellDisplay := "C";
                  ELSIF tempEvent.phaseType[j+counter] = "L";
                     cellDisplay := "L";
                  ELSIF tempEvent.phaseType[j+counter] = "P";
                     cellDisplay := "P";;
                  ELSIF tempEvent.phaseType[j+counter] = "F";
                     cellDisplay := "F";
                  ELSE
                     cell := REALTOSTR(tempEvent.phaseValue[j+counter]);
                     cell := SUBSTR(1, POSITION(cell, ".")+6, cell);
                     cellDisplay := cell;
                  END IF;
                  ASK eTable TO SetText(cellDisplay, j+1, i);
               ELSE
                  IF j = 1
                     ASK eTable TO SetText("F", j+2, i);
                  ELSE
                     ASK eTable TO SetText("P", j+2, i);
                  END IF;
               END IF;
            END FOR;
         END FOR;
      END IF;
     
      IF phaseNodes>0
         FOR i := 3 TO numCols+2
            IF i < 12
               ASK nTable TO SetText("P00"+INTTOSTR(i-2), i, 0);
            ELSIF i < 102
               ASK nTable TO SetText("P0"+INTTOSTR(i-2), i, 0);
            ELSE
               ASK nTable TO SetText("P"+INTTOSTR(i-2), i, 0);
            END IF;
            IF i > 5
               ASK nTable TO SetColumnWidth(i,8);
               ASK nTable TO SetColumnAlignment(i,cellAlign);
            END IF;
            FOR j := 1 TO phaseNodes
               IF i > tablesFull
                  ASK nTable TO SetText("-", i, j);
               END IF;
            END FOR;
         END FOR;
         FOR i := 1 TO phaseNodes
            counter := 0;
            tempNode := ASK root Child("RBDNode", translateNArray[i,2]);
            FOR j := 1 TO tempTotalPhases
               IF j <= tempOldPhases
                  IF axedPhases[j] = 1
                     INC(counter);
                  END IF;
                  IF tempNode.phase[j+counter] = 0
                     cellDisplay := "L";
                  ELSIF tempNode.phase[j+counter] = -1
                     cellDisplay := "C";
                  ELSE
                     cellDisplay := INTTOSTR(tempNode.phase[j+counter]);
                  END IF;
                  ASK nTable TO SetText(cellDisplay, j+2, i);
               ELSE
                  ASK nTable TO SetText(INTTOSTR(tempNode.goodPaths), j+2, i);
               END IF;
            END FOR;
         END FOR;
      END IF;
      
      IF phaseHiers > 0
         FOR i := 1 TO numCols
            IF i < 11
               ASK hTable TO SetText("P00"+INTTOSTR(i), i, 0);
            ELSIF i < 101
               ASK hTable TO SetText("P0"+INTTOSTR(i), i, 0);
            ELSE
               ASK hTable TO SetText("P"+INTTOSTR(i), i, 0);
            END IF;
            IF i > 5
               ASK hTable TO SetColumnWidth(i,9);
               ASK hTable TO SetColumnAlignment(i,cellAlign);
            END IF;
            FOR j := 1 TO phaseHiers
               IF i > tablesFull
                  ASK hTable TO SetText("-", i, j);
               END IF;
            END FOR;
         END FOR;
         
         FOR i := 1 TO phaseHiers
            counter := 0;
            tempHier := ASK root Child("RBDHier", translateHArray[i,2]);
            tempNode := ASK root Child("RBDNode", tempHier.outID);
            FOR j := 1 TO tempTotalPhases
               IF j <= tempOldPhases
                  IF axedPhases[j] = 1
                     INC(counter);
                  END IF;
                  IF tempNode.phase[j+counter] = 0   
                     cellDisplay := "L";
                  ELSIF tempNode.phase[j+counter] = -1
                     cellDisplay := "C";
                  ELSIF tempNode.phase[j+counter] = 1
                     cellDisplay := "A";
                  END IF;
                  ASK hTable TO SetText(cellDisplay, j, i);
               ELSE
                  ASK hTable TO SetText("A", j, i);
               END IF;
            END FOR;
         END FOR;
      END IF;
      tablesFull := tempTotalPhases;
   END METHOD; {ShowTabs}

   
   ASK METHOD AdjustTables(IN addPhase                  : BOOLEAN;
                           IN changePhase               : INTEGER);

   VAR
      i,j,k,numCols{,counter} : INTEGER;
      tempBlock             : RBDBlockObj;
      tempNode              : RBDNodeObj;
      cell, cellDisplay                  : STRING;
      

   BEGIN
      bTab            := Descendant("BlockTab", 200);
      nTab            := Descendant("NodeTab", 300);
      eTab            := Descendant("EventTab", 400);
      bTable          := Descendant("BlockTable", 201);
      eTable          := Descendant("EventTable", 401);
      nTable          := Descendant("NodeTable", 301);
      hTable          := Descendant("HierTable", 501);
      cellAlign       := HorizCentered;
     
     IF tempTotalPhases > 13
        ASK dTable TO SetSize(4, tempTotalPhases+1);
        ASK dTable TO SetVisibleSize(57,15);
     ELSE
        ASK dTable TO SetSize(4, 14);
        ASK dTable TO SetVisibleSize(54,15);
     END IF;
                  
      numCols := 5;
      IF tempTotalPhases > 5   
         numCols := tempTotalPhases;
         {Adjust table sizes for scrolls}
         IF phaseBlocks > 14
            ASK bTable TO SetVisibleSize(70, 16);
         ELSE
            ASK bTable TO SetVisibleSize(67, 16);
         END IF;
         IF phaseNodes > 14 
            ASK nTable TO SetVisibleSize(74, 16);
         ELSE
            ASK nTable TO SetVisibleSize(71, 16);
         END IF;
         IF phaseEvents > 14 
            ASK eTable TO SetVisibleSize(74, 16);
         ELSE
            ASK eTable TO SetVisibleSize(71, 16);
         END IF;
         IF phaseHiers > 14 
            ASK hTable TO SetVisibleSize(70, 16);
         ELSE
            ASK hTable TO SetVisibleSize(67, 16);
         END IF;
      ELSE
         IF phaseBlocks > 14
            ASK bTable TO SetVisibleSize(70, 15);
         ELSE
            ASK bTable TO SetVisibleSize(67, 15);
         END IF;
         IF phaseNodes > 14 
            ASK nTable TO SetVisibleSize(74, 15);
         ELSE
            ASK nTable TO SetVisibleSize(71, 15);
         END IF;
         IF phaseEvents > 14 
            ASK eTable TO SetVisibleSize(74, 15);
         ELSE
            ASK eTable TO SetVisibleSize(71, 15);
         END IF;
         IF phaseHiers > 14 
            ASK hTable TO SetVisibleSize(70, 15);
         ELSE
            ASK hTable TO SetVisibleSize(67, 15);
         END IF;
      END IF; 
       
      IF addPhase = TRUE {Inserting Phase}
         IF phaseBlocks > 0
            ASK bTable TO SetSize(numCols, phaseBlocks);
         END IF;
         IF phaseNodes > 0
            ASK nTable TO SetSize(numCols+2, phaseNodes);
         END IF;
         IF phaseEvents > 0
            ASK eTable TO SetSize(numCols+1, phaseEvents);
         END IF;
         IF phaseHiers > 0
            ASK hTable TO SetSize(numCols, phaseHiers);
         END IF;
         
         IF phaseBlocks > 0
            FOR i := 1 TO phaseBlocks
               FOR j := tempTotalPhases DOWNTO changePhase
                  IF j > 5
                     ASK bTable TO SetColumnWidth(j,10);
                     ASK bTable TO SetColumnAlignment(j,cellAlign);
                     IF j < 10
                        ASK bTable TO SetText("P00"+INTTOSTR(j), j, 0);
                     ELSIF j < 100
                        ASK bTable TO SetText("P0"+INTTOSTR(j), j, 0);
                     ELSE
                        ASK bTable TO SetText("P"+INTTOSTR(j), j, 0);
                     END IF;
                  END IF;
                  IF j > changePhase
                     ASK bTable TO SetText(ASK bTable Text(j-1,i), j, i);
                  ELSE 
                     ASK bTable TO SetText("A:1.000", j, i);
                  END IF;
               END FOR;
            END FOR;
         END IF;
         IF phaseEvents > 0
            FOR i := 1 TO phaseEvents
               FOR j := (tempTotalPhases+1) DOWNTO (changePhase+1)
                  IF j > 6
                     ASK eTable TO SetColumnWidth(j,9);
                     ASK eTable TO SetColumnAlignment(j,cellAlign);
                     IF j < 11
                        ASK eTable TO SetText("P00"+INTTOSTR(j-1), j, 0);
                     ELSIF j < 101
                        ASK eTable TO SetText("P0"+INTTOSTR(j-1), j, 0);
                     ELSE
                        ASK eTable TO SetText("P"+INTTOSTR(j-1), j, 0);
                     END IF;
                  END IF;
                  IF j > (changePhase+1)
                     ASK eTable TO SetText(ASK eTable Text(j-1,i), j, i);
                  ELSE 
                     ASK eTable TO SetText("P", j, i);
                  END IF;
               END FOR;
            END FOR;
         END IF;
         IF phaseHiers > 0
            FOR i := 1 TO phaseHiers
               FOR j := tempTotalPhases DOWNTO changePhase
                  IF j > 5
                     ASK hTable TO SetColumnWidth(j,10);
                     ASK hTable TO SetColumnAlignment(j,cellAlign);
                     IF j < 11
                        ASK hTable TO SetText("P00"+INTTOSTR(j), j, 0);
                     ELSIF j < 101
                        ASK hTable TO SetText("P0"+INTTOSTR(j), j, 0);
                     ELSE
                        ASK hTable TO SetText("P"+INTTOSTR(j), j, 0);
                     END IF;
                  END IF;
                  IF j > changePhase
                     ASK hTable TO SetText(ASK hTable Text(j-1,i), j, i);
                  ELSE 
                     ASK hTable TO SetText("A", j, i);
                  END IF;
               END FOR;
            END FOR;
         END IF;
         IF phaseNodes > 0
            FOR i := 1 TO phaseNodes
               FOR j := (tempTotalPhases+2) DOWNTO (changePhase+2)
                  IF j > 7
                     ASK nTable TO SetColumnWidth(j,8);
                     ASK nTable TO SetColumnAlignment(j,cellAlign);
                     IF j < 12
                        ASK nTable TO SetText("P00"+INTTOSTR(j-2), j, 0);
                     ELSIF j < 102
                        ASK nTable TO SetText("P0"+INTTOSTR(j-2), j, 0);
                     ELSE
                        ASK nTable TO SetText("P"+INTTOSTR(j-2), j, 0);
                     END IF;
                  END IF;
                  IF j > (changePhase+2)
                     ASK nTable TO SetText(nTable.Text(j-1,i), j, i);
                  ELSE
                     IF (nTable.Text(2,i) = "0")
                        ASK nTable TO SetText("L", j, i);
                     ELSE
                        ASK nTable TO SetText(nTable.Text(2,i), j, i);
                     END IF;
                  END IF;
               END FOR;
            END FOR;
         END IF;
      ELSE {Deleting Phase}
         IF phaseBlocks > 0
            FOR i := 1 TO phaseBlocks
               FOR j := changePhase TO tempTotalPhases
                  ASK bTable TO SetText(ASK bTable Text(j+1,i), j, i);
               END FOR;
               IF tempTotalPhases < 5
                  ASK bTable TO SetText("-", tempTotalPhases+1, i);
               END IF;
               {IF tempTotalPhases = 0
                  FOR j := 1 TO 5
                     ASK bTable TO SetText("", j, i);
                  END FOR;
               END IF;}
            END FOR;
         END IF;
         IF phaseNodes > 0
            FOR i := 1 TO phaseNodes
               FOR j := (changePhase+2) TO (tempTotalPhases+2)
                  ASK nTable TO SetText(ASK nTable Text(j+1,i), j, i);
               END FOR;
               IF tempTotalPhases < 5
                  ASK nTable TO SetText("-", tempTotalPhases+3, i);
               END IF;
            END FOR;
         END IF;
         IF phaseEvents > 0
            FOR i := 1 TO phaseEvents
               FOR j := (changePhase+1) TO (tempTotalPhases+1)
                  ASK eTable TO SetText(ASK eTable Text(j+1,i), j, i);
               END FOR;
               IF tempTotalPhases < 5
                  ASK eTable TO SetText("-", tempTotalPhases+2, i);
               END IF;
            END FOR;
         END IF;
         IF phaseHiers > 0
            FOR i := 1 TO phaseHiers
               FOR j := changePhase TO tempTotalPhases
                  ASK hTable TO SetText(ASK hTable Text(j+1,i), j, i);
               END FOR;
               IF tempTotalPhases < 5
                  ASK hTable TO SetText("-", tempTotalPhases+1, i);
               END IF;
            END FOR;
         END IF;
         
         IF phaseBlocks > 0
            ASK bTable TO SetSize(numCols, phaseBlocks);
         END IF;
         IF phaseNodes > 0
            ASK nTable TO SetSize(numCols+2, phaseNodes);
         END IF;
         IF phaseEvents > 0
            ASK eTable TO SetSize(numCols+1, phaseEvents);
         END IF;
         IF phaseHiers > 0
            ASK hTable TO SetSize(numCols, phaseHiers);
         END IF;
      END IF;
      tablesFull := tempTotalPhases;
   END METHOD; {AdjustTables}
   
   ASK METHOD BeSelected;
   VAR
      i,j,k,bRow,eRow,bCol,
      eCol,nRow,nCol,hRow, hCol, numParams, tempPhases        : INTEGER;
      value,gap, num             : REAL;
      cellVal, tempPhaseString, tempString, valueString               : STRING;
      arrayR, tempArray                : realArray;
      arrayS                : strArray;
      arrayI                : intArray;
      objClicked            : GraphicVObj;
      block                 : RBDBlockObj;
      event                 : RBDEventObj;
      node, outNode         : RBDNodeObj;
      hier                  : RBDHierObj;
      times                 : ARRAY INTEGER OF REAL;
      phase                 : PhaseObj;
      validData             : BOOLEAN;
      temp1,temp2,temp3 : STRING;
      temp4,temp5,temp6, tempPhaseNum  : INTEGER;
      tempLastClicked : GraphicVObj;
      text                  : strArray;
   BEGIN
      objClicked := LastPicked;
      NEW(text,1..20);
      errors := 0;
      bCombo     := Descendant("BComboBox", 207);
      nCombo     := Descendant("NComboBox", 307);
      eCombo     := Descendant("EComboBox", 406);
      hCombo     := Descendant("HComboBox", 502);
      stressBox  := Descendant("StressBox", 202);
      kValBox    := Descendant("kValBox", 302);

      IF ((objClicked.Id = 90) OR (objClicked.Id = 106)) {OK or Apply}
         IF objClicked.Id = 106
            ASK applyButton TO Deactivate;
         END IF;
         somethingChanged := TRUE;
         IF activePhases > 0
            FOR i:=1 TO activePhases
               phase := phaseObjArray[i];
               DISPOSE(phase);
            END FOR;
         END IF;
         activePhases := tempTotalPhases;
         IF tempTotalPhases > 0
            DISPOSE(phaseObjArray);
            NEW(phaseObjArray, 1..activePhases);
            FOR i:=1 TO activePhases
               NEW(phase);
               phaseObjArray[i] := phase;
            END FOR;
               
            FOR i := 1 TO activePhases
               ASK phaseObjArray[i] TO SetPhaseData(dTable.Text(1,i), phaseDistArray[i], i, 
                                                    phaseParamsArray[i], (dTable.Text(4,i) = "Yes"));
            END FOR;
            NEW(arrayI, 1..activePhases);
            NEW(arrayR, 1..activePhases);
            NEW(arrayS, 1..activePhases);
            FOR i := 1 TO phaseBlocks
               block := ASK root Child("RBDBlock", translateBArray[i,2]);
               FOR j := 1 TO activePhases
                  cellVal := bTable.Text(j, i);
                  tempString := cellVal + "       ";
                  arrayS[j] := SUBSTR(1,1,cellVal);
                  arrayR[j] := STRTOREAL(SUBSTR(3,10,tempString));
               END FOR;
               ASK block TO SetPhases(TRUE,arrayR, arrayS);
            END FOR;
            FOR i := 1 TO phaseEvents
               event := ASK root Child("RBDEvent", translateEArray[i,2]);
               FOR j := 1 TO activePhases
                  cellVal := eTable.Text(j+1, i);
                  tempString := SUBSTR(1,1,cellVal);
                  arrayR[j] := 0.;
                  IF tempString = "F"
                     arrayS[j] := "F";
                  ELSIF tempString = "L";
                     arrayS[j] := "L";
                  ELSIF tempString = "C";
                     arrayS[j] := "C";
                  ELSIF tempString = "P"
                     arrayS[j] := "P";
                  ELSE
                     arrayS[j] := "A";
                     tempString := cellVal + "       ";
                     arrayR[j] := STRTOREAL(SUBSTR(1,10,tempString));
                  END IF;
               END FOR;
               ASK event TO SetPhases(TRUE,arrayR, arrayS);
            END FOR;
            FOR i := 1 TO phaseNodes
               node := ASK root Child("RBDNode", translateNArray[i,2]);
               FOR j := 1 TO activePhases
                  cellVal:= nTable.Text(j+2, i);
                  IF cellVal = "C"
                     arrayI[j] := -1;
                  ELSIF cellVal = "L"
                     arrayI[j] := 0;
                  ELSE
                     arrayI[j] := STRTOINT(cellVal);
                  END IF;
               END FOR;
               ASK node TO SetPhases(TRUE,arrayI);
            END FOR;
            FOR i := 1 TO phaseHiers
               hier := ASK root Child("RBDHier", translateHArray[i,2]);
               outNode := ASK root Child("RBDNode", hier.outID);
               FOR j := 1 TO activePhases
                  cellVal:= hTable.Text(j, i);
                  IF cellVal = "C"
                     arrayI[j] := -1;
                  ELSIF cellVal = "L"
                     arrayI[j] := 0;
                  ELSIF cellVal = "A"
                     arrayI[j] := 1;
                  END IF;
               END FOR;
               ASK outNode TO SetPhases(TRUE,arrayI);
            END FOR;
            DISPOSE(arrayI);
            DISPOSE(arrayR);
            DISPOSE(arrayS);
         ELSE
            FOR i := 1 TO phaseBlocks
               block := ASK root Child("RBDBlock", translateBArray[i,2]);
               ASK block TO SetPhases(TRUE,NILARRAY, NILARRAY);
            END FOR;
            FOR i := 1 TO phaseEvents
               event := ASK root Child("RBDEvent", translateEArray[i,2]);
               ASK event TO SetPhases(TRUE,NILARRAY, NILARRAY);
            END FOR;
            FOR i := 1 TO phaseNodes
               node := ASK root Child("RBDNode", translateNArray[i,2]);
               ASK node TO SetPhases(TRUE,NILARRAY);
            END FOR;
            FOR i := 1 TO phaseHiers
               hier := ASK root Child("RBDHier", translateHArray[i,2]);
               outNode := ASK root Child("RBDNode", hier.outID);
               ASK outNode TO SetPhases(TRUE,NILARRAY);
            END FOR;
         END IF;
      END IF;
      CASE objClicked.Id
         WHEN 91: {Cancel}
            IF activePhases > 0
               FOR i := 1 TO phaseBlocks
                  block := ASK root Child("RBDBlock", translateBArray[i,2]);
                  IF block.phaseValue = NILARRAY
                     ASK block TO SetPhases(TRUE,NILARRAY, NILARRAY);
                  END IF;
               END FOR;
               FOR i := 1 TO phaseEvents
                  event := ASK root Child("RBDEvent", translateEArray[i,2]);
                  IF event.phaseValue = NILARRAY
                     ASK event TO SetPhases(TRUE,NILARRAY, NILARRAY);
                  END IF;
               END FOR;
               FOR i := 1 TO phaseNodes
                  node := ASK root Child("RBDNode", translateNArray[i,2]);
                  IF node.phase = NILARRAY
                     ASK node TO SetPhases(TRUE,NILARRAY);
                  END IF;
               END FOR;
               FOR i := 1 TO phaseHiers
                  hier := ASK root Child("RBDHier", translateHArray[i,2]);
                  outNode := ASK root Child("RBDNode", hier.outID);
                  IF outNode.phase = NILARRAY
                     ASK outNode TO SetPhases(TRUE, NILARRAY);
                  END IF;
               END FOR;
            END IF;
         WHEN 100:  {Define Tab}
            lastClicked := objClicked.LastPicked; 
            CASE lastClicked.Id
            WHEN 101:
               {If the selected row is a row with a phase}
               IF (dTable.SelectedRow > 0) AND (dTable.SelectedRow <= tempTotalPhases)
                  tempPhaseNum := STRTOINT(dTable.Text(2,dTable.SelectedRow));
                  IF tempPhaseNum < 10
                     ASK phaseNum TO SetLabel(" Phase # 00" + INTTOSTR(tempPhaseNum) + " ");
                  ELSIF tempPhaseNum < 100
                     ASK phaseNum TO SetLabel(" Phase # 0" + INTTOSTR(tempPhaseNum) + " ");
                  ELSE
                     ASK phaseNum TO SetLabel(" Phase # " + INTTOSTR(tempPhaseNum) + " ");
                  END IF;
                  ASK phaseName TO SetText(dTable.Text(1, dTable.SelectedRow));
                  ASK phaseDistTable TO SetText(dTable.Text(3, dTable.SelectedRow),1,0);
                  ASK missionChkBox TO SetCheck(dTable.Text(4, dTable.SelectedRow) = "Yes");
                  ASK dTable TO SetSelected(1,dTable.SelectedRow);
                  {??}
                  tempPhaseDist := phaseDistArray[dTable.SelectedRow];
                  GetNumParams(tempPhaseDist, numParams);
                  NEW(tempPhaseParams, 1..numParams);
                  FOR i:=1 TO numParams
                     tempPhaseParams[i] := phaseParamsArray[dTable.SelectedRow,i];
                  END FOR;
                  {??}
                  
                  ASK deleteButton TO Activate;
                  ASK editButton TO Activate;
               ELSE {Set up default values for phase info}
                  ASK dTable TO SetSelected(1,tempTotalPhases+1);
                  ASK phaseName TO SetText("unnamed");
                  tempPhaseNum := tempTotalPhases+1;
                  IF tempPhaseNum < 10
                     ASK phaseNum TO SetLabel(" Phase # 00" + INTTOSTR(tempPhaseNum) + " ");
                  ELSIF tempPhaseNum < 100
                     ASK phaseNum TO SetLabel(" Phase # 0" + INTTOSTR(tempPhaseNum) + " ");
                  ELSE
                     ASK phaseNum TO SetLabel(" Phase # " + INTTOSTR(tempPhaseNum) + " ");
                  END IF;
                  ASK missionChkBox TO SetCheck(TRUE);
                  tempPhaseDist := 19;
                  NEW(tempPhaseParams, 1..1);
                  tempPhaseParams[1] := 100.;
                  MakeDistString(tempPhaseDist, tempPhaseParams, tempString);
                  ASK phaseDistTable TO SetText(tempString, 1, 0);
                  ASK deleteButton TO Deactivate;
                  ASK editButton TO Deactivate;
               END IF;
            WHEN 105: {Edit Phase}
               {Save phase info in table}
               IF ((phaseName.Text() = "") OR (POSITION(phaseName.Text(), " ") <> 0))
                  INC(errors);
                  text[errors] := "'Phase Name' field can't be blank or have blank spaces!     ";
                  validData := FALSE;
               ELSIF STRLEN(phaseName.Text()) > 20
                  INC(errors);
                  text[errors] := "'Phase Name' must be no greater than 20 characters!     ";
                  ASK phaseName TO SetText(SUBSTR(1,20,phaseName.Text()));
                  Draw;
               ELSE
                  ASK dTable TO SetText(phaseName.Text(), 1, dTable.SelectedRow);
                  validData := TRUE;
               END IF;
               IF validData   
                  ASK dTable TO SetText(phaseDistTable.Text(1,0), 3, dTable.SelectedRow);
                  IF missionChkBox.Checked
                     ASK dTable TO SetText("Yes", 4, dTable.SelectedRow);
                  ELSE
                     ASK dTable TO SetText("No", 4, dTable.SelectedRow);
                  END IF;
                  {Save phase dist and params in arrays}   
                  phaseDistArray[dTable.SelectedRow] := tempPhaseDist;
                  FOR i:=1 TO HIGH(tempPhaseParams)
                     phaseParamsArray[dTable.SelectedRow,i] := tempPhaseParams[i];
                  END FOR;
               END IF;
               ASK applyButton TO Activate;
            WHEN 107:  {Delete Selected Phase} 
               IF dTable.SelectedRow < tempTotalPhases
                  FOR i:=dTable.SelectedRow TO tempTotalPhases-1
                     ASK dTable TO SetText(dTable.Text(1,i+1),1,i);
                     ASK dTable TO SetText(dTable.Text(3,i+1),3,i);
                     ASK dTable TO SetText(dTable.Text(4,i+1),4,i);
                     phaseDistArray[i] := phaseDistArray[i+1];
                     FOR j:=1 TO 3
                        phaseParamsArray[i,j] := phaseParamsArray[i+1,j];
                     END FOR;
                  END FOR;
               END IF;
               DEC(tempTotalPhases);
               
               {Set info for currently selected phase}
               {If deleting last row, set up defaults for empty row}
               IF dTable.SelectedRow = tempTotalPhases + 1
                  ASK phaseName TO SetText("unnamed");
                  ASK phaseNum TO SetLabel(" Phase # "+ (dTable.Text(2,dTable.SelectedRow)) + " ");
                  tempPhaseDist := 19;
                  NEW(tempPhaseParams, 1..1);
                  tempPhaseParams[1] := 100.;
                  MakeDistString(tempPhaseDist, tempPhaseParams, tempString);
                  ASK phaseDistTable TO SetText(tempString, 1,0); 
                  ASK missionChkBox TO SetCheck(TRUE);
                  ASK deleteButton TO Deactivate;
                  ASK editButton TO Deactivate;
               ELSE  
                  ASK phaseName TO SetText(dTable.Text(1,dTable.SelectedRow));
                  ASK phaseNum TO SetLabel(" Phase # " + (dTable.Text(2,dTable.SelectedRow)) + " ");
                  ASK phaseDistTable TO SetText((dTable.Text(3,dTable.SelectedRow)), 1,0); 
                  ASK missionChkBox TO SetCheck((dTable.Text(4,dTable.SelectedRow)) = "Yes");
                  tempPhaseDist := phaseDistArray[dTable.SelectedRow];
                  GetNumParams(tempPhaseDist, numParams);
                  NEW(tempPhaseParams, 1..numParams);
                  FOR i:=1 TO numParams               
                     tempPhaseParams[i] := phaseParamsArray[dTable.SelectedRow,i];
                  END FOR;
               END IF;
  
               FOR i:=1 TO 4
                 ASK dTable TO SetText("",i,tempTotalPhases+1);
               END FOR;

               AdjustTables(FALSE, dTable.SelectedRow);
               IF tempTotalPhases = 0
                  ASK bTab TO Deactivate;
                  ASK eTab TO Deactivate;
                  ASK nTab TO Deactivate;
                  ASK clrAllButton TO Deactivate;
                  ASK deleteButton TO Deactivate;
                  ASK editButton TO Deactivate;
               END IF;
               ASK applyButton TO Activate;
            WHEN 108:  {Clear All Phases}
               FOR i := 1 TO tempTotalPhases
                  ASK dTable TO SetText("",1,i);
                  ASK dTable TO SetText("",2,i);
                  ASK dTable TO SetText("",3,i);
                  ASK dTable TO SetText("",4,i);
               END FOR;
               ASK phaseNum TO SetLabel(" Phase # 001 ");
               ASK phaseName TO ReceiveFocus;
               ASK dTable TO SetSelected(1,1);
               tablesFull := 0;
               tempPhases := tempTotalPhases;
               tempTotalPhases := 0;
               {Deleted last phase, set up defaults}            
               ASK phaseName TO SetText("unnamed");
               ASK phaseNum TO SetLabel(" Phase # 001");
               tempPhaseDist := 19;
               NEW(tempPhaseParams, 1..1);
               tempPhaseParams[1] := 100.;
               MakeDistString(tempPhaseDist, tempPhaseParams, tempString);
               ASK phaseDistTable TO SetText(tempString, 1,0); 
               ASK missionChkBox TO SetCheck(TRUE);
               ASK deleteButton TO Deactivate;
               ASK editButton TO Deactivate;
               
               ASK clrAllButton TO Deactivate;
               ASK deleteButton TO Deactivate;
               ASK applyButton TO Deactivate;
               IF tempOldPhases > 0
                  FOR i := 1 TO tempTotalPhases
                     axedPhases[i] := 1;
                  END FOR;
                  tempOldPhases := 0;
               END IF;
               FOR i:=1 TO tempPhases
                  AdjustTables(FALSE, i);
               END FOR;
               FOR i:=1 TO 5
                  FOR j:=1 TO phaseBlocks
                     ASK bTable TO SetText("-", i, j);
                  END FOR;
                  FOR j:=1 TO phaseNodes
                     ASK nTable TO SetText("-", i+2, j);
                  END FOR;
                  FOR j:=1 TO phaseEvents
                     ASK eTable TO SetText("-", i+1, j);
                  END FOR;
                  FOR j:=1 TO phaseHiers
                     ASK hTable TO SetText("-", i, j);
                  END FOR;
               END FOR;
               ASK bTab TO Deactivate;
               ASK nTab TO Deactivate;
               ASK eTab TO Deactivate;
               ASK hTab TO Deactivate;
               ASK applyButton TO Activate;
            WHEN 111:  {Pick Phase Distribution}
               {Show dist box with appropriate info}
               tempLastClicked := ASK SELF LastPicked;
               ShowDistBox(tempPhaseDist, tempPhaseParams);
               LastPicked := tempLastClicked;
               MakeDistString(tempPhaseDist, tempPhaseParams, tempPhaseString);
               ASK phaseDistTable TO SetText(tempPhaseString, 1, 0);
               Draw;
            WHEN 114:  {Insert Phase Button}
               IF ((phaseName.Text() = "") OR (POSITION(phaseName.Text(), " ") <> 0))
                  INC(errors);
                  text[errors] := "'Phase Name' field can't be blank or have blank spaces!     ";
                  ASK phaseName TO ReceiveFocus;
                  validData := FALSE;
               ELSIF STRLEN(phaseName.Text()) > 20
                  INC(errors);
                  text[errors] := "'Phase Name' must be no greater than 20 characters!     ";
                  ASK phaseName TO SetText(SUBSTR(1,20,phaseName.Text()));
                  ASK phaseName TO ReceiveFocus;
                  validData := FALSE;
               ELSE
                  validData := TRUE;
               END IF;
               
               IF validData
                  IF tempTotalPhases = 0 {Entering first phase}
                     ASK dTable TO Activate;
                     IF phaseBlocks > 0
                        ASK bTab TO Activate;
                     END IF;
                     IF phaseNodes > 0
                        ASK nTab TO Activate;
                     END IF;
                     IF phaseEvents > 0
                        ASK eTab TO Activate;
                     END IF;
                     IF phaseHiers > 0
                        ASK hTab TO Activate;
                     END IF;
                  END IF;
                  {If inserting in the middle}
                  IF dTable.SelectedRow <= tempTotalPhases
                     FOR i:=(tempTotalPhases+1) DOWNTO (dTable.SelectedRow+1)
                        ASK dTable TO SetText(dTable.Text(1,i-1),1,i);
                        ASK dTable TO SetText(dTable.Text(3,i-1),3,i);
                        ASK dTable TO SetText(dTable.Text(4,i-1),4,i);
                        phaseDistArray[i] := phaseDistArray[i-1];
                        FOR j:=1 TO 3
                           phaseParamsArray[i,j] := phaseParamsArray[i-1,j];
                        END FOR;
                     END FOR;
                  END IF;
               
                  {Insert another phase number in table}
                  IF tempTotalPhases+1 < 10
                     tempString := "00" + INTTOSTR(tempTotalPhases+1);
                  ELSIF tempTotalPhases+1 < 100
                     tempString := "0" + INTTOSTR(tempTotalPhases+1);
                  ELSE
                     tempString := INTTOSTR(tempTotalPhases+1);
                  END IF;
                  ASK dTable TO SetText(tempString,2,tempTotalPhases+1); 

                  ASK dTable TO SetText(phaseName.Text(), 1, dTable.SelectedRow);
                  ASK dTable TO SetText(phaseDistTable.Text(1,0),3,dTable.SelectedRow);
                  IF missionChkBox.Checked
                     ASK dTable TO SetText("Yes", 4, dTable.SelectedRow);
                  ELSE
                     ASK dTable TO SetText("No", 4, dTable.SelectedRow);
                  END IF;
                  {Save phase dist and parameters in their arrays}
                  phaseDistArray[dTable.SelectedRow] := tempPhaseDist;
                  FOR i:=1 TO HIGH(tempPhaseParams)
                     phaseParamsArray[dTable.SelectedRow,i] := tempPhaseParams[i];
                  END FOR;
              
                  INC(tempTotalPhases);
                  AdjustTables(TRUE, dTable.SelectedRow);
 
                  IF dTable.SelectedRow = tempTotalPhases
                     ASK phaseName TO SetText("unnamed");
                     IF tempTotalPhases+1 < 10
                        tempString := "00" + INTTOSTR(tempTotalPhases+1);
                     ELSIF tempTotalPhases+1 < 100
                        tempString := "0" + INTTOSTR(tempTotalPhases+1);
                     ELSE
                        tempString := INTTOSTR(tempTotalPhases+1);
                     END IF;
                     ASK phaseNum TO SetLabel(" Phase # " + tempString + " ");
                     tempPhaseDist := 19;
                     NEW(tempPhaseParams, 1..1);
                     tempPhaseParams[1] := 100.;
                     MakeDistString(tempPhaseDist, tempPhaseParams, tempString);
                     ASK phaseDistTable TO SetText(tempString, 1,0); 
                     ASK missionChkBox TO SetCheck(TRUE);
                     ASK dTable TO SetSelected(1, tempTotalPhases + 1);
                  END IF;
              
                  ASK clrAllButton TO Activate;
                  ASK applyButton TO Activate;
               END IF;
               Draw;
            WHEN 605:
            IF compileType <> "demo"
               PrintPhases(dTable);
            ELSE
               NEW(message, 1..1);
               message[1] := "The demo version has no print capability     ";
               result := SendAlert(message, FALSE, FALSE, TRUE);
               DISPOSE(message);
            END IF;
            OTHERWISE;
            END CASE;  
         WHEN 200:  {Assign Block Tab}
            bMode := bCombo.Text();
            IF bMode = "Active"
               bText := "A:";
            END IF;
            IF (stressBox.Value() < 0.0) OR (stressBox.Value() > 999.999)
               INC(errors);
               text[errors] := "Stress value must be between 0 and 999.999!     ";
               ASK stressBox TO DisplayValue(1.0);
            END IF;
            bCol := bTable.SelectedColumn;
            bRow := bTable.SelectedRow;
            IF (bCol > 0) AND (bRow > 0)
               oldBCol := bCol;
               oldBRow := bRow;
            END IF;
            ASK bTable TO SetSelected(oldBCol, oldBRow);
            lastClicked := objClicked.LastPicked;
            valueString := REALTOSTR(stressBox.Value());
            valueString := SUBSTR(1, POSITION(valueString, ".")+3, valueString);
            CASE lastClicked.Id
               WHEN 201:    {block table}
                  ASK applyButton TO Activate;
                  IF ((bRow > 0) AND (bCol > 0) AND (bRow <= phaseBlocks))
                     IF bCol > tempTotalPhases
                        bCol := tempTotalPhases;
                        ASK bTable TO SetSelected(bCol, bRow);
                     END IF;
                     ASK b1Butt TO Activate;
                     ASK b2Butt TO Activate;
                     ASK b3Butt TO Activate;
                     ASK b4Butt TO Activate;
                     ASK bTable TO SetText(bText + valueString, bCol, bRow);
                  END IF;
                  {Set up phase name buttons}
                  IF ((bRow = 0) AND (bCol <= tempTotalPhases) AND (bCol > 0))
                     ASK BPhaseLabel TO SetHidden(FALSE);
                     ASK BPhaseLabel TO SetLabel(dTable.Text(1,bCol));
                     Draw;
                     Delay(2);
                     ASK BPhaseLabel TO SetHidden(TRUE);
                  END IF;
               WHEN 203:    {Mark Row}
                  IF bRow > 0
                     FOR i := 1 TO tempTotalPhases                     
                        ASK bTable TO SetText(bText + valueString, i, bRow);
                     END FOR;
                  END IF;
               WHEN 204:    {Reset Row}
                  IF bRow > 0
                     FOR i := 1 TO tempTotalPhases                     
                        ASK bTable TO SetText("A:1.000", i, bRow);
                     END FOR;
                  END IF;
               WHEN 205:    {Mark Column}
                  IF bCol > 0
                     FOR i := 1 TO phaseBlocks                     
                        ASK bTable TO SetText(bText + valueString, bCol, i);
                     END FOR;
                  END IF;
               WHEN 206:    {Reset Column}
                  IF bCol > 0
                     FOR i := 1 TO phaseBlocks                    
                        ASK bTable TO SetText("A:1.000", bCol, i);
                     END FOR;
                  END IF;
               WHEN 207:    {Mode combo}
                  IF bMode = "Cut"
                     bText := "C:"; 
                     ASK stressBox TO SetHidden(FALSE);
                  ELSIF bMode = "Link"
                     bText := "L:"; 
                     ASK stressBox TO SetHidden(FALSE);
                  ELSE
                     bText := "A:"; 
                     ASK stressBox TO SetHidden(FALSE);
                  END IF;
               WHEN 601:    {Print}
                  IF compileType <> "demo"
                     PrintBlockPhases(bTable);
                  ELSE
                     NEW(message, 1..1);
                     message[1] := "The demo version has no print capability     ";
                     result := SendAlert(message, FALSE, FALSE, TRUE);
                     DISPOSE(message);
                  END IF;
               OTHERWISE
            END CASE;
         WHEN 300:  {Assign Node Tab}
            nMode := nCombo.Text();
            nCol := nTable.SelectedColumn;
            nRow := nTable.SelectedRow;
            IF (nCol > 0) AND (nRow > 0)
               oldNCol := nCol;
               oldNRow := nRow;
            END IF;
            ASK nTable TO SetSelected(oldNCol, oldNRow);
            IF nMode = "k-value";
               IF (kValBox.Value() < 0.) OR (kValBox.Value() > 9999.);
                  INC(errors);
                  text[errors] := "k value to use for changes must be between 0 and 9999!     ";
                  IF (nTable.Text(1,nRow) = "0")
                     ASK kValBox TO DisplayValue(0.0);
                     nText := "L";
                  ELSE        
                     ASK kValBox TO DisplayValue(1.0);
                     nText := "1";
                  END IF;
               ELSIF (kValBox.Value() <> FLOAT(ROUND(kValBox.Value())))
                  INC(errors);
                  text[errors] := "k value must be an integer!     ";
                  ASK kValBox TO DisplayValue(1.0);
                  nText := "1";
               ELSE   
                  nText := INTTOSTR(ROUND(kValBox.Value()));
               END IF;
            END IF;
            lastClicked := objClicked.LastPicked;
            CASE lastClicked.Id
               WHEN 301:    {node table}
                  ASK applyButton TO Activate;
                  IF ((nRow > 0) AND (nCol > 2) AND (nRow <= phaseNodes))
                     IF nCol > (tempTotalPhases+2)
                        nCol := tempTotalPhases+2;
                        ASK nTable TO SetSelected(nCol, nRow);
                     END IF;
                     ASK n4Butt TO Activate;
                     IF nMode = "Increment"
                        cellVal := nTable.Text(nCol, nRow);
                        IF ((cellVal = "C") OR (cellVal = nTable.Text(1, nRow)) OR (nTable.Text(1,nRow) = "0"))
                           cellVal := "C";
                        ELSIF ((cellVal = "L") AND (nTable.Text(1,nRow) = "0"))
                           cellVal := "L";
                        ELSIF (cellVal = "L")
                           cellVal := "1";
                        ELSIF (STRTOINT(cellVal) > 9999)
                           cellVal := "9999";
                        ELSE
                           cellVal := INTTOSTR(STRTOINT(cellVal) + 1);
                        END IF;
                        ASK nTable TO SetText(cellVal, nCol, nRow);
                     ELSIF nMode = "Decrement"
                        cellVal := nTable.Text(nCol, nRow);
                        IF ((cellVal = "L") OR (cellVal = "1") OR (nTable.Text(1,nRow) = "0"))
                           cellVal := "L";
                        ELSIF ((cellVal = "C") AND (nTable.Text(1,nRow) = "0"))
                           cellVal := "C";
                        ELSIF (cellVal = "C") 
                           cellVal := nTable.Text(1, nRow);
                        ELSE 
                           cellVal := INTTOSTR(STRTOINT(nTable.Text(nCol, nRow)) - 1);
                        END IF;
                        ASK nTable TO SetText(cellVal, nCol, nRow);
                     ELSIF nMode = "Link"
                        ASK nTable TO SetText("L", nCol, nRow);
                     ELSIF nMode = "Cut"
                        ASK nTable TO SetText("C", nCol, nRow);
                     ELSE
                        IF ( STRTOINT(nTable.Text(1, nRow)) >= STRTOINT(nText) )
                           IF nText = "0"
                              ASK nTable TO SetText("L", nCol, nRow);
                           ELSE
                              ASK nTable TO SetText(nText, nCol, nRow);
                           END IF;
                        ELSE
                           INC(errors);
                           text[errors] := "The k value for node '"+nTable.Text(0,nRow)+"' must be between 0 and "+nTable.Text(1,nRow)+"!     ";
                           IF (nTable.Text(1,nRow) = "0")
                             ASK kValBox TO DisplayValue(0.0);
                             ASK nTable TO SetText("L", nCol, nRow);
                           ELSE
                             ASK kValBox TO DisplayValue(1.0);
                             ASK nTable TO SetText("1", nCol, nRow);
                           END IF;
                        END IF;
                     END IF;
                  END IF;
                  {Set up phase name buttons}
                  IF ((nRow = 0) AND (nCol > 2) AND (nCol <= tempTotalPhases + 2))
                     ASK NPhaseLabel TO SetHidden(FALSE);
                     ASK NPhaseLabel TO SetLabel(dTable.Text(1,nCol - 2));
                     Draw;
                     Delay(2);
                     ASK NPhaseLabel TO SetHidden(TRUE);
                  END IF;
               WHEN 303:    {k=n for all}
                 FOR i := 1 TO phaseNodes
                    cellVal := nTable.Text(1, i);
                    IF cellVal = "0"
                       cellVal := "L";
                    END IF;
                    FOR j := 3 TO tempTotalPhases+2                     
                       ASK nTable TO SetText(cellVal, j, i);
                    END FOR;
                 END FOR;
               WHEN 304:    {k=1 for all}
                  FOR i := 1 TO phaseNodes
                     IF (nTable.Text(1,i) = "0")
                        INC(errors);
                        text[errors] := "The k value for node '"+nTable.Text(0,i)+"' must be 0!     ";
                     ELSE
                        FOR j := 3 TO tempTotalPhases+2                     
                           ASK nTable TO SetText("1", j, i);
                        END FOR;
                     END IF;
                  END FOR;
               WHEN 305:    {reset all}
                  FOR i := 1 TO phaseNodes
                     cellVal := nTable.Text(2, i);
                     IF cellVal = "0"
                       cellVal := "L";
                     END IF;
                     FOR j := 3 TO tempTotalPhases+2                     
                        ASK nTable TO SetText(cellVal, j, i);
                     END FOR;
                  END FOR;
               WHEN 306:    {reset row}
                  IF nRow > 0
                     cellVal := nTable.Text(2, nRow);
                     IF cellVal = "0"
                          cellVal := "L";
                     END IF;
                     FOR j := 3 TO tempTotalPhases+2                     
                        ASK nTable TO SetText(cellVal, j, nRow);
                     END FOR;
                  END IF;
               WHEN 307:    {mode combo}
                  IF nMode = "k-value"
                     ASK kValBox TO Activate;
                  ELSE
                     ASK kValBox TO Deactivate;
                  END IF;
               WHEN 602:    {Print}
                  IF compileType <> "demo"
                     PrintNodePhases(nTable);
                  ELSE
                     NEW(message, 1..1);
                     message[1] := "The demo version has no print capability     ";
                     result := SendAlert(message, FALSE, FALSE, TRUE);
                     DISPOSE(message);
                  END IF;
               OTHERWISE
            END CASE;
         WHEN 400:  {Assign Event Tab}
            ASK pTextBox TO Deactivate;
            ASK pValBox TO Deactivate;

            eMode := eCombo.Text();
            IF eMode = "Fire"
               eText := "F";
            ELSIF eMode = "Link"
               eText := "L";
            ELSIF eMode = "Cut"
               eText := "C";
            ELSIF eMode = "Prior"
               eText := "P";
            ELSIF eMode = "p-value";
               ASK pTextBox TO Activate;
               ASK pValBox TO Activate;
               IF (pValBox.Value() < 0.) OR (pValBox.Value() > 1.);
                  INC(errors);
                  text[errors] := "p value must be between 0 and 1!     ";
                  ASK pValBox TO DisplayValue(1.0);
                  eText := "1";
               ELSE
                  eText := REALTOSTR(pValBox.Value());
               END IF;
            END IF;
            eCol := eTable.SelectedColumn;
            eRow := eTable.SelectedRow;
            IF (eCol > 0) AND (eRow > 0)
               oldECol := eCol;
               oldERow := eRow;
            END IF;
            ASK eTable TO SetSelected(oldECol, oldERow);
            lastClicked := objClicked.LastPicked;
            CASE lastClicked.Id
               WHEN 401:    {event table}
                  ASK applyButton TO Activate;
                  IF ((eRow > 0) AND (eCol > 1) AND (eRow <= phaseEvents))
                     IF eCol > tempTotalPhases + 1
                        eCol := tempTotalPhases + 1;
                        ASK eTable TO SetSelected(eCol, eRow);
                     END IF;
                     ASK e1Butt TO Activate;
                     ASK e2Butt TO Activate;
                     ASK e3Butt TO Activate;
                     ASK e4Butt TO Activate;
                     cellVal := eTable.Text(eCol, eRow);
                     IF cellVal <> eText
                        ASK eTable TO SetText(eText, eCol, eRow);
                     END IF;
                  END IF;
                  {Set up phase name buttons}
                  IF ((eRow = 0) AND (eCol > 1) AND (eCol <= tempTotalPhases + 1))
                     ASK EPhaseLabel TO SetHidden(FALSE);
                     ASK EPhaseLabel TO SetLabel(dTable.Text(1,eCol - 1));
                     Draw;
                     Delay(2);
                     ASK EPhaseLabel TO SetHidden(TRUE);
                  END IF;
               WHEN 402:    {Mark Row}
                  IF eRow > 0
                     FOR i := 2 TO tempTotalPhases + 1                     
                        ASK eTable TO SetText(eText, i, eRow);
                     END FOR;
                  END IF;
               WHEN 403:    {Reset Row}
                  IF eRow > 0
                     ASK eTable TO SetText("F", 2, eRow);
                     FOR i := 3 TO tempTotalPhases + 1                     
                        ASK eTable TO SetText("P", i, eRow);
                     END FOR;
                  END IF;
               WHEN 404:    {Mark Column}
                  IF eCol > 1
                     FOR i := 1 TO phaseEvents                      
                        ASK eTable TO SetText(eText, eCol, i);
                     END FOR;
                  END IF;
               WHEN 405:    {Reset Column}
                  IF eCol > 1  {Don't reset Base p column}
                     IF eCol = 2
                        FOR i := 1 TO phaseEvents                   
                           ASK eTable TO SetText("F", eCol, i);
                        END FOR;
                     ELSE
                        FOR i := 1 TO phaseEvents                   
                           ASK eTable TO SetText("P", eCol, i);
                        END FOR;
                     END IF;
                  END IF;
               WHEN 603:    {Print}
                  IF compileType <> "demo"
                     PrintEventPhases(eTable);
                  ELSE
                     NEW(message, 1..1);
                     message[1] := "The demo version has no print capability     ";
                     result := SendAlert(message, FALSE, FALSE, TRUE);
                     DISPOSE(message);
                  END IF;
              OTHERWISE
            END CASE;
         WHEN 500:  {Assign Hierarchy Tab}
            hMode := hCombo.Text();
            IF hMode = "Active"
               hText := "A";
            ELSIF hMode = "Link"
               hText := "L";
            ELSIF hMode = "Cut"
               hText := "C";
            END IF;
            hCol := hTable.SelectedColumn;
            hRow := hTable.SelectedRow;
            IF (hCol > 0) AND (hRow > 0)
               oldHCol := hCol;
               oldHRow := hRow;
            END IF;
            ASK hTable TO SetSelected(oldHCol, oldHRow);
            lastClicked := objClicked.LastPicked;
            CASE lastClicked.Id
               WHEN 501:    {hierarchy table}
                  ASK applyButton TO Activate;
                  IF ((hRow > 0) AND (hCol > 0) AND (hRow <= phaseHiers))
                     IF hCol > tempTotalPhases
                        hCol := tempTotalPhases;
                        ASK hTable TO SetSelected(hCol, hRow);
                     END IF;
                     ASK h1Butt TO Activate;
                     ASK h2Butt TO Activate;
                     ASK h3Butt TO Activate;
                     ASK h4Butt TO Activate;
                     cellVal := hTable.Text(hCol, hRow);
                     IF cellVal <> hText
                        ASK hTable TO SetText(hText, hCol, hRow);
                     END IF;
                  END IF;
                  {Set up phase name buttons}
                  IF ((hRow = 0) AND (hCol > 0) AND (hCol <= tempTotalPhases))
                     ASK HPhaseLabel TO SetHidden(FALSE);
                     ASK HPhaseLabel TO SetLabel(dTable.Text(1,hCol));
                     Draw;
                     Delay(2);
                     ASK HPhaseLabel TO SetHidden(TRUE);
                  END IF;
               WHEN 503:    {Set Row}
                  IF hRow > 0
                     FOR i := 1 TO tempTotalPhases                     
                        ASK hTable TO SetText(hText, i, hRow);
                     END FOR;
                  END IF;
               WHEN 504:    {Set Column}
                  IF hCol > 0
                     FOR i := 1 TO phaseHiers                      
                        ASK hTable TO SetText(hText, hCol, i);
                     END FOR;
                  END IF;
               WHEN 505:    {Reset Row}
                  IF hRow > 0
                     FOR i := 1 TO tempTotalPhases                    
                        ASK hTable TO SetText("A", i, hRow);
                     END FOR;
                  END IF;
               WHEN 506:    {Reset Column}
                  IF hCol > 0
                     FOR i := 1 TO phaseHiers                   
                        ASK hTable TO SetText("A", hCol, i);
                     END FOR;
                  END IF;
               WHEN 604:    {Print}
                  IF compileType <> "demo"
                     PrintHierPhases(hTable);
                  ELSE
                     NEW(message, 1..1);
                     message[1] := "The demo version has no print capability     ";
                     result := SendAlert(message, FALSE, FALSE, TRUE);
                     DISPOSE(message);
                  END IF;
              OTHERWISE
            END CASE;
         OTHERWISE
      END CASE;
      IF errors > 1
         {Erase;}
         NEW(message, 1..errors+2);
         message[1] := "The following errors must be corrected:   ";
         message[2] := "";
         FOR i := 1 TO errors
            message[i+2] := "   "+INTTOSTR(i)+". "+text[i];
         END FOR;
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
         {IF phaseBlocks = 0
            ASK bTab TO Deactivate;
         END IF;
         IF phaseNodes = 0
            ASK nTab TO Deactivate;
         END IF;
         IF phaseEvents = 0
            ASK eTab TO Deactivate;
         END IF;}
         Draw;
      ELSIF errors = 1
         {Erase;}
         NEW(message, 1..1);
         message[1] := text[1];
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
         {IF phaseBlocks = 0
            ASK bTab TO Deactivate;
         END IF;
         IF phaseNodes = 0
            ASK nTab TO Deactivate;
         END IF;
         IF phaseEvents = 0
            ASK eTab TO Deactivate;
         END IF;}
         Draw;
      END IF;
      DISPOSE(text);
      Update;
      INHERITED BeSelected;
   END METHOD; {BeSelected}
   
   ASK METHOD FillTables;
   VAR
      i, j, pos, index               : INTEGER;
      cell,currentBlock,currentNode, currentHier  : STRING;
      block                          : RBDBlockObj;
      event                          : RBDEventObj;
      node                           : RBDNodeObj;
      hier                           : RBDHierObj;
   BEGIN
      IF phaseBlocks>0
         IF translateBArray <> NILARRAY
            DISPOSE(translateBArray);
         END IF;
         NEW(translateBArray, 1..phaseBlocks, 1..2);
         pos := 1;
         index:=1;
         FOREACH block IN blockGroup
            IF block.usesPhasing         
               translateBArray[index, 1] := block.Id;
               currentBlock := block.name;           
               cell := bTable.Text(0, index-1);
               IF ((index = 1)OR(currentBlock >= cell))
                  translateBArray[index, 2] := block.Id;
                  ASK bTable TO SetText(block.name, 0, index);
               ELSE
                  pos := 1;
                  cell := bTable.Text(0, pos);
                  WHILE(currentBlock > cell)
                     INC(pos);
                     cell := bTable.Text(0, pos);                  
                  END WHILE;
                  FOR j := index DOWNTO (pos+1)
                     translateBArray[j, 2] := translateBArray[j-1, 2];
                     cell := bTable.Text(0, j-1);
                     ASK bTable TO SetText(cell, 0, j);
                  END FOR;
                  translateBArray[pos, 2] := block.Id;
                  ASK bTable TO SetText(block.name, 0, pos);
               END IF;
               INC(index);
            END IF;
         END FOREACH;
      END IF;
      {Create Event Array}
      IF phaseEvents>0
         IF translateEArray <> NILARRAY
            DISPOSE(translateEArray);
         END IF;
         NEW(translateEArray, 1..phaseEvents, 1..2);
         pos := 1;
         index:=1;
         FOREACH event IN eventGroup
            IF event.usesPhasing         
               translateEArray[index, 1] := event.Id;
               currentBlock := event.name;           
               cell := eTable.Text(0, index-1);
               IF ((index = 1)OR(currentBlock >= cell))
                  translateEArray[index, 2] := event.Id;
                  ASK eTable TO SetText(event.name, 0, index);
                  ASK eTable TO SetText(REALTOSTR(event.failVals[1]), 1, index);
               ELSE
                  pos := 1;
                  cell := eTable.Text(0, pos);
                  WHILE(currentBlock > cell)
                     INC(pos);
                     cell := eTable.Text(0, pos);                  
                  END WHILE;
                  FOR j := index DOWNTO (pos+1)
                     translateEArray[j, 2] := translateEArray[j-1, 2];
                     cell := eTable.Text(0, j-1);
                     ASK eTable TO SetText(cell, 0, j);
                     cell := eTable.Text(1, j-1);
                     ASK eTable TO SetText(cell, 1, j);
                  END FOR;
                  translateEArray[pos, 2] := event.Id;
                  ASK eTable TO SetText(event.name, 0, pos);
                  ASK eTable TO SetText(REALTOSTR(event.failVals[1]), 1, pos);
               END IF;
               INC(index);
            END IF;
         END FOREACH; 
      END IF;
      j := 1;
      IF phaseNodes>0
         IF translateNArray <> NILARRAY
            DISPOSE(translateNArray);
         END IF;
         NEW(translateNArray, 1..phaseNodes, 1..2);
         pos := 1;
         index:=1;
         FOREACH node IN nodeGroup
            IF ((node.typeNode <> 5) AND (node.usesPhasing))
               translateNArray[index, 1] := node.Id;
               currentNode := node.name;           
               cell := nTable.Text(0, index-1);
               IF ((index = 1)OR(currentNode >= cell))
                  translateNArray[index, 2] := node.Id;
                  ASK nTable TO SetText(node.name, 0, index);
                  ASK nTable TO SetText(INTTOSTR(node.goodPaths), 2, index);
                  ASK nTable TO SetText(INTTOSTR(node.connectIntoNum), 1, index);
               ELSE
                  pos := 1;
                  cell := nTable.Text(0, pos);
                  WHILE(currentNode > cell)
                     INC(pos);
                     cell := nTable.Text(0, pos);                  
                  END WHILE;
                  FOR j := index DOWNTO (pos+1)
                     translateNArray[j, 2] := translateNArray[j-1, 2];
                     cell := nTable.Text(0, j-1);
                     ASK nTable TO SetText(cell, 0, j);
                     cell := nTable.Text(1, j-1);
                     ASK nTable TO SetText(cell, 1, j);
                     cell := nTable.Text(2, j-1);
                     ASK nTable TO SetText(cell, 2, j);
                  END FOR;
                  translateNArray[pos, 2] := node.Id;
                  ASK nTable TO SetText(node.name, 0, pos);
                  ASK nTable TO SetText(INTTOSTR(node.goodPaths), 2, pos);
                  ASK nTable TO SetText(INTTOSTR(node.connectIntoNum), 1, pos);
               END IF;
               INC(index);
            END IF;
         END FOREACH;
      END IF;     
              
      IF phaseHiers>0
         IF translateHArray <> NILARRAY
            DISPOSE(translateHArray);
         END IF;
         NEW(translateHArray, 1..phaseHiers, 1..2);
         pos := 1;
         index:=1;
         FOREACH hier IN hierGroup
            IF hier.usesPhasing        
               translateHArray[index, 1] := hier.Id;
               currentHier := hier.name;           
               cell := hTable.Text(0, index-1);
               IF ((index = 1)OR(currentHier >= cell))
                  translateHArray[index, 2] := hier.Id;
                  ASK hTable TO SetText(hier.name, 0, index);
               ELSE
                  pos := 1;
                  cell := hTable.Text(0, pos);
                  WHILE(currentHier > cell)
                     INC(pos);
                     cell := hTable.Text(0, pos);                  
                  END WHILE;
                  FOR j := index DOWNTO (pos+1)
                     translateHArray[j, 2] := translateHArray[j-1, 2];
                     cell := hTable.Text(0, j-1);
                     ASK hTable TO SetText(cell, 0, j);
                     cell := hTable.Text(1, j-1);
                     ASK hTable TO SetText(cell, 1, j);
                  END FOR;
                  translateHArray[pos, 2] := hier.Id;
                  ASK hTable TO SetText(hier.name, 0, pos);
               END IF;
               INC(index);
            END IF;
         END FOREACH; 
      END IF;
   END METHOD;   {FillTables}
END OBJECT;
                     
OBJECT PrefsBoxObj;
   ASK METHOD ReceiveData;
   VAR
      validData, updateNodes, tempSaveIsOn, tempSoundIsOn,
      tempMuSigmaMode, tempLambdaMode, cancelled      : BOOLEAN;
      total,i, tempSaveInc, diff                       : INTEGER;
      textPool                                        : ARRAY INTEGER OF STRING;
      idiotAlert                                      : TextBufferType;
      tempString, tempGlobalUnits, tempGlobalImage                : STRING;
      defaultStream : StreamObj;
   BEGIN
      cancelled    := FALSE;
      
      {General Tab}
      soundChk    := Descendant("SoundChkBox", 101);
      saveChk     := Descendant("SaveChkBox", 102);
      saveVal     := Descendant("SaveVal", 103);
      unitsBox    := Descendant("UnitsCombo", 104);
      sysStatus   := Descendant("SysStatusBox", 105);
      {Parameters Tab}
      expoRadBox  := Descendant("ExpoRadBox", 201);
      mtbfButton  := ASK expoRadBox Child("MTBFButton", 2011);
      fRateButton := ASK expoRadBox Child("FRateButton", 2012);
      logRadBox   := Descendant("LogRadBox", 202);
      meanButton  := ASK logRadBox Child("MeanButton", 2021);
      muButton    := ASK logRadBox Child("MuButton", 2022);
      
      NEW(textPool, 1..20);
      
      {General Tab Setup}
      IF soundPath <> ""
         IF soundIsOn
            ASK soundChk TO SetCheck(TRUE);
         ELSE
            ASK soundChk TO SetCheck(FALSE);
         END IF;
      ELSE
         ASK soundChk TO Deactivate;
      END IF;
      
      IF saveIsOn
         ASK saveChk TO SetCheck(TRUE);
         ASK saveVal TO SetValue(FLOAT(saveInc)/60.);
      ELSE
         ASK saveChk TO SetCheck(FALSE);
         ASK saveVal TO Deactivate;
      END IF;
      IF globalUnits <> ""
         ASK unitsBox TO SetText(globalUnits);
      END IF;   

      ASK sysStatus TO SetText(globalImage);

      {Parameters Tab Setup}
      IF lambdaMode
         ASK expoRadBox TO SetSelectedButton(fRateButton);
      ELSE
         ASK expoRadBox TO SetSelectedButton(mtbfButton);
      END IF;
      IF muSigmaMode
         ASK logRadBox TO SetSelectedButton(muButton);
      ELSE
         ASK logRadBox TO SetSelectedButton(meanButton);
      END IF;
       
      
{Snag User Data}
      Draw;
      REPEAT
         errors := 0;
         validData := TRUE;
         button := AcceptInput();        
         IF button.ReferenceName = "OKButton"
            tempSoundIsOn  := soundChk.Checked;
            tempGlobalImage := sysStatus.Text();
            tempGlobalImage := (UPPER(SUBSTR(1,1,tempGlobalImage))+(SUBSTR(2, STRLEN(tempGlobalImage), tempGlobalImage)));
            IF sysStatus.Text() <> globalImage
               somethingChanged := TRUE;
            END IF;
            tempSaveIsOn := saveChk.Checked;
            IF tempSaveIsOn AND (saveVal.Value <5.)
               NEW(idiotAlert,1..3);
               idiotAlert[1] := "The auto recovery feature backs up your entire file     ";
               idiotAlert[2] := "at the specified interval.  Inteval values less than     ";
               idiotAlert[3] := "5 minutes are not allowed for performance reasons.       ";
               result := SendAlert(idiotAlert, FALSE, FALSE, FALSE);
               ASK saveVal TO SetValue(5.);
               {ASK saveVal TO Draw;}
               validData := FALSE;
               DISPOSE(idiotAlert);
            ELSIF tempSaveIsOn AND (saveVal.Value > 2160.)
               NEW(idiotAlert,1..3);
               IF saveVal.Value() > 16666666.
                  idiotAlert[1] := "Hello McFly!!!  You have chosen to autosave every "+INTTOSTR(ROUND(saveVal.Value()/76200.))+" years.     ";
                  idiotAlert[2] := "What if your great-grandchildren turn the computer off by then?     ";
                  idiotAlert[3] := "Maybe you should try something slightly shorter like one hour. ";
                  result := SendAlert(idiotAlert, FALSE, FALSE, FALSE);
                  ASK saveVal TO SetValue(60.);
                  {ASK saveVal TO Draw;}
                  validData := FALSE;
               ELSE
                  idiotAlert[1] := "Hello McFly!!!  You have chosen to autosave every "+INTTOSTR(ROUND(saveVal.Value()/1440.))+" days.  How     ";
                  idiotAlert[2] := "about trimming it down to less than one day.  We actually recommend     ";
                  idiotAlert[3] := "one hour.  Do you really want to continue with "+INTTOSTR(ROUND(saveVal.Value()/1440.))+" days?";
                  result := SendAlert(idiotAlert, FALSE, TRUE, FALSE);
                  IF result
                     tempSaveInc := TRUNC(saveVal.Value)*60; 
                  ELSE
                     ASK saveVal TO SetValue(15.);
                     {ASK saveVal TO Draw;}
                     validData := FALSE;
                  END IF;
               END IF;
               DISPOSE(idiotAlert);
            ELSE
               tempSaveInc := TRUNC(saveVal.Value)*60; 
            END IF;
            IF FLOAT(ROUND(saveVal.Value)) <> saveVal.Value
               INC(errors);
               textPool[errors] := "The save increment must be an integer!     ";
               ASK saveVal TO SetValue(15.);
               ASK saveVal TO Draw;
               validData := FALSE;
            END IF;
            tempGlobalUnits := unitsBox.Text();
            IF ((tempGlobalUnits = "") OR (POSITION(tempGlobalUnits, " ") <> 0))
               INC(errors);
               textPool[errors] := "The units field can't be blank or have blank spaces!     ";
               ASK unitsBox TO SetText("units");
               ASK unitsBox TO Draw;
               validData := FALSE;
            ELSIF STRLEN(tempGlobalUnits) > 20
               INC(errors);
               textPool[errors] := "The units name must be no greater than 20 characters!     ";
               ASK unitsBox TO SetText(SUBSTR(1,20,tempGlobalUnits));
               ASK unitsBox TO Draw;
               validData := FALSE;
            END IF;
            IF expoRadBox.SelectedButton = mtbfButton   {use MTBF}
               tempLambdaMode := FALSE;
            ELSE
               tempLambdaMode := TRUE;
            END IF;
            IF logRadBox.SelectedButton = meanButton    {use dist. Mean}
               tempMuSigmaMode := FALSE;
            ELSE
               tempMuSigmaMode := TRUE;
            END IF;
         ELSIF button.ReferenceName = "CancelButton"
            cancelled := TRUE;
         ELSE
            NEW(message, 1..1);
            message[1] := "Invalid preference box termination     ";
            result := SendAlert(message, TRUE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
         IF errors > 1
            NEW(message, 1..errors+2);
            message[1] := "The following errors must be corrected:   ";
            message[2] := "";
            FOR i := 1 TO errors
               message[i+2] := "   "+INTTOSTR(i)+". "+textPool[i];
            END FOR;
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         ELSIF errors = 1
            NEW(message, 1..1);
            message[1] := textPool[1];
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
      UNTIL validData;
      IF NOT cancelled
         muSigmaMode := tempMuSigmaMode;
         lambdaMode := tempLambdaMode;
         IF NOT cancelled
            IF tempSaveIsOn <> saveIsOn
               saveIsOn := tempSaveIsOn;
               diff := TRUNC(ClockRealSecs)-lastSave;
               IF tempSaveInc >= diff
                  lastSave := TRUNC(ClockRealSecs) - (tempSaveInc-diff);
               ELSE
                  lastSave := TRUNC(ClockRealSecs);
               END IF;
            END IF;
            soundIsOn := tempSoundIsOn;
            saveInc := tempSaveInc;
            IF tempGlobalImage <> globalImage
               globalImage := tempGlobalImage;
               systemImage := globalImage;
               somethingChanged := TRUE;
            END IF;
            IF tempGlobalUnits <> globalUnits
               globalUnits := tempGlobalUnits;
               systemUnits := globalUnits;
               somethingChanged := TRUE;
            END IF;
            NEW(defaultStream);
            ASK defaultStream TO Open(( {GetProgDir("Raptor7.exe")} userPath + "prefs70.cfg"), Output);  { wds/TES, 8/18/08 }
            IF defaultStream.ioResult <> 0
               NEW(message, 1..2);
               message[1] := "There is a problem opening the prefs70.cfg file.     ";
               message[2] := "Make sure this file has not been set to read only.     ";
               result := SendAlert(message, FALSE, FALSE, FALSE);
               DISPOSE(message);
               RETURN;
            END IF;
            ASK defaultStream TO WriteString("Raptor 7.0 Preferences File");
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteString("-----------------------------------");
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteString("The simulation image is the "+globalImage);
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteString("The units being used are "+globalUnits);
            ASK defaultStream TO WriteLn;
            IF saveIsOn
               ASK defaultStream TO WriteString("AutoSave is ON");
            ELSE
               ASK defaultStream TO WriteString("AutoSave is OFF");
            END IF;
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteString("The AutoSave interval is ");
            ASK defaultStream TO WriteInt(TRUNC(FLOAT(saveInc)/60.),6); 
            ASK defaultStream TO WriteString(" minutes");
            ASK defaultStream TO WriteLn;
            IF soundIsOn
               ASK defaultStream TO WriteString("Sound is ON");
            ELSE
               ASK defaultStream TO WriteString("Sound is OFF");
            END IF;
            ASK defaultStream TO WriteLn;
            IF lambdaMode
               ASK defaultStream TO WriteString("Lambda mode is ON");
            ELSE
               ASK defaultStream TO WriteString("Lambda mode is OFF");
            END IF;
            ASK defaultStream TO WriteLn;
            IF muSigmaMode
               ASK defaultStream TO WriteString("Mu-Sigma mode is ON");
            ELSE
               ASK defaultStream TO WriteString("Mu-Sigma mode is OFF");
            END IF;
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteString(" ");
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteString("Unused variables");
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteString(" ");
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteString(". ");
            ASK defaultStream TO WriteInt(0,1);
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteString(". ");
            ASK defaultStream TO WriteInt(0,1);
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteString(". ");
            ASK defaultStream TO WriteReal(0.,2,1);
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteString(". ");
            ASK defaultStream TO WriteReal(0.,2,1);
            ASK defaultStream TO WriteLn;
            ASK defaultStream TO WriteString("End of File");
            ASK defaultStream TO Close;
            DISPOSE(defaultStream);
         END IF;
      END IF;
      DISPOSE(textPool);
   END METHOD; {ReceiveData}

   ASK METHOD BeSelected;
   VAR
      tabClicked : GraphicVObj;
      tempString : STRING;
   BEGIN
      tabClicked := LastPicked;
      CASE tabClicked.Id
         WHEN 100: {General Tab}
            lastClicked := ASK tabClicked LastPicked;
            CASE lastClicked.Id
               WHEN 102: {Autosave}
                  IF saveChk.Checked
                     ASK saveVal TO Activate;
                  ELSE
                     ASK saveVal TO Deactivate;
                  END IF;
               OTHERWISE
            END CASE;
         OTHERWISE;
      END CASE;
      INHERITED BeSelected;
   END METHOD; {BeSelected}
END OBJECT; {PrefsBoxObj}

OBJECT SystemBoxObj;
   ASK METHOD ReceiveData;
   VAR
      validData, updateNodes, tempdSimWithGraph, cancelled      :BOOLEAN;
      total,i, tempStrA, tempStrB, tempStrC, tempStrD,
      tempStrE, tempStrF, tempStrG, tempStrH, tempStrI,
      tempStrJ, tempStrAnc, tempTermType, tempWeakLinkAnalType, 
      tempFlowGenerated                                         : INTEGER;
      textPool                                                  : ARRAY INTEGER OF STRING;
      idiotAlert                                                : TextBufferType;
      tempString, tempSystemUnits, tempSystemImage              : STRING;
      startStat, tempSysLostCost, tempdNumberOfRuns,
      tempSystemRedCost, tempSimLength, tempdTimeStartTime,
      tempdTimeTrunc, tempdCycleStartTime, tempdCycleTrunc,
      tempdFailTrunc, tempdFailStartTime, tempGYthreshold,
      tempYRthreshold                                           : REAL;
      cycleButton                                               : RadioButtonObj;

   BEGIN
      
      {General Tab}
      unitsBox    := Descendant("UnitsCombo", 101);
      sysStatus   := Descendant("SysStatusBox", 102);
      capValBox   := Descendant("CapValBox", 103);
      volumeText  := Descendant("VolumeText", 313);
      lostCostBox := Descendant("LostCostBox", 104);
      lostText1   := Descendant("LostText", 314);
      lostText2   := Descendant("LostText", 315); 
      redCostBox  := Descendant("RedCostBox", 105);
      negValChkBox := Descendant("NegValChkBox", 106);
      commentBox   := Descendant("CommentBox", 107);
      unitsLabel1  := Descendant("UnitsLabel", 1);
      unitsLabel2  := Descendant("UnitsLabel", 2);
      unitsLabel3  := Descendant("UnitsLabel", 3);
      {Simulation Tab}                              
      stopRadBox   := Descendant("StopRadBox", 201);
      simTimeVal   := Descendant("SimTimeVal", 202);
      simFailVal   := Descendant("SimFailVal", 203);
      simCycleVal  := Descendant("SimCycleVal", 204);
      stopLabel1   := Descendant("StopLabel1", 1);
      stopLabel2   := Descendant("StopLabel2", 2);
      stopLabel3   := Descendant("StopLabel3", 3);
      simRunsVal   := Descendant("SimRunsVal", 205);
      startTimeVal := Descendant("StartTimeVal", 206);
      startLabel2  := Descendant("StartLabel2", 207);
      graphicsChkBox := Descendant("GraphicsChkBox", 208);
      {System Streams Tab}
      streamA       := Descendant("StreamACombo", 301);
      antiStreamA   := Descendant("StreamAChkBox", 302);
      streamB       := Descendant("StreamBCombo", 303);
      antiStreamB   := Descendant("StreamBChkBox", 304);
      streamC       := Descendant("StreamCCombo", 305);
      antiStreamC   := Descendant("StreamCChkBox", 306);
      streamD       := Descendant("StreamDCombo", 307);
      antiStreamD   := Descendant("StreamDChkBox", 308);
      streamE       := Descendant("StreamECombo", 309);
      antiStreamE   := Descendant("StreamEChkBox", 310);
      streamF       := Descendant("StreamFCombo", 311);
      antiStreamF   := Descendant("StreamFChkBox", 312);
      streamG       := Descendant("StreamGCombo", 313);
      antiStreamG   := Descendant("StreamGChkBox", 314);
      streamH       := Descendant("StreamHCombo", 315);
      antiStreamH   := Descendant("StreamHChkBox", 316);
      streamI       := Descendant("StreamICombo", 317);
      antiStreamI   := Descendant("StreamIChkBox", 318);
      streamJ       := Descendant("StreamJCombo", 319);
      antiStreamJ   := Descendant("StreamJChkBox", 320);
      ancStreamText := Descendant("AncStreamText", 321);
      ancStream     := Descendant("AncStreamCombo", 322);
      antiAncStream := Descendant("AncStreamChkBox", 323);
      {Weak Link Analysis Tab}
      weakLinkTab  := Child("WeakLinkTab", 400);
      weakLinkRad := Descendant("WeakLinkAnalType", 401);
      availButt   := ASK weakLinkRad Child("AvailButt", 4011);
      dependButt  := ASK weakLinkRad Child("DependButt", 4012);
      relyButt    := ASK weakLinkRad Child("RelyButt", 4013);
      gyThreshVal := Descendant("GYThreshVal", 402);
      yrThreshVal := Descendant("YRThreshVal", 403);
      yellowText  := Descendant("YellowText", 404);
      redText     := Descendant("RedText", 405);
      
      NEW(textPool, 1..20);
      
      {General Tab Setup}
      IF systemUnits <> ""
         ASK unitsBox TO SetText(systemUnits); 
         ASK unitsLabel1 TO SetLabel(systemUnits);
         ASK unitsLabel2 TO SetLabel(systemUnits);
         ASK unitsLabel3 TO SetLabel(systemUnits);
      END IF;   
      ASK sysStatus TO SetText(systemImage); 
      ASK capValBox TO SetValue(FLOAT(flowGenerated));
      ASK lostCostBox TO SetValue(sysLostCost);
      ASK redCostBox TO SetValue(systemRedCost);
      ASK negValChkBox TO SetCheck(negShutUp);
      IF sysComment <> ""
         ASK commentBox TO SetText(sysComment);
      ELSE
         ASK commentBox TO SetText("Comment");
      END IF;
      {Simulation Tab Setup}
      ASK simTimeVal  TO SetValue(dTimeTrunc);
      ASK simRunsVal TO SetValue(dNumberOfRuns);
      ASK simFailVal TO SetValue(dFailTrunc);
      ASK simCycleVal TO SetValue(dCycleTrunc);
      ASK stopLabel1 TO SetLabel(systemUnits);
      IF termType = 1 {time truncated}
         ASK startLabel2 TO SetLabel(systemUnits);
         ASK stopRadBox TO SetSelectedButton(ASK stopRadBox Child("TimeButton", 2011));
         ASK simFailVal TO SetSelectable(FALSE);
         ASK simCycleVal TO SetSelectable(FALSE);
         ASK stopLabel2 TO Deactivate;
         ASK stopLabel3 TO Deactivate;
         ASK startTimeVal TO SetValue(dTimeStartTime);
      ELSIF termType = 2 {failure truncated}
         ASK startLabel2 TO SetLabel("failures");
         ASK stopRadBox TO SetSelectedButton(ASK stopRadBox Child("FailButton", 2012));
         ASK simTimeVal  TO SetSelectable(FALSE);
         ASK simCycleVal TO SetSelectable(FALSE);
         IF dFailTrunc = 1.
            ASK startTimeVal TO SetSelectable(FALSE);
         END IF;
         ASK stopLabel1 TO Deactivate;
         ASK stopLabel3 TO Deactivate;
         ASK startTimeVal TO SetValue(dFailStartTime);
      ELSIF termType = 3 {cycle truncated}
         IF activePhases = 0
            NEW(message, 1..3);
            message[1] := "You cannot use cycle terminated without phases defined.    "
            message[2] := "The termination type has been returned to the default     ";
            message[3] := "of time terminated.     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
            termType := 1;
            ASK startLabel2 TO SetLabel(systemUnits);
            ASK stopRadBox TO SetSelectedButton(ASK stopRadBox Child("TimeButton", 2011));
            ASK simFailVal TO SetSelectable(FALSE);
            ASK simCycleVal TO SetSelectable(FALSE);
            ASK stopLabel2 TO Deactivate;
            ASK stopLabel3 TO Deactivate;
            ASK startTimeVal TO SetValue(dTimeStartTime);
         ELSE
            ASK startLabel2 TO SetLabel("cycles");
            ASK stopRadBox TO SetSelectedButton(ASK stopRadBox Child("CycleButton", 2013));
            ASK simTimeVal TO SetSelectable(FALSE);
            ASK simFailVal TO SetSelectable(FALSE);
            ASK stopLabel1 TO Deactivate;
            ASK stopLabel2 TO Deactivate;
            ASK startTimeVal TO SetValue(dCycleStartTime);
         END IF;
      END IF;
      IF activePhases = 0
         cycleButton := ASK stopRadBox Child("CycleButton", 2013);
         ASK cycleButton TO Deactivate;
      END IF;
      IF dSimWithGraph
         ASK graphicsChkBox TO SetCheck(TRUE);
      ELSE
         ASK graphicsChkBox TO SetCheck(FALSE);
      END IF;
  
      {System Stream Tab Setup} 
      tempStrA := sysStreams[1];
      tempStrB := sysStreams[2];
      tempStrC := sysStreams[3];
      tempStrD := sysStreams[4];
      tempStrE := sysStreams[5];
      tempStrF := sysStreams[6];
      tempStrG := sysStreams[7];
      tempStrH := sysStreams[8];
      tempStrI := sysStreams[9];
      tempStrJ := sysStreams[10];
      tempStrAnc := sysStreams[11];
      
      IF tempStrA < 0
         tempStrA := -1*tempStrA;
         ASK antiStreamA TO SetCheck(TRUE);
      END IF;
      IF tempStrB < 0
         tempStrB := -1*tempStrB;
         ASK antiStreamB TO SetCheck(TRUE);
      END IF;
      IF tempStrC < 0
         tempStrC := -1*tempStrC;
         ASK antiStreamC TO SetCheck(TRUE);
      END IF;
      IF tempStrD < 0
         tempStrD := -1*tempStrD;
         ASK antiStreamD TO SetCheck(TRUE);
      END IF;
      IF tempStrE < 0
         tempStrE := -1*tempStrE;
         ASK antiStreamE TO SetCheck(TRUE);
      END IF;
      IF tempStrF < 0
         tempStrF := -1*tempStrF;
         ASK antiStreamF TO SetCheck(TRUE);
      END IF;
      IF tempStrG < 0
         tempStrG := -1*tempStrG;
         ASK antiStreamG TO SetCheck(TRUE);
      END IF;
      IF tempStrH < 0
         tempStrH := -1*tempStrH;
         ASK antiStreamH TO SetCheck(TRUE);
      END IF;
      IF tempStrI < 0
         tempStrI := -1*tempStrI;
         ASK antiStreamI TO SetCheck(TRUE);
      END IF;
      IF tempStrJ < 0
         tempStrJ := -1*tempStrJ;
         ASK antiStreamJ TO SetCheck(TRUE);
      END IF;
      IF tempStrAnc < 0
         tempStrAnc := -1*tempStrAnc;
         ASK antiAncStream TO SetCheck(TRUE);
      END IF;
      

      ASK streamA TO SetText(INTTOSTR(tempStrA));
      ASK streamB TO SetText(INTTOSTR(tempStrB));
      ASK streamC TO SetText(INTTOSTR(tempStrC));
      ASK streamD TO SetText(INTTOSTR(tempStrD));
      ASK streamE TO SetText(INTTOSTR(tempStrE));
      ASK streamF TO SetText(INTTOSTR(tempStrF));
      ASK streamG TO SetText(INTTOSTR(tempStrG));
      ASK streamH TO SetText(INTTOSTR(tempStrH));
      ASK streamI TO SetText(INTTOSTR(tempStrI));
      ASK streamJ TO SetText(INTTOSTR(tempStrJ));
      ASK ancStream TO SetText(INTTOSTR(tempStrAnc));
        
      {Weak Link Analysis Tab Setup}
      ASK gyThreshVal TO SetValue(GYthreshold);
      tempString := REALTOSTR(GYthreshold) + "000000";
      ASK yellowText TO SetLabel("<=  Yellow  <  " + SUBSTR(1,8,tempString));
     
      ASK yrThreshVal TO SetValue(YRthreshold);
      tempString := REALTOSTR(YRthreshold) + "000000";
      ASK redText TO SetLabel("0.0         <=  Red      <  " + SUBSTR(1,8,tempString));
      
      IF weakLinkAnalType = 1
         ASK weakLinkRad TO SetSelectedButton(availButt);
      ELSIF weakLinkAnalType = 2
         ASK weakLinkRad TO SetSelectedButton(dependButt);
      ELSE
         ASK weakLinkRad TO SetSelectedButton(relyButt);
      END IF;
      tempSystemImage   := systemImage;
      tempFlowGenerated := flowGenerated;
      tempSysLostCost   := sysLostCost;
      tempSystemRedCost := systemRedCost;
      tempSimLength       := simLength;
      tempdTimeStartTime  := dTimeStartTime;
      tempdTimeTrunc      := dTimeTrunc;
      tempdCycleStartTime := dCycleStartTime;
      tempdCycleTrunc     := dCycleTrunc;
      tempdFailTrunc      := dFailTrunc;
      tempdFailStartTime  := dFailStartTime;
      tempTermType        := termType;
      tempdNumberOfRuns   := dNumberOfRuns;
      tempdSimWithGraph   := dSimWithGraph;
      tempWeakLinkAnalType := weakLinkAnalType;
      tempGYthreshold := GYthreshold;
      tempYRthreshold := YRthreshold;
      
      IF compileType = "student"
         ASK lostCostBox TO Deactivate;
         ASK lostText1 TO Deactivate;
         ASK redCostBox TO Deactivate;
         ASK lostText2 TO Deactivate;
         ASK capValBox TO Deactivate;
         ASK volumeText TO Deactivate;
         ASK unitsLabel1 TO Deactivate;
         ASK unitsLabel2 TO Deactivate;
         ASK unitsLabel3 TO Deactivate;
         ASK weakLinkTab TO Deactivate;
         ASK weakLinkRad TO Deactivate;
         ASK gyThreshVal TO Deactivate;
         ASK yrThreshVal TO Deactivate;
         ASK yellowText TO Deactivate;
         ASK redText TO Deactivate;
      END IF;
{Snag User Data}
      Draw;
      REPEAT
         errors := 0;
         validData := TRUE;
         button := AcceptInput();        
         IF button.ReferenceName = "OKButton"
            somethingChanged := TRUE;
            {General}
            tempSystemUnits := unitsBox.Text(); {systemUnits}
            IF ((tempSystemUnits = "") OR (POSITION(tempSystemUnits, " ") <> 0))
               INC(errors);
               textPool[errors] := "The units field can't be blank or have blank spaces!     ";
               ASK unitsBox TO SetText("units");
               ASK unitsBox TO Draw;
               validData := FALSE;
            ELSIF STRLEN(tempSystemUnits) > 20
               INC(errors);
               textPool[errors] := "The units name must be no greater than 20 characters!     ";
               ASK unitsBox TO SetText(SUBSTR(1,20,tempSystemUnits));
               ASK unitsBox TO Draw;
               validData := FALSE;
            END IF;
            IF sysStatus.Text() <> systemImage
               tempSystemImage := sysStatus.Text();
               somethingChanged := TRUE;
            END IF;
            IF ((capValBox.Value < 0.0) OR (capValBox.Value() > 999999999.))
               INC(errors);
               textPool[errors] := "Volume of capacity generated must be between 0 and 999,999,999!     ";
               ASK capValBox TO DisplayValue(1.0);
               validData := FALSE;
            ELSE
               tempFlowGenerated := TRUNC(capValBox.Value);
            END IF;
            IF ((lostCostBox.Value < 0.0) OR (lostCostBox.Value() > 999999999.))
               INC(errors);
               textPool[errors] := "Cost of a lost unit of volume must be between 0 and 999,999,999!     ";
               ASK lostCostBox TO DisplayValue(1.0);
               validData := FALSE;
            ELSE
               tempSysLostCost := lostCostBox.Value;
            END IF;
            IF ((redCostBox.Value < 0.0) OR (redCostBox.Value() > 999999999.))
               INC(errors);
               textPool[errors] := "The cost of system red time must be between 0 and 999,999,999!     ";
               ASK redCostBox TO DisplayValue(1.0);
               validData := FALSE;
            ELSE
               tempSystemRedCost := redCostBox.Value;
            END IF;
            {Simulation}
            IF stopRadBox.SelectedButton.Id = 2011
               tempSimLength := simTimeVal.Value();
               startStat := startTimeVal.Value();
               IF (simTimeVal.Value() > 999999999.999999) OR (simTimeVal.Value() < 0.000001)
                  INC(errors);
                  textPool[errors] := "Stop simulation time must be between 0.000001 and 999,999,999.999999!     ";
                  ASK simTimeVal TO DisplayValue(dTimeTrunc);
                  validData := FALSE;
               END IF;
               IF (startStat > 999999999.999998) OR (startStat < 0.0)
                  INC(errors);
                  textPool[errors] := "Start statistics time must be between 0 and 999,999,999.999998!     ";
                  ASK startTimeVal TO DisplayValue(dTimeStartTime);
                  validData := FALSE;
               END IF;
               IF tempSimLength <= startStat
                  INC(errors);
                  textPool[errors] := "Start statistics time must be less than stop simulation time!     ";
                  ASK startTimeVal TO DisplayValue(dTimeStartTime);
                  validData := FALSE;
               END IF;
               IF (startStat<>dTimeStartTime) AND validData
                  somethingChanged := TRUE;
                  tempdTimeStartTime := startStat;
               END IF;
               IF (simLength<>dTimeTrunc) AND validData
                  somethingChanged := TRUE;
                  tempdTimeTrunc := tempSimLength;
               END IF;
               tempTermType := 1;
            ELSIF stopRadBox.SelectedButton.Id = 2012
               tempSimLength := simFailVal.Value();
               startStat := startTimeVal.Value();
               IF (tempSimLength < 1.) OR (tempSimLength > 999999999.)
                  INC(errors);
                  textPool[errors] := "Stop simulation failure must be between 1 and 999,999,999!     ";
                  ASK simFailVal TO DisplayValue(dFailTrunc);
                  validData := FALSE;
               END IF;
               IF (startStat < 0.) OR (startStat > 999999998.)
                  INC(errors);
                  textPool[errors] := "Start statisics failure must be between 0 and 999,999,998!     ";
                  ASK startTimeVal TO DisplayValue(dFailStartTime);
                  validData := FALSE;
               END IF;
               IF tempSimLength <= startStat
                  INC(errors);
                  textPool[errors] := "Start statistics failure must be less than stop simulation failure!     ";
                  ASK startTimeVal TO DisplayValue(dFailStartTime);
                  validData := FALSE;
               END IF;
               IF (simLength<>dFailTrunc) AND validData
                  somethingChanged := TRUE;
                  tempdFailTrunc := tempSimLength;
               END IF;
               IF (startStat<>dFailStartTime) AND validData
                  somethingChanged := TRUE;
                  tempdFailStartTime := FLOAT(ROUND(startStat));
               END IF;
               tempTermType := 2;
            ELSIF stopRadBox.SelectedButton.Id = 2013
               tempSimLength := simCycleVal.Value();
               startStat := startTimeVal.Value();
               IF (tempSimLength < 1.) OR (tempSimLength > 999999999.)
                  INC(errors);
                  textPool[errors] := "Stop simulation cycle must be between 1 and 999,999,999!     ";
                  ASK simCycleVal TO DisplayValue(dCycleTrunc);
                  validData := FALSE;
               END IF;
               IF (startStat < 0.) OR (startStat > 999999998.)
                  INC(errors);
                  textPool[errors] := "Start statisics cycle must be between 0 and 999,999,998!     ";
                  ASK startTimeVal TO DisplayValue(dCycleStartTime);
                  validData := FALSE;
               END IF;
               IF tempSimLength <= startStat
                  INC(errors);
                  textPool[errors] := "Start statistics cycle must be less than stop simulation cycle!     ";
                  ASK startTimeVal TO DisplayValue(dCycleStartTime);
                  validData := FALSE;
               END IF;
               IF (simLength<>dCycleTrunc) AND validData
                  somethingChanged := TRUE;
                  tempdCycleTrunc := tempSimLength;
               END IF;
               IF (startStat<>dCycleStartTime) AND validData
                  somethingChanged := TRUE;
                  tempdCycleStartTime := FLOAT(ROUND(startStat));
               END IF;
               tempTermType := 3;
            END IF;        
            IF (simRunsVal.Value() < 1.) OR (simRunsVal.Value() > 999999999.)
               INC(errors);
               textPool[errors] := "Number of runs must be between 1 and 999,999,999!     ";
               ASK simRunsVal TO DisplayValue(dNumberOfRuns);
               validData := FALSE;
            ELSE
               IF (TRUNC(simRunsVal.Value())<>TRUNC(dNumberOfRuns))
                  somethingChanged := TRUE;
                  tempdNumberOfRuns := simRunsVal.Value();
               END IF;
            END IF;   
            IF dSimWithGraph <> graphicsChkBox.Checked
               tempdSimWithGraph := graphicsChkBox.Checked;
               somethingChanged := TRUE;
            END IF;
   
            {System Streams}
            tempStrA := STRTOINT(streamA.Text());
            tempStrB := STRTOINT(streamB.Text());
            tempStrC := STRTOINT(streamC.Text());
            tempStrD := STRTOINT(streamD.Text());
            tempStrE := STRTOINT(streamE.Text());
            tempStrF := STRTOINT(streamF.Text());
            tempStrG := STRTOINT(streamG.Text());
            tempStrH := STRTOINT(streamH.Text());
            tempStrI := STRTOINT(streamI.Text());
            tempStrJ := STRTOINT(streamJ.Text());
            tempStrAnc := STRTOINT(ancStream.Text());
    
            IF antiStreamA.Checked 
              tempStrA := -1*tempStrA;
            END IF;
            IF antiStreamB.Checked
              tempStrB := -1*tempStrB;
            END IF;
            IF antiStreamC.Checked
              tempStrC := -1*tempStrC;
            END IF;
            IF antiStreamD.Checked
              tempStrD := -1*tempStrD;
            END IF;
            IF antiStreamE.Checked
              tempStrE := -1*tempStrE;
            END IF;
            IF antiStreamF.Checked
              tempStrF := -1*tempStrF;
            END IF;
            IF antiStreamG.Checked
              tempStrG := -1*tempStrG;
            END IF;
            IF antiStreamH.Checked
              tempStrH := -1*tempStrH;
            END IF;
            IF antiStreamI.Checked
              tempStrI := -1*tempStrI;
            END IF;
            IF antiStreamJ.Checked
              tempStrJ := -1*tempStrJ;
            END IF;
            IF antiAncStream.Checked
              tempStrAnc := -1*tempStrAnc;
            END IF;
            total := ABS(tempStrA*tempStrB*tempStrC*tempStrD);
            {IF (total <> 5040)
               NEW(message, 1..2);
               message[1] := "Two or more system streams share the same seed or that     ";
               message[2] := "seeds antithetic.  Are you sure you want to continue?     ";
               result := SendAlert(message, TRUE, FALSE, FALSE);
               IF NOT result
                  validData := FALSE;
               END IF;
               DISPOSE(message);
            END IF;}        
            {Weak Link Analysis}
            tempWeakLinkAnalType := weakLinkRad.SelectedButton.Id - 4010;
            IF (gyThreshVal.Value() <> GYthreshold) OR (YRthreshold <> yrThreshVal.Value())
               IF (yrThreshVal.Value() < 0.) OR ((yrThreshVal.Value() >= gyThreshVal.Value()) AND (gyThreshVal.Value() > 0.01)) 
                  INC(errors);
                  textPool[errors] := "The YR threshold value value must be between 0 and the GY threshold value!     ";
                  ASK yrThreshVal TO SetValue(gyThreshVal.Value()/2.);
                  ASK yrThreshVal TO Draw;
                  IF STRLEN(REALTOSTR(gyThreshVal.Value()/2.)) > 6
                     ASK redText TO SetLabel("0.0         <=  Red      <  "+ChopZeros(yrThreshVal.Value,6));
                  ELSE
                     ASK redText TO SetLabel("0.0         <=  Red      <  "+REALTOSTR(yrThreshVal.Value));
                  END IF;
                  ASK redText TO Draw;
                  validData := FALSE;
               END IF;
               IF (gyThreshVal.Value() > 1.00) OR (gyThreshVal.Value() < 0.10)
                  INC(errors);
                  textPool[errors] := "The GY threshold value value must be between 0.01 and  1.0!     ";
                  ASK gyThreshVal TO SetValue(0.95);
                  ASK yellowText TO SetLabel("<=  Yellow  <  0.950000");
                  ASK gyThreshVal TO Draw;
                  ASK yellowText TO Draw;
                  validData := FALSE;
               END IF;
               IF validData
                  tempGYthreshold := gyThreshVal.Value();
                  tempYRthreshold := yrThreshVal.Value();
               END IF;
            END IF;
         ELSE
            validData := TRUE;
            cancelled := TRUE;
         END IF;
         IF errors > 1
            NEW(message, 1..errors+2);
            message[1] := "The following errors must be corrected:   ";
            message[2] := "";
            FOR i := 1 TO errors
               message[i+2] := "   "+INTTOSTR(i)+". "+textPool[i];
            END FOR;
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         ELSIF errors = 1
            NEW(message, 1..1);
            message[1] := textPool[1];
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
      UNTIL validData;
      IF (NOT cancelled)
         {General}
         systemUnits   := tempSystemUnits;
         systemImage   := tempSystemImage;
         flowGenerated := tempFlowGenerated;
         sysLostCost   := tempSysLostCost;
         systemRedCost := tempSystemRedCost;
         negShutUp     := negValChkBox.Checked();
         sysComment    := commentBox.Text();
         
         {Simulation}
         simLength       := tempSimLength;
         dTimeStartTime  := tempdTimeStartTime;
         dTimeTrunc      := tempdTimeTrunc;
         dCycleStartTime := tempdCycleStartTime;
         dCycleTrunc     := tempdCycleTrunc;
         dFailTrunc      := tempdFailTrunc;
         dFailStartTime  := tempdFailStartTime;
         termType        := tempTermType;
         dNumberOfRuns   := tempdNumberOfRuns;
         dSimWithGraph   := tempdSimWithGraph;
            
         {System Streams}
         sysStreams[1] := tempStrA;
         sysStreams[2] := tempStrB;
         sysStreams[3] := tempStrC;
         sysStreams[4] := tempStrD;
         sysStreams[5] := tempStrE;
         sysStreams[6] := tempStrF;
         sysStreams[7] := tempStrG;
         sysStreams[8] := tempStrH;
         sysStreams[9] := tempStrI;
         sysStreams[10] := tempStrJ;
         sysStreams[11] := tempStrAnc;
         
         {Weak Link Analysis}
         weakLinkAnalType := tempWeakLinkAnalType;
         GYthreshold := tempGYthreshold;
         YRthreshold := tempYRthreshold;
         IF NodeArray <> NILARRAY
            UpdateNodeArray;
         END IF;
         IF BlockArray <> NILARRAY
            UpdateBlockArray;
         END IF;
         IF EventArray <> NILARRAY
            UpdateEventArray;
         END IF;
      END IF;
      DISPOSE(textPool);
   END METHOD; {ReceiveData}

   ASK METHOD GetColors(OUT cancelled : BOOLEAN);
   VAR
      validData  : BOOLEAN;
   BEGIN
      genTab      := Child("GenTab", 100);
      simTab      := Child("SimTab", 200);
      stopRadBox   := Descendant("StopRadBox", 201);
      simFailVal   := Descendant("SimFailVal", 203);
      startTimeVal := Descendant("StartTimeVal", 206);
      streamsTab  := Child("StreamsTab", 300);
      weakLinkRad := Descendant("WeakLinkAnalType", 401);
      availButt   := ASK weakLinkRad Child("AvailButt", 4011);
      dependButt  := ASK weakLinkRad Child("DependButt", 4012);
      relyButt    := ASK weakLinkRad Child("RelyButt", 4013);
      gyThreshVal := Descendant("GYThreshVal", 402);
      yrThreshVal := Descendant("YRThreshVal", 403);
      yellowText  := Descendant("YellowText", 404);
      redText     := Descendant("RedText", 405);
{Hide Four Tabs}
      ASK genTab TO SetHidden(TRUE);
      ASK simTab TO SetHidden(TRUE);
      ASK streamsTab TO SetHidden(TRUE);
{Weak Link Analysis Tab______________________________}
      ASK gyThreshVal TO SetValue(GYthreshold);
      ASK yrThreshVal TO SetValue(YRthreshold);
      ASK yellowText TO SetLabel("<=  Yellow  <  "+REALTOSTR(GYthreshold));
      ASK redText TO SetLabel("0.0         <=  Red      <  "+REALTOSTR(YRthreshold));
      IF weakLinkAnalType = 1
         ASK weakLinkRad TO SetSelectedButton(availButt);
      ELSIF weakLinkAnalType = 2
         ASK weakLinkRad TO SetSelectedButton(dependButt);
      ELSE
         ASK weakLinkRad TO SetSelectedButton(relyButt);
      END IF;
      Draw;
      REPEAT
         cancelled := FALSE;
         validData := TRUE;
         button := AcceptInput();        
         IF button.ReferenceName = "OKButton"
            weakLinkAnalType := weakLinkRad.SelectedButton.Id - 4010;
            IF (gyThreshVal.Value() <> GYthreshold) OR (YRthreshold <> yrThreshVal.Value())
               IF (yrThreshVal.Value() < 0.) OR ((yrThreshVal.Value() >= gyThreshVal.Value()) AND (gyThreshVal.Value() > 0.01)) 
                  NEW(message, 1..1);
                  message[1] := "The YR threshold value value must be between 0 and the GY threshold value!     ";
                  result := SendAlert(message, TRUE, FALSE, TRUE);
                  DISPOSE(message);
                  ASK yrThreshVal TO SetValue(gyThreshVal.Value()/2.);
                  ASK yrThreshVal TO Draw;
                  IF STRLEN(REALTOSTR(gyThreshVal.Value()/2.)) > 6
                     ASK redText TO SetLabel("0.0         <=  Red      <  "+ChopZeros(yrThreshVal.Value,6));
                  ELSE
                     ASK redText TO SetLabel("0.0         <=  Red      <  "+REALTOSTR(yrThreshVal.Value));
                  END IF;
                  ASK redText TO Draw;
                  validData := FALSE;
               END IF;
               IF (gyThreshVal.Value() > 1.00) OR (gyThreshVal.Value() < 0.10)
                  NEW(message, 1..1);
                  message[1] := "The GY threshold value value must be less or equal to than 1!     ";
                  result := SendAlert(message, TRUE, FALSE, TRUE);
                  DISPOSE(message);
                  ASK gyThreshVal TO SetValue(0.95);
                  ASK yellowText TO SetLabel("<=  Yellow  <  0.950000");
                  ASK gyThreshVal TO Draw;
                  ASK yellowText TO Draw;
                  validData := FALSE;
               END IF;
               IF validData
                  GYthreshold := gyThreshVal.Value();
                  YRthreshold := yrThreshVal.Value();
               END IF;
            END IF;
            IF NodeArray <> NILARRAY
               UpdateNodeArray;
            END IF;
            IF BlockArray <> NILARRAY
               UpdateBlockArray;
            END IF;
            IF EventArray <> NILARRAY
               UpdateEventArray;
            END IF;
         ELSE
            cancelled := TRUE;
         END IF;
      UNTIL validData;
   END METHOD; {GetColors}

   ASK METHOD BeSelected;
   VAR
      tabClicked : GraphicVObj;
      tempString : STRING;
      buttonId   : INTEGER;
   BEGIN
      tabClicked := LastPicked;
      CASE tabClicked.Id
         WHEN 200: {Simulation Tab}
            lastClicked := ASK tabClicked LastPicked;
            CASE lastClicked.Id
               WHEN 201: {Stop simulation RadioBox}
                  buttonId := stopRadBox.SelectedButton.Id;
                  IF buttonId = 2011
                     ASK startLabel2 TO SetLabel(systemUnits);
                     ASK stopLabel1 TO Activate;
                     ASK stopLabel2 TO Deactivate;
                     ASK stopLabel3 TO Deactivate;
                     ASK simTimeVal TO Activate;
                     ASK simFailVal TO Deactivate;
                     ASK simCycleVal TO Deactivate;
                  ELSIF buttonId = 2012
                     ASK startLabel2 TO SetLabel("failures");
                     ASK stopLabel1 TO Deactivate;
                     ASK stopLabel2 TO Activate;
                     ASK stopLabel3 TO Deactivate;
                     ASK simTimeVal TO Deactivate;
                     ASK simFailVal TO Activate;
                     ASK simCycleVal TO Deactivate;
                  ELSIF buttonId = 2013
                     ASK startLabel2 TO SetLabel("cycles");
                     ASK stopLabel1 TO Deactivate;
                     ASK stopLabel2 TO Deactivate;
                     ASK stopLabel3 TO Activate;
                     ASK simTimeVal TO Deactivate;
                     ASK simFailVal TO Deactivate;
                     ASK simCycleVal TO Activate;
                  END IF;
            OTHERWISE;
            END CASE;
         OTHERWISE;
      END CASE;
      IF SELF.Visible
         IF (stopRadBox.SelectedButton.Id = 2012) AND (simFailVal.Value() = 1.)
            ASK startTimeVal TO Deactivate;
         ELSE
            ASK startTimeVal TO Activate;
         END IF;
         ASK startTimeVal TO Draw;
      END IF;

      tempString := REALTOSTR(gyThreshVal.Value) + "000000";
      ASK yellowText TO SetLabel("<=  Yellow  <  " + SUBSTR(1,8,tempString));
     
      tempString := REALTOSTR(yrThreshVal.Value) + "000000";
      ASK redText TO SetLabel("0.0         <=  Red      <  " + SUBSTR(1,8,tempString));
      Draw;
      
      INHERITED BeSelected;
   END METHOD; {BeSelected}

END OBJECT; {SystemBoxObj}

OBJECT PrintTablesBoxObj;
   ASK METHOD GetPreferences(IN savingTXT : BOOLEAN);
   VAR
      blockPhased,nodePhased,eventPhased,hierPhased,
      blockInLoaded, nodeInLoaded, eventInLoaded,
      hierInLoaded, systemInLoaded, eventsExist,
      phasesLoaded,coldExists,
      tablesOutLoaded, capExists, blockPM   : BOOLEAN;
      tempPhase                             : PhaseBoxObj;
      blockInputBox                         : BlockInputBoxObj;
      nodeInputBox                          : NodeInputBoxObj;
      eventInputBox                         : EventInputBoxObj;
      hierInputBox                          : HierInputBoxObj;
      systemInputBox                        : SystemInputBoxObj;
      tempOut                               : TablesOutObj;
      dumbTime                              : timeType;
      i{, pagesAcross}                        : INTEGER;     {UnGreen}
      block                                 : RBDBlockObj;
      event                                 : RBDEventObj;
      node                                  : RBDNodeObj;
      tempLink                              : LinkObj;
      dir                                   : STRING;
      tempBlock                             : RBDBlockObj;
      tempNode                              : RBDNodeObj;
      hier                                  : RBDHierObj;
   BEGIN
      savingText := savingTXT;
      printPhase := TRUE;
      blockInLoaded  := FALSE;
      nodeInLoaded   := FALSE;
      eventInLoaded  := FALSE;
      hierInLoaded   := FALSE;
      systemInLoaded := FALSE;
      tablesOutLoaded  := FALSE;
      blockFailBox  := Descendant("BlockFailBox", 101);
      blockRepBox   := Descendant("BlockRepBox", 102);
      blockPMBox    := Descendant("BlockPMBox", 103);
      blockMaintBox := Descendant("BlockMaintBox", 104);
      blockCostBox  := Descendant("BlockCostBox", 105);
      blockDepBox   := Descendant("BlockDepBox", 106);
      blockAdvBox   := Descendant("BlockAdvBox", 107);
      nodeGenBox    := Descendant("NodeGenBox", 108);
      nodeStandbyBox:= Descendant("NodeStandbyBox", 109);
      nodeCapBox    := Descendant("NodeCapBox", 110);
      eventsBox     := Descendant("EventsBox", 111);
      hierBox       := Descendant("HierBox", 112);
      sysBox        := Descendant("SysBox", 113);
      spareBox      := Descendant("SpareBox", 114);
      resBox        := Descendant("ResBox", 115);
      trigBox       := Descendant("TrigBox", 116);
      detailsPhaseBox := Descendant("DetailsPhaseBox", 130);
      blockPhaseBox := Descendant("BlockPhaseBox", 117);
      nodePhaseBox  := Descendant("NodePhaseBox", 118);
      eventPhaseBox := Descendant("EventPhaseBox", 119);
      hierPhaseBox  := Descendant("HierPhaseBox", 120);
      summaryBox    := Descendant("SummaryBox", 121);
      logBox        := Descendant("LogBox", 122);
      spareOutBox   := Descendant("SpareOutBox", 123);
      blockCostOut  := Descendant("BlockCostOutBox", 124);
      blockAnalBox  := Descendant("BlockAnalBox", 125);
      nodeAnalBox   := Descendant("NodeAnalBox", 126);
      eventAnalBox  := Descendant("EventAnalBox", 127);
      hierAnalBox   := Descendant("HierAnalBox", 128);
      capOutBox     := Descendant("CapOutBox", 129);
      NEW(tempPhase);
      NEW(blockInputBox);
      NEW(nodeInputBox);
      NEW(eventInputBox);
      NEW(hierInputBox);
      NEW(systemInputBox);
      NEW(tempOut);
      ASK blockInputBox  TO LoadFromLibrary(dialogs, "BlockInputBox");
      ASK nodeInputBox   TO LoadFromLibrary(dialogs, "NodeInputBox");
      ASK eventInputBox  TO LoadFromLibrary(dialogs, "EventInputBox");
      ASK hierInputBox   TO LoadFromLibrary(dialogs, "HierInputBox");
      ASK systemInputBox TO LoadFromLibrary(dialogs, "SystemInputBox");
      ASK tempOut TO LoadFromLibrary(dialogs, "ViewOutputBox");
      ASK tempPhase TO LoadFromLibrary(dialogs, "PhaseBox");
      IF activePhases = 0
         ASK detailsPhaseBox TO Deactivate;
         ASK blockPhaseBox TO Deactivate;
         ASK nodePhaseBox TO Deactivate;
         ASK eventPhaseBox TO Deactivate;
         ASK hierPhaseBox TO Deactivate;
      END IF;
      IF NOT simulated
         ASK summaryBox   TO Deactivate;
         ASK logBox       TO Deactivate;
         ASK spareOutBox  TO Deactivate;
         ASK blockCostOut TO Deactivate;
         ASK blockAnalBox TO Deactivate;
         ASK nodeAnalBox  TO Deactivate;
         ASK eventAnalBox TO Deactivate;
         ASK hierAnalBox  TO Deactivate;
         ASK capOutBox    TO Deactivate;
      END IF;
      IF NOT weakAnalysis
         ASK blockAnalBox TO Deactivate;
         ASK nodeAnalBox  TO Deactivate;
         ASK eventAnalBox TO Deactivate;
         ASK hierAnalBox  TO Deactivate;
      END IF;
      IF CapacityArray = NILARRAY
         ASK capOutBox TO Deactivate;
      END IF;
      IF BlockCostArray = NILARRAY
         ASK blockCostOut TO Deactivate;
      END IF;
      IF NodeArray = NILARRAY
         ASK nodeAnalBox TO Deactivate;
      END IF;
      IF totalBlocks = 0
         ASK blockFailBox TO Deactivate;
         ASK blockRepBox TO Deactivate;
         ASK blockPMBox TO Deactivate;
         ASK blockMaintBox TO Deactivate;
         ASK blockCostBox TO Deactivate;
         ASK blockDepBox TO Deactivate;
         ASK blockAdvBox TO Deactivate;
         ASK blockCostOut TO Deactivate;
         ASK blockAnalBox TO Deactivate;
         ASK blockPhaseBox TO Deactivate;
      ELSE
         FOREACH block IN blockGroup
            IF block.usesPhasing
               blockPhased := TRUE;
            END IF;
            IF block.usesPM
               blockPM := TRUE;
            END IF;
         END FOREACH;
      END IF;  
      IF NOT blockPM
         ASK blockPMBox TO Deactivate;
      END IF;
      IF totalEvents = 0
         ASK eventsBox TO Deactivate;
         ASK eventPhaseBox TO Deactivate;
      ELSE
         eventsExist := TRUE;
         FOREACH event IN eventGroup
            IF event.usesPhasing
               eventPhased := TRUE;
            END IF;
         END FOREACH;
      END IF;         
      IF (totalNodes <= (2 + totalHiers*2)) {only start, end, in, out markers there}
         ASK nodeGenBox TO Deactivate;
         ASK nodeAnalBox TO Deactivate;
         ASK nodePhaseBox TO Deactivate;
         ASK nodeCapBox TO Deactivate;
         ASK nodeStandbyBox TO Deactivate;
      ELSE
         FOREACH node IN nodeGroup
            IF node.usesPhasing
               nodePhased := TRUE;
            END IF;
            IF node.coldStandby
               coldExists := TRUE;
            END IF;
         END FOREACH;
      END IF;
      IF totalHiers = 0
         ASK hierBox TO Deactivate;
         ASK hierAnalBox TO Deactivate;
         ASK hierPhaseBox TO Deactivate;
      ELSE
         FOREACH hier IN hierGroup
            IF hier.usesPhasing
               hierPhased := TRUE;
            END IF;
         END FOREACH;
      END IF;
      IF totalLinks > 0
         FOREACH tempLink IN linkGroup
            IF (tempLink.connectFRef = "RBDNode") OR (tempLink.connectTRef = "RBDNode")
               capExists := TRUE;
               EXIT;
            END IF;
         END FOREACH;
      END IF;
      IF NOT capExists
         ASK nodeCapBox TO Deactivate;
      END IF;
      IF NOT coldExists
         ASK nodeStandbyBox TO Deactivate;
      END IF;
      IF NOT eventsExist
         ASK eventsBox TO Deactivate;
      END IF;
      IF NOT blockPhased
         ASK blockPhaseBox TO Deactivate;
      END IF;
      IF NOT nodePhased
         ASK nodePhaseBox TO Deactivate;
      END IF;
      IF NOT eventPhased
         ASK eventPhaseBox TO Deactivate;
      END IF;
      IF NOT hierPhased
         ASK hierPhaseBox TO Deactivate;
      END IF;
      IF totalSpares = 0
         ASK spareBox TO Deactivate;
      END IF;
      IF totalRes = 0
         ASK resBox TO Deactivate;
      END IF;
      IF totalTriggers = 0
         ASK trigBox TO Deactivate;
      END IF;
      Draw;
      button := AcceptInput();        
      IF button.ReferenceName = "OKButton"
         IF blockFailBox.Checked
            IF NOT blockInLoaded 
               ASK blockInputBox TO InitSelf(TRUE);
            END IF;    
            blockInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(blockInputBox.distroTable, "Distro", "Save");
            ELSE
               PrintInputTable(blockInputBox.distroTable, "Distro", "Print");
            END IF;
         END IF;
         IF blockRepBox.Checked
            IF NOT blockInLoaded 
               ASK blockInputBox TO InitSelf(TRUE);
            END IF;    
            blockInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(blockInputBox.sparesTable, "Sparing", "Save");
            ELSE
               PrintInputTable(blockInputBox.sparesTable, "Sparing", "Print");
            END IF;
         END IF;
         IF blockPMBox.Checked
            IF NOT blockInLoaded 
               ASK blockInputBox TO InitSelf(TRUE);
            END IF;    
            blockInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(blockInputBox.pmTable, "PM", "Save");
            ELSE 
               PrintInputTable(blockInputBox.pmTable, "PM", "Print");
            END IF;  
         END IF;
         IF blockMaintBox.Checked
            IF NOT blockInLoaded 
               ASK blockInputBox TO InitSelf(TRUE);
            END IF;    
            blockInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(blockInputBox.maintTable, "MaintDelays", "Save");
            ELSE
               PrintInputTable(blockInputBox.maintTable, "MaintDelays", "Print");
            END IF;  
         END IF;
         IF blockCostBox.Checked
            IF NOT blockInLoaded 
               ASK blockInputBox TO InitSelf(TRUE);
            END IF;    
            blockInLoaded := TRUE;
            IF savingTXT
               SaveCostInput(blockInputBox.costTable);
            ELSE
               PrintCostInput(blockInputBox.costTable);
            END IF;
         END IF;
         IF blockDepBox.Checked
            IF NOT blockInLoaded 
               ASK blockInputBox TO InitSelf(TRUE);
            END IF;    
            blockInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(blockInputBox.depTable, "Dependency", "Save");
            ELSE   
               PrintInputTable(blockInputBox.depTable, "Dependency", "Print");
            END IF;
         END IF;
         IF blockAdvBox.Checked
            IF NOT blockInLoaded 
               ASK blockInputBox TO InitSelf(TRUE);
            END IF; 
            blockInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(blockInputBox.advTable, "Advanced", "Save");
            ELSE
               PrintInputTable(blockInputBox.advTable, "Advanced", "Print");
            END IF;
         END IF;  
         IF nodeGenBox.Checked
            IF NOT nodeInLoaded 
               ASK nodeInputBox TO InitSelf(TRUE);
            END IF;    
            nodeInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(nodeInputBox.genTable, "NodeGeneral", "Save");
            ELSE
               PrintInputTable(nodeInputBox.genTable, "NodeGeneral", "Print");
            END IF;
         END IF;
         IF nodeStandbyBox.Checked
            IF NOT nodeInLoaded 
               ASK nodeInputBox TO InitSelf(TRUE);
            END IF; 
            nodeInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(nodeInputBox.standbyTable, "NodeStandby", "Save");
            ELSE
               PrintInputTable(nodeInputBox.standbyTable, "NodeStandby", "Print");
            END IF;
         END IF;  
         IF nodeCapBox.Checked
            IF NOT nodeInLoaded 
               ASK nodeInputBox TO InitSelf(TRUE);
            END IF; 
            nodeInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(nodeInputBox.capInTable, "NodeCapacity", "Save");
            ELSE
               PrintInputTable(nodeInputBox.capInTable, "NodeCapacity", "Print");
            END IF;
         END IF;  
         IF eventsBox.Checked
            IF NOT eventInLoaded 
               ASK eventInputBox TO InitSelf(TRUE);
            END IF;    
            eventInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(eventInputBox.genTable, "Event", "Save");
            ELSE
               PrintInputTable(eventInputBox.genTable, "Event", "Print");
            END IF; 
         END IF;
         IF hierBox.Checked
            IF NOT hierInLoaded 
               ASK hierInputBox TO InitSelf;
            END IF;    
            hierInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(hierInputBox.genTable, "Hierarchy", "Save");
            ELSE
               PrintInputTable(hierInputBox.genTable, "Hierarchy", "Print");
            END IF;
         END IF;
         IF sysBox.Checked
            IF NOT systemInLoaded 
               ASK systemInputBox TO InitSelf(TRUE);
            END IF; 
            systemInLoaded := TRUE;
            IF savingTXT
               SaveSystemInput(systemInputBox.sysArray);
            ELSE
               PrintSystemInput(systemInputBox.sysArray);
            END IF;
         END IF;
         IF spareBox.Checked
            IF NOT systemInLoaded 
               ASK systemInputBox TO InitSelf(TRUE);
            END IF; 
            systemInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(systemInputBox.spareTable, "SparePools", "Save");
            ELSE
               PrintInputTable(systemInputBox.spareTable, "SparePools", "Print");
            END IF;
         END IF;
         IF resBox.Checked
            IF NOT systemInLoaded 
               ASK systemInputBox TO InitSelf(TRUE);
            END IF; 
            systemInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(systemInputBox.resTable, "Resources", "Save");
            ELSE
               PrintInputTable(systemInputBox.resTable, "Resources", "Print");
            END IF;
         END IF;
         IF trigBox.Checked
            IF NOT systemInLoaded 
               ASK systemInputBox TO InitSelf(TRUE);
            END IF; 
            systemInLoaded := TRUE;
            IF savingTXT
               PrintInputTable(systemInputBox.trigTable, "Triggers", "Save");
            ELSE
               PrintInputTable(systemInputBox.trigTable, "Triggers", "Print");
            END IF;
         END IF;
         IF summaryBox.Checked
            IF NOT tablesOutLoaded 
               ASK tempOut TO InitSelf(TRUE);
            END IF; 
            tablesOutLoaded := TRUE;
            IF savingTXT
               SaveSummaryOutput(tempOut.resultsTable);
            ELSE
               PrintSummaryOutput(tempOut.resultsTable);
            END IF;
         END IF;
         IF logBox.Checked
            IF NOT tablesOutLoaded 
               ASK tempOut TO InitSelf(TRUE);
            END IF; 
            tablesOutLoaded := TRUE;
            IF savingTXT
               SaveLogisticsOutput(tempOut.logisticsTable);
            ELSE
               PrintLogisticsOutput(tempOut.logisticsTable);
            END IF;
         END IF;
         IF spareOutBox.Checked
            IF NOT tablesOutLoaded 
               ASK tempOut TO InitSelf(TRUE);
            END IF; 
            ASK tempOut.sparesTable TO SetText("Component           ", 1, 0);
            ASK tempOut.sparesTable TO SetText("Spare Source         ", 2, 0);
            tablesOutLoaded := TRUE;
            IF savingTXT
               SaveSparingOutput(tempOut.sparesTable);
            ELSE
               PrintSparingOutput(tempOut.sparesTable);
            END IF;
         END IF;
         IF blockCostOut.Checked
            IF NOT tablesOutLoaded 
               ASK tempOut TO InitSelf(TRUE);
            END IF;    
            tablesOutLoaded := TRUE;
            IF savingTXT
               SaveCostOutput(tempOut.costTable);
            ELSE
               PrintCostOutput(tempOut.costTable);
            END IF;
         END IF;
         IF capOutBox.Checked
            IF NOT tablesOutLoaded 
               ASK tempOut TO InitSelf(TRUE);
            END IF; 
            tablesOutLoaded := TRUE;
            IF savingTXT
               SaveCapacityOutput(tempOut.capOutTable);
            ELSE
               PrintCapacityOutput(tempOut.capOutTable);
            END IF;
         END IF;  
         IF nodeAnalBox.Checked
            IF NOT tablesOutLoaded 
               ASK tempOut TO InitSelf(TRUE);
            END IF; 
            tablesOutLoaded := TRUE;
            IF savingTXT
               SaveAnalOutput(tempOut.nodeAnalTable,"Node");
            ELSE
               PrintAnalOutput(tempOut.nodeAnalTable, "Node");
            END IF;
         END IF; 
         IF blockAnalBox.Checked
            IF NOT tablesOutLoaded 
               ASK tempOut TO InitSelf(TRUE);
            END IF; 
            tablesOutLoaded := TRUE;
            IF savingTXT
               SaveAnalOutput(tempOut.blockAnalTable,"Block");
            ELSE
               PrintAnalOutput(tempOut.blockAnalTable, "Block");
            END IF;
         END IF; 
         IF eventAnalBox.Checked
            IF NOT tablesOutLoaded 
               ASK tempOut TO InitSelf(TRUE);
            END IF; 
            tablesOutLoaded := TRUE;
            IF savingTXT
               SaveAnalOutput(tempOut.eventAnalTable,"Event");
            ELSE
               PrintAnalOutput(tempOut.eventAnalTable, "Event");
            END IF;
         END IF; 
         IF hierAnalBox.Checked
            IF NOT tablesOutLoaded 
               ASK tempOut TO InitSelf(TRUE);
            END IF; 
            tablesOutLoaded := TRUE;
            IF savingTXT
               SaveAnalOutput(tempOut.hierAnalTable,"Hier");
            ELSE
               PrintAnalOutput(tempOut.hierAnalTable, "Hier");
            END IF;
         END IF; 
         IF detailsPhaseBox.Checked 
            IF NOT phasesLoaded 
               ASK tempPhase TO InitSelf;
            END IF;    
            IF savingTXT
               SavePhases(tempPhase.dTable);  
            ELSE
               PrintPhases(tempPhase.dTable);
            END IF;
            phasesLoaded := TRUE;
         END IF;
         IF blockPhaseBox.Checked
            IF NOT phasesLoaded 
               ASK tempPhase TO InitSelf;
            END IF;    
            IF savingTXT
               SaveBlockPhases(tempPhase.bTable);  
            ELSE
               PrintBlockPhases(tempPhase.bTable);
            END IF;
            phasesLoaded := TRUE;
         END IF;
         IF nodePhaseBox.Checked
            IF NOT phasesLoaded 
               ASK tempPhase TO InitSelf;
            END IF;    
            phasesLoaded := TRUE;
            IF savingTXT
               SaveNodePhases(tempPhase.nTable);
            ELSE
               PrintNodePhases(tempPhase.nTable);
            END IF;
         END IF;
         IF eventPhaseBox.Checked
            IF NOT phasesLoaded 
               ASK tempPhase TO InitSelf;
            END IF; 
            phasesLoaded := TRUE;
            IF savingTXT
               SaveEventPhases(tempPhase.eTable);
            ELSE
               PrintEventPhases(tempPhase.eTable);
            END IF;
         END IF;
         IF hierPhaseBox.Checked
            IF NOT phasesLoaded 
               ASK tempPhase TO InitSelf;
            END IF; 
            phasesLoaded := TRUE;
            IF savingTXT
               SaveHierPhases(tempPhase.hTable);
            ELSE
               PrintHierPhases(tempPhase.hTable); 
            END IF;
         END IF;
      END IF;
      printPhase := FALSE;
   END METHOD; {GetPreferences}
  
   ASK METHOD BeSelected;
   BEGIN
      lastClicked := ASK SELF LastPicked;
      CASE ASK lastClicked Id
         WHEN 965: {HELP Button}
            IF savingText
               CallHelp(52);
            ELSE
               CallHelp(43);
            END IF;
         OTHERWISE
      END CASE;
      INHERITED BeSelected;
   END METHOD; {BeSelected}
END OBJECT; {PrintTablesBoxObj}

OBJECT SimBoxObj;
   ASK METHOD BeSelected;
   VAR
      buttonId                                                 : INTEGER;
      value1, simTimeVal, simFailVal, startTimeVal, 
      simCycleVal, sampleBox, freqVal                          : ValueBoxObj;
      nodeAnal, checkBox, availGraphBox,
      availPlotBox, costRepChk, stepMode, detByCompChk         : CheckBoxObj;
      radioBox, graphOption                                    : RadioBoxObj;
      startT1, startT2, startF1, label1, label2, graphOptTitle, 
      startF2, stopLabel2, stopLabel1, stopLabel3, unitsLabel6 : LabelObj;
      tabClicked                                               : GraphicVObj;
      detEventComp                                             : ComboBoxObj;
   BEGIN
      tabClicked := LastPicked;
      generalTab := Child("GenTab",100);
      fileTab := Child("FileTab",200);
      advTab := Child("AdvTab",300);
      avtGrdTab := Child("AvtGrdTab", 400);
      checkBox := Descendant("GraphicsChkBox", 103);
      value1 := Descendant("TimeSliceVal", 311);
      simTimeVal := Descendant("SimTimeVal", 106);
      simFailVal := Descendant("SimFailVal", 107);
      startTimeVal := Descendant("StartTimeVal", 302);
      simCycleVal := Descendant("SimCycleVal", 111);
      stopLabel1  := Descendant("StopLabel1", 104);
      stopLabel2  := Descendant("StopLabel2", 105);
      stopLabel3  := Descendant("StopLabel3", 110);
      startT2 := Descendant("StartLabel2", 303);
      label1 := Descendant("TimeSliceLabel1", 310);
      label2 := Descendant("TimeSliceLabel2", 312);
      radioBox := Descendant("StopRadBox", 101);
      costRepChk   := Descendant("CostReportBox", 214);
      freqVal      := Descendant("FreqVal", 216);
      availPlotBox := Descendant("AvailPlotBox", 209); 
      availGraphBox := Descendant("AvailGraphBox", 306); 
      sampleBox := Descendant("SampleVal", 307);
      unitsLabel6 := Descendant("UnitsLabel6", 308);
      stepMode     := Descendant("StepModeBox", 309); 
      detByCompChk := Descendant("DetByCompChk", 402);
      detEventComp := Descendant("DetEventComp", 0);
      
      CASE tabClicked.Id
         WHEN 100:
            lastClicked := ASK tabClicked LastPicked;
            CASE ASK lastClicked Id}
               WHEN 103: {Graphics Check Box}
                  IF checkBox.Checked
                     ASK value1 TO Activate;
                     ASK label1 TO Activate;
                     ASK label2 TO Activate;
                     ASK stepMode TO Activate;
                  ELSE
                     ASK value1 TO Deactivate;
                     ASK label1 TO Deactivate;
                     ASK label2 TO Deactivate;
                     ASK stepMode TO Deactivate;
                  END IF;
                  Draw;
               WHEN 101: {Stop simulation RadioBox}
                  buttonId := radioBox.SelectedButton.Id;
                  IF buttonId = 1011
                     ASK startT2 TO SetLabel(systemUnits);
                     ASK simTimeVal    TO Activate;
                     ASK stopLabel1    TO Activate;
                     ASK simFailVal    TO Deactivate;
                     ASK stopLabel2    TO Deactivate;
                     ASK simCycleVal   TO Deactivate;
                     ASK stopLabel3    TO Deactivate;
                     ASK availPlotBox  TO Activate;
                     ASK availGraphBox TO Activate;
                     ASK sampleBox     TO Deactivate;
                     ASK unitsLabel6   TO Activate;
                     Draw;   
                  ELSIF buttonId = 1012
                     ASK startT2 TO SetLabel("failures");
                     ASK simTimeVal    TO Deactivate;
                     ASK stopLabel1    TO Deactivate;
                     ASK simFailVal    TO Activate;
                     ASK stopLabel2    TO Activate;
                     ASK simCycleVal   TO Deactivate;
                     ASK stopLabel3    TO Deactivate;
                     ASK availPlotBox  TO DisplayCheck(FALSE);
                     ASK availPlotBox  TO Deactivate;
                     ASK availGraphBox TO DisplayCheck(FALSE);
                     ASK availGraphBox TO Deactivate;
                     ASK sampleBox     TO Deactivate;
                     ASK unitsLabel6   TO Deactivate;
                     Draw;
                  ELSIF buttonId = 1013
                     ASK startT2 TO SetLabel("cycles");
                     ASK simTimeVal    TO Deactivate;
                     ASK stopLabel1    TO Deactivate;
                     ASK simFailVal    TO Deactivate;
                     ASK stopLabel2    TO Deactivate;
                     ASK simCycleVal   TO Activate;
                     ASK stopLabel3    TO Activate;
                     ASK availPlotBox  TO Deactivate;
                     ASK availGraphBox TO Deactivate;
                     ASK sampleBox     TO Deactivate;
                     ASK unitsLabel6   TO Deactivate;
                     Draw;
                  END IF; 
               OTHERWISE
            END CASE;
         WHEN 200:
            lastClicked := ASK tabClicked LastPicked;
            CASE ASK lastClicked Id 
               WHEN 214: {Check Box}
                  IF costRepChk.Checked
                     ASK freqVal TO Activate;
                  ELSE
                     ASK freqVal TO Deactivate;
                  END IF;
               OTHERWISE
            END CASE;
         WHEN 300: {Advanced Tab}
            lastClicked := ASK tabClicked LastPicked;
            CASE ASK lastClicked Id
               WHEN 306: {Avail Graph Check Box}
                  IF availGraphBox.Checked
                     ASK sampleBox TO Activate;
                  ELSE
                     ASK sampleBox TO Deactivate;
                  END IF;
               OTHERWISE
            END CASE;
         WHEN 400: {Avant Garde Tab}
            lastClicked := ASK tabClicked LastPicked;
            CASE ASK lastClicked Id
               WHEN 402: {Detailed Event by component Check Box}
                  IF detByCompChk.Checked
                     ASK detEventComp TO Activate;
                  ELSE
                     ASK detEventComp TO Deactivate;
                  END IF;
               OTHERWISE
            END CASE;
         OTHERWISE
      END CASE;
      Draw;
      IF SELF.Visible
         IF (radioBox.SelectedButton.Id = 1012) AND (simFailVal.Value() = 1.)
            ASK startTimeVal TO Deactivate;
         ELSE
            ASK startTimeVal TO Activate;
         END IF;
         ASK startTimeVal TO Draw;
      END IF;
      INHERITED BeSelected;
   END METHOD; {BeSelected}
END OBJECT; {SimBoxObj}

OBJECT PrintWinObj;
   ASK METHOD BeSelected;
   VAR
      numPages  : ValueBoxObj;
      dirRadBox : RadioBoxObj;
   BEGIN
      numPages := Descendant("NumPages", 102);
      dirRadBox := Descendant("DirRadBox", 104);
      lastClicked := ASK SELF LastPicked;
      INHERITED BeSelected;
   END METHOD; {BeSelected}
END OBJECT; {PrintWinBox}

OBJECT ResimBoxObj;
   ASK METHOD GetPreferences(INOUT lengthNum : REAL; 
                             INOUT truncType,runsNum : INTEGER; OUT cancelSim : BOOLEAN;
                             INOUT phasingInUse,
                                      inStep,startStep, statBarOn,
                                      graphics,evntChk,rsltChk,finalChk : BOOLEAN); 
   VAR
      validData : BOOLEAN;
      i : INTEGER;
      text     : strArray;
   BEGIN
      NEW(text, 1..6);
      rStopRadBox := Descendant("RStopRadBox",100);
         rTimeRadButton := Descendant("RTimeRadButton",1001);
         rFailRadButton := Descendant("RFailRadButton",1002);
         rCycleRadButton := Descendant("RCycleRadButton", 1003);
      rLengthVal := Descendant("RLengthValBox",101);
      rFailVal := Descendant("RFailValBox",102);
      rRunsVal := Descendant("RRunsValBox",103);
      rGraphicsBox := Descendant("RGraphicsBox",105);
      rStepBox    := Descendant("RStepBox",106);
      rResultsBox := Descendant("RFinalResultsBox",107);
      rRunResultsBox := Descendant("RRunResultsBox",108);
      rEventLogBox := Descendant("REventLogBox",109); 
      rStatusChkBox := Descendant("RStatusChkBox",110); 
      stopLabel1 := Descendant("StopLabel1",111);
      stopLabel2 := Descendant("StopLabel2",112);
      stopLabel3 := Descendant("StopLabel3", 113);
      rCycleVal  := Descendant("RCycleValBox", 114);
      Draw;   
      IF truncType = 1
         ASK rStopRadBox TO SetSelectedButton(rTimeRadButton);
         ASK rLengthVal TO Activate;
         ASK rLengthVal TO SetValue(lengthNum);
         ASK rFailVal TO SetValue(dFailTrunc);
         ASK rFailVal TO Deactivate;
         ASK rCycleVal TO SetValue(dCycleTrunc);
         ASK rCycleVal TO Deactivate;
         ASK stopLabel1 TO SetLabel(systemUnits);
         ASK stopLabel1 TO Activate;
         ASK stopLabel2 TO Deactivate;
         ASK stopLabel3 TO Deactivate;
      ELSIF truncType = 2
         ASK rStopRadBox TO SetSelectedButton(rFailRadButton);
         ASK rLengthVal TO SetValue(dTimeTrunc);
         ASK rLengthVal TO Deactivate;
         ASK rFailVal TO SetValue(lengthNum);
         ASK rFailVal TO Activate;
         ASK rCycleVal TO SetValue(dCycleTrunc);
         ASK rCycleVal TO Deactivate;
         ASK stopLabel1 TO SetLabel(systemUnits);
         ASK stopLabel1 TO Deactivate;
         ASK stopLabel2 TO Activate;
         ASK stopLabel3 TO Deactivate;
      ELSIF truncType = 3 {cycle truncated}
         ASK rStopRadBox TO SetSelectedButton(rCycleRadButton);
         ASK rLengthVal TO SetValue(lengthNum);
         ASK rLengthVal TO Deactivate;
         ASK rFailVal TO SetValue(dFailTrunc);
         ASK rFailVal TO Deactivate;
         ASK rCycleVal TO SetValue(dCycleTrunc);
         ASK rCycleVal TO Activate;
         ASK stopLabel1 TO SetLabel(systemUnits);
         ASK stopLabel1 TO Deactivate;
         ASK stopLabel2 TO Deactivate;
         ASK stopLabel3 TO Activate;
      END IF;
      IF activePhases = 0
         ASK rCycleRadButton TO Deactivate;
      END IF;
      ASK rRunsVal TO SetValue(FLOAT(runsNum));
      ASK rGraphicsBox TO SetCheck(graphics);
      IF compileType = "demo"
         ASK rEventLogBox TO SetCheck(FALSE);
         ASK rResultsBox TO SetCheck(FALSE);
         ASK rRunResultsBox TO SetCheck(FALSE);
         ASK rEventLogBox TO Deactivate;
         ASK rResultsBox TO Deactivate;
         ASK rRunResultsBox TO Deactivate;
      ELSE
         ASK rEventLogBox TO SetCheck(evntChk);
         ASK rResultsBox TO SetCheck(rsltChk);
      END IF;
      ASK rStepBox TO SetCheck(inStep);
      IF NOT dSimWithGraph
         ASK rStepBox TO Deactivate;
      END IF;
      ASK rStatusChkBox TO SetCheck(statBarOn);
      Draw;   
      REPEAT
         errors := 0;
         validData := TRUE;
         button := AcceptInput();        
         IF button.ReferenceName = "OKButton"
            IF (rRunsVal.Value() < 1.) OR (rRunsVal.Value() > 999999999.)
               INC(errors);
               text[errors] := "Number of runs must be between 1 and 999,999,999!     ";
               ASK rRunsVal TO SetValue(FLOAT(runsNum));
               validData := FALSE;
            ELSE
               numberOfRuns := TRUNC(rRunsVal.Value());
            END IF;
            IF rStopRadBox.SelectedButton.Id = 1001
               truncType := 1;
               IF (rLengthVal.Value() > 999999999.999999) OR (rLengthVal.Value() < 0.000001)
                  INC(errors);
                  text[errors] := "Stop simulation time must be between 0.000001 and 999,999,999.999999!     ";
                  ASK rLengthVal TO SetValue(dTimeTrunc);
                  validData := FALSE;
               ELSE
                  lengthNum := rLengthVal.Value();
               END IF;
            ELSIF rStopRadBox.SelectedButton.Id = 1002
               truncType := 2;
               IF (rFailVal.Value() < 1.) OR (rFailVal.Value() > 999999999.)
                  INC(errors);
                  text[errors] := "Stop simulation failure must be between 1 and 999,999,999!     ";
                  ASK rFailVal TO SetValue(dFailTrunc);
                  validData := FALSE;
               ELSE
                  lengthNum := rFailVal.Value();
               END IF;
            ELSIF rStopRadBox.SelectedButton.Id = 1003
               truncType := 3;
               IF (rCycleVal.Value() < 1.) OR (rCycleVal.Value() > 999999999.)
                  INC(errors);
                  text[errors] := "Stop simulation cycle must be between 1 and 999,999,999!     ";
                  ASK rCycleVal TO SetValue(dCycleTrunc);
                  validData := FALSE;
               ELSE
                  lengthNum := rCycleVal.Value();
               END IF;
            END IF;
            graphics := rGraphicsBox.Checked;
            evntChk  := rEventLogBox.Checked;
            finalChk := rResultsBox.Checked;
            rsltChk := rRunResultsBox.Checked;
            startStep := rStepBox.Checked;
            inStep := rStepBox.Checked;
            statBarOn := rStatusChkBox.Checked;
            cancelSim := FALSE;
         ELSE
            cancelSim := TRUE;
         END IF;
         Draw;
         IF errors > 1
            poolsDone := FALSE;
            NEW(message, 1..errors+2);
            message[1] := "The following errors must be corrected:   ";
            message[2] := "";
            FOR i := 1 TO errors
               message[i+2] := "   "+INTTOSTR(i)+". "+text[i];
            END FOR;
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         ELSIF errors = 1
            poolsDone := FALSE;
            NEW(message, 1..1);
            message[1] := text[1];
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
      UNTIL validData;
      DISPOSE(text);
   END METHOD; {GetPreferences}

   ASK METHOD BeSelected;
   BEGIN
      lastClicked := ASK SELF LastPicked;
      CASE lastClicked.Id
         WHEN 100:
            IF rStopRadBox.SelectedButton.Id = 1001
               ASK rLengthVal TO Activate;
               ASK stopLabel1 TO Activate;
               ASK rFailVal TO Deactivate;
               ASK stopLabel2 TO Deactivate;
               ASK rCycleVal TO Deactivate;
               ASK stopLabel3 TO Deactivate;
            ELSIF rStopRadBox.SelectedButton.Id = 1002
               ASK rLengthVal TO Deactivate;
               ASK stopLabel1 TO Deactivate;
               ASK rFailVal TO Activate;
               ASK stopLabel2 TO Activate; 
               ASK rCycleVal TO Deactivate;
               ASK stopLabel3 TO Deactivate;
            ELSIF rStopRadBox.SelectedButton.Id = 1003
               ASK rLengthVal TO Deactivate;
               ASK stopLabel1 TO Deactivate;
               ASK rFailVal TO Deactivate;
               ASK stopLabel2 TO Deactivate;
               ASK rCycleVal TO Activate;
               ASK stopLabel3 TO Activate;
            END IF;
         WHEN 105:
            IF rGraphicsBox.Checked
               ASK rStepBox TO Activate;
            ELSE
               ASK rStepBox TO Deactivate;
            END IF;
         OTHERWISE
      END CASE;
      Draw;   
      INHERITED BeSelected;
   END METHOD; {BeSelected}
END OBJECT; {ResimBoxObj}

OBJECT ResultsBoxObj;
   ASK METHOD BeClosed;
   BEGIN
   END METHOD; {BeClosed};
END OBJECT; {ResultsBoxObj}

OBJECT BlockInputBoxObj;
   ASK METHOD InitSelf(IN printing : BOOLEAN);
   VAR
      i,j,n, numPM, hiercount  : INTEGER;
      block, tempBlock         : RBDBlockObj;
      tempEvent                : RBDEventObj;
      tempLink               : LinkObj;
      tempNode               : RBDNodeObj;
      hierList               : OptionListType;
      text2, stagger, test : STRING;
      logMean2, logVar : REAL;
      tempHier : RBDHierObj;
      pool : SparePoolObj;
   BEGIN
      distroTab        := Child("DistroTab", 100);
      distroTable      := Descendant("DistroTable", 101);
      distroFilter     := Descendant("DistroFilter", 115);
      dFilterText      := Descendant("DFilterText", 116);
      applyButton      := Descendant("ApplyButton", 114);
      sparesTab        := Child("SparesTab", 200);
      sparesTable      := Descendant("SparesTable", 201);
      sparesFilter     := Descendant("SparesFilter", 202);
      sFilterText      := Descendant("SFilterText", 203);
      pmTab            := Child("PMTab", 300);
      pmTable          := Descendant("PMTable", 301);
      pmFilter         := Descendant("PMFilter", 302);
      pmFilterText     := Descendant("PMFilterText", 303);
      maintTab         := Child("MaintTab", 400);
      maintTable       := Descendant("MaintTable", 401);
      maintFilter      := Descendant("MaintFilter", 402);
      mFilterText      := Descendant("MFilterText", 403);
      costsTab         := Child("CostsTab", 500);
      costTable        := Descendant("CostTable", 501);
      costFilter       := Descendant("CostFilter", 502);
      cFilterText      := Descendant("CFilterText", 503);
      depTab           := Child("DepTab", 600);
      depTable         := Descendant("DepTable", 601);
      depFilter        := Descendant("DepFilter", 602);
      depFilterText    := Descendant("DepFilterText", 603);
      advTab           := Child("AdvTab", 700);
      advTable         := Descendant("AdvTable", 701);
      advFilter        := Descendant("AdvFilter", 702);
      aFilterText      := Descendant("AFilterText", 703);
      failHeader       := Descendant("failures", 0);
      repHeader        := Descendant("repairs", 0);
      oldDFilter   := "All";
      oldSFilter   := "All";
      oldPMFilter  := "All";
      oldMFilter   := "All";
      oldCFilter   := "All";
      oldDepFilter := "All";
      oldAFilter   := "All";
      IF NOT printing
         Draw;
         IF totalHiers > 0
            NEW(hierList, 1..totalHiers+3);
            InitHierFilter(hierList);
            ASK distroFilter TO SetOptions(hierList);
            ASK distroFilter TO SetText(hierList[1]);
            ASK sparesFilter TO SetOptions(hierList);
            ASK sparesFilter TO SetText(hierList[1]);
            ASK pmFilter TO SetOptions(hierList);
            ASK pmFilter TO SetText(hierList[1]);
            ASK maintFilter TO SetOptions(hierList);
            ASK maintFilter TO SetText(hierList[1]);
            ASK costFilter TO SetOptions(hierList);
            ASK costFilter TO SetText(hierList[1]);
            ASK depFilter TO SetOptions(hierList);
            ASK depFilter TO SetText(hierList[1]);
            ASK advFilter TO SetOptions(hierList);
            ASK advFilter TO SetText(hierList[1]);
         ELSE
            ASK distroFilter TO SetText("Home");
            ASK distroFilter TO Deactivate;
            ASK dFilterText TO Deactivate;
            ASK sparesFilter TO SetText("Home");
            ASK sparesFilter TO Deactivate;
            ASK sFilterText TO Deactivate;
            ASK pmFilter TO SetText("Home");
            ASK pmFilter TO Deactivate;
            ASK pmFilterText TO Deactivate;
            ASK maintFilter TO SetText("Home");
            ASK maintFilter TO Deactivate;
            ASK mFilterText TO Deactivate;
            ASK depFilter TO SetText("Home");
            ASK depFilter TO Deactivate;
            ASK depFilterText TO Deactivate;
            ASK advFilter TO SetText("Home");
            ASK advFilter TO Deactivate;
            ASK aFilterText TO Deactivate;
         END IF;
      END IF;
      NEW(translateArray, 1..totalBlocks);
      NEW(labels, 1..6);
      FOR i := 102 TO 107
         labels[i-101] := Descendant("label", i);
         ASK labels[i-101] TO SetHidden(TRUE);
      END FOR;
      NEW(values, 1..6);
      FOR i := 108 TO 113
         values[i-107] := Descendant("values", i);
         ASK values[i-107] TO SetHidden(TRUE);
      END FOR;
      ASK failHeader TO SetHidden(TRUE);
      ASK repHeader TO SetHidden(TRUE);
      FOREACH block IN blockGroup
         IF block.usesPM
            INC(numPM);
         END IF;
      END FOREACH;
      IF totalBlocks > 11
         ASK distroTable TO SetSize(9, totalBlocks);
      ELSE
         ASK distroTable TO SetSize(9, 11);
      END IF;
      {Fill up Distro Table and display array}
      NEW(DArray, 1..totalBlocks, 1..11);
      j:=1;
      FOREACH block IN blockGroup
         translateArray[j] := block.Id;
         DArray[j,1] := block.name;
         ConvertToString(block.failDistro, text2);
         DArray[j,2] := text2;
         IF block.failDistro <> 16
            FOR n:=1 TO 3
               DArray[j,2+n] := "";                       
            END FOR;
            FOR n:=1 TO block.numFailParams
               IF ((lambdaMode) AND (block.failDistro=4) AND (n=1))
                  DArray[j,2+n] := ChopZeros(1.0/block.failVals[n],9);            
               ELSIF ((muSigmaMode) AND (block.failDistro=7) AND (n=1))
                  logMean2 := POWER(block.failVals[1], 2.);
                  logVar := POWER(block.failVals[2], 2.);
                  DArray[j,2+n] := ChopZeros(LN(logMean2/SQRT(logVar + logMean2)),9);            
               ELSIF ((muSigmaMode) AND (block.failDistro=7) AND (n=2))
                  DArray[j,2+n] := ChopZeros(SQRT(LN((logVar + logMean2)/logMean2)),9);                       
               ELSE
                  DArray[j,2+n] := ChopZeros(block.failVals[n],9);
               END IF;
            END FOR;
         ELSE
            FOR n := 1 TO 3
               DArray[j,2+n] := "...";
            END FOR;
         END IF;
         ConvertToString(block.repairDistro, text2);
         DArray[j,6] := text2;
         IF (block.repairDistro <> 16) AND (block.repairDistro <> 18)
            FOR n := 1 TO 3
               DArray[j,6+n] := "";                       
            END FOR;
            FOR n := 1 TO block.numRepairParams
               IF ((lambdaMode) AND (block.repairDistro=4) AND (n=1))
                  DArray[j,6+n] := ChopZeros(1.0/block.repairVals[n],9);            
               ELSIF ((muSigmaMode) AND (block.repairDistro=7) AND (n=1))
                  logMean2 := POWER(block.repairVals[1], 2.);
                  logVar := POWER(block.repairVals[2], 2.);
                  DArray[j,6+n] := ChopZeros(LN(logMean2/SQRT(logVar + logMean2)),9);            
               ELSIF ((muSigmaMode) AND (block.repairDistro=7) AND (n=2))
                  DArray[j,6+n] := ChopZeros(SQRT(LN((logVar + logMean2)/logMean2)),9);                       
               ELSE
                  DArray[j,6+n] := ChopZeros(block.repairVals[n],9);
               END IF;
            END FOR;
         ELSIF block.repairDistro = 16
            FOR n := 1 TO 3
               DArray[j,6+n] := "...";
            END FOR;
         ELSE
            FOR n := 1 TO 3
               DArray[j,6+n] := "";
            END FOR;
         END IF;
         IF block.parentID > 0
            tempHier := ASK root Child("RBDHier", block.parentID);
            DArray[j,10] := INTTOSTR(tempHier.Id);

         ELSE
            DArray[j,10] := "0";
         END IF;
         DArray[j,11] := INTTOSTR(block.Id);
         INC(j);
      END FOREACH;
      HierFilter("distro", distroTable, DArray, HIGH(DArray), 10, "All", printing, displayDArray, hiercount);
      IF ((hiercount = 0) AND (NOT printing))
         ASK distroFilter TO SetText("Home");
         ASK distroFilter TO Deactivate;
         ASK dFilterText TO Deactivate;
      END IF;
      ASK applyButton TO SetSelectable(FALSE);
      DistSort(1, totalBlocks, "A");
      sort1 := "1A";
      IF totalBlocks > 15
         ASK sparesTable TO SetSize(9, totalBlocks);
      ELSE
         ASK sparesTable TO SetSize(9, 15);
      END IF;
      {Fill up Spares Table and display array}
      NEW(SArray, 1..totalBlocks, 1..10);
      j:=1;
      FOREACH block IN blockGroup
         SArray[j,1] := block.name;
         IF block.sparingType = Infinite
            SArray[j,2] := "Infinite";
            SArray[j,3] := "N/A";
            SArray[j,4] := "N/A";
            SArray[j,5] := "N/A";
            SArray[j,6] := "N/A";
            SArray[j,7] := "N/A";
            SArray[j,8] := "N/A";
            SArray[j,9] := "N/A";
         ELSIF block.sparingType = SparePool
            SArray[j,2] := block.poolName;
            FOREACH pool IN poolGroup
               IF ((pool.sparingType = SparePool) AND (pool.poolName = block.poolName))
                  SArray[j,3] := INTTOSTR(pool.initialSpares);
                  IF pool.routineSpareOrdering
                     SArray[j,4] := INTTOSTR(pool.newSpares);
                     SArray[j,5] := ChopZeros(pool.newSparesArrival,9);
                  ELSE
                     SArray[j,4] := "N/A";
                     SArray[j,5] := "N/A";
                  END IF;
                  IF pool.stockLevelOrdering
                     SArray[j,6] := INTTOSTR(pool.SLOOrderLevel);
                     SArray[j,7] := INTTOSTR(pool.SLONewSpares);
                     SArray[j,8] := ChopZeros(pool.SLOTime,9);
                  ELSE
                     SArray[j,6] := "N/A";
                     SArray[j,7] := "N/A";
                     SArray[j,8] := "N/A";
                  END IF;
                  IF pool.emerSpareOrdering
                     SArray[j,9] := ChopZeros(pool.emergencyTime,9);
                  ELSE
                     SArray[j,9] := "N/A";
                  END IF;
               END IF;
            END FOREACH;
         ELSE
            SArray[j,2] := "Custom";
            SArray[j,3] := INTTOSTR(block.initStock);
            IF block.routineSpareOrdering
               SArray[j,4] := INTTOSTR(block.newSpares);
               SArray[j,5] := ChopZeros(block.arrivalRate,9);
            ELSE
               SArray[j,4] := "N/A";
               SArray[j,5] := "N/A";
            END IF;
            IF block.emerSpareOrdering
               SArray[j,9] := ChopZeros(block.emerTime,9);
            ELSE
               SArray[j,9] := "N/A";
            END IF;
            IF block.stockLevelOrdering
               SArray[j,6] := INTTOSTR(block.SLOOrderLevel);
               SArray[j,7] := INTTOSTR(block.SLONewSpares);
               SArray[j,8] := ChopZeros(block.SLOTime,9);
            ELSE
               SArray[j,6] := "N/A";
               SArray[j,7] := "N/A";
               SArray[j,8] := "N/A";
            END IF;
         END IF;      
         IF block.parentID > 0
            tempHier := ASK root Child("RBDHier", block.parentID);
            SArray[j,10] := INTTOSTR(tempHier.Id);
         ELSE
            SArray[j,10] := "0";
         END IF;
         INC(j);
      END FOREACH;
      HierFilter("bspares", sparesTable, SArray, HIGH(SArray), 10, "All"{-1}, printing, displaySArray, hiercount);
      IF ((hiercount = 0) AND (NOT printing))
         ASK sparesFilter TO SetText("Home");
         ASK sparesFilter TO Deactivate;
         ASK sFilterText TO Deactivate;
      END IF;
      RepSort(1, totalBlocks, "A");
      sort2 := "1A";
      IF numPM = 0
         IF NOT printing
            Draw;
            ASK pmTable TO SetSize(9, 15);
            ASK pmTab TO Deactivate;
         END IF;
      ELSE
         IF numPM > 15
            ASK pmTable TO SetSize(9, numPM);
         ELSE
            ASK pmTable TO SetSize(9, 15);
         END IF;
         {Fill up PM Table and display array}
         NEW(PMArray, 1..numPM, 1..10);
         j:=1;
         FOREACH block IN blockGroup
            IF block.usesPM
               PMArray[j,1] := block.name;
               IF block.pmTriggered 
                  stagger := "N/A";
                  PMArray[j,2] := block.pmTrig;
               ELSE
                  stagger := REALTOSTR(block.pmStagger);
                  PMArray[j,2] := REALTOSTR(block.pmFreq);
               END IF;
               MakeDistString(block.pmDist, block.pmParams, tempString);
               PMArray[j,3] := tempString; 
               PMArray[j,4] := stagger;
               IF block.pmSpareNeeded
                  PMArray[j,5] := "True";
               ELSE
                  PMArray[j,5] := "False";
               END IF;
               IF block.pmRefresh
                  PMArray[j,6] := "True";
               ELSE
                  PMArray[j,6] := "False";
               END IF;
               IF block.pmMisDefer
                  PMArray[j,7] := "True";
               ELSE
                  PMArray[j,7] := "False";
               END IF;
               IF block.pmReqDefer
                  PMArray[j,8] := "True";
               ELSE
                  PMArray[j,8] := "False";
               END IF;
               IF block.pmTriggered
                  PMArray[j,9] := "N/A";
               ELSE
                  IF block.pmFailReset
                     PMArray[j,9] := "True";
                  ELSE
                     PMArray[j,9] := "False";
                  END IF;
               END IF;  
               IF block.parentID > 0
                  tempHier := ASK root Child("RBDHier", block.parentID);
                  PMArray[j,10] := INTTOSTR(tempHier.Id);
               ELSE
                  PMArray[j,10] := "0";
               END IF;
               INC(j);
            END IF;
         END FOREACH;
         HierFilter("pm", pmTable, PMArray, HIGH(PMArray), 10, "All"{-1}, printing, displayPMArray, hiercount);
         IF ((hiercount = 0) AND (NOT printing))
            ASK pmFilter TO SetText("Home");
            ASK pmFilter TO Deactivate;
            ASK pmFilterText TO Deactivate;
         END IF;
         PMSort(1, numPM, "A");
         sort3 := "1A";
      END IF;
      IF totalBlocks > 15
         ASK maintTable TO SetSize(6, totalBlocks);
      ELSE
         ASK maintTable TO SetSize(6, 15);
      END IF;
      {Fill up Maint Table and display array}
      NEW(MArray, 1..totalBlocks, 1..7);
      j:=1;
      FOREACH block IN blockGroup
         MArray[j,1] := block.name;
         MakeDistString(block.preDist, block.preParams, tempString);
         MArray[j,2] := tempString;
         MakeDistString(block.postDist, block.postParams, tempString);
         MArray[j,3] := tempString; 
         IF ((block.res1Name = "unnamed") OR (block.res1Name = "None"))
            MArray[j,4] := "None";
            MArray[j,5] := "N/A";
            MArray[j,6] := "N/A";
         ELSE
            MArray[j,4] := block.res1Name;
            MArray[j,5] := INTTOSTR(block.numRes1);
            MArray[j,6] := INTTOSTR(block.numRes1PM);
         END IF;
         IF block.parentID > 0
            tempHier := ASK root Child("RBDHier", block.parentID);
            MArray[j,7] := INTTOSTR(tempHier.Id);
         ELSE
            MArray[j,7] := "0";
         END IF;
         INC(j);
      END FOREACH;
      HierFilter("maint", maintTable, MArray, HIGH(MArray), 7, "All"{-1}, printing, displayMArray, hiercount);
      IF ((hiercount = 0) AND (NOT printing))
         ASK maintFilter TO SetText("Home");
         ASK maintFilter TO Deactivate;
         ASK mFilterText TO Deactivate;
      END IF;
      MaintSort(1, totalBlocks, "A");
      sort4 := "1A";
      IF totalBlocks > 15
         ASK costTable TO SetSize(15, totalBlocks);
      ELSE
         ASK costTable TO SetSize(15, 15);
      END IF;
      {Fill up Cost Table and display array}
      NEW(CArray, 1..totalBlocks, 1..17);
      j:=1;
      FOREACH block IN blockGroup
         CArray[j,1] := block.name;
         CArray[j,2] := REALTOCOST(block.operatingCost,13,3);
         CArray[j,3] := REALTOCOST(block.standbyCost,13,3);
         CArray[j,4] := REALTOCOST(block.repairingCost,13,3);
         CArray[j,5] := REALTOCOST(block.repFixedCost,13,3);
         CArray[j,6] := REALTOCOST(block.pmCost,13,3);
         CArray[j,7] := REALTOCOST(block.pmFixedCost,13,3);
         CArray[j,8] := REALTOCOST(block.repHoldCost,13,3);
         CArray[j,9] := REALTOCOST(block.pmHoldCost,13,3);
         CArray[j,10] := REALTOCOST(block.idleCost,13,3);
         CArray[j,11] := REALTOCOST(block.doneCost,13,3);
         CArray[j,12] := REALTOCOST(block.initialCost,13,3);
         IF block.sparingType = SparePool
            FOREACH pool IN poolGroup
               IF ((pool.sparingType = SparePool) AND (pool.poolName = block.poolName))
                  CArray[j,13] := REALTOCOST(pool.spareCost,13,3);
               END IF;
            END FOREACH;
         ELSE
            CArray[j,13] := REALTOCOST(block.spareCost,13,3);
         END IF;
         CArray[j,14] := REALTOCOST(block.emerShippingCost,13,3);
         CArray[j,15] := REALTOCOST(block.doneFixedCost,13,3);
         IF block.alwaysAddDoneCost
            CArray[j,16] := "True";
         ELSE
            CArray[j,16] := "False";
         END IF;
         IF block.parentID > 0
            tempHier := ASK root Child("RBDHier", block.parentID);
            CArray[j,17] := INTTOSTR(tempHier.Id);
         ELSE
            CArray[j,17] := "0";
         END IF;
         INC(j);
      END FOREACH;
      HierFilter("cost", costTable, CArray, HIGH(CArray), 17, "All"{-1}, printing, displayCArray, hiercount);
      IF ((hiercount = 0) AND (NOT printing))
         ASK costFilter TO SetText("Home");
         ASK costFilter TO Deactivate;
         ASK cFilterText TO Deactivate;
      END IF;
      CostSort(0, totalBlocks, "A");
      sort5 := "0A"; 
      IF totalBlocks > 15
         ASK depTable TO SetSize(8, totalBlocks);
      ELSE
         ASK depTable TO SetSize(8, 15);
      END IF;
      {Fill up Dependency Table and display array}
      NEW(DepArray, 1..totalBlocks, 1..9);
      j:=1;
      FOREACH block IN blockGroup
         DepArray[j,1] := block.name;
         IF block.DependencyNum=-2            
            DepArray[j,2] := "System Dependent";
         ELSIF block.DependencyNum=-1            
            DepArray[j,2] := "Locally Dependent";
         ELSIF block.DependencyNum=0            
            DepArray[j,2] := "Independent";
         ELSE
            IF block.depType = "RBDBlock"
               tempBlock := ASK root Child("RBDBlock",block.DependencyNum); 
               DepArray[j,2] := tempBlock.name+" Block";
            ELSIF block.depType = "RBDEvent"
               tempEvent := ASK root Child("RBDEvent",block.DependencyNum); 
               DepArray[j,2] := tempEvent.name+" Event";
            ELSE
               tempNode := ASK root Child("RBDNode",block.DependencyNum);  
               IF tempNode.typeNode = 5
                  tempHier := ASK root Child("RBDHier", tempNode.parentID);
                  DepArray[j,2] := tempHier.name + " Hier";
               ELSE
                  DepArray[j,2] := tempNode.name + " Node";
               END IF;
            END IF;
         END IF;
         IF block.DependencyNum = 0
            DepArray[j,3] := "N/A";
            DepArray[j,4] := "N/A";
            DepArray[j,5] := "N/A";
            DepArray[j,6] := "N/A";
            DepArray[j,7] := "N/A";
            DepArray[j,8] := "N/A";
         ELSE
            DepArray[j,3] := REALTOSTR(block.DepNothingPerc);
            DepArray[j,4] := REALTOSTR(block.DepIdlePerc);
            DepArray[j,5] := REALTOSTR(block.DepPMPerc);
            DepArray[j,6] := REALTOSTR(block.DepPMThreshold);;
            DepArray[j,7] := REALTOSTR(block.DepFailPerc);
            IF block.defDepStateIdle
               DepArray[j,8] := "Idle";
            ELSE
               DepArray[j,8] := "Operating";
            END IF;
         END IF;      
         IF block.parentID > 0
            tempHier := ASK root Child("RBDHier", block.parentID);
            DepArray[j,9] := INTTOSTR(tempHier.Id);
         ELSE
            DepArray[j,9] := "0";
         END IF;
         INC(j);
      END FOREACH;
      HierFilter("dep", depTable, DepArray, HIGH(DepArray), 9, "All"{-1}, printing, displayDepArray, hiercount);
      IF ((hiercount = 0) AND (NOT printing))
         ASK depFilter TO SetText("Home");
         ASK depFilter TO Deactivate;
         ASK depFilterText TO Deactivate;
      END IF;
      DepSort(1, totalBlocks, "A")
      sort6 := "1A";
      IF totalBlocks > 15
         ASK advTable TO SetSize(10, totalBlocks);
      ELSE
         ASK advTable TO SetSize(10, 15);
      END IF;
      {Fill up Advanced Table and display array}
      NEW(AArray, 1..totalBlocks, 1..11);
      j:=1;
      FOREACH block IN blockGroup
         AArray[j,1] := block.name;
         IF block.failStream = 201
            text2:="A";
         ELSIF block.failStream = 202
            text2:="B";
         ELSIF block.failStream = 203
            text2:="C";
         ELSIF block.failStream = 204
            text2:="D";
         ELSIF block.failStream = 205
            text2:="E";
         ELSIF block.failStream = 206
            text2:="F";
         ELSIF block.failStream = 207
            text2:="G";
         ELSIF block.failStream = 208
            text2:="H";
         ELSIF block.failStream = 209
            text2:="I";
         ELSIF block.failStream = 210
            text2:="J";
         ELSE
            text2:=INTTOSTR(block.failStream);
         END IF;
         AArray[j,2] := text2;
         IF block.repairStream = 201
            text2:="A";
         ELSIF block.repairStream = 202
            text2:="B";
         ELSIF block.repairStream = 203
            text2:="C";
         ELSIF block.repairStream = 204
            text2:="D";
         ELSIF block.repairStream = 205
            text2:="E";
         ELSIF block.repairStream = 206
            text2:="F";
         ELSIF block.repairStream = 207
            text2:="G";
         ELSIF block.repairStream = 208
            text2:="H";
         ELSIF block.repairStream = 209
            text2:="I";
         ELSIF block.repairStream = 210
            text2:="J";
         ELSE
            text2:=INTTOSTR(block.repairStream);
         END IF;
         AArray[j,3] := text2;
         IF block.simStartType = 1
            AArray[j,4] := "Random";
         ELSIF block.simStartType = 2
            AArray[j,4] := "Up";
         ELSIF block.simStartType = 3
            AArray[j,4] := "Up";
         ELSIF block.simStartType = 4
            AArray[j,4] := "Down";
         ELSIF block.simStartType = 5
            AArray[j,4] := "Down";
         ELSE
            AArray[j,4] := "Down";
         END IF;
         IF block.simStartType = 1
            AArray[j,5] := "N/A";
         ELSIF block.simStartType = 2
            AArray[j,5] := "Random";
         ELSIF block.simStartType = 3
            AArray[j,5] := REALTOSTR(block.amountExhausted);
         ELSIF block.simStartType = 4
            AArray[j,5] := "Random";
         ELSIF block.simStartType = 5
            AArray[j,5] := REALTOSTR(block.amountExhausted);
         ELSE
            AArray[j,5] := "At Start";
         END IF;
         IF block.usesPhasing
            AArray[j,6] := "True";
         ELSE
            AArray[j,6] := "False";
         END IF;
         IF block.GDType = 0
            AArray[j,7] := "N/A";
         ELSIF block.GDType = 1
            AArray[j,7] := "Linear";
         ELSIF block.GDType = 2
            AArray[j,7] := "Geometric";
         ELSE
            AArray[j,7] := "Asymptotic";
         END IF;
         IF block.GDType > 0
            AArray[j,8] := REALTOSTR(block.GDRate);
         ELSE
            AArray[j,8] := "N/A";
         END IF;
         IF block.GDType > 0
            AArray[j,9] := REALTOSTR(block.GDLimit);
         ELSE
            AArray[j,9] := "N/A";
         END IF;
         AArray[j,10] := REALTOSTR(block.sbStress);
         IF block.parentID > 0
            tempHier := ASK root Child("RBDHier", block.parentID);
            AArray[j,11] := INTTOSTR(tempHier.Id);
         ELSE
            AArray[j,11] := "0";
         END IF;
         INC(j);
      END FOREACH;
      HierFilter("adv", advTable, AArray, HIGH(AArray), 11, "All"{-1}, printing, displayAArray, hiercount);
      IF ((hiercount = 0) AND (NOT printing))
         ASK advFilter TO SetText("Home");
         ASK advFilter TO Deactivate;
         ASK aFilterText TO Deactivate;
      END IF;
      AdvSort(1, totalBlocks, "A");
      sort7 := "1A"; 
      IF NOT printing
         Draw;
      END IF;
   END METHOD; {InitSelf, BlockInputBoxObj}

   ASK METHOD BeSelected;
   VAR
      validRepairs,failDistChanged,
      validFails,repairDistChanged : BOOLEAN;
      i, hiercount    , test             : INTEGER;
      textValue                    : STRING;
      block                        : RBDBlockObj;
      newFvals, newRvals           : realArray;
      objClicked                   : GraphicVObj;
      mu,sigma2                    : REAL;
      startfVal, startrVal         : ARRAY INTEGER OF STRING;
      dumbArray                    : timeType;
      table                        : TableObj;    
      text                         : strArray;
   BEGIN
      NEW(table);        {needed}
      objClicked := LastPicked;
      CASE objClicked.Id
         WHEN 100:  {Distributions Tab}         
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 101 {Distribution Table}
               IF (distroTable.SelectedRow > 0) AND (distroTable.SelectedRow <= totalBlocks)
                  block := ASK root Child ("RBDBlock", translateArray[distroTable.SelectedRow]);
                  IF (block.repairDistro <> 16) AND (block.repairDistro <> 18) 
                     ManageLabels(block.repairDistro, FALSE);
                     FOR i := 1 TO block.numRepairParams
                        textValue := ASK distroTable Text(6+i, distroTable.SelectedRow);
                        ASK values[3+i] TO SetValue(STRTOREAL(textValue));
                        ASK values[3+i] TO SetHidden(FALSE);
                     END FOR;
                     ASK applyButton TO Activate;
                  ELSIF block.repairDistro = 16
                     ManageLabels(16, FALSE);
                  ELSIF block.repairDistro = 18
                     ManageLabels(18, FALSE);
                  END IF;
                  IF block.failDistro <> 16
                     ManageLabels(block.failDistro, TRUE);
                     FOR i := 1 TO block.numFailParams
                        textValue := ASK distroTable Text(2+i, distroTable.SelectedRow);
                        ASK values[i] TO SetValue(STRTOREAL(textValue));
                        ASK values[i] TO SetHidden(FALSE);
                     END FOR;
                     ASK applyButton TO Activate;
                  ELSE
                     ManageLabels(16, TRUE);
                  END IF;
                  ASK applyButton TO SetDefault(TRUE);
               ELSE
                  IF STRTOINT(SUBSTR(1,1,sort1)) = distroTable.SelectedColumn
                     IF SUBSTR(2,2,sort1) = "A"
                        DistSort(distroTable.SelectedColumn, HIGH(displayDArray), "D");
                        sort1 := INTTOSTR(distroTable.SelectedColumn)+"D";
                     ELSE
                        DistSort(distroTable.SelectedColumn, HIGH(displayDArray), "A");
                        sort1 := INTTOSTR(distroTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     DistSort(distroTable.SelectedColumn, HIGH(displayDArray), "A");
                     sort1 := INTTOSTR(distroTable.SelectedColumn)+"A";
                     ASK failHeader TO SetHidden(TRUE);
                     ASK repHeader TO SetHidden(TRUE);
                     FOR i := 1 TO 6
                        ASK labels[i] TO SetHidden(TRUE);
                        ASK values[i] TO SetHidden(TRUE);
                     END FOR;  
                     ASK applyButton TO Deactivate;
                  END IF;
               END IF;
               ASK distroTable TO ReceiveFocus;
            ELSIF lastClicked.Id = 114  {Apply button}
               errors := 0;
               NEW(text,1..10);
               NEW(startfVal,1..3);
               NEW(startrVal,1..3);
               FOR i:=1 TO 3
                  startfVal[i]:=ASK distroTable Text(2+i, distroTable.SelectedRow);
                  startrVal[i]:=ASK distroTable Text(6+i, distroTable.SelectedRow);
               END FOR;
               block := ASK root Child ("RBDBlock", translateArray[distroTable.SelectedRow]);
               NEW(newFvals, 1..block.numFailParams);
               NEW(newRvals, 1..block.numRepairParams);
               validFails   := TRUE;
               validRepairs := TRUE;
               failDistChanged:=FALSE;
               IF block.failDistro <> 16 
                  FOR i := 1 TO block.numFailParams
                     newFvals[i] := ASK values[i] Value();
                     IF startfVal[i] <> ChopZeros(newFvals[i],9)
                        failDistChanged:=TRUE;
                     END IF;
                  END FOR;
               END IF;
               IF failDistChanged
                  FOR i := 1 TO block.numRepairParams
                     newRvals[i] := block.repairVals[i];
                  END FOR;
                  CheckValidInput(block.failDistro, block.numFailParams, newFvals, "Failure ", validFails, text);
                  IF validFails
                     FOR i := 1 TO block.numFailParams
                        ASK distroTable TO SetText(ChopZeros(newFvals[i],9), 2+i, distroTable.SelectedRow);
                        IF ((lambdaMode) AND (block.failDistro=4) AND (i=1))
                           newFvals[1]:=1.0/newFvals[i];
                        ELSIF ((muSigmaMode) AND (block.failDistro=7) AND (i=1))
                           mu:=newFvals[1];
                           sigma2:=POWER(newFvals[2],2.0);                           
                           newFvals[1]:=EXP(mu+sigma2/2.0);
                        ELSIF ((muSigmaMode) AND (block.failDistro=7) AND (i=2))
                           newFvals[2]:=SQRT(EXP(2.0*mu+sigma2)*(EXP(sigma2)-1.));
                        END IF;
                     END FOR;
                     ASK distroTable TO Update;                  
                     ASK block  TO SetNewBlockParams(newFvals, newRvals);
                     ASK block TO SetStats();
                     somethingChanged := TRUE;
                  END IF; 
               END IF;
               repairDistChanged:=FALSE;
               IF (block.repairDistro <> 16 ) AND (block.repairDistro <> 18)
                  FOR i := 1 TO block.numRepairParams
                     newRvals[i] := ASK values[3+i] Value();
                     IF startrVal[i] <> ChopZeros(newRvals[i],9)
                        repairDistChanged:=TRUE;
                     END IF;
                  END FOR;
               END IF;
               IF repairDistChanged
                  FOR i := 1 TO block.numFailParams
                     newFvals[i] := block.failVals[i];
                  END FOR;
                  CheckValidInput(block.repairDistro, block.numRepairParams, newRvals, "Repair ",validRepairs, text);
                  IF validRepairs
                     FOR i := 1 TO block.numRepairParams
                        ASK distroTable TO SetText(ChopZeros(newRvals[i],9), 6+i, distroTable.SelectedRow);
                        IF ((lambdaMode) AND (block.repairDistro=4) AND (i=1))
                           newRvals[1]:=1.0/newRvals[i];
                        ELSIF ((muSigmaMode) AND (block.repairDistro=7) AND (i=1))
                           mu:=newRvals[1];
                           sigma2:=POWER(newRvals[2],2.0);                           
                           newRvals[1]:=EXP(mu+sigma2/2.0);
                        ELSIF ((muSigmaMode) AND (block.failDistro=7) AND (i=2))
                           newRvals[2]:=SQRT(EXP(2.0*mu+sigma2)*(EXP(sigma2)-1.));
                        END IF;
                     END FOR;
                     ASK distroTable TO Update;                  
                     ASK block TO SetNewBlockParams(newFvals, newRvals);
                     ASK block TO SetStats();
                     somethingChanged := TRUE;
                  END IF;
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
               ELSE
                  ASK failHeader TO SetHidden(TRUE);
                  ASK repHeader TO SetHidden(TRUE);
                  FOR i := 1 TO 6
                     ASK labels[i] TO SetHidden(TRUE);
                     ASK values[i] TO SetHidden(TRUE);
                  END FOR;
                  ASK applyButton TO Deactivate;
               END IF;
               ASK distroTable TO ReceiveFocus;
               DISPOSE(startfVal);
               DISPOSE(startrVal);
               DISPOSE(newFvals);
               DISPOSE(newRvals);
            ELSIF lastClicked.Id = 115
               IF ((distroFilter.Text() = "--------------------------------------") 
                   OR (distroFilter.Text() = oldDFilter))
                  ASK distroFilter TO SetText(oldDFilter);
               ELSE
                  HierFilter("distro", distroTable, DArray, HIGH(DArray), 10, distroFilter.Text(), FALSE, displayDArray, hiercount);
                  oldDFilter := distroFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 972 {Print Button}
               IF compileType <> "demo"
                  PrintInputTable(distroTable, "Distro", "Print");
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
            DISPOSE(text);
         WHEN 200:  {Spares Tab}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 201 {Rep Table}
               IF sparesTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort2)) = sparesTable.SelectedColumn
                     IF SUBSTR(2,2,sort2) = "A"
                        RepSort(sparesTable.SelectedColumn, HIGH(displaySArray), "D");
                        sort2 := INTTOSTR(sparesTable.SelectedColumn)+"D";
                     ELSE
                        RepSort(sparesTable.SelectedColumn, HIGH(displaySArray), "A");
                        sort2 := INTTOSTR(sparesTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     RepSort(sparesTable.SelectedColumn, HIGH(displaySArray), "A");
                     sort2 := INTTOSTR(sparesTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 202
               IF ((sparesFilter.Text() = "--------------------------------------") 
                   OR (sparesFilter.Text() = oldSFilter))
                  ASK sparesFilter TO SetText(oldSFilter);
               ELSE
                  HierFilter("bspares", sparesTable, SArray, HIGH(SArray), 10, sparesFilter.Text(), FALSE, displaySArray, hiercount);
                  oldSFilter := sparesFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 973 {Print Button}
               IF compileType <> "demo"
                  PrintInputTable(sparesTable, "Sparing", "Print");
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 300:  {PM Tab}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 301 {PM Table}
               IF pmTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort3)) = pmTable.SelectedColumn
                     IF SUBSTR(2,2,sort3) = "A"
                        PMSort(pmTable.SelectedColumn, HIGH(displayPMArray), "D");
                        sort3 := INTTOSTR(pmTable.SelectedColumn)+"D";
                     ELSE
                        PMSort(pmTable.SelectedColumn, HIGH(displayPMArray), "A");
                        sort3 := INTTOSTR(pmTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     PMSort(pmTable.SelectedColumn, HIGH(displayPMArray), "A");
                     sort3 := INTTOSTR(pmTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 302
               IF ((pmFilter.Text() = "--------------------------------------") 
                   OR (pmFilter.Text() = oldPMFilter))
                  ASK pmFilter TO SetText(oldPMFilter);
               ELSE
                  HierFilter("pm", pmTable, PMArray, HIGH(PMArray), 10, pmFilter.Text(), FALSE, displayPMArray, hiercount);
                  oldPMFilter := pmFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 974 {Print Button}
               IF compileType <> "demo"
                  PrintInputTable(pmTable, "PM", "Print");
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 400:  {Maintenance Delay Tab}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 401 {Maintenance Delay Table}
               IF maintTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort4)) = maintTable.SelectedColumn
                     IF SUBSTR(2,2,sort4) = "A"
                        MaintSort(maintTable.SelectedColumn, HIGH(displayMArray), "D");
                        sort4 := INTTOSTR(maintTable.SelectedColumn)+"D";
                     ELSE
                        MaintSort(maintTable.SelectedColumn, HIGH(displayMArray), "A");
                        sort4 := INTTOSTR(maintTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     MaintSort(maintTable.SelectedColumn, HIGH(displayMArray), "A");
                     sort4 := INTTOSTR(maintTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 402
               IF ((maintFilter.Text() = "--------------------------------------") 
                   OR (maintFilter.Text() = oldMFilter))
                  ASK maintFilter TO SetText(oldMFilter);
               ELSE
                  HierFilter("maint", maintTable, MArray, HIGH(MArray), 7, maintFilter.Text(), FALSE, displayMArray, hiercount);
                  oldMFilter := maintFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 975 {Print Button}
               IF compileType <> "demo"
                  PrintInputTable(maintTable, "MaintDelays", "Print");
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 500:  {Costs Tab}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 501
               IF costTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,(STRLEN(sort5)-1),sort5)) = costTable.SelectedColumn
                     IF SUBSTR(STRLEN(sort5),STRLEN(sort5),sort5) = "A"
                        CostSort(costTable.SelectedColumn, HIGH(displayCArray), "D");
                        sort5 := INTTOSTR(costTable.SelectedColumn)+"D";
                     ELSE
                        CostSort(costTable.SelectedColumn, HIGH(displayCArray), "A");
                        sort5 := INTTOSTR(costTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     CostSort(costTable.SelectedColumn, HIGH(displayCArray), "A");
                     sort5 := INTTOSTR(costTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 502
               IF ((costFilter.Text() = "--------------------------------------") 
                   OR (costFilter.Text() = oldCFilter))
                  ASK costFilter TO SetText(oldCFilter);
               ELSE
                  HierFilter("cost", costTable, CArray, HIGH(CArray), 17, costFilter.Text(), FALSE, displayCArray, hiercount);
                  oldCFilter := costFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 976 {Print Button}
               IF compileType <> "demo"
                  PrintCostInput(costTable);
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 600:  {Dependency Tab}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 601
               IF depTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,(STRLEN(sort6)-1),sort6)) = depTable.SelectedColumn
                     IF SUBSTR(STRLEN(sort6),STRLEN(sort6),sort6) = "A"
                        DepSort(depTable.SelectedColumn, HIGH(displayDepArray), "D");
                        sort6 := INTTOSTR(depTable.SelectedColumn)+"D";
                     ELSE
                        DepSort(depTable.SelectedColumn, HIGH(displayDepArray), "A");
                        sort6 := INTTOSTR(depTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     DepSort(depTable.SelectedColumn, HIGH(displayDepArray), "A");
                     sort6 := INTTOSTR(depTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 602
               IF ((depFilter.Text() = "--------------------------------------") 
                   OR (depFilter.Text() = oldDFilter))
                  ASK depFilter TO SetText(oldDFilter);
               ELSE
                  HierFilter("dep", depTable, DepArray, HIGH(DepArray), 9, depFilter.Text(), FALSE, displayDepArray, hiercount);
                  oldDFilter := depFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 977 {Print Button}
               IF compileType <> "demo"
                  PrintInputTable(depTable, "Dependency", "Print");
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 700:  {Adv Tab}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 701
               IF advTable.SelectedRow = 0
                 IF STRTOINT(SUBSTR(1,(STRLEN(sort7)-1),sort7)) = advTable.SelectedColumn
                     IF SUBSTR(STRLEN(sort7),STRLEN(sort7),sort7) = "A"
                        AdvSort(advTable.SelectedColumn, HIGH(displayAArray), "D");
                        sort7 := INTTOSTR(advTable.SelectedColumn)+"D";
                     ELSE
                        AdvSort(advTable.SelectedColumn, HIGH(displayAArray), "A");
                        sort7 := INTTOSTR(advTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     AdvSort(advTable.SelectedColumn, HIGH(displayAArray), "A");
                     sort7 := INTTOSTR(advTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 702
               IF ((advFilter.Text() = "--------------------------------------") 
                   OR (advFilter.Text() = oldAFilter))
                  ASK advFilter TO SetText(oldAFilter);
               ELSE
                  HierFilter("adv", advTable, AArray, HIGH(AArray), 11, advFilter.Text(), FALSE, displayAArray, hiercount);
                  oldAFilter := advFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 978 {Print Button}
               IF compileType <> "demo"
                  PrintInputTable(advTable, "Advanced", "Print");
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 971:  {Help Button}
         OTHERWISE
      END CASE;
      Update;
      INHERITED BeSelected;  {Call the inherited method of a HelpBoxObj}
   END METHOD; {BeSelected}

   ASK METHOD ObjTerminate;
   BEGIN
      DISPOSE(translateArray);
      DISPOSE(values);
      DISPOSE(labels);  
      INHERITED ObjTerminate;
   END METHOD; {ObjTerminate}
   
   ASK METHOD ManageLabels(IN distro   : INTEGER;
                           IN failures : BOOLEAN);
   VAR
      i            : INTEGER;
      dist         : LabelObj;
      tags, units  : strArray;
      fOff         : realArray;
      labelType    : STRING;
   BEGIN
      NEW(tags, 1..3);
      NEW(fOff, 1..3);
      NEW(units, 1..3);
      IF failures
         GetParameters(distro, "failure", tags, units, fOff);
      ELSE
         GetParameters(distro, "repair", tags, units, fOff);
      END IF;
      IF failures
         dist := Descendant("failures", 0);
         FOR i := 1 TO 3
            ASK labels[i] TO SetLabel(tags[i]);
            ASK labels[i] TO DisplayAt(fOff[i]+3.5, 12.5+(FLOAT(i)*1.5));
            IF tags[i] = ""
               ASK labels[i] TO SetHidden(TRUE);
               ASK values[i] TO SetHidden(TRUE);
            ELSE   
               ASK labels[i] TO SetHidden(FALSE);
               ASK values[i] TO SetHidden(FALSE);
            END IF;
         END FOR;
         labelType := "Fail Distro: ";
      ELSE
         dist := Descendant("repairs", 0);
         FOR i := 1 TO 3
            ASK labels[i+3] TO SetLabel(tags[i]);
            ASK labels[i+3] TO DisplayAt(fOff[i]+37.5, 12.5+(FLOAT(i)*1.5));
            IF tags[i] = ""
               ASK labels[i+3] TO SetHidden(TRUE);
               ASK values[i+3] TO SetHidden(TRUE);
            ELSE   
               ASK labels[i+3] TO SetHidden(FALSE);
               ASK values[i+3] TO SetHidden(FALSE);
            END IF;
         END FOR;
         labelType := "Repair Distro: ";
      END IF;      
      ASK dist TO SetHidden(FALSE);
      CASE distro
         WHEN 1:  ASK dist TO SetLabel(labelType+"Beta");
         WHEN 2:  ASK dist TO SetLabel(labelType+"Chi Square");
         WHEN 3:  ASK dist TO SetLabel(labelType+"Binomial");
         WHEN 4:  ASK dist TO SetLabel(labelType+"Exponential");
         WHEN 5:  ASK dist TO SetLabel(labelType+"Erlang");
         WHEN 6:  ASK dist TO SetLabel(labelType+"Gamma");
         WHEN 7:  ASK dist TO SetLabel(labelType+"Lognormal");
         WHEN 8:  ASK dist TO SetLabel(labelType+"Normal");
         WHEN 9:  ASK dist TO SetLabel(labelType+"Uniform Int");
         WHEN 10: ASK dist TO SetLabel(labelType+"Uniform Real");
         WHEN 11: ASK dist TO SetLabel(labelType+"Pearson 5");
         WHEN 12: ASK dist TO SetLabel(labelType+"Pearson 6");
         WHEN 13: ASK dist TO SetLabel(labelType+"Poisson");
         WHEN 14: ASK dist TO SetLabel(labelType+"Triangular");
         WHEN 15: ASK dist TO SetLabel(labelType+"Weibull");
         WHEN 16: ASK dist TO SetLabel(labelType+"Empirical");
         WHEN 17: ASK dist TO SetLabel(labelType+"Point Est");
         WHEN 18: ASK dist TO SetLabel(labelType+"None");
         WHEN 19: ASK dist TO SetLabel(labelType+"Fixed");
         WHEN 20: ASK dist TO SetLabel(labelType+"Exponential Power");
         WHEN 21: ASK dist TO SetLabel(labelType+"Extreme Value");
         WHEN 22: ASK dist TO SetLabel(labelType+"Laplace");
         OTHERWISE
      END CASE;
      DISPOSE(tags);
      DISPOSE(fOff);
      DISPOSE(units);
      Update;
   END METHOD; {ManageLabels}
   
   ASK METHOD DistSort(IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
   VAR
      numPlaced,i,j,pos              : INTEGER;
      cell,currentStr,blockSparing : STRING;
      block                          : RBDBlockObj;
      currentNum, cellNum            : REAL;
   BEGIN
      pos := 1;
      numPlaced := 1;
      FOR i := 1 TO tableSize
         CASE sortCol
            WHEN 1:
               currentStr := LOWER(displayDArray[i,1])
            WHEN 2:
               currentStr := LOWER(displayDArray[i,2])
            WHEN 3:
               currentNum := STRTOREAL(displayDArray[i,3]);
            WHEN 4:
               currentNum := STRTOREAL(displayDArray[i,4]);
            WHEN 5:
               currentNum := STRTOREAL(displayDArray[i,5]);
            WHEN 6:
               currentStr := LOWER(displayDArray[i,6])
            WHEN 7:
               currentNum := STRTOREAL(displayDArray[i,7]);
            WHEN 8:                          
               currentNum := STRTOREAL(displayDArray[i,8]);
            WHEN 9:
               currentNum := STRTOREAL(displayDArray[i,9]);
            OTHERWISE            
         END CASE;
         IF (sortCol=1) OR (sortCol=2) OR (sortCol=6)
            cell := LOWER(distroTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                  DistInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(distroTable.Text(sortCol, pos));
                  WHILE(currentStr > cell)
                     INC(pos);
                     cell := LOWER(distroTable.Text(sortCol, pos));
                  END WHILE;
                  DistInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  DistInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(distroTable.Text(sortCol, pos));
                  WHILE(currentStr < cell)
                     INC(pos);
                     cell := LOWER(distroTable.Text(sortCol, pos));
                  END WHILE;
                  DistInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            IF (ASK distroTable Text(sortCol, numPlaced-1) = "") OR (numPlaced = 1)
               cellNum := -1.;
            ELSE
               cellNum := STRTOREAL(ASK distroTable Text(sortCol, numPlaced-1));
            END IF;
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  DistInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF (ASK distroTable Text(sortCol, pos)) = ""
                     cellNum := -1.;
                  ELSE
                     cellNum := STRTOREAL(ASK distroTable Text(sortCol, pos));
                  END IF;
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     IF (ASK distroTable Text(sortCol, pos)) = ""
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(ASK distroTable Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  DistInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  DistInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
               pos := 1;
                  IF (ASK distroTable Text(sortCol, pos)) = ""
                     cellNum := -1.;
                  ELSE
                     cellNum := STRTOREAL(ASK distroTable Text(sortCol, pos));
                  END IF;
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     IF (ASK distroTable Text(sortCol, pos)) = ""
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(ASK distroTable Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  DistInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {DistSort}
            
   ASK METHOD DistInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j,n, temp               : INTEGER;
      text2             : STRING;
      pool              : SparePoolObj;
      block             : RBDBlockObj;
      logMean2,logVar   : REAL;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK distroTable Text(1, j-1);
         ASK distroTable TO SetText(text2, 1, j);
         text2 := ASK distroTable Text(2, j-1);
         ASK distroTable TO SetText(text2, 2, j);
         text2 := ASK distroTable Text(3, j-1);
         ASK distroTable TO SetText(text2, 3, j);
         text2 := ASK distroTable Text(4, j-1);
         ASK distroTable TO SetText(text2, 4, j);
         text2 := ASK distroTable Text(5, j-1);
         ASK distroTable TO SetText(text2, 5, j); 
         text2 := ASK distroTable Text(6, j-1);
         ASK distroTable TO SetText(text2, 6, j);
         text2 := ASK distroTable Text(7, j-1);
         ASK distroTable TO SetText(text2, 7, j);
         text2 := ASK distroTable Text(8, j-1);
         ASK distroTable TO SetText(text2, 8, j);
         text2 := ASK distroTable Text(9, j-1);
         ASK distroTable TO SetText(text2, 9, j);
         translateArray[j] := translateArray[j-1];
      END FOR;   
      translateArray[pos] := STRTOINT(displayDArray[i,11]);
      ASK distroTable TO SetText(displayDArray[i,1], 1, pos);
      ASK distroTable TO SetText(displayDArray[i,2], 2, pos);                       
      ASK distroTable TO SetText(displayDArray[i,3], 3, pos);                       
      ASK distroTable TO SetText(displayDArray[i,4], 4, pos);                       
      ASK distroTable TO SetText(displayDArray[i,5], 5, pos);                       
      ASK distroTable TO SetText(displayDArray[i,6], 6, pos);                       
      ASK distroTable TO SetText(displayDArray[i,7], 7, pos);                       
      ASK distroTable TO SetText(displayDArray[i,8], 8, pos);                       
      ASK distroTable TO SetText(displayDArray[i,9], 9, pos); 
   END METHOD;{DistInsert}

   ASK METHOD RepSort(IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
   VAR
      numPlaced,i,j,pos              : INTEGER;
      cell,currentStr,blockSparing : STRING;
      block                          : RBDBlockObj;
      currentNum, cellNum            : REAL;
      pool                           : SparePoolObj;
   BEGIN
      pos := 1;
      numPlaced := 1;
      FOR i := 1 TO tableSize
         CASE sortCol
            WHEN 1:
               currentStr := LOWER(displaySArray[i,1])
            WHEN 2:
               currentStr := LOWER(displaySArray[i,2])
            WHEN 3:
               IF (displaySArray[i,3] = "N/A")
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displaySArray[i,3]);
               END IF;
            WHEN 4:
               IF (displaySArray[i,4] = "N/A")
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displaySArray[i,4]);
               END IF;
            WHEN 5:
               IF (displaySArray[i,5] = "N/A")
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displaySArray[i,5]);
               END IF;
            WHEN 6:
               IF (displaySArray[i,6] = "N/A")
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displaySArray[i,6]);
               END IF;
            WHEN 7:
               IF (displaySArray[i,7] = "N/A")
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displaySArray[i,7]);
               END IF;
            WHEN 8:
               IF (displaySArray[i,8] = "N/A")
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displaySArray[i,8]);
               END IF;
            WHEN 9:
               IF (displaySArray[i,9] = "N/A")
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displaySArray[i,9]);
               END IF;
            OTHERWISE            
         END CASE;
         IF (sortCol=1) OR (sortCol=2)
            cell := LOWER(sparesTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                   RepInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(sparesTable.Text(sortCol, pos));
                  WHILE(currentStr > cell)
                     INC(pos);
                     cell := LOWER(sparesTable.Text(sortCol, pos));
                  END WHILE;
                  RepInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  RepInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(sparesTable.Text(sortCol, pos));
                  WHILE(currentStr < cell)
                     INC(pos);
                     cell := LOWER(sparesTable.Text(sortCol, pos));
                  END WHILE;
                  RepInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            IF (numPlaced = 1) OR (ASK sparesTable Text(sortCol, numPlaced-1) = "N/A")
               cellNum := -1.;
            ELSE
               cellNum := STRTOREAL(ASK sparesTable Text(sortCol, numPlaced-1));
            END IF;
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  RepInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF ASK sparesTable Text(sortCol, pos) = "N/A"
                     cellNum := -1.;
                  ELSE
                     cellNum := STRTOREAL(ASK sparesTable Text(sortCol, pos));
                  END IF;
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     IF ASK sparesTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(ASK sparesTable Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  RepInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  RepInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF ASK sparesTable Text(sortCol, pos) = "N/A"
                     cellNum := -1.;
                  ELSE
                     cellNum := STRTOREAL(ASK sparesTable Text(sortCol, pos));
                  END IF;
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     IF ASK sparesTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(ASK sparesTable Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  RepInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {RepSort}
            
   ASK METHOD RepInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j     : INTEGER;
      text2 : STRING;
      pool  : SparePoolObj;
      block : RBDBlockObj;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK sparesTable Text(1, j-1);
         ASK sparesTable TO SetText(text2, 1, j);
         text2 := ASK sparesTable Text(2, j-1);
         ASK sparesTable TO SetText(text2, 2, j);
         text2 := ASK sparesTable Text(3, j-1);
         ASK sparesTable TO SetText(text2, 3, j);
         text2 := ASK sparesTable Text(4, j-1);
         ASK sparesTable TO SetText(text2, 4, j);
         text2 := ASK sparesTable Text(5, j-1);
         ASK sparesTable TO SetText(text2, 5, j);
         text2 := ASK sparesTable Text(6, j-1);
         ASK sparesTable TO SetText(text2, 6, j);
         text2 := ASK sparesTable Text(7, j-1);
         ASK sparesTable TO SetText(text2, 7, j);
         text2 := ASK sparesTable Text(8, j-1);
         ASK sparesTable TO SetText(text2, 8, j);
         text2 := ASK sparesTable Text(9, j-1);
         ASK sparesTable TO SetText(text2, 9, j);
      END FOR;         
      ASK sparesTable TO SetText(displaySArray[i,1], 1, pos);                       
      ASK sparesTable TO SetText(displaySArray[i,2], 2, pos);                       
      ASK sparesTable TO SetText(displaySArray[i,3], 3, pos);                       
      ASK sparesTable TO SetText(displaySArray[i,4], 4, pos);                       
      ASK sparesTable TO SetText(displaySArray[i,5], 5, pos);                       
      ASK sparesTable TO SetText(displaySArray[i,6], 6, pos);                       
      ASK sparesTable TO SetText(displaySArray[i,7], 7, pos);                       
      ASK sparesTable TO SetText(displaySArray[i,8], 8, pos);                       
      ASK sparesTable TO SetText(displaySArray[i,9], 9, pos);                       
   END METHOD;{RepInsert, BlockInputBoxObj}
   
   ASK METHOD PMSort(IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
   VAR
      numPlaced,i, j, pos            : INTEGER;
      cell,currentStr,blockSparing, tempString : STRING;
      block                          : RBDBlockObj;
      pool                           : SparePoolObj;
      currentNum, cellNum            : REAL;
   BEGIN
      pos := 1;
      numPlaced:=1;
      FOR i := 1 TO tableSize
         CASE sortCol
            WHEN 1:
               currentStr := LOWER(displayPMArray[i,1])
            WHEN 2:
               currentNum := STRTOREAL(displayPMArray[i,2]);
            WHEN 3:
               currentStr := LOWER(displayPMArray[i,3])
               GetDistMean(currentStr, currentNum);
            WHEN 4:
               currentNum := STRTOREAL(displayPMArray[i,4]);
            WHEN 5:
               currentStr := LOWER(displayPMArray[i,5])
            WHEN 6:
               currentStr := LOWER(displayPMArray[i,6])
            WHEN 7:
               currentStr := LOWER(displayPMArray[i,7])
            WHEN 8:
               currentStr := LOWER(displayPMArray[i,8])
            WHEN 9:
               currentStr := LOWER(displayPMArray[i,9])
            OTHERWISE
         END CASE;
         IF (sortCol = 1) {OR (sortCol = 3)} OR (sortCol = 5) OR (sortCol = 6) 
             OR (sortCol = 7) OR (sortCol = 8) OR (sortCol = 9)
            cell := LOWER(pmTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                  PMInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(pmTable.Text(sortCol, pos));
                  WHILE(currentStr > cell)
                     INC(pos);
                     cell := LOWER(pmTable.Text(sortCol, pos));
                  END WHILE;
                  PMInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  PMInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(pmTable.Text(sortCol, pos));
                  WHILE(currentStr < cell)
                     INC(pos);
                     cell := LOWER(pmTable.Text(sortCol, pos));
                  END WHILE;
                  PMInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSIF (sortCol = 3) {PM duration field (sort by distribution mean)}
            tempString := pmTable.Text(sortCol, numPlaced-1);
            GetDistMean(tempString, cellNum);
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  PMInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  tempString := pmTable.Text(sortCol, pos);
                  GetDistMean(tempString, cellNum);
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     tempString := pmTable.Text(sortCol, pos);
                     GetDistMean(tempString, cellNum);
                  END WHILE;
                  PMInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  PMInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  tempString := pmTable.Text(sortCol, pos);
                  GetDistMean(tempString, cellNum);
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     tempString := pmTable.Text(sortCol, pos);
                     GetDistMean(tempString, cellNum);
                  END WHILE;
                  PMInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            cellNum := STRTOREAL(pmTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  PMInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(pmTable.Text(sortCol, pos));
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(pmTable.Text(sortCol, pos));
                  END WHILE;
                  PMInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  PMInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(pmTable.Text(sortCol, pos));
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(pmTable.Text(sortCol, pos));
                  END WHILE;
                  PMInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {PMSort}
            
   ASK METHOD PMInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j     : INTEGER;
      text2, tempString, stagger : STRING;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK pmTable Text(1, j-1);
         ASK pmTable TO SetText(text2, 1, j);
         text2 := ASK pmTable Text(2, j-1);
         ASK pmTable TO SetText(text2, 2, j);
         text2 := ASK pmTable Text(3, j-1);
         ASK pmTable TO SetText(text2, 3, j);
         text2 := ASK pmTable Text(4, j-1);
         ASK pmTable TO SetText(text2, 4, j);
         text2 := ASK pmTable Text(5, j-1);
         ASK pmTable TO SetText(text2, 5, j);
         text2 := ASK pmTable Text(6, j-1);
         ASK pmTable TO SetText(text2, 6, j);
         text2 := ASK pmTable Text(7, j-1);
         ASK pmTable TO SetText(text2, 7, j);
         text2 := ASK pmTable Text(8, j-1);
         ASK pmTable TO SetText(text2, 8, j);
         text2 := ASK pmTable Text(9, j-1);
         ASK pmTable TO SetText(text2, 9, j);
      END FOR;         
      ASK pmTable TO SetText(displayPMArray[i,1], 1, pos);                       
      ASK pmTable TO SetText(displayPMArray[i,2], 2, pos);                       
      ASK pmTable TO SetText(displayPMArray[i,3], 3, pos);                       
      ASK pmTable TO SetText(displayPMArray[i,4], 4, pos);                       
      ASK pmTable TO SetText(displayPMArray[i,5], 5, pos);                       
      ASK pmTable TO SetText(displayPMArray[i,6], 6, pos);                       
      ASK pmTable TO SetText(displayPMArray[i,7], 7, pos);                       
      ASK pmTable TO SetText(displayPMArray[i,8], 8, pos);                       
      ASK pmTable TO SetText(displayPMArray[i,9], 9, pos);                       
   END METHOD;{PMInsert}

   ASK METHOD MaintSort(IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
   VAR
      numPlaced,i, j, pos            : INTEGER;
      cell,currentStr,blockSparing, tempString : STRING;
      block                          : RBDBlockObj;
      pool                           : SparePoolObj;
      currentBool                    : BOOLEAN;
      currentNum, cellNum : REAL;
   BEGIN
      pos := 1;
      numPlaced:=1;
      FOR i := 1 TO tableSize
         CASE sortCol
            WHEN 1:
               currentStr := LOWER(displayMArray[i,1]);
            WHEN 2:
               currentStr := LOWER(displayMArray[i,2]);
               GetDistMean(currentStr, currentNum);
            WHEN 3:
               currentStr := LOWER(displayMArray[i,3]);
               GetDistMean(currentStr, currentNum);               
            WHEN 4:
               currentStr := LOWER(displayMArray[i,4]);
            WHEN 5:
               currentNum := STRTOREAL(displayMArray[i,5]);
            WHEN 6:
               currentNum := STRTOREAL(displayMArray[i,6]);
            OTHERWISE
         END CASE;
         IF (sortCol = 1) {OR (sortCol = 2) OR (sortCol = 3)} OR (sortCol = 4)
            cell := LOWER(maintTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                  MaintInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(maintTable.Text(sortCol, pos));
                  WHILE(currentStr > cell)
                     INC(pos);
                     cell := LOWER(maintTable.Text(sortCol, pos));
                  END WHILE;
                  MaintInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  MaintInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(maintTable.Text(sortCol, pos));
                  WHILE(currentStr < cell)
                     INC(pos);
                     cell := LOWER(maintTable.Text(sortCol, pos));
                  END WHILE;
                  MaintInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSIF (sortCol = 2) OR (sortCol = 3) {Pre and Post logistics delay time (sort distro by mean)}
            tempString := maintTable.Text(sortCol, numPlaced-1);
            GetDistMean(tempString, cellNum);
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  MaintInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  tempString := maintTable.Text(sortCol, pos);
                  GetDistMean(tempString, cellNum);
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     tempString := maintTable.Text(sortCol, pos);
                     GetDistMean(tempString, cellNum);
                  END WHILE;
                  MaintInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  MaintInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  tempString := maintTable.Text(sortCol, pos);
                  GetDistMean(tempString, cellNum);
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     tempString := maintTable.Text(sortCol, pos);
                     GetDistMean(tempString, cellNum);
                  END WHILE;
                  MaintInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSIF (sortCol = 5) OR (sortCol = 6) 
            cellNum := STRTOREAL(maintTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  MaintInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(maintTable.Text(sortCol, pos));
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(maintTable.Text(sortCol, pos));
                  END WHILE;
                  MaintInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  MaintInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(maintTable.Text(sortCol, pos));
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(maintTable.Text(sortCol, pos));
                  END WHILE;
                  MaintInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {MaintSort}
            
   ASK METHOD MaintInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j     : INTEGER;
      text2, tempString, stagger : STRING;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK maintTable Text(1, j-1);
         ASK maintTable TO SetText(text2, 1, j);
         text2 := ASK maintTable Text(2, j-1);
         ASK maintTable TO SetText(text2, 2, j);
         text2 := ASK maintTable Text(3, j-1);
         ASK maintTable TO SetText(text2, 3, j);
         text2 := ASK maintTable Text(4, j-1);
         ASK maintTable TO SetText(text2, 4, j);
         text2 := ASK maintTable Text(5, j-1);
         ASK maintTable TO SetText(text2, 5, j);
         text2 := ASK maintTable Text(6, j-1);
         ASK maintTable TO SetText(text2, 6, j);
      END FOR;         
      ASK maintTable TO SetText(displayMArray[i,1], 1, pos);                       
      ASK maintTable TO SetText(displayMArray[i,2], 2, pos);                       
      ASK maintTable TO SetText(displayMArray[i,3], 3, pos);                       
      ASK maintTable TO SetText(displayMArray[i,4], 4, pos);                       
      ASK maintTable TO SetText(displayMArray[i,5], 5, pos);                       
      ASK maintTable TO SetText(displayMArray[i,6], 6, pos);                       
   END METHOD;{MaintInsert}

   ASK METHOD CostSort(IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
   VAR
      numPlaced,i, j, pos, test            : INTEGER;
      cell,currentStr,blockSparing : STRING;
      block                          : RBDBlockObj;
      pool                           : SparePoolObj;
      currentNum, cellNum            : REAL;
   BEGIN
      pos := 1;
      numPlaced:=1;
      FOR i := 1 TO tableSize
         CASE sortCol
            WHEN 0:
               currentStr := LOWER(displayCArray[i,1])
            WHEN 1:
               currentNum := COSTTOREAL(displayCArray[i,2]);
            WHEN 2:
               currentNum := COSTTOREAL(displayCArray[i,3]);
            WHEN 3:
               currentNum := COSTTOREAL(displayCArray[i,4]);
            WHEN 4:
               currentNum := COSTTOREAL(displayCArray[i,5]);
            WHEN 5:
               currentNum := COSTTOREAL(displayCArray[i,6]);
            WHEN 6:
               currentNum := COSTTOREAL(displayCArray[i,7]);
            WHEN 7:
               currentNum := COSTTOREAL(displayCArray[i,8]);
            WHEN 8:
               currentNum := COSTTOREAL(displayCArray[i,9]);
            WHEN 9:
               currentNum := COSTTOREAL(displayCArray[i,10]);
            WHEN 10:
               currentNum := COSTTOREAL(displayCArray[i,11]);
            WHEN 11:
               currentNum := COSTTOREAL(displayCArray[i,12]);
            WHEN 12:
               currentNum := COSTTOREAL(displayCArray[i,13]);
            WHEN 13:
               currentNum := COSTTOREAL(displayCArray[i,14]);
            WHEN 14:
               currentNum := COSTTOREAL(displayCArray[i,15]);
            WHEN 15:
               currentStr := LOWER(displayCArray[i,16])
            OTHERWISE
         END CASE;
         IF (sortCol=0) OR (sortCol=15)
            cell := LOWER(costTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                  CostInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(costTable.Text(sortCol, pos));
                  WHILE(currentStr > cell)
                     INC(pos);
                     cell := LOWER(costTable.Text(sortCol, pos));
                  END WHILE;
                  CostInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  CostInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(costTable.Text(sortCol, pos));
                  WHILE(currentStr < cell)
                     INC(pos);
                     cell := LOWER(costTable.Text(sortCol, pos));
                  END WHILE;
                  CostInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            cellNum := COSTTOREAL(costTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  CostInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := COSTTOREAL(costTable.Text(sortCol, pos));
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     cellNum := COSTTOREAL(costTable.Text(sortCol, pos));
                  END WHILE;
                  CostInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  CostInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := COSTTOREAL(costTable.Text(sortCol, pos));
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     cellNum := COSTTOREAL(costTable.Text(sortCol, pos));
                  END WHILE;
                  CostInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {CostSort}
            
   ASK METHOD CostInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j     : INTEGER;
      text2 : STRING;
      pool  : SparePoolObj;
      block : RBDBlockObj;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK costTable Text(0, j-1);
         ASK costTable TO SetText(text2, 0, j);
         text2 := ASK costTable Text(1, j-1);
         ASK costTable TO SetText(text2, 1, j);
         text2 := ASK costTable Text(2, j-1);
         ASK costTable TO SetText(text2, 2, j);
         text2 := ASK costTable Text(3, j-1);
         ASK costTable TO SetText(text2, 3, j);
         text2 := ASK costTable Text(4, j-1);
         ASK costTable TO SetText(text2, 4, j);
         text2 := ASK costTable Text(5, j-1);
         ASK costTable TO SetText(text2, 5, j);
         text2 := ASK costTable Text(6, j-1);
         ASK costTable TO SetText(text2, 6, j);
         text2 := ASK costTable Text(7, j-1);
         ASK costTable TO SetText(text2, 7, j);
         text2 := ASK costTable Text(8, j-1);
         ASK costTable TO SetText(text2, 8, j);
         text2 := ASK costTable Text(9, j-1);
         ASK costTable TO SetText(text2, 9, j);
         text2 := ASK costTable Text(10, j-1);
         ASK costTable TO SetText(text2, 10, j);
         text2 := ASK costTable Text(11, j-1);
         ASK costTable TO SetText(text2, 11, j);
         text2 := ASK costTable Text(12, j-1);
         ASK costTable TO SetText(text2, 12, j);
         text2 := ASK costTable Text(13, j-1);
         ASK costTable TO SetText(text2, 13, j);
         text2 := ASK costTable Text(14, j-1);
         ASK costTable TO SetText(text2, 14, j);
         text2 := ASK costTable Text(15, j-1);
         ASK costTable TO SetText(text2, 15, j);
      END FOR;         
      ASK costTable TO SetText(displayCArray[i,1], 0, pos); 
      ASK costTable TO SetText(displayCArray[i,2], 1, pos);                       
      ASK costTable TO SetText(displayCArray[i,3], 2, pos);                       
      ASK costTable TO SetText(displayCArray[i,4], 3, pos);                       
      ASK costTable TO SetText(displayCArray[i,5], 4, pos);                       
      ASK costTable TO SetText(displayCArray[i,6], 5, pos);                       
      ASK costTable TO SetText(displayCArray[i,7], 6, pos);                       
      ASK costTable TO SetText(displayCArray[i,8], 7, pos);                       
      ASK costTable TO SetText(displayCArray[i,9], 8, pos);                       
      ASK costTable TO SetText(displayCArray[i,10], 9, pos);                       
      ASK costTable TO SetText(displayCArray[i,11], 10, pos);                       
      ASK costTable TO SetText(displayCArray[i,12], 11, pos);                       
      ASK costTable TO SetText(displayCArray[i,13], 12, pos);                       
      ASK costTable TO SetText(displayCArray[i,14], 13, pos);                       
      ASK costTable TO SetText(displayCArray[i,15], 14, pos);                       
      ASK costTable TO SetText(displayCArray[i,16], 15, pos);                       
   END METHOD;{CostInsert}

   ASK METHOD DepSort(IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
   VAR
      numPlaced,i, j, pos           : INTEGER;
      cell,currentStr,blockSparing, tempString : STRING;
      block, tempBlock                          : RBDBlockObj;
      tempNode : RBDNodeObj;
      tempHier : RBDHierObj;
      pool                           : SparePoolObj;
      currentBool                    : BOOLEAN;
      currentNum, cellNum : REAL;
   BEGIN
      pos := 1;
      numPlaced:=1;
      FOR i := 1 TO tableSize
         CASE sortCol
            WHEN 1:
               currentStr := LOWER(displayDepArray[i,1])
            WHEN 2:
               currentStr := LOWER(displayDepArray[i,2])
            WHEN 3:
               IF (displayDepArray[i,3] = "N/A")
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displayDepArray[i,3]);
               END IF;
            WHEN 4:
               IF (displayDepArray[i,4] = "N/A")
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displayDepArray[i,4]);
               END IF;
            WHEN 5:
               IF (displayDepArray[i,5] = "N/A")
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displayDepArray[i,5]);
               END IF;
            WHEN 6:
               IF (displayDepArray[i,6] = "N/A")
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displayDepArray[i,6]);
               END IF;
            WHEN 7:
               IF (displayDepArray[i,7] = "N/A")
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displayDepArray[i,7]);
               END IF;
            WHEN 8:
               currentStr := LOWER(displayDepArray[i,8])
            OTHERWISE
         END CASE;
         IF (sortCol = 1) OR (sortCol = 2) OR (sortCol = 8)
            cell := LOWER(depTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                  DepInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(depTable.Text(sortCol, pos));
                  WHILE(currentStr > cell)
                     INC(pos);
                     cell := LOWER(depTable.Text(sortCol, pos));
                  END WHILE;
                  DepInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  DepInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(depTable.Text(sortCol, pos));
                  WHILE(currentStr < cell)
                     INC(pos);
                     cell := LOWER(depTable.Text(sortCol, pos));
                  END WHILE;
                  DepInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSIF (sortCol = 3) OR (sortCol = 4) OR (sortCol = 5) OR (sortCol = 6) OR (sortCol = 7)
            IF (numPlaced = 1) OR (ASK depTable Text(sortCol, numPlaced-1) = "N/A")
               cellNum := -1.;
            ELSE
               cellNum := STRTOREAL(depTable.Text(sortCol, numPlaced-1));
            END IF;
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  DepInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF ASK depTable Text(sortCol, pos) = "N/A"
                     cellNum := -1.;
                  ELSE
                     cellNum := STRTOREAL(depTable.Text(sortCol, pos));
                  END IF;
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     IF ASK depTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(depTable.Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  DepInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  DepInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF ASK depTable Text(sortCol, pos) = "N/A"
                     cellNum := -1.;
                  ELSE
                     cellNum := STRTOREAL(depTable.Text(sortCol, pos));
                  END IF;
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     IF ASK depTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(depTable.Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  DepInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {DepSort}
            
   ASK METHOD DepInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j     : INTEGER;
      text2, tempString, stagger : STRING;
      block, tempBlock : RBDBlockObj;
      tempNode : RBDNodeObj;
      tempHier : RBDHierObj;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK depTable Text(1, j-1);
         ASK depTable TO SetText(text2, 1, j);
         text2 := ASK depTable Text(2, j-1);
         ASK depTable TO SetText(text2, 2, j);
         text2 := ASK depTable Text(3, j-1);
         ASK depTable TO SetText(text2, 3, j);
         text2 := ASK depTable Text(4, j-1);
         ASK depTable TO SetText(text2, 4, j);
         text2 := ASK depTable Text(5, j-1);
         ASK depTable TO SetText(text2, 5, j);
         text2 := ASK depTable Text(6, j-1);
         ASK depTable TO SetText(text2, 6, j);
         text2 := ASK depTable Text(7, j-1);
         ASK depTable TO SetText(text2, 7, j);
         text2 := ASK depTable Text(8, j-1);
         ASK depTable TO SetText(text2, 8, j);
      END FOR;         
      ASK depTable TO SetText(displayDepArray[i,1], 1, pos);                       
      ASK depTable TO SetText(displayDepArray[i,2], 2, pos);                       
      ASK depTable TO SetText(displayDepArray[i,3], 3, pos);                       
      ASK depTable TO SetText(displayDepArray[i,4], 4, pos);                       
      ASK depTable TO SetText(displayDepArray[i,5], 5, pos);                       
      ASK depTable TO SetText(displayDepArray[i,6], 6, pos);                       
      ASK depTable TO SetText(displayDepArray[i,7], 7, pos);                       
      ASK depTable TO SetText(displayDepArray[i,8], 8, pos); 
   END METHOD;{DepInsert} 

   ASK METHOD AdvSort(IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
   VAR
      numPlaced,i,j,pos              : INTEGER;
      cell,currentStr,blockSparing : STRING;
      block                          : RBDBlockObj;
      currentNum, cellNum            : REAL;
   BEGIN
      pos := 1;
      numPlaced := 1;
      FOR i := 1 TO tableSize
         CASE sortCol
            WHEN 1:
               currentStr := LOWER(displayAArray[i,1])
            WHEN 2:
               currentStr := LOWER(displayAArray[i,2])
            WHEN 3:
               currentStr := LOWER(displayAArray[i,3])
            WHEN 4:
               currentStr := LOWER(displayAArray[i,4])
            WHEN 5:
               currentStr := LOWER(displayAArray[i,5])
            WHEN 6:
               currentStr := LOWER(displayAArray[i,6])
            WHEN 7:
               currentStr := LOWER(displayAArray[i,7])
            WHEN 8:
               currentNum := STRTOREAL(displayAArray[i,8]);
            WHEN 9:
               currentNum := STRTOREAL(displayAArray[i,9]);
            WHEN 10:
               currentNum := STRTOREAL(displayAArray[i,10]);
            OTHERWISE            
         END CASE;
         IF (sortCol=1) OR (sortCol=2) OR (sortCol=3) OR (sortCol=4)
            OR (sortCol=5) OR (sortCol=6) OR (sortCol=7)
            cell := LOWER(advTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                  AdvInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(advTable.Text(sortCol, pos));
                  WHILE(currentStr > cell)
                     INC(pos);
                     cell := LOWER(advTable.Text(sortCol, pos));
                  END WHILE;
                  AdvInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  AdvInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(advTable.Text(sortCol, pos));
                  WHILE(currentStr < cell)
                     INC(pos);
                     cell := LOWER(advTable.Text(sortCol, pos));
                  END WHILE;
                  AdvInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            cellNum := STRTOREAL(ASK advTable Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  AdvInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK advTable Text(sortCol, pos));
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK advTable Text(sortCol, pos));
                  END WHILE;
                  AdvInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  AdvInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK advTable Text(sortCol, pos));
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK advTable Text(sortCol, pos));
                  END WHILE;
                  AdvInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {AdvSort}
            
   ASK METHOD AdvInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j             : INTEGER;
      text2         : STRING;
      pool          : SparePoolObj;
      block         : RBDBlockObj;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK advTable Text(1, j-1);
         ASK advTable TO SetText(text2, 1, j);
         text2 := ASK advTable Text(2, j-1);
         ASK advTable TO SetText(text2, 2, j);
         text2 := ASK advTable Text(3, j-1);
         ASK advTable TO SetText(text2, 3, j);
         text2 := ASK advTable Text(4, j-1);
         ASK advTable TO SetText(text2, 4, j);
         text2 := ASK advTable Text(5, j-1);
         ASK advTable TO SetText(text2, 5, j);
         text2 := ASK advTable Text(6, j-1);
         ASK advTable TO SetText(text2, 6, j);
         text2 := ASK advTable Text(7, j-1);
         ASK advTable TO SetText(text2, 7, j);
         text2 := ASK advTable Text(8, j-1);
         ASK advTable TO SetText(text2, 8, j);
         text2 := ASK advTable Text(9, j-1);
         ASK advTable TO SetText(text2, 9, j);
         text2 := ASK advTable Text(10, j-1);
         ASK advTable TO SetText(text2, 10, j);
      END FOR;         
      ASK advTable TO SetText(displayAArray[i,1], 1, pos);                       
      ASK advTable TO SetText(displayAArray[i,2], 2, pos);                       
      ASK advTable TO SetText(displayAArray[i,3], 3, pos);                       
      ASK advTable TO SetText(displayAArray[i,4], 4, pos);                       
      ASK advTable TO SetText(displayAArray[i,5], 5, pos);                       
      ASK advTable TO SetText(displayAArray[i,6], 6, pos);                       
      ASK advTable TO SetText(displayAArray[i,7], 7, pos);                       
      ASK advTable TO SetText(displayAArray[i,8], 8, pos);                       
      ASK advTable TO SetText(displayAArray[i,9], 9, pos);                       
      ASK advTable TO SetText(displayAArray[i,10], 10, pos);                       
   END METHOD;{AdvInsert}
   

END OBJECT; {BlockInputBoxObj}

OBJECT NodeInputBoxObj;
   ASK METHOD InitSelf(IN printing : BOOLEAN);
   VAR
      i,j, numPM, hiercount  : INTEGER;
      block, tempBlock, tempBlk         : RBDBlockObj;
      tempEvent : RBDEventObj;
      tempLink               : LinkObj;
      tempNode, tempNode2, tempNd       : RBDNodeObj;
      tempHier               : RBDHierObj;
      hierList               : OptionListType;
      fromS, toS             : STRING;
      capLinkFound           : BOOLEAN;                       
   BEGIN
      genTab            := Child("GenTab", 100);
      genTable          := Descendant("GenTable", 101);
      genFilter         := Descendant("GenFilter", 102);
      gFilterText       := Descendant("GFilterText", 103);
      standbyTab        := Child("StandbyTab", 200);
      standbyTable      := Descendant("StandbyTable", 201);
      standbyFilter     := Descendant("StandbyFilter", 202);
      sFilterText       := Descendant("SFilterText", 203);
      capTab            := Child("CapTab", 300);
      capInTable        := Descendant("CapInTable", 301);
      capFilter         := Descendant("CapFilter", 302);
      cFilterText       := Descendant("CFilterText", 303);
      oldGFilter := "All";
      oldSFilter := "All";
      oldCFilter := "All";
      IF NOT printing
         Draw;
         IF totalHiers > 0
            NEW(hierList, 1..totalHiers+3);
            InitHierFilter(hierList);
            ASK genFilter TO SetOptions(hierList);
            ASK genFilter TO SetText(hierList[1]);
            ASK standbyFilter TO SetOptions(hierList);
            ASK standbyFilter TO SetText(hierList[1]);
            ASK capFilter TO SetOptions(hierList);
            ASK capFilter TO SetText(hierList[1]);
         ELSE
            ASK genFilter TO SetText("Home");
            ASK genFilter TO Deactivate;
            ASK gFilterText TO Deactivate;
            ASK standbyFilter TO SetText("Home");
            ASK standbyFilter TO Deactivate;
            ASK sFilterText TO Deactivate;
            ASK capFilter TO SetText("Home");
            ASK capFilter TO Deactivate;
            ASK cFilterText TO Deactivate;
         END IF;
      END IF;
      IF totalLinks > 0
         NEW(coldArray, 1..totalLinks);
         NEW(coldArray2, 1..totalLinks);
         NEW(capArray, 1..totalLinks);
         NEW(capArray2, 1..totalLinks);
         FOREACH tempLink IN linkGroup                           
            capLinkFound:=FALSE;
            IF tempLink.connectTRef = "RBDNode"
               tempNode := ASK root Child("RBDNode",tempLink.connectToId);
               IF tempNode.coldStandby
                  INC(coldLinks);
                  coldArray2[coldLinks] := tempLink.Id;
               END IF;
               IF (NOT tempNode.fullFlow)
                  INC(capLinks);
                  capLinkFound:=TRUE;
                  capArray2[capLinks] := tempLink.Id;
               END IF;               
            END IF;
            IF NOT capLinkFound
               IF tempLink.connectFRef = "RBDNode"
                  tempNode := ASK root Child("RBDNode",tempLink.connectFromId);
                  IF (NOT tempNode.anyPath)
                     INC(capLinks);
                     capArray2[capLinks] := tempLink.Id;
                  END IF;               
               END IF;
            END IF;                                              
         END FOREACH;
      END IF;
      ASK genTable TO SetSize(9, totalNodes-(totalHiers*2)-2);
      {Fill up General Table and display array}
      NEW(GArray, 1..totalNodes-(totalHiers*2)-2, 1..10);
      j:=1;
      FOREACH tempNode IN nodeGroup
         IF tempNode.typeNode = 2
            GArray[j,1] := tempNode.name;
            GArray[j,2] := INTTOSTR(tempNode.goodPaths);
            GArray[j,3] := INTTOSTR(tempNode.connectIntoNum);
            GArray[j,4] := INTTOSTR(tempNode.connectOutOfNum);
            IF tempNode.DependencyNum=-2            
               GArray[j,5] := "System Dependent";
            ELSIF tempNode.DependencyNum=-1            
               GArray[j,5] := "Locally Dependent";
            ELSIF tempNode.DependencyNum=0            
               GArray[j,5] := "Independent";
            ELSE 
               IF tempNode.depType = "RBDBlock"
                  tempBlk := ASK root Child("RBDBlock",tempNode.DependencyNum);  
                  GArray[j,5] := tempBlk.name + " Block";
               ELSIF tempNode.depType = "RBDEvent"
                  tempEvent := ASK root Child("RBDEvent",tempNode.DependencyNum);  
                  GArray[j,5] := tempEvent.name + " Event";
               ELSE
                  tempNd := ASK root Child("RBDNode",tempNode.DependencyNum); 
                  IF tempNd.typeNode = 5
                     tempHier := ASK root Child("RBDHier", tempNd.parentID);
                     GArray[j,5] := tempHier.name + " Hier";
                  ELSE
                     GArray[j,5] := tempNd.name + " Node";
                  END IF;
               END IF;
            END IF;
            GArray[j,6] := INTTOSTR(tempNode.Id);
            IF tempNode.usesPhasing
               GArray[j,7] := "True";
            ELSE
               GArray[j,7] := "False";
            END IF;
            IF tempNode.coldStandby
               GArray[j,8] := "True";
            ELSE
               GArray[j,8] := "False";
            END IF;
            IF tempNode.reportNodeAnal
               GArray[j,9] := "True";
            ELSE
               GArray[j,9] := "False";
            END IF;
            IF tempNode.parentID > 0
               tempHier := ASK root Child("RBDHier", tempNode.parentID);
               GArray[j,10] := INTTOSTR(tempHier.Id);
            ELSE
               GArray[j,10] := "0";
            END IF;
            INC(j);
         END IF;
      END FOREACH;
      HierFilter("nodegen", genTable, GArray, HIGH(GArray), 10, "All"{-1}, printing, displayGArray, hiercount);
      IF ((hiercount = 0) AND (NOT printing))
         ASK genFilter TO SetText("Home");
         ASK genFilter TO Deactivate;
         ASK gFilterText TO Deactivate;
      END IF;
      GenSort(1, totalNodes-(totalHiers*2)-2,"A");
      sort1 := "1A";
      IF coldLinks > 0
         ASK standbyTable TO SetSize(9, coldLinks);
         {Fill up Standby Table and display array}
         NEW(SArray, 1..coldLinks, 1..10);
         FOR i := 1 TO coldLinks
            coldArray[i] := coldArray2[i];
         END FOR;
         FOR i:=1 TO coldLinks
            tempLink := ASK root Child("RBDLink", coldArray[i]);
            {coldArray[pos] := tempLink.Id;}
            IF tempLink.connectFRef = "RBDBlock"
               tempBlock := ASK root Child("RBDBlock",tempLink.connectFromId);
               SArray[i,1] := tempBlock.name+" Block"; 
            ELSIF tempLink.connectFRef = "RBDEvent"
               tempEvent := ASK root Child("RBDEvent",tempLink.connectFromId);
               SArray[i,1] := tempEvent.name+" Event"; 
            ELSIF tempLink.connectFRef = "RBDHier"
               tempHier := ASK root Child("RBDHier", tempLink.connectFromId);
               SArray[i,1] := tempHier.name+" Hier";
            ELSE
               tempNode := ASK root Child("RBDNode",tempLink.connectFromId);
               SArray[i,1] := tempNode.name+" Node";                       
            END IF;
            IF tempLink.connectTRef = "RBDBlock"
               tempBlock := ASK root Child("RBDBlock",tempLink.connectToId);
               SArray[i,2] := tempBlock.name+" Block";                       
            ELSIF tempLink.connectTRef = "RBDEvent"
               tempEvent := ASK root Child("RBDEvent",tempLink.connectToId);
               SArray[i,2] := tempEvent.name+" Event";                       
            ELSIF tempLink.connectTRef = "RBDHier"
               tempHier := ASK root Child("RBDHier", tempLink.connectToId);
               SArray[i,2] := tempHier.name+" Hier";
            ELSE
               tempNode := ASK root Child("RBDNode",tempLink.connectToId);
               SArray[i,2] := tempNode.name+" Node";                       
            END IF;
            tempNode := ASK root Child("RBDNode",tempLink.connectToId);
            SArray[i,3] := INTTOSTR(tempNode.KStar);
            SArray[i,4] := INTTOSTR(tempLink.coldPriority);                       
            SArray[i,5] := REALTOSTR(tempLink.autoSwitchProb);                       
            SArray[i,6] := ChopZeros(tempLink.autoSwitchTime,9);                       
            IF tempLink.manualSwitchTime < 0.
               SArray[i,7] := "N/A";                       
            ELSE
               SArray[i,7] := ChopZeros(tempLink.manualSwitchTime,9);                       
            END IF;
            IF tempNode.priorityReturn
               SArray[i,8] := "True"; 
            ELSE
               SArray[i,8] := "False"; 
            END IF;
            IF tempNode.checkAutosFirst
               SArray[i,9] := "True"; 
            ELSE
               SArray[i,9] := "False"; 
            END IF;
            IF tempLink.parentID > 0
               tempHier := ASK root Child("RBDHier", tempLink.parentID);
               SArray[i,10] := INTTOSTR(tempHier.Id);
            ELSE
               SArray[i,10] := "0"
            END IF;
         END FOR;
         HierFilter("standby", standbyTable, SArray, HIGH(SArray), 10, "All"{-1}, printing, displaySArray, hiercount);
         IF ((hiercount = 0) AND (NOT printing))
            ASK standbyFilter TO SetText("Home");
            ASK standbyFilter TO Deactivate;
            ASK sFilterText TO Deactivate;
         END IF;
         StandbySort(1, coldLinks, "A");
         sort2 := "1A";
      ELSE
         IF NOT printing
            ASK standbyTab TO Deactivate;
         END IF;
      END IF;
      IF capLinks > 0
         ASK capInTable TO SetSize(5, capLinks);
         {Fill up Capacity Table and display array}
         NEW(CArray, 1..capLinks, 1..6);
         FOR i := 1 TO capLinks
            capArray[i] := capArray2[i];
         END FOR;
         FOR i:=1 TO capLinks
            tempLink := ASK root Child("RBDLink", capArray[i]);
            IF tempLink.connectFRef = "RBDNode"
               tempNode := ASK root Child("RBDNode",tempLink.connectFromId);
               fromS := tempNode.name+" Node";
            ELSIF tempLink.connectFRef = "RBDBlock"
               tempBlock := ASK root Child("RBDBlock",tempLink.connectFromId);
               fromS := tempBlock.name+" Block";
            ELSIF tempLink.connectFRef = "RBDEvent"
               tempEvent := ASK root Child("RBDEvent",tempLink.connectFromId);
               fromS := tempEvent.name+" Event";
            ELSIF tempLink.connectFRef = "RBDHier"
               tempHier := ASK root Child("RBDHier", tempLink.connectFromId);
               fromS := tempHier.name+" Hier";
            END IF;
            IF tempLink.connectTRef = "RBDNode"
               tempNode2 := ASK root Child("RBDNode",tempLink.connectToId);
               toS := tempNode2.name+" Node";
            ELSIF tempLink.connectTRef = "RBDBlock"
               tempBlock := ASK root Child("RBDBlock",tempLink.connectToId);
               toS := tempBlock.name+" Block";
            ELSIF tempLink.connectTRef = "RBDEvent"
               tempEvent := ASK root Child("RBDEvent",tempLink.connectToId);
               toS := tempEvent.name+" Event";
            ELSIF tempLink.connectTRef = "RBDHier"
               tempHier := ASK root Child("RBDHier", tempLink.connectToId);
               toS := tempHier.name+" Hier";
            END IF;
            CArray[i,1] := fromS;                       
            CArray[i,2] := toS;                       
            IF tempLink.connectTRef = "RBDNode"
               IF tempNode2.fullFlow
                  CArray[i,3] := "N/A"; 
               ELSE
                  IF tempLink.nomFlow = -2
                     CArray[i,3] := "MaxFlow/"+INTTOSTR(tempNode2.connectIntoNum); 
                  ELSIF tempLink.nomFlow = -1
                     CArray[i,3] := "MaxFlow"; 
                  ELSE
                     CArray[i,3] := INTTOSTR(tempLink.nomFlow); 
                  END IF;
               END IF;
            ELSE
              CArray[i,3] := "N/A"; 
            END IF;
            IF tempLink.connectTRef = "RBDNode"
               IF tempNode2.fullFlow
                  CArray[i,4] := "N/A"; 
               ELSE
                  IF tempLink.nomFlow = -2
                     CArray[i,4] := "FullFlow/"+INTTOSTR(tempNode2.connectIntoNum); 
                  ELSIF tempLink.nomFlow = -1
                     CArray[i,4] := "FullFlow"; 
                  ELSE
                     CArray[i,4] := INTTOSTR(tempLink.maxFlow); 
                  END IF;
               END IF;
            ELSE
               CArray[i,4] := "N/A"; 
            END IF;
            IF tempLink.connectFRef = "RBDNode"
              IF tempNode.anyPath
                  CArray[i,5] := "N/A"; 
               ELSE
                  CArray[i,5] := INTTOSTR(tempLink.capPriority); 
               END IF;
            ELSE
               CArray[i,5] := "N/A"; 
            END IF;
            IF tempLink.parentID > 0
               tempHier := ASK root Child("RBDHier", tempLink.parentID);
               CArray[i,6] := INTTOSTR(tempHier.Id);
            ELSE
               CArray[i,6] := "0";
            END IF;
         END FOR;
         HierFilter("cap", capInTable, CArray, HIGH(CArray), 6, "All"{-1}, printing, displayCArray, hiercount);
         IF ((hiercount = 0) AND (NOT printing))
            ASK capFilter TO SetText("Home");
            ASK capFilter TO Deactivate;
            ASK cFilterText TO Deactivate;
         END IF;
         CapSort(1, capLinks, "A");
         sort3 := "1A";
      ELSE
         IF NOT printing
            ASK capTab TO Deactivate;
         END IF;
      END IF;
      IF NOT printing
         Draw;
      END IF;
   END METHOD; {InitSelf, NodeInputBoxObj}

   ASK METHOD BeSelected;
   VAR
      validRepairs,failDistChanged,
      validFails,repairDistChanged : BOOLEAN;
      i, hiercount                            : INTEGER;
      textValue                    : STRING;
      block                        : RBDBlockObj;
      newFvals, newRvals           : realArray;
      objClicked                   : GraphicVObj;
      mu,sigma2                    : REAL;
      startfVal, startrVal         : ARRAY INTEGER OF STRING;
      dumbArray                    : timeType;
      table                        : TableObj;    
   BEGIN
      NEW(table);        {needed}
      objClicked := LastPicked;
      CASE objClicked.Id
         WHEN 100:  {General Tab}         
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 101 {General Table}
               IF genTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort2)) = genTable.SelectedColumn
                     IF SUBSTR(2,2,sort2) = "A"
                        GenSort(genTable.SelectedColumn, HIGH(displayGArray), "D");
                        sort2 := INTTOSTR(genTable.SelectedColumn)+"D";
                     ELSE
                        GenSort(genTable.SelectedColumn, HIGH(displayGArray), "A");
                        sort2 := INTTOSTR(genTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     GenSort(genTable.SelectedColumn, HIGH(displayGArray), "A");
                     sort2 := INTTOSTR(genTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 102
               IF ((genFilter.Text() = "--------------------------------------") 
                   OR (genFilter.Text() = oldGFilter))
                  ASK genFilter TO SetText(oldGFilter);
               ELSE
                  HierFilter("nodegen", genTable, GArray, HIGH(GArray), 10, genFilter.Text(), FALSE, displayGArray, hiercount);
                  oldGFilter := genFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 972 {Print Button}
               IF compileType <> "demo"
                  PrintInputTable(genTable, "NodeGeneral", "Print");
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 200:  {Standby Tab}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 201 {Standby Table}
               IF standbyTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort2)) = standbyTable.SelectedColumn
                     IF SUBSTR(2,2,sort2) = "A"
                        StandbySort(standbyTable.SelectedColumn, HIGH(displaySArray), "D");
                        sort2 := INTTOSTR(standbyTable.SelectedColumn)+"D";
                     ELSE
                        StandbySort(standbyTable.SelectedColumn, HIGH(displaySArray), "A");
                        sort2 := INTTOSTR(standbyTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     StandbySort(standbyTable.SelectedColumn, HIGH(displaySArray), "A");
                     sort2 := INTTOSTR(standbyTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 202
               IF ((standbyFilter.Text() = "--------------------------------------") 
                   OR (standbyFilter.Text() = oldSFilter))
                  ASK standbyFilter TO SetText(oldSFilter);
               ELSE
                  HierFilter("standby", standbyTable, SArray, HIGH(SArray), 10, standbyFilter.Text(), FALSE, displaySArray, hiercount);
                  oldSFilter := standbyFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 973 {Print Button}
               IF compileType <> "demo"
                  PrintInputTable(standbyTable, "NodeStandby", "Print");
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 300:  {Capacity Tab}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 301 {Capacity Table}
               IF capInTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort3)) = capInTable.SelectedColumn
                     IF SUBSTR(2,2,sort3) = "A"
                        CapSort(capInTable.SelectedColumn, HIGH(displayCArray), "D");
                        sort3 := INTTOSTR(capInTable.SelectedColumn)+"D";
                     ELSE
                        CapSort(capInTable.SelectedColumn, HIGH(displayCArray), "A");
                        sort3 := INTTOSTR(capInTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     CapSort(capInTable.SelectedColumn, HIGH(displayCArray), "A");
                     sort3 := INTTOSTR(capInTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 302
               IF ((capFilter.Text() = "--------------------------------------") 
                   OR (capFilter.Text() = oldCFilter))
                  ASK capFilter TO SetText(oldCFilter);
               ELSE
                  HierFilter("cap", capInTable, CArray, HIGH(CArray), 6, capFilter.Text(), FALSE, displayCArray, hiercount);
                  oldCFilter := capFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 974 {Print Button}
               IF compileType <> "demo"
                  PrintInputTable(capInTable, "NodeCapacity", "Print");
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 971:  {Help Button}
         OTHERWISE
      END CASE;
      Update;
      INHERITED BeSelected;  {Call the inherited method of a HelpBoxObj}
   END METHOD; {BeSelected}

   ASK METHOD GenSort(IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
   VAR
      numPlaced,i,j,pos              : INTEGER;
      cell,currentStr               : STRING;
      currentNum, cellNum            : REAL;
      tempNode, tempNd               : RBDNodeObj;
      tempBlk                        : RBDBlockObj;
   BEGIN
      pos := 1;
      numPlaced := 1;
      FOR i := 1 TO tableSize
         CASE sortCol
            WHEN 1:
               currentStr := LOWER(displayGArray[i,1])
            WHEN 2:
               currentNum := STRTOREAL(displayGArray[i,2]);
            WHEN 3:
               currentNum := STRTOREAL(displayGArray[i,3]);
            WHEN 4:
               currentNum := STRTOREAL(displayGArray[i,4]);
            WHEN 5:
               currentStr := LOWER(displayGArray[i,5]);
            WHEN 6: 
               currentNum := STRTOREAL(displayGArray[i,6]);
            WHEN 7:
               currentStr := LOWER(displayGArray[i,7]);
            WHEN 8:
               currentStr := LOWER(displayGArray[i,8]);
            WHEN 9:
               currentStr := LOWER(displayGArray[i,9]);
            OTHERWISE            
         END CASE;
         IF (sortCol = 1) OR (sortCol = 5) OR (sortCol = 7) OR (sortCol = 8) OR (sortCol = 9)
            cell := LOWER(genTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                  GenInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(genTable.Text(sortCol, pos));
                  WHILE((currentStr > cell) AND (pos < (tableSize-1)))
                     INC(pos);
                     cell := LOWER(genTable.Text(sortCol, pos));
                  END WHILE;
                  GenInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  GenInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(genTable.Text(sortCol, pos));
                  WHILE((currentStr < cell) AND (pos < (tableSize-1)))
                     INC(pos);
                     cell := LOWER(genTable.Text(sortCol, pos));
                  END WHILE;
                  GenInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            cellNum := STRTOREAL(ASK genTable Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  GenInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK genTable Text(sortCol, pos));
                  WHILE((currentNum > cellNum) AND (pos < (tableSize-1)))
                     INC(pos);
                     cellNum := STRTOREAL(ASK genTable Text(sortCol, pos));
                  END WHILE;
                  GenInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  GenInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK genTable Text(sortCol, pos));
                  WHILE((currentNum < cellNum) AND (pos < (tableSize-1)))
                     INC(pos);
                     cellNum := STRTOREAL(ASK genTable Text(sortCol, pos));
                  END WHILE;
                  GenInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {GenSort}
            
   ASK METHOD GenInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j,n                : INTEGER;
      text2              : STRING;
      pool               : SparePoolObj;
      tempBlk            : RBDBlockObj;
      tempNode, tempNd   : RBDNodeObj;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK genTable Text(1, j-1);
         ASK genTable TO SetText(text2, 1, j);
         text2 := ASK genTable Text(2, j-1);
         ASK genTable TO SetText(text2, 2, j);
         text2 := ASK genTable Text(3, j-1);
         ASK genTable TO SetText(text2, 3, j);
         text2 := ASK genTable Text(4, j-1);
         ASK genTable TO SetText(text2, 4, j);
         text2 := ASK genTable Text(5, j-1);
         ASK genTable TO SetText(text2, 5, j); 
         text2 := ASK genTable Text(6, j-1);
         ASK genTable TO SetText(text2, 6, j); 
         text2 := ASK genTable Text(7, j-1);
         ASK genTable TO SetText(text2, 7, j); 
         text2 := ASK genTable Text(8, j-1);
         ASK genTable TO SetText(text2, 8, j); 
         text2 := ASK genTable Text(9, j-1);
         ASK genTable TO SetText(text2, 9, j); 
      END FOR;     
      ASK genTable TO SetText(displayGArray[i,1], 1, pos);                       
      ASK genTable TO SetText(displayGArray[i,2], 2, pos);                       
      ASK genTable TO SetText(displayGArray[i,3], 3, pos);                       
      ASK genTable TO SetText(displayGArray[i,4], 4, pos);                       
      ASK genTable TO SetText(displayGArray[i,5], 5, pos);                       
      ASK genTable TO SetText(displayGArray[i,6], 6, pos);                       
      ASK genTable TO SetText(displayGArray[i,7], 7, pos);                       
      ASK genTable TO SetText(displayGArray[i,8], 8, pos);                       
      ASK genTable TO SetText(displayGArray[i,9], 9, pos);                       
   END METHOD;{GenInsert}

   ASK METHOD StandbySort(IN sortCol, tableSize : INTEGER;
                          IN sortDir : STRING);
   VAR
      numPlaced,i,j,pos              : INTEGER;
      cell,currentStr                : STRING;
      currentNum, cellNum            : REAL;
      tempLink                       : LinkObj;
      tempNode                       : RBDNodeObj;
      tempBlock                      : RBDBlockObj;
   BEGIN
      pos := 1;
      numPlaced := 1;
      FOR i := 1 TO coldLinks
         coldArray[i] := coldArray2[i];
      END FOR;
      FOR i := 1 TO tableSize
         tempLink := ASK root Child ("RBDLink", coldArray[i]);
         CASE sortCol
            WHEN 1:
               currentStr := LOWER(displaySArray[i,1]);
            WHEN 2:
               currentStr := LOWER(displaySArray[i,2]);
            WHEN 3:
               currentNum := STRTOREAL(displaySArray[i,3]);
            WHEN 4:
               currentNum := STRTOREAL(displaySArray[i,4]);
            WHEN 5:
               currentNum := STRTOREAL(displaySArray[i,5]);
            WHEN 6:
               currentNum := STRTOREAL(displaySArray[i,6]);
            WHEN 7:
               currentNum := STRTOREAL(displaySArray[i,7]);
            WHEN 8:
               currentStr := LOWER(displaySArray[i,8]);
            WHEN 9:
               currentStr := LOWER(displaySArray[i,9]);
            OTHERWISE            
         END CASE;
         IF (sortCol=1) OR (sortCol=2) OR (sortCol=8) OR (sortCol=9)
            cell := LOWER(standbyTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                  StandbyInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(standbyTable.Text(sortCol, pos));
                  WHILE((currentStr > cell) AND (pos < (tableSize-1)))
                     INC(pos);
                     cell := LOWER(standbyTable.Text(sortCol, pos));
                  END WHILE;
                  StandbyInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  StandbyInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(standbyTable.Text(sortCol, pos));
                  WHILE((currentStr < cell) AND (pos < (tableSize-1)))
                     INC(pos);
                     cell := LOWER(standbyTable.Text(sortCol, pos));
                  END WHILE;
                  StandbyInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            IF (ASK standbyTable Text(sortCol, numPlaced-1) = "N/A") OR (numPlaced = 1)
               cellNum := -1.;
            ELSE
               cellNum := STRTOREAL(ASK standbyTable Text(sortCol, numPlaced-1));
            END IF;
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  StandbyInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF (ASK standbyTable Text(sortCol, pos)) = "N/A"
                     cellNum := -1.;
                  ELSE
                     cellNum := STRTOREAL(ASK standbyTable Text(sortCol, pos));
                  END IF;
                  WHILE((currentNum > cellNum) AND (pos < (tableSize-1)))
                     INC(pos);
                     IF (ASK standbyTable Text(sortCol, pos)) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(ASK standbyTable Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  StandbyInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  StandbyInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF (ASK standbyTable Text(sortCol, pos)) = "N/A"
                     cellNum := -1.;
                  ELSE
                     cellNum := STRTOREAL(ASK standbyTable Text(sortCol, pos));
                  END IF;
                  WHILE((currentNum < cellNum) AND (pos < (tableSize-1)))
                     INC(pos);
                     IF (ASK standbyTable Text(sortCol, pos)) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(ASK standbyTable Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  StandbyInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {StandbySort}
            
   ASK METHOD StandbyInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j,n               : INTEGER;
      text2             : STRING;
      pool              : SparePoolObj;
      tempBlock         : RBDBlockObj;
      tempLink          : LinkObj;
      tempNode          : RBDNodeObj;
      logMean2,logVar   : REAL;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK standbyTable Text(1, j-1);
         ASK standbyTable TO SetText(text2, 1, j);
         text2 := ASK standbyTable Text(2, j-1);
         ASK standbyTable TO SetText(text2, 2, j);
         text2 := ASK standbyTable Text(3, j-1);
         ASK standbyTable TO SetText(text2, 3, j);
         text2 := ASK standbyTable Text(4, j-1);
         ASK standbyTable TO SetText(text2, 4, j);
         text2 := ASK standbyTable Text(5, j-1);
         ASK standbyTable TO SetText(text2, 5, j); 
         text2 := ASK standbyTable Text(6, j-1);
         ASK standbyTable TO SetText(text2, 6, j);
         text2 := ASK standbyTable Text(7, j-1);
         ASK standbyTable TO SetText(text2, 7, j);
         text2 := ASK standbyTable Text(8, j-1);
         ASK standbyTable TO SetText(text2, 8, j);
         text2 := ASK standbyTable Text(9, j-1);
         ASK standbyTable TO SetText(text2, 9, j);
         coldArray[j] := coldArray[j-1];
      END FOR;     
      ASK standbyTable TO SetText(displaySArray[i,1], 1, pos);                       
      ASK standbyTable TO SetText(displaySArray[i,2], 2, pos);                       
      ASK standbyTable TO SetText(displaySArray[i,3], 3, pos);                       
      ASK standbyTable TO SetText(displaySArray[i,4], 4, pos);                       
      ASK standbyTable TO SetText(displaySArray[i,5], 5, pos);                       
      ASK standbyTable TO SetText(displaySArray[i,6], 6, pos);                       
      ASK standbyTable TO SetText(displaySArray[i,7], 7, pos);                       
      ASK standbyTable TO SetText(displaySArray[i,8], 8, pos);                       
      ASK standbyTable TO SetText(displaySArray[i,9], 9, pos);                       
   END METHOD;{StandbyInsert}
   
   ASK METHOD CapSort(IN sortCol, tableSize : INTEGER;
                      IN sortDir : STRING);
   VAR
      numPlaced,i,j,pos              : INTEGER;
      cell,currentStr                : STRING;
      currentNum, cellNum            : REAL;
      tempLink                       : LinkObj;
      tempNode                       : RBDNodeObj;
      tempBlock                      : RBDBlockObj;
   BEGIN
      pos := 1;
      numPlaced := 1;
      FOR i := 1 TO capLinks
         capArray[i] := capArray2[i];
      END FOR;
      FOR i := 1 TO tableSize
         CASE sortCol
            WHEN 1:
               currentStr := LOWER(displayCArray[i,1]); 
            WHEN 2:
               currentStr := LOWER(displayCArray[i,2]);
            WHEN 3:
               IF SUBSTR(1,1,displayCArray[i,3]) = "N"
                  currentNum := -1.;
               ELSIF SUBSTR(1,1,displayCArray[i,3]) = "F"
                  IF SUBSTR(9,9,displayCArray[i,3]) = "/"
                     currentNum := 9999999997.;
                  ELSE
                     currentNum := 9999999998.;
                  END IF;
               ELSIF SUBSTR(1,1,displayCArray[i,3]) = "M"
                  IF SUBSTR(8,8,displayCArray[i,3]) = "/"
                     currentNum := 9999999997.;
                  ELSE
                     currentNum := 9999999998.;
                  END IF;
               ELSE
                  currentNum := STRTOREAL(displayCArray[i,3]);
               END IF;
            WHEN 4:
               IF SUBSTR(1,1,displayCArray[i,4]) = "N"
                  currentNum := -1.;
               ELSIF SUBSTR(1,1,displayCArray[i,4]) = "F"
                  IF SUBSTR(9,9,displayCArray[i,4]) = "/"
                     currentNum := 9999999997.;
                  ELSE
                     currentNum := 9999999998.;
                  END IF;
               ELSIF SUBSTR(1,1,displayCArray[i,4]) = "M"
                  IF SUBSTR(8,8,displayCArray[i,4]) = "/"
                     currentNum := 9999999997.;
                  ELSE
                     currentNum := 9999999998.;
                  END IF;
               ELSE
                  currentNum := STRTOREAL(displayCArray[i,4]);
               END IF;
            WHEN 5:
               IF SUBSTR(1,1,displayCArray[i,5]) = "N"
                  currentNum := -1.;
               ELSE
                  currentNum := STRTOREAL(displayCArray[i,5]);
               END IF;
            OTHERWISE            
         END CASE;
         IF (sortCol = 1) OR (sortCol = 2)
            cell := LOWER(capInTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                  CapInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(capInTable.Text(sortCol, pos));
                  WHILE((currentStr > cell) AND (pos < (tableSize-1)))
                     INC(pos);
                     cell := LOWER(capInTable.Text(sortCol, pos));
                  END WHILE;
                  CapInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  CapInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(capInTable.Text(sortCol, pos));
                  WHILE((currentStr < cell) AND (pos < (tableSize-1)))
                     INC(pos);
                     cell := LOWER(capInTable.Text(sortCol, pos));
                  END WHILE;
                  CapInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            IF SUBSTR(1,1,(ASK capInTable Text(sortCol, numPlaced-1))) = "N"
               cellNum := -1.;
            ELSIF SUBSTR(1,1,(ASK capInTable Text(sortCol, numPlaced-1))) = "F"
               IF SUBSTR(9,9,(ASK capInTable Text(sortCol, numPlaced-1))) = "/"
                  cellNum := 9999999997.;
               ELSE
                  cellNum := 9999999998.;
               END IF;
            ELSIF SUBSTR(1,1,(ASK capInTable Text(sortCol, numPlaced-1))) = "M"
               IF SUBSTR(8,8,(ASK capInTable Text(sortCol, numPlaced-1))) = "/"
                  cellNum := 9999999997.;
               ELSE
                  cellNum := 9999999998.;
               END IF;
            ELSE
               cellNum := STRTOREAL(ASK capInTable Text(sortCol, numPlaced-1));
            END IF;
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  CapInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF SUBSTR(1,1,(ASK capInTable Text(sortCol, pos))) = "N"
                     cellNum := -1.;
                  ELSIF SUBSTR(1,1,(ASK capInTable Text(sortCol, pos))) = "F"
                     IF SUBSTR(9,9,(ASK capInTable Text(sortCol, pos))) = "/"
                        cellNum := 9999999997.;
                     ELSE
                        cellNum := 9999999998.;
                     END IF;
                  ELSIF SUBSTR(1,1,(ASK capInTable Text(sortCol, pos))) = "M"
                     IF SUBSTR(8,8,(ASK capInTable Text(sortCol, pos))) = "/"
                        cellNum := 9999999997.;
                     ELSE
                        cellNum := 9999999998.;
                     END IF;
                  ELSE
                     cellNum := STRTOREAL(ASK capInTable Text(sortCol, pos));
                  END IF;
                  WHILE((currentNum > cellNum) AND (pos < (tableSize-1)))
                     INC(pos);
                     IF SUBSTR(1,1,(ASK capInTable Text(sortCol, pos))) = "N"
                        cellNum := -1.;
                     ELSIF SUBSTR(1,1,(ASK capInTable Text(sortCol, pos))) = "F"
                        IF SUBSTR(9,9,(ASK capInTable Text(sortCol, pos))) = "/"
                           cellNum := 9999999997.;
                        ELSE
                           cellNum := 9999999998.;
                        END IF;
                     ELSIF SUBSTR(1,1,(ASK capInTable Text(sortCol, pos))) = "M"
                        IF SUBSTR(8,8,(ASK capInTable Text(sortCol, pos))) = "/"
                           cellNum := 9999999997.;
                        ELSE
                           cellNum := 9999999998.;
                        END IF;
                     ELSE
                        cellNum := STRTOREAL(ASK capInTable Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  CapInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  CapInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF SUBSTR(1,1,(ASK capInTable Text(sortCol, pos))) = "N"
                     cellNum := -1.;
                  ELSIF SUBSTR(1,1,(ASK capInTable Text(sortCol, pos))) = "F"
                     IF SUBSTR(9,9,(ASK capInTable Text(sortCol, pos))) = "/"
                        cellNum := 9999999997.;
                     ELSE
                        cellNum := 9999999998.;
                     END IF;
                  ELSIF SUBSTR(1,1,(ASK capInTable Text(sortCol, pos))) = "M"
                     IF SUBSTR(8,8,(ASK capInTable Text(sortCol, pos))) = "/"
                        cellNum := 9999999997.;
                     ELSE
                        cellNum := 9999999998.;
                     END IF;
                  ELSE
                     cellNum := STRTOREAL(ASK capInTable Text(sortCol, pos));
                  END IF;
                  WHILE((currentNum < cellNum) AND (pos < (tableSize-1)))
                     INC(pos);
                     IF SUBSTR(1,1,(ASK capInTable Text(sortCol, pos))) = "N"
                        cellNum := -1.;
                     ELSIF SUBSTR(1,1,(ASK capInTable Text(sortCol, pos))) = "F"
                        IF SUBSTR(9,9,(ASK capInTable Text(sortCol, pos))) = "/"
                           cellNum := 9999999997.;
                        ELSE
                           cellNum := 9999999998.;
                        END IF;
                     ELSIF SUBSTR(1,1,(ASK capInTable Text(sortCol, pos))) = "M"
                        IF SUBSTR(8,8,(ASK capInTable Text(sortCol, pos))) = "/"
                           cellNum := 9999999997.;
                        ELSE
                           cellNum := 9999999998.;
                        END IF;
                     ELSE
                        cellNum := STRTOREAL(ASK capInTable Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  CapInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {CapSort}
            
   ASK METHOD CapInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j,n                : INTEGER;
      text2,fromS, toS   : STRING;
      pool               : SparePoolObj;
      tempBlock          : RBDBlockObj;
      tempLink           : LinkObj;
      tempNode,tempNode2 : RBDNodeObj;
      logMean2,logVar    : REAL;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK capInTable Text(1, j-1);
         ASK capInTable TO SetText(text2, 1, j);
         text2 := ASK capInTable Text(2, j-1);
         ASK capInTable TO SetText(text2, 2, j);
         text2 := ASK capInTable Text(3, j-1);
         ASK capInTable TO SetText(text2, 3, j);
         text2 := ASK capInTable Text(4, j-1);
         ASK capInTable TO SetText(text2, 4, j);
         text2 := ASK capInTable Text(5, j-1);
         ASK capInTable TO SetText(text2, 5, j);
         capArray[j] := capArray[j-1];
      END FOR;     
      ASK capInTable TO SetText(displayCArray[i,1], 1, pos);                       
      ASK capInTable TO SetText(displayCArray[i,2], 2, pos);                       
      ASK capInTable TO SetText(displayCArray[i,3], 3, pos);                       
      ASK capInTable TO SetText(displayCArray[i,4], 4, pos);                       
      ASK capInTable TO SetText(displayCArray[i,5], 5, pos);                       
   END METHOD;{CapInsert}

END OBJECT; {NodeInputBoxObj}

OBJECT EventInputBoxObj;
   ASK METHOD InitSelf(IN printing : BOOLEAN);
   VAR
      i,j, numPM, hiercount  : INTEGER;
      event                                    : RBDEventObj;
      tempHier                                 : RBDHierObj;
      hierList                                 : OptionListType;
      text2                                    : STRING;
   BEGIN
      genTable          := Descendant("GenTable", 101);
      genFilter         := Descendant("GenFilter", 102);
      gFilterText       := Descendant("GFilterText", 103);
      oldGFilter := "All";
  
      IF NOT printing
         Draw;
         IF totalHiers > 0
            NEW(hierList, 1..totalHiers+3);
            InitHierFilter(hierList);
            ASK genFilter TO SetOptions(hierList);
            ASK genFilter TO SetText(hierList[1]);
         ELSE
            ASK genFilter TO SetText("Home");
            ASK genFilter TO Deactivate;
            ASK gFilterText TO Deactivate;
         END IF;
      END IF;
      ASK genTable TO SetSize(7, totalEvents);
      {Fill up General Table and display array}
      NEW(GArray, 1..totalEvents, 1..8);
      j:=1;
      FOREACH event IN eventGroup 
         GArray[j,1] := event.name;
         GArray[j,2] := REALTOSTR(event.failVals[1]);
         IF event.failStream = 201
            text2:="A";
         ELSIF event.failStream = 202
            text2:="B";
         ELSIF event.failStream = 203
            text2:="C";
         ELSIF event.failStream = 204
            text2:="D";
         ELSIF event.failStream = 205
            text2:="E";
         ELSIF event.failStream = 206
            text2:="F";
         ELSIF event.failStream = 207
            text2:="G";
         ELSIF event.failStream = 208
            text2:="H";
         ELSIF event.failStream = 209
            text2:="I";
         ELSIF event.failStream = 210
            text2:="J";
         ELSE
            text2:=INTTOSTR(event.failStream);
         END IF;
         GArray[j,3] := text2;
         IF event.usesPhasing
            GArray[j,4] := "True";
         ELSE
            GArray[j,4] := "False";
         END IF;
         GArray[j,5] := REALTOCOST(event.initialCost, 13, 3);
         GArray[j,6] := REALTOCOST(event.operatingCost, 13, 3);
         GArray[j,7] := REALTOCOST(event.repairingCost, 13, 3);
         IF event.parentID > 0
            tempHier := ASK root Child("RBDHier", event.parentID);
            GArray[j,8] := INTTOSTR(tempHier.Id);
         ELSE
            GArray[j,8] := "0";
         END IF;
         INC(j);
      END FOREACH;
      HierFilter("eventgen", genTable, GArray, HIGH(GArray), 8, "All", printing, displayGArray, hiercount);
      IF ((hiercount = 0) AND (NOT printing))
         ASK genFilter TO SetText("Home");
         ASK genFilter TO Deactivate;
         ASK gFilterText TO Deactivate;
      END IF;
      GenSort(1, HIGH(GArray), "A");
      sort1 := "1A";
      IF NOT printing
         Draw;
      END IF;
   END METHOD; {InitSelf, EventInputBoxObj}

   ASK METHOD BeSelected;
   VAR
      i, hiercount                 : INTEGER;
      textValue                    : STRING;
      block                        : RBDBlockObj;
      objClicked                   : GraphicVObj;
      table                        : TableObj;    
   BEGIN
      NEW(table);        {needed}
      lastClicked := LastPicked;
      IF lastClicked.Id = 101 {General Table}
         IF genTable.SelectedRow = 0
            IF STRTOINT(SUBSTR(1,1,sort1)) = genTable.SelectedColumn
               IF SUBSTR(2,2,sort1) = "A"
                  GenSort(genTable.SelectedColumn, HIGH(displayGArray), "D");
                  sort1 := INTTOSTR(genTable.SelectedColumn)+"D";
               ELSE
                  GenSort(genTable.SelectedColumn, HIGH(displayGArray), "A");
                  sort1 := INTTOSTR(genTable.SelectedColumn)+"A";
               END IF;   
            ELSE
               GenSort(genTable.SelectedColumn, HIGH(displayGArray), "A");
               sort1 := INTTOSTR(genTable.SelectedColumn)+"A";
            END IF;
         END IF;
      ELSIF lastClicked.Id = 102
         IF ((genFilter.Text() = "--------------------------------------") 
            OR (genFilter.Text() = oldGFilter))
            ASK genFilter TO SetText(oldGFilter);
         ELSE
            HierFilter("eventgen", genTable, GArray, HIGH(GArray), 8, genFilter.Text(), FALSE, displayGArray, hiercount);
            oldGFilter := genFilter.Text();
         END IF;
      ELSIF lastClicked.Id = 972 {Print Button}
         IF compileType <> "demo"
            PrintInputTable(genTable, "Event", "Print");
         ELSE
            NEW(message, 1..1);
            message[1] := "The demo version has no print capability     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
      ELSIF lastClicked.Id = 971  {Help Button}
      END IF;
      Update;
      INHERITED BeSelected;  {Call the inherited method of a HelpBoxObj}
   END METHOD; {BeSelected}
   
   ASK METHOD GenSort(IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
   VAR
      numPlaced,i,j,pos              : INTEGER;
      cell,currentStr                : STRING;
      block                          : RBDBlockObj;
      currentNum, cellNum            : REAL;
   BEGIN
      pos := 1;
      numPlaced := 1;
      FOR i := 1 TO tableSize
         CASE sortCol
            WHEN 1:
               currentStr := LOWER(displayGArray[i,1]);
            WHEN 2: {Prob Success}
               currentNum := STRTOREAL(displayGArray[i,2]);
            WHEN 3: {F-Stream}
               currentStr:=LOWER(displayGArray[i,3]);
            WHEN 4: {Phased}
               currentStr := LOWER(displayGArray[i,4]);
            WHEN 5: {Initial $}
               currentNum := COSTTOREAL(displayGArray[i,5]);
            WHEN 6: {Success $}
               currentNum := COSTTOREAL(displayGArray[i,6]);
            WHEN 7: {Failure $}
               currentNum := COSTTOREAL(displayGArray[i,7]);
            OTHERWISE            
         END CASE;
         IF (sortCol=1) OR (sortCol=3) OR (sortCol=4)
            cell := LOWER(genTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                  GenInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(genTable.Text(sortCol, pos));
                  WHILE(currentStr > cell)
                     INC(pos);
                     cell := LOWER(genTable.Text(sortCol, pos));
                  END WHILE;
                  GenInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  GenInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(genTable.Text(sortCol, pos));
                  WHILE(currentStr < cell)
                     INC(pos);
                     cell := LOWER(genTable.Text(sortCol, pos));
                  END WHILE;
                  GenInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSIF (sortCol = 2)
            cellNum := STRTOREAL(ASK genTable Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  GenInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK genTable Text(sortCol, pos));
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK genTable Text(sortCol, pos));
                  END WHILE;
                  GenInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  GenInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK genTable Text(sortCol, pos));
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK genTable Text(sortCol, pos));
                  END WHILE;
                  GenInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            cellNum := COSTTOREAL(genTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  GenInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := COSTTOREAL(genTable.Text(sortCol, pos));
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     cellNum := COSTTOREAL(genTable.Text(sortCol, pos));
                  END WHILE;
                  GenInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  GenInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := COSTTOREAL(genTable.Text(sortCol, pos));
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     cellNum := COSTTOREAL(genTable.Text(sortCol, pos));
                  END WHILE;
                  GenInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {GenSort}
            
   ASK METHOD GenInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j             : INTEGER;
      text2         : STRING;
      pool          : SparePoolObj;
      block         : RBDBlockObj;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK genTable Text(1, j-1);
         ASK genTable TO SetText(text2, 1, j);
         text2 := ASK genTable Text(2, j-1);
         ASK genTable TO SetText(text2, 2, j);
         text2 := ASK genTable Text(3, j-1);
         ASK genTable TO SetText(text2, 3, j);
         text2 := ASK genTable Text(4, j-1);
         ASK genTable TO SetText(text2, 4, j);
         text2 := ASK genTable Text(5, j-1);
         ASK genTable TO SetText(text2, 5, j);
         text2 := ASK genTable Text(6, j-1);
         ASK genTable TO SetText(text2, 6, j);
         text2 := ASK genTable Text(7, j-1);
         ASK genTable TO SetText(text2, 7, j);
      END FOR;         
      ASK genTable TO SetText(displayGArray[i,1], 1, pos);                       
      ASK genTable TO SetText(displayGArray[i,2], 2, pos);                       
      ASK genTable TO SetText(displayGArray[i,3], 3, pos);                       
      ASK genTable TO SetText(displayGArray[i,4], 4, pos);                       
      ASK genTable TO SetText(displayGArray[i,5], 5, pos);                       
      ASK genTable TO SetText(displayGArray[i,6], 6, pos);                       
      ASK genTable TO SetText(displayGArray[i,7], 7, pos);                       
   END METHOD;{GenInsert}

END OBJECT; {EventInputBoxObj}

OBJECT HierInputBoxObj;
   ASK METHOD InitSelf;
   BEGIN
      genTable          := Descendant("GenTable", 101);

      ASK genTable TO SetSize(8, totalHiers);
      GenSort(1,"A");
      sort1 := "1A";
   END METHOD; {InitSelf, HierInputBoxObj}

   ASK METHOD BeSelected;
   VAR
      objClicked                   : GraphicVObj;
      table                        : TableObj;    
   BEGIN
      NEW(table);        {needed}
      lastClicked := LastPicked;
      IF lastClicked.Id = 101 {General Table}
         IF genTable.SelectedRow = 0
            IF STRTOINT(SUBSTR(1,1,sort1)) = genTable.SelectedColumn
               IF SUBSTR(2,2,sort1) = "A"
                  GenSort(genTable.SelectedColumn, "D");
                  sort1 := INTTOSTR(genTable.SelectedColumn)+"D";
               ELSE
                  GenSort(genTable.SelectedColumn, "A");
                  sort1 := INTTOSTR(genTable.SelectedColumn)+"A";
               END IF;   
            ELSE
               GenSort(genTable.SelectedColumn, "A");
               sort1 := INTTOSTR(genTable.SelectedColumn)+"A";
            END IF;
         END IF;
      ELSIF lastClicked.Id = 972 {Print Button}
         IF compileType <> "demo"
            PrintInputTable(genTable, "Hierarchy", "Print");
         ELSE
            NEW(message, 1..1);
            message[1] := "The demo version has no print capability     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
      ELSIF lastClicked.Id = 971 {Help Button}
      END IF;
      Update;
      INHERITED BeSelected;  {Call the inherited method of a HelpBoxObj}
   END METHOD; {BeSelected}
   
   ASK METHOD GenSort(IN sortCol : INTEGER;
                      IN sortDir : STRING);
   VAR
      numPlaced,i,j,pos, 
      numBlocks, numEvents, numNodes,
      numHiers                       : INTEGER;
      cell,currentStr                : STRING;
      block, tempBlock               : RBDBlockObj;
      tempEvent                      : RBDEventObj;
      outNode, tempNode              : RBDNodeObj;
      hier, tempHier                 : RBDHierObj;
      currentNum, cellNum            : REAL;
      child : ANYOBJ;
   BEGIN
      pos := 1;
      numPlaced := 1;
      FOREACH hier IN hierGroup
         numBlocks := 0;
         numEvents := 0;
         numNodes := 0;
         numHiers := 0;
         FOREACH child IN hier.childGroup
            IF OBJTYPENAME(child) = "RBDBlockObj"
               INC(numBlocks);
            ELSIF OBJTYPENAME(child) = "RBDEventObj"
               INC(numEvents);
            ELSIF OBJTYPENAME(child) = "RBDNodeObj"
               INC(numNodes);
            ELSIF OBJTYPENAME(child) = "RBDHierObj"
               INC(numHiers);
            END IF;
         END FOREACH;
         CASE sortCol
            WHEN 1:
               currentStr := LOWER(hier.name);
            WHEN 2: {Level}
               currentNum := FLOAT(hier.level);
            WHEN 3: {Dependency Type}
               outNode := ASK root Child("RBDNode", hier.outID);
               IF outNode.DependencyNum=-2            
                  currentStr := "system dependent";
               ELSIF outNode.DependencyNum=-1            
                  currentStr := "locally dependent";
               ELSIF outNode.DependencyNum=0            
                  currentStr := "independent";
               ELSE
                  IF outNode.depType = "RBDBlock"
                     tempBlock := ASK root Child("RBDBlock",outNode.DependencyNum);  
                     currentStr := LOWER(tempBlock.name) + " block";
                  ELSIF outNode.depType = "RBDEvent"
                     tempEvent := ASK root Child("RBDEvent",outNode.DependencyNum);  
                     currentStr := LOWER(tempEvent.name) + " event";
                  ELSE
                     tempNode := ASK root Child("RBDNode",outNode.DependencyNum);
                     IF tempNode.typeNode = 5
                        tempHier := ASK root Child("RBDHier", tempNode.parentID);
                        currentStr := LOWER(tempHier.name) + " hier";
                     ELSE
                        currentStr := LOWER(tempNode.name) + " node";
                     END IF;
                  END IF;
               END IF; 
            WHEN 4: {Phased}
               IF hier.usesPhasing
                  currentStr := "true";
               ELSE
                  currentStr := "false";
               END IF;
            WHEN 5: {Blocks}
               currentNum := FLOAT(numBlocks);
            WHEN 6: {Nodes}
               currentNum := FLOAT(numNodes);
            WHEN 7: {Events}
               currentNum := FLOAT(numEvents);
            WHEN 8: {Hiers}
               currentNum := FLOAT(numHiers);
            OTHERWISE            
         END CASE;
         IF (sortCol=1) OR (sortCol=3) OR (sortCol=4)
            cell := LOWER(genTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                  GenInsert(hier, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(genTable.Text(sortCol, pos));
                  WHILE(currentStr > cell)
                     INC(pos);
                     cell := LOWER(genTable.Text(sortCol, pos));
                  END WHILE;
                  GenInsert(hier, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  GenInsert(hier, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(genTable.Text(sortCol, pos));
                  WHILE(currentStr < cell)
                     INC(pos);
                     cell := LOWER(genTable.Text(sortCol, pos));
                  END WHILE;
                  GenInsert(hier, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            cellNum := STRTOREAL(ASK genTable Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  GenInsert(hier, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK genTable Text(sortCol, pos));
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK genTable Text(sortCol, pos));
                  END WHILE;
                  GenInsert(hier, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  GenInsert(hier, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK genTable Text(sortCol, pos));
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK genTable Text(sortCol, pos));
                  END WHILE;
                  GenInsert(hier, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOREACH;
   END METHOD; {GenSort}
            
   ASK METHOD GenInsert(IN currHier : RBDHierObj; IN pos, numPlaced : INTEGER);
   VAR
      j, numBlocks, numNodes, 
      numEvents, numHiers     : INTEGER;
      text2                   : STRING;
      pool                    : SparePoolObj;
      block, tempBlock        : RBDBlockObj;
      tempEvent               : RBDEventObj;
      hier, tempHier          : RBDHierObj;
      outNode, tempNode       : RBDNodeObj;
      child                   : ANYOBJ;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK genTable Text(1, j-1);
         ASK genTable TO SetText(text2, 1, j);
         text2 := ASK genTable Text(2, j-1);
         ASK genTable TO SetText(text2, 2, j);
         text2 := ASK genTable Text(3, j-1);
         ASK genTable TO SetText(text2, 3, j);
         text2 := ASK genTable Text(4, j-1);
         ASK genTable TO SetText(text2, 4, j);
         text2 := ASK genTable Text(5, j-1);
         ASK genTable TO SetText(text2, 5, j);
         text2 := ASK genTable Text(6, j-1);
         ASK genTable TO SetText(text2, 6, j);
         text2 := ASK genTable Text(7, j-1);
         ASK genTable TO SetText(text2, 7, j);
         text2 := ASK genTable Text(8, j-1);
         ASK genTable TO SetText(text2, 8, j);
      END FOR;         
      ASK genTable TO SetText(currHier.name, 1, pos);
      ASK genTable TO SetText(INTTOSTR(currHier.level), 2, pos);
      outNode := ASK root Child("RBDNode", currHier.outID);
      IF outNode.DependencyNum=-2            
         ASK genTable TO SetText("System Dependent", 3, pos);
      ELSIF outNode.DependencyNum=-1            
         ASK genTable TO SetText("Locally Dependent", 3, pos);
      ELSIF outNode.DependencyNum=0            
         ASK genTable TO SetText("Independent", 3, pos);
      ELSE
         IF outNode.depType = "RBDBlock"
            tempBlock := ASK root Child("RBDBlock",outNode.DependencyNum); 
            ASK genTable TO SetText(tempBlock.name+" Block", 3, pos);
         ELSIF outNode.depType = "RBDEvent"
            tempEvent := ASK root Child("RBDEvent",outNode.DependencyNum); 
            ASK genTable TO SetText(tempEvent.name+" Event", 3, pos);
         ELSE
            tempNode := ASK root Child("RBDNode",outNode.DependencyNum);  
            IF tempNode.typeNode = 5
               tempHier := ASK root Child("RBDHier", tempNode.parentID);
               ASK genTable TO SetText(tempHier.name + " Hier", 3, pos);
            ELSE
               ASK genTable TO SetText(tempNode.name + " Node", 3, pos);
            END IF;
         END IF;
      END IF; 
      IF currHier.usesPhasing
         ASK genTable TO SetText("True", 4, pos);
      ELSE
         ASK genTable TO SetText("False", 4, pos);
      END IF;
      numBlocks := 0;
      numEvents := 0;
      numNodes := 0;
      numHiers := 0;
      FOREACH child IN currHier.childGroup
         IF OBJTYPENAME(child) = "RBDBlockObj"
            INC(numBlocks);
         ELSIF OBJTYPENAME(child) = "RBDEventObj"
            INC(numEvents);
         ELSIF OBJTYPENAME(child) = "RBDNodeObj"
            INC(numNodes);
         ELSIF OBJTYPENAME(child) = "RBDHierObj"
            INC(numHiers);
         END IF;
      END FOREACH;
      ASK genTable TO SetText(INTTOSTR(numBlocks), 5, pos);
      ASK genTable TO SetText(INTTOSTR(numNodes), 6, pos);
      ASK genTable TO SetText(INTTOSTR(numEvents), 7, pos);
      ASK genTable TO SetText(INTTOSTR(numHiers), 8, pos);
   END METHOD;{GenInsert}

END OBJECT; {HierInputBoxObj}

OBJECT SystemInputBoxObj;
   ASK METHOD InitSelf(IN printing : BOOLEAN);
   VAR
      i  : INTEGER;
      block                  : RBDBlockObj;
      tempLink               : LinkObj;
      tempNode               : RBDNodeObj;
   BEGIN
      sysTab          := Child("SysTab", 100);
      truncLabel      := Descendant("TruncLabel", 101);
      trialLabel      := Descendant("TrialLabel", 102);
      statLabel       := Descendant("StatLabel", 103);
      blockLabel      := Descendant("BlockLabel", 104);
      nodeLabel       := Descendant("NodeLabel", 105);
      linkLabel       := Descendant("LinkLabel", 106);
      eventLabel      := Descendant("EventLabel", 107);
      hierLabel       := Descendant("HierLabel", 108);
      redLabel        := Descendant("RedLabel", 109);
      flowLabel       := Descendant("FlowLabel", 110);
      spareLabel      := Descendant("SpareLabel", 111);
      resLabel        := Descendant("ResLabel", 112);
      trigLabel       := Descendant("TrigLabel", 113);
      volumeLabel     := Descendant("VolumeLabel", 114);
      ancLabel        := Descendant("AncLabel", 115);
      spareTab        := Child("SpareTab", 200);
      spareTable      := Descendant("SpareTable", 201);
      resTab          := Child("ResTab", 300);
      resTable        := Descendant("ResTable", 301);
      trigTab         := Child("TrigTab", 400);
      trigTable       := Descendant("TrigTable", 401);
      
      NEW(sysArray, 1..19);
      IF termType = 1
         sysArray[2] := "Time-truncated; t = " + REALTOSTR(dTimeTrunc);
         ASK truncLabel TO SetLabel(sysArray[2]);
         sysArray[4] := "Start statistics at " + REALTOSTR(dTimeStartTime) + " " + systemUnits;
         ASK statLabel TO SetLabel(sysArray[4]);
      ELSIF termType = 2
         sysArray[2] := "Failure-truncated; r = " + REALTOSTR(dFailTrunc);
         ASK truncLabel TO SetLabel(sysArray[2]);
         sysArray[4] := "Start statistics at " + REALTOSTR(dFailStartTime) + " failures";
         ASK statLabel TO SetLabel(sysArray[4]);
      ELSIF termType = 3
         sysArray[2] := "Cycle-truncated; c = " + REALTOSTR(dCycleTrunc);
         ASK truncLabel TO SetLabel(sysArray[2]);
         sysArray[4] := "Start statistics at " + REALTOSTR(dCycleStartTime) + " cycles";
         ASK statLabel TO SetLabel(sysArray[4]);
      END IF;
      sysArray[1] := "Simulation Settings:"
      sysArray[3] := "Trials = " + REALTOSTR(dNumberOfRuns);
      sysArray[5] := "Topology:"
      sysArray[6] := "Number of blocks = " + INTTOSTR(totalBlocks);
      sysArray[7] := "Number of nodes = " + INTTOSTR(totalNodes);
      sysArray[8] := "Number of links = " + INTTOSTR(totalLinks);
      sysArray[9] := "Number of events = " + INTTOSTR(totalEvents);
      sysArray[10] := "Number of hierarchies = " + INTTOSTR(totalHiers);
      sysArray[11] := "Costs:"
      sysArray[12] := "Cost of red time = " + REALTOSTR(systemRedCost) + " per " + systemUnits;
      sysArray[13] := "Cost of lost flow = " + REALTOSTR(sysLostCost) + " per " + systemUnits;
      sysArray[14] := "General:"
      sysArray[15] := "Number of spare pools = " + INTTOSTR(totalSpares);
      sysArray[16] := "Number of resource types = " + INTTOSTR(totalRes);
      sysArray[17] := "Number of triggers = " + " INTTOSTR(totalTriggers);
      sysArray[18] := "Volume generated: " + INTTOSTR(flowGenerated) + " units per " + systemUnits;
      sysArray[19] := "Ancillary stream is " + INTTOSTR(sysStreams[11]);
      ASK trialLabel TO SetLabel(sysArray[3]);
      ASK blockLabel TO SetLabel(sysArray[6]);
      ASK nodeLabel TO SetLabel(sysArray[7]);
      ASK linkLabel TO SetLabel(sysArray[8]);
      ASK eventLabel TO SetLabel(sysArray[9]);
      ASK hierLabel TO SetLabel(sysArray[10]);
      ASK redLabel TO SetLabel(sysArray[12]);
      ASK flowLabel TO SetLabel(sysArray[13]);
      ASK spareLabel TO SetLabel(sysArray[15]);
      ASK resLabel TO SetLabel(sysArray[16]);
      ASK trigLabel TO SetLabel(sysArray[17]);
      ASK volumeLabel TO SetLabel(sysArray[18]);
      ASK ancLabel TO SetLabel(sysArray[19]);
      IF NOT printing
         Draw;
      END IF;
      IF totalSpares > 0
         ASK spareTable TO SetSize(10, totalSpares);
         SpareSort(1,"A");
         sort1 := "1A";
      ELSE
         IF NOT printing
            ASK spareTab TO Deactivate;
         END IF;
      END IF;
      IF totalRes > 0
         ASK resTable TO SetSize(5, totalRes);
         ResSort(1, "A");
         sort2 := "1A";
      ELSE
         IF NOT printing
            ASK resTab TO Deactivate;
         END IF;
      END IF;
      IF totalTriggers > 0
         ASK trigTable TO SetSize(4, totalTriggers);
         TrigSort(1,"A");
         sort3 := "1A";
      ELSE
         IF NOT printing
            ASK trigTab TO Deactivate;
         END IF;
      END IF;
      IF NOT printing
         Draw;
      END IF;
   END METHOD; {InitSelf}
   
   ASK METHOD BeSelected;
   VAR
      i                            : INTEGER;
      block                        : RBDBlockObj;
      objClicked                   : GraphicVObj;
      table                        : TableObj;    
   BEGIN
      NEW(table);        {needed}
      objClicked := LastPicked;
      CASE objClicked.Id
         WHEN 100:  {System Settings Tab}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 972 {Print Button}
               IF compileType <> "demo"
                  PrintSystemInput(sysArray);
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 200:  {Spares Tab}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 201 {Spares Table}
               IF spareTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,(STRLEN(sort1)-1),sort1)) = spareTable.SelectedColumn
                     IF SUBSTR(STRLEN(sort1),STRLEN(sort1),sort1) = "A"
                        SpareSort(spareTable.SelectedColumn, "D");
                        sort1 := INTTOSTR(spareTable.SelectedColumn)+"D";
                     ELSE
                        SpareSort(spareTable.SelectedColumn, "A");
                        sort1 := INTTOSTR(spareTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     SpareSort(spareTable.SelectedColumn, "A");
                     sort1 := INTTOSTR(spareTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 973 {Print Button}
               IF compileType <> "demo"
                  PrintInputTable(spareTable,"SparePools","Print"); 
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 300:  {Resources Tab}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 301 {General Table}
               IF resTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,(STRLEN(sort2)-1),sort2)) = resTable.SelectedColumn
                     IF SUBSTR(STRLEN(sort2),STRLEN(sort2),sort2) = "A"
                        ResSort(resTable.SelectedColumn, "D");
                        sort2 := INTTOSTR(resTable.SelectedColumn)+"D";
                     ELSE
                        ResSort(resTable.SelectedColumn, "A");
                        sort2 := INTTOSTR(resTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     ResSort(resTable.SelectedColumn, "A");
                     sort2 := INTTOSTR(resTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 974 {Print Button}
               IF compileType <> "demo"
                  PrintInputTable(resTable, "Resources", "Print"); 
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 400:  {Triggers Tab}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 401
               IF trigTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,(STRLEN(sort3)-1),sort3)) = trigTable.SelectedColumn
                     IF SUBSTR(STRLEN(sort3),STRLEN(sort3),sort3) = "A"
                        TrigSort(trigTable.SelectedColumn, "D");
                        sort3 := INTTOSTR(trigTable.SelectedColumn)+"D";
                     ELSE
                        TrigSort(trigTable.SelectedColumn, "A");
                        sort3 := INTTOSTR(trigTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     TrigSort(trigTable.SelectedColumn, "A");
                     sort3 := INTTOSTR(trigTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 975 {Print Button}
               IF compileType <> "demo"
                  PrintInputTable(trigTable, "Triggers", "Print");
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 971:  {Help Button}
         OTHERWISE  
      END CASE;
      Update;
      INHERITED BeSelected;  {Call the inherited method of a HelpBoxObj}
   END METHOD; {BeSelected}

   ASK METHOD SpareSort(IN sortCol : INTEGER;
                      IN sortDir : STRING);
   VAR
      numPlaced,i,j,pos              : INTEGER;
      cell,currentStr,blockSparing : STRING;
      block                          : RBDBlockObj;
      currentNum, cellNum            : REAL;
      pool                           : SparePoolObj;
   BEGIN
      pos := 1;
      numPlaced := 1;
      FOREACH pool IN poolGroup
         IF pool.sparingType = SparePool
            CASE sortCol
               WHEN 1:
                  currentStr := LOWER(pool.poolName);
               WHEN 2:
                  currentNum := FLOAT(pool.initialSpares);
               WHEN 3:
                  IF (NOT pool.routineSpareOrdering)
                     currentNum := -1.;
                  ELSE
                     currentNum := FLOAT(pool.newSpares);
                  END IF;
               WHEN 4:
                  IF (NOT pool.routineSpareOrdering)
                     currentNum := -1.;
                  ELSE
                     currentNum := pool.newSparesArrival;
                  END IF;
               WHEN 5:
                  IF (NOT pool.stockLevelOrdering)
                     currentNum := -1.;
                  ELSE
                     currentNum := FLOAT(pool.SLOOrderLevel);
                  END IF;
               WHEN 6:
                  IF (NOT pool.stockLevelOrdering)
                     currentNum := -1.;
                  ELSE
                     currentNum := FLOAT(pool.SLONewSpares);
                  END IF;
               WHEN 7:
                  IF (NOT pool.stockLevelOrdering)
                     currentNum := -1.;
                  ELSE
                     currentNum := pool.SLOTime;
                  END IF;
               WHEN 8:
                  IF (NOT pool.emerSpareOrdering)
                     currentNum := -1.;
                  ELSE
                     currentNum := pool.emergencyTime;
                  END IF;
               WHEN 9:
                  currentNum := pool.spareCost;
               WHEN 10:
                  currentNum := pool.emerShippingCost;
               OTHERWISE            
            END CASE;
            IF (sortCol = 1)
               cell := LOWER(spareTable.Text(sortCol, numPlaced-1));
               IF sortDir="A"
                  IF ((numPlaced = 1) OR (currentStr > cell))
                     SpareInsert(pool, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     cell := LOWER(spareTable.Text(sortCol, pos));
                     WHILE(currentStr > cell)
                        INC(pos);
                        cell := LOWER(spareTable.Text(sortCol, pos));
                     END WHILE;
                     SpareInsert(pool, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               ELSE            
                  IF ((numPlaced = 1) OR (currentStr < cell))
                     SpareInsert(pool, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     cell := LOWER(spareTable.Text(sortCol, pos));
                     WHILE(currentStr < cell)
                        INC(pos);
                        cell := LOWER(spareTable.Text(sortCol, pos));
                     END WHILE;
                     SpareInsert(pool, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               END IF;
            ELSIF (sortCol = 9) OR (sortCol = 10)
               IF (numPlaced = 1) OR (ASK spareTable Text(sortCol, numPlaced-1) = "N/A")
                  cellNum := -1.;
               ELSE
                  cellNum := COSTTOREAL(ASK spareTable Text(sortCol, numPlaced-1));
               END IF;
               IF sortDir="A"
                  IF ((numPlaced = 1) OR (currentNum > cellNum))
                     SpareInsert(pool, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     IF ASK spareTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := COSTTOREAL(ASK spareTable Text(sortCol, pos));
                     END IF;
                     WHILE(currentNum > cellNum)
                        INC(pos);
                        IF ASK spareTable Text(sortCol, pos) = "N/A"
                           cellNum := -1.;
                        ELSE
                           cellNum := COSTTOREAL(ASK spareTable Text(sortCol, pos));
                        END IF;
                     END WHILE;
                     SpareInsert(pool, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               ELSE
                  IF ((numPlaced = 1) OR (currentNum < cellNum))
                     SpareInsert(pool, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     IF ASK spareTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := COSTTOREAL(ASK spareTable Text(sortCol, pos));
                     END IF;
                     WHILE(currentNum < cellNum)
                        INC(pos);
                        IF ASK spareTable Text(sortCol, pos) = "N/A"
                           cellNum := -1.;
                        ELSE
                           cellNum := COSTTOREAL(ASK spareTable Text(sortCol, pos));
                        END IF;
                     END WHILE;
                     SpareInsert(pool, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               END IF;
           ELSE
               IF (numPlaced = 1) OR (ASK spareTable Text(sortCol, numPlaced-1) = "N/A")
                  cellNum := -1.;
               ELSE
                  cellNum := STRTOREAL(ASK spareTable Text(sortCol, numPlaced-1));
               END IF;
               IF sortDir="A"
                  IF ((numPlaced = 1) OR (currentNum > cellNum))
                     SpareInsert(pool, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     IF ASK spareTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(ASK spareTable Text(sortCol, pos));
                     END IF;
                     WHILE(currentNum > cellNum)
                        INC(pos);
                        IF ASK spareTable Text(sortCol, pos) = "N/A"
                           cellNum := -1.;
                        ELSE
                           cellNum := STRTOREAL(ASK spareTable Text(sortCol, pos));
                        END IF;
                     END WHILE;
                     SpareInsert(pool, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               ELSE
                  IF ((numPlaced = 1) OR (currentNum < cellNum))
                     SpareInsert(pool, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     IF ASK spareTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(ASK spareTable Text(sortCol, pos));
                     END IF;
                     WHILE(currentNum < cellNum)
                        INC(pos);
                        IF ASK spareTable Text(sortCol, pos) = "N/A"
                           cellNum := -1.;
                        ELSE
                           cellNum := STRTOREAL(ASK spareTable Text(sortCol, pos));
                        END IF;
                     END WHILE;
                     SpareInsert(pool, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               END IF;
            END IF;
         END IF;
      END FOREACH;
   END METHOD; {SpareSort}
            
   ASK METHOD SpareInsert(IN pool : SparePoolObj; IN pos, numPlaced : INTEGER);
   VAR
      j     : INTEGER;
      text2 : STRING;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK spareTable Text(1, j-1);
         ASK spareTable TO SetText(text2, 1, j);
         text2 := ASK spareTable Text(2, j-1);
         ASK spareTable TO SetText(text2, 2, j);
         text2 := ASK spareTable Text(3, j-1);
         ASK spareTable TO SetText(text2, 3, j);
         text2 := ASK spareTable Text(4, j-1);
         ASK spareTable TO SetText(text2, 4, j);
         text2 := ASK spareTable Text(5, j-1);
         ASK spareTable TO SetText(text2, 5, j);
         text2 := ASK spareTable Text(6, j-1);
         ASK spareTable TO SetText(text2, 6, j);
         text2 := ASK spareTable Text(7, j-1);
         ASK spareTable TO SetText(text2, 7, j);
         text2 := ASK spareTable Text(8, j-1);
         ASK spareTable TO SetText(text2, 8, j);
         text2 := ASK spareTable Text(9, j-1);
         ASK spareTable TO SetText(text2, 9, j);
         text2 := ASK spareTable Text(10, j-1);
         ASK spareTable TO SetText(text2, 10, j);
      END FOR;         
      ASK spareTable TO SetText(pool.poolName, 1, pos);
      ASK spareTable TO SetText(INTTOSTR(pool.initialSpares), 2, pos);
      IF (NOT pool.routineSpareOrdering)
         ASK spareTable TO SetText("N/A", 3, pos);
         ASK spareTable TO SetText("N/A", 4, pos);
      ELSE
         ASK spareTable TO SetText(INTTOSTR(pool.newSpares), 3, pos);
         ASK spareTable TO SetText(ChopZeros(pool.newSparesArrival,9), 4, pos);
      END IF;
      IF (NOT pool.stockLevelOrdering)
         ASK spareTable TO SetText("N/A", 5, pos);
         ASK spareTable TO SetText("N/A", 6, pos);
         ASK spareTable TO SetText("N/A", 7, pos);
      ELSE
         ASK spareTable TO SetText(INTTOSTR(pool.SLOOrderLevel), 5, pos);
         ASK spareTable TO SetText(INTTOSTR(pool.SLONewSpares), 6, pos);
         ASK spareTable TO SetText(ChopZeros(pool.SLOTime,9), 7, pos);
      END IF;
      IF (NOT pool.emerSpareOrdering)
         ASK spareTable TO SetText("N/A", 8, pos);
      ELSE
         ASK spareTable TO SetText(ChopZeros(pool.emergencyTime,9), 8, pos);
      END IF;
      ASK spareTable TO SetText(REALTOCOST(pool.spareCost, 13, 3), 9, pos);
      ASK spareTable TO SetText(REALTOCOST(pool.emerShippingCost, 13, 3), 10, pos);
   END METHOD;{SpareInsert}

   ASK METHOD ResSort(IN sortCol : INTEGER;
                      IN sortDir : STRING);
   VAR
      numPlaced,i,j,pos              : INTEGER;
      cell,currentStr,blockSparing : STRING;
      block                          : RBDBlockObj;
      currentNum, cellNum            : REAL;
      pool                           : SparePoolObj;
   BEGIN
      pos := 1;
      numPlaced := 1;
      FOREACH pool IN poolGroup
         IF pool.sparingType = Resource
            CASE sortCol
               WHEN 1:
                  currentStr := LOWER(pool.poolName);
               WHEN 2:
                  currentNum := FLOAT(pool.initialSpares);
               WHEN 3:
                  currentNum := pool.spareCost;
               WHEN 4:
                  currentNum := pool.costPerTime;
               WHEN 5:
                  currentNum := pool.fixedPerUse;
               OTHERWISE            
            END CASE;
            IF (sortCol=1)
               cell := LOWER(resTable.Text(sortCol, numPlaced-1));
               IF sortDir="A"
                  IF ((numPlaced = 1) OR (currentStr > cell))
                     ResInsert(pool, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     cell := LOWER(resTable.Text(sortCol, pos));
                     WHILE(currentStr > cell)
                        INC(pos);
                        cell := LOWER(resTable.Text(sortCol, pos));
                     END WHILE;
                     ResInsert(pool, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               ELSE            
                  IF ((numPlaced = 1) OR (currentStr < cell))
                     ResInsert(pool, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     cell := LOWER(resTable.Text(sortCol, pos));
                     WHILE(currentStr < cell)
                        INC(pos);
                        cell := LOWER(resTable.Text(sortCol, pos));
                     END WHILE;
                     ResInsert(pool, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               END IF;
            ELSIF (sortCol = 3) OR (sortCol = 4) OR (sortCol = 5)
               IF (numPlaced = 1) OR (ASK resTable Text(sortCol, numPlaced-1) = "N/A")
                  cellNum := -1.;
               ELSE
                  cellNum := COSTTOREAL(ASK resTable Text(sortCol, numPlaced-1));
               END IF;
               IF sortDir="A"
                  IF ((numPlaced = 1) OR (currentNum > cellNum))
                     ResInsert(pool, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     IF ASK resTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := COSTTOREAL(ASK resTable Text(sortCol, pos));
                     END IF;
                     WHILE(currentNum > cellNum)
                        INC(pos);
                        IF ASK resTable Text(sortCol, pos) = "N/A"
                           cellNum := -1.;
                        ELSE
                           cellNum := COSTTOREAL(ASK resTable Text(sortCol, pos));
                        END IF;
                     END WHILE;
                     ResInsert(pool, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               ELSE
                  IF ((numPlaced = 1) OR (currentNum < cellNum))
                     ResInsert(pool, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     IF ASK resTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := COSTTOREAL(ASK resTable Text(sortCol, pos));
                     END IF;
                     WHILE(currentNum < cellNum)
                        INC(pos);
                        IF ASK resTable Text(sortCol, pos) = "N/A"
                           cellNum := -1.;
                        ELSE
                           cellNum := COSTTOREAL(ASK resTable Text(sortCol, pos));
                        END IF;
                     END WHILE;
                     ResInsert(pool, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               END IF;
            ELSE
               IF (numPlaced = 1) OR (ASK resTable Text(sortCol, numPlaced-1) = "N/A")
                  cellNum := -1.;
               ELSE
                  cellNum := STRTOREAL(ASK resTable Text(sortCol, numPlaced-1));
               END IF;
               IF sortDir="A"
                  IF ((numPlaced = 1) OR (currentNum > cellNum))
                     ResInsert(pool, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     IF ASK resTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(ASK resTable Text(sortCol, pos));
                     END IF;
                     WHILE(currentNum > cellNum)
                        INC(pos);
                        IF ASK resTable Text(sortCol, pos) = "N/A"
                           cellNum := -1.;
                        ELSE
                           cellNum := STRTOREAL(ASK resTable Text(sortCol, pos));
                        END IF;
                     END WHILE;
                     ResInsert(pool, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               ELSE
                  IF ((numPlaced = 1) OR (currentNum < cellNum))
                     ResInsert(pool, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     IF ASK resTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(ASK resTable Text(sortCol, pos));
                     END IF;
                     WHILE(currentNum < cellNum)
                        INC(pos);
                        IF ASK resTable Text(sortCol, pos) = "N/A"
                           cellNum := -1.;
                        ELSE
                           cellNum := STRTOREAL(ASK resTable Text(sortCol, pos));
                        END IF;
                     END WHILE;
                     ResInsert(pool, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               END IF;
            END IF;
         END IF;
      END FOREACH;
   END METHOD; {ResSort}
            
   ASK METHOD ResInsert(IN pool : SparePoolObj; IN pos, numPlaced : INTEGER);
   VAR
      j     : INTEGER;
      text2 : STRING;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK resTable Text(1, j-1);
         ASK resTable TO SetText(text2, 1, j);
         text2 := ASK resTable Text(2, j-1);
         ASK resTable TO SetText(text2, 2, j);
         text2 := ASK resTable Text(3, j-1);
         ASK resTable TO SetText(text2, 3, j);
         text2 := ASK resTable Text(4, j-1);
         ASK resTable TO SetText(text2, 4, j);
         text2 := ASK resTable Text(5, j-1);
         ASK resTable TO SetText(text2, 5, j);
      END FOR;         
      ASK resTable TO SetText(pool.poolName, 1, pos);
      ASK resTable TO SetText(INTTOSTR(pool.initialSpares), 2, pos);
      ASK resTable TO SetText(REALTOCOST(pool.spareCost, 13, 3), 3, pos);
      ASK resTable TO SetText(REALTOCOST(pool.costPerTime, 13, 3), 4, pos);
      ASK resTable TO SetText(REALTOCOST(pool.fixedPerUse, 13, 3), 5, pos);
   END METHOD;{ResInsert}

   ASK METHOD TrigSort(IN sortCol : INTEGER;
                      IN sortDir : STRING);
   VAR
      numPlaced,i,j,pos              : INTEGER;
      cell,currentStr,tempString : STRING;
      block                          : RBDBlockObj;
      currentNum, cellNum, tempStdev            : REAL;
      trig                           : RapTriggerObj;
   BEGIN
      pos := 1;
      numPlaced := 1;
      FOREACH trig IN triggerGroup
         CASE sortCol
            WHEN 1:
               currentStr := LOWER(trig.TrigName);
            WHEN 2:
               MakeDistString(trig.TrigDist, trig.TrigParams, tempString);
               GetDistMean(tempString, currentNum)
            WHEN 3:
               currentNum := trig.InitUsed;
            WHEN 4:
               IF trig.Repeats
                  currentStr := "true"
               ELSE
                  currentStr := "false"
               END IF;
            OTHERWISE            
         END CASE;
         IF (sortCol = 1) OR (sortCol = 4)
            cell := LOWER(trigTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentStr > cell))
                  TrigInsert(trig, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(trigTable.Text(sortCol, pos));
                  WHILE(currentStr > cell)
                     INC(pos);
                     cell := LOWER(trigTable.Text(sortCol, pos));
                  END WHILE;
                  TrigInsert(trig, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentStr < cell))
                  TrigInsert(trig, numPlaced, numPlaced);
                  INC(numPlaced);
                ELSE
                  pos := 1;
                  cell := LOWER(trigTable.Text(sortCol, pos));
                  WHILE(currentStr < cell)
                     INC(pos);
                     cell := LOWER(trigTable.Text(sortCol, pos));
                  END WHILE;
                  TrigInsert(trig, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSIF (sortCol = 2) {distribution (sort by mean)}
            IF (numPlaced = 1) OR (ASK trigTable Text(sortCol, numPlaced-1) = "N/A")
               cellNum := -1.;
            ELSE
               tempString := ASK trigTable Text(sortCol, numPlaced-1);
               GetDistMean(tempString, cellNum)
            END IF;
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  TrigInsert(trig, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF ASK trigTable Text(sortCol, pos) = "N/A"
                     cellNum := -1.;
                  ELSE
                     tempString := ASK trigTable Text(sortCol, pos);
                     GetDistMean(tempString, cellNum)
                  END IF;
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     IF ASK trigTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        tempString := ASK trigTable Text(sortCol, pos);
                        GetDistMean(tempString, cellNum)
                     END IF;
                  END WHILE;
                  TrigInsert(trig, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  TrigInsert(trig, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF ASK trigTable Text(sortCol, pos) = "N/A"
                     cellNum := -1.;
                  ELSE
                     tempString := ASK trigTable Text(sortCol, pos);
                     GetDistMean(tempString, cellNum)
                  END IF;
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     IF ASK trigTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        tempString := ASK trigTable Text(sortCol, pos);
                        GetDistMean(tempString, cellNum)
                     END IF;
                  END WHILE;
                  TrigInsert(trig, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            IF (numPlaced = 1) OR (ASK trigTable Text(sortCol, numPlaced-1) = "N/A")
               cellNum := -1.;
            ELSE
               cellNum := STRTOREAL(ASK trigTable Text(sortCol, numPlaced-1));
            END IF;
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  TrigInsert(trig, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF ASK trigTable Text(sortCol, pos) = "N/A"
                     cellNum := -1.;
                  ELSE
                     cellNum := STRTOREAL(ASK trigTable Text(sortCol, pos));
                  END IF;
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     IF ASK trigTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(ASK trigTable Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  TrigInsert(trig, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  TrigInsert(trig, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF ASK trigTable Text(sortCol, pos) = "N/A"
                     cellNum := -1.;
                  ELSE
                     cellNum := STRTOREAL(ASK trigTable Text(sortCol, pos));
                  END IF;
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     IF ASK trigTable Text(sortCol, pos) = "N/A"
                        cellNum := -1.;
                     ELSE
                        cellNum := STRTOREAL(ASK trigTable Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  TrigInsert(trig, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOREACH;
   END METHOD; {TrigSort}
            
   ASK METHOD TrigInsert(IN trig : RapTriggerObj; IN pos, numPlaced : INTEGER);
   VAR
      j     : INTEGER;
      text2, tempString : STRING;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK trigTable Text(1, j-1);
         ASK trigTable TO SetText(text2, 1, j);
         text2 := ASK trigTable Text(2, j-1);
         ASK trigTable TO SetText(text2, 2, j);
         text2 := ASK trigTable Text(3, j-1);
         ASK trigTable TO SetText(text2, 3, j);
         text2 := ASK trigTable Text(4, j-1);
         ASK trigTable TO SetText(text2, 4, j);
      END FOR;         
      ASK trigTable TO SetText(trig.TrigName, 1, pos);
      MakeDistString(trig.TrigDist, trig.TrigParams, tempString);
      ASK trigTable TO SetText(tempString, 2, pos);
      ASK trigTable TO SetText(REALTOSTR(trig.InitUsed), 3, pos);
      IF trig.Repeats
         ASK trigTable TO SetText("True", 4, pos);
      ELSE
         ASK trigTable TO SetText("False", 4, pos);
      END IF;
   END METHOD;{TrigInsert}

END OBJECT;            

OBJECT TablesOutObj;

   ASK METHOD FillResultsTable(IN printing : BOOLEAN);
   VAR
      i,k,j, mod, hiercount                : INTEGER;
      tempBlock                            : RBDBlockObj;
      tempNode                             : RBDNodeObj;
      runString,durationStr                : STRING;
      totalCost                            : REAL;
   BEGIN
      ASK window TO SetSysCursor(BusyCursor);
      IF NOT printing
         Draw;
      END IF;
      {*** ------------ Summary Tab ------------ ***}
      IF NumRunsCompleted<>1
         runString:=" runs";
      ELSE 
         runString:=" run";
      END IF;
      IF termType=1
         durationStr:=" of sim time "+FinalArray[0,2]+":";
      ELSIF termType=2
         IF (STRTOINT(FinalArray[0,2])) <> 1
            durationStr:=" to " +FinalArray[0,2]+" failures:";
         ELSE   
            durationStr:=" to 1 failure:";
         END IF;
      ELSE
         IF (STRTOINT(FinalArray[0,2])) <> 1
            durationStr:=" of " +FinalArray[0,2]+" cycles:";
         ELSE   
            durationStr:=" of 1 cycle:";
         END IF;   
      END IF;   
      ASK resultsLabel TO SetLabel("Results from " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
      IF BlockCostArray <> NILARRAY
         ASK resultsTable TO SetText(FinalArray[1,1], 1, 1);
         ASK resultsTable TO SetText(REALTOCOST(STRTOREAL(FinalArray[1,2]),14,3), 2, 1);
         ASK resultsTable TO SetText(REALTOCOST(STRTOREAL(FinalArray[1,3]),14,3), 3, 1);
         ASK resultsTable TO SetText(REALTOCOST(STRTOREAL(FinalArray[1,4]),14,3), 4, 1);
         IF FinalArray[1,5] <> "N/A"
            ASK resultsTable TO SetText(REALTOCOST(STRTOREAL(FinalArray[1,5]),14,3), 5, 1);
         ELSE
            ASK resultsTable TO SetText("N/A", 5, 1);
         END IF;
         IF FinalArray[1,6] <> "N/A"
            ASK resultsTable TO SetText(REALTOCOST(STRTOREAL(FinalArray[1,6]),14,3), 6, 1);
         ELSE
            ASK resultsTable TO SetText("N/A", 6, 1);
         END IF;
         mod := 1;
      END IF; 
      FOR i := (1+mod) TO HIGH(FinalArray)
         FOR j := 1 TO 6
            tempString := FinalArray[i,j];
            ASK resultsTable TO SetText(FinalArray[i,j], j, i);
         END FOR;
      END FOR;
      ASK mrLabel TO SetLabel(FinalArray[0, 1]); 
      ASK sumLabel2 TO SetLabel(FinalArray[0,3]);
      ASK sumLabel3 TO SetLabel(FinalArray[0,4]);
 
      {*** ----------- Logistics Tab ----------- ***}
      ASK logisticsLabel TO SetLabel("Results from " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
      FOR i:=1 TO HIGH(LogArray)
         FOR j:=1 TO 6
            ASK logisticsTable TO SetText(LogArray[i,j],j,i);
         END FOR;
      END FOR;
      ASK logLabel1 TO SetLabel(LogArray[0,1]);
      ASK logLabel2 TO SetLabel(LogArray[0,2]);
 
      {*** ------------ Sparing Tab ------------ ***}
      ASK sparesLabel TO SetLabel("Average sparing data over " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
      IF (STRTOINT(SparesArray[0,1]) > 0)
         IF STRTOINT(SparesArray[0,1]) > 13
            ASK sparesTable TO SetSize(6 , STRTOINT(SparesArray[0,1]));
            ASK sparesTable TO SetVisibleSize(91,14);
         END IF;
         HierFilter("sparesOut", sparesTable, SparesArray, STRTOINT(SparesArray[0,1]), 7,
                "All", printing, displaySArray, hiercount);
         IF ((hiercount = 0) AND (NOT printing))
            ASK sparesFilter TO SetText("Home");
            ASK sparesFilter TO Deactivate;
            ASK sFilterText TO Deactivate;
         END IF;
      END IF;
   
      {*** ----------- Block Cost Tab ---------- ***}
      IF BlockCostArray <> NILARRAY
         IF HIGH(BlockCostArray) > 12
            ASK costTable TO SetSize(6,HIGH(BlockCostArray));
            ASK costTable TO SetVisibleSize(93,14);
         END IF;
         ASK costLabel TO SetLabel("Average cost data across " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
         HierFilter("blockcostOut", costTable, BlockCostArray, HIGH(BlockCostArray), 7,
                "All", printing, displayBCArray, hiercount);
         IF ((hiercount = 0) AND (NOT printing))
            ASK costFilter TO SetText("Home");
            ASK costFilter TO Deactivate;
            ASK bcFilterText TO Deactivate;
         END IF;
      END IF;
      
      {*** --------- Block Analysis Tab -------- ***}
      IF BlockArray <> NILARRAY
         IF weakLinkAnalType = 1
            ASK blockAnalLabel TO SetLabel("Availability from " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
         ELSIF weakLinkAnalType = 2
            ASK blockAnalLabel TO SetLabel("Dependability from " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
         ELSE
            ASK blockAnalLabel TO SetLabel("Reliability from " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
         END IF;
         IF HIGH(BlockArray) > 13
            ASK blockAnalTable TO SetSize(7,HIGH(BlockArray)); 
            ASK blockAnalTable TO SetVisibleSize(90, 14);
         END IF;
         HierFilter("blockanalOut", blockAnalTable, BlockArray, HIGH(BlockArray), 19,
                "All", printing, displayBArray, hiercount);
         IF ((hiercount = 0) AND (NOT printing))
            ASK blockAnalFilter TO SetText("Home");
            ASK blockAnalFilter TO Deactivate;
            ASK bFilterText TO Deactivate;
         END IF;
      END IF;
      
      {*** --------- Node Analysis Tab --------- ***}
      IF NodeArray <> NILARRAY
         IF weakLinkAnalType = 1
            ASK nodeAnalLabel TO SetLabel("Availability from " + INTTOSTR(NumRunsCompleted) + runString + durationStr); {3-7}
         ELSIF weakLinkAnalType = 2
            ASK nodeAnalLabel TO SetLabel("Dependability from " + INTTOSTR(NumRunsCompleted) + runString + durationStr); {8-12}
         ELSE
            ASK nodeAnalLabel TO SetLabel("Reliability from " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
         END IF;
         IF HIGH(NodeArray) > 13
            ASK nodeAnalTable TO SetSize(7, HIGH(NodeArray));
            ASK nodeAnalTable TO SetVisibleSize(90, 14);
         END IF;
         HierFilter("nodeanalOut", nodeAnalTable, NodeArray, HIGH(NodeArray), 19,
                "All", printing, displayNArray, hiercount);
         IF ((hiercount = 0) AND (NOT printing))
            ASK nodeAnalFilter TO SetText("Home");
            ASK nodeAnalFilter TO Deactivate;
            ASK nFilterText TO Deactivate;
         END IF;
      END IF;
      
      {*** --------- Event Analysis Tab -------- ***}
      IF EventArray <> NILARRAY
         IF weakLinkAnalType = 1
            ASK eventAnalLabel TO SetLabel("Availability from " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
         ELSIF weakLinkAnalType = 2
            ASK eventAnalLabel TO SetLabel("Dependability from " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
         ELSE
            ASK eventAnalLabel TO SetLabel("Reliability from " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
         END IF;
         IF HIGH(EventArray) > 13
            ASK eventAnalTable TO SetSize(7,HIGH(EventArray)); 
            ASK eventAnalTable TO SetVisibleSize(90, 14);
         END IF;
         HierFilter("eventanalOut", eventAnalTable, EventArray, HIGH(EventArray), 19,
                "All", printing, displayEArray, hiercount);
         IF ((hiercount = 0) AND (NOT printing))
            ASK eventAnalFilter TO SetText("Home");
            ASK eventAnalFilter TO Deactivate;
            ASK eFilterText TO Deactivate;
         END IF;
      END IF;
      
      {*** --------- Hier Analysis Tab -------- ***}
      IF HierArray <> NILARRAY
         IF weakLinkAnalType = 1
            ASK hierAnalLabel TO SetLabel("Availability from " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
         ELSIF weakLinkAnalType = 2
            ASK hierAnalLabel TO SetLabel("Dependability from " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
         ELSE
            ASK hierAnalLabel TO SetLabel("Reliability from " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
         END IF;
         IF HIGH(HierArray) > 13
            ASK hierAnalTable TO SetSize(7,HIGH(HierArray)); 
            ASK hierAnalTable TO SetVisibleSize(90, 14);
         END IF;
         HierFilter("hieranalOut", hierAnalTable, HierArray, HIGH(HierArray), 18,
                "All", printing, displayHArray, hiercount);
      END IF;

     {*** ------------ Capacity Tab ----------- ***}
      IF CapacityArray <> NILARRAY
         ASK capLabel TO SetLabel("Average capacity flow across " + INTTOSTR(NumRunsCompleted) + runString + durationStr);
         IF HIGH(CapacityArray) > 13
            ASK capOutTable TO SetSize(9, HIGH(CapacityArray));
            ASK capOutTable TO SetVisibleSize(103, 14);
         END IF;
         HierFilter("capacityOut", capOutTable, CapacityArray, HIGH(CapacityArray), 11,
                "All", printing, displayCArray, hiercount);
         IF ((hiercount = 0) AND (NOT printing))
            ASK capFilter TO SetText("Home");
            ASK capFilter TO Deactivate;
            ASK cFilterText TO Deactivate;
         END IF;
      END IF;
      IF NOT printing
         Draw;
      END IF;
      ASK window TO SetSysCursor(NormalCursor);    
   END METHOD; {FillResultsTable} 
      
   ASK METHOD InitSelf(IN printing : BOOLEAN);
   VAR
      i, j, minloc : INTEGER;
      tempBlock               : RBDBlockObj;
      tempHier                : RBDHierObj;
      hierList                : OptionListType;
      min, temp   : STRING;
      
   BEGIN
      resultsTab       := Child("ResultsTab", 100);
      resultsLabel     := Descendant("ResultsLabel", 101);
      resultsTable     := Descendant("ResultsTable", 102);
      mrLabel          := Descendant("MRLabel", 103);
      sumLabel2        := Descendant("SumLabel2", 104);
      sumLabel3        := Descendant("SumLabel3", 105);
      logisticsTab     := Child("LogisticsTab", 200);
      logisticsLabel   := Descendant("LogisticsLabel", 201);
      logisticsTable   := Descendant("LogisticsTable", 202);
      logLabel1        := Descendant("LogLabel1", 203);
      logLabel2        := Descendant("LogLabel2", 204);
      sparingTab       := Child("SparingTab", 300);
      sparesLabel      := Descendant("SparesLabel", 301);
      sparesTable      := Descendant("SparesTable", 302);
      sparesFilter     := Descendant("SparesFilter", 303);
      sFilterText      := Descendant("SFilterText", 304);
      costsTab         := Child("CostTab", 400);
      costLabel        := Descendant("CostLabel", 401);
      costTable        := Descendant("CostTable", 402);
      costFilter       := Descendant("CostFilter", 403);
      bcFilterText     := Descendant("BCFilterText", 404);
      blockAnalTab     := Child("BlockAnalTab", 500);
      blockAnalLabel   := Descendant("BlockAnalLabel", 501);
      blockAnalTable   := Descendant("BlockAnalTable", 502);
      blockAnalFilter  := Descendant("BlockAnalFilter", 503);
      bFilterText      := Descendant("BFilterText", 504);
      nodeAnalTab      := Child("NodeAnalTab", 600);
      nodeAnalLabel    := Descendant("NodeAnalLabel", 601);
      nodeAnalTable    := Descendant("NodeAnalTable", 602);
      nodeAnalFilter   := Descendant("NodeAnalFilter", 603);
      nFilterText      := Descendant("NFilterText", 604);
      eventAnalTab     := Child("EventAnalTab", 700);
      eventAnalLabel   := Descendant("EventAnalLabel", 701);
      eventAnalTable   := Descendant("EventAnalTable", 702);
      eventAnalFilter  := Descendant("EventAnalFilter", 703);
      eFilterText      := Descendant("EFilterText", 704);
      hierAnalTab      := Child("HierAnalTab", 800);
      hierAnalLabel    := Descendant("HierAnalLabel", 801);
      hierAnalTable    := Descendant("HierAnalTable", 802);
      capTab           := Child("CapTab", 900);
      capLabel         := Descendant("CapLabel", 901);
      capOutTable      := Descendant("CapOutTable", 902);
      capFilter        := Descendant("CapFilter", 903);
      cFilterText      := Descendant("CFilterText", 904);
      oldSFilter := "All";
      oldBCFilter := "All";
      IF totalHiers > 0
         NEW(hierList, 1..totalHiers+3);
         InitHierFilter(hierList);
         ASK sparesFilter TO SetOptions(hierList);
         ASK sparesFilter TO SetText(hierList[1]);
         ASK costFilter TO SetOptions(hierList);
         ASK costFilter TO SetText(hierList[1]);
         ASK blockAnalFilter TO SetOptions(hierList);
         ASK blockAnalFilter TO SetText(hierList[1]);
         ASK nodeAnalFilter TO SetOptions(hierList);
         ASK nodeAnalFilter TO SetText(hierList[1]);
         ASK eventAnalFilter TO SetOptions(hierList);
         ASK eventAnalFilter TO SetText(hierList[1]);
         ASK capFilter TO SetOptions(hierList);
         ASK capFilter TO SetText(hierList[1]);
      ELSE
         IF NOT printing
            Draw;
            ASK sparesFilter    TO Deactivate;
            ASK costFilter      TO Deactivate;
            ASK blockAnalFilter TO Deactivate;
            ASK nodeAnalFilter  TO Deactivate;
            ASK eventAnalFilter TO Deactivate;
            ASK capFilter       TO Deactivate;
            ASK sFilterText  TO Deactivate;
            ASK bcFilterText TO Deactivate;
            ASK bFilterText  TO Deactivate;
            ASK nFilterText  TO Deactivate;
            ASK eFilterText  TO Deactivate;
            ASK cFilterText  TO Deactivate;
         END IF;
      END IF; 
      FillResultsTable(printing);
      IF (totalBlocks = 0) OR  (totalBlocks=totalEvents)
         ASK sparingTab TO SetHidden(TRUE);
      ELSE
         SpareSort(1,"A");
         sort3 := "1A"; 
      END IF;
      IF (BlockCostArray = NILARRAY) OR (totalBlocks=0)
         ASK costsTab TO SetHidden(TRUE);
      ELSE
         CostSort(1,"A");
         sort1 := "1A";
      END IF;
      IF BlockArray = NILARRAY
         ASK blockAnalTab TO SetHidden(TRUE);
      ELSE
         BlockSort(1,"A"); 
         sort4 := "1A";
      END IF;  
      IF NodeArray = NILARRAY
         ASK nodeAnalTab TO SetHidden(TRUE);
      ELSE
         NodeSort(1,"A");
         sort4 := "1A";
      END IF;     
      IF EventArray = NILARRAY
         ASK eventAnalTab TO SetHidden(TRUE);
      ELSE
         EventSort(1,"A"); 
         sort4 := "1A";
      END IF;  
      IF HierArray = NILARRAY
         ASK hierAnalTab TO SetHidden(TRUE);
      ELSE
         HierSort(1,"A"); 
         sort4 := "1A";
      END IF;  
      IF CapacityArray = NILARRAY
         ASK capTab TO SetHidden(TRUE);  
      ELSE
         CapSort(1,"A");
         sort2 := "1A";
      END IF;
      IF NOT printing
         Draw;
      END IF;
   END METHOD; {InitSelf}

   ASK METHOD BeSelected;
   VAR
      objClicked             : GraphicVObj;
      table                  : TableObj;    
      printedOne             : BOOLEAN;
      hiercount              : INTEGER;
   BEGIN
      objClicked := LastPicked;
      CASE objClicked.Id
         WHEN 100: {Summary Table}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 190 {Print Button}
               IF compileType <> "demo"
                  PrintSummaryOutput(resultsTable); 
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 200: {Logistics Table}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 290 {Print Button}
               IF compileType <> "demo"
                  PrintLogisticsOutput(logisticsTable); 
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 300: {Sparing Table}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 302
               IF sparesTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort3)) = sparesTable.SelectedColumn
                     IF SUBSTR(2,2,sort3) = "A"
                        SpareSort(sparesTable.SelectedColumn, "D");
                        sort3 := INTTOSTR(sparesTable.SelectedColumn)+"D";
                     ELSE
                        SpareSort(sparesTable.SelectedColumn, "A");
                        sort3 := INTTOSTR(sparesTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     SpareSort(sparesTable.SelectedColumn, "A");
                     sort3 := INTTOSTR(sparesTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 303
               IF ((sparesFilter.Text() = "--------------------------------------") 
                   OR (sparesFilter.Text() = oldSFilter))
                  ASK sparesFilter TO SetText(oldSFilter);
               ELSE
                  HierFilter("sparesOut", sparesTable, SparesArray, STRTOINT(SparesArray[0,1]), 7,
                         sparesFilter.Text(), FALSE, displaySArray, hiercount);
                  oldSFilter := sparesFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 390 {Print Button}
               IF compileType <> "demo"
                  PrintSparingOutput(sparesTable); 
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 400: {Block Costs Table} 
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 402
               IF costTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort1)) = costTable.SelectedColumn
                     IF SUBSTR(2,2,sort1) = "A"
                        CostSort(costTable.SelectedColumn, "D");
                        sort1 := INTTOSTR(costTable.SelectedColumn)+"D";
                     ELSE
                        CostSort(costTable.SelectedColumn, "A");
                        sort1 := INTTOSTR(costTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     CostSort(costTable.SelectedColumn, "A");
                     sort1 := INTTOSTR(costTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 403
               IF ((costFilter.Text() = "--------------------------------------") 
                   OR (costFilter.Text() = oldBCFilter))
                  ASK costFilter TO SetText(oldBCFilter);
               ELSE
                  HierFilter("blockcostOut", costTable, BlockCostArray, HIGH(BlockCostArray), 7,
                         costFilter.Text(), FALSE, displayBCArray, hiercount);
                  oldBCFilter := costFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 490 {Print Button}
               IF compileType <> "demo"
                  PrintCostOutput(costTable); 
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 500: {Block Analysis Table}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 502
               IF blockAnalTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort4)) = blockAnalTable.SelectedColumn
                     IF SUBSTR(2,2,sort4) = "A"
                        BlockSort(blockAnalTable.SelectedColumn, "D");
                        sort4 := INTTOSTR(blockAnalTable.SelectedColumn)+"D";
                     ELSE
                        BlockSort(blockAnalTable.SelectedColumn, "A");
                        sort4 := INTTOSTR(blockAnalTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     BlockSort(blockAnalTable.SelectedColumn, "A");
                     sort4 := INTTOSTR(blockAnalTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 503
               IF ((blockAnalFilter.Text() = "--------------------------------------") 
                   OR (blockAnalFilter.Text() = oldBFilter))
                  ASK blockAnalFilter TO SetText(oldBFilter);
               ELSE
                  HierFilter("blockanalOut", blockAnalTable, BlockArray, HIGH(BlockArray), 19,
                         blockAnalFilter.Text(), FALSE, displayBArray, hiercount);
                  oldBFilter := blockAnalFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 590 {Print Button}
               IF compileType <> "demo"
                  PrintAnalOutput(blockAnalTable, "Block"); 
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 600: {Node Analysis Table}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 602
               IF nodeAnalTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort4)) = nodeAnalTable.SelectedColumn
                     IF SUBSTR(2,2,sort4) = "A"
                        NodeSort(nodeAnalTable.SelectedColumn, "D");
                        sort4 := INTTOSTR(nodeAnalTable.SelectedColumn)+"D";
                     ELSE
                        NodeSort(nodeAnalTable.SelectedColumn, "A");
                        sort4 := INTTOSTR(nodeAnalTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     NodeSort(nodeAnalTable.SelectedColumn, "A");
                     sort4 := INTTOSTR(nodeAnalTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 603
               IF ((nodeAnalFilter.Text() = "--------------------------------------") 
                   OR (nodeAnalFilter.Text() = oldNFilter))
                  ASK nodeAnalFilter TO SetText(oldNFilter);
               ELSE
                  HierFilter("nodeanalOut", nodeAnalTable, NodeArray, HIGH(NodeArray), 19,
                         nodeAnalFilter.Text(), FALSE, displayNArray, hiercount);
                  oldNFilter := nodeAnalFilter.Text();
               END IF;
             ELSIF lastClicked.Id = 690 {Print Button}
               IF compileType <> "demo"
                  PrintAnalOutput(nodeAnalTable, "Node"); 
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 700: {Event Analysis Table}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 702
               IF eventAnalTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort4)) = eventAnalTable.SelectedColumn
                     IF SUBSTR(2,2,sort4) = "A"
                        EventSort(eventAnalTable.SelectedColumn, "D");
                        sort4 := INTTOSTR(eventAnalTable.SelectedColumn)+"D";
                     ELSE
                        EventSort(eventAnalTable.SelectedColumn, "A");
                        sort4 := INTTOSTR(eventAnalTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     EventSort(eventAnalTable.SelectedColumn, "A");
                     sort4 := INTTOSTR(eventAnalTable.SelectedColumn)+"A";
                  END IF;
               END IF; 
            ELSIF lastClicked.Id = 703
               IF ((eventAnalFilter.Text() = "--------------------------------------") 
                   OR (eventAnalFilter.Text() = oldEFilter))
                  ASK eventAnalFilter TO SetText(oldEFilter);
               ELSE
                  HierFilter("eventanalOut", eventAnalTable, EventArray, HIGH(EventArray), 19,
                         eventAnalFilter.Text(), FALSE, displayEArray, hiercount);
                  oldEFilter := eventAnalFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 790 {Print Button}
               IF compileType <> "demo"
                  PrintAnalOutput(eventAnalTable, "Event"); 
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 800: {Hierarchy Analysis Table}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 802
               IF hierAnalTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort4)) = hierAnalTable.SelectedColumn
                     IF SUBSTR(2,2,sort4) = "A"
                        HierSort(hierAnalTable.SelectedColumn, "D");
                        sort4 := INTTOSTR(hierAnalTable.SelectedColumn)+"D";
                     ELSE
                        HierSort(hierAnalTable.SelectedColumn, "A");
                        sort4 := INTTOSTR(hierAnalTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     HierSort(hierAnalTable.SelectedColumn, "A");
                     sort4 := INTTOSTR(hierAnalTable.SelectedColumn)+"A";
                  END IF;
               END IF; 
            ELSIF lastClicked.Id = 890 {Print Button}
               IF compileType <> "demo"
                  PrintAnalOutput(hierAnalTable, "Hier"); 
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         WHEN 900: {Capacity Table}
            lastClicked := objClicked.LastPicked;
            IF lastClicked.Id = 902
               IF capOutTable.SelectedRow = 0
                  IF STRTOINT(SUBSTR(1,1,sort2)) = capOutTable.SelectedColumn
                     IF SUBSTR(2,2,sort2) = "A"
                        CapSort(capOutTable.SelectedColumn, "D");
                        sort2 := INTTOSTR(capOutTable.SelectedColumn)+"D";
                     ELSE
                        CapSort(capOutTable.SelectedColumn, "A");
                        sort2 := INTTOSTR(capOutTable.SelectedColumn)+"A";
                     END IF;   
                  ELSE
                     CapSort(capOutTable.SelectedColumn, "A");
                     sort2 := INTTOSTR(capOutTable.SelectedColumn)+"A";
                  END IF;
               END IF;
            ELSIF lastClicked.Id = 903
               IF ((capFilter.Text() = "--------------------------------------") 
                   OR (capFilter.Text() = oldCFilter))
                  ASK capFilter TO SetText(oldCFilter);
               ELSE
                  HierFilter("capacityOut", capOutTable, CapacityArray, HIGH(CapacityArray), 11,
                         capFilter.Text(), FALSE, displayCArray, hiercount);
                  oldCFilter := capFilter.Text();
               END IF;
            ELSIF lastClicked.Id = 990 {Print Button}
               IF compileType <> "demo"
                  PrintCapacityOutput(capOutTable); 
               ELSE
                  NEW(message, 1..1);
                  message[1] := "The demo version has no print capability     ";
                  result := SendAlert(message, FALSE, FALSE, TRUE);
                  DISPOSE(message);
               END IF;
            END IF;
         OTHERWISE
      END CASE;
      Update;
      INHERITED BeSelected;  {Call the inherited method of a HelpBoxObj}
   END METHOD;

   ASK METHOD CostSort(IN sortCol : INTEGER; IN sortDir : STRING);
   VAR
      numPlaced,i, j, pos            : INTEGER;
      cell,currentBlock,blockSparing : STRING;
      block                          : RBDBlockObj;
      pool                           : SparePoolObj;
      currentNum, cellNum            : REAL;
   BEGIN
      pos := 1;
      numPlaced:=1;
      FOR i := 1 TO HIGH(displayBCArray)
         CASE sortCol
            WHEN 1:
               currentBlock := LOWER(displayBCArray[i,1]);
            WHEN 2:
               currentNum := COSTTOREAL(displayBCArray[i,2]);
            WHEN 3:
               currentNum := COSTTOREAL(displayBCArray[i,3]);
            WHEN 4: {Sparing costs}
               IF displayBCArray[i,4] = "N/A"
                  currentNum := -1.;
               ELSE
                  currentNum := COSTTOREAL(displayBCArray[i,4]);
               END IF;
            WHEN 5:
               currentNum := COSTTOREAL(displayBCArray[i,5]);
            WHEN 6:
               currentNum := COSTTOREAL(displayBCArray[i,6]);
            OTHERWISE
         END CASE;
         IF sortCol=1
            cell := LOWER(costTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentBlock > cell))
                  CostInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(costTable.Text(sortCol, pos));
                  WHILE(currentBlock > cell)
                     INC(pos);
                     cell := LOWER(costTable.Text(sortCol, pos));
                  END WHILE;
                  CostInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentBlock < cell))
                  CostInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(costTable.Text(sortCol, pos));
                  WHILE(currentBlock < cell)
                     INC(pos);
                     cell := LOWER(costTable.Text(sortCol, pos));
                  END WHILE;
                  CostInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            cellNum := COSTTOREAL(costTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  CostInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF (costTable.Text(sortCol, pos) = "N/A")
                     cellNum := -1.;
                  ELSE
                     cellNum := COSTTOREAL(costTable.Text(sortCol, pos));
                  END IF;
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     IF (costTable.Text(sortCol, pos) = "N/A")
                        cellNum := -1.;
                     ELSE
                        cellNum := COSTTOREAL(costTable.Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  CostInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  CostInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  IF (costTable.Text(sortCol, pos) = "N/A")
                     cellNum := -1.;
                  ELSE
                     cellNum := COSTTOREAL(costTable.Text(sortCol, pos));
                  END IF;
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     IF (costTable.Text(sortCol, pos) = "N/A")
                        cellNum := -1.;
                     ELSE   
                        cellNum := COSTTOREAL(costTable.Text(sortCol, pos));
                     END IF;
                  END WHILE;
                  CostInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {CostSort}
            
   ASK METHOD CostInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j     : INTEGER;
      text2 : STRING;
      pool  : SparePoolObj;
      block : RBDBlockObj;
   BEGIN
      block := ASK root Child("RBDBlock", i);
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK costTable Text(1, j-1);
         ASK costTable TO SetText(text2, 1, j);
         text2 := ASK costTable Text(2, j-1);
         ASK costTable TO SetText(text2, 2, j);
         text2 := ASK costTable Text(3, j-1);
         ASK costTable TO SetText(text2, 3, j);
         text2 := ASK costTable Text(4, j-1);
         ASK costTable TO SetText(text2, 4, j);
         text2 := ASK costTable Text(5, j-1);
         ASK costTable TO SetText(text2, 5, j);
         text2 := ASK costTable Text(6, j-1);
         ASK costTable TO SetText(text2, 6, j);
      END FOR;         
      ASK costTable TO SetText(displayBCArray[i, 1], 1, pos);
      ASK costTable TO SetText(displayBCArray[i, 2], 2, pos);
      ASK costTable TO SetText(displayBCArray[i, 3], 3, pos);
      ASK costTable TO SetText(displayBCArray[i, 4], 4, pos);
      ASK costTable TO SetText(displayBCArray[i, 5], 5, pos);
      ASK costTable TO SetText(displayBCArray[i, 6], 6, pos);
   END METHOD;{CostInsert}

   ASK METHOD SpareSort(IN sortCol : INTEGER;
                        IN sortDir : STRING);
   VAR
      numPlaced,i, j, pos            : INTEGER;
      cell,currentBlock,blockSparing : STRING;
      block                          : RBDBlockObj;
      pool                           : SparePoolObj;
      currentNum, cellNum            : REAL;
   BEGIN
      pos := 1;
      numPlaced:=1;
      FOR i := 1 TO HIGH(displaySArray)
         CASE sortCol
            WHEN 1:
               currentBlock := LOWER(displaySArray[i,1]);
            WHEN 2:
               currentBlock := LOWER(displaySArray[i,2]);
            WHEN 3:                            
               currentNum := STRTOREAL(displaySArray[i,3]);
            WHEN 4:
               currentNum := STRTOREAL(displaySArray[i,4]);
            WHEN 5:
               currentNum := STRTOREAL(displaySArray[i,5]);
            WHEN 6:
               currentNum := STRTOREAL(displaySArray[i,6]);
            OTHERWISE
         END CASE;
         IF sortCol<=2
            cell := LOWER(sparesTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentBlock > cell))
                  SpareInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(sparesTable.Text(sortCol, pos));
                  WHILE(currentBlock > cell)
                     INC(pos);
                     cell := LOWER(sparesTable.Text(sortCol, pos));
                  END WHILE;
                  SpareInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentBlock < cell))
                  SpareInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(sparesTable.Text(sortCol, pos));
                  WHILE(currentBlock < cell)
                     INC(pos);
                     cell := LOWER(sparesTable.Text(sortCol, pos));
                  END WHILE;
                  SpareInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            cellNum := STRTOREAL(ASK sparesTable Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  SpareInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK sparesTable Text(sortCol, pos));
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK sparesTable Text(sortCol, pos));
                  END WHILE;
                  SpareInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  SpareInsert(i, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK sparesTable Text(sortCol, pos));
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK sparesTable Text(sortCol, pos));
                  END WHILE;
                  SpareInsert(i, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {SpareSort}
            
   ASK METHOD SpareInsert(IN i, pos, numPlaced : INTEGER);
   VAR
      j     : INTEGER;
      text2 : STRING;
      pool  : SparePoolObj;
      block : RBDBlockObj;
   BEGIN
      block := ASK root Child("RBDBlock", i);
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK sparesTable Text(1, j-1);
         ASK sparesTable TO SetText(text2, 1, j);
         text2 := ASK sparesTable Text(2, j-1);
         ASK sparesTable TO SetText(text2, 2, j);
         text2 := ASK sparesTable Text(3, j-1);
         ASK sparesTable TO SetText(text2, 3, j);
         text2 := ASK sparesTable Text(4, j-1);
         ASK sparesTable TO SetText(text2, 4, j);
         text2 := ASK sparesTable Text(5, j-1);
         ASK sparesTable TO SetText(text2, 5, j);
         text2 := ASK sparesTable Text(6, j-1);
         ASK sparesTable TO SetText(text2, 6, j);
      END FOR;         
      ASK sparesTable TO SetText(displaySArray[i,1], 1, pos);
      ASK sparesTable TO SetText(displaySArray[i,2], 2, pos);
      ASK sparesTable TO SetText(displaySArray[i,3], 3, pos);
      ASK sparesTable TO SetText(displaySArray[i,4], 4, pos);
      ASK sparesTable TO SetText(displaySArray[i,5], 5, pos);
      ASK sparesTable TO SetText(displaySArray[i,6], 6, pos);
   END METHOD;{SpareInsert}

   ASK METHOD NodeSort(IN sortCol : INTEGER;
                        IN sortDir : STRING);
   VAR
      numPlaced,i, j, k, pos         : INTEGER;
      cell,currentBlock,blockSparing : STRING;
      block                          : RBDBlockObj;
      pool                           : SparePoolObj;
      currentNum, cellNum            : REAL;
      tempNode                       : RBDNodeObj;
   BEGIN
      pos := 1;
      numPlaced:=1;
      IF sortCol = 2
         sortCol := 4;
      END IF;
      FOR k := 1 TO HIGH(displayNArray)
         {tempNode := ASK root Child("RBDNode", STRTOINT(displayNArray[k,18]));
         IF tempNode.reportNodeAnal
            INC(i);}
            CASE sortCol
               WHEN 1: {name}
                  currentBlock := LOWER(displayNArray[k,1]); 
               WHEN 2: {color}
                  currentBlock := LOWER(displayNArray[k,2]);
               WHEN 3: {min}                           
                  currentNum := STRTOREAL(displayNArray[k,3]); 
               WHEN 4: {mean}
                  currentNum := STRTOREAL(displayNArray[k,4]); 
               WHEN 5: {max}
                  currentNum := STRTOREAL(displayNArray[k,5]); 
               WHEN 6: {stdev}
                  currentNum := STRTOREAL(displayNArray[k,6]); 
               WHEN 7: {SEM}
                  currentNum := STRTOREAL(displayNArray[k,7]); 
               OTHERWISE
            END CASE;
            IF (sortCol=1) OR (sortCol=2)
               cell := LOWER(nodeAnalTable.Text(sortCol, numPlaced-1));
               IF sortDir="A"
                  IF ((numPlaced = 1) OR (currentBlock > cell))
                     NodeInsert(i, k, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     cell := LOWER(nodeAnalTable.Text(sortCol, pos));
                     WHILE(currentBlock > cell)
                        INC(pos);
                        cell := LOWER(nodeAnalTable.Text(sortCol, pos));
                     END WHILE;
                     NodeInsert(i, k, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               ELSE            
                  IF ((numPlaced = 1) OR (currentBlock < cell))
                     NodeInsert(i, k, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     cell := LOWER(nodeAnalTable.Text(sortCol, pos));
                     WHILE(currentBlock < cell)
                        INC(pos);
                        cell := LOWER(nodeAnalTable.Text(sortCol, pos));
                     END WHILE;
                     NodeInsert(i, k, pos, numPlaced);
                    INC(numPlaced);
                  END IF;
               END IF;
            ELSE
               cellNum := STRTOREAL(ASK nodeAnalTable Text(sortCol, numPlaced-1));
               IF sortDir="A"
                  IF ((numPlaced = 1) OR (currentNum > cellNum))
                     NodeInsert(i, k, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     cellNum := STRTOREAL(ASK nodeAnalTable Text(sortCol, pos));
                     WHILE(currentNum > cellNum)
                        INC(pos);
                        cellNum := STRTOREAL(ASK nodeAnalTable Text(sortCol, pos));
                     END WHILE;
                     NodeInsert(i, k, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               ELSE
                  IF ((numPlaced = 1) OR (currentNum < cellNum))
                     NodeInsert(i, k, numPlaced, numPlaced);
                     INC(numPlaced);
                  ELSE
                     pos := 1;
                     cellNum := STRTOREAL(ASK nodeAnalTable Text(sortCol, pos));
                     WHILE(currentNum < cellNum)
                        INC(pos);
                        cellNum := STRTOREAL(ASK nodeAnalTable Text(sortCol, pos));
                     END WHILE;
                     NodeInsert(i, k, pos, numPlaced);
                     INC(numPlaced);
                  END IF;
               END IF;
            {END IF;}
         END IF;
      END FOR;
   END METHOD; {NodeSort}
            
   ASK METHOD NodeInsert(IN i, k, pos, numPlaced : INTEGER); 
   VAR
      j     : INTEGER;
      text2 : STRING;
      pool  : SparePoolObj;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK nodeAnalTable Text(1, j-1);
         ASK nodeAnalTable TO SetText(text2, 1, j);
         text2 := ASK nodeAnalTable Text(2, j-1);
         ASK nodeAnalTable TO SetText(text2, 2, j);
         text2 := ASK nodeAnalTable Text(3, j-1);
         ASK nodeAnalTable TO SetText(text2, 3, j);
         text2 := ASK nodeAnalTable Text(4, j-1);
         ASK nodeAnalTable TO SetText(text2, 4, j);
         text2 := ASK nodeAnalTable Text(5, j-1);
         ASK nodeAnalTable TO SetText(text2, 5, j);
         text2 := ASK nodeAnalTable Text(6, j-1);
         ASK nodeAnalTable TO SetText(text2, 6, j);
         text2 := ASK nodeAnalTable Text(7, j-1);
         ASK nodeAnalTable TO SetText(text2, 7, j);
      END FOR;         
      ASK nodeAnalTable TO SetText(displayNArray[k,1], 1, pos);
      ASK nodeAnalTable TO SetText(displayNArray[k,2], 2, pos);
      ASK nodeAnalTable TO SetText(displayNArray[k,3], 3, pos);
      ASK nodeAnalTable TO SetText(displayNArray[k,4], 4, pos);
      ASK nodeAnalTable TO SetText(displayNArray[k,5], 5, pos);
      ASK nodeAnalTable TO SetText(displayNArray[k,6], 6, pos);
      ASK nodeAnalTable TO SetText(displayNArray[k,7], 7, pos);
   END METHOD;{NodeInsert}

   ASK METHOD BlockSort(IN sortCol : INTEGER;
                        IN sortDir : STRING);
   VAR
      numPlaced,i, j, k, pos         : INTEGER;
      cell,currentBlock,blockSparing : STRING;
      block                          : RBDBlockObj;
      pool                           : SparePoolObj;
      currentNum, cellNum            : REAL;
   BEGIN
      pos := 1;
      numPlaced:=1;
      IF sortCol = 2
         sortCol := 4;
      END IF;
      FOR k := 1 TO HIGH(displayBArray)
         INC(i);
         CASE sortCol
            WHEN 1: {name}
               currentBlock := LOWER(displayBArray[k,1]); 
            WHEN 2: {color}
               currentBlock := LOWER(displayBArray[k,2]);
            WHEN 3: {min}                           
               currentNum := STRTOREAL(displayBArray[k,3]); 
            WHEN 4: {mean}
               currentNum := STRTOREAL(displayBArray[k,4]); 
            WHEN 5: {max}
               currentNum := STRTOREAL(displayBArray[k,5]); 
            WHEN 6: {stdev}
               currentNum := STRTOREAL(displayBArray[k,6]); 
            WHEN 7: {SEM}
               currentNum := STRTOREAL(displayBArray[k,7]); 
            OTHERWISE
         END CASE;
         IF (sortCol=1) OR (sortCol=2)
            cell := LOWER(blockAnalTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentBlock > cell))
                  BlockInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(blockAnalTable.Text(sortCol, pos));
                  WHILE(currentBlock > cell)
                     INC(pos);
                     cell := LOWER(blockAnalTable.Text(sortCol, pos));
                  END WHILE;
                  BlockInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentBlock < cell))
                  BlockInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(blockAnalTable.Text(sortCol, pos));
                  WHILE(currentBlock < cell)
                     INC(pos);
                     cell := LOWER(blockAnalTable.Text(sortCol, pos));
                  END WHILE;
                  BlockInsert(i, k, pos, numPlaced);
                 INC(numPlaced);
               END IF;
            END IF;
         ELSE
            cellNum := STRTOREAL(ASK blockAnalTable Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  BlockInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK blockAnalTable Text(sortCol, pos));
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK blockAnalTable Text(sortCol, pos));
                  END WHILE;
                  BlockInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  BlockInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK blockAnalTable Text(sortCol, pos));
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK blockAnalTable Text(sortCol, pos));
                  END WHILE;
                  BlockInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {BlockSort}
            
   ASK METHOD BlockInsert(IN i, k, pos, numPlaced : INTEGER); 
   VAR
      j     : INTEGER;
      text2 : STRING;
      pool  : SparePoolObj;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK blockAnalTable Text(1, j-1);
         ASK blockAnalTable TO SetText(text2, 1, j);
         text2 := ASK blockAnalTable Text(2, j-1);
         ASK blockAnalTable TO SetText(text2, 2, j);
         text2 := ASK blockAnalTable Text(3, j-1);
         ASK blockAnalTable TO SetText(text2, 3, j);
         text2 := ASK blockAnalTable Text(4, j-1);
         ASK blockAnalTable TO SetText(text2, 4, j);
         text2 := ASK blockAnalTable Text(5, j-1);
         ASK blockAnalTable TO SetText(text2, 5, j);
         text2 := ASK blockAnalTable Text(6, j-1);
         ASK blockAnalTable TO SetText(text2, 6, j);
         text2 := ASK blockAnalTable Text(7, j-1);
         ASK blockAnalTable TO SetText(text2, 7, j);
      END FOR;         
      ASK blockAnalTable TO SetText(displayBArray[k,1], 1, pos);
      ASK blockAnalTable TO SetText(displayBArray[k,2], 2, pos);
      ASK blockAnalTable TO SetText(displayBArray[k,3], 3, pos);
      ASK blockAnalTable TO SetText(displayBArray[k,4], 4, pos);
      ASK blockAnalTable TO SetText(displayBArray[k,5], 5, pos);
      ASK blockAnalTable TO SetText(displayBArray[k,6], 6, pos);
      ASK blockAnalTable TO SetText(displayBArray[k,7], 7, pos);
   END METHOD;{BlockInsert}

   ASK METHOD EventSort(IN sortCol : INTEGER;
                        IN sortDir : STRING);
   VAR
      numPlaced,i, j, k, pos         : INTEGER;
      cell,currentEvent              : STRING;
      event                          : RBDEventObj;
      currentNum, cellNum            : REAL;
   BEGIN
      pos := 1;
      numPlaced:=1;
      IF sortCol = 2
         sortCol := 4;
      END IF;
      FOR k := 1 TO HIGH(displayEArray)
         INC(i);
         CASE sortCol
            WHEN 1: {name}
               currentEvent := LOWER(displayEArray[k,1]); 
            WHEN 2: {color}
               currentEvent := LOWER(displayEArray[k,2]);
            WHEN 3: {min}                           
               currentNum := STRTOREAL(displayEArray[k,3]); 
            WHEN 4: {mean}
               currentNum := STRTOREAL(displayEArray[k,4]); 
            WHEN 5: {max}
               currentNum := STRTOREAL(displayEArray[k,5]); 
            WHEN 6: {stdev}
               currentNum := STRTOREAL(displayEArray[k,6]); 
            WHEN 7: {SEM}
               currentNum := STRTOREAL(displayEArray[k,7]); 
            OTHERWISE
         END CASE;
         IF (sortCol=1) OR (sortCol=2)
            cell := LOWER(eventAnalTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentEvent > cell))
                  EventInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(eventAnalTable.Text(sortCol, pos));
                  WHILE(currentEvent > cell)
                     INC(pos);
                     cell := LOWER(eventAnalTable.Text(sortCol, pos));
                  END WHILE;
                  EventInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentEvent < cell))
                  EventInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(eventAnalTable.Text(sortCol, pos));
                  WHILE(currentEvent < cell)
                     INC(pos);
                     cell := LOWER(eventAnalTable.Text(sortCol, pos));
                  END WHILE;
                  EventInsert(i, k, pos, numPlaced);
                 INC(numPlaced);
               END IF;
            END IF;
         ELSE
            cellNum := STRTOREAL(ASK eventAnalTable Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  EventInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK eventAnalTable Text(sortCol, pos));
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK eventAnalTable Text(sortCol, pos));
                  END WHILE;
                  EventInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  EventInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK eventAnalTable Text(sortCol, pos));
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK eventAnalTable Text(sortCol, pos));
                  END WHILE;
                  EventInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {EventSort}
            
   ASK METHOD EventInsert(IN i, k, pos, numPlaced : INTEGER); 
   VAR
      j     : INTEGER;
      text2 : STRING;
      pool  : SparePoolObj;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK eventAnalTable Text(1, j-1);
         ASK eventAnalTable TO SetText(text2, 1, j);
         text2 := ASK eventAnalTable Text(2, j-1);
         ASK eventAnalTable TO SetText(text2, 2, j);
         text2 := ASK eventAnalTable Text(3, j-1);
         ASK eventAnalTable TO SetText(text2, 3, j);
         text2 := ASK eventAnalTable Text(4, j-1);
         ASK eventAnalTable TO SetText(text2, 4, j);
         text2 := ASK eventAnalTable Text(5, j-1);
         ASK eventAnalTable TO SetText(text2, 5, j);
         text2 := ASK eventAnalTable Text(6, j-1);
         ASK eventAnalTable TO SetText(text2, 6, j);
         text2 := ASK eventAnalTable Text(7, j-1);
         ASK eventAnalTable TO SetText(text2, 7, j);
      END FOR;         
      ASK eventAnalTable TO SetText(displayEArray[k,1], 1, pos);
      ASK eventAnalTable TO SetText(displayEArray[k,2], 2, pos);
      ASK eventAnalTable TO SetText(displayEArray[k,3], 3, pos);
      ASK eventAnalTable TO SetText(displayEArray[k,4], 4, pos);
      ASK eventAnalTable TO SetText(displayEArray[k,5], 5, pos);
      ASK eventAnalTable TO SetText(displayEArray[k,6], 6, pos);
      ASK eventAnalTable TO SetText(displayEArray[k,7], 7, pos);
   END METHOD;{EventInsert}

   ASK METHOD HierSort(IN sortCol : INTEGER;
                        IN sortDir : STRING);
   VAR
      numPlaced,i, j, k, pos         : INTEGER;
      cell,currentHier               : STRING;
      hier                           : RBDHierObj;
      currentNum, cellNum            : REAL;
   BEGIN
      pos := 1;
      numPlaced:=1;
      IF sortCol = 2
         sortCol := 4;
      END IF;
      FOR k := 1 TO HIGH(displayHArray)
         INC(i);
         CASE sortCol
            WHEN 1: {name}
               currentHier := LOWER(displayHArray[k,1]); 
            WHEN 2: {color}
               currentHier := LOWER(displayHArray[k,2]);
            WHEN 3: {min}                           
               currentNum := STRTOREAL(displayHArray[k,3]); 
            WHEN 4: {mean}
               currentNum := STRTOREAL(displayHArray[k,4]); 
            WHEN 5: {max}
               currentNum := STRTOREAL(displayHArray[k,5]); 
            WHEN 6: {stdev}
               currentNum := STRTOREAL(displayHArray[k,6]); 
            WHEN 7: {SEM}
               currentNum := STRTOREAL(displayHArray[k,7]); 
            OTHERWISE
         END CASE;
         IF (sortCol=1) OR (sortCol=2)
            cell := LOWER(hierAnalTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentHier > cell))
                  HierInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(hierAnalTable.Text(sortCol, pos));
                  WHILE(currentHier > cell)
                     INC(pos);
                     cell := LOWER(hierAnalTable.Text(sortCol, pos));
                  END WHILE;
                  HierInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentHier < cell))
                  HierInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(hierAnalTable.Text(sortCol, pos));
                  WHILE(currentHier < cell)
                     INC(pos);
                     cell := LOWER(hierAnalTable.Text(sortCol, pos));
                  END WHILE;
                  HierInsert(i, k, pos, numPlaced);
                 INC(numPlaced);
               END IF;
            END IF;
         ELSE
            cellNum := STRTOREAL(ASK hierAnalTable Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  HierInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK hierAnalTable Text(sortCol, pos));
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK hierAnalTable Text(sortCol, pos));
                  END WHILE;
                  HierInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  HierInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK hierAnalTable Text(sortCol, pos));
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK hierAnalTable Text(sortCol, pos));
                  END WHILE;
                  HierInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {HierSort}
            
   ASK METHOD HierInsert(IN i, k, pos, numPlaced : INTEGER); 
   VAR
      j     : INTEGER;
      text2 : STRING;
      pool  : SparePoolObj;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK hierAnalTable Text(1, j-1);
         ASK hierAnalTable TO SetText(text2, 1, j);
         text2 := ASK hierAnalTable Text(2, j-1);
         ASK hierAnalTable TO SetText(text2, 2, j);
         text2 := ASK hierAnalTable Text(3, j-1);
         ASK hierAnalTable TO SetText(text2, 3, j);
         text2 := ASK hierAnalTable Text(4, j-1);
         ASK hierAnalTable TO SetText(text2, 4, j);
         text2 := ASK hierAnalTable Text(5, j-1);
         ASK hierAnalTable TO SetText(text2, 5, j);
         text2 := ASK hierAnalTable Text(6, j-1);
         ASK hierAnalTable TO SetText(text2, 6, j);
         text2 := ASK hierAnalTable Text(7, j-1);
         ASK hierAnalTable TO SetText(text2, 7, j);
      END FOR;         
      ASK hierAnalTable TO SetText(displayHArray[k,1], 1, pos);
      ASK hierAnalTable TO SetText(displayHArray[k,2], 2, pos);
      ASK hierAnalTable TO SetText(displayHArray[k,3], 3, pos);
      ASK hierAnalTable TO SetText(displayHArray[k,4], 4, pos);
      ASK hierAnalTable TO SetText(displayHArray[k,5], 5, pos);
      ASK hierAnalTable TO SetText(displayHArray[k,6], 6, pos);
      ASK hierAnalTable TO SetText(displayHArray[k,7], 7, pos);
   END METHOD;{HierInsert}

   ASK METHOD CapSort(IN sortCol : INTEGER;
                        IN sortDir : STRING);
   VAR
      numPlaced, i, j, pos, k : INTEGER;
      cell,currentBlock       : STRING;
      tempNode                : RBDNodeObj;
      currentNum, cellNum     : REAL;
   BEGIN
      pos := 1;
      numPlaced:=1;
      FOR k := 1 TO HIGH(displayCArray)
         CASE sortCol
            WHEN 1:
               currentBlock := LOWER(displayCArray[k,1]);
            WHEN 2:                            
               currentNum := STRTOREAL(displayCArray[k,2]);
            WHEN 3:
               currentNum := STRTOREAL(displayCArray[k,3]);
            WHEN 4:
               currentNum := STRTOREAL(displayCArray[k,4]);
            WHEN 5:
               currentNum := STRTOREAL(displayCArray[k,5]);
            WHEN 6:
               currentNum := STRTOREAL(displayCArray[k,6]);
            WHEN 7:
               currentNum := STRTOREAL(displayCArray[k,7]);
            WHEN 8:
               currentNum := STRTOREAL(displayCArray[k,8]);
            WHEN 9:
               currentNum := STRTOREAL(displayCArray[k,9]);
            OTHERWISE
         END CASE;
         IF sortCol=1
            cell := LOWER(capOutTable.Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentBlock > cell))
                  CapInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(capOutTable.Text(sortCol, pos));
                  WHILE(currentBlock > cell)
                     INC(pos);
                     cell := LOWER(capOutTable.Text(sortCol, pos));
                  END WHILE;
                  CapInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE            
               IF ((numPlaced = 1) OR (currentBlock < cell))
                  CapInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cell := LOWER(capOutTable.Text(sortCol, pos));
                  WHILE(currentBlock < cell)
                     INC(pos);
                     cell := LOWER(capOutTable.Text(sortCol, pos));
                  END WHILE;
                  CapInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         ELSE
            cellNum := STRTOREAL(ASK capOutTable Text(sortCol, numPlaced-1));
            IF sortDir="A"
               IF ((numPlaced = 1) OR (currentNum > cellNum))
                  CapInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK capOutTable Text(sortCol, pos));
                  WHILE(currentNum > cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK capOutTable Text(sortCol, pos));
                  END WHILE;
                  CapInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            ELSE
               IF ((numPlaced = 1) OR (currentNum < cellNum))
                  CapInsert(i, k, numPlaced, numPlaced);
                  INC(numPlaced);
               ELSE
                  pos := 1;
                  cellNum := STRTOREAL(ASK capOutTable Text(sortCol, pos));
                  WHILE(currentNum < cellNum)
                     INC(pos);
                     cellNum := STRTOREAL(ASK capOutTable Text(sortCol, pos));
                  END WHILE;
                  CapInsert(i, k, pos, numPlaced);
                  INC(numPlaced);
               END IF;
            END IF;
         END IF;
      END FOR;
   END METHOD; {CapSort}
            
   ASK METHOD CapInsert(IN i, k, pos, numPlaced : INTEGER);
   VAR
      j     : INTEGER;
      text2 : STRING;
   BEGIN
      FOR j := numPlaced DOWNTO (pos+1)
         text2 := ASK capOutTable Text(1, j-1);
         ASK capOutTable TO SetText(text2, 1, j);
         text2 := ASK capOutTable Text(2, j-1);
         ASK capOutTable TO SetText(text2, 2, j);
         text2 := ASK capOutTable Text(3, j-1);
         ASK capOutTable TO SetText(text2, 3, j);
         text2 := ASK capOutTable Text(4, j-1);
         ASK capOutTable TO SetText(text2, 4, j);
         text2 := ASK capOutTable Text(5, j-1);
         ASK capOutTable TO SetText(text2, 5, j);
         text2 := ASK capOutTable Text(6, j-1);
         ASK capOutTable TO SetText(text2, 6, j);
         text2 := ASK capOutTable Text(7, j-1);
         ASK capOutTable TO SetText(text2, 7, j);
         text2 := ASK capOutTable Text(8, j-1);
         ASK capOutTable TO SetText(text2, 8, j);
         text2 := ASK capOutTable Text(9, j-1);
         ASK capOutTable TO SetText(text2, 9, j);
      END FOR;         
      ASK capOutTable TO SetText(displayCArray[k,1], 1, pos);
      ASK capOutTable TO SetText(displayCArray[k,2], 2, pos);
      ASK capOutTable TO SetText(displayCArray[k,3], 3, pos);
      ASK capOutTable TO SetText(displayCArray[k,4], 4, pos);
      ASK capOutTable TO SetText(displayCArray[k,5], 5, pos);
      ASK capOutTable TO SetText(displayCArray[k,6], 6, pos);
      ASK capOutTable TO SetText(displayCArray[k,7], 7, pos);
      ASK capOutTable TO SetText(displayCArray[k,8], 8, pos);
      ASK capOutTable TO SetText(displayCArray[k,9], 9, pos);
   END METHOD;{CapInsert}
END OBJECT; {Tables Out}

OBJECT DataBoxObj;
   ASK METHOD BeSelected;
   VAR
      fileName,pathName,filter,extension    : STRING;      
      dotPosition,checksOut,i,tempPts       : INTEGER;
      checkFile                             : StreamObj;
      numPtsText                            : LabelObj;
      dataTable                             : TableObj;
      objClicked                            : GraphicVObj;
      dataEntryBox                          : ValueBoxObj;
      tempArray                             : realArray;
      deleteButton,getButton,cancelButton,
      acceptButton,helpButton,okButton      : ButtonObj;
   BEGIN
      okButton := Child("OKButton", 0);
      cancelButton := Child("CancelButton", 0);
      deleteButton := Child("DeleteButton", 100);
      dataTable := Child("DataTable", 101);
      getButton := Child("GetButton", 102);
      numPtsText := Child("NumPtsText", 104);
      dataEntryBox := Child("DataEntryBox", 105);
      acceptButton := Child("AcceptButton", 106);
      helpButton := Child("HelpButton", 25);
      objClicked := LastPicked;
      CASE objClicked.Id
         WHEN 100:  {Delete Data Button}
            IF cellSelected
               ASK dataTable TO SetText("", 2, dataTable.SelectedRow);
               IF dataTable.SelectedRow < numDataPoints
                  FOR i := dataTable.SelectedRow TO numDataPoints
                     ASK dataTable TO SetText((ASK dataTable Text(2,i+1)), 2, i);
                  END FOR; 
                  ASK dataEntryBox TO SetValue(STRTOREAL(ASK dataTable Text(dataTable.SelectedColumn,dataTable.SelectedRow)));
                  DEC(numDataPoints);
                  IF numDataPoints = 0
                     ASK deleteButton TO Deactivate;
                  END IF;
                  ASK numPtsText TO SetLabel(INTTOSTR(numDataPoints));
                  cellSelected := TRUE;
               ELSE
                  ASK dataEntryBox TO SetValue(0.);
                  DEC(numDataPoints);
                  IF numDataPoints = 0
                     ASK deleteButton TO Deactivate;
                  END IF;
                  ASK numPtsText TO SetLabel(INTTOSTR(numDataPoints));
                  cellSelected := FALSE;
               END IF;
            END IF;
         WHEN 101:  {Data Table}
            IF (dataTable.SelectedRow <= numDataPoints) AND (dataTable.SelectedRow <> 0) 
               ASK dataTable TO SetSelected(2, dataTable.SelectedRow);
               ASK deleteButton TO Activate;
               ASK dataEntryBox TO SetValue(STRTOREAL(ASK dataTable Text(dataTable.SelectedColumn,dataTable.SelectedRow)));
               cellSelected := TRUE;
            ELSE
               ASK dataTable TO SetSelected(2, numDataPoints+1);
               ASK deleteButton TO Deactivate;
               ASK dataEntryBox TO SetValue(0.);
               cellSelected := FALSE;
            END IF;
         WHEN 102:  {Retrieve Data Button}
            loadCanceled := FALSE;
            ASK getButton TO Deactivate;
            ASK okButton TO Deactivate;
            ASK cancelButton TO Deactivate;
            ASK acceptButton TO Deactivate;
            ASK helpButton TO Deactivate;
            ASK deleteButton TO Deactivate;
            IF numDataPoints > 0
               NEW(message,1..2);
               message[1] := "Data is currently shown in the table.     ";
               message[2] := "Click yes to append to current data, no to overwrite current data?";
               threeWay := TRUE;
               result := SendAlert(message, FALSE, TRUE, FALSE);
               threeWay := FALSE;
               DISPOSE(message);
            END IF;
            IF NOT loadCanceled
               filter := "*.*";
               GetFileName(fileName, pathName, filter, "Select Empirical Data File Name");
               IF fileName <> "NoFile"
                  NEW(checkFile);
                  ASK checkFile TO Open((pathName + fileName), Input);
                  checksOut := checkFile.ioResult;
                  ASK checkFile TO Close;
                  DISPOSE(checkFile);   
                  IF checksOut <> 0
                     NEW(message, 1..1);
                     message[1] := "Cannot find specified file!     ";
                     result := SendAlert(message, FALSE, FALSE, TRUE);
                     DISPOSE(message);
                  ELSE
                     IF (numDataPoints <> 0) AND result
                        ReadData((pathName + fileName), tempPts, tempArray);
                        ASK numPtsText TO SetLabel(INTTOSTR(numDataPoints+tempPts));
                        FOR i := (numDataPoints+1) TO (numDataPoints+tempPts)
                           ASK dataTable TO SetText(REALTOSTR(tempArray[i-numDataPoints]), 2, i);
                        END FOR;
                        numDataPoints := numDataPoints + tempPts;
                        IF numDataPoints < 999
                           ASK dataTable TO SetSelected(2,numDataPoints+1);
                        END IF;
                     ELSE
                        ReadData((pathName + fileName), tempPts, tempArray);
                        ASK numPtsText TO SetLabel(INTTOSTR(tempPts));
                        FOR i := 1 TO tempPts
                           ASK dataTable TO SetText(REALTOSTR(tempArray[i]), 2, i);
                        END FOR;
                        IF tempPts < numDataPoints
                           FOR i := tempPts+1 TO numDataPoints
                              ASK dataTable TO SetText("", 2, i);
                           END FOR;
                        END IF;
                        numDataPoints := tempPts;
                        IF numDataPoints < 999
                           ASK dataTable TO SetSelected(2,numDataPoints+1);
                        ELSE
                           ASK dataTable TO SetSelected(2,999);
                        END IF;
                     END IF;
                  END IF;
               END IF;
            END IF;
            IF cellSelected
               ASK deleteButton TO Activate;
            END IF;
            ASK getButton TO Activate;
            ASK okButton TO Activate;
            ASK cancelButton TO Activate;
            ASK helpButton TO Activate;
            ASK acceptButton TO Activate;
         WHEN 106:  {Accept Data Button}
            IF (dataEntryBox.Value >= 0.) AND (dataEntryBox.Value < 999999999.999999)
               IF cellSelected
                  ASK dataTable TO SetText(REALTOSTR(dataEntryBox.Value), 2, dataTable.SelectedRow);
               ELSE
                  ASK deleteButton TO Deactivate;
                  INC(numDataPoints);
                  ASK numPtsText TO SetLabel(INTTOSTR(numDataPoints));
                  ASK dataTable TO SetText(REALTOSTR(dataEntryBox.Value), 2, numDataPoints);
                  IF numDataPoints < 999
                     ASK dataTable TO SetSelected(2,numDataPoints+1);
                     cellSelected := FALSE;
                  ELSE
                     ASK dataTable TO SetSelected(2,999);
                     cellSelected := TRUE;
                  END IF;
               END IF;
            ELSE
               NEW(message, 1..1);
               message[1] := "Data point must be between 0 and 999,999,999.999999!     ";
               result := SendAlert(message, FALSE, FALSE, TRUE);
               DISPOSE(message);
            END IF;
         WHEN 25:
            CallHelp(25);
         OTHERWISE
      END CASE;
      ASK dataEntryBox TO ReceiveFocus;
      Draw;
      DISPOSE(tempArray);  
   END METHOD; {BeSelected}
END OBJECT; {DataBoxObj}

OBJECT HierBoxObj;
   ASK METHOD ReceiveData(INOUT tempHier : RBDHierObj;
                          OUT cancelled  : BOOLEAN);
   VAR
      newName, buddy, deptype   : STRING;
      validData                 : BOOLEAN;
      outNode, outNode2                   : RBDNodeObj;
      i, j, k, counter, dep     : INTEGER;
      tempBlock                 : RBDBlockObj;
      tempEvent                 : RBDEventObj;
      tempHier2                 : RBDHierObj;
      tempNode                  : RBDNodeObj;
      basic                     : RBDBasicObj;
      buddyList, buddyList2     : OptionListType;
   BEGIN
      nameBox := Descendant("HierName", 101);
      phaseChk := Descendant("PhaseCheck", 102);
      commentBox := Descendant("CommentBox", 103);
      dependRadBox    := Descendant("DependRadBox", 201);
         indepButton   := ASK dependRadBox Child("IndepButton", 2011);
         sysButton     := ASK dependRadBox Child("SysButton", 2013);
         localButton   := ASK dependRadBox Child("LocalButton", 2012);
         itemButton    := ASK dependRadBox Child("ItemButton", 2014);
      buddyCombo      := Descendant("BuddyCombo", 202);
      IF totalObjects > 1
         NEW(buddyList, 1..(totalObjects-1));
      END IF;
      
      IF tempHier.name <> ""
         ASK nameBox TO SetText(tempHier.name);
         SetLabel("Hierarchy Properties - " + tempHier.name + " - " + INTTOSTR(tempHier.Id));
      ELSE
         ASK nameBox TO SetText("unnamed");
         SetLabel("Hierarchy Properties - unnamed - " + INTTOSTR(tempHier.Id));
      END IF;
      ASK nameBox TO ReceiveFocus;
      IF tempHier.comment <> ""
         ASK commentBox TO SetText(tempHier.comment);
      ELSE
         ASK commentBox TO SetText("Comment");
      END IF;
      i:=1;
      IF (totalBlocks = 0) AND ((totalNodes-2) = 0) AND ((totalHiers-1) = 0) AND (totalEvents = 0)
         ASK itemButton TO Deactivate;
         ASK buddyCombo TO Deactivate;
      END IF;
      FOREACH tempBlock IN blockGroup
         tempString := INTTOSTR(tempBlock.Id);
         IF tempBlock.Id < 10
            tempString := "00" + tempString;
         ELSIF tempBlock.Id < 100
            tempString := "0" + tempString;
         END IF;
         buddyList[i] := tempBlock.name + " block - " + tempString;
         IF (tempHier.DependencyNum > 0) AND (tempHier.depType = "RBDBlock") AND 
            (tempBlock.Id = tempHier.DependencyNum)
            buddy := tempBlock.name + " block - " + tempString;
         END IF; 
         INC(i);
      END FOREACH;       
      FOREACH tempEvent IN eventGroup
         tempString := INTTOSTR(tempEvent.Id);
         IF tempEvent.Id < 10
            tempString := "00" + tempString;
         ELSIF tempEvent.Id < 100
            tempString := "0" + tempString;
         END IF;
         buddyList[i] := tempEvent.name + " event - " + tempString;
         IF (tempHier.DependencyNum > 0) AND (tempHier.depType = "RBDEvent") AND 
            (tempEvent.Id = tempHier.DependencyNum)
            buddy := tempEvent.name + " event - " + tempString;
         END IF; 
         INC(i);
      END FOREACH;       
      FOREACH tempNode IN nodeGroup
         tempString := INTTOSTR(tempNode.Id);
         IF tempNode.Id < 10
            tempString := "00" + tempString;
         ELSIF tempNode.Id < 100
            tempString := "0" + tempString;
         END IF;
         IF ((tempNode.typeNode = 5) AND (tempNode.parentID = tempHier.Id)) {do nothing}
            INC(j);
         ELSE
            IF ((tempNode.name = "end") OR (tempNode.name = "start")
                OR (tempNode.typeNode = 4))
               INC(j);
            ELSE
               IF tempNode.typeNode = 5
                  tempHier2 := ASK root Child("RBDHier", tempNode.parentID);
                  tempString := INTTOSTR(tempNode.parentID);
                  IF tempNode.parentID < 10
                     tempString := "00" + tempString;
                  ELSIF tempNode.parentID < 100
                     tempString := "0" + tempString;
                  END IF;
                  buddyList[i-j] := tempHier2.name + " hier - " + tempString;
                  IF (tempHier.DependencyNum > 0) AND (tempHier.depType = "RBDHier")  
                      AND (tempNode.Id = tempHier.DependencyNum)
                     buddy := tempHier2.name + " hier - " + tempString;
                  END IF;
               ELSE
                  buddyList[i-j] := tempNode.name + " node - " + tempString;
               END IF;
            END IF;
            IF (tempNode.typeNode <> 5) AND (tempHier.DependencyNum > 0) AND (tempHier.depType = "RBDNode") 
                AND (tempNode.Id = tempHier.DependencyNum)
               buddy := tempNode.name + " node - " + tempString;
            END IF;
         END IF;
         INC(i);
      END FOREACH;
      IF (tempHier.outID <> 0) {if not first time opening hier box}
         outNode := ASK root Child("RBDNode", tempHier.outID);
         IF outNode.usesPhasing
            ASK phaseChk TO DisplayCheck(TRUE);
         ELSE
            ASK phaseChk TO DisplayCheck(FALSE);
         END IF;
         IF outNode.DependencyNum > 0
            tempString := INTTOSTR(outNode.DependencyNum);
            IF outNode.DependencyNum < 10
               tempString := "00" + tempString;
            ELSIF outNode.DependencyNum < 100
               tempString := "0" + tempString;
            END IF;
            IF outNode.depType = "RBDBlock"
               tempBlock := ASK root Child("RBDBlock", outNode.DependencyNum);
               buddy := tempBlock.name + " block - " + tempString;
            ELSIF outNode.depType = "RBDEvent"
               tempEvent := ASK root Child("RBDEvent", outNode.DependencyNum);
               buddy := tempEvent.name + " event - " + tempString;
            ELSIF outNode.depType = "RBDNode"
               tempNode := ASK root Child("RBDNode", outNode.DependencyNum);
               buddy := tempNode.name + " node - " + tempString;
            ELSIF outNode.depType = "RBDHier"
               outNode2 := ASK root Child("RBDNode", outNode.DependencyNum);
               tempHier2 := ASK root Child("RBDHier", outNode2.parentID);
               tempString := INTTOSTR(tempHier2.Id);
               IF tempHier2.Id < 10
                  tempString := "00" + tempString;
               ELSIF tempHier2.Id < 100
                  tempString := "0" + tempString;
               END IF;
               buddy := tempHier2.name + " hier - " + tempString;
            END IF;
         END IF;
      ELSE
         ASK phaseChk TO DisplayCheck(FALSE);
      END IF;
      IF totalObjects > 1
         counter := 0;
         FOR i:= 1 TO HIGH(buddyList)
            IF (buddyList[i] = "")
               INC(counter);
            END IF;
         END FOR;
         IF (HIGH(buddyList) > counter)
            NEW(buddyList2, 1..(HIGH(buddyList) - counter));
         ELSE
            NEW(buddyList2, 1..(HIGH(buddyList)));
         END IF;
         FOR i:= 1 TO HIGH(buddyList2)
            buddyList2[i] := buddyList[i];
         END FOR;
         ASK buddyCombo TO SetOptions(buddyList2);
         IF buddy <> ""
            ASK buddyCombo TO SetText(buddy);
         ELSE
            ASK buddyCombo TO SetText(buddyList2[1]);
         END IF;
      ELSE
         ASK buddyCombo TO Deactivate;
         ASK itemButton TO Deactivate;
      END IF;
      IF (tempHier.outID <> 0)
         outNode := ASK root Child("RBDNode", tempHier.outID);
         IF outNode.DependencyNum  = -2
            ASK dependRadBox TO SetSelectedButton(sysButton);
            ASK buddyCombo TO Deactivate;
         ELSIF outNode.DependencyNum  = 0
            ASK dependRadBox TO SetSelectedButton(indepButton);
            ASK buddyCombo TO Deactivate;
         ELSIF outNode.DependencyNum  = -1
            ASK dependRadBox TO SetSelectedButton(localButton);
            ASK buddyCombo TO Deactivate;
         ELSE
            IF totalObjects > 1
               ASK dependRadBox TO SetSelectedButton(itemButton);
               ASK buddyCombo TO SetText(buddy);
               ASK buddyCombo TO Activate;
            ELSE
               ASK dependRadBox TO SetSelectedButton(localButton);
               ASK buddyCombo TO Deactivate;
            END IF;
         END IF;
      ELSE
         ASK dependRadBox TO SetSelectedButton(indepButton);
         ASK buddyCombo TO Deactivate;
      END IF;
      IF compileType = "student"
         ASK phaseChk TO Deactivate;
      END IF;
      Draw;
      REPEAT
         cancelled := FALSE;
         validData := TRUE;
         button := AcceptInput();
         IF button.ReferenceName = "CancelButton"   {User clicked cancel}
            cancelled := TRUE;
         ELSE                                       {User clicked OK}
            newName := nameBox.Text();
            IF ((newName = "") OR (POSITION(newName, " ") <> 0))
               NEW(message, 1..1);
               message[1] := "Name field can't be blank or have blank spaces!     ";
               result := SendAlert(message, FALSE, FALSE, TRUE);
               DISPOSE(message);
               ASK nameBox TO SetText("unnamed");
               validData := FALSE;
            ELSIF STRLEN(newName) > 20
               NEW(message, 1..1);
               message[1] := "Name must be no greater than 20 characters!     ";
               result := SendAlert(message, FALSE, FALSE, TRUE);
               DISPOSE(message);
               ASK nameBox TO SetText(SUBSTR(1,20,newName));
               validData := FALSE;
            END IF;
         END IF;      
      UNTIL validData;
      IF NOT cancelled
         somethingChanged := TRUE;
         ASK tempHier TO SetName(newName);
         ASK tempHier TO SetComment(commentBox.Text());
         deptype := "";
         IF dependRadBox.SelectedButton = indepButton
            dep := 0;
         ELSIF dependRadBox.SelectedButton = sysButton
            dep := -2;
         ELSIF dependRadBox.SelectedButton = localButton
            dep := -1;
         ELSE   
            i := POSITION(buddyCombo.Text, " -");
            dep := STRTOINT(SUBSTR(i+2, i+11, buddyCombo.Text + "      "));
            IF (SUBSTR(i-1,i-1, buddyCombo.Text) = "r")
               tempHier2 := ASK root Child("RBDHier", dep);
               dep := tempHier2.outID;
               deptype := "RBDHier";
            ELSIF (SUBSTR(i-1,i-1, buddyCombo.Text) = "k")
               deptype := "RBDBlock";
            ELSIF (SUBSTR(i-1,i-1, buddyCombo.Text) = "e")
               deptype := "RBDNode";
            ELSIF (SUBSTR(i-1,i-1, buddyCombo.Text) = "t")
               deptype := "RBDEvent";
            END IF;
         END IF;
         tempHier.SetDep(dep, deptype); {Set hier's dependency as flag to set its outnode's dependence upon creation}
         tempHier.SetusesPhasing(phaseChk.Checked); {Set hier's phasing as flag to set its outnode's phasing}
         IF (tempHier.outID <> 0)
            outNode := ASK root Child("RBDNode", tempHier.outID);
            IF phaseChk.Checked
               outNode.SetusesPhasing(TRUE);
               ASK outNode TO SetPhases(TRUE, NILARRAY);
            ELSE
               tempHier.SetusesPhasing(FALSE);
               outNode.SetusesPhasing(FALSE);
               ASK outNode TO SetPhases(FALSE, NILARRAY);
            END IF;
            ASK outNode TO SetDep(dep, deptype);
         END IF;
      END IF;
   END METHOD;
   
   ASK METHOD BeSelected;
   VAR
      tabClicked                        : GraphicVObj;
   BEGIN
      tabClicked := LastPicked;
      CASE tabClicked.Id
         WHEN 200:{Distro tab}
            lastClicked := ASK tabClicked LastPicked;
            IF lastClicked.Id = 201
               IF dependRadBox.SelectedButton.Id = 2013
                  ASK dependRadBox TO SetSelectedButton(sysButton);
                  ASK buddyCombo TO Deactivate;
               ELSIF dependRadBox.SelectedButton.Id = 2011
                  ASK dependRadBox TO SetSelectedButton(indepButton);
                  ASK buddyCombo TO Deactivate;
               ELSIF dependRadBox.SelectedButton.Id = 2012
                  ASK dependRadBox TO SetSelectedButton(localButton);
                  ASK buddyCombo TO Deactivate;
               ELSE
                  ASK dependRadBox TO SetSelectedButton(itemButton);
                  ASK buddyCombo TO Activate;
               END IF;
            END IF;
         OTHERWISE;
      END CASE;
      INHERITED BeSelected;
   END METHOD;
END OBJECT;

OBJECT TrigBoxObj;

   {ReceiveData -- TrigBoxObj}
   ASK METHOD ReceiveData;
   VAR
      tempString : STRING;
   BEGIN
      trigComboBox     := Descendant("TrigComboBox", 101);
      freqTable        := Descendant("FreqTable", 103);
      unitsLabel1      := Descendant("UnitsLabel1", 104);
      deleteButton     := Descendant("DeleteButton", 105);
      staggerValBox    := Descendant("StaggerValBox", 106);
      unitsLabel2      := Descendant("UnitsLabel2", 107);
      repChkBox        := Descendant("RepChkBox", 108);     
      helpButton       := Descendant("HelpButton", 109);
      tempRep := TRUE;
      tempStagger := 0.;
      tempDist := 19;
      NEW(tempParams, 1..1);
      tempParams[1] := 100.;
      MakeDistString(tempDist, tempParams, tempString);
      ASK freqTable TO SetText(tempString,1,0);
      ASK freqTable TO SetSize(1,0);
      ASK freqTable TO SetVisibleSize(23,1);
      Draw;
        
      ASK unitsLabel1 TO SetLabel(systemUnits);
      ASK unitsLabel2 TO SetLabel(systemUnits);
    
      GetTrigList(trigList);
      ASK trigComboBox TO SetOptions(trigList);
      {IF trigList[1] <> ""
         ASK trigComboBox TO SetText(trigList[1]);
      END IF;}
      ASK trigComboBox TO SetText("unnamed");
      Draw;
      REPEAT
         button := AcceptInput();
      UNTIL trigsDone; 
   END METHOD; {ReceiveData -- TrigBoxObj}

   ASK METHOD BeSelected;
   VAR 
      distBox          : DistBoxObj;
      trigName, tempString         : STRING;
      trig        : RapTriggerObj;
   
      i, numParams        : INTEGER;
      result,update,validName,goodValue, distChanged
      {blockUsesTrig}, activeUse, defaultUsesTrig, defaultActiveUse                : BOOLEAN;
      message2                                : TextBufferType;
      textTrig                                : ARRAY INTEGER OF STRING;
      block                                   : RBDBlockObj;
      
   BEGIN
      trigsDone := TRUE;
      blockUsesTrig := FALSE;
      trigName := trigComboBox.Text();  
      distChanged:= FALSE;
      NEW(trigDistParams, 1..1); 
      NEW(textTrig, 1..20);
      errors := 0;
      lastClicked := ASK SELF LastPicked;
      CASE lastClicked.Id
         WHEN 101:
            {Cannot have a trigger named "unnamed".  No warning yet.}
            IF trigName = ""
               ASK trigComboBox TO SetText("unnamed");
            ELSIF trigName <> "unnamed"
               ASK staggerValBox TO SetValue(0.);
               ASK repChkBox TO SetCheck(TRUE);
               
               FOREACH trig IN triggerGroup
                  IF trigName = trig.TrigName
                     tempStagger := trig.InitUsed;
                     tempRep := trig.Repeats;
                     tempDist := trig.TrigDist;
                     GetNumParams(tempDist, numParams);
                     DISPOSE(tempParams);
                     NEW(tempParams, 1..numParams);
                     FOR i:=1 TO numParams
                        tempParams[i] := trig.TrigParams[i];
                     END FOR;
                     EXIT;
                  END IF;
               END FOREACH;
               ASK staggerValBox TO SetValue(tempStagger);
               ASK repChkBox TO SetCheck(tempRep);
               MakeDistString(tempDist, tempParams, tempString);
               ASK freqTable TO SetText(tempString,1,0);
               Draw;
            END IF;
         WHEN 103: {Pick Distribution} 
            ShowDistBox(tempDist, tempParams);
            MakeDistString(tempDist, tempParams, tempString);
            ASK freqTable TO SetText(tempString, 1, 0);
            Draw;
            distChanged:= TRUE;
         WHEN 51: 
            CallHelp(51);
         OTHERWISE
      END CASE;
      
      IF (NOT distChanged)
         IF ((lastClicked.ReferenceName = "CreateButton") OR (lastClicked.ReferenceName = "OKButton"))
            AND ((trigName <> "Unnamed") AND (trigName <> "unnamed"))
            GetTrigList(trigList);
            validName := TRUE;
            goodValue := TRUE;
            update := FALSE;
            IF trigName = "" 
               validName := FALSE;
               INC(errors);
               textTrig[errors] := "The trigger name cannot be blank!     ";
               ASK trigComboBox TO ReceiveFocus;
            ELSIF (POSITION(trigName, " ") <> 0)
               validName := FALSE;
               INC(errors);
               textTrig[errors] := "A trigger name can't have blank spaces!     ";
               ASK trigComboBox TO ReceiveFocus;
            ELSIF STRLEN(trigName) > 20
               validName := FALSE;
               INC(errors);
               textTrig[errors] := "The trigger name must be no greater than 20 spaces!     ";
               ASK trigComboBox TO SetText(SUBSTR(1,20,trigName));
               ASK trigComboBox TO ReceiveFocus;
            END IF;
            IF (staggerValBox.Value()< 0.0) OR (staggerValBox.Value() > 999999999.999999)
               INC(errors);
               textTrig[errors] := "The stagger value must be between 0 and 999,999,999.999999!     ";
               ASK staggerValBox TO SetValue(tempStagger);
               goodValue := FALSE;
            ELSE
               tempStagger := staggerValBox.Value();
            END IF;
            IF goodValue AND validName
               FOREACH trig IN triggerGroup
                  IF trig.TrigName = trigName
                     update := TRUE;
                     NEW(message2,1..2);
                     message2[1] := "Trigger '"+trigName+"' already exists, do you     ";
                     message2[2] := "want to update the trigger with the new data?     ";
                     result := SendAlert(message2, TRUE, FALSE, FALSE);
                     DISPOSE(message2);
                     IF result
                        ASK trig TO RemoveTrig(trigName);
                        ASK triggerGroup TO RemoveThis(trig);
                        NEW(trig);
                        ASK trig TO SetTrigData(trigName, tempDist, tempParams, repChkBox.Checked, tempStagger);
                        INC(totalTriggers);                       
                        somethingChanged := TRUE;
                        EXIT;
                     END IF;
                  END IF;
               END FOREACH;                  
            END IF;
            IF ((validName) AND (goodValue) AND (NOT update))
               NEW(trig);
               ASK trig TO SetTrigData(trigName, tempDist, tempParams, repChkBox.Checked, tempStagger);
               INC(totalTriggers);
               somethingChanged := TRUE;
            END IF;
            DISPOSE(message);
            GetTrigList(trigList);
            IF validName AND goodValue AND (lastClicked.ReferenceName <> "OKButton")
               ASK trigComboBox TO SetOptions(trigList);
               ASK trigComboBox TO SetText("unnamed");
               tempRep := TRUE;
               tempStagger := 0.;
               tempDist := 19;
               NEW(tempParams, 1..1);
               tempParams[1] := 100.;
               MakeDistString(tempDist, tempParams, tempString);
               ASK freqTable TO SetText(tempString,1,0);
               ASK staggerValBox TO SetValue(0.);
               ASK repChkBox TO SetCheck(TRUE);
            END IF;
         ELSIF lastClicked.ReferenceName = "DeleteButton"
            IF trigName <> "unnamed"
               pmTrigName := trigName;
               NEW(message, 1..1);      
               NEW(message2, 1..3);
               blockUsesTrig := FALSE;
               block := ASK root Child("RBDBlock", 0);
               IF (block.pmTrig = trigName)
                  defaultUsesTrig := TRUE;
                  IF((block.usesPM = TRUE) AND (block.pmTriggered = TRUE))
                     defaultActiveUse := TRUE;
                  END IF;
               END IF;   
               FOREACH block IN blockGroup
                  IF (block.pmTrig = trigName) 
                     blockUsesTrig := TRUE;
                     IF((block.usesPM = TRUE) AND (block.pmTriggered = TRUE))
                        activeUse := TRUE;
                     END IF;
                     EXIT;              
                  END IF;
               END FOREACH;
               FOREACH trig IN triggerGroup
                  IF (trig.TrigName = trigName)
                     IF (activeUse)
                        message2[1] := "One or more blocks use the '"+trigName+"' trigger.     ";
                        message2[2] := "If you delete it, these blocks will now have no preventive maintenance.     ";
                        message2[3] := "Are you sure you want to delete the '"+trigName+"' trigger?     ";
                        result := SendAlert(message2, TRUE, FALSE, FALSE);
                     ELSIF (defaultActiveUse)
                        message2[1] := "Your default block uses the '"+trigName+"' trigger.     ";
                        message2[2] := "If you delete it, the default block will have no preventive maintenance.     ";
                        message2[3] := "Are you sure you want to delete the '"+trigName+"' trigger?     ";
                        result := SendAlert(message2, TRUE, FALSE, FALSE);
                     ELSE
                        message[1] := "Are you sure you want to delete the '"+trigName+"' trigger?     ";
                        result := SendAlert(message, TRUE, FALSE, FALSE);
                     END IF;
                
                     IF result
                        ASK trig TO RemoveTrig(trigName);
                        ASK triggerGroup TO RemoveThis(trig);
                        GetTrigList(trigList);
                        ASK trigComboBox TO SetOptions(trigList);
                        ASK trigComboBox TO SetText("unnamed");
          
                        tempRep := TRUE;
                        tempStagger := 0.;
                        tempDist := 19;
                        NEW(tempParams, 1..1);
                        tempParams[1] := 100.;
                        MakeDistString(tempDist, tempParams, tempString);
                        ASK freqTable TO SetText(tempString,1,0);
                        ASK staggerValBox TO SetValue(0.);
                        ASK repChkBox TO SetCheck(TRUE);
                        somethingChanged := TRUE;
                        IF (blockUsesTrig OR activeUse OR defaultUsesTrig OR defaultActiveUse)
                           FOREACH block IN blockGroup   {for loop originally started with 0 -- should include 
                                                          default block ???}
                              IF (block.pmTrig = trigName)
                                 IF ((block.usesPM = TRUE) AND (block.pmTriggered = TRUE) )
                                    {Removed trigger used by this block}
                                    ASK block TO SetUsesPM(FALSE);
                                    ASK block TO SetpmTriggered(FALSE);
                                    ASK block TO SetpmTrig("None");
                                 ELSIF ((block.usesPM = FALSE) AND (block.pmTriggered = TRUE))
                                    ASK block TO SetpmTriggered(FALSE);
                                    ASK block TO SetpmTrig("None");
                                 ELSIF (block.pmTriggered = FALSE)
                                    ASK block TO SetpmTrig("None");
                                 END IF;
                              END IF;
                           END FOREACH;
                        END IF;
                     END IF;
                  END IF;
               END FOREACH;
               DISPOSE(message);
               DISPOSE(message2);
               GetTrigList(trigList);
            END IF;   
         END IF;  
      END IF;
 
      IF errors > 1
         trigsDone := FALSE;
         NEW(message, 1..errors+2);
         message[1] := "The following errors must be corrected:   ";
         message[2] := "";
         FOR i := 1 TO errors
            message[i+2] := "   "+INTTOSTR(i)+". "+textTrig[i];
         END FOR;
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      ELSIF errors = 1
         trigsDone := FALSE;
         NEW(message, 1..1);
         message[1] := textTrig[1];
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      END IF;
      DISPOSE(textTrig);
      Update;   
      INHERITED BeSelected; 
   END METHOD; {BeSelected -- TrigBoxObj}   
END OBJECT; {TrigBoxObj}


OBJECT DistBoxObj;
 
   {ReceiveData -- DistBoxObj}
   ASK METHOD ReceiveData(INOUT distType   : INTEGER;
                          INOUT distParams : realArray);
  
   VAR
      distString                                      : STRING;
      tags, units                                     : strArray;
      Off, pVals                                      : realArray;
      distroNum, i, numParams                         : INTEGER;
      Flag, validData                                 : BOOLEAN;
      mu,sigma2,logMean2,logVar,startVal1, startVal2,
      tempMean, tempSTDev                             : REAL; 
      text                                            : strArray;
      
   BEGIN
      distComboBox     := Descendant("DistComboBox", 101);
      NEW(paramLabels, 1..3);
      NEW(paramVals, 1..3);
      NEW(paramUnits, 1..3);
      NEW(units, 1..3);
      NEW(Off, 1..3);
      NEW(tags, 1..3);
      NEW(text, 1..25);
      
      FOR i := 1 TO 3
         paramLabels[i] := Descendant("ParamLabel", i);
         paramVals[i]   := Descendant("ParamVal", i);
         paramUnits[i]  := Descendant("ParamUnit", i);
      END FOR;
      parametersLabel := Descendant("ParametersLabel", 104);
      meanLabel := Descendant("MeanLabel", 102); 
      stdevLabel := Descendant("StdevLabel", 103);

      {Set distribution to distType passed in}
      ConvertToString(distType,distString);
      GetParameters(distType, "other", tags, units, Off);
      ASK distComboBox TO DisplayText(distString);
      FOR i := 1 TO 3
         ASK paramLabels[i] TO DisplayAt(Off[i]+26., (FLOAT(i*2)-.25));
         ASK paramLabels[i] TO SetLabel(tags[i]);
         ASK paramUnits[i] TO SetLabel(units[i]);
      END FOR;
      GetNumParams(distType, numParams);
      FOR i:=1 TO numParams
         ASK paramVals[i] TO SetHidden(FALSE);
         ASK paramUnits[i] TO SetHidden(FALSE);
         IF ((muSigmaMode) AND (distType=7) AND (i=1))
            logMean2 := POWER(distParams[1], 2.);
            logVar := POWER(distParams[2], 2.);
            ASK paramVals[i] TO DisplayValue(LN(logMean2/SQRT(logVar + logMean2))); 
            startVal1:=paramVals[1].Value();           
         ELSIF ((muSigmaMode) AND (distType=7) AND (i=2))
            ASK paramVals[i] TO DisplayValue(SQRT(LN((logVar + logMean2)/logMean2)));        
            startVal2:=paramVals[2].Value();           
         ELSE
            ASK paramVals[i] TO DisplayValue(distParams[i]);
         END IF;
      END FOR;
      FOR i:=numParams+1 TO 3
         ASK paramVals[i] TO SetHidden(TRUE);
         ASK paramUnits[i] TO SetHidden(TRUE);
      END FOR;
      ComputeStats(distType, distParams, tempMean, tempSTDev);
      IF tempMean = 12345.6789
         ASK meanLabel TO SetLabel("Undefined");
      ELSIF tempMean = 12345.6788
         ASK meanLabel TO SetLabel("N/A");
      ELSE
         ASK meanLabel TO SetLabel(REALTOSTR(tempMean));
      END IF;
      IF tempSTDev = 12345.6789      
         ASK stdevLabel TO SetLabel("Undefined");
      ELSIF tempSTDev = 12345.6788
         ASK stdevLabel TO SetLabel("N/A");
      ELSE
         ASK stdevLabel TO SetLabel(REALTOSTR(tempSTDev));
      END IF;
      Update;
      REPEAT
         errors := 0;
         validData := TRUE;
         Flag := TRUE;
         button := AcceptInput();
         IF button.ReferenceName = "CancelButton"   {User clicked cancel}
            Flag     := TRUE;
            validData := TRUE;
         ELSE  {error checking}                    {User clicked OK}
            distString := distComboBox.Text();
            ConvertToInteger(distString, distroNum, numParams);
            NEW(pVals, 1..numParams);
            FOR i := 1 TO numParams
               pVals[i] := paramVals[i].Value();
            END FOR;
            CheckValidInput(distroNum, numParams, pVals,"" ,Flag, text);
            IF ((muSigmaMode) AND (distroNum=7))
               IF ((paramVals[1].Value() <> startVal1) 
                   OR (paramVals[2].Value() <> startVal2))
                  mu:=paramVals[1].Value();
                  sigma2:=POWER(paramVals[2].Value(),2.0);
                  pVals[1] := EXP(mu+sigma2/2.0);
                  pVals[2] := SQRT(EXP(2.0*mu+sigma2)*(EXP(sigma2)-1.));
               ELSE
                  pVals[1] :=distParams[1];
                  pVals[2] :=distParams[2];
               END IF;
            END IF;
            IF (validData AND Flag)
               distType := distroNum;
               DISPOSE(distParams);
               NEW(distParams,1..numParams);
               FOR i:=1 TO numParams
                  distParams[i]:=pVals[i];
               END FOR;
            END IF;
         END IF;
         IF errors > 1
            validData := FALSE;
            NEW(message, 1..errors+2);
            message[1] := "The following errors must be corrected:   ";
            message[2] := "";
            FOR i := 1 TO errors
               message[i+2] := "   "+INTTOSTR(i)+". "+text[i];
            END FOR;
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         ELSIF errors = 1
            validData := FALSE;
            NEW(message, 1..1);
            message[1] := text[1];
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
      UNTIL(Flag AND validData);
      DISPOSE(paramLabels);
      DISPOSE(paramVals);
      DISPOSE(paramUnits);
      DISPOSE(units);
      DISPOSE(Off);
      DISPOSE(tags);
      DISPOSE(text);
    
   END METHOD; {ReceiveData -- DistBoxObj};
  
   {BeSelected -- DistBoxObj} 
   ASK METHOD BeSelected; 
 
   VAR
      distroStr, rDistro, tempDistro                  : STRING;
      distroNum, numParams, i, tempDistroNum          : INTEGER;
      tags, units                                     : strArray;
      Off                                            : realArray;
      logMean2, logVar, startVal1, startVal2        : REAL;   
      tempMean, tempSTDev, x, y, z                    : REAL;
      tempVal                                         : realArray;
        
   BEGIN
  
      NEW(paramLabels, 1..3);
      NEW(paramVals, 1..3);
      NEW(paramUnits, 1..3);
      NEW(units, 1..3);
      NEW(Off, 1..3);
      NEW(tags, 1..3);

      FOR i := 1 TO 3
         paramLabels[i] := Descendant("ParamLabel", i);
         paramVals[i]   := Descendant("ParamVal", i);
         paramUnits[i]  := Descendant("ParamUnit", i);
      END FOR;

      lastClicked := ASK SELF LastPicked;
      CASE lastClicked.Id
         WHEN 101: {Distro Combo Box}
            distroStr := distComboBox.Text();
            IF distroStr <> ""
               ConvertToInteger(distroStr, distroNum, numParams);
               GetParameters(distroNum, "other", tags, units, Off);
               FOR i := 1 TO 3
                  ASK paramLabels[i] TO DisplayAt(Off[i]+26., (FLOAT(i*2)-.25));
                  ASK paramLabels[i] TO SetLabel(tags[i]);
                  ASK paramUnits[i] TO SetLabel(units[i]);
               END FOR;
               Update;
               ASK parametersLabel TO SetHidden(FALSE);
               FOR i := 1 TO numParams
                  ASK paramVals[i] TO SetHidden(FALSE);
                  ASK paramUnits[i] TO SetHidden(FALSE);
                  IF (paramLabels[i].Label = "Location") AND (distComboBox.Text <> "Exponential")
                      AND (distComboBox.Text <> "Extreme Value") AND (distComboBox.Text <> "Laplace")
                     ASK paramVals[i] TO DisplayValue(0.0);
                  ELSIF paramLabels[i].Label = "Probability"
                     ASK paramVals[i] TO DisplayValue(0.8);
                  ELSIF (distComboBox.Text = "Lognormal") AND (i=2)
                     IF muSigmaMode
                        logMean2 := POWER(10., 2.);
                        logVar := POWER(2., 2.);
                        ASK paramVals[1] TO DisplayValue(LN(logMean2/SQRT(logVar + logMean2)));        
                        startVal1:=paramVals[1].Value();           
                        ASK paramVals[2] TO DisplayValue(SQRT(LN((logVar + logMean2)/logMean2)));        
                        startVal2:=paramVals[2].Value();           
                     ELSE
                        ASK paramVals[1] TO DisplayValue(10.);
                        ASK paramVals[2] TO DisplayValue(2.);
                     END IF;
                  ELSIF (distComboBox.Text = "Exponential") AND (i=2)
                     {IF lambdaMode
                        ASK paramVals[1] TO DisplayValue(1.0 / 100.);
                        startVal1:=paramVals[1].Value();
                     ELSE}
                        ASK paramVals[1] TO DisplayValue(100.);
                     {END IF;}
                     ASK paramVals[2] TO DisplayValue(0.);
                  ELSIF (distComboBox.Text = "Triangular") AND (i>1)
                     IF i=2
                        ASK paramVals[2] TO DisplayValue(1.5);
                     ELSE
                        ASK paramVals[3] TO DisplayValue(2.0);
                     END IF;
                  ELSIF (distComboBox.Text = "Pearson 5")
                     ASK paramVals[1] TO DisplayValue(4.);
                     ASK paramVals[2] TO DisplayValue(1.);
                  ELSIF (distComboBox.Text = "Pearson 6")
                     ASK paramVals[1] TO DisplayValue(4.);
                     ASK paramVals[2] TO DisplayValue(4.);
                     ASK paramVals[3] TO DisplayValue(1.);
                  ELSIF (distComboBox.Text = "Normal")
                     ASK paramVals[1] TO DisplayValue(100.);
                     ASK paramVals[2] TO DisplayValue(10.);
                  ELSIF (distComboBox.Text = "Exponential Power")
                     {defaults here}
                  ELSIF (distComboBox.Text = "Extreme Value")
                     ASK paramVals[1] TO DisplayValue(10.);
                     ASK paramVals[2] TO DisplayValue(95.);
                  ELSIF (distComboBox.Text = "Laplace")
                     ASK paramVals[1] TO DisplayValue(10.);
                     ASK paramVals[2] TO DisplayValue(100.);
                  ELSE
                     ASK paramVals[i] TO DisplayValue(1.0);
                  END IF;
               END FOR;
               FOR i := (numParams+1) TO 3
                  ASK paramVals[i] TO DisplayValue(0.0);
                  ASK paramVals[i] TO SetHidden(TRUE);
                  ASK paramUnits[i] TO SetHidden(TRUE);
               END FOR;     
               Draw;
               DISPOSE(paramLabels);
               DISPOSE(paramVals);
               DISPOSE(paramUnits);
               DISPOSE(units);
               DISPOSE(Off);
               DISPOSE(tags);
            END IF; 
         WHEN 24:
            CallHelp(24);
         WHEN 107: {Refresh Button}
            NEW(tempVal, 1..3);
            tempDistro := distComboBox.Text();
            ConvertToInteger(tempDistro, tempDistroNum, numParams);
            tempVal[1] := paramVals[1].Value();
            tempVal[2] := paramVals[2].Value();
            tempVal[3] := paramVals[3].Value();
            {IF ((lambdaMode) AND (tempDistro="Exponential"))
               tempVal[1] := (1.0 / paramVals[1].Value());
            ELS}IF ((muSigmaMode) AND (tempDistro="Lognormal"))
               tempVal[1] := EXP(paramVals[1].Value()+POWER(paramVals[2].Value(),2.0)/2.0);
               tempVal[2] := SQRT(EXP(2.0*paramVals[1].Value()+POWER(paramVals[2].Value(),2.0))*(EXP(POWER(paramVals[2].Value(),2.0))-1.));
            END IF;
            ComputeStats(tempDistroNum, tempVal, tempMean, tempSTDev);
            IF tempMean = 12345.6789
               ASK meanLabel TO SetLabel("Undefined");
            ELSIF tempMean = 12345.6788
               ASK meanLabel TO SetLabel("N/A");
            ELSE
               ASK meanLabel TO SetLabel(REALTOSTR(tempMean));
            END IF;
            IF tempSTDev = 12345.6789      
               ASK stdevLabel TO SetLabel("Undefined");
            ELSIF tempSTDev = 12345.6788
               ASK stdevLabel TO SetLabel("N/A");
            ELSE
               ASK stdevLabel TO SetLabel(REALTOSTR(tempSTDev));
            END IF;
            DISPOSE(tempVal);
            Update;
         OTHERWISE
      END CASE;
      INHERITED BeSelected;
   END METHOD; {BeSelected -- DistBoxObj}
END OBJECT; {DistBoxObj}


   
OBJECT SparePoolsBoxObj;
 
  {ReceiveData -- SparePoolsBoxObj}
   ASK METHOD ReceiveData;
   BEGIN
      spareCombo     := Descendant("SpareComboBox", 101);
      initVal        := Descendant("InitVal", 102);
      newChk         := Descendant("NewChkBox", 103);
      newAmt         := Descendant("NewAmount", 104);
      newText        := Descendant("NewText", 105);
      newArrival     := Descendant("NewArrival", 106);
      timeLabel1     := Descendant("TimeLabel1", 107);
      stockChk       := Descendant("StockChkBox", 108);
      stockTxt1      := Descendant("StockTxt1", 109);
      stockVal       := Descendant("StockValBox", 110);
      stockTxt2      := Descendant("StockTxt2", 111);
      stockAmt       := Descendant("StockAmtVal", 112);
      stockTxt3      := Descendant("StockTxt3", 113);
      stockRate      := Descendant("StockRateVal", 114);
      timeLabel2     := Descendant("TimeLabel2", 115);
      emerChk        := Descendant("EmerChkBox", 116);
      emerText       := Descendant("EmerText", 117);
      emerVal        := Descendant("EmerVal", 118); 
      timeLabel3     := Descendant("TimeLabel3", 119);
      createCost     := Descendant("CreateCost", 120);
      useCost        := Descendant("UseCost", 121);
      
      ASK timeLabel1 TO SetLabel(systemUnits);
      ASK timeLabel2 TO SetLabel(systemUnits);
      ASK timeLabel3 TO SetLabel(systemUnits);
      GetSpareList(spareList);
      ASK spareCombo TO SetOptions(spareList);
      ASK spareCombo TO SetText("unnamed");
      Draw;
      ASK newAmt TO Deactivate;
      ASK newArrival TO Deactivate;
      ASK emerVal TO Deactivate;
      ASK stockAmt TO Deactivate;
      ASK stockRate TO Deactivate;
      ASK stockVal TO Deactivate;
      ASK initVal TO SetValue(1.);
      ASK stockChk TO SetCheck(FALSE);
      ASK newChk TO SetCheck(FALSE);
      ASK emerChk TO SetCheck(FALSE);
      ASK createCost TO SetValue(1.);
      ASK useCost TO SetValue(1.);
      ASK spareCombo TO ReceiveFocus;
      Draw;
      REPEAT
         button := AcceptInput();
      UNTIL sparesDone; 
   END METHOD; {ReceiveData -- SparePoolsBoxObj}

   {BeSelected -- SparePoolsBoxObj}
   ASK METHOD BeSelected;
   VAR
      result,update,validName,goodValue,
      blockUsesPool,cancelDelete,
      defaultUsesPool                         : BOOLEAN;
      i, radButtonId,poolNum,newSpares,maxNum,
      stkVal,stkAmt,oldStkAmt,oldStkVal       : INTEGER;
      newSparesArrival,emerTime,stkRate,
      oldCreateCost,oldNewSp,oldEmerCost,
      oldNewSpArr,oldEmerTime,oldUseCost,
      oldInitSp,oldFixCost,spCost,
      fixCost,useShipCost,oldStkRate          : REAL;
      pool                                    : SparePoolObj;
      name                                    : STRING;
      type                                    : SparingType;
      message2                                : TextBufferType;
      block                                   : RBDBlockObj;
      textSpare                               : ARRAY INTEGER OF STRING;
   BEGIN
      sparesDone := TRUE;
      oldCreateCost := 1.;
      oldNewSp := 1.;
      oldNewSpArr := 720.;
      oldEmerTime := 24.;
      oldStkVal := 0;
      oldStkAmt := 1;
      oldStkRate := 24.;
      NEW(textSpare, 1..20);
      errors := 0;
      type := SparePool;
      lastClicked := ASK SELF LastPicked;
      name := spareCombo.Text();
      CASE lastClicked.Id
         WHEN 48:
            CallHelp(48);
         WHEN 101:
            GetSpareList(spareList);
            IF name = ""
               ASK spareCombo TO SetText("unnamed");
            ELSIF name <> "unnamed"
               ASK newAmt TO SetValue(1.);
               ASK newArrival TO SetValue(720.);
               ASK stockVal TO SetValue(0.);
               ASK stockAmt TO SetValue(1.);
               ASK stockRate TO SetValue(24.);
               ASK emerVal TO SetValue(24.);
               FOREACH pool IN poolGroup
                  IF ((name = pool.poolName) AND (pool.sparingType = type))
                     oldInitSp := FLOAT(pool.initialSpares);
                     oldCreateCost := pool.spareCost;
                     oldNewSp := FLOAT(pool.newSpares);
                     oldNewSpArr := pool.newSparesArrival;
                     oldStkVal := pool.SLOOrderLevel;
                     oldStkAmt := pool.SLONewSpares;
                     oldStkRate := pool.SLOTime;
                     oldEmerTime := pool.emergencyTime;
                     oldEmerCost := pool.emerShippingCost;
                     EXIT;
                  END IF;
               END FOREACH;
               ASK initVal TO SetValue(oldInitSp);
               ASK createCost TO SetValue(oldCreateCost);
               ASK useCost TO SetValue(oldEmerCost);
               IF (pool.routineSpareOrdering)
                  ASK newChk TO SetCheck(TRUE);
                  ASK newAmt TO SetValue(oldNewSp);
                  ASK newArrival TO SetValue(oldNewSpArr);
                  ASK newAmt TO Activate;
                  ASK newArrival TO Activate;
               ELSE
                  ASK newChk TO SetCheck(FALSE);
                  ASK newAmt TO Deactivate;
                  ASK newArrival TO Deactivate;
               END IF;
               IF (pool.stockLevelOrdering)
                  ASK stockChk TO SetCheck(TRUE);
                  ASK stockVal TO SetValue(FLOAT(oldStkVal));
                  ASK stockAmt TO SetValue(FLOAT(oldStkAmt));
                  ASK stockRate TO SetValue(oldStkRate);
                  ASK stockVal TO Activate;
                  ASK stockAmt TO Activate;
                  ASK stockRate TO Activate;
               ELSE
                  ASK stockChk TO SetCheck(FALSE);
                  ASK stockVal TO Deactivate;
                  ASK stockAmt TO Deactivate;
                  ASK stockRate TO Deactivate;
               END IF;
               IF (pool.emerSpareOrdering)
                  ASK emerChk TO SetCheck(TRUE);
                  ASK emerVal TO SetValue(oldEmerTime);
                  ASK emerVal TO Activate;
               ELSE
                  ASK emerChk TO SetCheck(FALSE);
                  ASK emerVal TO Deactivate;
               END IF;
            END IF;
         WHEN 103:
            IF newChk.Checked
               ASK newAmt TO Activate;
               ASK newArrival TO Activate;
            ELSE
               ASK newAmt TO Deactivate;
               ASK newArrival TO Deactivate;
            END IF;
         WHEN 108:
            IF stockChk.Checked
               ASK stockAmt TO Activate;
               ASK stockRate TO Activate;
               ASK stockVal TO Activate;
            ELSE
               ASK stockAmt TO Deactivate;
               ASK stockRate TO Deactivate;
               ASK stockVal TO Deactivate;
            END IF;
         WHEN 116:
            IF emerChk.Checked
               ASK emerVal TO Activate;
            ELSE
               ASK emerVal TO Deactivate;
            END IF;
         OTHERWISE;
      END CASE;
      IF ((lastClicked.ReferenceName = "CreateButton") OR (lastClicked.ReferenceName = "OKButton"))
          AND ((name <> "Unnamed") AND (name <> "unnamed"))
         GetSpareList(spareList);
         validName := TRUE;
         goodValue := TRUE;
         update := FALSE;
         IF name = ""
            validName := FALSE;
            INC(errors);
            textSpare[errors] := "The spare pool name cannot be blank!     ";
            ASK spareCombo TO ReceiveFocus;
         ELSIF (POSITION(name, " ") <> 0)
            validName := FALSE;
            INC(errors);
            textSpare[errors] := "A spare pool name can't have blank spaces!     ";
            ASK spareCombo TO ReceiveFocus;
         ELSIF STRLEN(name) > 20
            validName := FALSE;
            INC(errors);
            textSpare[errors] := "The spare pool name must be no greater than 20 spaces!     ";
            ASK spareCombo TO SetText(SUBSTR(1,20,name));
            ASK spareCombo TO ReceiveFocus;
         END IF;
         IF (initVal.Value()< 0.0) OR (initVal.Value() > 999999999.)
            INC(errors);
            textSpare[errors] := "The initial stock level must be between 0 and 999,999,999!     ";
            ASK initVal TO SetValue(oldInitSp);
            goodValue := FALSE;
         ELSE
            poolNum := ROUND(initVal.Value());
         END IF;
         IF(createCost.Value() < 0.0) OR (createCost.Value() > 999999999.)
            INC(errors);
            textSpare[errors] := "The cost to create this spare pool must be between 0 and 999,999,999!     ";
            ASK createCost TO SetValue(oldCreateCost);
            goodValue := FALSE;
         ELSE
            spCost := createCost.Value();
         END IF;
         IF(useCost.Value() < 0.0) OR (useCost.Value() > 999999999.)
            INC(errors);
            textSpare[errors] := "The cost to ship an emergency spare must be between 0 and 999,999,999!     ";
            ASK useCost TO SetValue(oldEmerCost);
            goodValue := FALSE;
         ELSE
            useShipCost := useCost.Value();
         END IF;
         IF newChk.Checked
            IF ((newAmt.Value() < 1.) OR (newAmt.Value() > 99999.))
               INC(errors);
               textSpare[errors] := "The number of new spares must be between 1 and 99,999!     ";
               ASK newAmt TO SetValue(oldNewSp);
               goodValue := FALSE;
            ELSE
               newSpares := ROUND(newAmt.Value());
            END IF;
            IF ((newArrival.Value() < .000001) OR (newArrival.Value() > 999999999.999999))
               INC(errors);
               textSpare[errors] := "The new spares arrival rate must be between 0.000001 and 999,999,999.999999!     ";
               ASK newArrival TO SetValue(oldNewSpArr);
               goodValue := FALSE;
            ELSE
               newSparesArrival := newArrival.Value();
            END IF;
         ELSE
            newSpares := 1;
            newSparesArrival := 720.0;
         END IF;
         IF stockChk.Checked
            IF stockVal.Value() < 0.
               INC(errors);
               textSpare[errors] := "The stock level for ordering new spares must be greater than 0!     ";
               ASK stockVal TO SetValue(FLOAT(oldStkVal));
               goodValue := FALSE;
            ELSE
               stkVal := ROUND(stockVal.Value());
            END IF;
            IF ((stockAmt.Value() < 1.) OR (stockAmt.Value() > 99999.))
               INC(errors);
               textSpare[errors] := "The number of spares ordered must be between 1 and 99,999!     ";
               ASK stockAmt TO SetValue(FLOAT(oldStkAmt));
               goodValue := FALSE;
            ELSE
               stkAmt := ROUND(stockAmt.Value());
            END IF;
            IF ((stockRate.Value() < .000001) OR (stockRate.Value() > 999999999.999999))
               INC(errors);
               textSpare[errors] := "The stock arrival rate must be between 0.000001 and 999,999,999.999999!     ";
               ASK stockRate TO SetValue(oldStkRate);
               goodValue := FALSE;
            ELSE
               stkRate := stockRate.Value();
            END IF;
         ELSE
            stkVal := 0;
            stkAmt := 1;
            stkRate := 24.0;
         END IF;
         IF emerChk.Checked
            IF (emerVal.Value() < .000001) OR (emerVal.Value() > 999999999.999999)
               INC(errors);
               textSpare[errors] := "The time to receive an emergency spare must be between 0.000001 and 999,999,999.999999!     ";
               ASK emerVal TO SetValue(oldEmerTime);
               goodValue := FALSE;
            ELSE
               emerTime := emerVal.Value();
            END IF;
         ELSE
            emerTime := 24.0;
         END IF;
         IF goodValue AND validName
            FOREACH pool IN poolGroup
               IF ((pool.poolName = name) AND (pool.sparingType = type))
                  update := TRUE;
                  NEW(message2,1..2);
                  message2[1] := "Pool '"+name+"' already exists, do you     ";
                  message2[2] := "want to update the pool with the new data?     ";
                  result := SendAlert(message2, TRUE, FALSE, FALSE);
                  IF (pool.sparingType = Resource)
                     FOREACH block IN blockGroup
                        IF ((block.res1Name = name) AND (poolNum < block.numRes1)) OR (poolNum < block.numRes1PM)
                           IF (block.numRes1 > maxNum) 
                              maxNum := block.numRes1;
                           END IF;
                           IF (block.numRes1PM > maxNum)
                              maxNum := block.numRes1PM;
                           END IF;
                        END IF;
                     END FOREACH;
                  END IF;      
                  IF maxNum > 0
                     message2[1] := "There is at least one block using "+INTTOSTR(maxNum)+" "+name+"!  Reduce the number   ";
                     message2[2] := "being used or leave "+INTTOSTR(maxNum)+" "+name+" in the pool.  Edit was unsuccessful.     ";
                     result := SendAlert(message2, FALSE, FALSE, TRUE);
                     result := FALSE;
                  END IF;
                  DISPOSE(message2);
                  IF result
                     ASK pool TO RemovePool(name, type);
                     ASK poolGroup TO RemoveThis(pool);
                     NEW(pool);
                     ASK pool TO SetData(name, poolNum, newSpares, stkVal, stkAmt, newChk.Checked,
                                         emerChk.Checked, stockChk.Checked, newSparesArrival,
                                         emerTime,stkRate,spCost,useShipCost);
                     somethingChanged := TRUE;
                     EXIT;
                  END IF;
               END IF;
            END FOREACH;                  
         END IF;
         IF ((validName) AND (goodValue) AND (NOT update))
            NEW(pool);
            ASK pool TO SetData(name, poolNum, newSpares, stkVal, stkAmt, newChk.Checked,
                                emerChk.Checked, stockChk.Checked, newSparesArrival,
                                emerTime, stkRate, spCost, useShipCost);
            somethingChanged := TRUE;
         END IF;
         DISPOSE(message);
         GetSpareList(spareList);
         IF validName AND goodValue AND (lastClicked.ReferenceName <> "OKButton")
            ASK spareCombo TO SetOptions(spareList);
            ASK spareCombo TO SetText("unnamed");
            ASK createCost TO SetValue(1.);
            ASK useCost TO SetValue(1.);
            ASK initVal TO SetValue(1.);
            ASK newAmt TO SetValue(1.);
            ASK newArrival TO SetValue(720.);
            ASK emerVal TO SetValue(24.);
            ASK newChk TO SetCheck(FALSE);
            ASK emerChk TO SetCheck(FALSE);
            ASK newAmt TO Deactivate;
            ASK newArrival TO Deactivate;
            ASK emerVal TO Deactivate;
            ASK stockChk TO SetCheck(FALSE);
            ASK stockVal TO SetValue(0.);
            ASK stockVal TO  Deactivate;
            ASK stockAmt TO SetValue(1.);
            ASK stockAmt TO Deactivate;
            ASK stockRate TO SetValue(720.);
            ASK stockRate TO Deactivate;
         END IF;
      ELSIF lastClicked.ReferenceName = "DeleteButton"
         name := spareCombo.Text();
         IF name <> "unnamed"
           NEW(message, 1..1);      
           NEW(message2, 1..3);
           blockUsesPool := FALSE;
           cancelDelete := FALSE;
           block := ASK root Child("RBDBlock", 0);
           IF (block.poolName = name)
              defaultUsesPool := TRUE;
           END IF;   
           FOREACH block IN blockGroup
              IF (block.poolName = name)
                 blockUsesPool := TRUE;
                 EXIT;
              END IF;
           END FOREACH;
           FOREACH pool IN poolGroup
              IF ((pool.poolName = name) AND (pool.sparingType = type))
                 IF (blockUsesPool)
                    message2[1] := "One or more blocks use the '"+name+"' spare pool.     ";
                    message2[2] := "If you delete it, these blocks will now have infinite spares.     ";
                    message2[3] := "Are you sure you want to delete the '"+name+"' spare pool?     ";
                    result := SendAlert(message2, TRUE, FALSE, FALSE);
                 ELSIF(defaultUsesPool)
                    message2[1] := "Your default block uses the '"+name+"' spare pool.     ";
                    message2[2] := "If you delete it, the default block will have infinite spares.     ";
                    message2[3] := "Are you sure you want to delete the '"+name+"' spare pool?     ";
                    result := SendAlert(message2, TRUE, FALSE, FALSE);
                 ELSE
                    message[1] := "Are you sure you want to delete the '"+name+"' spare pool?     ";
                    result := SendAlert(message, TRUE, FALSE, FALSE);
                 END IF;
                 IF result
                    ASK pool TO RemovePool(name, type);
                    ASK poolGroup TO RemoveThis(pool);
                    ASK initVal TO DisplayValue(1.);
                    ASK createCost TO DisplayValue(1.);
                    ASK useCost TO DisplayValue(1.);
                    ASK spareCombo TO SetOptions(spareList);
                    ASK spareCombo TO SetText("unnamed");
                    ASK newAmt TO DisplayValue(1.);
                    ASK newArrival TO DisplayValue(720.);
                    ASK emerVal TO DisplayValue(24.);
                    ASK newAmt TO Deactivate;
                    ASK newArrival TO Deactivate;
                    ASK emerVal TO Deactivate;
                    ASK newChk TO SetCheck(FALSE);
                    ASK emerChk TO SetCheck(FALSE);
                    somethingChanged := TRUE;
                    IF (defaultUsesPool)
                       block := ASK root Child("RBDBlock", 0);
                       ASK block TO SetSparingInfinite;
                    END IF;   
                    IF (blockUsesPool)
                       FOREACH block IN blockGroup
                          IF (block.poolName = name)
                             ASK block TO SetSparingInfinite;
                          END IF;
                       END FOREACH;
                    END IF;
                 ELSE
                    cancelDelete := TRUE;
                 END IF;
              END IF;
           END FOREACH;
           DISPOSE(message);
           DISPOSE(message2);
           GetSpareList(spareList);
         END IF;
      END IF;
      IF errors > 1
         sparesDone := FALSE;
         NEW(message, 1..errors+2);
         message[1] := "The following errors must be corrected:   ";
         message[2] := "";
         FOR i := 1 TO errors
            message[i+2] := "   "+INTTOSTR(i)+". "+textSpare[i];
         END FOR;
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      ELSIF errors = 1
         sparesDone := FALSE;
         NEW(message, 1..1);
         message[1] := textSpare[1];
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      END IF;
      DISPOSE(textSpare);
      Update;
   END METHOD; {BeSelected}
END OBJECT; {SparePoolsBoxObj}

OBJECT ResPoolsBoxObj;
   ASK METHOD ReceiveData;
   BEGIN
      resCombo       := Descendant("ResComboBox", 101);
      initVal        := Descendant("InitVal", 102);
      createCost     := Descendant("CreateCost", 103);
      useCost        := Descendant("UseCost", 104);
      fixedCost      := Descendant("FixedCost", 105);
      unitLabel      := Descendant("UnitLabel", 106);
      
      GetResList(resList);
      ASK resCombo TO SetOptions(resList);
      ASK resCombo TO SetText("unnamed");
      ASK initVal TO SetValue(1.);
      ASK createCost TO SetValue(1.);
      ASK fixedCost TO SetValue(1.);
      ASK useCost TO SetValue(1.);
      ASK unitLabel TO SetLabel(systemUnits);
      Draw;
      ASK resCombo TO ReceiveFocus;
      REPEAT
         button := AcceptInput();
      UNTIL resDone; 
   END METHOD; {ReceiveData -- ResPoolsBoxObj}

   {BeSelected -- ResPoolsBoxObj}
   ASK METHOD BeSelected;
   VAR
      result,update,validName,goodValue,
      blockUsesRes,cancelDelete, defaultUsesRes : BOOLEAN;
      i, radButtonId,poolNum,newSpares,maxNum,
      stkVal,stkAmt,oldStkAmt,oldStkVal       : INTEGER;
      newSparesArrival,emerTime,stkRate,
      oldCreateCost,oldNewSp,oldEmerCost,
      oldNewSpArr,oldEmerTime,oldUseCost,
      oldInitSp,oldFixCost,spCost,
      fixCost,useShipCost,oldStkRate          : REAL;
      pool                                    : SparePoolObj;
      name                                    : STRING;
      type                                    : SparingType;
      message2                                : TextBufferType;
      block                                   : RBDBlockObj;
      textRes                                : ARRAY INTEGER OF STRING;
   BEGIN
      resDone := TRUE;
      NEW(textRes, 1..20);
      errors := 0;
      type := Resource;
      lastClicked := ASK SELF LastPicked;
      name := resCombo.Text();
      CASE lastClicked.Id
         WHEN 46:
            CallHelp(46);
         WHEN 101:         
            IF name = ""
               ASK resCombo TO SetText("unnamed");
            ELSIF name <> "unnamed"
               FOREACH pool IN poolGroup
                  IF ((name = pool.poolName) AND (pool.sparingType = type))
                     oldInitSp := FLOAT(pool.initialSpares);
                     oldCreateCost := pool.spareCost;
                     oldUseCost := pool.costPerTime;
                     oldFixCost := pool.fixedPerUse;
                     EXIT;
                  END IF;
               END FOREACH;
               ASK initVal TO SetValue(oldInitSp);
               ASK createCost TO SetValue(oldCreateCost);
               ASK useCost TO SetValue(oldUseCost);
               ASK fixedCost TO SetValue(oldFixCost);
            END IF;
         OTHERWISE;
      END CASE;
      IF ((lastClicked.ReferenceName = "CreateButton") OR (lastClicked.ReferenceName = "OKButton"))
         AND ((name <> "Unnamed") AND (name <> "unnamed"))
         GetResList(resList);
         validName := TRUE;
         goodValue := TRUE;
         update := FALSE;
         IF name = ""
            validName := FALSE;
            INC(errors);
            textRes[errors] := "The resource pool name cannot be blank!     ";
            ASK resCombo TO ReceiveFocus;
         ELSIF (POSITION(name, " ") <> 0)
            validName := FALSE;
            INC(errors);
            textRes[errors] := "A resource pool name can't have blank spaces!     ";
            ASK resCombo TO ReceiveFocus;
         ELSIF STRLEN(name) > 20
            validName := FALSE;
            INC(errors);
            textRes[errors] := "The resource pool name must be no greater than 20 spaces!     ";
            ASK resCombo TO SetText(SUBSTR(1,20,name));
            ASK resCombo TO ReceiveFocus;
         END IF;
         IF (initVal.Value()< 0.0) OR (initVal.Value() > 9999.)
            INC(errors);
            textRes[errors] := "The number of resources must be between 0 and 9,999!     ";
            ASK initVal TO SetValue(oldInitSp);
            goodValue := FALSE;
         ELSE
            poolNum := ROUND(initVal.Value());
         END IF;
         IF(createCost.Value() < 0.0) OR (createCost.Value() > 999999999.)
            INC(errors);
            textRes[errors] := "The cost to create this pool must be between 0 and 999,999,999!     ";
            ASK createCost TO SetValue(oldCreateCost);
            goodValue := FALSE;
         ELSE
            spCost := createCost.Value();
         END IF;
         IF(fixedCost.Value() < 0.0) OR (fixedCost.Value() > 999999999.)
            INC(errors);
            textRes[errors] := "The fixed cost of using this resource must be between 0 and 999,999,999!     ";
            ASK fixedCost TO SetValue(oldFixCost);
            goodValue := FALSE;
         ELSE
            fixCost := fixedCost.Value();
         END IF;
         IF(useCost.Value() < 0.0) OR (useCost.Value() > 999999999.)
            INC(errors);
            textRes[errors] := "The cost of using this resource per "+systemUnits+" must be between 0 and 999,999,999!     ";
            ASK useCost TO SetValue(oldUseCost);
            goodValue := FALSE;
         ELSE
            useShipCost := useCost.Value();
         END IF;
         IF goodValue AND validName
            FOREACH pool IN poolGroup
               IF ((pool.poolName = name) AND (pool.sparingType = type))
                  update := TRUE;
                  NEW(message2,1..2);
                  message2[1] := "Resource pool '"+name+"' already exists, do you     ";
                  message2[2] := "want to update the pool with the new data?     ";
                  result := SendAlert(message2, TRUE, FALSE, FALSE);
                  IF (pool.sparingType = Resource)
                     FOREACH block IN blockGroup
                        IF ((block.res1Name = name) AND (poolNum < block.numRes1)) OR (poolNum < block.numRes1PM)
                           IF (block.numRes1 > maxNum) 
                              maxNum := block.numRes1;
                           END IF;
                           IF (block.numRes1PM > maxNum)
                              maxNum := block.numRes1PM;
                           END IF;
                        END IF;
                     END FOREACH;
                  END IF;      
                  IF maxNum > 0
                     message2[1] := "There is at least one block using "+INTTOSTR(maxNum)+" "+name+"!  Reduce the number   ";
                     message2[2] := "being used or leave "+INTTOSTR(maxNum)+" "+name+" in the pool.  Edit was unsuccessful.     ";
                     result := SendAlert(message2, FALSE, FALSE, TRUE);
                     result := FALSE;
                  END IF;
                  DISPOSE(message2);
                  IF result
                     ASK pool TO RemovePool(name, type);
                     ASK poolGroup TO RemoveThis(pool);
                     NEW(pool);
                     ASK pool TO SetResData(name, poolNum, spCost, fixCost, useShipCost, FALSE);
                     somethingChanged := TRUE;
                     EXIT;
                  END IF;
               END IF;
            END FOREACH;                  
         END IF;
         IF ((validName) AND (goodValue) AND (NOT update))
            NEW(pool);
            ASK pool TO SetResData(name, poolNum, spCost, fixCost, useShipCost, FALSE);
            somethingChanged := TRUE;
         END IF;
         DISPOSE(message);
         GetResList(resList);
         IF validName AND goodValue AND (lastClicked.ReferenceName <> "OKButton")
            ASK resCombo TO SetOptions(resList);
            ASK resCombo TO SetText("unnamed");
            ASK createCost TO SetValue(1.);
            ASK useCost TO SetValue(1.);
            ASK fixedCost TO SetValue(1.);
            ASK initVal TO SetValue(1.);
         END IF;
      ELSIF lastClicked.ReferenceName = "DeleteButton"
         name := resCombo.Text();
         IF name <> "unnamed"
           NEW(message, 1..1);      
           NEW(message2, 1..3);
           blockUsesRes  := FALSE;
           cancelDelete := FALSE;
           block := ASK root Child("RBDBlock", 0);
           IF (block.res1Name = name)
              defaultUsesRes := TRUE;
           END IF;   
           FOREACH block IN blockGroup
              IF (block.res1Name = name)
                 blockUsesRes := TRUE;
                 EXIT;
              END IF;
           END FOREACH;
           FOREACH pool IN poolGroup
              IF ((pool.poolName = name) AND (pool.sparingType = type))
                 IF (blockUsesRes)
                    message2[1] := "One or more blocks use the '"+name+"' resource.     ";
                    message2[2] := "If you delete it, these blocks will not require a resource.     ";
                    message2[3] := "Are you sure you want to delete the '"+name+"' resource?     ";
                    result := SendAlert(message2, TRUE, FALSE, FALSE);
                 ELSIF (defaultUsesRes)
                    message2[1] := "Your default block uses the '"+name+"' resource.     ";
                    message2[2] := "If you delete it, the default block will not require a resource.     ";
                    message2[3] := "Are you sure you want to delete the '"+name+"' resource?     ";
                    result := SendAlert(message2, TRUE, FALSE, FALSE);
                 ELSE
                    message[1] := "Are you sure you want to delete the '"+name+"' resource?     ";
                    result := SendAlert(message, TRUE, FALSE, FALSE);
                 END IF;
                 IF result
                    ASK pool TO RemovePool(name, type);
                    ASK poolGroup TO RemoveThis(pool);
                    ASK initVal TO DisplayValue(1.);
                    ASK createCost TO DisplayValue(1.);
                    ASK fixedCost TO DisplayValue(1.);
                    ASK useCost TO DisplayValue(1.);
                    ASK resCombo TO SetOptions(resList);
                    ASK resCombo TO SetText("unnamed");
                    somethingChanged := TRUE;
                    IF (defaultUsesRes)
                       block := ASK root Child("RBDBlock", 0);
                       ASK block TO SetNoResources;
                    END IF;  
                    IF (blockUsesRes)
                       FOREACH block IN blockGroup
                          IF (block.res1Name = name)
                             ASK block TO SetNoResources;
                          END IF;
                       END FOREACH;
                    END IF;
                 ELSE
                    cancelDelete := TRUE;
                 END IF;
              END IF;
           END FOREACH;
           DISPOSE(message);
           DISPOSE(message2);
           GetResList(resList);
         END IF;
      END IF;
      IF errors > 1
         resDone := FALSE;
         NEW(message, 1..errors+2);
         message[1] := "The following errors must be corrected:   ";
         message[2] := "";
         FOR i := 1 TO errors
            message[i+2] := "   "+INTTOSTR(i)+". "+textRes[i];
         END FOR;
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      ELSIF errors = 1
         resDone := FALSE;
         NEW(message, 1..1);
         message[1] := textRes[1];
         result := SendAlert(message, FALSE, FALSE, TRUE);
         DISPOSE(message);
      END IF;
      DISPOSE(textRes);
      Update;
   END METHOD; {BeSelected}
END OBJECT; {ResPoolsBoxObj}

OBJECT TreeBoxObj;
   ASK METHOD ReceiveData;
   VAR
      treeItem, treeItem2                 : TreeItemObj;
      tempItem, tempItem2 : TreeItemObj;
      theTree, tempTree                     : TreeObj;
      tempObj                   : ANYOBJ;
      counter, main, i                        : INTEGER;
      tempHier, tempHier2, tempHier3 : RBDHierObj;
      tempBlock : RBDBlockObj;
      tempEvent : RBDEventObj;
      obj : ANYOBJ;
      child : RBDBasicObj;
      tempNode : RBDNodeObj;
   BEGIN
      theTree := Descendant("HierTree", 100);
      NEW(tempTree);
      FOR i:=1 TO deepestLevel
         FOREACH tempHier IN hierGroup
            IF tempHier.level = i;
               counter := 0;
               NEW(treeItem);
               ASK treeItem TO SetId(tempHier.Id);
               ASK treeItem TO SetReferenceName("branch");
               ASK treeItem TO SetLabel("Hier-" + INTTOSTR(tempHier.Id) + " " + tempHier.name); 
               IF devmode
                  ASK treeItem TO SetLabel("Hier-" + INTTOSTR(tempHier.Id) + " " + tempHier.name + "-" 
                                           + INTTOSTR(tempHier.childGroup.numberIn) + " childs" + "(" + INTTOSTR(counter) + " links)");
               END IF;
               FOREACH obj IN tempHier.childGroup
                  IF OBJTYPENAME(obj) = "LinkObj"
                     INC(counter);
                  ELSE
                     child := obj;
                     IF OBJTYPENAME(child) = "RBDBlockObj"
                        tempBlock := ASK root Child("RBDBlock", child.Id);
                        NEW(treeItem2);
                        ASK treeItem2 TO SetId(tempBlock.Id);
                        ASK treeItem2 TO SetReferenceName("branch");
                        ASK treeItem2 TO SetLabel("Block-" + INTTOSTR(tempBlock.Id) + " " + tempBlock.name);
                        {ASK treeItem2 TO SetIconName("C:\RaptorXDev\Graphics\Block.bmp");}
                        ASK treeItem TO AddGraphic(treeItem2);
                     ELSIF OBJTYPENAME(child) = "RBDEventObj"
                        tempEvent := ASK root Child("RBDEvent", child.Id);
                        NEW(treeItem2);
                        ASK treeItem2 TO SetId(tempEvent.Id);
                        ASK treeItem2 TO SetReferenceName("branch");
                        ASK treeItem2 TO SetLabel("Event-" + INTTOSTR(tempEvent.Id) + " " + tempEvent.name);
                        {ASK treeItem2 TO SetIconName("C:\RaptorXDev\Graphics\Event.bmp");}
                        ASK treeItem TO AddGraphic(treeItem2);
                     ELSIF OBJTYPENAME(child) = "RBDNodeObj"
                        tempNode := ASK root Child("RBDNode", child.Id);
                        NEW(treeItem2);
                        ASK treeItem2 TO SetId(tempNode.Id);
                        ASK treeItem2 TO SetReferenceName("branch");
                        ASK treeItem2 TO SetLabel("Node-" + INTTOSTR(tempNode.Id) + " " + tempNode.name);
                        {ASK treeItem2 TO SetIconName("C:\RaptorXDev\Graphics\Node.bmp");}
                        ASK treeItem TO AddGraphic(treeItem2);
                     END IF;
                  END IF;
               END FOREACH; 
               IF (i = 1) {First level (all these hiers have no parent)}
                  INC(main);
                  ASK tempTree TO AddGraphic(treeItem);
               ELSE {hier has parent}
                  tempHier2 := ASK root Child("RBDHier", tempHier.parentID);
                  treeItem2 := ASK tempTree Descendant("branch", tempHier2.Id);
                  ASK treeItem2 TO AddGraphic(treeItem);
               END IF;
            END IF;
         END FOREACH;
         Draw;
      END FOR;
      Draw;
      FOREACH tempBlock IN blockGroup
         NEW(treeItem2);
         ASK treeItem2 TO SetLabel("Block-" + INTTOSTR(tempBlock.Id) + " " + tempBlock.name);
         IF tempBlock.parentID = 0
            INC(main);
            ASK tempTree TO AddGraphic(treeItem2);
         END IF; 
         Draw;
      END FOREACH;
      FOREACH tempEvent IN eventGroup
         NEW(treeItem2);
         ASK treeItem2 TO SetLabel("Event-" + INTTOSTR(tempEvent.Id) + " " + tempEvent.name);
         IF tempEvent.parentID = 0
            INC(main);
            ASK tempTree TO AddGraphic(treeItem2);
         END IF; 
         Draw;
      END FOREACH;
      FOREACH tempNode IN nodeGroup
         NEW(treeItem2);
         ASK treeItem2 TO SetLabel("Node-" + INTTOSTR(tempNode.Id) + " " + tempNode.name);
         IF tempNode.parentID = 0
            INC(main);
            ASK tempTree TO AddGraphic(treeItem2);
         END IF; 
         Draw;
      END FOREACH;
      obj := ASK tempTree FirstGraphic();
      FOR i := 1 TO main
         NEW(tempItem);
         tempItem := CLONE(obj);
         IF (SUBSTR(1, 5, tempItem.Label) = "Block")
            ASK theTree TO AddGraphic(tempItem);
         END IF;
         obj := ASK tempTree NextGraphic(obj);
      END FOR;
      obj := ASK tempTree FirstGraphic();
      FOR i := 1 TO main
         NEW(tempItem);
         tempItem := CLONE(obj);
         IF (SUBSTR(1, 5, tempItem.Label) = "Event")
            ASK theTree TO AddGraphic(tempItem);
         END IF;
         obj := ASK tempTree NextGraphic(obj);
      END FOR;
      obj := ASK tempTree FirstGraphic();
      FOR i := 1 TO main
         NEW(tempItem);
         tempItem := CLONE(obj);
         IF (SUBSTR(1, 4, tempItem.Label) = "Node")
            ASK theTree TO AddGraphic(tempItem);
         END IF;
         obj := ASK tempTree NextGraphic(obj);
      END FOR;
      obj := ASK tempTree FirstGraphic();
      FOR i := 1 TO main
         NEW(tempItem);
         tempItem := CLONE(obj);
         IF (SUBSTR(1, 4, tempItem.Label) = "Hier")
            ASK theTree TO AddGraphic(tempItem);
         END IF;
         obj := ASK tempTree NextGraphic(obj);
      END FOR; 
      Draw;
      button := AcceptInput(); 
   END METHOD;
   
   ASK METHOD BeSelected;
   BEGIN
      lastClicked := ASK SELF LastPicked;
      IF lastClicked.ReferenceName = "PrintButton"
         IF compileType <> "demo"
            PrintTree;
         ELSE
            NEW(message, 1..1);
            message[1] := "The demo version has no print capability     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
      END IF;
   END METHOD;
END OBJECT;

OBJECT FailEffectsBoxObj;
   ASK METHOD ReceiveData;
   VAR
      failEffectsList                : ListBoxObj;
      listItem                       : ListBoxItemObj;
      writeButton                    : ButtonObj;
      menuItem                       : MenuItemObj;
      i                              : INTEGER;
   BEGIN
      failEffectsList := Descendant("FailEffectsList", 101);
      FOR i := 1 TO HIGH(FMECAarray)
         IF FMECAarray[i]  = "END_OF_ARRAY"
            EXIT;
         END IF;
         NEW(listItem);
         ASK listItem TO SetText(FMECAarray[i]);
         ASK failEffectsList TO AddChild(listItem, "item", 0);
      END FOR;
      writeButton := Descendant("WriteButton", 103);
      menuItem := ASK fevMenuBar Descendant("OpenItem", 101);
      IF (menuItem.Label = "&Open Interactive Log...")
         Draw;
         ASK writeButton TO Deactivate;
      END IF;
      button := AcceptInput();

   END METHOD;
   
   ASK METHOD BeSelected;
   BEGIN
      lastClicked := ASK SELF LastPicked;

      IF lastClicked.ReferenceName = "WriteButton"
         IF compileType <> "demo"
            WriteFMECAtoFile;
         ELSE
            NEW(message, 1..1);
            message[1] := "The demo version has no save capability     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
      ELSIF lastClicked.ReferenceName = "PrintButton"
         IF compileType <> "demo"
            PrintFMECA(FMECAarray);
         ELSE
            NEW(message, 1..1);
            message[1] := "The demo version has no print capability     ";
            result := SendAlert(message, FALSE, FALSE, TRUE);
            DISPOSE(message);
         END IF;
      END IF;
   END METHOD;
   
END OBJECT;

OBJECT GotoHierBoxObj;
   ASK METHOD ReceiveData;
   VAR
      hierList : OptionListType;
      i, hierId : INTEGER;
      tempHier : RBDHierObj;
   BEGIN
      hierComboBox := Descendant("HierComboBox", 101);
      IF totalHiers > 0
         i := 1;
         NEW(hierList, 1..totalHiers);
         FOREACH tempHier IN hierGroup
            IF tempHier.Id < 10
               tempString := "00" + INTTOSTR(tempHier.Id);
            ELSIF tempHier.Id < 100
               tempString := "0" + INTTOSTR(tempHier.Id);
            ELSE
               tempString := INTTOSTR(tempHier.Id);
            END IF;
            hierList[i] := tempHier.name + " - " + tempString;
            INC(i);
         END FOREACH;
      END IF;
      ASK hierComboBox TO SetOptions(hierList);
      ASK hierComboBox TO SetText(hierList[1]);

      button := AcceptInput();
      IF button.ReferenceName = "OKButton"
         i := POSITION(hierComboBox.Text, " -");
         hierId := STRTOINT(SUBSTR(i+2, i+11, hierComboBox.Text + "      "));
         tempHier := ASK root Child("RBDHier", hierId);
         ChangeWindow(hierId, tempHier.level);
      END IF;

   END METHOD;
   
   ASK METHOD BeSelected;
   VAR
      hierId, i : INTEGER;
      tempHier : RBDHierObj;
   BEGIN
      lastClicked := ASK SELF LastPicked;
      CASE lastClicked.Id
         WHEN 101:
            i := POSITION(hierComboBox.Text, " -");
            hierId := STRTOINT(SUBSTR(i+2, i+11, hierComboBox.Text + "      "));
            tempHier := ASK root Child("RBDHier", hierId);
            ChangeWindow(hierId, tempHier.level);
         WHEN 30:
            CallHelp(30);
         OTHERWISE;
      END CASE;
      IF lastClicked.ReferenceName = "CancelButton"
      END IF;
   END METHOD;
   
END OBJECT;

OBJECT FailRepairBoxObj;
   ASK METHOD ReceiveData(IN failOrRep : STRING);
   VAR
      failrepList : OptionListType;
      i : INTEGER;
      tempBlock : RBDBlockObj;
      tempEvent : RBDEventObj;
   BEGIN
      failOrRepair := failOrRep;
      failRepairComboBox := Descendant("FailRepairComboBox", 101);
      IF (totalBlocks+totalEvents) > 0
         NEW(failrepList, 1..(totalBlocks + totalEvents));
         i := 1;
         FOREACH tempBlock IN blockGroup
            IF tempBlock.Id < 10
               tempString := "00" + INTTOSTR(tempBlock.Id);
            ELSIF tempBlock.Id < 100
               tempString := "0" + INTTOSTR(tempBlock.Id);
            ELSE
               tempString := INTTOSTR(tempBlock.Id);
            END IF;
            failrepList[i] := tempBlock.name + " block - " + tempString;
            INC(i);
         END FOREACH;
         FOREACH tempEvent IN eventGroup
            IF tempEvent.Id < 10
               tempString := "00" + INTTOSTR(tempEvent.Id);
            ELSIF tempEvent.Id < 100
               tempString := "0" + INTTOSTR(tempEvent.Id);
            ELSE
               tempString := INTTOSTR(tempEvent.Id);
            END IF;
            failrepList[i] := tempEvent.name + " event - " + tempString;
            INC(i);
         END FOREACH;
         ASK failRepairComboBox TO SetOptions(failrepList);
         ASK failRepairComboBox TO SetText(failrepList[1]);
      END IF;

      button := AcceptInput();
   END METHOD;
   
   ASK METHOD BeSelected;
   VAR
      blockId, eventId, i : INTEGER;
      tempBlock, fevBlock : RBDBlockObj;
      tempEvent : RBDEventObj;
   BEGIN
      lastClicked := ASK SELF LastPicked;
      IF ((lastClicked.Id = 101) OR (lastClicked.ReferenceName = "OKButton"))
            i := POSITION(failRepairComboBox.Text, " -");
            IF (SUBSTR(i-1,i-1, failRepairComboBox.Text) = "k")
               blockId := STRTOINT(SUBSTR(i+2, i+11, failRepairComboBox.Text + "      "));
               tempBlock := ASK root Child("RBDBlock", blockId);
               IF (failOrRepair = "fail")
                  IF ((tempBlock.opStatus = Running) OR ((tempBlock.opStatus = Standby) AND (tempBlock.sbStress > 0.)))
                     IF (NOT(FevDepGroup.Includes(tempBlock)))
                        IF (FevDepGroup.numberIn>0)                          
                           FOREACH fevBlock IN FevDepGroup
                              ASK fevBlock TO SetFevDepMode(FALSE);
                              ASK FevDepGroup TO RemoveThis(fevBlock);
                           END FOREACH;
                        END IF;
                        critFactor := 1.0;
                        ASK tempBlock TO ChangeBlockState(Repairing,tempBlock.activeStatus,"");
                        CheckColds;
                        AnalyzeSystem;
                     END IF;
                  END IF;
               ELSIF (failOrRepair = "repair")
                  IF ((tempBlock.opStatus = Repairing) OR (tempBlock.opStatus = PM))
                     IF (NOT(FevDepGroup.Includes(tempBlock)))
                        IF (FevDepGroup.numberIn>0)                          
                           FOREACH fevBlock IN FevDepGroup
                              ASK fevBlock TO SetFevDepMode(FALSE);
                              ASK FevDepGroup TO RemoveThis(fevBlock);
                           END FOREACH;
                        END IF;
                        ASK tempBlock TO ChangeBlockState(Running,tempBlock.activeStatus,"");
                        CheckColds;
                        AnalyzeSystem;
                     END IF;
                  END IF;
               END IF;
            ELSIF (SUBSTR(i-1,i-1, failRepairComboBox.Text) = "t")
               eventId := STRTOINT(SUBSTR(i+2, i+11, failRepairComboBox.Text + "      "));
               tempEvent := ASK root Child("RBDEvent", eventId);
               {If failing and event is in a state where it can fail}
               IF ((failOrRepair = "fail") AND (tempEvent.opStatus=Success))
                  IF (FevDepGroup.numberIn>0)                          
                     FOREACH fevBlock IN FevDepGroup
                        ASK fevBlock TO SetFevDepMode(FALSE);
                        ASK FevDepGroup TO RemoveThis(fevBlock);
                     END FOREACH;
                  END IF;
                  critFactor := 1.0;
                  ASK tempEvent TO ChangeEventState(Failure,tempEvent.activeStatus,"");
                  CheckColds;
                  AnalyzeSystem;
               ELSIF ((failOrRepair = "repair") AND (tempEvent.opStatus=Failure))
                  IF (FevDepGroup.numberIn>0)                          
                     FOREACH fevBlock IN FevDepGroup
                        ASK fevBlock TO SetFevDepMode(FALSE);
                        ASK FevDepGroup TO RemoveThis(fevBlock);
                     END FOREACH;
                  END IF;
                  ASK tempEvent TO ChangeEventState(Success,tempEvent.activeStatus,"");
                  CheckColds;
                  AnalyzeSystem;
               END IF;
           END IF;
      ELSIF lastClicked.Id = 28
         IF (failOrRepair = "fail")
            CallHelp(28);
         ELSE
            CallHelp(53);
         END IF;
      END IF;
      IF lastClicked.ReferenceName = "CancelButton"
      END IF;
   END METHOD;
   
END OBJECT;

END MODULE. {imod Dialogs}
