{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Implementation Module : IPrint                                                +}
{+  Author                : Steve Brown / Tony Malerich                           +}
{+  Last Modified         : October 2008  Chuck Carter                            +}
{+                                                                                +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}






IMPLEMENTATION MODULE Print;

FROM IOMod    IMPORT StreamObj,FileUseType(Output,Input,Append);
FROM Text     IMPORT TextObj;
FROM Objects  IMPORT RBDBlockObj,LinkObj,RBDNodeObj,RBDHierObj,RBDBasicObj,RBDEventObj;
FROM Window   IMPORT WindowObj;
FROM GPrint   IMPORT PrinterObj, ALL PrintOrientationType;
FROM GTypes   IMPORT TextBufferType, ALL ColorType,TextHorizType(HorizLeft),ALL SysCursorType,
                     TextVertType(VertMiddle),TextFontType(SystemFont,RomanFont,
                     CourierFont,SystemText), ALL FillStyleType;
FROM OSMod    IMPORT SystemCall, FileExists, DeleteFile, CopyFile, AppendSlash,GetCurrentDrive;
FROM MathMod  IMPORT CEIL;               
FROM Runsim   IMPORT FinalArray,NumRunsCompleted;
FROM FileFx   IMPORT GetFileName;
FROM Image    IMPORT ImageObj;
FROM UtilMod  IMPORT DateTime;
FROM Intract  IMPORT HelpBoxObj,SendAlert,doorOpen; 
FROM Button   IMPORT ButtonObj;
FROM Display  IMPORT root,nameOfFile,window,selectGroup,totalBlocks,totalNodes,dialogs,diagFile, 
                     filter,activePhases,selectedLinksGroup,linksIn,AoVisible,termType, 
                     capacityAnalysis,nowSimulating,diagnostics,diagnosticsStream,weakLinkAnalType,
                     totalHiers,totalEvents,blockGroup,eventGroup,nodeGroup,hierGroup,linkGroup;
FROM Fill     IMPORT FillVObj;

FROM Display  IMPORT userPath;   { wds/TES, 8/18/08 }

VAR
   bodyText, titleText, footerText,
   headerText, separatorText,
   titleText1, titleText2           : TextObj;
   textBuffer, footBuffer, message  : TextBufferType;
   i, numPages, j, k, m, n, column  : INTEGER;
   outputText                       : ImageObj;
   printWindow                      : WindowObj;
   printer                          : PrinterObj;
   Date, text, tempFile, tempPath,
   runString,durationStr            : STRING;
   scale                            : REAL;
   dialogBox                        : HelpBoxObj;
   fileIsThere, goodName            : BOOLEAN;
   button                           : ButtonObj;
   exportFile                       : StreamObj;



{wds/TES - mods for printing/convert RBD from user directory, 8/28/08}
{cannot use a SystemCall because RapPrin70.exe expects a simple target filename (not incl. directory path) in its directory }
PROCEDURE PrintRBP();   NONMODSIM "CPP";
PROCEDURE ConvertRBP(); NONMODSIM "CPP";



PROCEDURE Setup(IN size : INTEGER);
VAR
   sepOff, titleOff : REAL;
BEGIN
   j := 0;
   column := 0;
   IF size = 1
      sepOff := 100.;
   ELSIF (size = 2) 
      titleOff := 350.;
   ELSIF (size=13)    {portrait}
      titleOff := 10000.0;
      sepOff := 4000.0;
   ELSE
      titleOff := 950.;
   END IF;
   DateTime(Date);
   NEW(printer);
   IF size=13
      ASK printer TO SetOrientation(PortraitOrientation);
   ELSE
      ASK printer TO SetOrientation(LandscapeOrientation);
   END IF;   
   ASK printer TO SetSizeToFit(FALSE);
   NEW(printWindow);
   ASK printWindow TO SetTranslation(100.,0.);
   ASK printWindow TO Draw;
   scale := printWindow.PixelWidth/44.221323;
   IF size=13
      ASK printer TO SetPortion(0.,0.,32767.*scale, 42000.*scale); 
   ELSE
      ASK printer TO SetPortion(0.,0.,32767.*scale, 32767.*scale); 
   END IF;
   NEW (headerText);
   NEW (titleText);
   NEW (titleText1);
   NEW (titleText2);
   NEW (separatorText);
   NEW (footerText);
   NEW (bodyText);
   NEW (outputText);
   ASK headerText TO SetColor(Black);  
   ASK headerText TO SetAlignment(HorizLeft,VertMiddle); 
   ASK headerText TO SetSysFont("Courier New", 11, 94,0);
   IF size=13
      ASK headerText TO SetTranslation(2500.*scale,40000.*scale);
   ELSE
      ASK headerText TO SetTranslation(2500.*scale,30400.*scale);
   END IF;
   ASK headerText TO SetFont(SystemText);
   ASK titleText TO SetColor(Black);  
   ASK titleText TO SetAlignment(HorizLeft,VertMiddle); 
   ASK titleText TO SetSysFont("Courier New", 9, 61,0);
   ASK titleText TO SetTranslation(2500.*scale,(28100.+titleOff)*scale);
   ASK titleText TO SetFont(SystemText);
   ASK titleText2 TO SetColor(Black);                
   ASK titleText2 TO SetAlignment(HorizLeft,VertMiddle); 
   ASK titleText2 TO SetSysFont("Courier New", 9, 61,0);
   ASK titleText2 TO SetTranslation(2500.*scale,28000.*scale);
   ASK titleText2 TO SetFont(SystemText);
   ASK separatorText TO SetColor(Black);   
   ASK separatorText TO SetAlignment(HorizLeft,VertMiddle); 
   ASK separatorText TO SetSysFont("Courier New", 9, 61,0);
   ASK separatorText TO SetTranslation(2500.*scale,(27850.+sepOff)*scale);
   ASK separatorText TO SetFont(SystemText);
   NEW(footBuffer, 1..2);
   ASK footerText TO SetColor(Black); 
   ASK footerText TO SetAlignment(HorizLeft,VertMiddle); 
   ASK footerText TO SetSysFont("Courier New", 8, 65,0);
   ASK footerText TO SetTranslation(2500.*scale,2350.*scale);
   ASK footerText TO SetFont(SystemText);
   footBuffer[1] := nameOfFile;
   ASK bodyText TO SetColor(Black);  
   ASK bodyText TO SetAlignment(HorizLeft,VertMiddle); 
   ASK bodyText TO SetSysFont("Courier New", 9, 51,0);
   ASK bodyText TO SetTranslation(2500.*scale,(15850.+sepOff)*scale);
   ASK bodyText TO SetFont(SystemText);
   ASK outputText TO AddGraphic(titleText);
   ASK outputText TO AddGraphic(titleText2);
   ASK outputText TO AddGraphic(headerText);
   ASK outputText TO AddGraphic(separatorText);
   ASK outputText TO AddGraphic(footerText);
   ASK outputText TO AddGraphic(bodyText);
   ASK printWindow TO AddGraphic(outputText);
END PROCEDURE;

PROCEDURE Cleanup;
BEGIN
   DISPOSE(footBuffer);
   DISPOSE(bodyText);
   DISPOSE(footerText);
   DISPOSE(titleText);
   DISPOSE(titleText2);
   DISPOSE(headerText);
   DISPOSE(separatorText);
   DISPOSE(outputText);   
   DISPOSE(printWindow);   
   DISPOSE(printer);   
END PROCEDURE;

PROCEDURE GetFile (IN title : STRING);
BEGIN
   filter := "*.txt";
   DateTime(Date);
   REPEAT
      GetFileName(tempFile, tempPath, filter, title);
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
END PROCEDURE;

PROCEDURE PrintInputTable(IN printee : TableObj;  IN type,action : STRING);
VAR
  h,columns                     : INTEGER;
  colWidth                      : ARRAY INTEGER OF INTEGER;
  headerStr,titleStr,titleStr2,
  separatorStr                  : STRING;
BEGIN
  IF action="Print"
     Setup(2);
  END IF;   
  numPages := CEIL(FLOAT(printee.NumberRows)/35.);
  CASE type
     WHEN "Distro":
        headerStr:="Block Distribution Input Table";
        titleStr2:="Block Name             Fail Distro     Param1     Param2     Param3   Repair Distro     Param1     Param2     Param3";
        columns:=9;
        NEW(colWidth,1..columns);
        colWidth[1]:=20; colWidth[2]:=12; colWidth[3]:=11; colWidth[4]:=10; colWidth[5]:=11;
        colWidth[6]:=16; colWidth[7]:=11; colWidth[8]:=10; colWidth[9]:=11;
     WHEN "Sparing":
        headerStr:="Block Spares Input Table";
        titleStr:="                                                Initial  New        Arrival    Order  Order      Arrival   Emergency";
        titleStr2:="Block Name                 Source of Spares     Stock    Spares     Rate       Level  Amount     Rate      Time";
        columns:=9;
        NEW(colWidth,1..columns);
        colWidth[1]:=21; colWidth[2]:=20; colWidth[3]:=10; colWidth[4]:=9; colWidth[5]:=11;
        colWidth[6]:=10; colWidth[7]:=8; colWidth[8]:=11; colWidth[9]:=11;
     WHEN "PM":
        headerStr:="Block Preventive Maintenance Input Table";
        titleStr:= "                                                                       Uses              Mission  Logistics  Reset on";
        titleStr2:="Block Name                Frequency           Duration     Startup     Spare   Refresh     Defer      Defer   Failure";
        columns:=9;
        NEW(colWidth,1..columns);
        colWidth[1]:=21; colWidth[2]:=12; colWidth[3]:=20; colWidth[4]:=11; colWidth[5]:=9;
        colWidth[6]:=10; colWidth[7]:=10; colWidth[8]:=10; colWidth[9]:=10;
     WHEN "MaintDelays":
        headerStr:="Block Maintenance Delays Input Table";
        titleStr2:="Block Name                         Pre-LDT            Post-LDT              Resource     # for Mx    # for PM";
        columns:=6;
        NEW(colWidth,1..columns);
        colWidth[1]:=21; colWidth[2]:=20; colWidth[3]:=20; colWidth[4]:=20; colWidth[5]:=12;
        colWidth[6]:=12;
     WHEN "Dependency":
        headerStr:="Block Dependency Input Table";
        titleStr2:="Block Name                            Type        %Same       %Idle         %PM   If Exhaust      %Fail      Default";
        columns:=8;
        NEW(colWidth,1..columns);
        colWidth[1]:=21; colWidth[2]:=20; colWidth[3]:=12; colWidth[4]:=12; colWidth[5]:=12;
        colWidth[6]:=12; colWidth[7]:=12; colWidth[8]:=12;
     WHEN "Advanced":
        headerStr:="Block Advanced Input Table";
        titleStr2:="Block Name             F-Stream  R-Stream   Start       Exhaust      Phased     Growth     Rate      Limit   Standby";
        columns:=10;
        NEW(colWidth,1..columns);
        colWidth[1]:=21; colWidth[2]:=5; colWidth[3]:=10; colWidth[4]:=10; colWidth[5]:=15;
        colWidth[6]:=11; colWidth[7]:=11; colWidth[8]:=10; colWidth[9]:=11; colWidth[10]:=10;
     WHEN "NodeGeneral":
        headerStr:="Node General Input Table";
        titleStr:="                                                           Dependency   ";
        titleStr2:="Node Name             k-value  Links In   Links Out           Type        Node#     Phased         SBB         INA";
        columns:=9;
        NEW(colWidth,1..columns);
        colWidth[1]:=21; colWidth[2]:=5; colWidth[3]:=10; colWidth[4]:=10; colWidth[5]:=20;
        colWidth[6]:=10; colWidth[7]:=12; colWidth[8]:=12; colWidth[9]:=12;
     WHEN "NodeStandby":
        headerStr:="Node Standby Input Table";
        titleStr2:="Links From                          Links To   K-star   Priority    Probability    Auto    Manual       PRB     CAB";
        columns:=9;
        NEW(colWidth,1..columns);
        colWidth[1]:=21; colWidth[2]:=21; colWidth[3]:=7; colWidth[4]:=10; colWidth[5]:=17;
        colWidth[6]:=8; colWidth[7]:=10; colWidth[8]:=10; colWidth[9]:=8;
     WHEN "NodeCapacity":
        headerStr:="Node Capacity Input Table";
        titleStr:= "                                                                          Nominal            Max    ";
        titleStr2:="Links From                                     Links To                      Flow           Flow     Priority";
        columns:=5;
        NEW(colWidth,1..columns);
        colWidth[1]:=27; colWidth[2]:=27; colWidth[3]:=24; colWidth[4]:=15; colWidth[5]:=13;
     WHEN "Event":
        headerStr:="Event Input Table";
        titleStr :="                        Probability of                              Initial      Success      Failure   ";
        titleStr2:="Event Name                 Success      F-Stream   Phased             Cost         Cost         Cost";
        columns:=7;
        NEW(colWidth,1..columns);
        colWidth[1]:=25; colWidth[2]:=5; colWidth[3]:=10; colWidth[4]:=12; colWidth[5]:=17;
        colWidth[6]:=13; colWidth[7]:=13;
     WHEN "Hierarchy":
        headerStr:="Hierarchy Input Table";
        titleStr:= "                                            Dependency     ";
        titleStr2:="Hierarchy Name              Level              Type        Phased      Blocks     Nodes     Events   Hiers";
        columns:=8;
        NEW(colWidth,1..columns);
        colWidth[1]:=25; colWidth[2]:=5; colWidth[3]:=22; colWidth[4]:=10; colWidth[5]:=10;
        colWidth[6]:=10; colWidth[7]:=10; colWidth[8]:=10;
     WHEN "SparePools":
        headerStr:="Spare Pools Input Table";
        titleStr:= "                                                                                                Normal        Extra";
        titleStr2:="Pool Name             Stock      New   Arrive at    Order    # of  Arrive at   Emergency          Cost         Cost";
        columns:=10;
        NEW(colWidth,1..columns);
        colWidth[1]:=21; colWidth[2]:=5; colWidth[3]:=9; colWidth[4]:=11; colWidth[5]:=9;
        colWidth[6]:=8; colWidth[7]:=10; colWidth[8]:=12; colWidth[9]:=14; colWidth[10]:=14;
     WHEN "Resources":
        headerStr:="Resources Input Table";
        titleStr2:="Resource Name               Number     Initial Cost         Cost/T       Fixed Cost";
        columns:=5;
        NEW(colWidth,1..columns);
        colWidth[1]:=25; colWidth[2]:=5; colWidth[3]:=17; colWidth[4]:=17; colWidth[5]:=17;
     WHEN "Triggers":
        headerStr:="Triggers Input Table";
        titleStr2:="Trigger Name                              Frequency         Startup        Repetitive";
        columns:=4;
        NEW(colWidth,1..columns);
        colWidth[1]:=25; colWidth[2]:=25; colWidth[3]:=15; colWidth[4]:=17;
     OTHERWISE
  
  END CASE; 
  separatorStr:="____________________________________________________________________________________________________________________";
  
  IF action="Print"
     ASK headerText TO SetText(headerStr);
     ASK titleText TO SetText(titleStr);
     ASK titleText2 TO SetText(titleStr2);
     ASK separatorText TO SetText(separatorStr);
     FOR i := 1 TO numPages
        NEW(textBuffer, 1..35);
        FOR j := ((i-1)*35)+1 TO (i*35)
           m := j - ((i-1)*35);
           IF j <= printee.NumberRows
              textBuffer[m] := textBuffer[m] + printee.Text(1,j);
              FOR k:=STRLEN(printee.Text(1,j)) TO colWidth[1]
                 textBuffer[m] := textBuffer[m] + " ";
              END FOR;
              FOR h:=2 TO columns
                 FOR k := 1 TO ( colWidth[h]-(STRLEN(printee.Text(h,j))) )
                    textBuffer[m] := textBuffer[m] + " ";
                 END FOR;
                 textBuffer[m] := textBuffer[m] + printee.Text(h,j);
              END FOR;
           ELSE
              textBuffer[m] := "";
           END IF;
        END FOR;
        footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(i)+" of "+INTTOSTR(numPages);
        ASK footerText TO SetTextBuffer(footBuffer);
        ASK bodyText TO SetTextBuffer(textBuffer);
        ASK outputText TO Draw;
        ASK printer TO SetUseDialog(FALSE);
        ASK printer TO Print(printWindow); 
        DISPOSE(textBuffer);
     END FOR;
     Cleanup;
  ELSIF action="Save"
     GetFile(headerStr);
     IF tempFile <> "NoFile"
        NEW(exportFile);
        ASK exportFile TO Open((tempPath + tempFile), Output);
        IF exportFile.ioResult <> 0
           NEW(message, 1..2);
           message[1] := "There is a problem opening the selected save file.     ";
           message[2] := "Make sure this file has not been set to read only.     ";
           result := SendAlert(message, FALSE, FALSE, FALSE);
           DISPOSE(message);
        ELSE
           ASK exportFile TO WriteLn;
           ASK exportFile TO WriteString(headerStr);
           ASK exportFile TO WriteLn;
           ASK exportFile TO WriteLn;
           ASK exportFile TO WriteString(titleStr);
           ASK exportFile TO WriteLn;
           ASK exportFile TO WriteString(titleStr2);
           ASK exportFile TO WriteLn;
           ASK exportFile TO WriteLn;
           FOR j:=1 TO printee.NumberRows
              text := printee.Text(1,j);
              FOR k:=STRLEN(printee.Text(1,j)) TO colWidth[1]
                 text := text + " ";
              END FOR;
              FOR h:=2 TO columns
                 FOR k := 1 TO ( colWidth[h]-(STRLEN(printee.Text(h,j))) )
                    text := text + " ";
                 END FOR;
                 text := text + printee.Text(h,j);
              END FOR;
              ASK exportFile TO WriteString(text);
              ASK exportFile TO WriteLn;
           END FOR;
           ASK exportFile TO WriteLn;
           ASK exportFile TO WriteLn;
           ASK exportFile TO WriteLn;
           ASK exportFile TO WriteString(Date);
           ASK exportFile TO WriteLn;
           ASK exportFile TO WriteString(nameOfFile);
        END IF;
        ASK exportFile TO Close;      
        DISPOSE(exportFile);
     END IF;  {tempFile <> "NoFile"}
  END IF; {action="Print"}    
  DISPOSE(colWidth);
END PROCEDURE; {PrintInputTable}

PROCEDURE PrintSystemInput(IN printArray : strArray);
VAR
  separation,tabLength  : INTEGER;
  tab                   : STRING;
BEGIN
  Setup(1);
  ASK headerText TO SetText("System Settings Input Table");
  NEW(textBuffer, 1..35);
  separation:=50;
  tab:="    ";
  tabLength:=4;
  numPages := 1;
  i:=1;
        
  textBuffer[1]:=printArray[1];
  FOR j:=1 TO (separation - STRLEN(printArray[1]) )
     textBuffer[1]:=textBuffer[1] + " ";
  END FOR;
  textBuffer[1]:=textBuffer[1] + printArray[11];
  {skip a line}
  textBuffer[3]:=tab+printArray[2];
  FOR j:=1 TO (separation -tabLength - STRLEN(printArray[2]) )
     textBuffer[3]:=textBuffer[3] + " ";
  END FOR;
  textBuffer[3]:=textBuffer[3] + tab +printArray[12]; 
  textBuffer[4]:=tab + printArray[3];
  FOR j:=1 TO (separation -tabLength - STRLEN(printArray[3]) )
     textBuffer[4]:=textBuffer[4] + " ";
  END FOR;
  textBuffer[4]:=textBuffer[4] + tab + printArray[13];
  textBuffer[5]:=tab + printArray[4];
  FOR j:=1 TO (separation -tabLength - STRLEN(printArray[4]) )
     textBuffer[5]:=textBuffer[5] + " ";
  END FOR;
  {skip a line}
  textBuffer[8]:=printArray[5];
  FOR j:=1 TO (separation-STRLEN(printArray[5]) )
     textBuffer[8]:=textBuffer[8]+" ";
  END FOR;
  textBuffer[8]:=textBuffer[8] + printArray[14];
  {skip a line}
  textBuffer[10]:=tab + printArray[6];
  FOR j:=1 TO (separation -tabLength -STRLEN(printArray[6]) )
     textBuffer[10]:=textBuffer[10]+" ";
  END FOR;
  textBuffer[10]:=textBuffer[10] + tab + printArray[15];
  textBuffer[11]:=tab + printArray[7];
  FOR j:=1 TO (separation -tabLength -STRLEN(printArray[7])  )
     textBuffer[11]:=textBuffer[11]+" ";
  END FOR;
  textBuffer[11]:=textBuffer[11] + tab + printArray[16];
  textBuffer[12]:=tab + printArray[8];
  FOR j:=1 TO (separation -tabLength -STRLEN(printArray[8])  )
     textBuffer[12]:=textBuffer[12]+" ";
  END FOR;
  textBuffer[12]:=textBuffer[12] + tab + printArray[17];
  textBuffer[13]:=tab + printArray[9];
  FOR j:=1 TO (separation -tabLength -STRLEN(printArray[9])  )
     textBuffer[13]:=textBuffer[13]+" ";
  END FOR;
  textBuffer[13]:=textBuffer[13] + tab + printArray[18];
  textBuffer[14]:=tab + printArray[10];
  FOR j:=1 TO (separation -tabLength -STRLEN(printArray[10])  )
     textBuffer[14]:=textBuffer[14]+" ";
  END FOR;
  textBuffer[14]:=textBuffer[14] + tab + printArray[19];
         
  footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(i)+" of "+INTTOSTR(numPages);
  ASK footerText TO SetTextBuffer(footBuffer);
  ASK bodyText TO SetTextBuffer(textBuffer);
  ASK outputText TO Draw;
  ASK printer TO SetUseDialog(FALSE);
  ASK printer TO Print(printWindow); 
  DISPOSE(textBuffer);
  Cleanup;
END PROCEDURE;

PROCEDURE SaveSystemInput  (IN printArray : strArray);
VAR
  separation,tabLength  : INTEGER;
  tab,text              : STRING;
BEGIN
  separation:=50;
  tab:="    ";
  tabLength:=4;
  GetFile("System Settings Input Table");
  IF tempFile <> "NoFile"
     NEW(exportFile);
     ASK exportFile TO Open((tempPath + tempFile), Output);
     IF exportFile.ioResult <> 0
        NEW(message, 1..2);
        message[1] := "There is a problem opening the selected save file.     ";
        message[2] := "Make sure this file has not been set to read only.     ";
        result := SendAlert(message, FALSE, FALSE, FALSE);
        DISPOSE(message);
     ELSE
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString("System Settings Input Table");
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        
        text:=printArray[1];
        FOR j:=1 TO (separation - STRLEN(printArray[1]) )
           text:=text + " ";
        END FOR;
        text:=text + printArray[11];
        ASK exportFile TO WriteString(text);
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
 
        text:=tab+printArray[2];
        FOR j:=1 TO (separation -tabLength - STRLEN(printArray[2]) )
           text:=text + " ";
        END FOR;
        text:=text + tab + printArray[12];
        ASK exportFile TO WriteString(text);
        ASK exportFile TO WriteLn;
        
        text:=tab + printArray[3];
        FOR j:=1 TO (separation -tabLength - STRLEN(printArray[3]) )
           text:=text + " ";
        END FOR;
        text:=text + tab + printArray[13];
        ASK exportFile TO WriteString(text);
        ASK exportFile TO WriteLn;
        
        text:=tab + printArray[4];
        FOR j:=1 TO (separation -tabLength - STRLEN(printArray[4]) )
           text:=text + " ";
        END FOR;
        ASK exportFile TO WriteString(text);
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        
        text:=printArray[5];
        FOR j:=1 TO (separation-STRLEN(printArray[5]) )
           text:=text+" ";
        END FOR;
        text:=text + printArray[14];
        ASK exportFile TO WriteString(text);
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        
        text:=tab + printArray[6];
        FOR j:=1 TO (separation -tabLength -STRLEN(printArray[6]) )
           text:=text+" ";
        END FOR;
        text:=text + tab + printArray[15];
        ASK exportFile TO WriteString(text);
        ASK exportFile TO WriteLn;
        
        text:=tab + printArray[7];
        FOR j:=1 TO (separation -tabLength -STRLEN(printArray[7])  )
           text:=text+" ";
        END FOR;
        text:=text + tab + printArray[16];
        ASK exportFile TO WriteString(text);
        ASK exportFile TO WriteLn;
        
        text:=tab + printArray[8];
        FOR j:=1 TO (separation -tabLength -STRLEN(printArray[8])  )
           text:=text+" ";
        END FOR;
        text:=text + tab + printArray[17];
        ASK exportFile TO WriteString(text);
        ASK exportFile TO WriteLn;
        
        text:=tab + printArray[9];
        FOR j:=1 TO (separation -tabLength -STRLEN(printArray[9])  )
           text:=text+" ";
        END FOR;
        text:=text + tab + printArray[18];
        ASK exportFile TO WriteString(text);
        ASK exportFile TO WriteLn;

        text:=tab + printArray[10];
        FOR j:=1 TO (separation -tabLength -STRLEN(printArray[10])  )
           text:=text+" ";
        END FOR;
        text:=text + tab + printArray[19];
        ASK exportFile TO WriteString(text);
        ASK exportFile TO WriteLn;
           
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString(Date);
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString(nameOfFile);
        ASK exportFile TO Close;
     END IF;
  END IF;
END PROCEDURE;

PROCEDURE PrintSummaryOutput(IN printee : TableObj); 
VAR
   rtText   : TextObj; 
BEGIN
  Setup(3);
  NEW (rtText);
  SetOutputStrings;  
  ASK headerText TO SetText("Output Summary Table");
  ASK titleText TO SetText("                                               Results from " + runString + durationStr);
  ASK rtText TO SetColor(Black);  
  ASK rtText TO SetAlignment(HorizLeft,VertMiddle); 
  ASK rtText TO SetSysFont("Courier New", 9, 61,0);
  ASK rtText TO SetTranslation(2500.*scale,16000.*scale);
  ASK rtText TO SetFont(SystemText);
  ASK rtText TO SetText(FinalArray[0,1]);
  ASK titleText2 TO SetText("Parameter                            Minimum               Mean"+
            "            Maximum       Standard Dev                SEM");
  ASK separatorText TO SetText("_____________________________________________________________"+
                "____________________________________________________________");
  ASK outputText TO AddGraphic(rtText);
  NEW(textBuffer, 1..16);
  FOR j := 1 TO 16
     m := j;
     IF j <= printee.NumberRows
        textBuffer[m] := printee.Text(1,j);
        FOR k := (STRLEN(printee.Text(1,j))+STRLEN(printee.Text(2,j))) TO 35
           textBuffer[m] := textBuffer[m] + " ";
        END FOR;
        textBuffer[m] := textBuffer[m] +"       "+ printee.Text(2,j);
        FOR k := STRLEN(printee.Text(3,j)) TO 18
           textBuffer[m] := textBuffer[m] + " ";
        END FOR;
        textBuffer[m] := textBuffer[m] + printee.Text(3,j);
        FOR k := STRLEN(printee.Text(4,j)) TO 18
           textBuffer[m] := textBuffer[m] + " ";
        END FOR;
        textBuffer[m] := textBuffer[m] + printee.Text(4,j);
        FOR k := STRLEN(printee.Text(5,j)) TO 18
           textBuffer[m] := textBuffer[m] + " ";
        END FOR;
        textBuffer[m] := textBuffer[m] + printee.Text(5,j);
        FOR k := STRLEN(printee.Text(6,j)) TO 18
           textBuffer[m] := textBuffer[m] + " ";
        END FOR;
        textBuffer[m] := textBuffer[m] + printee.Text(6,j);
     ELSE
        textBuffer[m] := "";
     END IF;
  END FOR;
  footBuffer[2] := Date+"                                                                                                       Page 1 of 1";
  ASK footerText TO SetTextBuffer(footBuffer);
  ASK bodyText TO SetTranslation(2500.*scale,22200.*scale);
  ASK bodyText TO SetTextBuffer(textBuffer);
  ASK outputText TO Draw;
  ASK printer TO SetUseDialog(FALSE);
  ASK printer TO Print(printWindow); 
  Cleanup;
END PROCEDURE; {PrintSummaryOutput}

PROCEDURE SaveSummaryOutput(IN printee : TableObj);
BEGIN
   GetFile("Summary Output Table");
   IF tempFile <> "NoFile"
      NEW(exportFile);
      ASK exportFile TO Open((tempPath + tempFile), Output);
      IF exportFile.ioResult <> 0
         NEW(message, 1..2);
         message[1] := "There is a problem opening the selected save file.     ";
         message[2] := "Make sure this file has not been set to read only.     ";
         result := SendAlert(message, FALSE, FALSE, FALSE);
         DISPOSE(message);
      ELSE
         SetOutputStrings;
         ASK exportFile TO WriteString("Output Summary Table");
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString("                                                 Results from " + runString + durationStr);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString("Parameter                         Minimum               Mean"+
             "            Maximum       Standard Dev                SEM");
         ASK exportFile TO WriteLn;
         FOR j := 1 TO 16
            IF j <= printee.NumberRows
               text := printee.Text(1,j);
               FOR k := (STRLEN(printee.Text(1,j))+STRLEN(printee.Text(2,j))) TO 35
                  text := text + " ";
               END FOR;
               text := text +"     "+ printee.Text(2,j);
               FOR k := STRLEN(printee.Text(3,j)) TO 18
                  text := text + " ";
               END FOR;
               text := text + printee.Text(3,j);
               FOR k := STRLEN(printee.Text(4,j)) TO 18
                  text := text + " ";
               END FOR;
               text := text + printee.Text(4,j);
               FOR k := STRLEN(printee.Text(5,j)) TO 18
                  text := text + " ";
               END FOR;
               text := text + printee.Text(5,j);
               FOR k := STRLEN(printee.Text(6,j)) TO 18
                  text := text + " ";
               END FOR;
               text := text + printee.Text(6,j);
               ASK exportFile TO WriteString(text);
               ASK exportFile TO WriteLn;
            END IF;      
         END FOR;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(FinalArray[0,1]);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(nameOfFile);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(Date);
      END IF;
      ASK exportFile TO Close;      
      DISPOSE(exportFile);
   END IF;
END PROCEDURE {SaveSummaryOutput};

PROCEDURE PrintLogisticsOutput (IN printee : TableObj);
VAR
   rtText   : TextObj; 
BEGIN
   Setup(3);
   NEW (rtText);
   SetOutputStrings;
   ASK headerText TO SetText("Logistics Output Table");
   ASK titleText TO SetText("                                               Results from " + runString + durationStr);
   ASK rtText TO SetColor(Black);  
   ASK rtText TO SetAlignment(HorizLeft,VertMiddle); 
   ASK rtText TO SetSysFont("Courier New", 9, 61,0);
   ASK rtText TO SetTranslation(2500.*scale,16000.*scale);
   ASK rtText TO SetFont(SystemText);
   ASK rtText TO SetText(FinalArray[0,1]);
   ASK titleText2 TO SetText("Parameter                            Minimum               Mean"+
             "            Maximum       Standard Dev                SEM");
   ASK separatorText TO SetText("_____________________________________________________________"+
                 "____________________________________________________________");
   ASK outputText TO AddGraphic(rtText);
   NEW(textBuffer, 1..16);
   FOR j := 1 TO 16
      m := j;
      IF j <= printee.NumberRows
         textBuffer[m] := printee.Text(1,j);
         FOR k := (STRLEN(printee.Text(1,j))+STRLEN(printee.Text(2,j))) TO 35
            textBuffer[m] := textBuffer[m] + " ";
         END FOR;
         textBuffer[m] := textBuffer[m] +"       "+ printee.Text(2,j);
         FOR k := STRLEN(printee.Text(3,j)) TO 18
            textBuffer[m] := textBuffer[m] + " ";
         END FOR;
         textBuffer[m] := textBuffer[m] + printee.Text(3,j);
         FOR k := STRLEN(printee.Text(4,j)) TO 18
            textBuffer[m] := textBuffer[m] + " ";
         END FOR;
         textBuffer[m] := textBuffer[m] + printee.Text(4,j);
         FOR k := STRLEN(printee.Text(5,j)) TO 17
            textBuffer[m] := textBuffer[m] + " ";
         END FOR;
         textBuffer[m] := textBuffer[m] + printee.Text(5,j);
         FOR k := STRLEN(printee.Text(6,j)) TO 18
            textBuffer[m] := textBuffer[m] + " ";
         END FOR;
         textBuffer[m] := textBuffer[m] + printee.Text(6,j);
      ELSE
         textBuffer[m] := "";
      END IF;
   END FOR;
   footBuffer[2] := Date+"                                                                                                       Page 1 of 1";
   ASK footerText TO SetTextBuffer(footBuffer);
   ASK bodyText TO SetTranslation(2500.*scale,22200.*scale);
   ASK bodyText TO SetTextBuffer(textBuffer);
   ASK outputText TO Draw;
   ASK printer TO SetUseDialog(FALSE);
   ASK printer TO Print(printWindow); 
   Cleanup;
END PROCEDURE;

PROCEDURE SaveLogisticsOutput (IN printee : TableObj);
BEGIN
  GetFile("Logistics Output Table");
  IF tempFile <> "NoFile"
     NEW(exportFile);
     ASK exportFile TO Open((tempPath + tempFile), Output);
     IF exportFile.ioResult <> 0
        NEW(message, 1..2);
        message[1] := "There is a problem opening the selected save file.     ";
        message[2] := "Make sure this file has not been set to read only.     ";
        result := SendAlert(message, FALSE, FALSE, FALSE);
        DISPOSE(message);
     ELSE
        SetOutputStrings;
        ASK exportFile TO WriteString("Logistics Output Table");
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString("                                                 Results from " + runString + durationStr);
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString("Parameter                         Minimum               Mean"+
            "            Maximum       Standard Dev                SEM");
        ASK exportFile TO WriteLn;
        FOR j := 1 TO 16
           IF j <= printee.NumberRows
              text := printee.Text(1,j);
              FOR k := (STRLEN(printee.Text(1,j))+STRLEN(printee.Text(2,j))) TO 35
                 text := text + " ";
              END FOR;
              text := text +"     "+ printee.Text(2,j);
              FOR k := STRLEN(printee.Text(3,j)) TO 18
                 text := text + " ";
              END FOR;
              text := text + printee.Text(3,j);
              FOR k := STRLEN(printee.Text(4,j)) TO 18
                 text := text + " ";
              END FOR;
              text := text + printee.Text(4,j);
              FOR k := STRLEN(printee.Text(5,j)) TO 18
                 text := text + " ";
              END FOR;
              text := text + printee.Text(5,j);
              FOR k := STRLEN(printee.Text(6,j)) TO 18
                 text := text + " ";
              END FOR;
              text := text + printee.Text(6,j);
              ASK exportFile TO WriteString(text);
              ASK exportFile TO WriteLn;
           END IF;      
        END FOR;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString(FinalArray[0,1]);
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString(nameOfFile);
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString(Date);
     END IF;
     ASK exportFile TO Close;      
     DISPOSE(exportFile);
  END IF;
END PROCEDURE;

PROCEDURE PrintSparingOutput(IN printee : TableObj);     
BEGIN
   Setup(3);
   numPages := CEIL(FLOAT(printee.NumberRows)/35.);
   SetOutputStrings;
   ASK headerText TO SetText("Sparing Output Table");
   ASK titleText TO SetText("                                          Average Sparing Data from " + runString + durationStr);
   ASK titleText2 TO SetText("Component               Source of Spares     Min Used      Average Used      Max Used      Standard Dev");
   ASK separatorText TO SetText("_______________________________________________________________________________________________________");
   FOR i := 1 TO numPages
      NEW(textBuffer, 1..35);
      FOR j := ((i-1)*35)+1 TO (i*35)
         m := j - ((i-1)*35);
         IF j <= printee.NumberRows
            textBuffer[m] := printee.Text(1,j);
            FOR k := STRLEN(printee.Text(1,j)) TO 22
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(2,j);
            FOR k := (STRLEN(printee.Text(2,j))+STRLEN(printee.Text(3,j))) TO 28
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(3,j);
            FOR k := STRLEN(printee.Text(4,j)) TO 17
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(4,j);
            FOR k := STRLEN(printee.Text(5,j)) TO 12
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(5,j);
            FOR k := STRLEN(printee.Text(6,j)) TO 17
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(6,j);
         ELSE
            textBuffer[m] := "";
         END IF;
      END FOR;
      footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(i)+" of "+INTTOSTR(numPages);
      ASK footerText TO SetTextBuffer(footBuffer);
      ASK bodyText TO SetTextBuffer(textBuffer);
      ASK outputText TO Draw;
      ASK printer TO SetUseDialog(FALSE);
      ASK printer TO Print(printWindow); 
      DISPOSE(textBuffer);
   END FOR;
   Cleanup;
END PROCEDURE; {PrintSparingOutput}

PROCEDURE SaveSparingOutput(IN printee : TableObj);
BEGIN
   GetFile("Sparing Output Table");
   IF tempFile <> "NoFile"
      NEW(exportFile);
      ASK exportFile TO Open((tempPath + tempFile), Output);
      IF exportFile.ioResult <> 0
         NEW(message, 1..2);
         message[1] := "There is a problem opening the selected save file.     ";
         message[2] := "Make sure this file has not been set to read only.     ";
         result := SendAlert(message, FALSE, FALSE, FALSE);
         DISPOSE(message);
      ELSE
         SetOutputStrings;
         ASK exportFile TO WriteString("Sparing Output Table");
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString("                                          Average sparing data over " + runString + durationStr);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString("Component               Source_of_Spares     Min_Used      Average_Used      Max_Used      Standard_Dev");
         ASK exportFile TO WriteLn;
         FOR j := 1 TO printee.NumberRows
            text := printee.Text(1,j);
            FOR k := STRLEN(printee.Text(1,j)) TO 22
               text := text + " ";
            END FOR;
            text := text + printee.Text(2,j);
            FOR k := (STRLEN(printee.Text(2,j))+STRLEN(printee.Text(3,j))) TO 28
               text := text + " ";
            END FOR;
            text := text + printee.Text(3,j);
            FOR k := STRLEN(printee.Text(4,j)) TO 17
               text := text + " ";
            END FOR;
            text := text + printee.Text(4,j);
            FOR k := STRLEN(printee.Text(5,j)) TO 12
               text := text + " ";
            END FOR;
            text := text + printee.Text(5,j);
            FOR k := STRLEN(printee.Text(6,j)) TO 17
               text := text + " ";
            END FOR;
            text := text + printee.Text(6,j);
            ASK exportFile TO WriteString(text);
            ASK exportFile TO WriteLn;
         END FOR;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(nameOfFile);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(Date);
      END IF;
      ASK exportFile TO Close;      
      DISPOSE(exportFile);
   END IF;
END PROCEDURE {SaveSparingOutput};

PROCEDURE PrintCapacityOutput(IN printee : TableObj);     
BEGIN
   Setup(3);
   numPages := CEIL(FLOAT(printee.NumberRows)/35.);
   SetOutputStrings;
   ASK headerText TO SetText("Capacity Output Table");
   ASK titleText TO SetText("                                          Average capacity flow across " + runString + durationStr);
   ASK titleText2 TO SetText("Node Name                 Min Flow     Avg Flow     Max Flow     Usage       Min Cap      Avg Cap      Max Cap  Capablty");
   ASK separatorText TO SetText("_______________________________________________________________________________________________________________________");
   FOR i := 1 TO numPages
      NEW(textBuffer, 1..35);
      FOR j := ((i-1)*35)+1 TO (i*35)
         m := j - ((i-1)*35);
         IF j <= printee.NumberRows
            textBuffer[m] := printee.Text(1,j);
            FOR k := (STRLEN(printee.Text(1,j))+STRLEN(printee.Text(2,j))) TO 32
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(2,j);
            FOR k := STRLEN(printee.Text(3,j)) TO 12
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(3,j);
            FOR k := STRLEN(printee.Text(4,j)) TO 12
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(4,j);
            FOR k := STRLEN(printee.Text(5,j)) TO 9
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(5,j);
            FOR k := STRLEN(printee.Text(6,j)) TO 12
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(6,j);
            FOR k := STRLEN(printee.Text(7,j)) TO 12
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(7,j);
            FOR k := STRLEN(printee.Text(8,j)) TO 12
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(8,j);
            FOR k := STRLEN(printee.Text(9,j)) TO 8
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(9,j);
         ELSE
            textBuffer[m] := "";
         END IF;
      END FOR;
      footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(i)+" of "+INTTOSTR(numPages);
      ASK footerText TO SetTextBuffer(footBuffer);
      ASK bodyText TO SetTextBuffer(textBuffer);
      ASK outputText TO Draw;
      ASK printer TO SetUseDialog(FALSE);
      ASK printer TO Print(printWindow); 
      DISPOSE(textBuffer);
   END FOR;
   Cleanup;
END PROCEDURE; {PrintCapacityOutput}

PROCEDURE SaveCapacityOutput(IN printee : TableObj);
BEGIN
   GetFile("Capacity Output Table");
   IF tempFile <> "NoFile"
      NEW(exportFile);
      ASK exportFile TO Open((tempPath + tempFile), Output);
      IF exportFile.ioResult <> 0
         NEW(message, 1..2);
         message[1] := "There is a problem opening the selected save file.     ";
         message[2] := "Make sure this file has not been set to read only.     ";
         result := SendAlert(message, FALSE, FALSE, FALSE);
         DISPOSE(message);
      ELSE
         SetOutputStrings;
         ASK exportFile TO WriteString("Capacity Output Table");
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString("                                          Average capacity flow across " + runString + durationStr);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString("Node_Name                 Min_Flow     Avg_Flow     Max_Flow    Usage       Min_Cap      Avg_Cap      Max_Cap  Capablty");
         ASK exportFile TO WriteLn;
         FOR j := 1 TO printee.NumberRows
            text := printee.Text(1,j);
            FOR k := (STRLEN(printee.Text(1,j))+STRLEN(printee.Text(2,j))) TO 32
               text := text + " ";
            END FOR;
            text := text + printee.Text(2,j);
            FOR k := STRLEN(printee.Text(3,j)) TO 12
               text := text + " ";
            END FOR;
            text := text + printee.Text(3,j);
            FOR k := STRLEN(printee.Text(4,j)) TO 12
               text := text + " ";
            END FOR;
            text := text + printee.Text(4,j);
            FOR k := STRLEN(printee.Text(5,j)) TO 8
               text := text + " ";
            END FOR;
            text := text + printee.Text(5,j);
            FOR k := STRLEN(printee.Text(6,j)) TO 12
               text := text + " ";
            END FOR;
            text := text + printee.Text(6,j);
            FOR k := STRLEN(printee.Text(7,j)) TO 12
               text := text + " ";
            END FOR;
            text := text + printee.Text(7,j);
            FOR k := STRLEN(printee.Text(8,j)) TO 12
               text := text + " ";
            END FOR;
            text := text + printee.Text(8,j);
            FOR k := STRLEN(printee.Text(9,j)) TO 8
               text := text + " ";
            END FOR;
            text := text + printee.Text(9,j);
            ASK exportFile TO WriteString(text);
            ASK exportFile TO WriteLn;
         END FOR;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(nameOfFile);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(Date);
      END IF;
      ASK exportFile TO Close;      
      DISPOSE(exportFile);
   END IF;
END PROCEDURE {SaveCapacityOutput};

PROCEDURE PrintAnalOutput(IN printee : TableObj; IN Input : STRING);    
VAR
   name,type : STRING;
BEGIN
   Setup(3);
   SetOutputStrings;
   IF weakLinkAnalType=1
      type := "Availability";
   ELSIF weakLinkAnalType=2
      type:= "Dependability";
   ELSIF weakLinkAnalType=3
      type:="Reliability";
   END IF;  
   IF Input="Node"        {to get the correct spacing in the table}
      name:="Node Name ";
   ELSIF Input="Block"
      name:="Block Name";
   ELSIF Input="Event"
      name:="Event Name";
   ELSIF Input="Hier"
      name:="Hier Name ";
   END IF;   
   numPages := CEIL(FLOAT(printee.NumberRows)/35.);
   ASK headerText TO SetText(Input+ " Analysis Output Table");
   ASK titleText TO SetText("                                         "+type+" data across " + runString + durationStr);
   ASK titleText2 TO SetText(name+"                         Color           Minimum         Mean      "
               +"Maximum       Std Dev          SEM");
   ASK separatorText TO SetText("______________________________________________________"+
              "___________________________________________________________");
   FOR i := 1 TO numPages
      NEW(textBuffer, 1..35);
      FOR j := ((i-1)*35)+1 TO (i*35)
         m := j - ((i-1)*35);     {line number}
         IF j <= printee.NumberRows
            textBuffer[m] := printee.Text(1,j);
            FOR k := (STRLEN(printee.Text(1,j))+STRLEN(printee.Text(2,j))) TO 36
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] +"  "+ printee.Text(2,j);
            FOR k := STRLEN(printee.Text(3,j)) TO 15
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m]+"  " + printee.Text(3,j);
            FOR k := STRLEN(printee.Text(4,j)) TO 9
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m]+"  " + printee.Text(4,j);
            FOR k := STRLEN(printee.Text(5,j)) TO 9
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m]+"  " + printee.Text(5,j);
            FOR k := STRLEN(printee.Text(6,j)) TO 9
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m]+"  " + printee.Text(6,j);
            FOR k := STRLEN(printee.Text(7,j)) TO 9
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m]+"  " + printee.Text(7,j);
         ELSE
            textBuffer[m] := "";
         END IF;
      END FOR;
      footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(i)+" of "+INTTOSTR(numPages);
      ASK footerText TO SetTextBuffer(footBuffer);
      ASK bodyText TO SetTextBuffer(textBuffer);
      ASK outputText TO Draw;
      ASK printer TO SetUseDialog(FALSE);
      ASK printer TO Print(printWindow); 
      DISPOSE(textBuffer);
   END FOR;
   Cleanup;
END PROCEDURE; {PrintAnalOutput}

PROCEDURE SaveAnalOutput(IN printee : TableObj; IN Input : STRING);
VAR
   name,type : STRING;
BEGIN
   IF weakLinkAnalType=1
      type := "Availability";
   ELSIF weakLinkAnalType=2
      type:= "Dependability";
   ELSIF weakLinkAnalType=3
      type:="Reliability";
   END IF;  
   IF Input="Node"        {to get the correct spacing in the table}
      name:="Node Name "
   ELSIF Input="Block"
      name:="Block Name"
   ELSIF Input="Hier"
      name:="Hier Name ";
   END IF;   
   GetFile(Input+ " Analysis Output Table");
   IF tempFile <> "NoFile"
      NEW(exportFile);
      ASK exportFile TO Open((tempPath + tempFile), Output);
      IF exportFile.ioResult <> 0
         NEW(message, 1..2);
         message[1] := "There is a problem opening the selected save file.     ";
         message[2] := "Make sure this file has not been set to read only.     ";
         result := SendAlert(message, FALSE, FALSE, FALSE);
         DISPOSE(message);
      ELSE
         SetOutputStrings;
         ASK exportFile TO WriteString(Input+ " Analysis Output Table");
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString("                                         "+type+" data across " + runString + durationStr);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(name+"                         Color          Minimum         Mean      "
               +"Maximum      Std Dev          SEM");
         ASK exportFile TO WriteLn;
         FOR j := 1 TO printee.NumberRows
            text := printee.Text(1,j);
            FOR k := (STRLEN(printee.Text(1,j))+STRLEN(printee.Text(2,j))) TO 36
               text := text + " ";
            END FOR;
            text := text +"  "+ printee.Text(2,j);
            FOR k := STRLEN(printee.Text(3,j)) TO 15
               text := text + " ";
            END FOR;
            text := text +"  "+ printee.Text(3,j);
            FOR k := STRLEN(printee.Text(4,j)) TO 9
               text := text + " ";
            END FOR;
            text := text +"  "+ printee.Text(4,j);
            FOR k := STRLEN(printee.Text(5,j)) TO 9
               text := text + " ";
            END FOR;
            text := text +"  "+ printee.Text(5,j);
            FOR k := STRLEN(printee.Text(6,j)) TO 9
               text := text + " ";
            END FOR;
            text := text +"  "+ printee.Text(6,j);
            FOR k := STRLEN(printee.Text(7,j)) TO 9
               text := text + " ";
            END FOR;
            text := text +"  "+ printee.Text(7,j);
            ASK exportFile TO WriteString(text);
            ASK exportFile TO WriteLn;
         END FOR;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(nameOfFile);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(Date);
      END IF;
      ASK exportFile TO Close;      
      DISPOSE(exportFile);
   END IF;
END PROCEDURE {SaveAnalOutput};

PROCEDURE PrintCostOutput(IN printee : TableObj);     
BEGIN
   Setup(3);
   numPages := CEIL(FLOAT(printee.NumberRows)/35.);
   SetOutputStrings;
   ASK headerText TO SetText("Block Costs Output Table");
   ASK titleText TO SetText("                                          Average cost data over " + runString + durationStr);
   ASK titleText2 TO SetText("Block Name                 Initial Costs    Operating Costs      Sparing Costs      Disposal Costs         Total Costs");
   ASK separatorText TO SetText("_____________________________________________________________________________________________________________________");
   FOR i := 1 TO numPages
      NEW(textBuffer, 1..35);
      FOR j := ((i-1)*35)+1 TO (i*35)
         m := j - ((i-1)*35);
         IF j <= printee.NumberRows
            textBuffer[m] := printee.Text(1,j);
            FOR k := (STRLEN(printee.Text(1,j))+STRLEN(printee.Text(2,j))) TO 38
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(2,j);
            FOR k := (+STRLEN(printee.Text(3,j))) TO 18
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(3,j);
            FOR k := STRLEN(printee.Text(4,j)) TO 18
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(4,j);
            FOR k := STRLEN(printee.Text(5,j)) TO 18
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(5,j);
            FOR k := STRLEN(printee.Text(6,j)) TO 18
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] + printee.Text(6,j);
         ELSE
            textBuffer[m] := "";
         END IF;
      END FOR;
      footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(i)+" of "+INTTOSTR(numPages);
      ASK footerText TO SetTextBuffer(footBuffer);
      ASK bodyText TO SetTextBuffer(textBuffer);
      ASK outputText TO Draw;
      ASK printer TO SetUseDialog(FALSE);
      ASK printer TO Print(printWindow); 
      DISPOSE(textBuffer);
   END FOR;
   Cleanup;
END PROCEDURE; {PrintCostOutput}

PROCEDURE SaveCostOutput(IN printee : TableObj);
BEGIN
   GetFile("Block Cost Output Table");
   IF tempFile <> "NoFile"
      NEW(exportFile);
      ASK exportFile TO Open((tempPath + tempFile), Output);
      IF exportFile.ioResult <> 0
         NEW(message, 1..2);
         message[1] := "There is a problem opening the selected save file.     ";
         message[2] := "Make sure this file has not been set to read only.     ";
         result := SendAlert(message, FALSE, FALSE, FALSE);
         DISPOSE(message);
      ELSE
         SetOutputStrings;
         ASK exportFile TO WriteString("Block Costs Output Table");
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString("                                          Average cost data over " + runString + durationStr);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString("Block_Name                 Initial_Costs    Operating_Costs      Sparing_Costs      Disposal_Costs         Total_Costs");
         ASK exportFile TO WriteLn;
         FOR j := 1 TO printee.NumberRows
            text := printee.Text(1,j);
            FOR k := (STRLEN(printee.Text(1,j))+STRLEN(printee.Text(2,j))) TO 38
               text := text + " ";
            END FOR;
            text := text + printee.Text(2,j);
            FOR k := (+STRLEN(printee.Text(3,j))) TO 18
               text := text + " ";
            END FOR;
            text := text + printee.Text(3,j);
            FOR k := STRLEN(printee.Text(4,j)) TO 18
               text := text + " ";
            END FOR;
            text := text + printee.Text(4,j);
            FOR k := STRLEN(printee.Text(5,j)) TO 18
               text := text + " ";
            END FOR;
            text := text + printee.Text(5,j);
            FOR k := STRLEN(printee.Text(6,j)) TO 18
               text := text + " ";
            END FOR;
            text := text + printee.Text(6,j);
            ASK exportFile TO WriteString(text);
            ASK exportFile TO WriteLn;
         END FOR;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(nameOfFile);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(Date);
      END IF;
      ASK exportFile TO Close;      
      DISPOSE(exportFile);
   END IF;
END PROCEDURE {SaveCostOutput};

PROCEDURE PrintCostInput(IN printee : TableObj);     
BEGIN
  Setup(2);
  numPages := CEIL(FLOAT(printee.NumberRows)/35.);
  ASK headerText TO SetText("Block Costs Input Table");
  ASK separatorText TO SetText("_______________________________________________________________________________________________________________________");
  FOR i := 1 TO numPages
     FOR n := 0 TO 2
        column := n*6;
        IF n = 0
           ASK titleText2 TO SetText("Block Name                   Running           Standby        Repair         Fixed Repair         PM          Fixed PM");
        ELSIF n = 1
           ASK titleText2 TO SetText("Block Name                 Logistic Hold       PM Hold          Idle             Done         Initial     Spare");
        ELSE
           ASK titleText2 TO SetText("Block Name                Emergency          Disposal        Assess Done");
        END IF;
        NEW(textBuffer, 1..35);
        FOR j := ((i-1)*35)+1 TO (i*35)
           m := j - ((i-1)*35);
           IF j <= printee.NumberRows
              textBuffer[m] := printee.Text(0,j);
              FOR k := (STRLEN(printee.Text(0,j))+STRLEN(printee.Text(1+column,j))) TO 35
                 textBuffer[m] := textBuffer[m] + " ";
              END FOR;
              textBuffer[m] := textBuffer[m] + printee.Text(1+column,j);
              FOR k := STRLEN(printee.Text(2+column,j)) TO 15
                 textBuffer[m] := textBuffer[m] + " ";
              END FOR;
              textBuffer[m] := textBuffer[m] + printee.Text(2+column,j);
              FOR k := STRLEN(printee.Text(3+column,j)) TO 15
                 textBuffer[m] := textBuffer[m] + " ";
              END FOR;
              textBuffer[m] := textBuffer[m] + printee.Text(3+column,j);
              IF n<2
                 FOR k := STRLEN(printee.Text(4+column,j)) TO 15
                    textBuffer[m] := textBuffer[m] + " ";
                 END FOR;
                 textBuffer[m] := textBuffer[m] + printee.Text(4+column,j);
                 FOR k := STRLEN(printee.Text(5+column,j)) TO 15
                    textBuffer[m] := textBuffer[m] + " ";
                 END FOR;
                 textBuffer[m] := textBuffer[m] + printee.Text(5+column,j);
                 FOR k := STRLEN(printee.Text(6+column,j)) TO (11+(4-(n*4)))
                    textBuffer[m] := textBuffer[m] + " ";
                 END FOR;
                 textBuffer[m] := textBuffer[m] + printee.Text(6+column,j);
              END IF;
            ELSE
               textBuffer[m] := "";
            END IF;
        END FOR;
        IF i = 1
           footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(i+n)+" of "+INTTOSTR(numPages*3);
        ELSE
           footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(i+i-1+n)+" of "+INTTOSTR(numPages*3);
        END IF;
        ASK footerText TO SetTextBuffer(footBuffer);
        ASK bodyText TO SetTextBuffer(textBuffer);
        ASK outputText TO Draw;
        ASK printer TO SetUseDialog(FALSE);
        ASK printer TO Print(printWindow); 
        DISPOSE(textBuffer);
     END FOR;
  END FOR;
  Cleanup;
END PROCEDURE; {PrintCostInput}

PROCEDURE SaveCostInput(IN printee : TableObj);
BEGIN
  GetFile("Block Costs Input Table");
  IF tempFile <> "NoFile"
     NEW(exportFile);
     ASK exportFile TO Open((tempPath + tempFile), Output);
     IF exportFile.ioResult <> 0
        NEW(message, 1..2);
        message[1] := "There is a problem opening the selected save file.     ";
        message[2] := "Make sure this file has not been set to read only.     ";
        result := SendAlert(message, FALSE, FALSE, FALSE);
        DISPOSE(message);
     ELSE
        ASK exportFile TO WriteString("Block Costs Input Table");
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString("Block_Name                   Running           Standby        Repair       Fixed_Repair         PM          Fixed_PM"+
                                      "     Logistic_Hold       PM_Hold          Idle             Done         Initial         Spare   "+
                                      "     Emergency          Disposal        Assess_Done");
        ASK exportFile TO WriteLn;
        FOR j := 1 TO printee.NumberRows
           text := printee.Text(0,j);
           FOR k := (STRLEN(printee.Text(0,j))+STRLEN(printee.Text(1,j))) TO 35
              text := text + " ";
           END FOR;
           text := text + printee.Text(1,j);
           FOR k := STRLEN(printee.Text(2,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(2,j);
           FOR k := STRLEN(printee.Text(3,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(3,j);
           FOR k := STRLEN(printee.Text(4,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(4,j);
           FOR k := STRLEN(printee.Text(5,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(5,j);
           FOR k := STRLEN(printee.Text(6,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(6,j);
           FOR k := STRLEN(printee.Text(7,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(7,j);
           FOR k := STRLEN(printee.Text(8,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(8,j);
           FOR k := STRLEN(printee.Text(9,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(9,j);
           FOR k := STRLEN(printee.Text(10,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(10,j);
           FOR k := STRLEN(printee.Text(11,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(11,j);
           FOR k := STRLEN(printee.Text(12,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(12,j);
           FOR k := STRLEN(printee.Text(13,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(13,j);
           FOR k := STRLEN(printee.Text(14,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(14,j);
           FOR k := STRLEN(printee.Text(15,j)) TO 15
              text := text + " ";
           END FOR;
           text := text + printee.Text(15,j);
           ASK exportFile TO WriteString(text);
           ASK exportFile TO WriteLn;
        END FOR;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString(nameOfFile);
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString(Date);
     END IF;
     ASK exportFile TO Close;      
     DISPOSE(exportFile);
  END IF;
END PROCEDURE {SaveCostInput};

PROCEDURE PrintEventPhases(IN printee : TableObj);     
VAR
   o,phaseGroups,page,left,passes,
   phaseRemainder                     : INTEGER;
   sepBuffer, titleBuff3,tempString   : STRING;
   titleText3                         : TextObj;
   rowsFinished                       : BOOLEAN;
BEGIN
   Setup(2);
   NEW(titleText3);
   ASK titleText3 TO SetColor(Black);                
   ASK titleText3 TO SetAlignment(HorizLeft,VertMiddle); 
   ASK titleText3 TO SetSysFont("Courier New", 9, 61,0);
   ASK titleText3 TO SetTranslation(9200.*scale,28000.*scale);
   ASK titleText3 TO SetFont(SystemText);
   ASK titleText TO SetTranslation(9200.*scale,28600.*scale);
   ASK outputText TO AddGraphic(titleText3);

   phaseGroups:=((activePhases+1) DIV 8)+1;     {add 1 to have a column for "base p"}
   phaseRemainder:=(activePhases+1) MOD 8;
   IF phaseRemainder=0
      phaseGroups:=phaseGroups-1;
      phaseRemainder:=8;      
   END IF;      
   numPages := (CEIL(FLOAT(printee.NumberRows)/35.))*phaseGroups;
   {tony numPages fix}
   ASK headerText TO SetText("Event Phasing Table");
   ASK titleText2 TO SetText("Event Name          ");
   page:=1;   {page number}
      FOR k:=1 TO phaseGroups
         IF k<phaseGroups
            left:=8;
         ELSE
            left:=phaseRemainder;
         END IF;   
         titleBuff3 := "";
         sepBuffer := "_____________________";
         FOR j := (k-1)*8+1 TO (k-1)*8+left     {fills column header}
            sepBuffer := sepBuffer + "__";
            titleBuff3 := titleBuff3 + "  ";
            IF j=1
               titleBuff3:= "   Base p ";
            ELSIF j <11   
               titleBuff3 := titleBuff3 + " P00"+INTTOSTR(j-1)+"   ";
            ELSIF j<101
               titleBuff3 := titleBuff3 + " P0"+INTTOSTR(j-1)+"   ";
            ELSE
               titleBuff3 := titleBuff3 + " P"+INTTOSTR(j-1)+"   ";
            END IF;   
            sepBuffer := sepBuffer + "_________";
         END FOR;
         rowsFinished:=FALSE;
         passes:=0;
         REPEAT
            NEW(textBuffer, 1..35);
            FOR i:=1 TO 35           {fills row input}
               m:=i+passes*35;       {row that needs printing}
               textBuffer[i] := printee.Text(0,m);
               FOR n := STRLEN(printee.Text(0,m)) TO 20
                  textBuffer[i] := textBuffer[i] + " ";
               END FOR;
               FOR j := (k-1)*8+1 TO (k-1)*8+left      
                  textBuffer[i] := textBuffer[i] + "  ";
                  tempString:=printee.Text(j,m)+"           ";
                  textBuffer[i] := textBuffer[i] + SUBSTR(1,8,tempString);;
               END FOR;
               IF (m=printee.NumberRows)
                  rowsFinished:=TRUE;
                  EXIT;
               END IF;   
            END FOR;
            INC(passes);
         
            footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(page)+" of "+INTTOSTR(numPages);
            ASK footerText TO SetTextBuffer(footBuffer);
            ASK bodyText TO SetTextBuffer(textBuffer);
            ASK separatorText TO SetText(sepBuffer);
            ASK titleText3 TO SetText(titleBuff3);
            ASK outputText TO Draw;
            ASK printer TO SetUseDialog(FALSE);
            ASK printer TO Print(printWindow); 
            DISPOSE(textBuffer);
            INC(page);
         UNTIL rowsFinished;
      END FOR;   {k:=1 TO phaseGroups}
   DISPOSE(titleText3);
   Cleanup;
END PROCEDURE {PrintEventPhases};

PROCEDURE SaveEventPhases(IN printee : TableObj);
VAR
   sepBuffer, titleBuff3,titleBuff,
   tempString                        : STRING;
   phaseGroups,phaseRemainder,left   : INTEGER;
BEGIN
   GetFile("Event Phasing Table");
   IF tempFile <> "NoFile"
      NEW(exportFile);
      ASK exportFile TO Open((tempPath + tempFile), Output);
      IF exportFile.ioResult <> 0
         NEW(message, 1..2);
         message[1] := "There is a problem opening the selected save file.     ";
         message[2] := "Make sure this file has not been set to read only.     ";
         result := SendAlert(message, FALSE, FALSE, FALSE);
         DISPOSE(message);
      ELSE
         phaseGroups:=((activePhases+1) DIV 8)+1;     {add 1 to have a column for "base p"}
         phaseRemainder:=(activePhases+1) MOD 8;
         IF phaseRemainder=0
            phaseGroups:=phaseGroups-1;
            phaseRemainder:=8;      
         END IF;      
         sepBuffer := "_____________________";
         ASK exportFile TO WriteString("Event Phasing Table");
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         FOR k:=1 TO phaseGroups
            IF k<phaseGroups
               left:=8;
            ELSE
               left:=phaseRemainder;
            END IF;   
            titleBuff3 := "";
            sepBuffer := "_____________________";
            FOR j := (k-1)*8+1 TO (k-1)*8+left     {fills column header}
               sepBuffer := sepBuffer + "__";
               titleBuff3 := titleBuff3 + "  ";
               IF j=1
                  titleBuff3:= "   Base p ";
               ELSIF j <11   
                  titleBuff3 := titleBuff3 + " P00"+INTTOSTR(j-1)+"   ";
               ELSIF j<101
                  titleBuff3 := titleBuff3 + " P0"+INTTOSTR(j-1)+"   ";
               ELSE
                  titleBuff3 := titleBuff3 + " P"+INTTOSTR(j-1)+"   ";
               END IF;   
               sepBuffer := sepBuffer + "_________";
            END FOR;
            ASK exportFile TO WriteString("Event_Name          ");
            ASK exportFile TO WriteString(titleBuff3);
            ASK exportFile TO WriteLn;
         
            FOR m:=1 TO printee.NumberRows           {fills row input}
               text:= printee.Text(0,m);
               FOR n := STRLEN(printee.Text(0,m)) TO 20
                  text:= text + " ";
               END FOR;
               FOR j := (k-1)*8+1 TO (k-1)*8+left      
                  text := text + "  ";
                  tempString:=printee.Text(j,m)+"           ";
                  text := text + SUBSTR(1,8,tempString);;
               END FOR;
            ASK exportFile TO WriteString(text);;
            ASK exportFile TO WriteLn;
            END FOR;
            ASK exportFile TO WriteLn;
            ASK exportFile TO WriteLn;
         END FOR;   {k:=1 TO phaseGroups}

         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(nameOfFile);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(Date);
      END IF;
      ASK exportFile TO Close;      
      DISPOSE(exportFile);
   END IF;
END PROCEDURE {SaveEventPhases};

PROCEDURE PrintNodePhases(IN printee : TableObj);     
VAR
   rowsFinished                       : BOOLEAN;
   phaseGroups,phaseRemainder,left,
   passes, page                       : INTEGER;
   sepBuffer, titleBuff3,tempString   : STRING;
   titleText3                         : TextObj;
BEGIN
   Setup(2);
   NEW(titleText3);
   ASK titleText3 TO SetColor(Black);                
   ASK titleText3 TO SetAlignment(HorizLeft,VertMiddle); 
   ASK titleText3 TO SetSysFont("Courier New", 9, 61,0);
   ASK titleText3 TO SetTranslation(9200.*scale,28000.*scale);
   ASK titleText3 TO SetFont(SystemText);
   ASK titleText TO SetTranslation(9200.*scale,28600.*scale);
   ASK outputText TO AddGraphic(titleText3);
            
   phaseGroups:=((activePhases+2) DIV 14)+1;     {add 2 to have enough columns}
   phaseRemainder:=(activePhases+2) MOD 14;
   IF phaseRemainder=0
      phaseGroups:=phaseGroups-1;
      phaseRemainder:=14;      
   END IF;      
   numPages := (CEIL(FLOAT(printee.NumberRows)/35.))*phaseGroups;
   {tony numPages fix}
   ASK headerText TO SetText("Node Phasing Table");
   ASK titleText2 TO SetText("Node Name          ");
   page:=1;   {page number}
   FOR k:=1 TO phaseGroups
      IF k<phaseGroups
         left:=14;
      ELSE
         left:=phaseRemainder;
      END IF;   
      titleBuff3 := "";
      sepBuffer := "_____________________";
      FOR j := (k-1)*14+1 TO (k-1)*14+left     {fills column header}
         sepBuffer := sepBuffer + "__";
         titleBuff3 := titleBuff3 + "  ";
         IF j=1
            titleBuff3:= "   n  ";
         ELSIF j=2
            titleBuff3:= titleBuff3 + "  k  ";
         ELSIF j <12   
            titleBuff3 := titleBuff3 + " P00"+INTTOSTR(j-2)+"";
         ELSIF j<102
            titleBuff3 := titleBuff3 + " P0"+INTTOSTR(j-2)+"";
         ELSE
            titleBuff3 := titleBuff3 + " P"+INTTOSTR(j-2)+"";
         END IF;   
         sepBuffer := sepBuffer + "_________";
      END FOR;
      rowsFinished:=FALSE;
      passes:=0;
      REPEAT
         NEW(textBuffer, 1..35);
         FOR i:=1 TO 35           {fills row input}
            m:=i+passes*35;       {row that needs printing}
            textBuffer[i] := printee.Text(0,m);
            FOR n := STRLEN(printee.Text(0,m)) TO 20
               textBuffer[i] := textBuffer[i] + " ";
            END FOR;
            FOR j := (k-1)*14+1 TO (k-1)*14+left      
               textBuffer[i] := textBuffer[i] + "  ";
               tempString:=printee.Text(j,m)+"           ";
               textBuffer[i] := textBuffer[i] + SUBSTR(1,5,tempString);;
            END FOR;
            IF (m=printee.NumberRows)
               rowsFinished:=TRUE;
               EXIT;
            END IF;   
         END FOR;
         INC(passes);
         
         footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(page)+" of "+INTTOSTR(numPages);
         ASK footerText TO SetTextBuffer(footBuffer);
         ASK bodyText TO SetTextBuffer(textBuffer);
         ASK separatorText TO SetText(sepBuffer);
         ASK titleText3 TO SetText(titleBuff3);
         ASK outputText TO Draw;
         ASK printer TO SetUseDialog(FALSE);
         ASK printer TO Print(printWindow); 
         DISPOSE(textBuffer);
         INC(page);
      UNTIL rowsFinished;
   END FOR;   {k:=1 TO phaseGroups}
   DISPOSE(titleText3);
   Cleanup;
END PROCEDURE {PrintNodePhases};

PROCEDURE SaveNodePhases(IN printee : TableObj);
VAR
   sepBuffer, titleBuff3,titleBuff,
   tempString                        : STRING;
   phaseGroups,phaseRemainder,left   : INTEGER;
BEGIN
   GetFile("Node Phasing Table");
   IF tempFile <> "NoFile"
      NEW(exportFile);
      ASK exportFile TO Open((tempPath + tempFile), Output);
      IF exportFile.ioResult <> 0
         NEW(message, 1..2);
         message[1] := "There is a problem opening the selected save file.     ";
         message[2] := "Make sure this file has not been set to read only.     ";
         result := SendAlert(message, FALSE, FALSE, FALSE);
         DISPOSE(message);
      ELSE
         phaseGroups:=((activePhases+2) DIV 14)+1;     {add 2 to have enough columns}
         phaseRemainder:=(activePhases+2) MOD 14;
         IF phaseRemainder=0
            phaseGroups:=phaseGroups-1;
            phaseRemainder:=14;      
         END IF;      
         sepBuffer := "_____________________";
         ASK exportFile TO WriteString("Node Phasing Table");
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         FOR k:=1 TO phaseGroups
            IF k<phaseGroups
               left:=14;
            ELSE
               left:=phaseRemainder;
            END IF;   
            titleBuff3 := "";
            sepBuffer := "_____________________";
            FOR j := (k-1)*14+1 TO (k-1)*14+left     {fills column header}
               sepBuffer := sepBuffer + "_______";
               titleBuff3 := titleBuff3 + "  ";
               IF j=1
                  titleBuff3:= "   n  ";
               ELSIF j=2
                  titleBuff3:= titleBuff3 + "  k  ";
               ELSIF j <12   
                  titleBuff3 := titleBuff3 + " P00"+INTTOSTR(j-2)+"";
               ELSIF j<102
                  titleBuff3 := titleBuff3 + " P0"+INTTOSTR(j-2)+"";
               ELSE
                  titleBuff3 := titleBuff3 + " P"+INTTOSTR(j-2)+"";
               END IF;  
            END FOR;
            ASK exportFile TO WriteString("Node_Name           ");
            ASK exportFile TO WriteString(titleBuff3);
            ASK exportFile TO WriteLn;
         
            FOR m:=1 TO printee.NumberRows           {fills row input}
               text:= printee.Text(0,m);
               FOR n := STRLEN(printee.Text(0,m)) TO 20
                  text:= text + " ";
               END FOR;
               FOR j := (k-1)*14+1 TO (k-1)*14+left      
                  text := text + "  ";
                  tempString:=printee.Text(j,m)+"           ";
                  text := text + SUBSTR(1,5,tempString);;
               END FOR;
            ASK exportFile TO WriteString(text);;
            ASK exportFile TO WriteLn;
            END FOR;
            ASK exportFile TO WriteLn;
            ASK exportFile TO WriteLn;
         END FOR;   {k:=1 TO phaseGroups}
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(nameOfFile);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(Date);
      END IF;
      ASK exportFile TO Close;      
      DISPOSE(exportFile);
   END IF;
END PROCEDURE {SaveNodePhases};


PROCEDURE PrintBlockPhases(IN printee : TableObj);     
VAR
   rowsFinished                                   : BOOLEAN;
   phaseGroups,phaseRemainder,left,passes, page   : INTEGER;
   sepBuffer, titleBuff3,tempString               : STRING;
   titleText3                                     : TextObj;
BEGIN
{beta144: the number of phaseGroups per page has changed to 8 to account for spacing
          there are changes in spaces and __s added to account for spacing
          there are now 9 characters taken from tempString
          these same changes are implemented in SaveBlockPhases}
   Setup(2);
   NEW(titleText3);
   ASK titleText3 TO SetColor(Black);                
   ASK titleText3 TO SetAlignment(HorizLeft,VertMiddle); 
   ASK titleText3 TO SetSysFont("Courier New", 9, 61,0);
   ASK titleText3 TO SetTranslation(9200.*scale,28000.*scale);
   ASK titleText3 TO SetFont(SystemText);
   ASK titleText TO SetTranslation(9200.*scale,28600.*scale);
   ASK outputText TO AddGraphic(titleText3);
   
   phaseGroups:=((activePhases) DIV 8)+1;    
   phaseRemainder:=(activePhases) MOD 8;
   IF phaseRemainder=0
      phaseGroups:=phaseGroups-1;
      phaseRemainder:=8;      
   END IF;      
   numPages := (CEIL(FLOAT(printee.NumberRows)/35.))*phaseGroups;
   {tony numPages fix}
   ASK headerText TO SetText("Block Phasing Table");
   ASK titleText2 TO SetText("Block Name          ");
   page:=1;   {page number}
   FOR k:=1 TO phaseGroups
      IF k<phaseGroups
         left:=8;
      ELSE
         left:=phaseRemainder;
      END IF;   
      titleBuff3 := "";
      sepBuffer := "_____________________";
      FOR j := (k-1)*8+1 TO (k-1)*8+left     {fills column header}
         sepBuffer := sepBuffer + "___";
         titleBuff3 := titleBuff3 + "   ";
         IF j <10   
            titleBuff3 := titleBuff3 + " P00"+INTTOSTR(j)+"   ";
         ELSIF j<100
            titleBuff3 := titleBuff3 + " P0"+INTTOSTR(j)+"   ";
         ELSE
            titleBuff3 := titleBuff3 + " P"+INTTOSTR(j)+"   ";
         END IF;   
         sepBuffer := sepBuffer + "_________";
      END FOR;
      rowsFinished:=FALSE;
      passes:=0;
      REPEAT
         NEW(textBuffer, 1..35);
         FOR i:=1 TO 35           {fills row input}
            m:=i+passes*35;       {row that needs printing}
            textBuffer[i] := printee.Text(0,m);
            FOR n := STRLEN(printee.Text(0,m)) TO 20
               textBuffer[i] := textBuffer[i] + " ";
            END FOR;
            FOR j := (k-1)*8+1 TO (k-1)*8+left      
               textBuffer[i] := textBuffer[i] + "  ";
               tempString:=printee.Text(j,m)+"           ";
               textBuffer[i] := textBuffer[i] + SUBSTR(1,9,tempString);  {;  tony 11-04}
            END FOR;
            IF (m=printee.NumberRows)
               rowsFinished:=TRUE;
               EXIT;
            END IF;   
         END FOR;
         INC(passes);
         
         footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(page)+" of "+INTTOSTR(numPages);
         ASK footerText TO SetTextBuffer(footBuffer);
         ASK bodyText TO SetTextBuffer(textBuffer);
         ASK separatorText TO SetText(sepBuffer);
         ASK titleText3 TO SetText(titleBuff3);
         ASK outputText TO Draw;
         ASK printer TO SetUseDialog(FALSE);
         ASK printer TO Print(printWindow); 
         DISPOSE(textBuffer);
         INC(page);
      UNTIL rowsFinished;
   END FOR;   {k:=1 TO phaseGroups}
   DISPOSE(titleText3);
   Cleanup;
END PROCEDURE {PrintBlockPhases};

PROCEDURE SaveBlockPhases(IN printee : TableObj);
VAR
   titleBuff3,titleBuff,tempString                : STRING;
   phaseGroups,phaseRemainder,left                : INTEGER;
BEGIN
   GetFile("Block Phasing Table");
   IF tempFile <> "NoFile"
      NEW(exportFile);
      ASK exportFile TO Open((tempPath + tempFile), Output);
      IF exportFile.ioResult <> 0
         NEW(message, 1..2);
         message[1] := "There is a problem opening the selected save file.     ";
         message[2] := "Make sure this file has not been set to read only.     ";
         result := SendAlert(message, FALSE, FALSE, FALSE);
         DISPOSE(message);
      ELSE
         phaseGroups:=((activePhases) DIV 8)+1;     
         phaseRemainder:=(activePhases) MOD 8;
         IF phaseRemainder=0
            phaseGroups:=phaseGroups-1;
            phaseRemainder:=8;      
         END IF;      
         ASK exportFile TO WriteString("Block Phasing Table");
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         FOR k:=1 TO phaseGroups
            IF k<phaseGroups
               left:=8;
            ELSE
               left:=phaseRemainder;
            END IF;   
            titleBuff3 := "";
            FOR j := (k-1)*8+1 TO (k-1)*8+left     {fills column header}
               titleBuff3 := titleBuff3 + "   ";
               IF j <10   
                  titleBuff3 := titleBuff3 + " P00"+INTTOSTR(j)+"   ";
               ELSIF j<100
                  titleBuff3 := titleBuff3 + " P0"+INTTOSTR(j)+"   ";
               ELSE
                  titleBuff3 := titleBuff3 + " P"+INTTOSTR(j)+"   ";
               END IF;  
            END FOR;
            ASK exportFile TO WriteString("Block_Name           ");
            ASK exportFile TO WriteString(titleBuff3);
            ASK exportFile TO WriteLn;
         
            FOR m:=1 TO printee.NumberRows           {fills row input}
               text:= printee.Text(0,m);
               FOR n := STRLEN(printee.Text(0,m)) TO 20
                  text:= text + " ";
               END FOR;
               FOR j := (k-1)*8+1 TO (k-1)*8+left      
                  text := text + "  ";
                  tempString:=printee.Text(j,m)+"           ";
                  text := text + SUBSTR(1,9,tempString);;
               END FOR;
            ASK exportFile TO WriteString(text);;
            ASK exportFile TO WriteLn;
            END FOR;
            ASK exportFile TO WriteLn;
            ASK exportFile TO WriteLn;
         END FOR;   {k:=1 TO phaseGroups}
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(nameOfFile);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(Date);
      END IF;
      ASK exportFile TO Close;      
      DISPOSE(exportFile);
   END IF;
END PROCEDURE {SaveBlockPhases};

  
PROCEDURE PrintHierPhases (IN printee : TableObj);
VAR
   rowsFinished                       : BOOLEAN;
   phaseGroups,phaseRemainder,left,
   passes, page                       : INTEGER;
   sepBuffer, titleBuff3,tempString   : STRING;
   titleText3                         : TextObj;
BEGIN
   Setup(2);
   NEW(titleText3);
   ASK titleText3 TO SetColor(Black);                
   ASK titleText3 TO SetAlignment(HorizLeft,VertMiddle); 
   ASK titleText3 TO SetSysFont("Courier New", 9, 61,0);
   ASK titleText3 TO SetTranslation(9200.*scale,28000.*scale);
   ASK titleText3 TO SetFont(SystemText);
   ASK titleText TO SetTranslation(9200.*scale,28600.*scale);
   ASK outputText TO AddGraphic(titleText3);
            
   phaseGroups:=((activePhases) DIV 14)+1;    
   phaseRemainder:=(activePhases) MOD 14;
   IF phaseRemainder=0
      phaseGroups:=phaseGroups-1;
      phaseRemainder:=14;      
   END IF;      
   numPages := (CEIL(FLOAT(printee.NumberRows)/35.))*phaseGroups;
   ASK headerText TO SetText("Hierarchy Phasing Table");
   ASK titleText2 TO SetText("Hierarchy Name          ");
   page:=1;   {page number}
   FOR k:=1 TO phaseGroups
      IF k<phaseGroups
         left:=14;
      ELSE
         left:=phaseRemainder;
      END IF;   
      titleBuff3 := "";
      sepBuffer := "_____________________";
      FOR j := (k-1)*14+1 TO (k-1)*14+left     {fills column header}
         sepBuffer := sepBuffer + "__";
         titleBuff3 := titleBuff3 + "  ";
         IF j <10   
            titleBuff3 := titleBuff3 + " P00"+INTTOSTR(j)+"";
         ELSIF j<100
            titleBuff3 := titleBuff3 + " P0"+INTTOSTR(j)+"";
         ELSE
            titleBuff3 := titleBuff3 + " P"+INTTOSTR(j)+"";
         END IF;   
         sepBuffer := sepBuffer + "_________";
      END FOR;
      rowsFinished:=FALSE;
      passes:=0;
      REPEAT
         NEW(textBuffer, 1..35);
         FOR i:=1 TO 35           {fills row input}
            m:=i+passes*35;       {row that needs printing}
            textBuffer[i] := printee.Text(0,m);
            FOR n := STRLEN(printee.Text(0,m)) TO 21
               textBuffer[i] := textBuffer[i] + " ";
            END FOR;
            FOR j := (k-1)*14+1 TO (k-1)*14+left      
               textBuffer[i] := textBuffer[i] + "  ";
               tempString:=printee.Text(j,m)+"           ";
               textBuffer[i] := textBuffer[i] + SUBSTR(1,5,tempString);;
            END FOR;
            IF (m=printee.NumberRows)
               rowsFinished:=TRUE;
               EXIT;
            END IF;   
         END FOR;
         INC(passes);
         
         footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(page)+" of "+INTTOSTR(numPages);
         ASK footerText TO SetTextBuffer(footBuffer);
         ASK bodyText TO SetTextBuffer(textBuffer);
         ASK separatorText TO SetText(sepBuffer);
         ASK titleText3 TO SetText(titleBuff3);
         ASK outputText TO Draw;
         ASK printer TO SetUseDialog(FALSE);
         ASK printer TO Print(printWindow); 
         DISPOSE(textBuffer);
         INC(page);
      UNTIL rowsFinished;
   END FOR;   {k:=1 TO phaseGroups}
   DISPOSE(titleText3);
   Cleanup;
END PROCEDURE;

PROCEDURE SaveHierPhases (IN printee : TableObj);
VAR
   sepBuffer, titleBuff3,titleBuff,
   tempString                        : STRING;
   phaseGroups,phaseRemainder,left   : INTEGER;
BEGIN
   GetFile("Hierarchy Phasing Table");
   IF tempFile <> "NoFile"
      NEW(exportFile);
      ASK exportFile TO Open((tempPath + tempFile), Output);
      IF exportFile.ioResult <> 0
         NEW(message, 1..2);
         message[1] := "There is a problem opening the selected save file.     ";
         message[2] := "Make sure this file has not been set to read only.     ";
         result := SendAlert(message, FALSE, FALSE, FALSE);
         DISPOSE(message);
      ELSE
         phaseGroups:=((activePhases) DIV 14)+1;    
         phaseRemainder:=(activePhases) MOD 14;
         IF phaseRemainder=0
            phaseGroups:=phaseGroups-1;
            phaseRemainder:=14;      
         END IF;      
         sepBuffer := "_____________________";
         ASK exportFile TO WriteString("Hierarchy Phasing Table");
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         FOR k:=1 TO phaseGroups
            IF k<phaseGroups
               left:=14;
            ELSE
               left:=phaseRemainder;
            END IF;   
            titleBuff3 := "";
            sepBuffer := "__________________________";
            FOR j := (k-1)*14+1 TO (k-1)*14+left     {fills column header}
               sepBuffer := sepBuffer + "_______";
               titleBuff3 := titleBuff3 + "  ";
               IF j <10   
                  titleBuff3 := titleBuff3 + " P00"+INTTOSTR(j)+"";
               ELSIF j<100
                  titleBuff3 := titleBuff3 + " P0"+INTTOSTR(j)+"";
               ELSE
                  titleBuff3 := titleBuff3 + " P"+INTTOSTR(j)+"";
               END IF;  
            END FOR;
            ASK exportFile TO WriteString("Hierarchy_Name           ");
            ASK exportFile TO WriteString(titleBuff3);
            ASK exportFile TO WriteLn;
            FOR m:=1 TO printee.NumberRows           {fills row input}
               text:= printee.Text(0,m);
               FOR n := STRLEN(printee.Text(0,m)) TO 26
                  text:= text + " ";
               END FOR;
               FOR j := (k-1)*14+1 TO (k-1)*14+left      
                  text := text + "  ";
                  tempString:=printee.Text(j,m)+"           ";
                  text := text + SUBSTR(1,5,tempString);;
               END FOR;
            ASK exportFile TO WriteString(text);;
            ASK exportFile TO WriteLn;
            END FOR;
            ASK exportFile TO WriteLn;
            ASK exportFile TO WriteLn;
         END FOR;   {k:=1 TO phaseGroups}
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(nameOfFile);
         ASK exportFile TO WriteLn;
         ASK exportFile TO WriteString(Date);
      END IF;
      ASK exportFile TO Close;      
      DISPOSE(exportFile);
   END IF;
END PROCEDURE;

PROCEDURE PrintFMECA (IN printArray : strArray);     
VAR
  lastLine  : INTEGER;
BEGIN
  Setup(13);
  ASK headerText TO SetText("FMECA");
  ASK titleText  TO SetText("System Status Summary         ");
  
  REPEAT
     INC(j);
  UNTIL printArray[j]="END_OF_ARRAY";
  lastLine:=j-1;
  
  numPages:=CEIL(FLOAT(lastLine)/50.0);
  FOR i:=1 TO numPages
     IF (lastLine - i*50 < 0)
        k:=lastLine - (i-1)*50;
     ELSE
        k:=50;
     END IF;
     NEW(textBuffer, 1..50);
     FOR j:=1 TO k
        textBuffer[j] := printArray[j+(i-1)*50];
     END FOR;
     footBuffer[2] := Date+"                                                                        Page "+INTTOSTR(i)+" of "+INTTOSTR(numPages);
     ASK footerText TO SetTextBuffer(footBuffer);
     ASK bodyText TO SetTextBuffer(textBuffer);
     ASK outputText TO Draw;
     ASK printer TO SetUseDialog(FALSE);
     ASK printer TO Print(printWindow); 
     DISPOSE(textBuffer);
     INC(i);
  END FOR;
  Cleanup;
END PROCEDURE {PrintFMECA};

PROCEDURE ColorToString(IN color : ColorType) : STRING;
VAR
   colorText : STRING;
BEGIN
   IF color = Firebrick
      colorText := "140 033 033";
   ELSIF color = Red
      colorText := "255 000 000";
   ELSIF color = Black
      colorText := "000 000 000";
   ELSIF color = Blue
      colorText := "000 000 255";
   ELSIF color = LimeGreen
      colorText := "048 204 048";
   ELSIF color = DarkGreen
      colorText := "000 127 000";
   ELSIF color = Yellow
      colorText := "255 255 000";
   ELSIF color = DarkRed
      colorText := "127 000 000";
   ELSIF color = Orange
      colorText := "255 163 000";
   ELSIF color = Magenta
      colorText := "255 000 255";
   ELSIF color = Grey
      colorText := "127 127 127";
   ELSIF color = Green
      colorText := "000 255 000";
   ELSIF color = White
      colorText := "255 255 255";
   {ELSIF color = Wheat
      colorText := "999 999 999";}
   ELSIF color = Sienna
      colorText := "140 107 033";
   ELSIF color = Cyan
      colorText := "000 255 255";
   ELSIF color = DarkSlateBlue
      colorText := "107 033 140";
   ELSIF color = IndianRed
      colorText := "79 45 45";
   ELSE
      colorText := "048 204 048";
   END IF;
   RETURN colorText;
END PROCEDURE;   
   
PROCEDURE ExportRBD(IN print : BOOLEAN; IN numPages,printLevel : INTEGER; IN dir : STRING);
VAR
   pathStream                              : StreamObj;
   tempBlock                               : RBDBlockObj;
   tempEvent                               : RBDEventObj;
   tempNode                                : RBDNodeObj;
   tempHier                                : RBDHierObj;
   tempLink                                : LinkObj;
   tempImage                               : FillVObj;
   text, analText                          : TextObj;
   innerSquare                             : ImageObj;
   rtn,printObjects,spacePosition          : INTEGER;
   {rbpPath,} cmdLine1, cmdLine2, nextString : STRING;
BEGIN
   IF NOT print
      filter := "*.bmp";
      REPEAT
         GetFileName(tempFile, tempPath, filter, "Save Bitmap As");
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
   ELSE
      tempPath := "";
      tempFile := "Printing";
      ASK window TO SetSysCursor(BusyCursor);
   END IF;
   IF tempFile <> "NoFile"
      NEW(pathStream);
      {spacePosition := POSITION(installPath, " ");
      IF spacePosition=0    {no spaces in installPath}
         rbpPath := installPath;
      ELSE    
         rbpPath := AppendSlash(GetCurrentDrive());
      END IF;}    
      ASK pathStream TO Open(({rbpPath + } userPath + "fromRBD.rbp"), Output);   { wds/TES, 8/18/08 }
      IF pathStream.ioResult <> 0
         NEW(message, 1..2);
         message[1] := "There is a problem opening the fromrbd.rpb file.     ";
         message[2] := "Please contact RAPTORTECH@arinc.com to report this problem.     ";
         result := SendAlert(message, FALSE, FALSE, FALSE);
         DISPOSE(message);
         RETURN;
      END IF;
      FOREACH tempBlock IN blockGroup
         IF tempBlock.parentID=printLevel
            INC(printObjects);
         END IF;
      END FOREACH;
      FOREACH tempEvent IN eventGroup
         IF tempEvent.parentID=printLevel
            INC(printObjects);
         END IF;
      END FOREACH;
      FOREACH tempNode IN nodeGroup
         IF tempNode.parentID=printLevel
            INC(printObjects);
         END IF;
      END FOREACH;      
      FOREACH tempHier IN hierGroup
         IF tempHier.parentID=printLevel
            INC(printObjects);
         END IF;
      END FOREACH;
      IF printLevel=0
         ASK pathStream TO WriteString(nameOfFile);
      ELSE
         tempHier:= ASK root Child("RBDHier",printLevel);
         ASK pathStream TO WriteString(nameOfFile +"-"+ tempHier.name);
      END IF;
      ASK pathStream TO WriteLn;
      ASK pathStream TO WriteString(tempPath+tempFile);
      ASK pathStream TO WriteLn;
      ASK pathStream TO WriteString(dir);
      ASK pathStream TO WriteLn;
      ASK pathStream TO WriteInt(numPages,6); 
      ASK pathStream TO WriteLn;
      ASK pathStream TO WriteInt(printObjects,6); 
      ASK pathStream TO WriteLn;
      ASK pathStream TO WriteString("__________"); 
      ASK pathStream TO WriteLn;
      IF totalBlocks > 0
         FOREACH tempBlock IN blockGroup
            IF (tempBlock.parentID = printLevel)
               analText := ASK tempBlock Child("BlockAoText",0);
               innerSquare := ASK tempBlock Child("InnerSquare", 0);
               ASK pathStream TO WriteString(SUBSTR(1,28,tempBlock.name)); 
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(0,6); 
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(tempBlock.Id,6); 
               ASK pathStream TO WriteLn;
               IF ((tempBlock.usesPhasing  AND (activePhases > 0)) AND (currentView = "workspace"))
                  ASK pathStream TO WriteInt(1,6); 
               ELSE
                  ASK pathStream TO WriteInt(0,6); 
               END IF;
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(TRUNC(tempBlock.xPosition),6); 
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(TRUNC(tempBlock.yPosition),6); 
               ASK pathStream TO WriteLn;
               tempImage := ASK tempBlock Child("BasicBlock",601);
               ASK pathStream TO WriteString(ColorToString(tempImage.Color)); 
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteString(ColorToString(innerSquare.Color)); 
               ASK pathStream TO WriteLn;
               IF AoVisible OR (capacityAnalysis AND nowSimulating)
                  ASK pathStream TO WriteString(SUBSTR(1,4,analText.String)); 
                  ASK pathStream TO WriteLn;
               ELSE
                  ASK pathStream TO WriteReal(-2.000,7,5); 
                  ASK pathStream TO WriteLn;
               END IF;
               ASK pathStream TO WriteLn;
            END IF;
         END FOREACH;
      END IF;
      IF totalEvents > 0
         FOREACH tempEvent IN eventGroup
            IF (tempEvent.parentID = printLevel)
               analText := ASK tempEvent Child("EventAoText",0);
               innerSquare := ASK tempEvent Child("InnerSquare", 0);
               ASK pathStream TO WriteString(SUBSTR(1,28,tempEvent.name)); 
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(3,6); 
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(tempEvent.Id,6); 
               ASK pathStream TO WriteLn;
               IF ((tempEvent.usesPhasing  AND (activePhases > 0)) AND (currentView = "workspace"))
                  ASK pathStream TO WriteInt(1,6); 
               ELSE
                  ASK pathStream TO WriteInt(0,6); 
               END IF;
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(TRUNC(tempEvent.xPosition),6); 
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(TRUNC(tempEvent.yPosition),6); 
               ASK pathStream TO WriteLn;
               tempImage := ASK tempEvent Child("BasicBlock",601);
               ASK pathStream TO WriteString(ColorToString(tempImage.Color)); 
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteString(ColorToString(innerSquare.Color)); 
               ASK pathStream TO WriteLn;
               IF AoVisible OR (capacityAnalysis AND nowSimulating)
                  ASK pathStream TO WriteString(SUBSTR(1,4,analText.String)); 
                  ASK pathStream TO WriteLn;
               ELSE
                  ASK pathStream TO WriteReal(-2.000,7,5); 
                  ASK pathStream TO WriteLn;
               END IF;
               ASK pathStream TO WriteLn;
            END IF;
         END FOREACH;
      END IF;
      IF totalNodes > 0
         FOREACH tempNode IN nodeGroup
            IF (tempNode.parentID = printLevel)
               text := ASK tempNode Child("RBDNodeKofN", 0);
               analText := ASK tempNode Child("NodeAoText",0);
               ASK pathStream TO WriteString(SUBSTR(1,28,tempNode.name)); 
               ASK pathStream TO WriteLn;
               IF tempNode.typeNode = 2
                  ASK pathStream TO WriteInt(1,6);
               ELSE
                  ASK pathStream TO WriteInt(2,6);
               END IF;   
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(tempNode.Id,6); 
               ASK pathStream TO WriteLn;
               IF (tempNode.typeNode <> 5)
                  IF ((tempNode.usesPhasing AND (activePhases > 0)) AND (currentView = "workspace"))
                     ASK pathStream TO WriteInt(1,6); 
                  ELSE
                     ASK pathStream TO WriteInt(0,6); 
                  END IF;
               END IF;
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(TRUNC(tempNode.xPosition),6); 
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(TRUNC(tempNode.yPosition),6); 
               ASK pathStream TO WriteLn;
               tempImage := ASK tempNode Child("Node", 602);
               ASK pathStream TO WriteString(ColorToString(tempImage.Color)); 
               ASK pathStream TO WriteLn;
               IF (AoVisible AND (tempNode.typeNode <> 1)) OR (capacityAnalysis AND nowSimulating)
                  ASK pathStream TO WriteString(SUBSTR(1,4,analText.String)); 
                  ASK pathStream TO WriteLn;
               ELSE
                  ASK pathStream TO WriteReal(-2.000,7,5); 
                  ASK pathStream TO WriteLn;
               END IF;
               IF (text.String = " ") OR (text.String = "")
                  ASK pathStream TO WriteString("gram");
               ELSE
                  ASK pathStream TO WriteString(text.String);
               END IF;
               ASK pathStream TO WriteLn;
            END IF;
         END FOREACH;
      END IF;
      IF totalHiers >0
         FOREACH tempHier IN hierGroup
            IF (tempHier.parentID = printLevel)
               ASK pathStream TO WriteString(SUBSTR(1,28,tempHier.name)); 
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(4,6);
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(tempHier.Id,6); 
               ASK pathStream TO WriteLn;
               IF ((tempHier.usesPhasing AND (activePhases > 0)) AND (currentView = "workspace"))
                  ASK pathStream TO WriteInt(1,6); 
               ELSE
                  ASK pathStream TO WriteInt(0,6); 
               END IF;
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(TRUNC(tempHier.xPosition),6); 
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteInt(TRUNC(tempHier.yPosition),6); 
               ASK pathStream TO WriteLn;
               tempImage := ASK tempHier Child("Hier", 603);     
               ASK pathStream TO WriteString(ColorToString(tempImage.Color)); 
               ASK pathStream TO WriteLn;
               innerSquare := ASK tempHier Child("HierMid", 0);
               ASK pathStream TO WriteString(ColorToString(innerSquare.Color)); 
               ASK pathStream TO WriteLn;
               IF (AoVisible) OR (capacityAnalysis AND nowSimulating)
                  analText := ASK tempHier Child("HierAoText",0);
                  ASK pathStream TO WriteString(SUBSTR(1,4,analText.String)); 
                  ASK pathStream TO WriteLn;
               ELSE
                  ASK pathStream TO WriteReal(-2.000,7,5); 
                  ASK pathStream TO WriteLn;
               END IF;
               ASK pathStream TO WriteLn;
            END IF;
         END FOREACH;
      END IF;      
      IF totalLinks > 0
         FOREACH tempLink IN linkGroup
            IF (tempLink.parentID = printLevel)
               IF tempLink.connectFRef = "RBDBlock"
                  ASK pathStream TO WriteInt(tempLink.connectFromId,6);
               ELSIF tempLink.connectFRef = "RBDNode"
                  ASK pathStream TO WriteInt((tempLink.connectFromId),6);
               ELSE
                  ASK pathStream TO WriteInt((tempLink.connectFromId),6);
               END IF;
               ASK pathStream TO WriteLn;
               IF tempLink.connectTRef = "RBDBlock"
                  ASK pathStream TO WriteInt(tempLink.connectToId,6)
               ELSIF tempLink.connectTRef = "RBDNode"
                  ASK pathStream TO WriteInt((tempLink.connectToId),6);
               ELSE
                  ASK pathStream TO WriteInt((tempLink.connectToId),6);
               END IF;   
               ASK pathStream TO WriteLn;
               ASK pathStream TO WriteString(ColorToString(tempLink.Color)); 
               IF NOT(i = totalLinks) 
                  ASK pathStream TO WriteLn; 
               END IF;
            END IF;
         END FOREACH;
      END IF;      
      ASK pathStream TO Close;
      
      
      
      
     DISPOSE(pathStream);
     { cmdLine1 :=  {installPath +} "RapPrin70.exe /p """+{rbpPath+} userPath + "fromrbd.rbp""";   { wds/TES, 8/27/08 }
      cmdLine2 :=  {installPath +} "RapPrin70.exe """   +{rbpPath+} userPath + "fromrbd.rbp""";   { wds/TES, 8/27/08 }
      OUTPUT(cmdLine1);
         {***}
      IF diagnostics 
         ASK diagnosticsStream TO Open(diagFile, Append);
         IF print
            nextString:="Printing RBD.  SystemCall = " + cmdLine1;
         ELSE
            nextString:="Saving RBD as BMP.  SystemCall = " + cmdLine2;         
         END IF;
         ASK diagnosticsStream TO WriteString(nextString);   
         ASK diagnosticsStream TO WriteLn; 
         ASK diagnosticsStream TO Close; 
      END IF; 
     {***}    }
      
      { wds/TES, 8/28/08 - copy RapPrin70.exe to the user's raptor7 directory }
      rtn := CopyFile( "RapPrin70.exe", userPath + "RapPrin70.exe" );
     
      IF print
         {  rtn := SystemCall(cmdLine1,0);   wds/TES, 8/28/08 }
         PrintRBP();
      ELSE  
         { rtn := SystemCall(cmdLine2,0);    wds/TES, 8/28/08 }
         ConvertRBP();
      END IF;     
   {  IF (NOT diagnostics)
         rtn := DeleteFile({rbpPath + } userPath + "fromRBD.rbp");   { wds/TES, 8/18/08 }
         rtn := DeleteFile({rbpPath + } userPath + "RapPrin70.exe"); { wds/TES, 8/29/08 } 
      END IF;   }                                                    { cmc 10/13/08}  {decided to leave files there,
                                                 this prevents file from being deleted too early}
           
      
   END IF;
   ASK window TO SetSysCursor(NormalCursor); 
   
   
END PROCEDURE; {ExportRBD}   


PROCEDURE PrintSelected();
VAR
  tempImage      : FillVObj;
  pathStream     : StreamObj;
  block          : RBDBlockObj;
  event          : RBDEventObj;
  node           : RBDNodeObj;
  link           : LinkObj;
  text, analText : TextObj;
  thing          : RBDBasicObj;
  rtn            : INTEGER;
 { rbpPath        : STRING;}
  hier           : RBDHierObj;
  innerSquare    : ImageObj;
BEGIN
  NEW(pathStream);
   {  rbpPath := AppendSlash(GetCurrentDrive());}
     ASK pathStream TO Open(({rbpPath +} userPath + "fromRBD.rbp"), Output)   { wds/TES, 8/18/08 };
  IF pathStream.ioResult <> 0
     NEW(message, 1..2);
     message[1] := "There is a problem opening the fromrbd.rbp file.     ";
     message[2] := "Please contact RAPTORTECH@arinc.com to report this problem.     ";
     result := SendAlert(message, FALSE, FALSE, FALSE);
     DISPOSE(message);
     RETURN;
  END IF;
  ASK pathStream TO WriteString(nameOfFile);
  ASK pathStream TO WriteLn;
  ASK pathStream TO WriteString("Printing");
  ASK pathStream TO WriteLn;
  ASK pathStream TO WriteString("Landscape");
  ASK pathStream TO WriteLn;
  ASK pathStream TO WriteInt(1,6); 
  ASK pathStream TO WriteLn;
  ASK pathStream TO WriteInt((selectGroup.numberIn),6); 
  ASK pathStream TO WriteLn;
  ASK pathStream TO WriteString("__________"); 
  ASK pathStream TO WriteLn;
  FOREACH thing IN selectGroup
     IF OBJTYPENAME(thing) = "RBDBlockObj"
        block := ASK root Child("RBDBlock",thing.Id);
        analText := ASK block Child("BlockAoText",0);
        innerSquare := ASK block Child("InnerSquare", 0);
        ASK pathStream TO WriteString(SUBSTR(1,28,block.name)); 
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(0,6); 
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(block.Id,6); 
        ASK pathStream TO WriteLn;
        IF ((block.usesPhasing  AND (activePhases > 0)) AND (currentView = "workspace"))
           ASK pathStream TO WriteInt(1,6); 
        ELSE
           ASK pathStream TO WriteInt(0,6); 
        END IF;
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(TRUNC(block.xPosition),6); 
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(TRUNC(block.yPosition),6); 
        ASK pathStream TO WriteLn;
        tempImage := ASK block Child("BasicBlock",601);
        ASK pathStream TO WriteString(ColorToString(tempImage.Color)); 
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteString(ColorToString(innerSquare.Color)); 
        ASK pathStream TO WriteLn;
        IF AoVisible OR (capacityAnalysis AND nowSimulating)
           ASK pathStream TO WriteString(SUBSTR(1,4,analText.String)); 
           ASK pathStream TO WriteLn;
        ELSE
           ASK pathStream TO WriteReal(-2.000,7,5); 
           ASK pathStream TO WriteLn;
        END IF;
        ASK pathStream TO WriteLn;
     ELSIF OBJTYPENAME(thing) = "RBDEventObj"
        event := ASK root Child("RBDEvent",thing.Id);
        analText := ASK event Child("EventAoText",0);
        innerSquare := ASK event Child("InnerSquare", 0);
        ASK pathStream TO WriteString(SUBSTR(1,28,event.name)); 
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(3,6); 
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(event.Id,6); 
        ASK pathStream TO WriteLn;
        IF ((event.usesPhasing  AND (activePhases > 0)) AND (currentView = "workspace"))
           ASK pathStream TO WriteInt(1,6); 
        ELSE
           ASK pathStream TO WriteInt(0,6); 
        END IF;
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(TRUNC(event.xPosition),6); 
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(TRUNC(event.yPosition),6); 
        ASK pathStream TO WriteLn;
        tempImage := ASK event Child("BasicBlock",601);
        ASK pathStream TO WriteString(ColorToString(tempImage.Color)); 
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteString(ColorToString(innerSquare.Color)); 
        ASK pathStream TO WriteLn;
        IF AoVisible OR (capacityAnalysis AND nowSimulating)
           ASK pathStream TO WriteString(SUBSTR(1,4,analText.String)); 
           ASK pathStream TO WriteLn;
        ELSE
           ASK pathStream TO WriteReal(-2.000,7,5); 
           ASK pathStream TO WriteLn;
        END IF;
        ASK pathStream TO WriteLn;
     ELSIF OBJTYPENAME(thing) = "RBDNodeObj"
        node := ASK root Child("RBDNode",thing.Id);
        text := ASK node Child("RBDNodeKofN", 0);
        analText := ASK node Child("NodeAoText",0);
        ASK pathStream TO WriteString(SUBSTR(1,28,node.name)); 
        ASK pathStream TO WriteLn;
        IF node.typeNode = 2
           ASK pathStream TO WriteInt(1,6);
        ELSE
           ASK pathStream TO WriteInt(2,6);
        END IF;   
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(node.Id,6); 
        ASK pathStream TO WriteLn;
        IF (node.typeNode <> 5)
           IF ((node.usesPhasing AND (activePhases > 0)) AND (currentView = "workspace"))
              ASK pathStream TO WriteInt(1,6); 
           ELSE
              ASK pathStream TO WriteInt(0,6); 
           END IF;
        END IF;
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(TRUNC(node.xPosition),6); 
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(TRUNC(node.yPosition),6); 
        ASK pathStream TO WriteLn;
        tempImage := ASK node Child("Node", 602);
        ASK pathStream TO WriteString(ColorToString(tempImage.Color)); 
        ASK pathStream TO WriteLn;
        IF (AoVisible AND (node.typeNode <> 1)) OR (capacityAnalysis AND nowSimulating)
           ASK pathStream TO WriteString(SUBSTR(1,4,analText.String)); 
           ASK pathStream TO WriteLn;
        ELSE
           ASK pathStream TO WriteReal(-2.000,7,5); 
           ASK pathStream TO WriteLn;
        END IF;
        IF (text.String = " ") OR (text.String = "")
           ASK pathStream TO WriteString("gram");
        ELSE
           ASK pathStream TO WriteString(text.String);
        END IF;
        ASK pathStream TO WriteLn;
     ELSIF OBJTYPENAME(thing) = "RBDHierObj"
        hier := ASK root Child("RBDHier",thing.Id);
        ASK pathStream TO WriteString(SUBSTR(1,28,hier.name)); 
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(4,6);
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(hier.Id,6); 
        ASK pathStream TO WriteLn;
        IF ((hier.usesPhasing AND (activePhases > 0)) AND (currentView = "workspace"))
           ASK pathStream TO WriteInt(1,6); 
        ELSE
           ASK pathStream TO WriteInt(0,6); 
        END IF;
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(TRUNC(hier.xPosition),6); 
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(TRUNC(hier.yPosition),6); 
        ASK pathStream TO WriteLn;
        tempImage := ASK hier Child("Hier", 603);     
        ASK pathStream TO WriteString(ColorToString(tempImage.Color)); 
        ASK pathStream TO WriteLn;
        innerSquare := ASK hier Child("HierMid", 0);
        ASK pathStream TO WriteString(ColorToString(innerSquare.Color)); 
        ASK pathStream TO WriteLn;
        IF (AoVisible) OR (capacityAnalysis AND nowSimulating)
           analText := ASK hier Child("HierAoText",0);
           ASK pathStream TO WriteString(SUBSTR(1,4,analText.String)); 
           ASK pathStream TO WriteLn;
        ELSE
           ASK pathStream TO WriteReal(-2.000,7,5); 
           ASK pathStream TO WriteLn;
        END IF;
        ASK pathStream TO WriteLn;
     END IF;      
  END FOREACH;
  IF linksIn > 0
     FOREACH link IN selectedLinksGroup
        ASK pathStream TO WriteInt(link.connectFromId,6);
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteInt(link.connectToId,6)
        ASK pathStream TO WriteLn;
        ASK pathStream TO WriteString(ColorToString(link.Color)); 
        IF NOT (ASK selectedLinksGroup Next(link) = NILOBJ)
           ASK pathStream TO WriteLn; 
        END IF;       
     END FOREACH;
  END IF;      
  ASK pathStream TO Close;
  DISPOSE(pathStream);
  
  { wds/TES, 8/28/08 - copy RapPrin70.exe to the user's raptor7 directory }
  rtn := CopyFile( "RapPrin70.exe", userPath + "RapPrin70.exe" );
  { wds/TES 8/28/08 - print from user's raptor7 directory }
  PrintRBP();
  
  { rtn := SystemCall({installPath +} "RapPrin70.exe /p """+ {rbpPath +} userPath + "fromrbd.rbp""",0); }   { wds/TES, 8/27/08 }
  IF ((NOT diagnostics) AND (NOT doorOpen))
     rtn := DeleteFile({rbpPath +} userPath + "fromRBD.rbp");   { wds/TES, 8/18/08 }
     rtn := DeleteFile({rbpPath +} userPath + "RapPrin70.exe"); { wds/TES, 8/28/08 }    
  END IF;
END PROCEDURE {PrintSelected};

PROCEDURE PrintPhases (IN printee : TableObj);  
VAR
   name,type : STRING;
BEGIN
   Setup(3);
   numPages := CEIL(FLOAT(printee.NumberRows)/35.);
   ASK headerText TO SetText("Phase Details Table");
   ASK titleText TO SetText("");
   ASK titleText2 TO SetText("Phase Name               P##                   Length    Mission     ");
   ASK separatorText TO SetText("_________________________________________________________________");
   FOR i := 1 TO numPages                                                  
      NEW(textBuffer, 1..35);
      FOR j := ((i-1)*35)+1 TO (i*35)
         m := j - ((i-1)*35);     {line number}
         IF j <= printee.NumberRows
            textBuffer[m] := printee.Text(1,j);
            FOR k := (STRLEN(printee.Text(1,j))+STRLEN(printee.Text(2,j))) TO 25
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m] +"  "+ printee.Text(2,j);
            FOR k := STRLEN(printee.Text(3,j)) TO 20
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m]+"   " + printee.Text(3,j);
            FOR k := STRLEN(printee.Text(4,j)) TO 9
               textBuffer[m] := textBuffer[m] + " ";
            END FOR;
            textBuffer[m] := textBuffer[m]+" " + printee.Text(4,j);
         ELSE
            textBuffer[m] := "";
         END IF;
      END FOR;
      footBuffer[2] := Date+"                                                                                                       Page "+INTTOSTR(i)+" of "+INTTOSTR(numPages);
      ASK footerText TO SetTextBuffer(footBuffer);
      ASK bodyText TO SetTextBuffer(textBuffer);
      ASK outputText TO Draw;
      ASK printer TO SetUseDialog(FALSE);
      ASK printer TO Print(printWindow); 
      DISPOSE(textBuffer);
   END FOR;
   Cleanup;
END PROCEDURE {PrintPhases}

PROCEDURE SavePhases (IN printee : TableObj); 
VAR
   text                          : STRING;
BEGIN
  GetFile("Phase Details Table");
  IF tempFile <> "NoFile"
     NEW(exportFile);
     ASK exportFile TO Open((tempPath + tempFile), Output);
     IF exportFile.ioResult <> 0
        NEW(message, 1..2);
        message[1] := "There is a problem opening the selected save file.     ";
        message[2] := "Make sure this file has not been set to read only.     ";
        result := SendAlert(message, FALSE, FALSE, FALSE);
        DISPOSE(message);
     ELSE
        numPages := CEIL(FLOAT(printee.NumberRows)/35.);
        ASK exportFile TO WriteString("Phase Details Table");
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString("Phase Name               P##                  Length    Mission     ");
        ASK exportFile TO WriteLn;
        FOR m:=1 TO printee.NumberRows           {fills row input}
           text := printee.Text(1,m);
           FOR k := (STRLEN(printee.Text(1,m))+STRLEN(printee.Text(2,m))) TO 25
              text := text + " ";
           END FOR;
           text := text +"  "+ printee.Text(2,m);
           FOR k := STRLEN(printee.Text(3,m)) TO 20
              text := text + " ";
           END FOR;
           text := text+"   " + printee.Text(3,m);
           FOR k := STRLEN(printee.Text(4,m)) TO 9
              text := text + " ";
           END FOR;
           text := text+" " + printee.Text(4,m);
           ASK exportFile TO WriteString(text);;
           ASK exportFile TO WriteLn;
        END FOR;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString(nameOfFile);
        ASK exportFile TO WriteLn;
        ASK exportFile TO WriteString(Date);
     END IF;
     ASK exportFile TO Close;      
     DISPOSE(exportFile);
  END IF;
END PROCEDURE; {SavePhases}

PROCEDURE PrintTree;
VAR
  totalLines               : INTEGER;
  textStrings              : strArray;
BEGIN
  Setup(13);
  ASK headerText TO SetText("Hierarchy Tree");
{  ASK titleText  TO SetText("Tree        ");  }
  totalLines:= totalBlocks+totalEvents+totalNodes+totalHiers;  
  numPages:=CEIL(FLOAT(totalLines)/50.0);
  NEW(textBuffer, 1..50);
  NEW(textStrings,1..totalLines);
  m:=1;      {this m is being set for the procedure GetTreeLevels}
  FillTreeLevels("",0,textStrings);
  FOR i:=1 TO numPages
     IF (totalLines - i*50 < 0)
        k:=totalLines - (i-1)*50;
     ELSE
        k:=50;
     END IF;
     FOR m:=1 TO 50   
        IF (m <= k)
           textBuffer[m]:=textStrings[m+(i-1)*50];
        ELSE
           textBuffer[m]:="";
        END IF;   
        INC(m);
     END FOR;  
     footBuffer[2] := Date+"                                                                        Page "+INTTOSTR(i)+" of "+INTTOSTR(numPages);
     ASK footerText TO SetTextBuffer(footBuffer);
     ASK bodyText TO SetTextBuffer(textBuffer);
     ASK outputText TO Draw;
     ASK printer TO SetUseDialog(FALSE);
     ASK printer TO Print(printWindow); 
     INC(i);
  END FOR;
  DISPOSE(textStrings);
  DISPOSE(textBuffer);
  Cleanup;
END PROCEDURE;

PROCEDURE FillTreeLevels (IN tab : STRING; IN parent : INTEGER; IN textStrings : strArray); 
VAR
  block                    : RBDBlockObj;
  node                     : RBDNodeObj;
  hier                     : RBDHierObj;
  event                    : RBDEventObj;
BEGIN  
  FOREACH block IN blockGroup
     IF block.parentID=parent
        textStrings[m]:=tab+"block-"+INTTOSTR(block.Id)+" "+block.name;
        INC(m);
     END IF;
  END FOREACH;      
  FOREACH event IN eventGroup
     IF event.parentID=parent
        textStrings[m]:=tab+"event-"+INTTOSTR(event.Id)+" "+event.name;
        INC(m);
     END IF;
  END FOREACH;      
  FOREACH node IN nodeGroup
     IF node.parentID=parent
        textStrings[m]:=tab+"node-"+INTTOSTR(node.Id)+" "+node.name;
        INC(m);
     END IF;
  END FOREACH;      
  FOREACH hier IN hierGroup
     IF hier.parentID=parent
        textStrings[m]:=tab+"hier-"+INTTOSTR(hier.Id)+" "+hier.name;
        INC(m);
           {now print whats under the hier}
        FillTreeLevels(tab+"  ",hier.Id,textStrings);
     END IF;   
  END FOREACH;      
END PROCEDURE;

PROCEDURE SetOutputStrings
BEGIN
  IF NumRunsCompleted>1
     runString:=INTTOSTR(NumRunsCompleted)+" runs";
  ELSE
     runString:=INTTOSTR(NumRunsCompleted)+" run";
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
END PROCEDURE;

END MODULE. {imod print}



