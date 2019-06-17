{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Definition Module : Intract                                                   +}
{+  Author            : Steve Brown / Elizabeth Grimes                            +}
{+  Last Modified     : 2 July 03                                                 +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

DEFINITION MODULE Intract; 

FROM Form    IMPORT DialogBoxObj;
FROM GTypes  IMPORT TextBufferType, OptionListType, TextHorizType;
FROM GTable  IMPORT TableObj;
FROM GTab    IMPORT TabObj;      
FROM Button  IMPORT ButtonObj;
FROM TextBox IMPORT TextBoxObj, ComboBoxObj;
FROM Radio   IMPORT RadioBoxObj, RadioButtonObj;
FROM Value   IMPORT ValueBoxObj;
FROM Check   IMPORT CheckBoxObj;
FROM Label   IMPORT LabelObj;
FROM Objects IMPORT RBDBlockObj, RBDEventObj, RBDNodeObj, RBDHierObj, boolArray, intArray, realArray, strArray, ALL SparingType,
                    SparePoolObj, RapTriggerObj;
FROM Display IMPORT selectGroupObj;
FROM Print   IMPORT timeType;
FROM Runsim  IMPORT FinalArrayType;

TYPE
   AboutBoxObj = OBJECT(DialogBoxObj);
      ASK METHOD ScrollCredits;
   END OBJECT; {AboutBoxObj}
   
   HelpBoxObj = OBJECT (DialogBoxObj);
   ASK METHOD CheckValidInput(IN distro, numParam : INTEGER;
                              IN values           : realArray;
                              IN distroUse        : STRING;
                              OUT flag            : BOOLEAN;
                              INOUT errorText     : strArray);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {HelpBoxObj}
   
   BlockPropObj = OBJECT(HelpBoxObj);
      advTab, distroTab, maintTab, prevMaintTab, costTab, 
      dependTab, resTab, simTheoryTab                         : TabObj;
      resInUse, poolInUse, buddy                     : STRING;
      nameBox, commentBox                                     : TextBoxObj;
      updateButton, doneCostButton, defButton, createTrigger,
      createSpare, createResources                            : ButtonObj;
      freqLabel, 
      firesLabel, durationLabel, optionsLabel, staggerLabel,
      repairSTDev, failSTDev, startStateText                  : LabelObj;
      failLabels, repairLabels, failUnits, repairUnits        : ARRAY INTEGER OF LabelObj;
      failVals, repairVals, costVals                          : ARRAY INTEGER OF ValueBoxObj;
      buddyCombo, indRCombo, indFCombo,res2Combo, res3Combo,
      sysCombo, res1Combo, poolCombo, failCombo, repairCombo,
      blockTypeCombo, trigComboBox, res4Combo, res5Combo      : ComboBoxObj;
      dependRadBox, streamRadBox, maintRadBox, degradeRadBox,
      startRadBox, failedRadBox, freqRadBox, durRadBox        : RadioBoxObj;
      sysButton,indepButton, itemButton, infRadButton, 
      poolRadButton, cusRadButton, sysStrmButton, preButton,
      indStrmButton, linearButton, upButton, downButton, 
      localButton, randomButton, repairButton,
      geoButton,assButton, randButton, hourRadButton, 
      trigRadButton, fixedRadButton, distRadButton            : RadioButtonObj;
      arrRateVal, emerVal, newSparesVal, stockVal,
      stockAmount, stockRate, expiredVal, 
      initStockVal, degradeVal1, 
      degradeVal2, hourValBox, staggerVal                                  : ValueBoxObj;
      degradeChk, phaseChk,emerChk,newChk,stockChk,doneBool,
      spareChkBox, refreshChkBox, misDefChkBox, failChkBox,
      resDefChkBox,
      pmActivitiesChkBox, failAntiChkBox, repairAntiChkBox    : CheckBoxObj;
      durTable, preTable, postTable               : TableObj; 
      {eventList, opList,} buddyList, tempResList               : OptionListType;
      tempPreDist, tempPostDist, tempDurDist,
      numDistroParm, helpNum                                  : INTEGER;
      oldSpareCost,oldEmerSpCost                              : REAL; 
      realsArray, preVals, postVals, tempDurParams            : realArray;
      ASK METHOD ReceiveData     (INOUT dBlock                : RBDBlockObj;
                                  IN  helpCase                : STRING;
                                  OUT cancel                  : BOOLEAN);
      ASK METHOD LoadDefs        (IN type                     : INTEGER);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {BlockPropObj}

   HierBoxObj = OBJECT(HelpBoxObj);
      nameBox, commentBox : TextBoxObj;
      phaseChk : CheckBoxObj;
      dependRadBox                               : RadioBoxObj;
      indepButton, sysButton, itemButton,
      localButton                                : RadioButtonObj;
      buddyCombo : ComboBoxObj;
      ASK METHOD ReceiveData(INOUT tempHier : RBDHierObj;
                             OUT cancelled  : BOOLEAN);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {DetailsBoxObj}

   MassEditBoxObj = OBJECT(HelpBoxObj);
      blockTab, eventTab, nodeTab, hierTab                                   : TabObj;
      dependRadBox, bPhaseRadBox, ePhaseRadBox, nPhaseRadBox, 
      hPhaseRadBox, setKRadBox, nodeDepRad, hierDepRad, bNameRadBox,
      eNameRadBox, nNameRadBox, hNameRadBox                                  : RadioBoxObj;
      sysDepItem, sysIndItem, bPhaseItem, bNoPhaseItem, ePhaseItem, 
      eNoPhaseItem, nPhaseItem, nNoPhaseItem, hPhaseItem, 
      hNoPhaseItem, oneItem, nItem, bElementRad, nElementRad, hElementRad,
      bSuffixRadButton, eSuffixRadButton, nSuffixRadButton, hSuffixRadButton : RadioButtonObj;
      ALDTTable,b,c                                                          : TableObj;   
      successValBox                                                          : ValueBoxObj;
      sysDepChk, noRepairChk, infSparesChk, bReqPhaseChk, 
      successChk, eReqPhaseChk, nReqPhaseChk, hReqPhaseChk, setKChk,
      bSysStreamChk, eSysStreamChk, noResChk, nodeDepBox, hierDepBox,
      bNameChk, eNameChk, nNameChk, hNameChk                                 : CheckBoxObj;
      bSysCombo, eSysCombo, hElementCombo, bElementCombo, nElementCombo      : ComboBoxObj;    
      bNameTextBox, eNameTextBox, nNameTextBox, hNameTextBox                 : TextBoxObj;
      ASK METHOD MakeChanges (IN changeGroup    : selectGroupObj);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {MassEditBoxObj}  
   
   EventsBoxObj = OBJECT(HelpBoxObj);
      advTab, costTab                            : TabObj;
      eventName, commentBox                      : TextBoxObj;
      streamRadBox                               : RadioBoxObj;
      sysButton, indButton                       : RadioButtonObj;
      probVal, initCost, successCost, failCost   : ValueBoxObj;
      phaseChk, indChkBox                        : CheckBoxObj;
      sysCombo, indCombo                         : ComboBoxObj;
      ASK METHOD ReceiveData (INOUT event   : RBDEventObj;
                              OUT cancel     : BOOLEAN);                          
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {EventsBoxObj}

   NodeBoxObj = OBJECT (HelpBoxObj);
      dependTab, capTab, coldTab, advTab         : TabObj;
      pathsText, 
      coldChangeTxt1,
      flowGenText, coldChangeTxt2, capChangeText,
      flowInTxt, flowOutTxt, kStarText           : LabelObj;
      dependRadBox                               : RadioBoxObj;
      indepButton, sysButton, itemButton,
      localButton                                : RadioButtonObj;
      nodeName, commentBox                       : TextBoxObj;
      nodeNum, pathsReqBox, 
      kStarVal                                   : ValueBoxObj;
      phaseChk, coldChk, priorChk, flowOutChk,
      orderChk,nodeInfoChk,flowInChk             : CheckBoxObj;
      buddyCombo, coldChangeVal, capChangeVal    : ComboBoxObj;
      coldTable, flowInTable, 
      flowOutTable                               : TableObj;
      coldButton, capButton                      : ButtonObj;
      nodeUp                                     : RBDNodeObj;
      tempNum, changeNum, blueNodes, oldNum,
      connectIn,connectOut,
      totalOut, totalIn                          : INTEGER;
      startAble, endAble                         : BOOLEAN;
      capVals                                    : ARRAY INTEGER OF STRING;
      ASK METHOD ReceiveData (INOUT cancelled            : BOOLEAN;
                              IN adding                  : BOOLEAN;
                              IN tempNode                : RBDNodeObj;
                              IN initType                : INTEGER);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {NodeBoxObj}
   
   PhaseBoxObj =    OBJECT(HelpBoxObj);
      fromValues,toValues, phaseDistArray                      : ARRAY INTEGER OF INTEGER;
      translateBArray, translateHArray,
      translateEArray,translateNArray          : ARRAY INTEGER, INTEGER OF INTEGER; 
      bMode, nMode, bText,nText, eMode, eText,
      hMode, hText                             : STRING;  
      startTime,phaseNum                       : LabelObj;
      stressBox, kValBox, endTimeBox           : ValueBoxObj;
      bTable, nTable, eTable, hTable, dTable   : TableObj;
      bCombo, nCombo, eCombo, hCombo           : ComboBoxObj; 
      bTab, nTab, eTab, hTab                   : TabObj;
      okButton,applyButton, deleteButton,editButton,
      clrAllButton, n4Butt, e1Butt, e2Butt, 
      e3Butt, e4Butt, b1Butt, b2Butt, b3Butt,
      b4Butt, h1Butt, h2Butt, h3Butt, h4Butt   : ButtonObj;
      phaseDistTable                           : TableObj;
      missionChkBox                            : CheckBoxObj;
      phaseName                                : TextBoxObj;
      phaseParamsArray                         : ARRAY INTEGER OF realArray;
      cellAlign                                : TextHorizType;
      oldBCol, oldBRow, oldNCol, oldNRow, 
      oldECol, oldERow, oldHCol, oldHRow       : INTEGER;
      ASK METHOD InitSelf;
      ASK METHOD AdjustTables(IN addPhase                  : BOOLEAN;
                              IN changePhase               : INTEGER);
      ASK METHOD ShowTabs;
      ASK METHOD FillTables;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {PhaseBoxObj}

   PrefsBoxObj = OBJECT (HelpBoxObj);
      soundChk, saveChk           : CheckBoxObj;
      unitsBox, sysStatus         : ComboBoxObj;
      saveVal                     : ValueBoxObj;
      expoRadBox, logRadBox : RadioBoxObj;
      mtbfButton, fRateButton,
      meanButton,muButton  : RadioButtonObj;
      ASK METHOD ReceiveData();{INOUT saveOn, sound, player : BOOLEAN;
                                INOUT tempUnits             : STRING;
                                INOUT saveIn                : INTEGER;
                                INOUT imageString           : STRING;
                                OUT   cancelled             : BOOLEAN);}
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {PrefsBoxObj}

   SystemBoxObj = OBJECT (HelpBoxObj);
      genTab, simTab, streamsTab, weakLinkTab : TabObj;
      antiStreamA, antiStreamB, antiStreamC,
      antiStreamD, antiStreamE, antiStreamF,
      antiStreamG, antiStreamH, antiStreamI,
      antiStreamJ, antiAncStream, negValChkBox,
      graphicsChkBox                          : CheckBoxObj;
      unitsBox, sysStatus, streamA, 
      streamB, streamC, streamD, streamE, 
      streamF, streamG, streamH, streamI,
      streamJ, ancStream                      : ComboBoxObj;
      stopRadBox, weakLinkRad                 : RadioBoxObj;
      dependButt, relyButt, availButt         : RadioButtonObj;
      gyThreshVal, yrThreshVal, capValBox,
      lostCostBox, redCostBox, simTimeVal,
      simFailVal, simCycleVal, simRunsVal,
      startTimeVal                            : ValueBoxObj;
      yellowText, redText, stopLabel1, 
      stopLabel2, stopLabel3, startLabel2,
      volumeText, lostText1, lostText2        : LabelObj;
      commentBox                              : TextBoxObj;
      ASK METHOD GetColors(OUT cancelled : BOOLEAN);
      ASK METHOD ReceiveData;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {SystemBoxObj}

   PrintTablesBoxObj = OBJECT (DialogBoxObj);
      blockFailBox, blockRepBox, blockPMBox, blockMaintBox, blockCostBox,
      blockDepBox, blockAdvBox, nodeGenBox, nodeStandbyBox, nodeCapBox,
      eventsBox, hierBox, sysBox, spareBox, resBox, trigBox, detailsPhaseBox, blockPhaseBox,
      nodePhaseBox, eventPhaseBox, hierPhaseBox, summaryBox, logBox, 
      spareOutBox, blockCostOut, blockAnalBox, nodeAnalBox, eventAnalBox,
      hierAnalBox, capOutBox                                                : CheckBoxObj;
      savingText                              : BOOLEAN;
      ASK METHOD GetPreferences(IN savingTXT  : BOOLEAN);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {PrintTablesBoxObj}

   PrintWinObj = OBJECT(HelpBoxObj);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {PrintWinObj}

   SimBoxObj = OBJECT(HelpBoxObj);
      generalTab, advTab, fileTab, avtGrdTab    : TabObj;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {SimBoxObj}

   ResimBoxObj = OBJECT(HelpBoxObj);
      rStopRadBox                               : RadioBoxObj;
      rTimeRadButton, rFailRadButton, rCycleRadButton            : RadioButtonObj;
      rLengthVal, rFailVal, rCycleVal, rRunsVal            : ValueBoxObj;
      {rPhaseSimBox,} rGraphicsBox, rStepBox,
      rEventLogBox, rResultsBox, rRunResultsBox,
      rStatusChkBox                             : CheckBoxObj;
      stopLabel1, stopLabel2, stopLabel3                    : LabelObj;
      ASK METHOD GetPreferences(INOUT lengthNum : REAL; 
                                INOUT truncType,runsNum : INTEGER; OUT cancelSim : BOOLEAN;
                                INOUT phasingInUse,
                                      inStep,startStep, statBarOn,
                                      graphics,evntChk,rsltChk,finalChk : BOOLEAN); 
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {SimBoxObj}

   DataBoxObj = OBJECT(HelpBoxObj);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {DataBoxObj}

   ResultsBoxObj = OBJECT(DialogBoxObj);
      OVERRIDE
         ASK METHOD BeClosed;
   END OBJECT; {ResultsBoxObj}
   
   BlockInputBoxObj = OBJECT(HelpBoxObj);
      distroTab, sparesTab, pmTab, maintTab, 
      costsTab, depTab, advTab                     : TabObj;
      dFilterText, sFilterText, pmFilterText,
      mFilterText, cFilterText, depFilterText,
      aFilterText, failHeader, repHeader           : LabelObj;     
      distroFilter, sparesFilter, pmFilter,  
      maintFilter, costFilter, depFilter, advFilter: ComboBoxObj;
      applyButton                                  : ButtonObj;
      distroTable, sparesTable, pmTable, 
      maintTable, costTable, depTable, advTable    : TableObj;
      {translateArray                               : intArray;}
      labels                                       : ARRAY INTEGER OF LabelObj;
      values                                       : ARRAY INTEGER OF ValueBoxObj;
      oldDFilter, oldSFilter, oldPMFilter,
      oldMFilter, oldCFilter, oldDepFilter,
      oldAFilter                                   : STRING;
      displayDArray, displaySArray, displayPMArray,
      displayMArray, displayCArray, displayDepArray, 
      displayAArray, DArray, SArray, PMArray, 
      MArray, CArray, DepArray, AArray             : FinalArrayType;
      ASK METHOD InitSelf(IN printing : BOOLEAN);
      ASK METHOD ManageLabels(IN distro    : INTEGER; IN failures  : BOOLEAN);
      ASK METHOD DistSort    (IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
      ASK METHOD RepSort     (IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
      ASK METHOD PMSort      (IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
      ASK METHOD MaintSort   (IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
      ASK METHOD CostSort    (IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
      ASK METHOD DepSort     (IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
      ASK METHOD AdvSort     (IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
      ASK METHOD DistInsert  (IN i, pos, numPlaced    : INTEGER);
      ASK METHOD RepInsert   (IN i, pos, numPlaced    : INTEGER);
      ASK METHOD PMInsert    (IN i, pos, numPlaced    : INTEGER);
      ASK METHOD MaintInsert (IN i, pos, numPlaced    : INTEGER);
      ASK METHOD CostInsert  (IN i, pos, numPlaced    : INTEGER);
      ASK METHOD DepInsert   (IN i, pos, numPlaced    : INTEGER);
      ASK METHOD AdvInsert   (IN i, pos, numPlaced    : INTEGER);
      OVERRIDE
         ASK METHOD BeSelected;
         ASK METHOD ObjTerminate;
   END OBJECT; {BlockInputboxObj}

   NodeInputBoxObj = OBJECT(HelpBoxObj);
      genTab, standbyTab, capTab           : TabObj;
      genTable, standbyTable, capInTable     : TableObj;
      gFilterText, sFilterText, cFilterText : LabelObj;     
      genFilter, standbyFilter, capFilter : ComboBoxObj;
      translateArray, coldArray, capArray,
      coldArray2, capArray2                     : intArray;
      coldLinks, capLinks                       : INTEGER;
      oldGFilter, oldSFilter, oldCFilter        : STRING;
      displayGArray, displaySArray, displayCArray,
      GArray, SArray, CArray : FinalArrayType;
      ASK METHOD InitSelf(IN printing : BOOLEAN);
      ASK METHOD GenSort    (IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
      ASK METHOD StandbySort(IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
      ASK METHOD CapSort    (IN sortCol, tableSize : INTEGER; IN sortDir : STRING); 
      ASK METHOD GenInsert  (IN i, pos, numPlaced    : INTEGER);
      ASK METHOD StandbyInsert(IN i, pos, numPlaced    : INTEGER);
      ASK METHOD CapInsert    (IN i, pos, numPlaced    : INTEGER);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {NodeInputboxObj}

   EventInputBoxObj = OBJECT(HelpBoxObj);
      genTable                                  : TableObj;
      gFilterText                               : LabelObj;
      genFilter                                 : ComboBoxObj;
      displayGArray, GArray                     : FinalArrayType;
      oldGFilter                                : STRING;
      ASK METHOD InitSelf(IN printing : BOOLEAN);
      ASK METHOD GenSort    (IN sortCol, tableSize : INTEGER; IN sortDir : STRING);
      ASK METHOD GenInsert  (IN i, pos, numPlaced    : INTEGER);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {EventInputBoxObj}

   HierInputBoxObj = OBJECT(HelpBoxObj);
      genTable                                  : TableObj;
      displayGArray                             : FinalArrayType;
      ASK METHOD InitSelf;
      ASK METHOD GenSort    (IN sortCol   : INTEGER; IN sortDir : STRING);
      ASK METHOD GenInsert  (IN currHier : RBDHierObj; IN pos, numPlaced    : INTEGER);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {HierInputBoxObj}

   SystemInputBoxObj = OBJECT(HelpBoxObj);
      sysTab, spareTab, resTab, trigTab        : TabObj;
      sysTable, spareTable, resTable, trigTable: TableObj;
      truncLabel, trialLabel, statLabel, blockLabel, nodeLabel, eventLabel, linkLabel, 
      hierLabel, redLabel, flowLabel, spareLabel, resLabel, trigLabel,
      volumeLabel, ancLabel    : LabelObj;
      displaySArray, displayRArray, displayTArray : FinalArrayType;
      sysArray : strArray;
      ASK METHOD InitSelf(IN printing : BOOLEAN);
      ASK METHOD SpareSort   (IN sortCol   : INTEGER; IN sortDir : STRING);
      ASK METHOD ResSort     (IN sortCol   : INTEGER; IN sortDir : STRING);
      ASK METHOD TrigSort    (IN sortCol   : INTEGER; IN sortDir : STRING);
      ASK METHOD SpareInsert (IN pool : SparePoolObj; IN pos, numPlaced    : INTEGER);
      ASK METHOD ResInsert   (IN pool : SparePoolObj; IN pos, numPlaced    : INTEGER);
      ASK METHOD TrigInsert  (IN trig : RapTriggerObj; IN pos, numPlaced    : INTEGER);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {SystemInputBoxObj}
   
   TablesOutObj = OBJECT(HelpBoxObj);
      resultsTab, logisticsTab, sparingTab, costsTab, 
      blockAnalTab, nodeAnalTab, eventAnalTab, hierAnalTab, capTab           : TabObj;
      resultsTable, logisticsTable, sparesTable, costTable, 
      blockAnalTable, nodeAnalTable, eventAnalTable, hierAnalTable, capOutTable : TableObj;
      resultsLabel, mrLabel, logisticsLabel, sparesLabel, costLabel,
      blockAnalLabel, nodeAnalLabel, eventAnalLabel, hierAnalLabel, capLabel,
      sFilterText, bcFilterText, bFilterText, nFilterText, eFilterText, 
      cFilterText, sumLabel2, sumLabel3, logLabel1, logLabel2                : LabelObj;
      sparesFilter, costFilter, blockAnalFilter, nodeAnalFilter, 
      eventAnalFilter, capFilter                                             : ComboBoxObj;
      sort1,sort2,sort3,sort4, oldSFilter, oldBCFilter, oldBFilter, 
      oldNFilter, oldEFilter, oldCFilter                                     : STRING;
      displaySArray, displayBCArray, displayBArray, displayNArray, 
      displayEArray, displayCArray, displayHArray                            : FinalArrayType;
      ASK METHOD InitSelf(IN printing : BOOLEAN);
      ASK METHOD SpareSort   (IN sortCol   : INTEGER; IN sortDir : STRING);
      ASK METHOD CostSort    (IN sortCol   : INTEGER; IN sortDir : STRING);
      ASK METHOD BlockSort   (IN sortCol   : INTEGER; IN sortDir : STRING); 
      ASK METHOD NodeSort    (IN sortCol   : INTEGER; IN sortDir : STRING); 
      ASK METHOD EventSort   (IN sortCol   : INTEGER; IN sortDir : STRING); 
      ASK METHOD HierSort    (IN sortCol   : INTEGER; IN sortDir : STRING); 
      ASK METHOD CapSort     (IN sortCol   : INTEGER; IN sortDir : STRING);
      ASK METHOD SpareInsert (IN i, pos, numPlaced    : INTEGER);
      ASK METHOD CostInsert  (IN i, pos, numPlaced    : INTEGER);
      ASK METHOD BlockInsert (IN i, k, pos, numPlaced    : INTEGER); 
      ASK METHOD NodeInsert  (IN i, k, pos, numPlaced    : INTEGER); 
      ASK METHOD EventInsert (IN i, k, pos, numPlaced    : INTEGER);
      ASK METHOD HierInsert  (IN i, k, pos, numPlaced    : INTEGER);
      ASK METHOD CapInsert   (IN i, k, pos, numPlaced    : INTEGER);  
      ASK METHOD FillResultsTable(IN printing : BOOLEAN);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {TablesOutObj}
   
   
   TrigBoxObj = OBJECT(DialogBoxObj);
     trigComboBox                                       : ComboBoxObj;
     applyButton, deleteButton, helpButton,
     cancelButton, okButton                             : ButtonObj;
     freqTable                                          : TableObj;
     unitsLabel1, unitsLabel2, freqLabel                : LabelObj;
     staggerValBox                                      : ValueBoxObj;
     repChkBox                                          : CheckBoxObj;
     trigDistType, tempDist                              : INTEGER;
     trigDistParams, tempParams                          : realArray;
     tempStagger                                            : REAL;
     tempRep                                                : BOOLEAN;
         
    

     ASK METHOD ReceiveData;
     OVERRIDE
       ASK METHOD BeSelected;
   END OBJECT; {TriggerBoxObj}

   DistBoxObj = OBJECT(HelpBoxObj);
     distComboBox                                       : ComboBoxObj;
     meanLabel, unitsLabel1, stdevLabel, unitsLabel2,
     unitsLabel3, unitsLabel4, parametersLabel          : LabelObj;
     paramLabels, paramUnits                            : ARRAY INTEGER OF LabelObj;
     paramVals                                          : ARRAY INTEGER OF ValueBoxObj;
     helpButton, cancelButton, refreshButton, okButton  : ButtonObj;
     oldDist                                            : INTEGER;
     oldParams                                          : realArray;
        
     ASK METHOD ReceiveData(INOUT distType                    : INTEGER;
                            INOUT distParams                  : realArray);

     OVERRIDE
       ASK METHOD BeSelected;
   END OBJECT; {DistBoxObj}    
   
   SparePoolsBoxObj = OBJECT(DialogBoxObj);
      spareCombo                                : ComboBoxObj;
      timeLabel1, timeLabel2, newText, emerText,
      initLabel, stockTxt1, stockTxt2, 
      stockTxt3, timeLabel3                     : LabelObj;
      newAmt, newArrival, emerVal, initVal,
      createCost, useCost, stockVal, stockAmt, 
      stockRate                                 : ValueBoxObj;
      newChk, emerChk, stockChk                 : CheckBoxObj;
      button                                    : ButtonObj;
      cancelled                                 : BOOLEAN;
     
      ASK METHOD ReceiveData;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {SparePoolsBoxObj}

   ResPoolsBoxObj = OBJECT(DialogBoxObj);
      resCombo                                  : ComboBoxObj;
      unitLabel                                 : LabelObj;
      initVal, createCost, fixedCost, useCost   : ValueBoxObj;
      button                                    : ButtonObj;
      cancelled                                 : BOOLEAN;
     
      ASK METHOD ReceiveData;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {ResPoolsBoxObj}
   
   TreeBoxObj = OBJECT(DialogBoxObj);
      ASK METHOD ReceiveData;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT;

   FailEffectsBoxObj = OBJECT(DialogBoxObj);
      ASK METHOD ReceiveData;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT;

   GotoHierBoxObj = OBJECT(DialogBoxObj);
      ASK METHOD ReceiveData;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT;

   FailRepairBoxObj = OBJECT(DialogBoxObj);
      failOrRepair                                : STRING;
      failRepairComboBox                          : ComboBoxObj;
      ASK METHOD ReceiveData(IN failOrRep : STRING);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT;

   PROCEDURE CallHelp        (IN  helpCode                     : INTEGER);
   PROCEDURE ConvertToString (IN  distroNum                    : INTEGER;
                              OUT distroName                   : STRING);
   PROCEDURE GetParameters   (IN distroType                    : INTEGER;
                              IN typeOfDistro                  : STRING;
                              INOUT params, xUnits             : strArray;
                              INOUT xOffset                    : realArray);
   PROCEDURE SendAlert(IN message : TextBufferType;
                       IN cancel, yesNo, error  : BOOLEAN) : BOOLEAN;

   PROCEDURE GetTrigList(INOUT trigs : OptionListType);
   PROCEDURE GetSpareList(INOUT spares : OptionListType);
   PROCEDURE GetResList(INOUT res : OptionListType);
   PROCEDURE ChopZeros(IN num    : REAL; IN length : INTEGER) : STRING;
   PROCEDURE GetNumParams (IN  distroNum                    : INTEGER;
                              OUT nPars             : INTEGER);
   PROCEDURE ComputeStats(IN distro : INTEGER;
                          IN params : realArray;
                          OUT mean, stdev : REAL);
   PROCEDURE MakeDistString(IN distro : INTEGER;
                            IN params : realArray;
                            OUT distroString : STRING);
   PROCEDURE GammaFunction(IN x : REAL) : REAL;                       
   PROCEDURE HierFilter(IN tableType: STRING; INOUT table : TableObj; IN arr : FinalArrayType; 
                        IN arraySize : INTEGER; IN hierNameLoc : INTEGER; IN hierName : STRING;
                        IN printing : BOOLEAN; OUT displayArray : FinalArrayType; OUT hiercount : INTEGER);
                        

VAR
   poolsDone, trigsDone, 
   sparesDone, resDone,
   returnToRBD,threeWay,
   loadCanceled, doorOpen          : BOOLEAN;
   libBlockUp                      : STRING;
   pagesAcross                     : INTEGER;  {Kill}
   fEmpArray, rEmpArray            : realArray;
   blockProp                                 : BlockPropObj;
   blockUsesTrig                : BOOLEAN;
   pmTrigName : STRING;  
   hierComboBox : ComboBoxObj;
      translateArray                               : intArray;
                     
END MODULE. {dmod Intract}
