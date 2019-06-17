{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Definition Module : DObjects                                                  +}
{+  Author            : Chuck Carter / Steve Brown / Tony Malerich                +}
{+  Last Modified     : May 2004                                                  +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

DEFINITION MODULE Objects;

FROM Image   IMPORT ImageObj;
FROM GTypes  IMPORT OptionListType;
FROM Fill    IMPORT PolygonObj;
FROM Intract IMPORT HelpBoxObj;
FROM GrpMod  IMPORT QueueObj;
FROM SimMod  IMPORT TriggerObj;
FROM Analyze IMPORT RandomGenObj;

TYPE
   directionType = (INTO, OUTOF, MINUSINTO, MINUSOUTOF);
   boolArray = ARRAY INTEGER OF BOOLEAN;
   intArray  = ARRAY INTEGER OF INTEGER;
   realArray = ARRAY INTEGER OF REAL;
   strArray  = ARRAY INTEGER OF STRING;
   BlockStatusType=(Running,Repairing,Done,Idle,Standby,Workspace,PM,PMhold,RepHold); 
   EventStatusType=(Armed,Success,Failure);                 
   NodeStatusType=(AllUp,NodeStandby,Degraded,NodeIdle,NodePM,Down);
   LinkStatusType=(LinkUp,LinkDown,LinkIdle,LinkStandby,LinkPM,LinkDone);
   ActiveStatusType=(Active,Cut,Linked);
   SparingType = (Infinite, Custom, SparePool, ColdPool, Resource, None);
                                     
   SparePoolObj = OBJECT;
      poolName                                                    : STRING;
      sparingType                                                 : SparingType;
      initialSpares,newSpares, SLOOrderLevel, SLONewSpares        : INTEGER;
      newSparesArrival,emergencyTime,fixedPerUse,spareCost,
      costPerTime,SLOTime,emerShippingCost                        : REAL;
      usesPhasing,stockLevelOrdering,routineSpareOrdering,
      emerSpareOrdering                                           : BOOLEAN;
      phase                                                       : intArray;                                  
      ASK METHOD SetData(IN name                                               : STRING;
                         IN initStock,RSONum,SLOlvl,SLOquan                    : INTEGER;
                         IN RSO,ESO,SLO                                        : BOOLEAN;
                         IN RSOArrivalTime,emerTime,SLOtime,spCost,emerShipCost: REAL);
      ASK METHOD SetResData(IN name                                 : STRING;
                            IN initSp                               : INTEGER;
                            IN init, fixed, perUse                  : REAL;
                            IN phases                               : BOOLEAN);
      ASK METHOD RemovePool(IN name : STRING;
                            IN type : SparingType);
   END OBJECT;{SparePoolObj}

   PhaseObj = OBJECT;
      phaseName                                                  : STRING;
      ID,Dist                                                    : INTEGER;
      Params                                                     : realArray;
      mission                                                    : BOOLEAN;
                            
      ASK METHOD SetPhaseData    (IN newString                   : STRING;
                                  IN newDist,newID               : INTEGER;
                                  IN newParams                   : realArray;
                                  IN newMission                  : BOOLEAN);
   END OBJECT;{PhaseObj}

   RBDBasicObj = OBJECT(ImageObj);
      activeStatus                                                    : ActiveStatusType;
      connectIntoNum,connectOutOfNum,copiedFromId,
      GoodPathsRequired,MinGoodPathsReq,NumGoodPaths,NumActivePaths,
      DependencyNum,simDependencyNum,EconnectIntoNum, 
      EconnectOutOfNum,parentID,seqNum                                : INTEGER;
      xPosition,yPosition,stateStartTime                              : REAL;
      Selected,usesPhasing,hasDepObjects,sysDepend,locDepend,coldDep,
      IgnoreDep                                                       : BOOLEAN;
      connectToIds                                                    : intArray;
      connectToRefs                                                   : ARRAY INTEGER OF STRING;
      comment,depType,name,Ao,Do,R                                    : STRING;
      ASK METHOD SetID        (IN    RBDObjName                       : STRING;
                               IN    RBDIdNum                         : INTEGER);
      ASK METHOD SetCopiedFromId (IN copiedFrom                       : INTEGER);
      ASK METHOD SetSelected  (IN isSelected                          : BOOLEAN);
      ASK METHOD ResetLinks   (IN totalLinks, activeBlock             : INTEGER;
                               IN xPos, yPos                          : REAL;
                               IN activeRef                           : STRING;
                               IN noRedraw,nowPasting                 : BOOLEAN);
      ASK METHOD BackFlowDependencies(IN changeType                   : NodeStatusType);
      ASK METHOD SetHasDepObjects (IN newSetting                      : BOOLEAN);
      ASK METHOD ResetStateStart;
      ASK METHOD CheckForDeps;
      ASK METHOD SetColdDep;
      ASK METHOD SetIgnoreDep (IN IgDepStatus                         : BOOLEAN);
      ASK METHOD SetDep       (IN depNum                              : INTEGER;
                               IN dType                               : STRING);                              
      ASK METHOD SetParentID  (IN newID                               : INTEGER); {eliz} 
      ASK METHOD SetEconnectIntoNum   (IN newValue                    : INTEGER);
      ASK METHOD SetEconnectOutOfNum  (IN newValue                    : INTEGER);
      ASK METHOD SetGoodPathsRequired (IN newValue                    : INTEGER); 
      ASK METHOD SetusesPhasing    (IN newBool                        : BOOLEAN);
      ASK METHOD SetComment        (IN newValue                       : STRING);
      ASK METHOD ResetConnectToInfo;
      ASK METHOD UpdateConnectToInfo(IN newLink,newElement            : INTEGER;
                                     IN conTRef                       : STRING);
      ASK METHOD SetSequenceNum(IN newValue                           : INTEGER);                               
      ASK METHOD IncLink       (IN direction                          : directionType);
      ASK METHOD SetAoDoR      (IN newAo,newDo,newR                   : STRING);     
      OVERRIDE
      ASK METHOD DisplayAt     (IN    x, y                            : REAL);

   END OBJECT; {RBDBasicObj}

   RBDHierObj = OBJECT(RBDBasicObj);      
      isConnectedNode                                                 : BOOLEAN;
      inID,outID,level,numCurrentlyFailed,dsiBlues,dsiYellows,
      dsiSiennas,dsiOranges,dsiReds,oldID,myDepth                     : INTEGER;
      childGroup                                                      : QueueObj;
      Status,dsiState,DSIsignal                                       : NodeStatusType;
      zoom,xCenter,yCenter,xOrigin,yOrigin                            : REAL;
      ASK METHOD SetName (IN newName                                  : STRING);                              
      ASK METHOD SetZoom (IN newZoom                                  : REAL);                              
      ASK METHOD SetLevel (IN newLevel                                : INTEGER);                              
      ASK METHOD SetConnectToNode (IN isConnected                     : BOOLEAN);
      ASK METHOD SetInID  (IN newValue                                : INTEGER);
      ASK METHOD SetOutID (IN newValue                                : INTEGER);
      ASK METHOD ChangeStatus (IN newcolor                            : NodeStatusType);
      ASK METHOD CreateChildGroup;
      ASK METHOD AugmentSelectGroup;
      ASK METHOD ChangeDSIStatus (IN gre,blu,yel,sie,ora,red          : INTEGER);
      ASK METHOD IncNumFailed;
      ASK METHOD DecNumFailed;
      ASK METHOD SetToInitValues;
      ASK METHOD CleanUp;
      ASK METHOD SetxCenter   (IN newValue                            : REAL);        
      ASK METHOD SetyCenter   (IN newValue                            : REAL);        
      ASK METHOD SetLocation  (IN xLoc, yLoc                          : REAL);
      ASK METHOD SetOrigin    (IN X, Y                                : REAL);
      ASK METHOD SetOldID     (IN newValue                            : INTEGER);
      ASK METHOD SetmyDepth   (IN newValue                            : INTEGER);
      ASK METHOD CalculateDepth (OUT depth : INTEGER);      
      ASK METHOD SetChildLevels;
      ASK METHOD MassEditHier(IN changeP, useP, changeDep, changeName : BOOLEAN; 
                              IN newDep                                  : INTEGER;
                              IN newDepType, nameChangeType, newName           : STRING;
                              OUT namemsg                                      : BOOLEAN); 
      END OBJECT; {RBDHierObj}

   RBDBlockObj = OBJECT(RBDBasicObj);      
      opStatus,oldStatus                                              : BlockStatusType;
      poolName,res1Name,startCond,pmTrig,InterruptReason              : STRING; 
      prevDepState,TzeroAction                                          : STRING;              {cmc 12/13/06}
      failDistro,numFailParams,failStream,repairDistro,numRepairParams,
      repairStream,initStock,newSpares,sparePoolNum,resPoolNum,
      FailureCounter,EFPAnode,EFPAlink,SLOOrderLevel,SLONewSpares,GDType,
      simStartType,numDiffRes,discardedFailures,numRes1,numRes1PM,
      pmDist,preDist,postDist                                         : INTEGER;
      sparingType                                                     : SparingType;
      arrivalRate,failMean,repairMean,failSTDev,repairSTDev,emerTime,
      initialCost,operatingCost,pmCost,pmHoldCost,repHoldCost,repairingCost,
      spareCost,idleCost,standbyCost,doneCost,repFixedCost,doneFixedCost,
      PhaseStress,TimeHack,OperatingTime,SLOTime,GDRate,GDLimit,
      emerShippingCost,amountExhausted,startVal,firstPreLDT,sbStress,
      pmStagger,pmFreq,pmFixedCost,FailTimeLeft,PMTimeLeft,DepNothingPerc,
      DepIdlePerc,DepPMPerc,DepFailPerc,DepPMThreshold                : REAL;
      isConnectedNode,infiniteSpares,AllowSwitching,RepWait,FailWait,
      PMWait,ZeroWait,AllowReconfigureSystem,stockLevelOrdering,
      routineSpareOrdering,emerSpareOrdering,alwaysAddDoneCost,usesPM,
      pmSpareNeeded,pmRefresh,deferPM,WaitingForPrereqs, pmMisDefer,
      pmFailReset,pmTriggered,pmReqDefer,spareObtained,skipPM,
      emerSpareOrdered,defDepStateIdle,returnFromMaint,FevDepMode,
      blockHasStarted: BOOLEAN;                                                        {cmc 12/13/06}
      failVals,repairVals,pmParams,preParams,postParams,phaseValue    : realArray;
      phaseType                                                       : strArray;
      ASK METHOD SetBlockData     (INOUT boolsArray                   : boolArray; 
                                   INOUT intsArray                    : intArray;
                                   INOUT realsArray                   : realArray; 
                                   INOUT strsArray                    : strArray;
                                   INOUT fVals, rVals,preVals,
                                         postVals,pmVals              : realArray;
                                   INOUT sparing                      : SparingType);
      ASK METHOD SetNewBlockParams(INOUT fVals, rVals                 : realArray);
      ASK METHOD GetCloneOf       (IN cloner                          : RBDBlockObj;
                                   IN copying                         : BOOLEAN);
      ASK METHOD SetConnectToNode (IN isConnected                     : BOOLEAN);
      ASK METHOD CopyRestOfData   (IN intoNum, outOfNum               : INTEGER;
                                   IN xPos, yPos                      : REAL);
      ASK METHOD SetPoolNum       (IN i                               : INTEGER;
                                   IN setSpare                        : BOOLEAN);
      ASK METHOD MassEditBlock    (IN changeSD,changeP,useP,changeRS,
                                      changeIS,changeNR, changeRes,
                                      changeName                      : BOOLEAN;
                                   IN newRS, sysNum                   : INTEGER;
                                   IN newDepType, nameChangeType, 
                                      newName                         : STRING;
                                   OUT namemsg                        : BOOLEAN); 
      ASK METHOD SetPhases        (IN phasing                         : BOOLEAN; 
                                   IN arrayR                          : realArray;
                                   IN arrayS                          : strArray);
      ASK METHOD ReconfigureBlock (IN RunChange,FailChange,IdleChange,
                                      StandbyChange,pmChange          : INTEGER);  
      ASK METHOD ReconfigureSystem(IN inPath                          : INTEGER);
      ASK METHOD ChangeBlockState (IN newOpStatus                     : BlockStatusType;
                                   IN newActiveStatus                 : ActiveStatusType;
                                   IN comment                         : STRING);      
      ASK METHOD Initialize;
      ASK METHOD ChangePhases     (IN oldVal,newVal                   : REAL;
                                   IN oldType,newType                 : STRING);
      ASK METHOD SetCopyBlockStreams;
      ASK METHOD SetSparingInfinite;
      ASK METHOD SetNoResources;
      ASK METHOD ResetForPasting;
      ASK METHOD SetStats;
      ASK METHOD SetEFPAvalues    (IN EFPAdownNode, EFPApath          : INTEGER);
      ASK METHOD GetStartCond; 
      ASK METHOD ResetStats;
      ASK METHOD SetToInitValues;
      ASK METHOD CleanUp;      
      ASK METHOD SetInitName             (IN newValue   : STRING);   
      ASK METHOD SetsimStartType         (IN newValue   : INTEGER);
      ASK METHOD SetamountExhausted      (IN newValue   : REAL);      
      ASK METHOD SetfailDistro           (IN newValue   : INTEGER);
      ASK METHOD SetnumFailParams        (IN newValue   : INTEGER);
      ASK METHOD SetfailVals             (IN newVals    : realArray);
      ASK METHOD SetfailStream           (IN newValue   : INTEGER);
      ASK METHOD SetrepairDistro         (IN newValue   : INTEGER);
      ASK METHOD SetnumRepairParams      (IN newValue   : INTEGER);
      ASK METHOD SetrepairVals           (IN newVals    : realArray);
      ASK METHOD SetrepairStream         (IN newValue   : INTEGER);
      ASK METHOD SetnumDiffRes           (IN newValue   : INTEGER);
      ASK METHOD SetnumRes1              (IN newValue   : INTEGER);
      ASK METHOD SetnumRes1PM            (IN newValue   : INTEGER);
      ASK METHOD Setres1Name             (IN newValue   : STRING);
      ASK METHOD SetinfiniteSpares       (IN newValue   : BOOLEAN);
      ASK METHOD SetsparingType          (IN newValue   : SparingType);
      ASK METHOD SetroutineSpareOrdering (IN newValue   : BOOLEAN);
      ASK METHOD SetinitStock            (IN newValue   : INTEGER);
      ASK METHOD SetnewSpares            (IN newValue   : INTEGER);
      ASK METHOD SetarrivalRate          (IN newValue   : REAL);      
      ASK METHOD SetemerSpareOrdering    (IN newValue   : BOOLEAN);
      ASK METHOD SetemerTime             (IN newValue   : REAL);      
      ASK METHOD SetstockLevelOrdering   (IN newValue   : BOOLEAN);
      ASK METHOD SetSLOOrderLevel        (IN newValue   : INTEGER);
      ASK METHOD SetSLONewSpares         (IN newValue   : INTEGER);
      ASK METHOD SetSLOTime              (IN newValue   : REAL);      
      ASK METHOD SetGDType               (IN newValue   : INTEGER); 
      ASK METHOD SetGDRate               (IN newValue   : REAL);      
      ASK METHOD SetGDLimit              (IN newValue   : REAL);      
      ASK METHOD SetpoolName             (IN newValue   : STRING);
      ASK METHOD SetinitialCost          (IN newValue   : REAL);      
      ASK METHOD SetOperatingCost        (IN newValue   : REAL);
      ASK METHOD SetstandbyCost          (IN newValue   : REAL);      
      ASK METHOD SetidleCost             (IN newValue   : REAL);    
      ASK METHOD SetrepHoldCost          (IN newValue   : REAL);      
      ASK METHOD SetpmHoldCost           (IN newValue   : REAL);      
      ASK METHOD SetpmFixedCost          (IN newValue   : REAL);      
      ASK METHOD SetrepairingCost        (IN newValue   : REAL);      
      ASK METHOD SetrepFixedCost         (IN newValue   : REAL);      
      ASK METHOD SetdoneCost             (IN newValue   : REAL);      
      ASK METHOD SetdoneFixedCost        (IN newValue   : REAL); 
      ASK METHOD SetspareCost            (IN newValue   : REAL);
      ASK METHOD SetemerShippingCost     (IN newValue   : REAL);      
      ASK METHOD SetalwaysAddDoneCost    (IN newValue   : BOOLEAN);     
      ASK METHOD SetSBstress             (IN newValue   : REAL);  
      ASK METHOD SetpreDist              (IN newValue   : INTEGER);   
      ASK METHOD SetpreParams            (IN newVals    : realArray);
      ASK METHOD SetpostDist             (IN newValue   : INTEGER);   
      ASK METHOD SetpostParams           (IN newVals    : realArray);
      ASK METHOD SetUsesPM               (IN newValue   : BOOLEAN);
      ASK METHOD SetpmSpare              (IN newValue   : BOOLEAN);
      ASK METHOD SetpmRefresh            (IN newValue   : BOOLEAN);
      ASK METHOD SetpmMisDefer           (IN newValue   : BOOLEAN);
      ASK METHOD SetpmFailReset          (IN newValue   : BOOLEAN);
      ASK METHOD SetpmReqDefer           (IN newValue   : BOOLEAN);
      ASK METHOD SetpmTriggered          (IN newValue   : BOOLEAN);
      ASK METHOD SetpmTrig               (IN newValue   : STRING);
      ASK METHOD SetpmFreq               (IN newValue   : REAL);
      ASK METHOD SetpmStagger            (IN newValue   : REAL);            
      ASK METHOD SetpmDist               (IN newValue   : INTEGER); 
      ASK METHOD SetpmParams             (IN newValue   : realArray);
      ASK METHOD SetpmCost               (IN newValue   : REAL);
      ASK METHOD SetInterruptReason      (IN newValue   : STRING);
      ASK METHOD SetDeferPM              (IN newValue   : BOOLEAN);
      ASK METHOD fevInitialize;
      ASK METHOD fevStartCond;
      ASK METHOD fevCleanUp;
      ASK METHOD fevReset;
      ASK METHOD SetopStatus             (IN newValue   : STRING);  
      ASK METHOD ChangeRunningColor      (IN newValue   : REAL);      
      ASK METHOD CheckForSpare           (OUT spareAvail,sparesOut  : BOOLEAN);
      ASK METHOD ShowAnalColor;
      ASK METHOD DetermineNewState       (IN previousState : STRING);
      ASK METHOD SetDepVals              (IN val1,val2,val3,val4,val5  : REAL;
                                          IN bool1                     : BOOLEAN);
      ASK METHOD SetreturnFromMaint      (IN newValue   : BOOLEAN);
      ASK METHOD SetFevDepMode           (IN newValue   : BOOLEAN);
      ASK METHOD TurnOffStartCond;
      ASK METHOD SetDefaultBlockName     (IN newValue   : STRING);
      ASK METHOD SetPrevDepState         (IN newValue   : STRING);
      ASK METHOD SetTzeroAction         (IN newValue   : STRING);     {cmc 12/13/06}
      TELL METHOD Run; 
      TELL METHOD Repair; 
      TELL METHOD PerformPM; 
      TELL METHOD ObtainPMprereqs;
      TELL METHOD GenerateSpares;
      TELL METHOD OrderASpare;  
      TELL METHOD OrderStock             (IN quantity   :INTEGER);  
      TELL METHOD ControlPMtriggers;
      TELL METHOD InitTimeZeroDeps;                                     {cmc 12/13/06}

   END OBJECT; {RBDBlockObj}

   RBDEventObj = OBJECT(RBDBasicObj);      
      opStatus,oldStatus                                              : EventStatusType;
      failDistro,failStream,FailureCounter,EFPAnode,EFPAlink,
      discardedFailures                                               : INTEGER;
      failMean,failSTDev,initialCost,operatingCost,repairingCost      : REAL;
      isConnectedNode,AllowReconfigureSystem                          : BOOLEAN;
      failVals,phaseValue                                             : realArray;
      phaseType                                                       : strArray;
      ASK METHOD SetInitName         (IN newValue                     : STRING);
      ASK METHOD SetEventData        (INOUT usesPhase                 : BOOLEAN; 
                                      INOUT fDist,fStrm               : INTEGER;
                                      INOUT opCost,repCost,initCost,fVal : REAL; 
                                      INOUT eName,comm                : STRING);
      ASK METHOD SetNewEventParams   (INOUT fVal                      : REAL);
      ASK METHOD GetCloneOf       (IN cloner                          : RBDEventObj;
                                   IN copying                         : BOOLEAN);
      ASK METHOD SetConnectToNode (IN isConnected                     : BOOLEAN);
      ASK METHOD CopyRestOfData   (IN intoNum, outOfNum               : INTEGER;
                                   IN xPos, yPos                      : REAL);
      ASK METHOD MassEditEvent    (IN changeSP,changeP,useP,changeRS,
                                      changeName                      : BOOLEAN;
                                   IN newRS                           : INTEGER;
                                   IN newSP                           : REAL;
                                   IN nameChangeType, newName         : STRING;
                                   OUT namemsg                        : BOOLEAN);
      ASK METHOD SetPhases        (IN phasing                         : BOOLEAN; 
                                   IN arrayR                          : realArray;
                                   IN arrayS                          : strArray);
      ASK METHOD ReconfigureEvent (IN RunChange,FailChange            : INTEGER);  
      ASK METHOD ReconfigureSystem(IN inPath                          : INTEGER);
      ASK METHOD ChangeEventState (IN newOpStatus                     : EventStatusType;
                                   IN newActiveStatus                 : ActiveStatusType;
                                   IN comment                         : STRING);      
      ASK METHOD Initialize;
      ASK METHOD ChangePhases     (IN oldVal,newVal                   : REAL;
                                   IN oldType,newType                 : STRING);
      ASK METHOD SetEventDefs;
      ASK METHOD SetCopyEventStream;
      ASK METHOD ResetForPasting;
      ASK METHOD SetEFPAvalues    (IN EFPAdownNode, EFPApath          : INTEGER);
      ASK METHOD ConductEvent;  
      ASK METHOD ResetStats;
      ASK METHOD SetToInitValues;
      ASK METHOD CleanUp;      
      ASK METHOD SetfailDistro           (IN newValue   : INTEGER);
      ASK METHOD SetfailVals             (IN newVal     : REAL);
      ASK METHOD SetfailStream           (IN newValue   : INTEGER);
      ASK METHOD SetinitialCost          (IN newValue   : REAL);      
      ASK METHOD SetOperatingCost        (IN newValue   : REAL);
      ASK METHOD SetrepairingCost        (IN newValue   : REAL);      
      ASK METHOD fevInitialize;
      ASK METHOD fevStartCond;
      ASK METHOD fevCleanUp;
      ASK METHOD fevReset;
      ASK METHOD SetopStatus             (IN newValue   : STRING);  
      ASK METHOD ShowAnalColor;
      TELL METHOD InitTimeZeroDeps;                                        {cmc 12/13/06}

   END OBJECT; {RBDEventObj}

   RBDNodeObj = OBJECT(RBDBasicObj);
      nodeName                                                       : STRING;
      typeNode,goodPaths,EFPAConnectOut,CapConnectOut,nodeFlow, 
      NRBDConnectOut,KStar                                           : INTEGER; 
      EFPAConnectTo,EFPAConnectPath,inPathsArray,outPathsArray,
      CapConnectTo,CapConnectPath,pathFailValue,pathIdleValue,
      pathStandbyValue,pathRunValue,pathMaxValue,pathPMValue,
      coldLinks,phase,NRBDConnectTo,NRBDConnectPath  {CAP}           : intArray;
      goBack,coldStandby,priorityReturn,checkAutosFirst,
      reportNodeAnal,fullFlow,anyPath,higherCold,trialSuccess, 
      EFPAtested,finished,capNode, capTested                         : BOOLEAN;
      Status                                                         : NodeStatusType;
      ASK METHOD SetName      (IN newName                            : STRING);
      ASK METHOD Init1        (IN intsArray                          : intArray);
      ASK METHOD SetType      (IN nodeType                           : INTEGER);
      ASK METHOD SetGoodPaths (IN k                                  : INTEGER);
      ASK METHOD SetNum       (IN label                              : INTEGER);
      ASK METHOD SetKofN      (IN k, n                               : INTEGER);
      ASK METHOD SetLocation  (IN xLoc, yLoc                         : REAL);
      ASK METHOD SetGoBack    (IN back                               : BOOLEAN);
      ASK METHOD MassEditNode (IN changeP, useP, changeK, changeDep,
                                  changeName                         : BOOLEAN;
                               IN newK, newDep                       : INTEGER;
                               IN newDepType, nameChangeType, newName: STRING;
                               OUT namemsg                           : BOOLEAN); 
      ASK METHOD SetPhases    (IN phasing                            : BOOLEAN; 
                               IN array                              : intArray);
      ASK METHOD ChangeNodeState (IN newcolor                        : NodeStatusType;
                                  IN newActiveStatus                 : ActiveStatusType);
      ASK METHOD ChangePhases (IN currentPhase,newPhase              : INTEGER);
      ASK METHOD ReconfigureNode(IN RunChange,FailChange,IdleChange,
                                    StandbyChange,pmChange,inPath    : INTEGER);  
      ASK METHOD ReconfigureSystem(IN inPath                         : INTEGER);
      ASK METHOD ShowAnalColor;
      ASK METHOD ShowFlow;
      ASK METHOD Initialize;
      ASK METHOD SetToInitValues;
      ASK METHOD SetNRBDvalues(IN NRBDOutNum                         : INTEGER;
                               IN NRBDDownstreamArray, NRBDPathArray : intArray);   {CAP}
      ASK METHOD SetEFPAvalues(IN EFPAOutNum                         : INTEGER;
                               IN EFPADownstreamArray, EFPAPathArray : intArray);
      ASK METHOD SetCapValues (IN EFPAOutNum                         : INTEGER;
                               IN EFPADownstreamArray, EFPAPathArray : intArray);
      ASK METHOD SetReportNodeAnal(IN newSetting                     : BOOLEAN);
      ASK METHOD SetFullFlow      (IN newSetting                     : BOOLEAN);
      ASK METHOD SetAnyPath       (IN newSetting                     : BOOLEAN);      
      ASK METHOD SetHigherCold    (IN newSetting                     : BOOLEAN);      
      ASK METHOD SetTrialSuccess  (IN newSetting                     : BOOLEAN);
      ASK METHOD SetEFPAtested    (IN newSetting                     : BOOLEAN);
      ASK METHOD SetCapNode       (IN newSetting                     : BOOLEAN);
      ASK METHOD SetCapTested     (IN newSetting                     : BOOLEAN);
      ASK METHOD CleanUp;
      ASK METHOD IdAndSortColds;
      ASK METHOD ReOrderColds(IN lastLink                           : INTEGER);
      ASK METHOD SortCapacityArrays;
      ASK METHOD RunColdLogic;
      ASK METHOD DistributeFlow (IN   sent                          : INTEGER; 
                                 OUT  rejected                      : INTEGER);
      ASK METHOD ResetNodeFlow;                                 
      ASK METHOD UncheckCold;
      ASK METHOD TestFail       (IN mama                            : INTEGER);
      ASK METHOD SetNumGoodPaths (IN numGood                        : INTEGER);
      ASK METHOD DisposeSimVars;
      ASK METHOD fevInit;
      ASK METHOD FindCapPaths(IN  priority                          : INTEGER);
      ASK METHOD SetKStar      (IN kStar                            : INTEGER);
      ASK METHOD ResetLocalPathArrays;
      ASK METHOD UpdateOutPathsArray(IN newLink                     : INTEGER);
      ASK METHOD UpdateInPathsArray(IN newLink                      : INTEGER);
      ASK METHOD GetDownNum(IN   inPath                             : INTEGER; 
                            OUT  downNum                            : INTEGER);
      TELL METHOD InitTimeZeroDeps;                                               {cmc 12/13/06}
                      

   END OBJECT; {RBDNodeObj}
   
   LinkObj = OBJECT(PolygonObj);
      connectFRef, connectTRef, EconnectFRef, EconnectTRef,
      switchStatus                                                    : STRING;
      connectFromId,connectToId,objectNum,coldPriority,
      capPriority,maxFlow,nomFlow,actualFlow,depLink,availFlow,
      simNomFlow,simMaxFlow,simCapPriority,sendAmount,EconnectToId, 
      EconnectFromId, parentID{eliz}                                 : INTEGER;
      Selected,hasSiblings,flowing,switchCompleted,
      nomReached,maxReached,pathFull,stuckOn,active,SBlink           : BOOLEAN;
      Status                                                         : LinkStatusType;
      autoSwitchProb,autoSwitchTime,manualSwitchTime,startStateTime,
      switchTime                                                     : REAL;
      siblingsGroup                                                  : QueueObj;
      ASK METHOD LinkInit      (IN intsArray                         : intArray;
                                IN realsArray                        : realArray);
      ASK METHOD SetID         (IN  RBDObjName                       : STRING;
                                IN  RBDObjNum                        : INTEGER);
      ASK METHOD SetSelected   (IN  isSelected                       : BOOLEAN);
      ASK METHOD SetConnections(IN  fromConnect, toConnect           : INTEGER;
                                IN  fromConRef, toConRef             : STRING);
      ASK METHOD CheckValidLink(IN  objectToCheck                    : ImageObj;
                                OUT isValid                          : BOOLEAN;
                                OUT errorMessage                     : STRING;
                                IN  linkdirection                    : directionType);
      ASK METHOD ChangeLinkStatus(IN newStatus                       : LinkStatusType);
      ASK METHOD ChangeRoots   (IN draggin                           : BOOLEAN;
                                IN bRoot                             : ImageObj);
      ASK METHOD SetToInitValues;
      ASK METHOD SetActualFlow (IN newValue                          : INTEGER);
      ASK METHOD SetColdVals   (IN priority                          : INTEGER;
                                IN autoProb,autoTime,manTime         : REAL);
      ASK METHOD AddFamilyMember(IN sibling                          : LinkObj);
      ASK METHOD ShowFlowStatus;
      ASK METHOD SetDepLink    (IN downLink                          : INTEGER);
      ASK METHOD CleanUp;
      ASK METHOD SetParentID   (IN newID                             : INTEGER); {eliz} 
      ASK METHOD SetStuckOn    (IN stuckStatus                       : BOOLEAN);
      ASK METHOD SetPathFull   (IN fullStatus                        : BOOLEAN);
      ASK METHOD SetAvailFlow  (IN amount                            : INTEGER);
{stv} ASK METHOD SetFlowInVals (IN nom,max                           : INTEGER);
      ASK METHOD SetSimFlowVals (IN nom,max                          : INTEGER);
      ASK METHOD SetSimCapPriority (IN newSetting                    : INTEGER);
{stv} ASK METHOD SetCapPriority(IN priority                          : INTEGER);  
      ASK METHOD SetSendAmount(IN newValue                           : INTEGER);
      ASK METHOD SetEconnectToId    (IN newValue                    : INTEGER);
      ASK METHOD SetEconnectFromId  (IN newValue                    : INTEGER);
      ASK METHOD SetEconnectTRef(IN toConRef                         : STRING);
      ASK METHOD SetEconnectFRef(IN fromConRef                       : STRING);
      ASK METHOD SetActiveLink(IN newVal                             : BOOLEAN);
      ASK METHOD SetSBlink    (IN newVal                             : BOOLEAN);   
      ASK METHOD SetSwitchTime; 
      ASK METHOD PrepForSwitch;
      TELL METHOD PerformSwitch;
   
   END OBJECT; {LinkObj}

   RapTriggerObj = OBJECT(TriggerObj);
      TrigName                                                      : STRING;
      IDnum,TrigDist                                                : INTEGER;
      Repeats                                                       : BOOLEAN;
      TrigParams                                                    : realArray;
      InitUsed                                                      : REAL;
      ASK METHOD SetTrigData        (IN newString                   : STRING;
                                     IN newDist                     : INTEGER;
                                     IN newParams                   : realArray;
                                     IN newRepeats                  : BOOLEAN;
                                     IN newUsed                     : REAL);
      ASK METHOD RemoveTrig         (IN name                        : STRING);
      TELL METHOD PullRapTrigger;
   END OBJECT; {RapTriggerObj}
   
   BogusRBD = OBJECT;
      TELL METHOD Run;
   END OBJECT;   
   
   PROCEDURE arrowhead (IN  connectInType                : STRING;
                        IN  startX, startY, endX, endY   : REAL;
                        OUT p1X, p1Y, p2X, p2Y, p3X, p3Y : REAL);
   PROCEDURE AnalyzeSystem;
   PROCEDURE CheckColds;
   PROCEDURE InitFactorySettings;
   PROCEDURE DetermineMaxCapacity(OUT maxCap : INTEGER);   
   PROCEDURE WriteEvents    (IN time                    : REAL;
                             IN typeString,nameString,
                            actionString,commentString  : STRING);
   PROCEDURE GetStream      (IN usesStream              : INTEGER;
                             IN typeString              : STRING;
                             IN idStream                : INTEGER;
                             OUT sysStream              : RandomGenObj);
   PROCEDURE RunBogusRBD;   
   PROCEDURE ResendBackFlow;


   VAR
      resetting,SystemStateChange,
      FlowingAtMaxCap,CapAnalysisRequired   : BOOLEAN;
      negExpBox                             : HelpBoxObj; 
      poolGroup,triggerGroup                : QueueObj;
      spareList,coldList,resList,trigList   : OptionListType;
      totalSpares,totalCold,totalRes        : INTEGER;      
      factBools                             : boolArray;
      factInts                              : intArray;
      factStrs                              : strArray;
      factReals,factFparams,factRparams,factPreParams,
      factPostParams,factPMParams           : realArray;
      factSparing                           : SparingType;
      randStream                            : RandomGenObj;
      critFactor                            : REAL;
      deferTrig                             : STRING;

   
END MODULE. {dmod RBDBlock}
