{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Definition Module : Menubar                                                   +}
{+  Author            : Steve Brown                                               +}
{+  Last Modified     : 3 August 00                                               +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

DEFINITION MODULE Menubar;

FROM GPalet  IMPORT PaletteObj, PaletteButtonObj;
FROM Menu    IMPORT MenuBarObj;
FROM GTypes  IMPORT OptionListType;
FROM IOMod   IMPORT StreamObj;

TYPE
   graphMenuObj = OBJECT(MenuBarObj);
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {graphMenuObj}

   simMenuObj = OBJECT(MenuBarObj);
      ASK METHOD SetNavigation;
      ASK METHOD ChangeState(IN state : STRING);
      ASK METHOD KillSim;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {simMenuObj}

   simToolObj = OBJECT(PaletteObj);
      ASK METHOD SetNavigation;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {simToolObj}
   
   weakMenuObj = OBJECT(MenuBarObj);
      ASK METHOD SetNavigation;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {simMenuObj}

   weakToolObj = OBJECT(PaletteObj);
      ASK METHOD SetNavigation;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {simToolObj}
   
   fevMenuObj = OBJECT(MenuBarObj);
      ASK METHOD SetNavigation;
      ASK METHOD Initialize;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {graphMenuObj}

   fevToolObj = OBJECT(PaletteObj);
      ASK METHOD SetNavigation;
      ASK METHOD Initialize;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {simToolObj}
   
   mainMenuObj = OBJECT(MenuBarObj);
      path1, file1, path2, file2, path3, file3, 
      path4, file4, path5, file5, path6, file6 : STRING;
      ASK METHOD Disable       (IN greyLevel : INTEGER);
      ASK METHOD Disable1Thru8;
      ASK METHOD Enable        (IN greyLevel : INTEGER);
      ASK METHOD Enable2Thru5;
      ASK METHOD Enable2Thru6;
      ASK METHOD SetChecks;
      ASK METHOD SetZoomButts;
      ASK METHOD AddFileName   (IN path, file      : STRING);
      ASK METHOD SetFileNames  (IN defPath1, defFile1, defPath2, defFile2,
                                   defPath3, defFile3, defPath4, defFile4,
                                   defPath5, defFile5, defPath6, defFile6  : STRING);
      ASK METHOD SetNavigation;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {MenuBarObj}

   mainToolObj = OBJECT(PaletteObj);
      ASK METHOD SetNavigation;
      OVERRIDE
         ASK METHOD BeSelected;
   END OBJECT; {MenuToolObj}

   PROCEDURE UpOneLevel;
   PROCEDURE Home;
   PROCEDURE FindItem;
   PROCEDURE MassEdit;
   PROCEDURE PauseSim;
   PROCEDURE ResumeSim;
   PROCEDURE ColorPrefs;
   PROCEDURE ViewOutputs;
   PROCEDURE DisplayFailureEffects;
   PROCEDURE GotoHier;
   PROCEDURE Step;
   PROCEDURE Jump;
   PROCEDURE ResetNewFile;
   PROCEDURE ReturnToWorkspace;

   VAR
      goToNext, inStepMode, inJumpMode, inEndState, notPaused : BOOLEAN;
      nameArray                        : OptionListType;
      libArray                         : ARRAY INTEGER, INTEGER OF STRING;
      FEVStream                        : StreamObj; 
      libVersion : INTEGER;
END MODULE. {dmod MenuBar}
