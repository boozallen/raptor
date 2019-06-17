{++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Definition Module : FileFx                                                    +}
{+  Author            : Steve Brown/Chuck Carter/Tony Malerich                    +}
{+  Last Modified     : September 04                                              +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

DEFINITION MODULE FileFx;

FROM Objects  IMPORT realArray,intArray,boolArray,strArray;
FROM IOMod    IMPORT StreamObj;

PROCEDURE OpenFile    (IN    startOpen,append                              : BOOLEAN;
                       INOUT totalBlocks,totalNodes,totalLinks,totalHiers,
                             totalEvents                                   : INTEGER;
                       IN    appendingLevel                                : INTEGER;      
                       INOUT gridIsOn, fileIsOpen                          : BOOLEAN;
                       INOUT nameOfFile, pathName, filter                  : STRING;
                       OUT   nixed                                         : BOOLEAN);
PROCEDURE GetFileName (OUT   FileName, PathName                            : STRING;
                       IN    filter, titleText                             : STRING);
PROCEDURE ReadData    (IN    dataFile                                      : STRING;
                       OUT numDataPoints                                   : INTEGER;
                       OUT tempArray                                       : realArray);
PROCEDURE NewFFile    (IN    gridIsOn                                      : BOOLEAN;
                       INOUT fileIsOpen                                    : BOOLEAN;
                       INOUT nameOfFile, pathName, filter                  : STRING;
                       INOUT totalBlocks,totalNodes,totalLinks,totalHiers,
                             totalEvents                                   : INTEGER;
                       OUT   nixed                                         : BOOLEAN);
PROCEDURE SaveFile    (IN    fileName, pathName, filter                    : STRING;
                       IN    totalBlocks,totalNodes,totalLinks,totalHiers,
                             totalEvents                                   : INTEGER);
PROCEDURE SaveAsFile  (OUT   nameOfFile, pathName                          : STRING;
                       IN    filter, title                                 : STRING);
PROCEDURE CloseFFile  (INOUT gridIsOn, fileIsOpen, saveCancelled           : BOOLEAN;
                       INOUT nameOfFile, pathName, filter                  : STRING;
                       INOUT totalBlocks,totalNodes,totalLinks,totalHiers,
                             totalEvents                                   : INTEGER);
PROCEDURE AskForSave  (INOUT saveCancelled, fileIsOpen                     : BOOLEAN;
                       INOUT nameOfFile, pathName, filter                  : STRING;
                       IN    totalBlocks,totalNodes,totalLinks,totalHiers,
                             totalEvents                                   : INTEGER);
PROCEDURE OpenDefFile (IN    defFileName, defPath                          : STRING;
                       INOUT ioResult                                      : BOOLEAN);
PROCEDURE SavePathsCFG (IN   defPath                                       : STRING);                         
PROCEDURE OpenPathsCFG (IN   defPath,exampPath                             : STRING);
PROCEDURE UpdateVersion (IN totalBlocks,totalNodes                         : INTEGER);
PROCEDURE AutoAddStartEnd (IN start,end                                    : BOOLEAN);
PROCEDURE ReadWriteCR   (IN action,inString                                : STRING) : STRING;  
PROCEDURE ErrorOpeningFile (INOUT totBs,totNs,totLs,totHs,totEs,existBs,
                               existNs,existLs,existHs,existEs,existTs,
                               numResPools                                 : INTEGER;
                         INOUT saveFile                                    : StreamObj;      
                         INOUT fileIsOpen                                  : BOOLEAN;
                         IN    saveCancelled,append                        : BOOLEAN;
                         OUT   nixed                                       : BOOLEAN);
PROCEDURE CleanArrays   (INOUT posArray,realsArray                         : realArray;
                         INOUT idsArray,intsArray                          : intArray;
                         INOUT boolsArray                                  : boolArray;
                         INOUT stringArray                                 : strArray);
                     
   
VAR
   menuPath,menuFile,versionStr                          : STRING;
   rapVersion                                            : INTEGER;
   loadingFile,ProtectedFile                             : BOOLEAN;


END MODULE. {dmod File_Fx}

