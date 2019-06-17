MAIN MODULE Raptor7;
{+++++++++++++++++++++++++++++++++++RAPTOR+++++++++++++++++++++++++++++++++++++++++}
{+                                                                                +}
{+  Main Module   : RaptorX                                                       +}
{+  Author        : C. Carter                                                     +}
{+  Last Modified : June 2004                                                     +}
{+  Description   : The only purpose of main is to start the program and then     +}
{+                  chuck control over to the menubar (or mouseclick)             +}
{+                                                                                +}
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}


FROM Menu    IMPORT MenuItemObj;
FROM Display IMPORT InitDisplay,menubar,CheckTimeBomb,password,compileType,training;

VAR
   item : MenuItemObj;
   exploded : BOOLEAN;
   
BEGIN
   InitDisplay(exploded);
   IF NOT password
      IF ((compileType="demo") OR (compileType="student") OR training)
         CheckTimeBomb(exploded);
      END IF;
   END IF;   
   IF NOT exploded     
      REPEAT
         item := ASK menubar TO AcceptInput();
      UNTIL (ASK item ReferenceName = "ExitItem");
   END IF; 
END MODULE.


