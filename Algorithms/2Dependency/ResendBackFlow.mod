

PROCEDURE ResendBackFlow;  
VAR
     tempBlock : RBDBlockObj;
     tempNode  : RBDNodeObj;
     tempEvent : RBDEventObj;
BEGIN
    {asdf}
   specialBackFlow:=TRUE;
   FOREACH tempBlock IN blockHasDepsGroup
      IF tempBlock.activeStatus=Cut
         ASK tempBlock TO BackFlowDependencies(Down);
      ELSIF  tempBlock.activeStatus=Active
         IF ((tempBlock.opStatus=Repairing) OR (tempBlock.opStatus=PM)  
             OR (tempBlock.opStatus=RepHold) OR (tempBlock.opStatus=PMhold))
            ASK tempBlock TO BackFlowDependencies(Down);
         END IF;
      END IF; 
   END FOREACH;
   FOREACH tempEvent IN eventHasDepsGroup
      IF tempEvent.activeStatus=Cut
            ASK tempEvent TO BackFlowDependencies(Down);      
      ELSIF tempEvent.activeStatus=Active   
         IF (tempEvent.opStatus=Failure)
            ASK tempEvent TO BackFlowDependencies(Down);
         END IF;
      END IF;
   END FOREACH;
   FOREACH tempNode IN nodeHasDepsGroup
      IF (tempNode.Status=Down)
         ASK tempNode TO BackFlowDependencies(Down);
      END IF;
   END FOREACH;
   specialBackFlow:=FALSE;

END PROCEDURE;     {ResendBackFlow}

