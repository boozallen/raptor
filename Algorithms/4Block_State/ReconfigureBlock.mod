

   ASK METHOD ReconfigureBlock(IN RunChange,FailChange,IdleChange,StandbyChange,pmChange: INTEGER);  
    VAR
        tempBlock                                 : RBDBlockObj;
        tempNode                                  : RBDNodeObj;
    BEGIN
    IF ((hasDepObjects) AND (simInProgress))
       IF    ((FailChange=1) OR (pmChange=1))  {block just failed or was cut}
                  BackFlowDependencies(Down);
       ELSIF (StandbyChange=1)
                  BackFlowDependencies(NodeStandby);
       ELSIF ((RunChange=1) AND ((FailChange=-1) OR (StandbyChange=-1) OR (pmChange=-1)))
                  BackFlowDependencies(AllUp);
       ELSIF ((IdleChange=1) AND ((FailChange=-1) OR (StandbyChange=-1) OR (pmChange=1)))
                  BackFlowDependencies(AllUp);
       END IF;
    END IF;
    tempNode := ASK window Descendant("RBDNode", EFPAnode);                       
    ASK tempNode TO ReconfigureNode(RunChange,FailChange,IdleChange,StandbyChange,pmChange,EFPAlink);  
   END METHOD;                      
     
    ASK METHOD ReconfigureSystem(IN inPath: INTEGER);
     VAR
        tempnode                      : RBDNodeObj;
     BEGIN
        NumActivePaths:=NumActivePaths-1; 
        tempnode := ASK window Descendant("RBDNode", EFPAnode);              
        ASK tempnode TO ReconfigureSystem(EFPAlink);        
  END METHOD;      {ReconfigureSystem}     

