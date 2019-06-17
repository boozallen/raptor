

  ASK METHOD ReconfigureEvent (IN RunChange,FailChange            : INTEGER);  
     VAR
        tempNode                                  : RBDNodeObj;
     BEGIN
     IF ((hasDepObjects) AND (simInProgress))
        IF (FailChange=1)  
           BackFlowDependencies(Down);
        ELSIF (RunChange=1)
           BackFlowDependencies(AllUp);
        END IF;
     END IF;
     tempNode := ASK window Descendant("RBDNode", EFPAnode);                       
     ASK tempNode TO ReconfigureNode(RunChange,FailChange,0,0,0,EFPAlink);  
  END METHOD;                      

