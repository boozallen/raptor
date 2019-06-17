

  ASK METHOD ChangeDSIStatus(IN gre,blu,yel,sie,ora,red         : INTEGER);
  VAR
     dsi                                   : ImageObj;
     tempHier                              : RBDHierObj;
     newState,oldState, oldDSIsignal       : NodeStatusType;
     node                                  : RBDNodeObj;
  BEGIN
     oldState:=dsiState;
     dsiBlues:=dsiBlues+blu;
     dsiYellows:=dsiYellows+yel;
     dsiSiennas:=dsiSiennas+sie;
     dsiOranges:=dsiOranges+ora;
     dsiReds:=dsiReds+red;  
     IF dsiReds>0
        newState:=Down;
     ELSIF dsiOranges>0;
        newState:=NodePM;
     ELSIF dsiSiennas>0;
        newState:=NodeIdle;
     ELSIF dsiYellows>0;
        newState:=Degraded;
     ELSIF dsiBlues>0;
        newState:=NodeStandby;
     ELSE;
        newState:=AllUp;
     END IF;     
     dsi:= Descendant("HierMid", 0);
     node:=ASK root Child("RBDNode",outID);
     IF (node.activeStatus=Linked)    
        ASK dsi TO SetColor(White);
     ELSIF (node.activeStatus=Cut)    
        ASK dsi TO SetColor(Black);
     ELSE
        CASE newState
           WHEN AllUp:
              ASK dsi TO SetColor(LimeGreen);
           WHEN Degraded:
              ASK dsi TO SetColor(Yellow);
           WHEN NodeIdle:
              ASK dsi TO SetColor(Sienna);
           WHEN NodeStandby:
              ASK dsi TO SetColor(Blue);
           WHEN Down:
              ASK dsi TO SetColor(Red);
           WHEN NodePM:
              ASK dsi TO SetColor(Orange);
           OTHERWISE 
              NEW(message,1..1);
              message[1]:="Unknown DSI status color!     "; 
              result:=SendAlert(message,FALSE, FALSE, TRUE);
              DISPOSE(message);   
        END CASE;
     END IF;
     Draw; 
     IF newState<>oldState
        dsiState:=newState;  
        IF (parentID > 0)
           oldDSIsignal:=DSIsignal;
           IF (Status > dsiState)     {own color worse than descendents}
              DSIsignal:=Status;
           ELSE
              DSIsignal:=dsiState;
           END IF;
           IF DSIsignal<>oldDSIsignal; 
              gre:=0;
              yel:=0;
              sie:=0;
              blu:=0;
              red:=0;
              ora:=0;
              CASE oldDSIsignal
                 WHEN AllUp:
                    gre:=-1;
                 WHEN Degraded:
                    yel:=-1;
                 WHEN NodeIdle:
                    sie:=-1;
                 WHEN NodeStandby:
                    blu:=-1;
                 WHEN Down:
                    red:=-1;
                 WHEN NodePM:
                    ora:=-1;
                 OTHERWISE 
                    NEW(message,1..1);
                    message[1]:="Unknown hierarchy status color!     "; 
                    result:=SendAlert(message,FALSE, FALSE, TRUE);
                    DISPOSE(message);   
              END CASE;
              CASE DSIsignal
                 WHEN AllUp:
                    gre:=1;
                 WHEN Degraded:
                    yel:=1;
                 WHEN NodeIdle:
                    sie:=1;
                 WHEN NodeStandby:
                    blu:=1;
                 WHEN Down:
                    red:=1;
                 WHEN NodePM:
                    ora:=1;
                 OTHERWISE 
                    NEW(message,1..1);
                    message[1]:="Unknown hierarchy status color!     "; 
                    result:=SendAlert(message,FALSE, FALSE, TRUE);
                    DISPOSE(message);   
              END CASE;
              tempHier := ASK root Child("RBDHier", parentID);
              ASK tempHier TO ChangeDSIStatus(gre,blu,yel,sie,ora,red);
           END IF;
        END IF;
     END IF;
  END METHOD;     {ChangeDSIStatus}

