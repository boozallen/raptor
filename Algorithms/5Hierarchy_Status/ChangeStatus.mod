

  ASK METHOD ChangeStatus(IN newcolor : NodeStatusType);
     VAR
        hierImage,dsi           : ImageObj;
        tempHier                : RBDHierObj;
        oldDSIsignal,oldStatus  : NodeStatusType;
        gre,yel,sie,blu,red,ora : INTEGER;
     BEGIN
        oldStatus:=Status;
        Status:=newcolor;
        IF (GraphicsOutput AND (oldStatus<>newcolor))   
           hierImage := Descendant("Hier", 603); 
           CASE newcolor
              WHEN AllUp:
                 ASK hierImage TO SetColor(LimeGreen);
                 ASK hierImage TO SetTranslation(hierImage.Translation.x, hierImage.Translation.y);
              WHEN Degraded:
                 ASK hierImage TO SetColor(Yellow);
                 ASK hierImage TO SetTranslation(hierImage.Translation.x, hierImage.Translation.y);
              WHEN NodeIdle:
                 ASK hierImage TO SetColor(Sienna);
                 ASK hierImage TO SetTranslation(hierImage.Translation.x, hierImage.Translation.y);
              WHEN NodeStandby:
                 ASK hierImage TO SetColor(Blue);
                 ASK hierImage TO SetTranslation(hierImage.Translation.x, hierImage.Translation.y);
              WHEN Down:
                 ASK hierImage TO SetColor(Red);
                 ASK hierImage TO SetTranslation(hierImage.Translation.x, hierImage.Translation.y);
              WHEN NodePM:
                 ASK hierImage TO SetColor(Orange);
                 ASK hierImage TO SetTranslation(hierImage.Translation.x, hierImage.Translation.y);
              OTHERWISE 
                 NEW(message,1..1);
                 message[1]:="Unknown hierarchy status color!     "; 
                 result:=SendAlert(message,FALSE, FALSE, TRUE);
                 DISPOSE(message);   
           END CASE;
           Draw; 
           IF NOT runCompleted
              IF (parentID > 0)
                 oldDSIsignal:=DSIsignal;
                 IF (Status > dsiState)     {newColor worse than }
                    DSIsignal:=Status;
                 ELSE
                    DSIsignal:=dsiState;
                 END IF;
                 tempHier := ASK root Child("RBDHier", parentID);
                 IF ( ((oldStatus=AllUp) OR (oldStatus=Degraded)  OR (oldStatus=NodeStandby)) AND
                      ((Status=Down) OR (Status=NodeIdle) OR (Status=NodePM))   ) 
                    ASK tempHier TO IncNumFailed;
                 ELSIF ( ((Status=AllUp) OR (Status=Degraded)  OR (Status=NodeStandby)) AND
                      ((oldStatus=Down) OR (oldStatus=NodeIdle) OR (oldStatus=NodePM))   ) 
                    ASK tempHier TO DecNumFailed;
                 END IF;
                 IF DSIsignal<>oldDSIsignal;              
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
                    ASK tempHier TO ChangeDSIStatus(gre,blu,yel,sie,ora,red);                 
                 END IF;
              END IF;  {parentId>0}
           END IF;  {runCompleted}
        END IF; {GraphicsOutput AND (oldStatus<>newcolor)}
        IF (GraphicsOutput AND runCompleted)
            dsi:= Descendant("HierMid", 0);
            ASK dsi TO SetColor(LimeGreen);
        END IF;
        
        
  END METHOD;  {HierObj - ChangeStatus}

