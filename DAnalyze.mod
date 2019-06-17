{+++++++++++++++++++++++++++++++++++++++++++RAPTOR++++++++++++++++++++++++++++++++++++++++++++++}
{+                                                                                             +}
{+  Definition module file name : ANALYZE                                                      +}
{+  Author                      : Chuck Carter                                                 +}
{+  Date                        : 24 Mar 03                                                    +}
{+                                                                                             +}
{+  Description Here                   .                                                       +}
{+                                                                                             +}
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

DEFINITION MODULE Analyze;

FROM RandMod IMPORT RandomObj;
FROM Objects IMPORT realArray,intArray;
FROM GrpMod  IMPORT QueueObj; 

TYPE

RandomGenObj = OBJECT (RandomObj);
   ASK METHOD DrawNumber   (IN  currentDistro            : INTEGER; 
                            IN  dataArray                : realArray;
                            IN  gettingDraw              : STRING;
                            OUT drawn                    : REAL);
   ASK METHOD Bernoulli    (IN  probability              : REAL)       : INTEGER;
   ASK METHOD ChiSquare    (IN  shape                    : INTEGER)    : REAL;
   ASK METHOD Pearson5     (IN  scale, shape, location   : REAL)       : REAL;
   ASK METHOD Pearson6     (IN  scale, shape, shape2     : REAL)       : REAL;
   ASK METHOD Empirical    (IN  dataArray                : realArray)  : REAL;
   ASK METHOD ExponentialPower  (IN scale,shape,location : REAL)       : REAL;
   ASK METHOD ExtremeValue (IN  scale,location           : REAL)       : REAL;
   ASK METHOD Laplace      (IN  scale,location           : REAL)       : REAL;
   OVERRIDE
      ASK METHOD LogNormal (IN  mean, stDev              : REAL)       : REAL;
      ASK METHOD Poisson   (IN  mu                       : REAL)       : INTEGER;
END OBJECT; {RandomGenObj}


PROCEDURE RunEFPA          (INOUT LoopId                 : INTEGER);
PROCEDURE SortBlocks;
PROCEDURE SortNodes;
PROCEDURE SortEvents;
PROCEDURE SortHiers;
PROCEDURE FetchRaptorSeed  (IN seedNum                   : INTEGER)    : INTEGER;
PROCEDURE CreateDomainTree;
PROCEDURE WriteDomains     (IN i                         : INTEGER;
                            IN tab                       : STRING);
PROCEDURE WriteCores       (IN i                         : INTEGER;
                            IN tab                       : STRING);


VAR
   BlockAlphaSort,NodeAlphaSort,EventAlphaSort,HierAlphaSort   : intArray;
   DownstreamGroup, PropagatedGroup                            : QueueObj;
   analysisType                                                : STRING;
   LoopCheck,TestElement,totalCapNodes,randNumCount            : INTEGER;
   LoopError                                                   : BOOLEAN;

END MODULE.



