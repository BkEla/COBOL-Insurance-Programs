000100$$SET LINEINFO                                                                
000200******************************************************************            
000300******************************************************************            
000400*                                                                *            
000500*              PROGRAM NAME:BEA/PROJECT13                        *        
000600******************************************************************            
000700*                                                                             
000800*  THIS PROGRAM READS *CLAIM/KIO SEQUENTIALLY AND CHECKS IF A             
000900*  POLICY IS NOT CANCELED IN *MULTI/FILE/KIO. IT THEN PRINTS              
001000*  NON-PKG POLICIES IN *MULTI/FILE/KIO THAT ARE EXPIRING IN 90            
001100*  DAYS IN ONE REPORT FILE AND NON-EXPIRING IN ANOTHER.                       
001200*  REPORT INCLUDES INSURED NAME, POLICY #, STATE, CLAIM #,                    
001300*  DATE OF LOSS AND EFFECTIVE DATE.                                           
001400*                                                                             
001500*                                                                             
001600*  FILES:                                                                     
001700*  INFORCE:                                                                   
001800*         *MULTI/FILE/KIO ON DISK           (INPUT)                            
001900*         *CLAIM/KIO      ON DISK           (INPUT)                            
002000*                                                                             
002100*  PRINTER FILE                                                               
002200*         *PRINT-RECORD-1                                                     
002300*         *PRINT-RECORD-2                                                     
002400*                                                                             
002500*                                                                             
002600*                                                                             
002700******************************************************************            
002800******************************************************************            
002900*          I D E N T I F I C A T I O N    D I V I S I O N        *            
003000******************************************************************            
003100******************************************************************            
003200                                                                              
003300 IDENTIFICATION DIVISION.                                                     
003400                                                                              
003500 PROGRAM-ID.             PROJECT13.                                           
003600                                                                              
003700*AUTHOR.                 BIRUK AYALEW                                         
003800                                                                              
003900*DATE-WRITTEN.           08/29/2023                                           
004000                                                                              
004100*CHANGE LOG:                                                                  
004200*                                                                             
004300******************************************************************            
004400******************************************************************            
004500*         E N V I R O N M E N T       D I V I S I O N            *            
004600******************************************************************            
004700******************************************************************            
004800                                                                              
004900 ENVIRONMENT DIVISION.                                                        
005000                                                                              
005100******************************************************************            
005200   CONFIGURATION SECTION.                                                     
005300******************************************************************            
005400                                                                              
005500   SOURCE-COMPUTER.    CS7101.                                                
005600   OBJECT-COMPUTER.    CS7101.                                                
005700                                                                              
005800                                                                              
005900******************************************************************            
006000   INPUT-OUTPUT SECTION.                                                      
006100******************************************************************            
006200                                                                              
006300   FILE-CONTROL.                                                              
006400                                                                              
006500******************************************************************            
006600*                SELECT MONTH-END MASTER FILE                    *            
006700******************************************************************            
006800     SELECT                       MULTI-FILE                              
006900          ASSIGN TO               DISK                                        
007000          ORGANIZATION IS         INDEXED                                     
007100          ACCESS IS               RANDOM                                      
007200          RECORD KEY IS           MULTI-KEY.                                  
007300                                                                              
007400******************************************************************            
007500     SELECT                       CLAIMS-FILE                                 
007600          ASSIGN TO               DISK                                        
007700          ORGANIZATION IS         INDEXED                                     
007800          ACCESS IS               DYNAMIC                                     
007900          RECORD KEY IS           CLAIM-KEY.                                  
008000                                                                              
008100******************************************************************            
008200*                       SELECT PRINT FILE 1                                   
008300******************************************************************            
008400     SELECT                       PRINT-RECORD-1                              
008500         ASSIGN TO                PRINTER.                                    
008600                                                                              
008700******************************************************************            
008800*                       SELECT PRINT FILE 2                                   
008900******************************************************************            
009000     SELECT                       PRINT-RECORD-2                              
009100         ASSIGN TO                PRINTER.                                    
009200                                                                              
009300******************************************************************            
009400******************************************************************            
009500*                   D A T A   D I V I S I O N                    *            
009600******************************************************************            
009700******************************************************************            
009800                                                                              
009900 DATA DIVISION.                                                               
010000                                                                              
010100 FILE SECTION.                                                                
010200                                                                              
010300******************************************************************            
010400*                    MULTI  MASTER FILE                          *            
010500******************************************************************            
010600                                                                              
010700 FD  MULTI-FILE                                                           
010800     LABEL RECORDS ARE STANDARD                                               
010900     RECORD CONTAINS 750 CHARACTERS                                           
011000     VALUE OF DEPENDENTSPECS IS TRUE                                          
011100     TITLE IS "*MULTI/FILE/KIO ON DISK".                                  
011200 01  MULTI-MAST.                                                              
011300     05 MULTI-KEY.                                                            
011400        10 MULTI-POLNUM.                                                      
011500           15 MULTI-POLNUM-LOB         PIC X(2).                              
011600           15 MULTI-POLNUMBER          PIC X(5).                              
011700        10 MULTI-ST                    PIC 99.                                
011800           88 MINNESOTA                              VALUE 22.                
011900           88 NORTH-DAKOTA                           VALUE 33.                
012000           88 OKLAHOMA                               VALUE 35.                
012100           88 SOUTH-DAKOTA                           VALUE 40.                
012200           88 NEBRASKA                               VALUE 26.                
012300           88 IOWA                                   VALUE 14.                
012400           88 WISCONSIN                              VALUE 48.                
012500     05 MULTI-LOB                      PIC X.                                 
012600        88 MH-LOB                                    VALUE "8".               
012700     05 FILLER                         PIC X(2).                              
012800     05 MULTI-EFF-DATE.                                                       
012900        10  MULTI-EFF-DATE-YYYY        PIC 9(4).                              
013000        10  MULTI-EFF-DATE-MM          PIC 9(2).                              
013100        10  MULTI-EFF-DATE-DD          PIC 9(2).                              
013200     05 MULTI-EXP-DATE.                                                       
013300        10  MULTI-EXP-DATE-YYYY        PIC 9(4).                              
013400        10  MULTI-EXP-DATE-MM          PIC 9(2).                              
013500        10  MULTI-EXP-DATE-DD          PIC 9(2).                              
013600     05 MULTI-EXP-DATER  REDEFINES                                            
013700        MULTI-EXP-DATE                 PIC 9(8).                              
013800     05 FILLER                         PIC X(2).                              
013900     05 MULTI-AGENT-NUM                PIC X(4).                              
014000     05 FILLER                         PIC X(2).                              
014100     05 MULTI-INS-NAME                 PIC X(36).                             
014200     05 FILLER                         PIC X(3).                              
014300     05 MULTI-ZIP                      PIC 9(5).                              
014400     05 FILLER                         PIC X(4).                              
014500     05 MULTI-PREM                     PIC 9(7)V99.                           
014600     05 FILLER                         PIC X(346).                            
014700     05 MULTI-LAST-TRAN-DATE.                                                 
014800        10  MULTI-LST-TRN-DATE-YYYY    PIC 9(4).                              
014900        10  MULTI-LST-TRN-DATE-MM      PIC 9(2).                              
015000        10  MULTI-LST-TRN-DATE-DD      PIC 9(2).                              
015100     05 FILLER                         PIC X(123).                            
015200     05 MULTI-DELETED                  PIC X.                                 
015300        88 DELETED-MULTI                             VALUE "D".               
015400     05 FILLER                         PIC X(179).                            
015500                                                                              
015600******************************************************************            
015700*                          CLAIMS-OS FILE                        *            
015800******************************************************************            
015900                                                                              
016000 FD  CLAIMS-FILE                                                              
016100     LABEL RECORDS ARE STANDARD                                               
016200     RECORD CONTAINS 500 CHARACTERS                                           
016300     VALUE OF DEPENDENTSPECS IS TRUE                                          
016400     TITLE IS "*CLAIM/KIO ON DISK".                                       
016500                                                                              
016600 01  CLAIMS-RECORD.                                                           
016700     05 FILLER                         PIC X(4).                              
016800     05 CLM-P-OR-D                     PIC X.                                 
016900        88 PKG                                 VALUE "P" "D".                 
017000     05 CLM-POL-NUM                    PIC X(7).                              
017100     05 FILLER                         PIC X(5).                              
017200     05 CLM-LOSS-DATE.                                                        
017300        10 CLM-LOSS-DATE-YYYY          PIC X(4).                              
017400        10 CLM-LOSS-DATE-MM            PIC X(2).                              
017500        10 CLM-LOSS-DATE-DD            PIC X(2).                              
017600     05 FILLER                         PIC X(1).                              
017700     05 CLAIM-KEY.                                                            
017800        10 CLM-YY                      PIC X(2).                              
017900        10 CLM-NUM                     PIC X(5).                              
018000        10 CLM-RIC                     PIC X(1).                              
018100     05 CLM-STATE                      PIC X(2).                              
018200     05 FILLER                         PIC X(63).                             
018300     05 CLM-DELETED                    PIC X(1).                              
018400        88 DELETED-CLM                               VALUE "D".               
018500     05 FILLER                         PIC X(400).                            
018600                                                                              
018700******************************************************************            
018800*                          PRINT FILES                           *            
018900******************************************************************            
019000*            PRINT FOR POLICIES EXPIRING IN 90 DAYS                           
019100******************************************************************            
019200 FD  PRINT-RECORD-1                                                           
019300     RECORD CONTAINS 132 CHARACTERS.                                          
019400                                                                              
019500 01  P-REC-1                           PIC X(132).                            
019600                                                                              
019700******************************************************************            
019800*            PRINT FOR POLICIES NOT EXPIRING IN 90 DAYS                       
019900******************************************************************            
020000 FD  PRINT-RECORD-2                                                           
020100     RECORD CONTAINS 132 CHARACTERS.                                          
020200                                                                              
020300 01  P-REC-2                           PIC X(132).                            
020400                                                                              
020500******************************************************************            
020600******************************************************************            
020700*         W O R K I N G    S T O R A G E    S E C T I O N        *            
020800******************************************************************            
020900******************************************************************            
021000                                                                              
021100 WORKING-STORAGE SECTION.                                                     
021200                                                                              
021300 01  WS-END-OF-MULTI-FILE              PIC 9         VALUE ZEROES.            
021400     88 END-OF-MULTI-FILE                            VALUE 1.                 
021500 01  WS-END-OF-CLAIM-FILE              PIC 9         VALUE ZEROES.            
021600     88 END-OF-CLAIM-FILE                            VALUE 1.                 
021700                                                                              
021800 01  WS-PRT-FL-OPN                     PIC 9         VALUE ZERO.              
021900     88 PRT-FL-OPN                                   VALUE 1.                 
022000 01  WS-PRT-EXP-FL-OPN                 PIC 9         VALUE ZERO.              
022100     88 PRT-EXP-FL-OPN                               VALUE 1.                 
022200                                                                              
022300 01  WS-PG-NUM                         PIC 999       VALUE ZEROES.            
022400 01  WS-PG-NUM-EXP                     PIC 999       VALUE ZEROES.            
022500                                                                              
022600 01  WS-PRT-LNS                        PIC 99        VALUE ZEROES.            
022700 01  WS-PRT-EXP-LNS                    PIC 99        VALUE ZEROES.            
022800                                                                              
022900 01  WS-STATE-TABLE.                                                          
023000     05  WS-TABLE-STATE-REC OCCURS 7 TIMES                                    
023100            INDEXED BY STATE-INDEX.                                           
023200         10 WS-TABLE-STATE-CODE        PIC 99.                                
023300         10 WS-TABLE-STATE-ABBRV       PIC XX.                                
023400                                                                              
023500 01  WS-STATE-FOUND                    PIC 9         VALUE ZEROES.            
023600     88 STATE-FOUND                                  VALUE 1.                 
023700                                                                              
023800 COPY "(SCR)LNK/DATE/EDITOR/NEW ON PROD".                                     
023900                                                                              
024000 01  WS-CURRENT-DATE.                                                         
024100     05 WS-CURRENT-MO                  PIC 99.                                
024200     05 WS-CURRENT-DA                  PIC 99.                                
024300     05 WS-CURRENT-YR                  PIC 9(4).                              
024400                                                                              
024500 01  WS-HEADING-1.                                                            
024600     05 FILLER                         PIC X(6)      VALUE SPACES.            
024700     05 FILLER                         PIC X(10)     VALUE                    
024800        "RUN DATE: ".                                                         
024900     05 WS-PRT-CURR-DATE               PIC Z9/99/99  VALUE ZEROES.            
025000     05 FILLER                         PIC X(10)     VALUE SPACES.            
025100     05 WS-HEADING-1-DTL               PIC X(50)     VALUE SPACES.            
025200     05 FILLER                         PIC X(5)     VALUE SPACES.             
025300     05 FILLER                         PIC X(15)     VALUE                    
025400        "(BEA/PROJECT13)".                                           
025500     05 FILLER                         PIC X(10)     VALUE SPACES.            
025600     05 FILLER                         PIC X(5)      VALUE                    
025700        "PAGE ".                                                              
025800     05 WS-PG-NUM-PRT                  PIC ZZ9       VALUE ZEROES.            
025900                                                                              
026000 01  WS-HEADING-2.                                                            
026100     05 FILLER                         PIC X(6)      VALUE SPACES.            
026200     05 FILLER                         PIC X(12)     VALUE                    
026300        "INSURED NAME".                                                       
026400     05 FILLER                         PIC X(30)     VALUE SPACES.            
026500     05 FILLER                         PIC X(7)      VALUE                    
026600        "POLICY#".                                                            
026700     05 FILLER                         PIC X(6)      VALUE SPACES.            
026800     05 FILLER                         PIC X(5)      VALUE                    
026900        "STATE".                                                              
027000     05 FILLER                         PIC X(6)      VALUE SPACES.            
027100     05 FILLER                         PIC X(6)      VALUE                    
027200        "CLAIM#".                                                             
027300     05 FILLER                         PIC X(6)      VALUE SPACES.            
027400     05 FILLER                         PIC X(12)     VALUE                    
027500        "DATE OF LOSS".                                                       
027600     05 FILLER                         PIC X(6)      VALUE SPACES.            
027700     05 FILLER                         PIC X(14)     VALUE                    
027800        "EFFECTIVE DATE".                                                     
027900     05 FILLER                         PIC X(3)      VALUE SPACES.            
028000                                                                              
028100 01  WS-REC-CLAIM-DEL.                                                        
028200     05 FILLER                         PIC X(6)      VALUE SPACES.            
028300     05 WS-INS-NAME                    PIC X(36)     VALUE SPACES.            
028400     05 FILLER                         PIC X(6)      VALUE SPACES.            
028500     05 WS-POLNUM                      PIC X(7)      VALUE SPACES.            
028600     05 FILLER                         PIC X(8)      VALUE SPACES.            
028700     05 WS-STATE                       PIC X(2)      VALUE SPACES.            
028800     05 FILLER                         PIC X(6)      VALUE SPACES.            
028900     05 WS-CLAIM-NUM                   PIC X(5)      VALUE SPACES.            
029000     05 FILLER                         PIC X         VALUE "-".               
029100     05 WS-CLAIM-YY                    PIC X(2)      VALUE SPACES.            
029200     05 FILLER                         PIC X(6)      VALUE SPACES.            
029300     05 WS-LOSS-DATE.                                                         
029400        10 WS-LOSS-DATE-YYYY           PIC 9(4)      VALUE ZEROES.            
029500        10 FILLER                      PIC X         VALUE "/".               
029600        10 WS-LOSS-DATE-MM             PIC 9(2)      VALUE ZEROES.            
029700        10 FILLER                      PIC X         VALUE "/".               
029800        10 WS-LOSS-DATE-DD             PIC 9(2)      VALUE ZEROES.            
029900     05 FILLER                         PIC X(8)      VALUE SPACES.            
030000     05 WS-EFF-DATE.                                                          
030100        10 WS-EFF-DATE-YYYY            PIC 9(4)      VALUE ZEROES.            
030200        10 FILLER                      PIC X         VALUE "/".               
030300        10 WS-EFF-DATE-MM              PIC 9(2)      VALUE ZEROES.            
030400        10 FILLER                      PIC X         VALUE "/".               
030500        10 WS-EFF-DATE-DD              PIC 9(2)      VALUE ZEROES.            
030600     05 FILLER                         PIC X(6)      VALUE SPACES.            
030700                                                                              
030800                                                                              
030900******************************************************************            
031000******************************************************************            
031100*            P R O C E D U R E   D I V I S I O N                 *            
031200******************************************************************            
031300******************************************************************            
031400                                                                              
031500 PROCEDURE DIVISION.                                                          
031600                                                                              
031700***************************************************************               
031800*                          START UP                           *               
031900***************************************************************               
032000 1000-START-UP.                                                               
032100                                                                              
032200     PERFORM 1000-OPENING-PROCEDURE THRU                                      
032300             1099-OPENING-PROCEDURE-X.                                        
032400                                                                              
032500     PERFORM 2000-INIT-STATE-REC THRU                                         
032600             2099-INIT-STATE-REC-X.                                           
032700                                                                              
032800     PERFORM 3000-READ-CLAIM-OS THRU                                          
032900             3099-READ-CLAIM-OS-X                                             
033000                  UNTIL END-OF-CLAIM-FILE.                                    
033100                                                                              
033200     PERFORM 7000-CLOSING-PROCEDURE THRU                                      
033300             7099-CLOSING-PROCEDURE-X.                                        
033400                                                                              
033500     STOP RUN.                                                                
033600***************************************************************               
033700*                     OPENING PROCEDURE                       *               
033800***************************************************************               
033900 1000-OPENING-PROCEDURE.                                                      
034000                                                                              
034100     OPEN INPUT MULTI-FILE.                                               
034200     OPEN INPUT CLAIMS-FILE.                                                  
034300                                                                              
034400     ACCEPT WS-PRT-CURR-DATE FROM TODAYS-DATE.                                
034500     ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.                               
034600 1099-OPENING-PROCEDURE-X.                                                    
034700     EXIT.                                                                    
034800***************************************************************               
034900*                    INITIALIZE SATE REC                      *               
035000***************************************************************               
035100 2000-INIT-STATE-REC.                                                         
035200                                                                              
035300     MOVE 22 TO WS-TABLE-STATE-CODE(1).                                       
035400     MOVE "MN" TO WS-TABLE-STATE-ABBRV(1).                                    
035500     MOVE 33 TO WS-TABLE-STATE-CODE(2).                                       
035600     MOVE "ND" TO WS-TABLE-STATE-ABBRV(2).                                    
035700     MOVE 35 TO WS-TABLE-STATE-CODE(3).                                       
035800     MOVE "OK" TO WS-TABLE-STATE-ABBRV(3).                                    
035900     MOVE 40 TO WS-TABLE-STATE-CODE(4).                                       
036000     MOVE "SD" TO WS-TABLE-STATE-ABBRV(4).                                    
036100     MOVE 26 TO WS-TABLE-STATE-CODE(5).                                       
036200     MOVE "NB" TO WS-TABLE-STATE-ABBRV(5).                                    
036300     MOVE 14 TO WS-TABLE-STATE-CODE(6).                                       
036400     MOVE "IA" TO WS-TABLE-STATE-ABBRV(6).                                    
036500     MOVE 48 TO WS-TABLE-STATE-CODE(7).                                       
036600     MOVE "WI" TO WS-TABLE-STATE-ABBRV(7).                                    
036700                                                                              
036800 2099-INIT-STATE-REC-X.                                                       
036900     EXIT.                                                                    
037000***************************************************************               
037100*                      READ CLAIM/OS                          *               
037200***************************************************************               
037300 3000-READ-CLAIM-OS.                                                          
037400                                                                              
037500     READ CLAIMS-FILE NEXT RECORD                                             
037600        AT END                                                                
037700          MOVE 1 TO WS-END-OF-CLAIM-FILE                                      
037750          GO TO 3099-READ-CLAIM-OS-X.                                         
037800                                                                              
037900        IF PKG                                                                
038000            GO TO 3099-READ-CLAIM-OS-X.                                       
038100        IF DELETED-CLM                                                        
038200             GO TO 3099-READ-CLAIM-OS-X.                                      
038300                                                                              
038400        *>MOVE CALIM KEY TO MULTI FOR READ<*                                  
038500        MOVE CLM-POL-NUM TO MULTI-POLNUM.                                     
038600        MOVE CLM-STATE TO MULTI-ST.                                           
038700                                                                              
038800        PERFORM 4000-READ-MULTI-MAST THRU                                     
038900                4099-READ-MULTI-MAST-X.                                       
039600                                                                              
039700 3099-READ-CLAIM-OS-X.                                                        
039800     EXIT.                                                                    
039900***************************************************************               
040000*                    READ MULTI/FILE                      *               
040100***************************************************************               
040200 4000-READ-MULTI-MAST.                                                        
040300                                                                              
040400     READ MULTI-FILE RECORD                                               
040500        INVALID KEY                                                           
040550           DISPLAY "INVALID KEY: " MULTI-KEY                                  
040600           GO TO 4099-READ-MULTI-MAST-X.                                      
040700                                                                              
040800        IF DELETED-MULTI                                                      
040900           GO TO 4099-READ-MULTI-MAST-X.                                      
041100                                                                              
041300        MOVE ZERO TO WS-STATE-FOUND.                                          
041400        PERFORM 4500-CHECK-STATE-CODE THRU                                    
041500                4599-CHECK-STATE-CODE-X                                       
041600                    VARYING STATE-INDEX                                       
041700                    FROM 1 BY 1                                               
041800                        UNTIL STATE-INDEX > 7                                 
041900                        OR WS-STATE-FOUND = 1.                                
041933                                                                              
041966        PERFORM 5000-WRITE-CLAIM-REC THRU                                     
042000                5099-WRITE-CLAIM-REC-X.                                       
042050                                                                              
042100 4099-READ-MULTI-MAST-X.                                                      
042200     EXIT.                                                                    
042300***************************************************************               
042400*                  CHECK AND GET STATE CODE                   *               
042500***************************************************************               
042600 4500-CHECK-STATE-CODE.                                                       
042700                                                                              
042800     IF NOT MULTI-ST = WS-TABLE-STATE-CODE(STATE-INDEX)                       
042900        GO TO 4599-CHECK-STATE-CODE-X.                                        
043000                                                                              
043100        MOVE WS-TABLE-STATE-ABBRV(STATE-INDEX) TO WS-STATE                    
043200        MOVE 1 TO WS-STATE-FOUND.                                             
043300                                                                              
043400 4599-CHECK-STATE-CODE-X.                                                     
043500     EXIT.                                                                    
043600***************************************************************               
043700*                     WRITE CLAIM REC                         *               
043800***************************************************************               
043900 5000-WRITE-CLAIM-REC.                                                        
044000                                                                              
044100     MOVE "D" TO FDATE-TASK.                                                  
044200     MOVE WS-CURRENT-DATE TO FDATE.                                           
044300     MOVE MULTI-EXP-DATER TO FDATE-DIFF-DATE-R.                               
044400     CALL "PROCEDUREDIVISION OF OBJ/LIB/DATE"                      
044500        USING FDATE-RECORD                                                    
044600     END-CALL.                                                                
044700                                                                              
044800     MOVE MULTI-POLNUM TO WS-POLNUM.                                          
044900     MOVE MULTI-INS-NAME TO WS-INS-NAME.                                      
045000     MOVE MULTI-EFF-DATE-MM TO WS-EFF-DATE-MM.                                
045100     MOVE MULTI-EFF-DATE-DD TO WS-EFF-DATE-DD.                                
045200     MOVE MULTI-EFF-DATE-YYYY TO WS-EFF-DATE-YYYY.                            
045300                                                                              
045400     MOVE CLM-YY TO WS-CLAIM-YY                                               
045500     MOVE CLM-NUM TO WS-CLAIM-NUM.                                            
045600     MOVE CLM-LOSS-DATE-MM TO WS-LOSS-DATE-MM.                                
045700     MOVE CLM-LOSS-DATE-DD TO WS-LOSS-DATE-DD.                                
045800     MOVE CLM-LOSS-DATE-YYYY TO WS-LOSS-DATE-YYYY.                            
045900                                                                              
046000     IF FDATE-JULIAN < 90 OR = 9999                                           
046100         PERFORM 5600-PRINT-EXP THRU                                          
046200                 5699-PRINT-EXP-X                                             
046300     ELSE                                                                     
046400         PERFORM 5500-PRINT-NON-EXP THRU                                      
046500                 5599-PRINT-NON-EXP-X.                                        
046600                                                                              
046700 5099-WRITE-CLAIM-REC-X.                                                      
046800     EXIT.                                                                    
046900***************************************************************               
047000*                     PRINTING REPORTS                        *               
047100***************************************************************               
047200 5500-PRINT-NON-EXP.                                                          
047300                                                                              
047500     IF NOT PRT-FL-OPN                                                        
047600         OPEN OUTPUT PRINT-RECORD-1                                           
047700         MOVE 1 TO WS-PRT-FL-OPN                                              
047800         PERFORM 6000-START-NEW-PG THRU                                       
047900                 6099-START-NEW-PG-X                                          
048000     ELSE                                                                     
048100         IF WS-PRT-LNS > 50                                                   
048200            MOVE SPACES TO P-REC-1                                            
048300            WRITE P-REC-1 AFTER ADVANCING PAGE                                
048400            PERFORM 6000-START-NEW-PG THRU                                    
048500                    6099-START-NEW-PG-X                                       
048600         END-IF.                                                              
048700     WRITE P-REC-1 FROM WS-REC-CLAIM-DEL AFTER ADVANCING 1.                   
048800     ADD 1 TO WS-PRT-LNS.                                                     
048900                                                                              
049000 5599-PRINT-NON-EXP-X.                                                        
049100     EXIT.                                                                    
049200***************************************************************               
049300 5600-PRINT-EXP.                                                              
049400                                                                              
049500     IF NOT PRT-EXP-FL-OPN                                                    
049600         OPEN OUTPUT PRINT-RECORD-2                                           
049700         MOVE 1 TO WS-PRT-EXP-FL-OPN                                          
049800         PERFORM 6500-START-NEW-PG-EXP THRU                                   
049900                 6599-START-NEW-PG-EXP-X                                      
050000     ELSE                                                                     
050100         IF WS-PRT-EXP-LNS > 50                                               
050200            MOVE SPACES TO P-REC-2                                            
050300            WRITE P-REC-2 AFTER ADVANCING PAGE                                
050400            PERFORM 6500-START-NEW-PG-EXP THRU                                
050500                    6599-START-NEW-PG-EXP-X                                   
050600         END-IF.                                                              
050700     WRITE P-REC-2 FROM WS-REC-CLAIM-DEL AFTER ADVANCING 1.                   
050800     ADD 1 TO WS-PRT-EXP-LNS.                                                 
050900                                                                              
051000 5699-PRINT-EXP-X.                                                            
051100     EXIT.                                                                    
051200***************************************************************               
051300*                     STARTING PAGES                          *               
051400***************************************************************               
051500 6000-START-NEW-PG.                                                           
051600                                                                              
051700     MOVE 4 TO WS-PRT-LNS.                                                    
051800     ADD 1 TO WS-PG-NUM.                                                      
051900     MOVE WS-PG-NUM TO WS-PG-NUM-PRT.                                         
052000     MOVE "CLAIMS WITH POLICIES EXPIRING IN MORE THAN 90 DAYS" TO             
052100        WS-HEADING-1-DTL.                                                     
052200     WRITE P-REC-1 FROM WS-HEADING-1 BEFORE ADVANCING 2 LINE.                 
052300     WRITE P-REC-1 FROM WS-HEADING-2 BEFORE ADVANCING 1 LINE.                 
052400                                                                              
052500 6099-START-NEW-PG-X.                                                         
052600     EXIT.                                                                    
052700***************************************************************               
052800 6500-START-NEW-PG-EXP.                                                       
052900                                                                              
053000     MOVE 4 TO WS-PRT-EXP-LNS.                                                
053100     ADD 1 TO WS-PG-NUM-EXP.                                                  
053150                                                                              
053200     MOVE WS-PG-NUM-EXP TO WS-PG-NUM-PRT.                                     
053300     MOVE "CLAIMS WITH POLICIES EXPIRING IN 90 DAYS" TO                       
053400        WS-HEADING-1-DTL.                                                     
053500     WRITE P-REC-2 FROM WS-HEADING-1 BEFORE ADVANCING 2 LINE.                 
053600     WRITE P-REC-2 FROM WS-HEADING-2 BEFORE ADVANCING 1 LINE.                 
053700                                                                              
053800 6599-START-NEW-PG-EXP-X.                                                     
053900     EXIT.                                                                    
054000***************************************************************               
054100*                       CLOSING PROCEDURE                     *               
054200***************************************************************               
054300 7000-CLOSING-PROCEDURE.                                                      
054400                                                                              
054500     CLOSE CLAIMS-FILE WITH RELEASE.                                          
054600     CLOSE MULTI-FILE WITH RELEASE.                                       
054700     IF PRT-FL-OPN                                                            
054800       CLOSE PRINT-RECORD-1 WITH SAVE.                                        
054900                                                                              
055000     IF PRT-EXP-FL-OPN                                                        
055100       CLOSE PRINT-RECORD-2 WITH SAVE.                                        
055200                                                                              
055300 7099-CLOSING-PROCEDURE-X.                                                    
055400     EXIT.                                                                    
055500*****************************************************************             
