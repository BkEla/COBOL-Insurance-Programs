000100                                                                              
000200******************************************************************            
000300******************************************************************            
000400*                                                                *            
000500*              PROGRAM NAME:BEA/PROJECT9                         *            
000600******************************************************************            
000700*                                                                             
000800*  THIS PROGRAM UPDATES (REWRITE) THE MASTER RECORD DELETE FIELD              
000900*  WITH AN "A". IT ALSO READS  *MULTI/FILE/KIO ON DISK, AND               
001000*  *AGENT/FILE/KIO ON DISK THEN LISTS ALL  MH (MOBILE HOME)                
001100*  POLICIES WRITTEN WITH AGENCY #174.                                         
001200*  DELETED POLICIES INDICATE "A" VALUES UNDER 'DELTED' COLUMN.                
001300*                                                                             
001400*                                                                             
001500* FILES:                                                                      
001600*  INFORCE:                                                                   
001700*         *MULTI/FILE/KIO ON DISK      (INPUT)                            
001800*         *AGENT/FILE/KIO ON DISK      (INPUT)                            
001900*                                                                             
002000*  PRINTER FILE                                                               
002100*         *PRINT-RECORD-1                                                     
002200*                                                                             
002300*                                                                             
002400*                                                                             
002500******************************************************************            
002600******************************************************************            
002700*          I D E N T I F I C A T I O N    D I V I S I O N        *            
002800******************************************************************            
002900******************************************************************            
003000                                                                              
003100 IDENTIFICATION DIVISION.                                                     
003200                                                                              
003300 PROGRAM-ID.             PROJECT9.                                            
003400                                                                              
003500*AUTHOR.                 BIRUK AYALEW                                         
003600                                                                              
003700*DATE-WRITTEN.           08/01/2023                                           
003800                                                                              
003900*CHANGE LOG:                                                                  
004000*                                                                             
004100******************************************************************            
004200******************************************************************            
004300*         E N V I R O N M E N T       D I V I S I O N            *            
004400******************************************************************            
004500******************************************************************            
004600                                                                              
004700 ENVIRONMENT DIVISION.                                                        
004800                                                                              
004900******************************************************************            
005000   CONFIGURATION SECTION.                                                     
005100******************************************************************            
005200                                                                              
005300   SOURCE-COMPUTER.    CS7101.                                                
005400   OBJECT-COMPUTER.    CS7101.                                                
005500                                                                              
005600                                                                              
005700******************************************************************            
005800   INPUT-OUTPUT SECTION.                                                      
005900******************************************************************            
006000                                                                              
006100   FILE-CONTROL.                                                              
006200                                                                              
006300******************************************************************            
006400*                SELECT MONTH-END MASTER FILE                    *            
006500******************************************************************            
006600     SELECT                       MULTI-FILE                              
006625          ASSIGN TO               DISK                                        
006650          ORGANIZATION IS         INDEXED                                     
006675          ACCESS IS               DYNAMIC                                     
006700          RECORD KEY IS           MULTI-KEY.                                  
006900                                                                              
007000******************************************************************            
007100     SELECT                       AGENT-FILE                               
007200          ASSIGN TO               DISK                                        
007300          ORGANIZATION IS         INDEXED                                     
007350          ACCESS IS               DYNAMIC                                     
007400          RECORD KEY IS           AGT-KEY.                                    
007500                                                                              
007600******************************************************************            
007700*                       SELECT PRINT FILE 1                                   
007800******************************************************************            
007900     SELECT                       PRINT-RECORD-1                              
008000         ASSIGN TO                PRINTER.                                    
008100                                                                              
008200******************************************************************            
008300******************************************************************            
008400*                   D A T A   D I V I S I O N                    *            
008500******************************************************************            
008600******************************************************************            
008700                                                                              
008800 DATA DIVISION.                                                               
008900                                                                              
009000 FILE SECTION.                                                                
009100                                                                              
009200******************************************************************            
009300*                    MULTI  MASTER FILE                          *            
009400******************************************************************            
009500                                                                              
009600 FD  MULTI-FILE                                                           
009700     LABEL RECORDS ARE STANDARD                                               
009800     RECORD CONTAINS 750 CHARACTERS                                           
009900     VALUE OF DEPENDENTSPECS IS TRUE                                          
010000     TITLE IS "*MULTI/FILE/KIO ON DISK".                                  
010100 01  MULTI-MAST.                                                              
010108     05 MULTI-KEY.                                                            
010116        10 MULTI-POLNUM.                                                      
010124           15 MULTI-POLNUM-LOB            PIC X(2).                           
010132           15 MULTI-POLNUMBER             PIC X(5).                           
010140        10 MULTI-ST                       PIC 99.                             
010148           88 MINNESOTA                              VALUE 22.                
010156           88 NORTH-DAKOTA                           VALUE 33.                
010164           88 OKLAHOMA                               VALUE 35.                
010172           88 SOUTH-DAKOTA                           VALUE 40.                
010180           88 NEBRASKA                               VALUE 26.                
010188           88 IOWA                                   VALUE 14.                
010200           88 WISCONSIN                              VALUE 48.                
011150     05 MULTI-LOB                      PIC X.                                 
011200        88 MH-LOB                                    VALUE "8".               
012600     05 FILLER                         PIC X(2).                              
012700     05 MULTI-EFF-DATE.                                                       
012800        10  MULTI-EFF-DATE-YYYY        PIC 9(4).                              
012900        10  MULTI-EFF-DATE-MM          PIC 9(2).                              
013000        10  MULTI-EFF-DATE-DD          PIC 9(2).                              
013033     05 FILLER                         PIC X(10).                             
013066     05 MULTI-AGENT-NUM                PIC X(4).                              
013100     05 FILLER                         PIC X(2).                              
013200     05 MULTI-INS-NAME                 PIC X(36).                             
013300     05 FILLER                         PIC X(3).                              
013400     05 MULTI-ZIP                      PIC 9(5).                              
013500        88 MINNEOTA                                 VALUE 56264.              
013600        88 COTTONWOOD                               VALUE 56229.              
013700        88 MARSHALL                                 VALUE 56258.              
013800     05 FILLER                         PIC X(4).                              
013900     05 MULTI-PREM                     PIC 9(7)V99.                           
014000     05 FILLER                         PIC X(477).                            
014100     05 MULTI-DELETED                  PIC X.                                 
014200        88 DELETED                                  VALUE "D".                
014300     05 FILLER                         PIC X(179).                            
014400                                                                              
014500******************************************************************            
014600*                       AGENT-FILE FILE                       *            
014700******************************************************************            
014750                                                                              
014800 FD  AGENT-FILE                                                            
014900     LABEL RECORDS ARE STANDARD                                               
015000     RECORD CONTAINS 850 CHARACTERS                                           
015200     VALUE OF DEPENDENTSPECS IS TRUE                                          
015300     TITLE IS "*AGENT/FILE/KIO ON DISK".                                   
015400                                                                              
015500 01  AGNT-PRDCR.                                                              
015600     05 FILLER                         PIC X.                                 
015700     05 AGT-KEY                        PIC X(4).                              
015800        88 AGT-174                                   VALUE " 174".            
015900     05 AGT-NAME                       PIC X(30).                             
016000     05 AGT-ADDRESS                    PIC X(25).                             
016100     05 FILLER                         PIC X(20).                             
016200     05 AGT-CT-ST                      PIC X(21).                             
016300     05 AGT-ZIP                        PIC X(5).                              
016400     05 FILLER                         PIC X(744).                            
016450                                                                              
016500******************************************************************            
016600*                          PRINT FILES                           *            
016700******************************************************************            
016750                                                                              
016800 FD  PRINT-RECORD-1                                                           
016900     RECORD CONTAINS 132 CHARACTERS.                                          
017000                                                                              
017100 01  P-REC-1                           PIC X(132).                            
017200                                                                              
017300******************************************************************            
017400******************************************************************            
017500*         W O R K I N G    S T O R A G E    S E C T I O N        *            
017600******************************************************************            
017700******************************************************************            
017800                                                                              
017900 WORKING-STORAGE SECTION.                                                     
017950                                                                              
018000     *>END OF FILE CONTROL<*                                                  
018100 01  WS-END-OF-MULTI-FILE              PIC 9         VALUE ZEROES.            
018200     88 END-OF-MULTI-FILE                            VALUE 1.                 
018300 01  WS-END-OF-AGT-FILE                PIC 9         VALUE ZEROES.            
018400     88 END-OF-AGT-FILE                              VALUE 1.                 
018508                                                                              
018511     *>FILE STATUS CONTROL<                                                   
018522 01  WS-PRT-FL                         PIC 9         VALUE ZERO.              
018533     88 WS-PRT-FL-OPN                          VALUE 1.                       
018544                                                                              
018555     *>PAGE NUMBER<                                                           
018566 01  WS-PG-NUM                         PIC 999       VALUE ZEROES.            
018577                                                                              
018588     *>PRINT LINES*<                                                          
018600 01  WS-PRT-LNS                        PIC 99        VALUE ZEROES.            
019050                                                                              
019500     *>CURRENT RUN DATE*<                                                     
019600 01  WS-CURR-DATE                      PIC Z9/99/99  VALUE ZEROES.            
019700                                                                              
019720     *>HEADING 1<*                                                            
019740 01  WS-HEADING-1.                                                            
019760     05 FILLER                         PIC X(6)      VALUE SPACES.            
019780     05 FILLER                         PIC X(10)     VALUE                    
019800        "RUN DATE: ".                                                         
020300     05 WS-PRT-CURR-DATE               PIC Z9/99/99  VALUE ZEROES.            
020400     05 FILLER                         PIC X(10)     VALUE SPACES.            
020500     05 FILLER                         PIC X(30)     VALUE                    
020600        "IN-FORCE MH POLICIES BY AGENT".                                      
020700     05 WS-PRT-AGT-NUM                 PIC ZZZ9      VALUE ZEROES.            
020800     05 FILLER                         PIC X(10)     VALUE SPACES.            
020900     05 FILLER                         PIC X(14)     VALUE                    
021000        "(BEA/PROJECT9)".                                            
021100     05 FILLER                         PIC X(10)     VALUE SPACES.            
021200     05 FILLER                         PIC X(6)      VALUE                    
021300        "PAGE ".                                                              
021400     05 WS-PG-NUM-PRT                  PIC ZZ9       VALUE ZEROES.            
021500                                                                              
021600     *>AGENCY HEADER<*                                                        
021700 01  WS-AGENCY-NAME-PRT.                                                      
021800     05 FILLER                         PIC X(6)      VALUE SPACES.            
021900     05 FILER                          PIC X(13)                              
022000        VALUE "AGENT NAME: ".                                                 
022100     05 WS-AGENCY-NAME                 PIC X(30)     VALUE SPACES.            
022200 01  WS-AGENCY-ADDRESS-PRT.                                                   
022300     05 FILLER                         PIC X(6)      VALUE SPACES.            
022400     05 FILER                          PIC X(16)                              
022500        VALUE "AGENT ADDRESS: ".                                              
022600     05 WS-AGENCY-ADDRESS              PIC X(25)     VALUE SPACES.            
022650                                                                              
022700     *>WS AGENT INFO<*                                                        
022800 01 WS-AGENCY-CT-ST-ZIP-PRT.                                                  
022900        05 FILLER                      PIC X(6)      VALUE SPACES.            
023000        05 FILER                       PIC X(28)                              
023100           VALUE "AGENT CITY, STATE AND ZIP: ".                               
023200        05 WS-AGENCY-CT-ST             PIC X(21)     VALUE SPACES.            
023300        05 FILLER                      PIC X(2)      VALUE ", ".              
023400        05 WS-AGENCY-ZIP               PIC X(5)      VALUE SPACES.            
023500                                                                              
023600     *>HEADING 2<*                                                            
023700 01  WS-HEADING-2.                                                            
023800     05 FILLER                         PIC X(6)      VALUE SPACES.            
023900     05 FILLER                         PIC X(7)      VALUE                    
024000        "POLICY#".                                                            
024100     05 FILLER                         PIC X(11)     VALUE SPACES.            
024200     05 FILLER                         PIC X(29)     VALUE                    
024300        "INSURED NAME".                                                       
024400     05 FILLER                         PIC X(6)      VALUE SPACES.            
024500     05 FILLER                         PIC X(15)     VALUE                    
024600        "EFFECTIVE DATE".                                                     
024700     05 FILLER                         PIC X(8)      VALUE SPACES.            
024800     05 FILLER                         PIC X(10)     VALUE                    
024900        "PREMIUM".                                                            
025000     05 FILLER                         PIC X(3)      VALUE SPACES.            
025033     05 FILLER                         PIC X(8)      VALUE                    
025066        "DELETED".                                                            
025100                                                                              
025200     *>WS MH<*                                                                
025300 01  WS-MH-147.                                                               
025400     05 FILLER                         PIC X(6)      VALUE SPACES.            
025500     05 WS-POLNUM                      PIC X(7)      VALUE SPACES.            
025600     05 FILLER                         PIC X(6)      VALUE SPACES.            
025700     05 WS-INS-NAME                    PIC X(36)     VALUE SPACES.            
025800     05 FILLER                         PIC X(6)      VALUE SPACES.            
025900     05 WS-EFF-DATE.                                                          
026000        10 WS-EFF-DATE-YYYY            PIC 9(4)      VALUE ZEROES.            
026100        10 FILLER                      PIC X         VALUE "-".               
026200        10 WS-EFF-DATE-MM              PIC 9(2)      VALUE ZEROES.            
026300        10 FILLER                      PIC X         VALUE "-".               
026400        10 WS-EFF-DATE-DD              PIC 9(2)      VALUE ZEROES.            
026500     05 FILLER                         PIC X(6)      VALUE SPACES.            
026600     05 WS-PREM                        PIC $$,$$$,$$9.99                      
026700        VALUE ZEROES.                                                         
026750     05 FILLER                         PIC X(8)      VALUE SPACES.            
026800     05 WS-DELETED                     PIC X         VALUE SPACES.            
026850                                                                              
026900******************************************************************            
027000******************************************************************            
027100*            P R O C E D U R E   D I V I S I O N                 *            
027200******************************************************************            
027300******************************************************************            
027400                                                                              
027500 PROCEDURE DIVISION.                                                          
027600                                                                              
027700*****************************************************************             
027800                                                                              
027900 1000-START-UP.                                                               
027950                                                                              
028000     PERFORM 1000-OPENING-PROCEDURE THRU                                      
028100             1099-OPENING-PROCEDURE-X.                                        
028200                                                                              
028300     PERFORM 2000-START-AGENT-FILE THRU                                    
028400             2099-START-AGENT-FILE-X                                       
028500                  UNTIL END-OF-AGT-FILE.                                      
028600                                                                              
028700     PERFORM 3000-READ-MULTI-MAST THRU                                        
028800             3099-READ-MULTI-MAST-X                                           
028900                  UNTIL END-OF-MULTI-FILE.                                    
028950                                                                              
029100     PERFORM 7000-CLOSING-PROCEDURE THRU                                      
029200             7099-CLOSING-PROCEDURE-X.                                        
029300                                                                              
029400     STOP RUN.                                                                
029500                                                                              
029600***************************************************************               
029700                                                                              
029800 1000-OPENING-PROCEDURE.                                                      
029900                                                                              
030000     OPEN I-O MULTI-FILE.                                                 
030100     OPEN INPUT AGENT-FILE.                                                
030150                                                                              
030200*    *** INITIALIZE CURRENT DATE  *****                                       
030300     ACCEPT WS-CURR-DATE FROM TODAYS-DATE.                                    
030400     MOVE WS-CURR-DATE TO WS-PRT-CURR-DATE.                                   
030500                                                                              
030600 1099-OPENING-PROCEDURE-X.                                                    
030700     EXIT.                                                                    
030750                                                                              
030800*****************************************************************             
030900                                                                              
031000 2000-START-AGENT-FILE.                                                    
031100                                                                              
031200     MOVE " 174" TO AGT-KEY.                                                  
031250                                                                              
031300     START AGENT-FILE                                                      
031400        KEY NOT < AGT-KEY                                                     
031500        INVALID KEY                                                           
031600            MOVE 1 TO WS-END-OF-AGT-FILE.                                     
031700                                                                              
031800        PERFORM 2100-READ-AGENT-PRDCR THRU                                    
031900                2199-READ-AGENT-PRDCR-X                                       
032000                    UNTIL END-OF-AGT-FILE.                                    
032100                                                                              
032200 2099-START-AGENT-FILE-X.                                                  
032300     EXIT.                                                                    
032400                                                                              
032500*****************************************************************             
032600                                                                              
032700 2100-READ-AGENT-PRDCR.                                                       
032750                                                                              
032800     READ AGENT-FILE NEXT RECORD                                           
032900        AT END                                                                
033000           MOVE 1 TO WS-END-OF-AGT-FILE.                                      
033100                                                                              
033200     IF NOT AGT-174                                                           
033300        MOVE 1 TO WS-END-OF-AGT-FILE                                          
033400        GO TO 2199-READ-AGENT-PRDCR-X.                                        
033500                                                                              
033600     PERFORM 1600-MOVE-AGT-REC THRU                                           
033700             1699-MOVE-AGT-REC-X.                                             
033800                                                                              
033900 2199-READ-AGENT-PRDCR-X.                                                     
034000     EXIT.                                                                    
034100                                                                              
034200*****************************************************************             
034300                                                                              
034400 3000-READ-MULTI-MAST.                                                        
034496                                                                              
034500     READ MULTI-FILE NEXT RECORD                                          
034600         AT END                                                               
034700            MOVE 1 TO WS-END-OF-MULTI-FILE                                    
034800            GO TO 3099-READ-MULTI-MAST-X.                                     
035300                                                                              
035400         *> CHECK IF AGENT NUM IS 174 <*                                      
035500         IF MULTI-AGENT-NUM NOT = " 174"                                      
035600            GO TO 3099-READ-MULTI-MAST-X.                                     
035700                                                                              
035712         *> CHECK IF ONLY INFORCE <*                                          
035724         IF DELETED                                                           
035736            MOVE "A" TO MULTI-DELETED                                         
035748            PERFORM 3100-REWRITE-MULTI-RECORD THRU                            
035760                    3199-REWRITE-MULTI-RECORD-X.                              
035784                                                                              
035800         *> CHECK IF LOB IS MH <*                                             
035900         IF NOT MH-LOB                                                        
036000            GO TO 3099-READ-MULTI-MAST-X.                                     
036100                                                                              
036200        ADD 1 TO WS-PRT-LNS                                                   
036300        PERFORM 1500-MOVE-MULTI-REC THRU                                      
036400                1599-MOVE-MULTI-REC-X                                         
036500        IF NOT WS-PRT-FL-OPN                                                  
036600            OPEN OUTPUT PRINT-RECORD-1                                        
036700            MOVE 1 TO WS-PRT-FL                                               
036800            *>START A NEW PAGE WITH HEADING AND PAGE NUMBER<*                 
036900            PERFORM 2500-START-NEW-PG THRU                                    
037000                    2599-START-NEW-PG-X                                       
037100            WRITE P-REC-1 FROM WS-MH-147                                      
037200                AFTER ADVANCING 1 LINE                                        
037300            GO TO 3099-READ-MULTI-MAST-X.                                     
037400                                                                              
037500        IF WS-PRT-FL-OPN                                                      
037600            *> CHECK IF PRINT LINES IS NOT >50 <*                             
037700            IF WS-PRT-LNS NOT > 50                                            
037800                WRITE P-REC-1 FROM WS-MH-147                                  
037900                    AFTER ADVANCING 1 LINE                                    
038000                GO TO 3099-READ-MULTI-MAST-X                                  
038100            ELSE                                                              
038200                *> START A NEW PAGE IF PRT LINES > 50 <*                      
038300                MOVE SPACES TO P-REC-1                                        
038400                WRITE P-REC-1 AFTER ADVANCING PAGE                            
038500                PERFORM 2500-START-NEW-PG THRU                                
038600                        2599-START-NEW-PG-X                                   
038700                MOVE 0 TO WS-PRT-LNS                                          
038800                WRITE P-REC-1 FROM WS-MH-147                                  
038900                    AFTER ADVANCING 1 LINE                                    
039000                GO TO 3099-READ-MULTI-MAST-X.                                 
039050                                                                              
039300                                                                              
039400 3099-READ-MULTI-MAST-X.                                                      
039500     EXIT.                                                                    
039600***************************************************************               
039608                                                                              
039616 3100-REWRITE-MULTI-RECORD.                                                   
039624                                                                              
039632     REWRITE MULTI-MAST                                                       
039648        INVALID KEY                                                           
039664        DISPLAY "ERROR REWRITING *MULTI/MULTI-MAST RECORD.".                  
039672                                                                              
039680 3199-REWRITE-MULTI-RECORD-X.                                                 
039690     EXIT.                                                                    
039695                                                                              
039700***************************************************************               
039800*                     MOVE MULTI REC                          *               
039900***************************************************************               
040000                                                                              
040100 1500-MOVE-MULTI-REC.                                                         
040150                                                                              
040200     MOVE MULTI-POLNUM TO WS-POLNUM.                                          
040300     MOVE MULTI-INS-NAME TO WS-INS-NAME.                                      
040400     MOVE MULTI-EFF-DATE-YYYY TO WS-EFF-DATE-YYYY.                            
040500     MOVE MULTI-EFF-DATE-MM TO WS-EFF-DATE-MM.                                
040600     MOVE MULTI-EFF-DATE-DD TO WS-EFF-DATE-DD.                                
040700     MOVE MULTI-PREM TO WS-PREM.                                              
040750     MOVE MULTI-DELETED TO WS-DELETED.                                        
040800                                                                              
040900 1599-MOVE-MULTI-REC-X.                                                       
041000     EXIT.                                                                    
041100                                                                              
041200***************************************************************               
041300*                     MOVE AGENT REC                          *               
041400***************************************************************               
041500                                                                              
041600 1600-MOVE-AGT-REC.                                                           
041650                                                                              
041700     MOVE AGT-NAME TO WS-AGENCY-NAME.                                         
041800     MOVE AGT-ADDRESS TO WS-AGENCY-ADDRESS.                                   
041900     MOVE AGT-CT-ST TO WS-AGENCY-CT-ST.                                       
042000     MOVE AGT-ZIP TO WS-AGENCY-ZIP.                                           
042050     MOVE AGT-KEY TO WS-PRT-AGT-NUM.                                          
042075                                                                              
042100 1699-MOVE-AGT-REC-X.                                                         
042200     EXIT.                                                                    
042300                                                                              
042400***************************************************************               
042500*                     STARTING PAGES                          *               
042600***************************************************************               
042700                                                                              
042800 2500-START-NEW-PG.                                                           
042850                                                                              
042900     MOVE 0 TO WS-PRT-LNS.                                                    
043000     ADD 1 TO WS-PG-NUM.                                                      
043100     MOVE WS-PG-NUM TO WS-PG-NUM-PRT.                                         
043200     WRITE P-REC-1 FROM WS-HEADING-1                                          
043300        BEFORE ADVANCING 2 LINE.                                              
043400     PERFORM 2200-WRITE-AGT-REC THRU                                          
043500             2299-WRITE-AGT-REC-X.                                            
043600     WRITE P-REC-1 FROM WS-HEADING-2 BEFORE ADVANCING 1 LINE.                 
043700     MOVE 10 TO WS-PRT-LNS.                                                   
043750                                                                              
043800 2599-START-NEW-PG-X.                                                         
043900     EXIT.                                                                    
044000                                                                              
044100***************************************************************               
044200 2200-WRITE-AGT-REC.                                                          
044250                                                                              
044300     WRITE P-REC-1 FROM WS-AGENCY-NAME-PRT BEFORE ADVANCING 1.                
044400     WRITE P-REC-1 FROM WS-AGENCY-ADDRESS-PRT BEFORE ADVANCING 1.             
044500     WRITE P-REC-1 FROM WS-AGENCY-CT-ST-ZIP-PRT                               
044600           BEFORE ADVANCING 2.                                                
044700                                                                              
044800 2299-WRITE-AGT-REC-X.                                                        
044900     EXIT.                                                                    
045000                                                                              
045100***************************************************************               
045200                                                                              
045300 7000-CLOSING-PROCEDURE.                                                      
045350                                                                              
045400     CLOSE MULTI-FILE WITH RELEASE.                                       
045500     CLOSE AGENT-FILE WITH RELEASE.                                        
045525     IF WS-PRT-FL-OPN                                                         
045550       CLOSE PRINT-RECORD-1 WITH SAVE                                         
045575     ELSE                                                                     
045600       DISPLAY "NO RECORDS FOUND".                                            
045700                                                                              
045800 7099-CLOSING-PROCEDURE-X.                                                    
045900     EXIT.                                                                    
046000                                                                              
046100*****************************************************************             
