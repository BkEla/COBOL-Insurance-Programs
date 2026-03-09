000100******************************************************************            
000200******************************************************************            
000300*                                                                             
000400*  PROGRAM NAME: BEA/UMB/AUTOUND                                               
000500******************************************************************            
000600* CONVERTED AN RPG PROGRAM TO COBOL.                                                                            
000700* LIST OUT THE UMBRELLA POLICIES WITH UNDERLYING AUTO LIMITS.                      
000800*                                                                             
000900* SWITCHES WERE ADDED TO PRINT THE COUNTS BY UMBRELLA TYPE:                   
001000*         NO SWITCHES: ALL TYPES                                              
001100*         U1 - PERSONAL    U2 - FARM      U3 - COMMERCIAL                     
001200*                                                                             
001300* WRITTEN BY: JAN FALKUM(RPG)                                                 
001400* REWRITTEN : BIRUK AYALEW(COBOL)                                             
001500*                                                                             
001600* DATE WRITTEN: 6-24-25                                                       
001700*                                      
001800*  FYI: THERE ARE DISPLAYS FOR UNKNOWN LIMITS BUT THOSE                       
001900*       DO GET INCLUDED                                                       
002000*       IN THE COUNTS IN THE OTHER MISC LIMITS SECTION.                       
002100*       THE DISPLAYS MIGHT JUST BE THERE IF ?S COME UP ON THAT.               
002200*                                                                             
002300* FILES:                                                                      
002400*S                                                                            
002500*         *MULTI/FILE/KIO ON DISK         (INPUT)                            
002600*         *UMB/FILE/KIO   ON DISK         (INPUT)                            
002700*         *DATE/RECORD    ON DISK         (INPUT)                            
002800*  PRINTER FILE                                                               
002900*                                                                             
003000*                                                                             
003100******************************************************************            
003200******************************************************************            
003300*          I D E N T I F I C A T I O N    D I V I S I O N                     
003400******************************************************************            
003500******************************************************************            
003600                                                                              
003700 IDENTIFICATION DIVISION.                                                     
003800                                                                              
003900 PROGRAM-ID.             UMB-AUTOUND.                                         
004000                                                                              
004100*AUTHOR.                 BIRUK AYALEW                                         
004200                                                                              
004300*DATE-WRITTEN.           06/24/2025                                           
004400                                                                              
004500*CHANGE LOG:                                                                  
004600*                                                                             
004700******************************************************************            
004800******************************************************************            
004900*         E N V I R O N M E N T       D I V I S I O N                         
005000******************************************************************            
005100******************************************************************            
005200                                                                              
005300 ENVIRONMENT DIVISION.                                                        
005400                                                                              
005500******************************************************************            
005600   CONFIGURATION SECTION.                                                     
005700******************************************************************            
005800                                                                              
005900   SOURCE-COMPUTER.    CS7101.                                                
006000   OBJECT-COMPUTER.    CS7101.                                                
006100                                                                              
006200   SPECIAL-NAMES.                                                             
006300     SW1 OFF STATUS IS SW1-OFF                                                
006400         ON  STATUS IS SW1-ON                                                 
006500     SW2 OFF STATUS IS SW2-OFF                                                
006600         ON  STATUS IS SW2-ON                                                 
006700     SW2 OFF STATUS IS SW3-OFF                                                
006800         ON  STATUS IS SW3-ON.                                                
006900                                                                              
007000/                                                                             
007100******************************************************************            
007200   INPUT-OUTPUT SECTION.                                                      
007300******************************************************************            
007400                                                                              
007500   FILE-CONTROL.                                                              
007600                                                                              
007700******************************************************************            
007800*                SELECT MULTI MASTER FILE                                     
007900******************************************************************            
008000     SELECT                       MASTER                                      
008100          ASSIGN TO               DISK                                        
008200          ORGANIZATION IS         INDEXED                                     
008300          ACCESS MODE IS          DYNAMIC                                     
008400          RECORD KEY IS           UMB-INF-KEY.                                
008700                                                                              
008800******************************************************************            
008900*                SELECT UMB FILE                                        
009000******************************************************************            
009100     SELECT                       FILE                                       
009200          ASSIGN TO               DISK                                        
009300          ORGANIZATION IS         INDEXED                                     
009400          ACCESS MODE IS          DYNAMIC                                     
009500          RECORD KEY IS           UMB-COV-KEY.                                
009700                                                                              
009800******************************************************************            
009900*                SELECT DATE RECORD FILE                                      
010000******************************************************************            
010100     SELECT                       DATER                                       
010200          ASSIGN TO               DISK.                                       
010300                                                                              
010400******************************************************************            
010500*                       SELECT PRINT FILE 1                                   
010600******************************************************************            
010700     SELECT                       PRINT-132                                   
010800         ASSIGN TO                PRINTER.                                    
010900******************************************************************            
011000******************************************************************            
011100*                   D A T A   D I V I S I O N                                 
011200******************************************************************            
011300******************************************************************            
011400                                                                              
011500 DATA DIVISION.                                                               
011600                                                                              
011700 FILE SECTION.                                                                
011800                                                                              
011900******************************************************************            
012000*            F D    M U L T I   M A S T E R   F I L E                         
012100******************************************************************            
012200                                                                              
012300  FD  MASTER                                                                  
012400      RECORD CONTAINS 750 CHARACTERS                                          
012500      VALUE OF DEPENDENTSPECS IS TRUE                                         
012600         TITLE IS "*MULTI/FILE/KIO ON DISK".                              
012700                                                                              
012800      COPY "(SCR)CFD/UMB/MAST   ON DISK" FROM 1000.                          
012900                                                                              
013000******************************************************************            
013100*                   F D    C O V E R   F I L E                                
013200                                                                              
013300                                                                              
013400  FD  FILE                                                                   
013500      RECORD CONTAINS 200 CHARACTERS                                          
013600      VALUE OF DEPENDENTSPECS IS TRUE                                         
013700         TITLE IS "*UMB/FILE/KIO ON DISK".                                   
013800                                                                              
013900      COPY "(SCR)CFD/UMB/FILE  ON DISK"  FROM 1000.                         
014000                                                                              
014100                                                                              
014200******************************************************************            
014300*              F D   D A T E   R E C O R D   F I L E                          
014400******************************************************************            
014500                                                                              
014600 FD  DATER                                                                    
014700     RECORD CONTAINS 80 CHARACTERS                                            
014800     VALUE OF DEPENDENTSPECS IS TRUE                                          
014900        TITLE IS "*DATE/RECORD ON DISK".                                      
015000                                                                              
015100 01  DATE-REC.                                                                
015200     05 DATER-YR              PIC 9999.                                       
015300     05 DATER-MM              PIC 99.                                         
015400     05 DATER-DD              PIC 99.                                         
015500     05 FILLER                PIC X(72).                                      
015600                                                                              
015700******************************************************************            
015800*                          PRINT FILES                                        
015900******************************************************************            
016000                                                                              
016100 FD  PRINT-132.                                                               
016200                                                                              
016300 01  PRINT-REC                PIC X(132).                                     
016400                                                                              
016500                                                                              
016600******************************************************************            
016700******************************************************************            
016800*         W O R K I N G    S T O R A G E    S E C T I O N                     
016900******************************************************************            
017000******************************************************************            
017100                                                                              
017200 WORKING-STORAGE SECTION.                                                     
017300                                                                              
017400*UTILITIES                                                                    
017500 01  WS-UTILITIES.                                                            
017533     05 HOLD-UMB-COV-KEY    PIC X(14)           VALUE SPACES.                 
017566                                                                              
017600     05 WS-END-OF-MASTER    PIC 9               VALUE 0.                      
017700        88 END-OF-MASTER                        VALUE 1.                      
017800                                                                              
017900     05 WS-END-OF-FILE  PIC 9               VALUE 0.                         
018000        88 END-OF-FILE                      VALUE 1.                         
018100                                                                              
018200     05 WS-NSTAR-TAB.                                                         
018300        10 WS-NS-STATE-TAB OCCURS 8 TIMES.                                    
018400           15 WS-NS-LIA-TAB OCCURS 12 TIMES.                                  
018500              20 WS-NS-COUNT       PIC 9(5)     VALUE ZEROES.                 
018600                                                                              
018700     05 WS-NON-NSTAR-TAB.                                                     
018800        10 WS-NON-NS-STATE-TAB OCCURS 8 TIMES.                                
018900            15 WS-NON-NS-LIA-TAB OCCURS 12 TIMES.                             
018950               20 WS-NON-NS-COUNT  PIC 9(5)     VALUE ZEROES.                 
019000                                                                              
019033     05 WS-IDX              PIC 99              VALUE ZEROES.                 
019049                                                                              
019066     05 WS-STATE-IDX        PIC 9               VALUE ZERO.                   
019100                                                                              
019200     05 WS-CNT              PIC 99              VALUE ZEROES.                 
019300     05 WS-GRND-TOT         PIC 9(6)            VALUE ZEROES.                 
019350                                                                              
019400     05 WS-BLANK-LINE       PIC X(132)          VALUE SPACES.                 
019450                                                                              
019500 01  WS-LIAB-DESCS.                                                           
019506     05 WS-LIAB-DESC-TBL    PIC X(17) OCCURS 12 TIMES                         
019548                                INDEXED BY IDX-LIAB.                          
019590                                                                              
019600 01  WS-PL-HEADING-1.                                                         
019700     05 FILLER              PIC X(5)            VALUE SPACES.                 
019800     05 FILLER              PIC X(21)           VALUE                         
019900        "UMB - AUTO UNDERLYING".                                              
020000     05 FILLER              PIC X(12)           VALUE SPACES.                 
020100     05 FILLER              PIC X(9)            VALUE "RUN DATE:".            
020200     05 FILLER              PIC X(3)            VALUE SPACES.                 
020300     05 WS-PL-RUN-DATE      PIC Z9/99/99        VALUE ZEROES.                 
020400     05 FILLER              PIC X(10)           VALUE SPACES.                 
020500     05 FILLER              PIC X(6)            VALUE "AS OF:".               
020600     05 FILLER              PIC X(2)            VALUE SPACES.                 
020700     05 WS-PL-ASOF-DATE     PIC X(10)           VALUE SPACES.                 
020800     05 FILLER              PIC X(15)           VALUE SPACES.                 
020900     05 FILLER              PIC X(15)           VALUE                         
021000        "OBJ/UMB/AUTOUND".                                                
021100 01  WS-PL-UMB-TYP-HEADING.                                                   
021200     05 FILLER              PIC X(31)           VALUE SPACES.                 
021300     05 FILLER              PIC X(19)           VALUE                         
021400        "TYPE OF UMBRELLA:".                                                  
021500     05 FILLER              PIC X(2)            VALUE SPACES.                 
021600     05 WS-PL-UMB-TYPE      PIC X(4)            VALUE SPACES.                 
021700                                                                              
021800 01  WS-PL-NS-AUTO-HEADING.                                                   
021900     05 FILLER              PIC X(47)           VALUE SPACES.                 
022000     05 WS-PL-NS-AUTO-PRT   PIC X(16)           VALUE                         
022100        "NORTH STAR AUTO:".                                                   
022120                                                                              
022140 01  WS-PL-NON-NS-AUTO-HEADING.                                               
022160     05 FILLER              PIC X(47)           VALUE SPACES.                 
022180     05 WS-PL-NON-NS-AUTO-PRT   PIC X(20)           VALUE                     
022200        "NON-NORTH STAR AUTO:".                                               
022275                                                                              
022350 01  WS-STATE-HEADING.                                                        
022425     05 FILLER              PIC X(33)           VALUE SPACES.                 
022500     05 FILLER              PIC X(50)           VALUE                         
022600        "IA          MN          NE          ND          SD".                 
022700     05 FILLER              PIC X(37)           VALUE                         
022800        "          OK          WI        TOTAL".                              
022900                                                                              
023000 01  WS-PL-MAIN.                                                              
023100     05 FILLER              PIC X(6)            VALUE SPACES.                 
023200     05 WS-LIAB-DESC        PIC X(17)           VALUE SPACES.                 
023300     05 FILLER              PIC X(6)            VALUE SPACES.                 
023400     05 WS-PL-IA            PIC ZZ,ZZ9          VALUE ZEROES.                 
023500     05 FILLER              PIC X(6)            VALUE SPACES.                 
023600     05 WS-PL-MN            PIC ZZ,ZZ9          VALUE ZEROES.                 
023700     05 FILLER              PIC X(6)            VALUE SPACES.                 
023800     05 WS-PL-NE            PIC ZZ,ZZ9          VALUE ZEROES.                 
023900     05 FILLER              PIC X(6)            VALUE SPACES.                 
024000     05 WS-PL-ND            PIC ZZ,ZZ9          VALUE ZEROES.                 
024100     05 FILLER              PIC X(6)            VALUE SPACES.                 
024200     05 WS-PL-SD            PIC ZZ,ZZ9          VALUE ZEROES.                 
024300     05 FILLER              PIC X(6)            VALUE SPACES.                 
024400     05 WS-PL-OK            PIC ZZ,ZZ9          VALUE ZEROES.                 
024500     05 FILLER              PIC X(6)            VALUE SPACES.                 
024600     05 WS-PL-WI            PIC ZZ,ZZ9          VALUE ZEROES.                 
024700     05 FILLER              PIC X(7)            VALUE SPACES.                 
024800     05 WS-PL-TOTAL         PIC ZZ,ZZ9          VALUE ZEROES.                 
024900                                                                              
025000 01  WS-PL-TOTALS.                                                            
025100     05 FILLER              PIC X(6)            VALUE SPACES.                 
025200     05 WS-PL-TOT-DESC      PIC X(11)           VALUE SPACES.                 
025300     05 FILLER              PIC X(97)           VALUE SPACES.                 
025400     05 WS-PL-TOT-AMT       PIC ZZ,ZZ9          VALUE SPACES.                 
025500                                                                              
025600 01  WS-300K-CSL-COUNTERS.                                                    
025700     05 WS-IA-300K-CSL      PIC 9(5)            VALUE ZEROES.                 
025800     05 WS-MN-300K-CSL      PIC 9(5)            VALUE ZEROES.                 
025900     05 WS-NE-300K-CSL      PIC 9(5)            VALUE ZEROES.                 
026000     05 WS-ND-300K-CSL      PIC 9(5)            VALUE ZEROES.                 
026100     05 WS-SD-300K-CSL      PIC 9(5)            VALUE ZEROES.                 
026200     05 WS-OK-300K-CSL      PIC 9(5)            VALUE ZEROES.                 
026300     05 WS-WI-300K-CSL      PIC 9(5)            VALUE ZEROES.                 
026400     05 WS-TOT-300K-CSL     PIC 9(6)            VALUE ZEROES.                 
026500                                                                              
026600                                                                              
026700******************************************************************            
026800******************************************************************            
026900*            P R O C E D U R E   D I V I S I O N                              
027000******************************************************************            
027100******************************************************************            
027200 PROCEDURE DIVISION.                                                          
027300******************************************************************            
027400******************************************************************            
027500*         PROGRAM MAINLINER                                                   
027600******************************************************************            
027700******************************************************************            
027800 START-OUT.                                                                   
027900                                                                              
028000     PERFORM 1000-GET-IN THRU                                                 
028100             1099-GET-IN-X.                                                   
028200                                                                              
028300     PERFORM 3000-MASTER-CYCLE THRU                                           
028400             3099-MASTER-CYCLE-X                                              
028500                UNTIL END-OF-MASTER.                                          
028575                                                                              
028650     PERFORM 6000-PRINT THRU                                                  
028725             6099-PRINT-X.                                                    
028800                                                                              
029200     PERFORM 9000-CLOSING-UP THRU                                             
029300             9099-CLOSING-UP-X.                                               
029366                                                                              
029432     STOP RUN.                                                                
029466                                                                              
029500/                                                                             
029600 1000-GET-IN.                                                                 
029700                                                                              
029800     OPEN INPUT MASTER,                                                       
029900          INPUT FILE,                                                        
030000          INPUT DATER.                                                        
030100     OPEN OUTPUT PRINT-132.                                                   
030106                                                                              
030112     MOVE SPACES TO WS-PL-UMB-TYPE.                                           
030118     IF SW1-ON                                                                
030124        MOVE "PERS" TO WS-PL-UMB-TYPE                                         
030136     ELSE                                                                     
030142     IF SW2-ON                                                                
030148        MOVE "FARM" TO WS-PL-UMB-TYPE                                         
030160     ELSE                                                                     
030166     IF SW3-ON                                                                
030172        MOVE "COMM" TO WS-PL-UMB-TYPE                                         
030178     ELSE                                                                     
030184        MOVE "ALL " TO WS-PL-UMB-TYPE.                                        
030187                                                                              
030190     ACCEPT WS-PL-RUN-DATE FROM TODAYS-DATE.                                  
030216                                                                              
030220     READ DATER                                                               
030224     END-READ.                                                                
030232                                                                              
030240     STRING DATER-MM "/" DATER-DD, "/" DATER-YR DELIMITED BY                  
030244            SIZE INTO WS-PL-ASOF-DATE.                                        
030271                                                                              
030300 1099-GET-IN-X.                                                               
030400     EXIT.                                                                    
030406                                                                              
030412                                                                              
030418 3000-MASTER-CYCLE.                                                           
030427                                                                              
030430     READ MASTER NEXT RECORD                                                  
030436        AT END MOVE 1 TO WS-END-OF-MASTER                                     
030442               GO TO 3099-MASTER-CYCLE-X.                                     
030445                                                                              
030447                                                                              
030449     PERFORM 3200-START-FILE THRU                                            
030451             3299-START-FILE-X.                                              
030453                                                                              
030455                                                                              
030457 3099-MASTER-CYCLE-X.                                                         
030459     EXIT.                                                                    
030461                                                                              
030463 3200-START-FILE.                                                            
030465                                                                              
030466     MOVE 0 TO WS-END-OF-FILE.                                               
030467     MOVE SPACES TO HOLD-UMB-COV-KEY.                                         
030478     STRING UMB-INF-KEY "J    " DELIMITED BY                                  
030479            SIZE INTO HOLD-UMB-COV-KEY.                                       
030480                                                                              
030481     START FILE                                                              
030482        KEY IS NOT LESS THAN HOLD-UMB-COV-KEY                                 
030484            INVALID KEY                                                       
030486                DISPLAY "INVALID UMB-FILE START " UMB-COV-KEY                
030488                GO TO 3299-START-FILE-X.                                     
030489                                                                              
030490     PERFORM 3250-FILE-READER THRU                                           
030492             3259-FILE-READER-X                                              
030494                UNTIL END-OF-FILE.                                           
030496                                                                              
030501                                                                              
030504 3299-START-FILE-X.                                                          
030505     EXIT.                                                                    
030506                                                                              
030507 3250-FILE-READER.                                                           
030508                                                                              
030509     READ FILE NEXT RECORD                                                   
030510          AT END MOVE 1 TO WS-END-OF-FILE                                    
030511                GO TO 3259-FILE-READER-X.                                    
030512                                                                              
030513     IF UMB-INF-POLICY NOT = UMB-COV-POLICY                                   
030514        MOVE 1 TO WS-END-OF-FILE                                             
030515        GO TO 3259-FILE-READER-X.                                            
030516                                                                              
030517     IF NOT UMB-COV-UNDERLYING                                                
030519        GO TO 3259-FILE-READER-X.                                            
030520                                                                              
030521     IF UMB-COV-LIM-BRKDWN NOT = "01"                                         
030522        GO TO 3259-FILE-READER-X.                                            
030523                                                                              
030524     IF UMB-COV-UND-TYPE(1:1) NOT = "A"                                       
030525        GO TO 3259-FILE-READER-X.                                            
030526                                                                              
030527     MOVE SPACES TO WS-PL-UMB-TYPE.                                           
030528                                                                              
030529     IF SW1-ON                                                                
030530        IF UMB-COV-UND-TYPE NOT = "P"                                         
030531            GO TO 3259-FILE-READER-X                                         
030532        END-IF                                                                
030533     ELSE                                                                     
030534     IF SW2-ON                                                                
030539        IF UMB-COV-UND-TYPE NOT = "F"                                         
030544            GO TO 3259-FILE-READER-X                                         
030549        END-IF                                                                
030554     ELSE                                                                     
030559     IF SW3-ON                                                                
030564        IF UMB-COV-UND-TYPE NOT = "C"                                         
030569            GO TO 3259-FILE-READER-X.                                        
030576                                                                              
030583                                                                              
030590     EVALUATE UMB-COV-STATE                                                   
030597        WHEN "14" MOVE 1 TO WS-STATE-IDX                                      
030604        WHEN "22" MOVE 2 TO WS-STATE-IDX                                      
030611        WHEN "26" MOVE 3 TO WS-STATE-IDX                                      
030618        WHEN "33" MOVE 4 TO WS-STATE-IDX                                      
030625        WHEN "35" MOVE 5 TO WS-STATE-IDX                                      
030632        WHEN "40" MOVE 6 TO WS-STATE-IDX                                      
030639        WHEN "48" MOVE 7 TO WS-STATE-IDX                                      
030646     END-EVALUATE.                                                            
030653                                                                              
030660     IF UMB-COV-NORTH-STAR                                                    
030669                                                                              
030670        IF WS-STATE-IDX > 0                                                   
030671            PERFORM 3300-CHECK-LIMITS THRU                                    
030672                    3399-CHECK-LIMITS-X                                       
030673        END-IF                                                                
030675        MOVE 8 TO WS-STATE-IDX                          *>TOTAL               
030676        PERFORM 3300-CHECK-LIMITS THRU                                        
030677                3399-CHECK-LIMITS-X                                           
030678     ELSE                                                                     
030679     IF NOT UMB-COV-NORTH-STAR                                                
030680        IF WS-STATE-IDX > 0                                                   
030681            PERFORM 3400-CHECK-NON-NS-LIMITS THRU                             
030682                    3499-CHECK-NON-NS-LIMITS-X                                
030683        END-IF                                                                
030684        MOVE 8 TO WS-STATE-IDX                          *>TOTAL               
030685        PERFORM 3400-CHECK-NON-NS-LIMITS THRU                                 
030686                3499-CHECK-NON-NS-LIMITS-X                                    
030687     END-IF.                                                                  
030688                                                                              
030689 3259-FILE-READER-X.                                                         
030690     EXIT.                                                                    
030691                                                                              
030692                                                                              
030693 3300-CHECK-LIMITS.                                                           
030695                                                                              
030697    IF (WS-STATE-IDX >= 1) AND (WS-STATE-IDX <= 8 )                           
030699                                                                              
030701         ADD 1 TO WS-NS-COUNT(WS-STATE-IDX, 12)    *>>>STATE TOTAL            
030703                                                                              
030705         MOVE 0 TO WS-CNT                                                     
030707         INSPECT UMB-COV-UND-LIMITS                                           
030709            TALLYING WS-CNT FOR ALL  "300CSL" "300 CSL"                       
030711          IF WS-CNT > 0                                                       
030713              ADD 1 TO WS-NS-COUNT(WS-STATE-IDX, 1)                           
030715          ELSE                                                                
030717        INSPECT UMB-COV-UND-LIMITS                                            
030719            TALLYING WS-CNT FOR ALL  "500CSL" "500 CSL"                       
030721         IF WS-CNT > 0                                                        
030723             ADD 1 TO WS-NS-COUNT(WS-STATE-IDX, 2)                            
030725        ELSE                                                                  
030727        INSPECT UMB-COV-UND-LIMITS                                            
030729           TALLYING WS-CNT FOR ALL  "750CSL" "750 CSL"                        
030731        IF WS-CNT > 0                                                         
030733            ADD 1 TO WS-NS-COUNT(WS-STATE-IDX, 3)                             
030735        ELSE                                                                  
030737        INSPECT UMB-COV-UND-LIMITS                                            
030739           TALLYING WS-CNT FOR ALL  "1000CSL" "1000 CSL"                      
030741                                    "1,000CSL" "1000000"                      
030743                                    "1,000,000"                               
030745        IF WS-CNT > 0                                                         
030747            ADD 1 TO WS-NS-COUNT(WS-STATE-IDX, 4)                             
030749        ELSE                                                                  
030751        INSPECT UMB-COV-UND-LIMITS                                            
030753           TALLYING WS-CNT FOR ALL  "250/500"                                 
030755        IF WS-CNT > 0                                                         
030757            ADD 1 TO WS-NS-COUNT(WS-STATE-IDX, 5)                             
030759        ELSE                                                                  
030761        INSPECT UMB-COV-UND-LIMITS                                            
030763           TALLYING WS-CNT FOR ALL  "300/300"                                 
030765        IF WS-CNT > 0                                                         
030767            ADD 1 TO WS-NS-COUNT(WS-STATE-IDX, 6)                             
030769        ELSE                                                                  
030771        INSPECT UMB-COV-UND-LIMITS                                            
030773           TALLYING WS-CNT FOR ALL  "500/500"                                 
030775        IF WS-CNT > 0                                                         
030777            ADD 1 TO WS-NS-COUNT(WS-STATE-IDX, 7)                             
030779        ELSE                                                                  
030781        INSPECT UMB-COV-UND-LIMITS                                            
030783           TALLYING WS-CNT FOR ALL  "300/500"                                 
030785        IF WS-CNT > 0                                                         
030787            ADD 1 TO WS-NS-COUNT(WS-STATE-IDX, 8)                             
030788        ELSE                                                                  
030789        INSPECT UMB-COV-UND-LIMITS                                            
030790           TALLYING WS-CNT FOR ALL  "500/1000"                                
030791        IF WS-CNT > 0                                                         
030792            ADD 1 TO WS-NS-COUNT(WS-STATE-IDX, 9)                             
030793        ELSE                                                                  
030794        INSPECT UMB-COV-UND-LIMITS                                            
030795           TALLYING WS-CNT FOR ALL  "1000/1000"                               
030796        IF WS-CNT > 0                                                         
030797            ADD 1 TO WS-NS-COUNT(WS-STATE-IDX, 10)                            
030798        ELSE                                                                  
030799            DISPLAY "*****************************"                           
030800            DISPLAY "UNKNOWN UNDL: " UMB-COV-UND-LIMITS                       
030801            DISPLAY "POLICY:       " UMB-COV-POLICY                           
030802            DISPLAY "*****************************"                           
030803            ADD 1 TO WS-NS-COUNT(WS-STATE-IDX, 11)                            
030804                                                                              
030805     ELSE                                                                     
030806        DISPLAY "INVALID STATE IDX".                                          
030813                                                                              
030814 3399-CHECK-LIMITS-X.                                                         
030815     EXIT.                                                                    
030816                                                                              
030817                                                                              
030818                                                                              
030819 3400-CHECK-NON-NS-LIMITS.                                                    
030820                                                                              
030821     IF (WS-STATE-IDX >= 1) AND (WS-STATE-IDX <= 8 )                          
030822                                                                              
030823         ADD 1 TO WS-NON-NS-COUNT(WS-STATE-IDX, 12)   *>STATE TOT             
030825                                                                              
030826         MOVE 0 TO WS-CNT                                                     
030827         INSPECT UMB-COV-UND-LIMITS                                           
030828            TALLYING WS-CNT FOR ALL  "300CSL" "300 CSL"                       
030829          IF WS-CNT > 0                                                       
030830              ADD 1 TO WS-NON-NS-COUNT(WS-STATE-IDX, 1)                       
030831          ELSE                                                                
030832        INSPECT UMB-COV-UND-LIMITS                                            
030833            TALLYING WS-CNT FOR ALL  "500CSL" "500 CSL"                       
030834         IF WS-CNT > 0                                                        
030835             ADD 1 TO WS-NON-NS-COUNT(WS-STATE-IDX, 2)                        
030836        ELSE                                                                  
030837        INSPECT UMB-COV-UND-LIMITS                                            
030838           TALLYING WS-CNT FOR ALL  "750CSL" "750 CSL"                        
030839        IF WS-CNT > 0                                                         
030840            ADD 1 TO WS-NON-NS-COUNT(WS-STATE-IDX, 3)                         
030841        ELSE                                                                  
030842        INSPECT UMB-COV-UND-LIMITS                                            
030843           TALLYING WS-CNT FOR ALL  "1000CSL" "1000 CSL"                      
030844                                    "1,000CSL" "1000000"                      
030845                                    "1,000,000"                               
030846        IF WS-CNT > 0                                                         
030847            ADD 1 TO WS-NON-NS-COUNT(WS-STATE-IDX, 4)                         
030848        ELSE                                                                  
030849        INSPECT UMB-COV-UND-LIMITS                                            
030850           TALLYING WS-CNT FOR ALL  "250/500"                                 
030851        IF WS-CNT > 0                                                         
030852            ADD 1 TO WS-NON-NS-COUNT(WS-STATE-IDX, 5)                         
030853        ELSE                                                                  
030854        INSPECT UMB-COV-UND-LIMITS                                            
030855           TALLYING WS-CNT FOR ALL  "300/300"                                 
030856        IF WS-CNT > 0                                                         
030857            ADD 1 TO WS-NON-NS-COUNT(WS-STATE-IDX, 6)                         
030858        ELSE                                                                  
030859        INSPECT UMB-COV-UND-LIMITS                                            
030860           TALLYING WS-CNT FOR ALL  "500/500"                                 
030861        IF WS-CNT > 0                                                         
030862            ADD 1 TO WS-NON-NS-COUNT(WS-STATE-IDX, 7)                         
030863        ELSE                                                                  
030864        INSPECT UMB-COV-UND-LIMITS                                            
030865           TALLYING WS-CNT FOR ALL  "300/500"                                 
030866        IF WS-CNT > 0                                                         
030867            ADD 1 TO WS-NON-NS-COUNT(WS-STATE-IDX, 8)                         
030868        ELSE                                                                  
030869        INSPECT UMB-COV-UND-LIMITS                                            
030870           TALLYING WS-CNT FOR ALL  "500/1000"                                
030871        IF WS-CNT > 0                                                         
030872            ADD 1 TO WS-NON-NS-COUNT(WS-STATE-IDX, 9)                         
030873        ELSE                                                                  
030874        INSPECT UMB-COV-UND-LIMITS                                            
030875           TALLYING WS-CNT FOR ALL  "1000/1000"                               
030876        IF WS-CNT > 0                                                         
030877            ADD 1 TO WS-NON-NS-COUNT(WS-STATE-IDX, 10)                        
030878        ELSE                                                                  
030879            DISPLAY "*****************************"                           
030880            DISPLAY "UNKNOWN UNDL: " UMB-COV-UND-LIMITS                       
030881            DISPLAY "POLICY:       " UMB-COV-POLICY                           
030882            DISPLAY "*****************************"                           
030883            ADD 1 TO WS-NON-NS-COUNT(WS-STATE-IDX, 11)                        
030884                                                                              
030885     ELSE                                                                     
030886        DISPLAY "INVALID STATE IDX".                                          
030887                                                                              
030888                                                                              
030889                                                                              
030890 3499-CHECK-NON-NS-LIMITS-X.                                                  
030891     EXIT.                                                                    
030892                                                                              
030893                                                                              
030894 6000-PRINT.                                                                  
030895                                                                              
030896     PERFORM 6200-SETUP-DESC-TAB THRU                                         
030897             6299-SETUP-DESC-TAB-X.                                           
030898                                                                              
030900     WRITE PRINT-REC FROM WS-PL-HEADING-1 BEFORE ADVANCING 3.                 
031000     WRITE PRINT-REC FROM WS-PL-UMB-TYP-HEADING BEFORE                        
031100            ADVANCING 3.                                                      
031200     WRITE PRINT-REC FROM WS-PL-NS-AUTO-HEADING BEFORE                        
031300            ADVANCING 2.                                                      
031400     WRITE PRINT-REC FROM WS-STATE-HEADING BEFORE ADVANCING 2.                
032633                                                                              
032666                                                                              
032700     PERFORM VARYING WS-IDX                                                   
032705        FROM 1 BY 1                                                           
032710            UNTIL WS-IDX > 12                                                 
032715            MOVE WS-LIAB-DESC-TBL(WS-IDX) TO WS-LIAB-DESC                     
032720            MOVE WS-NS-COUNT(1, WS-IDX) TO WS-PL-IA                           
032725            MOVE WS-NS-COUNT(2, WS-IDX) TO WS-PL-MN                           
032730            MOVE WS-NS-COUNT(3, WS-IDX) TO WS-PL-NE                           
032735            MOVE WS-NS-COUNT(4, WS-IDX) TO WS-PL-ND                           
032740            MOVE WS-NS-COUNT(5, WS-IDX) TO WS-PL-OK                           
032745            MOVE WS-NS-COUNT(6, WS-IDX) TO WS-PL-SD                           
032750            MOVE WS-NS-COUNT(7, WS-IDX) TO WS-PL-WI                           
032755            MOVE WS-NS-COUNT(8, WS-IDX) TO WS-PL-TOTAL                        
032760            IF WS-IDX = 12                                                    
032765                WRITE PRINT-REC FROM WS-PL-MAIN AFTER 2                       
032770            ELSE                                                              
032775                WRITE PRINT-REC FROM WS-PL-MAIN AFTER 1                       
032777            END-IF                                                            
032780     END-PERFORM.                                                             
032782                                                                              
032785     MOVE WS-NS-COUNT(8, 12) TO WS-PL-TOT-AMT.                                
032787     MOVE "TOTALS     " TO WS-PL-TOT-DESC.                                    
032789     WRITE PRINT-REC FROM WS-PL-TOTALS AFTER ADVANCING 3.                     
032792                                                                              
032795*** PRINT FOR  NON NORTHSTAR ***                                              
032798     WRITE PRINT-REC FROM WS-PL-NON-NS-AUTO-HEADING AFTER                     
032801            ADVANCING 3.                                                      
032804     WRITE PRINT-REC FROM WS-STATE-HEADING AFTER ADVANCING 2.                 
032805     WRITE PRINT-REC FROM WS-BLANK-LINE AFTER ADVANCING 2.                    
032807                                                                              
032810                                                                              
032813     PERFORM VARYING WS-IDX                                                   
032816        FROM 1 BY 1                                                           
032819            UNTIL WS-IDX > 12                                                 
032822            MOVE WS-LIAB-DESC-TBL(WS-IDX) TO WS-LIAB-DESC                     
032825            MOVE WS-NON-NS-COUNT(1, WS-IDX) TO WS-PL-IA                       
032828            MOVE WS-NON-NS-COUNT(2, WS-IDX) TO WS-PL-MN                       
032831            MOVE WS-NON-NS-COUNT(3, WS-IDX) TO WS-PL-NE                       
032834            MOVE WS-NON-NS-COUNT(4, WS-IDX) TO WS-PL-ND                       
032837            MOVE WS-NON-NS-COUNT(5, WS-IDX) TO WS-PL-OK                       
032840            MOVE WS-NON-NS-COUNT(6, WS-IDX) TO WS-PL-SD                       
032843            MOVE WS-NON-NS-COUNT(7, WS-IDX) TO WS-PL-WI                       
032846            MOVE WS-NON-NS-COUNT(8, WS-IDX) TO WS-PL-TOTAL                    
032849            IF WS-IDX = 12                                                    
032852                WRITE PRINT-REC FROM WS-PL-MAIN AFTER 2                       
032855            ELSE                                                              
032858                WRITE PRINT-REC FROM WS-PL-MAIN AFTER 1                       
032861            END-IF                                                            
032864     END-PERFORM.                                                             
032867                                                                              
032870     MOVE WS-NON-NS-COUNT(8, 12) TO WS-PL-TOT-AMT.                            
032873     MOVE "TOTALS     " TO WS-PL-TOT-DESC.                                    
032876     WRITE PRINT-REC FROM WS-PL-TOTALS AFTER ADVANCING 3.                     
032877                                                                              
032879     ADD WS-NS-COUNT(8, 12)   TO WS-GRND-TOT.                                 
032880     ADD WS-NON-NS-COUNT(8, 12) TO WS-GRND-TOT.                               
032881     MOVE WS-GRND-TOT TO WS-PL-TOT-AMT.                                       
032882     MOVE "GRAND TOTAL" TO WS-PL-TOT-DESC.                                    
032885     WRITE PRINT-REC FROM WS-PL-TOTALS AFTER ADVANCING 2.                     
032888                                                                              
032891 6099-PRINT-X.                                                                
032900     EXIT.                                                                    
033000                                                                              
033100 6200-SETUP-DESC-TAB.                                                         
033105                                                                              
033110     MOVE "300,000 CSL      " TO WS-LIAB-DESC-TBL(1).                         
033115     MOVE "500,000 CSL      " TO WS-LIAB-DESC-TBL(2).                         
033120     MOVE "750,000 CSL      " TO WS-LIAB-DESC-TBL(3).                         
033125     MOVE "1,000,000 CSL    " TO WS-LIAB-DESC-TBL(4).                         
033130     MOVE "250/500          " TO WS-LIAB-DESC-TBL(5).                         
033135     MOVE "300/300          " TO WS-LIAB-DESC-TBL(6).                         
033140     MOVE "500/500          " TO WS-LIAB-DESC-TBL(7).                         
033145     MOVE "300/500          " TO WS-LIAB-DESC-TBL(8).                         
033150     MOVE "500/1000         " TO WS-LIAB-DESC-TBL(9).                         
033155     MOVE "1000/1000        " TO WS-LIAB-DESC-TBL(10).                        
033160     MOVE "OTHER MISC LIMITS" TO WS-LIAB-DESC-TBL(11).                        
033165     MOVE "STATE TOTALS     " TO WS-LIAB-DESC-TBL(12).                        
033170                                                                              
033175 6299-SETUP-DESC-TAB-X.                                                       
033180     EXIT.                                                                    
033200                                                                              
033300 9000-CLOSING-UP.                                                             
033400                                                                              
033500     CLOSE MASTER WITH RELEASE,                                               
033600           FILE WITH RELEASE,                                                
033700           DATER WITH RELEASE.                                                
033800     CLOSE PRINT-132 WITH SAVE.                                               
033900                                                                              
034000 9099-CLOSING-UP-X.                                                           
034100     EXIT.                                                                    
034200                                                                              
034300                                                                              
034400                                                                              
034500                                                                              
034600                                                                              
034700**************************************************************                
034800**************************************************************                
