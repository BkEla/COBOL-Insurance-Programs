$$SET LINEINFO
******************************************************************
******************************************************************
*                                                                *
*              PROGRAM NAME:BEA/CONVERT/FTR/RCCTEST
******************************************************************
*   DESCRIPTION 
*
*   A BATCH CONVERSION PROGRAM THAT READS SEQUENTIAL POLICY UPLOAD 
*   RECORDS, VALIDATES AND REFORMATS POLICY NUMBERS, RETRIEVES 
*   MATCHING INDEXED MASTER RECORDS, PRINTS COMPANION AUTO    
*   AND UMBRELLA POLICY REPORTS, MIGRATES QUALIFYING POLICIES TO A 
*   NEW AGENCY CODE, UPDATES RELATED EXPOSURE RECORDS, DELETES 
*   OBSOLETE RECORDS, AND MARKS PROCESSED TRANSACTIONS IN THE 
*   INPUT FILE.
*
*
*  FILES:
*         *PKG/FTR/UPLD      ON DISK
*         *PKG/FILE/KIO      ON DISK
*         *EXP/FILE/KIO      ON DISK
*
*
*
******************************************************************
******************************************************************

 IDENTIFICATION DIVISION.

 PROGRAM-ID.             RCCTEST.

*AUTHOR.                 BIRUK AYALEW

*DATE-WRITTEN.           09/11/2023

*CHANGE LOG:
*
******************************************************************
******************************************************************
*         E N V I R O N M E N T       D I V I S I O N            *
******************************************************************
******************************************************************

 ENVIRONMENT DIVISION.

******************************************************************
   CONFIGURATION SECTION.
******************************************************************

   SOURCE-COMPUTER.    CS7101.
   OBJECT-COMPUTER.    CS7101.


******************************************************************
   INPUT-OUTPUT SECTION.
******************************************************************

   FILE-CONTROL.

******************************************************************
*                SELECT MONTH-END FILE                           *
******************************************************************
     SELECT                       PKMAINFO
          ASSIGN TO               DISK
          ORGANIZATION IS         INDEXED
          ACCESS IS               RANDOM
          RECORD KEY IS           PKMA-INFO-KEY.

******************************************************************
     SELECT                       FTRRECORD
          ASSIGN TO               DISK
          ORGANIZATION IS         SEQUENTIAL
          ACCESS IS               SEQUENTIAL.

******************************************************************
     SELECT                       EXPMAST
          ASSIGN TO               DISK
          ORGANIZATION IS         INDEXED
          ACCESS IS               DYNAMIC
          RECORD KEY IS           EXP-KEY.
******************************************************************
*                       SELECT PRINT FILE 1
******************************************************************
     SELECT                       PRINT-RECORD-AUTO
         ASSIGN TO                PRINTER.

******************************************************************
*                       SELECT PRINT FILE 2
******************************************************************
     SELECT                       PRINT-RECORD-UMB
         ASSIGN TO                PRINTER.

******************************************************************
******************************************************************
*                   D A T A   D I V I S I O N                    *
******************************************************************
******************************************************************

 DATA DIVISION.

 FILE SECTION.

******************************************************************
*                         PKG  FILE                              *
******************************************************************

 COPY "(SCR)CFD/PKG/MAST ON DISK" FROM 1200.

******************************************************************
*                         EXPMAST FILE                           *
******************************************************************

 COPY "(SCR)CFD/EXP ON DISK".

******************************************************************
*                         FTRRECORD FILE                         *
******************************************************************

 COPY "(SCR)CFD/FTR/UPLD ON DISK" FROM 1000.

******************************************************************
*                          PRINT FILES                           *
******************************************************************
*            PRINT FOR AUTO COMPANION POLICIES
******************************************************************
 FD  PRINT-RECORD-AUTO
     RECORD CONTAINS 132 CHARACTERS.

 01  P-REC-AUTO                        PIC X(132).

******************************************************************
*            PRINT FOR UMBRELLA POLICIES
******************************************************************
 FD  PRINT-RECORD-UMB
     RECORD CONTAINS 132 CHARACTERS.

 01  P-REC-UMB                         PIC X(132).

******************************************************************
******************************************************************
*         W O R K I N G    S T O R A G E    S E C T I O N        *
******************************************************************
******************************************************************

 WORKING-STORAGE SECTION.

 01  WS-END-OF-UPLD-FILE               PIC 9         VALUE ZEROES.
     88 END-OF-UPLD-FILE                             VALUE 1.

 01  WS-PRT-AUTO-FL-OPN                PIC 9         VALUE ZERO.
     88 PRT-AUTO-FL-OPN                              VALUE 1.
 01  WS-PRT-UMB-FL-OPN                 PIC 9         VALUE ZERO.
     88 PRT-UMB-FL-OPN                               VALUE 1.

 01  WS-PKG-POL-FOUND                  PIC 9         VALUE ZERO.
     88 PKG-POL-FOUND                                VALUE 1.

 01  WS-EXP-POL-FOUND                  PIC 9         VALUE ZERO.
     88 EXP-POL-FOUND                                VALUE 1.

 01  WS-PG-AUTO-NUM                    PIC 999       VALUE ZEROES.
 01  WS-PG-UMB-NUM                     PIC 999       VALUE ZEROES.

 01  WS-PRT-AUTO-LNS                   PIC 99        VALUE ZEROES.
 01  WS-PRT-UMB-LNS                    PIC 99        VALUE ZEROES.

 01  WS-POLICY-NUMBER                  PIC X(12)     VALUE SPACES.

 01  WS-PKG-POL-KEY REDEFINES WS-POLICY-NUMBER.
     05 WS-GAGT-NUM                    PIC X(4).
     05 WS-POL-TYPE                    PIC X.
     05 WS-POL-NUM                     PIC X(7).

 01  WS-EXP-POL-KEY.
     05 WS-EXP-POL.
        10 WS-EXP-GAGT                 PIC X(4).
        10 WS-EXP-POLNUM               PIC X(8).

 01  WS-CURRENT-DATE.
     05 WS-CURRENT-MO                  PIC 99.
     05 WS-CURRENT-DA                  PIC 99.
     05 WS-CURRENT-YR                  PIC 9(4).

 01  WS-HEADING-1.
     05 FILLER                         PIC X(6)      VALUE SPACES.
     05 FILLER                         PIC X(10)     VALUE
        "RUN DATE: ".
     05 WS-PRT-CURR-DATE               PIC Z9/99/99  VALUE ZEROES.
     05 FILLER                         PIC X(10)     VALUE SPACES.
     05 WS-HEADING-1-DTL               PIC X(50)     VALUE SPACES.
     05 FILLER                         PIC X(5)     VALUE SPACES.
     05 FILLER                         PIC X(25)     VALUE
        "(BEA/FTR/CONVERT/RCCTEST)".
     05 FILLER                         PIC X(10)     VALUE SPACES.
     05 FILLER                         PIC X(5)      VALUE
        "PAGE ".
     05 WS-PG-NUM-PRT                  PIC ZZ9       VALUE ZEROES.

 01  WS-AUTO-HEAD-2.
     05 FILLER                         PIC X(6)      VALUE SPACES.
     05 FILLER                         PIC X(10)     VALUE
        "PKG POLICY".
     05 FILLER                         PIC X(45)     VALUE SPACES.
     05 FILLER                         PIC X(6)      VALUE
        "POLICY".
     05 FILLER                         PIC X(8)      VALUE SPACES.
     05 FILLER                         PIC X(9)      VALUE
        "COMPANION".
     05 FILLER                         PIC X(6)      VALUE SPACES.
     05 FILLER                         PIC X(11)     VALUE
        "AUTO POLICY".
     05 FILLER                         PIC X(4)      VALUE SPACES.
     05 FILLER                         PIC X(11)     VALUE
        "AUTO POLICY".
     05 FILLER                         PIC X(4)      VALUE SPACES.
     05 FILLER                         PIC X(11)     VALUE
        "AUTO POLICY".
     05 FILLER                         PIC X(4)      VALUE SPACES.
*    05 FILLER                         PIC X(9)      VALUE
*       "AUTO COMP".

 01  WS-AUTO-HEAD-3.
     05 FILLER                         PIC X(8)      VALUE SPACES.
     05 FILLER                         PIC X(6)      VALUE
        "NUMBER".
     05 FILLER                         PIC X(9)      VALUE SPACES.
     05 FILLER                         PIC X(12)     VALUE
        "INSURED NAME".
     05 FILLER                         PIC X(24)     VALUE SPACES.
     05 FILLER                         PIC X(10)     VALUE
        "EFFEC DATE".
     05 FILLER                         PIC X(4)      VALUE SPACES.
     05 FILLER                         PIC X(13)     VALUE
        "POLICY DISCNT".
     05 FILLER                         PIC X(6)      VALUE SPACES.
     05 FILLER                         PIC X(6)     VALUE
        "NUMBER".
     05 FILLER                         PIC X(9)      VALUE SPACES.
     05 FILLER                         PIC X(6)      VALUE
        "STATUS".
     05 FILLER                         PIC X(10)     VALUE SPACES.
     05 FILLER                         PIC X(4)      VALUE
        "TYPE".
     05 FILLER                         PIC X(6)      VALUE SPACES.
*    05 FILLER                         PIC X(4)      VALUE "FLAG".
*    05 FILLER                         PIC X(3)      VALUE SPACES.

 01  WS-UMB-HEAD-2.
     05 FILLER                         PIC X(6)      VALUE SPACES.
     05 FILLER                         PIC X(10)     VALUE
        "PKG POLICY".
     05 FILLER                         PIC X(45)     VALUE SPACES.
     05 FILLER                         PIC X(6)      VALUE
        "POLICY".
     05 FILLER                         PIC X(8)      VALUE SPACES.
     05 FILLER                         PIC X(8)      VALUE
        "UMBRELLA".
     05 FILLER                         PIC X(6)      VALUE SPACES.

 01  WS-UMB-HEAD-3.
     05 FILLER                         PIC X(8)      VALUE SPACES.
     05 FILLER                         PIC X(6)      VALUE
        "NUMBER".
     05 FILLER                         PIC X(10)     VALUE SPACES.
     05 FILLER                         PIC X(12)     VALUE
        "INSURED NAME".
     05 FILLER                         PIC X(25)     VALUE SPACES.
     05 FILLER                         PIC X(10)     VALUE
        "EFFEC DATE".
     05 FILLER                         PIC X(4)      VALUE SPACES.
     05 FILLER                         PIC X(13)     VALUE
        "POLICY NUMBER".

 01  WS-REC-AUTO-COMP.
     05 FILLER                         PIC X(5)      VALUE SPACES.
     05 WS-APKG-POLNUM                 PIC X(12)     VALUE SPACES.
     05 FILLER                         PIC X(4)      VALUE SPACES.
     05 WS-AINS-NAME                   PIC X(36)     VALUE SPACES.
     05 FILLER                         PIC X(2)      VALUE SPACES.
     05 WS-AEFF-DATE.
        10 WS-AEFF-DATE-YYYY           PIC 9(4)      VALUE ZEROES.
        10 FILLER                      PIC X         VALUE "/".
        10 WS-AEFF-DATE-MM             PIC 9(2)      VALUE ZEROES.
        10 FILLER                      PIC X         VALUE "/".
        10 WS-AEFF-DATE-DD             PIC 9(2)      VALUE ZEROES.
     05 FILLER                         PIC X(10)     VALUE SPACES.
     05 WS-AUTO-POL-DSCNT              PIC X         VALUE SPACE.
     05 FILLER                         PIC X(11)     VALUE SPACES.
     05 WS-AUTO-POLNUM                 PIC X(9)      VALUE SPACES.
     05 FILLER                         PIC X(10)     VALUE SPACES.
     05 WS-AUTO-STATUS                 PIC X(1)      VALUE SPACES.
     05 FILLER                         PIC X(14)     VALUE SPACES.
     05 WS-AUTO-TYPE                   PIC X(1)      VALUE SPACES.
     05 FILLER                         PIC X(13)     VALUE SPACES.
*    05 WS-AUTO-COMP-F                 PIC X         VALUE SPACE.
*    05 FILLER                         PIC X(4)      VALUE SPACES.


 01  WS-REC-UMB-COMP.
     05 FILLER                         PIC X(5)      VALUE SPACES.
     05 WS-UPKG-POLNUM                 PIC X(12)     VALUE SPACES.
     05 FILLER                         PIC X(4)      VALUE SPACES.
     05 WS-UINS-NAME                   PIC X(36)     VALUE SPACES.
     05 FILLER                         PIC X(4)      VALUE SPACES.
     05 WS-UEFF-DATE.
        10 WS-UEFF-DATE-YYYY           PIC 9(4)      VALUE ZEROES.
        10 FILLER                      PIC X         VALUE "/".
        10 WS-UEFF-DATE-MM             PIC 9(2)      VALUE ZEROES.
        10 FILLER                      PIC X         VALUE "/".
        10 WS-UEFF-DATE-DD             PIC 9(2)      VALUE ZEROES.
     05 FILLER                         PIC X(7)      VALUE SPACES.
     05 WS-UMB-POL-NUMBER              PIC X(7)      VALUE ZERO.

******************************************************************
******************************************************************
*            P R O C E D U R E   D I V I S I O N                 *
******************************************************************
******************************************************************

 PROCEDURE DIVISION.

***************************************************************
*                          START UP                           *
***************************************************************
 1000-START-UP.

     PERFORM 1000-OPENING-PROCEDURE THRU
             1099-OPENING-PROCEDURE-X.

     PERFORM 3000-READ-UPLD THRU
             3099-READ-UPLD-X
                  UNTIL END-OF-UPLD-FILE.

     PERFORM 7000-CLOSING-PROCEDURE THRU
             7099-CLOSING-PROCEDURE-X.

     STOP RUN.
***************************************************************
*                     OPENING PROCEDURE                       *
***************************************************************
 1000-OPENING-PROCEDURE.

     OPEN I-O PKMAINFO.
     OPEN I-O FTRRECORD.
     OPEN I-O EXPMAST.

     ACCEPT WS-PRT-CURR-DATE FROM TODAYS-DATE.
     ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.

 1099-OPENING-PROCEDURE-X.
     EXIT.
***************************************************************
*                      READ UPLD                              *
***************************************************************
 3000-READ-UPLD.

     READ FTRRECORD NEXT RECORD
        AT END
          MOVE 1 TO WS-END-OF-UPLD-FILE
          GO TO 3099-READ-UPLD-X.

     IF FTR-I-ENTRY = "N"
        CONTINUE
     ELSE
        GO TO 3099-READ-UPLD-X.

     IF FTR-GENERAL-AGENCY-NO= " 784"
        CONTINUE
     ELSE
        GO TO 3099-READ-UPLD-X.

     IF REC-10
        CONTINUE
     ELSE
         GO TO 3099-READ-UPLD-X.

     PERFORM 3500-CREATE-POLICY-KEY THRU
             3599-CREATE-POLICY-KEY-X.

     MOVE " 806" TO WS-GAGT-NUM
     MOVE WS-PKG-POL-KEY TO PKMA-INFO-KEY.
     MOVE ZERO TO WS-PKG-POL-FOUND.

     PERFORM 4000-READ-PKG-MAST THRU
             4099-READ-PKG-MAST-X.

     IF NOT PKG-POL-FOUND
        GO TO  3099-READ-UPLD-X.

     IF PKMA-INFO-COMP-POL NOT = SPACES
        PERFORM 3700-PRINT-AUTO-COMP THRU
                3799-PRINT-AUTO-COMP-X.

     DISPLAY "UMBRELLA POLICY: " PKMA-INFO-UMB-NO.
     IF PKMA-INFO-UMB-NO NOT = SPACES
        PERFORM 3900-PRINT-UMB-COMP THRU
                3999-PRINT-UMB-COMP-X.

     PERFORM 5300-ADD-MAST-REC-REDWOOD THRU
             5399-ADD-MAST-REC-REDWOOD-X.

     MOVE WS-PKG-POL-KEY TO PKMA-INFO-KEY.
     MOVE ZERO TO WS-PKG-POL-FOUND.

     PERFORM 4000-READ-PKG-MAST THRU
             4099-READ-PKG-MAST-X.

     PERFORM 5500-UPDATE-FLORA-REC THRU
             5599-UPDATE-FLORA-REC-X.

     MOVE WS-POLICY-NUMBER TO WS-EXP-POL.
     MOVE " 806" TO WS-EXP-GAGT.
     MOVE WS-EXP-POL-KEY TO EXP-KEY.
     MOVE ZERO TO WS-EXP-POL-FOUND.

     PERFORM 4500-READ-EXP-MAST THRU
             4599-READ-EXP-MAST-X.

     IF NOT EXP-POL-FOUND
        CONTINUE
     ELSE
        PERFORM 5700-UPDATE-EXP-MAST-REC THRU
                5799-UPDATE-EXP-MAST-REC-X

         MOVE WS-EXP-POL-KEY TO EXP-KEY
         MOVE ZERO TO WS-EXP-POL-FOUND

         PERFORM 4500-READ-EXP-MAST THRU
                 4599-READ-EXP-MAST-X

        PERFORM 5900-DELETE-FLORA-EXP-REC THRU
                5999-DELETE-FLORA-EXP-REC-X
     END-IF.

     PERFORM 5000-UPDATE-UPLD-TYPE THRU
             5099-UPDATE-UPLD-TYPE-X.

 3099-READ-UPLD-X.
     EXIT.
***************************************************************
*               UPDATE LEADING ZEROES TO SPACES               *
***************************************************************
 3500-CREATE-POLICY-KEY.

     IF FTR-I-POL-TYPE-3 = "C" OR "D" OR "O"
        MOVE "D" TO WS-POL-TYPE
     ELSE
        MOVE "P" TO WS-POL-TYPE.

     IF FTR-I-POL-NUM-POS1 = "0"
        IF FTR-I-POL-NUM-POS2 = "0"
           IF FTR-I-POL-NUM-POS3 = "0"
              IF FTR-I-POL-NUM-POS4 = "0"
                 IF FTR-I-POL-NUM-POS5 = "0"
                    STRING FTR-GENERAL-AGENCY-NO, WS-POL-TYPE,
                      "     ", FTR-I-POL-NUM2 DELIMITED BY SIZE
                      INTO WS-POLICY-NUMBER
                 ELSE
                    STRING FTR-GENERAL-AGENCY-NO, WS-POL-TYPE,
                      "    ", FTR-I-POL-NUM DELIMITED BY SIZE
                      INTO WS-POLICY-NUMBER
              ELSE
                 STRING FTR-GENERAL-AGENCY-NO, WS-POL-TYPE,
                   "   ", FTR-I-POL-NUM-POS4, FTR-I-POL-NUM
                   DELIMITED BY SIZE INTO WS-POLICY-NUMBER
           ELSE
              STRING FTR-GENERAL-AGENCY-NO, WS-POL-TYPE,
                "  ", FTR-I-POL-NUM-POS3, FTR-I-POL-NUM-POS4,
                FTR-I-POL-NUM DELIMITED BY SIZE
                INTO WS-POLICY-NUMBER
        ELSE
           STRING FTR-GENERAL-AGENCY-NO, WS-POL-TYPE,
             " ", FTR-I-POL-NUM-POS2, FTR-I-POL-NUM-POS3,
             FTR-I-POL-NUM-POS4, FTR-I-POL-NUM DELIMITED BY SIZE
             INTO WS-POLICY-NUMBER
     ELSE
        STRING FTR-GENERAL-AGENCY-NO, WS-POL-TYPE,
             FTR-I-POL-NUM-POS1, FTR-I-POL-NUM-POS2,
             FTR-I-POL-NUM-POS3, FTR-I-POL-NUM-POS4, FTR-I-POL-NUM
             DELIMITED BY SIZE INTO WS-POLICY-NUMBER.

 3599-CREATE-POLICY-KEY-X.
     EXIT.
***************************************************************
*                     PRINTING REPORTS                        *
***************************************************************
 3700-PRINT-AUTO-COMP.

     MOVE PKMA-INFO-KEY TO WS-APKG-POLNUM.
     MOVE PKMA-INFO-INSURED TO WS-AINS-NAME.
     MOVE PKMA-INFO-EFF-MO TO WS-AEFF-DATE-MM.
     MOVE PKMA-INFO-EFF-DA TO WS-AEFF-DATE-DD.
     MOVE PKMA-INFO-EFF-YR TO WS-AEFF-DATE-YYYY.
     MOVE PKMA-INFO-COMP TO WS-AUTO-POL-DSCNT.
     MOVE PKMA-INFO-COMP-POL TO WS-AUTO-POLNUM.
     MOVE PKMA-INFO-AUTO-STAT TO WS-AUTO-STATUS.
     MOVE PKMA-INFO-AUTO-TYPE TO WS-AUTO-TYPE.

     IF NOT PRT-AUTO-FL-OPN
         OPEN OUTPUT PRINT-RECORD-AUTO
         MOVE 1 TO WS-PRT-AUTO-FL-OPN
         PERFORM 6000-START-NEW-PG-AUTO THRU
                 6099-START-NEW-PG-AUTO-X
     ELSE
         IF WS-PRT-AUTO-LNS > 50
            MOVE SPACES TO P-REC-AUTO
            WRITE P-REC-AUTO AFTER ADVANCING PAGE
            PERFORM 6000-START-NEW-PG-AUTO THRU
                    6099-START-NEW-PG-AUTO-X
         END-IF.
     WRITE P-REC-AUTO FROM WS-REC-AUTO-COMP AFTER ADVANCING 1.
     ADD 1 TO WS-PRT-AUTO-LNS.

 3799-PRINT-AUTO-COMP-X.
     EXIT.
***************************************************************
 3900-PRINT-UMB-COMP.

     MOVE PKMA-INFO-KEY TO WS-UPKG-POLNUM.
     MOVE PKMA-INFO-INSURED TO WS-UINS-NAME.
     MOVE PKMA-INFO-EFF-MO TO WS-UEFF-DATE-MM.
     MOVE PKMA-INFO-EFF-DA TO WS-UEFF-DATE-DD.
     MOVE PKMA-INFO-EFF-YR TO WS-UEFF-DATE-YYYY.
     MOVE PKMA-INFO-UMB-NO TO WS-UMB-POL-NUMBER.

     IF NOT PRT-UMB-FL-OPN
         OPEN OUTPUT PRINT-RECORD-UMB
         MOVE 1 TO WS-PRT-UMB-FL-OPN
         PERFORM 6500-START-NEW-PG-UMB THRU
                 6599-START-NEW-PG-UMB-X
     ELSE
         IF WS-PRT-UMB-LNS > 50
            MOVE SPACES TO P-REC-UMB
            WRITE P-REC-UMB AFTER ADVANCING PAGE
            PERFORM 6500-START-NEW-PG-UMB THRU
                    6599-START-NEW-PG-UMB-X
         END-IF.
     WRITE P-REC-UMB FROM WS-REC-UMB-COMP AFTER ADVANCING 1.
     ADD 1 TO WS-PRT-UMB-LNS.

 3999-PRINT-UMB-COMP-X.
     EXIT.

***************************************************************
*                    READ PKG/FILE                            *
***************************************************************
 4000-READ-PKG-MAST.

     READ PKMAINFO RECORD
        INVALID KEY
           GO TO 4099-READ-PKG-MAST-X.

     MOVE 1 TO WS-PKG-POL-FOUND.

 4099-READ-PKG-MAST-X.
     EXIT.
***************************************************************
*                    READ EXP/FILE/KIO                      *
***************************************************************
 4500-READ-EXP-MAST.

     READ EXPMAST RECORD
        INVALID KEY
           GO TO 4599-READ-EXP-MAST-X.

     MOVE 1 TO WS-EXP-POL-FOUND.

 4599-READ-EXP-MAST-X.
     EXIT.
***************************************************************
*             UPDATE UPLD ENTRY TYPE TO "R"                   *
***************************************************************
 5000-UPDATE-UPLD-TYPE.

     MOVE "R" TO FTR-I-ENTRY.
     REWRITE FTR-RECORD.

 5099-UPDATE-UPLD-TYPE-X.
     EXIT.
***************************************************************
*         ADD FILE RECORD WITH REDWOOD MUTUAL #               *
***************************************************************
 5300-ADD-MAST-REC-REDWOOD.

     MOVE WS-PKG-POL-KEY TO PKMA-INFO-OLD-POLICY.
     MOVE " 784" TO PKMA-INFO-GAGT.
     WRITE PKMA-INFO-RECORD
        INVALID KEY
        DISPLAY "ERROR WRITING *PKG/FILE/KIO RECORD".

 5399-ADD-MAST-REC-REDWOOD-X.
     EXIT.
***************************************************************
*               UPDATE FLORA RECORD IN PKG/FILE               *
***************************************************************
 5500-UPDATE-FLORA-REC.

     MOVE "D" TO PKMA-INFO-DELETE.
     REWRITE PKMA-INFO-RECORD
        INVALID KEY
        DISPLAY "ERROR REWRITING *PKG/FILE/KIO RECORD".

 5599-UPDATE-FLORA-REC-X.
     EXIT.
***************************************************************
*            UPDATE FLORA RECORD IN *EXP/FILE/KIO             *
***************************************************************
 5700-UPDATE-EXP-MAST-REC.

     MOVE " 784" TO EXP-GAGT.
     WRITE EXPMAST-REC
        INVALID KEY
        DISPLAY "ERROR WRITING *EXP/FILE/KIO RECORD".

 5799-UPDATE-EXP-MAST-REC-X.
     EXIT.
***************************************************************
*            DELETE FLORA RECORD IN *EXP/FILE/KIO             *
***************************************************************
 5900-DELETE-FLORA-EXP-REC.

     DELETE EXPMAST
        INVALID KEY
        DISPLAY "ERROR DELETING *EXP/FILE/KIO " WS-EXP-POL-KEY
            " RECORD".

 5999-DELETE-FLORA-EXP-REC-X.
     EXIT.
***************************************************************
*                     STARTING PAGES                          *
***************************************************************
 6000-START-NEW-PG-AUTO.

     MOVE 4 TO WS-PRT-AUTO-LNS.
     ADD 1 TO WS-PG-AUTO-NUM.
     MOVE WS-PG-AUTO-NUM TO WS-PG-NUM-PRT.
     MOVE "AUTO COMPANION POLICIES" TO
        WS-HEADING-1-DTL.
     WRITE P-REC-AUTO FROM WS-HEADING-1 BEFORE ADVANCING 3 LINE.
     WRITE P-REC-AUTO FROM WS-AUTO-HEAD-2 BEFORE ADVANCING 1 LINE.
     WRITE P-REC-AUTO FROM WS-AUTO-HEAD-3 BEFORE ADVANCING 1 LINE.

 6099-START-NEW-PG-AUTO-X.
     EXIT.
***************************************************************
 6500-START-NEW-PG-UMB.

     MOVE 4 TO WS-PRT-UMB-LNS.
     ADD 1 TO WS-PG-UMB-NUM.

     MOVE WS-PG-UMB-NUM TO WS-PG-NUM-PRT.
     MOVE "UMBRELLA POLICIES" TO
        WS-HEADING-1-DTL.
     WRITE P-REC-UMB FROM WS-HEADING-1 BEFORE ADVANCING 3 LINE.
     WRITE P-REC-UMB FROM WS-UMB-HEAD-2 BEFORE ADVANCING 1 LINE.
     WRITE P-REC-UMB FROM WS-UMB-HEAD-3 BEFORE ADVANCING 1 LINE.

 6599-START-NEW-PG-UMB-X.
     EXIT.
***************************************************************
*                       CLOSING PROCEDURE                     *
***************************************************************
 7000-CLOSING-PROCEDURE.

     CLOSE FTRRECORD WITH RELEASE.
     CLOSE PKMAINFO WITH RELEASE.
     CLOSE EXPMAST WITH RELEASE.
     IF PRT-AUTO-FL-OPN
       CLOSE PRINT-RECORD-AUTO WITH SAVE.

     IF PRT-UMB-FL-OPN
       CLOSE PRINT-RECORD-UMB WITH SAVE.

 7099-CLOSING-PROCEDURE-X.
     EXIT.
*****************************************************************