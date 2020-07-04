       IDENTIFICATION DIVISION.
       PROGRAM-ID. COVID19A.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COUNTRY-FILE ASSIGN TO CNTRYREC.
           SELECT PRINT-FILE   ASSIGN TO PRTLINE.
      *===============================================================*
       DATA DIVISION.
      *---------------------------------------------------------------*
       FILE SECTION.
       FD  COUNTRY-FILE
               RECORDING MODE F.
       01  COUNTRY-RECORD             PIC X(130).
      *---------------------------------------------------------------*
       FD  PRINT-FILE
               RECORDING MODE F.
       01  PRINT-RECORD.
      *    05  CC                     PIC X(01).
           05  PRINT-LINE             PIC X(80).
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
       01   REPORT-LINES.
      *---------------------------------------------------------------*
           05  NEXT-REPORT-LINE       PIC X(80).
      *---------------------------------------------------------------*
           05  HEADING-LINE-1.
              10  HL1-DATE.
                  15  FILLER          PIC X(01) VALUE SPACE.
                  15  FILLER          PIC X(12) VALUE 'TODAYS DATE:'.
                  15  HL1-MONTH-OUT   PIC XX.
                  15  FILLER          PIC X     VALUE '/'.
                  15  HL1-DAY-OUT     PIC XX.
                  15  FILLER          PIC X     VALUE '/'.
                  15  HL1-YEAR-OUT    PIC XX.
              10  FILLER              PIC X(10) VALUE SPACE.
              10  HL1-AS-OF-DATE.
                  15  FILLER          PIC X(11) VALUE 'RPT AS OF: '.
                  15  HL1-AOD-MM-OUT  PIC X(02).
                  15  FILLER          PIC X(01) VALUE '/'.
                  15  HL1-AOD-DD-OUT  PIC X(02).
                  15  FILLER          PIC X(01) VALUE '/'.
                  15  HL1-AOD-YY-OUT  PIC X(04).
              10  FILLER              PIC X(10) VALUE SPACE.
              10  HL1-PAGE-COUNT-AREA.
                  15  FILLER          PIC X(04) VALUE SPACE.
                  15  FILLER          PIC X(05) VALUE 'PAGE:'.
                  15  HL1-PAGE-NUM    PIC ZZZZ9.
                  15  FILLER          PIC X(05) VALUE SPACE.
      *---------------------------------------------------------------*
           05 HEADING-LINE-2.
              10  FILLER    PIC X(20) VALUE '                    '.
              10  FILLER    PIC X(20) VALUE '      NEW       TOTA'.
              10  FILLER    PIC X(20) VALUE 'L       NEW       TO'.
              10  FILLER    PIC X(20) VALUE 'TAL        FATAL    '.
      *---------------------------------------------------------------*
           05 HEADING-LINE-3.
              10  FILLER    PIC X(20) VALUE ' COUNTRY            '.
              10  FILLER    PIC X(20) VALUE '     CASES      CASE'.
              10  FILLER    PIC X(20) VALUE 'S     DEATHS     DEA'.
              10  FILLER    PIC X(20) VALUE 'THS       PERCENT   '.
      *---------------------------------------------------------------*
           05 HEADING-LINE-4.
              10  FILLER    PIC X(20) VALUE ' -------            '.
              10  FILLER    PIC X(20) VALUE '     -----      ----'.
              10  FILLER    PIC X(20) VALUE '-     ------     ---'.
              10  FILLER    PIC X(20) VALUE '---       -------   '.
      *---------------------------------------------------------------*
           05  TOTAL-HEADING-LINE-1.
              10  FILLER    PIC X(20) VALUE '                    '.
              10  FILLER    PIC X(20) VALUE 'WORLD           TOTA'.
              10  FILLER    PIC X(20) VALUE 'L                TOT'.
              10  FILLER    PIC X(20) VALUE 'AL         FATAL    '.
      *---------------------------------------------------------------*
           05  TOTAL-HEADING-LINE-2.
              10  FILLER    PIC X(20) VALUE '                    '.
              10  FILLER    PIC X(20) VALUE 'TOTALS          CASE'.
              10  FILLER    PIC X(20) VALUE 'S                DEA'.
              10  FILLER    PIC X(20) VALUE 'THS       PERCENT   '.
      *---------------------------------------------------------------*
           05  TOTAL-HEADING-LINE-3.
              10  FILLER    PIC X(20) VALUE '                    '.
              10  FILLER    PIC X(20) VALUE '------          ----'.
              10  FILLER    PIC X(20) VALUE '-                ---'.
              10  FILLER    PIC X(20) VALUE '---       -------   '.
      *---------------------------------------------------------------*
           05  DETAIL-LINE.
               10 DL-COUNTRY          PIC X(20).
               10 FILLER              PIC X(02) VALUE SPACE.
               10 DL-CASE-NEW         PIC ZZZZ,ZZ9.
               10 FILLER              PIC X(02) VALUE SPACE.
               10 DL-CASE-TOTAL       PIC Z,ZZZ,ZZ9.
               10 FILLER              PIC X(03) VALUE SPACE.
               10 DL-DEATH-NEW        PIC ZZZZ,ZZ9.
               10 FILLER              PIC X(03) VALUE SPACE.
               10 DL-DEATH-TOTAL      PIC ZZZZ,ZZ9.
               10 FILLER              PIC X(05) VALUE SPACE.
               10 DL-PERCENT          PIC ZZ9.9999.
               10 FILLER              PIC X(02) VALUE '% '.
      *---------------------------------------------------------------*
           05  TOTAL-LINE.
               10 FILLER              PIC X(31) VALUE SPACE.
               10 TL-CASE-TOTAL       PIC ZZ,ZZZ,ZZ9.
               10 FILLER              PIC X(12) VALUE SPACE.
               10 TL-DEATH-TOTAL      PIC ZZ,ZZZ,ZZ9.
               10 FILLER              PIC X(05) VALUE SPACE.
               10 TL-PERCENT          PIC ZZ9.9999.
               10 FILLER              PIC X(02) VALUE '% '.
      *---------------------------------------------------------------*
           05  HOLD-AREA.
               10  WS-COUNTRY         PIC X(44).
               10  WS-CODE            PIC X(02).
               10  WS-SLUG            PIC X(44).
               10  WS-CASE-NEW        PIC 9(07).
               10  WS-CASE-TOT        PIC 9(07).
               10  WS-DEATH-NEW       PIC 9(07).
               10  WS-DEATH-TOT       PIC 9(07).
               10  WS-RECVD-NEW       PIC 9(07).
               10  WS-RECVD-TOT       PIC 9(07).
               10  WS-RECVD-TOT-2     PIC 9(07).
               10  WS-TIMESTAMP.
                   15  WS-AOD-YEAR    PIC X(04).
                   15  FILLER         PIC X(01).
                   15  WS-AOD-MONTH   PIC X(02).
                   15  FILLER         PIC X(01).
                   15  WS-AOD-DAY     PIC X(02).
                   15  FILLER         PIC X(11).
               10  WS-PERCENT         PIC 99V999999.
           05  TOTAL-ACCUMULATORS.
               10  TA-CASE-TOT        PIC 9(08).
               10  TA-DEATH-TOT       PIC 9(08).
      *---------------------------------------------------------------*
       01  SWITCHES-MISC-FIELDS.
      *---------------------------------------------------------------*
           05  END-OF-FILE-SW         PIC X(01)   VALUE 'N'.
               88  END-OF-FILE                    VALUE 'Y'.
           05  VALID-RECORD-SW        PIC X(01)   VALUE 'Y'.
               88  VALID-RECORD                   VALUE 'Y'.
           05  TODAYS-DATE.
               10  TD-YEAR            PIC 99.
               10  TD-MONTH           PIC 99.
               10  TD-DAY             PIC 99.
           05  FILE-STATUS            PIC X(02).
           05  CARRAGE-CONTROL.
               10  TOP-OF-PAGE        PIC X       VALUE '1'.
               10  SINGLE-SPACE       PIC X       VALUE ' '.
               10  DOUBLE-SPACE       PIC X       VALUE '0'.
      *---------------------------------------------------------------*
       01  PRINTER-CONTROL-FIELDS.
      *---------------------------------------------------------------*
           05  LINE-SPACEING          PIC 9(02) VALUE 1.
           05  LINE-COUNT             PIC 9(03) VALUE 999.
           05  LINES-ON-PAGE          PIC 9(03) VALUE 56.
           05  PAGE-COUNT             PIC 9(03) VALUE 1.
      *===============================================================*
       PROCEDURE DIVISION.
      *---------------------------------------------------------------*
       0000-MAIN-PROCESSING.
      *---------------------------------------------------------------*
           PERFORM 1000-OPEN-FILES.
           PERFORM 8000-READ-COUNTRY-FILE.
           PERFORM 2000-PROCESS-COUNTRY-FILE
               UNTIL END-OF-FILE.
           PERFORM 3000-PRINT-TOTAL-LINE.
           PERFORM 4000-CLOSE-FILES.
           GOBACK.
      *---------------------------------------------------------------*
       1000-OPEN-FILES.
      *---------------------------------------------------------------*
           OPEN INPUT  COUNTRY-FILE
                OUTPUT PRINT-FILE.
           ACCEPT TODAYS-DATE FROM DATE.
           MOVE TD-YEAR                TO HL1-YEAR-OUT.
           MOVE TD-MONTH               TO HL1-MONTH-OUT.
           MOVE TD-DAY                 TO HL1-DAY-OUT.
      *---------------------------------------------------------------*
       2000-PROCESS-COUNTRY-FILE.
      *---------------------------------------------------------------*
           MOVE  WS-COUNTRY            TO DL-COUNTRY.
           MOVE  WS-CASE-NEW           TO DL-CASE-NEW.
           MOVE  WS-CASE-TOT           TO DL-CASE-TOTAL.
           MOVE  WS-DEATH-NEW          TO DL-DEATH-NEW.
           MOVE  WS-DEATH-TOT          TO DL-DEATH-TOTAL.
           IF  WS-CASE-TOT > ZERO
               DIVIDE WS-DEATH-TOT BY WS-CASE-TOT GIVING WS-PERCENT
               MULTIPLY WS-PERCENT BY 100 GIVING DL-PERCENT
           ELSE
               MOVE ZERO               TO DL-PERCENT.
           PERFORM 2100-ACCUMULATE-TOTALS.
           MOVE DETAIL-LINE            TO NEXT-REPORT-LINE.
           PERFORM 9000-PRINT-REPORT-LINE.
           PERFORM 8000-READ-COUNTRY-FILE.
      *---------------------------------------------------------------*
       2100-ACCUMULATE-TOTALS.
      *---------------------------------------------------------------*
           ADD   WS-CASE-TOT           TO TA-CASE-TOT.
           ADD   WS-DEATH-TOT          TO TA-DEATH-TOT.
      *---------------------------------------------------------------*
       3000-PRINT-TOTAL-LINE.
      *---------------------------------------------------------------*
           PERFORM 3100-PRINT-TOTAL-HEADING.
           MOVE  1                     TO LINE-COUNT.
           MOVE  TA-CASE-TOT           TO TL-CASE-TOTAL.
           MOVE  TA-DEATH-TOT          TO TL-DEATH-TOTAL.
           IF  TA-CASE-TOT > ZERO
               DIVIDE TA-DEATH-TOT BY TA-CASE-TOT GIVING WS-PERCENT
               MULTIPLY WS-PERCENT BY 100 GIVING TL-PERCENT
           ELSE
               MOVE ZERO               TO TL-PERCENT.
           MOVE TOTAL-LINE             TO  NEXT-REPORT-LINE.
           PERFORM  9000-PRINT-REPORT-LINE.
      *---------------------------------------------------------------*
       3100-PRINT-TOTAL-HEADING.
      *---------------------------------------------------------------*
           MOVE 3 TO LINE-SPACEING.
           MOVE TOTAL-HEADING-LINE-1   TO PRINT-LINE.
           PERFORM 9200-WRITE-PRINT-LINE.
           MOVE 1        TO LINE-SPACEING.
           MOVE TOTAL-HEADING-LINE-2   TO PRINT-LINE.
           PERFORM 9200-WRITE-PRINT-LINE.
           MOVE TOTAL-HEADING-LINE-3   TO PRINT-LINE.
           PERFORM 9200-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
       4000-CLOSE-FILES.
      *---------------------------------------------------------------*
           CLOSE COUNTRY-FILE
                 PRINT-FILE.
      *---------------------------------------------------------------*
       8000-READ-COUNTRY-FILE.
      *---------------------------------------------------------------*
           READ COUNTRY-FILE
               AT END MOVE 'Y'         TO END-OF-FILE-SW
                      MOVE 'N'         TO VALID-RECORD-SW.
           IF VALID-RECORD
               UNSTRING COUNTRY-RECORD DELIMITED BY ','
               INTO WS-COUNTRY
                    WS-CODE
                    WS-SLUG
                    WS-CASE-NEW
                    WS-CASE-TOT
                    WS-DEATH-NEW
                    WS-DEATH-TOT
                    WS-RECVD-NEW
                    WS-RECVD-TOT
                    WS-RECVD-TOT-2
                    WS-TIMESTAMP.
      *---------------------------------------------------------------*
       9000-PRINT-REPORT-LINE.
      *---------------------------------------------------------------*
           IF LINE-COUNT GREATER THAN LINES-ON-PAGE
              PERFORM 9100-PRINT-HEADING-LINES.
           MOVE NEXT-REPORT-LINE       TO PRINT-LINE.
           PERFORM 9200-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
       9100-PRINT-HEADING-LINES.
      *---------------------------------------------------------------*
           MOVE WS-AOD-YEAR            TO HL1-AOD-YY-OUT.
           MOVE WS-AOD-MONTH           TO HL1-AOD-MM-OUT.
           MOVE WS-AOD-DAY             TO HL1-AOD-DD-OUT.
           MOVE PAGE-COUNT             TO HL1-PAGE-NUM.
           MOVE HEADING-LINE-1         TO PRINT-LINE.
           PERFORM 9110-WRITE-TOP-OF-PAGE.
           MOVE 2                      TO LINE-SPACEING.
           MOVE HEADING-LINE-2         TO PRINT-LINE.
           PERFORM 9200-WRITE-PRINT-LINE.
           MOVE 1                      TO LINE-SPACEING.
           MOVE HEADING-LINE-3         TO PRINT-LINE.
           PERFORM 9200-WRITE-PRINT-LINE.
           MOVE HEADING-LINE-4         TO PRINT-LINE.
           PERFORM 9200-WRITE-PRINT-LINE.
           ADD 1                       TO PAGE-COUNT.
           MOVE 6                      TO LINE-COUNT.
      *---------------------------------------------------------------*
       9110-WRITE-TOP-OF-PAGE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING PAGE.
      *---------------------------------------------------------------*
       9200-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING LINE-SPACEING.
           ADD LINE-SPACEING           TO LINE-COUNT.
           MOVE 1                      TO LINE-SPACEING.
           MOVE SPACE                  TO PRINT-LINE.
