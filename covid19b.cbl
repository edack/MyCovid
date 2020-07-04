       IDENTIFICATION DIVISION.
       PROGRAM-ID. COVID19B.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USA-HIST-FILE ASSIGN TO USAHIST.
           SELECT PRINT-FILE    ASSIGN TO PRTLINE.
      *===============================================================*
       DATA DIVISION.
      *---------------------------------------------------------------*
       FILE SECTION.
       FD  USA-HIST-FILE
               RECORDING MODE F.
       01  USA-HIST-RECORD            PIC X(130).
      *---------------------------------------------------------------*
       FD  PRINT-FILE
               RECORDING MODE F.
       01  PRINT-RECORD.
      *    05  CC                     PIC X(01).
           05  PRINT-LINE             PIC X(130).
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
       01   REPORT-LINES.
      *---------------------------------------------------------------*
           05  NEXT-REPORT-LINE       PIC X(130).
      *---------------------------------------------------------------*
           05  UHR-RECORD.
               10  UHR-TIMESTAMP.
                   15  UHR-MONTH      PIC X(02).
                   15  FILLER         PIC X(01)  VALUE '/'.
                   15  UHR-DAY        PIC X(02).
                   15  FILLER         PIC X(01)  VALUE '/'.
                   15  UHR-YEAR       PIC X(04).
               10  FILLER             PIC X(02)  VALUE SPACE.
               10  UHR-CASE-POSITIVE  PIC Z,ZZZ,ZZ9.
               10  FILLER             PIC X(02)  VALUE SPACE.
               10  UHR-CASE-NEGATIVE  PIC Z,ZZZ,ZZ9.
               10  FILLER             PIC X(02)  VALUE SPACE.
               10  UHR-CASE-PENDING   PIC ZZ,ZZ9.
               10  FILLER             PIC X(01)  VALUE SPACE.
               10  UHR-CASE-NEW       PIC Z,ZZZ,ZZ9.
               10  UHR-HOSPITAL-TOT   PIC Z,ZZZ,ZZ9.
               10  UHR-ICU-TOT        PIC Z,ZZZ,ZZ9.
               10  UHR-VENT-TOT       PIC Z,ZZZ,ZZ9.
               10  UHR-RECOVERED      PIC Z,ZZZ,ZZ9.
               10  UHR-DEATH          PIC Z,ZZZ,ZZ9.
               10  UHR-DEATH-NEW      PIC Z,ZZZ,ZZ9.
               10  FILLER             PIC X(02)  VALUE SPACE.
               10  UHR-PERCENT        PIC Z9.9999.
               10  FILLER             PIC X(04)  VALUE '%   '.
      *---------------------------------------------------------------*
           05  HEADING-LINE-1.
               10 HL1-DATE.
                   15  FILLER         PIC X(01) VALUE SPACE.
                   15  FILLER         PIC X(12) VALUE 'TODAYS DATE:'.
                   15  HL1-MONTH-OUT  PIC XX.
                   15  FILLER         PIC X     VALUE '/'.
                   15  HL1-DAY-OUT    PIC XX.
                   15  FILLER         PIC X     VALUE '/'.
                   15  HL1-YEAR-OUT   PIC XX.
               10  FILLER             PIC X(80) VALUE SPACE.
               10  HL1-PAGE-COUNT-AREA.
                   15  FILLER         PIC X(04) VALUE SPACE.
                   15  FILLER         PIC X(05) VALUE 'PAGE:'.
                   15  HL1-PAGE-NUM   PIC ZZZZ9.
                   15  FILLER         PIC X(05) VALUE SPACE.
      *---------------------------------------------------------------*
           05  HEADING-LINE-2.
               10  FILLER    PIC X(12) VALUE '  AS OF     '.
               10  FILLER    PIC X(20) VALUE '  POSITIVE  NEGATIVE'.
               10  FILLER    PIC X(20) VALUE '    PEND     NEW +  '.
               10  FILLER    PIC X(20) VALUE 'HOSPITAL    ICU     '.
               10  FILLER    PIC X(20) VALUE 'VENT              TO'.
               10  FILLER    PIC X(20) VALUE 'TAL     NEW         '.
               10  FILLER    PIC X(10) VALUE '         '.
      *---------------------------------------------------------------*
           05  HEADING-LINE-3.
               10  FILLER    PIC X(12) VALUE '  DATE      '.
               10  FILLER    PIC X(20) VALUE '   TESTS      TESTS '.
               10  FILLER    PIC X(20) VALUE '   TESTS     TESTS  '.
               10  FILLER    PIC X(20) VALUE ' ADMITS   ADMITS    '.
               10  FILLER    PIC X(20) VALUE 'ADMIT  RECOVER   DEA'.
               10  FILLER    PIC X(20) VALUE 'THS   DEATHS   PERCE'.
               10  FILLER    PIC X(10) VALUE 'NT        '.
      *---------------------------------------------------------------*
           05  HEADING-LINE-4.
               10  FILLER    PIC X(12) VALUE '  ----      '.
               10  FILLER    PIC X(20) VALUE '  -------    -------'.
               10  FILLER    PIC X(20) VALUE '--  -----    -----  '.
               10  FILLER    PIC X(20) VALUE ' ------   ------    '.
               10  FILLER    PIC X(20) VALUE '-----  -------   ---'.
               10  FILLER    PIC X(20) VALUE '---   ------   -----'.
               10  FILLER    PIC X(10) VALUE '--        '.
      *---------------------------------------------------------------*
       01  WS-HOLD-FIELDS.
      *---------------------------------------------------------------*
           05  WS-UHR-RECORD.
               10  WS-UHR-DATE.
                   15  WS-UHR-YEAR       PIC X(04).
                   15  WS-UHR-MONTH      PIC X(02).
                   15  WS-UHR-DAY        PIC X(02).
               10  WS-UHR-CASE-POSITIVE  PIC 9(07).
               10  WS-UHR-CASE-NEGATIVE  PIC 9(07).
               10  WS-UHR-CASE-PENDING   PIC 9(07).
               10  WS-UHR-CASE-NEW       PIC 9(07).
               10  WS-UHR-HOSPITAL-CURR  PIC 9(07).
               10  WS-UHR-HOSPITAL-TOT   PIC 9(07).
               10  WS-UHR-ICU-CURR       PIC 9(07).
               10  WS-UHR-ICU-TOT        PIC 9(07).
               10  WS-UHR-VENT-CURR      PIC 9(07).
               10  WS-UHR-VENT-TOT       PIC 9(07).
               10  WS-UHR-RECOVERED      PIC 9(07).
               10  WS-UHR-DEATH          PIC 9(07).
               10  WS-UHR-DEATH-NEW      PIC 9(07).
               10  WS-UHR-PERCENT        PIC 99V99.
      *---------------------------------------------------------------*
           05  WS-PERCENT             PIC 99V999999.
           05  TOTAL-ACCUMULATORS.
               10  TA-CASE-TOT        PIC 9(08).
               10  TA-DEATH-TOT       PIC 9(08).
           05  TODAYS-DATE.
               10  TD-YEAR            PIC 99.
               10  TD-MONTH           PIC 99.
               10  TD-DAY             PIC 99.
           05  FILE-STATUS            PIC X(02).
           05  END-OF-FILE-SW         PIC X(01)   VALUE 'N'.
               88  END-OF-FILE                    VALUE 'Y'.
           05  VALID-RECORD-SW        PIC X(01)   VALUE 'Y'.
               88  VALID-RECORD                   VALUE 'Y'.
      *---------------------------------------------------------------*
       01  PRINTER-CONTROL-FIELDS.
      *---------------------------------------------------------------*
           05  LINE-SPACEING          PIC 9(02) VALUE 1.
           05  LINE-COUNT             PIC 9(03) VALUE 999.
           05  LINES-ON-PAGE          PIC 9(03) VALUE 56.
           05  PAGE-COUNT             PIC 9(03) VALUE 1.
           05  TOP-OF-PAGE            PIC X     VALUE '1'.
           05  SINGLE-SPACE           PIC X     VALUE ' '.
           05  DOUBLE-SPACE           PIC X     VALUE '0'.
           05  TRIPLE-SPACE           PIC X     VALUE '-'.
      *===============================================================*
       PROCEDURE DIVISION.
      *---------------------------------------------------------------*
       0000-MAIN-PROCESSING.
      *---------------------------------------------------------------*
           PERFORM 1000-OPEN-FILES.
           PERFORM 8000-READ-USA-HIST-FILE.
           PERFORM 2000-PROCESS-USA-HIST-FILE
               UNTIL END-OF-FILE.
           PERFORM 3000-CLOSE-FILES.
           GOBACK.
      *---------------------------------------------------------------*
       1000-OPEN-FILES.
      *---------------------------------------------------------------*
           OPEN INPUT  USA-HIST-FILE
                OUTPUT PRINT-FILE.
           ACCEPT TODAYS-DATE FROM DATE.
           MOVE TD-YEAR                TO HL1-YEAR-OUT.
           MOVE TD-MONTH               TO HL1-MONTH-OUT.
           MOVE TD-DAY                 TO HL1-DAY-OUT.
      *---------------------------------------------------------------*
       2000-PROCESS-USA-HIST-FILE.
      *---------------------------------------------------------------*
           MOVE WS-UHR-DAY           TO UHR-DAY.
           MOVE WS-UHR-MONTH         TO UHR-MONTH.
           MOVE WS-UHR-YEAR          TO UHR-YEAR.
           MOVE WS-UHR-CASE-POSITIVE TO UHR-CASE-POSITIVE.
           MOVE WS-UHR-CASE-NEGATIVE TO UHR-CASE-NEGATIVE.
           MOVE WS-UHR-CASE-PENDING  TO UHR-CASE-PENDING.
           MOVE WS-UHR-CASE-NEW      TO UHR-CASE-NEW.
           MOVE WS-UHR-HOSPITAL-TOT  TO UHR-HOSPITAL-TOT.
           MOVE WS-UHR-ICU-TOT       TO UHR-ICU-TOT.
           MOVE WS-UHR-VENT-TOT      TO UHR-VENT-TOT.
           MOVE WS-UHR-RECOVERED     TO UHR-RECOVERED.
           MOVE WS-UHR-DEATH         TO UHR-DEATH.
           MOVE WS-UHR-DEATH-NEW     TO UHR-DEATH-NEW.
           IF  WS-UHR-CASE-POSITIVE > ZERO
               DIVIDE WS-UHR-DEATH BY WS-UHR-CASE-POSITIVE
                   GIVING WS-PERCENT
               MULTIPLY WS-PERCENT BY 100 GIVING UHR-PERCENT
           ELSE
               MOVE ZERO               TO UHR-PERCENT.
           MOVE UHR-RECORD           TO NEXT-REPORT-LINE.
           PERFORM 9000-PRINT-REPORT-LINE.
           PERFORM 8000-READ-USA-HIST-FILE.
      *---------------------------------------------------------------*
       3000-CLOSE-FILES.
      *---------------------------------------------------------------*
           CLOSE USA-HIST-FILE
                 PRINT-FILE.
      *---------------------------------------------------------------*
       8000-READ-USA-HIST-FILE.
      *---------------------------------------------------------------*
           READ USA-HIST-FILE
               AT END MOVE 'Y'         TO END-OF-FILE-SW
                      MOVE 'N'         TO VALID-RECORD-SW.
           IF VALID-RECORD
               UNSTRING USA-HIST-RECORD DELIMITED BY ','
               INTO  WS-UHR-DATE
                     WS-UHR-CASE-POSITIVE
                     WS-UHR-CASE-NEGATIVE
                     WS-UHR-CASE-PENDING
                     WS-UHR-CASE-NEW
                     WS-UHR-HOSPITAL-CURR
                     WS-UHR-HOSPITAL-TOT
                     WS-UHR-ICU-CURR
                     WS-UHR-ICU-TOT
                     WS-UHR-VENT-CURR
                     WS-UHR-VENT-TOT
                     WS-UHR-RECOVERED
                     WS-UHR-DEATH
                     WS-UHR-DEATH-NEW
                     WS-UHR-PERCENT.
      *---------------------------------------------------------------*
       9000-PRINT-REPORT-LINE.
      *---------------------------------------------------------------*
           IF LINE-COUNT GREATER THAN LINES-ON-PAGE
              PERFORM 9100-PRINT-HEADING-LINES.
           MOVE NEXT-REPORT-LINE       TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
       9100-PRINT-HEADING-LINES.
      *---------------------------------------------------------------*
           MOVE PAGE-COUNT             TO HL1-PAGE-NUM.
           MOVE HEADING-LINE-1         TO PRINT-LINE.
           PERFORM 9110-WRITE-TOP-OF-PAGE.
           MOVE 2                      TO LINE-SPACEING.
           MOVE HEADING-LINE-2         TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE 1                      TO LINE-SPACEING.
           MOVE HEADING-LINE-3         TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE HEADING-LINE-4         TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           ADD 1                       TO PAGE-COUNT.
           MOVE 6                      TO LINE-COUNT.
      *---------------------------------------------------------------*
       9110-WRITE-TOP-OF-PAGE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING PAGE.
      *---------------------------------------------------------------*
       9120-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING LINE-SPACEING.
           ADD LINE-SPACEING           TO LINE-COUNT.
           MOVE 1                      TO LINE-SPACEING.
           MOVE SPACE                  TO PRINT-LINE.
