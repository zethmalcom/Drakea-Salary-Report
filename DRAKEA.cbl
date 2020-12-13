        IDENTIFICATION DIVISION.
        PROGRAM-ID. PROGRAM-2.
        AUTHOR. ZETH MALCOM.
      ****************************************************************
      *  This is a program that creates a detailed employee salary 
      *  report for Drakea, LTD. This report is grouped by 
      *  each warehouse.
      * ***************
      *  INPUT:
      *     The EMPLOYEE RECORD FILE contains the following
      *     data in each record:
      *         1.  WAREHOUSE ID
      *         2.  EMPLOYEE ID
      *         3.  EMPLOYEE POSITION
      *         4.  EMPLOYEE LAST NAME
      *         5.  EMPLOYEE FIRST NAME
      *         6.  HIRE DATE
      *         7.  STARTING SALARY
      *         8.  DATE OF LAST PAY INCREASE
      *         9.  CURRENT SALARY
      *        10.  UNION DUES
      *        11.  INSURANCE
      *
      * ***************
      *  OUTPUT:
      *      The DETAILED EMPLOYEE SALARY REPORT contains 
      *      the following information:
      *    ************
      *    DETAIL LINE:
      *         1.  Expanded Warehouse ID
      *         2.  Employee ID,
      *         3.  Employee Position
      *         4.  Employee Last Name
      *         5.  Increased Current Salary 
      *         6.  Increased Union Dues
      *         7.  Increased Insurance
      *
      *    *************
      *    FINAL TOTALS:
      *         1.  THE SUM OF ALL WAREHOUSES INCREASED SALARY'S,
      *             INCREASES UNION DUES, AND INCREASES INSURANCE.
      ****************************************************************
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER.   IBM-PC.
        OBJECT-COMPUTER.   IBM-PC.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT EMPLOYEE-RECORD-FILE
                ASSIGN TO 'PR2FA19-1.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT DETAILED-SALARY-REPORT
                ASSIGN TO PRINTER 'DRAKEA-DSR.TXT'.

        DATA DIVISION.
        FILE SECTION.

        FD   EMPLOYEE-RECORD-FILE
            RECORD CONTAINS 83 CHARACTERS.

        01  EMPLOYEE-RECORD.
            05  WAREHOUSE-ID            PIC X(4).
            05  EMPLOYEE-ID             PIC X(5).
            05  EMPLOYEE-POSITION       PIC X(2).
            05  LAST-NAME               PIC X(10).
            05  FIRST-NAME              PIC X(10).
            05  FILLER                  PIC X(3).
            05  HIRE-DATE               PIC 99999999.
            05  STARTING-SALARY         PIC 999999V99.
            05  FILLER                  PIC X(4).
            05  LAST-PAY-INCREASE       PIC 999999999.
            05  CURRENT-SALARY          PIC 999999V99. 
            05  FILLER                  PIC X(5).
            05  UNION-DUES              PIC 999.
            05  INSURANCE               PIC 999V99.


        FD    DETAILED-SALARY-REPORT
             RECORD CONTAINS 80 CHARACTERS.

        01  REPORT-RECORD               PIC X(80).

        WORKING-STORAGE SECTION.

        01  FLAGS-N-SWITCHES.
           05  FIRST-RECORD            PIC X(3)      VALUE 'YES'.
           05  EOF-FLAG                PIC X         VALUE ' '.
               88  NO-MORE-DATA                      VALUE 'N'.
        
        01 TEMPORARY-FIELDS.
           05  CURRENT-TMP          PIC S999999V99  VALUE +0.
           05  UNION-TMP            PIC S99999V99  VALUE +0.
           05  INSURANCE-TMP        PIC S99999V99  VALUE +0.
           05  TOTAL-CURRENT-TMP          PIC S9999999V99  VALUE +0.
           05  TOTAL-UNION-TMP            PIC S999999V99  VALUE +0.
           05  TOTAL-INSURANCE-TMP        PIC S999999V99  VALUE +0.

        01  DETAIL-FIELDS.
           05  DF-WAREHOUSE-ID         PIC X(4).
           05  DF-WAREHOUSE-HOLD       PIC X(11).
           05  DF-EMPLOYEE-POSITION        PIC X(2).
           05  DF-INCREASED-CURRENT    PIC S999999V99  VALUE +0.
           05  DF-INCREASED-UNION      PIC S99999V99   VALUE +0.
           05  DF-INCREASED-INSURANCE  PIC S99999V99   VALUE +0.

        01  TOTAL-FIELDS.
           05  TF-INCREASED-CURRENT    PIC S9999999V99  VALUE +0.
           05  TF-INCREASED-UNION      PIC S999999V99   VALUE +0.
           05  TF-INCREASED-INSURANCE  PIC S999999V99   VALUE +0.

        01  REPORT-FIELDS.
           05  PROPER-SPACING             PIC S9   VALUE +1.
           05  WS-PAGE-NUMBER             PIC S9   VALUE +1.

        01  WS-DATE.
           05  RUN-YEAR                PIC 9(4).
           05  RUN-MONTH               PIC 99.
           05  RUN-DAY                 PIC 99.
        

        01  HEADING-ONE.

           05  H1-MONTH           PIC 99/.
           05  H1-DAY             PIC 99/.
           05  H1-YEAR            PIC 9999.
           05                     PIC X(8)     VALUE '     Y3I'.
           05  FILLER             PIC X(18)    VALUE SPACES.
           05                     PIC X(12)     VALUE 'DRAKEA, LTD'.
           05  FILLER             PIC X(24)    VALUE SPACES.
           05                     PIC X(5)     VALUE "PAGE ".
           05  PAGE-NUM           PIC 99.
           05  FILLER             PIC X(1)     VALUE SPACES.

        01  HEADING-TWO.

            05  FILLER             PIC X(35)    VALUE SPACES.
            05  H2                 PIC X(13)    VALUE 'SALARY REPORT'.
            05  FILLER             PIC X(30)    VALUE SPACES.

        01  HEADING-THREE.
            
            05  FILLER             PIC X(2)      VALUE SPACES.
            05                     PIC X(12)     VALUE 'EMPLOYEE    '.
            05                     PIC X(13)     VALUE 'EMPLOYEE     '.
            05                     PIC X(8)      VALUE 'EMPLOYEE'.
            05  FILLER             PIC X(4)      VALUE SPACES.
            05                     PIC X(9)      VALUE 'INCREASED'.
            05  FILLER             PIC X(5)      VALUE SPACES.
            05                     PIC X(9)      VALUE 'INCREASED'.
            05  FILLER             PIC X(4)      VALUE SPACES.
            05                     PIC X(9)      VALUE 'INCREASED'.
            05  FILLER             PIC X(4)      VALUE SPACES.

        01  HEADING-FOUR.

            05  FILLER             PIC X(5)      VALUE SPACES.
            05                     PIC X(2)      VALUE 'ID'.
            05  FILLER             PIC X(7)      VALUE SPACES.
            05                     PIC X(8)      VALUE 'POSITION'.
            05  FILLER             PIC X(4)      VALUE SPACES.
            05                     PIC X(9)      VALUE 'LAST NAME'.
            05  FILLER             PIC X(5)      VALUE SPACES.
            05                     PIC X(7)      VALUE 'CURRENT'.
            05  FILLER             PIC X(6)      VALUE SPACES.
            05                     PIC X(10)      VALUE 'UNION DUES'.
            05  FILLER             PIC X(3)      VALUE SPACES.
            05                     PIC X(9)      VALUE 'INSURANCE'.
            05  FILLER             PIC X(4)      VALUE SPACES.
    
        01  HEADING-FIVE.

            05                     PIC X(12)     VALUE 'WAREHOUSE:  '.
            05  WAREHOUSE-HEADER   PIC X(11).
            05  FILLER             PIC X(57)      VALUE SPACES.
            

        01  DETAIL-LINE.

           05  FILLER                  PIC X(3)   VALUE SPACES.
           05  DL-EMPLOYEE-ID          PIC X(5).
           05  FILLER                  PIC X(5)   VALUE SPACES.
           05  DL-EMPLOYEE-POSITION    PIC X(10).
           05  FILLER                  PIC X(3)   VALUE SPACES.
           05  DL-LAST-NAME            PIC X(10).
           05  FILLER                  PIC X(3)   VALUE SPACES.
           05  DL-INCREASED-CURRENT    PIC $ZZZ,ZZZ.99.
           05  FILLER                  PIC X(3)   VALUE SPACES.
           05  DL-INCREASED-UNION      PIC $ZZ,ZZZ.99.
           05  FILLER                  PIC X(3)   VALUE SPACES.
           05  DL-INCREASED-INSURANCE  PIC $ZZ,ZZZ.99.
           05  FILLER                  PIC X(3)   VALUE SPACES.

        01  TOTAL-LINE.

           05  FILLER              PIC X(7)   VALUE SPACES.
           05  TL-WAREHOUSE-NAME   PIC X(11).
           05                     PIC X(18)  VALUE ' WAREHOUSE TOTAL :'.
           05  FILLER              PIC X(2)   VALUE SPACES.
           05  TL-INCREASED-CURRENT  PIC $Z,ZZZ,ZZZ.99.
           05  FILLER              PIC X(2)   VALUE SPACES.
           05  TL-INCREASED-UNION  PIC $ZZZ,ZZZ.99.
           05  FILLER              PIC X(2)   VALUE SPACES.
           05  TL-INCREASED-INSURANCE  PIC $ZZZ,ZZZ.99.
           05  FILLER              PIC X(3)   VALUE SPACES.
           
           
        PROCEDURE DIVISION.

        100-CONTROL-MODULE.
           PERFORM 150-HOUSEKEEPING-ROUTINE
           PERFORM 250-PROCESS-EMPLOYEE-DATA
           PERFORM 600-EOF-ROUTINE

           .
           
        
        150-HOUSEKEEPING-ROUTINE.

           OPEN INPUT EMPLOYEE-RECORD-FILE
               OUTPUT DETAILED-SALARY-REPORT
           ACCEPT WS-DATE FROM DATE YYYYMMDD 
           MOVE WS-DATE(1:4) TO H1-YEAR
           MOVE WS-DATE(5:2) TO H1-MONTH
           MOVE WS-DATE(7:2) TO H1-DAY
           MOVE WS-PAGE-NUMBER TO PAGE-NUM

           
           PERFORM 200-HEADER-ROUTINE
           .

        200-HEADER-ROUTINE.
           
           WRITE REPORT-RECORD FROM HEADING-ONE
               AFTER ADVANCING PAGE
           
           MOVE 2 TO PROPER-SPACING
           MOVE HEADING-TWO TO REPORT-RECORD 
           PERFORM 350-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
           .

        250-PROCESS-EMPLOYEE-DATA.
            PERFORM UNTIL NO-MORE-DATA
               READ EMPLOYEE-RECORD-FILE
                  AT END
                     MOVE 'N' TO EOF-FLAG
                  NOT AT END
                     PERFORM 300-DATA-INPUT-ROUTINE
                END-READ
                      
            END-PERFORM
            .

        300-DATA-INPUT-ROUTINE.

           If FIRST-RECORD = 'YES'

             MOVE WAREHOUSE-ID TO DF-WAREHOUSE-HOLD
             MOVE 'NO' TO FIRST-RECORD
             PERFORM 500-PRINT-WAREHOUSE-HEADER

          ELSE

             IF WAREHOUSE-ID NOT EQUAL TO DF-WAREHOUSE-HOLD
            
              PERFORM 450-WAREHOUSE-BREAK
              PERFORM 500-PRINT-WAREHOUSE-HEADER
             END-IF

          END-IF
           
                   
           MOVE EMPLOYEE-POSITION TO DF-EMPLOYEE-POSITION 
           MOVE CURRENT-SALARY TO DF-INCREASED-CURRENT       
           MOVE UNION-DUES TO DF-INCREASED-UNION            
           MOVE INSURANCE TO DF-INCREASED-INSURANCE 
           

           EVALUATE TRUE
               WHEN DF-EMPLOYEE-POSITION = 'WM'
                   MOVE 'MANAGER' TO DL-EMPLOYEE-POSITION
               WHEN DF-EMPLOYEE-POSITION = 'DS'
                   MOVE 'SUPERVISOR' TO DL-EMPLOYEE-POSITION
               WHEN DF-EMPLOYEE-POSITION = 'OW'
                   MOVE 'OFFICE' TO DL-EMPLOYEE-POSITION
               WHEN DF-EMPLOYEE-POSITION = 'WW'
                   MOVE 'WAREHOUSE' TO DL-EMPLOYEE-POSITION
               WHEN DF-EMPLOYEE-POSITION = 'WS'
                   MOVE 'SECURITY' TO DL-EMPLOYEE-POSITION
               WHEN OTHER
                   DISPLAY 'INVALID EMPLOYEE ID'
            END-EVALUATE

           MULTIPLY DF-INCREASED-CURRENT BY .05 GIVING CURRENT-TMP
           ADD CURRENT-TMP TO DF-INCREASED-CURRENT 

           MULTIPLY DF-INCREASED-UNION BY .03 GIVING UNION-TMP
           ADD UNION-TMP TO DF-INCREASED-UNION

           MULTIPLY DF-INCREASED-INSURANCE BY .05 GIVING INSURANCE-TMP
           ADD INSURANCE-TMP TO DF-INCREASED-INSURANCE

           ADD DF-INCREASED-CURRENT TO TF-INCREASED-CURRENT
           ADD DF-INCREASED-UNION TO TF-INCREASED-UNION
           ADD DF-INCREASED-INSURANCE TO TF-INCREASED-INSURANCE

           MOVE EMPLOYEE-ID TO DL-EMPLOYEE-ID 
           MOVE LAST-NAME TO DL-LAST-NAME  
           MOVE DF-INCREASED-CURRENT TO DL-INCREASED-CURRENT
           MOVE DF-INCREASED-UNION TO DL-INCREASED-UNION
           MOVE DF-INCREASED-INSURANCE TO DL-INCREASED-INSURANCE
           

           MOVE DETAIL-LINE TO REPORT-RECORD
           PERFORM 350-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING
               
           

           MOVE ZEROS TO CURRENT-TMP
           MOVE ZEROS TO UNION-TMP
           MOVE ZEROS TO INSURANCE-TMP
           

           .                     

        350-WRITE-A-LINE.

           WRITE REPORT-RECORD
               AFTER ADVANCING PROPER-SPACING
           .
        
        450-WAREHOUSE-BREAK.
           
           MOVE WAREHOUSE-ID TO DF-WAREHOUSE-HOLD
           MOVE TF-INCREASED-CURRENT TO TL-INCREASED-CURRENT
           MOVE TF-INCREASED-INSURANCE TO TL-INCREASED-INSURANCE
           MOVE TF-INCREASED-UNION TO TL-INCREASED-UNION

           MOVE 3 TO PROPER-SPACING
           MOVE TOTAL-LINE TO REPORT-RECORD
           PERFORM 350-WRITE-A-LINE

           MOVE ZEROS TO TF-INCREASED-CURRENT
           MOVE ZEROS TO TF-INCREASED-INSURANCE
           MOVE ZEROS TO TF-INCREASED-UNION


           .
        
        500-PRINT-WAREHOUSE-HEADER.

           IF DF-WAREHOUSE-HOLD = 'AL10' THEN 
              MOVE 'ALABAMA' TO WAREHOUSE-HEADER
           END-IF
           IF DF-WAREHOUSE-HOLD = 'GA11' THEN
              MOVE 'GEORGIA' TO WAREHOUSE-HEADER
           END-IF
           IF DF-WAREHOUSE-HOLD = 'MS12' THEN
              MOVE 'MISSISSIPPI' TO WAREHOUSE-HEADER
           END-IF
           
           WRITE REPORT-RECORD FROM HEADING-FIVE
               AFTER ADVANCING PROPER-SPACING
           MOVE 3 TO PROPER-SPACING
           WRITE REPORT-RECORD FROM HEADING-THREE
               AFTER ADVANCING PROPER-SPACING
           MOVE 1 TO PROPER-SPACING
           WRITE REPORT-RECORD FROM HEADING-FOUR
               AFTER ADVANCING PROPER-SPACING
           MOVE 2 TO PROPER-SPACING
           .
        
        550-END-OF-JOB-ROUTINE.
      *    CODE FOR LAST CONTROL LINE GOES HERE

           PERFORM 450-WAREHOUSE-BREAK

        .

        600-EOF-ROUTINE.

           PERFORM 550-END-OF-JOB-ROUTINE

           CLOSE EMPLOYEE-RECORD-FILE
               DETAILED-SALARY-REPORT
           STOP RUN
           .
        