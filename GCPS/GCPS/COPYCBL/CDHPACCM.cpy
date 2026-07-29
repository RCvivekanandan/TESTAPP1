      ***************************************************************** 05/11/04
      *  COPY MEMBER CDHPACCM                                         * HGCDBT60
      *  GCPS - BLUE TRANSFER/BLUE CHIP INTERFACE                     *    LV002
      *  BLUE TRANSFER     - BLUE CHIP- GCPS I/O COMMAREA             * HGCDBT60
      *  LENGTH                                                       * HGCDBT60
      *   FIXED    :   DATA TO GCPS - 107                             * CDHPACCM
      *                DATA TO BCHP - 15                              * CDHPACCM
      *   VARIABLE :   DATA TO BCHP - 117                             * CDHPACCM
      *                                                               * HGCDBT60
      *   VARY MAX OCCUR :          - 1891 (123 + 13 + (117*15))      * CDHPACCM
      ***************************************************************** HGCDBT60
      *  INPUT/OUTPUT BLUE TRANSFER FOR GCPS:                         * HGCDBT60
      *                                                               * HGCDBT60
      *  NOTE: DATES ARE JULIAN YYYYDDD COMP-3.                       * HGCDBT60
      *        ACCUM REQUESTS ARE:  DE FOR DED                        * HGCDBT60
      *                             MX FOR MAX                        * HGCDBT60
      *                             CP FOR CPY                        * HGCDBT60
      *                             OP FOR OPX                        * HGCDBT60
      *                             CO FOR COI                        * HGCDBT60
      ***************************************************************** HGCDBT60
      *  LOG #   DATE     BY   DESCRIPTION                            * HGCDBT60
      * ------  --------  ---  -------------------------------------  * HGCDBT60
      * P02384  11/22/04  GDM  CREATED                                * HGCDBT60
      * P02384  03/10/05  GDM  MODIFIY                                * CDHPACCM
      * P02384  09/22/05  GDM  MODIFY FOR MULTIPLE VENDOR TBL         * CDHPACCM
      *                                                               * HGCDBT60
      ***************************************************************** HGCDBT60
      *01  GCDHP-GCDHP-IO-AREA.                                         HGCDBT60
           05  GCDHP-DATA-TO-GCPS.                                      HGCDBT60
               10 GCDHP-PLAN-CODE                    PIC X(03).         CDHPACCM
               10 GCDHP-GROUP                        PIC X(09).         CDHPACCM
               10 GCDHP-SECTION                      PIC X(05).         CDHPACCM
               10 GCDHP-PKG                          PIC X(03).         CDHPACCM
               10 GCDHP-LOB                          PIC X(01).         CDHPACCM
                                                                        HGCDBT60
               10 GCDHP-PROVDR-CONTROL               PIC XX.            CDHPACCM
               10 GCDHP-FAM-REL-LVL                  PIC XX.            CDHPACCM
               10 GCDHP-EFFECTIVE-DATE.                                 CDHPACCM
                   15 GCDHP-EFFDT-CC                 PIC X.             CDHPACCM
                   15 GCDHP-EFF-DT                   PIC S9(5)  COMP-3. CDHPACCM
               10 GCDHP-EFFDT-CEN REDEFINES                             CDHPACCM
                  GCDHP-EFFECTIVE-DATE               PIC S9(7)  COMP-3. CDHPACCM
               10 GCDHP-TERM-DATE.                                      CDHPACCM
                   15 GCDHP-TERMDT-CC                PIC X.             CDHPACCM
                   15 GCDHP-TERMN-DT                 PIC S9(5)  COMP-3. CDHPACCM
               10 GCDHP-TERMDT-CEN REDEFINES                            CDHPACCM
                  GCDHP-TERM-DATE                    PIC S9(7)  COMP-3. CDHPACCM
                                                                        CDHPACCM
               10 GCDHP-FAM-REL-LVL-MEDICARE         PIC X(01).         CDHPACCM
               10 GCDHP-FAM-REL-LVL-M-S-D            PIC X(01).         CDHPACCM
               10 GCDHP-FAM-REL-LVL-LO-HI            PIC X(01).         CDHPACCM
               10 GCDHP-PATIENT-AGE                  PIC S9(03) COMP-3. CDHPACCM
                                                                        CDHPACCM
               10 GCDHP-PROV-CTL-PLAN-BASIC          PIC X(01).         CDHPACCM
               10 GCDHP-PROV-CTL-PREF-BASIC.                            CDHPACCM
                   15 GCDHP-PROV-CTL-PPO-BASIC       PIC X(01).         CDHPACCM
               10 GCDHP-PROV-CTL-EMPL-BASIC          PIC X(01).         CDHPACCM
                                                                        CDHPACCM
               10 GCDHP-PROV-CTL-PLAN-MM             PIC X(01).         CDHPACCM
               10 GCDHP-PROV-CTL-PREF-MM.                               CDHPACCM
                   15 GCDHP-PROV-CTL-PPO-MM          PIC X(01).         CDHPACCM
               10 GCDHP-PROV-CTL-EMPL-MM             PIC X(01).         CDHPACCM
                                                                        CDHPACCM
               10 GCDHP-DATE-PARM.                                      CDHPACCM
                   15 GCDHP-START-NEXT-SVC-DT-CEN    PIC X(08).         CDHPACCM
                   15 GCDHP-END-SVC-DT-CEN           PIC X(08).         CDHPACCM
                                                                        CDHPACCM
               10 GCDHP-IGCPS-SEND-RETURN-CODE       PIC X(02).         CDHPACCM
                   88 GCDHP-DATES-FILE-LIST-BUILD      VALUE 'D1'.      CDHPACCM
                                                                        CDHPACCM
               10 GCDHP-SUBSCRIBER-NO                PIC X(12).         CDHPACCM
               10 GCDHP-REAL-GROUP-NO                PIC X(09).         CDHPACCM
               10 GCDHP-TYPE-OF-ACCUM                PIC X(02).         CDHPACCM
                   88 GCDHP-ACCUM-DED                  VALUE 'DE'.      CDHPACCM
                   88 GCDHP-ACCUM-OPX                  VALUE 'OP'.      CDHPACCM
                   88 GCDHP-ACCUM-MAX                  VALUE 'MX'.      CDHPACCM
                   88 GCDHP-ACCUM-COI                  VALUE 'CO'.      CDHPACCM
                   88 GCDHP-ACCUM-CPY                  VALUE 'CP'.      CDHPACCM
               10 GCDHP-VENDOR-ID                    PIC X(02).         CDHPACCM
               10 GCDHP-VENDOR-BEN-PER               PIC X(02).         CDHPACCM
               10 GCDHP-VENDOR-BP-BEGIN-DATE         PIC S9(07) COMP-3. CDHPACCM
               10 GCDHP-VENDOR-BP-END-DATE           PIC S9(07) COMP-3. CDHPACCM
               10 GCDHP-VENDOR-OPTIONAL-INFO.                           CDHPACCM
                   15 GCDHP-VENDOR-OPTIONAL-FILL     PIC X(04).         CDHPACCM
                   15 GCDHP-VENDOR-OPTIONAL-ID       PIC X(04).         CDHPACCM
               10 GCDHP-REQUEST-TYPE                 PIC X(02).         CDHPACCM
                   88 INBOUND-REQUEST                  VALUE '01'.      CDHPACCM
                   88 OUTBOUND-REQUEST                 VALUE '02'.      CDHPACCM
               10 GCDHP-REQUESTED-VENDOR             PIC X(2).          CDHPACCM
               10 GCDHP-VENDOR-LIST OCCURS 5 TIMES                      CDHPACCM
                                    INDEXED BY VENDOR-INDEX.            CDHPACCM
                   15 GCDHP-LIST-VENDOR              PIC X(02).         CDHPACCM
               10 GCDHP-CALLING-SYS-REQUEST-ID       PIC X(02).         CDHPACCM
                   88 GCDHP-PVI-REQUEST                VALUE '01'.      CDHPACCM
                   88 GCDHP-AVI-REQUEST                VALUE '02'.      CDHPACCM
                   88 GCDHP-GVO-REQUEST                VALUE '03'.      CDHPACCM
                   88 GCDHP-GVO-REQUEST-PVI            VALUE '04'.      CDHPACCM
                   88 GCDHP-INQ-REQUEST                VALUE '05'.      CDHPACCM
               10 GCDHP-ACCUM-VENDOR-USE-IND         PIC X(01).         CDHPACCM
                   88 GCDHP-SINGLE-VENDOR-REQ          VALUE '1'.       CDHPACCM
                   88 GCDHP-MULTI-VENDOR-REQ           VALUE '2'.       CDHPACCM
               10 GCDHP-ACCM-VENDOR-IO-IND           PIC X(01).         CDHPACCM
                   88 GCDHP-ACCM-VENDOR-INBND          VALUE '1'.       CDHPACCM
                   88 GCDHP-ACCM-VENDOR-OUTBND         VALUE '2'.       CDHPACCM
                                                                        CDHPACCM
           05  GCDHP-DATA-TO-BCHP.                                      HGCDBT60
               10 GCDHP-STATUS-CODE                  PIC X(02).         CDHPACCM
                   88 GCDHP-VALID-RETURN               VALUE '00'.      CDHPACCM
                   88 GCDHP-INVALID-REQUEST            VALUE '01'.      CDHPACCM
                   88 GCDHP-INVALID-GROUP              VALUE '02'.      CDHPACCM
               10 GCDHP-PSEUDO-GRP-NBR               PIC X(9).          CDHPACCM
               10 GCDHP-ENTRY-COUNT                  PIC S999  COMP-3.  CDHPACCM
               10 GCDHP-ACCUMS-CHOSEN OCCURS 1 TO 15 TIMES              CDHPACCM
                     DEPENDING ON GCDHP-ENTRY-COUNT                     HGCDBT60
                     INDEXED BY GCDHP-INDEX.                            HGCDBT60
                   15 GCDHP-MULTIPLE-VENDOR-COUNT    PIC 9(02).                 
                   15 GCDHP-ACCUM-MULTI-VENDORS                                 
                        OCCURS 5 TIMES                                          
                        INDEXED BY GCDHP-MULTI-VEND-INDEX.                      
                      20 GCDHP-ACCUM-MULTI-VENDOR    PIC X(02).                 
                   15 GCDHP-MANDATORY-IND            PIC X(1).          CDHPACCM
                   15 GCDHP-BENEFIT-PERIOD           PIC X(2).          CDHPACCM
                   15 GCDHP-BEN-PRD-BEGIN-DATE       PIC X(08).         CDHPACCM
                   15 GCDHP-BEN-PRD-END-DATE         PIC X(08).         CDHPACCM
                   15 GCDHP-FAM-OR-INDIV             PIC X.             CDHPACCM
                   15 GCDHP-L-O-B                    PIC X.             CDHPACCM
                   15 GCDHP-TIME-DOLLAR-IND          PIC XX.            CDHPACCM
                   15 GCDHP-DEFINITION               PIC XX.            CDHPACCM
                   15 GCDHP-DAY-FACTOR-IND           PIC X.             CDHPACCM
                   15 GCDHP-INTERNAL-DESCRIPTOR      PIC X(9).          CDHPACCM
                   15 GCDHP-SERVICE-GROUP            PIC X(2).          CDHPACCM
                   15 GCDHP-PLACE-OF-TREATMENT       PIC X(2).          CDHPACCM
                                                                                
                   15 GCDHP-CONDITION.                                  CDHPACCM
                       20  GCDHP-ALL-BIT             PIC X.             DES 0118
                       20  GCDHP-EXCLUSION-BIT       PIC X.             DES 0118
                       20  GCDHP-ICD-BIT             PIC X.             DES 0118
                       20  GCDHP-TB-BIT              PIC X.             DES 0118
                       20  GCDHP-MENTAL-BIT          PIC X.             DES 0118
                       20  GCDHP-DRUG-BIT            PIC X.             DES 0118
                       20  GCDHP-ALCOHOL-BIT         PIC X.             DES 0118
                       20  GCDHP-OB-COMP-BIT         PIC X.             DES 0118
                       20  GCDHP-OB-NORM-BIT         PIC X.             DES 0118
                       20  GCDHP-MALIGNANCY-BIT      PIC X.             DES 0118
                       20  GCDHP-CARDIAC-DISEASE-BIT PIC X.             RKH 0409
                       20  GCDHP-OBESITY-BIT         PIC X.             DES 0118
                       20  GCDHP-KIDNEY-DISEASE-BIT  PIC X.             RKH 0409
                       20  GCDHP-ACCIDENT-BIT        PIC X.             AHL 1112
                       20  GCDHP-PRE-EXIST-BIT       PIC X.             AHL 1112
                       20  GCDHP-NON-EMER-BIT        PIC X.             AHL 1112
                       20  GCDHP-SUICIDE-BIT         PIC X.             FRY 0827
                       20  GCDHP-TMJ-BIT             PIC X.             NGE D200
                       20  GCDHP-INF-BIT             PIC X.             NGE D200
                       20  GCDHP-LIFE-THREAT-BIT     PIC X.             NGE D200
                       20  GCDHP-EMER-MED-BIT        PIC X.             NGE D200
                       20  GCDHP-EMER-ACC-BIT        PIC X.             NGE D200
                       20  GCDHP-SER-MEN-ILL-BIT     PIC X.             NGE D200
                       20  GCDHP-NON-SER-MEN-ILL-BIT PIC X.             NGE D200
                       20  GCDHP-FILLER-BIT          PIC X(06).         NGE11154
                                                                                
                   15  GCDHP-BISCENDING-IND          PIC X(1).          ENW 0289
                   15  GCDHP-CLAIM-LVL-ACCUM-IND     PIC X.             ENW 0386
                   15  GCDHP-CO-PAY-IND              PIC X.             TJR 0106
                   15  GCDHP-COST-CONTAIN-IND        PIC X(2).          TJR 0106
                                                                                
                   15  GCDHP-1ST-DOLR-COVRGE-LMT     PIC X.             CDHPACCM
                                                                                
                   15  GCDHP-SEL-ADDL-BEN-DET        PIC X(1).          RKH 0287
                   15  GCDHP-COMB-APPLIED-IND        PIC X(2).          RKH 0287
                   15  GCDHP-ACCUMID                 PIC X(4).          RKH 0287
                   15  GCDHP-AGE-LIMIT-FROM  COMP-3  PIC S9(3).         NGE11154
                   15  GCDHP-AGE-LIMIT-TO    COMP-3  PIC S9(3).         NGE11154
                   15  GCDHP-AGE-QUAL-IND-FROM       PIC X.             NGE11154
                   15  GCDHP-AGE-QUAL-IND-TO         PIC X.             NGE11154
                   15  GCDHP-RELATIONSHIP-IND        PIC X(2).          NGE11154
                                                                                
                   15  GCDHP-CARRY-OVER-CREDIT-IND   PIC X.                     
                   15  GCDHP-PERCENT-LEVEL   COMP-3  PIC S9(3).                 
                   15  GCDHP-ASCEND-DESCEND-IND      PIC X.                     
                                                                                
                   15  GCDHP-REINSTATEMENT-IND       PIC X.                     
                   15  GCDHP-VALUE-LIMIT     COMP-3  PIC S9(7)V99.      DES 0118
                   15  GCDHP-VALUE-QUALIFIER         PIC X.                     
                   15  GCDHP-FEAK-IND                PIC X.                     
                   15  GCDHP-FYI-VALUE               PIC X(3).          RKH 0287
