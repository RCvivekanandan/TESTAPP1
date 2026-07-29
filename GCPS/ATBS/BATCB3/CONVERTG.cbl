       IDENTIFICATION DIVISION.                                         01/20/07
       PROGRAM-ID.        CONVERTG.                                     P02384GS
       AUTHOR.            KIKI.                                         UT LV021
       DATE-WRITTEN.      AUGUST 15, 2015.                              P02384GS
       DATE-COMPILED.                                                   P02384GS
                                                                        P02384GS
      *--------------------------------------------------------------*  P09398WK
      *   IL                                                         *  P09398WK
      *   CONVERT  THE  G2  GRPOUSPC  FILE  RECORDS  -  TOTAL  CARE  *     CL**2
      *                                                              *  P09398WK
      *--------------------------------------------------------------*  P09398WK
      *--------------------------------------------------------------*  P09398WK
      *    SYSTEM   -  GCPS  - GENERIC  CONTRACT  PROCESSING  SYSTEM *  P09398WK
      *                                                              *  P09398WK
      *    PROGRAM  -  CONVERTG  CONVERSION  PROGRAM  FOR  ALL       *     CL**2
      *                          GROUP  SPECIFIC  FILE  RECORDS      *  P09398WK
      *                                                              *  P09398WK
      *                                                              *  P09398WK
      *    INPUT    -  SEQ  FILE  CONTAINING  WORKFILE  RECORDS      *  P09398WK
      *                           OLD  FORMAT                        *  P09398WK
      *                                                              *  P09398WK
      *    OUTPUT   -  SEQ  FILE  CONTAINING  WORKFILE  RECORDS      *  P09398WK
      *                           NEW  FORMAT                        *  P09398WK
      *--------------------------------------------------------------*  P09398WK
                                                                        P02384GS
       ENVIRONMENT DIVISION.                                            P02384GS
       CONFIGURATION SECTION.                                           P02384GS
       SOURCE-COMPUTER.  IBM-370.                                       P02384GS
       OBJECT-COMPUTER.  IBM-370.                                       P02384GS
                                                                        P02384GS
       INPUT-OUTPUT SECTION.                                            P02384GS
       FILE-CONTROL.                                                    P02384GS
                                                                        P02384GS
           SELECT IN-GROUPSPC-FILE          ASSIGN TO   INFILE.         P02384GS
           SELECT OUT-GROUPSPC-FILE         ASSIGN TO   OUTFILE.        P02384GS
                                                                        P02384GS
       DATA DIVISION.                                                   P02384GS
       FILE SECTION.                                                    P02384GS
                                                                        P02384GS
       FD  IN-GROUPSPC-FILE                                             P02384GS
           BLOCK CONTAINS  0  RECORDS                                   P02384GS
           LABEL RECORDS ARE STANDARD                                   P02384GS
           RECORDING MODE IS V.                                         P02384GS
                                                                        P02384GS
       01  OLD-GRPSPEC-RECORD                               PIC X(1200).P02384GS
                                                                        P02384GS
       01  OLD-FORMAT-GRPSPEC-RECORD.                                   P02384GS
           05 GCG-GRP-SPEC-RECORD.                                         CL*12
               10 GCG-GRP-SPECIF-ID.                                       CL*12
                   15 GCG-PLAN-CODE                     PIC X(3).       00028800
                   15 GCG-GROUP-NUM.                                    00028900
                      20 GCG-GROUP-NO-1-3               PIC X(3).       00029000
                      20 GCG-GRP-NO                     PIC X(6).       00029100
                   15 GCG-SECTION-NUM.                                  00029200
                      20 GCG-SEC-NO-1                   PIC X(1).       00029300
                      20 GCG-SECTN-NO                   PIC X(4).       00029400
                   15 GCG-PKG-CODE                      PIC X(3).       00029500
                   15 GCG-FAM-REL-LVL                   PIC XX.         00029600
                   15 GCG-EFFECTIVE-DATE.                               00029700
028400                20 GCG-EFFDT-CC                   PIC X.          00029800
028500                20 GCG-EFF-DT             COMP-3  PIC S9(5).      00029900
028600             15 GCG-EFFDT-CEN REDEFINES GCG-EFFECTIVE-DATE        00030000
028700                                          COMP-3  PIC S9(7).      00030100
028800         10 GCG-TERM-DATE.                                        00030200
028900             15 GCG-TERMDT-CC                     PIC X.          00030300
029000             15 GCG-TERMN-DT              COMP-3  PIC S9(5).      00030400
029100         10 GCG-TERMDT-CEN REDEFINES GCG-TERM-DATE                00030500
029200                                          COMP-3  PIC S9(7).      00030600
029300         10 GCG-DATE-LAST-CHANGE.                                 00030700
029400            15 GCG-DATE-LAST-CHANGE-CC            PIC X.          00030800
029500            15 GCG-DT-OF-LAST-CHANGE      COMP-3  PIC S9(5).      00030900
029600         10 GCG-DT-LAST-CHANGE-CEN REDEFINES                      00031000
029700               GCG-DATE-LAST-CHANGE       COMP-3  PIC S9(07).     00031100
029800         10 GCG-COUNT-TAB-PROVN-POINTERS  COMP-3  PIC S999.       00031200
029900         10 GCG-HUMAN-ORGAN-DONOR-BEN-IND         PIC X.          00031300
030000         10 GCG-BC-TYPE-BEN-PERD-IND              PIC X.          00031400
030100         10 GCG-BS-TYPE-BEN-PERD-IND              PIC X.          00031500
030200         10 GCG-MM-TYPE-BEN-PERD-IND              PIC X.          00031600
030300         10 FILLER                                PIC X.          00031700
030400         10 GCG-DEFAULT-PAYEE-IND                 PIC X.          00031800
030500         10 GCG-MCARE-LIFET-RSRV-DAYS-IND         PIC X.          00031900
030600         10 GCG-COORD-MCARE-IND                   PIC X.          00032000
030700         10 GCG-MCARE-DRG-PROCS-IND               PIC X.          00032100
030800         10 GCG-BC-START-AGRMS                    PIC XX.         00032200
030900         10 GCG-BS-START-AGRMS                    PIC XX.         00032300
031000         10 GCG-MM-START-AGRMS                    PIC XX.         00032400
031100         10 GCG-BC-TERMN-BEN-EXTEN-IND            PIC XX.         00032500
031200         10 GCG-BS-TERMN-BEN-EXTEN-IND            PIC XX.         00032600
031300         10 GCG-MM-TERMN-BEN-EXTEN-IND            PIC XX.         00032700
031400         10 GCG-BC-WAIVR-IND                      PIC XX.         00032800
031500         10 GCG-BS-WAIVR-IND                      PIC XX.         00032900
031600         10 GCG-MM-WAIVR-IND                      PIC XX.         00033000
031700         10 GCG-POS-PARTICP-IND                   PIC XX.         00033100
031800         10 FILLER                                PIC X.          00033200
031900         10 GCG-BC-EXPENSE-FREE-IND               PIC X.          00033300
032000         10 GCG-BS-EXPENSE-FREE-IND               PIC X.          00033400
032100         10 GCG-MM-EXPENSE-FREE-IND               PIC X.          00033500
032200         10 GCG-BC-EXPENSE-FREE-DAYS      COMP-3  PIC S999.       00033600
032300         10 GCG-BS-EXPENSE-FREE-DAYS      COMP-3  PIC S999.       00033700
032400         10 GCG-MM-EXPENSE-FREE-DAYS      COMP-3  PIC S999.       00033800
032500         10 GCG-EOMB-REQRD-IND                    PIC X.          00033900
032600         10 GCG-BC-FORGN-CLM-IND                  PIC X.          00034000
032700         10 GCG-BS-FORGN-CLM-IND                  PIC X.          00034100
032800         10 GCG-MM-FORGN-CLM-IND                  PIC X.          00034200
032900         10 GCG-ANNL-REINST-IND                   PIC X.          00034300
033000         10 GCG-ANNL-REINST-AMT           COMP-3  PIC S9(7).      00034400
033100         10 GCG-GOOD-HLTH-REINST-IND              PIC X.          00034500
033200         10 GCG-GOOD-HLTH-REINST-AFTR-BEN COMP-3  PIC S9(7).      00034600
033300         10 GCG-FAM-SECUR-COV-IND                 PIC XX.         00034700
033400         10 GCG-DEP-TERMN-IND                     PIC X.          00034800
033500         10 GCG-DEP-MAX-AGE               COMP-3  PIC S999.       00034900
033600         10 GCG-STU-MAX-AGE               COMP-3  PIC S999.       00035000
033700         10 GCG-STU-CERTN-REQRD-IND               PIC X.          00035100
033800         10 GCG-STU-HLTH-PLAN-IND                 PIC X.          00035200
033900         10 GCG-U-C-CORRIDOR-APPL-IND             PIC X.          00035300
034000         10 GCG-BC-CARD-REH-BIT-IND               PIC X.          00035400
034100         10 GCG-MATER-LEAVE-IND                   PIC X.          00035500
034200         10 GCG-BC-OB-WAITG-PERD-IND              PIC X.          00035600
034300         10 GCG-BS-OB-WAITG-PERD-IND              PIC X.          00035700
034400         10 GCG-MM-OB-WAITG-PERD-IND              PIC X.          00035800
034500         10 GCG-BC-OB-WAITG-PERD-MEM-DAYS COMP-3 PIC S999.        00035900
034600         10 GCG-BS-OB-WAITG-PERD-MEM-DAYS COMP-3 PIC S999.        00036000
034700         10 GCG-MM-OB-WAITG-PERD-MEM-DAYS COMP-3 PIC S999.        00036100
034800         10 GCG-BC-OB-WAITG-PERD-SPS-DAYS COMP-3 PIC S999.        00036200
034900         10 GCG-BS-OB-WAITG-PERD-SPS-DAYS COMP-3 PIC S999.        00036300
035000         10 GCG-MM-OB-WAITG-PERD-SPS-DAYS COMP-3 PIC S999.        00036400
035100         10 GCG-BC-OB-WAITG-PERD-DEP-DAYS COMP-3 PIC S999.        00036500
035200         10 GCG-BS-OB-WAITG-PERD-DEP-DAYS COMP-3 PIC S999.        00036600
035300         10 GCG-MM-OB-WAITG-PERD-DEP-DAYS COMP-3 PIC S999.        00036700
035400         10 GCG-WC-IND                            PIC X.          00036800
035500         10 GCG-WC-MAR-STAT-IND                   PIC X.          00036900
035600         10 GCG-BC-MM-WC-MIN-AMT          COMP-3  PIC S9(5).      00037000
035700         10 GCG-BS-MM-WC-MIN-AMT          COMP-3  PIC S9(5).      00037100
035800         10 GCG-EVIDENCE-OF-INSURANCE-IND         PIC X.          00037200
035900         10 FILLER                                PIC X.          00037300
036000         10 GCG-WC-MAX-AGE                COMP-3  PIC S999.       00037400
036100         10 GCG-WC-MIN-AGE                COMP-3  PIC S999.       00037500
036200         10 GCG-POS-PENALTY-DT.                                   00037600
036300            15 GCG-POS-PENLTY-DT-CC               PIC X.          00037700
036400            15 GCG-POS-PENALTY-DATE       COMP-3  PIC S9(5).      00037800
036500         10 GCG-POS-PENLTY-DT-CEN REDEFINES                       00037900
036600              GCG-POS-PENALTY-DT          COMP-3  PIC S9(7).      00038000
036700         10 FILLER                                PIC X.          00038100
036800         10 GCG-CHC-PRIOR-ADMISSION               PIC X.          00038200
036900         10 GCG-DAY-PSYCH-BIT-IND                 PIC X.          00038300
037000         10 FILLER                                PIC X.          00038400
037100         10 GCG-DAY-PSYCH-FASLT-TYPE-IND          PIC X.          00038500
037200         10 GCG-DAY-PSYCH-PART-CD                 PIC X.          00038600
037300         10 GCG-DAY-PSYCH-PRIOR-ADM-CD            PIC X.          00038700
037400         10 GCG-DAY-PSYCH-APPRD-SVCS-CD           PIC X.          00038800
037500         10 GCG-NIGHT-PSYCH-BIT-IND               PIC X.          00038900
037600         10 FILLER                                PIC X.          00039000
037700         10 GCG-NIGHT-PSYCH-FCLT-TYPE-IND         PIC X.          00039100
037800         10 GCG-NIGHT-PSYCH-PART-CD               PIC X.          00039200
037900         10 GCG-NIGHT-PSYCH-PRIOR-ADM-CD          PIC X.          00039300
038000         10 GCG-NIGHT-PSYCH-APPRD-SVCS-CD         PIC X.          00039400
038100         10 GCG-MM-CARD-REHAB-BIT-IND             PIC X.          00039500
038200         10 FILLER                                PIC X.          00039600
038300         10 GCG-MM-CARD-REH-PRIOR-ADM             PIC X.          00039700
038400         10 GCG-MM-CARD-REH-APPRD-SVCS-CD         PIC X.          00039800
038500         10 GCG-SUBS-ABUSE-BIT-IND                PIC X.          00039900
038600         10 FILLER                                PIC X.          00040000
038700         10 GCG-MM-TRANSSXL-PMT-RESTR-IND         PIC X.          00040100
038800         10 GCG-BS-TRANSSXL-PMT-RESTR-IND         PIC X.          00040200
038900         10 GCG-SUBS-ABUSE-APPRD-SVCS-CD          PIC X.          00040300
039000         10 GCG-ECF-SNF-BIT-IND                   PIC X.          00040400
039100         10 FILLER                                PIC X.          00040500
039200         10 GCG-ECF-SNF-FACLT-TYPE-IND            PIC X.          00040600
039300         10 GCG-ECF-SNF-PRIOR-ADM-CD              PIC X.          00040700
039400         10 GCG-BC-TYPE-ADM-IND                   PIC X.          00040800
039500         10 GCG-BS-TYPE-ADM-IND                   PIC X.          00040900
039600         10 GCG-MM-TYPE-ADM-IND                   PIC X.          00041000
039700         10 GCG-BC-SPEC-INVESTN-IND               PIC X.          00041100
039800         10 GCG-DED-BASE-AMT-SOURCE-IND           PIC X.          00041200
039900         10 GCG-MAX-LEAVES-ADM            COMP-3  PIC S9(3).      00041300
040000         10 FILLER                                PIC X.          00041400
040100         10 GCG-BC-INTERPL-BANK-IND               PIC X.          00041500
040200         10 FILLER                                PIC X.          00041600
040300         10 GCG-ROLLUP-IND                        PIC X.          00041700
040400         10 GCG-U-C-CORR-AMT              COMP-3  PIC S9(5).      00041800
040500         10 GCG-CRDT-CART-ACCEPT-IND              PIC X.          00041900
040600         10 GCG-RFCR-ADJUSTMENT-PROC-IND          PIC X.          00042000
040700         10 GCG-BC-TRANSSXL-PMT-RESTR-IND         PIC X.          00042100
040800         10 GCG-EOB-GROUP-COPY-IND                PIC X.          00042200
040900         10 GCG-BC-WAITG-PERD-IND                 PIC XX.         00042300
041000         10 GCG-BS-WAITG-PERD-IND                 PIC XX.         00042400
041100         10 GCG-MM-WAITG-PERD-IND                 PIC XX.         00042500
041200         10 FILLER                                PIC X(2).       00042600
041300         10 GCG-GROUP-SECTION-NAME                PIC X(50).      00042700
041400         10 FILLER                                PIC X(7).       00042800
041500         10 GCG-INTER-RELATIONAL-CODE             PIC X(31).      00042900
041600         10 GCG-INTER-RELATIONAL-CODE-X  REDEFINES                00043000
041700            GCG-INTER-RELATIONAL-CODE.                            00043100
041800            15  GCG-INTER-RELATIONAL-CODE-1       PIC X(01).      00043200
041900            15  GCG-INTER-RELATIONAL-CODE-2       PIC X(10).      00043300
042000            15  GCG-INTER-RELATIONAL-CODE-3       PIC X(10).      00043400
042100            15  GCG-INTER-RELATIONAL-CODE-4       PIC X(10).      00043500
042200         10 GCG-L-O-B-CONTRACT-LEVEL-IND          PIC X(2).       00043600
042300         10 GCG-PROV-CONTROL-CONT-BC-IND          PIC X(2).       00043700
042400         10 GCG-PROV-CONTROL-CONT-BS-IND          PIC X(2).       00043800
042500         10 GCG-PROV-CONTROL-CONT-MM-IND          PIC X(2).       00043900
042600         10 GCG-FAM-REL-CONTROL-CONT-BC           PIC X(2).       00044000
042700         10 GCG-FAM-REL-CONTROL-CONT-BS           PIC X(2).       00044100
042800         10 GCG-FAM-REL-CONTROL-CONT-MM           PIC X(2).       00044200
042900         10 GCG-GROUP-CONTROL-RNSTATE-IND         PIC X.          00044300
043000         10 GCG-CHC-BIT-INDICATOR                 PIC X(1).       00044400
043100         10 FILLER                                PIC X(1).       00044500
043200         10 GCG-CHC-PARTICIPATION-CODE            PIC X(1).       00044600
043300         10 GCG-TPL-INVESTIGATION-SEQ-IND         PIC X.          00044700
043400         10 GCG-COB-CALCULATION-METHOD            PIC X.          00044800
043500         10 GCG-COB-ACCM-BEN-PERD-IND             PIC X(2).       00044900
043600         10 GCG-COB-ACCM-L-O-B-IND                PIC X.          00045000
043700         10 GCG-MEDCR-ACCM-BEN-PERD-IND           PIC X(2).       00045100
043800         10 GCG-MEDCR-ACCM-L-O-B                  PIC X.          00045200
043900         10 GCG-WKR-COMP-L-O-B                    PIC X.          00045300
044000         10 GCG-REIMBUR-SUBROF-ACCM-L-O-B         PIC X.          00045400
044100         10 FILLER-1                              PIC X.          00045500
044200         10 GCG-FIRST-YR-REINST-EFF-DT            PIC X(8).       00045600
044300         10 GCG-ACCUM-PSEUDO-GRP-NBR              PIC X(9).       00045700
044400         10 GCG-ACCUM-PSEUDO-SECTION-NBR.                         00045800
044500            15 GCG-ACCUM-PSEUDO-SEC-NBR           PIC X(04).      00045900
044600            15 GCG-ACCUM-PSEUDO-SEC-NBR-1         PIC X(01).      00046000
044700         10 GCG-PSEU-NBR-USG-BEN-AGG-MAXM         PIC X.          00046100
044800         10 GCG-PSEU-NBR-USG-COINS-LIMITS         PIC X.          00046200
044900         10 GCG-PSEU-NBR-USG-DEDU-LIMITS          PIC X.          00046300
045000         10 GCG-PSEU-NBR-USG-OPX-LIMITS           PIC X.          00046400
045100         10 GCG-BC-CARD-REH-PRIOR-ADM             PIC X.          00046500
045200         10 GCG-BC-CARD-REH-APPRD-SVCS-CD         PIC X.          00046600
045300         10 GCG-NRM-NWBRN-BILG-IND                PIC X.          00046700
045400         10 GCG-BC-NRM-NWBRN-ELIG-IND             PIC X.          00046800
045500         10 GCG-BS-NRM-NWBRN-ELIG-IND             PIC X.          00046900
045600         10 GCG-MM-NRM-NWBRN-ELIG-IND             PIC X.          00047000
045700         10 GCG-BC-SPCL-NWBRN-COVERAGE            PIC X.          00047100
045800         10 GCG-BS-SPCL-NWBRN-COVERAGE            PIC X.          00047200
045900         10 GCG-MM-SPCL-NWBRN-COVERAGE            PIC X.          00047300
046000         10 GCG-NWBRN-AGE-LIMIT           COMP-3  PIC S9(3).      00047400
046100         10 GCG-WC-DIAGNOSIS-BIT-IND              PIC X.          00047500
046200         10 GCG-WC-INVESN-MODE                    PIC X.          00047600
046300         10 GCG-UNSOLICIT-RFND-BEN-PERD           PIC X(2).       00047700
046400         10 GCG-UNSOLICIT-RFND-L-O-B              PIC X.          00047800
046500         10 GCG-UNSOLICIT-INT-TIME-FACTOR COMP-3  PIC S9(3).      00047900
046600         10 GCG-TERMINAL-OB-COVERAGE-IND          PIC X.          00048000
046700         10 GCG-ADJUSTMENT-PROCESS-AMOUNT COMP-3  PIC S9(5)V99.   00048100
046800         10 FILLER                                PIC X(11).      00048200
046900         10 GCG-PENALTY-APPLIC-IND                PIC X(02).      00048300
047000         10 FILLER                                PIC X.          00048400
047100         10 GCG-OUTPKT-BASE-AMT-SOURCE-IN         PIC X.          00048500
047200         10 GCG-MULTI-PENALTY-SELECTION           PIC X.          00048600
047300         10 FILLER                                PIC X.          00048700
047400         10 GCG-ALT-BEN-PROV-IND                  PIC X(02).      00048800
047500         10 GCG-ALT-BEN-PROV-TIME-FACTOR COMP-3   PIC S999.       00048900
047600         10 GCG-BC-SPEC-PROC-IND                  PIC XX.         00049000
047700         10 GCG-BS-SPEC-PROC-IND                  PIC XX.         00049100
047800         10 GCG-PROC-PRE-POST-PYMT-IND            PIC X.          00049200
047900         10 GCG-BC-WAITG-PERD-MEM-DAYS    COMP-3  PIC S999.       00049300
048000         10 GCG-BS-WAITG-PERD-MEM-DAYS    COMP-3  PIC S999.       00049400
048100         10 GCG-MM-WAITG-PERD-MEM-DAYS    COMP-3  PIC S999.       00049500
048200         10 GCG-BC-WAITG-PERD-SPS-DAYS    COMP-3  PIC S999.       00049600
048300         10 GCG-BS-WAITG-PERD-SPS-DAYS    COMP-3  PIC S999.       00049700
048400         10 GCG-MM-WAITG-PERD-SPS-DAYS    COMP-3  PIC S999.       00049800
048500         10 GCG-BC-WAITG-PERD-DEP-DAYS    COMP-3  PIC S999.       00049900
048600         10 GCG-BS-WAITG-PERD-DEP-DAYS    COMP-3  PIC S999.       00050000
048700         10 GCG-MM-WAITG-PERD-DEP-DAYS    COMP-3  PIC S999.       00050100
048800         10 GCG-COMB-COST-CONTAIN-PGM-IND        PIC X(02).       00050200
048900         10 GCG-TIMELY-FILG-IND                   PIC X(02).      00050300
049000         10 GCG-PARTICIPAT-PROV-OPTION            PIC X(02).      00050400
049100         10 GCG-ADDL-TRNSPLNT-COVRG-IND           PIC X(02).      00050500
049200         10 GCG-FRI-SAT-ADM-IND                   PIC X(02).      00050600
049300         10 GCG-HOSPICE-IND                       PIC X(02).      00050700
049400         10 GCG-INCENTIVE-OB-IND                  PIC X(02).      00050800
049500         10 GCG-MAND-ADDL-SURG-OPN-IND            PIC X(02).      00050900
049600         10 GCG-MED-NECESSITY-HCNR-IPS-IN         PIC X(02).      00051000
049700         10 GCG-MAND-OP-SURG-PROG-IND             PIC X(02).      00051100
049800         10 GCG-MED-SERV-ADV-PROG-IND             PIC X(02).      00051200
049900         10 GCG-PRE-ADM-REVIEW-IND                PIC X(02).      00051300
050000         10 GCG-PRE-ADM-TESTING-PROGRAM           PIC X(02).      00051400
050100         10 GCG-REIMBUR-SUBROG-IND                PIC X(02).      00051500
050200         10 GCG-SUBS-ABUSE-MENTAL-IND             PIC X(02).      00051600
050300         10 GCG-MONDAY-DISCHARGE-IND              PIC X(02).      00051700
050400         10 GCG-ATCP-PENALTY-DT.                                  00051800
050500            15 GCG-ATCP-PENLTY-DT-CC              PIC X.          00051900
050600            15 GCG-ATCP-PENALTY-DATE       COMP-3 PIC S9(05).     00052000
050700         10 GCG-ATCP-PENLTY-DT-CEN REDEFINES                      00052100
050800              GCG-ATCP-PENALTY-DT         COMP-3  PIC S9(7).      00052200
050900         10 GCG-WKND-PENALTY-DT.                                  00052300
051000            15 GCG-WKND-PENLTY-DT-CC              PIC X.          00052400
051100            15 GCG-WKND-PENALTY-DATE       COMP-3 PIC S9(05).     00052500
051200         10 GCG-WKND-PENLTY-DT-CEN REDEFINES                      00052600
051300              GCG-WKND-PENALTY-DT         COMP-3  PIC S9(7).      00052700
051400         10 GCG-HOSP-PENALTY-DT.                                  00052800
051500            15 GCG-HOSP-PENLTY-DT-CC              PIC X.          00052900
051600            15 GCG-HOSP-PENALTY-DATE       COMP-3 PIC S9(05).     00053000
051700         10 GCG-HOSP-PENLTY-DT-CEN REDEFINES                      00053100
051800              GCG-HOSP-PENALTY-DT         COMP-3  PIC S9(7).      00053200
051900         10 GCG-INOB-PENALTY-DT.                                  00053300
052000            15 GCG-INOB-PENLTY-DT-CC              PIC X.          00053400
052100            15 GCG-INOB-PENALTY-DATE      COMP-3  PIC S9(5).      00053500
052200         10 GCG-INOB-PENLTY-DT-CEN REDEFINES                      00053600
052300              GCG-INOB-PENALTY-DT         COMP-3  PIC S9(7).      00053700
052400         10 GCG-MASOP-PENALTY-DT.                                 00053800
052500            15 GCG-MASOP-PENLTY-DT-CC             PIC X.          00053900
052600            15 GCG-MASOP-PENALTY-DATE     COMP-3  PIC S9(5).      00054000
052700         10 GCG-MASOP-PENLTY-DT-CEN REDEFINES                     00054100
052800              GCG-MASOP-PENALTY-DT        COMP-3  PIC S9(7).      00054200
052900         10 GCG-MED-NEC-PENALTY-DT.                               00054300
053000            15 GCG-MED-NEC-PENLTY-DT-CC           PIC X.          00054400
053100            15 GCG-MED-NEC-PENALTY-DATE   COMP-3  PIC S9(5).      00054500
053200         10 GCG-MED-NEC-PENLTY-DT-CEN REDEFINES                   00054600
053300              GCG-MED-NEC-PENALTY-DT      COMP-3  PIC S9(7).      00054700
053400         10 GCG-PPO-PENALTY-DT.                                   00054800
053500            15 GCG-PPO-PENLTY-DT-CC               PIC X.          00054900
053600            15 GCG-PPO-PENALTY-DATE       COMP-3  PIC S9(5).      00055000
053700         10 GCG-PPO-PENLTY-DT-CEN REDEFINES                       00055100
053800              GCG-PPO-PENALTY-DT          COMP-3  PIC S9(7).      00055200
053900         10 GCG-MOPS-PENALTY-DT.                                  00055300
054000            15 GCG-MOPS-PENLTY-DT-CC              PIC X.          00055400
054100            15 GCG-MOPS-PENALTY-DATE      COMP-3  PIC S9(5).      00055500
054200         10 GCG-MOPS-PENLTY-DT-CEN REDEFINES                      00055600
054300              GCG-MOPS-PENALTY-DT         COMP-3  PIC S9(7).      00055700
054400         10 GCG-MSA-PENALTY-DT.                                   00055800
054500            15 GCG-MSA-PENLTY-DT-CC               PIC X.          00055900
054600            15 GCG-MSA-PENALTY-DATE       COMP-3  PIC S9(5).      00056000
054700         10 GCG-MSA-PENLTY-DT-CEN REDEFINES                       00056100
054800              GCG-MSA-PENALTY-DT          COMP-3  PIC S9(7).      00056200
054900         10 GCG-PAR-PENALTY-DT.                                   00056300
055000            15 GCG-PAR-PENLTY-DT-CC               PIC X.          00056400
055100            15 GCG-PAR-PENALTY-DATE       COMP-3  PIC S9(5).      00056500
055200         10 GCG-PAR-PENLTY-DT-CEN REDEFINES                       00056600
055300              GCG-PAR-PENALTY-DT          COMP-3  PIC S9(7).      00056700
055400         10 GCG-PAT-PENALTY-DT.                                   00056800
055500            15 GCG-PAT-PENLTY-DT-CC               PIC X.          00056900
055600            15 GCG-PAT-PENALTY-DATE       COMP-3  PIC S9(5).      00057000
055700         10 GCG-PAT-PENLTY-DT-CEN REDEFINES                       00057100
055800              GCG-PAT-PENALTY-DT          COMP-3  PIC S9(7).      00057200
055900         10 GCG-REIM-PENALTY-DT.                                  00057300
056000            15 GCG-REIM-PENLTY-DT-CC              PIC X.          00057400
056100            15 GCG-REIM-PENALTY-DATE      COMP-3  PIC S9(5).      00057500
056200         10 GCG-REIM-PENLTY-DT-CEN REDEFINES                      00057600
056300              GCG-REIM-PENALTY-DT         COMP-3  PIC S9(7).      00057700
056400         10 GCG-MON-DISCH-PENALTY-DT.                             00057800
056500            15 GCG-MON-DISCH-PENLTY-DT-CC         PIC X.          00057900
056600            15 GCG-MON-DISCH-PENALTY-DATE COMP-3  PIC S9(5).      00058000
056700         10 GCG-MON-DISCH-PENLTY-DT-CEN REDEFINES                 00058100
056800              GCG-MON-DISCH-PENALTY-DT    COMP-3  PIC S9(7).      00058200
056900         10 GCG-SUB-ABUSE-PENALTY-DT.                             00058300
057000            15 GCG-SUB-ABUSE-PENLTY-DT-CC         PIC X.          00058400
057100            15 GCG-SUB-ABUSE-PENALTY-DATE COMP-3  PIC S9(5).      00058500
057200         10 GCG-SUB-ABUSE-PENLTY-DT-CEN REDEFINES                 00058600
057300              GCG-SUB-ABUSE-PENALTY-DT    COMP-3  PIC S9(7).      00058700
057400         10 GCG-ST-PRGM-ACCUM-BEN-PRD-IND         PIC  X(02).     00058800
057500         10 GCG-ST-PRGM-ACCUM-L-O-B-IND           PIC  X(01).     00058900
057600         10 GCG-PRODUCT-TYPE                      PIC  X(09).     00059000
057700         10 GCG-PRODUCT-TYPE-IND                  PIC  X(01).     00059100
057800         10 GCG-NON-PLAN-COVER-IND                PIC  X(03).     00059200
057900         10 GCG-ACC-USG-CON-FEAKS-MAX-IND         PIC  X(01).     00059300
058000         10 GCG-ACC-USG-CON-FEAKS-CO-IND          PIC  X(01).     00059400
058100         10 GCG-ACC-USG-CON-FEAKS-DED-IND         PIC  X(01).     00059500
058200         10 GCG-ACC-USG-CON-FEAKS-OPX-IND         PIC  X(01).     00059600
058300         10 GCG-NEW-POS-IND                       PIC  X(02).     00059700
058400         10 GCG-NEW-POS-PENALTY-DT.                               00059800
058500            15 GCG-NEW-POS-PENLTY-DT-CC           PIC X.          00059900
058600            15 GCG-NEW-POS-PENALTY-DATE   COMP-3  PIC S9(5).      00060000
058700         10 GCG-NEW-POS-PENLTY-DT-CEN REDEFINES                   00060100
058800              GCG-NEW-POS-PENALTY-DT      COMP-3  PIC S9(7).      00060200
058900         10 GCG-NEW-MEN-SUB-ABUSE-IND             PIC  X(02).     00060300
059000         10 GCG-NEW-MEN-SUB-AB-PEN-DATE.                          00060400
059100            15 GCG-NEW-MEN-SUB-AB-PEN-DT-CC        PIC X.         00060500
059200            15 GCG-NEW-MEN-SUB-ABUSE-PEN-DT COMP-3 PIC S9(5).     00060600
059300         10 GCG-NEW-MEN-SUB-AB-PEN-DT-CEN REDEFINES               00060700
059400             GCG-NEW-MEN-SUB-AB-PEN-DATE   COMP-3 PIC S9(7).      00060800
059500         10 GCG-MAX-BASE-AMT-SOURCE-IND           PIC X.          00060900
059600         10 GCG-IPAR-PLAN-FORMAT                  PIC  X(01).     00061000
059700         10 GCG-IPAR-PLAN-TRANS-RULE              PIC  X(01).     00061100
059800         10 GCG-IPAR-PLAN-PROCESS-REQ             PIC  X(02).     00061200
059900         10 GCG-NETWORK-UTIL-REVIEW-IND           PIC  X(02).     00061300
060000         10 GCG-RPO-INDICATOR                     PIC  X(02).     00061400
060100         10 GCG-RPO-PENALTY-DT.                                   00061500
060200            15 GCG-RPO-PENLTY-DT-CC               PIC X.          00061600
060300            15 GCG-RPO-PENALTY-DATE       COMP-3  PIC S9(5).      00061700
060400         10 GCG-RPO-PENLTY-DT-CEN REDEFINES                       00061800
060500              GCG-RPO-PENALTY-DT          COMP-3  PIC S9(7).      00061900
060600         10 GCG-CPO-PARTICIPATION-IND             PIC  X(02).     00062000
060700         10 GCG-CPO-PENALTY-DT.                                   00062100
060800            15 GCG-CPO-PENLTY-DT-CC               PIC X.          00062200
060900            15 GCG-CPO-PENALTY-DATE       COMP-3  PIC S9(5).      00062300
061000         10 GCG-CPO-PENLTY-DT-CEN REDEFINES                       00062400
061100              GCG-CPO-PENALTY-DT          COMP-3  PIC S9(7).      00062500
061200         10 GCG-ELEC-PRES-DRUG-PGM-IND            PIC  X(02).     00062600
061300         10 GCG-BENEFIT-INDICATOR                 PIC  X(02).     00062700
061400         10 GCG-HEALTHY-EXPECTATIONS-IND          PIC  X(02).     00062800
061500         10 GCG-INELIG-EXCEP-PROCESS-IND          PIC  X(02).     00062900
061600         10 GCG-CBL-PARTICIPATION-IND             PIC  X(02).     00063000
061700         10 GCG-CBL-PENALTY-DT.                                   00063100
061800            15 GCG-CBL-PENLTY-DT-CC               PIC X.          00063200
061900            15 GCG-CBL-PENALTY-DATE       COMP-3  PIC S9(5).      00063300
062000         10 GCG-CBL-PENLTY-DT-CEN REDEFINES                       00063400
062100              GCG-CBL-PENALTY-DT          COMP-3  PIC S9(7).      00063500
062200         10 GCG-PAN-PARTICIPATION-IND             PIC  X(02).     00063600
062300         10 GCG-PAN-PENALTY-DT.                                   00063700
062400            15 GCG-PAN-PENLTY-DT-CC               PIC X.          00063800
062500            15 GCG-PAN-PENALTY-DATE       COMP-3  PIC S9(5).      00063900
062600         10 GCG-PAN-PENLTY-DT-CEN REDEFINES                       00064000
062700              GCG-PAN-PENALTY-DT          COMP-3  PIC S9(7).      00064100
062800         10 GCG-TRAN-TO-OTHER-RSPNBTY-IND         PIC  X(02).     00064200
062900         10 GCG-DISCOUNT-PRODUCT-TYPE             PIC  X(03).     00064300
063000         10 GCG-BC-LATE-ENROLL-MEM-DAYS   COMP-3  PIC S9(03).     00064400
063100         10 GCG-BS-LATE-ENROLL-MEM-DAYS   COMP-3  PIC S9(03).     00064500
063200         10 GCG-MM-LATE-ENROLL-MEM-DAYS   COMP-3  PIC S9(03).     00064600
063300         10 GCG-BC-LATE-ENROLL-SPS-DAYS   COMP-3  PIC S9(03).     00064700
063400         10 GCG-BS-LATE-ENROLL-SPS-DAYS   COMP-3  PIC S9(03).     00064800
063500         10 GCG-MM-LATE-ENROLL-SPS-DAYS   COMP-3  PIC S9(03).     00064900
063600         10 GCG-BC-LATE-ENROLL-DEP-DAYS   COMP-3  PIC S9(03).     00065000
063700         10 GCG-BS-LATE-ENROLL-DEP-DAYS   COMP-3  PIC S9(03).     00065100
063800         10 GCG-MM-LATE-ENROLL-DEP-DAYS   COMP-3  PIC S9(03).     00065200
063900         10 GCG-PORTABILITY-PREEXIST-IND          PIC  X(02).     00065300
064000         10 GCG-POR-PREEXIST-DT.                                  00065400
064100          15 GCG-POR-PREEXIST-DT-CC               PIC  X(01).     00065500
064200          15 GCG-PORTABILITY-PREEXIST-DATE COMP-3 PIC S9(05).     00065600
064300         10 GCG-POR-PREEXIST-DT-CEN         REDEFINES             00065700
064400            GCG-POR-PREEXIST-DT            COMP-3 PIC S9(07).     00065800
064500         10 GCG-MENTAL-HEALTH-PARITY-IND          PIC  X(02).     00065900
064600         10 GCG-MEN-HEALTH-PARITY-DT.                             00066000
064700          15 GCG-MEN-HEALTH-PARITY-DT-CC          PIC  X(01).     00066100
064800          15 GCG-MENTAL-HEALTH-PARITY-DATE COMP-3 PIC S9(05).     00066200
064900         10 GCG-MEN-HEALTH-PARITY-DT-CEN    REDEFINES             00066300
065000            GCG-MEN-HEALTH-PARITY-DT       COMP-3 PIC S9(07).     00066400
065100         10 GCG-MSPS-ACCM-BEN-PERD-IND            PIC  X(02).     00066500
065200         10 GCG-MSPS-ACCM-L-O-B-IND               PIC  X(01).     00066600
065300         10 GCG-ACC-USG-CON-FEAKS-COP-IND         PIC  X(01).     00066700
065400         10 GCG-PSEU-NBR-USG-COPAY                PIC  X(01).     00066800
065500         10 GCG-BAE-INDICATOR                     PIC  X(02).     00066900
065600         10 GCG-BAE-PENALTY-DT.                                   00067000
065700            15 GCG-BAE-PENLTY-DT-CC               PIC X.          00067100
065800            15 GCG-BAE-PENALTY-DATE       COMP-3  PIC S9(5).      00067200
065900         10 GCG-BAE-PENLTY-DT-CEN REDEFINES                       00067300
066000              GCG-BAE-PENALTY-DT          COMP-3  PIC S9(7).      00067400
066100         10 GCG-BC-CLM-CHECK-IND                  PIC X.          00067500
066200         10 GCG-BS-CLM-CHECK-IND                  PIC X.          00067600
066300         10 GCG-MM-CLM-CHECK-IND                  PIC X.          00067700
066400         10 GCG-REINSTATE-DATE.                                   00067800
066500             15 GCG-RESTATDT-CC                   PIC X.          00067900
066600             15 GCG-RESTAT-DT             COMP-3  PIC S9(5).      00068000
066700         10 GCG-RESTATDT-CEN REDEFINES GCG-REINSTATE-DATE         00068100
066800                                          COMP-3  PIC S9(7).      00068200
066900         10 GCG-HMO-MC-INDICATOR                  PIC X(02).      00068300
067000         10 GCG-HMO-MC-PENALTY-DT.                                00068400
067100            15 GCG-HMO-MC-PENLTY-DT-CC            PIC X.          00068500
067200            15 GCG-HMO-MC-PENALTY-DATE     COMP-3 PIC S9(5).      00068600
067300         10 GCG-HMO-MC-PENLTY-DT-CEN REDEFINES                    00068700
067400              GCG-HMO-MC-PENALTY-DT        COMP-3 PIC S9(7).      00068800
067500         10 GCG-COPAY-BASE-AMT-SOURCE-IND         PIC X.          00068900
067600         10 GCG-ITS-NONPAR-PRICE.                                 00069000
067700            15 GCG-BC-ITS-NONPAR-PRICE-IND           PIC X.       00069100
067800            15 GCG-BS-ITS-NONPAR-PRICE-IND           PIC X.       00069200
067900            15 GCG-MM-ITS-NONPAR-PRICE-IND           PIC X.       00069300
068000         10 GCG-ACCM-REL-IND.                                     00069400
068100            15 GCG-ACCM-REL-IND-1                    PIC X.       00069500
068200            15 GCG-ACCM-REL-IND-2                    PIC X.       00069600
068300         10 GCG-CONS-DRVN-IND                     PIC  X(02).     00069700
068400         10 GCG-CONS-DRVN-PENALTY-DT.                             00069800
068500            15 GCG-CONS-DRVN-PENLTY-DT-CC         PIC X.          00069900
068600            15 GCG-CONS-DRVN-PENALTY-DATE   COMP-3  PIC S9(5).    00070000
068700         10 GCG-CONS-DRVN-PENLTY-DT-CEN REDEFINES                 00070100
068800              GCG-CONS-DRVN-PENALTY-DT      COMP-3  PIC S9(7).    00070200
068900         10 GCG-HIAA-PRICING-PERCENT              PIC 999.        00070300
069000         10 GCG-REIMB-PRICING-TYPE                PIC XX.         00070400
069100         10 GCG-FSA-INDICATOR                     PIC X(2).       00070500
069200         10 GCG-FSA-PENALTY-DT.                                   00070600
069300            15 GCG-FSA-PENLTY-DT-CC               PIC X.          00070700
069400            15 GCG-FSA-PENALTY-DATE        COMP-3 PIC S9(5).      00070800
069500         10 GCG-FSA-PENLTY-DT-CEN REDEFINES                       00070900
069600              GCG-FSA-PENALTY-DT           COMP-3 PIC S9(7).      00071000
069700         10 GCG-HSA-INDICATOR                     PIC X(2).       00071100
069800         10 GCG-HSA-PENALTY-DT.                                   00071200
069900            15 GCG-HSA-PENLTY-DT-CC               PIC X.          00071300
070000            15 GCG-HSA-PENALTY-DATE        COMP-3 PIC S9(5).      00071400
070100         10 GCG-HSA-PENLTY-DT-CEN REDEFINES                       00071500
070200              GCG-HSA-PENALTY-DT           COMP-3 PIC S9(7).      00071600
070300         10 GCG-STACKING-IND                      PIC X(2).       00071700
070400         10 GCG-STACKING-PENALTY-DT.                              00071800
070500            15 GCG-STACKING-PENLTY-DT-CC          PIC X.          00071900
070600            15 GCG-STACKING-PENALTY-DATE   COMP-3 PIC S9(5).      00072000
070700         10 GCG-STACKING-PENLTY-DT-CEN REDEFINES                  00072100
070800              GCG-STACKING-PENALTY-DT      COMP-3 PIC S9(7).      00072200
070900         10 GCG-NON-PLAN-PRICE-IND                PIC X(01).      00072300
071000         10 GCG-NON-PLAN-PRICING-PERC-IND         PIC 9(03).      00072400
071100         10 GCG-ONLINE-ACCUM-VENDOR               PIC X(02).      00072500
071200         10 GCG-ONLINE-ACCUMS-EXCH-VALUE          PIC X(02).      00072600
071300         10 GCG-WELLNESS-HCA-IND                  PIC X(02).      00072700
071400         10 GCG-FREESTANDING-HCA-IND              PIC X(02).      00072800
071500         10 GCG-LTD-PURPOSE-FSA-IND               PIC X(02).      00072900
071600         10 GCG-LTD-PURPOSE-HCA-IND               PIC X(02).      00073000
071700         10 GCG-HSA-2-IND                         PIC X(02).      00073100
071800         10 GCG-HCA-2-IND                         PIC X(02).      00073200
071900         10 GCG-UPD-LTM-HCA-IND                   PIC X(02).      00073300
072000         10 GCG-MEMBER-FIRST-HCA                  PIC X(02).      00073400
072100         10 GCG-MULTI-VENDOR-AREA.                                00073500
072200            15 GCG-MULTI-VENDOR-1-ID.                             00073600
072300               20 GCG-MULTI-VENDOR-ID1            PIC X(02).      00073700
072400               20 GCG-MULTI-VENDOR-IN1            PIC X(02).      00073800
072500               20 GCG-MULTI-VENDOR-OUT1           PIC X(02).      00073900
072600            15 GCG-MULTI-VENDOR-2-ID.                             00074000
072700               20 GCG-MULTI-VENDOR-ID2            PIC X(02).      00074100
072800               20 GCG-MULTI-VENDOR-IN2            PIC X(02).      00074200
072900               20 GCG-MULTI-VENDOR-OUT2           PIC X(02).      00074300
073000            15 GCG-MULTI-VENDOR-3-ID.                             00074400
073100               20 GCG-MULTI-VENDOR-ID3            PIC X(02).      00074500
073200               20 GCG-MULTI-VENDOR-IN3            PIC X(02).      00074600
073300               20 GCG-MULTI-VENDOR-OUT3           PIC X(02).      00074700
073400            15 GCG-MULTI-VENDOR-4-ID.                             00074800
073500               20 GCG-MULTI-VENDOR-ID4            PIC X(02).      00074900
073600               20 GCG-MULTI-VENDOR-IN4            PIC X(02).      00075000
073700               20 GCG-MULTI-VENDOR-OUT4           PIC X(02).      00075100
073800            15 GCG-MULTI-VENDOR-5-ID.                             00075200
073900               20 GCG-MULTI-VENDOR-ID5            PIC X(02).      00075300
074000               20 GCG-MULTI-VENDOR-IN5            PIC X(02).      00075400
074100               20 GCG-MULTI-VENDOR-OUT5           PIC X(02).      00075500
074200         10 GCG-MULTI-VENDOR-DATA REDEFINES                       00075600
074300                                  GCG-MULTI-VENDOR-AREA.          00075700
074400            15 GCG-MULTI-VENDOR-OCCURS OCCURS 5 TIMES             00075800
074500               INDEXED BY GCG-MULTI-VEND-IDX.                     00075900
074600             20 GCG-VENDOR-MULT-ID                PIC X(02).      00076000
074700             20 GCG-VENDOR-MULT-INBND             PIC X(02).      00076100
074800             20 GCG-VENDOR-MULT-OUTBND            PIC X(02).      00076200
074900         10 GCG-OTHER-PRICING-PERCENT             PIC 9(03).      00076300
075000         10 GCG-OTHER-PRICING-EXPT-PERCNT         PIC 9(03).      00076400
075100         10 GCG-MULTI-VENDOR-ACCUM-RULES.                         00076500
                  15 GCG-MULTI-VENDOR-ACCUM-RULE1       PIC X(02).      00076600
                  15 GCG-MULTI-VENDOR-ACCUM-RULE2       PIC X(02).      00076700
                  15 GCG-MULTI-VENDOR-ACCUM-RULE3       PIC X(02).      00076800
                  15 GCG-MULTI-VENDOR-ACCUM-RULE4       PIC X(02).      00076900
                  15 GCG-MULTI-VENDOR-ACCUM-RULE5       PIC X(02).      00077000
               10 GCG-MULT-VEND-ACCM-RUL-DATA  REDEFINES                00077100
                  GCG-MULTI-VENDOR-ACCUM-RULES.                         00077200
                  15 GCG-MULT-VEND-ACCM-RUL-OCCRS  OCCURS  5  TIMES     00077300
                              INDEXED BY GCG-MULT-VEND-ACCM-RUL-IDX.    00077400
                     20 GCG-MULTI-VENDOR-ACCUM-RULE     PIC X(02).      00077500
               10 GCG-CORP-ADDRESS-PROTECTED-IND        PIC X(01).      00077600
               10 GCG-MULTI-VENDOR-DED-ACCUM-RUL.                       00077700
                  15 GCG-MULTI-VENDOR-DED-ACCUM1            PIC X(02).  00077800
                  15 GCG-MULTI-VENDOR-DED-ACCUM2            PIC X(02).  00077900
                  15 GCG-MULTI-VENDOR-DED-ACCUM3            PIC X(02).  00078000
                  15 GCG-MULTI-VENDOR-DED-ACCUM4            PIC X(02).  00078100
                  15 GCG-MULTI-VENDOR-DED-ACCUM5            PIC X(02).  00078200
               10 GCG-MULT-VEND-DED-ACCUM-DATA      REDEFINES           00078300
                  GCG-MULTI-VENDOR-DED-ACCUM-RUL.                       00078400
                  15 GCG-MULT-VEND-DED-ACCM-OCCRS  OCCURS  5  TIMES     00078500
                        INDEXED BY GCG-MULT-VEND-DED-IDX.               00078600
                     20 GCG-MULTI-VENDOR-DED-ACCUM          PIC X(02).  00078700
               10 GCG-MULTI-VENDOR-OPX-ACCUM-RUL.                       00078800
                  15 GCG-MULTI-VENDOR-OPX-ACCUM1            PIC X(02).  00078900
                  15 GCG-MULTI-VENDOR-OPX-ACCUM2            PIC X(02).  00079000
                  15 GCG-MULTI-VENDOR-OPX-ACCUM3            PIC X(02).  00079100
                  15 GCG-MULTI-VENDOR-OPX-ACCUM4            PIC X(02).  00079200
                  15 GCG-MULTI-VENDOR-OPX-ACCUM5            PIC X(02).  00079300
               10 GCG-MULT-VEND-OPX-ACCUM-DATA  REDEFINES               00079400
                  GCG-MULTI-VENDOR-OPX-ACCUM-RUL.                       00079500
                  15 GCG-MULT-VEND-OPX-ACCM-OCCRS  OCCURS  5  TIMES     00079600
                             INDEXED BY GCG-MULT-VEND-OPX-IDX.          00079700
                     20 GCG-MULTI-VENDOR-OPX-ACCUM          PIC X(02).  00079800
               10 GCG-MULTI-VENDOR-HUB1-AREA.                           00079900
                  15 GCG-MULTI-VENDOR-HUB1-1                PIC X(02).  00080000
                  15 GCG-MULTI-VENDOR-HUB1-2                PIC X(02).  00080100
                  15 GCG-MULTI-VENDOR-HUB1-3                PIC X(02).  00080200
                  15 GCG-MULTI-VENDOR-HUB1-4                PIC X(02).  00080300
                  15 GCG-MULTI-VENDOR-HUB1-5                PIC X(02).  00080400
               10 GCG-MULT-VEND-HUB1-DATA  REDEFINES                    00080500
                  GCG-MULTI-VENDOR-HUB1-AREA.                           00080600
                  15 GCG-MULT-VEND-HUB1-OCCRS  OCCURS  5  TIMES         00080700
                              INDEXED BY GCG-MULT-VEND-HUB1-IDX.        00080800
                     20 GCG-MULTI-VENDOR-HUB1               PIC X(02).  00080900
               10 GCG-MULTI-VENDOR-HUB2-AREA.                           00081000
                  15 GCG-MULTI-VENDOR-HUB2-1                PIC X(02).  00081100
                  15 GCG-MULTI-VENDOR-HUB2-2                PIC X(02).  00081200
                  15 GCG-MULTI-VENDOR-HUB2-3                PIC X(02).  00081300
                  15 GCG-MULTI-VENDOR-HUB2-4                PIC X(02).  00081400
                  15 GCG-MULTI-VENDOR-HUB2-5                PIC X(02).  00081500
               10 GCG-MULT-VEND-HUB2-DATA  REDEFINES                    00081600
                  GCG-MULTI-VENDOR-HUB2-AREA.                           00081700
                  15 GCG-MULT-VEND-HUB2-OCCRS  OCCURS  5  TIMES         00081800
                        INDEXED BY GCG-MULT-VEND-HUB2-IDX.              00081900
                     20 GCG-MULTI-VENDOR-HUB2               PIC X(02).  00082000
               10 GCG-PROVIDR-OF-EXCELLNCE-IND              PIC X(2).   00082010
               10 GCG-POE-PENALTY-DT.                                   00082020
                  15 GCG-POE-PENALTY-DT-CC                  PIC X.      00082030
                  15 GCG-POE-PENALTY-DATE         COMP-3    PIC S9(5).  00082040
               10 GCG-POE-PENALTY-DT-CEN  REDEFINES                     00082050
                  GCG-POE-PENALTY-DT              COMP-3    PIC S9(7).  00082060
               10 GCG-BS-NRM-NWBRN-BILG-IND                 PIC X.              
               10 GCG-MM-NRM-NWBRN-BILG-IND                 PIC X.              
      ****                                                              00082100
         10 FILLER                                          PIC X(125). 00082200
      ****                                                              00082300
               07 GCG-ENTRIES.                                          00082400
               10 GCG-GRP-SPEC-TAB-ID   OCCURS 1 TO 30 TIMES            00082500
                      DEPENDING ON GCG-COUNT-TAB-PROVN-POINTERS         00082600
                      INDEXED BY GCG-INDEX.                             00082700
                   15 GCG-TAB-ID                        PIC X(6).       00082800
                   15 GCG-TAB-SLOT-NO            COMP-3 PIC S9(7).      00082900
                                                                        P09398WK
                                                                        P09398WK
       FD  OUT-GROUPSPC-FILE                                            P02384GS
           BLOCK CONTAINS  0  RECORDS                                   P02384GS
           LABEL RECORDS ARE STANDARD                                   P02384GS
           RECORDING MODE IS V.                                         P02384GS
                                                                        P02384GS
       01  NEW-GRPSPEC-RECORD                           PIC X(1200).    P02384GS
                                                                        P02384GS
       01  NEW-FORMAT-GRPSPEC-RECORD.                                   P02384GS
           05 GCG2-GRP-SPEC-RECORD.                                        CL*12
               10 GCG2-GRP-SPECIF-ID.                                      CL*12
                   15 GCG2-PLAN-CODE                    PIC X(3).          CL*12
029611             15 GCG2-GROUP-NUM.                                   00027600
029612                20 GCG2-GROUP-NO-1-3              PIC X(3).       00027700
029613                20 GCG2-GRP-NO                    PIC X(6).       00027800
029614             15 GCG2-SECTION-NUM.                                 00027900
029615                20 GCG2-SEC-NO-1                  PIC X(1).       00028000
029616                20 GCG2-SECTN-NO                  PIC X(4).       00028100
029617             15 GCG2-PKG-CODE                     PIC X(3).       00028200
029618             15 GCG2-FAM-REL-LVL                  PIC XX.         00028300
029619             15 GCG2-EFFECTIVE-DATE.                              00028400
029620                20 GCG2-EFFDT-CC                  PIC X.          00028500
029621                20 GCG2-EFF-DT            COMP-3  PIC S9(5).      00028600
029622             15 GCG2-EFFDT-CEN REDEFINES GCG2-EFFECTIVE-DATE      00028700
029623                                          COMP-3  PIC S9(7).      00028800
029624         10 GCG2-TERM-DATE.                                       00028900
029625             15 GCG2-TERMDT-CC                    PIC X.          00029000
029626             15 GCG2-TERMN-DT             COMP-3  PIC S9(5).      00029100
029627         10 GCG2-TERMDT-CEN REDEFINES GCG2-TERM-DATE              00029200
029628                                          COMP-3  PIC S9(7).      00029300
029629         10 GCG2-DATE-LAST-CHANGE.                                00029400
029630            15 GCG2-DATE-LAST-CHANGE-CC           PIC X.          00029500
029640            15 GCG2-DT-OF-LAST-CHANGE     COMP-3  PIC S9(5).      00029600
029700         10 GCG2-DT-LAST-CHANGE-CEN REDEFINES                     00029700
029800               GCG2-DATE-LAST-CHANGE      COMP-3  PIC S9(07).     00029800
029900         10 GCG2-COUNT-TAB-PROVN-POINTERS COMP-3  PIC S999.       00029900
030000         10 GCG2-HUMAN-ORGAN-DONOR-BEN-IND        PIC X.          00030000
030100         10 GCG2-BC-TYPE-BEN-PERD-IND             PIC X.          00030100
030200         10 GCG2-BS-TYPE-BEN-PERD-IND             PIC X.          00030200
030300         10 GCG2-MM-TYPE-BEN-PERD-IND             PIC X.          00030300
030400         10 FILLER                                PIC X.          00030400
030500         10 GCG2-DEFAULT-PAYEE-IND                PIC X.          00030500
030600         10 GCG2-MCARE-LIFET-RSRV-DAYS-IND        PIC X.          00030600
030700         10 GCG2-COORD-MCARE-IND                  PIC X.          00030700
030800         10 GCG2-MCARE-DRG-PROCS-IND              PIC X.          00030800
030900         10 GCG2-BC-START-AGRMS                   PIC XX.         00030900
031000         10 GCG2-BS-START-AGRMS                   PIC XX.         00031000
031100         10 GCG2-MM-START-AGRMS                   PIC XX.         00031100
031200         10 GCG2-BC-TERMN-BEN-EXTEN-IND           PIC XX.         00031200
031300         10 GCG2-BS-TERMN-BEN-EXTEN-IND           PIC XX.         00031300
031400         10 GCG2-MM-TERMN-BEN-EXTEN-IND           PIC XX.         00031400
031500         10 GCG2-BC-WAIVR-IND                     PIC XX.         00031500
031600         10 GCG2-BS-WAIVR-IND                     PIC XX.         00031600
031700         10 GCG2-MM-WAIVR-IND                     PIC XX.         00031700
031800         10 GCG2-POS-PARTICP-IND                  PIC XX.         00031800
031900         10 FILLER                                PIC X.          00031900
032000         10 GCG2-BC-EXPENSE-FREE-IND              PIC X.          00032000
032100         10 GCG2-BS-EXPENSE-FREE-IND              PIC X.          00032100
032200         10 GCG2-MM-EXPENSE-FREE-IND              PIC X.          00032200
032300         10 GCG2-BC-EXPENSE-FREE-DAYS     COMP-3  PIC S999.       00032300
032400         10 GCG2-BS-EXPENSE-FREE-DAYS     COMP-3  PIC S999.       00032400
032500         10 GCG2-MM-EXPENSE-FREE-DAYS     COMP-3  PIC S999.       00032500
032600         10 GCG2-EOMB-REQRD-IND                   PIC X.          00032600
032700         10 GCG2-BC-FORGN-CLM-IND                 PIC X.          00032700
032800         10 GCG2-BS-FORGN-CLM-IND                 PIC X.          00032800
032900         10 GCG2-MM-FORGN-CLM-IND                 PIC X.          00032900
033000         10 GCG2-ANNL-REINST-IND                  PIC X.          00033000
033100         10 GCG2-ANNL-REINST-AMT          COMP-3  PIC S9(7).      00033100
033200         10 GCG2-GOOD-HLTH-REINST-IND             PIC X.          00033200
033300         10 GCG2-GOOD-HLTH-REINST-AFTR-BEN COMP-3 PIC S9(7).      00033300
033400         10 GCG2-FAM-SECUR-COV-IND                PIC XX.         00033400
033500         10 GCG2-DEP-TERMN-IND                    PIC X.          00033500
033600         10 GCG2-DEP-MAX-AGE              COMP-3  PIC S999.       00033600
033700         10 GCG2-STU-MAX-AGE              COMP-3  PIC S999.       00033700
033800         10 GCG2-STU-CERTN-REQRD-IND              PIC X.          00033800
033900         10 GCG2-STU-HLTH-PLAN-IND                PIC X.          00033900
034000         10 GCG2-U-C-CORRIDOR-APPL-IND            PIC X.          00034000
034100         10 GCG2-BC-CARD-REH-BIT-IND              PIC X.          00034100
034200         10 GCG2-MATER-LEAVE-IND                  PIC X.          00034200
034300         10 GCG2-BC-OB-WAITG-PERD-IND             PIC X.          00034300
034400         10 GCG2-BS-OB-WAITG-PERD-IND             PIC X.          00034400
034500         10 GCG2-MM-OB-WAITG-PERD-IND             PIC X.          00034500
034600         10 GCG2-BC-OB-WAITG-PERD-MEM-DAYS COMP-3 PIC S999.       00034600
034700         10 GCG2-BS-OB-WAITG-PERD-MEM-DAYS COMP-3 PIC S999.       00034700
034800         10 GCG2-MM-OB-WAITG-PERD-MEM-DAYS COMP-3 PIC S999.       00034800
034900         10 GCG2-BC-OB-WAITG-PERD-SPS-DAYS COMP-3 PIC S999.       00034900
035000         10 GCG2-BS-OB-WAITG-PERD-SPS-DAYS COMP-3 PIC S999.       00035000
035100         10 GCG2-MM-OB-WAITG-PERD-SPS-DAYS COMP-3 PIC S999.       00035100
035200         10 GCG2-BC-OB-WAITG-PERD-DEP-DAYS COMP-3 PIC S999.       00035200
035300         10 GCG2-BS-OB-WAITG-PERD-DEP-DAYS COMP-3 PIC S999.       00035300
035400         10 GCG2-MM-OB-WAITG-PERD-DEP-DAYS COMP-3 PIC S999.       00035400
035500         10 GCG2-WC-IND                           PIC X.          00035500
035600         10 GCG2-WC-MAR-STAT-IND                  PIC X.          00035600
035700         10 GCG2-BC-MM-WC-MIN-AMT         COMP-3  PIC S9(5).      00035700
035800         10 GCG2-BS-MM-WC-MIN-AMT         COMP-3  PIC S9(5).      00035800
035900         10 GCG2-EVIDENCE-OF-INSURANCE-IND        PIC X.          00035900
036000         10 FILLER                                PIC X.          00036000
036100         10 GCG2-WC-MAX-AGE               COMP-3  PIC S999.       00036100
036200         10 GCG2-WC-MIN-AGE               COMP-3  PIC S999.       00036200
036300         10 GCG2-POS-PENALTY-DT.                                  00036300
036400            15 GCG2-POS-PENLTY-DT-CC              PIC X.          00036400
036500            15 GCG2-POS-PENALTY-DATE      COMP-3  PIC S9(5).      00036500
036600         10 GCG2-POS-PENLTY-DT-CEN REDEFINES                      00036600
036700              GCG2-POS-PENALTY-DT         COMP-3  PIC S9(7).      00036700
036800         10 FILLER                                PIC X.          00036800
036900         10 GCG2-CHC-PRIOR-ADMISSION              PIC X.          00036900
037000         10 GCG2-DAY-PSYCH-BIT-IND                PIC X.          00037000
037100         10 FILLER                                PIC X.          00037100
037200         10 GCG2-DAY-PSYCH-FASLT-TYPE-IND         PIC X.          00037200
037300         10 GCG2-DAY-PSYCH-PART-CD                PIC X.          00037300
037400         10 GCG2-DAY-PSYCH-PRIOR-ADM-CD           PIC X.          00037400
037500         10 GCG2-DAY-PSYCH-APPRD-SVCS-CD          PIC X.          00037500
037600         10 GCG2-NIGHT-PSYCH-BIT-IND              PIC X.          00037600
037700         10 FILLER                                PIC X.          00037700
037800         10 GCG2-NIGHT-PSYCH-FCLT-TYPE-IND        PIC X.          00037800
037900         10 GCG2-NIGHT-PSYCH-PART-CD              PIC X.          00037900
038000         10 GCG2-NIGHT-PSYCH-PRIOR-ADM-CD         PIC X.          00038000
038100         10 GCG2-NIGHT-PSYCH-APPRD-SVCS-CD        PIC X.          00038100
038200         10 GCG2-MM-CARD-REHAB-BIT-IND            PIC X.          00038200
038300         10 FILLER                                PIC X.          00038300
038400         10 GCG2-MM-CARD-REH-PRIOR-ADM            PIC X.          00038400
038500         10 GCG2-MM-CARD-REH-APPRD-SVCS-CD        PIC X.          00038500
038600         10 GCG2-SUBS-ABUSE-BIT-IND               PIC X.          00038600
038700         10 FILLER                                PIC X.          00038700
038800         10 GCG2-MM-TRANSSXL-PMT-RESTR-IND        PIC X.          00038800
038900         10 GCG2-BS-TRANSSXL-PMT-RESTR-IND        PIC X.          00038900
039000         10 GCG2-SUBS-ABUSE-APPRD-SVCS-CD         PIC X.          00039000
039100         10 GCG2-ECF-SNF-BIT-IND                  PIC X.          00039100
039200         10 FILLER                                PIC X.          00039200
039300         10 GCG2-ECF-SNF-FACLT-TYPE-IND           PIC X.          00039300
039400         10 GCG2-ECF-SNF-PRIOR-ADM-CD             PIC X.          00039400
039500         10 GCG2-BC-TYPE-ADM-IND                  PIC X.          00039500
039600         10 GCG2-BS-TYPE-ADM-IND                  PIC X.          00039600
039700         10 GCG2-MM-TYPE-ADM-IND                  PIC X.          00039700
039800         10 GCG2-BC-SPEC-INVESTN-IND              PIC X.          00039800
039900         10 GCG2-DED-BASE-AMT-SOURCE-IND          PIC X.          00039900
040000         10 GCG2-MAX-LEAVES-ADM           COMP-3  PIC S9(3).      00040000
040100         10 FILLER                                PIC X.          00040100
040200         10 GCG2-BC-INTERPL-BANK-IND              PIC X.          00040200
040300         10 FILLER                                PIC X.          00040300
040400         10 GCG2-ROLLUP-IND                       PIC X.          00040400
040500         10 GCG2-U-C-CORR-AMT             COMP-3  PIC S9(5).      00040500
040600         10 GCG2-CRDT-CART-ACCEPT-IND             PIC X.          00040600
040700         10 GCG2-RFCR-ADJUSTMENT-PROC-IND         PIC X.          00040700
040800         10 GCG2-BC-TRANSSXL-PMT-RESTR-IND        PIC X.          00040800
040900         10 GCG2-EOB-GROUP-COPY-IND               PIC X.          00040900
041000         10 GCG2-BC-WAITG-PERD-IND                PIC XX.         00041000
041100         10 GCG2-BS-WAITG-PERD-IND                PIC XX.         00041100
041200         10 GCG2-MM-WAITG-PERD-IND                PIC XX.         00041200
041300         10 FILLER                                PIC X(2).       00041300
041400         10 GCG2-GROUP-SECTION-NAME               PIC X(50).      00041400
041500         10 FILLER                                PIC X(7).       00041500
041600         10 GCG2-INTER-RELATIONAL-CODE            PIC X(31).      00041600
041700         10 GCG2-INTER-RELATIONAL-CODE-X REDEFINES                00041700
041800            GCG2-INTER-RELATIONAL-CODE.                           00041800
041900            15  GCG2-INTER-RELATIONAL-CODE-1      PIC X(01).      00041900
042000            15  GCG2-INTER-RELATIONAL-CODE-2      PIC X(10).      00042000
042100            15  GCG2-INTER-RELATIONAL-CODE-3      PIC X(10).      00042100
042200            15  GCG2-INTER-RELATIONAL-CODE-4      PIC X(10).      00042200
042300         10 GCG2-L-O-B-CONTRACT-LEVEL-IND         PIC X(2).       00042300
042400         10 GCG2-PROV-CONTROL-CONT-BC-IND         PIC X(2).       00042400
042500         10 GCG2-PROV-CONTROL-CONT-BS-IND         PIC X(2).       00042500
042600         10 GCG2-PROV-CONTROL-CONT-MM-IND         PIC X(2).       00042600
042700         10 GCG2-FAM-REL-CONTROL-CONT-BC          PIC X(2).       00042700
042800         10 GCG2-FAM-REL-CONTROL-CONT-BS          PIC X(2).       00042800
042900         10 GCG2-FAM-REL-CONTROL-CONT-MM          PIC X(2).       00042900
043000         10 GCG2-GROUP-CONTROL-RNSTATE-IND        PIC X.          00043000
043100         10 GCG2-CHC-BIT-INDICATOR                PIC X(1).       00043100
043200         10 FILLER                                PIC X(1).       00043200
043300         10 GCG2-CHC-PARTICIPATION-CODE           PIC X(1).       00043300
043400         10 GCG2-TPL-INVESTIGATION-SEQ-IND        PIC X.          00043400
043500         10 GCG2-COB-CALCULATION-METHOD           PIC X.          00043500
043600         10 GCG2-COB-ACCM-BEN-PERD-IND            PIC X(2).       00043600
043700         10 GCG2-COB-ACCM-L-O-B-IND               PIC X.          00043700
043800         10 GCG2-MEDCR-ACCM-BEN-PERD-IND          PIC X(2).       00043800
043900         10 GCG2-MEDCR-ACCM-L-O-B                 PIC X.          00043900
044000         10 GCG2-WKR-COMP-L-O-B                   PIC X.          00044000
044100         10 GCG2-REIMBUR-SUBROF-ACCM-L-O-B        PIC X.          00044100
044200         10 FILLER-1                              PIC X.          00044200
044300         10 GCG2-FIRST-YR-REINST-EFF-DT           PIC X(8).       00044300
044400         10 GCG2-ACCUM-PSEUDO-GRP-NBR             PIC X(9).       00044400
044500         10 GCG2-ACCUM-PSEUDO-SECTION-NBR.                        00044500
044600            15 GCG2-ACCUM-PSEUDO-SEC-NBR          PIC X(04).      00044600
044700            15 GCG2-ACCUM-PSEUDO-SEC-NBR-1        PIC X(01).      00044700
044800         10 GCG2-PSEU-NBR-USG-BEN-AGG-MAXM        PIC X.          00044800
044900         10 GCG2-PSEU-NBR-USG-COINS-LIMITS        PIC X.          00044900
045000         10 GCG2-PSEU-NBR-USG-DEDU-LIMITS         PIC X.          00045000
045100         10 GCG2-PSEU-NBR-USG-OPX-LIMITS          PIC X.          00045100
045200         10 GCG2-BC-CARD-REH-PRIOR-ADM            PIC X.          00045200
045300         10 GCG2-BC-CARD-REH-APPRD-SVCS-CD        PIC X.          00045300
045400         10 GCG2-NRM-NWBRN-BILG-IND               PIC X.          00045400
045500         10 GCG2-BC-NRM-NWBRN-ELIG-IND            PIC X.          00045500
045600         10 GCG2-BS-NRM-NWBRN-ELIG-IND            PIC X.          00045600
045700         10 GCG2-MM-NRM-NWBRN-ELIG-IND            PIC X.          00045700
045800         10 GCG2-BC-SPCL-NWBRN-COVERAGE           PIC X.          00045800
045900         10 GCG2-BS-SPCL-NWBRN-COVERAGE           PIC X.          00045900
046000         10 GCG2-MM-SPCL-NWBRN-COVERAGE           PIC X.          00046000
046100         10 GCG2-NWBRN-AGE-LIMIT          COMP-3  PIC S9(3).      00046100
046200         10 GCG2-WC-DIAGNOSIS-BIT-IND             PIC X.          00046200
046300         10 GCG2-WC-INVESN-MODE                   PIC X.          00046300
046400         10 GCG2-UNSOLICIT-RFND-BEN-PERD          PIC X(2).       00046400
046500         10 GCG2-UNSOLICIT-RFND-L-O-B             PIC X.          00046500
046600         10 GCG2-UNSOLICIT-INT-TIME-FACTOR COMP-3 PIC S9(3).      00046600
046700         10 GCG2-TERMINAL-OB-COVERAGE-IND         PIC X.          00046700
046800         10 GCG2-ADJUSTMENT-PROCESS-AMOUNT COMP-3 PIC S9(5)V99.   00046800
046900         10 FILLER                                PIC X(11).      00046900
047000         10 GCG2-PENALTY-APPLIC-IND               PIC X(02).      00047000
047100         10 FILLER                                PIC X.          00047100
047200         10 GCG2-OUTPKT-BASE-AMT-SOURCE-IN        PIC X.          00047200
047300         10 GCG2-MULTI-PENALTY-SELECTION          PIC X.          00047300
047400         10 FILLER                                PIC X.          00047400
047500         10 GCG2-ALT-BEN-PROV-IND                 PIC X(02).      00047500
047600         10 GCG2-ALT-BEN-PROV-TIME-FACTOR COMP-3  PIC S999.       00047600
047700         10 GCG2-BC-SPEC-PROC-IND                 PIC XX.         00047700
047800         10 GCG2-BS-SPEC-PROC-IND                 PIC XX.         00047800
047900         10 GCG2-PROC-PRE-POST-PYMT-IND           PIC X.          00047900
048000         10 GCG2-BC-WAITG-PERD-MEM-DAYS   COMP-3  PIC S999.       00048000
048100         10 GCG2-BS-WAITG-PERD-MEM-DAYS   COMP-3  PIC S999.       00048100
048200         10 GCG2-MM-WAITG-PERD-MEM-DAYS   COMP-3  PIC S999.       00048200
048300         10 GCG2-BC-WAITG-PERD-SPS-DAYS   COMP-3  PIC S999.       00048300
048400         10 GCG2-BS-WAITG-PERD-SPS-DAYS   COMP-3  PIC S999.       00048400
048500         10 GCG2-MM-WAITG-PERD-SPS-DAYS   COMP-3  PIC S999.       00048500
048600         10 GCG2-BC-WAITG-PERD-DEP-DAYS   COMP-3  PIC S999.       00048600
048700         10 GCG2-BS-WAITG-PERD-DEP-DAYS   COMP-3  PIC S999.       00048700
048800         10 GCG2-MM-WAITG-PERD-DEP-DAYS   COMP-3  PIC S999.       00048800
048900         10 GCG2-COMB-COST-CONTAIN-PGM-IND       PIC X(02).       00048900
049000         10 GCG2-TIMELY-FILG-IND                  PIC X(02).      00049000
049100         10 GCG2-PARTICIPAT-PROV-OPTION           PIC X(02).      00049100
049200         10 GCG2-ADDL-TRNSPLNT-COVRG-IND          PIC X(02).      00049200
049300         10 GCG2-FRI-SAT-ADM-IND                  PIC X(02).      00049300
049400         10 GCG2-HOSPICE-IND                      PIC X(02).      00049400
049500         10 GCG2-INCENTIVE-OB-IND                 PIC X(02).      00049500
049600         10 GCG2-MAND-ADDL-SURG-OPN-IND           PIC X(02).      00049600
049700         10 GCG2-MED-NECESSITY-HCNR-IPS-IN        PIC X(02).      00049700
049800         10 GCG2-MAND-OP-SURG-PROG-IND            PIC X(02).      00049800
049900         10 GCG2-MED-SERV-ADV-PROG-IND            PIC X(02).      00049900
050000         10 GCG2-PRE-ADM-REVIEW-IND               PIC X(02).      00050000
050100         10 GCG2-PRE-ADM-TESTING-PROGRAM          PIC X(02).      00050100
050200         10 GCG2-REIMBUR-SUBROG-IND               PIC X(02).      00050200
050300         10 GCG2-SUBS-ABUSE-MENTAL-IND            PIC X(02).      00050300
050400         10 GCG2-MONDAY-DISCHARGE-IND             PIC X(02).      00050400
050500         10 GCG2-ATCP-PENALTY-DT.                                 00050500
050600            15 GCG2-ATCP-PENLTY-DT-CC             PIC X.          00050600
050700            15 GCG2-ATCP-PENALTY-DATE      COMP-3 PIC S9(05).     00050700
050800         10 GCG2-ATCP-PENLTY-DT-CEN REDEFINES                     00050800
050900              GCG2-ATCP-PENALTY-DT        COMP-3  PIC S9(7).      00050900
051000         10 GCG2-WKND-PENALTY-DT.                                 00051000
051100            15 GCG2-WKND-PENLTY-DT-CC             PIC X.          00051100
051200            15 GCG2-WKND-PENALTY-DATE      COMP-3 PIC S9(05).     00051200
051300         10 GCG2-WKND-PENLTY-DT-CEN REDEFINES                     00051300
051400              GCG2-WKND-PENALTY-DT        COMP-3  PIC S9(7).      00051400
051500         10 GCG2-HOSP-PENALTY-DT.                                 00051500
051600            15 GCG2-HOSP-PENLTY-DT-CC             PIC X.          00051600
051700            15 GCG2-HOSP-PENALTY-DATE      COMP-3 PIC S9(05).     00051700
051800         10 GCG2-HOSP-PENLTY-DT-CEN REDEFINES                     00051800
051900              GCG2-HOSP-PENALTY-DT        COMP-3  PIC S9(7).      00051900
052000         10 GCG2-INOB-PENALTY-DT.                                 00052000
052100            15 GCG2-INOB-PENLTY-DT-CC             PIC X.          00052100
052200            15 GCG2-INOB-PENALTY-DATE     COMP-3  PIC S9(5).      00052200
052300         10 GCG2-INOB-PENLTY-DT-CEN REDEFINES                     00052300
052400              GCG2-INOB-PENALTY-DT        COMP-3  PIC S9(7).      00052400
052500         10 GCG2-MASOP-PENALTY-DT.                                00052500
052600            15 GCG2-MASOP-PENLTY-DT-CC            PIC X.          00052600
052700            15 GCG2-MASOP-PENALTY-DATE    COMP-3  PIC S9(5).      00052700
052800         10 GCG2-MASOP-PENLTY-DT-CEN REDEFINES                    00052800
052900              GCG2-MASOP-PENALTY-DT       COMP-3  PIC S9(7).      00052900
053000         10 GCG2-MED-NEC-PENALTY-DT.                              00053000
053100            15 GCG2-MED-NEC-PENLTY-DT-CC          PIC X.          00053100
053200            15 GCG2-MED-NEC-PENALTY-DATE  COMP-3  PIC S9(5).      00053200
053300         10 GCG2-MED-NEC-PENLTY-DT-CEN REDEFINES                  00053300
053400              GCG2-MED-NEC-PENALTY-DT     COMP-3  PIC S9(7).      00053400
053500         10 GCG2-PPO-PENALTY-DT.                                  00053500
053600            15 GCG2-PPO-PENLTY-DT-CC              PIC X.          00053600
053700            15 GCG2-PPO-PENALTY-DATE      COMP-3  PIC S9(5).      00053700
053800         10 GCG2-PPO-PENLTY-DT-CEN REDEFINES                      00053800
053900              GCG2-PPO-PENALTY-DT         COMP-3  PIC S9(7).      00053900
054000         10 GCG2-MOPS-PENALTY-DT.                                 00054000
054100            15 GCG2-MOPS-PENLTY-DT-CC             PIC X.          00054100
054200            15 GCG2-MOPS-PENALTY-DATE     COMP-3  PIC S9(5).      00054200
054300         10 GCG2-MOPS-PENLTY-DT-CEN REDEFINES                     00054300
054400              GCG2-MOPS-PENALTY-DT        COMP-3  PIC S9(7).      00054400
054500         10 GCG2-MSA-PENALTY-DT.                                  00054500
054600            15 GCG2-MSA-PENLTY-DT-CC              PIC X.          00054600
054700            15 GCG2-MSA-PENALTY-DATE      COMP-3  PIC S9(5).      00054700
054800         10 GCG2-MSA-PENLTY-DT-CEN REDEFINES                      00054800
054900              GCG2-MSA-PENALTY-DT         COMP-3  PIC S9(7).      00054900
055000         10 GCG2-PAR-PENALTY-DT.                                  00055000
055100            15 GCG2-PAR-PENLTY-DT-CC              PIC X.          00055100
055200            15 GCG2-PAR-PENALTY-DATE      COMP-3  PIC S9(5).      00055200
055300         10 GCG2-PAR-PENLTY-DT-CEN REDEFINES                      00055300
055400              GCG2-PAR-PENALTY-DT         COMP-3  PIC S9(7).      00055400
055500         10 GCG2-PAT-PENALTY-DT.                                  00055500
055600            15 GCG2-PAT-PENLTY-DT-CC              PIC X.          00055600
055700            15 GCG2-PAT-PENALTY-DATE      COMP-3  PIC S9(5).      00055700
055800         10 GCG2-PAT-PENLTY-DT-CEN REDEFINES                      00055800
055900              GCG2-PAT-PENALTY-DT         COMP-3  PIC S9(7).      00055900
056000         10 GCG2-REIM-PENALTY-DT.                                 00056000
056100            15 GCG2-REIM-PENLTY-DT-CC             PIC X.          00056100
056200            15 GCG2-REIM-PENALTY-DATE     COMP-3  PIC S9(5).      00056200
056300         10 GCG2-REIM-PENLTY-DT-CEN REDEFINES                     00056300
056400              GCG2-REIM-PENALTY-DT        COMP-3  PIC S9(7).      00056400
056500         10 GCG2-MON-DISCH-PENALTY-DT.                            00056500
056600            15 GCG2-MON-DISCH-PENLTY-DT-CC        PIC X.          00056600
056700            15 GCG2-MON-DISCH-PENALTY-DATE COMP-3 PIC S9(5).      00056700
056800         10 GCG2-MON-DISCH-PENLTY-DT-CEN REDEFINES                00056800
056900              GCG2-MON-DISCH-PENALTY-DT   COMP-3  PIC S9(7).      00056900
057000         10 GCG2-SUB-ABUSE-PENALTY-DT.                            00057000
057100            15 GCG2-SUB-ABUSE-PENLTY-DT-CC        PIC X.          00057100
057200            15 GCG2-SUB-ABUSE-PENALTY-DATE COMP-3 PIC S9(5).      00057200
057300         10 GCG2-SUB-ABUSE-PENLTY-DT-CEN REDEFINES                00057300
057400              GCG2-SUB-ABUSE-PENALTY-DT   COMP-3  PIC S9(7).      00057400
057500         10 GCG2-ST-PRGM-ACCUM-BEN-PRD-IND        PIC  X(02).     00057500
057600         10 GCG2-ST-PRGM-ACCUM-L-O-B-IND          PIC  X(01).     00057600
057700         10 GCG2-PRODUCT-TYPE                     PIC  X(09).     00057700
057800         10 GCG2-PRODUCT-TYPE-IND                 PIC  X(01).     00057800
057900         10 GCG2-NON-PLAN-COVER-IND               PIC  X(03).     00057900
058000         10 GCG2-ACC-USG-CON-FEAKS-MAX-IND        PIC  X(01).     00058000
058100         10 GCG2-ACC-USG-CON-FEAKS-CO-IND         PIC  X(01).     00058100
058200         10 GCG2-ACC-USG-CON-FEAKS-DED-IND        PIC  X(01).     00058200
058300         10 GCG2-ACC-USG-CON-FEAKS-OPX-IND        PIC  X(01).     00058300
058400         10 GCG2-NEW-POS-IND                      PIC  X(02).     00058400
058500         10 GCG2-NEW-POS-PENALTY-DT.                              00058500
058600            15 GCG2-NEW-POS-PENLTY-DT-CC          PIC X.          00058600
058700            15 GCG2-NEW-POS-PENALTY-DATE  COMP-3  PIC S9(5).      00058700
058800         10 GCG2-NEW-POS-PENLTY-DT-CEN REDEFINES                  00058800
058900              GCG2-NEW-POS-PENALTY-DT     COMP-3  PIC S9(7).      00058900
059000         10 GCG2-NEW-MEN-SUB-ABUSE-IND            PIC  X(02).     00059000
059100         10 GCG2-NEW-MEN-SUB-AB-PEN-DATE.                         00059100
059200            15 GCG2-NEW-MEN-SUB-AB-PEN-DT-CC       PIC X.         00059200
059300            15 GCG2-NEW-MEN-SUB-ABUSE-PEN-DT COMP-3 PIC S9(5).    00059300
059400         10 GCG2-NEW-MEN-SUB-AB-PEN-DT-CEN REDEFINES              00059400
059500             GCG2-NEW-MEN-SUB-AB-PEN-DATE  COMP-3 PIC S9(7).      00059500
059600         10 GCG2-MAX-BASE-AMT-SOURCE-IND          PIC X.          00059600
059700         10 GCG2-IPAR-PLAN-FORMAT                 PIC  X(01).     00059700
059800         10 GCG2-IPAR-PLAN-TRANS-RULE             PIC  X(01).     00059800
059900         10 GCG2-IPAR-PLAN-PROCESS-REQ            PIC  X(02).     00059900
060000         10 GCG2-NETWORK-UTIL-REVIEW-IND          PIC  X(02).     00060000
060100         10 GCG2-RPO-INDICATOR                    PIC  X(02).     00060100
060200         10 GCG2-RPO-PENALTY-DT.                                  00060200
060300            15 GCG2-RPO-PENLTY-DT-CC              PIC X.          00060300
060400            15 GCG2-RPO-PENALTY-DATE      COMP-3  PIC S9(5).      00060400
060500         10 GCG2-RPO-PENLTY-DT-CEN REDEFINES                      00060500
060600              GCG2-RPO-PENALTY-DT         COMP-3  PIC S9(7).      00060600
060700         10 GCG2-CPO-PARTICIPATION-IND            PIC  X(02).     00060700
060800         10 GCG2-CPO-PENALTY-DT.                                  00060800
060900            15 GCG2-CPO-PENLTY-DT-CC              PIC X.          00060900
061000            15 GCG2-CPO-PENALTY-DATE      COMP-3  PIC S9(5).      00061000
061100         10 GCG2-CPO-PENLTY-DT-CEN REDEFINES                      00061100
061200              GCG2-CPO-PENALTY-DT         COMP-3  PIC S9(7).      00061200
061300         10 GCG2-ELEC-PRES-DRUG-PGM-IND           PIC  X(02).     00061300
061400         10 GCG2-BENEFIT-INDICATOR                PIC  X(02).     00061400
061500         10 GCG2-HEALTHY-EXPECTATIONS-IND         PIC  X(02).     00061500
061600         10 GCG2-INELIG-EXCEP-PROCESS-IND         PIC  X(02).     00061600
061700         10 GCG2-CBL-PARTICIPATION-IND            PIC  X(02).     00061700
061800         10 GCG2-CBL-PENALTY-DT.                                  00061800
061900            15 GCG2-CBL-PENLTY-DT-CC              PIC X.          00061900
062000            15 GCG2-CBL-PENALTY-DATE      COMP-3  PIC S9(5).      00062000
062100         10 GCG2-CBL-PENLTY-DT-CEN REDEFINES                      00062100
062200              GCG2-CBL-PENALTY-DT         COMP-3  PIC S9(7).      00062200
062300         10 GCG2-PAN-PARTICIPATION-IND            PIC  X(02).     00062300
062400         10 GCG2-PAN-PENALTY-DT.                                  00062400
062500            15 GCG2-PAN-PENLTY-DT-CC              PIC X.          00062500
062600            15 GCG2-PAN-PENALTY-DATE      COMP-3  PIC S9(5).      00062600
062700         10 GCG2-PAN-PENLTY-DT-CEN REDEFINES                      00062700
062800              GCG2-PAN-PENALTY-DT         COMP-3  PIC S9(7).      00062800
062900         10 GCG2-TRAN-TO-OTHER-RSPNBTY-IND        PIC  X(02).     00062900
063000         10 GCG2-DISCOUNT-PRODUCT-TYPE            PIC  X(03).     00063000
063100         10 GCG2-BC-LATE-ENROLL-MEM-DAYS  COMP-3  PIC S9(03).     00063100
063200         10 GCG2-BS-LATE-ENROLL-MEM-DAYS  COMP-3  PIC S9(03).     00063200
063300         10 GCG2-MM-LATE-ENROLL-MEM-DAYS  COMP-3  PIC S9(03).     00063300
063400         10 GCG2-BC-LATE-ENROLL-SPS-DAYS  COMP-3  PIC S9(03).     00063400
063500         10 GCG2-BS-LATE-ENROLL-SPS-DAYS  COMP-3  PIC S9(03).     00063500
063600         10 GCG2-MM-LATE-ENROLL-SPS-DAYS  COMP-3  PIC S9(03).     00063600
063700         10 GCG2-BC-LATE-ENROLL-DEP-DAYS  COMP-3  PIC S9(03).     00063700
063800         10 GCG2-BS-LATE-ENROLL-DEP-DAYS  COMP-3  PIC S9(03).     00063800
063900         10 GCG2-MM-LATE-ENROLL-DEP-DAYS  COMP-3  PIC S9(03).     00063900
064000         10 GCG2-PORTABILITY-PREEXIST-IND         PIC  X(02).     00064000
064100         10 GCG2-POR-PREEXIST-DT.                                 00064100
064200          15 GCG2-POR-PREEXIST-DT-CC              PIC  X(01).     00064200
064300          15 GCG2-PORTABILITY-PREEXIST-DATE COMP-3 PIC S9(05).    00064300
064400         10 GCG2-POR-PREEXIST-DT-CEN        REDEFINES             00064400
064500            GCG2-POR-PREEXIST-DT           COMP-3 PIC S9(07).     00064500
064600         10 GCG2-MENTAL-HEALTH-PARITY-IND         PIC  X(02).     00064600
064700         10 GCG2-MEN-HEALTH-PARITY-DT.                            00064700
064800          15 GCG2-MEN-HEALTH-PARITY-DT-CC         PIC  X(01).     00064800
064900          15 GCG2-MENTAL-HEALTH-PARITY-DATE COMP-3 PIC S9(05).    00064900
065000         10 GCG2-MEN-HEALTH-PARITY-DT-CEN   REDEFINES             00065000
065100            GCG2-MEN-HEALTH-PARITY-DT      COMP-3 PIC S9(07).     00065100
065200         10 GCG2-MSPS-ACCM-BEN-PERD-IND           PIC  X(02).     00065200
065300         10 GCG2-MSPS-ACCM-L-O-B-IND              PIC  X(01).     00065300
065400         10 GCG2-ACC-USG-CON-FEAKS-COP-IND        PIC  X(01).     00065400
065500         10 GCG2-PSEU-NBR-USG-COPAY               PIC  X(01).     00065500
065600         10 GCG2-BAE-INDICATOR                    PIC  X(02).     00065600
065700         10 GCG2-BAE-PENALTY-DT.                                  00065700
065800            15 GCG2-BAE-PENLTY-DT-CC              PIC X.          00065800
065900            15 GCG2-BAE-PENALTY-DATE      COMP-3  PIC S9(5).      00065900
066000         10 GCG2-BAE-PENLTY-DT-CEN REDEFINES                      00066000
066100              GCG2-BAE-PENALTY-DT         COMP-3  PIC S9(7).      00066100
066200         10 GCG2-BC-CLM-CHECK-IND                 PIC X.          00066200
066300         10 GCG2-BS-CLM-CHECK-IND                 PIC X.          00066300
066400         10 GCG2-MM-CLM-CHECK-IND                 PIC X.          00066400
066500         10 GCG2-REINSTATE-DATE.                                  00066500
066600             15 GCG2-RESTATDT-CC                   PIC X.         00066600
066700             15 GCG2-RESTAT-DT             COMP-3  PIC S9(5).     00066700
066800         10 GCG2-RESTATDT-CEN REDEFINES GCG2-REINSTATE-DATE       00066800
066900                                          COMP-3  PIC S9(7).      00066900
067000         10 GCG2-HMO-MC-INDICATOR                 PIC X(02).      00067000
067100         10 GCG2-HMO-MC-PENALTY-DT.                               00067100
067200            15 GCG2-HMO-MC-PENLTY-DT-CC           PIC X.          00067200
067300            15 GCG2-HMO-MC-PENALTY-DATE    COMP-3 PIC S9(5).      00067300
067400         10 GCG2-HMO-MC-PENLTY-DT-CEN REDEFINES                   00067400
067500              GCG2-HMO-MC-PENALTY-DT       COMP-3 PIC S9(7).      00067500
067600         10 GCG2-COPAY-BASE-AMT-SOURCE-IND        PIC X.          00067600
067700         10 GCG2-ITS-NONPAR-PRICE.                                00067700
067800            15 GCG2-BC-ITS-NONPAR-PRICE-IND          PIC X.       00067800
067900            15 GCG2-BS-ITS-NONPAR-PRICE-IND          PIC X.       00067900
068000            15 GCG2-MM-ITS-NONPAR-PRICE-IND          PIC X.       00068000
068100         10 GCG2-ACCM-REL-IND.                                    00068100
068200            15 GCG2-ACCM-REL-IND-1                   PIC X.       00068200
068300            15 GCG2-ACCM-REL-IND-2                   PIC X.       00068300
068400         10 GCG2-CONS-DRVN-IND                     PIC  X(02).    00068400
068500         10 GCG2-CONS-DRVN-PENALTY-DT.                            00068500
068600            15 GCG2-CONS-DRVN-PENLTY-DT-CC         PIC X.         00068600
068700            15 GCG2-CONS-DRVN-PENALTY-DATE  COMP-3 PIC S9(5).     00068700
068800         10 GCG2-CONS-DRVN-PENLTY-DT-CEN REDEFINES                00068800
068900              GCG2-CONS-DRVN-PENALTY-DT     COMP-3  PIC S9(7).    00068900
069000         10 GCG2-HIAA-PRICING-PERCENT               PIC 999.      00069000
069100         10 GCG2-REIMB-PRICING-TYPE                 PIC XX.       00069100
069200         10 GCG2-FSA-INDICATOR                      PIC XX.       00069200
069300         10 GCG2-FSA-PENALTY-DT.                                  00069300
069400            15 GCG2-FSA-PENLTY-DT-CC                PIC X.        00069400
069500            15 GCG2-FSA-PENALTY-DATE        COMP-3 PIC S9(5).     00069500
069600         10 GCG2-FSA-PENLTY-DT-CEN REDEFINES                      00069600
069700              GCG2-FSA-PENALTY-DT           COMP-3 PIC S9(7).     00069700
069800         10 GCG2-HSA-INDICATOR                      PIC XX.       00069800
069900         10 GCG2-HSA-PENALTY-DT.                                  00069900
070000            15 GCG2-HSA-PENLTY-DT-CC                PIC X.        00070000
070100            15 GCG2-HSA-PENALTY-DATE        COMP-3 PIC S9(5).     00070100
070200         10 GCG2-HSA-PENLTY-DT-CEN REDEFINES                      00070200
070300              GCG2-HSA-PENALTY-DT           COMP-3 PIC S9(7).     00070300
070400         10 GCG2-STACKING-IND                       PIC XX.       00070400
070500         10 GCG2-STACKING-PENALTY-DT.                             00070500
070600            15 GCG2-STACKING-PENLTY-DT-CC           PIC X.        00070600
070700            15 GCG2-STACKING-PENALTY-DATE   COMP-3 PIC S9(5).     00070700
070800         10 GCG2-STACKING-PENLTY-DT-CEN REDEFINES                 00070800
070900              GCG2-STACKING-PENALTY-DT      COMP-3 PIC S9(7).     00070900
071000         10 GCG2-NON-PLAN-PRICE-IND                 PIC X(01).    00071000
071100         10 GCG2-NON-PLAN-PRICING-PERC-IND          PIC 9(03).    00071100
071200         10 GCG2-ONLINE-ACCUM-VENDOR                PIC X(02).    00071200
071300         10 GCG2-ONLINE-ACCUMS-EXCH-VALUE           PIC X(02).    00071300
071400         10 GCG2-WELLNESS-HCA-IND                   PIC X(02).    00071400
071500         10 GCG2-FREESTANDING-HCA-IND               PIC X(02).    00071500
071600         10 GCG2-LTD-PURPOSE-FSA-IND                PIC X(02).    00071600
071700         10 GCG2-LTD-PURPOSE-HCA-IND                PIC X(02).    00071700
071800         10 GCG2-HSA-2-IND                          PIC X(02).    00071800
071900         10 GCG2-HCA-2-IND                          PIC X(02).    00071900
072000         10 GCG2-UPD-LTM-HCA-IND                    PIC X(02).    00072000
072100         10 GCG2-MEMBER-FIRST-HCA                   PIC X(02).    00072100
072200         10 GCG2-MULTI-VENDOR-AREA.                               00072200
072300            15 GCG2-MULTI-VENDOR-1-ID.                            00072300
072400               20 GCG2-MULTI-VENDOR-ID1             PIC X(02).    00072400
072500               20 GCG2-MULTI-VENDOR-IN1             PIC X(02).    00072500
072600               20 GCG2-MULTI-VENDOR-OUT1            PIC X(02).    00072600
072700            15 GCG2-MULTI-VENDOR-2-ID.                            00072700
072800               20 GCG2-MULTI-VENDOR-ID2             PIC X(02).    00072800
072900               20 GCG2-MULTI-VENDOR-IN2             PIC X(02).    00072900
073000               20 GCG2-MULTI-VENDOR-OUT2            PIC X(02).    00073000
073100            15 GCG2-MULTI-VENDOR-3-ID.                            00073100
073200               20 GCG2-MULTI-VENDOR-ID3             PIC X(02).    00073200
073300               20 GCG2-MULTI-VENDOR-IN3             PIC X(02).    00073300
073400               20 GCG2-MULTI-VENDOR-OUT3            PIC X(02).    00073400
073500            15 GCG2-MULTI-VENDOR-4-ID.                            00073500
073600               20 GCG2-MULTI-VENDOR-ID4             PIC X(02).    00073600
073700               20 GCG2-MULTI-VENDOR-IN4             PIC X(02).    00073700
073800               20 GCG2-MULTI-VENDOR-OUT4            PIC X(02).    00073800
073900            15 GCG2-MULTI-VENDOR-5-ID.                            00073900
074000               20 GCG2-MULTI-VENDOR-ID5             PIC X(02).    00074000
074100               20 GCG2-MULTI-VENDOR-IN5             PIC X(02).    00074100
074200               20 GCG2-MULTI-VENDOR-OUT5            PIC X(02).    00074200
074300         10 GCG2-MULTI-VENDOR-DATA REDEFINES                      00074300
074400                                   GCG2-MULTI-VENDOR-AREA.        00074400
074500            15 GCG2-MULTI-VENDOR-OCCURS OCCURS 5 TIMES            00074500
074600               INDEXED BY GCG2-MULTI-VEND-IDX.                    00074600
074700             20 GCG2-VENDOR-MULT-ID                 PIC X(02).    00074700
074800             20 GCG2-VENDOR-MULT-INBND              PIC X(02).    00074800
074900             20 GCG2-VENDOR-MULT-OUTBND             PIC X(02).    00074900
075000         10 GCG2-OTHER-PRICING-PERCENT              PIC 9(03).    00075000
075100         10 GCG2-OTHER-PRICING-EXPT-PERCNT          PIC 9(03).    00075100
               10 GCG2-MULTI-VENDOR-ACCUM-RULES.                        00075104
                  15 GCG2-MULTI-VENDOR-ACCUM-RULE1        PIC X(02).    00075105
                  15 GCG2-MULTI-VENDOR-ACCUM-RULE2        PIC X(02).    00075106
                  15 GCG2-MULTI-VENDOR-ACCUM-RULE3        PIC X(02).    00075107
                  15 GCG2-MULTI-VENDOR-ACCUM-RULE4        PIC X(02).    00075108
                  15 GCG2-MULTI-VENDOR-ACCUM-RULE5        PIC X(02).    00075109
               10 GCG2-MULT-VEND-ACCM-RUL-DATA  REDEFINES               00075110
                  GCG2-MULTI-VENDOR-ACCUM-RULES.                        00075111
                  15 GCG2-MULT-VEND-ACCM-RUL-OCCRS  OCCURS  5  TIMES    00075112
                              INDEXED BY GCG2-MULT-VEND-ACCM-RUL-IDX.   00075113
                     20 GCG2-MULTI-VENDOR-ACCUM-RULE      PIC X(02).    00075114
               10 GCG2-CORP-ADDRES-PROTECTED-IND          PIC X(01).    00075115
               10 GCG2-MULT-VENDOR-DED-ACCUM-RUL.                       00075116
                  15 GCG2-MULTI-VENDOR-DED-ACCUM1           PIC X(02).  00075117
                  15 GCG2-MULTI-VENDOR-DED-ACCUM2           PIC X(02).  00075118
                  15 GCG2-MULTI-VENDOR-DED-ACCUM3           PIC X(02).  00075119
                  15 GCG2-MULTI-VENDOR-DED-ACCUM4           PIC X(02).  00075120
                  15 GCG2-MULTI-VENDOR-DED-ACCUM5           PIC X(02).  00075121
               10 GCG2-MULT-VEND-DED-ACCUM-DATA     REDEFINES           00075122
                  GCG2-MULT-VENDOR-DED-ACCUM-RUL.                       00075123
                  15 GCG2-MULT-VEND-DED-ACCM-OCCRS OCCURS  5  TIMES     00075124
                             INDEXED BY GCG2-MULT-VEND-DED-IDX.         00075125
                     20 GCG2-MULTI-VENDOR-DED-ACCUM         PIC X(02).  00075126
               10 GCG2-MULT-VENDOR-OPX-ACCUM-RUL.                       00075127
                  15 GCG2-MULTI-VENDOR-OPX-ACCUM1           PIC X(02).  00075128
                  15 GCG2-MULTI-VENDOR-OPX-ACCUM2           PIC X(02).  00075129
                  15 GCG2-MULTI-VENDOR-OPX-ACCUM3           PIC X(02).  00075130
                  15 GCG2-MULTI-VENDOR-OPX-ACCUM4           PIC X(02).  00075131
                  15 GCG2-MULTI-VENDOR-OPX-ACCUM5           PIC X(02).  00075132
               10 GCG2-MULT-VEND-OPX-ACCUM-DATA REDEFINES               00075133
                  GCG2-MULT-VENDOR-OPX-ACCUM-RUL.                       00075134
                  15 GCG2-MULT-VEND-OPX-ACCM-OCCRS OCCURS  5  TIMES     00075135
                             INDEXED BY GCG2-MULT-VEND-OPX-IDX.         00075136
                     20 GCG2-MULTI-VENDOR-OPX-ACCUM         PIC X(02).  00075137
               10 GCG2-MULTI-VENDOR-HUB1-AREA.                          00075138
                  15 GCG2-MULTI-VENDOR-HUB1-1               PIC X(02).  00075139
                  15 GCG2-MULTI-VENDOR-HUB1-2               PIC X(02).  00075140
                  15 GCG2-MULTI-VENDOR-HUB1-3               PIC X(02).  00075141
                  15 GCG2-MULTI-VENDOR-HUB1-4               PIC X(02).  00075142
                  15 GCG2-MULTI-VENDOR-HUB1-5               PIC X(02).  00075143
               10 GCG2-MULT-VEND-HUB1-DATA REDEFINES                    00075144
                  GCG2-MULTI-VENDOR-HUB1-AREA.                          00075145
                  15 GCG2-MULT-VEND-HUB1-OCCRS OCCURS  5  TIMES         00075146
                             INDEXED BY GCG2-MULT-VEND-HUB1-IDX.        00075147
                     20 GCG2-MULTI-VENDOR-HUB1              PIC X(02).  00075148
               10 GCG2-MULTI-VENDOR-HUB2-AREA.                          00075149
                  15 GCG2-MULTI-VENDOR-HUB2-1               PIC X(02).  00075150
                  15 GCG2-MULTI-VENDOR-HUB2-2               PIC X(02).  00075151
                  15 GCG2-MULTI-VENDOR-HUB2-3               PIC X(02).  00075152
                  15 GCG2-MULTI-VENDOR-HUB2-4               PIC X(02).  00075153
                  15 GCG2-MULTI-VENDOR-HUB2-5               PIC X(02).  00075154
               10 GCG2-MULT-VEND-HUB2-DATA REDEFINES                    00075155
                  GCG2-MULTI-VENDOR-HUB2-AREA.                          00075156
                  15 GCG2-MULT-VEND-HUB2-OCCRS OCCURS  5  TIMES         00075157
                        INDEXED BY GCG2-MULT-VEND-HUB2-IDX.             00075158
                     20 GCG2-MULTI-VENDOR-HUB2              PIC X(02).  00075159
               10 GCG2-PROVIDR-OF-EXCELLNCE-IND             PIC X(2).   00075160
               10 GCG2-POE-PENALTY-DT.                                  00075161
                  15 GCG2-POE-PENALTY-DT-CC                 PIC X.      00075162
                  15 GCG2-POE-PENALTY-DATE        COMP-3    PIC S9(5).  00075163
               10 GCG2-POE-PENALTY-DT-CEN REDEFINES                     00075164
                  GCG2-POE-PENALTY-DT             COMP-3    PIC S9(7).  00075165
               10 GCG2-BS-NRM-NWBRN-BILG-IND                PIC X.              
               10 GCG2-MM-NRM-NWBRN-BILG-IND                PIC X.              
               10 GCG2-TOTAL-CARE-IND                       PIC XX.     00082100
      ****                                                              00075166
               10 FILLER                                    PIC X(123). 00075167
      ****                                                              00075170
               07 GCG2-ENTRIES.                                         00075700
               10 GCG2-GRP-SPEC-TAB-ID  OCCURS 1 TO 30 TIMES            00075800
                      DEPENDING ON GCG2-COUNT-TAB-PROVN-POINTERS        00075900
                      INDEXED BY GCG2-INDEX.                            00076000
                   15 GCG2-TAB-ID                         PIC X(6).     00076100
                   15 GCG2-TAB-SLOT-NO           COMP-3   PIC S9(7).    00076200
                                                                        00000200
                                                                           CL**5
       WORKING-STORAGE SECTION.                                         P02384GS
       01  FILLER                               PIC  X(32) VALUE        P02384GS
                                   'WORKING STORAGE PROGRAM CONVERTG'.  P02384GS
                                                                        P02384GS
       01  FILLER                               PIC  X(18) VALUE        P02384GS
                                                  '  WS-ABEND-CODE  '.  P02384GS
       01  WS-ABEND-CODE                        PIC X(04) VALUE ZEROES. P02384GS
                                                                        P02384GS
       01  WS-PROGRAM-COUNTERS.                                         P02384GS
           05  WS-GRP-READ-OLD-FORMAT-RECS       PIC 9(08) VALUE ZEROES.P02384GS
           05  WS-GRP-NEW-FORMAT-WRITTEN         PIC 9(08) VALUE ZEROES.P02384GS
                                                                        P02384GS
       01  WS-PROGRAM-SWITCHES.                                         P02384GS
           05  WS-INPUT-END-OF-FILE-SWITCH       PIC X(01)  VALUE 'N'.  P02384GS
               88  WS-INPUT-EOF-SWITCH-OFF                  VALUE 'N'.  P02384GS
               88  WS-INPUT-EOF-SWITCH-ON                   VALUE 'Y'.  P02384GS
                                                                        P02384GS
                                                                        P02384GS
      ****************************                                      P02384GS
      *        MAIN  LINE        *                                      P02384GS
      ****************************                                      P02384GS
       PROCEDURE DIVISION.                                              P02384GS
       0000-MAINLINE.                                                   P02384GS
                                                                        P02384GS
           PERFORM 8000-OPEN-FILES               THRU  8000-EXIT.       P02384GS
                                                                        P02384GS
           IF WS-INPUT-EOF-SWITCH-ON                                    P02384GS
              PERFORM 7000-DISPLAY-ABEND-INFO    THRU  7000-EXIT        P02384GS
           ELSE                                                         P02384GS
              PERFORM 1000-PROCESS-INPUT-RECORD  THRU  1000-EXIT        P02384GS
                UNTIL WS-INPUT-EOF-SWITCH-ON                            P02384GS
              PERFORM 6000-DISPLAY-STATISTICS    THRU  6000-EXIT.       P02384GS
                                                                        P02384GS
           PERFORM 9000-CLOSE-SEQ-FILES          THRU  9000-EXIT.       P02384GS
                                                                        P02384GS
           GOBACK.                                                      P02384GS
                                                                        P02384GS
       0000-EXIT.  EXIT.                                                P02384GS
                                                                        P02384GS
      ****************************                                      P02384GS
      *   PROCESS INPUT RECORD   *                                      P02384GS
      ****************************                                      P02384GS
       1000-PROCESS-INPUT-RECORD.                                       P02384GS
                                                                        P02384GS
           PERFORM 2000-MOVE-INPUT-TO-OUTPUT  THRU  2000-EXIT.          P02384GS
           PERFORM 4000-READ-INPUT-FILE       THRU  4000-EXIT.          P02384GS
                                                                        P02384GS
       1000-EXIT.  EXIT.                                                P02384GS
                                                                        P02384GS
      ****************************                                      P02384GS
      *     BUILD  OUT  RECORD   *                                      P02384GS
      ****************************                                      P02384GS
       2000-MOVE-INPUT-TO-OUTPUT.                                       P02384GS
                                                                        P02384GS
           MOVE SPACES               TO NEW-GRPSPEC-RECORD.             P02384GS
                                                                        P02384GS
           MOVE GCG-COUNT-TAB-PROVN-POINTERS                            P02384GS
             TO GCG2-COUNT-TAB-PROVN-POINTERS.                          P02384GS
                                                                        P02384GS
           MOVE GCG-GRP-SPEC-RECORD  TO  GCG2-GRP-SPEC-RECORD.          P02384GS
                                                                        P02384GS
           IF (( GCG2-TOTAL-CARE-IND  =  HIGH-VALUES )  OR                 CL*10
               ( GCG2-TOTAL-CARE-IND  =  LOW-VALUES )   OR                 CL*10
               ( GCG2-TOTAL-CARE-IND  =  SPACES ))                         CL*10
                                                                           CL**9
                 MOVE '00'           TO  GCG2-TOTAL-CARE-IND               CL**9
           END-IF.                                                         CL*10
                                                                           CL**9
                                                                           CL*16
           PERFORM 5000-WRITE-OUTPUT-RECORD  THRU  5000-EXIT.           P02384GS
                                                                        P02384GS
       2000-EXIT.  EXIT.                                                P02384GS
                                                                        P02384GS
      ****************************                                      P02384GS
      *    READ  INPUT  FILE     *                                      P02384GS
      ****************************                                      P02384GS
       4000-READ-INPUT-FILE.                                            P02384GS
                                                                        P02384GS
           READ IN-GROUPSPC-FILE                                        P02384GS
               AT END                                                   P02384GS
                 MOVE 'Y'   TO  WS-INPUT-END-OF-FILE-SWITCH             P02384GS
                   GO TO 4000-EXIT.                                     P02384GS
                                                                        P02384GS
           ADD 1 TO WS-GRP-READ-OLD-FORMAT-RECS.                        P02384GS
                                                                        P02384GS
       4000-EXIT.  EXIT.                                                P02384GS
                                                                        P02384GS
      ****************************                                      P02384GS
      *    WRITE  OUT  RECORD    *                                      P02384GS
      ****************************                                      P02384GS
       5000-WRITE-OUTPUT-RECORD.                                        P02384GS
                                                                        P02384GS
           WRITE NEW-FORMAT-GRPSPEC-RECORD.                             P02384GS
           ADD 1 TO WS-GRP-NEW-FORMAT-WRITTEN.                          P02384GS
                                                                        P02384GS
       5000-EXIT.  EXIT.                                                P02384GS
                                                                        P02384GS
      ****************************                                      P02384GS
      *     DISPLAY   TOTALS     *                                      P02384GS
      ****************************                                      P02384GS
       6000-DISPLAY-STATISTICS.                                         P02384GS
                                                                        P02384GS
           DISPLAY '  '.                                                P02384GS
           DISPLAY 'TOTAL INPUT RECORDS READ      =  '                  P02384GS
                    WS-GRP-READ-OLD-FORMAT-RECS.                        P02384GS
           DISPLAY 'TOTAL OUTPUT RECORDS WRITEN   =  '                  P02384GS
                    WS-GRP-NEW-FORMAT-WRITTEN.                          P02384GS
           DISPLAY '  '.                                                P02384GS
                                                                        P02384GS
       6000-EXIT.  EXIT.                                                P02384GS
                                                                        P02384GS
      ****************************                                      P02384GS
      *      DISPLAY  ABENDS     *                                      P02384GS
      ****************************                                      P02384GS
       7000-DISPLAY-ABEND-INFO.                                         P02384GS
                                                                        P02384GS
           DISPLAY '  '.                                                P02384GS
           DISPLAY ' CONVERTG PROGRAM ABENDED '.                        P02384GS
           DISPLAY ' INPUT FILE IS EMPTY'.                              P02384GS
           MOVE 1000  TO  WS-ABEND-CODE.                                P02384GS
           PERFORM 9999-USER-ABORT  THRU  9999-EXIT.                    P02384GS
                                                                        P02384GS
       7000-EXIT.  EXIT.                                                P02384GS
                                                                        P02384GS
      ****************************                                      P02384GS
      *  OPEN / READ  SEQ  FILE  *                                      P02384GS
      ****************************                                      P02384GS
       8000-OPEN-FILES.                                                 P02384GS
                                                                        P02384GS
           OPEN  INPUT IN-GROUPSPC-FILE                                 P02384GS
                OUTPUT OUT-GROUPSPC-FILE.                               P02384GS
                                                                        P02384GS
           PERFORM 4000-READ-INPUT-FILE THRU 4000-EXIT.                 P02384GS
                                                                        P02384GS
       8000-EXIT.  EXIT.                                                P02384GS
                                                                        P02384GS
      ****************************                                      P02384GS
      *  CLOSE  IN / OUT  FILES  *                                      P02384GS
      ****************************                                      P02384GS
       9000-CLOSE-SEQ-FILES.                                            P02384GS
                                                                        P02384GS
           CLOSE IN-GROUPSPC-FILE                                       P02384GS
                 OUT-GROUPSPC-FILE.                                     P02384GS
                                                                        P02384GS
       9000-EXIT.  EXIT.                                                P02384GS
                                                                        P02384GS
      ****************************                                      P02384GS
      *  ABORT  -  FATAL  ERROR  *                                      P02384GS
      ****************************                                      P02384GS
       9999-USER-ABORT.                                                 P02384GS
                                                                        P02384GS
           CALL 'TSGEND' USING WS-ABEND-CODE.                           P02384GS
                                                                        P02384GS
       9999-EXIT.  EXIT.                                                P02384GS
                                                                        P02384GS
