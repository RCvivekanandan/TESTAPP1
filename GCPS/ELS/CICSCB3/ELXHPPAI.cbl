00001  ID DIVISION.                                                     12/13/05
00002  PROGRAM-ID.     ELXHPPAI.                                        ELXHPPAI
00003  AUTHOR.         ANNE KING.                                          LV002
00004  DATE-WRITTEN.   04/24/01.                                        ELXHPPAI
00005  DATE-COMPILED.                                                   ELXHPPAI
00006 ******************************************************************ELXHPPAI
00007 *                                                                *ELXHPPAI
00008 *        M A I N T E N A N C E     L O G                         *ELXHPPAI
00009 *                                                                *ELXHPPAI
00010 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELXHPPAI
00011 *                                                                *ELXHPPAI
00012 *            04/24/01  AKK  CREATED HIPPA INTERFACE TO PMCI.     *ELXHPPAI
00013 *                                                                *ELXHPPAI
00014 *            04/01/03  AKK  CHANGES IN ENDEVOR.                  *ELXHPPAI
00015 *                                                                *ELXHPPAI
00016 *            08/27/03  AKK  EDIT TO CREATE NEW LEVEL FOR MOVE TO *ELXHPPAI
00017 *                           PREPROD                              *ELXHPPAI
00018 *                                                                *ELXHPPAI
00019 *            12/02/03  AKK  REGEN TO PICK UP NEW PMCIF EXECUTABLE*ELXHPPAI
00020 *                                                                *ELXHPPAI
00021 *            01/27/04  AKK  REGEN DUE TO COMPILER FIX            *ELXHPPAI
00022 *                                                                *ELXHPPAI
00023 ******************************************************************ELXHPPAI
00024 /*****************************************************************ELXHPPAI
00025 *      P R O G R A M   N A R R A T I V E                         *ELXHPPAI
00026 ******************************************************************ELXHPPAI
00027 *                                                                *ELXHPPAI
00028 *   PURPOSE:   HANDLE THE PASSING OF DATA BETWEEN HIPPA AND      *ELXHPPAI
00029 *              PMCI.                                             *ELXHPPAI
00030 *                                                                *ELXHPPAI
00031 *   FUNCTIONS: THIS MODULE IS CALLED FROM INTERNAL HIPPA TRANS   *ELXHPPAI
00032 *              AND TEST TRANS ELHP.                              *ELXHPPAI
00033 *                                                                *ELXHPPAI
00034 ******************************************************************ELXHPPAI
00035 /                                                                 ELXHPPAI
00036  ENVIRONMENT DIVISION.                                            ELXHPPAI
00037  DATA DIVISION.                                                   ELXHPPAI
00038                                                                   ELXHPPAI
00039  WORKING-STORAGE SECTION.                                         ELXHPPAI
00040  01  WS-BEGIN                    PIC X(58) VALUE                  ELXHPPAI
00041      '*** ELXHPPAI WORKING-STORAGE BEGINS HERE ***'.              ELXHPPAI
00042                                                                   ELXHPPAI
00043  01  WS-FIELDS.                                                   ELXHPPAI
00044      05  WS-BEN-IDX              PIC S9(02) COMP-3.               ELXHPPAI
00045                                                                   ELXHPPAI
00046 /-------- MILLENNIUM DATE ROUTINE COMMAREA --------------------*  ELXHPPAI
00047  COPY MLDATE01.                                                   ELXHPPAI
00048                                                                   ELXHPPAI
00049 /                                                                 ELXHPPAI
00050  01  WS-END                       PIC X(58) VALUE                 ELXHPPAI
00051      '*** ELXHPPAI WORKING-STORAGE ENDS HERE ***'.                ELXHPPAI
00052 /                                                                 ELXHPPAI
00053 *--- COMMAREA PASSED TO ELXPMCIF --------------------------------*ELXHPPAI
00054  01  PMCI-COMM-AREA.                                              ELXHPPAI
00055      COPY PMCCOMM.                                                ELXHPPAI
00056 /                                                                 ELXHPPAI
00057  LINKAGE SECTION.                                                 ELXHPPAI
00058                                                                   ELXHPPAI
00059 *--- COMMAREA PASSED FROM CALLER --------------------------------*ELXHPPAI
00060  01  DFHCOMMAREA.                                                 ELXHPPAI
00061      COPY HPPCOMM.                                                ELXHPPAI
00062 /                                                                 ELXHPPAI
00063  PROCEDURE DIVISION.                                              ELXHPPAI
00064                                                                   ELXHPPAI
00065 ************************************************************      ELXHPPAI
00066 *                                                          *      ELXHPPAI
00067 *    ELXHPPAI MAINLINE                                     *      ELXHPPAI
00068 *                                                          *      ELXHPPAI
00069 ************************************************************      ELXHPPAI
00070  0000-MAINLINE.                                                   ELXHPPAI
00071      PERFORM 0001-PROCESS-CONTROL THRU 0001-EXIT.                 ELXHPPAI
00072      PERFORM 1000-PROCESS-BENEFITS THRU 1000-EXIT.                ELXHPPAI
00073                                                                   ELXHPPAI
00074      EXEC CICS RETURN END-EXEC.                                   ELXHPPAI
00075      GOBACK.                                                      ELXHPPAI
00076                                                                   ELXHPPAI
00077  0000-EXIT.                                                       ELXHPPAI
00078      EXIT.                                                        ELXHPPAI
00079                                                                   ELXHPPAI
00080 ****************************************************************  ELXHPPAI
00081 *                                                              *  ELXHPPAI
00082 *           P R O C E S S     C O N T R O L                    *  ELXHPPAI
00083 *                                                              *  ELXHPPAI
00084 ****************************************************************  ELXHPPAI
00085  0001-PROCESS-CONTROL.                                            ELXHPPAI
00086                                                                   ELXHPPAI
00087      MOVE HPPA-COMMON-DATA TO PMCI-COMMON-DATA.                   ELXHPPAI
00088 *    MOVE 0000 TO PMCI-BLUE-CHIP-RETURN-CODE.                     ELXHPPAI
00089 *    MOVE 0000 TO PMCI-BLUE-CHIP-ERROR-CODE.                      ELXHPPAI
00090 *    MOVE HPPA-PROVIDER-NUMBER TO PMCI-PROVIDER-NUMBER.           ELXHPPAI
00091 *    MOVE HPPA-PROVIDER-TYPE   TO PMCI-PROVIDER-TYPE.             ELXHPPAI
00092 *    MOVE HPPA-PPO-INDICATOR   TO PMCI-PPO-INDICATOR.             ELXHPPAI
00093 *    MOVE HPPA-PROVIDER-INDICATOR  TO HPPA-PROVIDER-INDICATOR.    ELXHPPAI
00094 *    MOVE HPPA-PLAN-INDICATOR      TO HPPA-PLAN-INDICATOR.        ELXHPPAI
00095 *    MOVE '000'                  TO PMCI-PLAN-CODE.               ELXHPPAI
00096 *    MOVE HPPA-GROUP-NBR         TO PMCI-GROUP-NBR.               ELXHPPAI
00097 *    MOVE HPPA-SECT-NUM          TO PMCI-SECT-NUM.                ELXHPPAI
00098 *    MOVE '000'                  TO PMCI-PACKAGE-CODE.            ELXHPPAI
00099 *    MOVE HPPA-SUBSCRIBER-NBR    TO PMCI-SUBSCRIBER-NBR.          ELXHPPAI
00100 *    MOVE HPPA-ADS-PROG-TYPE     TO PMCI-ADS-PROG-TYPE.           ELXHPPAI
00101 *    MOVE HPPA-PAT-LAST-NAME     TO PMCI-PAT-LAST-NAME.           ELXHPPAI
00102 *    MOVE HPPA-PAT-FIRST-NAME    TO PMCI-PAT-FIRST-NAME.          ELXHPPAI
00103 *    MOVE HPPA-PAT-SEX           TO PMCI-PAT-SEX.                 ELXHPPAI
00104 *    MOVE HPPA-PAT-BIRTH-DATE    TO PMCI-PAT-BIRTH-DATE.          ELXHPPAI
00105 *    MOVE HPPA-PAT-AGE           TO PMCI-PAT-AGE.                 ELXHPPAI
00106 *    MOVE HPPA-PAT-RELATIONSHIP  TO PMCI-PAT-RELATIONSHIP.        ELXHPPAI
00107 *    MOVE HPPA-MEDICARE-ELIGIBILITY TO PMCI-MEDICARE-ELIGIBILITY. ELXHPPAI
00108 *    MOVE HPPA-DATE-OF-SERVICE   TO PMCI-DATE-OF-SERVICE.         ELXHPPAI
00109 *    MOVE HPPA-PROVIDER-INDICATOR TO  PMCI-PROVIDER-INDICATOR.    ELXHPPAI
00110 *    MOVE HPPA-IP-OR-OP-INQUIRY   TO  PMCI-IP-OR-OP-INQUIRY.      ELXHPPAI
00111 *    MOVE HPPA-BEN-POINTERS       TO  PMCI-BEN-POINTERS.          ELXHPPAI
00112                                                                   ELXHPPAI
00113      EXEC CICS  LINK  PROGRAM ('ELXPMCIF')                        ELXHPPAI
00114                       COMMAREA(PMCI-COMM-AREA)                    ELXHPPAI
00115                       LENGTH  (LENGTH OF PMCI-COMM-AREA)          ELXHPPAI
00116                       END-EXEC.                                   ELXHPPAI
00117  0001-EXIT.                                                       ELXHPPAI
00118      EXIT.                                                        ELXHPPAI
00119                                                                   ELXHPPAI
00120  1000-PROCESS-BENEFITS.                                           ELXHPPAI
00121                                                                   ELXHPPAI
00122 *    PERFORM 2000-INITIALIZE-PMCI-VALUES THRU 2000-EXIT.          ELXHPPAI
00123      MOVE PMCI-CONTRACT-INFO TO HPPA-CONTRACT-INFO.               ELXHPPAI
00124                                                                   ELXHPPAI
00125      IF PMCI-BLUE-CHIP-RETURN-CODE = 0                            ELXHPPAI
00126      AND PMCI-BLUE-CHIP-ERROR-CODE = 0                            ELXHPPAI
00127         CONTINUE                                                  ELXHPPAI
00128      ELSE                                                         ELXHPPAI
00129        MOVE PMCI-BLUE-CHIP-RETURN-CODE TO                         ELXHPPAI
00130             HPPA-BLUE-CHIP-RETURN-CODE                            ELXHPPAI
00131        MOVE PMCI-BLUE-CHIP-ERROR-CODE TO                          ELXHPPAI
00132             HPPA-BLUE-CHIP-ERROR-CODE                             ELXHPPAI
00133      END-IF.                                                      ELXHPPAI
00134  1000-EXIT.                                                       ELXHPPAI
00135      EXIT.                                                        ELXHPPAI
00136                                                                   ELXHPPAI
