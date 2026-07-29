00001  IDENTIFICATION DIVISION.                                         09/03/03
00002 *                                                                 ELGIDGD 
00003  PROGRAM-ID.         ELGIDGD.                                        LV002
00004 *                                                                 ELGIDGD 
00005  AUTHOR.             JOHN BEIRNE                                  ELGIDGD 
00006 *                                                                 ELGIDGD 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGIDGD 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELGIDGD 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGIDGD 
00010                      233 N. MICHIGAN AVE                          ELGIDGD 
00011                      CHICAGO, ILLINOIS 60601                      ELGIDGD 
00012 *                                                                 ELGIDGD 
00013  DATE-WRITTEN.       19-DEC-1990.                                 ELGIDGD 
00014 *                                                                 ELGIDGD 
00015  DATE-COMPILED.                                                   ELGIDGD 
00016 *                                                                 ELGIDGD 
00017  SECURITY.           COPYRIGHT 1990,                              ELGIDGD 
00018                      HEALTH CARE SERVICE CORPORATION              ELGIDGD 
00019 *                                                                 ELGIDGD 
00020 *                                                                 ELGIDGD 
00021 ******************************************************************ELGIDGD 
00022 *   ELGIDGD                                                      *ELGIDGD 
00023 *                                                                *ELGIDGD 
00024 *                        PROGRAM ABSTRACT                        *ELGIDGD 
00025 *                                                                *ELGIDGD 
00026 *   PROGRAM NAME:   E.L.S. INTERNAL TABULAR TRANSLATOR           *ELGIDGD 
00027 *                   SUBROUTINE                                   *ELGIDGD 
00028 *                                                                *ELGIDGD 
00029 *   PROGRAM I.D.:   ELGIDGD                                      *ELGIDGD 
00030 *                                                                *ELGIDGD 
00031 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE FIELDS *ELGIDGD 
00032 *              IN THE #IDGD INTERNAL TABULAR (DIAGNOSIS TABULAR) *ELGIDGD 
00033 *                                                                *ELGIDGD 
00034 *   RECORDS                                                      *ELGIDGD 
00035 *   ACCESSED:  #IDGD INTERNAL TABULAR                            *ELGIDGD 
00036 *                                                                *ELGIDGD 
00037 ******************************************************************ELGIDGD 
00038 *                                                                *ELGIDGD 
00039 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGIDGD 
00040 *       *-*         U P D A T E   H I S T O R Y         *-*      *ELGIDGD 
00041 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELGIDGD 
00042 *                                                                *ELGIDGD 
00043 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELGIDGD 
00044 *                                                                *ELGIDGD 
00045 *  01.00     12/19/90  JPB  CLONED FROM ELGIPGN.                 *ELGIDGD 
00046 *                                                                *ELGIDGD 
00047 *  02.00     01/20/92  GEM  ACCUM DISPLAY FIX ISSR# 12010.       *ELGIDGD 
00048 *                                                                *ELGIDGD 
00049 *  03.00     10/30/98  AKK  ADDED SUPPORT FOR ACP TABULAR.       *ELGIDGD 
00050 *                                                                *ELGIDGD 
00051 *  04.00     05/07/03  AKK  CHANGES DUE TO DIAG/PROC EXPANSION   *ELGIDGD 
00052 *                                                                *ELGIDGD 
00053 *  05.00     05/22/03  AKK  ADDED WORKING STORAGE FIELDS         *ELGIDGD 
00054 *                           TO MOVE 6 CHARS TO PRCDR              ELGIDGD 
00055 *  05.01     07/30/03  AKK  CHANGED WS-DIAG-CODE-1 TO 4 CHAR     *ELGIDGD 
00056 *                           FROM 1 AS DIAG IS 10 NOT 7!           ELGIDGD 
ED0624*                                                                *        
ED0624* BBDA-58217 06/14/24  ED   RECOMPILE FOR PEAQ COPYBOOK          *        
ED0624*                           EXPANSION:                           *        
ED0624*                                 COPYBOOK ELSACUMC              *        
ED0624*                                                                *        
00057 ******************************************************************ELGIDGD 
00058 /                                                                 ELGIDGD 
00059  ENVIRONMENT DIVISION.                                            ELGIDGD 
00060  CONFIGURATION SECTION.                                           ELGIDGD 
00061  SOURCE-COMPUTER.    IBM-3033.                                    ELGIDGD 
00062  OBJECT-COMPUTER.    IBM-3033.                                    ELGIDGD 
00063 /                                                                 ELGIDGD 
00064  DATA DIVISION.                                                   ELGIDGD 
00065  WORKING-STORAGE SECTION.                                         ELGIDGD 
00066  01  WS-BEGIN                   PIC X(24)  VALUE                  ELGIDGD 
00067      'ELGIDGD WORKING STORAGE*'.                                  ELGIDGD 
00068 /     W O R K F I E L D S   A N D   S W I T C H E S               ELGIDGD 
00069  01  WS-MISC-WORK.                                                ELGIDGD 
00070      05  WS-DIAGNOSIS-CODE.                                       ELGIDGD 
00071          10 WS-DIAG-CODE-6      PIC X(6).                         ELGIDGD 
00072          10 WS-DIAG-CODE-1      PIC X(4).                         ELGIDGD 
00073                                                                   ELGIDGD 
00074      05  WS-ACM-SUB             PIC S9(4)  COMP SYNC VALUE ZEROES.ELGIDGD 
00075      05  WS-CUR-SUB             PIC S9(4)  COMP SYNC VALUE ZEROES.ELGIDGD 
00076      05  WS-NXT-SUB             PIC S9(4)  COMP SYNC VALUE ZEROES.ELGIDGD 
00077      05  WS-TAG-SUB             PIC S9(4)  COMP SYNC VALUE ZEROES.ELGIDGD 
00078      05  WS-LVL-SUB             PIC S9(4)  COMP SYNC VALUE ZEROES.ELGIDGD 
00079      05  WS-IDGD                PIC X(5)   VALUE '#IDGD'.         ELGIDGD 
00080      05  WS-BEN-ID.                                               ELGIDGD 
00081          10  WS-BEN-ID-5        PIC X(5)   VALUE SPACES.          ELGIDGD 
00082          10  WS-BEN-ID-1        PIC X      VALUE SPACES.          ELGIDGD 
00083          10  FILLER             PIC X(4)   VALUE SPACES.          ELGIDGD 
00084      05  WS-GETMAIN-SWITCH      PIC X      VALUE 'N'.             ELGIDGD 
00085          88  GETMAIN-DONE                  VALUE 'Y'.             ELGIDGD 
00086      05  WS-EDIT-COMP           PIC X      VALUE 'N'.             ELGIDGD 
00087          88  WS-EDIT-COMPLETE              VALUE 'Y'.             ELGIDGD 
00088                                                                   ELGIDGD 
00089  01  FILLER.                                                      ELGIDGD 
00090    02  WS-ACCUMULATOR-TYPE.                                       ELGIDGD 
00091      03  FILLER   PIC X(21)  VALUE                                ELGIDGD 
00092         '#ACL    COINSURANCE  '.                                  ELGIDGD 
00093      03  FILLER   PIC X(21)  VALUE                                ELGIDGD 
00094         '#ADL    DEDUCTIBLE   '.                                  ELGIDGD 
00095      03  FILLER   PIC X(21)  VALUE                                ELGIDGD 
00096         '#ABM    MAXIMUM      '.                                  ELGIDGD 
00097      03  FILLER   PIC X(21)  VALUE                                ELGIDGD 
00098         '#AOL    OUT-OF-POCKET'.                                  ELGIDGD 
00099      03  FILLER   PIC X(21)  VALUE                                ELGIDGD 
00100         '#ACP    CO-PAY       '.                                  ELGIDGD 
00101    02  WS-ACCUMULATOR-TYPE-TABLE REDEFINES  WS-ACCUMULATOR-TYPE.  ELGIDGD 
00102      03  WS-ACCUMULATOR-TYPE-TBL  OCCURS 5 TIMES                  ELGIDGD 
00103                                   INDEXED BY ACUM-IDX.            ELGIDGD 
00104          05  WS-ACCUM-TYPE       PIC X(08).                       ELGIDGD 
00105          05  WS-ACCUM-NAME       PIC X(13).                       ELGIDGD 
00106                                                                   ELGIDGD 
00107  01  WS-IDGD-VARIABLE-AREA.                                       ELGIDGD 
00108    02  WS-IDGD-COUNT     COMP-3       PIC S9(03) VALUE ZEROES.    ELGIDGD 
00109    02  WS-IDGD-VARIABLES                                          ELGIDGD 
00110           OCCURS 46 TIMES DEPENDING  ON  WS-IDGD-COUNT.           ELGIDGD 
00111        05  WS-IDGD-SLOT-NBR           PIC S9(07) COMP-3.          ELGIDGD 
00112        05  WS-IDGD-LEVEL              PIC  X(04).                 ELGIDGD 
00113        05  WS-IDGD-PERCENT            PIC S9(03) COMP-3.          ELGIDGD 
00114        05  WS-IDGD-STATUS             PIC X(01).                  ELGIDGD 
00115          88  WS-IDGD-PROCESSED            VALUE 'Y'.              ELGIDGD 
00116          88  WS-IDGD-NOT-PROCESSED        VALUE 'N'.              ELGIDGD 
00117                                                                   ELGIDGD 
00118  01  WS-LEVEL-AREA.                                               ELGIDGD 
00119    02  WS-LEVEL-COUNT    COMP-3       PIC S9(03) VALUE ZEROES.    ELGIDGD 
00120    02  WS-LEVEL-TABLE.                                            ELGIDGD 
00121      03  WS-LEVEL-TABLE-ENTRIES                                   ELGIDGD 
00122            OCCURS 46 TIMES DEPENDING ON WS-LEVEL-COUNT.           ELGIDGD 
00123        05  WS-LEVEL-TAG            PIC X(04).                     ELGIDGD 
00124        05  WS-LEVEL-PERCENT        PIC ZZ9.                       ELGIDGD 
00125        05  WS-LEVEL-EDITOR         PIC X(06).                     ELGIDGD 
00126          88  WS-LEVEL-PCT           VALUE '%     '.               ELGIDGD 
00127          88  WS-LEVEL-PCT-COMMA     VALUE '%,    '.               ELGIDGD 
00128          88  WS-LEVEL-PCT-AND       VALUE '% AND '.               ELGIDGD 
00129                                                                   ELGIDGD 
00130  01  WS-SERVICES-PAID-PHRASE.                                     ELGIDGD 
00131      05  FILLER                  PIC X(21)  VALUE                 ELGIDGD 
00132          'FOR SERVICES PAID AT'.                                  ELGIDGD 
00133                                                                   ELGIDGD 
00134  01  WS-FLAG-LINE.                                                ELGIDGD 
00135      05  FILLER                  PIC X(79)  VALUE '*****'.        ELGIDGD 
00136 /                                                                 ELGIDGD 
00137      COPY ELSVLTGC.                                               ELGIDGD 
00138 /                                                                 ELGIDGD 
00139  01  WS-END                      PIC X(24)  VALUE                 ELGIDGD 
00140      '*** ELGIDGD W/S ENDS ***'.                                  ELGIDGD 
00141 /             L I N K A G E   S E C T I O N                       ELGIDGD 
00142  LINKAGE SECTION.                                                 ELGIDGD 
00143  01  DFHCOMMAREA.                                                 ELGIDGD 
00144      COPY ELSCOMMC.                                               ELGIDGD 
00145 /             C I A                                               ELGIDGD 
00146      COPY ELSCIA2C.                                               ELGIDGD 
00147 /                                                                 ELGIDGD 
00148      COPY ELSCMDSC.                                               ELGIDGD 
00149 /                                                                 ELGIDGD 
00150      COPY ELSCMIFC.                                               ELGIDGD 
00151 /                                                                 ELGIDGD 
00152      COPY ELSIOPMC.                                               ELGIDGD 
00153 /                                                                 ELGIDGD 
00154      COPY ELSKEYSC.                                               ELGIDGD 
00155 /                                                                 ELGIDGD 
00156      COPY ELSOUTPC.                                               ELGIDGD 
00157 /                                                                 ELGIDGD 
00158      COPY ELSTCWAC.                                               ELGIDGD 
00159 /                                                                 ELGIDGD 
00160      COPY ELSSRTPC.                                               ELGIDGD 
00161 /                                                                 ELGIDGD 
00162      COPY ELSACUMC.                                               ELGIDGD 
00163 /                                                                 ELGIDGD 
00164  01  GX9-TABULAR-REC-AREA.                                        ELGIDGD 
00165      COPY GCTIDGDC.                                               ELGIDGD 
00166 /    D I A G N O S I S   T O   N A M E   D B   P A R M S          ELGIDGD 
00167  01  PDB-IO-AREA.                                                 ELGIDGD 
00168      COPY DBPIOPMC.                                               ELGIDGD 
00169 /    P R O V I D E R   M A S T E R   R E C O R D   D E S C R .    ELGIDGD 
00170  01  DIAGNOSIS-MSTR-REC.                                          ELGIDGD 
00171      COPY CR8200.                                                 ELGIDGD 
00172 /    D I A G N O S I S   M A S T E R   R E C O R D   D E S C R .  ELGIDGD 
00173 /                                                                 ELGIDGD 
00174  PROCEDURE DIVISION.                                              ELGIDGD 
00175 ***************************************************************   ELGIDGD 
00176 *                                                             *   ELGIDGD 
00177 *  I N T E R N A L   T A B U L A R   D I S P L A Y   I D G D  *   ELGIDGD 
00178 *                                                             *   ELGIDGD 
00179 ***************************************************************   ELGIDGD 
00180  INTERNAL-TABULAR-DISPLAY-IDGD.                                   ELGIDGD 
00181                                                                   ELGIDGD 
00182      PERFORM INITIALIZATION-ROUTINE.                              ELGIDGD 
00183                                                                   ELGIDGD 
00184      IF ACCUM-ASCEND-DESCEND-COUNT = 1 AND                        ELGIDGD 
00185        (ACCUM-ABM OR  ACCUM-ACL OR ACCUM-ADL OR ACCUM-AOL         ELGIDGD 
00186         OR ACCUM-ACP)                                             ELGIDGD 
00187           PERFORM SINGLE-LEVEL-DISPLAY                            ELGIDGD 
00188      ELSE                                                         ELGIDGD 
00189         IF ACCUM-ASCEND-DESCEND-COUNT > 1 AND                     ELGIDGD 
00190           (ACCUM-ACL OR ACCUM-AOL)                                ELGIDGD 
00191              PERFORM VARIABLE-LEVEL-DISPLAY                       ELGIDGD 
00192         ELSE                                                      ELGIDGD 
00193            EXEC CICS ABEND  ABCODE('EL99')  END-EXEC.             ELGIDGD 
00194                                                                   ELGIDGD 
00195      GOBACK.                                                      ELGIDGD 
00196                                                                   ELGIDGD 
00197 ************************************************************      ELGIDGD 
00198 *                                                          *      ELGIDGD 
00199 *  I N I T I A L I Z A T I O N   R O U T I N E             *      ELGIDGD 
00200 *                                                          *      ELGIDGD 
00201 ************************************************************      ELGIDGD 
00202  INITIALIZATION-ROUTINE.                                          ELGIDGD 
00203      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELGIDGD 
00204          EXEC CICS ABEND  ABCODE('EL01')  END-EXEC.               ELGIDGD 
00205                                                                   ELGIDGD 
00206      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGIDGD 
00207          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGIDGD 
00208      IF CIA-RC-PTR-NULL                                           ELGIDGD 
00209          EXEC CICS ABEND  ABCODE('EL02')  END-EXEC.               ELGIDGD 
00210                                                                   ELGIDGD 
00211      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGIDGD 
00212      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIDGD 
00213          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGIDGD 
00214      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIDGD 
00215                                                                   ELGIDGD 
00216      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGIDGD 
00217      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIDGD 
00218          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELGIDGD 
00219      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIDGD 
00220                                                                   ELGIDGD 
00221      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGIDGD 
00222      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIDGD 
00223          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELGIDGD 
00224      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIDGD 
00225                                                                   ELGIDGD 
00226      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGIDGD 
00227      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIDGD 
00228          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELGIDGD 
00229      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIDGD 
00230                                                                   ELGIDGD 
00231      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGIDGD 
00232      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIDGD 
00233          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELGIDGD 
00234      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIDGD 
00235                                                                   ELGIDGD 
00236      SET  CIA-DBPIOPM-DDN TO TRUE.                                ELGIDGD 
00237      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIDGD 
00238          ADDRESS OF PDB-IO-AREA.                                  ELGIDGD 
00239      IF CIA-RC-PTR-NULL                                           ELGIDGD 
00240          PERFORM GETMAIN-IO-PARM-AREA.                            ELGIDGD 
00241      IF GETMAIN-DONE                                              ELGIDGD 
00242         SET  CIA-DBPIOPM-DDN TO TRUE                              ELGIDGD 
00243         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGIDGD 
00244            ADDRESS OF PDB-IO-AREA.                                ELGIDGD 
00245                                                                   ELGIDGD 
00246      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGIDGD 
00247      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIDGD 
00248          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGIDGD 
00249      PERFORM CHECK-IF-CIA-RC-PTR-NULL.                            ELGIDGD 
00250                                                                   ELGIDGD 
00251      SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR.       ELGIDGD 
00252                                                                   ELGIDGD 
00253      INITIALIZE CMF-CODES-MANUAL-INTERFACE                        ELGIDGD 
00254                 TCAR-FROM-AREA.                                   ELGIDGD 
00255                                                                   ELGIDGD 
00256  CHECK-IF-CIA-RC-PTR-NULL.                                        ELGIDGD 
00257      IF CIA-RC-PTR-NULL                                           ELGIDGD 
00258          SET CIA-AB-UNALLOC-AREA TO TRUE                          ELGIDGD 
00259          EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.           ELGIDGD 
00260                                                                   ELGIDGD 
00261 ************************************************************      ELGIDGD 
00262 *                                                          *      ELGIDGD 
00263 *  S I N G L E   L E V E L   D I S P L A Y                 *      ELGIDGD 
00264 *                                                          *      ELGIDGD 
00265 ************************************************************      ELGIDGD 
00266  SINGLE-LEVEL-DISPLAY.                                            ELGIDGD 
00267      PERFORM READ-INTERNAL-TABULAR-RECORD.                        ELGIDGD 
00268      PERFORM LIST-TABULAR-HEADING.                                ELGIDGD 
00269      PERFORM LIST-TABULAR-CONTENTS                                ELGIDGD 
00270        VARYING GX9-INDEX FROM 1 BY 1                              ELGIDGD 
00271          UNTIL GX9-INDEX = GX9-ENTRY-COUNT OR                     ELGIDGD 
00272          GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX) = HIGH-VALUES.        ELGIDGD 
00273      PERFORM INSERT-BLANK-LINE.                                   ELGIDGD 
00274                                                                   ELGIDGD 
00275 ************************************************************      ELGIDGD 
00276 *                                                          *      ELGIDGD 
00277 *  V A R I A B L E   L E V E L   D I S P L A Y             *      ELGIDGD 
00278 *                                                          *      ELGIDGD 
00279 ************************************************************      ELGIDGD 
00280  VARIABLE-LEVEL-DISPLAY.                                          ELGIDGD 
00281      MOVE 1 TO WS-CUR-SUB.                                        ELGIDGD 
00282      SET VLT-INDEX TO 1.                                          ELGIDGD 
00283      PERFORM EXTRACT-PERCENT-N-SLOT-NBR                           ELGIDGD 
00284        VARYING ASC-DES-INDEX FROM 1 BY 1                          ELGIDGD 
00285          UNTIL ASC-DES-INDEX > ACCUM-ASCEND-DESCEND-COUNT.        ELGIDGD 
00286      MOVE WS-IDGD-COUNT TO WS-CUR-SUB.                            ELGIDGD 
00287      ADD 1 TO WS-CUR-SUB.                                         ELGIDGD 
00288      MOVE HIGH-VALUES TO WS-IDGD-STATUS (WS-CUR-SUB).             ELGIDGD 
00289      PERFORM IDGD-DISPLAY-PROCESSING                              ELGIDGD 
00290        VARYING WS-CUR-SUB FROM 1 BY 1                             ELGIDGD 
00291          UNTIL WS-IDGD-STATUS (WS-CUR-SUB) = HIGH-VALUES.         ELGIDGD 
00292                                                                   ELGIDGD 
00293 ************************************************************      ELGIDGD 
00294 *  R E A D   I N T E R N A L   T A B U L A R   R E C O R D *      ELGIDGD 
00295 ************************************************************      ELGIDGD 
00296  READ-INTERNAL-TABULAR-RECORD.                                    ELGIDGD 
00297      SET CIA-GCTABULR-DDN TO TRUE.                                ELGIDGD 
00298      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIDGD 
00299          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGIDGD 
00300      IF CIA-RC-PTR-NULL                                           ELGIDGD 
00301         SET CIA-STG-GETMAIN TO TRUE                               ELGIDGD 
00302         EXEC CICS LINK  PROGRAM('ELUSTGMG')                       ELGIDGD 
00303                         COMMAREA(DFHCOMMAREA)  END-EXEC           ELGIDGD 
00304         SET CIA-GCTABULR-DDN TO TRUE                              ELGIDGD 
00305         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELGIDGD 
00306                         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.   ELGIDGD 
00307      MOVE '#IDGD ' TO KWA-PROVISION-ID.                           ELGIDGD 
00308      MOVE ACCUM-IDGD-SLOT-NBR (ASC-DES-INDEX)                     ELGIDGD 
00309                                     TO KWA-PROVISION-SLOT-NO.     ELGIDGD 
00310      SET IOP-RD TO TRUE.                                          ELGIDGD 
00311      SET IOP-FCQ-NONE TO TRUE.                                    ELGIDGD 
00312      SET IOP-KVQ-EQ TO TRUE.                                      ELGIDGD 
00313      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELGIDGD 
00314      MOVE SPACES TO IOP-AIX-DDNAME.                               ELGIDGD 
00315      SET IOP-REC-PTR TO NULL.                                     ELGIDGD 
00316      SET IOP-STG-MODE-MOVE TO TRUE.                               ELGIDGD 
00317      EXEC CICS LINK  PROGRAM('ELUIOPGM')                          ELGIDGD 
00318                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIDGD 
00319      IF IOP-RC-OK                                                 ELGIDGD 
00320         SET ADDRESS OF GX9-TABULAR-REC-AREA TO IOP-REC-PTR        ELGIDGD 
00321      ELSE                                                         ELGIDGD 
00322         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELGIDGD 
00323         EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.            ELGIDGD 
00324                                                                   ELGIDGD 
00325 ****************************************************************  ELGIDGD 
00326 *  E X T R A C T   P E R C E N T   &   S L O T   N B R         *  ELGIDGD 
00327 ****************************************************************  ELGIDGD 
00328  EXTRACT-PERCENT-N-SLOT-NBR.                                      ELGIDGD 
00329      IF ACCUM-IDGD-SLOT-NBR (ASC-DES-INDEX) > ZERO                ELGIDGD 
00330         MOVE ACCUM-PERCENT-LEVEL (ASC-DES-INDEX)                  ELGIDGD 
00331           TO WS-IDGD-PERCENT (WS-CUR-SUB)                         ELGIDGD 
00332         MOVE ACCUM-IDGD-SLOT-NBR (ASC-DES-INDEX)                  ELGIDGD 
00333           TO WS-IDGD-SLOT-NBR (WS-CUR-SUB)                        ELGIDGD 
00334         MOVE VLT-VARIABLE-LEVEL-TAG (VLT-INDEX)                   ELGIDGD 
00335           TO WS-IDGD-LEVEL (WS-CUR-SUB)                           ELGIDGD 
00336         SET WS-IDGD-NOT-PROCESSED (WS-CUR-SUB) TO TRUE            ELGIDGD 
00337         ADD 1 TO WS-IDGD-COUNT.                                   ELGIDGD 
00338      ADD 1 TO WS-CUR-SUB.                                         ELGIDGD 
00339      SET VLT-INDEX UP BY 1.                                       ELGIDGD 
00340                                                                   ELGIDGD 
00341 ****************************************************************  ELGIDGD 
00342 *  I D G D   D I S P L A Y   P R O C E S S I N G               *  ELGIDGD 
00343 ****************************************************************  ELGIDGD 
00344  IDGD-DISPLAY-PROCESSING.                                         ELGIDGD 
00345      IF WS-IDGD-NOT-PROCESSED (WS-CUR-SUB)                        ELGIDGD 
00346         MOVE 1 TO WS-LVL-SUB                                      ELGIDGD 
00347         MOVE ZEROES TO WS-LEVEL-COUNT                             ELGIDGD 
00348         PERFORM PUT-EQUAL-SLOT-NBR-TO-LVL-TBL                     ELGIDGD 
00349           VARYING WS-NXT-SUB FROM WS-CUR-SUB BY 1                 ELGIDGD 
00350             UNTIL WS-IDGD-STATUS (WS-NXT-SUB) = HIGH-VALUES       ELGIDGD 
00351         SET ASC-DES-INDEX TO WS-CUR-SUB                           ELGIDGD 
00352         PERFORM READ-INTERNAL-TABULAR-RECORD                      ELGIDGD 
00353         PERFORM EDIT-CONNECT-PERCENT-LEVELS                       ELGIDGD 
00354           VARYING WS-LVL-SUB FROM 1 BY 1                          ELGIDGD 
00355             UNTIL WS-EDIT-COMPLETE                                ELGIDGD 
00356         PERFORM LIST-TABULAR-HEADING                              ELGIDGD 
00357         PERFORM LIST-TABULAR-CONTENTS                             ELGIDGD 
00358           VARYING GX9-INDEX FROM 1 BY 1                           ELGIDGD 
00359             UNTIL GX9-INDEX = GX9-ENTRY-COUNT OR                  ELGIDGD 
00360             GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX) = HIGH-VALUES      ELGIDGD 
00361         PERFORM INSERT-BLANK-LINE.                                ELGIDGD 
00362                                                                   ELGIDGD 
00363 ****************************************************************  ELGIDGD 
00364 *  P U T   E Q U A L   S L O T   N B R   T O   L V L   T B L   *  ELGIDGD 
00365 ****************************************************************  ELGIDGD 
00366  PUT-EQUAL-SLOT-NBR-TO-LVL-TBL.                                   ELGIDGD 
00367      IF WS-IDGD-NOT-PROCESSED (WS-NXT-SUB) AND                    ELGIDGD 
00368         WS-IDGD-SLOT-NBR (WS-CUR-SUB) =                           ELGIDGD 
00369                           WS-IDGD-SLOT-NBR (WS-NXT-SUB)           ELGIDGD 
00370            MOVE WS-IDGD-PERCENT (WS-NXT-SUB) TO                   ELGIDGD 
00371                              WS-LEVEL-PERCENT (WS-LVL-SUB)        ELGIDGD 
00372            MOVE WS-IDGD-LEVEL   (WS-NXT-SUB) TO                   ELGIDGD 
00373                              WS-LEVEL-TAG (WS-LVL-SUB)            ELGIDGD 
00374            SET WS-IDGD-PROCESSED (WS-NXT-SUB) TO TRUE             ELGIDGD 
00375            ADD 1 TO WS-LVL-SUB                                    ELGIDGD 
00376            ADD 1 TO WS-LEVEL-COUNT.                               ELGIDGD 
00377                                                                   ELGIDGD 
00378 ***************************************************************   ELGIDGD 
00379 *   E D I T   C O N N E C T   P E R C E N T   L E V E L S     *   ELGIDGD 
00380 ***************************************************************   ELGIDGD 
00381  EDIT-CONNECT-PERCENT-LEVELS.                                     ELGIDGD 
00382      IF WS-LVL-SUB = 1 AND                                        ELGIDGD 
00383         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIDGD 
00384         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIDGD 
00385         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIDGD 
00386                                                                   ELGIDGD 
00387      IF WS-LVL-SUB = 2 AND                                        ELGIDGD 
00388         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIDGD 
00389         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIDGD 
00390         SUBTRACT 1 FROM WS-LVL-SUB                                ELGIDGD 
00391         SET WS-LEVEL-PCT-AND (WS-LVL-SUB) TO TRUE                 ELGIDGD 
00392         SET WS-EDIT-COMPLETE     TO TRUE.                         ELGIDGD 
00393                                                                   ELGIDGD 
00394      IF WS-LVL-SUB > 2 AND                                        ELGIDGD 
00395         WS-LVL-SUB = WS-LEVEL-COUNT                               ELGIDGD 
00396         SET WS-LEVEL-PCT (WS-LVL-SUB) TO TRUE                     ELGIDGD 
00397         SUBTRACT 1 FROM WS-LVL-SUB                                ELGIDGD 
00398         SET WS-LEVEL-PCT-AND (WS-LVL-SUB) TO TRUE                 ELGIDGD 
00399         SET WS-EDIT-COMPLETE     TO TRUE                          ELGIDGD 
00400      ELSE                                                         ELGIDGD 
00401      IF WS-LVL-SUB > 2 AND                                        ELGIDGD 
00402         WS-LVL-SUB NOT EQUAL WS-LEVEL-COUNT                       ELGIDGD 
00403            SET WS-LEVEL-PCT-COMMA (WS-LVL-SUB) TO TRUE.           ELGIDGD 
00404                                                                   ELGIDGD 
00405 ****************************************************************  ELGIDGD 
00406 *  L I S T   T A B U L A R   H E A D I N G                     *  ELGIDGD 
00407 ****************************************************************  ELGIDGD 
00408  LIST-TABULAR-HEADING.                                            ELGIDGD 
00409      INITIALIZE TCAR-FROM-AREA.                                   ELGIDGD 
00410      MOVE WS-IDGD TO CMF-RECORD-PREFIX.                           ELGIDGD 
00411      MOVE 'INCLUDE-EXCLUDE-IND'   TO CMF-ELEMENT-SYSTEM-NAME.     ELGIDGD 
00412      MOVE GX9-INCLUDE-EXCLUDE-IND TO CMF-CODE-VALUE.              ELGIDGD 
00413      EXEC CICS LINK  PROGRAM ('ELUCMIF')                          ELGIDGD 
00414                      COMMAREA (DFHCOMMAREA)  END-EXEC.            ELGIDGD 
00415      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGIDGD 
00416      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGIDGD 
00417          ADDRESS OF CMF-DESCR.                                    ELGIDGD 
00418      EVALUATE TRUE                                                ELGIDGD 
00419         WHEN ACCUM-ACL                                            ELGIDGD 
00420              MOVE 1 TO WS-ACM-SUB                                 ELGIDGD 
00421         WHEN ACCUM-ADL                                            ELGIDGD 
00422              MOVE 2 TO WS-ACM-SUB                                 ELGIDGD 
00423         WHEN ACCUM-ABM                                            ELGIDGD 
00424              MOVE 3 TO WS-ACM-SUB                                 ELGIDGD 
00425         WHEN ACCUM-AOL                                            ELGIDGD 
00426              MOVE 4 TO WS-ACM-SUB                                 ELGIDGD 
00427         WHEN ACCUM-AOL                                            ELGIDGD 
00428              MOVE 5 TO WS-ACM-SUB                                 ELGIDGD 
00429      END-EVALUATE.                                                ELGIDGD 
00430      IF (ACCUM-ABM OR ACCUM-ADL OR ACCUM-ACP)                     ELGIDGD 
00431          PERFORM STRING-FIRST-LINE-FOR-MAXIMUMX                   ELGIDGD 
00432      ELSE                                                         ELGIDGD 
00433         IF (ACCUM-ACL OR ACCUM-AOL)                               ELGIDGD 
00434            PERFORM STRING-FIRST-LINE-FOR-COINSURA.                ELGIDGD 
00435      PERFORM DO-TEXT-COMPRESSION.                                 ELGIDGD 
00436      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGIDGD 
00437      MOVE +04 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGIDGD 
00438      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                          ELGIDGD 
00439                  TCAR-OUTPUT-FIELD-2-LEN                          ELGIDGD 
00440                  TCAR-OUTPUT-FIELD-3-LEN                          ELGIDGD 
00441                  TCAR-OUTPUT-FIELD-4-LEN.                         ELGIDGD 
00442      PERFORM DO-TEXT-UNSTRING.                                    ELGIDGD 
00443      PERFORM MOVE-ASTERISKS-TO-FIRST-OUTPUT.                      ELGIDGD 
00444      PERFORM MOVE-COMPRESSED-PHRASE                               ELGIDGD 
00445          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELGIDGD 
00446            UNTIL TCAR-FROM-SUB GREATER THAN                       ELGIDGD 
00447             TCAR-OUTPUT-FIELDS-USED.                              ELGIDGD 
00448      PERFORM CALL-OUTPUT.                                         ELGIDGD 
00449                                                                   ELGIDGD 
00450 ****************************************************************  ELGIDGD 
00451 *  S T R I N G   F I R S T   L I N E   F O R   M A X           *  ELGIDGD 
00452 ****************************************************************  ELGIDGD 
00453  STRING-FIRST-LINE-FOR-MAXIMUMX.                                  ELGIDGD 
00454      STRING 'THIS '                   DELIMITED BY SIZE           ELGIDGD 
00455          WS-ACCUM-NAME (WS-ACM-SUB)   DELIMITED BY ' '            ELGIDGD 
00456             ' ACCUMULATOR '           DELIMITED BY SIZE           ELGIDGD 
00457             CMF-DESCR-LINE(1)         DELIMITED BY '  '           ELGIDGD 
00458          ' THE FOLLOWING DIAGNOSES:'  DELIMITED BY SIZE           ELGIDGD 
00459                                       INTO TCAR-FROM-AREA.        ELGIDGD 
00460                                                                   ELGIDGD 
00461 ****************************************************************  ELGIDGD 
00462 *  S T R I N G   F I R S T   L I N E   F O R   C O I N S       *  ELGIDGD 
00463 ****************************************************************  ELGIDGD 
00464  STRING-FIRST-LINE-FOR-COINSURA.                                  ELGIDGD 
00465      IF ACCUM-ASCEND-DESCEND-COUNT = 1                            ELGIDGD 
00466         MOVE 1 TO WS-LEVEL-COUNT                                  ELGIDGD 
00467         MOVE SPACES TO WS-LEVEL-TAG (1)                           ELGIDGD 
00468         MOVE ACCUM-PERCENT-LEVEL (1) TO WS-LEVEL-PERCENT (1)      ELGIDGD 
00469         SET WS-LEVEL-PCT (1) TO TRUE.                             ELGIDGD 
00470                                                                   ELGIDGD 
00471      STRING 'THE '                    DELIMITED BY SIZE           ELGIDGD 
00472          WS-ACCUM-NAME (WS-ACM-SUB)   DELIMITED BY ' '            ELGIDGD 
00473             ' ACCUMULATOR '           DELIMITED BY SIZE           ELGIDGD 
00474             WS-SERVICES-PAID-PHRASE   DELIMITED BY SIZE           ELGIDGD 
00475             WS-LEVEL-TABLE            DELIMITED BY SIZE           ELGIDGD 
00476             CMF-DESCR-LINE(1)         DELIMITED BY '  '           ELGIDGD 
00477           ' THE FOLLOWING DIAGNOSES:' DELIMITED BY SIZE           ELGIDGD 
00478                                       INTO TCAR-FROM-AREA.        ELGIDGD 
00479                                                                   ELGIDGD 
00480 ****************************************************************  ELGIDGD 
00481 *  L I S T   T A B U L A R   C O N T E N T S                   *  ELGIDGD 
00482 ****************************************************************  ELGIDGD 
00483  LIST-TABULAR-CONTENTS.                                           ELGIDGD 
00484      MOVE GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX)  TO                  ELGIDGD 
00485          WS-DIAGNOSIS-CODE.                                       ELGIDGD 
00486      MOVE WS-DIAG-CODE-6 TO PDB-I-SERVICE-CODE.                   ELGIDGD 
00487      MOVE '9' TO PDB-I-SERVICE-CODE-SYSTEM-ID.                    ELGIDGD 
00488      SET PDB-I-RQN-PROCEDURE TO TRUE.                             ELGIDGD 
00489      MOVE '00' TO PDB-I-VERSION.                                  ELGIDGD 
00490      MOVE 'PRCDR' TO PDB-I-SERVICE-TYPE.                          ELGIDGD 
00491      MOVE 'D' TO PDB-I-ACCESS-MODE.                               ELGIDGD 
00492      SET PDB-I-READ-DIRECT TO TRUE.                               ELGIDGD 
00493      EXEC CICS  LINK  PROGRAM ('DBPIOC')                          ELGIDGD 
00494                       COMMAREA(PDB-IO-AREA)  END-EXEC.            ELGIDGD 
00495      IF  NOT PDB-O-RC-SUCCESSFUL                                  ELGIDGD 
00496          PERFORM DIAGNOSIS-FILE-PROBLEM                           ELGIDGD 
00497      ELSE                                                         ELGIDGD 
00498          SET ADDRESS OF DIAGNOSIS-MSTR-REC TO                     ELGIDGD 
00499              ADDRESS OF PDB-O-RECORD-AREA                         ELGIDGD 
00500          PERFORM STRING-DIAG-AND-NAME.                            ELGIDGD 
00501      PERFORM CALL-OUTPUT.                                         ELGIDGD 
00502                                                                   ELGIDGD 
00503 ****************************************************************  ELGIDGD 
00504 *  S T R I N G   D I A G   A N D   N A M E                     *  ELGIDGD 
00505 ****************************************************************  ELGIDGD 
00506  STRING-DIAG-AND-NAME.                                            ELGIDGD 
00507      MOVE SPACES  TO  COF-DTL-LINE(1).                            ELGIDGD 
00508      MOVE 1  TO  COF-NBR-DTL-LINES.                               ELGIDGD 
00509      STRING GX9-DIAGNOSIS-ARGUMENT(GX9-INDEX),                    ELGIDGD 
00510             '  ',                        DELIMITED BY SIZE,       ELGIDGD 
00511             DP-DESCRIPT,                 DELIMITED BY SIZE,       ELGIDGD 
00512             ' ',                         DELIMITED BY SIZE,       ELGIDGD 
00513             INTO  COF-DTL-LINE(1).                                ELGIDGD 
00514                                                                   ELGIDGD 
00515 ****************************************************************  ELGIDGD 
00516 *  I N S E R T   B L A N K   L I N E                           *  ELGIDGD 
00517 ****************************************************************  ELGIDGD 
00518  INSERT-BLANK-LINE.                                               ELGIDGD 
00519      ADD  1       TO  COF-NBR-DTL-LINES.                          ELGIDGD 
00520      MOVE SPACES  TO  COF-DTL-LINE (COF-NBR-DTL-LINES).           ELGIDGD 
00521      PERFORM CALL-OUTPUT.                                         ELGIDGD 
00522                                                                   ELGIDGD 
00523 ****************************************************************  ELGIDGD 
00524 *  M O V E   C O M P R E S S E D   P H R A S E                 *  ELGIDGD 
00525 ****************************************************************  ELGIDGD 
00526  MOVE-COMPRESSED-PHRASE.                                          ELGIDGD 
00527      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGIDGD 
00528      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB)                           ELGIDGD 
00529         TO COF-DTL-LINE (COF-NBR-DTL-LINES).                      ELGIDGD 
00530                                                                   ELGIDGD 
00531 ****************************************************************  ELGIDGD 
00532 *  M O V E   A S T E R I S K S   T O   O U T P U T             *  ELGIDGD 
00533 ****************************************************************  ELGIDGD 
00534  MOVE-ASTERISKS-TO-FIRST-OUTPUT.                                  ELGIDGD 
00535      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGIDGD 
00536      MOVE WS-FLAG-LINE TO COF-DTL-LINE                            ELGIDGD 
00537          (COF-NBR-DTL-LINES).                                     ELGIDGD 
00538                                                                   ELGIDGD 
00539 ****************************************************************  ELGIDGD 
00540 *  D O   T E X T   C O M P R E S S I O N                       *  ELGIDGD 
00541 ****************************************************************  ELGIDGD 
00542  DO-TEXT-COMPRESSION.                                             ELGIDGD 
00543      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGIDGD 
00544                                                                   ELGIDGD 
00545 ****************************************************************  ELGIDGD 
00546 *  D O   T E X T   U N S T R I N G                             *  ELGIDGD 
00547 ****************************************************************  ELGIDGD 
00548  DO-TEXT-UNSTRING.                                                ELGIDGD 
00549      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGIDGD 
00550                                                                   ELGIDGD 
00551 ****************************************************************  ELGIDGD 
00552 *        CALL OUTPUT                                           *  ELGIDGD 
00553 ****************************************************************  ELGIDGD 
00554  CALL-OUTPUT.                                                     ELGIDGD 
00555      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELGIDGD 
00556                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGIDGD 
00557                                                                   ELGIDGD 
00558 ****************************************************************  ELGIDGD 
00559 *  D I A G N O S I S   F I L E   P R O B L E M                 *  ELGIDGD 
00560 ****************************************************************  ELGIDGD 
00561  DIAGNOSIS-FILE-PROBLEM.                                          ELGIDGD 
00562      ADD  1       TO  COF-NBR-DTL-LINES.                          ELGIDGD 
00563      STRING GX9-DIAGNOSIS-ARGUMENT(GX9-INDEX),                    ELGIDGD 
00564             '  ',                        DELIMITED BY SIZE,       ELGIDGD 
00565             '(DIAGNOSIS IS NOT ON FILE)' DELIMITED BY SIZE,       ELGIDGD 
00566             ' ',                         DELIMITED BY SIZE        ELGIDGD 
00567        INTO COF-DTL-LINE(COF-NBR-DTL-LINES).                      ELGIDGD 
00568                                                                   ELGIDGD 
00569 ****************************************************************  ELGIDGD 
00570 *  G E T M A I N   I O   P A R M   A R E A                     *  ELGIDGD 
00571 ****************************************************************  ELGIDGD 
00572  GETMAIN-IO-PARM-AREA.                                            ELGIDGD 
00573      SET CIA-STG-GETMAIN TO TRUE.                                 ELGIDGD 
00574      EXEC CICS LINK  PROGRAM('ELUSTGMG')                          ELGIDGD 
00575                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELGIDGD 
00576      SET GETMAIN-DONE TO TRUE.                                    ELGIDGD 
00577                                                                   ELGIDGD 
