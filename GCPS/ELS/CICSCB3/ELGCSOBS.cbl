00001 *      LAST MAINTENANCE TIME: 14.38.16  DATE: 08/21/89            09/03/03
00002 * STRUCTURE(S) MEMBER ELGCSOBSPL - LEVEL 018 AS OF 10/14/88       ELGCSOBS
00003 * FROM PANLIB R360059.STRUCTPL.PANLIB                                LV002
00004 *                                                                 ELGCSOBS
00005  IDENTIFICATION DIVISION.                                         ELGCSOBS
00006                                                                   ELGCSOBS
00007  PROGRAM-ID.         ELGCSOBS.                                    ELGCSOBS
00008                                                                   ELGCSOBS
00009  AUTHOR.             ANNE KEFFER KING.                            ELGCSOBS
00010                                                                   ELGCSOBS
00011  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGCSOBS
00012                      A MUTUAL LEGAL RESERVE COMPANY               ELGCSOBS
00013                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGCSOBS
00014                      233 N. MICHIGAN AVE                          ELGCSOBS
00015                      CHICAGO, ILLINOIS 60601                      ELGCSOBS
00016                                                                   ELGCSOBS
00017  DATE-WRITTEN.       16-FEB-1988.                                 ELGCSOBS
00018                                                                   ELGCSOBS
00019  DATE-COMPILED.                                                   ELGCSOBS
00020                                                                   ELGCSOBS
00021  SECURITY.           COPYRIGHT 1986,                              ELGCSOBS
00022                      HEALTH CARE SERVICE CORPORATION              ELGCSOBS
00023      SKIP3                                                        ELGCSOBS
00024  ENVIRONMENT DIVISION.                                            ELGCSOBS
00025                                                                   ELGCSOBS
00026  CONFIGURATION SECTION.                                           ELGCSOBS
00027  SOURCE-COMPUTER.    IBM-3090.                                    ELGCSOBS
00028  OBJECT-COMPUTER.    IBM-3090.                                    ELGCSOBS
00029      EJECT                                                        ELGCSOBS
00030 ******************************************************************ELGCSOBS
00031 *ELGCSOBS -- ELS:                                                *ELGCSOBS
00032 *                                                                *ELGCSOBS
00033 *ELGCSOBS WILL OBTAIN AND  FORMAT THE COVERAGE AND PAYMENT LEVEL *ELGCSOBS
00034 *INFORMATION FOR OB/STRILIZATION SERVICES                        *ELGCSOBS
00035 ******************************************************************ELGCSOBS
00036 *                                                                *ELGCSOBS
00037 *                      MAINTENANCE HISTORY                       *ELGCSOBS
00038 *                                                                *ELGCSOBS
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELGCSOBS
00040 * ----- ----------- --- ----- ---------------------------------- *ELGCSOBS
00041 * 01.00 16-FEB-1988 AKK       CREATED                            *ELGCSOBS
00042 * 01.01 14-OCT-1988 EGL       REMOVED SPECIAL MESSAGE FOR        *ELGCSOBS
00043 *                             ADDITIONAL BENEFITS.               *ELGCSOBS
00044 *                                                                *ELGCSOBS
00045 * 01.02 21-AUG-1989 AKK       DESTRUCTED PROGRAM.                *ELGCSOBS
00046 *                                                                *ELGCSOBS
00047 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGCSOBS
00048 *                                                                *ELGCSOBS
00049 ******************************************************************ELGCSOBS
00050                                                                   ELGCSOBS
00051  DATA DIVISION.                                                   ELGCSOBS
00052  WORKING-STORAGE SECTION.                                         ELGCSOBS
00053  01   HEADING-LINES.                                              ELGCSOBS
00054       03  WS-HEADER-LINE2             PIC X(79)  VALUE            ELGCSOBS
00055       'THE FOLLOWING DISPLAYED BENEFITS ARE FOR:  OBSTETRICAL/STERELGCSOBS
00056 -     'ILIZATION SERVICES'.                                       ELGCSOBS
00057 *                                                                 ELGCSOBS
00058       03  WS-HEADER-LINE3             PIC X(79)   VALUE           ELGCSOBS
00059       '-----------------------------------------------------------ELGCSOBS
00060 -     '--------------------'.                                     ELGCSOBS
00061  LINKAGE SECTION.                                                 ELGCSOBS
00062  01   DFHCOMMAREA.                                                ELGCSOBS
00063 /                                                                 ELGCSOBS
00064       COPY ELSCOMMC.                                              ELGCSOBS
00065 /                                                                 ELGCSOBS
00066       COPY ELSCIA2C.                                              ELGCSOBS
00067 /                                                                 ELGCSOBS
00068       COPY ELSCSPTC.                                              ELGCSOBS
00069 /                                                                 ELGCSOBS
00070       COPY ELSOUTPC.                                              ELGCSOBS
00071      EJECT                                                        ELGCSOBS
00072  PROCEDURE DIVISION.                                              ELGCSOBS
00073 ************************************************************      ELGCSOBS
00074 *                                                          *      ELGCSOBS
00075 *                    PROCEDURE DIVISION                    *      ELGCSOBS
00076 *                                                          *      ELGCSOBS
00077 ************************************************************      ELGCSOBS
00078                                                                   ELGCSOBS
00079                                                                   ELGCSOBS
00080 ************************************************************      ELGCSOBS
00081 *                                                          *      ELGCSOBS
00082 *        OBSTERTRICS STERILIZATION                         *      ELGCSOBS
00083 *                                                          *      ELGCSOBS
00084 ************************************************************      ELGCSOBS
00085  OBSTERTRICS-STERILIZATION.                                       ELGCSOBS
00086      PERFORM INITIALIZATION.                                      ELGCSOBS
00087      PERFORM PROCESS-OBSTETRICS-STERILIZATI.                      ELGCSOBS
00088      GOBACK.                                                      ELGCSOBS
00089                                                                   ELGCSOBS
00090 ************************************************************      ELGCSOBS
00091 *                                                          *      ELGCSOBS
00092 *        INITIALIZATION                                    *      ELGCSOBS
00093 *                                                          *      ELGCSOBS
00094 ************************************************************      ELGCSOBS
00095  INITIALIZATION.                                                  ELGCSOBS
00096      PERFORM ESTABLISH-ADDRESS-OF-CO.                             ELGCSOBS
00097      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELGCSOBS
00098      PERFORM ESTABLISH-ADDRESSABILITY-OF-PO.                      ELGCSOBS
00099                                                                   ELGCSOBS
00100 ************************************************************      ELGCSOBS
00101 *                                                          *      ELGCSOBS
00102 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGCSOBS
00103 *                                                          *      ELGCSOBS
00104 ************************************************************      ELGCSOBS
00105  ESTABLISH-ADDRESS-OF-CO.                                         ELGCSOBS
00106      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELGCSOBS
00107      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELGCSOBS
00108                                                                   ELGCSOBS
00109 ************************************************************      ELGCSOBS
00110 *                                                          *      ELGCSOBS
00111 *        CHECK FOR VALID COMMAREA                          *      ELGCSOBS
00112 *                                                          *      ELGCSOBS
00113 ************************************************************      ELGCSOBS
00114  CHECK-FOR-VALID-COMMAREA.                                        ELGCSOBS
00115      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGCSOBS
00116         EXEC CICS ABEND                                           ELGCSOBS
00117                  ABCODE('EL01')                                   ELGCSOBS
00118           END-EXEC.                                               ELGCSOBS
00119                                                                   ELGCSOBS
00120 ************************************************************      ELGCSOBS
00121 *                                                          *      ELGCSOBS
00122 *        ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA *      ELGCSOBS
00123 *                                                          *      ELGCSOBS
00124 ************************************************************      ELGCSOBS
00125  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELGCSOBS
00126      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGCSOBS
00127                 ADDRESS OF                                        ELGCSOBS
00128          CIA-ELS-COMMON-INTERFACE-AREA.                           ELGCSOBS
00129      IF CIA-RC-PTR-NULL                                           ELGCSOBS
00130         EXEC CICS ABEND                                           ELGCSOBS
00131                   ABCODE('EL02')                                  ELGCSOBS
00132            END-EXEC.                                              ELGCSOBS
00133                                                                   ELGCSOBS
00134                                                                   ELGCSOBS
00135 ************************************************************      ELGCSOBS
00136 *                                                          *      ELGCSOBS
00137 *        SIGNAL UNALLOC AREA ERROR                         *      ELGCSOBS
00138 *                                                          *      ELGCSOBS
00139 ************************************************************      ELGCSOBS
00140  SIGNAL-UNALLOC-AREA-ERROR.                                       ELGCSOBS
00141      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGCSOBS
00142      PERFORM SIGNAL-ABEND.                                        ELGCSOBS
00143                                                                   ELGCSOBS
00144                                                                   ELGCSOBS
00145 ************************************************************      ELGCSOBS
00146 *                                                          *      ELGCSOBS
00147 *        SIGNAL ABEND                                      *      ELGCSOBS
00148 *                                                          *      ELGCSOBS
00149 ************************************************************      ELGCSOBS
00150  SIGNAL-ABEND.                                                    ELGCSOBS
00151      EXEC CICS ABEND                                              ELGCSOBS
00152                ABCODE(CIA-ABCODE)                                 ELGCSOBS
00153         END-EXEC.                                                 ELGCSOBS
00154      EJECT                                                        ELGCSOBS
00155                                                                   ELGCSOBS
00156                                                                   ELGCSOBS
00157 ************************************************************      ELGCSOBS
00158 *                                                          *      ELGCSOBS
00159 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELGCSOBS
00160 *                                                          *      ELGCSOBS
00161 ************************************************************      ELGCSOBS
00162  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELGCSOBS
00163      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGCSOBS
00164      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSOBS
00165                 ADDRESS OF COF-OUTPUT-INTERFACE.                  ELGCSOBS
00166      IF CIA-RC-PTR-NULL                                           ELGCSOBS
00167          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSOBS
00168      EJECT                                                        ELGCSOBS
00169                                                                   ELGCSOBS
00170                                                                   ELGCSOBS
00171 ************************************************************      ELGCSOBS
00172 *                                                          *      ELGCSOBS
00173 *        ESTABLISH ADDRESSABILITY OF POINTER LIST TABLE    *      ELGCSOBS
00174 *                                                          *      ELGCSOBS
00175 ************************************************************      ELGCSOBS
00176  ESTABLISH-ADDRESSABILITY-OF-PO.                                  ELGCSOBS
00177      SET CIA-ELSCSPTC-DDN TO TRUE.                                ELGCSOBS
00178      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSOBS
00179                 ADDRESS OF CSPT-POINTER-LIST.                     ELGCSOBS
00180      IF CIA-RC-PTR-NULL                                           ELGCSOBS
00181          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSOBS
00182      EJECT                                                        ELGCSOBS
00183                                                                   ELGCSOBS
00184                                                                   ELGCSOBS
00185 ************************************************************      ELGCSOBS
00186 *                                                          *      ELGCSOBS
00187 *        PROCESS OBSTETRICS STERILIZATION                  *      ELGCSOBS
00188 *                                                          *      ELGCSOBS
00189 ************************************************************      ELGCSOBS
00190  PROCESS-OBSTETRICS-STERILIZATI.                                  ELGCSOBS
00191      PERFORM CONSTRUCT-HEADER-LINE.                               ELGCSOBS
00192      SET PROCESS-OBS TO TRUE.                                     ELGCSOBS
00193      PERFORM LINK-TO-ELUCSENT.                                    ELGCSOBS
00194      PERFORM LINK-TO-ELUCSOUT.                                    ELGCSOBS
00195      SET COMPLETED-OBS TO TRUE.                                   ELGCSOBS
00196                                                                   ELGCSOBS
00197                                                                   ELGCSOBS
00198 ************************************************************      ELGCSOBS
00199 *                                                          *      ELGCSOBS
00200 *        CONSTRUCT HEADER LINE                             *      ELGCSOBS
00201 *                                                          *      ELGCSOBS
00202 ************************************************************      ELGCSOBS
00203  CONSTRUCT-HEADER-LINE.                                           ELGCSOBS
00204      SET COF-NEW-PAGE TO TRUE.                                    ELGCSOBS
00205      MOVE 3 TO COF-NBR-HDR-LINES.                                 ELGCSOBS
00206      MOVE 0 TO COF-NBR-DTL-LINES.                                 ELGCSOBS
00207      MOVE WS-HEADER-LINE2 TO COF-HDR-LINE (2).                    ELGCSOBS
00208      MOVE WS-HEADER-LINE3 TO COF-HDR-LINE (3).                    ELGCSOBS
00209      PERFORM LINK-TO-ELUOUTPT.                                    ELGCSOBS
00210                                                                   ELGCSOBS
00211                                                                   ELGCSOBS
00212 ************************************************************      ELGCSOBS
00213 *                                                          *      ELGCSOBS
00214 *        LINK TO ELUOUTPT                                  *      ELGCSOBS
00215 *                                                          *      ELGCSOBS
00216 ************************************************************      ELGCSOBS
00217  LINK-TO-ELUOUTPT.                                                ELGCSOBS
00218      EXEC CICS LINK                                               ELGCSOBS
00219                PROGRAM ('ELUOUTPT')                               ELGCSOBS
00220                COMMAREA (DFHCOMMAREA)                             ELGCSOBS
00221                END-EXEC.                                          ELGCSOBS
00222      EJECT                                                        ELGCSOBS
00223                                                                   ELGCSOBS
00224                                                                   ELGCSOBS
00225 ************************************************************      ELGCSOBS
00226 *                                                          *      ELGCSOBS
00227 *        LINK TO ELUCSENT                                  *      ELGCSOBS
00228 *                                                          *      ELGCSOBS
00229 ************************************************************      ELGCSOBS
00230  LINK-TO-ELUCSENT.                                                ELGCSOBS
00231      EXEC CICS LINK                                               ELGCSOBS
00232                PROGRAM ('ELUCSENT')                               ELGCSOBS
00233                COMMAREA (DFHCOMMAREA)                             ELGCSOBS
00234                END-EXEC.                                          ELGCSOBS
00235                                                                   ELGCSOBS
00236 ************************************************************      ELGCSOBS
00237 *                                                          *      ELGCSOBS
00238 *        LINK TO ELUCSOUT                                  *      ELGCSOBS
00239 *                                                          *      ELGCSOBS
00240 ************************************************************      ELGCSOBS
00241  LINK-TO-ELUCSOUT.                                                ELGCSOBS
00242      EXEC CICS LINK                                               ELGCSOBS
00243                PROGRAM ('ELUCSOUT')                               ELGCSOBS
00244                COMMAREA (DFHCOMMAREA)                             ELGCSOBS
00245                END-EXEC.                                          ELGCSOBS
