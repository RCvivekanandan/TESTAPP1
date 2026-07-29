00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGCSOPS
00003  PROGRAM-ID.         ELGCSOPS.                                       LV002
00004                                                                   ELGCSOPS
00005  AUTHOR.             ANNE KEFFER KING.                            ELGCSOPS
00006                                                                   ELGCSOPS
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGCSOPS
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELGCSOPS
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGCSOPS
00010                      233 N. MICHIGAN AVE                          ELGCSOPS
00011                      CHICAGO, ILLINOIS 60601                      ELGCSOPS
00012                                                                   ELGCSOPS
00013  DATE-WRITTEN.       16-FEB-1988.                                 ELGCSOPS
00014                                                                   ELGCSOPS
00015  DATE-COMPILED.                                                   ELGCSOPS
00016                                                                   ELGCSOPS
00017  SECURITY.           COPYRIGHT 1986,                              ELGCSOPS
00018                      HEALTH CARE SERVICE CORPORATION              ELGCSOPS
00019      SKIP3                                                        ELGCSOPS
00020  ENVIRONMENT DIVISION.                                            ELGCSOPS
00021                                                                   ELGCSOPS
00022  CONFIGURATION SECTION.                                           ELGCSOPS
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELGCSOPS
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELGCSOPS
00025 ******************************************************************ELGCSOPS
00026 *ELGCSOPS -- ELS:                                                *ELGCSOPS
00027 *                                                                *ELGCSOPS
00028 *ELGCSOPS WILL BE RESPONSIBLE FOR OBTAINING AND FORMATTING       *ELGCSOPS
00029 *COVERAGE AND PAYMENT LEVEL INFORMATION CONCERNING OUTPATIENT    *ELGCSOPS
00030 *SERVICES.                                                       *ELGCSOPS
00031 ******************************************************************ELGCSOPS
00032 *                                                                *ELGCSOPS
00033 *                      MAINTENANCE HISTORY                       *ELGCSOPS
00034 *                                                                *ELGCSOPS
00035 *  MOD     DATE     BY  DRPT                ACTION               *ELGCSOPS
00036 * ----- ----------- --- ----- ---------------------------------- *ELGCSOPS
00037 * 01.00 16-FEB-1988 AKK       CREATED                            *ELGCSOPS
00038 *                                                                *ELGCSOPS
00039 * 02.00 31-MAR-1989 NAC       DESTRUCT CONVERSION USING STRUCT-  *ELGCSOPS
00040 *                             URES VER: 3.5                      *ELGCSOPS
00041 *                                                                *ELGCSOPS
00042 *                                                                *ELGCSOPS
00043 * 03.00 04-APR-1989 GEM       STORAGE MANAGEMENT ENHANCEMENTS    *ELGCSOPS
00044 *                                                                *ELGCSOPS
00045 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGCSOPS
00046 ******************************************************************ELGCSOPS
00047                                                                   ELGCSOPS
00048  DATA DIVISION.                                                   ELGCSOPS
00049  WORKING-STORAGE SECTION.                                         ELGCSOPS
00050  01   HEADING-LINES.                                              ELGCSOPS
00051       03  WS-HEADER-LINE2             PIC X(79)  VALUE            ELGCSOPS
00052       'THE FOLLOWING DISPLAYED BENEFITS ARE FOR:  OUTPATIENT SERVIELGCSOPS
00053 -     'CES'.                                                      ELGCSOPS
00054 *                                                                 ELGCSOPS
00055       03  WS-HEADER-LINE3             PIC X(79)   VALUE           ELGCSOPS
00056       '-----------------------------------------------------------ELGCSOPS
00057 -     '--------------------'.                                     ELGCSOPS
00058  LINKAGE SECTION.                                                 ELGCSOPS
00059  01   DFHCOMMAREA.                                                ELGCSOPS
00060 /                                                                 ELGCSOPS
00061       COPY ELSCOMMC.                                              ELGCSOPS
00062 /                                                                 ELGCSOPS
00063       COPY ELSCIA2C.                                              ELGCSOPS
00064 /                                                                 ELGCSOPS
00065       COPY ELSCSPTC.                                              ELGCSOPS
00066 /                                                                 ELGCSOPS
00067       COPY ELSOUTPC.                                              ELGCSOPS
00068  PROCEDURE DIVISION.                                              ELGCSOPS
00069 ************************************************************      ELGCSOPS
00070 *                                                          *      ELGCSOPS
00071 *                    PROCEDURE DIVISION                    *      ELGCSOPS
00072 *                                                          *      ELGCSOPS
00073 ************************************************************      ELGCSOPS
00074      PERFORM INITIALIZATION.                                      ELGCSOPS
00075      PERFORM PROCESS-OUTPATIENT-SERVICES.                         ELGCSOPS
00076      GOBACK.                                                      ELGCSOPS
00077                                                                   ELGCSOPS
00078                                                                   ELGCSOPS
00079  INITIALIZATION.                                                  ELGCSOPS
00080      PERFORM ESTABLISH-ADDR-OF-CNTRL-BLKS.                        ELGCSOPS
00081      PERFORM ESTABLISH-ADDR-OF-OUTPUT-INTER.                      ELGCSOPS
00082      PERFORM ESTABLISH-ADDR-OF-POINTER-LIST.                      ELGCSOPS
00083                                                                   ELGCSOPS
00084                                                                   ELGCSOPS
00085 ************************************************************      ELGCSOPS
00086 *                                                          *      ELGCSOPS
00087 *        ESTABLISH ADDR OF CNTRL BLKS                      *      ELGCSOPS
00088 *                                                          *      ELGCSOPS
00089 ************************************************************      ELGCSOPS
00090  ESTABLISH-ADDR-OF-CNTRL-BLKS.                                    ELGCSOPS
00091      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGCSOPS
00092          PERFORM SIGNAL-INVALID-COMMAREA.                         ELGCSOPS
00093                                                                   ELGCSOPS
00094      IF ECA-CIA-PTR = NULL                                        ELGCSOPS
00095          PERFORM SIGNAL-INVALID-CIA                               ELGCSOPS
00096      ELSE                                                         ELGCSOPS
00097          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELGCSOPS
00098                                                                   ELGCSOPS
00099                                                                   ELGCSOPS
00100 ************************************************************      ELGCSOPS
00101 *                                                          *      ELGCSOPS
00102 *        SIGNAL INVALID COMMAREA                           *      ELGCSOPS
00103 *                                                          *      ELGCSOPS
00104 ************************************************************      ELGCSOPS
00105  SIGNAL-INVALID-COMMAREA.                                         ELGCSOPS
00106      EXEC CICS ABEND                                              ELGCSOPS
00107                ABCODE('EL01')                                     ELGCSOPS
00108         END-EXEC.                                                 ELGCSOPS
00109                                                                   ELGCSOPS
00110                                                                   ELGCSOPS
00111 ************************************************************      ELGCSOPS
00112 *                                                          *      ELGCSOPS
00113 *        SIGNAL INVALID CIA                                *      ELGCSOPS
00114 *                                                          *      ELGCSOPS
00115 ************************************************************      ELGCSOPS
00116  SIGNAL-INVALID-CIA.                                              ELGCSOPS
00117      EXEC CICS ABEND                                              ELGCSOPS
00118                ABCODE('EL02')                                     ELGCSOPS
00119         END-EXEC.                                                 ELGCSOPS
00120                                                                   ELGCSOPS
00121                                                                   ELGCSOPS
00122 ************************************************************      ELGCSOPS
00123 *                                                          *      ELGCSOPS
00124 *        SIGNAL UNALLOC AREA ERROR                         *      ELGCSOPS
00125 *                                                          *      ELGCSOPS
00126 ************************************************************      ELGCSOPS
00127  SIGNAL-UNALLOC-AREA-ERROR.                                       ELGCSOPS
00128      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGCSOPS
00129      PERFORM SIGNAL-ABEND.                                        ELGCSOPS
00130                                                                   ELGCSOPS
00131                                                                   ELGCSOPS
00132 ************************************************************      ELGCSOPS
00133 *                                                          *      ELGCSOPS
00134 *        SIGNAL ABEND                                      *      ELGCSOPS
00135 *                                                          *      ELGCSOPS
00136 ************************************************************      ELGCSOPS
00137  SIGNAL-ABEND.                                                    ELGCSOPS
00138      EXEC CICS ABEND                                              ELGCSOPS
00139                ABCODE(CIA-ABCODE)                                 ELGCSOPS
00140         END-EXEC.                                                 ELGCSOPS
00141                                                                   ELGCSOPS
00142                                                                   ELGCSOPS
00143 ************************************************************      ELGCSOPS
00144 *                                                          *      ELGCSOPS
00145 *        ESTABLISH ADDR OF OUTPUT INTERFACE                *      ELGCSOPS
00146 *                                                          *      ELGCSOPS
00147 ************************************************************      ELGCSOPS
00148  ESTABLISH-ADDR-OF-OUTPUT-INTER.                                  ELGCSOPS
00149      PERFORM ESTABLISH-ADDRESS-OF-OUTP.                           ELGCSOPS
00150      IF CIA-RC-PTR-NULL                                           ELGCSOPS
00151         PERFORM SIGNAL-UNALLOC-AREA-ERROR.                        ELGCSOPS
00152                                                                   ELGCSOPS
00153                                                                   ELGCSOPS
00154 ************************************************************      ELGCSOPS
00155 *                                                          *      ELGCSOPS
00156 *        ESTABLISH ADDR OF POINTER LIST TABLE              *      ELGCSOPS
00157 *                                                          *      ELGCSOPS
00158 ************************************************************      ELGCSOPS
00159  ESTABLISH-ADDR-OF-POINTER-LIST.                                  ELGCSOPS
00160      PERFORM ESTABLISH-ADDRESS-OF-CSPT.                           ELGCSOPS
00161      IF CIA-RC-PTR-NULL                                           ELGCSOPS
00162         PERFORM SIGNAL-UNALLOC-AREA-ERROR.                        ELGCSOPS
00163                                                                   ELGCSOPS
00164                                                                   ELGCSOPS
00165 ************************************************************      ELGCSOPS
00166 *                                                          *      ELGCSOPS
00167 *        ESTABLISH ADDRESS OF CIA                          *      ELGCSOPS
00168 *                                                          *      ELGCSOPS
00169 ************************************************************      ELGCSOPS
00170  ESTABLISH-ADDRESS-OF-CIA.                                        ELGCSOPS
00171      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGCSOPS
00172          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGCSOPS
00173                                                                   ELGCSOPS
00174                                                                   ELGCSOPS
00175 ************************************************************      ELGCSOPS
00176 *                                                          *      ELGCSOPS
00177 *        ESTABLISH ADDRESS OF OUTP                         *      ELGCSOPS
00178 *                                                          *      ELGCSOPS
00179 ************************************************************      ELGCSOPS
00180  ESTABLISH-ADDRESS-OF-OUTP.                                       ELGCSOPS
00181      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGCSOPS
00182      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSOPS
00183          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGCSOPS
00184                                                                   ELGCSOPS
00185                                                                   ELGCSOPS
00186 ************************************************************      ELGCSOPS
00187 *                                                          *      ELGCSOPS
00188 *        ESTABLISH ADDRESS OF CSPT                         *      ELGCSOPS
00189 *                                                          *      ELGCSOPS
00190 ************************************************************      ELGCSOPS
00191  ESTABLISH-ADDRESS-OF-CSPT.                                       ELGCSOPS
00192      SET CIA-ELSCSPTC-DDN TO TRUE.                                ELGCSOPS
00193      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSOPS
00194          ADDRESS OF CSPT-POINTER-LIST.                            ELGCSOPS
00195                                                                   ELGCSOPS
00196                                                                   ELGCSOPS
00197 ************************************************************      ELGCSOPS
00198 *                                                          *      ELGCSOPS
00199 *        PROCESS OUTPATIENT SERVICES                       *      ELGCSOPS
00200 *                                                          *      ELGCSOPS
00201 ************************************************************      ELGCSOPS
00202  PROCESS-OUTPATIENT-SERVICES.                                     ELGCSOPS
00203      PERFORM CONSTRUCT-HEADER-LINE.                               ELGCSOPS
00204      SET PROCESS-OPS TO TRUE.                                     ELGCSOPS
00205      PERFORM LINK-TO-ELUCSENT.                                    ELGCSOPS
00206      PERFORM LINK-TO-ELUCSOUT.                                    ELGCSOPS
00207      SET COMPLETED-OPS TO TRUE.                                   ELGCSOPS
00208                                                                   ELGCSOPS
00209                                                                   ELGCSOPS
00210 ************************************************************      ELGCSOPS
00211 *                                                          *      ELGCSOPS
00212 *        CONSTRUCT HEADER LINE                             *      ELGCSOPS
00213 *                                                          *      ELGCSOPS
00214 ************************************************************      ELGCSOPS
00215  CONSTRUCT-HEADER-LINE.                                           ELGCSOPS
00216      SET COF-NEW-PAGE TO TRUE.                                    ELGCSOPS
00217      MOVE +0 TO COF-NBR-DTL-LINES.                                ELGCSOPS
00218      MOVE +3 TO COF-NBR-HDR-LINES.                                ELGCSOPS
00219      SET COF-HDR-IDX TO 2.                                        ELGCSOPS
00220      MOVE WS-HEADER-LINE2 TO COF-HDR-LINE (COF-HDR-IDX).          ELGCSOPS
00221      SET COF-HDR-IDX UP BY 1.                                     ELGCSOPS
00222      MOVE WS-HEADER-LINE3 TO COF-HDR-LINE (COF-HDR-IDX).          ELGCSOPS
00223      PERFORM LINK-TO-ELUOUTPT.                                    ELGCSOPS
00224                                                                   ELGCSOPS
00225                                                                   ELGCSOPS
00226 ************************************************************      ELGCSOPS
00227 *                                                          *      ELGCSOPS
00228 *        LINK TO ELUOUTPT                                  *      ELGCSOPS
00229 *                                                          *      ELGCSOPS
00230 ************************************************************      ELGCSOPS
00231  LINK-TO-ELUOUTPT.                                                ELGCSOPS
00232      EXEC CICS LINK                                               ELGCSOPS
00233                PROGRAM ('ELUOUTPT')                               ELGCSOPS
00234                COMMAREA (DFHCOMMAREA)                             ELGCSOPS
00235                END-EXEC.                                          ELGCSOPS
00236                                                                   ELGCSOPS
00237                                                                   ELGCSOPS
00238 ************************************************************      ELGCSOPS
00239 *                                                          *      ELGCSOPS
00240 *        LINK TO ELUCSENT                                  *      ELGCSOPS
00241 *                                                          *      ELGCSOPS
00242 ************************************************************      ELGCSOPS
00243  LINK-TO-ELUCSENT.                                                ELGCSOPS
00244      EXEC CICS LINK                                               ELGCSOPS
00245                PROGRAM ('ELUCSENT')                               ELGCSOPS
00246                COMMAREA (DFHCOMMAREA)                             ELGCSOPS
00247                END-EXEC.                                          ELGCSOPS
00248                                                                   ELGCSOPS
00249                                                                   ELGCSOPS
00250 ************************************************************      ELGCSOPS
00251 *                                                          *      ELGCSOPS
00252 *        LINK TO ELUCSOUT                                  *      ELGCSOPS
00253 *                                                          *      ELGCSOPS
00254 ************************************************************      ELGCSOPS
00255  LINK-TO-ELUCSOUT.                                                ELGCSOPS
00256      EXEC CICS LINK                                               ELGCSOPS
00257                PROGRAM ('ELUCSOUT')                               ELGCSOPS
00258                COMMAREA (DFHCOMMAREA)                             ELGCSOPS
00259                END-EXEC.                                          ELGCSOPS
