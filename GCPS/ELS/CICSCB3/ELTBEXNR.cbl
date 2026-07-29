00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELTBEXNR
00003  PROGRAM-ID.         ELTBEXNR.                                       LV002
00004                                                                   ELTBEXNR
00005  AUTHOR.             ANNE KEFFER KING.                            ELTBEXNR
00006                                                                   ELTBEXNR
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTBEXNR
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTBEXNR
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTBEXNR
00010                      233 N. MICHIGAN AVE                          ELTBEXNR
00011                      CHICAGO, ILLINOIS 60601                      ELTBEXNR
00012                                                                   ELTBEXNR
00013  DATE-WRITTEN.       11-FEB-1994.                                 ELTBEXNR
00014                                                                   ELTBEXNR
00015  DATE-COMPILED.                                                   ELTBEXNR
00016                                                                   ELTBEXNR
00017  SECURITY.           COPYRIGHT 1986,                              ELTBEXNR
00018                      HEALTH CARE SERVICE CORPORATION              ELTBEXNR
00019      SKIP3                                                        ELTBEXNR
00020  ENVIRONMENT DIVISION.                                            ELTBEXNR
00021                                                                   ELTBEXNR
00022  CONFIGURATION SECTION.                                           ELTBEXNR
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELTBEXNR
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELTBEXNR
00025      EJECT                                                        ELTBEXNR
00026 ******************************************************************ELTBEXNR
00027 *                    MAINTENANCE HISTORY                         *ELTBEXNR
00028 *                                                                *ELTBEXNR
00029 *  MOD     DATE     BY  DPRT            ACTION                   *ELTBEXNR
00030 * ----- ----------- --- ----- ---------------------------------- *ELTBEXNR
00031 * 01.00 11-FEB-1994 AKK       CREATED                            *ELTBEXNR
00032 *                                                                *ELTBEXNR
00033 * 01.01 23-JUN-1994 AKK       ADD CODE TO ACCEPT CODE VALUES OF  *ELTBEXNR
00034 *                             LONGER THAN 20 TRANSLATED LINES.   *ELTBEXNR
00035 *                             THE LACK OF THIS CODE WAS CAUSING  *ELTBEXNR
00036 *                              SYSTEM OUTAGES.                   *ELTBEXNR
00037 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTBEXNR
00038 ******************************************************************ELTBEXNR
00039 /                                                                 ELTBEXNR
00040  DATA DIVISION.                                                   ELTBEXNR
00041                                                                   ELTBEXNR
00042  WORKING-STORAGE SECTION.                                         ELTBEXNR
00043  01  WS-BEGIN                    PIC X(26)  VALUE                 ELTBEXNR
00044                                 '*** ELTBEXNR WS BEGINS ***'.     ELTBEXNR
00045 *                                                                 ELTBEXNR
00046  01  WS-BEN-HDR-LINE.                                             ELTBEXNR
00047      05  FILLER                          PIC X(26) VALUE SPACES.  ELTBEXNR
00048      05  WS-HDR-LINE                     PIC X(27) VALUE          ELTBEXNR
00049      'BENEFIT EXCEPTION NARRATIVE'.                               ELTBEXNR
00050      05  FILLER                          PIC X(26) VALUE SPACES.  ELTBEXNR
00051                                                                   ELTBEXNR
00052  01  WS-NOT-APPLCBL-MSG.                                          ELTBEXNR
00053      05  WS-NOT-APLCBL-MSG1              PIC X(62) VALUE          ELTBEXNR
00054      'SPECIAL BENEFIT EXCEPTIONS DO NOT APPLY TO THIS GROUP/SECTIOELTBEXNR
00055 -    'N.'.                                                        ELTBEXNR
00056                                                                   ELTBEXNR
00057  01  WS-END                              PIC X(18) VALUE          ELTBEXNR
00058                                          '*** END OF W/S ***'.    ELTBEXNR
00059                                                                   ELTBEXNR
00060  LINKAGE SECTION.                                                 ELTBEXNR
00061  01  DFHCOMMAREA.                                                 ELTBEXNR
00062      COPY ELSCOMMC.                                               ELTBEXNR
00063 /                                                                 ELTBEXNR
00064      COPY ELSCIA2C.                                               ELTBEXNR
00065 /                                                                 ELTBEXNR
00066      COPY ELSCMDSC.                                               ELTBEXNR
00067 /                                                                 ELTBEXNR
00068      COPY ELSCMIFC.                                               ELTBEXNR
00069 /                                                                 ELTBEXNR
00070      COPY ELSIOPMC.                                               ELTBEXNR
00071 /                                                                 ELTBEXNR
00072      COPY ELSOUTPC.                                               ELTBEXNR
00073 /                                                                 ELTBEXNR
00074      COPY ELSSSCBC.                                               ELTBEXNR
00075 /                                                                 ELTBEXNR
00076  01  GROUP-SPECIFIC-RECORD.                                       ELTBEXNR
00077      COPY GCGROUPC.                                               ELTBEXNR
00078 /                                                                 ELTBEXNR
00079      EJECT                                                        ELTBEXNR
00080  PROCEDURE DIVISION.                                              ELTBEXNR
00081 ************************************************************      ELTBEXNR
00082 *                                                          *      ELTBEXNR
00083 *                    PROCEDURE DIVISION                    *      ELTBEXNR
00084 *                                                          *      ELTBEXNR
00085 ************************************************************      ELTBEXNR
00086                                                                   ELTBEXNR
00087                                                                   ELTBEXNR
00088 ************************************************************      ELTBEXNR
00089 *                                                          *      ELTBEXNR
00090 *        BENEFIT EXCEPTION NARRATIVE                       *      ELTBEXNR
00091 *                                                          *      ELTBEXNR
00092 ************************************************************      ELTBEXNR
00093  BENEFIT-EXCEPTION-NARRATIVE.                                     ELTBEXNR
00094      PERFORM 0000-INITIALIZATION.                                 ELTBEXNR
00095      PERFORM 1000-PROCESS-BNFT-EXCPTN-NRTV.                       ELTBEXNR
00096      SET COF-END TO TRUE.                                         ELTBEXNR
00097      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTBEXNR
00098                            DFHCOMMAREA.                           ELTBEXNR
00099      GOBACK.                                                      ELTBEXNR
00100                                                                   ELTBEXNR
00101  0000-INITIALIZATION.                                             ELTBEXNR
00102      PERFORM 0100-ESTBLSH-ENVRNMNT.                               ELTBEXNR
00103      PERFORM 0150-ESTBLSH-ADDRSS-SSB.                             ELTBEXNR
00104      PERFORM 0200-ESTBLSH-ADDRSS-CDS-MNL.                         ELTBEXNR
00105      PERFORM 0350-ESTBLSH-ADDRSS-OUTPT.                           ELTBEXNR
00106      PERFORM 0450-ESTBLSH-ADDRSS-GRP-SPCFC.                       ELTBEXNR
00107                                                                   ELTBEXNR
00108 ************************************************************      ELTBEXNR
00109 *                                                          *      ELTBEXNR
00110 *        ESTABLISH ENVIRONMENT                             *      ELTBEXNR
00111 *                                                          *      ELTBEXNR
00112 ************************************************************      ELTBEXNR
00113  0100-ESTBLSH-ENVRNMNT.                                           ELTBEXNR
00114      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELTBEXNR
00115         EXEC CICS ABEND                                           ELTBEXNR
00116                   ABCODE ('EL01')                                 ELTBEXNR
00117         END-EXEC                                                  ELTBEXNR
00118      END-IF.                                                      ELTBEXNR
00119      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTBEXNR
00120                   ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.       ELTBEXNR
00121      IF ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA = NULL           ELTBEXNR
00122            EXEC CICS ABEND                                        ELTBEXNR
00123                 ABCODE ('EL02')                                   ELTBEXNR
00124            END-EXEC                                               ELTBEXNR
00125      END-IF.                                                      ELTBEXNR
00126                                                                   ELTBEXNR
00127 ************************************************************      ELTBEXNR
00128 *                                                          *      ELTBEXNR
00129 *        ESTABLISH SSB ADDRESS                             *      ELTBEXNR
00130 *                                                          *      ELTBEXNR
00131 ************************************************************      ELTBEXNR
00132  0150-ESTBLSH-ADDRSS-SSB.                                         ELTBEXNR
00133      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTBEXNR
00134      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBEXNR
00135                      ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.      ELTBEXNR
00136      IF CIA-RC-PTR-NULL                                           ELTBEXNR
00137         SET CIA-AB-PARM-MISSING TO TRUE                           ELTBEXNR
00138         EXEC CICS ABEND                                           ELTBEXNR
00139                  ABCODE(CIA-ABCODE)                               ELTBEXNR
00140         END-EXEC                                                  ELTBEXNR
00141      END-IF.                                                      ELTBEXNR
00142                                                                   ELTBEXNR
00143 ************************************************************      ELTBEXNR
00144 *                                                          *      ELTBEXNR
00145 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELTBEXNR
00146 *                                                          *      ELTBEXNR
00147 ************************************************************      ELTBEXNR
00148  0200-ESTBLSH-ADDRSS-CDS-MNL.                                     ELTBEXNR
00149      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTBEXNR
00150      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBEXNR
00151                            ADDRESS OF                             ELTBEXNR
00152          CMF-CODES-MANUAL-INTERFACE.                              ELTBEXNR
00153      IF CIA-RC-PTR-NULL                                           ELTBEXNR
00154          PERFORM 9000-SGNL-UNALLOC-ERR.                           ELTBEXNR
00155                                                                   ELTBEXNR
00156 ************************************************************      ELTBEXNR
00157 *                                                          *      ELTBEXNR
00158 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELTBEXNR
00159 *                                                          *      ELTBEXNR
00160 ************************************************************      ELTBEXNR
00161  0350-ESTBLSH-ADDRSS-OUTPT.                                       ELTBEXNR
00162      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTBEXNR
00163      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBEXNR
00164                            ADDRESS OF                             ELTBEXNR
00165          COF-OUTPUT-INTERFACE.                                    ELTBEXNR
00166      IF CIA-RC-PTR-NULL                                           ELTBEXNR
00167          PERFORM 9000-SGNL-UNALLOC-ERR.                           ELTBEXNR
00168                                                                   ELTBEXNR
00169 ************************************************************      ELTBEXNR
00170 *                                                          *      ELTBEXNR
00171 *        ESTABLISH ADDRESSABILITY OF GRP SPEC              *      ELTBEXNR
00172 *                                                          *      ELTBEXNR
00173 ************************************************************      ELTBEXNR
00174  0450-ESTBLSH-ADDRSS-GRP-SPCFC.                                   ELTBEXNR
00175      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTBEXNR
00176      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBEXNR
00177                            ADDRESS OF GROUP-SPECIFIC-RECORD.      ELTBEXNR
00178      IF CIA-RC-PTR-NULL                                           ELTBEXNR
00179          PERFORM 9000-SGNL-UNALLOC-ERR.                           ELTBEXNR
00180                                                                   ELTBEXNR
00181 ************************************************************      ELTBEXNR
00182 *                                                          *      ELTBEXNR
00183 *        SGNL UNALLOC AREA ERROR                           *      ELTBEXNR
00184 *                                                          *      ELTBEXNR
00185 ************************************************************      ELTBEXNR
00186  1000-PROCESS-BNFT-EXCPTN-NRTV.                                   ELTBEXNR
00187      MOVE 0 TO COF-NBR-DTL-LINES.                                 ELTBEXNR
00188      MOVE 2 TO COF-NBR-HDR-LINES.                                 ELTBEXNR
00189      SET COF-NEW-PAGE TO TRUE.                                    ELTBEXNR
00190      MOVE WS-BEN-HDR-LINE TO COF-HDR-LINE (COF-NBR-HDR-LINES).    ELTBEXNR
00191      IF GCG-PRODUCT-TYPE = ZEROES OR SPACES OR LOW-VALUES         ELTBEXNR
00192         PERFORM 2500-DSPLY-NOT-APLCBL-MSG                         ELTBEXNR
00193      ELSE                                                         ELTBEXNR
00194         PERFORM 1500-TRNSLT-DSPLY-PRDCT-TYPE                      ELTBEXNR
00195      END-IF.                                                      ELTBEXNR
00196                                                                   ELTBEXNR
00197 ************************************************************      ELTBEXNR
00198 *                                                          *      ELTBEXNR
00199 *     TRANSLATE AND DISPLAY PRODUCT TYPE                   *      ELTBEXNR
00200 *                                                          *      ELTBEXNR
00201 ************************************************************      ELTBEXNR
00202  1500-TRNSLT-DSPLY-PRDCT-TYPE.                                    ELTBEXNR
00203      MOVE 'GROUP' TO  CMF-RECORD-PREFIX.                          ELTBEXNR
00204      MOVE 'PRODUCT-TYPE' TO CMF-ELEMENT-SYSTEM-NAME.              ELTBEXNR
00205      MOVE GCG-PRODUCT-TYPE TO CMF-CODE-VALUE.                     ELTBEXNR
00206      EXEC CICS LINK                                               ELTBEXNR
00207                PROGRAM ('ELUCMIF')                                ELTBEXNR
00208                COMMAREA (DFHCOMMAREA)                             ELTBEXNR
00209                END-EXEC.                                          ELTBEXNR
00210      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTBEXNR
00211      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTBEXNR
00212                     ADDRESS OF CMF-DESCR.                         ELTBEXNR
00213      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTBEXNR
00214      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTBEXNR
00215      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTBEXNR
00216      PERFORM VARYING CMF-DESCR-IDX FROM                           ELTBEXNR
00217            1 BY 1 UNTIL CMF-DESCR-IDX >                           ELTBEXNR
00218               CMF-NBR-DESCR-LINES                                 ELTBEXNR
00219         MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                    ELTBEXNR
00220              COF-DTL-LINE (COF-NBR-DTL-LINES)                     ELTBEXNR
00221         ADD 1 TO COF-NBR-DTL-LINES                                ELTBEXNR
00222         PERFORM 1525-CHECK-FOR-INTRMDT-DISPLAY                    ELTBEXNR
00223      END-PERFORM.                                                 ELTBEXNR
00224      SET COF-CONTINUE TO TRUE.                                    ELTBEXNR
00225      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTBEXNR
00226                            DFHCOMMAREA.                           ELTBEXNR
00227                                                                   ELTBEXNR
00228 ************************************************************      ELTBEXNR
00229 *                                                          *      ELTBEXNR
00230 *        CHECK FOR INTERMEDIATE OUTPUT                     *      ELTBEXNR
00231 *                                                          *      ELTBEXNR
00232 ************************************************************      ELTBEXNR
00233  1525-CHECK-FOR-INTRMDT-DISPLAY.                                  ELTBEXNR
00234      IF COF-NBR-DTL-LINES > 17                                    ELTBEXNR
00235          AND CMF-DESCR-IDX >                                      ELTBEXNR
00236               CMF-NBR-DESCR-LINES                                 ELTBEXNR
00237          CONTINUE                                                 ELTBEXNR
00238      ELSE                                                         ELTBEXNR
00239         IF COF-NBR-DTL-LINES > 17                                 ELTBEXNR
00240             SET COF-CONTINUE TO TRUE                              ELTBEXNR
00241             CALL 'ELUOUTPT' USING DFHEIBLK                        ELTBEXNR
00242                                   DFHCOMMAREA                     ELTBEXNR
00243             MOVE 1 TO COF-NBR-DTL-LINES                           ELTBEXNR
00244      END-IF.                                                      ELTBEXNR
00245                                                                   ELTBEXNR
00246 ************************************************************      ELTBEXNR
00247 *                                                          *      ELTBEXNR
00248 *        DISPLAY NOT APPLICABLE MESSAGE                    *      ELTBEXNR
00249 *                                                          *      ELTBEXNR
00250 ************************************************************      ELTBEXNR
00251  2500-DSPLY-NOT-APLCBL-MSG.                                       ELTBEXNR
00252      SET COF-NEW-PAGE TO TRUE.                                    ELTBEXNR
00253      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTBEXNR
00254      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELTBEXNR
00255      ADD 1 TO COF-NBR-DTL-LINES.                                  ELTBEXNR
00256      MOVE WS-NOT-APPLCBL-MSG                                      ELTBEXNR
00257         TO COF-DTL-LINE (COF-NBR-DTL-LINES).                      ELTBEXNR
00258      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTBEXNR
00259                            DFHCOMMAREA.                           ELTBEXNR
00260                                                                   ELTBEXNR
00261 ************************************************************      ELTBEXNR
00262 *                                                          *      ELTBEXNR
00263 *        SGNL UNALLOC AREA ERROR                           *      ELTBEXNR
00264 *                                                          *      ELTBEXNR
00265 ************************************************************      ELTBEXNR
00266  9000-SGNL-UNALLOC-ERR.                                           ELTBEXNR
00267      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTBEXNR
00268      EXEC CICS ABEND                                              ELTBEXNR
00269                ABCODE(CIA-ABCODE)                                 ELTBEXNR
00270      END-EXEC.                                                    ELTBEXNR
00271                                                                   ELTBEXNR
