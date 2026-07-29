00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGCSIPS
00003  PROGRAM-ID.         ELGCSIPS.                                       LV002
00004                                                                   ELGCSIPS
00005  AUTHOR.             RICK BARILEAU.                               ELGCSIPS
00006                                                                   ELGCSIPS
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGCSIPS
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELGCSIPS
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGCSIPS
00010                      233 N. MICHIGAN AVE                          ELGCSIPS
00011                      CHICAGO, ILLINOIS 60601                      ELGCSIPS
00012                                                                   ELGCSIPS
00013  DATE-WRITTEN.       11-FEB-1988.                                 ELGCSIPS
00014                                                                   ELGCSIPS
00015  DATE-COMPILED.                                                   ELGCSIPS
00016                                                                   ELGCSIPS
00017  SECURITY.           COPYRIGHT 1986,                              ELGCSIPS
00018                      HEALTH CARE SERVICE CORPORATION              ELGCSIPS
00019      SKIP3                                                        ELGCSIPS
00020  ENVIRONMENT DIVISION.                                            ELGCSIPS
00021                                                                   ELGCSIPS
00022  CONFIGURATION SECTION.                                           ELGCSIPS
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELGCSIPS
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELGCSIPS
00025 ******************************************************************ELGCSIPS
00026 *                                                                *ELGCSIPS
00027 *  ELGCSIPS - ELS:  THIS MODULE GATHERS ALL INFORMATION THAT     *ELGCSIPS
00028 *                   PERTAINS TO INPATIENT PHYSICIANS SERVICES    *ELGCSIPS
00029 *                   WITH THE CONTRACT AND WILL DISPLAY IT.       *ELGCSIPS
00030 *                                                                *ELGCSIPS
00031 *  NOTES :  THE MODULES ELUCSCOV AND ELUCSPLG WILL ALREADY HAVE  *ELGCSIPS
00032 *           DETERMINED THE COVERAGE AND PAYMENT LEVEL FOR EACH   *ELGCSIPS
00033 *           BENEFIT PROVISION ASSOCIATED WITH THIS SUBTOPIC      *ELGCSIPS
00034 *           PRIOR TO REACHING THIS PROGRAM.                      *ELGCSIPS
00035 *                                                                *ELGCSIPS
00036 *  THE FOLLOWING ARE THOSE BENEFIT PROVISIONS INVOLVED :         *ELGCSIPS
00037 *           SRGI C  =  SURGERY                                   *ELGCSIPS
00038 *           XRYI E  =  XRAY                                      *ELGCSIPS
00039 *           LABI E  =  LABORATORY                                *ELGCSIPS
00040 *           DXTI E  =  RADIATION THERAPY                         *ELGCSIPS
00041 *           MCHI E  =  CHEMO THERAPY                             *ELGCSIPS
00042 *           HVD  D  =  MEDICAL VISITS                            *ELGCSIPS
00043 *           ANSI C  =  ANESTHESIA                                *ELGCSIPS
00044 *           CONI E  =  CONSULTATION                              *ELGCSIPS
00045 *           ASSI C  =  ASSISTANT SURGEON                         *ELGCSIPS
00046 *                                                                *ELGCSIPS
00047 ******************************************************************ELGCSIPS
00048 *                                                                *ELGCSIPS
00049 *                      MAINTENANCE HISTORY                       *ELGCSIPS
00050 *                                                                *ELGCSIPS
00051 *  MOD     DATE     BY  DRPT                ACTION               *ELGCSIPS
00052 * ----- ----------- --- ----- ---------------------------------- *ELGCSIPS
00053 * 01.00 11-FEB-1988 REB       CREATED                            *ELGCSIPS
00054 * 01.01 04-MAR-1988 REB       REMOVED CODE FOR DISCLAIMER MESSAGE*ELGCSIPS
00055 * 02.00 31-MAR-1989 NAC       DESTRUCT CONVERSION USING STRUCT-  *ELGCSIPS
00056 *                             URES VER: 3.5.                     *ELGCSIPS
00057 * 03.00 04-APR-1989 GEM       STORAGE MANAGEMENT ENHANCEMENTS    *ELGCSIPS
00058 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGCSIPS
00059 ******************************************************************ELGCSIPS
00060                                                                   ELGCSIPS
00061  DATA DIVISION.                                                   ELGCSIPS
00062  WORKING-STORAGE SECTION.                                         ELGCSIPS
00063  01  WS-MISC.                                                     ELGCSIPS
00064      05  FILLER                  PIC  X(24) VALUE                 ELGCSIPS
00065          '** ELGCSIPS WS BEGINS **'.                              ELGCSIPS
00066                                                                   ELGCSIPS
00067  01  WS-FIXED-TEXT-DESCRIPTION.                                   ELGCSIPS
00068      05  WS-IPS-HEADER-1.                                         ELGCSIPS
00069          10  FILLER              PIC X(79) VALUE 'THE FOLLOWING DIELGCSIPS
00070 -       'SPLAYED BENEFITS ARE FOR: INPATIENT PHYSICIANS SERVICES'.ELGCSIPS
00071      05  WS-DASH-LINE.                                            ELGCSIPS
00072          10  FILLER              PIC X(70) VALUE '----------------ELGCSIPS
00073 -        '------------------------------------------------------'.ELGCSIPS
00074          10  FILLER              PIC X(09) VALUE '---------'.     ELGCSIPS
00075                                                                   ELGCSIPS
00076  LINKAGE SECTION.                                                 ELGCSIPS
00077  01  DFHCOMMAREA.                                                 ELGCSIPS
00078      COPY ELSCOMMC.                                               ELGCSIPS
00079 /                                                                 ELGCSIPS
00080      COPY ELSCIA2C.                                               ELGCSIPS
00081 /                                                                 ELGCSIPS
00082      COPY ELSIOPMC.                                               ELGCSIPS
00083 /                                                                 ELGCSIPS
00084      COPY ELSKEYSC.                                               ELGCSIPS
00085 /                                                                 ELGCSIPS
00086      COPY ELSSRTPC.                                               ELGCSIPS
00087 /                                                                 ELGCSIPS
00088      COPY ELSTCWAC.                                               ELGCSIPS
00089 /                                                                 ELGCSIPS
00090      COPY ELSOUTPC.                                               ELGCSIPS
00091 /    COPYBOOK FOR CONTRACT SUMMARY POINTER TABLE                  ELGCSIPS
00092      COPY ELSCSPTC.                                               ELGCSIPS
00093 /    COPYBOOK FOR CONTRACT SUMMARY BENEFIT PROVISION TABLE        ELGCSIPS
00094      COPY ELSCSBPC.                                               ELGCSIPS
00095 /                                                                 ELGCSIPS
00096  PROCEDURE DIVISION.                                              ELGCSIPS
00097 ************************************************************      ELGCSIPS
00098 *                                                          *      ELGCSIPS
00099 *                    PROCEDURE DIVISION                    *      ELGCSIPS
00100 *                                                          *      ELGCSIPS
00101 ************************************************************      ELGCSIPS
00102                                                                   ELGCSIPS
00103      PERFORM INITIALIZATION.                                      ELGCSIPS
00104      PERFORM PROCESS.                                             ELGCSIPS
00105      GOBACK.                                                      ELGCSIPS
00106                                                                   ELGCSIPS
00107                                                                   ELGCSIPS
00108 ************************************************************      ELGCSIPS
00109 *                                                          *      ELGCSIPS
00110 *        INITIALIZATION                                    *      ELGCSIPS
00111 *                                                          *      ELGCSIPS
00112 ************************************************************      ELGCSIPS
00113  INITIALIZATION.                                                  ELGCSIPS
00114      PERFORM ESTABLISH-ADDR-OF-CNTRL-BLKS.                        ELGCSIPS
00115      PERFORM ESTABLISH-ADDR-OF-WORKAREAS.                         ELGCSIPS
00116                                                                   ELGCSIPS
00117                                                                   ELGCSIPS
00118 ************************************************************      ELGCSIPS
00119 *                                                          *      ELGCSIPS
00120 *        ESTABLISH ADDR OF CNTRL BLKS                      *      ELGCSIPS
00121 *                                                          *      ELGCSIPS
00122 ************************************************************      ELGCSIPS
00123  ESTABLISH-ADDR-OF-CNTRL-BLKS.                                    ELGCSIPS
00124      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGCSIPS
00125          PERFORM SIGNAL-INVALID-COMMAREA.                         ELGCSIPS
00126                                                                   ELGCSIPS
00127      IF ECA-CIA-PTR = NULL                                        ELGCSIPS
00128          PERFORM SIGNAL-INVALID-CIA                               ELGCSIPS
00129      ELSE                                                         ELGCSIPS
00130          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELGCSIPS
00131                                                                   ELGCSIPS
00132                                                                   ELGCSIPS
00133 ************************************************************      ELGCSIPS
00134 *                                                          *      ELGCSIPS
00135 *        SIGNAL INVALID COMMAREA                           *      ELGCSIPS
00136 *                                                          *      ELGCSIPS
00137 ************************************************************      ELGCSIPS
00138  SIGNAL-INVALID-COMMAREA.                                         ELGCSIPS
00139      EXEC CICS ABEND                                              ELGCSIPS
00140                ABCODE('EL01')                                     ELGCSIPS
00141         END-EXEC.                                                 ELGCSIPS
00142                                                                   ELGCSIPS
00143                                                                   ELGCSIPS
00144 ************************************************************      ELGCSIPS
00145 *                                                          *      ELGCSIPS
00146 *        SIGNAL INVALID CIA                                *      ELGCSIPS
00147 *                                                          *      ELGCSIPS
00148 ************************************************************      ELGCSIPS
00149  SIGNAL-INVALID-CIA.                                              ELGCSIPS
00150      EXEC CICS ABEND                                              ELGCSIPS
00151                ABCODE('EL02')                                     ELGCSIPS
00152         END-EXEC.                                                 ELGCSIPS
00153                                                                   ELGCSIPS
00154                                                                   ELGCSIPS
00155 ************************************************************      ELGCSIPS
00156 *                                                          *      ELGCSIPS
00157 *        SIGNAL UNALLOC AREA ERROR                         *      ELGCSIPS
00158 *                                                          *      ELGCSIPS
00159 ************************************************************      ELGCSIPS
00160  SIGNAL-UNALLOC-AREA-ERROR.                                       ELGCSIPS
00161      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGCSIPS
00162      PERFORM SIGNAL-ABEND.                                        ELGCSIPS
00163                                                                   ELGCSIPS
00164                                                                   ELGCSIPS
00165 ************************************************************      ELGCSIPS
00166 *                                                          *      ELGCSIPS
00167 *        SIGNAL ABEND                                      *      ELGCSIPS
00168 *                                                          *      ELGCSIPS
00169 ************************************************************      ELGCSIPS
00170  SIGNAL-ABEND.                                                    ELGCSIPS
00171      EXEC CICS ABEND                                              ELGCSIPS
00172                ABCODE(CIA-ABCODE)                                 ELGCSIPS
00173         END-EXEC.                                                 ELGCSIPS
00174                                                                   ELGCSIPS
00175                                                                   ELGCSIPS
00176 ************************************************************      ELGCSIPS
00177 *                                                          *      ELGCSIPS
00178 *        ESTABLISH ADDR OF WORKAREAS                       *      ELGCSIPS
00179 *                                                          *      ELGCSIPS
00180 ************************************************************      ELGCSIPS
00181  ESTABLISH-ADDR-OF-WORKAREAS.                                     ELGCSIPS
00182      PERFORM ESTABLISH-ADDRESS-OF-OUTP.                           ELGCSIPS
00183      IF CIA-RC-PTR-NULL                                           ELGCSIPS
00184          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSIPS
00185                                                                   ELGCSIPS
00186      PERFORM ESTABLISH-ADDRESS-OF-CSPT.                           ELGCSIPS
00187      IF CIA-RC-PTR-NULL                                           ELGCSIPS
00188          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSIPS
00189                                                                   ELGCSIPS
00190      IF CSPT-IPS-BP-TBL-PTR = NULL                                ELGCSIPS
00191          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELGCSIPS
00192      ELSE                                                         ELGCSIPS
00193          PERFORM ESTABLISH-ADDRESS-OF-CSBPC.                      ELGCSIPS
00194                                                                   ELGCSIPS
00195                                                                   ELGCSIPS
00196 ************************************************************      ELGCSIPS
00197 *                                                          *      ELGCSIPS
00198 *        ESTABLISH ADDRESS OF CIA                          *      ELGCSIPS
00199 *                                                          *      ELGCSIPS
00200 ************************************************************      ELGCSIPS
00201  ESTABLISH-ADDRESS-OF-CIA.                                        ELGCSIPS
00202      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGCSIPS
00203          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGCSIPS
00204                                                                   ELGCSIPS
00205                                                                   ELGCSIPS
00206 ************************************************************      ELGCSIPS
00207 *                                                          *      ELGCSIPS
00208 *        ESTABLISH ADDRESS OF OUTP                         *      ELGCSIPS
00209 *                                                          *      ELGCSIPS
00210 ************************************************************      ELGCSIPS
00211  ESTABLISH-ADDRESS-OF-OUTP.                                       ELGCSIPS
00212      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGCSIPS
00213      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIPS
00214          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGCSIPS
00215                                                                   ELGCSIPS
00216                                                                   ELGCSIPS
00217 ************************************************************      ELGCSIPS
00218 *                                                          *      ELGCSIPS
00219 *        ESTABLISH ADDRESS OF CSPT                         *      ELGCSIPS
00220 *                                                          *      ELGCSIPS
00221 ************************************************************      ELGCSIPS
00222  ESTABLISH-ADDRESS-OF-CSPT.                                       ELGCSIPS
00223      SET CIA-ELSCSPTC-DDN TO TRUE.                                ELGCSIPS
00224      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIPS
00225          ADDRESS OF CSPT-POINTER-LIST.                            ELGCSIPS
00226                                                                   ELGCSIPS
00227                                                                   ELGCSIPS
00228 ************************************************************      ELGCSIPS
00229 *                                                          *      ELGCSIPS
00230 *        ESTABLISH ADDRESS OF CSBPC                        *      ELGCSIPS
00231 *                                                          *      ELGCSIPS
00232 ************************************************************      ELGCSIPS
00233  ESTABLISH-ADDRESS-OF-CSBPC.                                      ELGCSIPS
00234      SET ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE TO               ELGCSIPS
00235          CSPT-IPS-BP-TBL-PTR.                                     ELGCSIPS
00236                                                                   ELGCSIPS
00237                                                                   ELGCSIPS
00238 ************************************************************      ELGCSIPS
00239 *                                                          *      ELGCSIPS
00240 *        PROCESS                                           *      ELGCSIPS
00241 *                                                          *      ELGCSIPS
00242 ************************************************************      ELGCSIPS
00243  PROCESS.                                                         ELGCSIPS
00244      PERFORM DISPLAY-HEADINGS-FOR-SUBTOPIC.                       ELGCSIPS
00245      SET PROCESS-IPS   TO TRUE.                                   ELGCSIPS
00246      PERFORM CALL-SENTENCE-INTERFACE.                             ELGCSIPS
00247      PERFORM CALL-CONTRACT-SUMMARY-OUTPUT-I.                      ELGCSIPS
00248      SET COMPLETED-IPS TO TRUE.                                   ELGCSIPS
00249                                                                   ELGCSIPS
00250                                                                   ELGCSIPS
00251 ************************************************************      ELGCSIPS
00252 *                                                          *      ELGCSIPS
00253 *        DISPLAY HEADINGS FOR SUBTOPIC                     *      ELGCSIPS
00254 *                                                          *      ELGCSIPS
00255 ************************************************************      ELGCSIPS
00256  DISPLAY-HEADINGS-FOR-SUBTOPIC.                                   ELGCSIPS
00257      SET COF-NEW-PAGE     TO TRUE.                                ELGCSIPS
00258      MOVE +3              TO COF-NBR-HDR-LINES.                   ELGCSIPS
00259      MOVE +0              TO COF-NBR-DTL-LINES.                   ELGCSIPS
00260      MOVE WS-IPS-HEADER-1 TO COF-HDR-LINE (2).                    ELGCSIPS
00261      MOVE WS-DASH-LINE    TO COF-HDR-LINE (3).                    ELGCSIPS
00262      PERFORM CALL-OUTPUT-INTERFACE.                               ELGCSIPS
00263                                                                   ELGCSIPS
00264                                                                   ELGCSIPS
00265 ************************************************************      ELGCSIPS
00266 *                                                          *      ELGCSIPS
00267 *        CALL OUTPUT INTERFACE                             *      ELGCSIPS
00268 *                                                          *      ELGCSIPS
00269 ************************************************************      ELGCSIPS
00270  CALL-OUTPUT-INTERFACE.                                           ELGCSIPS
00271      EXEC CICS LINK                                               ELGCSIPS
00272                PROGRAM ('ELUOUTPT')                               ELGCSIPS
00273                COMMAREA (DFHCOMMAREA)                             ELGCSIPS
00274         END-EXEC.                                                 ELGCSIPS
00275                                                                   ELGCSIPS
00276                                                                   ELGCSIPS
00277 ************************************************************      ELGCSIPS
00278 *                                                          *      ELGCSIPS
00279 *        CALL SENTENCE INTERFACE                           *      ELGCSIPS
00280 *                                                          *      ELGCSIPS
00281 ************************************************************      ELGCSIPS
00282  CALL-SENTENCE-INTERFACE.                                         ELGCSIPS
00283      EXEC CICS LINK                                               ELGCSIPS
00284                PROGRAM ('ELUCSENT')                               ELGCSIPS
00285                COMMAREA (DFHCOMMAREA)                             ELGCSIPS
00286         END-EXEC.                                                 ELGCSIPS
00287                                                                   ELGCSIPS
00288                                                                   ELGCSIPS
00289 ************************************************************      ELGCSIPS
00290 *                                                          *      ELGCSIPS
00291 *        CALL CONTRACT SUMMARY OUTPUT INTERFACE            *      ELGCSIPS
00292 *                                                          *      ELGCSIPS
00293 ************************************************************      ELGCSIPS
00294  CALL-CONTRACT-SUMMARY-OUTPUT-I.                                  ELGCSIPS
00295      EXEC CICS LINK                                               ELGCSIPS
00296                PROGRAM ('ELUCSOUT')                               ELGCSIPS
00297                COMMAREA (DFHCOMMAREA)                             ELGCSIPS
00298         END-EXEC.                                                 ELGCSIPS
