00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELUSTGMG
00003  PROGRAM-ID.         ELUSTGMG.                                       LV005
00004                                                                   ELUSTGMG
00005  AUTHOR.             RICHARD J. LUKETICH C.C.P.                   ELUSTGMG
00006                                                                   ELUSTGMG
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUSTGMG
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUSTGMG
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUSTGMG
00010                      233 N. MICHIGAN AVE                          ELUSTGMG
00011                      CHICAGO, ILLINOIS 60601                      ELUSTGMG
00012                                                                   ELUSTGMG
00013  DATE-WRITTEN.       10-NOV-1986.                                 ELUSTGMG
00014                                                                   ELUSTGMG
00015  DATE-COMPILED.                                                   ELUSTGMG
00016                                                                   ELUSTGMG
00017  SECURITY.           COPYRIGHT 1986,                              ELUSTGMG
00018                      HEALTH CARE SERVICE CORPORATION              ELUSTGMG
00019      SKIP3                                                        ELUSTGMG
00020  ENVIRONMENT DIVISION.                                            ELUSTGMG
00021                                                                   ELUSTGMG
00022  CONFIGURATION SECTION.                                           ELUSTGMG
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELUSTGMG
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELUSTGMG
00025 ***********************************************************       ELUSTGMG
00026 *                                                         *       ELUSTGMG
00027 *                                                         *       ELUSTGMG
00028 *    PROGRAM:    ELUSTGMG                                 *       ELUSTGMG
00029 *    AUTHOR:     RICHARD J. LUKETICH                      *       ELUSTGMG
00030 *    DATE:       04-NOV-1986                              *       ELUSTGMG
00031 *                                                         *       ELUSTGMG
00032 ***********************************************************       ELUSTGMG
00033 *               MAINTAINANCE HISTORY                      *       ELUSTGMG
00034 *                                                         *       ELUSTGMG
00035 *   MOD     DATE     BY   ACTION                          *       ELUSTGMG
00036 *                                                         *       ELUSTGMG
00037 *  01.00  04-NOV-86 RJL   CREATED                         *       ELUSTGMG
00038 *                                                         *       ELUSTGMG
00039 *  02.00  04-JAN-88 AKK   ADDED CODE TO MOVE LENGTH OF    *       ELUSTGMG
00040 *                         ELSCSPTC COPYBOOK TO CIA, THIS  *       ELUSTGMG
00041 *                         COPYBOOK IS USED WITH CONTRACT  *       ELUSTGMG
00042 *                         SUMMARY.                        *       ELUSTGMG
00043 *                                                         *       ELUSTGMG
00044 *  02.01  27-JUN-88 EGL   CHANGED TO SUPPORT NEW STORAGE  *       ELUSTGMG
00045 *                         MANAGEMENT SCHEME               *       ELUSTGMG
00046 *  02.02  22-SEP-88 EGL   CHANGED TO SUPPORT DATA AREAS   *       ELUSTGMG
00047 *                         ABOVE AND BELOW THE THE 16M     *       ELUSTGMG
00048 *                         LINE.                           *       ELUSTGMG
00049 *  02.03  09-OCT-89 EGL   DESTRUCTED                      *       ELUSTGMG
00050 *                                                         *       ELUSTGMG
00051 *  02.04  18-OCT-89 AKK   ADDED STATEMENT TO MOVE LENGTH  *       ELUSTGMG
00052 *                         OF ASCEND-DESCEND TABLE TO SMA  *       ELUSTGMG
00053 *                         ALSO ON OVER 32 AND OVER 64 K   *       ELUSTGMG
00054 *                         CHECK CHANGED ARGUEMENT TO < 1  *       ELUSTGMG
00055 *                         NOT LESS THAN ZERO.             *       ELUSTGMG
00056 *                                                         *       ELUSTGMG
00057 *                                                         *       ELUSTGMG
00058 * 01.01 01-APR-2003 AKK CHANGES FOR ENDEVOR                      *ELUSTGMG
00059 *       12-AUG-2003 AKK GEN IN QE TO TEST ORDER OF COMPILE       *ELUSTGMG
00060 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELUSTGMG
00061 *                                                                *ELUSTGMG
00062 * 02.01 09-JAN-2004 AKK S0C7 INTERTEST                            ELUSTGMG
00063 *                                                                *ELUSTGMG
00064 * 02.02 27-JAN-2004 AKK REGEN FOR COMPILER FIX                    ELUSTGMG
00065 *                                                                *ELUSTGMG
00066 * 03.00 25-APR-2005 AKK ADD CODE TO DIPLAY LENGTHS AND ADDRESS    ELUSTGMG
00067 *                       ES FOR GETMAINS                          *ELUSTGMG
00068 * 03.01 09-JUL-2005 AKK REMOVE CODE ADDED 04/25/05                ELUSTGMG
00069 **AKK 12/06/05 REGEN FOR TEST                                    *ELUSTGMG
00070 ***********************************************************       ELUSTGMG
00071      EJECT                                                        ELUSTGMG
00072  DATA DIVISION.                                                   ELUSTGMG
00073                                                                   ELUSTGMG
00074  WORKING-STORAGE SECTION.                                         ELUSTGMG
00075  77  WS-CIA-DDNAME            PICTURE X(8) VALUE SPACES.          ELUSTGMG
00076  77  WS-AREA-LEN              PICTURE S9(8) COMP.                 ELUSTGMG
00077                                                                   ELUSTGMG
00078      COPY HEXCOBOL.                                               ELUSTGMG
00079                                                                   ELUSTGMG
00080  LINKAGE SECTION.                                                 ELUSTGMG
00081                                                                   ELUSTGMG
00082  01  DFHCOMMAREA.                                                 ELUSTGMG
00083      COPY ELSCOMMC.                                               ELUSTGMG
00084      EJECT                                                        ELUSTGMG
00085      COPY ELSCIA2C.                                               ELUSTGMG
00086      EJECT                                                        ELUSTGMG
00087      COPY ELSSMAC.                                                ELUSTGMG
00088      EJECT                                                        ELUSTGMG
00089      COPY ELSIOPMC.                                               ELUSTGMG
00090      EJECT                                                        ELUSTGMG
00091  01  LS-AREA-TO-FREE             PICTURE  X(01).                  ELUSTGMG
00092      SKIP3                                                        ELUSTGMG
00093  01  LS-FROM-AREA                PICTURE  X(01).                  ELUSTGMG
00094      SKIP3                                                        ELUSTGMG
00095  01  LS-TO-AREA                  PICTURE  X(01).                  ELUSTGMG
00096      EJECT                                                        ELUSTGMG
00097 ******************************************************************ELUSTGMG
00098 *                                                                *ELUSTGMG
00099 *    THE FOLLOWING COPYBOOKS ARE INCLUDED ONLY FOR THE PURPOSE   *ELUSTGMG
00100 *    OF THE INITIALIZATION ROUTINE.  THE ROUTINE PLACES THE      *ELUSTGMG
00101 *    LENGTH OF EACH DATA STRUCTURE IN THE APPROPRIATE ENTRY IN   *ELUSTGMG
00102 *    THE STORAGE MANAGEMENT CONTROL TABLE.                       *ELUSTGMG
00103 *                                                                *ELUSTGMG
00104 ******************************************************************ELUSTGMG
00105  SKIP1                                                            ELUSTGMG
00106      COPY COBXIO2.                                                ELUSTGMG
00107      EJECT                                                        ELUSTGMG
00108  01  DBPIOPM-PARM.                                                ELUSTGMG
00109      COPY DBPIOPMC.                                               ELUSTGMG
00110      EJECT                                                        ELUSTGMG
00111      COPY ELSCSADC.                                               ELUSTGMG
00112      EJECT                                                        ELUSTGMG
00113      COPY ELSPLGSW.                                               ELUSTGMG
00114      EJECT                                                        ELUSTGMG
00115      COPY ELSCMIFC.                                               ELUSTGMG
00116      EJECT                                                        ELUSTGMG
00117      COPY ELSKEYSC.                                               ELUSTGMG
00118      EJECT                                                        ELUSTGMG
00119      COPY ELSOUTPC.                                               ELUSTGMG
00120      EJECT                                                        ELUSTGMG
00121      COPY ELSSRTPC.                                               ELUSTGMG
00122      EJECT                                                        ELUSTGMG
00123      COPY ELSSSCBC.                                               ELUSTGMG
00124      EJECT                                                        ELUSTGMG
00125      COPY ELSTCWAC.                                               ELUSTGMG
00126      EJECT                                                        ELUSTGMG
00127      COPY ELSTWAC.                                                ELUSTGMG
00128      EJECT                                                        ELUSTGMG
00129      COPY ELSCSPTC.                                               ELUSTGMG
00130      EJECT                                                        ELUSTGMG
00131  PROCEDURE DIVISION.                                              ELUSTGMG
00132 ************************************************************      ELUSTGMG
00133 *                                                          *      ELUSTGMG
00134 *        PERFORM STG MGR                                   *      ELUSTGMG
00135 *                                                          *      ELUSTGMG
00136 ************************************************************      ELUSTGMG
00137  PERFORM-STG-MGR.                                                 ELUSTGMG
00138      PERFORM INITIALIZE-MODULE.                                   ELUSTGMG
00139      PERFORM PROCESS-STG-MGR-REQUEST.                             ELUSTGMG
00140      PERFORM TERMINATE-MODULE.                                    ELUSTGMG
00141                                                                   ELUSTGMG
00142                                                                   ELUSTGMG
00143 ************************************************************      ELUSTGMG
00144 *                                                          *      ELUSTGMG
00145 *        INITIALIZE MODULE                                 *      ELUSTGMG
00146 *                                                          *      ELUSTGMG
00147 ************************************************************      ELUSTGMG
00148  INITIALIZE-MODULE.                                               ELUSTGMG
00149      PERFORM CHECK-COMMAREA-LENGTH.                               ELUSTGMG
00150      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELUSTGMG
00151      PERFORM ESTABLISH-ADDRESSING-TO-STG-MG.                      ELUSTGMG
00152      IF SMA-ELSCOMM-LEN IS EQUAL TO ZERO                          ELUSTGMG
00153          PERFORM INITIALIZE-AREA-LENGTHS.                         ELUSTGMG
00154      IF     NOT CIA-STG-INITIALIZE                                ELUSTGMG
00155                   AND NOT CIA-STG-PURGE                           ELUSTGMG
00156          PERFORM LOCATE-AREA-NAME-IN-TABLE.                       ELUSTGMG
00157      EJECT                                                        ELUSTGMG
00158                                                                   ELUSTGMG
00159                                                                   ELUSTGMG
00160 ************************************************************      ELUSTGMG
00161 *                                                          *      ELUSTGMG
00162 *        INITIALIZE AREA LENGTHS                           *      ELUSTGMG
00163 *                                                          *      ELUSTGMG
00164 ************************************************************      ELUSTGMG
00165  INITIALIZE-AREA-LENGTHS.                                         ELUSTGMG
00166      MOVE LENGTH OF CSEXECIO-CONTROL TO SMA-COBXIO-LEN.           ELUSTGMG
00167      MOVE LENGTH OF DBPIOPM-PARM TO SMA-DBPIOPM-LEN.              ELUSTGMG
00168      MOVE LENGTH OF PLS-PAYMENT-LEVEL-SWITCHES TO                 ELUSTGMG
00169          SMA-ELSPLGSW-LEN.                                        ELUSTGMG
00170      MOVE LENGTH OF CIA-ELS-COMMON-INTERFACE-AREA TO              ELUSTGMG
00171          SMA-ELSCIA-LEN.                                          ELUSTGMG
00172      MOVE LENGTH OF SMA-STORAGE-MANAGEMENT-AREA TO                ELUSTGMG
00173          SMA-ELSSMA-LEN.                                          ELUSTGMG
00174      MOVE LENGTH OF CMF-CODES-MANUAL-INTERFACE TO                 ELUSTGMG
00175          SMA-ELSCMIF-LEN.                                         ELUSTGMG
00176      MOVE LENGTH OF DFHCOMMAREA TO SMA-ELSCOMM-LEN.               ELUSTGMG
00177      MOVE LENGTH OF IOP-INPUT-OUTPUT-PARAMETERS TO                ELUSTGMG
00178          SMA-ELSIOPM-LEN.                                         ELUSTGMG
00179      MOVE LENGTH OF KWA-FILE-KEY-WORK-AREA TO SMA-ELSKEYS-LEN.    ELUSTGMG
00180      MOVE LENGTH OF COF-OUTPUT-INTERFACE TO SMA-ELSOUTP-LEN.      ELUSTGMG
00181      MOVE LENGTH OF SRP-SUBROUTINE-PARAMETERS TO SMA-ELSSRTP-LEN. ELUSTGMG
00182      MOVE LENGTH OF SSB-SELECTOR-STATUS-CTL-BLK TO                ELUSTGMG
00183          SMA-ELSSSCB-LEN.                                         ELUSTGMG
00184      MOVE LENGTH OF TCAR-COMPRESSION-WORK-AREA TO                 ELUSTGMG
00185          SMA-ELSTCWA-LEN.                                         ELUSTGMG
00186      MOVE LENGTH OF TWA-TRANSACTION-WORK-AREA TO SMA-ELSTWA-LEN.  ELUSTGMG
00187      MOVE LENGTH OF CSPT-POINTER-LIST TO SMA-ELSCSPTC-LEN.        ELUSTGMG
00188      MOVE LENGTH OF CSAD-ACL-TABLE TO SMA-ELSCSADC-LEN.           ELUSTGMG
00189      EJECT                                                        ELUSTGMG
00190                                                                   ELUSTGMG
00191                                                                   ELUSTGMG
00192 ************************************************************      ELUSTGMG
00193 *                                                          *      ELUSTGMG
00194 *        LOCATE AREA NAME IN TABLE                         *      ELUSTGMG
00195 *                                                          *      ELUSTGMG
00196 ************************************************************      ELUSTGMG
00197  LOCATE-AREA-NAME-IN-TABLE.                                       ELUSTGMG
00198      SEARCH ALL SMA-STG-MGT-TBL                                   ELUSTGMG
00199             AT END                                                ELUSTGMG
00200                 SET CIA-AB-AREANAME TO TRUE                       ELUSTGMG
00201                 EXEC CICS ABEND ABCODE (CIA-ABCODE)               ELUSTGMG
00202          END-EXEC                                                 ELUSTGMG
00203             WHEN SMA-DDN (SMA-STG-MGT-IDX) = CIA-DDNAME           ELUSTGMG
00204                 CONTINUE.                                         ELUSTGMG
00205      EJECT                                                        ELUSTGMG
00206                                                                   ELUSTGMG
00207                                                                   ELUSTGMG
00208 ************************************************************      ELUSTGMG
00209 *                                                          *      ELUSTGMG
00210 *        PROCESS STG MGR REQUEST                           *      ELUSTGMG
00211 *                                                          *      ELUSTGMG
00212 ************************************************************      ELUSTGMG
00213  PROCESS-STG-MGR-REQUEST.                                         ELUSTGMG
00214      IF    CIA-STG-INITIALIZE                                     ELUSTGMG
00215         OR CIA-STG-PURGE                                          ELUSTGMG
00216         OR CIA-STG-RETRIEVE                                       ELUSTGMG
00217         OR CIA-STG-STOW                                           ELUSTGMG
00218          PERFORM PROCESS-TEMP-STG-REQUEST                         ELUSTGMG
00219      ELSE IF    CIA-STG-FREEMAIN                                  ELUSTGMG
00220         OR CIA-STG-GETMAIN                                        ELUSTGMG
00221          PERFORM PROCESS-STORAGE-REQUEST                          ELUSTGMG
00222      ELSE                                                         ELUSTGMG
00223          PERFORM SIGNAL-INVALID-STG-MGR-REQUEST.                  ELUSTGMG
00224                                                                   ELUSTGMG
00225                                                                   ELUSTGMG
00226 ************************************************************      ELUSTGMG
00227 *                                                          *      ELUSTGMG
00228 *        PROCESS TEMP STG REQUEST                          *      ELUSTGMG
00229 *                                                          *      ELUSTGMG
00230 ************************************************************      ELUSTGMG
00231  PROCESS-TEMP-STG-REQUEST.                                        ELUSTGMG
00232      PERFORM SETUP-TEMP-STG-REQUEST.                              ELUSTGMG
00233      PERFORM EXECUTE-TEMP-STG-REQUEST.                            ELUSTGMG
00234                                                                   ELUSTGMG
00235                                                                   ELUSTGMG
00236 ************************************************************      ELUSTGMG
00237 *                                                          *      ELUSTGMG
00238 *        SETUP TEMP STG REQUEST                            *      ELUSTGMG
00239 *                                                          *      ELUSTGMG
00240 ************************************************************      ELUSTGMG
00241  SETUP-TEMP-STG-REQUEST.                                          ELUSTGMG
00242      IF     NOT CIA-STG-INITIALIZE                                ELUSTGMG
00243         AND NOT CIA-STG-PURGE                                     ELUSTGMG
00244          PERFORM DETERMINE-IF-TEMP-STG-AREA.                      ELUSTGMG
00245      PERFORM BUILD-TEMP-STG-QUEUE-ID.                             ELUSTGMG
00246      EJECT                                                        ELUSTGMG
00247                                                                   ELUSTGMG
00248                                                                   ELUSTGMG
00249 ************************************************************      ELUSTGMG
00250 *                                                          *      ELUSTGMG
00251 *        EXECUTE TEMP STG REQUEST                          *      ELUSTGMG
00252 *                                                          *      ELUSTGMG
00253 ************************************************************      ELUSTGMG
00254  EXECUTE-TEMP-STG-REQUEST.                                        ELUSTGMG
00255      IF CIA-STG-INITIALIZE                                        ELUSTGMG
00256          PERFORM INITIALIZE-TEMP-STG                              ELUSTGMG
00257      ELSE IF CIA-STG-PURGE                                        ELUSTGMG
00258          PERFORM PURGE-TEMP-STG                                   ELUSTGMG
00259      ELSE IF CIA-STG-RETRIEVE                                     ELUSTGMG
00260          PERFORM RETRIEVE-DATA-FROM-TEMP-STG                      ELUSTGMG
00261      ELSE IF CIA-STG-STOW                                         ELUSTGMG
00262          PERFORM STOW-DATA-IN-TEMP-STG                            ELUSTGMG
00263      ELSE                                                         ELUSTGMG
00264          PERFORM SIGNAL-INVALID-STG-MGR-REQUEST.                  ELUSTGMG
00265      EJECT                                                        ELUSTGMG
00266                                                                   ELUSTGMG
00267                                                                   ELUSTGMG
00268 ************************************************************      ELUSTGMG
00269 *                                                          *      ELUSTGMG
00270 *        DETERMINE IF TEMP STG AREA                        *      ELUSTGMG
00271 *                                                          *      ELUSTGMG
00272 ************************************************************      ELUSTGMG
00273  DETERMINE-IF-TEMP-STG-AREA.                                      ELUSTGMG
00274      SET CIA-TSQ-IDX TO 1.                                        ELUSTGMG
00275      SEARCH CIA-TSQ-TBL VARYING CIA-TSQ-IDX                       ELUSTGMG
00276             AT END                                                ELUSTGMG
00277                 SET CIA-AB-STG-INVREQ TO TRUE                     ELUSTGMG
00278                 EXEC CICS ABEND ABCODE (CIA-ABCODE)               ELUSTGMG
00279          END-EXEC                                                 ELUSTGMG
00280           WHEN CIA-TSQ-DDN (CIA-TSQ-IDX) = CIA-DDNAME             ELUSTGMG
00281                 CONTINUE.                                         ELUSTGMG
00282                                                                   ELUSTGMG
00283                                                                   ELUSTGMG
00284 ************************************************************      ELUSTGMG
00285 *                                                          *      ELUSTGMG
00286 *        BUILD TEMP STG QUEUE ID                           *      ELUSTGMG
00287 *                                                          *      ELUSTGMG
00288 ************************************************************      ELUSTGMG
00289  BUILD-TEMP-STG-QUEUE-ID.                                         ELUSTGMG
00290      MOVE EIBTRMID TO CIA-TSQ-ID-PFX.                             ELUSTGMG
00291      MOVE EIBTRNID TO CIA-TSQ-ID-SFX.                             ELUSTGMG
00292      EJECT                                                        ELUSTGMG
00293                                                                   ELUSTGMG
00294                                                                   ELUSTGMG
00295 ************************************************************      ELUSTGMG
00296 *                                                          *      ELUSTGMG
00297 *        INITIALIZE TEMP STG                               *      ELUSTGMG
00298 *                                                          *      ELUSTGMG
00299 ************************************************************      ELUSTGMG
00300  INITIALIZE-TEMP-STG.                                             ELUSTGMG
00301      PERFORM PURGE-TEMP-STG.                                      ELUSTGMG
00302      PERFORM CREATE-TEMP-STG-RECORDS                              ELUSTGMG
00303          VARYING CIA-TSQ-IDX                                      ELUSTGMG
00304             FROM 1                                                ELUSTGMG
00305               BY 1                                                ELUSTGMG
00306            UNTIL CIA-TSQ-IDX > CIA-TSQ-MAX.                       ELUSTGMG
00307                                                                   ELUSTGMG
00308                                                                   ELUSTGMG
00309 ************************************************************      ELUSTGMG
00310 *                                                          *      ELUSTGMG
00311 *        CREATE TEMP STG RECORDS                           *      ELUSTGMG
00312 *                                                          *      ELUSTGMG
00313 ************************************************************      ELUSTGMG
00314  CREATE-TEMP-STG-RECORDS.                                         ELUSTGMG
00315      PERFORM OBTAIN-INITIAL-RECORD-AREA.                          ELUSTGMG
00316      PERFORM STOW-RECORD-IN-TEMP-STG.                             ELUSTGMG
00317      IF CIA-TSQ-IDX > 1                                           ELUSTGMG
00318          PERFORM FREE-RECORD-AREA.                                ELUSTGMG
00319      EJECT                                                        ELUSTGMG
00320                                                                   ELUSTGMG
00321                                                                   ELUSTGMG
00322 ************************************************************      ELUSTGMG
00323 *                                                          *      ELUSTGMG
00324 *        OBTAIN INITIAL RECORD AREA                        *      ELUSTGMG
00325 *                                                          *      ELUSTGMG
00326 ************************************************************      ELUSTGMG
00327  OBTAIN-INITIAL-RECORD-AREA.                                      ELUSTGMG
00328      PERFORM SAVE-CURRENT-FUNCTION.                               ELUSTGMG
00329      MOVE CIA-TSQ-DDN (CIA-TSQ-IDX) TO CIA-DDNAME.                ELUSTGMG
00330      SET CIA-STG-GETMAIN TO TRUE.                                 ELUSTGMG
00331      MOVE +4 TO CIA-AREA-LEN.                                     ELUSTGMG
00332      PERFORM LOCATE-AREA-NAME-IN-TABLE.                           ELUSTGMG
00333      PERFORM PROCESS-STORAGE-REQUEST.                             ELUSTGMG
00334      PERFORM RESTORE-CURRENT-FUNCTION.                            ELUSTGMG
00335                                                                   ELUSTGMG
00336                                                                   ELUSTGMG
00337 ************************************************************      ELUSTGMG
00338 *                                                          *      ELUSTGMG
00339 *        STOW RECORD IN TEMP STG                           *      ELUSTGMG
00340 *                                                          *      ELUSTGMG
00341 ************************************************************      ELUSTGMG
00342  STOW-RECORD-IN-TEMP-STG.                                         ELUSTGMG
00343      SET ADDRESS OF LS-FROM-AREA TO SMA-PTR                       ELUSTGMG
00344          (SMA-STG-MGT-IDX).                                       ELUSTGMG
00345      SET CIA-TSQ-ITEM-NBR TO CIA-TSQ-IDX.                         ELUSTGMG
00346      MOVE CIA-AREA-LEN TO CIA-TSQ-TS-LEN.                         ELUSTGMG
00347      EXEC CICS WRITEQ TS                                          ELUSTGMG
00348                FROM   (LS-FROM-AREA       )                       ELUSTGMG
00349                ITEM   (CIA-TSQ-ITEM-NBR   )                       ELUSTGMG
00350                LENGTH (CIA-TSQ-TS-LEN     )                       ELUSTGMG
00351                QUEUE  (CIA-TSQ-ID         )                       ELUSTGMG
00352                END-EXEC.                                          ELUSTGMG
00353      EJECT                                                        ELUSTGMG
00354                                                                   ELUSTGMG
00355                                                                   ELUSTGMG
00356 ************************************************************      ELUSTGMG
00357 *                                                          *      ELUSTGMG
00358 *        FREE RECORD AREA                                  *      ELUSTGMG
00359 *                                                          *      ELUSTGMG
00360 ************************************************************      ELUSTGMG
00361  FREE-RECORD-AREA.                                                ELUSTGMG
00362      PERFORM SAVE-CURRENT-FUNCTION.                               ELUSTGMG
00363      MOVE CIA-TSQ-DDN (CIA-TSQ-IDX) TO CIA-DDNAME.                ELUSTGMG
00364      SET CIA-STG-FREEMAIN TO TRUE.                                ELUSTGMG
00365      PERFORM LOCATE-AREA-NAME-IN-TABLE.                           ELUSTGMG
00366      PERFORM PROCESS-STORAGE-REQUEST.                             ELUSTGMG
00367      PERFORM RESTORE-CURRENT-FUNCTION.                            ELUSTGMG
00368      EJECT                                                        ELUSTGMG
00369                                                                   ELUSTGMG
00370                                                                   ELUSTGMG
00371 ************************************************************      ELUSTGMG
00372 *                                                          *      ELUSTGMG
00373 *        PURGE TEMP STG                                    *      ELUSTGMG
00374 *                                                          *      ELUSTGMG
00375 ************************************************************      ELUSTGMG
00376  PURGE-TEMP-STG.                                                  ELUSTGMG
00377      EXEC CICS IGNORE CONDITION QIDERR END-EXEC.                  ELUSTGMG
00378      EXEC CICS DELETEQ TS                                         ELUSTGMG
00379                QUEUE (CIA-TSQ-ID)                                 ELUSTGMG
00380                END-EXEC.                                          ELUSTGMG
00381      EXEC CICS HANDLE CONDITION QIDERR END-EXEC.                  ELUSTGMG
00382      EJECT                                                        ELUSTGMG
00383                                                                   ELUSTGMG
00384                                                                   ELUSTGMG
00385 ************************************************************      ELUSTGMG
00386 *                                                          *      ELUSTGMG
00387 *        RETRIEVE DATA FROM TEMP STG                       *      ELUSTGMG
00388 *                                                          *      ELUSTGMG
00389 ************************************************************      ELUSTGMG
00390  RETRIEVE-DATA-FROM-TEMP-STG.                                     ELUSTGMG
00391      PERFORM RETRIEVE-REQUESTED-DATA.                             ELUSTGMG
00392      PERFORM OBTAIN-STORAGE-FOR-REQ-DATA.                         ELUSTGMG
00393      PERFORM MOVE-DATA-TO-STORAGE.                                ELUSTGMG
00394                                                                   ELUSTGMG
00395                                                                   ELUSTGMG
00396 ************************************************************      ELUSTGMG
00397 *                                                          *      ELUSTGMG
00398 *        RETRIEVE REQUESTED DATA                           *      ELUSTGMG
00399 *                                                          *      ELUSTGMG
00400 ************************************************************      ELUSTGMG
00401  RETRIEVE-REQUESTED-DATA.                                         ELUSTGMG
00402      SET CIA-TSQ-ITEM-NBR TO CIA-TSQ-IDX.                         ELUSTGMG
00403      EXEC CICS READQ TS                                           ELUSTGMG
00404                ITEM   (CIA-TSQ-ITEM-NBR)                          ELUSTGMG
00405                LENGTH (CIA-TSQ-TS-LEN  )                          ELUSTGMG
00406                QUEUE  (CIA-TSQ-ID      )                          ELUSTGMG
00407                SET    (CIA-TSQ-TS-PTR  )                          ELUSTGMG
00408                END-EXEC.                                          ELUSTGMG
00409      EJECT                                                        ELUSTGMG
00410                                                                   ELUSTGMG
00411                                                                   ELUSTGMG
00412 ************************************************************      ELUSTGMG
00413 *                                                          *      ELUSTGMG
00414 *        OBTAIN STORAGE FOR REQ DATA                       *      ELUSTGMG
00415 *                                                          *      ELUSTGMG
00416 ************************************************************      ELUSTGMG
00417  OBTAIN-STORAGE-FOR-REQ-DATA.                                     ELUSTGMG
00418      IF SMA-PTR(SMA-STG-MGT-IDX) IS NOT EQUAL TO NULL             ELUSTGMG
00419          PERFORM RELEASE-CURRENT-STORAGE.                         ELUSTGMG
00420      PERFORM SAVE-CURRENT-FUNCTION.                               ELUSTGMG
00421      SET CIA-STG-GETMAIN TO TRUE.                                 ELUSTGMG
00422      MOVE CIA-TSQ-TS-LEN TO CIA-AREA-LEN.                         ELUSTGMG
00423      PERFORM PROCESS-STORAGE-REQUEST.                             ELUSTGMG
00424      PERFORM RESTORE-CURRENT-FUNCTION.                            ELUSTGMG
00425                                                                   ELUSTGMG
00426                                                                   ELUSTGMG
00427 ************************************************************      ELUSTGMG
00428 *                                                          *      ELUSTGMG
00429 *        RELEASE CURRENT STORAGE                           *      ELUSTGMG
00430 *                                                          *      ELUSTGMG
00431 ************************************************************      ELUSTGMG
00432  RELEASE-CURRENT-STORAGE.                                         ELUSTGMG
00433      PERFORM SAVE-CURRENT-FUNCTION.                               ELUSTGMG
00434      SET CIA-STG-FREEMAIN TO TRUE.                                ELUSTGMG
00435      PERFORM PROCESS-STORAGE-REQUEST.                             ELUSTGMG
00436      PERFORM RESTORE-CURRENT-FUNCTION.                            ELUSTGMG
00437                                                                   ELUSTGMG
00438                                                                   ELUSTGMG
00439 ************************************************************      ELUSTGMG
00440 *                                                          *      ELUSTGMG
00441 *        MOVE DATA TO STORAGE                              *      ELUSTGMG
00442 *                                                          *      ELUSTGMG
00443 ************************************************************      ELUSTGMG
00444  MOVE-DATA-TO-STORAGE.                                            ELUSTGMG
00445      SET ADDRESS OF LS-FROM-AREA TO CIA-TSQ-TS-PTR.               ELUSTGMG
00446      SET ADDRESS OF LS-TO-AREA TO SMA-PTR                         ELUSTGMG
00447          (SMA-STG-MGT-IDX).                                       ELUSTGMG
00448      CALL 'ELUMVCL'                                               ELUSTGMG
00449           USING LS-FROM-AREA                                      ELUSTGMG
00450                 CIA-AREA-LEN                                      ELUSTGMG
00451                 LS-TO-AREA                                        ELUSTGMG
00452                 CIA-AREA-LEN                                      ELUSTGMG
00453                 HEX-40.                                           ELUSTGMG
00454      EJECT                                                        ELUSTGMG
00455                                                                   ELUSTGMG
00456                                                                   ELUSTGMG
00457 ************************************************************      ELUSTGMG
00458 *                                                          *      ELUSTGMG
00459 *        STOW DATA IN TEMP STG                             *      ELUSTGMG
00460 *                                                          *      ELUSTGMG
00461 ************************************************************      ELUSTGMG
00462  STOW-DATA-IN-TEMP-STG.                                           ELUSTGMG
00463      SET ADDRESS OF LS-FROM-AREA TO SMA-PTR                       ELUSTGMG
00464          (SMA-STG-MGT-IDX).                                       ELUSTGMG
00465      SET CIA-TSQ-ITEM-NBR TO CIA-TSQ-IDX.                         ELUSTGMG
00466      PERFORM DETERMINE-STOW-AREA-LENGTH.                          ELUSTGMG
00467      EXEC CICS WRITEQ TS                                          ELUSTGMG
00468                FROM   (LS-FROM-AREA    )                          ELUSTGMG
00469                ITEM   (CIA-TSQ-ITEM-NBR)                          ELUSTGMG
00470                LENGTH (CIA-TSQ-TS-LEN  )                          ELUSTGMG
00471                QUEUE  (CIA-TSQ-ID      )                          ELUSTGMG
00472                REWRITE                                            ELUSTGMG
00473                END-EXEC.                                          ELUSTGMG
00474                                                                   ELUSTGMG
00475                                                                   ELUSTGMG
00476 ************************************************************      ELUSTGMG
00477 *                                                          *      ELUSTGMG
00478 *        DETERMINE STOW AREA LENGTH                        *      ELUSTGMG
00479 *                                                          *      ELUSTGMG
00480 ************************************************************      ELUSTGMG
00481  DETERMINE-STOW-AREA-LENGTH.                                      ELUSTGMG
00482      IF SMA-LEN (SMA-STG-MGT-IDX) IS GREATER THAN ZERO            ELUSTGMG
00483          PERFORM SELECT-STANDARD-LENGTH                           ELUSTGMG
00484      ELSE                                                         ELUSTGMG
00485          PERFORM SELECT-USER-DEFINED-LENGTH.                      ELUSTGMG
00486                                                                   ELUSTGMG
00487                                                                   ELUSTGMG
00488 ************************************************************      ELUSTGMG
00489 *                                                          *      ELUSTGMG
00490 *        SELECT STANDARD LENGTH                            *      ELUSTGMG
00491 *                                                          *      ELUSTGMG
00492 ************************************************************      ELUSTGMG
00493  SELECT-STANDARD-LENGTH.                                          ELUSTGMG
00494      MOVE SMA-LEN (SMA-STG-MGT-IDX) TO CIA-TSQ-TS-LEN.            ELUSTGMG
00495                                                                   ELUSTGMG
00496                                                                   ELUSTGMG
00497 ************************************************************      ELUSTGMG
00498 *                                                          *      ELUSTGMG
00499 *        SELECT USER DEFINED LENGTH                        *      ELUSTGMG
00500 *                                                          *      ELUSTGMG
00501 ************************************************************      ELUSTGMG
00502  SELECT-USER-DEFINED-LENGTH.                                      ELUSTGMG
00503      MOVE CIA-AREA-LEN TO CIA-TSQ-TS-LEN.                         ELUSTGMG
00504      EJECT                                                        ELUSTGMG
00505                                                                   ELUSTGMG
00506                                                                   ELUSTGMG
00507 ************************************************************      ELUSTGMG
00508 *                                                          *      ELUSTGMG
00509 *        PROCESS STORAGE REQUEST                           *      ELUSTGMG
00510 *                                                          *      ELUSTGMG
00511 ************************************************************      ELUSTGMG
00512  PROCESS-STORAGE-REQUEST.                                         ELUSTGMG
00513      IF CIA-STG-FREEMAIN                                          ELUSTGMG
00514          PERFORM FREE-CONTROLLED-STORAGE                          ELUSTGMG
00515      ELSE IF CIA-STG-GETMAIN                                      ELUSTGMG
00516          PERFORM OBTAIN-CONTROLLED-STORAGE                        ELUSTGMG
00517      ELSE                                                         ELUSTGMG
00518          PERFORM SIGNAL-INVALID-STG-MGR-REQUEST.                  ELUSTGMG
00519                                                                   ELUSTGMG
00520                                                                   ELUSTGMG
00521 ************************************************************      ELUSTGMG
00522 *                                                          *      ELUSTGMG
00523 *        FREE CONTROLLED STORAGE                           *      ELUSTGMG
00524 *                                                          *      ELUSTGMG
00525 ************************************************************      ELUSTGMG
00526  FREE-CONTROLLED-STORAGE.                                         ELUSTGMG
00527      IF SMA-PTR (SMA-STG-MGT-IDX) IS NOT EQUAL TO NULL            ELUSTGMG
00528          PERFORM FREE-STORAGE                                     ELUSTGMG
00529      ELSE                                                         ELUSTGMG
00530          PERFORM SET-STORAGE-ALREADY-FREED.                       ELUSTGMG
00531      EJECT                                                        ELUSTGMG
00532                                                                   ELUSTGMG
00533                                                                   ELUSTGMG
00534 ************************************************************      ELUSTGMG
00535 *                                                          *      ELUSTGMG
00536 *        FREE STORAGE                                      *      ELUSTGMG
00537 *                                                          *      ELUSTGMG
00538 ************************************************************      ELUSTGMG
00539  FREE-STORAGE.                                                    ELUSTGMG
00540      IF    SMA-TYP-IO (SMA-STG-MGT-IDX)                           ELUSTGMG
00541         OR SMA-TYP-TEMPSTG (SMA-STG-MGT-IDX)                      ELUSTGMG
00542          PERFORM FREE-STORAGE-FOR-FILE                            ELUSTGMG
00543      ELSE IF SMA-TYP-DATA (SMA-STG-MGT-IDX)                       ELUSTGMG
00544          PERFORM FREE-STORAGE-FOR-DATA                            ELUSTGMG
00545      ELSE                                                         ELUSTGMG
00546          PERFORM SIGNAL-INVALID-STG-MGR-REQUEST.                  ELUSTGMG
00547      EJECT                                                        ELUSTGMG
00548                                                                   ELUSTGMG
00549                                                                   ELUSTGMG
00550 ************************************************************      ELUSTGMG
00551 *                                                          *      ELUSTGMG
00552 *        FREE STORAGE FOR FILE                             *      ELUSTGMG
00553 *                                                          *      ELUSTGMG
00554 ************************************************************      ELUSTGMG
00555  FREE-STORAGE-FOR-FILE.                                           ELUSTGMG
00556      PERFORM ESTABLISH-ADDRESSING-TO-PARAME.                      ELUSTGMG
00557      IF     (   IOP-FREEMAIN-REC                                  ELUSTGMG
00558              OR IOP-FREEMAIN-BOTH-REC                             ELUSTGMG
00559              OR IOP-FREEMAIN-ALL      )                           ELUSTGMG
00560         AND IOP-REC-PTR NOT = NULL                                ELUSTGMG
00561          PERFORM FREE-STORAGE-FOR-RECORD-AREA.                    ELUSTGMG
00562      IF     (   IOP-FREEMAIN-DUP-REC                              ELUSTGMG
00563              OR IOP-FREEMAIN-BOTH-REC                             ELUSTGMG
00564              OR IOP-FREEMAIN-ALL      )                           ELUSTGMG
00565         AND IOP-DUP-REC-PTR NOT = NULL                            ELUSTGMG
00566          PERFORM FREE-STORAGE-FOR-DUP-RECORD-AR.                  ELUSTGMG
00567      IF IOP-FREEMAIN-ALL                                          ELUSTGMG
00568          PERFORM FREE-STORAGE-FOR-PARAMETER-BLO.                  ELUSTGMG
00569                                                                   ELUSTGMG
00570                                                                   ELUSTGMG
00571 ************************************************************      ELUSTGMG
00572 *                                                          *      ELUSTGMG
00573 *        FREE STORAGE FOR RECORD AREA                      *      ELUSTGMG
00574 *                                                          *      ELUSTGMG
00575 ************************************************************      ELUSTGMG
00576  FREE-STORAGE-FOR-RECORD-AREA.                                    ELUSTGMG
00577      SET ADDRESS OF LS-AREA-TO-FREE TO IOP-REC-PTR.               ELUSTGMG
00578      EXEC CICS FREEMAIN                                           ELUSTGMG
00579                DATA (LS-AREA-TO-FREE)                             ELUSTGMG
00580                END-EXEC.                                          ELUSTGMG
00581      SET IOP-REC-PTR TO NULL.                                     ELUSTGMG
00582 *    DISPLAY ' FROM FREE-STORAGE-FOR-RECORD-AREA PARA'.           ELUSTGMG
00583 *    DISPLAY 'AREA TO FREE ' LS-AREA-TO-FREE.                     ELUSTGMG
00584 *    DISPLAY 'IOP REC PTR  ' IOP-REC-PTR.                         ELUSTGMG
00585 *    DISPLAY 'IOP REC DDN  ' IOP-FILE-DDNAME.                     ELUSTGMG
00586      EJECT                                                        ELUSTGMG
00587                                                                   ELUSTGMG
00588                                                                   ELUSTGMG
00589 ************************************************************      ELUSTGMG
00590 *                                                          *      ELUSTGMG
00591 *        FREE STORAGE FOR DUP RECORD AREA                  *      ELUSTGMG
00592 *                                                          *      ELUSTGMG
00593 ************************************************************      ELUSTGMG
00594  FREE-STORAGE-FOR-DUP-RECORD-AR.                                  ELUSTGMG
00595      SET ADDRESS OF LS-AREA-TO-FREE TO IOP-DUP-REC-PTR.           ELUSTGMG
00596      EXEC CICS FREEMAIN                                           ELUSTGMG
00597                DATA (LS-AREA-TO-FREE)                             ELUSTGMG
00598                END-EXEC.                                          ELUSTGMG
00599      SET IOP-DUP-REC-PTR TO NULL.                                 ELUSTGMG
00600 *    DISPLAY ' FROM FREE-STORAGE-FOR-RECORD-AREA PARA'.           ELUSTGMG
00601 *    DISPLAY 'AREA TO FREE ' LS-AREA-TO-FREE.                     ELUSTGMG
00602 *    DISPLAY 'IOP DUP REC PTR  ' IOP-DUP-REC-PTR.                 ELUSTGMG
00603 *    DISPLAY 'IOP REC DDN  ' IOP-FILE-DDNAME.                     ELUSTGMG
00604      EJECT                                                        ELUSTGMG
00605                                                                   ELUSTGMG
00606                                                                   ELUSTGMG
00607 ************************************************************      ELUSTGMG
00608 *                                                          *      ELUSTGMG
00609 *        FREE STORAGE FOR PARAMETER BLOCK                  *      ELUSTGMG
00610 *                                                          *      ELUSTGMG
00611 ************************************************************      ELUSTGMG
00612  FREE-STORAGE-FOR-PARAMETER-BLO.                                  ELUSTGMG
00613      SET ADDRESS OF LS-AREA-TO-FREE TO SMA-PTR                    ELUSTGMG
00614          (SMA-STG-MGT-IDX).                                       ELUSTGMG
00615      EXEC CICS FREEMAIN                                           ELUSTGMG
00616                DATA (LS-AREA-TO-FREE)                             ELUSTGMG
00617                END-EXEC.                                          ELUSTGMG
00618 *    DISPLAY ' FROM FREE-STORAGE-FOR-RECORD-AREA PARA'.           ELUSTGMG
00619 *    DISPLAY 'AREA TO FREE ' LS-AREA-TO-FREE.                     ELUSTGMG
00620 *    DISPLAY 'SMA POINTER    ' SMA-PTR (SMA-STG-MGT-IDX).         ELUSTGMG
00621 *    DISPLAY 'SMA DDN      ' SMA-DDN (SMA-STG-MGT-IDX).           ELUSTGMG
00622      SET SMA-PTR (SMA-STG-MGT-IDX) TO NULL.                       ELUSTGMG
00623      SET SMA-ELSIOPM-PTR TO NULL.                                 ELUSTGMG
00624                                                                   ELUSTGMG
00625                                                                   ELUSTGMG
00626 ************************************************************      ELUSTGMG
00627 *                                                          *      ELUSTGMG
00628 *        FREE STORAGE FOR DATA                             *      ELUSTGMG
00629 *                                                          *      ELUSTGMG
00630 ************************************************************      ELUSTGMG
00631  FREE-STORAGE-FOR-DATA.                                           ELUSTGMG
00632      SET ADDRESS OF LS-AREA-TO-FREE TO SMA-PTR                    ELUSTGMG
00633          (SMA-STG-MGT-IDX).                                       ELUSTGMG
00634      EXEC CICS FREEMAIN                                           ELUSTGMG
00635                DATA (LS-AREA-TO-FREE)                             ELUSTGMG
00636                END-EXEC.                                          ELUSTGMG
00637      SET SMA-PTR (SMA-STG-MGT-IDX) TO NULL.                       ELUSTGMG
00638 *    DISPLAY ' FROM FREE-STORAGE-FOR-RECORD-AREA PARA'.           ELUSTGMG
00639 *    DISPLAY 'AREA TO FREE ' LS-AREA-TO-FREE.                     ELUSTGMG
00640 *    DISPLAY 'SMA POINTER    ' SMA-PTR (SMA-STG-MGT-IDX).         ELUSTGMG
00641 *    DISPLAY 'SMA DDN      ' SMA-DDN (SMA-STG-MGT-IDX).           ELUSTGMG
00642                                                                   ELUSTGMG
00643                                                                   ELUSTGMG
00644 ************************************************************      ELUSTGMG
00645 *                                                          *      ELUSTGMG
00646 *        SET STORAGE ALREADY FREED                         *      ELUSTGMG
00647 *                                                          *      ELUSTGMG
00648 ************************************************************      ELUSTGMG
00649  SET-STORAGE-ALREADY-FREED.                                       ELUSTGMG
00650      SET CIA-RC-STG-DUP-REL TO TRUE.                              ELUSTGMG
00651      EJECT                                                        ELUSTGMG
00652                                                                   ELUSTGMG
00653                                                                   ELUSTGMG
00654 ************************************************************      ELUSTGMG
00655 *                                                          *      ELUSTGMG
00656 *        OBTAIN CONTROLLED STORAGE                         *      ELUSTGMG
00657 *                                                          *      ELUSTGMG
00658 ************************************************************      ELUSTGMG
00659  OBTAIN-CONTROLLED-STORAGE.                                       ELUSTGMG
00660      IF    SMA-TYP-IO (SMA-STG-MGT-IDX)                           ELUSTGMG
00661         OR SMA-TYP-TEMPSTG (SMA-STG-MGT-IDX)                      ELUSTGMG
00662          PERFORM OBTAIN-STORAGE-FOR-FILE                          ELUSTGMG
00663      ELSE IF SMA-TYP-DATA (SMA-STG-MGT-IDX)                       ELUSTGMG
00664          PERFORM OBTAIN-STORAGE-FOR-DATA                          ELUSTGMG
00665      ELSE                                                         ELUSTGMG
00666          PERFORM SIGNAL-INVALID-STG-MGR-REQUEST.                  ELUSTGMG
00667                                                                   ELUSTGMG
00668                                                                   ELUSTGMG
00669 ************************************************************      ELUSTGMG
00670 *                                                          *      ELUSTGMG
00671 *        OBTAIN STORAGE FOR FILE                           *      ELUSTGMG
00672 *                                                          *      ELUSTGMG
00673 ************************************************************      ELUSTGMG
00674  OBTAIN-STORAGE-FOR-FILE.                                         ELUSTGMG
00675      IF SMA-PTR (SMA-STG-MGT-IDX) = NULL                          ELUSTGMG
00676          PERFORM OBTAIN-STORAGE-FOR-PARAMETER-B                   ELUSTGMG
00677      ELSE                                                         ELUSTGMG
00678          PERFORM OBTAIN-STORAGE-FOR-RECORD.                       ELUSTGMG
00679      EJECT                                                        ELUSTGMG
00680                                                                   ELUSTGMG
00681                                                                   ELUSTGMG
00682 ************************************************************      ELUSTGMG
00683 *                                                          *      ELUSTGMG
00684 *        OBTAIN STORAGE FOR PARAMETER BLOCK                *      ELUSTGMG
00685 *                                                          *      ELUSTGMG
00686 ************************************************************      ELUSTGMG
00687  OBTAIN-STORAGE-FOR-PARAMETER-B.                                  ELUSTGMG
00688      EXEC CICS GETMAIN                                            ELUSTGMG
00689                SET    (SMA-PTR (SMA-STG-MGT-IDX) )                ELUSTGMG
00690                LENGTH (SMA-ELSIOPM-LEN)                           ELUSTGMG
00691                END-EXEC.                                          ELUSTGMG
00692 *    DISPLAY 'INFO FROM OBTAIN-STORAGE-FOR-PARAMETER-B PARA '.    ELUSTGMG
00693 *    DISPLAY '  ' .                                               ELUSTGMG
00694 *    DISPLAY 'DDNAME : '  SMA-DDN (SMA-STG-MGT-IDX).              ELUSTGMG
00695 *    DISPLAY 'ABOVE LINE? ' SMA-TYP (SMA-STG-MGT-IDX).            ELUSTGMG
00696 *    DISPLAY ' PTR: ' SMA-PTR (SMA-STG-MGT-IDX).                  ELUSTGMG
00697 *    DISPLAY ' LEN: ' SMA-LEN (SMA-STG-MGT-IDX).                  ELUSTGMG
00698      PERFORM ESTABLISH-ADDRESSING-TO-PARAME.                      ELUSTGMG
00699      PERFORM INITIALIZE-PARAMETER-BLOCK.                          ELUSTGMG
00700                                                                   ELUSTGMG
00701                                                                   ELUSTGMG
00702 ************************************************************      ELUSTGMG
00703 *                                                          *      ELUSTGMG
00704 *        INITIALIZE PARAMETER BLOCK                        *      ELUSTGMG
00705 *                                                          *      ELUSTGMG
00706 ************************************************************      ELUSTGMG
00707  INITIALIZE-PARAMETER-BLOCK.                                      ELUSTGMG
00708      INITIALIZE IOP-INPUT-OUTPUT-PARAMETERS.                      ELUSTGMG
00709      MOVE SMA-DDN (SMA-STG-MGT-IDX) TO IOP-FILE-DDNAME.           ELUSTGMG
00710      MOVE SMA-LEN (SMA-STG-MGT-IDX) TO IOP-MAX-REC-LEN.           ELUSTGMG
00711      SET IOP-STG-MODE-LOCATE TO TRUE.                             ELUSTGMG
00712      SET IOP-REC-PTR, IOP-DUP-REC-PTR, IOP-FC-REC-PTR TO          ELUSTGMG
00713          NULL.                                                    ELUSTGMG
00714      IF SMA-TYP-TEMPSTG (SMA-STG-MGT-IDX)                         ELUSTGMG
00715          PERFORM MODIFY-FILENAME-TO-TS-QUEUE-NA.                  ELUSTGMG
00716                                                                   ELUSTGMG
00717                                                                   ELUSTGMG
00718 ************************************************************      ELUSTGMG
00719 *                                                          *      ELUSTGMG
00720 *        MODIFY FILENAME TO TS QUEUE NAME                  *      ELUSTGMG
00721 *                                                          *      ELUSTGMG
00722 ************************************************************      ELUSTGMG
00723  MODIFY-FILENAME-TO-TS-QUEUE-NA.                                  ELUSTGMG
00724 * TS QUEUE NAME IS TTTTSSSS                                       ELUSTGMG
00725 * WHERE 'TTTT' IS TERMINAL ID                                     ELUSTGMG
00726 *   AND 'SSSS' IS LAST FOUR CHARACTERS OF CIA DDNAME              ELUSTGMG
00727      MOVE EIBTRMID TO IOP-TSQ-TERMID.                             ELUSTGMG
00728      EJECT                                                        ELUSTGMG
00729                                                                   ELUSTGMG
00730                                                                   ELUSTGMG
00731 ************************************************************      ELUSTGMG
00732 *                                                          *      ELUSTGMG
00733 *        OBTAIN STORAGE FOR RECORD                         *      ELUSTGMG
00734 *                                                          *      ELUSTGMG
00735 ************************************************************      ELUSTGMG
00736  OBTAIN-STORAGE-FOR-RECORD.                                       ELUSTGMG
00737      PERFORM ESTABLISH-ADDRESSING-TO-PARAME.                      ELUSTGMG
00738      IF IOP-MAX-REC-LEN > 0                                       ELUSTGMG
00739          PERFORM SET-FIXED-RECORD-LENGTH-TO-ALL.                  ELUSTGMG
00740      IF     IOP-REC-PTR = NULL                                    ELUSTGMG
00741         AND IOP-GETMAIN-REC                                       ELUSTGMG
00742          PERFORM OBTAIN-STORAGE-FOR-RECORD-AREA                   ELUSTGMG
00743      ELSE IF     IOP-DUP-REC-PTR = NULL                           ELUSTGMG
00744         AND IOP-GETMAIN-DUP-REC                                   ELUSTGMG
00745          PERFORM OBTAIN-STORAGE-FOR-DUP-RECORDX                   ELUSTGMG
00746      ELSE                                                         ELUSTGMG
00747          PERFORM SET-STORAGE-ALREADY-OBTAINED.                    ELUSTGMG
00748                                                                   ELUSTGMG
00749                                                                   ELUSTGMG
00750 ************************************************************      ELUSTGMG
00751 *                                                          *      ELUSTGMG
00752 *        SET FIXED RECORD LENGTH TO ALLOCATE               *      ELUSTGMG
00753 *                                                          *      ELUSTGMG
00754 ************************************************************      ELUSTGMG
00755  SET-FIXED-RECORD-LENGTH-TO-ALL.                                  ELUSTGMG
00756      MOVE IOP-MAX-REC-LEN TO IOP-REC-LEN,                         ELUSTGMG
00757          IOP-DUP-REC-LEN.                                         ELUSTGMG
00758      EJECT                                                        ELUSTGMG
00759                                                                   ELUSTGMG
00760                                                                   ELUSTGMG
00761 ************************************************************      ELUSTGMG
00762 *                                                          *      ELUSTGMG
00763 *        OBTAIN STORAGE FOR RECORD AREA                    *      ELUSTGMG
00764 *                                                          *      ELUSTGMG
00765 ************************************************************      ELUSTGMG
00766  OBTAIN-STORAGE-FOR-RECORD-AREA.                                  ELUSTGMG
00767      PERFORM CHECK-AMOUNT-OF-RECORD-STORAGE.                      ELUSTGMG
00768      EXEC CICS GETMAIN                                            ELUSTGMG
00769                SET     (IOP-REC-PTR)                              ELUSTGMG
00770                LENGTH  (IOP-REC-LEN)                              ELUSTGMG
00771                INITIMG (HEX-00)                                   ELUSTGMG
00772                END-EXEC.                                          ELUSTGMG
00773 *    DISPLAY 'INFO FROM OBTAIN-STORAGE-FOR-RECORD-AREA PARA '.    ELUSTGMG
00774 *    DISPLAY '  ' .                                               ELUSTGMG
00775 *    DISPLAY 'DDNAME : '  IOP-FILE-DDNAME.                        ELUSTGMG
00776 *    DISPLAY 'ABOVE LINE? ' SMA-TYP (SMA-STG-MGT-IDX).            ELUSTGMG
00777 *    DISPLAY ' PTR: ' IOP-REC-PTR.                                ELUSTGMG
00778 *    DISPLAY ' LEN: ' IOP-REC-LEN.                                ELUSTGMG
00779                                                                   ELUSTGMG
00780                                                                   ELUSTGMG
00781 ************************************************************      ELUSTGMG
00782 *                                                          *      ELUSTGMG
00783 *        CHECK AMOUNT OF RECORD STORAGE REQUESTED          *      ELUSTGMG
00784 *                                                          *      ELUSTGMG
00785 ************************************************************      ELUSTGMG
00786  CHECK-AMOUNT-OF-RECORD-STORAGE.                                  ELUSTGMG
00787      IF    IOP-REC-LEN > 32752                                    ELUSTGMG
00788         OR IOP-REC-LEN < 1                                        ELUSTGMG
00789          PERFORM SIGNAL-INVALID-STG-MGR-REQUEST.                  ELUSTGMG
00790      EJECT                                                        ELUSTGMG
00791                                                                   ELUSTGMG
00792                                                                   ELUSTGMG
00793 ************************************************************      ELUSTGMG
00794 *                                                          *      ELUSTGMG
00795 *        OBTAIN STORAGE FOR DUP RECORD AREA                *      ELUSTGMG
00796 *                                                          *      ELUSTGMG
00797 ************************************************************      ELUSTGMG
00798  OBTAIN-STORAGE-FOR-DUP-RECORDX.                                  ELUSTGMG
00799      PERFORM CHECK-AMOUNT-OF-DUP-RECORD-STO.                      ELUSTGMG
00800      EXEC CICS GETMAIN                                            ELUSTGMG
00801                SET     (IOP-DUP-REC-PTR)                          ELUSTGMG
00802                LENGTH  (IOP-DUP-REC-LEN)                          ELUSTGMG
00803                INITIMG (HEX-00)                                   ELUSTGMG
00804                END-EXEC.                                          ELUSTGMG
00805 *    DISPLAY 'INFO FROM OBTAIN-STORAGE-FOR-DUP-RECORDX PARA '.    ELUSTGMG
00806 *    DISPLAY '  '.                                                ELUSTGMG
00807 *    DISPLAY 'DDNAME : '  IOP-FILE-DDNAME.                        ELUSTGMG
00808 *    DISPLAY 'ABOVE LINE? ' SMA-TYP (SMA-STG-MGT-IDX).            ELUSTGMG
00809 *    DISPLAY ' PTR: ' IOP-DUP-REC-PTR.                            ELUSTGMG
00810 *    DISPLAY ' LEN: ' IOP-DUP-REC-LEN.                            ELUSTGMG
00811 *    DISPLAY ' IO FUNCTION ' IOP-FUNCTION.                        ELUSTGMG
00812                                                                   ELUSTGMG
00813                                                                   ELUSTGMG
00814 ************************************************************      ELUSTGMG
00815 *                                                          *      ELUSTGMG
00816 *        CHECK AMOUNT OF DUP RECORD STORAGE REQUESTED      *      ELUSTGMG
00817 *                                                          *      ELUSTGMG
00818 ************************************************************      ELUSTGMG
00819  CHECK-AMOUNT-OF-DUP-RECORD-STO.                                  ELUSTGMG
00820      IF    IOP-DUP-REC-LEN > 32752                                ELUSTGMG
00821         OR IOP-DUP-REC-LEN < 0                                    ELUSTGMG
00822          PERFORM SIGNAL-INVALID-STG-MGR-REQUEST.                  ELUSTGMG
00823                                                                   ELUSTGMG
00824                                                                   ELUSTGMG
00825 ************************************************************      ELUSTGMG
00826 *                                                          *      ELUSTGMG
00827 *        OBTAIN STORAGE FOR DATA                           *      ELUSTGMG
00828 *                                                          *      ELUSTGMG
00829 ************************************************************      ELUSTGMG
00830  OBTAIN-STORAGE-FOR-DATA.                                         ELUSTGMG
00831      IF SMA-LEN (SMA-STG-MGT-IDX) > 0                             ELUSTGMG
00832          PERFORM OBTAIN-FIXED-STORAGE-LENGTH.                     ELUSTGMG
00833      PERFORM OBTAIN-PROGRAM-DEFINED-STORAGE.                      ELUSTGMG
00834      EJECT                                                        ELUSTGMG
00835                                                                   ELUSTGMG
00836                                                                   ELUSTGMG
00837 ************************************************************      ELUSTGMG
00838 *                                                          *      ELUSTGMG
00839 *        OBTAIN FIXED STORAGE LENGTH                       *      ELUSTGMG
00840 *                                                          *      ELUSTGMG
00841 ************************************************************      ELUSTGMG
00842  OBTAIN-FIXED-STORAGE-LENGTH.                                     ELUSTGMG
00843      MOVE SMA-LEN (SMA-STG-MGT-IDX) TO CIA-AREA-LEN,              ELUSTGMG
00844          CIA-AREA-LEN-16M.                                        ELUSTGMG
00845      EJECT                                                        ELUSTGMG
00846                                                                   ELUSTGMG
00847                                                                   ELUSTGMG
00848 ************************************************************      ELUSTGMG
00849 *                                                          *      ELUSTGMG
00850 *        OBTAIN PROGRAM DEFINED STORAGE LENGTH             *      ELUSTGMG
00851 *                                                          *      ELUSTGMG
00852 ************************************************************      ELUSTGMG
00853  OBTAIN-PROGRAM-DEFINED-STORAGE.                                  ELUSTGMG
00854      IF SMA-TYP-DATA-BELOW-16M (SMA-STG-MGT-IDX)                  ELUSTGMG
00855          PERFORM OBTAIN-STORAGE-BELOW-THE-16M-L                   ELUSTGMG
00856      ELSE                                                         ELUSTGMG
00857          PERFORM OBTAIN-STORAGE-ABOVE-THE-16M-L.                  ELUSTGMG
00858                                                                   ELUSTGMG
00859                                                                   ELUSTGMG
00860 ************************************************************      ELUSTGMG
00861 *                                                          *      ELUSTGMG
00862 *        OBTAIN STORAGE BELOW THE 16M LINE                 *      ELUSTGMG
00863 *                                                          *      ELUSTGMG
00864 ************************************************************      ELUSTGMG
00865  OBTAIN-STORAGE-BELOW-THE-16M-L.                                  ELUSTGMG
00866      MOVE CIA-AREA-LEN TO CIA-AREA-LEN-16M.                       ELUSTGMG
00867      PERFORM CHECK-AMOUNT-OF-BELOW-16M-STOR.                      ELUSTGMG
00868      EXEC CICS GETMAIN                                            ELUSTGMG
00869                INITIMG (HEX-00)                                   ELUSTGMG
00870                LENGTH  (CIA-AREA-LEN-16M          )               ELUSTGMG
00871                SET     (SMA-PTR (SMA-STG-MGT-IDX) )               ELUSTGMG
00872                END-EXEC.                                          ELUSTGMG
00873 *    DISPLAY 'INFO FROM OBTAIN-STORAGE-BELOW-THE-16M-L PARA'.     ELUSTGMG
00874 *    DISPLAY '  ' .                                               ELUSTGMG
00875 *    DISPLAY 'DDNAME : '  SMA-DDN (SMA-STG-MGT-IDX).              ELUSTGMG
00876 *    DISPLAY 'ABOVE LINE? ' SMA-TYP (SMA-STG-MGT-IDX).            ELUSTGMG
00877 *    DISPLAY ' PTR: ' SMA-PTR (SMA-STG-MGT-IDX).                  ELUSTGMG
00878 *    DISPLAY ' LEN: ' CIA-AREA-LEN-16M.                           ELUSTGMG
00879                                                                   ELUSTGMG
00880                                                                   ELUSTGMG
00881 ************************************************************      ELUSTGMG
00882 *                                                          *      ELUSTGMG
00883 *        CHECK AMOUNT OF BELOW 16M STORAGE REQUESTED       *      ELUSTGMG
00884 *                                                          *      ELUSTGMG
00885 ************************************************************      ELUSTGMG
00886  CHECK-AMOUNT-OF-BELOW-16M-STOR.                                  ELUSTGMG
00887      IF    CIA-AREA-LEN > 32767                                   ELUSTGMG
00888         OR CIA-AREA-LEN < 0                                       ELUSTGMG
00889          PERFORM SIGNAL-INVALID-STG-MGR-REQUEST.                  ELUSTGMG
00890      EJECT                                                        ELUSTGMG
00891                                                                   ELUSTGMG
00892                                                                   ELUSTGMG
00893 ************************************************************      ELUSTGMG
00894 *                                                          *      ELUSTGMG
00895 *        OBTAIN STORAGE ABOVE THE 16M LINE                 *      ELUSTGMG
00896 *                                                          *      ELUSTGMG
00897 ************************************************************      ELUSTGMG
00898  OBTAIN-STORAGE-ABOVE-THE-16M-L.                                  ELUSTGMG
00899      MOVE CIA-AREA-LEN TO CIA-AREA-LEN-16M.                       ELUSTGMG
00900      PERFORM CHECK-AMOUNT-OF-ABOVE-16M-STOR.                      ELUSTGMG
00901      EXEC CICS GETMAIN                                            ELUSTGMG
00902                INITIMG (HEX-00)                                   ELUSTGMG
00903                FLENGTH (CIA-AREA-LEN              )               ELUSTGMG
00904                SET     (SMA-PTR (SMA-STG-MGT-IDX) )               ELUSTGMG
00905                END-EXEC.                                          ELUSTGMG
00906 *    DISPLAY 'INFO FROM OBTAIN-STORAGE-ABOVE-THE-16M-L PARA'.     ELUSTGMG
00907 *    DISPLAY '  ' .                                               ELUSTGMG
00908 *    DISPLAY 'DDNAME : '  SMA-DDN (SMA-STG-MGT-IDX).              ELUSTGMG
00909 *    DISPLAY 'ABOVE LINE? ' SMA-TYP (SMA-STG-MGT-IDX).            ELUSTGMG
00910 *    DISPLAY ' PTR: ' SMA-PTR (SMA-STG-MGT-IDX).                  ELUSTGMG
00911 *    DISPLAY ' LEN: ' CIA-AREA-LEN.                               ELUSTGMG
00912                                                                   ELUSTGMG
00913                                                                   ELUSTGMG
00914 ************************************************************      ELUSTGMG
00915 *                                                          *      ELUSTGMG
00916 *        CHECK AMOUNT OF ABOVE 16M STORAGE REQUESTED       *      ELUSTGMG
00917 *                                                          *      ELUSTGMG
00918 ************************************************************      ELUSTGMG
00919  CHECK-AMOUNT-OF-ABOVE-16M-STOR.                                  ELUSTGMG
00920      IF    CIA-AREA-LEN > 65504                                   ELUSTGMG
00921         OR CIA-AREA-LEN < 1                                       ELUSTGMG
00922          PERFORM SIGNAL-INVALID-STG-MGR-REQUEST.                  ELUSTGMG
00923                                                                   ELUSTGMG
00924                                                                   ELUSTGMG
00925 ************************************************************      ELUSTGMG
00926 *                                                          *      ELUSTGMG
00927 *        SET STORAGE ALREADY OBTAINED                      *      ELUSTGMG
00928 *                                                          *      ELUSTGMG
00929 ************************************************************      ELUSTGMG
00930  SET-STORAGE-ALREADY-OBTAINED.                                    ELUSTGMG
00931      SET CIA-RC-STG-DUP-REQ TO TRUE.                              ELUSTGMG
00932                                                                   ELUSTGMG
00933                                                                   ELUSTGMG
00934 ************************************************************      ELUSTGMG
00935 *                                                          *      ELUSTGMG
00936 *        ESTABLISH ADDRESSING TO PARAMETER BLOCK           *      ELUSTGMG
00937 *                                                          *      ELUSTGMG
00938 ************************************************************      ELUSTGMG
00939  ESTABLISH-ADDRESSING-TO-PARAME.                                  ELUSTGMG
00940      SET ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS TO                ELUSTGMG
00941          SMA-PTR(SMA-STG-MGT-IDX).                                ELUSTGMG
00942      SET SMA-ELSIOPM-PTR TO ADDRESS OF                            ELUSTGMG
00943          IOP-INPUT-OUTPUT-PARAMETERS.                             ELUSTGMG
00944                                                                   ELUSTGMG
00945                                                                   ELUSTGMG
00946 ************************************************************      ELUSTGMG
00947 *                                                          *      ELUSTGMG
00948 *        CHECK COMMAREA LENGTH                             *      ELUSTGMG
00949 *                                                          *      ELUSTGMG
00950 ************************************************************      ELUSTGMG
00951  CHECK-COMMAREA-LENGTH.                                           ELUSTGMG
00952      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELUSTGMG
00953          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELUSTGMG
00954      EJECT                                                        ELUSTGMG
00955                                                                   ELUSTGMG
00956                                                                   ELUSTGMG
00957 ************************************************************      ELUSTGMG
00958 *                                                          *      ELUSTGMG
00959 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELUSTGMG
00960 *                                                          *      ELUSTGMG
00961 ************************************************************      ELUSTGMG
00962  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELUSTGMG
00963      SET CIA-AB-DFHCOMMAREA TO TRUE.                              ELUSTGMG
00964      EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.                ELUSTGMG
00965      EJECT                                                        ELUSTGMG
00966                                                                   ELUSTGMG
00967                                                                   ELUSTGMG
00968 ************************************************************      ELUSTGMG
00969 *                                                          *      ELUSTGMG
00970 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELUSTGMG
00971 *                                                          *      ELUSTGMG
00972 ************************************************************      ELUSTGMG
00973  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELUSTGMG
00974      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUSTGMG
00975          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELUSTGMG
00976      EJECT                                                        ELUSTGMG
00977                                                                   ELUSTGMG
00978                                                                   ELUSTGMG
00979 ************************************************************      ELUSTGMG
00980 *                                                          *      ELUSTGMG
00981 *        ESTABLISH ADDRESSING TO STG MGR AREA              *      ELUSTGMG
00982 *                                                          *      ELUSTGMG
00983 ************************************************************      ELUSTGMG
00984  ESTABLISH-ADDRESSING-TO-STG-MG.                                  ELUSTGMG
00985      MOVE CIA-DDNAME TO WS-CIA-DDNAME.                            ELUSTGMG
00986      MOVE CIA-AREA-LEN TO WS-AREA-LEN.                            ELUSTGMG
00987      SET CIA-ELSSMA-DDN TO TRUE.                                  ELUSTGMG
00988      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUSTGMG
00989          ADDRESS OF SMA-STORAGE-MANAGEMENT-AREA.                  ELUSTGMG
00990      MOVE WS-CIA-DDNAME TO CIA-DDNAME.                            ELUSTGMG
00991      MOVE WS-AREA-LEN TO CIA-AREA-LEN.                            ELUSTGMG
00992      EJECT                                                        ELUSTGMG
00993                                                                   ELUSTGMG
00994                                                                   ELUSTGMG
00995 ************************************************************      ELUSTGMG
00996 *                                                          *      ELUSTGMG
00997 *        SIGNAL INVALID STG MGR REQUEST                    *      ELUSTGMG
00998 *                                                          *      ELUSTGMG
00999 ************************************************************      ELUSTGMG
01000  SIGNAL-INVALID-STG-MGR-REQUEST.                                  ELUSTGMG
01001      SET CIA-AB-STG-INVREQ TO TRUE.                               ELUSTGMG
01002      EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.                ELUSTGMG
01003                                                                   ELUSTGMG
01004                                                                   ELUSTGMG
01005 ************************************************************      ELUSTGMG
01006 *                                                          *      ELUSTGMG
01007 *        SAVE CURRENT FUNCTION                             *      ELUSTGMG
01008 *                                                          *      ELUSTGMG
01009 ************************************************************      ELUSTGMG
01010  SAVE-CURRENT-FUNCTION.                                           ELUSTGMG
01011      MOVE CIA-STG-MGT-FCN TO CIA-STG-MGT-FCN-SAVE.                ELUSTGMG
01012                                                                   ELUSTGMG
01013                                                                   ELUSTGMG
01014 ************************************************************      ELUSTGMG
01015 *                                                          *      ELUSTGMG
01016 *        RESTORE CURRENT FUNCTION                          *      ELUSTGMG
01017 *                                                          *      ELUSTGMG
01018 ************************************************************      ELUSTGMG
01019  RESTORE-CURRENT-FUNCTION.                                        ELUSTGMG
01020      MOVE CIA-STG-MGT-FCN-SAVE TO CIA-STG-MGT-FCN.                ELUSTGMG
01021                                                                   ELUSTGMG
01022                                                                   ELUSTGMG
01023 ************************************************************      ELUSTGMG
01024 *                                                          *      ELUSTGMG
01025 *        TERMINATE MODULE                                  *      ELUSTGMG
01026 *                                                          *      ELUSTGMG
01027 ************************************************************      ELUSTGMG
01028  TERMINATE-MODULE.                                                ELUSTGMG
01029      GOBACK.                                                      ELUSTGMG
