00001  ID DIVISION.                                                     06/29/02
00002  PROGRAM-ID.    ELELCMML.                                         ELELCMML
00003 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    LV001
00004 *       CCCCCCC OOOOOOOO  BBBBBBBBB OOOOOOOO LLLL   2222222222  * ELELCMML
00005 *     CCC      OOO   OOO BBB   BBB OOO   OOO LLL     222  222   * ELELCMML
00006 *    CCC      OOO   OOO BBB   BBB OOO   OOO LLL      222  222   * ELELCMML
00007 *   CCC      OOO   OOO BBBBBBBBB OOO   OOO LLL       222  222   * ELELCMML
00008 *  CCC      OOO   OOO BBB   BBB OOO   OOO LLL        222  222   * ELELCMML
00009 * CCC      OOO   OOO BBB   BBB OOO   OOO LLL         222  222   * ELELCMML
00010 * CCCCCCCC OOOOOOOO BBBBBBBBB OOOOOOOOO LLLLLLLLL   2222222222  * ELELCMML
00011 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ELELCMML
00012  AUTHOR.        NINA CERVANTES.                                   ELELCMML
00013  DATE-WRITTEN.  09/85.                                            ELELCMML
00014  DATE-COMPILED.   /  /  .                                         ELELCMML
00015      SKIP3                                                        ELELCMML
00016 ***************************************************************** ELELCMML
00017 *            ELCMPGM - ENGLISH LANGUAGE PROTOTYPE DRIVER        * ELELCMML
00018 *                                                               * ELELCMML
00019 *    TRANSACTIONS 'ELCI' 'ELCM' 'ELCS' WILL INITIATE THIS MODULE* ELELCMML
00020 *    UPON USER ENTRY.  THE COMMAREA WILL BE INITIALIZED AND     * ELELCMML
00021 *    PROGRAM CONTROL WILL BE TRANSFERED TO ELCAPGM.             * ELELCMML
00022 ***************************************************************** ELELCMML
00023 /                                                                 ELELCMML
00024  ENVIRONMENT DIVISION.                                            ELELCMML
00025  DATA DIVISION.                                                   ELELCMML
00026  WORKING-STORAGE SECTION.                                         ELELCMML
00027  01  FILLER            PIC X(41)    VALUE                         ELELCMML
00028      '***ELCMPGM WORKING STORAGE BEGINS HERE***'.                 ELELCMML
00029  01  WS-TRANSID.                                                  ELELCMML
00030      03  FILLER            PIC XXX.                               ELELCMML
00031      03  TRANSID-4TH-POS   PIC X.                                 ELELCMML
00032  01  WS-MESSAGES.                                                 ELELCMML
00033      03  INQ-MSG           PIC X(30)       VALUE                  ELELCMML
00034                    '        CODES MANUAL INQUIRY'.                ELELCMML
00035      03  MNT-MSG           PIC X(32)       VALUE                  ELELCMML
00036                  '      CODES MANUAL MAINTENANCE'.                ELELCMML
00037      03  SUP-MSG           PIC X(40)       VALUE                  ELELCMML
00038            'CODES MANUAL SUPERVISORY MAINTENANCE'.                ELELCMML
00039  01  COMMON-AREA.                                                 ELELCMML
00040  COPY ELPCOMMC.                                                   ELELCMML
00041 /**************************************************************** ELELCMML
00042 *                                                               * ELELCMML
00043 *                PROCEDURE DIVISION                             * ELELCMML
00044 *                                                               * ELELCMML
00045 ***************************************************************** ELELCMML
00046  PROCEDURE DIVISION.                                              ELELCMML
00047                                                                   ELELCMML
00048      INITIALIZE         COMMON-AREA.                              ELELCMML
00049      MOVE LOW-VALUES TO CA-SELECTED-KEYS                          ELELCMML
00050                         CA-RECORD-SCROLL-KEYS                     ELELCMML
00051                         CA-ELEMENT-SCROLL-KEYS                    ELELCMML
00052                         CA-CODE-VALUE-SCROLL-KEYS                 ELELCMML
00053                         CA-MAPFROM-KEYS.                          ELELCMML
00054                                                                   ELELCMML
00055      MOVE EIBTRNID   TO WS-TRANSID.                               ELELCMML
00056      MOVE TRANSID-4TH-POS TO CA-USER-LEVEL.                       ELELCMML
00057                                                                   ELELCMML
00058      EVALUATE CA-USER-LEVEL                                       ELELCMML
00059         WHEN 'M'     MOVE MNT-MSG TO CA-TRANS-HDR,                ELELCMML
00060         WHEN 'S'     MOVE SUP-MSG TO CA-TRANS-HDR,                ELELCMML
00061         WHEN OTHER   MOVE INQ-MSG TO CA-TRANS-HDR,                ELELCMML
00062      END-EVALUATE.                                                ELELCMML
00063                                                                   ELELCMML
00064      EXEC CICS XCTL                                               ELELCMML
00065                PROGRAM('ELELCAML')                                ELELCMML
00066                COMMAREA(COMMON-AREA)                              ELELCMML
00067                LENGTH(500)                                        ELELCMML
00068           END-EXEC.                                               ELELCMML
00069      GOBACK.                                                      ELELCMML
