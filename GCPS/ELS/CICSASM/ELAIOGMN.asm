         TITLE 'ELAIOGMN - ENGLISH LANGUAGE GENERALIZED GETMAIN MODULE' 00001   
ELAIOGMN DFHEIENT CODEREG=(3,4,5)                                       00002   
*********************************************************************** 00003   
*        M O D I F I C A T I O N S   L O G                              00004   
*********************************************************************** 00005   
*   MOD#    DATE     NAME        DESCRIPTION                            00006   
*   ------  -------- ----------- -------------------------------------- 00007   
*           11/28/84 B WOLOWIEC  INITIAL INSTALLATION OF HGAIOGMN       00008   
*                                                                       00009   
*           02/27/86 DAVE SECOR  TAILORED FOR ENGLISH LANGUAGE SUB      00010   
*                                SYSTEM                                 00011   
*********************************************************************** 00012   
*********************************************************************** 00013   
*                                                                       00014   
*   PROGRAM:      ELIOGMN                                               00015   
*   AUTHOR:       BOB WOLOWIEC                                          00016   
*   DATE WRITTEN: 11/15/84                                              00017   
*                                                                       00018   
*   PURPOSE:   PROVIDE I/O SUPPORT FOR THE ENGLISH LANGUAGE SUB SYSTEM  00019   
*                                                                       00020   
*                                                                       00021   
*   FUNCTIONS: WHEN LINKED TO                                           00022   
*              THIS MODULE WILL PROVIDE GETMAIN AREAS FOR               00023   
*              THE I/O PARM AND SELECTED RECORD AREA.                   00024   
*              (RECORD AREA INDICATED BY PARM IN CIA)                   00025   
*                                                                       00026   
*********************************************************************** 00027   
         EJECT                                                          00028   
*********************************************************************** 00029   
*        R E G I S T E R     U S A G E                                  00030   
*********************************************************************** 00031   
R0       EQU   0    LINKAGE/WORK                                        00032   
R1       EQU   1    LINKAGE/WORK                                        00033   
R2       EQU   2    WORK                                                00034   
R3       EQU   3    1ST BASE REGISTER                                   00035   
R4       EQU   4    2ND BASE REGISTER                                   00036   
R5       EQU   5    3RD BASE REGISTER                                   00037   
R6       EQU   6    COMMON INTERFACE AREA FROM CALLER                   00038   
R7       EQU   7    COMMON I/O PARM AREA FROM CALLER                    00039   
R8       EQU   8    RECORD ADDRESS POINTER                              00040   
R9       EQU   9    SECOND RECORD ADDR. POINTER FOR ADDS                00041   
R10      EQU   10   WORK                                                00042   
R11      EQU   11   EXEC INTERFACE POINTER                              00043   
R12      EQU   12   BAL REGISTER FOR SUBROUTINE PROCESSES               00044   
R13      EQU   13   DFHEISTG DYNAMIC STORAGE AREA                       00045   
R14      EQU   14   LINKAGE/WORK                                        00046   
R15      EQU   15   LINKAGE/WORK                                        00047   
         EJECT                                                          00048   
         COPY  ELADCIA                                                  00049   
         EJECT                                                          00050   
         COPY  ELADIOPM                                                 00051   
         EJECT                                                          00052   
*                                                                       00053   
*  DSECTS FOR SETTING ADDRESSABILITY FOR INSTRUCTIONS                   00054   
*                                                                       00055   
ELIOREC  DSECT                                                          00056   
ELIOREC2 DSECT                                                          00057   
         EJECT                                                          00058   
DFHEISTG DSECT                                                          00059   
ABCODE   DS    CL4                ABEND CODE                            00060   
GTMAINL  DS    H                                                        00061   
*                                                                       00062   
         EJECT                                                          00063   
*                                                                       00064   
ELAIOGMN CSECT                                                          00065   
         PROGDATE                                                       00066   
         SPACE                                                          00067   
         CLC   EIBCALEN,=H'4'      EXPECTED COMMAREA LENGTH             00068   
         BNE   INVREQ                                                   00069   
         SPACE                                                          00070   
         L     R6,DFHEICAP         GET THE COMMAREA ADDRESS             00071   
         L     R6,0(R6)            GET THE COMMON INTERFACE AREA        00072   
         USING ELADCIA,R6           PASSED FROM CALLER.                 00073   
         SPACE                                                          00074   
*  FIRST DO A GETMAIN FOR THE I/O PARM AREA                             00075   
         SPACE                                                          00076   
         LA    R10,ELIOPRML                                             00077   
         STH   R10,GTMAINL                                              00078   
         SPACE                                                          00079   
         EXEC CICS GETMAIN   SET(R7) LENGTH(GTMAINL) INITIMG(X'00')     00080   
         SPACE                                                          00081   
         USING ELADIOPM,R7     ADDRESSABILIBY TO I/O PARMS              00082   
         ST    R7,CIAIOINT     SAVE I/O PARM ADDRESS IN CALLER CIA      00083   
         SPACE                                                          00084   
*                                                                       00085   
*   SCAN FILE TABLE AND FIND MATCH AND SAVE                             00086   
*   MAXIMUM RECORD LENGTH IN IO PARM AREA..                             00087   
*                                                                       00088   
         LA    R10,ELRECLEN           POINT TO NAME/LENGTH TABLE        00089   
FILELOOP EQU   *                                                        00090   
         CLC   CIADDN,0(R10)                  CHECK DDNAME              00091   
         BE    MOVELEN                                                  00092   
         LA    R10,10(R10)                   BUMP UP TABLE POINTER      00093   
         CLC   0(4,R10),=4X'FF'              IF END OF TABLE            00094   
         BE    INVREQ                        GO TO INVALID REQUEST      00095   
         B     FILELOOP                                                 00096   
         SPACE                                                          00097   
MOVELEN  EQU   *                                                        00098   
         MVC   ELIOMXRL,8(R10)       MOVE MAX TRAILER LENGTH            00099   
         SPACE                                                          00100   
*     NOW LETS DO THE GETMAIN FOR THE RECORD USING MAX LENGTH           00101   
         SPACE 2                                                        00102   
         SPACE 2                                                        00103   
         EXEC CICS GETMAIN   SET(R9) LENGTH(ELIOMXRL) INITIMG(X'00')    00104   
         SPACE 2                                                        00105   
*     SET UP THE I/O PARM AREA WITH THESE FIELDS                        00106   
         ST    R9,ELIORECA         SAVE RECORD ADDRESS                  00107   
         MVC   ELIODDNM,CIADDN     MOVE DDNAME TO I/O PARM              00108   
         MVI   ELIOSTOR,C'M'       SET MOVE MODE SWITCH                 00109   
         SPACE 2                                                        00110   
* ##NOTE  MAX RECORD LENGTH WAS ALREADY SET.                            00111   
         SPACE 2                                                        00112   
*        INDICATE I/O PARM AREA AND REC AREA WERE OBTAINED              00113   
*        THRU ELAIOGMN.                                                 00114   
         OI   ELGTMIND,X'88'                                            00115   
         SPACE                                                          00116   
******************************************************************      00117   
*   ELGTMIND IS A AREA THAT HAS NO APPLICATION USE BUT MAY HAVE         00118   
*   HAVE FUTURE  USE AS A DEBUGGING AID.   ELGTMIND WILL TELL           00119   
*   US HOW THE GETMAIN AREA WAS OBTAINED FOR I/O PARMS AND RECAREA.     00120   
*   BITS ON INDICATE WHERE OBTAINED                                     00121   
*                                                                       00122   
*      BIT                                                              00123   
*       0   I/O PARM AREA GETMAIN BY ELAIOGMN                           00124   
*       4   RECORD AREA BY ELAIOGMN                                     00125   
*       6   RECORD AREA BY ELAIOPGM                                     00126   
*                                                                       00127   
         SPACE 2                                                        00128   
*        ************************                                       00129   
*        *  RETURN TO CALLER    *                                       00130   
*        ************************                                       00131   
RETURN   DS    0H                                                       00132   
         SPACE 2                                                        00133   
         EXEC  CICS RETURN                                              00134   
         EJECT                                                          00135   
*     *****************************************                         00136   
*     *  INVALID FUNCTION REQUESTED - ABEND   *                         00137   
*     *****************************************                         00138   
INVREQ   DS    0H                                                       00139   
         MVC   ELIOMSG,=CL16'INVALID REQUEST '  ERROR MSG RETURNED      00140   
INVREQ99 DS    0H                                                       00141   
         MVC   ABCODE,=C'INVR'         MOVE ABEND TYPE                  00142   
         EXEC  CICS ABEND                                              X00143   
               ABCODE(ABCODE)                                           00144   
         EJECT                                                          00145   
         DS    0F                                                       00146   
         COPY  ELADRLEN                                                 00147   
         EJECT                                                          00148   
         LTORG                                                          00149   
         END                                                            00150   
