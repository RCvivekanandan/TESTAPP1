*********************************************************************** 00001   
* PROGRAM NAME  ===> ELKFLXOR                                         * 00002   
* PROGRAM TITLE ===> FUZZY LOGIC - EXCLUSIVE OR (XOR)                 * 00003   
* AUTHOR        ===> R. LUKETICH                                      * 00004   
* DATE WRITTEN  ===> 27-SEP-1989                                      * 00005   
* FUNCTION      ===> PERFORM A FUZZY LOGIC 'XOR' BETWEEN TWO OR MORE  * 00006   
*                    OPERANDS USING FUZZY TRUTH VALUES IN THE RANGE   * 00007   
*                    (-1..+1).                                        * 00008   
*                                                                     * 00009   
*                    THE EXCLUSIVE OR OF TWO VARIABLES 'A' AND 'B'    * 00010   
*                    CAN BE WRITTEN USING 'AND', 'OR' AND 'NOT' AS:   * 00011   
*                                                                     * 00012   
*                              (A OR B) AND NOT (A AND B)             * 00013   
*                                                                     * 00014   
*                    THE FUZZY 'XOR' CAN BE DETERMINED THE SAME WAY,  * 00015   
*                    USING MINIMUM, MAXIMUM AND MULTIPLICATION BY -1  * 00016   
*                    AS:                                              * 00017   
*                                                                     * 00018   
*                              MIN( MAX(A,B), -1 * MIN(A,B) )         * 00019   
*                                                                     * 00020   
*                                                                     * 00021   
*                    THIS PROCESS IS APPLIED FIRST TO THE FIRST TWO   * 00022   
*                    VALUES IN THE OPERAND LIST.  THEN IT IS APPLIED  * 00023   
*                    REPLEATEDLY TO THE RESULT OF THE PREVIOUS        * 00024   
*                    OPERATION AND THE NEXT SUCCESSIVE OPERAND.  THIS * 00025   
*                    CONTINUES UNTIL THE LIST OF OPERANDS IS          * 00026   
*                    EXHAUSTED AND THE RESULT PLACED IN THE RESULT    * 00027   
*                    FIELD.  NOTE THAT IF NO OPERANDS ARE SUBMITTED,  * 00028   
*                    THE RESULT IS SET TO ZERO (INDETERMINATE).  IF   * 00029   
*                    ONLY ONE OPERAND IS SUBMITTED, THE VALUE OF THAT * 00030   
*                    OPERAND IS RETURNED AS THE RESULT, I.E., THE     * 00031   
*                    RESULT IS THE SAME AS A SIMPLE MOVE OR           * 00032   
*                    ASSIGNMENT OF THE OPERAND TO THE RESULT.         * 00033   
*                                                                     * 00034   
* -------------------------- REGISTER USAGE ------------------------- * 00035   
* 00 -                               08 -                             * 00036   
* 01 - PARAMETER LIST BASE ADDRESS   09 -                             * 00037   
* 02 - RESULT ADDRESS                10 -                             * 00038   
* 03 - PARAMETER ADDRESS             11 -                             * 00039   
* 04 -                               12 - POINTER INTO PARAMETER LIST * 00040   
* 05 -                               13 - SAVE AREA ADDRESS           * 00041   
* 06 -                               14 - RETURN ADDRESS              * 00042   
* 07 -                               15 - ENTRY POINT ADDRESS         * 00043   
*                                                                     * 00044   
* F0 - CURRENT RESULT                F4 -                             * 00045   
* F2 -                               F8 -                             * 00046   
*                                                                     * 00047   
* ---------------------------- PARAMTERS ---------------------------- * 00048   
*                                                                     * 00049   
* THIS PROGRAM ASSUMES THAT REGISTER (1) POINTS TO A PARAMETER LIST   * 00050   
* THAT CONTAINS THE FOLLOWING:                                        * 00051   
*                                                                     * 00052   
*              A(RESULT)                                              * 00053   
*              A(OPERAND-1)                                           * 00054   
*              A(OPERAND-2)                                           * 00055   
*                   .                                                 * 00056   
*                   .                                                 * 00057   
*              A(OPERAND-N) (OR'D WITH X'80000000')                   * 00058   
*                                                                     * 00059   
* THE NUMBER OF OPERANDS IS (THEORETICALLY) LIMITED ONLY BY THE       * 00060   
* AMOUNT OF STORAGE AVAILABLE.  THE LAST ENTRY IN THE PARAMETER LIST  * 00061   
* IS IDENTIFIED BY THE FACT THAT THE HIGH ORDER BIT OF THE ADDRESS IS * 00062   
* SET TO '1'.  IBM COBOL LINKAGE CONVENTIONS SET THIS BIT CORRECTLY.  * 00063   
*                                                                     * 00064   
* ----------------------- MAINTENANCE HISTORY ----------------------- * 00065   
*                                                                     * 00066   
*    DATE     LVL  BY                    DESCRIPTION                  * 00067   
* ----------- ---- --- ---------------------------------------------- * 00068   
* 27-SEP-1989 0001 RJL ORIGINAL PROGRAM                               * 00069   
*                                                                     * 00070   
* 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX                    * 00071   
*                             ASM RECOMPILES                          * 00072   
*                                                                     * 00073   
* 02-DEC-2003 AKK       REGEN'D FOR CHANGES IN COPYBOOKS              * 00074   
*                                                                     * 00075   
*********************************************************************** 00076   
*                                                                       00077   
*                                                                       00078   
PARMLIST EQU   1                                                        00079   
RESULT   EQU   2                                                        00080   
NEXTPARM EQU   3                                                        00081   
PARMPTR  EQU   12                                                       00082   
RETADD   EQU   14                                                       00083   
BASEREG  EQU   15                                                       00084   
CURRESLT EQU   0                                                        00085   
NEXTOPND EQU   2                                                        00086   
WORKAND  EQU   4                                                        00087   
WORKOR   EQU   6                                                        00088   
ELKFLXOR CSECT                                                          00089   
         USING *,BASEREG                                                00090   
         SAVE  (14,12)                 SAVE CALLER'S REGISTERS          00091   
         LE    CURRESLT,=E'0E+00'      SET DEFAULT RESULT VALUE         00092   
         L     RESULT,0(PARMLIST)      GET RESULT ADDRESS               00093   
         LR    PARMPTR,PARMLIST        SET POINTER INTO PARAMETER LIST  00094   
         TM    0(PARMPTR),X'80'        CHECK FOR END OF PARAMETER LIST  00095   
         BNZ   STORESLT                STORE RESULT IF DONE             00096   
         LA    PARMPTR,4(PARMPTR)      POINT TO NEXT PARM LIST ENTRY    00097   
         L     NEXTPARM,0(PARMPTR)     GET ADDRESS OF NEXT LOGIC VALUE  00098   
         LE    CURRESLT,0(NEXTPARM)    SET INITIAL RESULT TO FIRST OP   00099   
         TM    0(PARMPTR),X'80'        CHECK FOR END OF PARAMETER LIST  00100   
         BNZ   STORESLT                STORE RESULT IF DONE             00101   
*                                                                       00102   
NEXTOP   LA    PARMPTR,4(PARMPTR)      POINT TO NEXT PARM LIST ENTRY    00103   
         L     NEXTPARM,0(PARMPTR)     GET ADDRESS OF NEXT LOGIC VALUE  00104   
         LE    NEXTOPND,0(NEXTPARM)    GET NEXT LOGIC VALUE             00105   
*                                                                       00106   
*  FUZZY OR OF OPERANDS                                                 00107   
*                                                                       00108   
ORAB     LER   WORKOR,CURRESLT                                          00109   
         CER   WORKOR,NEXTOPND                                          00110   
         BH    ANDAB                                                    00111   
         LER   WORKOR,NEXTOPND                                          00112   
*                                                                       00113   
*  FUZZY AND OF OPERANDS                                                00114   
*                                                                       00115   
ANDAB    LER   WORKAND,CURRESLT                                         00116   
         CER   WORKAND,NEXTOPND                                         00117   
         BL    NOTANDAB                                                 00118   
         LER   WORKAND,NEXTOPND                                         00119   
*                                                                       00120   
*  FUZZY NOT OF INTERMEDIATE AND RESULTS                                00121   
*                                                                       00122   
NOTANDAB LCER  WORKAND,WORKAND                                          00123   
*                                                                       00124   
*  FUZZY AND OF INTERMEDIATE RESULTS                                    00125   
*                                                                       00126   
         LER   CURRESLT,WORKOR                                          00127   
         CER   CURRESLT,WORKAND                                         00128   
         BL    TESTEND                                                  00129   
         LER   CURRESLT,WORKAND                                         00130   
TESTEND  TM    0(PARMPTR),X'80'        CHECK FOR END OF LIST            00131   
         BZ    NEXTOP                  LOOP IF NOT END                  00132   
STORESLT STE   CURRESLT,0(RESULT)      ELSE STORE RESULT                00133   
         RETURN (14,12)                RETURN TO CALLER                 00134   
         LTORG                                                          00135   
         END                                                            00136   
