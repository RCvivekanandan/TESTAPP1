*********************************************************************** 00001   
*                                                                     * 00002   
* PROGRAM NAME  ===> ELKFLCMB                                         * 00003   
* PROGRAM TITLE ===> FUZZY LOGIC - COMBINE                            * 00004   
* AUTHOR        ===> R. LUKETICH                                      * 00005   
* DATE WRITTEN  ===> 29-SEP-1989                                      * 00006   
* FUNCTION      ===> PERFORMS A FUZZY LOGIC 'COMBINE' OF TWO OR MORE  * 00007   
*                    OPERANDS USING FUZZY TRUTH VALUES IN THE RANGE   * 00008   
*                    (-1..+1).                                        * 00009   
*                                                                     * 00010   
*                    A FUZZY LOGIC 'COMBINE' MERGES TWO OR MORE       * 00011   
*                    CONFIDENCE FACTORS TO PRODUCE A MODIFIED         * 00012   
*                    CONFIDENCE FACTOR.  THE COMBINATION FUNCTION     * 00013   
*                    OPERATES ON TWO CONFIDENCE FACTORS AT A TIME.    * 00014   
*                    IF MORE THAN TWO OPERANDS ARE SUBMITTED, THE     * 00015   
*                    COMBINE FUNCTION OPERATES ON THE FIRST TWO       * 00016   
*                    OPERANDS, THEN ON THE RESULT OF THE FIRST        * 00017   
*                    COMBINE AND THE THIRD OPERAND, ETC. UNTIL ALL    * 00018   
*                    OPERANDS ARE EXHAUSTED.                          * 00019   
*                                                                     * 00020   
*                                                                     * 00021   
*                    THE COMBINATION FUNCTION OPERATES DIFFERENTLY    * 00022   
*                    DEPENDING ON THE RELATIVE SIGNS OF THE TWO       * 00023   
*                    OPERANDS.  TO COMBINE TWO OPERANDS 'A' AND 'B':  * 00024   
*                                                                     * 00025   
*                    CASE (A >= 0) AND (B >= 0)                       * 00026   
*                                                                     * 00027   
*                         COMBINATION = A + B * (1 - A)               * 00028   
*                                                                     * 00029   
*                    CASE (A > 0) AND (B > 0)                         * 00030   
*                                                                     * 00031   
*                         COMBINATION = (A + B) / (1 - MIN(|A|,|B|))  * 00032   
*                                                                     * 00033   
*                    CASE (A < 0) AND (B < 0)                         * 00034   
*                                                                     * 00035   
*                         COMBINATION = -1 * COMBINATION(-A, -B)      * 00036   
*                                     = A + B * (1 + A)               * 00037   
*                                                                     * 00038   
*                                                                     * 00039   
*                    NOTE THAT IF NO OPERANDS ARE SUBMITTED, THE      * 00040   
*                    RESULT IS SET TO ZERO (INDETERMINATE).  IF ONLY  * 00041   
*                    ONE OPERAND IS SUBMITTED, THE VALUE OF THAT      * 00042   
*                    OPERAND IS RETURNED AS THE RESULT, I.E., THE     * 00043   
*                    RESULT IS THE SAME AS A SIMPLE MOVE OR           * 00044   
*                    ASSIGNMENT OF THE OPERAND TO THE RESULT.         * 00045   
*                                                                     * 00046   
* -------------------------- REGISTER USAGE ------------------------- * 00047   
* 00 -                               08 -                             * 00048   
* 01 - PARAMETER LIST BASE ADDRESS   09 -                             * 00049   
* 02 - RESULT ADDRESS                10 -                             * 00050   
* 03 - PARAMETER ADDRESS             11 -                             * 00051   
* 04 -                               12 - POINTER INTO PARAMETER LIST * 00052   
* 05 -                               13 - SAVE AREA ADDRESS           * 00053   
* 06 -                               14 - RETURN ADDRESS              * 00054   
* 07 -                               15 - ENTRY POINT ADDRESS         * 00055   
*                                                                     * 00056   
* F0 - CURRENT RESULT                F4 - WORK REGISTER               * 00057   
* F2 - NEXT PARAMETER                F8 - CONSTANT OF ONE             * 00058   
*                                                                     * 00059   
* ---------------------------- PARAMTERS ---------------------------- * 00060   
*                                                                     * 00061   
* THIS PROGRAM ASSUMES THAT REGISTER (1) POINTS TO A PARAMETER LIST   * 00062   
* THAT CONTAINS THE FOLLOWING:                                        * 00063   
*                                                                     * 00064   
*              A(RESULT)                                              * 00065   
*              A(OPERAND-1)                                           * 00066   
*              A(OPERAND-2)                                           * 00067   
*                   .                                                 * 00068   
*                   .                                                 * 00069   
*              A(OPERAND-N) (OR'D WITH X'80000000')                   * 00070   
*                                                                     * 00071   
* THE NUMBER OF OPERANDS IS (THEORETICALLY) LIMITED ONLY BY THE       * 00072   
* AMOUNT OF STORAGE AVAILABLE.  THE LAST ENTRY IN THE PARAMETER LIST  * 00073   
* IS IDENTIFIED BY THE FACT THAT THE HIGH ORDER BIT OF THE ADDRESS IS * 00074   
* SET TO '1'.  IBM COBOL LINKAGE CONVENTIONS SET THIS BIT CORRECTLY.  * 00075   
*                                                                     * 00076   
* ----------------------- MAINTENANCE HISTORY ----------------------- * 00077   
*                                                                     * 00078   
*    DATE     LVL  BY                    DESCRIPTION                  * 00079   
* ----------- ---- --- ---------------------------------------------- * 00080   
* 29-SEP-1989 0001 RJL ORIGINAL PROGRAM                               * 00081   
*                                                                     * 00082   
* 01-APR-2003 AKK       REGEN'D FOR CHANGES IN ELX                    * 00083   
*                             ASM RECOMPILES                          * 00084   
*                                                                     * 00085   
* 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *      00086   
*                                                                *      00087   
* 02.01 23-JAN-2004 AKK RECOMPILE AFTER COMPILER CHANGES         *      00088   
**AKK 12/06/05 REGEN FOR TEST                                    *    * 00089   
*********************************************************************** 00090   
*                                                                       00091   
PARMLIST EQU   1                                                        00092   
RESULT   EQU   2                                                        00093   
NEXTPARM EQU   3                                                        00094   
PARMPTR  EQU   12                                                       00095   
RETADD   EQU   14                                                       00096   
BASEREG  EQU   15                                                       00097   
CURRESLT EQU   0                                                        00098   
NEXTVALU EQU   2                                                        00099   
WORKVALU EQU   4                                                        00100   
WORKMIN  EQU   6                                                        00101   
ELKFLCMB CSECT                                                          00102   
         USING *,BASEREG                                                00103   
         SAVE  (14,12)                 SAVE CALLER'S REGISTERS          00104   
         LE    CURRESLT,=E'0E+00'      SET DEFAULT RESULT VALUE         00105   
         L     RESULT,0(PARMLIST)      GET RESULT ADDRESS               00106   
         LR    PARMPTR,PARMLIST        SET POINTER INTO PARAMETER LIST  00107   
         TM    0(PARMPTR),X'80'        CHECK FOR END OF PARAMETER LIST  00108   
         BNZ   STORESLT                STORE RESULT IF DONE             00109   
         LA    PARMPTR,4(PARMPTR)      POINT TO NEXT PARM LIST ENTRY    00110   
         L     NEXTPARM,0(PARMPTR)     GET ADDRESS OF NEXT LOGIC VALUE  00111   
         LE    CURRESLT,0(NEXTPARM)    GET VALUE                        00112   
         TM    0(PARMPTR),X'80'        CHECK FOR END OF PARAMETER LIST  00113   
         BNZ   STORESLT                STORE RESULT IF DONE             00114   
*                                                                       00115   
NEXTOP   LA    PARMPTR,4(PARMPTR)      POINT TO NEXT PARM LIST ENTRY    00116   
         L     NEXTPARM,0(PARMPTR)     GET ADDRESS OF NEXT LOGIC VALUE  00117   
         LE    NEXTVALU,0(NEXTPARM)    ELSE REPLACE CURR                00118   
*                                                                       00119   
*  TEST SIGNS OF OPERANDS                                               00120   
*                                                                       00121   
         LTER  CURRESLT,CURRESLT       CHECK 'A' VALUE                  00122   
         BM    TESTB                   A < 0                            00123   
         LTER  NEXTVALU,NEXTVALU       CHECK 'B' VALUE                  00124   
         BM    FORMULA2                A > 0 && B < 0                   00125   
         B     FORMULA1                A > 0 && B > 0                   00126   
*                                                                       00127   
TESTB    LTER  NEXTVALU,NEXTVALU       CHECK 'B' VALUE                  00128   
         BM    FORMULA3                A < 0 && B < 0                   00129   
*                                                                       00130   
FORMULA2 LPER  WORKMIN,CURRESLT        DETERMINE ABSOLUTE VALUES        00131   
         LPER  WORKVALU,NEXTVALU                                        00132   
         CER   WORKMIN,WORKVALU        DETERMINE MINIMUM                00133   
         BL    F2NEXT                                                   00134   
         LER   WORKMIN,WORKVALU        |B| < |A|                        00135   
F2NEXT   LE    WORKVALU,=E'1E+00'      CALCULATE 1 - MIN...             00136   
         SER   WORKVALU,WORKMIN                                         00137   
         LER   WORKMIN,CURRESLT        CALCULATE A + B                  00138   
         AER   WORKMIN,NEXTVALU                                         00139   
         DER   WORKMIN,WORKVALU        DO DIVISION                      00140   
         LER   CURRESLT,WORKMIN                                         00141   
         B     TESTEND                                                  00142   
*                                                                       00143   
FORMULA1 LE    WORKVALU,=E'1E+00'                                       00144   
         SER   WORKVALU,CURRESLT                                        00145   
         MER   WORKVALU,NEXTVALU                                        00146   
         LRER  WORKVALU,WORKVALU                                        00147   
         AER   CURRESLT,WORKVALU                                        00148   
         B     TESTEND                                                  00149   
*                                                                       00150   
FORMULA3 LE    WORKVALU,=E'1E+00'                                       00151   
         AER   WORKVALU,CURRESLT                                        00152   
         MER   WORKVALU,NEXTVALU                                        00153   
         LRER  WORKVALU,WORKVALU                                        00154   
         AER   CURRESLT,WORKVALU                                        00155   
*                                                                       00156   
TESTEND  TM    0(PARMPTR),X'80'        CHECK FOR END OF LIST            00157   
         BZ    NEXTOP                  LOOP IF NOT END                  00158   
STORESLT STE   CURRESLT,0(RESULT)      ELSE STORE RESULT                00159   
         RETURN (14,12)                RETURN TO CALLER                 00160   
         LTORG                                                          00161   
         END                                                            00162   
