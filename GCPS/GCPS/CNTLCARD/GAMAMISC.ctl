*CICS 07 BATCNTL(OPERV.CICS.BATCNTL) APPLID(A46MISCP)                   00010009
A46MISCP,CEMT S FI(GCDATES) CL                                         I00020009
WAIT=00000025                                                           00021009
A46MISCP,CEMT S FI(GCDATES) DIS                                        A00030009
A46MISCP,CEMT S FI(GCDATES) DS(HCV.EGENERIC.DATE)                      A00040009
A46MISCP,CEMT S FI(GCDATES) CL NOU NOA NOD ENA                         A00050009
*                                                                       00061009
A46MISCP,CEMT S FI(GCCONTR) CL                                         I00070009
WAIT=00000025                                                           00071009
A46MISCP,CEMT S FI(GCCONTR) DIS                                        A00080009
A46MISCP,CEMT S FI(GCCONTR) DS(HCV.EGENERIC.CONTRACT)                  A00090009
A46MISCP,CEMT S FI(GCCONTR) CL NOU NOA NOD ENA                         A00100009
*                                                                       00110009
A46MISCP,CEMT S FI(GCGRPSPC) CL                                        I00120009
WAIT=00000025                                                           00121009
A46MISCP,CEMT S FI(GCGRPSPC) DIS                                       A00130009
A46MISCP,CEMT S FI(GCGRPSPC) DS(HCV.EGENERIC.GROUPSPC)                 A00140009
A46MISCP,CEMT S FI(GCGRPSPC) CL NOU NOA NOD ENA                        A00150009
*                                                                       00160009
A46MISCP,CEMT S FI(GCSYSTBL) CL                                        I00170009
WAIT=00000025                                                           00171009
A46MISCP,CEMT S FI(GCSYSTBL) DIS                                       A00180009
A46MISCP,CEMT S FI(GCSYSTBL) DS(HCGENV.GENERIC.GCSYSTBL)               A00190009
A46MISCP,CEMT S FI(GCSYSTBL) CL NOU NOA NOD ENA                        A00200009
