*CICS 59 BATCNTL(OPERV.CICS.BATCNTL.FILE2) APPLID(A46OAS1P)             00010012
A46OAS1P,CEMT S FI(GCDATES) CL                                         I00020012
WAIT=00000025                                                           00021012
A46OAS1P,CEMT S FI(GCDATES) DIS                                        A00030012
A46OAS1P,CEMT S FI(GCDATES) DS(HCV.EGENERIC.DATE)                      A00040012
A46OAS1P,CEMT S FI(GCDATES) CL NOU NOA NOD ENA                         A00050012
*                                                                       00060012
A46OAS1P,CEMT S FI(GCCONTR) CL                                         I00070012
WAIT=00000025                                                           00071012
A46OAS1P,CEMT S FI(GCCONTR) DIS                                        A00080012
A46OAS1P,CEMT S FI(GCCONTR) DS(HCV.EGENERIC.CONTRACT)                  A00090012
A46OAS1P,CEMT S FI(GCCONTR) CL NOU NOA NOD ENA                         A00100012
*                                                                       00110012
A46OAS1P,CEMT S FI(GCGRPSPC) CL                                        I00120012
WAIT=00000025                                                           00121012
A46OAS1P,CEMT S FI(GCGRPSPC) DIS                                       A00130012
A46OAS1P,CEMT S FI(GCGRPSPC) DS(HCV.EGENERIC.GROUPSPC)                 A00140012
A46OAS1P,CEMT S FI(GCGRPSPC) CL NOU NOA NOD ENA                        A00150012
