*CICS 59 BATCNTL(OPERV.CICS.BATCNTL.FILE2) APPLID(A46OAS1P)             00010000
A46OAS1P,CEMT S FI(GCDATES) CL                                         I00020000
WAIT=00000025                                                           00021000
A46OAS1P,CEMT S FI(GCDATES) DIS                                        A00030000
A46OAS1P,CEMT S FI(GCDATES) DS(HCV.EGENERIC.READONLY.DATE)             A00040000
A46OAS1P,CEMT S FI(GCDATES) CL NOU NOA NOD ENA                         A00050000
*                                                                       00051000
A46OAS1P,CEMT S FI(GCCONTR) CL                                         I00070000
WAIT=00000025                                                           00071000
A46OAS1P,CEMT S FI(GCCONTR) DIS                                        A00080000
A46OAS1P,CEMT S FI(GCCONTR) DS(HCV.EGENERIC.READONLY.CONTRACT)         A00090000
A46OAS1P,CEMT S FI(GCCONTR) CL NOU NOA NOD ENA                         A00100000
*                                                                       00101000
A46OAS1P,CEMT S FI(GCGRPSPC) CL                                        I00120000
WAIT=00000025                                                           00121000
A46OAS1P,CEMT S FI(GCGRPSPC) DIS                                       A00130000
A46OAS1P,CEMT S FI(GCGRPSPC) DS(HCV.EGENERIC.READONLY.GROUPSPC)        A00140000
A46OAS1P,CEMT S FI(GCGRPSPC) CL NOU NOA NOD ENA                        A00150000
