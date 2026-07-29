*CICS 07 BATCNTL(OPERV.CICS.BATCNTL) APPLID(A46MISCP)                   00010000
*                                                                       00020000
A46MISCP,CEMT S FI(GCDATES) CL                                         I00030000
WAIT=00000025                                                           00040000
A46MISCP,CEMT S FI(GCDATES) DIS                                        A00050000
A46MISCP,CEMT S FI(GCDATES) DS(HCV.EGENERIC.READONLY.DATE)             A00060000
A46MISCP,CEMT S FI(GCDATES) CL NOU NOA NOD ENA                         A00070000
*                                                                       00080000
A46MISCP,CEMT S FI(GCCONTR) CL                                         I00090000
WAIT=00000025                                                           00100000
A46MISCP,CEMT S FI(GCCONTR) DIS                                        A00110000
A46MISCP,CEMT S FI(GCCONTR) DS(HCV.EGENERIC.READONLY.CONTRACT)         A00120000
A46MISCP,CEMT S FI(GCCONTR) CL NOU NOA NOD ENA                         A00130000
*                                                                       00140000
A46MISCP,CEMT S FI(GCGRPSPC) CL                                        I00150000
WAIT=00000025                                                           00160000
A46MISCP,CEMT S FI(GCGRPSPC) DIS                                       A00170000
A46MISCP,CEMT S FI(GCGRPSPC) DS(HCV.EGENERIC.READONLY.GROUPSPC)        A00180000
A46MISCP,CEMT S FI(GCGRPSPC) CL NOU NOA NOD ENA                        A00190000
*                                                                       00200000
A46MISCP,CEMT S FI(GCSYSTBL) CL                                        I00210000
WAIT=00000025                                                           00220000
A46MISCP,CEMT S FI(GCSYSTBL) DIS                                       A00230000
A46MISCP,CEMT S FI(GCSYSTBL) DS(HCGENV.GENERIC.READONLY.GCSYSTBL)      A00240000
A46MISCP,CEMT S FI(GCSYSTBL) CL NOU NOA NOD ENA                        A00250000
A46MISCP,CEMT S FI(GCSYSTBL) ENA                                       A00260000
