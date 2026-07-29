*CICS 83 BATCNTL(OPERV.CICS.BATCNTL) APPLID(A46NAS1P)                   00010011
A46NAS1P,CEMT S FI(GCDATES) CL                                         I00020011
WAIT=00000025                                                           00021011
A46NAS1P,CEMT S FI(GCDATES) DIS                                        A00030011
A46NAS1P,CEMT S FI(GCDATES) DS(HCV.EGENERIC.DATE)                      A00040011
A46NAS1P,CEMT S FI(GCDATES) CL NOU NOA NOD ENA                         A00050011
*                                                                       00061011
A46NAS1P,CEMT S FI(GCCONTR) CL                                         I00070011
WAIT=00000025                                                           00071011
A46NAS1P,CEMT S FI(GCCONTR) DIS                                        A00080011
A46NAS1P,CEMT S FI(GCCONTR) DS(HCV.EGENERIC.CONTRACT)                  A00090011
A46NAS1P,CEMT S FI(GCCONTR) CL NOU NOA NOD ENA                         A00100011
*                                                                       00101011
A46NAS1P,CEMT S FI(GCGRPSPC) CL                                        I00120011
WAIT=00000025                                                           00121011
A46NAS1P,CEMT S FI(GCGRPSPC) DIS                                       A00130011
A46NAS1P,CEMT S FI(GCGRPSPC) DS(HCV.EGENERIC.GROUPSPC)                 A00140011
A46NAS1P,CEMT S FI(GCGRPSPC) CL NOU NOA NOD ENA                        A00150011
