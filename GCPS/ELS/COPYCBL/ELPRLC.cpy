            05  RECORD-LIST.                                            00010001
000200         10  RL-RECORD-PREFIX        PICTURE  X(08).              00020000
000300         10  RL-FILE                 PICTURE  X(01).              00030000
000400             88  RL-GROUP-FILE       VALUE 'G'.                   00040000
000500             88  RL-CONTRACT-FILE    VALUE 'C'.                   00050000
000600             88  RL-BENEFIT-FILE     VALUE 'B'.                   00060000
000700             88  RL-TABULAR-FILE     VALUE 'T'.                   00070000
000800         10  RL-RECORD-NAME          PICTURE  X(50).              00080000
000900         10  RL-AUTO-REPRINT-FLAG    PICTURE  X(01).              00090000
001000             88  RL-REPRINT          VALUE 'Y'.                   00100000
001100         10  RL-DELETE-RECORD-FLAG   PICTURE  X(01).              00110000
001200             88  RL-DELETE           VALUE 'D'.                   00120000
