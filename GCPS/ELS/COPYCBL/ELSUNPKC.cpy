      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELSUNPKC                                        *00030000
      *    DATE:       27-APR-1989                                     *00040000
      *    AUTHOR:     EDWARD G LISS                                   *00050000
      *    FUNCTION:   ELS ABEND PROCESSING UNPACK SUBROUTINE          *00060000
      *                PARAMETER LIST.                                 *00070000
      *                                                                *00080000
      ******************************************************************00090000
      *                                                                *00100000
      *                      MAINTENANCE HISTORY                       *00110000
      *                                                                *00120000
      *  MOD     DATE     BY  DRPT                ACTION               *00130000
      * ----- ----------- --- ----- ---------------------------------- *00140000
      * 01.00 27-APR-1989 EGL       CREATED                            *00150000
      *                                                                *00160000
      ******************************************************************00170000
                                                                        00180000
       01  ELPUNPKR-PARM.                                               00190000
           05  EP-START-OFFSET         PICTURE S9(8) COMP.              00200000
           05  EP-CICS-REC-PTR         POINTER.                         00210000
           05  EP-LENGTH               PICTURE S9(4) COMP.              00220000
           05  EP-LINE-1.                                               00230001
               10  EP-LINE-1-ID        PICTURE X(11).                   00231001
               10  EP-LINE-1-TEXT      PICTURE X(121).                  00232001
           05  EP-LINE-2.                                               00240001
               10  EP-LINE-2-ID        PICTURE X(11).                   00241001
               10  EP-LINE-2-TEXT      PICTURE X(121).                  00242001
           05  EP-LINE-3.                                               00250001
               10  EP-LINE-3-ID        PICTURE X(11).                   00260001
               10  EP-LINE-3-TEXT      PICTURE X(121).                  00270001
