000100******************************************************************        
000200*    COPYBOOK:   ELXDMNLC                                        *        
000300*    AUTHOR:     ANNE KEFFER-KING.                               *        
000400*    DATE:       28-SEP-1992                                     *        
000500*    FUNCTION:   LIST OF MENTAL DIAGNOSES CODES.  THIS LIST      *        
000600*                IS BEING USED BY THE NA/ES SYSTEM TO HELP       *        
000700*                IN DETERMINING WHETHER OUTPATIENT SERVICES      *        
000800*                ARE COVERED.  AND IN DETERMINING HOW            *        
000900*                HOW 'CONFIDENT' WE ARE THAT PYSCH IS            *        
001000*                COVERED FOR A GIVEN PATIENT.                    *        
001100*                                                                *        
001200******************************************************************        
001300*                      MAINTENANCE HISTORY                       *        
001400*                                                                *        
001500*  MOD     DATE     BY  DRPT                ACTION               *        
001600* ----- ----------- --- ----- ---------------------------------- *        
001700* 1.00  28-SEP-1992 AKK       CREATED                            *        
001800******************************************************************        
001900 01  DMNL-DIAGNOSES-TABLE.                                                
002000     05 DMNL-DIAGNOSES-COUNT     PIC S9(04) COMP VALUE +461.              
002100     05 DMNL-DIAGNOSES-LIST.                                              
002200        10 FILLER                PIC  X(06)      VALUE 'V11  '.           
002300        10 FILLER                PIC  X(06)      VALUE 'V110 '.           
002400        10 FILLER                PIC  X(06)      VALUE 'V111 '.           
002500        10 FILLER                PIC  X(06)      VALUE 'V112 '.           
002600        10 FILLER                PIC  X(06)      VALUE 'V118 '.           
002700        10 FILLER                PIC  X(06)      VALUE 'V119 '.           
002800        10 FILLER                PIC  X(06)      VALUE 'V154 '.           
002900        10 FILLER                PIC  X(06)      VALUE 'V40  '.           
003000        10 FILLER                PIC  X(06)      VALUE 'V400 '.           
003100        10 FILLER                PIC  X(06)      VALUE 'V401 '.           
003200        10 FILLER                PIC  X(06)      VALUE 'V402 '.           
003300        10 FILLER                PIC  X(06)      VALUE 'V403 '.           
003400        10 FILLER                PIC  X(06)      VALUE 'V409 '.           
003500        10 FILLER                PIC  X(06)      VALUE 'V62  '.           
003600        10 FILLER                PIC  X(06)      VALUE 'V624 '.           
003700        10 FILLER                PIC  X(06)      VALUE 'V628 '.           
003800        10 FILLER                PIC  X(06)      VALUE 'V6281'.           
003900        10 FILLER                PIC  X(06)      VALUE 'V6289'.           
004000        10 FILLER                PIC  X(06)      VALUE 'V629 '.           
004100        10 FILLER                PIC  X(06)      VALUE 'V663 '.           
004200        10 FILLER                PIC  X(06)      VALUE 'V673 '.           
004300        10 FILLER                PIC  X(06)      VALUE 'V701 '.           
004400        10 FILLER                PIC  X(06)      VALUE 'V702 '.           
004500        10 FILLER                PIC  X(06)      VALUE 'V710 '.           
004600        10 FILLER                PIC  X(06)      VALUE 'V7101'.           
004700        10 FILLER                PIC  X(06)      VALUE 'V7102'.           
004800        10 FILLER                PIC  X(06)      VALUE 'V7109'.           
004900        10 FILLER                PIC  X(06)      VALUE 'V790 '.           
005000        10 FILLER                PIC  X(06)      VALUE '290  '.           
005100        10 FILLER                PIC  X(06)      VALUE '2900 '.           
005200        10 FILLER                PIC  X(06)      VALUE '2901 '.           
005300        10 FILLER                PIC  X(06)      VALUE '29010'.           
005400        10 FILLER                PIC  X(06)      VALUE '29011'.           
005500        10 FILLER                PIC  X(06)      VALUE '29012'.           
005600        10 FILLER                PIC  X(06)      VALUE '29013'.           
005700        10 FILLER                PIC  X(06)      VALUE '2902 '.           
005800        10 FILLER                PIC  X(06)      VALUE '29020'.           
005900        10 FILLER                PIC  X(06)      VALUE '29021'.           
006000        10 FILLER                PIC  X(06)      VALUE '2903 '.           
006100        10 FILLER                PIC  X(06)      VALUE '2904 '.           
006200        10 FILLER                PIC  X(06)      VALUE '29040'.           
006300        10 FILLER                PIC  X(06)      VALUE '29041'.           
006400        10 FILLER                PIC  X(06)      VALUE '29042'.           
006500        10 FILLER                PIC  X(06)      VALUE '29043'.           
006600        10 FILLER                PIC  X(06)      VALUE '2908 '.           
006700        10 FILLER                PIC  X(06)      VALUE '2909 '.           
006800        10 FILLER                PIC  X(06)      VALUE '293  '.           
006900        10 FILLER                PIC  X(06)      VALUE '2931 '.           
007000        10 FILLER                PIC  X(06)      VALUE '2938 '.           
007100        10 FILLER                PIC  X(06)      VALUE '29381'.           
007200        10 FILLER                PIC  X(06)      VALUE '29382'.           
007300        10 FILLER                PIC  X(06)      VALUE '29383'.           
007400        10 FILLER                PIC  X(06)      VALUE '29389'.           
007500        10 FILLER                PIC  X(06)      VALUE '2939 '.           
007600        10 FILLER                PIC  X(06)      VALUE '294  '.           
007700        10 FILLER                PIC  X(06)      VALUE '2940 '.           
007800        10 FILLER                PIC  X(06)      VALUE '2941 '.           
007900        10 FILLER                PIC  X(06)      VALUE '2948 '.           
008000        10 FILLER                PIC  X(06)      VALUE '2949 '.           
008100        10 FILLER                PIC  X(06)      VALUE '295  '.           
008200        10 FILLER                PIC  X(06)      VALUE '2950 '.           
008300        10 FILLER                PIC  X(06)      VALUE '29500'.           
008400        10 FILLER                PIC  X(06)      VALUE '29501'.           
008500        10 FILLER                PIC  X(06)      VALUE '29502'.           
008600        10 FILLER                PIC  X(06)      VALUE '29503'.           
008700        10 FILLER                PIC  X(06)      VALUE '29504'.           
008800        10 FILLER                PIC  X(06)      VALUE '29505'.           
008900        10 FILLER                PIC  X(06)      VALUE '2951 '.           
009000        10 FILLER                PIC  X(06)      VALUE '29510'.           
009100        10 FILLER                PIC  X(06)      VALUE '29511'.           
009200        10 FILLER                PIC  X(06)      VALUE '29512'.           
009300        10 FILLER                PIC  X(06)      VALUE '29513'.           
009400        10 FILLER                PIC  X(06)      VALUE '29514'.           
009500        10 FILLER                PIC  X(06)      VALUE '29515'.           
009600        10 FILLER                PIC  X(06)      VALUE '2952 '.           
009700        10 FILLER                PIC  X(06)      VALUE '29520'.           
009800        10 FILLER                PIC  X(06)      VALUE '29521'.           
009900        10 FILLER                PIC  X(06)      VALUE '29522'.           
010000        10 FILLER                PIC  X(06)      VALUE '29523'.           
010100        10 FILLER                PIC  X(06)      VALUE '29524'.           
010200        10 FILLER                PIC  X(06)      VALUE '29525'.           
010300        10 FILLER                PIC  X(06)      VALUE '2953 '.           
010400        10 FILLER                PIC  X(06)      VALUE '29530'.           
010500        10 FILLER                PIC  X(06)      VALUE '29531'.           
010600        10 FILLER                PIC  X(06)      VALUE '29532'.           
010700        10 FILLER                PIC  X(06)      VALUE '29533'.           
010800        10 FILLER                PIC  X(06)      VALUE '29534'.           
010900        10 FILLER                PIC  X(06)      VALUE '29535'.           
011000        10 FILLER                PIC  X(06)      VALUE '2954 '.           
011100        10 FILLER                PIC  X(06)      VALUE '29540'.           
011200        10 FILLER                PIC  X(06)      VALUE '29541'.           
011300        10 FILLER                PIC  X(06)      VALUE '29542'.           
011400        10 FILLER                PIC  X(06)      VALUE '29543'.           
011500        10 FILLER                PIC  X(06)      VALUE '29544'.           
011600        10 FILLER                PIC  X(06)      VALUE '29545'.           
011700        10 FILLER                PIC  X(06)      VALUE '2955 '.           
011800        10 FILLER                PIC  X(06)      VALUE '29550'.           
011900        10 FILLER                PIC  X(06)      VALUE '29551'.           
012000        10 FILLER                PIC  X(06)      VALUE '29552'.           
012100        10 FILLER                PIC  X(06)      VALUE '29553'.           
012200        10 FILLER                PIC  X(06)      VALUE '29554'.           
012300        10 FILLER                PIC  X(06)      VALUE '29555'.           
012400        10 FILLER                PIC  X(06)      VALUE '2956 '.           
012500        10 FILLER                PIC  X(06)      VALUE '29560'.           
012600        10 FILLER                PIC  X(06)      VALUE '29561'.           
012700        10 FILLER                PIC  X(06)      VALUE '29562'.           
012800        10 FILLER                PIC  X(06)      VALUE '29563'.           
012900        10 FILLER                PIC  X(06)      VALUE '29564'.           
013000        10 FILLER                PIC  X(06)      VALUE '29565'.           
013100        10 FILLER                PIC  X(06)      VALUE '2957 '.           
013200        10 FILLER                PIC  X(06)      VALUE '29570'.           
013300        10 FILLER                PIC  X(06)      VALUE '29571'.           
013400        10 FILLER                PIC  X(06)      VALUE '29572'.           
013500        10 FILLER                PIC  X(06)      VALUE '29573'.           
013600        10 FILLER                PIC  X(06)      VALUE '29574'.           
013700        10 FILLER                PIC  X(06)      VALUE '29575'.           
013800        10 FILLER                PIC  X(06)      VALUE '2958 '.           
013900        10 FILLER                PIC  X(06)      VALUE '29580'.           
014000        10 FILLER                PIC  X(06)      VALUE '29581'.           
014100        10 FILLER                PIC  X(06)      VALUE '29582'.           
014200        10 FILLER                PIC  X(06)      VALUE '29583'.           
014300        10 FILLER                PIC  X(06)      VALUE '29584'.           
014400        10 FILLER                PIC  X(06)      VALUE '29585'.           
014500        10 FILLER                PIC  X(06)      VALUE '2959 '.           
014600        10 FILLER                PIC  X(06)      VALUE '29590'.           
014700        10 FILLER                PIC  X(06)      VALUE '29591'.           
014800        10 FILLER                PIC  X(06)      VALUE '29592'.           
014900        10 FILLER                PIC  X(06)      VALUE '29593'.           
015000        10 FILLER                PIC  X(06)      VALUE '29594'.           
015100        10 FILLER                PIC  X(06)      VALUE '29595'.           
015200        10 FILLER                PIC  X(06)      VALUE '296  '.           
015300        10 FILLER                PIC  X(06)      VALUE '2960 '.           
015400        10 FILLER                PIC  X(06)      VALUE '29600'.           
015500        10 FILLER                PIC  X(06)      VALUE '29601'.           
015600        10 FILLER                PIC  X(06)      VALUE '29602'.           
015700        10 FILLER                PIC  X(06)      VALUE '29603'.           
015800        10 FILLER                PIC  X(06)      VALUE '29604'.           
015900        10 FILLER                PIC  X(06)      VALUE '29605'.           
016000        10 FILLER                PIC  X(06)      VALUE '29606'.           
016100        10 FILLER                PIC  X(06)      VALUE '2961 '.           
016200        10 FILLER                PIC  X(06)      VALUE '29610'.           
016300        10 FILLER                PIC  X(06)      VALUE '29611'.           
016400        10 FILLER                PIC  X(06)      VALUE '29612'.           
016500        10 FILLER                PIC  X(06)      VALUE '29613'.           
016600        10 FILLER                PIC  X(06)      VALUE '29614'.           
016700        10 FILLER                PIC  X(06)      VALUE '29615'.           
016800        10 FILLER                PIC  X(06)      VALUE '29616'.           
016900        10 FILLER                PIC  X(06)      VALUE '2962 '.           
017000        10 FILLER                PIC  X(06)      VALUE '29620'.           
017100        10 FILLER                PIC  X(06)      VALUE '29621'.           
017200        10 FILLER                PIC  X(06)      VALUE '29622'.           
017300        10 FILLER                PIC  X(06)      VALUE '29623'.           
017400        10 FILLER                PIC  X(06)      VALUE '29624'.           
017500        10 FILLER                PIC  X(06)      VALUE '29625'.           
017600        10 FILLER                PIC  X(06)      VALUE '29626'.           
017700        10 FILLER                PIC  X(06)      VALUE '2963 '.           
017800        10 FILLER                PIC  X(06)      VALUE '29630'.           
017900        10 FILLER                PIC  X(06)      VALUE '29631'.           
018000        10 FILLER                PIC  X(06)      VALUE '29632'.           
018100        10 FILLER                PIC  X(06)      VALUE '29633'.           
018200        10 FILLER                PIC  X(06)      VALUE '29634'.           
018300        10 FILLER                PIC  X(06)      VALUE '29635'.           
018400        10 FILLER                PIC  X(06)      VALUE '29636'.           
018500        10 FILLER                PIC  X(06)      VALUE '2964 '.           
018600        10 FILLER                PIC  X(06)      VALUE '29640'.           
018700        10 FILLER                PIC  X(06)      VALUE '29641'.           
018800        10 FILLER                PIC  X(06)      VALUE '29642'.           
018900        10 FILLER                PIC  X(06)      VALUE '29643'.           
019000        10 FILLER                PIC  X(06)      VALUE '29644'.           
019100        10 FILLER                PIC  X(06)      VALUE '29645'.           
019200        10 FILLER                PIC  X(06)      VALUE '29646'.           
019300        10 FILLER                PIC  X(06)      VALUE '2965 '.           
019400        10 FILLER                PIC  X(06)      VALUE '29650'.           
019500        10 FILLER                PIC  X(06)      VALUE '29651'.           
019600        10 FILLER                PIC  X(06)      VALUE '29652'.           
019700        10 FILLER                PIC  X(06)      VALUE '29653'.           
019800        10 FILLER                PIC  X(06)      VALUE '29654'.           
019900        10 FILLER                PIC  X(06)      VALUE '29655'.           
020000        10 FILLER                PIC  X(06)      VALUE '29656'.           
020100        10 FILLER                PIC  X(06)      VALUE '2966 '.           
020200        10 FILLER                PIC  X(06)      VALUE '29660'.           
020300        10 FILLER                PIC  X(06)      VALUE '29661'.           
020400        10 FILLER                PIC  X(06)      VALUE '29662'.           
020500        10 FILLER                PIC  X(06)      VALUE '29663'.           
020600        10 FILLER                PIC  X(06)      VALUE '29664'.           
020700        10 FILLER                PIC  X(06)      VALUE '29665'.           
020800        10 FILLER                PIC  X(06)      VALUE '29666'.           
020900        10 FILLER                PIC  X(06)      VALUE '2967 '.           
021000        10 FILLER                PIC  X(06)      VALUE '2968 '.           
021100        10 FILLER                PIC  X(06)      VALUE '29680'.           
021200        10 FILLER                PIC  X(06)      VALUE '29681'.           
021300        10 FILLER                PIC  X(06)      VALUE '29682'.           
021400        10 FILLER                PIC  X(06)      VALUE '29689'.           
021500        10 FILLER                PIC  X(06)      VALUE '2969 '.           
021600        10 FILLER                PIC  X(06)      VALUE '29690'.           
021700        10 FILLER                PIC  X(06)      VALUE '29699'.           
021800        10 FILLER                PIC  X(06)      VALUE '297  '.           
021900        10 FILLER                PIC  X(06)      VALUE '2970 '.           
022000        10 FILLER                PIC  X(06)      VALUE '2971 '.           
022100        10 FILLER                PIC  X(06)      VALUE '2972 '.           
022200        10 FILLER                PIC  X(06)      VALUE '2973 '.           
022300        10 FILLER                PIC  X(06)      VALUE '2978 '.           
022400        10 FILLER                PIC  X(06)      VALUE '2979 '.           
022500        10 FILLER                PIC  X(06)      VALUE '298  '.           
022600        10 FILLER                PIC  X(06)      VALUE '2980 '.           
022700        10 FILLER                PIC  X(06)      VALUE '2981 '.           
022800        10 FILLER                PIC  X(06)      VALUE '2982 '.           
022900        10 FILLER                PIC  X(06)      VALUE '2983 '.           
023000        10 FILLER                PIC  X(06)      VALUE '2984 '.           
023100        10 FILLER                PIC  X(06)      VALUE '2988 '.           
023200        10 FILLER                PIC  X(06)      VALUE '2989 '.           
023300        10 FILLER                PIC  X(06)      VALUE '299  '.           
023400        10 FILLER                PIC  X(06)      VALUE '2990 '.           
023500        10 FILLER                PIC  X(06)      VALUE '29900'.           
023600        10 FILLER                PIC  X(06)      VALUE '29901'.           
023700        10 FILLER                PIC  X(06)      VALUE '2991 '.           
023800        10 FILLER                PIC  X(06)      VALUE '29910'.           
023900        10 FILLER                PIC  X(06)      VALUE '29911'.           
024000        10 FILLER                PIC  X(06)      VALUE '2998 '.           
024100        10 FILLER                PIC  X(06)      VALUE '29980'.           
024200        10 FILLER                PIC  X(06)      VALUE '29981'.           
024300        10 FILLER                PIC  X(06)      VALUE '2999 '.           
024400        10 FILLER                PIC  X(06)      VALUE '29990'.           
024500        10 FILLER                PIC  X(06)      VALUE '29991'.           
024600        10 FILLER                PIC  X(06)      VALUE '300  '.           
024700        10 FILLER                PIC  X(06)      VALUE '3000 '.           
024800        10 FILLER                PIC  X(06)      VALUE '30000'.           
024900        10 FILLER                PIC  X(06)      VALUE '30001'.           
025000        10 FILLER                PIC  X(06)      VALUE '30002'.           
025100        10 FILLER                PIC  X(06)      VALUE '30009'.           
025200        10 FILLER                PIC  X(06)      VALUE '3001 '.           
025300        10 FILLER                PIC  X(06)      VALUE '30010'.           
025400        10 FILLER                PIC  X(06)      VALUE '30011'.           
025500        10 FILLER                PIC  X(06)      VALUE '30012'.           
025600        10 FILLER                PIC  X(06)      VALUE '30013'.           
025700        10 FILLER                PIC  X(06)      VALUE '30014'.           
025800        10 FILLER                PIC  X(06)      VALUE '30015'.           
025900        10 FILLER                PIC  X(06)      VALUE '30016'.           
026000        10 FILLER                PIC  X(06)      VALUE '30019'.           
026100        10 FILLER                PIC  X(06)      VALUE '3002 '.           
026200        10 FILLER                PIC  X(06)      VALUE '30020'.           
026300        10 FILLER                PIC  X(06)      VALUE '30021'.           
026400        10 FILLER                PIC  X(06)      VALUE '30022'.           
026500        10 FILLER                PIC  X(06)      VALUE '30023'.           
026600        10 FILLER                PIC  X(06)      VALUE '30029'.           
026700        10 FILLER                PIC  X(06)      VALUE '3003 '.           
026800        10 FILLER                PIC  X(06)      VALUE '3004 '.           
026900        10 FILLER                PIC  X(06)      VALUE '3005 '.           
027000        10 FILLER                PIC  X(06)      VALUE '3006 '.           
027100        10 FILLER                PIC  X(06)      VALUE '3007 '.           
027200        10 FILLER                PIC  X(06)      VALUE '3008 '.           
027300        10 FILLER                PIC  X(06)      VALUE '30081'.           
027400        10 FILLER                PIC  X(06)      VALUE '30089'.           
027500        10 FILLER                PIC  X(06)      VALUE '3009 '.           
027600        10 FILLER                PIC  X(06)      VALUE '301  '.           
027700        10 FILLER                PIC  X(06)      VALUE '3010 '.           
027800        10 FILLER                PIC  X(06)      VALUE '3011 '.           
027900        10 FILLER                PIC  X(06)      VALUE '30110'.           
028000        10 FILLER                PIC  X(06)      VALUE '30111'.           
028100        10 FILLER                PIC  X(06)      VALUE '30112'.           
028200        10 FILLER                PIC  X(06)      VALUE '30113'.           
028300        10 FILLER                PIC  X(06)      VALUE '3012 '.           
028400        10 FILLER                PIC  X(06)      VALUE '30120'.           
028500        10 FILLER                PIC  X(06)      VALUE '30121'.           
028600        10 FILLER                PIC  X(06)      VALUE '30122'.           
028700        10 FILLER                PIC  X(06)      VALUE '3013 '.           
028800        10 FILLER                PIC  X(06)      VALUE '3014 '.           
028900        10 FILLER                PIC  X(06)      VALUE '3015 '.           
029000        10 FILLER                PIC  X(06)      VALUE '30150'.           
029100        10 FILLER                PIC  X(06)      VALUE '30151'.           
029200        10 FILLER                PIC  X(06)      VALUE '30159'.           
029300        10 FILLER                PIC  X(06)      VALUE '3016 '.           
029400        10 FILLER                PIC  X(06)      VALUE '3017 '.           
029500        10 FILLER                PIC  X(06)      VALUE '3018 '.           
029600        10 FILLER                PIC  X(06)      VALUE '30181'.           
029700        10 FILLER                PIC  X(06)      VALUE '30182'.           
029800        10 FILLER                PIC  X(06)      VALUE '30183'.           
029900        10 FILLER                PIC  X(06)      VALUE '30184'.           
030000        10 FILLER                PIC  X(06)      VALUE '30189'.           
030100        10 FILLER                PIC  X(06)      VALUE '3019 '.           
030200        10 FILLER                PIC  X(06)      VALUE '302  '.           
030300        10 FILLER                PIC  X(06)      VALUE '3020 '.           
030400        10 FILLER                PIC  X(06)      VALUE '3021 '.           
030500        10 FILLER                PIC  X(06)      VALUE '3022 '.           
030600        10 FILLER                PIC  X(06)      VALUE '3023 '.           
030700        10 FILLER                PIC  X(06)      VALUE '3024 '.           
030800        10 FILLER                PIC  X(06)      VALUE '3025 '.           
030900        10 FILLER                PIC  X(06)      VALUE '30250'.           
031000        10 FILLER                PIC  X(06)      VALUE '30251'.           
031100        10 FILLER                PIC  X(06)      VALUE '30252'.           
031200        10 FILLER                PIC  X(06)      VALUE '30253'.           
031300        10 FILLER                PIC  X(06)      VALUE '3026 '.           
031400        10 FILLER                PIC  X(06)      VALUE '3027 '.           
031500        10 FILLER                PIC  X(06)      VALUE '30270'.           
031600        10 FILLER                PIC  X(06)      VALUE '30271'.           
031700        10 FILLER                PIC  X(06)      VALUE '30272'.           
031800        10 FILLER                PIC  X(06)      VALUE '30273'.           
031900        10 FILLER                PIC  X(06)      VALUE '30274'.           
032000        10 FILLER                PIC  X(06)      VALUE '30275'.           
032100        10 FILLER                PIC  X(06)      VALUE '30276'.           
032200        10 FILLER                PIC  X(06)      VALUE '30279'.           
032300        10 FILLER                PIC  X(06)      VALUE '3028 '.           
032400        10 FILLER                PIC  X(06)      VALUE '30281'.           
032500        10 FILLER                PIC  X(06)      VALUE '30282'.           
032600        10 FILLER                PIC  X(06)      VALUE '30283'.           
032700        10 FILLER                PIC  X(06)      VALUE '30284'.           
032800        10 FILLER                PIC  X(06)      VALUE '30285'.           
032900        10 FILLER                PIC  X(06)      VALUE '30289'.           
033000        10 FILLER                PIC  X(06)      VALUE '3029 '.           
033100        10 FILLER                PIC  X(06)      VALUE '306  '.           
033200        10 FILLER                PIC  X(06)      VALUE '3060 '.           
033300        10 FILLER                PIC  X(06)      VALUE '3061 '.           
033400        10 FILLER                PIC  X(06)      VALUE '3062 '.           
033500        10 FILLER                PIC  X(06)      VALUE '3063 '.           
033600        10 FILLER                PIC  X(06)      VALUE '3064 '.           
033700        10 FILLER                PIC  X(06)      VALUE '3065 '.           
033800        10 FILLER                PIC  X(06)      VALUE '30650'.           
033900        10 FILLER                PIC  X(06)      VALUE '30651'.           
034000        10 FILLER                PIC  X(06)      VALUE '30652'.           
034100        10 FILLER                PIC  X(06)      VALUE '30653'.           
034200        10 FILLER                PIC  X(06)      VALUE '30659'.           
034300        10 FILLER                PIC  X(06)      VALUE '3066 '.           
034400        10 FILLER                PIC  X(06)      VALUE '3067 '.           
034500        10 FILLER                PIC  X(06)      VALUE '3068 '.           
034600        10 FILLER                PIC  X(06)      VALUE '3069 '.           
034700        10 FILLER                PIC  X(06)      VALUE '307  '.           
034800        10 FILLER                PIC  X(06)      VALUE '3070 '.           
034900        10 FILLER                PIC  X(06)      VALUE '3071 '.           
035000        10 FILLER                PIC  X(06)      VALUE '3072 '.           
035100        10 FILLER                PIC  X(06)      VALUE '30720'.           
035200        10 FILLER                PIC  X(06)      VALUE '30721'.           
035300        10 FILLER                PIC  X(06)      VALUE '30722'.           
035400        10 FILLER                PIC  X(06)      VALUE '30723'.           
035500        10 FILLER                PIC  X(06)      VALUE '3073 '.           
035600        10 FILLER                PIC  X(06)      VALUE '3074 '.           
035700        10 FILLER                PIC  X(06)      VALUE '30740'.           
035800        10 FILLER                PIC  X(06)      VALUE '30741'.           
035900        10 FILLER                PIC  X(06)      VALUE '30742'.           
036000        10 FILLER                PIC  X(06)      VALUE '30743'.           
036100        10 FILLER                PIC  X(06)      VALUE '30744'.           
036200        10 FILLER                PIC  X(06)      VALUE '30745'.           
036300        10 FILLER                PIC  X(06)      VALUE '30746'.           
036400        10 FILLER                PIC  X(06)      VALUE '30747'.           
036500        10 FILLER                PIC  X(06)      VALUE '30748'.           
036600        10 FILLER                PIC  X(06)      VALUE '30749'.           
036700        10 FILLER                PIC  X(06)      VALUE '3075 '.           
036800        10 FILLER                PIC  X(06)      VALUE '30750'.           
036900        10 FILLER                PIC  X(06)      VALUE '30751'.           
037000        10 FILLER                PIC  X(06)      VALUE '30752'.           
037100        10 FILLER                PIC  X(06)      VALUE '30753'.           
037200        10 FILLER                PIC  X(06)      VALUE '30754'.           
037300        10 FILLER                PIC  X(06)      VALUE '30759'.           
037400        10 FILLER                PIC  X(06)      VALUE '3076 '.           
037500        10 FILLER                PIC  X(06)      VALUE '3077 '.           
037600        10 FILLER                PIC  X(06)      VALUE '3078 '.           
037700        10 FILLER                PIC  X(06)      VALUE '30780'.           
037800        10 FILLER                PIC  X(06)      VALUE '30789'.           
037900        10 FILLER                PIC  X(06)      VALUE '3079 '.           
038000        10 FILLER                PIC  X(06)      VALUE '308  '.           
038100        10 FILLER                PIC  X(06)      VALUE '3080 '.           
038200        10 FILLER                PIC  X(06)      VALUE '3081 '.           
038300        10 FILLER                PIC  X(06)      VALUE '3082 '.           
038400        10 FILLER                PIC  X(06)      VALUE '3083 '.           
038500        10 FILLER                PIC  X(06)      VALUE '3084 '.           
038600        10 FILLER                PIC  X(06)      VALUE '3089 '.           
038700        10 FILLER                PIC  X(06)      VALUE '309  '.           
038800        10 FILLER                PIC  X(06)      VALUE '3090 '.           
038900        10 FILLER                PIC  X(06)      VALUE '3091 '.           
039000        10 FILLER                PIC  X(06)      VALUE '3092 '.           
039100        10 FILLER                PIC  X(06)      VALUE '30921'.           
039200        10 FILLER                PIC  X(06)      VALUE '30922'.           
039300        10 FILLER                PIC  X(06)      VALUE '30923'.           
039400        10 FILLER                PIC  X(06)      VALUE '30924'.           
039500        10 FILLER                PIC  X(06)      VALUE '30928'.           
039600        10 FILLER                PIC  X(06)      VALUE '30929'.           
039700        10 FILLER                PIC  X(06)      VALUE '3093 '.           
039800        10 FILLER                PIC  X(06)      VALUE '3094 '.           
039900        10 FILLER                PIC  X(06)      VALUE '3098 '.           
040000        10 FILLER                PIC  X(06)      VALUE '30981'.           
040100        10 FILLER                PIC  X(06)      VALUE '30982'.           
040200        10 FILLER                PIC  X(06)      VALUE '30983'.           
040300        10 FILLER                PIC  X(06)      VALUE '30989'.           
040400        10 FILLER                PIC  X(06)      VALUE '3099 '.           
040500        10 FILLER                PIC  X(06)      VALUE '311  '.           
040600        10 FILLER                PIC  X(06)      VALUE '312  '.           
040700        10 FILLER                PIC  X(06)      VALUE '3120 '.           
040800        10 FILLER                PIC  X(06)      VALUE '31200'.           
040900        10 FILLER                PIC  X(06)      VALUE '31201'.           
041000        10 FILLER                PIC  X(06)      VALUE '31202'.           
041100        10 FILLER                PIC  X(06)      VALUE '31203'.           
041200        10 FILLER                PIC  X(06)      VALUE '3121 '.           
041300        10 FILLER                PIC  X(06)      VALUE '31210'.           
041400        10 FILLER                PIC  X(06)      VALUE '31211'.           
041500        10 FILLER                PIC  X(06)      VALUE '31212'.           
041600        10 FILLER                PIC  X(06)      VALUE '31213'.           
041700        10 FILLER                PIC  X(06)      VALUE '3122 '.           
041800        10 FILLER                PIC  X(06)      VALUE '31220'.           
041900        10 FILLER                PIC  X(06)      VALUE '31221'.           
042000        10 FILLER                PIC  X(06)      VALUE '31222'.           
042100        10 FILLER                PIC  X(06)      VALUE '31223'.           
042200        10 FILLER                PIC  X(06)      VALUE '3123 '.           
042300        10 FILLER                PIC  X(06)      VALUE '31230'.           
042400        10 FILLER                PIC  X(06)      VALUE '31231'.           
042500        10 FILLER                PIC  X(06)      VALUE '31232'.           
042600        10 FILLER                PIC  X(06)      VALUE '31233'.           
042700        10 FILLER                PIC  X(06)      VALUE '31234'.           
042800        10 FILLER                PIC  X(06)      VALUE '31235'.           
042900        10 FILLER                PIC  X(06)      VALUE '31239'.           
043000        10 FILLER                PIC  X(06)      VALUE '3124 '.           
043100        10 FILLER                PIC  X(06)      VALUE '3128 '.           
043200        10 FILLER                PIC  X(06)      VALUE '3129 '.           
043300        10 FILLER                PIC  X(06)      VALUE '313  '.           
043400        10 FILLER                PIC  X(06)      VALUE '3130 '.           
043500        10 FILLER                PIC  X(06)      VALUE '3131 '.           
043600        10 FILLER                PIC  X(06)      VALUE '3132 '.           
043700        10 FILLER                PIC  X(06)      VALUE '31321'.           
043800        10 FILLER                PIC  X(06)      VALUE '31322'.           
043900        10 FILLER                PIC  X(06)      VALUE '31323'.           
044000        10 FILLER                PIC  X(06)      VALUE '3133 '.           
044100        10 FILLER                PIC  X(06)      VALUE '3138 '.           
044200        10 FILLER                PIC  X(06)      VALUE '31381'.           
044300        10 FILLER                PIC  X(06)      VALUE '31382'.           
044400        10 FILLER                PIC  X(06)      VALUE '31383'.           
044500        10 FILLER                PIC  X(06)      VALUE '31389'.           
044600        10 FILLER                PIC  X(06)      VALUE '3139 '.           
044700        10 FILLER                PIC  X(06)      VALUE '314  '.           
044800        10 FILLER                PIC  X(06)      VALUE '3140 '.           
044900        10 FILLER                PIC  X(06)      VALUE '31400'.           
045000        10 FILLER                PIC  X(06)      VALUE '31401'.           
045100        10 FILLER                PIC  X(06)      VALUE '3141 '.           
045200        10 FILLER                PIC  X(06)      VALUE '3142 '.           
045300        10 FILLER                PIC  X(06)      VALUE '3148 '.           
045400        10 FILLER                PIC  X(06)      VALUE '3149 '.           
045500        10 FILLER                PIC  X(06)      VALUE '315  '.           
045600        10 FILLER                PIC  X(06)      VALUE '3150 '.           
045700        10 FILLER                PIC  X(06)      VALUE '31500'.           
045800        10 FILLER                PIC  X(06)      VALUE '31501'.           
045900        10 FILLER                PIC  X(06)      VALUE '31502'.           
046000        10 FILLER                PIC  X(06)      VALUE '31509'.           
046100        10 FILLER                PIC  X(06)      VALUE '3151 '.           
046200        10 FILLER                PIC  X(06)      VALUE '3152 '.           
046300        10 FILLER                PIC  X(06)      VALUE '3153 '.           
046400        10 FILLER                PIC  X(06)      VALUE '31531'.           
046500        10 FILLER                PIC  X(06)      VALUE '31539'.           
046600        10 FILLER                PIC  X(06)      VALUE '3154 '.           
046700        10 FILLER                PIC  X(06)      VALUE '3155 '.           
046800        10 FILLER                PIC  X(06)      VALUE '3158 '.           
046900        10 FILLER                PIC  X(06)      VALUE '3159 '.           
047000        10 FILLER                PIC  X(06)      VALUE '316  '.           
047100        10 FILLER                PIC  X(06)      VALUE '317  '.           
047200        10 FILLER                PIC  X(06)      VALUE '318  '.           
047300        10 FILLER                PIC  X(06)      VALUE '3180 '.           
047400        10 FILLER                PIC  X(06)      VALUE '3181 '.           
047500        10 FILLER                PIC  X(06)      VALUE '3182 '.           
047600        10 FILLER                PIC  X(06)      VALUE '319  '.           
047700        10 FILLER                PIC  X(06)      VALUE '6484 '.           
047800        10 FILLER                PIC  X(06)      VALUE '64840'.           
047900        10 FILLER                PIC  X(06)      VALUE '64841'.           
048000        10 FILLER                PIC  X(06)      VALUE '64842'.           
048100        10 FILLER                PIC  X(06)      VALUE '64843'.           
048200        10 FILLER                PIC  X(06)      VALUE '64844'.           
