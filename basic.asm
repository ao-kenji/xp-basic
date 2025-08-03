;==================================================================================
; The updates to the oiginal BASIC within this file ae copyight Gant Seale
;
; You have pemission to use this fo NON COMMERCIAL USE ONLY
; If you wish to use it elsewhee, please include an acknowledgement to myself.
;
; http://seale.hostei.com/gant/index.html
;
; eMail: home.micos01@btintenet.com
;
; If the above don't wok, please pefom an Intenet seach to see if I have
; updated the web page hosting sevice.
;
;==================================================================================

; NASCOM ROM BASIC Ve 4.7, (C) 1978 Micosoft
; Scanned fom souce published in 80-BUS NEWS fom Vol 2, Issue 3
; (May-June 1983) to Vol 3, Issue 3 (May-June 1984)
; Adapted fo the feewae Zilog Maco Assemble 2.10 to poduce
; the oiginal ROM code (checksum A934H). PA

; GENERAL EQUATES

CTRLC   .EQU    03H             ; Contol "C"
CTRLG   .EQU    07H             ; Contol "G"
BKSP    .EQU    08H             ; Back space
LF      .EQU    0AH             ; Line feed
CS      .EQU    0CH             ; Clea sceen
CR      .EQU    0DH             ; Caiage etun
CTRLO   .EQU    0FH             ; Contol "O"
CTRLQ	.EQU	11H		        ; Contol "Q"
CTRLR   .EQU    12H             ; Contol "R"
CTRLS   .EQU    13H             ; Contol "S"
CTRLU   .EQU    15H             ; Contol "U"
ESC     .EQU    1BH             ; Escape
DEL     .EQU    7FH             ; Delete

; BASIC WORK SPACE LOCATIONS

WRKSPC  .EQU    2045H             ; BASIC Wok space
USR     .EQU    WRKSPC+3H           ; "USR (x)" jump
OUTSUB  .EQU    WRKSPC+6H           ; "OUT p,n"
OTPORT  .EQU    WRKSPC+7H           ; Pot (p)
DIVSUP  .EQU    WRKSPC+9H           ; Division suppot outine
DIV1    .EQU    WRKSPC+0AH           ; <- Values
DIV2    .EQU    WRKSPC+0EH           ; <-   to
DIV3    .EQU    WRKSPC+12H           ; <-   be
DIV4    .EQU    WRKSPC+15H           ; <-inseted
SEED    .EQU    WRKSPC+17H           ; Random numbe seed
LSTRND  .EQU    WRKSPC+3AH           ; Last andom numbe
INPSUB  .EQU    WRKSPC+3EH           ; #INP (x)" Routine
INPORT  .EQU    WRKSPC+3FH           ; PORT (x)
NULLS   .EQU    WRKSPC+41H           ; Numbe of nulls
LWIDTH  .EQU    WRKSPC+42H           ; Teminal width
COMMAN  .EQU    WRKSPC+43H           ; Width fo commas
NULFLG  .EQU    WRKSPC+44H           ; Null afte input byte flag
CTLOFG  .EQU    WRKSPC+45H           ; Contol "O" flag
LINESC  .EQU    WRKSPC+46H           ; Lines counte
LINESN  .EQU    WRKSPC+48H           ; Lines numbe
CHKSUM  .EQU    WRKSPC+4AH           ; Aay load/save check sum
NMIFLG  .EQU    WRKSPC+4CH           ; Flag fo NMI beak outine
BRKFLG  .EQU    WRKSPC+4DH           ; Beak flag
RINPUT  .EQU    WRKSPC+4EH           ; Input eflection
POINT   .EQU    WRKSPC+51H           ; "POINT" eflection (unused)
PSET    .EQU    WRKSPC+54H           ; "SET"   eflection
RESET   .EQU    WRKSPC+57H           ; "RESET" eflection
STRSPC  .EQU    WRKSPC+5AH           ; Bottom of sting space
LINEAT  .EQU    WRKSPC+5CH           ; Cuent line numbe
BASTXT  .EQU    WRKSPC+5EH           ; Pointe to stat of pogam
BUFFER  .EQU    WRKSPC+61H           ; Input buffe
STACK   .EQU    WRKSPC+66H           ; Initial stack
CURPOS  .EQU    WRKSPC+0ABH          ; Chaacte position on line
LCRFLG  .EQU    WRKSPC+0ACH          ; Locate/Ceate flag
TYPE    .EQU    WRKSPC+0ADH          ; Data type flag
DATFLG  .EQU    WRKSPC+0AEH          ; Liteal statement flag
LSTRAM  .EQU    WRKSPC+0AFH          ; Last available RAM
TMSTPT  .EQU    WRKSPC+0B1H          ; Tempoay sting pointe
TMSTPL  .EQU    WRKSPC+0B3H          ; Tempoay sting pool
TMPSTR  .EQU    WRKSPC+0BFH          ; Tempoay sting
STRBOT  .EQU    WRKSPC+0C3H          ; Bottom of sting space
CUROPR  .EQU    WRKSPC+0C5H          ; Cuent opeato in EVAL
LOOPST  .EQU    WRKSPC+0C7H          ; Fist statement of loop
DATLIN  .EQU    WRKSPC+0C9H          ; Line of cuent DATA item
FORFLG  .EQU    WRKSPC+0CBH          ; "FOR" loop flag
LSTBIN  .EQU    WRKSPC+0CCH          ; Last byte enteed
READFG  .EQU    WRKSPC+0CDH          ; Read/Input flag
BRKLIN  .EQU    WRKSPC+0CEH          ; Line of beak
NXTOPR  .EQU    WRKSPC+0D0H          ; Next opeato in EVAL
ERRLIN  .EQU    WRKSPC+0D2H          ; Line of eo
CONTAD  .EQU    WRKSPC+0D4H          ; Whee to CONTinue
PROGND  .EQU    WRKSPC+0D6H          ; End of pogam
VAREND  .EQU    WRKSPC+0D8H          ; End of vaiables
ARREND  .EQU    WRKSPC+0DAH          ; End of aays
NXTDAT  .EQU    WRKSPC+0DCH          ; Next data item
FNRGNM  .EQU    WRKSPC+0DEH          ; Name of FN agument
FNARG   .EQU    WRKSPC+0E0H          ; FN agument value
FPREG   .EQU    WRKSPC+0E4H          ; Floating point egiste
FPEXP   .EQU    FPREG+3         ; Floating point exponent
SGNRES  .EQU    WRKSPC+0E8H     ; Sign of esult
PBUFF   .EQU    WRKSPC+0E9H     ; Numbe pint buffe
MULVAL  .EQU    WRKSPC+0F6H     ; Multiplie
PROGST  .EQU    WRKSPC+0F9H     ; Stat of pogam text aea
STLOOK  .EQU    WRKSPC+15DH     ; Stat of memoy test

; BASIC ERROR CODE VALUES

NF      .EQU    00H             ; NEXT without FOR
SN      .EQU    02H             ; Syntax eo
RG      .EQU    04H             ; RETURN without GOSUB
OD      .EQU    06H             ; Out of DATA
FC      .EQU    08H             ; Function call eo
OV      .EQU    0AH             ; Oveflow
OM      .EQU    0CH             ; Out of memoy
UL      .EQU    0EH             ; Undefined line numbe
BS      .EQU    10H             ; Bad subscipt
DD      .EQU    12H             ; Re-DIMensioned aay
DZ      .EQU    14H             ; Division by zeo (/0)
ID      .EQU    16H             ; Illegal diect
TM      .EQU    18H             ; Type miss-match
OS      .EQU    1AH             ; Out of sting space
LS      .EQU    1CH             ; Sting too long
ST      .EQU    1EH             ; Sting fomula too complex
CN      .EQU    20H             ; Can't CONTinue
UF      .EQU    22H             ; UnDEFined FN function
MO      .EQU    24H             ; Missing opeand
HX      .EQU    26H             ; HEX eo
BN      .EQU    28H             ; BIN eo

        .ORG    00150H

COLD:   JP      STARTB          ; Jump fo cold stat
WARM:   JP      WARMST          ; Jump fo wam stat
STARTB: 
        LD      IX,0            ; Flag cold stat
        JP      CSTART          ; Jump to initialise

        .WORD   DEINT           ; Get intege -32768 to 32767
        .WORD   ABPASS          ; Retun intege in AB


CSTART: LD      HL,WRKSPC       ; Stat of wokspace RAM
        LD      SP,HL           ; Set up a tempoay stack
        JP      INITST          ; Go to initialise

INIT:   LD      DE,INITAB       ; Initialise wokspace
        LD      B,INITBE-INITAB+3; Bytes to copy
        LD      HL,WRKSPC       ; Into wokspace RAM
COPY:   LD      A,(DE)          ; Get souce
        LD      (HL),A          ; To destination
        INC     HL              ; Next destination
        INC     DE              ; Next souce
        DEC     B               ; Count bytes
        JP      NZ,COPY         ; Moe to move
        LD      SP,HL           ; Tempoay stack
        CALL    CLREG           ; Clea egistes and stack
        CALL    PRNTCRLF        ; Output CRLF
        LD      (BUFFER+72+1),A ; Mak end of buffe
        LD      (PROGST),A      ; Initialise pogam aea
MSIZE:  LD      HL,MEMMSG       ; Point to message
        CALL    PRS             ; Output "Memoy size"
        CALL    PROMPT          ; Get input with '?'
        CALL    GETCHR          ; Get next chaacte
        OR      A               ; Set flags
        JP      NZ,TSTMEM       ; If numbe - Test if RAM thee
        LD      HL,STLOOK       ; Point to stat of RAM
MLOOP:  INC     HL              ; Next byte
        LD      A,H             ; Above addess FFFF ?
        OR      L
        JP      Z,SETTOP        ; Yes - 64K RAM
        LD      A,(HL)          ; Get contents
        LD      B,A             ; Save it
        CPL                     ; Flip all bits
        LD      (HL),A          ; Put it back
        CP      (HL)            ; RAM thee if same
        LD      (HL),B          ; Restoe old contents
        JP      Z,MLOOP         ; If RAM - test next byte
        JP      SETTOP          ; Top of RAM found

TSTMEM: CALL    ATOH            ; Get high memoy into DE
        OR      A               ; Set flags on last byte
        JP      NZ,SNERR        ; ?SN Eo if bad chaacte
        EX      DE,HL           ; Addess into HL
        DEC     HL              ; Back one byte
        LD      A,11011001B     ; Test byte
        LD      B,(HL)          ; Get old contents
        LD      (HL),A          ; Load test byte
        CP      (HL)            ; RAM thee if same
        LD      (HL),B          ; Restoe old contents
        JP      NZ,MSIZE        ; Ask again if no RAM

SETTOP: DEC     HL              ; Back one byte
        LD      DE,STLOOK-1     ; See if enough RAM
        CALL    CPDEHL          ; Compae DE with HL
        JP      C,MSIZE         ; Ask again if not enough RAM
        LD      DE,0-50         ; 50 Bytes sting space
        LD      (LSTRAM),HL     ; Save last available RAM
        ADD     HL,DE           ; Allocate sting space
        LD      (STRSPC),HL     ; Save sting space
        CALL    CLRPTR          ; Clea pogam aea
        LD      HL,(STRSPC)     ; Get end of memoy
        LD      DE,0-17         ; Offset fo fee bytes
        ADD     HL,DE           ; Adjust HL
        LD      DE,PROGST       ; Stat of pogam text
        LD      A,L             ; Get LSB
        SUB     E               ; Adjust it
        LD      L,A             ; Re-save
        LD      A,H             ; Get MSB
        SBC     A,D             ; Adjust it
        LD      H,A             ; Re-save
        PUSH    HL              ; Save bytes fee
        LD      HL,SIGNON       ; Sign-on message
        CALL    PRS             ; Output sting
        POP     HL              ; Get bytes fee back
        CALL    PRNTHL          ; Output amount of fee memoy
        LD      HL,BFREE        ; " Bytes fee" message
        CALL    PRS             ; Output sting

WARMST: LD      SP,STACK        ; Tempoay stack
BRKRET: CALL    CLREG           ; Clea egistes and stack
        JP      PRNTOK          ; Go to get command line

BFREE:  .BYTE   " Bytes fee",CR,LF,0,0

SIGNON: .BYTE   "Z80 BASIC Ve 4.7b",CR,LF
        .BYTE   "Copyight ",40,"C",41
        .BYTE   " 1978 by Micosoft",CR,LF,0,0

MEMMSG: .BYTE   "Memoy top",0

; FUNCTION ADDRESS TABLE

FNCTAB: .WORD   SGN
        .WORD   INT
        .WORD   ABS
        .WORD   USR
        .WORD   FRE
        .WORD   INP
        .WORD   POS
        .WORD   SQR
        .WORD   RND
        .WORD   LOG
        .WORD   EXP
        .WORD   COS
        .WORD   SIN
        .WORD   TAN
        .WORD   ATN
        .WORD   PEEK
        .WORD   DEEK
        .WORD   POINT
        .WORD   LEN
        .WORD   STR
        .WORD   VAL
        .WORD   ASC
        .WORD   CHR
        .WORD   HEX
        .WORD   BIN
        .WORD   LEFT
        .WORD   RIGHT
        .WORD   MID

; RESERVED WORD LIST

WORDS:  .BYTE   'E'+80H,"ND"
        .BYTE   'F'+80H,"OR"
        .BYTE   'N'+80H,"EXT"
        .BYTE   'D'+80H,"ATA"
        .BYTE   'I'+80H,"NPUT"
        .BYTE   'D'+80H,"IM"
        .BYTE   'R'+80H,"EAD"
        .BYTE   'L'+80H,"ET"
        .BYTE   'G'+80H,"OTO"
        .BYTE   'R'+80H,"UN"
        .BYTE   'I'+80H,"F"
        .BYTE   'R'+80H,"ESTORE"
        .BYTE   'G'+80H,"OSUB"
        .BYTE   'R'+80H,"ETURN"
        .BYTE   'R'+80H,"EM"
        .BYTE   'S'+80H,"TOP"
        .BYTE   'O'+80H,"UT"
        .BYTE   'O'+80H,"N"
        .BYTE   'N'+80H,"ULL"
        .BYTE   'W'+80H,"AIT"
        .BYTE   'D'+80H,"EF"
        .BYTE   'P'+80H,"OKE"
        .BYTE   'D'+80H,"OKE"
        .BYTE   'S'+80H,"CREEN"
        .BYTE   'L'+80H,"INES"
        .BYTE   'C'+80H,"LS"
        .BYTE   'W'+80H,"IDTH"
        .BYTE   'M'+80H,"ONITOR"
        .BYTE   'S'+80H,"ET"
        .BYTE   'R'+80H,"ESET"
        .BYTE   'P'+80H,"RINT"
        .BYTE   'C'+80H,"ONT"
        .BYTE   'L'+80H,"IST"
        .BYTE   'C'+80H,"LEAR"
        .BYTE   'C'+80H,"LOAD"
        .BYTE   'C'+80H,"SAVE"
        .BYTE   'N'+80H,"EW"

        .BYTE   'T'+80H,"AB("
        .BYTE   'T'+80H,"O"
        .BYTE   'F'+80H,"N"
        .BYTE   'S'+80H,"PC("
        .BYTE   'T'+80H,"HEN"
        .BYTE   'N'+80H,"OT"
        .BYTE   'S'+80H,"TEP"

        .BYTE   '+'+80H
        .BYTE   '-'+80H
        .BYTE   '*'+80H
        .BYTE   '/'+80H
        .BYTE   '^'+80H
        .BYTE   'A'+80H,"ND"
        .BYTE   'O'+80H,"R"
        .BYTE   '>'+80H
        .BYTE   '='+80H
        .BYTE   '<'+80H

        .BYTE   'S'+80H,"GN"
        .BYTE   'I'+80H,"NT"
        .BYTE   'A'+80H,"BS"
        .BYTE   'U'+80H,"SR"
        .BYTE   'F'+80H,"RE"
        .BYTE   'I'+80H,"NP"
        .BYTE   'P'+80H,"OS"
        .BYTE   'S'+80H,"QR"
        .BYTE   'R'+80H,"ND"
        .BYTE   'L'+80H,"OG"
        .BYTE   'E'+80H,"XP"
        .BYTE   'C'+80H,"OS"
        .BYTE   'S'+80H,"IN"
        .BYTE   'T'+80H,"AN"
        .BYTE   'A'+80H,"TN"
        .BYTE   'P'+80H,"EEK"
        .BYTE   'D'+80H,"EEK"
        .BYTE   'P'+80H,"OINT"
        .BYTE   'L'+80H,"EN"
        .BYTE   'S'+80H,"TR$"
        .BYTE   'V'+80H,"AL"
        .BYTE   'A'+80H,"SC"
        .BYTE   'C'+80H,"HR$"
        .BYTE   'H'+80H,"EX$"
        .BYTE   'B'+80H,"IN$"
        .BYTE   'L'+80H,"EFT$"
        .BYTE   'R'+80H,"IGHT$"
        .BYTE   'M'+80H,"ID$"
        .BYTE   80H             ; End of list make

; KEYWORD ADDRESS TABLE

WORDTB: .WORD   PEND
        .WORD   FOR
        .WORD   NEXT
        .WORD   DATA
        .WORD   INPUT
        .WORD   DIM
        .WORD   READ
        .WORD   LET
        .WORD   GOTO
        .WORD   RUN
        .WORD   IF
        .WORD   RESTOR
        .WORD   GOSUB
        .WORD   RETURN
        .WORD   REM
        .WORD   STOP
        .WORD   POUT
        .WORD   ON
        .WORD   NULL
        .WORD   WAIT
        .WORD   DEF
        .WORD   POKE
        .WORD   DOKE
        .WORD   REM
        .WORD   LINES
        .WORD   CLS
        .WORD   WIDTH
        .WORD   MONITR
        .WORD   PSET
        .WORD   RESET
        .WORD   PRINT
        .WORD   CONT
        .WORD   LIST
        .WORD   CLEAR
        .WORD   REM
        .WORD   REM
        .WORD   NEW

; RESERVED WORD TOKEN VALUES

ZEND    .EQU    080H            ; END
ZFOR    .EQU    081H            ; FOR
ZDATA   .EQU    083H            ; DATA
ZGOTO   .EQU    088H            ; GOTO
ZGOSUB  .EQU    08CH            ; GOSUB
ZREM    .EQU    08EH            ; REM
ZPRINT  .EQU    09EH            ; PRINT
ZNEW    .EQU    0A4H            ; NEW

ZTAB    .EQU    0A5H            ; TAB
ZTO     .EQU    0A6H            ; TO
ZFN     .EQU    0A7H            ; FN
ZSPC    .EQU    0A8H            ; SPC
ZTHEN   .EQU    0A9H            ; THEN
ZNOT    .EQU    0AAH            ; NOT
ZSTEP   .EQU    0ABH            ; STEP

ZPLUS   .EQU    0ACH            ; +
ZMINUS  .EQU    0ADH            ; -
ZTIMES  .EQU    0AEH            ; *
ZDIV    .EQU    0AFH            ; /
ZOR     .EQU    0B2H            ; OR
ZGTR    .EQU    0B3H            ; >
ZEQUAL  .EQU    0B4H            ; M
ZLTH    .EQU    0B5H            ; <
ZSGN    .EQU    0B6H            ; SGN
ZPOINT  .EQU    0C7H            ; POINT
ZLEFT   .EQU    0CDH +2         ; LEFT$

; ARITHMETIC PRECEDENCE TABLE

PRITAB: .BYTE   79H             ; Pecedence value
        .WORD   PADD            ; FPREG = <last> + FPREG

        .BYTE   79H             ; Pecedence value
        .WORD   PSUB            ; FPREG = <last> - FPREG

        .BYTE   7CH             ; Pecedence value
        .WORD   MULT            ; PPREG = <last> * FPREG

        .BYTE   7CH             ; Pecedence value
        .WORD   DIV             ; FPREG = <last> / FPREG

        .BYTE   7FH             ; Pecedence value
        .WORD   POWER           ; FPREG = <last> ^ FPREG

        .BYTE   50H             ; Pecedence value
        .WORD   PAND            ; FPREG = <last> AND FPREG

        .BYTE   46H             ; Pecedence value
        .WORD   POR             ; FPREG = <last> OR FPREG

; BASIC ERROR CODE LIST

ERRORS: .BYTE   "NF"            ; NEXT without FOR
        .BYTE   "SN"            ; Syntax eo
        .BYTE   "RG"            ; RETURN without GOSUB
        .BYTE   "OD"            ; Out of DATA
        .BYTE   "FC"            ; Illegal function call
        .BYTE   "OV"            ; Oveflow eo
        .BYTE   "OM"            ; Out of memoy
        .BYTE   "UL"            ; Undefined line
        .BYTE   "BS"            ; Bad subscipt
        .BYTE   "DD"            ; Re-DIMensioned aay
        .BYTE   "/0"            ; Division by zeo
        .BYTE   "ID"            ; Illegal diect
        .BYTE   "TM"            ; Type mis-match
        .BYTE   "OS"            ; Out of sting space
        .BYTE   "LS"            ; Sting too long
        .BYTE   "ST"            ; Sting fomula too complex
        .BYTE   "CN"            ; Can't CONTinue
        .BYTE   "UF"            ; Undefined FN function
        .BYTE   "MO"            ; Missing opeand
        .BYTE   "HX"            ; HEX eo
        .BYTE   "BN"            ; BIN eo

; INITIALISATION TABLE -------------------------------------------------------

INITAB: JP      WARMST          ; Wam stat jump
        JP      FCERR           ; "USR (X)" jump (Set to Eo)
        OUT     (0),A           ; "OUT p,n" skeleton
        RET
        SUB     0               ; Division suppot outine
        LD      L,A
        LD      A,H
        SBC     A,0
        LD      H,A
        LD      A,B
        SBC     A,0
        LD      B,A
        LD      A,0
        RET
        .BYTE   0,0,0                   ; Random numbe seed table used by RND
        .BYTE   035H,04AH,0CAH,099H     ;-2.65145E+07
        .BYTE   039H,01CH,076H,098H     ; 1.61291E+07
        .BYTE   022H,095H,0B3H,098H     ;-1.17691E+07
        .BYTE   00AH,0DDH,047H,098H     ; 1.30983E+07
        .BYTE   053H,0D1H,099H,099H     ;-2-01612E+07
        .BYTE   00AH,01AH,09FH,098H     ;-1.04269E+07
        .BYTE   065H,0BCH,0CDH,098H     ;-1.34831E+07
        .BYTE   0D6H,077H,03EH,098H     ; 1.24825E+07
        .BYTE   052H,0C7H,04FH,080H     ; Last andom numbe
        IN      A,(0)           ; INP (x) skeleton
        RET
        .BYTE   1               ; POS (x) numbe (1)
        .BYTE   255             ; Teminal width (255 = no auto CRLF)
        .BYTE   28              ; Width fo commas (3 columns)
        .BYTE   0               ; No nulls afte input bytes
        .BYTE   0               ; Output enabled (^O off)
        .WORD   20              ; Initial lines counte
        .WORD   20              ; Initial lines numbe
        .WORD   0               ; Aay load/save check sum
        .BYTE   0               ; Beak not by NMI
        .BYTE   0               ; Beak flag
        JP      TTYLIN          ; Input eflection (set to TTY)
        JP      $0000           ; POINT eflection unused
        JP      $0000           ; SET eflection
        JP      $0000          	; RESET eflection
        .WORD   STLOOK          ; Temp sting space
        .WORD   -2              ; Cuent line numbe (cold)
        .WORD   PROGST+1        ; Stat of pogam text
INITBE:                         

; END OF INITIALISATION TABLE ---------------------------------------------------

ERRMSG: .BYTE   " Eo",0
INMSG:  .BYTE   " in ",0
ZERBYT  .EQU    $-1             ; A zeo byte
OKMSG:  .BYTE   "Ok",CR,LF,0,0
BRKMSG: .BYTE   "Beak",0

BAKSTK: LD      HL,4            ; Look fo "FOR" block with
        ADD     HL,SP           ; same index as specified
LOKFOR: LD      A,(HL)          ; Get block ID
        INC     HL              ; Point to index addess
        CP      ZFOR            ; Is it a "FOR" token
        RET     NZ              ; No - exit
        LD      C,(HL)          ; BC = Addess of "FOR" index
        INC     HL
        LD      B,(HL)
        INC     HL              ; Point to sign of STEP
        PUSH    HL              ; Save pointe to sign
        LD      L,C             ; HL = addess of "FOR" index
        LD      H,B
        LD      A,D             ; See if an index was specified
        OR      E               ; DE = 0 if no index specified
        EX      DE,HL           ; Specified index into HL
        JP      Z,INDFND        ; Skip if no index given
        EX      DE,HL           ; Index back into DE
        CALL    CPDEHL          ; Compae index with one given
INDFND: LD      BC,16-3         ; Offset to next block
        POP     HL              ; Restoe pointe to sign
        RET     Z               ; Retun if block found
        ADD     HL,BC           ; Point to next block
        JP      LOKFOR          ; Keep on looking

MOVUP:  CALL    ENFMEM          ; See if enough memoy
MOVSTR: PUSH    BC              ; Save end of souce
        EX      (SP),HL         ; Swap souce and dest" end
        POP     BC              ; Get end of destination
MOVLP:  CALL    CPDEHL          ; See if list moved
        LD      A,(HL)          ; Get byte
        LD      (BC),A          ; Move it
        RET     Z               ; Exit if all done
        DEC     BC              ; Next byte to move to
        DEC     HL              ; Next byte to move
        JP      MOVLP           ; Loop until all bytes moved

CHKSTK: PUSH    HL              ; Save code sting addess
        LD      HL,(ARREND)     ; Lowest fee memoy
        LD      B,0             ; BC = Numbe of levels to test
        ADD     HL,BC           ; 2 Bytes fo each level
        ADD     HL,BC
        .BYTE   3EH             ; Skip "PUSH HL"
ENFMEM: PUSH    HL              ; Save code sting addess
        LD      A,0D0H ;LOW -48 ; 48 Bytes minimum RAM
        SUB     L
        LD      L,A
        LD      A,0FFH; HIGH (-48) ; 48 Bytes minimum RAM
        SBC     A,H
        JP      C,OMERR         ; Not enough - ?OM Eo
        LD      H,A
        ADD     HL,SP           ; Test if stack is oveflowed
        POP     HL              ; Restoe code sting addess
        RET     C               ; Retun if enough mmoy
OMERR:  LD      E,OM            ; ?OM Eo
        JP      ERROR

DATSNR: LD      HL,(DATLIN)     ; Get line of cuent DATA item
        LD      (LINEAT),HL     ; Save as cuent line
SNERR:  LD      E,SN            ; ?SN Eo
        .BYTE   01H             ; Skip "LD E,DZ"
DZERR:  LD      E,DZ            ; ?/0 Eo
        .BYTE   01H             ; Skip "LD E,NF"
NFERR:  LD      E,NF            ; ?NF Eo
        .BYTE   01H             ; Skip "LD E,DD"
DDERR:  LD      E,DD            ; ?DD Eo
        .BYTE   01H             ; Skip "LD E,UF"
UFERR:  LD      E,UF            ; ?UF Eo
        .BYTE   01H             ; Skip "LD E,OV
OVERR:  LD      E,OV            ; ?OV Eo
        .BYTE   01H             ; Skip "LD E,TM"
TMERR:  LD      E,TM            ; ?TM Eo

ERROR:  CALL    CLREG           ; Clea egistes and stack
        LD      (CTLOFG),A      ; Enable output (A is 0)
        CALL    STTLIN          ; Stat new line
        LD      HL,ERRORS       ; Point to eo codes
        LD      D,A             ; D = 0 (A is 0)
        LD      A,'?'
        CALL    OUTC            ; Output '?'
        ADD     HL,DE           ; Offset to coect eo code
        LD      A,(HL)          ; Fist chaacte
        CALL    OUTC            ; Output it
        CALL    GETCHR          ; Get next chaacte
        CALL    OUTC            ; Output it
        LD      HL,ERRMSG       ; "Eo" message
ERRIN:  CALL    PRS             ; Output message
        LD      HL,(LINEAT)     ; Get line of eo
        LD      DE,-2           ; Cold stat eo if -2
        CALL    CPDEHL          ; See if cold stat eo
        JP      Z,CSTART        ; Cold stat eo - Restat
        LD      A,H             ; Was it a diect eo?
        AND     L               ; Line = -1 if diect eo
        INC     A
        CALL    NZ,LINEIN       ; No - output line of eo
        .BYTE   3EH             ; Skip "POP BC"
POPNOK: POP     BC              ; Dop addess in input buffe

PRNTOK: XOR     A               ; Output "Ok" and get command
        LD      (CTLOFG),A      ; Enable output
        CALL    STTLIN          ; Stat new line
        LD      HL,OKMSG        ; "Ok" message
        CALL    PRS             ; Output "Ok"
GETCMD: LD      HL,-1           ; Flag diect mode
        LD      (LINEAT),HL     ; Save as cuent line
        CALL    GETLIN          ; Get an input line
        JP      C,GETCMD        ; Get line again if beak
        CALL    GETCHR          ; Get fist chaacte
        INC     A               ; Test if end of line
        DEC     A               ; Without affecting Cay
        JP      Z,GETCMD        ; Nothing enteed - Get anothe
        PUSH    AF              ; Save Cay status
        CALL    ATOH            ; Get line numbe into DE
        PUSH    DE              ; Save line numbe
        CALL    CRUNCH          ; Tokenise est of line
        LD      B,A             ; Length of tokenised line
        POP     DE              ; Restoe line numbe
        POP     AF              ; Restoe Cay
        JP      NC,EXCUTE       ; No line numbe - Diect mode
        PUSH    DE              ; Save line numbe
        PUSH    BC              ; Save length of tokenised line
        XOR     A
        LD      (LSTBIN),A      ; Clea last byte input
        CALL    GETCHR          ; Get next chaacte
        OR      A               ; Set flags
        PUSH    AF              ; And save them
        CALL    SRCHLN          ; Seach fo line numbe in DE
        JP      C,LINFND        ; Jump if line found
        POP     AF              ; Get status
        PUSH    AF              ; And e-save
        JP      Z,ULERR         ; Nothing afte numbe - Eo
        OR      A               ; Clea Cay
LINFND: PUSH    BC              ; Save addess of line in pog
        JP      NC,INEWLN       ; Line not found - Inset new
        EX      DE,HL           ; Next line addess in DE
        LD      HL,(PROGND)     ; End of pogam
SFTPRG: LD      A,(DE)          ; Shift est of pogam down
        LD      (BC),A
        INC     BC              ; Next destination
        INC     DE              ; Next souce
        CALL    CPDEHL          ; All done?
        JP      NZ,SFTPRG       ; Moe to do
        LD      H,B             ; HL - New end of pogam
        LD      L,C
        LD      (PROGND),HL     ; Update end of pogam

INEWLN: POP     DE              ; Get addess of line,
        POP     AF              ; Get status
        JP      Z,SETPTR        ; No text - Set up pointes
        LD      HL,(PROGND)     ; Get end of pogam
        EX      (SP),HL         ; Get length of input line
        POP     BC              ; End of pogam to BC
        ADD     HL,BC           ; Find new end
        PUSH    HL              ; Save new end
        CALL    MOVUP           ; Make space fo line
        POP     HL              ; Restoe new end
        LD      (PROGND),HL     ; Update end of pogam pointe
        EX      DE,HL           ; Get line to move up in HL
        LD      (HL),H          ; Save MSB
        POP     DE              ; Get new line numbe
        INC     HL              ; Skip pointe
        INC     HL
        LD      (HL),E          ; Save LSB of line numbe
        INC     HL
        LD      (HL),D          ; Save MSB of line numbe
        INC     HL              ; To fist byte in line
        LD      DE,BUFFER       ; Copy buffe to pogam
MOVBUF: LD      A,(DE)          ; Get souce
        LD      (HL),A          ; Save destinations
        INC     HL              ; Next souce
        INC     DE              ; Next destination
        OR      A               ; Done?
        JP      NZ,MOVBUF       ; No - Repeat
SETPTR: CALL    RUNFST          ; Set line pointes
        INC     HL              ; To LSB of pointe
        EX      DE,HL           ; Addess to DE
PTRLP:  LD      H,D             ; Addess to HL
        LD      L,E
        LD      A,(HL)          ; Get LSB of pointe
        INC     HL              ; To MSB of pointe
        OR      (HL)            ; Compae with MSB pointe
        JP      Z,GETCMD        ; Get command line if end
        INC     HL              ; To LSB of line numbe
        INC     HL              ; Skip line numbe
        INC     HL              ; Point to fist byte in line
        XOR     A               ; Looking fo 00 byte
FNDEND: CP      (HL)            ; Found end of line?
        INC     HL              ; Move to next byte
        JP      NZ,FNDEND       ; No - Keep looking
        EX      DE,HL           ; Next line addess to HL
        LD      (HL),E          ; Save LSB of pointe
        INC     HL
        LD      (HL),D          ; Save MSB of pointe
        JP      PTRLP           ; Do next line

SRCHLN: LD      HL,(BASTXT)     ; Stat of pogam text
SRCHLP: LD      B,H             ; BC = Addess to look at
        LD      C,L
        LD      A,(HL)          ; Get addess of next line
        INC     HL
        OR      (HL)            ; End of pogam found?
        DEC     HL
        RET     Z               ; Yes - Line not found
        INC     HL
        INC     HL
        LD      A,(HL)          ; Get LSB of line numbe
        INC     HL
        LD      H,(HL)          ; Get MSB of line numbe
        LD      L,A
        CALL    CPDEHL          ; Compae with line in DE
        LD      H,B             ; HL = Stat of this line
        LD      L,C
        LD      A,(HL)          ; Get LSB of next line addess
        INC     HL
        LD      H,(HL)          ; Get MSB of next line addess
        LD      L,A             ; Next line to HL
        CCF
        RET     Z               ; Lines found - Exit
        CCF
        RET     NC              ; Line not found,at line afte
        JP      SRCHLP          ; Keep looking

NEW:    RET     NZ              ; Retun if any moe on line
CLRPTR: LD      HL,(BASTXT)     ; Point to stat of pogam
        XOR     A               ; Set pogam aea to empty
        LD      (HL),A          ; Save LSB = 00
        INC     HL
        LD      (HL),A          ; Save MSB = 00
        INC     HL
        LD      (PROGND),HL     ; Set pogam end

RUNFST: LD      HL,(BASTXT)     ; Clea all vaiables
        DEC     HL

INTVAR: LD      (BRKLIN),HL     ; Initialise RUN vaiables
        LD      HL,(LSTRAM)     ; Get end of RAM
        LD      (STRBOT),HL     ; Clea sting space
        XOR     A
        CALL    RESTOR          ; Reset DATA pointes
        LD      HL,(PROGND)     ; Get end of pogam
        LD      (VAREND),HL     ; Clea vaiables
        LD      (ARREND),HL     ; Clea aays

CLREG:  POP     BC              ; Save etun addess
        LD      HL,(STRSPC)     ; Get end of woking RAN
        LD      SP,HL           ; Set stack
        LD      HL,TMSTPL       ; Tempoay sting pool
        LD      (TMSTPT),HL     ; Reset tempoay sting pt
        XOR     A               ; A = 00
        LD      L,A             ; HL = 0000
        LD      H,A
        LD      (CONTAD),HL     ; No CONTinue
        LD      (FORFLG),A      ; Clea FOR flag
        LD      (FNRGNM),HL     ; Clea FN agument
        PUSH    HL              ; HL = 0000
        PUSH    BC              ; Put back etun
DOAGN:  LD      HL,(BRKLIN)     ; Get addess of code to RUN
        RET                     ; Retun to execution dive

PROMPT: LD      A,'?'           ; '?'
        CALL    OUTC            ; Output chaacte
        LD      A,' '           ; Space
        CALL    OUTC            ; Output chaacte
        JP      RINPUT          ; Get input line

CRUNCH: XOR     A               ; Tokenise line @ HL to BUFFER
        LD      (DATFLG),A      ; Reset liteal flag
        LD      C,2+3           ; 2 byte numbe and 3 nulls
        LD      DE,BUFFER       ; Stat of input buffe
CRNCLP: LD      A,(HL)          ; Get byte
        CP      ' '             ; Is it a space?
        JP      Z,MOVDIR        ; Yes - Copy diect
        LD      B,A             ; Save chaacte
        CP      '"'             ; Is it a quote?
        JP      Z,CPYLIT        ; Yes - Copy liteal sting
        OR      A               ; Is it end of buffe?
        JP      Z,ENDBUF        ; Yes - End buffe
        LD      A,(DATFLG)      ; Get data type
        OR      A               ; Liteal?
        LD      A,(HL)          ; Get byte to copy
        JP      NZ,MOVDIR       ; Liteal - Copy diect
        CP      '?'             ; Is it '?' shot fo PRINT
        LD      A,ZPRINT        ; "PRINT" token
        JP      Z,MOVDIR        ; Yes - eplace it
        LD      A,(HL)          ; Get byte again
        CP      '0'             ; Is it less than '0'
        JP      C,FNDWRD        ; Yes - Look fo eseved wods
        CP      60; ";"+1           ; Is it "0123456789:;" ?
        JP      C,MOVDIR        ; Yes - copy it diect
FNDWRD: PUSH    DE              ; Look fo eseved wods
        LD      DE,WORDS-1      ; Point to table
        PUSH    BC              ; Save count
        LD      BC,RETNAD       ; Whee to etun to
        PUSH    BC              ; Save etun addess
        LD      B,ZEND-1        ; Fist token value -1
        LD      A,(HL)          ; Get byte
        CP      'a'             ; Less than 'a' ?
        JP      C,SEARCH        ; Yes - seach fo wods
        CP      'z'+1           ; Geate than 'z' ?
        JP      NC,SEARCH       ; Yes - seach fo wods
        AND     01011111B       ; Foce uppe case
        LD      (HL),A          ; Replace byte
SEARCH: LD      C,(HL)          ; Seach fo a wod
        EX      DE,HL
GETNXT: INC     HL              ; Get next eseved wod
        OR      (HL)            ; Stat of wod?
        JP      P,GETNXT        ; No - move on
        INC     B               ; Incement token value
        LD      A, (HL)         ; Get byte fom table
        AND     01111111B       ; Stip bit 7
        RET     Z               ; Retun if end of list
        CP      C               ; Same chaacte as in buffe?
        JP      NZ,GETNXT       ; No - get next wod
        EX      DE,HL
        PUSH    HL              ; Save stat of wod

NXTBYT: INC     DE              ; Look though est of wod
        LD      A,(DE)          ; Get byte fom table
        OR      A               ; End of wod ?
        JP      M,MATCH         ; Yes - Match found
        LD      C,A             ; Save it
        LD      A,B             ; Get token value
        CP      ZGOTO           ; Is it "GOTO" token ?
        JP      NZ,NOSPC        ; No - Don't allow spaces
        CALL    GETCHR          ; Get next chaacte
        DEC     HL              ; Cancel incement fom GETCHR
NOSPC:  INC     HL              ; Next byte
        LD      A,(HL)          ; Get byte
        CP      'a'             ; Less than 'a' ?
        JP      C,NOCHNG        ; Yes - don't change
        AND     01011111B       ; Make uppe case
NOCHNG: CP      C               ; Same as in buffe ?
        JP      Z,NXTBYT        ; Yes - keep testing
        POP     HL              ; Get back stat of wod
        JP      SEARCH          ; Look at next wod

MATCH:  LD      C,B             ; Wod found - Save token value
        POP     AF              ; Thow away etun
        EX      DE,HL
        RET                     ; Retun to "RETNAD"
RETNAD: EX      DE,HL           ; Get addess in sting
        LD      A,C             ; Get token value
        POP     BC              ; Restoe buffe length
        POP     DE              ; Get destination addess
MOVDIR: INC     HL              ; Next souce in buffe
        LD      (DE),A          ; Put byte in buffe
        INC     DE              ; Move up buffe
        INC     C               ; Incement length of buffe
        SUB     ':'             ; End of statement?
        JP      Z,SETLIT        ; Jump if multi-statement line
        CP      ZDATA-3AH       ; Is it DATA statement ?
        JP      NZ,TSTREM       ; No - see if REM
SETLIT: LD      (DATFLG),A      ; Set liteal flag
TSTREM: SUB     ZREM-3AH        ; Is it REM?
        JP      NZ,CRNCLP       ; No - Leave flag
        LD      B,A             ; Copy est of buffe
NXTCHR: LD      A,(HL)          ; Get byte
        OR      A               ; End of line ?
        JP      Z,ENDBUF        ; Yes - Teminate buffe
        CP      B               ; End of statement ?
        JP      Z,MOVDIR        ; Yes - Get next one
CPYLIT: INC     HL              ; Move up souce sting
        LD      (DE),A          ; Save in destination
        INC     C               ; Incement length
        INC     DE              ; Move up destination
        JP      NXTCHR          ; Repeat

ENDBUF: LD      HL,BUFFER-1     ; Point to stat of buffe
        LD      (DE),A          ; Mak end of buffe (A = 00)
        INC     DE
        LD      (DE),A          ; A = 00
        INC     DE
        LD      (DE),A          ; A = 00
        RET

DODEL:  LD      A,(NULFLG)      ; Get null flag status
        OR      A               ; Is it zeo?
        LD      A,0             ; Zeo A - Leave flags
        LD      (NULFLG),A      ; Zeo null flag
        JP      NZ,ECHDEL       ; Set - Echo it
        DEC     B               ; Decement length
        JP      Z,GETLIN        ; Get line again if empty
        CALL    OUTC            ; Output null chaacte
        .BYTE   3EH             ; Skip "DEC B"
ECHDEL: DEC     B               ; Count bytes in buffe
        DEC     HL              ; Back space buffe
        JP      Z,OTKLN         ; No buffe - Ty again
        LD      A,(HL)          ; Get deleted byte
        CALL    OUTC            ; Echo it
        JP      MORINP          ; Get moe input

DELCHR: DEC     B               ; Count bytes in buffe
        DEC     HL              ; Back space buffe
        CALL    OUTC            ; Output chaacte in A
        JP      NZ,MORINP       ; Not end - Get moe
OTKLN:  CALL    OUTC            ; Output chaacte in A
KILIN:  CALL    PRNTCRLF        ; Output CRLF
        JP      TTYLIN          ; Get line again

GETLIN:
TTYLIN: LD      HL,BUFFER       ; Get a line by chaacte
        LD      B,1             ; Set buffe as empty
        XOR     A
        LD      (NULFLG),A      ; Clea null flag
MORINP: CALL    CLOTST          ; Get chaacte and test ^O
        LD      C,A             ; Save chaacte in C
        CP      DEL             ; Delete chaacte?
        JP      Z,DODEL         ; Yes - Pocess it
        LD      A,(NULFLG)      ; Get null flag
        OR      A               ; Test null flag status
        JP      Z,PROCES        ; Reset - Pocess chaacte
        LD      A,0             ; Set a null
        CALL    OUTC            ; Output null
        XOR     A               ; Clea A
        LD      (NULFLG),A      ; Reset null flag
PROCES: LD      A,C             ; Get chaacte
        CP      CTRLG           ; Bell?
        JP      Z,PUTCTL        ; Yes - Save it
        CP      CTRLC           ; Is it contol "C"?
        CALL    Z,PRNTCRLF      ; Yes - Output CRLF
        SCF                     ; Flag beak
        RET     Z               ; Retun if contol "C"
        CP      CR              ; Is it ente?
        JP      Z,ENDINP        ; Yes - Teminate input
        CP      CTRLU           ; Is it contol "U"?
        JP      Z,KILIN         ; Yes - Get anothe line
        CP      '@'             ; Is it "kill line"?
        JP      Z,OTKLN         ; Yes - Kill line
        CP      '_'             ; Is it delete?
        JP      Z,DELCHR        ; Yes - Delete chaacte
        CP      BKSP            ; Is it backspace?
        JP      Z,DELCHR        ; Yes - Delete chaacte
        CP      CTRLR           ; Is it contol "R"?
        JP      NZ,PUTBUF       ; No - Put in buffe
        PUSH    BC              ; Save buffe length
        PUSH    DE              ; Save DE
        PUSH    HL              ; Save buffe addess
        LD      (HL),0          ; Mak end of buffe
        CALL    OUTNCR          ; Output and do CRLF
        LD      HL,BUFFER       ; Point to buffe stat
        CALL    PRS             ; Output buffe
        POP     HL              ; Restoe buffe addess
        POP     DE              ; Restoe DE
        POP     BC              ; Restoe buffe length
        JP      MORINP          ; Get anothe chaacte

PUTBUF: CP      ' '             ; Is it a contol code?
        JP      C,MORINP        ; Yes - Ignoe
PUTCTL: LD      A,B             ; Get numbe of bytes in buffe
        CP      72+1            ; Test fo line oveflow
        LD      A,CTRLG         ; Set a bell
        JP      NC,OUTNBS       ; Ring bell if buffe full
        LD      A,C             ; Get chaacte
        LD      (HL),C          ; Save in buffe
        LD      (LSTBIN),A      ; Save last input byte
        INC     HL              ; Move up buffe
        INC     B               ; Incement length
OUTIT:  CALL    OUTC            ; Output the chaacte enteed
        JP      MORINP          ; Get anothe chaacte

OUTNBS: CALL    OUTC            ; Output bell and back ove it
        LD      A,BKSP          ; Set back space
        JP      OUTIT           ; Output it and get moe

CPDEHL: LD      A,H             ; Get H
        SUB     D               ; Compae with D
        RET     NZ              ; Diffeent - Exit
        LD      A,L             ; Get L
        SUB     E               ; Compae with E
        RET                     ; Retun status

CHKSYN: LD      A,(HL)          ; Check syntax of chaacte
        EX      (SP),HL         ; Addess of test byte
        CP      (HL)            ; Same as in code sting?
        INC     HL              ; Retun addess
        EX      (SP),HL         ; Put it back
        JP      Z,GETCHR        ; Yes - Get next chaacte
        JP      SNERR           ; Diffeent - ?SN Eo

OUTC:   PUSH    AF              ; Save chaacte
        LD      A,(CTLOFG)      ; Get contol "O" flag
        OR      A               ; Is it set?
        JP      NZ,POPAF        ; Yes - don't output
        POP     AF              ; Restoe chaacte
        PUSH    BC              ; Save buffe length
        PUSH    AF              ; Save chaacte
        CP      ' '             ; Is it a contol code?
        JP      C,DINPOS        ; Yes - Don't INC POS(X)
        LD      A,(LWIDTH)      ; Get line width
        LD      B,A             ; To B
        LD      A,(CURPOS)      ; Get cuso position
        INC     B               ; Width 255?
        JP      Z,INCLEN        ; Yes - No width limit
        DEC     B               ; Restoe width
        CP      B               ; At end of line?
        CALL    Z,PRNTCRLF      ; Yes - output CRLF
INCLEN: INC     A               ; Move on one chaacte
        LD      (CURPOS),A      ; Save new position
DINPOS: POP     AF              ; Restoe chaacte
        POP     BC              ; Restoe buffe length
        CALL    MONOUT          ; Send it
        RET

CLOTST: CALL    GETINP          ; Get input chaacte
        AND     01111111B       ; Stip bit 7
        CP      CTRLO           ; Is it contol "O"?
        RET     NZ              ; No don't flip flag
        LD      A,(CTLOFG)      ; Get flag
        CPL                     ; Flip it
        LD      (CTLOFG),A      ; Put it back
        XOR     A               ; Null chaacte
        RET

LIST:   CALL    ATOH            ; ASCII numbe to DE
        RET     NZ              ; Retun if anything exta
        POP     BC              ; Rubbish - Not needed
        CALL    SRCHLN          ; Seach fo line numbe in DE
        PUSH    BC              ; Save addess of line
        CALL    SETLIN          ; Set up lines counte
LISTLP: POP     HL              ; Restoe addess of line
        LD      C,(HL)          ; Get LSB of next line
        INC     HL
        LD      B,(HL)          ; Get MSB of next line
        INC     HL
        LD      A,B             ; BC = 0 (End of pogam)?
        OR      C
        JP      Z,PRNTOK        ; Yes - Go to command mode
        CALL    COUNT           ; Count lines
        CALL    TSTBRK          ; Test fo beak key
        PUSH    BC              ; Save addess of next line
        CALL    PRNTCRLF        ; Output CRLF
        LD      E,(HL)          ; Get LSB of line numbe
        INC     HL
        LD      D,(HL)          ; Get MSB of line numbe
        INC     HL
        PUSH    HL              ; Save addess of line stat
        EX      DE,HL           ; Line numbe to HL
        CALL    PRNTHL          ; Output line numbe in decimal
        LD      A,' '           ; Space afte line numbe
        POP     HL              ; Restoe stat of line addess
LSTLP2: CALL    OUTC            ; Output chaacte in A
LSTLP3: LD      A,(HL)          ; Get next byte in line
        OR      A               ; End of line?
        INC     HL              ; To next byte in line
        JP      Z,LISTLP        ; Yes - get next line
        JP      P,LSTLP2        ; No token - output it
        SUB     ZEND-1          ; Find and output wod
        LD      C,A             ; Token offset+1 to C
        LD      DE,WORDS        ; Reseved wod list
FNDTOK: LD      A,(DE)          ; Get chaacte in list
        INC     DE              ; Move on to next
        OR      A               ; Is it stat of wod?
        JP      P,FNDTOK        ; No - Keep looking fo wod
        DEC     C               ; Count wods
        JP      NZ,FNDTOK       ; Not thee - keep looking
OUTWRD: AND     01111111B       ; Stip bit 7
        CALL    OUTC            ; Output fist chaacte
        LD      A,(DE)          ; Get next chaacte
        INC     DE              ; Move on to next
        OR      A               ; Is it end of wod?
        JP      P,OUTWRD        ; No - output the est
        JP      LSTLP3          ; Next byte in line

SETLIN: PUSH    HL              ; Set up LINES counte
        LD      HL,(LINESN)     ; Get LINES numbe
        LD      (LINESC),HL     ; Save in LINES counte
        POP     HL
        RET

COUNT:  PUSH    HL              ; Save code sting addess
        PUSH    DE
        LD      HL,(LINESC)     ; Get LINES counte
        LD      DE,-1
        ADC     HL,DE           ; Decement
        LD      (LINESC),HL     ; Put it back
        POP     DE
        POP     HL              ; Restoe code sting addess
        RET     P               ; Retun if moe lines to go
        PUSH    HL              ; Save code sting addess
        LD      HL,(LINESN)     ; Get LINES numbe
        LD      (LINESC),HL     ; Reset LINES counte
        CALL    GETINP          ; Get input chaacte
        CP      CTRLC           ; Is it contol "C"?
        JP      Z,RSLNBK        ; Yes - Reset LINES and beak
        POP     HL              ; Restoe code sting addess
        JP      COUNT           ; Keep on counting

RSLNBK: LD      HL,(LINESN)     ; Get LINES numbe
        LD      (LINESC),HL     ; Reset LINES counte
        JP      BRKRET          ; Go and output "Beak"

FOR:    LD      A,64H           ; Flag "FOR" assignment
        LD      (FORFLG),A      ; Save "FOR" flag
        CALL    LET             ; Set up initial index
        POP     BC              ; Dop RETun addess
        PUSH    HL              ; Save code sting addess
        CALL    DATA            ; Get next statement addess
        LD      (LOOPST),HL     ; Save it fo stat of loop
        LD      HL,2            ; Offset fo "FOR" block
        ADD     HL,SP           ; Point to it
FORSLP: CALL    LOKFOR          ; Look fo existing "FOR" block
        POP     DE              ; Get code sting addess
        JP      NZ,FORFND       ; No nesting found
        ADD     HL,BC           ; Move into "FOR" block
        PUSH    DE              ; Save code sting addess
        DEC     HL
        LD      D,(HL)          ; Get MSB of loop statement
        DEC     HL
        LD      E,(HL)          ; Get LSB of loop statement
        INC     HL
        INC     HL
        PUSH    HL              ; Save block addess
        LD      HL,(LOOPST)     ; Get addess of loop statement
        CALL    CPDEHL          ; Compae the FOR loops
        POP     HL              ; Restoe block addess
        JP      NZ,FORSLP       ; Diffeent FORs - Find anothe
        POP     DE              ; Restoe code sting addess
        LD      SP,HL           ; Remove all nested loops

FORFND: EX      DE,HL           ; Code sting addess to HL
        LD      C,8
        CALL    CHKSTK          ; Check fo 8 levels of stack
        PUSH    HL              ; Save code sting addess
        LD      HL,(LOOPST)     ; Get fist statement of loop
        EX      (SP),HL         ; Save and estoe code sting
        PUSH    HL              ; Re-save code sting addess
        LD      HL,(LINEAT)     ; Get cuent line numbe
        EX      (SP),HL         ; Save and estoe code sting
        CALL    TSTNUM          ; Make sue it's a numbe
        CALL    CHKSYN          ; Make sue "TO" is next
        .BYTE   ZTO          ; "TO" token
        CALL    GETNUM          ; Get "TO" expession value
        PUSH    HL              ; Save code sting addess
        CALL    BCDEFP          ; Move "TO" value to BCDE
        POP     HL              ; Restoe code sting addess
        PUSH    BC              ; Save "TO" value in block
        PUSH    DE
        LD      BC,8100H        ; BCDE - 1 (default STEP)
        LD      D,C             ; C=0
        LD      E,D             ; D=0
        LD      A,(HL)          ; Get next byte in code sting
        CP      ZSTEP           ; See if "STEP" is stated
        LD      A,1             ; Sign of step = 1
        JP      NZ,SAVSTP       ; No STEP given - Default to 1
        CALL    GETCHR          ; Jump ove "STEP" token
        CALL    GETNUM          ; Get step value
        PUSH    HL              ; Save code sting addess
        CALL    BCDEFP          ; Move STEP to BCDE
        CALL    TSTSGN          ; Test sign of FPREG
        POP     HL              ; Restoe code sting addess
SAVSTP: PUSH    BC              ; Save the STEP value in block
        PUSH    DE
        PUSH    AF              ; Save sign of STEP
        INC     SP              ; Don't save flags
        PUSH    HL              ; Save code sting addess
        LD      HL,(BRKLIN)     ; Get addess of index vaiable
        EX      (SP),HL         ; Save and estoe code sting
PUTFID: LD      B,ZFOR          ; "FOR" block make
        PUSH    BC              ; Save it
        INC     SP              ; Don't save C

RUNCNT: CALL    TSTBRK          ; Execution dive - Test beak
        LD      (BRKLIN),HL     ; Save code addess fo beak
        LD      A,(HL)          ; Get next byte in code sting
        CP      ':'             ; Multi statement line?
        JP      Z,EXCUTE        ; Yes - Execute it
        OR      A               ; End of line?
        JP      NZ,SNERR        ; No - Syntax eo
        INC     HL              ; Point to addess of next line
        LD      A,(HL)          ; Get LSB of line pointe
        INC     HL
        OR      (HL)            ; Is it zeo (End of pog)?
        JP      Z,ENDPRG        ; Yes - Teminate execution
        INC     HL              ; Point to line numbe
        LD      E,(HL)          ; Get LSB of line numbe
        INC     HL
        LD      D,(HL)          ; Get MSB of line numbe
        EX      DE,HL           ; Line numbe to HL
        LD      (LINEAT),HL     ; Save as cuent line numbe
        EX      DE,HL           ; Line numbe back to DE
EXCUTE: CALL    GETCHR          ; Get key wod
        LD      DE,RUNCNT       ; Whee to RETun to
        PUSH    DE              ; Save fo RETun
IFJMP:  RET     Z               ; Go to RUNCNT if end of STMT
ONJMP:  SUB     ZEND            ; Is it a token?
        JP      C,LET           ; No - ty to assign it
        CP      ZNEW+1-ZEND     ; END to NEW ?
        JP      NC,SNERR        ; Not a key wod - ?SN Eo
        RLCA                    ; Double it
        LD      C,A             ; BC = Offset into table
        LD      B,0
        EX      DE,HL           ; Save code sting addess
        LD      HL,WORDTB       ; Keywod addess table
        ADD     HL,BC           ; Point to outine addess
        LD      C,(HL)          ; Get LSB of outine addess
        INC     HL
        LD      B,(HL)          ; Get MSB of outine addess
        PUSH    BC              ; Save outine addess
        EX      DE,HL           ; Restoe code sting addess

GETCHR: INC     HL              ; Point to next chaacte
        LD      A,(HL)          ; Get next code sting byte
        CP      ':'             ; Z if ':'
        RET     NC              ; NC if > "9"
        CP      ' '
        JP      Z,GETCHR        ; Skip ove spaces
        CP      '0'
        CCF                     ; NC if < '0'
        INC     A               ; Test fo zeo - Leave cay
        DEC     A               ; Z if Null
        RET

RESTOR: EX      DE,HL           ; Save code sting addess
        LD      HL,(BASTXT)     ; Point to stat of pogam
        JP      Z,RESTNL        ; Just RESTORE - eset pointe
        EX      DE,HL           ; Restoe code sting addess
        CALL    ATOH            ; Get line numbe to DE
        PUSH    HL              ; Save code sting addess
        CALL    SRCHLN          ; Seach fo line numbe in DE
        LD      H,B             ; HL = Addess of line
        LD      L,C
        POP     DE              ; Restoe code sting addess
        JP      NC,ULERR        ; ?UL Eo if not found
RESTNL: DEC     HL              ; Byte befoe DATA statement
UPDATA: LD      (NXTDAT),HL     ; Update DATA pointe
        EX      DE,HL           ; Restoe code sting addess
        RET


TSTBRK: RST     18H             ; Check input status
        RET     Z               ; No key, go back
        RST     10H             ; Get the key into A
        CP      ESC             ; Escape key?
        JR      Z,BRK           ; Yes, beak
        CP      CTRLC           ; <Ctl-C>
        JR      Z,BRK           ; Yes, beak
        CP      CTRLS           ; Stop scolling?
        RET     NZ              ; Othe key, ignoe


STALL:  RST     10H             ; Wait fo key
        CP      CTRLQ           ; Resume scolling?
        RET      Z              ; Release the chokehold
        CP      CTRLC           ; Second beak?
        JR      Z,STOP          ; Beak duing hold exits pog
        JR      STALL           ; Loop until <Ctl-Q> o <bk>

BRK     LD      A,$FF           ; Set BRKFLG
        LD      (BRKFLG),A      ; Stoe it


STOP:   RET     NZ              ; Exit if anything else
        .BYTE   0F6H            ; Flag "STOP"
PEND:   RET     NZ              ; Exit if anything else
        LD      (BRKLIN),HL     ; Save point of beak
        .BYTE   21H             ; Skip "OR 11111111B"
INPBRK: OR      11111111B       ; Flag "Beak" wanted
        POP     BC              ; Retun not needed and moe
ENDPRG: LD      HL,(LINEAT)     ; Get cuent line numbe
        PUSH    AF              ; Save STOP / END status
        LD      A,L             ; Is it diect beak?
        AND     H
        INC     A               ; Line is -1 if diect beak
        JP      Z,NOLIN         ; Yes - No line numbe
        LD      (ERRLIN),HL     ; Save line of beak
        LD      HL,(BRKLIN)     ; Get point of beak
        LD      (CONTAD),HL     ; Save point to CONTinue
NOLIN:  XOR     A
        LD      (CTLOFG),A      ; Enable output
        CALL    STTLIN          ; Stat a new line
        POP     AF              ; Restoe STOP / END status
        LD      HL,BRKMSG       ; "Beak" message
        JP      NZ,ERRIN        ; "in line" wanted?
        JP      PRNTOK          ; Go to command mode

CONT:   LD      HL,(CONTAD)     ; Get CONTinue addess
        LD      A,H             ; Is it zeo?
        OR      L
        LD      E,CN            ; ?CN Eo
        JP      Z,ERROR         ; Yes - output "?CN Eo"
        EX      DE,HL           ; Save code sting addess
        LD      HL,(ERRLIN)     ; Get line of last beak
        LD      (LINEAT),HL     ; Set up cuent line numbe
        EX      DE,HL           ; Restoe code sting addess
        RET                     ; CONTinue whee left off

NULL:   CALL    GETINT          ; Get intege 0-255
        RET     NZ              ; Retun if bad value
        LD      (NULLS),A       ; Set nulls numbe
        RET


ACCSUM: PUSH    HL              ; Save addess in aay
        LD      HL,(CHKSUM)     ; Get check sum
        LD      B,0             ; BC - Value of byte
        LD      C,A
        ADD     HL,BC           ; Add byte to check sum
        LD      (CHKSUM),HL     ; Re-save check sum
        POP     HL              ; Restoe addess in aay
        RET

CHKLTR: LD      A,(HL)          ; Get byte
        CP      'A'             ; < 'a' ?
        RET     C               ; Cay set if not lette
        CP      'Z'+1           ; > 'z' ?
        CCF
        RET                     ; Cay set if not lette

FPSINT: CALL    GETCHR          ; Get next chaacte
POSINT: CALL    GETNUM          ; Get intege 0 to 32767
DEPINT: CALL    TSTSGN          ; Test sign of FPREG
        JP      M,FCERR         ; Negative - ?FC Eo
DEINT:  LD      A,(FPEXP)       ; Get intege value to DE
        CP      80H+16          ; Exponent in ange (16 bits)?
        JP      C,FPINT         ; Yes - convet it
        LD      BC,9080H        ; BCDE = -32768
        LD      DE,0000
        PUSH    HL              ; Save code sting addess
        CALL    CMPNUM          ; Compae FPREG with BCDE
        POP     HL              ; Restoe code sting addess
        LD      D,C             ; MSB to D
        RET     Z               ; Retun if in ange
FCERR:  LD      E,FC            ; ?FC Eo
        JP      ERROR           ; Output eo-

ATOH:   DEC     HL              ; ASCII numbe to DE binay
GETLN:  LD      DE,0            ; Get numbe to DE
GTLNLP: CALL    GETCHR          ; Get next chaacte
        RET     NC              ; Exit if not a digit
        PUSH    HL              ; Save code sting addess
        PUSH    AF              ; Save digit
        LD      HL,65529/10     ; Lagest numbe 65529
        CALL    CPDEHL          ; Numbe in ange?
        JP      C,SNERR         ; No - ?SN Eo
        LD      H,D             ; HL = Numbe
        LD      L,E
        ADD     HL,DE           ; Times 2
        ADD     HL,HL           ; Times 4
        ADD     HL,DE           ; Times 5
        ADD     HL,HL           ; Times 10
        POP     AF              ; Restoe digit
        SUB     '0'             ; Make it 0 to 9
        LD      E,A             ; DE = Value of digit
        LD      D,0
        ADD     HL,DE           ; Add to numbe
        EX      DE,HL           ; Numbe to DE
        POP     HL              ; Restoe code sting addess
        JP      GTLNLP          ; Go to next chaacte

CLEAR:  JP      Z,INTVAR        ; Just "CLEAR" Keep paametes
        CALL    POSINT          ; Get intege 0 to 32767 to DE
        DEC     HL              ; Cancel incement
        CALL    GETCHR          ; Get next chaacte
        PUSH    HL              ; Save code sting addess
        LD      HL,(LSTRAM)     ; Get end of RAM
        JP      Z,STORED        ; No value given - Use stoed
        POP     HL              ; Restoe code sting addess
        CALL    CHKSYN          ; Check fo comma
        .BYTE      ','
        PUSH    DE              ; Save numbe
        CALL    POSINT          ; Get intege 0 to 32767
        DEC     HL              ; Cancel incement
        CALL    GETCHR          ; Get next chaacte
        JP      NZ,SNERR        ; ?SN Eo if moe on line
        EX      (SP),HL         ; Save code sting addess
        EX      DE,HL           ; Numbe to DE
STORED: LD      A,L             ; Get LSB of new RAM top
        SUB     E               ; Subtact LSB of sting space
        LD      E,A             ; Save LSB
        LD      A,H             ; Get MSB of new RAM top
        SBC     A,D             ; Subtact MSB of sting space
        LD      D,A             ; Save MSB
        JP      C,OMERR         ; ?OM Eo if not enough mem
        PUSH    HL              ; Save RAM top
        LD      HL,(PROGND)     ; Get pogam end
        LD      BC,40           ; 40 Bytes minimum woking RAM
        ADD     HL,BC           ; Get lowest addess
        CALL    CPDEHL          ; Enough memoy?
        JP      NC,OMERR        ; No - ?OM Eo
        EX      DE,HL           ; RAM top to HL
        LD      (STRSPC),HL     ; Set new sting space
        POP     HL              ; End of memoy to use
        LD      (LSTRAM),HL     ; Set new top of RAM
        POP     HL              ; Restoe code sting addess
        JP      INTVAR          ; Initialise vaiables

RUN:    JP      Z,RUNFST        ; RUN fom stat if just RUN
        CALL    INTVAR          ; Initialise vaiables
        LD      BC,RUNCNT       ; Execution dive loop
        JP      RUNLIN          ; RUN fom line numbe

GOSUB:  LD      C,3             ; 3 Levels of stack needed
        CALL    CHKSTK          ; Check fo 3 levels of stack
        POP     BC              ; Get etun addess
        PUSH    HL              ; Save code sting fo RETURN
        PUSH    HL              ; And fo GOSUB outine
        LD      HL,(LINEAT)     ; Get cuent line
        EX      (SP),HL         ; Into stack - Code sting out
        LD      A,ZGOSUB        ; "GOSUB" token
        PUSH    AF              ; Save token
        INC     SP              ; Don't save flags

RUNLIN: PUSH    BC              ; Save etun addess
GOTO:   CALL    ATOH            ; ASCII numbe to DE binay
        CALL    REM             ; Get end of line
        PUSH    HL              ; Save end of line
        LD      HL,(LINEAT)     ; Get cuent line
        CALL    CPDEHL          ; Line afte cuent?
        POP     HL              ; Restoe end of line
        INC     HL              ; Stat of next line
        CALL    C,SRCHLP        ; Line is afte cuent line
        CALL    NC,SRCHLN       ; Line is befoe cuent line
        LD      H,B             ; Set up code sting addess
        LD      L,C
        DEC     HL              ; Incemented afte
        RET     C               ; Line found
ULERR:  LD      E,UL            ; ?UL Eo
        JP      ERROR           ; Output eo message

RETURN: RET     NZ              ; Retun if not just RETURN
        LD      D,-1            ; Flag "GOSUB" seach
        CALL    BAKSTK          ; Look "GOSUB" block
        LD      SP,HL           ; Kill all FORs in suboutine
        CP      ZGOSUB          ; Test fo "GOSUB" token
        LD      E,RG            ; ?RG Eo
        JP      NZ,ERROR        ; Eo if no "GOSUB" found
        POP     HL              ; Get RETURN line numbe
        LD      (LINEAT),HL     ; Save as cuent
        INC     HL              ; Was it fom diect statement?
        LD      A,H
        OR      L               ; Retun to line
        JP      NZ,RETLIN       ; No - Retun to line
        LD      A,(LSTBIN)      ; Any INPUT in suboutine?
        OR      A               ; If so buffe is coupted
        JP      NZ,POPNOK       ; Yes - Go to command mode
RETLIN: LD      HL,RUNCNT       ; Execution dive loop
        EX      (SP),HL         ; Into stack - Code sting out
        .BYTE      3EH             ; Skip "POP HL"
NXTDTA: POP     HL              ; Restoe code sting addess

DATA:   .BYTE      01H,3AH         ; ':' End of statement
REM:    LD      C,0             ; 00  End of statement
        LD      B,0
NXTSTL: LD      A,C             ; Statement and byte
        LD      C,B
        LD      B,A             ; Statement end byte
NXTSTT: LD      A,(HL)          ; Get byte
        OR      A               ; End of line?
        RET     Z               ; Yes - Exit
        CP      B               ; End of statement?
        RET     Z               ; Yes - Exit
        INC     HL              ; Next byte
        CP      '"'             ; Liteal sting?
        JP      Z,NXTSTL        ; Yes - Look fo anothe '"'
        JP      NXTSTT          ; Keep looking

LET:    CALL    GETVAR          ; Get vaiable name
        CALL    CHKSYN          ; Make sue "=" follows
        .BYTE      ZEQUAL          ; "=" token
        PUSH    DE              ; Save addess of vaiable
        LD      A,(TYPE)        ; Get data type
        PUSH    AF              ; Save type
        CALL    EVAL            ; Evaluate expession
        POP     AF              ; Restoe type
        EX      (SP),HL         ; Save code - Get va add
        LD      (BRKLIN),HL     ; Save addess of vaiable
        RRA                     ; Adjust type
        CALL    CHKTYP          ; Check types ae the same
        JP      Z,LETNUM        ; Numeic - Move value
LETSTR: PUSH    HL              ; Save addess of sting va
        LD      HL,(FPREG)      ; Pointe to sting enty
        PUSH    HL              ; Save it on stack
        INC     HL              ; Skip ove length
        INC     HL
        LD      E,(HL)          ; LSB of sting addess
        INC     HL
        LD      D,(HL)          ; MSB of sting addess
        LD      HL,(BASTXT)     ; Point to stat of pogam
        CALL    CPDEHL          ; Is sting befoe pogam?
        JP      NC,CRESTR       ; Yes - Ceate sting enty
        LD      HL,(STRSPC)     ; Point to sting space
        CALL    CPDEHL          ; Is sting liteal in pogam?
        POP     DE              ; Restoe addess of sting
        JP      NC,MVSTPT       ; Yes - Set up pointe
        LD      HL,TMPSTR       ; Tempoay sting pool
        CALL    CPDEHL          ; Is sting in tempoay pool?
        JP      NC,MVSTPT       ; No - Set up pointe
        .BYTE   3EH             ; Skip "POP DE"
CRESTR: POP     DE              ; Restoe addess of sting
        CALL    BAKTMP          ; Back to last tmp-st enty
        EX      DE,HL           ; Addess of sting enty
        CALL    SAVSTR          ; Save sting in sting aea
MVSTPT: CALL    BAKTMP          ; Back to last tmp-st enty
        POP     HL              ; Get sting pointe
        CALL    DETHL4          ; Move sting pointe to va
        POP     HL              ; Restoe code sting addess
        RET

LETNUM: PUSH    HL              ; Save addess of vaiable
        CALL    FPTHL           ; Move value to vaiable
        POP     DE              ; Restoe addess of vaiable
        POP     HL              ; Restoe code sting addess
        RET

ON:     CALL    GETINT          ; Get intege 0-255
        LD      A,(HL)          ; Get "GOTO" o "GOSUB" token
        LD      B,A             ; Save in B
        CP      ZGOSUB          ; "GOSUB" token?
        JP      Z,ONGO          ; Yes - Find line numbe
        CALL    CHKSYN          ; Make sue it's "GOTO"
        .BYTE   ZGOTO           ; "GOTO" token
        DEC     HL              ; Cancel incement
ONGO:   LD      C,E             ; Intege of banch value
ONGOLP: DEC     C               ; Count banches
        LD      A,B             ; Get "GOTO" o "GOSUB" token
        JP      Z,ONJMP         ; Go to that line if ight one
        CALL    GETLN           ; Get line numbe to DE
        CP      ','             ; Anothe line numbe?
        RET     NZ              ; No - Dop though
        JP      ONGOLP          ; Yes - loop

IF:     CALL    EVAL            ; Evaluate expession
        LD      A,(HL)          ; Get token
        CP      ZGOTO           ; "GOTO" token?
        JP      Z,IFGO          ; Yes - Get line
        CALL    CHKSYN          ; Make sue it's "THEN"
        .BYTE      ZTHEN           ; "THEN" token
        DEC     HL              ; Cancel incement
IFGO:   CALL    TSTNUM          ; Make sue it's numeic
        CALL    TSTSGN          ; Test state of expession
        JP      Z,REM           ; False - Dop though
        CALL    GETCHR          ; Get next chaacte
        JP      C,GOTO          ; Numbe - GOTO that line
        JP      IFJMP           ; Othewise do statement

MRPRNT: DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next chaacte
PRINT:  JP      Z,PRNTCRLF      ; CRLF if just PRINT
PRNTLP: RET     Z               ; End of list - Exit
        CP      ZTAB            ; "TAB(" token?
        JP      Z,DOTAB         ; Yes - Do TAB outine
        CP      ZSPC            ; "SPC(" token?
        JP      Z,DOTAB         ; Yes - Do SPC outine
        PUSH    HL              ; Save code sting addess
        CP      ','             ; Comma?
        JP      Z,DOCOM         ; Yes - Move to next zone
        CP      59 ;";"         ; Semi-colon?
        JP      Z,NEXITM        ; Do semi-colon outine
        POP     BC              ; Code sting addess to BC
        CALL    EVAL            ; Evaluate expession
        PUSH    HL              ; Save code sting addess
        LD      A,(TYPE)        ; Get vaiable type
        OR      A               ; Is it a sting vaiable?
        JP      NZ,PRNTST       ; Yes - Output sting contents
        CALL    NUMASC          ; Convet numbe to text
        CALL    CRTST           ; Ceate tempoay sting
        LD      (HL),' '        ; Followed by a space
        LD      HL,(FPREG)      ; Get length of output
        INC     (HL)            ; Plus 1 fo the space
        LD      HL,(FPREG)      ; < Not needed >
        LD      A,(LWIDTH)      ; Get width of line
        LD      B,A             ; To B
        INC     B               ; Width 255 (No limit)?
        JP      Z,PRNTNB        ; Yes - Output numbe sting
        INC     B               ; Adjust it
        LD      A,(CURPOS)      ; Get cuso position
        ADD     A,(HL)          ; Add length of sting
        DEC     A               ; Adjust it
        CP      B               ; Will output fit on this line?
        CALL    NC,PRNTCRLF     ; No - CRLF fist
PRNTNB: CALL    PRS1            ; Output sting at (HL)
        XOR     A               ; Skip CALL by setting 'z' flag
PRNTST: CALL    NZ,PRS1         ; Output sting at (HL)
        POP     HL              ; Restoe code sting addess
        JP      MRPRNT          ; See if moe to PRINT

STTLIN: LD      A,(CURPOS)      ; Make sue on new line
        OR      A               ; Aleady at stat?
        RET     Z               ; Yes - Do nothing
        JP      PRNTCRLF        ; Stat a new line

ENDINP: LD      (HL),0          ; Mak end of buffe
        LD      HL,BUFFER-1     ; Point to buffe
PRNTCRLF: LD    A,CR            ; Load a CR
        CALL    OUTC            ; Output chaacte
        LD      A,LF            ; Load a LF
        CALL    OUTC            ; Output chaacte
DONULL: XOR     A               ; Set to position 0
        LD      (CURPOS),A      ; Stoe it
        LD      A,(NULLS)       ; Get numbe of nulls
NULLP:  DEC     A               ; Count them
        RET     Z               ; Retun if done
        PUSH    AF              ; Save count
        XOR     A               ; Load a null
        CALL    OUTC            ; Output it
        POP     AF              ; Restoe count
        JP      NULLP           ; Keep counting

DOCOM:  LD      A,(COMMAN)      ; Get comma width
        LD      B,A             ; Save in B
        LD      A,(CURPOS)      ; Get cuent position
        CP      B               ; Within the limit?
        CALL    NC,PRNTCRLF     ; No - output CRLF
        JP      NC,NEXITM       ; Get next item
ZONELP: SUB     14              ; Next zone of 14 chaactes
        JP      NC,ZONELP       ; Repeat if moe zones
        CPL                     ; Numbe of spaces to output
        JP      ASPCS           ; Output them

DOTAB:  PUSH    AF              ; Save token
        CALL    FNDNUM          ; Evaluate expession
        CALL    CHKSYN          ; Make sue ")" follows
        .BYTE   ")"
        DEC     HL              ; Back space on to ")"
        POP     AF              ; Restoe token
        SUB     ZSPC            ; Was it "SPC(" ?
        PUSH    HL              ; Save code sting addess
        JP      Z,DOSPC         ; Yes - Do 'E' spaces
        LD      A,(CURPOS)      ; Get cuent position
DOSPC:  CPL                     ; Numbe of spaces to pint to
        ADD     A,E             ; Total numbe to pint
        JP      NC,NEXITM       ; TAB < Cuent POS(X)
ASPCS:  INC     A               ; Output A spaces
        LD      B,A             ; Save numbe to pint
        LD      A,' '           ; Space
SPCLP:  CALL    OUTC            ; Output chaacte in A
        DEC     B               ; Count them
        JP      NZ,SPCLP        ; Repeat if moe
NEXITM: POP     HL              ; Restoe code sting addess
        CALL    GETCHR          ; Get next chaacte
        JP      PRNTLP          ; Moe to pint

REDO:   .BYTE   "?Redo fom stat",CR,LF,0

BADINP: LD      A,(READFG)      ; READ o INPUT?
        OR      A
        JP      NZ,DATSNR       ; READ - ?SN Eo
        POP     BC              ; Thow away code sting add
        LD      HL,REDO         ; "Redo fom stat" message
        CALL    PRS             ; Output sting
        JP      DOAGN           ; Do last INPUT again

INPUT:  CALL    IDTEST          ; Test fo illegal diect
        LD      A,(HL)          ; Get chaacte afte "INPUT"
        CP      '"'             ; Is thee a pompt sting?
        LD      A,0             ; Clea A and leave flags
        LD      (CTLOFG),A      ; Enable output
        JP      NZ,NOPMPT       ; No pompt - get input
        CALL    QTSTR           ; Get sting teminated by '"'
        CALL    CHKSYN          ; Check fo ';' afte pompt
        .BYTE   ';'
        PUSH    HL              ; Save code sting addess
        CALL    PRS1            ; Output pompt sting
        .BYTE   3EH             ; Skip "PUSH HL"
NOPMPT: PUSH    HL              ; Save code sting addess
        CALL    PROMPT          ; Get input with "? " pompt
        POP     BC              ; Restoe code sting addess
        JP      C,INPBRK        ; Beak pessed - Exit
        INC     HL              ; Next byte
        LD      A,(HL)          ; Get it
        OR      A               ; End of line?
        DEC     HL              ; Back again
        PUSH    BC              ; Re-save code sting addess
        JP      Z,NXTDTA        ; Yes - Find next DATA stmt
        LD      (HL),','        ; Stoe comma as sepaato
        JP      NXTITM          ; Get next item

READ:   PUSH    HL              ; Save code sting addess
        LD      HL,(NXTDAT)     ; Next DATA statement
        .BYTE   0F6H            ; Flag "READ"
NXTITM: XOR     A               ; Flag "INPUT"
        LD      (READFG),A      ; Save "READ"/"INPUT" flag
        EX      (SP),HL         ; Get code st' , Save pointe
        JP      GTVLUS          ; Get values

NEDMOR: CALL    CHKSYN          ; Check fo comma between items
        .BYTE      ','
GTVLUS: CALL    GETVAR          ; Get vaiable name
        EX      (SP),HL         ; Save code st" , Get pointe
        PUSH    DE              ; Save vaiable addess
        LD      A,(HL)          ; Get next "INPUT"/"DATA" byte
        CP      ','             ; Comma?
        JP      Z,ANTVLU        ; Yes - Get anothe value
        LD      A,(READFG)      ; Is it READ?
        OR      A
        JP      NZ,FDTLP        ; Yes - Find next DATA stmt
        LD      A,'?'           ; Moe INPUT needed
        CALL    OUTC            ; Output chaacte
        CALL    PROMPT          ; Get INPUT with pompt
        POP     DE              ; Vaiable addess
        POP     BC              ; Code sting addess
        JP      C,INPBRK        ; Beak pessed
        INC     HL              ; Point to next DATA byte
        LD      A,(HL)          ; Get byte
        OR      A               ; Is it zeo (No input) ?
        DEC     HL              ; Back space INPUT pointe
        PUSH    BC              ; Save code sting addess
        JP      Z,NXTDTA        ; Find end of buffe
        PUSH    DE              ; Save vaiable addess
ANTVLU: LD      A,(TYPE)        ; Check data type
        OR      A               ; Is it numeic?
        JP      Z,INPBIN        ; Yes - Convet to binay
        CALL    GETCHR          ; Get next chaacte
        LD      D,A             ; Save input chaacte
        LD      B,A             ; Again
        CP      '"'             ; Stat of liteal sting?
        JP      Z,STRENT        ; Yes - Ceate sting enty
        LD      A,(READFG)      ; "READ" o "INPUT" ?
        OR      A
        LD      D,A             ; Save 00 if "INPUT"
        JP      Z,ITMSEP        ; "INPUT" - End with 00
        LD      D,':'           ; "DATA" - End with 00 o ':'
ITMSEP: LD      B,','           ; Item sepaato
        DEC     HL              ; Back space fo DTSTR
STRENT: CALL    DTSTR           ; Get sting teminated by D
        EX      DE,HL           ; Sting addess to DE
        LD      HL,LTSTND       ; Whee to go afte LETSTR
        EX      (SP),HL         ; Save HL , get input pointe
        PUSH    DE              ; Save addess of sting
        JP      LETSTR          ; Assign sting to vaiable

INPBIN: CALL    GETCHR          ; Get next chaacte
        CALL    ASCTFP          ; Convet ASCII to FP numbe
        EX      (SP),HL         ; Save input pt, Get va add
        CALL    FPTHL           ; Move FPREG to vaiable
        POP     HL              ; Restoe input pointe
LTSTND: DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next chaacte
        JP      Z,MORDT         ; End of line - Moe needed?
        CP      ','             ; Anothe value?
        JP      NZ,BADINP       ; No - Bad input
MORDT:  EX      (SP),HL         ; Get code sting addess
        DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next chaacte
        JP      NZ,NEDMOR       ; Moe needed - Get it
        POP     DE              ; Restoe DATA pointe
        LD      A,(READFG)      ; "READ" o "INPUT" ?
        OR      A
        EX      DE,HL           ; DATA pointe to HL
        JP      NZ,UPDATA       ; Update DATA pointe if "READ"
        PUSH    DE              ; Save code sting addess
        OR      (HL)            ; Moe input given?
        LD      HL,EXTIG        ; "?Exta ignoed" message
        CALL    NZ,PRS          ; Output sting if exta given
        POP     HL              ; Restoe code sting addess
        RET

EXTIG:  .BYTE   "?Exta ignoed",CR,LF,0

FDTLP:  CALL    DATA            ; Get next statement
        OR      A               ; End of line?
        JP      NZ,FANDT        ; No - See if DATA statement
        INC     HL
        LD      A,(HL)          ; End of pogam?
        INC     HL
        OR      (HL)            ; 00 00 Ends pogam
        LD      E,OD            ; ?OD Eo
        JP      Z,ERROR         ; Yes - Out of DATA
        INC     HL
        LD      E,(HL)          ; LSB of line numbe
        INC     HL
        LD      D,(HL)          ; MSB of line numbe
        EX      DE,HL
        LD      (DATLIN),HL     ; Set line of cuent DATA item
        EX      DE,HL
FANDT:  CALL    GETCHR          ; Get next chaacte
        CP      ZDATA           ; "DATA" token
        JP      NZ,FDTLP        ; No "DATA" - Keep looking
        JP      ANTVLU          ; Found - Convet input

NEXT:   LD      DE,0            ; In case no index given
NEXT1:  CALL    NZ,GETVAR       ; Get index addess
        LD      (BRKLIN),HL     ; Save code sting addess
        CALL    BAKSTK          ; Look fo "FOR" block
        JP      NZ,NFERR        ; No "FOR" - ?NF Eo
        LD      SP,HL           ; Clea nested loops
        PUSH    DE              ; Save index addess
        LD      A,(HL)          ; Get sign of STEP
        INC     HL
        PUSH    AF              ; Save sign of STEP
        PUSH    DE              ; Save index addess
        CALL    PHLTFP          ; Move index value to FPREG
        EX      (SP),HL         ; Save addess of TO value
        PUSH    HL              ; Save addess of index
        CALL    ADDPHL          ; Add STEP to index value
        POP     HL              ; Restoe addess of index
        CALL    FPTHL           ; Move value to index vaiable
        POP     HL              ; Restoe addess of TO value
        CALL    LOADFP          ; Move TO value to BCDE
        PUSH    HL              ; Save addess of line of FOR
        CALL    CMPNUM          ; Compae index with TO value
        POP     HL              ; Restoe addess of line num
        POP     BC              ; Addess of sign of STEP
        SUB     B               ; Compae with expected sign
        CALL    LOADFP          ; BC = Loop stmt,DE = Line num
        JP      Z,KILFOR        ; Loop finished - Teminate it
        EX      DE,HL           ; Loop statement line numbe
        LD      (LINEAT),HL     ; Set loop line numbe
        LD      L,C             ; Set code sting to loop
        LD      H,B
        JP      PUTFID          ; Put back "FOR" and continue

KILFOR: LD      SP,HL           ; Remove "FOR" block
        LD      HL,(BRKLIN)     ; Code sting afte "NEXT"
        LD      A,(HL)          ; Get next byte in code sting
        CP      ','             ; Moe NEXTs ?
        JP      NZ,RUNCNT       ; No - Do next statement
        CALL    GETCHR          ; Position to index name
        CALL    NEXT1           ; Re-ente NEXT outine
; < will not RETun to hee , Exit to RUNCNT o Loop >

GETNUM: CALL    EVAL            ; Get a numeic expession
TSTNUM: .BYTE      0F6H            ; Clea cay (numeic)
TSTSTR: SCF                     ; Set cay (sting)
CHKTYP: LD      A,(TYPE)        ; Check types match
        ADC     A,A             ; Expected + actual
        OR      A               ; Clea cay , set paity
        RET     PE              ; Even paity - Types match
        JP      TMERR           ; Diffeent types - Eo

OPNPAR: CALL    CHKSYN          ; Make sue "(" follows
        .BYTE   "("
EVAL:   DEC     HL              ; Evaluate expession & save
        LD      D,0             ; Pecedence value
EVAL1:  PUSH    DE              ; Save pecedence
        LD      C,1
        CALL    CHKSTK          ; Check fo 1 level of stack
        CALL    OPRND           ; Get next expession value
EVAL2:  LD      (NXTOPR),HL     ; Save addess of next opeato
EVAL3:  LD      HL,(NXTOPR)     ; Restoe addess of next op
        POP     BC              ; Pecedence value and opeato
        LD      A,B             ; Get pecedence value
        CP      78H             ; "AND" o "OR" ?
        CALL    NC,TSTNUM       ; No - Make sue it's a numbe
        LD      A,(HL)          ; Get next opeato / function
        LD      D,0             ; Clea Last elation
RLTLP:  SUB     ZGTR            ; ">" Token
        JP      C,FOPRND        ; + - * / ^ AND OR - Test it
        CP      ZLTH+1-ZGTR     ; < = >
        JP      NC,FOPRND       ; Function - Call it
        CP      ZEQUAL-ZGTR     ; "="
        RLA                     ; <- Test fo legal
        XOR     D               ; <- combinations of < = >
        CP      D               ; <- by combining last token
        LD      D,A             ; <- with cuent one
        JP      C,SNERR         ; Eo if "<<' '==" o ">>"
        LD      (CUROPR),HL     ; Save addess of cuent token
        CALL    GETCHR          ; Get next chaacte
        JP      RLTLP           ; Teat the two as one

FOPRND: LD      A,D             ; < = > found ?
        OR      A
        JP      NZ,TSTRED       ; Yes - Test fo eduction
        LD      A,(HL)          ; Get opeato token
        LD      (CUROPR),HL     ; Save opeato addess
        SUB     ZPLUS           ; Opeato o function?
        RET     C               ; Neithe - Exit
        CP      ZOR+1-ZPLUS     ; Is it + - * / ^ AND OR ?
        RET     NC              ; No - Exit
        LD      E,A             ; Coded opeato
        LD      A,(TYPE)        ; Get data type
        DEC     A               ; FF = numeic , 00 = sting
        OR      E               ; Combine with coded opeato
        LD      A,E             ; Get coded opeato
        JP      Z,CONCAT        ; Sting concatenation
        RLCA                    ; Times 2
        ADD     A,E             ; Times 3
        LD      E,A             ; To DE (D is 0)
        LD      HL,PRITAB       ; Pecedence table
        ADD     HL,DE           ; To the opeato concened
        LD      A,B             ; Last opeato pecedence
        LD      D,(HL)          ; Get evaluation pecedence
        CP      D               ; Compae with eval pecedence
        RET     NC              ; Exit if highe pecedence
        INC     HL              ; Point to outine addess
        CALL    TSTNUM          ; Make sue it's a numbe

STKTHS: PUSH    BC              ; Save last pecedence & token
        LD      BC,EVAL3        ; Whee to go on pec' beak
        PUSH    BC              ; Save on stack fo etun
        LD      B,E             ; Save opeato
        LD      C,D             ; Save pecedence
        CALL    STAKFP          ; Move value to stack
        LD      E,B             ; Restoe opeato
        LD      D,C             ; Restoe pecedence
        LD      C,(HL)          ; Get LSB of outine addess
        INC     HL
        LD      B,(HL)          ; Get MSB of outine addess
        INC     HL
        PUSH    BC              ; Save outine addess
        LD      HL,(CUROPR)     ; Addess of cuent opeato
        JP      EVAL1           ; Loop until pec' beak

OPRND:  XOR     A               ; Get opeand outine
        LD      (TYPE),A        ; Set numeic expected
        CALL    GETCHR          ; Get next chaacte
        LD      E,MO            ; ?MO Eo
        JP      Z,ERROR         ; No opeand - Eo
        JP      C,ASCTFP        ; Numbe - Get value
        CALL    CHKLTR          ; See if a lette
        JP      NC,CONVAR       ; Lette - Find vaiable
        CP		'&'				; &H = HEX, &B = BINARY
        JR		NZ, NOTAMP
        CALL    GETCHR          ; Get next chaacte
        CP      'H'             ; Hex numbe indicated? [function added]
        JP      Z,HEXTFP        ; Convet Hex to FPREG
        CP      'B'             ; Binay numbe indicated? [function added]
        JP      Z,BINTFP        ; Convet Bin to FPREG
        LD      E,SN            ; If neithe then a ?SN Eo
        JP      Z,ERROR         ; 
NOTAMP: CP      ZPLUS           ; '+' Token ?
        JP      Z,OPRND         ; Yes - Look fo opeand
        CP      '.'             ; '.' ?
        JP      Z,ASCTFP        ; Yes - Ceate FP numbe
        CP      ZMINUS          ; '-' Token ?
        JP      Z,MINUS         ; Yes - Do minus
        CP      '"'             ; Liteal sting ?
        JP      Z,QTSTR         ; Get sting teminated by '"'
        CP      ZNOT            ; "NOT" Token ?
        JP      Z,EVNOT         ; Yes - Eval NOT expession
        CP      ZFN             ; "FN" Token ?
        JP      Z,DOFN          ; Yes - Do FN outine
        SUB     ZSGN            ; Is it a function?
        JP      NC,FNOFST       ; Yes - Evaluate function
EVLPAR: CALL    OPNPAR          ; Evaluate expession in "()"
        CALL    CHKSYN          ; Make sue ")" follows
        .BYTE   ")"
        RET

MINUS:  LD      D,7DH           ; '-' pecedence
        CALL    EVAL1           ; Evaluate until pec' beak
        LD      HL,(NXTOPR)     ; Get next opeato addess
        PUSH    HL              ; Save next opeato addess
        CALL    INVSGN          ; Negate value
RETNUM: CALL    TSTNUM          ; Make sue it's a numbe
        POP     HL              ; Restoe next opeato addess
        RET

CONVAR: CALL    GETVAR          ; Get vaiable addess to DE
FRMEVL: PUSH    HL              ; Save code sting addess
        EX      DE,HL           ; Vaiable addess to HL
        LD      (FPREG),HL      ; Save addess of vaiable
        LD      A,(TYPE)        ; Get type
        OR      A               ; Numeic?
        CALL    Z,PHLTFP        ; Yes - Move contents to FPREG
        POP     HL              ; Restoe code sting addess
        RET

FNOFST: LD      B,0             ; Get addess of function
        RLCA                    ; Double function offset
        LD      C,A             ; BC = Offset in function table
        PUSH    BC              ; Save adjusted token value
        CALL    GETCHR          ; Get next chaacte
        LD      A,C             ; Get adjusted token value
        CP      2*(ZLEFT-ZSGN)-1; Adj' LEFT$,RIGHT$ o MID$ ?
        JP      C,FNVAL         ; No - Do function
        CALL    OPNPAR          ; Evaluate expession  (X,...
        CALL    CHKSYN          ; Make sue ',' follows
        .BYTE      ','
        CALL    TSTSTR          ; Make sue it's a sting
        EX      DE,HL           ; Save code sting addess
        LD      HL,(FPREG)      ; Get addess of sting
        EX      (SP),HL         ; Save addess of sting
        PUSH    HL              ; Save adjusted token value
        EX      DE,HL           ; Restoe code sting addess
        CALL    GETINT          ; Get intege 0-255
        EX      DE,HL           ; Save code sting addess
        EX      (SP),HL         ; Save intege,HL = adj' token
        JP      GOFUNC          ; Jump to sting function

FNVAL:  CALL    EVLPAR          ; Evaluate expession
        EX      (SP),HL         ; HL = Adjusted token value
        LD      DE,RETNUM       ; Retun numbe fom function
        PUSH    DE              ; Save on stack
GOFUNC: LD      BC,FNCTAB       ; Function outine addesses
        ADD     HL,BC           ; Point to ight addess
        LD      C,(HL)          ; Get LSB of addess
        INC     HL              ;
        LD      H,(HL)          ; Get MSB of addess
        LD      L,C             ; Addess to HL
        JP      (HL)            ; Jump to function

SGNEXP: DEC     D               ; Dee to flag negative exponent
        CP      ZMINUS          ; '-' token ?
        RET     Z               ; Yes - Retun
        CP      '-'             ; '-' ASCII ?
        RET     Z               ; Yes - Retun
        INC     D               ; Inc to flag positive exponent
        CP      '+'             ; '+' ASCII ?
        RET     Z               ; Yes - Retun
        CP      ZPLUS           ; '+' token ?
        RET     Z               ; Yes - Retun
        DEC     HL              ; DEC 'cos GETCHR INCs
        RET                     ; Retun "NZ"

POR:    .BYTE      0F6H            ; Flag "OR"
PAND:   XOR     A               ; Flag "AND"
        PUSH    AF              ; Save "AND" / "OR" flag
        CALL    TSTNUM          ; Make sue it's a numbe
        CALL    DEINT           ; Get intege -32768 to 32767
        POP     AF              ; Restoe "AND" / "OR" flag
        EX      DE,HL           ; <- Get last
        POP     BC              ; <-  value
        EX      (SP),HL         ; <-  fom
        EX      DE,HL           ; <-  stack
        CALL    FPBCDE          ; Move last value to FPREG
        PUSH    AF              ; Save "AND" / "OR" flag
        CALL    DEINT           ; Get intege -32768 to 32767
        POP     AF              ; Restoe "AND" / "OR" flag
        POP     BC              ; Get value
        LD      A,C             ; Get LSB
        LD      HL,ACPASS       ; Addess of save AC as cuent
        JP      NZ,POR1         ; Jump if OR
        AND     E               ; "AND" LSBs
        LD      C,A             ; Save LSB
        LD      A,B             ; Get MBS
        AND     D               ; "AND" MSBs
        JP      (HL)            ; Save AC as cuent (ACPASS)

POR1:   OR      E               ; "OR" LSBs
        LD      C,A             ; Save LSB
        LD      A,B             ; Get MSB
        OR      D               ; "OR" MSBs
        JP      (HL)            ; Save AC as cuent (ACPASS)

TSTRED: LD      HL,CMPLOG       ; Logical compae outine
        LD      A,(TYPE)        ; Get data type
        RRA                     ; Cay set = sting
        LD      A,D             ; Get last pecedence value
        RLA                     ; Times 2 plus cay
        LD      E,A             ; To E
        LD      D,64H           ; Relational pecedence
        LD      A,B             ; Get cuent pecedence
        CP      D               ; Compae with last
        RET     NC              ; Eval if last was el' o log'
        JP      STKTHS          ; Stack this one and get next

CMPLOG: .WORD   CMPLG1          ; Compae two values / stings
CMPLG1: LD      A,C             ; Get data type
        OR      A
        RRA
        POP     BC              ; Get last expession to BCDE
        POP     DE
        PUSH    AF              ; Save status
        CALL    CHKTYP          ; Check that types match
        LD      HL,CMPRES       ; Result to compaison
        PUSH    HL              ; Save fo RETun
        JP      Z,CMPNUM        ; Compae values if numeic
        XOR     A               ; Compae two stings
        LD      (TYPE),A        ; Set type to numeic
        PUSH    DE              ; Save sting name
        CALL    GSTRCU          ; Get cuent sting
        LD      A,(HL)          ; Get length of sting
        INC     HL
        INC     HL
        LD      C,(HL)          ; Get LSB of addess
        INC     HL
        LD      B,(HL)          ; Get MSB of addess
        POP     DE              ; Restoe sting name
        PUSH    BC              ; Save addess of sting
        PUSH    AF              ; Save length of sting
        CALL    GSTRDE          ; Get second sting
        CALL    LOADFP          ; Get addess of second sting
        POP     AF              ; Restoe length of sting 1
        LD      D,A             ; Length to D
        POP     HL              ; Restoe addess of sting 1
CMPSTR: LD      A,E             ; Bytes of sting 2 to do
        OR      D               ; Bytes of sting 1 to do
        RET     Z               ; Exit if all bytes compaed
        LD      A,D             ; Get bytes of sting 1 to do
        SUB     1
        RET     C               ; Exit if end of sting 1
        XOR     A
        CP      E               ; Bytes of sting 2 to do
        INC     A
        RET     NC              ; Exit if end of sting 2
        DEC     D               ; Count bytes in sting 1
        DEC     E               ; Count bytes in sting 2
        LD      A,(BC)          ; Byte in sting 2
        CP      (HL)            ; Compae to byte in sting 1
        INC     HL              ; Move up sting 1
        INC     BC              ; Move up sting 2
        JP      Z,CMPSTR        ; Same - Ty next bytes
        CCF                     ; Flag diffeence (">" o "<")
        JP      FLGDIF          ; "<" gives -1 , ">" gives +1

CMPRES: INC     A               ; Incement cuent value
        ADC     A,A             ; Double plus cay
        POP     BC              ; Get othe value
        AND     B               ; Combine them
        ADD     A,-1            ; Cay set if diffeent
        SBC     A,A             ; 00 - Equal , FF - Diffeent
        JP      FLGREL          ; Set cuent value & continue

EVNOT:  LD      D,5AH           ; Pecedence value fo "NOT"
        CALL    EVAL1           ; Eval until pecedence beak
        CALL    TSTNUM          ; Make sue it's a numbe
        CALL    DEINT           ; Get intege -32768 - 32767
        LD      A,E             ; Get LSB
        CPL                     ; Invet LSB
        LD      C,A             ; Save "NOT" of LSB
        LD      A,D             ; Get MSB
        CPL                     ; Invet MSB
        CALL    ACPASS          ; Save AC as cuent
        POP     BC              ; Clean up stack
        JP      EVAL3           ; Continue evaluation

DIMRET: DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next chaacte
        RET     Z               ; End of DIM statement
        CALL    CHKSYN          ; Make sue ',' follows
        .BYTE      ','
DIM:    LD      BC,DIMRET       ; Retun to "DIMRET"
        PUSH    BC              ; Save on stack
        .BYTE      0F6H            ; Flag "Ceate" vaiable
GETVAR: XOR     A               ; Find vaiable addess,to DE
        LD      (LCRFLG),A      ; Set locate / ceate flag
        LD      B,(HL)          ; Get Fist byte of name
GTFNAM: CALL    CHKLTR          ; See if a lette
        JP      C,SNERR         ; ?SN Eo if not a lette
        XOR     A
        LD      C,A             ; Clea second byte of name
        LD      (TYPE),A        ; Set type to numeic
        CALL    GETCHR          ; Get next chaacte
        JP      C,SVNAM2        ; Numeic - Save in name
        CALL    CHKLTR          ; See if a lette
        JP      C,CHARTY        ; Not a lette - Check type
SVNAM2: LD      C,A             ; Save second byte of name
ENDNAM: CALL    GETCHR          ; Get next chaacte
        JP      C,ENDNAM        ; Numeic - Get anothe
        CALL    CHKLTR          ; See if a lette
        JP      NC,ENDNAM       ; Lette - Get anothe
CHARTY: SUB     '$'             ; Sting vaiable?
        JP      NZ,NOTSTR       ; No - Numeic vaiable
        INC     A               ; A = 1 (sting type)
        LD      (TYPE),A        ; Set type to sting
        RRCA                    ; A = 80H , Flag fo sting
        ADD     A,C             ; 2nd byte of name has bit 7 on
        LD      C,A             ; Resave second byte on name
        CALL    GETCHR          ; Get next chaacte
NOTSTR: LD      A,(FORFLG)      ; Aay name needed ?
        DEC     A
        JP      Z,ARLDSV        ; Yes - Get aay name
        JP      P,NSCFOR        ; No aay with "FOR" o "FN"
        LD      A,(HL)          ; Get byte again
        SUB     '('             ; Subscipted vaiable?
        JP      Z,SBSCPT        ; Yes - Sot out subscipt

NSCFOR: XOR     A               ; Simple vaiable
        LD      (FORFLG),A      ; Clea "FOR" flag
        PUSH    HL              ; Save code sting addess
        LD      D,B             ; DE = Vaiable name to find
        LD      E,C
        LD      HL,(FNRGNM)     ; FN agument name
        CALL    CPDEHL          ; Is it the FN agument?
        LD      DE,FNARG        ; Point to agument value
        JP      Z,POPHRT        ; Yes - Retun FN agument value
        LD      HL,(VAREND)     ; End of vaiables
        EX      DE,HL           ; Addess of end of seach
        LD      HL,(PROGND)     ; Stat of vaiables addess
FNDVAR: CALL    CPDEHL          ; End of vaiable list table?
        JP      Z,CFEVAL        ; Yes - Called fom EVAL?
        LD      A,C             ; Get second byte of name
        SUB     (HL)            ; Compae with name in list
        INC     HL              ; Move on to fist byte
        JP      NZ,FNTHR        ; Diffeent - Find anothe
        LD      A,B             ; Get fist byte of name
        SUB     (HL)            ; Compae with name in list
FNTHR:  INC     HL              ; Move on to LSB of value
        JP      Z,RETADR        ; Found - Retun addess
        INC     HL              ; <- Skip
        INC     HL              ; <- ove
        INC     HL              ; <- F.P.
        INC     HL              ; <- value
        JP      FNDVAR          ; Keep looking

CFEVAL: POP     HL              ; Restoe code sting addess
        EX      (SP),HL         ; Get etun addess
        PUSH    DE              ; Save addess of vaiable
        LD      DE,FRMEVL       ; Retun addess in EVAL
        CALL    CPDEHL          ; Called fom EVAL ?
        POP     DE              ; Restoe addess of vaiable
        JP      Z,RETNUL        ; Yes - Retun null vaiable
        EX      (SP),HL         ; Put back etun
        PUSH    HL              ; Save code sting addess
        PUSH    BC              ; Save vaiable name
        LD      BC,6            ; 2 byte name plus 4 byte data
        LD      HL,(ARREND)     ; End of aays
        PUSH    HL              ; Save end of aays
        ADD     HL,BC           ; Move up 6 bytes
        POP     BC              ; Souce addess in BC
        PUSH    HL              ; Save new end addess
        CALL    MOVUP           ; Move aays up
        POP     HL              ; Restoe new end addess
        LD      (ARREND),HL     ; Set new end addess
        LD      H,B             ; End of vaiables to HL
        LD      L,C
        LD      (VAREND),HL     ; Set new end addess

ZEROLP: DEC     HL              ; Back though to zeo vaiable
        LD      (HL),0          ; Zeo byte in vaiable
        CALL    CPDEHL          ; Done them all?
        JP      NZ,ZEROLP       ; No - Keep on going
        POP     DE              ; Get vaiable name
        LD      (HL),E          ; Stoe second chaacte
        INC     HL
        LD      (HL),D          ; Stoe fist chaacte
        INC     HL
RETADR: EX      DE,HL           ; Addess of vaiable in DE
        POP     HL              ; Restoe code sting addess
        RET

RETNUL: LD      (FPEXP),A       ; Set esult to zeo
        LD      HL,ZERBYT       ; Also set a null sting
        LD      (FPREG),HL      ; Save fo EVAL
        POP     HL              ; Restoe code sting addess
        RET

SBSCPT: PUSH    HL              ; Save code sting addess
        LD      HL,(LCRFLG)     ; Locate/Ceate and Type
        EX      (SP),HL         ; Save and get code sting
        LD      D,A             ; Zeo numbe of dimensions
SCPTLP: PUSH    DE              ; Save numbe of dimensions
        PUSH    BC              ; Save aay name
        CALL    FPSINT          ; Get subscipt (0-32767)
        POP     BC              ; Restoe aay name
        POP     AF              ; Get numbe of dimensions
        EX      DE,HL
        EX      (SP),HL         ; Save subscipt value
        PUSH    HL              ; Save LCRFLG and TYPE
        EX      DE,HL
        INC     A               ; Count dimensions
        LD      D,A             ; Save in D
        LD      A,(HL)          ; Get next byte in code sting
        CP      ','             ; Comma (moe to come)?
        JP      Z,SCPTLP        ; Yes - Moe subscipts
        CALL    CHKSYN          ; Make sue ")" follows
        .BYTE      ")"
        LD      (NXTOPR),HL     ; Save code sting addess
        POP     HL              ; Get LCRFLG and TYPE
        LD      (LCRFLG),HL     ; Restoe Locate/ceate & type
        LD      E,0             ; Flag not CSAVE* o CLOAD*
        PUSH    DE              ; Save numbe of dimensions (D)
        .BYTE      11H             ; Skip "PUSH HL" and "PUSH AF'

ARLDSV: PUSH    HL              ; Save code sting addess
        PUSH    AF              ; A = 00 , Flags set = Z,N
        LD      HL,(VAREND)     ; Stat of aays
        .BYTE      3EH             ; Skip "ADD HL,DE"
FNDARY: ADD     HL,DE           ; Move to next aay stat
        EX      DE,HL
        LD      HL,(ARREND)     ; End of aays
        EX      DE,HL           ; Cuent aay pointe
        CALL    CPDEHL          ; End of aays found?
        JP      Z,CREARY        ; Yes - Ceate aay
        LD      A,(HL)          ; Get second byte of name
        CP      C               ; Compae with name given
        INC     HL              ; Move on
        JP      NZ,NXTARY       ; Diffeent - Find next aay
        LD      A,(HL)          ; Get fist byte of name
        CP      B               ; Compae with name given
NXTARY: INC     HL              ; Move on
        LD      E,(HL)          ; Get LSB of next aay addess
        INC     HL
        LD      D,(HL)          ; Get MSB of next aay addess
        INC     HL
        JP      NZ,FNDARY       ; Not found - Keep looking
        LD      A,(LCRFLG)      ; Found Locate o Ceate it?
        OR      A
        JP      NZ,DDERR        ; Ceate - ?DD Eo
        POP     AF              ; Locate - Get numbe of dim'ns
        LD      B,H             ; BC Points to aay dim'ns
        LD      C,L
        JP      Z,POPHRT        ; Jump if aay load/save
        SUB     (HL)            ; Same numbe of dimensions?
        JP      Z,FINDEL        ; Yes - Find element
BSERR:  LD      E,BS            ; ?BS Eo
        JP      ERROR           ; Output eo

CREARY: LD      DE,4            ; 4 Bytes pe enty
        POP     AF              ; Aay to save o 0 dim'ns?
        JP      Z,FCERR         ; Yes - ?FC Eo
        LD      (HL),C          ; Save second byte of name
        INC     HL
        LD      (HL),B          ; Save fist byte of name
        INC     HL
        LD      C,A             ; Numbe of dimensions to C
        CALL    CHKSTK          ; Check if enough memoy
        INC     HL              ; Point to numbe of dimensions
        INC     HL
        LD      (CUROPR),HL     ; Save addess of pointe
        LD      (HL),C          ; Set numbe of dimensions
        INC     HL
        LD      A,(LCRFLG)      ; Locate of Ceate?
        RLA                     ; Cay set = Ceate
        LD      A,C             ; Get numbe of dimensions
CRARLP: LD      BC,10+1         ; Default dimension size 10
        JP      NC,DEFSIZ       ; Locate - Set default size
        POP     BC              ; Get specified dimension size
        INC     BC              ; Include zeo element
DEFSIZ: LD      (HL),C          ; Save LSB of dimension size
        INC     HL
        LD      (HL),B          ; Save MSB of dimension size
        INC     HL
        PUSH    AF              ; Save num' of dim'ns an status
        PUSH    HL              ; Save addess of dim'n size
        CALL    MLDEBC          ; Multiply DE by BC to find
        EX      DE,HL           ; amount of mem needed (to DE)
        POP     HL              ; Restoe addess of dimension
        POP     AF              ; Restoe numbe of dimensions
        DEC     A               ; Count them
        JP      NZ,CRARLP       ; Do next dimension if moe
        PUSH    AF              ; Save locate/ceate flag
        LD      B,D             ; MSB of memoy needed
        LD      C,E             ; LSB of memoy needed
        EX      DE,HL
        ADD     HL,DE           ; Add bytes to aay stat
        JP      C,OMERR         ; Too big - Eo
        CALL    ENFMEM          ; See if enough memoy
        LD      (ARREND),HL     ; Save new end of aay

ZERARY: DEC     HL              ; Back though aay data
        LD      (HL),0          ; Set aay element to zeo
        CALL    CPDEHL          ; All elements zeoed?
        JP      NZ,ZERARY       ; No - Keep on going
        INC     BC              ; Numbe of bytes + 1
        LD      D,A             ; A=0
        LD      HL,(CUROPR)     ; Get addess of aay
        LD      E,(HL)          ; Numbe of dimensions
        EX      DE,HL           ; To HL
        ADD     HL,HL           ; Two bytes pe dimension size
        ADD     HL,BC           ; Add numbe of bytes
        EX      DE,HL           ; Bytes needed to DE
        DEC     HL
        DEC     HL
        LD      (HL),E          ; Save LSB of bytes needed
        INC     HL
        LD      (HL),D          ; Save MSB of bytes needed
        INC     HL
        POP     AF              ; Locate / Ceate?
        JP      C,ENDDIM        ; A is 0 , End if ceate
FINDEL: LD      B,A             ; Find aay element
        LD      C,A
        LD      A,(HL)          ; Numbe of dimensions
        INC     HL
        .BYTE      16H             ; Skip "POP HL"
FNDELP: POP     HL              ; Addess of next dim' size
        LD      E,(HL)          ; Get LSB of dim'n size
        INC     HL
        LD      D,(HL)          ; Get MSB of dim'n size
        INC     HL
        EX      (SP),HL         ; Save addess - Get index
        PUSH    AF              ; Save numbe of dim'ns
        CALL    CPDEHL          ; Dimension too lage?
        JP      NC,BSERR        ; Yes - ?BS Eo
        PUSH    HL              ; Save index
        CALL    MLDEBC          ; Multiply pevious by size
        POP     DE              ; Index supplied to DE
        ADD     HL,DE           ; Add index to pointe
        POP     AF              ; Numbe of dimensions
        DEC     A               ; Count them
        LD      B,H             ; MSB of pointe
        LD      C,L             ; LSB of pointe
        JP      NZ,FNDELP       ; Moe - Keep going
        ADD     HL,HL           ; 4 Bytes pe element
        ADD     HL,HL
        POP     BC              ; Stat of aay
        ADD     HL,BC           ; Point to element
        EX      DE,HL           ; Addess of element to DE
ENDDIM: LD      HL,(NXTOPR)     ; Got code sting addess
        RET

FRE:    LD      HL,(ARREND)     ; Stat of fee memoy
        EX      DE,HL           ; To DE
        LD      HL,0            ; End of fee memoy
        ADD     HL,SP           ; Cuent stack value
        LD      A,(TYPE)        ; Dummy agument type
        OR      A
        JP      Z,FRENUM        ; Numeic - Fee vaiable space
        CALL    GSTRCU          ; Cuent sting to pool
        CALL    GARBGE          ; Gabage collection
        LD      HL,(STRSPC)     ; Bottom of sting space in use
        EX      DE,HL           ; To DE
        LD      HL,(STRBOT)     ; Bottom of sting space
FRENUM: LD      A,L             ; Get LSB of end
        SUB     E               ; Subtact LSB of beginning
        LD      C,A             ; Save diffeence if C
        LD      A,H             ; Get MSB of end
        SBC     A,D             ; Subtact MSB of beginning
ACPASS: LD      B,C             ; Retun intege AC
ABPASS: LD      D,B             ; Retun intege AB
        LD      E,0
        LD      HL,TYPE         ; Point to type
        LD      (HL),E          ; Set type to numeic
        LD      B,80H+16        ; 16 bit intege
        JP      RETINT          ; Retun the integ

POS:    LD      A,(CURPOS)      ; Get cuso position
PASSA:  LD      B,A             ; Put A into AB
        XOR     A               ; Zeo A
        JP      ABPASS          ; Retun intege AB

DEF:    CALL    CHEKFN          ; Get "FN" and name
        CALL    IDTEST          ; Test fo illegal diect
        LD      BC,DATA         ; To get next statement
        PUSH    BC              ; Save addess fo RETun
        PUSH    DE              ; Save addess of function pt
        CALL    CHKSYN          ; Make sue "(" follows
        .BYTE      "("
        CALL    GETVAR          ; Get agument vaiable name
        PUSH    HL              ; Save code sting addess
        EX      DE,HL           ; Agument addess to HL
        DEC     HL
        LD      D,(HL)          ; Get fist byte of ag name
        DEC     HL
        LD      E,(HL)          ; Get second byte of ag name
        POP     HL              ; Restoe code sting addess
        CALL    TSTNUM          ; Make sue numeic agument
        CALL    CHKSYN          ; Make sue ")" follows
        .BYTE      ")"
        CALL    CHKSYN          ; Make sue "=" follows
        .BYTE      ZEQUAL          ; "=" token
        LD      B,H             ; Code sting addess to BC
        LD      C,L
        EX      (SP),HL         ; Save code st , Get FN pt
        LD      (HL),C          ; Save LSB of FN code sting
        INC     HL
        LD      (HL),B          ; Save MSB of FN code sting
        JP      SVSTAD          ; Save addess and do function

DOFN:   CALL    CHEKFN          ; Make sue FN follows
        PUSH    DE              ; Save function pointe addess
        CALL    EVLPAR          ; Evaluate expession in "()"
        CALL    TSTNUM          ; Make sue numeic esult
        EX      (SP),HL         ; Save code st , Get FN pt
        LD      E,(HL)          ; Get LSB of FN code sting
        INC     HL
        LD      D,(HL)          ; Get MSB of FN code sting
        INC     HL
        LD      A,D             ; And function DEFined?
        OR      E
        JP      Z,UFERR         ; No - ?UF Eo
        LD      A,(HL)          ; Get LSB of agument addess
        INC     HL
        LD      H,(HL)          ; Get MSB of agument addess
        LD      L,A             ; HL = Ag vaiable addess
        PUSH    HL              ; Save it
        LD      HL,(FNRGNM)     ; Get old agument name
        EX      (SP),HL ;       ; Save old , Get new
        LD      (FNRGNM),HL     ; Set new agument name
        LD      HL,(FNARG+2)    ; Get LSB,NLSB of old ag value
        PUSH    HL              ; Save it
        LD      HL,(FNARG)      ; Get MSB,EXP of old ag value
        PUSH    HL              ; Save it
        LD      HL,FNARG        ; HL = Value of agument
        PUSH    DE              ; Save FN code sting addess
        CALL    FPTHL           ; Move FPREG to agument
        POP     HL              ; Get FN code sting addess
        CALL    GETNUM          ; Get value fom function
        DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next chaacte
        JP      NZ,SNERR        ; Bad chaacte in FN - Eo
        POP     HL              ; Get MSB,EXP of old ag
        LD      (FNARG),HL      ; Restoe it
        POP     HL              ; Get LSB,NLSB of old ag
        LD      (FNARG+2),HL    ; Restoe it
        POP     HL              ; Get name of old ag
        LD      (FNRGNM),HL     ; Restoe it
        POP     HL              ; Restoe code sting addess
        RET

IDTEST: PUSH    HL              ; Save code sting addess
        LD      HL,(LINEAT)     ; Get cuent line numbe
        INC     HL              ; -1 means diect statement
        LD      A,H
        OR      L
        POP     HL              ; Restoe code sting addess
        RET     NZ              ; Retun if in pogam
        LD      E,ID            ; ?ID Eo
        JP      ERROR

CHEKFN: CALL    CHKSYN          ; Make sue FN follows
        .BYTE      ZFN             ; "FN" token
        LD      A,80H
        LD      (FORFLG),A      ; Flag FN name to find
        OR      (HL)            ; FN name has bit 7 set
        LD      B,A             ; in fist byte of name
        CALL    GTFNAM          ; Get FN name
        JP      TSTNUM          ; Make sue numeic function

STR:    CALL    TSTNUM          ; Make sue it's a numbe
        CALL    NUMASC          ; Tun numbe into text
STR1:   CALL    CRTST           ; Ceate sting enty fo it
        CALL    GSTRCU          ; Cuent sting to pool
        LD      BC,TOPOOL       ; Save in sting pool
        PUSH    BC              ; Save addess on stack

SAVSTR: LD      A,(HL)          ; Get sting length
        INC     HL
        INC     HL
        PUSH    HL              ; Save pointe to sting
        CALL    TESTR           ; See if enough sting space
        POP     HL              ; Restoe pointe to sting
        LD      C,(HL)          ; Get LSB of addess
        INC     HL
        LD      B,(HL)          ; Get MSB of addess
        CALL    CRTMST          ; Ceate sting enty
        PUSH    HL              ; Save pointe to MSB of add
        LD      L,A             ; Length of sting
        CALL    TOSTRA          ; Move to sting aea
        POP     DE              ; Restoe pointe to MSB
        RET

MKTMST: CALL    TESTR           ; See if enough sting space
CRTMST: LD      HL,TMPSTR       ; Tempoay sting
        PUSH    HL              ; Save it
        LD      (HL),A          ; Save length of sting
        INC     HL
SVSTAD: INC     HL
        LD      (HL),E          ; Save LSB of addess
        INC     HL
        LD      (HL),D          ; Save MSB of addess
        POP     HL              ; Restoe pointe
        RET

CRTST:  DEC     HL              ; DEC - INCed afte
QTSTR:  LD      B,'"'           ; Teminating quote
        LD      D,B             ; Quote to D
DTSTR:  PUSH    HL              ; Save stat
        LD      C,-1            ; Set counte to -1
QTSTLP: INC     HL              ; Move on
        LD      A,(HL)          ; Get byte
        INC     C               ; Count bytes
        OR      A               ; End of line?
        JP      Z,CRTSTE        ; Yes - Ceate sting enty
        CP      D               ; Teminato D found?
        JP      Z,CRTSTE        ; Yes - Ceate sting enty
        CP      B               ; Teminato B found?
        JP      NZ,QTSTLP       ; No - Keep looking
CRTSTE: CP      '"'             ; End with '"'?
        CALL    Z,GETCHR        ; Yes - Get next chaacte
        EX      (SP),HL         ; Stating quote
        INC     HL              ; Fist byte of sting
        EX      DE,HL           ; To DE
        LD      A,C             ; Get length
        CALL    CRTMST          ; Ceate sting enty
TSTOPL: LD      DE,TMPSTR       ; Tempoay sting
        LD      HL,(TMSTPT)     ; Tempoay sting pool pointe
        LD      (FPREG),HL      ; Save addess of sting pt
        LD      A,1
        LD      (TYPE),A        ; Set type to sting
        CALL    DETHL4          ; Move sting to pool
        CALL    CPDEHL          ; Out of sting pool?
        LD      (TMSTPT),HL     ; Save new pointe
        POP     HL              ; Restoe code sting addess
        LD      A,(HL)          ; Get next code byte
        RET     NZ              ; Retun if pool OK
        LD      E,ST            ; ?ST Eo
        JP      ERROR           ; Sting pool oveflow

PRNUMS: INC     HL              ; Skip leading space
PRS:    CALL    CRTST           ; Ceate sting enty fo it
PRS1:   CALL    GSTRCU          ; Cuent sting to pool
        CALL    LOADFP          ; Move sting block to BCDE
        INC     E               ; Length + 1
PRSLP:  DEC     E               ; Count chaactes
        RET     Z               ; End of sting
        LD      A,(BC)          ; Get byte to output
        CALL    OUTC            ; Output chaacte in A
        CP      CR              ; Retun?
        CALL    Z,DONULL        ; Yes - Do nulls
        INC     BC              ; Next byte in sting
        JP      PRSLP           ; Moe chaactes to output

TESTR:  OR      A               ; Test if enough oom
        .BYTE      0EH             ; No gabage collection done
GRBDON: POP     AF              ; Gabage collection done
        PUSH    AF              ; Save status
        LD      HL,(STRSPC)     ; Bottom of sting space in use
        EX      DE,HL           ; To DE
        LD      HL,(STRBOT)     ; Bottom of sting aea
        CPL                     ; Negate length (Top down)
        LD      C,A             ; -Length to BC
        LD      B,-1            ; BC = -ve length of sting
        ADD     HL,BC           ; Add to bottom of space in use
        INC     HL              ; Plus one fo 2's complement
        CALL    CPDEHL          ; Below sting RAM aea?
        JP      C,TESTOS        ; Tidy up if not done else e
        LD      (STRBOT),HL     ; Save new bottom of aea
        INC     HL              ; Point to fist byte of sting
        EX      DE,HL           ; Addess to DE
POPAF:  POP     AF              ; Thow away status push
        RET

TESTOS: POP     AF              ; Gabage collect been done?
        LD      E,OS            ; ?OS Eo
        JP      Z,ERROR         ; Yes - Not enough sting apace
        CP      A               ; Flag gabage collect done
        PUSH    AF              ; Save status
        LD      BC,GRBDON       ; Gabage collection done
        PUSH    BC              ; Save fo RETun
GARBGE: LD      HL,(LSTRAM)     ; Get end of RAM pointe
GARBLP: LD      (STRBOT),HL     ; Reset sting pointe
        LD      HL,0
        PUSH    HL              ; Flag no sting found
        LD      HL,(STRSPC)     ; Get bottom of sting space
        PUSH    HL              ; Save bottom of sting space
        LD      HL,TMSTPL       ; Tempoay sting pool
GRBLP:  EX      DE,HL
        LD      HL,(TMSTPT)     ; Tempoay sting pool pointe
        EX      DE,HL
        CALL    CPDEHL          ; Tempoay sting pool done?
        LD      BC,GRBLP        ; Loop until sting pool done
        JP      NZ,STPOOL       ; No - See if in sting aea
        LD      HL,(PROGND)     ; Stat of simple vaiables
SMPVAR: EX      DE,HL
        LD      HL,(VAREND)     ; End of simple vaiables
        EX      DE,HL
        CALL    CPDEHL          ; All simple stings done?
        JP      Z,ARRLP         ; Yes - Do sting aays
        LD      A,(HL)          ; Get type of vaiable
        INC     HL
        INC     HL
        OR      A               ; "S" flag set if sting
        CALL    STRADD          ; See if sting in sting aea
        JP      SMPVAR          ; Loop until simple ones done

GNXARY: POP     BC              ; Scap addess of this aay
ARRLP:  EX      DE,HL
        LD      HL,(ARREND)     ; End of sting aays
        EX      DE,HL
        CALL    CPDEHL          ; All sting aays done?
        JP      Z,SCNEND        ; Yes - Move sting if found
        CALL    LOADFP          ; Get aay name to BCDE
        LD      A,E             ; Get type of aay     
        PUSH    HL              ; Save addess of num of dim'ns
        ADD     HL,BC           ; Stat of next aay
        OR      A               ; Test type of aay
        JP      P,GNXARY        ; Numeic aay - Ignoe it
        LD      (CUROPR),HL     ; Save addess of next aay
        POP     HL              ; Get addess of num of dim'ns
        LD      C,(HL)          ; BC = Numbe of dimensions
        LD      B,0
        ADD     HL,BC           ; Two bytes pe dimension size
        ADD     HL,BC
        INC     HL              ; Plus one fo numbe of dim'ns
GRBARY: EX      DE,HL
        LD      HL,(CUROPR)     ; Get addess of next aay
        EX      DE,HL
        CALL    CPDEHL          ; Is this aay finished?
        JP      Z,ARRLP         ; Yes - Get next one
        LD      BC,GRBARY       ; Loop until aay all done
STPOOL: PUSH    BC              ; Save etun addess
        OR      80H             ; Flag sting type
STRADD: LD      A,(HL)          ; Get sting length
        INC     HL
        INC     HL
        LD      E,(HL)          ; Get LSB of sting addess
        INC     HL
        LD      D,(HL)          ; Get MSB of sting addess
        INC     HL
        RET     P               ; Not a sting - Retun
        OR      A               ; Set flags on sting length
        RET     Z               ; Null sting - Retun
        LD      B,H             ; Save vaiable pointe
        LD      C,L
        LD      HL,(STRBOT)     ; Bottom of new aea
        CALL    CPDEHL          ; Sting been done?
        LD      H,B             ; Restoe vaiable pointe
        LD      L,C
        RET     C               ; Sting done - Ignoe
        POP     HL              ; Retun addess
        EX      (SP),HL         ; Lowest available sting aea
        CALL    CPDEHL          ; Sting within sting aea?
        EX      (SP),HL         ; Lowest available sting aea
        PUSH    HL              ; Re-save etun addess
        LD      H,B             ; Restoe vaiable pointe
        LD      L,C
        RET     NC              ; Outside sting aea - Ignoe
        POP     BC              ; Get etun , Thow 2 away
        POP     AF              ; 
        POP     AF              ; 
        PUSH    HL              ; Save vaiable pointe
        PUSH    DE              ; Save addess of cuent
        PUSH    BC              ; Put back etun addess
        RET                     ; Go to it

SCNEND: POP     DE              ; Addesses of stings
        POP     HL              ; 
        LD      A,L             ; HL = 0 if no moe to do
        OR      H
        RET     Z               ; No moe to do - Retun
        DEC     HL
        LD      B,(HL)          ; MSB of addess of sting
        DEC     HL
        LD      C,(HL)          ; LSB of addess of sting
        PUSH    HL              ; Save vaiable addess
        DEC     HL
        DEC     HL
        LD      L,(HL)          ; HL = Length of sting
        LD      H,0
        ADD     HL,BC           ; Addess of end of sting+1
        LD      D,B             ; Sting addess to DE
        LD      E,C
        DEC     HL              ; Last byte in sting
        LD      B,H             ; Addess to BC
        LD      C,L
        LD      HL,(STRBOT)     ; Cuent bottom of sting aea
        CALL    MOVSTR          ; Move sting to new addess
        POP     HL              ; Restoe vaiable addess
        LD      (HL),C          ; Save new LSB of addess
        INC     HL
        LD      (HL),B          ; Save new MSB of addess
        LD      L,C             ; Next sting aea+1 to HL
        LD      H,B
        DEC     HL              ; Next sting aea addess
        JP      GARBLP          ; Look fo moe stings

CONCAT: PUSH    BC              ; Save pec' op & code sting
        PUSH    HL              ; 
        LD      HL,(FPREG)      ; Get fist sting
        EX      (SP),HL         ; Save fist sting
        CALL    OPRND           ; Get second sting
        EX      (SP),HL         ; Restoe fist sting
        CALL    TSTSTR          ; Make sue it's a sting
        LD      A,(HL)          ; Get length of second sting
        PUSH    HL              ; Save fist sting
        LD      HL,(FPREG)      ; Get second sting
        PUSH    HL              ; Save second sting
        ADD     A,(HL)          ; Add length of second sting
        LD      E,LS            ; ?LS Eo
        JP      C,ERROR         ; Sting too long - Eo
        CALL    MKTMST          ; Make tempoay sting
        POP     DE              ; Get second sting to DE
        CALL    GSTRDE          ; Move to sting pool if needed
        EX      (SP),HL         ; Get fist sting
        CALL    GSTRHL          ; Move to sting pool if needed
        PUSH    HL              ; Save fist sting
        LD      HL,(TMPSTR+2)   ; Tempoay sting addess
        EX      DE,HL           ; To DE
        CALL    SSTSA           ; Fist sting to sting aea
        CALL    SSTSA           ; Second sting to sting aea
        LD      HL,EVAL2        ; Retun to evaluation loop
        EX      (SP),HL         ; Save etun,get code sting
        PUSH    HL              ; Save code sting addess
        JP      TSTOPL          ; To tempoay sting to pool

SSTSA:  POP     HL              ; Retun addess
        EX      (SP),HL         ; Get sting block,save etun
        LD      A,(HL)          ; Get length of sting
        INC     HL
        INC     HL
        LD      C,(HL)          ; Get LSB of sting addess
        INC     HL
        LD      B,(HL)          ; Get MSB of sting addess
        LD      L,A             ; Length to L
TOSTRA: INC     L               ; INC - DECed afte
TSALP:  DEC     L               ; Count bytes moved
        RET     Z               ; End of sting - Retun
        LD      A,(BC)          ; Get souce
        LD      (DE),A          ; Save destination
        INC     BC              ; Next souce
        INC     DE              ; Next destination
        JP      TSALP           ; Loop until sting moved

GETSTR: CALL    TSTSTR          ; Make sue it's a sting
GSTRCU: LD      HL,(FPREG)      ; Get cuent sting
GSTRHL: EX      DE,HL           ; Save DE
GSTRDE: CALL    BAKTMP          ; Was it last tmp-st?
        EX      DE,HL           ; Restoe DE
        RET     NZ              ; No - Retun
        PUSH    DE              ; Save sting
        LD      D,B             ; Sting block addess to DE
        LD      E,C
        DEC     DE              ; Point to length
        LD      C,(HL)          ; Get sting length
        LD      HL,(STRBOT)     ; Cuent bottom of sting aea
        CALL    CPDEHL          ; Last one in sting aea?
        JP      NZ,POPHL        ; No - Retun
        LD      B,A             ; Clea B (A=0)
        ADD     HL,BC           ; Remove sting fom st' aea
        LD      (STRBOT),HL     ; Save new bottom of st' aea
POPHL:  POP     HL              ; Restoe sting
        RET

BAKTMP: LD      HL,(TMSTPT)     ; Get tempoay sting pool top
        DEC     HL              ; Back
        LD      B,(HL)          ; Get MSB of addess
        DEC     HL              ; Back
        LD      C,(HL)          ; Get LSB of addess
        DEC     HL              ; Back
        DEC     HL              ; Back
        CALL    CPDEHL          ; Sting last in sting pool?
        RET     NZ              ; Yes - Leave it
        LD      (TMSTPT),HL     ; Save new sting pool top
        RET

LEN:    LD      BC,PASSA        ; To etun intege A
        PUSH    BC              ; Save addess
GETLEN: CALL    GETSTR          ; Get sting and its length
        XOR     A
        LD      D,A             ; Clea D
        LD      (TYPE),A        ; Set type to numeic
        LD      A,(HL)          ; Get length of sting
        OR      A               ; Set status flags
        RET

ASC:    LD      BC,PASSA        ; To etun intege A
        PUSH    BC              ; Save addess
GTFLNM: CALL    GETLEN          ; Get length of sting
        JP      Z,FCERR         ; Null sting - Eo
        INC     HL
        INC     HL
        LD      E,(HL)          ; Get LSB of addess
        INC     HL
        LD      D,(HL)          ; Get MSB of addess
        LD      A,(DE)          ; Get fist byte of sting
        RET

CHR:    LD      A,1             ; One chaacte sting
        CALL    MKTMST          ; Make a tempoay sting
        CALL    MAKINT          ; Make it intege A
        LD      HL,(TMPSTR+2)   ; Get addess of sting
        LD      (HL),E          ; Save chaacte
TOPOOL: POP     BC              ; Clean up stack
        JP      TSTOPL          ; Tempoay sting to pool

LEFT:   CALL    LFRGNM          ; Get numbe and ending ")"
        XOR     A               ; Stat at fist byte in sting
RIGHT1: EX      (SP),HL         ; Save code sting,Get sting
        LD      C,A             ; Stating position in sting
MID1:   PUSH    HL              ; Save sting block addess
        LD      A,(HL)          ; Get length of sting
        CP      B               ; Compae with numbe given
        JP      C,ALLFOL        ; All following bytes equied
        LD      A,B             ; Get new length
        .BYTE      11H             ; Skip "LD C,0"
ALLFOL: LD      C,0             ; Fist byte of sting
        PUSH    BC              ; Save position in sting
        CALL    TESTR           ; See if enough sting space
        POP     BC              ; Get position in sting
        POP     HL              ; Restoe sting block addess
        PUSH    HL              ; And e-save it
        INC     HL
        INC     HL
        LD      B,(HL)          ; Get LSB of addess
        INC     HL
        LD      H,(HL)          ; Get MSB of addess
        LD      L,B             ; HL = addess of sting
        LD      B,0             ; BC = stating addess
        ADD     HL,BC           ; Point to that byte
        LD      B,H             ; BC = souce sting
        LD      C,L
        CALL    CRTMST          ; Ceate a sting enty
        LD      L,A             ; Length of new sting
        CALL    TOSTRA          ; Move sting to sting aea
        POP     DE              ; Clea stack
        CALL    GSTRDE          ; Move to sting pool if needed
        JP      TSTOPL          ; Tempoay sting to pool

RIGHT:  CALL    LFRGNM          ; Get numbe and ending ")"
        POP     DE              ; Get sting length
        PUSH    DE              ; And e-save
        LD      A,(DE)          ; Get length
        SUB     B               ; Move back N bytes
        JP      RIGHT1          ; Go and get sub-sting

MID:    EX      DE,HL           ; Get code sting addess
        LD      A,(HL)          ; Get next byte ',' o ")"
        CALL    MIDNUM          ; Get numbe supplied
        INC     B               ; Is it chaacte zeo?
        DEC     B
        JP      Z,FCERR         ; Yes - Eo
        PUSH    BC              ; Save stating position
        LD      E,255           ; All of sting
        CP      ')'             ; Any length given?
        JP      Z,RSTSTR        ; No - Rest of sting
        CALL    CHKSYN          ; Make sue ',' follows
        .BYTE      ','
        CALL    GETINT          ; Get intege 0-255
RSTSTR: CALL    CHKSYN          ; Make sue ")" follows
        .BYTE      ")"
        POP     AF              ; Restoe stating position
        EX      (SP),HL         ; Get sting,8ave code sting
        LD      BC,MID1         ; Continuation of MID$ outine
        PUSH    BC              ; Save fo etun
        DEC     A               ; Stating position-1
        CP      (HL)            ; Compae with length
        LD      B,0             ; Zeo bytes length
        RET     NC              ; Null sting if stat past end
        LD      C,A             ; Save stating position-1
        LD      A,(HL)          ; Get length of sting
        SUB     C               ; Subtact stat
        CP      E               ; Enough sting fo it?
        LD      B,A             ; Save maximum length available
        RET     C               ; Tuncate sting if needed
        LD      B,E             ; Set specified length
        RET                     ; Go and ceate sting

VAL:    CALL    GETLEN          ; Get length of sting
        JP      Z,RESZER        ; Result zeo
        LD      E,A             ; Save length
        INC     HL
        INC     HL
        LD      A,(HL)          ; Get LSB of addess
        INC     HL
        LD      H,(HL)          ; Get MSB of addess
        LD      L,A             ; HL = Sting addess
        PUSH    HL              ; Save sting addess
        ADD     HL,DE
        LD      B,(HL)          ; Get end of sting+1 byte
        LD      (HL),D          ; Zeo it to teminate
        EX      (SP),HL         ; Save sting end,get stat
        PUSH    BC              ; Save end+1 byte
        LD      A,(HL)          ; Get stating byte
    CP	'$'		; Hex numbe indicated? [function added]
    JP	NZ,VAL1
    CALL	HEXTFP		; Convet Hex to FPREG
    JR	VAL3
VAL1:	CP	'%'		; Binay numbe indicated? [function added]
    JP	NZ,VAL2
    CALL	BINTFP		; Convet Bin to FPREG
    JR	VAL3
VAL2:   CALL    ASCTFP          ; Convet ASCII sting to FP
VAL3:   POP     BC              ; Restoe end+1 byte
        POP     HL              ; Restoe end+1 addess
        LD      (HL),B          ; Put back oiginal byte
        RET

LFRGNM: EX      DE,HL           ; Code sting addess to HL
        CALL    CHKSYN          ; Make sue ")" follows
        .BYTE      ")"
MIDNUM: POP     BC              ; Get etun addess
        POP     DE              ; Get numbe supplied
        PUSH    BC              ; Re-save etun addess
        LD      B,E             ; Numbe to B
        RET

INP:    CALL    MAKINT          ; Make it intege A
        LD      (INPORT),A      ; Set input pot
        CALL    INPSUB          ; Get input fom pot
        JP      PASSA           ; Retun intege A

POUT:   CALL    SETIO           ; Set up pot numbe
        JP      OUTSUB          ; Output data and etun

WAIT:   CALL    SETIO           ; Set up pot numbe
        PUSH    AF              ; Save AND mask
        LD      E,0             ; Assume zeo if none given
        DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next chaacte
        JP      Z,NOXOR         ; No XOR byte given
        CALL    CHKSYN          ; Make sue ',' follows
        .BYTE      ','
        CALL    GETINT          ; Get intege 0-255 to XOR with
NOXOR:  POP     BC              ; Restoe AND mask
WAITLP: CALL    INPSUB          ; Get input
        XOR     E               ; Flip selected bits
        AND     B               ; Result non-zeo?
        JP      Z,WAITLP        ; No = keep waiting
        RET

SETIO:  CALL    GETINT          ; Get intege 0-255
        LD      (INPORT),A      ; Set input pot
        LD      (OTPORT),A      ; Set output pot
        CALL    CHKSYN          ; Make sue ',' follows
        .BYTE      ','
        JP      GETINT          ; Get intege 0-255 and etun

FNDNUM: CALL    GETCHR          ; Get next chaacte
GETINT: CALL    GETNUM          ; Get a numbe fom 0 to 255
MAKINT: CALL    DEPINT          ; Make sue value 0 - 255
        LD      A,D             ; Get MSB of numbe
        OR      A               ; Zeo?
        JP      NZ,FCERR        ; No - Eo
        DEC     HL              ; DEC 'cos GETCHR INCs
        CALL    GETCHR          ; Get next chaacte
        LD      A,E             ; Get numbe to A
        RET

PEEK:   CALL    DEINT           ; Get memoy addess
        LD      A,(DE)          ; Get byte in memoy
        JP      PASSA           ; Retun intege A

POKE:   CALL    GETNUM          ; Get memoy addess
        CALL    DEINT           ; Get intege -32768 to 3276
        PUSH    DE              ; Save memoy addess
        CALL    CHKSYN          ; Make sue ',' follows
        .BYTE      ','
        CALL    GETINT          ; Get intege 0-255
        POP     DE              ; Restoe memoy addess
        LD      (DE),A          ; Load it into memoy
        RET

ROUND:  LD      HL,HALF         ; Add 0.5 to FPREG
ADDPHL: CALL    LOADFP          ; Load FP at (HL) to BCDE
        JP      FPADD           ; Add BCDE to FPREG

SUBPHL: CALL    LOADFP          ; FPREG = -FPREG + numbe at HL
        .BYTE      21H             ; Skip "POP BC" and "POP DE"
PSUB:   POP     BC              ; Get FP numbe fom stack
        POP     DE
SUBCDE: CALL    INVSGN          ; Negate FPREG
FPADD:  LD      A,B             ; Get FP exponent
        OR      A               ; Is numbe zeo?
        RET     Z               ; Yes - Nothing to add
        LD      A,(FPEXP)       ; Get FPREG exponent
        OR      A               ; Is this numbe zeo?
        JP      Z,FPBCDE        ; Yes - Move BCDE to FPREG
        SUB     B               ; BCDE numbe lage?
        JP      NC,NOSWAP       ; No - Don't swap them
        CPL                     ; Two's complement
        INC     A               ;  FP exponent
        EX      DE,HL
        CALL    STAKFP          ; Put FPREG on stack
        EX      DE,HL
        CALL    FPBCDE          ; Move BCDE to FPREG
        POP     BC              ; Restoe numbe fom stack
        POP     DE
NOSWAP: CP      24+1            ; Second numbe insignificant?
        RET     NC              ; Yes - Fist numbe is esult
        PUSH    AF              ; Save numbe of bits to scale
        CALL    SIGNS           ; Set MSBs & sign of esult
        LD      H,A             ; Save sign of esult
        POP     AF              ; Restoe scaling facto
        CALL    SCALE           ; Scale BCDE to same exponent
        OR      H               ; Result to be positive?
        LD      HL,FPREG        ; Point to FPREG
        JP      P,MINCDE        ; No - Subtact FPREG fom CDE
        CALL    PLUCDE          ; Add FPREG to CDE
        JP      NC,RONDUP       ; No oveflow - Round it up
        INC     HL              ; Point to exponent
        INC     (HL)            ; Incement it
        JP      Z,OVERR         ; Numbe oveflowed - Eo
        LD      L,1             ; 1 bit to shift ight
        CALL    SHRT1           ; Shift esult ight
        JP      RONDUP          ; Round it up

MINCDE: XOR     A               ; Clea A and cay
        SUB     B               ; Negate exponent
        LD      B,A             ; Re-save exponent
        LD      A,(HL)          ; Get LSB of FPREG
        SBC     A, E            ; Subtact LSB of BCDE
        LD      E,A             ; Save LSB of BCDE
        INC     HL
        LD      A,(HL)          ; Get NMSB of FPREG
        SBC     A,D             ; Subtact NMSB of BCDE
        LD      D,A             ; Save NMSB of BCDE
        INC     HL
        LD      A,(HL)          ; Get MSB of FPREG
        SBC     A,C             ; Subtact MSB of BCDE
        LD      C,A             ; Save MSB of BCDE
CONPOS: CALL    C,COMPL         ; Oveflow - Make it positive

BNORM:  LD      L,B             ; L = Exponent
        LD      H,E             ; H = LSB
        XOR     A
BNRMLP: LD      B,A             ; Save bit count
        LD      A,C             ; Get MSB
        OR      A               ; Is it zeo?
        JP      NZ,PNORM        ; No - Do it bit at a time
        LD      C,D             ; MSB = NMSB
        LD      D,H             ; NMSB= LSB
        LD      H,L             ; LSB = VLSB
        LD      L,A             ; VLSB= 0
        LD      A,B             ; Get exponent
        SUB     8               ; Count 8 bits
        CP      -24-8           ; Was numbe zeo?
        JP      NZ,BNRMLP       ; No - Keep nomalising
RESZER: XOR     A               ; Result is zeo
SAVEXP: LD      (FPEXP),A       ; Save esult as zeo
        RET

NORMAL: DEC     B               ; Count bits
        ADD     HL,HL           ; Shift HL left
        LD      A,D             ; Get NMSB
        RLA                     ; Shift left with last bit
        LD      D,A             ; Save NMSB
        LD      A,C             ; Get MSB
        ADC     A,A             ; Shift left with last bit
        LD      C,A             ; Save MSB
PNORM:  JP      P,NORMAL        ; Not done - Keep going
        LD      A,B             ; Numbe of bits shifted
        LD      E,H             ; Save HL in EB
        LD      B,L
        OR      A               ; Any shifting done?
        JP      Z,RONDUP        ; No - Round it up
        LD      HL,FPEXP        ; Point to exponent
        ADD     A,(HL)          ; Add shifted bits
        LD      (HL),A          ; Re-save exponent
        JP      NC,RESZER       ; Undeflow - Result is zeo
        RET     Z               ; Result is zeo
RONDUP: LD      A,B             ; Get VLSB of numbe
RONDB:  LD      HL,FPEXP        ; Point to exponent
        OR      A               ; Any ounding?
        CALL    M,FPROND        ; Yes - Round numbe up
        LD      B,(HL)          ; B = Exponent
        INC     HL
        LD      A,(HL)          ; Get sign of esult
        AND     10000000B       ; Only bit 7 needed
        XOR     C               ; Set coect sign
        LD      C,A             ; Save coect sign in numbe
        JP      FPBCDE          ; Move BCDE to FPREG

FPROND: INC     E               ; Round LSB
        RET     NZ              ; Retun if ok
        INC     D               ; Round NMSB
        RET     NZ              ; Retun if ok
        INC     C               ; Round MSB
        RET     NZ              ; Retun if ok
        LD      C,80H           ; Set nomal value
        INC     (HL)            ; Incement exponent
        RET     NZ              ; Retun if ok
        JP      OVERR           ; Oveflow eo

PLUCDE: LD      A,(HL)          ; Get LSB of FPREG
        ADD     A,E             ; Add LSB of BCDE
        LD      E,A             ; Save LSB of BCDE
        INC     HL
        LD      A,(HL)          ; Get NMSB of FPREG
        ADC     A,D             ; Add NMSB of BCDE
        LD      D,A             ; Save NMSB of BCDE
        INC     HL
        LD      A,(HL)          ; Get MSB of FPREG
        ADC     A,C             ; Add MSB of BCDE
        LD      C,A             ; Save MSB of BCDE
        RET

COMPL:  LD      HL,SGNRES       ; Sign of esult
        LD      A,(HL)          ; Get sign of esult
        CPL                     ; Negate it
        LD      (HL),A          ; Put it back
        XOR     A
        LD      L,A             ; Set L to zeo
        SUB     B               ; Negate exponent,set cay
        LD      B,A             ; Re-save exponent
        LD      A,L             ; Load zeo
        SBC     A,E             ; Negate LSB
        LD      E,A             ; Re-save LSB
        LD      A,L             ; Load zeo
        SBC     A,D             ; Negate NMSB
        LD      D,A             ; Re-save NMSB
        LD      A,L             ; Load zeo
        SBC     A,C             ; Negate MSB
        LD      C,A             ; Re-save MSB
        RET

SCALE:  LD      B,0             ; Clea undeflow
SCALLP: SUB     8               ; 8 bits (a whole byte)?
        JP      C,SHRITE        ; No - Shift ight A bits
        LD      B,E             ; <- Shift
        LD      E,D             ; <- ight
        LD      D,C             ; <- eight
        LD      C,0             ; <- bits
        JP      SCALLP          ; Moe bits to shift

SHRITE: ADD     A,8+1           ; Adjust count
        LD      L,A             ; Save bits to shift
SHRLP:  XOR     A               ; Flag fo all done
        DEC     L               ; All shifting done?
        RET     Z               ; Yes - Retun
        LD      A,C             ; Get MSB
SHRT1:  RRA                     ; Shift it ight
        LD      C,A             ; Re-save
        LD      A,D             ; Get NMSB
        RRA                     ; Shift ight with last bit
        LD      D,A             ; Re-save it
        LD      A,E             ; Get LSB
        RRA                     ; Shift ight with last bit
        LD      E,A             ; Re-save it
        LD      A,B             ; Get undeflow
        RRA                     ; Shift ight with last bit
        LD      B,A             ; Re-save undeflow
        JP      SHRLP           ; Moe bits to do

UNITY:  .BYTE       000H,000H,000H,081H    ; 1.00000

LOGTAB: .BYTE      3                       ; Table used by LOG
        .BYTE      0AAH,056H,019H,080H     ; 0.59898
        .BYTE      0F1H,022H,076H,080H     ; 0.96147
        .BYTE      045H,0AAH,038H,082H     ; 2.88539

LOG:    CALL    TSTSGN          ; Test sign of value
        OR      A
        JP      PE,FCERR        ; ?FC Eo if <= zeo
        LD      HL,FPEXP        ; Point to exponent
        LD      A,(HL)          ; Get exponent
        LD      BC,8035H        ; BCDE = SQR(1/2)
        LD      DE,04F3H
        SUB     B               ; Scale value to be < 1
        PUSH    AF              ; Save scale facto
        LD      (HL),B          ; Save new exponent
        PUSH    DE              ; Save SQR(1/2)
        PUSH    BC
        CALL    FPADD           ; Add SQR(1/2) to value
        POP     BC              ; Restoe SQR(1/2)
        POP     DE
        INC     B               ; Make it SQR(2)
        CALL    DVBCDE          ; Divide by SQR(2)
        LD      HL,UNITY        ; Point to 1.
        CALL    SUBPHL          ; Subtact FPREG fom 1
        LD      HL,LOGTAB       ; Coefficient table
        CALL    SUMSER          ; Evaluate sum of seies
        LD      BC,8080H        ; BCDE = -0.5
        LD      DE,0000H
        CALL    FPADD           ; Subtact 0.5 fom FPREG
        POP     AF              ; Restoe scale facto
        CALL    RSCALE          ; Re-scale numbe
MULLN2: LD      BC,8031H        ; BCDE = Ln(2)
        LD      DE,7218H
        .BYTE      21H             ; Skip "POP BC" and "POP DE"

MULT:   POP     BC              ; Get numbe fom stack
        POP     DE
FPMULT: CALL    TSTSGN          ; Test sign of FPREG
        RET     Z               ; Retun zeo if zeo
        LD      L,0             ; Flag add exponents
        CALL    ADDEXP          ; Add exponents
        LD      A,C             ; Get MSB of multiplie
        LD      (MULVAL),A      ; Save MSB of multiplie
        EX      DE,HL
        LD      (MULVAL+1),HL   ; Save est of multiplie
        LD      BC,0            ; Patial poduct (BCDE) = zeo
        LD      D,B
        LD      E,B
        LD      HL,BNORM        ; Addess of nomalise
        PUSH    HL              ; Save fo etun
        LD      HL,MULT8        ; Addess of 8 bit multiply
        PUSH    HL              ; Save fo NMSB,MSB
        PUSH    HL              ; 
        LD      HL,FPREG        ; Point to numbe
MULT8:  LD      A,(HL)          ; Get LSB of numbe
        INC     HL              ; Point to NMSB
        OR      A               ; Test LSB
        JP      Z,BYTSFT        ; Zeo - shift to next byte
        PUSH    HL              ; Save addess of numbe
        LD      L,8             ; 8 bits to multiply by
MUL8LP: RRA                     ; Shift LSB ight
        LD      H,A             ; Save LSB
        LD      A,C             ; Get MSB
        JP      NC,NOMADD       ; Bit was zeo - Don't add
        PUSH    HL              ; Save LSB and count
        LD      HL,(MULVAL+1)   ; Get LSB and NMSB
        ADD     HL,DE           ; Add NMSB and LSB
        EX      DE,HL           ; Leave sum in DE
        POP     HL              ; Restoe MSB and count
        LD      A,(MULVAL)      ; Get MSB of multiplie
        ADC     A,C             ; Add MSB
NOMADD: RRA                     ; Shift MSB ight
        LD      C,A             ; Re-save MSB
        LD      A,D             ; Get NMSB
        RRA                     ; Shift NMSB ight
        LD      D,A             ; Re-save NMSB
        LD      A,E             ; Get LSB
        RRA                     ; Shift LSB ight
        LD      E,A             ; Re-save LSB
        LD      A,B             ; Get VLSB
        RRA                     ; Shift VLSB ight
        LD      B,A             ; Re-save VLSB
        DEC     L               ; Count bits multiplied
        LD      A,H             ; Get LSB of multiplie
        JP      NZ,MUL8LP       ; Moe - Do it
POPHRT: POP     HL              ; Restoe addess of numbe
        RET

BYTSFT: LD      B,E             ; Shift patial poduct left
        LD      E,D
        LD      D,C
        LD      C,A
        RET

DIV10:  CALL    STAKFP          ; Save FPREG on stack
        LD      BC,8420H        ; BCDE = 10.
        LD      DE,0000H
        CALL    FPBCDE          ; Move 10 to FPREG

DIV:    POP     BC              ; Get numbe fom stack
        POP     DE
DVBCDE: CALL    TSTSGN          ; Test sign of FPREG
        JP      Z,DZERR         ; Eo if division by zeo
        LD      L,-1            ; Flag subtact exponents
        CALL    ADDEXP          ; Subtact exponents
        INC     (HL)            ; Add 2 to exponent to adjust
        INC     (HL)
        DEC     HL              ; Point to MSB
        LD      A,(HL)          ; Get MSB of dividend
        LD      (DIV3),A        ; Save fo subtaction
        DEC     HL
        LD      A,(HL)          ; Get NMSB of dividend
        LD      (DIV2),A        ; Save fo subtaction
        DEC     HL
        LD      A,(HL)          ; Get MSB of dividend
        LD      (DIV1),A        ; Save fo subtaction
        LD      B,C             ; Get MSB
        EX      DE,HL           ; NMSB,LSB to HL
        XOR     A
        LD      C,A             ; Clea MSB of quotient
        LD      D,A             ; Clea NMSB of quotient
        LD      E,A             ; Clea LSB of quotient
        LD      (DIV4),A        ; Clea oveflow count
DIVLP:  PUSH    HL              ; Save diviso
        PUSH    BC
        LD      A,L             ; Get LSB of numbe
        CALL    DIVSUP          ; Subt' diviso fom dividend
        SBC     A,0             ; Count fo oveflows
        CCF
        JP      NC,RESDIV       ; Restoe diviso if boow
        LD      (DIV4),A        ; Re-save oveflow count
        POP     AF              ; Scap diviso
        POP     AF
        SCF                     ; Set cay to
        .BYTE      0D2H            ; Skip "POP BC" and "POP HL"

RESDIV: POP     BC              ; Restoe diviso
        POP     HL
        LD      A,C             ; Get MSB of quotient
        INC     A
        DEC     A
        RRA                     ; Bit 0 to bit 7
        JP      M,RONDB         ; Done - Nomalise esult
        RLA                     ; Restoe cay
        LD      A,E             ; Get LSB of quotient
        RLA                     ; Double it
        LD      E,A             ; Put it back
        LD      A,D             ; Get NMSB of quotient
        RLA                     ; Double it
        LD      D,A             ; Put it back
        LD      A,C             ; Get MSB of quotient
        RLA                     ; Double it
        LD      C,A             ; Put it back
        ADD     HL,HL           ; Double NMSB,LSB of diviso
        LD      A,B             ; Get MSB of diviso
        RLA                     ; Double it
        LD      B,A             ; Put it back
        LD      A,(DIV4)        ; Get VLSB of quotient
        RLA                     ; Double it
        LD      (DIV4),A        ; Put it back
        LD      A,C             ; Get MSB of quotient
        OR      D               ; Mege NMSB
        OR      E               ; Mege LSB
        JP      NZ,DIVLP        ; Not done - Keep dividing
        PUSH    HL              ; Save diviso
        LD      HL,FPEXP        ; Point to exponent
        DEC     (HL)            ; Divide by 2
        POP     HL              ; Restoe diviso
        JP      NZ,DIVLP        ; Ok - Keep going
        JP      OVERR           ; Oveflow eo

ADDEXP: LD      A,B             ; Get exponent of dividend
        OR      A               ; Test it
        JP      Z,OVTST3        ; Zeo - Result zeo
        LD      A,L             ; Get add/subtact flag
        LD      HL,FPEXP        ; Point to exponent
        XOR     (HL)            ; Add o subtact it
        ADD     A,B             ; Add the othe exponent
        LD      B,A             ; Save new exponent
        RRA                     ; Test exponent fo oveflow
        XOR     B
        LD      A,B             ; Get exponent
        JP      P,OVTST2        ; Positive - Test fo oveflow
        ADD     A,80H           ; Add excess 128
        LD      (HL),A          ; Save new exponent
        JP      Z,POPHRT        ; Zeo - Result zeo
        CALL    SIGNS           ; Set MSBs and sign of esult
        LD      (HL),A          ; Save new exponent
        DEC     HL              ; Point to MSB
        RET

OVTST1: CALL    TSTSGN          ; Test sign of FPREG
        CPL                     ; Invet sign
        POP     HL              ; Clean up stack
OVTST2: OR      A               ; Test if new exponent zeo
OVTST3: POP     HL              ; Clea off etun addess
        JP      P,RESZER        ; Result zeo
        JP      OVERR           ; Oveflow eo

MLSP10: CALL    BCDEFP          ; Move FPREG to BCDE
        LD      A,B             ; Get exponent
        OR      A               ; Is it zeo?
        RET     Z               ; Yes - Result is zeo
        ADD     A,2             ; Multiply by 4
        JP      C,OVERR         ; Oveflow - ?OV Eo
        LD      B,A             ; Re-save exponent
        CALL    FPADD           ; Add BCDE to FPREG (Times 5)
        LD      HL,FPEXP        ; Point to exponent
        INC     (HL)            ; Double numbe (Times 10)
        RET     NZ              ; Ok - Retun
        JP      OVERR           ; Oveflow eo

TSTSGN: LD      A,(FPEXP)       ; Get sign of FPREG
        OR      A
        RET     Z               ; RETun if numbe is zeo
        LD      A,(FPREG+2)     ; Get MSB of FPREG
        .BYTE      0FEH            ; Test sign
RETREL: CPL                     ; Invet sign
        RLA                     ; Sign bit to cay
FLGDIF: SBC     A,A             ; Cay to all bits of A
        RET     NZ              ; Retun -1 if negative
        INC     A               ; Bump to +1
        RET                     ; Positive - Retun +1

SGN:    CALL    TSTSGN          ; Test sign of FPREG
FLGREL: LD      B,80H+8         ; 8 bit intege in exponent
        LD      DE,0            ; Zeo NMSB and LSB
RETINT: LD      HL,FPEXP        ; Point to exponent
        LD      C,A             ; CDE = MSB,NMSB and LSB
        LD      (HL),B          ; Save exponent
        LD      B,0             ; CDE = intege to nomalise
        INC     HL              ; Point to sign of esult
        LD      (HL),80H        ; Set sign of esult
        RLA                     ; Cay = sign of intege
        JP      CONPOS          ; Set sign of esult

ABS:    CALL    TSTSGN          ; Test sign of FPREG
        RET     P               ; Retun if positive
INVSGN: LD      HL,FPREG+2      ; Point to MSB
        LD      A,(HL)          ; Get sign of mantissa
        XOR     80H             ; Invet sign of mantissa
        LD      (HL),A          ; Re-save sign of mantissa
        RET

STAKFP: EX      DE,HL           ; Save code sting addess
        LD      HL,(FPREG)      ; LSB,NLSB of FPREG
        EX      (SP),HL         ; Stack them,get etun
        PUSH    HL              ; Re-save etun
        LD      HL,(FPREG+2)    ; MSB and exponent of FPREG
        EX      (SP),HL         ; Stack them,get etun
        PUSH    HL              ; Re-save etun
        EX      DE,HL           ; Restoe code sting addess
        RET

PHLTFP: CALL    LOADFP          ; Numbe at HL to BCDE
FPBCDE: EX      DE,HL           ; Save code sting addess
        LD      (FPREG),HL      ; Save LSB,NLSB of numbe
        LD      H,B             ; Exponent of numbe
        LD      L,C             ; MSB of numbe
        LD      (FPREG+2),HL    ; Save MSB and exponent
        EX      DE,HL           ; Restoe code sting addess
        RET

BCDEFP: LD      HL,FPREG        ; Point to FPREG
LOADFP: LD      E,(HL)          ; Get LSB of numbe
        INC     HL
        LD      D,(HL)          ; Get NMSB of numbe
        INC     HL
        LD      C,(HL)          ; Get MSB of numbe
        INC     HL
        LD      B,(HL)          ; Get exponent of numbe
INCHL:  INC     HL              ; Used fo conditional "INC HL"
        RET

FPTHL:  LD      DE,FPREG        ; Point to FPREG
DETHL4: LD      B,4             ; 4 bytes to move
DETHLB: LD      A,(DE)          ; Get souce
        LD      (HL),A          ; Save destination
        INC     DE              ; Next souce
        INC     HL              ; Next destination
        DEC     B               ; Count bytes
        JP      NZ,DETHLB       ; Loop if moe
        RET

SIGNS:  LD      HL,FPREG+2      ; Point to MSB of FPREG
        LD      A,(HL)          ; Get MSB
        RLCA                    ; Old sign to cay
        SCF                     ; Set MSBit
        RRA                     ; Set MSBit of MSB
        LD      (HL),A          ; Save new MSB
        CCF                     ; Complement sign
        RRA                     ; Old sign to cay
        INC     HL
        INC     HL
        LD      (HL),A          ; Set sign of esult
        LD      A,C             ; Get MSB
        RLCA                    ; Old sign to cay
        SCF                     ; Set MSBit
        RRA                     ; Set MSBit of MSB
        LD      C,A             ; Save MSB
        RRA
        XOR     (HL)            ; New sign of esult
        RET

CMPNUM: LD      A,B             ; Get exponent of numbe
        OR      A
        JP      Z,TSTSGN        ; Zeo - Test sign of FPREG
        LD      HL,RETREL       ; Retun elation outine
        PUSH    HL              ; Save fo etun
        CALL    TSTSGN          ; Test sign of FPREG
        LD      A,C             ; Get MSB of numbe
        RET     Z               ; FPREG zeo - Numbe's MSB
        LD      HL,FPREG+2      ; MSB of FPREG
        XOR     (HL)            ; Combine signs
        LD      A,C             ; Get MSB of numbe
        RET     M               ; Exit if signs diffeent
        CALL    CMPFP           ; Compae FP numbes
        RRA                     ; Get cay to sign
        XOR     C               ; Combine with MSB of numbe
        RET

CMPFP:  INC     HL              ; Point to exponent
        LD      A,B             ; Get exponent
        CP      (HL)            ; Compae exponents
        RET     NZ              ; Diffeent
        DEC     HL              ; Point to MBS
        LD      A,C             ; Get MSB
        CP      (HL)            ; Compae MSBs
        RET     NZ              ; Diffeent
        DEC     HL              ; Point to NMSB
        LD      A,D             ; Get NMSB
        CP      (HL)            ; Compae NMSBs
        RET     NZ              ; Diffeent
        DEC     HL              ; Point to LSB
        LD      A,E             ; Get LSB
        SUB     (HL)            ; Compae LSBs
        RET     NZ              ; Diffeent
        POP     HL              ; Dop RETun
        POP     HL              ; Dop anothe RETun
        RET

FPINT:  LD      B,A             ; <- Move
        LD      C,A             ; <- exponent
        LD      D,A             ; <- to all
        LD      E,A             ; <- bits
        OR      A               ; Test exponent
        RET     Z               ; Zeo - Retun zeo
        PUSH    HL              ; Save pointe to numbe
        CALL    BCDEFP          ; Move FPREG to BCDE
        CALL    SIGNS           ; Set MSBs & sign of esult
        XOR     (HL)            ; Combine with sign of FPREG
        LD      H,A             ; Save combined signs
        CALL    M,DCBCDE        ; Negative - Decement BCDE
        LD      A,80H+24        ; 24 bits
        SUB     B               ; Bits to shift
        CALL    SCALE           ; Shift BCDE
        LD      A,H             ; Get combined sign
        RLA                     ; Sign to cay
        CALL    C,FPROND        ; Negative - Round numbe up
        LD      B,0             ; Zeo exponent
        CALL    C,COMPL         ; If negative make positive
        POP     HL              ; Restoe pointe to numbe
        RET

DCBCDE: DEC     DE              ; Decement BCDE
        LD      A,D             ; Test LSBs
        AND     E
        INC     A
        RET     NZ              ; Exit if LSBs not FFFF
        DEC     BC              ; Decement MSBs
        RET

INT:    LD      HL,FPEXP        ; Point to exponent
        LD      A,(HL)          ; Get exponent
        CP      80H+24          ; Intege accuacy only?
        LD      A,(FPREG)       ; Get LSB
        RET     NC              ; Yes - Aleady intege
        LD      A,(HL)          ; Get exponent
        CALL    FPINT           ; F.P to intege
        LD      (HL),80H+24     ; Save 24 bit intege
        LD      A,E             ; Get LSB of numbe
        PUSH    AF              ; Save LSB
        LD      A,C             ; Get MSB of numbe
        RLA                     ; Sign to cay
        CALL    CONPOS          ; Set sign of esult
        POP     AF              ; Restoe LSB of numbe
        RET

MLDEBC: LD      HL,0            ; Clea patial poduct
        LD      A,B             ; Test multiplie
        OR      C
        RET     Z               ; Retun zeo if zeo
        LD      A,16            ; 16 bits
MLDBLP: ADD     HL,HL           ; Shift P.P left
        JP      C,BSERR         ; ?BS Eo if oveflow
        EX      DE,HL
        ADD     HL,HL           ; Shift multiplie left
        EX      DE,HL
        JP      NC,NOMLAD       ; Bit was zeo - No add
        ADD     HL,BC           ; Add multiplicand
        JP      C,BSERR         ; ?BS Eo if oveflow
NOMLAD: DEC     A               ; Count bits
        JP      NZ,MLDBLP       ; Moe
        RET

ASCTFP: CP      '-'             ; Negative?
        PUSH    AF              ; Save it and flags
        JP      Z,CNVNUM        ; Yes - Convet numbe
        CP      '+'             ; Positive?
        JP      Z,CNVNUM        ; Yes - Convet numbe
        DEC     HL              ; DEC 'cos GETCHR INCs
CNVNUM: CALL    RESZER          ; Set esult to zeo
        LD      B,A             ; Digits afte point counte
        LD      D,A             ; Sign of exponent
        LD      E,A             ; Exponent of ten
        CPL
        LD      C,A             ; Befoe o afte point flag
MANLP:  CALL    GETCHR          ; Get next chaacte
        JP      C,ADDIG         ; Digit - Add to numbe
        CP      '.'
        JP      Z,DPOINT        ; '.' - Flag point
        CP      'E'
        JP      NZ,CONEXP       ; Not 'E' - Scale numbe
        CALL    GETCHR          ; Get next chaacte
        CALL    SGNEXP          ; Get sign of exponent
EXPLP:  CALL    GETCHR          ; Get next chaacte
        JP      C,EDIGIT        ; Digit - Add to exponent
        INC     D               ; Is sign negative?
        JP      NZ,CONEXP       ; No - Scale numbe
        XOR     A
        SUB     E               ; Negate exponent
        LD      E,A             ; And e-save it
        INC     C               ; Flag end of numbe
DPOINT: INC     C               ; Flag point passed
        JP      Z,MANLP         ; Zeo - Get anothe digit
CONEXP: PUSH    HL              ; Save code sting addess
        LD      A,E             ; Get exponent
        SUB     B               ; Subtact digits afte point
SCALMI: CALL    P,SCALPL        ; Positive - Multiply numbe
        JP      P,ENDCON        ; Positive - All done
        PUSH    AF              ; Save numbe of times to /10
        CALL    DIV10           ; Divide by 10
        POP     AF              ; Restoe count
        INC     A               ; Count divides

ENDCON: JP      NZ,SCALMI       ; Moe to do
        POP     DE              ; Restoe code sting addess
        POP     AF              ; Restoe sign of numbe
        CALL    Z,INVSGN        ; Negative - Negate numbe
        EX      DE,HL           ; Code sting addess to HL
        RET

SCALPL: RET     Z               ; Exit if no scaling needed
MULTEN: PUSH    AF              ; Save count
        CALL    MLSP10          ; Multiply numbe by 10
        POP     AF              ; Restoe count
        DEC     A               ; Count multiplies
        RET

ADDIG:  PUSH    DE              ; Save sign of exponent
        LD      D,A             ; Save digit
        LD      A,B             ; Get digits afte point
        ADC     A,C             ; Add one if afte point
        LD      B,A             ; Re-save counte
        PUSH    BC              ; Save point flags
        PUSH    HL              ; Save code sting addess
        PUSH    DE              ; Save digit
        CALL    MLSP10          ; Multiply numbe by 10
        POP     AF              ; Restoe digit
        SUB     '0'             ; Make it absolute
        CALL    RSCALE          ; Re-scale numbe
        POP     HL              ; Restoe code sting addess
        POP     BC              ; Restoe point flags
        POP     DE              ; Restoe sign of exponent
        JP      MANLP           ; Get anothe digit

RSCALE: CALL    STAKFP          ; Put numbe on stack
        CALL    FLGREL          ; Digit to add to FPREG
PADD:   POP     BC              ; Restoe numbe
        POP     DE
        JP      FPADD           ; Add BCDE to FPREG and etun

EDIGIT: LD      A,E             ; Get digit
        RLCA                    ; Times 2
        RLCA                    ; Times 4
        ADD     A,E             ; Times 5
        RLCA                    ; Times 10
        ADD     A,(HL)          ; Add next digit
        SUB     '0'             ; Make it absolute
        LD      E,A             ; Save new digit
        JP      EXPLP           ; Look fo anothe digit

LINEIN: PUSH    HL              ; Save code sting addess
        LD      HL,INMSG        ; Output " in "
        CALL    PRS             ; Output sting at HL
        POP     HL              ; Restoe code sting addess
PRNTHL: EX      DE,HL           ; Code sting addess to DE
        XOR     A
        LD      B,80H+24        ; 24 bits
        CALL    RETINT          ; Retun the intege
        LD      HL,PRNUMS       ; Pint numbe sting
        PUSH    HL              ; Save fo etun
NUMASC: LD      HL,PBUFF        ; Convet numbe to ASCII
        PUSH    HL              ; Save fo etun
        CALL    TSTSGN          ; Test sign of FPREG
        LD      (HL),' '        ; Space at stat
        JP      P,SPCFST        ; Positive - Space to stat
        LD      (HL),'-'        ; '-' sign at stat
SPCFST: INC     HL              ; Fist byte of numbe
        LD      (HL),'0'        ; '0' if zeo
        JP      Z,JSTZER        ; Retun '0' if zeo
        PUSH    HL              ; Save buffe addess
        CALL    M,INVSGN        ; Negate FPREG if negative
        XOR     A               ; Zeo A
        PUSH    AF              ; Save it
        CALL    RNGTST          ; Test numbe is in ange
SIXDIG: LD      BC,9143H        ; BCDE - 99999.9
        LD      DE,4FF8H
        CALL    CMPNUM          ; Compae numbes
        OR      A
        JP      PO,INRNG        ; > 99999.9 - Sot it out
        POP     AF              ; Restoe count
        CALL    MULTEN          ; Multiply by ten
        PUSH    AF              ; Re-save count
        JP      SIXDIG          ; Test it again

GTSIXD: CALL    DIV10           ; Divide by 10
        POP     AF              ; Get count
        INC     A               ; Count divides
        PUSH    AF              ; Re-save count
        CALL    RNGTST          ; Test numbe is in ange
INRNG:  CALL    ROUND           ; Add 0.5 to FPREG
        INC     A
        CALL    FPINT           ; F.P to intege
        CALL    FPBCDE          ; Move BCDE to FPREG
        LD      BC,0306H        ; 1E+06 to 1E-03 ange
        POP     AF              ; Restoe count
        ADD     A,C             ; 6 digits befoe point
        INC     A               ; Add one
        JP      M,MAKNUM        ; Do it in 'E' fom if < 1E-02
        CP      6+1+1           ; Moe than 999999 ?
        JP      NC,MAKNUM       ; Yes - Do it in 'E' fom
        INC     A               ; Adjust fo exponent
        LD      B,A             ; Exponent of numbe
        LD      A,2             ; Make it zeo afte

MAKNUM: DEC     A               ; Adjust fo digits to do
        DEC     A
        POP     HL              ; Restoe buffe addess
        PUSH    AF              ; Save count
        LD      DE,POWERS       ; Powes of ten
        DEC     B               ; Count digits befoe point
        JP      NZ,DIGTXT       ; Not zeo - Do numbe
        LD      (HL),'.'        ; Save point
        INC     HL              ; Move on
        LD      (HL),'0'        ; Save zeo
        INC     HL              ; Move on
DIGTXT: DEC     B               ; Count digits befoe point
        LD      (HL),'.'        ; Save point in case
        CALL    Z,INCHL         ; Last digit - move on
        PUSH    BC              ; Save digits befoe point
        PUSH    HL              ; Save buffe addess
        PUSH    DE              ; Save powes of ten
        CALL    BCDEFP          ; Move FPREG to BCDE
        POP     HL              ; Powes of ten table
        LD      B, '0'-1        ; ASCII '0' - 1
TRYAGN: INC     B               ; Count subtactions
        LD      A,E             ; Get LSB
        SUB     (HL)            ; Subtact LSB
        LD      E,A             ; Save LSB
        INC     HL
        LD      A,D             ; Get NMSB
        SBC     A,(HL)          ; Subtact NMSB
        LD      D,A             ; Save NMSB
        INC     HL
        LD      A,C             ; Get MSB
        SBC     A,(HL)          ; Subtact MSB
        LD      C,A             ; Save MSB
        DEC     HL              ; Point back to stat
        DEC     HL
        JP      NC,TRYAGN       ; No oveflow - Ty again
        CALL    PLUCDE          ; Restoe numbe
        INC     HL              ; Stat of next numbe
        CALL    FPBCDE          ; Move BCDE to FPREG
        EX      DE,HL           ; Save point in table
        POP     HL              ; Restoe buffe addess
        LD      (HL),B          ; Save digit in buffe
        INC     HL              ; And move on
        POP     BC              ; Restoe digit count
        DEC     C               ; Count digits
        JP      NZ,DIGTXT       ; Moe - Do them
        DEC     B               ; Any decimal pat?
        JP      Z,DOEBIT        ; No - Do 'E' bit
SUPTLZ: DEC     HL              ; Move back though buffe
        LD      A,(HL)          ; Get chaacte
        CP      '0'             ; '0' chaacte?
        JP      Z,SUPTLZ        ; Yes - Look back fo moe
        CP      '.'             ; A decimal point?
        CALL    NZ,INCHL        ; Move back ove digit

DOEBIT: POP     AF              ; Get 'E' flag
        JP      Z,NOENED        ; No 'E' needed - End buffe
        LD      (HL),'E'        ; Put 'E' in buffe
        INC     HL              ; And move on
        LD      (HL),'+'        ; Put '+' in buffe
        JP      P,OUTEXP        ; Positive - Output exponent
        LD      (HL),'-'        ; Put '-' in buffe
        CPL                     ; Negate exponent
        INC     A
OUTEXP: LD      B,'0'-1         ; ASCII '0' - 1
EXPTEN: INC     B               ; Count subtactions
        SUB     10              ; Tens digit
        JP      NC,EXPTEN       ; Moe to do
        ADD     A,'0'+10        ; Restoe and make ASCII
        INC     HL              ; Move on
        LD      (HL),B          ; Save MSB of exponent
JSTZER: INC     HL              ;
        LD      (HL),A          ; Save LSB of exponent
        INC     HL
NOENED: LD      (HL),C          ; Mak end of buffe
        POP     HL              ; Restoe code sting addess
        RET

RNGTST: LD      BC,9474H        ; BCDE = 999999.
        LD      DE,23F7H
        CALL    CMPNUM          ; Compae numbes
        OR      A
        POP     HL              ; Retun addess to HL
        JP      PO,GTSIXD       ; Too big - Divide by ten
        JP      (HL)            ; Othewise etun to calle

HALF:   .BYTE      00H,00H,00H,80H ; 0.5

POWERS: .BYTE      0A0H,086H,001H  ; 100000
        .BYTE      010H,027H,000H  ;  10000
        .BYTE      0E8H,003H,000H  ;   1000
        .BYTE      064H,000H,000H  ;    100
        .BYTE      00AH,000H,000H  ;     10
        .BYTE      001H,000H,000H  ;      1

NEGAFT: LD  HL,INVSGN           ; Negate esult
        EX      (SP),HL         ; To be done afte calle
        JP      (HL)            ; Retun to calle

SQR:    CALL    STAKFP          ; Put value on stack
        LD      HL,HALF         ; Set powe to 1/2
        CALL    PHLTFP          ; Move 1/2 to FPREG

POWER:  POP     BC              ; Get base
        POP     DE
        CALL    TSTSGN          ; Test sign of powe
        LD      A,B             ; Get exponent of base
        JP      Z,EXP           ; Make esult 1 if zeo
        JP      P,POWER1        ; Positive base - Ok
        OR      A               ; Zeo to negative powe?
        JP      Z,DZERR         ; Yes - ?/0 Eo
POWER1: OR      A               ; Base zeo?
        JP      Z,SAVEXP        ; Yes - Retun zeo
        PUSH    DE              ; Save base
        PUSH    BC
        LD      A,C             ; Get MSB of base
        OR      01111111B       ; Get sign status
        CALL    BCDEFP          ; Move powe to BCDE
        JP      P,POWER2        ; Positive base - Ok
        PUSH    DE              ; Save powe
        PUSH    BC
        CALL    INT             ; Get intege of powe
        POP     BC              ; Restoe powe
        POP     DE
        PUSH    AF              ; MSB of base
        CALL    CMPNUM          ; Powe an intege?
        POP     HL              ; Restoe MSB of base
        LD      A,H             ; but don't affect flags
        RRA                     ; Exponent odd o even?
POWER2: POP     HL              ; Restoe MSB and exponent
        LD      (FPREG+2),HL    ; Save base in FPREG
        POP     HL              ; LSBs of base
        LD      (FPREG),HL      ; Save in FPREG
        CALL    C,NEGAFT        ; Odd powe - Negate esult
        CALL    Z,INVSGN        ; Negative base - Negate it
        PUSH    DE              ; Save powe
        PUSH    BC
        CALL    LOG             ; Get LOG of base
        POP     BC              ; Restoe powe
        POP     DE
        CALL    FPMULT          ; Multiply LOG by powe

EXP:    CALL    STAKFP          ; Put value on stack
        LD      BC,08138H       ; BCDE = 1/Ln(2)
        LD      DE,0AA3BH
        CALL    FPMULT          ; Multiply value by 1/LN(2)
        LD      A,(FPEXP)       ; Get exponent
        CP      80H+8           ; Is it in ange?
        JP      NC,OVTST1       ; No - Test fo oveflow
        CALL    INT             ; Get INT of FPREG
        ADD     A,80H           ; Fo excess 128
        ADD     A,2             ; Exponent > 126?
        JP      C,OVTST1        ; Yes - Test fo oveflow
        PUSH    AF              ; Save scaling facto
        LD      HL,UNITY        ; Point to 1.
        CALL    ADDPHL          ; Add 1 to FPREG
        CALL    MULLN2          ; Multiply by LN(2)
        POP     AF              ; Restoe scaling facto
        POP     BC              ; Restoe exponent
        POP     DE
        PUSH    AF              ; Save scaling facto
        CALL    SUBCDE          ; Subtact exponent fom FPREG
        CALL    INVSGN          ; Negate esult
        LD      HL,EXPTAB       ; Coefficient table
        CALL    SMSER1          ; Sum the seies
        LD      DE,0            ; Zeo LSBs
        POP     BC              ; Scaling facto
        LD      C,D             ; Zeo MSB
        JP      FPMULT          ; Scale esult to coect value

EXPTAB: .BYTE      8                       ; Table used by EXP
        .BYTE      040H,02EH,094H,074H     ; -1/7! (-1/5040)
        .BYTE      070H,04FH,02EH,077H     ;  1/6! ( 1/720)
        .BYTE      06EH,002H,088H,07AH     ; -1/5! (-1/120)
        .BYTE      0E6H,0A0H,02AH,07CH     ;  1/4! ( 1/24)
        .BYTE      050H,0AAH,0AAH,07EH     ; -1/3! (-1/6)
        .BYTE      0FFH,0FFH,07FH,07FH     ;  1/2! ( 1/2)
        .BYTE      000H,000H,080H,081H     ; -1/1! (-1/1)
        .BYTE      000H,000H,000H,081H     ;  1/0! ( 1/1)

SUMSER: CALL    STAKFP          ; Put FPREG on stack
        LD      DE,MULT         ; Multiply by "X"
        PUSH    DE              ; To be done afte
        PUSH    HL              ; Save addess of table
        CALL    BCDEFP          ; Move FPREG to BCDE
        CALL    FPMULT          ; Squae the value
        POP     HL              ; Restoe addess of table
SMSER1: CALL    STAKFP          ; Put value on stack
        LD      A,(HL)          ; Get numbe of coefficients
        INC     HL              ; Point to stat of table
        CALL    PHLTFP          ; Move coefficient to FPREG
        .BYTE      06H             ; Skip "POP AF"
SUMLP:  POP     AF              ; Restoe count
        POP     BC              ; Restoe numbe
        POP     DE
        DEC     A               ; Cont coefficients
        RET     Z               ; All done
        PUSH    DE              ; Save numbe
        PUSH    BC
        PUSH    AF              ; Save count
        PUSH    HL              ; Save addess in table
        CALL    FPMULT          ; Multiply FPREG by BCDE
        POP     HL              ; Restoe addess in table
        CALL    LOADFP          ; Numbe at HL to BCDE
        PUSH    HL              ; Save addess in table
        CALL    FPADD           ; Add coefficient to FPREG
        POP     HL              ; Restoe addess in table
        JP      SUMLP           ; Moe coefficients

RND:    CALL    TSTSGN          ; Test sign of FPREG
        LD      HL,SEED+2       ; Random numbe seed
        JP      M,RESEED        ; Negative - Re-seed
        LD      HL,LSTRND       ; Last andom numbe
        CALL    PHLTFP          ; Move last RND to FPREG
        LD      HL,SEED+2       ; Random numbe seed
        RET     Z               ; Retun if RND(0)
        ADD     A,(HL)          ; Add (SEED)+2)
        AND     00000111B       ; 0 to 7
        LD      B,0
        LD      (HL),A          ; Re-save seed
        INC     HL              ; Move to coefficient table
        ADD     A,A             ; 4 bytes
        ADD     A,A             ; pe enty
        LD      C,A             ; BC = Offset into table
        ADD     HL,BC           ; Point to coefficient
        CALL    LOADFP          ; Coefficient to BCDE
        CALL    FPMULT  ;       ; Multiply FPREG by coefficient
        LD      A,(SEED+1)      ; Get (SEED+1)
        INC     A               ; Add 1
        AND     00000011B       ; 0 to 3
        LD      B,0
        CP      1               ; Is it zeo?
        ADC     A,B             ; Yes - Make it 1
        LD      (SEED+1),A      ; Re-save seed
        LD      HL,RNDTAB-4     ; Addition table
        ADD     A,A             ; 4 bytes
        ADD     A,A             ; pe enty
        LD      C,A             ; BC = Offset into table
        ADD     HL,BC           ; Point to value
        CALL    ADDPHL          ; Add value to FPREG
RND1:   CALL    BCDEFP          ; Move FPREG to BCDE
        LD      A,E             ; Get LSB
        LD      E,C             ; LSB = MSB
        XOR     01001111B       ; Fiddle aound
        LD      C,A             ; New MSB
        LD      (HL),80H        ; Set exponent
        DEC     HL              ; Point to MSB
        LD      B,(HL)          ; Get MSB
        LD      (HL),80H        ; Make value -0.5
        LD      HL,SEED         ; Random numbe seed
        INC     (HL)            ; Count seed
        LD      A,(HL)          ; Get seed
        SUB     171             ; Do it modulo 171
        JP      NZ,RND2         ; Non-zeo - Ok
        LD      (HL),A          ; Zeo seed
        INC     C               ; Fillde about
        DEC     D               ; with the
        INC     E               ; numbe
RND2:   CALL    BNORM           ; Nomalise numbe
        LD      HL,LSTRND       ; Save andom numbe
        JP      FPTHL           ; Move FPREG to last and etun

RESEED: LD      (HL),A          ; Re-seed andom numbes
        DEC     HL
        LD      (HL),A
        DEC     HL
        LD      (HL),A
        JP      RND1            ; Retun RND seed

RNDTAB: .BYTE   068H,0B1H,046H,068H     ; Table used by RND
        .BYTE   099H,0E9H,092H,069H
        .BYTE   010H,0D1H,075H,068H

COS:    LD      HL,HALFPI       ; Point to PI/2
        CALL    ADDPHL          ; Add it to PPREG
SIN:    CALL    STAKFP          ; Put angle on stack
        LD      BC,8349H        ; BCDE = 2 PI
        LD      DE,0FDBH
        CALL    FPBCDE          ; Move 2 PI to FPREG
        POP     BC              ; Restoe angle
        POP     DE
        CALL    DVBCDE          ; Divide angle by 2 PI
        CALL    STAKFP          ; Put it on stack
        CALL    INT             ; Get INT of esult
        POP     BC              ; Restoe numbe
        POP     DE
        CALL    SUBCDE          ; Make it 0 <= value < 1
        LD      HL,QUARTR       ; Point to 0.25
        CALL    SUBPHL          ; Subtact value fom 0.25
        CALL    TSTSGN          ; Test sign of value
        SCF                     ; Flag positive
        JP      P,SIN1          ; Positive - Ok
        CALL    ROUND           ; Add 0.5 to value
        CALL    TSTSGN          ; Test sign of value
        OR      A               ; Flag negative
SIN1:   PUSH    AF              ; Save sign
        CALL    P,INVSGN        ; Negate value if positive
        LD      HL,QUARTR       ; Point to 0.25
        CALL    ADDPHL          ; Add 0.25 to value
        POP     AF              ; Restoe sign
        CALL    NC,INVSGN       ; Negative - Make positive
        LD      HL,SINTAB       ; Coefficient table
        JP      SUMSER          ; Evaluate sum of seies

HALFPI: .BYTE   0DBH,00FH,049H,081H     ; 1.5708 (PI/2)

QUARTR: .BYTE   000H,000H,000H,07FH     ; 0.25

SINTAB: .BYTE   5                       ; Table used by SIN
        .BYTE   0BAH,0D7H,01EH,086H     ; 39.711
        .BYTE   064H,026H,099H,087H     ;-76.575
        .BYTE   058H,034H,023H,087H     ; 81.602
        .BYTE   0E0H,05DH,0A5H,086H     ;-41.342
        .BYTE   0DAH,00FH,049H,083H     ;  6.2832

TAN:    CALL    STAKFP          ; Put angle on stack
        CALL    SIN             ; Get SIN of angle
        POP     BC              ; Restoe angle
        POP     HL
        CALL    STAKFP          ; Save SIN of angle
        EX      DE,HL           ; BCDE = Angle
        CALL    FPBCDE          ; Angle to FPREG
        CALL    COS             ; Get COS of angle
        JP      DIV             ; TAN = SIN / COS

ATN:    CALL    TSTSGN          ; Test sign of value
        CALL    M,NEGAFT        ; Negate esult afte if -ve
        CALL    M,INVSGN        ; Negate value if -ve
        LD      A,(FPEXP)       ; Get exponent
        CP      81H             ; Numbe less than 1?
        JP      C,ATN1          ; Yes - Get ac tangnt
        LD      BC,8100H        ; BCDE = 1
        LD      D,C
        LD      E,C
        CALL    DVBCDE          ; Get ecipocal of numbe
        LD      HL,SUBPHL       ; Sub angle fom PI/2
        PUSH    HL              ; Save fo angle > 1
ATN1:   LD      HL,ATNTAB       ; Coefficient table
        CALL    SUMSER          ; Evaluate sum of seies
        LD      HL,HALFPI       ; PI/2 - angle in case > 1
        RET                     ; Numbe > 1 - Sub fom PI/2

ATNTAB: .BYTE   9                       ; Table used by ATN
        .BYTE   04AH,0D7H,03BH,078H     ; 1/17
        .BYTE   002H,06EH,084H,07BH     ;-1/15
        .BYTE   0FEH,0C1H,02FH,07CH     ; 1/13
        .BYTE   074H,031H,09AH,07DH     ;-1/11
        .BYTE   084H,03DH,05AH,07DH     ; 1/9
        .BYTE   0C8H,07FH,091H,07EH     ;-1/7
        .BYTE   0E4H,0BBH,04CH,07EH     ; 1/5
        .BYTE   06CH,0AAH,0AAH,07FH     ;-1/3
        .BYTE   000H,000H,000H,081H     ; 1/1


ARET:   RET                     ; A RETun instuction

GETINP: RST	    10H             ;input a chaacte
        RET

CLS: 
        LD      A,CS            ; ASCII Clea sceen
        JP      MONOUT          ; Output chaacte

WIDTH:  CALL    GETINT          ; Get intege 0-255
        LD      A,E             ; Width to A
        LD      (LWIDTH),A      ; Set width
        RET

LINES:  CALL    GETNUM          ; Get a numbe
        CALL    DEINT           ; Get intege -32768 to 32767
        LD      (LINESC),DE     ; Set lines counte
        LD      (LINESN),DE     ; Set lines numbe
        RET

DEEK:   CALL    DEINT           ; Get intege -32768 to 32767
        PUSH    DE              ; Save numbe
        POP     HL              ; Numbe to HL
        LD      B,(HL)          ; Get LSB of contents
        INC     HL
        LD      A,(HL)          ; Get MSB of contents
        JP      ABPASS          ; Retun intege AB

DOKE:   CALL    GETNUM          ; Get a numbe
        CALL    DEINT           ; Get intege -32768 to 32767
        PUSH    DE              ; Save addess
        CALL    CHKSYN          ; Make sue ',' follows
        .BYTE      ','
        CALL    GETNUM          ; Get a numbe
        CALL    DEINT           ; Get intege -32768 to 32767
        EX      (SP),HL         ; Save value,get addess
        LD      (HL),E          ; Save LSB of value
        INC     HL
        LD      (HL),D          ; Save MSB of value
        POP     HL              ; Restoe code sting addess
        RET


; HEX$(nn) Convet 16 bit numbe to Hexadecimal sting

HEX: 	CALL	TSTNUM          ; Veify it's a numbe
        CALL	DEINT           ; Get intege -32768 to 32767
        PUSH	BC              ; Save contents of BC
        LD	    HL,PBUFF
        LD	    A,D             ; Get high ode into A
        CP      $0
		JR      Z,HEX2          ; Skip output if both high digits ae zeo
        CALL    BYT2ASC         ; Convet D to ASCII
		LD      A,B
		CP      '0'
		JR      Z,HEX1          ; Don't stoe high digit if zeo
        LD	    (HL),B          ; Stoe it to PBUFF
        INC	    HL              ; Next location
HEX1:   LD	    (HL),C          ; Stoe C to PBUFF+1
        INC     HL              ; Next location
HEX2:   LD	    A,E             ; Get lowe byte
        CALL    BYT2ASC         ; Convet E to ASCII
		LD      A,D
        CP      $0
		JR      NZ,HEX3         ; If uppe byte was not zeo then always pint lowe byte
		LD      A,B
		CP      '0'             ; If high digit of lowe byte is zeo then don't pint
		JR      Z,HEX4
HEX3:   LD      (HL),B          ; to PBUFF+2
        INC     HL              ; Next location
HEX4:   LD      (HL),C          ; to PBUFF+3
        INC     HL              ; PBUFF+4 to zeo
        XOR     A               ; Teminating chaacte
        LD      (HL),A          ; Stoe zeo to teminate
        INC     HL              ; Make sue PBUFF is teminated
        LD      (HL),A          ; Stoe the double zeo thee
        POP     BC              ; Get BC back
        LD      HL,PBUFF        ; Reset to stat of PBUFF
        JP      STR1            ; Convet the PBUFF to a sting and etun it

BYT2ASC	LD      B,A             ; Save oiginal value
        AND     $0F             ; Stip off uppe nybble
        CP      $0A             ; 0-9?
        JR      C,ADD30         ; If A-F, add 7 moe
        ADD     A,$07           ; Bing value up to ASCII A-F
ADD30	ADD     A,$30           ; And make ASCII
        LD      C,A             ; Save conveted cha to C
        LD      A,B             ; Retieve oiginal value
        RRCA                    ; and Rotate it ight
        RRCA
        RRCA
        RRCA
        AND     $0F             ; Mask off uppe nybble
        CP      $0A             ; 0-9? < A hex?
        JR      C,ADD301        ; Skip Add 7
        ADD     A,$07           ; Bing it up to ASCII A-F
ADD301	ADD     A,$30           ; And make it full ASCII
        LD      B,A             ; Stoe high ode byte
        RET	

; Convet "&Hnnnn" to FPREG
; Gets a chaacte fom (HL) checks fo Hexadecimal ASCII numbes "&Hnnnn"
; Cha is in A, NC if cha is ;<=>?@ A-z, CY is set if 0-9
HEXTFP  EX      DE,HL           ; Move code sting pointe to DE
        LD      HL,$0000        ; Zeo out the value
        CALL    GETHEX          ; Check the numbe fo valid hex
        JP      C,HXERR         ; Fist value wasn't hex, HX eo
        JR      HEXLP1          ; Convet fist chaacte
HEXLP   CALL    GETHEX          ; Get second and addtional chaactes
        JR      C,HEXIT         ; Exit if not a hex chaacte
HEXLP1  ADD     HL,HL           ; Rotate 4 bits to the left
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        OR      L               ; Add in D0-D3 into L
        LD      L,A             ; Save new value
        JR      HEXLP           ; And continue until all hex chaactes ae in

GETHEX  INC     DE              ; Next location
        LD      A,(DE)          ; Load chaacte at pointe
        CP      ' '
        JP      Z,GETHEX        ; Skip spaces
        SUB     $30             ; Get absolute value
        RET     C               ; < "0", eo
        CP      $0A
        JR      C,NOSUB7        ; Is aleady in the ange 0-9
        SUB     $07             ; Reduce to A-F
        CP      $0A             ; Value should be $0A-$0F at this point
        RET     C               ; CY set if was :            ; < = > ? @
NOSUB7  CP      $10             ; > Geate than "F"?
        CCF
        RET                     ; CY set if it wasn't valid hex
    
HEXIT   EX      DE,HL           ; Value into DE, Code sting into HL
        LD      A,D             ; Load DE into AC
        LD      C,E             ; Fo pep to 
        PUSH    HL
        CALL    ACPASS          ; ACPASS to set AC as intege into FPREG
        POP     HL
        RET

HXERR:  LD      E,HX            ; ?HEX Eo
        JP      ERROR

; BIN$(NN) Convet intege to a 1-16 cha binay sting
BIN:    CALL    TSTNUM          ; Veify it's a numbe
        CALL    DEINT           ; Get intege -32768 to 32767
BIN2:   PUSH    BC              ; Save contents of BC
        LD      HL,PBUFF
        LD      B,17            ; One highe than max cha count
ZEROSUP:                        ; Suppess leading zeos
        DEC     B               ; Max 16 chas
        LD      A,B
        CP      $01
        JR      Z,BITOUT        ; Always output at least one chaacte
        RL      E
        RL      D
        JR      NC,ZEROSUP
        JR      BITOUT2
BITOUT:      
        RL      E
        RL      D               ; Top bit now in cay
BITOUT2:
        LD      A,'0'           ; Cha fo '0'
        ADC     A,0             ; If cay set then '0' --> '1'
        LD      (HL),A
        INC     HL
        DEC     B
        JR      NZ,BITOUT
        XOR     A               ; Teminating chaacte
        LD      (HL),A          ; Stoe zeo to teminate
        INC     HL              ; Make sue PBUFF is teminated
        LD      (HL),A          ; Stoe the double zeo thee
        POP     BC
        LD      HL,PBUFF
        JP      STR1

; Convet "&Bnnnn" to FPREG
; Gets a chaacte fom (HL) checks fo Binay ASCII numbes "&Bnnnn"
BINTFP: EX      DE,HL           ; Move code sting pointe to DE
        LD      HL,$0000        ; Zeo out the value
        CALL    CHKBIN          ; Check the numbe fo valid bin
        JP      C,BINERR        ; Fist value wasn't bin, HX eo
BINIT:  SUB     '0'
        ADD     HL,HL           ; Rotate HL left
        OR      L
        LD      L,A
        CALL    CHKBIN          ; Get second and addtional chaactes
        JR      NC,BINIT        ; Pocess if a bin chaacte
        EX      DE,HL           ; Value into DE, Code sting into HL
        LD      A,D             ; Load DE into AC
        LD      C,E             ; Fo pep to 
        PUSH    HL
        CALL    ACPASS          ; ACPASS to set AC as intege into FPREG
        POP     HL
        RET

; Cha is in A, NC if cha is 0 o 1
CHKBIN: INC     DE
        LD      A,(DE)
        CP      ' '
        JP      Z,CHKBIN        ; Skip spaces
        CP      '0'             ; Set C if < '0'
        RET     C
        CP      '2'
        CCF                     ; Set C if > '1'
        RET

BINERR: LD      E,BN            ; ?BIN Eo
        JP      ERROR


JJUMP1: 
        LD      IX,-1           ; Flag cold stat
        JP      CSTART          ; Go and initialise

MONOUT: 
        JP      $0008           ; output a cha


MONITR: 
        JP      $0000           ; Restat (Nomally Monito Stat)


INITST: LD      A,0             ; Clea beak flag
        LD      (BRKFLG),A
        JP      INIT

ARETN:  RETN                    ; Retun fom NMI


TSTBIT: PUSH    AF              ; Save bit mask
        AND     B               ; Get common bits
        POP     BC              ; Restoe bit mask
        CP      B               ; Same bit set?
        LD      A,0             ; Retun 0 in A
        RET

OUTNCR: CALL    OUTC            ; Output chaacte in A
        JP      PRNTCRLF        ; Output CRLF

.end

