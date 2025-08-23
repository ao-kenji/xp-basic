;==================================================================================
; Initializing, input and output routines for Grant's BASIC for LUNA XP
;==================================================================================

; Minimum 6850 ACIA interrupt driven serial I/O to run modified NASCOM Basic 4.7
; Full input buffering with incoming data hardware handshaking
; Handshake shows full before the buffer is totally filled to allow run-on from the sender

SER_BUFSIZE     .EQU     3FH
SER_FULLSIZE    .EQU     30H
SER_EMPTYSIZE   .EQU     5

serBuf          .EQU     $2000
basicStarted    .EQU     serBuf
TEMPSTACK       .EQU     $20ED ; Top of BASIC line input buffer so is "free ram" when BASIC resets

CR              .EQU     0DH
LF              .EQU     0AH
CS              .EQU     0CH             ; Clear screen

;------------------------------------------------------------------------------
; LUNAXP: HD647180(Z180) internal ports (not complete list)

TIME_RLDR0L	.EQU	00EH
TIME_RLDR0H	.EQU	00FH
TIME_TCR	.EQU	010H
DMA_DCNTL	.EQU	032H
INT_ITC		.EQU	034H
MEM_RCR		.EQU	036H
MMU_CBR		.EQU	038H
MMU_BBR		.EQU	039H
MMU_CBAR	.EQU	03AH
IO_ICR		.EQU	03FH

;------------------------------------------------------------------------------
; Reset

		.ORG $0000
RST00:		DI			;Disable interrupts
		JP	INITXP		;Initialize Hardware and go

;------------------------------------------------------------------------------
; TX a character over xpfe tty

		.ORG     0008H
RST08:		JR	TXA		;use JR, someone else touches 0x000AH?

;------------------------------------------------------------------------------
; RX a character over xpfe tty, hold here until char ready.

		.ORG 0010H
RST10:		JP      RXA

;------------------------------------------------------------------------------
; Check serial status

		.ORG 0018H
RST18:		JP      CKINCHAR

;------------------------------------------------------------------------------
; RST 38 - INTERRUPT VECTOR [ for IM 1 ]

		.ORG 0038H
RST38:		JR	INT_HANDLER

;------------------------------------------------------------------------------
INT_HANDLER:	EI			; dummy now
		RETI

;------------------------------------------------------------------------------
RXA:
	push hl			; save HL in case
	ld hl, XPFE_RX_FLAG
	ld a, (hl)		; is the flag set?
	or a
	jr nz, __getc0
	xor a			; if no, set the input char 0
	jr __getc1
__getc0:
	dec hl			; now HL points XPFE_RX_DATA
	ld a, (hl)		; A has the input char
	ld (hl), 0		; clear XPFE_RX_DATA, in case
	inc hl			; now HL points XPFE_RX_FLAG
	ld (hl), 0		; reset the flag
__getc1:
	pop hl			; restore HL
	ret

rts1:
                LD       A,(HL)
                POP      HL
                EI
                RET                      ; Char ready in A

;------------------------------------------------------------------------------
TXA:
	push hl
	push af
	ld hl, XPFE_TX_FLAG
__putc0:
	ld a, (hl)		; is the flag cleared?
	or a
	jr nz, __putc0		; if no, try again until cleared
	pop af
	dec hl			; now HL points XPFE_TX_DATA
	ld (hl), a		; write the output char in XPFE_TX_DATA
	inc hl			; now HL points XPFE_TX_FLAG
	ld (hl), 0ffh		; set the flag
	pop hl
	ret
;------------------------------------------------------------------------------
CKINCHAR:	LD	A, (XPFE_RX_FLAG)	; Get status
		CP	A, 0FFH			; if 0xff, there is a character
                RET

PRINTSTR:       LD       A,(HL)          ; Get character
                OR       A               ; Is it $00 ?
                RET      Z               ; Then RETurn on terminator
                RST      08H             ; Print it
                INC      HL              ; Next Character
                JR       PRINTSTR        ; Continue until $00
                RET
;------------------------------------------------------------------------------
INITXP:
;	initial set up HD647180
;
;	I/O control register
;	 - internal I/O address: 0000H - 003FH
	xor a
	out0 (IO_ICR), a

;	DMA/WAIT control register
;	 - no memory wait
;	 - 3 external I/O wait
;	 - disable DMA
	ld a, 0x20
	out0 (DMA_DCNTL), a

;	refresh control register
;	 - disable refresh controller (HD647180 on LUNA uses SRAM)
;	 - cycle interval 80 states (recommended for 6.144MHz clock)
	ld a, 0x03
	out0 (MEM_RCR), a

;	set MMU control registers
;	CBAR(3AH) = 80H
;	 - map 3port RAM first 32k to BANK, next 32k to COMMON1
	ld a, 0x80
	out0 (MMU_CBAR), a

;	CBR(38H) = 00H
;	 - common1 area
	xor a
	out0 (MMU_CBR), a

;	BBR(39H) = 00H
;	 - bank area
	out0 (MMU_BBR), a

;	memory set up done, set the stack
               LD        HL,TEMPSTACK    ; Temp stack
               LD        SP,HL           ; Set up a temporary stack

;
;	Set up system tick timer
;

;	Disable Timer Ch0
	xor a
	out0 (TIME_TCR), a

;	Set RDLR0L and RDLR0H
;	The value is calculated by:
;	  CPU_CLOCK_KHZ * 1000 / Z180_TIMER_SCALE / TICKSPERSEC - 1
;	  = 6144 * 1000 / 20 / 40 - 1 = 7679 = 0x1dff
	ld hl, 0x1dff
	out0 (TIME_RLDR0L), l
	out0 (TIME_RLDR0H), h

;	Enable Timer Ch0
	ld a, 0x11			; enable downcounting & interrupt
;;	out0 (TIME_TCR), a		; not enabled for now

;	If we enable ITE1, it seems some wild interrupts are running on the
;	 real LUNA XP hardware.  So we use only IE0 for now.
;	set Interrupt/TRAP control reg
;	 - enable interrupt ITE0 (for now)

	ld a, 0x01
	out0 (INT_ITC), a

               IM        1
               EI

               LD        HL,SIGNON1      ; Sign-on message
               CALL      PRINTSTR        ; Output string
               LD        A,(basicStarted); Check the BASIC STARTED flag
               CP        'Y'             ; to see if this is power-up
               JR        NZ,COLDSTART    ; If not BASIC started then always do cold start
               LD        HL,SIGNON2      ; Cold/warm message
               CALL      PRINTSTR        ; Output string
CORW:
               CALL      RXA
               AND       %11011111       ; lower to uppercase
               CP        'C'
               JR        NZ, CHECKWARM
               RST       08H
               LD        A,$0D
               RST       08H
               LD        A,$0A
               RST       08H
COLDSTART:     LD        A,'Y'           ; Set the BASIC STARTED flag
               LD        (basicStarted),A
               JP        $0150           ; Start BASIC COLD
CHECKWARM:
               CP        'W'
               JR        NZ, CORW
               RST       08H
               LD        A,$0D
               RST       08H
               LD        A,$0A
               RST       08H
               JP        $0153           ; Start BASIC WARM
              
SIGNON1:       .BYTE     CS
               .BYTE     "Z80 SBC By Grant Searle",CR,LF,0
SIGNON2:       .BYTE     CR,LF
               .BYTE     "Cold or warm start (C or W)? ",0

;------------------------------------------------------------------------------
; LUNAXP: xpfe tty interface area

		.ORG 01F00H
XPFE_IF_BASE:	.BYTE	"XPFE"
XPFE_TX_DATA:	.BYTE	00H
XPFE_TX_FLAG:	.BYTE	00H
		.BYTE	00H, 00H	; Padding
XPFE_RX_DATA:	.BYTE	00H
XPFE_RX_FLAG:	.BYTE	00H
		.BYTE	00H, 00H	; Padding

;; .END
