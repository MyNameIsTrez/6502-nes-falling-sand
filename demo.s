; sei | Set Interrupt Disable Status | 1 -> I
; cld | Clear Decimal Mode | 0 -> D
; ldx | Load Index X with Memory | M -> X
; stx | Store Index X in Memory | X -> M
; txs | Transfer Index X to Stack Register | X -> SP
; inx | Increment Index X by One | X + 1 -> X
; bit | Test Bits in Memory with Accumulator | bits 7 and 6 of operand are transferred to bit 7 and 6 of SR (N,V); the zero-flag is set according to the result of the operand AND the accumulator (set, if the result is zero, unset otherwise). This allows a quick check of a few bits at once without affecting any of the registers, other than the status register (SR). A AND M, M7 -> N, M6 -> V
; bpl | Branch on Result Plus | Branch on N = 0
; lda | Load Accumulator with Memory | M -> A
; sta | Store Accumulator in Memory | A -> M
; bne | Branch on Result not Zero | Branch on Z = 0
; jmp | Jump to New Location | TODO: ?: (PC+1) -> PCL , (PC+2) -> PCH
; cpx | Compare Memory and Index X | X - M
; rti | Return from Interrupt | Pull SR, pull PC from stack

; "PPU pattern table" defines tiles using pixels https://www.nesdev.org/wiki/PPU_pattern_tables
; "PPU nametable" defines background using tiles https://www.nesdev.org/wiki/PPU_nametables
; "PPU attribute table" defines 16x16 background metatile palettes https://www.nesdev.org/wiki/PPU_attribute_tables

.segment "HEADER"
	; .byte "NES", $1A ; iNES header identifier
	.byte $4E, $45, $53, $1A
	.byte 2 ; 2x 16KB PRG code
	.byte 1 ; 1x  8KB CHR data
	.byte $01, $00 ; Mapper 0, vertical mirroring

.segment "VECTORS"
	.addr nmi ; When an NMI happens (once per frame if enabled) the label 'nmi'
	.addr reset ; When the processor first turns on or is reset, it will jump to the label 'reset'
	.addr 0 ; External interrupt IRQ (unused)

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

; Main code segment for the program
.segment "CODE"

PPU_CTRL = $2000 ; https://www.nesdev.org/wiki/PPU_registers#Controller_.28.242000.29_.3E_write
PPU_MASK = $2001 ; https://www.nesdev.org/wiki/PPU_registers#Mask_.28.242001.29_.3E_write
PPU_STATUS = $2002 ; https://www.nesdev.org/wiki/PPU_registers#Status_.28.242002.29_.3C_read

OAM_ADDR = $2003 ; https://www.nesdev.org/wiki/PPU_registers#OAM_address_.28.242003.29_.3E_write
OAM_DATA = $2004 ; https://www.nesdev.org/wiki/PPU_registers#OAM_data_.28.242004.29_.3C.3E_read.2Fwrite
OAM_DMA = $4014 ; https://www.nesdev.org/wiki/PPU_registers#OAM_DMA_.28.244014.29_.3E_write

PPU_ADDR = $2006 ; https://www.nesdev.org/wiki/PPU_registers#Address_.28.242006.29_.3E.3E_write_x2
PPU_DATA = $2007 ; https://www.nesdev.org/wiki/PPU_registers#Data_.28.242007.29_.3C.3E_read.2Fwrite

APU_DMC = $4010 ; https://www.nesdev.org/wiki/APU#DMC_($4010%E2%80%93$4013)
APU_FRAME_COUNTER = $4017 ; https://www.nesdev.org/wiki/APU#Frame_Counter_.28.244017.29

.proc reset
	sei ; Disable IRQs
	cld ; Disable decimal mode
	ldx #$40
	stx APU_FRAME_COUNTER ; Disable APU frame IRQ

	; Initialize stack register
	ldx #$ff
	txs

	inx ; Now X = 0
	stx PPU_CTRL ; Disable NMI
	stx PPU_MASK ; Disable rendering
	stx APU_DMC ; Disable DMC IRQs

	bit PPU_STATUS ; Clear the VBL flag if it was set at reset time

; First wait for vblank to make sure PPU is ready
vblank_wait1:
	bit PPU_STATUS ; Transfer the 7th vblank bit to the N flag
	bpl vblank_wait1 ; Jump while not in vblank

; Clears $0000 to $07ff
clear_memory:
	lda #$00 ; A = 0
	sta $0000, x ; $0000 = A, $0001 = A, ... , $00ff = A
	sta $0100, x
	sta $0200, x
	sta $0300, x
	sta $0400, x
	sta $0500, x
	sta $0600, x
	sta $0700, x

	; Loop 256 times
	inx
	bne clear_memory

; Second wait for vblank, PPU is ready after this
vblank_wait2:
	bit PPU_STATUS
	bpl vblank_wait2

load_palettes:
	; Reading the status register clears the address latch for the upcoming PPU_ADDR stores
	; https://retrocomputing.stackexchange.com/a/8755/27499
	lda PPU_STATUS

	; Send #$3f00 to PPU_ADDR
	; VRAM #$3f00 to #$3f0f is the background palette
	; https://www.nesdev.org/wiki/PPU_palettes#Memory_Map
	lda #$3f
	sta PPU_ADDR ; PPU_ADDR = #$3f, high byte first
	lda #$00
	sta PPU_ADDR ; PPU_ADDR = #$00

	ldx #$00
load_palettes_loop:
	; Write palette byte
	lda palettes, x
	sta PPU_DATA

	; Repeat 32 times
	inx
	cpx #$20
	bne load_palettes_loop

load_background:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0
load_background_loop:
  LDA background, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$80              ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE load_background_loop  ; Branch to load_background_loop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down


load_attributes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
load_attributes_loop:
  LDA attributes, x      ; load data from address (attributes + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE load_attributes_loop  ; Branch to load_attributes_loop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down

enable_rendering:
	; #%10000000 is "Generate an NMI at the start of the vertical blanking interval"
	lda #%10000000
	sta PPU_CTRL

	; #%00001000 is "Show background"
	; #%00000010 is "Show background in leftmost 8 pixels of screen"
	lda #%00001010
	sta PPU_MASK
.endproc

.proc main
	jmp main
.endproc

.proc nmi
	; Set SPR-RAM address to 0 in OAMADDR
	ldx #$00
	stx OAM_ADDR

	; Copy $0200-$02ff into OAM
	lda #$02
	sta OAM_DMA

	rti
.endproc

background:
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

attributes:
  .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

palettes:
	G = $00 ; Gray
	B = $0f ; Black
	W = $20 ; White
	C = $21 ; Cyan
	O = $27 ; Orange

	; Background Palette
	; Background0, Color1, Color2, Color3
	; Note: A sprite palette's background overwrites the background palette's
	; background color that has the same palette number, so sprite_bg2 -> background_bg2
	.byte W,G,C,O
	.byte W,G,C,O
	.byte W,G,C,O
	.byte W,G,C,O

	; Sprite Palette
	.byte W,G,C,O
	.byte W,G,C,O
	.byte W,G,C,O
	.byte W,G,C,O

; Character memory
.segment "CHARS"
	; Character H
	; Bitplane 0 (low)
	.byte %11111111
	.byte %11000011
	.byte %10100101
	.byte %10000001
	.byte %10100101
	.byte %10111001
	.byte %10000001
	.byte %11111111
	; Bitplane 1 (high)
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	; Character E
	.byte %01111110
	.byte %11111111
	.byte %11000011
	.byte %11111111
	.byte %11111111
	.byte %11000011
	.byte %11000011
	.byte %11000011
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	; Character L
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %11111111
	.byte %11111111
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	; Character O
	.byte %01111110
	.byte %11100111
	.byte %11000011
	.byte %11000011
	.byte %11000011
	.byte %11000011
	.byte %11100111
	.byte %01111110
	.byte $00, $00, $00, $00, $00, $00, $00, $00
