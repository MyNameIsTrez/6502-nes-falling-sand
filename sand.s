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
	.byte $4E, $45, $53, $1A ; Magic signature bytes
	.byte 1 ; 1x 16KB PRG code
	.byte 1 ; 1x  8KB CHR data
	.byte $00 ; Horizontal mirroring
	.byte $00 ; Mapper 0

.segment "VECTORS"
	.addr nmi ; When an NMI happens (once per frame if enabled) the label 'nmi'
	.addr reset ; When the processor first turns on or is reset, it will jump to the label 'reset'
	.addr 0 ; External interrupt IRQ (unused)

.segment "BACKGROUND_ROM"
; background_rom:
; 	.byte $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
; 	.byte $f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f
; 	.byte $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0

; background_rom:
; 	.byte $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
; 	.byte $1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1,$1
; 	.byte $2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2
; 	.byte $2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2,$2
; 	.byte $3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3
; 	.byte $3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3
; 	.byte $4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4
; 	.byte $4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4,$4
; 	.byte $5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5
; 	.byte $5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5,$5
; 	.byte $6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6
; 	.byte $6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6,$6
; 	.byte $7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7
; 	.byte $7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7,$7
; 	.byte $8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8
; 	.byte $8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8
; 	.byte $9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9
; 	.byte $9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9,$9
; 	.byte $a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a
; 	.byte $a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a
; 	.byte $b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b
; 	.byte $b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b
; 	.byte $c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c
; 	.byte $c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c
; 	.byte $d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d
; 	.byte $d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d
; 	.byte $e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e
; 	.byte $e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e
; 	.byte $f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f
; 	.byte $f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f

background_rom:
	.byte 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

.segment "BACKGROUND_RAM"
	background: .res 960

.segment "ZEROPAGE"
	; Scratch registers
	R0: .res 1
	R1: .res 1
	R2: .res 1
	R3: .res 1
	R4: .res 1
	R5: .res 1
	R6: .res 1
	R7: .res 1
	R8: .res 1
	R9: .res 1
	R10: .res 1
	R11: .res 1
	R12: .res 1
	R13: .res 1
	R14: .res 1
	R15: .res 1

	register_a_backup: .res 1
	register_x_backup: .res 1
	register_y_backup: .res 1

	frame_ready: .res 1
	row_activity: .res 1
	lowest_active_row: .res 1
	highest_active_row: .res 1
	frame_count: .res 1
	updated_tile_count: .res 1

	; Up to how many particles can be updated per nmi
	; If this is upped a lot, the arrays will be outside of the zero page, which'll slow down the code slightly
	; TODO: Use MAX_PARTICLES
	; MAX_PARTICLES = 64_UPDATES_PER_FRAME

	; previous_row: .res 32

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

.segment "CODE"
	NAMETABLE_0 = $2000 ; $2000-$23ff

	PPU_CONTROL = $2000 ; https://www.nesdev.org/wiki/PPU_registers#Controller_.28.242000.29_.3E_write
	PPU_MASK = $2001 ; https://www.nesdev.org/wiki/PPU_registers#Mask_.28.242001.29_.3E_write
	PPU_STATUS = $2002 ; https://www.nesdev.org/wiki/PPU_registers#Status_.28.242002.29_.3C_read
	PPU_SCROLL = $2005 ; https://www.nesdev.org/wiki/PPU_registers#Scroll_.28.242005.29_.3E.3E_write_x2
	PPU_ADDR = $2006 ; https://www.nesdev.org/wiki/PPU_registers#Address_.28.242006.29_.3E.3E_write_x2
	PPU_DATA = $2007 ; https://www.nesdev.org/wiki/PPU_registers#Data_.28.242007.29_.3C.3E_read.2Fwrite

	APU_DMC = $4010 ; https://www.nesdev.org/wiki/APU#DMC_($4010%E2%80%93$4013)

	OAM_DMA = $4014 ; https://www.nesdev.org/wiki/PPU_registers#OAM_DMA_.28.244014.29_.3E_write

	APU_FRAME_COUNTER = $4017 ; https://www.nesdev.org/wiki/APU#Frame_Counter_.28.244017.29

.proc reset
	sei ; Disable IRQs
	cld ; Disable decimal mode
	ldx #%01000000 ; Disable APU frame IRQ
	stx APU_FRAME_COUNTER

	; Initialize stack register
	; The stack occupies page $01
	ldx #$ff
	txs

	inx ; Now X = 0
	stx PPU_CONTROL ; Disable NMI
	stx PPU_MASK ; Disable rendering
	stx APU_DMC ; Disable DMC IRQs

	bit PPU_STATUS ; Clear the VBL flag if it was set at reset time

; First wait for vblank to make sure PPU is ready
vblank_wait1:
	bit PPU_STATUS ; Transfer the 7th vblank bit to the N flag
	bpl vblank_wait1 ; Jump while not in vblank

; Clears $0000 to $07ff; all 2 KB of RAM
clear_memory:
	lda #0
	.repeat 8, i
	sta $0100*i, x
	.endrepeat

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
	bit PPU_STATUS
	; Send #$3f00 to PPU_ADDR
	; VRAM #$3f00 to #$3f0f is the background palette
	; https://www.nesdev.org/wiki/PPU_palettes#Memory_Map
	lda #$3f
	sta PPU_ADDR ; High byte
	lda #$00
	sta PPU_ADDR ; Low byte

	ldx #$00
load_palettes_loop:
	; Write palette byte
	lda palettes, x
	sta PPU_DATA
	inx
	cpx #$20
	bne load_palettes_loop

load_attributes:
	bit PPU_STATUS ; Reset the address latch
	lda #$23
	sta PPU_ADDR ; High byte
	lda #$C0
	sta PPU_ADDR ; Low byte

	ldx #$00
load_attributes_loop:
	lda attributes, x
	sta PPU_DATA
	inx
	cpx #$08
	bne load_attributes_loop

load_background:
	; It's fastest to just let the first row get potentially uselessly updated on startup
	lda #1
	sta row_activity

	bit PPU_STATUS ; Reset the address latch

	; Initialize PPU_ADDR
	lda #>NAMETABLE_0 ; Add high byte
	sta PPU_ADDR
	lda #0
	sta PPU_ADDR

	background_rom_ptr = R0 ; Also uses R1
	background_ptr = R2 ; Also uses R3

	; Not strictly necessary
	sta background_rom_ptr+0
	sta background_ptr+0

	ldx #0
row_loop:
	txa ; Get tile y
	lsr ; Now /2
	lsr ; Now /4
	lsr ; Now /8
	tay

	clc
	adc #>background_rom ; Add high byte
	sta background_rom_ptr+1

	tya
	clc
	adc #>background ; Add high byte
	sta background_ptr+1

	; If you're running out space for code, roll this back into a loop
	.repeat 32, column ; For all 32 columns
	txa ; Get tile y
	asl ; Now x2
	asl ; Now x4
	asl ; Now x8
	asl ; Now x16
	asl ; Now x32
	clc
	adc #column ; Add tile x
	tay

	lda (background_rom_ptr),y ; Load ROM tile state

	sta (background_ptr),y ; Write tile state
	sta PPU_DATA ; Write tile state
	.endrepeat

	inx
	cpx #30 ; Highest row
	beq row_loop_end
	jmp row_loop
row_loop_end:

enable_rendering:
	bit PPU_STATUS
	lda #0 ; Camera position x
	sta PPU_SCROLL
	lda #0 ; Camera position y
	sta PPU_SCROLL

	; #%10000000 is "Generate an NMI at the start of the vertical blanking interval"
	lda #%10000000
	sta PPU_CONTROL

	; #%00001000 is "Show background"
	; #%00000010 is "Show background in leftmost 8 pixels of screen"
	lda #%00001010
	sta PPU_MASK
.endproc

; Explanation:
; The particle evaluation order HAS to be from the bottom of the screen to the top,
; in order to not have a tile merge with the one below it, when they're both falling:
; [00
;  10]
; [01
;  11]
; Start at the bottom row, and loop over all tiles from left to right.
; For every tile, move the bottom two particles, and then the top two particles.
; If a particle has air below it, it moves down by pushing its old X, Y, and state with itself removed (`and #1<<3; Bit 3`). It then pushes X, Y+1, and the state below it with itself added to it (`ora #1<<3; Bit 3`)
; If it didn't have air below it, it uses `lda frame_count` `and #1` to decide whether to move diagonally down either left or right. It moves by pushing its old X, Y, and state with itself removed. It then pushes X, Y+-1, and the state diagonally below it with itself added to it.
;     [00
;      10] <- If this one falls left, it might try to push a new tile state, while that tile may have had an update pushed earlier. This should be fine if the nmi array loop starts at index 0, and previous_row is kept up-to-date at all times.
; [00 [10
;  00] 11]
;
; Old idea, where updated_tile_count is *not* reset to 0 at the start of main():
; If the particle has background below it AND the particle below it has NOT MOVED, it moves,
; potentially creating a new particle below it (depending on whether there was already a partial background tile below it).
; If the particle moved, the old position's MOVED corner bit is set to 1.
; Clear tiles will be removed by the next main() loop,
; but we need to keep them around for the current loop so nmi() can draw them having been cleared.
.proc main
wait_on_frame_ready:
	lda frame_ready
	beq wait_on_frame_ready

	ldx row_activity
	bne update_rows
	jmp row_loop_end
update_rows:
	lda #0
	sta row_activity

	ldx highest_active_row

	background_ptr = R0 ; Also uses R1
	sta background_ptr+0

	; Make a solid floor below the screen
	; lda #$ff
	; .repeat 32, i
	; sta previous_row+i
	; .endrepeat

	; TODO: Use Lua to do this during runtime
	; .assert *background_ptr==0, error, "Low byte of background_ptr should always be 0"
row_loop:
	txa ; Get tile y
	lsr ; Now /2
	lsr ; Now /4
	lsr ; Now /8
	clc
	adc #>background ; Add high byte
	sta background_ptr+1

	.repeat 32, column ; For all 32 columns
	.scope
	txa ; Get tile y
	asl ; Now x2
	asl ; Now x4
	asl ; Now x8
	asl ; Now x16
	asl ; Now x32
	clc
	adc column ; Add tile x
	tay

	lda (background_ptr),y ; Load tile state

	beq tile_end ; When the tile is clear

	brk ; TODO: Remove this

	; TODO: Go to row_loop_end if PARTICLE_ARRAY_LENGTH is reached after pushing one

	; lda state_array, x ; Load state
	; pha ; Push tile state

	; lda y_array, x ; Load particle y
	; lsr ; Now /2
	; lsr ; Now /4
	; lsr ; Now /8
	; clc
	; adc #>NAMETABLE_0 ; Add high byte
	; pha ; Push tile y

	; lda y_array, x ; Load particle y
	; asl ; Now x2
	; asl ; Now x4
	; asl ; Now x8
	; asl ; Now x16
	; asl ; Now x32
	; clc
	; adc x_array, x ; Add particle x
	; pha ; Push tile x

tile_end:
	.endscope
	.endrepeat

	; sta previous_row

	dex
	cpx lowest_active_row
	bmi row_loop_end
	jmp row_loop
row_loop_end:

	; TODO: Get rid of this, in favor of a RNG boolean LUT, rather than frame_count&1
	inc frame_count

	lda #0
	sta frame_ready

	jmp main
.endproc

.proc nmi
	; Save registers
	sta register_a_backup
	stx register_x_backup
	sty register_y_backup

	lda frame_ready
	bne ppu_done

	ldx updated_tile_count
	beq updated_tiles_loop_end
	dex
updated_tiles_loop:
	bit PPU_STATUS ; Reset the address latch

	; TODO: Do the x,y -> ppuaddr1,ppuaddr2 ahead of time in main!

	pla
	sta PPU_ADDR ; Tile x
	pla
	sta PPU_ADDR ; Tile y
	pla
	sta PPU_DATA ; Tile state

	dex
	bpl updated_tiles_loop

	lda #0
	sta updated_tile_count
updated_tiles_loop_end:

	; Setting PPU_CONTROL is important to guarantee that bits 0 and 1 are set to 0,
	; because these bits are used as the ninth bits of the camera's x and y
	; #%10000000 is "Generate an NMI at the start of the vertical blanking interval"
	lda #%10000000
	sta PPU_CONTROL

	bit PPU_STATUS
	lda #0 ; Camera position x
	sta PPU_SCROLL
	lda #0 ; Camera position y
	sta PPU_SCROLL

	lda #1
	sta frame_ready
ppu_done:

	; Restore registers
	lda register_a_backup
	ldx register_x_backup
	ldy register_y_backup

	rti
.endproc

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
	.byte B,O,C,G
	.byte B,O,C,G
	.byte B,O,C,G
	.byte B,O,C,G

	; Sprite Palette
	.byte B,O,C,G
	.byte B,O,C,G
	.byte B,O,C,G
	.byte B,O,C,G

; Character memory
.segment "CHARS"
	; Bitplane 0 (low bit)
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	; Bitplane 1 (high bit)
	.byte 0,0,0,0,0,0,0,0

	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte 0,0,0,0,0,0,0,0

	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte 0,0,0,0,0,0,0,0

	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte 0,0,0,0,0,0,0,0

	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte 0,0,0,0,0,0,0,0

	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte 0,0,0,0,0,0,0,0

	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte 0,0,0,0,0,0,0,0

	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte 0,0,0,0,0,0,0,0

	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte 0,0,0,0,0,0,0,0

	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte 0,0,0,0,0,0,0,0

	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte 0,0,0,0,0,0,0,0

	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte 0,0,0,0,0,0,0,0

	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte 0,0,0,0,0,0,0,0

	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte 0,0,0,0,0,0,0,0

	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte 0,0,0,0,0,0,0,0

	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte 0,0,0,0,0,0,0,0
