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
	.byte $4E, $45, $53, $1A ; ASCII "NES" followed by MS-DOS end-of-file
	.byte 2 ; 2x 16KB PRG code
	.byte 1 ; 1x  8KB CHR data
	.byte $00 ; Horizontal mirroring
	.byte $00 ; Mapper 0

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
PPU_SCROLL = $2005 ; https://www.nesdev.org/wiki/PPU_registers#Scroll_.28.242005.29_.3E.3E_write_x2
PPU_ADDR = $2006 ; https://www.nesdev.org/wiki/PPU_registers#Address_.28.242006.29_.3E.3E_write_x2
PPU_DATA = $2007 ; https://www.nesdev.org/wiki/PPU_registers#Data_.28.242007.29_.3C.3E_read.2Fwrite

APU_DMC = $4010 ; https://www.nesdev.org/wiki/APU#DMC_($4010%E2%80%93$4013)

OAM_DMA = $4014 ; https://www.nesdev.org/wiki/PPU_registers#OAM_DMA_.28.244014.29_.3E_write

APU_FRAME_COUNTER = $4017 ; https://www.nesdev.org/wiki/APU#Frame_Counter_.28.244017.29

PARTICLE_TILE_COUNT = $39

ptr = $10 ; $10-$11, temporary variable
X_ARRAY = $40 ; $40-$7f
Y_ARRAY = $80 ; $80-$bf
STATE_ARRAY = $c0 ; $c0-$ff
BACKGROUND_BUFFER = $0200 ; $0200-$05bf
NAMETABLE_0 = $2000 ; $2000-$23ff

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

; Clears $0000 to $07ff; all 2 KB of RAM
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

; load_background:
; 	bit PPU_STATUS ; Reset the address latch
; 	lda #$20
; 	sta PPU_ADDR ; High byte
; 	lda #$00
; 	sta PPU_ADDR ; Low byte

; 	ldx #$00
; load_background_loop:
; 	lda background, x
; 	sta PPU_DATA
; 	inx
; 	cpx #$80
; 	bne load_background_loop

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

	lda #2 ; Particle tile count
	sta PARTICLE_TILE_COUNT

add_particle_1:
	lda #2
	sta X_ARRAY+0
	lda #2
	sta Y_ARRAY+0
	lda #1
	sta STATE_ARRAY+0

copy_particle_1_to_background_buffer:
	lda Y_ARRAY+0
	lsr ; Now /2
	lsr ; Now /4
	lsr ; Now /8
	clc
	adc #>BACKGROUND_BUFFER ; Add high byte
	sta ptr+1

	lda Y_ARRAY+0
	asl ; Now x2
	asl ; Now x4
	asl ; Now x8
	asl ; Now x16
	asl ; Now x32
	clc
	adc X_ARRAY+0
	tay
	lda STATE_ARRAY+0
	sta (ptr),y

add_particle_2:
	lda #2
	sta X_ARRAY+1
	lda #3
	sta Y_ARRAY+1
	lda #14
	sta STATE_ARRAY+1

copy_particle_2_to_background_buffer:
	lda Y_ARRAY+1
	lsr ; Now /2
	lsr ; Now /4
	lsr ; Now /8
	clc
	adc #>BACKGROUND_BUFFER ; Add high byte
	sta ptr+1

	lda Y_ARRAY+1
	asl ; Now x2
	asl ; Now x4
	asl ; Now x8
	asl ; Now x16
	asl ; Now x32
	clc
	adc X_ARRAY+1
	tay
	lda STATE_ARRAY+1
	sta (ptr),y

scroll:
	; TODO: Why doesn't this fix the camera's position in the first frame?
	; bit PPU_STATUS
	; lda #0 ; Camera position x
	; sta PPU_SCROLL
	; lda #0 ; Camera position y
	; sta PPU_SCROLL

enable_rendering:
	; #%10000000 is "Generate an NMI at the start of the vertical blanking interval"
	; #$00000100 is "VRAM address increment per CPU read/write of PPUDATA (0: add 1, going across; 1: add 32, going down)"
	lda #%10000100
	sta PPU_CTRL

	; #%00001000 is "Show background"
	; #%00000010 is "Show background in leftmost 8 pixels of screen"
	lda #%00001010
	sta PPU_MASK
.endproc

.proc main
	; The particle evaluation order HAS to be from the bottom of the screen to the top,
	; in order to not have a tile merge with the one below it, when they're both falling:
	; [00
	;  10]
	; [01
	;  11]
	ldx PARTICLE_TILE_COUNT
	beq particle_tile_loop_end
	dex
particle_tile_loop:
	; TODO: If the particle has background below it AND the particle below it has NOT MOVED, it moves,
	; potentially creating a new particle below it (depending on whether there was already a partial background tile below it)
	; The old position's state is set to 0 on move
	; The particle sets its MOVED boolean to true in the upper nibble of the old position's state byte,
	; where there are four such booleans per tile
	; Tiles that have a clear lower nibble will be removed by the next main() loop,
	; but we need to keep it around for the current loop so nmi() can draw the clear tile


	dex
	bpl particle_tile_loop
particle_tile_loop_end:

	jmp main
.endproc

.proc nmi
	ldx PARTICLE_TILE_COUNT
	beq particle_tile_loop_end
	dex
particle_tile_loop:
	bit PPU_STATUS ; Reset the address latch

	lda Y_ARRAY, x ; Load y
	lsr ; Now /2
	lsr ; Now /4
	lsr ; Now /8
	clc
	adc #>NAMETABLE_0 ; Add high byte
	sta PPU_ADDR

	lda Y_ARRAY, x ; Load y
	asl ; Now x2
	asl ; Now x4
	asl ; Now x8
	asl ; Now x16
	asl ; Now x32
	clc
	adc X_ARRAY, x ; Add x
	sta PPU_ADDR

	lda STATE_ARRAY, x ; Load state
	sta PPU_DATA

	dex
	bpl particle_tile_loop
particle_tile_loop_end:

	bit PPU_STATUS
	lda #0 ; Camera position x
	sta PPU_SCROLL
	lda #0 ; Camera position y
	sta PPU_SCROLL

	rti
.endproc

; background:
; 	.byte 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; 	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; 	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; 	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

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
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %00001111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte $00, $00, $00, $00, $00, $00, $00, $00

	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte $00, $00, $00, $00, $00, $00, $00, $00
