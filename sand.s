; To run this program: cl65 -C map.cfg -l sand.lst -g --ld-args --dbgfile,sand.dbg --verbose --target nes -o sand.nes sand.s
; With this as map.cfg:
; #
; # Linker script for NROM-128 games
; # Copyright 2010-2014 Damian Yerrick
; #
; # Copying and distribution of this file, with or without
; # modification, are permitted in any medium without royalty
; # provided the copyright notice and this notice are preserved.
; # This file is offered as-is, without any warranty.
; #
; MEMORY {
;   ZP:     start = $10, size = $f0, type = rw;
;   # use first $10 zeropage locations as locals
;   HEADER: start = 0, size = $0010, type = ro, file = %O, fill=yes, fillval=$00;
;   RAM:    start = $0200, size = $03c0, type = rw;
;   ROM7:   start = $C000, size = $4000, type = ro, file = %O, fill=yes, fillval=$FF;
;   CHRROM: start = $0000, size = $2000, type = ro, file = %O, fill=yes, fillval=$FF;
; }

; SEGMENTS {
;   HEADER:         load = HEADER, type = ro, align = $10;
;   ZEROPAGE:       load = ZP, type = zp;
; #   DATA:         load = ROM7, type = ro, define = yes, align = $100;
;   BACKGROUND_ROM: load = ROM7, type = ro, define = yes, align = $100;
;   BACKGROUND_RAM: load = ROM7, run = RAM, type = rw, define = yes, align = $100;
; #   BSS:          load = RAM, type = bss, define = yes, align = $100;
; #   DMC:          load = ROM7, type = ro, align = 64, optional = yes;
;   CODE:           load = ROM7, type = ro, align = $100;
; #   RODATA:       load = ROM7, type = ro, align = $100;
;   VECTORS:        load = ROM7, type = ro, start = $FFFA;
;   CHARS:          load = CHRROM, type = ro, align = 16, optional = yes;
; }

; FILES {
;   %O: format = bin;
; }

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
background_rom:
	.byte $54,$59,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $75,$95,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

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

	frame_ready: .res 1
	row_activity: .res 1
	lowest_active_row: .res 1
	highest_active_row: .res 1
	frame_count: .res 1
	updated_tile_count: .res 1
	nmi_stack_pointer_backup: .res 1

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

	; Up to how many particles can be updated per nmi
	; If this is upped a lot, the arrays will be outside of the zero page, which'll slow down the code slightly
	; TODO: Use MAX_PARTICLES
	; MAX_PARTICLES = 64

	; Near 0, leaving a bit of stack space for any potential jsr during nmi
	TOP_OF_UPDATED_TILES_STACK_OFFSET = 8

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

	lda (background_rom_ptr), y ; Load ROM tile state

	sta (background_ptr), y ; Write tile state
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

	y_divided_by_8 = R2

	nametable_tile_address_high_byte = R3

	top_of_stack_pointer = R4 ; Also uses R5
	lda #0
	sta top_of_stack_pointer+0
	lda #1
	sta top_of_stack_pointer+1

	tile_address_low_byte = R6

	next_tile_stack_offset = R7
	lda #TOP_OF_UPDATED_TILES_STACK_OFFSET
	sta next_tile_stack_offset

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
	sta y_divided_by_8

	clc
	adc #>background ; Add high byte
	sta background_ptr+1

	lda y_divided_by_8

	clc
	adc #>NAMETABLE_0 ; Add high byte
	sta nametable_tile_address_high_byte

	.repeat 32, column ; For all 32 columns
	.scope
	txa ; Get tile y
	asl ; Now x2
	asl ; Now x4
	asl ; Now x8
	asl ; Now x16
	asl ; Now x32
	clc
	adc #column ; Add tile x
	tay
	sty tile_address_low_byte

	lda (background_ptr), y ; Load tile state

	beq tile_end ; When the tile is clear

	; TODO: Go to row_loop_end if PARTICLE_ARRAY_LENGTH is reached after pushing one

	ldy next_tile_stack_offset

	lda nametable_tile_address_high_byte
	sta (top_of_stack_pointer), y ; Push tile address high byte
	iny

	lda tile_address_low_byte
	sta (top_of_stack_pointer), y ; Push tile address low byte
	iny

	lda #0
	sta (top_of_stack_pointer), y ; Push tile state
	iny

	sty next_tile_stack_offset

	; inc updated_tile_count

tile_end:
	.endscope
	.endrepeat

	; sta previous_row

	cpx lowest_active_row
	beq row_loop_end
	dex
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
	pha
	txa
	pha
	tya
	pha

	lda frame_ready
	bne ppu_done

	ldy updated_tile_count
	beq updated_tiles_loop_end

	; Save stack pointer
	tsx
	stx nmi_stack_pointer_backup

	; Prepare to start pulling updated tiles
	ldx #TOP_OF_UPDATED_TILES_STACK_OFFSET-1
	txs

updated_tiles_loop:
	dey

	bit PPU_STATUS ; Reset the address latch

	pla ; Pull tile address high byte
	sta PPU_ADDR
	pla ; Pull tile address low byte
	sta PPU_ADDR
	pla ; Pull tile state
	sta PPU_DATA

	bne updated_tiles_loop

	sty updated_tile_count

	ldx nmi_stack_pointer_backup
	txs
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
	pla
	tay
	pla
	tax
	pla

	rti
.endproc

attributes:
	.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

palettes:
	B = $0f ; Black; Air
	G = $10 ; Gray; Stone
	O = $27 ; Orange; Sand
	C = $21 ; Cyan; Water

	; Background Palette
	; Background0, Color1, Color2, Color3
	; Note: A sprite palette's background overwrites the background palette's
	; background color that has the same palette number
	.byte B,G,O,C
	.byte B,G,O,C
	.byte B,G,O,C
	.byte B,G,O,C

	; Sprite Palette
	.byte B,G,O,C
	.byte B,G,O,C
	.byte B,G,O,C
	.byte B,G,O,C

.segment "CHARS"
	; air,air,air,air at index $0
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; air,air,air,stone at index $1
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; air,air,air,sand at index $2
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; air,air,air,water at index $3
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; air,air,stone,air at index $4
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; air,air,stone,stone at index $5
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; air,air,stone,sand at index $6
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; air,air,stone,water at index $7
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; air,air,sand,air at index $8
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; air,air,sand,stone at index $9
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; air,air,sand,sand at index $a
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; air,air,sand,water at index $b
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; air,air,water,air at index $c
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; air,air,water,stone at index $d
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; air,air,water,sand at index $e
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; air,air,water,water at index $f
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; air,stone,air,air at index $10
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; air,stone,air,stone at index $11
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; air,stone,air,sand at index $12
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; air,stone,air,water at index $13
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; air,stone,stone,air at index $14
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; air,stone,stone,stone at index $15
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; air,stone,stone,sand at index $16
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; air,stone,stone,water at index $17
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; air,stone,sand,air at index $18
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; air,stone,sand,stone at index $19
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; air,stone,sand,sand at index $1a
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; air,stone,sand,water at index $1b
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; air,stone,water,air at index $1c
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; air,stone,water,stone at index $1d
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; air,stone,water,sand at index $1e
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; air,stone,water,water at index $1f
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; air,sand,air,air at index $20
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; air,sand,air,stone at index $21
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; air,sand,air,sand at index $22
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; air,sand,air,water at index $23
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; air,sand,stone,air at index $24
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; air,sand,stone,stone at index $25
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; air,sand,stone,sand at index $26
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; air,sand,stone,water at index $27
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; air,sand,sand,air at index $28
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; air,sand,sand,stone at index $29
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; air,sand,sand,sand at index $2a
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; air,sand,sand,water at index $2b
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; air,sand,water,air at index $2c
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; air,sand,water,stone at index $2d
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; air,sand,water,sand at index $2e
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; air,sand,water,water at index $2f
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; air,water,air,air at index $30
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; air,water,air,stone at index $31
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; air,water,air,sand at index $32
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; air,water,air,water at index $33
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; air,water,stone,air at index $34
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; air,water,stone,stone at index $35
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; air,water,stone,sand at index $36
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; air,water,stone,water at index $37
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; air,water,sand,air at index $38
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; air,water,sand,stone at index $39
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; air,water,sand,sand at index $3a
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; air,water,sand,water at index $3b
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; air,water,water,air at index $3c
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; air,water,water,stone at index $3d
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; air,water,water,sand at index $3e
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; air,water,water,water at index $3f
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; stone,air,air,air at index $40
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; stone,air,air,stone at index $41
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; stone,air,air,sand at index $42
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; stone,air,air,water at index $43
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; stone,air,stone,air at index $44
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; stone,air,stone,stone at index $45
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; stone,air,stone,sand at index $46
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; stone,air,stone,water at index $47
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; stone,air,sand,air at index $48
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,air,sand,stone at index $49
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,air,sand,sand at index $4a
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; stone,air,sand,water at index $4b
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; stone,air,water,air at index $4c
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,air,water,stone at index $4d
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,air,water,sand at index $4e
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; stone,air,water,water at index $4f
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; stone,stone,air,air at index $50
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; stone,stone,air,stone at index $51
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; stone,stone,air,sand at index $52
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; stone,stone,air,water at index $53
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; stone,stone,stone,air at index $54
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; stone,stone,stone,stone at index $55
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; High bit plane

	; stone,stone,stone,sand at index $56
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; stone,stone,stone,water at index $57
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; High bit plane

	; stone,stone,sand,air at index $58
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,stone,sand,stone at index $59
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,stone,sand,sand at index $5a
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; stone,stone,sand,water at index $5b
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; stone,stone,water,air at index $5c
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,stone,water,stone at index $5d
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,stone,water,sand at index $5e
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; stone,stone,water,water at index $5f
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; High bit plane

	; stone,sand,air,air at index $60
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; stone,sand,air,stone at index $61
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; stone,sand,air,sand at index $62
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; stone,sand,air,water at index $63
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; stone,sand,stone,air at index $64
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; stone,sand,stone,stone at index $65
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; stone,sand,stone,sand at index $66
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; stone,sand,stone,water at index $67
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; stone,sand,sand,air at index $68
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,sand,sand,stone at index $69
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,sand,sand,sand at index $6a
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; stone,sand,sand,water at index $6b
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; stone,sand,water,air at index $6c
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,sand,water,stone at index $6d
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,sand,water,sand at index $6e
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; stone,sand,water,water at index $6f
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; stone,water,air,air at index $70
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; stone,water,air,stone at index $71
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; stone,water,air,sand at index $72
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; stone,water,air,water at index $73
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; stone,water,stone,air at index $74
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; stone,water,stone,stone at index $75
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; High bit plane

	; stone,water,stone,sand at index $76
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; stone,water,stone,water at index $77
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; High bit plane

	; stone,water,sand,air at index $78
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,water,sand,stone at index $79
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,water,sand,sand at index $7a
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; stone,water,sand,water at index $7b
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; stone,water,water,air at index $7c
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,water,water,stone at index $7d
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; High bit plane

	; stone,water,water,sand at index $7e
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; stone,water,water,water at index $7f
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; High bit plane

	; sand,air,air,air at index $80
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; sand,air,air,stone at index $81
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; sand,air,air,sand at index $82
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; sand,air,air,water at index $83
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; sand,air,stone,air at index $84
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; sand,air,stone,stone at index $85
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; sand,air,stone,sand at index $86
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; sand,air,stone,water at index $87
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; sand,air,sand,air at index $88
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,air,sand,stone at index $89
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,air,sand,sand at index $8a
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; sand,air,sand,water at index $8b
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; sand,air,water,air at index $8c
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,air,water,stone at index $8d
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,air,water,sand at index $8e
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; sand,air,water,water at index $8f
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; sand,stone,air,air at index $90
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; sand,stone,air,stone at index $91
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; sand,stone,air,sand at index $92
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; sand,stone,air,water at index $93
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; sand,stone,stone,air at index $94
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; sand,stone,stone,stone at index $95
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; sand,stone,stone,sand at index $96
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; sand,stone,stone,water at index $97
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; sand,stone,sand,air at index $98
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,stone,sand,stone at index $99
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,stone,sand,sand at index $9a
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; sand,stone,sand,water at index $9b
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; sand,stone,water,air at index $9c
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,stone,water,stone at index $9d
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,stone,water,sand at index $9e
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; sand,stone,water,water at index $9f
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; sand,sand,air,air at index $a0
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; sand,sand,air,stone at index $a1
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; sand,sand,air,sand at index $a2
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; sand,sand,air,water at index $a3
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; sand,sand,stone,air at index $a4
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; sand,sand,stone,stone at index $a5
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; sand,sand,stone,sand at index $a6
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; sand,sand,stone,water at index $a7
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; sand,sand,sand,air at index $a8
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,sand,sand,stone at index $a9
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,sand,sand,sand at index $aa
	.byte $0,$0,$0,$0,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; sand,sand,sand,water at index $ab
	.byte $0,$0,$0,$0,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; sand,sand,water,air at index $ac
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,sand,water,stone at index $ad
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,sand,water,sand at index $ae
	.byte $0,$0,$0,$0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; sand,sand,water,water at index $af
	.byte $0,$0,$0,$0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; sand,water,air,air at index $b0
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; sand,water,air,stone at index $b1
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; sand,water,air,sand at index $b2
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; sand,water,air,water at index $b3
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; sand,water,stone,air at index $b4
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; sand,water,stone,stone at index $b5
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; sand,water,stone,sand at index $b6
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; sand,water,stone,water at index $b7
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; sand,water,sand,air at index $b8
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,water,sand,stone at index $b9
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,water,sand,sand at index $ba
	.byte $f,$f,$f,$f,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; sand,water,sand,water at index $bb
	.byte $f,$f,$f,$f,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; sand,water,water,air at index $bc
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,water,water,stone at index $bd
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; sand,water,water,sand at index $be
	.byte $f,$f,$f,$f,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; sand,water,water,water at index $bf
	.byte $f,$f,$f,$f,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; water,air,air,air at index $c0
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; water,air,air,stone at index $c1
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; water,air,air,sand at index $c2
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; water,air,air,water at index $c3
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; water,air,stone,air at index $c4
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; water,air,stone,stone at index $c5
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; water,air,stone,sand at index $c6
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; water,air,stone,water at index $c7
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; water,air,sand,air at index $c8
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; water,air,sand,stone at index $c9
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; water,air,sand,sand at index $ca
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; water,air,sand,water at index $cb
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; water,air,water,air at index $cc
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; water,air,water,stone at index $cd
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; water,air,water,sand at index $ce
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; water,air,water,water at index $cf
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; water,stone,air,air at index $d0
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; water,stone,air,stone at index $d1
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; water,stone,air,sand at index $d2
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; water,stone,air,water at index $d3
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; water,stone,stone,air at index $d4
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; water,stone,stone,stone at index $d5
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; High bit plane

	; water,stone,stone,sand at index $d6
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; water,stone,stone,water at index $d7
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; High bit plane

	; water,stone,sand,air at index $d8
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; water,stone,sand,stone at index $d9
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; water,stone,sand,sand at index $da
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; water,stone,sand,water at index $db
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; water,stone,water,air at index $dc
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; water,stone,water,stone at index $dd
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; High bit plane

	; water,stone,water,sand at index $de
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; water,stone,water,water at index $df
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; High bit plane

	; water,sand,air,air at index $e0
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; water,sand,air,stone at index $e1
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; water,sand,air,sand at index $e2
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; water,sand,air,water at index $e3
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; water,sand,stone,air at index $e4
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; water,sand,stone,stone at index $e5
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; water,sand,stone,sand at index $e6
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; water,sand,stone,water at index $e7
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; water,sand,sand,air at index $e8
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; water,sand,sand,stone at index $e9
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; water,sand,sand,sand at index $ea
	.byte $f0,$f0,$f0,$f0,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; water,sand,sand,water at index $eb
	.byte $f0,$f0,$f0,$f0,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; water,sand,water,air at index $ec
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; water,sand,water,stone at index $ed
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; water,sand,water,sand at index $ee
	.byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; water,sand,water,water at index $ef
	.byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; water,water,air,air at index $f0
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; water,water,air,stone at index $f1
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; water,water,air,sand at index $f2
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; water,water,air,water at index $f3
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; water,water,stone,air at index $f4
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; water,water,stone,stone at index $f5
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; High bit plane

	; water,water,stone,sand at index $f6
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; water,water,stone,water at index $f7
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; High bit plane

	; water,water,sand,air at index $f8
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; water,water,sand,stone at index $f9
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; water,water,sand,sand at index $fa
	.byte $ff,$ff,$ff,$ff,$0,$0,$0,$0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; water,water,sand,water at index $fb
	.byte $ff,$ff,$ff,$ff,$f,$f,$f,$f ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; water,water,water,air at index $fc
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; water,water,water,stone at index $fd
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; High bit plane

	; water,water,water,sand at index $fe
	.byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane

	; water,water,water,water at index $ff
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Low bit plane
	.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; High bit plane
