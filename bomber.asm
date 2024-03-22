	processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	include "vcs.h"
	include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare varibales
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	seg.u Variables
	org $80

JetXPos		 byte		; player0 x-position
JetYPos 	 byte 		; player0 y-position
BomberXPos       byte		; player1 x-position
BomberYPos	 byte		; player1 y-position
MissileXPos  byte 
MissileYPos  byte 
Score         byte      
Timer          byte
Temp            byte    ; auxiliary Variable to store temp score values
OnesDigitOffset word   
TensDigitOffset word 
JetSpritePtr	 word		; pointer to player0 sprite lookup table
JetColorPtr	 word 		; pointer to player0 color lookup table
BomberSpritePtr	 word 		; pointer to palyer1 sprite lookup table
BomberColorPtr	 word		; pointer to player1 color lookup table
JetAnimOffset	 byte 		; player0 sprite frame offset for animation effects
Random           byte       ; random number generated to set enemy position
ScoreSprite      byte 
TimerSprite      byte 
TerrainColor     byte 
RiverColor       byte 

JET_HEIGHT = 9			; define constants 			
BOMBER_HEIGHT = 9
DIGITS_HEIGHT = 5

	seg Code
	org $F000

Reset:
	CLEAN_START

	lda #68
	sta JetXPos
    lda #10
	sta JetYPos
	lda #83
	sta BomberYPos
	lda #62
	sta BomberXPos
    lda #%11010100
    sta Random          ; Random = $D4
    lda #0
    sta Score
    sta Timer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare a MACRO to check if we should display the missile0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        lda #%00000000
        cpx MissileYPos
        bne .SkipMissibleDraw
.DrawMissile:
        lda #%00000010
        inc MissileYPos
.SkipMissibleDraw:
        sta ENAM0
    ENDM

	lda #<JetSprite
	sta JetSpritePtr	; lo-byte pointer for jet sprite lookup table
	lda #>JetSprite
	sta JetSpritePtr+1	; hi-byte table for jet sprite lookup table

	lda #<JetColor
	sta JetColorPtr		; lo-byte pointer for jet color lookup table
	lda #>JetColor
	sta JetColorPtr+1	; hi-byte table for jet color lookup table

	lda #<BomberSprite
	sta BomberSpritePtr	; lo-byte pointer for bomber sprite lookup table
	lda #>BomberSprite
	sta BomberSpritePtr+1	; hi-byte table for bomber sprite lookup table

	lda #<BomberColor
	sta BomberColorPtr		; lo-byte pointer for bomber color lookup table
	lda #>BomberColor
	sta BomberColorPtr+1	; hi-byte table for bomber color lookup table

StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #2
	sta VBLANK
	sta VSYNC
	REPEAT 3
		sta WSYNC
	REPEND
	lda #0
	sta VSYNC

	REPEAT 33
		sta WSYNC
	REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda JetXPos
	ldy #0
	jsr SetObjectXPos	; set player 0 horizontal position

	lda BomberXPos
	ldy #1
	jsr SetObjectXPos	; set player 1 horizontal position

    lda MissileXPos
    ldy #2
    jsr SetObjectXPos

    jsr CalculateDigitOffset ; calculate the scoreboard digits lookup table offset

    jsr GenerateJetSound

	sta WSYNC
	sta HMOVE		; apply the horizontal offsets previously set

    lda #0
	sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0
    sta COLUBK
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF

    lda #$1c
    sta COLUPF

    ldx #DIGITS_HEIGHT

.ScoreDigitLoop:
    ldy TensDigitOffset
    lda Digits,Y 
    and #$F0
    sta ScoreSprite
    ldy OnesDigitOffset
    lda Digits,Y 
    and #$0F
    ora ScoreSprite     ; merge it with the saved tens digit graphics
    sta ScoreSprite     ; and save it
    sta WSYNC
    sta PF1

    ldy TensDigitOffset+1
    lda Digits,Y 
    and #$F0
    sta TimerSprite
    ldy OnesDigitOffset+1
    lda Digits,Y 
    lda Digits,Y 
    and #$0F 
    ora TimerSprite
    sta TimerSprite

    jsr Sleep12Cycles     
    sta PF1  

    ldy ScoreSprite
    sta WSYNC

    sty PF1
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1

    jsr Sleep12Cycles

    dex 
    bne .ScoreDigitLoop
    sta WSYNC

    lda #0
    sta PF0 
    sta PF1 
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC

GameVisibleLine:
    lda TerrainColor
    sta COLUPF               
    lda RiverColor
    sta COLUBK               
    lda #%00000001
    sta CTRLPF               ; enable playfield reflection
    lda #$F0
    sta PF0                  ; setting PF0 bit pattern
    lda #$FC
    sta PF1                  ; setting PF1 bit pattern
    lda #0
    sta PF2                  ; setting PF2 bit pattern

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display visible scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldx #85                  ; X counts the number of remaining scanlines (2-line kernal)
.GameLineLoop:
    DRAW_MISSILE             ; macro to check if we should draw the missle.
.AreWeInsideJetSprite:       ; check if should render sprite player0
    txa                      ; transfer X to A
    sec                      ; make sure carry flag is set
    sbc JetYPos              ; subtract sprite Y coordinate
    cmp #JET_HEIGHT           ; are we inside the sprite height bounds?
    bcc .DrawSpriteP0        ; if result < SpriteHeight, call subroutine
    lda #0                   ; else, set lookup index to 0
.DrawSpriteP0:
    clc
    adc JetAnimOffset	     ; jump to the correct sprite frame in memorry
    tay                      ; load Y so we can work with pointer
    lda (JetSpritePtr),Y     ; load player bitmap slice of data
    sta WSYNC                ; wait for next scanline
    sta GRP0                 ; set graphics for player 0
    lda (JetColorPtr),Y      ; load player color from lookup table
    sta COLUP0               ; set color for player 0 slice

.AreWeInsideBomberSprite:    ; check if should render sprite player1
    txa                      ; transfer X to A
    sec                      ; make sure carry flag is set
    sbc BomberYPos           ; subtract sprite Y coordinate
    cmp #BOMBER_HEIGHT        ; are we inside the sprite height bounds?
    bcc .DrawSpriteP1        ; if result < SpriteHeight, call subroutine
    lda #0                   ; else, set index to 0
.DrawSpriteP1:
    tay
    lda #%0000101
    sta NUSIZ1               ; stretch player1 sprite
    lda (BomberSpritePtr),Y  ; load player bitmap slice of data
    sta WSYNC                ; wait for next scanline
    sta GRP1                 ; set graphics for player 0
    lda (BomberColorPtr),Y   ; load player color from lookup table
    sta COLUP1               ; set color for player 0 slice

    dex                      ; X--
    bne .GameLineLoop        ; repeat next main game scanline while X != 0

    lda #0
    sta JetAnimOffset        ; reset jet animation frame to zero each frame

    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #2
	sta VBLANK
	REPEAT 30
		sta WSYNC
	REPEND

	lda #0
	sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000           ; player0 joystick up
    bit SWCHA
    bne CheckP0Down          ; if bit pattern doesnt match, bypass Up block
    lda JetYPos
    cmp #70
    bpl CheckP0Down
.P0UpPressed:
    inc JetYPos
    lda #0
    sta JetAnimOffset        ; reset sprite animation to first frame

CheckP0Down:
    lda #%00100000           ; player0 joystick down
    bit SWCHA
    bne CheckP0Left          ; if bit pattern doesnt match, bypass Down block
    lda JetYPos
    cmp #5
    bmi CheckP0Left
.P0DownPressed:
    dec JetYPos
    lda #0
    sta JetAnimOffset        ; reset sprite animation to first frame

CheckP0Left:
    lda #%01000000           ; player0 joystick left
    bit SWCHA
    bne CheckP0Right         ; if bit pattern doesnt match, bypass Left block
    lda JetXPos
    cmp #35
    bmi CheckP0Right
.P0LeftPressed:
    dec JetXPos
    lda #JET_HEIGHT           ; 9
    sta JetAnimOffset        ; set animation offset to the second frame

CheckP0Right:
    lda #%10000000           ; player0 joystick right
    bit SWCHA
    bne CheckButtonPressed        ; if bit pattern doesnt match, bypass Right block
    lda JetXPos
    cmp #100
    bpl CheckButtonPressed
.P0RightPressed:
    inc JetXPos
    lda #JET_HEIGHT           ; 9
    sta JetAnimOffset        ; set animation offset to the second frame

CheckButtonPressed:
    lda #%10000000
    bit INPT4 
    bne EndInputCheck
.ButtonPressed:
    lda JetXPos
    clc 
    adc #5
    sta MissileXPos
    lda JetYPos
    clc 
    adc #8
    sta MissileYPos

    jsr GenerateMissileSound

EndInputCheck:               ; fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
	lda BomberYPos
	clc
	cmp #0
	bmi .ResetBomberPosition
	dec BomberYPos
	jmp EndPositionUpdate
.ResetBomberPosition
    jsr GetRandomBomberPosition ; call subroutine for random position
.SetScoreValues:
    sed         ; set BCD mode for score and timer values 
    lda Timer
    clc
    adc #1
    sta Timer
    cld         ; disable decimal mode 

EndPositionUpdate:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collission checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    lda #%10000000   ; CXPPMM bit 7 detects P0 and P1 Collission
    bit CXPPMM
    bne .CollisionP0P1
    jsr SeterrainRiverColor
    jmp CheckCollisionM0P1
.CollisionP0P1:
    jsr GameOver

.CollisionP0PF:
    jsr GameOver

CheckCollisionM0P1:
    lda #%10000000
    bit CXM0P
    bne .M0P1Collided
    jmp EndCollisionCheck
.M0P1Collided:
    sed 
    lda Score
    clc 
    adc #1
    sta Score
    cld 
    lda #0
    sta MissileYPos
     
EndCollisionCheck:
    sta CXCLR   ; clear all the collision flags 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate audio for the jet engine sound based on the jet y-position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GenerateJetSound subroutine
    lda #1
    sta AUDV0

    lda JetYPos
    lsr 
    lsr 
    lsr 
    sta Temp
    lda #31
    sec 
    sbc Temp
    sta AUDF0

    lda #8
    sta AUDC0
    rts 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate audio for the Missible engine sound when shooting missiles 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GenerateMissileSound subroutine
    lda #3
    sta AUDV0

    lda #30
    sta AUDF0

    lda #10
    sta AUDC0
    rts 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to set the color for the terrain and river to green and blue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SeterrainRiverColor subroutine
    lda #$C2
    sta TerrainColor
    lda #$84
    sta RiverColor
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle onject horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of of our object
;; Y is the object type(0:player0, 1:player1, 2:missle0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
	sta WSYNC
	sec
.Div15Loop
	sbc #15
	bcs .Div15Loop
	eor #7
	asl
	asl
	asl
	asl
	sta HMP0,Y
	sta RESP0,Y
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gameover Subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    lda #$30
    sta TerrainColor
    sta RiverColor
    lda #0
    sta Score
    rts 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate a Linear-Feedback Shift Register random number
;; Generate a LFSR random number
;; Divide the random value by 4 to limit the size of the result to match river
;; Add 30 to compensate for the left playfield
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPosition subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl 
    rol Random      ; performs a series of shifts and bit operation

    lsr 
    lsr             ; divide the value by 4 with 2 right shifts
    sta BomberXPos

    lda #30
    adc BomberXPos   ; adds 30 to compensate for left PF
    sta BomberXPos

	lda #96         
	sta BomberYPos   ; set the y-position to the top of the screen

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    ldx #1      ; loop counter
.PrepareScoreLoop ; will loop twice
    lda Score,X     ; load A with Timer(X=1) or Score (X=0)
    and #$0F        ; remove the tens digit by masking 4 bits 00001111
    sta Temp        ; save the value of A into Temp
    asl
    asl 
    adc Temp
    sta OnesDigitOffset,X   

    lda Score,X    
    and #$F0    ; remove the ones digit by masking 4 bits 11110000
    lsr 
    lsr 
    sta Temp
    lsr 
    lsr 
    adc Temp
    sta TensDigitOffset,X

    dex           ; X--
    bpl .PrepareScoreLoop

    rts 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waster 12 clock cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

JetSprite:
    .byte #%00000000         ;
    .byte #%00010100         ;   # #
    .byte #%01111111         ; #######
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

JetSpriteTurn:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

BomberSprite:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00101010         ;  # # #
    .byte #%00111110         ;  #####
    .byte #%01111111         ; #######
    .byte #%00101010         ;  # # #
    .byte #%00001000         ;    #
    .byte #%00011100         ;   ###

JetColor:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColor:
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                ; move to position $FFFC
    word Reset               ; write 2 bytes with the program reset address
    word Reset               ; write 2 bytes with the interruption vector
