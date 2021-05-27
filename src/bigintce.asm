
;------------------------------------------------
include '../include/library.inc'

;------------------------------------------------
library "BIGINTCE", 0

;------------------------------------------------
; v0 functions

	export u64_alloc
	export u64_zero
	export u64_add
	export u64_addi
	export u64_ito64
	export u64_sub
	export u64_subi
	export u64_shl
	export u64_shr
	export u64_mul
	export u64_div
	export vint_alloc
	export vint_zero
	export vint_add
	export vint_addi
	export vint_sub
	export vint_subi
	export vint_shl
	export vint_shr
	; export vint_mul
	; export vint_div

;------------------------------------------------
; macros


;------------------------------------------------
;vint_t *vint_zero(vint_t *A);
vint_zero:
	pop bc,hl
	push hl,bc
	push hl
	ld b,(hl)
	inc hl
	xor a,a
.zeroloop:
	ld (hl),a
	inc hl
	djnz .zeroloop
	pop hl
	ret

;------------------------------------------------
;vint_t *vint_alloc(uint8_t bytes, void*(void(*_alloc)(size_t)));
vint_alloc:
	pop hl,bc,de
	push de,bc,hl
	or a,a
	sbc hl,hl
	ld a,c
	cp a,3 ;check if below minimum size
	ret c
	inc a
	ld l,a
	push hl
	ex hl,de
	call int64ce_jphl
	pop de
	add hl,bc
	or a,a
	sbc hl,bc
	ret z
	dec e
	ld (hl),e
	ret

;------------------------------------------------
;vint_t *vint_addi(vint_t *A, int B);
vint_addi:
	pop bc,hl,de
	push de,hl,bc
	push hl
	ld a,(hl)
	sub a,3
	inc hl
	ld bc,(hl)
	or a,a
	ex hl,de
	adc hl,bc
	ex hl,de
	ld (hl),de
	inc hl
	inc hl
	inc hl
	ld b,a
	ld c,0
.add_loop:
	ld a,(hl)
	adc a,c
	ld (hl),a
	inc hl
	djnz .add_loop
	pop hl
	ret

;------------------------------------------------
;vint_t *vint_add(vint_t *A, vint_t *B);
vint_add:
	pop bc,hl,de
	push de,hl,bc
	push hl
	ld a,(de)
	ld c,(hl)
	cp a,c
	ret nz
	ld a,c
	inc hl
	inc de
	ld b,a
	ld c,0
.add_loop:
	ld a,(de)
	adc a,(hl)
	ld (hl),a
	inc hl
	inc de
	djnz .add_loop
	pop hl
	ret

;------------------------------------------------
;vint_t *vint_subi(vint_t *A, int B);
vint_subi:
	pop bc,hl,de
	push de,hl,bc
	push hl
	ld a,(hl)
	sub a,3
	inc hl
	ld bc,(hl)
	or a,a
	ex hl,de
	adc hl,bc
	ex hl,de
	ld (hl),de
	inc hl
	inc hl
	inc hl
	ld b,a
	ld c,0
.add_loop:
	ld a,(hl)
	adc a,c
	ld (hl),a
	inc hl
	djnz .add_loop
	pop hl
	ret

;------------------------------------------------
;vint_t *vint_sub(vint_t *A, vint_t *B);
vint_sub:
	pop bc,de,hl
	push hl,de,bc
	push de
	ld a,(de)
	ld c,(hl)
	cp a,c
	ret nz
	ld a,c
	inc hl
	inc de
	ld b,a
	ld c,0
.add_loop:
	ld a,(de)
	sbc a,(hl)
	ld (de),a
	inc hl
	inc de
	djnz .add_loop
	pop hl
	ret

;------------------------------------------------
;uint64_t *u64_alloc(void*(void(*_alloc)(size_t)));
u64_alloc:
	pop bc,hl
	push hl,bc
	ld bc,8
	push bc
	call int64ce_jphl
	pop bc
	ret

;------------------------------------------------
;uint64_t *u64_zero(uint64_t *A);
;output pointer to A = 0
u64_zero:
	pop bc,hl
	push hl,bc
	push hl
	ld b,8
	xor a,a
.zeroloop:
	ld (hl),a
	inc hl
	djnz .zeroloop
	pop hl
	ret

;------------------------------------------------
; uint64_t *u64_shl(uint64_t *A, uint8_t B);
; output A = A << B
u64_shl:
	pop bc,hl,de
	push de,hl,bc
	ld c,e
	ld de,-8
.shl_loop_outer:
	ld b,8
	or a,a
.shl_loop:
	rl (hl)
	inc hl
	djnz .shl_loop
	add hl,de
	dec c
	jr nz,.shl_loop_outer
	sbc hl,de
	ret

;------------------------------------------------
; uint64_t *u64_shr(uint64_t *A, uint8_t B);
; output A = A >> B
u64_shr:
	pop bc,hl,de
	push de,hl,bc
	ld c,e
	ld de,8
.shr_loop_outer:
	ld b,e
	add hl,de
	or a,a
.shr_loop:
	dec hl
	rr (hl)
	djnz .shr_loop
	add hl,de
	dec c
	jr nz,.shr_loop_outer
	ret

;------------------------------------------------
;uint64_t *u64_add(uint64_t *A, uint64_t *B);
;output pointer to A = A + B
u64_add:
	pop bc,hl,de
	push de,hl,bc
	push hl
	or a,a
	ld b,8
.add_loop:
	ld a,(de)
	adc a,(hl)
	ld (hl),a
	inc hl
	inc de
	djnz .add_loop
	pop hl
	ret

;------------------------------------------------
;uint64_t *u64_addi(uint64_t *A, unsigned int B);
;output pointer to A = A + B
u64_addi:
	pop bc,hl,de
	push de,hl,bc
	push hl
	xor a,a
	ld bc,(hl)
	ex hl,de
	adc hl,bc
	ex hl,de
	ld (hl),de
	inc hl
	inc hl
	inc hl
	ld b,5
	ld c,a
.add_loop:
	ld a,(hl)
	adc a,c
	ld (hl),a
	inc hl
	djnz .add_loop
	pop hl
	ret

;------------------------------------------------
;uint64_t *u64_ito64(uint64_t *A, unsigned int B);
;output pointer to A = B
u64_ito64:
	pop bc,hl,de
	push de,hl,bc
	push hl
	ld (hl),de ;set low 3 bytes of arg0 to arg1
	inc hl
	inc hl
	inc hl
	xor a,a
	ld b,5 ;set high 5 bytes of arg0 to 0
.voidint_loop:
	ld (hl),a
	inc hl
	djnz .voidint_loop
	pop hl
	ret

;-----------------------------------
;  Multiplication Algorithm used:
;  R = (u128)0, I = 0
;  while I < 8:
;    J = 0, C = 0
;    while J < 8:
;      D = R[I+J] + C + A[I]*B[J]
;      C = D >> 8
;      D &= 0xFF
;      R[I+J] = D
;    R[I+8] = C
;-----------------------------------
;uint64_t *u64_mul(uint64_t *A, uint64_t *B);
;output A = A * B
u64_mul:
	ld hl,-19
	call ti._frameset
	push iy
	lea hl,ix-16
	ld b,16
	xor a,a
.zeroloop:
	ld (hl),a
	inc hl
	djnz .zeroloop
	lea iy,ix-16
	ld a,8
	ld (ix-17),a
.outer_loop:
	ld a,8
	ld (ix-18),a
	or a,a
	sbc hl,hl
	push hl
	pop de
	ld bc,(ix+6)
	ld a,(bc)
	inc bc
	ld (ix+6),bc
	ld (ix-19),a
.inner_loop:
	ld h,0
	ld l,(iy)
	add hl,de
	ld bc,(ix+9)
	ld a,(bc)
	inc bc
	ld (ix+9),bc
	ld c,a
	ld b,(ix-19)
	mlt bc
	add hl,bc
	ld e,h
	ld (iy),l
	inc iy
	dec (ix-18)
	jq nz,.inner_loop
	ld (iy),e
	lea iy,iy-7
	ld hl,(ix+9)
	ld bc,-8
	add hl,bc
	ld (ix+9),hl
	dec (ix-17)
	jq nz,.outer_loop
	ld de,(ix+6)
	lea hl,ix-16
	ld bc,16
	push de
	ldir
	pop hl
	pop iy
	ld sp,ix
	pop ix
	ret


;!!!-----WIP-----!!!
;uint64_t *u64_div(uint64_t *A, uint64_t *B, uint64_t *R);
;returns A = A / B, *R = A % B
u64_div:
	ld hl,-4
	call ti._frameset
	ld hl,(ix+12)
	push hl
	ld a,8
	ld (ix-1),a
	ld hl,(ix+9)
	ld de,(ix+6)
.loop:
	ld c,(hl)
	ld a,(de)
	ld b,-1
.subloop:
	inc b
	sub a,c
	jr nc,.subloop
	add a,c
	ex (sp),hl
	ld (hl),a
	inc hl
	ex (sp),hl
	ld a,b
	ld (de),a
	inc hl
	inc de
	dec (ix-1)
	jq nz,.loop

	ld hl,(ix+3)
	ld sp,ix
	pop ix
	ret

;------------------------------------------------
; uint64_t *u64_sub(uint64_t *A, uint64_t *B);
; output A = A - B
u64_sub:
	pop bc,de,hl
	push hl,de,bc
	push de
	or a,a
	ld b,8
.sub_loop:
	ld a,(de)
	sbc a,(hl)
	ld (de),a
	inc hl
	inc de
	djnz .sub_loop
	pop hl
	ret

;------------------------------------------------
;uint64_t *u64_subi(uint64_t *A, unsigned int B);
;output A = A + B
u64_subi:
	pop bc,hl,de
	push de,hl,bc
	push hl
	xor a,a
	ld bc,(hl)
	ex hl,de
	sbc hl,bc
	ex hl,de
	ld (hl),de
	inc hl
	inc hl
	inc hl
	ld b,5
	ld c,a
.sub_loop:
	ld a,(hl)
	sbc a,c
	ld (hl),a
	inc hl
	djnz .sub_loop
	pop hl
	ret

;------------------------------------------------
; vint_t *vint_shl(vint_t *A, uint8_t B);
; output A = A << B
vint_shl:
	pop bc,hl,de
	push de,hl,bc
	ld c,e
	ld b,(hl)
	inc hl
.shl_loop_outer:
	push bc,hl
	or a,a
.shl_loop:
	rl (hl)
	inc hl
	djnz .shl_loop
	pop hl,bc
	dec c
	jr nz,.shl_loop_outer
	ret

;------------------------------------------------
; vint_t *vint_shr(vint_t *A, uint8_t B);
; output A = A >> B
vint_shr:
	pop bc,hl,de
	push de,hl,bc
	ld a,(hl)
	ld bc,0
	ld c,a
	add hl,bc
	inc hl
	ld b,a
	ld c,e
.shr_loop_outer:
	push hl,bc
	or a,a
.shr_loop:
	dec hl
	rr (hl)
	djnz .shr_loop
	pop bc,hl
	dec c
	jr nz,.shr_loop_outer
	ret


;------------------------------------------------
;helper functions
int64ce_jphl:
	jp (hl)

