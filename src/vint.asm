;------------------------------------------------
;vint_t *vint_alloc(uint8_t bytes, void*(void(*_alloc)(size_t)));
vint_alloc:
	pop hl,bc,de
	push de,bc,hl
	or a,a
	sbc hl,hl
	ld a,c
	cp a,4 ;check if below minimum size
	ret c
	inc a
	ld l,a
	push hl
	ex hl,de
	call _helper_jphl
	pop de
	add hl,bc
	or a,a
	sbc hl,bc
	ret z
	dec e
	ld (hl),e
	ret

;------------------------------------------------
;vint_t *vint_zero(vint_t *A);
vint_zero:
	pop bc,hl
	push hl,bc
.entry0:
	xor a,a
.entrya:
	push hl
	ld b,(hl)
	inc hl
.zeroloop:
	ld (hl),a
	inc hl
	djnz .zeroloop
	pop hl
	ret

;------------------------------------------------
;vint_t *vint_addi(vint_t *A, int B);
vint_addi:
	pop bc,hl,de
	push de,hl,bc
.entry:
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
; vint_t *vint_add(vint_t *A, vint_t *B);
; returns pointer to A = A + B
vint_add:
	pop bc,hl,de
	push de,hl,bc
.entry:
	push hl
	ld a,(de)
	ld b,(hl)
	ld c,a
	sub a,b
	jq c,.a_gteq_b ;if len(A) >= len(B)
	xor a,a
.a_gteq_b:
	ld c,a
	ld a,(de)
	cp a,b
	jq nc,.b_gt_a ;check if len(B) >= len(A)
	ld b,a
.b_gt_a:
	push bc ;save c = abs(len(B)-len(A))
	inc hl
	inc de
	or a,a
.add_loop:
	ld a,(de)
	adc a,(hl)
	ld (hl),a
	inc hl
	inc de
	djnz .add_loop
	pop bc
	push af
	ld a,c
	or a,a
	jq z,.donepop2 ;if len(B) == len(A)
	pop af
	ld b,c
	ld c,0
.set_loop:
	ld a,(hl)
	adc a,c
	ld (hl),a
	inc hl
	djnz .set_loop
	db $3E ;ld a,...
.donepop2:
	pop bc
.done:
	pop hl
	ret

;------------------------------------------------
;vint_t *vint_subi(vint_t *A, int B);
vint_subi:
	pop bc,hl,de
	push de,hl,bc
.entry:
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
; vint_t *vint_sub(vint_t *A, vint_t *B);
; returns pointer to A = A - B
vint_sub:
	pop bc,hl,de
	push de,hl,bc
.entry:
	push hl
	ld a,(de)
	ld b,(hl)
	ld c,a
	sub a,b
	jq c,.a_gteq_b ;if len(A) >= len(B)
	xor a,a
.a_gteq_b:
	ld c,a
	ld a,(de)
	cp a,b
	jq nc,.b_gt_a ;check if len(B) >= len(A)
	ld b,a
.b_gt_a:
	push bc ;save c = abs(len(B)-len(A))
	inc hl
	inc de
	or a,a
.sub_loop:
	ld a,(de)
	sbc a,(hl)
	ld (hl),a
	inc hl
	inc de
	djnz .sub_loop
	pop bc
	push af
	ld a,c
	or a,a
	jq z,.donepop2 ;if len(B) == len(A)
	pop af
	ld b,c
	ld c,0
.set_loop:
	ld a,(hl)
	sbc a,c
	ld (hl),a
	inc hl
	djnz .set_loop
	db $3E ;ld a,...
.donepop2:
	pop bc
.done:
	pop hl
	ret

;------------------------------------------------
; vint_t *vint_shl(vint_t *A, uint8_t B);
; output A = A << B
vint_shl:
	pop bc,hl,de
	push de,hl,bc
.entry_e:
	ld c,e
.entry_c:
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
.entry_e:
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
; vint_t *vint_itov(vint_t *A, unsigned int B);
; returns pointer to A
vint_itov:
	pop bc,hl,de
	push de,hl,bc
	ld b,(hl)
	push hl
	inc hl
	ld (hl),de
	inc hl
	inc hl
	dec b
	dec b
	dec b
	xor a,a
.zeroloop:
	inc hl
	ld (hl),a
	djnz .zeroloop
	pop hl
	ret

;------------------------------------------------
; vint_t *vint_copy(vint_t *dest, vint_t *src);
; returns pointer to dest
vint_copy:
	pop bc,de
	ex (sp),hl
	push de,bc
;copy len(dest) bytes from src
	ld a,(de)
	push hl ;save src
	inc hl
	push de ;save dest
	inc de
.copyloop:
	ldi
	dec a
	jq nz,.copyloop
	pop hl ;restore dest
	ld c,a ;zero bc by setting c to 0 and bc to b*c, saving a byte
	mlt bc
	ld c,(hl)
	pop hl ;restore src
	ld b,(hl)
	scf
	sbc a,b ;len(dest) - len(src)
	ret c ;return if len(dest) <= len(src)
	inc a
	push hl
	add hl,bc
	ld b,a
	xor a,a
.zeroloop:
	inc hl
	ld (hl),a
	djnz .zeroloop
	pop hl
	ret

;------------------------------------------------
; char *vint_tohex(char *buf, vint_t *A);
; returns pointer to buf
vint_tohex:
	pop bc,de
	ex (sp),hl
	push de,bc
	ld bc,0
	ld c,(hl)
	inc hl
	ld a,c
	or a,a
	jq nz,u64_tohex.entry
	ld b,1 ;c is zero, setting b to 1 means bc = 256
	jq u64_tohex.entry

;------------------------------------------------
;!!!NOT YET IMPLEMENTED!!!
; vint_t *vint_powmod(vint_t *C, vint_t *E, vint_t *M);
; return C = (C pow E) % M
vint_powmod:
	ret

;------------------------------------------------
;!!!NOT YET IMPLEMENTED!!!
; vint_t *vint_div(vint_t *A, vint_t *B, vint_t *R);
; return A = A / B, R = A % B
vint_div:
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
;vint_t *vint_mul(vint_t *A, vint_t *B, vint_t *C);
;output C = A * B
vint_mul:
	ld hl,-14
	call ti._frameset
	ld (ix-9),iy
	ld hl,(ix+12)
	ld b,(hl)
	ld (ix-11),b
	xor a,a
.zeroloop:
	inc hl
	ld (hl),a
	djnz .zeroloop
	ld hl,(ix+9)
	ld a,(hl)
	ld (ix-1),a
	ld hl,(ix+6)
	ld (ix-6),hl
	ld a,(hl)
	ld (ix-10),a
	ld iy,(ix+12)
	lea hl,iy+1
	ld bc,0
	ld c,a
	or a,a
	jq nz,.outputunder256b
	inc b
.outputunder256b:
	add hl,bc
	ld (ix-14),hl
	ld iy,(ix+12)
	inc iy
.outer_loop:
	ld a,(ix-10)
	ld (ix-2),a
	or a,a
	sbc hl,hl
	push hl
	pop de
	ld bc,(ix+6)
	ld a,(bc)
	inc bc
	ld (ix+6),bc
	ld (ix-3),a
.inner_loop:
	ld h,0
	ld l,(iy)
	inc iy
	add hl,de
	ld bc,(ix+9)
	ld a,(bc)
	inc bc
	ld (ix+9),bc
	ld c,a
	ld b,(ix-3)
	mlt bc
	add hl,bc
	ld e,h
	call .checkbounds
	jq z,.dontload
	ld (iy),l
.dontload:
	inc iy
	dec (ix-2)
	jq nz,.inner_loop
	call .checkbounds
	jq z,.dontloadcarry
	ld (iy),e
.dontloadcarry:
	ld hl,(ix+12)
	ld bc,0
	ld c,(hl)
	dec c
	lea hl,iy
	or a,a
	sbc hl,bc
	push hl
	pop iy
	ld hl,(ix+9)
	ld bc,-8
	add hl,bc
	ld (ix+9),hl
	dec (ix-1)
	jq nz,.outer_loop
	ld hl,(ix+12)
	ld iy,(ix-9)
	ld sp,ix
	pop ix
	ret

.checkbounds:
	push hl
	lea hl,iy
	ld bc,(ix-14)
	or a,a
	sbc hl,bc
	pop hl
	ret
