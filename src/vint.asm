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
; vint_t *vint_add(vint_t *A, vint_t *B);
; returns pointer to A = A + B
vint_add:
	pop bc,hl,de
	push de,hl,bc
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
; vint_t *vint_mul(vint_t *A, vint_t *B, vint_t *R);
; return R = A * B
vint_mul:
	ld hl,-3
	call ti._frameset
	push iy
	ld hl,(ix+12) ;zero argument R
	ld b,(hl)
	xor a,a
.zeroloop:
	inc hl
	ld (hl),a
	djnz .zeroloop
	ld iy,(ix+12)
	inc iy
	ld (ix+12),iy
	ld hl,(ix+9) ;setup inner loop counter
	ld a,(hl)
	ld (.smc_counter),a
	ld c,a
	ld hl,(ix+6) ;setup outer loop counter
	ld a,(hl)
	ld (ix-1),a

;calculate location of last byte of R
	or a,a
	ld b,a
	jq z,.mul_256_lenA
	ld c,a
	or a,a
	jq z,.mul_256_lenB
;multiply len(A)*len(B)
	mlt bc
	jq .set_max_r
.mul_256_lenB:
;multiply len(B)*256
	ld a,c
	jq .mul_a_256
.mul_256_lenA:
	ld c,a
	or a,a
	jq z,.mul_256_256
;multiply len(A)*256
	ld a,b
.mul_a_256:
	ld bc,0
	ld b,a
	jq .set_max_r
.mul_256_256:
	ld bc,$010000
.set_max_r:
	ld hl,(ix+12)
	add hl,bc
	ld (.smc_max_r),hl
.outer_loop:
	ld a,0
.smc_counter:=$-1
	ld (ix-2),a
	or a,a
	sbc hl,hl
	push hl
	pop de
	ld bc,(ix+6)
	inc bc
	ld a,(bc)
	ld (ix+6),bc
	ld (ix-3),a
	ld hl,(ix+12)
	push hl ;load iy with outer loop output pointer
	pop iy
	inc hl
	ld (ix+12),hl ;advance outer loop output pointer by a byte
	ld hl,(ix+9)
	push hl ;save B so we can iterate over it in the inner loop and reset it after
.inner_loop:
	ld h,0
	call .ld_l_ind_iy
	add hl,de
	ld bc,(ix+9)
	inc bc
	ld a,(bc)
	ld (ix+9),bc
	ld c,a
	ld b,(ix-3)
	mlt bc
	add hl,bc
	ld e,h
	call .ld_ind_iy_l
	inc iy
	dec (ix-2)
	jq nz,.inner_loop
	ld l,e
	call .ld_ind_iy_l
	pop hl
	ld (ix+9),hl
	dec (ix-1)
	jq nz,.outer_loop
	ld hl,(ix+12)
	pop iy
	ld sp,ix
	pop ix
	ret
.ld_l_ind_iy:
	push hl
	lea hl,iy
	ld bc,0
.smc_max_r:=$-3
	or a,a
	sbc hl,bc
	pop hl
	ccf
	ret c
	ld l,(iy)
	ret
.ld_ind_iy_l:
	push hl
	lea hl,iy
	ld bc,(.smc_max_r)
	or a,a
	sbc hl,bc
	pop hl
	ret nc
	ld (iy),l
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
	jq nz,.entry
	ld b,1 ;c is zero, setting b to 1 means bc = 256
	jq u64_tohex.entry:

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
