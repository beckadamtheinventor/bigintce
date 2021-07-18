
;------------------------------------------------
;ce_uint64_t *u64_alloc(void* (*_alloc)(size_t));
u64_alloc:
	pop bc
	ex (sp),hl
	push bc
	ld bc,8
	push bc
	call _helper_jphl
	pop bc
	ret

;------------------------------------------------
;ce_uint64_t *u64_zero(ce_uint64_t *A);
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
; ce_uint64_t *u64_shl(ce_uint64_t *A, uint8_t B);
; output A = A << B
u64_shl:
	pop bc,hl,de
	push de,hl,bc
.entry_e:
	ld c,e
.entry_c:
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
; ce_uint64_t *u64_shr(ce_uint64_t *A, uint8_t B);
; output A = A >> B
u64_shr:
	pop bc,hl,de
	push de,hl,bc
.entry_e:
	ld c,e
.entry_c:
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
;ce_uint64_t *u64_add(ce_uint64_t *A, ce_uint64_t *B);
;output pointer to A = A + B
u64_add:
	pop bc,de
	ex (sp),hl
	push de,bc
.entry:
	push de
	or a,a
	ld b,8
.add_loop:
	ld a,(de)
	adc a,(hl)
	ld (de),a
	inc hl
	inc de
	djnz .add_loop
	pop hl
	ret

;------------------------------------------------
;ce_uint64_t *u64_addi(ce_uint64_t *A, unsigned int B);
;output pointer to A = A + B
u64_addi:
	pop bc,hl,de
	push de,hl,bc
.entry:
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
;ce_uint64_t *u64_ito64(ce_uint64_t *A, unsigned int B);
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
;ce_uint64_t *u64_mul(ce_uint64_t *A, ce_uint64_t *B);
;output A = A * B
u64_mul:
	ld hl,-26
	call ti._frameset
	ld (ix-26),iy
	lea hl,ix-17
	ld b,17
	xor a,a
.zeroloop:
	ld (hl),a
	inc hl
	djnz .zeroloop
	ld hl,(ix+6)
	ld (ix-23),hl
	lea iy,ix-17
	ld a,8
	ld (ix-18),a
.outer_loop:
	ld a,8
	ld (ix-19),a
	or a,a
	sbc hl,hl
	push hl
	pop de
	ld bc,(ix+6)
	ld a,(bc)
	inc bc
	ld (ix+6),bc
	ld (ix-20),a
.inner_loop:
	ld h,0
	ld l,(iy)
	add hl,de
	ld bc,(ix+9)
	ld a,(bc)
	inc bc
	ld (ix+9),bc
	ld c,a
	ld b,(ix-20)
	mlt bc
	add hl,bc
	ld e,h
	ld (iy),l
	inc iy
	dec (ix-19)
	jq nz,.inner_loop
	ld (iy),e
	lea iy,iy-7
	ld hl,(ix+9)
	ld bc,-8
	add hl,bc
	ld (ix+9),hl
	dec (ix-18)
	jq nz,.outer_loop
	ld de,(ix-23)
	lea hl,ix-17
	ld bc,8
	push de
	ldir
	pop hl
	ld iy,(ix-26)
	ld sp,ix
	pop ix
	ret


;-----------------------------------
; Division algorithm is bytewise long division
;-----------------------------------
;ce_uint64_t *u64_div(ce_uint64_t *A, ce_uint64_t *B, ce_uint64_t *R);
;returns A = A / B, *R = A % B. Returns -1 if divide by zero.
u64_div:
	ld hl,-19
	call ti._frameset
	ld hl,(ix+9)
	ld a,(hl)
	or a,a
	jq nz,.not_zero
	ld b,7
.zerocheckloop:
	inc hl
	or a,(hl)
	djnz .zerocheckloop
	jq z,.return_neg_1
.not_zero:
	dec a
	jq nz,.not_one
	ld b,7
.zerocheckloop2:
	inc hl
	or a,(hl)
	djnz .zerocheckloop2
	jq z,.return_arg0
.not_one:
	ld hl,(ix+12)
	xor a,a
	ld b,8
.zero_arg2_loop:
	ld (hl),a
	inc hl
	djnz .zero_arg2_loop

	ld hl,(ix+6)
	ld b,8
.zerocheckloop3:
	or a,(hl)
	inc hl
	djnz .zerocheckloop3
	ld (ix-19),hl
	ld b,8
	or a,a
	jq z,.zero_arg0_loop ;return A = 0, R = 0 if A = 0

	ld de,(ix+6)
	ld hl,(ix+9)
	call u64_cmp.entry
	jq z,.return_1 ;return A = 1 if A == B
	inc a
	jq z,.return_B_gt_A

	lea hl,ix-16 ;set result counter and increment to 0
	xor a,a
	ld b,16
.set_c_loop:
	ld (hl),a
	inc hl
	djnz .set_c_loop

	inc a
	ld (ix+7-16),a ;set increment counter high byte to 0x01

	ld b,8
.divloop:
	push bc
	ld bc,7
	ld hl,(ix+12) ;shift accumulator up (forward) a byte
	add hl,bc
	ex hl,de
	ld hl,(ix+12)
	add hl,bc
	dec hl
	lddr
	ld hl,(ix-19) ;shift in next byte of A
	dec hl
	ld a,(hl)
	ld (de),a
	ld (ix-19),hl
.subloop:
	ld de,(ix+12) ;accumulator
	ld hl,(ix+9) ;divisor
	call u64_cmp.entry ;compare accumulator and divisor
	inc a
	jq z,.divloop_next ;accumulator < divisor, shift in another byte

	lea hl,ix-16
	lea de,ix-8
	call u64_add.entry ;Add the increment to the result

	ld de,(ix+12)
	ld hl,(ix+9)
	call u64_sub.entry ;Subtract the divisor from the accumulator
	jq .subloop

.divloop_next:
	lea hl,ix-15 ;shift increment counter down (backward) a byte
	lea de,ix-16
	ld bc,7
	ldir
	xor a,a
	ld (de),a
	pop bc
	djnz .divloop

	lea hl,ix-8
	ld de,(ix+6)
	ld bc,8
	ldir
	jq .return_arg0
	

.return_B_gt_A: ;if B > A
	ld hl,(ix+6)
	ld de,(ix+12)
	ld bc,8
	ldir
	ld hl,(ix+6)
	ld b,8
	xor a,a
	jq .zero_arg0_loop
.return_1: ;if A == B, return A = 1, R = 0
	ld hl,(ix+6)
	ld (hl),1
	ld b,7
	xor a,a
.zero_arg0_loop:
	inc hl
	ld (hl),a
	djnz .zero_arg0_loop
.return_arg0:
	ld hl,(ix+6)
	db $01
.return_neg_1:
	scf
	sbc hl,hl
.return:
	ld sp,ix
	pop ix
	ret

;------------------------------------------------
; ce_uint64_t *u64_sub(ce_uint64_t *A, ce_uint64_t *B);
; output A = A - B
u64_sub:
	pop bc,de,hl
	push hl,de,bc
.entry:
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
;ce_uint64_t *u64_subi(ce_uint64_t *A, unsigned int B);
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
; uint8_t u64_cmp(ce_uint64_t *A, ce_uint64_t *B);
; compare two 64-bit unsigned integers.
; if A == B, Returns 0, Cf unset
; if A > B, Returns 1, Cf unset
; if A < B, Returns -1, Cf set
u64_cmp:
	pop bc,de
	ex (sp),hl
	push de,bc
.entry:
	ld bc,8
	add hl,bc
	ex hl,de
	add hl,bc
	ex hl,de
	ld b,c
.loop:
	dec hl
	dec de
	ld a,(de)
	cp a,(hl)
	jq nz,.compare
	djnz .loop
	xor a,a
	ret ;return Cf unset, A = 0
.compare: ;Cf is set if A > B
	sbc a,a ;(a - a) - Cf will be 0 if Cf unset, -1 if Cf set
	ret c ;return if Cf set, A = -1
	inc a ;a = 0 --> a = 1
	ret ;return Cf unset, A = 1

;------------------------------------------------
;ce_uint64_t *u64_copy(ce_uint64_t *dest, ce_uint64_t *src);
u64_copy:
	pop bc,de
	ex (sp),hl
	push de,bc,de
	call ti.Mov8b
	pop hl
	ret

;------------------------------------------------
; char *u64_tohex(char *buf, ce_uint64_t *A);
; returns pointer to buf
u64_tohex:
	pop bc,de
	ex (sp),hl
	push de,bc
.entry64:
	ld bc,8
.entry:
	add hl,bc
	ld b,c
	push de
.loop:
	dec hl
	ld a,(hl)
	rrca
	rrca
	rrca
	rrca
	call .nibble
	ld a,(hl)
	call .nibble
	djnz .loop
	xor a,a
	ld (de),a
	pop hl
	ret
.nibble:
	and a,$F
	add a,'0'
	cp a,'9'+1
	jq c,.setindde
.over9:
	add a,-1+'A'-'9'
.setindde:
	ld (de),a
	inc de
	ret

;------------------------------------------------
; ce_uint64_t *u64_powmod(ce_uint64_t *C, ce_uint64_t *E, ce_uint64_t *M);
; return C = (C pow E) % M
u64_powmod:
	ld hl,-16
	call ti._frameset
	ld hl,(ix+12)
	ld c,(hl)
	xor a,a
	ld b,8
.zero_check_loop:
	inc hl
	or a,(hl)
	djnz .zero_check_loop
	jq z,.m_is_not_zero
	dec c
	jq z,.return_A_0 ;return A = 0 if M = 1
	inc c
	jq z,.return_neg_1 ;return -1 if M = 0
.m_is_not_zero:
	lea hl,ix-8 ;set R = 1
	ld (hl),1
	ld b,7
	xor a,a
.zero_r_loop:
	inc hl
	ld (hl),a
	djnz .zero_r_loop

	ld hl,(ix+6)
	call .mod_m ;C = C % M

.outer_loop:
	ld hl,(ix+9)
	ld a,(hl)
	ld b,7
.check_E_zero_loop:
	inc hl
	or a,(hl)
	djnz .check_E_zero_loop
	jq z,.return_R
	ld hl,(ix+9)
	bit 0,(hl)
	jq z,.no_mul_r_b
	ld hl,(ix+6)
	push hl
	pea ix-8
	call u64_mul ; R = R * C
	pop bc,bc
	lea hl,ix-8
	call .mod_m ; R = R % M
.no_mul_r_b:
	ld hl,(ix+9) ;E = E >> 1
	ld c,1
	call u64_shr.entry_c
	ld hl,(ix+6)
	push hl,hl
	call u64_mul ;C = C * C
	pop bc,hl
	call .mod_m ;C = C % M
	jq .outer_loop
.return_R:
	lea hl,ix-8
	ld de,(ix+6)
	ld bc,8
	ldir
	jq .return_A
.return_A_0:
	ld hl,(ix+6)
	ld (hl),0
.set_A_zero_loop_entry:
	ld b,7
	xor a,a
.set_A_zero_loop:
	inc hl
	ld (hl),a
	djnz .set_A_zero_loop
.return_A:
	ld hl,(ix+6)
	db $01
.return_neg_1:
	scf
	sbc hl,hl
.return_hl:
	ld sp,ix
	pop ix
	ret

.mod_m:
	ld bc,(ix+12)
	push hl
	pea ix-16
	push bc,hl
	call u64_div
	pop bc,bc,hl,de
	ld bc,8
	ldir
	ret
