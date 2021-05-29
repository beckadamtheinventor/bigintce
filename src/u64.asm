;------------------------------------------------
;ce_uint64_t *u64_alloc(void* (*_alloc)(size_t))
u64_alloc:
	pop bc,hl
	push hl,bc
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
;ce_uint64_t *u64_addi(ce_uint64_t *A, unsigned int B);
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
	ld hl,-20
	call ti._frameset
	push iy
	lea hl,ix-17
	ld b,16
	xor a,a
.zeroloop:
	ld (hl),a
	inc hl
	djnz .zeroloop
	ld hl,(ix+6)
	push hl
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
	pop de
	lea hl,ix-17
	ld bc,8
	push de
	ldir
	pop hl
	pop iy
	ld sp,ix
	pop ix
	ret


;-----------------------------------
; Division algorithm used:
;  if B == 0:
;    return -1, R = 0
;  if B == 1:
;    return A, R = 0
;  if A == 0:
;    return A = 0, R = 0
;  if A == B:
;    return A = 1, R = 0
;  if B > A:
;    return A = 0, R = A    ; remainder is set first
;  if A&1 == 0 and B&1 == 0:
;    C = 1
;  while A&1 == 0 and B&1 == 0:
;    A /= 2; B /= 2; C *= 2
;  if B:
;    while A >= B:
;      A -= B
;  Example:
;    11 / 9
;    A = 11, B = 9
;    A = 2, B = 8
;-----------------------------------
;ce_uint64_t *u64_div(ce_uint64_t *A, ce_uint64_t *B, ce_uint64_t *R);
;returns A = A / B, *R = A % B
u64_div:
	ld hl,-8
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
	or a,a
	jq z,.return_neg_1
.not_zero:
	dec a
	jq nz,.not_one
	ld b,7
.zerocheckloop2:
	inc hl
	or a,(hl)
	djnz .zerocheckloop2
	or a,a
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
	ld b,8
	or a,a
	jq z,.zero_arg0_loop ;return A = 0, R = 0 if A = 0

	ld de,(ix+6)
	ld hl,(ix+9)
	call u64_cmp.entry
	jq z,.return_1 ;return 1 if A == B
	inc a
	jq z,.return_B_gt_A

	lea hl,ix-8 ;set result counter to 1
	ld (hl),1
	xor a,a
	ld b,7
.set_c_loop:
	inc hl
	ld (hl),a
	djnz .set_c_loop

.shift_loop: ;shift divisor, dividend down and shift result counter up while divisor and dividend are even numbers
	ld hl,(ix+6)
	ld de,(ix+9)
	bit 0,(hl)
	ex hl,de
	jq nz,.done_shifting
	bit 0,(hl)
	jq nz,.done_shifing
	ld a,(hl)
	ld b,7
.check_zero_loop:
	inc hl
	or a,(hl)
	djnz .check_zero_loop
	jq z,.return_arg0
	push de
	ld c,1
	call u64_shr.entry_c ;divisor /= 2
	pop hl
	inc c ;u64_shr always returns c = 0
	call u64_shr.entry_c ;dividend /= 2
	lea hl,ix-8
	inc c
	call u64_shl.entry_c ;result *= 2
	jq .shift_loop
.done_shifting:
	ld de,(ix+6)
	ld hl,(ix+9)
	call u64_cmp.entry
	jq z,.return_arg0
	inc a
	jq z,.return_B_gt_A
	
	
.return_B_gt_A: ;if B > A
	ld hl,(ix+6)
	ld de,(ix+12)
	ld bc,8
	ldir
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
	ld b,8
.loop:
	ld a,(de)
	cp a,(hl)
	inc hl
	inc de
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
;!!!NOT YET IMPLEMENTED!!!
; ce_uint64_t *u64_powmod(ce_uint64_t *C, ce_uint64_t *E, ce_uint64_t *M);
; return C = (C pow E) % M
u64_powmod:
	ret

