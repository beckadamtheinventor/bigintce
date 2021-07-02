
;------------------------------------------------
;ce_int64_t *i64_addi(ce_int64_t *A, int B);
;output pointer to A = A + B
i64_addi:
	ld hl,-8
	call ti._frameset
	call i64_ito64.main
	pea ix-8
	ld hl,(ix+6)
	push hl
	call u64_add
	pop hl,bc
	ld sp,ix
	pop ix
	ret

;------------------------------------------------
;ce_int64_t *i64_subi(ce_int64_t *A, int B);
;output pointer to A = A - B
i64_subi:
	ld hl,-8
	call ti._frameset
	call i64_ito64.main
	pea ix-8
	ld hl,(ix+6)
	push hl
	call u64_sub
	pop hl,bc
	ld sp,ix
	pop ix
	ret

;------------------------------------------------
;ce_int64_t *i64_ito64(ce_int64_t *A, int B);
;output pointer to A = B
i64_ito64:
	pop bc,hl,de
	push de,hl,bc
	push hl
	ld (hl),de ;set low 3 bytes of arg0 to arg1
	inc hl
	inc hl
	jq .entry
.main:
	ld hl,(ix+9)
	ld (ix-8),hl
	lea hl,ix-6
.entry:
	ld a,(hl)
	inc hl
	add a,a
	sbc a,a
	ld b,5
.zeroloop:
	ld (hl),a
	inc hl
	djnz .zeroloop
	ret


;------------------------------------------------
; uint8_t i64_cmp(ce_int64_t *A, ce_int64_t *B);
; compare two 64-bit unsigned integers.
; if A == B, Returns 0, Cf unset
; if A > B, Returns 1, Cf unset
; if A < B, Returns -1, Cf set
i64_cmp:=u64_cmp

;------------------------------------------------
; ce_int64_t *i64_neg(ce_int64_t *A);
; return pointer to A = ~A
i64_neg:
	pop bc
	ex (sp),hl
	push bc
.entry:
	ld b,8
	push hl
.loop:
	ld a,$FF
	xor a,(hl)
	ld (hl),a
	inc hl
	djnz .loop
	pop hl
	ret

;------------------------------------------------
; char *i64_tohex(char *buf, ce_int64_t *A);
; returns pointer to buf
i64_tohex:
	ld hl,6
	add hl,sp
	ld de,(hl)
	dec hl
	ld a,(hl)
	dec hl
	dec hl
	ld hl,(hl)
	add a,a
	jq nc,u64_tohex.entry64
	ld a,'-'
	ld (de),a
	inc de
	call i64_neg.entry ;preserves DE
	jq u64_tohex.entry64
