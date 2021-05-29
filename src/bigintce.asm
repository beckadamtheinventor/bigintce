
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
	export u64_cmp
	export u64_copy
	export u64_tohex
	export u64_powmod

;------------------------------------------------
; wip functions, tba
	; export vint_alloc
	; export vint_zero
	; export vint_add
	; export vint_addi
	; export vint_sub
	; export vint_subi
	; export vint_shl
	; export vint_shr
	; export vint_itov
	; export vint_copy
	; export vint_tohex
	; export vint_mul
	; export vint_div
	; export vint_powmod

include 'u64.asm'
;include 'vint.asm'

;------------------------------------------------
;helper functions
_helper_jphl:
	jp (hl)
