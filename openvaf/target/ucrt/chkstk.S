.global __chkstk
__chkstk:
	pushq	%rax
	pushq	%rcx
	cmpq	$0x1000, %rax
	leaq	24(%rsp), %rcx
	jb	.Lchkstk_ms_end
.Lchkstk_ms_loop:
	subq	$0x1000, %rcx
	subq	$0x1000, %rax
	orq	$0x0, (%rcx)
	cmpq	$0x1000, %rax
	ja	.Lchkstk_ms_loop
.Lchkstk_ms_end:
	subq	%rax, %rcx
	orq	$0x0, (%rcx)
	popq	%rcx
	popq	%rax
	ret
