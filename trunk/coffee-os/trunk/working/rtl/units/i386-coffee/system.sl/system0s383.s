
SECTION .text
	ALIGN 16
	GLOBAL SYSTEM_UNICODETOUTF8$PCHAR$LONGWORD$PUNICODECHAR$LONGWORD$$LONGWORD
SYSTEM_UNICODETOUTF8$PCHAR$LONGWORD$PUNICODECHAR$LONGWORD$$LONGWORD:
		push	ebp
		mov	ebp,esp
		sub	esp,96
		mov	dword [ebp-96],ebx
		mov	dword [ebp-92],esi
		mov	dword [ebp-88],edi
		mov	dword [ebp-60],eax
		mov	ebx,edx
		mov	dword [ebp-68],ecx
		mov	eax,dword [ebp+8]
		mov	dword [ebp-76],eax
		mov	dword [ebp-56],0
		mov	dword [ebp-52],0
		mov	dword [ebp-48],0
		lea	ecx,[ebp-16]
		lea	edx,[ebp-40]
		mov	eax,1
		call	NEAR FPC_PUSHEXCEPTADDR
		call	NEAR FPC_SETJMP
		push	eax
		test	eax,eax
		jne	NEAR ..@j7584
		mov	dword [ebp-84],0
		mov	eax,dword [ebp-68]
		test	eax,eax
		je	NEAR ..@j7584
		mov	dword [ebp-72],0
		mov	dword [ebp-80],0
		mov	eax,dword [ebp-60]
		test	eax,eax
		je	NEAR ..@j7669
		jmp	NEAR ..@j7598
	ALIGN 4
..@j7597:
		mov	edx,dword [ebp-68]
		mov	eax,dword [ebp-72]
		mov	ax,word [edx+eax*2]
		mov	word [ebp-64],ax
		sub	ax,127
		jbe	NEAR ..@j7604
		sub	ax,1920
		jbe	NEAR ..@j7605
		sub	ax,53248
		jbe	NEAR ..@j7606
		sub	ax,1024
		jbe	NEAR ..@j7607
		sub	ax,1025
		jb	NEAR ..@j7603
		sub	ax,8191
		jbe	NEAR ..@j7606
		jmp	NEAR ..@j7603
..@j7604:
		mov	al,byte [ebp-64]
		mov	edx,dword [ebp-60]
		mov	ecx,dword [ebp-80]
		mov	byte [edx+ecx*1],al
		inc	dword [ebp-80]
		jmp	NEAR ..@j7602
..@j7605:
		mov	eax,dword [ebp-80]
		inc	eax
		cmp	eax,ebx
		jae	NEAR ..@j7599
		movzx	eax,word [ebp-64]
		shr	eax,6
		or	eax,192
		mov	edx,dword [ebp-60]
		mov	ecx,dword [ebp-80]
		mov	byte [edx+ecx*1],al
		mov	eax,dword [ebp-80]
		inc	eax
		mov	ecx,eax
		mov	ax,word [ebp-64]
		and	ax,63
		or	ax,128
		mov	edx,dword [ebp-60]
		mov	byte [edx+ecx*1],al
		add	dword [ebp-80],2
		jmp	NEAR ..@j7602
..@j7606:
		mov	eax,dword [ebp-80]
		add	eax,2
		cmp	eax,ebx
		jae	NEAR ..@j7599
		movzx	eax,word [ebp-64]
		shr	eax,12
		or	eax,224
		mov	edx,dword [ebp-60]
		mov	ecx,dword [ebp-80]
		mov	byte [edx+ecx*1],al
		mov	eax,dword [ebp-80]
		inc	eax
		mov	edx,eax
		movzx	eax,word [ebp-64]
		shr	eax,6
		and	eax,63
		or	eax,128
		mov	ecx,dword [ebp-60]
		mov	byte [ecx+edx*1],al
		mov	eax,dword [ebp-80]
		add	eax,2
		mov	ecx,eax
		mov	ax,word [ebp-64]
		and	ax,63
		or	ax,128
		mov	edx,dword [ebp-60]
		mov	byte [edx+ecx*1],al
		add	dword [ebp-80],3
		jmp	NEAR ..@j7602
..@j7607:
		mov	eax,dword [ebp-80]
		add	eax,3
		cmp	eax,ebx
		jae	NEAR ..@j7599
		mov	edi,dword [ebp-76]
		mov	eax,0
		sub	edi,1
		sbb	eax,0
		mov	ecx,dword [ebp-72]
		mov	edx,0
		cmp	eax,edx
		jg	NEAR ..@j7629
		jl	NEAR ..@j7602
		cmp	edi,ecx
		jna	NEAR ..@j7602
..@j7629:
		mov	eax,dword [ebp-72]
		inc	eax
		mov	edx,dword [ebp-68]
		mov	ax,word [edx+eax*2]
		cmp	ax,56320
		jnae	NEAR ..@j7602
		mov	eax,dword [ebp-72]
		inc	eax
		mov	edx,dword [ebp-68]
		mov	ax,word [edx+eax*2]
		cmp	ax,57343
		jnbe	NEAR ..@j7602
		lea	eax,[ebp-48]
		call	NEAR FPC_UNICODESTR_DECR_REF
		mov	dword [ebp-48],0
		lea	eax,[ebp-52]
		call	NEAR FPC_UNICODESTR_DECR_REF
		mov	dword [ebp-52],0
		mov	eax,dword [ebp-72]
		inc	eax
		mov	ecx,dword [ebp-68]
		mov	dx,word [ecx+eax*2]
		lea	eax,[ebp-52]
		call	NEAR fpc_uchar_to_unicodestr
		mov	edi,dword [ebp-52]
		lea	eax,[ebp-56]
		call	NEAR FPC_UNICODESTR_DECR_REF
		mov	dword [ebp-56],0
		mov	eax,dword [ebp-68]
		mov	ecx,dword [ebp-72]
		mov	dx,word [eax+ecx*2]
		lea	eax,[ebp-56]
		call	NEAR fpc_uchar_to_unicodestr
		mov	edx,dword [ebp-56]
		lea	eax,[ebp-48]
		mov	ecx,edi
		call	NEAR fpc_unicodestr_concat
		mov	eax,dword [ebp-48]
		lea	ecx,[ebp-4]
		mov	edx,1
		call	NEAR SYSTEM_UTF16TOUTF32$UNICODESTRING$LONGINT$LONGINT$$UCS4CHAR
		mov	esi,eax
		shr	eax,18
		or	eax,240
		mov	edx,dword [ebp-60]
		mov	ecx,dword [ebp-80]
		mov	byte [edx+ecx*1],al
		mov	eax,dword [ebp-80]
		inc	eax
		mov	ecx,eax
		mov	eax,esi
		shr	eax,12
		and	eax,63
		or	eax,128
		mov	edx,dword [ebp-60]
		mov	byte [edx+ecx*1],al
		mov	eax,dword [ebp-80]
		add	eax,2
		mov	ecx,eax
		mov	eax,esi
		shr	eax,6
		and	eax,63
		or	eax,128
		mov	edx,dword [ebp-60]
		mov	byte [edx+ecx*1],al
		mov	eax,dword [ebp-80]
		add	eax,3
		mov	ecx,eax
		mov	eax,esi
		and	eax,63
		or	eax,128
		mov	edx,dword [ebp-60]
		mov	byte [edx+ecx*1],al
		add	dword [ebp-80],4
		inc	dword [ebp-72]
..@j7603:
..@j7602:
		inc	dword [ebp-72]
..@j7598:
		mov	eax,dword [ebp-72]
		cmp	eax,dword [ebp-76]
		jnb	NEAR ..@j7599
		cmp	dword [ebp-80],ebx
		jb	NEAR ..@j7597
..@j7599:
		mov	eax,ebx
		dec	eax
		cmp	eax,dword [ebp-80]
		jnb	NEAR ..@j7662
		mov	eax,ebx
		dec	eax
		mov	dword [ebp-80],eax
..@j7662:
		mov	edx,dword [ebp-60]
		mov	eax,dword [ebp-80]
		mov	byte [edx+eax*1],0
		jmp	NEAR ..@j7667
	ALIGN 4
..@j7668:
		mov	edx,dword [ebp-68]
		mov	ecx,dword [ebp-72]
		mov	ax,word [edx+ecx*2]
		sub	ax,127
		jbe	NEAR ..@j7673
		sub	ax,1920
		jbe	NEAR ..@j7674
		sub	ax,53248
		jbe	NEAR ..@j7675
		sub	ax,1024
		jbe	NEAR ..@j7676
		sub	ax,1025
		jb	NEAR ..@j7672
		sub	ax,8191
		jbe	NEAR ..@j7675
		jmp	NEAR ..@j7672
..@j7673:
		inc	dword [ebp-80]
		jmp	NEAR ..@j7671
..@j7674:
		add	dword [ebp-80],2
		jmp	NEAR ..@j7671
..@j7675:
		add	dword [ebp-80],3
		jmp	NEAR ..@j7671
..@j7676:
		mov	eax,dword [ebp-76]
		mov	ecx,0
		sub	eax,1
		sbb	ecx,0
		mov	edx,dword [ebp-72]
		mov	ebx,0
		cmp	ecx,ebx
		jg	NEAR ..@j7680
		jl	NEAR ..@j7671
		cmp	eax,edx
		jna	NEAR ..@j7671
..@j7680:
		mov	eax,dword [ebp-72]
		inc	eax
		mov	edx,dword [ebp-68]
		mov	ax,word [edx+eax*2]
		cmp	ax,56320
		jnae	NEAR ..@j7671
		mov	eax,dword [ebp-72]
		inc	eax
		mov	edx,dword [ebp-68]
		mov	ax,word [edx+eax*2]
		cmp	ax,57343
		jnbe	NEAR ..@j7671
		add	dword [ebp-80],4
		inc	dword [ebp-72]
..@j7672:
..@j7671:
		inc	dword [ebp-72]
..@j7669:
		mov	eax,dword [ebp-72]
		cmp	eax,dword [ebp-76]
		jb	NEAR ..@j7668
..@j7670:
..@j7667:
		mov	eax,dword [ebp-80]
		inc	eax
		mov	dword [ebp-84],eax
..@j7584:
		call	NEAR FPC_POPADDRSTACK
		lea	eax,[ebp-56]
		call	NEAR FPC_UNICODESTR_DECR_REF
		mov	dword [ebp-56],0
		lea	eax,[ebp-52]
		call	NEAR FPC_UNICODESTR_DECR_REF
		mov	dword [ebp-52],0
		lea	eax,[ebp-48]
		call	NEAR FPC_UNICODESTR_DECR_REF
		mov	dword [ebp-48],0
		pop	eax
		test	eax,eax
		je	NEAR ..@j7585
		call	NEAR FPC_RERAISE
..@j7585:
		mov	eax,dword [ebp-84]
		mov	ebx,dword [ebp-96]
		mov	esi,dword [ebp-92]
		mov	edi,dword [ebp-88]
		leave
		ret	4
EXTERN	FPC_RERAISE
EXTERN	FPC_POPADDRSTACK
EXTERN	SYSTEM_UTF16TOUTF32$UNICODESTRING$LONGINT$LONGINT$$UCS4CHAR
EXTERN	fpc_unicodestr_concat
EXTERN	fpc_uchar_to_unicodestr
EXTERN	FPC_UNICODESTR_DECR_REF
EXTERN	FPC_SETJMP
EXTERN	FPC_PUSHEXCEPTADDR
