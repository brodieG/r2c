
r2c-xkkxvb3w71.o:	file format mach-o 64-bit x86-64

SYMBOL TABLE:
0000000000000000 g     F __TEXT,__text _run
0000000000000000         *UND* _R_CheckUserInterrupt

Disassembly of section __TEXT,__text:

0000000000000000 <_run>:
; run():
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:64
; ) {
       0: 55                           	push	rbp
       1: 48 89 e5                     	mov	rbp, rsp
       4: 41 57                        	push	r15
       6: 41 56                        	push	r14
       8: 41 55                        	push	r13
       a: 41 54                        	push	r12
       c: 53                           	push	rbx
       d: 48 81 ec a8 00 00 00         	sub	rsp, 168
      14: 4c 89 85 60 ff ff ff         	mov	qword ptr [rbp - 160], r8
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:69
;   add(data, lens, *di++);
      1b: 48 8b 1a                     	mov	rbx, qword ptr [rdx]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:9
;   int di0 = di[0];
      1e: 48 63 0b                     	movsxd	rcx, dword ptr [rbx]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:11
;   int dires = di[2];
      21: 4c 63 53 08                  	movsxd	r10, dword ptr [rbx + 8]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:14
;   R_xlen_t len1 = lens[di0];
      25: 4c 8b 0c ce                  	mov	r9, qword ptr [rsi + 8*rcx]
      29: 45 31 c0                     	xor	r8d, r8d
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:18
;   if(len1 == 0 || len2 == 0) { // empty recycle is zero
      2c: 4d 85 c9                     	test	r9, r9
      2f: 48 89 7d 88                  	mov	qword ptr [rbp - 120], rdi
      33: 48 89 75 a8                  	mov	qword ptr [rbp - 88], rsi
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:18
;   if(len1 == 0 || len2 == 0) { // empty recycle is zero
      37: 0f 84 bf 05 00 00            	je	0x5fc <_run+0x5fc>
      3d: 48 63 5b 04                  	movsxd	rbx, dword ptr [rbx + 4]
      41: 48 8b 04 de                  	mov	rax, qword ptr [rsi + 8*rbx]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:18
;   if(len1 == 0 || len2 == 0) { // empty recycle is zero
      45: 48 85 c0                     	test	rax, rax
      48: 0f 84 ae 05 00 00            	je	0x5fc <_run+0x5fc>
      4e: 4c 8b 3c cf                  	mov	r15, qword ptr [rdi + 8*rcx]
      52: 4c 8b 2c df                  	mov	r13, qword ptr [rdi + 8*rbx]
      56: 4e 8b 24 d7                  	mov	r12, qword ptr [rdi + 8*r10]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:25
;   if(len1 == len2) {
      5a: 49 39 c1                     	cmp	r9, rax
      5d: 4c 89 4d b0                  	mov	qword ptr [rbp - 80], r9
      61: 48 89 55 98                  	mov	qword ptr [rbp - 104], rdx
      65: 4c 89 55 a0                  	mov	qword ptr [rbp - 96], r10
      69: 0f 85 ca 02 00 00            	jne	0x339 <_run+0x339>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:26
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + e2[i]);});
      6f: 48 8b 07                     	mov	rax, qword ptr [rdi]
      72: f2 4c 0f 2c 70 08            	cvttsd2si	r14, qword ptr [rax + 8]
      78: 49 8d 44 24 30               	lea	rax, [r12 + 48]
      7d: 48 89 45 80                  	mov	qword ptr [rbp - 128], rax
      81: 49 8d 45 30                  	lea	rax, [r13 + 48]
      85: 48 89 85 68 ff ff ff         	mov	qword ptr [rbp - 152], rax
      8c: 49 8d 47 30                  	lea	rax, [r15 + 48]
      90: 48 89 85 70 ff ff ff         	mov	qword ptr [rbp - 144], rax
      97: 31 db                        	xor	ebx, ebx
      99: 4d 39 ce                     	cmp	r14, r9
      9c: 4d 89 ca                     	mov	r10, r9
      9f: 4d 0f 4c d6                  	cmovl	r10, r14
      a3: 4c 89 f1                     	mov	rcx, r14
      a6: 49 0f 4f c9                  	cmovg	rcx, r9
      aa: 4d 8d 1c cc                  	lea	r11, [r12 + 8*rcx]
      ae: 49 8d 04 cf                  	lea	rax, [r15 + 8*rcx]
      b2: 48 89 45 c8                  	mov	qword ptr [rbp - 56], rax
      b6: 49 8d 44 cd 00               	lea	rax, [r13 + 8*rcx]
      bb: 48 89 45 b8                  	mov	qword ptr [rbp - 72], rax
      bf: 48 89 de                     	mov	rsi, rbx
      c2: 4c 89 55 90                  	mov	qword ptr [rbp - 112], r10
      c6: 4c 89 9d 78 ff ff ff         	mov	qword ptr [rbp - 136], r11
      cd: 0f 1f 00                     	nop	dword ptr [rax]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:26
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + e2[i]);});
      d0: 48 89 cf                     	mov	rdi, rcx
      d3: 48 29 f7                     	sub	rdi, rsi
      d6: 0f 8e 04 01 00 00            	jle	0x1e0 <_run+0x1e0>
      dc: 48 83 ff 04                  	cmp	rdi, 4
      e0: 0f 82 6a 01 00 00            	jb	0x250 <_run+0x250>
      e6: 49 8d 04 f4                  	lea	rax, [r12 + 8*rsi]
      ea: 49 8d 14 f7                  	lea	rdx, [r15 + 8*rsi]
      ee: 48 8d 1c f5 00 00 00 00      	lea	rbx, [8*rsi]
      f6: 4c 01 eb                     	add	rbx, r13
      f9: 48 3b 45 c8                  	cmp	rax, qword ptr [rbp - 56]
      fd: 41 0f 92 c0                  	setb	r8b
     101: 4c 39 da                     	cmp	rdx, r11
     104: 0f 92 c2                     	setb	dl
     107: 48 3b 45 b8                  	cmp	rax, qword ptr [rbp - 72]
     10b: 0f 92 c0                     	setb	al
     10e: 4c 39 db                     	cmp	rbx, r11
     111: 0f 92 c3                     	setb	bl
     114: 41 84 d0                     	test	r8b, dl
     117: 0f 85 33 01 00 00            	jne	0x250 <_run+0x250>
     11d: 20 d8                        	and	al, bl
     11f: 0f 85 2b 01 00 00            	jne	0x250 <_run+0x250>
     125: 48 89 fa                     	mov	rdx, rdi
     128: 48 83 e2 fc                  	and	rdx, -4
     12c: 48 8d 42 fc                  	lea	rax, [rdx - 4]
     130: 49 89 c3                     	mov	r11, rax
     133: 49 c1 eb 02                  	shr	r11, 2
     137: 49 83 c3 01                  	add	r11, 1
     13b: 48 85 c0                     	test	rax, rax
     13e: 0f 84 a4 00 00 00            	je	0x1e8 <_run+0x1e8>
     144: 48 8b 45 80                  	mov	rax, qword ptr [rbp - 128]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:26
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + e2[i]);});
     148: 4c 8d 04 f0                  	lea	r8, [rax + 8*rsi]
     14c: 48 8b 85 68 ff ff ff         	mov	rax, qword ptr [rbp - 152]
     153: 4c 8d 0c f0                  	lea	r9, [rax + 8*rsi]
     157: 48 8b 85 70 ff ff ff         	mov	rax, qword ptr [rbp - 144]
     15e: 4c 8d 14 f0                  	lea	r10, [rax + 8*rsi]
     162: 4c 89 d8                     	mov	rax, r11
     165: 48 83 e0 fe                  	and	rax, -2
     169: 48 f7 d8                     	neg	rax
     16c: 31 db                        	xor	ebx, ebx
     16e: 66 90                        	nop
     170: 66 41 0f 10 44 da d0         	movupd	xmm0, xmmword ptr [r10 + 8*rbx - 48]
     177: 66 41 0f 10 4c da e0         	movupd	xmm1, xmmword ptr [r10 + 8*rbx - 32]
     17e: 66 41 0f 10 54 d9 d0         	movupd	xmm2, xmmword ptr [r9 + 8*rbx - 48]
     185: 66 0f 58 d0                  	addpd	xmm2, xmm0
     189: 66 41 0f 10 44 d9 e0         	movupd	xmm0, xmmword ptr [r9 + 8*rbx - 32]
     190: 66 0f 58 c1                  	addpd	xmm0, xmm1
     194: 66 41 0f 11 54 d8 d0         	movupd	xmmword ptr [r8 + 8*rbx - 48], xmm2
     19b: 66 41 0f 11 44 d8 e0         	movupd	xmmword ptr [r8 + 8*rbx - 32], xmm0
     1a2: 66 41 0f 10 44 da f0         	movupd	xmm0, xmmword ptr [r10 + 8*rbx - 16]
     1a9: 66 41 0f 10 0c da            	movupd	xmm1, xmmword ptr [r10 + 8*rbx]
     1af: 66 41 0f 10 54 d9 f0         	movupd	xmm2, xmmword ptr [r9 + 8*rbx - 16]
     1b6: 66 0f 58 d0                  	addpd	xmm2, xmm0
     1ba: 66 41 0f 10 04 d9            	movupd	xmm0, xmmword ptr [r9 + 8*rbx]
     1c0: 66 0f 58 c1                  	addpd	xmm0, xmm1
     1c4: 66 41 0f 11 54 d8 f0         	movupd	xmmword ptr [r8 + 8*rbx - 16], xmm2
     1cb: 66 41 0f 11 04 d8            	movupd	xmmword ptr [r8 + 8*rbx], xmm0
     1d1: 48 83 c3 08                  	add	rbx, 8
     1d5: 48 83 c0 02                  	add	rax, 2
     1d9: 75 95                        	jne	0x170 <_run+0x170>
     1db: eb 0d                        	jmp	0x1ea <_run+0x1ea>
     1dd: 0f 1f 00                     	nop	dword ptr [rax]
     1e0: 48 89 f3                     	mov	rbx, rsi
     1e3: e9 16 01 00 00               	jmp	0x2fe <_run+0x2fe>
     1e8: 31 db                        	xor	ebx, ebx
     1ea: 41 f6 c3 01                  	test	r11b, 1
     1ee: 4c 8b 55 90                  	mov	r10, qword ptr [rbp - 112]
     1f2: 74 33                        	je	0x227 <_run+0x227>
     1f4: 48 01 f3                     	add	rbx, rsi
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:26
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + e2[i]);});
     1f7: 66 41 0f 10 04 df            	movupd	xmm0, xmmword ptr [r15 + 8*rbx]
     1fd: 66 41 0f 10 4c df 10         	movupd	xmm1, xmmword ptr [r15 + 8*rbx + 16]
     204: 66 41 0f 10 54 dd 00         	movupd	xmm2, xmmword ptr [r13 + 8*rbx]
     20b: 66 0f 58 d0                  	addpd	xmm2, xmm0
     20f: 66 41 0f 10 44 dd 10         	movupd	xmm0, xmmword ptr [r13 + 8*rbx + 16]
     216: 66 0f 58 c1                  	addpd	xmm0, xmm1
     21a: 66 41 0f 11 14 dc            	movupd	xmmword ptr [r12 + 8*rbx], xmm2
     220: 66 41 0f 11 44 dc 10         	movupd	xmmword ptr [r12 + 8*rbx + 16], xmm0
     227: 48 89 cb                     	mov	rbx, rcx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:26
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + e2[i]);});
     22a: 48 39 d7                     	cmp	rdi, rdx
     22d: 4c 8b 4d b0                  	mov	r9, qword ptr [rbp - 80]
     231: 4c 8b 9d 78 ff ff ff         	mov	r11, qword ptr [rbp - 136]
     238: 0f 84 c0 00 00 00            	je	0x2fe <_run+0x2fe>
     23e: 48 01 d6                     	add	rsi, rdx
     241: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     24b: 0f 1f 44 00 00               	nop	dword ptr [rax + rax]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:26
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + e2[i]);});
     250: 44 89 d2                     	mov	edx, r10d
     253: 29 f2                        	sub	edx, esi
     255: 48 89 f0                     	mov	rax, rsi
     258: 48 f7 d0                     	not	rax
     25b: 4c 01 d0                     	add	rax, r10
     25e: 48 83 e2 03                  	and	rdx, 3
     262: 74 29                        	je	0x28d <_run+0x28d>
     264: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     26e: 66 90                        	nop
     270: f2 41 0f 10 04 f7            	movsd	xmm0, qword ptr [r15 + 8*rsi] ## xmm0 = mem[0],zero
     276: f2 41 0f 58 44 f5 00         	addsd	xmm0, qword ptr [r13 + 8*rsi]
     27d: f2 41 0f 11 04 f4            	movsd	qword ptr [r12 + 8*rsi], xmm0
     283: 48 83 c6 01                  	add	rsi, 1
     287: 48 83 c2 ff                  	add	rdx, -1
     28b: 75 e3                        	jne	0x270 <_run+0x270>
     28d: 48 89 cb                     	mov	rbx, rcx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:26
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + e2[i]);});
     290: 48 83 f8 03                  	cmp	rax, 3
     294: 72 68                        	jb	0x2fe <_run+0x2fe>
     296: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     2a0: f2 41 0f 10 04 f7            	movsd	xmm0, qword ptr [r15 + 8*rsi] ## xmm0 = mem[0],zero
     2a6: f2 41 0f 58 44 f5 00         	addsd	xmm0, qword ptr [r13 + 8*rsi]
     2ad: f2 41 0f 11 04 f4            	movsd	qword ptr [r12 + 8*rsi], xmm0
     2b3: f2 41 0f 10 44 f7 08         	movsd	xmm0, qword ptr [r15 + 8*rsi + 8] ## xmm0 = mem[0],zero
     2ba: f2 41 0f 58 44 f5 08         	addsd	xmm0, qword ptr [r13 + 8*rsi + 8]
     2c1: f2 41 0f 11 44 f4 08         	movsd	qword ptr [r12 + 8*rsi + 8], xmm0
     2c8: f2 41 0f 10 44 f7 10         	movsd	xmm0, qword ptr [r15 + 8*rsi + 16] ## xmm0 = mem[0],zero
     2cf: f2 41 0f 58 44 f5 10         	addsd	xmm0, qword ptr [r13 + 8*rsi + 16]
     2d6: f2 41 0f 11 44 f4 10         	movsd	qword ptr [r12 + 8*rsi + 16], xmm0
     2dd: f2 41 0f 10 44 f7 18         	movsd	xmm0, qword ptr [r15 + 8*rsi + 24] ## xmm0 = mem[0],zero
     2e4: f2 41 0f 58 44 f5 18         	addsd	xmm0, qword ptr [r13 + 8*rsi + 24]
     2eb: f2 41 0f 11 44 f4 18         	movsd	qword ptr [r12 + 8*rsi + 24], xmm0
     2f2: 48 83 c6 04                  	add	rsi, 4
     2f6: 48 39 f1                     	cmp	rcx, rsi
     2f9: 75 a5                        	jne	0x2a0 <_run+0x2a0>
     2fb: 48 89 cb                     	mov	rbx, rcx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:26
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + e2[i]);});
     2fe: 4c 39 cb                     	cmp	rbx, r9
     301: 0f 84 c6 02 00 00            	je	0x5cd <_run+0x5cd>
     307: 48 89 de                     	mov	rsi, rbx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:26
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + e2[i]);});
     30a: 4c 39 f3                     	cmp	rbx, r14
     30d: 0f 85 bd fd ff ff            	jne	0xd0 <_run+0xd0>
     313: e8 00 00 00 00               	call	0x318 <_run+0x318>
		0000000000000314:  X86_64_RELOC_BRANCH	_R_CheckUserInterrupt
     318: 4c 8b 4d b0                  	mov	r9, qword ptr [rbp - 80]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:26
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + e2[i]);});
     31c: 48 b8 81 69 67 ff ff ff 0f 00	movabs	rax, 4503599617370497
     326: 49 39 c6                     	cmp	r14, rax
     329: 4d 8d b6 80 96 98 00         	lea	r14, [r14 + 10000000]
     330: 4d 0f 4d f1                  	cmovge	r14, r9
     334: e9 60 fd ff ff               	jmp	0x99 <_run+0x99>
     339: 49 89 c0                     	mov	r8, rax
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:28
;   } else if (len2 == 1) {
     33c: 48 83 f8 01                  	cmp	rax, 1
     340: 0f 85 6a 05 00 00            	jne	0x8b0 <_run+0x8b0>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:29
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + *e2);});
     346: 48 8b 07                     	mov	rax, qword ptr [rdi]
     349: f2 4c 0f 2c 70 08            	cvttsd2si	r14, qword ptr [rax + 8]
     34f: 49 8d 45 08                  	lea	rax, [r13 + 8]
     353: 48 89 45 90                  	mov	qword ptr [rbp - 112], rax
     357: 49 8d 44 24 30               	lea	rax, [r12 + 48]
     35c: 48 89 85 78 ff ff ff         	mov	qword ptr [rbp - 136], rax
     363: 49 8d 47 30                  	lea	rax, [r15 + 48]
     367: 48 89 45 80                  	mov	qword ptr [rbp - 128], rax
     36b: 31 db                        	xor	ebx, ebx
     36d: 4d 39 ce                     	cmp	r14, r9
     370: 4d 89 ca                     	mov	r10, r9
     373: 4d 0f 4c d6                  	cmovl	r10, r14
     377: 4c 89 f1                     	mov	rcx, r14
     37a: 49 0f 4f c9                  	cmovg	rcx, r9
     37e: 49 8d 04 cc                  	lea	rax, [r12 + 8*rcx]
     382: 48 89 45 c8                  	mov	qword ptr [rbp - 56], rax
     386: 49 8d 04 cf                  	lea	rax, [r15 + 8*rcx]
     38a: 48 89 45 b8                  	mov	qword ptr [rbp - 72], rax
     38e: 48 89 df                     	mov	rdi, rbx
     391: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     39b: 0f 1f 44 00 00               	nop	dword ptr [rax + rax]
     3a0: 48 89 ce                     	mov	rsi, rcx
     3a3: 48 29 fe                     	sub	rsi, rdi
     3a6: 0f 8e d4 01 00 00            	jle	0x580 <_run+0x580>
     3ac: 48 83 fe 04                  	cmp	rsi, 4
     3b0: 0f 82 1a 01 00 00            	jb	0x4d0 <_run+0x4d0>
     3b6: 48 8b 45 c8                  	mov	rax, qword ptr [rbp - 56]
     3ba: 49 39 c5                     	cmp	r13, rax
     3bd: 41 0f 92 c0                  	setb	r8b
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:29
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + *e2);});
     3c1: 49 8d 14 fc                  	lea	rdx, [r12 + 8*rdi]
     3c5: 49 8d 1c ff                  	lea	rbx, [r15 + 8*rdi]
     3c9: 48 3b 55 b8                  	cmp	rdx, qword ptr [rbp - 72]
     3cd: 41 0f 92 c3                  	setb	r11b
     3d1: 48 39 c3                     	cmp	rbx, rax
     3d4: 0f 92 c3                     	setb	bl
     3d7: 48 3b 55 90                  	cmp	rdx, qword ptr [rbp - 112]
     3db: 0f 92 c2                     	setb	dl
     3de: 41 84 db                     	test	r11b, bl
     3e1: 0f 85 e9 00 00 00            	jne	0x4d0 <_run+0x4d0>
     3e7: 44 20 c2                     	and	dl, r8b
     3ea: 0f 85 e0 00 00 00            	jne	0x4d0 <_run+0x4d0>
     3f0: 49 89 f3                     	mov	r11, rsi
     3f3: 49 83 e3 fc                  	and	r11, -4
     3f7: 49 8d 43 fc                  	lea	rax, [r11 - 4]
     3fb: 49 89 c1                     	mov	r9, rax
     3fe: 49 c1 e9 02                  	shr	r9, 2
     402: 49 83 c1 01                  	add	r9, 1
     406: 48 85 c0                     	test	rax, rax
     409: 0f 84 87 01 00 00            	je	0x596 <_run+0x596>
     40f: f2 41 0f 12 45 00            	movddup	xmm0, qword ptr [r13]   ## xmm0 = mem[0,0]
     415: 48 8b 85 78 ff ff ff         	mov	rax, qword ptr [rbp - 136]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:29
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + *e2);});
     41c: 48 8d 14 f8                  	lea	rdx, [rax + 8*rdi]
     420: 48 8b 45 80                  	mov	rax, qword ptr [rbp - 128]
     424: 4c 8d 04 f8                  	lea	r8, [rax + 8*rdi]
     428: 4c 89 c8                     	mov	rax, r9
     42b: 48 83 e0 fe                  	and	rax, -2
     42f: 48 f7 d8                     	neg	rax
     432: 31 db                        	xor	ebx, ebx
     434: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     43e: 66 90                        	nop
     440: 66 41 0f 10 4c d8 d0         	movupd	xmm1, xmmword ptr [r8 + 8*rbx - 48]
     447: 66 41 0f 10 54 d8 e0         	movupd	xmm2, xmmword ptr [r8 + 8*rbx - 32]
     44e: 66 0f 58 c8                  	addpd	xmm1, xmm0
     452: 66 0f 58 d0                  	addpd	xmm2, xmm0
     456: 66 0f 11 4c da d0            	movupd	xmmword ptr [rdx + 8*rbx - 48], xmm1
     45c: 66 0f 11 54 da e0            	movupd	xmmword ptr [rdx + 8*rbx - 32], xmm2
     462: 66 41 0f 10 4c d8 f0         	movupd	xmm1, xmmword ptr [r8 + 8*rbx - 16]
     469: 66 41 0f 10 14 d8            	movupd	xmm2, xmmword ptr [r8 + 8*rbx]
     46f: 66 0f 58 c8                  	addpd	xmm1, xmm0
     473: 66 0f 58 d0                  	addpd	xmm2, xmm0
     477: 66 0f 11 4c da f0            	movupd	xmmword ptr [rdx + 8*rbx - 16], xmm1
     47d: 66 0f 11 14 da               	movupd	xmmword ptr [rdx + 8*rbx], xmm2
     482: 48 83 c3 08                  	add	rbx, 8
     486: 48 83 c0 02                  	add	rax, 2
     48a: 75 b4                        	jne	0x440 <_run+0x440>
     48c: 41 f6 c1 01                  	test	r9b, 1
     490: 74 2b                        	je	0x4bd <_run+0x4bd>
     492: 48 01 fb                     	add	rbx, rdi
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:29
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + *e2);});
     495: 66 41 0f 10 04 df            	movupd	xmm0, xmmword ptr [r15 + 8*rbx]
     49b: 66 41 0f 10 4c df 10         	movupd	xmm1, xmmword ptr [r15 + 8*rbx + 16]
     4a2: f2 41 0f 12 55 00            	movddup	xmm2, qword ptr [r13]   ## xmm2 = mem[0,0]
     4a8: 66 0f 58 c2                  	addpd	xmm0, xmm2
     4ac: 66 0f 58 ca                  	addpd	xmm1, xmm2
     4b0: 66 41 0f 11 04 dc            	movupd	xmmword ptr [r12 + 8*rbx], xmm0
     4b6: 66 41 0f 11 4c dc 10         	movupd	xmmword ptr [r12 + 8*rbx + 16], xmm1
     4bd: 48 89 cb                     	mov	rbx, rcx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:29
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + *e2);});
     4c0: 4c 39 de                     	cmp	rsi, r11
     4c3: 4c 8b 4d b0                  	mov	r9, qword ptr [rbp - 80]
     4c7: 0f 84 b6 00 00 00            	je	0x583 <_run+0x583>
     4cd: 4c 01 df                     	add	rdi, r11
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:29
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + *e2);});
     4d0: 44 89 d2                     	mov	edx, r10d
     4d3: 29 fa                        	sub	edx, edi
     4d5: 48 89 f8                     	mov	rax, rdi
     4d8: 48 f7 d0                     	not	rax
     4db: 4c 01 d0                     	add	rax, r10
     4de: 48 83 e2 03                  	and	rdx, 3
     4e2: 74 28                        	je	0x50c <_run+0x50c>
     4e4: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     4ee: 66 90                        	nop
     4f0: f2 41 0f 10 04 ff            	movsd	xmm0, qword ptr [r15 + 8*rdi] ## xmm0 = mem[0],zero
     4f6: f2 41 0f 58 45 00            	addsd	xmm0, qword ptr [r13]
     4fc: f2 41 0f 11 04 fc            	movsd	qword ptr [r12 + 8*rdi], xmm0
     502: 48 83 c7 01                  	add	rdi, 1
     506: 48 83 c2 ff                  	add	rdx, -1
     50a: 75 e4                        	jne	0x4f0 <_run+0x4f0>
     50c: 48 89 cb                     	mov	rbx, rcx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:29
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + *e2);});
     50f: 48 83 f8 03                  	cmp	rax, 3
     513: 72 6e                        	jb	0x583 <_run+0x583>
     515: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     51f: 90                           	nop
     520: f2 41 0f 10 04 ff            	movsd	xmm0, qword ptr [r15 + 8*rdi] ## xmm0 = mem[0],zero
     526: f2 41 0f 58 45 00            	addsd	xmm0, qword ptr [r13]
     52c: f2 41 0f 11 04 fc            	movsd	qword ptr [r12 + 8*rdi], xmm0
     532: f2 41 0f 10 44 ff 08         	movsd	xmm0, qword ptr [r15 + 8*rdi + 8] ## xmm0 = mem[0],zero
     539: f2 41 0f 58 45 00            	addsd	xmm0, qword ptr [r13]
     53f: f2 41 0f 11 44 fc 08         	movsd	qword ptr [r12 + 8*rdi + 8], xmm0
     546: f2 41 0f 10 44 ff 10         	movsd	xmm0, qword ptr [r15 + 8*rdi + 16] ## xmm0 = mem[0],zero
     54d: f2 41 0f 58 45 00            	addsd	xmm0, qword ptr [r13]
     553: f2 41 0f 11 44 fc 10         	movsd	qword ptr [r12 + 8*rdi + 16], xmm0
     55a: f2 41 0f 10 44 ff 18         	movsd	xmm0, qword ptr [r15 + 8*rdi + 24] ## xmm0 = mem[0],zero
     561: f2 41 0f 58 45 00            	addsd	xmm0, qword ptr [r13]
     567: f2 41 0f 11 44 fc 18         	movsd	qword ptr [r12 + 8*rdi + 24], xmm0
     56e: 48 83 c7 04                  	add	rdi, 4
     572: 48 39 f9                     	cmp	rcx, rdi
     575: 75 a9                        	jne	0x520 <_run+0x520>
     577: 48 89 cb                     	mov	rbx, rcx
     57a: eb 07                        	jmp	0x583 <_run+0x583>
     57c: 0f 1f 40 00                  	nop	dword ptr [rax]
     580: 48 89 fb                     	mov	rbx, rdi
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:29
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + *e2);});
     583: 4c 39 cb                     	cmp	rbx, r9
     586: 74 45                        	je	0x5cd <_run+0x5cd>
     588: 48 89 df                     	mov	rdi, rbx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:29
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + *e2);});
     58b: 4c 39 f3                     	cmp	rbx, r14
     58e: 0f 85 0c fe ff ff            	jne	0x3a0 <_run+0x3a0>
     594: eb 11                        	jmp	0x5a7 <_run+0x5a7>
     596: 31 db                        	xor	ebx, ebx
     598: 41 f6 c1 01                  	test	r9b, 1
     59c: 0f 85 f0 fe ff ff            	jne	0x492 <_run+0x492>
     5a2: e9 16 ff ff ff               	jmp	0x4bd <_run+0x4bd>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:29
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + *e2);});
     5a7: e8 00 00 00 00               	call	0x5ac <_run+0x5ac>
		00000000000005a8:  X86_64_RELOC_BRANCH	_R_CheckUserInterrupt
     5ac: 4c 8b 4d b0                  	mov	r9, qword ptr [rbp - 80]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:29
;     LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + *e2);});
     5b0: 48 b8 81 69 67 ff ff ff 0f 00	movabs	rax, 4503599617370497
     5ba: 49 39 c6                     	cmp	r14, rax
     5bd: 4d 8d b6 80 96 98 00         	lea	r14, [r14 + 10000000]
     5c4: 4d 0f 4d f1                  	cmovge	r14, r9
     5c8: e9 a0 fd ff ff               	jmp	0x36d <_run+0x36d>
     5cd: 0f 57 c0                     	xorps	xmm0, xmm0
     5d0: f2 49 0f 2a c6               	cvtsi2sd	xmm0, r14
     5d5: 0f 57 c9                     	xorps	xmm1, xmm1
     5d8: f2 49 0f 2a c9               	cvtsi2sd	xmm1, r9
     5dd: f2 0f 5c c1                  	subsd	xmm0, xmm1
     5e1: 48 8b 7d 88                  	mov	rdi, qword ptr [rbp - 120]
     5e5: 48 8b 07                     	mov	rax, qword ptr [rdi]
     5e8: f2 0f 11 40 08               	movsd	qword ptr [rax + 8], xmm0
     5ed: 4d 89 c8                     	mov	r8, r9
     5f0: 48 8b 75 a8                  	mov	rsi, qword ptr [rbp - 88]
     5f4: 48 8b 55 98                  	mov	rdx, qword ptr [rbp - 104]
     5f8: 4c 8b 55 a0                  	mov	r10, qword ptr [rbp - 96]
     5fc: 4e 89 04 d6                  	mov	qword ptr [rsi + 8*r10], r8
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:73
;   sum(data, lens, *di++, *flag);
     600: 48 8b 42 08                  	mov	rax, qword ptr [rdx + 8]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:46
;   int di0 = di[0];
     604: 48 63 08                     	movsxd	rcx, dword ptr [rax]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:47
;   int di1 = di[1];
     607: 4c 63 70 04                  	movsxd	r14, dword ptr [rax + 4]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:49
;   R_xlen_t len_n = lens[di0];
     60b: 4c 8b 3c ce                  	mov	r15, qword ptr [rsi + 8*rcx]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:50
;   double * dat = data[di0];
     60f: 48 8b 1c cf                  	mov	rbx, qword ptr [rdi + 8*rcx]
     613: 48 8b 85 60 ff ff ff         	mov	rax, qword ptr [rbp - 160]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:55
;   if(!narm) LOOP_W_INTERRUPT1(len_n, tmp += dat[i];);
     61a: 83 78 04 00                  	cmp	dword ptr [rax + 4], 0
     61e: 48 8b 07                     	mov	rax, qword ptr [rdi]
     621: f2 4c 0f 2c 60 08            	cvttsd2si	r12, qword ptr [rax + 8]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:55
;   if(!narm) LOOP_W_INTERRUPT1(len_n, tmp += dat[i];);
     627: 0f 84 79 01 00 00            	je	0x7a6 <_run+0x7a6>
     62d: d9 ee                        	fldz
     62f: 45 31 ed                     	xor	r13d, r13d
     632: d9 c0                        	fld	st(0)
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     634: 4d 39 fc                     	cmp	r12, r15
     637: 4c 89 f8                     	mov	rax, r15
     63a: 49 0f 4c c4                  	cmovl	rax, r12
     63e: 4c 89 e1                     	mov	rcx, r12
     641: 49 0f 4f cf                  	cmovg	rcx, r15
     645: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     64f: 90                           	nop
     650: 49 39 cd                     	cmp	r13, rcx
     653: 0f 8d 06 01 00 00            	jge	0x75f <_run+0x75f>
     659: 89 c7                        	mov	edi, eax
     65b: 44 29 ef                     	sub	edi, r13d
     65e: 4c 89 ee                     	mov	rsi, r13
     661: 48 f7 d6                     	not	rsi
     664: 48 01 c6                     	add	rsi, rax
     667: 48 83 e7 03                  	and	rdi, 3
     66b: 74 44                        	je	0x6b1 <_run+0x6b1>
     66d: 4c 89 ea                     	mov	rdx, r13
     670: d9 ee                        	fldz
     672: d9 c9                        	fxch	st(1)
     674: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     67e: 66 90                        	nop
     680: dd d9                        	fstp	st(1)
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     682: f2 0f 10 04 d3               	movsd	xmm0, qword ptr [rbx + 8*rdx] ## xmm0 = mem[0],zero
; /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/math.h:212
;     return __x != __x;
     687: 66 0f 2e c0                  	ucomisd	xmm0, xmm0
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     68b: f2 0f 11 85 38 ff ff ff      	movsd	qword ptr [rbp - 200], xmm0
     693: d9 c0                        	fld	st(0)
     695: dc 85 38 ff ff ff            	fadd	qword ptr [rbp - 200]
     69b: d9 c9                        	fxch	st(1)
     69d: db d9                        	fcmovnu	st, st(1)
     69f: dd d9                        	fstp	st(1)
     6a1: 48 83 c2 01                  	add	rdx, 1
     6a5: d9 c0                        	fld	st(0)
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     6a7: 48 83 c7 ff                  	add	rdi, -1
     6ab: d9 c9                        	fxch	st(1)
     6ad: 75 d1                        	jne	0x680 <_run+0x680>
     6af: eb 05                        	jmp	0x6b6 <_run+0x6b6>
     6b1: d9 c1                        	fld	st(1)
     6b3: 4c 89 ea                     	mov	rdx, r13
     6b6: 49 89 cd                     	mov	r13, rcx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     6b9: 48 83 fe 03                  	cmp	rsi, 3
     6bd: 0f 82 a0 00 00 00            	jb	0x763 <_run+0x763>
     6c3: dd d8                        	fstp	st(0)
     6c5: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     6cf: 90                           	nop
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     6d0: f2 0f 10 04 d3               	movsd	xmm0, qword ptr [rbx + 8*rdx] ## xmm0 = mem[0],zero
; /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/math.h:212
;     return __x != __x;
     6d5: 66 0f 2e c0                  	ucomisd	xmm0, xmm0
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     6d9: f2 0f 10 4c d3 08            	movsd	xmm1, qword ptr [rbx + 8*rdx + 8] ## xmm1 = mem[0],zero
     6df: f2 0f 11 85 40 ff ff ff      	movsd	qword ptr [rbp - 192], xmm0
     6e7: d9 c0                        	fld	st(0)
     6e9: dc 85 40 ff ff ff            	fadd	qword ptr [rbp - 192]
     6ef: d9 c9                        	fxch	st(1)
     6f1: db d9                        	fcmovnu	st, st(1)
     6f3: dd d9                        	fstp	st(1)
; /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/math.h:212
;     return __x != __x;
     6f5: 66 0f 2e c9                  	ucomisd	xmm1, xmm1
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     6f9: f2 0f 11 8d 58 ff ff ff      	movsd	qword ptr [rbp - 168], xmm1
     701: d9 c0                        	fld	st(0)
     703: dc 85 58 ff ff ff            	fadd	qword ptr [rbp - 168]
     709: d9 c9                        	fxch	st(1)
     70b: db d9                        	fcmovnu	st, st(1)
     70d: dd d9                        	fstp	st(1)
     70f: f2 0f 10 44 d3 10            	movsd	xmm0, qword ptr [rbx + 8*rdx + 16] ## xmm0 = mem[0],zero
; /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/math.h:212
;     return __x != __x;
     715: 66 0f 2e c0                  	ucomisd	xmm0, xmm0
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     719: f2 0f 11 85 50 ff ff ff      	movsd	qword ptr [rbp - 176], xmm0
     721: d9 c0                        	fld	st(0)
     723: dc 85 50 ff ff ff            	fadd	qword ptr [rbp - 176]
     729: d9 c9                        	fxch	st(1)
     72b: db d9                        	fcmovnu	st, st(1)
     72d: dd d9                        	fstp	st(1)
     72f: f2 0f 10 44 d3 18            	movsd	xmm0, qword ptr [rbx + 8*rdx + 24] ## xmm0 = mem[0],zero
; /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/math.h:212
;     return __x != __x;
     735: 66 0f 2e c0                  	ucomisd	xmm0, xmm0
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     739: f2 0f 11 85 48 ff ff ff      	movsd	qword ptr [rbp - 184], xmm0
     741: d9 c0                        	fld	st(0)
     743: dc 85 48 ff ff ff            	fadd	qword ptr [rbp - 184]
     749: d9 c9                        	fxch	st(1)
     74b: db d9                        	fcmovnu	st, st(1)
     74d: dd d9                        	fstp	st(1)
     74f: 48 83 c2 04                  	add	rdx, 4
     753: 48 39 d0                     	cmp	rax, rdx
     756: 0f 85 74 ff ff ff            	jne	0x6d0 <_run+0x6d0>
     75c: 49 89 cd                     	mov	r13, rcx
     75f: d9 ee                        	fldz
     761: d9 c9                        	fxch	st(1)
     763: dd d9                        	fstp	st(1)
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     765: 4d 39 fd                     	cmp	r13, r15
     768: 0f 84 fc 00 00 00            	je	0x86a <_run+0x86a>
     76e: 4d 39 e5                     	cmp	r13, r12
     771: 0f 85 d9 fe ff ff            	jne	0x650 <_run+0x650>
     777: db 7d c8                     	fstp	tbyte ptr [rbp - 56]
     77a: db 7d b8                     	fstp	tbyte ptr [rbp - 72]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     77d: e8 00 00 00 00               	call	0x782 <_run+0x782>
		000000000000077e:  X86_64_RELOC_BRANCH	_R_CheckUserInterrupt
     782: db 6d b8                     	fld	tbyte ptr [rbp - 72]
     785: db 6d c8                     	fld	tbyte ptr [rbp - 56]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:56
;   else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});
     788: 48 b8 81 69 67 ff ff ff 0f 00	movabs	rax, 4503599617370497
     792: 49 39 c4                     	cmp	r12, rax
     795: 4d 8d a4 24 80 96 98 00      	lea	r12, [r12 + 10000000]
     79d: 4d 0f 4d e7                  	cmovge	r12, r15
     7a1: e9 8e fe ff ff               	jmp	0x634 <_run+0x634>
     7a6: d9 ee                        	fldz
     7a8: 45 31 ed                     	xor	r13d, r13d
     7ab: d9 c0                        	fld	st(0)
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:55
;   if(!narm) LOOP_W_INTERRUPT1(len_n, tmp += dat[i];);
     7ad: 4d 39 fc                     	cmp	r12, r15
     7b0: 4c 89 f8                     	mov	rax, r15
     7b3: 49 0f 4c c4                  	cmovl	rax, r12
     7b7: 4c 89 e1                     	mov	rcx, r12
     7ba: 49 0f 4f cf                  	cmovg	rcx, r15
     7be: 66 90                        	nop
     7c0: 49 39 cd                     	cmp	r13, rcx
     7c3: 7d 66                        	jge	0x82b <_run+0x82b>
     7c5: 89 c7                        	mov	edi, eax
     7c7: 44 29 ef                     	sub	edi, r13d
     7ca: 4c 89 ee                     	mov	rsi, r13
     7cd: 48 f7 d6                     	not	rsi
     7d0: 48 01 c6                     	add	rsi, rax
     7d3: 48 83 e7 03                  	and	rdi, 3
     7d7: 74 1c                        	je	0x7f5 <_run+0x7f5>
     7d9: 4c 89 ea                     	mov	rdx, r13
     7dc: d9 ee                        	fldz
     7de: d9 c9                        	fxch	st(1)
     7e0: dd d9                        	fstp	st(1)
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:55
;   if(!narm) LOOP_W_INTERRUPT1(len_n, tmp += dat[i];);
     7e2: dc 04 d3                     	fadd	qword ptr [rbx + 8*rdx]
     7e5: 48 83 c2 01                  	add	rdx, 1
     7e9: d9 c0                        	fld	st(0)
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:55
;   if(!narm) LOOP_W_INTERRUPT1(len_n, tmp += dat[i];);
     7eb: 48 83 c7 ff                  	add	rdi, -1
     7ef: d9 c9                        	fxch	st(1)
     7f1: 75 ed                        	jne	0x7e0 <_run+0x7e0>
     7f3: eb 05                        	jmp	0x7fa <_run+0x7fa>
     7f5: d9 c1                        	fld	st(1)
     7f7: 4c 89 ea                     	mov	rdx, r13
     7fa: 49 89 cd                     	mov	r13, rcx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:55
;   if(!narm) LOOP_W_INTERRUPT1(len_n, tmp += dat[i];);
     7fd: 48 83 fe 03                  	cmp	rsi, 3
     801: 72 2c                        	jb	0x82f <_run+0x82f>
     803: dd d8                        	fstp	st(0)
     805: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     80f: 90                           	nop
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:55
;   if(!narm) LOOP_W_INTERRUPT1(len_n, tmp += dat[i];);
     810: dc 04 d3                     	fadd	qword ptr [rbx + 8*rdx]
     813: dc 44 d3 08                  	fadd	qword ptr [rbx + 8*rdx + 8]
     817: dc 44 d3 10                  	fadd	qword ptr [rbx + 8*rdx + 16]
     81b: dc 44 d3 18                  	fadd	qword ptr [rbx + 8*rdx + 24]
     81f: 48 83 c2 04                  	add	rdx, 4
     823: 48 39 d0                     	cmp	rax, rdx
     826: 75 e8                        	jne	0x810 <_run+0x810>
     828: 49 89 cd                     	mov	r13, rcx
     82b: d9 ee                        	fldz
     82d: d9 c9                        	fxch	st(1)
     82f: dd d9                        	fstp	st(1)
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:55
;   if(!narm) LOOP_W_INTERRUPT1(len_n, tmp += dat[i];);
     831: 4d 39 fd                     	cmp	r13, r15
     834: 74 34                        	je	0x86a <_run+0x86a>
     836: 4d 39 e5                     	cmp	r13, r12
     839: 75 85                        	jne	0x7c0 <_run+0x7c0>
     83b: db 7d c8                     	fstp	tbyte ptr [rbp - 56]
     83e: db 7d b8                     	fstp	tbyte ptr [rbp - 72]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:55
;   if(!narm) LOOP_W_INTERRUPT1(len_n, tmp += dat[i];);
     841: e8 00 00 00 00               	call	0x846 <_run+0x846>
		0000000000000842:  X86_64_RELOC_BRANCH	_R_CheckUserInterrupt
     846: db 6d b8                     	fld	tbyte ptr [rbp - 72]
     849: db 6d c8                     	fld	tbyte ptr [rbp - 56]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:55
;   if(!narm) LOOP_W_INTERRUPT1(len_n, tmp += dat[i];);
     84c: 48 b8 81 69 67 ff ff ff 0f 00	movabs	rax, 4503599617370497
     856: 49 39 c4                     	cmp	r12, rax
     859: 4d 8d a4 24 80 96 98 00      	lea	r12, [r12 + 10000000]
     861: 4d 0f 4d e7                  	cmovge	r12, r15
     865: e9 43 ff ff ff               	jmp	0x7ad <_run+0x7ad>
     86a: dd d9                        	fstp	st(1)
     86c: 0f 57 c0                     	xorps	xmm0, xmm0
     86f: f2 49 0f 2a c4               	cvtsi2sd	xmm0, r12
     874: 0f 57 c9                     	xorps	xmm1, xmm1
     877: f2 49 0f 2a cf               	cvtsi2sd	xmm1, r15
     87c: f2 0f 5c c1                  	subsd	xmm0, xmm1
     880: 48 8b 4d 88                  	mov	rcx, qword ptr [rbp - 120]
     884: 48 8b 01                     	mov	rax, qword ptr [rcx]
     887: f2 0f 11 40 08               	movsd	qword ptr [rax + 8], xmm0
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:58
;   *data[di1] = (double) tmp;
     88c: 4a 8b 04 f1                  	mov	rax, qword ptr [rcx + 8*r14]
     890: dd 18                        	fstp	qword ptr [rax]
     892: 48 8b 45 a8                  	mov	rax, qword ptr [rbp - 88]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:59
;   lens[di1] = 1;
     896: 4a c7 04 f0 01 00 00 00      	mov	qword ptr [rax + 8*r14], 1
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:75
; }
     89e: 48 81 c4 a8 00 00 00         	add	rsp, 168
     8a5: 5b                           	pop	rbx
     8a6: 41 5c                        	pop	r12
     8a8: 41 5d                        	pop	r13
     8aa: 41 5e                        	pop	r14
     8ac: 41 5f                        	pop	r15
     8ae: 5d                           	pop	rbp
     8af: c3                           	ret
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:31
;   } else if (len1 == 1) {
     8b0: 49 83 f9 01                  	cmp	r9, 1
     8b4: 4c 89 45 c8                  	mov	qword ptr [rbp - 56], r8
     8b8: 0f 85 97 02 00 00            	jne	0xb55 <_run+0xb55>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:32
;     LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
     8be: 48 8b 07                     	mov	rax, qword ptr [rdi]
     8c1: f2 4c 0f 2c 70 08            	cvttsd2si	r14, qword ptr [rax + 8]
     8c7: 49 8d 47 08                  	lea	rax, [r15 + 8]
     8cb: 48 89 45 b0                  	mov	qword ptr [rbp - 80], rax
     8cf: 49 8d 44 24 30               	lea	rax, [r12 + 48]
     8d4: 48 89 85 78 ff ff ff         	mov	qword ptr [rbp - 136], rax
     8db: 49 8d 45 30                  	lea	rax, [r13 + 48]
     8df: 48 89 45 80                  	mov	qword ptr [rbp - 128], rax
     8e3: 31 db                        	xor	ebx, ebx
     8e5: 4d 39 c6                     	cmp	r14, r8
     8e8: 4d 89 c2                     	mov	r10, r8
     8eb: 4d 0f 4c d6                  	cmovl	r10, r14
     8ef: 4c 89 f1                     	mov	rcx, r14
     8f2: 49 0f 4f c8                  	cmovg	rcx, r8
     8f6: 4d 8d 0c cc                  	lea	r9, [r12 + 8*rcx]
     8fa: 48 8d 04 cd 00 00 00 00      	lea	rax, [8*rcx]
     902: 4c 01 e8                     	add	rax, r13
     905: 48 89 45 b8                  	mov	qword ptr [rbp - 72], rax
     909: 48 89 df                     	mov	rdi, rbx
     90c: 4c 89 4d 90                  	mov	qword ptr [rbp - 112], r9
     910: 48 89 ce                     	mov	rsi, rcx
     913: 48 29 fe                     	sub	rsi, rdi
     916: 0f 8e 34 01 00 00            	jle	0xa50 <_run+0xa50>
     91c: 48 83 fe 04                  	cmp	rsi, 4
     920: 0f 82 3a 01 00 00            	jb	0xa60 <_run+0xa60>
     926: 4d 39 cf                     	cmp	r15, r9
     929: 41 0f 92 c0                  	setb	r8b
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:32
;     LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
     92d: 49 8d 04 fc                  	lea	rax, [r12 + 8*rdi]
     931: 48 8d 14 fd 00 00 00 00      	lea	rdx, [8*rdi]
     939: 4c 01 ea                     	add	rdx, r13
     93c: 48 3b 45 b0                  	cmp	rax, qword ptr [rbp - 80]
     940: 0f 92 c3                     	setb	bl
     943: 48 3b 45 b8                  	cmp	rax, qword ptr [rbp - 72]
     947: 0f 92 c0                     	setb	al
     94a: 4c 39 ca                     	cmp	rdx, r9
     94d: 0f 92 c2                     	setb	dl
     950: 44 84 c3                     	test	bl, r8b
     953: 0f 85 ff 00 00 00            	jne	0xa58 <_run+0xa58>
     959: 20 d0                        	and	al, dl
     95b: 4c 8b 45 c8                  	mov	r8, qword ptr [rbp - 56]
     95f: 0f 85 fb 00 00 00            	jne	0xa60 <_run+0xa60>
     965: 49 89 f3                     	mov	r11, rsi
     968: 49 83 e3 fc                  	and	r11, -4
     96c: 49 8d 43 fc                  	lea	rax, [r11 - 4]
     970: 49 89 c1                     	mov	r9, rax
     973: 49 c1 e9 02                  	shr	r9, 2
     977: 49 83 c1 01                  	add	r9, 1
     97b: 48 85 c0                     	test	rax, rax
     97e: 0f 84 9a 01 00 00            	je	0xb1e <_run+0xb1e>
     984: f2 41 0f 12 07               	movddup	xmm0, qword ptr [r15]   ## xmm0 = mem[0,0]
     989: 48 8b 85 78 ff ff ff         	mov	rax, qword ptr [rbp - 136]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:32
;     LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
     990: 48 8d 14 f8                  	lea	rdx, [rax + 8*rdi]
     994: 48 8b 45 80                  	mov	rax, qword ptr [rbp - 128]
     998: 4c 8d 04 f8                  	lea	r8, [rax + 8*rdi]
     99c: 4c 89 cb                     	mov	rbx, r9
     99f: 48 83 e3 fe                  	and	rbx, -2
     9a3: 48 f7 db                     	neg	rbx
     9a6: 31 c0                        	xor	eax, eax
     9a8: 0f 1f 84 00 00 00 00 00      	nop	dword ptr [rax + rax]
     9b0: 66 41 0f 10 4c c0 d0         	movupd	xmm1, xmmword ptr [r8 + 8*rax - 48]
     9b7: 66 41 0f 10 54 c0 e0         	movupd	xmm2, xmmword ptr [r8 + 8*rax - 32]
     9be: 66 0f 58 c8                  	addpd	xmm1, xmm0
     9c2: 66 0f 58 d0                  	addpd	xmm2, xmm0
     9c6: 66 0f 11 4c c2 d0            	movupd	xmmword ptr [rdx + 8*rax - 48], xmm1
     9cc: 66 0f 11 54 c2 e0            	movupd	xmmword ptr [rdx + 8*rax - 32], xmm2
     9d2: 66 41 0f 10 4c c0 f0         	movupd	xmm1, xmmword ptr [r8 + 8*rax - 16]
     9d9: 66 41 0f 10 14 c0            	movupd	xmm2, xmmword ptr [r8 + 8*rax]
     9df: 66 0f 58 c8                  	addpd	xmm1, xmm0
     9e3: 66 0f 58 d0                  	addpd	xmm2, xmm0
     9e7: 66 0f 11 4c c2 f0            	movupd	xmmword ptr [rdx + 8*rax - 16], xmm1
     9ed: 66 0f 11 14 c2               	movupd	xmmword ptr [rdx + 8*rax], xmm2
     9f2: 48 83 c0 08                  	add	rax, 8
     9f6: 48 83 c3 02                  	add	rbx, 2
     9fa: 75 b4                        	jne	0x9b0 <_run+0x9b0>
     9fc: 41 f6 c1 01                  	test	r9b, 1
     a00: 74 2b                        	je	0xa2d <_run+0xa2d>
     a02: 48 01 f8                     	add	rax, rdi
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:32
;     LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
     a05: f2 41 0f 12 07               	movddup	xmm0, qword ptr [r15]   ## xmm0 = mem[0,0]
     a0a: 66 41 0f 10 4c c5 00         	movupd	xmm1, xmmword ptr [r13 + 8*rax]
     a11: 66 41 0f 10 54 c5 10         	movupd	xmm2, xmmword ptr [r13 + 8*rax + 16]
     a18: 66 0f 58 c8                  	addpd	xmm1, xmm0
     a1c: 66 0f 58 d0                  	addpd	xmm2, xmm0
     a20: 66 41 0f 11 0c c4            	movupd	xmmword ptr [r12 + 8*rax], xmm1
     a26: 66 41 0f 11 54 c4 10         	movupd	xmmword ptr [r12 + 8*rax + 16], xmm2
     a2d: 48 89 cb                     	mov	rbx, rcx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:32
;     LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
     a30: 4c 39 de                     	cmp	rsi, r11
     a33: 4c 8b 45 c8                  	mov	r8, qword ptr [rbp - 56]
     a37: 4c 8b 4d 90                  	mov	r9, qword ptr [rbp - 112]
     a3b: 0f 84 c6 00 00 00            	je	0xb07 <_run+0xb07>
     a41: 4c 01 df                     	add	rdi, r11
     a44: eb 1a                        	jmp	0xa60 <_run+0xa60>
     a46: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     a50: 48 89 fb                     	mov	rbx, rdi
     a53: e9 af 00 00 00               	jmp	0xb07 <_run+0xb07>
     a58: 4c 8b 45 c8                  	mov	r8, qword ptr [rbp - 56]
     a5c: 0f 1f 40 00                  	nop	dword ptr [rax]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:32
;     LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
     a60: 44 89 d2                     	mov	edx, r10d
     a63: 29 fa                        	sub	edx, edi
     a65: 48 89 f8                     	mov	rax, rdi
     a68: 48 f7 d0                     	not	rax
     a6b: 4c 01 d0                     	add	rax, r10
     a6e: 48 83 e2 03                  	and	rdx, 3
     a72: 74 28                        	je	0xa9c <_run+0xa9c>
     a74: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     a7e: 66 90                        	nop
     a80: f2 41 0f 10 07               	movsd	xmm0, qword ptr [r15]   ## xmm0 = mem[0],zero
     a85: f2 41 0f 58 44 fd 00         	addsd	xmm0, qword ptr [r13 + 8*rdi]
     a8c: f2 41 0f 11 04 fc            	movsd	qword ptr [r12 + 8*rdi], xmm0
     a92: 48 83 c7 01                  	add	rdi, 1
     a96: 48 83 c2 ff                  	add	rdx, -1
     a9a: 75 e4                        	jne	0xa80 <_run+0xa80>
     a9c: 48 89 cb                     	mov	rbx, rcx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:32
;     LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
     a9f: 48 83 f8 03                  	cmp	rax, 3
     aa3: 72 62                        	jb	0xb07 <_run+0xb07>
     aa5: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     aaf: 90                           	nop
     ab0: f2 41 0f 10 07               	movsd	xmm0, qword ptr [r15]   ## xmm0 = mem[0],zero
     ab5: f2 41 0f 58 44 fd 00         	addsd	xmm0, qword ptr [r13 + 8*rdi]
     abc: f2 41 0f 11 04 fc            	movsd	qword ptr [r12 + 8*rdi], xmm0
     ac2: f2 41 0f 10 07               	movsd	xmm0, qword ptr [r15]   ## xmm0 = mem[0],zero
     ac7: f2 41 0f 58 44 fd 08         	addsd	xmm0, qword ptr [r13 + 8*rdi + 8]
     ace: f2 41 0f 11 44 fc 08         	movsd	qword ptr [r12 + 8*rdi + 8], xmm0
     ad5: f2 41 0f 10 07               	movsd	xmm0, qword ptr [r15]   ## xmm0 = mem[0],zero
     ada: f2 41 0f 58 44 fd 10         	addsd	xmm0, qword ptr [r13 + 8*rdi + 16]
     ae1: f2 41 0f 11 44 fc 10         	movsd	qword ptr [r12 + 8*rdi + 16], xmm0
     ae8: f2 41 0f 10 07               	movsd	xmm0, qword ptr [r15]   ## xmm0 = mem[0],zero
     aed: f2 41 0f 58 44 fd 18         	addsd	xmm0, qword ptr [r13 + 8*rdi + 24]
     af4: f2 41 0f 11 44 fc 18         	movsd	qword ptr [r12 + 8*rdi + 24], xmm0
     afb: 48 83 c7 04                  	add	rdi, 4
     aff: 48 39 f9                     	cmp	rcx, rdi
     b02: 75 ac                        	jne	0xab0 <_run+0xab0>
     b04: 48 89 cb                     	mov	rbx, rcx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:32
;     LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
     b07: 4c 39 c3                     	cmp	rbx, r8
     b0a: 0f 84 6c 01 00 00            	je	0xc7c <_run+0xc7c>
     b10: 48 89 df                     	mov	rdi, rbx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:32
;     LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
     b13: 4c 39 f3                     	cmp	rbx, r14
     b16: 0f 85 f4 fd ff ff            	jne	0x910 <_run+0x910>
     b1c: eb 11                        	jmp	0xb2f <_run+0xb2f>
     b1e: 31 c0                        	xor	eax, eax
     b20: 41 f6 c1 01                  	test	r9b, 1
     b24: 0f 85 d8 fe ff ff            	jne	0xa02 <_run+0xa02>
     b2a: e9 fe fe ff ff               	jmp	0xa2d <_run+0xa2d>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:32
;     LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
     b2f: e8 00 00 00 00               	call	0xb34 <_run+0xb34>
		0000000000000b30:  X86_64_RELOC_BRANCH	_R_CheckUserInterrupt
     b34: 4c 8b 45 c8                  	mov	r8, qword ptr [rbp - 56]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:32
;     LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
     b38: 48 b8 81 69 67 ff ff ff 0f 00	movabs	rax, 4503599617370497
     b42: 49 39 c6                     	cmp	r14, rax
     b45: 4d 8d b6 80 96 98 00         	lea	r14, [r14 + 10000000]
     b4c: 4d 0f 4d f0                  	cmovge	r14, r8
     b50: e9 90 fd ff ff               	jmp	0x8e5 <_run+0x8e5>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:34
;   } else if (len1 > len2) {
     b55: 4d 39 c1                     	cmp	r9, r8
     b58: 0f 8e 43 01 00 00            	jle	0xca1 <_run+0xca1>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:35
;     LOOP_W_INTERRUPT2(len1, len2, res[i] = (e1[i] + e2[j]););
     b5e: 48 8b 07                     	mov	rax, qword ptr [rdi]
     b61: f2 4c 0f 2c 50 08            	cvttsd2si	r10, qword ptr [rax + 8]
     b67: 45 31 f6                     	xor	r14d, r14d
     b6a: 45 31 db                     	xor	r11d, r11d
     b6d: 31 db                        	xor	ebx, ebx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:35
;     LOOP_W_INTERRUPT2(len1, len2, res[i] = (e1[i] + e2[j]););
     b6f: 4d 39 ca                     	cmp	r10, r9
     b72: 4c 89 c8                     	mov	rax, r9
     b75: 49 0f 4c c2                  	cmovl	rax, r10
     b79: 4c 89 d1                     	mov	rcx, r10
     b7c: 49 0f 4f c9                  	cmovg	rcx, r9
     b80: 48 39 cb                     	cmp	rbx, rcx
     b83: 0f 8d a4 00 00 00            	jge	0xc2d <_run+0xc2d>
     b89: 89 c2                        	mov	edx, eax
     b8b: 29 da                        	sub	edx, ebx
     b8d: 48 8d 7b 01                  	lea	rdi, [rbx + 1]
     b91: f6 c2 01                     	test	dl, 1
     b94: 75 0a                        	jne	0xba0 <_run+0xba0>
     b96: 48 89 de                     	mov	rsi, rbx
     b99: 4c 89 da                     	mov	rdx, r11
     b9c: eb 2b                        	jmp	0xbc9 <_run+0xbc9>
     b9e: 66 90                        	nop
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:35
;     LOOP_W_INTERRUPT2(len1, len2, res[i] = (e1[i] + e2[j]););
     ba0: 4d 39 c3                     	cmp	r11, r8
     ba3: ba 00 00 00 00               	mov	edx, 0
     ba8: 49 0f 45 d3                  	cmovne	rdx, r11
     bac: f2 41 0f 10 04 df            	movsd	xmm0, qword ptr [r15 + 8*rbx] ## xmm0 = mem[0],zero
     bb2: f2 41 0f 58 44 d5 00         	addsd	xmm0, qword ptr [r13 + 8*rdx]
     bb9: f2 41 0f 11 04 dc            	movsd	qword ptr [r12 + 8*rbx], xmm0
     bbf: 48 83 c2 01                  	add	rdx, 1
     bc3: 49 89 d3                     	mov	r11, rdx
     bc6: 48 89 fe                     	mov	rsi, rdi
     bc9: 48 89 cb                     	mov	rbx, rcx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:35
;     LOOP_W_INTERRUPT2(len1, len2, res[i] = (e1[i] + e2[j]););
     bcc: 48 39 f8                     	cmp	rax, rdi
     bcf: 74 5c                        	je	0xc2d <_run+0xc2d>
     bd1: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     bdb: 0f 1f 44 00 00               	nop	dword ptr [rax + rax]
     be0: 4c 39 c2                     	cmp	rdx, r8
     be3: 49 0f 44 d6                  	cmove	rdx, r14
     be7: f2 41 0f 10 04 f7            	movsd	xmm0, qword ptr [r15 + 8*rsi] ## xmm0 = mem[0],zero
     bed: f2 41 0f 58 44 d5 00         	addsd	xmm0, qword ptr [r13 + 8*rdx]
     bf4: 48 83 c2 01                  	add	rdx, 1
     bf8: 4c 39 c2                     	cmp	rdx, r8
     bfb: 49 0f 44 d6                  	cmove	rdx, r14
     bff: f2 41 0f 11 04 f4            	movsd	qword ptr [r12 + 8*rsi], xmm0
     c05: f2 41 0f 10 44 f7 08         	movsd	xmm0, qword ptr [r15 + 8*rsi + 8] ## xmm0 = mem[0],zero
     c0c: f2 41 0f 58 44 d5 00         	addsd	xmm0, qword ptr [r13 + 8*rdx]
     c13: f2 41 0f 11 44 f4 08         	movsd	qword ptr [r12 + 8*rsi + 8], xmm0
     c1a: 48 83 c6 02                  	add	rsi, 2
     c1e: 48 83 c2 01                  	add	rdx, 1
     c22: 48 39 f0                     	cmp	rax, rsi
     c25: 75 b9                        	jne	0xbe0 <_run+0xbe0>
     c27: 49 89 d3                     	mov	r11, rdx
     c2a: 48 89 cb                     	mov	rbx, rcx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:35
;     LOOP_W_INTERRUPT2(len1, len2, res[i] = (e1[i] + e2[j]););
     c2d: 4c 39 cb                     	cmp	rbx, r9
     c30: 0f 84 7b 01 00 00            	je	0xdb1 <_run+0xdb1>
     c36: 4c 39 d3                     	cmp	rbx, r10
     c39: 0f 85 41 ff ff ff            	jne	0xb80 <_run+0xb80>
     c3f: 4c 89 55 90                  	mov	qword ptr [rbp - 112], r10
     c43: 4c 89 5d b8                  	mov	qword ptr [rbp - 72], r11
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:35
;     LOOP_W_INTERRUPT2(len1, len2, res[i] = (e1[i] + e2[j]););
     c47: e8 00 00 00 00               	call	0xc4c <_run+0xc4c>
		0000000000000c48:  X86_64_RELOC_BRANCH	_R_CheckUserInterrupt
     c4c: 4c 8b 5d b8                  	mov	r11, qword ptr [rbp - 72]
     c50: 4c 8b 45 c8                  	mov	r8, qword ptr [rbp - 56]
     c54: 4c 8b 4d b0                  	mov	r9, qword ptr [rbp - 80]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:35
;     LOOP_W_INTERRUPT2(len1, len2, res[i] = (e1[i] + e2[j]););
     c58: 48 b8 81 69 67 ff ff ff 0f 00	movabs	rax, 4503599617370497
     c62: 48 8b 4d 90                  	mov	rcx, qword ptr [rbp - 112]
     c66: 48 39 c1                     	cmp	rcx, rax
     c69: 48 8d 81 80 96 98 00         	lea	rax, [rcx + 10000000]
     c70: 49 0f 4d c1                  	cmovge	rax, r9
     c74: 49 89 c2                     	mov	r10, rax
     c77: e9 f3 fe ff ff               	jmp	0xb6f <_run+0xb6f>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:32
;     LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
     c7c: 0f 57 c0                     	xorps	xmm0, xmm0
     c7f: f2 49 0f 2a c6               	cvtsi2sd	xmm0, r14
     c84: 0f 57 c9                     	xorps	xmm1, xmm1
     c87: f2 49 0f 2a c8               	cvtsi2sd	xmm1, r8
     c8c: f2 0f 5c c1                  	subsd	xmm0, xmm1
     c90: 48 8b 7d 88                  	mov	rdi, qword ptr [rbp - 120]
     c94: 48 8b 07                     	mov	rax, qword ptr [rdi]
     c97: f2 0f 11 40 08               	movsd	qword ptr [rax + 8], xmm0
     c9c: e9 4f f9 ff ff               	jmp	0x5f0 <_run+0x5f0>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:38
;   } else if (len2 > len1) {
     ca1: 4d 39 c8                     	cmp	r8, r9
     ca4: 0f 8e 56 f9 ff ff            	jle	0x600 <_run+0x600>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:39
;     LOOP_W_INTERRUPT2(len2, len1, res[i] = (e1[j] + e2[i]););
     caa: 48 8b 07                     	mov	rax, qword ptr [rdi]
     cad: f2 4c 0f 2c 58 08            	cvttsd2si	r11, qword ptr [rax + 8]
     cb3: 31 c9                        	xor	ecx, ecx
     cb5: 45 31 f6                     	xor	r14d, r14d
     cb8: 31 db                        	xor	ebx, ebx
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:39
;     LOOP_W_INTERRUPT2(len2, len1, res[i] = (e1[j] + e2[i]););
     cba: 4d 39 c3                     	cmp	r11, r8
     cbd: 4c 89 c0                     	mov	rax, r8
     cc0: 49 0f 4c c3                  	cmovl	rax, r11
     cc4: 4d 89 da                     	mov	r10, r11
     cc7: 4d 0f 4f d0                  	cmovg	r10, r8
     ccb: 0f 1f 44 00 00               	nop	dword ptr [rax + rax]
     cd0: 4c 39 d3                     	cmp	rbx, r10
     cd3: 0f 8d 93 00 00 00            	jge	0xd6c <_run+0xd6c>
     cd9: 89 c2                        	mov	edx, eax
     cdb: 29 da                        	sub	edx, ebx
     cdd: 48 8d 7b 01                  	lea	rdi, [rbx + 1]
     ce1: f6 c2 01                     	test	dl, 1
     ce4: 75 08                        	jne	0xcee <_run+0xcee>
     ce6: 48 89 de                     	mov	rsi, rbx
     ce9: 4c 89 f2                     	mov	rdx, r14
     cec: eb 29                        	jmp	0xd17 <_run+0xd17>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:39
;     LOOP_W_INTERRUPT2(len2, len1, res[i] = (e1[j] + e2[i]););
     cee: 4d 39 ce                     	cmp	r14, r9
     cf1: ba 00 00 00 00               	mov	edx, 0
     cf6: 49 0f 45 d6                  	cmovne	rdx, r14
     cfa: f2 41 0f 10 04 d7            	movsd	xmm0, qword ptr [r15 + 8*rdx] ## xmm0 = mem[0],zero
     d00: f2 41 0f 58 44 dd 00         	addsd	xmm0, qword ptr [r13 + 8*rbx]
     d07: f2 41 0f 11 04 dc            	movsd	qword ptr [r12 + 8*rbx], xmm0
     d0d: 48 83 c2 01                  	add	rdx, 1
     d11: 49 89 d6                     	mov	r14, rdx
     d14: 48 89 fe                     	mov	rsi, rdi
     d17: 4c 89 d3                     	mov	rbx, r10
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:39
;     LOOP_W_INTERRUPT2(len2, len1, res[i] = (e1[j] + e2[i]););
     d1a: 48 39 f8                     	cmp	rax, rdi
     d1d: 74 4d                        	je	0xd6c <_run+0xd6c>
     d1f: 90                           	nop
     d20: 4c 39 ca                     	cmp	rdx, r9
     d23: 48 0f 44 d1                  	cmove	rdx, rcx
     d27: f2 41 0f 10 04 d7            	movsd	xmm0, qword ptr [r15 + 8*rdx] ## xmm0 = mem[0],zero
     d2d: f2 41 0f 58 44 f5 00         	addsd	xmm0, qword ptr [r13 + 8*rsi]
     d34: 48 83 c2 01                  	add	rdx, 1
     d38: 4c 39 ca                     	cmp	rdx, r9
     d3b: 48 0f 44 d1                  	cmove	rdx, rcx
     d3f: f2 41 0f 11 04 f4            	movsd	qword ptr [r12 + 8*rsi], xmm0
     d45: f2 41 0f 10 04 d7            	movsd	xmm0, qword ptr [r15 + 8*rdx] ## xmm0 = mem[0],zero
     d4b: f2 41 0f 58 44 f5 08         	addsd	xmm0, qword ptr [r13 + 8*rsi + 8]
     d52: f2 41 0f 11 44 f4 08         	movsd	qword ptr [r12 + 8*rsi + 8], xmm0
     d59: 48 83 c6 02                  	add	rsi, 2
     d5d: 48 83 c2 01                  	add	rdx, 1
     d61: 48 39 f0                     	cmp	rax, rsi
     d64: 75 ba                        	jne	0xd20 <_run+0xd20>
     d66: 49 89 d6                     	mov	r14, rdx
     d69: 4c 89 d3                     	mov	rbx, r10
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:39
;     LOOP_W_INTERRUPT2(len2, len1, res[i] = (e1[j] + e2[i]););
     d6c: 4c 39 c3                     	cmp	rbx, r8
     d6f: 74 78                        	je	0xde9 <_run+0xde9>
     d71: 4c 39 db                     	cmp	rbx, r11
     d74: 0f 85 56 ff ff ff            	jne	0xcd0 <_run+0xcd0>
     d7a: 4c 89 5d b8                  	mov	qword ptr [rbp - 72], r11
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:39
;     LOOP_W_INTERRUPT2(len2, len1, res[i] = (e1[j] + e2[i]););
     d7e: e8 00 00 00 00               	call	0xd83 <_run+0xd83>
		0000000000000d7f:  X86_64_RELOC_BRANCH	_R_CheckUserInterrupt
     d83: 31 c9                        	xor	ecx, ecx
     d85: 4c 8b 45 c8                  	mov	r8, qword ptr [rbp - 56]
     d89: 4c 8b 4d b0                  	mov	r9, qword ptr [rbp - 80]
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:39
;     LOOP_W_INTERRUPT2(len2, len1, res[i] = (e1[j] + e2[i]););
     d8d: 48 b8 81 69 67 ff ff ff 0f 00	movabs	rax, 4503599617370497
     d97: 48 8b 55 b8                  	mov	rdx, qword ptr [rbp - 72]
     d9b: 48 39 c2                     	cmp	rdx, rax
     d9e: 48 8d 82 80 96 98 00         	lea	rax, [rdx + 10000000]
     da5: 49 0f 4d c0                  	cmovge	rax, r8
     da9: 49 89 c3                     	mov	r11, rax
     dac: e9 09 ff ff ff               	jmp	0xcba <_run+0xcba>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:35
;     LOOP_W_INTERRUPT2(len1, len2, res[i] = (e1[i] + e2[j]););
     db1: 0f 57 c0                     	xorps	xmm0, xmm0
     db4: f2 49 0f 2a c2               	cvtsi2sd	xmm0, r10
     db9: f2 49 0f 2a c9               	cvtsi2sd	xmm1, r9
     dbe: f2 0f 5c c1                  	subsd	xmm0, xmm1
     dc2: 48 8b 7d 88                  	mov	rdi, qword ptr [rbp - 120]
     dc6: 48 8b 07                     	mov	rax, qword ptr [rdi]
     dc9: f2 0f 11 40 08               	movsd	qword ptr [rax + 8], xmm0
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:36
;     if(j != len2) data[0][0] = 1.;   // bad recycle
     dce: 4d 39 c3                     	cmp	r11, r8
     dd1: 0f 84 16 f8 ff ff            	je	0x5ed <_run+0x5ed>
     dd7: 48 b9 00 00 00 00 00 00 f0 3f	movabs	rcx, 4607182418800017408
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:36
;     if(j != len2) data[0][0] = 1.;   // bad recycle
     de1: 48 89 08                     	mov	qword ptr [rax], rcx
     de4: e9 04 f8 ff ff               	jmp	0x5ed <_run+0x5ed>
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:39
;     LOOP_W_INTERRUPT2(len2, len1, res[i] = (e1[j] + e2[i]););
     de9: 0f 57 c0                     	xorps	xmm0, xmm0
     dec: f2 49 0f 2a c3               	cvtsi2sd	xmm0, r11
     df1: f2 49 0f 2a c8               	cvtsi2sd	xmm1, r8
     df6: f2 0f 5c c1                  	subsd	xmm0, xmm1
     dfa: 48 8b 7d 88                  	mov	rdi, qword ptr [rbp - 120]
     dfe: 48 8b 07                     	mov	rax, qword ptr [rdi]
     e01: f2 0f 11 40 08               	movsd	qword ptr [rax + 8], xmm0
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:40
;     if(j != len1) data[0][0] = 1.;   // bad recycle
     e06: 4d 39 ce                     	cmp	r14, r9
     e09: 0f 84 e1 f7 ff ff            	je	0x5f0 <_run+0x5f0>
     e0f: 48 b9 00 00 00 00 00 00 f0 3f	movabs	rcx, 4607182418800017408
; /Users/milberg/repos/r2c/tmp/new/r2c-xkkxvb3w71.c:40
;     if(j != len1) data[0][0] = 1.;   // bad recycle
     e19: 48 89 08                     	mov	qword ptr [rax], rcx
     e1c: e9 cf f7 ff ff               	jmp	0x5f0 <_run+0x5f0>
