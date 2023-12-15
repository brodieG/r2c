
r2c-56xmrcc6fe.o:	file format mach-o 64-bit x86-64

SYMBOL TABLE:
0000000000000000 g     F __TEXT,__text _run

Disassembly of section __TEXT,__text:

0000000000000000 <_run>:
; run():
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:71
; ) {
       0: 55                           	push	rbp
       1: 48 89 e5                     	mov	rbp, rsp
       4: 41 57                        	push	r15
       6: 41 56                        	push	r14
       8: 41 55                        	push	r13
       a: 41 54                        	push	r12
       c: 53                           	push	rbx
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:76
;   add(data, lens, *di++);
       d: 48 8b 1a                     	mov	rbx, qword ptr [rdx]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:6
;   int di0 = di[0];
      10: 48 63 0b                     	movsxd	rcx, dword ptr [rbx]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:8
;   int dires = di[2];
      13: 4c 63 4b 08                  	movsxd	r9, dword ptr [rbx + 8]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:11
;   R_xlen_t len1 = lens[di0];
      17: 4c 8b 1c ce                  	mov	r11, qword ptr [rsi + 8*rcx]
      1b: 31 c0                        	xor	eax, eax
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:15
;   if(len1 == 0 || len2 == 0) { // empty recycle is zero
      1d: 4d 85 db                     	test	r11, r11
      20: 0f 84 c2 06 00 00            	je	0x6e8 <_run+0x6e8>
      26: 48 63 5b 04                  	movsxd	rbx, dword ptr [rbx + 4]
      2a: 4c 8b 34 de                  	mov	r14, qword ptr [rsi + 8*rbx]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:15
;   if(len1 == 0 || len2 == 0) { // empty recycle is zero
      2e: 4d 85 f6                     	test	r14, r14
      31: 0f 84 b1 06 00 00            	je	0x6e8 <_run+0x6e8>
      37: 4c 8b 3c cf                  	mov	r15, qword ptr [rdi + 8*rcx]
      3b: 4c 8b 14 df                  	mov	r10, qword ptr [rdi + 8*rbx]
      3f: 4e 8b 24 cf                  	mov	r12, qword ptr [rdi + 8*r9]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:24
;   if(len1 == len2) {
      43: 4d 39 f3                     	cmp	r11, r14
      46: 0f 85 c3 00 00 00            	jne	0x10f <_run+0x10f>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:25
;     for(i = 0; i < len1; ++i) res[i] = (e1[i] + e2[i]);
      4c: 4d 85 db                     	test	r11, r11
      4f: 0f 8e 90 06 00 00            	jle	0x6e5 <_run+0x6e5>
      55: 49 83 fb 04                  	cmp	r11, 4
      59: 0f 83 29 02 00 00            	jae	0x288 <_run+0x288>
      5f: 31 db                        	xor	ebx, ebx
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:25
;     for(i = 0; i < len1; ++i) res[i] = (e1[i] + e2[i]);
      61: 48 89 d8                     	mov	rax, rbx
      64: 48 f7 d0                     	not	rax
      67: 4c 01 d8                     	add	rax, r11
      6a: 4c 89 d9                     	mov	rcx, r11
      6d: 48 83 e1 03                  	and	rcx, 3
      71: 74 29                        	je	0x9c <_run+0x9c>
      73: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
      7d: 0f 1f 00                     	nop	dword ptr [rax]
      80: f2 41 0f 10 04 df            	movsd	xmm0, qword ptr [r15 + 8*rbx] ## xmm0 = mem[0],zero
      86: f2 41 0f 58 04 da            	addsd	xmm0, qword ptr [r10 + 8*rbx]
      8c: f2 41 0f 11 04 dc            	movsd	qword ptr [r12 + 8*rbx], xmm0
      92: 48 83 c3 01                  	add	rbx, 1
      96: 48 83 c1 ff                  	add	rcx, -1
      9a: 75 e4                        	jne	0x80 <_run+0x80>
      9c: 48 83 f8 03                  	cmp	rax, 3
      a0: 0f 82 3f 06 00 00            	jb	0x6e5 <_run+0x6e5>
      a6: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
      b0: f2 41 0f 10 04 df            	movsd	xmm0, qword ptr [r15 + 8*rbx] ## xmm0 = mem[0],zero
      b6: f2 41 0f 58 04 da            	addsd	xmm0, qword ptr [r10 + 8*rbx]
      bc: f2 41 0f 11 04 dc            	movsd	qword ptr [r12 + 8*rbx], xmm0
      c2: f2 41 0f 10 44 df 08         	movsd	xmm0, qword ptr [r15 + 8*rbx + 8] ## xmm0 = mem[0],zero
      c9: f2 41 0f 58 44 da 08         	addsd	xmm0, qword ptr [r10 + 8*rbx + 8]
      d0: f2 41 0f 11 44 dc 08         	movsd	qword ptr [r12 + 8*rbx + 8], xmm0
      d7: f2 41 0f 10 44 df 10         	movsd	xmm0, qword ptr [r15 + 8*rbx + 16] ## xmm0 = mem[0],zero
      de: f2 41 0f 58 44 da 10         	addsd	xmm0, qword ptr [r10 + 8*rbx + 16]
      e5: f2 41 0f 11 44 dc 10         	movsd	qword ptr [r12 + 8*rbx + 16], xmm0
      ec: f2 41 0f 10 44 df 18         	movsd	xmm0, qword ptr [r15 + 8*rbx + 24] ## xmm0 = mem[0],zero
      f3: f2 41 0f 58 44 da 18         	addsd	xmm0, qword ptr [r10 + 8*rbx + 24]
      fa: f2 41 0f 11 44 dc 18         	movsd	qword ptr [r12 + 8*rbx + 24], xmm0
     101: 48 83 c3 04                  	add	rbx, 4
     105: 49 39 db                     	cmp	r11, rbx
     108: 75 a6                        	jne	0xb0 <_run+0xb0>
     10a: e9 d6 05 00 00               	jmp	0x6e5 <_run+0x6e5>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:27
;   } else if (len2 == 1) {
     10f: 49 83 fe 01                  	cmp	r14, 1
     113: 0f 85 af 00 00 00            	jne	0x1c8 <_run+0x1c8>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:28
;     for(i = 0; i < len1; ++i) res[i] = (e1[i] + *e2);
     119: 4d 85 db                     	test	r11, r11
     11c: 0f 8e c3 05 00 00            	jle	0x6e5 <_run+0x6e5>
     122: 49 83 fb 04                  	cmp	r11, 4
     126: 0f 83 59 02 00 00            	jae	0x385 <_run+0x385>
     12c: 31 db                        	xor	ebx, ebx
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:28
;     for(i = 0; i < len1; ++i) res[i] = (e1[i] + *e2);
     12e: 48 89 d8                     	mov	rax, rbx
     131: 48 f7 d0                     	not	rax
     134: 4c 01 d8                     	add	rax, r11
     137: 4c 89 d9                     	mov	rcx, r11
     13a: 48 83 e1 03                  	and	rcx, 3
     13e: 74 1b                        	je	0x15b <_run+0x15b>
     140: f2 41 0f 10 04 df            	movsd	xmm0, qword ptr [r15 + 8*rbx] ## xmm0 = mem[0],zero
     146: f2 41 0f 58 02               	addsd	xmm0, qword ptr [r10]
     14b: f2 41 0f 11 04 dc            	movsd	qword ptr [r12 + 8*rbx], xmm0
     151: 48 83 c3 01                  	add	rbx, 1
     155: 48 83 c1 ff                  	add	rcx, -1
     159: 75 e5                        	jne	0x140 <_run+0x140>
     15b: 48 83 f8 03                  	cmp	rax, 3
     15f: 0f 82 80 05 00 00            	jb	0x6e5 <_run+0x6e5>
     165: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     16f: 90                           	nop
     170: f2 41 0f 10 04 df            	movsd	xmm0, qword ptr [r15 + 8*rbx] ## xmm0 = mem[0],zero
     176: f2 41 0f 58 02               	addsd	xmm0, qword ptr [r10]
     17b: f2 41 0f 11 04 dc            	movsd	qword ptr [r12 + 8*rbx], xmm0
     181: f2 41 0f 10 44 df 08         	movsd	xmm0, qword ptr [r15 + 8*rbx + 8] ## xmm0 = mem[0],zero
     188: f2 41 0f 58 02               	addsd	xmm0, qword ptr [r10]
     18d: f2 41 0f 11 44 dc 08         	movsd	qword ptr [r12 + 8*rbx + 8], xmm0
     194: f2 41 0f 10 44 df 10         	movsd	xmm0, qword ptr [r15 + 8*rbx + 16] ## xmm0 = mem[0],zero
     19b: f2 41 0f 58 02               	addsd	xmm0, qword ptr [r10]
     1a0: f2 41 0f 11 44 dc 10         	movsd	qword ptr [r12 + 8*rbx + 16], xmm0
     1a7: f2 41 0f 10 44 df 18         	movsd	xmm0, qword ptr [r15 + 8*rbx + 24] ## xmm0 = mem[0],zero
     1ae: f2 41 0f 58 02               	addsd	xmm0, qword ptr [r10]
     1b3: f2 41 0f 11 44 dc 18         	movsd	qword ptr [r12 + 8*rbx + 24], xmm0
     1ba: 48 83 c3 04                  	add	rbx, 4
     1be: 49 39 db                     	cmp	r11, rbx
     1c1: 75 ad                        	jne	0x170 <_run+0x170>
     1c3: e9 1d 05 00 00               	jmp	0x6e5 <_run+0x6e5>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:30
;   } else if (len1 == 1) {
     1c8: 49 83 fb 01                  	cmp	r11, 1
     1cc: 0f 85 8e 01 00 00            	jne	0x360 <_run+0x360>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:31
;     for(i = 0; i < len2; ++i) res[i] = (*e1 + e2[i]);
     1d2: 4d 85 f6                     	test	r14, r14
     1d5: 0f 8e c2 06 00 00            	jle	0x89d <_run+0x89d>
     1db: 49 83 fe 04                  	cmp	r14, 4
     1df: 0f 83 85 02 00 00            	jae	0x46a <_run+0x46a>
     1e5: 31 db                        	xor	ebx, ebx
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:31
;     for(i = 0; i < len2; ++i) res[i] = (*e1 + e2[i]);
     1e7: 48 89 d8                     	mov	rax, rbx
     1ea: 48 f7 d0                     	not	rax
     1ed: 4c 01 f0                     	add	rax, r14
     1f0: 4c 89 f1                     	mov	rcx, r14
     1f3: 48 83 e1 03                  	and	rcx, 3
     1f7: 74 22                        	je	0x21b <_run+0x21b>
     1f9: 0f 1f 80 00 00 00 00         	nop	dword ptr [rax]
     200: f2 41 0f 10 07               	movsd	xmm0, qword ptr [r15]   ## xmm0 = mem[0],zero
     205: f2 41 0f 58 04 da            	addsd	xmm0, qword ptr [r10 + 8*rbx]
     20b: f2 41 0f 11 04 dc            	movsd	qword ptr [r12 + 8*rbx], xmm0
     211: 48 83 c3 01                  	add	rbx, 1
     215: 48 83 c1 ff                  	add	rcx, -1
     219: 75 e5                        	jne	0x200 <_run+0x200>
     21b: 48 83 f8 03                  	cmp	rax, 3
     21f: 0f 82 78 06 00 00            	jb	0x89d <_run+0x89d>
     225: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     22f: 90                           	nop
     230: f2 41 0f 10 07               	movsd	xmm0, qword ptr [r15]   ## xmm0 = mem[0],zero
     235: f2 41 0f 58 04 da            	addsd	xmm0, qword ptr [r10 + 8*rbx]
     23b: f2 41 0f 11 04 dc            	movsd	qword ptr [r12 + 8*rbx], xmm0
     241: f2 41 0f 10 07               	movsd	xmm0, qword ptr [r15]   ## xmm0 = mem[0],zero
     246: f2 41 0f 58 44 da 08         	addsd	xmm0, qword ptr [r10 + 8*rbx + 8]
     24d: f2 41 0f 11 44 dc 08         	movsd	qword ptr [r12 + 8*rbx + 8], xmm0
     254: f2 41 0f 10 07               	movsd	xmm0, qword ptr [r15]   ## xmm0 = mem[0],zero
     259: f2 41 0f 58 44 da 10         	addsd	xmm0, qword ptr [r10 + 8*rbx + 16]
     260: f2 41 0f 11 44 dc 10         	movsd	qword ptr [r12 + 8*rbx + 16], xmm0
     267: f2 41 0f 10 07               	movsd	xmm0, qword ptr [r15]   ## xmm0 = mem[0],zero
     26c: f2 41 0f 58 44 da 18         	addsd	xmm0, qword ptr [r10 + 8*rbx + 24]
     273: f2 41 0f 11 44 dc 18         	movsd	qword ptr [r12 + 8*rbx + 24], xmm0
     27a: 48 83 c3 04                  	add	rbx, 4
     27e: 49 39 de                     	cmp	r14, rbx
     281: 75 ad                        	jne	0x230 <_run+0x230>
     283: e9 15 06 00 00               	jmp	0x89d <_run+0x89d>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:25
;     for(i = 0; i < len1; ++i) res[i] = (e1[i] + e2[i]);
     288: 4b 8d 0c dc                  	lea	rcx, [r12 + 8*r11]
     28c: 4b 8d 04 df                  	lea	rax, [r15 + 8*r11]
     290: 4b 8d 1c da                  	lea	rbx, [r10 + 8*r11]
     294: 49 39 c4                     	cmp	r12, rax
     297: 41 0f 92 c6                  	setb	r14b
     29b: 49 39 cf                     	cmp	r15, rcx
     29e: 41 0f 92 c5                  	setb	r13b
     2a2: 49 39 dc                     	cmp	r12, rbx
     2a5: 0f 92 c0                     	setb	al
     2a8: 49 39 ca                     	cmp	r10, rcx
     2ab: 0f 92 c1                     	setb	cl
     2ae: 31 db                        	xor	ebx, ebx
     2b0: 45 84 ee                     	test	r14b, r13b
     2b3: 0f 85 a8 fd ff ff            	jne	0x61 <_run+0x61>
     2b9: 20 c8                        	and	al, cl
     2bb: 0f 85 a0 fd ff ff            	jne	0x61 <_run+0x61>
     2c1: 4c 89 db                     	mov	rbx, r11
     2c4: 48 83 e3 fc                  	and	rbx, -4
     2c8: 48 8d 43 fc                  	lea	rax, [rbx - 4]
     2cc: 49 89 c6                     	mov	r14, rax
     2cf: 49 c1 ee 02                  	shr	r14, 2
     2d3: 49 83 c6 01                  	add	r14, 1
     2d7: 48 85 c0                     	test	rax, rax
     2da: 0f 84 48 02 00 00            	je	0x528 <_run+0x528>
     2e0: 4c 89 f1                     	mov	rcx, r14
     2e3: 48 83 e1 fe                  	and	rcx, -2
     2e7: 48 f7 d9                     	neg	rcx
     2ea: 31 c0                        	xor	eax, eax
     2ec: 0f 1f 40 00                  	nop	dword ptr [rax]
     2f0: 66 41 0f 10 04 c7            	movupd	xmm0, xmmword ptr [r15 + 8*rax]
     2f6: 66 41 0f 10 4c c7 10         	movupd	xmm1, xmmword ptr [r15 + 8*rax + 16]
     2fd: 66 41 0f 10 14 c2            	movupd	xmm2, xmmword ptr [r10 + 8*rax]
     303: 66 0f 58 d0                  	addpd	xmm2, xmm0
     307: 66 41 0f 10 44 c2 10         	movupd	xmm0, xmmword ptr [r10 + 8*rax + 16]
     30e: 66 0f 58 c1                  	addpd	xmm0, xmm1
     312: 66 41 0f 11 14 c4            	movupd	xmmword ptr [r12 + 8*rax], xmm2
     318: 66 41 0f 11 44 c4 10         	movupd	xmmword ptr [r12 + 8*rax + 16], xmm0
     31f: 66 41 0f 10 44 c7 20         	movupd	xmm0, xmmword ptr [r15 + 8*rax + 32]
     326: 66 41 0f 10 4c c7 30         	movupd	xmm1, xmmword ptr [r15 + 8*rax + 48]
     32d: 66 41 0f 10 54 c2 20         	movupd	xmm2, xmmword ptr [r10 + 8*rax + 32]
     334: 66 0f 58 d0                  	addpd	xmm2, xmm0
     338: 66 41 0f 10 44 c2 30         	movupd	xmm0, xmmword ptr [r10 + 8*rax + 48]
     33f: 66 0f 58 c1                  	addpd	xmm0, xmm1
     343: 66 41 0f 11 54 c4 20         	movupd	xmmword ptr [r12 + 8*rax + 32], xmm2
     34a: 66 41 0f 11 44 c4 30         	movupd	xmmword ptr [r12 + 8*rax + 48], xmm0
     351: 48 83 c0 08                  	add	rax, 8
     355: 48 83 c1 02                  	add	rcx, 2
     359: 75 95                        	jne	0x2f0 <_run+0x2f0>
     35b: e9 ca 01 00 00               	jmp	0x52a <_run+0x52a>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:33
;   } else if (len1 > len2) {
     360: 4d 39 f3                     	cmp	r11, r14
     363: 0f 8e dc 00 00 00            	jle	0x445 <_run+0x445>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:34
;     for(i = 0, j = 0; i < len1; ++i, ++j) {
     369: 4d 85 db                     	test	r11, r11
     36c: 0f 8e fb 01 00 00            	jle	0x56d <_run+0x56d>
     372: 49 83 fb 01                  	cmp	r11, 1
     376: 0f 85 f8 01 00 00            	jne	0x574 <_run+0x574>
     37c: 31 db                        	xor	ebx, ebx
     37e: 31 c0                        	xor	eax, eax
     380: e9 50 02 00 00               	jmp	0x5d5 <_run+0x5d5>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:28
;     for(i = 0; i < len1; ++i) res[i] = (e1[i] + *e2);
     385: 4b 8d 0c dc                  	lea	rcx, [r12 + 8*r11]
     389: 4b 8d 04 df                  	lea	rax, [r15 + 8*r11]
     38d: 49 8d 5a 08                  	lea	rbx, [r10 + 8]
     391: 49 39 c4                     	cmp	r12, rax
     394: 41 0f 92 c6                  	setb	r14b
     398: 49 39 cf                     	cmp	r15, rcx
     39b: 41 0f 92 c5                  	setb	r13b
     39f: 49 39 dc                     	cmp	r12, rbx
     3a2: 0f 92 c0                     	setb	al
     3a5: 49 39 ca                     	cmp	r10, rcx
     3a8: 0f 92 c1                     	setb	cl
     3ab: 31 db                        	xor	ebx, ebx
     3ad: 45 84 ee                     	test	r14b, r13b
     3b0: 0f 85 78 fd ff ff            	jne	0x12e <_run+0x12e>
     3b6: 20 c8                        	and	al, cl
     3b8: 0f 85 70 fd ff ff            	jne	0x12e <_run+0x12e>
     3be: 4c 89 db                     	mov	rbx, r11
     3c1: 48 83 e3 fc                  	and	rbx, -4
     3c5: 48 8d 43 fc                  	lea	rax, [rbx - 4]
     3c9: 49 89 c6                     	mov	r14, rax
     3cc: 49 c1 ee 02                  	shr	r14, 2
     3d0: 49 83 c6 01                  	add	r14, 1
     3d4: 48 85 c0                     	test	rax, rax
     3d7: 0f 84 31 02 00 00            	je	0x60e <_run+0x60e>
     3dd: f2 41 0f 12 02               	movddup	xmm0, qword ptr [r10]   ## xmm0 = mem[0,0]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:28
;     for(i = 0; i < len1; ++i) res[i] = (e1[i] + *e2);
     3e2: 4c 89 f1                     	mov	rcx, r14
     3e5: 48 83 e1 fe                  	and	rcx, -2
     3e9: 48 f7 d9                     	neg	rcx
     3ec: 31 c0                        	xor	eax, eax
     3ee: 66 90                        	nop
     3f0: 66 41 0f 10 0c c7            	movupd	xmm1, xmmword ptr [r15 + 8*rax]
     3f6: 66 41 0f 10 54 c7 10         	movupd	xmm2, xmmword ptr [r15 + 8*rax + 16]
     3fd: 66 0f 58 c8                  	addpd	xmm1, xmm0
     401: 66 0f 58 d0                  	addpd	xmm2, xmm0
     405: 66 41 0f 11 0c c4            	movupd	xmmword ptr [r12 + 8*rax], xmm1
     40b: 66 41 0f 11 54 c4 10         	movupd	xmmword ptr [r12 + 8*rax + 16], xmm2
     412: 66 41 0f 10 4c c7 20         	movupd	xmm1, xmmword ptr [r15 + 8*rax + 32]
     419: 66 41 0f 10 54 c7 30         	movupd	xmm2, xmmword ptr [r15 + 8*rax + 48]
     420: 66 0f 58 c8                  	addpd	xmm1, xmm0
     424: 66 0f 58 d0                  	addpd	xmm2, xmm0
     428: 66 41 0f 11 4c c4 20         	movupd	xmmword ptr [r12 + 8*rax + 32], xmm1
     42f: 66 41 0f 11 54 c4 30         	movupd	xmmword ptr [r12 + 8*rax + 48], xmm2
     436: 48 83 c0 08                  	add	rax, 8
     43a: 48 83 c1 02                  	add	rcx, 2
     43e: 75 b0                        	jne	0x3f0 <_run+0x3f0>
     440: e9 cb 01 00 00               	jmp	0x610 <_run+0x610>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:40
;   } else if (len2 > len1) {
     445: 4d 39 de                     	cmp	r14, r11
     448: 0f 8e 9e 02 00 00            	jle	0x6ec <_run+0x6ec>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:41
;     for(i = 0, j = 0; i < len2; ++i, ++j) {
     44e: 4d 85 f6                     	test	r14, r14
     451: 0f 8e f4 01 00 00            	jle	0x64b <_run+0x64b>
     457: 49 83 fe 01                  	cmp	r14, 1
     45b: 0f 85 ee 01 00 00            	jne	0x64f <_run+0x64f>
     461: 31 db                        	xor	ebx, ebx
     463: 31 c0                        	xor	eax, eax
     465: e9 38 02 00 00               	jmp	0x6a2 <_run+0x6a2>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:31
;     for(i = 0; i < len2; ++i) res[i] = (*e1 + e2[i]);
     46a: 4b 8d 0c f4                  	lea	rcx, [r12 + 8*r14]
     46e: 49 8d 47 08                  	lea	rax, [r15 + 8]
     472: 4b 8d 1c f2                  	lea	rbx, [r10 + 8*r14]
     476: 49 39 c4                     	cmp	r12, rax
     479: 41 0f 92 c3                  	setb	r11b
     47d: 49 39 cf                     	cmp	r15, rcx
     480: 41 0f 92 c5                  	setb	r13b
     484: 49 39 dc                     	cmp	r12, rbx
     487: 0f 92 c0                     	setb	al
     48a: 49 39 ca                     	cmp	r10, rcx
     48d: 0f 92 c1                     	setb	cl
     490: 31 db                        	xor	ebx, ebx
     492: 45 84 eb                     	test	r11b, r13b
     495: 0f 85 4c fd ff ff            	jne	0x1e7 <_run+0x1e7>
     49b: 20 c8                        	and	al, cl
     49d: 0f 85 44 fd ff ff            	jne	0x1e7 <_run+0x1e7>
     4a3: 4c 89 f3                     	mov	rbx, r14
     4a6: 48 83 e3 fc                  	and	rbx, -4
     4aa: 48 8d 43 fc                  	lea	rax, [rbx - 4]
     4ae: 49 89 c3                     	mov	r11, rax
     4b1: 49 c1 eb 02                  	shr	r11, 2
     4b5: 49 83 c3 01                  	add	r11, 1
     4b9: 48 85 c0                     	test	rax, rax
     4bc: 0f 84 a3 03 00 00            	je	0x865 <_run+0x865>
     4c2: f2 41 0f 12 07               	movddup	xmm0, qword ptr [r15]   ## xmm0 = mem[0,0]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:31
;     for(i = 0; i < len2; ++i) res[i] = (*e1 + e2[i]);
     4c7: 4c 89 d9                     	mov	rcx, r11
     4ca: 48 83 e1 fe                  	and	rcx, -2
     4ce: 48 f7 d9                     	neg	rcx
     4d1: 31 c0                        	xor	eax, eax
     4d3: 66 41 0f 10 0c c2            	movupd	xmm1, xmmword ptr [r10 + 8*rax]
     4d9: 66 41 0f 10 54 c2 10         	movupd	xmm2, xmmword ptr [r10 + 8*rax + 16]
     4e0: 66 0f 58 c8                  	addpd	xmm1, xmm0
     4e4: 66 0f 58 d0                  	addpd	xmm2, xmm0
     4e8: 66 41 0f 11 0c c4            	movupd	xmmword ptr [r12 + 8*rax], xmm1
     4ee: 66 41 0f 11 54 c4 10         	movupd	xmmword ptr [r12 + 8*rax + 16], xmm2
     4f5: 66 41 0f 10 4c c2 20         	movupd	xmm1, xmmword ptr [r10 + 8*rax + 32]
     4fc: 66 41 0f 10 54 c2 30         	movupd	xmm2, xmmword ptr [r10 + 8*rax + 48]
     503: 66 0f 58 c8                  	addpd	xmm1, xmm0
     507: 66 0f 58 d0                  	addpd	xmm2, xmm0
     50b: 66 41 0f 11 4c c4 20         	movupd	xmmword ptr [r12 + 8*rax + 32], xmm1
     512: 66 41 0f 11 54 c4 30         	movupd	xmmword ptr [r12 + 8*rax + 48], xmm2
     519: 48 83 c0 08                  	add	rax, 8
     51d: 48 83 c1 02                  	add	rcx, 2
     521: 75 b0                        	jne	0x4d3 <_run+0x4d3>
     523: e9 3f 03 00 00               	jmp	0x867 <_run+0x867>
     528: 31 c0                        	xor	eax, eax
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:25
;     for(i = 0; i < len1; ++i) res[i] = (e1[i] + e2[i]);
     52a: 41 f6 c6 01                  	test	r14b, 1
     52e: 74 2f                        	je	0x55f <_run+0x55f>
     530: 66 41 0f 10 04 c7            	movupd	xmm0, xmmword ptr [r15 + 8*rax]
     536: 66 41 0f 10 4c c7 10         	movupd	xmm1, xmmword ptr [r15 + 8*rax + 16]
     53d: 66 41 0f 10 14 c2            	movupd	xmm2, xmmword ptr [r10 + 8*rax]
     543: 66 0f 58 d0                  	addpd	xmm2, xmm0
     547: 66 41 0f 10 44 c2 10         	movupd	xmm0, xmmword ptr [r10 + 8*rax + 16]
     54e: 66 0f 58 c1                  	addpd	xmm0, xmm1
     552: 66 41 0f 11 14 c4            	movupd	xmmword ptr [r12 + 8*rax], xmm2
     558: 66 41 0f 11 44 c4 10         	movupd	xmmword ptr [r12 + 8*rax + 16], xmm0
     55f: 49 39 db                     	cmp	r11, rbx
     562: 0f 85 f9 fa ff ff            	jne	0x61 <_run+0x61>
     568: e9 78 01 00 00               	jmp	0x6e5 <_run+0x6e5>
     56d: 31 db                        	xor	ebx, ebx
     56f: e9 89 00 00 00               	jmp	0x5fd <_run+0x5fd>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:34
;     for(i = 0, j = 0; i < len1; ++i, ++j) {
     574: 4c 89 d9                     	mov	rcx, r11
     577: 48 83 e1 fe                  	and	rcx, -2
     57b: 45 31 ed                     	xor	r13d, r13d
     57e: 31 db                        	xor	ebx, ebx
     580: 31 c0                        	xor	eax, eax
     582: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     58c: 0f 1f 40 00                  	nop	dword ptr [rax]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:35
;       if(j >= len2) j = 0;
     590: 4c 39 f3                     	cmp	rbx, r14
     593: 49 0f 4d dd                  	cmovge	rbx, r13
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:36
;       res[i] = (e1[i] + e2[j]);
     597: f2 41 0f 10 04 c7            	movsd	xmm0, qword ptr [r15 + 8*rax] ## xmm0 = mem[0],zero
     59d: f2 41 0f 58 04 da            	addsd	xmm0, qword ptr [r10 + 8*rbx]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:34
;     for(i = 0, j = 0; i < len1; ++i, ++j) {
     5a3: 48 83 c3 01                  	add	rbx, 1
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:35
;       if(j >= len2) j = 0;
     5a7: 4c 39 f3                     	cmp	rbx, r14
     5aa: 49 0f 4d dd                  	cmovge	rbx, r13
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:36
;       res[i] = (e1[i] + e2[j]);
     5ae: f2 41 0f 11 04 c4            	movsd	qword ptr [r12 + 8*rax], xmm0
     5b4: f2 41 0f 10 44 c7 08         	movsd	xmm0, qword ptr [r15 + 8*rax + 8] ## xmm0 = mem[0],zero
     5bb: f2 41 0f 58 04 da            	addsd	xmm0, qword ptr [r10 + 8*rbx]
     5c1: f2 41 0f 11 44 c4 08         	movsd	qword ptr [r12 + 8*rax + 8], xmm0
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:34
;     for(i = 0, j = 0; i < len1; ++i, ++j) {
     5c8: 48 83 c0 02                  	add	rax, 2
     5cc: 48 83 c3 01                  	add	rbx, 1
     5d0: 48 39 c1                     	cmp	rcx, rax
     5d3: 75 bb                        	jne	0x590 <_run+0x590>
     5d5: 41 f6 c3 01                  	test	r11b, 1
     5d9: 74 22                        	je	0x5fd <_run+0x5fd>
     5db: 31 c9                        	xor	ecx, ecx
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:35
;       if(j >= len2) j = 0;
     5dd: 4c 39 f3                     	cmp	rbx, r14
     5e0: 48 0f 4c cb                  	cmovl	rcx, rbx
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:36
;       res[i] = (e1[i] + e2[j]);
     5e4: f2 41 0f 10 04 c7            	movsd	xmm0, qword ptr [r15 + 8*rax] ## xmm0 = mem[0],zero
     5ea: f2 41 0f 58 04 ca            	addsd	xmm0, qword ptr [r10 + 8*rcx]
     5f0: f2 41 0f 11 04 c4            	movsd	qword ptr [r12 + 8*rax], xmm0
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:34
;     for(i = 0, j = 0; i < len1; ++i, ++j) {
     5f6: 48 83 c1 01                  	add	rcx, 1
     5fa: 48 89 cb                     	mov	rbx, rcx
     5fd: 4c 89 d8                     	mov	rax, r11
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:38
;     if(j != len2) data[0][0] = 1.;   // bad recycle
     600: 4c 39 f3                     	cmp	rbx, r14
     603: 0f 85 cc 00 00 00            	jne	0x6d5 <_run+0x6d5>
     609: e9 da 00 00 00               	jmp	0x6e8 <_run+0x6e8>
     60e: 31 c0                        	xor	eax, eax
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:28
;     for(i = 0; i < len1; ++i) res[i] = (e1[i] + *e2);
     610: 41 f6 c6 01                  	test	r14b, 1
     614: 74 27                        	je	0x63d <_run+0x63d>
     616: 66 41 0f 10 04 c7            	movupd	xmm0, xmmword ptr [r15 + 8*rax]
     61c: 66 41 0f 10 4c c7 10         	movupd	xmm1, xmmword ptr [r15 + 8*rax + 16]
     623: f2 41 0f 12 12               	movddup	xmm2, qword ptr [r10]   ## xmm2 = mem[0,0]
     628: 66 0f 58 c2                  	addpd	xmm0, xmm2
     62c: 66 0f 58 ca                  	addpd	xmm1, xmm2
     630: 66 41 0f 11 04 c4            	movupd	xmmword ptr [r12 + 8*rax], xmm0
     636: 66 41 0f 11 4c c4 10         	movupd	xmmword ptr [r12 + 8*rax + 16], xmm1
     63d: 49 39 db                     	cmp	r11, rbx
     640: 0f 85 e8 fa ff ff            	jne	0x12e <_run+0x12e>
     646: e9 9a 00 00 00               	jmp	0x6e5 <_run+0x6e5>
     64b: 31 db                        	xor	ebx, ebx
     64d: eb 7b                        	jmp	0x6ca <_run+0x6ca>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:41
;     for(i = 0, j = 0; i < len2; ++i, ++j) {
     64f: 4c 89 f1                     	mov	rcx, r14
     652: 48 83 e1 fe                  	and	rcx, -2
     656: 45 31 ed                     	xor	r13d, r13d
     659: 31 db                        	xor	ebx, ebx
     65b: 31 c0                        	xor	eax, eax
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:42
;       if(j >= len1) j = 0;
     65d: 4c 39 db                     	cmp	rbx, r11
     660: 49 0f 4d dd                  	cmovge	rbx, r13
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:43
;       res[i] = (e1[j] + e2[i]);
     664: f2 41 0f 10 04 df            	movsd	xmm0, qword ptr [r15 + 8*rbx] ## xmm0 = mem[0],zero
     66a: f2 41 0f 58 04 c2            	addsd	xmm0, qword ptr [r10 + 8*rax]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:41
;     for(i = 0, j = 0; i < len2; ++i, ++j) {
     670: 48 83 c3 01                  	add	rbx, 1
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:42
;       if(j >= len1) j = 0;
     674: 4c 39 db                     	cmp	rbx, r11
     677: 49 0f 4d dd                  	cmovge	rbx, r13
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:43
;       res[i] = (e1[j] + e2[i]);
     67b: f2 41 0f 11 04 c4            	movsd	qword ptr [r12 + 8*rax], xmm0
     681: f2 41 0f 10 04 df            	movsd	xmm0, qword ptr [r15 + 8*rbx] ## xmm0 = mem[0],zero
     687: f2 41 0f 58 44 c2 08         	addsd	xmm0, qword ptr [r10 + 8*rax + 8]
     68e: f2 41 0f 11 44 c4 08         	movsd	qword ptr [r12 + 8*rax + 8], xmm0
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:41
;     for(i = 0, j = 0; i < len2; ++i, ++j) {
     695: 48 83 c0 02                  	add	rax, 2
     699: 48 83 c3 01                  	add	rbx, 1
     69d: 48 39 c1                     	cmp	rcx, rax
     6a0: 75 bb                        	jne	0x65d <_run+0x65d>
     6a2: 41 f6 c6 01                  	test	r14b, 1
     6a6: 74 22                        	je	0x6ca <_run+0x6ca>
     6a8: 31 c9                        	xor	ecx, ecx
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:42
;       if(j >= len1) j = 0;
     6aa: 4c 39 db                     	cmp	rbx, r11
     6ad: 48 0f 4c cb                  	cmovl	rcx, rbx
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:43
;       res[i] = (e1[j] + e2[i]);
     6b1: f2 41 0f 10 04 cf            	movsd	xmm0, qword ptr [r15 + 8*rcx] ## xmm0 = mem[0],zero
     6b7: f2 41 0f 58 04 c2            	addsd	xmm0, qword ptr [r10 + 8*rax]
     6bd: f2 41 0f 11 04 c4            	movsd	qword ptr [r12 + 8*rax], xmm0
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:41
;     for(i = 0, j = 0; i < len2; ++i, ++j) {
     6c3: 48 83 c1 01                  	add	rcx, 1
     6c7: 48 89 cb                     	mov	rbx, rcx
     6ca: 4c 89 f0                     	mov	rax, r14
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:45
;     if(j != len1) data[0][0] = 1.;   // bad recycle
     6cd: 4c 39 db                     	cmp	rbx, r11
     6d0: 4d 89 f3                     	mov	r11, r14
     6d3: 74 13                        	je	0x6e8 <_run+0x6e8>
     6d5: 48 8b 07                     	mov	rax, qword ptr [rdi]
     6d8: 48 b9 00 00 00 00 00 00 f0 3f	movabs	rcx, 4607182418800017408
     6e2: 48 89 08                     	mov	qword ptr [rax], rcx
     6e5: 4c 89 d8                     	mov	rax, r11
     6e8: 4a 89 04 ce                  	mov	qword ptr [rsi + 8*r9], rax
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:80
;   sum(data, lens, *di++, *flag);
     6ec: 48 8b 42 08                  	mov	rax, qword ptr [rdx + 8]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:51
;   int di0 = di[0];
     6f0: 48 63 08                     	movsxd	rcx, dword ptr [rax]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:52
;   int di1 = di[1];
     6f3: 4c 63 48 04                  	movsxd	r9, dword ptr [rax + 4]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:54
;   R_xlen_t len_n = lens[di0];
     6f7: 48 8b 14 ce                  	mov	rdx, qword ptr [rsi + 8*rcx]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:55
;   double * dat = data[di0];
     6fb: 48 8b 0c cf                  	mov	rcx, qword ptr [rdi + 8*rcx]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:59
;   if(!narm)
     6ff: 41 83 78 04 00               	cmp	dword ptr [r8 + 4], 0
     704: 74 25                        	je	0x72b <_run+0x72b>
     706: d9 ee                        	fldz
     708: 48 85 d2                     	test	rdx, rdx
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:62
;     for(R_xlen_t i = 0; i < len_n; ++i)
     70b: 0f 8e 3b 01 00 00            	jle	0x84c <_run+0x84c>
     711: dd d8                        	fstp	st(0)
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:62
;     for(R_xlen_t i = 0; i < len_n; ++i)
     713: 48 8d 42 ff                  	lea	rax, [rdx - 1]
     717: 89 d3                        	mov	ebx, edx
     719: 83 e3 03                     	and	ebx, 3
     71c: d9 ee                        	fldz
     71e: 48 83 f8 03                  	cmp	rax, 3
     722: 73 30                        	jae	0x754 <_run+0x754>
     724: 31 c0                        	xor	eax, eax
     726: e9 a5 00 00 00               	jmp	0x7d0 <_run+0x7d0>
     72b: d9 ee                        	fldz
     72d: 48 85 d2                     	test	rdx, rdx
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:60
;     for(R_xlen_t i = 0; i < len_n; ++i) tmp += dat[i];
     730: 0f 8e 16 01 00 00            	jle	0x84c <_run+0x84c>
     736: dd d8                        	fstp	st(0)
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:60
;     for(R_xlen_t i = 0; i < len_n; ++i) tmp += dat[i];
     738: 48 8d 42 ff                  	lea	rax, [rdx - 1]
     73c: 89 d3                        	mov	ebx, edx
     73e: 83 e3 03                     	and	ebx, 3
     741: d9 ee                        	fldz
     743: 48 83 f8 03                  	cmp	rax, 3
     747: 0f 83 b7 00 00 00            	jae	0x804 <_run+0x804>
     74d: 31 c0                        	xor	eax, eax
     74f: e9 d4 00 00 00               	jmp	0x828 <_run+0x828>
     754: dd d8                        	fstp	st(0)
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:62
;     for(R_xlen_t i = 0; i < len_n; ++i)
     756: 48 83 e2 fc                  	and	rdx, -4
     75a: d9 ee                        	fldz
     75c: 31 c0                        	xor	eax, eax
     75e: 66 90                        	nop
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:63
;       if(!ISNAN(dat[i])) tmp += dat[i];
     760: f2 0f 10 04 c1               	movsd	xmm0, qword ptr [rcx + 8*rax] ## xmm0 = mem[0],zero
; /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/math.h:212
;     return __x != __x;
     765: 66 0f 2e c0                  	ucomisd	xmm0, xmm0
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:63
;       if(!ISNAN(dat[i])) tmp += dat[i];
     769: f2 0f 10 4c c1 08            	movsd	xmm1, qword ptr [rcx + 8*rax + 8] ## xmm1 = mem[0],zero
     76f: f2 0f 11 45 b0               	movsd	qword ptr [rbp - 80], xmm0
     774: d9 c0                        	fld	st(0)
     776: dc 45 b0                     	fadd	qword ptr [rbp - 80]
     779: d9 c9                        	fxch	st(1)
     77b: db d9                        	fcmovnu	st, st(1)
     77d: dd d9                        	fstp	st(1)
; /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/math.h:212
;     return __x != __x;
     77f: 66 0f 2e c9                  	ucomisd	xmm1, xmm1
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:63
;       if(!ISNAN(dat[i])) tmp += dat[i];
     783: f2 0f 11 4d c8               	movsd	qword ptr [rbp - 56], xmm1
     788: d9 c0                        	fld	st(0)
     78a: dc 45 c8                     	fadd	qword ptr [rbp - 56]
     78d: d9 c9                        	fxch	st(1)
     78f: db d9                        	fcmovnu	st, st(1)
     791: dd d9                        	fstp	st(1)
     793: f2 0f 10 44 c1 10            	movsd	xmm0, qword ptr [rcx + 8*rax + 16] ## xmm0 = mem[0],zero
; /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/math.h:212
;     return __x != __x;
     799: 66 0f 2e c0                  	ucomisd	xmm0, xmm0
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:63
;       if(!ISNAN(dat[i])) tmp += dat[i];
     79d: f2 0f 11 45 c0               	movsd	qword ptr [rbp - 64], xmm0
     7a2: d9 c0                        	fld	st(0)
     7a4: dc 45 c0                     	fadd	qword ptr [rbp - 64]
     7a7: d9 c9                        	fxch	st(1)
     7a9: db d9                        	fcmovnu	st, st(1)
     7ab: dd d9                        	fstp	st(1)
     7ad: f2 0f 10 44 c1 18            	movsd	xmm0, qword ptr [rcx + 8*rax + 24] ## xmm0 = mem[0],zero
; /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/math.h:212
;     return __x != __x;
     7b3: 66 0f 2e c0                  	ucomisd	xmm0, xmm0
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:63
;       if(!ISNAN(dat[i])) tmp += dat[i];
     7b7: f2 0f 11 45 b8               	movsd	qword ptr [rbp - 72], xmm0
     7bc: d9 c0                        	fld	st(0)
     7be: dc 45 b8                     	fadd	qword ptr [rbp - 72]
     7c1: d9 c9                        	fxch	st(1)
     7c3: db d9                        	fcmovnu	st, st(1)
     7c5: dd d9                        	fstp	st(1)
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:62
;     for(R_xlen_t i = 0; i < len_n; ++i)
     7c7: 48 83 c0 04                  	add	rax, 4
     7cb: 48 39 c2                     	cmp	rdx, rax
     7ce: 75 90                        	jne	0x760 <_run+0x760>
     7d0: 48 85 db                     	test	rbx, rbx
     7d3: 74 77                        	je	0x84c <_run+0x84c>
     7d5: 48 8d 04 c1                  	lea	rax, [rcx + 8*rax]
     7d9: 31 c9                        	xor	ecx, ecx
     7db: 0f 1f 44 00 00               	nop	dword ptr [rax + rax]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:63
;       if(!ISNAN(dat[i])) tmp += dat[i];
     7e0: f2 0f 10 04 c8               	movsd	xmm0, qword ptr [rax + 8*rcx] ## xmm0 = mem[0],zero
; /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/math.h:212
;     return __x != __x;
     7e5: 66 0f 2e c0                  	ucomisd	xmm0, xmm0
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:63
;       if(!ISNAN(dat[i])) tmp += dat[i];
     7e9: f2 0f 11 45 d0               	movsd	qword ptr [rbp - 48], xmm0
     7ee: d9 c0                        	fld	st(0)
     7f0: dc 45 d0                     	fadd	qword ptr [rbp - 48]
     7f3: d9 c9                        	fxch	st(1)
     7f5: db d9                        	fcmovnu	st, st(1)
     7f7: dd d9                        	fstp	st(1)
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:62
;     for(R_xlen_t i = 0; i < len_n; ++i)
     7f9: 48 83 c1 01                  	add	rcx, 1
     7fd: 48 39 cb                     	cmp	rbx, rcx
     800: 75 de                        	jne	0x7e0 <_run+0x7e0>
     802: eb 48                        	jmp	0x84c <_run+0x84c>
     804: dd d8                        	fstp	st(0)
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:60
;     for(R_xlen_t i = 0; i < len_n; ++i) tmp += dat[i];
     806: 48 83 e2 fc                  	and	rdx, -4
     80a: d9 ee                        	fldz
     80c: 31 c0                        	xor	eax, eax
     80e: 66 90                        	nop
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:60
;     for(R_xlen_t i = 0; i < len_n; ++i) tmp += dat[i];
     810: dc 04 c1                     	fadd	qword ptr [rcx + 8*rax]
     813: dc 44 c1 08                  	fadd	qword ptr [rcx + 8*rax + 8]
     817: dc 44 c1 10                  	fadd	qword ptr [rcx + 8*rax + 16]
     81b: dc 44 c1 18                  	fadd	qword ptr [rcx + 8*rax + 24]
     81f: 48 83 c0 04                  	add	rax, 4
     823: 48 39 c2                     	cmp	rdx, rax
     826: 75 e8                        	jne	0x810 <_run+0x810>
     828: 48 85 db                     	test	rbx, rbx
     82b: 74 1f                        	je	0x84c <_run+0x84c>
     82d: 48 8d 04 c1                  	lea	rax, [rcx + 8*rax]
     831: 31 c9                        	xor	ecx, ecx
     833: 66 2e 0f 1f 84 00 00 00 00 00	nop	word ptr cs:[rax + rax]
     83d: 0f 1f 00                     	nop	dword ptr [rax]
     840: dc 04 c8                     	fadd	qword ptr [rax + 8*rcx]
     843: 48 83 c1 01                  	add	rcx, 1
     847: 48 39 cb                     	cmp	rbx, rcx
     84a: 75 f4                        	jne	0x840 <_run+0x840>
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:65
;   *data[di1] = (double) tmp;
     84c: 4a 8b 04 cf                  	mov	rax, qword ptr [rdi + 8*r9]
     850: dd 18                        	fstp	qword ptr [rax]
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:66
;   lens[di1] = 1;
     852: 4a c7 04 ce 01 00 00 00      	mov	qword ptr [rsi + 8*r9], 1
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:82
; }
     85a: 5b                           	pop	rbx
     85b: 41 5c                        	pop	r12
     85d: 41 5d                        	pop	r13
     85f: 41 5e                        	pop	r14
     861: 41 5f                        	pop	r15
     863: 5d                           	pop	rbp
     864: c3                           	ret
     865: 31 c0                        	xor	eax, eax
; /Users/milberg/repos/r2c/tmp/old/r2c-56xmrcc6fe.c:31
;     for(i = 0; i < len2; ++i) res[i] = (*e1 + e2[i]);
     867: 41 f6 c3 01                  	test	r11b, 1
     86b: 74 27                        	je	0x894 <_run+0x894>
     86d: f2 41 0f 12 07               	movddup	xmm0, qword ptr [r15]   ## xmm0 = mem[0,0]
     872: 66 41 0f 10 0c c2            	movupd	xmm1, xmmword ptr [r10 + 8*rax]
     878: 66 41 0f 10 54 c2 10         	movupd	xmm2, xmmword ptr [r10 + 8*rax + 16]
     87f: 66 0f 58 c8                  	addpd	xmm1, xmm0
     883: 66 0f 58 d0                  	addpd	xmm2, xmm0
     887: 66 41 0f 11 0c c4            	movupd	xmmword ptr [r12 + 8*rax], xmm1
     88d: 66 41 0f 11 54 c4 10         	movupd	xmmword ptr [r12 + 8*rax + 16], xmm2
     894: 49 39 de                     	cmp	r14, rbx
     897: 0f 85 4a f9 ff ff            	jne	0x1e7 <_run+0x1e7>
     89d: 4c 89 f0                     	mov	rax, r14
     8a0: e9 43 fe ff ff               	jmp	0x6e8 <_run+0x6e8>
