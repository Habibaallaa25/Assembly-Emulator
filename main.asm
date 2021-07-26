INCLUDE Irvine32.inc

INCLUDE macros.inc

.DATA

MemoryRegString BYTE "eax,ebx,ecx,edx,esi,edi,ebp,esp,ax,bx,cx,dx,si,di,bp,sp,al,bl,cl,dl,ah,bh,ch,dh,memory1,memory2,memory3,memory4,memory5,memory6,memory7,memory8",0
oneOpString byte "neg,mul,div,",0
TwoOpString byte "add,sub,mov,movzx,movsx,",0
dumpregstring byte "call,dumpregs",0
writeintstring byte "call,writeint",0

subs DWORD 0
mnemonicCounter DWORD 0
op1Counter DWord 0
op2Counter DWord 0
incounter DWORD 0

inpSize Dword 0
memory1 byte 1
memory2 byte 1
memory3 byte 1
memory4 byte 1
memory5 byte 1
memory6 byte 1
memory7 byte 1
memory8 byte 1

reg1Val DWORD 1
reg2Val DWORD 1
reg3Val DWORD 1
reg4Val DWORD 1
reg5Val DWORD 1
reg6Val DWORD 1
reg7Val DWORD 1
reg8Val DWORD 1

regoldval1 DWORD ?
regoldval2 DWORD ?
regoldval3 DWORD ?

sourceVal DWORD ?
DestVal DWORD ?

flag byte 0
flagop1 byte 0
flagop2 byte 0
flagop byte 0
currentOp DWORD 1
opMem DWORD 0
desiredDest DWORD 0

buff BYTE 150 DUP(?),0
notcommented BYTE 150 DUP(?),0
readyInstruction BYTE 150 DUP(?),0
notspace byte 150 DUP(?),0
Mnemonic BYTE 20 DUP(?),0
Operand1 BYTE 20 DUP(?),0
Operand2 BYTE 20 DUP(?),0
MnemonicCompare BYTE 20 DUP(?),0
regsterCompare BYTE 20 DUP(?),0

.CODE		
main PROC

mov eax,1
mov ebx,1
mov ecx,1
mov edx,1
mov edi,1
mov esi,1
;mov ebp,1
;mov esp,1


progrun:

mWrite < ">> ",0>
mov reg1Val,eax
mov reg2Val,ebx
mov reg3Val,ecx
mov reg4Val,edx
mov reg5Val,esi
mov reg6Val,edi
mov reg7Val,ebp
mov reg8Val,esp

mov opMem,0
mov currentOp,1


;read the instruction
mov edx,offset buff
mov ecx,lengthof buff
call readstring

;Find the semicolon
cld
mov edi, offset buff
mov ecx, eax
mov ebx,ecx
mov al, ';'
repne scasb ;scan string till ';' is found
jne not_found ;';' is not found, jump to not_found label
;Otherwise, ecx has the index of that character but reversed
;So, make eax = lengthof(str1) - ecx - 1
dec ebx
sub ebx, ecx
not_found:
done:
	cld
	mov inpSize,ebx
	mov edx,offset buff
	mov edi,offset notcommented
	mov ecx,ebx
	mov esi, offset buff
	mov edi, offset notcommented
	rep movsb

	cld
	;Remove spaces from the begining
	mov ecx, inpSize
	mov edi, offset notcommented
	mov al,' '
	repe scasb

	cld
	dec edi
	inc ecx
	mov esi,edi
	mov edi,offset notspace
	mov ebx,ecx
	rep movsb
	mov inpSize,ebx
	;;;;;;;;;;;;;;;;;;;;;;;;;
	mWrite <":">
	mov edx,offset notspace
	mov ecx,inpSize
	call writestring
	mWrite <":">
	
	;convert first space into ','
	cld
	mov ecx,inpSize
	mov edi,offset notspace
	mov al,' '
	repne scasb
	jne unsupportedInst
	dec edi
	mov byte ptr [edi],','
	;remove any space
	mov esi,offset readyInstruction
	mov edi,offset notspace
	mov ecx,inpSize

	L1:
		mov al,' '
		cmp [edi],al
		je cc
		mov al,[edi]
		mov [esi],al
		inc esi
		jmp ddd
		cc:
		dec inpSize
		ddd:
		inc edi
		loop L1

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;complete


	; Converting String To LowerCase letters
	mov edx,offset readyInstruction
	mov ecx,inpSize
	mov esi,offset Mnemonic
	call ConvertToLower

	mWrite <"ReadyInstruction:">
	mov edx,offset readyInstruction
	mov ecx,inpSize
	call Writestring
	mWrite <":">
	call crlf

	call DividString1
	;mov subs,ebx
	mov eax ,subs
	call writeint
	call crlf

	cmp flag,1
	je unsupportedInst


	INVOKE Str_compare, ADDR readyInstruction, ADDR dumpregstring
	jne notdump
	jmp complete

	notdump:
		INVOKE Str_compare, ADDR readyInstruction, ADDR writeintstring
		jne notInt
			mov eax,reg1Val
			call writeint
		jmp complete
	notInt:
	
	
	;INVOKE Str_compare, ADDR Operand2, ADDR zerostring
	cmp subs,1
	jne twoops
		call OneOperandMnemoic
		call OneOperand
		mov eax,op1Counter
		
		call writeint
		call crlf

		mov eax,mnemonicCounter
		
		call writeint
		call crlf

		cmp flagop,1
		je unsupportedInst

		call OneOperandFinish

		cmp flag,1
		je unsupportedInst
	jmp complete

	twoops:
		cmp subs,2
		jne unsupportedInst
		call TwoOperandMnemoic
		call OneOperand
		mov eax ,op1Counter
		call writeint
		call crlf

		call OneOperand
		mov eax,op2Counter
		call writeint
		call crlf

		cmp opMem,2
		je unsupportedInst
		call TwoOperandFinish		
	jmp complete

	unsupportedInst:
		mWrite <"Error Instruction is not supported",0dh, 0ah>
		mov flag,0
		call RemoveStrs

		call getregVVals
		jmp progrun
	complete:
		;get the registers values first then display it
		call getregVVals
		call dumpregs
		call crlf
		mov eax,0

		mWrite <"Memory1: ">
		mov al,memory1
		call writeint
		call crlf
		mWrite <"Memory2: ">
		mov al,memory2
		call writeint
		call crlf
		mWrite <"Memory3: ">
		mov al,memory3
		call writeint
		call crlf
		mWrite <"Memory4: ">
		mov al,memory4
		call writeint
		call crlf
		mWrite <"Memory5: ">
		mov al,memory5
		call writeint
		call crlf
		mWrite <"Memory6: ">
		mov al,memory6
		call writeint
		call crlf
		mWrite <"Memory7: ">
		mov al,memory7
		call writeint
		call crlf
		mWrite <"Memory8: ">
		mov al,memory8
		call writeint
		call crlf

		call RemoveStrs

		call getregVVals

	jmp progrun

exit 
main ENDP

getregVVals proc

	mov eax,reg1Val
	mov ebx,reg2Val
	mov ecx,reg3Val
	mov edx,reg4Val
	mov esi,reg5Val
	mov edi,reg6Val
	;mov ebp,reg7Val
	;mov esp,reg8Val
ret
getregVVals endp

setregVVals proc

	mov reg1Val,eax
	mov reg2Val,ebx
	mov reg3Val,ecx
	mov reg4Val,edx
	mov reg5Val,esi
	mov reg6Val,edi
	;mov reg7Val,ebp
	;mov reg8Val,esp
ret
setregVVals endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Converting string to lower
ConvertToLower proc
L1:
	mov al, [edx]
	;CHECK IF CHAR IS UPPERCASE LETTER.
	cmp  al, 65
	jb   notUpper    ; CHAR IS LOWER THAN 'A'.
	cmp  al, 90
	ja   notUpper    ; CHAR IS HIGHER THAN 'Z'.
	add al,32
	mov [edx],byte ptr al
	notUpper:
	inc edx
	;COPY LETTER TO AUX STRING.
	loop L1
ret
ConvertToLower endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dividing String into substrings
DividString1 proc  
pushad
mov esi,offset Mnemonic
mov edi,offset readyInstruction
mov ecx,inpSize
L1:
	mov al, [edi]
	cmp  al, ','
	je   found
		mov [esi],al
		inc esi
	jmp notSub
	
	found:
		add subs,1
		mov ebx,subs
		cmp subs,2
		ja error
			cmp subs,1
			je ops1
			mov esi,offset Operand2
			jmp notSub
		ops1:
			mov esi,offset Operand1
			jmp notSub
		error:
			mov flag,1
	notSub:
		inc edi
	loop L1
popad
ret
DividString1 endp

OneOperandMnemoic proc

	;INVOKE Str_compare, ADDR Mnemonic, ADDR 
	mov edi,offset oneOpString
	mov esi,offset MnemonicCompare
	mov ecx,lengthof oneOpString
	mov mnemonicCounter,0
	L1:
		mov al, [edi]
		cmp  al, ','
		je   found
			mov [esi],al
			inc esi
		jmp notSub
	
		found:
			inc mnemonicCounter
			INVOKE Str_compare, ADDR Mnemonic, ADDR MnemonicCompare
			je MnemonicFound
			mov ebx,ecx
			mov esi,offset MnemonicCompare
			mov ecx,sizeof MnemonicCompare
			L2:
				mov al,0
				mov [esi],al
				inc esi
				loop L2
				mov ecx,ebx
			;INVOKE Str_remove, ADDR [MnemonicCompare], 9
			mov esi,offset MnemonicCompare
		notSub:
			inc edi
		loop L1

		mov flag,1
		jmp Finish
		MnemonicFound:

		
		Finish:
ret
OneOperandMnemoic endp

OneOperand proc USES ebp

	mov esi,offset regsterCompare
	mov edi,offset MemoryRegString
	mov ecx,lengthof MemoryRegString
	mov ebp,0
	L1:
		mov al, [edi]
		cmp  al, ','
		je   found
			mov [esi],al
			inc esi
		jmp notSub
	
		found:
			inc ebp
			cmp currentOp,1
			ja ooop2
				INVOKE Str_compare, ADDR Operand1, ADDR regsterCompare
				je OperandFound
				jmp notn
			ooop2:
				INVOKE Str_compare, ADDR Operand2, ADDR regsterCompare
				je OperandFound
			notn:
			mov ebx,ecx
			mov esi,offset regsterCompare
			mov ecx,sizeof regsterCompare
			L2:
				mov al,0
				mov [esi],al
				inc esi
				loop L2
			mov ecx,ebx
			;INVOKE Str_remove, ADDR [regsterCompare], 9
			mov esi,offset regsterCompare
		notSub:
			inc edi
		loop L1
	mov flagop,1
	jmp Finish
	OperandFound:
	cmp ebp,24
	jbe Finish
		inc opMem
	Finish:
	.if currentOp == 1
		mov op1Counter,ebp
	.elseif currentOp == 2
		mov op2Counter,ebp
	.endif
	inc currentOp
ret
OneOperand endp

GetDestinationVal proc
	cmp desiredDest,24
	ja Memory
	.If (desiredDest == 1 || desiredDest == 9 || desiredDest == 17 || desiredDest == 21)
		Mov reg1Val,ebx
 	.ElseIf (desiredDest == 2 || desiredDest == 10 || desiredDest == 18 || desiredDest == 22)
		Mov reg2Val,ebx
	.ElseIf (desiredDest == 3 || desiredDest == 11 || desiredDest == 19 || desiredDest == 23)
		Mov reg3Val,ebx
	.ElseIf (desiredDest == 4 || desiredDest == 12 || desiredDest == 20 || desiredDest == 24)
		Mov reg4Val,ebx
	.ElseIf (desiredDest == 5 || desiredDest == 13)
		Mov reg5Val,ebx
	.ElseIf (desiredDest == 6 || desiredDest == 14)
		Mov reg6Val,ebx
	.ElseIf (desiredDest == 7 || desiredDest == 15)
		Mov reg7Val,ebx
	.ElseIf (desiredDest == 8 || desiredDest == 16)
		Mov reg8Val,ebx
	.EndIf
	jmp Complete
	Memory:
		.If (desiredDest == 25) 
			Mov memory1,bl 
 		.ElseIf (desiredDest == 26)
			Mov memory2,bl
		.ElseIf (desiredDest == 27)
			Mov memory3,bl
		.ElseIf (desiredDest == 28)
			Mov memory4,bl
		.ElseIf (desiredDest == 29)
			Mov memory5,bl
		.ElseIf (desiredDest == 30)
			Mov memory6,bl
		.ElseIf (desiredDest == 31)
			Mov memory7,bl
		.ElseIf (desiredDest == 32)
			Mov memory8,bl
		.EndIf

	Complete:
ret
GetDestinationVal endp

FindDestVal proc
	cmp desiredDest,24
	ja Memory
	.If (desiredDest == 1 || desiredDest == 9 || desiredDest == 17 || desiredDest == 21)
		Mov ebx,reg1Val
 	.ElseIf (desiredDest == 2 || desiredDest == 10 || desiredDest == 18 || desiredDest == 22)
		Mov ebx,reg2Val
	.ElseIf (desiredDest == 3 || desiredDest == 11 || desiredDest == 19 || desiredDest == 23)
		Mov ebx,reg3Val
	.ElseIf (desiredDest == 4 || desiredDest == 12 || desiredDest == 20 || desiredDest == 24)
		Mov ebx,reg4Val
	.ElseIf (desiredDest == 5 || desiredDest == 13)
		Mov ebx,reg5Val
	.ElseIf (desiredDest == 6 || desiredDest == 14)
		Mov ebx,reg6Val
	.ElseIf (desiredDest == 7 || desiredDest == 15)
		Mov ebx,reg7Val
	.ElseIf (desiredDest == 8 || desiredDest == 16)
		Mov ebx,reg8Val
	.EndIf
	jmp Complete
	Memory:
		Mov destVal,0
		.If (desiredDest == 25) 
			Mov bl,memory1 
 		.ElseIf (desiredDest == 26)
			Mov bl,memory2
		.ElseIf (desiredDest == 27)
			Mov bl,memory3
		.ElseIf (desiredDest == 28)
			Mov bl,memory4
		.ElseIf (desiredDest == 29)
			Mov bl,memory5
		.ElseIf (desiredDest == 30)
			Mov bl,memory6
		.ElseIf (desiredDest == 31)
			Mov bl,memory7
		.ElseIf (desiredDest == 32)
			Mov bl,memory8
		.EndIf
	Complete:
ret
FindDestVal endp

OneOperandFinish proc

	mov eax,op1Counter
	mov desiredDest,eax
	
	call FindDestVal

	mov regoldval1,ebx

	mov eax,reg1Val
	mov edx,reg4Val
	.if (mnemonicCounter == 1)
		.if (op1Counter > 0 && op1Counter <=8)
			neg ebx
		.elseif (op1Counter >= 9 && op1Counter <= 16)
			neg bx
		.elseif (op1Counter >= 17 && op1Counter <=20)
			neg bl
		.elseif (op1Counter >= 21 && op1Counter <=24)
			neg bh
		.elseif (op1Counter >= 25 && op1Counter <= 32)
			neg bl
		.endif
	.elseif (mnemonicCounter == 2)
		.if (op1Counter >0 && op1Counter <=8)
			mul ebx
			jc overflow
		.elseif (op1Counter >= 9 && op1Counter <= 16)
			mul bx
			jc overflow
		.elseif (op1Counter >= 17 && op1Counter <=20)
			mul bl
			jc overflow
		.elseif (op1Counter >= 21 && op1Counter <=24)
			mul bh
			jc overflow
		.elseif (op1Counter >= 25 && op1Counter <= 32)
			mul bl
			jc overflow
		.endif
	.elseif (mnemonicCounter == 3)
		xor edx,edx
		.if (op1Counter >0 && op1Counter <=8)
			div ebx
			jc overflow
		.elseif (op1Counter >= 9 && op1Counter <= 16)
			div bx
			jc overflow
		.elseif (op1Counter >= 17 && op1Counter <=20)
			div bl
			jc overflow
		.elseif (op1Counter >= 21 && op1Counter <=24)
			div bh
			jc overflow
		.elseif (op1Counter >= 25 && op1Counter <= 32)
			div bl
			jc overflow
		.endif
	.endif

		mov reg4Val,edx
		mov reg1Val,eax
	jmp Complete
	overflow:
		mov regoldval2,eax
		mov regoldval3,edx
		mWrite <"Warning: The result of this operation won’t fit in the specified destination, would you still like to continue? (Y/N) ",0dh, 0ah>

		call readchar
		.if al == 'Y' || al == 'y'
			mov reg4Val,edx
			mov reg1Val,eax
			call GetDestinationVal

		.elseif al == 'n' || al == 'N'

			call getregVVals

			jmp Finish
		.endif
	Complete:
		mov reg4Val,edx
		mov reg1Val,eax
		call GetDestinationVal
	
	Finish:

ret
OneOperandFinish endp


TwoOperandMnemoic proc

	;INVOKE Str_compare, ADDR Mnemonic, ADDR 
	mov edi,offset twoOpString
	mov esi,offset MnemonicCompare
	mov ecx,lengthof twoOpString
	mov mnemonicCounter,0
	L1:
		mov al, [edi]
		cmp  al, ','
		je   found
			mov [esi],al
			inc esi
		jmp notSub
	
		found:
			inc mnemonicCounter
			INVOKE Str_compare, ADDR Mnemonic, ADDR MnemonicCompare
			je MnemonicFound
			mov ebx,ecx
			mov esi,offset MnemonicCompare
			mov ecx,sizeof MnemonicCompare
			L2:
				mov al,0
				mov [esi],al
				inc esi
				loop L2
				mov ecx,ebx
			mov esi,offset MnemonicCompare
		notSub:
			inc edi
		loop L1

	mov flag,1
	jmp Finish
	MnemonicFound:

		
	Finish:
ret
TwoOperandMnemoic endp

TwoOperandFinish proc
	
	mov eax,op1Counter
	mov desiredDest,eax
	call FindDestVal
	mov regoldval1,ebx
	mov eax,ebx

	mov eax,op2Counter
	mov desiredDest,eax

	mov eax,ebx

	call FindDestVal
	mov regoldval2,ebx

	.if (mnemonicCounter == 1)
		.if (op1Counter <=8 && op1Counter >0 && op2Counter <=8 && op2Counter > 0)
			add eax,ebx
			jc overflow
		.elseif (op1Counter >= 9 && op1Counter <= 16 && op2Counter >= 9 && op2Counter <= 16)
			add ax,bx
			jc overflow
		.elseif (op1Counter >= 17 && op1Counter <=20 && op2Counter >= 17 && op2Counter <= 20)
			add al,bl
			jc overflow
		.elseif (op1Counter >= 21 && op1Counter <=24 && op2Counter >= 21 && op2Counter <=24)
			add ah,bh
			jc overflow
		.elseif (op1Counter >= 17 && op1Counter <=20 && op2Counter >= 21 && op2Counter <= 24)
			add al,bh 
			jc overflow
		.elseif (op1Counter >= 21 && op1Counter <=24 && op2Counter >= 17 && op2Counter <=20)
			add ah,bl
			jc overflow
		.elseif (op1Counter >= 25 && op1Counter <=32 && op2Counter >= 17 && op2Counter <=20)
			add al,bl
			jc overflow
		.elseif (op1Counter >= 17 && op1Counter <=20 && op2Counter >= 25 && op2Counter <=32)
			add al,bl
			jc overflow
		.elseif (op1Counter >= 25 && op1Counter <=32 && op2Counter >= 21 && op2Counter <=24)
			add al,bh
			jc overflow
		.elseif (op1Counter >= 21 && op1Counter <=24 && op2Counter >= 25 && op2Counter <=32)
			add ah,bl
			jc overflow
		.else
			mWrite <"Operands Are not The Same Size",0dh,0ah>
		.endif
	.elseif (mnemonicCounter == 2)
		.if (op1Counter <=8 && op1Counter >0 && op2Counter <=8 && op2Counter > 0)
			sub eax,ebx
		.elseif (op1Counter >= 9 && op1Counter <= 16 && op2Counter >= 9 && op2Counter <= 16)
			sub ax,bx
		.elseif (op1Counter >= 17 && op1Counter <=20 && op2Counter >= 17 && op2Counter <= 20)
			sub al,bl
		.elseif (op1Counter >= 21 && op1Counter <=24 && op2Counter >= 21 && op2Counter <=24)
			sub ah,bh
		.elseif (op1Counter >= 17 && op1Counter <=20 && op2Counter >= 21 && op2Counter <= 24)
			sub al,bh 
		.elseif (op1Counter >= 21 && op1Counter <=24 && op2Counter >= 17 && op2Counter <=20)
			sub ah,bl
		.elseif (op1Counter >= 25 && op1Counter <=32 && op2Counter >= 17 && op2Counter <=20)
			sub al,bl
		.elseif (op1Counter >= 17 && op1Counter <=20 && op2Counter >= 25 && op2Counter <=32)
			sub al,bl
		.elseif (op1Counter >= 25 && op1Counter <=32 && op2Counter >= 21 && op2Counter <=24)
			sub al,bh
		.elseif (op1Counter >= 21 && op1Counter <=24 && op2Counter >= 25 && op2Counter <=32)
			sub ah,bl
		.else
			mWrite <"Operands Are not The Same Size",0dh,0ah>
		.endif
	.elseif (mnemonicCounter == 3)
		.if (op1Counter <=8 && op1Counter >0 && op2Counter <=8 && op2Counter > 0)
			mov eax,ebx
		.elseif (op1Counter >= 9 && op1Counter <= 16 && op2Counter >= 9 && op2Counter <= 16)
			mov ax,bx
		.elseif (op1Counter >= 17 && op1Counter <=20 && op2Counter >= 17 && op2Counter <= 20)
			mov al,bl
		.elseif (op1Counter >= 21 && op1Counter <=24 && op2Counter >= 21 && op2Counter <=24)
			mov ah,bh
		.elseif (op1Counter >= 17 && op1Counter <=20 && op2Counter >= 21 && op2Counter <= 24)
			mov al,bh 
		.elseif (op1Counter >= 21 && op1Counter <=24 && op2Counter >= 17 && op2Counter <=20)
			mov ah,bl
		.elseif (op1Counter >= 25 && op1Counter <=32 && op2Counter >= 17 && op2Counter <=20)
			mov al,bl
		.elseif (op1Counter >= 17 && op1Counter <=20 && op2Counter >= 25 && op2Counter <=32)
			mov al,bl
		.elseif (op1Counter >= 25 && op1Counter <=32 && op2Counter >= 21 && op2Counter <=24)
			mov al,bh
		.elseif (op1Counter >= 21 && op1Counter <=24 && op2Counter >= 25 && op2Counter <=32)
			mov ah,bl
		.else
			mWrite <"Operands Are not The Same Size",0dh,0ah>
		.endif
	.elseif (mnemonicCounter == 4)
		.if (op1Counter <=8 && op1Counter >0 && (op2Counter <=32 && op2Counter >= 25 || op2Counter <= 20 && op2Counter >= 17))
			movzx eax,bl
		.elseif (op1Counter <=8 && op1Counter >0 && op2Counter <= 16 && op2Counter >= 9)
			movzx eax,bx
		.elseif (op1Counter <=8 && op1Counter >0 && op2Counter <= 24 && op2Counter >= 20)
			movzx eax,bh
		.elseif (op1Counter >= 9 && op1Counter <= 16 && (op2Counter <=32 && op2Counter >= 25 || op2Counter <= 20 && op2Counter >= 17))
			movzx ax,bl
		.elseif (op1Counter >= 9 && op1Counter <= 16 && op2Counter <= 24 && op2Counter >= 20)
			movzx ax,bh
		.else
			jmp operandsError
		.endif
	.elseif (mnemonicCounter == 5)
		.if (op1Counter <=8 && op1Counter >0 && (op2Counter <=32 && op2Counter >= 25 || op2Counter <= 20 && op2Counter >= 17))
			movsx eax,bl
		.elseif (op1Counter <=8 && op1Counter >0 && op2Counter <= 16 && op2Counter >= 9)
			movsx eax,bx
		.elseif (op1Counter <=8 && op1Counter >0 && op2Counter <= 24 && op2Counter >= 20)
			movsx eax,bh
		.elseif (op1Counter >= 9 && op1Counter <= 16 && (op2Counter <=32 && op2Counter >= 25 || op2Counter <= 20 && op2Counter >= 17))
			movsx ax,bl
		.elseif (op1Counter >= 9 && op1Counter <= 16 && op2Counter <= 24 && op2Counter >= 20)
			movsx ax,bh
		.else 
			jmp operandsError
		.endif
	.endif
	jmp Finish
	operandsError:
		mWrite <"ERROR: the first operand is not bigger than the second one.",0dh, 0ah>
		call getregVVals
		jmp doneeee
		overflow:
		mWrite <"Warning: The result of this operation won’t fit in the specified destination, would you still like to continue? (Y/N) ",0dh, 0ah>
		call readchar
		.if al == 'Y' || al == 'y'
			mov ebx,0

		.elseif al == 'n' || al == 'N'

			call getregVVals

			jmp doneeee
		.endif
	Finish:
	mov ebx,eax
	mov eax,op1Counter
	mov desiredDest,eax
	call GetDestinationVal
	
	doneeee:
ret
TwoOperandFinish endp

GetSourceFun proc
	;INVOKE Str_compare, ADDR Mnemonic, ADDR 


ret
GetSourceFun endp


RemoveStrs proc
pushad
mov esi,offset buff
mov ecx,lengthof buff
L1:
	mov al,0
	mov [esi],al
	inc esi
	loop L1
mov esi,offset notcommented
mov ecx,lengthof notcommented
L8:
	mov al,0
	mov [esi],al
	inc esi
	loop L8

mov esi,offset notspace
mov ecx,lengthof notspace
L2:
	mov al,0
	mov [esi],al
	inc esi
	loop L2

mov esi,offset readyInstruction
mov ecx,lengthof readyInstruction
L3:
	mov al,0
	mov [esi],al
	inc esi
	loop L3

mov esi,offset Mnemonic
mov ecx,lengthof Mnemonic
L4:
	mov al,0
	mov [esi],al
	inc esi
	loop L4

mov esi,offset Operand1
mov ecx,lengthof Operand1
L5:
	mov al,0
	mov [esi],al
	inc esi
	loop L5

mov esi,offset Operand2
mov ecx,lengthof Operand2
L6:
	mov al,0
	mov [esi],al
	inc esi
	loop L6

mov esi,offset MnemonicCompare
mov ecx,lengthof MnemonicCompare
L7:
	mov al,0
	mov [esi],al
	inc esi
	loop L7


mov esi,offset regsterCompare
mov ecx,sizeof regsterCompare
L9:
	mov al,0
	mov [esi],al
	inc esi
	loop L9

mov subs,0
mov mnemonicCounter,0
mov currentOp,1

mov flag , 0
mov flagop1 , 0
mov flagop2 , 0
mov flagop , 0
mov opMem , 0
popad
ret
RemoveStrs endp
END main