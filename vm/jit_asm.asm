[bits 64]

push rax
push rcx
push rdx
push rbx
push rsi
push rdi
push r8
push r9
push r10
push r11
push r12
push r13
push r14
push r15
push rsp
push rbp
mov rax, rsp
mov rsp, 140420209361424
push rax
mov rbp, rsp

push rbp
mov rbp , rsp
call label0
mov rsp , rbp
pop rbp

pop rax
mov rsp, rax
pop rbp
pop rsp
pop r15
pop r14
pop r13
pop r12
pop r11
pop r10
pop r9
pop r8
pop rdi
pop rsi
pop rbx
pop rdx
pop rcx
pop rax

mov rax , 2
ret
label3:
push rcx
push rdx
push rbx
           
cmp rax , 1
setl dl
cmp rax , 1
sete cl
or rdx , rcx
not rdx
           
and rdx , 1
             
test dl , dl
jnz label1
mov r15 , rax
jmp label2
label1:
push rax
           
dec rax
                                  
call label3
pop rax
mov rcx , r15
push rax
push rcx
           
sub rax , 2
                                  
call label3
pop rcx
pop rax
add rcx , r15
mov r15 , rcx
label2:
pop rbx
pop rdx
pop rcx
ret
label0:
push rax
push rax
mov rax , 34
                                  
call label3
pop rax
mov rdi , r15
mov rax , 3
mov rax , 94053701666528
call rax
pop rax
ret
