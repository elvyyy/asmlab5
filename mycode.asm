.model tiny
.data 
cmd_length 					db ?
cmd_line 					db 127 dup('$')

file_name 					db 50 dup('$')   
file_handle 				dw 0000h 
file_size 					dw 0000h

new_file_name 				db '/', 50 dup(0), 0   
new_file_handle 			dw 0000h 

msgFileError 				db 10,13,"No such file in directory$",10,13,'$'
msgFileOpened 				db 10,13,"File is opened!$",10,13,'$'  
msgFileClosed 				db 10,13,"File is closed!$",10,13,'$'
msgCMD_Error 				db 10,13,"Error occured while parsing cmdline",10,13,'$'
msgErrorMoving 				db 10,13,"Error moving pointer...$",10,13,'$'

count_of_read_bytes 		dw 3Eh 

maxWordSize 				equ 50
cmdBufSize 					db 52 dup(0)
cmdBUFFER 					db 52 dup(0)

start_buffer_pos 			dw 0000h
end_buffer_pos 				dw 0000h
file_buffer 				db 202 dup ('$')
buffer_size 				dw 0000h 
new_buffer_size 			dw 0000h
amount_of_bytes_to_shift 	dw 0000h

delem 						db " .,!",09H,0Dh,0Ah

word_to_replace 			db 54 dup ('$') 
word_size 					dw 0
new_word 					db 54 dup ('$')                       
size_of_new_word 				dw 0 
	
current_indexL 				dw 0000h 
current_indexH 				dw 0000h 
out_indexL 					dw 0000h 
out_indexH 					dw 0000h 
read_bytes 					dw 0000h 

.code
print_str macro out_str 
    pusha
    mov ah,09h
    mov dx,offset out_str
    int 21h 
    popa
endm 

get_word macro inp_str
    pusha
    mov ah, 0Ah
    lea dx, inp_str
    int 21h
    popa
endm  

main:
    mov ax,@data
    mov es,ax
    
    xor cx,cx
  
	call move_cmd
    call process_command_line
	
	lea si, file_name
	lea di, new_file_name
	mov cx, 11
	REPE cmpsb
	cmp cx, 0
	je exit
	
    call open_file 
    mov  file_handle, ax             ;get a file handle  
    call get_size_of_file
    call create_new_file   
main_loop: 
    cmp file_size, 0000h
    je close_file_exit    
    call read_file       
    call shrink_buffer_to_word
    print_str file_buffer  
    
    call find_words_to_replace
     
    print_str file_buffer
     
    call write_into_new_file
    
    mov ax,read_bytes
    sub file_size,ax
    
    
    cmp file_size,0000h
    jne main_loop
   
close_file_exit: 
    call close_file 
    
    mov bx, new_file_handle ;
    xor ax, ax      ; 
    mov ah, 3Eh     ; close file
    int 21h
	
	mov ah, 41h                         ;del a file
    lea dx, file_name
    int 21h 
	
	xor ax, ax
    mov ah, 56h                         ;rename
    lea dx, new_file_name
    lea di, file_name
    int 21h  
	
exit:    
    mov ah,4Ch
    mov al,00h
    int 21h 
;Functions

move_cmd proc USES si, di
		push cx
		
		mov cl,ds:[80h]				; 0x80h holds a size of cmd
		mov cmd_length,cl			; put the size into cl reg
		mov si,82h					
		lea di,cmd_line
		rep movsb           		; move bytes to plain di mem
		
		pop cx
		ret
move_cmd endp

process_command_line proc
    pusha
    cld

    xor cx,cx
    xor ax,ax
    mov cl,cmd_length				; length of cmd line
    lea si,cmd_line					; cmd line
    
    lea di,file_name
    call get_cmd_word 
    
    
    lea di,word_to_replace
    call get_cmd_word
    cmp word_to_replace,'$'
    je error_cmd
    push si
    lea si,word_to_replace
    call get_cmd_word_size
    mov word_size,ax
    pop si 
    
    lea di,new_word
    call get_cmd_word
    cmp new_word,'$'
    je error_cmd 
    lea si,new_word 
    call get_cmd_word_size
    mov size_of_new_word,ax
    popa
    ret
error_cmd:
    print_str msgCMD_Error
    jmp exit     
            
    
process_command_line endp 

get_cmd_word proc			; extract a word
    push ax
    push cx
    push di 
    xor ax,ax
get_cmd_word_loop:
    mov al,[si]
    
    cmp al,' '
    je stop_get_cmd_word_loop
    cmp al,09h
    je stop_get_cmd_word_loop
    cmp al,0Ah
    je stop_get_cmd_word_loop
    cmp al,0Dh
    je stop_get_cmd_word_loop
    cmp al,00h
    je stop_get_cmd_word_loop
    
    mov [di],al
    
    inc si
    inc di
    
    loop get_cmd_word_loop
stop_get_cmd_word_loop:
    mov [di],0
    inc si
            
    pop di
    pop cx
    pop ax
    ret
get_cmd_word endp

get_cmd_word_size proc USES ax         	; calc size of words           
	push bx                     
	push si                   
	                            
	xor ax, ax                 ; counter 
                                
    startCalc:                  
	    mov bl, [si]         
	    cmp bl, 0            		; while not zero
	    je endCalc             
                               
	    inc si                  
	    inc ax                                                                      
	    jmp startCalc           
	                            
endCalc:                   
	pop si                      
	pop bx                     
	ret                         
get_cmd_word_size endp                          
    
open_file proc USES ax
    push dx
    mov dx,offset file_name 
    mov ax, 3D02h  					; 3D - an attempt to open a file 
									; 02 - read and write mode
    int 21h       
    jc error_opening				; carry flag == 1
    jmp file_opened_successfully
error_opening:
    print_str msgFileError
    pop dx
    jmp exit
file_opened_successfully:      
    print_str msgFileOpened  
    pop dx
    ret
open_file endp   

;READING 
read_file proc 
    push bx
    push cx
    push dx
    
    call set_cursor						; set cursor after the last processed byte
    
    mov bx, file_handle              
    mov ah, 3Fh                                             
    mov cx, count_of_read_bytes         	; expected value
    mov dx, offset file_buffer    
    int 21h   
    
    mov read_bytes, ax					; actual value
    dec ax
    mov buffer_size,ax
    mov new_buffer_size,ax
    mov amount_of_bytes_to_shift,ax 
      
    mov ax,count_of_read_bytes  			; test of
    clc									; clear carry
    add current_indexH,ax 
    jc add_l_bytes
    jmp end_reading_file 
    
add_l_bytes:
    inc current_indexL

end_reading_file:    
    
    pop dx
    pop cx
    pop bx
    ret
read_file endp

set_cursor proc 
    pusha
      
    mov bx, file_handle
    mov al, 00h                    
    xor cx, cx                        
    mov dx, current_indexH         
    mov cx, current_indexL        
    mov ah, 42h 						; move read/write courser
    int 21h
    
    jc set_cursor_error								; if carry flag == 1 then an error occured
    jmp set_cursor_success
set_cursor_error:
    print_str msgErrorMoving 
    jmp exit
set_cursor_success:    
    popa    
    ret
set_cursor endp

;CLOSING
close_file proc
    pusha
    mov bx, file_handle 
    xor ax, ax       
    mov ah, 3Eh    
    int 21h          
    print_str msgFileClosed
    popa
    ret
close_file endp  

shrink_buffer_to_word proc
    pusha
    lea si,file_buffer
    add si,count_of_read_bytes			; add the offset
    dec si
    shrink_loop:
    cmp [si],'$' 
    je its_normal
    cmp [si],00h 
    je its_normal
    cmp [si],' '
    je its_normal
    mov [si],'$'
    dec si
    dec current_indexH
    dec read_bytes
    jmp shrink_loop 
its_normal:    
    popa
    ret
shrink_buffer_to_word endp    
 
get_size_of_file proc
    pusha    
    xor cx,cx
    xor dx,dx
    mov ah,42h
    mov al,02h
    mov bx,file_handle
    int 21h 
    mov file_size,ax
    popa 
    ret
get_size_of_file endp 
   
find_words_to_replace proc
    pusha
    lea si,file_buffer

    loop1:

        mov start_buffer_pos,si
        lea di,word_to_replace
        mov cx,word_size  
        REPE cmpsb
        cmp cx,0000h
        je find 
continue_loop1:
 
        sub cx,word_size
        not cx
        inc cx
        sub new_buffer_size,cx 
        sub amount_of_bytes_to_shift,cx
        ;sub file_size,cx 
        cmp file_size,0000h
        je ch_buf_endp 

        lea di,delem
        loop2:             
            cmpsb 
            je loop1
            dec si
            cmp [si],'$'
            je ch_buf_endp
            cmp [si],0
            je ch_buf_endp
            cmp [di],0Ah
            je loop1
            jne loop2
    find:                
    
    cmp [si], ' '
    je check_before_word
    cmp [si], 0Ah
    je check_before_word
    cmp [si], 0
    je check_before_word
    cmp [si], 09h
    je check_before_word   
    cmp [si], '.'
    je check_before_word     
    cmp [si], ','
    je check_before_word  
    cmp [si], 0Dh
    je check_before_word
    
    jmp continue_loop1

to_finish_word:
	pop si
    jmp finish_find
    
check_before_word: 
	push si   
    sub si, word_size
    dec si
    
    cmp [si], ' '
    je to_finish_word
    cmp [si], 0Ah
    je to_finish_word
    cmp [si], 0
    je to_finish_word
    cmp [si], 09h
    je to_finish_word   
    cmp [si], '.'
    je to_finish_word 
    cmp [si], ','
    je to_finish_word
    
    
    ;add si, word_size
    pop si 
     
    jmp continue_loop1
    
finish_find:
    call recalculate_new_buffer_length
    
    call change_word 
    cmp file_size,0000h
    je ch_buf_endp
    jne loop1
ch_buf_endp:
    dec si
    cmp [si],00h
    je end_ 
    jne all_is_clear 
end_:    
    mov file_size,0000h
    mov cx,word_size
    sub cx,size_of_new_word 
    sub si,cx
    dec si
all_is_clear:   
    mov [si],00h
    inc si
    loop all_is_clear              
    popa
    ret
find_words_to_replace endp

;GETTING INFO IF CHANGE SIZE OF BUFFER
get_output_buf_size proc
    pusha
    lea si,file_buffer
    mov buffer_size,000h
    loop_get_out_size:
    inc si
    inc buffer_size
    cmp [si],00h
     
    jne check_$ 
    jmp end_check_counting
check_$:    
    cmp [si],'$'
    jne loop_get_out_size
	dec buffer_size
    mov [si],00h
    jmp end_check_counting
end_check_counting:    
    popa
    ret
get_output_buf_size endp    

;MOVSB CHGWORD AND NEW WORD        
change_word proc
    mov si,start_buffer_pos
    mov di,si
    mov cx,size_of_new_word    

    cmp [si],' '
    je word_is_changed 
    cmp [si],'!'
    je word_is_changed
	cmp [si],'.'
    je word_is_changed
    cmp [si],09H
    je word_is_changed
	
get_pos_to_ch:
    ;cmp [si],0Dh            ;enter
    ;je enter_in_buf

    inc si
    dec amount_of_bytes_to_shift  
    cmp [si],00h
    je found_end_of_word
    cmp [si],'!'
    je found_end_of_word
    cmp [si],09H 
    je found_end_of_word
    cmp [si],','
    je found_end_of_word
	cmp [si],'.'
    je found_end_of_word
	cmp [si], 0Dh
    je found_end_of_word
    cmp [si],' '
    jne get_pos_to_ch 
    
found_end_of_word:      
    mov ax, word_size
    cmp size_of_new_word,ax
    ja add_to_right								; if the new word is bigger
    cld											; clear duration
    jmp add_to_left
add_to_right: 
    mov amount_of_bytes_to_shift, 0000h
    add_to_right_loop:
    inc si
    inc amount_of_bytes_to_shift
    cmp [si], 00
    jne add_to_right_loop
    mov cx,amount_of_bytes_to_shift
    inc cx
    mov di,si
    add di,size_of_new_word
    sub di,word_size
    
    std  
    jmp replace_word_loop
add_to_left:
    cmp [si],00h 
    je replace_word_loop 
    push si
    mov amount_of_bytes_to_shift,0000h
    add_to_left_loop:
    inc si
    inc amount_of_bytes_to_shift
    cmp [si],00
    jne add_to_left_loop
    mov cx,amount_of_bytes_to_shift
    pop si
    add di,size_of_new_word
replace_word_loop:    
    repe movsb
    mov ax, buffer_size					; recalc buffer size
    add ax, size_of_new_word
    sub ax, word_size
    mov buffer_size, ax
    ;/////////////////////////
    cld
    lea si,new_word
    mov di,start_buffer_pos
    mov cx,size_of_new_word
    repe movsb
    xchg si,di     
    ;/////////////////////////
    jmp word_is_changed
        
enter_in_buf:
    inc si
	;jmp get_pos_to_ch
    dec amount_of_bytes_to_shift
    jmp get_pos_to_ch    
end_of_buf:
    movsb
    
word_is_changed:        
    ret
change_word endp  

;CREATING NEW RESULTING FILE
create_new_file proc
    pusha
        mov ah, 5Ah
        mov cx,0000h 
        lea dx, new_file_name
        int 21h
        mov new_file_handle, ax  
              
    popa
    ret
create_new_file endp 

open_new_file proc
    pusha
    mov dx,offset new_file_name 
    mov ax, 3D02h  ;3D - open file, 02 - for reading and recording  
    int 21h 
    popa
    ret
open_new_file endp 

recalculate_new_buffer_length proc
    pusha
    mov ax,new_buffer_size
    sub ax,word_size
    add ax,size_of_new_word
    ;mov new_buffer_size,ax
    popa
    ret
recalculate_new_buffer_length endp 

get_end_pos_buf proc
    pusha
    mov ax,start_buffer_pos
    add ax,buffer_size
    mov end_buffer_pos,ax
    popa
    ret
get_end_pos_buf endp    

write_into_new_file proc
    pusha 
   
    mov bx, file_handle
    mov al, 00h                     ;start position in file
    xor cx, cx                      ;the beginning of file
    
    mov dx, out_indexH         ; - 
    mov cx, out_indexL         ; - 
    mov ah, 42h 
    int 21h						; set a file cursor
	
    mov ah,40h					; write into file function
    mov bx,new_file_handle 
    call get_output_buf_size
    mov cx,buffer_size
    ;inc cx
    lea dx,file_buffer
    int 21h
	
	
    call flush_buffer
        
    clc
    add out_indexH,cx  ; check of of

    jc add_to_l_bytes
    jmp end_writing
add_to_l_bytes:
    inc out_indexL
    mov out_indexH,0000h     
    
end_writing:    
    popa
    ret
write_into_new_file endp

flush_buffer proc
    pusha
    lea si,file_buffer
    mov cx,202
    init_$_buf:
    mov [si],'$'
    inc si
    loop init_$_buf
    mov buffer_size,0000h 
    popa
    ret
flush_buffer endp    


end main 