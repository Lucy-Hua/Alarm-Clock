
$NOLIST
$MODEFM8LB1
$LIST

CLK           EQU 24000000 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 2000*2    ; The tone we want out is A mayor.  Interrupt rate must be twice as fast.
TIMER0_RELOAD EQU ((65536-(CLK/(TIMER0_RATE))))
TIMER4_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER4_RELOAD EQU ((65536-(CLK/(TIMER4_RATE))))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/(TIMER2_RATE))))

SET_MODE      equ p0.0
BOOT_BUTTON   equ P3.7
SOUND_OUT     equ P2.1
SET_TIME      equ P3.3
SET_MIN       equ P2.5
SET_HR        equ P2.3
SET_ALARM     equ P3.1


SW_END      equ P3.1
SW_START      equ P3.3


; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR
	

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B

	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR
	
	
org 0x008B 
	ljmp Timer4_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when a second has passed
BCD_sec_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
BCD_min_counter:  ds 1
BCD_hr_counter:   ds 1

alarm_min: ds 1
alarm_hr: ds 1

;for stopwatch mode-----------------
Count1s: ds 2
BCD_milisec_swcounter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
BCD_sec_swcounter:  ds 1
BCD_min_swcounter:   ds 1


bseg
clk_mode: dbit 1; 1=timer, 0 = stopwatch

seconds_flag: dbit 1 ; Set to one in the ISR every time 1 second had passed
AMPM_flag: dbit 1 ;Toggle between AM and PM everytime hour is 13, 1 = AM, 0 = PM
AMPM_alarm_flag: dbit 1

set_time_flag: dbit 1
set_alarm_flag: dbit 1

stop_alarm_flag: dbit 1 ;is needed for when the minuts and hours match but the stop alarm button was already pressed
is_alarm_flag: dbit 1

;for stopwatch setting --------------
ms_seconds_flag: dbit 1
sw_start_flag: dbit 1 ; 1= pause, 0 = start

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P2.0
LCD_RW equ P1.7
LCD_E  equ P1.6
LCD_D4 equ P1.1
LCD_D5 equ P1.0
LCD_D6 equ P0.7
LCD_D7 equ P0.6
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
clock_mode_init:  db '  :  :  ', 0
stopwatch_mode_init:  db '00:00.00    ', 0
AM:  db 'AM', 0
PM:  db 'PM', 0


	
;-----------------------------------;
; Routine to initialize the timer 0 ;
;-----------------------------------;
Timer0_Init:
	orl CKCON0, #00000100B ; Timer 0 uses the system clock
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    jb is_alarm_flag, skip_timer0
    setb TR0  ; Start timer 0
skip_timer0:
	ret

;---------------------------------;
; ISR for timer 0.                ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	; Timer 0 can not autoreload so we need to reload it in the ISR:
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	jb is_alarm_flag, skip_timer0isr
	setb TR0 ;wierd clicky sound??
	cpl SOUND_OUT ; Toggle the pin connected to the speaker //actually make sounds
skip_timer0isr:		
	reti
	
	
;---------------------------------;
; Routine to initialize timer 4  ;
;---------------------------------;
Timer4_Init:
	MOV SFRPAGE, #0x10
	orl CKCON1, #0b00000001 ; Timer 4 uses the system clock
	mov TMR4CN0, #0 ; Stop timer/counter.  Autoreload mode.
	mov TMR4H, #high(TIMER4_RELOAD)
	mov TMR4L, #low(TIMER4_RELOAD)
	; Set the reload value
	mov TMR4RLH, #high(TIMER4_RELOAD)
	mov TMR4RLL, #low(TIMER4_RELOAD)

    
    mov a, EIE2
    orl a, #0b00000100
    mov EIE2, a
    
    clr a
	mov Count1s+0, a
	mov Count1s+1, a
  	jb sw_start_flag, skip_timer4isr 
    setb TR4  ; Enable timer 4
  skip_timer4isr:     
	ret



;---------------------------------;
; ISR for timer 4                 ;
;---------------------------------;
Timer4_ISR:
	clr TF4H 

	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw


	; Increment the 16-bit one mili second counter
	inc Count1s+0    ; Increment the low 8-bits first
	mov a, Count1s+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done1 ;if the value in a is not zero
	inc Count1s+1


Inc_Done1:
	mov a, Count1s+0
	cjne a, #low(16), Timer4_ISR_done ; Warning: this instruction changes the carry flag! //compare and jump if not equal
	mov a, Count1s+1
	cjne a, #high(16), Timer4_ISR_done 
	
	setb ms_seconds_flag 
	
		; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1s+0, a
	mov Count1s+1, a
	
	jb sw_start_flag, Timer4_ISR_done 
	  	
	mov a, BCD_milisec_swcounter
	add a, #0x01
	da a
	mov BCD_milisec_swcounter, a
	mov a, BCD_milisec_swcounter
	cjne a, #100, Timer4_ISR_done
	mov BCD_milisec_swcounter, #0x00 ; if stopwatch is at 100 milliseconds, reset to zero and increment seconds 
 

 
 	mov a, BCD_sec_swcounter
	add a, #0x01
	da a
	mov BCD_sec_swcounter, a
	mov a, BCD_sec_swcounter
	cjne a, #0x60, Timer4_ISR_done
	mov BCD_sec_swcounter, #0x00 ; if the clock is at 60 seconds, set back to zero and update minutes
	
	mov a, BCD_min_swcounter
	add a, #0x01
	da a
	mov BCD_min_swcounter, a
	mov a, BCD_min_swcounter  
	cjne a, #0x60, Timer4_ISR_done ; if the clock is at 60 min, set back to zero
	mov BCD_min_swcounter, #0x00

		
Timer4_ISR_done:
	pop psw
	pop acc
		

	reti

;---------------------------------;
; Routine to initialize timer 2   ;
;---------------------------------;
Timer2_Init:
	orl CKCON0, #0b00010000 ; Timer 2 uses the system clock
	mov TMR2CN0, #0 ; Stop timer/counter.  Autoreload mode.
	mov TMR2H, #high(TIMER2_RELOAD)
	mov TMR2L, #low(TIMER2_RELOAD)
	; Set the reload value
	mov TMR2RLH, #high(TIMER2_RELOAD)
	mov TMR2RLL, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
    ;setb pt2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2H  ; Timer 2 doesn't clear TF2H automatically. Do it in ISR

	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done2 ;if the value in a is not zero
	inc Count1ms+1


Inc_Done2:
	; Check if a second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag! //compare and jump if not equal
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 1 second has passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know a second had passed
	
	
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	
	; Increment the BCD counter for seconds, minutes, hour and AM/PM indication
	mov a, BCD_sec_counter
	add a, #0x01
	da a
	mov BCD_sec_counter, a
	mov a, BCD_sec_counter
	cjne a, #0x60, Timer2_ISR_done
	mov BCD_sec_counter, #0x00 ; if the clock is at 60 seconds, set back to zero and update minutes
 	setb stop_alarm_flag ;reset alarm flag
	
	mov a, BCD_min_counter
	add a, #0x01
	da a
	mov BCD_min_counter, a
	mov a, BCD_min_counter  
	cjne a, #0x60, Timer2_ISR_done ; if the clock is at 60 min, set back to zero and update hours
	mov BCD_min_counter, #0x00

	

	mov a, BCD_hr_counter
	add a, #0x01
	da a
	mov BCD_hr_counter, a
	mov a, BCD_hr_counter
	cjne a, #0x13, Timer2_ISR_done ; if the clock is at 13 hour, set back to 1 and compliment AM/PM flag
	mov BCD_hr_counter, #0x01

	
	cpl AMPM_flag
	


		
Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------;
; Hardware initialization         ;
;---------------------------------;
Initialize_All:
    ; DISABLE WDT: provide Watchdog disable keys
	mov	WDTCN,#0xDE ; First key
	mov	WDTCN,#0xAD ; Second key

    ; Enable crossbar and weak pull-ups
	mov	XBR0,#0x00
	mov	XBR1,#0x00
	mov	XBR2,#0x40

	mov	P2MDOUT,#0x02 ; make sound output pin (P2.1) push-pull
	
	; Switch clock to 24 MHz
	mov	CLKSEL, #0x00 ; 
	mov	CLKSEL, #0x00 ; Second write to CLKSEL is required according to the user manual (page 77)
	
	; Wait for 24 MHz clock to stabilze by checking bit DIVRDY in CLKSEL
waitclockstable:
	mov a, CLKSEL
	jnb acc.7, waitclockstable 

	; Initialize the two timers used in this program
    lcall Timer0_Init
    lcall Timer2_Init
    lcall Timer4_Init

    lcall LCD_4BIT ; Initialize LCD
    
    setb EA   ; Enable Global interrupts

	ret



;---------------------------------;
; Function to check_alarm         ;
;---------------------------------;

check_alarm:


	clr c
	
	mov a, alarm_min
	subb a, BCD_min_counter ;check if the minutes match
	jnz exit_check
	
	mov a, alarm_hr
	subb a, BCD_hr_counter ;check if the hours match
	jnz exit_check
						    ;check if it is the same AM/ PM
						    
	JB AMPM_flag, check_one ;if AMPM flag is one
    LJMP check_zero
check_one:
    JB AMPM_alarm_flag, next ;if also one, it is match
    LJMP exit_check
check_zero:
	jnb AMPM_alarm_flag, next ;if also zero, it is match 
    LJMP exit_check
       
next:
	jnb stop_alarm_flag, exit_check ;if zero, it indicates the alarm was pressed


alarm:
	cpl is_alarm_flag
	setb TR0  
	setb SOUND_OUT

exit_check:
	ret
;---------------------------------;
; Function to stop alarm          ;
;---------------------------------;
to_stop_alarm:	
	jb BOOT_BUTTON, exit  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, exit  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr stop_alarm_flag
	cpl is_alarm_flag
	clr Tr0
	cpl SOUND_OUT
exit:
	ret
;---------------------------------;
; Function to set minute for ALARM;
;---------------------------------;
to_set_min_alarm:

	jb SET_MIN, exit_min_alarm
	Wait_Milli_Seconds(#50)
	jb SET_MIN, exit_min_alarm
	jnb SET_MIN, $	

		
	mov a, alarm_min
	add a, #0x01
	da a
	mov alarm_min, a
	mov a, alarm_min  
	cjne a, #0x60, skip_min_alarm; if the clock is at 60 min, set back to zero and update hours
	mov alarm_min, #0x00
skip_min_alarm:
	Set_Cursor(1, 4)      
	Display_BCD(alarm_min)
	ljmp to_set_min_alarm

exit_min_alarm:
	ret
;---------------------------------;
; Function to set hour for ALARM  ;
;---------------------------------;
to_set_hr_alarm:	
	jb SET_HR, exit_hr_alarm 
	Wait_Milli_Seconds(#50)
	jb SET_HR, exit_hr_alarm  
	jnb SET_HR, $		

	mov a, alarm_hr
	add a, #0x01
	da a
	mov alarm_hr, a
	mov a, alarm_hr  
	cjne a, #0x13, skip_hr_alarm ; if the clock is at 12 hr, set back to zero and update hours
	mov alarm_hr, #0x01
	Set_Cursor(1, 10) 
	cpl AMPM_alarm_flag
	jnb AMPM_alarm_flag, set_PMloop_alarm     
    Send_Constant_String(#AM)
    ljmp skip_hr_alarm
set_PMloop_alarm:
    Send_Constant_String(#PM)	
  
skip_hr_alarm:
	Set_Cursor(1, 1)      
	Display_BCD(alarm_hr)
	ljmp to_set_hr_alarm

exit_hr_alarm:
	ret
	
;---------------------------------;
; Function to set minute for CLOCK;
;---------------------------------;
to_set_min:

	jb SET_MIN, exit_min
	Wait_Milli_Seconds(#50)
	jb SET_MIN, exit_min
	jnb SET_MIN, $	
	
	mov a, BCD_min_counter
	add a, #0x01
	da a
	mov BCD_min_counter, a
	mov a, BCD_min_counter  
	cjne a, #0x60, skip_min; if the clock is at 60 min, set back to zero and update hours
	mov BCD_min_counter, #0x00
skip_min:
	Set_Cursor(1, 4)      
	Display_BCD(BCD_min_counter)
	ljmp to_set_min

exit_min:
	ret
;---------------------------------;
; Function to set hour for CLOCK  ;
;---------------------------------;
to_set_hr:	
	jb SET_HR, exit_hr 
	Wait_Milli_Seconds(#50)
	jb SET_HR, exit_hr  
	jnb SET_HR, $		
	
	mov a, BCD_hr_counter
	add a, #0x01
	da a
	mov BCD_hr_counter, a
	mov a, BCD_hr_counter  
	cjne a, #0x13, skip_hr ; if the clock is at 12 hr, set back to zero and update hours
	mov BCD_hr_counter, #0x01
	Set_Cursor(1, 10) 
	cpl AMPM_flag
	jnb AMPM_flag, set_PMloop     
    Send_Constant_String(#AM)
    ljmp skip_hr

set_PMloop:
    Send_Constant_String(#PM)	
  
skip_hr:
	Set_Cursor(1, 1)      
	Display_BCD(BCD_hr_counter)
	ljmp to_set_hr

exit_hr:
	ret
;---------------------------------;
; Function to set time on Clock   ;
;---------------------------------;

to_set_time:
	jb SET_TIME, end_set_time   
	Wait_Milli_Seconds(#50)	
	jb SET_TIME, end_set_time   ; check if button is held down
	

	setb set_time_flag
	;stop alarm
	clr Tr0
	clr SOUND_OUT
	
	;set time
	clr TR2  ; Stop timer 2	
	
	lcall to_set_hr
	lcall to_set_min
	
	ljmp to_set_time
	
end_set_time:
	jnb set_time_flag, skipSetTime 	
	clr a
	clr set_time_flag
	mov Count1ms+0, a
	mov Count1ms+1, a	
	setb TR2                ; Start timer 2

skipSetTime:
	ret	

;---------------------------------;
; Function to set alarm          ;
;---------------------------------;

to_set_alarm:
	jb SET_ALARM, end_set_alarm 
	Wait_Milli_Seconds(#50)	
	jb SET_ALARM, end_set_alarm  	; check if button is held down
	

	Set_Cursor(1, 4)      
	Display_BCD(alarm_min)
	Set_Cursor(1, 1)      
	Display_BCD(alarm_hr)

	setb set_alarm_flag
	;stop alarm
	clr Tr0
	clr SOUND_OUT
	
	
	lcall to_set_hr_alarm
	lcall to_set_min_alarm
	
	ljmp to_set_alarm
	
end_set_alarm:
	jnb set_alarm_flag, skipSetAlarm 	
	clr a
	clr set_alarm_flag
	

skipSetAlarm:
	ret	
;---------------------------------;
; stopwatch mode                  ;
;---------------------------------;
stopwatch_mode:

	lcall check_alarm

loop_a_sw:

	jnb ms_seconds_flag, next_sw
	  
loop_b_sw:
    clr ms_seconds_flag
    Set_Cursor(1, 1)
    Send_Constant_String(#stopwatch_mode_init)
	Set_Cursor(1, 7)      
	Display_BCD(BCD_milisec_swcounter)
	Set_Cursor(1, 4)      
	Display_BCD(BCD_sec_swcounter)
	Set_Cursor(1, 1)      
	Display_BCD(BCD_min_swcounter)
 
next_sw:
 	jb SW_START, restart_sw  
 	Wait_Milli_Seconds(#50)
	jb SW_START, restart_sw  
	jnb SW_START, $	; check if button is pressed
	
	cpl sw_start_flag ;stop or continue/start the stopwatch

restart_sw:	
	jb SW_END, loop_sw_mode  
	Wait_Milli_Seconds(#50)	
	jb SW_END, loop_sw_mode  
	jnb SW_END, $		; check if button is pressed
	

	setb sw_start_flag ;stop timer 4
	mov BCD_milisec_swcounter, #0x00 ;restart the stopwatch to 00:00.0
	mov BCD_sec_swcounter, #0x00
	mov BCD_min_swcounter, #0x00
	Set_Cursor(1, 1)
    Send_Constant_String(#stopwatch_mode_init)
  loop_sw_mode:  
	ret

	
;---------------------------------;
; set timer screen                ;
;---------------------------------;
timer_screen:	
	Set_Cursor(1, 1)
    Send_Constant_String(#clock_mode_init)
	lcall check_alarm
    clr seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 7)      
	Display_BCD(BCD_sec_counter)
	Set_Cursor(1, 4)      
	Display_BCD(BCD_min_counter)
	Set_Cursor(1, 1)      
	Display_BCD(BCD_hr_counter)
	Set_Cursor(1, 10)  
	jnb AMPM_flag, PMloop     
    Send_Constant_String(#AM)
    ljmp exit_timer_screen
PMloop:
    Send_Constant_String(#PM)

exit_timer_screen:
	ret
	
;---------------------------------;
; timer mode                      ;
;---------------------------------;

clock_mode:

	lcall to_stop_alarm	
	lcall to_set_time
	lcall to_set_alarm
 
loop_a:
	jnb seconds_flag, loop_clock_mode 

loop_b:
	lcall timer_screen

loop_clock_mode:

    ret	

;---------------------------------;
; Main program.                   ;
;---------------------------------;
main:
	; Setup the stack start to the begining of memory only accesible with pointers
    mov SP, #7FH
    
	lcall Initialize_All
	
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':

    setb seconds_flag
    setb ms_seconds_flag
    setb AMPM_flag
    setb AMPM_alarm_flag
    setb stop_alarm_flag
    setb is_alarm_flag
    setb sw_start_flag
    setb clk_mode
	mov BCD_sec_counter, #0x00
	mov BCD_min_counter, #0x00
	mov BCD_hr_counter, #0x01
	mov alarm_min, #0x00
	mov alarm_hr, #0x00
	mov BCD_milisec_swcounter, #0x00
	mov BCD_sec_swcounter, #0x00
	mov BCD_min_swcounter, #0x00
	Set_Cursor(1, 1)
    Send_Constant_String(#clock_mode_init)
   
	
	; After initialization the program stays in this 'forever' loop
loop:
	jb SET_MODE, stay_mode  
	Wait_Milli_Seconds(#50)	
	jb SET_MODE, stay_mode  
	jnb SET_MODE, $		
	
	cpl clk_mode ;toggles the mode of clock

stay_mode:
	jnb clk_mode, to_stopwatch_mode

	lcall clock_mode
	ljmp loop
	
to_stopwatch_mode: 
	lcall stopwatch_mode

    ljmp loop
END
