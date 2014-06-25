TITLE Program 7 - Smiley Two			(P7PAUL.asm)
; Program Description:Program 7 - Smiley Two
; Author: Paul A. Mould
; Date Created: November 11, 2011
; Last Modification Date:November 21, 2011
INCLUDE Irvine32.inc

setCursor MACRO r,c                  ;sets the cursor to row and column speified, r = row and c = column 
	push dx                    		 ;pushes dx on to stack
	mov dh,r                 		 ; set row
	mov dl,c                  		 ; set column
	call gotoXY                		 ; set cursor
	pop dx					 		 ;pop dx from stack
	ENDM                             ; end Macro
		  
setSmiley MACRO r,c                  ; Displays the smiley on the console window, r = row and c = column
	setCursor r,c      			     ;Set cursor for Smiley
	mov al, 1              
	call WriteChar		 		     ;Put Smiley on screen	
	ENDM                  		     ; end Macro
	
copy MACRO a,b						 ;Copies data from variable a to into variable b
	ECHO Expanding the 1
	push eax              
	mov al,a              		     ;AL=a
	mov b,al		      		     ;b=a
	pop eax 
	ENDM                       ; end Macro
	
insetSpace MACRO r,c  		         ;inserts a space into the back String variable at the row and column specified by r and c               
	push cx
	push dx
	push esi
  	mov cl, r
	mov dl, c
	call getOffset         			 ;returns the offset of a character on the screen from the back string variable
	mov cl," "
	mov back[esi], cl
	pop cx
	pop dx
	pop esi
	ENDM                       ; end Macro
	
	setColor MACRO tColor, bColor 	 ;sets the background color to tColor and foreground color to bColor 
	push eax
	mov eax, tColor+bColor*16 
	call setTextColor
	pop eax
	ENDM                       ; end Macro
	
	writeStringAt MACRO r,c, String	 ; Writes a certain String variable, String at a certain row,r and column, c
		ECHO Expanding the 2
	push eax
	setCursor r,c
	mov edx , OFFSET String
	call WriteString
	pop eax
	ENDM                       ; end Macro
	
	writeDecAt MACRO r,c, Value		 ; Writes a certain Decimal variable, Value at a certain row,r and column, c
	push eax
	setCursor r,c
	mov eax , Value
	call WriteDec
	pop eax
	ENDM                       ; end Macro
	
	checkWall MACRO; checks if an "*" is in the position the smiley wants to move to, if it is Smiley does'nt proceed to move    
		ECHO Expanding the 3
	call getOffset         			 ;returns the offset of a character on the screen from the back string variable 
	cmp back[esi], "*"  ; compares whether the position where the user wants to move the smily contains a space or not
	je TOP
   ENDM                       ; end Macro
   
.data
back    db      "***************************************  ***************************************"
        db      "* RED SCORE: 000*          *          *  *             *      * BLUE SCORE: 000*"
        db      "***********************    *     ******  ***********   *    ********************"
        db      "*. . . . . . . . . . . . . *     ...................   * . . . . . . . . . . . *"
        db      "****  . . . . . . . . . . .*     ...................   *  . . . . . . . . . ****"
        db      "                           *     ...................   *                        "
        db      "**** . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .****"
        db      "*.          *********************************************************         .*"
        db      "*.                                                                            .*"
        db      "*.                               ******** ********                            .*"
        db      "*.                         *     *...............*     *                      .*"
        db      "*.                         *     *...............*     *                      .*"
        db      "*.                         *     *...............*     *                      .*"
        db      "*.                         *     *...............*     *                      .*"
        db      "*.                         *     *...............*     *                      .*"
        db      "*.                               *...............*                            .*"
        db      "*.          *********************************************************         .*"
        db      "*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . *"
        db      "*. . . . . . . . . . . . . * . . . . . . . . . . . . . *. . . . . . . . . . . .*"
        db      "*. . . . . . . . . . . . . * . . . . . . . . . . . . . * . . . . . . . . . . . *"
        db      "****...................    *  ......................   *  ..................****"
        db      "    ...................    *   *   *       *     *     *  ..................    "
        db      "****...................    *   * * *  *  * *  *  * *   *  ..................****"
        db      "*......................          *    *  *    *    *      .....................*"
        db      "***************************************  **************************************",0 ;Strign variable to display the game screen on the console window
blankSpace db 4000 DUP(" ")          ; String variable that stores 4000 blank spaces that can be used to clear the screen
rowRed    db 12             		 ; variable for row number Red smiley is on
colRed     db 12			 		 ; variable for column number Red smiley is on
rowBlue    db 12             		 ; variable for row number Blue smiley is on
colBlue    db 66			   		 ; variable for column number Blue smiley is on
nextRowRed db 0              		 ; variable that stores the row number of the row that the user wants to move to
nextColRed db 0 			 		 ; variable that stores the column number of the column that the user wants to move to
nextRowBlue db 0               		 ; variable that stores the row number of the row that the user wants to move to
nextColBlue db 0 			   		 ; variable that stores the column number of the column that the user wants to move to

num80     db 80           			 ; variable that stores the integer 80( total number of columns)
num16     db 16			   			 ; variable that stores the integer 16 ( total number of rows)
redCounter dd 0						 ;variable that holds how many dots the red smiley eats
blueCounter dd 0					 ;variable that holds how many dots the blue smiley eats
WinnerRed db  "GAME OVER: RED SMILEY WINS!!!",0      ; String variable to dislay if Red Smiley wins
WinnerBlue db "GAME OVER: BLUE SMILEY WINS!!!",0     ; String variable to dislay if Blue Smiley wins
Draw   db "THE GAME ENDED IN A DRAW!!!",0            ; String variable to display if it is a Draw
quit   db "GAME QUIT!!!!!"							 ; String variable to be displayed if "Q" or "q" is pressed

.code

main PROC                            ;START OF MAIN PROCEDURE
setColor black, brown 				 ; Sets the color to blue on brown  
call ClrScr				  			 ; Clears the screen
writeStringAt 0,0, back              ;  Writes the String Pattern onto the console window

TOP:								 ;LABEL WHERE THE PROGRAM RESTARTS AFTER EACH ATTEMPTED MOVE
	setColor red, brown        		 ; Sets the color to red text on green background
	setSmiley rowRed,colRed    		 ; Displays red Smiley on screen
	setColor blue, brown       		 ; sets color to blue text on brown background
	setSmiley rowBlue,colBlue  		 ; Displays blue Smiley on screen
  CHECKWINNER:               		 ; LABEL TO START CHECK FOR A WINNER OF THE PROGARM
    cmp redCounter, 269        	   	 ; check to see if red smiley has eaten 269 dots
	je BOTTOM                  		 ; if its equal it jumps to end the program
	cmp blueCounter, 269       		 ; else it checks if blue smiley has eated 269 dots
	je BOTTOM                  		 ; if its equal it jumps to end the program
	cmp redCounter, 268        		 ; check to see if red smiley has eaten 268 dots
	jne NOTDRAW               		 ; if it is not equal it jumps to stop checking for a draw 
	cmp blueCounter, 268      		 ; else it checks to see whetehr the blue smiley has eaten 268 dots   
	je BOTTOM				  		 ; if its equal it jumps to end the program
  NOTDRAW:                  		 ;THE PROGRAM CONTINUES ON IF THERE IS NO WINNER AND IT IS NOT A DRAW
	call ReadChar           		 ;Reads a character inputted by the user
	cmp al, 'q'              
	je BOTTOM              			 ; If "q" is pressed the program jump to the end of the program
	cmp al, 'Q'             
	je BOTTOM						 ; If "q" is pressed the program jump to the end of the program
	cmp al, 0
	je REDARROWACTIONS           	 ; If an arrow key is pressed the program jumps to RedArrowActions 
	cmp al, 32h   
	je BLUEARROWACTIONS          	 ; when  "2" in the numeric keypad is pressed it jumps to BlueArrowActions 
	cmp al, 38h
	je BLUEARROWACTIONS           	 ; when  "8" in the numeric keypad is pressed it jumps to BlueArrowActions
	cmp al, 34h
	je BLUEARROWACTIONS          	 ; when  "4" in the numeric keypad is pressed it jumps to BlueArrowActions	
	cmp al, 36h
	je BLUEARROWACTIONS           	 ; when  "6" in the numeric keypad is pressed it jumps to BlueArrowActions
    jmp TOP

REDARROWACTIONS:                  	 ; WHEN THE ARROW KEYS ARE PRESSED THE CODE MOVES TO THIS LABEL                
   setCursor rowRed, colRED 	  	 ;Set cursor to the Red Smiley 
	push eax
	mov al, ' ' 				 	 ;Erase Red Smiley from screen 
	call WriteChar         	     	 ;Erase Smiley from screen
    insetSpace rowRed, colRed    	 ;inserts a space CHARACTER (" ") into the back String variable at ROW, rowRed and COLUMN, colRed
	pop eax
	cmp ah, 50h                   	 ; down arrow key pressed
	je DOWN               			 ; jumps to DOWN
	cmp ah, 48h           			 ; up arrow key pressed
	je UP                  			 ; jumps to UP
	cmp ah, 4Bh           			 ; left arrow key pressed
	je LEFT               			 ; jumps to LEFT
	cmp ah, 4Dh           			 ; right arrow key pressed
	je RIGHT               			 ; jumps to RIGHT
	
DOWN:							     ; WHEN THE DOWN ARROW KEY IS PRESSED THE CODE MOVES TO THIS LABEL 
	cmp rowRed, 24					 ;CHECKS TO SEE IF USER WANTS TO MOVE SMILEY INTO A BOTTOM "DOORWAY"
	jne END4        	;, if so it should display the smiley at the opposite doorway (bottom "doorway")
	cmp colRed, 39
	jne COL40
	mov rowRed, 0                    ; moves the row of the smiley to 0    
	jmp UPDATEREDSCORE               ; JUMPS TO THE CODE TO UPDATE THE RED SCORE COUNTER ON THE SCREEN 
  COL40:
	cmp colRed,40
	jne END4
	mov rowRed, 0			         ; moves the row of the smiley to 0 
	jmp UPDATEREDSCORE               ; JUMPS TO THE CODE TO UPDATE THE RED SCORE COUNTER ON THE SCREEN
  END4:
	copy colRed, nextColRed     	 ;Sets the nextColRed variable to the colRed variable
	copy rowRed,nextRowRed			 ;Sets the nextRowRed variable to the rowRed variable
	inc nextRowRed              	 ; increment nextRowRed by 1 to set it to the row the smiley wants to move to 
	mov dl, nextColRed       	     ;stores the nextRow variable in the dl register
	mov cl, nextRowRed       	     ;stores the nextRow variable in the cl register	
	checkWall           ;CHECKS FOR AN "*" IN THE POSITION THE SMILEY WANTS TO MOVE TO AND JUMPS TO TOP IF TRUE
	cmp cl, RowBlue     ; CHECKS IF THE RED SMILEY IS MOVING TO WHERE THE BLUE SMILEY IS, IF TRUE IT JUMPS TO TOP
	jne NOTEQUAL
	mov eax, 1
	cmp dl, colBlue    
	jne endof
	inc eax
	endof:
	cmp eax, 2			; if eax = 2 then RED smiley wants to move to BLUE smiley location therefore we dont move
	je TOP
  NOTEQUAL:
	inc rowRed          ; if the desired position contains a space then decrement the row to move the smiley up
	jmp UPDATEREDSCORE       		 ; JUMPS TO THE CODE TO UPDATE THE RED SCORE COUNTER ON THE SCREEN
	
UP:							         ; WHEN THE UP ARROW KEY IS PRESSED THE CODE MOVES TO THIS LABEL 
	cmp rowRed, 0			 		 ;CHECKS TO SEE IF USER WANTS TO MOVE SMILEY INTO A TOP "DOORWAY"
	jne END3			;, if so it should display the smiley at the opposite doorway (Upper "doorway")
	cmp colRed, 39
	jne COL40??
	mov rowRed, 24            		 ; moves the row of the smiley to 24 
	jmp UPDATEREDSCORE       		 ; JUMPS TO THE CODE TO UPDATE THE RED SCORE COUNTER ON THE SCREEN
  COL40??:
	cmp colRed,40
	jne END2
	mov rowRed, 24            		 ; moves the row of the smiley to 24 
	jmp UPDATEREDSCORE       		 ; JUMPS TO THE CODE TO UPDATE THE RED SCORE COUNTER ON THE SCREEN
  END3:
	copy colRed, nextColRed			 ;Sets the nextColRed variable to the colRed variable
	mov dl, nextColRed        		 ;stores the nextRow variable in the dl register
	copy rowRed,nextRowRed			 ;Sets the nextRow variable to the next row above
	dec nextRowRed
	mov cl, nextRowRed      		 ;stores the nextRow variable in the cl register
	checkWall			;CHECKS FOR AN "*" IN THE POSITION THE SMILEY WANTS TO MOVE TO AND JUMPS TO TOP IF TRUE
	cmp cl, RowBlue		; CHECKS IF THE RED SMILEY IS MOVING TO WHERE THE BLUE SMILEY IS, IF TRUE IT JUMPS TO TOP
	jne ISCOLEQUAL0
	mov eax, 1
  ISCOLEQUAL0:
	cmp dl, colBlue
	jne ENDOFF
	inc eax
  ENDOFF:
	cmp eax, 2		    ; if eax = 2 then red smiley wants to move to BLUE smiley location therefor we dont move
	je INVALID2
	dec rowRed          ; if the desired position contains a space then decrement the row to move the smiley up
  INVALID2:
	jmp UPDATEREDSCORE               ; JUMPS TO THE CODE TO UPDATE THE RED SCORE COUNTER ON THE SCREEN
	
LEFT:							     ; WHEN THE LEFT ARROW KEY IS PRESSED THE CODE MOVES TO THIS LABEL	
	cmp colRed, 0			         ;CHECKS TO SEE IF USER WANTS TO MOVE SMILEY INTO A LEFT "DOORWAY"
	jne END2			;, if so it should display the smiley at the opposite doorway (Left "doorway")
	cmp rowRed, 5
	jne COL21??
	mov colRed, 79                   ; moves the column of the smiley to 79 
	jmp UPDATEREDSCORE               ; JUMPS TO THE CODE TO UPDATE THE RED SCORE COUNTER ON THE SCREEN
  COL21??:
	cmp rowRed,21
	jne END2
	mov colRed, 79             		 ; moves the column of the smiley to 79
	jmp UPDATEREDSCORE               ; JUMPS TO THE CODE TO UPDATE THE RED SCORE COUNTER ON THE SCREEN
  END2:
	copy colRed, nextColRed		     ;Sets the nextCol variable to the next column on the left			
	dec nextColRed
	mov dl, nextColRed					
	copy rowRed,nextRowRed			 ;Sets the nextRow variable to the row that the user wants to move to
	mov cl, nextRowRed               ;stores the nextRow variable in the cl register
	checkWall 			;CHECKS FOR AN "*" IN THE POSITION THE SMILEY WANTS TO MOVE TO AND JUMPS TO TOP IF TRUE
	cmp cl, RowBlue		; CHECKS IF THE RED SMILEY IS MOVING TO WHERE THE BLUE SMILEY IS, IF TRUE IT JUMPS TO TOP
	jne ISCOLEQUAL1
	mov eax, 1
  ISCOLEQUAL1:
	cmp dl, colBlue
	jne ENDOFG
	inc eax
  ENDOFG:
	cmp eax, 2			; if eax = 2 then red smiley wants to move to BUE smiley location therefor we dont move
	je INVALID3
    dec colRed          ; if the desired position contains a space then decrement the column to move the smiley left
  INVALID3:
	jmp UPDATEREDSCORE     			 ; JUMPS TO THE CODE TO UPDATE THE RED SCORE COUNTER ON THE SCREEN
	
RIGHT:							     ; WHEN THE RIGHT ARROW KEY IS PRESSED THE CODE MOVES TO THIS LABEL
	cmp colRed, 79					 ;CHECKS TO SEE IF USER WANTS TO MOVE SMILEY INTO A RIGHT "DOORWAY"
	jne END1			;, if so it should display the smiley at the opposite doorway (Right "doorway")
	cmp rowRed, 5
	jne COL21?
	mov colRed, 0             		 ; moves the column of the smiley to 0
	jmp UPDATEREDSCORE      		 ; JUMPS TO THE CODE TO UPDATE THE RED SCORE COUNTER ON THE SCREEN
  COL21?:
	cmp rowRed,21
	jne END1
	mov colRed, 0           		 ; moves the column of the smiley to 0
	jmp UPDATEREDSCORE      		 ; JUMPS TO THE CODE TO UPDATE THE RED SCORE COUNTER ON THE SCREEN
  END1:
	copy colRed, nextColRed			 ;Sets the nextCol variable to the next column on the right
	inc nextColRed
	mov dl, nextColRed
	copy rowRed,nextRowRed			 ;Sets the nextRow variable to the row that the user wants to move to
	mov cl, nextRowRed      		 ;stores the nextRow variable in the cl register
	checkWall			;CHECKS FOR AN "*" IN THE POSITION THE SMILEY WANTS TO MOVE TO AND JUMPS TO TOP IF TRUE
	cmp cl, RowBlue		; CHECKS IF THE RED SMILEY IS MOVING TO WHERE THE BLUE SMILEY IS, IF TRUE IT JUMPS TO TOP
	jne ISCOLEQUAL2
	mov eax, 1
  ISCOLEQUAL2:
	cmp dl, colBlue
	jne ENDOFH
	inc eax
  ENDOFH:
	cmp eax, 2		    ; if eax = 2 then red smiley wants to move to BLUE smiley location therefor we dont move
	je INVALID4
	inc colRed          ; if the desired position contains a space then increment the column to move the smiley right
  INVALID4:
	jmp UPDATEREDSCORE      		 ; JUMPS TO THE CODE TO UPDATE THE RED SCORE COUNTER ON THE SCREEN

BLUEARROWACTIONS:                  	 ; WHEN THE ARRPW KEYS IN THE NUMERIC KEYPAD ARE PRESSED THE CODE MOVES TO THIS LABEL 
    setCursor rowBlue, colBlue 		 ;Set cursor for Smiley
    push eax
	mov al, ' '     
	call WriteChar        			 ;Erase Smiley ofrom screen
	insetSpace rowBlue, colBlue      ;inserts a space into the back String variable at the rowBlue and colBlue 
	pop eax
	cmp al, 32h
	je DOWNBLUE          			 ; when  "2" in the numeric keypad is pressed it jumps to DownBlue Label
	cmp al, 38h
	je UPBLUE          			     ; when  "8" in the numeric keypad is pressed it jumps to UpBlue Label
	cmp al, 34h
	je LEFTBLUE          			 ; when  "6" in the numeric keypad is pressed it jumps to leftBlue Label
	cmp al, 36h
	je RIGHTBLUE          			 ; when  "8" in the numeric keypad is pressed it jumps to rightBlue Label
	
DOWNBLUE:							 ; WHEN THE "2" KEY FORM THE NUERIC KEYPAD IS PRESSED THE CODE MOVES TO THIS LABEL
	cmp rowBlue, 24			 		 ;CHECKS TO SEE IF USER WANTS TO MOVE SMILEY INTO A BOTTOM "DOORWAY"
	jne END4B	        ;, if so it should display the smiley at the opposite doorway (Bottom "doorway")
	cmp colBlue, 39
	jne COL40?B
	mov rowBlue, 0            		 ; moves the row of the smiley to 0
	jmp UPDATEBLUESCORE 			 ; JUMPS TO THE CODE TO UPDATE THE BLUE SCORE COUNTER ON THE SCREEN
  COL40?B:
	cmp colBlue,40
	jne END4B
	mov rowBlue, 0           		 ; moves the row of the smiley to 0
	jmp UPDATEBLUESCORE 			 ; JUMPS TO THE CODE TO UPDATE THE BLUE SCORE COUNTER ON THE SCREEN
  END4B:
	copy colBlue, nextColBlue		 ;Sets the nextCol variable to the column that the user wants to move to
	copy rowBlue,nextRowBlue		 ;Sets the nextRow variable to the next row below
	inc nextRowBlue     
	mov dl, nextColBlue     		 ;stores the nextRow variable in the dl register
	mov cl, nextRowBlue   			 ;stores the nextRow variable in the cl register
	checkWall		    ;CHECKS FOR AN "*" IN THE POSITION THE SMILEY WANTS TO MOVE TO AND JUMPS TO TOP IF TRUE
	cmp cl, RowRed	    ; CHECKS IF THE BLUE SMILEY IS MOVING TO WHERE THE RED SMILEY IS, IF TRUE IT JUMPS TO TOP
	jne ISCOLEQUALB
	mov eax, 1
  ISCOLEQUALB:
	cmp dl, colRed
	jne endofeb
	inc eax
	endofeb:
	cmp eax, 2          ; if eax = 2 then BLUE smiley wants to move to BLUE smiley location thereforE we dont move
	je INVALID1B
	inc rowBlue         ; if the desired position contains a space then increment the row to move the smiley down
  INVALID1B:
	jmp UPDATEBLUESCORE 			 ; JUMPS TO THE CODE TO UPDATE THE BLUE SCORE COUNTER ON THE SCREEN
	
UPBLUE:							     ; WHEN THE "8" KEY FORM THE NUERIC KEYPAD IS PRESSED THE CODE MOVES TO THIS LABEL
	cmp rowBlue, 0			         ;CHECKS TO SEE IF USER WANTS TO MOVE SMILEY INTO A UPPER "DOORWAY"
	jne END3B			;, if so it should display the smiley at the opposite doorway (Upper "doorway")
	cmp colBlue, 39
	jne COL40??B
	mov rowRed, 24            ; moves the row of the smiley to 24
	jmp UPDATEBLUESCORE 			 ; jumps to the code to update the Blue Score Counter on the screen
  COL40??B:
	cmp colBlue,40
	jne END2B
	mov rowBlue, 24                  ; moves the row of the smiley to 24
	jmp UPDATEBLUESCORE 			 ; JUMPS TO THE CODE TO UPDATE THE BLUE SCORE COUNTER ON THE SCREEN
  END3B:
	copy colBlue, nextColBlue		 ;Sets the nextCol variable to the column that the user wants to move to
	mov dl, nextColBlue    			 ;stores the nextRow variable in the dl register
	copy rowBlue,nextRowBlue		 ;Sets the nextRow variable to the next row above
	dec nextRowBlue
	mov cl, nextRowBlue     		 ;stores the nextRow variable in the cl register
	checkWall		    ;CHECKS FOR AN "*" IN THE POSITION THE SMILEY WANTS TO MOVE TO AND JUMPS TO TOP IF TRUE
	cmp cl, RowRed	    ; CHECKS IF THE BLUE SMILEY IS MOVING TO WHERE THE RED SMILEY IS, IF TRUE IT JUMPS TO TOP
	jne ISCOLEQUAL0B
	mov eax, 1
  ISCOLEQUAL0B:
	cmp dl, colRed
	jne ENDOFFB
	inc eax
  ENDOFFB:
	cmp eax, 2		 	; if eax = 2 then BLUE smiley wants to move to red smiley location therefor we dont move
	je INVALID1B
	dec rowBlue       	; if the desired position contains a space then decrement the row to move the smiley up
  INVALID2B:
	jmp UPDATEBLUESCORE 			 ; JUMPS TO THE CODE TO UPDATE THE BLUE SCORE COUNTER ON THE SCREEN
	
LEFTBLUE:							 ; WHEN THE "4" KEY FORM THE NUERIC KEYPAD IS PRESSED THE CODE MOVES TO THIS LABEL
	cmp colBlue, 0			         ;CHECKS TO SEE IF USER WANTS TO MOVE SMILEY INTO A LEFT "DOORWAY"
	jne END2B			;, if so it should display the smiley at the opposite doorway (left "doorway")
	cmp rowBlue, 5
	jne COL21??B
	mov colBlue, 79                  ; moves the column of the smiley to 79
	jmp UPDATEBLUESCORE 			 ; JUMPS TO THE CODE TO UPDATE THE BLUE SCORE COUNTER ON THE SCREEN
  COL21??B:
	cmp rowBlue,21
	jne END2B
	mov colBlue, 79                  ; moves the column of the smiley to 79
	jmp UPDATEBLUESCORE 			 ; JUMPS TO THE CODE TO UPDATE THE BLUE SCORE COUNTER ON THE SCREEN
  END2B:
	copy colBlue, nextColBlue		 ;Sets the nextCol variable to the next column on the left
	dec nextColBlue
	mov dl, nextColBlue
	copy rowBlue,nextRowBlue		 ;Sets the nextRow variable to the row that the user wants to move to
	mov cl, nextRowBlue        		 ;stores the nextRow variable in the cl register
	checkWall 	   		;CHECKS FOR AN "*" IN THE POSITION THE SMILEY WANTS TO MOVE TO AND JUMPS TO TOP IF TRUE
	cmp cl, RowRed 		; CHECKS IF THE BLUE SMILEY IS MOVING TO WHERE THE RED SMILEY IS, IF TRUE IT JUMPS TO TOP
	jne ISCOLEQUAL1B
	mov eax, 1
  ISCOLEQUAL1B:
	cmp dl, colRed
	jne ENDOFGB
	inc eax
  ENDOFGB:
	cmp eax, 2	 		; if eax = 2 then BLUE smiley wants to move to red smiley location therefor we dont move
	je INVALID1B
    dec colBlue  		; if the desired position contains a space then decrement the column to move the smiley left
  INVALID3B:
	jmp UPDATEBLUESCORE 			 ; jumps to the code to update the Blue Score Counter on the screen
	
RIGHTBLUE:							 ; WHEN THE "6" KEY FORM THE NUERIC KEYPAD IS PRESSED THE CODE MOVES TO THIS LABEL
	cmp colBlue, 79					 ;CHECKS TO SEE IF USER WANTS TO MOVE SMILEY INTO A RIGHT "DOORWAY"
	jne END1B			;, if so it should display the smiley at the opposite doorway (Right "doorway")
	cmp rowBlue, 5
	jne COL21?B
	mov colBlue, 0            		 ; moves the column of the smiley to 0
	jmp UPDATEBLUESCORE 			 ; JUMPS TO THE CODE TO UPDATE THE BLUE SCORE COUNTER ON THE SCREEN
  COL21?B:
	cmp rowBlue,21
	jne END1B
	mov colBlue, 0           		 ; moves the column of the smiley to 0
	jmp UPDATEBLUESCORE 			 ; JUMPS TO THE CODE TO UPDATE THE BLUE SCORE COUNTER ON THE SCREEN
  END1B:
	copy colBlue, nextColBlue		 ;Sets the nextCol variable to the next column on the right
	inc nextColBlue
	mov dl, nextColBlue
	copy rowBlue,nextRowBlue		 ;Sets the nextRow variable to the row that the user wants to move to
	mov cl, nextRowBlue        		 ;stores the nextRow variable in the cl register
	checkWall      		;CHECKS FOR AN "*" IN THE POSITION THE SMILEY WANTS TO MOVE TO AND JUMPS TO TOP IF TRUE
	cmp cl, RowRed 		; CHECKS IF THE BLUE SMILEY IS MOVING TO WHERE THE RED SMILEY IS, IF TRUE IT JUMPS TO TOP
	jne ISCOLEQUAL2B
	mov eax, 1
  ISCOLEQUAL2B:
	cmp dl, colRed
	jne ENDOFHB
	inc eax
	ENDOFHB:
	cmp eax, 2	        ; if eax = 2 then BLUE smiley wants to move to red smiley location therefor we dont move
	je INVALID4B
	inc colBlue   		; if the desired position contains a space then increment the column to move the smiley right
  INVALID4B:
	jmp UPDATEBLUESCORE 			 ; JUMPS TO THE CODE TO UPDATE THE BLUE SCORE COUNTER ON THE SCREEN

UPDATEREDSCORE:					     ; LABEL THAT STARTS CODE TO UPDATE RED SCORE, DISPLAY IT, THEN MOVE TO THE TOP LABEL
	setColor red, brown            ; Sets the color to blue on brown
	mov cl, rowRed					 ;Sets the Red Score Counter when the red smiley eats a dot
	mov dl, colRed
   	call getOffset        			 ;returns the offset of a character on the screen from the back string variable 
	cmp back[esi], "."  ; compares whether the position where the user wants to move the smiley contains a space or not
	jne notDot          ; if it does not contain a space then it jumps to the INVALID1 
	mov edx, "r"					 ;Sets the Red Score Counter when the red smiley eats a dot
	call incScore
	cmp redCounter, 99
	jna LESS100
	writeDecAt 1,13, redCounter      ; Displays the Red Score Counter at the appropriate position on the screen
	jmp notDot
  LESS100:
	cmp redCounter, 9
	jna LESS10
	writeDecAt 1,14,redCounter      ; Displays the Red Score Counter at the appropriate position on the screen
	jmp notDot
  LESS10:
	writeDecAt 1,15,redcounter      ; Displays the Red Score Counter at the appropriate position on the screen
	notDot:
    jmp TOP

UPDATEBLUESCORE:					 ; LABEL THAT STARTS CODE TO UPDATE BLUE SCORE, DISPLAY IT, THEN MOVE TO THE TOP LABEL
	setColor blue, brown    		 ; Sets the color to blue on brown
	mov cl, rowBlue					 ;Sets the Blue Score Counter when the red smiley eats a dot
	mov dl, colBlue
   	call getOffset         			 ;returns the offset of a character on the screen from the back string variable 
	cmp back[esi], "."  ; compares whether the position where the user wants to move the smily contains a space or not
	jne NOTDOTB           			 ; if it does not contain a space then it jumps to the INVALID1 
	mov edx, "b"
	call incScore
	cmp blueCounter, 99
	jna LESS100B
	writeDecAt 1,76,blueCounter      ; Displays the Blue Score Counter at the appropriate position on the screen
	jmp NOTDOTB
  LESS100B:
	cmp blueCounter, 9
	jna LESS10B2
	writeDecAt 1,77,blueCounter      ; Displays the Blue Score Counter at the appropriate position on the screen 
	jmp NOTDOTB
  LESS10B2:
	writeDecAt 1,78,blueCounter      ; Displays the Blue Score Counter at the appropriate position on the screen
  NOTDOTB:
	jmp TOP 
	
BOTTOM:                              ; LABEL CODE MOVES TO WHEN GAME IS OVER
	setColor white, black            ; Set color to white characters on black background
	WriteStringAt 2,0, blankSpace    ; DISPLAYS BLACK SPACES BELOW THE ROW DISPLAYING THE RESLUTS OF THE GAME  
	setColor black, yellow   
	cmp redCounter, 269              ; THIS FOLLOWING CODE PRINTS OUT THE RESULTS OF THE JUST ENDED GAME
	je REDWINS1                      ; if equal it jumps to REDWINS1
	cmp blueCounter, 269
	je BLUEWINS1                     ; if equal it jumps to BLUEWINS1	
	cmp redCounter, 268
	je ADRAW1                        ; if equal it jumps to ADRAW1
	writeStringAt 3, 33, quit        ;if not equal it displays that the game has terminated and displays "GAME QUIT!!!!!" 
	jmp ENDPROGRAM                   ; jumps to end the program
  REDWINS1:                          ; GOES HERE IF RED SMILEY WINS   
	writeStringAt 3, 27 , winnerRed  ; displays text stating that red smilety is the winner
	jmp ENDPROGRAM                   ; jumps to end the program
  BLUEWINS1:                         ; GOES HERE IF BLUE SMILEY WINS
	writeStringAt 3, 27, winnerBlue  ; displays text stating that blue smilety is the winner
	jmp ENDPROGRAM                   ; jumps to end the program
  ADRAW1:                            ; GOES HERE IF GAME IS A DRAW
	writeStringAt 3, 27,Draw         ; displays text stating that the game ended in a draw   
	
ENDPROGRAM:                  		 ; LABEL OF CODE ENDING THE PROGRAM
	setColor white, black            ; Set color to white characters on blackback ground
	setCursor 3,0 				     ; Sets Cursor to upper left corner
	exit						     ; exit to operating system
main ENDP

incScore PROC
;Recieves EDX = "r" or "b"
	cmp edx, "r"
	je INCRED                        ; JUMPS TO INCRED IF EDX CONTAINS "r"
	cmp edx, "b"
	je INCBLUE                       ; JUMPS TO INCRED IF EDX CONTAINS "b"
	jmp over                         ; JUMPS TO END OF PROCEDURE
INCRED:								 ; LABEL PROGRAM GOES TO INCREMENT redCounter
	inc redCounter				     ; increments redCounter
	jmp over
INCBLUE:							 ; LABEL PROGRAM GOES TO INCREMENT blueCounter
	inc blueCounter				     ; increments blueCounter
over:
incScore ENDP

getOffset PROC 
;returns:ESI = the offset of a character on the back string variable which contains the game screen pattern
;Recievs: CL = the number of the desired row to move to. DL = the number of the deisred column to move to 
	mov al,cl                        ; AL stores the number of the desired row to go to
	mul num80                        ; AX stores the multiplication result of 80 times the row number 
	movzx bx, dl                     ; BX stores the number of the desired column to go to
	add ax, bx                       ; AX = (80*NextRow)+ NextCol
	movzx esi,ax                    ; ESI stores the offset of the character in the psotion that the user wants to move the smiley to 
	ret                              ; returns to the calling procedure
getOffset ENDP
END main