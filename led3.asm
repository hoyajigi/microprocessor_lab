ORG 8000H
MOV A, #11111110B
LOOP: MOV P1, A
RL A
CALL DELAY
JMP LOOP
DELAY: MOV R0, #0FFH
DELAY1: MOV R1, #0FFH
DELAY2: MOV R2, #0FFH
DELAY3: MOV R3, #0FFH
DELAY4: DJNZ R2, DELAY3
DJNZ R1, DELAY2
DJNZ R0, DELAY1
RET
END