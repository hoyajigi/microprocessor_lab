	ORG 8000H		;시작주소를 8000H로 설정
LOOP:   MOV P1, #0AAH		;P1포인트에 AA를 넣는다. 0B10101010
	CALL DELAY		;DELAY라는 함수 호출
	MOV P1, #55H		;P1포인트에 55를 넣는다. 0B01010101
	CALL DELAY
	JMP LOOP		;루프로 점프를 한다.

DELAY:  MOV R7, #0FFH		;R7레지스터에 FF를 넣는다.
DELAY1: MOV R6, #0FFH
DELAY2: DJNZ R6, DELAY2		;<바이트>를 감소시키고 0이 아니면 분기
	DJNZ R7, DELAY1

	RET
END