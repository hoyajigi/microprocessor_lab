ORG 8000H
MAIN: MOV TMOD,#00000001B
;GATE =0,TIMER MODE,RUN MODE 01
MOV IE,#10000010B
;ENABLE ONLY TIMER 0
MOV TH0,#4CH
MOV TL0,#00H
CLR C
SETB TCON.TR0
MOV A #14H
JMP $
; ���ѷ����� ���ٰ� Interrupt �߻� �� SERVICE ��ƾ���� ���� �ȴ�.
SERVICE: CLR TCON.TR0
DJNZ A SERVICE2
MOV A #14H
JNC LEDON
JMP LEDOFF


SERVICE2:
MOV TH0,#3CH
MOV TL0,#0AFH
SETB TCON.TR0
RETI

LEDON: SETB C
MOV P1,#00H ; LED �ѱ�
MOV TH0,#3CH
MOV TL0,#0AFH
SETB TCON.TR0
RETI
LEDOFF: CLR C
MOV P1,#0FFH ; LED ����
MOV TH0,#3CH
MOV TL0,#0AFH
SETB TCON.TR0
RETI
ORG 9F0BH
JMP SERVICE
END