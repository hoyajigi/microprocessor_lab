	ORG 8000H		;�����ּҸ� 8000H�� ����
LOOP:   MOV P1, #0AAH		;P1����Ʈ�� AA�� �ִ´�. 0B10101010
	CALL DELAY		;DELAY��� �Լ� ȣ��
	MOV P1, #55H		;P1����Ʈ�� 55�� �ִ´�. 0B01010101
	CALL DELAY
	JMP LOOP		;������ ������ �Ѵ�.

DELAY:  MOV R7, #0FFH		;R7�������Ϳ� FF�� �ִ´�.
DELAY1: MOV R6, #0FFH
DELAY2: DJNZ R6, DELAY2		;<����Ʈ>�� ���ҽ�Ű�� 0�� �ƴϸ� �б�
	DJNZ R7, DELAY1

	RET
END