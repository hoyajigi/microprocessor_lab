;*******************************************************************                       
LCDWIR  EQU     0FFE0H   ; LCD IR ���� 
LCDWDR  EQU     0FFE1H ; LCD DR ���� 
LCDRIR  EQU     0FFE2H  ; LCD IR �б� 
LCDRDR  EQU     0FFE3H ; LCD DR �б� 

;DEFINE VARIABLE ;******************************************************************** 
INST  EQU     20H ; LCD INSTRUCTION �� ���� 
DATA  EQU     21H ; LCD DATA �� ���� 
LROW  EQU     22H  ; LCD ǥ�� ��ǥ: ���� �� ����   
LCOL  EQU     23H ; LCD ǥ�� ��ǥ: ���� �� ���� 
NUMFONT  EQU     24H ; message ���� ����  
FDPL  EQU     25H ; DPL �� ���� 
FDPH  EQU     26H ; DPH �� ���� 

HOUR EQU      80H
MIN  EQU      81H
SEC  EQU      82H



; DEFINE LCD INSTRUCTION  ;******************************************************************** 
CLEAR  EQU     01H  ; CLEAR ���  
; Cursor home  
CUR_HOME  EQU     02H  ; CURSOR HOME ��ġ�� �̵�  
; Ŀ���� ���� ������ �����ϰ�,ǥ���� �̵��� ���� 
ENTRY2      EQU     06H     ; ��巹���� 1 ���� ��Ű��, Ŀ���� ��ũ�� ��� �̵�  
; ǥ�ú� ON/OFF ���� 
DCB6         EQU     0EH     ; ǥ��(ON) ,Ŀ��(ON) ,��ũ(OFF)  
; Function setting 
FUN5         EQU     38H    ; 8��Ʈ 2�� 5*7  1/16 ��Ƽ  
; DDRAM Address setting 
LINE_1        EQU     80H     ;1 000 0000 : LCD 1 ��° �ٷ� �̵� 
LINE_2        EQU     0C0H    ;1 010 0000 : LCD 2 ��° �ٷ� �̵�  



ORG 8000H

;LCD initialization 
LCD_INIT:    MOV      INST,#FUN5
             CALL     INSTWR                                                     
             MOV      INST,#DCB6            
             CALL    INSTWR 
             MOV      INST,#CLEAR  
             CALL     INSTWR  
             MOV      INST,#ENTRY2 
             CALL     INSTWR       

; Initial message 
LCD_MESG:    MOV      LROW,#01H
               MOV      LCOL,#02H
               CALL     CUR_MOV
               MOV      DPTR,#MESSAGE1
              MOV      FDPL,DPL
              MOV      FDPH,DPH
              MOV      NUMFONT,#0EH
               CALL     DISFONT
               MOV      LROW,#02H
              MOV      LCOL,#02H
               CALL     CUR_MOV
                MOV      DPTR,#MESSAGE2
               MOV      FDPL,DPL
               MOV      FDPH,DPH
                MOV      NUMFONT,#0EH
               CALL     DISFONT               
;JMP      $ 

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


;********************************************************* 
;*           ���� ��ƾ: DISFONT                                 * 
;*           ��       ��: ����                                         * 
;*           ��       ��: LCD        * 
;*           ��       ��: ���� ��Ʈ�� �о�� LCD�� ǥ��          * 
;********************************************************* 
DISFONT:     MOV      R5,#00H       
FLOOP:       MOV      DPL,FDPL
               MOV      DPH,FDPH 
               MOV      A,R5 
              MOVC     A,@A+DPTR 
                               MOV      DATA,A  
              CALL     DATAWR 
              INC      R5 
              MOV      A,R5 
              CJNE     A,NUMFONT,FLOOP 
              RET        


;********************************************************* 
;*       ���� ��ƾ: Ŀ���� ��ġ ����(CUR_MOV)                   * 
;*       ��       ��: Ŀ���� ��� �� < LROW(��) ,LCOL(��) >       * 
;*       ��       ��: LCD ȭ ��                                     *  
;*       ��       ��: Ŀ�� ��ġ ����                               * 
;********************************************************* 
CUR_MOV:     MOV      A,LROW 
              CJNE     A,#01H, NEXT  
             MOV      A ,#LINE_1  
             ADD      A ,LCOL  
             MOV      INST,A  
             CALL     INSTWR   
            JMP      RET_POINT
NEXT:        CJNE     A,#02H, RET_POINT
               MOV      A ,#LINE_2
               ADD      A ,LCOL 
              MOV      INST,A  
              CALL     INSTWR  
RET_POINT:   RET 



;********************************************************* 
;*         ���� ��ƾ: INSTWR                                     * 
;*         ��       ��: INST                                       * 
;*         ��       ��: LCD ȭ ��                                   * 
;*         ��       ��: LCD INSTRUCTION �������� ����             * 
;********************************************************* 
INSTWR:      CALL      INSTRD
               MOV       DPTR,#LCDWIR 
              MOV       A,INST 
              MOVX      @DPTR,A 
               RET 
;********************************************************* 
;*          ���� ��ƾ: DATAW                                      * 
;*          ��       ��: DATA                                      *  
;*          ��       ��: LCD ȭ ��                                   * 
;*          ��       ��: LCD DATA �������� ����                    * 
;********************************************************* 
DATAWR:      CALL      INSTRD 
              MOV       DPTR,#LCDWDR
               MOV       A,DATA
               MOVX      @DPTR,A
               RET 


;********************************************************* 
;*          ���� ��ƾ: INSTRD                                     *  
;*          ��       ��: ����                                       * 
;*          ��       ��: BUSY                                       * 
;*          ��       ��: ���� �÷���/��巹�� �б�                 * 
;********************************************************* 
INSTRD:      MOV       DPTR,#LCDRIR
               MOVX      A,@DPTR 
              JB        ACC.7,INSTRD 
               RET                 
;********************************************************* 
;*         DEFINE  MESSAGE                               * 
;********************************************************* 
MESSAGE1:    DB  'D','O',' ','Y','O' 
              DB  'U','R',' ','B','E' 
              DB  'S','T',' ','!' 
MESSAGE2:    DB  'Y','O','U',' ','C'
               DB  'A','N',' ','D','O' 
              DB  ' ','I','T','!'              




ORG 9F0BH
JMP SERVICE
END