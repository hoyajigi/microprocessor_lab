;*******************************************************************                       
LCDWIR  EQU     0FFE0H   ; LCD IR ���� 
LCDWDR  EQU     0FFE1H ; LCD DR ���� 
LCDRIR  EQU     0FFE2H  ; LCD IR �б� 
LCDRDR  EQU     0FFE3H ; LCD DR �б� 

DATAOUT    EQU     0FFF0H ;������ �ƿ��� �ּ� 
DATAIN     EQU     0FFF1H ;������ ���� �ּ�  
DLED       EQU     0FFC1H ;������ 2��  7_SEGMENT�� �ּ� 
ALED0      EQU     0FFC2H ;�߰� 2�� 7_SEGMENT�� �ּ� 
ALED1      EQU     0FFC3H ;���� 2�� 7_SEGMENT�� �ּ� 

;DEFINE VARIABLE ;******************************************************************** 
INST  EQU     20H ; LCD INSTRUCTION �� ���� 
DATA  EQU     21H ; LCD DATA �� ���� 
LROW  EQU     22H  ; LCD ǥ�� ��ǥ: ���� �� ����   
LCOL  EQU     23H ; LCD ǥ�� ��ǥ: ���� �� ���� 
NUMFONT  EQU     24H ; message ���� ����  
FDPL  EQU     25H ; DPL �� ���� 
FDPH  EQU     26H ; DPH �� ���� 

VSEC       EQU     30H  ; ������ 2���� 7_SEGMENT�� ǥ�� �� �� ���� 
VMIN       EQU     31H  ; �߰� 2���� 7_SEGMENT�� ǥ�� �� �� ����  
VHOUR      EQU     32H  ; ���� 2���� 7_SEGMENT�� ǥ�� �� �� ���� 
VBUF       EQU     33H  ; �ӽ� ���� ��� 
VX         EQU     34H
;HOUR EQU      80H
;MIN  EQU      81H
;SEC  EQU      82H

;��� ����  
REP_COUNT EQU     5 


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

MOV TMOD,#00000001B
;GATE =0,TIMER MODE,RUN MODE 01
MOV IE,#10000010B
;ENABLE ONLY TIMER 0
MOV TH0,#4CH
MOV TL0,#00H
MOV A #14H
;JMP $


MOV     SP,#60H    ; ���� �������� ��ġ�� �̵�  
MOV     VHOUR,#80H ; VHOUR�� �ʱ�ȭ            
MOV     VMIN,#51H  ; VMIN��  �ʱ�ȭ            
MOV     VSEC,#00H  ; VSEC��  �ʱ�ȭ            
CALL    DISPLAY 

MOV VX #06
MAIN:      CALL    FINDKEYCODE   ; Ű�ڵ� ���� �о���� ��ƾ ȣ��
           ;JB      ACC.4, ERR    ; �Լ� Ű �̸�, ���� ǥ��
           MOV     VBUF, A       ; �о�� Ű �ڵ� ���� VBUF�� ����
           CALL    SHIFT         ; ǥ�� �̵� ��ƾ ȣ��
           CALL    DISPLAY       ; �̵��� ���� ǥ��
           CALL    BOUNCE        ; �ٿ ������ ���ֱ� ���� ��ƾ ȣ��
           DJNZ    VX,MAIN          ; �ݺ� ������ ���� �б� 
SETB TCON.TR0
JMP $

ERR:       CALL    ERROR         ; ���� ǥ�� ��ƾ ȣ��
             JMP     MAIN          ; ���� ��ƾ���� ����
  
           ; ������ Ű�� ������ ������ ���� �ð� ���� 
BOUNCE:    CALL    DELAY         ; �ð� ���� ��ƾ ȣ��    
RELOAD:    MOV     A,#0          ; Ű�� ���� ������ üũ
             CALL    SUBKEY
                   CPL     A
            JNZ     RELOAD        ; Ű�� �������� �ʾ�����, �ٽ� üũ  
           CALL    DELAY         ; �ð� ���� ��ƾ ȣ��
           RET                   ; ���� ��ƾ���� ����

;**************************************************** 
;*        ���� ��ƾ : ERROR                         * 
;*             �Է� : ����                          * 
;*             ��� : 7_SEGMENT                     * 
;*             ��� : REP_COUNT �� ������ Ƚ����ŭ  * 
;*                    ���� �ѱ⸦ �ݺ�              * 
;**************************************************** 
ERROR:     MOV     R4,#REP_COUNT  
ERR_1:     MOV     VSEC,#0 
           MOV     VMIN,#0
           MOV     VHOUR,#0
           CALL    DISPLAY       ; 6���� 7_SEGMENT �ѱ�
           MOV     R3,#10
FIRST:    CALL    DELAY 
           DJNZ    R3,FIRST      ; ���� �ð� ����  
           MOV     VSEC , #0FFH 
           MOV     VMIN , #0FFH 
           MOV     VHOUR, #0FFH 
           CALL    DISPLAY       ; 6���� 7_SEGMENT �ѱ�   
           MOV     R3,#10       
SECOND:    CALL    DELAY 
           DJNZ    R3,SECOND     ; ���� �ð� ����  
           DJNZ    R4,ERR_1      ; �ݺ� Ƚ�� ��ŭ �ݺ� �ߴ��� üũ 
           RET                   ; ���� ��ƾ���� ���� 

;*************************************************** 
;*         ���� ��ƾ : DELAY                       * 
;*              �Է� : ����                        * 
;*              ��� : ����                        * 
;*              ��� : R1*2+3 ��ŭ�� �ð� ����     * 
;*************************************************** 
DELAY:     MOV     R0,#020H  
REPEAT:    MOV     R1,#0FFH 
           DJNZ    R1,$ 
           DJNZ    R0,REPEAT 
           RET                

;*************************************************** 
;*         ���� ��ƾ : SHIFT                       * 
;*              �Է� : VBUF                        * 
;*              ��� : VSEC,VMIN,VHOUR             * 
;*              ��� : ǥ�õ� ���� �������� �̵�   * 
;*                     ��Ű��                      *
;*************************************************** 
SHIFT:     MOV     A,VHOUR 
           SWAP    A 
           MOV     VHOUR,A           
           MOV     A,VMIN 
           SWAP    A   
           MOV     VMIN,A            
           MOV     A,VSEC 
           SWAP    A   
           MOV     VSEC,A            
           MOV     A,VMIN 
           MOV     R0,#VHOUR 
           XCHD    A,@R0          
           MOV     VMIN,A  
           MOV     A,VSEC 
           MOV     R0,#VMIN 
           XCHD    A,@R0       
           MOV     VSEC,A  
           MOV     A,VBUF 
           MOV     R0,#VSEC    
           XCHD    A,@R0        ; ���� �Էµ� Ű ������ �ٲ� 
           RET 

;*************************************************** 
;*         ���� ��ƾ: FINDKEYCODE                  *  
;*              �Է�: A                            * 
;*              ���: A                            * 
;*              ���: Ű �ڵ尪��  ã�Ƴ���        * 
;*************************************************** 
FINDKEYCODE: PUSH    PSW        ; ���ÿ� PSW���� ����
             SETB    PSW.4      ; ��ũ3 �������� ���
             SETB    PSW.3  
INITIAL:    MOV     R1,#00H      ; R1���� �ʱ�ȭ 
            MOV     A,#11101111B ; ������ �ƿ��� �ʱⰪ 
           SETB    C               
COLSCAN:    MOV     R0,A         ; R0�� ������ �ƿ� �� ���� 
             INC     R1           ; R1 ���� �� ����
            CALL    SUBKEY       ; Ű �е� �Է� ���� ����  
            CJNE    A,#0FFH,RSCAN ; ������� ���� 0FFH �� �ƴϸ�, Ű �Է��� �߻�                               
               MOV     A,R0 
            SETB    C 
           RRC     A            ; ���� ���� �̵� 
           JNC     INITIAL      ; ��� ���� ��ĵ������,�ٽ� ���� 
            JMP     COLSCAN      ; ���� ���� ��ĵ�� ���� �б�  
RSCAN:      MOV     R2,#00H      ; R2 ���� �� ���� 
ROWSCAN:    RRC     A            ; ��� ���� 1 �� �ٲ������ ����
            JNC     MATRIX       ; ĳ���� �߻��ϸ�, MATRIX�� �б� 
            INC     R2           ; ĳ���� �߻����� ������, ���� ������ �̵� 
           JMP     ROWSCAN      ; ���� ���� ��ĵ�� ���� �б� 

MATRIX:     MOV     A,R2         ; R2���� ���� �� ���� 
            MOV     B,#05H       ; 1���� 5���� �̷���� 
           MUL     AB           ; 2���� �迭�� 1���� �迭�� ���� �ٲ� 
            ADD     A,R1         ; R1���� ���� �� ���� 
            CALL    INDEX        ; Ű �ڵ� ���� ���� 
            POP     PSW          ; �������� ���� PSW ���� ������ �� 
            RET                  ; ���� ��ƾ���� ���� 


;***************************************************************** 
;*          ���� ��ƾ : SUBKEY                                   * 
;*               �Է� : ACC                                      * 
;*               ��� : ACC                                      * 
;*               ��� : ������ �ƿ����� �����͸� ��������        * 
;*                      ������ ������ ����� Ȯ��                *  
;***************************************************************** 
SUBKEY:     MOV     DPTR,#DATAOUT 
            MOVX    @DPTR,A 
            MOV     DPTR,#DATAIN 
            MOVX    A,@DPTR 
            RET 
;***************************************************************** 
;*          ���� ��ƾ : DISPLAY                                  * 
;*               �Է� : ACC                                      * 
;*               ��� : ACC                                      * 
;*               ��� : Ű �ڵ尪�� 7_SEGMENT �� ǥ��            *
;*****************************************************************  
DISPLAY:   MOV     DPTR,#DLED      ; ������ 2���� 7_SEGMENT ���� 
            MOV     A,VSEC        ; VSEC ���� ������ 
            MOVX    @DPTR,A       ; VSEC ���� ǥ��  
            MOV     DPTR,#ALED0   ; �߰� 2���� 7_SEGMENT ���� 
            MOV     A,VMIN        ; VMIN ���� ������ 
            MOVX    @DPTR,A       ; VMIN ���� ǥ��

MOV      DPTR,#ALED1   ; ������ 2���� 7_SEGMENT ���� 
            MOV     A,VHOUR       ; VHOUR ���� ������ 
            MOVX    @DPTR,A       ; VHOUR ���� ǥ�� 
            RET                   ; ���� ��ƾ���� ����    

;***************************************************************** 
;*          ���� ��ƾ : INDEX                                    * 
;*               �Է� : ACC                                      * 
;*               ��� : ACC                                      * 
;*              ��� : Ű �ڵ尪�� ����                          *  
;***************************************************************** 
;DEFINE  FUNCTION KEY 
RWKEY       EQU     10H ;READ AND WRITE KEY 
INCKEY      EQU     11H ;INCRESE KEY(COMMA ,) 
ENDKEY      EQU     12H ;END KEY (PERIOD . ) 
GO          EQU     13H ;GO-KEY 
REG         EQU     14H ;REGISTER KEY 
DECKEY      EQU     15H ;DECRESE KEY 
CODE        EQU     16H ;CODE KEY 
ST          EQU     17H ;SINGLE STEP KEY 
RST         EQU     18H ;RST KEY   


; ���ѷ����� ���ٰ� Interrupt �߻� �� SERVICE ��ƾ���� ���� �ȴ�.
SERVICE: CLR TCON.TR0
DJNZ A SERVICE2

INC VSEC
MOV A,VSEC
CLR C
CLR AC
DA A
CLR C
CLR AC
MOV VSEC,A
CJNE A, #60H, RETIP
MOV VSEC,#00

INC VMIN
MOV A,VMIN
CLR C
CLR AC
DA A
CLR C
CLR AC
MOV VMIN,A
CJNE A, #60H, RETIP
MOV VMIN,#00

INC VHOUR
MOV A,VHOUR
CLR C
CLR AC
DA A
CLR C
CLR AC
MOV VHOUR,A
CJNE A, #24H, RETIP
MOV VHOUR,#00

RETIP: CALL DISPLAY
MOV A #14H
MOV TH0,#3CH
MOV TL0,#0AFH
SETB TCON.TR0
RETI


SERVICE2:
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
;*          ���� ��ƾ: DATAW                             * 
;*          ��       ��: DATA                            *  
;*          ��       ��: LCD ȭ ��                       * 
;*          ��       ��: LCD DATA �������� ����          * 
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


INDEX:      MOVC    A,@A+PC ;������ 1 ~ 24�� ���� ������. 
            RET 
KEYBASE:    DB ST             ;SW1,ST                   1
            DB CODE           ;SW6,CODE                 2
             DB DECKEY         ;SW11,CD                 3 
            DB REG            ;SW15,REG                 4
            DB GO             ;SW19,GO                  5 
            DB 0CH            ;SW2,C                    6 
            DB 0DH            ;SW7,D                    7 
            DB 0EH            ;SW12,E                   8 
            DB 0FH           ;SW16,F                   9  
           DB INCKEY         ;SW20,COMMA (,)          10  
           DB 08H            ;SW3,8                    11 
            DB 09H            ;SW8,9                    12 
           DB 0AH            ;SW13,A                  13   
          DB 0BH            ;SW17,B                  14    
         DB ENDKEY         ;SW21,PERIOD(.)          15    
         DB 04H           ;SW4,4                    16    
         DB 05H            ;SW9,5                    17  
           DB 06H            ;SW14,6                  18
             DB 07H            ;SW18,7                  19
             DB RWKEY          ;SW22,R/W                20 
            DB 00H            ;SW5,0                    21 
            DB 01H            ;SW10,1                  22  
           DB 02H            ;SW24,2                  23 
            DB 03H            ;SW23,3                  24 
           ;DB RST            ;SW24  RST KEY           25                      
END 
