;*******************************************************************                       
LCDWIR  EQU     0FFE0H   ; LCD IR 쓰기 
LCDWDR  EQU     0FFE1H ; LCD DR 쓰기 
LCDRIR  EQU     0FFE2H  ; LCD IR 읽기 
LCDRDR  EQU     0FFE3H ; LCD DR 읽기 

;DEFINE VARIABLE ;******************************************************************** 
INST  EQU     20H ; LCD INSTRUCTION 값 보관 
DATA  EQU     21H ; LCD DATA 값 보관 
LROW  EQU     22H  ; LCD 표시 좌표: 행의 값 보관   
LCOL  EQU     23H ; LCD 표시 좌표: 열의 값 보관 
NUMFONT  EQU     24H ; message 개수 보관  
FDPL  EQU     25H ; DPL 값 보관 
FDPH  EQU     26H ; DPH 값 보관 

HOUR EQU      80H
MIN  EQU      81H
SEC  EQU      82H



; DEFINE LCD INSTRUCTION  ;******************************************************************** 
CLEAR  EQU     01H  ; CLEAR 명령  
; Cursor home  
CUR_HOME  EQU     02H  ; CURSOR HOME 위치로 이동  
; 커서의 진행 방향을 제어하고,표시의 이동을 제어 
ENTRY2      EQU     06H     ; 어드레스를 1 증가 시키고, 커서나 블링크를 우로 이동  
; 표시부 ON/OFF 제어 
DCB6         EQU     0EH     ; 표시(ON) ,커서(ON) ,블링크(OFF)  
; Function setting 
FUN5         EQU     38H    ; 8비트 2행 5*7  1/16 듀티  
; DDRAM Address setting 
LINE_1        EQU     80H     ;1 000 0000 : LCD 1 번째 줄로 이동 
LINE_2        EQU     0C0H    ;1 010 0000 : LCD 2 번째 줄로 이동  



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
; 무한루프를 돌다가 Interrupt 발생 시 SERVICE 루틴으로 가게 된다.
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
MOV P1,#00H ; LED 켜기
MOV TH0,#3CH
MOV TL0,#0AFH
SETB TCON.TR0
RETI

LEDOFF: CLR C
MOV P1,#0FFH ; LED 끄기
MOV TH0,#3CH
MOV TL0,#0AFH
SETB TCON.TR0
RETI


;********************************************************* 
;*           서브 루틴: DISFONT                                 * 
;*           입       력: 없음                                         * 
;*           출       력: LCD        * 
;*           기       능: 글자 폰트를 읽어와 LCD에 표시          * 
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
;*       서브 루틴: 커서의 위치 제어(CUR_MOV)                   * 
;*       입       력: 커서의 행과 열 < LROW(행) ,LCOL(열) >       * 
;*       출       력: LCD 화 면                                     *  
;*       기       능: 커서 위치 조정                               * 
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
;*         서브 루틴: INSTWR                                     * 
;*         입       력: INST                                       * 
;*         출       력: LCD 화 면                                   * 
;*         기       능: LCD INSTRUCTION 레지스터 쓰기             * 
;********************************************************* 
INSTWR:      CALL      INSTRD
               MOV       DPTR,#LCDWIR 
              MOV       A,INST 
              MOVX      @DPTR,A 
               RET 
;********************************************************* 
;*          서브 루틴: DATAW                                      * 
;*          입       력: DATA                                      *  
;*          출       력: LCD 화 면                                   * 
;*          기       능: LCD DATA 레지스터 쓰기                    * 
;********************************************************* 
DATAWR:      CALL      INSTRD 
              MOV       DPTR,#LCDWDR
               MOV       A,DATA
               MOVX      @DPTR,A
               RET 


;********************************************************* 
;*          서브 루틴: INSTRD                                     *  
;*          입       력: 없음                                       * 
;*          출       력: BUSY                                       * 
;*          기       능: 비지 플래그/어드레스 읽기                 * 
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