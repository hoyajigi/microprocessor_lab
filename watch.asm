;*******************************************************************                       
LCDWIR  EQU     0FFE0H   ; LCD IR 쓰기 
LCDWDR  EQU     0FFE1H ; LCD DR 쓰기 
LCDRIR  EQU     0FFE2H  ; LCD IR 읽기 
LCDRDR  EQU     0FFE3H ; LCD DR 읽기 

DATAOUT    EQU     0FFF0H ;데이터 아웃의 주소 
DATAIN     EQU     0FFF1H ;데이터 인의 주소  
DLED       EQU     0FFC1H ;오른쪽 2개  7_SEGMENT의 주소 
ALED0      EQU     0FFC2H ;중간 2개 7_SEGMENT의 주소 
ALED1      EQU     0FFC3H ;왼쪽 2개 7_SEGMENT의 주소 

;DEFINE VARIABLE ;******************************************************************** 
INST  EQU     20H ; LCD INSTRUCTION 값 보관 
DATA  EQU     21H ; LCD DATA 값 보관 
LROW  EQU     22H  ; LCD 표시 좌표: 행의 값 보관   
LCOL  EQU     23H ; LCD 표시 좌표: 열의 값 보관 
NUMFONT  EQU     24H ; message 개수 보관  
FDPL  EQU     25H ; DPL 값 보관 
FDPH  EQU     26H ; DPH 값 보관 

VSEC       EQU     30H  ; 오른쪽 2개의 7_SEGMENT에 표시 될 값 보관 
VMIN       EQU     31H  ; 중간 2개의 7_SEGMENT에 표시 될 값 보관  
VHOUR      EQU     32H  ; 왼쪽 2개의 7_SEGMENT에 표시 될 값 보관 
VBUF       EQU     33H  ; 임시 보관 장소 
VX         EQU     34H
;HOUR EQU      80H
;MIN  EQU      81H
;SEC  EQU      82H

;상수 정의  
REP_COUNT EQU     5 


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

MOV TMOD,#00000001B
;GATE =0,TIMER MODE,RUN MODE 01
MOV IE,#10000010B
;ENABLE ONLY TIMER 0
MOV TH0,#4CH
MOV TL0,#00H
MOV A #14H
;JMP $


MOV     SP,#60H    ; 스택 포인터의 위치를 이동  
MOV     VHOUR,#80H ; VHOUR의 초기화            
MOV     VMIN,#51H  ; VMIN의  초기화            
MOV     VSEC,#00H  ; VSEC의  초기화            
CALL    DISPLAY 

MOV VX #06
MAIN:      CALL    FINDKEYCODE   ; 키코드 값을 읽어오는 루틴 호출
           ;JB      ACC.4, ERR    ; 함수 키 이면, 에러 표시
           MOV     VBUF, A       ; 읽어온 키 코드 값을 VBUF에 보관
           CALL    SHIFT         ; 표시 이동 루틴 호출
           CALL    DISPLAY       ; 이동된 값을 표시
           CALL    BOUNCE        ; 바운스 현상을 없애기 위한 루틴 호출
           DJNZ    VX,MAIN          ; 반복 동작을 위한 분기 
SETB TCON.TR0
JMP $

ERR:       CALL    ERROR         ; 에러 표시 루틴 호출
             JMP     MAIN          ; 메인 루틴으로 복귀
  
           ; 눌러진 키가 떨어질 때까지 일정 시간 지연 
BOUNCE:    CALL    DELAY         ; 시간 지연 루틴 호출    
RELOAD:    MOV     A,#0          ; 키가 떨어 졌는지 체크
             CALL    SUBKEY
                   CPL     A
            JNZ     RELOAD        ; 키가 떨어지지 않았으면, 다시 체크  
           CALL    DELAY         ; 시간 지연 루틴 호출
           RET                   ; 상위 루틴으로 복귀

;**************************************************** 
;*        서브 루틴 : ERROR                         * 
;*             입력 : 없음                          * 
;*             출력 : 7_SEGMENT                     * 
;*             기능 : REP_COUNT 에 지정된 횟수만큼  * 
;*                    끄고 켜기를 반복              * 
;**************************************************** 
ERROR:     MOV     R4,#REP_COUNT  
ERR_1:     MOV     VSEC,#0 
           MOV     VMIN,#0
           MOV     VHOUR,#0
           CALL    DISPLAY       ; 6개의 7_SEGMENT 켜기
           MOV     R3,#10
FIRST:    CALL    DELAY 
           DJNZ    R3,FIRST      ; 일정 시간 지연  
           MOV     VSEC , #0FFH 
           MOV     VMIN , #0FFH 
           MOV     VHOUR, #0FFH 
           CALL    DISPLAY       ; 6개의 7_SEGMENT 켜기   
           MOV     R3,#10       
SECOND:    CALL    DELAY 
           DJNZ    R3,SECOND     ; 일정 시간 지연  
           DJNZ    R4,ERR_1      ; 반복 횟수 만큼 반복 했는지 체크 
           RET                   ; 상위 루틴으로 복귀 

;*************************************************** 
;*         서브 루틴 : DELAY                       * 
;*              입력 : 없음                        * 
;*              출력 : 없음                        * 
;*              기능 : R1*2+3 만큼의 시간 지연     * 
;*************************************************** 
DELAY:     MOV     R0,#020H  
REPEAT:    MOV     R1,#0FFH 
           DJNZ    R1,$ 
           DJNZ    R0,REPEAT 
           RET                

;*************************************************** 
;*         서브 루틴 : SHIFT                       * 
;*              입력 : VBUF                        * 
;*              출력 : VSEC,VMIN,VHOUR             * 
;*              기능 : 표시된 값을 왼쪽으로 이동   * 
;*                     시키기                      *
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
           XCHD    A,@R0        ; 새로 입력된 키 값으로 바꿈 
           RET 

;*************************************************** 
;*         서브 루틴: FINDKEYCODE                  *  
;*              입력: A                            * 
;*              출력: A                            * 
;*              기능: 키 코드값을  찾아내기        * 
;*************************************************** 
FINDKEYCODE: PUSH    PSW        ; 스택에 PSW값을 보관
             SETB    PSW.4      ; 뱅크3 레지스터 사용
             SETB    PSW.3  
INITIAL:    MOV     R1,#00H      ; R1값을 초기화 
            MOV     A,#11101111B ; 데이터 아웃의 초기값 
           SETB    C               
COLSCAN:    MOV     R0,A         ; R0에 데이터 아웃 값 보관 
             INC     R1           ; R1 열의 값 보관
            CALL    SUBKEY       ; 키 패드 입력 상태 조사  
            CJNE    A,#0FFH,RSCAN ; 누산기의 값이 0FFH 가 아니면, 키 입력이 발생                               
               MOV     A,R0 
            SETB    C 
           RRC     A            ; 다음 열로 이동 
           JNC     INITIAL      ; 모든 열을 스캔했으면,다시 시작 
            JMP     COLSCAN      ; 다음 열의 스캔을 위한 분기  
RSCAN:      MOV     R2,#00H      ; R2 행의 값 보관 
ROWSCAN:    RRC     A            ; 어느 행이 1 로 바뀌었는지 조사
            JNC     MATRIX       ; 캐리가 발생하면, MATRIX로 분기 
            INC     R2           ; 캐리가 발생하지 않으면, 다음 행으로 이동 
           JMP     ROWSCAN      ; 다음 행의 스캔을 위한 분기 

MATRIX:     MOV     A,R2         ; R2에는 행의 값 보존 
            MOV     B,#05H       ; 1행은 5열로 이루어짐 
           MUL     AB           ; 2차원 배열을 1차원 배열로 값을 바꿈 
            ADD     A,R1         ; R1에는 열의 값 보존 
            CALL    INDEX        ; 키 코드 값을 지정 
            POP     PSW          ; 스택으로 부터 PSW 값을 가지고 옴 
            RET                  ; 상위 루틴으로 복귀 


;***************************************************************** 
;*          서브 루틴 : SUBKEY                                   * 
;*               입력 : ACC                                      * 
;*               출력 : ACC                                      * 
;*               기능 : 데이터 아웃으로 데이터를 내보내고        * 
;*                      데이터 인으로 결과를 확인                *  
;***************************************************************** 
SUBKEY:     MOV     DPTR,#DATAOUT 
            MOVX    @DPTR,A 
            MOV     DPTR,#DATAIN 
            MOVX    A,@DPTR 
            RET 
;***************************************************************** 
;*          서브 루틴 : DISPLAY                                  * 
;*               입력 : ACC                                      * 
;*               출력 : ACC                                      * 
;*               기능 : 키 코드값을 7_SEGMENT 로 표시            *
;*****************************************************************  
DISPLAY:   MOV     DPTR,#DLED      ; 오른쪽 2개의 7_SEGMENT 지정 
            MOV     A,VSEC        ; VSEC 값을 누산기로 
            MOVX    @DPTR,A       ; VSEC 값의 표시  
            MOV     DPTR,#ALED0   ; 중간 2개의 7_SEGMENT 지정 
            MOV     A,VMIN        ; VMIN 값을 누산기로 
            MOVX    @DPTR,A       ; VMIN 값의 표시

MOV      DPTR,#ALED1   ; 오른쪽 2개의 7_SEGMENT 지정 
            MOV     A,VHOUR       ; VHOUR 값을 누산기로 
            MOVX    @DPTR,A       ; VHOUR 값의 표시 
            RET                   ; 상위 루틴으로 복귀    

;***************************************************************** 
;*          서브 루틴 : INDEX                                    * 
;*               입력 : ACC                                      * 
;*               출력 : ACC                                      * 
;*              기능 : 키 코드값을 정의                          *  
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


; 무한루프를 돌다가 Interrupt 발생 시 SERVICE 루틴으로 가게 된다.
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
;*          서브 루틴: DATAW                             * 
;*          입       력: DATA                            *  
;*          출       력: LCD 화 면                       * 
;*          기       능: LCD DATA 레지스터 쓰기          * 
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


INDEX:      MOVC    A,@A+PC ;누산기는 1 ~ 24의 값을 가진다. 
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
