      PARAMETER (N777=1000000)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER MIT
      DIMENSION ZZ(0:N777), T0(0:N777), U0(0:N777), U(0:N777),
     *UP(0:2), UN(0:2), T0P(0:2), ZP(0:2), U0P(0:2),
     * T0N(0:2), ZN(0:2), U0N(0:2)
      character(len=1024) :: name
      character(len=1024) :: filename
      character(len=1024) :: format_string

      PRINT*, 'ENTER Z'
      READ(*,*) ZZB
      PRINT*, 'ENTER U'
      READ(*,*) UB

C       TO NAME OUTPUT FILE AS ZZB_UB
      format_string = "(A1,F5.3,A2,F6.3)"
      write (filename,format_string) "d", ZZB, "_u", UB




      OPEN(779,FILE='779')
      OPEN(780,FILE='780')
      OPEN(781,FILE='781')

C     FILE LENGTH DETERMINATION
      N778=0
  720 CONTINUE
      READ(779,*,END=721) A, A, A, A
      N778=N778+1
C      WRITE(6,*) N778
      GOTO 720
  721 CONTINUE

      CLOSE(779)
      OPEN(779,FILE='779')

      DO K=1,N778
        READ(779,*) ZZ(K), T0(K), U0(K), U(K)
C        WRITE(6,*) ZZ(K), T0(K), U0(K), U(K)
      ENDDO
      write(6,*) 'READ 779'


      CLOSE(779)
      OPEN(779,FILE='779')

*===========CHOSING=PARTICLES=WITH=STATED=VELOCITIES======

      DO K=1,N778
        IF (ZZ(K).GE.ZZB-0.002 .AND. ZZ(K).LE.ZZB .AND.
     *  U(K).GT.UB-0.01 .AND. U(K).LT.UB+0.01) THEN
            WRITE(780,*) ZZ(K), T0(K), U0(K), U(K)
        ELSEIF (ZZ(K).GE.ZZB .AND. ZZ(K).LE.ZZB+0.002 .AND.
     *  U(K).GT.UB-0.01 .AND. U(K).LT.UB+0.01) THEN
            WRITE(780,*) ZZ(K), T0(K), U0(K), U(K)
        ENDIF
      ENDDO

      CLOSE(780)
      OPEN(780,FILE='780')

      N778=0
  754 CONTINUE
      READ(780,*,END=753) A, A, A, A
      N778=N778+1
C      WRITE(6,*) N778
      GOTO 754
  753 CONTINUE

      CLOSE(780)
      OPEN(780,FILE='780')

      DO K=1,N778
        READ(780,*) ZZ(K), T0(K), U0(K), U(K)
C        WRITE(6,*) ZZ(K), T0(K), U0(K), U(K)
      ENDDO

*   SORTING 780 BY T0 INCREASING
       DO J=1,N778
            DO K=1,N778-1
                IF(T0(K).GT.T0(K+1)) THEN
                    T1=T0(K)
                    Z1=ZZ(K)
                    U01=U0(K)
                    U1=U(K)
                    T0(K)=T0(K+1)
                    ZZ(K)=ZZ(K+1)
                    U0(K)=U0(K+1)
                    U(K)=U(K+1)
                    T0(K+1)=T1
                    ZZ(K+1)=Z1
                    U0(K+1)=U01
                    U(K+1)=U1
                ENDIF
            ENDDO
        ENDDO

      CLOSE(780)
      OPEN(780,FILE='780')

      DO K=1,N778
            WRITE(780,*) ZZ(K), T0(K), U0(K), U(K)
      ENDDO

      write(6,*) '780 IS SORTED BY T0 AND WRITTEN IN 780'

      CLOSE(780)
      OPEN(780,FILE='780')

      N780=0
  722 CONTINUE
      READ(780,*,END=723) A, A, A, A
      N780=N780+1
C      WRITE(6,*) N778
      GOTO 722
  723 CONTINUE
      CLOSE(780)

*===========PUT=PARTICLES=IN=CORRESPONDING=COORDINATE=======
      OPEN(780,FILE='780')
c       CREATE FILE FOR PREVIOUS SPATIAL POINT
      OPEN(781,FILE='P1',STATUS='REPLACE')
c       CREATE FILE FOR CURRENT SPATIAL POINT
      OPEN(782,FILE='P2',STATUS='REPLACE')
c       CREATE FILE FOR NEXT SPATIAL POINT
      OPEN(783,FILE='P3',STATUS='REPLACE')


      DO K=1,N780
        READ(780,*,END=723) ZZ(K), T0(K), U0(K), U(K)

        IF (ZZ(K).LT.ZZB-0.001) THEN
            WRITE(781,*) ZZ(K), T0(K), U0(K), U(K)
        ELSEIF (ZZ(K).GT.ZZB+0.001) THEN
            WRITE(783,*) ZZ(K), T0(K), U0(K), U(K)
        ELSE
            WRITE(782,*) ZZ(K), T0(K), U0(K), U(K)
        ENDIF


      ENDDO

      write(6,*) 'FANTASTIC TRIO IS CREATED'

      CLOSE(780)
      CLOSE(781)
      CLOSE(782)
      CLOSE(783)

      OPEN(781,FILE='P1')
      OPEN(782,FILE='P2')
      OPEN(783,FILE='P3')


*====DETERMINE=SPATIAL=POINT=WITH=MINIMUM=PARTICLE=COUNT=====

      P781=0
  724 CONTINUE
      READ(781,*,END=725) A, A, A, A
      P781=P781+1
C      WRITE(6,*) N778
      GOTO 724
  725 CONTINUE


      P782=0
  726 CONTINUE
      READ(782,*,END=727) A, A, A, A
      P782=P782+1
C      WRITE(6,*) N778
      GOTO 726
  727 CONTINUE


      P783=0
  728 CONTINUE
      READ(783,*,END=729) A, A, A, A
      P783=P783+1
C      WRITE(6,*) N778
      GOTO 728
  729 CONTINUE

      IF (P781.LT.P782 .AND. P781.LT.P783) THEN
        P781=P781
      ELSEIF (P782.LT.P781 .AND. P782.LT.P783) THEN
        P781=P782
      ELSE
        P781=P783
      ENDIF

      CLOSE(781)
      CLOSE(782)
      CLOSE(783)

*==========COMPUTING=FUNCTIONS=Q=AND=D========================
      OPEN(781,FILE='P1')
      OPEN(782,FILE='P2')
      OPEN(783,FILE='P3')


      OPEN(785,FILE='OUTPUT')
      OPEN(786,FILE=FILENAME)

      IF (UB.GE.0.0) THEN
        DO I=1,P781
            READ(781,*) ZP1, T0P1, U0P1, UP1
            READ(782,*) ZP2, T0P2, U0P2, UP2
            READ(783,*) ZP3, T0P3, U0P3, UP3
            DP=-(ZP3-ZP1)/(2*(T0P3-T0P1))
            QP=UP2-DP
            WRITE(785,*) ZP2, T0P2, U0P2, UP2, DP, QP
            WRITE(786,*) ZP2, T0P2, U0P2, UP2, DP, QP
        ENDDO
      ELSE
        DO I=1,P781
            READ(781,*) ZP1, T0P1, U0P1, UP1
            READ(782,*) ZP2, T0P2, U0P2, UP2
            READ(783,*) ZP3, T0P3, U0P3, UP3
            DP=(ZP1-ZP3)/(2*(T0P3-T0P1))
            QP=UP2-DP
            WRITE(785,*) ZP2, T0P2, U0P2, UP2, DP, QP
            WRITE(786,*) ZP2, T0P2, U0P2, UP2, DP, QP
        ENDDO

      ENDIF



      CLOSE(781)
      CLOSE(782)
      CLOSE(785)
      CLOSE(786)

      END
