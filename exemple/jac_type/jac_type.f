        PROGRAM    JAC_TYPE
        PARAMETER    (L=8,  ITMAX=20)
        REAL     A(L,L),  B(L,L)
        TYPE T_EPS
            REAL:: EPS,MAXEPS
        END TYPE T_EPS
        TYPE (T_EPS) :: D
        PRINT *,  '**********  TEST_JACOBI   **********'
        D%MAXEPS  =  0.5E - 7
            DO    J  =  1, L
            DO    I  =  1, L
                A(I,  J)  =  0.
                IF(I.EQ.1 .OR. J.EQ.1 .OR. I.EQ.L .OR. J.EQ.L) THEN
                      B(I,  J) = 0.
                ELSE
                      B(I,  J)  = ( 1. + I + J )
                ENDIF
            ENDDO
            ENDDO 

        DO     IT  =  1,  ITMAX
                  D%EPS  =  0.
                  DO    J  =  2, L-1
                  DO    I  =  2, L-1
                     D%EPS = MAX ( D%EPS,  ABS(B( I, J)  -  A( I, J)))
                     A(I, J)  =  B(I, J)
                  ENDDO
                  ENDDO
                  DO    J = 2,  L-1
                  DO   I = 2,  L-1
        B(I, J) =  (A( I-1, J ) + A( I, J-1 ) + A( I+1, J)+
     *                        A( I, J+1 )) / 4
                  ENDDO
                  ENDDO
                  PRINT 200,  IT, D%EPS
200               FORMAT(' IT = ',I4, '   EPS = ', E14.7)
                  IF ( D%EPS . LT . D%MAXEPS )    EXIT
        ENDDO 

        END

