	PROGRAM  SOR
	PARAMETER  ( N = 10 )
	REAL   A( N, N ),  EPS,  MAXEPS, W
	INTEGER   ITMAX

        PRINT *,  '**********  TEST_SOR   **********'
	ITMAX=20
	MAXEPS = 0.5E - 5
	W = 0.5
	DO  1  J = 1, N
	DO  1  I = 1, N
		IF ( I .EQ.J)   THEN
		     A( I, J ) = N + 2
		ELSE
		     A( I, J ) = -1.0
		ENDIF 
1	CONTINUE

	DO  2   IT = 1, ITMAX
		EPS = 0.
	DO  21   J = 2, N-1
	DO  21   I = 2, N-1
		S = A( I, J )
		A( I, J ) = (W / 4) * (A( I-1, J ) + A( I+1, J ) + A( I, J-1 ) +
     *		A( I, J+1 )) + ( 1-W ) * A( I, J)
	EPS = MAX ( EPS,  ABS( S - A( I, J )))
21	CONTINUE
	PRINT 200,  IT, EPS
200     FORMAT(' IT = ',I4, '   EPS = ', E14.7)
	IF    (EPS  .LT.  MAXEPS )    EXIT
2	CONTINUE
	END
