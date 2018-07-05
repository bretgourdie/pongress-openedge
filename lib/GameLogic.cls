ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.GameObjects.*.

CLASS lib.GameLogic:

  CONSTRUCTOR PUBLIC GameLogic():
  END CONSTRUCTOR.

  METHOD PUBLIC LOG IsBallScoredOnLeftSide(
    pLeftSidePlayer AS Player,
    pBall AS Ball):
    RETURN pBall:X < pLeftSidePlayer:X.
  END METHOD.

  METHOD PUBLIC LOG IsBallScoredOnRightSide(
    pRightSidePlayer AS Player,
    pBall AS Ball):
    RETURN pBall:X > pRightSidePlayer:X.
  END METHOD.

  METHOD PUBLIC LOG ResetScores(
    pLeftSidePlayer AS Player,
    pRightSidePlayer AS Player):
    pLeftSidePlayer:ResetScore().
    pRightSidePlayer:ResetScore().
  END METHOD.

  METHOD PUBLIC LOG HasPlayerWon(
    pPlayer AS Player):
    DEF VAR lWinningScore AS INT NO-UNDO INITIAL 10.
    RETURN pPlayer:Score = lWinningScore.
  END METHOD.

END CLASS.
