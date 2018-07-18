ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Constants.
USING lib.GameObjects.Ball.
USING lib.GameObjects.Player.
USING lib.TheField.


CLASS lib.CollisionManager:

  CONSTRUCTOR PRIVATE CollisionManager():
  END CONSTRUCTOR.

  DEFINE PUBLIC STATIC EVENT BallCollidedWithPaddle
    SIGNATURE VOID (pBall AS Ball, pPlayer AS Player).

  DEFINE PUBLIC STATIC EVENT BallCollidedWithFloor
    SIGNATURE VOID (pBall AS Ball).

  DEFINE PUBLIC STATIC EVENT BallCollidedWithCeiling
    SIGNATURE VOID (pBall AS Ball).

  DEFINE PUBLIC STATIC EVENT BallCollidedWithLeftWall
    SIGNATURE VOID (pBall AS Ball).

  DEFINE PUBLIC STATIC EVENT BallCollidedWithRightWall
    SIGNATURE VOID (pBall AS Ball).

  METHOD PUBLIC STATIC VOID CheckCollisions(pTheField AS TheField):
    DEF VAR lBallIndex AS INT NO-UNDO.
    DO lBallIndex = 1 TO EXTENT(pTheField:Balls):
      DEF VAR lBall AS Ball NO-UNDO.
      ASSIGN lBall = pTheField:Balls[lBallIndex].

      IF isBallCollidingWithFloor(lBall) THEN
      DO:
        BallCollidedWithFloor:Publish(lBall).
      END.

      IF isBallCollidingWithCeiling(lBall) THEN
      DO:
        BallCollidedWithCeiling:Publish(lBall).
      END.

      IF isBallCollidingWithLeftWall(lBall) THEN
      DO:
        BallCollidedWithLeftWall:Publish(lBall).
      END.

      IF isBallCollidingWithRightWall(lBall) THEN
      DO:
        BallCollidedWithRightWall:Publish(lBall).
      END.

      DEF VAR lPlayerIndex AS INT NO-UNDO.
      DO lPlayerIndex = 1 TO EXTENT(pTheField:Players):
        DEF VAR lPlayer AS Player NO-UNDO.
        ASSIGN lPlayer = pTheField:Players[lPlayerIndex].

        IF isBallCollidingWithPaddle(lBall, lPlayer) THEN
        DO:
          BallCollidedWithPaddle:Publish(lBall, lPlayer).
        END.
      END.
    END.
  END METHOD.

  METHOD PRIVATE STATIC LOG isBallCollidingWithPaddle(
    pBall AS Ball,
    pPlayer AS Player):
    DEF VAR lCenterOfBallYAxis AS DECIMAL NO-UNDO.
    ASSIGN lCenterOfBallYAxis = pBall:YLoc + pBall:TheHeight / 2 + pBall:YVelocity.
    DEF VAR lPlayerLowYBound AS DECIMAL NO-UNDO.
    ASSIGN lPlayerLowYBound = pPlayer:YLoc + pPlayer:YVelocity.
    DEF VAR lPlayerHighYBound AS DECIMAL NO-UNDO.
    ASSIGN lPlayerHighYBound = pPlayer:YLoc + pPlayer:TheHeight + pPlayer:YVelocity.

    DEF VAR lCorrectYAxis AS LOG NO-UNDO.
    ASSIGN lCorrectYAxis =
      lPlayerLowYBound <= lCenterOfBallYAxis
      AND lCenterOfBallYAxis <= lPlayerHighYBound.

    DEF VAR lCenterOfBallXAxis AS DECIMAL NO-UNDO.
    ASSIGN lCenterOfBallXAxis = pBall:XLoc + pBall:TheWidth / 2.
    DEF VAR lCenterOfBallXAxisAfterMovement AS DECIMAL NO-UNDO.
    ASSIGN lCenterOfBallXAxisAfterMovement = lCenterOfBallXAxis + pBall:XVelocity.
    DEF VAR lPlayerXBound AS DECIMAL NO-UNDO.
    ASSIGN lPlayerXBound = pPlayer:XLoc + pPlayer:TheWidth + pPlayer:XVelocity.

    DEF VAR lBallLowerBound AS DECIMAL NO-UNDO.
    IF lCenterOfBallXAxis < lCenterOfBallXAxisAfterMovement THEN
    DO:
      ASSIGN lBallLowerBound = lCenterOfBallXAxis.
    END.
    ELSE
    DO:
      ASSIGN lBallLowerBound = lCenterOfBallXAxisAfterMovement.
    END.

    DEF VAR lBallHigherBound AS DECIMAL NO-UNDO.
    IF lCenterOfBallXAxis > lCenterOfBallXAxisAfterMovement THEN
    DO:
      ASSIGN lBallHigherBound = lCenterOfBallXAxis.
    END.
    ELSE
    DO:
      ASSIGN lBallHigherBound = lCenterOfBallXAxisAfterMovement.
    END.

    DEF VAR lCorrectXAxis AS LOG NO-UNDO.
    ASSIGN lCorrectXAxis =
      lBallLowerBound <= lPlayerXBound
      AND lPlayerXBound <= lBallHigherBound.

    RETURN lCorrectXAxis AND lCorrectYAxis.

  END METHOD.

  METHOD PRIVATE STATIC LOG isBallCollidingWithFloor(
    pBall AS Ball):
    RETURN pBall:YLoc + pBall:TheHeight + pBall:YVelocity
      >= Constants:GameAreaHeight.
  END METHOD.

  METHOD PRIVATE STATIC LOG isBallCollidingWithCeiling(
    pBall AS Ball):
    RETURN pBall:YLoc + pBall:YVelocity <= 0.
  END METHOD.

  METHOD PRIVATE STATIC LOG isBallCollidingWithLeftWall(
    pBall AS Ball):
    RETURN pBall:XLoc + pBall:XVelocity <= 0.
  END METHOD.

  METHOD PRIVATE STATIC LOG isBallCollidingWithRightWall(
    pBall AS Ball):
    RETURN pBall:XLoc + pBall:TheWidth + pBall:XVelocity
      >= Constants:GameAreaWidth.
  END METHOD.

END CLASS.
