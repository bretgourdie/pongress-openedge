ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Constants.
USING lib.GameObjects.Ball.
USING lib.GameObjects.Player.
USING lib.TheField.


CLASS lib.CollisionManager:

  CONSTRUCTOR PRIVATE CollisionManager():
  END CONSTRUCTOR.

  DEFINE PUBLIC STATIC EVENT BallCollidedWithPaddle
    SIGNATURE VOID (pPlayer AS Player).

  DEFINE PUBLIC STATIC EVENT BallCollidedWithFloor
    SIGNATURE VOID ().

  DEFINE PUBLIC STATIC EVENT BallCollidedWithCeiling
    SIGNATURE VOID ().

  DEFINE PUBLIC STATIC EVENT BallCollidedWithLeftWall
    SIGNATURE VOID ().

  DEFINE PUBLIC STATIC EVENT BallCollidedWithRightWall
    SIGNATURE VOID ().

  METHOD PUBLIC STATIC VOID CheckCollisions(pTheField AS TheField):
    
    IF isBallCollidingWithPaddle(pTheField:Ball, pTheField:UserPlayer) THEN
    DO:
      BallCollidedWithPaddle:Publish(pTheField:UserPlayer).
    END.

    ELSE IF isBallCollidingWithPaddle(pTheField:Ball, pTheField:CPUPlayer) THEN
    DO:
      BallCollidedWithPaddle:Publish(pTheField:CPUPlayer).
    END.

    ELSE IF isBallCollidingWithFloor(pTheField:Ball) THEN
    DO:
      BallCollidedWithFloor:Publish().
    END.

    ELSE IF isBallCollidingWithCeiling(pTheField:Ball) THEN
    DO:
      BallCollidedWithCeiling:Publish().
    END.

    ELSE IF isBallCollidingWithLeftWall(pTheField:Ball) THEN
    DO:
      BallCollidedWithLeftWall:Publish().
    END.

    ELSE IF isBallCollidingWithRightWall(pTheField:Ball) THEN
    DO:
      BallCollidedWithRightWall:Publish().
    END.
    
  END METHOD.

  METHOD PRIVATE STATIC LOG isBallCollidingWithPaddle(
    pBall AS Ball,
    pPlayer AS Player):
    RETURN FALSE.
  END METHOD.

  METHOD PRIVATE STATIC LOG isBallCollidingWithFloor(
    pBall AS Ball):
    RETURN pBall:YLoc + pBall:YVelocity >= Constants:GameAreaHeight.
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
    RETURN pBall:XLoc + pBall:XVelocity >= Constants:GameAreaWidth.
  END METHOD.

END CLASS.
