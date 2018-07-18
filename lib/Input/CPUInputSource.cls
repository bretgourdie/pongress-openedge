ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Constants.
USING lib.GameObjects.Player.
USING lib.Input.InputSource.
USING lib.TheField.

CLASS lib.Input.CPUInputSource INHERITS InputSource:

  DEF PRIVATE PROPERTY isSubbed AS LOG NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY YDirection AS INT NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY CPUPlayer AS Player NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY isRealBallApproaching AS LOG NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC CPUInputSource():
    SUPER().
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC INT HandleInput(pTheField AS TheField):
    IF NOT isSubbed THEN
    DO:
      subscribeToEvents(pTheField).
      isSubbed = TRUE.
    END.

    RETURN YDirection.
  END METHOD.

  METHOD PROTECTED VOID subscribeToEvents(pTheField AS TheField):
    pTheField:Invisiball:BallMoved:Subscribe(handleInvisiballMoved).
    pTheField:Ball:BallMoved:Subscribe(handleRealBallMoved).
    CPUPlayer = pTheField:CPUPlayer.
  END METHOD.

  METHOD PRIVATE VOID handleRealBallMoved(
    pBallXLoc AS INT,
    pBallYLoc AS INT,
    pBallXVelocity AS DECIMAL,
    pBallYVelocity AS DECIMAL):
    ASSIGN isRealBallApproaching = pBallXVelocity > 0.
  END METHOD.

  METHOD PRIVATE VOID handleInvisiballMoved(
    pBallXLoc AS INT,
    pBallYLoc AS INT,
    pBallXVelocity AS DECIMAL,
    pBallYVelocity AS DECIMAL):

    IF isRealBallApproaching THEN
    DO:
      DEF VAR lCenterOfBall AS INT NO-UNDO.
      ASSIGN lCenterOfBall = pBallYLoc + Constants:BallHeight / 2.
      DEF VAR lCenterOfPaddle AS INT NO-UNDO.
      ASSIGN lCenterOfPaddle = CPUPlayer:YLoc + Constants:PlayerHeight / 2.
  
      IF ABSOLUTE(lCenterOfBall - lCenterOfPaddle) >= Constants:PlayerMovementVectorMultiplier THEN
      DO:
        IF lCenterOfBall > lCenterOfPaddle THEN
        DO:
          ASSIGN YDirection = 1.
        END.
        ELSE IF lCenterOfBall < lCenterOfPaddle THEN
        DO:
          ASSIGN YDirection = -1.
        END.
        ELSE
        DO:
          ASSIGN YDirection = 0.
        END.
      END.
      ELSE
      DO:
        ASSIGN YDirection = 0.
      END.
    END.
    ELSE
    DO:
      ASSIGN YDirection = 0.
    END.

  END METHOD.
END CLASS.
