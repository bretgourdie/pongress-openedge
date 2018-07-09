ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.GameObjects.GameObject.
USING lib.Constants.
USING lib.Movement.MoveMessage.
USING lib.Movement.NoMovement.
USING lib.Movement.BallMoveMessage.
USING lib.Movement.BallMoveDownLeft.
USING lib.Movement.BallMoveDownRight.
USING lib.Movement.BallMoveUpLeft.
USING lib.Movement.BallMoveUpRight.

CLASS lib.GameObjects.Ball INHERITS GameObject:

  DEF PRIVATE PROPERTY spriteHandle AS HANDLE NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY ballMoveMessage AS MoveMessage NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY parentFrame AS HANDLE NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY framesSinceInitialSet AS INT NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY ballMoveDownLeft AS BallMoveMessage NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY ballMoveDownRight AS BallMoveMessage NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY ballMoveUpLeft AS BallMoveMessage NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY ballMoveUpRight AS BallMoveMessage NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY noMovement AS MoveMessage NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC Ball(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE):
    SUPER(Constants:BallHeight, Constants:BallWidth, pStartX, pStartY).

    ASSIGN
      THIS-OBJECT:ballMoveDownLeft = NEW BallMoveDownLeft()
      THIS-OBJECT:ballMoveDownRight = NEW BallMoveDownRight()
      THIS-OBJECT:ballMoveUpLeft = NEW BallMoveUpLeft()
      THIS-OBJECT:ballMoveUpRight = NEW BallMoveUpRight()
      THIS-OBJECT:noMovement = NEW NoMovement().

    CREATE RECTANGLE spriteHandle ASSIGN
      X = pStartX
      Y = pStartY
      BGCOLOR = Constants:WhiteColorCode
      FGCOLOR = Constants:WhiteColorCode
      HEIGHT-PIXELS = Constants:BallHeight
      WIDTH-PIXELS = Constants:BallWidth
      FRAME = pParentFrame
      VISIBLE = TRUE.

  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC VOID Update():
    
    IF framesSinceInitialSet >= Constants:FramesToWaitForBallMovement THEN
    DO:
      /* if hit paddle */
      IF FALSE THEN
      DO:
        ballMoveMessage = handlePaddle().
      END.

      /* if hit boundary */
      IF FALSE THEN
      DO:
        ballMoveMessage = handleCeilingOrFloor().
      END.

      /* if scored */
      IF FALSE THEN
      DO:
        handleBeginningTimer().
      END.
      /*MoveRelativeToCurrentPosition(lNewXDistance, lNewYDistance).*/
    END.
    framesSinceInitialSet = framesSinceInitialSet + 1.
  END METHOD.

  METHOD PRIVATE BallMoveMessage handleCeilingOrFloor():
    CASE ballMoveMessage:
      WHEN ballMoveDownLeft THEN RETURN ballMoveUpLeft.
      WHEN ballMoveDownRight THEN RETURN ballMoveUpRight.
      WHEN ballMoveUpLeft THEN RETURN ballMoveDownLeft.
      WHEN ballMoveUpRight THEN RETURN ballMoveDownRight.
    END CASE.
  END METHOD.

  METHOD PRIVATE BallMoveMessage handlePaddle():
    CASE ballMoveMessage:
      WHEN ballMoveDownLeft THEN RETURN ballMoveDownRight.
      WHEN ballMoveDownRight THEN RETURN ballMoveDownLeft.
      WHEN ballMoveUpLeft THEN RETURN ballMoveUpRight.
      WHEN ballMoveUpRight THEN RETURN ballMoveUpLeft.
    END CASE.
  END METHOD.

  METHOD PRIVATE VOID handleBeginningTimer():
    ASSIGN THIS-OBJECT:ballMoveMessage = THIS-OBJECT:NoMovement.
  END METHOD.

  METHOD OVERRIDE PUBLIC VOID Draw():
    ASSIGN
      spriteHandle:X = XLoc
      spriteHandle:Y = YLoc.
  END METHOD.
END CLASS.
