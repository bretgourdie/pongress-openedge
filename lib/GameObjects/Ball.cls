ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.GameObjects.GameObject.
USING lib.Constants.
USING lib.TheField.
USING lib.Movement.MoveMessage.
USING lib.Movement.NoMovement.
USING lib.Movement.BallMoveMessage.
USING lib.Movement.BallMoveDownLeft.
USING lib.Movement.BallMoveDownRight.
USING lib.Movement.BallMoveUpLeft.
USING lib.Movement.BallMoveUpRight.

CLASS lib.GameObjects.Ball INHERITS GameObject:

  DEF PRIVATE PROPERTY spriteInner AS HANDLE NO-UNDO
    GET.  
    SET.

  DEF PRIVATE PROPERTY spriteMiddle AS HANDLE NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY spriteOuter AS HANDLE NO-UNDO
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

  DEF PRIVATE PROPERTY spriteColorCode AS INT NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC Ball(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE,
    pTheField AS TheField):
    SUPER(Constants:BallHeight, Constants:BallWidth, pStartX, pStartY, pTheField).

    ASSIGN
      THIS-OBJECT:ballMoveDownLeft = NEW BallMoveDownLeft()
      THIS-OBJECT:ballMoveDownRight = NEW BallMoveDownRight()
      THIS-OBJECT:ballMoveUpLeft = NEW BallMoveUpLeft()
      THIS-OBJECT:ballMoveUpRight = NEW BallMoveUpRight()
      THIS-OBJECT:noMovement = NEW NoMovement()
      THIS-OBJECT:spriteColorCode = Constants:WhiteColorCode.

    CREATE RECTANGLE spriteInner ASSIGN
      X = pStartX + Constants:BallInnerSpriteXOffset
      Y = pStartY + Constants:BallInnerSpriteYOffset
      HEIGHT-PIXELS = Constants:BallInnerRectangleHeight
      WIDTH-PIXELS = Constants:BallInnerRectangleWidth
      FRAME = pParentFrame
      VISIBLE = TRUE.

    CREATE RECTANGLE spriteMiddle ASSIGN
      X = pStartX + Constants:BallMiddleSpriteXOffset
      Y = pStartY + Constants:BallMiddleSpriteYOffset
      HEIGHT-PIXELS = Constants:BallMiddleRectangleHeight
      WIDTH-PIXELS = Constants:BallMiddleRectangleWidth
      FRAME = pParentFrame
      VISIBLE = TRUE.

    CREATE RECTANGLE spriteOuter ASSIGN
      X = pStartX + Constants:BallOuterSpriteXOffset
      Y = pStartY + Constants:BallOuterSpriteYOffset
      HEIGHT-PIXELS = Constants:BallOuterRectangleHeight
      WIDTH-PIXELS = Constants:BallOuterRectangleWidth
      FRAME = pParentFrame
      VISIBLE = TRUE.

  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC VOID Update():

    IF framesSinceInitialSet < Constants:FramesToWaitForBallMovement THEN
    DO:
      ballMoveMessage = noMovement.
      setStateColor("Waiting").
    END.

    ELSE IF framesSinceInitialSet = Constants:FramesToWaitForBallMovement THEN
    DO:
      ballMoveMessage = ballMoveUpLeft.
    END.
    
    ELSE
    DO:
      IF isHittingPaddle() THEN
      DO:
        ballMoveMessage = handlePaddle().
        setStateColor("Paddle bounce").
      END.

      ELSE IF isHittingBoundary() THEN
      DO:
        ballMoveMessage = handleCeilingOrFloor().
        setStateColor("Boundary bounce").
      END.

      ELSE IF isScored() THEN
      DO:
        handleBeginningTimer().
        ballMoveMessage = noMovement.
        setStateColor("Scored").
      END.

      ELSE
      DO:
        setStateColor("Normal").
      END.

      MoveRelativeToCurrentPosition(ballMoveMessage).
    END.

    framesSinceInitialSet = framesSinceInitialSet + 1.
  END METHOD.

  METHOD PRIVATE LOG isHittingPaddle():
    RETURN FALSE.
  END METHOD.

  METHOD PRIVATE LOG isHittingBoundary():
    RETURN
      YLoc = 0 OR YLoc + theHeight = Constants:GameAreaHeight.
  END METHOD.

  METHOD PRIVATE LOG isScored():
    RETURN FALSE.
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

  METHOD PRIVATE VOID setStateColor(pState AS CHAR):
    CASE pState:
      WHEN "Waiting" THEN spriteColorCode = 11.
      WHEN "Boundary bounce" THEN spriteColorCode = 14.
      WHEN "Paddle bounce" THEN spriteColorCode = 10.
      WHEN "Scored" THEN spriteColorCode = 12.
      OTHERWISE spriteColorCode = Constants:WhiteColorCode.
    END CASE.
  END METHOD.

  METHOD OVERRIDE PUBLIC VOID Draw():

    ASSIGN
      spriteInner:X = XLoc + Constants:BallInnerSpriteXOffset
      spriteInner:Y = YLoc + Constants:BallInnerSpriteYOffset
      spriteInner:BGCOLOR = spriteColorCode
      spriteInner:FGCOLOR = spriteColorCode
      spriteMiddle:X = XLoc + Constants:BallMiddleSpriteXOffset
      spriteMiddle:Y = YLoc + Constants:BallMiddleSpriteYOffset
      spriteMiddle:BGCOLOR = spriteColorCode
      spriteMiddle:FGCOLOR = spriteColorCode
      spriteOuter:X = XLoc + Constants:BallOuterSpriteXOffset
      spriteOuter:Y = YLoc + Constants:BallOuterSpriteYOffset
      spriteOuter:BGCOLOR = spriteColorCode
      spriteOuter:FGCOLOR = spriteColorCode.
  END METHOD.
END CLASS.
