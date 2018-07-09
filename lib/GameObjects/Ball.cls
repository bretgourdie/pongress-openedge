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

    CREATE RECTANGLE spriteInner ASSIGN
      X = pStartX + Constants:BallInnerSpriteOffset
      Y = pStartY - Constants:BallInnerSpriteOffset
      BGCOLOR = Constants:WhiteColorCode
      FGCOLOR = Constants:WhiteColorCode
      HEIGHT-PIXELS = Constants:BallInnerRectangleHeight
      WIDTH-PIXELS = Constants:BallInnerRectangleWidth
      FRAME = pParentFrame
      VISIBLE = TRUE.

    CREATE RECTANGLE spriteMiddle ASSIGN
      X = pStartX + Constants:BallMiddleSpriteOffset
      Y = pStartY - Constants:BallMiddleSpriteOffset
      BGCOLOR = Constants:WhiteColorCode
      FGCOLOR = Constants:WhiteColorCode
      HEIGHT-PIXELS = Constants:BallMiddleRectangleHeight
      WIDTH-PIXELS = Constants:BallMiddleRectangleWidth
      FRAME = pParentFrame
      VISIBLE = TRUE.

    CREATE RECTANGLE spriteOuter ASSIGN
      X = pStartX + Constants:BallOuterSpriteOffset
      Y = pStartY - Constants:BallOuterSpriteOffset
      BGCOLOR = Constants:WhiteColorCode
      FGCOLOR = Constants:WhiteColorCode
      HEIGHT-PIXELS = Constants:BallOuterRectangleHeight
      WIDTH-PIXELS = Constants:BallOuterRectangleWidth
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
      spriteInner:X = XLoc + Constants:BallInnerSpriteOffset
      spriteInner:Y = YLoc - Constants:BallInnerSpriteOffset
      spriteMiddle:X = XLoc + Constants:BallMiddleSpriteOffset
      spriteMiddle:Y = YLoc - Constants:BallMiddleSpriteOffset
      spriteOuter:X = XLoc + Constants:BallOuterSpriteOffset
      spriteOuter:Y = YLoc - Constants:BallOuterSpriteOffset.
  END METHOD.
END CLASS.
