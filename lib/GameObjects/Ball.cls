ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.CollisionManager.
USING lib.GameObjects.GameObject.
USING lib.GameObjects.Player.
USING lib.Constants.
USING lib.TheField.

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

  DEF PRIVATE PROPERTY parentFrame AS HANDLE NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY framesSinceInitialSet AS INT NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY spriteColorCode AS INT NO-UNDO
    GET.
    SET.

  DEFINE PUBLIC EVENT BallMoved
    SIGNATURE VOID (pXLoc AS INT, pYLoc AS INT).

  CONSTRUCTOR PUBLIC Ball(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE,
    pTheField AS TheField):
    SUPER(Constants:BallHeight, Constants:BallWidth, pStartX, pStartY, pTheField).

    ASSIGN
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
      setStateColor("Waiting").
    END.

    ELSE IF framesSinceInitialSet = Constants:FramesToWaitForBallMovement THEN
    DO:
    END.
    
    ELSE
    DO:
      setStateColor("Normal").
    END.

    framesSinceInitialSet = framesSinceInitialSet + 1.
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

  METHOD PRIVATE VOID handleBallCollidedWithHorizontalBoundary(pPlayer AS Player):
    setStateColor("Scored").
    handleBallCollidedWithHorizontalBoundaryInner().
  END METHOD.

  METHOD PRIVATE VOID handleBallCollidedWithHorizontalBoundary():
    setStateColor("Paddle bounce").
    handleBallCollidedWithHorizontalBoundaryInner().
  END METHOD.

  METHOD PRIVATE VOID handleBallCollidedWithHorizontalBoundaryInner():

  END METHOD.

  METHOD PRIVATE VOID handleBallCollidedWithVerticalBoundary():
    setStateColor("Boundary bounce").
  END METHOD.

  METHOD OVERRIDE PROTECTED VOID subscribeToEvents():
    CollisionManager:BallCollidedWithPaddle:Subscribe(
      handleBallCollidedWithHorizontalBoundary).
    CollisionManager:BallCollidedWithCeiling:Subscribe(
      handleBallCollidedWithVerticalBoundary).
    CollisionManager:BallCollidedWithFloor:Subscribe(
      handleBallCollidedWithVerticalBoundary).
    CollisionManager:BallCollidedWithLeftWall:Subscribe(
      handleBallCollidedWithHorizontalBoundary).
    CollisionManager:BallCollidedWithRightWall:Subscribe(
      handleBallCollidedWithHorizontalBoundary).
  END METHOD.
END CLASS.
