ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.CollisionManager.
USING lib.GameObjects.Ball.
USING lib.GameObjects.GameObject.
USING lib.GameObjects.Player.
USING lib.Constants.
USING lib.TheField.
USING lib.Sprites.Sprite.
USING lib.Sprites.BallSprite.

CLASS lib.GameObjects.Ball INHERITS GameObject:

  DEF PRIVATE PROPERTY framesSinceInitialSet AS INT NO-UNDO
    GET.
    SET.

  DEFINE PUBLIC EVENT BallMoved
    SIGNATURE VOID (
      pXLoc AS INT,
      pYLoc AS INT,
      xVelocity AS DECIMAL,
      yVelocity AS DECIMAL).

  CONSTRUCTOR PUBLIC Ball(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE,
    pTheField AS TheField):
    SUPER(Constants:BallHeight, Constants:BallWidth, pStartX, pStartY, pTheField).
    
    ASSIGN sprite = NEW BallSprite(
      pStartX,
      pStartY,
      pParentFrame,
      Constants:WhiteColorCode).
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC VOID Update():

    IF framesSinceInitialSet < Constants:FramesToWaitForBallMovement THEN
    DO:
      sprite:SetStateColor("Waiting").
    END.

    ELSE IF framesSinceInitialSet = Constants:FramesToWaitForBallMovement THEN
    DO:
      DEF VAR lXDirection AS INT NO-UNDO.
      DEF VAR lYDirection AS INT NO-UNDO.

      ASSIGN
        lXDirection = getRandomCartesianDirection()
        XVelocity = lXDirection * getBallSpeed()
        lYDirection = getRandomCartesianDirection()
        YVelocity = lYDirection * getBallSpeed().
    END.
    
    IF framesSinceInitialSet >= Constants:FramesToWaitForBallMovement THEN
    DO:

      MoveRelativeToCurrentPosition().
      sprite:SetStateColor("Normal").
      publishBallMoved().
    END.

    framesSinceInitialSet = framesSinceInitialSet + 1.
  END METHOD.

  METHOD PROTECTED DECIMAL getBallSpeed():
    RETURN Constants:BallMovementVectorMultiplier.
  END METHOD.

  METHOD PRIVATE INT getRandomCartesianDirection():
    RETURN RANDOM(0, 1) * 2 - 1.
  END METHOD.

  METHOD PROTECTED VOID handleBallCollidedWithPaddle(
    pBall AS Ball,
    pPlayer AS Player):

    IF pBall = THIS-OBJECT THEN
    DO:
      sprite:SetStateColor("Scored").
      handleBallCollidedWithHorizontalBoundaryInner(pBall).
    END.
  END METHOD.

  METHOD PROTECTED VOID handleBallCollidedWithHorizontalBoundary(
    pBall AS Ball):

    IF pBall = THIS-OBJECT THEN
    DO:
      sprite:SetStateColor("Paddle bounce").
      handleBallCollidedWithHorizontalBoundaryInner(pBall).
    END.
  END METHOD.

  METHOD PROTECTED VOID handleBallCollidedWithHorizontalBoundaryInner(
    pBall AS Ball):
    IF pBall = THIS-OBJECT THEN
    DO:
      ASSIGN XVelocity = -1 * XVelocity.
    END.
  END METHOD.

  METHOD PROTECTED VOID handleBallCollidedWithVerticalBoundary(
    pBall AS Ball):
    IF pBall = THIS-OBJECT THEN
    DO:
      sprite:SetStateColor("Boundary bounce").
      ASSIGN YVelocity = -1 * YVelocity.
    END.
  END METHOD.

  METHOD PROTECTED VOID publishBallMoved():
    BallMoved:Publish(XLoc, YLoc, XVelocity, YVelocity).
  END METHOD.

  METHOD OVERRIDE PROTECTED VOID subscribeToEvents():
    CollisionManager:BallCollidedWithPaddle:Subscribe(
      handleBallCollidedWithPaddle).
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
