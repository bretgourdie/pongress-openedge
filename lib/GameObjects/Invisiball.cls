ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.CollisionManager.
USING lib.Constants.
USING lib.GameObjects.Ball.
USING lib.GameObjects.Player.
USING lib.TheField.

CLASS lib.GameObjects.Invisiball INHERITS Ball:
  
  CONSTRUCTOR PUBLIC Invisiball(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE,
    pTheField AS TheField):
    SUPER(pStartX, pStartY, pParentFrame, pTheField).
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC VOID Update():
    MoveRelativeToCurrentPosition().
    sprite:SetStateColor("Scored").
    publishBallMoved().
  END METHOD.

  METHOD OVERRIDE PROTECTED VOID handleBallCollidedWithPaddle(
    pBall AS Ball,
    pPlayer AS Player):
    
    IF pPlayer EQ theField:UserPlayer AND NOT pBall = THIS-OBJECT THEN DO:
      ASSIGN
        XLoc = theField:Ball:XLoc
        YLoc = theField:Ball:YLoc
        XVelocity =
          theField:Ball:XVelocity * Constants:InvisiballVelocityMultiplier
        YVelocity =
          theField:Ball:YVelocity * Constants:InvisiballVelocityMultiplier.
    END.
  END METHOD.

  METHOD PROTECTED VOID handleBallCollidedWithLeftWall(
    pBall AS Ball):
    handleBallCollidedWithPaddle(pBall, theField:UserPlayer).
  END METHOD.

  METHOD PROTECTED VOID stopMovement(
    pBall AS Ball):
    IF pBall EQ THIS-OBJECT THEN
    DO:
      ASSIGN
        XVelocity = 0
        YVelocity = 0.
    END.
  END METHOD.

  METHOD OVERRIDE PROTECTED VOID subscribeToEvents():
    CollisionManager:BallCollidedWithPaddle:Subscribe(
      handleBallCollidedWithPaddle).
    CollisionManager:BallCollidedWithCeiling:Subscribe(
      handleBallCollidedWithVerticalBoundary).
    CollisionManager:BallCollidedWithFloor:Subscribe(
      handleBallCollidedWithVerticalBoundary).
    CollisionManager:BallCollidedWithLeftWall:Subscribe(
      handleBallCollidedWithLeftWall).
    CollisionManager:BallCollidedWithRightWall:Subscribe(
      stopMovement).
  END METHOD.

END CLASS.
