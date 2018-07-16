ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.CollisionManager.
USING lib.Constants.
USING lib.GameObjects.Ball.
USING lib.GameObjects.Player.
USING lib.TheField.

CLASS lib.GameObjects.Invisiball INHERITS Ball:

  DEF PRIVATE PROPERTY isSubbedToBallStartedMovement AS LOG NO-UNDO
    GET.
    SET.
  
  CONSTRUCTOR PUBLIC Invisiball(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE,
    pTheField AS TheField):
    SUPER(pStartX, pStartY, pParentFrame, pTheField).
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC VOID Update():
    IF NOT isSubbedToBallStartedMovement THEN
    DO:
      subscribeToBallStartedMovement().
      isSubbedToBallStartedMovement = TRUE.
    END.

    MoveRelativeToCurrentPosition().
    sprite:SetStateColor("Scored").
    publishBallMoved().
  END METHOD.

  METHOD OVERRIDE PROTECTED VOID handleBallCollidedWithPaddle(
    pBall AS Ball,
    pPlayer AS Player):
    
    IF pPlayer EQ theField:UserPlayer AND NOT pBall = THIS-OBJECT THEN DO:
      startInvisiballMovement(
        theField:Ball:XLoc,
        theField:Ball:YLoc,
        theField:Ball:XVelocity,
        theField:Ball:YVelocity).
    END.
  END METHOD.

  METHOD PROTECTED VOID startInvisiballMovement(
    pXLoc AS INT,
    pYLoc AS INT,
    pXVelocity AS DECIMAL,
    pYVelocity AS DECIMAL):
    
    ASSIGN
      XLoc = pXLoc
      YLoc = pYLoc
      XVelocity = pXVelocity * Constants:InvisiballVelocityMultiplier
      YVelocity = pYVelocity * Constants:InvisiballVelocityMultiplier.
  END METHOD.

  METHOD PROTECTED VOID subscribeToBallStartedMovement():
    theField:Ball:BallStartedMovement:Subscribe(startInvisiballMovement).
  END METHOD.

  METHOD OVERRIDE PROTECTED VOID subscribeToEvents():
    CollisionManager:BallCollidedWithPaddle:Subscribe(
      handleBallCollidedWithPaddle).
    CollisionManager:BallCollidedWithCeiling:Subscribe(
      handleBallCollidedWithVerticalBoundary).
    CollisionManager:BallCollidedWithFloor:Subscribe(
      handleBallCollidedWithVerticalBoundary).
    CollisionManager:BallCollidedWithLeftWall:Subscribe(
      stopMovement).
    CollisionManager:BallCollidedWithRightWall:Subscribe(
      stopMovement).
  END METHOD.

END CLASS.
