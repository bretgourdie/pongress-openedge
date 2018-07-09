ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.GameObjects.GameObject.
USING lib.GameObjects.Player.
USING lib.GameObjects.Ball.
USING lib.Input.CPUInputSource.
USING lib.Input.UserInputSource.
USING lib.Constants.

CLASS lib.GameEngine:

  DEF PRIVATE PROPERTY userPlayer AS Player NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY opponentPlayer AS Player NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY ball AS Ball NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY gameObjects AS GameObject EXTENT NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY isRunning AS LOG NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY frameHandle AS HANDLE NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC GameEngine(pFrameHandle AS HANDLE):
    frameHandle = pFrameHandle.
  END CONSTRUCTOR.

  METHOD PRIVATE VOID initialize():
    createGameObjectCollection().
    ASSIGN isRunning = TRUE.
  END METHOD.

  METHOD PRIVATE VOID createGameObjectCollection():
    DEF VAR lPlayers AS Player EXTENT NO-UNDO.
    DEF VAR lBall AS Ball NO-UNDO.
    ASSIGN
      lPlayers = createPlayers()
      lBall = createBall().

    DEF VAR lArrayLength AS INT NO-UNDO.
    ASSIGN lArrayLength = EXTENT(lPlayers) + 1 /* ball */.

    DEF VAR lGameObjects AS GameObject EXTENT.
    EXTENT(lGameObjects) = lArrayLength.

    DEF VAR lPlayerIndex AS INT NO-UNDO.
    DO lPlayerIndex = 1 TO EXTENT(lPlayers):
      lGameObjects[lPlayerIndex] = lPlayers[lPlayerIndex].
    END.

    lGameObjects[lArrayLength] = lBall.

    ASSIGN gameObjects = lGameObjects.
  END METHOD.

  METHOD PUBLIC VOID Run():
    THIS-OBJECT:initialize().

    DO WHILE THIS-OBJECT:isRunning:
      ETIME(TRUE).

      THIS-OBJECT:Update().
      THIS-OBJECT:Draw().

      THIS-OBJECT:performTiming().
    END.
  END METHOD.

  METHOD PUBLIC VOID Update():
    DEF VAR lGameObjectIndex AS INT NO-UNDO.
    DO lGameObjectIndex = 1 TO EXTENT(gameObjects):
      gameObjects[lGameObjectIndex]:Update().
    END.
  END METHOD.

  METHOD PUBLIC VOID Draw():
    DEF VAR lGameObjectIndex AS INT NO-UNDO.
    DO lGameObjectIndex = 1 TO EXTENT(gameObjects):
      gameObjects[lGameObjectIndex]:Draw().
    END.
  END METHOD.

  METHOD PRIVATE VOID performTiming():
    DEF VAR lEndTime AS INT NO-UNDO.
    ASSIGN lEndTime = ETIME(FALSE).

    DEF VAR lTime AS INT NO-UNDO.
    ASSIGN lTime = 
      (Constants:MillisecondsInOneSecond / Constants:FramesPerSecond) 
      - lEndTime.

    IF lTime > 0 THEN
    DO:
      RUN Sleep(lTime).
    END.
  END METHOD.

  METHOD PRIVATE Player EXTENT createPlayers():
    DEF VAR lStartY AS INT NO-UNDO.
    ASSIGN lStartY = (Constants:GameAreaHeight - Constants:PlayerHeight) / 2.

    DEF VAR lLeftStartX AS INT NO-UNDO.
    DEF VAR lRightStartX AS INT NO-UNDO.
    DEF VAR lDistanceFromEdgeOfScreen AS INT NO-UNDO.
    ASSIGN 
      lDistanceFromEdgeOfScreen = Constants:GameAreaWidth / 20
      lLeftStartX = lDistanceFromEdgeOfScreen
      lRightStartX = Constants:GameAreaWidth - lDistanceFromEdgeOfScreen.

    ASSIGN
      userPlayer = NEW Player(
        lLeftStartX,
        lStartY,
        frameHandle, 
        NEW UserInputSource())
      opponentPlayer = NEW Player(
        lRightStartX,
        lStartY,
        frameHandle,
        NEW CPUInputSource()).

    DEF VAR lPlayers AS Player EXTENT 2.
    ASSIGN
      lPlayers[1] = userPlayer
      lPlayers[2] = opponentPlayer.

    RETURN lPlayers.
  END METHOD.

  METHOD PRIVATE Ball createBall():
    DEF VAR lStartX AS INT NO-UNDO.
    DEF VAR lStartY AS INT NO-UNDO.

    ASSIGN
      lStartX = Constants:GameAreaWidth / 2
      lStartY = Constants:GameAreaHeight / 2.

    ASSIGN
      ball = NEW Ball(
        lStartX,
        lStartY,
        frameHandle).

    RETURN ball.
  END METHOD.

  PROCEDURE Sleep EXTERNAL "kernel32.DLL":
    DEFINE INPUT PARAMETER intMilliseconds AS LONG.
  END PROCEDURE.

END CLASS.
