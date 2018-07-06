ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.GameObjects.Player.
USING lib.GameObjects.Ball.
USING lib.Input.InputSource.
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

  DEF PRIVATE PROPERTY isRunning AS LOG NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY frameHandle AS HANDLE NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC GameEngine(pFrameHandle AS HANDLE):
    frameHandle = pFrameHandle.
  END CONSTRUCTOR.

  METHOD PRIVATE VOID initialize(pOpponentInputSource AS InputSource):
    createPlayers(pOpponentInputSource).
    createBall().
    ASSIGN isRunning = TRUE.
  END METHOD.

  METHOD PUBLIC VOID Run(pOpponentInputSource AS InputSource):
    THIS-OBJECT:initialize(pOpponentInputSource).

    DO WHILE THIS-OBJECT:isRunning:
      ETIME(TRUE).

      THIS-OBJECT:update().
      draw().

      THIS-OBJECT:performTiming().
    END.
  END METHOD.

  METHOD PUBLIC VOID Update():
    userPlayer:Update().
    opponentPlayer:Update().
    ball:Update().
  END METHOD.

  METHOD PUBLIC VOID Draw():
    userPlayer:Draw().
    opponentPlayer:Draw().
    ball:Draw().
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

  METHOD PRIVATE VOID createPlayers(pOpponentInputSource AS InputSource):
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
        pOpponentInputSource).
  END METHOD.

  METHOD PRIVATE VOID createBall():
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

  END METHOD.

  PROCEDURE Sleep EXTERNAL "kernel32.DLL":
    DEFINE INPUT PARAMETER intMilliseconds AS LONG.
  END PROCEDURE.

END CLASS.
