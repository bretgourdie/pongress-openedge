ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.GameObjects.Player.
USING lib.GameObjects.Ball.
USING lib.Input.InputSource.

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

  DEF PRIVATE PROPERTY framesPerSecond AS INT NO-UNDO
    GET:
      RETURN 30.
    END GET.

  DEF PRIVATE PROPERTY millisecondsInOneSecond AS INT NO-UNDO
    GET:
      RETURN 1000.
    END GET.

  DEF PRIVATE PROPERTY frameHandle AS HANDLE NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC GameEngine(pFrameHandle AS HANDLE):
    frameHandle = pFrameHandle.
  END CONSTRUCTOR.

  METHOD PRIVATE VOID initialize():

  END METHOD.

  METHOD PUBLIC VOID Run():
    THIS-OBJECT:initialize().

    DO WHILE THIS-OBJECT:isRunning:
      DEF VAR lStartTime AS INT NO-UNDO.
      ASSIGN lStartTime = ETIME(TRUE).

      THIS-OBJECT:update().
      THIS-OBJECT:draw().

      THIS-OBJECT:performTiming(lStartTime).
    END.
  END METHOD.

  METHOD PUBLIC VOID Update():

  END METHOD.

  METHOD PUBLIC VOID Draw():

  END METHOD.

  METHOD PRIVATE VOID performTiming(lStartTime AS INT):
    DEF VAR lEndTime AS INT NO-UNDO.
    ASSIGN lEndTime = ETIME(FALSE).

    DEF VAR lTime AS INT NO-UNDO.
    ASSIGN lTime = 
      (THIS-OBJECT:millisecondsInOneSecond / THIS-OBJECT:framesPerSecond) 
      - (lEndTime - lStartTime).

    IF lTime > 0 THEN
    DO:
      RUN Sleep(lTime).
    END.
  END METHOD.

  METHOD PRIVATE VOID createPlayers(pOpponentInputSource AS InputSource):
    ASSIGN
      THIS-OBJECT:userPlayer = NEW Player(NEW UserInputSource())
      THIS-OBJECT:opponentPlayer = NEW Player(pOpponentInputSource).
  END METHOD.

  PROCEDURE Sleep EXTERNAL "kernel32.DLL":
    DEFINE INPUT PARAMETER intMilliseconds AS LONG.
  END PROCEDURE.

END CLASS.
