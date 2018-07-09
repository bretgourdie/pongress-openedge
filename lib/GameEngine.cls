ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.GameObjects.GameObject.
USING lib.GameObjects.Player.
USING lib.GameObjects.Ball.
USING lib.Input.CPUInputSource.
USING lib.Input.UserInputSource.
USING lib.Constants.
USING lib.TheField.

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

  DEF PRIVATE PROPERTY theField AS TheField NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY isRunning AS LOG NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY gameFieldFrameHandle AS HANDLE NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC GameEngine(pGameFieldFrameHandle AS HANDLE):
    gameFieldFrameHandle = pGameFieldFrameHandle.
  END CONSTRUCTOR.

  METHOD PRIVATE VOID initialize():
    ASSIGN 
      theField = NEW TheField(gameFieldFrameHandle)
      isRunning = TRUE.
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
    DO lGameObjectIndex = 1 TO EXTENT(theField:GameObjects):
      theField:GameObjects[lGameObjectIndex]:Update().
    END.
  END METHOD.

  METHOD PUBLIC VOID Draw():
    DEF VAR lGameObjectIndex AS INT NO-UNDO.
    DO lGameObjectIndex = 1 TO EXTENT(theField:GameObjects):
      theField:GameObjects[lGameObjectIndex]:Draw().
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

  PROCEDURE Sleep EXTERNAL "kernel32.DLL":
    DEFINE INPUT PARAMETER intMilliseconds AS LONG.
  END PROCEDURE.

END CLASS.
