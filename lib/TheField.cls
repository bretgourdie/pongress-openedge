USING lib.Constants.
USING lib.GameObjects.Ball.
USING lib.GameObjects.GameObject.
USING lib.GameObjects.Player.
USING lib.Input.CPUInputSource.
USING lib.Input.UserInputSource.

CLASS lib.TheField:

  DEF PUBLIC PROPERTY UserPlayer AS Player NO-UNDO
    GET.
    PRIVATE SET.

  DEF PUBLIC PROPERTY CPUPlayer AS Player NO-UNDO
    GET.
    PRIVATE SET.

  DEF PUBLIC PROPERTY Ball AS Ball NO-UNDO
    GET.
    PRIVATE SET.

  DEF PUBLIC PROPERTY GameObjects AS GameObject EXTENT NO-UNDO
    GET.
    PRIVATE SET.

  CONSTRUCTOR PUBLIC TheField(pGameFieldFrameHandle AS HANDLE):
    createGameObjectCollection(pGameFieldFrameHandle).
  END CONSTRUCTOR.

  METHOD PRIVATE VOID createGameObjectCollection(
    pGameFieldFrameHandle AS HANDLE):
    DEF VAR lPlayers AS Player EXTENT NO-UNDO.
    DEF VAR lBall AS Ball NO-UNDO.
    ASSIGN
      lPlayers = createPlayers(pGameFieldFrameHandle)
      lBall = createBall(pGameFieldFrameHandle).

    DEF VAR lArrayLength AS INT NO-UNDO.
    ASSIGN lArrayLength = EXTENT(lPlayers) + 1 /* ball */.

    DEF VAR lGameObjects AS GameObject EXTENT.
    EXTENT(lGameObjects) = lArrayLength.

    DEF VAR lPlayerIndex AS INT NO-UNDO.
    DO lPlayerIndex = 1 TO EXTENT(lPlayers):
      lGameObjects[lPlayerIndex] = lPlayers[lPlayerIndex].
    END.

    lGameObjects[lArrayLength] = lBall.

    ASSIGN GameObjects = lGameObjects.
  END METHOD.

  METHOD PRIVATE Player EXTENT createPlayers(
    pGameFieldFrameHandle AS HANDLE):
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
      UserPlayer = NEW Player(
        lLeftStartX,
        lStartY,
        pGameFieldFrameHandle, 
        NEW UserInputSource(),
        THIS-OBJECT)
      CPUPlayer = NEW Player(
        lRightStartX,
        lStartY,
        pGameFieldFrameHandle,
        NEW CPUInputSource(THIS-OBJECT),
        THIS-OBJECT).

    DEF VAR lPlayers AS Player EXTENT 2.
    ASSIGN
      lPlayers[1] = UserPlayer
      lPlayers[2] = CPUPlayer.

    RETURN lPlayers.
  END METHOD.

  METHOD PRIVATE Ball createBall(pGameFieldFrameHandle AS HANDLE):
    DEF VAR lStartX AS INT NO-UNDO.
    DEF VAR lStartY AS INT NO-UNDO.

    ASSIGN
      lStartX = Constants:GameAreaWidth / 2
      lStartY = Constants:GameAreaHeight / 2.

    ASSIGN
      Ball = NEW Ball(
        lStartX,
        lStartY,
        pGameFieldFrameHandle,
        THIS-OBJECT).

    RETURN Ball.
  END METHOD.

END CLASS.
