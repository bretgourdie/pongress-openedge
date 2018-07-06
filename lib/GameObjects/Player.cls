ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Input.InputSource.
USING lib.GameObjects.GameObject.
USING lib.Constants.

USING lib.Movement.MoveMessage.

CLASS lib.GameObjects.Player INHERITS GameObject:

  DEF PRIVATE PROPERTY spriteHandle AS HANDLE NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY inputSource AS InputSource NO-UNDO
    GET.
    SET.

  DEF PUBLIC PROPERTY Score AS INT NO-UNDO
    GET.
    PRIVATE SET.

  CONSTRUCTOR PUBLIC Player(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE,
    pInputSource AS InputSource):

    SUPER(Constants:PlayerHeight, Constants:PlayerWidth, pStartX, pStartY).

    CREATE RECTANGLE spriteHandle ASSIGN
      X = pStartX
      Y = pStartY
      BGCOLOR = Constants:WhiteColorCode
      FGCOLOR = Constants:WhiteColorCode
      HEIGHT-PIXELS = Constants:PlayerHeight
      WIDTH-PIXELS = Constants:PlayerWidth
      FRAME = pParentFrame
      VISIBLE = TRUE.

    ASSIGN
      THIS-OBJECT:inputSource = pInputSource.
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC VOID Draw():
    ASSIGN
      spriteHandle:X = XLoc
      spriteHandle:Y = YLoc.
  END METHOD.

  METHOD OVERRIDE PUBLIC VOID Update():
    DEF VAR lMoveMessage AS MoveMessage NO-UNDO.
    ASSIGN lMoveMessage = inputSource:HandleInput().
    MoveRelativeToCurrentPosition(lMoveMessage).
  END METHOD.

  METHOD PUBLIC VOID IncrementScore():
    Score = Score + 1.
  END METHOD.

  METHOD PUBLIC VOID ResetScore():
    Score = 0.
  END METHOD.

END CLASS.
