ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.GameObjects.GameObject.
USING lib.Constants.

CLASS lib.GameObjects.Ball INHERITS GameObject:

  DEF PRIVATE PROPERTY spriteHandle AS HANDLE NO-UNDO
    GET.
    SET.

  DEF PUBLIC PROPERTY Velocity AS INT NO-UNDO
    GET.
    PRIVATE SET.

  DEF PUBLIC PROPERTY XDirection AS INT NO-UNDO
    GET.
    PRIVATE SET.

  DEF PUBLIC PROPERTY YDirection AS INT NO-UNDO
    GET.
    PRIVATE SET.

  DEF PRIVATE PROPERTY parentFrame AS HANDLE NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC Ball(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE):
    SUPER(Constants:BallHeight, Constants:BallWidth, pStartX, pStartY).

    CREATE RECTANGLE spriteHandle ASSIGN
      X = pStartX
      Y = pStartY
      BGCOLOR = Constants:WhiteColorCode
      FGCOLOR = Constants:WhiteColorCode
      HEIGHT-PIXELS = Constants:BallHeight
      WIDTH-PIXELS = Constants:BallWidth
      FRAME = pParentFrame
      VISIBLE = TRUE.
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC VOID Update():
    DEF VAR lStayXDistance AS INT NO-UNDO.
    DEF VAR lStayYDistance AS INT NO-UNDO.
    DEF VAR lFlippedXDistance AS INT NO-UNDO.
    DEF VAR lFlippedYDistance AS INT NO-UNDO.
    ASSIGN
      lStayXDistance = Velocity * XDirection
      lStayYDistance = Velocity * YDirection
      lFlippedXDistance = Velocity * XDirection * -1
      lFlippedYDistance = Velocity * YDirection * -1.

    DEF VAR lNewXDistance AS INT NO-UNDO.
    DEF VAR lNewYDistance AS INT NO-UNDO.

    IF CanMoveX(lStayXDistance) THEN
    DO:
      ASSIGN lNewXDistance = lStayXDistance.
    END.
    ELSE DO:
      ASSIGN
        lNewXDistance = lFlippedXDistance
        XDirection = XDirection * -1.
    END.

    IF CanMoveY(lStayYDistance) THEN
    DO:
      ASSIGN lNewYDistance = lStayYDistance.
    END.
    ELSE DO:
      ASSIGN
        lNewYDistance = lFlippedYDistance
        YDirection = YDirection * -1.
    END.

    MoveRelativeToCurrentPosition(lNewXDistance, lNewYDistance).
  END METHOD.

  METHOD OVERRIDE PUBLIC VOID Draw():
    ASSIGN
      spriteHandle:X = XLoc
      spriteHandle:Y = YLoc.
  END METHOD.
END CLASS.
