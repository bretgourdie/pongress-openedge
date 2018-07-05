USING lib.Constants.

CLASS lib.GameObjects.GameObject ABSTRACT:

  DEF PROTECTED PROPERTY minX AS INT
    GET:
      RETURN 0.
    END GET.
  DEF PROTECTED PROPERTY maxX AS INT
    GET:
      RETURN Constants:GameAreaWidth - THIS-OBJECT:width.
    END GET.

  DEF PROTECTED PROPERTY minY AS INT
    GET:
      RETURN 0.
    END GET.
  DEF PROTECTED PROPERTY maxY AS INT
    GET:
      RETURN Constants:GameAreaHeight - THIS-OBJECT:height.
    END GET.

  DEF PUBLIC PROPERTY X AS INT
    GET.
    PROTECTED SET.

  DEF PUBLIC PROPERTY Y AS INT
    GET.
    PROTECTED SET.

  DEF PROTECTED PROPERTY width AS INT
    GET.
    PRIVATE SET.

  DEF PROTECTED PROPERTY height AS INT
    GET.
    PRIVATE SET.

  CONSTRUCTOR PUBLIC GameObject(
    pHeight AS INT,
    pWidth AS INT,
    pStartX AS INT,
    pStartY AS INT):
    setDimensions(pHeight, pWidth).
    MoveToAbsolutePosition(pStartX, pStartY).
  END CONSTRUCTOR.

  METHOD PUBLIC ABSTRACT VOID Draw().

  METHOD PUBLIC VOID MoveRelativeToCurrentPosition(
    pDeltaX AS INT,
    pDeltaY AS INT):

    IF CanMoveX(pDeltaX) THEN
    DO:
      THIS-OBJECT:X = THIS-OBJECT:X + pDeltaX.
    END.

    IF CanMoveY(pDeltaY) THEN
    DO:
      THIS-OBJECT:Y = THIS-OBJECT:Y + pDeltaY.
    END.
  END METHOD.

  METHOD PUBLIC VOID MoveToAbsolutePosition(
    newX AS INT,
    newY AS INT):
    ASSIGN
      THIS-OBJECT:X = MIN(MAX(newX, minX), maxX)
      THIS-OBJECT:Y = MIN(MAX(newY, minY), maxY).
  END METHOD.

  METHOD PROTECTED VOID setDimensions(
    pHeight AS INT,
    pWidth AS INT):
    ASSIGN
      THIS-OBJECT:height = pHeight
      THIS-OBJECT:width = pWidth.
  END METHOD.

  METHOD PUBLIC LOG CanMoveX(
    pDeltaX AS INT):
    RETURN CanMove(THIS-OBJECT:X, pDeltaX, minX, maxX).
  END METHOD.

  METHOD PUBLIC LOG CanMoveY(
    pDeltaY AS INT):
    RETURN CanMove(THIS-OBJECT:Y, pDeltaY, minY, maxY).
  END METHOD.

  METHOD PRIVATE LOG CanMove(
    pCurrentPoint AS INT,
    pNewOffset AS INT,
    pMinPoint AS INT,
    pMaxPoint AS INT):
    RETURN pCurrentPoint + pNewOffset >= pMinPoint
      AND pCurrentPoint + pNewOffset <= pMaxPoint.
  END METHOD.
END CLASS.
