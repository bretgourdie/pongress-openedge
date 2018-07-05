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

    IF THIS-OBJECT:X + pDeltaX >= THIS-OBJECT:minX
      AND THIS-OBJECT:X + pDeltaX <= THIS-OBJECT:maxX THEN
    DO:
      THIS-OBJECT:X = THIS-OBJECT:X + pDeltaX.
    END.

    IF THIS-OBJECT:Y + pDeltaY >= THIS-OBJECT:minY
      AND THIS-OBJECT:Y + pDeltaY <= THIS-OBJECT:maxY THEN
    DO:
      THIS-OBJECT:Y = THIS-OBJECT:Y + pDeltaY.
    END.
  END METHOD.

  METHOD PUBLIC VOID MoveToAbsolutePosition(
    newX AS INT,
    newY AS INT):
    ASSIGN
      THIS-OBJECT:X = MIN(MAX(newX, THIS-OBJECT:minX), THIS-OBJECT:maxX)
      THIS-OBJECT:X = MIN(MAX(newY, THIS-OBJECT:minY), THIS-OBJECT:maxY).
  END METHOD.

  METHOD PROTECTED VOID setDimensions(
    pHeight AS INT,
    pWidth AS INT):
    ASSIGN
      THIS-OBJECT:height = pHeight
      THIS-OBJECT:width = pWidth.
  END METHOD.
END CLASS.
