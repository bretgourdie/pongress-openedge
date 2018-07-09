USING lib.Constants.
USING lib.TheField.
USING lib.Movement.MoveMessage.

CLASS lib.GameObjects.GameObject ABSTRACT:

  DEF PROTECTED PROPERTY minX AS INT NO-UNDO
    GET:
      RETURN 0.
    END GET.
  DEF PROTECTED PROPERTY maxX AS INT NO-UNDO
    GET:
      RETURN Constants:GameAreaWidth - THIS-OBJECT:theWidth.
    END GET.

  DEF PROTECTED PROPERTY minY AS INT NO-UNDO
    GET:
      RETURN 0.
    END GET.
  DEF PROTECTED PROPERTY maxY AS INT NO-UNDO
    GET:
      RETURN Constants:GameAreaHeight - THIS-OBJECT:theHeight.
    END GET.

  DEF PUBLIC PROPERTY XLoc AS INT NO-UNDO
    GET.
    PROTECTED SET.

  DEF PUBLIC PROPERTY YLoc AS INT NO-UNDO
    GET.
    PROTECTED SET.

  DEF PUBLIC PROPERTY originalXLoc AS INT NO-UNDO
    GET.
    PROTECTED SET.

  DEF PUBLIC PROPERTY originalYLoc AS INT NO-UNDO
    GET.
    PROTECTED SET.

  DEF PROTECTED PROPERTY theWidth AS INT NO-UNDO
    GET.
    PRIVATE SET.

  DEF PROTECTED PROPERTY theHeight AS INT NO-UNDO
    GET.
    PRIVATE SET.

  DEF PROTECTED PROPERTY theField AS TheField NO-UNDO
    GET.
    PRIVATE SET.

  CONSTRUCTOR PUBLIC GameObject(
    pHeight AS INT,
    pWidth AS INT,
    pStartX AS INT,
    pStartY AS INT,
    pTheField AS TheField):

    setDimensions(pHeight, pWidth).
    MoveToAbsolutePosition(pStartX, pStartY).
    ASSIGN
      THIS-OBJECT:theField = pTheField
      THIS-OBJECT:originalXLoc = pStartX
      THIS-OBJECT:originalYLoc = pStartY.

  END CONSTRUCTOR.

  METHOD PUBLIC ABSTRACT VOID Draw().

  METHOD PUBLIC ABSTRACT VOID Update().

  METHOD PUBLIC VOID MoveRelativeToCurrentPosition(
    pMoveMessage AS MoveMessage):
    IF VALID-OBJECT(pMoveMessage) THEN
    DO:  
      MoveRelativeToCurrentPosition(
        pMoveMessage:GetXVector(),
        pMoveMessage:GetYVector()).
    END.
  END METHOD.

  METHOD PUBLIC VOID MoveRelativeToCurrentPosition(
    pDeltaX AS INT,
    pDeltaY AS INT):

/*     IF pDeltaY > 0 THEN                                             */
/*     DO:                                                             */
/*       MESSAGE                                                       */
/*         "YLoc:" YLoc SKIP                                           */
/*         "pDeltaY:" pDeltaY SKIP                                     */
/*         VIEW-AS ALERT-BOX                                           */
/*         TITLE "GameObject.MoveRElativeTOCurrnetPosition(int, int)". */
/*     END.                                                            */

    ASSIGN 
      XLoc = MAX(MIN(XLoc + pDeltaX, Constants:GameAreaWidth - theWidth), 0)
      YLoc = MAX(MIN(YLoc + pDeltaY, Constants:GameAreaHeight - theHeight), 0).
  END METHOD.

  METHOD PUBLIC VOID MoveToAbsolutePosition(
    newX AS INT,
    newY AS INT):
    ASSIGN
      XLoc = MIN(MAX(newX, minX), maxX)
      YLoc = MIN(MAX(newY, minY), maxY).
  END METHOD.

  METHOD PROTECTED VOID setDimensions(
    pHeight AS INT,
    pWidth AS INT):
    ASSIGN
      theHeight = pHeight
      theWidth = pWidth.
  END METHOD.

  METHOD PUBLIC LOG CanMoveX(
    pDeltaX AS INT):
    RETURN CanMove(XLoc, pDeltaX, minX, maxX).
  END METHOD.

  METHOD PUBLIC LOG CanMoveY(
    pDeltaY AS INT):
    RETURN CanMove(YLoc, pDeltaY, minY, maxY).
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
