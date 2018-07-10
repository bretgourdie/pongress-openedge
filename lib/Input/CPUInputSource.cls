ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Input.InputSource.
USING lib.Movement.MoveMessage.
USING lib.TheField.

CLASS lib.Input.CPUInputSource INHERITS InputSource:

  DEF PRIVATE PROPERTY theField AS TheField NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC CPUInputSource(pTheField AS TheField):
    SUPER().
    ASSIGN
      theField = pTheField.
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC MoveMessage HandleInput():
    IF theField:Ball:BallMoveMessage NE theField:Ball:NoMovement THEN
    DO:
      DEF VAR lRelativeBallYPosition AS INT NO-UNDO.
      ASSIGN
        lRelativeBallYPosition =
          theField:Ball:YLoc + theField:Ball:TheWidth / 2.
  
      IF lRelativeBallYPosition > theField:CPUPlayer:YLoc THEN
      DO:
        RETURN PlayerMoveDown.
      END.
  
      ELSE IF lRelativeBallYPosition < theField:CPUPlayer:YLoc THEN
      DO:
        RETURN PlayerMoveUp.
      END.
    END.

    RETURN THIS-OBJECT:NoMovement.
  END METHOD.
END CLASS.
