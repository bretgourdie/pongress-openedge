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
    IF theField:Ball:YLoc + theField:Ball:theWidth / 2
      > theField:CPUPlayer:YLoc THEN
    DO:
      RETURN PlayerMoveUp.
    END.

    ELSE IF theField:Ball:YLoc + theField:Ball:theWidth / 2
      < theField:CPUPlayer:YLoc THEN
    DO:
      RETURN PlayerMoveDown.
    END.

    RETURN THIS-OBJECT:NoMovement.
  END METHOD.
END CLASS.
