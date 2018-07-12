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
    subscribeToEvents().
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC MoveMessage HandleInput():
    RETURN THIS-OBJECT:NoMovement.
  END METHOD.

  METHOD PROTECTED VOID subscribeToEvents():
    IF VALID-OBJECT(theField:Ball) THEN
    DO:
      theField:Ball:BallMoved:Subscribe(handleBallMoved).
    END.
  END METHOD.

  METHOD PRIVATE VOID handleBallMoved(pBallXLoc AS INT, pBallYLoc AS INT):
  
  END METHOD.
END CLASS.
