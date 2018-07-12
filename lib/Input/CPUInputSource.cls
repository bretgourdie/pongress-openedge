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

  METHOD OVERRIDE PUBLIC INT HandleInput():
    DEF VAR lYDirection AS INT NO-UNDO INITIAL 0.
    RETURN lYDirection.
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
