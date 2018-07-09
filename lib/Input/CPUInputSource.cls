ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Input.InputSource.
USING lib.Movement.MoveMessage.

CLASS lib.Input.CPUInputSource INHERITS InputSource:

  CONSTRUCTOR PUBLIC CPUInputSource():
    SUPER().
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC MoveMessage HandleInput():
    /* find ball */

    /* travel to ball */

    RETURN THIS-OBJECT:NoMovement.
  END METHOD.
END CLASS.
