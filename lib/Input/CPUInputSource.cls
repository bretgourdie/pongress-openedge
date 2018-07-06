ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Input.InputSource.
USING lib.Movement.MoveMessage.

CLASS lib.Input.CPUInputSource ABSTRACT INHERITS InputSource:

  DEF PROTECTED PROPERTY difficulty AS INT NO-UNDO
    GET.
    PRIVATE SET.

  CONSTRUCTOR PUBLIC CPUInputSource(pDifficulty AS INT):
    SUPER().

    ASSIGN THIS-OBJECT:difficulty = pDifficulty.
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC MoveMessage HandleInput():
    /* find ball */

    /* calculate difficulty speed */

    /* travel to ball */

    RETURN THIS-OBJECT:NoMovement.
  END METHOD.
END CLASS.
