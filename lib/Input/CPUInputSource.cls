ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Input.InputSource.

CLASS lib.Input.CPUInputSource ABSTRACT INHERITS InputSource:

  DEF PROTECTED PROPERTY difficulty AS INT NO-UNDO
    GET.
    PRIVATE SET.

  CONSTRUCTOR PUBLIC CPUInputSource(pDifficulty AS INT):
    SUPER(FALSE).

    ASSIGN THIS-OBJECT:difficulty = pDifficulty.
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC VOID HandleInput():
    /* find ball */

    /* calculate difficulty speed */

    /* travel to ball */
  END METHOD.
END CLASS.
