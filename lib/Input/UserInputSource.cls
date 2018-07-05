ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Input.InputSource.

CLASS lib.Input.UserInputSource INHERITS InputSource:
  
  CONSTRUCTOR PUBLIC UserInputSource():
    SUPER(TRUE).
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC VOID HandleInput():
    /* get user input */
  END METHOD.
END CLASS.
