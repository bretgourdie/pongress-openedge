USING lib.Movement.MoveMessage.
USING lib.Constants.

CLASS lib.Movement.PlayerMoveMessage ABSTRACT INHERITS MoveMessage:
  
  CONSTRUCTOR PUBLIC PlayerMoveMessage():
    SUPER().
    ASSIGN XDirection = 0.
  END CONSTRUCTOR.

  METHOD PUBLIC OVERRIDE INT GetYVector():
    RETURN SUPER:GetYVector() * Constants:PlayerMovementVectorMultiplier.
  END METHOD.
END CLASS.
