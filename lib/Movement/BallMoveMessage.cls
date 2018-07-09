USING lib.Constants.
USING lib.Movement.MoveMessage.

CLASS lib.Movement.BallMoveMessage ABSTRACT INHERITS MoveMessage:

  METHOD PUBLIC OVERRIDE INT GetXVector():
    RETURN SUPER:GetXVector() * Constants:BallMovementVectorMultiplier.
  END METHOD.


  METHOD PUBLIC OVERRIDE INT GetYVector():
    RETURN SUPER:GetYVector() * Constants:BallMovementVectorMultiplier.
  END METHOD.

END CLASS.
