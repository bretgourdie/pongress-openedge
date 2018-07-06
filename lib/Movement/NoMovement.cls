USING lib.Movement.MoveMessage.

CLASS lib.Movement.NoMovement INHERITS MoveMessage:
  
  CONSTRUCTOR PUBLIC NoMovement():
    SUPER().
    ASSIGN
      XDirection = 0
      YDirection = 0.
  END CONSTRUCTOR.
END CLASS.
