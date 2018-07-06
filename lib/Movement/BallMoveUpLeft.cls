USING lib.Movement.BallMoveMessage.

CLASS lib.Movement.BallMoveUpLeft INHERITS BallMoveMessage:
  
  CONSTRUCTOR PUBLIC BallMoveUpLeft():
    ASSIGN
      XDirection = -1
      YDirection = -1.
  END CONSTRUCTOR.

END CLASS.
