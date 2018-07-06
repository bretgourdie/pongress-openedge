
USING lib.Movement.BallMoveMessage.

CLASS lib.Movement.BallMoveUpRight INHERITS BallMoveMessage:
  
  CONSTRUCTOR PUBLIC BallMoveUpRight():
    ASSIGN
      XDirection =  1
      YDirection = -1.
  END CONSTRUCTOR.

END CLASS.
