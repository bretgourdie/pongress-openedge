
USING lib.Movement.BallMoveMessage.

CLASS lib.Movement.BallMoveDownLeft INHERITS BallMoveMessage:
  
  CONSTRUCTOR PUBLIC BallMoveDownLeft():
    ASSIGN
      XDirection = -1
      YDirection =  1.
  END CONSTRUCTOR.

END CLASS.
