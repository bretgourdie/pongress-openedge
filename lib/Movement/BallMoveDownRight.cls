
USING lib.Movement.BallMoveMessage.

CLASS lib.Movement.BallMoveDownRight INHERITS BallMoveMessage:
  
  CONSTRUCTOR PUBLIC BallMoveDownRight():
    ASSIGN
      XDirection =  1
      YDirection =  1.
  END CONSTRUCTOR.

END CLASS.
