
USING lib.Movement.PlayerMoveMessage.

CLASS lib.Movement.PlayerMoveDown INHERITS PlayerMoveMessage:
  
  CONSTRUCTOR PUBLIC PlayerMoveDown():
    SUPER().
    ASSIGN YDirection = 1.
  END CONSTRUCTOR.
END CLASS.
