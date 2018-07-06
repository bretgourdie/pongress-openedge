USING lib.Movement.PlayerMoveMessage.

CLASS lib.Movement.PlayerMoveUp INHERITS PlayerMoveMessage:
  
  CONSTRUCTOR PUBLIC PlayerMoveUp():
    SUPER().
    ASSIGN YDirection = -1.
  END CONSTRUCTOR.
END CLASS.
