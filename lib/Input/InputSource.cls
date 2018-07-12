ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Movement.MoveMessage.
USING lib.Movement.PlayerMoveUp.
USING lib.Movement.PlayerMoveDown.
USING lib.Movement.NoMovement.

CLASS lib.Input.InputSource ABSTRACT:

  METHOD PUBLIC ABSTRACT INT HandleInput().

END CLASS.
