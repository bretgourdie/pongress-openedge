ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Movement.MoveMessage.
USING lib.Movement.PlayerMoveUp.
USING lib.Movement.PlayerMoveDown.
USING lib.Movement.NoMovement.

CLASS lib.Input.InputSource ABSTRACT:

  DEF PROTECTED PROPERTY PlayerMoveUp AS PlayerMoveUp NO-UNDO
    GET.
    PRIVATE SET.

  DEF PROTECTED PROPERTY PlayerMoveDown AS PlayerMoveDown NO-UNDO
    GET.
    PRIVATE SET.

  DEF PROTECTED PROPERTY NoMovement AS NoMovement NO-UNDO
    GET.
    PRIVATE SET.

  CONSTRUCTOR PUBLIC InputSource():
    ASSIGN
      THIS-OBJECT:PlayerMoveUp = NEW PlayerMoveUp()
      THIS-OBJECT:PlayerMoveDown = NEW PlayerMoveDown()
      THIS-OBJECT:NoMovement = NEW NoMovement().
  END CONSTRUCTOR.

  METHOD PUBLIC ABSTRACT MoveMessage HandleInput().

END CLASS.
