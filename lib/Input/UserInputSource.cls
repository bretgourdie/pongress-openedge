ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Input.InputSource.
USING lib.Movement.MoveMessage.
USING lib.Movement.PlayerMoveMessage.

CLASS lib.Input.UserInputSource INHERITS InputSource:

  DEF PRIVATE PROPERTY UpArrow AS INT NO-UNDO
    GET:
      RETURN 501.
    END GET.

  DEF PRIVATE PROPERTY DownArrow AS INT NO-UNDO
    GET:
      RETURN 502.
    END GET.

  DEF PRIVATE PROPERTY RightArrow AS INT NO-UNDO
    GET:
      RETURN 503.
    END GET.

  DEF PRIVATE PROPERTY LeftArrow AS INT NO-UNDO
    GET:
      RETURN 504.
    END GET.

  DEF PRIVATE PROPERTY Spacebar AS INT NO-UNDO
    GET:
      RETURN 32.
    END GET.
  
  CONSTRUCTOR PUBLIC UserInputSource():
    SUPER().
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC MoveMessage HandleInput():
    READKEY PAUSE 0.

    CASE LASTKEY:
      WHEN UpArrow THEN RETURN moveUp().
      WHEN DownArrow THEN RETURN moveDown().
      WHEN SpaceBar THEN togglePause().
      OTHERWISE RETURN otherPresses().
    END CASE.
    
  END METHOD.

  METHOD PRIVATE PlayerMoveMessage moveUp():
    RETURN THIS-OBJECT:PlayerMoveUp.
  END METHOD.

  METHOD PRIVATE PlayerMoveMessage moveDown():
    RETURN THIS-OBJECT:PlayerMoveDown.
  END METHOD.

  METHOD PRIVATE MoveMessage togglePause():
    MESSAGE "Please implement me"
      VIEW-AS ALERT-BOX
      TITLE "UserInputSource.togglePause()".
    RETURN ?.
  END METHOD.

  METHOD PRIVATE MoveMessage otherPresses():
    RETURN THIS-OBJECT:NoMovement.
  END METHOD.
END CLASS.
