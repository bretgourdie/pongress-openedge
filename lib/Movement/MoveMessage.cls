CLASS lib.Movement.MoveMessage ABSTRACT:

  DEF PROTECTED PROPERTY XDirection AS INT NO-UNDO
    GET.
    SET.

  DEF PROTECTED PROPERTY YDirection AS INT NO-UNDO
    GET.
    SET.

  METHOD PUBLIC INT GetXVector():
    RETURN XDirection.
  END METHOD.

  METHOD PUBLIC INT GetYVector():
    RETURN YDirection.
  END METHOD.

END CLASS.