ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Input.InputSource.
USING lib.GameObjects.GameObject.

CLASS lib.GameObjects.Player INHERITS GameObject:

  DEF PRIVATE PROPERTY inputSource AS InputSource NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC Player(
    pStartX AS INT,
    pStartY AS INT,
    pInputSource AS InputSource):
    
    DEF VAR lHeight AS INT NO-UNDO INITIAL 28.
    DEF VAR lWidth AS INT NO-UNDO INITIAL 2.

    SUPER(lHeight, lWidth, pStartX, pStartY).
    ASSIGN
      THIS-OBJECT:inputSource = pInputSource.
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC VOID Draw():
  END METHOD.

END CLASS.
