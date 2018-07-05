ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.InputSource.

CLASS lib.Player:

  DEF PRIVATE PROPERTY inputSource AS InputSource NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC Player(pInputSource AS InputSource):
    ASSIGN
      THIS-OBJECT:inputSource = pInputSource.
  END CONSTRUCTOR.



END CLASS.
