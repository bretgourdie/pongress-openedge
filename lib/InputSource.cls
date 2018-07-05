ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.InputSource.

CLASS lib.InputSource INHERITS lib.Enum:

  DEF PUBLIC STATIC PROPERTY User AS InputSource
    GET:
      IF NOT VALID-OBJECT(InputSource:User)
      THEN InputSource:User = NEW InputSource(0, "User").
      RETURN InputSource:User.
    END GET.
    PRIVATE SET.

  DEF PUBLIC STATIC PROPERTY EasyAI AS InputSource
    GET:
      IF NOT VALID-OBJECT(InputSource:EasyAI)
      THEN InputSource:EasyAI = NEW InputSource(1, "EasyAI").
      RETURN InputSource:EasyAI.
    END GET.
    PRIVATE SET.

  DEF PUBLIC STATIC PROPERTY MediumAI AS InputSource
    GET:
      IF NOT VALID-OBJECT(InputSource:MediumAI)
      THEN InputSource:MediumAI = NEW InputSource(1, "MediumAI").
      RETURN InputSource:MediumAI.
    END GET.
    PRIVATE SET.

  DEF PUBLIC STATIC PROPERTY HardAI AS InputSource
    GET:
      IF NOT VALID-OBJECT(InputSource:HardAI)
      THEN InputSource:HardAI = NEW InputSource(1, "HardAI").
      RETURN InputSource:HardAI.
    END GET.
    PRIVATE SET.

  CONSTRUCTOR PRIVATE InputSource(pValue AS INT, pLabel AS CHAR):
    ASSIGN
      THIS-OBJECT:VALUE = pValue
      THIS-OBJECT:LABEL = pLabel.
  END CONSTRUCTOR.

END CLASS.
