ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Input.InputSource.
USING lib.GameObjects.GameObject.
USING lib.Constants.
USING lib.Sprites.Sprite.
USING lib.Sprites.PlayerSprite.
USING lib.TheField.

CLASS lib.GameObjects.Player INHERITS GameObject:

  DEF PRIVATE PROPERTY inputSource AS InputSource NO-UNDO
    GET.
    SET.

  DEF PUBLIC PROPERTY Score AS INT NO-UNDO
    GET.
    PRIVATE SET.

  DEFINE PUBLIC EVENT PlayerScoreUpdated
    SIGNATURE VOID (pCurrentScore AS INT).

  CONSTRUCTOR PUBLIC Player(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE,
    pInputSource AS InputSource,
    pTheField AS TheField):

    SUPER(Constants:PlayerHeight, Constants:PlayerWidth, pStartX, pStartY, pTheField).

    ASSIGN sprite = NEW PlayerSprite(pStartX, pStartY, pParentFrame, Constants:WhiteColorCode).

    ASSIGN
      THIS-OBJECT:inputSource = pInputSource.
  END CONSTRUCTOR.

  METHOD OVERRIDE PUBLIC VOID Update():
    DEF VAR lYDirection AS INT NO-UNDO.
    ASSIGN lYDirection = inputSource:HandleInput(theField).

    YVelocity = lYDirection * Constants:PlayerMovementVectorMultiplier.
    MoveRelativeToCurrentPosition().
  END METHOD.

  METHOD OVERRIDE PROTECTED VOID subscribeToEvents():
  END METHOD.

  METHOD PUBLIC VOID IncrementScore():
    Score = Score + 1.
    PlayerScoreUpdated:Publish(Score).
  END METHOD.

  METHOD PUBLIC VOID ResetScore():
    Score = 0.
    PlayerScoreUpdated:Publish(Score).
  END METHOD.

END CLASS.
