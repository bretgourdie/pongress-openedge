ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS lib.Sprites.Sprite ABSTRACT:

  DEF PRIVATE PROPERTY spriteColorCode AS INT NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC Sprite(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE,
    pSpriteColorCode AS INT):
    createSprite(pStartX, pStartY, pParentFrame, pSpriteColorCode).
  END CONSTRUCTOR.

  METHOD PROTECTED ABSTRACT VOID createSprite(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE,
    pSpriteColorCode AS INT).

  METHOD PUBLIC ABSTRACT VOID Draw(
    pXLoc AS INT,
    pYLoc AS INT).

  METHOD PUBLIC ABSTRACT VOID SetStateColor(pState AS CHAR).

END CLASS.
