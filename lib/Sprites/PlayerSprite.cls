USING lib.Constants.
USING lib.Sprites.Sprite.

CLASS lib.Sprites.PlayerSprite INHERITS Sprite:
  
  DEF PRIVATE PROPERTY spriteHandle AS HANDLE NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC PlayerSprite(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE,
    pSpriteColorCode AS INT):
    SUPER(pStartX, pStartY, pParentFrame, pSpriteColorCode).
  END CONSTRUCTOR.

  METHOD PROTECTED OVERRIDE VOID createSprite(
    pStartX AS INT,
    pStartY AS INT,
    pParentFrame AS HANDLE,
    pSpriteColorCode AS INT):

    CREATE RECTANGLE spriteHandle ASSIGN
      X = pStartX
      Y = pStartY
      BGCOLOR = pSpriteColorCode
      FGCOLOR = pSpriteColorCode
      HEIGHT-PIXELS = Constants:PlayerHeight
      WIDTH-PIXELS = Constants:PlayerWidth
      FRAME = pParentFrame
      VISIBLE = TRUE.
    
  END METHOD.

  METHOD PUBLIC OVERRIDE VOID Draw(
    pXLoc AS INT,
    pYLoc AS INT):
    
    ASSIGN
      spriteHandle:X = pXLoc
      spriteHandle:Y = pYLoc.
  END METHOD.

  METHOD PUBLIC OVERRIDE VOID SetStateColor(pState AS CHAR):
    /* don't change state color */
  END METHOD.
  
END CLASS.
