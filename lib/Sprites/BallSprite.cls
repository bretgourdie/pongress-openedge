ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING lib.Constants.
USING lib.Sprites.Sprite.

CLASS lib.Sprites.BallSprite INHERITS Sprite:

  DEF PRIVATE PROPERTY spriteInner AS HANDLE NO-UNDO
    GET.  
    SET.

  DEF PRIVATE PROPERTY spriteMiddle AS HANDLE NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY spriteOuter AS HANDLE NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY parentFrame AS HANDLE NO-UNDO
    GET.
    SET.

  DEF PRIVATE PROPERTY spriteColorCode AS INT NO-UNDO
    GET.
    SET.

  CONSTRUCTOR PUBLIC BallSprite(
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
    ASSIGN spriteColorCode = pSpriteColorCode.

    CREATE RECTANGLE spriteInner ASSIGN
      X = pStartX + Constants:BallInnerSpriteXOffset
      Y = pStartY + Constants:BallInnerSpriteYOffset
      HEIGHT-PIXELS = Constants:BallInnerRectangleHeight
      WIDTH-PIXELS = Constants:BallInnerRectangleWidth
      FRAME = pParentFrame
      VISIBLE = TRUE.

    CREATE RECTANGLE spriteMiddle ASSIGN
      X = pStartX + Constants:BallMiddleSpriteXOffset
      Y = pStartY + Constants:BallMiddleSpriteYOffset
      HEIGHT-PIXELS = Constants:BallMiddleRectangleHeight
      WIDTH-PIXELS = Constants:BallMiddleRectangleWidth
      FRAME = pParentFrame
      VISIBLE = TRUE.

    CREATE RECTANGLE spriteOuter ASSIGN
      X = pStartX + Constants:BallOuterSpriteXOffset
      Y = pStartY + Constants:BallOuterSpriteYOffset
      HEIGHT-PIXELS = Constants:BallOuterRectangleHeight
      WIDTH-PIXELS = Constants:BallOuterRectangleWidth
      FRAME = pParentFrame
      VISIBLE = TRUE.
  END METHOD.

  METHOD PUBLIC OVERRIDE VOID SetStateColor(pState AS CHAR):
    CASE pState:
      WHEN "Waiting" THEN spriteColorCode = 11.
      WHEN "Boundary bounce" THEN spriteColorCode = 14.
      WHEN "Paddle bounce" THEN spriteColorCode = 10.
      WHEN "Scored" THEN spriteColorCode = 12.
      OTHERWISE spriteColorCode = Constants:WhiteColorCode.
    END CASE.
  END METHOD.

  METHOD PUBLIC OVERRIDE VOID Draw(pXLoc AS INT, pYLoc AS INT):
    ASSIGN
      spriteInner:X = pXLoc + Constants:BallInnerSpriteXOffset
      spriteInner:Y = pYLoc + Constants:BallInnerSpriteYOffset
      spriteInner:BGCOLOR = spriteColorCode
      spriteInner:FGCOLOR = spriteColorCode
      spriteMiddle:X = pXLoc + Constants:BallMiddleSpriteXOffset
      spriteMiddle:Y = pYLoc + Constants:BallMiddleSpriteYOffset
      spriteMiddle:BGCOLOR = spriteColorCode
      spriteMiddle:FGCOLOR = spriteColorCode
      spriteOuter:X = pXLoc + Constants:BallOuterSpriteXOffset
      spriteOuter:Y = pYLoc + Constants:BallOuterSpriteYOffset
      spriteOuter:BGCOLOR = spriteColorCode
      spriteOuter:FGCOLOR = spriteColorCode.
  END METHOD.

END CLASS.