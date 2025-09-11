module Draw where

import Types
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Types (Fighter(fighterTamanho))





window :: Display
window = InWindow "" (1920,1080) (0,0)

background :: Color
background = greyN 0

desenha :: World -> Picture
desenha w@(World { player1 = p1, player2 = p2, mapa = map}) =
  Pictures [desenhaMapa map, desenhaPlayer1 p1, desenhaHitbox p1, desenhaPlayer2 p2, desenhaHitbox p2]


desenhaPlayer1 :: Fighter -> Picture
desenhaPlayer1 p1@(Fighter {fighterPos = (x,y), fighterTamanho = altura,fighterDir = dir, fighterStance = stance}) = 
    case stance of
        Crouching -> corpoCrouching
        _ -> corpoStanding
    where
        largura = altura / 2
        corpoStanding = Color red $ Translate (x + largura/2) (y + altura/2 - 450) $ rectangleSolid largura altura
        corpoCrouching = Color red $ Translate (x + largura/2) (altura/2 - altura/3 - 450) $ rectangleSolid largura (altura / 3)



desenhaPlayer2 :: Fighter -> Picture
desenhaPlayer2 p1@(Fighter {fighterPos = (x,y), fighterTamanho = altura, fighterDir = dir, fighterStance = stance}) = 
    case stance of
        Crouching -> corpoCrouching
        _ -> corpoStanding
    where
        largura = altura / 2
        corpoStanding = Color blue $ Translate (x + largura/2) (y + altura/2 - 450) $ rectangleSolid largura altura
        corpoCrouching = Color blue $ Translate (x + largura/2) (altura/2 - altura/3 - 450) $ rectangleSolid largura (altura / 3)



desenhaMapa :: Mapa -> Picture
desenhaMapa mapa@(Mapa esq chao dir) = Pictures [paredeEsq, floor, paredeDir]
    where
        largura = 60
        paredeEsq = Color white $ Translate (esq - largura/2) 0 $ rectangleSolid largura 1060
        floor = Color white $ Translate 0 (chao - largura/2 - 450) $ rectangleSolid 1920 largura
        paredeDir = Color white $ Translate (dir + largura/2) 0 $ rectangleSolid largura 1060

desenhaHitbox :: Fighter -> Picture
desenhaHitbox f@(Fighter {fighterPos = (x,y), fighterDir = dir, fighterStance = stance, keyLeft = kl, keyRight = kr}) =
  case normalAttackHitbox f of
    Nothing -> Blank
    Just (cx, w, h) ->
        case stance of
            Standing ->
                if kl && not kr then Translate (largura * x + largura/2 - cx) (altura * y + altura/2 - 420) $ Color orange $ rectangleSolid w h
                else if not kl && kr then Translate (largura * x + largura/2 + cx) (altura * y + altura/2 - 420) $ Color orange $ rectangleSolid w h
                else if dir == Esquerda then Translate (largura * x + largura/2 - cx) (altura * y + altura/2 - 420) $ Color orange $ rectangleSolid w h
                else Translate (largura * x + largura/2 + cx) (altura * y + altura/2 - 420) $ Color orange $ rectangleSolid w h
            Crouching -> Translate (largura * x + largura/2) (altura * y + altura/2 - 545) $ Color orange $ rectangleSolid w h
            Jumping -> Translate (largura * x + largura/2) (altura * y + altura/2 - 400 + cx) $ Rotate 90 $ Color orange $ rectangleSolid w h
            Falling -> Translate (largura * x + largura/2) (altura * y + altura/2 - 400 + cx) $ Rotate (-90) $ Color orange $ rectangleSolid w h
            where
                altura = 250
                largura = altura / 2

normalAttackHitbox :: Fighter -> Maybe (Float, Float, Float)
normalAttackHitbox (Fighter { normalAttack = Nothing }) = Nothing
normalAttackHitbox f@(Fighter { normalAttack = Just (AttackInstance phase _)
                             , fighterDir = dir }) =
  let def   = defaultNormalAttack
      w     = naWidth def
      h     = naHeight def
      mult = case phase of
        Windup   -> 0.5
        Peak     -> 1
        Recovery -> 0.5
      w'   = w * mult
      h'   = h
      offX = w'/2
  in Just (offX, w', h')