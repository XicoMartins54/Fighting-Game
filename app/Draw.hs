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
desenha w@(World { player1 = p1, player2 = p2, mapa = mp }) =
    let
        (front, back, drawFront, drawBack) =
            if fighterTamanho p1 > fighterTamanho p2 then (p2, p1, desenhaPlayer2, desenhaPlayer1)
            else (p1, p2, desenhaPlayer1, desenhaPlayer2)
    in Pictures
       [ desenhaMapa mp
       , drawBack back
       , desenhaHitbox back
       , drawFront front
       , desenhaHitbox front
       , desenhaVidaP1 p1
       , desenhaVidaP2 p2
       ]


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
desenhaHitbox f@(Fighter {fighterPos = (x,y), fighterDir = dir, fighterStance = stance, fighterTamanho = altura, keyLeft = kl, keyRight = kr}) =
  case normalAttackHitbox f of
    Nothing -> Blank
    Just (cx, cy, w, h) ->
        case stance of
            Crouching -> Translate (x + largura/2 + cx) (h/2 - 450 - cy) $ Color orange $ rectangleSolid w h
            _ -> Translate (x + largura/2 + cx) (y + altura/3*2 - 450 - cy) $ Color orange $ rectangleSolid w h
            where
                largura = altura / 2

desenhaVidaP1 :: Fighter -> Picture
desenhaVidaP1 f@(Fighter {fighterVida = vida}) = Translate (-600) 300 $ Pictures [barra, barraVida]
    where
        barra = Color (greyN 0.5) $ rectangleSolid 600 80
        barraVida = Color green $ rectangleSolid (6 * vida) 80

desenhaVidaP2 :: Fighter -> Picture
desenhaVidaP2 f@(Fighter {fighterVida = vida}) = Translate 600 300 $ Pictures [barra, barraVida]
    where
        barra = Color (greyN 0.5) $ rectangleSolid 600 80
        barraVida = Color green $ rectangleSolid (6 * vida) 80
