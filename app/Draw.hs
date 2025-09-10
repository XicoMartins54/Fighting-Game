module Draw where

import Types
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Types (World(player1, player2))

window :: Display
window = InWindow "" (1920,1080) (0,0)

background :: Color
background = greyN 0

desenha :: World -> Picture
desenha w@(World {player1 = p1, player2 = p2, mapa = map}) = Pictures [desenhaMapa map, desenhaPlayer1 p1, desenhaPlayer2 p2]

desenhaPlayer1 :: Fighter -> Picture
desenhaPlayer1 p1@(Fighter {fighterPos = (x,y), fighterDir = dir, fighterStance = stance}) = 
    case stance of
        Crouching -> corpoCrouching
        _ -> corpoStanding
    where
        altura = 250
        largura = altura / 2
        corpoStanding = Color red $ Translate (largura * x + largura/2) (altura * y + altura/2 - 450) $ rectangleSolid largura altura
        corpoCrouching = Color red $ Translate (largura * x + largura/2) (altura * y + altura/2 - 535) $ rectangleSolid largura (altura / 3)



desenhaPlayer2 :: Fighter -> Picture
desenhaPlayer2 p1@(Fighter {fighterPos = (x,y), fighterDir = dir, fighterStance = stance}) = 
    case stance of
        Crouching -> corpoCrouching
        _ -> corpoStanding
    where
        altura = 250
        largura = altura / 2
        corpoStanding = Color blue $ Translate (largura * x + largura/2) (altura * y + altura/2 - 450) $ rectangleSolid largura altura
        corpoCrouching = Color blue $ Translate (largura * x + largura/2) (altura * y + altura/2 - 535) $ rectangleSolid largura (altura / 3)



desenhaMapa :: Mapa -> Picture
desenhaMapa mapa@(Mapa esq chao dir) = Pictures [paredeEsq, floor, paredeDir]
    where
        largura = 60
        paredeEsq = Color white $ Translate (esq - largura/2 - 870) 0 $ rectangleSolid largura 1060
        floor = Color white $ Translate 0 (chao - largura/2 - 450) $ rectangleSolid 1920 largura
        paredeDir = Color white $ Translate (dir + largura/2 + 870) 0 $ rectangleSolid largura 1060