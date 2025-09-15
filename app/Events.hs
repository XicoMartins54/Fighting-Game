module Events where

import Types
import Graphics.Gloss.Interface.Pure.Game
import Data.Char (toLower)

reageEventos :: Event -> World -> World
reageEventos e w@(World { player1 = p1, player2 = p2, mapa = m }) =
  w { player1 = reageEventosPlayer1 e p1, player2 = reageEventosPlayer2 e p2 }



reageEventosPlayer1 :: Event -> Fighter -> Fighter
reageEventosPlayer1 (EventKey (Char c) Down _ _) f
  | toLower c == 'w' && fighterStance f == Standing =
      f { fighterStance = Jumping, fighterVelY = 0 }
  | toLower c == 'a' = f { keyLeft = True }
  | toLower c == 'd' = f { keyRight = True }
  | toLower c == 's' = f { keyDown = True }
  | toLower c == 'v' =
      case normalAttack f of
      Nothing ->
        let dirAtaque = chooseAttackDir f
            def       = case dirAtaque of
                          Baixo    -> defaultNormalAttackDown f
                          BaixoDir -> defaultNormalAttackDown f
                          BaixoEsq -> defaultNormalAttackDown f
                          _        -> defaultNormalAttack f
        in f { normalAttack = Just (AttackInstance Windup (naWindup def) False (naDamage def) dirAtaque) }
      Just _  -> f
reageEventosPlayer1 (EventKey (Char c) Up _ _) f
  | toLower c == 'a' = f { keyLeft = False }
  | toLower c == 'd' = f { keyRight = False }
  | toLower c == 's' = f { keyDown = False }
reageEventosPlayer1 _ f = f



reageEventosPlayer2 :: Event -> Fighter -> Fighter
reageEventosPlayer2 (EventKey (SpecialKey KeyUp) Down _ _) f@(Fighter {fighterStance = stance, fighterDir = dir , fighterVelY = vy})
    | stance == Standing = 
        f { fighterStance = Jumping, fighterVelY = 0 }
    | otherwise = f
reageEventosPlayer2 (EventKey (SpecialKey KeyLeft) Down _ _) f = f { keyLeft = True }
reageEventosPlayer2 (EventKey (SpecialKey KeyLeft) Up _ _) f = f { keyLeft = False }
reageEventosPlayer2 (EventKey (SpecialKey KeyRight) Down _ _) f = f { keyRight = True }
reageEventosPlayer2 (EventKey (SpecialKey KeyRight) Up _ _) f = f { keyRight = False }
reageEventosPlayer2 (EventKey (SpecialKey KeyDown) Down _ _) f = f { keyDown = True }
reageEventosPlayer2 (EventKey (SpecialKey KeyDown) Up _ _) f = f { keyDown = False }
reageEventosPlayer2 (EventKey (Char '3') Down _ _) f =
  case normalAttack f of
      Nothing ->
        let dirAtaque = chooseAttackDir f
            def       = case dirAtaque of
                          Baixo    -> defaultNormalAttackDown f
                          BaixoDir -> defaultNormalAttackDown f
                          BaixoEsq -> defaultNormalAttackDown f
                          _        -> defaultNormalAttack f
        in f { normalAttack = Just (AttackInstance Windup (naWindup def) False (naDamage def) dirAtaque) }
      Just _  -> f
reageEventosPlayer2 _ f = f