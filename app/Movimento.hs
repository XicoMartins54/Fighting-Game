module Movimento where

import Types
import Types (Fighter(normalAttack, fighterTamanho, fighterPeso), AttackInstance)
import Data.Maybe
import Prelude hiding (minimum, maximum)
import Data.List (minimum, maximum)
import Data.Fixed (mod')


gravityForPeso :: Peso -> Float
gravityForPeso MuitoLeve   = -4500
gravityForPeso Leve        = -6000
gravityForPeso Medio       = -7500
gravityForPeso Pesado      = -9000
gravityForPeso MuitoPesado = -10500

jumpInitialVel :: Float
jumpInitialVel = 2250

velocidadeForPeso :: Peso -> Maybe AttackInstance -> Float
velocidadeForPeso MuitoLeve   Nothing = 13
velocidadeForPeso Leve        Nothing = 11
velocidadeForPeso Medio       Nothing = 9
velocidadeForPeso Pesado      Nothing = 7
velocidadeForPeso MuitoPesado Nothing = 5
velocidadeForPeso _ _ = 0



atualiza :: Float -> World -> World
atualiza dt w@(World { player1 = p1, player2 = p2, mapa = mp }) =
  let p1' = atualizaP dt p1 p2 mp
      p2' = atualizaP dt p2 p1 mp

      -- decresce timers de invencibilidade antes de checar colisões
      p1d = decayInv dt p1'
      p2d = decayInv dt p2'

      -- depois resolve colisões e aplica danos
      (p1final, p2final) = resolveCollisions p1d p2d
  in w { player1 = p1final, player2 = p2final }

-- actualiza um fighter dado o delta-time e o mapa
-- actualiza um fighter dado o delta-time e o mapa
atualizaP :: Float -> Fighter -> Fighter -> Mapa -> Fighter
atualizaP dt f1@(Fighter { fighterPos = (x,y)
                        , fighterTamanho = tam
                        , fighterPeso = peso
                        , fighterVelY = vy
                        , fighterStance = stance
                        , fighterDir = dir
                        , keyLeft = kl
                        , keyRight = kr
                        , keyDown = kd})
            f2@(Fighter { fighterPos = (z,w)})
           (Mapa {paredeEsq = pe, chao = ch, paredeDir = pd}) =

  let
    -- avança a instância do ataque
    normalAttack' = stepNormalAttack dt f1 (normalAttack f1)

    -- atualizamos f1 já com a instância de ataque avançada
    f1' = f1 { normalAttack = normalAttack' }

    -- detecta se o ataque actual é um "down attack"
    isDownAttack = case normalAttack' of
                    Just (AttackInstance _ _ _ _ aiDir)
                      | stance /= Crouching -> aiDir `elem` [Baixo, BaixoDir, BaixoEsq]
                      | otherwise -> False
                    Nothing -> False


    -- velocidade horizontal depende se tem ataque activo (ex.: slows while attacking)
    vx = velocidadeForPeso peso normalAttack'

    -- movimento horizontal (vx é velocidade em px/s -> multiplicamos por dt)
    dx
      | kl && not kr = - vx
      | kr && not kl =   vx
      | otherwise    = 0

    x'
      | x + dx <= pe = pe
      | x + dx >= pd - tam / 2 = pd - tam / 2
      | otherwise = x + dx

    -- gravidade base e possível amplificação durante down-attack
    gBase = gravityForPeso peso
    g = if isDownAttack then gBase * 2 else gBase

    -- movimento vertical (mantive a tua lógica, usando g)
    (y', vy', stance') = case stance of
      Jumping ->
        let vy0 = if vy == 0 then jumpInitialVel else vy
            vy1 = vy0 + g * dt
            y1  = y + vy1 * dt
        in if vy1 <= 0
           then (y1, vy1, Falling)
           else (y1, vy1, Jumping)

      Falling ->
        let vy1 = vy + g * dt
            y1  = y + vy1 * dt
        in if y1 <= ch
           then (ch, 0, Standing)
           else (y1, vy1, Falling)

      _ ->
        if y > ch
        then let vy1 = vy + g * dt
                 y1  = y + vy1 * dt
             in if y1 <= ch
                then (ch, 0, Standing)
                else (y1, vy1, Falling)
        else (ch, 0, stance)

    -- se é um down-attack e tocou o chão, força y/ch/stance
    (yFinal, vyFinal, touchedGroundByDownAttack) =
      if isDownAttack && y' <= ch
      then (ch, 0, True)
      else (y', vy', False)

    -- combinar regra de crouch com o caso especial do down-attack atingindo o chão
    stanceCombined
      | touchedGroundByDownAttack = Standing               -- down-attack que bateu no chão -> fica Standing
      | yFinal <= ch && kd        = Crouching              -- tecla para baixo -> crouch (quando no chão)
      | yFinal <= ch               = Standing
      | otherwise                 = stance'

    -- troca de direção (usando x' depois do movimento)
    -- só troca de facing quando não há ataque activo (impede inverter frente do hitbox)
    dir'
      | dir == Direita && x' > z && normalAttack' == Nothing = Esquerda
      | dir == Esquerda  && x' < z && normalAttack' == Nothing = Direita
      | otherwise = dir

    -- actualizar o AttackInstance caso um down-attack tenha tocado o chão:
    -- passa para Recovery (com timers da definição down) ou termina se já estiver em Recovery
    normalAttackFinal = case normalAttack' of
      Just ai@(AttackInstance phase _ hasHit dmg dirAtaque)
        | touchedGroundByDownAttack ->
            let defDown = defaultNormalAttackDown f1
            in case phase of
                 Recovery -> Nothing
                 _        -> Just (AttackInstance Recovery (naRecovery defDown) hasHit dmg dirAtaque)
      _ -> normalAttack'

  in f1' { fighterPos    = (x', yFinal)
        , fighterVelY   = vyFinal
        , fighterStance = stanceCombined
        , fighterDir    = dir'
        , normalAttack  = normalAttackFinal }


-- avança a instância do ataque normal dt segundos
stepNormalAttack :: Float -> Fighter -> Maybe AttackInstance -> Maybe AttackInstance
stepNormalAttack _ _ Nothing = Nothing
stepNormalAttack dt f (Just ai@(AttackInstance phase t hasHit dmg dirAtaque))
  | t > dt = Just (ai { aiTimer = t - dt })
  | otherwise =
      let def = if fighterStance f == Crouching
                  then defaultNormalAttack f
                  else case dirAtaque of
                         Baixo    -> defaultNormalAttackDown f
                         _        -> defaultNormalAttack f
      in case phase of
           Start    -> Just (AttackInstance Windup     (naWindup def)     False (naDamage def) dirAtaque)
           Windup   -> Just (AttackInstance Peak     (naPeak def)     False (naDamage def) dirAtaque)
           Peak     -> Just (AttackInstance Retract (naRetract def) hasHit (aiDamage ai) dirAtaque)
           Retract  -> Just (AttackInstance Recovery (naRecovery def) hasHit (aiDamage ai) dirAtaque)
           Recovery -> Nothing





rectsIntersect :: (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> Bool
rectsIntersect (cx1, cy1, w1, h1) (cx2, cy2, w2, h2) =
  (abs (cx1 - cx2) * 2 <= (w1 + w2)) && (abs (cy1 - cy2) * 2 <= (h1 + h2))


attackHitboxWorld :: Fighter -> Maybe (Float,Float,Float,Float)
attackHitboxWorld f@(Fighter { fighterPos = (x,y)
                            , fighterTamanho = altura
                            , fighterStance = stance }) =
  case normalAttackHitbox f of
    Nothing -> Nothing
    Just (cx, cy, w, h, angle) ->
      let largura = altura / 2
          centerX = x + largura/2 + cx
          centerY = case stance of
                      Crouching -> (h/2) - 450 - cy
                      _         -> y + (altura/3*2) - 450 - cy
      in Just (centerX, centerY, w, h)

-- hurtbox do corpo do fighter (mesmas coordenadas que usas no desenho)
fighterHurtbox :: Fighter -> (Float,Float,Float,Float)
fighterHurtbox f@(Fighter { fighterPos = (x,y)
                          , fighterTamanho = altura
                          , fighterStance = stance }) =
  let largura = altura / 2
  in case stance of
       Crouching -> (x + largura/2, (altura/2 - altura/3 - 450), largura, (altura / 3))
       _         -> (x + largura/2, (y + altura/2 - 450), largura, altura)

-- decrementa invincibility timer
decayInv :: Float -> Fighter -> Fighter
decayInv dt f@(Fighter { isInvincible = inv, invincibleTimer = t })
  | not inv = f
  | otherwise =
      let t' = t - dt
      in if t' <= 0 then f { isInvincible = False, invincibleTimer = 0 } else f { invincibleTimer = t' }

-- aplica ataque de attacker a defender, se em Peak e se intersecta e defender não invencível
applyAttackOnce :: Fighter -> Fighter -> (Fighter, Fighter)
applyAttackOnce attacker defender =
  case normalAttack attacker of
    Just ai@(AttackInstance phase _ hasHit dmg dirAtaque)
      | phase == Peak && not hasHit ->
          case normalAttackHitbox attacker of
              Just (cx, cy, w, h, angle) ->
                  let (hx, hy, hw, hh) = fighterHurtbox defender
                  in if not (isInvincible defender) && obbAabbIntersect (cx, cy, w, h, angle) (hx, hy, hw, hh, 0)
                     then
                         let ai' = ai { aiHasHit = True }
                             attacker' = attacker { normalAttack = Just ai' }
                             defender' = defender { fighterVida = fighterVida defender - dmg
                                                  , isInvincible = True
                                                  , invincibleTimer = 0.4
                                                  }
                         in (attacker', defender')
                     else (attacker, defender)
              Nothing -> (attacker, defender)
    _ -> (attacker, defender)




-- resolve colisões dos dois fighters (aplica ataques de ambos, sem duplicar)
resolveCollisions :: Fighter -> Fighter -> (Fighter, Fighter)
resolveCollisions p1 p2 =
  let (p1a, p2a) = applyAttackOnce p1 p2   -- p1 tenta acertar p2
      (p2b, p1b) = applyAttackOnce p2a p1a -- p2 (actualizado) tenta acertar p1 (actualizado)
  in (p1b, p2b)



deg2rad :: Float -> Float
deg2rad d = d * pi / 180

-- calcula cantos da OBB (centro cx,cy; w,h; angle degrees)
obbCorners :: (Float,Float,Float,Float,Float) -> [(Float,Float)]
obbCorners (cx, cy, w, h, angleDeg) =
  let a = deg2rad angleDeg
      ux = cos a; uy = sin a      -- axis u
      vx = -sin a; vy = cos a     -- axis v (perpendicular)
      hx = w / 2; hy = h / 2
      -- combinacoes
      add (sx, sy) = (cx + sx, cy + sy)
      corners =
        [ add ( hx * ux + hy * vx,  hx * uy + hy * vy)
        , add (-hx * ux + hy * vx, -hx * uy + hy * vy)
        , add (-hx * ux - hy * vx, -hx * uy - hy * vy)
        , add ( hx * ux - hy * vx,  hx * uy - hy * vy)
        ]
  in corners

aabbCorners :: (Float,Float,Float,Float) -> [(Float,Float)]
aabbCorners (cx, cy, w, h) =
  let hx = w/2; hy = h/2
  in [ (cx+hx, cy+hy), (cx-hx, cy+hy), (cx-hx, cy-hy), (cx+hx, cy-hy) ]

-- project points on axis (nx,ny) and return (min,max)
projectOnAxis :: (Float,Float) -> [(Float,Float)] -> (Float,Float)
projectOnAxis (nx,ny) pts =
  let dots = map (\(x,y) -> x*nx + y*ny) pts
  in (minimum dots, maximum dots)

-- check overlap intervals
intervalsOverlap :: (Float,Float) -> (Float,Float) -> Bool
intervalsOverlap (aMin,aMax) (bMin,bMax) = not (aMax < bMin || bMax < aMin)

-- axes to test: two axes from OBB (u and v) and world axes (1,0) and (0,1) for safety
obbAxes :: Float -> [(Float,Float)]
obbAxes angleDeg =
  let a = deg2rad angleDeg
  in [(cos a, sin a), (-sin a, cos a)]

obbAabbIntersect :: (Float, Float, Float, Float, Float)  -- ataque
                 -> (Float, Float, Float, Float, Float)  -- defensor
                 -> Bool
obbAabbIntersect (x1, y1, w1, h1, angle1) (x2, y2, w2, h2, angle2) =
    -- para já, ignora o angle, assume axis-aligned
    x1 < x2 + w2 && x1 + w1 > x2 &&
    y1 < y2 + h2 && y1 + h1 > y2

