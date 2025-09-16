module Types where
import GHC.Generics (Associativity(LeftAssociative))
import Foreign.C (CIntMax)



data World = World {player1 :: Fighter, player2 :: Fighter, mapa :: Mapa}

type Tempo = Float

data Mapa = Mapa {paredeEsq :: Float, chao :: Float, paredeDir :: Float}

type Posicao = (Float, Float)

data Fighter = Fighter
    {
        fighterPos :: Posicao,
        fighterTamanho :: Float,
        fighterPeso :: Peso,

        fighterVida :: Float,

        fighterStance :: Stance,

        fighterDir :: Direcao,

        fighterVelY   :: Float,

        keyLeft       :: Bool,
        keyRight      :: Bool,
        keyDown       :: Bool,
        keyUp         :: Bool,

        normalAttack  :: Maybe AttackInstance,

        isInvincible  :: Bool,
        invincibleTimer:: Float
    }
    deriving (Eq, Show)


data AttackPhase = Start | Windup | Peak | Retract | Recovery 
  deriving (Eq, Show)

data AttackInstance = AttackInstance
  { aiPhase  :: AttackPhase
  , aiTimer  :: Float
  , aiHasHit :: Bool
  , aiDamage :: Float
  , aiDir    :: DirecaoAtaque
  } deriving (Eq, Show)


-- definição de parâmetros do ataque normal (constante global)
data NormalAttackDef = NormalAttackDef
  { naStart    :: Tempo
  , naWindup   :: Tempo
  , naPeak     :: Tempo
  , naRetract  :: Tempo
  , naRecovery :: Tempo
  , naWidth    :: Float
  , naHeight   :: Float
  , naDamage   :: Float
  } deriving (Eq, Show)

defaultNormalAttack :: Fighter -> NormalAttackDef
defaultNormalAttack f@(Fighter {fighterTamanho = tam}) = NormalAttackDef
  { naStart    = 0.12
  , naWindup   = 0.08
  , naPeak     = 0.14
  , naRetract  = 0.08
  , naRecovery = 0.12
  , naWidth    = tam / 5 * 3
  , naHeight   = tam / 4
  , naDamage   = 10    -- escolhe o valor que quiseres
  }

defaultNormalAttackDown :: Fighter -> NormalAttackDef
defaultNormalAttackDown f@(Fighter {fighterTamanho = tam}) = NormalAttackDef
  { naStart    = 0.05
  , naWindup   = 0.05
  , naPeak     = 5
  , naRetract  = 0
  , naRecovery = 0
  , naWidth    = tam / 2
  , naHeight   = tam / 3
  , naDamage   = 100
  }


data Peso 
  = MuitoLeve
  | Leve
  | Medio
  | Pesado
  | MuitoPesado
  deriving (Eq, Show)

data Direcao
    = Esquerda
    | Direita
    deriving (Eq, Show)

data DirecaoAtaque
    = Esq
    | Dir
    | Cima
    | Baixo
    | CimaDir
    | CimaEsq
    | BaixoDir
    | BaixoEsq
    deriving (Eq, Show)

data Stance
    = Standing
    | Jumping
    | Falling
    | Crouching
    deriving (Eq, Show)

type IsHit = Bool

type IsInvincible = Bool 


-- retorna: Just (centerX, centerY, width, height, angleDegrees)
normalAttackHitbox :: Fighter -> Maybe (Float, Float, Float, Float, Float)
normalAttackHitbox (Fighter { normalAttack = Nothing }) = Nothing
normalAttackHitbox f@(Fighter { normalAttack = Just (AttackInstance phase _ _ _ aiDir)
                             , fighterPos = (x,y)
                             , fighterTamanho = tam
                             , fighterStance = stance
                             , fighterDir = dir }) =

  let
      -- escolhe def conforme stance/aiDir (igual ao teu código)
      def = if stance == Crouching
              then defaultNormalAttack f
              else if aiDir `elem` [Baixo]
                     then defaultNormalAttackDown f
                     else defaultNormalAttack f

      w = naWidth def
      h = naHeight def

      -- animação do tamanho conforme fase
      mult = case phase of
               Start    -> 0.4
               Windup   -> 0.7
               Peak     -> 1
               Retract  -> 0.7
               Recovery -> 0.4

      w' = w * mult
      h' = h * mult


      -- face sign
      faceSign = case dir of Esquerda -> -1; Direita -> 1

      -- horizontal sign base (se aiDir tiver componente horiz, usa-a; se for puramente vertical usa facing)
      horizSign = case aiDir of
                    Esq      -> -1
                    CimaEsq  -> -1
                    BaixoEsq -> -1
                    Dir      -> 1
                    CimaDir  -> 1
                    BaixoDir -> 1
                    Cima     -> faceSign
                    Baixo    -> faceSign

      -- offset base (ajusta visualmente se precisares)
      offX = case aiDir of
               Cima -> 0
               CimaDir -> horizSign * (w' / 2)
               CimaEsq -> horizSign * (w' / 2)
               Baixo -> 0
               BaixoDir -> horizSign * (w' / 2)
               BaixoEsq -> horizSign * (w' / 2)
               _ -> horizSign * (w' / 2)
      offY = case aiDir of
               Cima -> w' / 2
               CimaDir -> w' / 2
               CimaEsq -> w' / 2
               Baixo -> -(tam / 3 * 2 + (h' / 2))
               BaixoDir -> -(tam / 3 * 2)
               BaixoEsq -> -(tam / 3 * 2)
               _ -> 0



      baseY = case stance of
                Crouching -> (tam/2 - tam/3 - 450)
                _         -> y + (tam/3*2) - 450

      centerX = x + (tam/2)/2 + offX
      centerY = baseY + offY

      -- angle mapping (graus)
      angleDeg = case aiDir of
                   CimaDir  -> -45
                   BaixoDir -> 45
                   CimaEsq  -> -135
                   BaixoEsq -> 135
                   Cima     -> 90
                   Baixo    -> -90
                   _        -> 0

  in
    case aiDir of
      Cima -> Just (centerX, centerY, w', h, angleDeg)
      CimaDir -> Just (centerX, centerY, w', h, angleDeg)
      CimaEsq -> Just (centerX, centerY, w', h, angleDeg)
      Baixo -> Just (centerX, centerY, h', w, angleDeg)
      BaixoDir -> Just (centerX, centerY, w', h, angleDeg)
      BaixoEsq -> Just (centerX, centerY, w', h, angleDeg)
      _ -> Just (centerX, centerY, w', h, angleDeg)




chooseAttackDir :: Fighter -> DirecaoAtaque
chooseAttackDir f =
  let kl = keyLeft f
      kr = keyRight f
      kd = keyDown f
      ku = keyUp f
      st = fighterStance f
      dirBase = case fighterDir f of Esquerda -> Esq; Direita -> Dir
      vy = fighterVelY f

      -- sinais base X/Y
      signX
        | kl && not kr    = -1
        | kr && not kl    = 1
        | otherwise       = 0

      signY
        | ku = 1
        | kd && st /= Crouching = -1
        | st /= Standing && vy > 0 = 1
        | st /= Standing && vy <= 0 = -1
        | otherwise = 0

      dirGuess = case (signX, signY) of
        (-1, -1) -> BaixoEsq
        ( 1, -1) -> BaixoDir
        ( 0, -1) -> Baixo
        (-1,  1) -> CimaEsq
        ( 1,  1) -> CimaDir
        ( 0,  1) -> Cima
        (-1,  0) -> Esq
        ( 1,  0) -> Dir
        ( 0,  0) -> dirBase

  in case dirGuess of
       -- proíbe down attacks se já estivermos crouching
       Baixo    | st == Crouching -> dirBase
       BaixoDir | st == Crouching -> Dir
       BaixoEsq | st == Crouching -> Esq
       _ -> dirGuess
