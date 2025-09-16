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
  { naStart    = 0.1
  , naWindup   = 0.1
  , naPeak     = 0.14
  , naRetract  = 0.1
  , naRecovery = 0.1
  , naWidth    = tam / 5 * 3
  , naHeight   = tam / 4
  , naDamage   = 10    -- escolhe o valor que quiseres
  }

defaultNormalAttackDown :: Fighter -> NormalAttackDef
defaultNormalAttackDown f@(Fighter {fighterTamanho = tam}) = NormalAttackDef
  { naStart    = 0.1
  , naWindup   = 0.1
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


normalAttackHitbox :: Fighter -> Maybe (Float, Float, Float, Float)
normalAttackHitbox (Fighter { normalAttack = Nothing }) = Nothing
normalAttackHitbox f@(Fighter { normalAttack = Just (AttackInstance phase _ _ _ aiDir)
                             , fighterTamanho = tam
                             , fighterStance = stance
                             , fighterDir = dir
                             , keyLeft = kl
                             , keyRight = kr
                             }) =

  let
      -- escolhe a definição de ataque de acordo com stance e aiDir
      def = if stance == Crouching
              then defaultNormalAttack f           -- crouching = normal attack
              else if aiDir `elem` [Baixo, BaixoDir, BaixoEsq]
                     then defaultNormalAttackDown f  -- down attack
                     else defaultNormalAttack f       -- normal / up attack

      w = naWidth def
      h = naHeight def
      mult = case phase of
               Start    -> 0.3
               Windup   -> 0.7
               Peak     -> 1
               Retract  -> 0.7
               Recovery -> 0.3

      -- faceSign baseado na direcao do fighter (esquerda = -1, direita = 1)
      faceSign = case dir of Esquerda -> -1; Direita -> 1

      -- sinal horizontal: se aiDir tem componente horiz usa-a, caso contrario usa facing
      signX = case aiDir of
                Esq      -> -1
                CimaEsq  -> -1
                BaixoEsq -> -1
                Dir      -> 1
                CimaDir  -> 1
                BaixoDir -> 1
                Cima     -> faceSign
                Baixo    -> faceSign

      w' = w * mult
      offX = signX * w' / 2
      offY = tam / 3 * 2
      offXUp = h/2 * signX
      offYUp = w' / 2

      -- decide se é up/down attack (apenas se não estivermos crouching para down)
      isDownAttack = stance /= Crouching && aiDir `elem` [Baixo, BaixoDir, BaixoEsq]
      isUpAttack   = aiDir `elem` [Cima, CimaDir, CimaEsq]

  in case (isUpAttack, isDownAttack, stance) of
     (_, True, Jumping)      -> Just (0, offY + (naHeight (defaultNormalAttackDown f)) / 2, naWidth (defaultNormalAttackDown f), naHeight (defaultNormalAttackDown f))
     (_, True, Falling)      -> Just (0, offY + (naHeight (defaultNormalAttackDown f)) / 2, naWidth (defaultNormalAttackDown f), naHeight (defaultNormalAttackDown f))
     (True, _, _)            -> Just (offXUp, -offYUp, h, w')
     (False, False, Jumping) -> Just (offX, 0, w', h)
     (False, False, Falling) -> Just (offX, 0, w', h)
     (_, True, _)            -> Just (offX, 0, naWidth (defaultNormalAttackDown f), naHeight (defaultNormalAttackDown f))
     (_, False, _)           -> Just (offX, 0, w', h)




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
