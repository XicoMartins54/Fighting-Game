module Types where
import GHC.Generics (Associativity(LeftAssociative))



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

        normalAttack  :: Maybe AttackInstance
    }
    deriving (Eq, Show)


data AttackPhase = Windup | Peak | Recovery
  deriving (Eq, Show)

data AttackInstance = AttackInstance
  { aiPhase :: AttackPhase   -- fase actual
  , aiTimer :: Float         -- tempo restante na fase (segundos)
  } deriving (Eq, Show)

-- definição de parâmetros do ataque normal (constante global)
data NormalAttackDef = NormalAttackDef
  { naWindup  :: Tempo   -- duracao da windup (s)
  , naPeak    :: Tempo   -- duracao da peak (s)
  , naRecovery:: Tempo   -- duracao da recovery (s)
  , naWidth   :: Float   -- largura do hitbox
  , naHeight  :: Float   -- altura do hitbox
  } deriving (Eq, Show)

defaultNormalAttack :: Fighter -> NormalAttackDef
defaultNormalAttack f@(Fighter {fighterTamanho = tam}) = NormalAttackDef
  { naWindup   = 0.1
  , naPeak     = 0.08
  , naRecovery = 0.1
  , naWidth    = tam / 5 * 3
  , naHeight   = tam / 5
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

data Stance
    = Standing
    | Jumping
    | Falling
    | Crouching
    deriving (Eq, Show)

type IsHit = Bool

type IsInvincible = Bool 


normalAttackHitbox :: Fighter -> Maybe (Float, Float, Float)
normalAttackHitbox (Fighter { normalAttack = Nothing }) = Nothing
normalAttackHitbox f@(Fighter { normalAttack = Just (AttackInstance phase _)
                             , fighterDir = dir
                             , keyLeft = kl
                             , keyRight = kr}) =
  let def   = defaultNormalAttack f
      w     = naWidth def
      h     = naHeight def
      mult = case phase of
        Windup   -> 0.6
        Peak     -> 1
        Recovery -> 0.6
      sign
        | kl && not kr = -1
        | not kl && kr = 1
        | dir == Esquerda = -1
        | otherwise = 1
      w'   = w * mult
      h'   = h
      offX = sign * w'/2
  in Just (offX, w', h')