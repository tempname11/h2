module Heroes.H3 where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Bimap                                as Bimap
import Data.Bimap                                        (Bimap)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Town
  = Castle
  | Rampart
  | Tower
  | Citadel
  | Fortress
  | Dungeon
  | Necropolis
  | Inferno
  | Conflux

data Grade = Ungraded | Basic | Upgraded
data Level = Unleveled | L1 | L2 | L3 | L4 | L5 | L6 | L7

data Creature
  -- Castle
  = Pikeman
  | Halberdier
  | Archer
  | Marksman
  | Griffin
  | RoyalGriffin
  | Swordsman
  | Crusader
  | Monk
  | Zealot
  | Cavalier
  | Champion
  | Angel
  | Archangel
  -- Rampart
  | Centaur
  | CentaurCaptain
  | Dwarf
  | BattleDwarf
  | WoodElf
  | GrandElf
  | Pegasus
  | SilverPegasus
  | DendroidGuard
  | DendroidSoldier
  | Unicorn
  | WarUnicorn
  | GreenDragon
  | GoldDragon
  -- Tower
  | Gremlin
  | MasterGremlin
  | StoneGargoyle
  | ObsidianGargoyle
  | StoneGolem
  | IronGolem
  | Mage
  | Archmage
  | Genie
  | MasterGenie
  | Naga
  | NagaQueen
  | Giant
  | Titan
  -- Citadel
  | Goblin
  | Hobgoblin
  | WolfRider
  | WolfRaider
  | Orc
  | OrcChieftain
  | Ogre
  | OgreMage
  | Roc
  | Thunderbird
  | Cyclops
  | CyclopsKing
  | Behemoth
  | AncientBehemoth
  -- Fortress
  | Gnoll
  | GnollMarauder
  | Lizardman
  | LizardWarrior
  | SerpentFly
  | DragonFly
  | Basilisk
  | GreaterBasilisk
  | Gorgon
  | MightyGorgon
  | Wyvern
  | WyvernMonarch
  | Hydra
  | ChaosHydra
  -- Dungeon
  | Troglodyte
  | InfernalTroglodyte
  | Harpy
  | HarpyHag
  | Beholder
  | EvilEye
  | Medusa
  | MedusaQueen
  | Minotaur
  | MinotaurKing
  | Manticore
  | Scorpicore
  | RedDragon
  | BlackDragon
  -- Necropolis
  | Skeleton
  | SkeletonWarrior
  | WalkingDead
  | Zombie
  | Wight
  | Wraith
  | Vampire
  | VampireLord
  | Lich
  | PowerLich
  | BlackKnight
  | DreadKnight
  | BoneDragon
  | GhostDragon
  -- Inferno
  | Imp
  | Familiar
  | Gog
  | Magog
  | HellHound
  | Cerberus
  | Demon
  | HornedDemon
  | PitFiend
  | PitLord
  | Efreet
  | EfreetSultan
  | Devil
  | ArchDevil
  -- Conflux
  | Pixie
  | Sprite
  | AirElemental
  | StormElemental
  | WaterElemental
  | IceElemental
  | FireElemental
  | EnergyElemental
  | EarthElemental
  | MagmaElemental
  | PsychicElemental
  | MagicElemental
  | Firebird
  | Phoenix
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

--------------------------------------------------------------------------------

cSndName :: Creature -> String
cSndName = go
  where
  -- Castle
  go Pikeman            = "PIKE"
  go Halberdier         = "HALB"
  go Archer             = "LCRS"
  go Marksman           = "HCRS"
  go Griffin            = "GRIF"
  go RoyalGriffin       = "RGRF"
  go Swordsman          = "SWRD"
  go Crusader           = "CRUS"
  go Monk               = "MONK"
  go Zealot             = "ZELT"
  go Cavalier           = "CAVA"
  go Champion           = "CHMP"
  go Angel              = "ANGL"
  go Archangel          = "AAGL"
  -- Rampart
  go Centaur            = "CNTR"
  go CentaurCaptain     = "ECNT"
  go Dwarf              = "DWRF"
  go BattleDwarf        = "BDRF"
  go WoodElf            = "WELF"
  go GrandElf           = "GELF"
  go Pegasus            = "PEGA"
  go SilverPegasus      = "APEG"
  go DendroidGuard      = "BTRE"
  go DendroidSoldier    = "TREE"
  go Unicorn            = "UNIC"
  go WarUnicorn         = "WUNC"
  go GreenDragon        = "GRDR"
  go GoldDragon         = "GODR"
  -- Tower
  go Gremlin            = "AGRM"
  go MasterGremlin      = "MGRM"
  go StoneGargoyle      = "SGRG"
  go ObsidianGargoyle   = "OGRG"
  go StoneGolem         = "SGLM"
  go IronGolem          = "IGLM"
  go Mage               = "MAGE"
  go Archmage           = "AMAG"
  go Genie              = "GENI"
  go MasterGenie        = "CALF"
  go Naga               = "NSEN"
  go NagaQueen          = "NGRD"
  go Giant              = "LTIT"
  go Titan              = "GTIT"
  -- Citadel
  go Goblin             = "GBLN"
  go Hobgoblin          = "HGOB"
  go WolfRider          = "GWRD"
  go WolfRaider         = "HGWR"
  go Orc                = "OORC"
  go OrcChieftain       = "ORCC"
  go Ogre               = "OGRE"
  go OgreMage           = "OGRM"
  go Roc                = "ROCC"
  go Thunderbird        = "TBRD"
  go Cyclops            = "CCYC"
  go CyclopsKing        = "CYCL"
  go Behemoth           = "YBMH"
  go AncientBehemoth    = "BMTH"
  -- Fortress
  go Gnoll              = "GNOL"
  go GnollMarauder      = "GNLM"
  go Lizardman          = "PLIZ"
  go LizardWarrior      = "ALIZ"
  go SerpentFly         = "DFLY"
  go DragonFly          = "FDFL"
  go Basilisk           = "BASL"
  go GreaterBasilisk    = "GBAS"
  go Gorgon             = "CGOR"
  go MightyGorgon       = "BGOR"
  go Wyvern             = "WYVN"
  go WyvernMonarch      = "WYVM"
  go Hydra              = "HYDR"
  go ChaosHydra         = "CHYD"
  -- Dungeon
  go Troglodyte         = "TROG"
  go InfernalTroglodyte = "ITRG"
  go Harpy              = "HARP"
  go HarpyHag           = "HHAG"
  go Beholder           = "BHDR"
  go EvilEye            = "EVLI"
  go Medusa             = "MEDU"
  go MedusaQueen        = "MEDQ"
  go Minotaur           = "MINO"
  go MinotaurKing       = "MINK"
  go Manticore          = "MANT"
  go Scorpicore         = "SCRP"
  go RedDragon          = "RDDR"
  go BlackDragon        = "BKDR"
  -- Necropolis
  go Skeleton           = "SKEL"
  go SkeletonWarrior    = "SKLW"
  go WalkingDead        = "ZOMB"
  go Zombie             = "ZMBL"
  go Wight              = "WGHT"
  go Wraith             = "WRTH"
  go Vampire            = "VAMP"
  go VampireLord        = "NOSF"
  go Lich               = "LICH"
  go PowerLich          = "PLCH"
  go BlackKnight        = "BKNT"
  go DreadKnight        = "BLRD"
  go BoneDragon         = "BODR"
  go GhostDragon        = "GHDR"
  -- Inferno
  go Imp                = "IMPP"
  go Familiar           = "FMLR"
  go Gog                = "GOGG"
  go Magog              = "MGOG"
  go HellHound          = "HHND"
  go Cerberus           = "CERB"
  go Demon              = "SHDM"
  go HornedDemon        = "DHDM"
  go PitFiend           = "PFND"
  go PitLord            = "PFOE"
  go Efreet             = "EFRT"
  go EfreetSultan       = "ESUL"
  go Devil              = "DEVL"
  go ArchDevil          = "ADVL"
  -- Conflux
  go Pixie              = "PIXI"
  go Sprite             = "SPRT"
  go AirElemental       = "AELM"
  go StormElemental     = "STOR"
  go WaterElemental     = "WELM"
  go IceElemental       = "ICEL"
  go FireElemental      = "FELM"
  go EnergyElemental    = "ENER"
  go EarthElemental     = "EELM"
  go MagmaElemental     = "MAGM"
  go PsychicElemental   = "PSYC"
  go MagicElemental     = "MGEL"
  go Firebird           = "FIRB"
  go Phoenix            = "PHOE"

cDefName :: Creature -> String
cDefName = go
  where
  -- Castle
  go Pikeman            = "CPKMAN"
  go Halberdier         = "chalbd"
  go Archer             = "CLCBOW"
  go Marksman           = "CHCBOW"
  go Griffin            = "CGRIFF"
  go RoyalGriffin       = "Crgrif"
  go Swordsman          = "Csword"
  go Crusader           = "Ccrusd"
  go Monk               = "Cmonkk"
  go Zealot             = "Czealt"
  go Cavalier           = "CCAVLR"
  go Champion           = "CCHAMP"
  go Angel              = "cangel"
  go Archangel          = "CRANGL"
  -- Rampart
  go Centaur            = "CCENTR"
  go CentaurCaptain     = "CECENT"
  go Dwarf              = "CDWARF"
  go BattleDwarf        = "CBDWAR"
  go WoodElf            = "CELF"
  go GrandElf           = "CGRELF"
  go Pegasus            = "CPEGAS"
  go SilverPegasus      = "CAPEGS"
  go DendroidGuard      = "CBTREE"
  go DendroidSoldier    = "CTREE"
  go Unicorn            = "CUNICO"
  go WarUnicorn         = "CWUNIC"
  go GreenDragon        = "CGDRAG"
  go GoldDragon         = "CDDRAG"
  -- Tower
  go Gremlin            = "CGREMA"
  go MasterGremlin      = "CGREMM"
  go StoneGargoyle      = "CGARGO"
  go ObsidianGargoyle   = "COGARG"
  go StoneGolem         = "CSGOLE"
  go IronGolem          = "CIGOLE"
  go Mage               = "CMAGE"
  go Archmage           = "CAMAGE"
  go Genie              = "CGENIE"
  go MasterGenie        = "CSULTA"
  go Naga               = "CNAGA"
  go NagaQueen          = "CNAGAG"
  go Giant              = "CLTITA"
  go Titan              = "CGTITA"
  -- Citadel
  go Goblin             = "CGOBLI"
  go Hobgoblin          = "CHGOBL"
  go WolfRider          = "CBWLFR"
  go WolfRaider         = "CUWLFR"
  go Orc                = "CORC"
  go OrcChieftain       = "CORCCH"
  go Ogre               = "COGRE"
  go OgreMage           = "COGMAG"
  go Roc                = "CROC"
  go Thunderbird        = "CTBIRD"
  go Cyclops            = "CCYCLR"
  go CyclopsKing        = "CcyclLor"
  go Behemoth           = "CYBEHE"
  go AncientBehemoth    = "CABEHE"
  -- Fortress
  go Gnoll              = "CGNOLL"
  go GnollMarauder      = "CGNOLM"
  go Lizardman          = "CPLIZA"
  go LizardWarrior      = "CALIZA"
  go SerpentFly         = "CDRFLY"
  go DragonFly          = "CDRFIR"
  go Basilisk           = "CBASIL"
  go GreaterBasilisk    = "CGBASI"
  go Gorgon             = "Ccgorg"
  go MightyGorgon       = "Cbgog"
  go Wyvern             = "CWYVER"
  go WyvernMonarch      = "CWYVMN"
  go Hydra              = "CHYDRA"
  go ChaosHydra         = "cchydr"
  -- Dungeon
  go Troglodyte         = "Ctrogl"
  go InfernalTroglodyte = "Citrog"
  go Harpy              = "CHARPY"
  go HarpyHag           = "CHARPH"
  go Beholder           = "cbehol"
  go EvilEye            = "Ceveye"
  go Medusa             = "Cmedus"
  go MedusaQueen        = "Cmeduq"
  go Minotaur           = "CMINOT"
  go MinotaurKing       = "Cminok"
  go Manticore          = "CMCORE"
  go Scorpicore         = "CCMCOR"
  go RedDragon          = "CRDRGN"
  go BlackDragon        = "CBDRGN"
  -- Necropolis
  go Skeleton           = "CSKELE"
  go SkeletonWarrior    = "CWSKEL"
  go WalkingDead        = "CZOMBI"
  go Zombie             = "CZOMLO"
  go Wight              = "CWIGHT"
  go Wraith             = "CWRAIT"
  go Vampire            = "CVAMP"
  go VampireLord        = "CNOSFE"
  go Lich               = "CLICH"
  go PowerLich          = "CPLICH"
  go BlackKnight        = "CBKNIG"
  go DreadKnight        = "CBLORD"
  go BoneDragon         = "CNDRGN"
  go GhostDragon        = "CHDRGN"
  -- Inferno
  go Imp                = "CIMP"
  go Familiar           = "CFAMIL"
  go Gog                = "CGOG"
  go Magog              = "CMAGOG"
  go HellHound          = "CHHOUN"
  go Cerberus           = "CCERBU"
  go Demon              = "COHDEM"
  go HornedDemon        = "CTHDEM"
  go PitFiend           = "CPFIEN"
  go PitLord            = "CPFOE"
  go Efreet             = "cefree"
  go EfreetSultan       = "cefres"
  go Devil              = "CDEVIL"
  go ArchDevil          = "CADEVL"
  -- Conflux
  go Pixie              = "Cpixie"
  go Sprite             = "CSprite"
  go AirElemental       = "CAELEM"
  go StormElemental     = "Cstorm"
  go WaterElemental     = "CWELEM"
  go IceElemental       = "Cicee"
  go FireElemental      = "cfelem"
  go EnergyElemental    = "Cnrg"
  go EarthElemental     = "CEELEM"
  go MagmaElemental     = "Cstone"
  go PsychicElemental   = "Cpsyel"
  go MagicElemental     = "Cmagel"
  go Firebird           = "Cfbird"
  go Phoenix            = "CPHX"

shoots :: Creature -> Bool
shoots = go
  where
  -- Castle
  go Pikeman            = False
  go Halberdier         = False
  go Archer             = True
  go Marksman           = True
  go Griffin            = False
  go RoyalGriffin       = False
  go Swordsman          = False
  go Crusader           = False
  go Monk               = True
  go Zealot             = True
  go Cavalier           = False
  go Champion           = False
  go Angel              = False
  go Archangel          = False
  -- Rampart
  go Centaur            = False
  go CentaurCaptain     = False
  go Dwarf              = False
  go BattleDwarf        = False
  go WoodElf            = True
  go GrandElf           = True
  go Pegasus            = False
  go SilverPegasus      = False
  go DendroidGuard      = False
  go DendroidSoldier    = False
  go Unicorn            = False
  go WarUnicorn         = False
  go GreenDragon        = False
  go GoldDragon         = False
  -- Tower
  go Gremlin            = False
  go MasterGremlin      = True
  go StoneGargoyle      = False
  go ObsidianGargoyle   = False
  go StoneGolem         = False
  go IronGolem          = False
  go Mage               = True
  go Archmage           = True
  go Genie              = False
  go MasterGenie        = False
  go Naga               = False
  go NagaQueen          = False
  go Giant              = False
  go Titan              = True
  -- Citadel
  go Goblin             = False
  go Hobgoblin          = False
  go WolfRider          = False
  go WolfRaider         = False
  go Orc                = True
  go OrcChieftain       = True
  go Ogre               = False
  go OgreMage           = False
  go Roc                = False
  go Thunderbird        = False
  go Cyclops            = True
  go CyclopsKing        = True
  go Behemoth           = False
  go AncientBehemoth    = False
  -- Fortress
  go Gnoll              = False
  go GnollMarauder      = False
  go Lizardman          = True
  go LizardWarrior      = True
  go SerpentFly         = False
  go DragonFly          = False
  go Basilisk           = False
  go GreaterBasilisk    = False
  go Gorgon             = False
  go MightyGorgon       = False
  go Wyvern             = False
  go WyvernMonarch      = False
  go Hydra              = False
  go ChaosHydra         = False
  -- Dungeon
  go Troglodyte         = False
  go InfernalTroglodyte = False
  go Harpy              = False
  go HarpyHag           = False
  go Beholder           = True
  go EvilEye            = True
  go Medusa             = True
  go MedusaQueen        = True
  go Minotaur           = False
  go MinotaurKing       = False
  go Manticore          = False
  go Scorpicore         = False
  go RedDragon          = False
  go BlackDragon        = False
  -- Necropolis
  go Skeleton           = False
  go SkeletonWarrior    = False
  go WalkingDead        = False
  go Zombie             = False
  go Wight              = False
  go Wraith             = False
  go Vampire            = False
  go VampireLord        = False
  go Lich               = True
  go PowerLich          = True
  go BlackKnight        = False
  go DreadKnight        = False
  go BoneDragon         = False
  go GhostDragon        = False
  -- Inferno
  go Imp                = False
  go Familiar           = False
  go Gog                = True
  go Magog              = True
  go HellHound          = False
  go Cerberus           = False
  go Demon              = False
  go HornedDemon        = False
  go PitFiend           = False
  go PitLord            = False
  go Efreet             = False
  go EfreetSultan       = False
  go Devil              = False
  go ArchDevil          = False
  -- Conflux
  go Pixie              = False
  go Sprite             = False
  go AirElemental       = False
  go StormElemental     = True
  go WaterElemental     = False
  go IceElemental       = True
  go FireElemental      = False
  go EnergyElemental    = False
  go EarthElemental     = False
  go MagmaElemental     = False
  go PsychicElemental   = False
  go MagicElemental     = False
  go Firebird           = False
  go Phoenix            = False

flies :: Creature -> Bool
flies = go
  where
  -- Castle
  go Pikeman            = False
  go Halberdier         = False
  go Archer             = False
  go Marksman           = False
  go Griffin            = True
  go RoyalGriffin       = True
  go Swordsman          = False
  go Crusader           = False
  go Monk               = False
  go Zealot             = False
  go Cavalier           = False
  go Champion           = False
  go Angel              = True
  go Archangel          = True
  -- Rampart
  go Centaur            = False
  go CentaurCaptain     = False
  go Dwarf              = False
  go BattleDwarf        = False
  go WoodElf            = False
  go GrandElf           = False
  go Pegasus            = True
  go SilverPegasus      = True
  go DendroidGuard      = False
  go DendroidSoldier    = False
  go Unicorn            = False
  go WarUnicorn         = False
  go GreenDragon        = True
  go GoldDragon         = True
  -- Tower
  go Gremlin            = False
  go MasterGremlin      = False
  go StoneGargoyle      = True
  go ObsidianGargoyle   = True
  go StoneGolem         = False
  go IronGolem          = False
  go Mage               = False
  go Archmage           = False
  go Genie              = True
  go MasterGenie        = True
  go Naga               = False
  go NagaQueen          = False
  go Giant              = False
  go Titan              = False
  -- Citadel
  go Goblin             = False
  go Hobgoblin          = False
  go WolfRider          = False
  go WolfRaider         = False
  go Orc                = False
  go OrcChieftain       = False
  go Ogre               = False
  go OgreMage           = False
  go Roc                = True
  go Thunderbird        = True
  go Cyclops            = False
  go CyclopsKing        = False
  go Behemoth           = False
  go AncientBehemoth    = False
  -- Fortress
  go Gnoll              = False
  go GnollMarauder      = False
  go Lizardman          = False
  go LizardWarrior      = False
  go SerpentFly         = True
  go DragonFly          = True
  go Basilisk           = False
  go GreaterBasilisk    = False
  go Gorgon             = False
  go MightyGorgon       = False
  go Wyvern             = True
  go WyvernMonarch      = True
  go Hydra              = False
  go ChaosHydra         = False
  -- Dungeon
  go Troglodyte         = False
  go InfernalTroglodyte = False
  go Harpy              = True
  go HarpyHag           = True
  go Beholder           = False
  go EvilEye            = False
  go Medusa             = False
  go MedusaQueen        = False
  go Minotaur           = False
  go MinotaurKing       = False
  go Manticore          = True
  go Scorpicore         = True
  go RedDragon          = True
  go BlackDragon        = True
  -- Necropolis
  go Skeleton           = False
  go SkeletonWarrior    = False
  go WalkingDead        = False
  go Zombie             = False
  go Wight              = True
  go Wraith             = True
  go Vampire            = True
  go VampireLord        = True
  go Lich               = False
  go PowerLich          = False
  go BlackKnight        = False
  go DreadKnight        = False
  go BoneDragon         = True
  go GhostDragon        = True
  -- Inferno
  go Imp                = False
  go Familiar           = False
  go Gog                = False
  go Magog              = False
  go HellHound          = False
  go Cerberus           = False
  go Demon              = False
  go HornedDemon        = False
  go PitFiend           = False
  go PitLord            = False
  go Efreet             = True
  go EfreetSultan       = True
  go Devil              = False
  go ArchDevil          = False
  -- Conflux
  go Pixie              = True
  go Sprite             = True
  go AirElemental       = False
  go StormElemental     = False
  go WaterElemental     = False
  go IceElemental       = False
  go FireElemental      = True
  go EnergyElemental    = True
  go EarthElemental     = False
  go MagmaElemental     = False
  go PsychicElemental   = False
  go MagicElemental     = False
  go Firebird           = True
  go Phoenix            = True

wide :: Creature -> Bool
wide = go
  where
  -- Castle
  go Pikeman            = False
  go Halberdier         = False
  go Archer             = False
  go Marksman           = False
  go Griffin            = True
  go RoyalGriffin       = True
  go Swordsman          = False
  go Crusader           = False
  go Monk               = False
  go Zealot             = False
  go Cavalier           = True
  go Champion           = True
  go Angel              = True
  go Archangel          = True
  -- Rampart
  go Centaur            = True
  go CentaurCaptain     = True
  go Dwarf              = False
  go BattleDwarf        = False
  go WoodElf            = False
  go GrandElf           = False
  go Pegasus            = True
  go SilverPegasus      = True
  go DendroidGuard      = False
  go DendroidSoldier    = False
  go Unicorn            = True
  go WarUnicorn         = True
  go GreenDragon        = True
  go GoldDragon         = True
  -- Tower
  go Gremlin            = False
  go MasterGremlin      = False
  go StoneGargoyle      = False
  go ObsidianGargoyle   = False
  go StoneGolem         = False
  go IronGolem          = False
  go Mage               = False
  go Archmage           = False
  go Genie              = False
  go MasterGenie        = False
  go Naga               = True
  go NagaQueen          = True
  go Giant              = True
  go Titan              = True
  -- Citadel
  go Goblin             = False
  go Hobgoblin          = False
  go WolfRider          = True
  go WolfRaider         = True
  go Orc                = False
  go OrcChieftain       = False
  go Ogre               = False
  go OgreMage           = False
  go Roc                = True
  go Thunderbird        = True
  go Cyclops            = False
  go CyclopsKing        = False
  go Behemoth           = True
  go AncientBehemoth    = True
  -- Fortress
  go Gnoll              = False
  go GnollMarauder      = False
  go Lizardman          = False
  go LizardWarrior      = False
  go SerpentFly         = False
  go DragonFly          = False
  go Basilisk           = True
  go GreaterBasilisk    = True
  go Gorgon             = True
  go MightyGorgon       = True
  go Wyvern             = True
  go WyvernMonarch      = True
  go Hydra              = True
  go ChaosHydra         = True
  -- Dungeon
  go Troglodyte         = False
  go InfernalTroglodyte = False
  go Harpy              = False
  go HarpyHag           = False
  go Beholder           = False
  go EvilEye            = False
  go Medusa             = True
  go MedusaQueen        = True
  go Minotaur           = False
  go MinotaurKing       = False
  go Manticore          = True
  go Scorpicore         = True
  go RedDragon          = True
  go BlackDragon        = True
  -- Necropolis
  go Skeleton           = False
  go SkeletonWarrior    = False
  go WalkingDead        = False
  go Zombie             = False
  go Wight              = False
  go Wraith             = False
  go Vampire            = False
  go VampireLord        = False
  go Lich               = False
  go PowerLich          = False
  go BlackKnight        = True
  go DreadKnight        = True
  go BoneDragon         = True
  go GhostDragon        = True
  -- Inferno
  go Imp                = False
  go Familiar           = False
  go Gog                = False
  go Magog              = False
  go HellHound          = True
  go Cerberus           = True
  go Demon              = False
  go HornedDemon        = False
  go PitFiend           = False
  go PitLord            = False
  go Efreet             = False
  go EfreetSultan       = False
  go Devil              = False
  go ArchDevil          = False
  -- Conflux
  go Pixie              = False
  go Sprite             = False
  go AirElemental       = False
  go StormElemental     = False
  go WaterElemental     = True
  go IceElemental       = True
  go FireElemental      = False
  go EnergyElemental    = False
  go EarthElemental     = False
  go MagmaElemental     = False
  go PsychicElemental   = False
  go MagicElemental     = False
  go Firebird           = True
  go Phoenix            = True

grade :: Creature -> Grade
grade = go
  where
  -- Castle
  go Pikeman            = Basic
  go Halberdier         = Upgraded
  go Archer             = Basic
  go Marksman           = Upgraded
  go Griffin            = Basic
  go RoyalGriffin       = Upgraded
  go Swordsman          = Basic
  go Crusader           = Upgraded
  go Monk               = Basic
  go Zealot             = Upgraded
  go Cavalier           = Basic
  go Champion           = Upgraded
  go Angel              = Basic
  go Archangel          = Upgraded
  -- Rampart
  go Centaur            = Basic
  go CentaurCaptain     = Upgraded
  go Dwarf              = Basic
  go BattleDwarf        = Upgraded
  go WoodElf            = Basic
  go GrandElf           = Upgraded
  go Pegasus            = Basic
  go SilverPegasus      = Upgraded
  go DendroidGuard      = Basic
  go DendroidSoldier    = Upgraded
  go Unicorn            = Basic
  go WarUnicorn         = Upgraded
  go GreenDragon        = Basic
  go GoldDragon         = Upgraded
  -- Tower
  go Gremlin            = Basic
  go MasterGremlin      = Upgraded
  go StoneGargoyle      = Basic
  go ObsidianGargoyle   = Upgraded
  go StoneGolem         = Basic
  go IronGolem          = Upgraded
  go Mage               = Basic
  go Archmage           = Upgraded
  go Genie              = Basic
  go MasterGenie        = Upgraded
  go Naga               = Basic
  go NagaQueen          = Upgraded
  go Giant              = Basic
  go Titan              = Upgraded
  -- Citadel
  go Goblin             = Basic
  go Hobgoblin          = Upgraded
  go WolfRider          = Basic
  go WolfRaider         = Upgraded
  go Orc                = Basic
  go OrcChieftain       = Upgraded
  go Ogre               = Basic
  go OgreMage           = Upgraded
  go Roc                = Basic
  go Thunderbird        = Upgraded
  go Cyclops            = Basic
  go CyclopsKing        = Upgraded
  go Behemoth           = Basic
  go AncientBehemoth    = Upgraded
  -- Fortress
  go Gnoll              = Basic
  go GnollMarauder      = Upgraded
  go Lizardman          = Basic
  go LizardWarrior      = Upgraded
  go SerpentFly         = Basic
  go DragonFly          = Upgraded
  go Basilisk           = Basic
  go GreaterBasilisk    = Upgraded
  go Gorgon             = Basic
  go MightyGorgon       = Upgraded
  go Wyvern             = Basic
  go WyvernMonarch      = Upgraded
  go Hydra              = Basic
  go ChaosHydra         = Upgraded
  -- Dungeon
  go Troglodyte         = Basic
  go InfernalTroglodyte = Upgraded
  go Harpy              = Basic
  go HarpyHag           = Upgraded
  go Beholder           = Basic
  go EvilEye            = Upgraded
  go Medusa             = Basic
  go MedusaQueen        = Upgraded
  go Minotaur           = Basic
  go MinotaurKing       = Upgraded
  go Manticore          = Basic
  go Scorpicore         = Upgraded
  go RedDragon          = Basic
  go BlackDragon        = Upgraded
  -- Necropolis
  go Skeleton           = Basic
  go SkeletonWarrior    = Upgraded
  go WalkingDead        = Basic
  go Zombie             = Upgraded
  go Wight              = Basic
  go Wraith             = Upgraded
  go Vampire            = Basic
  go VampireLord        = Upgraded
  go Lich               = Basic
  go PowerLich          = Upgraded
  go BlackKnight        = Basic
  go DreadKnight        = Upgraded
  go BoneDragon         = Basic
  go GhostDragon        = Upgraded
  -- Inferno
  go Imp                = Basic
  go Familiar           = Upgraded
  go Gog                = Basic
  go Magog              = Upgraded
  go HellHound          = Basic
  go Cerberus           = Upgraded
  go Demon              = Basic
  go HornedDemon        = Upgraded
  go PitFiend           = Basic
  go PitLord            = Upgraded
  go Efreet             = Basic
  go EfreetSultan       = Upgraded
  go Devil              = Basic
  go ArchDevil          = Upgraded
  -- Conflux
  go Pixie              = Basic
  go Sprite             = Upgraded
  go AirElemental       = Basic
  go StormElemental     = Upgraded
  go WaterElemental     = Basic
  go IceElemental       = Upgraded
  go FireElemental      = Basic
  go EnergyElemental    = Upgraded
  go EarthElemental     = Basic
  go MagmaElemental     = Upgraded
  go PsychicElemental   = Basic
  go MagicElemental     = Upgraded
  go Firebird           = Basic
  go Phoenix            = Upgraded

level :: Creature -> Level
level = go
  where
  -- Castle
  go Pikeman            = L1
  go Halberdier         = L1
  go Archer             = L2
  go Marksman           = L2
  go Griffin            = L3
  go RoyalGriffin       = L3
  go Swordsman          = L4
  go Crusader           = L4
  go Monk               = L5
  go Zealot             = L5
  go Cavalier           = L6
  go Champion           = L6
  go Angel              = L7
  go Archangel          = L7
  -- Rampart
  go Centaur            = L1
  go CentaurCaptain     = L1
  go Dwarf              = L2
  go BattleDwarf        = L2
  go WoodElf            = L3
  go GrandElf           = L3
  go Pegasus            = L4
  go SilverPegasus      = L4
  go DendroidGuard      = L5
  go DendroidSoldier    = L5
  go Unicorn            = L6
  go WarUnicorn         = L6
  go GreenDragon        = L7
  go GoldDragon         = L7
  -- Tower
  go Gremlin            = L1
  go MasterGremlin      = L1
  go StoneGargoyle      = L2
  go ObsidianGargoyle   = L2
  go StoneGolem         = L3
  go IronGolem          = L3
  go Mage               = L4
  go Archmage           = L4
  go Genie              = L5
  go MasterGenie        = L5
  go Naga               = L6
  go NagaQueen          = L6
  go Giant              = L7
  go Titan              = L7
  -- Citadel
  go Goblin             = L1
  go Hobgoblin          = L1
  go WolfRider          = L2
  go WolfRaider         = L2
  go Orc                = L3
  go OrcChieftain       = L3
  go Ogre               = L4
  go OgreMage           = L4
  go Roc                = L5
  go Thunderbird        = L5
  go Cyclops            = L6
  go CyclopsKing        = L6
  go Behemoth           = L7
  go AncientBehemoth    = L7
  -- Fortress
  go Gnoll              = L1
  go GnollMarauder      = L1
  go Lizardman          = L2
  go LizardWarrior      = L2
  go SerpentFly         = L3
  go DragonFly          = L3
  go Basilisk           = L4
  go GreaterBasilisk    = L4
  go Gorgon             = L5
  go MightyGorgon       = L5
  go Wyvern             = L6
  go WyvernMonarch      = L6
  go Hydra              = L7
  go ChaosHydra         = L7
  -- Dungeon
  go Troglodyte         = L1
  go InfernalTroglodyte = L1
  go Harpy              = L2
  go HarpyHag           = L2
  go Beholder           = L3
  go EvilEye            = L3
  go Medusa             = L4
  go MedusaQueen        = L4
  go Minotaur           = L5
  go MinotaurKing       = L5
  go Manticore          = L6
  go Scorpicore         = L6
  go RedDragon          = L7
  go BlackDragon        = L7
  -- Necropolis
  go Skeleton           = L1
  go SkeletonWarrior    = L1
  go WalkingDead        = L2
  go Zombie             = L2
  go Wight              = L3
  go Wraith             = L3
  go Vampire            = L4
  go VampireLord        = L4
  go Lich               = L5
  go PowerLich          = L5
  go BlackKnight        = L6
  go DreadKnight        = L6
  go BoneDragon         = L7
  go GhostDragon        = L7
  -- Inferno
  go Imp                = L1
  go Familiar           = L1
  go Gog                = L2
  go Magog              = L2
  go HellHound          = L3
  go Cerberus           = L3
  go Demon              = L4
  go HornedDemon        = L4
  go PitFiend           = L5
  go PitLord            = L5
  go Efreet             = L6
  go EfreetSultan       = L6
  go Devil              = L7
  go ArchDevil          = L7
  -- Conflux
  go Pixie              = L1
  go Sprite             = L1
  go AirElemental       = L2
  go StormElemental     = L2
  go WaterElemental     = L3
  go IceElemental       = L3
  go FireElemental      = L4
  go EnergyElemental    = L4
  go EarthElemental     = L5
  go MagmaElemental     = L5
  go PsychicElemental   = L6
  go MagicElemental     = L6
  go Firebird           = L7
  go Phoenix            = L7

town :: Creature -> Town
town = go
  where
  -- Castle
  go Pikeman            = Castle
  go Halberdier         = Castle
  go Archer             = Castle
  go Marksman           = Castle
  go Griffin            = Castle
  go RoyalGriffin       = Castle
  go Swordsman          = Castle
  go Crusader           = Castle
  go Monk               = Castle
  go Zealot             = Castle
  go Cavalier           = Castle
  go Champion           = Castle
  go Angel              = Castle
  go Archangel          = Castle
  -- Rampart
  go Centaur            = Rampart
  go CentaurCaptain     = Rampart
  go Dwarf              = Rampart
  go BattleDwarf        = Rampart
  go WoodElf            = Rampart
  go GrandElf           = Rampart
  go Pegasus            = Rampart
  go SilverPegasus      = Rampart
  go DendroidGuard      = Rampart
  go DendroidSoldier    = Rampart
  go Unicorn            = Rampart
  go WarUnicorn         = Rampart
  go GreenDragon        = Rampart
  go GoldDragon         = Rampart
  -- Tower
  go Gremlin            = Tower
  go MasterGremlin      = Tower
  go StoneGargoyle      = Tower
  go ObsidianGargoyle   = Tower
  go StoneGolem         = Tower
  go IronGolem          = Tower
  go Mage               = Tower
  go Archmage           = Tower
  go Genie              = Tower
  go MasterGenie        = Tower
  go Naga               = Tower
  go NagaQueen          = Tower
  go Giant              = Tower
  go Titan              = Tower
  -- Citadel
  go Goblin             = Citadel
  go Hobgoblin          = Citadel
  go WolfRider          = Citadel
  go WolfRaider         = Citadel
  go Orc                = Citadel
  go OrcChieftain       = Citadel
  go Ogre               = Citadel
  go OgreMage           = Citadel
  go Roc                = Citadel
  go Thunderbird        = Citadel
  go Cyclops            = Citadel
  go CyclopsKing        = Citadel
  go Behemoth           = Citadel
  go AncientBehemoth    = Citadel
  -- Fortress
  go Gnoll              = Fortress
  go GnollMarauder      = Fortress
  go Lizardman          = Fortress
  go LizardWarrior      = Fortress
  go SerpentFly         = Fortress
  go DragonFly          = Fortress
  go Basilisk           = Fortress
  go GreaterBasilisk    = Fortress
  go Gorgon             = Fortress
  go MightyGorgon       = Fortress
  go Wyvern             = Fortress
  go WyvernMonarch      = Fortress
  go Hydra              = Fortress
  go ChaosHydra         = Fortress
  -- Dungeon
  go Troglodyte         = Dungeon
  go InfernalTroglodyte = Dungeon
  go Harpy              = Dungeon
  go HarpyHag           = Dungeon
  go Beholder           = Dungeon
  go EvilEye            = Dungeon
  go Medusa             = Dungeon
  go MedusaQueen        = Dungeon
  go Minotaur           = Dungeon
  go MinotaurKing       = Dungeon
  go Manticore          = Dungeon
  go Scorpicore         = Dungeon
  go RedDragon          = Dungeon
  go BlackDragon        = Dungeon
  -- Necropolis
  go Skeleton           = Necropolis
  go SkeletonWarrior    = Necropolis
  go WalkingDead        = Necropolis
  go Zombie             = Necropolis
  go Wight              = Necropolis
  go Wraith             = Necropolis
  go Vampire            = Necropolis
  go VampireLord        = Necropolis
  go Lich               = Necropolis
  go PowerLich          = Necropolis
  go BlackKnight        = Necropolis
  go DreadKnight        = Necropolis
  go BoneDragon         = Necropolis
  go GhostDragon        = Necropolis
  -- Inferno
  go Imp                = Inferno
  go Familiar           = Inferno
  go Gog                = Inferno
  go Magog              = Inferno
  go HellHound          = Inferno
  go Cerberus           = Inferno
  go Demon              = Inferno
  go HornedDemon        = Inferno
  go PitFiend           = Inferno
  go PitLord            = Inferno
  go Efreet             = Inferno
  go EfreetSultan       = Inferno
  go Devil              = Inferno
  go ArchDevil          = Inferno
  -- Conflux
  go Pixie              = Conflux
  go Sprite             = Conflux
  go AirElemental       = Conflux
  go StormElemental     = Conflux
  go WaterElemental     = Conflux
  go IceElemental       = Conflux
  go FireElemental      = Conflux
  go EnergyElemental    = Conflux
  go EarthElemental     = Conflux
  go MagmaElemental     = Conflux
  go PsychicElemental   = Conflux
  go MagicElemental     = Conflux
  go Firebird           = Conflux
  go Phoenix            = Conflux

upgradeMap :: Bimap Creature Creature
upgradeMap = Bimap.fromList
  -- Castle
  [ (Pikeman, Halberdier)
  , (Archer, Marksman)
  , (Griffin, RoyalGriffin)
  , (Swordsman, Crusader)
  , (Monk, Zealot)
  , (Cavalier, Champion)
  , (Angel, Archangel)
  -- Rampart
  , (Centaur, CentaurCaptain)
  , (Dwarf, BattleDwarf)
  , (WoodElf, GrandElf)
  , (Pegasus, SilverPegasus)
  , (DendroidGuard, DendroidSoldier)
  , (Unicorn, WarUnicorn)
  , (GreenDragon, GoldDragon)
  -- Tower
  , (Gremlin, MasterGremlin)
  , (StoneGargoyle, ObsidianGargoyle)
  , (StoneGolem, IronGolem)
  , (Mage, Archmage)
  , (Genie, MasterGenie)
  , (Naga, NagaQueen)
  , (Giant, Titan)
  -- Citadel
  , (Goblin, Hobgoblin)
  , (WolfRider, WolfRaider)
  , (Orc, OrcChieftain)
  , (Ogre, OgreMage)
  , (Roc, Thunderbird)
  , (Cyclops, CyclopsKing)
  , (Behemoth, AncientBehemoth)
  -- Fortress
  , (Gnoll, GnollMarauder)
  , (Lizardman, LizardWarrior)
  , (SerpentFly, DragonFly)
  , (Basilisk, GreaterBasilisk)
  , (Gorgon, MightyGorgon)
  , (Wyvern, WyvernMonarch)
  , (Hydra, ChaosHydra)
  -- Dungeon
  , (Troglodyte, InfernalTroglodyte)
  , (Harpy, HarpyHag)
  , (Beholder, EvilEye)
  , (Medusa, MedusaQueen)
  , (Minotaur, MinotaurKing)
  , (Manticore, Scorpicore)
  , (RedDragon, BlackDragon)
  -- Necropolis
  , (Skeleton, SkeletonWarrior)
  , (WalkingDead, Zombie)
  , (Wight, Wraith)
  , (Vampire, VampireLord)
  , (Lich, PowerLich)
  , (BlackKnight, DreadKnight)
  , (BoneDragon, GhostDragon)
  -- Inferno
  , (Imp, Familiar)
  , (Gog, Magog)
  , (HellHound, Cerberus)
  , (Demon, HornedDemon)
  , (PitFiend, PitLord)
  , (Efreet, EfreetSultan)
  , (Devil, ArchDevil)
  -- Conflux
  , (Pixie, Sprite)
  , (AirElemental, StormElemental)
  , (WaterElemental, IceElemental)
  , (FireElemental, EnergyElemental)
  , (EarthElemental, MagmaElemental)
  , (PsychicElemental, MagicElemental)
  , (Firebird, Phoenix)
  ]

upgraded :: Creature -> Maybe Creature
upgraded = (`Bimap.lookup` upgradeMap)

downgraded :: Creature -> Maybe Creature
downgraded = (`Bimap.lookupR` upgradeMap)

data SFX
  = SFX'Haste
  | SFX'Slow
  -- | SFX'Sacrifice
  deriving (Eq, Ord, Show, Generic)

sSndName :: SFX -> String
sSndName = \case
  SFX'Haste -> "TAILWIND"
  SFX'Slow -> "MUCKMIRE"
  -- SFX'Sacrifice -> "SACRIF1"

sDefName :: SFX -> String
sDefName = \case
  SFX'Haste -> "C15SPA0"
  SFX'Slow -> "C09SPE0"
  -- SFX'Sacrifice -> "C01SPE0"
  --
-- XXX DeriveAnyClass
instance GEnum Creature
instance GEnum SFX
