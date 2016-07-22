{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.XHB.MappingState.Internal
    ( KeyMask(..)
    , ButMask(..)
    , ModMap(..)
    , KeyMap
    , keyCodesOf
    , Pointer
    , PointerMap
    , noPointer

    , MappingState(..)
    , initMapState
    , updateMapState

    ) where


import Graphics.XHB
import Graphics.XHB.Monad

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Array
import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Word
import qualified Data.Map as M
import qualified Data.Set as S


noSymbol :: KEYSYM
noSymbol = 0


-- TYPES --


data KeyMask = Shift | Lock | Control | Mod1 | Mod2 | Mod3 | Mod4 | Mod5
    deriving (Show, Ord, Eq, Enum, Ix, Bounded)

data ButMask = Button1 | Button2 | Button3 | Button4 | Button5
    deriving (Show, Ord, Eq, Enum, Ix, Bounded)


newtype ModMap = ModMap { getModMap :: KeyMask -> S.Set KEYCODE}

instance Show ModMap where
    show (ModMap modMap) = "fmap fromJust (flip lookup " ++ show assocs ++ ")"
      where assocs = [(mod, modMap mod) | mod <- [minBound..maxBound]]


type KeyMap = Array KEYCODE [KEYSYM]

keyCodesOf :: KEYSYM -> KeyMap -> [KEYCODE]
keyCodesOf sym = map fst . filter (elem sym . snd) . assocs


type Pointer = Word8
type PointerMap = Array Pointer Pointer

noPointer :: Pointer
noPointer = 0


-- PURE --


chunksOf :: Integral n => n -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = go xs where go xs = uncurry (:) . fmap go $ genericSplitAt n xs


createKeyMap :: GetKeyboardMapping -> GetKeyboardMappingReply -> KeyMap
createKeyMap (MkGetKeyboardMapping first count) (MkGetKeyboardMappingReply n syms) =
    listArray (first, first + count - 1) (chunksOf n syms)


updateKeyMap :: GetKeyboardMapping -> GetKeyboardMappingReply -> KeyMap -> KeyMap
updateKeyMap (MkGetKeyboardMapping first count) (MkGetKeyboardMappingReply n syms) keyMap =
    keyMap // zip (enumFrom first) (chunksOf n syms)


createModMap :: GetModifierMappingReply -> ModMap
createModMap (MkGetModifierMappingReply n codes) =
    ModMap $ (!!) (map (S.fromList . filter (/= 0)) (chunksOf n codes)) . fromEnum


createPointerMap :: GetPointerMappingReply -> PointerMap
createPointerMap (MkGetPointerMappingReply n pointers) =
    listArray (1, n) pointers


-- CORE --


data MappingState = MappingState
    { modMap :: ModMap
    , keyMap :: KeyMap
    , pointerMap :: PointerMap
    } deriving Show


initMapState :: MonadIO m => X m MappingState
initMapState = do
    setup <- asksX connectionSetup
    let minCode = min_keycode_Setup setup
        maxCode = max_keycode_Setup setup
        getkm = MkGetKeyboardMapping minCode (maxCode - minCode + 1)
    join . runIOU $ MappingState <$> (createModMap <$> IOU (reqAsync MkGetModifierMapping))
                                 <*> (createKeyMap getkm <$> IOU (reqAsync getkm))
                                 <*> (createPointerMap <$> IOU (reqAsync MkGetPointerMapping))


updateMapState :: MonadIO m => MappingNotifyEvent -> X m (MappingState -> MappingState)

updateMapState (MkMappingNotifyEvent MappingModifier _ _) =
    go <$> req MkGetModifierMapping
        where go resp ms = ms { modMap = createModMap resp }

updateMapState (MkMappingNotifyEvent MappingKeyboard first count) =
    go <$> req getkm
        where
            getkm = MkGetKeyboardMapping first count
            go resp ms = ms { keyMap = updateKeyMap getkm resp (keyMap ms) }

updateMapState (MkMappingNotifyEvent MappingPointer _ _) =
    go <$> req MkGetPointerMapping
        where go resp ms = ms { pointerMap = createPointerMap resp }
