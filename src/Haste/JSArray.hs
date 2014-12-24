{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Haste.JSArray where

import Control.Applicative hiding ((<*>), pure)
import qualified Control.Applicative as Ap ((<*>), pure)
import Control.DeepSeq (NFData, force)
import Control.Monad (mapM_)
import Data.List (foldl')
import Haste.Foreign
import Haste.Prim
import Prelude hiding ((!), (++), concat, concatMap, fmap, length, return)
import qualified Prelude as P (fmap, return)
import System.IO.Unsafe (unsafePerformIO)

newtype JSArray a = JSArray JSAny deriving (Pack, Unpack)

newJSArray::IO (JSArray a)
newJSArray = ffi "(function() {return [];})"

push::Unpack a=>JSArray a->a->IO ()
push = ffi "(function(arr, val) {arr.push(val);})"

pop::Pack a=>JSArray a->IO a
pop = ffi "(function(arr) {return arr.pop();})"

(!)::Pack a=>JSArray a->Int->IO a
(!) = ffi "(function(arr, idx) {return arr[idx];})"

slice::Int->Int->JSArray a->JSArray a
slice begin end arr = unsafePerformIO $ slice' begin end arr
  where
    slice'::Int->Int->JSArray a->IO (JSArray a)
    slice' = ffi "(function(begin, end, arr) {return arr.slice(begin, end);})"

take::Int->JSArray a->JSArray a
take = slice 0

fromList::Unpack a=>[a]->JSArray a
fromList vals = unsafePerformIO $ do
  arr <- newJSArray
  mapM_ (push arr) vals
  P.return arr

toList::(Pack a, NFData a)=>JSArray a->[a]
toList arr = force $ P.fmap (unsafePerformIO . (arr !))
             [0..(unsafePerformIO . length $ arr) - 1]

(++)::JSArray a->JSArray a->JSArray a
(++) a b = unsafePerformIO $ plus a b
  where
    plus :: JSArray a->JSArray a->IO (JSArray a)
    plus = ffi "(function(a, b) {return a.concat(b);})"

concat::JSArray (JSArray a)->JSArray a
concat = unsafePerformIO . ffi "(function(arrs) {var result = [];arrs.forEach(function(arr) {result = result.concat(arr);});return result})"

concat'::[JSArray a]->JSArray a
concat' = foldl' (++) (unsafePerformIO newJSArray)

concatMap::(Pack a, NFData a)=>(a->JSArray b)->JSArray a->JSArray b
concatMap f = concat . fmap f

concatMap'::(Pack a, Unpack b, NFData a)=>(a->[b])->JSArray a->JSArray b
concatMap' f = concatMap (fromList . f)

length::JSArray a->IO Int
length = ffi "(function(arr) {return arr.length;})"

fmap::(Pack a, Unpack b, NFData a)=>(a->b)->JSArray a->JSArray b
fmap f xs = fromList . P.fmap f . toList $ xs
