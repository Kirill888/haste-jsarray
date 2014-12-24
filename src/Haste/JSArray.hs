{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Haste.JSArray where

import Control.Applicative hiding ((<*>), pure)
import qualified Control.Applicative as Ap ((<*>), pure)
import Control.DeepSeq (NFData, force)
import Control.Monad (liftM, mapM_)
import Data.List (foldl')
import Haste.Foreign
import Haste.Prim
import Prelude hiding ((!), (++), concat, concatMap, drop, fmap, length)
import qualified Prelude as P (fmap, return)
import System.IO.Unsafe (unsafePerformIO)

newtype JSArray a = JSArray JSAny deriving (Pack, Unpack)

newJSArray::IO (JSArray a)
newJSArray = ffi "(function() {return [];})"

(++)::JSArray a->JSArray a->JSArray a
(++) a b = unsafePerformIO $ plus a b
  where
    plus :: JSArray a->JSArray a->IO (JSArray a)
    plus = ffi "(function(a, b) {return a.concat(b);})"

head::Pack a=>JSArray a->IO (Maybe a)
head arr = do
  len <- length arr
  case len of
    0 -> return Nothing
    _ -> Just <$> arr ! 0

last::Pack a=>JSArray a->IO (Maybe a)
last arr = do
  len <- length arr
  case len of
    0 -> return Nothing
    _ -> Just <$> arr ! (len - 1)

tail::JSArray a->JSArray a
tail = drop 1

init::JSArray a->JSArray a
init = slice 0 (negate 1)

null::JSArray a->IO Bool
null = liftM (0==) . length

length::JSArray a->IO Int
length = ffi "(function(arr) {return arr.length;})"

fmap::(Pack a, Unpack b, NFData a)=>(a->b)->JSArray a->JSArray b
fmap f xs = fromList . P.fmap f . toList $ xs

reverse::JSArray a->JSArray a
reverse = unsafePerformIO . ffi "(function(arr) {var result = arr.slice();result.reverse();return result;})"

foldl::(Pack a, Pack b, Unpack b)=>(b->a->b)->b->JSArray a->b
foldl f x0 arr = unsafePerformIO $ foldl_ (\x y -> return $ f x y) x0 arr
  where
    foldl_::(Pack a, Pack b, Unpack b)=>(b->a->IO b)->b->JSArray a->IO b
    foldl_ = ffi "(function(f, x0, arr) {return arr.reduce(function(cumulant, cur) {return f(cumulant, cur);});})"

concat::JSArray (JSArray a)->JSArray a
concat = unsafePerformIO . ffi "(function(arrs) {return Array.prototype.concat.apply([], arrs);})"

concat'::[JSArray a]->JSArray a
concat' = foldl' (++) (unsafePerformIO newJSArray)

concatMap::(Pack a, NFData a)=>(a->JSArray b)->JSArray a->JSArray b
concatMap f = concat . fmap f

concatMap'::(Pack a, Unpack b, NFData a)=>(a->[b])->JSArray a->JSArray b
concatMap' f = concatMap (fromList . f)

slice::Int->Int->JSArray a->JSArray a
slice begin end arr = unsafePerformIO $ slice' begin end arr
  where
    slice'::Int->Int->JSArray a->IO (JSArray a)
    slice' = ffi "(function(begin, end, arr) {return arr.slice(begin, end);})"

take::Int->JSArray a->JSArray a
take = slice 0

drop::Int->JSArray a->JSArray a
drop n arr = slice n (unsafePerformIO $ length arr) arr

push::Unpack a=>JSArray a->a->IO ()
push = ffi "(function(arr, val) {arr.push(val);})"

pop::Pack a=>JSArray a->IO a
pop = ffi "(function(arr) {return arr.pop();})"

(!)::Pack a=>JSArray a->Int->IO a
(!) = ffi "(function(arr, idx) {return arr[idx];})"

zipWith::(Pack a, Pack b, Unpack c)=>(a->b->c)->JSArray a->JSArray b->JSArray c
zipWith f xs ys = unsafePerformIO $ zipWith' (\x y -> return $ f x y) xs ys
  where
    zipWith'::(Pack a, Pack b, Unpack c)=>(a->b->IO c)->JSArray a->JSArray b->IO (JSArray c)
    zipWith' = ffi "(function(f, xs, ys) {var result = [];for (var i = 0;i < Math.min(xs.length, ys.length); ++i) {result.push(f(xs[i], ys[i]));};return result})"

fromList::Unpack a=>[a]->JSArray a
fromList vals = unsafePerformIO $ do
  arr <- newJSArray
  mapM_ (push arr) vals
  return arr

toList::(Pack a, NFData a)=>JSArray a->[a]
toList arr = force $ P.fmap (unsafePerformIO . (arr !))
             [0..(unsafePerformIO . length $ arr) - 1]
