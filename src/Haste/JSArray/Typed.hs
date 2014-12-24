{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts #-}

module Haste.JSArray.Typed where

import Control.DeepSeq (NFData, force)
import Control.Monad (liftM2)
import Haste.Foreign
import qualified Haste.JSArray as JA
import Haste.Prim
import System.IO.Unsafe (unsafePerformIO)

newtype ArrayBuffer = ArrayBuffer JSAny deriving (Pack, Unpack)

newArrayBuffer::Int->IO ArrayBuffer
newArrayBuffer = ffi "(function(byteLength) {return ArrayBuffer(byteLength);})"

arrayBufferLength::ArrayBuffer->Int
arrayBufferLength = unsafePerformIO . ffi "(function(buf) {return buf.byteLength;})"

class (Pack a, Unpack a)=>TypedArray a where
  type EltType a
  newSizedArray::Int->IO a
  copyTypedArray::TypedArray b=>b->a
  fromList::Unpack (EltType a)=>[EltType a]->a
  viewArrayBufferSized::Int->Int->ArrayBuffer->a
  viewArrayBuffer::ArrayBuffer->a
  fromJSArray::JA.JSArray (EltType a)->a

  byteLength::a->Int
  byteLength = unsafePerformIO . ffi "(function(arr) {return arr.byteLength;})"
  bytesPerElement::a->Int
  bytesPerElement = unsafePerformIO . ffi "(function(arr) {return arr.BYTES_PER_ELEMENT;})"
  arrayLength::a->Int
  arrayLength arr = quot (byteLength arr) (bytesPerElement arr)

  getIndex::Pack (EltType a)=>a->Int->IO (EltType a)
  getIndex = ffi "(function(arr, idx) {return arr[idx];})"
  setIndex::Unpack (EltType a)=>a->Int->EltType a->IO ()
  setIndex = ffi "(function(arr, idx, val) {arr[idx] = val;})"
  toList::(Pack (EltType a), NFData (EltType a))=>a->[EltType a]
  toList arr = force $ fmap (unsafePerformIO . getIndex arr)
               [0..arrayLength arr - 1]
  toJSArray::a->JA.JSArray (EltType a)
  toJSArray = unsafePerformIO . ffi "(function(arr) {return Array.prototype.slice.call(arr);})"

newtype Int8Array = Int8Array JSAny deriving (Pack, Unpack)

instance TypedArray Int8Array where
  type (EltType Int8Array) = Int
  newSizedArray = ffi "(function(size) {return new Int8Array(size);})"
  copyTypedArray = unsafePerformIO . ffi "(function(other) {return new Int8Array(other);})"
  fromList = unsafePerformIO . ffi "(function(other) {return new Int8Array(other);})"
  viewArrayBufferSized start len buf = unsafePerformIO $ viewSized start len buf
    where
      viewSized::Int->Int->ArrayBuffer->IO Int8Array
      viewSized = ffi "(function(byteOffset, length, buffer) {return new Int8Array(buffer, byteOffset, length);})"
  viewArrayBuffer = unsafePerformIO . ffi "(function(buffer) {return new Int8Array(buffer);})"
  fromJSArray = unsafePerformIO . ffi "(function(buffer) {return new Int8Array(buffer);})"

newtype Int16Array = Int16Array JSAny deriving (Pack, Unpack)

instance TypedArray Int16Array where
  type (EltType Int16Array) = Int
  newSizedArray = ffi "(function(size) {return new Int16Array(size);})"
  copyTypedArray = unsafePerformIO . ffi "(function(other) {return new Int16Array(other);})"
  fromList = unsafePerformIO . ffi "(function(other) {return new Int16Array(other);})"
  viewArrayBufferSized start len buf = unsafePerformIO $ viewSized start len buf
    where
      viewSized::Int->Int->ArrayBuffer->IO Int16Array
      viewSized = ffi "(function(byteOffset, length, buffer) {return new Int16Array(buffer, byteOffset, length);})"
  viewArrayBuffer = unsafePerformIO . ffi "(function(buffer) {return new Int16Array(buffer);})"
  fromJSArray = unsafePerformIO . ffi "(function(buffer) {return new Int16Array(buffer);})"

newtype Int32Array = Int32Array JSAny deriving (Pack, Unpack)

instance TypedArray Int32Array where
  type (EltType Int32Array) = Int
  newSizedArray = ffi "(function(size) {return new Int32Array(size);})"
  copyTypedArray = unsafePerformIO . ffi "(function(other) {return new Int32Array(other);})"
  fromList = unsafePerformIO . ffi "(function(other) {return new Int32Array(other);})"
  viewArrayBufferSized start len buf = unsafePerformIO $ viewSized start len buf
    where
      viewSized::Int->Int->ArrayBuffer->IO Int32Array
      viewSized = ffi "(function(byteOffset, length, buffer) {return new Int32Array(buffer, byteOffset, length);})"
  viewArrayBuffer = unsafePerformIO . ffi "(function(buffer) {return new Int32Array(buffer);})"
  fromJSArray = unsafePerformIO . ffi "(function(buffer) {return new Int32Array(buffer);})"

newtype Int64Array = Int64Array JSAny deriving (Pack, Unpack)

instance TypedArray Int64Array where
  type (EltType Int64Array) = Int
  newSizedArray = ffi "(function(size) {return new Int64Array(size);})"
  copyTypedArray = unsafePerformIO . ffi "(function(other) {return new Int64Array(other);})"
  fromList = unsafePerformIO . ffi "(function(other) {return new Int64Array(other);})"
  viewArrayBufferSized start len buf = unsafePerformIO $ viewSized start len buf
    where
      viewSized::Int->Int->ArrayBuffer->IO Int64Array
      viewSized = ffi "(function(byteOffset, length, buffer) {return new Int64Array(buffer, byteOffset, length);})"
  viewArrayBuffer = unsafePerformIO . ffi "(function(buffer) {return new Int64Array(buffer);})"
  fromJSArray = unsafePerformIO . ffi "(function(buffer) {return new Int64Array(buffer);})"

newtype Uint8Array = Uint8Array JSAny deriving (Pack, Unpack)

instance TypedArray Uint8Array where
  type (EltType Uint8Array) = Int
  newSizedArray = ffi "(function(size) {return new Uint8Array(size);})"
  copyTypedArray = unsafePerformIO . ffi "(function(other) {return new Uint8Array(other);})"
  fromList = unsafePerformIO . ffi "(function(other) {return new Uint8Array(other);})"
  viewArrayBufferSized start len buf = unsafePerformIO $ viewSized start len buf
    where
      viewSized::Int->Int->ArrayBuffer->IO Uint8Array
      viewSized = ffi "(function(byteOffset, length, buffer) {return new Uint8Array(buffer, byteOffset, length);})"
  viewArrayBuffer = unsafePerformIO . ffi "(function(buffer) {return new Uint8Array(buffer);})"
  fromJSArray = unsafePerformIO . ffi "(function(buffer) {return new Uint8Array(buffer);})"

newtype Uint16Array = Uint16Array JSAny deriving (Pack, Unpack)

instance TypedArray Uint16Array where
  type (EltType Uint16Array) = Int
  newSizedArray = ffi "(function(size) {return new Uint16Array(size);})"
  copyTypedArray = unsafePerformIO . ffi "(function(other) {return new Uint16Array(other);})"
  fromList = unsafePerformIO . ffi "(function(other) {return new Uint16Array(other);})"
  viewArrayBufferSized start len buf = unsafePerformIO $ viewSized start len buf
    where
      viewSized::Int->Int->ArrayBuffer->IO Uint16Array
      viewSized = ffi "(function(byteOffset, length, buffer) {return new Uint16Array(buffer, byteOffset, length);})"
  viewArrayBuffer = unsafePerformIO . ffi "(function(buffer) {return new Uint16Array(buffer);})"
  fromJSArray = unsafePerformIO . ffi "(function(buffer) {return new Uint16Array(buffer);})"

newtype Uint32Array = Uint32Array JSAny deriving (Pack, Unpack)

instance TypedArray Uint32Array where
  type (EltType Uint32Array) = Int
  newSizedArray = ffi "(function(size) {return new Uint32Array(size);})"
  copyTypedArray = unsafePerformIO . ffi "(function(other) {return new Uint32Array(other);})"
  fromList = unsafePerformIO . ffi "(function(other) {return new Uint32Array(other);})"
  viewArrayBufferSized start len buf = unsafePerformIO $ viewSized start len buf
    where
      viewSized::Int->Int->ArrayBuffer->IO Uint32Array
      viewSized = ffi "(function(byteOffset, length, buffer) {return new Uint32Array(buffer, byteOffset, length);})"
  viewArrayBuffer = unsafePerformIO . ffi "(function(buffer) {return new Uint32Array(buffer);})"
  fromJSArray = unsafePerformIO . ffi "(function(buffer) {return new Uint32Array(buffer);})"

newtype Float32Array = Float32Array JSAny deriving (Pack, Unpack)

instance TypedArray Float32Array where
  type (EltType Float32Array) = Double
  newSizedArray = ffi "(function(size) {return new Float32Array(size);})"
  copyTypedArray = unsafePerformIO . ffi "(function(other) {return new Float32Array(other);})"
  fromList = unsafePerformIO . ffi "(function(other) {return new Float32Array(other);})"
  viewArrayBufferSized start len buf = unsafePerformIO $ viewSized start len buf
    where
      viewSized::Int->Int->ArrayBuffer->IO Float32Array
      viewSized = ffi "(function(byteOffset, length, buffer) {return new Float32Array(buffer, byteOffset, length);})"
  viewArrayBuffer = unsafePerformIO . ffi "(function(buffer) {return new Float32Array(buffer);})"
  fromJSArray = unsafePerformIO . ffi "(function(buffer) {return new Float32Array(buffer);})"
