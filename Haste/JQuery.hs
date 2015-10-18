{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE DataKinds, TypeOperators, GADTs #-}
module Haste.JQuery where

import Haste hiding (AttrName)
import Haste.Foreign
import Data.Char
import GHC.TypeLits

newtype JQuery = JQuery JSAny

fromJQuery :: JQuery -> JSAny
fromJQuery (JQuery q) = q

type Selector = String
type ClassName = String
type AttrName = String
type PropName = String
type HTML = String
type Plain = String

-- Internal

-- chain :: JSString -> x1 -> .. -> xn -> JSAny -> IO a
-- chain method = function(xs,q){ return q.method(xs); }

chainFFI :: Int -> String -> String
chainFFI 0 method = "(function(q){ return q." ++ method ++ "(); })"
chainFFI n method = "(function(" ++ xss ++ "q){ return q." ++ method ++ "(" ++ init xss ++ "); })" where
  xss = foldr (\x y -> x ++ "," ++ y) "" $ fmap (\i -> 'x' : show i) xs
  xs = [1..n]

class ChainMethod r where
  chain :: String -> r

instance FromAny a => ChainMethod (JSAny -> IO a) where
  chain = ffi . toJSString . chainFFI 0

instance (FromAny a, ToAny x1) => ChainMethod (x1 -> JSAny -> IO a) where
  chain = ffi . toJSString . chainFFI 1

instance (FromAny a, ToAny x1, ToAny x2) => ChainMethod (x1 -> x2 -> JSAny -> IO a) where
  chain = ffi . toJSString . chainFFI 2

instance (FromAny a, ToAny x1, ToAny x2, ToAny x3) => ChainMethod (x1 -> x2 -> x3 -> JSAny -> IO a) where
  chain = ffi . toJSString . chainFFI 3

instance (FromAny a, ToAny x1, ToAny x2, ToAny x3, ToAny x4) => ChainMethod (x1 -> x2 -> x3 -> x4 -> JSAny -> IO a) where
  chain = ffi . toJSString . chainFFI 4

-- Core

jQuery :: Selector -> IO JQuery
jQuery s = JQuery <$> f s where
  f :: Selector -> IO JSAny
  f = ffi "(function(sel){ return $(sel); })"

j :: Selector -> IO JQuery
j = jQuery

-- Attributes

addClass :: ClassName -> JQuery -> IO JQuery
addClass cls (JQuery q) = JQuery <$> chain "addClass" cls q

getAttr :: AttrName -> JQuery -> IO String
getAttr atn (JQuery q) = chain "attr" atn q

setAttr :: AttrName -> String -> JQuery -> IO JQuery
setAttr atn v (JQuery q) = JQuery <$> chain "attr" atn v q

hasClass :: ClassName -> JQuery -> IO Bool
hasClass cls (JQuery q) = chain "hasClass" cls q

getHtml :: JQuery -> IO String
getHtml (JQuery q) = chain "html" () q

setHtml :: String -> JQuery -> IO JQuery
setHtml html (JQuery q) = JQuery <$> chain "html" html q

getProp :: PropName -> JQuery -> IO String
getProp prop (JQuery q) = chain "prop" prop q

setProp :: PropName -> String -> JQuery -> IO JQuery
setProp prop v (JQuery q) = JQuery <$> chain "prop" prop v q

removeAttr :: AttrName -> JQuery -> IO JQuery
removeAttr atn (JQuery q) = JQuery <$> chain "removeAttr" atn q

removeClass :: ClassName -> JQuery -> IO JQuery
removeClass cls (JQuery q) = JQuery <$> chain "removeClass" cls q

removeProp :: PropName -> JQuery -> IO JQuery
removeProp prop (JQuery q) = JQuery <$> chain "removeProp" prop q

toggleClass :: ClassName -> JQuery -> IO JQuery
toggleClass cls (JQuery q) = JQuery <$> chain "toggleClass" cls q

getValue :: JQuery -> IO String
getValue (JQuery q) = chain "val" () q

setValue :: String -> JQuery -> IO JQuery
setValue val (JQuery q) = JQuery <$> chain "val" val q

-- Traversing

add :: Selector -> JQuery -> IO JQuery
add sel (JQuery q) = JQuery <$> chain "add" sel q

addAt :: Selector -> JQuery -> JQuery -> IO JQuery
addAt sel (JQuery qat) (JQuery q) = JQuery <$> chain "add" sel qat q

addBack :: Selector -> JQuery -> IO JQuery
addBack sel (JQuery q) = JQuery <$> chain "addBack" sel q

children :: JQuery -> IO JQuery
children (JQuery q) = JQuery <$> chain "children" q

childrenAt :: Selector -> JQuery -> IO JQuery
childrenAt sel (JQuery q) = JQuery <$> chain "children" sel q

closest :: Selector -> JQuery -> IO JQuery
closest sel (JQuery q) = JQuery <$> chain "closest" sel q

contents :: JQuery -> IO JQuery
contents (JQuery q) = JQuery <$> chain "contents" q

-- ?
each :: (JQuery -> IO JQuery) -> JQuery -> IO JQuery
each f (JQuery q) = JQuery <$> chain "each" (\any -> fromJQuery <$> f (JQuery any)) q

end :: JQuery -> IO JQuery
end (JQuery q) = JQuery <$> chain "end" q

eq :: Int -> JQuery -> IO JQuery
eq i (JQuery q) = JQuery <$> chain "eq" i q

jmap :: ((Int, Elem) -> IO JQuery) -> JQuery -> IO JQuery
jmap f (JQuery q) = JQuery <$> chain "map" (\x -> fromJQuery <$> f x) q

sfilter :: Selector -> JQuery -> IO JQuery
sfilter sel (JQuery q) = JQuery <$> chain "filter" sel q

sfind :: Selector -> JQuery -> IO JQuery
sfind sel (JQuery q) = JQuery <$> chain "find" sel q

jfirst :: JQuery -> IO JQuery
jfirst (JQuery q) = JQuery <$> chain "first" q

jlast :: JQuery -> IO JQuery
jlast (JQuery q) = JQuery <$> chain "last" q

next :: JQuery -> IO JQuery
next (JQuery q) = JQuery <$> chain "next" q

nextAt :: Selector -> JQuery -> IO JQuery
nextAt sel (JQuery q) = JQuery <$> chain "next" sel q

nextAll :: JQuery -> IO JQuery
nextAll (JQuery q) = JQuery <$> chain "nextAll" q

nextAllAt :: Selector -> JQuery -> IO JQuery
nextAllAt sel (JQuery q) = JQuery <$> chain "nextAll" sel q

nextUntil :: JQuery -> IO JQuery
nextUntil (JQuery q) = JQuery <$> chain "nextUntil" q

nextUntilAt :: Selector -> JQuery -> IO JQuery
nextUntilAt sel (JQuery q) = JQuery <$> chain "nextUntil" sel q

prev :: JQuery -> IO JQuery
prev (JQuery q) = JQuery <$> chain "prev" q

prevAt :: Selector -> JQuery -> IO JQuery
prevAt sel (JQuery q) = JQuery <$> chain "prev" sel q

prevAll :: JQuery -> IO JQuery
prevAll (JQuery q) = JQuery <$> chain "prevAll" q

prevAllAt :: Selector -> JQuery -> IO JQuery
prevAllAt sel (JQuery q) = JQuery <$> chain "prevAll" sel q

prevUntil :: JQuery -> IO JQuery
prevUntil (JQuery q) = JQuery <$> chain "prevUntil" q

prevUntilAt :: Selector -> JQuery -> IO JQuery
prevUntilAt sel (JQuery q) = JQuery <$> chain "prevUntil" sel q

has :: Selector -> JQuery -> IO JQuery
has sel (JQuery q) = JQuery <$> chain "has" sel q

is :: Selector -> JQuery -> IO JQuery
is sel (JQuery q) = JQuery <$> chain "is" sel q

snot :: Selector -> JQuery -> IO JQuery
snot sel (JQuery q) = JQuery <$> chain "not" sel q

offsetParent :: JQuery -> IO JQuery
offsetParent (JQuery q) = JQuery <$> chain "offsetParent" q

parent :: JQuery -> IO JQuery
parent (JQuery q) = JQuery <$> chain "parent" q

parentAt :: Selector -> JQuery -> IO JQuery
parentAt sel (JQuery q) = JQuery <$> chain "parent" sel q

parentsUntil :: JQuery -> IO JQuery
parentsUntil (JQuery q) = JQuery <$> chain "parentsUntil" q

parentsUntilAt :: Selector -> JQuery -> IO JQuery
parentsUntilAt sel (JQuery q) = JQuery <$> chain "parentsUntil" sel q

siblings :: JQuery -> IO JQuery
siblings (JQuery q) = JQuery <$> chain "siblings" q

siblingsAt :: Selector -> JQuery -> IO JQuery
siblingsAt sel (JQuery q) = JQuery <$> chain "siblings" sel q

sliceFrom :: Int -> JQuery -> IO JQuery
sliceFrom s (JQuery q) = JQuery <$> chain "slice" s q

slice :: Int -> Int -> JQuery -> IO JQuery
slice s t (JQuery q) = JQuery <$> chain "slice" (s,t) q

-- Manipulation

before :: HTML -> JQuery -> IO JQuery
before html (JQuery q) = JQuery <$> chain "after" html q

after :: HTML -> JQuery -> IO JQuery
after html (JQuery q) = JQuery <$> chain "after" html q

insertAfter :: Selector -> JQuery -> IO JQuery
insertAfter sel (JQuery q) = JQuery <$> chain "insertAfter" sel q

insertBefore :: Selector -> JQuery -> IO JQuery
insertBefore sel (JQuery q) = JQuery <$> chain "insertBefore" sel q

append :: HTML -> JQuery -> IO JQuery
append html (JQuery q) = JQuery <$> chain "after" html q

appendTo :: Selector -> JQuery -> IO JQuery
appendTo sel (JQuery q) = JQuery <$> chain "after" sel q

clone :: JQuery -> IO JQuery
clone (JQuery q) = JQuery <$> chain "clone" q

getCss :: PropName -> JQuery -> IO JQuery
getCss prop (JQuery q) = JQuery <$> chain "css" prop q

setCss :: PropName -> String -> JQuery -> IO JQuery
setCss prop v (JQuery q) = JQuery <$> chain "css" prop v q

detach :: Selector -> JQuery -> IO JQuery
detach sel (JQuery q) = JQuery <$> chain "css" sel q

empty :: JQuery -> IO JQuery
empty (JQuery q) = JQuery <$> chain "empty" q

getHeight :: JQuery -> IO Int
getHeight (JQuery q) = chain "height" q

setHeight :: Int -> JQuery -> IO JQuery
setHeight h (JQuery q) = JQuery <$> chain "height" h q

getInnerHeight :: JQuery -> IO Int
getInnerHeight (JQuery q) = chain "innerHeight" q

setInnerHeight :: Int -> JQuery -> IO JQuery
setInnerHeight h (JQuery q) = JQuery <$> chain "innerHeight" h q

getInnerWidth :: JQuery -> IO Int
getInnerWidth (JQuery q) = chain "innerWidth" q

setInnerWidth :: Int -> JQuery -> IO JQuery
setInnerWidth h (JQuery q) = JQuery <$> chain "innerWidth" h q

cssNumber :: IO JQuery
cssNumber = JQuery <$> (ffi "(function(){ return jQuery.cssNumber; })" $ ())

getOffset :: JQuery -> IO JQuery
getOffset (JQuery q) = JQuery <$> chain "offset" q

setOffset :: Plain -> JQuery -> IO JQuery
setOffset p (JQuery q) = JQuery <$> chain "offset" p q

getOuterHeight :: JQuery -> IO Int
getOuterHeight (JQuery q) = chain "outerHeight" q

setOuterHeight :: Int -> JQuery -> IO JQuery
setOuterHeight h (JQuery q) = JQuery <$> chain "outerHeight" h q

getOuterWidth :: JQuery -> IO Int
getOuterWidth (JQuery q) = chain "outerWidth" q

setOuterWidth :: Int -> JQuery -> IO JQuery
setOuterWidth h (JQuery q) = JQuery <$> chain "outerWidth" h q

position :: JQuery -> IO JSAny
position (JQuery q) = chain "position" q

prepend :: HTML -> JQuery -> IO JQuery
prepend html (JQuery q) = JQuery <$> chain "prepend" html q

prependTo :: Selector -> JQuery -> IO JQuery
prependTo sel (JQuery q) = JQuery <$> chain "prependTo" sel q

remove :: JQuery -> IO JQuery
remove (JQuery q) = JQuery <$> chain "remove" q

removeAt :: Selector -> JQuery -> IO JQuery
removeAt sel (JQuery q) = JQuery <$> chain "remove" sel q

replaceAll :: Selector -> JQuery -> IO JQuery
replaceAll sel (JQuery q) = JQuery <$> chain "replaceAll" sel q

replaceWith :: HTML -> JQuery -> IO JQuery
replaceWith html (JQuery q) = JQuery <$> chain "replaceWith" html q

getScrollLeft :: JQuery -> IO Int
getScrollLeft (JQuery q) = chain "scrollLeft" q

setScrollLeft :: Int -> JQuery -> IO JQuery
setScrollLeft v (JQuery q) = JQuery <$> chain "scrollLeft" v q

getScrollTop :: JQuery -> IO Int
getScrollTop (JQuery q) = chain "scrollTop" q

setScrollTop :: Int -> JQuery -> IO JQuery
setScrollTop v (JQuery q) = JQuery <$> chain "scrollTop" v q

getText :: JQuery -> IO String
getText (JQuery q) = chain "text" q

setText :: String -> JQuery -> IO JQuery
setText v (JQuery q) = JQuery <$> chain "text" v q

getWidth :: JQuery -> IO Int
getWidth (JQuery q) = chain "width" q

setWidth :: Int -> JQuery -> IO JQuery
setWidth v (JQuery q) = JQuery <$> chain "width" v q

unwrap :: JQuery -> IO JQuery
unwrap (JQuery q) = JQuery <$> chain "unwrap" q

wrap :: Selector -> JQuery -> IO JQuery
wrap sel (JQuery q) = JQuery <$> chain "wrap" sel q

wrapAll :: Selector -> JQuery -> IO JQuery
wrapAll sel (JQuery q) = JQuery <$> chain "wrapAll" sel q

wrapInner :: HTML -> JQuery -> IO JQuery
wrapInner html (JQuery q) = JQuery <$> chain "wrapInner" html q

-- CSS

cssHooks :: IO JQuery
cssHooks = JQuery <$> (ffi "(function(){ return jQuery.cssHooks; })" $ ())

-- Event

-- Effects

data Easing = Linear | Swing | Jswing |
              EaseInQuad | EaseOutQuad | EaseInOutQuad |
              EaseInCubic | EaseOutCubic | EaseInOutCubic |
              EaseInQuart | EaseOutQuart | EaseInOutQuart |
              EaseInQuint | EaseOutQuint | EaseInOutQuint |
              EaseInSine | EaseOutSine | EaseInOutSine |
              EaseInExpo | EaseOutExpo | EaseInOutExpo |
              EaseInCirc | EaseOutCirc | EaseInOutCirc |
              EaseInElastic | EaseOutElastic | EaseInOutElastic |
              EaseInBack | EaseOutBack | EaseInOutBack |
              EaseInBounce | EaseOutBounce | EaseInOutBounce
            deriving (Eq, Show)

showEasing :: Easing -> String
showEasing e = (\(x:xs) -> toLower x : xs) $ show e

instance ToAny Easing where
  toAny = toAny . showEasing
  listToAny = listToAny . fmap showEasing

data AnimateOption = AnimateOption {
  duration :: Int,
  easing :: Easing,
  bqueue :: Bool,
  specialEasing :: Plain

  -- how to convert a normal function to JSString ?
  -- complete, step, progress
  -- start, done, fail, always
  }

defAnimateOption :: AnimateOption
defAnimateOption = AnimateOption 400 Swing True ""

-- ?
-- animation *With functions work right ?
fromAnimateOption :: AnimateOption -> JSAny
fromAnimateOption anim =
  toObject [("duration", toAny $ duration anim),
            ("easing", toAny $ easing anim),
            ("queue", toAny $ bqueue anim),
            ("specialEasing", toAny $ specialEasing anim)]

animate :: Plain -> JQuery -> IO JQuery
animate p (JQuery q) = JQuery <$> chain "animate" p q

animateOption :: Plain -> Int -> Easing -> IO () -> JQuery -> IO JQuery
animateOption p d e func (JQuery q) = JQuery <$> chain "animate" p d (showEasing e) func q

animateWith :: Plain -> AnimateOption -> JQuery -> IO JQuery
animateWith p opt (JQuery q) = JQuery <$> chain "animate" p (fromAnimateOption opt) q

type family FuncType (as :: [*]) where
  FuncType '[] = JQuery -> IO JQuery
  FuncType (a ': as) = a -> FuncType as

data Name (s :: Symbol) = Name

class AnimateOptionFunc (s :: Symbol) where
  type Arg s :: [*]
  option :: (KnownSymbol s) => Name s -> FuncType (Arg s)

  optionSimple :: (KnownSymbol s) => Name s -> JQuery -> IO JQuery
  optionSimple n (JQuery q) = JQuery <$> chain (symbolVal n) q

  optionWith :: (KnownSymbol s) => Name s -> AnimateOption -> JQuery -> IO JQuery
  optionWith n anim (JQuery q) = JQuery <$> chain (symbolVal n) q

instance AnimateOptionFunc "fadeIn" where
  type Arg "fadeIn" = [Int, IO ()]
  option n x1 x2 (JQuery q) = JQuery <$> chain (symbolVal n) x1 x2 q

fadeIn :: JQuery -> IO JQuery
fadeIn = optionSimple (Name :: Name "fadeIn")

fadeInOption :: Int -> IO () -> JQuery -> IO JQuery
fadeInOption = option (Name :: Name "fadeIn")

fadeInWith :: AnimateOption -> JQuery -> IO JQuery
fadeInWith = optionWith (Name :: Name "fadeIn")

instance AnimateOptionFunc "fadeOut" where
  type Arg "fadeOut" = [Int, IO ()]
  option n x1 x2 (JQuery q) = JQuery <$> chain (symbolVal n) x1 x2 q

fadeOut :: JQuery -> IO JQuery
fadeOut = optionSimple (Name :: Name "fadeOut")

fadeOutOption :: Int -> IO () -> JQuery -> IO JQuery
fadeOutOption = option (Name :: Name "fadeOut")

fadeOutWith :: AnimateOption -> JQuery -> IO JQuery
fadeOutWith = optionWith (Name :: Name "fadeOut")

fadeTo :: Int -> Int -> JQuery -> IO JQuery
fadeTo d o (JQuery q) = JQuery <$> chain "fadeTo" d o q

instance AnimateOptionFunc "fadeToggle" where
  type Arg "fadeToggle" = [Int, Easing, IO ()]
  option n d e func (JQuery q) = JQuery <$> chain (symbolVal n) d (showEasing e) func q

fadeToggle :: JQuery -> IO JQuery
fadeToggle = optionSimple (Name :: Name "fadeToggle")

fadeToggleOption :: Int -> Easing -> IO () -> JQuery -> IO JQuery
fadeToggleOption = option (Name :: Name "fadeToggle")

fadeToggleWith :: AnimateOption -> JQuery -> IO JQuery
fadeToggleWith = optionWith (Name :: Name "fadeToggle")

instance AnimateOptionFunc "slideDown" where
  type Arg "slideDown" = [Int, IO ()]
  option n d func (JQuery q) = JQuery <$> chain (symbolVal n) d func q

slideDown :: JQuery -> IO JQuery
slideDown = optionSimple (Name :: Name "slideDown")

slideDownOption :: Int -> IO () -> JQuery -> IO JQuery
slideDownOption = option (Name :: Name "slideDown")

slideDownWith :: AnimateOption -> JQuery -> IO JQuery
slideDownWith = optionWith (Name :: Name "slideDown")

instance AnimateOptionFunc "slideToggle" where
  type Arg "slideToggle" = [Int, IO ()]
  option n d func (JQuery q) = JQuery <$> chain (symbolVal n) d func q

slideToggle :: JQuery -> IO JQuery
slideToggle = optionSimple (Name :: Name "slideToggle")

slideToggleOption :: Int -> IO () -> JQuery -> IO JQuery
slideToggleOption = option (Name :: Name "slideToggle")

slideToggleWith :: AnimateOption -> JQuery -> IO JQuery
slideToggleWith = optionWith (Name :: Name "slideToggle")

instance AnimateOptionFunc "slideUp" where
  type Arg "slideUp" = [Int, IO ()]
  option n d func (JQuery q) = JQuery <$> chain (symbolVal n) d func q

slideUp :: JQuery -> IO JQuery
slideUp = optionSimple (Name :: Name "slideUp")

slideUpOption :: Int -> IO () -> JQuery -> IO JQuery
slideUpOption = option (Name :: Name "slideUp")

slideUpWith :: AnimateOption -> JQuery -> IO JQuery
slideUpWith = optionWith (Name :: Name "slideUp")

instance AnimateOptionFunc "jshow" where
  type Arg "jshow" = [Int, IO ()]
  option n d func (JQuery q) = JQuery <$> chain (symbolVal n) d func q

jshow :: JQuery -> IO JQuery
jshow = optionSimple (Name :: Name "jshow")

jshowOption :: Int -> IO () -> JQuery -> IO JQuery
jshowOption = option (Name :: Name "jshow")

jshowWith :: AnimateOption -> JQuery -> IO JQuery
jshowWith = optionWith (Name :: Name "jshow")

instance AnimateOptionFunc "jhide" where
  type Arg "jhide" = [Int, IO ()]
  option n d func (JQuery q) = JQuery <$> chain (symbolVal n) d func q

jhide :: JQuery -> IO JQuery
jhide = optionSimple (Name :: Name "jhide")

jhideOption :: Int -> IO () -> JQuery -> IO JQuery
jhideOption = option (Name :: Name "jhide")

jhideWith :: AnimateOption -> JQuery -> IO JQuery
jhideWith = optionWith (Name :: Name "jhide")

instance AnimateOptionFunc "toggle" where
  type Arg "toggle" = [Int, IO ()]
  option n d func (JQuery q) = JQuery <$> chain (symbolVal n) d func q

toggle :: JQuery -> IO JQuery
toggle = optionSimple (Name :: Name "toggle")

toggleOption :: Int -> IO () -> JQuery -> IO JQuery
toggleOption = option (Name :: Name "toggle")

toggleWith :: AnimateOption -> JQuery -> IO JQuery
toggleWith = optionWith (Name :: Name "toggle")


clearQueue :: JQuery -> IO JQuery
clearQueue (JQuery q) = JQuery <$> chain "clearQueue" q

delay :: Int -> JQuery -> IO JQuery
delay n (JQuery q) = JQuery <$> chain "delay" n q

queue :: JQuery -> IO JQuery
queue (JQuery q) = JQuery <$> chain "queue" q

dequeue :: JQuery -> IO JQuery
dequeue (JQuery q) = JQuery <$> chain "dequeue" q

finish :: JQuery -> IO JQuery
finish (JQuery q) = JQuery <$> chain "finish" q

fxInterval :: IO Int
fxInterval = ffi "(function(){ return jQuery.fx.interval; })" $ ()

fxOff :: IO Bool
fxOff = ffi "(function(){ return jQuery.fx.off; })" $ ()

stop :: JQuery -> IO JQuery
stop (JQuery q) = JQuery <$> chain "stop" q

-- Ajax

-- ?
ajaxComplete :: IO () -> JQuery -> IO JQuery
ajaxComplete func (JQuery q) = JQuery <$> chain "ajaxComplete" func q

ajaxError :: IO () -> JQuery -> IO JQuery
ajaxError func (JQuery q) = JQuery <$> chain "ajaxError" func q

ajaxSend :: IO () -> JQuery -> IO JQuery
ajaxSend func (JQuery q) = JQuery <$> chain "ajaxSend" func q

ajaxStart :: IO () -> JQuery -> IO JQuery
ajaxStart func (JQuery q) = JQuery <$> chain "ajaxStart" func q

ajaxStop :: IO () -> JQuery -> IO JQuery
ajaxStop func (JQuery q) = JQuery <$> chain "ajaxStop" func q

ajaxSucess :: IO () -> JQuery -> IO JQuery
ajaxSucess func (JQuery q) = JQuery <$> chain "ajaxSucess" func q

param :: Plain -> IO String
param p = ffi "(function(p){ return jQuery.param(p); })" $ p

serialize :: JQuery -> IO String
serialize (JQuery q) = chain "serialize" q

serializeArray :: JQuery -> IO String
serializeArray (JQuery q) = chain "serializeArray" q

-- ajax option ?

-- type jqXHR
ajax :: URL -> Plain -> IO JSAny
ajax = ffi "(function(url,p){ return jQuery.ajax(url,p); })"

-- jquery.ajaxPrefilter
-- jquery.ajaxTransport

ajaxSetup :: Plain -> IO JSAny
ajaxSetup = ffi "(function(p){ return jQuery.ajaxSetup(p); })"

ajaxGet :: URL -> IO JSAny
ajaxGet = ffi "(function(url){ return jQuery.get(url); })"

ajaxGetJSON :: URL -> IO JSAny
ajaxGetJSON = ffi "(function(url){ return jQuery.getJSON(url); })"

ajaxGetScript :: URL -> IO JSAny
ajaxGetScript = ffi "(function(url){ return jQuery.getScript(url); })"

ajaxPost :: URL -> IO JSAny
ajaxPost = ffi "(function(url){ return jQuery.post(url); })"

load :: URL -> JQuery -> IO JQuery
load url (JQuery q) = JQuery <$> chain "load" url q

-- Utilities

contains :: Elem -> Elem -> IO Bool
contains = ffi "(function(e1,e2){ return jQuery.contains(e1,e2); })"

jdata :: Elem -> String -> JSAny -> IO JSAny
jdata = ffi "(function(e,k,v){ return jQuery.data(e,k,v); })"

extend :: JSAny -> IO JSAny
extend = ffi "(function(t){ return jQuery.extend(t); })"

fnextend :: JSAny -> IO JSAny
fnextend = ffi "(function(t){ return jQuery.fn.extend(t); })"

-- globalEval, grep
-- inArray, isArray

isEmptyObject :: JSAny -> IO Bool
isEmptyObject = ffi "(function(t){ return jQuery.isEmptyObject(t); })"

isFunction :: Plain -> IO Bool
isFunction = ffi "(function(t){ return jQuery.isFunction(t); })"

isNumeric :: Plain -> IO Bool
isNumeric = ffi "(function(t){ return jQuery.isNumeric(t); })"

isPlainObject :: Plain -> IO Bool
isPlainObject = ffi "(function(t){ return jQuery.isPlainObject(t); })"

isWindow :: Plain -> IO Bool
isWindow = ffi "(function(t){ return jQuery.isWindow(t); })"

isXMLDoc :: Elem -> IO Bool
isXMLDoc = ffi "(function(t){ return jQuery.isXMLDoc(t); })"

-- makeArray, merge

noop :: IO ()
noop = ffi "(function(){ return jQuery.noop(); })" $ ()

now :: IO Int
now = ffi "(function(){ return jQuery.now(); })" $ ()

-- parseHTML, parseJSON, parseXML, proxy

removeData :: Elem -> IO JQuery
removeData e = JQuery <$> (ffi "(function(e){ return jQuery.removeData(e); })" e)

trim :: String -> IO String
trim = ffi "(function(e){ return jQuery.trim(e); })"

jtype :: JSAny -> IO String
jtype = ffi "(function(a){ return jQuery.type(a); })"

-- unique


