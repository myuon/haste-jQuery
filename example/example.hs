import Haste
import Haste.JQuery as J

(<&>) = (>>=)

main = do
  j "p" <&> J.jlast <&> J.addClass "selected highlight"

