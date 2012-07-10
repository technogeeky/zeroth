#!/usr/bin/env runhaskell -isrc
> import Distribution.ZeroTH
> main = zeroTHCabalMain (Just ["Data.Derive"]) ["--hashes"] ["Language/Haskell/TH/ZeroTH/GetOpt.hs"]

