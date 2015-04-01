# GHCi Safe

A custom version of GHCi that starts in Safe Haskell mode.

## Building

This is very fragile and a massive hack right now. It should build with
**7.8.4**.

You'll need to update the line:

```
defaultTopDir       = Just "/usr/lib/ghc-7.8.4/"
```

In `ghci-safe/Main.hs` to reflect where GHC is located.

##  Updating for GHC

When GHC changes, you'll need to copy across (with no changes) the files from
the `ghc/*` folder to `src`.

