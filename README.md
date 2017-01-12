# xhb-mapping-state

This package provides a monad transformer over [MonadX](https://github.com/nickspinale/xhb-monad)
that keeps track of modifier, key, and button mappings within the X server.
At its core is the following typeclass:

```haskell
class Monad m => MappingCtx m where
    getMapping :: m MappingState
    updateMapping :: MappingNotifyEvent -> m ()
```

## Documentation

[This article](http://nickspinale.com/articles/xhb-monad) describes this package and some of its friends in detail.

Haddock can be found [here](https://nickspinale.github.io/xhb-mapping-state).
