# Custom Prelude and Extensions

The Halsekk 2010 standard Prelude is widely considered to be inadequate for serious application development, because it comes with lots of historical baggage, unsafe partial functions and no longer sensible defaults. Therefore, we need to decide on replacements.

Haskell 2010 is sleek but also rather limited. GHC offers a zoo of language extensions that either simplify development or provide extra power. We need to decide which ones to use.

## Prelude

The [Prelude](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html) is part of the [Base Package](https://hackage.haskell.org/package/base-4.14.0.0). It contains lots of useful types and functions that are explained in every Haskell beginner course. But some functions and data structures are outdated, inefficient or outright [dangerous](https://www.microsoft.com/en-us/research/wp-content/uploads/2012/01/safe-haskell.pdf).

### Alternatives to the Standard Prelude

Several alternatives to the Standard Prelude are discussed [here](https://guide.aelve.com/haskell/alternative-preludes-zr69k1hc) and [here](http://dev.stephendiehl.com/hask/#prelude-1). The problem is that replacing the Standard Prelude easily breaks much existing code, so any alternative will be a compromise between purity and practicality.

For our application at hand, the prelude should provide the following:

- Replace the inefficient `String` type consistently with `Text`
- Remove unsafe functions.
Otherwise, an alternative Prelude should not reinvent the wheel but keep known functions and types that work well in general. Fixing the mathematical hierarchy is not a priority for our application. S. Diehl gives [discussion](https://www.stephendiehl.com/posts/protolude.html) on this approach.

### Prelude _Proposal_: RIO

I propose to use the [RIO library](https://www.fpcomplete.com/haskell/library/rio/) as replacement for the Standard Prelude. It satisfies the above requirements, imports additional types that are generally useful, but does not completely change the way how to write Haskell code. RIO is promoted by FPComplete, the company behind Stack and Yesod. There is a [video tutorial](https://www.youtube.com/watch?v=gu0ZCqQe3BY) available.

In addition to being a Prelude replacement, RIO also provides an opinionated approach to application architecture via a simple monad transformer stack, in the form of the [RIO Monad (Reader + IO)](https://www.fpcomplete.com/blog/2017/07/the-rio-monad/). Yet, using the RIO package does not imply that we must use the RIO stack.

To use an alternative prelude, we must prefix each source file with the language pragma [`{-# NoImplicitPrelude #-}`](https://typeclasses.com/ghc/no-implicit-prelude), to prevent GHC from loading it by default.

## Language Extensions

Using [extensions](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html) to the language standardized in the Haskell 2010 reports might seem like a bad idea - after all, standards are meant to unify development and allow for portability. However, changes that we will need to compile our code on anything but GHC are rather slim - there simply does not exist a competing compiler at the moment. Furthermore, we are not writing libraries for others to use, but an application. Therefore, we can freely pick those language extensions that make the most sense for our purposes.

### Details about Language Extensions

Many language extensions are explained [here](https://ocharles.org.uk/pages/2014-12-01-24-days-of-ghc-extensions.html) and [here](http://dev.stephendiehl.com/hask/#language-extensions), in addition to the official GHC documentation. Alexis King gives an [opinionated discussion](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/) of pros and cons of may extensions.

### Extension Policy _Prpopsal_ for the Present Project

- Make them explicit: For any given source file, list all the language extensions used at the top of the file, via `LANGUAGE` pragmas. Do not set up default extensions in Stack or Cabal configuration files or via the compiler command line.
- Only include those language extensions actually used in any given source file. Do not enable language extensions just in case.
- Do not use language extensions that alter the meaning of regular Haskell 2010 code. That is, do not use the following extensions:
  - [OverlappingInstances](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-OverlappingInstances)
  - [IncoherentInstances](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-IncoherentInstances)
