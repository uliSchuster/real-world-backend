# A Commented List of Reference Material

## Haskell Basics

### Haskell Web Resources

- The [Haskell Wiki](https://wiki.haskell.org/Haskell). Official web presence for the Haskell language.
- [What I Whish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/) Stephen Diehl provides excellent coverage of all aspects of modern haskell, from the basics to advanced type-level programming and GHC internals.
- [Monday Morning Haskell](https://mmhaskell.com). Introductory Haskell tutorials and blog posts.
- [Haskell Programming Guidelines](https://wiki.haskell.org/Programming_guidelines) from the Haskell Wiki.
- [Haskell Programming Tips](https://wiki.haskell.org/Haskell_programming_tips#Don.27t_use_Int_when_you_don.27t_consider_integers), also from the Haskell Wiki.
- Kowainik's opinionated [Haskell Styel Guide](https://kowainik.github.io/posts/2019-02-06-style-guide).
- O.Charles [Haskell Blog](https://ocharles.org.uk). Good exposition of advanced Haskell topics, language extensions, and key libraries.
- The [Haskell Section](https://github.com/caiorss/Functional-Programming/tree/master/haskell) from the _FP by Example_ github repository.

### Haskell Blogs

- The [FP Complete](https://www.fpcomplete.com) [Haskell Blog](https://www.fpcomplete.com/tags/haskell/). The blog from the compay behind Stack, Yesod, Persistent and RIO.
- The [Tweag Haskell Blog Posts](https://www.tweag.io/blog/tags/haskell) from a company that specializes in Haskell.
- [Advanced Haskell Blog](http://okmij.org/ftp/Haskell/)
- [Haskell for All](http://www.haskellforall.com). G. Gonzales' blog about all things Haskell. Good tutorials and though-provoking posts, but hard to browse.
- [Mark Karpov's Haskell Blog](https://markkarpov.com/tag/haskell.html).
- [William Yao's Haskell Blog](https://www.williamyaoh.com/archive.html)
- [Alexis King's Haskell Blog](https://lexi-lambda.github.io)

### Books about Haskell

- C. Allen, J. Moronuki: [Haskell Programming from first principles](https://haskellbook.com), aka _The Haskell Book_.
- V. Bragilevsky: [Haskell in Depth](https://www.manning.com/books/haskell-in-depth?query=Haskell). Covers intermediate and advanced topics in more detail.
- A. Serrano: [Practical Haskell](https://www.apress.com/gp/book/9781484244791). Covers many topics of relevance for serious Haskell applications
- B. O'Sullivan, D. Stewart, J. Goerzen: [Real World Haskell](http://book.realworldhaskell.org). A Book on using Haskell for commercial applications. Somewhat dated (2008).
- G. Hutton: [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html). Concise introduction to the core language.
- S. Thompson: [Haskell - the Craft of Functional Programming](http://www.haskellcraft.com/craft3e/Home.html).

### Monads and Monad Transformers

- [You Could Have Invented Monads! (And Maybe You Already Have.)](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html). In my opinion, the best introduction to functional state handling, and the monad abstraction that follows from it.
- [Monadic examples for exception handling and logging](https://www.williamyaoh.com/posts/2019-10-12-how-to-basic-error-handling-logging.html).
- [Concise introduction to the state monad](https://www.quora.com/What-is-a-state-monad). This simple description finally helped me understand how functions can be monadic structure, and what is meant by a _monadic action_: They key is to use currying and write `(a, s) -> (b, s)` as `a -> (s -> (b, s))`, which is a function that returns a state action.
- [Purpose of the Reader monad](https://stackoverflow.com/questions/12968351/monad-transformers-vs-passing-parameters-to-functions), nicely explained in a StackOverflow answer.
- [A Gentle Introduction to Monad Transformers](https://two-wrongs.com/a-gentle-introduction-to-monad-transformers).

### Exception and Error Handling

To get started, we need to understand the [distinction between errors and exceptions](https://wiki.haskell.org/Error_vs._Exception) as described in the Haskell Wiki.
There are many different ways to handle exceptions in Haskell. Which one to pick, or how to mix the approaches is a subject of lively debate.

- FPComplete's [opinionated view on error handling](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/) and [safe exception handling](https://www.fpcomplete.com/haskell/tutorial/exceptions/): `MonadThrow`. Throwing exceptions is not a bad thing as long as you are atop IO, because you need to handle all sorts of exceptions anyways.
- The [blog post](https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell/) from Tweag is a summary along the same lines, with a difference to not handle _imprecise_ exceptions.
- An outdated but still instructional [overview of handling errors](http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/), and an [updated overview](http://blog.ezyang.com/2011/08/8-ways-to-report-errors-in-haskell-revisited/).
- [The trouble with typed errors](https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html) examines the problem of building up typed error hierarchies, which mess with the concept of independent software modules.

### Testing

- [How to specify it!](https://www.youtube.com/watch?v=G0NUOst-53U) Keynote talk by John Hughes (one of the developers of QuickCheck) at lambda days 2020. He talks about how to write properties of pure functions.

## Haskell Libraries

### General Purpose Libraries

### Web Frameworks

- The [Yesod Web Framework](https://www.yesodweb.com). A tried and tested web application framework for Haskell. Provides for server-side rendered web applications, persistence, authentication, etc. To my knowledge, the most widely-used Haskell web framework.
- M. Snoyman [Developing Web Apps with Haskell and Yesod](http://shop.oreilly.com/product/0636920035664.do). Written by the framework's author.
- The [Scotty Web Framework](https://github.com/scotty-web/scotty). A lightweight web framework, inspired by [Sinatra](http://sinatrarb.com).
- [Spock](https://www.spock.li) web application framework. "Latest News" from 2016, though.
- [Servant](https://www.servant.dev). Client and server libraries for web APIs in Haskell.
- The Integrated Haskell Platform [IHP](https://ihp.digitallyinduced.com) is probably the youngest of the frameworks listed here. It is a full-fledged framework for server-rendered web applications, currently in beta status.
- [Snap Web Framework](http://snapframework.com). An early Haskell web framework. Not much activity recently.

### Persistence

- [Opinionated comparison of persistence libraries for Haskell](https://williamyaoh.com/posts/2019-12-14-typesafe-db-libraries.html) by W. Yao
- [Persistent](https://www.yesodweb.com/book/persistent), the persistence framework for a variety of relational and non-relational databases that powers Yesod.
- [Esqueleto](https://github.com/bitemyapp/esqueleto), and embedded type-safe DSL for expressing SQL queries within Haskell. A library that works atop Persistent.
- [School of Haskell Tutorial](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/persistent-db) on Esqueleto and Persistent.
- [Tutorial on Esqueleto and Persisten](https://ocharles.org.uk/posts/2013-12-06-24-days-of-hackage-persistent-esqueleto.html).
- [Opaleye](https://github.com/tomjaguarpaw/haskell-opaleye). Type-safe embedded DSL to interact with PostgreSQL.
- [Groundhog](https://www.schoolofhaskell.com/user/lykahb/groundhog). Relational DB mapping and querying.

## Application Architecture

Software architecture is about design in the large. It strives to attain the conflicting goals of functionality and performance while managing complexity. Software complexity is sharply constrained by our limited intellectual capacity. Therefore, one can argue that functionality and performance cannot be attained without proper management of complexity.

### Clean Architecture

To limit complexity, it is essential to properly organize and minimize dependencies. This is the goal the architecture principles know as _Clean Architecture_, _Onion Architecture_, _Hexagonal Architecture_, or _Ports and Adapters_.

- Bob Martin's [introductory blog post](https://blog.cleancoder.com/uncle-bob/2012/08/13/the-clean-architecture.html) on Clean Architecture.
- R.C. Martin [Clean Architecture](https://www.oreilly.com/library/view/clean-architecture-a/9780134494272/). Detailed description of Clean Architecture principles. Good read, but lacking examples at the source-code level.
- A [summary](https://pusher.com/tutorials/clean-architecture-introduction) of the book, plus some examples.
- [Functional Architecture is Ports and Adapters](https://blog.ploeh.dk/2016/03/18/functional-architecture-is-ports-and-adapters/). Blog post by Mark Seeman on how to architect a functional programm to minimize dependencies and push impure code to the outer rings. With F# and Haskell examples.

[Numbers every Programmer Should Know](https://colin-scott.github.io/personal_website/research/interactive_latency.html). Before attempting any sort of performance optimization, make sure you understand the performance impact of the various layers of I/O.

### Haskell Application Architecture

- [Three Layer Haskell Cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html). Architecture proposal by P. Matt. It's an implementation of Clean Architecture principles, with a pure domain core, and adapters to external systems in the outermost layer.
- [Invert Your Mocks](https://www.parsonsmatt.org/2017/07/27/inverted_mocking.html). Mocking external systems for efficient testing of the aplication core.
- [The ReaderT Desidn Pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/). Minimal Transformer stack (ReaderT and IO) advocated by FPComplete. See also their posts on the [RIO Monad](https://www.fpcomplete.com/blog/2017/07/the-rio-monad/) and the [RIO Standard Library](https://www.fpcomplete.com/haskell/library/rio/.)
- An [StackOverflow] question about modular application architecture. The answers explain why stacking multiple Reader monads might be problematic, and explain the ReaderT design pattern as a solution for large applications.
- An [example](https://gvolpe.github.io/blog/lessons-learned-while-writing-a-haskell-app/) about a web application that uses `dhall` configuration, the ReaderT design pattern plus polymprphic function records.

### Best Practices

Here are some articles, blog posts, and videos that provide guidance on how to write Haskell applications in the large:

- [Parse, don't validate!](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) gives deep insight into type-driven design. Key guideline: _Make illegal states unrepresentable_.
- In the same vein, here are more examples on [pushing type failure to the application boundary](https://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html).

## Tooling

### Haskell Build System

- [Stack](https://docs.haskellstack.org/en/stable/README/), the Haskell tool stack.

### Editors

- [Setting up Haskell in Visual Studio Code with Stack and the IDE Engine](https://medium.com/@dogwith1eye/setting-up-haskell-in-vs-code-with-stack-and-the-ide-engine-81d49eda3ecf). If you use VS Code, this seems to be the most up-to-date introduction how to set up a decent development environment with advanced editing support.
- The above setup uses [Haskell IDE-Engine](https://github.com/haskell/haskell-ide-engine) to integrate the editor with a ghci console to obtain advanced editor features like type annotations.

### Version Control

- The [git source code management system](https://git-scm.com). Home of git, and the ultimate git reference.
- This [simple introduction to git](https://rogerdudler.github.io/git-guide/) shows the most important commands.
- Understand [branching and merging](https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging).

### Debugging

## General FP Resources

### Books

- B. Milewski: [Category Theory for Programmers](https://github.com/hmemcpy/milewski-ctfp-pdf/).
- A. Serrano: [The Book of Monads](https://www.goodreads.com/book/show/42449863-the-book-of-monads). Covers a monads from the perspective of a working programmer. Examples mostly in Haskell, plus some Scala.
- S. Wlaschin: [Domain Modeling Made Functional](https://www.oreilly.com/library/view/domain-modeling-made/9781680505481/). Excellent example on how domain driven design works in combination with FP. Examples in F#.
- D. Ghosh: [Functional and Reactive Domain Modeling](https://www.manning.com/books/functional-and-reactive-domain-modeling).
- H. Abelson, G. J. Sussman, J. Sussman: [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/index.html). A classic M.I.T. introductory textbook on programming that set standards for explaining the essential concepts of programs, programming language design and data-driven abstraction. Heavily relies on the Scheme language.

### Papers

- J. Hughes: [Why Functional Programming Matters](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf) Good summary about the essence of FP.
- B. Moseley, P. Marks: [Out of the Tar Pit](http://curtclifton.net/papers/MoseleyMarks06a.pdf) Essential software architecture: reduce accidential complexity.

### Blogs and Websites

- [Bartosz Milewski's Programming Café](https://bartoszmilewski.com)
- [F# for Fun and Profit](https://fsharpforfunandprofit.com/). S. Wlaschin's excellent blog and video tutorials on FP. Examples are mostly in F#.
- [FP by Example](https://github.com/caiorss/Functional-Programming). FP concepts in many different programming languages.
