# Testing

Testing is part of production-ready software development. Because the goal of the present project is to learn how to use Haskell in production, it makes sense to also learn how to perform testing in Haskell. Yet, because _testing_ is very broad, we first need to agree on our testing objects (why?), the entities under test (what?), and the tools we use and processes we follow (how?).

When discussing testing, we refer to automated testing _only_. That is, all test cases are themselves programs that execute a piece of our production software. While manual testing does make sense for complex applications in production, the techniques used are not specific to Haskell and, therefore, not of interested for a project aimed at learning how to write a production application in Haskell.

## How Much Testing is Sensible in a Haskell Application

It is common knowledge that testing cannot proof the correctness of software, nor the absence of bugs. Nevertheless, testing has proved sensible to find and fix bugs over and over again - because most of the time, we cannot proof a program to be correct, or the moethods for doing so are not widely used in practice. For typical OO enterprise software development, the combination of testing and code review is thought to be most effective in finding bugs (according to [Code Complete](https://www.oreilly.com/library/view/code-complete-second/0735619670/)).

Haskell's type system allows us to encode information into types that the compiler can check. The type checker is in fact a weak form of proof assistant. Haskell's compile-time guarantees are much stronger than what get in other languages. Therefore, on often hears the statement about Haskell code that it is hard to get it type-check, but once the code compiles, it is correct. Unfortunately, this is not true - even though the type-checked code probably contains way fewer bugs than a comparable type-checked Java or C# code.

Clearly, because of the type-safety of Haskell, we do not need to write those types of tests that would be necessary for dynamically typed languages like JavaScript or Ruby; but this is true for other statically-typed languages as well. In addition, referential transparency rules out an entire class of bugs that result from unexpected mutation of state. Thus, can we dispose of more tests, or even forego testing alltogether?

I don't think so, for the following reasons:

- The type-checker can only check for those properties that we encode in the types. Thus, we can trade off the effort for designing and using specific types vs. the effort in writing and maintaining tests. For example, we can design our type for the content of an article to hold characters from a specific character set only. Then, we do not need to test how our code handles Unicode points outside this character set. What we should test is that the contructor indeed does guarantee this invariant; but we do not need to test all the code that handles the type for illegal characters: we made them unrepresentable.
- We cannot have arbitrarily complex invariants encoded in our types. Haskell's type system is expressive. But if we want to express very elaborate constraints in a type, it might come at a cost we are not willing to spent. For example, there are [libraries](https://hackage.haskell.org/package/refined) for [refinement types](https://en.wikipedia.org/wiki/Refinement_type); e.g., to limit the range of a numeric value. But the libraries are experimental and hard to use. Or we could go for [dependent types](https://serokell.io/blog/why-dependent-haskell), which would even let us express the ordering of a list as a compile-time property. Yet, corresponding Haskell [extensions](https://www.cis.upenn.edu/~sweirich/papers/icfp19.pdf) and [libraries](https://wiki.haskell.org/Dependent_type) are still experimental and hard to use, too. Thus, for our application at hand, there will be many properties that we cannot encode in the types. Which of these properties should be coverdd by tests instead is a question about the risk associated with the business logic that uses a specific type - what is probability to get it wrong, and what are the posible consequences of a bug?
- Type checks are compile-time checks. Yet, the algorithms we write are executed at run-time. Even if we work hard to only feed them proper data constrained by specific types, the algorithms might be wrong in my subtle ways. Thus, we should test them.
- Even though referential transparency prevents accidental mutation of state, the processing chaing of complex data structures that stretches across multiple architectural layers and invokes many library functions get complex quickly. Tests help us to be more confident that these processing chains perform the way we want them to.
- Finally, complex requirements are most often not complete: Typically, not all valid combinations of input and output are specified up front. Good tests help us to find the gaps in the specification and to fill them before the program goes into production.

To conclude: Testing does make sense also for Haskell applications. We should focus on those properties of our types that we do not want to encode into the types themselves, and on corner cases of our business logic.

## Why Test

Testing cannot proof the absence of bugs, but it shows the presence of bugs. Testing thus serves three primary purposes:

1. To find bugs.
2. To determine if we have sucessfully removed a previously-discovered bug.
3. To ensure that we do not re-introduce a previously-discovered bug.

Different types of testing thus serve to discover different types of bugs.

### Learning How to Test

The whole purpose of the present project is to learn how to write a production-ready application in Haskell. Thus, the main objective in testing here actually is to learn how to test: How to write tests, how to execute them, how to handle the tools. But also to learn about the merits and drawbacks of different types of tests, so that we can form an undertanding of when to use which type of testing strategy, how much effort it is to implement and adapt, and what the benefits are.

### Understanding the Specification: Explorative Testing

A large class of bugs results because we have not completely specified the problem initially. Thus, we can write tests to see what our code does for specific corner cases, and to understand how we need to improve our specification. There are two ways to perform explorative testing:

1. Playing around with code, create some input and analyze the results; e.g., to understand how some external library works. This typically is a manual endevor.
1. Create all sorts of inputs to catch corner cases that might not have been given consideration. This often can be automated via random test data generation.

For the project at hand, where the functionality and the interfece of the application is specified, explorative testing of the first kind does not seem to be necessary on the ResT API; yet, because a web application is confronted with all sorts of unexpected user input, randomized testing does make sense - both at the interface and internally. Exploring third-party libraries will be an important part of our learning experience; therefore, explorative manual testing is something we need to support.

### Ensure the Specified Functionality Works: Test-Driven Development

Precisely specifying the behavior of an application is a question of risk. Which functionality is essential? Which risks am I willing to run in exchange for development time and effort? Test-driven development starts by encoding the essnetial specification as tests, which will fail initially because there is no implementation. Then, wen start writing the implementation until all tests pass. This is the point to stop implementing and move on, because essential functionality is in place. For the present project, it test-driven development might be a good choice for the ReST API layer, where the sufficient specification is available. In general, test-driven development in Haskell seems to be beneficial, because the language reduces the risk that there are lots of additional bugs in the code once the specification is implemented - the more so the more we encode properties in our domain types.

### Documenting the Implementation

Tests execute a specific part of the code; therefore, they demonstrate how certain functionality is to be used - they are executable examples, both for external users of some library as well as internally for other developers on the team. As opposed to documentation, tests never get out of sync with the overall codebase if a CI pipeline in place that executes all tests regularly. In the present project, the aspect of tests as documentation are not that important on the API, because we follow a given specification. However, for functions and types used internally only, tests should be our primary means of code-level documentation.

Tests do not replace high-level documentation that explains design decisions, it only shows how functions and types are used.

## What to Test: Test Hierarchy

The subject under test can be as small as a one-line function and as large as the entire application. Similarly to how we manage complexity by subdividing the application into modules and functions that can be analyzed and developed indepentently, our tests should also be broken down to match the structure of the application.

For a production application, at which layers to test and how much to test is a risk-driven business decision. In our project here, though, our goal is to learn how to write an application, part of which is writing tests. Therefore, we consider the standard layers inside out, but we do not strive for maximum coverage but maximum understanding.

### Unit Testing

Unit testing in Haskell is similar to unit testing in most other programming languages: We take some isolated function and corresponding types, write a test program that executes the function with specific inputs and compares the output to expected results. Thus, unit testing does not run the actual application, but it runs a test program that contains just the unit under test.

Because writing little test programs for lots of units and executing them is tedious, there are unit testing frameworks that take care of execution and error reporting, very similar to comparable frameworks in other programming languages.

To learn how to efficiently unit-test our Haskell application, we need to set up a unit-testing framework and use it on all types of units, from very small to very large. We also need to deal with different kinds of test data (created internally, loaded from a file, etc.) to get a feel for the effort involved. Ideally, we manage to use and compare several unit testing frameworks and determine their strengths and weaknesses. However, our primary goal is to get one unit testing setup working as part of CI-pipeline, as would be the case with a regular production development project.

### IO Testing

Haskell has a clear separation between pure code and effectful code in the IO monad. This inherent structure of Haskell programs can be used to create an architecture that pushes all IO to the boundary of the application - called [Ports and Adapters](https://blog.ploeh.dk/2016/03/18/functional-architecture-is-ports-and-adapters/) or Onion Architecture. While the inner rings are perfectly suited to unit testing because all code is referentially tansparent, the IO ring must be handled differently.

First, I assume that our architecture will separate IO into encapsulated modules that separate the different IO concerns: Network IO via HTTP, Database IO, and terminal IO for configuration and logging. Ideally, we can test these different IO modules independently. The main problem to take into account here are the external dependencies: Can we replace them by mocks? How?

### Integration Testing

While a unit test effectively is a test program wrapped around some unit of functionality, the subject under test in integration testing is the application proper - at least, the minimum part of the application that can be executed independently. While a unit test requires a single test program to run, integration testing requires the actual application plus a separate test program that attaches to the external interfaces of the application under test.

For the application at hand, we will probably write a monolithic program that offers the specified ReST interface. Therefore, integration testing will exercise this ReST interface via some external test runner. Because nothing about the ReST inteface is specific to a Haskell application, we might use any integration testing tool. It will be part of the learning experience to determine which tools are indeed suitable, and if a Haskell tool might still be the preferred choice to instrument and monitor the application under test.

### Special-Purpose Testing

Besides the above-listed general purpose testing hierarchy, there exist more specialized types of tests; for example:

- performance testing
- fault handling and recovery testing
- penetration testing

Keeping with the prupose of the present project, these are only of secondary concern. We might look at performance testing once we feel that all other learning objectives are met.

## Unit-Test Tooling and Testing Styles

### Property Testing

Property testing means to specify a property that should hold for all inputs to a given function, and then test that the propery does indeed hold for a wide array of randomly generated test data. Thus, instead of manually specifying a small set of corner cases, Property testing focuses on the invariants that should hold and lets the testing library take care of creating many test cases.

Coming from an OO background, it takes a while to understand how property testing works, and how to implement generators to randomly create test data for custom types. The original property testing library is [QuickCheck](https://hackage.haskell.org/package/QuickCheck). Here are some helpful resources to understand how QuickCheck works, how to write properties and generators:

- [The Design and Use of QuickCheck](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html): A very good general-purpose introduction that also shows advanced techniques to control test data and to efficiently shrink tests.
- [QuickCheck Generators Tutorial](https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators): Explains how to implement QuichCheck generators for custom types.
- [QuickCheck and Magic of Testing](https://www.fpcomplete.com/blog/2017/01/quickcheck/): Another good introduction to writing QuickCheck properties and Generators.

Over time, several other libraries have emerged to make up for some shortcomings of QuickCheck:

- [SmartCheck](https://github.com/leepike/SmartCheck): Uses QuickCheck properties but claims to perform smarter shrinking.
- [SmallCheck](https://github.com/Bodigrim/smallcheck): For types that are inhabited by a small, finite set of values, randomly drawing samples does not make too much sense. Instead, SmallCheck exhaustively tests for all possible values of a given type. [Here](https://ro-che.info/articles/2018-05-25-quickcheck-vs-smallcheck) is a comparison between QuickCheck and SmallCheck.
- [LeanCheck](https://hackage.haskell.org/package/leancheck): Even more specialized property testing for enumerated types.
- [HedgeHog](https://hedgehog.qa): A new take on property testing - supposed to be the newer and better QuickCheck. Hedgehog comes with special emphasis on testing state machines, and it automatically performs shrinking. See also this [blog post](https://teh.id.au/posts/2017/04/23/property-testing-with-hedgehog/) and this [blog post](https://www.fpcomplete.com/blog/quickcheck-hedgehog-validity) about HedgeHog. [This article](https://www.well-typed.com/blog/2019/05/integrated-shrinking) compares shrinking in QuickCheck and in HedgeHog.

In general, property testing seems to be the way to go in Haskell, at least for pure code, and as long as custom types do not require a large effort to come up with sensible generators.

### Behavioral Testing

### Unit Test Runner

The job of a unit test runner is to execute unit tests, collect their results and report them. From the way how unit tests work, the test runner is an application framework - an application with its own `main` function that sets up the test environment and then calls the actual test cases supplied by the developers. The test cases get compiled into the test-runner application each time anew when the tests are executed.

There exist several Haskell unit test runners to choose from. The most popular seem to be

- [Test Framework](https://hackage.haskell.org/package/test-framework): Probably the oldest test runner of the pack. It's selling point is that it runs many tests in parallel. [Documentation](http://haskell.github.io/test-framework/) is sparse. Test Framework relies on _providers_ for sepcific types of test cases. This can be conventional unit tests (via HUnit below), or QuickCheck property tests.
- [HUnit](https://hackage.haskell.org/package/HUnit): Closely follows the original JUnit framework. Comes with utilities to write assertions, to control how test cases are executed, and how test results are reported.
- [HUnit Plus](https://hackage.haskell.org/package/HUnit-Plus): Builds on HUnit but provides additional assertions and ties in with the cabal test suite. It can write XML test reports that are compatible with JUnit test reports.
- [HSpec](https://hspec.github.io): Provides a domain-specific language (DSL) for writing human-friendly test specifications, similar to other specification-based testing frameworks for other programing languages. It is interoperable with HUnit for conventional unit testing, and QuickCheck for property testing. In addition, it comes with extensions that simplify writing test cases for specific types of functions, such as lenses or parsers. It can also be used as an integration testing framework for use with Webdriver.
- [tasty](https://hackage.haskell.org/package/tasty): Testing framework that can handle differnt testing styles. It allows to write unit tests with HUnit, property tests with QuickCheck and SmallCheck, and it can create _golden tests_ that are exactly reproducible, for acceptance testing. It comes with support for handling IO resources, parallelization of tests, and a way to structure complex test suites.

As can be seen from the description, some of the above frameworks are more than test runners; they provide a DSL for writing test cases, and come with assertion libraries. The basic theme is that most frameworks support both conventional test cases as well as properties, and therefore rely on HUnit and QuickCheck under the hood.

From some forum research, _tasty_ seems to be the most popular test runner currently. It also provides the most flexibility for running different types of tests. Therefore, I propose to use tasty to get started. We can later on decide to add another test runner for the purpose of comparison.

## Testing Policy

### Writing Unit Test Cases

The following is my _proposal_ how to go about unit testing in the project:

- Each nontrivial function must have at least one unit test. Nontrivial means that the function works on a complex data type, has nonlinear control flow (e.g., branching), or combines other functions. If the function handles both positive and negative cases, there must be a test for a negative case in addition to the happy path.
- Each nontrivial type with smart contructor must have at least two unit tests, for both the happy path and for a failed input validation.
- Where possible, prefer randomized property testing over individual test cases.
  
### Test-Driven Bug-Fixing

In case we find a bug:

1. Write a test case that reproduces the bug; i.e., the test case must fail.
1. Fix the bug.
1. Make sure the test case passes.
