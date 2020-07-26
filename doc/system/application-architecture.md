# Application Architecture

Application architecture encompasses several aspects that all deal with organization and structure: Discover structure in the domain-level problem the application is supposed to solve, and organize data, functionality, and control flow to match that structure. For only structure allows for systematic reasoning about the problem and its solution.

Architecture predominantly targets humans, not machines. Because our intellectual capacity is limited, architecture helps us to manage the complexity of the software without getting lost. A good architecture limits the number of things a developer needs to be concerned with at any one time. Therefore, architecture divides the problem in increasingly small subproblems that can be conquered one at a time. Each level of subdivision should have appropriate abstractions that hide the lower-level detail without leaking, and the dependencies between the individual subdivisions should be low. This is the architectural heuristic of strong cohesion within a subsystem and loose coupling between subsystems.

In the following, we devise the architecture for the Conduit bloggig server and provide the rationale and references for our architecture decisions.

## Model of Computation

The conduit application is a standard web application with a simple ReST API. Therefore, it works according to a request-response pattern: The application receives a request via its API, interprets the request, loads the required data from its persistence store, processes that data plus the data that was part of the request, stores all or part of the result, and returns all or part of the result back to the requester. Because the API follows the ReST pradigm, all resource requests are stateless - that is, there is not protocol state machine that describes the sequencing of requests; there is not interaction with the caller within one request. Requests can come in any order; the result of a given request is completely determined by the request itself and the data stored locally.

This model of computation lets us divide the appllication into individual services that can be called independently of each other. Consistency across concurrent calls is ensured by the database. This is the standard web application architecture. It might reach its limit under very high load, but it completely suffices for the problem at hand. Writing individual services simplifies testing, because each service can be tested independently.

## Clean Architecture

Even though the application consists of individual services, we have to take care of many _cross-cutting concerns_ like persistence, logging, error handling, and providing the API. We could split the application into a set of completely independent micro-applications that all do their own data access, logging, error handling, etc., while each application is structured as a [Transaction Script](https://martinfowler.com/eaaCatalog/transactionScript.html); this is similar to how the first CGI web applications worked.

However, there would be quite some overlap between these micro-applications, because many services rely on similar data and perform similar tasks. To not repeat ourselves, increase reuse, and provide structure across these micro-applications, it is sensible to collect common code and data structures in modules. The result is an architecture whre the API requests are received by a web server that routes them to dedicated business logic, which in turn relies on common modules for persistence, logging, and other cross-cutting concerns.

This architecture style comes at the expense of dependencies. For example, if the `display_article` service and the `display_user_profile` service rely on the same `User` domain type, a change in the domain library affects both services. If not managed properly, these dependencies quickly turn into an incomprehensible thicket that hinders joint development work and makes maintenance very difficult. Thus, properly scoping modules and managing their dependencies is a key aspect of application architecture.

Proper scoping of modules and dependency management is the core of the [Clean Architecture](https://blog.cleancoder.com/uncle-bob/2012/08/13/the-clean-architecture.html) approach.

### Modules

### Dependencies

The Clean Architecture _Dependency Rule_ says that dependencies are allowd to point inwards only: Modules that may change quickly and are less essential are allowed to depend on modules that are central to the core functionality of the application, but not the other way around. This implies that the application core is free of any dependencies on outer rings; the services/usecase ring may depend on the domain core but not on anything else. Adapters and Controllers that mediate access to external data sources and sinks may depend on both services and the domain core, but they should not directly depend on each other.

Rationale: The speed at which modules at different rings change is one justification for the dependency rule. For example, UI technology evolves very quickly, and even more stable system services receive constant updates; e.g., to fix security vulnerabilities. The second, more important argument compares essential with accidental dependencies: The core business logic of the application might also be extended and altered. If this happens, it will invariably lead to a change in other parts of the application; e.g., UI elements need to be added and fields inserted in the data store. This type of dependency is neccesary to provide the intended domain functionality, as opposed to a nonessential concern like change in UI technologz or a change in some JSON field a client application expects. The application architecture should follow the essential dependency, so that the application core does not need to change if requirements on the domain functionality don't change.

### Dependency Inversion

A dependency of module A on module B arises if module A uses some function or type declared in module B. Thus, the normal flow of dependencies follows the flow of control: Module A calls a function from module B. However, the typical control flow of a business application might start at the UI module in the outermost ring, then go through a service in the usecase module, back to fetch data via the persistence module in the adapter and gateway ring, back to the service, from there to a specific part of the domain logic in the core module and then back again through the service module to store data via the persistence model and to prepare it for display via the UI.

The think to observer here is that control flow does not always travel from the outermost ring to the center - it has to somehow come back to the surface for output. Thus, we need the dependency to counter the control flow, to _invert_ the dependency. In object-oriented languages, the standard tool for dependency inversion are interfaces and dependency injection: If module A calls module B, but the dependency should run in the other direction, A defines an interface that B implements. Thus, B imports the interface from A, but A does not need to know about the concrete implementation provided by B. The concrete instance is injected upon application launch by the outermost ring, where the application entry point resides - the `main()` function.

TODO: Explain dependency inversion in our Haskell code.


### Third-Party Libraries and Frameworks

- Library: A set of functions and types that are meant to be called by code that uses the library. Thus, code that uses a library calls the functions provided by the library but remains in control of the program logic. Application developers are free to decide how and when to call library functions, and how to integrate them in the overall control flow of the application.
- Framework: A set of functions and types with predefined slots to be filled by application developers. The framework is in control of the application flow, whereas developers provide functions and data that are used by the framework.

Because functions are first-class in Haskell, the distinction between libraries and frameworks is much less pronounced than in imperative languages: it is common also for library functions to take function arguments. Yet, the distinction remains that a framework takes control of the application while a library does not.

Does the dependency rule forbid using any kind of library or framework? Must the application core consist of 100% custom code only? No, it is possible to use third-party libraries or even frameworks in the inner architectural rings. This code becomes part of the domain and should adhere to the same quality standards. Depending on the domain requirements, it should be stable and mature, so that the domain does not change more often than necessary. Most importantly, a library of framework should be used in the inner rings only if it directly supports the domain functionality. If it provides some cross-cutting service, it does not belong in the application's core in the first place. Good examples for libraries used in the core are those that provide domain types like a UUID, an email address or a monetary value. Bad examples are a parser, or a JSON serializer.

## Implementation

How to implement our Conduit server application following Clean Architecture principles?

### Modules

What Clean Architecture refers to as a _Module_ is not the same as a Haskell module and source file in our code base. We represent an architectural _Module_ by a directory and a path in the namespace. Our application consists of the following _Modules_:

- _Application Core_: `Domain`
- _Use Cases_: `Usecases`
- _Gateways_: `Persistence`

With this codebase setup, the dependency rule states that source files in `Domain` must not depend on any Haskell module in `Usecases` nor in `Persistence`, nor may it depend on other frameworks. A Haskell module in the `Usecases` _Module_ may depend on one or more Haskelle modules from `Domain`, but it must not depend on anything in `Persistence`, nor on other frameworks.

### Dependency Inversion




- - -
TODO: Exchangeable modules.
Open Issues: Parsing in smart constructors to enforce domain type invariants: Should this be delegated to outer rings if it relies on specific libraries?

- - -
©2020, Author: Ulrich Schuster
