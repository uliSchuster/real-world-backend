# Data Persistence

## Which Type of Data Management to Use

The [Conduit](https://github.com/gothinkster/realworld) blogging server needs to store new articles and comments, and retrieve existing articles, comments, and user information for presentation. Hence, we need a way to persist data.

### Persistence Management

There are two basic approaches we can take to persist data:

- Directly interact with the file system and thus manage persistence ourselves.
- Let a persistence management system take care of low-level details.

For the application at hand, and our goal to learn writing a pretty much standard enterprise application in Haskell, we take the second approach.

### Data Model, Data Retrieval and Mutation

Persistence management systems (PMS) come in several flavors. For an initial analyziz, though, it suffices to differentiate between relational database management systems (RDBMS) and "all the rest" - often called NoSQL, but more precisely non-relational. RDBMS provide all of the so-called ACID properties: Atomicity, Consistency, Isolation, and Durability, wile non-relational PMS sacrifice some of these properties in exchange for other desireable properties, like scalability, read throughput, or others.

RDBMS are well suited for storing and retrieving data at an atomic level, where individual information elements can be accessed and modified (like a name or an email address), and where the application needs to query for various combinations of these atomic data items. RDBMS are broadly similar; data access can be expressed in various dialects of the Structured Query Language (SQL). Well-known freely available RDBMS are [MySQL](https://www.mysql.com), [MariaDB](https://mariadb.org) (a MySQL fork), and [PostgreSQL](https://www.postgresql.org).

Non-relational PMS differ widely, both in their fundamental concepts and in their implementation. It is not the goal of our project to provide a review of existing non-relational PMS - this would be a reasearch project in its own right. Rather, we look at one specific type of non-relational PMS that might suite our application: [Document-oriented DBMS](https://en.wikipedia.org/wiki/Document-oriented_database). Freely available document DBMS are [ElasticSearch](https://www.elastic.co), [Solr](https://lucene.apache.org/solr/), [MongoDB](https://www.mongodb.com), or [CouchDB](https://couchdb.apache.org).

For the application at hand, both an RDBMS as well as a document-oriented DBMS seem valid architecture choices: On the one hand, blog articles are documents; on the other hand, additional data like tags and user information are atomic and must be related to articles proper. We do not face scaling requirements as would a large-scale blogging site, and we do not worry too much about massively concurrent updates to the same articles; so neither the ACID gaurantees of an RDBMS nor the scaling and sharding capabilitites of the document-oriented systems provide a compelling advantage.

_Proposal:_ Start with the well-understood and widely used relational model. Most production web applications currently rely on RDBMS; therefore, we can learn how to set up such a system and which problems might arise. Once the persistence ring is operational, we can decide to add a document store, or replace parts or all of the RDBMS.

## Interfaceing with an RDBMS in Haskell

Most RDBMS are programmed via a language - typically some dialect of SQL. In fact, SQL contains three orthogonal languages: A data query languate to retrieve data elements, a data mutation language to insert and update data, and a data modeling language to set up and alter the relations. In SQL, these languages are declarative. A programmer describes _what_ to do, but leaves the details of _how_ to do it up to the DBMS.

### Haskell RDBMS Libraries

There does not seem to be one de-facto standard for accessing RDBMS in Haskell. Instead, we can choose from a range of libraries, each of which has its own best practice.

There are low-level, mid-level and high-level libraries. The low-level libraries encapsulate the DBMS protocol; besides that, it is up to the user to write SQL queries and to map between Haskell types and the primitive types of the DBMS, and to handle all DBMS errors:
- [MySQL](https://hackage.haskell.org/package/mysql): A binding to the `mysqlclient` C-library.
- [MySQL-simple](https://hackage.haskell.org/package/mysql-simple): MySQL client library.
- [PostgreSQL-simple](https://hackage.haskell.org/package/postgresql-simple): A binding to the `libpq` PostgreSQL client library.
- [HaSQL](https://github.com/nikita-volkov/hasql): Native PostgreSQL driver.

Mid-level abstractions:

- [Haskell Database Connectivity (HDBC)](https://github.com/hdbc/hdbc/wiki): DBMS-independent abstraction to connecto to ODBC-compliant DBMS. Loosely similar do JDBC in the Java worls.
- [HSQL](https://hackage.haskell.org/package/hsql): Seems to be superseded by HDBC.
- [PostgreSQL-typed](https://hackage.haskell.org/package/postgresql-typed): Alibrary to generate type-safe PostgreSQL query statements.
- [HaskellDB](https://hackage.haskell.org/package/haskelldb): A library to generate type-safe SQL statements

High-level abstractions:

- [Persistent](https://www.yesodweb.com/book/persistent), the persistence framework for a variety of relational and non-relational databases that powers Yesod. Uses a Template Haskell DSL to specify DB schemata. Can be used with RDBMS and with MongoDB.
- [Esqueleto](https://github.com/bitemyapp/esqueleto), and embedded type-safe DSL for expressing SQL queries within Haskell. A library that works atop Persistent for RDBMS. Allows for JOINs, which Persistend is missing.
- [Opaleye](https://github.com/tomjaguarpaw/haskell-opaleye). Type-safe embedded DSL to interact with PostgreSQL.
- [Groundhog](https://www.schoolofhaskell.com/user/lykahb/groundhog). Relational DB mapping and querying. Similar in power to Persistent. No JOINs.
- [Beam](http://travis.athougies.net/projects/beam.html): Another type-safe DSL that gets translated to SQL statements. Backend-agnostic.
- [Haskell Relational Record](https://khibino.github.io/haskell-relational-record/)

There are some more libraries available. Some seem to be outdated, others not yet ready for production. Which bastraction level and which library to pick? The benefit of the high-level libraries is type-safety: They ensure that we cannot mess up columns, or write illegal queries. The price to pay is a learning cuve for the library.

W. Yao has written an [opinionated comparison of persistence libraries for Haskell](https://williamyaoh.com/posts/2019-12-14-typesafe-db-libraries.html) on the basis of an example project. According to this comparison, Opaleye is the library of choice, even though it has a steep learning curve. However, Opaleye comes with fairly good [documentation](https://www.haskelltutorials.com/opaleye/index.html), even though it has not been updated since 2016 and is still missing some chapters. There are good [tutorials](https://github.com/tomjaguarpaw/haskell-opaleye/tree/master/Doc/Tutorial) as well.

Persistent comes with automated schema migration, but falls short in expressiveness. Esquelete is supposed to fill that gap, as described in this [tutorial on Esqueleto and Persisten](https://ocharles.org.uk/posts/2013-12-06-24-days-of-hackage-persistent-esqueleto.html), and this [School of Haskell Tutorial](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/persistent-db) as well..

_Proposal:_ Give _Opaleye_ a try. If it does not work out even for our simple project, move to _Persistent/Esqueleto_.

### Comparison: RDBMS in Imperative Languages

In imperative languages, the programmer specifies _how_ to retrieve, process and mutate data. To interface with an RDBMS, the SQL queries are typically encapsulated behind a procedural interface like a [Data Access Object (DAO)](https://en.wikipedia.org/wiki/Data_access_object) or a [Repository](https://martinfowler.com/eaaCatalog/repository.html). An imperative program maintains state in memory. Data in memory might have changed because variables are mutable; therefore, most persistence frameworks need to track the modification status of in-memory data elements.

Mutable variables often lead to an in-memory graph, where elements refer to each other. These references often do not correspond to the relations modeled in the database. In object-oriented languages, inheritance is another feature of the in-memory data model that cannot be directly mapped onto relations of the relational data model. Therefore, accessing a RDBMS is most often channeled through an object-relational mapper (ORM), such as [Hibernate](https://hibernate.org). For simpler data models, simpler means to structure data access may suffice, such as [Active Record](https://www.martinfowler.com/eaaCatalog/activeRecord.html), [Table Data Gateway](https://www.martinfowler.com/eaaCatalog/tableDataGateway.html), or [Row Data Gateway](https://www.martinfowler.com/eaaCatalog/rowDataGateway.html).

### RDBMS in Haskell

A purely functional, mostly declarative language like Haskell apperas to be much better suited to interface with an RDBMS. On the surface, this seems to be true. There are no heavyweight ORM frameworks that build proxy objects in memory to sync in-memory changes with the database.

However, Haskell applications also mutate state - that's the point why we need persistence. Even though our business logic does not mutate data but returns a new data element, when we finally write this new data element back to the database, we typically update fields in one or more relations that make up as specific business _entity_ - a long-lived set of data with a distinct identity. This becomes apparent when multiple threads of our application try to modify the same data element in the database. We must isolate data access in separate transactions, [..]
