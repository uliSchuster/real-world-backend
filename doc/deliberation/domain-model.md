# Domain Model

## Analysis

The [Real-World API Specification](https://github.com/gothinkster/realworld/tree/master/api) defines types at the backend/frontend interface.

### User

#### Roles

- Anonymous user (read-only)
- Authenticated user
- _Proposal:_ There is no admin role, and thus no admin users for now. This simplifies authentication and access control.

 _Proposal:_ Skip authentication initially, only model the user profile.

#### Profile

There are two user-related objects in the API: One for authentication, and one for the user profile. They overlap. Each prodile can contain a linkt to a profile picture. _Proposal:_ Model the link as type safe URL (`Text.URI`), not as text.

- Creation: Register user.
- Read: Users can read and view the profile of other users. _Question:_ All users or only authenticated users can read profiles?
- Update: Possible by the named user only.
- Delete: Not specified. This simplifies the application logic significantly, because we do not need to deal with orphaned articles and comments.

#### User References

The user is referenced in multiple places: Each Article belongs to exactly one user, as does each comment. How do we model this relation?

- Make the `User` type part of both the `Article` and the `Comment` types. That is, include the user whenever we are dealing with an article or a comment. This creates some memory overhead. On the other hand, it is easy to handle. User data might become inconsistent across multiple articles or comments.
- Include a reference to the `User`. This can be a database foreign key or an abstraction thereof, or it can be another unique field, like the username. Key point: If we use a user reference, we need to separately retrieve users from storage and maintain them in another data structure; e.g., a map.

### Article

Core type for the blogging system. The main content resides in the title, description and body. In addition, there is a _slug_ used to identify the article in routes. The slug should somehow be drived from the title. When the user changes the title, the slug must be updated as well; hence, the slug cannot be used to refer to other articles.

Each article has an author. This can be modeled simply as a link to a user profile; alternatively, the user profile can be embedded.

Creation and update timestamps can either handled automatically by the DBMS or as part of the domain logic (see design question below).

Each article can be tagged. Does adding or removing a tag imply an update to the article itself (and thus an updated timestamp)?

- Creation: A logged-in user can write a new article.
- Read: Anyone can read any article. There is no more fine-grained access control.
- Update: The original author can update an article. The API does not allow to transfer an article from one author to another author.
- Delete: The original author can delete an article. This triggers deletion of all comments posted for this article.

### Comments

A comment always pertains to exactly one article. Similar to articles, comments are attributed to a user and need to have creation and modification time stamps.

- Creation: A logged-in user can comment on an any existing article.
- Read: Anyone can read all comments that pertain to a given article.
- Update: Authorship cannot change, nor can the comment be attributed to another article.
- Delete: The original author can delete a comment.

### Tags

Tags are words that can be affixed to articles for means of organizing them. Tags can only be affixe to articles, not to comments nor users.

_Questions:_

1. Should users be able to create new tags, or should we provide a fixed set of tags? _Proposal:_ Start with predefined tags. This simplifies the problem, because we no not need CRUD-logic for tags.
2. Should the Author add tags to his or her articles, or should every user be able to add tags to arbitrary articles? _Proposal:_ Only authors can add tags to their articles.
3. If arbitrary users can add tags to arbitrary articles, are these tags private – that is, every user only sees his or her own tags, so that Article A has tags [U1.A, U1.B, U1.C] for user U1, but, say, tags [U2.A, U2.B] for user U2? _Proposal:_ Don't make it that complex, follow the preceding option and only let authors tag their own articles.

### Favorits

A user can mark an article to be a favourite. This can apply to multiple articles. Thus, the user can retrieve all favourite articles.

- Creation: A logged-in user marks an existing article as favourite
- Read: Query for all favourite articles. Favourites are private to a given user; other users can neither query nor see a given user's favorites.
- Update/Deleta: A user can remove the favorite mark on a given article.

### Following, Feeeds

A user can decide to _follow_ another user. This allows the following user to query for new articles by all followed users and thus create a _feed_ of articles of all followed users.

- Creation: A logged-in user marks an existing other user to follow that user.
- Read: The specification does not list a specific query to obtain the profiles of all followed users. Thus, the follow-status can be inspected on a user-by-user basis only, when viewing a specific user profile.
- Update/Deleta: A user can select to unfollow another user he or she is following.

## Domain Logic

_Question_ Should we provide generic functionality for listing and paging of articles, users, tags, and comments? The API specification requires this functionality for articles only.

## Design Questions

1. Should we map the domain model directly to the database schema, or should we decouple them?
    1. Depending on the persistence library used, _direct mapping_ may imply that the domain model is automatically derived from the database schema or vice versa, or that the link between the two is defined via some mapping definition. This would be the default for OO programs that rely on an Object-Relational Mapper (ORM) like Hibernate.
    1. A _decoupled_ domain model does not have any reference to the persistence model. Translation happens via a dedicated, manually written adapter/mapper.
1. Similarly, should we map the domain model directly to the ReST API, or should we decouple the API data model from the internal domain model?
1. How to model composition and relation among domain objects? Via nested data structures, via object ID values, or a combination thereof? In contrast to OO domain models, where objects are linked via memory references to form an in-memory object graph, immutable data structures favor a hierarchical structure over a graph. In OO porgrams that rely on an ORM persistence layer, references are typically resolved automatically upon access by the ORM engine (e.g., Hibernate may lazyly load all comment objects referenced in a blog-post object when they are accessed).
1. Source-of-truth and consistency: Any in-memory domain model that is backed by persistent storage can become inconsistent at any time - whenever the database is modified but the thread that maintains the in-memory domain model does not notice. Typically, the DBMS verifies consistency upon inserts and updates. This implies that in-memory data is tentative until persisted. Do we need to make this fact transparent in our domain model? e.g., by giving all types a "dirty" flag, as some ORM libraries would do automatically?
1. DBMS generated values: It is common for the DBMS to auto-generate primary keys upon insertion of a record, as well as additional attributes such as modification dates. Because values for these auto-generated attributes do not exist before a transaction is committed, they cannot appear as part of an immutable in-memory object. How to deal with this problem?
   1. Do not rely on the DBMS to auto-generate values. Instead, create all values in the domain code. E.g., create UUID identifiers.
   1. Wrap all auto-generated values in `Maybe`s.
   1. Define two types: A core domain type without the auto-generated fields, and a persistence type that contains these fields. Consequence: auto-generated fields cannot be referred to inside the domain logic, because they are specific to the chosen persistence layer.
1. How strongly should we encapsulate our domain types?
   1. No encapsulation at all: The domain types are mostly records of primitive types or library data structures. The corresponding values can freely be pattern-matched on, because all data constructors are accessible. On the downside, there cannot be semantic guarantees; e.g., if we store a name as `Text`, we cannot ensure a maximum length or constraints on the character set.
   2. Purely abstract types: All domain types are abstract and can only be accessed via a defined interface. This allows for strong semantic guarantees provided by smart constructors. On the other hand, we would need lots of boilerplate code to access fields. Pattern matching can be achieved via the `PatternSynonyms` language extension, as shown in [this example](https://haskell-explained.gitlab.io/blog/posts/2019/08/27/pattern-synonyms/index.html).
1. If we opt for strong domain types, exactly how much information do we want to encode in the type? For example, should our `Title` type constrain the number of characters? Should it ban line feed and tab characters? Which Unicode symbols are admissible? Scott Wlaschin has an [interesting blog post](https://fsharpforfunandprofit.com/posts/designing-with-types-more-semantic-types/) about these issues, as does Tom Moertel on [Safe Strings in Haskell](http://blog.moertel.com/posts/2006-10-18-a-type-based-solution-to-the-strings-problem.html).  On a more theoretical note, such types are called [refinement types](https://en.wikipedia.org/wiki/Refinement_type), and there indeed does exist a [refinement types library](http://nikita-volkov.github.io/refined/).
1. The `Slug` is used in the URL to uniquely identify an article. Hence, slugs must be unique. Yet, because slugs are derived from an Article's title, and thus may change if the title changes, how can we ensure that all slugs are unique?
1. So far, we take an `Article` and a `Comment` to be two separate things. Ar they? Or do they have enough things in common that we can factor out? Both are some text written by some author. Is this enough to create a `BlogContent` type that handels the text and author link, and then wrap it up in a surrounding `Article` and `Comment` types. What would we gain from this overhead?

## Design Decisions

1. Use `Text` instead of `String`.
1. Use `Data.Time` UTC time stamps internally. Conversion to local time happens at the application boundary only.
1. Use `Text.Email` to represent email addresses.
1. We try to follow the premise of "making illegal state unrepresentable". Therefore, we use dedicated types for `Title` and `User` types. They are set up as abstract data types, where the actual data constructor is hidden (not exported by the corresponding module). Construction can only be performed via smart constructors that validate the type's invariants.
1. Because the `Title` type is constrained to a list of words with whitespace in between, we can automatically derive a `Slug` from it. Consequently, we do not need to store the slug separately as part of the `Article` type; instead, we can derive it whenever it is needed. This eliminates an illegal state - where a would not match the slug.
