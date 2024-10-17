# Design

Simple data types for basic markdown objects are created and composed into the Algebraic Data Type `Md`.
Parsing is done by creating parsers for basic data types, composed into the `markdownParser`. 
Printing to `html` is done by creating a type class `PrettyBoy` with a method `prettyPrint`. 
Markdown data types are instances of `PrettyBoy`, with `prettyPrint` defined accordingly.

# Parser Construction

Base parsers are small with specific tasks, Then successively combined into more complex parsers. For example, `pUntil` takes two parsers, and repeatedly performs the first parser until the first time the second parser succeeds. This is used to create `pModText` and `pImages` among others.

- `<$>` is used to apply type constructors into the result of parsers. See `pBold`.
- `<*>` is used with `.` combine small parsers in a point free way. see `pad`.
- `<|>` allows grouping parsers of the same type, while keeping the code separate and modular. See `pModText` and `pBlock`.
- `>>=` is used to combine parsers and apply constraints that aren't context-free. See `pHeading`, using a `guard` clause inside a `do` block to read at most 7 `#`s. 

The result is declarative code which describes parsers in terms of smaller parsers. 

# Printing to html

`PrettyBoy` has a minimal definition `unwrapPrint`, specifying how an algebraic datatypes should be unwrapped and printed.
From there, the function `prettyPrint` is given for free. This reduces repetition between printing for different data types. If more granularity is required, it can be done by manually setting the `PrettyBoy` functions `preText`, `postText` and `prettyPrintAux`.

Thus, code is incredibly flexible:
- Changing default indentation can be done by passing different delimiters to `prettyPrint`. e.g. passing `"\n\t"` produces an extra indentation through out the whole html doc.
- Changing specific print features is easy, by changing the `unwrapPrint` function of the specific type you want to change.
- Adding new features is easy, by creating a new type instance of `PrettyBoy`, defining a parser for that type (and composing with `markdownParser`), and defining `unwrapPrint`. Barely any code needs to be changed to add features.

# Nested Text Modifiers

Implemented by allowing `ModText` to have another instance of `ModText` inside.
The only adjustment was changing `pBold` (and other text modifiers) to run `pModText` inside (creating a co-recursive structure). Printing nested modifiers is automatically handled since all types are already instances of `PrettyBoy`.
Infinite recursion is avoided by use of `<|>`, ensuring identical combinations of parsers are never repeated in a single parse. `Untouched` explicitly shows that characters are parsed with each application of `pModText`, demonstrating that the parser will succeed eventually.