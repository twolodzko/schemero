# El Schemero Pequeño

> *Do It, Do It Again, and Again, and Again ...*  
> &emsp; — *The Little Schemer* by Friedmann and Felleisen

![Lisp cycles XKCD #297: "Those are your father's parentheses. Elegant weapons for a more... civilized age."](https://imgs.xkcd.com/comics/lisp_cycles.png)

(source <https://xkcd.com/297/>)

**Schemero** is a minimal Scheme implementation in [Erlang]/[OTP]. Erlang is a functional programming language, with
native support of lists and uses recursion as the basic execution mode, the same as Scheme. It also has its quirks.
It is dynamically typed and doesn't have user-defined types, so the way of imitating them is to create tuples like
`{symbol, Name}` to define a `symbol` type.
The core concept of Erlang is [message passing], the programs are built of processes that communicate
with each other by passing messages to each other. This is how the code is parsed in Schemero: there's a `reader`
process that is connected to a file or standard input, to read a character you need to send it a message asking for
the next character and get an answer with the character back.
Since [everything] is immutable, the only way to preserve a state is by playing [hot potato] and [passing it along]
to other functions. The environments in Schemero are servers that keep the saved objects and support the `find`
(return the value of) and `store` (save) requests.

## Limitations

The biggest limitation of this implementation is the lack of garbage collector for the environments. Some functions
in Scheme (`lambda`, `let`, `let*`, etc) create closures, in Schemero this means spawning new `envir` servers. Those
servers need to be kept alive as long as they are referenced by anything returned by the functions. With a garbage
collector, the servers would be terminated as soon as they are not needed, but it is not implemented.


 [Erlang]: https://www.erlang.org/
 [everything]: https://learnyousomeerlang.com/starting-out-for-real#invariable-variables
 [hot potato]: https://en.wikipedia.org/wiki/Hot_potato
 [passing it along]: https://learnyousomeerlang.com/more-on-multiprocessing#state-your-state
 [message passing]: https://www.erlang.org/blog/message-passing/
 [OTP]: https://www.oreilly.com/library/view/designing-for-scalability/9781449361556/ch04.html
