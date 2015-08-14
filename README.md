# Wombat-J

A Scheme-like LISP, implemented in Clojure, which compiles at runtime to JVM bytecode.

Upon close inspection one might notice an extraordinary similarity between the Wombat-J
reader and compiler and the ones in Clojure. Truly a bizarre coincidence. If anything,
it's evidence that both implementations were divinely inspired.

My original focus was on learning the uses and limitations of invokeDynamic. It quickly
became clear, however, that invokeDynamic doesn't play nice with higher-order functions,
and therefore would not be terribly useful. However, MethodHandles were exactly what I
wanted, which is to say I wanted to implement lambdas without doing what Clojure does and
writing a gargantuan definition of apply and applyTo with 20 separate arities.

Wombat-J requires at least Java 1.7, but runs considerably faster on 1.8. There is a
100-200x performance boost between 1.7 and 1.8 when using MethodHandles, at least
according to my rudimentary and totally undocumented benchmarks, performed nearly a year
ago and about which I have only the vaguest recollection. I also (probably) recall that in
1.7, old-style reflection was actually faster than MethodHandles. So use 1.8. Besides,
it's been out for like 2 years at least. Get with the times, man!


## Usage

- Install Java 1.8 and [Leiningen](http://leiningen.org)
- `lein run`
- I like to set inferior-lisp-program to "lein run". There's no readline, so it's pretty
  annoying otherwise. I guess rlwrap would work.
- Don't do anything that doesn't work and it will work flawlessly!


## License

Copyright © 2014-2015 Jon Distad

Distributed under the Eclipse Public License either version 1.0 or (at your option) any
later version.
