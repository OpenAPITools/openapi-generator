Introduction
============

Mypyc compiles Python modules to C extensions. It uses standard Python
`type hints
<https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html>`_ to
generate fast code.

The compiled language is a strict, statically typed Python variant.
It restricts the use of some dynamic Python features to gain performance,
but it's mostly compatible with standard Python.

Mypyc uses `mypy <http://www.mypy-lang.org/>`_ to perform type
checking and type inference. Most type system features in the stdlib
`typing <https://docs.python.org/3/library/typing.html>`_ module are
supported.

Compiled modules can import arbitrary Python modules and third-party
libraries.

You can run the modules you compile also as normal, interpreted Python
modules.

You can roughly expect speedups like these (2x improvement means half the
runtime):

* Existing code with type annotations often gets **1.5x to 5x** faster.

* Code tuned for mypyc can be **5x to 15x** faster.

There is no simple answer to how fast your code will be when compiled.
You should try it out!

Mypyc currently aims to speed up non-numeric code, such as server
applications. We also use mypyc to compile mypyc, of course!

Motivation
----------

Though Python has been successful without a good performance story
for non-numeric code, speed still matters:

1. Users prefer more efficient and responsive software and libraries.

2. You need less hardware to run your server application and save money.

3. You'll waste less time waiting for your tests and jobs to finish.

4. Faster code correlates with less energy use and is better for the
   environment.

Perks
-----

**Easy to get started.** Compiled code looks and behaves like normal
Python code. You get the benefits of static typing while using the
syntax, libraries and idioms you (and millions of developers) already
know.

**Expressive types.** Mypyc fully supports standard Python type hints.
Mypyc has local type inference, generics, optional types, tuple types,
union types, and more. Type hints act as machine-checked
documentation, making code not only faster but also easier to
understand and modify.

**Fast program startup.** Mypyc uses ahead-of-time compilation, so
compilation does not slow down program startup. Slow program startup
is a common problem with JIT compilers.

**Python ecosystem supported.** Mypyc runs on top of CPython, the
standard Python implementation. Code can freely use standard library
features. Use pip to install any third-party libraries you need,
including C extensions.

**Migration path for existing Python code.** Existing Python code
often requires only minor changes to compile using mypyc.

**Waiting for compilation is optional.** Compiled code also runs as
normal Python code. You can use interpreted Python during development,
with familiar and fast workflows.

**Runtime type safety.** Mypyc protects you from segfaults and memory
corruption. Any unexpected runtime type safety violation is a bug in
mypyc.

**Find errors statically.** Mypyc uses mypy for static type checking
that will catch many bugs. This saves time you'd otherwise spend
debugging.

Use cases
---------

**Fix performance bottlenecks.** Often most time is spent in a few
Python modules or functions. Add type annotations and compile these
modules for easy performance gains.

**Compile it all.** During development you use interpreted mode, for a
quick edit-run cycle. In releases all non-test code is compiled. This
is how mypy got a *4x performance improvement* over interpreted Python.

**Take advantage of existing type hints.** If you already use type
annotations in your code, adopting mypyc will be easier. You've already
done most of the work needed to use mypyc.

**Alternative to a lower-level language.** Instead of writing
performance-critical code in C, C++, Cython or Rust, you may get good
performance while staying in the comfort of Python.

**Migrate C extensions.** Maintaining C extensions is not always fun
for a Python developer. With mypyc you may get performance similar to
the original C, with the convenience of Python.

How does it work
----------------

Mypyc uses several techniques to produce fast code:

* Mypyc uses *ahead-of-time compilation* to native code. This removes
  CPython interpreter overhead.

* Mypyc enforces type annotations (and type comments) at runtime,
  raising ``TypeError`` if runtime values don't match annotations.

* Compiled code uses optimized, type-specific primitives.

* Mypyc uses *early binding* to resolve called functions and name
  references at compile time. Mypyc avoids many dynamic namespace
  lookups.

* Classes are compiled to *C extension classes*. They use `vtables
  <https://en.wikipedia.org/wiki/Virtual_method_table>`_ for fast
  method calls and attribute access.

* Mypyc treats compiled functions, classes, and attributes declared
  ``Final`` as immutable.

* Mypyc has memory-efficient, unboxed representions for integers and
  booleans.

Development status
------------------

Mypyc is currently *alpha software*. It's only recommended for
production use cases with careful testing, and if you are willing to
contribute fixes or to work around issues you will encounter.
