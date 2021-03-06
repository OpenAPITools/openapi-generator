.. _list-ops:

Native list operations
======================

These ``list`` operations have fast, optimized implementations. Other
list operations use generic implementations that are often slower.

Construction
------------

Construct list with specific items:

* ``[item0, ..., itemN]``

Construct list from iterable:

* ``list(x: Iterable)``

List comprehensions:

* ``[... for ... in ...]``
* ``[... for ... in ... if ...]``

Operators
---------

Get item by integer index:

* ``lst[n]``

Slicing:

* ``lst[n:m]``, ``lst[n:]``, ``lst[:m]``, ``lst[:]``

Repeat list ``n`` times:

* ``lst * n``, ``n * lst``

Statements
----------

Set item by integer index:

* ``lst[n] = x``

For loop over a list:

* ``for item in lst:``

Methods
-------

* ``lst.append(item)``
* ``lst.extend(x: Iterable)``
* ``lst.pop()``
* ``lst.count(item)``

Functions
---------

* ``len(lst: list)``
