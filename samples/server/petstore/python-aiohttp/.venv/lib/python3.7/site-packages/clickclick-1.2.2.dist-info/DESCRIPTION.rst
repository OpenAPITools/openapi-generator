===========
Click Click
===========

.. image:: https://travis-ci.org/zalando/python-clickclick.svg?branch=master
   :target: https://travis-ci.org/zalando/python-clickclick
   :alt: Travis CI build status

.. image:: https://coveralls.io/repos/zalando/python-clickclick/badge.svg
   :target: https://coveralls.io/r/zalando/python-clickclick

.. image:: https://img.shields.io/pypi/dw/clickclick.svg
   :target: https://pypi.python.org/pypi/clickclick/
   :alt: PyPI Downloads

.. image:: https://img.shields.io/pypi/v/clickclick.svg
   :target: https://pypi.python.org/pypi/clickclick/
   :alt: Latest PyPI version

.. image:: https://img.shields.io/pypi/l/clickclick.svg
   :target: https://pypi.python.org/pypi/clickclick/
   :alt: License

Utility functions (Python 3 only) for the wonderful `Click library`_.
Click is a Python package for creating beautiful command line interfaces in a composable way with as little code as necessary.


Usage
=====

.. code-block:: python

    from clickclick import Action, OutputFormat

    with Action('Performing remote call..') as act:
        do_something()
        act.progress()
        do_something_else()

    output_format = 'json' # default: "text"
    with OutputFormat(output_format):
        print_table(['col1', 'col2'], rows)


.. _Click library: http://click.pocoo.org/

Working Example
---------------

See this `example script`_ and the `shell script`_.

.. _example script: example.py

.. _shell script: example.sh

.. code-block:: python3

   $ ./example.py
   Usage: example.py [OPTIONS] COMMAND [ARGS]...

   Options:
     -V, --version  Print the current version number and exit.
     -h, --help     Show this message and exit.

   Commands:
     list              Example for Listings
     localtime         Print the localtime
     output            Example for all possible Echo Formats You see...
     work-in-progress  Work untile working is done
     work_done         Work done in ?? %

::

   $ ./example.py l
   Usage: example.py [OPTIONS] COMMAND [ARGS]...

   Error: Too many matches: list, localtime

::

   $ ./example.py lo
   Localtime: 2015-08-27 15:47:46.688547

::

   $ ./example.py li
   Identifier|Name     |Status |Creation Date|Description                                       |Without Title
            0 Column #0 ERROR   -4228033s ago this is a verrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr.. column without title
            1 Column #1 FINE    -4228033s ago this is a verrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr.. column without title
            2 Column #2 WARNING -4228033s ago this is a verrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr.. column without title

::

   $ ./example.py li -o tsv
   id  name    state   creation_time   desc    without_title
   0   Column #0   ERROR   -4228033s ago   this is a verrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrry long description    column without title
   1   Column #1   FINE    -4228033s ago   this is a verrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrry long description    column without title
   2   Column #2   WARNING -4228033s ago   this is a verrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrry long description    column without title

::

   $ ./example.py li -o json
   [{"creation_time": 1444911300, "desc": "this is a verrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrry long description", "id": 0, "name": "Column #0", "state": "ERROR", "without_title": "column without title"}, {"creation_time": 1444911300, "desc": "this is a verrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrry long description", "id": 1, "name": "Column #1", "state": "FINE", "without_title": "column without title"}, {"creation_time": 1444911300, "desc": "this is a verrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrry long description", "id": 2, "name": "Column #2", "state": "WARNING", "without_title": "column without title"}]

::

   $ ./example.py li -o yaml
   creation_time: 1444911300
   desc: this is a verrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrry long description
   id: 0
   name: 'Column #0'
   state: ERROR
   without_title: column without title
   ---
   creation_time: 1444911300
   desc: this is a verrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrry long description
   id: 1
   name: 'Column #1'
   state: FINE
   without_title: column without title
   ---
   creation_time: 1444911300
   desc: this is a verrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrry long description
   id: 2
   name: 'Column #2'
   state: WARNING
   without_title: column without title


::

   $ ./example.py work-
   do anything.. OK
   create an excption.. EXCEPTION OCCURRED: No active exception to reraise
   Start with working.. . . . . OK
   Calc 1 + 1.. 2
   Oh, I make an error.. work not complete done
   Oh, I make a warning.. work is complicated
   Start an exception.. EXCEPTION OCCURRED: name 'function_not_found' is not defined
   Make a final error.. this is the end..

::

   $ ./example.py work_ 15.4
   Please select the state of your work
   1) Done
   2) In Progress
   3) unknown
   4) lost
   Please select (1-4) [4]: 2
   Your work is 15.4% In Progress

::

   $ ./example.py work_ 15.4
   Please select the state of your work
   1) Done
   2) In Progress
   3) unknown
   4) lost
   Please select (1-4) [4]: 3
   Your work is 15.4% unknown

::

   $ ./example.py work_ 15.4
   Please select the state of your work
   1) Done
   2) In Progress
   3) unknown
   4) lost
   Please select (1-4) [4]:
   Your work is 15.4% lost

::

   $ ./example.py output
   This is a ok: OK
   This is a ok with message:all is fine
   This is a warning: please check this
   Start with working.. . . . . OK
   Id|Name
    1 Test #1
    2 Test #2
   Only FYI
   This is a error: this is wrong, please fix
   This is a fatal error: this is a fuckup

::

   $ ./example.py output -o tsv
   id  name
   1   Test #1
   2   Test #2

::

   $ ./example.py output -o json
   [{"id": 1, "name": "Test #1"}, {"id": 2, "name": "Test #2"}]

::

   $ ./example.py output -o yaml
   id: 1
   name: 'Test #1'
   ---
   id: 2
   name: 'Test #2'


License
=======

Copyright (c) 2015 Zalando SE

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.


