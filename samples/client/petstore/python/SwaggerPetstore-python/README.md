## Requirements.
Python 2.7 and later.

## Setuptools
You can install the bindings via [Setuptools](http://pypi.python.org/pypi/setuptools).

```sh
python setup.py install
```

Or you can install from Github via pip:

```sh
pip install git+https://github.com/geekerzp/SwaggerPetstore-python.git
```

To use the bindings, import the pacakge:

```python
import SwaggerPetstore
```

## Manual Installation
If you do not wish to use setuptools, you can download the latest release.
Then, to use the bindings, import the package:

```python
import path.to.SwaggerPetstore-python.SwaggerPetstore
```

## Getting Started

TODO

## Documentation

TODO

## Tests

We use some external dependencies, multiple interpreters and code coverage analysis while running test suite.
Our Makefile handles much of this for you as long as you're running it inside of a [virtualenv](http://docs.python-guide.org/en/latest/dev/virtualenvs/):

```sh
$ make test
[... magically installs dependencies and runs tests on your virtualenv]
Ran 182 tests in 1.633s
OK (SKIP=6)
```

You can test in various python versions using:

```sh
$ make test-all
[... tox creates a virtualenv for every platform and runs tests inside of each]
py27: commands succeeded
py34: commands succeeded
```
