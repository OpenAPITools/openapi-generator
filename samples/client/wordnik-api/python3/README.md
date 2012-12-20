# Python 3 client for Wordnik.com API

## Overview

This is a Python 3 client for the Wordnik.com v4 API. For more information, see http://developer.wordnik.com/ .

## Generation

This client was generated using the provided script:

```
/bin/python3-wordnik-api.sh
```

## Testing

The tests require you to set three environment varibales:

```sh
export API_KEY=your api key
export USER_NAME=some wordnik.com username
export PASSWORD=the user's password
```

The tests can be run as follows, e.g:

```
python3.2 tests/BaseApiTest.py
```
