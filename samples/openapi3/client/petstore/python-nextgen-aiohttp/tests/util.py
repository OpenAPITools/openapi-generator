# flake8: noqa

import asyncio
import random


def id_gen(bits=32):
    """ Returns a n-bit randomly generated int """
    return int(random.getrandbits(bits))


def async_test(f):
    def wrapper(*args, **kwargs):
        coro = asyncio.coroutine(f)
        future = coro(*args, **kwargs)
        loop = asyncio.get_event_loop()
        loop.run_until_complete(future)
    return wrapper
