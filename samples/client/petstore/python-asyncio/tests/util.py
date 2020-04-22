# flake8: noqa

import random


def id_gen(bits=32):
    """ Returns a n-bit randomly generated int """
    return int(random.getrandbits(bits))



