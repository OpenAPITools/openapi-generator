# coding: utf-8

# flake8: noqa

# import all models into this package
# if you have many models here with many references from one model to another this may
# raise a RecursionError
# to avoid this, import only the models that you directly need like:
# from from one_of.model.pet import Pet
# or import this package, but before doing it, use:
# import sys
# sys.setrecursionlimit(n)

from one_of.model.apple import Apple
from one_of.model.apple_color import AppleColor
from one_of.model.banana import Banana
from one_of.model.banana_color import BananaColor
from one_of.model.fruit import Fruit
