"""
This plugin randomizes the order of tests within a unittest.TestCase class

The original source of the code is:

http://code.google.com/p/python-nose/issues/detail?id=255

and the original author is: Charles McCreary

"""
__test__ = False

import logging
from nose.plugins import Plugin
from nose import loader
from inspect import isfunction, ismethod
from nose.case import FunctionTestCase, MethodTestCase
from nose.failure import Failure
from nose.util import isclass, isgenerator, transplant_func, transplant_class
import random
import unittest
from nose.tools import nottest

log = logging.getLogger(__name__)
CLASS_SPECIFIC_RANDOMIZE_TESTS_FIELD_NAME = 'randomize_tests_seed'


@nottest
def randomize_tests(seed=None):
    """
     The randomize_tests decorator

     It supports using a predefined seed, otherwise a random seed will be generated.
     Usage:

         @randomize_tests()
         class MyClass(unittest.TestCase):
            pass

        @randomize_tests(seed=1723930311)
        class MyOtherClass(unittest.TestCase):
            pass
    """
    def rebuild(cls):
        setattr(cls, CLASS_SPECIFIC_RANDOMIZE_TESTS_FIELD_NAME, seed)
        if seed is None:
            setattr(cls, CLASS_SPECIFIC_RANDOMIZE_TESTS_FIELD_NAME, random.getrandbits(32))

        return cls

    return rebuild


class Randomize(Plugin):
    """
    Randomize the order of the tests within a unittest.TestCase class
    """
    name = 'randomize'
    # Generate a seed for deterministic behaviour
    seed = random.getrandbits(32)
    # Run on all modules/classes, or only on classes denoted by @randomize_tests
    class_specific = False

    def options(self, parser, env):
        """Register commandline options.
        """
        Plugin.options(self, parser, env)
        parser.add_option('--randomize', action='store_true', dest='randomize',
                          help="Randomize the order of the tests within a unittest.TestCase class")
        parser.add_option('--seed', action='store', dest='seed', default=None, type=int,
                          help="Initialize the seed for deterministic behavior in reproducing failed tests")
        parser.add_option('--class-specific', action="store_true", dest='class_specific',
                          help="Determines if test randomization should only used in classes marked by the "
                               "@randomize_tests decorator")

    def configure(self, options, conf):
        """
        Configure plugin.
        """
        Plugin.configure(self, options, conf)
        self.classes_to_look_at = []

        if options.randomize:
            self.enabled = True
            if options.seed is not None:
                self.seed = options.seed
            random.seed(self.seed)

            if options.class_specific:
                self.class_specific = True

            print("Using %d as seed" % (self.seed,))

            if options.seed is not None and options.class_specific:
                self.class_specific = False
                print(
                    'NOTE: options --seed and --class-specific conflict, '
                    'Specific class randomization ignored, seed %d will be used.' % (self.seed,))

    def loadTestsFromNames(self, names, module=None):
        pass

    def wantClass(self, cls):
        self.classes_to_look_at.append(cls)
        # Change this to populate a list that makeTest can then process?

    def makeTest(self, obj, parent=None):
        """Given a test object and its parent, return a test case
        or test suite.
        """
        ldr = loader.TestLoader()
        if isinstance(obj, unittest.TestCase):
            return obj
        elif isclass(obj):
            if not self.class_specific:
                if parent and obj.__module__ != parent.__name__:
                    obj = transplant_class(obj, parent.__name__)
                if issubclass(obj, unittest.TestCase):
                    # Randomize the order of the tests in the TestCase
                    return self.randomized_loadTestsFromTestCase(obj)
                else:
                    return self.randomized_loadTestsFromTestClass(obj)
            else:
                class_specific_seed = getattr(obj, CLASS_SPECIFIC_RANDOMIZE_TESTS_FIELD_NAME, None)
                if issubclass(obj, unittest.TestCase) and class_specific_seed is not None:
                    random.seed(class_specific_seed)
                    print("Using %d as seed to randomize tests in %s" % (class_specific_seed, '.'.join(
                        [obj.__module__, obj.__name__])))
                    return self.randomized_loadTestsFromTestCase(obj)
                else:
                    return ldr.loadTestsFromTestCase(obj)

        elif ismethod(obj):
            if parent is None:
                parent = obj.__class__
            if issubclass(parent, unittest.TestCase):
                return [parent(obj.__name__)]
            else:
                if isgenerator(obj):
                    return ldr.loadTestsFromGeneratorMethod(obj, parent)
                else:
                    return MethodTestCase(obj)
        elif isfunction(obj):
            if parent and obj.__module__ != parent.__name__:
                obj = transplant_func(obj, parent.__name__)
            if isgenerator(obj):
                return ldr.loadTestsFromGenerator(obj, parent)
            else:
                return [FunctionTestCase(obj)]
        else:
            return Failure(TypeError,
                           "Can't make a test from %s" % obj)

    def randomized_loadTestsFromTestClass(self, cls):
        tests = loader.TestLoader().loadTestsFromTestClass(cls)
        return self._shuffler(tests)

    def randomized_loadTestsFromContextSuite(self, suite):
        tests = loader.TestLoader().loadTestsFromTestModule(suite)
        return self._shuffler(tests)

    def randomized_loadTestsFromTestCase(self, testCaseClass):
        tests = loader.TestLoader().loadTestsFromTestCase(testCaseClass)
        return self._shuffler(tests)

    def _shuffler(self, tests):
        """Shuffles the given tests"""
        randomized_tests = []
        for t in tests._tests:
            randomized_tests.append(t)
        random.shuffle(randomized_tests)
        tests._tests = (t for t in randomized_tests)
        return tests
