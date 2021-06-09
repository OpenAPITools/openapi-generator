from copy import deepcopy
import unittest
from petstore_api.model.mammal import Mammal
from petstore_api.model.triangle import Triangle


class TestCopy(unittest.TestCase):
    """TestCopy unit test stubs"""

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def testDeepCopyOneOf(self):
        """test deepcopy"""
        assert deepcopy(Mammal(class_name="whale"))

    def testDeepCopyAllOf(self):
        """test deepcopy"""
        assert deepcopy(Triangle(shape_type="Triangle", triangle_type="EquilateralTriangle"))


if __name__ == '__main__':
    unittest.main()
