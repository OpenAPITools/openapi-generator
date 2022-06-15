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
        obj = deepcopy(Mammal(class_name="whale"))
        assert id(deepcopy(obj)) != id(obj)
        assert deepcopy(obj) == obj

    def testDeepCopyAllOf(self):
        """test deepcopy"""
        obj = Triangle(shape_type="Triangle", triangle_type="EquilateralTriangle", foo="blah")
        assert id(deepcopy(obj)) != id(obj)
        assert deepcopy(obj) == obj

        obj = Triangle._new_from_openapi_data(shape_type="Triangle", triangle_type="EquilateralTriangle", foo="blah")
        assert id(deepcopy(obj)) != id(obj)
        assert deepcopy(obj) == obj


if __name__ == '__main__':
    unittest.main()
