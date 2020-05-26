# coding: utf-8

"""
Run the tests.
$ pip install nose (optional)
$ cd OpenAPIPetstore-python
$ nosetests -v
"""


from __future__ import absolute_import

import unittest

import petstore_api


class TestChordates(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def testChordateDeserializationAndDiscriminator(self):
        """Test Chordates deserialization
        It should be possible to create instances from anywhere
        in the animal class hierarchy, as long as the discriminator
        value is valid. Internally, the get_discriminator_class()
        function may need to call itself recursively to traverse
        the composed hierarchy.
        These unit tests would not work if get_discriminator_class
        did not call itself recursively.
        """
        inst = petstore_api.BiologyChordate(
            class_name="biology.Mammal",
        )
        assert isinstance(inst, petstore_api.BiologyMammal)

        inst = petstore_api.BiologyChordate(
            class_name="biology.Primate",
        )
        assert isinstance(inst, petstore_api.BiologyPrimate)

        inst = petstore_api.BiologyChordate(
            class_name="biology.Reptile",
        )
        assert isinstance(inst, petstore_api.BiologyReptile)

        inst = petstore_api.BiologyChordate(
            class_name="biology.Hominid",
        )
        assert isinstance(inst, petstore_api.BiologyHominid)

    def testMammalDeserializationAndDiscriminator(self):
        """Test Chordates deserialization"""
        inst = petstore_api.BiologyMammal(
            class_name="biology.Hominid",
        )
        assert isinstance(inst, petstore_api.BiologyHominid)

    def testHominidDeserializationAndDiscriminator(self):
        inst = petstore_api.BiologyHominid(
            class_name="biology.Hominid",
        )
        assert isinstance(inst, petstore_api.BiologyHominid)
