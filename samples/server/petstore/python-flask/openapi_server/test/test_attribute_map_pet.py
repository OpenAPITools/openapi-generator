# coding: utf-8

from __future__ import absolute_import
import unittest

from openapi_server.models.api_response import ApiResponse  # noqa: E501
from openapi_server.models.pet import Pet
from openapi_server.test import BaseTestCase


class TestPetAttributeMap(BaseTestCase):
    """Model serialization has to respect attribute_map on the class"""

    def setUp(self) -> None:

        self.data = {
            "photo_urls": ["photoUrls", "photoUrls"],
            "name": "doggie",
            "id": 0,
            "category": {
                "name": "name",
                "id": 6
            },
            "tags": [{
                "name": "name",
                "id": 1
            }, {
                "name": "name",
                "id": 1
            }],
            "status": "available"
        }

        self.desired_data = {
            "photoUrls": ["photoUrls", "photoUrls"],
            "name": "doggie",
            "id": 0,
            "category": {
                "name": "name",
                "id": 6
            },
            "tags": [{
                "name": "name",
                "id": 1
            }, {
                "name": "name",
                "id": 1
            }],
            "status": "available"
        }

    def test_pet_to_dict_default(self):
        """
        By default `attribute_map` uses `True` value,
        attribute_map of the class is respected.
        """
        pet = Pet(**self.data)
        dikt = pet.to_dict()
        self.assertEqual(dikt, self.desired_data)

    def test_pet_to_dict_false(self):
        """
        Passing `False` into `to_dict` method does not use
        `attribute_map` on the class.
        """
        pet = Pet(**self.data)
        dikt = pet.to_dict(attr_map=False)
        self.assertEqual(dikt, self.data)

    def test_pet_from_dict_to_dict(self):
        """
        Creates new `Pet` instance by using `from_dict` method
        passing in data from existing pet `to_dict` method.
        Instances and their `dict` data are equal.
        """
        pet = Pet(**self.data)
        pet2 = Pet.from_dict(pet.to_dict())

        self.assertEqual(pet.to_dict(), pet2.to_dict())
        self.assertEqual(pet, pet2)


if __name__ == '__main__':
    unittest.main()
