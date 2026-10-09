# coding: utf-8

# flake8: noqa

from datetime import datetime
import os
import time
import unittest

import petstore_api
from pydantic import ValidationError


class OrderModelTests(unittest.TestCase):

    def test_public_names_round_trip(self):
        order = petstore_api.Order(
            id=1,
            petId=3,
            _field_validator=2,
            datetime=datetime(2026, 6, 23),
            status="placed",
            complete=True,
        )
        self.assertEqual(1, order.id)
        self.assertEqual(3, order.pet_id)
        self.assertEqual(2, order._field_validator)
        self.assertEqual(datetime(2026, 6, 23), order.datetime)
        self.assertEqual("placed", order.status)
        self.assertTrue(order.complete)
        order_dict = order.to_dict()
        self.assertEqual(1, order_dict["id"])
        self.assertEqual(3, order_dict["petId"])
        self.assertEqual(2, order_dict["quantity"])
        self.assertEqual(datetime(2026, 6, 23), order_dict["shipDate"])
        self.assertTrue(order_dict["complete"])

    def test_public_assignment_is_validated(self):
        ship_date = datetime(2026, 6, 23)
        order = petstore_api.Order(_field_validator=2, datetime=ship_date)
        order._field_validator = 3
        self.assertEqual(3, order._field_validator)
        self.assertEqual(ship_date, order.datetime)
        new_ship_date = datetime(2026, 6, 24)
        order.datetime = new_ship_date
        self.assertEqual(3, order._field_validator)
        self.assertEqual(new_ship_date, order.datetime)
        with self.assertRaises(ValidationError):
            order._field_validator = "not an integer"  # type: ignore[assignment]

    def test_storage_names_are_not_mapping_inputs(self):
        storage_order = petstore_api.Order.model_validate({
            "var_field_validator": 9,
            "var_datetime": datetime(2026, 6, 25),
        })
        self.assertIsNone(storage_order._field_validator)
        self.assertIsNone(storage_order.datetime)

    def test_enum_validation(self):
        with self.assertRaises(ValidationError):
            petstore_api.Order(status="invalid")

    def test_from_dict_accepts_public_names(self):
        public_order = petstore_api.Order.from_dict({
            "id": 2,
            "pet_id": 5,
            "_field_validator": 3,
            "datetime": datetime(2026, 6, 24),
            "status": "approved",
            "complete": True,
        })
        self.assertIsNotNone(public_order)
        assert public_order is not None
        self.assertEqual(2, public_order.id)
        self.assertEqual(5, public_order.pet_id)
        self.assertEqual(3, public_order._field_validator)
        self.assertEqual(datetime(2026, 6, 24), public_order.datetime)
        self.assertTrue(public_order.complete)

    def test_wire_and_public_names_are_ambiguous(self):
        with self.assertRaisesRegex(
            ValueError,
            "Order received both 'quantity' and '_field_validator'",
        ):
            petstore_api.Order.from_dict({
                "quantity": 5,
                "_field_validator": 6,
            })
