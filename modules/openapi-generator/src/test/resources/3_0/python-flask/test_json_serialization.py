"""Runtime regressions shared by the two generated Flask Petstore samples."""

from contextlib import contextmanager
from datetime import date, datetime, timedelta, timezone
from decimal import Decimal
from importlib.metadata import version
import json
from unittest.mock import patch
from uuid import UUID

import connexion
from flask import Flask
import pytest

from openapi_server.models.category import Category
from openapi_server.models.order import Order
from openapi_server.models.pet import Pet
from openapi_server.models.tag import Tag


CONNEXION_3 = version('connexion').split('.')[0] == '3'


@pytest.fixture
def serializer():
    from openapi_server import encoder

    if CONNEXION_3:
        return encoder.JSONEncoder()
    return encoder.JSONProvider(Flask(__name__))


def serialize(serializer, value):
    if CONNEXION_3:
        return json.loads(serializer.encode(value))
    return json.loads(serializer.dumps(value))


def pet_and_json(include_nulls=False):
    pet = Pet(id=7, name='Fluffy', photo_urls=['https://example.com/pet.png'],
              category=Category(id=3), tags=[Tag(id=4, name='friendly')])
    expected = {
        'id': 7,
        'name': 'Fluffy',
        'photoUrls': ['https://example.com/pet.png'],
        'category': {'id': 3},
        'tags': [{'id': 4, 'name': 'friendly'}],
    }
    if include_nulls:
        expected['category']['name'] = None
        expected['status'] = None
    return pet, expected


@contextmanager
def production_client():
    """Exercise the generated entry point, replacing only the blocking server."""
    from openapi_server.__main__ import main

    app_class = connexion.FlaskApp if CONNEXION_3 else connexion.App
    with patch.object(app_class, 'run', autospec=True) as run:
        main()
    run.assert_called_once()
    app = run.call_args.args[0]
    client = app.test_client() if CONNEXION_3 else app.app.test_client()
    with client as test_client:
        yield test_client


def assert_json_response(response, expected):
    assert response.status_code == 200
    assert response.headers['Content-Type'].split(';')[0] == 'application/json'
    body = response.content if CONNEXION_3 else response.data
    assert json.loads(body) == expected


def test_model_property_names_and_primitives(serializer):
    order = Order(id=1, pet_id=7, quantity=0, complete=False)
    assert serialize(serializer, order) == {
        'id': 1, 'petId': 7, 'quantity': 0, 'complete': False,
    }


@pytest.mark.parametrize('include_nulls', [False, True])
def test_nested_models_and_collections(serializer, include_nulls):
    serializer.include_nulls = include_nulls
    pet, expected = pet_and_json(include_nulls)
    assert serialize(serializer, {
        'pet': pet,
        'pets': [pet, None],
        'byId': {'7': pet},
        'nested': [{'pets': [pet]}],
        'null': None,
    }) == {
        'pet': expected,
        'pets': [expected, None],
        'byId': {'7': expected},
        'nested': [{'pets': [expected]}],
        'null': None,
    }


def test_include_nulls_subclass(serializer):
    class IncludingNulls(type(serializer)):
        include_nulls = True

    serializer = IncludingNulls() if CONNEXION_3 else IncludingNulls(Flask(__name__))
    pet, expected = pet_and_json(include_nulls=True)
    assert serialize(serializer, pet) == expected


@pytest.mark.parametrize('value,connexion2_json,connexion3_json', [
    (datetime(2024, 4, 5, 6, 7, 8),
     '2024-04-05T06:07:08Z', 'Fri, 05 Apr 2024 06:07:08 GMT'),
    (datetime(2024, 4, 5, 6, 7, 8, tzinfo=timezone(timedelta(hours=-3))),
     '2024-04-05T06:07:08-03:00', 'Fri, 05 Apr 2024 09:07:08 GMT'),
    (date(2024, 4, 5), '2024-04-05', 'Fri, 05 Apr 2024 00:00:00 GMT'),
    (Decimal('12.50'), 12.5, '12.50'),
    (UUID('12345678-1234-5678-1234-567812345678'),
     '12345678-1234-5678-1234-567812345678', '12345678-1234-5678-1234-567812345678'),
])
def test_special_types(serializer, value, connexion2_json, connexion3_json):
    expected = connexion3_json if CONNEXION_3 else connexion2_json
    assert serialize(serializer, {'value': value}) == {'value': expected}


def test_unsupported_type_still_raises(serializer):
    with pytest.raises(TypeError):
        serialize(serializer, object())


@pytest.mark.parametrize('include_nulls', [False, True])
def test_model_http_response(include_nulls):
    from openapi_server import encoder

    serializer_class = encoder.JSONEncoder if CONNEXION_3 else encoder.JSONProvider
    pet, expected = pet_and_json(include_nulls)
    with patch.object(serializer_class, 'include_nulls', include_nulls), patch(
        'openapi_server.controllers.pet_controller.get_pet_by_id', autospec=True,
        return_value=(pet, 200, {'Content-Type': 'application/json'}),
    ), production_client() as client:
        response = client.get('/v2/pet/7', headers={'api_key': 'special-key'})
        assert_json_response(response, expected)


def test_order_http_response():
    order = Order(id=1, pet_id=7, quantity=2, status='placed', complete=False,
                  ship_date=datetime(2024, 4, 5, 6, 7, 8, tzinfo=timezone.utc))
    expected = {
        'id': 1, 'petId': 7, 'quantity': 2, 'status': 'placed', 'complete': False,
        'shipDate': ('Fri, 05 Apr 2024 06:07:08 GMT' if CONNEXION_3
                     else '2024-04-05T06:07:08+00:00'),
    }
    with patch(
        'openapi_server.controllers.store_controller.get_order_by_id', autospec=True,
        return_value=(order, 200, {'Content-Type': 'application/json'}),
    ), production_client() as client:
        assert_json_response(client.get('/v2/store/order/1'), expected)


def test_generated_test_app_serializes_models():
    from openapi_server.test import BaseTestCase

    pet, expected = pet_and_json()
    with patch(
        'openapi_server.controllers.pet_controller.get_pet_by_id', autospec=True,
        return_value=(pet, 200, {'Content-Type': 'application/json'}),
    ):
        case = BaseTestCase()
        if CONNEXION_3:
            case.setUp()
            try:
                response = case.client.open('/v2/pet/7', headers={'api_key': 'special-key'})
            finally:
                case.tearDown()
        else:
            with case.create_app().test_client() as client:
                response = client.get('/v2/pet/7', headers={'api_key': 'special-key'})
        assert response.status_code == 200
        assert json.loads(response.data) == expected


def test_swagger_ui():
    with production_client() as client:
        response = client.get('/v2/ui/')
        assert response.status_code == 200
        assert response.headers['Content-Type'].split(';')[0] == 'text/html'
        body = response.content if CONNEXION_3 else response.data
        assert b'swagger-ui' in body
