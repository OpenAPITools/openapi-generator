import pytest
from fastapi import FastAPI
from fastapi.testclient import TestClient

from openapi_server.main import app as application


@pytest.fixture
def app() -> FastAPI:
    application.dependency_overrides = {}

    return application


@pytest.fixture
def client(app) -> TestClient:
    return TestClient(app)
