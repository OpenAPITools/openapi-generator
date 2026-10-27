"""Runtime contracts for native lazy clients generated from lazy-imports.yaml.

Run through PythonClientCodegenTest with -DpythonExecutable=/path/to/python.
Native import checks are skipped on Python versions below 3.15.
The interpreter needs the dependencies for all four Python libraries.
"""

import asyncio
import compileall
import importlib
import inspect
import json
from pathlib import Path
import sys
import unittest


class Response:
    status = 200
    reason = "OK"
    headers = {"content-type": "application/json"}
    data = json.dumps({"children": [{"id": 7, "name": "child"}]}).encode()

    def getheader(self, name, default=None):
        return self.headers.get(name, default)

    def read(self):
        return self.data


class AsyncResponse(Response):
    async def read(self):
        return self.data


async def exercise_api(package, library):
    from pydantic import ValidationError

    client = package.ApiClient(package.Configuration(host="https://example.invalid"))
    calls = []

    def call(*args, **kwargs):
        calls.append(args)
        assert "/parents/7" in args[1]
        return Response()

    async def async_call(*args, **kwargs):
        call(*args, **kwargs)
        return AsyncResponse()

    client.call_api = call if library == "urllib3" else async_call
    api = package.ParentsApi(client)

    async def invoke(**kwargs):
        value = api.get_parent(**kwargs)
        return await value if inspect.isawaitable(value) else value

    try:
        # Argument errors must still be raised before any HTTP request.
        for arguments in ({}, {"id": "invalid"}):
            try:
                await invoke(**arguments)
            except ValidationError:
                pass
            else:
                raise AssertionError("Invalid operation arguments were accepted")
        assert not calls

        for _ in range(2):
            result = await invoke(id=7)
            assert result.children[0].id == 7
            assert result.to_dict() == {"children": [{"id": 7, "name": "child"}]}
            assert client.sanitize_for_serialization(result) == result.to_dict()
        assert len(calls) == 2

        from openapi_client import Parent
        from openapi_client.models import Parent as ModelExport
        from openapi_client.api import ParentsApi

        assert isinstance(result, Parent)
        assert Parent is ModelExport
        assert isinstance(api, ParentsApi)
        assert "children" in Parent.model_json_schema()["properties"]
        try:
            Parent.from_dict({"children": [{"id": "invalid", "name": "child"}]})
        except ValidationError:
            pass
        else:
            raise AssertionError("Invalid nested model was accepted")
        assert Parent.from_json(result.to_json()) == result

        from openapi_client import AnyChoice, Choice, Left, Right

        for composed in (Choice, AnyChoice):
            value = composed.from_dict({"id": 7, "name": "child"})
            assert value.actual_instance.id == 7
            assert composed.model_json_schema()
        circular = Left.from_dict({"right": {"left": None}})
        assert isinstance(circular.right, Right)
        assert circular.to_dict() == {"right": {}}
        assert Left.model_json_schema()
    finally:
        if library == "urllib3":
            client.rest_client.pool_manager.clear()
        else:
            await client.close()


class TestGeneratedClient(unittest.TestCase):
    @unittest.skipIf(
        sys.version_info < (3, 15), "Native lazy imports require Python 3.15 or newer"
    )
    def test_native_lazy_imports(self):
        sys.path.insert(0, self.output)
        self.assertTrue(compileall.compile_dir(Path(self.output) / "openapi_client", quiet=1))
        package = importlib.import_module("openapi_client")
        self.assertTrue(Path(package.__file__).resolve().is_relative_to(Path(self.output).resolve()))
        self.assertNotIn("openapi_client.api.unused_api", sys.modules)
        self.assertNotIn("openapi_client.models.unused", sys.modules)
        asyncio.run(exercise_api(package, self.library))


if __name__ == "__main__":
    TestGeneratedClient.output, TestGeneratedClient.library = sys.argv[1:]
    unittest.main(argv=[sys.argv[0]], verbosity=2)
