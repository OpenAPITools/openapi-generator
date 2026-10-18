"""Runtime contracts for clients generated from lazy-imports.yaml.

Run through PythonClientCodegenTest with -Dpython315=/path/to/python3.15.
The interpreter needs the dependencies for all four Python libraries.
"""

import asyncio
import compileall
import importlib
import inspect
import json
from pathlib import Path
import sys

from pydantic import ValidationError


output, library, imports, deferred = sys.argv[1:]
sys.path.insert(0, output)
assert compileall.compile_dir(Path(output) / "openapi_client", quiet=1)
package = importlib.import_module("openapi_client")
assert Path(package.__file__).resolve().is_relative_to(Path(output).resolve())
if imports != "eager":
    assert "openapi_client.api.unused_api" not in sys.modules
    assert "openapi_client.models.unused" not in sys.modules


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


async def exercise_api():
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
        assert Parent.model_config.get("defer_build", False) is False
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


asyncio.run(exercise_api())
print(f"Runtime contracts passed: {library}, {imports}, deferBuild={deferred}")
