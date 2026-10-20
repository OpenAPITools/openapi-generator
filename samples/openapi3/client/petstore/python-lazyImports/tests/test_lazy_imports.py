import subprocess
import sys
from pathlib import Path
from textwrap import dedent


def test_lazy_imports_use_standard_library() -> None:
    subprocess.run(
        [
            sys.executable,
            "-c",
            dedent(
                """\
                import importlib.util
                import sys
                from types import ModuleType

                import petstore_api

                assert type(petstore_api) is ModuleType
                assert petstore_api.__spec__ is not None
                assert "lazy_imports" not in sys.modules
                assert not any(name.startswith("petstore_api.models.") for name in sys.modules)
                assert not any(name.startswith("petstore_api.api.") for name in sys.modules)

                import petstore_api.api
                import petstore_api.models

                assert not any(name.startswith("petstore_api.models.") for name in sys.modules)
                assert not any(name.startswith("petstore_api.api.") for name in sys.modules)

                assert importlib.util.find_spec("petstore_api.api") is not None
                assert importlib.util.find_spec("petstore_api.models") is not None
                assert "Pet" in petstore_api.__all__
                assert "ApiClient" in petstore_api.__all__
                assert "HttpSigningConfiguration" in petstore_api.__all__
                assert "Pet" in petstore_api.models.__all__
                assert "Pet" in dir(petstore_api.models)
                assert "DefaultApi" in petstore_api.api.__all__
                assert "DefaultApi" in dir(petstore_api.api)

                from petstore_api.api import DefaultApi

                assert DefaultApi is petstore_api.api.DefaultApi
                assert DefaultApi is petstore_api.DefaultApi
                assert "petstore_api.api.default_api" in sys.modules
                assert "petstore_api.api.pet_api" not in sys.modules

                from petstore_api import Pet

                assert Pet is petstore_api.Pet
                assert Pet is petstore_api.models.Pet
                assert Pet is petstore_api.__dict__["Pet"]
                assert Pet is petstore_api.models.__dict__["Pet"]
                assert petstore_api.CircularReferenceModel is petstore_api.models.CircularReferenceModel

                try:
                    petstore_api.missing_export
                except AttributeError as error:
                    assert "missing_export" in str(error)
                else:
                    raise AssertionError("unknown export did not raise AttributeError")

                assert "lazy_imports" not in sys.modules
                """
            ),
        ],
        check=True,
        cwd=Path(__file__).resolve().parents[1],
    )
