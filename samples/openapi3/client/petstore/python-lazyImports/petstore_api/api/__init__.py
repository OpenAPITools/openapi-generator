# flake8: noqa

__all__ = [
    "AnotherFakeApi",
    "DefaultApi",
    "FakeApi",
    "FakeClassnameTags123Api",
    "ImportTestDatetimeApi",
    "PetApi",
    "StoreApi",
    "UserApi",
]

if __import__("typing").TYPE_CHECKING:
    # import apis into api package
    from petstore_api.api.another_fake_api import AnotherFakeApi
    from petstore_api.api.default_api import DefaultApi
    from petstore_api.api.fake_api import FakeApi
    from petstore_api.api.fake_classname_tags123_api import FakeClassnameTags123Api
    from petstore_api.api.import_test_datetime_api import ImportTestDatetimeApi
    from petstore_api.api.pet_api import PetApi
    from petstore_api.api.store_api import StoreApi
    from petstore_api.api.user_api import UserApi
    
else:
    from importlib import import_module

    _exports = {
        "AnotherFakeApi": ".another_fake_api",
        "DefaultApi": ".default_api",
        "FakeApi": ".fake_api",
        "FakeClassnameTags123Api": ".fake_classname_tags123_api",
        "ImportTestDatetimeApi": ".import_test_datetime_api",
        "PetApi": ".pet_api",
        "StoreApi": ".store_api",
        "UserApi": ".user_api",
    }

    def __getattr__(name: str) -> object:
        if (module_name := _exports.get(name)) is None:
            raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
        value = getattr(import_module(module_name, __name__), name)
        globals()[name] = value
        return value

    def __dir__() -> list[str]:
        return sorted(globals().keys() | _exports.keys())
