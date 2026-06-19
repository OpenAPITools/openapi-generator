# flake8: noqa

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
    from lazy_imports import LazyModule, as_package, load

    load(
        LazyModule(
            *as_package(__file__),
            """# import apis into api package
from petstore_api.api.another_fake_api import AnotherFakeApi
from petstore_api.api.default_api import DefaultApi
from petstore_api.api.fake_api import FakeApi
from petstore_api.api.fake_classname_tags123_api import FakeClassnameTags123Api
from petstore_api.api.import_test_datetime_api import ImportTestDatetimeApi
from petstore_api.api.pet_api import PetApi
from petstore_api.api.store_api import StoreApi
from petstore_api.api.user_api import UserApi

""",
            name=__name__,
            doc=__doc__,
        )
    )
