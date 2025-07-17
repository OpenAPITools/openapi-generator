# flake8: noqa

if __import__("typing").TYPE_CHECKING:
    # import apis into api package
    from openapi_client.api.auth_api import AuthApi
    from openapi_client.api.body_api import BodyApi
    from openapi_client.api.form_api import FormApi
    from openapi_client.api.header_api import HeaderApi
    from openapi_client.api.path_api import PathApi
    from openapi_client.api.query_api import QueryApi
    
else:
    from lazy_imports import LazyModule, as_package, load

    load(
        LazyModule(
            *as_package(__file__),
            """# import apis into api package
from openapi_client.api.auth_api import AuthApi
from openapi_client.api.body_api import BodyApi
from openapi_client.api.form_api import FormApi
from openapi_client.api.header_api import HeaderApi
from openapi_client.api.path_api import PathApi
from openapi_client.api.query_api import QueryApi

""",
            name=__name__,
            doc=__doc__,
        )
    )
