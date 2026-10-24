
from setuptools import setup, find_packages

NAME = "openapi_server"
VERSION = "1.0.0"


REQUIRES = [
    "connexion[swagger-ui]>=2.15.1,<3.0.0",
    "Flask>=2.3.3,<3.0.0",
    "python-dateutil>=2.8.2",
]

setup(
    name=NAME,
    version=VERSION,
    description="OpenAPI Petstore",
    author_email="",
    url="",
    keywords=["OpenAPI", "OpenAPI Petstore"],
    python_requires=">=3.11",
    install_requires=REQUIRES,
    packages=find_packages(),
    package_data={
        "": [
            "openapi/openapi.yaml"
        ]
    },
    include_package_data=True,
    entry_points={
        "console_scripts": [
            "openapi_server=openapi_server.__main__:main"
        ]
    },
    long_description="""\
    This is a sample server Petstore server. For this sample, you can use the api key &#x60;special-key&#x60; to test the authorization filters.
    """
)

