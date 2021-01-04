# coding: utf-8

import sys
from setuptools import setup, find_packages

NAME = "openapi_server"
VERSION = "1.0.0"

# To install the library, run the following
#
# python setup.py install
#
# prerequisite: setuptools
# http://pypi.python.org/pypi/setuptools

REQUIRES = [
    "connexion==2.6.0",
    "swagger-ui-bundle==0.0.6",
    "aiohttp_jinja2==1.2.0",
]

setup(
    name=NAME,
    version=VERSION,
    description="OpenAPI Petstore",
    author_email="",
    url="",
    keywords=["OpenAPI", "OpenAPI Petstore"],
    install_requires=REQUIRES,
    packages=find_packages(),
    package_data={'': ['openapi/openapi.yaml']},
    include_package_data=True,
    entry_points={
        'console_scripts': ['openapi_server=openapi_server.__main__:main']},
    long_description="""\
    This is a sample server Petstore server. For this sample, you can use the api key &#x60;special-key&#x60; to test the authorization filters. NOTE - this spec differs from petstore.yaml in order to circumvent this known issue - https://github.com/OpenAPITools/openapi-generator/issues/3870 When the issue is resolved the bin/configs/python-aiohttp.yaml can be reverted to use petstore.yaml and this spec can be removed.
    """
)

