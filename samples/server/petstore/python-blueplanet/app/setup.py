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

# mirrors requirements.txt so `python setup.py install` produces the same
# runnable environment: connexion 2.x with the swagger-ui extra (/ui/ needs
# swagger-ui-bundle), the environment markers for python 3.4/3.5 vs newer,
# and the werkzeug peg old connexion releases require on old pythons
REQUIRES = [
    'connexion[swagger-ui] >= 2.6.0; python_version>="3.6"',
    'connexion[swagger-ui] <= 2.3.0; python_version=="3.5" or python_version=="3.4"',
    'connexion[swagger-ui] <= 2.14.2; python_version>"3.4"',
    'werkzeug == 0.16.1; python_version=="3.5" or python_version=="3.4"',
    "swagger-ui-bundle >= 0.0.2",
    "python_dateutil == 2.6.0",
    "setuptools >= 21.0.0",
    "bp2hookutil == 3.3.0",
    "plansdk",
    "twigjack",
]

setup(
    name=NAME,
    version=VERSION,
    description="OpenAPI Petstore",
    author_email="",
    url="",
    keywords=["Swagger", "OpenAPI Petstore"],
    install_requires=REQUIRES,
    packages=find_packages(),
    package_data={'': ['swagger/swagger.yaml']},
    include_package_data=True,
    entry_points={
        'console_scripts': ['openapi_server=openapi_server.__main__:main']},
    long_description="""\
    This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.
    """
)

