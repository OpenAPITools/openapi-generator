# coding: utf-8

import sys
from setuptools import setup, find_packages

NAME = "swagger_client"
VERSION = "1.0.0"



# To install the library, run the following
#
# python setup.py install
#
# prerequisite: setuptools
# http://pypi.python.org/pypi/setuptools

REQUIRES = ["urllib3 >= 1.10", "six >= 1.9", "certifi", "python-dateutil"]

setup(
    name=NAME,
    version=VERSION,
    description="Swagger Petstore",
    author_email="apiteam@swagger.io",
    url="",
    keywords=["Swagger", "Swagger Petstore"],
    install_requires=REQUIRES,
    packages=find_packages(),
    include_package_data=True,
    long_description="""\
    This is a sample server Petstore server.  You can find out more about Swagger at &lt;a href=\&quot;http://swagger.io\&quot;&gt;http://swagger.io&lt;/a&gt; or on irc.freenode.net, #swagger.  For this sample, you can use the api key \&quot;special-key\&quot; to test the authorization filters
    """
)


