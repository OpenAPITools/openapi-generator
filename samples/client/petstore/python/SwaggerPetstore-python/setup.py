import sys
from setuptools import setup, find_packages



# To install the library, open a Terminal shell, then run this
# file by typing:
#
# python setup.py install
#
# You need to have the setuptools module installed.
# Try reading the setuptools documentation:
# http://pypi.python.org/pypi/setuptools

REQUIRES = ["urllib3 >= 1.10", "six >= 1.9", "certifi"]

setup(
    name="SwaggerPetstore",
    version="1.0.0",
    description="Swagger Petstore",
    author_email="apiteam@wordnik.com",
    url="",
    keywords=["Swagger", "Swagger Petstore"],
    install_requires=REQUIRES,
    packages=find_packages(),
    include_package_data=True,
    long_description="""\
    This is a sample server Petstore server.  You can find out more about Swagger at &lt;a href=\&quot;http://swagger.io\&quot;&gt;http://swagger.io&lt;/a&gt; or on irc.freenode.net, #swagger.  For this sample, you can use the api key \&quot;special-key\&quot; to test the authorization filters
    """
)










