pytest-aiohttp
==============

pytest plugin for aiohttp support

The library allows to use `aiohttp pytest plugin
<http://aiohttp.readthedocs.io/en/stable/testing.html#pytest-example>`_
without need for implicitly loading it like `pytest_plugins =
'aiohttp.pytest_plugin'`.




Just run:

.. code-block:: console

    $ pip install pytest-aiohttp

and write tests with the plugin support:

.. code-block:: python

    from aiohttp import web

    async def hello(request):
        return web.Response(body=b'Hello, world')

    def create_app(loop):
        app = web.Application(loop=loop)
        app.router.add_route('GET', '/', hello)
        return app

    async def test_hello(test_client):
        client = await test_client(create_app)
        resp = await client.get('/')
        assert resp.status == 200
        text = await resp.text()
        assert 'Hello, world' in text

CHANGES
=======

0.2.0 (2017-11-30)
------------------

- Fix backward incompatibility changes introduced by `pytest` 3.3+

0.1.3 (2016-09-08)
------------------

- Add MANIFEST.in file

0.1.2 (2016-08-07)
------------------

- Fix README markup

0.1.1 (2016-07-22)
------------------

- Fix an url in setup.py

0.1.0 (2016-07-22)
------------------

- Initial release

