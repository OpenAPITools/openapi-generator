import asyncio
import functools
import warnings
import jinja2
from collections import Mapping
from aiohttp import web
from aiohttp.abc import AbstractView
from .helpers import GLOBAL_HELPERS


__version__ = '1.1.0'

__all__ = ('setup', 'get_env', 'render_template', 'render_string', 'template')


APP_CONTEXT_PROCESSORS_KEY = 'aiohttp_jinja2_context_processors'
APP_KEY = 'aiohttp_jinja2_environment'
REQUEST_CONTEXT_KEY = 'aiohttp_jinja2_context'


def setup(app, *args, app_key=APP_KEY, context_processors=(),
          filters=None, default_helpers=True, autoescape=True,
          **kwargs):
    env = jinja2.Environment(*args, autoescape=autoescape, **kwargs)
    if default_helpers:
        env.globals.update(GLOBAL_HELPERS)
    if filters is not None:
        env.filters.update(filters)
    app[app_key] = env
    if context_processors:
        app[APP_CONTEXT_PROCESSORS_KEY] = context_processors
        app.middlewares.append(context_processors_middleware)

    env.globals['app'] = app

    return env


def get_env(app, *, app_key=APP_KEY):
    return app.get(app_key)


def render_string(template_name, request, context, *, app_key=APP_KEY):
    env = request.config_dict.get(app_key)
    if env is None:
        text = ("Template engine is not initialized, "
                "call aiohttp_jinja2.setup(..., app_key={}) first"
                "".format(app_key))
        # in order to see meaningful exception message both: on console
        # output and rendered page we add same message to *reason* and
        # *text* arguments.
        raise web.HTTPInternalServerError(reason=text, text=text)
    try:
        template = env.get_template(template_name)
    except jinja2.TemplateNotFound as e:
        text = "Template '{}' not found".format(template_name)
        raise web.HTTPInternalServerError(reason=text, text=text) from e
    if not isinstance(context, Mapping):
        text = "context should be mapping, not {}".format(type(context))
        # same reason as above
        raise web.HTTPInternalServerError(reason=text, text=text)
    if request.get(REQUEST_CONTEXT_KEY):
        context = dict(request[REQUEST_CONTEXT_KEY], **context)
    text = template.render(context)
    return text


def render_template(template_name, request, context, *,
                    app_key=APP_KEY, encoding='utf-8', status=200):
    response = web.Response(status=status)
    if context is None:
        context = {}
    text = render_string(template_name, request, context, app_key=app_key)
    response.content_type = 'text/html'
    response.charset = encoding
    response.text = text
    return response


def template(template_name, *, app_key=APP_KEY, encoding='utf-8', status=200):

    def wrapper(func):
        @functools.wraps(func)
        async def wrapped(*args):
            if asyncio.iscoroutinefunction(func):
                coro = func
            else:
                warnings.warn("Bare functions are deprecated, "
                              "use async ones", DeprecationWarning)
                coro = asyncio.coroutine(func)
            context = await coro(*args)
            if isinstance(context, web.StreamResponse):
                return context

            # Supports class based views see web.View
            if isinstance(args[0], AbstractView):
                request = args[0].request
            else:
                request = args[-1]

            response = render_template(template_name, request, context,
                                       app_key=app_key, encoding=encoding)
            response.set_status(status)
            return response
        return wrapped
    return wrapper


@web.middleware
async def context_processors_middleware(request, handler):

    if REQUEST_CONTEXT_KEY not in request:
        request[REQUEST_CONTEXT_KEY] = {}
    for processor in request.config_dict[APP_CONTEXT_PROCESSORS_KEY]:
        request[REQUEST_CONTEXT_KEY].update(await processor(request))
    return await handler(request)


async def request_processor(request):
    return {'request': request}
