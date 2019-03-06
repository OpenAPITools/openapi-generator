try:
    import simplejson as json
except ImportError:
    import json


class _CompactJSON(object):
    """Wrapper around json module that strips whitespace."""

    @staticmethod
    def loads(payload):
        return json.loads(payload)

    @staticmethod
    def dumps(obj, **kwargs):
        kwargs.setdefault("ensure_ascii", False)
        kwargs.setdefault("separators", (",", ":"))
        return json.dumps(obj, **kwargs)
