"""OpenAPI spec validator managers module."""
from contextlib import contextmanager


class VisitingManager(dict):
    """Visiting manager. Mark keys which being visited."""

    @contextmanager
    def visit(self, key):
        """Visits key and marks as visited.
        Support context manager interface.

        :param key: key being visited.
        """
        self[key] = key
        try:
            yield key
        finally:
            del self[key]


class ResolverManager(object):
    def __init__(self, resolver):
        self.resolver = resolver

    @contextmanager
    def in_scope(self, item, scope='x-scope'):
        if scope not in item:
            yield self.resolver
        else:
            saved_scope_stack = self.resolver._scopes_stack
            try:
                self.resolver._scopes_stack = item[scope]
                yield self.resolver
            finally:
                self.resolver._scopes_stack = saved_scope_stack
