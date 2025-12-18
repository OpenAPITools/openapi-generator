from connexion.frameworks.flask import FlaskJSONProvider

from openapi_server.models.base_model import Model


class JSONEncoder(FlaskJSONProvider):
    include_nulls = False

    def default(self, o):
        if isinstance(o, Model):
            dikt = {}
            for attr in o.openapi_types:
                value = getattr(o, attr)
                if value is None and not self.include_nulls:
                    continue
                attr = o.attribute_map[attr]
                dikt[attr] = value
            return dikt
        return FlaskJSONProvider.default(self, o)
