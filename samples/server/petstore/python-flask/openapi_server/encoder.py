from connexion.apps.flask_app import FlaskJSONProvider

from openapi_server.models.base_model import Model


def _model_to_dict(model, include_nulls):
    dikt = {}
    for attr in model.openapi_types:
        value = getattr(model, attr)
        if value is None and not include_nulls:
            continue
        dikt[model.attribute_map[attr]] = value
    return dikt


class JSONProvider(FlaskJSONProvider):
    include_nulls = False

    def default(self, o):
        if isinstance(o, Model):
            return _model_to_dict(o, self.include_nulls)
        return super().default(o)
