import datetime
import decimal
import json
import uuid

from werkzeug.http import http_date

from openapi_server.models.base_model import Model


def _model_to_dict(model, include_nulls):
    dikt = {}
    for attr in model.openapi_types:
        value = getattr(model, attr)
        if value is None and not include_nulls:
            continue
        dikt[model.attribute_map[attr]] = value
    return dikt


class JSONEncoder(json.JSONEncoder):
    include_nulls = False

    def default(self, o):
        if isinstance(o, Model):
            return _model_to_dict(o, self.include_nulls)
        # Preserve this adapter's HTTP-date and string representations.
        # Connexion 2 delegates special types to its JSON provider instead.
        if isinstance(o, datetime.date):
            return http_date(o)
        if isinstance(o, (decimal.Decimal, uuid.UUID)):
            return str(o)
        return json.JSONEncoder.default(self, o)
