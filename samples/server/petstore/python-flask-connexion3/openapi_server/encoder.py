import datetime
import decimal
import json
import uuid

from werkzeug.http import http_date

from openapi_server.models.base_model import Model


class JSONEncoder(json.JSONEncoder):
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
        # Connexion 3 removed FlaskJSONEncoder, which is what used to
        # give us this handling for free -- replicate it here so
        # date/datetime/Decimal/UUID-typed model attributes (e.g. an
        # OpenAPI `date-time` field) still serialize instead of raising
        # TypeError. http_date() matches Flask's own default encoder
        # behavior for date/datetime (RFC 2822, not ISO 8601).
        if isinstance(o, datetime.date):
            return http_date(o)
        if isinstance(o, (decimal.Decimal, uuid.UUID)):
            return str(o)
        return json.JSONEncoder.default(self, o)
