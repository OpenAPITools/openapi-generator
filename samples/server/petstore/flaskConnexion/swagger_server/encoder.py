from connexion.decorators import produces
from six import iteritems
from swagger_server.models.base_model_ import Model


class JSONEncoder(produces.JSONEncoder):
    include_nulls = False

    def default(self, o):
        if isinstance(o, Model):
            dikt = {}
            for attr, _ in iteritems(o.swagger_types):
                value = getattr(o, attr)
                if value is None and not self.include_nulls:
                    continue
                attr = o.attribute_map[attr]
                dikt[attr] = value
            return dikt
        return produces.JSONEncoder.default(self, o)