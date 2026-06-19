import pprint

import typing

from openapi_server import util

T = typing.TypeVar('T')


class Model(object):
    # openapiTypes: The key is attribute name and the
    # value is attribute type.
    openapi_types = {}

    # attributeMap: The key is attribute name and the
    # value is json key in definition.
    attribute_map = {}

    @classmethod
    def from_dict(cls: T, dikt: dict) -> T:
        """Returns the dict as a model"""
        return util.deserialize_model(dikt, cls)

    def to_dict(self) -> dict:
        """Returns the model properties as a dict
        """
        result = {}

        for attr_key, json_key in self.attribute_map.items():
            value = getattr(self, attr_key)
            if value is None:
                continue
            if isinstance(value, list):
                result[json_key] = list(map(
                    lambda x: x.to_dict() if hasattr(x, "to_dict") else x,
                    value
                ))
            elif hasattr(value, "to_dict"):
                result[json_key] = value.to_dict()
            elif isinstance(value, dict):
                result[json_key] = dict(map(
                    lambda item: (item[0], item[1].to_dict())
                    if hasattr(item[1], "to_dict") else item,
                    value.items()
                ))
            else:
                result[json_key] = value

        return result

    def to_str(self) -> str:
        """Returns the string representation of the model
        """
        return pprint.pformat(self.to_dict())

    def __repr__(self):
        """For `print` and `pprint`"""
        return self.to_str()

    def __eq__(self, other):
        """Returns true if both objects are equal"""
        return self.__dict__ == other.__dict__

    def __ne__(self, other):
        """Returns true if both objects are not equal"""
        return not self == other
