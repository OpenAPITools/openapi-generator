import pprint

import six

from openapi_server import util


class Model(object):
    # openapiTypes: The key is attribute name and the
    # value is attribute type.
    openapi_types = {}

    # attributeMap: The key is attribute name and the
    # value is json key in definition.
    attribute_map = {}

    @classmethod
    def from_dict(cls: typing.Type[T], dikt, json_keys=True) -> T:
        """Returns the dict as a model"""
        return util.deserialize_model(dikt, cls, json_keys=True)

    def to_dict(self, json_keys=True):
        """Returns the model properties as a dict
        
        :param json_keys: Defines if json_keys are used in dict
        :type: bool
        :rtype: dict
        """
        result = {}

        for attr, _ in six.iteritems(self.openapi_types):
            value = getattr(self, attr)
<<<<<<< HEAD
            dict_attr = self.attribute_map[attr] if json_keys else attr
=======
            dict_attr = self.attribute_map[attr]
>>>>>>> created petstore examples
            if isinstance(value, list):
                result[dict_attr] = list(map(
                    lambda x: x.to_dict() if hasattr(x, "to_dict") else x,
                    value
                ))
            elif hasattr(value, "to_dict"):
                result[dict_attr] = value.to_dict()
            elif isinstance(value, dict):
                result[dict_attr] = dict(map(
                    lambda item: (item[0], item[1].to_dict())
                    if hasattr(item[1], "to_dict") else item,
                    value.items()
                ))
            else:
                result[dict_attr] = value

        return result

    def to_str(self):
        """Returns the string representation of the model

        :rtype: str
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
