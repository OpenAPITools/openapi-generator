import datetime

import typing
from typing import Union

T = typing.TypeVar('T')
Class = typing.Type[T]


def _deserialize(data: Union[dict, list, str], klass: Union[Class, str]) -> Union[dict, list, Class, int, float, str, bool, datetime.date, datetime.datetime]:
    """Deserializes dict, list, str into an object.

    :param data: dict, list or str.
    :param klass: class literal, or string of class name.

    :return: object.
    """
    if data is None:
        return None

    if klass in (int, float, str, bool):
        return _deserialize_primitive(data, klass)
    elif klass == object:
        return _deserialize_object(data)
    elif klass == datetime.date:
        return deserialize_date(data)
    elif klass == datetime.datetime:
        return deserialize_datetime(data)
    elif type(klass) == typing.GenericMeta:
        if klass.__extra__ == list:
            return _deserialize_list(data, klass.__args__[0])
        if klass.__extra__ == dict:
            return _deserialize_dict(data, klass.__args__[1])
    else:
        return deserialize_model(data, klass)


def _deserialize_primitive(data, klass: Class) -> Union[Class, int, float, str, bool]:
    """Deserializes to primitive type.

    :param data: data to deserialize.
    :param klass: class literal.

    :return: int, float, str, bool.
    """
    try:
        value = klass(data)
    except (UnicodeEncodeError, TypeError):
        value = data
    return value


def _deserialize_object(value: T) -> T:
    """Return an original value.

    :return: object.
    """
    return value


def deserialize_date(string: str) -> datetime.date:
    """Deserializes string to date.

    :param string: str.
    :return: date.
    """
    try:
        from dateutil.parser import parse
        return parse(string).date()
    except ImportError:
        return string


def deserialize_datetime(string: str) -> datetime.datetime:
    """Deserializes string to datetime.

    The string should be in iso8601 datetime format.

    :param string: str.
    :return: datetime.
    """
    try:
        from dateutil.parser import parse
        return parse(string)
    except ImportError:
        return string


def deserialize_model(data: Union[dict, list], klass: T) -> T:
    """Deserializes list or dict to model.

    :param data: dict, list.
    :param klass: class literal.
    :return: model object.
    """
    instance = klass()

    if not instance.openapi_types:
        return data

    if data is not None and isinstance(data, (list, dict)):
        for attr, attr_type in instance.openapi_types.items():
            attr_key = instance.attribute_map[attr]
            if attr_key in data:
                value = data[attr_key]
                setattr(instance, attr, _deserialize(value, attr_type))

    return instance


def _deserialize_list(data: list, boxed_type) -> list:
    """Deserializes a list and its elements.

    :param data: list to deserialize.
    :param boxed_type: class literal.

    :return: deserialized list.
    """
    return [_deserialize(sub_data, boxed_type) for sub_data in data]


def _deserialize_dict(data: dict, boxed_type) -> dict:
    """Deserializes a dict and its elements.

    :param data: dict to deserialize.
    :param boxed_type: class literal.

    :return: deserialized dict.
    """
    return {k: _deserialize(v, boxed_type) for k, v in data.items()}
