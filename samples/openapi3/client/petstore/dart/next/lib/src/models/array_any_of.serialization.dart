// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'array_any_of.dart';

//class serialization

Map<String, dynamic> _$ArrayAnyOfToMap(ArrayAnyOf instance) {
  final _reflection = ArrayAnyOfReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
  };
}

ArrayAnyOf _$ArrayAnyOfFromMap(Map<String, dynamic> src) {
  const _reflection = ArrayAnyOfReflection.instance;
  return ArrayAnyOf.$all(
    additionalProperties: AdditionalProperties(
        src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, (v)))),
    anyOf0: UndefinedWrapper.undefined(),
    anyOf1: UndefinedWrapper.undefined(),
  );
}

bool _$ArrayAnyOfCanFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayAnyOfReflection.instance;

  if (!src
      .except(_reflection.knownKeys)
      .values
      .every((v) => v == null ? true : (true))) {
    return false;
  }

  final anyOfs = [];
  final validAnyOfs = anyOfs.where((x) => x()).take(1).length;
  if (validAnyOfs == 0) {
    return false;
  }

  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
ArrayAnyOf _$ArrayAnyOfDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayAnyOfFromMap(src);
  } else {
    final v = src;
    return ArrayAnyOf.$all(
      anyOf0: (v == null
              ? false
              : ((v is int || (int.tryParse(v.toString()) != null))))
          ? UndefinedWrapper(((v is int ? v as int : int.parse(v.toString()))))
          : UndefinedWrapper.undefined(),
      anyOf1: (v == null
              ? false
              : (v is List &&
                  v.every((v) => v == null ? false : ((v is String)))))
          ? UndefinedWrapper((v as List)
              .map((v) => ((v is String
                  ? v as String
                  : throwArgumentMismatch(String, v))))
              .toList())
          : UndefinedWrapper.undefined(),
      // Additional Properties only make sense if the src is a Map<String, dynamic>
      additionalProperties: AdditionalProperties(),
    );
  }
}

/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ArrayAnyOfCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayAnyOfCanFromMap(src);
  } else {
    final v = src;
    final anyOfs = [
      () => v == null
          ? false
          : ((v is int || (int.tryParse(v.toString()) != null))),
      () => v == null
          ? false
          : (v is List && v.every((v) => v == null ? false : ((v is String)))),
    ];
    final validAnyOfs = anyOfs.where((x) => x()).take(1).length;
    if (validAnyOfs > 0) {
      return true;
    }
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$ArrayAnyOfSerialize(ArrayAnyOf src) {
  Object? initialResult = () {
    if (src.anyOf0.isDefined) {
      final v = src.anyOf0.valueRequired;
      return v;
    }

    if (src.anyOf1.isDefined) {
      final v = src.anyOf1.valueRequired;
      return v.map((v) => v).toList();
    }

    return null;
  }();
  if (initialResult is Map<String, Object?>) {
    return {
      ...src.additionalProperties,
      ...initialResult,
    };
  }
  return initialResult;
}

/*
XmlElement _$ArrayAnyOfToXml(ArrayAnyOf instance) {
  final reflection = ArrayAnyOfXmlReflection.instance;
  final result = XmlElement(
    XmlName(reflection.oasName, reflection.oasNamespace),
    //attributes
    [

    ],
    //elements
    [
    ],
  );
  return result;
}

ArrayAnyOf _$ArrayAnyOfFromXml(XmlElement src) {
  final reflection = ArrayAnyOfXmlReflection.instance;
  return ArrayAnyOf.$all(

  );
}
*/
