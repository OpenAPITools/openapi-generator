//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//


import 'dart:convert';
import 'dart:typed_data';

import 'package:built_collection/built_collection.dart';
import 'package:built_value/serializer.dart';
import 'package:dio/dio.dart';
import 'repository_base.dart';
import 'serializers.dart';

/// Converts our `TypeInfo` to `built_value`'s `FullType`
FullType typeInfoToFullType(TypeInfo info) {
  if (info.nullable) {
    return FullType.nullable(
        info.root, info.parameters.map(typeInfoToFullType).toList(),);
  } else {
    return FullType(
        info.root, info.parameters.map(typeInfoToFullType).toList(),);
  }
}

class BuiltValueJsonRepository extends SerializationRepositoryBase {
    final Serializers serializers;
    const BuiltValueJsonRepository(this.serializers);

    @override
    T deserialize<T>(Object value, TypeInfo targetTypeInfo, {Object? context}) {
      return serializers.deserialize(
        value, 
        specifiedType: typeInfoToFullType(targetTypeInfo),
      ) as T;
    }
    
    @override
    Object serialize<T>(T src, TypeInfo inputTypeInfo, {Object? context}) {
      return serializers.serialize(
        src,
        specifiedType: typeInfoToFullType(inputTypeInfo),
      ) as Object;
    }

    @override
    Object encodeFormParameter<T>(T src, TypeInfo inputTypeInfo, {Object? context,}) {
    
    }

    @override
    Object encodeQueryParameter<T>(T src, TypeInfo inputTypeInfo,
        {Object? context,}) {   
    }

    @override
    String encodeStringParameter<T>(T src, TypeInfo inputTypeInfo,
        {Object? context,}) {        
    }

   
}

/*
old api_util.dart for reference

/// Format the given form parameter object into something that Dio can handle.
/// Returns primitive or String.
/// Returns List/Map if the value is BuildList/BuiltMap.
dynamic encodeFormParameter(Serializers serializers, dynamic value, FullType type) {
  if (value == null) {
    return '';
  }
  if (value is String || value is num || value is bool) {
    return value;
  }
  final serialized = serializers.serialize(
    value as Object,
    specifiedType: type,
  );
  if (serialized is String) {
    return serialized;
  }
  if (value is BuiltList || value is BuiltSet || value is BuiltMap) {
    return serialized;
  }
  return json.encode(serialized);
}

dynamic encodeQueryParameter(
  Serializers serializers,
  dynamic value,
  FullType type,
) {
  if (value == null) {
    return '';
  }
  if (value is String || value is num || value is bool) {
    return value;
  }
  if (value is Uint8List) {
    // Currently not sure how to serialize this
    return value;
  }
  final serialized = serializers.serialize(
    value as Object,
    specifiedType: type,
  );
  if (serialized == null) {
    return '';
  }
  if (serialized is String) {
    return serialized;
  }
  return serialized;
}

ListParam<Object?> encodeCollectionQueryParameter<T>(
  Serializers serializers,
  dynamic value,
  FullType type, {
  ListFormat format = ListFormat.multi,
}) {
  final serialized = serializers.serialize(
    value as Object,
    specifiedType: type,
  );
  if (value is BuiltList<T> || value is BuiltSet<T>) {
    return ListParam(List.of((serialized as Iterable<Object?>).cast()), format);
  }
  throw ArgumentError('Invalid value passed to encodeCollectionQueryParameter');
}

*/