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
    FutureOr<T> deserialize<T>(Object? value, TypeInfo targetTypeInfo, {Object? context}) {      
      if (value is T) {
        return value;
      }
      return serializers.deserialize(
        value, 
        specifiedType: typeInfoToFullType(targetTypeInfo),
      ) as T;
    }
    
    @override
    FutureOr<Object?> serialize<T>(T src, TypeInfo inputTypeInfo, {Object? context}) {
      if (src == null) {
        return null;
      }
      return serializers.serialize(
        src,
        specifiedType: typeInfoToFullType(inputTypeInfo),
      ) as Object;
    }
}
