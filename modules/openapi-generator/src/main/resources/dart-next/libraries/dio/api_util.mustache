import 'package:dio/dio.dart';
import 'dart:convert';
import 'package:openapi/models.dart';
import 'dart:typed_data';

/// Format the given form parameter object into something that Dio can handle.
/// Returns primitive or String.
/// Returns List/Map if the value is BuildList/BuiltMap.
Object encodeFormParameter<T>(
  SerializationRepositoryBase repository,
  T value,
  TypeInfo type,
) {
    if (value == null) {
        return '';
    }
    final serialized = repository.serialize(
        value,
        type,
    );    
    return serialized;
}

String encodeStringParameter<T>(
  SerializationRepositoryBase repository,
  T value,
  TypeInfo type,
) {
  return repository.serialize(value, type).toString();
}

Object encodeBodyParameter<T>(
  SerializationRepositoryBase repository,
  T value,
  TypeInfo type,
) {
    if (value == null) {
        return '';
    }
    final serialized = repository.serialize(
        value,
        type,
    );    
    return serialized;
}

Object encodeQueryParameter<T>(
  SerializationRepositoryBase repository,
  T value,
  TypeInfo type, {
  ListFormat format = ListFormat.multi,
}) {
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
    final serialized = repository.serialize(
        value,
        type,
    );
    return serialized;
}

ListParam<Object?> encodeCollectionQueryParameter<T>(
  SerializationRepositoryBase repository,
  Iterable<T> value,
  TypeInfo type, {
  ListFormat format = ListFormat.multi,
}) {
    final serialized = repository.serialize(
        value,
        type,
    );
    if (serialized is Iterable) {
        return ListParam(serialized.toList(), format);
    }
    throw ArgumentError('Invalid value passed to encodeCollectionQueryParameter');
}

TOutput decodeResponse<TOutput, TInput extends Object>(SerializationRepositoryBase repository, TInput value, TypeInfo type) {
  return repository.deserialize(value, type);
}