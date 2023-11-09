import 'package:dio/dio.dart';
import 'dart:async';
import 'dart:convert';
import 'package:openapi/models.dart';
import 'dart:typed_data';

/// Format the given form parameter object into something that Dio can handle.
Future<Object?> encodeFormParameter<T extends Object?>(
  SerializationRepositoryBase repository,
  T value,
  TypeInfo type,
) async {
  return repository.serialize(
      value,
      type,
  );
}

Future<String?> encodeStringParameter<T extends Object?>(
  SerializationRepositoryBase repository,
  T value,
  TypeInfo type,
) async {  
  return (await repository.serialize(value, type)).toString();
}

Future<Object?> encodeBodyParameter<T extends Object?>(
  SerializationRepositoryBase repository,
  T value,
  TypeInfo type,
) async {
    return await repository.serialize(
        value,
        type,
    );
}

Future<Object?> encodeQueryParameter<T extends Object?>(
  SerializationRepositoryBase repository,
  T value,
  TypeInfo type, {
  ListFormat format = ListFormat.multi,
}) async {
    if (value == null) {
        return null;
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

Future<ListParam<Object?>> encodeCollectionQueryParameter<T extends Object?>(
  SerializationRepositoryBase repository,
  Iterable<T> value,
  TypeInfo type, {
  ListFormat format = ListFormat.multi,
}) async {
    final serialized = await repository.serialize(
        value,
        type,
    );
    if (serialized == null) {
      return ListParam([], format);
    }
    if (serialized is Iterable) {
        return ListParam(serialized.toList(), format);
    }
    throw ArgumentError('Invalid value passed to encodeCollectionQueryParameter');
}

Future<TOutput?> decodeResponse<TOutput, TInput extends Object>(SerializationRepositoryBase repository, TInput value, TypeInfo type) async {
  return await repository.deserialize(value, type);
}