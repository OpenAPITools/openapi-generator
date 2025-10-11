//ignore_for_file: invalid_annotation_target
import 'package:freezed_annotation/freezed_annotation.dart';
import 'package:dio/dio.dart';
import 'dart:convert';

part 'models.freezed.dart';
part 'models.g.dart';

part 'primitive_union_types.dart';
part 'apple.dart';part 'banana.dart';part 'fruit.dart';part 'orange.dart';

/// A typedef used in the deserialization of OneOf and AnyOf
/// models when no discriminator mapping is provided.
typedef FromJsonMethodType<T> = T Function(Map<String, dynamic>);

/// Deserialization error types for OneOf and AnyOf types.
enum DeserializationErrorType {
    MoreThanOneTypeSatisfied,
    UnKnownType,
}