//ignore_for_file: invalid_annotation_target
import 'package:freezed_annotation/freezed_annotation.dart';
import 'package:dio/dio.dart';
import 'dart:convert';

part 'models.freezed.dart';
part 'models.g.dart';

part 'primitive_union_types.dart';
part 'addressable.dart';part 'animal.dart';part 'apple.dart';part 'banana.dart';part 'bar.dart';part 'bar_create.dart';part 'bar_ref.dart';part 'bar_ref_or_value.dart';part 'cat.dart';part 'dog.dart';part 'entity.dart';part 'entity_ref.dart';part 'extensible.dart';part 'foo.dart';part 'foo_ref.dart';part 'foo_ref_or_value.dart';part 'fruit.dart';part 'fruit_type.dart';part 'pasta.dart';part 'pizza.dart';part 'pizza_speziale.dart';

/// A typedef used in the deserialization of OneOf and AnyOf
/// models when no discriminator mapping is provided.
typedef FromJsonMethodType<T> = T Function(Map<String, dynamic>);

/// Deserialization error types for OneOf and AnyOf types.
enum DeserializationErrorType {
    MoreThanOneTypeSatisfied,
    UnKnownType,
}