//ignore_for_file: invalid_annotation_target
import 'package:freezed_annotation/freezed_annotation.dart';
import 'package:dio/dio.dart';
import 'dart:convert';

part 'models.freezed.dart';
part 'models.g.dart';

part 'primitive_union_types.dart';
part 'additional_properties_class.dart';part 'all_of_with_single_ref.dart';part 'animal.dart';part 'api_response.dart';part 'array_of_array_of_number_only.dart';part 'array_of_number_only.dart';part 'array_test.dart';part 'capitalization.dart';part 'cat.dart';part 'category.dart';part 'child_with_nullable.dart';part 'class_model.dart';part 'deprecated_object.dart';part 'dog.dart';part 'enum_arrays.dart';part 'enum_test.dart';part 'fake_big_decimal_map200_response.dart';part 'file_schema_test_class.dart';part 'foo.dart';part 'foo_get_default_response.dart';part 'format_test.dart';part 'has_only_read_only.dart';part 'health_check_result.dart';part 'map_test.dart';part 'mixed_properties_and_additional_properties_class.dart';part 'model200_response.dart';part 'model_client.dart';part 'model_enum_class.dart';part 'model_file.dart';part 'model_list.dart';part 'model_return.dart';part 'name.dart';part 'nullable_class.dart';part 'number_only.dart';part 'object_with_deprecated_fields.dart';part 'order.dart';part 'outer_composite.dart';part 'outer_enum.dart';part 'outer_enum_default_value.dart';part 'outer_enum_integer.dart';part 'outer_enum_integer_default_value.dart';part 'outer_object_with_enum_property.dart';part 'parent_with_nullable.dart';part 'pet.dart';part 'read_only_first.dart';part 'single_ref_type.dart';part 'special_model_name.dart';part 'tag.dart';part 'test_inline_freeform_additional_properties_request.dart';part 'user.dart';

/// A typedef used in the deserialization of OneOf and AnyOf
/// models when no discriminator mapping is provided.
typedef FromJsonMethodType<T> = T Function(Map<String, dynamic>);

/// Deserialization error types for OneOf and AnyOf types.
enum DeserializationErrorType {
    MoreThanOneTypeSatisfied,
    UnKnownType,
}