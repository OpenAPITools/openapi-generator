//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_import

import 'package:built_collection/built_collection.dart';
import 'package:built_value/json_object.dart';
import 'package:built_value/serializer.dart';
import 'package:built_value/standard_json_plugin.dart';
import 'package:built_value/iso_8601_date_time_serializer.dart';
import 'package:openapi/src/date_serializer.dart';
import 'package:openapi/src/model/date.dart';

import 'package:openapi/src/model/additional_properties_class.dart';
import 'package:openapi/src/model/animal.dart';
import 'package:openapi/src/model/api_response.dart';
import 'package:openapi/src/model/array_of_array_of_number_only.dart';
import 'package:openapi/src/model/array_of_number_only.dart';
import 'package:openapi/src/model/array_test.dart';
import 'package:openapi/src/model/capitalization.dart';
import 'package:openapi/src/model/cat.dart';
import 'package:openapi/src/model/cat_all_of.dart';
import 'package:openapi/src/model/category.dart';
import 'package:openapi/src/model/class_model.dart';
import 'package:openapi/src/model/deprecated_object.dart';
import 'package:openapi/src/model/dog.dart';
import 'package:openapi/src/model/dog_all_of.dart';
import 'package:openapi/src/model/enum_arrays.dart';
import 'package:openapi/src/model/enum_test.dart';
import 'package:openapi/src/model/file_schema_test_class.dart';
import 'package:openapi/src/model/foo.dart';
import 'package:openapi/src/model/format_test.dart';
import 'package:openapi/src/model/has_only_read_only.dart';
import 'package:openapi/src/model/health_check_result.dart';
import 'package:openapi/src/model/inline_response_default.dart';
import 'package:openapi/src/model/map_test.dart';
import 'package:openapi/src/model/mixed_properties_and_additional_properties_class.dart';
import 'package:openapi/src/model/model200_response.dart';
import 'package:openapi/src/model/model_client.dart';
import 'package:openapi/src/model/model_enum_class.dart';
import 'package:openapi/src/model/model_file.dart';
import 'package:openapi/src/model/model_list.dart';
import 'package:openapi/src/model/model_return.dart';
import 'package:openapi/src/model/name.dart';
import 'package:openapi/src/model/nullable_class.dart';
import 'package:openapi/src/model/number_only.dart';
import 'package:openapi/src/model/object_with_deprecated_fields.dart';
import 'package:openapi/src/model/order.dart';
import 'package:openapi/src/model/outer_composite.dart';
import 'package:openapi/src/model/outer_enum.dart';
import 'package:openapi/src/model/outer_enum_default_value.dart';
import 'package:openapi/src/model/outer_enum_integer.dart';
import 'package:openapi/src/model/outer_enum_integer_default_value.dart';
import 'package:openapi/src/model/outer_object_with_enum_property.dart';
import 'package:openapi/src/model/pet.dart';
import 'package:openapi/src/model/read_only_first.dart';
import 'package:openapi/src/model/special_model_name.dart';
import 'package:openapi/src/model/tag.dart';
import 'package:openapi/src/model/user.dart';

part 'serializers.g.dart';

@SerializersFor([
  AdditionalPropertiesClass,
  Animal,
  ApiResponse,
  ArrayOfArrayOfNumberOnly,
  ArrayOfNumberOnly,
  ArrayTest,
  Capitalization,
  Cat,
  CatAllOf,
  Category,
  ClassModel,
  DeprecatedObject,
  Dog,
  DogAllOf,
  EnumArrays,
  EnumTest,
  FileSchemaTestClass,
  Foo,
  FormatTest,
  HasOnlyReadOnly,
  HealthCheckResult,
  InlineResponseDefault,
  MapTest,
  MixedPropertiesAndAdditionalPropertiesClass,
  Model200Response,
  ModelClient,
  ModelEnumClass,
  ModelFile,
  ModelList,
  ModelReturn,
  Name,
  NullableClass,
  NumberOnly,
  ObjectWithDeprecatedFields,
  Order,
  OuterComposite,
  OuterEnum,
  OuterEnumDefaultValue,
  OuterEnumInteger,
  OuterEnumIntegerDefaultValue,
  OuterObjectWithEnumProperty,
  Pet,
  ReadOnlyFirst,
  SpecialModelName,
  Tag,
  User,
])
Serializers serializers = (_$serializers.toBuilder()
      ..addBuilderFactory(
        const FullType(BuiltMap, [FullType(String), FullType(String)]),
        () => MapBuilder<String, String>(),
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(User)]),
        () => ListBuilder<User>(),
      )
      ..addBuilderFactory(
        const FullType(BuiltSet, [FullType(String)]),
        () => SetBuilder<String>(),
      )
      ..addBuilderFactory(
        const FullType(BuiltSet, [FullType(Pet)]),
        () => SetBuilder<Pet>(),
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(Pet)]),
        () => ListBuilder<Pet>(),
      )
      ..addBuilderFactory(
        const FullType(BuiltMap, [FullType(String), FullType(int)]),
        () => MapBuilder<String, int>(),
      )
      ..addBuilderFactory(
        const FullType(BuiltList, [FullType(String)]),
        () => ListBuilder<String>(),
      )
      ..add(const DateSerializer())
      ..add(Iso8601DateTimeSerializer()))
    .build();

Serializers standardSerializers =
    (serializers.toBuilder()..addPlugin(StandardJsonPlugin())).build();
