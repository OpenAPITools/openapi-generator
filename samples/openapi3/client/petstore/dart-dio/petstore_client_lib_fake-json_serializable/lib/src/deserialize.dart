import 'package:openapi/src/model/additional_properties_class.dart';
import 'package:openapi/src/model/all_of_with_single_ref.dart';
import 'package:openapi/src/model/animal.dart';
import 'package:openapi/src/model/api_response.dart';
import 'package:openapi/src/model/array_of_array_of_number_only.dart';
import 'package:openapi/src/model/array_of_number_only.dart';
import 'package:openapi/src/model/array_test.dart';
import 'package:openapi/src/model/capitalization.dart';
import 'package:openapi/src/model/cat.dart';
import 'package:openapi/src/model/category.dart';
import 'package:openapi/src/model/child_with_nullable.dart';
import 'package:openapi/src/model/class_model.dart';
import 'package:openapi/src/model/deprecated_object.dart';
import 'package:openapi/src/model/dog.dart';
import 'package:openapi/src/model/enum_arrays.dart';
import 'package:openapi/src/model/enum_test.dart';
import 'package:openapi/src/model/fake_big_decimal_map200_response.dart';
import 'package:openapi/src/model/file_schema_test_class.dart';
import 'package:openapi/src/model/foo.dart';
import 'package:openapi/src/model/foo_get_default_response.dart';
import 'package:openapi/src/model/format_test.dart';
import 'package:openapi/src/model/has_only_read_only.dart';
import 'package:openapi/src/model/health_check_result.dart';
import 'package:openapi/src/model/map_test.dart';
import 'package:openapi/src/model/mixed_properties_and_additional_properties_class.dart';
import 'package:openapi/src/model/model200_response.dart';
import 'package:openapi/src/model/model_client.dart';
import 'package:openapi/src/model/model_file.dart';
import 'package:openapi/src/model/model_list.dart';
import 'package:openapi/src/model/model_return.dart';
import 'package:openapi/src/model/name.dart';
import 'package:openapi/src/model/nullable_class.dart';
import 'package:openapi/src/model/number_only.dart';
import 'package:openapi/src/model/object_with_deprecated_fields.dart';
import 'package:openapi/src/model/order.dart';
import 'package:openapi/src/model/outer_composite.dart';
import 'package:openapi/src/model/outer_object_with_enum_property.dart';
import 'package:openapi/src/model/parent_with_nullable.dart';
import 'package:openapi/src/model/pet.dart';
import 'package:openapi/src/model/read_only_first.dart';
import 'package:openapi/src/model/special_model_name.dart';
import 'package:openapi/src/model/tag.dart';
import 'package:openapi/src/model/user.dart';

final _regList = RegExp(r'^List<(.*)>$');
final _regSet = RegExp(r'^Set<(.*)>$');
final _regMap = RegExp(r'^Map<String,(.*)>$');

  ReturnType deserialize<ReturnType, BaseType>(dynamic value, String targetType, {bool growable= true}) {
      switch (targetType) {
        case 'String':
          return '$value' as ReturnType;
        case 'int':
          return (value is int ? value : int.parse('$value')) as ReturnType;
        case 'bool':
          if (value is bool) {
            return value as ReturnType;
          }
          final valueString = '$value'.toLowerCase();
          return (valueString == 'true' || valueString == '1') as ReturnType;
        case 'double':
          return (value is double ? value : double.parse('$value')) as ReturnType;
        case 'AdditionalPropertiesClass':
          return AdditionalPropertiesClass.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'AllOfWithSingleRef':
          return AllOfWithSingleRef.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Animal':
          return Animal.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ApiResponse':
          return ApiResponse.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ArrayOfArrayOfNumberOnly':
          return ArrayOfArrayOfNumberOnly.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ArrayOfNumberOnly':
          return ArrayOfNumberOnly.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ArrayTest':
          return ArrayTest.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Capitalization':
          return Capitalization.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Cat':
          return Cat.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Category':
          return Category.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ChildWithNullable':
          return ChildWithNullable.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ClassModel':
          return ClassModel.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'DeprecatedObject':
          return DeprecatedObject.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Dog':
          return Dog.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'EnumArrays':
          return EnumArrays.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'EnumTest':
          return EnumTest.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FakeBigDecimalMap200Response':
          return FakeBigDecimalMap200Response.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FileSchemaTestClass':
          return FileSchemaTestClass.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Foo':
          return Foo.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FooGetDefaultResponse':
          return FooGetDefaultResponse.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FormatTest':
          return FormatTest.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'HasOnlyReadOnly':
          return HasOnlyReadOnly.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'HealthCheckResult':
          return HealthCheckResult.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'MapTest':
          return MapTest.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'MixedPropertiesAndAdditionalPropertiesClass':
          return MixedPropertiesAndAdditionalPropertiesClass.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Model200Response':
          return Model200Response.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ModelClient':
          return ModelClient.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ModelEnumClass':
          
          
        case 'ModelFile':
          return ModelFile.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ModelList':
          return ModelList.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ModelReturn':
          return ModelReturn.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Name':
          return Name.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'NullableClass':
          return NullableClass.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'NumberOnly':
          return NumberOnly.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ObjectWithDeprecatedFields':
          return ObjectWithDeprecatedFields.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Order':
          return Order.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'OuterComposite':
          return OuterComposite.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'OuterEnum':
          
          
        case 'OuterEnumDefaultValue':
          
          
        case 'OuterEnumInteger':
          
          
        case 'OuterEnumIntegerDefaultValue':
          
          
        case 'OuterObjectWithEnumProperty':
          return OuterObjectWithEnumProperty.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ParentWithNullable':
          return ParentWithNullable.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Pet':
          return Pet.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ReadOnlyFirst':
          return ReadOnlyFirst.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'SingleRefType':
          
          
        case 'SpecialModelName':
          return SpecialModelName.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Tag':
          return Tag.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'User':
          return User.fromJson(value as Map<String, dynamic>) as ReturnType;
        default:
          RegExpMatch? match;

          if (value is List && (match = _regList.firstMatch(targetType)) != null) {
            targetType = match![1]!; // ignore: parameter_assignments
            return value
              .map<BaseType>((dynamic v) => deserialize<BaseType, BaseType>(v, targetType, growable: growable))
              .toList(growable: growable) as ReturnType;
          }
          if (value is Set && (match = _regSet.firstMatch(targetType)) != null) {
            targetType = match![1]!; // ignore: parameter_assignments
            return value
              .map<BaseType>((dynamic v) => deserialize<BaseType, BaseType>(v, targetType, growable: growable))
              .toSet() as ReturnType;
          }
          if (value is Map && (match = _regMap.firstMatch(targetType)) != null) {
            targetType = match![1]!; // ignore: parameter_assignments
            return Map<dynamic, BaseType>.fromIterables(
              value.keys,
              value.values.map((dynamic v) => deserialize<BaseType, BaseType>(v, targetType, growable: growable)),
            ) as ReturnType;
          }
          break;
    } 
    throw Exception('Cannot deserialize');
  }