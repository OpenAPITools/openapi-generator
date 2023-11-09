import 'package:openapi/src/model/additional_properties_class.dart';
import 'package:openapi/src/model/addressable.dart';
import 'package:openapi/src/model/all_of_with_single_ref.dart';
import 'package:openapi/src/model/animal.dart';
import 'package:openapi/src/model/api_response.dart';
import 'package:openapi/src/model/apple.dart';
import 'package:openapi/src/model/apple_all_of_disc.dart';
import 'package:openapi/src/model/apple_grandparent_disc.dart';
import 'package:openapi/src/model/apple_one_of_disc.dart';
import 'package:openapi/src/model/apple_req_disc.dart';
import 'package:openapi/src/model/apple_variant1.dart';
import 'package:openapi/src/model/array_of_array_of_number_only.dart';
import 'package:openapi/src/model/array_of_number_only.dart';
import 'package:openapi/src/model/array_test.dart';
import 'package:openapi/src/model/banana.dart';
import 'package:openapi/src/model/banana_all_of_disc.dart';
import 'package:openapi/src/model/banana_grandparent_disc.dart';
import 'package:openapi/src/model/banana_one_of_disc.dart';
import 'package:openapi/src/model/banana_req_disc.dart';
import 'package:openapi/src/model/bar.dart';
import 'package:openapi/src/model/bar_create.dart';
import 'package:openapi/src/model/bar_ref.dart';
import 'package:openapi/src/model/bar_ref_or_value.dart';
import 'package:openapi/src/model/capitalization.dart';
import 'package:openapi/src/model/cat.dart';
import 'package:openapi/src/model/category.dart';
import 'package:openapi/src/model/class_model.dart';
import 'package:openapi/src/model/composed_disc_missing_from_properties.dart';
import 'package:openapi/src/model/composed_disc_optional_type_correct.dart';
import 'package:openapi/src/model/composed_disc_optional_type_inconsistent.dart';
import 'package:openapi/src/model/composed_disc_optional_type_incorrect.dart';
import 'package:openapi/src/model/composed_disc_required_inconsistent.dart';
import 'package:openapi/src/model/composed_disc_type_inconsistent.dart';
import 'package:openapi/src/model/composed_disc_type_incorrect.dart';
import 'package:openapi/src/model/deprecated_object.dart';
import 'package:openapi/src/model/disc_missing_from_properties.dart';
import 'package:openapi/src/model/disc_optional_type_correct.dart';
import 'package:openapi/src/model/disc_optional_type_incorrect.dart';
import 'package:openapi/src/model/disc_type_incorrect.dart';
import 'package:openapi/src/model/dog.dart';
import 'package:openapi/src/model/entity.dart';
import 'package:openapi/src/model/entity_ref.dart';
import 'package:openapi/src/model/enum_arrays.dart';
import 'package:openapi/src/model/enum_test.dart';
import 'package:openapi/src/model/extensible.dart';
import 'package:openapi/src/model/file_schema_test_class.dart';
import 'package:openapi/src/model/foo.dart';
import 'package:openapi/src/model/foo_basic_get_default_response.dart';
import 'package:openapi/src/model/foo_ref.dart';
import 'package:openapi/src/model/foo_ref_or_value.dart';
import 'package:openapi/src/model/format_test.dart';
import 'package:openapi/src/model/fruit.dart';
import 'package:openapi/src/model/fruit_all_of_disc.dart';
import 'package:openapi/src/model/fruit_any_of_disc.dart';
import 'package:openapi/src/model/fruit_grandparent_disc.dart';
import 'package:openapi/src/model/fruit_inline_disc.dart';
import 'package:openapi/src/model/fruit_inline_disc_one_of.dart';
import 'package:openapi/src/model/fruit_inline_disc_one_of1.dart';
import 'package:openapi/src/model/fruit_inline_inline_disc.dart';
import 'package:openapi/src/model/fruit_inline_inline_disc_one_of.dart';
import 'package:openapi/src/model/fruit_inline_inline_disc_one_of1.dart';
import 'package:openapi/src/model/fruit_inline_inline_disc_one_of_one_of.dart';
import 'package:openapi/src/model/fruit_one_of_disc.dart';
import 'package:openapi/src/model/fruit_req_disc.dart';
import 'package:openapi/src/model/fruit_type.dart';
import 'package:openapi/src/model/fruit_variant1.dart';
import 'package:openapi/src/model/giga_one_of.dart';
import 'package:openapi/src/model/grape_variant1.dart';
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
import 'package:openapi/src/model/one_of_primitive_child.dart';
import 'package:openapi/src/model/order.dart';
import 'package:openapi/src/model/outer_composite.dart';
import 'package:openapi/src/model/outer_object_with_enum_property.dart';
import 'package:openapi/src/model/parent.dart';
import 'package:openapi/src/model/pasta.dart';
import 'package:openapi/src/model/pet.dart';
import 'package:openapi/src/model/pizza.dart';
import 'package:openapi/src/model/pizza_speziale.dart';
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
        case 'Addressable':
          return Addressable.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'AllOfWithSingleRef':
          return AllOfWithSingleRef.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Animal':
          return Animal.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ApiResponse':
          return ApiResponse.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Apple':
          return Apple.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'AppleAllOfDisc':
          return AppleAllOfDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'AppleGrandparentDisc':
          return AppleGrandparentDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'AppleOneOfDisc':
          return AppleOneOfDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'AppleReqDisc':
          return AppleReqDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'AppleVariant1':
          return AppleVariant1.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ArrayOfArrayOfNumberOnly':
          return ArrayOfArrayOfNumberOnly.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ArrayOfNumberOnly':
          return ArrayOfNumberOnly.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ArrayTest':
          return ArrayTest.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Banana':
          return Banana.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'BananaAllOfDisc':
          return BananaAllOfDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'BananaGrandparentDisc':
          return BananaGrandparentDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'BananaOneOfDisc':
          return BananaOneOfDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'BananaReqDisc':
          return BananaReqDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Bar':
          return Bar.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'BarCreate':
          return BarCreate.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'BarRef':
          return BarRef.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'BarRefOrValue':
          return BarRefOrValue.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Capitalization':
          return Capitalization.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Cat':
          return Cat.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Category':
          return Category.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ClassModel':
          return ClassModel.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ComposedDiscMissingFromProperties':
          return ComposedDiscMissingFromProperties.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ComposedDiscOptionalTypeCorrect':
          return ComposedDiscOptionalTypeCorrect.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ComposedDiscOptionalTypeInconsistent':
          return ComposedDiscOptionalTypeInconsistent.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ComposedDiscOptionalTypeIncorrect':
          return ComposedDiscOptionalTypeIncorrect.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ComposedDiscRequiredInconsistent':
          return ComposedDiscRequiredInconsistent.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ComposedDiscTypeInconsistent':
          return ComposedDiscTypeInconsistent.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'ComposedDiscTypeIncorrect':
          return ComposedDiscTypeIncorrect.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'DeprecatedObject':
          return DeprecatedObject.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'DiscMissingFromProperties':
          return DiscMissingFromProperties.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'DiscOptionalTypeCorrect':
          return DiscOptionalTypeCorrect.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'DiscOptionalTypeIncorrect':
          return DiscOptionalTypeIncorrect.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'DiscTypeIncorrect':
          return DiscTypeIncorrect.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Dog':
          return Dog.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Entity':
          return Entity.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'EntityRef':
          return EntityRef.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'EnumArrays':
          return EnumArrays.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'EnumTest':
          return EnumTest.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Extensible':
          return Extensible.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FileSchemaTestClass':
          return FileSchemaTestClass.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Foo':
          return Foo.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FooBasicGetDefaultResponse':
          return FooBasicGetDefaultResponse.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FooRef':
          return FooRef.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FooRefOrValue':
          return FooRefOrValue.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FormatTest':
          return FormatTest.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Fruit':
          return Fruit.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitAllOfDisc':
          return FruitAllOfDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitAnyOfDisc':
          return FruitAnyOfDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitGrandparentDisc':
          return FruitGrandparentDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitInlineDisc':
          return FruitInlineDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitInlineDiscOneOf':
          return FruitInlineDiscOneOf.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitInlineDiscOneOf1':
          return FruitInlineDiscOneOf1.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitInlineInlineDisc':
          return FruitInlineInlineDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitInlineInlineDiscOneOf':
          return FruitInlineInlineDiscOneOf.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitInlineInlineDiscOneOf1':
          return FruitInlineInlineDiscOneOf1.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitInlineInlineDiscOneOfOneOf':
          return FruitInlineInlineDiscOneOfOneOf.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitOneOfDisc':
          return FruitOneOfDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitReqDisc':
          return FruitReqDisc.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitType':
          return FruitType.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'FruitVariant1':
          return FruitVariant1.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'GigaOneOf':
          return GigaOneOf.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'GrapeVariant1':
          return GrapeVariant1.fromJson(value as Map<String, dynamic>) as ReturnType;
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
        case 'OneOfPrimitiveChild':
          return OneOfPrimitiveChild.fromJson(value as Map<String, dynamic>) as ReturnType;
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
        case 'Parent':
          return Parent.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Pasta':
          return Pasta.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Pet':
          return Pet.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'Pizza':
          return Pizza.fromJson(value as Map<String, dynamic>) as ReturnType;
        case 'PizzaSpeziale':
          return PizzaSpeziale.fromJson(value as Map<String, dynamic>) as ReturnType;
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