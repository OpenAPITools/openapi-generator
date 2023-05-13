import 'dart:convert';
import 'dart:typed_data';
import 'repository_base.dart';
import 'package:openapi/models.dart';

class JsonSerializableRepository extends SerializationRepositoryBase {
  JsonSerializableRepository();

  @override
  Object serialize<T>(T src, TypeInfo inputTypeInfo, {Object? context}) {
    return jsonEncode(src);
  }

  @override
  T deserialize<T>(Object value, TypeInfo targetTypeInfo, {Object? context}) {
    Object valueParsed;
    if (context == 'decodedJson') {
      //if we are deserializing a decoded json, don't decode it again.
      valueParsed = value;
    } else {
      valueParsed = jsonDecode(value.toString()) as Object;
    }
    switch (targetTypeInfo.root) {
      case String:
        return valueParsed as T;
      case int:
        return (valueParsed is num
            ? valueParsed.toInt()
            : int.parse(valueParsed.toString())) as T;
      case bool:
        if (valueParsed is bool) {
          return valueParsed as T;
        }
        final valueString = valueParsed.toString().toLowerCase();
        return (valueString == 'true' || valueString == '1') as T;
      case double:
        return (valueParsed is num
            ? valueParsed.toDouble()
            : double.parse(valueParsed.toString())) as T;
      case AdditionalPropertiesClass:
        return AdditionalPropertiesClass.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case Addressable:
        return Addressable.fromJson(valueParsed as Map<String, dynamic>) as T;
      case AllOfWithSingleRef:
        return AllOfWithSingleRef.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case Animal:
        return Animal.fromJson(valueParsed as Map<String, dynamic>) as T;
      case ApiResponse:
        return ApiResponse.fromJson(valueParsed as Map<String, dynamic>) as T;
      case Apple:
        return Apple.fromJson(valueParsed as Map<String, dynamic>) as T;
      case AppleAllOfDisc:
        return AppleAllOfDisc.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case AppleAnyOfDisc:
        return AppleAnyOfDisc.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case AppleGrandparentDisc:
        return AppleGrandparentDisc.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case AppleOneOfDisc:
        return AppleOneOfDisc.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case AppleReqDisc:
        return AppleReqDisc.fromJson(valueParsed as Map<String, dynamic>) as T;
      case AppleVariant1:
        return AppleVariant1.fromJson(valueParsed as Map<String, dynamic>) as T;
      case ArrayOfArrayOfNumberOnly:
        return ArrayOfArrayOfNumberOnly.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case ArrayOfNumberOnly:
        return ArrayOfNumberOnly.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case ArrayTest:
        return ArrayTest.fromJson(valueParsed as Map<String, dynamic>) as T;
      case Banana:
        return Banana.fromJson(valueParsed as Map<String, dynamic>) as T;
      case BananaAllOfDisc:
        return BananaAllOfDisc.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case BananaAnyOfDisc:
        return BananaAnyOfDisc.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case BananaGrandparentDisc:
        return BananaGrandparentDisc.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case BananaOneOfDisc:
        return BananaOneOfDisc.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case BananaReqDisc:
        return BananaReqDisc.fromJson(valueParsed as Map<String, dynamic>) as T;
      case Bar:
        return Bar.fromJson(valueParsed as Map<String, dynamic>) as T;
      case BarCreate:
        return BarCreate.fromJson(valueParsed as Map<String, dynamic>) as T;
      case BarRef:
        return BarRef.fromJson(valueParsed as Map<String, dynamic>) as T;
      case BarRefOrValue:
        return BarRefOrValue.fromJson(valueParsed as Map<String, dynamic>) as T;
      case Capitalization:
        return Capitalization.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case Cat:
        return Cat.fromJson(valueParsed as Map<String, dynamic>) as T;
      case CatAllOf:
        return CatAllOf.fromJson(valueParsed as Map<String, dynamic>) as T;
      case Category:
        return Category.fromJson(valueParsed as Map<String, dynamic>) as T;
      case ClassModel:
        return ClassModel.fromJson(valueParsed as Map<String, dynamic>) as T;
      case ComposedDiscMissingFromProperties:
        return ComposedDiscMissingFromProperties.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case ComposedDiscOptionalTypeCorrect:
        return ComposedDiscOptionalTypeCorrect.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case ComposedDiscOptionalTypeInconsistent:
        return ComposedDiscOptionalTypeInconsistent.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case ComposedDiscOptionalTypeIncorrect:
        return ComposedDiscOptionalTypeIncorrect.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case ComposedDiscRequiredInconsistent:
        return ComposedDiscRequiredInconsistent.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case ComposedDiscTypeInconsistent:
        return ComposedDiscTypeInconsistent.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case ComposedDiscTypeIncorrect:
        return ComposedDiscTypeIncorrect.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case DeprecatedObject:
        return DeprecatedObject.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case DiscMissingFromProperties:
        return DiscMissingFromProperties.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case DiscOptionalTypeCorrect:
        return DiscOptionalTypeCorrect.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case DiscOptionalTypeIncorrect:
        return DiscOptionalTypeIncorrect.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case DiscTypeIncorrect:
        return DiscTypeIncorrect.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case Dog:
        return Dog.fromJson(valueParsed as Map<String, dynamic>) as T;
      case DogAllOf:
        return DogAllOf.fromJson(valueParsed as Map<String, dynamic>) as T;
      case Entity:
        return Entity.fromJson(valueParsed as Map<String, dynamic>) as T;
      case EntityRef:
        return EntityRef.fromJson(valueParsed as Map<String, dynamic>) as T;
      case EnumArrays:
        return EnumArrays.fromJson(valueParsed as Map<String, dynamic>) as T;
      case EnumTest:
        return EnumTest.fromJson(valueParsed as Map<String, dynamic>) as T;
      case Extensible:
        return Extensible.fromJson(valueParsed as Map<String, dynamic>) as T;
      case FileSchemaTestClass:
        return FileSchemaTestClass.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case Foo:
        return Foo.fromJson(valueParsed as Map<String, dynamic>) as T;
      case FooBasicGetDefaultResponse:
        return FooBasicGetDefaultResponse.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case FooRef:
        return FooRef.fromJson(valueParsed as Map<String, dynamic>) as T;
      case FooRefOrValue:
        return FooRefOrValue.fromJson(valueParsed as Map<String, dynamic>) as T;
      case FormatTest:
        return FormatTest.fromJson(valueParsed as Map<String, dynamic>) as T;
      case Fruit:
        return Fruit.fromJson(valueParsed as Map<String, dynamic>) as T;
      case FruitAllOfDisc:
        return FruitAllOfDisc.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case FruitAnyOfDisc:
        return FruitAnyOfDisc.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case FruitGrandparentDisc:
        return FruitGrandparentDisc.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case FruitInlineDisc:
        return FruitInlineDisc.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case FruitInlineDiscOneOf:
        return FruitInlineDiscOneOf.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case FruitInlineDiscOneOf1:
        return FruitInlineDiscOneOf1.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case FruitInlineInlineDisc:
        return FruitInlineInlineDisc.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case FruitInlineInlineDiscOneOf:
        return FruitInlineInlineDiscOneOf.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case FruitInlineInlineDiscOneOf1:
        return FruitInlineInlineDiscOneOf1.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case FruitInlineInlineDiscOneOfOneOf:
        return FruitInlineInlineDiscOneOfOneOf.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case FruitOneOfDisc:
        return FruitOneOfDisc.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case FruitReqDisc:
        return FruitReqDisc.fromJson(valueParsed as Map<String, dynamic>) as T;
      case FruitType:
        return FruitType.fromJson(valueParsed as Map<String, dynamic>) as T;
      case FruitVariant1:
        return FruitVariant1.fromJson(valueParsed as Map<String, dynamic>) as T;
      case GigaOneOf:
        return GigaOneOf.fromJson(valueParsed as Map<String, dynamic>) as T;
      case GrapeVariant1:
        return GrapeVariant1.fromJson(valueParsed as Map<String, dynamic>) as T;
      case HasOnlyReadOnly:
        return HasOnlyReadOnly.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case HealthCheckResult:
        return HealthCheckResult.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case MapTest:
        return MapTest.fromJson(valueParsed as Map<String, dynamic>) as T;
      case MixedPropertiesAndAdditionalPropertiesClass:
        return MixedPropertiesAndAdditionalPropertiesClass.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case Model200Response:
        return Model200Response.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case ModelClient:
        return ModelClient.fromJson(valueParsed as Map<String, dynamic>) as T;
      case ModelEnumClass:
      case ModelFile:
        return ModelFile.fromJson(valueParsed as Map<String, dynamic>) as T;
      case ModelList:
        return ModelList.fromJson(valueParsed as Map<String, dynamic>) as T;
      case ModelReturn:
        return ModelReturn.fromJson(valueParsed as Map<String, dynamic>) as T;
      case Name:
        return Name.fromJson(valueParsed as Map<String, dynamic>) as T;
      case NullableClass:
        return NullableClass.fromJson(valueParsed as Map<String, dynamic>) as T;
      case NumberOnly:
        return NumberOnly.fromJson(valueParsed as Map<String, dynamic>) as T;
      case ObjectWithDeprecatedFields:
        return ObjectWithDeprecatedFields.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case OneOfPrimitiveChild:
        return OneOfPrimitiveChild.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case Order:
        return Order.fromJson(valueParsed as Map<String, dynamic>) as T;
      case OuterComposite:
        return OuterComposite.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case OuterEnum:
      case OuterEnumDefaultValue:
      case OuterEnumInteger:
      case OuterEnumIntegerDefaultValue:
      case OuterObjectWithEnumProperty:
        return OuterObjectWithEnumProperty.fromJson(
            valueParsed as Map<String, dynamic>) as T;
      case Parent:
        return Parent.fromJson(valueParsed as Map<String, dynamic>) as T;
      case Pasta:
        return Pasta.fromJson(valueParsed as Map<String, dynamic>) as T;
      case Pet:
        return Pet.fromJson(valueParsed as Map<String, dynamic>) as T;
      case Pizza:
        return Pizza.fromJson(valueParsed as Map<String, dynamic>) as T;
      case PizzaSpeziale:
        return PizzaSpeziale.fromJson(valueParsed as Map<String, dynamic>) as T;
      case ReadOnlyFirst:
        return ReadOnlyFirst.fromJson(valueParsed as Map<String, dynamic>) as T;
      case SingleRefType:
      case SpecialModelName:
        return SpecialModelName.fromJson(valueParsed as Map<String, dynamic>)
            as T;
      case Tag:
        return Tag.fromJson(valueParsed as Map<String, dynamic>) as T;
      case User:
        return User.fromJson(valueParsed as Map<String, dynamic>) as T;
      default:
        RegExpMatch? match;

        if (valueParsed is List &&
            (match = _regList.firstMatch(targetTypeInfo)) != null) {
          targetType = match![1]!; // ignore: parameter_assignments
          return valueParsed
              .map<BaseType>((dynamic v) => deserialize<BaseType, BaseType>(
                  v, targetTypeInfo,
                  growable: growable))
              .toList(growable: growable) as T;
        }
        if (valueParsed is Set &&
            (match = _regSet.firstMatch(targetType)) != null) {
          targetType = match![1]!; // ignore: parameter_assignments
          return valueParsed
              .map<BaseType>((dynamic v) => deserialize<BaseType, BaseType>(
                  v, targetTypeInfo,
                  growable: growable))
              .toSet() as T;
        }
        if (valueParsed is Map &&
            (match = _regMap.firstMatch(targetType)) != null) {
          targetType = match![1]!; // ignore: parameter_assignments
          return Map<dynamic, BaseType>.fromIterables(
            valueParsed.keys,
            valueParsed.values.map((dynamic v) =>
                deserialize<BaseType, BaseType>(v, targetTypeInfo,
                    growable: growable)),
          ) as T;
        }
        break;
    }
    throw Exception('Cannot deserialize');
  }
}
