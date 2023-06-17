import 'dart:convert';
import 'dart:typed_data';
import 'repository_base.dart';
import 'package:openapi/models.dart';

class JsonSerializableRepository extends SerializationRepositoryBase {
  JsonSerializableRepository();

  /// Transforms an object of arbitrary type [T] (whose information is passed in inputTypeInfo) to a dart primitive
  @override
  FutureOr<Object?> serialize<T>(T src, TypeInfo inputTypeInfo,
      {Object? context}) {
    if (src == null) {
      return null;
    }
    if (src is String) {
      return src;
    }
    if (src is bool) {
      return src;
    }
    if (src is DateTime) {
      return src.toIso8601String();
    }
    if (src is num) {
      return src;
    }
    if (src is Uint8List) {
      return src;
    }
    if (src is Iterable) {
      return src
          .map((value) =>
              serialize(value, inputTypeInfo.parameters[0], context: context))
          .toList();
    }
    if (src is Map) {
      return src.map((key, value) => MapEntry(
          //Json Map keys must always be strings
          serialize(key, inputTypeInfo.parameters[0], context: context)
              .toString(),
          serialize(value, inputTypeInfo.parameters[1], context: context)));
    }

    if (context is Object? Function(Object?)) {
      return context(src);
    }

    try {
      return (src as dynamic).toJson();
    } on NoSuchMethodError {
      assert(false, '''
          Failed to serialize the object properly, falling back to `toString()`
          ''');
      return src.toString();
    }
  }

  /// Transforms a dart primitive to an object of arbitrary type [T] (whose information is passed in inputTypeInfo)
  @override
  T deserialize<T>(Object? value, TypeInfo targetTypeInfo, {Object? context}) {
    //Don't rely on T being accurate here, since it won't get passed for generic arguments.
    if (value == null) {
      return null as T;
    }
    final targetRootType = targetTypeInfo.root;
    if (targetRootType == T) {
      //short circuit if we already have the output as the input
      return value as T;
    }
    switch (targetRootType) {
      case String:
        return value.toString() as T;
      case double:
        return (value is num
            ? value.toDouble()
            : double.tryParse(value.toString())) as T;
      case int:
        return (value is num ? value.toInt() : int.tryParse(value.toString()))
            as T;
      case num:
        return (value is num ? value : num.tryParse(value.toString())) as T;
      case bool:
        return (value is bool ? value : bool.tryParse(value.toString())) as T;
      case DateTime:
        return (value is DateTime ? value : DateTime.tryParse(value.toString()))
            as T;
      case Iterable:
      case List:
      case Set:
        if (value is! Iterable) {
          throw Exception('Cannot deserialize');
        }
        final mappedItems = value.map((v) =>
            deserialize(v, targetTypeInfo.parameters[0], context: context));
        if (targetRootType == List) {
          return List.from(mappedItems);
        } else if (targetRootType == Set) {
          return Set.from(mappedItems);
        } else if (targetRootType == Iterable) {
          return mappedItems;
        } else {
          throw Exception('Cannot deserialize');
        }
      case Map:
        if (value is! Map) {
          throw Exception('Cannot deserialize');
        }
        return value.map((k, v) => MapEntry(
            deserialize(k, targetTypeInfo.parameters[0], context: context),
            deserialize(v, targetTypeInfo.parameters[1], context: context)));
      case AdditionalPropertiesClass:
        return AdditionalPropertiesClass.fromJson(value as Map<String, dynamic>)
            as T;
      case Addressable:
        return Addressable.fromJson(value as Map<String, dynamic>) as T;
      case AllOfWithSingleRef:
        return AllOfWithSingleRef.fromJson(value as Map<String, dynamic>) as T;
      case Animal:
        return Animal.fromJson(value as Map<String, dynamic>) as T;
      case ApiResponse:
        return ApiResponse.fromJson(value as Map<String, dynamic>) as T;
      case Apple:
        return Apple.fromJson(value as Map<String, dynamic>) as T;
      case AppleAllOfDisc:
        return AppleAllOfDisc.fromJson(value as Map<String, dynamic>) as T;
      case AppleGrandparentDisc:
        return AppleGrandparentDisc.fromJson(value as Map<String, dynamic>)
            as T;
      case AppleOneOfDisc:
        return AppleOneOfDisc.fromJson(value as Map<String, dynamic>) as T;
      case AppleReqDisc:
        return AppleReqDisc.fromJson(value as Map<String, dynamic>) as T;
      case AppleVariant1:
        return AppleVariant1.fromJson(value as Map<String, dynamic>) as T;
      case ArrayOfArrayOfNumberOnly:
        return ArrayOfArrayOfNumberOnly.fromJson(value as Map<String, dynamic>)
            as T;
      case ArrayOfNumberOnly:
        return ArrayOfNumberOnly.fromJson(value as Map<String, dynamic>) as T;
      case ArrayTest:
        return ArrayTest.fromJson(value as Map<String, dynamic>) as T;
      case Banana:
        return Banana.fromJson(value as Map<String, dynamic>) as T;
      case BananaAllOfDisc:
        return BananaAllOfDisc.fromJson(value as Map<String, dynamic>) as T;
      case BananaGrandparentDisc:
        return BananaGrandparentDisc.fromJson(value as Map<String, dynamic>)
            as T;
      case BananaOneOfDisc:
        return BananaOneOfDisc.fromJson(value as Map<String, dynamic>) as T;
      case BananaReqDisc:
        return BananaReqDisc.fromJson(value as Map<String, dynamic>) as T;
      case Bar:
        return Bar.fromJson(value as Map<String, dynamic>) as T;
      case BarCreate:
        return BarCreate.fromJson(value as Map<String, dynamic>) as T;
      case BarRef:
        return BarRef.fromJson(value as Map<String, dynamic>) as T;
      case BarRefOrValue:
        return BarRefOrValue.fromJson(value as Map<String, dynamic>) as T;
      case Capitalization:
        return Capitalization.fromJson(value as Map<String, dynamic>) as T;
      case Cat:
        return Cat.fromJson(value as Map<String, dynamic>) as T;
      case Category:
        return Category.fromJson(value as Map<String, dynamic>) as T;
      case ClassModel:
        return ClassModel.fromJson(value as Map<String, dynamic>) as T;
      case ComposedDiscMissingFromProperties:
        return ComposedDiscMissingFromProperties.fromJson(
            value as Map<String, dynamic>) as T;
      case ComposedDiscOptionalTypeCorrect:
        return ComposedDiscOptionalTypeCorrect.fromJson(
            value as Map<String, dynamic>) as T;
      case ComposedDiscOptionalTypeInconsistent:
        return ComposedDiscOptionalTypeInconsistent.fromJson(
            value as Map<String, dynamic>) as T;
      case ComposedDiscOptionalTypeIncorrect:
        return ComposedDiscOptionalTypeIncorrect.fromJson(
            value as Map<String, dynamic>) as T;
      case ComposedDiscRequiredInconsistent:
        return ComposedDiscRequiredInconsistent.fromJson(
            value as Map<String, dynamic>) as T;
      case ComposedDiscTypeInconsistent:
        return ComposedDiscTypeInconsistent.fromJson(
            value as Map<String, dynamic>) as T;
      case ComposedDiscTypeIncorrect:
        return ComposedDiscTypeIncorrect.fromJson(value as Map<String, dynamic>)
            as T;
      case DeprecatedObject:
        return DeprecatedObject.fromJson(value as Map<String, dynamic>) as T;
      case DiscMissingFromProperties:
        return DiscMissingFromProperties.fromJson(value as Map<String, dynamic>)
            as T;
      case DiscOptionalTypeCorrect:
        return DiscOptionalTypeCorrect.fromJson(value as Map<String, dynamic>)
            as T;
      case DiscOptionalTypeIncorrect:
        return DiscOptionalTypeIncorrect.fromJson(value as Map<String, dynamic>)
            as T;
      case DiscTypeIncorrect:
        return DiscTypeIncorrect.fromJson(value as Map<String, dynamic>) as T;
      case Dog:
        return Dog.fromJson(value as Map<String, dynamic>) as T;
      case Entity:
        return Entity.fromJson(value as Map<String, dynamic>) as T;
      case EntityRef:
        return EntityRef.fromJson(value as Map<String, dynamic>) as T;
      case EnumArrays:
        return EnumArrays.fromJson(value as Map<String, dynamic>) as T;
      case EnumTest:
        return EnumTest.fromJson(value as Map<String, dynamic>) as T;
      case Extensible:
        return Extensible.fromJson(value as Map<String, dynamic>) as T;
      case FileSchemaTestClass:
        return FileSchemaTestClass.fromJson(value as Map<String, dynamic>) as T;
      case Foo:
        return Foo.fromJson(value as Map<String, dynamic>) as T;
      case FooBasicGetDefaultResponse:
        return FooBasicGetDefaultResponse.fromJson(
            value as Map<String, dynamic>) as T;
      case FooRef:
        return FooRef.fromJson(value as Map<String, dynamic>) as T;
      case FooRefOrValue:
        return FooRefOrValue.fromJson(value as Map<String, dynamic>) as T;
      case FormatTest:
        return FormatTest.fromJson(value as Map<String, dynamic>) as T;
      case Fruit:
        return Fruit.fromJson(value as Map<String, dynamic>) as T;
      case FruitAllOfDisc:
        return FruitAllOfDisc.fromJson(value as Map<String, dynamic>) as T;
      case FruitAnyOfDisc:
        return FruitAnyOfDisc.fromJson(value as Map<String, dynamic>) as T;
      case FruitGrandparentDisc:
        return FruitGrandparentDisc.fromJson(value as Map<String, dynamic>)
            as T;
      case FruitInlineDisc:
        return FruitInlineDisc.fromJson(value as Map<String, dynamic>) as T;
      case FruitInlineDiscOneOf:
        return FruitInlineDiscOneOf.fromJson(value as Map<String, dynamic>)
            as T;
      case FruitInlineDiscOneOf1:
        return FruitInlineDiscOneOf1.fromJson(value as Map<String, dynamic>)
            as T;
      case FruitInlineInlineDisc:
        return FruitInlineInlineDisc.fromJson(value as Map<String, dynamic>)
            as T;
      case FruitInlineInlineDiscOneOf:
        return FruitInlineInlineDiscOneOf.fromJson(
            value as Map<String, dynamic>) as T;
      case FruitInlineInlineDiscOneOf1:
        return FruitInlineInlineDiscOneOf1.fromJson(
            value as Map<String, dynamic>) as T;
      case FruitInlineInlineDiscOneOfOneOf:
        return FruitInlineInlineDiscOneOfOneOf.fromJson(
            value as Map<String, dynamic>) as T;
      case FruitOneOfDisc:
        return FruitOneOfDisc.fromJson(value as Map<String, dynamic>) as T;
      case FruitReqDisc:
        return FruitReqDisc.fromJson(value as Map<String, dynamic>) as T;
      case FruitType:
        return FruitType.fromJson(value as Map<String, dynamic>) as T;
      case FruitVariant1:
        return FruitVariant1.fromJson(value as Map<String, dynamic>) as T;
      case GigaOneOf:
        return GigaOneOf.fromJson(value as Map<String, dynamic>) as T;
      case GrapeVariant1:
        return GrapeVariant1.fromJson(value as Map<String, dynamic>) as T;
      case HasOnlyReadOnly:
        return HasOnlyReadOnly.fromJson(value as Map<String, dynamic>) as T;
      case HealthCheckResult:
        return HealthCheckResult.fromJson(value as Map<String, dynamic>) as T;
      case MapTest:
        return MapTest.fromJson(value as Map<String, dynamic>) as T;
      case MixedPropertiesAndAdditionalPropertiesClass:
        return MixedPropertiesAndAdditionalPropertiesClass.fromJson(
            value as Map<String, dynamic>) as T;
      case Model200Response:
        return Model200Response.fromJson(value as Map<String, dynamic>) as T;
      case ModelClient:
        return ModelClient.fromJson(value as Map<String, dynamic>) as T;
      case ModelEnumClass:
        //TODO: find a way to support enums
        return value as T;
      case ModelFile:
        return ModelFile.fromJson(value as Map<String, dynamic>) as T;
      case ModelList:
        return ModelList.fromJson(value as Map<String, dynamic>) as T;
      case ModelReturn:
        return ModelReturn.fromJson(value as Map<String, dynamic>) as T;
      case Name:
        return Name.fromJson(value as Map<String, dynamic>) as T;
      case NullableClass:
        return NullableClass.fromJson(value as Map<String, dynamic>) as T;
      case NumberOnly:
        return NumberOnly.fromJson(value as Map<String, dynamic>) as T;
      case ObjectWithDeprecatedFields:
        return ObjectWithDeprecatedFields.fromJson(
            value as Map<String, dynamic>) as T;
      case OneOfPrimitiveChild:
        return OneOfPrimitiveChild.fromJson(value as Map<String, dynamic>) as T;
      case Order:
        return Order.fromJson(value as Map<String, dynamic>) as T;
      case OuterComposite:
        return OuterComposite.fromJson(value as Map<String, dynamic>) as T;
      case OuterEnum:
        //TODO: find a way to support enums
        return value as T;
      case OuterEnumDefaultValue:
        //TODO: find a way to support enums
        return value as T;
      case OuterEnumInteger:
        //TODO: find a way to support enums
        return value as T;
      case OuterEnumIntegerDefaultValue:
        //TODO: find a way to support enums
        return value as T;
      case OuterObjectWithEnumProperty:
        return OuterObjectWithEnumProperty.fromJson(
            value as Map<String, dynamic>) as T;
      case Parent:
        return Parent.fromJson(value as Map<String, dynamic>) as T;
      case Pasta:
        return Pasta.fromJson(value as Map<String, dynamic>) as T;
      case Pet:
        return Pet.fromJson(value as Map<String, dynamic>) as T;
      case Pizza:
        return Pizza.fromJson(value as Map<String, dynamic>) as T;
      case PizzaSpeziale:
        return PizzaSpeziale.fromJson(value as Map<String, dynamic>) as T;
      case ReadOnlyFirst:
        return ReadOnlyFirst.fromJson(value as Map<String, dynamic>) as T;
      case SingleRefType:
        //TODO: find a way to support enums
        return value as T;
      case SpecialModelName:
        return SpecialModelName.fromJson(value as Map<String, dynamic>) as T;
      case Tag:
        return Tag.fromJson(value as Map<String, dynamic>) as T;
      case User:
        return User.fromJson(value as Map<String, dynamic>) as T;
      default:
        if (value is T) {
          return value;
        }
        throw Exception('Cannot deserialize');
    }
  }
}
