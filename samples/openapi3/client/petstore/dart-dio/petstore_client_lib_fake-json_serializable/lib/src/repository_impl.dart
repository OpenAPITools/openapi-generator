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

    /*  ReturnType deserialize<ReturnType, BaseType>(dynamic value, String targetType, {bool growable= true}) {     }*/   
    @override
    T deserialize<T>(Object value, TypeInfo targetTypeInfo, {Object? context}) {
        //Dio automatically calls jsonDecode if the value is a string
        if (value is String) {
            value = jsonDecode(value);
        }
        switch (targetTypeInfo.root) {
            case String:
                return valueString as T;
            case int:
                return (value is num ? value.toInt() : int.parse(value.toString())) as T;
            case bool:
                if (value is bool) {
                    return value as T;
                }
                final valueString = value.toString().toLowerCase();
                return (valueString == 'true' || valueString == '1') as T;
            case double:
                return (value is num ? value.toDouble() : double.parse(value.toString())) as T;
            //TODO: add support for DateTime?
            case AdditionalPropertiesClass:
            return AdditionalPropertiesClass.fromJson(value as Map<String, dynamic>) as T;
            case AllOfWithSingleRef:
            return AllOfWithSingleRef.fromJson(value as Map<String, dynamic>) as T;
            case Animal:
            return Animal.fromJson(value as Map<String, dynamic>) as T;
            case ApiResponse:
            return ApiResponse.fromJson(value as Map<String, dynamic>) as T;
            case ArrayOfArrayOfNumberOnly:
            return ArrayOfArrayOfNumberOnly.fromJson(value as Map<String, dynamic>) as T;
            case ArrayOfNumberOnly:
            return ArrayOfNumberOnly.fromJson(value as Map<String, dynamic>) as T;
            case ArrayTest:
            return ArrayTest.fromJson(value as Map<String, dynamic>) as T;
            case Capitalization:
            return Capitalization.fromJson(value as Map<String, dynamic>) as T;
            case Cat:
            return Cat.fromJson(value as Map<String, dynamic>) as T;
            case CatAllOf:
            return CatAllOf.fromJson(value as Map<String, dynamic>) as T;
            case Category:
            return Category.fromJson(value as Map<String, dynamic>) as T;
            case ClassModel:
            return ClassModel.fromJson(value as Map<String, dynamic>) as T;
            case DeprecatedObject:
            return DeprecatedObject.fromJson(value as Map<String, dynamic>) as T;
            case Dog:
            return Dog.fromJson(value as Map<String, dynamic>) as T;
            case DogAllOf:
            return DogAllOf.fromJson(value as Map<String, dynamic>) as T;
            case EnumArrays:
            return EnumArrays.fromJson(value as Map<String, dynamic>) as T;
            case EnumTest:
            return EnumTest.fromJson(value as Map<String, dynamic>) as T;
            case FileSchemaTestClass:
            return FileSchemaTestClass.fromJson(value as Map<String, dynamic>) as T;
            case Foo:
            return Foo.fromJson(value as Map<String, dynamic>) as T;
            case FooGetDefaultResponse:
            return FooGetDefaultResponse.fromJson(value as Map<String, dynamic>) as T;
            case FormatTest:
            return FormatTest.fromJson(value as Map<String, dynamic>) as T;
            case HasOnlyReadOnly:
            return HasOnlyReadOnly.fromJson(value as Map<String, dynamic>) as T;
            case HealthCheckResult:
            return HealthCheckResult.fromJson(value as Map<String, dynamic>) as T;
            case MapTest:
            return MapTest.fromJson(value as Map<String, dynamic>) as T;
            case MixedPropertiesAndAdditionalPropertiesClass:
            return MixedPropertiesAndAdditionalPropertiesClass.fromJson(value as Map<String, dynamic>) as T;
            case Model200Response:
            return Model200Response.fromJson(value as Map<String, dynamic>) as T;
            case ModelClient:
            return ModelClient.fromJson(value as Map<String, dynamic>) as T;
            case ModelEnumClass:
            
            
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
            return ObjectWithDeprecatedFields.fromJson(value as Map<String, dynamic>) as T;
            case Order:
            return Order.fromJson(value as Map<String, dynamic>) as T;
            case OuterComposite:
            return OuterComposite.fromJson(value as Map<String, dynamic>) as T;
            case OuterEnum:
            
            
            case OuterEnumDefaultValue:
            
            
            case OuterEnumInteger:
            
            
            case OuterEnumIntegerDefaultValue:
            
            
            case OuterObjectWithEnumProperty:
            return OuterObjectWithEnumProperty.fromJson(value as Map<String, dynamic>) as T;
            case Pet:
            return Pet.fromJson(value as Map<String, dynamic>) as T;
            case ReadOnlyFirst:
            return ReadOnlyFirst.fromJson(value as Map<String, dynamic>) as T;
            case SingleRefType:
            
            
            case SpecialModelName:
            return SpecialModelName.fromJson(value as Map<String, dynamic>) as T;
            case Tag:
            return Tag.fromJson(value as Map<String, dynamic>) as T;
            case User:
            return User.fromJson(value as Map<String, dynamic>) as T;
            default:
            RegExpMatch? match;

            if (value is List && (match = _regList.firstMatch(targetTypeInfo)) != null) {
                targetType = match![1]!; // ignore: parameter_assignments
                return value
                .map<BaseType>((dynamic v) => deserialize<BaseType, BaseType>(v, targetTypeInfo, growable: growable))
                .toList(growable: growable) as T;
            }
            if (value is Set && (match = _regSet.firstMatch(targetType)) != null) {
                targetType = match![1]!; // ignore: parameter_assignments
                return value
                .map<BaseType>((dynamic v) => deserialize<BaseType, BaseType>(v, targetTypeInfo, growable: growable))
                .toSet() as T;
            }
            if (value is Map && (match = _regMap.firstMatch(targetType)) != null) {
                targetType = match![1]!; // ignore: parameter_assignments
                return Map<dynamic, BaseType>.fromIterables(
                value.keys,
                value.values.map((dynamic v) => deserialize<BaseType, BaseType>(v, targetTypeInfo, growable: growable)),
                ) as T;
            }
            break;
        } 
        throw Exception('Cannot deserialize');        
    }

    @override
    Object encodeFormParameter<T>(T src, TypeInfo inputTypeInfo, {Object? context}) {
        return src;
    }

    @override
    Object encodeQueryParameter<T>(T src, TypeInfo inputTypeInfo, {
        Object? context,
    }) {
        return src;
    }

    @override
    String encodeStringParameter<T>(T src, TypeInfo inputTypeInfo, {
        Object? context,
    }) {
        return src.toString();
    }


}