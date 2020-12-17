import 'package:built_collection/built_collection.dart';
import 'package:built_value/json_object.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'nullable_class.g.dart';

abstract class NullableClass implements Built<NullableClass, NullableClassBuilder> {

    @nullable
    @BuiltValueField(wireName: r'integer_prop')
    int get integerProp;

    @nullable
    @BuiltValueField(wireName: r'number_prop')
    num get numberProp;

    @nullable
    @BuiltValueField(wireName: r'boolean_prop')
    bool get booleanProp;

    @nullable
    @BuiltValueField(wireName: r'string_prop')
    String get stringProp;

    @nullable
    @BuiltValueField(wireName: r'date_prop')
    DateTime get dateProp;

    @nullable
    @BuiltValueField(wireName: r'datetime_prop')
    DateTime get datetimeProp;

    @nullable
    @BuiltValueField(wireName: r'array_nullable_prop')
    BuiltList<JsonObject> get arrayNullableProp;

    @nullable
    @BuiltValueField(wireName: r'array_and_items_nullable_prop')
    BuiltList<JsonObject> get arrayAndItemsNullableProp;

    @nullable
    @BuiltValueField(wireName: r'array_items_nullable')
    BuiltList<JsonObject> get arrayItemsNullable;

    @nullable
    @BuiltValueField(wireName: r'object_nullable_prop')
    BuiltMap<String, JsonObject> get objectNullableProp;

    @nullable
    @BuiltValueField(wireName: r'object_and_items_nullable_prop')
    BuiltMap<String, JsonObject> get objectAndItemsNullableProp;

    @nullable
    @BuiltValueField(wireName: r'object_items_nullable')
    BuiltMap<String, JsonObject> get objectItemsNullable;

    // Boilerplate code needed to wire-up generated code
    NullableClass._();

    factory NullableClass([updates(NullableClassBuilder b)]) = _$NullableClass;
    static Serializer<NullableClass> get serializer => _$nullableClassSerializer;
}

