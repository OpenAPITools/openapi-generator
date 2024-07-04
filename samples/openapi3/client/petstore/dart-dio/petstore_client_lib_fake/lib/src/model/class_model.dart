//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'class_model.g.dart';

/// Model for testing model with \"_class\" property
///
/// Properties:
/// * [classField] 
@BuiltValue()
abstract class ClassModel implements Built<ClassModel, ClassModelBuilder> {
  @BuiltValueField(wireName: r'_class')
  String? get classField;

  ClassModel._();

  factory ClassModel([void updates(ClassModelBuilder b)]) = _$ClassModel;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ClassModelBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ClassModel> get serializer => _$ClassModelSerializer();
}

class _$ClassModelSerializer implements PrimitiveSerializer<ClassModel> {
  @override
  final Iterable<Type> types = const [ClassModel, _$ClassModel];

  @override
  final String wireName = r'ClassModel';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ClassModel object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.classField != null) {
      yield r'_class';
      yield serializers.serialize(
        object.classField,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ClassModel object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ClassModelBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'_class':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.classField = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ClassModel deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ClassModelBuilder();
    final serializedList = (serialized as Iterable<Object?>).toList();
    final unhandled = <Object?>[];
    _deserializeProperties(
      serializers,
      serialized,
      specifiedType: specifiedType,
      serializedList: serializedList,
      unhandled: unhandled,
      result: result,
    );
    return result.build();
  }
}

