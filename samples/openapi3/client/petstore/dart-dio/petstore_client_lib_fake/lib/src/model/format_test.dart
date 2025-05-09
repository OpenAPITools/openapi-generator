//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'dart:typed_data';
import 'package:openapi/src/model/date.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'format_test.g.dart';

/// FormatTest
///
/// Properties:
/// * [integer] 
/// * [int32] 
/// * [int64] 
/// * [number] 
/// * [float] 
/// * [double_] 
/// * [decimal] 
/// * [string] 
/// * [byte] 
/// * [binary] 
/// * [date] 
/// * [dateTime] 
/// * [uuid] 
/// * [password] 
/// * [patternWithDigits] - A string that is a 10 digit number. Can have leading zeros.
/// * [patternWithDigitsAndDelimiter] - A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
@BuiltValue()
abstract class FormatTest implements Built<FormatTest, FormatTestBuilder> {
  @BuiltValueField(wireName: r'integer')
  int? get integer;

  @BuiltValueField(wireName: r'int32')
  int? get int32;

  @BuiltValueField(wireName: r'int64')
  int? get int64;

  @BuiltValueField(wireName: r'number')
  num get number;

  @BuiltValueField(wireName: r'float')
  double? get float;

  @BuiltValueField(wireName: r'double')
  double? get double_;

  @BuiltValueField(wireName: r'decimal')
  double? get decimal;

  @BuiltValueField(wireName: r'string')
  String? get string;

  @BuiltValueField(wireName: r'byte')
  String get byte;

  @BuiltValueField(wireName: r'binary')
  Uint8List? get binary;

  @BuiltValueField(wireName: r'date')
  Date get date;

  @BuiltValueField(wireName: r'dateTime')
  DateTime? get dateTime;

  @BuiltValueField(wireName: r'uuid')
  String? get uuid;

  @BuiltValueField(wireName: r'password')
  String get password;

  /// A string that is a 10 digit number. Can have leading zeros.
  @BuiltValueField(wireName: r'pattern_with_digits')
  String? get patternWithDigits;

  /// A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
  @BuiltValueField(wireName: r'pattern_with_digits_and_delimiter')
  String? get patternWithDigitsAndDelimiter;

  FormatTest._();

  factory FormatTest([void updates(FormatTestBuilder b)]) = _$FormatTest;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FormatTestBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FormatTest> get serializer => _$FormatTestSerializer();
}

class _$FormatTestSerializer implements PrimitiveSerializer<FormatTest> {
  @override
  final Iterable<Type> types = const [FormatTest, _$FormatTest];

  @override
  final String wireName = r'FormatTest';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FormatTest object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.integer != null) {
      yield r'integer';
      yield serializers.serialize(
        object.integer,
        specifiedType: const FullType(int),
      );
    }
    if (object.int32 != null) {
      yield r'int32';
      yield serializers.serialize(
        object.int32,
        specifiedType: const FullType(int),
      );
    }
    if (object.int64 != null) {
      yield r'int64';
      yield serializers.serialize(
        object.int64,
        specifiedType: const FullType(int),
      );
    }
    yield r'number';
    yield serializers.serialize(
      object.number,
      specifiedType: const FullType(num),
    );
    if (object.float != null) {
      yield r'float';
      yield serializers.serialize(
        object.float,
        specifiedType: const FullType(double),
      );
    }
    if (object.double_ != null) {
      yield r'double';
      yield serializers.serialize(
        object.double_,
        specifiedType: const FullType(double),
      );
    }
    if (object.decimal != null) {
      yield r'decimal';
      yield serializers.serialize(
        object.decimal,
        specifiedType: const FullType(double),
      );
    }
    if (object.string != null) {
      yield r'string';
      yield serializers.serialize(
        object.string,
        specifiedType: const FullType(String),
      );
    }
    yield r'byte';
    yield serializers.serialize(
      object.byte,
      specifiedType: const FullType(String),
    );
    if (object.binary != null) {
      yield r'binary';
      yield serializers.serialize(
        object.binary,
        specifiedType: const FullType(Uint8List),
      );
    }
    yield r'date';
    yield serializers.serialize(
      object.date,
      specifiedType: const FullType(Date),
    );
    if (object.dateTime != null) {
      yield r'dateTime';
      yield serializers.serialize(
        object.dateTime,
        specifiedType: const FullType(DateTime),
      );
    }
    if (object.uuid != null) {
      yield r'uuid';
      yield serializers.serialize(
        object.uuid,
        specifiedType: const FullType(String),
      );
    }
    yield r'password';
    yield serializers.serialize(
      object.password,
      specifiedType: const FullType(String),
    );
    if (object.patternWithDigits != null) {
      yield r'pattern_with_digits';
      yield serializers.serialize(
        object.patternWithDigits,
        specifiedType: const FullType(String),
      );
    }
    if (object.patternWithDigitsAndDelimiter != null) {
      yield r'pattern_with_digits_and_delimiter';
      yield serializers.serialize(
        object.patternWithDigitsAndDelimiter,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    FormatTest object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FormatTestBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'integer':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.integer = valueDes;
          break;
        case r'int32':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.int32 = valueDes;
          break;
        case r'int64':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.int64 = valueDes;
          break;
        case r'number':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(num),
          ) as num;
          result.number = valueDes;
          break;
        case r'float':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(double),
          ) as double;
          result.float = valueDes;
          break;
        case r'double':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(double),
          ) as double;
          result.double_ = valueDes;
          break;
        case r'decimal':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(double),
          ) as double;
          result.decimal = valueDes;
          break;
        case r'string':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.string = valueDes;
          break;
        case r'byte':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.byte = valueDes;
          break;
        case r'binary':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(Uint8List),
          ) as Uint8List;
          result.binary = valueDes;
          break;
        case r'date':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(Date),
          ) as Date;
          result.date = valueDes;
          break;
        case r'dateTime':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(DateTime),
          ) as DateTime;
          result.dateTime = valueDes;
          break;
        case r'uuid':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.uuid = valueDes;
          break;
        case r'password':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.password = valueDes;
          break;
        case r'pattern_with_digits':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.patternWithDigits = valueDes;
          break;
        case r'pattern_with_digits_and_delimiter':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.patternWithDigitsAndDelimiter = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  FormatTest deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FormatTestBuilder();
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

