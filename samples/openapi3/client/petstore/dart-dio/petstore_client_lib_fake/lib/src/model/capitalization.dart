//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'capitalization.g.dart';

/// Capitalization
///
/// Properties:
/// * [smallcamel] 
/// * [capitalcamel] 
/// * [smallSnake] 
/// * [capitalSnake] 
/// * [scaEthFlowPoints] 
/// * [attName] - Name of the pet 
@BuiltValue()
abstract class Capitalization implements Built<Capitalization, CapitalizationBuilder> {
  @BuiltValueField(wireName: r'smallCamel')
  String? get smallcamel;

  @BuiltValueField(wireName: r'CapitalCamel')
  String? get capitalcamel;

  @BuiltValueField(wireName: r'small_Snake')
  String? get smallSnake;

  @BuiltValueField(wireName: r'Capital_Snake')
  String? get capitalSnake;

  @BuiltValueField(wireName: r'SCA_ETH_Flow_Points')
  String? get scaEthFlowPoints;

  /// Name of the pet 
  @BuiltValueField(wireName: r'ATT_NAME')
  String? get attName;

  Capitalization._();

  factory Capitalization([void updates(CapitalizationBuilder b)]) = _$Capitalization;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(CapitalizationBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Capitalization> get serializer => _$CapitalizationSerializer();
}

class _$CapitalizationSerializer implements PrimitiveSerializer<Capitalization> {
  @override
  final Iterable<Type> types = const [Capitalization, _$Capitalization];

  @override
  final String wireName = r'Capitalization';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Capitalization object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.smallcamel != null) {
      yield r'smallCamel';
      yield serializers.serialize(
        object.smallcamel,
        specifiedType: const FullType(String),
      );
    }
    if (object.capitalcamel != null) {
      yield r'CapitalCamel';
      yield serializers.serialize(
        object.capitalcamel,
        specifiedType: const FullType(String),
      );
    }
    if (object.smallSnake != null) {
      yield r'small_Snake';
      yield serializers.serialize(
        object.smallSnake,
        specifiedType: const FullType(String),
      );
    }
    if (object.capitalSnake != null) {
      yield r'Capital_Snake';
      yield serializers.serialize(
        object.capitalSnake,
        specifiedType: const FullType(String),
      );
    }
    if (object.scaEthFlowPoints != null) {
      yield r'SCA_ETH_Flow_Points';
      yield serializers.serialize(
        object.scaEthFlowPoints,
        specifiedType: const FullType(String),
      );
    }
    if (object.attName != null) {
      yield r'ATT_NAME';
      yield serializers.serialize(
        object.attName,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Capitalization object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required CapitalizationBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'smallCamel':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.smallcamel = valueDes;
          break;
        case r'CapitalCamel':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.capitalcamel = valueDes;
          break;
        case r'small_Snake':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.smallSnake = valueDes;
          break;
        case r'Capital_Snake':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.capitalSnake = valueDes;
          break;
        case r'SCA_ETH_Flow_Points':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.scaEthFlowPoints = valueDes;
          break;
        case r'ATT_NAME':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.attName = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Capitalization deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = CapitalizationBuilder();
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

