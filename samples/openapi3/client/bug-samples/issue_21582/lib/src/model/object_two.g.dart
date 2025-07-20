// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'object_two.dart';

// **************************************************************************
// BuiltValueGenerator
// **************************************************************************

const ObjectTwoAttributeEnum _$objectTwoAttributeEnum_valueone =
    const ObjectTwoAttributeEnum._('valueone');
const ObjectTwoAttributeEnum _$objectTwoAttributeEnum_vauetwo =
    const ObjectTwoAttributeEnum._('vauetwo');

ObjectTwoAttributeEnum _$objectTwoAttributeEnumValueOf(String name) {
  switch (name) {
    case 'valueone':
      return _$objectTwoAttributeEnum_valueone;
    case 'vauetwo':
      return _$objectTwoAttributeEnum_vauetwo;
    default:
      throw new ArgumentError(name);
  }
}

final BuiltSet<ObjectTwoAttributeEnum> _$objectTwoAttributeEnumValues =
    new BuiltSet<ObjectTwoAttributeEnum>(const <ObjectTwoAttributeEnum>[
  _$objectTwoAttributeEnum_valueone,
  _$objectTwoAttributeEnum_vauetwo,
]);

Serializer<ObjectTwoAttributeEnum> _$objectTwoAttributeEnumSerializer =
    new _$ObjectTwoAttributeEnumSerializer();

class _$ObjectTwoAttributeEnumSerializer
    implements PrimitiveSerializer<ObjectTwoAttributeEnum> {
  static const Map<String, Object> _toWire = const <String, Object>{
    'valueone': 'valueone',
    'vauetwo': 'vauetwo',
  };
  static const Map<Object, String> _fromWire = const <Object, String>{
    'valueone': 'valueone',
    'vauetwo': 'vauetwo',
  };

  @override
  final Iterable<Type> types = const <Type>[ObjectTwoAttributeEnum];
  @override
  final String wireName = 'ObjectTwoAttributeEnum';

  @override
  Object serialize(Serializers serializers, ObjectTwoAttributeEnum object,
          {FullType specifiedType = FullType.unspecified}) =>
      _toWire[object.name] ?? object.name;

  @override
  ObjectTwoAttributeEnum deserialize(Serializers serializers, Object serialized,
          {FullType specifiedType = FullType.unspecified}) =>
      ObjectTwoAttributeEnum.valueOf(
          _fromWire[serialized] ?? (serialized is String ? serialized : ''));
}

class _$ObjectTwo extends ObjectTwo {
  @override
  final BuiltSet<ObjectTwoAttributeEnum>? attribute;

  factory _$ObjectTwo([void Function(ObjectTwoBuilder)? updates]) =>
      (new ObjectTwoBuilder()..update(updates))._build();

  _$ObjectTwo._({this.attribute}) : super._();

  @override
  ObjectTwo rebuild(void Function(ObjectTwoBuilder) updates) =>
      (toBuilder()..update(updates)).build();

  @override
  ObjectTwoBuilder toBuilder() => new ObjectTwoBuilder()..replace(this);

  @override
  bool operator ==(Object other) {
    if (identical(other, this)) return true;
    return other is ObjectTwo && attribute == other.attribute;
  }

  @override
  int get hashCode {
    var _$hash = 0;
    _$hash = $jc(_$hash, attribute.hashCode);
    _$hash = $jf(_$hash);
    return _$hash;
  }

  @override
  String toString() {
    return (newBuiltValueToStringHelper(r'ObjectTwo')
          ..add('attribute', attribute))
        .toString();
  }
}

class ObjectTwoBuilder implements Builder<ObjectTwo, ObjectTwoBuilder> {
  _$ObjectTwo? _$v;

  SetBuilder<ObjectTwoAttributeEnum>? _attribute;
  SetBuilder<ObjectTwoAttributeEnum> get attribute =>
      _$this._attribute ??= new SetBuilder<ObjectTwoAttributeEnum>();
  set attribute(SetBuilder<ObjectTwoAttributeEnum>? attribute) =>
      _$this._attribute = attribute;

  ObjectTwoBuilder() {
    ObjectTwo._defaults(this);
  }

  ObjectTwoBuilder get _$this {
    final $v = _$v;
    if ($v != null) {
      _attribute = $v.attribute?.toBuilder();
      _$v = null;
    }
    return this;
  }

  @override
  void replace(ObjectTwo other) {
    ArgumentError.checkNotNull(other, 'other');
    _$v = other as _$ObjectTwo;
  }

  @override
  void update(void Function(ObjectTwoBuilder)? updates) {
    if (updates != null) updates(this);
  }

  @override
  ObjectTwo build() => _build();

  _$ObjectTwo _build() {
    _$ObjectTwo _$result;
    try {
      _$result = _$v ?? new _$ObjectTwo._(attribute: _attribute?.build());
    } catch (_) {
      late String _$failedField;
      try {
        _$failedField = 'attribute';
        _attribute?.build();
      } catch (e) {
        throw new BuiltValueNestedFieldError(
            r'ObjectTwo', _$failedField, e.toString());
      }
      rethrow;
    }
    replace(_$result);
    return _$result;
  }
}

// ignore_for_file: deprecated_member_use_from_same_package,type=lint
