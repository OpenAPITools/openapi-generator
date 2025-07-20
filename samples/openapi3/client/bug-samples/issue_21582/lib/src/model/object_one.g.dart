// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'object_one.dart';

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

class _$ObjectOne extends ObjectOne {
  @override
  final BuiltSet<ObjectTwoAttributeEnum>? attribute;

  factory _$ObjectOne([void Function(ObjectOneBuilder)? updates]) =>
      (new ObjectOneBuilder()..update(updates))._build();

  _$ObjectOne._({this.attribute}) : super._();

  @override
  ObjectOne rebuild(void Function(ObjectOneBuilder) updates) =>
      (toBuilder()..update(updates)).build();

  @override
  ObjectOneBuilder toBuilder() => new ObjectOneBuilder()..replace(this);

  @override
  bool operator ==(Object other) {
    if (identical(other, this)) return true;
    return other is ObjectOne && attribute == other.attribute;
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
    return (newBuiltValueToStringHelper(r'ObjectOne')
          ..add('attribute', attribute))
        .toString();
  }
}

class ObjectOneBuilder implements Builder<ObjectOne, ObjectOneBuilder> {
  _$ObjectOne? _$v;

  SetBuilder<ObjectTwoAttributeEnum>? _attribute;
  SetBuilder<ObjectTwoAttributeEnum> get attribute =>
      _$this._attribute ??= new SetBuilder<ObjectTwoAttributeEnum>();
  set attribute(SetBuilder<ObjectTwoAttributeEnum>? attribute) =>
      _$this._attribute = attribute;

  ObjectOneBuilder() {
    ObjectOne._defaults(this);
  }

  ObjectOneBuilder get _$this {
    final $v = _$v;
    if ($v != null) {
      _attribute = $v.attribute?.toBuilder();
      _$v = null;
    }
    return this;
  }

  @override
  void replace(ObjectOne other) {
    ArgumentError.checkNotNull(other, 'other');
    _$v = other as _$ObjectOne;
  }

  @override
  void update(void Function(ObjectOneBuilder)? updates) {
    if (updates != null) updates(this);
  }

  @override
  ObjectOne build() => _build();

  _$ObjectOne _build() {
    _$ObjectOne _$result;
    try {
      _$result = _$v ?? new _$ObjectOne._(attribute: _attribute?.build());
    } catch (_) {
      late String _$failedField;
      try {
        _$failedField = 'attribute';
        _attribute?.build();
      } catch (e) {
        throw new BuiltValueNestedFieldError(
            r'ObjectOne', _$failedField, e.toString());
      }
      rethrow;
    }
    replace(_$result);
    return _$result;
  }
}

// ignore_for_file: deprecated_member_use_from_same_package,type=lint
