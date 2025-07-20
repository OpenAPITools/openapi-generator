// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'object_three.dart';

// **************************************************************************
// BuiltValueGenerator
// **************************************************************************

class _$ObjectThree extends ObjectThree {
  @override
  final ObjectOne? objectone;
  @override
  final ObjectTwo? objecttwo;

  factory _$ObjectThree([void Function(ObjectThreeBuilder)? updates]) =>
      (new ObjectThreeBuilder()..update(updates))._build();

  _$ObjectThree._({this.objectone, this.objecttwo}) : super._();

  @override
  ObjectThree rebuild(void Function(ObjectThreeBuilder) updates) =>
      (toBuilder()..update(updates)).build();

  @override
  ObjectThreeBuilder toBuilder() => new ObjectThreeBuilder()..replace(this);

  @override
  bool operator ==(Object other) {
    if (identical(other, this)) return true;
    return other is ObjectThree &&
        objectone == other.objectone &&
        objecttwo == other.objecttwo;
  }

  @override
  int get hashCode {
    var _$hash = 0;
    _$hash = $jc(_$hash, objectone.hashCode);
    _$hash = $jc(_$hash, objecttwo.hashCode);
    _$hash = $jf(_$hash);
    return _$hash;
  }

  @override
  String toString() {
    return (newBuiltValueToStringHelper(r'ObjectThree')
          ..add('objectone', objectone)
          ..add('objecttwo', objecttwo))
        .toString();
  }
}

class ObjectThreeBuilder implements Builder<ObjectThree, ObjectThreeBuilder> {
  _$ObjectThree? _$v;

  ObjectOneBuilder? _objectone;
  ObjectOneBuilder get objectone =>
      _$this._objectone ??= new ObjectOneBuilder();
  set objectone(ObjectOneBuilder? objectone) => _$this._objectone = objectone;

  ObjectTwoBuilder? _objecttwo;
  ObjectTwoBuilder get objecttwo =>
      _$this._objecttwo ??= new ObjectTwoBuilder();
  set objecttwo(ObjectTwoBuilder? objecttwo) => _$this._objecttwo = objecttwo;

  ObjectThreeBuilder() {
    ObjectThree._defaults(this);
  }

  ObjectThreeBuilder get _$this {
    final $v = _$v;
    if ($v != null) {
      _objectone = $v.objectone?.toBuilder();
      _objecttwo = $v.objecttwo?.toBuilder();
      _$v = null;
    }
    return this;
  }

  @override
  void replace(ObjectThree other) {
    ArgumentError.checkNotNull(other, 'other');
    _$v = other as _$ObjectThree;
  }

  @override
  void update(void Function(ObjectThreeBuilder)? updates) {
    if (updates != null) updates(this);
  }

  @override
  ObjectThree build() => _build();

  _$ObjectThree _build() {
    _$ObjectThree _$result;
    try {
      _$result = _$v ??
          new _$ObjectThree._(
              objectone: _objectone?.build(), objecttwo: _objecttwo?.build());
    } catch (_) {
      late String _$failedField;
      try {
        _$failedField = 'objectone';
        _objectone?.build();
        _$failedField = 'objecttwo';
        _objecttwo?.build();
      } catch (e) {
        throw new BuiltValueNestedFieldError(
            r'ObjectThree', _$failedField, e.toString());
      }
      rethrow;
    }
    replace(_$result);
    return _$result;
  }
}

// ignore_for_file: deprecated_member_use_from_same_package,type=lint
