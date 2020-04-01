// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'pet.dart';

// **************************************************************************
// BuiltValueGenerator
// **************************************************************************

Serializer<Pet> _$petSerializer = new _$PetSerializer();

class _$PetSerializer implements StructuredSerializer<Pet> {
  @override
  final Iterable<Type> types = const [Pet, _$Pet];
  @override
  final String wireName = 'Pet';

  @override
  Iterable<Object> serialize(Serializers serializers, Pet object,
      {FullType specifiedType = FullType.unspecified}) {
    final result = <Object>[];
    if (object.id != null) {
      result
        ..add('id')
        ..add(serializers.serialize(object.id,
            specifiedType: const FullType(int)));
    }
    if (object.category != null) {
      result
        ..add('category')
        ..add(serializers.serialize(object.category,
            specifiedType: const FullType(Category)));
    }
    if (object.name != null) {
      result
        ..add('name')
        ..add(serializers.serialize(object.name,
            specifiedType: const FullType(String)));
    }
    if (object.photoUrls != null) {
      result
        ..add('photoUrls')
        ..add(serializers.serialize(object.photoUrls,
            specifiedType:
                const FullType(BuiltList, const [const FullType(String)])));
    }
    if (object.tags != null) {
      result
        ..add('tags')
        ..add(serializers.serialize(object.tags,
            specifiedType:
                const FullType(BuiltList, const [const FullType(Tag)])));
    }
    if (object.status != null) {
      result
        ..add('status')
        ..add(serializers.serialize(object.status,
            specifiedType: const FullType(String)));
    }
    return result;
  }

  @override
  Pet deserialize(Serializers serializers, Iterable<Object> serialized,
      {FullType specifiedType = FullType.unspecified}) {
    final result = new PetBuilder();

    final iterator = serialized.iterator;
    while (iterator.moveNext()) {
      final key = iterator.current as String;
      iterator.moveNext();
      final dynamic value = iterator.current;
      switch (key) {
        case 'id':
          result.id = serializers.deserialize(value,
              specifiedType: const FullType(int)) as int;
          break;
        case 'category':
          result.category.replace(serializers.deserialize(value,
              specifiedType: const FullType(Category)) as Category);
          break;
        case 'name':
          result.name = serializers.deserialize(value,
              specifiedType: const FullType(String)) as String;
          break;
        case 'photoUrls':
          result.photoUrls.replace(serializers.deserialize(value,
                  specifiedType:
                      const FullType(BuiltList, const [const FullType(String)]))
              as BuiltList<dynamic>);
          break;
        case 'tags':
          result.tags.replace(serializers.deserialize(value,
                  specifiedType:
                      const FullType(BuiltList, const [const FullType(Tag)]))
              as BuiltList<dynamic>);
          break;
        case 'status':
          result.status = serializers.deserialize(value,
              specifiedType: const FullType(String)) as String;
          break;
      }
    }

    return result.build();
  }
}

class _$Pet extends Pet {
  @override
  final int id;
  @override
  final Category category;
  @override
  final String name;
  @override
  final BuiltList<String> photoUrls;
  @override
  final BuiltList<Tag> tags;
  @override
  final String status;

  factory _$Pet([void Function(PetBuilder) updates]) =>
      (new PetBuilder()..update(updates)).build();

  _$Pet._(
      {this.id,
      this.category,
      this.name,
      this.photoUrls,
      this.tags,
      this.status})
      : super._();

  @override
  Pet rebuild(void Function(PetBuilder) updates) =>
      (toBuilder()..update(updates)).build();

  @override
  PetBuilder toBuilder() => new PetBuilder()..replace(this);

  @override
  bool operator ==(Object other) {
    if (identical(other, this)) return true;
    return other is Pet &&
        id == other.id &&
        category == other.category &&
        name == other.name &&
        photoUrls == other.photoUrls &&
        tags == other.tags &&
        status == other.status;
  }

  @override
  int get hashCode {
    return $jf($jc(
        $jc(
            $jc($jc($jc($jc(0, id.hashCode), category.hashCode), name.hashCode),
                photoUrls.hashCode),
            tags.hashCode),
        status.hashCode));
  }

  @override
  String toString() {
    return (newBuiltValueToStringHelper('Pet')
          ..add('id', id)
          ..add('category', category)
          ..add('name', name)
          ..add('photoUrls', photoUrls)
          ..add('tags', tags)
          ..add('status', status))
        .toString();
  }
}

class PetBuilder implements Builder<Pet, PetBuilder> {
  _$Pet _$v;

  int _id;
  int get id => _$this._id;
  set id(int id) => _$this._id = id;

  CategoryBuilder _category;
  CategoryBuilder get category => _$this._category ??= new CategoryBuilder();
  set category(CategoryBuilder category) => _$this._category = category;

  String _name;
  String get name => _$this._name;
  set name(String name) => _$this._name = name;

  ListBuilder<String> _photoUrls;
  ListBuilder<String> get photoUrls =>
      _$this._photoUrls ??= new ListBuilder<String>();
  set photoUrls(ListBuilder<String> photoUrls) => _$this._photoUrls = photoUrls;

  ListBuilder<Tag> _tags;
  ListBuilder<Tag> get tags => _$this._tags ??= new ListBuilder<Tag>();
  set tags(ListBuilder<Tag> tags) => _$this._tags = tags;

  String _status;
  String get status => _$this._status;
  set status(String status) => _$this._status = status;

  PetBuilder();

  PetBuilder get _$this {
    if (_$v != null) {
      _id = _$v.id;
      _category = _$v.category?.toBuilder();
      _name = _$v.name;
      _photoUrls = _$v.photoUrls?.toBuilder();
      _tags = _$v.tags?.toBuilder();
      _status = _$v.status;
      _$v = null;
    }
    return this;
  }

  @override
  void replace(Pet other) {
    if (other == null) {
      throw new ArgumentError.notNull('other');
    }
    _$v = other as _$Pet;
  }

  @override
  void update(void Function(PetBuilder) updates) {
    if (updates != null) updates(this);
  }

  @override
  _$Pet build() {
    _$Pet _$result;
    try {
      _$result = _$v ??
          new _$Pet._(
              id: id,
              category: _category?.build(),
              name: name,
              photoUrls: _photoUrls?.build(),
              tags: _tags?.build(),
              status: status);
    } catch (_) {
      String _$failedField;
      try {
        _$failedField = 'category';
        _category?.build();

        _$failedField = 'photoUrls';
        _photoUrls?.build();
        _$failedField = 'tags';
        _tags?.build();
      } catch (e) {
        throw new BuiltValueNestedFieldError(
            'Pet', _$failedField, e.toString());
      }
      rethrow;
    }
    replace(_$result);
    return _$result;
  }
}

// ignore_for_file: always_put_control_body_on_new_line,always_specify_types,annotate_overrides,avoid_annotating_with_dynamic,avoid_as,avoid_catches_without_on_clauses,avoid_returning_this,lines_longer_than_80_chars,omit_local_variable_types,prefer_expression_function_bodies,sort_constructors_first,test_types_in_equals,unnecessary_const,unnecessary_new
