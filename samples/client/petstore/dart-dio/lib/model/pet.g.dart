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
    final result = <Object>[
      'id',
      serializers.serialize(object.id, specifiedType: const FullType(int)),
      'category',
      serializers.serialize(object.category,
          specifiedType: const FullType(Category)),
      'name',
      serializers.serialize(object.name, specifiedType: const FullType(String)),
      'photoUrls',
      serializers.serialize(object.photoUrls,
          specifiedType: const FullType(List, const [const FullType(String)])),
      'tags',
      serializers.serialize(object.tags,
          specifiedType: const FullType(List, const [const FullType(Tag)])),
      'status',
      serializers.serialize(object.status,
          specifiedType: const FullType(String)),
    ];

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
          result.photoUrls = serializers.deserialize(value,
                  specifiedType:
                      const FullType(List, const [const FullType(String)]))
              as List<String>;
          break;
        case 'tags':
          result.tags = serializers.deserialize(value,
                  specifiedType:
                      const FullType(List, const [const FullType(Tag)]))
              as List<Tag>;
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
  final List<String> photoUrls;
  @override
  final List<Tag> tags;
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
      : super._() {
    if (id == null) {
      throw new BuiltValueNullFieldError('Pet', 'id');
    }
    if (category == null) {
      throw new BuiltValueNullFieldError('Pet', 'category');
    }
    if (name == null) {
      throw new BuiltValueNullFieldError('Pet', 'name');
    }
    if (photoUrls == null) {
      throw new BuiltValueNullFieldError('Pet', 'photoUrls');
    }
    if (tags == null) {
      throw new BuiltValueNullFieldError('Pet', 'tags');
    }
    if (status == null) {
      throw new BuiltValueNullFieldError('Pet', 'status');
    }
  }

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

  List<String> _photoUrls;
  List<String> get photoUrls => _$this._photoUrls;
  set photoUrls(List<String> photoUrls) => _$this._photoUrls = photoUrls;

  List<Tag> _tags;
  List<Tag> get tags => _$this._tags;
  set tags(List<Tag> tags) => _$this._tags = tags;

  String _status;
  String get status => _$this._status;
  set status(String status) => _$this._status = status;

  PetBuilder();

  PetBuilder get _$this {
    if (_$v != null) {
      _id = _$v.id;
      _category = _$v.category?.toBuilder();
      _name = _$v.name;
      _photoUrls = _$v.photoUrls;
      _tags = _$v.tags;
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
              category: category.build(),
              name: name,
              photoUrls: photoUrls,
              tags: tags,
              status: status);
    } catch (_) {
      String _$failedField;
      try {
        _$failedField = 'category';
        category.build();
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
