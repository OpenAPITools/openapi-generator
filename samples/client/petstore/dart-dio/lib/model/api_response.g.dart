// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'api_response.dart';

// **************************************************************************
// BuiltValueGenerator
// **************************************************************************

Serializer<ApiResponse> _$apiResponseSerializer = new _$ApiResponseSerializer();

class _$ApiResponseSerializer implements StructuredSerializer<ApiResponse> {
  @override
  final Iterable<Type> types = const [ApiResponse, _$ApiResponse];
  @override
  final String wireName = 'ApiResponse';

  @override
  Iterable<Object> serialize(Serializers serializers, ApiResponse object,
      {FullType specifiedType = FullType.unspecified}) {
    final result = <Object>[
      'code',
      serializers.serialize(object.code, specifiedType: const FullType(int)),
      'type',
      serializers.serialize(object.type, specifiedType: const FullType(String)),
      'message',
      serializers.serialize(object.message,
          specifiedType: const FullType(String)),
    ];

    return result;
  }

  @override
  ApiResponse deserialize(Serializers serializers, Iterable<Object> serialized,
      {FullType specifiedType = FullType.unspecified}) {
    final result = new ApiResponseBuilder();

    final iterator = serialized.iterator;
    while (iterator.moveNext()) {
      final key = iterator.current as String;
      iterator.moveNext();
      final dynamic value = iterator.current;
      switch (key) {
        case 'code':
          result.code = serializers.deserialize(value,
              specifiedType: const FullType(int)) as int;
          break;
        case 'type':
          result.type = serializers.deserialize(value,
              specifiedType: const FullType(String)) as String;
          break;
        case 'message':
          result.message = serializers.deserialize(value,
              specifiedType: const FullType(String)) as String;
          break;
      }
    }

    return result.build();
  }
}

class _$ApiResponse extends ApiResponse {
  @override
  final int code;
  @override
  final String type;
  @override
  final String message;

  factory _$ApiResponse([void Function(ApiResponseBuilder) updates]) =>
      (new ApiResponseBuilder()..update(updates)).build();

  _$ApiResponse._({this.code, this.type, this.message}) : super._() {
    if (code == null) {
      throw new BuiltValueNullFieldError('ApiResponse', 'code');
    }
    if (type == null) {
      throw new BuiltValueNullFieldError('ApiResponse', 'type');
    }
    if (message == null) {
      throw new BuiltValueNullFieldError('ApiResponse', 'message');
    }
  }

  @override
  ApiResponse rebuild(void Function(ApiResponseBuilder) updates) =>
      (toBuilder()..update(updates)).build();

  @override
  ApiResponseBuilder toBuilder() => new ApiResponseBuilder()..replace(this);

  @override
  bool operator ==(Object other) {
    if (identical(other, this)) return true;
    return other is ApiResponse &&
        code == other.code &&
        type == other.type &&
        message == other.message;
  }

  @override
  int get hashCode {
    return $jf(
        $jc($jc($jc(0, code.hashCode), type.hashCode), message.hashCode));
  }

  @override
  String toString() {
    return (newBuiltValueToStringHelper('ApiResponse')
          ..add('code', code)
          ..add('type', type)
          ..add('message', message))
        .toString();
  }
}

class ApiResponseBuilder implements Builder<ApiResponse, ApiResponseBuilder> {
  _$ApiResponse _$v;

  int _code;
  int get code => _$this._code;
  set code(int code) => _$this._code = code;

  String _type;
  String get type => _$this._type;
  set type(String type) => _$this._type = type;

  String _message;
  String get message => _$this._message;
  set message(String message) => _$this._message = message;

  ApiResponseBuilder();

  ApiResponseBuilder get _$this {
    if (_$v != null) {
      _code = _$v.code;
      _type = _$v.type;
      _message = _$v.message;
      _$v = null;
    }
    return this;
  }

  @override
  void replace(ApiResponse other) {
    if (other == null) {
      throw new ArgumentError.notNull('other');
    }
    _$v = other as _$ApiResponse;
  }

  @override
  void update(void Function(ApiResponseBuilder) updates) {
    if (updates != null) updates(this);
  }

  @override
  _$ApiResponse build() {
    final _$result =
        _$v ?? new _$ApiResponse._(code: code, type: type, message: message);
    replace(_$result);
    return _$result;
  }
}

// ignore_for_file: always_put_control_body_on_new_line,always_specify_types,annotate_overrides,avoid_annotating_with_dynamic,avoid_as,avoid_catches_without_on_clauses,avoid_returning_this,lines_longer_than_80_chars,omit_local_variable_types,prefer_expression_function_bodies,sort_constructors_first,test_types_in_equals,unnecessary_const,unnecessary_new
