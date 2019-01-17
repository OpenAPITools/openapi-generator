// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'api_response.dart';

// **************************************************************************
// JaguarSerializerGenerator
// **************************************************************************

abstract class _$ApiResponseSerializer implements Serializer<ApiResponse> {
  @override
  Map<String, dynamic> toMap(ApiResponse model) {
    if (model == null) return null;
    Map<String, dynamic> ret = <String, dynamic>{};
    setMapValue(ret, 'code', model.code);
    setMapValue(ret, 'type', model.type);
    setMapValue(ret, 'message', model.message);
    return ret;
  }

  @override
  ApiResponse fromMap(Map map) {
    if (map == null) return null;
    final obj = new ApiResponse(
        code: map['code'] as int ?? getJserDefault('code'),
        type: map['type'] as String ?? getJserDefault('type'),
        message: map['message'] as String ?? getJserDefault('message'));
    return obj;
  }
}
