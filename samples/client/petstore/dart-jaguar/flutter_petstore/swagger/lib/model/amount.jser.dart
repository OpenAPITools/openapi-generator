// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'amount.dart';

// **************************************************************************
// JaguarSerializerGenerator
// **************************************************************************

abstract class _$AmountSerializer implements Serializer<Amount> {
  Serializer<Currency> __currencySerializer;
  Serializer<Currency> get _currencySerializer =>
      __currencySerializer ??= new CurrencySerializer();
  @override
  Map<String, dynamic> toMap(Amount model) {
    if (model == null) return null;
    Map<String, dynamic> ret = <String, dynamic>{};
    setMapValue(ret, 'value', model.value);
    setMapValue(ret, 'currency', _currencySerializer.toMap(model.currency));
    return ret;
  }

  @override
  Amount fromMap(Map map) {
    if (map == null) return null;
    final obj = new Amount(
        value: map['value'] as double ?? getJserDefault('value'),
        currency: _currencySerializer.fromMap(map['currency'] as Map) ??
            getJserDefault('currency'));
    return obj;
  }
}
