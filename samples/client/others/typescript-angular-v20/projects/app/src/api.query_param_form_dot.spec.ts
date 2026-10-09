import {BaseService as DotBaseService} from '../../../builds/query-param-form-dot/api.base.service';
import {OpenApiHttpParams, QueryParamStyle} from '../../../builds/query-param-form-dot/query.params';

class QuerySerializer extends DotBaseService {
  serialize(value: unknown, style = QueryParamStyle.Form, explode = true): string {
    return this.addToHttpParams(new OpenApiHttpParams(), 'filter', value, style, explode).toHttpParams().toString();
  }
}

describe('Opt-in form query object dot notation', () => {
  const serializer = new QuerySerializer();

  it('preserves the parameter name and nested paths without collisions', () => {
    expect(serializer.serialize({name: 'Alice', first: {id: 1}, second: {id: 2}}))
      .toBe('filter.name=Alice&filter.first.id=1&filter.second.id=2');
  });

  it('preserves repeated primitive arrays and sets', () => {
    expect(serializer.serialize({ids: [1, 2], tags: new Set(['a', 'b'])}))
      .toBe('filter.ids=1&filter.ids=2&filter.tags=a&filter.tags=b');
  });

  it('preserves dates, escaping, false and zero and omits nullish values', () => {
    expect(serializer.serialize({start: new Date('2026-01-02T03:04:05Z'), name: 'A & B', zero: 0, no: false, missing: null, absent: undefined}))
      .toBe('filter.start=2026-01-02T03%3A04%3A05.000Z&filter.name=A%20%26%20B&filter.zero=0&filter.no=false');
  });

  it('does not change top-level primitives or arrays', () => {
    expect(serializer.serialize('Alice')).toBe('filter=Alice');
    expect(serializer.serialize([1, 2])).toBe('filter=1&filter=2');
  });

  it('does not change non-exploded form objects or other styles', () => {
    expect(serializer.serialize({name: 'Alice'}, QueryParamStyle.Form, false)).toBe('filter=name,Alice');
    expect(serializer.serialize({name: 'Alice'}, QueryParamStyle.DeepObject)).toBe('filter%5Bname%5D=Alice');
    expect(serializer.serialize({name: 'Alice'}, QueryParamStyle.Json)).toBe('filter=%7B%22name%22%3A%22Alice%22%7D');
    expect(serializer.serialize([1, 2], QueryParamStyle.SpaceDelimited, false)).toBe('filter=1 2');
    expect(serializer.serialize([1, 2], QueryParamStyle.PipeDelimited, false)).toBe('filter=1|2');
  });
});
