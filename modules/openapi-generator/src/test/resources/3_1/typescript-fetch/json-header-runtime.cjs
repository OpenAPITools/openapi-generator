const assert = require('assert');
const path = require('path');
const { DefaultApi } = require(path.join(process.argv[2], 'apis', 'DefaultApi.js'));
const { jsonStringifyForHeader } = require(path.join(process.argv[2], 'runtime.js'));
assert.strictEqual(jsonStringifyForHeader(undefined), undefined);

(async () => {
    const api = new DefaultApi();
    for (const value of ['/ascii', '/тест', '/😀', '/é', '/quote"\\']) {
        const request = await api.getJsonRequestOpts({ xJsonArg: { filePath: value }, xPlainArg: 'plain' });
        const headers = new Headers(request.headers);
        const json = headers.get('X-Json-Arg');
        assert.match(json, /^[\x20-\x7e]*$/);
        assert.deepStrictEqual(JSON.parse(json), { file_path: value });
        assert.strictEqual(headers.get('X-Plain-Arg'), 'plain');
        assert.strictEqual(headers.has('X-Optional-Json-Arg'), false);
    }
    const nullable = await api.getJsonRequestOpts({ xJsonArg: {filePath: '/ascii'}, xOptionalJsonArg: null, xPlainArg: 'plain' });
    assert.strictEqual(nullable.headers['X-Optional-Json-Arg'], 'null');
    const vendor = await api.getVendorRequestOpts({ xVendorArg: { filePath: '/тест' } });
    assert.deepStrictEqual(JSON.parse(new Headers(vendor.headers).get('X-Vendor-Arg')), { file_path: '/тест' });
    console.log('JSON header runtime checks passed');
})().catch(error => { console.error(error); process.exitCode = 1; });
