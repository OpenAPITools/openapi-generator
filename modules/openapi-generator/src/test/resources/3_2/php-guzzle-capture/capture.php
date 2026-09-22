<?php
// Wire-level verification harness for OpenAPI 3.2 support in the generated
// `php` (guzzle) client. PhpClientCodegenTest copies this file into the
// generated client directory and runs it with `php capture.php <generated dir>`.
//
// A raw TCP listener (spawned as a child PHP process) records the HTTP request
// line for every generated call: standard methods go through Guzzle's normal
// dispatch, while query/additionalOperations methods and `in: querystring`
// parameters must reach the wire verbatim (no up-casing, no re-encoding).
$dir = $argv[1] ?? null;
if ($dir === null) { fwrite(STDERR, "usage: capture.php <generated client dir>\n"); exit(1); }

// --- raw TCP listener: prints "PORT <n>" once, then "LINE <request-line>" per
// connection. Reads the head + Content-Length body, answers 204, closes.
$serverCode = <<<'PHPEOF'
$srv = stream_socket_server("tcp://127.0.0.1:0", $errno, $errstr);
if (!$srv) { fwrite(STDERR, "listen failed: $errstr\n"); exit(1); }
$name = stream_socket_get_name($srv, false);
fwrite(STDOUT, "PORT " . substr($name, strrpos($name, ':') + 1) . "\n"); fflush(STDOUT);
while (true) {
    $conn = @stream_socket_accept($srv, 30);
    if (!$conn) break;
    stream_set_timeout($conn, 5);
    $buf = '';
    while (($pos = strpos($buf, "\r\n\r\n")) === false) {
        $chunk = fread($conn, 8192);
        if ($chunk === false || $chunk === '') break;
        $buf .= $chunk;
    }
    $head = $pos !== false ? substr($buf, 0, $pos) : $buf;
    $bodyStart = $pos !== false ? $pos + 4 : strlen($buf);
    $len = 0;
    if (preg_match('/^Content-Length:\s*(\d+)/mi', $head, $m)) $len = (int)$m[1];
    $body = substr($buf, $bodyStart);
    while (strlen($body) < $len) {
        $chunk = fread($conn, 8192);
        if ($chunk === false || $chunk === '') break;
        $body .= $chunk;
    }
    $lines = explode("\r\n", $head);
    fwrite(STDOUT, "LINE " . $lines[0] . " [CL=$len]" . ($body !== '' ? " BODY=$body" : '') . "\n"); fflush(STDOUT);
    fwrite($conn, "HTTP/1.1 204 No Content\r\nContent-Length: 0\r\nConnection: close\r\n\r\n");
    fclose($conn);
}
PHPEOF;

$proc = proc_open([PHP_BINARY, '-r', $serverCode],
    [0 => ['pipe', 'r'], 1 => ['pipe', 'w'], 2 => ['pipe', 'w']], $pipes);
if (!is_resource($proc)) { fwrite(STDERR, "cannot spawn listener\n"); exit(1); }
fclose($pipes[0]);
$portLine = fgets($pipes[1]);
if (!preg_match('/^PORT (\d+)/', (string)$portLine, $m)) {
    fwrite(STDERR, "no port from listener: $portLine\n"); exit(1);
}
$port = (int)$m[1];
stream_set_blocking($pipes[1], false);

require $dir . '/vendor/autoload.php';

$config = (new OpenAPI\Client\Configuration())->setHost("http://127.0.0.1:$port");
$api = new OpenAPI\Client\Api\DefaultApi(new GuzzleHttp\Client(), $config);

// `in: querystring` callers pass the query component without the leading `?`;
// embedded `//` must reach the wire intact.
$calls = [
    function () use ($api) { $api->queryPets('a=1&u=http://h//p'); },
    function () use ($api) { $api->customPets(); },
    function () use ($api) { $api->checkFetchPets(); },
    function () use ($api) { $api->purgePets(); },
    function () use ($api) { $api->hashPets(); },
    function () use ($api) { $api->listPets(); },
    // QUERY carrying a request body; its querystring param is named `uri`,
    // which collides with the template-internal request-uri variable
    function () use ($api) { $api->searchItems('k=v', ['a' => 1]); },
    function () use ($api) { $api->reportItems('r=1'); },
    function () use ($api) { $api->propPatch('<x/>'); },
];
foreach ($calls as $call) {
    try { $call(); } catch (\Throwable $e) { fwrite(STDERR, "call failed: {$e->getMessage()}\n"); }
}

$expected = [
    'QUERY /pets?a=1&u=http://h//p HTTP/1.1 [CL=0]',
    'customMethod /pets HTTP/1.1 [CL=0]',
    'CHECK&FETCH /pets HTTP/1.1 [CL=0]',
    'PURGE /pets HTTP/1.1 [CL=0]',
    'X#Y /pets HTTP/1.1 [CL=0]',
    'GET /pets HTTP/1.1 [CL=0]',
    'QUERY /items?k=v HTTP/1.1 [CL=7] BODY={"a":1}',
    'REPORT /report?r=1 HTTP/1.1 [CL=0]',
    'PROPPATCH /report HTTP/1.1 [CL=4] BODY=<x/>',
];
$got = [];
$deadline = microtime(true) + 30;
while (count($got) < count($expected) && microtime(true) < $deadline) {
    $line = fgets($pipes[1]);
    if ($line !== false && preg_match('/^LINE (.*)/', rtrim($line), $m)) {
        $got[] = $m[1];
    } else {
        usleep(10000);
    }
}
proc_terminate($proc);
proc_close($proc);

$ok = $got === $expected;
foreach ($expected as $i => $want) {
    if (($got[$i] ?? null) !== $want) {
        fwrite(STDERR, "FAIL: got " . var_export($got[$i] ?? null, true) . ", want " . var_export($want, true) . "\n");
    }
}
fwrite(STDOUT, $ok ? "CAPTURE-PASS\n" : "CAPTURE-FAIL\n");
exit($ok ? 0 : 1);
