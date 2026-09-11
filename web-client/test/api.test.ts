import { afterEach, test, mock } from 'node:test';
import assert from 'node:assert/strict';
import { evaluateCode, parseResponse } from '../src/services/api.ts';

const originalFetch = globalThis.fetch;
afterEach(() => { globalThis.fetch = originalFetch; mock.timers.reset(); });
const valid = {
  steps: [{ output: '9007199254740993', ast: 'Num 9007199254740993' }],
  finalError: null, diagnostic: null,
  finalEnvironment: { n: '9007199254740993', xs: ['1', true, ['2']] },
  traceLog: [], traceTruncated: false, evaluations: 1,
};

test('accepts exact numbers and nested environment values', () => {
  assert.equal(parseResponse(valid).finalEnvironment.n, '9007199254740993');
});
test('rejects rounded numeric values and malformed response fields', () => {
  for (const invalid of [null, {}, { ...valid, finalEnvironment: { n: Number('9007199254740993') } },
    { ...valid, traceLog: null }, { ...valid, steps: [{ output: 1, ast: '' }] },
    { ...valid, diagnostic: { code: 'parse.expected', message: 'bad', span: {} } }]) {
    assert.throws(() => parseResponse(invalid), /invalid evaluation response/);
  }
});
test('uses a relative endpoint and the real API response', async () => {
  globalThis.fetch = async (url, init) => {
    assert.equal(url, '/evaluate');
    assert.equal(init?.body, '1');
    assert.equal(init?.method, 'POST');
    return Response.json(valid);
  };
  assert.deepEqual(await evaluateCode('1'), valid);
});
test('reports transport and HTTP failures', async () => {
  globalThis.fetch = async () => { throw new TypeError('network error'); };
  await assert.rejects(evaluateCode('1'), /Cannot reach the Haskell server/);
  globalThis.fetch = async () => new Response('', { status: 503 });
  await assert.rejects(evaluateCode('1'), /HTTP 503/);
});
test('keeps the deadline active while reading the response body', async () => {
  mock.timers.enable({ apis: ['setTimeout'] });
  globalThis.fetch = async (_url, init) => new Response(new ReadableStream({
    start(stream) {
      init?.signal?.addEventListener('abort', () => stream.error(new DOMException('Aborted', 'AbortError')));
    },
  }));
  const result = evaluateCode('1');
  await Promise.resolve();
  mock.timers.tick(10001);
  await assert.rejects(result, /timed out after 10 seconds/);
});
test('caller cancellation aborts an outstanding request', async () => {
  globalThis.fetch = async (_url, init) => new Promise((_resolve, reject) => {
    init?.signal?.addEventListener('abort', () => reject(new DOMException('Aborted', 'AbortError')));
  });
  const controller = new AbortController();
  const result = evaluateCode('1', controller.signal);
  controller.abort();
  await assert.rejects(result, { name: 'AbortError' });
});
