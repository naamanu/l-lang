export type RuntimeValue = string | boolean | RuntimeValue[];
export interface Position { line: number; column: number }
export interface Diagnostic {
  code: string;
  message: string;
  span: { start: Position; end: Position };
}
export interface StepResult { output: string; ast: string }
export interface EvaluationResponse {
  steps: StepResult[];
  finalError: string | null;
  diagnostic: Diagnostic | null;
  finalEnvironment: Record<string, RuntimeValue>;
  traceLog: string[];
  traceTruncated: boolean;
  evaluations: number;
}

function object(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}
function position(value: unknown): value is Position {
  return object(value) && Number.isInteger(value.line) && Number.isInteger(value.column)
    && Number(value.line) > 0 && Number(value.column) > 0;
}
function diagnostic(value: unknown): value is Diagnostic {
  return object(value) && typeof value.code === 'string' && typeof value.message === 'string'
    && object(value.span) && position(value.span.start) && position(value.span.end);
}
function runtimeValue(value: unknown, depth = 0): value is RuntimeValue {
  return typeof value === 'string' || typeof value === 'boolean'
    || (depth < 1000 && Array.isArray(value) && value.every(item => runtimeValue(item, depth + 1)));
}
export function parseResponse(value: unknown): EvaluationResponse {
  if (!object(value) || !Array.isArray(value.steps)
    || !value.steps.every(step => object(step) && typeof step.output === 'string' && typeof step.ast === 'string')
    || !(value.finalError === null || typeof value.finalError === 'string')
    || !(value.diagnostic === null || diagnostic(value.diagnostic))
    || !object(value.finalEnvironment) || !Object.values(value.finalEnvironment).every(item => runtimeValue(item))
    || !Array.isArray(value.traceLog) || !value.traceLog.every(item => typeof item === 'string')
    || typeof value.traceTruncated !== 'boolean' || !Number.isInteger(value.evaluations) || Number(value.evaluations) < 0) {
    throw new Error('The server returned an invalid evaluation response.');
  }
  return value as unknown as EvaluationResponse;
}

export async function evaluateCode(code: string, signal?: AbortSignal): Promise<EvaluationResponse> {
  const controller = new AbortController();
  const cancel = () => controller.abort();
  signal?.addEventListener('abort', cancel, { once: true });
  if (signal?.aborted) controller.abort();
  let timedOut = false;
  const timer = setTimeout(() => { timedOut = true; controller.abort(); }, 10000);
  try {
    const response = await fetch('/evaluate', {
      method: 'POST', body: code, signal: controller.signal,
      headers: { 'Content-Type': 'text/plain;charset=UTF-8' },
    });
    if (!response.ok) throw new Error(`Evaluation server returned HTTP ${response.status}.`);
    return parseResponse(await response.json());
  } catch (error) {
    if (timedOut) throw new Error('The evaluation request timed out after 10 seconds.');
    if (controller.signal.aborted) throw error;
    if (error instanceof TypeError) throw new Error('Cannot reach the Haskell server. Check that it is running and try again.');
    throw error;
  } finally {
    clearTimeout(timer);
    signal?.removeEventListener('abort', cancel);
  }
}
