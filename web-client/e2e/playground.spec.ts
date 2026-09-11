import { expect, test, type Page } from '@playwright/test';
import examples from '../../examples/programs.json' with { type: 'json' };

async function openCode(page: Page, source: string) {
  const code = Buffer.from(source, 'utf8').toString('base64');
  await page.goto('/?code=' + encodeURIComponent(code));
  await expect(page.getByRole('textbox', { name: 'L source code' })).toBeVisible();
}
async function run(page: Page) {
  await page.getByRole('button', { name: 'Run', exact: true }).click();
}

test('runs recursive code and shows the real evaluation trace', async ({ page }) => {
  const factorial = examples.find(example => example.id === 'factorial')!;
  await openCode(page, factorial.code);
  await run(page);
  await expect(page.getByTestId('output')).toHaveText('120');
  await page.getByRole('tab', { name: 'Eval Steps' }).click();
  await expect(page.getByRole('tabpanel')).toContainText('Eval App');
});
test('shows positioned errors in the page and editor', async ({ page }) => {
  await openCode(page, 'x = 1\nx + missing');
  await run(page);
  await expect(page.getByRole('alert').first()).toContainText('2:5: Undefined variable: missing');
  await expect(page.locator('.squiggly-error')).not.toHaveCount(0);
});
test('keeps integers exact in output and the environment', async ({ page }) => {
  await openCode(page, 'large = 9223372036854775808\nlarge * large');
  await run(page);
  await expect(page.getByTestId('output')).toHaveText('85070591730234615865843651857942052864');
  await page.getByRole('tab', { name: 'Environment' }).click();
  await expect(page.getByRole('tabpanel')).toContainText('9223372036854775808');
});
test('each run starts fresh', async ({ page }) => {
  await openCode(page, 'x = 41\nx + 1');
  await run(page);
  await expect(page.getByTestId('output')).toHaveText('42');
  const editor = page.getByRole('textbox', { name: 'L source code' });
  await editor.fill('x');
  await run(page);
  await expect(page.getByRole('alert').first()).toContainText('Undefined variable: x');
  await expect(page.getByTestId('output')).not.toContainText('42');
});
test('network failure clears stale results and supports retry', async ({ page }) => {
  await openCode(page, '1 + 2');
  await run(page);
  await expect(page.getByTestId('output')).toHaveText('3');
  await page.route('**/evaluate', route => route.abort());
  await run(page);
  await expect(page.getByRole('alert').first()).toContainText('Cannot reach the Haskell server');
  await expect(page.getByTestId('output')).not.toHaveText('3');
  await page.unroute('**/evaluate');
  await run(page);
  await expect(page.getByTestId('output')).toHaveText('3');
});
test('Unicode lambda share links round-trip', async ({ page }) => {
  await openCode(page, '(λx -> x + 1) 4');
  await run(page);
  await expect(page.getByTestId('output')).toHaveText('5');
});
test('all published examples execute through HTTP', async ({ request }) => {
  for (const example of examples) {
    const response = await request.post('/evaluate', { data: example.code, headers: { 'Content-Type': 'text/plain' } });
    const result = await response.json();
    expect(result.finalError, example.id).toBeNull();
    expect(result.steps.at(-1).output, example.id).toBe(example.expected);
  }
});
test('simultaneous HTTP requests do not share definitions', async ({ request }) => {
  const results = await Promise.all(Array.from({ length: 8 }, async (_, index) => {
    const response = await request.post('/evaluate', { data: `x = ${index}\nx` });
    return (await response.json()).steps.at(-1).output;
  }));
  expect(results).toEqual(Array.from({ length: 8 }, (_, index) => String(index)));
  const result = await (await request.post('/evaluate', { data: 'x' })).json();
  expect(result.diagnostic.code).toBe('name.undefined');
});
test('evaluation exhaustion does not stall later requests', async ({ request }) => {
  const result = await (await request.post('/evaluate', { data: 'loop = \\x -> loop x\nloop 0' })).json();
  expect(result.diagnostic.code).toMatch(/^limit\./);
  const next = await (await request.post('/evaluate', { data: '1 + 2' })).json();
  expect(next.steps.at(-1).output).toBe('3');
});

test('nested JSON integer values stay exact', async ({ request }) => {
  const result = await (await request.post('/evaluate', { data: 'xs = [9223372036854775808, [2], True]\nxs' })).json();
  expect(result.finalEnvironment.xs).toEqual(['9223372036854775808', ['2'], true]);
});
