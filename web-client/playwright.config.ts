import { defineConfig } from '@playwright/test';
import path from 'node:path';

const port = process.env.L_LANG_TEST_PORT ?? '3107';
const baseURL = `http://127.0.0.1:${port}`;

export default defineConfig({
  testDir: './e2e',
  fullyParallel: true,
  workers: 2,
  use: {
    baseURL,
    channel: process.env.PLAYWRIGHT_CHANNEL,
    trace: 'retain-on-failure',
  },
  webServer: {
    command: process.env.L_LANG_SERVER_COMMAND ?? 'stack run -- -w',
    cwd: path.resolve('..'),
    url: baseURL,
    env: { PORT: port },
    reuseExistingServer: false,
    gracefulShutdown: { signal: 'SIGTERM', timeout: 5000 },
    timeout: 120000,
  },
});
