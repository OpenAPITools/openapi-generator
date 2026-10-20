import { defineConfig } from '@playwright/test';

/**
 * Playwright configuration for the generated API tests.
 *
 * Override the target server with the `BASE_URL` environment variable, e.g.
 *   BASE_URL=http://localhost:8080 npx playwright test
 *
 * @see https://playwright.dev/docs/test-configuration
 */
export default defineConfig({
    testDir: './spec',
    testMatch: '**/*.spec.ts',
    fullyParallel: true,
    forbidOnly: !!process.env.CI,
    retries: process.env.CI ? 2 : 0,
    reporter: 'list',
    use: {
        baseURL: process.env.BASE_URL ?? 'http://petstore.swagger.io/v2',
    },
});
