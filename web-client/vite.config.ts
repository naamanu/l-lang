import tailwindcss from '@tailwindcss/vite';
import react from '@vitejs/plugin-react-swc';
import path from 'node:path';
import { defineConfig } from 'vite';

export default defineConfig({
  plugins: [react(), tailwindcss()],
  server: {
    host: '127.0.0.1',
    proxy: { '/evaluate': 'http://127.0.0.1:3000' },
    fs: { allow: [path.resolve(__dirname, '..')] },
  },
  resolve: { alias: { '@': path.resolve(__dirname, './src') } },
});
