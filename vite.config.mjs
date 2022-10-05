import { defineConfig } from 'vite';
import reactRefresh from '@vitejs/plugin-react-refresh';
import createReScriptPlugin from '@jihchi/vite-plugin-rescript';

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [
        reactRefresh({
            include: "**/*.js",
            exclude: "/node_modules/"
        }),
        createReScriptPlugin()
    ],
});
