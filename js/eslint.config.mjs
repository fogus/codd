import js from "@eslint/js";
import { defineConfig } from "eslint/config";

export default defineConfig([
  { files: ["lib/**/*.{js,mjs,cjs}"], plugins: { js }, extends: ["js/recommended"] },
  { files: ["lib/**/*.js"], languageOptions: { sourceType: "commonjs" } },


]);
