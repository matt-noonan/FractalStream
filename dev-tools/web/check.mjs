// Check the web app (web/dist, from dev-tools/web/build.sh) in a headless
// browser: open every project in the library, one after another, and see
// that each of its viewers is drawn, with no errors on the page.
//
//   node dev-tools/web/check.mjs [--screenshots DIR]
//
// (The tools have checks of their own: check-tools.mjs.) See harness.mjs
// for what it needs. The exit status is 1 if any project failed.
import fs from "node:fs";
import path from "node:path";
import { start } from "./harness.mjs";

const args = process.argv.slice(2);
const shots = args[0] === "--screenshots" ? args[1] : null;
if (shots) fs.mkdirSync(shots, { recursive: true });

let failed = 0;
let app = null;
try {
  app = await start();
  const { page } = app;
  const n = await page.locator("#library li").count();
  for (let i = 0; i < n; i++) {
    app.clearErrors();
    await app.forgetMessages();
    const item = page.locator("#library li").nth(i);
    const title = (await item.textContent()).trim();
    const t = Date.now();
    await item.click();
    let result;
    try {
      const project = await app.opened();
      result = `${project.viewers.length} viewer(s) drawn in ${Date.now() - t} ms`;
    } catch (e) {
      result = `FAILED: ${e.message.split("\n")[0]}`;
    }
    if (app.errors.length) result += `; errors: ${app.errors.join(" | ")}`;
    if (result.startsWith("FAILED") || app.errors.length) failed++;
    if (shots) await page.screenshot({ path: path.join(shots, `${String(i).padStart(2, "0")}.png`) });
    console.log(`${title}: ${result}`);
  }
  console.log(`${n} projects, ${failed} failed`);
} catch (e) {
  console.log(`FAILED to load the app: ${e.message.split("\n")[0]}`);
  failed++;
} finally {
  await app?.close();
}
process.exit(failed ? 1 : 0);
