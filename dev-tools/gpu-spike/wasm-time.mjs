// Time a project's first full render in the web app (Backend.Wasm, on its
// workers), for the GPU spike's comparison: node wasm-time.mjs PROJECT.fs
import { start } from "../web/harness.mjs";
const app = await start();
try {
  for (let rep = 0; rep < 2; rep++) {
    const t = Date.now();
    await app.openFile(process.argv[2]);
    const ms = Date.now() - t;
    const size = await app.page.evaluate(() => {
      const c = document.querySelector(".viewer canvas");
      return [c.width, c.height, navigator.hardwareConcurrency];
    });
    console.log(`drawn in ${ms} ms, canvas ${size[0]}x${size[1]}, ${size[2]} hardware threads`);
  }
} finally {
  await app.close?.();
  process.exit(0);
}
