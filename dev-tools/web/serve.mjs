// Serve web/dist (dev-tools/web/build.sh's output) on http://localhost:8000
// (or the port given), for trying the web app locally:
//
//   node dev-tools/web/serve.mjs [port]
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const root = path.resolve(new URL("../../web/dist", import.meta.url).pathname);
const port = Number(process.argv[2] ?? 8000);
const types = {
  ".html": "text/html; charset=utf-8", ".css": "text/css; charset=utf-8",
  ".js": "text/javascript; charset=utf-8", ".mjs": "text/javascript; charset=utf-8",
  ".wasm": "application/wasm", ".map": "application/json", ".png": "image/png",
};

http.createServer((req, res) => {
  const url = new URL(req.url ?? "/", "http://localhost");
  const file = path.join(root, decodeURIComponent(url.pathname === "/" ? "/index.html" : url.pathname));
  if (!file.startsWith(root + path.sep) || !fs.existsSync(file) || fs.statSync(file).isDirectory()) {
    res.writeHead(404).end("not found");
    return;
  }
  res.writeHead(200, { "content-type": types[path.extname(file)] ?? "application/octet-stream",
                       "cache-control": "no-store" });
  fs.createReadStream(file).pipe(res);
}).listen(port, "127.0.0.1", () => console.log(`Serving ${root} at http://localhost:${port}`));
