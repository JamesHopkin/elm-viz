"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.runVizServer = exports.Server = void 0;
const fs = require("fs");
const http = require("http");
const url_1 = require("url");
const MIME_TYPES = new Map([
    ['js', 'application/javascript'],
    ['json', 'application/json'],
    ['css', 'text/css'],
    ['text', 'text/plain'],
    ['txt', 'text/plain'],
    ['html', 'text/html'],
    ['png', 'image/png'],
    ['jpg', 'image/jpeg'],
    ['gif', 'image/gif'],
    ['wasm', 'application/wasm'],
]);
function makeHeadersObject(kvs) {
    const headers = {};
    for (const [k, v] of kvs) {
        headers[k] = v;
    }
    return headers;
}
function ensureRegExp(route) {
    if (route instanceof RegExp) {
        return route;
    }
    // allow any character it wildcards at the moment
    // should maybe require ** to match /
    route = route
        .replace(/([().+?])/g, '\\$1')
        .replace(/\*/g, '([^/]*)');
    return new RegExp(`^${route}$`);
}
function writeError(response, code, message) {
    response.writeHead(code, { 'Content-Type': 'text/plain' });
    response.end(message);
}
class Server {
    constructor() {
        this.server = null;
        this.handlers = [];
    }
    open(optPort) {
        if (this.server) {
            throw new Error('already open');
        }
        const port = optPort || 80;
        this.server = new http.Server;
        this.server.on('request', (request, response) => this.onRequest(request, response));
        console.log(`Web server listening on port ${port}`);
        return new Promise((done, _fail) => this.server.listen(port, done));
    }
    close() {
        return new Promise((done, _fail) => this.server.close(done))
            .then(() => this.server = null);
    }
    addFileMapping(route, filePattern, opts) {
        let filetype = opts && opts.filetype;
        if (!filetype) {
            if (!(route instanceof RegExp)) {
                const lastDotIndex = route.lastIndexOf('.');
                if (lastDotIndex !== -1) {
                    filetype = route.substr(lastDotIndex + 1);
                }
            }
        }
        if (!filetype) {
            const lastDotIndex = filePattern.lastIndexOf('.');
            if (lastDotIndex === -1) {
                throw new Error('File type or file extension required');
            }
            filetype = filePattern.substr(lastDotIndex + 1);
        }
        if (!MIME_TYPES.has(filetype) && filetype.split('/').length !== 2) {
            throw new Error(`Unknown file type '${filetype}'`);
        }
        let encoding = null;
        let mimeType = MIME_TYPES.get(filetype);
        if (mimeType) {
            // assuming known file types other than images are text
            // HACK HACK HACK - any headers currently turn off utf8 decoding
            if (!mimeType.startsWith('image/') && !(opts && 'headers' in opts)) {
                encoding = 'utf8';
            }
        }
        else {
            mimeType = filetype;
        }
        const routeRegExp = ensureRegExp(route);
        this.handlers.push({
            route: routeRegExp,
            verb: 'GET',
            handler: (response, req, _match) => this.handleFileRequest(response, filePattern, encoding, mimeType, routeRegExp, req.url.pathname, opts && opts.headers)
        });
    }
    handleFileRequest(response, filePattern, encoding, mimeType, route, path, inHeaders) {
        const filePath = path.replace(route, filePattern);
        if (filePath.search(/\.\./) !== -1) {
            throw new Error('relative paths not allowed!');
        }
        // serve the file
        fs.readFile(filePath, encoding, (err, data) => {
            if (err) {
                writeError(response, 404, `File not found: ${filePath}`);
            }
            else {
                const headers = inHeaders ? makeHeadersObject(inHeaders) : {};
                headers['Content-Type'] = mimeType;
                response.writeHead(200, headers);
                response.end(data);
            }
        });
    }
    async onRequest(req, response) {
        let url = null;
        try {
            url = new url_1.URL(`http://${req.headers.host}${req.url}`);
        }
        catch (err) {
            console.log('Request error: ' + err);
        }
        if (!url || !url.pathname) {
            writeError(response, 500, 'URL parse failure');
            return;
        }
        //todo POST/PUT
        for (const handler of this.handlers) {
            if (req.method !== handler.verb) {
                continue;
            }
            const match = url.pathname.match(handler.route);
            if (match) {
                try {
                    await handler.handler(response, { url: url, cookies: req.headers.cookie }, match);
                }
                catch (err) {
                    writeError(response, 500, 'Internal server error: ' + err);
                }
                return;
            }
        }
        writeError(response, 404, 'Resource not found: ' + url);
    }
}
exports.Server = Server;
function runVizServer(port) {
    const s = new Server;
    s.addFileMapping('/*.js', 'js/$1.js');
    s.addFileMapping('/*.css', 'css/$1.css');
    s.addFileMapping('/', 'index.html', { filetype: "text/html" });
    s.addFileMapping('/*.html', '$1.html', { filetype: "text/html" });
    s.addFileMapping('/*.wasm', '$1.wasm.gz', { filetype: "application/wasm", headers: [
            ['Cache-Control', 'max-age=' + 60 * 24 * 7],
            ['Content-Encoding', 'gzip']
        ] });
    s.addFileMapping('/*', '$1', { filetype: "text/plain" });
    s.open(port);
    return s;
}
exports.runVizServer = runVizServer;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic2VydmVyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vc3JjL25ldy9zZXJ2ZXIudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7O0FBQUEseUJBQXlCO0FBQ3pCLDZCQUE2QjtBQUM3Qiw2QkFBMEI7QUFFMUIsTUFBTSxVQUFVLEdBQUcsSUFBSSxHQUFHLENBQUM7SUFDMUIsQ0FBQyxJQUFJLEVBQUUsd0JBQXdCLENBQUM7SUFDaEMsQ0FBQyxNQUFNLEVBQUUsa0JBQWtCLENBQUM7SUFDNUIsQ0FBQyxLQUFLLEVBQUUsVUFBVSxDQUFDO0lBQ25CLENBQUMsTUFBTSxFQUFFLFlBQVksQ0FBQztJQUN0QixDQUFDLEtBQUssRUFBRSxZQUFZLENBQUM7SUFDckIsQ0FBQyxNQUFNLEVBQUUsV0FBVyxDQUFDO0lBQ3JCLENBQUMsS0FBSyxFQUFFLFdBQVcsQ0FBQztJQUNwQixDQUFDLEtBQUssRUFBRSxZQUFZLENBQUM7SUFDckIsQ0FBQyxLQUFLLEVBQUUsV0FBVyxDQUFDO0lBQ3BCLENBQUMsTUFBTSxFQUFFLGtCQUFrQixDQUFDO0NBQzVCLENBQUMsQ0FBQTtBQUVGLFNBQVMsaUJBQWlCLENBQUMsR0FBdUI7SUFDakQsTUFBTSxPQUFPLEdBQVEsRUFBRSxDQUFDO0lBQ3hCLEtBQUssTUFBTSxDQUFDLENBQUMsRUFBRSxDQUFDLENBQUMsSUFBSSxHQUFHLEVBQUU7UUFDekIsT0FBTyxDQUFDLENBQUMsQ0FBQyxHQUFHLENBQUMsQ0FBQztLQUNmO0lBQ0QsT0FBTyxPQUFPLENBQUM7QUFDaEIsQ0FBQztBQUVELFNBQVMsWUFBWSxDQUFDLEtBQXNCO0lBQzNDLElBQUksS0FBSyxZQUFZLE1BQU0sRUFBRTtRQUM1QixPQUFPLEtBQUssQ0FBQTtLQUNaO0lBRUQsaURBQWlEO0lBQ2pELHFDQUFxQztJQUNyQyxLQUFLLEdBQUcsS0FBSztTQUNWLE9BQU8sQ0FBQyxZQUFZLEVBQUUsTUFBTSxDQUFDO1NBQzdCLE9BQU8sQ0FBQyxLQUFLLEVBQUUs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VUFBVSxHQUFHLEVBQUUsR0FBQyxFQUFFLEdBQUMsQ0FBQyxDQUFDO1lBQ3ZDLENBQUMsa0JBQWtCLEVBQUUsTUFBTSxDQUFDO1NBQzVCLEVBQUMsQ0FBQyxDQUFBO0lBQ0gsQ0FBQyxDQUFDLGNBQWMsQ0FBQyxJQUFJLEVBQUUsSUFBSSxFQUFFLEVBQUMsUUFBUSxFQUFFLFlBQVksRUFBQyxDQUFDLENBQUE7SUFFdEQsQ0FBQyxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQTtJQUNaLE9BQU8sQ0FBQyxDQUFBO0FBQ1QsQ0FBQztBQWZELG9DQWVDIiwic291cmNlc0NvbnRlbnQiOlsiaW1wb3J0ICogYXMgZnMgZnJvbSAnZnMnO1xuaW1wb3J0ICogYXMgaHR0cCBmcm9tICdodHRwJztcbmltcG9ydCB7IFVSTCB9IGZyb20gJ3VybCc7XG5cbmNvbnN0IE1JTUVfVFlQRVMgPSBuZXcgTWFwKFtcblx0WydqcycsICdhcHBsaWNhdGlvbi9qYXZhc2NyaXB0J10sXG5cdFsnanNvbicsICdhcHBsaWNhdGlvbi9qc29uJ10sXG5cdFsnY3NzJywgJ3RleHQvY3NzJ10sXG5cdFsndGV4dCcsICd0ZXh0L3BsYWluJ10sXG5cdFsndHh0JywgJ3RleHQvcGxhaW4nXSxcblx0WydodG1sJywgJ3RleHQvaHRtbCddLFxuXHRbJ3BuZycsICdpbWFnZS9wbmcnXSxcblx0WydqcGcnLCAnaW1hZ2UvanBlZyddLFxuXHRbJ2dpZicsICdpbWFnZS9naWYnXSxcblx0Wyd3YXNtJywgJ2FwcGxpY2F0aW9uL3dhc20nXSxcbl0pXG5cbmZ1bmN0aW9uIG1ha2VIZWFkZXJzT2JqZWN0KGt2czogW3N0cmluZywgc3RyaW5nXVtdKSB7XG5cdGNvbnN0IGhlYWRlcnM6IGFueSA9IHt9O1xuXHRmb3IgKGNvbnN0IFtrLCB2XSBvZiBrdnMpIHtcblx0XHRoZWFkZXJzW2tdID0gdjtcblx0fVxuXHRyZXR1cm4gaGVhZGVycztcdFxufVxuXG5mdW5jdGlvbiBlbnN1cmVSZWdFeHAocm91dGU6IFJlZ0V4cCB8IHN0cmluZykge1xuXHRpZiAocm91dGUgaW5zdGFuY2VvZiBSZWdFeHApIHtcblx0XHRyZXR1cm4gcm91dGVcblx0fVxuXG5cdC8vIGFsbG93IGFueSBjaGFyYWN0ZXIgaXQgd2lsZGNhcmRzIGF0IHRoZSBtb21lbnRcblx0Ly8gc2hvdWxkIG1heWJlIHJlcXVpcmUgKiogdG8gbWF0Y2ggL1xuXHRyb3V0ZSA9IHJvdXRlXG5cdFx0XHQucmVwbGFjZSgvKFsoKS4rP10pL2csICdcXFxcJDEnKVxuXHRcdFx0LnJlcGxhY2UoL1xcKi9nLCAnKFteL10qKScpXG5cblx0cmV0dXJuIG5ldyBSZWdFeHAoYF4ke3JvdXRlfSRgKVxufVxuXG5mdW5jdGlvbiB3cml0ZUVycm9yKHJlc3BvbnNlOiBodHRwLlNlcnZlclJlc3BvbnNlLCBjb2RlOiBudW1iZXIsIG1lc3NhZ2U6IHN0cmluZykge1xuXHRyZXNwb25zZS53cml0ZUhlYWQoY29kZSwgeydDb250ZW50LVR5cGUnOiAndGV4dC9wbGFpbid9KVxuXHRyZXNwb25zZS5lbmQobWVzc2FnZSlcbn1cblxuZXhwb3J0IGludGVyZmFjZSBXZWJSZXF1ZXN0IHtcblx0dXJsOiBVUkwsXG5cdGNvb2tpZXM6IHN0cmluZ1xuXHRyZXFEYXRhPzogc3RyaW5nXG59XG5cbmV4cG9ydCBpbnRlcmZhY2UgUmVxdWVzdE9wdHMge1xuXHRmaWxldHlwZT86IHN0cmluZ1xuXHRoZWFkZXJzPzogW3N0cmluZywgc3RyaW5nXVtdXG59XG5cbnR5cGUgSGFuZGxlckZ1bmMgPSAocmVzOiBodHRwLlNlcnZlclJlc3BvbnNlLCByZXE6IFdlYlJlcXVlc3QsIG1hdGNoOiBzdHJpbmdbXSkgPT4gYW55XG5cbmludGVyZmFjZSBIYW5kbGVySW50ZXJuYWxcbntcblx0cm91dGU6IFJlZ0V4cFxuXHR2ZXJiOiBzdHJpbmdcblx0aGFuZGxlcjogSGFuZGxlckZ1bmNcbn1cblxuZXhwb3J0IGNsYXNzIFNlcnZlciB7XG5cdG9wZW4ob3B0UG9ydD86IG51bWJlcikge1xuXHRcdGlmICh0aGlzLnNlcnZlcikge1xuXHRcdFx0dGhyb3cgbmV3IEVycm9yKCdhbHJlYWR5IG9wZW4nKVxuXHRcdH1cblxuXHRcdGNvbnN0IHBvcnQgPSBvcHRQb3J0IHx8IDgwXG5cblx0XHR0aGlzLnNlcnZlciA9IG5ldyBodHRwLlNlcnZlclxuXHRcdHRoaXMuc2VydmVyLm9uKCdyZXF1ZXN0JywgKHJlcXVlc3QsIHJlc3BvbnNlKSA9PiB0aGlzLm9uUmVxdWVzdChyZXF1ZXN0LCByZXNwb25zZSkpXG5cblx0XHRjb25zb2xlLmxvZyhgV2ViIHNlcnZlciBsaXN0ZW5pbmcgb24gcG9ydCAke3BvcnR9YClcblxuXHRcdHJldHVybiBuZXcgUHJvbWlzZTx2b2lkPigoZG9uZSwgX2ZhaWwpID0+IHRoaXMuc2VydmVyIS5saXN0ZW4ocG9ydCwgZG9uZSkpXG5cdH1cblxuXHRjbG9zZSgpIHtcblx0XHRyZXR1cm4gbmV3IFByb21pc2UoKGRvbmU6ICgpID0+IHZvaWQsIF9mYWlsKSA9PiB0aGlzLnNlcnZlciEuY2xvc2UoZG9uZSkpXG5cdFx0LnRoZW4oKCkgPT4gdGhpcy5zZXJ2ZXIgPSBudWxsKVxuXHR9XG5cblx0YWRkRmlsZU1hcHBpbmcocm91dGU6IHN0cmluZyB8IFJlZ0V4cCwgZmlsZVBhdHRlcm46IHN0cmluZywgb3B0cz86IFJlcXVlc3RPcHRzKSB7XG5cdFx0bGV0IGZpbGV0eXBlID0gb3B0cyAmJiBvcHRzLmZpbGV0eXBlO1xuXHRcdGlmICghZmlsZXR5cGUpIHtcblx0XHRcdGlmICghKHJvdXRlIGluc3RhbmNlb2YgUmVnRXhwKSkge1xuXHRcdFx0XHRjb25zdCBsYXN0RG90SW5kZXggPSByb3V0ZS5sYXN0SW5kZXhPZignLicpXG5cdFx0XHRcdGlmIChsYXN0RG90SW5kZXggIT09IC0xKSB7XG5cdFx0XHRcdFx0ZmlsZXR5cGUgPSByb3V0ZS5zdWJzdHIobGFzdERvdEluZGV4ICsgMSlcblx0XHRcdFx0fVxuXHRcdFx0fVxuXHRcdH1cblxuXHRcdGlmICghZmlsZXR5cGUpIHtcblx0XHRcdGNvbnN0IGxhc3REb3RJbmRleCA9IGZpbGVQYXR0ZXJuLmxhc3RJbmRleE9mKCcuJylcblx0XHRcdGlmIChsYXN0RG90SW5kZXggPT09IC0xKSB7XG5cdFx0XHRcdHRocm93IG5ldyBFcnJvcignRmlsZSB0eXBlIG9yIGZpbGUgZXh0ZW5zaW9uIHJlcXVpcmVkJylcblx0XHRcdH1cblx0XHRcdGZpbGV0eXBlID0gZmlsZVBhdHRlcm4uc3Vic3RyKGxhc3REb3RJbmRleCArIDEpXG5cdFx0fVxuXHRcdGlmICghTUlNRV9UWVBFUy5oYXMoZmlsZXR5cGUpICYmIGZpbGV0eXBlLnNwbGl0KCcvJykubGVuZ3RoICE9PSAyKSB7XG5cdFx0XHR0aHJvdyBuZXcgRXJyb3IoYFVua25vd24gZmlsZSB0eXBlICcke2ZpbGV0eXBlfSdgKVxuXHRcdH1cblxuXHRcdGxldCBlbmNvZGluZzogc3RyaW5nIHwgbnVsbCA9IG51bGxcblx0XHRsZXQgbWltZVR5cGUgPSBNSU1FX1RZUEVTLmdldChmaWxldHlwZSlcblx0XHRpZiAobWltZVR5cGUpIHtcblx0XHRcdC8vIGFzc3VtaW5nIGtub3duIGZpbGUgdHlwZXMgb3RoZXIgdGhhbiBpbWFnZXMgYXJlIHRleHRcblx0XHRcdC8vIEhBQ0sgSEFDSyBIQUNLIC0gYW55IGhlYWRlcnMgY3VycmVudGx5IHR1cm4gb2ZmIHV0ZjggZGVjb2Rpbmdcblx0XHRcdGlmICghbWltZVR5cGUuc3RhcnRzV2l0aCgnaW1hZ2UvJykgJiYgIShvcHRzICYmICdoZWFkZXJzJyBpbiBvcHRzKSkge1xuXHRcdFx0XHRlbmNvZGluZyA9ICd1dGY4J1xuXHRcdFx0fVxuXHRcdH1cblx0XHRlbHNlIHtcblx0XHRcdG1pbWVUeXBlID0gZmlsZXR5cGVcblx0XHR9XG5cblx0XHRjb25zdCByb3V0ZVJlZ0V4cCA9IGVuc3VyZVJlZ0V4cChyb3V0ZSlcblx0XHR0aGlzLmhhbmRsZXJzLnB1c2goe1xuXHRcdFx0cm91dGU6IHJvdXRlUmVnRXhwLFxuXHRcdFx0dmVyYjogJ0dFVCcsXG5cdFx0XHRoYW5kbGVyOiAocmVzcG9uc2U6IGh0dHAuU2VydmVyUmVzcG9uc2UsIHJlcTogV2ViUmVxdWVzdCwgX21hdGNoOiBzdHJpbmdbXSkgPT5cblx0XHRcdFx0dGhpcy5oYW5kbGVGaWxlUmVxdWVzdChyZXNwb25zZSwgZmlsZVBhdHRlcm4sIGVuY29kaW5nLCBtaW1lVHlwZSEsIHJvdXRlUmVnRXhwLCByZXEudXJsLnBhdGhuYW1lISwgb3B0cyAmJiBvcHRzLmhlYWRlcnMpXG5cdFx0fSlcblx0fVxuXG5cdHByaXZhdGUgaGFuZGxlRmlsZVJlcXVlc3QocmVzcG9uc2U6IGh0dHAuU2VydmVyUmVzcG9uc2UsIGZpbGVQYXR0ZXJuOiBzdHJpbmcsIGVuY29kaW5nOiBzdHJpbmcgfCBudWxsLFxuXHRcdFx0XHRcdFx0XHRcdG1pbWVUeXBlOiBzdHJpbmcsIHJvdXRlOiBSZWdFeHAsIHBhdGg6IHN0cmluZywgaW5IZWFkZXJzPzogW3N0cmluZywgc3RyaW5nXVtdKSB7XG5cdFx0Y29uc3QgZmlsZVBhdGggPSBwYXRoLnJlcGxhY2Uocm91dGUsIGZpbGVQYXR0ZXJuKVxuXHRcdGlmIChmaWxlUGF0aC5zZWFyY2goL1xcLlxcLi8pICE9PSAtMSkge1xuXHRcdFx0dGhyb3cgbmV3IEVycm9yKCdyZWxhdGl2ZSBwYXRocyBub3QgYWxsb3dlZCEnKVxuXHRcdH1cblxuXHRcdC8vIHNlcnZlIHRoZSBmaWxlXG5cdFx0ZnMucmVhZEZpbGUoZmlsZVBhdGgsIGVuY29kaW5nLCAoZXJyLCBkYXRhKSA9PiB7XG5cdFx0XHRpZiAoZXJyKSB7XG5cdFx0XHRcdHdyaXRlRXJyb3IocmVzcG9uc2UsIDQwNCwgYEZpbGUgbm90IGZvdW5kOiAke2ZpbGVQYXRofWApXG5cdFx0XHR9XG5cdFx0XHRlbHNlIHtcblx0XHRcdFx0Y29uc3QgaGVhZGVycyA9IGluSGVhZGVycyA/IG1ha2VIZWFkZXJzT2JqZWN0KGluSGVhZGVycykgOiB7fVxuXHRcdFx0XHRoZWFkZXJzWydDb250ZW50LVR5cGUnXSA9IG1pbWVUeXBlXG5cdFx0XHRcdHJlc3BvbnNlLndyaXRlSGVhZCgyMDAsIGhlYWRlcnMpXG5cdFx0XHRcdHJlc3BvbnNlLmVuZChkYXRhKVxuXHRcdFx0fVxuXHRcdH0pXG5cdH1cblxuXHRwcml2YXRlIGFzeW5jIG9uUmVxdWVzdChyZXE6IGh0dHAuSW5jb21pbmdNZXNzYWdlLCByZXNwb25zZTogaHR0cC5TZXJ2ZXJSZXNwb25zZSkge1xuXHRcdGxldCB1cmwgPSBudWxsXG5cdFx0dHJ5IHtcblx0XHRcdHVybCA9IG5ldyBVUkwoYGh0dHA6Ly8ke3JlcS5oZWFkZXJzLmhvc3R9JHtyZXEudXJsIX1gKVxuXHRcdH1cblx0XHRjYXRjaCAoZXJyKSB7XG5cdFx0XHRjb25zb2xlLmxvZygnUmVxdWVzdCBlcnJvcjogJyArIGVyciwgKVxuXHRcdH1cblxuXHRcdGlmICghdXJsIHx8ICF1cmwucGF0aG5hbWUpIHtcblx0XHRcdHdyaXRlRXJyb3IocmVzcG9uc2UsIDUwMCwgJ1VSTCBwYXJzZSBmYWlsdXJlJylcblx0XHRcdHJldHVyblxuXHRcdH1cblxuXHRcdC8vdG9kbyBQT1NUL1BVVFxuXG5cdFx0Zm9yIChjb25zdCBoYW5kbGVyIG9mIHRoaXMuaGFuZGxlcnMpIHtcblx0XHRcdGlmIChyZXEubWV0aG9kICE9PSBoYW5kbGVyLnZlcmIpIHtcblx0XHRcdFx0Y29udGludWVcblx0XHRcdH1cblxuXHRcdFx0Y29uc3QgbWF0Y2ggPSB1cmwucGF0aG5hbWUubWF0Y2goaGFuZGxlci5yb3V0ZSlcblx0XHRcdGlmIChtYXRjaCkge1xuXHRcdFx0XHR0cnkge1xuXHRcdFx0XHRcdGF3YWl0IGhhbmRsZXIuaGFuZGxlcihyZXNwb25zZSwge3VybDogdXJsLCBjb29raWVzOiByZXEuaGVhZGVycy5jb29raWUgYXMgc3RyaW5nfSwgbWF0Y2gpXG5cdFx0XHRcdH1cblx0XHRcdFx0Y2F0Y2ggKGVycikge1xuXHRcdFx0XHRcdHdyaXRlRXJyb3IocmVzcG9uc2UsIDUwMCwgJ0ludGVybmFsIHNlcnZlciBlcnJvcjogJyArIGVycilcblx0XHRcdFx0fVxuXHRcdFx0XHRyZXR1cm5cblx0XHRcdH1cblx0XHR9XG5cblx0XHR3cml0ZUVycm9yKHJlc3BvbnNlLCA0MDQsICdSZXNvdXJjZSBub3QgZm91bmQ6ICcgKyB1cmwpXG5cdH1cblxuXHRwcml2YXRlIHNlcnZlcjogaHR0cC5TZXJ2ZXIgfCBudWxsID0gbnVsbFxuXHRwcml2YXRlIGhhbmRsZXJzOiBIYW5kbGVySW50ZXJuYWxbXSA9IFtdXG59XG5cbmV4cG9ydCBmdW5jdGlvbiBydW5WaXpTZXJ2ZXIocG9ydDogbnVtYmVyKSB7XG5cdGNvbnN0IHMgPSBuZXcgU2VydmVyXG5cblx0cy5hZGRGaWxlTWFwcGluZygnLyouanMnLCAnanMvJDEuanMnKVxuXHRzLmFkZEZpbGVNYXBwaW5nKCcvKi5jc3MnLCAnY3NzLyQxLmNzcycpXG5cdHMuYWRkRmlsZU1hcHBpbmcoJy8nLCAnaW5kZXguaHRtbCcsIHtmaWxldHlwZTogXCJ0ZXh0L2h0bWxcIn0pXG5cdHMuYWRkRmlsZU1hcHBpbmcoJy8qLmh0bWwnLCAnJDEuaHRtbCcsIHtmaWxldHlwZTogXCJ0ZXh0L2h0bWxcIn0pXG5cdHMuYWRkRmlsZU1hcHBpbmcoJy8qLndhc20nLCAnJDEud2FzbS5neicsIHtmaWxldHlwZTogXCJhcHBsaWNhdGlvbi93YXNtXCIsIGhlYWRlcnM6IFtcblx0XHRbJ0NhY2hlLUNvbnRyb2wnLCAnbWF4LWFnZT0nICsgNjAqMjQqN10sXG5cdFx0WydDb250ZW50LUVuY29kaW5nJywgJ2d6aXAnXVxuXHRdfSlcblx0cy5hZGRGaWxlTWFwcGluZygnLyonLCAnJDEnLCB7ZmlsZXR5cGU6IFwidGV4dC9wbGFpblwifSlcblxuXHRzLm9wZW4ocG9ydClcblx0cmV0dXJuIHNcbn1cbiJdfQ==