#!/usr/bin/env python3

import atexit
import tempfile
import sys, os
import urllib
import urllib.request
import shutil
import atexit
import logging
from http.server import ThreadingHTTPServer, SimpleHTTPRequestHandler
from pathlib import Path
from zipfile import ZipFile
from urllib.parse import urlparse

class PdfFileHandler(SimpleHTTPRequestHandler):
    tmp_files = {}

    @classmethod
    def cleanup(cls):
        for _, file in cls.tmp_files.items():
            if os.path.exists(file):
                os.unlink(file)

    def __init__(self,*args, **kwargs):
        super().__init__(*args, directory="/", **kwargs)

    def end_headers(self):
        self.send_header("Access-Control-Allow-Origin", "*")
        super().end_headers()

    def _download_to_tmpfile(self, url: str, suffix: str) -> str:
        if url in self.tmp_files:
            return self.tmp_files[url]

        with urllib.request.urlopen(url) as resp:
            with tempfile.NamedTemporaryFile(suffix=suffix, mode='wb', delete=False) as tmp_file:
                logging.info(f"download '{url}' to '{tmp_file.name}'")
                try:
                    shutil.copyfileobj(resp, tmp_file)
                    self.tmp_files[url] = tmp_file.name
                    return tmp_file.name
                except:
                    if os.path.exists(tmp_file.name):
                        os.unlink(tmp_file.name)
                    # 404
                    return tmp_file.name

    def do_GET(self):
        parsed_path = urlparse(self.path)
        if parsed_path.path.startswith("/https://arxiv.org/pdf/"):
            self.path = self._download_to_tmpfile(parsed_path.path[1:], ".pdf")
        super().do_GET()

def extract_pdfjs():
    logging.info("extract pdfjs")
    root = os.path.dirname(__file__)
    pdfjs_path = Path(f"{root}/pdfjs")
    pdfjs_zip = Path(f"{root}/pdfjs.zip")
    if not pdfjs_path.exists():
        with ZipFile(pdfjs_zip, "r") as z:
            z.extractall(path=root)

def init_http_proxy():
    proxy_support = urllib.request.ProxyHandler(
        {"http": "http://127.0.0.1:1087", "https": "http://127.0.0.1:1087"}
    )
    opener = urllib.request.build_opener(proxy_support)
    urllib.request.install_opener(opener)

def start_server(port: int):
    logging.info("start http server")
    with ThreadingHTTPServer(("localhost", port), PdfFileHandler) as httpd:
        httpd.serve_forever()

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format='[%(asctime)s] %(levelname)s [%(name)s.%(funcName)s:%(lineno)d] %(message)s')
    init_http_proxy()
    extract_pdfjs()
    atexit.register(PdfFileHandler.cleanup)
    start_server(int(sys.argv[1]))
