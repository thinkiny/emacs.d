#!/usr/bin/env python3

import atexit
import tempfile
import sys
import os
import re
import urllib
import urllib.request
import shutil
import atexit
import mimetypes
import http.client
import logging
from http.server import ThreadingHTTPServer, SimpleHTTPRequestHandler
from pathlib import Path
from zipfile import ZipFile
from urllib.parse import urlparse

class FileHandler(SimpleHTTPRequestHandler):
    tmp_files = {}

    def log_message(self, format, *args):
        client_address = self.client_address[0]
        custom_format = f"{client_address} - {format}"
        logging.info(custom_format % args)

    @classmethod
    def cleanup(cls):
        for _, file in cls.tmp_files.items():
            if os.path.exists(file):
                os.unlink(file)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory="/", **kwargs)

    def end_headers(self):
        self.send_header("Access-Control-Allow-Origin", "*")
        super().end_headers()

    def _guess_http_suffix(self, resp: http.client.HTTPResponse):
        disposition = resp.getheader("content-disposition")
        if disposition:
            filename_match = re.search(r'filename=([^;]+)', disposition)
            if filename_match:
                filename = filename_match.group(1).strip('"\'')
                return filename

        content_type = resp.getheader("content-type", "text/html")
        if content_type:
            mime_type_main = content_type.split(";")[0].strip()
            extension = mimetypes.guess_extension(mime_type_main, strict=False)
            return extension

    def _download_to_tmpfile(self, url: str) -> str:
        if url in self.tmp_files:
            return self.tmp_files[url]

        with urllib.request.urlopen(url) as resp:
            suffix = self._guess_http_suffix(resp)
            with tempfile.NamedTemporaryFile(
                suffix=suffix, mode="wb", delete=False
            ) as tmp_file:
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
        if parsed_path.path.startswith(("/http://", "/https://")):
            self.path = self._download_to_tmpfile(parsed_path.path[1:])
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
    with ThreadingHTTPServer(("localhost", port), FileHandler) as httpd:
        httpd.serve_forever()


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.INFO,
        format="[%(asctime)s] %(levelname)s [%(name)s.%(funcName)s:%(lineno)d] %(message)s",
    )
    init_http_proxy()
    extract_pdfjs()
    atexit.register(FileHandler.cleanup)
    start_server(int(sys.argv[1]))
