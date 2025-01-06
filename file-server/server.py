#!/usr/bin/env python3

from http.server import HTTPServer, SimpleHTTPRequestHandler
from pathlib import Path
from zipfile import ZipFile
import sys, os

class CORSRequestHandler (SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory="/", **kwargs)

    def end_headers (self):
        self.send_header('Access-Control-Allow-Origin', '*')
        SimpleHTTPRequestHandler.end_headers(self)

def extractPdfJs():
    root = os.path.dirname(__file__)
    pdfjs_path = Path(f'{root}/pdfjs')
    pdfjs_zip = Path(f'{root}/pdfjs.zip')
    if not pdfjs_path.exists():
        with ZipFile(pdfjs_zip, 'r') as z:
            z.extractall(path = root)

if __name__ == '__main__':
    extractPdfJs()
    httpd = HTTPServer(('localhost', int(sys.argv[1])), CORSRequestHandler)
    httpd.serve_forever()
