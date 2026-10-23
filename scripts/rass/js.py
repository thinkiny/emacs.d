"""JS/TS preset: vtsls + eslint + tailwindcss with language-aware routing."""

import re

from rassumfrassum.frassum import LspLogic, Server
from rassumfrassum.util import dmerge


class JsTsLogic(LspLogic):
    """Route requests to the correct server based on language and method."""

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._doc_langs: dict[str, str] = {}

    def _lang(self, uri: str) -> str:
        """Get languageId for a document URI."""
        return self._doc_langs.get(uri, '')

    def _by_name(self, name: str) -> list[Server]:
        return [s for s in self.servers.values() if name in s.name]

    @staticmethod
    def _rass_settings(settings: dict, server: Server) -> dict:
        rass = settings.get('rass')
        generic = {key: value for key, value in settings.items() if key != 'rass'}
        if isinstance(rass, dict):
            for pattern, overlay in rass.items():
                if re.search(pattern, server.name) and isinstance(overlay, dict):
                    return dmerge(overlay, generic)
        return generic

    async def on_client_notification(self, method: str, params: dict) -> None:
        if method == 'workspace/didChangeConfiguration':
            settings = params.get('settings')
            if isinstance(settings, dict):
                empty_section = settings.get(':', settings.get(''))
                if isinstance(empty_section, dict):
                    for server in self.servers.values():
                        server_params = params
                        if 'vtsls' in server.name:
                            server_params = {
                                **params,
                                'settings': self._rass_settings(
                                    empty_section, server
                                ),
                            }
                        await self.notify_server(server, method, server_params)
                    return
        elif method == 'textDocument/didOpen':
            uri = params['textDocument']['uri']
            self._doc_langs[uri] = params['textDocument'].get('languageId', '')
        elif method == 'textDocument/didClose':
            self._doc_langs.pop(params.get('textDocument', {}).get('uri', ''), None)
        await super().on_client_notification(method, params)

    async def on_client_request(self, method: str, params: dict, servers: list[Server]):
        uri = params.get('textDocument', {}).get('uri', '')
        is_css_like = self._lang(uri) in ('css', 'scss', 'less', 'html', 'vue', 'svelte')

        # --- Completion ---
        if method == 'textDocument/completion':
            return self._by_name('tailwindcss') if is_css_like else self._by_name('vtsls')

        # --- Hover ---
        if method == 'textDocument/hover':
            return self._by_name('tailwindcss') if is_css_like else self._by_name('vtsls')

        # --- CodeAction: route by kind and language ---
        if method == 'textDocument/codeAction':
            if is_css_like:
                return self._by_name('tailwindcss')
            # source actions (organizeImports, fixAll, etc.) go to vtsls only
            context = params.get('context', {})
            only = context.get('only', [])
            if only and any(k.startswith('source.') for k in only):
                return self._by_name('vtsls')
            # quickfix/refactor: vtsls + eslint
            return self._by_name('vtsls') + self._by_name('eslint')

        # --- Diagnostic: eslint only ---
        if method == 'textDocument/diagnostic':
            return self._by_name('eslint')

        # --- CodeLens: vtsls for JS/TS, tailwind for CSS ---
        if method == 'textDocument/codeLens':
            if is_css_like:
                return self._by_name('tailwindcss')
            return self._by_name('vtsls')

        # --- Color: tailwind for CSS, skip for JS/TS ---
        if method in ('textDocument/documentColor', 'textDocument/colorPresentation'):
            if is_css_like:
                return self._by_name('tailwindcss')
            return []

        # --- DocumentLink: tailwind for CSS ---
        if method == 'textDocument/documentLink':
            if is_css_like:
                return self._by_name('tailwindcss')
            return []

        # --- ExecuteCommand: route by command prefix ---
        if method == 'workspace/executeCommand':
            cmd = params.get('command', '')
            vtsls_prefixes = ('typescript.', 'javascript.', '_typescript.', '_vtsls.')
            if cmd.startswith(vtsls_prefixes):
                return self._by_name('vtsls')
            if cmd.startswith('eslint.'):
                return self._by_name('eslint')
            result = await super().on_client_request(method, params, servers)
            return result if result else self._by_name('vtsls')

        # --- Everything else: vtsls (primary) ---
        return await super().on_client_request(method, params, servers)


def servers():
    return [
        ['vtsls', '--stdio'],
        ['vscode-eslint-language-server', '--stdio'],
        ['tailwindcss-language-server', '--stdio'],
    ]


def logic_class():
    return JsTsLogic
