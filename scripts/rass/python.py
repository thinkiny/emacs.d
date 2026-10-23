"""Python preset: pyrefly + ruff with capability-aware routing."""

from rassumfrassum.frassum import LspLogic, Server


class PythonLogic(LspLogic):
    """Route requests to pyrefly or ruff based on method."""

    def _by_name(self, name: str) -> list[Server]:
        return [s for s in self.servers.values() if name in s.name]

    async def on_client_request(self, method: str, params: dict, servers: list[Server]):
        # --- CodeAction: source.* to ruff, quickfix to both ---
        if method == 'textDocument/codeAction':
            context = params.get('context', {})
            only = context.get('only', [])
            if only and any(k.startswith('source.') for k in only):
                return self._by_name('ruff')
            return self._by_name('ruff')

        # --- ExecuteCommand: ruff commands only ---
        if method == 'workspace/executeCommand':
            cmd = params.get('command', '')
            if cmd.startswith('ruff.'):
                return self._by_name('ruff')
            return await super().on_client_request(method, params, servers)

        # --- Everything else: pyrefly (primary) ---
        return await super().on_client_request(method, params, servers)


def servers():
    return [
        ['pyrefly', 'lsp'],
        ['ruff', 'server'],
    ]


def logic_class():
    return PythonLogic
