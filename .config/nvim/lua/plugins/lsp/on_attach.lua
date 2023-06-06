return function(_, buffer)
	local opts = {buffer = buffer}

	vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
	vim.keymap.set('n', 'gI', vim.lsp.buf.implementation, opts)
	vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
	vim.keymap.set('n', 'gD', vim.lsp.buf.type_definition, opts)
	vim.keymap.set('n', 'gQ', vim.lsp.buf.format, opts)
	vim.keymap.set('n', '<leader>la', vim.lsp.buf.code_action, opts)
	vim.keymap.set('n', '<leader>lr', vim.lsp.buf.rename, opts)

	vim.keymap.set('', '[d', vim.diagnostic.goto_prev, opts)
	vim.keymap.set('', ']d', vim.diagnostic.goto_next, opts)
	vim.keymap.set('n', '<leader>ld', vim.diagnostic.open_float, opts)

	local tb = require('telescope.builtin')
	vim.keymap.set('n', 'gR', tb.lsp_references, opts)
	vim.keymap.set('n', '<leader>lsd', tb.lsp_document_symbols, opts)
	vim.keymap.set('n', '<leader>lsw', tb.lsp_dynamic_workspace_symbols, opts)
	vim.keymap.set('n', '<leader>lD', tb.diagnostics, opts)
end
