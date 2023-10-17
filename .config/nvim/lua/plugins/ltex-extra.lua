require('ltex_extra').setup({
	server_opts = { on_attach = require('plugins.lsp.on_attach') },
	path = vim.fn.stdpath('data') .. '/ltex'
})
