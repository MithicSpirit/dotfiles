local ht = require('haskell-tools')


vim.g.haskell_tools = {
	tools = {
		codeLens = {autoRefresh = true},
		hoogle = {mode = 'auto'},
		hover = {
			enable = true,
			stylize_markdown = false,  -- use md ts instead
			auto_focus = false,
		},
		definition = {hoogle_signature_fallback = true},
		repl = {
			handler = 'builtin',
			auto_focus = true,
		},
		dap = {cmd = {'haskell-debug-adapter'}},
	},
	hls = {
		default_settings = {
			haskell = {
				formattingProvider = 'ormolu',
				checkProject = true,
			},
		},
		on_attach = require('plugins.lsp.on_attach'),
	}
}

local augroup = vim.api.nvim_create_augroup('mithic-haskell', {})
vim.api.nvim_create_autocmd('FileType', {
	pattern = 'haskell',
	callback = function()
		local opts = {buffer = true}
		vim.keymap.set('n', '<localleader>r', ht.repl.toggle, opts)
		vim.keymap.set('n', '<localleader>f', function()
			ht.repl.toggle(vim.api.nvim_buf_get_name(0))
		end, opts)
		vim.keymap.set('n', '<localleader>q', ht.repl.quit, opts)

		ht.lsp.start()
		ht.dap.discover_configurations(0, {autodetect = true})
	end,
	group = augroup,
})

