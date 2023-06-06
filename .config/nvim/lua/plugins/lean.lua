local on_attach = require('plugins.lsp.on_attach')
require('lean').setup{
	lsp = {on_attach = on_attach},
	lsp3 = {on_attach = on_attach},
	lean3 = {mouse_events = false},
	ft = {
		default = 'lean3',
		nomodifiable = nil
	},

	abbreviations = {
		enable = true,
		extra = {},
		leader = '\\',
	},

	mappings = true,

	infoview = {
		autoopen = true,
		width = 60,
		height = 20,
		horizontal_position = 'bottom',
		separate_tab = false,
		indicators = 'auto',
	},

	progress_bars = {
		enable = true,
		priority = 10,
	},

	stderr = {
		enable = true,
		height = 5,
		on_lines = nil,
	},
}

local augroup = vim.api.nvim_create_augroup('mithic-lean', {})
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'lean', 'lean3'},
	callback = function()
		vim.keymap.set('n', '<localleader>r', function()
			local iv = require('lean.infoview').get_current_infoview()
			local async = require('plenary.async')
			if iv then
				async.void(function()
					iv.info.pin:async_update(true)
				end)
			end
		end, {buffer = true})
	end,
	group = augroup,
})
