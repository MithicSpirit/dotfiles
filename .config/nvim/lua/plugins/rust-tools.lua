local rt = require('rust-tools')

rt.setup({
	tools = {
		executor = require('rust-tools.executors').termopen,
		reload_workspace_from_cargo_toml = true,
		inlay_hints = {
			auto = true,
			only_current_line = false,
			show_parameter_hints = true,

			parameter_hints_prefix = '<- ',
			other_hints_prefix = '=> ',
			highlight = 'Comment',

			max_len_align = false,
			max_len_align_padding = 1,
			right_align = false,
			right_align_padding = 3,
		},

		hover_actions = {
			max_width = nil,
			max_height = nil,
			auto_focus = false,
		},

		crate_graph = {
			backend = 'svgz',
			output = nil,
			full = true,
		},
	},

	dap = {
		adapter = {
			type = 'executable',
			command = 'lldb-vscode',
			name = 'rt_lldb',
		},
	},

	server = {
		on_attach = require('plugins.lsp.on_attach'),
		standalone = true,
	},
})
