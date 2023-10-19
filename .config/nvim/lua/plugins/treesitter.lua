require('nvim-treesitter.configs').setup({
	ensure_installed = {'lua', 'vim', 'vimdoc', 'query'},
	sync_install = false,
	auto_install = true,
	ignore_install = {'verilog'},

	highlight = {
		enable = true,
		disable = {'latex'},
		additional_vim_regex_highlighting = true,
	},

	indent = {
		enable = true,
		disable = {}
	},

	incremental_selection = {
		enable = true,
		disable = {},
		keymaps = {
			init_selection = 'ga',
			node_incremental = 'ga',
			node_decremental = 'gA',
			scope_incremental = false,
		}
	},

	textobjects = {
		select = {
			enable = true,
			disable = {},
			lookahead = true,
			keymaps = {
				['aa'] = '@parameter.outer',
				['ia'] = '@parameter.inner',
				['af'] = '@function.outer',
				['if'] = '@function.inner',
				['ac'] = '@class.outer',
				['ic'] = '@class.inner',
			},
		},
		move = {
			enable = true,
			set_jumps = true,
			goto_next_start = {
				[']]'] = '@function.outer',
			},
			goto_next_end = {
				[']['] = '@function.outer',
			},
			goto_previous_start = {
				['[['] = '@function.outer',
			},
			goto_previous_end = {
				['[]'] = '@function.outer',
			},
		},
		swap = {enable = false},
	},
})
