require('catppuccin').setup({
	flavour = 'frappe',
	transparent_background = not vim.g.neovide,
	show_end_of_buffer = true,
	term_colors = true,
	dim_inactive = {
		enabled = false,
		shade = 'dark',
		percentage = 0.15,
	},
	no_italic = false,
	no_bold = false,
	styles = {
		comments = { 'italic' },
		conditionals = {},
		loops = {},
		functions = {},
		keywords = {},
		strings = {},
		variables = {},
		numbers = {},
		booleans = {},
		properties = {},
		types = {},
		operators = {},
	},
	color_overrides = {
		all = {
			text = '#eceaf0',
		},
		frappe = {
			base = '#333743',
		},
	},
	custom_highlights = {},
	integrations = {
		gitsigns = true,
		hop = true,
		markdown = true,
		mason = true,
		cmp = true,
		treesitter = true,
		telescope = true,
		gitgutter = true,
		vim_sneak = true,
		dap = {
			enabled = true,
			enable_ui = true,
		},
		native_lsp = {
			enabled = true,
		},
	},
})

vim.cmd.colorscheme('catppuccin')
