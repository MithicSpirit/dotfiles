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
		cmp = true,
		gitsigns = true,
		nvimtree = true,
		telescope = true,
		notify = false,
		mini = false,
	},
})

vim.cmd.colorscheme('catppuccin')
