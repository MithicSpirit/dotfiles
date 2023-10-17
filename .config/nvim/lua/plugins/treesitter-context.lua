require('treesitter-context').setup({
	enable = true,
	max_lines = 4,
	min_window_height = 24,
	line_numbers = true,
	multiline_threshold = 16,
	trim_scope = 'outer',
	mode = 'topline',
	separator = '-',
})

--vim.api.nvim_set_hl(0, "TreesitterContext", {link = "Comment"})
--vim.api.nvim_set_hl(0, "TreesitterContextBottom", {underdouble = true})
