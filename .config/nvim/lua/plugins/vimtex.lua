vim.g.vimtex_enabled = 1
vim.g.vimtex_compiler_method = 'latexmk'
vim.g.vimtex_mappings_prefix = '<localleader>'
vim.g.vimtex_text_obj_enabled = 1

vim.g.vimtex_complete_enabled = 1
--vim.g.vimtex_complete_close_braces = 1

vim.g.vimtex_indent_enabled = 1
vim.g.vimtex_indent_bib_enabled = 1
vim.g.vimtex_indent_tikz_commands = 1

vim.g.vimtex_syntax_enabled = 1
vim.g.vimtex_syntax_conceal = {
	accents = 1,
	ligatures = 1,
	cites = 1,
	fancy = 1,
	spacing = 1,
	greek = 1,
	math_bounds = 0,
	math_delimiters = 1,
	math_fracs = 0,
	math_super_sub = 1,
	math_symbols = 1,
	sections = 0,
	styles = 1,
}

vim.g.vimtex_view_enabled = 1
vim.g.vimtex_view_automatic = 1
vim.g.vimtex_view_method = 'zathura'
