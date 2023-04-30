require('nvim-treesitter.configs').setup {
	ensure_installed = {'lua', 'vim', 'vimdoc', 'query'},
	sync_install = false,
	auto_install = true,
	--ignore_install = {'gitcommit', 'markdown'},

	highlight = {
		enable = true,
		disable = {'latex'},
		additional_vim_regex_highlighting = true,
	},
}
