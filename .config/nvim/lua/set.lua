vim.opt.backup = false
vim.opt.swapfile = false
vim.opt.lazyredraw = true

vim.opt.showmode = false
vim.opt.termguicolors = false
vim.opt.mouse = 'nvi'
vim.opt.clipboard = 'unnamedplus'
vim.opt.timeout = true
vim.opt.conceallevel = 2
vim.opt.guifont = 'Iosevka Mithic:h12'
vim.opt.linespace = 0

vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = true
vim.opt.incsearch = true

vim.opt.undodir = (os.getenv('XDG_CACHE_HOME')
	or os.getenv('HOME') .. '/.cache') .. '/'
	.. (os.getenv('NVIM_APPNAME') or 'nvim') .. '/undodir'
vim.opt.undofile = true

vim.opt.scrolloff = 4
vim.opt.number = true
vim.opt.relativenumber = false

vim.opt.listchars = {
	tab = '> ',
	nbsp = '_',
	trail = '.',
	lead = '|',
	leadmultispace = '|   ',
	extends = '$',
	precedes = '$',
}
vim.opt.list = true

vim.opt.splitbelow = true
vim.opt.splitright = true

vim.opt.textwidth = 80
vim.opt.cursorline = true
vim.opt.colorcolumn = '+1'
vim.opt.wrap = false

vim.opt.tabstop = 8
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true


vim.g.tex_flavor = 'latex'
vim.g.rust_recommended_style = 0


local augroup = vim.api.nvim_create_augroup('mithic', {})
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'c', 'cpp', 'java', 'javascript', 'typescript', 'json'},
	callback = function()
		vim.opt_local.softtabstop = 8
		vim.opt_local.shiftwidth = 8
		vim.opt_local.expandtab = false
		vim.opt_local.cindent = true
	end,
	group = augroup,
})
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'sh', 'zsh', 'asm', 'lua', 'conf'},
	callback = function()
		vim.opt_local.softtabstop = 8
		vim.opt_local.shiftwidth = 8
		vim.opt_local.expandtab = false
	end,
	group = augroup,
})
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'haskell', 'lisp', 'tex', 'gitcommit', 'toml'},
	callback = function()
		vim.opt_local.softtabstop = 2
		vim.opt_local.shiftwidth = 2
		vim.opt_local.expandtab = true
	end,
	group = augroup,
})
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'python'},
	callback = function()
		vim.opt_local.textwidth = 79
	end,
	group = augroup,
})

vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = '*PKGBUILD*',
	callback = function()
		vim.opt_local.filetype = 'sh'
	end,
	group = augroup,
})
vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = '*/xresources/*',
	callback = function()
		vim.opt_local.filetype = 'xdefaults'
	end,
	group = augroup,
})

vim.api.nvim_create_autocmd('TermOpen', {
	callback = function()
		vim.opt_local.number = false
		vim.cmd('startinsert')
	end,
	group = augroup,
})
