vim.opt.backup = false
vim.opt.swapfile = false
vim.opt.lazyredraw = true

vim.opt.showmode = false
vim.opt.termguicolors = false
vim.opt.mouse = 'nvi'
vim.opt.clipboard = 'unnamedplus'
vim.opt.conceallevel = 2
vim.opt.guifont = 'Iosevka Mithic:h12:#e-antialias:#h-full'
vim.opt.linespace = 0
vim.opt.timeout = false
vim.opt.cursorline = true
vim.opt.path:append('**')

vim.opt.spelllang = 'en_us'
vim.opt.spellfile = vim.fn.stdpath('data') .. '/spellfile.utf-8.add'

vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = true
vim.opt.incsearch = true

vim.opt.undodir = (os.getenv('XDG_CACHE_HOME')
	or os.getenv('HOME') .. '/.cache') .. '/'
	.. (os.getenv('NVIM_APPNAME') or 'nvim') .. '/undodir'
vim.opt.undofile = true

vim.opt.scrolloff = 6
vim.opt.sidescrolloff = 6
vim.opt.number = true
vim.opt.relativenumber = false
vim.opt.numberwidth = 4

vim.opt.splitbelow = true
vim.opt.splitright = true

vim.opt.textwidth = 80
vim.opt.colorcolumn = '+1'
vim.opt.wrap = false

vim.opt.tabstop = 8
vim.opt.shiftwidth = 4
vim.opt.softtabstop = -1  -- stay in sync with 'sw'
vim.opt.expandtab = true


vim.g.tex_flavor = 'latex'
vim.g.zig_fmt_autosave = 0

vim.g.markdown_recommended_style = 0
vim.g.python_recommended_style = 0
vim.g.rust_recommended_style = 0

vim.opt.listchars = {
	tab = '> ',
	nbsp = '~',
	trail = '⏹',
	lead = '|',
	extends = '$',
	precedes = '$',
}
vim.opt.list = true
local function update_listchars_leadmultispace()
	vim.opt_local.listchars:append({
		leadmultispace = '|' .. string.rep(' ', vim.bo.shiftwidth - 1)
	})
	vim.opt_global.listchars:append({
		leadmultispace = '|' .. string.rep(' ', vim.go.shiftwidth - 1)
	})
end
update_listchars_leadmultispace()


local augroup = vim.api.nvim_create_augroup('mithic-set', {})
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'c', 'cpp', 'java', 'javascript', 'typescript', 'json', 'zig'},
	callback = function()
		vim.opt_local.shiftwidth = 8
		vim.opt_local.expandtab = false
		vim.opt_local.cindent = true
	end,
	group = augroup,
})
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'sh', 'zsh', 'asm', 'lua', 'conf', 'rust', 'zig', 'tsv'},
	callback = function()
		vim.opt_local.shiftwidth = 8
		vim.opt_local.expandtab = false
	end,
	group = augroup,
})
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'haskell', 'lisp', 'tex', 'gitcommit', 'toml', 'markdown'},
	callback = function()
		vim.opt_local.shiftwidth = 2
		vim.opt_local.expandtab = true
	end,
	group = augroup,
})
--[[ vim.api.nvim_create_autocmd('FileType', {
	pattern = {'tex','markdown','text'},
	callback = function()
		vim.opt_local.textwidth = 0
		vim.opt_local.colorcolumn = 81
		vim.opt_local.wrap = true
		vim.opt_local.linebreak = true
		vim.opt_local.breakindent = true
	end,
	group = augroup,
}) ]]
vim.api.nvim_create_autocmd('FileType', {
	pattern = 'python',
	callback = function()
		vim.opt_local.textwidth = 79
	end,
	group = augroup,
})
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'lean', 'lean3'},
	callback = function()
		vim.opt_local.shiftwidth = 2
		vim.opt_local.textwidth = 100
	end,
	group = augroup,
})
vim.api.nvim_create_autocmd('FileType', {
	pattern = 'help',
	callback = function()
		vim.opt_local.scrolloff = 1
		vim.opt_local.colorcolumn = nil
	end,
	group = augroup,
})


vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = '*PKGBUILD',
	callback = function()
		vim.opt_local.filetype = 'PKGBUILD'
	end,
	group = augroup,
})
vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = '*SRCINFO',
	callback = function()
		vim.opt_local.filetype = 'SRCINFO'
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

vim.api.nvim_create_autocmd('TextYankPost', {
	callback = function() vim.highlight.on_yank() end,
	group = augroup,
})

vim.api.nvim_create_autocmd({'OptionSet', 'BufWinEnter'}, {
	callback = update_listchars_leadmultispace,
	group = augroup,
})
