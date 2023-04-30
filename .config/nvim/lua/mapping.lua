vim.g.mapleader = ' '
vim.g.maplocalleader = ' m'

vim.keymap.set('n', '<leader>', '<Nop>')
vim.keymap.set('n', '<localleader>', '<Nop>')

vim.keymap.set('n', 'Y', 'y$')
vim.keymap.set('n', 'x', '"_x')
vim.keymap.set('n', '~', 'g~l')
vim.keymap.set('n', '<ESC>', function()
	vim.cmd.nohlsearch()
	vim.cmd.diffupdate()
	vim.cmd.mode()
end)

vim.keymap.set({'n', 'v'}, ':', ';')
vim.keymap.set({'n', 'v'}, ';', ':')
vim.keymap.set({'n', 'v'}, 'q:', 'q;')
vim.keymap.set({'n', 'v'}, 'q;', 'q:')

vim.keymap.set('n', '<C-W>,', '<C-w><')
vim.keymap.set('n', '<C-W>.', '<C-w>>')
vim.keymap.set('n', '<leader>t', function() vim.cmd('19split +terminal') end)

vim.keymap.set('v', 'J', ":move '>+1<CR>gv")
vim.keymap.set('v', 'K', ":move '<-2<CR>gv")

vim.keymap.set('n', '<C-d>', '<C-d>zz')
vim.keymap.set('n', '<C-u>', '<C-u>zz')
vim.keymap.set('n', 'n', 'nzzzv')
vim.keymap.set('n', 'N', 'Nzzzv')

vim.keymap.set('n', '<Tab>', vim.cmd.bnext)
vim.keymap.set('n', '<S-Tab>', vim.cmd.bprevious)
vim.keymap.set('n', '<leader><Tab>', '<C-^>')
vim.keymap.set('n', '<leader><S-Tab>', vim.cmd.bdelete)

vim.keymap.set('t', '<C-\\><C-\\>', '<C-\\><C-n>')
