vim.g.mapleader = ' '
vim.g.maplocalleader = '\\'

vim.keymap.set('n', '<leader>', '<Nop>')
vim.keymap.set('n', '<localleader>', '<Nop>')

vim.keymap.set('n', 'Y', 'y$')
vim.keymap.set('n', 'x', '"_x')
vim.keymap.set('n', '~', 'g~l')
vim.keymap.set('n', '<ESC>', function()
	vim.cmd.nohlsearch()
	--vim.cmd.diffupdate() (causes issues in command mode)
	vim.cmd.mode()
end)
vim.keymap.set('n', 'gQ', "mzgggqG'z")
vim.keymap.set('n', 'ZQ', vim.cmd.cquit)
vim.keymap.set('n', '<leader>s', vim.cmd.write)
vim.keymap.set('n', '<leader>E', function() vim.cmd('! "%:p"') end)

vim.keymap.set('n', '<C-W>,', '<C-w><')
vim.keymap.set('n', '<C-W>.', '<C-w>>')
vim.keymap.set('n', '<leader>t', function() vim.cmd('19split +terminal') end)
vim.keymap.set('t', '<C-\\><C-\\>', '<C-\\><C-n>')

vim.keymap.set('v', 'J', ":move '>+1<CR>gv")
vim.keymap.set('v', 'K', ":move '<-2<CR>gv")

vim.keymap.set('v', '>', '>gv')
vim.keymap.set('v', '<', '<gv')

vim.keymap.set('n', '<C-d>', '<C-d>zz')
vim.keymap.set('n', '<C-u>', '<C-u>zz')
vim.keymap.set('n', 'n', 'nzvzz')
vim.keymap.set('n', 'N', 'Nzvzz')

vim.keymap.set('n', '<Tab>', vim.cmd.bnext)
vim.keymap.set('n', '<S-Tab>', vim.cmd.bprevious)
vim.keymap.set('n', '<leader><Tab>', '<C-^>')
vim.keymap.set('n', '<leader><S-Tab>', function()
	if vim.opt_local.modified:get() then
		vim.api.nvim_err_writeln('E89: No write since last change for buffer')
		return
	end
	local curr_buf = vim.api.nvim_get_current_buf()
	local command = 'bdelete ' .. curr_buf

	local buffers = {}
	for _, buf in ipairs(vim.api.nvim_list_bufs()) do
		if vim.api.nvim_buf_is_loaded(buf) then
			table.insert(buffers, buf)
		end
	end

	if #buffers == 1 then
		vim.cmd(command)
		return
	end

	local nextbuf
	for i, buf in ipairs(buffers) do
		if buf == curr_buf then
			nextbuf = buffers[(i - 1 + 1) % #buffers + 1]
			break
		end
	end

	vim.api.nvim_set_current_buf(nextbuf)
	vim.cmd(command)
end)

vim.api.nvim_create_user_command('Cdfile', function()
	vim.api.nvim_set_current_dir(vim.fn.expand('%:p:h'))
end, {desc = 'cd to the parent of the current file', force=false})


local augroup = vim.api.nvim_create_augroup('mithic-map', {})
vim.api.nvim_create_autocmd('FileType', {
	-- TODO: use a snippet
	pattern = 'tex',
	callback = function()
		vim.keymap.set('i', '"', "``", {buffer = true})
	end,
	group = augroup,
})
