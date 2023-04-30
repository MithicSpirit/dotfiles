vim.g.netrw_liststyle = 3  -- tree view
vim.g.netrw_banner = 0
vim.g.netrw_preview = 1

vim.keymap.set('n', '<leader>.', function() vim.cmd.edit('.') end)

local augroup = vim.api.nvim_create_augroup('mithic-netrw', {})
vim.api.nvim_create_autocmd('FileType', {
	pattern = 'netrw',
	callback = function()
		local opts = {remap = true, buffer = 0}
		vim.keymap.set('n', 'h', '-', opts)
		vim.keymap.set('n', 'l', '<Cr>', opts)
		vim.keymap.set('n', 'M', 'mfj', opts)
		vim.keymap.set('n', 'i', 'R', opts)
		vim.keymap.set('n', 'o', '%', opts)
	end,
	group = augroup,
})
