local telescope = require('telescope')
local builtin = require('telescope.builtin')
local project_actions = require('telescope._extensions.project.actions')

telescope.setup({
	extensions = {
		['zf-native'] = {
			file = {
				enable = true,
				highlight_results = true,
				match_filename = true,
			},
			generic = {
				enable = true,
				highlight_results = true,
				match_filename = false,
			},
		},
		file_browser = require('plugins.telescope.file_browser'),
		project = {
			base_dirs = {
				{'~/documents/coding', max_depth = 2},
				{'~/.config', max_depth = 1},
			},
			hidden_files = true,
			order_by = 'recent',
			search_by = 'path',
			sync_with_nvim_tree = false,
			on_project_selected = function(prompt_bufnr)
				project_actions.change_working_directory(prompt_bufnr, false)
				builtin.find_files()
			end
		},
		emoji = {
			action = function(emoji)
				vim.api.nvim_put({emoji.value}, 'c', true, true)
			end
		},
	},
	pickers = {
		find_files = {
			find_command = {
				'fd',
				'--type=f',
				'--color=never',
				'--hidden',
				'--no-require-git',
				'--exclude=.git',
			},
		},
	},
})

telescope.load_extension('zf-native')
telescope.load_extension('project')
telescope.load_extension('file_browser')
telescope.load_extension('hoogle')
telescope.load_extension('emoji')
telescope.load_extension('dap')

vim.keymap.set('n', '<leader> ', builtin.find_files, {})
vim.keymap.set('n', '<leader>,', builtin.buffers, {})
vim.keymap.set('n', '<leader><', function()
	builtin.find_files({find_command = {
		'fd', '--type=f', '--color=never', '--hidden', '--no-ignore',
	}})
end, {})
vim.keymap.set('n', '<leader>/', builtin.live_grep, {})
vim.keymap.set('n', '<leader>p', builtin.registers, {})
vim.keymap.set('n', '<leader>h', builtin.help_tags, {})

vim.keymap.set('n', '<leader>.', telescope.extensions.file_browser.file_browser, {})
vim.keymap.set('n', '<leader>>', function()
	telescope.extensions.file_browser.file_browser({path = '%:p:h'})
end, {})

-- vim.keymap.set('n', '<leader>>', function()
-- 	telescope.extensions.project.project({display_type = 'full'})
-- end, {})

vim.keymap.set('n', '<leader>P', telescope.extensions.emoji.emoji, {})


--local augroup = vim.api.nvim_create_augroup('mithic-telescope', {})
--vim.api.nvim_create_autocmd('User', {
--	pattern = 'RooterChDir',
--	callback = function()
--		local root = vim.fn.FindRootDirectory()
--		if root ~= '' then
--			project_actions.add_project_cwd()
--		end
--	end,
--	group = augroup,
--})
