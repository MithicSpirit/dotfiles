local telescope = require('telescope')
local builtin = require('telescope.builtin')
local project_actions = require('telescope._extensions.project.actions')
local fb_actions = require('telescope._extensions.file_browser.actions')

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
		file_browser = {
			cwd_to_path = false,
			grouped = false,
			files = true,
			add_dirs = true,
			depth = 1,
			auto_depth = false,
			select_buffer = true,
			hidden = true,
			hide_parent_dir = false,
			collapse_dirs = false,
			quiet = false,
			dir_icon = "",
			dir_icon_hl = "Default",
			display_stat = { date = true, size = true, mode = true },
			hijack_netrw = true,
			use_fd = true,
			git_status = true,
			prompt_path = true,
			mappings = {
				["i"] = {
					["<S-Cr>"] = fb_actions.create_from_prompt,
					["<C-Cr>"] = fb_actions.change_cwd,
					["<C-h>"]  = fb_actions.toggle_hidden,
					["<C-w>"]  = fb_actions.goto_cwd,
					["<Bs>"]   = fb_actions.backspace,
					["<A-c>"] = false,
					["<A-r>"] = false,
					["<A-m>"] = false,
					["<A-y>"] = false,
					["<A-d>"] = false,
					["<C-o>"] = false,
					["<C-g>"] = false,
					["<C-e>"] = false,
					["<C-f>"] = false,
					["<C-s>"] = false,
				},
				["n"] = {
					["<leader>o"] = fb_actions.create,
					["<leader>r"] = fb_actions.rename,
					["<leader>R"] = fb_actions.move,
					["<leader>y"] = fb_actions.copy,
					["<leader>x"] = fb_actions.remove,
					["<C-w>"] = fb_actions.goto_cwd,
					["<C-h>"] = fb_actions.toggle_hidden,
					["u"] = fb_actions.goto_parent_dir,
					["~"] = fb_actions.goto_home_dir,
				},
      },
		},
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
	}
})

telescope.load_extension('zf-native')
telescope.load_extension('project')
telescope.load_extension('file_browser')
telescope.load_extension('hoogle')
telescope.load_extension('emoji')


vim.keymap.set('n', '<leader><leader>', builtin.find_files, {})
vim.keymap.set('n', '<leader>,', builtin.buffers, {})
vim.keymap.set('n', '<leader><', builtin.find_files, {})
vim.keymap.set('n', '<leader>p', builtin.registers, {})

vim.keymap.set('n', '<leader>.', telescope.extensions.file_browser.file_browser, {})

vim.keymap.set('n', '<leader>>', function()
	telescope.extensions.project.project({display_type = 'full'})
end, {})

vim.keymap.set('n', '<leader>P', telescope.extensions.emoji.emoji, {})


local augroup = vim.api.nvim_create_augroup('mithic-telescope', {})
vim.api.nvim_create_autocmd('User', {
	pattern = 'RooterChDir',
	callback = function() project_actions.add_project_cwd() end,
	group = augroup,
})
