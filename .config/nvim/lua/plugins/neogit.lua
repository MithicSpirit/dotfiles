local neogit = require('neogit')

neogit.setup({
	disable_insert_on_commit = true,
	filewatcher = {
		interval = 1000,
		enabled = true,
	},
	remember_settings = false,
	auto_refresh = true,
	kind = 'replace',
	console_timeout = 3000,
	auto_show_console = true,
	status = {
		recent_commit_count = 10,
	},
	commit_editor = {
		kind = 'auto',
	},
	commit_select_view = {
		kind = 'auto',
	},
	commit_view = {
		kind = 'auto',
	},
	log_view = {
		kind = 'auto',
	},
	rebase_editor = {
		kind = 'auto',
	},
	reflog_view = {
		kind = 'auto',
	},
	merge_editor = {
		kind = 'auto',
	},
	preview_buffer = {
		kind = 'auto',
	},
	popup = {
		kind = 'split',
	},
	--[[ signs = {
		-- { CLOSED, OPENED }
		hunk = { '', '' },
		item = { '>', 'v' },
		section = { '>', 'v' },
	}, ]]
})

vim.keymap.set('n', '<leader>G', neogit.open, {})
