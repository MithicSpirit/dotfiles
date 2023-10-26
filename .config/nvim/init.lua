-- Hotpot + Lazy boilerplate setup code
local lazypath = vim.fn.stdpath('data') .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.notify("Bootstrapping lazy.nvim...", vim.log.levels.INFO)
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"--branch=stable",
		"https://github.com/folke/lazy.nvim.git",
		lazypath,
	})
end

local hotpotpath = vim.fn.stdpath('data') .. "/lazy/hotpot.nvim"
if not vim.loop.fs_stat(hotpotpath) then
	vim.notify("Bootstrapping hotpot.nvim...", vim.log.levels.INFO)
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/rktjmp/hotpot.nvim.git",
		hotpotpath,
	})
end

vim.opt.runtimepath:prepend({hotpotpath, lazypath})
require('hotpot').setup({
	provide_require_fennel = true,
	macros = {env = "_COMPILER"},
})


local config_namespace = 'mithic'
require(config_namespace)


local plugins = {{
	"rktjmp/hotpot.nvim",
	lazy = false,
	priority = 2000,
	config = false,
}}

for _, lang in ipairs({'lua', 'fnl'}) do
	local plugins_path = vim.fn.stdpath('config')
		.. "/" .. lang .. "/" .. config_namespace .. "/plugins"
	for file in vim.fs.dir(plugins_path) do
		file = file:match("^(.*)%." .. lang .. "$")
		if file then
			plugins[#plugins + 1] = require(
				config_namespace .. '.plugins.' .. file
			)
		end
	end
end

require('lazy').setup({
	defaults = { lazy = false },
	spec = plugins,
	concurrency = vim.loop.available_parallelism(),
	install = {
		missing = true,
		colorscheme = {'catppuccin', 'default'},
	},
	ui = {
		size = { width = 0.8, height = 0.8 },
		wrap = true,
		pills = true,
		browser = vim.env.BROWSER,
	},
	diff = { cmd = "git" },
	checker = {
		enabled = true,
		concurrency = 1,
		notify = false,
		frequency = 3600,
	},
	change_detection = {
		enabled = true,
		notify = true,
	},
	performance = {
		cache = { enabled = true },
		reset_packpath = true,
		rtp = { reset = true },
	},
	readme = {
		enabled = true,
		files = {
			"README.md",
			"lua/**/README.md",
			"fnl/**/README.md",
			"readmes/*.md",
		},
		skip_if_doc_exists = true,
	},
})

vim.keymap.set('n', "<leader>L", require('lazy').home)
