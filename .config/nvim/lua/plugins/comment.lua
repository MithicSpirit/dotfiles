require('Comment').setup({
	padding = true,
	sticky = true,
	ignore = nil,
	toggler = {
		line = 'gcc',
		block = 'gcb',
	},
	opleader = {
		line = 'gcc',
		block = 'gcb',
	},
	extra = {
		above = 'gcO',
		below = 'gco',
		eol = 'gcA',
	},
	mappings = {
		basic = true,
		extra = true,
	},
	pre_hook = nil,
	post_hook = nil,
})
