local feline = require('feline')
local ctp_feline = require('catppuccin.groups.integrations.feline')
local clrs = require('catppuccin.palettes').get_palette()

ctp_feline.setup({
	assets = {
		right_separator = '',
		left_separator = '',
		mode_icon = "",
		dir = "",
		file = "",
		lsp = {
			server = "",
			error = "",
			warning = "",
			info = "",
			hint = "",
		},
		git = {
			branch = "",
			added = "",
			changed = "",
			removed = "",
		},
	},
	sett = {
		diffs = clrs.mauve,
		extras = clrs.overlay1,
		curr_file = clrs.maroon,
		curr_dir = clrs.flamingo,
		show_modified = true,
	},
	mode_colors = {
		['n']    = { 'NORMAL', clrs.lavender },
		['no']   = { 'PENDNG', clrs.lavender },
		['i']    = { 'INSERT', clrs.green },
		['ic']   = { 'INSERT', clrs.green },
		['t']    = { 'TERMNL', clrs.green },
		['v']    = { 'VISUAL', clrs.flamingo },
		['V']    = { 'V-LINE', clrs.flamingo },
		['\x16'] = { 'V-BLCK', clrs.flamingo },
		['R']    = { 'REPLCE', clrs.maroon },
		['Rv']   = { 'V-RPLC', clrs.maroon },
		['s']    = { 'SELECT', clrs.maroon },
		['S']    = { 'S-LINE', clrs.maroon },
		['\x13'] = { 'S-BLCK', clrs.maroon },
		['c']    = { 'COMMND', clrs.peach },
		['cv']   = { 'COMMND', clrs.peach },
		['ce']   = { 'COMMND', clrs.peach },
		['r']    = { 'PROMPT', clrs.teal },
		['rm']   = { 'MORE  ', clrs.teal },
		['r?']   = { 'CONFRM', clrs.mauve },
		['!']    = { 'SHELL ', clrs.green },
	},
})

feline.setup({ components = ctp_feline.get() })
