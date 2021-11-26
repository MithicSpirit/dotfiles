require('vis')

local plug = require('plugins/vis-plug')
local plugins = {
	{ url = 'repo.or.cz/vis-pairs.git', alias='pairs' },
	{ url = 'ingolemo/vis-smart-backspace' },
	{ url = 'erf/vis-sneak' },
	{ url = 'seifferth/vis-super-shellout', file='super-shellout' },
	{ url = 'repo.or.cz/vis-surround.git' },
	{ url = 'repo.or.cz/vis-toggler.git' },
	{ url = 'przmv/base16-vis', theme=true },
}
plug.init(plugins, true)
plug.plugins.pairs.autopairs = false

vis.events.subscribe(vis.events.INIT, function()
	vis:command('set autoindent')
	vis:command('set ignorecase')
	vis:command('set tabwidth 4')
	vis:command('set escdelay 1')
	
	vis:command('map normal <C-w>d :q<Enter>')
end)
vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	vis:command('set number')
	vis:command('set show-tabs')

	if io.popen("tput colors", "r"):read() == "256" then
		vis:command('set theme base16-vis/themes/base16-nord')
	else 
		vis:command('set theme dark-16')
	end
end)

