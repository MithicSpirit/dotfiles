--vim.cmd [[
--let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}, 'command': {}, 'select': {}}
--
--let s:nord0 = ["#2E3440", "NONE"]
--let s:nord1 = ["#3B4252", 0]
--let s:nord2 = ["#434C5E", "NONE"]
--let s:nord3 = ["#4C566A", 8]
--let s:nord4 = ["#D8DEE9", "NONE"]
--let s:nord5 = ["#E5E9F0", 7]
--let s:nord6 = ["#ECEFF4", 15]
--let s:nord7 = ["#8FBCBB", 14]
--let s:nord8 = ["#88C0D0", 6]
--let s:nord9 = ["#81A1C1", 4]
--let s:nord10 = ["#5E81AC", 12]
--let s:nord11 = ["#BF616A", 1]
--let s:nord12 = ["#D08770", 11]
--let s:nord13 = ["#EBCB8B", 3]
--let s:nord14 = ["#A3BE8C", 2]
--let s:nord15 = ["#B48EAD", 5]
--
--let s:p.normal.left = [ [ s:nord1, s:nord14 ], [ s:nord5, s:nord3 ] ]
--let s:p.normal.middle = [ [ s:nord5, s:nord1 ] ]
--let s:p.normal.right = [ [ s:nord5, s:nord3 ], [ s:nord5, s:nord3 ] ]
--let s:p.normal.warning = [ [ s:nord1, s:nord13 ] ]
--let s:p.normal.error = [ [ s:nord1, s:nord11 ] ]
--
--let s:p.inactive.left =  [ [ s:nord5, s:nord3 ]  ]
--let s:p.inactive.middle = [ [ s:nord5, s:nord1 ] ]
--let s:p.inactive.right = [ ]
--
--let s:p.insert.left = [ [ s:nord1, s:nord10 ], [ s:nord5, s:nord1 ] ]
--let s:p.replace.left = [ [ s:nord1, s:nord11 ], [ s:nord5, s:nord1 ] ]
--let s:p.visual.left = [ [ s:nord1, s:nord13 ], [ s:nord5, s:nord1 ] ]
--let s:p.command.left = [ [ s:nord1, s:nord6 ], [ s:nord5, s:nord1 ] ]
--let s:p.select.left = [ [ s:nord1, s:nord12 ], [ s:nord5, s:nord1 ] ]
--
--let s:p.tabline.left = [ [ s:nord5, s:nord3 ] ]
--let s:p.tabline.middle = [ [ s:nord5, s:nord3 ] ]
--let s:p.tabline.right = [ [ s:nord5, s:nord3 ] ]
--let s:p.tabline.tabsel = [ [ s:nord1, s:nord8 ] ]
--
--let g:lightline#colorscheme#nord#palette = lightline#colorscheme#flatten(s:p)
--]]

vim.g.lightline = {
	colorscheme = 'catppuccin',
	separator = { left = '', right = ''},
	mode_map = {
		n        = 'NORMAL',
		i        = 'INSERT',
		R        = 'REPLCE',
		v        = 'VISUAL',
		V        = 'V-LINE',
		['\x16'] = 'V-BLCK',
		c        = 'COMMND',
		s        = 'SELECT',
		S        = 'S-LINE',
		['\x13'] = 'S-BLCK',
		t        = 'TERMNL',
	},
}
