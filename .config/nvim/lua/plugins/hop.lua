local hop = require('hop')
hop.setup()

local dir = require('hop.hint').HintDirection

vim.keymap.set('', 'gsf', function()
	hop.hint_char1({direction = dir.AFTER_CURSOR})
end)
vim.keymap.set('', 'gsF', function()
	hop.hint_char1({direction = dir.BEFORE_CURSOR})
end)

vim.keymap.set('', 'gst', function()
	hop.hint_char1({direction = dir.AFTER_CURSOR, hint_offset = -1})
end)
vim.keymap.set('', 'gsT', function()
	hop.hint_char1({direction = dir.BEFORE_CURSOR, hint_offset = 1})
end)

vim.keymap.set('', 'gsj', function()
	hop.hint_vertical({direction = dir.AFTER_CURSOR})
end)
vim.keymap.set('', 'gsk', function()
	hop.hint_vertical({direction = dir.BEFORE_CURSOR})
end)

vim.keymap.set('', 'gsw', function()
	hop.hint_words({direction = dir.AFTER_CURSOR})
end)
vim.keymap.set('', 'gsb', function()
	hop.hint_words({direction = dir.BEFORE_CURSOR})
end)

vim.keymap.set('', 'gs/', function()
	hop.hint_pattern({direction = dir.AFTER_CURSOR})
end)
vim.keymap.set('', 'gs?', function()
	hop.hint_char1({direction = dir.BEFORE_CURSOR})
end)
